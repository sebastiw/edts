;;; edts.el --- Functions thot do Rest API calls.

;; Copyright 2012-2013 Thomas Järvstrand <tjarvstrand@gmail.com>

;; Author: Thomas Järvstrand <thomas.jarvstrand@gmail.com>
;; Keywords: erlang
;; This file is not part of GNU Emacs.

;;
;; This file is part of EDTS.
;;
;; EDTS is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; EDTS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with EDTS. If not, see <http://www.gnu.org/licenses/>.

(require 'eproject)
(require 's)

(require 'ferl)
(require 'edts-event)
(require 'edts-log)
(require 'edts-rest)

(defvar edts-api-node-name nil
  "Used to manually set the project node-name to use in a buffer
that is not part of a project")
(make-variable-buffer-local 'edts-api-node-name)

(defcustom edts-api-async-node-init t
  "Whether or not node initialization should be synchronous"
  :type 'boolean
  :group 'edts)

(defcustom edts-api-num-server-start-retries 20
  "The number of retries to wait for server to start before giving up"
  :type 'integer
  :group 'edts)

(defcustom edts-api-server-start-retry-interval 0.2
  "Time to wait between server availability checks at startup"
  :type 'number
  :group 'edts)

(defcustom edts-api-num-project-node-start-retries 5
  "The number of times to try starting a project node before giving up"
  :type 'integer
  :group 'edts)

(defcustom edts-api-project-node-start-retry-interval 0.5
  "Time to wait between project node availability checks at startup"
  :type 'number
  :group 'edts)

(defvar edts-api--pending-node-startups nil
  "List of nodes that we are waiting on to get ready for registration.")

(defvar edts-api--outstanding-node-registration-requests nil
  "List of nodes for which there are outstanding async registration
requests.")

(defvar edts-api-server-down-hook nil
  "Hooks to be run after the EDTS server has gone down")

(defvar edts-api-after-node-init-hook nil
  "Hooks to run after a node has been initialized")

(defvar edts-api-node-down-hook nil
  "Hooks to run after a node has gone down. These hooks are called with
the node-name of the node that has gone down as the argument.")

(defun edts-api-ensure-server-started ()
  "Starts an edts server-node in a comint-buffer unless it is already running."
  (unless (or (edts-api-node-started-p "edts") (edts-api-start-server))
    (error "EDTS: Could not start main server")))

(defun edts-api-start-server ()
  "Starts an edts server-node in a comint-buffer"
  (interactive)
  (when (edts-api-node-started-p "edts")
    (error "EDTS: Server already running"))
  (let* ((pwd (f-join (directory-file-name edts-lib-directory) ".."))
         (command (list "./start" edts-data-directory edts-erl-command edts-erl-flags))
         (retries edts-api-num-server-start-retries)
         available)
    (edts-shell-make-comint-buffer "*edts*" "edts" pwd command)
    (setq available (edts-api-get-nodes t))
    (while (and (> retries 0) (not available))
      (setq available (edts-api-get-nodes t))
      (sit-for edts-api-server-start-retry-interval)
      (decf retries))
    (when available
      (edts-log-info "Started EDTS server")
      (edts-event-listen))
    available))

(defun edts-api-ensure-node-not-started (node-name)
  "Signals an error if a node of name NODE-NAME is running on
localhost."
  (when (edts-api-node-started-p node-name)
    (error "Node already started")))

(defun edts-api-node-started-p (name)
  "Synchronously query epmd to see whether it has a node with NAME registered."
  (with-temp-buffer
    (let* ((otp-bin-dir (f-canonical (f-dirname edts-erl-command)))
           (epmd        (f-join otp-bin-dir "epmd")))
    (call-process epmd nil (current-buffer) nil "-names")
    (member (car (s-split "@" name))
                 (edts-api-epmd-nodenames-from-string (buffer-string))))))

(defun edts-api-init-node-when-ready (project-name
                                      node-name
                                      root
                                      libs
                                      &optional
                                      app-include-dirs
                                      project-include-dirs
                                      erlang-cookie
                                      retries)
  "Once NODE-NAME is registered with epmd, register it with the edts server."
  (add-to-list 'edts-api--pending-node-startups node-name)
  (let ((retries (or retries edts-api-num-project-node-start-retries)))
    (edts-log-debug "Waiting for node %s to start (tries left: %s)"
                    node-name
                    retries)
    (if (not (edts-api-node-started-p node-name))
        (if (> retries 0)
            ;; Wait same more
            (if edts-api-async-node-init
                ;; This used to use run-with-idle-timer.  A problem
                ;; with that is that we risk checking immediately
                ;; thereby exhausting the retries very quickly which
                ;; is a problem on slower systems.
                ;;
                ;;   * Assume the interval is 0.5 s.
                ;;   * Assume we leave emacs idle, then the timer
                ;;     triggers after 0.5 s.
                ;;   * The function will now add another 0.5 s idle
                ;;     timer, but the time is counted from when emacs
                ;;     became idle and the funtion will trigger
                ;;     immediately.
                ;;   * We could add the (current-idle-time) to the 0.5
                ;;     second interval, but then we have a problem if
                ;;     emacs goes from idle to active and then to idle
                ;;     a second time.  Now we have an idle timer with
                ;;     a (let's say) 4.5 second interval. In this case
                ;;     we would have to reset the timer's interval
                ;;     when emacs goes from active to idle.
                ;;
                ;; We could make it more complex by making this work
                ;; with idleness, but hopefully using the regular
                ;; run-with-timer doesn't load emacs too much.
                (run-with-timer edts-api-project-node-start-retry-interval
                                nil
                                'edts-api-init-node-when-ready
                                project-name
                                node-name
                                root
                                libs
                                app-include-dirs
                                project-include-dirs
                                erlang-cookie
                                (1- retries))
              ;; Synchronous init
              (sit-for edts-api-project-node-start-retry-interval)
              (edts-api-init-node-when-ready project-name
                                             node-name
                                             root
                                             libs
                                             app-include-dirs
                                             project-include-dirs
                                             erlang-cookie
                                             (1- retries)))
          ;; Give up
          (setq edts-api--pending-node-startups
                (remove node-name edts-api--pending-node-startups))
          (null (edts-log-error "Node %s failed to start" node-name)))
      ;; Node started, remove it from list of pending nodes and start
      ;; initialization.
      (edts-log-info "Node %s started" node-name)
      (setq edts-api--pending-node-startups
            (remove node-name edts-api--pending-node-startups))
      (edts-api-init-node project-name
                          node-name
                          root
                          libs
                          app-include-dirs
                          project-include-dirs
                          erlang-cookie))))

(defun edts-api-node-name ()
  "Return the sname of current buffer's project node."
  (condition-case ex
      (eproject-attribute :node-name)
    ('error edts-api-node-name)))

(defun edts-api-init-node (project-name
                           node-name
                           root
                           libs
                           app-include-dirs
                           project-include-dirs
                           erlang-cookie)
  "Register NODE-NAME with the EDTS server asynchronously."
  (interactive (list (eproject-attribute :name)
                     (edts-api-node-name)
                     (eproject-attribute :root)
                     (eproject-attribute :lib-dirs)
                     (eproject-attribute :app-include-dirs)
                     (eproject-attribute :project-include-dirs)
                     (eproject-attribute :erlang-cookie)))
  (unless (member node-name edts-api--outstanding-node-registration-requests)
    (edts-log-debug "Initializing node %s" node-name)
    (add-to-list 'edts-api--outstanding-node-registration-requests node-name)
    (let* ((resource (list "nodes" node-name))
           (args     (list (cons "project_name"         project-name)
                           (cons "project_root"         root)
                           (cons "project_lib_dirs"     libs)
                           (cons "app_include_dirs"     app-include-dirs)
                           (cons "project_include_dirs" project-include-dirs)
                           (cons "erlang_cookie"        erlang-cookie)))
           (cb-args  (list node-name)))
      (if edts-api-async-node-init
          (edts-rest-post-async resource
                                args
                                #'edts-api-init-node-async-callback
                                cb-args)
        (let ((reply (edts-rest-post resource args)))
          (edts-api-init-node-async-callback reply node-name))))))

(defun edts-api-init-node-async-callback (reply node-name)
  "Handle the result of an asynchronous node registration"
  (setq edts-api--outstanding-node-registration-requests
        (remove node-name edts-api--outstanding-node-registration-requests))
  (let ((result (cadr (assoc 'result reply))))
    (if (and result (eq (string-to-number result) 201))
        (progn
          (edts-log-info "Successfully intialized node %s" node-name)
          (run-hooks 'edts-api-after-node-init-hook))
      (null
       (edts-log-error "Failed to initialize node %s" node-name)))))

(defun edts-api-get-function-info (module function arity)
  "Fetches info MODULE on the current buffer's project node associated with
current buffer"
  (let* ((resource (list "nodes"     (edts-api-node-name)
                         "modules"   module
                         "functions" function
                         (number-to-string arity)))
         (res      (edts-rest-get resource nil)))
    (if (equal (assoc 'result res) '(result "200" "OK"))
        (cdr (assoc 'body res))
        (null
         (edts-log-error "Unexpected reply: %s" (cdr (assoc 'result res)))))))

(defun edts-api-get-modules ()
  "Fetches all available erlang modules for the node associated with
current buffer"
  (let* ((resource (list "nodes" (edts-api-node-name) "modules"))
         (res      (edts-rest-get resource nil)))
    (if (equal (assoc 'result res) '(result "200" "OK"))
        (cdr (assoc 'body res))
        (null
         (edts-log-error "Unexpected reply: %s" (cdr (assoc 'result res)))))))

(defun edts-api-get-module-export-strings (module &optional no-error)
  "Fetches all exported functions of MODULE on the node associated with
current buffer and returns them as strings. Does not fetch detailed
information about the individual functions. If NO-ERROR is non-nil,
don't report an error if the request fails."
  (let ((exports (edts-api-get-module-exports module no-error)))
    (mapcar #'edts-api-function-to-string exports)))

(defun edts-api-get-module-exports (module &optional no-error)
  "Fetches all exported functions of MODULE on the node associated with
current buffer. Does not fetch detailed information about the individual
functions. If NO-ERROR is non-nil, don't report an error if the request
fails."
  (let* ((resource (list "nodes" (edts-api-node-name)
                         "modules" module))
         (res      (edts-rest-get resource '(("info_level" . "basic")))))
    (if (equal (assoc 'result res) '(result "200" "OK"))
          (cdr (assoc 'exports (cdr (assoc 'body res))))
      (unless no-error
        (null
         (edts-log-error "Unexpected reply: %s" (cdr (assoc 'result res))))))))

(defun edts-api-function-to-string (function-struct)
  "Convert FUNCTION-STRUCT to a string of <function>/<arity>"
  (format "%s/%s"
          (cdr (assoc 'function function-struct))
          (cdr (assoc 'arity    function-struct))))

(defun edts-get-free-vars (snippet)
  "Return a list of the free variables in SNIPPET"
  (let* ((resource (list "code" "free_vars"))
         (res      (edts-rest-get resource nil snippet)))
    (if (equal (assoc 'result res) '(result "200" "OK"))
        (cdr (assoc 'vars (cdr (assoc 'body res))))
        (null
         (edts-log-error "Unexpected reply: %s" (cdr (assoc 'result res)))))))

(defun edts-api-get-mfas (snippets)
  "Return a each code snippet in SNIPPETS parsed as an MFA"
  (let* ((resource (list "code" "parsed_expressions" "mfa"))
         (res      (edts-rest-get resource nil snippets)))
    (if (equal (assoc 'result res) '(result "200" "OK"))
        (cdr (assoc 'body res))
        (null
         (edts-log-error "Unexpected reply: %s" (cdr (assoc 'result res)))))))

(defun edts-api-get-basic-module-info (module)
  "Fetches basic info about module on the node associated with current buffer"
  (edts-api-get-module-info (edts-api-node-name) module 'basic))

(defun edts-api-get-detailed-module-info (module)
  "Fetches detailed info about MODULE on the node associated with current
buffer"
  (edts-api-get-module-info (edts-api-node-name) module 'detailed))

(defun edts-api-get-module-info (node module level)
  "Fetches info about MODULE on NODE. LEVEL is either basic or detailed."
  (let* ((resource (list "nodes" node "modules" module))
         (args     (list (cons "info_level" (symbol-name level))))
         (res      (edts-rest-get resource args)))
    (if (equal (assoc 'result res) '(result "200" "OK"))
        (cdr (assoc 'body res))
        (null
         (edts-log-error "Unexpected reply: %s" (cdr (assoc 'result res)))))))

(defun edts-api-get-module-eunit-async (module callback)
  "Run eunit tests in MODULE on the node associated with current-buffer,
asynchronously. When the request terminates, call CALLBACK with the
parsed response as the single argument."
  (let* ((node-name (edts-api-node-name))
         (resource      (list "nodes"   node-name
                              "modules" module "eunit"))
         (cb-args (list callback 200)))
    (edts-log-debug
     "running eunit tests in %s async on %s" module node-name)
    (edts-rest-get-async resource nil #'edts-api-async-callback cb-args)))

(defun edts-api-compile-and-load-async (module file callback)
  "Compile MODULE in FILE on the node associated with current buffer,
asynchronously. When the request terminates, call CALLBACK with the
parsed response as the single argument."
  (let* ((node-name   (edts-api-node-name))
         (resource    (list "nodes" node-name "modules" module))
         (rest-args   (list (cons "file" file)))
         (cb-args     (list callback 201)))
    (edts-log-debug "Compiling %s async on %s" module node-name)
    (edts-rest-post-async resource
                          rest-args
                          #'edts-api-async-callback
                          cb-args)))

(defun edts-api-get-includes (&optional module)
  "Get all includes of module in current-buffer from the node
associated with that buffer"
  (let ((info (edts-api-get-detailed-module-info (or module
                                                     (ferl-get-module)))))
    (cdr (assoc 'includes info)))) ;; Get all includes

(defun edts-api-node-registeredp (node &optional no-error)
  "Return non-nil if NODE is registered with the EDTS server"
  (member node (edts-api-get-nodes no-error)))

(defun edts-api-get-nodes (&optional no-error)
  "Return all nodes registered with the EDTS server. If NO-ERROR is
non-nil, don't report an error if the request fails."
  (let (nodes
        (res (edts-rest-get '("nodes") nil)))
    (if (equal (assoc 'result res) '(result "200" "OK"))
        (cdr (assoc 'nodes (cdr (assoc 'body res))))
      (unless no-error
        (null (edts-log-error "Unexpected reply: %s"
                              (cdr (assoc 'result res))))))))

(defun edts-api-async-callback (reply callback expected &rest args)
  "Generic callback-function for handling the reply of REST requests.
If the HTTP return-code (an integer) of REPLY equals EXPECTED, call
CALLBACK with the HTTP body of REPLY as the first argument and
ARGS as the other arguments"
  (let ((result (cadr (assoc 'result reply))))
    (if (and result (eq (string-to-number result) expected))
        (when callback
          (apply callback (cdr (assoc 'body reply)) args))
      (null
       (edts-log-error "Unexpected reply: %s" (cdr (assoc 'result reply)))))))

(defun edts-api-epmd-nodenames-from-string (string)
  "Convert the epmd reply STRING into a list of nodenames"
  (setq string (split-string (substring string 4)))
  (let ((names  nil))
    (while string
      (when (string-equal (car string) "name")
        (setq names (cons (cadr string) names)))
      (setq string (cdr string)))
    names))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests

(provide 'edts-api)
