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

(require 'dash)
(require 's)

(require 'ferl)
(require 'edts-log)
(require 'edts-rpc)

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

(defvar edts-api-server-up-hook nil
  "Hooks to be run after the EDTS server has gone up")

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
         (command (list "./start"
                        edts-data-directory
                        edts-erl-command
                        edts-erl-flags))
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
      (run-hooks 'edts-api-server-up-hook))
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
    (-contains? (edts-api-epmd-nodenames-from-string (buffer-string))
                (car (s-split "@" name))))))

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
            ;; Wait some more
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
  (or (edts-project-attribute :node-name)
      edts-api-node-name))

(defun edts-api-init-node (project-name
                           node-name
                           root
                           libs
                           app-include-dirs
                           project-include-dirs
                           erlang-cookie)
  "Register NODE-NAME with the EDTS server asynchronously."
  (interactive (list (edts-project-attribute :name)
                     (edts-api-node-name)
                     (edts-project-attribute :root)
                     (edts-project-attribute :lib-dirs)
                     (edts-project-attribute :app-include-dirs)
                     (edts-project-attribute :project-include-dirs)
                     (edts-project-attribute :erlang-cookie)))
  (unless (member node-name edts-api--outstanding-node-registration-requests)
    (edts-log-debug "Initializing node %s" node-name)
    (add-to-list 'edts-api--outstanding-node-registration-requests node-name)
    (let* ((args     (list (cons "nodename"             node-name)
                           (cons "project_name"         project-name)
                           (cons "project_root"         root)
                           (cons "project_lib_dirs"     libs)
                           (cons "app_include_dirs"     app-include-dirs)
                           (cons "project_include_dirs" project-include-dirs)
                           (cons "erlang_cookie"        erlang-cookie)))
           (cb-args  (list node-name)))
      (if edts-api-async-node-init
          (edts-rpc-call-async "init_node"
                               args
                               #'edts-api-init-node-async-callback
                               cb-args)
        (let ((reply (edts-rpc-call "init_node" args)))
          (edts-api-init-node-async-callback reply node-name))))))

(defun edts-api-init-node-async-callback (reply node-name)
  "Handle the result of an asynchronous node registration"
  (setq edts-api--outstanding-node-registration-requests
        (remove node-name edts-api--outstanding-node-registration-requests))
  (let ((result (cadr (assoc 'result reply))))
    (if (and result (eq (string-to-number result) 200))
        (progn
          (edts-log-info "Successfully intialized node %s" node-name)
          (run-hooks 'edts-api-after-node-init-hook))
      (null
       (edts-log-error "Failed to initialize node %s" node-name)))))

(defun edts-api-get-function-info (module function arity)
  "Fetches info MODULE on the current buffer's project node associated with
current buffer"
  (let* ((args (list (cons "nodename" (edts-api-node-name))
                     (cons "module" module)
                     (cons "function" function)
                     (cons "arity" (number-to-string arity))))
         (res      (edts-rpc-call "get_function_info" args)))
    (if (equal (assoc 'result res) '(result "200" "OK"))
        (cdr (assoc 'body res))
        (null
         (edts-log-error "Unexpected reply: %s" (cdr (assoc 'result res)))))))

(defun edts-api-get-modules ()
  "Fetches all available erlang modules for the node associated with
current buffer"
  (let* ((args (list (cons "nodename" (edts-api-node-name))))
         (res  (edts-rpc-call "get_modules" args)))
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
  (-when-let (result (edts-api-get-module-info (edts-api-node-name)
                                               module
                                               'basic
                                               no-error))
    (cdr (assoc 'exports result))))

(defun edts-api-function-to-string (function-struct)
  "Convert FUNCTION-STRUCT to a string of <function>/<arity>"
  (format "%s/%s"
          (cdr (assoc 'function function-struct))
          (cdr (assoc 'arity    function-struct))))

(defun edts-api-get-free-vars (snippet)
  "Return a list of the free variables in SNIPPET"
  (let* ((args     (list (cons "code" snippet)))
         (res      (edts-rpc-call "get_free_vars" args)))
    (if (not (equal (assoc 'result res) '(result "200" "OK")))
        (edts-log-error "Unexpected reply: %s" (cdr (assoc 'result res)))
      (-if-let (errs (cdr (assoc 'errors (cdr (assoc 'body res)))))
          (error "%s"
                          (-map (lambda (err) (cdr (assoc 'description err)))
                                errs))
        (cdr (assoc 'vars (cdr (assoc 'body res))))))))

(defun edts-api-get-mfas (snippets)
  "Return a each code snippet in SNIPPETS parsed as an MFA"
  (let* ((args     (list (cons "expressions" snippets)))
         (res      (edts-rpc-call "get_mfas" args)))
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

(defun edts-api-get-module-info (node module level &optional no-error)
  "Fetches info about MODULE on NODE. LEVEL is either basic or detailed."
  (let* ((args     (list (cons "nodename" node)
                         (cons "module" module)
                         (cons "info_level" (symbol-name level))))
         (res      (edts-rpc-call "get_module_info" args)))
    (if (equal (assoc 'result res) '(result "200" "OK"))
        (cdr (assoc 'body res))
      (unless no-error
        (null
         (edts-log-error "Unexpected reply: %s" (cdr (assoc 'result res))))))))

(defun edts-api-get-module-eunit-async (module callback)
  "Run eunit tests in MODULE on the node associated with current-buffer,
asynchronously. When the request terminates, call CALLBACK with the
parsed response as the single argument."
  (let* ((node-name (edts-api-node-name))
         (args (list (cons "nodename" node-name)
                     (cons "module" module)))
         (cb-args (list callback 200)))
    (edts-log-debug
     "running eunit tests in %s async on %s" module node-name)
    (edts-rpc-call-async "run_eunit" args #'edts-api-async-callback cb-args)))

(defun edts-api-compile-and-load-async (module file callback)
  "Compile MODULE in FILE on the node associated with current buffer,
asynchronously. When the request terminates, call CALLBACK with the
parsed response as the single argument."
  (let* ((node-name   (edts-api-node-name))
         (args        (list (cons "nodename" node-name)
                            (cons "file" file)))
         (cb-args     (list callback 200)))
    (edts-log-debug "Compiling %s async on %s" module node-name)
    (edts-rpc-call-async "compile_and_load"
                         args
                         #'edts-api-async-callback
                         cb-args)))

(defun edts-api-get-includes (&optional module)
  "Get all includes of module in current-buffer from the node
associated with that buffer"
  (let ((info (edts-api-get-detailed-module-info (or module
                                                     (ferl-get-module)))))
    (cdr (assoc 'includes info)))) ;; Get all includes

(defun edts-api-node-registeredp (node)
  "Return non-nil if NODE is registered with the EDTS server"
  (member node (edts-api-get-nodes t)))

(defun edts-api-get-nodes (&optional no-error)
  "Return all nodes registered with the EDTS server. If NO-ERROR is
non-nil, don't report an error if the request fails."
  (let* (nodes
         (edts-log-inhibit no-error)
         (res (edts-rpc-call "get_nodes" nil)))
    (cdr (assoc 'nodes (cdr (assoc 'body res))))))


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

(defun edts-api-init-project-node ()
  (interactive)
  (edts-api-ensure-server-started)
  (if (edts-api-node-registeredp (edts-project-attribute :node-name))
      (progn
        (edts-api-refresh-project-node))
    ;; Ensure project node is started
    (unless (edts-api-node-started-p (edts-project-attribute :node-name))
      (edts-api--start-project-node))
    ;; Register it with the EDTS node
    (edts-api--register-project-node)))

(defun edts-api--start-project-node ()
  "Starts a new erlang node for current buffer's project."
  (let* ((buffer-name (concat "*" (edts-project-name) "*"))
         (command (split-string (edts-project-attribute :start-command)))
         (exec-path (edts-api--build-exec-path))
         (process-environment (edts-api--build-env))
         (node (edts-project-attribute :node-name)))
    (edts-api-ensure-node-not-started node)
    (edts-shell-make-comint-buffer buffer-name node (edts-project-root) command)
    (get-buffer buffer-name)))

(defun edts-api--build-exec-path ()
  "Build up the exec-path to use when starting the project-node of PROJECT."
  (-if-let (otp-path (edts-project-attribute :otp-path))
      ;; put otp-path first in path
      (cons (f-expand "bin" otp-path) exec-path)
    exec-path))

(defun edts-api--build-env ()
  "Build up the PATH environment variable to use when starting current-
buffer's project-node and return the resulting environment."
  (let* ((bin-dir  (edts-api--otp-bin-path))
         (path-var (concat "PATH=" bin-dir path-separator (getenv "PATH"))))
    (cons path-var process-environment)))

(defun edts-api--otp-bin-path ()
  "Return the otp bin-path of current-buffer's project or, if that is
not defined, the first directory in the `exec-path' that contains a file
named erl."
  (or (-when-let (otp-path (edts-project-attribute :otp-path))
        (f-full (f-join otp-path "bin")))
      (-when-let (erl (executable-find "erl"))
        (f-dirname erl))))

(defun edts-api-refresh-project-node ()
  "Asynchronously refresh the state of current buffer's project node"
  (interactive)
  (edts-api-init-node
   (edts-project-attribute :name)
   (edts-project-attribute :node-name)
   (edts-project-root)
   (edts-project-attribute :lib-dirs)
   (edts-project-attribute :app-include-dirs)
   (edts-project-attribute :project-include-dirs)
   (edts-project-attribute :erlang-cookie)))

(defun edts-api--register-project-node ()
  "Register the node of current buffer's project."
  (if (edts-api-node-registeredp (edts-project-attribute :node-name))
      (edts-log-info "Re-initializing node for project %s" (edts-project-name))
    (edts-log-info "Initializing node for project %s" (edts-project-name)))
  (edts-api-init-node-when-ready
   (edts-project-attribute :name)
   (edts-project-attribute :node-name)
   (edts-project-root)
   (edts-project-attribute :lib-dirs)
   (edts-project-attribute :app-include-dirs)
   (edts-project-attribute :project-include-dirs)
   (edts-project-attribute :erlang-cookie)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests

(provide 'edts-api)
