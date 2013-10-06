;;; edts.el --- Misc edts-related functionality.

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

(require 'edts-event)

(defcustom edts-erl-command
  (or (executable-find "erl")
      (null
        (warn
         "No erl on exec-path. Most of EDTS' functionality will be broken.")))
  "Location of the erl-executable to use when launching the main EDTS-
node."
  :group 'edts)

(defconst edts-erl-root
  (and edts-erl-command
       (file-name-directory
        (directory-file-name
         (file-name-directory (file-truename edts-erl-command)))))
  "Location of the Erlang root directory")

(defcustom edts-data-directory
  (if (boundp 'user-emacs-directory)
      (expand-file-name (concat user-emacs-directory "/edts"))
      (expand-file-name "~/.emacs.d"))
  "Where EDTS should save its data.")

(defvar edts-find-macro-regexp
  "\\(\\(\\('.*'\\)\\|\\([a-zA-Z0-9_@]*\\)\\)[\\s-]*\\((.*)\\)?\\)"
  "Regexp describing a macro name")

(defconst edts-find-macro-definition-regexp
  (format "^-define\\s-*(%s,\\s-*\\(.*\\))." edts-find-macro-regexp)
  "Regexp describing a macro definition")

(defvar edts-buffer-node-name nil
  "The node-name of current-buffer")
(make-variable-buffer-local 'edts-buffer-node-name)

(defvar edts-server-down-hook nil
  "Hooks to be run after the EDTS server has gone down")

(defvar edts-after-node-init-hook nil
  "Hooks to run after a node has been initialized.")

(defvar edts-node-down-hook nil
  "Hooks to run after a node has gone down. These hooks are called with
the node-name of the node that has gone down as the argument.")

(defun edts-event-handler (node class type info)
  (case type
    (node_down
     (let ((node (cdr (assoc 'node info))))
       (edts-log-info "Node %s down" node)
       (run-hook-with-args 'edts-node-down-hook node)))
    (server_down
     (edts-log-info "EDTS server down")
     (setq edts--outstanding-node-registration-requests nil)
     (run-hooks 'edts-server-down-hook))))
(edts-event-register-handler 'edts-event-handler 'edts)

(defun edts-buffer-node-name ()
  "Print the node sname of the erlang node connected to current
buffer. The node is either:
- The module's project node, if current buffer is an erlang module, or
- The buffer's erlang node if buffer is an edts-shell buffer.
- The project-node of the buffer that was current buffer before jumping
  to the current buffer if the file of the current buffer is located outside
  any project (eg. an \"externally\" loaded module such as an otp-module or a
  module loaded by ~/.erlang)."
  (interactive)
  (message "%s" (edts-node-name)))

(defun edts-find-module-macros ()
  (let* ((files  (cons (buffer-file-name) (edts-get-includes)))
         (macros   (apply #'append
                          (mapcar #'edts-get-file-macros files)))
         (parsed  (edts-parse-macros macros)))
    parsed))


(defun edts-parse-macros (raw-macros)
  (when raw-macros
    (let* (;; Raw macro strings: '("MACRO1" "MACRO2(ARG21,..., ARG2X)")
           (arity-macros (mapcar #'(lambda (m) (cdr (assoc 'string m)))
                                 raw-macros))
           ;; (("MACRO1" 0) ("MACRO2" X))
           (arity-macro-strings (mapcar #'(lambda (m)
                                            (cons (cdr (assoc 'function m))
                                                  (cdr (assoc 'arity    m))))
                                        (edts-get-mfas arity-macros))))
      (loop for raw-m in raw-macros collect
            (let* ((name      (cdr (assoc 'name raw-m)))
                   (args      (cdr (assoc 'args raw-m)))
                   (value     (cdr (assoc 'value raw-m)))
                   (raw-str   (cdr (assoc 'string raw-m)))
                   (arity     (cdr (assoc-string name arity-macro-strings)))
                   (arity-str (format "%s/%s" name arity))
                   (doc       (format "%s -> %s" raw-str value)))
              (cons arity-str doc))))))


(defun edts-get-file-macros (file-name)
  (with-temp-buffer
    (insert-file-contents file-name)
    (edts-get-macros)))

(defun edts-get-macros ()
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (let ((macros nil))
        (while (re-search-forward edts-find-macro-definition-regexp nil t)
            (push
             `((string . ,(match-string-no-properties 1))
               (name   . ,(match-string-no-properties 2))
               (args   . ,(match-string-no-properties 5))
               (value  . ,(match-string-no-properties 6)))
             macros)
            (goto-char (match-end 0)))
        macros))))

(defun edts-mfa-at (&optional point)
  "Find mfa under POINT. POINT defaults to current point."
  (goto-char (or point (point)))
  (save-excursion
    (save-match-data
      (let* ((start (save-excursion
                      (skip-chars-backward "a-zA-Z0-9_:'")
                      (point)))
             (end   (save-excursion
                      (ferl-goto-end-of-call-name)
                      (forward-sexp)
                      (point)))
             (str   (buffer-substring-no-properties start end)))
        (car (edts-strings-to-mfas (list str)))))))

(defun edts-strings-to-mfas (strs)
  "Return a list with each string in STRS parsed to an mfa."
  (mapcar #'edts--fix-mfa-mod (edts-get-mfas strs)))

(defun edts--fix-mfa-mod (mfa)
  (let ((m (cdr (assoc 'module mfa)))
        (f (cdr (assoc 'function mfa)))
        (a (cdr (assoc 'arity mfa))))
    (unless m
      (loop named import
            for (module . imported) in (erlang-get-import) do
            (when (eq a (cdr (assoc f imported)))
              (return-from import module))))
    (if m
        (list m f a)
      (list (erlang-get-module) f a))))

(defun edts-search-function (function arity)
  "Goto the definition of FUNCTION/ARITY in the current buffer."
  (let ((origin (point))
        (re (concat "^" function "\s*("))
        (match nil))
    (goto-char (point-min))
    (while (and (null match) (re-search-forward re nil t))
      (goto-char (match-beginning 0))
      (ferl-goto-end-of-call-name)
      (when (eq arity (car (last (edts-mfa-at (point)))))
        (setq match t)))
    (if match
        (beginning-of-line)
      (goto-char origin)
      (error "function %s/%s not found" function arity))))


(defun edts-query (prompt choices &optional error-msg)
  "Query the user for a choice"
  (let ((choice (ido-completing-read prompt choices)))
    (if (member choice choices)
        choice
      (error (or error-msg "Invalid choice")))))

(defun edts-find-doc ()
  "Find and show the man-page documentation for a function."
  (interactive)
  (let* ((module
          (edts-query "Module: " (edts-man-modules)))
         (fun-strings (edts-man-module-function-entries module))
         (fun (edts-query "Function: " (cons "-Top of Chapter-" fun-strings))))
    (if (string= fun "-Top of Chapter-")
        (edts-man-find-module module)
        (let* ((split     (split-string fun "/"))
               (fun-name  (car split))
               (fun-arity (string-to-number (cadr split))))
          (edts-man-find-function-entry module fun-name fun-arity)))))

(defun edts-show-doc-under-point ()
  "Find and display the man-page documentation for function under point
in a tooltip."
  (interactive)
  (let* ((mfa      (edts-mfa-at (point)))
         (module   (car mfa))
         (function (cadr mfa))
         (arity    (caddr mfa)))
    (unless (and module function arity)
      (error "Could not parse MFA at point"))
    (edts-show-tooltip
     (condition-case ex
         (edts-man-extract-function-entry module function)
       ('error
        (edts-extract-doc-from-source module function arity))))))

(defun edts-show-tooltip (text)
  "Show a tooltip using either popup.el or pos-tip.el"
  (condition-case ex
      (pos-tip-show text nil nil nil -1)
    ('error
     (popup-tip text))))

(defun edts-extract-doc-from-source (module function arity)
  "Find documentation for MODULE:FUNCTION/ARITY"
  (let ((source (cdr (assoc 'source (edts-get-basic-module-info module)))))
    (if source
        (edts-doc-extract-function-information-from-source source
                                                           function
                                                           arity)
      (null (edts-log-error "No such module: %s" module)))))

(defun edts-function-head-regexp (function &optional arity)
  "Construct a regexp matching FUNCTION(arg1, ..., argARITY). A negative number
for ARITY will give a regexp matching any arity."
  (unless arity (setq arity -1))
  (format "%s[[:space:]\n]*(%s)" function (edts-argument-regexp arity)))

(defun edts-function-regexp (function &optional arity)
  "Construct a regexp matching 'FUNCTION(arg1, ..., argARITY) ->'.
negative number for ARITY will give a regexp matching any arity."
  (concat (edts-function-head-regexp function arity) "[[:space:]\n]*->"))

(defun edts-any-function-regexp ()
  "Construct a regexp matching any function."
  ;; Kind of broken for strings, comments and single quoted atoms
  (format "\\(%s[[:space:]\n]*(.*)\\)[[:space:]]*->" erlang-atom-regexp))

(defun edts-argument-regexp (arity)
  "Contstruct a regexp matching ARITY arguments. A negative number
for ARITY will give a regexp matching any arity."
  (cond
   ((< arity 0) "[[:ascii:]]*?")
   ((equal arity 0) "[[:space:]]*")
   ((concat "[^,]*?" (apply #'concat (make-list (- arity 1) ",[^,]*?"))))))

(defun edts-ahs-edit-current-function ()
  "Activate ahs-edit-mode with erlang-current-function range-plugin."
  (interactive)
  (ahs-onekey-edit-function 'erlang-current-function nil))

(defun edts-ahs-edit-buffer ()
  "Activate ahs-edit-mode with ahs-range-whole-buffer range-plugin."
  (interactive)
  (ahs-onekey-edit-function 'whole-buffer nil))


(defun edts-ensure-server-started ()
  "Starts an edts server-node in a comint-buffer unless it is already running."
  (unless (or (edts-node-started-p "edts") (edts-start-server))
    (error "EDTS: Could not start main server")))

(defun edts-start-server ()
  "Starts an edts server-node in a comint-buffer"
  (interactive)
  (when (edts-node-started-p "edts")
    (error "EDTS: Server already running"))
  (let* ((pwd (path-util-join (directory-file-name edts-lib-directory) ".."))
         (command (list "./start" edts-data-directory edts-erl-command))
         (retries 20)
         available)
    (edts-shell-make-comint-buffer "*edts*" "edts" pwd command)
    (setq available (edts-get-nodes t))
    (while (and (> retries 0) (not available))
      (setq available (edts-get-nodes t))
      (sit-for 0.2)
      (decf retries))
    (when available
      (edts-log-info "Started EDTS server")
      (edts-event-listen))
    available))


(defun edts-ensure-node-not-started (node-name)
  "Signals an error if a node of name NODE-NAME is running on
localhost."
  (when (edts-node-started-p node-name)
    (error "Node already started")))

(defun edts-node-started-p (name)
  "Syncronously query epmd to see whether it has a node with NAME registered."
  (with-temp-buffer
    (let* ((otp-bin-dir (file-truename (path-util-pop edts-erl-command)))
           (epmd        (path-util-join otp-bin-dir "epmd")))
    (call-process epmd nil (current-buffer) nil "-names")
    (member name (edts-epmd-nodenames-from-string (buffer-string))))))

(defun edts-epmd-nodenames-from-string (string)
  "Convert the epmd reply STRING into a list of nodenames."
  (setq string (split-string (substring string 4)))
  (let ((names  nil))
    (while string
      (when (string-equal (car string) "name")
        (setq names (cons (cadr string) names)))
      (setq string (cdr string)))
    names))

(defcustom edts-async-node-init t
  "Whether or not node initialization should be synchronous")

(defvar edts--pending-node-startups nil
  "List of nodes that we are waiting on to get ready for registration.")

(defvar edts--outstanding-node-registration-requests nil
  "List of nodes for which there are outstanding async registration
requests.")

(defun edts-init-node-when-ready (project-name
                                  node-name
                                  root
                                  libs
                                  &optional
                                  app-include-dirs
                                  project-include-dirs
                                  &optional retries)
  "Once NODE-NAME is registered with epmd, register it with the edts server."
  (add-to-list 'edts--pending-node-startups node-name)
  (let ((retries (or retries 5)))
    (edts-log-debug "Waiting for node %s to start (retries %s)"
                    node-name
                    retries)
    (if (not (edts-node-started-p node-name))
        (if (> retries 0)
            ;; Wait same more
            (if edts-async-node-init
                (run-with-idle-timer 0.5
                                     nil
                                     'edts-init-node-when-ready
                                     project-name
                                     node-name
                                     root
                                     libs
                                     app-include-dirs
                                     project-include-dirs
                                     (1- retries))
              ;; Synchronous init
              (sit-for 0.5)
              (edts-init-node-when-ready project-name
                                         node-name
                                         root
                                         libs
                                         app-include-dirs
                                         project-include-dirs
                                         (1- retries)))
          ;; Give up
          (setq edts--pending-node-startups
                (remove node-name edts--pending-node-startups))
          (null (edts-log-error "Node %s failed to start." node-name)))
      ;; Node started, remove it from list of pending nodes and start
      ;; initialization.
      (edts-log-info "Node %s started" node-name)
      (setq edts--pending-node-startups
            (remove node-name edts--pending-node-startups))
      (edts-init-node project-name
                      node-name
                      root
                      libs
                      app-include-dirs
                      project-include-dirs))))

(defun edts-init-node (project-name
                       node-name
                       root
                       libs
                       app-include-dirs
                       project-include-dirs)
  "Register NODE-NAME with the EDTS server asynchronously."
  (unless (member node-name edts--outstanding-node-registration-requests)
    (edts-log-debug "Initializing node %s" node-name)
    (add-to-list 'edts--outstanding-node-registration-requests node-name)
    (interactive (list (eproject-attribute :name)
                       (edts-node-name)
                       (eproject-attribute :root)
                       (eproject-attribute :lib-dirs)
                       p(eproject-attribute :app-include-dirs)
                       (eproject-attribute :project-include-dirs)))
    (let* ((resource (list "nodes" node-name))
           (args     (list (cons "project_name"         project-name)
                           (cons "project_root"         root)
                           (cons "project_lib_dirs"     libs)
                           (cons "app_include_dirs"     app-include-dirs)
                           (cons "project_include_dirs" project-include-dirs)))
           (cb-args  (list node-name)))
      (if edts-async-node-init
          (edts-rest-post-async resource
                                args
                                #'edts-init-node-async-callback
                                cb-args)
        (let ((reply (edts-rest-post resource args)))
          (edts-init-node-async-callback reply node-name))))))

(defun edts-init-node-async-callback (reply node-name)
  "Handle the result of an asynchronous node registration."
  (setq edts--outstanding-node-registration-requests
        (remove node-name edts--outstanding-node-registration-requests))
  (let ((result (cadr (assoc 'result reply))))
    (if (and result (eq (string-to-number result) 201))
        (progn
          (edts-log-info "Successfuly intialized node %s" node-name)
          (run-hooks 'edts-after-node-init-hook))
      (null
       (edts-log-error "Failed to initialize node %s" node-name)))))


(defun edts-get-function-info (module function arity)
  "Fetches info MODULE on the current buffer's project node associated with
current buffer."
  (let* ((resource (list "nodes"     (edts-node-name)
                         "modules"   module
                         "functions" function
                         (number-to-string arity)))
         (res      (edts-rest-get resource nil)))
    (if (equal (assoc 'result res) '(result "200" "OK"))
        (cdr (assoc 'body res))
        (null
         (edts-log-error "Unexpected reply: %s" (cdr (assoc 'result res)))))))

(defun edts-get-modules ()
  "Fetches all available erlang modules for the node associated with
current buffer."
  (let* ((resource (list "nodes" (edts-node-name) "modules"))
         (res      (edts-rest-get resource nil)))
    (if (equal (assoc 'result res) '(result "200" "OK"))
        (cdr (assoc 'body res))
        (null
         (edts-log-error "Unexpected reply: %s" (cdr (assoc 'result res)))))))

(defun edts-get-module-exports (module &optional no-error)
  "Fetches all exported functions of MODULE on the node associated with
current buffer. Does not fetch detailed information about the individual
functions. If NO-ERROR is non-nil, don't report an error if the request
fails."
  (let* ((resource (list "nodes" (edts-node-name)
                         "modules" module))
         (res      (edts-rest-get resource '(("info_level" . "basic")))))
    (if (equal (assoc 'result res) '(result "200" "OK"))
          (cdr (assoc 'exports (cdr (assoc 'body res))))
      (unless no-error
        (null
         (edts-log-error "Unexpected reply: %s" (cdr (assoc 'result res))))))))

(defun edts-function-to-string (function-struct)
  "Convert FUNCTION-STRUCT to a string of <function>/<arity>."
  (format "%s/%s"
          (cdr (assoc 'function function-struct))
          (cdr (assoc 'arity    function-struct))))

(defun edts-get-basic-module-info (module)
  "Fetches basic info about module on the node associated with current buffer"
  (edts-get-module-info (edts-node-name) module 'basic))

(defun edts-get-detailed-module-info (module)
  "Fetches detailed info about MODULE on the node associated with current
buffer"
  (edts-get-module-info (edts-node-name) module 'detailed))

(defun edts-get-free-vars (snippet)
  "Return a list of the free variables in SNIPPET."
  (let* ((resource (list "code" "free_vars"))
         (res      (edts-rest-get resource nil snippet)))
    (if (equal (assoc 'result res) '(result "200" "OK"))
        (cdr (assoc 'vars (cdr (assoc 'body res))))
        (null
         (edts-log-error "Unexpected reply: %s" (cdr (assoc 'result res)))))))

(defun edts-get-mfas (snippets)
  "Return a each code snippet in SNIPPETS parsed as an mfa."
  (let* ((resource (list "code" "parsed_expressions" "mfa"))
         (res      (edts-rest-get resource nil snippets)))
    (if (equal (assoc 'result res) '(result "200" "OK"))
        (cdr (assoc 'body res))
        (null
         (edts-log-error "Unexpected reply: %s" (cdr (assoc 'result res)))))))


(defun edts-get-module-info (node module level)
  "Fetches info about MODULE on NODE LEVEL is either basic or detailed."
  (let* ((resource (list "nodes" node "modules" module))
         (args     (list (cons "info_level" (symbol-name level))))
         (res      (edts-rest-get resource args)))
    (if (equal (assoc 'result res) '(result "200" "OK"))
        (cdr (assoc 'body res))
        (null
         (edts-log-error "Unexpected reply: %s" (cdr (assoc 'result res)))))))


(defun edts-get-module-eunit-async (module callback)
  "Run eunit tests in MODULE on the node associated with current-buffer,
asynchronously. When the request terminates, call CALLBACK with the
parsed response as the single argument."
  (let* ((node-name (edts-node-name))
         (resource      (list "nodes"   node-name
                              "modules" module "eunit"))
         (cb-args (list callback 200)))
    (edts-log-debug
     "running eunit tests in %s async on %s" module node-name)
    (edts-rest-get-async resource nil #'edts-async-callback cb-args)))


(defun edts-compile-and-load-async (module file callback)
  "Compile MODULE in FILE on the node associated with current buffer,
asynchronously. When the request terminates, call CALLBACK with the
parsed response as the single argument."
  (let* ((node-name   (edts-node-name))
         (resource    (list "nodes" node-name "modules" module))
         (rest-args   (list (cons "file" file)))
         (cb-args     (list callback 201)))
    (edts-log-debug "Compiling %s async on %s" module node-name)
    (edts-rest-post-async resource rest-args #'edts-async-callback cb-args)))

(defun edts-get-dialyzer-analysis-async (modules otp-plt out-plt callback)
  "Run dialyzer analysis on MODULES on the node associated with
current-buffer asynchronously. When the request terminates, call
CALLBACK with the parsed response as the single argument."
  (let* ((node-name (edts-node-name))
         (resource (list "nodes"   node-name
                         "dialyzer_analysis"))
         (args     `(("modules" . ,modules)
                     ("otp_plt" . ,otp-plt)
                     ("out_plt" . ,out-plt)))
         (cb-args (list callback 200)))
    (edts-log-debug
     "running dialyzer on %s async on %s" modules node-name)
    (edts-rest-get-async resource args #'edts-async-callback cb-args)))

(defun edts-pretty-print-term (term-str indent max-col)
  "Pretty-print the term represented by TERM-STR, indenting it INDENT
spaces and breaking lines at column MAX-COL."
  (let* ((resource '("pretty_print"))
         (rest-args `(("string" .   ,term-str)
                      ("indent" .   ,(number-to-string indent))
                      ("max_column" ,(number-to-string max-col))))
         (res         (edts-rest-get resource rest-args )))
    (if (equal (assoc 'result res) '(result "200" "OK"))
        (cdr (assoc 'return (cdr (assoc 'body res))))
      (null
       (edts-log-error "Unexpected reply: %s" (cdr (assoc 'result res)))))))


(defun edts-async-callback (reply callback expected &rest args)
  "Generic callback-function for handling the reply of rest-requests.
If the http return-code (an integer) of REPLY equals EXPECTED, call
CALLBACK with the http-body part of REPLY as the first argument and
ARGS as the other arguments"
  (let ((result (cadr (assoc 'result reply))))
    (if (and result (eq (string-to-number result) expected))
        (when callback
          (apply callback (cdr (assoc 'body reply)) args))
      (null
       (edts-log-error "Unexpected reply: %s" (cdr (assoc 'result reply)))))))

(defun edts-get-includes (&optional module)
  "Get all includes of module in current-buffer from the node
associated with that buffer."
  (let ((info (edts-get-detailed-module-info (or module (ferl-get-module)))))
    (cdr (assoc 'includes info)))) ;; Get all includes

(defun edts-node-registeredp (node &optional no-error)
  "Return non-nil if NODE is registered with the EDTS server."
  (member node (edts-get-nodes no-error)))

(defun edts-get-nodes (&optional no-error)
  "Return all nodes registered with the EDTS server. If NO-ERROR is
non-nil, don't report an error if the request fails."
  (let (nodes
        (res (edts-rest-get '("nodes") nil)))
    (if (equal (assoc 'result res) '(result "200" "OK"))
        (cdr (assoc 'nodes (cdr (assoc 'body res))))
      (unless no-error
        (null (edts-log-error "Unexpected reply: %s"
                              (cdr (assoc 'result res))))))))

(defun edts--node-memberp (node nodes)
  (some #'(lambda (reg-node) (string-match (concat node "@") reg-node))))

(defvar edts-node-name nil
  "Used to manually set the project node-name to use in a buffer
that is not part of a project")
(make-variable-buffer-local 'edts-node-name)

(defun edts-node-name ()
  "Return the sname of current buffer's project node."
  (condition-case ex
      (eproject-attribute :node-sname)
    ('error edts-node-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests

(provide 'edts)
