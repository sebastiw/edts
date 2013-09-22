;; Copyright 2013 Thomas Järvstrand <tjarvstrand@gmail.com>
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
;;
;; Debugger interaction code for EDTS

;; Window configuration to be restored when quitting debug mode

(require 'cl)

(require 'edts_debug-mode)

(require 'edts_debug-list-breakpoint-mode)
(require 'edts_debug-list-interpreted-mode)
(require 'edts_debug-list-processes-mode)

(defface edts_debug-process-location-face
  '((((class color) (background dark)) (:background "midnight blue"))
    (((class color) (background light)) (:background "light blue"))
    (t (:bold t)))
  "Face used for marking warning lines."
  :group 'edts)

(defface edts_debug-breakpoint-active-face
  '((((class color) (background dark)) (:background "dark slate gray"))
    (((class color) (background light)) (:background "yellow4")) ;; ?
    (t (:bold t)))
  "Face used for marking warning lines."
  :group 'edts)

(defface edts_debug-breakpoint-inactive-face
  '((((class color) (background dark)) (:background "#3f3f3f"))
    (((class color) (background light)) (:background "yellow4")) ;; ?
    (t (:bold t)))
  "Face used for marking warning lines."
  :group 'edts)


(defface edts_debug-breakpoint-inactive-face
  '((((class color) (background dark)) (:background "grey"))
    (((class color) (background light)) (:background "light grey"))
    (t (:bold t)))
  "Face used for marking warning lines."
  :group 'edts)

(defconst edts_debug-breakpoint-face-prio 800
  "Face priority for breakpoints.")

(defconst edts_debug-process-location-face-prio 801
  "Face priority for breakpoints.")

(defvar edts_debug--interpret-request-buffer nil
  "Buffer for requests to attach to the debugged process. One such
request should always be outstanding if we are not already attached.")

(defcustom edts_debug-auto-attach t
  "If non-nil, automatically enter debug-mode when a breakpoint is hit.")

(defvar edts_debug-node nil
  "Then node we are debugging on")

(defvar edts_debug-pid nil
  "The debugged pid")

(defvar edts_debug-overlay-arrow-position nil)
(add-to-list 'overlay-arrow-variable-list
             'edts_debug-overlay-arrow-position)

(defun edts_debug-init ()
  "Initialize edts_debug."
  ;; Keys
  (define-key edts-mode-map "\C-c\C-db"   'edts_debug-toggle-breakpoint)
  (define-key edts-mode-map "\C-c\C-di"   'edts_debug-interpret)
  (define-key edts-mode-map "\C-c\C-d\M-b" 'edts_debug-list-breakpoints)
  (define-key edts-mode-map "\C-c\C-d\M-i" 'edts_debug-list-interpreted)
  (define-key edts-mode-map "\C-c\C-d\M-p" 'edts_debug-list-processes)
  (add-hook 'edts-after-node-init-hook 'edts_debug-after-node-init-hook)
  (add-hook 'edts-node-down-hook 'edts_debug-node-down-hook)
  (add-hook 'edts-server-down-hook 'edts_debug-server-down-hook))

(defun edts_debug-after-node-init-hook ()
  "Hook to run after node initialization."
  (edts_debug-sync))

(defun edts_debug-node-down-hook (node)
  "Hook to run after node initialization."
  (let ((interpreted (assoc node edts_debug-interpreted-alist))
        (breakpoints (assoc node edts_debug-breakpoint-alist))
        (processes   (assoc node edts_debug-processes-alist)))
    (setq edts_debug-interpreted-alist
          (delete interpreted edts_debug-interpreted-alist))
    (setq edts_debug-breakpoint-alist
          (delete breakpoints edts_debug-breakpoint-alist))
    (setq edts_debug-processes-alist
          (delete processes edts_debug-processes-alist))
    (run-hooks 'edts_debug-after-sync-hook)))

(defun edts_debug-server-down-hook ()
  "Hook to run after node initialization."
  (setq edts_debug-interpreted-alist nil)
  (setq edts_debug-breakpoint-alist nil)
  (setq edts_debug-processes-alist nil)
  (run-hooks 'edts_debug-after-sync-hook))

(defun edts_debug-format-mode-line ()
  "Formats the edts_debug mode line string for display."
  (concat (propertize edts_debug-mode-line-string 'face `(:box t)) " "))

(defun edts_debug-buffer-init ()
  "edts_debug buffer-specific initialization."
  (add-to-list 'mode-line-buffer-identification
               '(edts-mode (:eval (edts_debug-format-mode-line)))
               t))

(defvar edts_debug-mode-line-string ""
  "The string with edts_debug related information to display in
the mode-line.")
(make-variable-buffer-local 'edts_debug-mode-line-string)

(defvar edts_debug-breakpoint-alist nil
  "Alist with breakpoints for each node. Each value is an alist with one
key for each interpreted module the value of which is a list of
breakpoints for that module.")

(defvar edts_debug-interpreted-alist nil
  "Alist with interpreted modules for each node. Each value is a list
of strings.")

(defvar edts_debug-processes-alist nil
  "Alist with all debugged processes for each node. Each value is a list
of strings.")

(defvar edts_debug-after-sync-hook nil
  "Hook to run after synchronizing debug information (interpreted
modules, breakpoints and debugged processes).")

(defun edts_debug-sync ()
  "Synchronize edts_debug data."
  (interactive)
  (edts_debug-sync-interpreted-alist)
  (edts_debug-sync-breakpoint-alist)
  (edts_debug-sync-processes-alist)
  (run-hooks 'edts_debug-after-sync-hook))

(defun edts_debug-event-handler (node class type info)
  "Handles erlang-side debugger events"
  (case type
    (interpret     (let ((module (cdr (assoc 'module info))))
                     (edts-log-info "%s is now interpreted on %s" module node))
                   (edts_debug-sync-interpreted-alist))
    (no_interpret  (let ((module (cdr (assoc 'module info))))
                     (edts-log-info "%s is no longer interpreted on %s"
                                    module
                                    node))
                   (edts_debug-sync-interpreted-alist))
    (new_break     (let ((module (cdr (assoc 'module info)))
                         (line (cdr (assoc 'line info))))
                     (edts-log-info "breakpoint set on %s:%s on %s"
                                    module
                                    line
                                    node)
                     (edts_debug-sync-breakpoint-alist)))
    (delete_break  (let ((module (cdr (assoc 'module info)))
                         (line (cdr (assoc 'line info))))
                     (edts-log-info "breakpoint unset on %s:%s on %s"
                                    module
                                    line
                                    node)
                     (edts_debug-sync-breakpoint-alist)))
    (break_options (let ((module (cdr (assoc 'module info)))
                         (line (cdr (assoc 'line info))))
                     (edts-log-info "breakpoint options updated on %s:%s on %s"
                                    module
                                    line
                                    node)
                     (edts_debug-sync-breakpoint-alist)))
    (no_break      (let ((module (cdr (assoc 'module info))))
                     (edts-log-info "All breakpoints in %s deleted on %s"
                                    module
                                    node)
                     (edts_debug-sync-breakpoint-alist)))
    (new_process   (edts_debug-sync-processes-alist))
    (new_status    (edts_debug-sync-processes-alist)))
  (run-hooks 'edts_debug-after-sync-hook))
(edts-event-register-handler 'edts_debug-event-handler 'edts_debug)

(defun edts_debug-update-buffers ()
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when edts-mode
        (let ((node   (edts-node-name))
              (module (ferl-get-module)))
          (when (and node module)
            (edts_debug-update-buffer-mode-line node module)
            (edts_debug-update-buffer-breakpoints node module)))))))
(add-hook 'edts_debug-after-sync-hook 'edts_debug-update-buffers)


(defun edts_debug-sync-interpreted-alist ()
  "Synchronizes `edts_debug-interpreted-alist'."
  (setq edts_debug-interpreted-alist
        (loop for node in (edts_debug-get-nodes)
              collect (cons node (edts_debug-interpreted-modules node)))))

(defun edts_debug-sync-breakpoint-alist ()
  "Synchronizes `edts_debug-breakpoint-alist'."
  (setq edts_debug-breakpoint-alist
        (loop for node in (edts_debug-get-nodes)
              for node-breakpoints = (edts_debug-all-breakpoints node)
              when node-breakpoints
              collect (loop
                       for breakpoint in node-breakpoints
                       with breakpoints
                       for module     = (cdr (assoc 'module breakpoint))
                       for old-elt    = (assoc module breakpoints)
                       ;; To get the breakpoint representation, delete the
                       ;; module key/value of the breakpoint alist (since that
                       ;; is the key in the outer alist)
                       for break-list = (cons
                                         (delete
                                          (cons 'module module) breakpoint)
                                         (cdr old-elt))
                       for new-elt    = (cons module break-list)
                       do (setq breakpoints
                                (cons new-elt
                                      (delete old-elt breakpoints)))
                       finally (return (cons node breakpoints))))))

(defun edts_debug-sync-processes-alist ()
  "Synchronizes `edts_debug-processes-alist'."
  (setq edts_debug-processes-alist
        (loop for node in (edts_debug-get-nodes)
              for procs = (edts_debug-all-processes node)
              collect (cons
                       node
                       (cdr (assoc 'processes procs))))))

(defun edts_debug-update-buffer-mode-line (node module)
  (if (member module (cdr (assoc node edts_debug-interpreted-alist)))
      (setq edts_debug-mode-line-string "Interpreted")
    (setq edts_debug-mode-line-string ""))
  (force-mode-line-update))

(defun edts_debug-update-buffer-breakpoints (node module)
  (edts-face-remove-overlays '(edts_debug-breakpoint))
  (let ((breaks (cdr (assoc module
                            (cdr (assoc node edts_debug-breakpoint-alist))))))
    (loop for break in breaks
        for line      = (cdr (assoc 'line      break))
        for status    = (cdr (assoc 'status    break))
        for trigger   = (cdr (assoc 'trigger   break))
        for condition = (cdr (assoc 'condition break))
        for face      = (if (string= status "active")
                            'edts_debug-breakpoint-active-face
                          'edts_debug-breakpoint-inactive-face)
        for fmt       = "Breakpoint status: %s, trigger: %s, condition: %s"
        do
        (edts-face-display-overlay face
                                   line
                                   (format fmt status trigger condition)
                                   'edts_debug-breakpoint
                                   edts_debug-breakpoint-face-prio
                                   t))))

(defun edts_debug-update-buffer-process-location (module line)
  (edts-face-remove-overlays '(edts_debug-process-location))
  (let* ((info        (edts_debug-process-info))
         (status      (cdr (assoc 'status info)))
         (proc-module (cdr (assoc 'module info)))
         (proc-line   (cdr (assoc 'line info))))
    (if (or (not (equal status "break"))
            (not (equal module proc-module)))
        (setq edts_debug-overlay-arrow-position nil)
      (let ((pos (ferl-position-at-beginning-of-line proc-line)))
        (setq edts_debug-overlay-arrow-position (set-marker (make-marker) pos))
        (edts-face-display-overlay 'edts_debug-process-location-face
                                   proc-line
                                   ""
                                   'edts_debug-process-location
                                   edts_debug-process-location-face-prio
                                   t)))))


(defun edts_debug-interpret (&optional node module interpret)
  "Set interpretation state for MODULE on NODE according to INTERPRET.
NODE and MODULE default to the values associated with current buffer.
If INTERPRET is nil stop intepreting; if it is t interpret MODULE; any
other value toggles interpretation, which is the default behaviour."
  (interactive (list
                nil
                nil
                'toggle))
  (let* ((module    (or module (ferl-get-module)))
         (node-name (or node (edts-node-name)))
         (interpret (cond
                     ((eq interpret t) "true")
                     ((null interpret) "false")
                     (t                "toggle")))
         (resource  (list "plugins"
                          "debugger"
                          "nodes" node-name
                          "modules" module))
         (rest-args (list (cons "interpret" interpret)))
         (reply     (edts-rest-post resource rest-args))
         (res       (assoc 'result reply)))
    (cond
     ((equal res '(result "403" "Forbidden"))
      (null (edts-log-error "%s is not interpretable" module)))
     ((not (equal res '(result "201" "Created")))
      (null (edts-log-error "Unexpected reply: %s" (cdr res)))))))

(defun edts_debug-toggle-breakpoint ()
  "Toggle breakpoint on current line."
  (interactive)
  (edts_debug-break nil nil nil 'toggle))

(defun edts_debug-break (&optional node module line break)
  "Set breakpoint state for LINE in MODULE on NODE according to
BREAK. NODE and MODULE default to the values associated with current
buffer. If BREAK is nil remove any breakpoint; if it is t set a
breakpoint if one doesn't already exist; any other value toggles
breakpoint existence at LINE, which is the default behaviour."
  (let* ((node-name (or node (edts-node-name)))
         (module    (or module (ferl-get-module)))
         (line      (or line (line-number-at-pos)))
         (break     (cond
                     ((eq break t) "true")
                     ((null break) "false")
                     (t            "toggle")))
         (resource  (list "plugins"
                          "debugger"
                          "nodes"   node-name
                          "modules" module
                          "breakpoints" (number-to-string line)))
         (rest-args (list (cons "break" break)))
         (reply     (edts-rest-post resource rest-args))
         (res       (assoc 'result reply)))
    (unless (equal res '(result "201" "Created"))
      (null (edts-log-error "Unexpected reply: %s" (cdr res))))))

(defun edts_debug-breakpoints (&optional node module)
  "Return a list of all breakpoint states in module on NODE. NODE and
MODULE default to the value associated with current buffer."
  (let* ((node-name (or node (edts-node-name)))
         (module    (or module (ferl-get-module)))
         (resource  (list "plugins"
                          "debugger"
                          "nodes"   node-name
                          "modules" module
                          "breakpoints"))
         (rest-args nil)
         (reply     (edts-rest-get resource rest-args))
         (res       (assoc 'result reply)))
    (if (not (equal res '(result "200" "OK")))
        (null
         (edts-log-error "Unexpected reply: %s" (cdr res)))
      (cdr (assoc 'body reply)))))

(defun edts_debug-all-breakpoints (&optional node)
  "Return a list of all breakpoint states on NODE. NODE defaults to the
value associated with current buffer."
  (let* ((node-name (or node (edts-node-name)))
         (resource  (list "plugins"
                          "debugger"
                          "nodes"   node-name
                          "breakpoints"))
         (rest-args nil)
         (reply     (edts-rest-get resource rest-args))
         (res       (assoc 'result reply)))
    (if (not (equal res '(result "200" "OK")))
        (null
         (edts-log-error "Unexpected reply: %s" (cdr res)))
      (cdr (assoc 'body reply)))))

(defun edts_debug-all-processes (&optional node)
  "Return a list of all breakpoint states on NODE. NODE defaults to the
value associated with current buffer."
  (let* ((node-name (or node (edts-node-name)))
         (resource  (list "plugins"
                          "debugger"
                          "nodes"   node-name
                          "processes"))
         (rest-args nil)
         (reply     (edts-rest-get resource rest-args))
         (res       (assoc 'result reply)))
    (if (not (equal res '(result "200" "OK")))
        (null
         (edts-log-error "Unexpected reply: %s" (cdr res)))
      (cdr (assoc 'body reply)))))


(defun edts_debug-interpretedp (&optional node module)
  "Return non-nil if MODULE is interpreted on NODE. NODE and MODULE
default to the values associated with current buffer."
  (let* ((module    (or module (ferl-get-module)))
         (node-name (or node (edts-node-name)))
         (resource  (list "plugins"
                          "debugger"
                          "nodes" node-name
                          "modules" module))
         (rest-args nil)
         (reply     (edts-rest-get resource rest-args))
         (res       (assoc 'result reply)))
    (if (not (equal res '(result "200" "OK")))
        (null
         (edts-log-error "Unexpected reply: %s" (cdr res)))
      (cdr (assoc 'interpreted (cdr (assoc 'body reply)))))))

(defun edts_debug-interpreted-modules (&optional node)
  "Return a list of all modules that are interpreted on NODE. NODE
default to the values associated with current buffer."
  (let* ((node-name (or node (edts-node-name)))
         (resource  (list "plugins"
                          "debugger"
                          "nodes" node-name
                          "modules"))
         (rest-args nil)
         (reply     (edts-rest-get resource rest-args))
         (res       (assoc 'result reply)))
    (if (not (equal res '(result "200" "OK")))
        (null
         (edts-log-error "Unexpected reply: %s" (cdr (assoc 'result res))))
      (cdr (assoc 'modules (cdr (assoc 'body reply)))))))

(defun edts_debug-continue (node-name pid)
  "Send a continue-command to the debugged process with PID on NODE."
  (edts_debug-command node-name pid 'continue))

(defun edts_debug-finish (node-name pid)
  "Send a continue-command to the debugged process with PID on NODE."
  (interactive)
  (edts_debug-command node-name pid 'finish))

(defun edts_debug-step-into (node-name pid)
  "Send a continue-command to the debugged process with PID on NODE."
  (interactive)
  (edts_debug-command node-name pid 'step_into))

(defun edts_debug-step-over (node-name pid)
  "Send a continue-command to the debugged process with PID on NODE."
  (interactive)
  (edts_debug-command node-name pid 'step_over))

(defun edts_debug-command (node-name pid command)
  "Send COMMAND to the debugged process with PID on NODE. Command is
one of continue, finish, step_into or step_over."
  (let* ((resource (list "plugins"   "debugger"
                         "nodes"     node-name
                         "processes" pid
                         "command"))
         (args  (list (cons "cmd" (symbol-name command))))
         (reply (edts-rest-post resource args))
         (res   (car (cdr (assoc 'result reply)))))
    (unless (equal res "200")
      (null (edts-log-error "Unexpected reply: %s" res)))))

(defun edts_debug-get-nodes ()
  "Return a list of all nodes to consider when issuing debugger commands"
  ;; this is a bit of a hack...
  (remove "edts" (edts-get-nodes)))

(defun edts_debug-attach (node pid)
  (unless (equal (edts_debug-process-info node pid 'status) "break")
    (error "Process %s on %s is not in a 'break' state" pid node))
  (setq edts_debug-node node)
  (setq edts_debug-pid pid)
  (edts_debug-mode-attach))

(defun edts_debug-process-info (&optional node pid prop)
  (let* ((node  (or node edts_debug-node))
         (pid   (or pid edts_debug-pid))
         (procs (cdr (assoc node edts_debug-processes-alist)))
         (info  (find-if #'(lambda (p) (string= (cdr (assoc 'pid p)) pid))
                procs)))
    (if prop
        (cdr (assoc prop info))
      info)))

(when (member 'ert features)

  (require 'edts-test)
  (edts-test-add-suite
   ;; Name
   edts_debug-suite
   ;; Setup
   (lambda ()
     (edts-test-setup-project edts-test-project1-directory
                              "test"
                              nil))
   ;; Teardown
   (lambda (setup-config)
     (edts-test-teardown-project edts-test-project1-directory)))

  (edts-test-case edts_debug-suite edts_debug-basic-test ()
    "Basic debugger setup test"
    (let ((eproject-prefer-subproject t))
      (find-file (car (edts-test-project1-modules)))

      (should-not (edts_debug-interpretedp))
      (edts_debug-interpret nil nil 't)
      (should (edts_debug-interpretedp))
      (should-not (edts_debug-breakpoints))
      (edts_debug-break nil nil nil t)
      (should (eq 1 (length (edts_debug-breakpoints)))))))

(provide 'edts_debug)
