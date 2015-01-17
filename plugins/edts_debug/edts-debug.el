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

(require 'dash)

(require 'edts-api)
(require 'edts-event)
(require 'edts-log)
(require 'edts-plugin)

(defface edts-debug-process-location-face
  '((((class color) (background dark)) (:background "midnight blue"))
    (((class color) (background light)) (:background "light blue"))
    (t (:bold t)))
  "Face used for marking warning lines."
  :group 'edts)

(defface edts-debug-breakpoint-active-face
  '((((class color) (background dark)) (:background "dark slate gray"))
    (((class color) (background light)) (:background "yellow4")) ;; ?
    (t (:bold t)))
  "Face used for marking warning lines."
  :group 'edts)

(defface edts-debug-breakpoint-inactive-face
  '((((class color) (background dark)) (:background "#3f3f3f"))
    (((class color) (background light)) (:background "yellow4")) ;; ?
    (t (:bold t)))
  "Face used for marking warning lines."
  :group 'edts)

(defface edts-debug-breakpoint-inactive-face
  '((((class color) (background dark)) (:background "grey"))
    (((class color) (background light)) (:background "light grey"))
    (t (:bold t)))
  "Face used for marking warning lines."
  :group 'edts)

(defconst edts-debug-breakpoint-face-prio 800
  "Face priority for breakpoints.")

(defconst edts-debug-process-location-face-prio 801
  "Face priority for breakpoints.")

(defvar edts-debug--interpret-request-buffer nil
  "Buffer for requests to attach to the debugged process. One such
request should always be outstanding if we are not already attached.")

(defcustom edts-debug-auto-attach t
  "If non-nil, automatically enter debug-mode when a breakpoint is hit."
  :type 'boolean
  :group 'edts)

(defvar edts-debug-node nil
  "Then node we are debugging on")

(defvar edts-debug-pid nil
  "The debugged pid")

(defvar edts-debug-overlay-arrow-position nil)
(add-to-list 'overlay-arrow-variable-list
             'edts-debug-overlay-arrow-position)

(defvar edts-debug-attach-function nil
  "Function called to attach to a debugged process. Set by
`edts-debug-mode'.")

(defun edts-debug-init ()
  "Initialize edts-debug."
  ;; Keys
  (define-key edts-mode-map "\C-c\C-db"   'edts-debug-toggle-breakpoint)
  (define-key edts-mode-map "\C-c\C-di"   'edts-debug-toggle-interpreted)
  (define-key edts-mode-map "\C-c\C-d\M-b" 'edts-debug-list-breakpoints)
  (define-key edts-mode-map "\C-c\C-d\M-i" 'edts-debug-list-interpreted)
  (define-key edts-mode-map "\C-c\C-d\M-p" 'edts-debug-list-processes)
  (add-hook 'edts-api-after-node-init-hook 'edts-debug-after-node-init-hook)
  (add-hook 'edts-api-node-down-hook 'edts-debug-node-down-hook)
  (add-hook 'edts-api-server-down-hook 'edts-debug-server-down-hook))

(defun edts-debug-after-node-init-hook ()
  "Hook to run after node initialization."
  (edts-debug-sync))

(defun edts-debug-node-down-hook (node)
  "Hook to run after node goes down."
  (let ((interpreted (assoc node edts-debug-interpreted-alist))
        (breakpoints (assoc node edts-debug-breakpoint-alist))
        (processes   (assoc node edts-debug-processes-alist)))
    (setq edts-debug-interpreted-alist
          (delete interpreted edts-debug-interpreted-alist))
    (setq edts-debug-breakpoint-alist
          (delete breakpoints edts-debug-breakpoint-alist))
    (setq edts-debug-processes-alist
          (delete processes edts-debug-processes-alist))
    (run-hooks 'edts-debug-after-sync-hook)))

(defun edts-debug-server-down-hook ()
  "Hook to run after main server goes down."
  (setq edts-debug-interpreted-alist nil)
  (setq edts-debug-breakpoint-alist nil)
  (setq edts-debug-processes-alist nil)
  (run-hooks 'edts-debug-after-sync-hook))

(defun edts-debug-format-mode-line ()
  "Formats the edts-debug mode line string for display."
  (concat (propertize edts-debug-mode-line-string 'face `(:box t)) " "))

(defun edts-debug-buffer-init ()
  "edts-debug buffer-specific initialization."
  (add-to-list 'mode-line-buffer-identification
               '(edts-mode (:eval (edts-debug-format-mode-line)))
               t))

(defvar edts-debug-mode-line-string ""
  "The string with edts-debug related information to display in
the mode-line.")
(make-variable-buffer-local 'edts-debug-mode-line-string)

(defvar edts-debug-breakpoint-alist nil
  "Alist with breakpoints for each node. Each value is an alist with one
key for each interpreted module the value of which is a list of
breakpoints for that module.")

(defvar edts-debug-interpreted-alist nil
  "Alist with interpreted modules for each node. Each value is a list
of strings.")

(defvar edts-debug-processes-alist nil
  "Alist with all debugged processes for each node. Each value is a list
of strings.")

(defvar edts-debug-after-sync-hook nil
  "Hook to run after synchronizing debug information (interpreted
modules, breakpoints and debugged processes).")

(defun edts-debug-sync ()
  "Synchronize edts-debug data."
  (interactive)
  (edts-debug-sync-interpreted-alist)
  (edts-debug-sync-breakpoint-alist)
  (edts-debug-sync-processes-alist)
  (run-hooks 'edts-debug-after-sync-hook))

(defun edts-debug-event-handler (node class type info)
  "Handles erlang-side debugger events"
  (case type
    (interpret     (let ((module (cdr (assoc 'module info))))
                     (edts-log-info "%s is now interpreted on %s" module node))
                   (edts-debug-sync-interpreted-alist))
    (no_interpret  (let ((module (cdr (assoc 'module info))))
                     (edts-log-info "%s is no longer interpreted on %s"
                                    module
                                    node))
                   (edts-debug-sync-interpreted-alist))
    (new_break     (let ((module (cdr (assoc 'module info)))
                         (line (cdr (assoc 'line info))))
                     (edts-log-info "breakpoint set on %s:%s on %s"
                                    module
                                    line
                                    node)
                     (edts-debug-sync-breakpoint-alist)))
    (delete_break  (let ((module (cdr (assoc 'module info)))
                         (line (cdr (assoc 'line info))))
                     (edts-log-info "breakpoint unset on %s:%s on %s"
                                    module
                                    line
                                    node)
                     (edts-debug-sync-breakpoint-alist)))
    (break_options (let ((module (cdr (assoc 'module info)))
                         (line (cdr (assoc 'line info))))
                     (edts-log-info "breakpoint options updated on %s:%s on %s"
                                    module
                                    line
                                    node)
                     (edts-debug-sync-breakpoint-alist)))
    (no_break      (let ((module (cdr (assoc 'module info))))
                     (edts-log-info "All breakpoints in %s deleted on %s"
                                    module
                                    node)
                     (edts-debug-sync-breakpoint-alist)))
    (new_process   (edts-debug-sync-processes-alist))
    (new_status    (edts-debug-sync-processes-alist)
                   (edts-debug-handle-new-status node info)))
  (run-hooks 'edts-debug-after-sync-hook))
(edts-event-register-handler 'edts-debug-event-handler 'edts_debug)

(defun edts-debug-handle-new-status (node info)
  (let ((pid (cdr (assoc 'pid info))))
    (when (and (eq (intern (cdr (assoc 'status info))) 'break)
               (not edts-debug-pid)
               edts-debug-auto-attach)
      (edts-debug-attach node pid))))

(defun edts-debug-attach (node pid)
  (unless (equal (edts-debug-process-info node pid 'status) "break")
    (error "Process %s on %s is not in a 'break' state" pid node))
  (setq edts-debug-node node)
  (setq edts-debug-pid pid)
  (funcall edts-debug-attach-function))

(defun edts-debug-update-buffers ()
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when edts-mode
        (let ((node   (edts-api-node-name))
              (module (ferl-get-module)))
          (when (and node module (not (eq major-mode 'edts-debug-mode)))
            (edts-debug-update-buffer-mode-line node module)
            (edts-debug-update-buffer-breakpoints node module)))))))
(add-hook 'edts-debug-after-sync-hook 'edts-debug-update-buffers)

(defun edts-debug-sync-interpreted-alist ()
  "Synchronizes `edts-debug-interpreted-alist'."
  (setq edts-debug-interpreted-alist
        (loop for node in (edts-debug-get-nodes)
              collect (cons node (edts-debug-interpreted-modules node)))))

(defun edts-debug-sync-breakpoint-alist ()
  "Synchronizes `edts-debug-breakpoint-alist'."
  (setq edts-debug-breakpoint-alist
        (loop for node in (edts-debug-get-nodes)
              for node-breakpoints = (edts-debug-breakpoints node)
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

(defun edts-debug-sync-processes-alist ()
  "Synchronizes `edts-debug-processes-alist'."
  (setq edts-debug-processes-alist
        (loop for node in (edts-debug-get-nodes)
              for procs = (edts-debug-all-processes node)
              collect (cons node procs))))

(defun edts-debug-update-buffer-mode-line (node module)
  (if (member module (cdr (assoc node edts-debug-interpreted-alist)))
      (setq edts-debug-mode-line-string "Interpreted")
    (setq edts-debug-mode-line-string ""))
  (force-mode-line-update))

(defun edts-debug-update-buffer-breakpoints (node module)
  (edts-face-remove-overlays '(edts-debug-breakpoint))
  (let ((breaks (cdr (assoc module
                            (cdr (assoc node edts-debug-breakpoint-alist))))))
    (loop for break in breaks
        for line      = (cdr (assoc 'line      break))
        for status    = (cdr (assoc 'status    break))
        for trigger   = (cdr (assoc 'trigger   break))
        for condition = (cdr (assoc 'condition break))
        for face      = (if (string= status "active")
                            'edts-debug-breakpoint-active-face
                          'edts-debug-breakpoint-inactive-face)
        for fmt       = "Breakpoint status: %s, trigger: %s, condition: %s"
        do
        (edts-face-display-overlay face
                                   line
                                   (format fmt status trigger condition)
                                   'edts-debug-breakpoint
                                   edts-debug-breakpoint-face-prio
                                   t))))

(defun edts-debug-update-buffer-process-location (module line)
  (edts-face-remove-overlays '(edts-debug-process-location))
  (let* ((info        (edts-debug-process-info))
         (status      (cdr (assoc 'status info)))
         (proc-module (cdr (assoc 'module info)))
         (proc-line   (cdr (assoc 'line info))))
    (if (or (not (equal status "break"))
            (not (equal module proc-module)))
        (setq edts-debug-overlay-arrow-position nil)
      (let ((pos (ferl-position-at-beginning-of-line proc-line)))
        (goto-char pos)
        (back-to-indentation)
        (setq edts-debug-overlay-arrow-position (set-marker (make-marker) pos))
        (edts-face-display-overlay 'edts-debug-process-location-face
                                   proc-line
                                   ""
                                   'edts-debug-process-location
                                   edts-debug-process-location-face-prio
                                   t)))))

(defun edts-debug-toggle-interpreted ()
  "Toggle the interpretation state for module in current buffer."
  (interactive)
  (edts-debug-interpret nil nil 'toggle))

(defun edts-debug-interpret (&optional node module interpret)
  "Set interpretation state for MODULE on NODE according to INTERPRET.
NODE and MODULE default to the values associated with current buffer.
If INTERPRET is nil stop intepreting; if it is t interpret MODULE; any
other value toggles interpretation, which is the default behaviour when
called interactively."
  (let* ((module    (or module (ferl-get-module)))
         (node      (or node (edts-api-node-name)))
         (interpret (cond
                     ((eq interpret t) "true")
                     ((null interpret) "false")
                     (t                "toggle")))
         (args (list (cons "module"    module)
                     (cons "interpret" interpret))))
    (edts-plugin-call node 'edts_debug 'interpret_module args)))

(defun edts-debug-toggle-breakpoint ()
  "Toggle breakpoint on current line."
  (interactive)
  (edts-debug-break nil nil nil 'toggle))

(defun edts-debug-break (&optional node module line break)
  "Set breakpoint state for LINE in MODULE on NODE according to
BREAK. NODE and MODULE default to the values associated with current
buffer. If BREAK is nil remove any breakpoint; if it is t set a
breakpoint if one doesn't already exist; any other value toggles
breakpoint existence at LINE, which is the default behaviour."
  (let* ((node   (or node (edts-api-node-name)))
         (module (or module (ferl-get-module)))
         (line   (number-to-string (or line (line-number-at-pos))))
         (break  (cond
                  ((eq break t) "true")
                  ((null break) "false")
                  (t            "toggle")))
         (args   (list (cons "module" module)
                       (cons "line"   line)
                       (cons "break"  break))))
    (edts-plugin-call node 'edts_debug 'break args)))

(defun edts-debug-module-breakpoints (&optional node module)
  "Return a list of all breakpoint states in module on NODE. NODE and
MODULE default to the value associated with current buffer."
  (let* ((node   (or node (edts-api-node-name)))
         (module (or module (ferl-get-module)))
         (args   (list (cons "module" module))))
    (edts-plugin-call node 'edts_debug 'breakpoints args)))

(defun edts-debug-breakpoints (&optional node)
  "Return a list of all breakpoint states on NODE. NODE defaults to the
value associated with current buffer."
  (let* ((node (or node (edts-api-node-name))))
    (edts-plugin-call node 'edts_debug 'breakpoints)))

(defun edts-debug-all-processes (&optional node)
  "Return a list of all processes states on NODE. NODE defaults to the
value associated with current buffer."
  (let* ((node(or node (edts-api-node-name))))
    (edts-plugin-call node 'edts_debug 'processes)))

(defun edts-debug-interpretedp (&optional node module)
  "Return non-nil if MODULE is interpreted on NODE. NODE and MODULE
default to the values associated with current buffer."
  (let* ((module (or module (ferl-get-module)))
         (node   (or node (edts-api-node-name)))
         (args   (list (cons "module" module))))
    (edts-plugin-call node 'edts_debug 'module_interpreted_p args)))

(defun edts-debug-interpreted-modules (&optional node)
  "Return a list of all modules that are interpreted on NODE. NODE
default to the values associated with current buffer."
  (let* ((node (or node (edts-api-node-name))))
    (edts-plugin-call node 'edts_debug 'interpreted_modules)))

(defun edts-debug-continue (node-name pid)
  "Send a continue-command to the debugged process with PID on NODE."
  (edts-debug-command node-name pid 'continue))

(defun edts-debug-finish (node-name pid)
  "Send a continue-command to the debugged process with PID on NODE."
  (interactive)
  (edts-debug-command node-name pid 'finish))

(defun edts-debug-step-into (node-name pid)
  "Send a continue-command to the debugged process with PID on NODE."
  (interactive)
  (edts-debug-command node-name pid 'step_into))

(defun edts-debug-step-over (node-name pid)
  "Send a continue-command to the debugged process with PID on NODE."
  (interactive)
  (edts-debug-command node-name pid 'step_over))

(defun edts-debug-command (node pid command)
  "Send COMMAND to the debugged process with PID on NODE. Command is
one of continue, finish, step_into or step_over."
  (let* ((args  (list (cons "pid" pid))))
    (edts-plugin-call node 'edts_debug command args)))

(defun edts-debug-get-nodes ()
  "Return a list of all nodes to consider when issuing debugger commands"
  ;; this is a bit of a hack to avoid the debugger running on the main edts
  ;; server...
  (remove "edts" (edts-api-get-nodes)))

(defun edts-debug-get-bound-variables (node pid)
  "Return a list of all variables currently in PID's current scope on
NODE."
  (edts-plugin-call node 'edts_debug 'bound_variables `(("pid" ,pid))))


(defun edts-debug-get-bindings-pretty (node pid indent max-col)
  "Return a list of all variables (and their values) currently in PID's
current scope on NODE. Values are pretty-printed with INDENT spaces of
indentation and lines broken at MAX-COL."
  (edts-plugin-call node
                    'edts_debug
                    'get_bindings_pretty
                    `(("pid" ,pid)
                      ("indent" ,(number-to-string indent))
                      ("max_column" ,(number-to-string max-col)))))

(defun edts-debug-detach ()
  (setq edts-debug-node nil)
  (setq edts-debug-pid nil))

(defun edts-debug-process-info (&optional node pid prop)
  (let* ((node  (or node edts-debug-node))
         (pid   (or pid edts-debug-pid))
         (procs (cdr (assoc node edts-debug-processes-alist)))
         (info  (-first #'(lambda (p) (string= (cdr (assoc 'pid p)) pid))
                procs)))
    (if prop
        (cdr (assoc prop info))
      info)))

(provide 'edts-debug)
