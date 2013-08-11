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

(require 'edts-debug-list-breakpoint-mode)
(require 'edts-debug-list-interpreted-mode)

(defface edts-debug-breakpoint-active-face
  '((((class color) (background dark)) (:background "dark blue"))
    (((class color) (background light)) (:background "light blue"))
    (t (:bold t)))
  "Face used for marking warning lines."
  :group 'edts)

(defface edts-debug-breakpoint-inactive-face
  '((((class color) (background dark)) (:background "grey"))
    (((class color) (background light)) (:background "light grey"))
    (t (:bold t)))
  "Face used for marking warning lines."
  :group 'edts)


(defvar edts-debug-breakpoint-face-prio 800
  "Face priority for breakpoints.")

(defun edts-debug-init ()
  "Initialize edts-debug."
  ;; Keys
  (define-key edts-mode-map "\C-c\C-db"   'edts-debug-break)
  (define-key edts-mode-map "\C-c\C-di"   'edts-debug-interpret)
  (define-key edts-mode-map "\C-c\C-d\M-i" 'edts-debug-show-interpreted))

(defun edts-debug-buffer-init ()
  "edts-debug buffer-specific initialization."
  (add-to-list 'mode-line-buffer-identification
               '(edts-mode edts-debug-mode-line-info)
               't))

(defvar edts-debug-mode-line-info ""
  "The string with edts-debug related information to display in
the mode-line.")
(make-variable-buffer-local 'edts-debug-mode-line-info)

(defvar edts-debug-breakpoint-alist nil
  "Alist with breakpoints for each node. Each value is an alist with one
key for each interpreted module the value of which is a list of
breakpoints for that module.")

(defvar edts-debug-interpreted-alist nil
  "Alist with interpreted modules for each node. Each value is an list
of strings.")


(defun edts-debug-sync ()
  "Synchronize debug information between EDTS and the Emacs instance."
  (interactive)
  (edts-debug-sync-interpreted-alist)
  (edts-debug-list-interpreted-update)
  (edts-debug-sync-breakpoint-alist)
  (edts-debug-list-breakpoint-update)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when edts-mode
        (let ((node   (edts-node-name))
              (module (ferl-get-module)))
          (when (and node module)
            (edts-debug-update-buffer-info node module)))))))

(defun edts-debug-sync-interpreted-alist ()
  "Synchronizes `edts-debug-interpreted-alist'."
  (setq edts-debug-interpreted-alist
        (loop for node in (edts-get-nodes)
              collect (cons node (edts-debug-interpreted-modules node)))))

(defun edts-debug-sync-breakpoint-alist ()
  "Synchronizes `edts-debug-breakpoint-alist'."
  (setq edts-debug-breakpoint-alist
        (loop for node in (edts-get-nodes)
              for node-breakpoints = (edts-debug-all-breakpoints node)
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

(defun edts-debug-update-buffer-info (node module)
  (if (member module (cdr (assoc node edts-debug-interpreted-alist)))
      (setq edts-debug-mode-line-info "Interpreted ")
    (setq edts-debug-mode-line-info ""))
  (force-mode-line-update)

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

(defun edts-debug-interpret (&optional node module interpret)
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
     ((equal res '(result "201" "Created"))
      (let* ((interpreted (cdr (assoc 'interpreted (cdr (assoc 'body reply)))))
             (fmt (if interpreted
                      "%s is now interpreted on %s"
                    "%s is no longer interpreted on %s")))
        (edts-log-info fmt module node-name)
        (edts-debug-sync)))
     ((equal res '(result "403" "Forbidden"))
      (null (edts-log-error "%s is not interpretable" module)))
     (t
      (null
       (edts-log-error "Unexpected reply: %s" (cdr res)))))))

(defun edts-debug-break (&optional node module line break)
  "Set breakpoint state for LINE in MODULE on NODE according to
BREAK. NODE and MODULE default to the values associated with current
buffer. If BREAK is nil remove any breakpoint; if it is t set a
breakpoint if one doesn't already exist; any other value toggles
breakpoint existence at LINE, which is the default behaviour."
  (interactive (list nil
                     nil
                     nil
                     'toggle))
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
    (if (not (equal res '(result "201" "Created")))
        (null (edts-log-error "Unexpected reply: %s" (cdr res)))
      (if (cdr (assoc 'break (cdr (assoc 'body reply))))
          (edts-log-info "breakpoint set on %s:%s on %s" module line node-name)
        (edts-log-info "breakpoint unset on %s:%s on %s" module line node-name))
      (edts-debug-sync))))

(defun edts-debug-breakpoints (&optional node module)
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

(defun edts-debug-all-breakpoints (&optional node)
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


(defun edts-debug-interpretedp (&optional node module)
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

(defun edts-debug-interpreted-modules (&optional node)
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


(when (member 'ert features)
  (ert-deftest edts-debug-basic-test ()
    ;; Setup
    (edts-test-save-buffer-list
     (edts-test-with-config
      edts-test-project1-directory
      '(:name "test")
      (let ((eproject-prefer-subproject t)
            (file (car (edts-test-project1-modules))))
        (find-file file)

        ;; Test
        (should-not (edts-debug-interpretedp))
        (edts-debug-interpret nil nil 't)
        (should (edts-debug-interpretedp))
        (should-not (edts-debug-breakpoints))
        (edts-debug-break nil nil nil t)
        (should (eq 1 (length (edts-debug-breakpoints))))

        ;; Cleanup
        (edts-test-cleanup))))))


;; (defvar *edts-debug-window-config-to-restore* nil)

;; (defvar *edts-debug-last-visited-file* nil)

;; (defcustom edts-debug-interpret-after-saving t
;;   "Set to a non-NIL value if EDTS should automatically interpret a module
;; after save-and-compile"
;;   :group 'edts)

;; (defun edts-debug--is-node-interpreted (node-name)
;;   "Reports if the node for the current project is running interpreted code"
;;   (let* ((state (edts-is-node-interpreted node-name)))
;;     (eq (cdr (assoc 'state state)) t)))

;; (defun edts-debug-toggle-interpret-minor-mode ()
;;   (interactive)
;;   (mapcar #'(lambda (buffer)
;; 	      (with-current-buffer buffer
;; 		(when (and edts-mode (eproject-name))
;;                   (edts-int-mode 'toggle))))
;; 	  (buffer-list)))

;; ;; TODO: extend breakpoint toggling to add a breakpoint in every clause
;; ;; of a given function when the line at point is a function clause.
;; (defun edts-debug-toggle-breakpoint ()
;;   "Enables or disables breakpoint at point"
;;   (interactive)
;;   (let* ((line-number (edts-debug--line-number-at-point))
;;          (node-name  (or (edts-node-name)
;;                          (edts-debug-buffer-node-name)))
;;          (state (edts-toggle-breakpoint node-name
;;                                         (erlang-get-module)
;;                                         (number-to-string line-number)))
;;          (result (cdr (assoc 'result state))))
;;     (edts-debug-update-breakpoints)
;;     (edts-log-info "Breakpoint %s at %s:%s"
;;                    result
;;                    (cdr (assoc 'module state))
;;                    (cdr (assoc 'line state)))))

;; (defun edts-debug-step ()
;;   "Steps (into) when debugging"
;;   (interactive)
;;   (edts-log-info "Step")
;;   (edts-debug-handle-debugger-reply
;;    (edts-step-into (edts-debug-buffer-node-name))))

;; (defun edts-debug-step-out ()
;;   "Steps out of the current function when debugging"
;;   (interactive)
;;   (edts-log-info "Step out")
;;   (edts-debug-handle-debugger-reply
;;    (edts-step-out (edts-debug-buffer-node-name))))

;; (defun edts-debug-continue ()
;;   "Continues execution when debugging"
;;   (interactive)
;;   (edts-log-info "Continue")
;;   (edts-debug-handle-debugger-reply
;;    (edts-continue (edts-debug-buffer-node-name))))

;; (defun edts-debug-quit ()
;;   "Quits debug mode"
;;   (interactive)
;;   (edts-debug-stop (edts-debug-buffer-node-name))
;;   (edts-debug--kill-debug-buffers)
;;   (set-window-configuration *edts-debug-window-config-to-restore*)
;;   (setf *edts-debug-window-config-to-restore* nil)
;;   (edts-debug-update-breakpoints))

;; (defun edts-debug-start-debugging ()
;;   (interactive)
;;   (edts-debug-enter-debug-mode)
;;   (edts-wait-for-debugger (edts-debug-buffer-node-name)))

;; (defun edts-debug-enter-debug-mode (&optional file line)
;;   "Convenience function to setup and enter debug mode"
;;   (edts-debug-save-window-configuration)
;;   (edts-debug-enter-debug-buffer file line)
;;   (delete-other-windows)
;;   (edts-debug-mode)
;;   (edts-debug--create-auxiliary-buffers))

;; (defun edts-debug--line-number-at-point ()
;;   "Get line number at point"
;;   (interactive)
;;   (save-restriction
;;     (widen)
;;     (save-excursion
;;       (beginning-of-line)
;;       (1+ (count-lines 1 (point))))))

;; (defun edts-debug-save-window-configuration ()
;;   "Saves current window configuration if not currently in an EDTS-Debug buffer"
;;   (if (and (not (equal (buffer-local-value 'major-mode (current-buffer)) 'edts-debug-mode))
;;            (null *edts-debug-window-config-to-restore*))
;;       (setq *edts-debug-window-config-to-restore*
;;             (current-window-configuration))))

;; (defun edts-debug-enter-debug-buffer (file line)
;;   "Helper function to enter a debugger buffer with the contents of FILE"
;;   (if (and file (stringp file))
;;       (progn (pop-to-buffer (edts-debug-make-debug-buffer-name file))
;;              (when (not (equal *edts-debug-last-visited-file* file))
;;                (setq buffer-read-only nil)
;;                (erase-buffer)
;;                (insert-file-contents file)
;;                (setq buffer-read-only t))
;;              (setq *edts-debug-last-visited-file* file))
;;     (progn
;;       (let ((file (buffer-file-name)))
;;         (pop-to-buffer (edts-debug-make-debug-buffer-name file))
;;         (erase-buffer)
;;         (insert-file-contents file))
;;       (setq *edts-debug-last-visited-file* nil)))
;;   (edts-face-remove-overlays '("edts-debug-current-line"))
;;   (when (numberp line)
;;     (edts-face-display-overlay 'edts-face-debug-current-line
;;                                line
;;                                "EDTS debugger current line"
;;                                "edts-debug-current-line"
;;                                20
;;                                t))
;;   (setq *edts-debugger-buffer* (current-buffer))
;;   (edts-debug-update-breakpoints))


;; (defvar edts-debug-mode-keymap
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map (kbd "SPC") 'edts-debug-toggle-breakpoint)
;;     (define-key map (kbd "s")   'edts-debug-step)
;;     (define-key map (kbd "o")   'edts-debug-step-out)
;;     (define-key map (kbd "c")   'edts-debug-continue)
;;     (define-key map (kbd "q")   'edts-debug-quit)
;;     map))

;; (define-derived-mode edts-debug-mode erlang-mode
;;   "EDTS debug mode"
;;   "Major mode for debugging interpreted Erlang code using EDTS"
;;   (setq buffer-read-only t)
;;   (setq mode-name "EDTS-debug")
;;   (use-local-map edts-debug-mode-keymap))

;; (define-minor-mode edts-int-mode
;;   "Toggle code interpretation for the project node belonging to the current
;; buffer. This means all modules (except those belonging to OTP and to the
;; applications excluded explicity in the project's configuration will
;; be interpreted"
;;   :init-value nil
;;   :lighter " EDTS-interpreted"
;;   :group edts
;;   :require edts-mode
;;   :after-hook (let* ((node-name (or (edts-node-name)
;; 				    (edts-debug-buffer-node-name)))
;; 		     (exclusions (edts-project-interpretation-exclusions))
;; 		     (interpretedp (edts-debug--is-node-interpreted node-name)))
;; 		(if (and (not edts-int-mode) interpretedp)
;; 		    (edts-set-node-interpretation node-name nil exclusions)
;; 		  (progn (edts-log-info "Interpreting all loaded modules (this might take a while)...")
;; 			 (edts-set-node-interpretation node-name t exclusions))))
;; )

;; (defun edts-debug--create-auxiliary-buffers ()
;;   (let ((buffer-width 81))
;;     (split-window nil buffer-width 'left)
;;     (switch-to-buffer "*EDTS-Debugger Bindings*")
;;     (edts-debug--update-bindings '())
;;     (edts-debug-mode)
;;     (other-window 1)))

;; (defun edts-debug--kill-debug-buffers ()
;;   (dolist (buf (edts-debug--match-buffers
;;                 #'(lambda (buffer)
;;                     (let* ((name (buffer-name buffer))
;;                            (match
;;                             (string-match "^*EDTS-Debugger."
;;                                           name)))
;;                       (or (null match) name)))))
;;     (kill-buffer buf)))

;; (defun edts-debug--update-bindings (bindings)
;;   (with-writable-buffer "*EDTS-Debugger Bindings*"
;;    (erase-buffer)
;;    (insert "Current bindings in scope:\n\n")
;;    (mapcar #'(lambda (binding)
;;                (insert (format "%s = %s\n"
;;                                (car binding)
;;                                (cdr binding))))
;;            bindings)))

;; (defun edts-debug-handle-debugger-reply (reply)
;;   (let ((state (intern (cdr (assoc 'state reply)))))
;;     (case state
;;       ('break
;;        (let ((file (cdr (assoc 'file reply)))
;;              (module (cdr (assoc 'module reply)))
;;              (line (cdr (assoc 'line reply)))
;;              (bindings (cdr (assoc 'var_bindings reply))))
;;          (edts-log-info "Break at %s:%s" module line)
;;          (edts-debug-enter-debug-mode file line)
;;          (edts-debug--update-bindings bindings)))
;;       ('idle
;;        (edts-face-remove-overlays '("edts-debug-current-line"))
;;        (edts-log-info "Finished."))
;;       ('error
;;        (edts-log-info "Error:%s" (cdr (assoc 'message reply)))))))

;; (defun edts-debug-update-breakpoints ()
;;   "Display breakpoints in the buffer"
;;   (edts-face-remove-overlays '("edts-debug-breakpoint"))
;;   (let ((breaks (edts-get-breakpoints (or (edts-node-name)
;;                                           (edts-debug-buffer-node-name)))))
;;     (dolist (b breaks)
;;       (let ((module (cdr (assoc 'module b)))
;;             (line (cdr (assoc 'line b)))
;;             (status (cdr (assoc 'status b))))
;;         (if (and (equal module (erlang-get-module))
;;                  (equal status "active"))
;;             (edts-face-display-overlay 'edts-face-breakpoint-enabled-line
;;                                        line "Breakpoint" "edts-debug-breakpoint"
;;                                        10 t))))))


;; (defun edts-debug-make-debug-buffer-name (&optional file-name)
;;   (format "*EDTS-Debugger <%s>*" (edts-node-name)))

;; (defun edts-debug-buffer-node-name ()
;;   (save-match-data
;;     (let* ((name (buffer-name))
;;            (match (string-match "<\\([^)]+\\)>" name)))
;;       (match-string 1 name))))

;; (defun edts-debug--match-buffers (predicate)
;;   "Returns a list of buffers for which PREDICATE does not evaluate to T"
;;   (delq t
;;         (mapcar predicate (buffer-list))))

;; (defmacro with-writable-buffer (buffer-or-name &rest body)
;;   "Evaluates BODY by marking BUFFER-OR-NAME as writable and restoring its read-only status afterwards"
;;   `(with-current-buffer ,buffer-or-name
;;      (let ((was-read-only buffer-read-only))
;;        (setq buffer-read-only nil)
;;        ,@body
;;        (setq buffer-read-only was-read-only))))

(provide 'edts-debug)
