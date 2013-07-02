;; Copyright 2012 João Neves <sevenjp@gmail.com>
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
(defvar *edts-debug-window-config-to-restore* nil)

(defvar *edts-debug-last-visited-file* nil)

(defun edts-debug--is-node-interpreted (node-name)
  "Reports if the node for the current project is running interpreted code"
  (let* ((state (edts-is-node-interpreted node-name)))
    (eq (cdr (assoc 'state state)) t)))

(defun edts-debug-toggle-interpret-minor-mode ()
  (interactive)
  (mapcar #'(lambda (buffer)
	      (with-current-buffer buffer
		(when (and edts-mode (eproject-name))
                  (edts-int-mode 'toggle))))
	  (buffer-list)))

;; TODO: extend breakpoint toggling to add a breakpoint in every clause
;; of a given function when the line at point is a function clause.
(defun edts-debug-toggle-breakpoint ()
  "Enables or disables breakpoint at point"
  (interactive)
  (let* ((line-number (edts-debug--line-number-at-point))
         (node-name  (or (edts-node-name)
                         (edts-debug-buffer-node-name)))
         (state (edts-toggle-breakpoint node-name
                                        (erlang-get-module)
                                        (number-to-string line-number)))
         (result (cdr (assoc 'result state))))
    (edts-debug-update-breakpoints)
    (edts-log-info "Breakpoint %s at %s:%s"
                   result
                   (cdr (assoc 'module state))
                   (cdr (assoc 'line state)))))

(defun edts-debug-step ()
  "Steps (into) when debugging"
  (interactive)
  (edts-log-info "Step")
  (edts-debug-handle-debugger-reply
   (edts-step-into (edts-debug-buffer-node-name))))

(defun edts-debug-step-out ()
  "Steps out of the current function when debugging"
  (interactive)
  (edts-log-info "Step out")
  (edts-debug-handle-debugger-reply
   (edts-step-out (edts-debug-buffer-node-name))))

(defun edts-debug-continue ()
  "Continues execution when debugging"
  (interactive)
  (edts-log-info "Continue")
  (edts-debug-handle-debugger-reply
   (edts-continue (edts-debug-buffer-node-name))))

(defun edts-debug-quit ()
  "Quits debug mode"
  (interactive)
  (edts-debug-stop (edts-debug-buffer-node-name))
  (edts-debug--kill-debug-buffers)
  (set-window-configuration *edts-debug-window-config-to-restore*)
  (setf *edts-debug-window-config-to-restore* nil)
  (edts-debug-update-breakpoints))

(defun edts-debug-start-debugging ()
  (interactive)
  (edts-debug-enter-debug-mode)
  (edts-wait-for-debugger (edts-debug-buffer-node-name)))

(defun edts-debug-enter-debug-mode (&optional file line)
  "Convenience function to setup and enter debug mode"
  (edts-debug-save-window-configuration)
  (edts-debug-enter-debug-buffer file line)
  (delete-other-windows)
  (edts-debug-mode)
  (edts-debug--create-auxiliary-buffers))

(defun edts-debug--line-number-at-point ()
  "Get line number at point"
  (interactive)
  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)
      (1+ (count-lines 1 (point))))))

(defun edts-debug-save-window-configuration ()
  "Saves current window configuration if not currently in an EDTS-Debug buffer"
  (if (and (not (equal (buffer-local-value 'major-mode (current-buffer)) 'edts-debug-mode))
           (null *edts-debug-window-config-to-restore*))
      (setq *edts-debug-window-config-to-restore*
            (current-window-configuration))))

(defun edts-debug-enter-debug-buffer (file line)
  "Helper function to enter a debugger buffer with the contents of FILE"
  (if (and file (stringp file))
      (progn (pop-to-buffer (edts-debug-make-debug-buffer-name file))
             (when (not (equal *edts-debug-last-visited-file* file))
               (setq buffer-read-only nil)
               (erase-buffer)
               (insert-file-contents file)
               (setq buffer-read-only t))
             (setq *edts-debug-last-visited-file* file))
    (progn
      (let ((file (buffer-file-name)))
        (pop-to-buffer (edts-debug-make-debug-buffer-name file))
        (erase-buffer)
        (insert-file-contents file))
      (setq *edts-debug-last-visited-file* nil)))
  (edts-face-remove-overlays '("edts-debug-current-line"))
  (when (numberp line)
    (edts-face-display-overlay 'edts-face-debug-current-line
                               line
                               "EDTS debugger current line"
                               "edts-debug-current-line"
                               20
                               t))
  (setq *edts-debugger-buffer* (current-buffer))
  (edts-debug-update-breakpoints))

(defun edts-debug-keymap ()
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-d\C-d" 'edts-debug-start-debugging)
    (define-key map "\C-c\C-di"    'edts-debug-toggle-interpret-minor-mode)
    (define-key map "\C-c\C-db"    'edts-debug-toggle-breakpoint)
    map))

(defvar edts-debug-mode-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "SPC") 'edts-debug-toggle-breakpoint)
    (define-key map (kbd "s")   'edts-debug-step)
    (define-key map (kbd "o")   'edts-debug-step-out)
    (define-key map (kbd "c")   'edts-debug-continue)
    (define-key map (kbd "q")   'edts-debug-quit)
    map))

;; EDTS debug mode
(define-derived-mode edts-debug-mode erlang-mode
  "EDTS debug mode"
  "Major mode for debugging interpreted Erlang code using EDTS"
  (setq buffer-read-only t)
  (setq mode-name "EDTS-debug")
  (use-local-map edts-debug-mode-keymap))

(define-minor-mode edts-int-mode
  "Toggle code interpretation for the project node belonging to the current
buffer. This means all modules (except those belonging to OTP and to the
applications excluded explicity in the project's configuration will
be interpreted"
  :init-value nil
  :lighter " EDTS-interpreted"
  :group edts
  :require edts-mode
  :after-hook (let* ((node-name (or (edts-node-name)
				    (edts-debug-buffer-node-name)))
		     (exclusions (edts-project-interpretation-exclusions))
		     (interpretedp (edts-debug--is-node-interpreted node-name)))
		(if (and (not edts-int-mode) interpretedp)
		    (edts-set-node-interpretation node-name nil exclusions)
		  (progn (edts-log-info "Interpreting all loaded modules (this might take a while)...")
			 (edts-set-node-interpretation node-name t exclusions))))
)

(defun edts-debug--create-auxiliary-buffers ()
  (let ((buffer-width 81))
    (split-window nil buffer-width 'left)
    (switch-to-buffer "*EDTS-Debugger Bindings*")
    (edts-debug--update-bindings '())
    (edts-debug-mode)
    (other-window 1)))

(defun edts-debug--kill-debug-buffers ()
  (dolist (buf (edts-debug--match-buffers
                #'(lambda (buffer)
                    (let* ((name (buffer-name buffer))
                           (match
                            (string-match "^*EDTS-Debugger."
                                          name)))
                      (or (null match) name)))))
    (kill-buffer buf)))

(defun edts-debug--update-bindings (bindings)
  (with-writable-buffer "*EDTS-Debugger Bindings*"
   (erase-buffer)
   (insert "Current bindings in scope:\n\n")
   (mapcar #'(lambda (binding)
               (insert (format "%s = %s\n"
                               (car binding)
                               (cdr binding))))
           bindings)))

(defun edts-debug-handle-debugger-reply (reply)
  (let ((state (intern (cdr (assoc 'state reply)))))
    (case state
      ('break
       (let ((file (cdr (assoc 'file reply)))
             (module (cdr (assoc 'module reply)))
             (line (cdr (assoc 'line reply)))
             (bindings (cdr (assoc 'var_bindings reply))))
         (edts-log-info "Break at %s:%s" module line)
         (edts-debug-enter-debug-mode file line)
         (edts-debug--update-bindings bindings)))
      ('idle
       (edts-face-remove-overlays '("edts-debug-current-line"))
       (edts-log-info "Finished."))
      ('error
       (edts-log-info "Error:%s" (cdr (assoc 'message reply)))))))

(defun edts-debug-update-breakpoints ()
  "Display breakpoints in the buffer"
  (edts-face-remove-overlays '("edts-debug-breakpoint"))
  (let ((breaks (edts-get-breakpoints (or (edts-node-name)
                                          (edts-debug-buffer-node-name)))))
    (dolist (b breaks)
      (let ((module (cdr (assoc 'module b)))
            (line (cdr (assoc 'line b)))
            (status (cdr (assoc 'status b))))
        (if (and (equal module (erlang-get-module))
                 (equal status "active"))
            (edts-face-display-overlay 'edts-face-breakpoint-enabled-line
                                       line "Breakpoint" "edts-debug-breakpoint"
                                       10 t))))))


(defun edts-debug-make-debug-buffer-name (&optional file-name)
  (format "*EDTS-Debugger <%s>*" (edts-node-name)))

(defun edts-debug-buffer-node-name ()
  (save-match-data
    (let* ((name (buffer-name))
           (match (string-match "<\\([^)]+\\)>" name)))
      (match-string 1 name))))

(defun edts-debug--match-buffers (predicate)
  "Returns a list of buffers for which PREDICATE does not evaluate to T"
  (delq t
        (mapcar predicate (buffer-list))))

(defmacro with-writable-buffer (buffer-or-name &rest body)
  "Evaluates BODY by marking BUFFER-OR-NAME as writable and restoring its read-only status afterwards"
  `(with-current-buffer ,buffer-or-name
     (let ((was-read-only buffer-read-only))
       (setq buffer-read-only nil)
       ,@body
       (setq buffer-read-only was-read-only))))

(provide 'edts-debug)
