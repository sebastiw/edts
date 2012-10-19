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
(defvar *edts-window-config* nil)

;; List of existing (both enabled and disabled) breakpoints
;; UNUSED right now
(defvar *edts-debug-breakpoints* '())

(defvar *edts-debug-last-visited-file* nil)

;; TODO: extend breakpoint toggling to add a breakpoint in every clause
;; of a given function when the line at point is a function clause.
(defun edts-debug-toggle-breakpoint ()
  "Enables or disables breakpoint at point"
  (interactive)
  (let* ((line-number (edts-line-number-at-point))
         (state (edts-toggle-breakpoint (get-node-name-from-debug-buffer)
                                        (erlang-get-module)
                                        (number-to-string line-number)))
         (result (cdr (assoc 'result state))))
    (update-breakpoints)
    (edts-log-info "Breakpoint %s at %s:%s"
                   result
                   (cdr (assoc 'module state))
                   (cdr (assoc 'line state)))))

(defun edts-debug-step ()
  "Steps (into) when debugging"
  (interactive)
  (handle-debugger-state (edts-step-into (get-node-name-from-debug-buffer))))

(defun edts-debug-step-out ()
  "Steps out of the current function when debugging"
  (interactive)
  (handle-debugger-state (edts-step-out (get-node-name-from-debug-buffer))))

(defun edts-debug-continue ()
  "Continues execution when debugging"
  (interactive)
  (edts-log-info "Continue")
  (handle-debugger-state (edts-continue (get-node-name-from-debug-buffer))))

(defun edts-debug-quit ()
  "Quits debug mode"
  (interactive)
  (edts-debug-stop (get-node-name-from-debug-buffer))
  (kill-debug-buffers)
  (erlang-mode)
  (set-window-configuration *edts-window-config*))

(defun edts-enter-debug-mode (&optional file line)
  "Convenience function to setup and enter debug mode"
  (interactive)
  (edts-debug-save-window-configuration)
  (edts-debug-enter-debug-buffer file line)
  (delete-other-windows)
  (edts-debug-mode)
  (create-auxiliary-buffers))

(defun edts-line-number-at-point ()
  "Get line number at point"
  (interactive)
  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)
      (1+ (count-lines 1 (point))))))

(defun edts-debug-save-window-configuration ()
  (if (not (equal (buffer-local-value 'major-mode (current-buffer)) 'edts-debug-mode))
      (setq *edts-window-config* (current-window-configuration))))

(defun edts-debug-enter-debug-buffer (file line)
  "Helper function to enter a debugger buffer with the contents of FILE"
  (if (and (not (null file))
           (stringp file))
      (progn (pop-to-buffer (make-debug-buffer-name file))
             (if (or (null *edts-debug-last-visited-file*)
                     (not (equal *edts-debug-last-visited-file* file)))
                 (progn
                   (setq buffer-read-only nil)
                   (erase-buffer)
                   (insert-file-contents file)
                   (setq buffer-read-only t)))
             (setq *edts-debug-last-visited-file* file))
    (progn
      (let ((file (buffer-file-name)))
        (pop-to-buffer (make-debug-buffer-name file))
        (erase-buffer)
        (insert-file-contents file))
      (setq *edts-debug-last-visited-file* nil)))
  (edts-face-remove-overlays '("edts-debug-current"))
  (if (and (not (null line))
           (numberp line))
      (edts-face-display-overlay 'edts-face-debug-current-line
                                 line
                                 "EDTS debugger current line"
                                 "edts-debug-current"
                                 20
                                 t
                                 t
                                 ))
  (setq *edts-debugger-buffer* (current-buffer))
  (update-breakpoints))

(defvar edts-debug-keymap
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
  (use-local-map edts-debug-keymap))

(defun create-auxiliary-buffers ()
  (let ((buffer-width 81))
    (split-window nil buffer-width 'left)
    (switch-to-buffer "*EDTS-Debugger Bindings*")
    (update-bindings '())
    (edts-debug-mode)
    (other-window 1)))

(defun kill-debug-buffers ()
  (dolist (buf (match-buffers #'(lambda (buffer)
                                (let* ((name (buffer-name buffer))
                                       (match
                                        (string-match "^*EDTS-Debugger."
                                                      name)))
                                  (or (null match) name)))))
    (kill-buffer buf)))

(defun update-bindings (bindings)
  (set-buffer "*EDTS-Debugger Bindings*")
  (with-writable-buffer
   (erase-buffer)
   (insert "Current bindings in scope:\n\n")
   (mapcar #'(lambda (binding)
               (insert (format "%s = %s\n"
                               (car binding)
                               (cdr binding))))
           bindings)))

;; TODO: proper formatting to present in a buffer.
(defun edts-format-bindings (bindings)
  (format t "~A" bindings))

(defun handle-debugger-state (reply)
  (let ((state (cdr (assoc 'state reply))))
    (cond ((equal state "break")
           (let ((file (cdr (assoc 'file reply)))
                 (module (cdr (assoc 'module reply)))
                 (line (cdr (assoc 'line reply)))
                 (bindings (cdr (assoc 'var_bindings reply))))
             (edts-log-info "Break at %s:%s" module line)
             (edts-enter-debug-mode file line)
             (update-bindings bindings)))
          ((equal state "idle")
           (edts-log-info "Finished."))
          ((equal state "error")
           (edts-log-info "Error:%s" (cdr (assoc 'message reply)))))))

(defun update-breakpoints ()
  "Display breakpoints in the buffer"
  (edts-face-remove-overlays '("edts-breakpoint"))
  (let ((breaks (edts-get-breakpoints (get-node-name-from-debug-buffer))))
    (dolist (b breaks)
      (let ((module (cdr (assoc 'module b)))
            (line (cdr (assoc 'line b)))
            (status (cdr (assoc 'status b))))
        (if (and (equal module (erlang-get-module))
                 (equal status "active"))
            (edts-face-display-overlay 'edts-face-breakpoint-enabled-line
                                       line "Breakpoint" "edts-breakpoint"
                                       10 t))))))


(defun make-debug-buffer-name (&optional filename)
  (let ((project (if (null filename)
                     (edts-project-file-project (buffer-file-name))
                   (edts-project-file-project filename))))
    (format "*EDTS-Debugger <%s>*" (edts-project-node-name project))))

(defun get-node-name-from-debug-buffer ()
  (let ((match (string-match "<\\(\\w+\\)>" (buffer-name))))
    (match-string 1 (buffer-name))))

(defun match-buffers (predicate)
  "Returns a list of buffers for which PREDICATE does not evaluate to T"
  (delq t
        (mapcar predicate (buffer-list))))

(defmacro with-writable-buffer (&rest body)
  "Evaluates BODY by marking the current buffer as writable and restoring its read-only status afterwards"
  `(let ((was-read-only buffer-read-only))
     (setq buffer-read-only nil)
     ,@body
     (setq buffer-read-only was-read-only)))

(provide 'edts-debug)
