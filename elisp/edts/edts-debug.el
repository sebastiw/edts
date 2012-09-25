;; Window config stack so we can push and pop during debugging to easily
;; change buffer
(defvar *edts-window-config-stack* '())

;; TODO: extend breakpoint toggling to add a breakpoint in every clause
;; of a given function when the line at point is a function clause.
(defun edts-debug-toggle-breakpoint ()
  "Enables or disables breakpoint at point"
  (interactive)
  (let* ((line-number (edts-line-number-at-point))
         (state (edts-toggle-breakpoint (erlang-get-module)
                                        (number-to-string line-number))))
    (message "Breakpoint %s at %s:%s"
             (cdr (assoc 'result state))
             (cdr (assoc 'module state))
             (cdr (assoc 'line state)))))

(defun edts-debug-step ()
  "Steps (into) when debugging"
  (interactive)
  (let* ((reply (edts-step-into))
         (state (cdr (assoc 'state reply))))
    (cond ((equal state "break")
           (let ((module (cdr (assoc 'module reply)))
                 (line (cdr (assoc 'line reply))))
             (message "Step into %s:%s" module line)
             ;; This isn't really what we want. We should visit-file
             ;; (or insert file contents into a specific buffer)
             (if (not (equal (concat module ".erl") (buffer-name)))
                 (edts-enter-debug-mode (concat module ".erl")))
             (goto-line line)))
          ((equal state "idle")
           (message "Finished.")))))

(defun edts-debug-step-out ()
  "Steps out of the current function when debugging"
  (interactive)
  (let* ((reply (edts-step-out))
         (state (cdr (assoc 'state reply))))
    (cond ((equal state "break")
           (let ((module (cdr (assoc 'module reply)))
                 (line (cdr (assoc 'line reply))))
             (message "Step out to %s:%s" module line)
             ;; This isn't really what we want. We should visit-file
             ;; (or insert file contents into a specific buffer)
             (if (not (equal (concat module ".erl") (buffer-name)))
                 (edts-enter-debug-mode (concat module ".erl")))
             (goto-line line)))
          ((equal state "idle")
           (message "Finished.")))))

(defun edts-debug-continue ()
  "Continues execution when debugging"
  (interactive)
  (edts-continue)
  (hl-line-mode nil)
  (message "Continue")
  (edts-wait-for-debugger))

(defun edts-debug-quit ()
  "Quits debug mode"
  (interactive)
  (edts-debug-stop)
  (setq buffer-read-only nil)
  (erlang-mode)
  (set-window-configuration (car (last *edts-window-config-stack*)))
  (hl-line-mode nil)
  )

(defun edts-enter-debug-mode (&optional buffer-name line)
  "Convenience function to setup and enter debug mode"
  (interactive)
  (push (current-window-configuration)
        *edts-window-config-stack*)
  (if (and (not (null buffer-name))
           (stringp buffer-name))
      (pop-to-buffer buffer-name nil))
  (edts-debug-mode)
  (if (and (not (null line))
           (numberp line))
      (goto-line line))
  (hl-line-mode t))

(defun edts-line-number-at-point ()
  "Get line number at point"
  (interactive)
  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)
      (1+ (count-lines 1 (point))))))

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
  (delete-other-windows)
  (setq buffer-read-only t)
  (setq mode-name "EDTS-debug")
  (use-local-map edts-debug-keymap))

;; TODO: proper formatting to present in a buffer.
(defun edts-format-bindings (bindings)
  (format t "~A" bindings))

(provide 'edts-debug)
