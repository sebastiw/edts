(defvar *edts-current-window-config*
  (current-window-configuration))

(defun edts-debug-toggle-breakpoint ()
  (interactive)
  (let* ((line-number (edts-line-number-at-point))
         (state (edts-toggle-breakpoint (erlang-get-module)
                                        (number-to-string line-number))))
    (message "Breakpoint %s at %s:%s"
             (cdr (assoc 'result state))
             (cdr (assoc 'module state))
             (cdr (assoc 'line state)))))

(defun edts-debug-step ()
  (interactive)
  (let* ((state (edts-step-into))
         (line (cdr (assoc 'line state))))
    (message "Step into %s" line)
    (goto-line (1+ line))))

;(defun edts-debug-step-over ()
;  (interactive)
;  (edts-step-over)
;  (message "Step over"))

(defun edts-debug-continue ()
  (interactive)
  (edts-continue)
  (hl-line-mode nil)
  (message "Continue")
  (edts-wait-for-debugger))

(defun edts-debug-quit ()
  (interactive)
  (setq buffer-read-only nil)
  (hl-line-mode nil)
  (erlang-mode)
  (set-window-configuration *edts-current-window-config*))

(defun edts-enter-debug-mode (&optional buffer-name line)
  (interactive)
  (setq *edts-current-window-config*
        (current-window-configuration))
  (if (and (not (null buffer-name))
           (stringp buffer-name))
      (pop-to-buffer buffer-name nil))
  (edts-debug-mode)
  (goto-line line)
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
   ;(define-key map (kbd "o")   'edts-debug-step-over)
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

(provide 'edts-debug)
