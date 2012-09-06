(defvar *edts-current-window-config*
  (current-window-configuration))

(defun edts-debug-toggle-breakpoint ()
  (interactive)
  (let ((line-number (edts-line-number-at-point)))
    (message "Toggle breakpoint at line %s" line-number)))

(defun edts-debug-step ()
  (interactive)
  (message "Step"))

(defun edts-debug-step-over ()
  (interactive)
  (message "Step over"))

(defun edts-debug-continue ()
  (interactive)
  (message "Continue"))

(defun edts-debug-quit ()
  (interactive)
  (set-window-configuration *edts-current-window-config*))

(defun edts-enter-debug-mode ()
  (interactive)
  (setq *edts-current-window-config*
        (current-window-configuration))
  (pop-to-buffer "*EDTS Debugger*" nil)
  (edts-debug-mode))

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
    (define-key map (kbd "o")   'edts-debug-step-over)
    (define-key map (kbd "c")   'edts-debug-continue)
    (define-key map (kbd "q")   'edts-debug-quit)
    map))

;; EDTS debug mode
(define-derived-mode edts-debug-mode fundamental-mode
  "EDTS debug mode"
  "Major mode for debugging interpreted Erlang code using EDTS"
  (delete-other-windows)
  (setq buffer-read-only t)
  (setq mode-name "EDTS-debug")
  (use-local-map edts-debug-keymap))

(provide 'edts-debug)
