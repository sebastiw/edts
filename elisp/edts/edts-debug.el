(defun edts-enter-debug-mode ()
  (interactive)
  (pop-to-buffer "*EDTS Debugger*" nil)
  (edts-debug-mode))

;; EDTS debug mode
(define-derived-mode edts-debug-mode fundamental-mode
  "EDTS debug mode"
  "Major mode for debugging interpreted Erlang code using EDTS"
  (setq buffer-read-only t)
  (setq mode-name "EDTS-debug")
)

(provide 'edts-debug-mode)
