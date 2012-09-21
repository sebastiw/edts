(defvar *edts-current-window-config* '())

(defvar *edts-debug-buffer* nil)

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
  (let* ((reply (edts-step-into))
         (state (cdr (assoc 'state reply))))
    (cond ((equal state "break")
           (let ((module (cdr (assoc 'module reply)))
                 (line (cdr (assoc 'line reply))))
             (message "Step into %s:%s" module line)
             (if (not (equal (concat module ".erl") (buffer-name)))
                 (edts-enter-debug-mode (concat module ".erl")))
             (goto-line line)))
          ((equal state "idle")
           (message "Finished.")))))

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
  (set-window-configuration (pop *edts-current-window-config*))
  ;(kill-buffer *edts-debug-buffer*)
  ;(setq *edts-debug-buffer* nil)
  )

(defun edts-enter-debug-mode (&optional buffer-name line)
  (interactive)
  (push (current-window-configuration)
        *edts-current-window-config*)
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
  ;(let ((debug-buffer (edts-wait-for-debugger)))
  ;  (set-process-query-on-exit-flag (get-buffer-process debug-buffer) nil)
  ;  (setq *edts-debug-buffer* debug-buffer))
  (use-local-map edts-debug-keymap))

(provide 'edts-debug)
