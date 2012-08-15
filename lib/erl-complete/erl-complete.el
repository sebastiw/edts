(require 'auto-complete-config)
(require 'ferl)

(require 'erl-complete-variable-source)
(require 'erl-complete-local-function-source)
(require 'erl-complete-imported-function-source)
(require 'erl-complete-built-in-function-source)
(require 'erl-complete-exported-function-source)
(require 'erl-complete-module-source)
(require 'erl-complete-macro-source)
(require 'erl-complete-record-source)

(ac-config-default)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers

(defun erl-complete-point-inside-quotes ()
  "Returns 'double if point is inside double quotes, 'single if point is inside
single quotes and 'none otherwise. Relies on font-lock-string-face to work."
  (if (not (equal 'font-lock-string-face (get-text-property (point) 'face)))
      'none
      (save-excursion
        (let ((match (re-search-backward "['\\\"]")))
          (when match
            (let ((char          (char-after match))
                  (string-face-p (equal 'font-lock-string-face;
                                        (get-text-property (- match 1) 'face))))
           (cond
            ; we're inside a double quoted string if either:
            ; we hit a " and the preceding char is not string
            ; fontified.
            ((and (equal ?\" char) (not string-face-p)) 'double-quoted)
            ; or we hit a ' and the preceding char is still string
            ; fontified
            ((and (equal ?' char) string-face-p)              'double-quoted)
            ; we're inside a single quoted string if either:
            ; we hit a ' and the preceding char is not string
            ; fontified.
            ((and (equal ?' char) (not string-face-p))        'single-quoted)
            ; or we hit a " and the preceding char is still string
            ; fontified
            ((and (equal ?\" char) string-face-p)             'single-quoted)
            ; Otherwise we're not inside quotes
            (t                                                'none))))))))


(defun erl-complete-single-quote-terminate (str)
  "Removes any single quotes at start and end of `str' and adds one at the end
if not already present"
  (when      (string-match "^'" str) (setq str (substring str 1)))
  (when (not (string-match "'$" str)) (setq str (concat str "'")))
  str)

(defun symbol-at (&optional pos)
  "Returns the symbol at `pos', if any, otherwise nil."
  (save-excursion
    (when pos (goto-char pos))
    (thing-at-point 'symbol)))

(defun erl-complete-term-preceding-char ()
  "Returns the character preceding symbol, or if that is a single-quote, the
character before that."
  (let* ((char  (char-before ac-point)))
    (if (equal ?' char)
        (char-before (- ac-point 1))
        char)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup

(defun erl-complete-erlang-mode-hook ()
  "Buffer-setup for erl-complete."
  (setq ac-sources '(erl-complete-variable-source
                     erl-complete-local-function-source
                     erl-complete-imported-function-source
                     erl-complete-exported-function-source
                     erl-complete-built-in-function-source
                     erl-complete-module-source
                     erl-complete-macro-source
                     erl-complete-record-source))

  ;; this is to allow completion inside quoted atoms. As a side-effect we
  ;; get completion inside strings, which must be handled above.
  (make-local-variable 'ac-disable-faces)
  (setq ac-disable-faces (delete 'font-lock-string-face ac-disable-faces))
  )
(add-hook 'erlang-mode-hook 'erl-complete-erlang-mode-hook)

;; Default settings
(setq ac-ignore-case 'smart)
(setq ac-use-menu-map t)
(define-key ac-menu-map (kbd "C-n") 'ac-next)
(define-key ac-menu-map (kbd "C-p") 'ac-previous)

(add-to-list 'ac-modes 'erlang-mode)

(provide 'erl-complete)