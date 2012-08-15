(require 'auto-complete)
(require 'ferl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Source

(defvar erl-complete-macro-source
  '((candidates . erl-complete-macro-candidates)
    (document   . nil)
    (symbol     . "?")
    (requires   . nil)
    (limit      . nil)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Candidate functions

(defun erl-complete-macro-candidates ()
  (case (erl-complete-point-inside-quotes)
    ('double-quoted  nil) ; Don't complete inside strings
    ('single-quoted (erl-complete-single-quoted-macro-candidates))
    ('none          (erl-complete-normal-macro-candidates))))

(defun erl-complete-normal-macro-candidates ()
  "Produces the completion list for normal (unqoted) macros. Unimplemented"
  ;; (when (erl-complete-macro-p)
  ;;   ...)
  nil)

(defun erl-complete-single-quoted-macro-candidates ()
  "Produces the completion for single-qoted erlang bifs, Same as normal
candidates, except we single-quote-terminate candidates."
  (mapcar
   #'erl-complete-single-quote-terminate
   erl-complete-normal-macro-candidates))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conditions
;;

(defun erl-complete-macro-p ()
  "Returns non-nil if the current `ac-prefix' can be completed with a built-in
function."
  (and
   (equal ?? (erl-complete-term-preceding-char))
   (string-match erlang-atom-regexp ac-prefix)))

(provide 'erl-complete-macro-source)