(require 'auto-complete)
(require 'ferl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Source

(defvar erl-complete-local-function-source
  '((candidates . erl-complete-local-function-candidates)
    (document   . nil)
    (symbol     . "f")
    (requires   . nil)
    (limit      . nil)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Candidate functions

(defun erl-complete-local-function-candidates ()
  (case (erl-complete-point-inside-quotes)
    ('double-quoted  nil) ; Don't complete inside strings
    ('single-quoted (erl-complete-single-quoted-local-function-candidates))
    ('none          (erl-complete-normal-local-function-candidates))))

(defun erl-complete-normal-local-function-candidates ()
  "Produces the completion list for normal (unqoted) local functions."
  (when (erl-complete-local-function-p)
      (ferl-local-function-names)))

(defun erl-complete-single-quoted-local-function-candidates ()
  "Produces the completion for single-qoted erlang terms, Same as normal
candidates, except we single-quote-terminate candidates."
  (mapcar
   #'erl-complete-single-quote-terminate
   erl-complete-normal-local-function-candidates))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conditions
;;

(defun erl-complete-local-function-p ()
  "Returns non-nil if the current `ac-prefix' can be completed with a local
function."
  (let ((preceding (erl-complete-term-preceding-char)))
    (and
     (not (equal ?? preceding))
     (not (equal ?# preceding))
     (string-match erlang-atom-regexp ac-prefix))))

(provide 'erl-complete-local-function-source)