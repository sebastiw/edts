(require 'auto-complete)
(require 'ferl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Source

(defvar erl-complete-built-in-function-source
  '((candidates . erl-complete-built-in-function-candidates)
    (document   . nil)
    (symbol     . "f")
    (requires   . nil)
    (limit      . nil)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Candidate functions

(defun erl-complete-built-in-function-candidates ()
  (case (erl-complete-point-inside-quotes)
    ('double-quoted  nil) ; Don't complete inside strings
    ('single-quoted (erl-complete-single-quoted-built-in-function-candidates))
    ('none          (erl-complete-normal-built-in-function-candidates))))

(defun erl-complete-normal-built-in-function-candidates ()
  "Produces the completion list for normal (unqoted) local functions."
  (when (erl-complete-built-in-function-p)
    erlang-int-bifs))

(defun erl-complete-single-quoted-built-in-function-candidates ()
  "Produces the completion for single-qoted erlang bifs, Same as normal
candidates, except we single-quote-terminate candidates."
  (mapcar
   #'erl-complete-single-quote-terminate
   erl-complete-normal-built-in-function-candidates))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conditions
;;

(defun erl-complete-built-in-function-p ()
  "Returns non-nil if the current `ac-prefix' can be completed with a built-in
function."
  (let ((preceding (erl-complete-term-preceding-char)))
    (and
     (not (equal ?? preceding))
     (not (equal ?# preceding))
     (string-match erlang-atom-regexp ac-prefix))))

(provide 'erl-complete-built-in-function-source)