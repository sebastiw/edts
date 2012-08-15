(require 'auto-complete)
(require 'ferl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Source

(defvar erl-complete-record-source
  '((candidates . erl-complete-record-candidates)
    (document   . nil)
    (symbol     . "#")
    (requires   . nil)
    (limit      . nil)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Candidate functions

(defun erl-complete-record-candidates ()
  (case (erl-complete-point-inside-quotes)
    ('double-quoted  nil) ; Don't complete inside strings
    ('single-quoted (erl-complete-single-quoted-record-candidates))
    ('none          (erl-complete-normal-record-candidates))))

(defun erl-complete-normal-record-candidates ()
  "Produces the completion list for normal (unqoted) records. Unimplemented"
  ;; (when (erl-complete-record-p)
  ;;   ...)
  nil)

(defun erl-complete-single-quoted-record-candidates ()
  "Produces the completion for single-qoted erlang bifs, Same as normal
candidates, except we single-quote-terminate candidates."
  (mapcar
   #'erl-complete-single-quote-terminate
   erl-complete-normal-record-candidates))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conditions
;;

(defun erl-complete-record-p ()
  "Returns non-nil if the current `ac-prefix' can be completed with a built-in
function."
  (and
   (equal ?# (erl-complete-term-preceding-char))
   (string-match erlang-atom-regexp ac-prefix)))

(provide 'erl-complete-record-source)