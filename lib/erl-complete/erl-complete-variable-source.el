(require 'auto-complete)
(require 'ferl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Source

(defvar erl-complete-variable-source
  '((candidates . erl-complete-variable-candidates)
    (document   . nil)
    (symbol     . "v")
    (requires   . nil)
    (limit      . nil)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Candidate functions

(defun erl-complete-variable-candidates ()
  (case (erl-complete-point-inside-quotes)
    ('double-quoted nil) ; Don't complete inside strings
    ('single-quoted nil) ; No single-quoted variables
    ('none          (erl-complete-normal-variable-candidates))))

(defun erl-complete-normal-variable-candidates ()
  "Generates the auto-complete candidate list for variables. Matches variables
mentioned in current function, before current point."
  (when (erl-complete-variable-p)
    (save-excursion
      (let ((old-point  (point))
            (candidates ()))
        (ferl-beginning-of-function)
        (while (and (re-search-forward erlang-variable-regexp old-point t)
                    (< (match-end 0) old-point))
          (add-to-list 'candidates (thing-at-point 'symbol)))
        candidates))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conditions
;;

(defun erl-complete-variable-p ()
  "Returns non-nil if the current `ac-prefix' can be completed with an
variable."
  (let ((case-fold-search nil)
        (preceding        (erl-complete-term-preceding-char)))
    (and
     (not (equal ?? preceding))
     (not (equal ?# preceding))
     (string-match erlang-variable-regexp ac-prefix))))

(provide 'erl-complete-variable-source)