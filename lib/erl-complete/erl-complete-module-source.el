(require 'auto-complete)
(require 'ferl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Source

(defvar erl-complete-module-source
  '((candidates . erl-complete-module-candidates)
    (document   . nil)
    (symbol     . "m")
    (requires   . nil)
    (limit      . nil)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Candidate functions

(defun erl-complete-module-candidates ()
  (case (erl-complete-point-inside-quotes)
    ('double-quoted  nil) ; Don't complete inside strings
    ('single-quoted (erl-complete-single-quoted-module-candidates))
    ('none          (erl-complete-normal-module-candidates))))

(defvar erl-complete-module-completions nil
  "The current completion for modules")

(defun erl-complete-normal-module-candidates ()
  "Produces the completion list for normal (unqoted) modules."
  (when (erl-complete-module-p)
    (let* ((resource (list "nodes" (symbol-name erl-nodename-cache) "modules"))
           (res      (edts-rest-get resource nil)))
      (if (equal (assoc 'result res) '(result "200" "OK"))
          (cdr (assoc 'body res))
          (message "Unexpected reply: %s" (cdr (assoc 'result res)))))))

(defun erl-complete-single-quoted-module-candidates ()
  "Produces the completion for single-qoted erlang modules, Same as normal
candidates, except we single-quote-terminate candidates."
  (mapcar
   #'erl-complete-single-quote-terminate
   erl-complete-normal-module-candidates))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conditions
;;

(defun erl-complete-module-p ()
  "Returns non-nil if the current `ac-prefix' can be completed with a module."
  (let ((preceding (erl-complete-term-preceding-char)))
    (and
     (not (equal ?? preceding))
     (not (equal ?# preceding))
     (string-match erlang-atom-regexp ac-prefix))))

(provide 'erl-complete-module-source)