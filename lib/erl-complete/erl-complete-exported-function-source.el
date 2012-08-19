(require 'auto-complete)
(require 'ferl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Source

(defvar erl-complete-exported-function-source
  '((candidates . erl-complete-exported-function-candidates)
    (document   . nil)
    (symbol     . "f")
    (requires   . nil)
    (limit      . nil)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Candidate functions

(defun erl-complete-exported-function-candidates ()
  (case (erl-complete-point-inside-quotes)
    ('double-quoted nil) ; Don't complete inside strings
    ('single-quoted (erl-complete-single-quoted-exported-function-candidates))
    ('none          (erl-complete-normal-exported-function-candidates))))


(defun erl-complete-normal-exported-function-candidates ()
  "Produces the completion list for normal (unqoted) modules."
  (when (erl-complete-exported-function-p)
    (erl-complete-exported-function-candidates)))

(defvar erl-complete-exported-function-completions nil
  "The current completion list for exported functions")

(defun erl-complete-exported-function ()
  "Generates the auto-complete candidate list for exported functions for the
relevant module."
  (let* ((module (symbol-at (- ac-point 1)))
         (node erl-nodename-cache)
         (completions
          (erl-spawn
            (erl-send-rpc node 'distel 'functions (list module ac-prefix))
            (&erl-complete-receive-exported-function-completions))))
    erl-complete-exported-function-completions))

(defun &erl-complete-receive-exported-function-completions ()
  (erl-receive ()
      ((['rex ['ok completions]]
        (setq erl-complete-exported-function-completions completions))
       (other
        (message "Unexpected reply: %s" other)))))

(defun erl-complete-single-quoted-exported-function-candidates ()
  "Produces the completion for single-qoted erlang modules, Same as normal
candidates, except we single-quote-terminate candidates."
  (mapcar
   #'erl-complete-single-quote-terminate
   erl-complete-normal-module-candidates))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conditions
;;

(defun erl-complete-exported-function-p ()
  "Returns non-nil if the current `ac-prefix' can be completed with an
exported function."
  (let ((case-fold-search nil))
    (and
     (equal ?: (erl-complete-term-preceding-char))
     (string-match erlang-exported-function-regexp ac-prefix))))

(provide 'erl-complete-exported-function-source)