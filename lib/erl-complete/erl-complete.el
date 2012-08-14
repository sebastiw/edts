(require 'auto-complete-config)
(require 'ferl)
(ac-config-default)

(defvar erl-complete
  '((candidates . erl-complete-candidates)))

(defun erl-complete-candidates ()
  (cond
   ((erl-complete-macro-p)             (erl-complete-macro))
   ((erl-complete-record-p)            (erl-complete-record))
   ((erl-complete-variable-p)          (erl-complete-variable))
   ((erl-complete-exported-function-p) (erl-complete-exported-function))
   ((erl-complete-atom-p)              (append
                                        (erl-complete-local-function)
                                        (erl-complete-module)
                                        (erl-complete-built-in-function)
                                        (erl-complete-imported-function)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conditions
;;
;; These conditions all assume that preceding items in the condition list are
;; unsatisfied.

(defun erl-complete-macro-p ()
  (equal ?? (erl-complete-term-preceding-char)))

(defun erl-complete-record-p ()
  (equal ?# (erl-complete-term-preceding-char)))

(defun erl-complete-variable-p ()
  (let ((case-fold-search nil))
    (string-match erlang-variable-regexp ac-prefix)))

(defun erl-complete-exported-function-p ()
  (when (equal ?: (erl-complete-term-preceding-char))
    (string-match erlang-atom-regexp (symbol-at (- ac-point 1)))))

(defun erl-complete-local-function-p ()
  (when (not (equal ?: (erl-complete-term-preceding-char)))
    (string-match erlang-atom-regexp ac-prefix)))

(defun erl-complete-atom-p ()
  (string-match erlang-atom-regexp ac-prefix))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Candidate generators

(defun erl-complete-macro ()
  "Generates the auto-complete candidate list for macros. Unimplemented"
  (message "completing macros")
  nil)

(defun erl-complete-record ()
  "Generates the auto-complete candidate list for records. Unimplemented"
  (message "completing records")
  nil)

(defun erl-complete-variable ()
  "Generates the auto-complete candidate list for variables. Matches variables
mentioned in current function, before current point."
  (message "completing variables")
  (save-excursion
    (when (erl-complete-variable-p)
      (let ((old-point  (point))
            (candidates ()))
        (ferl-beginning-of-function)
        (while (and (re-search-forward erlang-variable-regexp old-point t)
                    (< (match-end 0) old-point))
          (add-to-list 'candidates (thing-at-point 'symbol)))
        candidates))))

(defun erl-complete-exported-function ()
  "Generates the auto-complete candidate list for exported functions for the
relevant module."
  (message "completing exported functions")
  nil)

(defun erl-complete-local-function ()
  "Generates the auto-complete candidate list for functions defined in the
current module."
  (message "completing local functions %s" (ferl-local-function-names))
  (ferl-local-function-names))

(defun erl-complete-module ()
  "Generates the auto-complete candidate list for modules, using a distel node.
Uniplemented."
  (message "completing modules")
  nil)

(defun erl-complete-built-in-function ()
  "Generates the auto-complete candidate list for built-in functions.
Uniplemented."
  (message "completing built-ins")
  erlang-int-bifs)

(defun erl-complete-imported-function ()
  "Generates the auto-complete candidate list for functions imported into the
current module. Uniplemented."
  (message "completing imported functions %s" (erlang-get-import))
  ;; erlang get-import is on format ((mod1 (fun1 . arity) (fun2 . arity)))
  ;; So for each element in the list, skip the car (the module name) and for
  ;; each consecutive element (fun-arity pair) get the head (the function name).
  ;; Finally, append all the results.
  (apply #'append                                   ;; append final results.
         (mapcar                                    ;; for each module import.
          #'(lambda (mod) (mapcar #'car (cdr mod))) ;; get car of all but first.
          (erlang-get-import))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers

(defun symbol-at (&optional pos)
  (save-excursion
    (when pos (goto-char pos))
    (thing-at-point 'symbol)))

(defun erl-complete-term-preceding-char ()
  (let* ((char  (char-before ac-point)))
    (if (equal ?' char)
        (char-before (- ac-point 1))
        char)))


(defun &erl-complete-receive-completions ()
  (erl-receive ()
      ((['rex ['ok completions]]
	(setq try-erl-complete-cache completions))
       (other
	(message "Unexpected reply: %s" other)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup

(defun erl-complete-erlang-mode-hook ()
  (setq ac-sources '(erl-complete))
  (setq erl-complete-local-functions (ferl-local-function-names)))
(add-hook 'erlang-mode-hook 'erl-complete-erlang-mode-hook)

;; Default settings
(setq ac-ignore-case 'smart)
(setq ac-use-menu-map t)
(define-key ac-menu-map (kbd "C-n") 'ac-next)
(define-key ac-menu-map (kbd "C-p") 'ac-previous)

(add-to-list 'ac-modes 'erlang-mode)

(provide 'erl-complete)