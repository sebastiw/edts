(require 'auto-complete-config)
(require 'ferl)
(ac-config-default)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Source

(defvar erl-complete-source
  '((candidates . erl-complete-candidates)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Top-level candidate functions

(defun erl-complete-candidates ()
  (case (point-inside-quotes)
    ('double-quoted  nil)
    ('single-quoted (erl-complete-single-quoted-candidates))
    ('none   (erl-complete-normal-candidates))))

(defun erl-complete-normal-candidates ()
  "Produces the completion string normal (unqoted) erlang terms."
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

(defun erl-complete-single-quoted-candidates ()
  "Produces the completion string single-qoted erlang terms, Same as normal
candidates, except we single-quote-terminate candidates and there is no variable
completion."
  (mapcar
   #'erl-complete-single-quote-terminate
   (cond
    ((erl-complete-macro-p)             (erl-complete-macro))
    ((erl-complete-record-p)            (erl-complete-record))
    ((erl-complete-exported-function-p) (erl-complete-exported-function))
    ((erl-complete-atom-p)              (append
                                         (erl-complete-local-function)
                                         (erl-complete-module)
                                         (erl-complete-built-in-function)
                                         (erl-complete-imported-function))))))

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
  nil)

(defun erl-complete-record ()
  "Generates the auto-complete candidate list for records. Unimplemented"
  nil)

(defun erl-complete-variable ()
  "Generates the auto-complete candidate list for variables. Matches variables
mentioned in current function, before current point."
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
relevant module. Unimplemented"
  nil)

(defun erl-complete-local-function ()
  "Generates the auto-complete candidate list for functions defined in the
current module."
  (ferl-local-function-names))

(defun erl-complete-module ()
  "Generates the auto-complete candidate list for modules, using a distel node.
Uniplemented."
  nil)

(defun erl-complete-built-in-function ()
  "Generates the auto-complete candidate list for built-in functions."
  erlang-int-bifs)

(defun erl-complete-imported-function ()
  "Generates the auto-complete candidate list for functions imported into the
current module."
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
  (setq ac-sources '(erl-complete-source))
  (setq erl-complete-local-functions (ferl-local-function-names))

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