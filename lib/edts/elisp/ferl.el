;; Utilities for locating things and moving around in erlang source code.

(defun ferl-point-beginning-of-function ()
  "If point is inside an Erlang function, return the starting position of that
   function, otherwise nil."
  (save-excursion (ferl-beginning-of-function) (point)))

(defun ferl-point-end-of-function ()
  "If point is inside an Erlang function, return the end position of that
   function, otherwise nil.
   NB. Doesn't work if point is at the functions final character (full stop)."
  (save-excursion (erlang-end-of-function) (point)))

(defun ferl-beginning-of-function ()
  "Return the first point of the first erlang function before point."
  ;; fixme rewrite with looking-at
  (let ((old-point (point))
        (beginning (progn (erlang-beginning-of-function) (point)))
        (end       (ferl-point-end-of-function)))
    (if (< end old-point)
        (progn (goto-char old-point))
        (progn (goto-char beginning)))))


(defun ferl-goto-previous-function ()
  (interactive)
  (erlang-beginning-of-function)
  ;; erlang-beginning-of-function doesn't distinguish between
  ;; functions and compiler directives, but erlang-get-function-name
  ;; does.
  (while (not (erlang-get-function-name))
    (erlang-beginning-of-function)))

(defun ferl-goto-next-function ()
  (interactive)
  (erlang-beginning-of-function -1)
  ;; erlang-beginning-of-function doesn't distinguish between
  ;; functions and compiler directives, but erlang-get-function-name
  ;; does.
  (while (not (erlang-get-function-name))
    (erlang-beginning-of-function -1)))

(defun ferl-goto-function()
  (interactive)
  (let* ((functions (ferl-functions))
         (names     (mapcar #'(lambda (el) (car el)) functions))
         (choice    (ido-completing-read "Function: " names))
         (start     (cdr (assoc choice functions)))
         )
    (goto-char start)))

(defun ferl-local-functions ()
  (save-excursion
    (goto-char (point-min))
    (let ((funs ())
          (exports   (erlang-get-export)))
      (while (erlang-beginning-of-function -1)
        (let* ((start    (point))
               (name      (erlang-get-function-name))
               (arity     (erlang-get-function-arity))
               (signature (format "%s/%s" name arity)))
          (when name
            ;; (when (erlang-function-exported-p name arity exports)
            ;;   (setq signature (concat signature "*")))
            (setq funs (cons (cons signature start) funs)))))
      funs)))

(defun ferl-local-function-names ()
  (save-excursion
    (goto-char (point-min))
    (let ((funs ()))
      (while (erlang-beginning-of-function -1)
        (let ((name     (erlang-get-function-name)))
          (when name (add-to-list 'funs name))))
      funs)))


(provide 'ferl)