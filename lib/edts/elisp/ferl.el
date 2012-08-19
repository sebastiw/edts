;; Copyright 2012 Thomas Järvstrand <tjarvstrand@gmail.com>
;;
;; This file is part of EDTS.
;;
;; EDTS is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; EDTS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with EDTS. If not, see <http://www.gnu.org/licenses/>.
;;
;; Rudimentary project support for edts so that we can relate buffers to
;; projects and communicate with the correct nodes.
;;
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