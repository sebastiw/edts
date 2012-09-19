;; Copyright 2012 Thomas Järvstrand <tjarvstrand@gmail.com>
;;
;; This file is part of EDTS.
;;
;; EDTS is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; EDTS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with EDTS. If not, see <http://www.gnu.org/licenses/>.
;;
;; Rudimentary project support for edts so that we can relate buffers to
;; projects and communicate with the correct nodes.
;;
;; Utilities for locating things and moving around in erlang source code.

(defun ferl-position-at-line (line)
  "Returns the position at the first position of LINE."
  (save-excursion
    (goto-char (point-min))
    (beginning-of-line line)
    (point)))

(defun ferl-first-char-on-line-at (pos)
  "Returns the position of the first character on line at POS in
current-buffer."
  (save-excursion
    (goto-char pos)
    (back-to-indentation)
    (point)))

(defun ferl-last-char-on-line-at (pos)
  "Returns the position of the last character on line at POS in
current-buffer"
  (save-excursion
    (goto-char pos)
    (move-end-of-line nil)
    (re-search-backward "^\\|[^[:space:]]")))

(defun ferl-point-beginning-of-function ()
  "If point is inside an Erlang function, return the starting position
of that function, otherwise nil."
  (save-excursion (ferl-beginning-of-function) (point)))

(defun ferl-point-end-of-function ()
  "If point is inside an Erlang function, return the end position of that
   function, otherwise nil."
  ;; Fixme
  ;; Doesn't work if point is at the functions final character (full stop).
  ;; Crashes on unbalanced panrentheses.
  (save-excursion (erlang-end-of-function) (point)))

(defun ferl-beginning-of-function ()
  "Return the first point of the first erlang function before point."
  ;; fixme rewrite with looking-at
  (unless (looking-at (concat "^" erlang-atom-regexp "\\s-*("))
    (erlang-beginning-of-function)))

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

(defun ferl-local-functions ()
  "Enumerate all erlang functions in current buffer. Return a list
of (function-name . starting-point)."
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

;; Borrowed from distel
(defun ferl-goto-end-of-call-name ()
  "Go to the end of the function or module:function at point."
  ;; We basically just want to do forward-sexp iff we're not already
  ;; in the right place
  (unless (or (member (char-before) '(?  ?\t ?\n))
              (and (not (eobp))
                   (member (char-syntax (char-after (point))) '(?w ?_))))
    (backward-sexp))
  (forward-sexp)
  ;; Special case handling: On some emacs installations the (forward-sexp)
  ;; won't skip over the : in a remote function call. This is a workaround for
  ;; that. The issue seems to be that the emacs considers : to be punctuation
  ;; (syntax class '.'), whereas my emacs calls it a symbol separator (syntax
  ;; class '_'). FIXME.
  (when (eq (char-after) ?:)
    (forward-sexp)))

;; borrowed from distel
;;; FIXME: Merge with erlang.el!
(defun ferl-arity-at-point ()
  "Get the number of arguments in a function reference.
Should be called with point directly before the opening ( or /."
  ;; Adapted from erlang-get-function-arity.
  (save-excursion
    (save-match-data
      (cond ((looking-at "/")
             ;; form is /<n>, like the /2 in foo:bar/2
             (forward-char)
             (let ((start (point)))
               (if (re-search-forward "[0-9]+" nil t)
                   (ignore-errors (car (read-from-string (match-string 0)))))))
            ((looking-at "[\n\r ]*(")
             (goto-char (match-end 0))
             (condition-case nil
                 (let ((res 0)
                       (cont t))
                   (while cont
                     (cond ((eobp)
                            (setq res nil)
                            (setq cont nil))
                           ((looking-at "\\s *)")
                            (setq cont nil))
                           ((looking-at "\\s *\\($\\|%\\)")
                            (forward-line 1))
                           ((looking-at "\\s *,")
                            (incf res)
                            (goto-char (match-end 0)))
                           (t
                            (when (zerop res)
                              (incf res))
                            (forward-sexp 1))))
                   res)
               (error nil)))))))

;; Based on code from distel and erlang-mode
;; FIXME Butt-ugly function, split to cheek-size.
(defun ferl-mfa-at-point (&optional default-module)
  "Return the module and function under the point, or nil.

Should no explicit module name be present at the point, the
list of imported functions is searched. If there is still no result
use DEFAULT-MODULE."
  (when (null default-module) (setq default-module (erlang-get-module)))
  (save-excursion
    (save-match-data
      (ferl-goto-end-of-call-name)
      (let ((arity (ferl-arity-at-point))
            (res nil))
        (if (eq (char-syntax (following-char)) ? )
            (skip-chars-backward " \t"))
        (skip-chars-backward "a-zA-Z0-9_:'")
        (cond ((looking-at
                (eval-when-compile
                  (concat erlang-atom-regexp ":" erlang-atom-regexp)))
               (setq res (list
                          (erlang-remove-quotes
                           (erlang-buffer-substring
                            (match-beginning 1) (match-end 1)))
                          (erlang-remove-quotes
                           (erlang-buffer-substring
                            (match-beginning (1+ erlang-atom-regexp-matches))
                            (match-end (1+ erlang-atom-regexp-matches)))))))
              ((looking-at erlang-atom-regexp)
               (let ((fk (erlang-remove-quotes
                          (erlang-buffer-substring
                           (match-beginning 0) (match-end 0))))
                     (mod nil)
                     (imports (erlang-get-import)))
                 (while (and imports (null mod))
                   (if (eq arity (cdr (assoc fk (cdr (car imports)))))
                       (setq mod (car (car imports)))
                     (setq imports (cdr imports))))
                 (when (null mod)
                   (setq mod default-module))
                 (setq res (list mod fk arity)))))
        res))))

(defun ferl-search-function (function arity)
  "Goto the definition of FUNCTION/ARITY in the current buffer."
  (let ((origin (point))
        (re (concat "^" (edts-function-regexp function arity))))
    (goto-char (point-min))
    (if (re-search-forward re nil t)
        (goto-char (match-beginning 0))
        (goto-char origin))))


(provide 'ferl)
