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

(defun ferl-goto-line (line)
  "Non-interactive version of goto-line."
  (goto-char (point-min))
  (forward-line (1- line)))

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
    (beginning-of-line)
    (let ((constraint (point)))
      (move-end-of-line nil)
      (if (re-search-backward "[^[:space:]]" constraint 'move-point)
          (+ (point) 1)
          (point)))))

(defun ferl-get-module (&optional buffer)
  "Try to find the name of the erlang module in BUFFER, or current
buffer if no argument is given"
  (with-current-buffer (or buffer (current-buffer))
    (or (erlang-get-module)
        (erlang-get-module-from-file-name))))

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

(defun ferl-arity-at-point ()
  "Get the number of arguments in a function reference.
Should be called with point directly before the opening ( or /."
  (save-excursion
    (save-match-data
      (skip-chars-forward "[:space:]")
      (cond
       ((looking-at "/")
        (re-search-forward "/\s*[0-9]+" nil t)
        (ferl-slash-arity (match-string 0)))
       ((looking-at "(")
        (let ((start (+ (point) 1))
              (end   (- (progn (forward-sexp) (point)) 1)))
          (ferl-paren-arity (buffer-substring start end))))
       ('otherwise 0)))))

(defun ferl-slash-arity (str)
  "Return the arity of an argument-string after a slash."
  (string-to-number (substring str 1)))

(defconst ferl-block-start-regexp
  "\\<\\(case\\|if\\|begin\\|try\\|fun\\|receive\\)\\>"
  "Regexp to match the start of a new block")

(defun ferl-paren-arity (str)
  "Return the arity of an argument string within a parenthesis."
  (let ((block-depth 0)
        (arity 0)
        (in-arg nil))
    ;; increase arity if we're not inside an argument
    (flet ((maybe-inc-arity () (unless (or in-arg (> block-depth 0))
                                 (incf arity)
                                 (setq in-arg t))))
      (with-temp-buffer
        (set-syntax-table erlang-mode-syntax-table)
        (save-excursion (insert str))
        (skip-chars-forward "[:space:]")
        (while (< (point) (point-max))
          (cond
           ;; start of block
           ((looking-at ferl-block-start-regexp)
            (maybe-inc-arity)
            (incf block-depth)
            (forward-word))
           ;; end of block
           ((looking-at "\\<end\\>")
            (when (eq (decf block-depth) 0)
              (setq in-arg nil))
            (forward-word))
           ;; start of paired delimiter
           ((looking-at "[\\\"'<\\[{(]")
            (maybe-inc-arity)
            (forward-sexp))
           ;; comment
           ((eq (char-syntax (char-after (point))) ?<)
            (forward-comment 1))
           ;; end of argument
           ((looking-at ",")
            (setq in-arg nil)
            (forward-char))
           ;; any other character
           (t
            (maybe-inc-arity)
            (forward-char)))
          (skip-chars-forward "[:space:]")))
      arity)))


(when (member 'ert features)

  (ert-deftest ferl-paren-arity-test ()
    (should (eq 0 (ferl-paren-arity "")))
    (should (eq 1 (ferl-paren-arity "a")))
    (should (eq 1 (ferl-paren-arity "[]")))
    (should (eq 1 (ferl-paren-arity "a,")))
    (should (eq 2 (ferl-paren-arity "a,a")))
    (should (eq 1 (ferl-paren-arity ",a")))
    (should (eq 3 (ferl-paren-arity "aa,bb,cc")))

    (should (eq 1 (ferl-paren-arity "\"aa,bb\"")))
    (should (eq 2 (ferl-paren-arity "\"aa,bb\", cc")))
    (should (eq 2 (ferl-paren-arity "\"a'a,b'b\", cc")))
    (should (eq 1 (ferl-paren-arity "'aa,bb'")))
    (should (eq 2 (ferl-paren-arity "'aa,bb', cc")))
    (should (eq 2 (ferl-paren-arity "'a\"a,b\"b', cc")))
    (should (eq 3 (ferl-paren-arity "a,%a,b\nb, cc")))
    (should (eq 2 (ferl-paren-arity "\"a\\\"a,bb\", cc")))
    (should (eq 2 (ferl-paren-arity "a[a,b]b,cc")))
    (should (eq 2 (ferl-paren-arity "a\"a,b\"b,cc")))
    (should (eq 2 (ferl-paren-arity "[[a],{c,d}], ee")))
    (should (eq 2 (ferl-paren-arity "#a{a,b}, cc")))

    (should (eq 1 (ferl-paren-arity "fun() -> ok end")))
    (should (eq 2 (ferl-paren-arity "fun() -> ok end, fun() -> ok end")))
    (should (eq 1 (ferl-paren-arity "fun() -> begin%a, b\n ok end end")))
    )

  (ert-deftest slash-arity-test ()
    (should (eq 2 (ferl-slash-arity "/2")))))

;; Based on code from distel and erlang-mode
(defun ferl-mfa-at-point (&optional default-module)
  "Return the module and function under the point, or nil.

Should no explicit module name be present at the point, the
list of imported functions is searched. If there is still no result
use DEFAULT-MODULE."
  (when (null default-module) (setq default-module (ferl-get-module)))
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
                            (match-end (1+ erlang-atom-regexp-matches))))
                          arity)))
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
        (re (concat "^" function "\s*("))
        (match nil))
    (goto-char (point-min))
    (while (and (null match) (re-search-forward re nil t))
      (goto-char (match-beginning 0))
      (ferl-goto-end-of-call-name)
      (when (eq arity (ferl-arity-at-point))
        (setq match t)))
    (if match
        (beginning-of-line)
      (goto-char origin)
      (error "function %s/%s not found" function arity))))


(provide 'ferl)
