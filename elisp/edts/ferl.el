;;; ferl.el --- Utilities for locating things and moving around in erlang code.

;; Copyright 2012-2013 Thomas Järvstrand <tjarvstrand@gmail.com>

;; Author: Thomas Järvstrand <thomas.jarvstrand@gmail.com>
;; Keywords: erlang
;; This file is not part of GNU Emacs.

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

(defun ferl-point-inside-quotes ()
  "Returns 'double if point is inside double quotes, 'single if point is inside
single quotes and 'none otherwise. Relies on font-lock-string-face to work."
  (if (not (equal 'font-lock-string-face (get-text-property (point) 'face)))
      'none
      (save-excursion
          (when (re-search-backward "\\([^\\]\\|^\\)\\(['\"]\\)" nil t)
            (let* ((start (match-beginning 2))
                   (char (char-after start))
                   (string-face-p (equal 'font-lock-string-face
                                         (get-text-property (1- start) 'face))))
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
            ('otherwise                                       'none)))))))


(defun ferl-single-quote-terminate (str)
  "Removes any single quotes at start and end of `str' and adds one at the end
if not already present"
  (when      (string-match "^'" str) (setq str (substring str 1)))
  (when (not (string-match "'$" str)) (setq str (concat str "'")))
  str)

(defun ferl-symbol-at (&optional pos)
  "Returns the symbol at `pos', if any, otherwise nil."
  (save-excursion
    (when pos (goto-char pos))
    (thing-at-point 'symbol)))

(defun ferl-term-preceding-char (&optional point)
  "Returns the character preceding symbol, or if that is a single-quote, the
character before that."
  (let* ((char  (char-before (or point ac-point))))
    (if (equal ?' char)
        (char-before (- ac-point 1))
        char)))


(defun ferl-goto-line (line)
  "Non-interactive version of goto-line."
  (goto-char (point-min))
  (forward-line (1- line)))

(defun ferl-position-at-beginning-of-line (line)
  "Returns the position at the first position of LINE."
  (save-excursion
    (goto-char (point-min))
    (line-beginning-position line)))

(defun ferl-position-at-end-of-line (line)
  "Returns the position at the end of LINE"
  (save-excursion
    (goto-char (point-min))
    (line-beginning-position (1+ line))))

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
        (and
         (buffer-file-name)
         (string= (file-name-extension (buffer-file-name buffer)) "erl")
         (erlang-get-module-from-file-name)))))

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

(defun ferl-is-point-in-export-list-p ()
  "Return t if point is inside an export definition list else nil"
  (save-excursion
    (let ((oldpoint (point)))
      (if (re-search-backward "^-export\\s-*(\\s-*\\[" nil t)
          (condition-case ex
              (progn
                (goto-char (1- (match-end 0)))
                (forward-sexp)
                (> (point) oldpoint))
            (error t))))))

(provide 'ferl)
