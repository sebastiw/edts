;;; edts-complete-macro-source.el ---
;;; Completion source for macros

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

(require 'auto-complete)
(require 'ferl)

(require 'edts-api)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Source

(defvar edts-complete-macro-source
  '((candidates . edts-complete-macro-candidates)
    (document   . edts-complete-macro-doc)
    (prefix     . edts-complete-macro-prefix)
    (symbol     . "M")
    (requires   . 0)
    (limit      . nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Candidate functions

(defvar edts-complete-macro-cache nil
  "The current list of record completions.")
(make-variable-buffer-local 'edts-complete-macro-cache)
(add-hook 'after-save-hook #'(lambda () (setq edts-complete-macro-cache nil)))

(defun edts-complete-macro-candidates ()
  (case (ferl-point-inside-quotes)
    ('double-quoted  nil) ; Don't complete inside strings
    ('single-quoted (edts-complete-single-quoted-macro-candidates))
    ('none          (edts-complete-normal-macro-candidates))))

(defun edts-complete-normal-macro-candidates ()
  "Produces the completion list for normal (unqoted) macros."
  (when (edts-complete-macro-p ac-point)
    (edts-log-debug "completing macros")
    (let ((completions
           (or edts-complete-macro-cache
               (setq edts-complete-macro-cache
                     (edts-complete-find-module-macros)))))
      (edts-log-debug "completing macros done")
      (mapcar #'car completions))))

(defun edts-complete-single-quoted-macro-candidates ()
  "Produces the completion for single-qoted erlang bifs, Same as normal
candidates, except we single-quote-terminate candidates."
  (mapcar
   #'ferl-single-quote-terminate
   (edts-complete-normal-macro-candidates)))

(defun edts-complete-macro-doc (candidate)
  "Find the documentation for CANDIDATE."
  (cdr (assoc candidate edts-complete-macro-cache)))

(defun edts-complete-find-module-macros ()
  (let* ((files  (cons (buffer-file-name) (edts-api-get-includes)))
         (macros   (apply #'append
                          (mapcar #'edts-complete-parse-get-file-macros files)))
         (parsed  (edts-complete-parse-macros macros)))
    parsed))

(defun edts-complete-parse-macros (raw-macros)
  (when raw-macros
    (let* (;; Raw macro strings: '("MACRO1" "MACRO2(ARG21,..., ARG2X)")
           (arity-macros (mapcar #'(lambda (m) (cdr (assoc 'string m)))
                                 raw-macros))
           ;; (("MACRO1" 0) ("MACRO2" X))
           (arity-macro-strings (mapcar #'(lambda (m)
                                            (cons (cdr (assoc 'function m))
                                                  (cdr (assoc 'arity    m))))
                                        (edts-api-get-mfas arity-macros))))
      (loop for raw-m in raw-macros collect
            (let* ((name      (cdr (assoc 'name raw-m)))
                   (args      (cdr (assoc 'args raw-m)))
                   (value     (cdr (assoc 'value raw-m)))
                   (raw-str   (cdr (assoc 'string raw-m)))
                   (arity     (cdr (assoc-string name arity-macro-strings)))
                   (arity-str (format "%s/%s" name arity))
                   (doc       (format "%s -> %s" raw-str value)))
              (cons arity-str doc))))))

(defun edts-complete-parse-get-file-macros (file-name)
  (with-temp-buffer
    (insert-file-contents file-name)
    (edts-complete-get-macros)))

(defun edts-complete-get-macros ()
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (let ((macros nil))
        (while (re-search-forward edts-find-macro-definition-regexp nil t)
            (push
             `((string . ,(match-string-no-properties 1))
               (name   . ,(match-string-no-properties 2))
               (args   . ,(match-string-no-properties 5))
               (value  . ,(match-string-no-properties 6)))
             macros)
            (goto-char (match-end 0)))
        macros))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conditions
;;

(defun edts-complete-macro-prefix ()
  "Returns non-nil if the current `ac-prefix' or a prefix starting at
POINT or current could be completed with an macro."
  (cond ((and ac-point (edts-complete-macro-p ac-point)) ac-point)
        ((edts-complete-macro-p (point)) (point))))

(defun edts-complete-macro-p (point)
  "Returns non-nil if the current `ac-prefix' can be completed with a built-in
function."
  (condition-case ex
      (and
       (equal ?? (ferl-term-preceding-char point))
       (or (not ac-prefix) (string= "" ac-prefix)
           (string-match "\\(\\('.*\\)\\|\\([a-zA-Z_][a-zA-Z1-9_@]*\\)\\)"
                         ac-prefix)))
    ('error nil)))

(provide 'edts-complete-macro-source)
