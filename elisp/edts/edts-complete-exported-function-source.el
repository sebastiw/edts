;;; edts-complete-exported-function-source.el ---
;;; Completion source for exported functions

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

(require 'edts-log)
(require 'edts-man)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Source

(defvar edts-complete-exported-function-source
  '((candidates . edts-complete-exported-function-candidates)
    (document   . edts-complete-exported-function-doc)
    (init       . edts-complete-exported-function-init)
    (symbol     . "f")
    (prefix     . edts-complete-exported-function-prefix)
    (requires   . 0)
    (limit      . nil)
    (cache)
    ))

(defvar edts-complete-exported-function-candidates nil
  "Current completions for exported functions.")
(make-variable-buffer-local 'edts-complete-exported-function-candidates)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Candidate functions

(defun edts-complete-exported-function-init ()
  "Initializes the list of exported function completions."
  (let ((point (or ac-point (point))))
    (when (edts-complete-exported-function-p point)
      (case (ferl-point-inside-quotes)
        ('double-quoted nil) ; Don't complete inside strings
        (otherwise
         (edts-log-debug "Initializing exported function completions")
         (let* ((module  (ferl-symbol-at (- point 1))))
           (setq edts-complete-exported-function-candidates
                 (edts-api-get-module-export-strings module t))))))))

(defun edts-complete-exported-function-candidates ()
  (case (ferl-point-inside-quotes)
    ('double-quoted nil) ; Don't complete inside strings
    ('single-quoted (edts-complete-single-quoted-exported-function-candidates))
    ('none          (edts-complete-normal-exported-function-candidates))))

(defun edts-complete-normal-exported-function-candidates ()
  "Produces the completion list for normal (unqoted) exported functions."
  (when (edts-complete-exported-function-p ac-point)
    (edts-log-debug "completing exported functions")
    (edts-log-debug "completing exported functions done")
    edts-complete-exported-function-candidates))

(defun edts-complete-single-quoted-exported-function-candidates ()
  "Produces the completion for single-qoted exported erlang functions.
Same as normal candidates, except we single-quote-terminate candidates."
  (mapcar
   #'ferl-single-quote-terminate
   (edts-complete-normal-exported-function-candidates)))

(defun edts-complete-exported-function-doc (candidate)
  "Find the documentation for CANDIDATE."
  (let* ((module   (ferl-symbol-at (- ac-point 1)))
         (split    (split-string candidate "/"))
         (function (car split))
         (arity    (string-to-number (cadr split))))
    (condition-case ex
        (edts-man-extract-function-entry module function)
        ('error (edts-extract-doc-from-source module function arity)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conditions
;;

(defun edts-complete-exported-function-prefix ()
  "Returns non-nil if the current `ac-prefix' or a prefix starting at
POINT or current could be completed with an exported function."
  (cond ((and ac-point (edts-complete-exported-function-p ac-point)) ac-point)
        ((edts-complete-exported-function-p (point)) (point))))

(defun edts-complete-exported-function-p (point)
  "Returns non-nil if a prefix starting at POINT could be completed with
an exported function."
  (condition-case ex
      (let ((case-fold-search nil))
        (and
         (equal ?: (ferl-term-preceding-char point))
         (string-match erlang-atom-regexp (ferl-symbol-at (- point 1)))))
      ('error nil)))

(provide 'edts-complete-exported-function-source)
