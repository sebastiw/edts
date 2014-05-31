;;; edts-complete-built-in-function-source.el ---
;;; Completion source for built-in functions

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

(require 'edts)
(require 'edts-log)
(require 'edts-man)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Source

(defvar edts-complete-built-in-function-source
  '((candidates . edts-complete-built-in-function-candidates)
    (document   . edts-complete-built-in-function-doc)
    (symbol     . "f")
    (requires   . 0)
    (limit      . nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Candidate functions

(defun edts-complete-built-in-function-candidates ()
  (case (ferl-point-inside-quotes)
    ('double-quoted  nil) ; Don't complete inside strings
    ('single-quoted (edts-complete-single-quoted-built-in-function-candidates))
    ('none          (edts-complete-normal-built-in-function-candidates))))

(defun edts-complete-normal-built-in-function-candidates ()
  "Produces the completion list for normal (unqoted) local functions."
  (when (edts-complete-built-in-function-p)
    (edts-log-debug "completing built-in functions")
    (let* ((completions edts-built-in-functions))
      (edts-log-debug "completing built-in functions done")
      completions)))

(defun edts-complete-single-quoted-built-in-function-candidates ()
  "Produces the completion for single-qoted erlang bifs, Same as normal
candidates, except we single-quote-terminate candidates."
  (mapcar
   #'ferl-single-quote-terminate
   (edts-complete-normal-built-in-function-candidates)))

(defun edts-complete-built-in-function-doc (candidate)
  (let* ((split  (split-string candidate "/"))
         (function   (car split))
         (arity  (string-to-number (cadr split))))
    (edts-man-extract-function-entry "erlang" function)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conditions
;;

(defun edts-complete-built-in-function-p ()
  "Returns non-nil if the current `ac-prefix' can be completed with a built-in
function."
  (condition-case ex
      (let ((preceding (ferl-term-preceding-char)))
        (and
         (not (equal ?? preceding))
         (not (equal ?# preceding))
         ;; qualified calls to built-in functions are handled by the
         ;; exported-function source
         (not (equal ?: preceding))
         (string-match erlang-atom-regexp ac-prefix)))
  ('error nil)))

(provide 'edts-complete-built-in-function-source)
