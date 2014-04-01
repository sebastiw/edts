;;; edts-complete-local-function-source.el ---
;;; Completion source for local functions

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Source

(defvar edts-complete-local-function-source
  '((candidates . edts-complete-local-function-candidates)
    (document   . edts-complete-local-function-doc)
    (init       . edts-complete-local-function-init)
    (symbol     . "f")
    (requires   . 0)
    (limit      . nil)
    ))

(defvar edts-complete-local-function-candidates nil
  "Current completions for local functions.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Candidate functions

(defun edts-complete-local-function-init ()
    "Initialize local function completions."
    (when (edts-complete-local-function-p)
      (case (ferl-point-inside-quotes)
        ('double-quoted nil) ; Don't complete inside strings
        (otherwise
         (edts-log-debug "Initializing local function completions")
         (setq edts-complete-local-function-candidates
               (mapcar #'car (ferl-local-functions)))))))

(defun edts-complete-local-function-candidates ()
  (case (ferl-point-inside-quotes)
    ('double-quoted  nil) ; Don't complete inside strings
    ('single-quoted (edts-complete-single-quoted-local-function-candidates))
    ('none          (edts-complete-normal-local-function-candidates))))

(defun edts-complete-normal-local-function-candidates ()
  "Produces the completion list for normal (unqoted) local functions."
  (when (edts-complete-local-function-p)
    (edts-log-debug "completing local functions")
    (edts-log-debug "completing local functions done")
    edts-complete-local-function-candidates))

(defun edts-complete-single-quoted-local-function-candidates ()
  "Produces the completion for single-qoted erlang terms, Same as normal
candidates, except we single-quote-terminate candidates."
  (mapcar
   #'ferl-single-quote-terminate
   (edts-complete-normal-local-function-candidates)))

(defun edts-complete-local-function-doc (candidate)
  "Find the documentation for CANDIDATE."
  (let* ((module   (ferl-get-module))
         (split    (split-string candidate "/"))
         (function (car split))
         (arity    (string-to-number (cadr split))))
    (condition-case ex
        (edts-man-extract-function-entry module function)
      ('error (edts-extract-doc-from-source module function arity)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conditions
;;

(defun edts-complete-local-function-p ()
  "Returns non-nil if the current `ac-prefix' can be completed with a local
function."
  (condition-case ex
      (let ((preceding (ferl-term-preceding-char)))
        (and
         (not (equal ?? preceding))
         (not (equal ?# preceding))
         ;; qualified calls to local functions are handled by the
         ;; exported-function source
         (not (equal ?: preceding))
         (string-match erlang-atom-regexp ac-prefix)))
    ('error nil)))

(provide 'edts-complete-local-function-source)
