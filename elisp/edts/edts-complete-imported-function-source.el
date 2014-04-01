;;; edts-complete-imported-function-source.el ---
;;; Completion source for imported functions

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Source

(defvar edts-complete-imported-function-source
  '((candidates . edts-complete-imported-function-candidates)
    (document   . nil)
    (symbol     . "f")
    (requires   . 0)
    (limit      . nil)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Candidate functions

(defun edts-complete-imported-function-candidates ()
  (case (ferl-point-inside-quotes)
    ('double-quoted  nil) ; Don't complete inside strings
    ('single-quoted (edts-complete-single-quoted-imported-function-candidates))
    ('none          (edts-complete-normal-imported-function-candidates))))

(defun edts-complete-normal-imported-function-candidates ()
  "Produces the completion list for normal (unqoted) imported functions."
  (when (edts-complete-imported-function-p)
    (edts-log-debug "completing imported functions")
    (let ((completions (edts-complete-imported-function)))
      (edts-log-debug "completing imported functions done")
      completions)))

(defun edts-complete-imported-function ()
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

(defun edts-complete-single-quoted-imported-function-candidates ()
  "Produces the completion for single-qoted erlang terms, Same as normal
candidates, except we single-quote-terminate candidates."
  (mapcar
   #'ferl-single-quote-terminate
   (edts-complete-normal-imported-function-candidates)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conditions
;;

(defun edts-complete-imported-function-p ()
  "Returns non-nil if the current `ac-prefix' can be completed with a imported
function."
  (condition-case ex
  (let ((preceding (ferl-term-preceding-char)))
    (and
     (not (equal ?? preceding))
     (not (equal ?# preceding))
     (not (equal ?: preceding))
     (string-match erlang-atom-regexp ac-prefix)))
  ('error nil)))

(provide 'edts-complete-imported-function-source)
