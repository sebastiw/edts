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
;; auto-complete source for imported erlang functions.

(require 'auto-complete)
(require 'ferl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Source

(defvar erl-complete-imported-function-source
  '((candidates . erl-complete-imported-function-candidates)
    (document   . nil)
    (symbol     . "f")
    (requires   . nil)
    (limit      . nil)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Candidate functions

(defun erl-complete-imported-function-candidates ()
  (case (erl-complete-point-inside-quotes)
    ('double-quoted  nil) ; Don't complete inside strings
    ('single-quoted (erl-complete-single-quoted-imported-function-candidates))
    ('none          (erl-complete-normal-imported-function-candidates))))

(defun erl-complete-normal-imported-function-candidates ()
  "Produces the completion list for normal (unqoted) imported functions."
  (when (erl-complete-imported-function-p)
    (erl-complete-imported-function)))

(defun erl-complete-imported-function ()
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

(defun erl-complete-single-quoted-imported-function-candidates ()
  "Produces the completion for single-qoted erlang terms, Same as normal
candidates, except we single-quote-terminate candidates."
  (mapcar
   #'erl-complete-single-quote-terminate
   erl-complete-normal-imported-function-candidates))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conditions
;;

(defun erl-complete-imported-function-p ()
  "Returns non-nil if the current `ac-prefix' can be completed with a imported
function."
  (let ((preceding (erl-complete-term-preceding-char)))
    (and
     (not (equal ?? preceding))
     (not (equal ?# preceding))
     (string-match erlang-atom-regexp ac-prefix))))

(provide 'erl-complete-imported-function-source)