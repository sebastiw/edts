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
;; auto-complete source for built-in erlang functions.

(require 'auto-complete)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Source

(defvar edts-complete-keyword-source
  '((candidates . edts-complete-keyword-candidates)
    (symbol     . "k")
    (requires   . nil)
    (limit      . nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Candidate functions

(defun edts-complete-keyword-candidates ()
  (case (edts-complete-point-inside-quotes)
    ('double-quoted  nil) ; Don't complete inside strings
    ('single-quoted (edts-complete-single-quoted-keyword-candidates))
    ('none          (edts-complete-normal-keyword-candidates))))

(defun edts-complete-normal-keyword-candidates ()
  "Produces the completion list for normal (unqoted) local functions."
  (when (edts-complete-keyword-p)
    (edts-log-debug "completing keywords")
    (let* ((completions erlang-keywords))
      (edts-log-debug "completing keywords done")
      completions)))

(defun edts-complete-single-quoted-keyword-candidates ()
  "Produces the completion for single-qoted erlang bifs, Same as normal
candidates, except we single-quote-terminate candidates."
  (mapcar
   #'edts-complete-single-quote-terminate
   (edts-complete-normal-keyword-candidates)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conditions
;;

(defun edts-complete-keyword-p ()
  "Returns non-nil if the current `ac-prefix' can be completed with a keyword
function."
  (let ((preceding (edts-complete-term-preceding-char)))
    (and
     (not (member preceding '(?? ?# ?:)))
     (string-match erlang-atom-regexp ac-prefix))))
