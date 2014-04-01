;;; edts-complete-variable-source.el --- Completion source for variables.

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

(defvar edts-complete-variable-source
  '((candidates . edts-complete-variable-candidates)
    (init       . edts-complete-variable-init)
    (document   . nil)
    (symbol     . "v")
    (requires   . 0)
    (limit      . nil)
    ))

(defvar edts-complete-variable-candidates nil
  "Current completions for variables.")
(make-variable-buffer-local 'edts-complete-variable-candidates)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Candidate functions

(defun edts-complete-variable-init ()
  "Initializes the list of variable completions"
  (when (edts-complete-variable-p)
    (edts-log-debug "Initializing variable completions")
    (setq edts-complete-variable-candidates
          (edts-complete-find-variable-candidates))))

(defun edts-complete-variable-candidates ()
  (case (ferl-point-inside-quotes)
    ('double-quoted nil) ; Don't complete inside strings
    ('single-quoted nil) ; No single-quoted variables
    ('none          (edts-complete-normal-variable-candidates))))

(defun edts-complete-normal-variable-candidates ()
  "Generates the auto-complete candidate list for variables. Matches variables
mentioned in current function, before current point."
  (when (edts-complete-variable-p)
    (edts-log-debug "completing variables")
    (edts-log-debug "completing variables done")
    edts-complete-variable-candidates))

(defun edts-complete-find-variable-candidates ()
  (save-excursion
    (let ((case-fold-search nil)
          (old-point  (point))
          (candidates ()))
      (ferl-beginning-of-function)
      (while (and (re-search-forward erlang-variable-regexp old-point t)
                  (< (match-end 0) old-point))
        (add-to-list 'candidates (thing-at-point 'symbol)))
      candidates)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conditions
;;

(defun edts-complete-variable-p ()
  "Returns non-nil if the current `ac-prefix' can be completed with an
variable."
  (condition-case ex
      (let ((case-fold-search nil)
            (preceding        (ferl-term-preceding-char)))
        (and
         (not (equal ?? preceding))
         (not (equal ?# preceding))
         (string-match erlang-variable-regexp ac-prefix)))
    ('error nil)))

(provide 'edts-complete-variable-source)
