;;; edts-complete-record-source.el --- Completion source for records.

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
;; auto-complete source for erlang records.

(require 'auto-complete)
(require 'ferl)

(require 'edts-log)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Source

(defvar edts-complete-record-source
  '((candidates . edts-complete-record-candidates)
    (init       . edts-complete-record-init)
    (prefix     . edts-complete-record-prefix)
    (document   . nil)
    (symbol     . "#")
    (requires   . 0)
    (limit      . nil)
    ))

(defvar edts-complete-record-candidates nil
  "Current completions for exported functions.")
(make-variable-buffer-local 'edts-complete-record-candidates)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Candidate functions

(defun edts-complete-record-init ()
  "Initialize record completions."
  (when (edts-complete-record-p ac-point)
    (case (ferl-point-inside-quotes)
      ('double-quoted nil) ; Don't complete inside strings
      (otherwise
       (edts-log-debug "Initializing record completions")
       (flet ((rec-name (rec) (cdr (assoc 'record rec))))
         (let* ((rec-structs (edts-api-get-detailed-module-info
                              (ferl-get-module)))
                (candidates
                 (mapcar #'rec-name (cdr (assoc 'records rec-structs)))))
           (setq edts-complete-record-candidates candidates)))))))

(defun edts-complete-record-candidates ()
  (case (ferl-point-inside-quotes)
    ('double-quoted  nil) ; Don't complete inside strings
    ('single-quoted (edts-complete-single-quoted-record-candidates))
    ('none          (edts-complete-normal-record-candidates))))

(defun edts-complete-normal-record-candidates ()
  "Produces the completion list for normal (unqoted) records. Unimplemented"
  (when (edts-complete-record-p ac-point)
    (edts-log-debug "completing records")
    (edts-log-debug "completing records done")
    edts-complete-record-candidates))

(defun edts-complete-single-quoted-record-candidates ()
  "Produces the completion for single-qoted erlang bifs, Same as normal
candidates, except we single-quote-terminate candidates."
  (mapcar
   #'ferl-single-quote-terminate
   (edts-complete-normal-record-candidates)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conditions
;;

(defun edts-complete-record-prefix ()
  "Returns non-nil if the current `ac-prefix' or a prefix starting at
POINT or current could be completed with an record."
  (cond ((and ac-point (edts-complete-record-p ac-point)) ac-point)
        ((edts-complete-record-p (point)) (point))))


(defun edts-complete-record-p (point)
  "Returns non-nil if the current `ac-prefix' can be completed with a built-in
function."
  (condition-case ex
      (and
       (equal ?# (ferl-term-preceding-char point))
       (or (not ac-prefix) (string= "" ac-prefix)
           (string-match erlang-atom-regexp ac-prefix)))
    ('error nil)))

(provide 'edts-complete-record-source)
