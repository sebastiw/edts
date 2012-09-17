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
;; auto-complete source for erlang records.

(require 'auto-complete)
(require 'ferl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Source

(defvar edts-complete-record-source
  '((candidates . edts-complete-record-candidates)
    (init       . edts-complete-record-init)
    (document   . nil)
    (symbol     . "#")
    (requires   . nil)
    (limit      . nil)
    ))

(defvar edts-complete-record-candidates nil
  "Current completions for exported functions.")
(make-variable-buffer-local 'edts-complete-record-candidates)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Candidate functions

(defun edts-complete-record-init ()
  "Initialize record completions."
  (when (edts-complete-record-p)
    (case (edts-complete-point-inside-quotes)
      ('double-quoted nil) ; Don't complete inside strings
      (otherwise
       (edts-log-debug "Initializing record completions")
       (flet ((rec-name (rec) (cdr (assoc 'record rec))))
         (let* ((rec-structs (edts-get-detailed-module-info
                              (erlang-get-module)))
                (candidates
                 (mapcar #'rec-name (cdr (assoc 'records rec-structs)))))
           (setq edts-complete-record-candidates candidates)))))))

(defun edts-complete-record-candidates ()
  (case (edts-complete-point-inside-quotes)
    ('double-quoted  nil) ; Don't complete inside strings
    ('single-quoted (edts-complete-single-quoted-record-candidates))
    ('none          (edts-complete-normal-record-candidates))))

(defun edts-complete-normal-record-candidates ()
  "Produces the completion list for normal (unqoted) records. Unimplemented"
  (when (edts-complete-record-p)
    (edts-log-debug "completing records")
    (edts-log-debug "completing records done")
    edts-complete-record-candidates))

(defun edts-complete-single-quoted-record-candidates ()
  "Produces the completion for single-qoted erlang bifs, Same as normal
candidates, except we single-quote-terminate candidates."
  (mapcar
   #'edts-complete-single-quote-terminate
   edts-complete-normal-record-candidates))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conditions
;;

(defun edts-complete-record-p ()
  "Returns non-nil if the current `ac-prefix' can be completed with a built-in
function."
  (and
   (equal ?# (edts-complete-term-preceding-char))
   (string-match erlang-atom-regexp ac-prefix)))

(provide 'edts-complete-record-source)
