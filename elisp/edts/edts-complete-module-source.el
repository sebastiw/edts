;;; edts-complete-module-source.el --- Completion source for macros

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

(defvar edts-complete-module-source
  '((candidates . edts-complete-module-candidates)
    (document   . nil)
    (symbol     . "m")
    (requires   . 0)
    (limit      . nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Candidate functions

(defvar edts-complete-module-cache nil
    "The current list of module completions.")
(make-variable-buffer-local 'edts-complete-module-cache)
(add-hook 'after-save-hook #'(lambda () (setq edts-complete-module-cache nil)))

(defun edts-complete-module-candidates ()
  (case (ferl-point-inside-quotes)
    ('double-quoted  nil) ; Don't complete inside strings
    ('single-quoted (edts-complete-single-quoted-module-candidates))
    ('none          (edts-complete-normal-module-candidates))))

(defun edts-complete-normal-module-candidates ()
  "Produces the completion list for normal (unqoted) modules."
  (when (edts-complete-module-p)
    (edts-log-debug "completing modules")
    (let ((completions
           (or edts-complete-module-cache
               (setq edts-complete-module-cache (edts-api-get-modules)))))
      (edts-log-debug "completing modules done")
      completions)))

(defun edts-complete-single-quoted-module-candidates ()
  "Produces the completion for single-qoted erlang modules, Same as normal
candidates, except we single-quote-terminate candidates."
  (mapcar
   #'ferl-single-quote-terminate
   (edts-complete-normal-module-candidates)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conditions
;;

(defun edts-complete-module-p ()
  "Returns non-nil if the current `ac-prefix' can be completed with a module."
  (condition-case ex
      (let ((preceding (ferl-term-preceding-char)))
        (and
         (not (equal ?? preceding))
         (not (equal ?# preceding))
         (not (equal ?: preceding))
         (string-match erlang-atom-regexp ac-prefix)))
  ('error nil)))

(provide 'edts-complete-module-source)
