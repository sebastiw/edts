;;; edts-log.el --- Logging.

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
;; Rudimentary project support for edts so that we can relate buffers to
;; projects and communicate with the correct nodes.


(defcustom edts-log-level 'info
  "The current EDTS log-level."
  :type '(choice
	  (const error)
	  (const warning)
	  (const info)
	  (const debug))
  :group 'edts)

(defconst edts-log-default-level 'error
  "The current EDTS log-level.")

(defconst edts-log-levels
  '((error   . 0)
    (warning . 1)
    (info    . 2)
    (debug   . 3))
  "The different edts log levels.")

(defun edts-log-set-level (level)
  "Set the EDTS log-level."
  (interactive
   (list
    (intern
     (ido-completing-read
      (format "EDTS log-level (default %s): " edts-log-default-level)
      (mapcar #'(lambda (lvl) (format "%s" (car lvl))) edts-log-levels)))))
  (setq edts-log-level level))

(defun edts-log-error (msg &rest args)
  "Log MSG at error-level."
  (apply #'edts-log-message 'error msg args))

(defun edts-log-warning (msg &rest args)
  "Log MSG at warning-level."
  (apply #'edts-log-message 'warning msg args))

(defun edts-log-info (msg &rest args)
  "Log MSG at info-level."
  (apply #'edts-log-message 'info msg args))

(defun edts-log-debug (msg &rest args)
  "Log MSG at debug-level."
  (apply #'edts-log-message 'debug msg args))

(defun edts-log-message (level msg &rest args)
  "Log MSG at LEVEL"
  (when (<= (edts-log--level-to-number level)
            (edts-log--level-to-number edts-log-level))
    (message (format "EDTS [%s]: %s" level (apply #'format msg args)))))

(defun edts-log--level-to-number (level)
  "Convert an edts-log log-level symbol to a number for comparison."
  (cdr (assoc level edts-log-levels)))

(provide 'edts-log)
