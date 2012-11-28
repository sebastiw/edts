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
;; Rudimentary project support for edts so that we can relate buffers to
;; projects and communicate with the correct nodes.
;;
;; Logging stuff.


(defcustom edts-log-level 1
  "The current EDTS log-level."
  :group 'edts)

(defconst edts-log-default-level 1
  "The current EDTS log-level.")

(defconst edts-log-level-error 0
  "EDTS error log-level.")

(defconst edts-log-level-warning 1
  "EDTS warning log-level.")

(defconst edts-log-level-info 2
  "EDTS info log-level.")

(defconst edts-log-level-debug 3
  "EDTS debug log-level.")

(defun edts-log-set-level (level)
  "Set the EDTS log-level."
  (interactive
   (list
    (read-input
     (format "EDTS log-level (default %s): " edts-log-default-level))))
  (setq edts-log-level (string-to-int level)))

(defun edts-log-error (msg &rest args)
  "Log MSG at error-level."
  (apply #'edts-log-message edts-log-level-error msg args))

(defun edts-log-warning (msg &rest args)
  "Log MSG at warning-level."
  (apply #'edts-log-message edts-log-level-warning msg args))

(defun edts-log-info (msg &rest args)
  "Log MSG at info-level."
  (apply #'edts-log-message edts-log-level-info msg args))

(defun edts-log-debug (msg &rest args)
  "Log MSG at debug-level."
  (apply #'edts-log-message edts-log-level-debug msg args))

(defun edts-log-message (level msg &rest args)
  "Log MSG at LEVEL"
  (when (<= level edts-log-level)
    (message (concat "EDTS: " (apply #'format msg args)))))

