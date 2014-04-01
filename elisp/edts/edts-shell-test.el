;;; edts-shell.el --- Erlang shell related functions.

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

(ert-deftest edts-shell-make-comint-buffer-test ()
  (let ((buffer (edts-shell-make-comint-buffer
                 "edts-test"
                 "edts-test"
                 "."
                 '("erl"))))
    (should (bufferp buffer))
    (should (string= "edts-test" (buffer-name buffer)))
    (should (string-match "erl\\(<[0-9]*>\\)?"
                          (process-name (get-buffer-process buffer))))
    (set-process-query-on-exit-flag (get-buffer-process buffer) nil)
    (kill-process (get-buffer-process buffer))
    (kill-buffer buffer)))
