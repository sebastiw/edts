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
;; Erlang shell related functions

(defvar edts-erl-next-shell-id 0
  "The id to give the next edts-erl shell started.")

(defun edts-erl-shell ()
  "Start an interactive erlang shell."
  (interactive)
  (let ((buffer-name (format "*edts-erl[%s]*" edts-erl-next-shell-id)))
    (incf edts-erl-next-shell-id)
    (switch-to-buffer
     (edts-erl-make-comint-buffer buffer-name "." (list edts-erl-command)))))

(defun edts-erl-make-comint-buffer (buffer-name pwd command)
  "In a comint-mode buffer Starts a node with BUFFER-NAME by cd'ing to
PWD and running COMMAND."
  (let* ((cmd  (car command))
         (args (cdr command)))
    (with-current-buffer (get-buffer-create buffer-name) (cd pwd))
    (apply #'make-comint-in-buffer cmd buffer-name cmd nil args)
    (with-current-buffer (get-buffer buffer-name)
      (make-local-variable 'comint-prompt-read-only)
      (setq comint-prompt-read-only t)
      (edts-complete-setup edts-complete-shell-sources)
      (erlang-syntax-table-init)
      (erlang-font-lock-init))
    (set-process-query-on-exit-flag (get-buffer-process buffer-name) nil)
    (get-buffer buffer-name)))

(when (member 'ert features)
  (ert-deftest edts-erl-make-comint-buffer-test ()
    (let ((buffer (edts-erl-make-comint-buffer "edts-test" "." '("erl"))))
      (should (bufferp buffer))
      (should (string= "edts-test" (buffer-name buffer)))
      (should (string-match "erl\\(<[0-9]*>\\)?"
                            (process-name (get-buffer-process buffer))))
      (set-process-query-on-exit-flag (get-buffer-process buffer) nil)
      (kill-process (get-buffer-process buffer))
      (kill-buffer buffer))))
