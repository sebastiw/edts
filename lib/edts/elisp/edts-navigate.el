;; Copyright 2012 Thomas Järvstrand <tjarvstrand@gmail.com>
;;
;; This file is part of EDTS.
;;
;; EDTS is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; EDTS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with EDTS. If not, see <http://www.gnu.org/licenses/>.
;;
;; Rudimentary project support for edts so that we can relate buffers to
;; projects and communicate with the correct nodes.
;;
;; Code for jumping around between modules etc.

(defvar edts-find-history-ring (make-ring 20)
  "History ring tracing for following functions to their definitions.")

;; Borrowed from distel
(defun edts-find-source-under-point ()
  "Goto the source code that defines the function being called at point.
For remote calls, contacts an Erlang node to determine which file to
look in, with the following algorithm:

  Find the directory of the module's beam file (loading it if necessary).
  Look for the source file in:
    Directory where source file was originally compiled.
    Todo: Same directory as the beam file
    Todo: Again with /ebin/ replaced with /src/
    Todo: Again with /ebin/ replaced with /erl/

  Otherwise, report that the file can't be found."
  (interactive)
  (apply #'edts-find-source
         (or (ferl-mfa-at-point) (error "No call at point."))))

(defun edts-find-source (module function arity)
  "Find the source code for MODULE in a buffer, loading it if necessary.
When FUNCTION is specified, the point is moved to its start."
  ;; Add us to the history list
  (ring-insert-at-beginning edts-find-history-ring
			    (copy-marker (point-marker)))
  (if (equal module (erlang-get-module))
      (when function (ferl-search-function function arity))
      (let* ((node (edts-project-buffer-node-name))
             (info (edts-get-function-info node module function arity)))
        (find-file (cdr (assoc 'source info)))
        (goto-line (cdr (assoc 'line   info))))))

;; Borrowed from distel
(defun edts-find-source-unwind ()
  "Unwind back from uses of `edts-find-source-under-point'."
  (interactive)
  (unless (ring-empty-p edts-find-history-ring)
    (let* ((marker (ring-remove edts-find-history-ring))
	   (buffer (marker-buffer marker)))
      (if (buffer-live-p buffer)
	  (progn (switch-to-buffer buffer)
		 (goto-char (marker-position marker)))
	;; If this buffer was deleted, recurse to try the next one
	(edts-find-source-unwind))))) 

(provide 'edts-navigate)