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
;; Misc edts-related functionality.

(defvar erl-find-history-ring (make-ring 20)
  "History ring tracing for following functions to their definitions.")

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
