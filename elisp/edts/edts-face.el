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
;; Utilities for displaying things


;; Faces for highlighting
(defface edts-face-error-line
  '((((class color) (background dark)) (:background "Firebrick"))
    (((class color) (background light)) (:background "LightPink1"))
    (t (:bold t)))
  "Face used for marking error lines."
  :group 'edts)

(defface edts-face-warning-line
  '((((class color) (background dark)) (:background "dark blue"))
    (((class color) (background light)) (:background "light blue"))
    (t (:bold t)))
  "Face used for marking warning lines."
  :group 'edts)

(defface edts-face-lesser-line
  '((((class color) (background dark)) (:background "dark olive green"))
    (((class color) (background light)) (:background "pale green"))
    (t (:bold t)))
  "Face used for marking lesser warning lines."
  :group 'edts)

(defface edts-face-user-specified-line
  '((((class color) (background dark)) (:background "orange red"))
    (((class color) (background light)) (:background "yellow"))
    (t (:bold t)))
  "Face used for marking lesser warning lines."
  :group 'edts)

(defadvice next-line (after edts-next-line)
  "Moves point to the next line and then prints the help-echo of the highest
priority any edts overlay at new point if any."
  (edts-face-print-overlay-on-line))

(defadvice previous-line (after edts-previous-line)
  "Moves point to the previous line and then prints the help-echo of
the highest priority any edts overlay at new point if any."
  (edts-face-print-overlay-on-line))

(defun edts-face-print-overlay-on-line ()
  (let ((overlay (edts-face-max-prio-overlay (overlays-at (point)))))
    (when overlay
      (message "%s" (overlay-get overlay 'help-echo)))))

(defun edts-face-max-prio-overlay (overlays)
  "Returns the edts-face-overlay with the highest priority in OVERLAYS"
  (mapcar #'(lambda (o) (message "props %s" (overlay-properties o))) overlays)
  ; find first edts overlay.
  (while (and overlays (not (edts-face-overlay-p (car overlays))))
    (setq overlays (cdr overlays)))
  (let ((cur-max (car overlays)))
    (setq overlays (cdr overlays))
    (while overlays
      (when (edts-face-overlay-p (car overlays))
        (setq cur-max (edts-face-cmp-overlay cur-max (car overlays))))
      (setq overlays (cdr overlays)))
    cur-max))

(defun edts-face-cmp-overlay (overlay1 overlay2)
  "Returns the overlay with highest priority of OVERLAY1 and OVERLAY2."
  (if (< (overlay-get overlay2 'priority) (overlay-get overlay1 'priority))
      overlay1
      overlay2))

(defun edts-face-display-overlay (face line desc type prio)
  "Displays overlay for ISSUE in current buffer."
  (save-excursion
    (let* ((pos (ferl-position-at-line   line))
           (beg (ferl-first-char-on-line-at pos))
           (end (ferl-last-char-on-line-at  pos))
           (overlay (make-overlay beg end nil t t)))
      (overlay-put overlay 'edts-face-overlay t)
      (overlay-put overlay 'face face)
      (overlay-put overlay 'help-echo desc)
      (overlay-put overlay 'edts-face-overlay-type type)
      (overlay-put overlay 'priority prio)
      overlay)))

(defun edts-face-remove-overlays (type)
  "Removes all overlays with the name TYPE"
  (interactive)
  (dolist (ol (overlays-in (point-min) (point-max)))
    (when (edts-face-overlay-p ol type)
      (delete-overlay ol))))

(defun edts-face-next-overlay (pos types)
  "returns the position of the next edts overlay of any of TYPES from
POS."
  (let ((next-pos          (next-overlay-change pos))
        (next-edts-overlay nil))
    (while (and (< next-pos (point-max)) (not next-edts-overlay))
      (let ((overlays (overlays-at next-pos)))
        (while (and overlays (not next-edts-overlay))
          (if (edts-face-overlay-p (car overlays) types)
              (setq next-edts-overlay (car overlays))
              (setq overlays          (cdr overlays)))))
      (setq next-pos (next-overlay-change next-pos)))
    next-edts-overlay))

(defun edts-face-previous-overlay (pos types)
  "returns the position of the previous edts overlay of any of TYPES
from POS."
  (let ((prev-pos          (previous-overlay-change pos))
        (prev-edts-overlay nil))
    (while (and (> prev-pos (point-min)) (not prev-edts-overlay))
      (let ((overlays (overlays-at prev-pos)))
        (while (and overlays (not prev-edts-overlay))
          (if (edts-face-overlay-p (car overlays) types)
              (setq prev-edts-overlay (car overlays))
              (setq overlays          (cdr overlays)))))
      (setq prev-pos (previous-overlay-change prev-pos)))
     prev-edts-overlay))

(defun edts-face-overlay-p (overlay &optional types)
  "Returns non-nil of OVERLAY is an edts-face-overlay of any of TYPES"
  (and
   (overlayp overlay)
   (overlay-get overlay 'edts-face-overlay)
   (or (null types)
       (member (overlay-get overlay 'edts-face-overlay-type) types))))

(provide 'edts-face)
