;;; edts-face.el --- Display utilities.

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

(require 'face-remap)
;; Faces for highlighting

(defcustom edts-face-inhibit-fringe-markers nil
  "If non-nil, do not display markers in the fringe for errors etc."
  :type 'boolean
  :group 'edts)

(defcustom edts-face-marker-fringe 'left-fringe
  "Which side to display fringe-markers on. The value must be either
left-fringe or right-fringe."
  :type '(choice (const :tag "Left fringe" left-fringe)
		 (const :tag "Right fringe" right-fringe))
  :group 'edts)

(defcustom edts-face-inhibit-mode-line-updates nil
  "If non-nil, don't make any changes to the mode-line appearance."
  :group 'edts
  :type 'boolean)

;; define-fringe-bitmap is not defined when built without GUI, only call
;; if available
(when (fboundp 'define-fringe-bitmap)
  (define-fringe-bitmap 'small-blip (vector #b00000000
                                            #b00011100
                                            #b00111110
                                            #b00111110
                                            #b00111110
                                            #b00011100
                                            #b00000000)))

(defconst edts-face-warning-color-light "#ffc000")
(defconst edts-face-warning-color-dark "#ffc000")
(defconst edts-face-error-color-light "#ff0000")
(defconst edts-face-error-color-dark "#ff0000")
(defconst edts-face-passed-test-color-light "#95e454")
(defconst edts-face-passed-test-color-dark "#95e454")

(defface edts-face-error-fringe-bitmap
  `((((class color) (background dark))  (:foreground ,edts-face-error-color-dark))
    (((class color) (background light)) (:foreground ,edts-face-error-color-light))
    (t (:bold t)))
  "Face used for marking error lines."
  :group 'edts)

(defface edts-face-warning-fringe-bitmap
  `((((class color) (background dark))  (:foreground ,edts-face-warning-color-dark))
    (((class color) (background light)) (:foreground ,edts-face-warning-color-light))
    (t (:bold t)))
  "Face used for marking error lines."
  :group 'edts)

(defface edts-face-error-line
  `((((class color) (background dark))  (:underline (:color ,edts-face-error-color-dark)))
    (((class color) (background light)) (:underline (:color ,edts-face-error-color-light)))
    (t (:bold t)))
  "Face used for marking error lines."
  :group 'edts)

(defface edts-face-warning-line
  `((((class color) (background dark))  (:underline (:color ,edts-face-warning-color-dark)))
    (((class color) (background light)) (:underline (:color ,edts-face-warning-color-light)))
    (t (:bold t)))
  "Face used for marking warning lines."
  :group 'edts)

(defface edts-face-error-mode-line
  `((((class color))  (:background ,edts-face-error-color-dark
                       :foreground "white"))
    (t (:bold t)))
  "Face used for marking errors in the mode-line."
  :group 'edts)

(defface edts-face-warning-mode-line
  `((((class color))  (:background ,edts-face-warning-color-dark
                       :foreground "black"))
    (t (:bold t)))
  "Face used for marking warnings in the mode-line."
  :group 'edts)

(defvar edts-face-modeline-remap-cookie nil
  "A list of The 'cookies' returned from face-remap-add-relative, so
that we can reset our face remappings.")
(make-variable-buffer-local 'edts-face-modeline-remap-cookie)

(defface edts-face-passed-test-line
  `((((class color) (background dark))  (:underline (:color ,edts-face-passed-test-color-dark)))
    (((class color) (background light)) (:underline (:color ,edts-face-passed-test-color-light)))
    (t (:bold t)))
  "Face used for marking passed test lines."
  :group 'edts)

(defface edts-face-failed-test-line
  `((((class color) (background dark))  (:underline (:color ,edts-face-error-color-dark)))
    (((class color) (background light)) (:underline (:color ,edts-face-error-color-light)))
    (t (:bold t)))
  "Face used for marking failed test lines."
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

(defface edts-face-breakpoint-enabled-line
  '((((class color) (background dark)) (:background "purple"))
    (((class color) (background light)) (:background "purple")))
  "Face used for marking lines where a breakpoint is enabled."
  :group 'edts)

(defface edts-face-debug-current-line
  '((((class color) (background dark)) (:background "DarkGoldenRod4"))
    (((class color) (background light)) (:background "DarkGoldenRod2")))
  "Face used for marking the current line during debugging"
  :group 'edts)

(defun edts-face-print-overlay-on-line ()
  (when (and (boundp 'edts-mode) edts-mode)
    (let ((overlay (edts-face-max-prio-overlay (overlays-at (point)))))
      (when overlay
        (message "%s" (overlay-get overlay 'help-echo))))))

(defadvice next-line (after edts-face-next-li1ne activate)
  "Moves point to the next line and then prints the help-echo of the highest
priority any edts overlay at new point if any."
  (edts-face-print-overlay-on-line))

(defadvice previous-line (after edts-face-previous-line activate)
  "Moves point to the previous line and then prints the help-echo of
the highest priority any edts overlay at new point if any."
  (edts-face-print-overlay-on-line))

(defun edts-face-max-prio-overlay (overlays)
  "Returns the edts-face-overlay with the highest priority in OVERLAYS"
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

(defun edts-face-display-overlay (face
                                  line
                                  desc
                                  type
                                  prio
                                  &optional
                                  fill-line
                                  fringe)
  "Displays overlay for ISSUE in current buffer."
  (save-excursion
    (save-restriction
      (widen)
      (let* ((pos (ferl-position-at-beginning-of-line line))
             (beg (if fill-line pos (ferl-first-char-on-line-at pos)))
             (end (if fill-line (ferl-position-at-end-of-line line)
                    (ferl-last-char-on-line-at  pos)))
             (overlay (make-overlay beg end nil t (not fill-line)))
             (display-prop (cons edts-face-marker-fringe fringe)))
        (overlay-put overlay 'edts-face-overlay t)
        (overlay-put overlay 'face face)
        (overlay-put overlay 'help-echo desc)
        (overlay-put overlay 'edts-face-overlay-type type)
        (overlay-put overlay 'priority prio)
        (when (and (not edts-face-inhibit-fringe-markers) fringe)
          (overlay-put overlay
                       'before-string
                       (propertize " " 'display display-prop)))
        overlay))))

(defun edts-face-remove-overlays (&optional types)
  "Removes all overlays with of a type that is in TYPES."
  (interactive)
  (save-restriction
    (widen)
    (dolist (ol (overlays-in (point-min) (point-max)))
      (when (edts-face-overlay-p ol types)
        (delete-overlay ol)))))

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

(defun edts-face-update-buffer-mode-line (status)
  "Sets the mode-line face unless `edts-face-inhibit-mode-line-updates'
is non-nil."
  (edts-face-reset-mode-line)
  (unless edts-face-inhibit-mode-line-updates
    (setq edts-face-modeline-remap-cookie
          (edts-face--remap-modeline-face status))))

(defun edts-face--remap-modeline-face (status)
  "Set a relative mapping to mode-line face for STATUS."
  (case status
    (warning (face-remap-add-relative 'mode-line 'edts-face-warning-mode-line))
    (error   (face-remap-add-relative 'mode-line 'edts-face-error-mode-line))
    (ok      nil)))

(defun edts-face-reset-mode-line ()
  "Reset mode-line face remapping."
  (when edts-face-modeline-remap-cookie
    (face-remap-remove-relative edts-face-modeline-remap-cookie)
    (setq edts-face-modeline-remap-cookie nil)))

(provide 'edts-face)
