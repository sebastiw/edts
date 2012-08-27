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
;; Utilities compiling and running tools on code


;; Faces for highlighting
(defface edts-code-error-line
  '((((class color) (background dark)) (:background "Firebrick"))
    (((class color) (background light)) (:background "LightPink1"))
    (t (:bold t)))
  "Face used for marking error lines."
  :group 'edts)

(defface edts-code-warning-line
  '((((class color) (background dark)) (:background "dark blue"))
    (((class color) (background light)) (:background "light blue"))
    (t (:bold t)))
  "Face used for marking warning lines."
  :group 'edts)

(defface edts-code-lesser-line
  '((((class color) (background dark)) (:background "dark olive green"))
    (((class color) (background light)) (:background "pale green"))
    (t (:bold t)))
  "Face used for marking lesser warning lines."
  :group 'edts)

(defface edts-code-user-specified-line
  '((((class color) (background dark)) (:background "orange red"))
    (((class color) (background light)) (:background "yellow"))
    (t (:bold t)))
  "Face used for marking lesser warning lines."
  :group 'edts)

(defun edts-code-compile-and-display ()
  "Compiles current buffer on node related the that buffer's project. Optional
parameter `compile-options' should be a list of compile options."
  (edts-code-remove-overlays "edts-code-compile")
  (let* ((module   (erlang-get-module))
         (file     (buffer-file-name))
         (comp-res (edts-get-compilation-result module file)))
    (when comp-res
      (let ((result   (cdr (assoc 'result comp-res)))
            (errors   (cdr (assoc 'errors comp-res)))
            (warnings (cdr (assoc 'warnings comp-res))))
        (edts-code-display-error-overlays errors)
        (edts-code-display-warning-overlays warnings)
        result))))

(defun edts-code-display-error-overlays (errors)
  "Displays overlays for `ERRORS' in current buffer."
  (mapcar
   #'(lambda (error)
       (edts-code-display-issue-overlay error 'edts-code-error-line))
   errors))

(defun edts-code-display-warning-overlays (warnings)
  "Displays overlays for `WARNINGS' in current buffer."
  (mapcar
   #'(lambda (warning)
       (edts-code-display-issue-overlay warning 'edts-code-warning-line))
   warnings))

(defun edts-code-display-issue-overlay (issue face)
  "Displays overlay for `ISSUE' in current buffer."
  (save-excursion
    (let* ((line    (edts-code-find-issue-overlay-line issue))
           (desc    (cdr (assoc 'description issue)))
           (start   (ferl-first-car-on-line line))
           (end     (ferl-last-car-on-line line))
           (overlay (make-overlay start end nil t t)))
      (overlay-put overlay 'edts-overlay t)
      (overlay-put overlay 'face face)
      (overlay-put overlay 'help-echo desc)
      (overlay-put overlay 'edts-overlay-type "edts-code-compile")
      (overlay-put overlay 'priority 100)
      overlay)))

(defun edts-code-find-issue-overlay-line (issue)
  "Tries to find where in current buffer to display overlay for `ISSUE'."
  (let ((cur-file (file-name-nondirectory (buffer-file-name)))
        (err-file (file-name-nondirectory (cdr (assoc 'file issue)))))
    (if (string-equal cur-file err-file)
        (cdr (assoc 'line issue))
        (save-excursion
          (goto-char (point-min))
          (let ((re (format "^-include\\(_lib\\)?(\".*%s\")." err-file)))
          ; this is probably not 100% correct in all cases
            (if (re-search-forward re nil t)
                (line-number-at-pos)
                0); This will to look strange, but at least we show the issue.
            )))))

(defun edts-code-remove-overlays (type)
  "Removes all overlays with the name `TYPE'"
  (interactive)
  (dolist (ol (overlays-in (point-min) (point-max)))
    (when (and (overlayp ol)
               (string-equal (overlay-get ol 'edts-overlay-type) type))
      (delete-overlay ol))))

(provide 'edts-code)
