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

(defconst edts-code-issue-overlay-priorities
  '((warning . 1001);auto-highlight-symbol prio + 1
    (error   . 1002))
  "The overlay priorities for compilation errors and warnings")

(defun edts-code-overlay-priority (type)
  "Returns the overlay priority of TYPE. Type can be either a string or
a symbol."
  (let ((type (if (symbolp type) type (intern type))))
    (cdr (assoc type edts-code-issue-overlay-priorities))))

(defun edts-code-compile-and-display ()
  "Compiles current buffer on node related the that buffer's project."
  (edts-face-remove-overlays "edts-code-compile")
  (let* ((module   (erlang-get-module))
         (file     (buffer-file-name))
         (comp-res (edts-compile-and-load module file)))
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
       (edts-code-display-issue-overlay 'edts-face-error-line error))
   errors))

(defun edts-code-display-warning-overlays (warnings)
  "Displays overlays for WARNINGS in current buffer."
  (mapcar
   #'(lambda (warning)
       (edts-code-display-issue-overlay 'edts-face-warning-line warning))
   warnings))

(defun edts-code-display-issue-overlay (face issue)
  "Displays overlay with FACE for ISSUE in current buffer."
  (let* ((line         (edts-code-find-issue-overlay-line issue))
         (issue-type   (cdr (assoc 'type issue)))
         (desc         (cdr (assoc 'description issue)))
         (help         (format "line %s, %s: %s" line issue-type desc))
         (overlay-type "edts-code-compile")
         (prio         (edts-code-overlay-priority
                        (cdr (assoc 'type issue)))))
    (edts-face-display-overlay face line help overlay-type prio)))

(defun edts-code-find-issue-overlay-line (issue)
  "Tries to find where in current buffer to display overlay for `ISSUE'."
  (let ((cur-file (file-name-nondirectory (buffer-file-name)))
        (err-file (file-name-nondirectory (cdr (assoc 'file issue)))))
    (if (string-equal cur-file err-file)
        (cdr (assoc 'line issue))
        (save-excursion
          (goto-char (point-min))
          (let ((re (format "^-include\\(_lib\\)?(\".*%s\")." err-file)))
          ; This is probably not 100% correct in all cases
            (if (re-search-forward re nil t)
                (line-number-at-pos)
                0); Will to look strange, but at least we show the issue.
            )))))

(defun edts-code-next-issue ()
  "Moves point to the next error in current buffer and prints the error."
  (interactive)
  (let* ((overlay (edts-face-next-overlay (point) "edts-code-compile")))
    (if overlay
        (progn
          (goto-char (overlay-start overlay))
          (message (overlay-get overlay 'help-echo)))
        (error "EDTS: no more issues found"))))

(defun edts-code-previous-issue ()
  "Moves point to the next error in current buffer and prints the error."
  (interactive)
  (let* ((overlay (edts-face-previous-overlay (point) "edts-code-compile")))
    (if overlay
        (progn
          (goto-char (overlay-start overlay))
          (message (overlay-get overlay 'help-echo)))
        (error "EDTS: no more issues found"))))

(provide 'edts-code)
