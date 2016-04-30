;;; edts-code.el --- Utilities for compiling and running tools on code.

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

;; All code for compilation and in-buffer highlighting is a rewrite of work
;; done by Sebastian Weddmark Olsson.

(require 'dash)
(require 'eproject-extras)
(require 'f)

(require 'edts-api)
(require 'edts-face)
(require 'ferl)

(defvar edts-code-issue-types '(edts-code-compile
                                edts-code-eunit-failed)
  "List of overlay categories that are considered edts-code-issues")

(defvar edts-code-before-compile-hook
  nil
  "Hooks to run before compilation. Hooks are called with the name of
the module to be compiled as the only argument.")

(defvar edts-code-after-compile-hook
  '(edts-code-eunit)
  "Hooks to run after compilation finishes. Hooks are called with the
compilation result as a symbol as the only argument")
(defvaralias ;; Compatibility
  'edts-code-after-compilation-hook
  'edts-code-after-compile-hook
  "This variable is deprecated, use `edts-code-after-compile-hook'")

(defvar edts-code-buffer-issues nil
  "A plist describing the current issues (errors and warnings) in the
current buffer. It is a plist with one entry for each type (compilation,
xref, eunit, etc). Each entry in turn is an plist with an entry for each
issue severity (error, warning, etc).")
(make-variable-buffer-local 'edts-code-buffer-issues)

(defcustom edts-code-issue-wrap-around nil
  "Should next/previous issue wrap around after no issues have been found"
  :type 'boolean
  :options '(t nil)
  :group 'edts)

(defconst edts-code-issue-overlay-priorities
  '((passed-test . 900)
    (failed-test . 901)
    (warning     . 902)
    (error       . 903))
  "The overlay priorities for compilation errors and warnings")

(defconst edts-code-issue-overlay-max-priority
  (apply #'max (mapcar #'cdr edts-code-issue-overlay-priorities))
  "The highest overlay priority for edts-code issues.")

(defconst edts-code-issue-fringe-bitmap
  (when (boundp 'fringe-bitmaps)
    (if (member 'small-blip fringe-bitmaps)
        'small-blip
      'filled-square))
  "The bitmap to display in the fringe to indicade an issue on that
line.")

(defun edts-code-overlay-priority (type)
  "Returns the overlay priority of TYPE. Type can be either a string or
a symbol."
  (let ((type (if (symbolp type) type (intern type))))
    (cdr (assoc type edts-code-issue-overlay-priorities))))

(defun edts-code--set-issues (type issues)
  "Set the buffer's issues of TYPE to ISSUES. Issues should be an plist
with severity as key and a lists of issues as values"
  (setq edts-code-buffer-issues
        (plist-put edts-code-buffer-issues type issues)))

(defun edts-code-buffer-status ()
  "Return 'error if there are any edts errors in current buffer,
'warning if there are warnings and 'ok otherwise."
  (block nil
    (let ((status 'ok)
          (issues edts-code-buffer-issues))
      (while issues
        (when (plist-get (cadr issues) 'error)
          (return 'error))
        (when (plist-get (cadr issues) 'warning)
          (setq status 'warning))
        (setq issues (cddr issues)))
      status)))

(defun edts-code-compile-and-display ()
  "Compiles current buffer on node related the that buffer's project."
  (interactive)
  (edts-face-remove-overlays '(edts-code-compile))
  (let ((module   (ferl-get-module))
        (file     (buffer-file-name)))
    (when module
      (run-hook-with-args 'edts-code-before-compile-hook (intern module))
      (edts-api-compile-and-load-async
       module file #'edts-code-handle-compilation-result))))

(defun edts-code-handle-compilation-result (comp-res)
  (when comp-res
    (let ((result   (cdr (assoc 'result comp-res)))
          (errors   (cdr (assoc 'errors comp-res)))
          (warnings (cdr (assoc 'warnings comp-res))))
      (edts-code--set-issues 'edts-code-compile (list 'error   errors
                                                      'warning warnings))
      (edts-code-display-error-overlays 'edts-code-compile errors)
      (edts-code-display-warning-overlays 'edts-code-compile warnings)
      (edts-face-update-buffer-mode-line (edts-code-buffer-status))
      (run-hook-with-args 'edts-code-after-compile-hook (intern result))
      result)))

(defun edts-code--issue-to-file-map (issues)
  "Creates an alist with mapping between filenames and related elements
of ISSUES."
  (let* ((issue-alist nil))
    (mapc
     #'(lambda (e)
         (let* ((file   (file-truename (cdr (assoc 'file e))))
                (el     (assoc file issue-alist))
                (new-el (cons file (cons e (cdr el)))))
           (setq issue-alist (cons new-el (delete el issue-alist)))))
     issues)
    issue-alist))

(defun edts-code-eunit (result)
  "Runs eunit tests for current buffer on node related to that
buffer's project."
  (interactive '(ok))
  (let ((module (ferl-get-module)))
    (when module
      (edts-face-remove-overlays '(edts-code-eunit-passed))
      (edts-face-remove-overlays '(edts-code-eunit-failed))
      (when (not (eq result 'error))
	(edts-api-get-module-eunit-async
	 module #'edts-code-handle-eunit-result)))))

(defun edts-code-handle-eunit-result (eunit-res)
  (when eunit-res
    (let ((failed (cdr (assoc 'failed eunit-res)))
          (passed (cdr (assoc 'passed eunit-res))))
      (edts-code--set-issues 'edts-code-eunit (list 'error failed))
      (edts-code-display-passed-test-overlays
       'edts-code-eunit-passed passed)
      (edts-code-display-failed-test-overlays
       'edts-code-eunit-failed failed)
      (edts-face-update-buffer-mode-line (edts-code-buffer-status)))))

(defun edts-code-directory-open-modules (dir)
  "Return a list of all modules in DIR being visited, non-recursive."
  (mapcar 'ferl-get-module (edts-code-directory-module-buffers dir)))

(defun edts-code-directory-module-buffers (dir)
  "Return a list of all edts buffers visiting an erlang module in DIR,
non-recursive."
  (let ((dir (directory-file-name dir)))
    (--reduce-from
     (with-current-buffer it
       (if (and (buffer-live-p it)
                (buffer-file-name)
                (string= dir (f-dirname (buffer-file-name)))
                (ferl-get-module it))
           (cons it acc)
         acc))
     nil
     (buffer-list))))

(defun edts-code-display-error-overlays (type errors)
  "Displays overlays for ERRORS in current buffer."
  (mapcar
   #'(lambda (error)
       (edts-code-display-issue-overlay type
                                        'edts-face-error-line
                                        'edts-face-error-fringe-bitmap
                                        error))
   errors))

(defun edts-code-display-warning-overlays (type warnings)
  "Displays overlays for WARNINGS in current buffer."
  (mapcar
   #'(lambda (warning)
       (edts-code-display-issue-overlay type
                                        'edts-face-warning-line
                                        'edts-face-warning-fringe-bitmap
                                        warning))
   warnings))

(defun edts-code-display-failed-test-overlays (type failed-tests)
  "Displays overlays for FAILED TESTS in current buffer."
  (mapcar
   #'(lambda (failed-test)
       (edts-code-display-issue-overlay type
                                        'edts-face-failed-test-line
                                        'edts-face-error-fringe-bitmap
                                        failed-test))
   failed-tests))

(defun edts-code-display-passed-test-overlays (type passed-tests)
  "Displays overlays for PASSED TESTS in current buffer."
  (mapcar
   #'(lambda (passed-test)
       (edts-code-display-issue-overlay type
                                        'edts-face-passed-test-line
                                        nil
                                        passed-test))
   passed-tests))


(defun edts-code-display-issue-overlay (type face fringe-face issue)
  "Displays overlay with FACE for ISSUE in current buffer."
  (let* ((line         (edts-code-find-issue-overlay-line issue))
         (issue-type   (cdr (assoc 'type issue)))
         (desc         (cdr (assoc 'description issue)))
         (help         (format "line %s, %s: %s" line issue-type desc))
         (overlay-type type)
         (prio         (edts-code-overlay-priority
                        (cdr (assoc 'type issue))))
         (fringe       (list edts-code-issue-fringe-bitmap fringe-face)))
    (when (integerp line)
      (edts-face-display-overlay face
                                 line
                                 help
                                 overlay-type
                                 prio
                                 nil
                                 fringe))))

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

(defun edts-code-next-issue (&optional wrapped)
  "Moves point to the next error in current buffer and prints the error."
  (interactive)
  (push-mark)
  (let* ((overlay (edts-face-next-overlay (point) edts-code-issue-types)))
    (if overlay
        (progn
          (goto-char (overlay-start overlay))
          (message (overlay-get overlay 'help-echo)))
      (if (and edts-code-issue-wrap-around (not wrapped))
          (progn
            (goto-char (point-min))
            (edts-code-next-issue t)
        (error "EDTS: no more issues found"))))))

(defun edts-code-previous-issue (&optional wrapped)
  "Moves point to the next error in current buffer and prints the error."
  (interactive)
  (push-mark)
  (let* ((overlay (edts-face-previous-overlay (point) edts-code-issue-types)))
    (if overlay
        (progn
          (goto-char (overlay-start overlay))
          (message (overlay-get overlay 'help-echo)))
      (if (and edts-code-issue-wrap-around (not wrapped))
          (progn
            (goto-char (point-max))
            (edts-code-previous-issue t))
        (error "EDTS: no more issues found")))))

(provide 'edts-code)
