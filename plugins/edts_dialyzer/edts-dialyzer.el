;; Copyright 2013 Thomas Järvstrand <tjarvstrand@gmail.com>
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
;; xref interaction code for EDTS

(require 'f)

(require 'edts-code)
(require 'edts-plugin)
(require 'edts-project)

(defcustom edts-code-inhibit-dialyzer-on-compile t
  "If non-nil, don't run dialyzer analysis on every save."
  :group 'edts
  :type 'boolean)

(defun edts-dialyzer-init ()
  "Initialize edts-debug."
  (add-to-list 'edts-code-issue-types 'edts-dialyzer)
  (add-hook 'edts-code-after-compile-hook 'edts-dialyzer-after-compile-hook)
  (add-to-list 'edts-project-valid-properties :dialyzer-plt))

(defun edts-dialyzer-after-compile-hook (result)
  "Hook to run after compilation of a module."
  (unless (or edts-code-inhibit-dialyzer-on-compile (eq result 'error))
    (edts-dialyzer-analyze)))

(defun edts-dialyzer-analyze ()
  "Runs dialyzer for all live buffers related to current
buffer either by belonging to the same project or, if current buffer
does not belong to any project, being in the same directory as the
current buffer's file."
  (interactive)
  (edts-face-remove-overlays '(edts-dialyzer))
  (if eproject-mode
      (edts-dialyzer-analyze-project)
    (edts-dialyzer-analyze-directory)))

(define-obsolete-function-alias
  'edts-code-dialyze-related
  'edts-dialyzer-analyze
  "2014-01-23")

(defun edts-dialyzer-analyze-project ()
  "Runs dialyzer for all live buffers with its file in current
buffer's project, on the node related to that project."
  (let* ((bufs (edts-project-buffer-list (eproject-root) '(ferl-get-module)))
         (otp-plt  (eproject-attribute :dialyzer-plt))
         (out-plt  (f-join edts-data-directory
                                   (concat (eproject-name) ".plt")))
         (modules  (mapcar #'ferl-get-module bufs))
         (args (list (cons "otp_plt" otp-plt)
                     (cons "out_plt" out-plt)
                     (cons "modules" modules))))
    (edts-plugin-call-async (edts-api-node-name)
                            'edts_dialyzer
                            'analyze
                            args
                            #'edts-dialyzer-handle-analysis-result)))

(defun edts-dialyzer-analyze-directory ()
  "Runs dialyzer for all live buffers with its file in current
buffer's directory, on the node related to that buffer."
  (let* ((plt-file (concat (file-name-nondirectory default-directory) ".plt"))
         (args '(("otp_plt"  nil)
                 ("out-plt"  (f-join edts-data-directory plt-file))
                 ("modules"  (edts-code-directory-open-modules default-directory)))))
    (edts-plugin-call-async (edts-api-node-name)
                            'edts_dialyzer
                            'analyze
                            args
                            #'edts-dialyzer-handle-analysis-result)))

(defun edts-dialyzer-handle-analysis-result (analysis-res)
  (when analysis-res
    (let* ((all-warnings (cdr (assoc 'warnings analysis-res)))
           (warn-alist  (edts-code--issue-to-file-map all-warnings)))
      ;; Set the warning list in each project-buffer
      (with-each-buffer-in-project (gen-sym) (eproject-root)
        (let ((warnings (cdr (assoc (buffer-file-name) warn-alist))))
          (edts-code--set-issues 'edts-dialyzer (list 'warning warnings))
          (edts-face-update-buffer-mode-line (edts-code-buffer-status))
          (when warnings
            (edts-code-display-warning-overlays 'edts-dialyzer warnings)))))))

(provide 'edts-dialyzer)

