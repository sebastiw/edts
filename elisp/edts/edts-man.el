;;; edts-man.el --- Find and read erlang man-pages.

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


(defcustom edts-man-root
  (concat (file-name-as-directory edts-erl-root) "man/")
  "Location of the Erlang documentation in man-format."
  :type  'directory
  :group 'edts)

(defconst edts-man-file-ext-regexp
  ".*\\.3\\(erl\\.gz\\)?$"
  "Regexp matching the extension of erlang man-page files.")

(defvar edts-man-module-cache nil
  "A cached list of available module man-pages.")

(defun edts-man-set-root (root)
  "Sets `edts-man-root' to ROOT and updates `edts-man-module-cache'."
  (setq edts-man-root root)
  (setq edts-man-module-cache (edts-man-modules))
  t)

(defun edts-man-modules ()
  "Return a list of all modules for which there are man-pages under
`edts-man-root'."
  (or edts-man-module-cache
      (let* ((dir     (edts-man-locate-dir edts-man-root 3))
             (modules (directory-files dir nil edts-man-file-ext-regexp)))
        (setq edts-man-module-cache
              (mapcar #'edts-man-file-base-name modules)))))

(defun edts-man-file-base-name (file-name)
  "Return file-name without its extension(s)."
  (while (string-match "\\." file-name)
    (setq file-name (file-name-sans-extension file-name)))
  file-name)

(defun edts-man-module-function-entries (module)
  "Return a list of all functions documented in the man-page of MODULE."
  (let ((funs nil)
        (re   (concat "^[[:space:]]*"(edts-any-function-regexp))))
    (with-temp-buffer
      (insert-file-contents (edts-man-locate-file edts-man-root module 3))
      (goto-char 0)
      (while (re-search-forward re nil t)
        (push (format "%s/%s" (match-string 1)
                      (ferl-paren-arity (match-string 2))) funs)))
    (reverse funs)))

(defun edts-man-extract-function-entry (module function)
  "Extract and display the man-page entry for MODULE:FUNCTION in
`edts-man-root'."
  (with-temp-buffer
    (insert-file-contents (edts-man-locate-file edts-man-root module 3))
    ;; woman-decode-region disregards the to-value and always keeps
    ;; going until end-of-buffer. It also always checks for the presence
    ;; of .TH and .SH tags at the beginning of the buffer (even if this
    ;; is outside the scope of the current region to decode). To
    ;; circumvent this, we start by deleting everything after the
    ;; section of interest, then decode that section ('til
    ;; end-of-buffer) and finally we delete everything before the
    ;; section of interest.
    (re-search-forward (format "^\\.B\n%s" function))
    (let ((end   (point-max)))
      (while (re-search-forward (format "^\\.B\n%s" function) nil t) nil)
      (when
          (re-search-forward "^\\.\\(\\(B\n\\)\\|\\(SH[[:space:]]\\)\\)" nil 't)
        (setq end (match-beginning 0)))
      (delete-region end (point-max))
      (woman-decode-region (point-min) (point-max))
      (goto-char (point-min))
      (re-search-forward (format "^\\s-*%s" function))
      (delete-region (point-min) (match-beginning 0))
      (set-left-margin (point-min) (point-max) 0)
      (delete-to-left-margin (point-min) (point-max))
      (buffer-string))))

(defun edts-man-find-function-entry (module function arity)
  "Find and display the man-page entry for MODULE:FUNCTION in
`edts-man-root'."
  (edts-man-find-module module)
  (re-search-forward (concat "^[[:space:]]*"
                             (edts-function-regexp function arity)))
  (beginning-of-line))

(defun edts-man-find-module (module)
  "Find and show the man-page documentation for MODULE under
`edts-man-root'."
  (condition-case ex
      (woman-find-file (edts-man-locate-file edts-man-root module 3))
      ('error (edts-log-error "No documentation found for %s" module))))

(defun edts-man-locate-file (root file page)
  "Locate man entry for FILE on PAGE under ROOT."
  (let ((dir (edts-man-locate-dir root page)))
    (locate-file file (list dir) '(".3" ".3.gz" ".3erl.gz"))))

(defun edts-man-locate-dir (root man-page)
  "Get the directory of MAN-PAGE under ROOT."
  (concat
   (file-name-as-directory edts-man-root) "man" (int-to-string man-page)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unit tests
