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

(require 'f)
(require 'woman)

(defconst edts-man-download-url "http://www.erlang.org/download"
  "Where to download the erlang documentation from.")

(defcustom edts-man-root nil
  "Do not set this variable directly, use `edts-man-setup' to install
documentation instead.

Location of the OTP documentation man-pages."
  :type  'directory
  :group 'edts)

(defconst edts-man-file-ext-regexp
  ".*\\.3\\(erl\\.gz\\)?$"
  "Regexp matching the extension of erlang man-page files.")

(defvar edts-man-module-cache nil
  "A cached list of available module man-pages.")

(defun edts-man-setup ()
  "Download and install OTP man-pages."
  (interactive)
  (when (yes-or-no-p "This will update your .emacs, continue?")
    (if (not edts-man-root)
        (edts-man-do-setup)
      (when (yes-or-no-p (concat "edts-man has already been setup. Do you "
                                 "want to fetch man-pages again?"))
        (edts-man-do-setup)))))

(defun edts-man-do-setup ()
  "Download and install OTP man-pages without queries."
  ;; This is to capture accidental enter key hits for people that have
  ;; exchanged yes-or-no-p for y-or-n-p. This would otherwise cause them
  ;; accidentally choose the first otp-version in the list.
  (read-event nil nil 0.2)
  (edts-log-info "Fetching available releases from %s..." edts-man-download-url)
  (let* ((vsn-urls   (edts-man--fetch-vsns))
         (vsns       (reverse (sort (mapcar #'car vsn-urls) #'string-lessp)))
         (vsn        (edts-man--choose-vsn vsns))
         (vsn-url    (concat edts-man-download-url
                             "/"
                             (cdr (assoc vsn vsn-urls))))
         (dir        (edts-man--choose-dir vsn)))
    (edts-log-info "Fetching %s..." vsn-url)
    (edts-man--fetch-and-extract vsn-url dir)
    (customize-save-variable 'edts-man-root dir)
    ;; Update the cache
    (edts-man-modules)
    (edts-log-info "OTP man-pages stored in %s. Hit C-c C-d H to access" dir)))

(defun edts-man--fetch-and-extract (url dir)
  "Fetch man-pages (tar.gz) from URL and extract the archive into DIR."
  (with-temp-buffer
    (let ((cmd (format "wget -O - %s | tar -xz --directory %s" url dir)))
      (shell-command cmd (current-buffer)))))

(defun edts-man--choose-dir (vsn)
  "Query the user for where to store documentation."
  (let* ((default (f-join edts-data-directory "doc"))
         (query "Where do you want to store the documentation? ")
         (dir   (f-join (read-file-name query default default) vsn)))
    (edts-man--ensure-dir dir)
    dir))

(defun edts-man--ensure-dir (dir)
  (if (file-exists-p dir)
      (unless (file-directory-p dir)
        (error "File exists and is not a directory"))
    (if (yes-or-no-p (format "Do you want to create directory %s?" dir))
        (make-directory dir t)
      (error "Need somewhere to store documentation"))))

(defun edts-man--fetch-vsns ()
  (with-current-buffer (url-retrieve-synchronously edts-man-download-url)
    (goto-char (point-min))
    (let ((case-fold-search t)
          (re "<a href=\".*/\\(otp_doc_man_\\(.*?\\)\\.tar\\.gz\\)\">")
          vsn-urls)
      (while (< (point) (point-max))
        (re-search-forward re nil 'move-point)
        (push (cons (match-string 2) (match-string 1)) vsn-urls))
      (kill-buffer)
      vsn-urls)))

(defun edts-man--choose-vsn (vsns)
  "Query the user to choose between available versions of man-pages."
  (edts-query "Please choose an OTP release:" vsns "No such version available"))

(defun edts-man-set-root (root)
  "Sets `edts-man-root' to ROOT and updates `edts-man-module-cache'."
  (warn "`edts-man-set-root' is deprecated, please call `edts-man-setup'
interactively to set up your man-pages instead")
  (unless (file-directory-p root)
    (error "No such directory: %s" root))
  (setq edts-man-root root)
  (edts-man-modules)
  t)

(defun edts-man-modules ()
  "Return a list of all modules for which there are man-pages under
`edts-man-root'."
  (or edts-man-module-cache
      (setq edts-man-module-cache (edts-man--update-module-cache))))

(defun edts-man--update-module-cache ()
  (let* ((dir     (edts-man-locate-dir edts-man-root 3))
         (modules (directory-files dir nil edts-man-file-ext-regexp)))
    (mapcar #'edts-man--file-base-name modules)))

(defun edts-man--file-base-name (file-name)
  "Return file-name without its extension(s)."
  (while (string-match "\\." file-name)
    (setq file-name (file-name-sans-extension file-name)))
  file-name)

(defun edts-man-module-function-entries (module)
  "Return a list of all functions documented in the man-page of MODULE."
  (let (funs
        (re (format "^\\.B\n\\(%s:\\)?%s" module (edts-any-function-regexp))))
    (with-temp-buffer
      (insert-file-contents (edts-man-locate-file edts-man-root module 3))
      (goto-char 0)
      (while (re-search-forward re nil t)
        (push (match-string 2) funs))
      ;; Hack so we don't choke on type specs in the documentation function
      ;; heads.
      (setq funs (mapcar #'(lambda (s) (replace-regexp-in-string "::" "=" s))
                         funs))
      (sort
       ;; each mfa is '(mod fun arity), but we don't want the module part
       (mapcar #'(lambda (mfa) (format "%s/%s"  (cadr mfa) (caddr mfa)))
               (edts-strings-to-mfas funs))
       #'string-lessp))))

(defun edts-man-extract-function-entry (module function)
  "Extract and display the man-page entry for MODULE:FUNCTION in
`edts-man-root'."
  (with-temp-buffer
    (insert-file-contents (edts-man-locate-file edts-man-root module 3))
    ;; woman-decode-region disregards the to-value and always keeps
    ;; going until end-of-buffer. It also always checks for the presence
    ;; of .TH and .SH tags at the beginning of the buffer (even if this
    ;; is outside the scope of the current region to decode). To
    ;; circumvent this, we delete everything before and after the
    ;; section of interest, and then add those tags at the top. This
    ;; produces two empty lines at the top that we delete before returning.
    (let (start
          end
          (re  (format "^\\.B\n\\(%s:\\)?%s" module function)))
      (re-search-forward re)
      (setq start (match-beginning 0))
      (while (re-search-forward re nil t) nil) ; Get the docs for all arities.
      (when
          (re-search-forward "^\\.\\(\\(B\n\\)\\|\\(SH[[:space:]]\\)\\)" nil 't)
        (delete-region (match-beginning 0) (point-max)))
      (delete-region (point-min) start)
      (goto-char (point-min))
      (insert ".TH\n.SH\n")
      (woman-decode-region (point-min) (point-max))
      (goto-char (point-min))
      (kill-line 2)
      (set-left-margin (point-min) (point-max) 0)
      (delete-to-left-margin (point-min) (point-max))
      (buffer-string))))

(defun edts-man-find-function-entry (module function arity)
  "Find and display the man-page entry for MODULE:FUNCTION in
`edts-man-root'."
  (edts-man-find-module module)
  (let (foundp
        (re (format "^\\s-*\\(\\(%s:\\)?%s\\)(.*)\\s-->" module function)))
    (while (and (not foundp) (re-search-forward re nil t))
      (save-excursion
        (goto-char (match-beginning 1))
        (let ((matchp (equal (list function arity)
                             (cdr (edts-mfa-at (point)))))
              ;; Check that the face is bold just in case the function occurs
              ;; in some example earlier in the file.
              (boldp (eq (face-at-point) 'woman-bold)))
          (setq foundp (and matchp boldp)))))
    (goto-char (match-beginning 1))))

(defun edts-man-find-module (module)
  "Find and show the man-page documentation for MODULE under
`edts-man-root'."
  (condition-case ex
      (woman-find-file (edts-man-locate-file edts-man-root module 3))
      ('error (edts-log-error "No documentation found for %s" module))))

(defun edts-man-locate-file (root file page)
  "Locate man entry for FILE on PAGE under ROOT."
  (let* ((dir (edts-man-locate-dir root page))
         (file-name (locate-file file (list dir) '(".3" ".3.gz" ".3erl.gz"))))
    (unless file-name
      (error (format "Could not locate man-file for %s" file)))
    file-name))

(defun edts-man-locate-dir (root man-page)
  "Get the directory of MAN-PAGE under ROOT."
  (unless edts-man-root
    (error "edts-man not configured, please run `edts-man-setup'"))
  (f-join edts-man-root "man" (concat "man" (int-to-string man-page))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unit tests

(provide 'edts-man)
