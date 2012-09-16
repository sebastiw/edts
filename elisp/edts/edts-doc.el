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
;; Functionality for finding and displaying Erlang documentation.

(defcustom edts-doc-root
  (concat (file-name-as-directory edts-erl-root) "man/")
  "Location of the Erlang documentation in html-format."
  :type  'directory
  :group 'edts)

(defconst edts-doc-module-regexp
  ".*\\.3\\(erl\\.gz\\)?$"
  "Regexp for finding module man-page files.")

(defvar edts-doc-man-module-cache nil
  "A cached list of available module man-pages.")

(defun edts-doc-set-root (root)
  "Sets `edts-doc-root' to ROOT and updates edts-doc-man-module-cache."
  (setq edts-doc-root root)
  (setq edts-doc-man-module-cache (edts-doc-man-modules))
  t)

(defun edts-doc-man-modules ()
  "Return a list of all modules for which there are man-pages under
`edts-doc-root'."
  (or edts-doc-man-module-cache
      (let* ((dir     (edts-doc-man-page-dir edts-doc-root 3))
             (modules (directory-files dir nil edts-doc-module-regexp)))
        (setq edts-doc-man-module-cache
              (mapcar #'edts-doc-file-base-name modules)))))

(defun edts-doc-file-base-name (file-name)
  "Return file-name without its extension(s)."
  (while (string-match "\\." file-name)
    (setq file-name (file-name-sans-extension file-name)))
  file-name)

(defun edts-doc-extract-man-entry (module function arity)
  "Extract and display the man-page entry for MODULE:FUNCTION in `edts-doc-root'."
  (with-temp-buffer
    (insert-file-contents (edts-doc-locate-man-file edts-doc-root module 3))
    ;; woman-decode-region disregards the to-value and always keeps
    ;; going until end-of-buffer. It also always checks for the presence
    ;; of .TH and .SH tags at the beginning of the buffer (even if this
    ;; is outside the scope of the current region to decode). To
    ;; circumvent this, we start by deleting everything after the
    ;; section of interest, then decode that section ('til
    ;; end-of-buffer) and finally we delete everything before the
    ;; section of interest.
    (re-search-forward (edts-doc-function-man-regexp function arity))
    (delete-region (+ (match-end 0) 1) (point-max))
    (woman-decode-region (point-min) (point-max))
    (goto-char (point-min))
    (re-search-forward (edts-function-regexp function arity))
    (delete-region (point-min) (match-beginning 0))
    (set-left-margin (point-min) (point-max) 0)
    (delete-to-left-margin (point-min) (point-max))
    (buffer-string)))

(defun edts-doc-function-man-regexp (function arity)
  "Construct a regexp matching FUNCTION/ARITY section in an Erlang
man-page."
  (format "\\.B\n%s[[:ascii:]]*?\n\n\\.LP\n\\.nf"
          (edts-function-regexp function arity)))

(defun edts-doc-spec-regexp (function arity)
  (format
   (concat
    "^-spec[[:space:]\n]*\\(%s[[:space:]\n]*(%s)[[:space:]\n]*->"
    "[[:space:]\n]*[[:ascii:]]+?\\.\\)")
   function (edts-argument-regexp arity)))

(defun edts-doc-any-function-regexp ()
  (format
   "^%s[[:space:]\n]*([[:ascii:]]??*)[[:space:]\n]*->[[:ascii:]]+?*\\."
   erlang-atom-regexp))


(defun edts-doc-find-man-entry (module function arity)
  "Find and display the man-page entry for MODULE:FUNCTION in
`edts-doc-root'."
  (edts-doc-find-man-module edts-doc-root module)
  (re-search-forward (concat "^[[:space:]]*"
                             (edts-function-regexp function arity)))
  (beginning-of-line))

(defun edts-doc-find-man-module (root module)
  "Find and show the man-page documentation for MODULE under ROOT."
  (condition-case ex
      (woman-find-file (edts-doc-locate-man-file root module 3))
      ('error (edts-log-error "No documentation found for %s" module))))

(defun edts-doc-locate-man-file (root file page)
  "Locate man entry for FILE on PAGE under ROOT."
  (let ((dir (edts-doc-man-page-dir root page)))
    (locate-file file (list dir) '(".3" ".3.gz" ".3erl.gz"))))

(defun edts-doc-man-page-dir (root man-page)
  "Get the directory of MAN-PAGE under ROOT."
  (concat
   (file-name-as-directory edts-doc-root) "man" (int-to-string man-page)))

(defun edts-doc-expand-root ()
  "Expands `edts-doc-root' to a full set of man-page doc-directories.x"
  (mapcar
   #'(lambda (dir) (concat (file-name-as-directory dir) "doc/html"))
  (file-expand-wildcards (concat (file-name-as-directory edts-doc-root) "*"))))

(defun edts-doc-extract-function-information-from-source (source function arity)
  "Extract information (spec and comments) about FUNCTION/ARITY from
source in SOURCE."
  (with-temp-buffer
    (insert-file-contents source)
    (edts-doc-extract-function-information function arity)))

(defun edts-doc-extract-function-information (function arity)
  "Extract information (spec and comments) about FUNCTION/ARITY from
source in current buffer."
  (goto-char 0)
  (re-search-forward (concat "^" (edts-function-regexp function arity)))
  (let ((end (match-beginning 0)))
    (re-search-backward (edts-doc-any-function-regexp) nil t)
    (let ((start (match-end 0)))
      (concat (edts-doc-extract-spec start end function arity)
              "\n\n"
              (edts-doc-extract-doc start end)))))

(defun edts-doc-extract-spec (start end function arity)
  "Extract spec for FUNCTION/ARITY from source in current buffer. Search
is bounded by START and END."
  (goto-char start)
  (when (re-search-forward (edts-doc-spec-regexp function arity) end t)
    (replace-regexp-in-string
     "\\([[:space:]]*%*[[:space:]]+\\)\\|\\([[:space:]]\{2,\}p\\)+"
     " "
     (buffer-substring (match-beginning 1) (match-end 1)))))

(defun edts-doc-extract-doc (start end)
  "Extract documentation from source. Search is bounded by
START and END."
  (goto-char start)
  (when (re-search-forward "^%% @doc\\([[:ascii:]]+?\\)\n[^%]" end t)
    (replace-regexp-in-string
     "\\([[:space:]]*%*[[:space:]]+\\)\\|\\([[:space:]]\{2,\}\\)+"
     " "
     (buffer-substring (match-beginning 1) (match-end 1)))))

(provide 'edts-doc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unit tests

(when (member 'ert features)

  (ert-deftest edts-doc-expand-root-test ()
    (flet ((file-expand-wildcards (path)
                                  (if (string= path
                                               "/usr/lib/erlang/lib/*")
                                      '("/usr/lib/erlang/lib/foo"
                                        "/usr/lib/erlang/lib/bar")
                                    (error "Bad path"))))
      (should
       (equal '("/usr/lib/erlang/lib/foo/doc/html"
                "/usr/lib/erlang/lib/bar/doc/html")
              (edts-doc-expand-root "/usr/lib/erlang/lib")))))

  (ert-deftest edts-doc-html-to-string-test ()
    (should
     (equal "\n\n<>\n"
            (edts-doc-html-to-string "<p><br>&lt;&gt;<a href=foo></a></p>")))))
