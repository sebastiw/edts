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

(defconst edts-doc-module-regexp
  ".*\\(\\(\\.3\\)\\|\\(\\.3erl\\.gz\\)\\)$"
  "Regexp for finding module man-page files.")

(defun edts-doc-modules (doc-root)
  "Return a list of all modules for which there is documentation."
  (let* ((dir     (edts-doc-man-page-dir doc-root 3))
         (modules (directory-files dir nil edts-doc-module-regexp)))
    (mapcar #'edts-doc-file-base-name modules)))

(defun edts-doc-file-base-name (file-name)
  "Return file-name without its extension(s)."
  (while (string-match "\\." file-name)
    (setq file-name (file-name-sans-extension file-name)))
  file-name)

(defun edts-doc-extract-man-entry (doc-root module function arity)
  "Extract and display the man-page entry for MODULE:FUNCTION in DOC-ROOT."
  (with-temp-buffer
    (insert-file-contents (edts-doc-locate-file doc-root module 3))
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

(defun edts-doc-find-man-entry (doc-root module function arity)
  "Find and display the man-page entry for MODULE:FUNCTION in DOC-ROOT."
  (edts-doc-find-module doc-root module)
  (re-search-forward (concat "^[[:space:]]*"
                             (edts-function-regexp function arity)))
  (beginning-of-line))

(defun edts-doc-find-module (doc-root module)
  "Find and show the html documentation for MODULE under DOC-ROOT."
  (condition-case ex
      (woman-find-file (edts-doc-locate-file doc-root module 3))
      ('error (edts-log-error "No documentation found for %s" module))))

(defun edts-doc-locate-file (doc-root file page)
  (let ((dir (edts-doc-man-page-dir doc-root page)))
  (locate-file file (list dir) '(".3" ".3.gz" ".3erl.gz"))))

(defun edts-doc-man-page-dir (doc-root man-page)
  "Get the directory of MAN-PAGE under ROOT-DIR."
  (concat (file-name-as-directory doc-root) "man" (int-to-string man-page)))

(defun edts-doc-expand-root (doc-root)
  "Expands DOC-ROOT to a full set of html doc-directories.x"
  (mapcar
   #'(lambda (dir) (concat (file-name-as-directory dir) "doc/html"))
  (file-expand-wildcards (concat (file-name-as-directory doc-root) "*"))))

(defun edts-doc-html-to-string (string)
  "Remove html formatting from STRING."
  (let ((replaces '(("\\(</?p>\\)\\|\\(<br>\\)" . "\n")
		    ("<.*??>" . "")
		    ("&gt;" . ">")
		    ("&lt;" . "<"))))
    (dolist (tagpair replaces string)
      (setq string
            (replace-regexp-in-string (car tagpair) (cdr tagpair) string)))))


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
              (edts-doc-expand-root "/usr/lib/erlang/lib"))))))

  (ert-deftest edts-doc-html-to-string-test ()
    (should
     (equal "\n\n<>\n"
            (edts-doc-html-to-string "<p><br>&lt;&gt;<a href=foo></a></p>")))))
