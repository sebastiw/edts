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

(defun edts-doc-find-man-entry (doc-root module function)
  (edts-doc-find-module doc-root module)
  (let* ((split  (split-string function "/"))
         (name   (car split))
         (arity  (string-to-int (cadr split)))
         (arg-re (edts-argument-regexp arity)))
    (re-search-forward
     (format "^[[:space:]]+%s ?(%s)[[:space:]]*->" name arg-re))
    (beginning-of-line))))

(defun edts-doc-find-module (doc-root module)
  "Find and show the html documentation for MODULE under DOC-ROOT."
  (let* ((dir  (edts-doc-man-page-dir doc-root 3))
         (file (locate-file module (list dir) '(".3" ".3.gz" ".3erl.gz"))))
    (if file
        (woman-find-file file)
        (edts-log-error "No documentation found for %s" module))))

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
