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
;; Functionality for finding and displaying documentation and specifications
;; in Erlang source files.

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
