;;; edts-doc.el --- Find and read in-source erlang documentation.

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

(defun edts-doc-spec-regexp (function arity)
  (format
   (concat
    "^-spec[[:space:]\n]*\\(%s[[:space:]\n]*(%s)[[:space:]\n]*->"
    "[[:space:]\n]*[[:ascii:]]+?\\.\\)")
   function (edts-argument-regexp arity)))

(defun edts-doc-any-function-regexp ()
  "Return a regexp matching the whole function (heads and body) of any
complete function."
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
  (edts-search-function function arity)
  (let* ((end (point))
         (re  (edts-doc-any-function-regexp))
         (start (or (and (re-search-backward re nil t) (match-end 0)) 0)))
    (concat (edts-doc-extract-spec start end function arity)
            "\n\n"
            (edts-doc-extract-doc start end))))

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
  (let ((re "^%% @doc\\([[:ascii:]]+?\\)\\(\n[^%]\\|@end\\)"))
    (when (re-search-forward re end t)
      (replace-regexp-in-string
       "\\([[:space:]]*%*[[:space:]]+\\)\\|\\([[:space:]]\{2,\}\\)+"
       " "
       (buffer-substring (match-beginning 1) (match-end 1))))))

(provide 'edts-doc)
