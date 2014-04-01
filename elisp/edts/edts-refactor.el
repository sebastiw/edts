;;; edts-refactor.el --- Code refactoring utilities.

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
;; Rudimentary project support for edts so that we can relate buffers to
;; projects and communicate with the correct nodes.
;;

(require 'edts)

;; Originally from distel
(defun edts-refactor-extract-function (name start end)
  "Refactor the expression(s) in the region as a function.

The expressions are replaced with a call to the new function, and the
function itself is placed on the kill ring for manual placement. The
new function's argument list includes all variables that become free
during refactoring - that is, the local variables needed from the
original function.

New bindings created by the refactored expressions are *not* exported
back to the original function. Thus this is not a \"pure\"
refactoring.

This command requires Erlang syntax_tools package to be available in
the node, version 1.2 (or perhaps later.)"
  (interactive (list (read-string "Function name: ")
		     (region-beginning)
		     (region-end)))
  ;; Skip forward over whitespace
  (setq start (save-excursion
                (goto-char start)
                (skip-chars-forward " \t\r\n")
                (point)))
  ;; Skip backwards over trailing syntax
  (setq end (save-excursion
              (goto-char end)
              (skip-chars-backward ". ,;\r\n\t")
              (point)))
  (let* ((body       (buffer-substring-no-properties start end))
         (text       (edts-refactor-strip-macros body))
         (free-vars  (edts-get-free-vars text))
         (arglist    (concat "(" (mapconcat #'identity free-vars ", ") ")"))
         (indent-lvl erlang-indent-level))
    ;; rewrite the original as a call
    (delete-region start end)
    (goto-char start)
    (insert (format "%s%s" name arglist))
    (indent-according-to-mode)
    ;; Now generate the function and stick it on the kill ring
    (kill-new (with-temp-buffer
                (insert (format "%s%s ->\n%s.\n" name arglist body))
                (erlang-mode)
                (setq erlang-indent-level indent-lvl)
                (indent-region (point-min) (point-max) nil)
                (buffer-string)))
    (message "Saved `%s' definition on kill ring." name)))

(defun edts-refactor-strip-macros (text)
  "Removed all use of macros in TEXT.
We do this by making a bogus expansion of each macro, such that the
expanded code should probably still have the right set of free
variables."
  (with-temp-buffer
    (save-excursion (insert text))
    (while (re-search-forward "\\?[A-Za-z_]+" nil t)
      (replace-match "deadmacro" t))
    (buffer-string)))

(provide 'edts-refactor)
