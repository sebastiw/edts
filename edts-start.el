;;; edts-start.el --- EDTS auto-activation and starting.

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

(require 'erlang)
(require 'f)

(defcustom edts-inhibit-package-check nil
  "If non-nil, don't check whether EDTS was installed as a package."
  :type 'boolean
  :group 'edts)

(unless (or edts-inhibit-package-check
            (and (boundp 'package-alist)
                 (assoc 'edts package-alist)))
  (warn (concat
"EDTS was not installed as a package.\n"
"\n"
"Please see the README for details on installing EDTS from MELPA.\n"
"\n"
"If you know what you're doing and have all the necessary dependencies\n"
"installed (see edts-pkg.el) you can disable this check by setting\n"
"`edts-inhibit-package-check' to a non-nil value."))
  (when (y-or-n-p "Do you want to disable package check now?")
    (customize-save-variable 'edts-inhibit-package-check t)))

(let* ((top-dir (f-dirname (f-this-file)))
       (dirs    (f-directories (f-expand "elisp" top-dir))))
  (-each dirs (lambda (d) (add-to-list 'load-path d t))))

(require 'edts-mode)

(defun edts-erlang-mode-hook ()
  (when (buffer-file-name)
    (edts-mode t)))

(add-hook 'erlang-mode-hook 'edts-erlang-mode-hook)

(defcustom edts-erlang-mode-regexps
  '("^\\.erlang$"
    "\\.app$"
    "\\.app.src$"
    "\\.erl$"
    "\\.es$"
    "\\.escript$"
    "\\.eterm$"
    "\\.script$"
    "\\.yaws$")
  "File-name patterns for which to auto-activate edts-mode."
  :type '(repeat regexp)
  :group 'edts)

(mapc #'(lambda(re) (add-to-list 'auto-mode-alist (cons re 'erlang-mode)))
      edts-erlang-mode-regexps)

(provide 'edts-start)
