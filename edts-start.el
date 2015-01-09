;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; EDTS Setup and configuration.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'erlang)

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

(let* ((top-dir (file-name-directory load-file-name))
       (dirs    (directory-files (expand-file-name "elisp" top-dir) t "^[^.]")))
  (dolist (dir dirs)
    (when (file-directory-p dir)
      (add-to-list 'load-path dir))))

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
