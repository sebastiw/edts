;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; EDTS Setup and configuration.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom edts-inhibit-package-check t
  "If non-nil, don't check whether EDTS was installed as a package."
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

(provide 'edts-start)
