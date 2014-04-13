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

(eval-when-compile
  (unless load-file-name
    (let ((default-directory edts-root-directory)
          (buf  "*EDTS compile*"))
      (pop-to-buffer (get-buffer-create buf))
      (goto-char (point-max))
      (if (= (call-process "make" nil t t "libs") 0)
          (quit-window)
        (error (format (concat "Failed to compile EDTS libraries. "
                               "See %s for details.")
                       buf))))))

(require 'edts-autoloads)

(provide 'edts-start)
