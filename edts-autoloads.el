;;; edts-autoloads.el ---
;;
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paths

(defconst edts-root-directory
  (file-name-directory (or (locate-library "edts-autoloads")
                           load-file-name
                           default-directory))
  "EDTS root directory.")

(dolist (pkg '(dash s f))
    (unless (require pkg nil t)
      (add-to-list 'load-path
                   (format "%s/elisp/%s"
                           (directory-file-name edts-root-directory)
                           pkg))
      (require pkg)))

(require 'f)

(unless (require 'erlang nil 'noerror)
  (add-to-list 'load-path
               (car
                (file-expand-wildcards
                 (f-join
                  (f-dirname (f-dirname (f-canonical (executable-find "erl"))))
                  "lib"
                  "tools*"
                  "emacs"))))
  (require 'erlang))

(defconst edts-code-directory
  (f-join edts-root-directory "elisp" "edts")
  "Directory where edts code is located.")

(defcustom edts-data-directory
  (if (boundp 'user-emacs-directory)
      (expand-file-name (concat user-emacs-directory "/edts"))
    (expand-file-name "~/.emacs.d"))
  "Where EDTS should save its data."
  :group 'edts)

(defconst edts-lib-directory
  (f-join edts-root-directory "elisp")
  "Directory where edts libraries are located.")

(defconst edts-plugin-directory
  (f-join edts-root-directory "plugins")
  "Directory where edts plugins are located.")

(defconst edts-test-directory
  (f-join edts-root-directory "test")
  "Directory where edts test data are located.")

;; Add all libs to load-path
(loop for  (name dirp . rest)
      in   (directory-files-and-attributes edts-lib-directory nil "^[^.]")
      when dirp
      do   (add-to-list 'load-path (f-join edts-lib-directory name)))

(add-to-list 'load-path edts-code-directory)
(require 'edts-plugin)
(mapc #'(lambda (p) (add-to-list 'load-path
                                 (f-join edts-plugin-directory p)))
      (edts-plugin-names))
(require 'edts)

;; Global setup
(edts-plugin-init-all)
(make-directory edts-data-directory 'parents)

(mapc #'(lambda(re) (add-to-list 'auto-mode-alist (cons re 'erlang-mode)))
      edts-erlang-mode-regexps)

(provide 'edts-autoloads)
;; Local Variables:
;; no-update-autoloads: t
;; End:
;;; edts-autoloads.el ends here
