;;; edts-autoloads.el ---
;;
;;; Code:
(require 'f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paths

(defconst edts-root-directory
  (file-name-directory (or (locate-library "edts-autoloads")
                           load-file-name
                           default-directory))
  "EDTS root directory.")

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
