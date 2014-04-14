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

(let ((default-directory (expand-file-name "elisp" edts-root-directory)))
    (normal-top-level-add-subdirs-to-load-path))

(require 'f)

(defconst edts-code-directory
  (f-join edts-root-directory "elisp" "edts")
  "Directory where edts code is located.")

(defcustom edts-data-directory
  (if (boundp 'user-emacs-directory)
      (expand-file-name "edts" user-emacs-directory)
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

;; HACKWARNING!! Avert your eyes lest you spend the rest ef your days in agony
;;
;; To avoid weird eproject types like generic-git interfering with us
;; make sure we only consider edts project types.
(defadvice eproject--all-types (around edts-eproject-types)
  "Ignore irrelevant eproject types for files where we should really only
consider EDTS."
  (let ((re (eproject--combine-regexps
             (cons "^\\.edts$" edts-erlang-mode-regexps)))
        (file-name (buffer-file-name)))
    ;; dired buffer has no file
    (if (and file-name
             (string-match re (f-filename file-name)))
        (setq ad-return-value '(edts-otp edts-temp edts generic))
      ad-do-it)))
(ad-activate-regexp "edts-eproject-types")

;; Global setupp
(edts-plugin-init-all)
(make-directory edts-data-directory 'parents)

(mapc #'(lambda(re) (add-to-list 'auto-mode-alist (cons re 'erlang-mode)))
      edts-erlang-mode-regexps)

(provide 'edts-autoloads)
;; Local Variables:
;; no-update-autoloads: t
;; End:
;;; edts-autoloads.el ends here
