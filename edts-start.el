;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; EDTS Setup and configuration.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Prerequisites
(require 'cl)
(require 'woman)
(require 'ert nil 'noerror)

(defvar edts-start-inhibit-load-msgs t
  "If non-nil, don't print messages when loading edts-packages.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paths

(eval-and-compile
  (defconst edts-root-directory
    (file-name-directory (or (locate-library "edts-start")
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
    "Directory where edts test data are located."))

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

;; Add all libs to load-path
(loop for  (name dirp . rest)
      in   (directory-files-and-attributes edts-lib-directory nil "^[^.]")
      when dirp
      do   (add-to-list 'load-path (f-join edts-lib-directory name)))

;; Add plugins to load-path
(require 'edts-plugin)
(mapc #'(lambda (p) (add-to-list 'load-path
                                 (f-join edts-plugin-directory p)))
      (edts-plugin-names))

(defcustom edts-erlang-mode-regexps
  '("^\\.erlang$"
    "\\.app$"
    "\\.app.src$"
    "\\.config$"
    "\\.erl$"
    "\\.es$"
    "\\.escript$"
    "\\.eterm$"
    "\\.script$"
    "\\.yaws$")
  "Additional extensions for which to auto-activate erlang-mode."
  :group 'edts)

(mapc #'(lambda(re) (add-to-list 'auto-mode-alist (cons re 'erlang-mode)))
      edts-erlang-mode-regexps)

;; workaround to get proper variable highlighting in the shell.
(defvar erlang-font-lock-keywords-vars
  (list
   (list
    #'(lambda (max)
        (block nil
          (while (re-search-forward erlang-variable-regexp max 'move-point)
            ;; no numerical constants
            (unless (eq ?# (char-before (match-beginning 0)))
              (return (match-string 0))))))
    1 'font-lock-variable-name-face nil))
  "Font lock keyword highlighting Erlang variables.
Must be preceded by `erlang-font-lock-keywords-macros' to work properly.")

(defun edts-start-load (file)
  "Load file"
  (load file nil edts-start-inhibit-load-msgs))

;; EDTS
(loop
 for  file
 in   (sort (directory-files edts-code-directory nil "\\.el$") #'string<)
 ;; avoid symlinks created as emacs backups
 when (and (not (file-symlink-p (f-join edts-code-directory file)))
           (not (string-match ".*-test" (f-base file))))
 do   (edts-start-load file))

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

(defgroup edts nil
  "Erlang development tools"
  :group 'convenience
  :prefix "edts-")

(defalias 'edts-inhibit-fringe-markers 'edts-face-inhibit-fringe-markers)
(defalias 'edts-marker-fringe 'edts-face-marker-fringe)

(defun edts-byte-compile ()
  "Byte-compile all elisp packages part of EDTS."
  (interactive)
  (let* ((dirs (directory-files edts-lib-directory t "^[^.]"))
         (files (apply #'append
                       (mapcar #'(lambda (dir)
                                   (directory-files dir t "\\.el$")) dirs))))
    (byte-compile-disable-warning 'cl-functions)
    (mapc #'byte-compile-file files)
    t))

(defun edts-start-load-tests ()
  "Load all test-files."
  (loop
   for  file
   in   (directory-files edts-code-directory nil "-test\\.el$")
   ;; avoid symlinks created as emacs backups
   when (not (file-symlink-p (f-join edts-code-directory file)))
   do   (edts-start-load file))
  (edts-plugin-load-tests))

;; Global setup
(edts-plugin-init-all)
(make-directory edts-data-directory 'parents)

(provide 'edts-start)
