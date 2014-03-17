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
(defconst edts-root-directory
  (file-name-directory (or (locate-library "edts-start") load-file-name))
  "EDTS root directory.")

(add-to-list 'load-path (concat edts-root-directory "elisp/path-util"))
(require 'path-util)

(defconst edts-code-directory
  (path-util-join edts-root-directory "elisp" "edts")
  "Directory where edts code is located.")

(defconst edts-lib-directory
  (path-util-join edts-root-directory "elisp")
  "Directory where edts libraries are located.")

(defconst edts-plugin-directory
  (path-util-join edts-root-directory "plugins")
  "Directory where edts plugins are located.")

(defconst edts-test-directory
  (path-util-join edts-root-directory "test")
  "Directory where edts test data are located.")

(unless (require 'erlang nil 'noerror)
  (add-to-list 'load-path
               (car
                (file-expand-wildcards
                 (path-util-join
                  (path-util-pop (file-truename (executable-find "erl")) 2)
                           "lib"
                           "tools*"
                           "emacs"))))
  (require 'erlang))

;; Add all libs to load-path
(loop for  (name dirp . rest)
      in   (directory-files-and-attributes edts-lib-directory nil "^[^.]")
      when dirp
      do   (add-to-list 'load-path (path-util-join edts-lib-directory name)))

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
  "Additional extensions for which to auto-activate erlang-mode.")

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

;; EDTS
(loop
 for  file
 in   (sort (directory-files edts-code-directory nil "\\.el$") #'string<)
 ;; avoid symlinks created as emacs backups
 when (not (file-symlink-p (path-util-join edts-code-directory file)))
 do   (load file nil edts-start-inhibit-load-msgs))

;; External
(require 'auto-highlight-symbol)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; autohighlight-symbol-mode setup for EDTS
(defconst erlang-auto-highlight-exclusions
  (cons (quote erlang-mode)
               (concat
                "^\\(" erlang-operators-regexp
                "\\|" erlang-keywords-regexp
                "\\|\\<[[:digit:]]+\\>\\)$")))

(defvar erlang-current-function-ahs-plugin
  '((name    . "erlang current function")
   (lighter . "CF")
   (face    . ahs-plugin-defalt-face)
   (start   . ferl-point-beginning-of-function)
   (end     . ferl-point-end-of-function)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EDTS mode


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
             (string-match re (path-util-base-name file-name)))
        (setq ad-return-value '(edts-otp edts-temp edts generic))
      ad-do-it)))
(ad-activate-regexp "edts-eproject-types")


(defgroup edts nil
  "Erlang development tools"
  :group 'convenience
  :prefix "edts-")

(defvar edts-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-n"     'edts-code-next-issue)
    (define-key map "\C-c\C-p"     'edts-code-previous-issue)
    (define-key map "\C-c\C-df"    'edts-find-local-function)
    (define-key map "\C-c\C-d\S-f" 'edts-find-global-function)
    (define-key map "\C-c\C-dH"    'edts-find-doc)
    (define-key map "\C-c\C-dh"    'edts-show-doc-under-point)
    (define-key map "\C-c\C-d\C-b" 'ferl-goto-previous-function)
    (define-key map "\C-c\C-d\C-f" 'ferl-goto-next-function)
    (define-key map "\C-c\C-de"    'edts-ahs-edit-current-function)
    (define-key map "\C-c\C-dE"    'edts-ahs-edit-buffer)
    (define-key map "\C-c\C-dt"    'edts-code-eunit)
    (define-key map "\M-."         'edts-find-source-under-point)
    (define-key map "\M-,"         'edts-find-source-unwind)
    map)
  "Keymap for EDTS.")

(defvar edts-mode-hook nil
  "Hooks to run at the end of edts-mode initialization in a buffer.")

(defvar edts-orig-left-margin nil
  "The original value of a buffer's left-margin width")
(make-variable-buffer-local 'edts-orig-left-margin)

(defvar edts-inhibit-fringe-markers nil
  "If non-nil, do not display markers in the fringe for errors etc.")

(defcustom edts-marker-fringe 'left-fringe
  "Which side to display fringe-markers on. The value must be either
left-fringe or right-fringe.")

(defun edts-setup ()
  (edts-log-debug "Setting up edts-mode in buffer %s" (current-buffer))

  ;; Start with our own stuff
  (edts-face-remove-overlays)
  (edts-ensure-server-started)
  (ad-activate-regexp "edts-face.*")
  (add-hook 'after-save-hook 'edts-code-compile-and-display t t)

  ;; Auto-activate erlang mode for some additional extensions.
  (mapcar
   #'(lambda(re) (add-to-list 'auto-mode-alist (cons re 'erlang-mode)))
   edts-erlang-mode-regexps)

  (auto-highlight-symbol-mode t)
  (add-to-list 'ahs-exclude erlang-auto-highlight-exclusions)
  (make-local-variable 'ahs-case-fold-search)
  (setq ahs-case-fold-search nil)

  ;; Register the range plugin with ahs
  (ahs-regist-range-plugin
    erlang-current-function
    erlang-current-function-ahs-plugin
    "Current Erlang function")

  ;; Make sure we remember our history
  (if (boundp 'window-persistent-parameters)
      (add-to-list 'window-persistent-parameters '(edts-find-history-ring . t))
      (setq window-persistent-parameters '((edts-find-history-ring . t))))

  ;; Ensure matching parentheses are visible above edts-faces.
  (when (boundp 'show-paren-priority)
    (make-local-variable 'show-paren-priority))
  (let ((paren-prio (or
                     (and (boundp 'show-paren-priority) show-paren-priority)
                     0)))
    (setq show-paren-priority
          (max paren-prio
               (+ 1 (apply #'max
                           (mapcar
                            #'cdr edts-code-issue-overlay-priorities))))))

  ;; Auto-completion
  (edts-complete-setup)
  (run-hooks 'edts-mode-hook))

(defun edts-teardown ()
  ;; Start with our own stuff
  (edts-face-remove-overlays)
  (ad-deactivate-regexp "edts-.*")
  (remove-hook 'after-save-hook 'edts-code-compile-and-display t)
  (auto-highlight-symbol-mode -1)

  ;; Remove custom value for show-paren-priority
  (if (boundp 'show-paren-priority)
      (kill-local-variable 'show-paren-priority)))

(defvar edts-mode nil
  "The edts mode-variable.")

(define-minor-mode edts-mode
  "An easy to set up Development-environment for Erlang. See README for
details about EDTS.

EDTS also incorporates a couple of other
minor-modes, currently auto-highlight-mode and auto-complete-mode.
They are configured to work together with EDTS but see their respective
documentation for information on how to configure their behaviour
further.

\\{edts-mode-map}Other useful commands:
\\[edts-buffer-node-name]           - Display the project node-name of
                                      current-buffer.
\\[edts-code-compile-and-display]   - Compile current buffer and display
                                      issues.
\\[edts-code-xref-analyze]          - Run xref analysis on current
                                      buffer.
\\[edts-code-xref-analyze-related]  - Runs xref-checks for all
                                      live buffers related to
                                      current buffer either by
                                      belonging to the same
                                      project or, if current
                                      buffer does not belong to
                                      any project, being in the
                                      same directory as the
                                      current buffer's file.
\\[edts-dialyzer-analyze]           - Same as the xref-check
                                      above, but for dialyzer.
\\[edts-byte-compile]               - Byte compile all EDTS elisp files.
\\[edts-project-start-node]         - Start current buffers project-node
                                      if not already running.
\\[edts-refactor-extract-function]  - Extract code in current region
                                      into a separate function.
\\[edts-init-node]                  - Register the project-node of
                                      current buffer with the central
                                      EDTS server.
\\[edts-shell]                      - Start an interactive Erlang shell.
\\[edts-start-server]               - Start the central EDTS server.
\\[edts-man-setup]                  - Install the OTP documentation"

  :lighter " EDTS"
  :keymap edts-mode-map
  :group edts
  :require erlang-mode
  (if edts-mode
      (edts-setup)
      (edts-teardown)))

(defun edts-erlang-mode-hook ()
  (when (buffer-file-name)
    (edts-mode t)))

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

;; Global setup
(edts-plugin-init-all)
(make-directory edts-data-directory 'parents)
(add-hook 'erlang-mode-hook 'edts-erlang-mode-hook)

(provide 'edts-start)
