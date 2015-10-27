;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; EDTS Setup and configuration.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Prerequisites
(require 'auto-highlight-symbol)
(require 'erlang)
(require 'f)

(defgroup edts nil
  "Erlang development tools"
  :group 'convenience
  :prefix "edts-")

;;;###autoload
(eval-and-compile
 (add-to-list 'load-path
              (file-name-as-directory
               (expand-file-name "elisp/edts"
                                 (file-name-directory (or load-file-name
                                                          byte-compile-current-file))))))

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

(defcustom edts-erl-command
  (or (executable-find "erl")
      (null
       (warn
        "No erl on exec-path. Most of EDTS' functionality will be broken.")))
  "Location of the erl-executable to use when launching the main EDTS-node."
  :type 'file
  :group 'edts)

(defcustom edts-erl-flags
  ""
  "Flags to use when launching the main EDTS-node."
  :type 'string
  :group 'edts)

(eval-and-compile
  (defconst edts-root-directory
    (file-name-directory (or (locate-library "edts-autoloads")
                             load-file-name
                             default-directory))
    "EDTS root directory."))
(add-to-list 'load-path edts-root-directory)

(defconst edts-code-directory
  (f-join edts-root-directory "elisp" "edts")
  "Directory where edts code is located.")
(add-to-list 'load-path edts-root-directory)

(defcustom edts-data-directory
  (if (boundp 'user-emacs-directory)
      (expand-file-name "edts" user-emacs-directory)
    (expand-file-name "~/.emacs.d"))
  "Where EDTS should save its data."
  :type 'directory
  :group 'edts)

(defconst edts-lib-directory
  (f-join edts-root-directory "elisp")
  "Directory where edts libraries are located.")
(dolist (dir (f-directories edts-lib-directory))
  (add-to-list 'load-path dir))

(defconst edts-plugin-directory
  (f-join edts-root-directory "plugins")
  "Directory where edts plugins are located.")

(dolist (dir (f-directories edts-plugin-directory))
  (add-to-list 'load-path dir))

(defconst edts-test-directory
  (f-join edts-root-directory "test")
  "Directory where edts test data are located.")

(defconst edts-erl-root
  (and edts-erl-command
       (file-name-directory
        (directory-file-name
         (file-name-directory (f-canonical edts-erl-command)))))
  "Location of the Erlang root directory")


(require 'edts)
(require 'edts-api)
(require 'edts-code)
(require 'edts-complete)
(require 'edts-face)
(require 'edts-log)
(require 'edts-project)
(require 'edts-plugin)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; autohighlight-symbol-mode setup for EDTS
(defconst edts-auto-highlight-exclusions
  (cons (quote erlang-mode)
               (concat
                "^\\(" erlang-operators-regexp
                "\\|" erlang-keywords-regexp
                "\\|\\<[[:digit:]]+\\>\\)$")))

(defvar edts-current-function-ahs-plugin
  '((name    . "erlang current function")
   (lighter . "CF")
   (face    . ahs-plugin-defalt-face)
   (start   . ferl-point-beginning-of-function)
   (end     . ferl-point-end-of-function)))

(defvar edts-mode-hook nil
  "Hooks to run at the end of edts-mode initialization in a buffer.")

(defun edts-setup ()
  (edts-log-debug "Setting up edts-mode in buffer %s" (current-buffer))

  ;; Start with our own stuff
  (edts-face-remove-overlays)
  (edts-api-ensure-server-started)
  (add-hook 'after-save-hook 'edts-code-compile-and-display t t)

  (auto-highlight-symbol-mode t)
  (add-to-list 'ahs-exclude edts-auto-highlight-exclusions)
  (make-local-variable 'ahs-case-fold-search)
  (setq ahs-case-fold-search nil)

  ;; Register the range plugin with ahs
  (ahs-regist-range-plugin
    edts-current-function
    edts-current-function-ahs-plugin
    "Current Erlang function")

  ;; Make sure we remember our history
  (if (boundp 'window-persistent-parameters)
      (add-to-list 'window-persistent-parameters '(edts-find-history-ring . t))
      (setq window-persistent-parameters '((edts-find-history-ring . t))))

  ;; Ensure matching parentheses are visible above edts-faces.
  (when (and (boundp 'show-paren-priority)
             (< show-paren-priority edts-code-issue-overlay-max-priority))
    (make-local-variable 'show-paren-priority)
    (setq show-paren-priority (1+ edts-code-issue-overlay-max-priority)))

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

;;;###autoload
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

(make-directory edts-data-directory 'parents)
(edts-plugin-init-all)

(provide 'edts-mode)

(eval-and-compile
  (defun edts-compile-deps ()
  "Compile EDTS' external (Erlang) dependecies."
  (interactive)
  (let ((default-directory edts-root-directory)
        (buf  "*EDTS compile*"))
    (pop-to-buffer (get-buffer-create buf))
    (goto-char (point-max))
    (let* ((path (mapconcat #'expand-file-name exec-path ":"))
           (process-environment (cons (concat "PATH=" path)
                                      process-environment)))
      (if (= (call-process "make" nil t t "libs" "plugins") 0)
          (when (called-interactively-p 'interactive)
            (quit-window))
        (error (format (concat "Failed to compile EDTS libraries. "
                               "See %s for details.")
                       buf))))))

  (unless load-file-name
    (edts-compile-deps)))
