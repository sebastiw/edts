;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; EDTS Setup and configuration.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paths
(defvar edts-lib-directory
  (concat (file-name-directory
            (or (locate-library "edts-start") load-file-name)) "/elisp/")
  "Directory where edts libraries are located.")
(add-to-list 'load-path (concat edts-lib-directory "edts"))
(add-to-list 'load-path (concat edts-lib-directory "auto-complete"))
(add-to-list 'load-path (concat edts-lib-directory "auto-highlight-symbol-mode"))
(add-to-list 'load-path (concat edts-lib-directory "popup-el"))
(when (boundp 'erlang-root-dir)
  (add-to-list 'exec-path (concat (directory-file-name erlang-root-dir) "/bin")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Requires

;; Prerequisites
(require 'cl)
(require 'erlang)
(require 'ert nil 'noerror)
(require 'woman)

;; EDTS
(require 'ferl)
(require 'edts)
(require 'edts-log)
(require 'edts-code)
(require 'edts-complete)
(require 'edts-doc)
(require 'edts-rest)
(require 'edts-face)
(require 'edts-project)
(require 'edts-navigate)

;; External
(require 'auto-highlight-symbol)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; autohighlight-symbol-mode setup for EDTS
(defconst erlang-auto-highlight-exclusions
  (cons (quote erlang-mode)
               (concat
                "\\(" erlang-operators-regexp
                "\\|" erlang-keywords-regexp
                "\\|\\<[[:digit:]]+\\>\\)")))

(defvar erlang-current-function-ahs-plugin
  '((name    . "erlang current function")
   (lighter . "CF")
   (face    . ahs-plugin-defalt-face)
   (start   . ferl-point-beginning-of-function)
   (end     . ferl-point-end-of-function)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EDTS mode

(defgroup edts nil
  "Erlang development tools"
  :group 'convenience
  :prefix "edts-")

(defvar edts-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-n")     'edts-code-next-issue)
    (define-key map (kbd "C-c C-p")     'edts-code-previous-issue)
    (define-key map (kbd "C-c C-d f")   'ferl-goto-function)
    (define-key map (kbd "C-c C-d F")   'edts-find-module)
    (define-key map (kbd "C-c C-d H")   'edts-find-doc)
    (define-key map (kbd "C-c C-d w")   'edts-who-calls)
    (define-key map (kbd "C-c C-d C-w") 'edts-last-who-calls)
    (define-key map (kbd "C-c C-d C-b") 'ferl-goto-previous-function)
    (define-key map (kbd "C-c C-d C-f") 'ferl-goto-next-function)
    (define-key map (kbd "C-c C-d e")   'edts-ahs-edit-current-function)
    (define-key map (kbd "C-c C-d C-e") 'ahs-edit-mode)
    (define-key map (kbd "M-.")         'edts-find-source-under-point)
    (define-key map (kbd "M-,")         'edts-find-source-unwind)
    map)
  "Keymap for EDTS.")

(defcustom edts-erlang-mode-regexps
  '("rebar.config$"
    "rebar.config.script$"
    "\\.app$"
    "\\.app.src$"
    "\\.es$"
    "\\.escript$"
    "\\.eterm$"
    "\\.yaws$")
  "Additional extensions for which to auto-activate erlang-mode.")

(defun edts-setup ()
  ;; Start with our own stuff
  (edts-ensure-server-started)
  (edts-project-init)
  (ad-activate-regexp "edts-.*")
  (add-hook 'after-save-hook 'edts-code-compile-and-display t t)

  ;; Auto-activate erlang mode for some additional extensions.
  (mapcar
   #'(lambda(re) (add-to-list 'auto-mode-alist (cons re 'erlang-mode)))
   edts-erlang-mode-regexps)

  (auto-highlight-symbol-mode t)
  (add-to-list 'ahs-exclude erlang-auto-highlight-exclusions)

  ;; Register the range plugin with ahs
  (ahs-regist-range-plugin
    erlang-current-function
    erlang-current-function-ahs-plugin
    "Current Erlang function")

  ;; Make sure we remember our history
  (if (boundp 'window-persistent-parameters)
      (add-to-list 'window-persistent-parameters '(edts-find-history-ring . t))
      (setq window-persistent-parameters '((edts-find-history-ring . t))))

  ;; Auto-completion
  (edts-complete-setup))

(defun edts-teardown ()
  ;; Start with our own stuff
  (ad-deactivate-regexp "edts-.*")
  (remove-hook 'after-save-hook 'edts-code-compile-and-display t)
  (auto-highlight-symbol-mode -1)

  ;; Indentation
  (remove-hook 'align-load-hook 'edts-align-hook))

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

\\{edts-mode-map}"
  :lighter " EDTS"
  :keymap edts-mode-map
  :group edts
  :require erlang-mode
  (if edts-mode
      (edts-setup)
      (edts-teardown)))

(defun edts-erlang-mode-hook ()
  (edts-mode t))

(eval-and-compile
  (add-hook 'erlang-mode-hook 'edts-erlang-mode-hook)

  (provide 'edts-start))
