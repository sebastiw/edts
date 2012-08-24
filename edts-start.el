;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; EDTS Setup and configuration.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar edts-lib-directory
  (file-truename
   (concat (file-name-directory
            (or (locate-library "edts-start") load-file-name)) "/lib/"))
  "Directory where edts libraries are located.")
(add-to-list 'load-path (concat "lib/edts/elisp/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paths
;;
;; Add all directory names in edts-lib-directory to load-path except . and ..
(mapcar
 #'(lambda (path) (add-to-list 'load-path path))
 (remove-if-not #'(lambda (f)
                    (when (file-directory-p f)
                      (let ((file-name (file-name-nondirectory f)))
                        (and (not (equal "." file-name))
                             (not (equal ".." file-name))))))
                (directory-files edts-lib-directory t)))
(add-to-list 'load-path (concat edts-lib-directory "edts/elisp/"))
(add-to-list 'exec-path (concat (directory-file-name erlang-root-dir)
                                "/bin"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Erlang mode
(require 'erlang)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rest of edts
(require 'ferl)
(require 'edts)
(require 'edts-rest)
(edts-ensure-server-started)
(require 'edts-project)
(edts-project-init)
(require 'edts-navigate)
(if (boundp 'window-persistent-parameters)
  (add-to-list 'window-persistent-parameters '(edts-find-history-ring . t))
  (setq         window-persistent-parameters '((edts-find-history-ring . t))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation
(add-hook 'align-load-hook
          (lambda ()
            (add-to-list 'align-rules-list
                         '(erlang-align
                           (regexp . ",\\(\\s-+\\)")
                           (repeat . t)
                           (modes quote (erlang-mode))))))

;; Auto-activate erlang mode for some additional extensions.
(add-to-list 'auto-mode-alist '("\\.yaws$" .     erlang-mode))
(add-to-list 'auto-mode-alist '("\\.eterm$" .    erlang-mode))
(add-to-list 'auto-mode-alist '("rebar.config$". erlang-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; autohighlight-symbol-mode for erlang
(require 'auto-highlight-symbol)

(defconst erlang-auto-highlight-exclusions
  (cons (quote erlang-mode)
               (concat
                "\\(" erlang-operators-regexp
                "\\|" erlang-keywords-regexp
                "\\|\\<[[:digit:]]+\\>\\)")))

(custom-set-variables
 '(ahs-exclude (cons erlang-auto-highlight-exclusions ahs-exclude)))

(ahs-regist-range-plugin
 erlang-current-function
 '((name    . "erlang current function")
   (lighter . "CF")
   (face    . ahs-plugin-defalt-face)
   (start   . ferl-point-beginning-of-function)
   (end     . ferl-point-end-of-function))
 "Current Erlang function")

(add-hook 'erlang-mode-hook #'(lambda () (auto-highlight-symbol-mode t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-completion
(require 'erl-complete)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Erlang-specific keybindings
(define-key erlang-mode-map (kbd "M-G")           'ferl-goto-function)
(define-key erlang-mode-map (kbd "C-c C-d C-b")   'ferl-goto-previous-function)
(define-key erlang-mode-map (kbd "C-c C-d C-f")   'ferl-goto-next-function)
(define-key erlang-mode-map (kbd "C-c C-d C-e")   'edts-ahs-edit-current-function)
(define-key erlang-mode-map (kbd "C-c C-d C-S-e") 'ahs-edit-mode)
(define-key erlang-mode-map (kbd "M-.")           'edts-find-source)
(define-key erlang-mode-map (kbd "M-,")           'edts-find-source-unwind))
(define-key erlang-mode-map (kbd "C-c C-d F")     'edts-find-module)
(when (boundp 'erlang-extended-mode-map)
  (define-key erlang-extended-mode-map (kbd "C-c C-d F") 'edts-find-module)
  (define-key erlang-extended-mode-map (kbd "M-.") 'edts-find-source-under-point)
  (define-key erlang-extended-mode-map (kbd "M-,") 'edts-find-source-unwind))

(provide 'edts-start)