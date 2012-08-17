;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; EDTS Setup and configuration.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
(add-to-list 'load-path (concat edts-lib-directory "distel/elisp/"))
(add-to-list 'load-path (concat (directory-file-name erlang-root-dir)
                                "/lib/tools/emacs"))
(add-to-list 'exec-path (concat (directory-file-name erlang-root-dir)
                                "/bin"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation
(add-hook 'align-load-hook
          (lambda ()
            (add-to-list 'align-rules-list
                         '(erlang-align
                           (regexp . ",\\(\\s-+\\)")
                           (repeat . t)
                           (modes quote (erlang-mode))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Erlang mode
(require 'erlang-start)

;; Auto-activate erlang mode for some additional extensions.
(add-to-list 'auto-mode-alist '("\\.yaws$" .     erlang-mode))
(add-to-list 'auto-mode-alist '("\\.eterm$" .    erlang-mode))
(add-to-list 'auto-mode-alist '("rebar.config$". erlang-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Distel
(require 'distel)
(distel-setup)

(require 'erl-project)
(erl-project-init)

;; Erlang Compile Server (sebastiw's distel)
(require 'erlang-compile-server)
(setq erl-ecs-backends '(xref dialyzer eunit))
(setq erl-ecs-backends nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; autohighlight-symbol-mode for erlang
(require 'auto-highlight-symbol)

(defconst erlang-auto-highlight-exclusions
  (cons (quote erlang-mode)
               (concat
                "\\(" erlang-operators-regexp
                "\\|" erlang-keywords-regexp "\\)")))

(custom-set-variables
 '(ahs-exclude (cons erlang-auto-highlight-exclusions ahs-exclude)))

(ahs-regist-range-plugin
 erlang-current-function
 '((name    . "erlang current function")
   (lighter . "CF")
   (face    . ahs-plugin-defalt-face)
   (start   . ahs-range-beginning-of-erlang-function)
   (end     . ahs-range-end-of-erlang-function))
 "Current Erlang function")

(add-hook 'erlang-mode-hook #'(lambda () (auto-highlight-symbol-mode t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-completion
(require 'erl-complete)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Erlang-specific keybindings
(define-key erlang-mode-map (kbd "C-c C-d C-b") 'erlang-goto-previous-function)
(define-key erlang-mode-map (kbd "C-c C-d C-f") 'erlang-goto-next-function)
(define-key erlang-mode-map (kbd "C-c C-d C-e") 'erlang-ahs-edit-current-function)
(define-key erlang-mode-map (kbd "C-c C-d C-S-e") 'ahs-edit-mode)
(define-key erlang-mode-map (kbd "M-G") 'erlang-goto-function)

(provide 'edts-setup)