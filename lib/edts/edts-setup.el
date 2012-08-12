;; TODO
;; - Keep tabs on modules in projects and auto-reload ones that change
;; - Close down (internal) node when last buffer in a project dies
;;   (erl-unload-hook)
;; - Fix bug with "buffer has a running process, kill it?"
;; - Don't start thousands of processes (related to previous point?).
;; - Fix edb-bug
;; - Fix assertion-bug in epmd.el
;; - Run distel nodeup-hook when inferior node is started.
;; - erl-who-calls without erl-output buffer.
;; - Can we use epmd-port-please to syncronously check if node is up?
;; - Start an internal node if external node disappears.

;; Wishlist
;; - Indexing of project contents.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paths
(add-to-list 'load-path (concat edts-lib-directory "auto-complete"))
(add-to-list 'load-path (concat edts-lib-directory "distel/elisp/"))
(add-to-list 'load-path (concat edts-lib-directory "auto-highlight-symbol-mode"))
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
(custom-set-variables
 '(ahs-exclude (cons erlang-auto-highlight-exclusions ahs-exclude)))


(defconst erlang-auto-highlight-exclusions
  (cons (quote erlang-mode)
               (concat
                "\\(" erlang-operators-regexp
                "\\|" erlang-keywords-regexp "\\)")))

(ahs-regist-range-plugin
 erlang-current-function
 '((name    . "erlang current function")
   (lighter . "CF")
   (face    . ahs-plugin-defalt-face)
   (start   . ahs-range-beginning-of-erlang-function)
   (end     . ahs-range-end-of-erlang-function))
 "Current Erlang function")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer setup
(add-hook 'erlang-mode-hook 'my-erlang-setup)
(defun my-erlang-setup ()
  (auto-highlight-symbol-mode)

  ;; Erlang mode electric commands
  (add-to-list 'erlang-electric-commands 'erlang-electric-newline)
  (setq erlang-next-lines-empty-threshold 0)
  ;; (setq indent-line-function 'my-erlang-indent)
  )




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Erlang-specific keybindings
(define-key erlang-mode-map (kbd "C-c C-d C-b") 'erlang-goto-previous-function)
(define-key erlang-mode-map (kbd "C-c C-d C-f") 'erlang-goto-next-function)
(define-key erlang-mode-map (kbd "C-c C-d C-e") 'erlang-ahs-edit-current-function)
(define-key erlang-mode-map (kbd "C-c C-d C-S-e") 'ahs-edit-mode)
(define-key erlang-mode-map (kbd "M-G") 'erlang-goto-function)

;; WIP
;; (defun my-erlang-indent (&optional whole-exp)
;;   (erlang-indent-command whole-exp)
;;   (when (and ((is-comment) (last-line-is-comment)))
;;     (indent-comment-text)))

;; (defun is-comment-line ()
;;   (interactive)
;;   (save-excursion
;;     (let ((line (buffer-substring (point-at-bol) (point-at-eol))))

;;     (re-search-backward "^[\s-]*%" (line-beginning-position) t)))


;; ;; Hippie Expand (sebastiw's distel)
;; (add-to-list 'load-path "~/elisp/hippie-expand-distel/")
;; (add-to-list 'load-path "~/elisp/company-mode/")
;; (autoload 'company-mode "company" nil t)
;; (require 'company)
;; (defun company-distel-setup ()
;;   (setq company-minimum-prefix-length 1)
;;   (setq company-idle-delay .2)
;;   (setq company-backends 'company-distel)
;;   (company-mode))

(provide 'edts-setup)