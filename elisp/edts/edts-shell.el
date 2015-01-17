;;; edts-shell.el --- Erlang shell related functions.

;; Copyright 2012-2013 Thomas Järvstrand <tjarvstrand@gmail.com>

;; Author: Thomas Järvstrand <thomas.jarvstrand@gmail.com>
;; Keywords: erlang
;; This file is not part of GNU Emacs.

;;
;; This file is part of EDTS.
;;
;; EDTS is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; EDTS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with EDTS. If not, see <http://www.gnu.org/licenses/>.

(require 'edts)
(require 'edts-api)
(require 'edts-complete)

(defvar edts-shell-next-shell-id 0
  "The id to give the next edts-erl shell started.")

(defvar edts-shell-font-lock-defaults nil
  "Font-lock defaults for current-buffer")
(make-variable-buffer-local 'edts-shell-font-lock-defaults)

(defvar edts-shell-font-lock-keywords nil
  "Font-lock defaults for current-buffer")
(make-variable-buffer-local 'edts-shell-font-lock-keywords)

(defvar edts-shell-list nil
  "An alist of currently alive edts-shell buffer's. Each entry in the
list is itself an alist of the shell's properties.")

(defcustom edts-shell-ac-sources
  '(edts-complete-keyword-source
    edts-complete-exported-function-source
    edts-complete-built-in-function-source
    edts-complete-module-source)
  "Sources that EDTS uses for auto-completion in shell (comint)
buffers."
  :type '(repeat symbol)
  :group 'edts)

(defcustom edts-shell-inhibit-comint-input-highlight t
  "Whether or not to inhibit comint's own highlighting of user input.
If nil, syntax highlighting will be removed once input is submitted to
the erlang process."
  :group 'edts
  :type 'boolean)

(defface edts-shell-output-face
  '((default (:inherit font-lock-string-face)))
  "The face to use for process output in edts-shells."
  :group 'edts)

(defconst edts-shell-prompt-regexp
  "([a-zA-Z0-9_-]*\\(@[a-zA-Z0-9_-]*\\)?)[0-9]*> ")

(defun edts-shell (&optional pwd switch-to)
  "Start an interactive erlang shell."
  (interactive '(nil t))
  (edts-api-ensure-server-started)
  (let*((buffer-name (format "*edts[%s]*" edts-shell-next-shell-id))
        (node-name   (format "edts-%s" edts-shell-next-shell-id))
        (command     (list edts-erl-command "-sname" node-name))
        (root        (expand-file-name (or pwd default-directory))))
    (incf edts-shell-next-shell-id)
    (let ((buffer (edts-shell-make-comint-buffer
                   buffer-name
                   node-name
                   root
                   command)))
      (edts-api-init-node-when-ready node-name node-name root nil)
      (when switch-to (switch-to-buffer buffer))
      buffer)))

(defadvice start-process (around edts-shell-start-process-advice)
  "Sets the TERM environment variable to vt100 to ensure that erl is
compatible with edts-shell. The reason for doing it here is that setting
it on the command-line is problematic for projects that call their own
start-scripts and because the TERM variable in `process-environment' is
unconditionally set by comint before calling `process-start' so that any
previous value is overwritten."
  (let ((process-environment (cons "TERM=vt100" process-environment)))
    ad-do-it))

(defun edts-shell-make-comint-buffer (buffer-name node-name pwd command)
  "In a comint-mode buffer Starts a node with BUFFER-NAME by cd'ing to
PWD and running COMMAND."
  (let* ((cmd  (car command))
         (args (cdr command))
         (pwd  (expand-file-name pwd)))
    (with-current-buffer (get-buffer-create buffer-name) (cd pwd))
    (ad-activate-regexp "edts-shell-start-process-advice")
    (apply #'make-comint-in-buffer cmd buffer-name cmd nil args)
    (ad-deactivate-regexp "edts-shell-start-process-advice")
    (with-current-buffer buffer-name
      (edts-shell-mode 1)
      ;; edts-specifics
      (setq edts-api-node-name
            (or (edts-shell-node-name-from-args args) node-name))
      (add-to-list
       'edts-shell-list `(,(buffer-name) . ((default-directory . ,pwd))))))
  (get-buffer buffer-name))

(define-minor-mode edts-shell-mode
  "Minor mode for running and EDTS Erlang shell in an Emacs buffer."
  :lighter " EDTS-shell"
  :group edts
  :require erlang-mode
  (if edts-shell-mode
      (edts-shell-mode-setup)))

;; FIXME This isn't working anyway
;; workaround to get proper variable highlighting in the shell.
;; (defvar erlang-font-lock-keywords-vars
;;   (list
;;    (list
;;     #'(lambda (max)
;;         (block nil
;;           (while (re-search-forward erlang-variable-regexp max 'move-point)
;;             ;; no numerical constants
;;             (unless (eq ?# (char-before (match-beginning 0)))
;;               (return (match-string 0))))))
;;     1 'font-lock-variable-name-face nil))
;;   "Font lock keyword highlighting Erlang variables.
;; Must be preceded by `erlang-font-lock-keywords-macros' to work properly.")

(defun edts-shell-mode-setup ()
  ;; generic stuff
  (ignore-errors
    (make-local-variable 'show-paren-mode)
    (show-paren-mode 1))
  (linum-mode -1)
  (setq show-trailing-whitespace nil)
  (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)
  (add-hook 'kill-buffer-hook #'edts-shell--kill-buffer-hook t t)

  ;; comint-variables
  (add-hook 'comint-output-filter-functions
            'edts-shell-comint-output-filter nil t)
  (add-hook 'comint-preoutput-filter-functions
            'edts-shell-comint-preoutput-filter nil t)
  (add-hook 'comint-input-filter-functions
            'edts-shell-comint-input-filter nil t)
  (make-local-variable 'comint-prompt-read-only)
  (setq comint-input-sender-no-newline t)
  (setq comint-process-echoes t)
  (setq comint-prompt-read-only t)
  (setq comint-scroll-to-bottom-on-input t)
  ;; We don't like tabs in our shells. The tab-key should only be used for
  ;; completion and is set to do just that when auto-complete-mode's
  ;; keymap is active.
  (make-local-variable 'comint-mode-map)
  (define-key comint-mode-map "\t" 'ignore)

  ;; erlang-mode syntax highlighting
  (edts-shell-font-lock-init)

  ;; Auto-completion
  (edts-complete-setup edts-shell-ac-sources))

(defvar edts-shell-prompt-output-p nil
  "Non nil if the Erlang shell has output its first prompt.")
(make-variable-buffer-local 'edts-shell-prompt-output-p)

(defun edts-shell-comint-preoutput-filter (str)
  "Comint preoutput-filter-function for edts-shell."
  (unless edts-shell-prompt-output-p
    (when (string-match edts-shell-prompt-regexp str)
      (setq edts-shell-prompt-output-p t))
    (setq str (replace-regexp-in-string "\\^G" "C-q C-g RET" str)))
  (put-text-property 0
                     (1- (length str))
                     'font-lock-face
                     'edts-shell-output-face str)
  str)

(defun edts-shell-comint-output-filter (str)
  "Comint output-filter-function for edts-shell."
  (edts-shell-maybe-toggle-completion str)
  ;; Set read-only for all text up until output-start + length of str
  (edts-shell-set-output-read-only)
  ;; Set read-only and remove comint's highlighting for input.
  (edts-shell-set-input-properties))

(defun edts-shell-maybe-toggle-completion (last-output)
  (save-match-data
    (if (string-match "\^M\n --> $" last-output)
        (edts-complete -1)
      (when (> (length last-output) 0)
        (edts-complete 1)))))

(defun edts-shell-set-input-properties ()
  "Update properties of text from `comint-last-input-start' until
`comint-last-input-start'."
  (let ((inhibit-read-only t)
        (start comint-last-input-start)
        (end comint-last-input-end))
    ;; Remove the font-lock-face property that comint likes to insert. Only the
    ;; property name is used when removing, the value is ignored.
    (remove-text-properties start end '(font-lock-face nil))
    ;; Make previous input read-only.
    (put-text-property start end 'read-only t)))

(defun edts-shell-set-output-read-only ()
  "Makes all text read-only from `comint-output-start' up until `process-mark'."
  (let* ((start comint-last-output-start)
         (end  (1- (process-mark (get-buffer-process (current-buffer)))))
         (inhibit-read-only t))
    (add-text-properties start end '(field output read-only t))))

(defun edts-shell-comint-input-filter (arg)
  "Comint input-filter-function for edts-shell."
  (setq buffer-undo-list nil)
  (if (string-match (format ".*%s\n$" (char-to-string ?\^G)) arg)
    ;; Entering the shell job control, switch off auto completion
      (edts-complete -1)
    ;; Otherwise switch auto-completion back on if it's off, ie
    ;; if we were previously in the job control
    (unless auto-complete-mode
      (edts-complete 1)))
  arg)


(defun edts-shell-font-lock-init ()
  "Set up the proper values for font lock variables, but do it in a
separate temporary buffer and only carry the values of
`font-lock-defaults' and `font-lock-keywords' over to the buffer-local
`edts-font-lock-defaults' and `edts-shell-font-lock-keywords'
respectively so we can use them later when fontifying user input."
  (let ((defaults nil)
        (keywords nil))
    (with-temp-buffer
      (erlang-syntax-table-init)
      (erlang-font-lock-init)
      (setq defaults font-lock-defaults)
      (setq keywords font-lock-keywords))
    (setq edts-shell-font-lock-defaults defaults)
    (setq edts-shell-font-lock-keywords keywords))
  (set (make-local-variable 'font-lock-fontify-region-function)
       'edts-shell-font-lock-fontify-region))

(defun edts-shell-font-lock-fontify-region (start end loudly)
  (while (< start end)
    (let ((temp-end nil))
      (case (get-text-property start 'field)
        ('output
         (setq temp-end (edts-shell-output-end start end))
         (font-lock-default-fontify-region start temp-end loudly))
        (otherwise
         (setq temp-end (edts-shell--non-output-end start end))
         (edts-shell--fontify-non-output-region start temp-end loudly)))
      (setq start (1+ temp-end)))))

(defun edts-shell-output-end (start bound)
  "Return the last position of the output field starting at START,
bounded by BOUND."
  (or (text-property-not-all start bound 'field 'output) bound))

(defun edts-shell--non-output-end (start bound)
  "Return the last position of the non-output field starting at START,
bounded by BOUND."
  (let ((output-start (text-property-any start bound 'field 'output)))
    (if output-start
        (1- output-start)
      bound)))

(defun edts-shell--fontify-non-output-region (start end loudly)
  (let ((font-lock-defaults edts-shell-font-lock-defaults)
        (font-lock-keywords edts-shell-font-lock-keywords))
    (with-syntax-table erlang-mode-syntax-table
      (font-lock-default-fontify-region start end loudly)
      ;; Narrow to region so that font-lock doesn't accidentally catch any
      ;; unmatched quotes from process output and puts font-lock-string-face on
      ;; the entire buffer.
      (save-restriction
        (narrow-to-region start end)
        (font-lock-fontify-syntactically-region start end loudly)))))

(defun edts-shell--kill-buffer-hook ()
  "Removes the buffer from `edts-shell-list'."
  (setq edts-shell-list (assq-delete-all (buffer-name) edts-shell-list)))

(defun edts-shell-kill-all ()
  "Kill all edts-shell buffers (including the edts-server)."
  (interactive)
  (mapc #'(lambda (shell-entry)
            (kill-buffer (car shell-entry))
            (edts-log-info "Killed %s" (car shell-entry)))
        edts-shell-list)
  (setq edts-shell-list nil))

(defun edts-shell-node-name-from-args (args)
  "Return node sname based on args"
  (block nil
    (while args
      (when (string= (car args) "-sname")
        (return (cadr args)))
      (pop args))))

(defun edts-shell-find-by-path (path)
  "Return the buffer of the first found shell with PATH as its
default directory if it exists, otherwise nil."
  (block nil
    (let ((shells edts-shell-list))
      (while shells
        (when (string= path (cdr (assoc 'default-directory (cdar shells))))
          (return (get-buffer (caar shells))))
          (pop shells)))))

(provide 'edts-shell)
