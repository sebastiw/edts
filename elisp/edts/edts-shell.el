;; Copyright 2012 Thomas Järvstrand <tjarvstrand@gmail.com>
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
;;
;; Erlang shell related functions

(require 'cl)

(defvar edts-shell-next-shell-id 0
  "The id to give the next edts-erl shell started.")

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

(defun edts-shell (&optional switch-to)
  "Start an interactive erlang shell."
  (interactive '(t))
  (edts-ensure-server-started)
  (let*((buffer-name (format "*edts[%s]*" edts-shell-next-shell-id))
        (node-name   (format "edts-%s" edts-shell-next-shell-id))
        (command     (list edts-erl-command "-sname" node-name))
        (root        (expand-file-name default-directory)))
    (incf edts-shell-next-shell-id)
    (let ((buffer (edts-shell-make-comint-buffer buffer-name "." command)))
      (edts-register-node-when-ready node-name root nil)
      (when switch-to (switch-to-buffer buffer))
      buffer)))

(defun edts-shell-make-comint-buffer (buffer-name pwd command)
  "In a comint-mode buffer Starts a node with BUFFER-NAME by cd'ing to
PWD and running COMMAND."
  (let* ((cmd  (car command))
         (args (cdr command))
         (node-name (edts-shell-node-name-from-args args))
         (pwd  (expand-file-name pwd)))
    (with-current-buffer (get-buffer-create buffer-name) (cd pwd))
    (apply #'make-comint-in-buffer cmd buffer-name cmd nil args)
    (with-current-buffer buffer-name
      ;; generic stuff
      (when (fboundp 'show-paren-mode)
        (make-local-variable 'show-paren-mode)
        (show-paren-mode t))
      (linum-mode -1)

      ;; comint-variables
      (make-local-variable 'comint-output-filter-functions)
      (add-hook 'comint-output-filter-functions 'edts-shell-comint-filter)
      (make-local-variable 'comint-prompt-read-only)
      (setq comint-prompt-read-only t)

      ;; erlang-mode syntax highlighting
      (erlang-syntax-table-init)
      (erlang-font-lock-init)
      (setq font-lock-keywords-only nil)
      (set (make-local-variable 'syntax-propertize-function)
           'edts-shell-syntax-propertize-function)

      ;; edts-specifics
      (setq edts-buffer-node-name (edts-shell-node-name-from-args args))
      (edts-complete-setup edts-shell-ac-sources)
      (add-to-list
       'edts-shell-list `(,(buffer-name) . ((default-directory . ,pwd))))
      (make-local-variable 'kill-buffer-hook)
      (add-hook 'kill-buffer-hook #'edts-shell--kill-buffer-hook)))
    (set-process-query-on-exit-flag (get-buffer-process buffer-name) nil)
    (get-buffer buffer-name))

(defun edts-shell-syntax-propertize-function (start end)
  "Set text properties from START to END; find and set face for process
output and, if `edts-shell-inhibit-comint-input-highlight' is non-nil,
disable comint-highligt-input face for input."
  (edts-shell--set-output-field-face start end)
  (when edts-shell-inhibit-comint-input-highlight
    (edts-shell--disable-highlight-input start end)))

(defun edts-shell--set-output-field-face (start end)
  "Find output fields from START to END and set their font-lock-face to
`edts-shell-output-face."
  (let ((output-start nil))
    (loop for p from start to end do
          (if (eq (get-text-property p 'field) 'output)
              (when (not output-start) ;; entering output field
                (setq output-start p))
            (when output-start
              ;; just went outside of output field
              (edts-shell--set-output-face output-start p)
              (setq output-start nil))))
    (when output-start ;; in case last position was inside output
      (edts-shell--set-output-face output-start p))))

(defun edts-shell--set-output-face (start end)
  "Set the face to `edts-shell-output-face' from START to END."
  (put-text-property start p 'font-lock-face 'edts-shell-output-face))

(defun edts-shell--disable-highlight-input (start end)
  "Find and disable comint's input highlighting."
  (let ((input-start nil))
    (loop for p from start to end do
          (if (eq (get-text-property p 'font-lock-face) 'comint-highlight-input)
                (when (not input-start) ;; entering input field
                  (setq input-start p))
            (when input-start
              ;; just went outside of input field
              (remove-list-of-text-properties input-start p '(font-lock-face))
              (setq input-start nil))))))

(defun edts-shell--kill-buffer-hook ()
  "Removes the buffer from `edts-shell-list'."
  (setq edts-shell-list (assq-delete-all (buffer-name) edts-shell-list)))

(defun edts-shell-node-name-from-args (args)
  "Return node sname based on args"
  (block nil
    (while args
      (when (string= (car args) "-sname")
        (return (cadr args)))
      (pop args))))

(defun edts-shell-comint-filter (arg)
  "Make comint output read-only. Added to `comint-output-filter-functions'."
  (when (and arg (not (string= arg "")))
    (setq buffer-undo-list nil)
    (let* ((limit (+ (point) (length arg)))
           (output-end (save-excursion
                        (goto-char comint-last-output-start)
                        (if (re-search-forward edts-shell-prompt-regexp limit t)
                            (1- (match-beginning 0))
                          (point-max)))))
      (put-text-property
       comint-last-input-start comint-last-input-end 'read-only t)
      (add-text-properties
       comint-last-output-start output-end
       '(field output read-only t)))))

(defun edts-shell-find-by-path (path)
  "Return the buffer of the first found shell with PATH as its
default directory if it exists, otherwise nil."
  (block nil
    (let ((shells edts-shell-list))
      (while shells
        (when (string= path (cdr (assoc 'default-directory (cdar shells))))
          (return (get-buffer (caar shells))))
          (pop shells)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unit tests

(when (member 'ert features)
  (ert-deftest edts-shell-make-comint-buffer-test ()
    (let ((buffer (edts-shell-make-comint-buffer "edts-test" "." '("erl"))))
      (should (bufferp buffer))
      (should (string= "edts-test" (buffer-name buffer)))
      (should (string-match "erl\\(<[0-9]*>\\)?"
                            (process-name (get-buffer-process buffer))))
      (set-process-query-on-exit-flag (get-buffer-process buffer) nil)
      (kill-process (get-buffer-process buffer))
      (kill-buffer buffer))))
