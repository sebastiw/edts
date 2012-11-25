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
buffers.")

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
      (make-local-variable 'comint-prompt-regexp)
      (setq comint-prompt-regexp edts-shell-prompt-regexp)
      (setq comint-use-prompt-regexp t)

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
  "Set the syntax table property of all output from START to END."
  (let ((output-start nil))
    (loop for p from start to end do
          (if (eq (get-text-property p 'field) 'output)
              (when (not output-start)
                (setq output-start p))
            (when output-start
              (put-text-property output-start p 'syntax-table '(2))
              (setq output-start nil))))
    (when output-start
      (put-text-property output-start p 'syntax-table '(2)))))

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
          (return (get-buffer (caar shells)))
          (pop shells))))))

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
