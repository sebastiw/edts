;; Copyright 2013 Thomas Järvstrand <tjarvstrand@gmail.com>
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
;; EDTS Debug mode

(require 'f)

(require 'edts)
(require 'edts-api)
(require 'edts-debug)
(require 'edts-debug-list-breakpoint-mode)
(require 'edts-debug-list-interpreted-mode)
(require 'edts-debug-list-processes-mode)

(defvar edts-debug-mode-pre-frame-configuration nil
  "The frame configuration before entering edts-debug-mode")

(defvar edts-debug-mode-buffer nil
  "The edts-debug-mode-buffer")

(defvar edts-debug-mode-variable-buffer nil
  "The edts-debug-mode-buffer")

(defvar edts-debug-mode-module nil
  "The module currently in the `edts-debug-mode-buffer'.")

(defvar edts-debug-mode-keymap (make-sparse-keymap))
(define-key edts-debug-mode-keymap (kbd "b") 'edts-debug-toggle-breakpoint)
(define-key edts-debug-mode-keymap (kbd "f") 'edts-debug-mode-finish)
(define-key edts-debug-mode-keymap (kbd "s") 'edts-debug-mode-step-into)
(define-key edts-debug-mode-keymap (kbd "o") 'edts-debug-mode-step-over)
(define-key edts-debug-mode-keymap (kbd "c") 'edts-debug-mode-continue)
(define-key edts-debug-mode-keymap (kbd "q") 'edts-debug-mode-quit)

(defun edts-debug-mode-attach ()
  (let* ((info  (edts-debug-process-info))
         (module (cdr (assoc 'module info)))
         (line   (cdr (assoc 'line info))))
    (unless (and edts-debug-mode-buffer (buffer-live-p edts-debug-mode-buffer))
      (setq edts-debug-mode-buffer (get-buffer-create "EDTS Debug")))
    (edts-debug-mode-update-buffer-info)
    (setq edts-debug-mode-pre-frame-configuration (current-frame-configuration))
    (switch-to-buffer edts-debug-mode-buffer)
    (delete-other-windows)

    (let* ((width  (- (frame-width) (round (* (frame-width) 0.4))))
           (height (/ (frame-height) 3)))
      ;; Set up variable bindings window
      (select-window (split-window nil width t))
      (edts-debug-mode-list-variable-bindings)

      ;; Set up breakpoint list window
      (select-window (split-window nil height))
      (edts-debug-list-breakpoints 'switch)

      ;; Set up process list window
      (select-window (split-window nil height))
      (edts-debug-list-processes 'switch)

      ;; Move back to the primary debugger window
      (select-window (frame-first-window)))))
(setq edts-debug-attach-function 'edts-debug-mode-attach)

(defun edts-debug-mode-list-variable-bindings ()
  (switch-to-buffer (get-buffer-create "EDTS Debug Variable Bindings"))
  (setq edts-debug-mode-variable-buffer (current-buffer))
  (setq show-trailing-whitespace nil)
  (edts-debug-mode-update-variable-bindings)
  (tabulated-list-mode)
  (setq truncate-partial-width-windows nil)
  (use-local-map edts-debug-mode-keymap))



(defun edts-debug-mode-update-variable-bindings ()
  (let ((buf (get-buffer "EDTS Debug Variable Bindings")))
    (when buf
      (with-current-buffer buf
        (let* ((inhibit-read-only t)
               ;; Figure out indentation
               (bound (edts-debug-get-bound-variables edts-debug-node
                                                      edts-debug-pid))
               (max-var-len (apply #'max
                                   8 ;; The length of the header name
                                   (loop for v in bound collect (length v))))
               (col-pad 4)
               (max-col (window-body-width (get-buffer-window buf)))
               (indent  (+ max-var-len col-pad))
               (bindings (edts-debug-get-bindings-pretty edts-debug-node
                                                         edts-debug-pid
                                                         indent
                                                         max-col))
               entries)
          (erase-buffer)
          (loop for (var . bind) in bindings
                for var-name = (propertize (symbol-name var)
                                           'face
                                           'font-lock-variable-name-face)
                do (push (list nil (vector var-name bind)) entries))
          (setq tabulated-list-format
                (vector
                 `("Variable" ,max-var-len 'string< :pad-right ,col-pad)
                 '("Binding"  0 'string<)))
          (tabulated-list-init-header)
          (setq tabulated-list-entries (reverse entries))
          (tabulated-list-print))))))

(defun edts-debug-mode-quit ()
  (interactive)
  (edts-debug-detach)
  (setq edts-debug-mode-module nil)
  (when (and edts-debug-mode-buffer (buffer-live-p edts-debug-mode-buffer))
    (kill-buffer edts-debug-mode-buffer)
    (setq edts-debug-mode-buffer nil))
  (when (and edts-debug-mode-variable-buffer
             (buffer-live-p edts-debug-mode-variable-buffer))
    (kill-buffer edts-debug-mode-variable-buffer)
    (setq edts-debug-mode-buffer nil))
  (when edts-debug-mode-pre-frame-configuration
    (set-frame-configuration edts-debug-mode-pre-frame-configuration)
    (setq edts-debug-mode-pre-frame-configuration nil)))

(defun edts-debug-mode-update-buffer-info ()
  "Update buffer info (overlays, mode-line etc."
  (let* ((info   (edts-debug-process-info))
         (status (cdr (assoc 'status info)))
         (mod    (cdr (assoc 'module info)))
         (line   (cdr (assoc 'line   info))))
    (when (and edts-debug-mode-buffer
               (buffer-live-p edts-debug-mode-buffer))
      (when (and (equal status "break")
                 (not (equal mod edts-debug-mode-module)))
        (edts-debug-mode-find-module mod line))
      (with-current-buffer edts-debug-mode-buffer
        (edts-debug-update-buffer-breakpoints edts-debug-node
                                              edts-debug-mode-module)
        (edts-debug-update-buffer-process-location edts-debug-mode-module
                                                   line)))))
(add-hook 'edts-debug-after-sync-hook 'edts-debug-mode-update-buffer-info)
(add-hook 'edts-debug-after-sync-hook 'edts-debug-mode-update-variable-bindings)

(defun edts-debug-mode-kill-buffer-hook ()
  "Hook to run just before the edts-debug-mode buffer is killed."
  (setq edts-debug-mode-buffer nil))

(define-derived-mode edts-debug-mode erlang-mode
  "EDTS debug mode"
  "Major mode for debugging interpreted Erlang code."
  (setq buffer-read-only t)
  (setq mode-name "EDTS-debug")
  (use-local-map edts-debug-mode-keymap)
  (hl-line-mode)
  (add-hook 'kill-buffer-hook 'edts-debug-mode-kill-buffer-hook nil t))

(defun edts-debug-mode-find-module (module line)
  (let* ((module-info (edts-api-get-module-info edts-debug-node module 'basic))
         (file        (cdr (assoc 'source module-info)))
         (buffer-name        (edts-debug-mode-file-buffer-name file)))
    ;; Make sure buffer is created and has the right name.
    (unless (and edts-debug-mode-buffer (buffer-live-p edts-debug-mode-buffer))
      (setq edts-debug-mode-buffer (get-buffer-create buffer-name)))
    (switch-to-buffer edts-debug-mode-buffer)
    (edts-debug-mode)
    (unless (equal buffer-name (buffer-name))
      (rename-buffer buffer-name))

    ;; Modeline
    (add-to-list 'mode-line-buffer-identification
               '(:eval (edts-debug-mode-mode-line-info))
               t)

    ;; Setup a bunch of variables
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert-file-contents file))
    (setq edts-debug-mode-module module)
    (setq buffer-file-name file)
    (set-buffer-modified-p nil)
    (setq edts-api-node-name edts-debug-node)
    (ferl-goto-line line)
    (back-to-indentation)))

(defun edts-debug-mode-mode-line-info ()
  (format " Pid: %s (%s)"
          edts-debug-pid
          (edts-debug-process-info edts-debug-node edts-debug-pid 'status)))

(defun edts-debug-mode-file-buffer-name (file)
  (concat "*" (f-filename file) " Debug*"))

(defun edts-debug-mode-continue ()
  "Send a continue-command to the debugged process with `edts-debug-pid'
on `edts-debug-node'."
  (interactive)
  (edts-debug-mode--command 'continue))

(defun edts-debug-mode-finish ()
  "Send a finish-command to the debugged process with `edts-debug-pid'
on `edts-debug-node'."
  (interactive)
  (edts-debug-mode--command 'finish))

(defun edts-debug-mode-step-into ()
  "Send a step-into command to the debugged process with `edts-debug-pid'
on `edts-debug-node'."
  (interactive)
  (edts-debug-mode--command 'step_into))

(defun edts-debug-mode-step-over ()
  "Send a step-over command to the debugged process with `edts-debug-pid'
on `edts-debug-node'."
  (interactive)
  (edts-debug-mode--command 'step_over))

(defun edts-debug-mode--command (cmd)
  "Send a step-over command to the debugged process with `edts-debug-pid'
on `edts-debug-node'."
  (interactive)
  (edts-debug-command edts-debug-node edts-debug-pid cmd))

(defun edts-debug-mode-node-down-hook (node)
  "Node-down hook for edts-debug."
  (when (string= node edts-debug-node)
    (edts-debug-mode-quit)))
(add-hook 'edts-api-node-down-hook 'edts-debug-mode-node-down-hook)

(defun edts-debug-mode-server-down-hook ()
  "Server-down hook for edts-debug-mode."
  (edts-debug-mode-quit))
(add-hook 'edts-api-server-down-hook 'edts-debug-mode-server-down-hook)

(provide 'edts-debug-mode)
