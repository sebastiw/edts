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

(defvar edts_debug-mode-pre-frame-configuration nil
  "The frame configuration before entering edts_debug-mode")

(defvar edts_debug-mode-buffer nil
  "The edts_debug-mode-buffer")

(defvar edts_debug-mode-variable-buffer nil
  "The edts_debug-mode-buffer")

(defvar edts_debug-mode-module nil
  "The module currently in the `edts_debug-mode-buffer'.")

(defvar edts_debug-mode-keymap (make-sparse-keymap))
(define-key edts_debug-mode-keymap (kbd "b") 'edts_debug-toggle-breakpoint)
(define-key edts_debug-mode-keymap (kbd "s") 'edts_debug-mode-step-into)
(define-key edts_debug-mode-keymap (kbd "o") 'edts_debug-mode-step-over)
(define-key edts_debug-mode-keymap (kbd "c") 'edts_debug-mode-continue)
(define-key edts_debug-mode-keymap (kbd "q") 'edts_debug-mode-quit)

(defun edts_debug-mode-attach ()
  (let* ((info  (edts_debug-process-info))
         (module (cdr (assoc 'module info)))
         (line   (cdr (assoc 'line info))))
    (unless (and edts_debug-mode-buffer (buffer-live-p edts_debug-mode-buffer))
      (setq edts_debug-mode-buffer (get-buffer-create "EDTS Debug")))
    (edts_debug-mode-update-buffer-info)
    (setq edts_debug-mode-pre-frame-configuration (current-frame-configuration))
    (switch-to-buffer edts_debug-mode-buffer)
    (delete-other-windows)

    ;; Set up variable bindings buffer
    (let* ((margin-size (- (window-total-width) (window-width)))
           (body-size   (* 2 (/ (window-width) 5)))
           (window-size (+ body-size margin-size)))
      (split-window nil window-size 'left)
      (edts_debug-mode-list-variable-bindings))

    ;; Move back to the primary debugger window
    (select-window (frame-first-window))))

(defun edts_debug-mode-list-variable-bindings ()
  (switch-to-buffer (get-buffer-create "EDTS Debug Variable Bindings"))
  (setq edts_debug-mode-variable-buffer (current-buffer))
  (setq show-trailing-whitespace nil)
  (edts_debug-mode-update-variable-bindings))

(defun edts_debug-mode-update-variable-bindings ()
  (let ((buf (get-buffer "EDTS Debug Variable Bindings")))
    (when buf
      (with-current-buffer buf
        (erase-buffer)
        (let* ((max-var-len 8) ;; The length of the header name
               (bindings (edts_debug-process-info edts_debug-node
                                                  edts_debug-pid
                                                  'bindings))
               (var-alist (sort (copy-sequence bindings)
                                #'(lambda (el1 el2) (string< (car el1)
                                                             (car el2)))))
               entries)
          (loop for (var . binding) in var-alist
                for var-name = (symbol-name var)
                do (setq max-var-len (max max-var-len (length var-name)))
                do (push (list nil (vector var-name binding)) entries))
          (setq tabulated-list-format
                (vector
                 `("Variable" ,max-var-len 'string< :pad-right 4)
                 '("Binding"  0 'string<)))
          (tabulated-list-init-header)
          (setq tabulated-list-entries (reverse entries))
          (tabulated-list-print))))))

(defun edts_debug-mode-quit ()
  (interactive)
  (edts_debug-detach)
  (setq edts_debug-mode-module nil)
  (when (and edts_debug-mode-buffer (buffer-live-p edts_debug-mode-buffer))
    (kill-buffer edts_debug-mode-buffer)
    (setq edts_debug-mode-buffer nil))
  (when (and edts_debug-mode-variable-buffer
             (buffer-live-p edts_debug-mode-variable-buffer))
    (kill-buffer edts_debug-mode-variable-buffer)
    (setq edts_debug-mode-buffer nil))
  (when edts_debug-mode-pre-frame-configuration
    (set-frame-configuration edts_debug-mode-pre-frame-configuration)
    (setq edts_debug-mode-pre-frame-configuration nil)))

(defun edts_debug-mode-update-buffer-info ()
  "Update buffer info (overlays, mode-line etc."
  (let* ((info   (edts_debug-process-info))
         (status (cdr (assoc 'status info)))
         (mod    (cdr (assoc 'module info)))
         (line   (cdr (assoc 'line   info))))
    (when (and edts_debug-mode-buffer
               (buffer-live-p edts_debug-mode-buffer))
      (when (and (equal status "break")
                 (not (equal mod edts_debug-mode-module)))
        (edts_debug-mode-find-module mod line))
      (with-current-buffer edts_debug-mode-buffer
        (edts_debug-update-buffer-breakpoints edts_debug-node
                                              edts_debug-mode-module)
        (edts_debug-update-buffer-process-location edts_debug-mode-module
                                                   line))
      (edts_debug-mode-update-variable-bindings))))
(add-hook 'edts_debug-after-sync-hook 'edts_debug-mode-update-buffer-info)

(defun edts_debug-mode-kill-buffer-hook ()
  "Hook to run just before the edts_debug-mode buffer is killed."
  (setq edts_debug-mode-buffer nil))

(define-derived-mode edts_debug-mode erlang-mode
  "EDTS debug mode"
  "Major mode for debugging interpreted Erlang code."
  (setq buffer-read-only t)
  (setq mode-name "EDTS-debug")
  (use-local-map edts_debug-mode-keymap)
  (hl-line-mode)
  (add-hook 'kill-buffer-hook 'edts_debug-mode-kill-buffer-hook nil t))

(defun edts_debug-mode-find-module (module line)
  (let* ((module-info (edts-get-module-info edts_debug-node module 'basic))
         (file        (cdr (assoc 'source module-info)))
         (buffer-name        (edts_debug-mode-file-buffer-name file)))
    ;; Make sure buffer is created and has the right name.
    (unless (and edts_debug-mode-buffer (buffer-live-p edts_debug-mode-buffer))
      (setq edts_debug-mode-buffer (get-buffer-create buffer-name)))
    (switch-to-buffer edts_debug-mode-buffer)
    (edts_debug-mode)
    (unless (equal buffer-name (buffer-name))
      (rename-buffer buffer-name))

    ;; Modeline
    (add-to-list 'mode-line-buffer-identification
               '(:eval (edts_debug-mode-mode-line-info))
               t)

    ;; Setup a bunch of variables
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert-file-contents file))
    (setq edts_debug-mode-module module)
    (setq buffer-file-name file)
    (set-buffer-modified-p nil)
    (setq edts-node-name edts_debug-node)
    (ferl-goto-line line)
    (back-to-indentation)))

(defun edts_debug-mode-mode-line-info ()
  (format " Pid: %s (%s)"
          edts_debug-pid
          (edts_debug-process-info edts_debug-node edts_debug-pid 'status)))

(defun edts_debug-mode-file-buffer-name (file)
  (concat "*" (path-util-base-name file) " Debug*"))

(defun edts_debug-mode-continue ()
  "Send a continue-command to the debugged process with `edts_debug-pid'
on `edts_debug-node'."
  (interactive)
  (edts_debug-mode--command 'continue))

(defun edts_debug-mode-finish ()
  "Send a finish-command to the debugged process with `edts_debug-pid'
on `edts_debug-node'."
  (interactive)
  (edts_debug-mode--command 'finish))

(defun edts_debug-mode-step-into ()
  "Send a step-into command to the debugged process with `edts_debug-pid'
on `edts_debug-node'."
  (interactive)
  (edts_debug-mode--command 'step_into))

(defun edts_debug-mode-step-over ()
  "Send a step-over command to the debugged process with `edts_debug-pid'
on `edts_debug-node'."
  (interactive)
  (edts_debug-mode--command 'step_over))

(defun edts_debug-mode--command (cmd)
  "Send a step-over command to the debugged process with `edts_debug-pid'
on `edts_debug-node'."
  (interactive)
  (edts_debug-command edts_debug-node edts_debug-pid cmd))

(provide 'edts_debug-mode)
