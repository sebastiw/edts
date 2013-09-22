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
    (edts_debug-mode-find-module module line)
    (edts_debug-mode-update-buffer-info)
    (setq edts_debug-mode-pre-frame-configuration (current-frame-configuration))
    (delete-other-windows)))

(defun edts_debug-mode-quit ()
  (interactive)
  (when (and edts_debug-mode-buffer (buffer-live-p edts_debug-mode-buffer))
    (kill-buffer edts_debug-mode-buffer))
  (when edts_debug-mode-pre-frame-configuration
    (set-frame-configuration edts_debug-mode-pre-frame-configuration)
    (setq edts_debug-mode-pre-frame-configuration nil)))

(defun edts_debug-mode-update-buffer-info ()
  "Update buffer info (overlays, mode-line etc."
  (let* ((info   (edts_debug-process-info))
         (status (cdr (assoc 'status info)))
         (mod    (cdr (assoc 'module info)))
         (line   (cdr (assoc 'line   info))))
    (when (and edts_debug-mode-buffer (buffer-live-p edts_debug-mode-buffer))
      (when (and (equal status "break") (equal mod edts_debug-mode-module))
        (edts_debug-mode-find-module mod line))
      (with-current-buffer edts_debug-mode-buffer
        (edts_debug-update-buffer-breakpoints edts_debug-node mod)
        (edts_debug-update-buffer-process-location mod line)))))
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
  (setq cursor-type nil)
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
