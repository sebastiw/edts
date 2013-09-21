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

(defvar edts_debug-mode-module nil
  "The edts_debug buffer's module.")
(make-variable-buffer-local 'edts-debug-mode-module)

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
    (setq edts_debug-mode-pre-frame-configuration (current-frame-configuration))
    (delete-other-windows)))

(defun edts_debug-mode-quit ()
  (interactive)
  (dolist (buf (buffer-list))
    (when (eq (buffer-local-value 'major-mode buf) 'edts_debug-mode)
      (kill-buffer buf)))
  (set-frame-configuration edts_debug-mode-pre-frame-configuration))


(defun edts_debug-mode-find-module (module &optional line)
  (let* ((module-info (edts-get-module-info edts_debug-node module 'basic))
         (file        (cdr (assoc 'source module-info)))
         (buffer-name (edts_debug-mode-file-buffer-name file)))
    (with-current-buffer (get-buffer-create buffer-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert-file-contents file))
      (setq buffer-file-name file)
      (set-buffer-modified-p nil)
      (setq edts-node-name edts_debug-node)
      (setq edts_debug-mode-module module)
      (edts_debug-mode)
      (edts_debug-mode-update-buffer-info))
  (switch-to-buffer buffer-name)
  (when line
    (ferl-goto-line line)
    (back-to-indentation))))

(defun edts_debug-mode-file-buffer-name (file)
  (concat "*" (path-util-base-name file) " Debug*"))

(define-derived-mode edts_debug-mode erlang-mode
  "EDTS debug mode"
  "Major mode for debugging interpreted Erlang code."
  (setq buffer-read-only t)
  (setq mode-name "EDTS-debug")
  (use-local-map edts_debug-mode-keymap))

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

(defun edts_debug-mode-update-all-buffers ()
  (dolist (buf (buffer-list))
    (when (eq (buffer-local-value 'major-mode buf) 'edts_debug-mode)
    (with-current-buffer buf
      (edts_debug-mode-update-buffer-info)))))

(defun edts_debug-mode-update-buffer-info ()
  "Update buffer info (overlays, mode-line etc."
  (edts_debug-update-buffer-breakpoints edts_debug-node edts_debug-mode-module)
  (edts_debug-update-buffer-process-location edts_debug-mode-module))

(provide 'edts_debug-mode)
