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

(defvar edts_debug-module-buffers nil
  "List of debugger buffers visiting modules")

(defvar edts_debug-mode-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "b") 'edts_debug-toggle-breakpoint)
    (define-key map (kbd "s") 'edts_debug-step)
    (define-key map (kbd "o") 'edts_debug-step-out)
    (define-key map (kbd "c") 'edts_debug-continue)
    (define-key map (kbd "q") 'edts_debug-mode-quit)
    map))

(defvar edts_debug-mode nil
  "non-nil if edts_debug-mode is currently active in any buffers")

(defun edts_debug-mode-attach ()
  (let* ((info  (edts_debug-process-info))
        (module (cdr (assoc 'module info)))
        (line   (cdr (assoc 'line info))))
    (edts_debug-mode-find-module module)
  (unless edts_debug-mode
    (setq edts_debug-mode t)
    (setq edts_debug-mode-pre-frame-configuration (current-frame-configuration))
    (delete-other-windows))))

(defun edts_debug-mode-quit ()
  (interactive)
  (dolist (buf edts_debug-module-buffers)
    (kill-buffer buf))
  (setq edts_debug-mode nil)
  (set-frame-configuration edts_debug-mode-pre-frame-configuration))


(defun edts_debug-mode-find-module (module)
  (let* ((module-info (edts-get-module-info edts_debug-node module 'basic))
         (file        (cdr (assoc 'source module-info)))
         (buffer-name (edts_debug-mode-file-buffer-name file)))
    (unless (get-buffer buffer-name)
      (with-current-buffer (get-buffer-create buffer-name)
        (insert-file-contents file)
        (setq buffer-file-name file)
        (set-buffer-modified-p nil)
        (setq edts-node-name edts_debug-node)
        (edts_debug-mode)
        (add-to-list 'edts_debug-module-buffers (current-buffer))
        (switch-to-buffer (current-buffer))
        (edts_debug-update-buffer-breakpoints edts_debug-node module)
        (edts_debug-update-buffer-process-location module)))))

(defun edts_debug-mode-file-buffer-name (file)
  (concat "*" (path-util-base-name file) " Debug*"))

(define-derived-mode edts_debug-mode erlang-mode
  "EDTS debug mode"
  "Major mode for debugging interpreted Erlang code."
  (setq buffer-read-only t)
  (setq mode-name "EDTS-debug")
  (use-local-map edts_debug-mode-keymap))

(provide 'edts_debug-mode)
