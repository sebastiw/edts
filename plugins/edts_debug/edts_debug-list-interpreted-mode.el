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
;; Mode for listing interpreted modules.

;; Window configuration to be restored when quitting debug mode

(defconst edts_debug-list-interpreted-buffer
  "*EDTS Interpreted Modules*"
  "Name of buffer where to display the list of interpreted modules")

(define-derived-mode edts_debug-list-interpreted-mode tabulated-list-mode
  "Mode for listing interpreted modules."
  ;; Keybindings
  (define-key
    edts_debug-list-interpreted-mode-map
    (kbd "RET")
    'edts_debug-list-interpreted-find-module)
  (define-key
    edts_debug-list-interpreted-mode-map
    (kbd "<delete>")
    'edts_debug-list-interpreted-uninterpret-module)
  (setq cursor-type nil)
  (hl-line-mode)
  (setq show-trailing-whitespace nil)
  (add-hook 'edts_debug-after-sync-hook 'edts_debug-list-interpreted-update)
  (setq major-mode 'edts_debug-list-interpreted-mode)
  (use-local-map edts_debug-list-interpreted-mode-map))

(defun edts_debug-list-interpreted ()
  "Show a listing of all interpreted modules on all nodes registered
with EDTS."
  (interactive)
  (with-current-buffer (get-buffer-create edts_debug-list-interpreted-buffer)
    (edts_debug-list-interpreted-mode)
    (edts_debug-list-interpreted-update)
    (switch-to-buffer (current-buffer))))

(defun edts_debug-list-interpreted-find-module ()
  "Find module given by list entry under point."
  (interactive)
  (let* ((entry (tabulated-list-get-entry))
         (node (elt entry 0))
         (mod  (elt entry 1))
         (file (cdr (assoc 'source (edts-get-module-info node mod 'basic)))))
    (edts-find-file-existing file)))

(defun edts_debug-list-interpreted-uninterpret-module ()
  "Uninterpret module given by list entry under point."
  (interactive)
  (let ((entry (tabulated-list-get-entry)))
    (edts_debug-interpret (elt entry 0) (elt entry 1))))

(defun edts_debug-list-interpreted-update ()
  "Update the list of interpreted modules and reintialize the header line."
  (when (buffer-live-p (get-buffer edts_debug-list-interpreted-buffer))
    (with-current-buffer edts_debug-list-interpreted-buffer
      (let ((max-node-len 4) ;; The length of the header name
            (int-alist (sort edts_debug-interpreted-alist
                             #'(lambda (el1 el2) (string< (car el1)
                                                          (car el2)))))
            entries)
        (loop for (node . mods) in int-alist
              when mods
              do (loop for mod in (sort (copy-sequence mods) 'string<)
                       do (setq max-node-len (max max-node-len (length node)))
                       do (push (list nil (vector node mod)) entries)))
        (setq tabulated-list-format
              (vector
               `("Node"   ,max-node-len 'string< :pad-right 4)
               '("Module" 0 'string<)))
        (tabulated-list-init-header)
        (setq tabulated-list-entries (reverse entries))
        (tabulated-list-print)))))

(provide 'edts_debug-list-interpreted-mode)
