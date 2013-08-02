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
;; Debugger interaction code for EDTS

;; Window configuration to be restored when quitting debug mode

(defconst edts-debug-list-interpreted-buffer
  "*EDTS Interpreted Modules*"
  "Name of buffer where to display the list of interpreted modules")

(defconst edts-debug-list-interpreted-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET")
      'edts-debug-list-interpreted-find-module)
    (define-key map (kbd "<delete>")
      'edts-debug-list-interpreted-uninterpret-module)
    map))

(define-derived-mode edts-debug-list-interpreted-mode tabulated-list-mode
  "Mode for listing interpreted modules."
  (setq major-mode 'edts-debug-list-mode)
  (use-local-map edts-debug-list-interpreted-mode-map))

(defun edts-debug-list-interpreted ()
  "Show a listing of all interpreted modules on all nodes registered
with EDTS."
  (interactive)
  (switch-to-buffer (get-buffer-create edts-debug-interpreted-list-buffer))
  (edts-debug-list-interpreted-mode)
  (edts-debug-update-interpreted-list))

(defun edts-debug-list-interpreted-find-module ()
  "Find module given by list entry under point."
  (interactive)
  (find-file-existing (elt (tabulated-list-get-entry) 2)))

(defun edts-debug-list-interpreted-uninterpret-module ()
  "Uninterpret module given by list entry under point."
  (interactive)
  (let ((entry (tabulated-list-get-entry)))
    (edts-debug-interpret (elt entry 1) (elt entry 0))))

(defun edts-debug-update-interpreted-list ()
  "Fetch and return a list of all interpreted modules on all nodes. Also
reintializes the header line."
  (let ((max-module-len 6) ;; The length of the header names
        (max-node-len   4)
        entries)
    (loop for node in (sort (edts-get-nodes) 'string<)
          do (loop for mod in (sort (edts-debug-interpreted-modules node)
                                    'string<)
                   for file = (edts-debug--get-module-source node mod)
                   do
                   (push (list nil (vector mod node file)) entries)
                   (setq max-module-len (max max-module-len (length mod)))
                   (setq max-node-len (max max-node-len (length node)))))
    (setq tabulated-list-format
        (vector
         `("Module" ,max-module-len 'string<)
         `("Node"   ,max-node-len   'string<)
         '("File"   0               'string<)))
    (tabulated-list-init-header)
    (setq tabulated-list-entries (reverse entries))
    (tabulated-list-print)))

(provide 'edts-debug-list-interpreted-mode)
