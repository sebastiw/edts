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
;; Mode for listing breakpoints.

;; Window configuration to be restored when quitting debug mode

(require 'edts-api)
(require 'edts-debug)
(require 'edts-navigate)

(defconst edts-debug-list-breakpoint-buffer
  "*EDTS Breakpoints*"
  "Name of buffer where to display the list of breakpoints")

(define-derived-mode edts-debug-list-breakpoint-mode tabulated-list-mode
  ""
  "Mode for listing breakpoint modules."
  ;; Keybindings
  (define-key
    edts-debug-list-breakpoint-mode-map
    (kbd "RET")
    'edts-debug-list-breakpoint-find-breakpoint)
  (define-key
    edts-debug-list-breakpoint-mode-map
    (kbd "<delete>")
    'edts-debug-list-breakpoint-delete-breakpoint)
  (setq cursor-type nil)
  (hl-line-mode)
  (overlay-put hl-line-overlay
               'before-string
               (propertize " " 'display (list 'left-fringe
                                              'right-triangle
                                              'default)))
  (setq major-mode 'edts-debug-list-breakpoint-mode)
  (setq show-trailing-whitespace nil)
  (add-hook 'edts-debug-after-sync-hook 'edts-debug-list-breakpoint-update)
  (use-local-map edts-debug-list-breakpoint-mode-map))

(defun edts-debug-list-breakpoints (&optional show)
  "Show a listing of all breakpoint on all nodes registered
with EDTS. If optional argument SHOW is nil or omitted, don't display
process list buffer. If it is pop call `pop-to-buffer', if it is switch
call `switch-to-buffer'."
  (interactive '(pop))
  (with-current-buffer (get-buffer-create edts-debug-list-breakpoint-buffer)
    (edts-debug-list-breakpoint-mode)
    (edts-debug-list-breakpoint-update)
    (case show
      (pop    (pop-to-buffer    (current-buffer)))
      (switch (switch-to-buffer (current-buffer))))))

(defun edts-debug-list-breakpoint-find-breakpoint ()
  "Find breakpoint given by list entry under point."
  (interactive)
  (let* ((entry (tabulated-list-get-entry))
         (node (elt entry 0))
         (mod  (elt entry 1))
         (line (string-to-number (elt entry 2)))
         (file (cdr (assoc 'source (edts-api-get-module-info node mod 'basic)))))
    (edts-find-file-existing file)
    (goto-char (point-min))
    (forward-line (1- line))))

(defun edts-debug-list-breakpoint-delete-breakpoint ()
  "Uninterpret module given by list entry under point."
  (interactive)
  (let* ((entry (tabulated-list-get-entry))
         (node (elt entry 0))
         (mod  (elt entry 1))
         (line (string-to-number (elt entry 2))))
    (edts-debug-break node mod line nil)))

(defun edts-debug-list-breakpoint-update ()
  "Update the list of breakpoints and reintialize the header line."
  (when (buffer-live-p (get-buffer edts-debug-list-breakpoint-buffer))
    (with-current-buffer edts-debug-list-breakpoint-buffer
      (let ((max-node-len   4) ;; The length of the header names
            (max-module-len 6)
            entries)
        (flet (;; Sort an alist by comparing the keys as strings
               (key-sort (kvs)
                         (sort
                          (copy-sequence kvs)
                          #'(lambda (el1 el2)
                              (string< (car el1) (car el2)))))
               ;; Sort breakpoints by line numbers)
               (line-sort (breakpoints)
                          (sort
                           (copy-sequence breakpoints)
                           #'(lambda (b1 b2)
                               (< (cdr (assoc 'line b1))
                                  (cdr (assoc 'line b2)))))))
          (loop for (node . modules) in (key-sort edts-debug-breakpoint-alist)
                do (loop for (mod . breakpoints) in (key-sort modules)
                         do (loop for break in (line-sort breakpoints)
                                  for line      = (cdr (assoc 'line      break))
                                  for status    = (cdr (assoc 'status    break))
                                  for trigger   = (cdr (assoc 'trigger   break))
                                  for condition = (cdr (assoc 'condition break))
                                  do
                                  (push (list nil
                                              (vector node
                                                      mod
                                                      (number-to-string line)
                                                      status
                                                      trigger
                                                      condition)) entries)
                                  (setq max-module-len (max max-module-len
                                                            (length mod)))
                                  (setq max-node-len (max max-node-len
                                                          (length node))))))
          (setq tabulated-list-format
                (vector
                 `("Node"      ,max-node-len   'string< :pad-right 4)
                 `("Module"    ,max-module-len 'string< :pad-right 4)
                 '("Line"      8               nil      :pad-right 4)
                 '("Status"    8               nil      :pad-right 4)
                 '("Trigger"   10              nil      :pad-right 4)
                 '("Condition" 0               nil)))
          (tabulated-list-init-header)
          (setq tabulated-list-entries (reverse entries))
          (tabulated-list-print))))))

(defun edts-debug--get-module-source (node module)
  (cdr (assoc 'source (edts-api-get-module-info node module 'basic))))

(provide 'edts-debug-list-breakpoint-mode)
