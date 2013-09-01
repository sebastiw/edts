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
;; merchantability or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with EDTS. If not, see <http://www.gnu.org/licenses/>.
;;
;; Mode for listing processess.

;; Window configuration to be restored when quitting debug mode

(defconst edts-debug-list-processes-buffer
  "*EDTS Processes*"
  "Name of buffer where to display the list of processes")

(define-derived-mode edts-debug-list-processes-mode tabulated-list-mode
  "Mode for listing processes modules."
  ;; Keybindings
  (define-key
    edts-debug-list-processes-mode-map
    (kbd "RET")
    'edts-debug-list-processes-find-processes)
  (define-key
    edts-debug-list-processes-mode-map
    (kbd "<delete>")
    'edts-debug-list-processes-kill-process)
  (define-key
    edts-debug-list-processes-mode-map
    (kbd "c")
    'edts-debug-list-processes-continue)
  (setq major-mode 'edts-debug-list-processes-mode)
  (use-local-map edts-debug-list-processes-mode-map))

(defun edts-debug-list-processes ()
  "Show a listing of all processes on all nodes registered
with EDTS."
  (interactive)
  (with-current-buffer (get-buffer-create edts-debug-list-processes-buffer)
    (edts-debug-list-processes-mode)
    (edts-debug-list-processes-update)
    (display-buffer (current-buffer))))

(defun edts-debug-list-processes-find-processes ()
  "Find processes given by list entry under point."
  (interactive)
  (let* ((entry (tabulated-list-get-entry))
         (node (elt entry 0))
         (mod  (elt entry 1))
         (line (string-to-number (elt entry 2)))
         (file (cdr (assoc 'source (edts-get-module-info node mod 'basic)))))
    (edts-find-file-existing file)
    (goto-char (point-min))
    (forward-line (1- line))))

(defun edts-debug-list-processes-continue ()
  "Uninterpret module given by list entry under point."
  (interactive)
  (let* ((entry (tabulated-list-get-entry))
         (node (elt entry 0))
         (pid  (elt entry 1)))
    (edts-debug-process-continue node pid)))

(defun edts-debug-list-processes-update ()
  "Update the list of processes and reintialize the header line."
  (when (buffer-live-p (get-buffer edts-debug-list-processes-buffer))
    (with-current-buffer edts-debug-list-processes-buffer
      (let ((max-node-len   4) ;; The length of the header names
            (max-pid-len    3)
            (max-init-len   4)
            (max-status-len 6)
            (max-info-len   4)
            entries)
        (flet (;; Sort an alist by comparing the keys as strings
               (key-sort (kvs)
                         (sort
                          kvs
                          #'(lambda (el1 el2)
                                  (string< (car el1) (car el2))))))
          (loop for (node . procs) in (key-sort edts-debug-processes-alist)
                do (setq max-node-len (max max-node-len
                                           (length node)))
                do (loop for proc in procs
                         for pid    = (cdr (assoc 'pid    proc))
                         for init   = (cdr (assoc 'init   proc))
                         for status = (cdr (assoc 'status proc))
                         for info   = (cdr (assoc 'info   proc))
                         do
                         (push (list nil
                                     (vector node
                                             pid
                                             init
                                             status
                                             info))
                               entries)
                         (setq max-pid-len (max max-pid-len
                                                (length pid)))
                         (setq max-init-len (max max-init-len
                                                 (length init)))
                         (setq max-status-len (max max-status-len
                                                   (length status)))
                         (setq max-info-len (max max-info-len
                                                 (length info)))))
           (setq tabulated-list-format
                 (vector
                  `("Node"   ,max-node-len   'string< :pad-right 4)
                  `("Pid"    ,max-pid-len     nil     :pad-right 4)
                  `("Init"   ,max-init-len    nil     :pad-right 4)
                  `("Status" ,max-status-len  nil     :pad-right 4)
                  `("Info"   ,max-info-len    nil     :pad-right 4)))
           (message "%s" tabulated-list-format)
           (tabulated-list-init-header)
           (setq tabulated-list-entries (reverse entries))
           (tabulated-list-print))))))

(defun edts-debug--get-module-source (node module)
  (cdr (assoc 'source (edts-get-module-info node module 'basic))))


(provide 'edts-debug-list-processes-mode)
