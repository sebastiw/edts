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
;; Rudimentary project support for edts so that we can relate buffers to
;; projects and communicate with the correct nodes.
;;
;; Code for jumping around between modules etc.

(defun edts-find-module()
  (interactive)
  (let ((modules (edts-get-modules)))
    (if modules
        (let* ((choice (if ido-mode
                           (ido-completing-read "Module: " modules)
                           (completing-read     "Module:"  modules)))
               (file (cdr (assoc 'source (edts-get-basic-module-info choice))))
               (mark (copy-marker (point-marker))))
          (find-file-existing file) ; Fixme, catch error
          (ring-insert-at-beginning (edts-window-find-history-ring) mark)
          (message "No modules found")))))


(defun edts-find-local-function()
  (interactive)
  (let* ((functions (ferl-local-functions))
         (names     (mapcar #'(lambda (el) (car el)) functions))
         (choice    (ido-completing-read "Function: " names))
         (start     (cdr (assoc choice functions)))
         )
    (goto-char start)))

;; Borrowed from distel
(defun edts-find-source-under-point ()
  "Goto the source code that defines the function being called at point.
For remote calls, contacts an Erlang node to determine which file to
look in, with the following algorithm:

  Find the directory of the module's beam file (loading it if necessary).
  Look for the source file in:
    Directory where source file was originally compiled.
    Todo: Same directory as the beam file
    Todo: Again with /ebin/ replaced with /src/
    Todo: Again with /ebin/ replaced with /erl/

  Otherwise, report that the file can't be found."
  (interactive)
  (apply #'edts-find-source
         (or (ferl-mfa-at-point) (error "No call at point."))))

(defun edts-find-source (module function arity)
  "Find the source code for MODULE in a buffer, loading it if necessary.
When FUNCTION is specified, the point is moved to its start."
  ;; Add us to the history list
  (let ((mark (copy-marker (point-marker))))
    (if (or (equal module (erlang-get-module))
            (string-equal module "MODULE"))
        (if function
            (progn
              (ring-insert-at-beginning (edts-window-find-history-ring) mark)
              (ferl-search-function function arity))
            (null (error "Function %s/s not found")))
        (let* ((node (edts-project-buffer-node-name))
               (info (edts-get-function-info node module function arity)))
          (if info
              (progn
                (find-file-existing (cdr (assoc 'source info)))
                (ring-insert-at-beginning (edts-window-find-history-ring) mark)
                (goto-line (cdr (assoc 'line   info))))
              (null (error "Function %s/s not found")))))))

;; Borrowed from distel
(defun edts-find-source-unwind ()
  "Unwind back from uses of `edts-find-source-under-point'."
  (interactive)
  (let ((ring (edts-window-find-history-ring)))
    (unless (ring-empty-p ring)
      (let* ((marker (ring-remove ring))
             (buffer (marker-buffer marker)))
        (if (buffer-live-p buffer)
            (progn (switch-to-buffer buffer)
                   (goto-char (marker-position marker)))
          ;; If this buffer was deleted, recurse to try the next one
          (edts-find-source-unwind))))))

(defun edts-window-find-history-ring ()
  (let ((window (selected-window)))
    (or (window-parameter window edts-find-history-ring)
        (set-window-parameter window edts-find-history-ring (make-ring 20)))))


(provide 'edts-navigate)