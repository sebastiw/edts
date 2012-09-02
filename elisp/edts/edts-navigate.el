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
;; Code for navigating through a project.

(require 'thingatpt)

(defun edts-find-module ()
  "Find a module in the current project."
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
          (error "No module found")))))


(defun edts-find-local-function()
  "Find a function in the current module."
  (interactive)
  (let* ((functions (ferl-local-functions))
         (names     (mapcar #'(lambda (el) (car el)) functions))
         (choice    (ido-completing-read "Function: " names))
         (start     (cdr (assoc choice functions)))
         )
    (goto-char start)))

;; Borrowed from distel
(defun edts-find-source-under-point ()
  "Goto the source code that:  defines the function being called at point or
header file included at point. For remote calls, contacts an Erlang node to
determine which file to look in, with the following algorithm:

  Find the directory of the module's beam file (loading it if necessary).
  Look for the source file in:
    Directory where source file was originally compiled.
    Todo: Same directory as the beam file
    Todo: Again with /ebin/ replaced with /src/
    Todo: Again with /ebin/ replaced with /erl/

  Otherwise, report that the file can't be found."
  (interactive)
  (let ((type-under-point (edts-type-under-point)))
    (cond
     ;; look for a include/include_lib
     ((eq type-under-point 'header)
      (or (edts-find-header-source (edts-header-at-point))
          (error "No header at point.")))
     ;; look for a M:F/A
     ((eq type-under-point 'mfa)
      (apply #'edts-find-source
             (or (ferl-mfa-at-point) (error "No call at point.")))))))

(defun edts-type-under-point ()
  "Return which type of erlang thing we have under our pointer."
  (save-excursion
    (beginning-of-thing 'symbol)
    (cond
     ((looking-back "-include\\(_lib(\\|(\\)\".*") 'header)
     (t 'mfa))))

(defun edts-header-at-point ()
  "Return the filename for the header at point"
  (save-excursion (end-of-thing 'filename) (thing-at-point 'filename)))

(defun edts-find-header-source (headerfile)
  "Open the source for the header file"
  (let* ((mark (copy-marker (point-marker))) ;; Add us to the history list
         (module (erlang-get-module))
         (info (edts-get-detailed-module-info module))
         (includes (cdr (assoc 'includes info))) ;; Get all includes
         (file (find-if #'(lambda(x) (edts-has-suffix headerfile x))
                        includes)))
    (if file
        (progn (ring-insert-at-beginning (edts-window-find-history-ring) mark)
               (find-file-existing file)))
    ))

(defun edts-has-suffix (suffix string)
  "returns string if string has suffix"
  (let* ((suffix_len (length suffix))
         (string_len (length string))
         (base_len (- string_len suffix_len)))
    (if (eq t (compare-strings string base_len nil suffix nil nil)) string )))

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
            (null (error "Function %s:%s/%s not found" module function arity)))
        (let* ((node (edts-project-buffer-node-name))
               (info (edts-get-function-info node module function arity)))
          (if info
              (progn
                (find-file-existing (cdr (assoc 'source info)))
                (ring-insert-at-beginning (edts-window-find-history-ring) mark)
                (goto-line (cdr (assoc 'line   info))))
              (null
               (error "Function %s:%s/%s not found" module function arity)))))))

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
    (or (window-parameter window 'edts-find-history-ring)
        (set-window-parameter window 'edts-find-history-ring (make-ring 20)))))

(defun edts-who-calls ()
  (interactive)
  (let ((node (edts-project-buffer-node-name))
        (mfa  (ferl-mfa-at-point)))
    (if mfa
        (apply #'edts-find-callers (cons node mfa))
        (error "No call at point."))))

(defvar edts-found-caller-items nil
  "The callers found during the last call to edts-who-calls")

(defun edts-find-callers (node module function arity)
  "Jump to any all functions calling `module':`function'/`arity' in the
current buffer's project."
  (let* ((callers (edts-get-who-calls node module function arity))
         (caller-items (mapcar #'edts-function-popup-item callers)))
    (edts-do-find-callers caller-items)))

(defun edts-do-find-callers (caller-items)
  (if caller-items
      (progn
        (setq edts-found-caller-items caller-items)
        (let* ((choice       (popup-menu* caller-items))
               (module       (cdr (assoc 'module   choice)))
               (function     (cdr (assoc 'function choice)))
               (arity        (cdr (assoc 'arity    choice))))
          (setq edts-found-caller-items caller-items)
          (edts-find-source module function arity)))
      (error "No callers found")))

(defun edts-last-who-calls ()
  "Redo previous call to edts-who-calls"
  (interactive)
  (edts-do-find-callers edts-found-caller-items))

(defun edts-function-popup-item (item)
  "Formats an association list describing a function as a string"
  (let* ((module   (cdr (assoc 'module   item)))
         (function (cdr (assoc 'function item)))
         (arity    (cdr (assoc 'arity    item)))
         (str (format "%s:%s/%s" module function arity)))
    (popup-make-item str :value item)))


(provide 'edts-navigate)
