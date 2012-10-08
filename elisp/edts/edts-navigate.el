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
(require 'cl)
(require 'ring)
(require 'thingatpt)

(require 'ferl)

;; Nameclash (sort of) with edts-find source. One of the names should be
;; changed to indicate the differences in how they work.
(defun edts-find-function ()
  "Find a module in the current project."
  (interactive)
  (let ((modules (edts-get-modules)))
    (if modules
        (let* ((choice (edts-query "Module: " modules))
               (file (cdr (assoc 'source (edts-get-basic-module-info choice))))
               (mark (copy-marker (point-marker))))
          (find-file-existing file) ; Fixme, catch error
          (ring-insert-at-beginning (edts-window-history-ring) mark)
          (edts-find-local-function))
        (error "No module found"))))

(defun edts-find-local-function()
  "Find a function in the current module."
  (interactive)
  (let* ((functions (ferl-local-functions))
         (names     (mapcar #'(lambda (el) (car el)) functions))
         (choice    (edts-query "Function: " (cons "-Top of Module-" names))))
    (if (string= "-Top of Module-" choice)
        (goto-char 0)
        (goto-char (cdr (assoc choice functions))))))

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
  (cond
   ;; look for a include/include_lib
   ((edts-header-under-point-p) (edts-find-header-source))
   ((edts-macro-under-point-p)  (edts-find-macro-source))
   ((edts-record-under-point-p) (edts-find-record-source))
   ;; look for a M:F/A
   ((apply #'edts-find-source
           (or (ferl-mfa-at-point) (error "No call at point."))))))

(defun edts-header-under-point-p ()
  "Return non nil if the form under point is an include or include_lib
directive."
  (save-excursion
    (beginning-of-line)
    (looking-at "-include\\(_lib\\)\\s-*(\\s-*.*")))

(defun edts-macro-under-point-p ()
  "Return non nil if the form under point is a macro."
  (save-excursion
    (beginning-of-thing 'symbol)
    (equal ?? (char-before (point)))))

(defun edts-record-under-point-p ()
  "Return non-nil if the form under point is a record"
    (save-excursion
    (beginning-of-thing 'symbol)
    (equal ?# (char-before (point)))))

(defun edts-header-at-point ()
  "Return the filename for the header under point."
  (let ((bound (ferl-last-char-on-line-at (point))))
    (save-excursion
      (beginning-of-line)
      (when (re-search-forward "-include\\(_lib\\)\\s-*(\\s-*\"\\s-*" bound t)
        (thing-at-point 'filename)))))

(defun edts-find-header-source ()
  "Open the source for the header file under point."
  (let* ((headerfile (edts-header-at-point))
         (mark (copy-marker (point-marker))) ;; Add us to the history list
         (includes (edts-get-includes))
         (file (find-if #'(lambda(x) (edts-has-suffix headerfile x))
                        includes)))
    (if file
        (progn (ring-insert-at-beginning (edts-window-history-ring) mark)
               (find-file-existing file)
               (goto-char (point-min)))
        (null (error "No header filename at point")))))

(defun edts-has-suffix (suffix string)
  "returns string if string has suffix"
  (string= (substring string (- 0 (length suffix))) suffix))

(defun edts-find-record-source ()
  "Jump to the record-definition under point."
  (let* ((mark (copy-marker (point-marker)))
         (rec-name (thing-at-point 'symbol))
         (info (edts-get-detailed-module-info (erlang-get-module)))
         (records (cdr (assoc 'records info)))
         (record (edts-nav-find-record rec-name records)))
    (if record
        (progn
          (ring-insert-at-beginning (edts-window-history-ring) mark)
          (find-file-existing (cdr (assoc 'source record)))
          (ferl-goto-line (cdr (assoc 'line   record))))
      (null (error "No record at point")))))

(defun edts-nav-find-record (rec-name records)
  "find record-struct with REC-NAME in RECORDS."
  (find-if #'(lambda (rec)(string= rec-name (cdr (assoc 'record rec))))
           records))

(defun edts-find-macro-source ()
  "Jump to the macro-definition under point."
  (let* ((macro (thing-at-point 'symbol))
         (re    (format "-define\\s-*(%s\\s-*[(,]" macro)))
  (or (edts-search-current-buffer re)
      (edts-search-includes re)
      (error "No macro at point"))))

(defun edts-search-current-buffer (re)
  "Find the first match for RE in the current buffer. Move point there
and make an entry in edts-window-history-ring."
  (let ((mark  (copy-marker (point-marker))))
    (goto-char (point-min))
    (if (re-search-forward re nil t)
        (progn
          (beginning-of-line)
          (ring-insert-at-beginning (edts-window-history-ring) mark))
        (null (goto-char mark)))))

(defun edts-search-includes (re &optional group-index)
  "Find the first match for RE in the current buffer's include files.
Move point there and make an entry in edts-window-history-ring."
  (let ((mark        (copy-marker (point-marker)))
        (group-index (or group-index 0))
        (includes    (edts-get-includes))
        (found       nil))
    (with-temp-buffer
      (save-excursion
        (while (and includes (not found))
          (insert-file-contents (car includes) nil nil nil t)
          (goto-char (point-min))
          (when (re-search-forward re nil t) (setq found (car includes)))
          (pop includes))))
    (when found
      (find-file-existing found)
      (goto-char (point-min))
      (re-search-forward re nil t)
      (beginning-of-line)
      (ring-insert-at-beginning (edts-window-history-ring) mark))))

(defun edts-find-source (module function arity)
  "Find the source code for MODULE in a buffer, loading it if necessary.
When FUNCTION is specified, the point is moved to its start."
  ;; Add us to the history list
  (let ((mark (copy-marker (point-marker))))
    (if (or (equal module (erlang-get-module))
            (string-equal module "MODULE"))
        (if function
            (progn
              (ring-insert-at-beginning (edts-window-history-ring) mark)
              (ferl-search-function function arity))
            (null (error "Function %s:%s/%s not found" module function arity)))
        (let* ((node (edts-project-buffer-node-name (current-buffer)))
               (info (edts-get-function-info node module function arity)))
          (if info
              (progn
                (find-file-existing (cdr (assoc 'source info)))
                (ring-insert-at-beginning (edts-window-history-ring) mark)
                (ferl-goto-line (cdr (assoc 'line   info))))
              (null
               (error "Function %s:%s/%s not found" module function arity)))))))

;; Borrowed from distel
(defun edts-find-source-unwind ()
  "Unwind back from uses of `edts-navigate'-commands."
  (interactive)
  (let ((ring (edts-window-history-ring)))
    (unless (ring-empty-p ring)
      (let* ((marker (ring-remove ring))
             (buffer (marker-buffer marker)))
        (if (buffer-live-p buffer)
            (progn (switch-to-buffer buffer)
                   (goto-char (marker-position marker)))
          ;; If this buffer was deleted, recurse to try the next one
          (edts-find-source-unwind))))))

(defun edts-window-history-ring ()
  (let ((window (selected-window)))
    (or (window-parameter window 'edts-find-history-ring)
        (set-window-parameter window 'edts-find-history-ring (make-ring 20)))))

(defun edts-who-calls ()
  (interactive)
  (let ((node (edts-project-buffer-node-name (current-buffer)))
        (mfa  (ferl-mfa-at-point)))
    (if mfa
        (apply #'edts-find-callers (cons node mfa))
        (error "No call at point."))))

(defvar edts-found-caller-items nil
  "The callers found during the last call to edts-who-calls")

(defun edts-find-callers (node module function arity)
  "Jump to any all functions calling `module':`function'/`arity' in the
current buffer's project."
  (edts-log-info "Finding callers of %s:%s/%s" module function arity)
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
  (edts-log-info "Re-doing last edts-who-calls")
  (edts-do-find-callers edts-found-caller-items))

(defun edts-function-popup-item (item)
  "Formats an association list describing a function as a string"
  (let* ((module   (cdr (assoc 'module   item)))
         (function (cdr (assoc 'function item)))
         (arity    (cdr (assoc 'arity    item)))
         (str (format "%s:%s/%s" module function arity)))
    (popup-make-item str :value item)))


(provide 'edts-navigate)
