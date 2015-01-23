;;; edts-navigate.el --- Code for navigating through edts projects.

;; Copyright 2012-2013 Thomas Järvstrand <tjarvstrand@gmail.com>

;; Author: Thomas Järvstrand <thomas.jarvstrand@gmail.com>
;; Keywords: erlang
;; This file is not part of GNU Emacs.

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

(require 'dash)
(require 'ring)
(require 'thingatpt)

(require 'edts-api)

(require 'ferl)

(defvar edts-navigate-originating-module nil
  "The module from which we navigated to the current header file. Only
set for .hrl-files.")
(make-variable-buffer-local 'edts-navigate-originating-module)

;; Nameclash (sort of) with edts-find source. One of the names should be
;; changed to indicate the differences in how they work.
(defun edts-find-global-function ()
  "Find a module in the current project."
  (interactive)
  (let ((modules (edts-api-get-modules)))
    (unless modules
      (error "No modules found"))
    (let* ((choice (edts-query "Module: " modules "No such module"))
           (file (cdr (assoc 'source (edts-api-get-basic-module-info choice)))))
      (edts-find-file-existing file)
      (edts-find-local-function nil))))

(defun edts-find-local-function (set-mark)
  "Find a function in the current module."
  (interactive '(t))
  (let* ((functions (ferl-local-functions))
         (names     (mapcar #'(lambda (el) (car el)) functions))
         (choice    (edts-query
                     "Function: " (cons "-Top of Module-" names)
                     "No such function"))
         (mark      (point-marker)))
    (if (string= "-Top of Module-" choice)
        (goto-char 0)
        (goto-char (cdr (assoc choice functions))))
    (when (and set-mark (not (eq (point) (marker-position mark))))
      (ring-insert-at-beginning (edts-window-history-ring) mark))))

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
   ((edts-header-under-point-p)    (edts-find-header-source))
   ((edts-macro-under-point-p)     (edts-find-macro-source))
   ((edts-record-under-point-p)    (edts-find-record-source))
   ((edts-behaviour-under-point-p) (edts-find-behaviour-source))
   ;; look for a M:F/A
   ((apply #'edts-find-source
           (or (edts-mfa-at (point)) (error "No call at point."))))))


(defun edts-header-under-point-p ()
  "Return non nil if the form under point is an include or include_lib
directive."
  (save-excursion
    (beginning-of-line)
    (looking-at "-include\\(_lib\\)?\\s-*(\\s-*.*")))

(defun edts-behaviour-under-point-p ()
  "Return non nil if the form under point is a behaviour directive."
  (save-excursion
    (beginning-of-line)
    (let ((re (format "-behaviou?r\\s-*(?\\s-*\\(%s\\)\\s-*)?."
                      erlang-atom-regexp)))
      (looking-at re))))

(defun edts-macro-under-point-p ()
  "Return non nil if the form under point is a macro."
  (save-excursion
    (beginning-of-thing 'symbol)
    (equal ?? (char-before (point)))))

(defun edts-record-under-point-p ()
  "Return non-nil if the form under point is a record"
  (let ((bounds (edts-atom-at-point)))
    (and bounds
         (eq ?# (char-before (nth 0 bounds))))))

(defun edts-atom-at-point ()
  "Return the bounds of the atom at point or nil."
  (let ((point (point)))
    (save-excursion
      (if (not (eq (get-text-property (point) 'face) 'font-lock-string-face))
          (re-search-backward "[^@_a-zA-Z09]")
        (while (eq (get-text-property (point) 'face)
                   'font-lock-string-face)
          (backward-char)))
      (forward-char)
      (when (looking-at erlang-atom-regexp)
        (match-data 0)))))

(defun edts-header-at-point ()
  "Return the filename for the header under point."
  (let ((bound (ferl-last-char-on-line-at (point))))
    (save-excursion
      (beginning-of-line)
      (when (re-search-forward "-include\\(_lib\\)?\\s-*(\\s-*\"\\s-*" bound t)
        (thing-at-point 'filename)))))

(defun edts-find-behaviour-source ()
  (save-excursion
    (beginning-of-line)
    (let ((bound (ferl-last-char-on-line-at (point)))
          (re (format "-behaviou?r\\s-*(?\\s-*\\(%s\\)\\s-*)?."
                      erlang-atom-regexp)))
      (when (re-search-forward re bound t)
        (edts-find-source (match-string 1) "behaviour_info" 1)))))

(defun edts-find-header-source ()
  "Open the source for the header file under point."
  (let* ((headerfile (edts-header-at-point))
         (mark (copy-marker (point-marker))) ;; Add us to the history list
         (includes (edts-navigate-get-includes))
         (module (or (ferl-get-module) edts-navigate-originating-module))
         (file (-first #'(lambda(x) (string=
                                     (file-name-nondirectory headerfile)
                                     (file-name-nondirectory x)))
                        includes)))
    (if (not file)
        (null (error "No header filename at point"))
      (edts-find-file-existing file)
      (setq edts-navigate-originating-module module)
      (goto-char (point-min)))))

(defun edts-has-suffix (suffix string)
  "returns string if string has suffix"
  (string= (substring string (- 0 (length suffix))) suffix))

(defun edts-find-record-source ()
  "Jump to the record-definition under point."
  (let* ((mark (copy-marker (point-marker)))
         (rec-bounds (edts-atom-at-point))
         (rec-name (buffer-substring (nth 0 rec-bounds) (nth 1 rec-bounds)))
         (info (edts-api-get-detailed-module-info (ferl-get-module)))
         (records (cdr (assoc 'records info)))
         (record (edts-nav-find-record rec-name records)))
    (if record
        (progn
          (edts-find-file-existing (cdr (assoc 'source record)))
          (ferl-goto-line (cdr (assoc 'line   record))))
      (null (error "No record at point")))))

(defun edts-nav-find-record (rec-name records)
  "find record-struct with REC-NAME in RECORDS."
  (-first #'(lambda (rec)
              (let ((rec (cdr (assoc 'record rec))))
                (or (string= rec-name rec)
                    (string= rec-name (format "'%s'" rec)))))
          records))

(defun edts-find-macro-source ()
  "Jump to the macro-definition under point."
  (let* ((macro            (thing-at-point 'symbol))
         (re               (format "-define\\s-*(%s\\s-*[(,]" macro))
         (case-fold-search nil))
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
        (includes    (edts-navigate-get-includes))
        (module      (or (ferl-get-module) edts-navigate-originating-module))
        (found       nil))
    (with-temp-buffer
      (save-excursion
        (while (and includes (not found))
          (insert-file-contents (car includes) nil nil nil t)
          (goto-char (point-min))
          (when (re-search-forward re nil t) (setq found (car includes)))
          (pop includes))))
    (when found
      (edts-find-file-existing found)
      (setq edts-navigate-originating-module module)
      (goto-char (point-min))
      (re-search-forward re nil t)
      (beginning-of-line))
    found))

(defun edts-navigate-get-includes ()
  (if (string= (file-name-extension (buffer-file-name)) "hrl")
      (edts-api-get-includes edts-navigate-originating-module)
    (edts-api-get-includes)))

(defun edts-find-source (module function arity)
  "Find the source code for MODULE in a buffer, loading it if necessary.
When FUNCTION is specified, the point is moved to its start."
  ;; Add us to the history list
  (let ((mark (copy-marker (point-marker))))
    (if (or (equal module (ferl-get-module))
            (string-equal module "MODULE"))
        ;; Function is local
        (if function
            (progn
              (edts-search-function function arity)
              (unless (eq (marker-position mark) (point))
                (ring-insert-at-beginning (edts-window-history-ring) mark)))
            (error "Function %s:%s/%s not found" module function arity))
        (let* ((info (edts-api-get-function-info module function arity)))
          (if info
              (let ((line (cdr (assoc 'line info))))
                (edts-find-file-existing (cdr (assoc 'source info)))
                (cond
                 ((integerp line) ;; We're ok
                  (ferl-goto-line line))
                 ((string= line "is_bif") ;; Function is a bif
                  (edts-log-error "%s:%s/%s is a bif" module function arity))
                 (t
                  (edts-log-error ;; Some unknown error
                   "Function %s:%s/%s not found" module function arity))))
            (edts-log-error
              "Function %s:%s/%s not found" module function arity))))))

(defun edts-navigate-to-module (module &optional line)
  "Find the source code for MODULE in a buffer, loading it if necessary.
When FUNCTION is specified, the point is moved to its start."
  ;; Add us to the history list
  (let* ((mark (copy-marker (point-marker)))
         (info (edts-api-get-basic-module-info module)))
    (if (not info)
        (null (edts-log-error "Module %s not found" module))
      (edts-find-file-existing (cdr (assoc 'source info)))
      (when line
        (ferl-goto-line line)
        (back-to-indentation))
      t)))

(defun edts-find-file-existing (file)
  "EDTS wrapper for find-file-existing."
  (let ((mark (point-marker)))
    (find-file-existing file)  ; Fixme, catch error
    (ring-insert-at-beginning (edts-window-history-ring) mark)))

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

(defun edts-navigate-function-popup (functions)
  "Popu a list of functions and navigate to the one chosen by the user.
FUNCTIONS is a list of functions where each function is an alist of module,
function and arity."
  (let* ((menu-items   (mapcar #'edts-navigate--popup-item functions))
         (choice       (popup-menu* menu-items :scroll-bar t))
         (module       (cdr (assoc 'module   choice)))
         (function     (cdr (assoc 'function choice)))
         (arity        (cdr (assoc 'arity    choice)))
         (line         (car (cdr (assoc 'lines choice)))))
    (if line
        (edts-navigate-to-module module line)
      (edts-find-source module function arity))))

(defun edts-navigate--popup-item (item)
  "Formats an association list describing a function as a string"
  (let* ((module   (cdr (assoc 'module   item)))
         (function (cdr (assoc 'function item)))
         (arity    (cdr (assoc 'arity    item)))
         (str (format "%s:%s/%s" module function arity)))
    (popup-make-item str :value item)))

(provide 'edts-navigate)
