;;; edts.el --- Misc edts-related functionality.

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

(require 'auto-highlight-symbol)
(require 'erlang)
(require 'f)

(require 'edts-doc)
(require 'edts-event)
(require 'edts-api)

(defvar edts-find-macro-regexp
  "\\(\\(\\('.*'\\)\\|\\([a-zA-Z0-9_@]*\\)\\)[\\s-]*\\((.*)\\)?\\)"
  "Regexp describing a macro name")

(defconst edts-find-macro-definition-regexp
  (format "^-define\\s-*(%s,\\s-*\\(.*\\))." edts-find-macro-regexp)
  "Regexp describing a macro definition")

(defalias 'edts-inhibit-fringe-markers 'edts-face-inhibit-fringe-markers)
(defalias 'edts-marker-fringe 'edts-face-marker-fringe)

;; workaround to get proper variable highlighting in the shell.
(defvar erlang-font-lock-keywords-vars
  (list
   (list
    #'(lambda (max)
        (block nil
          (while (re-search-forward erlang-variable-regexp max 'move-point)
            ;; no numerical constants
            (unless (eq ?# (char-before (match-beginning 0)))
              (return (match-string 0))))))
    1 'font-lock-variable-name-face nil))
  "Font lock keyword highlighting Erlang variables.
Must be preceded by `erlang-font-lock-keywords-macros' to work properly.")

(defun edts-event-handler (node class type info)
  (case type
    (node_down
     (let ((node (cdr (assoc 'node info))))
       (edts-log-info "Node %s down" node)
       (run-hook-with-args 'edts-api-node-down-hook node)))
    (server_down
     (edts-log-info "EDTS server down")
     (setq edts-api--outstanding-node-registration-requests nil)
     (run-hooks 'edts-api-server-down-hook))))
(edts-event-register-handler 'edts-event-handler 'edts)

(defun edts-buffer-node-name ()
  "Print the node sname of the erlang node connected to current
buffer. The node is either:
- The module's project node, if current buffer is an erlang module, or
- The buffer's erlang node if buffer is an edts-shell buffer.
- The project-node of the buffer that was current buffer before jumping
  to the current buffer if the file of the current buffer is located outside
  any project (eg. an \"externally\" loaded module such as an otp-module or a
  module loaded by ~/.erlang)."
  (interactive)
  (message "%s" (edts-api-node-name)))

(defun edts-mfa-at (&optional point)
  "Find mfa under POINT. POINT defaults to current point."
  (goto-char (or point (point)))
  (save-excursion
    (save-match-data
      (let* ((start (save-excursion
                      (skip-chars-backward "a-zA-Z0-9_:'")
                      (point)))
             (end   (save-excursion
                      (ferl-goto-end-of-call-name)
                      (forward-sexp)
                      (point)))
             (str   (buffer-substring-no-properties start end)))
        (car (edts-strings-to-mfas (list str)))))))

(defun edts-strings-to-mfas (strs)
  "Return a list with each string in STRS parsed to an mfa."
  (mapcar #'edts--fix-mfa-mod (edts-api-get-mfas strs)))

(defun edts--fix-mfa-mod (mfa)
  (let ((m (cdr (assoc 'module mfa)))
        (f (cdr (assoc 'function mfa)))
        (a (cdr (assoc 'arity mfa))))
    (unless m
      (loop named import
            for (module . imported) in (erlang-get-import) do
            (when (eq a (cdr (assoc f imported)))
              (return-from import module))))
    (if m
        (list m f a)
      (list (erlang-get-module) f a))))

(defun edts-search-function (function arity)
  "Goto the definition of FUNCTION/ARITY in the current buffer."
  (let ((origin (point))
        (re (concat "^" function "\s*("))
        (match nil))
    (goto-char (point-min))
    (while (and (null match) (re-search-forward re nil t))
      (goto-char (match-beginning 0))
      (ferl-goto-end-of-call-name)
      (when (eq arity (car (last (edts-mfa-at (point)))))
        (setq match t)))
    (if match
        (beginning-of-line)
      (goto-char origin)
      (error "function %s/%s not found" function arity))))


(defun edts-query (prompt choices &optional error-msg)
  "Query the user for a choice"
  (let ((choice (ido-completing-read prompt choices)))
    (if (member choice choices)
        choice
      (error (or error-msg "Invalid choice")))))

(defun edts-find-doc ()
  "Find and show the man-page documentation for a function."
  (interactive)
  (let* ((module
          (edts-query "Module: " (edts-man-modules)))
         (fun-strings (edts-man-module-function-entries module))
         (fun (edts-query "Function: " (cons "-Top of Chapter-" fun-strings))))
    (if (string= fun "-Top of Chapter-")
        (edts-man-find-module module)
        (let* ((split     (split-string fun "/"))
               (fun-name  (car split))
               (fun-arity (string-to-number (cadr split))))
          (edts-man-find-function-entry module fun-name fun-arity)))))

(defun edts-show-doc-under-point ()
  "Find and display the man-page documentation for function under point
in a tooltip."
  (interactive)
  (let* ((mfa      (edts-mfa-at (point)))
         (module   (car mfa))
         (function (cadr mfa))
         (arity    (caddr mfa)))
    (unless (and module function arity)
      (error "Could not parse MFA at point"))
    (edts-show-tooltip
     (condition-case ex
         (edts-man-extract-function-entry module function)
       ('error
        (edts-extract-doc-from-source module function arity))))))

(defun edts-show-tooltip (text)
  "Show a tooltip using either popup.el or pos-tip.el"
  (condition-case ex
      (pos-tip-show text nil nil nil -1)
    ('error
     (popup-tip text))))

(defun edts-extract-doc-from-source (module function arity)
  "Find documentation for MODULE:FUNCTION/ARITY"
  (let ((source (cdr (assoc 'source (edts-api-get-basic-module-info module)))))
    (if source
        (edts-doc-extract-function-information-from-source source
                                                           function
                                                           arity)
      (null (edts-log-error "No such module: %s" module)))))

(defun edts-function-head-regexp (function &optional arity)
  "Construct a regexp matching FUNCTION(arg1, ..., argARITY). A negative number
for ARITY will give a regexp matching any arity."
  (unless arity (setq arity -1))
  (format "%s[[:space:]\n]*(%s)" function (edts-argument-regexp arity)))

(defun edts-function-regexp (function &optional arity)
  "Construct a regexp matching 'FUNCTION(arg1, ..., argARITY) ->'.
negative number for ARITY will give a regexp matching any arity."
  (concat (edts-function-head-regexp function arity) "[[:space:]\n]*->"))

(defun edts-any-function-regexp ()
  "Construct a regexp matching any function."
  ;; Kind of broken for strings, comments and single quoted atoms
  (format "\\(%s[[:space:]\n]*(.*)\\)[[:space:]]*->" erlang-atom-regexp))

(defun edts-argument-regexp (arity)
  "Contstruct a regexp matching ARITY arguments. A negative number
for ARITY will give a regexp matching any arity."
  (cond
   ((< arity 0) "[[:ascii:]]*?")
   ((equal arity 0) "[[:space:]]*")
   ((concat "[^,]*?" (apply #'concat (make-list (- arity 1) ",[^,]*?"))))))

(defun edts-ahs-edit-current-function ()
  "Activate ahs-edit-mode with erlang-current-function range-plugin."
  (interactive)
  (ahs-onekey-edit-function 'erlang-current-function nil))

(defun edts-ahs-edit-buffer ()
  "Activate ahs-edit-mode with ahs-range-whole-buffer range-plugin."
  (interactive)
  (ahs-onekey-edit-function 'whole-buffer nil))

(defun edts-pretty-print-term (term-str indent max-col)
  "Pretty-print the term represented by TERM-STR, indenting it INDENT
spaces and breaking lines at column MAX-COL."
  (let* ((resource '("pretty_print"))
         (rest-args `(("string" .   ,term-str)
                      ("indent" .   ,(number-to-string indent))
                      ("max_column" ,(number-to-string max-col))))
         (res         (edts-rest-get resource rest-args )))
    (if (equal (assoc 'result res) '(result "200" "OK"))
        (cdr (assoc 'return (cdr (assoc 'body res))))
      (null
       (edts-log-error "Unexpected reply: %s" (cdr (assoc 'result res)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests

(provide 'edts)
