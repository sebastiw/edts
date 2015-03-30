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
(require 'edts-navigate)

(eval-and-compile
  (defvar edts-built-in-functions
    '("abs/1"
      "adler32/1"
      "adler32_combine/3"
      "alive/1"
      "apply/2"
      "apply/3"
      "atom_to_binary/2"
      "atom_to_list/1"
      "binary_to_atom/2"
      "binary_to_existing_atom/2"
      "binary_to_list/1"
      "binary_to_list/3"
      "binary_to_term/1"
      "binary_to_term/2"
      "binary_to_integer/1"
      "binary_to_integer/2"
      "binary_to_float/1"
      "bit_size/1"
      "bitstring_to_list/1"
      "byte_size/1"
      "check_process_code/2"
      ;; "contact_binary" removed?
      "crc32/1"
      "crc32/2"
      "crc32_combine/3"
      "date/0"
      "decode_packet/3"
      "delete_module/1"
      "disconnect_node/1"
      "element/2"
      "erase/0"
      "erase/1"
      "exit/1"
      "exit/2"
      "float/1"
      "float_to_list/1"
      "garbage_collect/0"
      "garbage_collect/1"
      "get/1"
      "get_keys/1"
      "group_leader/0"
      "group_leader/2"
      "halt/0"
      "halt/1"
      "halt/2"
      "hd/1"
      "integer_to_list/1"
      "integer_to_list/2"
      ;; "internal_bif" removed?
      "iolist_size/1"
      "iolist_to_binary/1"
      "is_alive/0"
      "is_atom/1"
      "is_binary/1"
      "is_bitstring/1"
      "is_boolean/1"
      "is_float/1"
      "is_function/1"
      "is_function/2"
      "is_integer/1"
      "is_list/1"
      "is_number/1"
      "is_pid/1"
      "is_port/1"
      "is_process_alive/1"
      "is_record/2"
      "is_record/3"
      "is_reference/1"
      "is_tuple/1"
      "length/1"
      "link/1"
      "list_to_atom/1"
      "list_to_binary/1"
      "list_to_bitstring/1"
      "list_to_existing_atom/1"
      "list_to_float/1"
      "list_to_integer/1"
      "list_to_pid/1"
      "list_to_tuple/1"
      "load_module/2"
      "make_ref/0"
      "module_loaded/1"
      "monitor_node/2"
      "monitor_node/3"
      "node/0"
      "node/1"
      ;; "node_link" removed?
      ;; "node_unlink" removed?
      "nodes/0"
      "nodes/1"
      ;; "notalive" removed?
      "now/0"
      "open_port/2"
      "pid_to_list/1"
      "port_close/1"
      "port_command/2"
      "port_command/3"
      "port_connect/2"
      "port_control/3"
      "pre_loaded/0"
      "process_flag/2"
      "process_flag/3"
      "process_info/1"
      "process_info/2"
      "processes/0"
      "purge_module/1"
      "put/2"
      "register/2"
      "registered/0"
      "round/1"
      "self/0"
      "setelement/3"
      "size/1"
      "spawn/1"
      "spawn/2"
      "spawn/3"
      "spawn/4"
      "spawn_link/1"
      "spawn_link/2"
      "spawn_link/3"
      "spawn_link/4"
      "spawn_monitor/1"
      "spawn_monitor/3"
      "spawn_opt/2"
      "spawn_opt/3"
      "spawn_opt/4"
      "spawn_opt/5"
      "split_binary/2"
      "statistics/1"
      "term_to_binary/1"
      "term_to_binary/2"
      "throw/1"
      "time/0"
      "tl/1"
      "trunc/1"
      "tuple_size/1"
      "tuple_to_list/1"
      "unlink/1"
      "unregister/1"
      "whereis/1")
    "Erlang built-in functions (BIFs)"))

(defvar edts-find-macro-regexp
  "\\(\\(\\('.*'\\)\\|\\([a-zA-Z0-9_@]*\\)\\)[\\s-]*\\((.*)\\)?\\)"
  "Regexp describing a macro name")

(defconst edts-find-macro-definition-regexp
  (format "^-define\\s-*(%s,\\s-*\\(.*\\))." edts-find-macro-regexp)
  "Regexp describing a macro definition")

(defalias 'edts-inhibit-fringe-markers 'edts-face-inhibit-fringe-markers)
(defalias 'edts-marker-fringe 'edts-face-marker-fringe)

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
            for (module . imported) in (erlang-get-import)
            do  (when (eq a (cdr (assoc f imported)))
                  (setq m module)
                  (return-from import module))))
    (unless m
      (when (member (format "%s/%s" f a) edts-built-in-functions)
        (setq m "erlang")))
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
  "Activate ahs-edit-mode with edts-current-function range-plugin."
  (interactive)
  (ahs-onekey-edit-function 'edts-current-function nil))

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
