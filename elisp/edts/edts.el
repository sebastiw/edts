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
;; Misc edts-related functionality.

(defcustom edts-erl-command (executable-find "erl")
  "Location of the erl-executable to use when launching the main EDTS-
node."
  :group 'edts)

(defconst edts-erl-root
  (file-name-directory
   (directory-file-name
    (file-name-directory (file-truename edts-erl-command))))
  "Location of the Erlang root directory")


(defun edts-query (prompt choices)
  "Query the user for a choice"
  (ido-completing-read prompt choices))

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

(defun edts-extract-doc-from-source (module function arity)
  "Find documentation for MODULE:FUNCTION/ARITY"
  (let ((source (cdr (assoc 'source (edts-get-basic-module-info module)))))
    (edts-doc-extract-function-information-from-source source function arity)))

(defun edts-function-regexp (function arity)
  "Construct a regexp matching FUNCTION(arg1, ..., argARITY)."
  (format "%s[[:space:]\n]*(%s)[[:space:]\n]*->"
          function (edts-argument-regexp arity)))

(defun edts-any-function-regexp ()
  "Construct a regexp matching any function."
  (format "%s[[:space:]\n]*(\\(.*\\))[[:space:]\n]*->" erlang-atom-regexp))

(defun edts-argument-regexp (arity)
  "Contstruct a regexp matching ARITY arguments."
  (if (equal arity 0)
      "[[:space:]\n]*"
      (concat "[^,]*" (apply #'concat (make-list (- arity 1) ",[^,]*")))))

(defun edts-ahs-edit-current-function ()
  "Activate ahs-edit-mode with erlang-current-function range-plugin"
  (interactive)
  (ahs-onekey-edit-function 'erlang-current-function nil))


(defun edts-ensure-server-started ()
  "Starts an edts server-node in a comint-buffer unless it is already running."
  (unless (edts-node-started-p "edts")
    (edts-start-server)))

(defun edts-start-server ()
  "Starts an edts server-node in a comint-buffer"
  (interactive)
  (when (edts-node-started-p "edts")
    (error "EDTS: Server already running"))
  (with-temp-buffer
    (cd (concat edts-lib-directory "/.."))
    (make-comint "edts" "./start.sh" nil edts-erl-command)))

(defun edts-ensure-node-not-started (node-name)
  "Signals an error if a node of name NODE-NAME is running on
localhost."
  (when (edts-node-started-p node-name)
    (error "Node already started")))

(defun edts-node-started-p (name)
  "Syncronously query epmd to see whether it has a node with NAME registered."
  (condition-case ex
      (with-temp-buffer
        (let ((socket (open-network-stream
                       "epmd"
                       (current-buffer)
                       "localhost"
                       4369)))
          (process-send-string socket (edts-build-epmd-message "n"))
          (accept-process-output socket 0.5))
          (member name (edts-nodenames-from-string (buffer-string))))
    ('file-error nil)))

(defun edts-nodenames-from-string (string)
  "Convert the epmd reply STRING into a list of nodenames."
  (setq string (split-string (substring string 4)))
  (let ((names  nil))
    (while string
      (when (string-equal (car string) "name")
        (setq names (cons (cadr string) names)))
      (setq string (cdr string)))
    names))

(defun edts-build-epmd-message (msg)
  "Build a message for the epmd from MSG. Logic taken from distel's epmd.el."
  (let* ((len (length msg))
(len-msb (ash len -8))
(len-lsb (logand len 255)))
    (concat (string len-msb len-lsb) msg)))

(defun edts-register-node-when-ready (node-name &optional retries)
  "Once NODE-NAME is registered with epmd, register it with the edts
node, optionally retrying RETRIES times."
  (let ((retries (if retries retries 20)))
  (run-with-timer
   0.5
   nil
   #'edts-register-node-when-ready-function node-name retries)))

(defun edts-register-node-when-ready-function (node-name retries)
  (if (edts-node-started-p node-name)
      (edts-register-node node-name)
      (if (> retries 0)
          (edts-register-node-when-ready node-name (- retries 1))
          (edts-log-error "Error: edts could not register node '%s'" node-name))))

(defun edts-register-node (node-name)
  "Register NODE-NAME with the edts node."
  (interactive "MNode-name: ")
  (let* ((res (edts-rest-post (list "nodes" node-name) nil)))
    (if (equal (assoc 'result res) '(result "201" "Created"))
        node-name
        (null (edts-log-error "Unexpected reply: %s" (cdr (assoc 'result res)))))))

(defun edts-get-who-calls (node module function arity)
  "Fetches a list of all function calling  MODULE:FUNCTION/ARITY on NODE."
  (let* ((node-name node)
         (resource (list "nodes" node-name
                         "modules" module
                         "functions" function
                         (number-to-string arity)
                         "callers"))
         (res      (edts-rest-get resource nil)))
    (if (equal (assoc 'result res) '(result "200" "OK"))
        (cdr (assoc 'body res))
        (null (edts-log-error "Unexpected reply: %s" (cdr (assoc 'result res)))))))

(defun edts-get-function-info (node module function arity)
  "Fetches info MODULE on the node associated with
current buffer."
  (let* ((node-name node)
         (resource (list "nodes" node-name
                         "modules" module
                         "functions" function
                         (number-to-string arity)))
         (res      (edts-rest-get resource nil)))
    (if (equal (assoc 'result res) '(result "200" "OK"))
        (cdr (assoc 'body res))
        (null (edts-log-error "Unexpected reply: %s" (cdr (assoc 'result res)))))))

(defun edts-get-modules ()
  "Fetches all available erlang modules for the node associated with
current buffer."
  (let* ((node-name (edts-project-buffer-node-name (current-buffer)))
         (resource (list "nodes" node-name "modules"))
         (res      (edts-rest-get resource nil)))
    (if (equal (assoc 'result res) '(result "200" "OK"))
        (cdr (assoc 'body res))
        (null (edts-log-error "Unexpected reply: %s" (cdr (assoc 'result res)))))))

(defun edts-get-module-exports (module)
  "Fetches all exported functions of MODULE on the node associated with
current buffer. Does not fetch detailed information about the individual
functions."
  (let* ((node-name (edts-project-buffer-node-name (current-buffer)))
         (resource (list "nodes" node-name
                         "modules" module))
         (res      (edts-rest-get resource '(("info_level" . "basic")))))
    (if (equal (assoc 'result res) '(result "200" "OK"))
          (cdr (assoc 'exports (cdr (assoc 'body res))))
        (null (edts-log-error "Unexpected reply: %s"
                              (cdr (assoc 'result res)))))))

(defun edts-function-to-string (function-struct)
  "Convert FUNCTION-STRUCT to a string of <function>/<arity>."
  (format "%s/%s"
          (cdr (assoc 'function function-struct))
          (cdr (assoc 'arity    function-struct))))

(defun edts-get-basic-module-info (module)
  "Fetches basic info about module on the node associated with current buffer"
  (edts-get-module-info module 'basic))

(defun edts-get-detailed-module-info (module)
  "Fetches detailed info about MODULE on the node associated with current
buffer"
  (edts-get-module-info module 'detailed))

(defun edts-get-module-info (module level)
  "Fetches info about MODULE on the node associated with current buffer.
LEVEL is either basic or detailed."
  (let* ((node-name (edts-project-buffer-node-name (current-buffer)))
         (resource (list "nodes" node-name "modules" module))
         (args     (list (cons "info_level" (symbol-name level))))
         (res      (edts-rest-get resource args)))
    (if (equal (assoc 'result res) '(result "200" "OK"))
        (cdr (assoc 'body res))
        (null (edts-log-error "Unexpected reply: %s" (cdr (assoc 'result res)))))))

(defun edts-get-module-xref-analysis-async (module checks callback buffer)
  "Compile MODULE in FILE on the node associated with current buffer,
asynchronously. When the request terminates, call CALLBACK with the
parsed response as the single argument."
  (let* ((node-name     (edts-project-buffer-node-name (current-buffer)))
         (resource      (list "nodes" node-name "modules" module "xref_analysis"))
         (args (list    (cons "xref_checks" (mapcar #'symbol-name checks))))
         (rest-callback #'(lambda (result callback buffer)
                            (if (equal (assoc 'result result)
                                       '(result "200" "OK"))
                                (apply
                                 callback
                                 (list
                                  (cdr (assoc 'body result))
                                  buffer))
                                (null
                                 (edts-log-error
                                  "Unexpected reply: %s"
                                  (cdr (assoc 'result result))))))))
    (edts-log-debug "fetching xref-analysis of %s async on %s" module node-name)
    (edts-rest-get-async resource args rest-callback (list callback buffer))))

(defun edts-compile-and-load-async (module file callback buffer)
  "Compile MODULE in FILE on the node associated with current buffer,
asynchronously. When the request terminates, call CALLBACK with the
parsed response as the single argument."
  (let* ((node-name     (edts-project-buffer-node-name (current-buffer)))
         (resource      (list "nodes" node-name "modules" module))
         (args (list    (cons "file" file)))
         (rest-callback #'(lambda (result callback buffer)
                            (if (equal (assoc 'result result)
                                       '(result "201" "Created"))
                                (apply
                                 callback
                                 (list
                                  (cdr (assoc 'body result))
                                  buffer))
                                (null
                                 (edts-log-error
                                  "Unexpected reply: %s"
                                  (cdr (assoc 'result result))))))))
    (edts-log-debug "Compiling %s async on %s" module node-name)
    (edts-rest-post-async resource args rest-callback (list callback buffer))))

(defun edts-compile-and-load (module file)
  "Compile MODULE in FILE on the node associated with current buffer."
  (let ((node-name (edts-project-buffer-node-name (current-buffer))))
    (edts-log-debug "Compiling %s on %s" module node-name)
    (let* ((resource
            (list "nodes" node-name "modules" module))
           (args (list (cons "file" file)))
           (res (edts-rest-post resource args)))
      (if (equal (assoc 'result res) '(result "201" "Created"))
          (cdr (assoc 'body res))
        (null (edts-log-error "Unexpected reply: %s"
                              (cdr (assoc 'result res))))))))

(defun edts-get-includes ()
  "Get all includes of module in current-buffer from the node
associated with that buffer."
  (let ((info (edts-get-detailed-module-info (erlang-get-module))))
    (cdr (assoc 'includes info)))) ;; Get all includes

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unit tests

(when (member 'ert features)

  (ert-deftest edts-erl-root-test ()
    (flet ((executable-find (exec) "/usr/bin/erl")
           (file-truename (file)   "/usr/lib/erlang/bin/erl"))
      (should
       (equal "/usr/lib/erlang/" edts-erl-root)))))


(provide 'edts)
