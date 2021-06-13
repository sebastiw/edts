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
;; EDTS Plugin management library

;; Prerequisites

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paths
(require 'cl-lib)
(require 'cl-macs)
(require 'f)
(require 's)

(require 'edts)

(defconst edts-plugin-directory-name
  "lib"
  "Bare directory name for the plugins")

(defconst edts-plugin-directory
  (f-join (file-name-directory edts-root-directory) edts-plugin-directory-name)
  "Directory where edts plugins are located.")

(add-to-list 'load-path edts-plugin-directory)

(defconst edts-plugin-names
  (cl-remove-if
   (lambda (v) (equal v "edts"))
   (cl-loop for (file dirp . rest)
            in (directory-files-and-attributes edts-plugin-directory nil "^[^.]")
            when dirp
            collect file))
  "a list of the names of all available plugins.")

(defcustom edts-plugin-disabled-plugins '("edts")
  "List of disabled plugins."
  :type (cons 'set
              (mapcar #'(lambda (plugin) (list 'const plugin))
                      edts-plugin-names))
  :group 'edts)

(defun edts-plugin-init-all ()
  "Initialize available plugins."
  (mapc #'edts-plugin-init edts-plugin-names))

(defun edts-plugin-load-tests ()
  "Load test-files for all plugins."
  (mapc #'edts-plugin--load-plugin-tests '("edts_xref"))
  ;; (mapc #'edts-plugin--load-plugin-tests edts-plugin-names)
  )

(defun edts-plugin--load-plugin-tests (plugin)
  "Load test-files for all plugins."
  (let* ((plugin-dir        (f-join edts-plugin-directory plugin))
         (elisp-plugin-name (replace-regexp-in-string "_" "-" plugin))
         (el-pattern        (f-join plugin-dir "*-test.el")))
    (mapc #'load (file-expand-wildcards el-pattern))))

(defun edts-plugin-init (plugin-name)
  "Do the necessary initialization for PLUGIN."
  (if (member plugin-name edts-plugin-disabled-plugins)
      (edts-log-debug "Plugin %s is disabled" plugin-name)
    (edts-log-debug "Initializing plugin %s" plugin-name)
    (let* ((plugin-dir        (f-join edts-plugin-directory plugin-name))
           (elisp-plugin-name (replace-regexp-in-string "_" "-" plugin-name))
           (init-fun          (intern (concat elisp-plugin-name "-init")))
           (buf-init-fun      (intern (concat elisp-plugin-name
                                              "-buffer-init")))
           (el-pattern        (f-join plugin-dir "*.el"))
           (el-files          (file-expand-wildcards el-pattern)))
      (mapc #'(lambda (f)
                (when (not (string-match ".*-test" (f-base f)))
                  (require (intern (f-base f)))))
            el-files)
      (when (fboundp init-fun)
        (funcall init-fun))
      (when (fboundp buf-init-fun)
        (add-hook 'edts-mode-hook buf-init-fun)))))

(defun edts-plugin-call (node plugin method &optional args)
  "Call PLUGIN's rpc method METHOD with ARGS on NODE."
  (edts-log-debug "Plugin call %s:%s on %s" plugin method node)
  (let* ((resource (s-join "/"
                           (list edts-plugin-directory-name
                                 (symbol-name plugin)
                                 (symbol-name method))))
         (args     (cons (cons "node" node) args))
         (reply    (edts-rpc-call resource args))
         (body     (cdr (assoc 'body reply))))
    (if (not (equal (cdr (assoc 'result reply)) '("200" "OK")))
        (prog1 nil
         (edts-log-error "Unexpected reply: %s" (cdr (assoc 'result reply))))
      (if (equal "error" (cdr (assoc 'result body)))
          (prog1 nil
            (edts-log-error "Error in %s call to %s: %s"
                            plugin
                            method
                            (cdr (assoc 'return body))))
        (cdr (assoc 'return body))))))

(defun edts-plugin-call-async (node plugin method &optional args cb cb-args)
  "Call PLUGIN's rpc method METHOD with ARGS on NODE asynchronously. Calling
CB with the result when request terminates."
  (edts-log-debug "Plugin async call %s:%s on %s" plugin method node)
  (let* ((resource (s-join "/"
                           (list edts-plugin-directory-name
                                 (symbol-name plugin)
                                 (symbol-name method))))
         (args     (cons (cons "node" node) args)))
    (edts-rpc-call-async resource
                          args
                          'edts-plugin-call-async-callback
                          (list plugin method cb cb-args))))

(defun edts-plugin-call-async-callback (reply
                                        plugin
                                        method
                                        callback
                                        callback-args)
  (let ((body       (cdr (assoc 'body reply))))
    (if (not (equal (cdr (assoc 'result reply)) '("200" "OK")))
        (prog1 nil
         (edts-log-error "Unexpected reply: %s" (cdr (assoc 'result reply))))
      (if (equal "error" (cdr (assoc 'result body)))
          (prog1 nil
            (edts-log-error "Error in %s call to %s: %s"
                            plugin
                            method
                            (cdr (assoc 'return body))))
        (apply callback (cdr (assoc 'return body)) callback-args)))))

(provide 'edts-plugin)
