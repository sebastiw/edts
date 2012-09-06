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

(defcustom edts-project-auto-start-node t
  "If non-nil, automagically start an erlang node whenever erlang-mode is
activated for the first file that is located inside a project."
  :type 'boolean
  :group 'edts)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun edts-project-init ()
  "Buffer specific (not necessarily buffer-local) setup."
  (let* ((buffer  (current-buffer))
         (project (edts-project-buffer-project buffer)))
    (when (and project edts-project-auto-start-node)
      (edts-project-ensure-buffer-node-started buffer))))

(defun edts-project-ensure-buffer-node-started (buffer)
  "Start BUFFER's project's node if it is not already started."
  (edts-project-ensure-node-started (edts-project-buffer-project buffer)))

(defun edts-project-ensure-node-started (project)
  "Start BUFFER's project's node if it is not already started."
  (let ((node-name (edts-project-node-name project)))
    (if (edts-project-node-started-p node-name)
        (edts-register-node-when-ready node-name)
        (edts-project-start-node project))))

(defun edts-project-start-node (project)
  "Starts a new erlang node for PROJECT."
  (let* ((project-name (edts-project-name project))
         (node-name    (edts-project-node-name project))
         (buffer-name  (concat "*" project-name "*"))
         (command      (edts-project-build-project-command project))
         (pwd          (expand-file-name (edts-project-root project))))
    (edts-project-ensure-node-not-started node-name)
    (edts-project-make-comint-buffer buffer-name pwd command)
    (edts-register-node-when-ready node-name)
    (when (member 'distel features)
      (let ((node-name-symbol
             (make-symbol (concat node-name "\@" system-name))))
        (setq erl-nodename-cache node-name-symbol)))
    (get-buffer buffer-name)))

(defun edts-project-build-project-command (project)
  "Build a command line for PROJECT"
  (let ((command (edts-project-start-command project)))
    (if command
        (delete "" (split-string command))
        (let ((path (edts-project-code-path-expand project))
              (sname (edts-project-node-name project)))
          (append
           (list (executable-find "erl") "-sname" sname "-pa")
           path)))))

(defun edts-project-make-comint-buffer (buffer-name pwd command)
  "In a comint-mode buffer Starts a node with BUFFER-NAME by cd'ing to
PWD and running COMMAND."
  (let* ((cmd  (car command))
         (args (cdr command)))
    (with-current-buffer (get-buffer-create buffer-name) (cd pwd))
    (apply #'make-comint-in-buffer cmd buffer-name cmd nil args)))

(defun edts-project-buffer-node-started-p (buffer)
  "Returns non-nil if there is an edts-project erlang node started that
corresponds to BUFFER."
  (edts-project-node-started-p (edts-project-buffer-node-name buffer)))

(defun edts-project-node-started-p (node-name)
  "Returns non-nil if there is an edts-project erlang node with name
NODE-NAME running on localhost."
  (edts-node-running node-name))

(defun edts-project-ensure-node-not-started (node-name)
  "Signals an error if a node of name NODE-NAME is running on
localhost."
  (when (edts-project-node-started-p node-name)
    (error "Node already up")))

(defun edts-project-name (project)
  "Returns the name of the edts-project PROJECT. No default value,
come on you have to do *something* yourself!"
  (edts-project-property 'name project))

(defun edts-project-root (project)
  "Returns the root directory of the edts-project PROJECT."
  (edts-project-property 'root project))

(defun edts-project-lib-dirs (project)
  "Returns the edts-project PROJECT's library directories. Defaults to
(\"lib\")"
  (or (edts-project-property 'lib-dirs project) '("lib")))

(defun edts-project-node-name (project)
  "Returns the edts-project PROJECT's erlang node-name. Currently only
short names are supported."
  (or (edts-project-property 'node-sname project) (edts-project-name project)))

(defun edts-project-start-command (project)
  "Returns the edts-project PROJECT's command for starting it's project
 node."
  (edts-project-property 'start-command project))

(defun edts-project-property (prop project)
  "Returns the value of the property of name PROP from PROJECT."
  (cdr (assoc prop project)))

(defun edts-project-code-path-expand (project)
  "Expands PROJECT's ebin and listed lib dirs to a full set of ebin
directories, treating every subdirectory of each lib dir a an OTP
application."
  (let ((root     (edts-project-root project))
        (lib-dirs (edts-project-lib-dirs project)))
    (cons
     (format "%s/ebin" (edts-project-normalize-path root))
     (apply #'append
            (mapcar #'(lambda (dir)
                        (edts-project-path-expand root dir)) lib-dirs)))))

(defun edts-project-path-expand (root dir)
  "Returns a list of all existing ebin directories in any folder directly
beneath ROOT/DIR."
  (setq root (edts-project-normalize-path root))
  (file-expand-wildcards (format "%s/%s/*/ebin" root dir)))

(defun edts-project-buffer-node-name (buffer)
  "Returns the erlang node-name of BUFFER's edts-project node."
  (edts-project-node-name (edts-project-buffer-project buffer)))

(defun edts-project-buffer-project (buffer)
  "Returns the edts-project that BUFFER is part of, if any,
otherwise nil."
  (edts-project-file-project (buffer-file-name buffer)))

(defun edts-project-file-project (file-name)
  "Returns the edts-project that the file with FILE-NAME is part of,
if any, otherwise nil."
  (find-if  #'(lambda (p) (edts-project-file-in-project-p p file-name))
            edts-projects))

(defun edts-project-file-in-project-p (project file-name)
  "Returns non-nil if the fully qualified FILE-NAME is located
inside the edts-project PROJECT."
  (edts-project-file-under-path-p (edts-project-root project) file-name))

(defun edts-project-file-under-path-p (path file-name)
  "Returns non-nil if the fully qualified file-name is located
underneath PATH."
  (string-prefix-p (edts-project-normalize-path path)
                   (expand-file-name file-name)))

(defun edts-project-normalize-path (path-str)
  "Badly named function. Only replaces duplicate /'s in PATH-STR and
make sure it ends with a '/'."
  (replace-regexp-in-string "//+" "/"
                            (concat (expand-file-name path-str) "/")))

(provide 'edts-project)
