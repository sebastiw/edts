;; Copyright 2012 Thomas Järvstrand <tjarvstrand@gmail.com>
;;
;; This file is part of EDTS.
;;
;; EDTS is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; EDTS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with EDTS. If not, see <http://www.gnu.org/licenses/>.
;;
;; Rudimentary project support for edts so that we can relate buffers to
;; projects and communicate with the correct nodes.


(defgroup edts-project '()
  "Distel and erlang-extended-mode development tools."
  :group 'tools)

(defcustom edts-projects nil
  "The list of projects."
  :group 'edts-project)

(defcustom edts-project-auto-start-node t
  "If non-nil, automagically start an erlang node whenever erlang-mode is
activated for the first file that is located inside a project."
  :type 'boolean)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar edts-project-nodes nil
  "The list of nodes.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun edts-project-init ()
  "Initializes `edts-project'."
  (when (member 'distel features) ;; some distel compatibility
        (make-variable-buffer-local 'erl-nodename-cache))
  (add-hook 'erlang-mode-hook 'edts-project-erlang-load-hook))

(defun edts-project-erlang-load-hook ()
  "Buffer specific (not necessarily buffer-local) setup."
  (let* ((buffer  (current-buffer))
         (project (edts-project-buffer-project buffer)))
    (when (and project edts-project-auto-start-node)
      (edts-project-ensure-buffer-node-started buffer))))

(defun edts-project-ensure-buffer-node-started (buffer)
  "Start a buffer's project's node if it is not already started."
  (edts-project-ensure-node-started (edts-project-buffer-project buffer)))

(defun edts-project-ensure-node-started (project)
  "Start a buffer's project's node if it is not already started."
  (if (edts-project-node-started-p (edts-project-node-name project))
      (when (member 'distel features)
        (edts-project-check-backend project))
      (edts-project-start-node project)))

(defun edts-project-check-backend (project)
  "Ensure that distel modules are available on the node used by `project'"
  (let* ((node-name-str (edts-project-buffer-node-name))
         (node-name     (make-symbol (concat node-name-str "\@" system-name))))
    (setq erl-nodename-cache node-name)
    (erl-check-backend node-name nil)))

(defun edts-project-buffer-start-node (&optional buffer)
  "Starts a new erlang node for the project that `buffer' belongs to."
  (unless buffer (setq buffer (current-buffer)))
  (edts-project-start-node (edts-project-buffer-project buffer)))

(defun edts-project-start-node (&optional project)
  "Starts a new erlang node for the project that `buffer' belongs to."
  (let* ((project-name (edts-project-name project))
         (node-name    (edts-project-node-name project))
         (buffer-name  (concat "*" project-name "*"))
         (command      (edts-project-build-project-command project))
         (pwd          (expand-file-name (edts-project-root project))))
    (edts-project-ensure-node-not-started node-name)
    (edts-project-make-comint-buffer buffer-name pwd command)
    (edts-register-node-when-ready node-name)
    (edts-project-add-node node-name buffer-name)
    (when (member 'distel features)
      (let ((node-name-symbol
             (make-symbol (concat node-name "\@" system-name))))
        (setq erl-nodename-cache node-name-symbol)))
    (get-buffer buffer-name)))

(defun edts-project-build-project-command (project)
  (let ((command (edts-project-start-command project)))
    (if command
        (delete "" (split-string command))
        (let ((path (edts-project-code-path-expand project))
              (sname (edts-project-node-name project)))
          (append (list "erl" "-sname" sname "-pa") path)))))

(defun edts-project-make-comint-buffer (buffer-name pwd command)
  "In a comint-mode buffer Starts a node with `name' in `buf-name' adding
`path' to the node's code-path using the -pa flag."
  (let* ((cmd  (car command))
         (args (cdr command)))
    (with-current-buffer (get-buffer-create buffer-name) (cd pwd))
    (apply #'make-comint-in-buffer cmd buffer-name cmd nil args)))

(defun edts-project-add-node (node-name buffer-name)
  "Add a new node to `edts-project-nodes'."
  (add-to-list 'edts-project-nodes
               (list
                (cons 'node-name   node-name)
                (cons 'buffer-name buffer-name))))

(defun edts-project-buffer-node-started-p (&optional buffer)
  "Returns non-nil if there is an edts-project erlang node started that
corresponds to 'buffer'."
  (edts-project-node-started-p (edts-project-buffer-node-name buffer)))

(defun edts-project-node-started-p (node-name)
  "Returns non-nil if there is an edts-project erlang node with name `node-name'
running on localhost."
  (edts-node-running node-name))

(defun edts-project-ensure-node-not-started (node-name)
  "Signals an error if a node of name `node-name' is running on localhost."
  (when (edts-project-node-started-p node-name)
    (error "-- Node already up. --")))

(defun edts-project-name (project)
  "Returns the name of the edts-project `project'. No default value, come on you
have to do *something* yourself!"
  (edts-project-property 'name project))

(defun edts-project-root (project)
  "Returns the root directory of the edts-project `project'."
  (edts-project-property 'root project))

(defun edts-project-lib-dirs (project)
  "Returns the edts-project `project's library directories."
  (edts-project-property 'lib-dirs project))

(defun edts-project-node-name (project)
  "Returns the edts-project `project's erlang node-name. Currently only short
names are supported."
  (or (edts-project-property 'node-sname project) (edts-project-name project)))

(defun edts-project-start-command (project)
  "Returns the edts-project `project's command for starting it's project node."
  (edts-project-property 'start-command project))

(defun edts-project-property (prop project)
  "Returns the value of the property of name prop from project."
  (cdr (assoc prop project)))

(defun edts-project-code-path-expand (project)
  "Expands `project's listed lib dirs to a full set of ebin directories,
treating every subdirectory of each lib dir a an OTP application."
  (let ((root     (edts-project-root project))
        (lib-dirs (edts-project-lib-dirs project)))
    (apply #'append
           (mapcar #'(lambda (dir)
                       (edts-project-path-expand root dir)) lib-dirs))))

(defun edts-project-path-expand (root dir)
  "Returns a list of all existing ebin directories in any folder directly
beneath `root'/`dir'."
  (setq root (edts-project-normalize-path root))
  (file-expand-wildcards (format "%s/%s/*/ebin" root dir)))

(defun edts-project-buffer-node-name (&optional buffer)
  "Returns the erlang node-name of `buffer''s edts-project node. `buffer
defaults to current-buffer."
  (unless buffer (setq buffer (current-buffer)))
  (edts-project-node-name (edts-project-buffer-project buffer)))

(defun edts-project-buffer-project (&optional buffer)
  "Returns the edts-project that `buffer' is part of, if any,
otherwise nil. If buffer is omitted, it defaults to the current buffer."
  (unless buffer (setq buffer (current-buffer)))
  (edts-project-file-project (buffer-file-name buffer)))

(defun edts-project-file-project (&optional file-name)
  "Returns the edts-project that the file with `file-name' is part of, if any,
otherwise nil. If `file-name' is omitted, it defaults to the file-name of the
current buffer."
  (unless file-name (setq file-name (buffer-file-name)))
  (find-if  #'(lambda (p) (edts-project-file-in-project-p p file-name))
            edts-projects))


(defun edts-project-file-in-project-p (project file-name)
  "Returns non-nil if the fully qualified `file-name' is located inside the
edts-project `project'."
  (file-under-path-p (edts-project-root project) file-name))

(defun file-under-path-p (path file-name)
  "Returns non-nil if the fully qualified `file-name' is located underneath
`path'."
   (string-prefix-p (edts-project-normalize-path path)
                    (expand-file-name file-name)))

(defun edts-project-normalize-path (path-str)
  "Bad name. Only replaces duplicate /'s in path-str and make sure it ends
with a /."
  (replace-regexp-in-string "//+" "/"
                            (concat (expand-file-name path-str) "/")))

(provide 'edts-project)
