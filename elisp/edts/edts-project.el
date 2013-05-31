;;; edts-project.el ---  Integration with Jonathan Rockway's eproject package.

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

(require 'cl)
(require 'eproject)
(require 'eproject-extras)
(require 'path-util)

(setq eproject-prefer-subproject nil)
(add-to-list 'auto-mode-alist '("\\.edts\\'" . dot-eproject-mode))

(defcustom edts-project-inhibit-conversion nil
  "If non-nil, don't convert old-style projects into .edts-files."
  :group 'edts
  :type 'boolean)

(defvar edts-project-overrides nil
  "Local overrides for project configurations")

(defun edts-project-override (root properties)
  "Add overrides for in ROOT. PROPERTIES is a plist with the properties
to set, and their new values.

Example:
 (edts-project-override \"~/my-project\" (:name \"my-project-dev\"
                                          :node-sname \"my-project-dev\"))"
  (interactive)
  (let ((exp-root (file-name-as-directory (expand-file-name root)))
        (invalid (delete-if #'edts-project--config-prop-p
                            (edts-project--plist-keys properties))))
    (when invalid
      (error "Invalid configuration properties:"))
    (when (eproject-attribute :name root)
      (edts-project-set-attributes exp-root properties))
    (push (cons exp-root properties) edts-project-overrides)))

(defun edts-project--config-prop-p (prop)
  "Return non-nil if PROP is a valid keyword for edts project configurations."
  (let ((valid '(:name
                 :node-sname
                 :lib-dirs
                 :start-command
                 :otp-path
                 :dialyzer-plt
                 :app-include-dirs
                 :project-include-dirs)))
    (member prop valid)))

(defun edts-project--plist-keys (plist)
  "Return all the keys of PLIST."
  (let ((ret nil))
    (while plist
      (unless (keywordp (car plist))
        (error "Invalid config plist"))
      (push (car plist) ret)
      (setq plist (cddr plist)))
    (reverse ret)))

(define-project-type edts (generic)
  (edts-project-selector file)
  :config-file ".edts"
  :relevant-files ("^\\.erlang$"
                   "\\.app$"
                   "\\.app.src$"
                   "\\.config$"
                   "\\.erl$"
                   "\\.es$"
                   "\\.escript$"
                   "\\.eterm$"
                   "\\.script$"
                   "\\.yaws$")
  :irrelevant-files (".edts"
                     ".gitignore"
                     ".gitmodules")
  :lib-dirs ("lib"))

(defun edts-project-selector (file)
  "Try to figure out if FILE should be part of an edts-project."
  (edts-project-maybe-create file)
  (look-for ".edts"))

(define-project-type edts-otp (edts)
  (edts-project-otp-selector file)
  :config-file nil
  :lib-dirs ("lib/erlang/lib"))

(defun edts-project-otp-selector (file)
  "Try to figure out if FILE should be part of an otp-project."
  (when (not (edts-project-selector file))
    (edts-project-otp-selector-path file)))

(defun edts-project-otp-selector-path (file)
    (let ((path (look-for "bin/erl")))
      (when (and path (not (or (string= (directory-file-name path) "/bin") ;; ?
                               (string= (directory-file-name path) "/")
                               (string= (directory-file-name path) "/usr"))))
        (if (string-match "\\(.*\\)/lib/erlang[/]?$" path)
            ;; Match out lib/erlang part if we're in an install directory.
            (match-string 1 path)
          ;; Do nothing if we're in an otp-repository.
          path))))

(define-project-type edts-temp (edts)
  (edts-project-temp-selector file)
  :config-file nil
  :lib-dirs nil)

(defun edts-project-temp-selector (file)
  "Try to figure out if FILE should be part of a temp-project."
  (when (and
         ;; otp-selector also checks that the normal project selector returns
         ;; nil
         (not (edts-project-selector file))
         (not (edts-project-otp-selector file))
         (string-match "\\.[eh]rl$" file))
    (edts-project--temp-root file)))


(defun edts-project-init-buffer ()
  "Called each time a buffer inside a configured edts-project is opened."
  (edts-log-debug "Initializing project for %s" (current-buffer))
  (let ((root (eproject-root)))

    (when (boundp 'edts-projects)
      ;; -- Backward compatibility code --
      ;; Override the configuration of the current buffer's eproject with the
      ;; values from the corresponding entry in `edts-projects'.
      (edts-project-set-attributes root (edts-project--old-plist-by-root root)))

    ;; Local project configuration overrides. These overrides take precedence
    ;; over the ones in `edts-projects'.
    (edts-project-set-attributes root (cdr (assoc root edts-project-overrides)))

    ;; Set values of absent config parameters whose defaults are derived from
    ;; other values.
    (unless (eproject-attribute :node-sname)
      (edts-project-set-attribute
       root
       :node-sname (eproject-name)))
    (unless (eproject-attribute :start-command)
      (edts-project-set-attribute
       root
       :start-command (edts-project--make-command)))

    ;; Make necessary initializations if opened file is relevant to its project.
    (when (and (buffer-file-name) (eproject-classify-file (buffer-file-name)))
      (edts-project-ensure-node-started))))
  (add-hook 'edts-project-file-visit-hook 'edts-project-init-buffer)

(defun edts-project-init-temp ()
  "Sets up values for a temporary project when visiting a non-project module."
  (edts-log-debug "Initializing temporary project for %s" (current-buffer))
  (let* ((file (buffer-file-name))
         (root-dir (edts-project--temp-root file))
         (node-name (path-util-base-name root-dir)))
    (unless (edts-shell-find-by-path root-dir)
      (edts-shell-make-comint-buffer
       (format "*%s*" node-name) ; buffer-name
       node-name ; node-name
       root-dir ; pwd
       (list "erl" "-sname" node-name))) ; command
    (edts-register-node-when-ready node-name root-dir nil)
    (edts-project-set-attribute root-dir :node-sname node-name)))
(add-hook 'edts-temp-project-file-visit-hook 'edts-project-init-temp)

(defun edts-project-init-otp ()
  "Sets up values for a temporary project when visiting an otp-module."
  (edts-log-debug "Initializing otp project for %s" (current-buffer))
  (let* ((file (buffer-file-name))
         (root-dir (eproject-root))
         (node-name (format "otp-%s" (eproject-name)))
         (erl (path-util-join (eproject-root) "bin/erl")))
    (unless (edts-shell-find-by-path root-dir)
      (edts-shell-make-comint-buffer
       (format "*%s*" node-name) ; buffer-name
       node-name ; node-name
       root-dir ; pwd
       (list erl "-sname" node-name))) ; command
    (edts-register-node-when-ready node-name root-dir nil)
    (edts-project-set-attribute root-dir :node-sname node-name)))
(add-hook 'edts-otp-project-file-visit-hook 'edts-project-init-otp)


(defun edts-project--temp-root (file)
  "Find the appropriate root directory for a temporary project for
FILE."
  (let* ((dir        (path-util-dir-name file))
         (parent-dir (path-util-dir-name dir)))
    (if (and (string-match "/src[/]?$" dir)
             (file-exists-p (path-util-join parent-dir "ebin")))
        (file-name-as-directory parent-dir)
      (file-name-as-directory dir))))

(defun edts-project--make-command (&optional node-name)
  "Construct a default command line to start current buffer's project node."
  (let ((node-name (or node-name
		       (eproject-attribute :node-sname)
		       (eproject-name))))
    (format "erl -sname %s" (edts-project--make-node-name node-name))))

(defun edts-project--make-node-name (src)
  "Construct a default node-sname for current buffer's project node."
  (replace-regexp-in-string "[^A-Za-z0-9_-]" "" src))

(defun edts-project-ensure-node-started ()
  "Start current-buffer's project's node if it is not already started."
  (if (edts-node-started-p (eproject-attribute :node-sname))
      (edts-project--register-project-node)
    (edts-project-start-node)))

(defun edts-project-start-node ()
  "Starts a new erlang node for PROJECT."
  (let* ((buffer-name (concat "*" (eproject-name) "*"))
         (command (split-string (eproject-attribute :start-command)))
         (exec-path (edts-project-build-exec-path))
         (process-environment (edts-project-build-env))
         (node (eproject-attribute :node-sname)))
    (edts-ensure-node-not-started node)
    (edts-shell-make-comint-buffer buffer-name node (eproject-root) command)
    (edts-project--register-project-node)
    (get-buffer buffer-name)))

(defun edts-project--register-project-node ()
  "Register the node of current buffer's project."
  (edts-register-node-when-ready
   (eproject-attribute :node-sname)
   (eproject-root)
   (eproject-attribute :lib-dirs)
   (eproject-attribute :app-include-dirs)
   (eproject-attribute :project-include-dirs)))

(defun edts-project-build-exec-path ()
  "Build up the exec-path to use when starting the project-node of PROJECT."
  (let ((otp-path (eproject-attribute :otp-path)))
    (if otp-path
        (cons (concat otp-path "/bin") exec-path) ;; put otp-path first in path
      exec-path)))

(defun edts-project-build-env ()
  "Build up the PATH environment variable to use when starting current-
buffer's project-node and return the resulting environment."
  (let* ((bin-dir  (edts-project--otp-bin-path))
         (path-var (concat "PATH=" bin-dir path-separator (getenv "PATH"))))
    (cons path-var process-environment)))

(defun edts-project--otp-bin-path ()
  "Return the otp bin-path of current-buffer's project or, if that is
not defined, the first directory in the `exec-path' that contains a file
named erl."
  (let ((otp-path (eproject-attribute :otp-path)))
    (if otp-path
        (path-util-join otp-path "bin" :expand t)
      (let ((erl (executable-find "erl")))
        (when erl
          (path-util-dir-name erl))))))

(defun edts-project--old-plist-by-root (root)
  "Finds the entry from `edts-projects' whose `root' equals ROOT after
they are both expanded and converts it into a plist."
  (let ((project (edts-project--find-old-by-root root)))
    (loop for (k . v) in project append
          (when (edts-project--config-prop-p k)
            (list (intern (format ":%s" k)) v)))))

(defun edts-project--find-old-by-root (root)
  "Returns the entry from `edts-projects' whose `root' equal ROOT after
they are both expanded."
  (let ((exp-root (file-name-as-directory (expand-file-name root))))
    (find-if
     #'(lambda (project)
         (string= (file-name-as-directory
                   (expand-file-name
                    (cdr (assoc 'root project))))
                  exp-root))
     edts-projects)))

(defun edts-project-set-attribute (root attr val)
  "Set current buffer's project's value of ATTR to VAL."
  (edts-project-set-attributes root (list attr val)))

(defun edts-project-set-attributes (root attrs)
  "ATTRS is an alist of (ATTR . VAL). For each element in ATTRS, set
current buffer's project's value of ATTR to VAL. ATTR can be either a
keyword, or a symbol, in which case it will be converted to a keyword."
  ;; This function is really dirty but I can't think of a better way to do it.
  (let* ((el         (assoc root eproject-attributes-alist))
         (old-attrs  (cdr el)))
    (loop for (k v) on attrs by #'cddr do
          (if (edts-project--config-prop-p k)
                (setq old-attrs (plist-put old-attrs k v))
            (edts-log-error "invalid configuration property %s" k)))
    (setq eproject-attributes-alist (delq el eproject-attributes-alist))
    (push (cons root old-attrs) eproject-attributes-alist)))

(defun edts-project-maybe-create (file)
  "Automatically creates a .edts-file from a an old-style project
definition if `edts-projects' is bound and, FILE is inside one of its
projects and there is no previous .edts-file."
  (when (and (boundp 'edts-projects)
             (not edts-project-inhibit-conversion))
    (let ((project (edts-project--file-old-project file)))
      (when (and project
                 (not (file-exists-p (edts-project--config-file project))))
        (edts-project--create project)
        (edts-log-info "Created .edts configuration file for project: ~p"
                       (cdr (assoc 'name project)))))))

(defun edts-project--file-old-project (file)
  "Return the entry in `edts-projects' that FILE belongs to, if any."
  (find-if
   #'(lambda (p) (path-util-file-in-dir-p file (cdr (assoc 'root p))))
   edts-projects))

(defun edts-project--create (project)
  "Create the .edts configuration file for PROJECT in its root directory."
  (with-temp-file (edts-project--config-file project)
    (loop for (k . v) in project do
          (when (edts-project--config-prop-p (intern (format ":%s" k)))
            (if (listp v)
                (insert (format ":%s '%S\n" k v)) ;; quote value if list
              (insert (format ":%s %S\n" k v)))))))

(defun edts-project--config-file (project)
  "Return the path to PROJECT's eproject configuration file."
  (path-util-join (cdr (assoc 'root project)) ".edts"))

(defun edts-project-buffer-list (project-root &optional predicates)
  "Given PROJECT-ROOT, return a list of the corresponding projects open
buffers, for which all PREDICATES hold true."
  (reduce
   #'(lambda (acc buf)
       (with-current-buffer buf
         (if (and (buffer-live-p buf)
                  eproject-mode
                  (string= project-root (eproject-root))
                  (every #'(lambda (pred) (funcall pred)) predicates))
             (cons buf acc)
           acc)))
   (buffer-list)
   :initial-value nil))

(defun edts-project-buffer-map (project-root function)
  "Return the result of running FUNCTION inside each buffer in PROJECT-ROOT."
  (let ((res nil))
    (with-each-buffer-in-project (gen-sym) project-root
      (push (funcall function) res))
    (reverse res)))


;;;;;;;;;;;;;;;;;;;;
;; Commands

(defun edts-project-revert-all-buffers ()
  "Revert all buffers belonging to current buffer's project. Ignores
auto-save data."
  (interactive)
  (when (y-or-n-p (format "Revert all buffers in %s" (eproject-name)))
    (with-each-buffer-in-project (gensym) (eproject-root)
      (revert-buffer t t t))))
