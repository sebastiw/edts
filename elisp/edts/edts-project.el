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
  :irrelevant-files ("^\\.edts$"
                     "^\\.gitignore$"
                     "^\\.gitmodules$")
  :lib-dirs ("lib"))

(defun edts-project-selector (file-name)
  "Try to figure out if FILE should be part of an edts-project."
  (edts-project-maybe-create file-name)
  (let (prev-root
        (cur-root (path-util-pop file-name))
        bestroot)
    (while (and cur-root (not (string= prev-root cur-root)))
      (setq prev-root cur-root)
      (setq cur-root (path-util-pop cur-root))
      (when (file-exists-p (path-util-join cur-root ".edts"))
        (setq bestroot cur-root)))
    (edts-log-debug "edts-project-selector result: %s" bestroot)
    bestroot))

(define-project-type edts-otp (edts)
  (edts-project-otp-selector file)
  :config-file nil
  :lib-dirs ("lib/erlang/lib"))

(defun edts-project-otp-selector (file)
  "Try to figure out if FILE should be part of an otp-project."
  (when (not (edts-project-selector file))
    (let ((res (edts-project-otp-selector-path file)))
      (edts-log-debug "edts-project-otp selector result: %s" res)
      res)))

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
  (let ((res (when (and
                    ;; otp-selector also checks that the normal project selector
                    ;; returns nil
                    (not (edts-project-selector file))
                    (not (edts-project-otp-selector file))
                    (string-match "\\.[eh]rl$" file))
               (edts-project--temp-root file))))
    (edts-log-debug "edts-project-otp-selector result: %s" res)
    res))


(defun edts-project-init-buffer ()
  "Called each time a buffer inside a configured edts-project is opened."
  (edts-log-debug "Initializing project for %s" (current-buffer))
  (edts-ensure-server-started)
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
      (if (edts-node-registeredp (eproject-attribute :node-sname))
          (edts-project-node-refresh)
      (edts-project-node-init)))))
  (add-hook 'edts-project-file-visit-hook 'edts-project-init-buffer)

(defun edts-project-node-init ()
  (interactive)
  (save-window-excursion
    (with-output-to-temp-buffer "EDTS Project"
      (edts-project--init-output-buffer)
      ;; Ensure project node is started
      (unless (edts-node-started-p (eproject-attribute :node-sname))
        (edts-project--display "Starting project node for %s\n"
                               (eproject-root))
        (edts-project-start-node))
      ;; Register it with the EDTS node
      (edts-project--register-project-node)
      (sleep-for 1))
      (edts-project--kill-output-buffer)))

(defun edts-project-node-refresh ()
  "Asynchronously refresh the state of current buffer's project node"
  (interactive)
  (edts-init-node-async
   (eproject-attribute :name)
   (eproject-attribute :node-sname)
   (eproject-root)
   (eproject-attribute :lib-dirs)
   (eproject-attribute :app-include-dirs)
   (eproject-attribute :project-include-dirs)))

(defun edts-project--init-output-buffer ()
  (with-current-buffer "EDTS Project"
    (erase-buffer))
  (display-buffer "EDTS Project")
  (redisplay))

(defun edts-project--kill-output-buffer ()
  (kill-buffer "EDTS Project"))

(defun edts-project--display (fmt &rest args)
  (princ (format fmt args))
  (redisplay))

(defun edts-project-init-temp ()
  "Sets up values for a temporary project when visiting a non-project module."
  (edts-log-debug "Initializing temporary project for %s" (current-buffer))
  (edts-ensure-server-started)
  (let* ((file (buffer-file-name))
         (root-dir (edts-project--temp-root file))
         (node-name (path-util-base-name root-dir)))
    (unless (edts-shell-find-by-path root-dir)
      (edts-shell-make-comint-buffer
       (format "*%s*" node-name) ; buffer-name
       node-name ; node-name
       root-dir ; pwd
       (list "erl" "-sname" node-name))) ; command
    (edts-init-node-when-ready node-name node-name root-dir nil)
    (edts-project-set-attribute root-dir :node-sname node-name)))
(add-hook 'edts-temp-project-file-visit-hook 'edts-project-init-temp)

(defun edts-project-init-otp ()
  "Sets up values for a temporary project when visiting an otp-module."
  (edts-log-debug "Initializing otp project for %s" (current-buffer))
  (edts-ensure-server-started)
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
    (edts-init-node-when-ready node-name node-name root-dir nil)
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

(defun edts-project-start-node ()
  "Starts a new erlang node for PROJECT."
  (let* ((buffer-name (concat "*" (eproject-name) "*"))
         (command (split-string (eproject-attribute :start-command)))
         (exec-path (edts-project-build-exec-path))
         (process-environment (edts-project-build-env))
         (node (eproject-attribute :node-sname)))
    (edts-ensure-node-not-started node)
    (edts-shell-make-comint-buffer buffer-name node (eproject-root) command)
    (get-buffer buffer-name)))

(defun edts-project--register-project-node ()
  "Register the node of current buffer's project."
  (if (edts-node-registeredp (eproject-attribute :node-sname))
      (edts-project--display "Re-initializing project node for %s. Please wait..."
                             (eproject-root))
    (edts-project--display "Initializing project node for %s. Please wait..."
                           (eproject-root)))
  (if (edts-init-node-when-ready
       (eproject-attribute :name)
       (eproject-attribute :node-sname)
       (eproject-root)
       (eproject-attribute :lib-dirs)
       (eproject-attribute :app-include-dirs)
       (eproject-attribute :project-include-dirs))
      (edts-project--display "Done.")
    (edts-project--display "Error.")))

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
        (edts-log-info "Created .edts configuration file for project: %s"
                       (cdr (assoc 'name project)))))))

(defun edts-project--file-old-project (file)
  "Return the entry in `edts-projects' that FILE belongs to, if any."
  (find-if
   #'(lambda (p) (path-util-file-in-dir-p file (cdr (assoc 'root p))))
   edts-projects))

(defun edts-project--create (project)
  "Create the .edts configuration file for PROJECT in its root directory."
    (edts-project-write-config (edts-project--config-file project)
                               (edts-project--to-config project)))


(defun edts-project--to-config (project)
  "Convert an old-style project spec into on an edts-config."
  (apply #'append
         (loop for     (k . v) in project
               for     prop = (intern (format ":%s" k))
               when    (edts-project--config-prop-p prop)
               collect (list prop v))))

(defun edts-project-write-config (file config)
  "Write CONFIG to FILE."
  (with-temp-file file
    (loop for (k v . rest) on config by #'cddr
          do  (if (listp v)
                  (insert (format "%s '%S\n" k v))
                (insert (format "%s %S\n" k v)))))
    config)

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

;;;;;;;;;;;;;;;;;;;;
;; Tests

(when (member 'ert features)

  (require 'edts-test)
  (edts-test-add-suite
   ;; Name
   edts-project-suite
   ;; Setup
   (lambda ()
     (edts-test-cleanup-all-buffers)
     (edts-test-setup-project edts-test-project1-directory
                              "test"
                              nil))
   ;; Teardown
   (lambda (setup-config)
     (edts-test-teardown-project edts-test-project1-directory)))


  (edts-test-case edts-project-suite edts-project-basic-test ()
    "Basic project setup test"
    (let ((eproject-prefer-subproject t))
      (find-file (car (edts-test-project1-modules)))
      (should (file-exists-p
               (path-util-join edts-test-project1-directory ".edts")))
      (should (string= "test" (eproject-name)))
      (should (get-buffer"*edts*"))
      (should (get-buffer"*test*"))))

  (edts-test-case edts-project-suite edts-project-selector-test ()
    "Test that the in the case of multiple levels of projects, the super
project is selected as the root, and other project types such as git-generic
are not considered for erl-files."
    ;; Setup

    ;; Assume that .git exists in edts directory (Yes, this is generally a
    ;; stupid idea, but I'm feeling lazy right now).
    (should (file-exists-p
             (path-util-join (path-util-pop edts-test-project1-directory 2)
                             ".git")))
    (edts-test-setup-project (path-util-join edts-test-project1-directory
                                             "lib"
                                             "one")
                             "test-dep"
                             nil)

    ;; There! Now we have a subproject called test-dep in
    ;; `edts-test-project1-directory'/lib/one, a super project in
    ;; `edts-test-project1-directory' and a git-project in
    ;; `edts-test-project1-directory'/../..
    ;; This test ensures that for
    ;; `edts-test-project1-directory'/lib/one/src/one.erl, we choose
    ;; `edts-test-project1-directory' as the project root.
    (find-file (car (edts-test-project1-modules)))
    (message "current-buffer %s %s" (current-buffer) (eproject-root))
    (should (string= (path-util-normalize edts-test-project1-directory)
                     (path-util-normalize (eproject-root)))))
)
