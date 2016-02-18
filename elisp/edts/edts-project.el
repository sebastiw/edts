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

(require 'eproject)
(require 'eproject-extras)
(require 'f)
(require 'dash)

(require 'edts-api)
(require 'edts-shell)

;; HACKWARNING!! Avert your eyes lest you spend the rest ef your days in agony
;;
;; To avoid weird eproject types like generic-git interfering with us
;; make sure we only consider edts project types.
(defadvice eproject--all-types (around edts-eproject-types activate)
  "Ignore irrelevant eproject types for files where we should really only
consider EDTS."
  (let ((re (eproject--combine-regexps
             (cons "^\\.edts$"
                   (mapcar 'car (-filter
                                 (lambda (e) (eq 'erlang-mode (cdr e)))
                                 auto-mode-alist)))))
        (file-name (buffer-file-name)))
    ;; dired buffer has no file
    (if (and file-name
             (string-match re (f-filename file-name)))
        (setq ad-return-value '(edts-otp edts-temp edts generic))
      ad-do-it)))

;; Prevent project-file-visit-hooks from being run several times every
;; time a file is opened or reverted.
;; Without this, the project-file-visit-hook for a file will be called three
;; times. Once for `after-change-major-mode-hook' for fundamental-mode, once for
;; `after-change-major-mode-hook' for erlang-mode and once for `find-file-hook'
(remove-hook 'after-change-major-mode-hook
             'eproject--after-change-major-mode-hook)

(add-to-list 'auto-mode-alist '("\\.edts\\'" . dot-eproject-mode))

(defcustom edts-project-inhibit-conversion nil
  "If non-nil, don't convert old-style projects into .edts-files."
  :group 'edts
  :type 'boolean)

(defvar edts-project-overrides nil
  "Local overrides for project configurations")

(defconst edts-projects nil
  "Deprecated, see README for how to configure EDTS projects.")

(defun edts-project-override (root properties)
  "Add overrides for in ROOT. PROPERTIES is a plist with the properties
to set, and their new values.

Example:
 (edts-project-override \"~/my-project\" (:name \"my-project-dev\"
                                          :node-name \"my-project-dev\"))"
  (interactive)
  (let ((exp-root (file-name-as-directory (expand-file-name root)))
        (invalid (-remove #'edts-project--config-prop-p
                          (edts-project--plist-keys properties))))
    (when invalid
      (error "Invalid configuration properties:"))
    (when (eproject-attribute :name root)
      (edts-project-set-attributes exp-root properties))
    (push (cons exp-root properties) edts-project-overrides)))

(defvar edts-project-valid-properties '(:name
                                        :node-name
                                        :node-sname
                                        :lib-dirs
                                        :start-command
                                        :otp-path
                                        :app-include-dirs
                                        :project-include-dirs
                                        :erlang-cookie))

(defun edts-project--config-prop-p (prop)
  "Return non-nil if PROP is a valid keyword for edts project configurations."
  (member prop edts-project-valid-properties))

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
  ;; TODO:
  ;; This is copied from `edts-erlang-mode-regexps', can't figure out right now
  ;; how to use that variable here and expand it inside the define-project-type
  ;; macro instead of hardcoding the regexps
  :relevant-files ("^\\.erlang$"
                   "^\\.edts$"
                   "\\.app$"
                   "\\.app.src$"
                   "\\.config$"
                   "\\.erl$"
                   "\\.es$"
                   "\\.escript$"
                   "\\.eterm$"
                   "\\.script$"
                   "\\.yaws$")
  :irrelevant-files ("^\\.gitignore$"
                     "^\\.gitmodules$")
  ;; Default lib dirs
  ;; * deps: dependencies included by rebar2
  ;; * _build/test/lib: test scope dependencies from rebar3
  ;; * _build/default/lib: dependencies included by rebar3

  ;; NOTE: It's important for rebar3's test profile to be loaded
  ;; first, because rebar3 might have different contents of a specific
  ;; dependency, so it doesn't load the second copy it finds. For your
  ;; root project, that basically would mean that any supporting
  ;; modules you have for your test cases won't be loaded into EDTS,
  ;; making your test modules a sea of red squiggles.
  :lib-dirs ("lib" "deps" "_build/test/lib" "_build/default/lib"))

(defun edts-project-selector (file-name)
  "Try to figure out if FILE should be part of an edts-project."
  (edts-project-maybe-create file-name)
  (let* ((base (f-dirname file-name))
         (bestroot  (when (f-exists? (f-join base ".edts"))
                      base)))
    (f-traverse-upwards #'(lambda (p)
                            (prog1 nil
                              (when (f-exists? (f-join p ".edts"))
                                (setq bestroot p)))))
    bestroot))

(define-project-type edts-otp (generic)
  (edts-project-otp-selector file)
  :config-file nil
  :relevant-files ("^\\.erlang$"
                   "^\\.edts$"
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
  :lib-dirs ("lib/erlang/lib"))

(defun edts-project-otp-selector (file)
  "Try to figure out if FILE should be part of an otp-project."
  (when (not (edts-project-selector file))
    (let ((res (edts-project-otp-selector-path file)))
      res)))

(declare-function look-for "eproject.el")
(defun edts-project-otp-selector-path (file)
    (let ((path (look-for "bin/erl")))
      (when (and path (not (or (string= (directory-file-name path) "/bin") ;; ?
                               (string= (directory-file-name path) "/")
                               (string= (directory-file-name path) "/usr"))))
        (if (string-match "\\(.*\\)/lib/erlang[/]?$" path)
            ;; Match out lib/erlang part if we're in an install directory.
            (let ((res (match-string 1 path)))
              (edts-log-debug "edts-project-otp-selector result %s" res)
              res)
          ;; Do nothing if we're in an otp-repository.
          (edts-log-debug "edts-project-otp-selector result %s" path)
          path))))

(define-project-type edts-temp (generic)
  (edts-project-temp-selector file)
  :config-file nil
  :relevant-files ("^\\.erlang$"
                   "^\\.edts$"
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
    res))


(defun edts-project-init-buffer ()
  "Called each time a buffer inside a configured edts-project is opened."
  (edts-log-debug "Project type is: %s" (eproject-type))
  (when (edts-project--run-init-p)
    (edts-log-debug "Initializing project for %s" (current-buffer))
    (edts-api-ensure-server-started)
    (let ((root (eproject-root)))

      (when (and (boundp 'edts-projects) edts-projects)
        ;; -- Backward compatibility code --
        ;; Override the configuration of the current buffer's eproject with the
        ;; values from the corresponding entry in `edts-projects'.
        (edts-project-set-attributes root
                                     (edts-project--old-plist-by-root root)))

      ;; Local project configuration overrides. These overrides take precedence
      ;; over the ones in `edts-projects'.
      (edts-project-set-attributes root
                                   (cdr (assoc root edts-project-overrides)))

      ;; Set values of absent config parameters whose defaults are derived from
      ;; other values.
      (unless (eproject-attribute :node-name)
        (edts-project-set-attribute
         root
         :node-name (edts-project--make-node-name
                     (or (eproject-attribute :node-sname)
                         (eproject-attribute :name)))))
      (unless (eproject-attribute :start-command)
        (edts-project-set-attribute
         root
         :start-command (edts-project--make-command)))

      ;; Make necessary initializations if opened file is relevant to its
      ;; project.
      (if (edts-api-node-registeredp (eproject-attribute :node-name))
          (edts-project-node-refresh)
        (edts-project-node-init)))))
(add-hook 'edts-project-file-visit-hook 'edts-project-init-buffer)

(defun edts-project-node-init ()
  (interactive)
  ;; Ensure project node is started
  (unless (edts-api-node-started-p (eproject-attribute :node-name))
    (edts-project-start-node))
  ;; Register it with the EDTS node
  (edts-project--register-project-node))

(defun edts-project-node-refresh ()
  "Asynchronously refresh the state of current buffer's project node"
  (interactive)
  (edts-api-init-node
   (eproject-attribute :name)
   (eproject-attribute :node-name)
   (eproject-root)
   (eproject-attribute :lib-dirs)
   (eproject-attribute :app-include-dirs)
   (eproject-attribute :project-include-dirs)
   (eproject-attribute :erlang-cookie)))

(defun edts-project-init-temp ()
  "Sets up values for a temporary project when visiting a non-project module."
  (edts-log-debug "Project type is: %s" (eproject-type))
  (when (edts-project--run-init-p)
    (edts-api-ensure-server-started)
    (let* ((file (buffer-file-name))
           (root-dir (edts-project--temp-root file))
           (node-name (f-filename root-dir)))
      (edts-project-set-attribute root-dir :node-name node-name)
      (if (edts-shell-find-by-path root-dir)
          (edts-project-node-refresh)
        (edts-log-debug "Initializing temporary project node for %s"
                        (current-buffer))
        (edts-shell-make-comint-buffer
         (format "*%s*" node-name) ; buffer-name
         node-name ; node-name
         root-dir ; pwd
         (list "erl" "-sname" node-name)) ; command
        (edts-api-init-node-when-ready node-name node-name root-dir nil)))))
(add-hook 'edts-temp-project-file-visit-hook 'edts-project-init-temp)

(defun edts-project-init-otp ()
  "Sets up values for a temporary project when visiting an otp-module."
  (edts-log-debug "Project type is: %s" (eproject-type))
  (when (edts-project--run-init-p)
    (edts-api-ensure-server-started)
    (let* ((file (buffer-file-name))
           (root-dir (eproject-root))
           (node-name (format "otp-%s" (eproject-name)))
           (erl (f-join (eproject-root) "bin" "erl")))
      (edts-project-set-attribute root-dir :node-name node-name)
      (if (edts-shell-find-by-path root-dir)
          (edts-project-node-refresh)
        (edts-log-debug "Initializing otp project node for %s" (current-buffer))
        (edts-shell-make-comint-buffer
         (format "*%s*" node-name) ; buffer-name
         node-name ; node-name
         root-dir ; pwd
         (list erl "-sname" node-name)) ; command
        (edts-api-init-node-when-ready node-name node-name root-dir nil)))))
(add-hook 'edts-otp-project-file-visit-hook 'edts-project-init-otp)

(defun edts-project--run-init-p ()
  "Return non-nil if project buffer initialization code should be run."
  (and (buffer-file-name)
       (eproject-classify-file (f-filename (buffer-file-name)))))

(defun edts-project--temp-root (file)
  "Find the appropriate root directory for a temporary project for
FILE."
  (let* ((dir        (f-dirname file))
         (parent-dir (f-dirname dir)))
    (if (and (string-match "/\\(src\\|test\\)[/]?$" dir)
             (f-exists? (f-join parent-dir "ebin")))
        (file-name-as-directory parent-dir)
      (file-name-as-directory dir))))

(defun edts-project--make-command (&optional node-name)
  "Construct a default command line to start current buffer's project node."
  (let ((node-name (or node-name
		       (eproject-attribute :node-name)
		       (eproject-name))))
    (format "erl -sname %s" node-name)))

(defun edts-project--make-node-name (src)
  "Construct a default node-sname for current buffer's project node."
  (replace-regexp-in-string "[^@A-Za-z0-9_-]" "" src))

(defun edts-project-start-node ()
  "Starts a new erlang node for PROJECT."
  (let* ((buffer-name (concat "*" (eproject-name) "*"))
         (command (split-string (eproject-attribute :start-command)))
         (exec-path (edts-project-build-exec-path))
         (process-environment (edts-project-build-env))
         (node (eproject-attribute :node-name)))
    (edts-api-ensure-node-not-started node)
    (edts-shell-make-comint-buffer buffer-name node (eproject-root) command)
    (get-buffer buffer-name)))

(defun edts-project--register-project-node ()
  "Register the node of current buffer's project."
  (if (edts-api-node-registeredp (eproject-attribute :node-name))
      (edts-log-info "Re-initializing node for project %s" (eproject-name))
    (edts-log-info "Initializing node for project %s" (eproject-name)))
  (edts-api-init-node-when-ready
   (eproject-attribute :name)
   (eproject-attribute :node-name)
   (eproject-root)
   (eproject-attribute :lib-dirs)
   (eproject-attribute :app-include-dirs)
   (eproject-attribute :project-include-dirs)
   (eproject-attribute :erlang-cookie)))

(defun edts-project-build-exec-path ()
  "Build up the exec-path to use when starting the project-node of PROJECT."
  (let ((otp-path (eproject-attribute :otp-path)))
    (if otp-path
        ;; put otp-path first in path
        (cons (expand-file-name "bin" otp-path) exec-path)
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
        (f-full (f-join otp-path "bin"))
      (let ((erl (executable-find "erl")))
        (when erl
          (f-dirname erl))))))

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
    (-first
     #'(lambda (project)
         (f-same? (file-name-as-directory
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
                 (not (f-exists? (edts-project--config-file project))))
        (edts-project--create project)
        (edts-log-info "Created .edts configuration file for project: %s"
                       (cdr (assoc 'name project)))))))

(defun edts-project--file-old-project (file)
  "Return the entry in `edts-projects' that FILE belongs to, if any."
  (-first
   #'(lambda (p) (f-descendant-of? file (cdr (assoc 'root p))))
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
  (f-join (cdr (assoc 'root project)) ".edts"))

(defun edts-project-buffer-list (project-root &optional predicates)
  "Given PROJECT-ROOT, return a list of the corresponding projects open
buffers, for which all PREDICATES hold true."
  (-filter
   #'(lambda (buf)
       (with-current-buffer buf
         (and (buffer-live-p buf)
              eproject-mode
              (f-same? project-root (eproject-root))
              (-all? #'(lambda (pred) (funcall pred)) predicates))))
   (buffer-list)))


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

(provide 'edts-project)
