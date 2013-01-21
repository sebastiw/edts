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
;; Integration with Jonathan Rockway's (jon@jrock.us) eproject.

(require 'cl)
(require 'eproject)
(require 'path-util)

(setq eproject-prefer-subproject nil)
(add-to-list 'auto-mode-alist '("\\.edts\\'" . dot-eproject-mode))

;; (define-project-type edts (generic)
(define-project-type edts ()
  (progn
     (edts-project-maybe-create file)
     (look-for ".edts"))
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

(defun temp-selector (file)
  (when (and (not (look-for ".edts")) (string-match "\\.[eh]rl$" file))
    (path-util-dir-name file)))

(define-project-type edts-temp ()
;; (define-project-type edts-temp (generic)
  (temp-selector file)
  :config-file nil
  :relevant-files ("^\\.erlang$"
                   "\\.app$"
                   "\\.app.src$"
                   "\\.config$"
                   "\\.erl$"
                   "\\.es$"
                   "\\.escript$"
                   "\\.eterm$"
                   "\\.hrl$"
                   "\\.script$"
                   "\\.yaws$")
  :irrelevant-files (".gitignore"
                     ".gitmodules")
  :lib-dirs nil)

(defun edts-project-init-buffer ()
  "Called each time a buffer inside a configured edts-project is opened."
  ;; OVERRIDE the configuration of the current buffer's eproject with the
  ;; values from PROJECT. The PROJECT's `root' is assumed to already be the
  ;; same as the current eproject's, if it's not then calling this function
  ;; will most likely break something in eproject.
  (when (boundp 'edts-projects)
    (let ((project (edts-project--find-by-root (eproject-root))))
      (when project
        (edts-project-set-attributes project))))

  ;; Set values of absent config parameters whose defaults are derived from
  ;; other values.
  (unless (eproject-attribute :node-sname)
    (edts-project-set-attribute :node-sname (eproject-name)))
  (unless (eproject-attribute :start-command)
    (edts-project-set-attribute :start-command (edts-project--make-command)))

  ;; Make necessary initializations if opened file is relevant to its project.
  (when (eproject-classify-file (buffer-file-name))
    (edts-project-ensure-node-started)))
(add-hook 'edts-project-file-visit-hook 'edts-project-init-buffer)

(defun edts-project-init-temp ()
  "Sets up values for a temporary project when visiting a non-project module."
  ;; TODO handle the case when we arrive in a non-project module by navigating
  ;; from a project module, as in the case with otp-modules.
  (let* ((file (buffer-file-name))
         (root-dir (edts-project--temp-root file))
         (node-name (path-util-base-name (path-util-dir-name file))))
    (unless (edts-shell-find-by-path root-dir)
      (edts-shell-make-comint-buffer
       (format "*%s*" node-name) ; buffer-name
       node-name ; node-name
       root-dir ; pwd
       (list "erl" "-sname" node-name)) ; command
      (edts-register-node-when-ready node-name root-dir nil))
    (edts-project-set-attribute :node-sname node-name)))
(add-hook 'edts-temp-project-file-visit-hook 'edts-project-init-temp)

(defun edts-project--temp-root (file)
  "Find the appropriate root directory for a temporary project for
FILE."
  (let* ; The beam-file to look for
      ((beam-name (concat (path-util-root-base-name file) ".beam"))
       ; Look for beam-file in this directory
       (ebin-dir  (path-util-join (path-util-pop file 2) "ebin")))
    (if (file-exists-p (path-util-join ebin-dir beam-name))
        ebin-dir
      (path-util-pop file))))

(defun edts-project--make-command (&optional node-name)
  "Construct a default command line to start current buffer's project node."
  (let ((node-name (or node-name (eproject-attribute :node-sname))))
    (format "erl -sname %s" (edts-project--make-node-name node-name))))

(defun edts-project--make-node-name (src)
  "Construct a default node-sname for current buffer's project node."
  (replace-regexp-in-string "[^A-Za-z0-9_-]" "" src))

(defun edts-project-ensure-node-started ()
  "Start current-buffer's project's node if it is not already started."
  (if (edts-node-started-p (eproject-attribute :node-sname))
      (edts-register-node-when-ready
       (eproject-attribute :node-sname)
       (eproject-root)
       (eproject-attribute :lib-dirs))
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
    (edts-register-node-when-ready
     (eproject-attribute :node-sname)
     (eproject-root)
     (eproject-attribute :lib-dirs))
    (get-buffer buffer-name)))

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

(defun edts-project--find-by-root (root)
  "Returns the entry from `edts-projects' whose `root' equal ROOT after
they are both expanded."
  (let ((exp-root (expand-file-name root)))
    (find-if
     #'(lambda (project)
         (string= (file-name-as-directory
                   (expand-file-name
                    (cdr (assoc 'root project))))
                  exp-root))
     edts-projects)))

(defun edts-project-set-attribute (attr val)
  "Set current buffer's project's value of ATTR to VAL."
  (edts-project-set-attributes (list (cons attr val))))

(defun edts-project-set-attributes (attrs)
  "ATTRS is an alist of (ATTR . VAL). For each element in ATTRS, set
current buffer's project's value of ATTR to VAL. ATTR can be either a
keyword, or a symbol, in which case it will be converted to a keyword."
  ;; This function is really dirty but I can't think of a better way to do it.
  (let* ((root       (eproject-root))
         (el         (assoc root eproject-attributes-alist))
         (old-attrs  (cdr el)))
    (loop for (k . v) in attrs do
          (unless (keywordp k)
            (setq k (intern (format ":%s" k))))
          (setq old-attrs (plist-put old-attrs k v)))
    (setq eproject-attributes-alist (delq el eproject-attributes-alist))
    (push (cons root old-attrs) eproject-attributes-alist)))

(defun edts-project-maybe-create (file)
  "Automatically creates a .edts-file from a an old-style project
definition if `edts-projects' is bound and, FILE is inside one of its
projects and there is no previous .edts-file."
  (when (boundp 'edts-projects)
    (let ((project (edts-project--file-override-project file)))
      (when (and project
                 (not (file-exists-p (edts-project--config-file project))))
        (edts-project--create project)
        (edts-log-info "Created .edts configuration file for project: ~p"
                       (cdr (assoc 'name project)))))))

(defun edts-project--file-override-project (file)
  "Return the entry in `edts-projects' that FILE belongs to, if any."
  (find-if
   #'(lambda (p) (path-util-file-in-dir-p file (cdr (assoc 'root p))))
   edts-projects))

(defun edts-project--create (project)
  (with-temp-file (edts-project--config-file project)
    (loop for field in project do
          (if (listp (cdr field))
              (insert (format ":%s '%S\n" (car field) (cdr field)))
            (insert (format ":%s %S\n" (car field) (cdr field)))))))

(defun edts-project--config-file (project)
  "Return the path to projects eproject configuration file."
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
