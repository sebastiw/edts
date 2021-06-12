;; edts-project.el --- Bare bones project management
;;
;; Copyright 2012-2017 Thomas Järvstrand <tjarvstrand@gmail.com>
;;
;; Author: Thomas Järvstrand <thomas.jarvstrand@gmail.com>
;; Keywords: erlang
;; This file is not part of GNU Emacs.
;;
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

(require 'f)
(require 'dash)

(require 'edts-alist)

(add-to-list 'auto-mode-alist '("\\.edts\\'" . emacs-lisp-mode))

(defvar edts-project-root nil
  "The root of the current buffer's project")
(make-variable-buffer-local 'edts-project-root)

(defvar edts-project-overrides nil
  "Local overrides for project configurations")

(defvar edts-project-attributes nil
  "Alist of current attributes for all open projects")

(defvar edts-project-roots nil
  "List of directories that are to be considered the top level of a project.
Use this if you want to override the behaviour where EDTS considers all projects
underneath a project root to be subprojects of that super project.")

(defun edts-project-name (&optional root)
  "Return current buffer's project name."
  (edts-project-attribute :name root))

(defun edts-project-attribute (key &optional root)
  (let ((root (or root (edts-project-root))))
    (edts-alist-get-in (list root key) edts-project-attributes)))

(defun edts-project-set-attribute (key value &optional root)
  (let ((root (or root (edts-project-root))))
    (!edts-alist-store-in (list root key) value 'edts-project-attributes)))

(defun edts-project--find-root (&optional dir)
  "Return the current buffer's project root."
  (when (f-this-file)
    (let ((dir (or dir (f-dirname (f-this-file)))))
      (or (edts-project--find-project-root dir)
          (edts-project--find-otp-root dir)
          (edts-project--find-rebar-root dir)
          (edts-project--find-temp-root dir)
          dir))))

(defun edts-project--find-project-root (dir)
  "Try to find the top-most edts-file above current buffer's file."
  (let (root
	(roots (-map 'f-slash edts-project-roots)))
    (f-traverse-upwards (lambda (path)
			  (when (or (f-file? (f-join path ".edts"))
				    (-contains? roots path))
			    (setq root path))
			  (-contains? roots path))
			(f-expand dir))
    root))

(defun edts-project--find-rebar-root (dir)
  "Try find the top-most rebar.config file or _build directory"
  (edts-traverse-downwards (lambda (path)
                             (or (f-file? (f-join path "rebar.config"))
				 (f-file? (f-join path "rebar.lock"))
				 (f-directory? (f-join path "_build"))))
			   (f-expand dir)))

(defun edts-project--find-otp-root (dir)
  (f-traverse-upwards (lambda (path)
                        (when (not (f-root? path))
                          (f-file? (f-join path "bin" "erl"))))
                      (f-expand dir)))

(defun edts-project--find-temp-root (dir)
  "Find the appropriate root directory for a temporary project for
FILE."
  (f-traverse-upwards (lambda (path)
                            (or (f-directory? (f-join (f-dirname dir) "ebin"))
                                (f-directory? (f-join (f-dirname dir) "_build"))
                                (f-directory? (f-join (f-dirname dir) "src"))
                                (f-directory? (f-join (f-dirname dir) "include"))))
                          (f-expand dir)))

(defun edts-traverse-downwards (fn &optional path)
  "Similar to `f-traverse-upwards' but other direction.

Traverse down PATH as long as FN return nil, ending at PATH."
  (unless path
    (setq path default-directory))
  (when (f-relative? path)
    (setq path (f-expand path)))
  (edts-traverse-downwards-prim fn (f-root) (f-split path)))

(defun edts-traverse-downwards-prim (fn cur parts)
  (if (eq (length parts) 0)
      cur
    (let ((cur (f-join cur (car parts)))
	  (parts (cdr parts)))
      (if (funcall fn cur)
	  cur
	(edts-traverse-downwards-prim fn cur parts)))))

(defun edts-project-root ()
  (unless edts-project-root
    (setq edts-project-root (edts-project--find-root)))
  edts-project-root)

(defun edts-project-refresh-root ()
  (setq edts-project-root nil)
  (edts-project--find-root))

(defun edts-project-init (&optional dir)
  "Initializes EDTS in DIR. DIR defaults to the directory of the current
buffer's file."
  (let ((dir (or dir (f-dirname (buffer-file-name)))))
    (or (edts-project--init-project dir)
        (edts-project--init-otp-project dir)
	(edts-project--init-rebar-project dir)
        (edts-project--init-temp-project dir))))

(defun edts-project--init-project (dir)
  "Initializes the EDTS project in ROOT."
  (-when-let (root (edts-project--find-project-root dir))
    (!edts-alist-store root
                       (-> (edts-project--build-project-config root)
                           edts-project--derive-attributes
                           edts-project--validate-config)
                       'edts-project-attributes)))

(defun edts-project--build-project-config (root)
  (edts-alist-merge (edts-project--config-default root)
                    (edts-project--config-from-file root)
                    (edts-project--config-overrides root)
                    '((:type . :project))))

(defun edts-project--config-default (root)
  (let ((name (f-filename root)))
    `((:name       . ,name)
      (:node-sname . ,name)
      ;; Default lib dirs
      ;; * deps: dependencies included by rebar2
      ;; * _build/test/lib: test scope dependencies from rebar3
      ;; * _build/default/lib: dependencies included by rebar3
      ;; * _checkouts: local dependencies included by rebar3

      ;; NOTE: It's important for rebar3's test profile to be loaded
      ;; first, because rebar3 might have different contents of a specific
      ;; dependency, so it doesn't load the second copy it finds. For your
      ;; root project, that basically would mean that any supporting
      ;; modules you have for your test cases won't be loaded into EDTS,
      ;; making your test modules a sea of red squiggles.
      (:lib-dirs . ("lib"
                    "deps"
                    "_checkouts"
                    ;; Probably use f-expand.
                    ,(f-join "_build" "test" "lib")
                    ,(f-join "_build" "default" "lib")
                    )))))

(defun edts-project--init-rebar-project (dir)
  "Initializes the EDTS rebar3 project in ROOT."
  (-when-let (root (edts-project--find-rebar-root dir))
    (!edts-alist-store root
                       (-> (edts-project--build-rebar-config root)
                           edts-project--derive-attributes
                           edts-project--validate-config)
                       'edts-project-attributes)))

(defun edts-project--build-rebar-config (root)
  (edts-alist-merge (edts-project--config-default-rebar root)
                    (edts-project--config-from-file root)
                    (edts-project--config-overrides root)
                    '((:type . :project))))

(defun edts-project--config-default (root)
  (let ((name (f-filename root)))
    `((:name       . ,name)
      (:node-sname . ,name)
      ;; Default lib dirs
      ;; * deps: dependencies included by rebar2
      (:lib-dirs . ("lib"
                    "deps"
                    )))))

(defun edts-project--config-default-rebar (root)
  (let ((name (f-filename root)))
    `((:name       . ,name)
      (:node-sname . ,name)
      ;; Default lib dirs
      ;; * _build/test/lib: test scope dependencies from rebar3
      ;; * _build/default/lib: dependencies included by rebar3
      ;; * _checkouts: local dependencies included by rebar3

      ;; NOTE: It's important for rebar3's test profile to be loaded
      ;; first, because rebar3 might have different contents of a specific
      ;; dependency, so it doesn't load the second copy it finds. For your
      ;; root project, that basically would mean that any supporting
      ;; modules you have for your test cases won't be loaded into EDTS,
      ;; making your test modules a sea of red squiggles.
      (:lib-dirs . ("_checkouts"
		    ,(f-join "_build" "test" "lib")
                    ,(f-join "_build" "default" "lib")
                    )))))

(defun edts-project--init-otp-project (dir)
  (-when-let (root (edts-project--find-otp-root dir))
    (!edts-alist-store root
                       (-> (edts-project--build-otp-config root)
                           edts-project--derive-attributes
                           edts-project--validate-config)
                       'edts-project-attributes)))

(defun edts-project--build-otp-config (root)
  (edts-alist-merge (edts-project--config-default-otp root)
                    (edts-project--config-overrides root)
                    '((:type . :otp))))

(defun edts-project--config-default-otp (root)
  (edts-alist-map (lambda (k v)
                    (if (-contains? '(:name :node-sname) k)
                        (cons k (format "otp-%s" v))
                      (cons k v)))
                  (edts-project--config-default root)))

(defun edts-project--init-temp-project (dir)
  (-when-let (root (edts-project--find-temp-root dir))
    (!edts-alist-store root
                       (-> (edts-project--build-temp-config root)
                           edts-project--derive-attributes
                           edts-project--validate-config)
                       'edts-project-attributes)))

(defun edts-project--build-temp-config (root)
  (edts-alist-merge (edts-project--config-default root)
                    (edts-project--config-overrides root)
                    '((:type . :temp))))


(defun edts-project--derive-attributes (config)
  (-> config
      edts-project--derive-node-name
      edts-project--derive-start-command))

(defun edts-project--derive-node-name (config)
  (edts-alist-ensure :node-name (edts-alist-get :node-sname config) config))

(defun edts-project--derive-start-command (config)
  (let ((node-name (edts-alist-get :node-name config)))
    (edts-alist-ensure :start-command
                       (format "erl -sname %s" node-name)
                       config)))

(defun edts-project--config-from-file (root)
  "Read config from FILE and return it's attributes."
  (let ((file (f-join root ".edts")))
    (when (f-file? file)
      (let* ((file-contents
              (with-temp-buffer
                (insert-file-contents file)
                (buffer-substring-no-properties (point-min) (point-max)))))
        (-if-let (unsafe (unsafep file-contents))
            (warn "%s contains unsafe data (%s), ignoring!" file unsafe)
          (edts-project--config-from-string file-contents))))))

(defun edts-project--config-overrides (root)
  (edts-alist-get root edts-project-overrides))

(defun edts-project--validate-config (config)
  (-when-let* ((invalid      (-remove #'edts-project--valid-property? config))
               (invalid-keys (--map (car it) invalid)))
    (error (format "Invalid configuration properties: %s" invalid)))
  config)

(defvar edts-project-valid-properties
  '((:app-include-dirs . edts-project--string-list?)
    (:erlang-cookie . stringp)
    (:lib-dirs . edts-project--string-list?)
    (:name . stringp)
    (:node-name . stringp)
    (:node-sname . stringp)
    (:otp-path . stringp)
    (:project-include-dirs . edts-project--string-list?)
    (:start-command . stringp)
    (:type . symbolp)))

(defun edts-project--string-list? (value)
  (and (listp value)
       (-all? 'stringp value)))

(defun edts-project--valid-property? (prop)
  "Return non-nil if PROP is a valid keyword for edts project configurations."
  (-when-let (validator (edts-alist-get (car prop)
                                        edts-project-valid-properties))
    (funcall validator (cdr prop))))

(defun edts-project-write-config (file config)
  "Write CONFIG to FILE."
  (with-temp-file file
    (insert (edts-project--config-to-string config))))

(defun edts-project--config-to-string (config)
  (s-join "\n"
          (edts-alist-map (lambda (k v)
                            (if (listp v)
                                (format "%s '%S" k v)
                              (format "%s %S" k v)))
                          config)))

(defun edts-project--config-from-string (config)
  (edts-alist-from-plist (eval (read (format "(list %s)" config)))))

(defun edts-project-buffers (&optional root)
  "Return a list of all open buffers in the project ROOT. ROOT defaults to the
current buffer's project root."
  (let ((root (or root (edts-project-root))))
    (-select (lambda (buf)
               (with-current-buffer buf
                 (-when-let (buf-root (edts-project-root))
                   (equal root buf-root))))
             (buffer-list))))

(defun edts-project-in-each-buffer (fn &optional root)
  "Evaluate FN in each buffer in the project ROOT. ROOT defaults to the current
buffer's project root."
  (-map (lambda (buf)
          (with-current-buffer buf (funcall fn)))
        (edts-project-buffers root)))

;;;;;;;;;;;;;;;;;;;;
;; Commands

(defun edts-project-override (root properties)
  "Add overrides for in ROOT. PROPERTIES is a plist with the properties
to set, and their new values.

Example:
 (edts-project-override \"~/my-project\" (:name \"my-project-dev\"
                                          :node-name \"my-project-dev\"))"
  (interactive)
  (edts-project--validate-config properties)
  (let ((config (edts-alist-merge (edts-alist-get root edts-project-attributes)
                                  properties)))
    (!edts-alist-store root config 'edts-project-attributes)))

(defun edts-project-revert-all-buffers ()
  "Revert all buffers belonging to current buffer's project. Ignores
auto-save data."
  (interactive)
  (when (y-or-n-p (format "Revert all buffers in %s" (edts-project-name)))
    (edts-project-in-each-buffer
     (lambda (buf) (revert-buffer t t t)))))

(provide 'edts-project)
