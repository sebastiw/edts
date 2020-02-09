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
          (edts-project--find-temp-root dir)))))

(defun edts-project--find-project-root (dir)
  "Try to find the top-most edts-file above current buffer's file."
  (let (stop
        (roots (-map 'f-slash edts-project-roots))
        root)
    (while (and (not stop) (not (f-root? dir)))
      (if (-contains? roots (f-slash dir))
          (setq root dir
                stop t)
        (when (f-file? (f-join dir ".edts"))
          (setq root dir))
        (setq dir (f-dirname dir))))
    root))

(defun edts-project--find-otp-root (dir)
  (f-traverse-upwards (lambda (path)
                        (when (not (f-root? path))
                          (f-file? (f-join path "bin" "erl"))))
                      (f-expand dir)))

(defun edts-project--find-temp-root (dir)
  "Find the appropriate root directory for a temporary project for
FILE."
  (if (and (-contains? '("src" "test" "include") (f-filename dir))
           (or (f-directory? (f-join (f-dirname dir) "ebin"))
               (f-directory? (f-join (f-dirname dir) "_build"))))
      (f-dirname dir)
    dir))


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

      ;; NOTE: It's important for rebar3's test profile to be loaded
      ;; first, because rebar3 might have different contents of a specific
      ;; dependency, so it doesn't load the second copy it finds. For your
      ;; root project, that basically would mean that any supporting
      ;; modules you have for your test cases won't be loaded into EDTS,
      ;; making your test modules a sea of red squiggles.
      (:lib-dirs . ("lib"
                    "deps"
                    ;; Probably use f-expand.
                    ,(f-join "_build" "test" "lib")
                    ,(f-join "_build" "default" "lib"))))))

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
  (edts-alist-from-plist (eval (read (format "(list %s)" file-contents)))))

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
    (!edts-alist-store root config)))

(defun edts-project-revert-all-buffers ()
  "Revert all buffers belonging to current buffer's project. Ignores
auto-save data."
  (interactive)
  (when (y-or-n-p (format "Revert all buffers in %s" (edts-project-name)))
    (edts-project-in-each-buffer
     (lambda (buf) ('revert-buffer t t t)))))

(provide 'edts-project)
