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

;; TODO: sugar around lambda/lambda, which is ugly
(define-project-type generic () nil
  :relevant-files (".*")
  :irrelevant-files ("^[.]" "^[#]")
  :file-name-map (lambda (root) (lambda (root file) file))
  :local-variables (lambda (root) (lambda (root file) nil))
  :config-file ".eproject")


(define-project-type edts (generic)
  (progn
     (edts-eproject-maybe-create file)
     (look-for ".edts"))
  :config-file ".edts"
  :relevant-files ("^\\.erlang$"
                   "\\.app$"
                   "\\.app.src$"
                   "\\.config$"
                   "\\.es$"
                   "\\.escript$"
                   "\\.eterm$"
                   "\\.script$"
                   "\\.yaws$")
  :irrelevant-files (".edts"
                     ".gitignore"
                     ".gitmodules"))

(defun edts-eproject-file-visit-hook ()
  "Called the first time a buffer is opened in a project."
  (when (and (eq (eproject-type) 'edts) (boundp 'edts-projects))
    (let ((project (edts-eproject--find-by-root (eproject-root))))
      (when project
        (edts-eproject--override project)))))
(add-hook 'edts-project-file-visit-hook 'edts-eproject-file-visit-hook)

(defun edts-eproject--find-by-root (root)
  "Returns the entry from `edts-projects' whose `root' equal ROOT after
they are both expanded."
  (let ((exp-root (expand-file-name root)))
    (find-if
     #'(lambda (project)
         (string= (file-name-as-directory
                   (expand-file-name
                    (edts-project-root project)))
                  exp-root))
     edts-projects)))

(defun edts-eproject--override (project)
  "Override the configuration of the current buffer's eproject with the
values from PROJECT. The PROJECT's `root' is assumed to already be the
same as the current eproject's, if it's not then calling this function
will most likely break something in eproject."
  ;; This function is really dirty but I can't think of a better way to do it.
  (let* ((root       (eproject-root))
         (attrs      (cdr (assoc root eproject-attributes-alist)))
         (attr-alist (assq-delete-all root eproject-attributes-alist)))
    (loop for (prop . val) in project do
          (setq attrs (plist-put attrs (intern (format ":%s" prop)) val)))
    (setq eproject-attributes-alist (cons (cons root attrs) attr-alist))))

(defun edts-eproject-maybe-create (file)
  "Automatically creates a .edts-file from a an old-style project
definition if `edts-projects' is bound and, FILE is inside one of its
projects and there is no previous .edts-file."
  (when (boundp 'edts-projects)
    (let ((project (edts-project-file-project file)))
      (when (and project
                 (not (file-exists-p (edts-eproject--config-file project))))
        (edts-eproject--create project)
        (edts-log-info "Created .edts configuration file for project: ~p"
                       (edts-project-name project))))))

(defun edts-eproject--create (project)
  (with-temp-file (edts-eproject--config-file project)
    (loop for field in project do
          (if (listp (cdr field))
              (insert (format ":%s '%S\n" (car field) (cdr field)))
            (insert (format ":%s %S\n" (car field) (cdr field)))))))

(defun edts-eproject--config-file (project)
  "Return the path to projects eproject configuration file."
  (path-util-join (edts-project-root project) ".edts"))
