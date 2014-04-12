;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; EDTS Setup and configuration.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Prerequisites
(require 'cl)
(require 'woman)
(require 'ert nil 'noerror)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paths

(dolist (pkg '(dash s f))
    (unless (require pkg nil t)
      (add-to-list 'load-path
                   (format "%s/elisp/%s"
                           (directory-file-name edts-root-directory)
                           pkg))
      (require pkg)))

(unless (require 'erlang nil 'noerror)
  (add-to-list 'load-path
               (car
                (file-expand-wildcards
                 (f-join
                  (f-dirname (f-dirname (f-canonical (executable-find "erl"))))
                  "lib"
                  "tools*"
                  "emacs"))))
  (require 'erlang))

;; Add all libs to load-path
(loop for  (name dirp . rest)
      in   (directory-files-and-attributes edts-lib-directory nil "^[^.]")
      when dirp
      do   (add-to-list 'load-path (f-join edts-lib-directory name)))

(defun edts-byte-compile ()
  "Byte-compile all elisp packages part of EDTS."
  (interactive)
  (let* ((dirs (directory-files edts-lib-directory t "^[^.]"))
         (files (apply #'append
                       (mapcar #'(lambda (dir)
                                   (directory-files dir t "\\.el$")) dirs))))
    (byte-compile-disable-warning 'cl-functions)
    (mapc #'byte-compile-file files)
    t))

(defun edts-start-load-tests ()
  "Load all test-files."
  (loop
   for  file
   in   (directory-files edts-code-directory nil "-test\\.el$")
   ;; avoid symlinks created as emacs backups
   when (not (file-symlink-p (f-join edts-code-directory file)))
   do   (load file))
  (edts-plugin-load-tests))

(provide 'edts-start)
