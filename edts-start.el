;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; EDTS Setup and configuration.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Prerequisites
(require 'cl)
(require 'woman)
(require 'ert nil 'noerror)

(require 'edts-autoloads)

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
