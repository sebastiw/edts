(defun edts-ahs-edit-current-function ()
  "Activate ahs-edit-mode with erlang-current-function range-plugin"
  (interactive)
  (ahs-onekey-edit-function 'erlang-current-function nil))

(defvar edts-lib-directory
  (file-truename
   (concat (file-name-directory
            (or (locate-library "edts") load-file-name)) "../"))
  "Directory where edts libraries are located.")

(require 'edts-setup)
(provide 'edts)