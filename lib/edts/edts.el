
(defun ahs-range-beginning-of-erlang-function ()
  "If point is inside an Erlang function, return the starting position of that
   function, otherwise nil.
   NB. Doesn't work correctly if point is at the functions first character."
  (save-excursion (erlang-beginning-of-function) (point)))

(defun ahs-range-end-of-erlang-function ()
  "If point is inside an Erlang function, return the end position of that
   function, otherwise nil.
   NB. Doesn't work if point is at the functions final character (full stop)."
  (save-excursion (erlang-end-of-function) (point)))

(defun erlang-ahs-edit-current-function ()
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