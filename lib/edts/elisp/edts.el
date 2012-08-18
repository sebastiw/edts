(defun edts-ahs-edit-current-function ()
  "Activate ahs-edit-mode with erlang-current-function range-plugin"
  (interactive)
  (ahs-onekey-edit-function 'erlang-current-function nil))

(provide 'edts)