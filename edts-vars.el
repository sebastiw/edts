(defgroup edts nil
  "Erlang development tools"
  :group 'convenience
  :prefix "edts-")

(defvar edts-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-n"     'edts-code-next-issue)
    (define-key map "\C-c\C-p"     'edts-code-previous-issue)
    (define-key map "\C-c\C-df"    'edts-find-local-function)
    (define-key map "\C-c\C-d\S-f" 'edts-find-global-function)
    (define-key map "\C-c\C-dH"    'edts-find-doc)
    (define-key map "\C-c\C-dh"    'edts-show-doc-under-point)
    (define-key map "\C-c\C-d\C-b" 'ferl-goto-previous-function)
    (define-key map "\C-c\C-d\C-f" 'ferl-goto-next-function)
    (define-key map "\C-c\C-de"    'edts-ahs-edit-current-function)
    (define-key map "\C-c\C-dE"    'edts-ahs-edit-buffer)
    (define-key map "\C-c\C-dt"    'edts-code-eunit)
    (define-key map "\C-c\C-dr"    'edts-refactor-extract-function)
    (define-key map "\M-."         'edts-find-source-under-point)
    (define-key map "\M-,"         'edts-find-source-unwind)
    map)
  "Keymap for EDTS.")

(defcustom edts-erl-command
  (or (executable-find "erl")
      (null
       (warn
        "No erl on exec-path. Most of EDTS' functionality will be broken.")))
  "Location of the erl-executable to use when launching the main EDTS-node."
  :type 'file
  :group 'edts)

(defcustom edts-erl-flags
  ""
  "Flags to use when launching the main EDTS-node."
  :type 'string
  :group 'edts)

(eval-and-compile
  (defconst edts-root-directory
    (file-name-directory (or (locate-library "edts-autoloads")
                             load-file-name
                             default-directory))
    "EDTS root directory."))

(defconst edts-code-directory
  (f-join edts-root-directory "elisp" "edts")
  "Directory where edts code is located.")

(defcustom edts-data-directory
  (if (boundp 'user-emacs-directory)
      (expand-file-name "edts" user-emacs-directory)
    (expand-file-name "~/.emacs.d"))
  "Where EDTS should save its data."
  :type 'directory
  :group 'edts)

(defconst edts-lib-directory
  (f-join edts-root-directory "elisp")
  "Directory where edts libraries are located.")

(defconst edts-plugin-directory-name
  "lib"
  "Bare directory name for the plugins")

(defconst edts-plugin-directory
  (f-join edts-root-directory edts-plugin-directory-name)
  "Directory where edts plugins are located.")

(defconst edts-plugin-names
  (cl-remove-if
   (lambda (v) (equal v "edts"))
   (cl-loop for (file dirp . rest)
            in (directory-files-and-attributes edts-plugin-directory nil "^[^.]")
            when dirp
            collect file))
  "a list of the names of all available plugins.")

(defcustom edts-plugin-disabled-plugins '("edts")
  "List of disabled plugins.

The plugins distributed with EDTS which you may want to add to the
list are listed in the `edts-plugin-names' value."
  :type '(repeat string)
  :group 'edts)

(defconst edts-test-directory
  (f-join edts-root-directory "test_data")
  "Directory where edts test data are located.")

(provide 'edts-vars)
