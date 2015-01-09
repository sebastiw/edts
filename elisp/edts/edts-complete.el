;;; edts-complete.el --- Auto-completion setup for erlang.

;; Copyright 2012-2013 Thomas Järvstrand <tjarvstrand@gmail.com>

;; Author: Thomas Järvstrand <thomas.jarvstrand@gmail.com>
;; Keywords: erlang
;; This file is not part of GNU Emacs.

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

(require 'auto-complete)
(require 'pos-tip nil t)
(require 'ferl)

(defcustom edts-complete-inhibit nil
  "Inhibit EDTS' auto-completion"
  :type  'boolean
  :group 'edts)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers

(defadvice ac-expand-string (before edts-complete-trim-arity activate)
  "Removes any /x at the end of completion string unless point is in an export list"
  (unless (ferl-is-point-in-export-list-p) (ad-set-arg 0 (replace-regexp-in-string "/[0-9]+$" "" (ad-get-arg 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup

(defcustom edts-complete-sources
  '(edts-complete-keyword-source
    edts-complete-variable-source
    edts-complete-local-function-source
    edts-complete-imported-function-source
    edts-complete-exported-function-source
    edts-complete-built-in-function-source
    edts-complete-module-source
    edts-complete-macro-source
    edts-complete-record-source)
  "Sources that EDTS uses for auto-completion."
  :type '(repeat symbol)
  :group 'edts)

(defcustom edts-complete-shell-sources
  '(edts-complete-keyword-source
    edts-complete-exported-function-source
    edts-complete-built-in-function-source
    edts-complete-module-source)
  "Sources that EDTS uses for auto-completion in shell (comint)
buffers."
  :type '(repeat symbol)
  :group 'edts)

(defun edts-complete-setup (&optional sources)
  "Set edts completion defaults local to current buffer."
  (make-local-variable 'ac-sources)
  (make-local-variable 'ac-disable-faces)
  (make-local-variable 'ac-ignore-case)
  (make-local-variable 'ac-use-menu-map)
  (make-local-variable 'ac-menu-map)
  (make-local-variable 'ac-use-dictionary-as-stop-words)
  (make-local-variable 'ac-disable-faces)

  (setq ac-sources (or sources edts-complete-sources))
  (setq ac-ignore-case 'smart)
  (setq ac-use-menu-map t)
  (setq ac-use-dictionary-as-stop-words nil)
  (define-key ac-menu-map (kbd "C-n") 'ac-next)
  (define-key ac-menu-map (kbd "C-p") 'ac-previous)
  (ad-activate-regexp "edts-complete-.*")

  ;; this is to allow completion inside quoted atoms. As a side-effect we
  ;; get completion inside strings, which must be handled in the sources
  ;; above.
  (setq ac-disable-faces (delete 'font-lock-string-face ac-disable-faces))

  (edts-complete 1))

(defun edts-complete (&optional arg)
  "Call `auto-complete-mode' with ARG unless `edts-complete-inhibit' is
non-nil."
  (unless edts-complete-inhibit
    (auto-complete-mode arg)))

(require 'edts-complete-variable-source)
(require 'edts-complete-local-function-source)
(require 'edts-complete-imported-function-source)
(require 'edts-complete-built-in-function-source)
(require 'edts-complete-exported-function-source)
(require 'edts-complete-module-source)
(require 'edts-complete-macro-source)
(require 'edts-complete-record-source)
(require 'edts-complete-keyword-source)

(provide 'edts-complete)
