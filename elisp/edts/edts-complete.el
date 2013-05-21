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
(require 'pos-tip)
(require 'ferl)

(load "edts-complete-variable-source" nil edts-start-inhibit-load-msgs)
(load "edts-complete-local-function-source" nil edts-start-inhibit-load-msgs)
(load "edts-complete-imported-function-source" nil edts-start-inhibit-load-msgs)
(load "edts-complete-built-in-function-source" nil edts-start-inhibit-load-msgs)
(load "edts-complete-exported-function-source" nil edts-start-inhibit-load-msgs)
(load "edts-complete-module-source" nil edts-start-inhibit-load-msgs)
(load "edts-complete-macro-source" nil edts-start-inhibit-load-msgs)
(load "edts-complete-record-source" nil edts-start-inhibit-load-msgs)
(load "edts-complete-keyword-source" nil edts-start-inhibit-load-msgs)

(defcustom edts-complete-inhibit nil
  "Inhibit EDTS' auto-completion"
  :type  'boolean
  :group 'edts)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers

(defun edts-complete-point-inside-quotes ()
  "Returns 'double if point is inside double quotes, 'single if point is inside
single quotes and 'none otherwise. Relies on font-lock-string-face to work."
  (if (not (equal 'font-lock-string-face (get-text-property (point) 'face)))
      'none
      (save-excursion
          (when (re-search-backward "\\([^\\]\\|^\\)\\(['\"]\\)" nil t)
            (let* ((start (match-beginning 2))
                   (char (char-after start))
                   (string-face-p (equal 'font-lock-string-face
                                         (get-text-property (1- start) 'face))))
           (cond
            ; we're inside a double quoted string if either:
            ; we hit a " and the preceding char is not string
            ; fontified.
            ((and (equal ?\" char) (not string-face-p)) 'double-quoted)
            ; or we hit a ' and the preceding char is still string
            ; fontified
            ((and (equal ?' char) string-face-p)              'double-quoted)
            ; we're inside a single quoted string if either:
            ; we hit a ' and the preceding char is not string
            ; fontified.
            ((and (equal ?' char) (not string-face-p))        'single-quoted)
            ; or we hit a " and the preceding char is still string
            ; fontified
            ((and (equal ?\" char) string-face-p)             'single-quoted)
            ; Otherwise we're not inside quotes
            ('otherwise                                       'none)))))))


(defun edts-complete-single-quote-terminate (str)
  "Removes any single quotes at start and end of `str' and adds one at the end
if not already present"
  (when      (string-match "^'" str) (setq str (substring str 1)))
  (when (not (string-match "'$" str)) (setq str (concat str "'")))
  str)

(defun symbol-at (&optional pos)
  "Returns the symbol at `pos', if any, otherwise nil."
  (save-excursion
    (when pos (goto-char pos))
    (thing-at-point 'symbol)))

(defun edts-complete-term-preceding-char (&optional point)
  "Returns the character preceding symbol, or if that is a single-quote, the
character before that."
  (let* ((char  (char-before (or point ac-point))))
    (if (equal ?' char)
        (char-before (- ac-point 1))
        char)))

(defadvice ac-expand-string (before edts-complete-trim-arity)
  "Removes any /x at the end of completion string"
  (ad-set-arg 0 (replace-regexp-in-string "/[0-9]+$" "" (ad-get-arg 0))))

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
  "Sources that EDTS uses for auto-completion.")

(defcustom edts-complete-shell-sources
  '(edts-complete-keyword-source
    edts-complete-exported-function-source
    edts-complete-built-in-function-source
    edts-complete-module-source)
  "Sources that EDTS uses for auto-completion in shell (comint)
buffers.")

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

