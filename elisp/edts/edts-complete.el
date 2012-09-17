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
;; setup of auto-complete support for erlang.

(require 'auto-complete)
(require 'ferl)

(require 'edts-complete-variable-source)
(require 'edts-complete-local-function-source)
(require 'edts-complete-imported-function-source)
(require 'edts-complete-built-in-function-source)
(require 'edts-complete-exported-function-source)
(require 'edts-complete-module-source)
(require 'edts-complete-macro-source)
(require 'edts-complete-record-source)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers

(defun edts-complete-point-inside-quotes ()
  "Returns 'double if point is inside double quotes, 'single if point is inside
single quotes and 'none otherwise. Relies on font-lock-string-face to work."
  (if (not (equal 'font-lock-string-face (get-text-property (point) 'face)))
      'none
      (save-excursion
        (let ((match (re-search-backward "['\\\"]")))
          (when match
            (let ((char          (char-after match))
                  (string-face-p (equal 'font-lock-string-face;
                                        (get-text-property (- match 1) 'face))))
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
            (t                                                'none))))))))


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

(defun edts-complete-term-preceding-char ()
  "Returns the character preceding symbol, or if that is a single-quote, the
character before that."
  (let* ((char  (char-before ac-point)))
    (if (equal ?' char)
        (char-before (- ac-point 1))
        char)))

(defadvice ac-expand-string (before edts-complete-trim-arity)
  "Removes any /x at the end of completion string"
  (message "string %s" (replace-regexp-in-string "/[0-9]+$" "" (ad-get-arg 0)))
  (ad-set-arg 0 (replace-regexp-in-string "/[0-9]+$" "" (ad-get-arg 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup

(defconst edts-complete-sources
  '(
    edts-complete-variable-source
    edts-complete-local-function-source
    edts-complete-imported-function-source
    edts-complete-exported-function-source
    edts-complete-built-in-function-source
    edts-complete-module-source
    edts-complete-macro-source
    edts-complete-record-source
    )
  "Sources that EDTS uses for auto-completion.")

(defun edts-complete-setup ()
  "Set edts completion defaults local to current buffer."
  (make-local-variable 'ac-sources)
  (make-local-variable 'ac-disable-faces)
  (make-local-variable 'ac-ignore-case)
  (make-local-variable 'ac-use-menu-map)
  (make-local-variable 'ac-menu-map)
  (make-local-variable 'ac-use-dictionary-as-stop-words)
  (make-local-variable 'ac-disable-faces)

  (setq ac-sources edts-complete-sources)
  (setq ac-ignore-case 'smart)
  (setq ac-use-menu-map t)
  (setq ac-use-dictionary-as-stop-words nil)
  (define-key ac-menu-map (kbd "C-n") 'ac-next)
  (define-key ac-menu-map (kbd "C-p") 'ac-previous)

  ;; this is to allow completion inside quoted atoms. As a side-effect we
  ;; get completion inside strings, which must be handled in the sources
  ;; above.
  (setq ac-disable-faces (delete 'font-lock-string-face ac-disable-faces))

  (auto-complete-mode))

(provide 'edts-complete)
