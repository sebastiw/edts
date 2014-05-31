;; Copyright 2013-2014 Thomas Järvstrand <tjarvstrand@gmail.com>
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
;; Test loading library for edts.


(defvar edts-dir (file-name-directory (directory-file-name
                                       (file-name-directory load-file-name))))

(dolist (file
         (directory-files
          (expand-file-name "elisp" edts-dir) t "^[^.]"))
  (when (file-directory-p file)
    (add-to-list 'load-path file)))
(add-to-list 'load-path (directory-file-name edts-dir))

(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize))

(require 'edts-mode)

(dolist (file (directory-files (f-join edts-dir "elisp" "edts") t "-test\\.el$"))
  ;; avoid symlinks created as emacs backups
  (when (not (file-symlink-p file))
    (load file)))

(require 'edts-test)
(require 'edts-plugin)
(edts-plugin-load-tests)
