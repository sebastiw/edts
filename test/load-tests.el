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

(require 'package)
(package-initialize)

(require 'f)

(defvar edts-dir (f-dirname (f-dirname (f-this-file))))

(dolist (file (f-directories (f-join edts-dir "elisp")))
  (add-to-list 'load-path file))
(add-to-list 'load-path (directory-file-name edts-dir))

(require 'edts-mode)

(dolist (file (f-glob (f-join edts-dir "elisp" "edts" "*-test.el")))
  ;; avoid symlinks created as emacs backups
  (when (not (f-symlink? file))
    (load file)))

(require 'edts-test)
(require 'edts-plugin)
(edts-plugin-load-tests)
