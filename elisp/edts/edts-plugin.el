;; Copyright 2013 Thomas Järvstrand <tjarvstrand@gmail.com>
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
;; EDTS Plugin management library

;; Prerequisites

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paths

(defconst edts-plugin-directory
  (path-util-join (file-name-directory edts-root-directory) "plugins")
  "Directory where edts plugins are located.")
(add-to-list 'load-path edts-plugin-directory)

(defun edts-plugin-names ()
  "Return a list of the namees of all available plugins."
  (loop for (file dirp . rest)
       in (directory-files-and-attributes edts-plugin-directory nil "^[^.]")
       when dirp
       collect file))

(defun edts-plugin-init-all ()
  "Initialize available plugins."
  (mapc #'edts-plugin-init (edts-plugin-names)))

(defun edts-plugin-init (plugin-name)
  "Do the necessary initialization for PLUGIN."
  (add-to-list `load-path (path-util-join edts-plugin-directory plugin-name))
  (require (intern plugin-name)))

(provide 'edts-plugin)
