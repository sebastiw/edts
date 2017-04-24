;;; edts-pkg.el --- EDTS dependency installation, for test.

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

(require 'cl-lib)
(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)
(package-refresh-contents)

(let* ((pkg-dir (concat (file-name-directory
                         (or load-file-name buffer-file-name))
                        ".."))
       (pkg-desc (package-load-descriptor (expand-file-name pkg-dir)))
       (pkgs     (package-compute-transaction (list pkg-desc)
                                              (package-desc-reqs pkg-desc)))
       (reqs     (cl-remove-if (lambda (p)
                                 (and p (equal (package-desc-name p) 'edts)))
                               pkgs)))
  (package-download-transaction reqs))
