;;; edts-pkg.el --- EDTS package declaration.

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

(define-package "edts" "1.0.0" "Erlang Development Tool Suite"
  '((auto-complete         "1.3.1")
    (auto-highlight-symbol "1.53")
    (dash                  "1.5.0")
    (eproject              "1.5")
    (erlang                "2.4.1")
    (f                     "0.16.0")
    (popup                 "0.4")
    (s                     "1.9.0")))
