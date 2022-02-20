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

(define-package "edts" "1.1.0" "Erlang Development Tool Suite"
  '((auto-complete         "20201213.1255")
    (auto-highlight-symbol "20211106.638")
    (dash                  "20210609.1330")
    (emacs                 "24.3")
    (erlang                "20210315.1640")
    (f                     "20191110.1357")
    (popup                 "20210317.138")
    (s                     "20210603.736")))
