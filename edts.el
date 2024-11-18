;;; edts.el --- EDTS package declaration.

;; Copyright (C) 2012-2024 Thomas Järvstrand <tjarvstrand@gmail.com>
;; Copyright (C) 2012-2024 Håkan Nilsson
;; Copyright (C) 2020-2024 Sebastian Weddmark Olsson <visnae@gmail.com>

;; Author: Thomas Järvstrand <tjarvstrand@gmail.com>
;;         Sebastian Weddmark Olsson <visnae@gmail.com>

;; URL: https://github.com/sebastiw/edts
;; Keywords: erlang, tools, programming, development

;; Package-Version: 0.0.0.0
;; Package-Requires: (
;;    (auto-complete         "20201213.1255")
;;    (auto-highlight-symbol "20211106.638")
;;    (dash                  "20210609.1330")
;;    (emacs                 "24.3")
;;    (erlang                "20210315.1640")
;;    (f                     "20191110.1357")
;;    (popup                 "20210317.138")
;;    (s                     "20210603.736"))

;; SPDX-License-Identifier: LGPL-3.0-or-later

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

;;; Commentary:

;; The Erlang Development Tool Suite (EDTS) is a package of useful development
;; tools for working with the Erlang programming language in Emacs. It bundles a
;; number of useful external packages, together with specialized Erlang plugins for
;; them, and its own features to create a complete and efficient development
;; environment that is easy to set up.

;; Currently EDTS provides:
;; - A snazzy erlang shell wrapper with syntax highlighting and auto-completion.
;; - In-buffer flymake-like compilation
;; - In-buffer xref checks
;; - Dialyzer integration
;; - Rudimentary project support
;; - Code navigation.
;; - Auto-completion, using auto-complete-mode
;; - Auto-highlighting, using auto-highlight-mode
;; - Convenient access to Erlang documentation
;; - In-buffer running of unit tests
;; - A usable interface to the erlang debugger

;; For more information, hit `M-x describe-minor-mode RET edts-mode RET`.

