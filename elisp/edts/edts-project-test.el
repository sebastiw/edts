;;; edts-project.el ---  Integration with Jonathan Rockway's eproject package.

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

(require 'edts-test)
(edts-test-add-suite
 ;; Name
 edts-project-suite
 ;; Setup
 (lambda ()
   (setq edts-event-inhibit t)
   (edts-test-pre-cleanup-all-buffers)
   (edts-test-setup-project edts-test-project1-directory
                            "test"
                            nil))
 ;; Teardown
 (lambda (setup-config)
   (setq edts-event-inhibit nil)
   (edts-test-post-cleanup-all-buffers)
   (edts-test-teardown-project edts-test-project1-directory)))


(edts-test-case edts-project-suite edts-project-basic-test ()
  "Basic project setup test"
  (find-file (car (edts-test-project1-modules)))
  (should (f-exists?
           (f-join edts-test-project1-directory ".edts")))
  (should (string= "test" (eproject-name)))
  (should (get-buffer"*edts*"))
  (should (get-buffer"*test*")))

(edts-test-case edts-project-suite edts-project-selector-test ()
  "Test that the in the case of multiple levels of projects, the super
project is selected as the root, and other project types such as git-generic
are not considered for erl-files."

  ;; Assume that .git exists in edts directory (Yes, this is generally a
  ;; stupid idea, but I'm feeling lazy right now).
  (should (f-exists?
           (f-join (f-dirname (f-dirname edts-test-project1-directory))
                   ".git")))
  (edts-test-setup-project (f-join edts-test-project1-directory
                                   "lib"
                                   "one")
                           "test-dep"
                           nil)

  ;; There! Now we have a subproject called test-dep in
  ;; `edts-test-project1-directory'/lib/one, a super project in
  ;; `edts-test-project1-directory' and a git-project in
  ;; `edts-test-project1-directory'/../..
  ;; This test ensures that for
  ;; `edts-test-project1-directory'/lib/one/src/one.erl, we choose
  ;; `edts-test-project1-directory' as the project root.
  (find-file (car (edts-test-project1-modules)))
  (should (string= (f-full edts-test-project1-directory)
                   (f-full (eproject-root)))))
