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
;; Unit tests for edts-project.el

(require 'ert nil 'noerror)
(require 'edts-project)

(defvar edts-project-test-project-1
  '((name          . "dev")
    (root          . "./foo")
    (node-sname    . "dev")
    (lib-dirs . ("lib" "test"))))

(ert-deftest edts-project-path-expand ()
  (flet ((file-expand-wildcards (path)
                                (when (string= "foo/lib/*" path)
                                    '("foo/lib/bar"))))
    (should (equal '("foo/lib/bar/ebin" "foo/lib/bar/test")
                   (edts-project-path-expand "foo" "lib")))))

(ert-deftest edts-project-buffer-node-name-test ()
  (let ((edts-projects (list edts-project-test-project-1)))
    (flet ((buffer-file-name (buffer) "./foo/bar.el"))
    (should
     (string= "dev"
              (edts-project-buffer-node-name (current-buffer)))))
    (flet ((buffer-file-name (buffer) "./bar/baz.el"))
      (should
       (eq nil
           (edts-project-buffer-node-name (current-buffer)))))))

(ert-deftest edts-project-buffer-project-test ()
  (let ((edts-projects (list edts-project-test-project-1)))
    (flet ((buffer-file-name (buffer) "./foo/bar.el"))
    (should
     (eq edts-project-test-project-1
         (edts-project-buffer-project (current-buffer)))))
    (flet ((buffer-file-name (buffer) "./bar/baz.el"))
      (should
       (eq nil
           (edts-project-buffer-project (current-buffer)))))))

(ert-deftest edts-project-file-project-test ()
  (let ((edts-projects (list edts-project-test-project-1)))
    (should
     (eq edts-project-test-project-1
         (edts-project-file-project "./foo/bar.el"))))
  (should-not (edts-project-file-project "./bar/baz.el")))

(ert-deftest edts-project-file-in-project-p ()
  (should
   (not (null (edts-project-file-in-project-p
               edts-project-test-project-1
               "./foo/bar.el"))))
  (should
   (not (null (edts-project-file-in-project-p
               edts-project-test-project-1
               "./foo/bar/baz.el"))))
  (should
   (null (edts-project-file-in-project-p
               edts-project-test-project-1
               "/bar/foo/baz.el"))))

(ert-deftest edts-project-file-under-path-p ()
  (should
   (not (null (edts-project-file-under-path-p "/foo" "/foo/bar/baz.el"))))
  (should
   (not (edts-project-file-under-path-p "/bar" "/foo/bar/baz.el"))))

(ert-deftest edts-project-normalize-path-test ()
  (flet ((expand-file-name (file-name) (concat "./" file-name)))
    (should (string= "./foo/bar/" (edts-project-normalize-path "foo//bar")))
    (should (string= "./foo/bar/" (edts-project-normalize-path "foo/bar/")))))


(provide 'edts-project-test)
