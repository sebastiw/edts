;; edts-project-test.el --- Tests for edts-project.el
;;
;; Copyright 2012-2017 Thomas Järvstrand <tjarvstrand@gmail.com>
;;
;; Author: Thomas Järvstrand <thomas.jarvstrand@gmail.com>
;; Keywords: erlang
;; This file is not part of GNU Emacs.
;;
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

(require 'cl-macs)
(require 'f)

(require 'edts-test)
(require 'edts-project)


(edts-test-add-suite
 ;; Name
 edts-project-suite
 ;; Setup
 (lambda ()
   (setq edts-event-inhibit t)
   (edts-test-pre-cleanup-all-buffers)
   (edts-test-setup-project 'project-1
                            "test"
                            '((:node-name . "test"))))
 ;; Teardown
 (lambda (setup-config)
   (setq edts-event-inhibit nil)
   (edts-test-post-cleanup-all-buffers)
   (edts-test-teardown-project 'project-1)))


(edts-test-case edts-project-suite edts-project--find-project-root-test ()
  "Tests edts-project--find-project-root"
  (should (string= "/foo"
                   (cl-letf (((symbol-function 'f-file?)
                              (lambda (f)
                                (or (equal f "/foo/.edts")
                                    (equal f "/foo/bar/baz/bam/.edts")))))
                     (edts-project--find-project-root "/foo/bar/baz/bam"))))
  (should (string= "/foo/bar"
                   (cl-letf (((symbol-function 'f-file?)
                              (lambda (f)
                                (or (equal f "/foo/.edts")
                                    (equal f "/foo/bar/baz/bam/.edts")))))
                     (let ((edts-project-roots '("/foo/bar")))
                       (edts-project--find-project-root "/foo/bar/baz/bam")))))
  (should-not (cl-letf (((symbol-function 'f-file?)
                         (lambda (f) nil)))
                (edts-project--find-project-root "/foo/bar/baz/bam"))))

(edts-test-case edts-project-suite edts-project--find-rebar-root-test ()
  "Tests edts-project--find-project-root"
  (should (string= "/foo"
                   (cl-letf (((symbol-function 'f-exists?)
                              (lambda (f) (equal f "/foo/rebar.config"))))
                     (edts-project--find-rebar-root "/foo/bar/baz/bam"))))
  (should-not (cl-letf (((symbol-function 'f-exists?)
                         (lambda (f) nil))
                (edts-project--find-rebar-root "/foo/bar/baz/bam")))))

(edts-test-case edts-project-suite edts-project--find-otp-root-test ()
  "Tests edts-project--find-project-root"
  (should (string= "/foo"
                   (cl-letf (((symbol-function 'f-exists?)
                              (lambda (f) t))
                             ((symbol-function 'f-file?)
                              (lambda (f) (equal f "/foo/bin/erl"))))
                     (edts-project--find-otp-root "/foo/bar/baz/bam"))))
  (should-not (cl-letf (((symbol-function 'f-exists?)
                         (lambda (f) t))
                        ((symbol-function 'f-file?)
                         (lambda (f) nil)))
                (edts-project--find-otp-root "/foo/bar/baz/bam"))))

(edts-test-case edts-project-suite edts-project--find-temp-root-test ()
  (should (string= "/foo/bar/baz"
                   (cl-letf (((symbol-function 'f-exists?)
                              (lambda (f) t))
                             ((symbol-function 'f-directory?)
                              (lambda (f) (or (equal f "/foo/bar/baz/src")
                                              (equal f "/foo/bar/baz/ebin")))))
                     (edts-project--find-temp-root "/foo/bar/baz/src"))))
  (should (string= "/foo/bar/baz/src"
                   (cl-letf (((symbol-function 'f-exists?)
                              (lambda (f) t))
                             ((symbol-function 'f-directory?)
                              (lambda (f) nil)))
                     (edts-project--find-temp-root "/foo/bar/baz/src")))))

(edts-test-case edts-project-suite edts-project--find-root-test ()
  (should (equal "project"
                 (cl-letf (((symbol-function 'edts-project--find-project-root)
                            (lambda (f) "project"))
                           ((symbol-function 'edts-project--find-rebar-root)
                            (lambda (f) "rebar"))
                           ((symbol-function 'edts-project--find-otp-root)
                            (lambda (f) "otp"))
                           ((symbol-function 'edts-project--find-temp-root)
                            (lambda (f) "temp"))
                           ((symbol-function 'f-this-file)
                            (lambda () "/foo/bar/baz/src/foo")))
                   (edts-project--find-root "/foo/bar/baz/src"))))
  (should (equal "rebar"
                 (cl-letf (((symbol-function 'edts-project--find-project-root)
                            (lambda (f) nil))
                           ((symbol-function 'edts-project--find-rebar-root)
                            (lambda (f) "rebar"))
                           ((symbol-function 'edts-project--find-otp-root)
                            (lambda (f) "otp"))
                           ((symbol-function 'edts-project--find-temp-root)
                            (lambda (f) "temp"))
                           ((symbol-function 'f-this-file)
                            (lambda () "/foo/bar/baz/src/foo")))
                   (edts-project--find-root "/foo/bar/baz/src"))))
  (should (equal "otp"
                 (cl-letf (((symbol-function 'edts-project--find-project-root)
                            (lambda (f) nil))
                           ((symbol-function 'edts-project--find-rebar-root)
                            (lambda (f) nil))
                           ((symbol-function 'edts-project--find-otp-root)
                            (lambda (f) "otp"))
                           ((symbol-function 'edts-project--find-temp-root)
                            (lambda (f) "temp"))
                           ((symbol-function 'f-this-file)
                            (lambda () "/foo/bar/baz/src/foo")))
                   (edts-project--find-root "/foo/bar/baz/src"))))
  (should (equal "temp"
                 (cl-letf (((symbol-function 'edts-project--find-project-root)
                            (lambda (f) nil))
                           ((symbol-function 'edts-project--find-rebar-root)
                            (lambda (f) nil))
                           ((symbol-function 'edts-project--find-otp-root)
                            (lambda (f) nil))
                           ((symbol-function 'edts-project--find-temp-root)
                            (lambda (f) "temp"))
                           ((symbol-function 'f-this-file)
                            (lambda () "/foo/bar/baz/src/foo")))
                   (edts-project--find-root "/foo/bar/baz/src"))))
  )

(edts-test-case edts-project-suite edts-project-basic-test ()
  "Basic project setup test"
  (edts-test-pre-cleanup-all-buffers)
  (let (auto-mode-alist)
        (find-file (car (edts-test-project-module-files 'project-1))))
  (edts-project-init)
  (should (string= (edts-test-project-directory 'project-1)
                   (edts-project-root)))
  (should (string= "test" (edts-project-name))))

(edts-test-case edts-project-suite edts-project-basic-otp-test ()
  "Basic project setup test"
  (edts-test-pre-cleanup-all-buffers)
  (let (auto-mode-alist)
    (find-file (car (edts-test-project-module-files 'otp-1))))
  (edts-project-init)
  (should (string= (edts-test-project-directory 'otp-1)
                   (edts-project-root)))
  (should (string= "otp-edts-test-project-otp-1" (edts-project-name))))

(edts-test-case edts-project-suite edts-project-basic-temp-test ()
  "Basic project setup test"
  (edts-test-pre-cleanup-all-buffers)
  (let (auto-mode-alist)
    (find-file (car (edts-test-project-module-files 'temp-1))))
  (edts-project-init)
  (should (string= (edts-test-project-directory 'temp-1)
                   (edts-project-root)))
  (should (string= "edts-test-project-temp-1" (edts-project-name))))

(edts-test-case edts-project-suite edts-project-set-attribute-test ()
  "Test setting of project attributes"
  (let (edts-project-attributes)
    (should-not edts-project-attributes)
    (edts-project-set-attribute :foo "bar" "baz")
    (should (equal "bar" (edts-project-attribute :foo "baz")))))


(edts-test-case edts-project-suite edts-project-override-test ()
  "Test setting project overrides"
  (let* (auto-mode-alist
         (root (edts-test-project-directory 'project-1))
         (edts-project-overrides `((,root . ((:name . "bla"))))))
    (find-file (car (edts-test-project-module-files 'project-1)))
    (edts-project-init)
    (should (equal "bla" (edts-project-attribute :name)))))

(edts-test-case edts-project-suite edts-project-buffers-test ()
  "Test getting all project's buffers"
  (edts-test-pre-cleanup-all-buffers)
  (let (auto-mode-alist
        find-file-hook
        edts-project-attributes)
    (find-file (car (edts-test-project-module-files 'project-1)))
    (edts-project-init)
    (find-file (car (cdr (edts-test-project-module-files 'project-1))))
    (edts-project-init)
    (should (equal 2
                   (length (-map 'buffer-name (edts-project-buffers)))))))

(edts-test-case edts-project-suite edts-project-in-each-buffer-test ()
  "Test running a function in all project's buffers"
  (edts-test-pre-cleanup-all-buffers)
  (let (auto-mode-alist
        find-file-hook
        edts-project-attributes)
    (find-file (car (edts-test-project-module-files 'project-1)))
    (edts-project-init)
    (find-file (car (cdr (edts-test-project-module-files 'project-1))))
    (edts-project-init)
    (should (equal '("test" "test")
                   (edts-project-in-each-buffer
                    (lambda () (edts-project-name)))))))
