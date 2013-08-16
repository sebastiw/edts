;; Copyright 2012 João Neves <sevenjp@gmail.com>
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
;; Test library for edts.
(require 'em-glob)
(require 'path-util)

(defconst edts-test-directory (path-util-join edts-root-directory "test")
  "Directory where EDTS test files are located")

(defconst edts-test-project1-directory
  (path-util-join edts-test-directory "edts-test-project1")
  "Directory where EDTS edts-test-project1 is located")

(defun edts-test-project1-modules ()
  "Return a list of all modules in edts-test-project1."
  (file-expand-wildcards
   (path-util-join edts-test-project1-directory "lib" "*" "src" "*.erl")))

(defun edts-test-cleanup ()
  (edts-log-debug "Doing test cleanup")
  (setq eproject-attributes-alist nil)
  (edts-log-debug "Test cleanup done"))

(defmacro edts-test-save-buffer-list (&rest body)
  "Save buffer-list; execute body; restore buffer-list."
  `(let ((pre-body-bufs (buffer-list)))
     (prog1 (progn ,@body)
       (dolist (buf (buffer-list))
         (unless (member buf pre-body-bufs)
           (edts-log-debug "Cleaning up leftover buffer: %s" buf)
           (kill-buffer buf))))))

(defmacro edts-test-with-config (project-path config &rest body)
  "Run BODY with the project in PROJECT-PATH using CONFIG."
  `(let ((cfg-file (path-util-join ,project-path ".edts"))
         (config   ,config))
     (edts-project-write-config cfg-file config)
     (prog1 (progn ,@body)
       (delete-file cfg-file))))


(defmacro edts-deftest (suite name args desc &rest body)
  "Define a testcase in SUITE. All other arguments are the same is in
`ert-deftest'."
  (declare (indent 3))
  `(macroexpand (ert-deftest ,name ,args ,desc :tags '(,suite) ,@body)))


(defvar edts-test-suite-alist nil
  "edts-tests")

(defmacro edts-test-add-suite (suite setup teardown)
  (assert (symbolp suite))
  `(add-to-list 'edts-test-suite-alist '(,suite ,setup ,teardown)))


(defun edts-test-run-suite-interactively (suite-name)
  (edts-test-run-suite 'ert-run-tests-interactively suite-name))

(defun edts-test-run-suite-batch (suite-name)
  (edts-test-run-suite 'ert-run-tests-batch suite-name))

(defun edts-test-run-suites-batch-and-exit ()
  (unwind-protect
      (let ((exit-status 0))
        (dolist (suite edts-test-suite-alist)
          (let* ((suite-name (car suite))
                 (stats (edts-test-run-suite-batch suite-name)))
            (unless (zerop (ert-stats-completed-unexpected stats))
              (setq exit-status 1))))
        (kill-emacs exit-status))
    (progn
      (message "Error running tests")
      (backtrace))))

(defun edts-test-run-suite (ert-fun suite-name)
  (let ((suite (cdr (assoc suite-name edts-test-suite-alist))))
    (when suite
      (let ((setup-res (funcall (car suite)))
            (test-res  (funcall ert-fun (list 'tag suite-name))))
        (funcall (cadr suite) setup-res)
        test-res))))
