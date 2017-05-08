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
;; Test library for edts.
(require 'dash)
(require 'ert)
(require 'em-glob)
(require 'f)

(require 'edts-mode)
(require 'edts-project)
(require 'edts-plugin)

(defconst edts-test-directory (f-join edts-root-directory "test")
  "Directory where EDTS test files are located")

(defun edts-test-project-directory (project)
  (f-join edts-test-directory
          (s-concat "edts-test-project-" (symbol-name project))))

(defun edts-test-project-module-files (project)
  "Return a list of all modules in PROJECT."
  (f-files (edts-test-project-directory project)
           (lambda (file) (equal (f-ext file) "erl"))
           t))

(defun edts-test-find-project-module (project module)
  (let* ((module-name (format "%s.erl" module))
         (files       (edts-test-project-module-files project))
         (file        (-first (lambda (f) (string= (f-filename f) module-name))
                              files)))
    (find-file file)
    (edts-mode t)))

(defun edts-test-cleanup ()
  (edts-log-debug "Doing test cleanup")
  (setq edts-project-attributes nil)
  (edts-log-debug "Test cleanup done"))


(defun edts-test-setup-project (project name &optional config)
  "Create project with NAME and CONFIG in ROOT."
  (let ((root (edts-test-project-directory project)))
    (edts-project-write-config (f-join root ".edts")
                               (edts-alist-add :name name config))))

(defun edts-test-teardown-project (project)
  "Kill all buffers of the project in ROOT and remove its config."
  (let* ((root         (edts-test-project-directory project))
         (project-file (f-join root ".edts")))
    (edts-project-in-each-buffer 'kill-buffer root)
    (when (f-file? project-file)
      (f-delete project-file))
    (setf edts-project-attributes
          (delete-if (lambda (x) (equal (car x) root))
                     edts-project-attributes))))

(defvar edts-test-pre-test-buffer-list nil
  "The buffer list prior to running tests.")

(defun edts-test-pre-cleanup-all-buffers ()
  (interactive)
  (edts-shell-kill-all)
  (dolist (buf (buffer-list))
    (let ((buffer-name (buffer-name buf)))
      (when (and
             (buffer-live-p buf)
             (buffer-file-name buf)
             (f-ancestor-of? edts-test-directory (buffer-file-name buf)))
        (kill-buffer buf)
        (edts-log-debug "Killed %s" buffer-name))))
  (setq edts-test-pre-test-buffer-list (buffer-list)))

(defun edts-test-post-cleanup-all-buffers ()
  (interactive)
  (edts-shell-kill-all)
  (dolist (buf (buffer-list))
    (let ((buffer-name (buffer-name buf)))
      (when (and (buffer-live-p buf)
                 (buffer-file-name buf)
                 (not (member buf edts-test-pre-test-buffer-list)))
        (kill-buffer buf)
        (edts-log-debug "Killed %s" buffer-name))))
  (setq edts-test-pre-test-buffer-list nil))

(defmacro edts-test-case (suite name args desc &rest body)
  "Define a testcase in SUITE. All other arguments are the same is in
`ert-deftest'."
  (unless body
    (error "edts-test-case: Not enough arguments"))
  (declare (indent 3))
  `(macroexpand
    (ert-deftest ,name ,args ,desc :tags '(,suite edts-test-suite) ,@body)))


(defvar edts-test-suite-alist nil
  "edts-tests")

(defmacro edts-test-add-suite (suite-name &optional setup teardown)
  (assert (symbolp suite-name))
  (let ((alistvar (make-symbol "alist")))

    `(let ((,alistvar (remove-if #'(lambda (suite)
                                     (eq (car suite) ',suite-name))
                                 edts-test-suite-alist)))
       (setq edts-test-suite-alist
             (cons '(,suite-name ,(eval setup)
                                 ,(eval teardown))
                   ,alistvar)))))

(defvar edts-test--suite-hist nil
  "List of recent test suites run interactively.")

(defun edts-test-run-suite-interactively (suite-name)
  (interactive
   (list
    (let* ((default (car edts-test--suite-hist))
           (prompt (if default
                       (format "Run suite (default %s): " default)
                     "Run suite: ")))
      (intern
       (completing-read prompt
                        (edts-alist-keys edts-test-suite-alist) ; collection
                        nil ; predicate
                        t ; require match
                        nil ; Initial value
                        'edts-test--suite-hist ; history
                        default)))))
  (edts-test-run-suite 'ert-run-tests-interactively suite-name))


(defalias 'edts-test-suite 'edts-test-run-suite-interactively)

(defun edts-test-run-suite-batch (suite-name)
  (edts-test-run-suite 'ert-run-tests-batch suite-name))

(defun edts-test-run-suites-batch-and-exit ()
  (condition-case err
      (unless (edts-test-run-suites-batch)
        (kill-emacs 1))
    (error
     (edts-log-error "Error running tests: %s" err)
     (kill-emacs 2)))
  (kill-emacs 0))

(defun edts-test-run-suites-batch ()
  (edts-log-set-level 'error)
  (let ((all-successful 0)
        (all-failed 0)
        (all-total 0)
        stats)

    (dolist (suite edts-test-suite-alist)
      (let ((stats (edts-test-run-suite-batch (car suite))))

      (incf all-successful (ert-stats-completed-expected stats))
      (incf all-failed (ert-stats-completed-unexpected stats))
      (incf all-total (ert-stats-total stats))))

    (message "Passed: %s" all-successful)
    (message "Failed: %s" all-failed)
    (message "Completed: %s" all-total)

    (= all-successful all-total)))

(defun edts-test-run-suite (ert-fun suite-name)
  (let* ((suite-name (car (assoc suite-name edts-test-suite-alist)))
         (suite      (cdr (assoc suite-name edts-test-suite-alist))))
    (when suite
      (edts-log-info "Running test suite: %s" suite-name)
      (let ((setup-res (when (car suite) (funcall (car suite))))
            (test-res  (funcall ert-fun (list 'tag suite-name))))
        (when (cadr suite)
          (funcall (cadr suite) setup-res))
        test-res))))


(defvar edts-test--testcase-hist nil
  "List of recent test suites run interactively.")

(defun edts-test-run-test-interactively (test-name)
  (interactive
   (list
    (let* ((default (car edts-test--testcase-hist))
           (test-cases (-map 'ert-test-name (ert-select-tests "edts-*" t)))
           (prompt (if default
                       (format "Run test (default %s): " default)
                     "Run test: ")))
      (completing-read prompt
                       test-cases ; collection
                       nil ; predicate
                       t ; require match
                       nil ; Initial value
                       'edts-test--testcase-hist ; history
                       default)))) ; default value
  (edts-test-run-testcase 'ert-run-tests-interactively test-name))

(defun edts-test-run-testcase (ert-fun test-name)
  (let* ((ert-test-obj (car (ert-select-tests test-name t)))
         (suite-name (car (remq 'edts-test-suite (ert-test-tags ert-test-obj))))
         (suite (cdr (assoc suite-name edts-test-suite-alist))))
    (when suite
      (let ((setup-res (when (car suite) (funcall (car suite))))
            (test-res  (funcall ert-fun test-name)))
        (when (cadr suite)
          (funcall (cadr suite) setup-res))
        test-res))))

(defun edts-test-wait-for (func-or-var &optional timeout sleep)
  "Wait for FUNC-OR-VAR to be non-nil (if it's a variable) or return a non-nil
 value (if it's a function). The function will wait SLEEP seconds between each
check and throws an error of the non-nil result does not happen before
TIMEOUT seconds.

TIMEOUT defaults to 10 seconds and SLEEP to 1 second."
  (let ((timeout (or timeout 5))
        (sleep   (or sleep 1))
        (func    (if (functionp func-or-var)
                     func-or-var
                   (lambda () (symbol-value func-or-var)))))
    (with-timeout (timeout
                   (ert-fail (format "EDTS test timeout while waiting for %s"
                                     func-or-var)))
      (let ((result (funcall func)))
        (while (not result)
          (accept-process-output nil sleep)
          (setq result (funcall func)))
        result))))

;;;;;;;;;;;;;;;;;;;;
;; Tests

(edts-test-case edts-test-suite edts-test-project-directory-test ()
  "Project directory test"
  (should (equal (f-join edts-test-directory "edts-test-project-project-1")
                 (edts-test-project-directory 'project-1)))
  (should (equal (f-join edts-test-directory "edts-test-project-otp-1")
                 (edts-test-project-directory 'otp-1))))

(edts-test-case edts-test-suite edts-test-project-module-files-test ()
  "Project directory test"
  (let ((lib-dir (f-join (edts-test-project-directory 'project-1) "lib")))
    (should (equal (list (f-join lib-dir "one" "src" "one.erl")
                         (f-join lib-dir "one" "src" "one_two.erl")
                         (f-join lib-dir "two" "src" "two.erl"))
                   (-sort 'string<
                          (edts-test-project-module-files 'project-1))))))

(provide 'edts-test)
