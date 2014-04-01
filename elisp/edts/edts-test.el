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
(require 'f)

(require 'edts-project)

(defconst edts-test-directory (f-join edts-root-directory "test")
  "Directory where EDTS test files are located")

(defconst edts-test-project1-directory
  (f-join edts-test-directory "edts-test-project1")
  "Directory where EDTS edts-test-project1 is located")

(defun edts-test-project1-modules ()
  "Return a list of all modules in edts-test-project1."
  (file-expand-wildcards
   (f-join edts-test-project1-directory "lib" "*" "src" "*.erl")))

(defun edts-test-cleanup ()
  (edts-log-debug "Doing test cleanup")
  (setq eproject-attributes-alist nil)
  (edts-log-debug "Test cleanup done"))


(defun edts-test-setup-project (root name config)
  "Create project with NAME and CONFIG in ROOT."
  (edts-project-write-config (f-join root ".edts")
                             (append (list :name name) config)))

(defun edts-test-teardown-project (root)
  "Kill all buffers of the project in ROOT and remove its config."
  (with-each-buffer-in-project (buf root)
      (kill-buffer buf))
  (f-delete (f-join root ".edts"))
  (setf eproject-attributes-alist
          (delete-if (lambda (x) (equal (car x) root))
                     eproject-attributes-alist)))

(defvar edts-test-pre-test-buffer-list nil
  "The buffer list prior to running tests.")

(defun edts-test-pre-cleanup-all-buffers ()
  (interactive)
  (edts-shell-kill-all)
  (dolist (buf (buffer-list))
    (when (and
           (buffer-live-p buf)
           edts-mode
           (kill-buffer buf))
      (edts-log-debug "Killed %s" buf)))
  (setq edts-test-pre-test-buffer-list (buffer-list)))

(defun edts-test-post-cleanup-all-buffers ()
  (interactive)
  (edts-shell-kill-all)
  (dolist (buf (buffer-list))
    (when (and (buffer-live-p buf)
               (buffer-file-name buf)
               (not (member buf edts-test-pre-test-buffer-list)))
          (kill-buffer buf)
      (edts-log-debug "Killed %s" buf)))
  (setq edts-test-pre-test-buffer-list nil))

(defmacro edts-test-case (suite name args desc &rest body)
  "Define a testcase in SUITE. All other arguments are the same is in
`ert-deftest'."
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
      (read-from-minibuffer prompt nil nil t 'edts-test--suite-hist default))))
  (edts-test-run-suite 'ert-run-tests-interactively suite-name))

(defalias 'edts-test-suite 'edts-test-run-suite-interactively)

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
      (edts-log-error "Error running tests")
      (backtrace)
      (kill-emacs 2))))

(defun edts-test-run-suite (ert-fun suite-name)
  (let* ((suite (cdr (assoc suite-name edts-test-suite-alist))))
    (when suite
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
           (prompt (if default
                       (format "Run test (default %s): " default)
                     "Run test: ")))
      (read-from-minibuffer prompt nil nil t 'edts-test--testcase-hist default))))
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

(provide 'edts-test)
