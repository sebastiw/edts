;; Copyright 2013 Thomas Järvstrand <tjarvstrand@gmail.com>
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
;; Tests for xref interaction code for EDTS

(require 'cl-lib)

(require 'edts-test)
(require 'edts-xref)

(defun edts-xref-test-testcase-init ()
  (edts-test-pre-cleanup-all-buffers))

(edts-test-add-suite
 ;; Name
 edts-xref-suite
 ;; Setup
 (lambda ()
   (setq edts-event-inhibit t)
   (edts-test-pre-cleanup-all-buffers)
   (edts-test-setup-project 'project-1 "test"))

 ;; Teardown
 (lambda (setup-config)
   (setq edts-event-inhibit nil)
   (edts-test-teardown-project 'project-1)
   (edts-test-post-cleanup-all-buffers)))


(edts-test-case edts-xref-suite edts-xref-analysis-test ()
  "Basic xref analysis setup test"
  (edts-xref-test-testcase-init)
  (let (edts-code-buffer-issues
        edts-xref-initialized-nodes)
    (edts-test-find-project-module 'project-1 'one)
    (edts-test-wait-for 'edts-xref-initialized-nodes 10)
    (edts-xref-module-analysis-async '("one"))
    (let* ((errors (edts-test-wait-for 'edts-xref-errors 10))
           (file   (cdr (assoc 'file (car errors)))))
      (should (equal (length errors) 2))
      (should (equal (cdr (assoc 'line (car errors))) 24))
      (should (string= (cdr (assoc 'type (car errors))) "error"))
      (should (or (string= file (buffer-file-name))
                  (string= file (file-truename (buffer-file-name))))))))


(edts-test-case edts-xref-suite edts-xref-error-whitelist-test ()
  "Tests xref whitelisting of calls"
  (edts-xref-test-testcase-init)
  (let (edts-code-buffer-issues
        edts-xref-initialized-nodes)
    (edts-test-find-project-module 'project-1 'one)
    (edts-test-wait-for 'edts-xref-initialized-nodes)
    (edts-project-set-attribute :xref-error-whitelist '("two"))
    (edts-xref-module-analysis-async '("one"))
    (let* ((errors (edts-test-wait-for 'edts-xref-errors)))
      (should (equal (length errors) 1)))))


(edts-test-case edts-xref-suite edts-xref-file-whitelist-test ()
  "Basic xref analysis setup test"
  (let (edts-code-buffer-issues
        edts-xref-initialized-nodes)
    (edts-xref-test-testcase-init)
    (edts-test-find-project-module 'project-1 'one)
    (edts-test-wait-for 'edts-xref-initialized-nodes)
    (edts-project-set-attribute :xref-file-whitelist '("one.erl"))
    (edts-xref-module-analysis-async '("one"))
    (edts-test-wait-for 'edts-code-buffer-issues)
    (should (equal (length (edts-xref-errors)) 0))))


(edts-test-case edts-xref-suite edts-xref-who-calls-test ()
  "Basic project setup test"
  (let (edts-code-buffer-issues
        edts-xref-initialized-nodes)
    (edts-xref-test-testcase-init)
    (edts-test-find-project-module 'project-1 'one)
    (edts-test-wait-for 'edts-xref-initialized-nodes)
    (let* ((callers  (edts-xref-get-who-calls "one_two" "one_two_fun" 1))
           (caller   (car callers))
           (module   (cdr (assoc 'module caller)))
           (function (cdr (assoc 'function caller)))
           (arity    (cdr (assoc 'arity caller)))
           (lines    (cdr (assoc 'lines caller))))
      (should (equal (length callers) 1))
      (should (equal module "one"))
      (should (equal function "one"))
      (should (equal arity 1)))))

