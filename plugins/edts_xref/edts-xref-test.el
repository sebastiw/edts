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
;; xref interaction code for EDTS
(require 'edts-api)
(require 'edts-test)
(require 'edts-xref)

(edts-test-add-suite
 ;; Name
 edts-xref-suite
 ;; Setup
 (lambda ()
   (setq edts-event-inhibit t)
   (edts-test-pre-cleanup-all-buffers)
   (edts-test-setup-project edts-test-project1-directory
                            "test"
                            nil)
   (edts-rest-force-sync t))

 ;; Teardown
 (lambda (setup-config)
   (setq edts-event-inhibit nil)
   (edts-test-teardown-project edts-test-project1-directory)
   (edts-test-post-cleanup-all-buffers)
   (edts-rest-force-sync nil)))

(edts-test-case edts-xref-suite edts-xref-analysis-test ()
  "Basic xref analysis setup test"
  (flet ((edts-node-down-request () nil))
    (let ((edts-api-async-node-init nil))
      (find-file (car (edts-test-project1-modules)))
      (edts-xref-module-analysis-async '("one"))
      (let* ((errors (plist-get (plist-get edts-code-buffer-issues 'edts-xref)
                                'error)))
        (should (equal (length errors) 1))
        (should (equal (cdr (assoc 'line (car errors))) 24))
        (should (string= (cdr (assoc 'type (car errors))) "error"))
        (should (string= (cdr (assoc 'file (car errors)))
                         (file-truename (buffer-file-name))))))))

(edts-test-case edts-xref-suite edts-xref-who-calls-test ()
  "Basic project setup test"
  (flet ((edts-node-down-request () nil))
    (let ((edts-api-async-node-init nil))
      (find-file (car (edts-test-project1-modules)))
      (let* ((callers  (edts-xref-get-who-calls "one_two" "one_two_fun" 1))
             (caller   (car callers))
             (module   (cdr (assoc 'module caller)))
             (function (cdr (assoc 'function caller)))
             (arity    (cdr (assoc 'arity caller)))
             (lines    (cdr (assoc 'lines caller))))
        (should (equal (length callers) 1))
        (should (equal module "one"))
        (should (equal function "one"))
        (should (equal arity 1))))))

