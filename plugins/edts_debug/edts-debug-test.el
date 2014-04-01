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
;; Debugger interaction code for EDTS

;; Window configuration to be restored when quitting debug mode

(require 'edts-test)

(edts-test-add-suite
 ;; Name
 edts-debug-suite
 ;; Setup
 (lambda ()
   (let ((async-node-init edts-async-node-init))
     (setq edts-async-node-init nil)
     (setq edts-event-inhibit t)
     (edts-rest-force-sync t)
     (edts-test-pre-cleanup-all-buffers)
     (edts-test-setup-project edts-test-project1-directory
                              "test"
                              nil)
     `((async-node-init . ,async-node-init))))

 ;; Teardown
 (lambda (setup-config)
   (setq edts-async-node-init (cdr (assoc 'async-node-init setup-config)))
   (edts-rest-force-sync nil)
   (setq edts-event-inhibit nil)
   (edts-test-post-cleanup-all-buffers)
   (edts-test-teardown-project edts-test-project1-directory)))

(edts-test-case edts-debug-suite edts-debug-basic-test ()
  "Basic debugger setup test"
  (let ((eproject-prefer-subproject t))
    (find-file (car (edts-test-project1-modules)))

    (should-not (edts-debug-interpretedp))
    (edts-debug-interpret nil nil 't)
    (should (edts-debug-interpretedp))
    (should-not (edts-debug-module-breakpoints))
    (edts-debug-break nil nil nil t)
    (should (eq 1 (length (edts-debug-breakpoints))))))
