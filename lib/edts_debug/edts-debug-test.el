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

(require 'edts-api)
(require 'edts-test)

(edts-test-add-suite
 ;; Name
 edts-debug-suite
 ;; Setup
 (lambda ()
   (let ((async-node-init edts-api-async-node-init))
     (setq edts-api-async-node-init nil)
     (setq edts-event-inhibit t)
     (edts-test-pre-cleanup-all-buffers)
     (edts-test-setup-project 'project-1 "test")
     `((async-node-init . ,async-node-init))))

 ;; Teardown
 (lambda (setup-config)
   (setq edts-api-async-node-init (cdr (assoc 'async-node-init setup-config)))
   (setq edts-event-inhibit nil)
   (edts-test-post-cleanup-all-buffers)
   (edts-test-teardown-project 'project-1)))

(edts-test-case edts-debug-suite edts-debug-basic-test ()
  "Basic debugger setup test"
  (edts-test-find-project-module 'project-1 'one)

  (should-not (edts-debug-interpretedp))
  (edts-debug-interpret nil nil 't)
  (should (edts-debug-interpretedp))
  (should-not (edts-debug-module-breakpoints))
  (edts-debug-break nil nil nil t)
  (should (eq 1 (length (edts-debug-breakpoints)))))

