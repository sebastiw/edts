;; edts-api-test.el --- Tests for edts-api.el
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

(require 'edts-test)
(require 'edts-api)
(require 'edts-code)

(require 'dash)
(require 's)

(edts-test-add-suite
 ;; Name
 edts-api-suite
 ;; Setup
 (lambda ()
   (edts-test-pre-cleanup-all-buffers)
   (setq edts-event-inhibit t))
 ;; Teardown
 (lambda (setup-config)
   (setq edts-event-inhibit nil)
   (edts-test-post-cleanup-all-buffers)
   (edts-test-teardown-project 'project-1)))

(edts-test-case edts-api-suite edts-api-start-server-test ()
  "Tests that the main server starts ok."
  (edts-test-pre-cleanup-all-buffers)
  (edts-test-wait-for (lambda () (not (edts-api-node-started-p "edts-server"))))
  (edts-api-start-server)
  (edts-test-wait-for (lambda () (edts-api-node-started-p "edts-server")))
  (kill-buffer "*edts-server*")
  (edts-test-wait-for
   (lambda () (not (edts-api-node-started-p "edts-server")))))

(edts-test-case edts-api-suite edts-api-init-node-test ()
  "Tests that it's possible to register nodes with the main server."
  (edts-api-ensure-server-started)
  (edts-test-wait-for (lambda () (edts-api-node-started-p "edts-server")))
  (edts-shell-make-comint-buffer "test-node"
                                 "*test_node*"
                                 (edts-test-project-directory 'temp-2)
                                 '("erl" "-sname" "test_node"))
  (edts-test-wait-for (lambda () (edts-api-node-started-p "test_node")))
  (should-not (-contains? (edts-api-get-nodes t) "test_node"))
  (let* (node-initialized
         (edts-api-after-node-init-hook
          (list (lambda () (setq node-initialized t)))))
    (edts-api-init-node "temp"
                        "test_node"
                        (edts-test-project-directory 'temp-2)
                        nil
                        nil
                        nil
                        nil)
    (edts-test-wait-for 'node-initialized)
    (should (-any? (lambda (node) (s-starts-with? "test_node@" node))
                   (edts-api-get-nodes t)))))

(edts-test-case edts-api-suite edts-api-module-operations-test ()
  "Tests that it's possible to perform module operations on a project node."
  (edts-api-ensure-server-started)
  (edts-test-wait-for (lambda () (edts-api-node-started-p "edts-server")))
  (let* (node-initialized
         (edts-api-after-node-init-hook
          (list (lambda () (setq node-initialized t)))))
    (edts-test-find-project-module 'temp-1 'one)
    (edts-test-wait-for 'node-initialized))
  (let* (module-compiled
         (callback (lambda (&rest res)
                     (setq module-compiled t))))
    (edts-api-compile-and-load-async "one" (buffer-file-name) callback)
    (edts-test-wait-for 'module-compiled))
  (should (edts-alist-get 'source (edts-api-get-detailed-module-info "one")))
  (should-not (edts-api-get-detailed-module-info "bla"))
  (should (edts-alist-get 'exported (edts-api-get-function-info "one" "one" 1)))
  (should (-contains? (edts-api-get-modules) "one"))
  (should (equal 4 (length (edts-api-get-module-exports "one")))))

(edts-test-case edts-api-suite edts-api-free-vars-test ()
  "Tests that get_free_vars works."
  (edts-api-ensure-server-started)
  (should (equal '("Bar" "Foo") (edts-api-get-free-vars "one(Foo, Bar)")))
  (should-error (edts-api-get-free-vars "one(Foo Bar)")))


