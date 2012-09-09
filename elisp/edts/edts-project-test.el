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
    (node-sname    . "dev-node")
    (lib-dirs . ("lib" "test"))))

;; Incorrectly defined project
(defvar edts-project-test-project-2
  '((start-command . "bin/start.sh -i")))

(ert-deftest edts-project-start-node-test ()
  (flet ((edts-node-started-p (node-name) t))
    (should-error (edt-project-start-node edts-project-test-project-1))))

(ert-deftest edts-project-build-project-command-test ()
  (flet ((edts-project-code-path-expand (project) '("./foo/test" "./foo/ebin"))
         (executable-find (cmd) cmd))
    (should
     (equal '("erl" "-sname" "dev-node" "-pa" "./foo/test" "./foo/ebin")
            (edts-project-build-project-command edts-project-test-project-1))))
  (should
   (equal '("bin/start.sh" "-i")
          (edts-project-build-project-command edts-project-test-project-2))))

(ert-deftest edts-project-make-comint-buffer-test ()
  (let ((buffer (edts-project-make-comint-buffer "edts-test" "." '("erl"))))
    (should (bufferp buffer))
    (should (string= "edts-test" (buffer-name buffer)))
    (should (string= "erl" (process-name (get-buffer-process buffer))))
    (set-process-query-on-exit-flag (get-buffer-process buffer) nil)
    (kill-process (get-buffer-process buffer))
    (kill-buffer buffer)))

(ert-deftest edts-project-buffer-node-started-p-test ()
  (flet ((edts-node-started-p (node)
                              (if (string= node "dev-node")
                                  t
                                  (error "wrong node-name")))
         (edts-project-buffer-project (buffer) edts-project-test-project-1))
    (should (edts-project-buffer-node-started-p (current-buffer))))
  (flet ((edts-node-started-p (node)
                              (if (string= node "dev-node")
                                  nil
                                  (error "wrong node-name")))
         (edts-project-buffer-project (buffer) edts-project-test-project-1))
    (should-not (edts-project-buffer-node-started-p (current-buffer)))))

(ert-deftest edts-project-project-name-test ()
  (should (string= "dev"
                 (edts-project-name edts-project-test-project-1)))
  (should (equal nil
                   (edts-project-name edts-project-test-project-2))))

(ert-deftest edts-project-project-root-test ()
  (should (string= "./foo"
                 (edts-project-root edts-project-test-project-1)))
  (should (equal nil
                   (edts-project-root edts-project-test-project-2))))

(ert-deftest edts-project-lib-dirs-test ()
  (should (equal '("lib" "test")
                 (edts-project-lib-dirs edts-project-test-project-1)))
  (should (equal '("lib")
                   (edts-project-lib-dirs edts-project-test-project-2))))

(ert-deftest edts-project-node-name-test ()
  (should (string= "dev-node"
              (edts-project-node-name edts-project-test-project-1)))
  (should (eq nil
              (edts-project-node-name edts-project-test-project-2))))

(ert-deftest edts-project-start-command-test ()
  (should (eq nil (edts-project-start-command edts-project-test-project-1)))
  (should (string= "bin/start.sh -i"
                   (edts-project-start-command edts-project-test-project-2))))

(ert-deftest edts-project-path-expand-test ()
  (let ((home (expand-file-name "~")))
    (flet ((file-expand-wildcards (path)
                                  (when (string= (concat home "/foo/lib/*")
                                                 path)
                                    (list (concat home "/foo/lib/bar")))))
      (should (equal (list
                      (concat home "/foo/lib/bar/ebin")
                      (concat home "/foo/lib/bar/test"))
                     (edts-project-path-expand "~/foo" "lib"))))))

(ert-deftest edts-project-buffer-node-name-test ()
  (let ((edts-projects (list edts-project-test-project-1)))
    (flet ((buffer-file-name (buffer) "./foo/bar.el"))
    (should
     (string= "dev-node"
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
