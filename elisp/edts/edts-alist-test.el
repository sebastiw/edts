;;; edts-alist.el --- Various alist convenience functions
;;
;; Copyright 2017 Thomas Järvstrand <tjarvstrand@gmail.com>
;;
;; Author: Thomas Järvstrand <tjarvstrand@gmail.com>
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

(edts-test-add-suite
 ;; Name
 edts-alist-suite)

(edts-test-case edts-alist-suite edts-alist-from-plist ()
  "Test edts-alist-from-plist"
  (should (equal nil (edts-alist-from-plist nil)))
  (should (equal '((:foo . "foo")) (edts-alist-from-plist '(:foo "foo")))))

(edts-test-case edts-alist-suite edts-alist-map ()
  "Test edts-alist-map"
  (should (equal nil (edts-alist-map (lambda (k v) (cons k (* 2 v))) nil)))
  (should (equal '((:one . 2) (:two . 4) (:three . 6))
                 (edts-alist-map (lambda (k v) (cons k (* 2 v)))
                                 '((:one . 1) (:two . 2) (:three . 3))))))

(edts-test-case edts-alist-suite edts-alist-keys ()
  "Test edts-alist-keys"
  (should-not (edts-alist-keys nil))
  (should (equal '(:foo) (edts-alist-keys '((:foo . "foo")))))
  (should (equal '(:foo :bar) (edts-alist-keys '((:foo . "foo")
                                                 (:bar . "bar"))))))

(edts-test-case edts-alist-suite edts-alist-values ()
  "Test edts-alist-keys"
  (should-not (edts-alist-values nil))
  (should (equal '("foo") (edts-alist-values '((:foo . "foo")))))
  (should (equal '("foo" "bar") (edts-alist-values '((:foo . "foo")
                                                 (:bar . "bar"))))))

(edts-test-case edts-alist-suite edts-alist-add ()
  "Test edts-alist-add"
  (should (equal '((:foo . "foo"))
                 (edts-alist-add :foo "foo" nil)))
  (should (equal '((:foo . "foo") (:bar . "bar"))
                 (edts-alist-add :foo "foo" '((:bar . "bar")))))
  (should (equal '((:foo . "bar") (:foo . "foo"))
                 (edts-alist-add :foo "bar" '((:foo . "foo"))))))

(edts-test-case edts-alist-suite edts-alist-ensure ()
  "Test edts-alist-ensure"
  (should (equal '((:foo . "foo"))
                 (edts-alist-ensure :foo "foo" nil)))
  (should (equal '((:foo . "foo") (:bar . "bar"))
                 (edts-alist-ensure :foo "foo" '((:bar . "bar")))))
  (should (equal '((:foo . "foo"))
                 (edts-alist-ensure :foo "bar" '((:foo . "foo"))))))

(edts-test-case edts-alist-suite edts-alist-remove ()
  "Test edts-alist-remove"
  (should (equal nil
                 (edts-alist-remove :foo nil)))
  (should (equal nil
                 (edts-alist-remove :foo '((:foo . "foo")))))
  (should (equal '((:bar . "bar"))
                 (edts-alist-remove :foo '((:foo . "foo") (:bar . "bar"))))))

(edts-test-case edts-alist-suite edts-alist-get ()
  "Test edts-alist-get"
  (should (equal nil
                 (edts-alist-get :foo nil)))
  (should (equal nil
                 (edts-alist-get :foo '((:bar . "bar")))))
  (should (equal "foo"
                 (edts-alist-get :foo '((:foo . "foo"))))))

(edts-test-case edts-alist-suite edts-alist-get-in ()
  "Test edts-alist-get-in"
  (should (equal nil
                 (edts-alist-get-in nil nil)))
  (should (equal '((:bar . "bar"))
                 (edts-alist-get-in nil '((:bar . "bar")))))
  (should (equal nil
                 (edts-alist-get-in '(:foo) nil)))
  (should (equal nil
                 (edts-alist-get-in '(:foo) '((:bar . "bar")))))
  (should (equal "foo"
                 (edts-alist-get-in '(:foo) '((:foo . "foo")))))
  (should (equal "bar"
                 (edts-alist-get-in '(:foo :bar)
                                    '((:foo . ((:bar . "bar"))))))))

(edts-test-case edts-alist-suite edts-alist-merge ()
  "Test edts-alist-merge"
  (should (equal '((:foo . "foo"))
                 (edts-alist-merge '((:foo . "foo")) nil)))
  (should (equal '((:foo . "foo"))
                 (edts-alist-merge nil '((:foo . "foo")))))
  (should (equal '((:foo . "foo"))
                 (edts-alist-merge '((:foo . "foo")) '((:foo . "foo")))))
  (should (equal '((:foo . "foo") (:bar . "bar"))
                 (edts-alist-merge '((:bar . "bar")) '((:foo . "foo")))))
  (should (equal '((:baz . "foo3")
                   (:bar . "bar2")
                   (:foo . "foo1"))
                 (edts-alist-merge '((:foo . "foo1")
                                     (:bar . "bar1")
                                     (:baz . "foo1"))
                                   '((:bar . "bar2")
                                     (:baz . "foo2"))
                                   '((:baz . "foo3"))))))

(edts-test-case edts-alist-suite edts-alist-store ()
  "Test edts-alist-store"
  (should (equal '((:foo . "bar"))
                 (edts-alist-store :foo "bar" '((:foo . "foo")))))
  (should (equal '((:foo . "bar"))
                 (edts-alist-store :foo "bar" nil))))

(edts-test-case edts-alist-suite !edts-alist-store ()
  "Test !edts-alist-store"
  (let ((alist '((:foo . "foo"))))
    (!edts-alist-store :foo "bar" 'alist)
    (should (equal '((:foo . "bar")) alist)))

  (let ((alist nil))
    (!edts-alist-store :foo "bar" 'alist)
    (should (equal '((:foo . "bar")) alist))))

(edts-test-case edts-alist-suite edts-alist-store-in ()
  "Test edts-alist-store-in"
  (let ((alist nil))
    (should (equal nil
                   (edts-alist-store-in nil nil alist)))
    (should (equal nil alist)))

  (let ((alist '((:bar . "bar"))))
    (should (equal '((:bar . "bar"))
                   (edts-alist-store-in nil nil alist)))
    (should (equal '((:bar . "bar")) alist)))

  (let ((alist '((:bar . "bar"))))
    (should (equal '((:bar . "foo"))
                   (edts-alist-store-in '(:bar) "foo" alist)))
    (should (equal '((:bar . "bar")) alist)))

  (let ((alist '((:bar . ((:foo . "bar"))))))
    (should (equal '((:bar . ((:foo . "foo"))))
                   (edts-alist-store-in '(:bar :foo) "foo" alist)))
    (should (equal '((:bar . ((:foo . "bar")))) alist)))

  (let ((alist '((:bar . "bar"))))
    (should (equal '((:bar . ((:foo . "foo"))))
                   (edts-alist-store-in '(:bar :foo) "foo" alist)))
    (should (equal '((:bar . "bar")) alist))))

(edts-test-case edts-alist-suite !edts-alist-store-in ()
  "Test !edts-alist-store-in"
  (let ((alist nil))
    (should (equal nil
                   (!edts-alist-store-in nil nil 'alist)))
    (should (equal nil alist)))

  (let ((alist '((:bar . "bar"))))
    (should (equal '((:bar . "bar"))
                   (!edts-alist-store-in nil nil 'alist)))
    (should (equal '((:bar . "bar")) alist)))

  (let ((alist '((:bar . "bar"))))
    (should (equal '((:bar . "foo"))
                   (!edts-alist-store-in '(:bar) "foo" 'alist)))
    (should (equal '((:bar . "foo")) alist)))

  (let ((alist '((:bar . ((:foo . "bar"))))))
    (should (equal '((:bar . ((:foo . "foo"))))
                   (!edts-alist-store-in '(:bar :foo) "foo" 'alist)))
    (should (equal '((:bar . ((:foo . "foo")))) alist)))

  (let ((alist '((:bar . "bar"))))
    (should (equal '((:bar . ((:foo . "foo"))))
                   (!edts-alist-store-in '(:bar :foo) "foo" 'alist)))
    (should (equal '((:bar . ((:foo . "foo")))) alist))))

(edts-test-case edts-alist-suite edts-alist-filter ()
  "Test edts-alist-filter"
  (should (equal nil (edts-alist-filter (lambda (k v) t) nil)))
  (should (equal nil (edts-alist-filter (lambda (k v) nil) nil)))

  (should (equal '((a . b)) (edts-alist-filter (lambda (k v) nil) '((a . b)))))
  (should (equal nil (edts-alist-filter (lambda (k v) t) '((a . b)))))

  (should (equal '((a . b)) (edts-alist-filter (lambda (k v) (equal v 'e))
                                               '((a . b) (c . e) (d . e)))))
  (should (equal nil (edts-alist-filter (lambda (k v) t) '((a . b))))))
(provide 'edts-alist-test)
