;;; edts-alist.el --- Various alist convenience functions

;; Copyright 2012-2013 Thomas Järvstrand <tjarvstrand@gmail.com>

;; Author: Thomas Järvstrand <thomas.jarvstrand@gmail.com>
;; Keywords: erlang
;; This file is not part of GNU Emacs.

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

(require 'dash)

(defun edts-alist-from-plist (plist)
  (loop for (k v . rest) on plist by #'cddr
        collect (cons k v)))

(defun edts-alist-map (f alist)
  (-map (-lambda ((k . v))
          (funcall f k v))
        alist))

(defun edts-alist-keys (alist)
  (-map 'car alist))

(defun edts-alist-values (alist)
  (-map 'cdr alist))

(defun edts-alist-remove (key alist)
  (-remove (lambda (e) (equal (car e) key)) alist))

(defun edts-alist-add (key value alist)
  (cons (cons key value) alist))

(defun edts-alist-ensure (key default alist)
  (if (edts-alist-get key alist)
      alist
    (edts-alist-add key default alist)))

(defun edts-alist-get (key alist)
  (cdr (assoc key alist)))

(defun edts-alist-get-in (key-list alist)
  (let ((value alist))
    (dolist (key key-list value)
      (setq value (edts-alist-get key value)))))

(defun edts-alist-merge (target &rest sources)
  (-reduce-from #'edts--alist-merge target sources))

(defun edts--alist-merge (target source)
  (-reduce-from (lambda (acc el)
                  (edts-alist-store (car el) (cdr el) acc))
                target
                source))

(defun edts-alist-store (key value alist)
  (edts-alist-add key value (edts-alist-remove key alist)))

(defun !edts-alist-store (key value alist-variable)
  (set alist-variable
       (edts-alist-store key value (symbol-value alist-variable))))

(defun edts-alist-store-in (key-list value alist)
  (if (not key-list)
      alist
    (let ((v value))
      (while key-list
        (let ((key (-last-item key-list)))
          (setq key-list (-butlast key-list))
          (let ((sub-element (edts-alist-get-in key-list alist)))
            (if (listp sub-element)
                (setq v (edts-alist-store key v sub-element))
              (setq v (edts-alist-store key v nil))))))
      v)))

(defun !edts-alist-store-in (key-list value alist-variable)
  (set alist-variable
       (edts-alist-store-in key-list value (symbol-value alist-variable))))


(defun edts-alist-select (predicate alist)
  "Return all elements of ALIST for which PREDICATE returns a non-nil value. PREDICATE is a function that takes two argument KEY and VALUE."
  (-filter (lambda (el) (funcall predicate (car el) (cdr el))) alist))


(defun edts-alist-filter (predicate alist)
  "Remove all elements of ALIST for which PREDICATE returns a non-nil value. PREDICATE is a function that takes two argument KEY and VALUE."
  (-filter (lambda (el) (not (funcall predicate (car el) (cdr el)))) alist))


(provide 'edts-alist)
