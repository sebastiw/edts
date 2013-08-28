;;; edts-rest.el --- Rest communication utilities.

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
;;
;; Rudimentary project support for edts so that we can relate buffers to
;; projects and communicate with the correct nodes.
;;

(require 'cl)
(require 'url)
(require 'json)

(defconst edts-rest-host "0"
  "The host where the edts erlang node is running.")

(defconst edts-rest-port 4587
  "The port on which the edts erlang node's rest-api is available.")

(defconst edts-rest-content-type-hdr '("Content-Type" . "application/json"))


(defun edts-rest-get (resource args &optional body)
  "Send a get request to RESOURCE with ARGS with optional BODY."
  (edts-rest-request "GET" resource args body))

(defun edts-rest-post (resource args &optional body)
  "Send a post request to RESOURCE with ARGS"
  (edts-rest-request "POST" resource args body))

(defun edts-rest-request (method resource args &optional body)
  "Send a request to RESOURCE with ARGS"
  (let ((url                       (edts-rest-resource-url resource args))
        (url-request-method        method)
        (url-request-extra-headers (list edts-rest-content-type-hdr))
        (url-request-data          (json-encode body)))
    (make-local-variable 'url-show-status)
    (setq url-show-status nil)
    (edts-log-debug "Sending %s-request to %s" method url)
    (let ((buffer (url-retrieve-synchronously url)))
      (when buffer
        (with-current-buffer buffer
          (let* ((reply  (edts-rest-parse-http-response))
                 (status (cdr (assoc 'result reply))))
            (edts-log-debug "Reply %s received for request to %s" status url)
            (kill-buffer (current-buffer))
            reply))))))

(defun edts-rest-get-async (resource args callback &optional callback-args)
  "Send a post request to RESOURCE with ARGS"
  (edts-rest-request-async "GET" resource args callback callback-args))

(defun edts-rest-post-async (resource args callback &optional callback-args)
  "Send a post request to RESOURCE with ARGS"
  (edts-rest-request-async "POST" resource args callback callback-args))

(defun edts-rest-request-async (method resource args callback callback-args)
  "Send asynchronous request to using METHOD to RESOURCE with ARGS. When
the request terminates, call CALLBACK with the parsed response and
CALLBACK-ARGS."
  (let* ((url                       (edts-rest-resource-url resource args))
         (url-request-method        method)
         (url-request-extra-headers (list edts-rest-content-type-hdr))
         (callback-args             (append
                                     (list url (current-buffer) callback)
                                     callback-args)))
    (make-local-variable 'url-show-status)
    (setq url-show-status nil)
    (edts-log-debug "Sending async %s-request to %s" method url)
    (with-current-buffer
      (if (>= emacs-major-version 24)
          (url-retrieve url #'edts-rest-request-callback callback-args t)
        (url-retrieve url #'edts-rest-request-callback callback-args))
      (make-local-variable 'url-show-status)
      (setq url-show-status nil)
      (current-buffer))))

(defun edts-rest-request-callback (events url cb-buf cb &rest cb-args)
  "Callback for asynchronous http requests."
  (let* ((reply         (edts-rest-parse-http-response))
         (status        (cdr (assoc 'result reply)))
         (reply-buf     (current-buffer)))
    (edts-log-debug "Reply %s received for async request to %s" status url)
    (if (not (buffer-live-p cb-buf))
        (edts-log-error "Callback buffer %s was killed!" cb-buf)
      (with-current-buffer cb-buf
        (apply cb reply cb-args))
      ;; Workaround for Emacs 23.x that sometimes leaves us in cb-buf even
      ;; when we are back outside the `with-current-buffer'. This seems to be
      ;; bug somewhere in `save-current-buffer', but is not present in Emacs 24.
      (unless (eq (current-buffer) reply-buf)
        (set-buffer reply-buf))
      (url-mark-buffer-as-dead reply-buf))))

(defun edts-rest-parse-http-response ()
  "Parses the contents of an http response in current buffer."
  (save-excursion
    (goto-char (point-min))
    (let* ((status     (split-string (buffer-substring (point) (point-at-eol))))
           (result     (cdr status))
           (body-start (search-forward "\n\n" nil t)))
    (if body-start
        (list
         (cons 'result result)
         (cons 'body
               (edts-rest-try-decode (buffer-substring body-start (point-max))))
        (list (cons 'result result)))))))

(defun edts-rest-resource-url (resource args)
  "Construct the edts resource url."
  (let ((host edts-rest-host)
        (port edts-rest-port)
        (path (mapconcat #'identity resource "/"))
        (args (edts-rest-encode-args args)))
    (format "http://%s:%s/%s%s" host port path args)))

(defun edts-rest-encode-args (args)
  "Encode ARGS as a list of url-arguments."
  (let ((encoded "?")
        (arg nil))
    (while args
      (setq arg (edts-rest-encode-arg (pop args)))
      (when arg (setq encoded (concat encoded arg "&"))))
    ;; strip the last &, or the ? if there where no args or only empty args.
    (substring encoded 0 -1)))

(defun edts-rest-encode-arg (arg)
  "Encode ARG as a url-argument"
  (let ((var (car arg))
        (val (cdr arg)))
    (cond
     ((null val) nil)
     ((listp val) (concat var "=" (mapconcat #'identity val ",")))
     (t (concat var "=" val)))))

(defun edts-rest-encode (data)
  "Encode DATA as json."
  (let ((json-object-type 'alist)
        (json-array-type  'list))
    (json-encode data)))

(defun edts-rest-try-decode (string)
  "Decode STRING from json if possible, otherwise return it as is."
  (condition-case nil
      (unless (string-equal string "")
        (let ((json-object-type 'alist)
              (json-array-type  'list)
              (json-false       nil))
          (json-read-from-string string)))
    (error string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unit tests

(when (featurep 'ert)

  (ert-deftest edts-rest-encode-arg-test ()
    (should (equal "foo=bar"
            (edts-rest-encode-arg '("foo" . "bar"))))
    (should (equal "foo=bar"
            (edts-rest-encode-arg '("foo" . ("bar")))))
    (should (equal "foo=bar,baz"
            (edts-rest-encode-arg '("foo" . ("bar" "baz")))))))

