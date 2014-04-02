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

(require 'url)
(require 'json)

(require 'edts-log)

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

(defun edts-rest-request (method resource args &optional body is-retry)
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
                 (status (cdr (assoc 'result reply)))
                 (proc   (get-buffer-process (current-buffer)))
                 ;; The url-library's retry-functionality is pretty broken
                 ;; for synchronous requests. url will try to re-use connections
                 ;; and sometimes the used connection will have expired, in
                 ;; which case the request has to be redone with another
                 ;; connection. The problem is that url issues an async request
                 ;; with callback and returns even if the original request was
                 ;; synchronous.
                 ;;
                 ;; Retrying here is a workaround for this.
                 (retry  (and proc
                              (eq (process-status proc) 'open)
                              (not reply)
                              (not is-retry))))
            (edts-log-debug "Reply %s received for request to %s" status url)
            (when proc
              (set-process-query-on-exit-flag proc nil))
            (kill-buffer (current-buffer))
            (if (not retry)
                reply
              (edts-log-debug "Retrying request")
              (edts-rest-request method resource args body t))))))))

(defun edts-rest-get-async (resource
                            args
                            callback
                            &optional
                            callback-args
                            force-callback)
  "Send asynchronous GET request to using METHOD to RESOURCE with ARGS.
When the request terminates, call CALLBACK with the parsed response and
CALLBACK-ARGS with the buffer that was current-buffer at the time the
request was issued as current-buffer. If that buffer was killed and
FORCE-CALLBACK is non-nil, call the callback anyway inside a
`with-temp-buffer'."
  (edts-rest-request-async "GET"
                           resource
                           args
                           callback
                           callback-args
                           force-callback))

(defun edts-rest-post-async (resource
                             args
                             callback
                             &optional
                             callback-args
                             force-callback)
  "Send asynchronous POST request to using METHOD to RESOURCE with ARGS.
When the request terminates, call CALLBACK with the parsed response and
CALLBACK-ARGS with the buffer that was current-buffer at the time the
request was issued as current-buffer. If that buffer was killed and
FORCE-CALLBACK is non-nil, call the callback anyway inside a
`with-temp-buffer'."
  (edts-rest-request-async "POST"
                           resource
                           args
                           callback
                           callback-args
                           force-callback))

(defun edts-rest-request-async (method
                                resource
                                args
                                callback
                                callback-args
                                force-callback)
  "Send asynchronous request to using METHOD to RESOURCE with ARGS. When
the request terminates, call CALLBACK with the parsed response and
CALLBACK-ARGS with the buffer that was current-buffer at the time the
request was issued as current-buffer. If that buffer was killed and
FORCE-CALLBACK is non-nil, call the callback anyway inside a
`with-temp-buffer'."
  (let* ((url                       (edts-rest-resource-url resource args))
         (url-request-method        method)
         (url-request-extra-headers (list edts-rest-content-type-hdr))
         (callback-args             (list url
                                          (current-buffer)
                                          callback
                                          callback-args
                                          force-callback)))
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

(defun edts-rest-request-callback (events url cb-buf cb cb-args force-cb)
  "Callback for asynchronous http requests."
  (let* ((reply         (edts-rest-parse-http-response))
         (status        (cdr (assoc 'result reply)))
         (reply-buf     (current-buffer)))
    (edts-log-debug "Reply %s received for async request to %s" status url)
    (if (not (buffer-live-p cb-buf))
        (if force-cb
            (with-temp-buffer (apply cb reply cb-args))
          (edts-log-error "Callback buffer %s was killed!" cb-buf))
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
;; Test support

(defun edts-rest-force-sync (force)
  "If FORCE is non-nil, force all requests to be synchronous. Otherwise
ensure that this is not enforced."
  (if force
      (ad-activate-regexp "edts-rest-test-sync")
    (ad-deactivate-regexp "edts-rest-test-sync")))

(defadvice edts-rest-request-async (around edts-rest-test-sync (method
                                                                resource
                                                                args
                                                                callback
                                                                callback-args
                                                                force-callback))
  "** Use only for testing **

Wrap a an async request to RESOURCE with ARGS and turn it into a
synchronous request, calling CALLBACK with CALLBACK-ARGS when the
request completes."
  (make-local-variable 'url-show-status)
  (setq url-show-status nil)
  (let ((url                       (edts-rest-resource-url resource args))
        (url-request-method        method)
        (url-request-extra-headers (list edts-rest-content-type-hdr)))
    (apply callback
           (edts-rest-request method resource args)
           callback-args)))
(edts-rest-force-sync nil)

(defadvice url-http-end-of-document-sentinel
  (around edts-rest-end-of-document-sentinel (process why))
  "Workaround for url-http-end-of-document-sentinel not properly
propagating buffer-local variables when retrying a request.

http://debbugs.gnu.org/cgi/bugreport.cgi?bug=14983 will most likely solve
the issue and make this hack redundant."
  (let* ((buf (process-buffer process))
         (url-request-method (buffer-local-value 'url-http-method buf))
         (url-request-extra-headers
          (buffer-local-value 'url-http-extra-headers buf))
         (url-request-data (buffer-local-value 'url-http-data buf)))
    ad-do-it))
(ad-activate-regexp "edts-rest-end-of-document-sentinel")

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

(provide 'edts-rest)
