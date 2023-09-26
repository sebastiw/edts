;;; edts-rpc.el --- Rpc communication utilities.

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

(require 'dash)
(require 'url)
(require 'json)

(require 'edts-log)

(defvar edts-rpc-suppress-error-codes nil
  "Do not log http errors of requests with these return codes")

(defconst edts-rpc-host "0"
  "The host where the edts erlang node is running.")

(defconst edts-rpc-port
  (let ((edts-port-env (getenv "EDTS_PORT")))
    (if edts-port-env
        edts-port-env
      4587))
  "The port on which the edts erlang node's rpc-api is available.")

(defconst edts-rpc-content-type-hdr '("Content-Type" . "application/json"))

(defun edts-rpc-call (command args &optional is-retry)
  "Send a call to COMMAND with ARGS"
  (let ((url                       (edts-rpc-command-url command))
        (url-request-method        "POST")
        (url-request-extra-headers (list edts-rpc-content-type-hdr))
        (url-request-data          (edts-rpc-encode args)))
    (make-local-variable 'url-show-status)
    (setq url-show-status nil)
    (edts-log-debug "Sending call to %s" url)
    (edts-log-debug-2 "Call args: %s" url-request-data)
    (-when-let (buffer (condition-case nil
                           (url-retrieve-synchronously url)
                         (error nil)))
      (with-current-buffer buffer
        (let* ((proc (get-buffer-process (current-buffer)))
               (reply (edts-rpc-parse-http-response))
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
          (edts-rpc--log-http-response url)
          (when proc
            (set-process-query-on-exit-flag proc nil))
          (kill-buffer (current-buffer))
          (if (not retry)
              reply
            (edts-log-debug "Retrying request")
            (edts-rpc-call command args t)))))))

(defun edts-rpc-call-async (command
                            args
                            callback
                            &optional
                            callback-args
                            force-callback)
  "Send asynchronous request with ARGS. When the request terminates, call
CALLBACK with the parsed response and CALLBACK-ARGS with the buffer that was
 current-buffer at the time the request was issued as current-buffer. If that
 buffer was killed and FORCE-CALLBACK is non-nil, call the callback anyway
inside a `with-temp-buffer'."
  (let* ((url                       (edts-rpc-command-url command))
         (url-request-method        "POST")
         (url-request-extra-headers (list edts-rpc-content-type-hdr))
         (url-request-data          (edts-rpc-encode args))
         (callback-args             (list url
                                          (current-buffer)
                                          callback
                                          callback-args
                                          force-callback)))
    (make-local-variable 'url-show-status)
    (setq url-show-status nil)
    (edts-log-debug "Sending async call to %s" url)
    (edts-log-debug-2 "Call args: %s" url-request-data)
    (with-current-buffer
        (url-retrieve url #'edts-rpc-request-callback callback-args t)
      (make-local-variable 'url-show-status)
      (setq url-show-status nil)
      (current-buffer))))

(defun edts-rpc-request-callback (events url cb-buf cb cb-args force-cb)
  "Callback for asynchronous http requests."
  (let* ((reply     (edts-rpc-parse-http-response))
         (reply-buf (current-buffer)))
    (edts-rpc--log-http-response url)
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

(defun edts-rpc--log-http-response (url)
  (let* ((status (cadr (edts-rpc--parse-http-response-status)))
         (levels (if (or (equal status "200")
                         (and
                          (stringp status)
                          (s-numeric? status)
                          (-contains? edts-rpc-suppress-error-codes
                                     (string-to-number status))))
                     '(debug debug-2)
                   '(error error))))
    (edts-log-message (car levels)
                      "Reply %s received for request to %s"
                      status
                      url)
    (edts-log-message (cadr levels)
                      "Reply:\n----\n%s\n----"
                      (buffer-string))))


(defun edts-rpc-parse-http-response ()
  "Parses the contents of an http response in current buffer."
  (save-excursion
    (goto-char (point-min))
    (let* ((status     (edts-rpc--parse-http-response-status))
           (result     (cdr status))
           (body-start (search-forward "\n\n" nil t)))
    (if body-start
        (list
         (cons 'result result)
         (cons 'body
               (edts-rpc-try-decode (buffer-substring body-start (point-max))))
        (list (cons 'result result)))))))

(defun edts-rpc--parse-http-response-status ()
  (save-excursion
    (goto-char (point-min))
    (s-split " " (buffer-substring (point) (line-end-position)))))

(defun edts-rpc-command-url (command)
  "Construct the edts command url."
  (let ((host edts-rpc-host)
        (port edts-rpc-port))
    (format "http://%s:%s/%s" host port command)))

(defun edts-rpc-encode (data)
  "Encode DATA as json."
  (let ((json-object-type 'alist)
        (json-array-type  'list))
    (json-encode (edts-alist-filter (lambda (k v) (not v)) data))))

(defun edts-rpc-try-decode (string)
  "Decode STRING from json if possible, otherwise return it as is."
  (condition-case nil
      (unless (string-equal string "")
        (let ((json-object-type 'alist)
              (json-array-type  'list)
              (json-false       nil))
          (json-read-from-string string)))
    (error string)))

(defadvice url-http-end-of-document-sentinel
  (around edts-rpc-end-of-document-sentinel (process why))
  "Workaround for url-http-end-of-document-sentinel not properly
propagating buffer-local variables when retrying a request.

http://debbugs.gnu.org/cgi/bugreport.cgi?bug=14983 will most likely solve
the issue and make this hack redundant."
  (let* ((buf (process-buffer process))
         (url-request-method "POST")
         (url-request-extra-headers
          (buffer-local-value 'url-http-extra-headers buf))
         (url-request-data (buffer-local-value 'url-http-data buf)))
    ad-do-it))
(ad-activate-regexp "edts-rpc-end-of-document-sentinel")

(provide 'edts-rpc)
