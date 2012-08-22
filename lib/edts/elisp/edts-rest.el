;; Copyright 2012 Thomas Järvstrand <tjarvstrand@gmail.com>
;;
;; This file is part of EDTS.
;;
;; EDTS is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; EDTS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with EDTS. If not, see <http://www.gnu.org/licenses/>.
;;
;; Rudimentary project support for edts so that we can relate buffers to
;; projects and communicate with the correct nodes.
;;
;; edts' library for communicating with it's erlang node.

(require 'url)
(require 'json)

(defconst edts-rest-host "localhost"
  "The host where the edts erlang node is running.")

(defconst edts-rest-port 4587
  "The port on which the edts erlang node's rest-api is available.")

(defconst edts-rest-content-type-hdr '("Content-Type" . "application/json"))


(defun edts-rest-get (resource args)
  "Send a get request to resource with args"
  (edts-rest-request "GET" resource args)

(defun edts-rest-post (resource args)
  "Send a post request to resource with args"
  (edts-rest-request "POST" resource args))

(defun edts-rest-request (method resource args)
  "Send a get request to resource with args"
 (let ((url                       (edts-rest-resource-url resource args))
       (url-request-method        method)
       (url-request-extra-headers (list edts-rest-content-type-hdr))
       (url-show-status           nil))
   (with-current-buffer (url-retrieve-synchronously url)
     (edts-rest-parse-http-response))))


(defun my-switch-to-url-buffer (status)
      "Switch to the buffer returned by `url-retreive'.
    The buffer contains the raw HTTP response sent by the server."
      (switch-to-buffer (current-buffer)))

(defun edts-rest-parse-http-response ()
  (save-excursion
    (goto-char (point-min))
    (let* ((status (split-string (buffer-substring (point) (point-at-eol))))
           (result (cdr status))
           (body   (edts-rest-try-decode
                    (buffer-substring (search-forward "\n\n") (point-max)))))
      (list
       (cons 'result result)
       (cons 'body   body)))))

(defun edts-rest-resource-url (resource args)
  "Construct the edts resource url."
  (let ((host edts-rest-host)
        (port edts-rest-port)
        (path (mapconcat #'identity resource "/"))
        (args (mapconcat #'edts-rest-encode-arg args "&")))
    (format "http://%s:%s/%s?%s" host port path args)))

(defun edts-rest-encode-arg (arg)
  (concat (car arg) "=" (cdr arg)))

(defun edts-rest-encode (data)
  "Encode `data' as json."
  (let ((json-object-type 'alist)
        (json-array-type  'list))
    (json-encode data)))

(defun edts-rest-try-decode (string)
  "Decode `string' from json if possible, otherwise return it as is."
  (condition-case nil
      (unless (string-equal string "")
        (let ((json-object-type 'alist)
              (json-array-type  'list))
          (json-read-from-string string)))
    (error string)))

(provide 'edts-rest)
