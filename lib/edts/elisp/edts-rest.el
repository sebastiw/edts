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
 (let ((url                       (edts-rest-resource-url resource args))
       (url-request-method        "GET")
       (url-request-extra-headers (list edts-rest-content-type-hdr)))
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
           (body   (edts-rest-decode
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
  "Url-encode a single argument."
  (concat (car arg) "=" (cdr arg)))

(defun edts-rest-encode (data)
  "Encode `data' as json."
  (let ((json-object-type 'alist)
        (json-array-type  'list))
    (json-encode data)))

(defun edts-rest-decode (data)
  "Encode `data' as json."
  (let ((json-object-type 'alist)
        (json-array-type  'list))
    (json-read-from-string data)))