;;; edts-event.el --- Event-loop handling for Erlang-side events

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

(defvar edts-event-handlers nil)

(defun edts-event-register-handler (handler event-type)
  "Register HANDLER to receive events of type EVENT-TYPE."
  (let* ((el           (assoc event-type edts-event-handlers))
         (handlers     (cdr el))
         (new-handlers (if (member handler handlers)
                           handlers
                         (cons handler handlers))))
    (setq edts-event-handlers (cons (cons event-type new-handlers)
                                    (delq el edts-event-handlers)))))

(defun edts-event-unregister-handler (handler &optional event-type)
  "Unregister HANDLER from receiving events. Optional argument
EVENT-TYPE specifies that handler should only stop receiving events of
that type."
  (let ((event-types (if event-type
                         (list event-type)
                       (mapcar #'car edts-event-handlers))))
    (setq edts-event-handlers
          (loop for (type . handlers) in edts-event-handlers
                collect (if (member type event-types)
                            (cons type (delq handler handlers))
                          (cons type handlers))))))

(defun edts-event-listen ()
  "Start the event-listening loop."
  (let ((buf (edts-rest-get-async '("event") nil #'edts-event-listen-callback)))
    (when buf
      (set-process-query-on-exit-flag (get-buffer-process buf) nil))))

(defun edts-event-listen-callback (reply)
  "Initialize things needed to detect when a node goes down"
  (if reply
      (let* ((result     (cdr (assoc 'result reply)))
             (event      (cdr (assoc 'event (cdr (assoc 'body reply)))))
             (event-type (intern (cdr (assoc 'type event))))
             (info       (cdr (assoc 'info event))))
        (if (not (string= (car result) "200"))
            (null (edts-log-error "Unexpected reply %s" result))
          (edts-event-handle event-type info)
          (edts-event-listen)))
    ;; Assume that the server went down if reply is empty
    (edts-event-handle 'server_down)))

(defun edts-event-handlers (event-type)
  (cdr (assoc event-type edts-event-handlers)))

(defun edts-event-handle (event-type &optional event-info)
  (mapc #'(lambda (handler)
            (edts-log-debug "Calling handler %s for event %s"
                            handler
                            event-type)
            (condition-case ex
                (funcall handler event-type event-info)
              (error (edts-log-error
                      (concat "Calling event handler failed\n"
                              "Handler: %s\n"
                              "Event type: %s\n"
                              "Event info: %s\n"
                              "Error: %s")
                      handler
                      event-type
                      event-info
                      ex))))
        (edts-event-handlers event-type))
  t)

;;;;;;;;;;;;;;;;;;;;
;; Tests

(when (member 'ert features)
  (ert-deftest edts-event-register-test ()
      (let ((handlers (copy-sequence edts-event-handlers)))
        ;; Register
        (setq edts-event-handlers nil)
        (edts-event-register-handler 'dummy-handler 'dummy-type)
        (should (equal edts-event-handlers
                       '((dummy-type dummy-handler))))
        (setq edts-event-handlers handlers)))

  (ert-deftest edts-event-register-twice-test ()
      (let ((handlers (copy-sequence edts-event-handlers)))
        ;; Register
        (setq edts-event-handlers nil)
        (edts-event-register-handler 'dummy-handler 'dummy-type)
        (edts-event-register-handler 'dummy-handler 'dummy-type)
        (should (equal edts-event-handlers
                       '((dummy-type dummy-handler))))
        (setq edts-event-handlers handlers)))

  (ert-deftest edts-event-unregister-test ()
      (let ((handlers (copy-sequence edts-event-handlers)))
        ;; Register
        (setq edts-event-handlers nil)
        (edts-event-register-handler 'dummy-handler 'dummy-type)
        (should (equal edts-event-handlers
                       '((dummy-type dummy-handler))))
        (edts-event-unregister-handler 'dummy-handler 'dummy-type)
        (should (equal edts-event-handlers '((dummy-type))))
        (setq edts-event-handlers handlers)))

  (ert-deftest edts-event-register-multi-type-test ()
    (let ((handlers (copy-sequence edts-event-handlers)))
      ;; Register for two event types
      (setq edts-event-handlers nil)
      (edts-event-register-handler 'dummy-handler 'dummy-type)
      (edts-event-register-handler 'dummy-handler 'dummy-type2)
      (should (equal edts-event-handlers
                     '((dummy-type2 dummy-handler)
                       (dummy-type dummy-handler))))
      (setq edts-event-handlers handlers)))

  (ert-deftest edts-event-unregister-multi-type-test ()
    (let ((handlers (copy-sequence edts-event-handlers)))
      ;; Register for two event types and unregister for one of them
      (setq edts-event-handlers nil)
      (edts-event-register-handler 'dummy-handler 'dummy-type)
      (edts-event-register-handler 'dummy-handler 'dummy-type2)
      (should (equal edts-event-handlers
                     '((dummy-type2 dummy-handler)
                       (dummy-type dummy-handler))))
      (edts-event-unregister-handler 'dummy-handler 'dummy-type2)
      (should (equal edts-event-handlers
                     '((dummy-type2)
                       (dummy-type dummy-handler))))
      (setq edts-event-handlers handlers)))

  (ert-deftest edts-event-unregister-multi-type-test-2 ()
    (let ((handlers (copy-sequence edts-event-handlers)))
      ;; Register for two event types and unregister for all types
      (setq edts-event-handlers nil)
      (edts-event-register-handler 'dummy-handler 'dummy-type)
      (edts-event-register-handler 'dummy-handler 'dummy-type2)
      (should (equal edts-event-handlers
                     '((dummy-type2 dummy-handler)
                       (dummy-type dummy-handler))))
      (edts-event-unregister-handler 'dummy-handler)
      (should (equal edts-event-handlers
                     '((dummy-type2)
                       (dummy-type))))
      (setq edts-event-handlers handlers)))

  (ert-deftest edts-event-handlers-test ()
    (let ((handlers (copy-sequence edts-event-handlers)))
      ;; Register for two event types and unregister for all types
      (setq edts-event-handlers nil)
      (edts-event-register-handler 'dummy-handler 'dummy-type)
      (edts-event-register-handler 'dummy-handler2 'dummy-type)
      (should (equal (edts-event-handlers 'dummy-type)
                     '(dummy-handler2 dummy-handler)))
      (setq edts-event-handlers handlers)))

  (ert-deftest edts-event-handle-event-test ()
    (defvar evts)
    (setq evts nil)
    (defun dummy-handler (evt-type evt) (push evt evts))
    (defun dummy-handler2 (evt-type evt) (push evt evts))

    (let ((handlers (copy-sequence edts-event-handlers)))
      (setq edts-event-handlers nil)
      (edts-event-register-handler 'dummy-handler 'dummy-type)
      (edts-event-register-handler 'dummy-handler2 'dummy-type)

      (edts-event-handle 'dummy-type 'foo)
      (should (equal evts '(foo foo)))
      (setq edts-event-handlers handlers))))


