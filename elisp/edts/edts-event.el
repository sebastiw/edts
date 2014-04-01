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
(require 'edts-rest)

(defvar edts-event-handlers nil
  "List of handlers for different types of events. This is an alist
where the keys are the event types (symbols) and each value is a list of
functions to call for that event type. Each function should take four
arguments: node (string) event-class (symbol) event-type (symbol) and
 event-info (alist) ")

(defvar edts-event-inhibit nil
  "If non-nil, inhibit the event-loop")

(defun edts-event-register-handler (handler event-class)
  "Register HANDLER to receive events of class EVENT-CLASS."
  (let* ((el           (assoc event-class edts-event-handlers))
         (handlers     (cdr el))
         (new-handlers (if (member handler handlers)
                           handlers
                         (cons handler handlers))))
    (setq edts-event-handlers (cons (cons event-class new-handlers)
                                    (delq el edts-event-handlers)))))

(defun edts-event-unregister-handler (handler &optional event-class)
  "Unregister HANDLER from receiving events. Optional argument
EVENT-CLASS specifies that handler should only stop receiving events of
that class."
  (let ((event-classs (if event-class
                         (list event-class)
                       (mapcar #'car edts-event-handlers))))
    (setq edts-event-handlers
          (loop for (class . handlers) in edts-event-handlers
                collect (if (member class event-classs)
                            (cons class (delq handler handlers))
                          (cons class handlers))))))

(defun edts-event-listen ()
  "Start the event-listening loop."
  (unless edts-event-inhibit
    (let ((buf (edts-rest-get-async '("event")
                                    nil
                                    #'edts-event-listen-callback
                                    nil
                                    t)))
      (when buf
        (set-process-query-on-exit-flag (get-buffer-process buf) nil)))))

(defun edts-event-listen-callback (reply)
  "Callback to be run when an event is received."
  (if reply
      (let* ((result (cdr (assoc 'result reply)))
             (event  (cdr (assoc 'event (cdr (assoc 'body reply)))))
             (node   (cdr (assoc 'node event)))
             (class  (intern (cdr (assoc 'class event))))
             (type   (intern (cdr (assoc 'type event))))
             (info   (cdr (assoc 'info event))))
        (edts-log-debug "Received event from %s: %s %s %s" node class type info)
        (if (not (string= (car result) "200"))
            (null (edts-log-error "Unexpected reply %s" result))
          (edts-event-handle node class type info)
          (edts-event-listen)))
    ;; Assume that the server went down if reply is empty
    (edts-event-handle nil 'edts 'server_down nil)))

(defun edts-event-handlers (event-class)
  (cdr (assoc event-class edts-event-handlers)))

(defun edts-event-handle (node class type info)
  (mapc #'(lambda (handler)
            (edts-log-debug "Calling handler %s for event %s" handler class)
            (condition-case ex
                (funcall handler node class type info)
              (error (edts-log-error
                      (concat "Calling event handler failed\n"
                              "Handler: %s\n"
                              "Event class: %s\n"
                              "Event info: %s\n"
                              "Error: %s")
                      handler
                      class
                      info
                      ex))))
        (edts-event-handlers class))
  t)

;;;;;;;;;;;;;;;;;;;;
;; Tests

(when (member 'ert features)
  (ert-deftest edts-event-register-test ()
      (let ((handlers (copy-sequence edts-event-handlers)))
        ;; Register
        (setq edts-event-handlers nil)
        (edts-event-register-handler 'dummy-handler 'dummy-class)
        (should (equal edts-event-handlers
                       '((dummy-class dummy-handler))))
        (setq edts-event-handlers handlers)))

  (ert-deftest edts-event-register-twice-test ()
      (let ((handlers (copy-sequence edts-event-handlers)))
        ;; Register
        (setq edts-event-handlers nil)
        (edts-event-register-handler 'dummy-handler 'dummy-class)
        (edts-event-register-handler 'dummy-handler 'dummy-class)
        (should (equal edts-event-handlers
                       '((dummy-class dummy-handler))))
        (setq edts-event-handlers handlers)))

  (ert-deftest edts-event-unregister-test ()
      (let ((handlers (copy-sequence edts-event-handlers)))
        ;; Register
        (setq edts-event-handlers nil)
        (edts-event-register-handler 'dummy-handler 'dummy-class)
        (should (equal edts-event-handlers
                       '((dummy-class dummy-handler))))
        (edts-event-unregister-handler 'dummy-handler 'dummy-class)
        (should (equal edts-event-handlers '((dummy-class))))
        (setq edts-event-handlers handlers)))

  (ert-deftest edts-event-register-multi-class-test ()
    (let ((handlers (copy-sequence edts-event-handlers)))
      ;; Register for two event classs
      (setq edts-event-handlers nil)
      (edts-event-register-handler 'dummy-handler 'dummy-class)
      (edts-event-register-handler 'dummy-handler 'dummy-class2)
      (should (equal edts-event-handlers
                     '((dummy-class2 dummy-handler)
                       (dummy-class dummy-handler))))
      (setq edts-event-handlers handlers)))

  (ert-deftest edts-event-unregister-multi-class-test ()
    (let ((handlers (copy-sequence edts-event-handlers)))
      ;; Register for two event classs and unregister for one of them
      (setq edts-event-handlers nil)
      (edts-event-register-handler 'dummy-handler 'dummy-class)
      (edts-event-register-handler 'dummy-handler 'dummy-class2)
      (should (equal edts-event-handlers
                     '((dummy-class2 dummy-handler)
                       (dummy-class dummy-handler))))
      (edts-event-unregister-handler 'dummy-handler 'dummy-class2)
      (should (equal edts-event-handlers
                     '((dummy-class2)
                       (dummy-class dummy-handler))))
      (setq edts-event-handlers handlers)))

  (ert-deftest edts-event-unregister-multi-class-test-2 ()
    (let ((handlers (copy-sequence edts-event-handlers)))
      ;; Register for two event classs and unregister for all classs
      (setq edts-event-handlers nil)
      (edts-event-register-handler 'dummy-handler 'dummy-class)
      (edts-event-register-handler 'dummy-handler 'dummy-class2)
      (should (equal edts-event-handlers
                     '((dummy-class2 dummy-handler)
                       (dummy-class dummy-handler))))
      (edts-event-unregister-handler 'dummy-handler)
      (should (equal edts-event-handlers
                     '((dummy-class2)
                       (dummy-class))))
      (setq edts-event-handlers handlers)))

  (ert-deftest edts-event-handlers-test ()
    (let ((handlers (copy-sequence edts-event-handlers)))
      ;; Register for two event classs and unregister for all classs
      (setq edts-event-handlers nil)
      (edts-event-register-handler 'dummy-handler 'dummy-class)
      (edts-event-register-handler 'dummy-handler2 'dummy-class)
      (should (equal (edts-event-handlers 'dummy-class)
                     '(dummy-handler2 dummy-handler)))
      (setq edts-event-handlers handlers)))

  (ert-deftest edts-event-handle-event-test ()
    (defvar edts-evts)
    (setq edts-evts nil)
    (defun dummy-handler  (node evt-class evt evt-info) (push evt edts-evts))
    (defun dummy-handler2 (node evt-class evt evt-info) (push evt edts-evts))

    (let ((handlers (copy-sequence edts-event-handlers)))
      (setq edts-event-handlers nil)
      (edts-event-register-handler 'dummy-handler 'dummy-class)
      (edts-event-register-handler 'dummy-handler2 'dummy-class)

      (edts-event-handle "node" 'dummy-class 'foo nil)
      (should (equal edts-evts '(foo foo)))
      (setq edts-event-handlers handlers))))

(provide 'edts-event)
