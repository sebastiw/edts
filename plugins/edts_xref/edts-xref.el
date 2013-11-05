;; Copyright 2013 Thomas Järvstrand <tjarvstrand@gmail.com>
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
;; xref interaction code for EDTS

;; Window configuration to be restored when quitting debug mode


(defcustom edts-xref-checks '(undefined_function_calls)
  "What xref checks EDTS should perform. A list of 0 or more of
undefined_function_calls, unexported_functions"
  :group 'edts)

(defvar edts-xref-initialized-nodes nil
  "The nodes for on which the xref-server has been initialized")

(defun edts-xref-init ()
  "Initialize edts-debug."
  (add-to-list 'edts-code-issue-types 'edts-xref)
  ;; Keys
  (define-key edts-mode-map "\C-c\C-dw" 'edts-xref-who-calls)
  (define-key edts-mode-map "\C-c\C-dW" 'edts-xref-last-who-calls)
  (add-hook 'edts-server-down-hook 'edts-xref-server-down-hook)
  (add-hook 'edts-code-after-compile-hook 'edts-xref-after-compile-hook)
  (add-hook 'edts-after-node-init-hook 'edts-xref-after-node-init-hook)
  (add-hook 'edts-node-down-hook 'edts-xref-node-down-hook))

(defun edts-xref-after-node-init-hook ()
  "Hook to run after node initialization."
  ;; Start the xref server
  (let ((node (edts-node-name)))
    (setq edts-xref-initialized-nodes (remove node edts-xref-initialized-nodes))
    (edts-plugin-call-async node
                            'edts_xref
                            'start
                            nil
                            'edts-xref-server-init-callback
                            (list node))))

(defun edts-xref-server-down-hook ()
  "Hook to run after the main edts server goes down"
  ;; Assume that we'll have to re-initialize all nodes.
  (setq edts-xref-initialized-nodes nil))

(defun edts-xref-node-down-hook (node)
  "Hook to run after a node has gone down"
  (setq edts-xref-initialized-nodes (remove node edts-xref-initialized-nodes)))

(defun edts-xref-after-compile-hook (result)
  "Hook to run after compilation of a module."
  (edts-xref-analyze-related))

(defun edts-xref-server-init-callback (body node-name)
  "Callback for when the xref server has been initialized."
  (add-to-list 'edts-xref-initialized-nodes node-name))


(defun edts-xref-analyze-related ()
  "Runs xref-checks for all live buffers related to current
buffer either by belonging to the same project or, if current buffer
does not belong to any project, being in the same directory as the
current buffer's file."
  (if (not (member (edts-node-name) edts-xref-initialized-nodes))
      (edts-log-info "Not running xref analysis on %s, server not ready yet"
                     (edts-node-name))
    (when edts-xref-checks
      (let* ((mods nil))
        (with-each-buffer-in-project (gen-sym) (eproject-root)
          (let ((mod (ferl-get-module)))
            (when mod
              (edts-face-remove-overlays '(edts-xref))
              (push mod mods))))
        (edts-xref-module-analysis-async mods)))))


(defun edts-xref-analyze-no-project ()
  "Runs xref-checks for all live buffers with its file in current
buffer's directory, on the node related to that buffer."
  (mapc
   #'(lambda (buf) (with-current-buffer buf (edts-xref-analyze))))
  (edts-code--modules-in-dir default-directory))


(defun edts-code-xref-analyze ()
  "Runs xref-checks for current buffer on the node related to that
buffer's project."
  (interactive)
  (let ((module (ferl-get-module)))
    (when module
      (edts-face-remove-overlays '(edts-xref))
      (edts-xref-module-analysis-async
       (list module) edts-xref-checks
       #'edts-xref-analysis-callback))))

(defun edts-xref-module-analysis-async (modules)
  "Run xref-checks on MODULE on the node associated with current buffer,
asynchronously. When the request terminates, call CALLBACK with the
parsed response as the single argument"
  (let* ((node (edts-node-name))
         (args `(("xref_checks" . ,(mapcar #'symbol-name edts-xref-checks))
                 ("modules"     . ,modules))))
    (edts-log-debug "fetching xref-analysis of %s async on %s" modules node)
    (edts-plugin-call-async node
                            'edts_xref
                            'analyze args
                            #'edts-xref-analysis-callback)))

(defun edts-xref-analysis-callback (analysis-res)
  (let* ((err-alist  (edts-code--issue-to-file-map analysis-res)))
    ;; Set the error list in each project-buffer
    (with-each-buffer-in-project (gen-sym) (eproject-root)
      (let ((errors (cdr (assoc (file-truename (buffer-file-name)) err-alist))))
        (edts-code--set-issues 'edts-xref (list 'error errors))
        (edts-face-update-buffer-mode-line (edts-code-buffer-status))
        (when errors
          (edts-code-display-error-overlays 'edts-xref errors))))))


(defun edts-xref-get-who-calls (module function arity)
  "Fetches a list of all function calling  MODULE:FUNCTION/ARITY on
current buffer's project node."
  (let ((args (list (cons "module"   module)
                    (cons "function" function)
                    (cons "arity"    (number-to-string arity)))))
    (edts-plugin-call (edts-node-name) 'edts_xref 'who_calls args)))

(defun edts-xref-who-calls ()
  (interactive)
  (let ((mfa (edts-mfa-at)))
    (if mfa
        (apply #'edts-xref--find-callers  mfa)
      (error "No call at point."))))

(defvar edts-xref--last-who-calls-result nil
  "The callers found during the last call to edts-who-calls")

(defun edts-xref--find-callers (module function arity)
  "Jump to any all functions calling `module':`function'/`arity' in the
current buffer's project."
  (edts-log-info "Finding callers of %s:%s/%s" module function arity)
  (let ((callers (edts-xref-get-who-calls module function arity)))
    (if (not callers)
        (error "No callers found")
      (setq edts-xref--last-who-calls-result callers)
      (edts-navigate-function-popup callers))))

(defun edts-xref-last-who-calls ()
  "Redo previous call to edts-who-calls"
  (interactive)
  (edts-log-info "Re-doing last edts-who-calls")
  (edts-navigate-function-popup edts-xref--last-who-calls-result))


(when (member 'ert features)

  (require 'edts-test)
  (edts-test-add-suite
   ;; Name
   edts-xref-suite
   ;; Setup
   (lambda ()
     (setq edts-event-inhibit t)
     (edts-test-pre-cleanup-all-buffers)
     (edts-test-setup-project edts-test-project1-directory
                              "test"
                              nil)
     (edts-rest-force-sync t))

   ;; Teardown
   (lambda (setup-config)
     (setq edts-event-inhibit nil)
     (edts-test-teardown-project edts-test-project1-directory)
     (edts-test-post-cleanup-all-buffers)
     (edts-rest-force-sync nil)))

  (edts-test-case edts-xref-suite edts-xref-analysis-test ()
    "Basic xref analysis setup test"
    (flet ((edts-node-down-request () nil))
      (let ((edts-async-node-init nil))
        (find-file (car (edts-test-project1-modules)))
        (edts-xref-module-analysis-async '("one"))
        (let* ((errors (plist-get (plist-get edts-code-buffer-issues 'edts-xref)
                                  'error)))
          (should (equal (length errors) 1))
          (should (equal (cdr (assoc 'line (car errors))) 24))
          (should (string= (cdr (assoc 'type (car errors))) "error"))
          (should (string= (cdr (assoc 'file (car errors)))
                           (file-truename (buffer-file-name))))))))

  (edts-test-case edts-xref-suite edts-xref-who-calls-test ()
    "Basic project setup test"
    (flet ((edts-node-down-request () nil))
      (let ((edts-async-node-init nil))
        (find-file (car (edts-test-project1-modules)))
        (let* ((callers  (edts-xref-get-who-calls "one_two" "one_two_fun" 1))
               (caller   (car callers))
               (module   (cdr (assoc 'module caller)))
               (function (cdr (assoc 'function caller)))
               (arity    (cdr (assoc 'arity caller)))
               (lines    (cdr (assoc 'lines caller))))
          (should (equal (length callers) 1))
          (should (equal module "one"))
          (should (equal function "one"))
          (should (equal arity 1)))))))

  (provide 'edts-xref)

