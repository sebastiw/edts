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
  ;; Keys
  (define-key edts-mode-map "\C-c\C-dw" 'edts-xref-who-calls)
  (define-key edts-mode-map "\C-c\C-dW" 'edts-xref-last-who-calls)
  (add-hook 'edts-code-after-compile-hook 'edts-xref-after-compile-hook)
  (add-hook 'edts-after-node-init-hook 'edts-xref-after-node-init-hook)
  (add-hook 'edts-node-down-hook 'edts-xref-node-down-hook))

(defun edts-xref-after-node-init-hook ()
  "Hook to run after node initialization."
  ;; Start the xref server
  (setq edts-xref-initialized-nodes
        (remove (edts-node-name) edts-xref-initialized-nodes))
  (let ((resource  (list "plugins"
                         "xref"
                         "nodes" (edts-node-name)))
        (rest-args '(("start" . "true")))
        (cb-args   `(edts-xref-server-init-callback 204 ,(edts-node-name))))
    (edts-rest-post-async resource rest-args #'edts-async-callback cb-args)))

(defun edts-xref-node-down-hook (node)
  "Hook to run after a node has gone down"
  (setq edts-xref-initialized-nodes (remove node edts-xref-initialized-nodes)))

(defun edts-xref-after-compile-hook (&optional result)
  "Hook to run after compilation of a module."
  (when (member (edts-node-name) edts-xref-initialized-nodes)
    (edts-xref-analyze-related result)))

(defun edts-xref-server-init-callback (body node-name)
  "Callback for when the xref server has been initialized."
  (add-to-list 'edts-xref-initialized-nodes node-name))

(defun edts-debug-buffer-init ()
  "edts-debug buffer-specific initialization."
  (add-to-list 'mode-line-buffer-identification
               '(edts-mode edts-debug-mode-line-info)
               't))

(defun edts-xref-analyze-related (&optional result)
  "Runs xref-checks for all live buffers related to current
buffer either by belonging to the same project or, if current buffer
does not belong to any project, being in the same directory as the
current buffer's file."
  (when (and edts-xref-checks (not (eq result 'error)))
    (interactive '(ok))
    (let* ((mods nil))
      (with-each-buffer-in-project (gen-sym) (eproject-root)
        (let ((mod (ferl-get-module)))
          (when mod
            (edts-face-remove-overlays '(edts-xref))
            (push mod mods))))
      (edts-get-module-xref-analysis-async
       mods
       edts-xref-checks
       #'edts-handle-xref-analysis-result))))


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
      (edts-get-module-xref-analysis-async
       (list module) edts-xref-checks
       #'edts-handle-xref-analysis-result))))

(defun edts-handle-xref-analysis-result (analysis-res)
  (when analysis-res
    (let* ((all-errors (cdr (assoc 'errors analysis-res)))
           (err-alist  (edts-code--issue-to-file-map all-errors)))
      ;; Set the error list in each project-buffer
      (with-each-buffer-in-project (gen-sym) (eproject-root)
        (let ((errors (cdr (assoc (buffer-file-name) err-alist))))
          (edts-code--set-issues 'edts-xref (list 'error errors))
          (edts-face-update-buffer-mode-line (edts-code-buffer-status))
          (when errors
            (edts-code-display-error-overlays 'edts-xref errors)))))))

(defun edts-get-module-xref-analysis-async (modules checks callback)
  "Run xref-checks on MODULE on the node associated with current buffer,
asynchronously. When the request terminates, call CALLBACK with the
parsed response as the single argument"
  (let* ((node-name (edts-node-name))
         (resource  (list "plugins" "xref"
                          "nodes" node-name
                          "analysis"))
         (rest-args `(("xref_checks" . ,(mapcar #'symbol-name checks))
                      ("modules"     . ,modules)))
         (cb-args   (list callback 200)))
    (edts-log-debug
     "fetching xref-analysis of %s async on %s" modules node-name)
    (edts-rest-get-async resource rest-args #'edts-async-callback cb-args)))

(defun edts-get-who-calls (module function arity)
  "Fetches a list of all function calling  MODULE:FUNCTION/ARITY on
current buffer's project node."
  (let* ((resource (list "nodes" (edts-node-name)
                         "modules" module
                         "functions" function
                         (number-to-string arity)
                         "callers"))
         (res      (edts-rest-get resource nil)))
    (if (equal (assoc 'result res) '(result "200" "OK"))
        (cdr (assoc 'body res))
        (null
         (edts-log-error "Unexpected reply: %s" (cdr (assoc 'result res)))))))



(provide 'edts-xref)
