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

(require 'edts)
(require 'edts-api)
(require 'edts-code)
(require 'edts-face)
(require 'edts-log)
(require 'edts-navigate)
(require 'edts-plugin)


(defcustom edts-xref-checks '(undefined_function_calls)
  "What xref checks EDTS should perform. A list of 0 or more of
undefined_function_calls, unexported_functions"
  :type '(set
	  (const undefined_function_calls)
	  (const unexported_functions))
  :group 'edts)

(defvar edts-xref-initialized-nodes nil
  "The nodes for on which the xref-server has been initialized")

(defun edts-xref-init ()
  "Initialize edts-debug."
  (add-to-list 'edts-code-issue-types 'edts-xref)
  ;; Keys
  (define-key edts-mode-map "\C-c\C-dw" 'edts-xref-who-calls)
  (define-key edts-mode-map "\C-c\C-dW" 'edts-xref-last-who-calls)
  (add-to-list 'edts-project-valid-properties :xref-file-whitelist)
  (add-to-list 'edts-project-valid-properties :xref-error-whitelist)
  (add-hook 'edts-api-server-down-hook 'edts-xref-server-down-hook)
  (add-hook 'edts-code-after-compile-hook 'edts-xref-after-compile-hook)
  (add-hook 'edts-api-after-node-init-hook 'edts-xref-after-node-init-hook)
  (add-hook 'edts-api-node-down-hook 'edts-xref-node-down-hook))

(defun edts-xref-after-node-init-hook ()
  "Hook to run after node initialization."
  ;; Start the xref server
  (let ((node (edts-api-node-name)))
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
  (if (not (member (edts-api-node-name) edts-xref-initialized-nodes))
      (edts-log-info "Not running xref analysis on %s, server not ready yet"
                     (edts-api-node-name))
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
   #'(lambda (buf) (with-current-buffer buf (edts-xref-analyze)))
  (edts-code-directory-module-buffers default-directory)))


(defun edts-xref-analyze ()
  "Runs xref-checks for current buffer on the node related to that
buffer's project."
  (interactive)
  (let ((module (ferl-get-module)))
    (when module
      (edts-face-remove-overlays '(edts-xref))
      (edts-xref-module-analysis-async (list module)))))

(defun edts-xref-module-analysis-async (modules)
  "Run xref-checks on MODULE on the node associated with current buffer,
asynchronously. When the request terminates, call CALLBACK with the
parsed response as the single argument"
  (let* ((node (edts-api-node-name))
         (args `(("xref_checks" . ,(mapcar #'symbol-name edts-xref-checks))
                 ("modules"     . ,modules))))
    (edts-log-debug "fetching xref-analysis of %s async on %s" modules node)
    (edts-plugin-call-async node
                            'edts_xref
                            'analyze args
                            #'edts-xref-analysis-callback)))

(defun edts-xref-analysis-callback (analysis-res)
  (let ((err-alist (edts-xref-apply-whitelists
                    (edts-code--issue-to-file-map analysis-res))))
    ;; Set the error list in each project-buffer
    (with-each-buffer-in-project (gen-sym) (eproject-root)
      (when (buffer-file-name)
        (let ((errs (cdr (assoc (file-truename (buffer-file-name)) err-alist))))
          (edts-code--set-issues 'edts-xref (list 'error errs))
          (edts-face-update-buffer-mode-line (edts-code-buffer-status))
          (when errs
            (edts-code-display-error-overlays 'edts-xref errs)))))))

(defun edts-xref-apply-whitelists (errs)
  "ERRS is an alist of (FILE . FILE-ERRORS) where FILE is a filename and
FILE-ERRORS is in turn a list of alists, each one describing an error.
This function applies the `:xref-error-whitelist' and
`:xref-file-whitelist' to each of the errors in each FILE-ERRORS and
returns the filtered ERRS alist."
  (-map #'(lambda (file-errs)
            (cons (car file-errs)
                  (-filter #'(lambda (err)
                               (and
                                (not (edts-xref--desc-whitelisted-p err))
                                (not (edts-xref--file-whitelisted-p err))))
                           (cdr file-errs))))
        errs))

(defun edts-xref--desc-whitelisted-p (err)
  (let ((desc (cdr (assoc 'description err)))
        (regexps (eproject-attribute :xref-error-whitelist)))
    (-any? (lambda (re) (string-match re desc)) regexps)))

(defun edts-xref--file-whitelisted-p (err)
  (let ((file (cdr (assoc 'file err)))
        (regexps (eproject-attribute :xref-file-whitelist)))
    (-any? (lambda (re) (string-match re file)) regexps)))


(defun edts-xref-get-who-calls (module function arity)
  "Fetches a list of all function calling  MODULE:FUNCTION/ARITY on
current buffer's project node."
  (let ((args (list (cons "module"   module)
                    (cons "function" function)
                    (cons "arity"    (number-to-string arity)))))
    (edts-plugin-call (edts-api-node-name) 'edts_xref 'who_calls args)))

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

(provide 'edts-xref)

