;; Copyright 2012 Thomas Järvstrand <tjarvstrand@gmail.com>
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
;; auto-complete source for built-in erlang functions.

(require 'auto-complete)
(require 'ferl)

(eval-and-compile
  (defvar edts-complete-built-in-functions
    '("abs/1"
      "adler32/1"
      "adler32_combine/3"
      "alive/1"
      "apply/2"
      "apply/3"
      "atom_to_binary/2"
      "atom_to_list/1"
      "binary_to_atom/2"
      "binary_to_existing_atom/2"
      "binary_to_list/1"
      "binary_to_list/3"
      "binary_to_term/1"
      "binary_to_term/2"
      "bit_size/1"
      "bitstring_to_list/1"
      "byte_size/1"
      "check_process_code/2"
      ;; "contact_binary" removed?
      "crc32/1"
      "crc32/2"
      "crc32_combine/3"
      "date/0"
      "decode_packet/3"
      "delete_module/1"
      "disconnect_node/1"
      "element/2"
      "erase/0"
      "erase/1"
      "exit/1"
      "exit/2"
      "float/1"
      "float_to_list/1"
      "garbage_collect/0"
      "garbage_collect/1"
      "get/1"
      "get_keys/1"
      "group_leader/0"
      "group_leader/2"
      "halt/0"
      "halt/1"
      "halt/2"
      "hd/1"
      "integer_to_list/1"
      "integer_to_list/2"
      ;; "internal_bif" removed?
      "iolist_size/1"
      "iolist_to_binary/1"
      "is_alive/0"
      "is_atom/1"
      "is_binary/1"
      "is_bitstring/1"
      "is_boolean/1"
      "is_float/1"
      "is_function/1"
      "is_function/2"
      "is_integer/1"
      "is_list/1"
      "is_number/1"
      "is_pid/1"
      "is_port/1"
      "is_process_alive/1"
      "is_record/2"
      "is_record/3"
      "is_reference/1"
      "is_tuple/1"
      "length/1"
      "link/1"
      "list_to_atom/1"
      "list_to_binary/1"
      "list_to_bitstring/1"
      "list_to_existing_atom/1"
      "list_to_float/1"
      "list_to_integer/1"
      "list_to_pid/1"
      "list_to_tuple/1"
      "load_module/2"
      "make_ref/0"
      "module_loaded/1"
      "monitor_node/2"
      "monitor_node/3"
      "node/0"
      "node/1"
      ;; "node_link" removed?
      ;; "node_unlink" removed?
      "nodes/0"
      "nodes/1"
      ;; "notalive" removed?
      "now/0"
      "open_port/2"
      "pid_to_list/1"
      "port_close/1"
      "port_command/2"
      "port_command/3"
      "port_connect/2"
      "port_control/3"
      "pre_loaded/0"
      "process_flag/2"
      "process_flag/3"
      "process_info/1"
      "process_info/2"
      "processes/0"
      "purge_module/1"
      "put/2"
      "register/2"
      "registered/0"
      "round/1"
      "self/0"
      "setelement/3"
      "size/1"
      "spawn/1"
      "spawn/2"
      "spawn/3"
      "spawn/4"
      "spawn_link/1"
      "spawn_link/2"
      "spawn_link/3"
      "spawn_link/4"
      "spawn_monitor/1"
      "spawn_monitor/3"
      "spawn_opt/2"
      "spawn_opt/3"
      "spawn_opt/4"
      "spawn_opt/5"
      "split_binary/2"
      "statistics/1"
      "term_to_binary/1"
      "term_to_binary/2"
      "throw/1"
      "time/0"
      "tl/1"
      "trunc/1"
      "tuple_size/1"
      "tuple_to_list/1"
      "unlink/1"
      "unregister/1"
      "whereis/1")
    "Erlang built-in functions (BIFs)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Source

(defvar edts-complete-built-in-function-source
  '((candidates . edts-complete-built-in-function-candidates)
    (document   . edts-complete-built-in-function-doc)
    (symbol     . "f")
    (requires   . nil)
    (limit      . nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Candidate functions

(defun edts-complete-built-in-function-candidates ()
  (case (edts-complete-point-inside-quotes)
    ('double-quoted  nil) ; Don't complete inside strings
    ('single-quoted (edts-complete-single-quoted-built-in-function-candidates))
    ('none          (edts-complete-normal-built-in-function-candidates))))

(defun edts-complete-normal-built-in-function-candidates ()
  "Produces the completion list for normal (unqoted) local functions."
  (when (edts-complete-built-in-function-p)
    (edts-log-debug "completing built-in functions")
    (let* ((completions edts-complete-built-in-functions))
      (edts-log-debug "completing built-in functions done")
      completions)))

(defun edts-complete-single-quoted-built-in-function-candidates ()
  "Produces the completion for single-qoted erlang bifs, Same as normal
candidates, except we single-quote-terminate candidates."
  (mapcar
   #'edts-complete-single-quote-terminate
   edts-complete-normal-built-in-function-candidates))

(defun edts-complete-built-in-function-doc (candidate)
  (let* ((split  (split-string candidate "/"))
         (function   (car split))
         (arity  (string-to-int (cadr split))))
    (edts-doc-extract-man-entry edts-erl-doc-root "erlang" function arity)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conditions
;;

(defun edts-complete-built-in-function-p ()
  "Returns non-nil if the current `ac-prefix' can be completed with a built-in
function."
  (let ((preceding (edts-complete-term-preceding-char)))
    (and
     (not (equal ?? preceding))
     (not (equal ?# preceding))
     ; qualified calls to built-in functions are handled by the
     ; exported-function source
     (not (equal ?: preceding))
     (string-match erlang-atom-regexp ac-prefix))))

(provide 'edts-complete-built-in-function-source)
