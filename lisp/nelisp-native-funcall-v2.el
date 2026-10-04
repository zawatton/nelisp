;;; nelisp-native-funcall-v2.el --- Generic rooted evaluator ABI -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'cl-lib)
(defconst nelisp-native-funcall-v2-version "nelisp-native-funcall-v2-1")
(let ((descriptor
       '(:version "nelisp-native-funcall-v2-1" :name "nl_native_funcall_v2"
         :kind func :arity 6 :params (u64 u64 u64 u64 u64 u64) :return u64
         :root-limit 256 :arguments contiguous :scratch-count 4
         :exit-offset 1 :exit-base 1024 :exit-kinds (1 2)
         :ownership reauthenticate :stash publish-before-clear)))
(defun nelisp-native-funcall-v2-descriptor ()
  "Return fresh source-owned bounds, signature and exit semantics."
  (copy-tree descriptor)))
(defun nelisp-native-funcall-v2-hash ()
  "Bind all generic evaluator ABI semantics."
  (let ((print-length nil) (print-level nil))
    (secure-hash 'sha256 (prin1-to-string (nelisp-native-funcall-v2-descriptor)))))
(let ((primitives '((64 car 1) (65 cdr 1) (66 cons 2))))
(defun nelisp-native-funcall-v2-primitive (opcode)
  "Return the canonical runtime primitive and arity for OPCODE."
  (copy-tree (assq opcode primitives))))
(let ((lookup (symbol-function 'symbol-function))
      (same (symbol-function 'eq))
      (originals (mapcar (lambda (name) (cons name (symbol-function name))) '(car cdr cons))))
(defun nelisp-native-funcall-v2-initializer (name)
  "Materialize only a canonical frozen VM primitive, never a public function cell."
  (unless (assq name originals) (error "Unknown funcall primitive initializer"))
  (if (fboundp 'nelisp--eval-source-string)
      ;; Builtin values are runtime evaluator tokens, not a caller certificate.
      (list 'builtin name)
    (cdr (assq name originals)))))
(defun nelisp-native-funcall-v2-reference (function arguments)
  "Lisp reference for the evaluator entry; roots are an infrastructure concern."
  (apply function arguments))
(defun nelisp-native-funcall-v2-copy-form (inputs roots body)
  "Emit authenticated full-slot copies before BODY; arguments may be phi indices."
  (let ((result body) (index (length inputs)))
    (while (> index 0)
      (setq index (1- index))
      (let ((source (intern (format "f1_source_%d" index)))
            (destination (intern (format "f1_destination_%d" index))))
        (setq result
              `(let ((,source (extern-call nl_root_pin_slot_v2 env ticket ,(nth index inputs) 0 0 0))
                     (,destination (extern-call nl_root_pin_slot_v2 env ticket ,(nth index roots) 0 0 0)))
                 (if (or (= ,source 0) (= ,destination 0)) 2
                   (progn
                     ,@(mapcar (lambda (offset)
                                 `(ptr-write-u64 ,destination ,offset (ptr-read-u64 ,source ,offset)))
                               '(0 8 16 24))
                     ,result)))))) result))
(defun nelisp-native-funcall-v2-emit (operation function inputs continuation)
  "Stage canonical OPERATION operands, call once and preserve its SSA result."
  (let* ((roots (plist-get operation :staging-roots))
         (result (plist-get operation :result-root))
         (status (intern (format "f1_status_%d" (plist-get operation :pc))))
         (success (nelisp-native-funcall-v2-copy-form
                   (list result) (list (plist-get operation :output-root)) continuation)))
    (nelisp-native-funcall-v2-copy-form
     inputs roots
     `(let ((,status (extern-call nl_native_funcall_v2 env ticket ,function
                                 ,(or (car roots) 1) ,(length inputs) ,result)))
        (if (= ,status 0) ,success ,status)))))
(provide 'nelisp-native-funcall-v2)
