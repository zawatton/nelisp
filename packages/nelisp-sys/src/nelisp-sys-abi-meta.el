;;; nelisp-sys-abi-meta.el --- Object ABI summary metadata for nelisp-sys -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Stage 132.4: derive the `nelisp-sys' ABI summary that the backend emits
;; into produced objects (or a sidecar) and that ABI tests inspect.  The
;; summary is pure data computed from a parsed module + a target descriptor;
;; it carries no codegen state, so it is built and tested independently of
;; the native backend.
;;
;; Shape (Doc 132):
;;   (:target "x86_64-unknown-linux-gnu" :object elf64 :c-abi sysv-amd64
;;    :pointer-width 64
;;    :exports (("nl_add" :args (i32 i32) :ret i32) ...))

;;; Code:

(require 'cl-lib)
(require 'nelisp-sys)
(require 'nelisp-sys-ast)
(require 'nelisp-sys-target)

(defun nelisp-sys-abi-meta--export-name (item)
  "Return the exported symbol name string for defun ITEM, or nil.
A function is exported when its linkage is :export; the symbol is its
:export-name, defaulting to the function name."
  (when (eq (nelisp-sys-ast-prop item :linkage) :export)
    (or (nelisp-sys-ast-prop item :export-name)
        (symbol-name (nelisp-sys-ast-prop item :name)))))

(defun nelisp-sys-abi-meta-exports (module)
  "Return the export entries of MODULE: ((NAME :args (TYPE...) :ret TYPE)...).
Only defuns with :export linkage are included, in source order."
  (let ((out '()))
    (dolist (item (nelisp-sys-ast-prop module :items))
      (when (eq (nelisp-sys-ast-kind item) 'defun)
        (let ((sym (nelisp-sys-abi-meta--export-name item)))
          (when sym
            (push (list sym
                        :args (mapcar #'cdr (nelisp-sys-ast-prop item :params))
                        :ret (nelisp-sys-ast-prop item :ret))
                  out)))))
    (nreverse out)))

(defun nelisp-sys-abi-meta-module (module &optional target)
  "Return the ABI summary plist for MODULE on TARGET (triple or descriptor).
TARGET defaults to the host descriptor."
  (let ((tg (nelisp-sys-target-get (or target (nelisp-sys-target-host)))))
    (list :target (nelisp-sys-target-triple tg)
          :object (nelisp-sys-target-object-format tg)
          :c-abi (nelisp-sys-target-c-abi tg)
          :pointer-width (nelisp-sys-target-pointer-width tg)
          :exports (nelisp-sys-abi-meta-exports module))))

(defun nelisp-sys-abi-meta-to-string (meta)
  "Render ABI summary META as a stable, readable one-form string.
Used for sidecar metadata files and golden tests."
  (let ((print-quoted t))
    (prin1-to-string meta)))

(provide 'nelisp-sys-abi-meta)

;;; nelisp-sys-abi-meta.el ends here
