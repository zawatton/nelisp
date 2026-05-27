;;; nelisp-sys-target.el --- Target descriptors for nelisp-sys -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Stage 132.1: structured target descriptors.  The compiler must know
;; exactly what target it is producing (pointer width, endian, object
;; format, C ABI, alignment, entry symbol) before it can credibly replace
;; C.  All target-specific facts live HERE, in the descriptor, never
;; scattered through the layout/backend code, and never taken from the host
;; (`sizeof', host endian, host calling convention) when cross-compiling.
;;
;; A descriptor is an immutable plist.  Callers read it through the typed
;; accessors below rather than poking raw keys, so the descriptor shape can
;; evolve without breaking call sites.

;;; Code:

(require 'cl-lib)
(require 'nelisp-sys)

(define-error 'nelisp-sys-target-error
  "nelisp-sys unknown or invalid target" 'nelisp-sys-error)

(defconst nelisp-sys-target--descriptors
  '(("x86_64-unknown-linux-gnu"
     :triple "x86_64-unknown-linux-gnu"
     :arch x86_64 :os linux :env gnu
     :object elf64 :endian little
     :pointer-width 64 :pointer-align 8
     :stack-align 16 :max-align 16
     :c-abi sysv-amd64 :entry-symbol "_start")
    ("aarch64-unknown-linux-gnu"
     :triple "aarch64-unknown-linux-gnu"
     :arch aarch64 :os linux :env gnu
     :object elf64 :endian little
     :pointer-width 64 :pointer-align 8
     :stack-align 16 :max-align 16
     :c-abi aapcs64 :entry-symbol "_start")
    ("x86_64-apple-darwin"
     :triple "x86_64-apple-darwin"
     :arch x86_64 :os macos :env nil
     :object mach-o64 :endian little
     :pointer-width 64 :pointer-align 8
     :stack-align 16 :max-align 16
     :c-abi sysv-amd64 :entry-symbol "start")
    ("aarch64-apple-darwin"
     :triple "aarch64-apple-darwin"
     :arch aarch64 :os macos :env nil
     :object mach-o64 :endian little
     :pointer-width 64 :pointer-align 8
     :stack-align 16 :max-align 16
     :c-abi aapcs64 :entry-symbol "start")
    ("x86_64-pc-windows-msvc"
     :triple "x86_64-pc-windows-msvc"
     :arch x86_64 :os windows :env msvc
     :object pe-coff :endian little
     :pointer-width 64 :pointer-align 8
     :stack-align 16 :max-align 16
     :c-abi win64 :entry-symbol "mainCRTStartup"))
  "Registry of supported nelisp-sys targets, keyed by triple string.
Each value is an immutable descriptor plist.  This is the single
source of truth for target-specific ABI facts (Doc 132).")

(defun nelisp-sys-target-list ()
  "Return the list of supported target triple strings."
  (mapcar #'car nelisp-sys-target--descriptors))

(defun nelisp-sys-target-p (triple)
  "Return non-nil when TRIPLE names a supported target."
  (and (assoc triple nelisp-sys-target--descriptors) t))

(defun nelisp-sys-target-get (triple)
  "Return the descriptor plist for TRIPLE, or signal `nelisp-sys-target-error'.
TRIPLE may be a triple string or an already-resolved descriptor plist
\(in which case it is validated and returned)."
  (cond
   ((and (consp triple) (keywordp (car triple))
         (plist-member triple :triple))
    triple)
   ((stringp triple)
    (or (cdr (assoc triple nelisp-sys-target--descriptors))
        (signal 'nelisp-sys-target-error
                (list (format "unknown target triple: %s" triple)))))
   (t (signal 'nelisp-sys-target-error
              (list (format "not a target: %S" triple))))))

;;; Typed accessors.

(defun nelisp-sys-target-triple (target)
  "Return the triple string of TARGET (triple or descriptor)."
  (plist-get (nelisp-sys-target-get target) :triple))

(defun nelisp-sys-target-arch (target)
  "Return the arch symbol of TARGET (e.g. `x86_64', `aarch64')."
  (plist-get (nelisp-sys-target-get target) :arch))

(defun nelisp-sys-target-os (target)
  "Return the OS symbol of TARGET (e.g. `linux', `macos', `windows')."
  (plist-get (nelisp-sys-target-get target) :os))

(defun nelisp-sys-target-env (target)
  "Return the environment symbol of TARGET, or nil."
  (plist-get (nelisp-sys-target-get target) :env))

(defun nelisp-sys-target-object-format (target)
  "Return the object-format symbol of TARGET (`elf64'/`mach-o64'/`pe-coff')."
  (plist-get (nelisp-sys-target-get target) :object))

(defun nelisp-sys-target-endian (target)
  "Return the endianness symbol of TARGET (`little' or `big')."
  (plist-get (nelisp-sys-target-get target) :endian))

(defun nelisp-sys-target-pointer-width (target)
  "Return the pointer width of TARGET in bits."
  (plist-get (nelisp-sys-target-get target) :pointer-width))

(defun nelisp-sys-target-pointer-align (target)
  "Return the pointer alignment of TARGET in bytes."
  (plist-get (nelisp-sys-target-get target) :pointer-align))

(defun nelisp-sys-target-stack-align (target)
  "Return the stack alignment of TARGET in bytes."
  (plist-get (nelisp-sys-target-get target) :stack-align))

(defun nelisp-sys-target-max-align (target)
  "Return the maximum natural alignment of TARGET in bytes."
  (plist-get (nelisp-sys-target-get target) :max-align))

(defun nelisp-sys-target-c-abi (target)
  "Return the C ABI symbol of TARGET (e.g. `sysv-amd64', `win64')."
  (plist-get (nelisp-sys-target-get target) :c-abi))

(defun nelisp-sys-target-entry-symbol (target)
  "Return the freestanding entry symbol name string of TARGET."
  (plist-get (nelisp-sys-target-get target) :entry-symbol))

(defun nelisp-sys-target-host ()
  "Return the descriptor for the best-guess host target.
Used by the backend adapter as the default codegen target.  Only the
host arch/OS is consulted; cross targets must be named explicitly."
  (let* ((arch (cond ((string-match-p "aarch64\\|arm64" system-configuration)
                      'aarch64)
                     (t 'x86_64)))
         (triple (cond
                  ((eq arch 'aarch64) "aarch64-unknown-linux-gnu")
                  ((string-match-p "darwin" system-configuration)
                   "x86_64-apple-darwin")
                  ((string-match-p "mingw\\|windows\\|msvc" system-configuration)
                   "x86_64-pc-windows-msvc")
                  (t "x86_64-unknown-linux-gnu"))))
    (nelisp-sys-target-get triple)))

(provide 'nelisp-sys-target)

;;; nelisp-sys-target.el ends here
