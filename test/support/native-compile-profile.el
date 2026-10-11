;;; native-compile-profile.el --- Opt-in producer phase observer -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Observe unsealed phase boundaries only: admission owners stay unchanged.
(require 'nelisp-native-cache)
(require 'nelisp-aot-compiler)
(require 'nelisp-bytecode-native-rooted-cfg-native)
(defvar native-compile-profile--depth (make-hash-table :test 'eq))
(defun native-compile-profile--scoped-input-call (original arguments)
  "Observe metadata after the genuine scoped builder completes."
  (let ((callback (cadr arguments)))
    (funcall original (car arguments)
             (lambda (input)
               (nelisp-native-cache--stage
                (format "profile-input-code-bytes=%d-constants=%d-blocks=%d"
                        (string-bytes (plist-get input :code))
                        (length (plist-get input :constants))
                        (length (plist-get (plist-get input :frame-result) :blocks))))
               (funcall callback input)))))
(dolist (spec '((nelisp-bytecode-compiler-input-call-with-built . "input-scope")
                (nelisp-aot-compiler--parse . "lower")
                (nelisp-aot-compiler--emit-defun . "emit")
                (base64-encode-string . "base64")
                (nelisp-native-cache-load . "load")))
  (let ((original (symbol-function (car spec)))
        (name (car spec)) (label (cdr spec)))
    (fset name
          (lambda (&rest arguments)
            (if (gethash name native-compile-profile--depth)
                (apply original arguments)
              (puthash name t native-compile-profile--depth)
              (nelisp-native-cache--stage (concat "profile-" label "-start"))
              (native-compile-profile--gc-marker (concat label "-start"))
              (unwind-protect
                  (if (eq name 'nelisp-bytecode-compiler-input-call-with-built)
                      (native-compile-profile--scoped-input-call original arguments)
                    (apply original arguments))
                (native-compile-profile--gc-marker (concat label "-end"))
                (nelisp-native-cache--stage (concat "profile-" label "-end"))
                (puthash name nil native-compile-profile--depth)))))))

;; Pause clocks require a native observer unavailable in restricted sandboxes.
;; Read the existing ABI-checked collection/allocation counters instead.
(defvar native-compile-profile--gc-address :unset)
(defun native-compile-profile--gc-marker (label)
  (nelisp-native-cache--stage
   (format "profile-jit-phase=%s-attempts=%d-preparing=%S" label
           (if (boundp 'nelisp-bytecode-jit--dispatch-attempt-count)
               (symbol-value 'nelisp-bytecode-jit--dispatch-attempt-count) -1)
           (and (boundp 'nelisp-bytecode-jit--preparing-active)
                (symbol-value 'nelisp-bytecode-jit--preparing-active))))
  (when (eq native-compile-profile--gc-address :unset)
    (setq native-compile-profile--gc-address
          (condition-case nil
              (let ((address (getenv "NATIVE_COMPILE_PROFILE_GC_ADDRESS")))
                (and address (nelisp-runtime-reload-contract-matches-p)
                     (let ((value (string-to-number address)))
                       (and (> value 4096) value))))
            (error nil))))
  (nelisp-native-cache--stage
   (if native-compile-profile--gc-address
       (format "profile-gc-phase=%s-count=%d-allocated=%d-live=%d" label
               (ptr-read-u64 native-compile-profile--gc-address 24)
               (+ (ptr-read-u64 native-compile-profile--gc-address 32)
                  (ptr-read-u64 native-compile-profile--gc-address 0))
               (ptr-read-u64 native-compile-profile--gc-address 16))
     (concat "profile-gc-unavailable-phase=" label))))
