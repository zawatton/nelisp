;;; nelisp-eln-specbind-integration-test.el --- gateway specpdl unwind tests -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Genuine native .eln code can call the authenticated `specbind' freloc
;; primitive (index 12) before calling back into Lisp; if that callback
;; throws or signals past the whole native frame, GNU's own `unbind_to'
;; never runs (this bridge treats the native frame as an opaque call, not
;; a stack of Lisp `unwind-protect' forms), so the binding would leak.
;; `nelisp-eln-callable-import--call-unary'/`--call-chain' now capture
;; `nelisp-eln-runtime-services-specpdl-depth' at entry and force-unbind
;; back to it in their own cleanup, before any re-signal.  These tests
;; drive that fix directly against the real specbind/unbind-n
;; implementation, mocking only the native/FFI floor exactly like
;; `nelisp-eln-callable-import-test.el' already does.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-eln-callable-import)

(defvar nelisp-eln-specbind-integration-test--var nil)

(defun nelisp-eln-specbind-integration-test--run (behavior)
  "Call `--call-unary' with a mocked native floor and real BEHAVIOR.
BEHAVIOR runs as the callback implementation with the real specbind
machinery available; returns (OUTCOME DEPTH-DELTA), where DEPTH-DELTA
is `nelisp-eln-runtime-services-specpdl-depth' immediately after the
call minus its value immediately before."
  (let ((nelisp-eln-callable-import--frames nil)
        (before (nelisp-eln-runtime-services-specpdl-depth)))
    (cl-letf (((symbol-function 'nelisp-eln-system-loader-validate-function-capability)
               (lambda (cap) cap))
              ((symbol-function 'nelisp-eln-objects-create) (lambda () (vector 'unit)))
              ((symbol-function 'nelisp-eln-objects-encode) (lambda (_unit value) value))
              ((symbol-function 'nelisp-eln-objects-activation-acquire)
               (lambda (_unit) (vector 'activation)))
              ((symbol-function 'nelisp-eln-objects-activation-decode)
               (lambda (_token word) word))
              ((symbol-function 'nelisp-eln-objects-activation-release) (lambda (_t) t))
              ((symbol-function 'nelisp-eln-objects-release) (lambda (_u) t))
              ((symbol-function 'nelisp-eln-raw-call-context-create) (lambda () 'ctx))
              ((symbol-function 'nelisp-eln-raw-call-context-release) (lambda (_c) t))
              ((symbol-function 'nl-ffi-memory-release) (lambda (_o) t))
              ((symbol-function 'nelisp--native-env) (lambda () 'env))
              ((symbol-function 'nelisp-native-load--symbol-addr)
               (lambda (name) (cond ((equal name "nelisp_eln_callback_context_push") 9001)
                                    ((equal name "wf_bytecode_call_gateway") 9002)
                                    ((equal name "nl_eln_callback7_context") 9003)
                                    ((equal name "nelisp_eln_callback_context_pop") 9004)
                                    (t 0))))
              ((symbol-function 'nelisp-native-load--pin-begin) (lambda (_e) 'marker))
              ((symbol-function 'nelisp-native-load--pin-reserve) (lambda (_e _m) 9100))
              ((symbol-function 'nelisp-native-load-box) (lambda (_s _f) t))
              ((symbol-function 'nelisp-native-load--pin-end) (lambda (_e _m) t))
              ((symbol-function 'ptr-read-u64) (lambda (_a _o) 0))
              ((symbol-function 'ptr-write-u64) (lambda (_a _o _v) t))
              ((symbol-function 'ptr-call)
               (lambda (address &rest _a) (if (memq address '(9001 9004)) 1 42)))
              ((symbol-function 'nelisp-eln-abi-read-word)
               (lambda (_address offset) (if (= offset 0) 5 0)))
              ((symbol-function 'nelisp-eln-raw-call-word)
               (lambda (_context _address _words)
                 (let ((result (nelisp-eln-callable-import--dispatch 8192)))
                   (+ (car result) (ash (cdr result) 32))))))
      (let ((outcome
             (condition-case err
                 (list 'return (nelisp-eln-callable-import--call-unary
                                '(cap nil nil 0) behavior 5))
               (error (list 'error err))
               (quit (list 'quit err)))))
        (list outcome (- (nelisp-eln-runtime-services-specpdl-depth) before))))))

(ert-deftest nelisp-eln-specbind-integration-error-past-native-frame-unwinds ()
  "A specbind left open by an errored callback is force-closed."
  (let ((nelisp-eln-specbind-integration-test--var 'original))
    (pcase-let ((`(,outcome ,delta)
                 (nelisp-eln-specbind-integration-test--run
                  (lambda (_x)
                    (nelisp-eln-runtime-services-specbind
                     'nelisp-eln-specbind-integration-test--var 'leaked)
                    (error "boom-marker")))))
      (should (equal outcome '(error (error "boom-marker"))))
      (should (equal delta 0))
      (should (eq nelisp-eln-specbind-integration-test--var 'original)))))

(ert-deftest nelisp-eln-specbind-integration-quit-past-native-frame-unwinds ()
  (let ((nelisp-eln-specbind-integration-test--var 'original))
    (pcase-let ((`(,outcome ,delta)
                 (nelisp-eln-specbind-integration-test--run
                  (lambda (_x)
                    (nelisp-eln-runtime-services-specbind
                     'nelisp-eln-specbind-integration-test--var 'leaked)
                    (signal 'quit nil)))))
      (should (equal outcome '(quit (quit))))
      (should (equal delta 0))
      (should (eq nelisp-eln-specbind-integration-test--var 'original)))))

(ert-deftest nelisp-eln-specbind-integration-normal-return-also-force-closed ()
  "A normal return also lands at entry depth.  In genuine native code
this is a no-op: the native frame's own compiled epilogue (its own
`helper_unbind_n' call) already unwinds its specbinds before EITHER a
normal return or its own error path, entirely inside the single native
call this gateway wraps -- the fix only matters when control never
reaches that epilogue at all.  This mock has no such epilogue to run,
so it demonstrates the gateway's own force-close covers the normal
path too, harmlessly, rather than only the abnormal ones."
  (let ((nelisp-eln-specbind-integration-test--var 'original))
    (pcase-let ((`(,outcome ,delta)
                 (nelisp-eln-specbind-integration-test--run
                  (lambda (x)
                    (nelisp-eln-runtime-services-specbind
                     'nelisp-eln-specbind-integration-test--var 'transient)
                    x))))
      (should (equal outcome '(return 5)))
      (should (equal delta 0))
      (should (eq nelisp-eln-specbind-integration-test--var 'original)))))

(ert-deftest nelisp-eln-specbind-integration-negative-control-pre-fix-leaks ()
  "Verify-the-verifier: confirm this test suite would actually have
caught the bug, by re-running the exact unwind path the callback's
error takes but WITHOUT the fix (calling the real specbind, then
skipping straight to a bare `error' the way the pre-fix gateway body
would have, with no interposed unbind).  Demonstrates the leak the fix
above closes, rather than asserting a tautology."
  (let ((nelisp-eln-specbind-integration-test--var 'original)
        (before (nelisp-eln-runtime-services-specpdl-depth)))
    (condition-case nil
        (unwind-protect
            (progn
              (nelisp-eln-runtime-services-specbind
               'nelisp-eln-specbind-integration-test--var 'leaked)
              (error "boom-marker"))
          nil)
      (error nil))
    (should (eq nelisp-eln-specbind-integration-test--var 'leaked))
    (should (equal (- (nelisp-eln-runtime-services-specpdl-depth) before) 1))
    ;; Clean up the deliberately-leaked entry so later tests are unaffected.
    (nelisp-eln-runtime-services-helper-unbind-n 1)))

(provide 'nelisp-eln-specbind-integration-test)

;;; nelisp-eln-specbind-integration-test.el ends here
