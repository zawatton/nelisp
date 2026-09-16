;;; nl-ffi-dsl-standalone-smoke.el --- ffi:library / ffi:defun gate -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Gate for `make ffi-dsl'.  `nl-ffi-call' (which `ffi:library'/`ffi:defun'
;; sit on top of) exists only in the dynamic reader, so `make ffi-dsl'
;; rebuilds it unconditionally with `NELISP_READER_DYNAMIC=1', exactly as
;; `standalone-reader-ffi-smoke' does, before loading this file.  Run
;; directly with:
;;
;;   NELISP_READER_DYNAMIC=1 make standalone-reader
;;   ./target/nelisp --load packages/nl-ffi/test/nl-ffi-dsl-standalone-smoke.el
;;
;; Covers: an integer call (toupper), a double call (sqrt), a string-
;; returning call through `ffi:defun' + `nl-ffi-get-string' (SQLite's
;; version string), `nl-ffi-get-string' reconstructing a string spanning
;; more than one internal chunk plus an immediate-NUL "" (both built
;; directly with alloc-bytes/ptr-write-u8, no C call needed), the
;; `ffi-call' compatibility alias, `ffi:library''s SONAME validation, and
;; three of the four named error conditions `ffi:defun' itself contracts
;; for (the fourth, `nl-ffi-unavailable', needs a build WITHOUT a working
;; `nl-ffi-call' -- see `packages/nl-ffi/test/nl-ffi-test.el', which
;; proves it on host Emacs instead, where that is naturally the case).

;;; Code:

;; Minimal ert-deftest/should shim, following the same inline pattern as
;; packages/nl-num/test/nl-num-standalone-smoke.el -- target/nelisp has no
;; `ert', and this package has no other standalone package dependency to
;; borrow a shared shim from.
(defvar nl-ffi-smoke--tests nil
  "Alist of (NAME . BODY-FN) registered by `nl-ffi-smoke-deftest'.")

(defmacro nl-ffi-smoke-deftest (name &rest body)
  "Register BODY as test NAME in `nl-ffi-smoke--tests'."
  `(setq nl-ffi-smoke--tests
         (cons (cons ',name (lambda () ,@body)) nl-ffi-smoke--tests)))

(defmacro nl-ffi-smoke-should (form)
  `(let ((nl-ffi-smoke--value ,form))
     (unless nl-ffi-smoke--value
       (error "should failed: %S" ',form))
     nl-ffi-smoke--value))

(defmacro nl-ffi-smoke-should-error (form condition)
  "Evaluate FORM and require it to signal CONDITION (a quoted symbol)."
  `(let ((nl-ffi-smoke--result
          (condition-case nl-ffi-smoke--err
              (progn ,form 'nl-ffi-smoke--no-error)
            (error nl-ffi-smoke--err))))
     (cond
      ((eq nl-ffi-smoke--result 'nl-ffi-smoke--no-error)
       (error "should-error: no error signaled by %S" ',form))
      ((not (memq ,condition (get (car nl-ffi-smoke--result) 'error-conditions)))
       (error "should-error: expected %S, got %S" ,condition nl-ffi-smoke--result))
      (t nl-ffi-smoke--result))))

(load "packages/nl-ffi/src/nl-ffi.el")

;;;; --- toupper: integer argument and return -----------------------------

(nl-ffi-smoke-deftest ffi-dsl-toupper-integer
  (ffi:library "libc.so.6")
  (ffi:defun nl-ffi-smoke-toupper "toupper" [:sint32 :sint32]
    "toupper(3) -- uppercase an ASCII char code.")
  (nl-ffi-smoke-should (= (nl-ffi-smoke-toupper 97) 65))
  (nl-ffi-smoke-should (= (nl-ffi-smoke-toupper 65) 65)))

;;;; --- sqrt: double argument and return ----------------------------------

(nl-ffi-smoke-deftest ffi-dsl-sqrt-double
  (ffi:library "libm.so.6")
  (ffi:defun nl-ffi-smoke-sqrt "sqrt" [:double :double]
    "sqrt(3) -- double-precision square root.")
  (nl-ffi-smoke-should (= (nl-ffi-smoke-sqrt 4.0) 2.0))
  ;; An integer argument is promoted to a float, not rejected.
  (nl-ffi-smoke-should (= (nl-ffi-smoke-sqrt 9) 3.0)))

;;;; --- sqlite3_libversion: string-returning call + nl-ffi-get-string --------

(nl-ffi-smoke-deftest ffi-dsl-sqlite-string-return
  (ffi:library "libsqlite3.so.0")
  (ffi:defun nl-ffi-smoke-sqlite-version "sqlite3_libversion" [:pointer]
    "sqlite3_libversion(3) -- SQLite version string (const char*).")
  (let* ((ptr (nl-ffi-smoke-sqlite-version))
         (str (nl-ffi-get-string ptr)))
    (nl-ffi-smoke-should (integerp ptr))
    (nl-ffi-smoke-should (> ptr 0))
    (nl-ffi-smoke-should (stringp str))
    (nl-ffi-smoke-should (> (length str) 0))))

;;;; --- nl-ffi-get-string: chunked reconstruction + immediate-NUL ------------
;;
;; No C call needed for either: the buffer is built directly with
;; alloc-bytes/ptr-write-u8, exactly the primitives nl-ffi-get-string
;; itself reads with.

(nl-ffi-smoke-deftest ffi-dsl-get-string-longer-than-one-chunk
  ;; 600 > nl-ffi--string-chunk-size (256), so this must cross at least
  ;; one chunk boundary and still reconstruct the exact original bytes.
  (let* ((len 600)
         (buf (alloc-bytes (1+ len) 1))
         (i 0))
    (while (< i len)
      (ptr-write-u8 buf i (+ 97 (mod i 26)))
      (setq i (1+ i)))
    (ptr-write-u8 buf len 0)
    (let ((str (nl-ffi-get-string buf)))
      (nl-ffi-smoke-should (stringp str))
      (nl-ffi-smoke-should (= (length str) len))
      (nl-ffi-smoke-should (= (aref str 0) 97))
      (nl-ffi-smoke-should (= (aref str 255) (+ 97 (mod 255 26))))
      (nl-ffi-smoke-should (= (aref str 256) (+ 97 (mod 256 26))))
      (nl-ffi-smoke-should (= (aref str 599) (+ 97 (mod 599 26)))))))

(nl-ffi-smoke-deftest ffi-dsl-get-string-immediate-nul
  (let ((buf (alloc-bytes 1 1)))
    (ptr-write-u8 buf 0 0)
    (nl-ffi-smoke-should (equal (nl-ffi-get-string buf) ""))))

;;;; --- ffi-call compatibility alias --------------------------------------

(nl-ffi-smoke-deftest ffi-dsl-compat-alias
  (nl-ffi-smoke-should (= (ffi-call nil "toupper" [:sint32 :sint32] 97) 65)))

;;;; --- error contract: wrong arity ----------------------------------------

(nl-ffi-smoke-deftest ffi-dsl-wrong-arity
  (ffi:defun nl-ffi-smoke-wrong-arity-fn "toupper" [:sint32 :sint32])
  (nl-ffi-smoke-should-error (nl-ffi-smoke-wrong-arity-fn) 'nl-ffi-wrong-arity)
  (nl-ffi-smoke-should-error (nl-ffi-smoke-wrong-arity-fn 1 2) 'nl-ffi-wrong-arity))

;;;; --- error contract: unresolved symbol -----------------------------------

(nl-ffi-smoke-deftest ffi-dsl-unresolved-symbol
  (ffi:defun nl-ffi-smoke-unresolved-fn
    "nl_ffi_dsl_smoke_no_such_symbol_ever" [:sint32 :sint32 :sint32])
  (nl-ffi-smoke-should-error (nl-ffi-smoke-unresolved-fn 1 2)
                              'nl-ffi-unresolved-symbol))

;;;; --- error contract: unknown type keyword (expansion-time) ----------------

(nl-ffi-smoke-deftest ffi-dsl-unknown-type
  (nl-ffi-smoke-should-error
   (macroexpand-1 '(ffi:defun nl-ffi-smoke-bad-type-fn "toupper"
                              [:sint32 :not-a-real-type]))
   'nl-ffi-unknown-type))

;;;; --- error contract: unknown library soname --------------------------

(nl-ffi-smoke-deftest ffi-dsl-unknown-library
  (nl-ffi-smoke-should-error (ffi:library "sqlite3") 'nl-ffi-unknown-library)
  ;; No dlopen exists yet: a known, successfully declared library's
  ;; handle is still nil.
  (nl-ffi-smoke-should (null (nl-ffi-library-handle "libc.so.6"))))

;;;; --- run ------------------------------------------------------------------

(let ((tests (reverse nl-ffi-smoke--tests))
      (ran 0)
      (failures nil))
  (while tests
    (let ((test (car tests)))
      (condition-case err
          (progn
            (funcall (cdr test))
            (setq ran (1+ ran)))
        (error
         (setq failures
               (cons (format "%s: %S" (car test) err) failures)))))
    (setq tests (cdr tests)))
  ;; `tools/ai/nelisp-ai.sh gate NAME -- ...' requires this exact line to
  ;; report what the gate checked; its absence is itself a hard failure
  ;; there (see tools/ai/nelisp-ai.sh's `cmd_gate').
  (princ (format "GATE-COUNT checked=%d findings=%d\n" ran (length failures)))
  (when failures
    (let ((all failures))
      (while all
        (princ (format "FAIL %s\n" (car all)))
        (setq all (cdr all))))
    (error "nl-ffi-dsl-standalone-smoke: %d failure(s), %d passed"
           (length failures) ran))
  (when (< ran 10)
    (error "nl-ffi-dsl-standalone-smoke: only %d tests ran (expected >= 10)"
           ran))
  (princ (format "nl-ffi-dsl-standalone-smoke: PASS (%d tests)\n" ran)))

;;; nl-ffi-dsl-standalone-smoke.el ends here
