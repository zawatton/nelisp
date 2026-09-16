;;; nl-ffi-test.el --- Host ERT tests for nl-ffi -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `nl-ffi-call' is a NeLisp standalone reader builtin (the dynamic reader
;; only; see the package README) -- it does not exist under host Emacs at
;; all.  So this file cannot exercise a real FFI call; that is what
;; `packages/nl-ffi/test/nl-ffi-dsl-standalone-smoke.el' is for, run
;; against `target/nelisp' by `make ffi-dsl'.
;;
;; What host Emacs CAN exercise, and this file does:
;;
;;   - the pure conversion/validation logic in `nl-ffi--convert-arg' and
;;     `nl-ffi--signature-types', none of which touches `nl-ffi-call' or
;;     the raw-memory primitives;
;;   - `nl-ffi-wrong-arity', checked in Lisp before `nl-ffi-call' is ever
;;     reached, so it fires identically here and on the standalone reader;
;;   - `nl-ffi-unavailable', which host Emacs hits on EVERY correctly
;;     aritied call, since `nl-ffi-call' is genuinely absent here -- this
;;     is the one condition host Emacs is actually the natural place to
;;     prove, rather than a workaround for not having the real runtime.
;;
;; `nl-ffi-unresolved-symbol' needs a real, resolving-or-not `nl-ffi-call'
;; and so cannot be reached here either; see the standalone smoke.

;;; Code:

(require 'ert)
(require 'nl-ffi)

;;;; --- type vocabulary ------------------------------------------------------

(ert-deftest nl-ffi-test-type-p ()
  (should (nl-ffi-type-p :pointer))
  (should (nl-ffi-type-p :double))
  (should (nl-ffi-type-p :void))
  (should-not (nl-ffi-type-p :not-a-real-type))
  (should-not (nl-ffi-type-p 'pointer)))

;;;; --- nl-ffi--signature-types -----------------------------------------------

(ert-deftest nl-ffi-test-signature-types-shape ()
  (let ((types (nl-ffi--signature-types [:sint32 :sint32 :pointer])))
    (should (equal types '(:sint32 :sint32 :pointer)))
    (should (eq (car types) :sint32))
    (should (equal (cdr types) '(:sint32 :pointer)))))

(ert-deftest nl-ffi-test-signature-types-rejects-non-vector ()
  (should-error (nl-ffi--signature-types "not a vector")
                :type 'wrong-type-argument)
  (should-error (nl-ffi--signature-types '(:sint32))
                :type 'wrong-type-argument))

(ert-deftest nl-ffi-test-signature-types-rejects-empty ()
  (should-error (nl-ffi--signature-types [])
                :type 'wrong-type-argument))

;;;; --- nl-ffi--convert-arg (pure logic, no runtime primitives needed) --------

(ert-deftest nl-ffi-test-convert-integer-passthrough ()
  (should (= (nl-ffi--convert-arg 'test 1 :sint32 97) 97))
  (should (= (nl-ffi--convert-arg 'test 1 :uint64 0) 0)))

(ert-deftest nl-ffi-test-convert-integer-rejects-non-integer ()
  (should-error (nl-ffi--convert-arg 'test 1 :sint32 "x")
                :type 'wrong-type-argument)
  (should-error (nl-ffi--convert-arg 'test 1 :sint32 1.5)
                :type 'wrong-type-argument))

(ert-deftest nl-ffi-test-convert-float-passthrough-and-promotion ()
  (should (= (nl-ffi--convert-arg 'test 1 :double 4.0) 4.0))
  (should (floatp (nl-ffi--convert-arg 'test 1 :double 4.0)))
  ;; An integer argument for a float slot is promoted, not rejected: this
  ;; mirrors C's own implicit int -> double widening for a typed parameter.
  (should (= (nl-ffi--convert-arg 'test 1 :double 4) 4.0))
  (should (floatp (nl-ffi--convert-arg 'test 1 :float 2))))

(ert-deftest nl-ffi-test-convert-float-rejects-non-number ()
  (should-error (nl-ffi--convert-arg 'test 1 :double "4.0")
                :type 'wrong-type-argument))

(ert-deftest nl-ffi-test-convert-pointer-integer-nil ()
  (should (= (nl-ffi--convert-arg 'test 1 :pointer 42) 42))
  (should (= (nl-ffi--convert-arg 'test 1 :pointer nil) 0)))

(ert-deftest nl-ffi-test-convert-pointer-rejects-bad-type ()
  (should-error (nl-ffi--convert-arg 'test 1 :pointer [1 2 3])
                :type 'wrong-type-argument))

(ert-deftest nl-ffi-test-convert-void-is-unknown-as-argument ()
  (should-error (nl-ffi--convert-arg 'test 1 :void 0)
                :type 'nl-ffi-unknown-type))

(ert-deftest nl-ffi-test-convert-rejects-unknown-keyword ()
  (should-error (nl-ffi--convert-arg 'test 1 :not-a-real-type 0)
                :type 'nl-ffi-unknown-type))

;;;; --- nl-ffi-get-string: the NULL short-circuit needs no memory access ------

(ert-deftest nl-ffi-test-get-string-null ()
  (should (null (nl-ffi-get-string 0))))

(ert-deftest nl-ffi-test-get-string-rejects-non-integer ()
  (should-error (nl-ffi-get-string "not an address")
                :type 'wrong-type-argument))

;;;; --- ffi:library: soname validation + host Emacs lacks nl-ffi-call --------

(ert-deftest nl-ffi-test-library-known-soname-predicate ()
  (should (nl-ffi-known-soname-p "libsqlite3.so.0"))
  (should (nl-ffi-known-soname-p "libm.so.6"))
  (should-not (nl-ffi-known-soname-p "sqlite3"))
  (should-not (nl-ffi-known-soname-p "libtotally-not-real.so.1")))

(ert-deftest nl-ffi-test-library-unknown-soname-also-hits-unavailable-on-host ()
  "Since step 2, a SONAME outside `nl-ffi-known-sonames' is no longer
rejected outright: `ffi:library' now attempts a real `dlopen' for it
\(see `nl-ffi-library-open-failed', proven on the dynamic reader by
packages/nl-ffi/test/nl-ffi-dsl-standalone-smoke.el, since a real dlopen
attempt needs the real runtime\).  Host Emacs cannot get that far at
all -- `nl-ffi-call' is not `fboundp' here -- so a real SONAME and a
typo'd one now hit the exact same `nl-ffi-unavailable' check, before any
SONAME-specific work; see `nl-ffi-test-library-unavailable-on-host' for
the known-SONAME half of the same check."
  (skip-unless (not (fboundp 'nl-ffi-call)))
  (should-error (ffi:library "sqlite3") :type 'nl-ffi-unavailable)
  (should-error (ffi:library "libtotally-not-real.so.1")
                :type 'nl-ffi-unavailable))

(ert-deftest nl-ffi-test-library-unavailable-on-host ()
  (skip-unless (not (fboundp 'nl-ffi-call)))
  ;; A real, known SONAME, so this reaches the availability check -- and
  ;; host Emacs genuinely has no nl-ffi-call.
  (should-error (ffi:library "libsqlite3.so.0") :type 'nl-ffi-unavailable))

(ert-deftest nl-ffi-test-library-rejects-non-string ()
  ;; Type-checked before either the soname or the availability check, so
  ;; this signals wrong-type-argument unconditionally.
  (should-error (ffi:library 'not-a-string) :type 'wrong-type-argument))

(ert-deftest nl-ffi-test-library-handle-is-nil-today ()
  ;; Still nil on host Emacs -- not because `dlopen' does not exist any
  ;; more (it does, as of step 2; see `nl-ffi--dlopen'), but because
  ;; `ffi:library' never gets past its own `nl-ffi-unavailable' check
  ;; here (`nl-ffi-call' is not `fboundp' on host Emacs at all), so
  ;; nothing is ever registered for either SONAME below.  This proves the
  ;; accessor exists and its answer here, not a live handle -- the
  ;; standalone smoke proves a real, non-zero handle instead.
  (should (null (nl-ffi-library-handle "libm.so.6")))
  (should (null (nl-ffi-library-handle "libtotally-not-real.so.1"))))

;;;; --- ffi:defun: unknown type fires at expansion time -------------------

(ert-deftest nl-ffi-test-defun-unknown-return-type ()
  (should-error
   (macroexpand-1 '(ffi:defun nl-ffi-test--bad-ret "f" [:not-a-real-type]))
   :type 'nl-ffi-unknown-type))

(ert-deftest nl-ffi-test-defun-unknown-arg-type ()
  (should-error
   (macroexpand-1 '(ffi:defun nl-ffi-test--bad-arg "f" [:sint32 :not-a-real-type]))
   :type 'nl-ffi-unknown-type))

(ert-deftest nl-ffi-test-defun-void-arg-is-declaration-error ()
  (should-error
   (macroexpand-1 '(ffi:defun nl-ffi-test--void-arg "f" [:sint32 :void]))
   :type 'nl-ffi-unknown-type))

;;;; --- ffi:defun-generated function: arity, then availability ----------------

(ffi:defun nl-ffi-test--toupper "toupper" [:sint32 :sint32]
  "toupper(3), declared for this test file only.")

(ert-deftest nl-ffi-test-defun-wrong-arity ()
  ;; Checked in Lisp before nl-ffi-call is ever reached (see
  ;; `nl-ffi--invoke'), so this fires identically on host Emacs and the
  ;; standalone reader.
  (should-error (nl-ffi-test--toupper) :type 'nl-ffi-wrong-arity)
  (should-error (nl-ffi-test--toupper 97 98) :type 'nl-ffi-wrong-arity))

(ert-deftest nl-ffi-test-defun-unavailable-on-host ()
  (skip-unless (not (fboundp 'nl-ffi-call)))
  ;; Correct arity, so this reaches the availability check -- and host
  ;; Emacs genuinely has no `nl-ffi-call'.
  (should-error (nl-ffi-test--toupper 97) :type 'nl-ffi-unavailable))

;;;; --- step 2: nl-ffi--ptr-call-invoke's own checks need no runtime ---------
;;
;; `nl-ffi--invoke' only reaches `nl-ffi--ptr-call-invoke' once `nl-ffi-call'
;; has already answered "not in the table" (a raw nil) -- unreachable from
;; host Emacs via the public `ffi:defun'/`ffi-call' surface, since
;; `nl-ffi--invoke' signals `nl-ffi-unavailable' before ever calling
;; `nl-ffi-call' at all here (see `nl-ffi-test-defun-unavailable-on-host'
;; above).  `nl-ffi--ptr-call-invoke' itself, though, checks the
;; :float/:double refusal and the argument-count limit BEFORE touching
;; `dlsym'/`ptr-call' or `nl-ffi-call' in any way (see its own
;; Commentary) -- calling it directly with a bad signature exercises
;; exactly those two checks with no runtime involved at all. Only the
;; success path (a real, resolved call) needs the standalone reader; see
;; packages/nl-ffi/test/nl-ffi-dsl-standalone-smoke.el for that.

(ert-deftest nl-ffi-test-ptr-call-invoke-rejects-float-return ()
  (should-error
   (nl-ffi--ptr-call-invoke 'test "not_in_any_table" '(:sint32) :double '(1))
   :type 'nl-ffi-dlsym-float-unsupported))

(ert-deftest nl-ffi-test-ptr-call-invoke-rejects-float-argument ()
  (should-error
   (nl-ffi--ptr-call-invoke 'test "not_in_any_table" '(:sint32 :float) :sint32
                             '(1 2.0))
   :type 'nl-ffi-dlsym-float-unsupported))

(ert-deftest nl-ffi-test-ptr-call-invoke-allows-integer-and-pointer-types ()
  "The float refusal is specific to :float/:double -- every other
`nl-ffi-types' member (fixed-width integers and `:pointer') is fine on
this path, so a 6-argument all-integer/pointer signature must clear
both checks.  With no library ever `ffi:library'-declared in this test
process, `nl-ffi--resolve-via-dlsym' has nothing to search and returns 0
without needing `nl-ffi-call' at all (see its own Commentary), so this
needs no runtime either -- it signals the same `nl-ffi-unresolved-
symbol' a build-time-table miss with zero declared libraries would."
  (should-error
   (nl-ffi--ptr-call-invoke
    'test "not_in_any_table_at_all"
    '(:sint32 :uint64 :pointer :sint8 :uint16 :pointer) :pointer
    '(1 2 3 4 5 6))
   :type 'nl-ffi-unresolved-symbol))

(ert-deftest nl-ffi-test-ptr-call-invoke-rejects-too-many-arguments ()
  "`ptr-call' carries at most six real arguments (see
`nl-ffi--ptr-call-max-args') -- a 7-argument, all-integer signature must
be refused before ever attempting to resolve or call anything, so this
needs no runtime either."
  (should-error
   (nl-ffi--ptr-call-invoke
    'test "not_in_any_table"
    '(:sint32 :sint32 :sint32 :sint32 :sint32 :sint32 :sint32) :sint32
    '(1 2 3 4 5 6 7))
   :type 'nl-ffi-too-many-arguments))

;;;; --- nl-ffi-compat-call: same engine, elisp-ffi/nelisp-ffi call shape ------

(ert-deftest nl-ffi-test-compat-call-unknown-type ()
  (should-error (nl-ffi-compat-call nil "toupper" [:sint32 :not-a-real-type] 97)
                :type 'nl-ffi-unknown-type))

(ert-deftest nl-ffi-test-compat-call-wrong-arity ()
  (should-error (nl-ffi-compat-call nil "toupper" [:sint32 :sint32] 97 98)
                :type 'nl-ffi-wrong-arity))

(ert-deftest nl-ffi-test-compat-call-unavailable-on-host ()
  (skip-unless (not (fboundp 'nl-ffi-call)))
  (should-error (nl-ffi-compat-call nil "toupper" [:sint32 :sint32] 97)
                :type 'nl-ffi-unavailable))

;;;; --- elisp-ffi/nelisp-ffi compatibility aliases -----------------------------

(ert-deftest nl-ffi-test-compat-aliases-installed ()
  (should (fboundp 'ffi-call))
  (should (fboundp 'ffi-get-string))
  ;; `defalias' makes the alias's `symbol-function' the TARGET SYMBOL, not
  ;; its function object -- compare against the symbol, not via a second
  ;; `symbol-function' indirection.
  (should (eq (symbol-function 'ffi-call) 'nl-ffi-compat-call))
  (should (eq (symbol-function 'ffi-get-string) 'nl-ffi-get-string)))

;;;; --- nl-ffi-known-sonames vs. the build script's own table -----------------
;;
;; `nl-ffi-known-sonames' is a hand-maintained mirror of the SONAME set in
;; `nelisp-standalone--reader-extern-table' (scripts/nelisp-standalone-
;; build.el) -- its own docstring says so, and says it must be kept in
;; sync by hand.  A hand-maintained mirror with nothing checking it is a
;; promise, not a fact; this makes the drift detectable instead of silent.
;;
;; This reads scripts/nelisp-standalone-build.el AS DATA -- with `read',
;; never `load' or `eval' -- rather than calling into it.  That file is a
;; large host-only BUILD TOOL: loading it runs its own top-level
;; `require's and defines thousands of names purely to construct a
;; reader binary, all just to reach one already-literal constant. The
;; table itself is a plain quoted list, not something computed at load
;; time, so parsing its printed form with the Lisp reader is both
;; sufficient and far cheaper/safer than loading the tool that builds it.

(defun nl-ffi-test--repo-root ()
  "Locate the repository root (the directory holding the top-level Makefile)."
  (let ((here (or (and load-file-name (file-name-directory load-file-name))
                  default-directory)))
    (or (locate-dominating-file here "Makefile")
        (error "nl-ffi-test: could not locate the repository root from %s" here))))

(defun nl-ffi-test--build-table-sonames ()
  "Return the SONAME set from the build script's own extern table.
Reads scripts/nelisp-standalone-build.el as data (see this section's
Commentary above for why) and collects the second element of every row
in the literal `nelisp-standalone--reader-extern-table' list."
  (let ((path (expand-file-name "scripts/nelisp-standalone-build.el"
                                 (nl-ffi-test--repo-root)))
        (sonames nil))
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (unless (re-search-forward
               "(defconst nelisp-standalone--reader-extern-table" nil t)
        (error "nl-ffi-test: nelisp-standalone--reader-extern-table not found in %s"
               path))
      (goto-char (match-beginning 0))
      (let* ((form (read (current-buffer)))
             (value (nth 2 form)))
        (when (and (consp value) (eq (car value) 'quote))
          (setq value (cadr value)))
        (unless (consp value)
          (error "nl-ffi-test: unexpected nelisp-standalone--reader-extern-table shape: %S"
                 form))
        (dolist (row value)
          (push (nth 1 row) sonames))))
    (delete-dups sonames)))

(ert-deftest nl-ffi-test-known-sonames-match-build-table ()
  "`nl-ffi-known-sonames' must track the build script's real extern table.
Fails loudly (rather than the hand-maintained list quietly going stale)
when a row with a new SONAME is added to
`nelisp-standalone--reader-extern-table' without updating this package,
and equally when this package claims a SONAME the table has dropped."
  (let ((build-sonames (nl-ffi-test--build-table-sonames)))
    (should (null (seq-difference build-sonames nl-ffi-known-sonames)))
    (should (null (seq-difference nl-ffi-known-sonames build-sonames)))))

;;; nl-ffi-test.el ends here
