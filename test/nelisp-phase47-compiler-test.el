;;; nelisp-phase47-compiler-test.el --- ert + e2e for Doc 97 compiler  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ert + end-to-end smoke for `nelisp-phase47-compile-sexp'.
;;
;; Three concerns covered:
;;
;;  1. Parser produces the expected IR shape for each v1 form.
;;  2. Emit phase produces the expected byte patterns (= invariant
;;     length per form; lengths summed across `seq` children).
;;  3. End-to-end smoke (= the Doc 97 production engagement gate):
;;     compile a Sexp program, exec the resulting binary on the host
;;     Linux kernel, observe stdout + exit code.
;;
;; E2E tests skip on non-Linux hosts via `ert-skip'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-phase47-compiler)

;; ---- §T.0 helpers ----

(defun nelisp-phase47-compiler-test--linux-p ()
  "Return non-nil when the host kernel can exec x86_64 ELF64 binaries."
  (and (eq system-type 'gnu/linux)
       (let ((arch (and (boundp 'system-configuration)
                        system-configuration)))
         (and (stringp arch)
              (string-match-p "x86_64\\|amd64" arch)))))

(defun nelisp-phase47-compiler-test--run-binary (path)
  "Exec PATH, return a plist `(:exit N :stdout S :stderr E)'."
  (let ((stdout-buf (generate-new-buffer " *nl97-stdout*"))
        (stderr-file (make-temp-file "nl97-stderr"))
        (exit-code nil))
    (unwind-protect
        (progn
          (setq exit-code
                (call-process path nil
                              (list stdout-buf stderr-file)
                              nil))
          (let ((stdout-text (with-current-buffer stdout-buf
                               (buffer-substring-no-properties
                                (point-min) (point-max))))
                (stderr-text (with-temp-buffer
                               (insert-file-contents stderr-file)
                               (buffer-substring-no-properties
                                (point-min) (point-max)))))
            (list :exit exit-code
                  :stdout stdout-text
                  :stderr stderr-text)))
      (when (buffer-live-p stdout-buf) (kill-buffer stdout-buf))
      (when (file-exists-p stderr-file) (delete-file stderr-file)))))

(defun nelisp-phase47-compiler-test--tmp-binary (suffix)
  "Return a fresh /tmp/nelisp-doc97-SUFFIX-NNNN path.
The file is not created — `nelisp-phase47-compile-sexp' creates it.
Caller is responsible for `delete-file' on cleanup."
  (make-temp-file (format "nelisp-doc97-%s-" suffix)))

;; ---- §T.1 parser unit tests ----

(ert-deftest nelisp-phase47-compiler/parse-exit-literal ()
  "Parse `(exit 0)' to an exit IR wrapping an imm value node."
  (let ((ir (nelisp-phase47-compiler--parse '(exit 0))))
    (should (eq (plist-get ir :kind) 'exit))
    (let ((v (plist-get ir :value)))
      (should (eq (plist-get v :kind) 'imm))
      (should (= (plist-get v :value) 0)))))

(ert-deftest nelisp-phase47-compiler/parse-write-literal ()
  "Parse `(write \"hi\")' to a write IR node."
  (let ((ir (nelisp-phase47-compiler--parse '(write "hi"))))
    (should (eq (plist-get ir :kind) 'write))
    (should (equal (plist-get ir :str) "hi"))))

(ert-deftest nelisp-phase47-compiler/parse-seq ()
  "Parse a `seq' of two children into nested IR."
  (let ((ir (nelisp-phase47-compiler--parse
             '(seq (write "x") (exit 0)))))
    (should (eq (plist-get ir :kind) 'seq))
    (should (= (length (plist-get ir :forms)) 2))
    (should (eq (plist-get (car (plist-get ir :forms)) :kind) 'write))
    (should (eq (plist-get (cadr (plist-get ir :forms)) :kind) 'exit))))

(ert-deftest nelisp-phase47-compiler/parse-let-arith-fold ()
  "Parse `(let ((x (+ 3 4))) (exit x))' folds arith + lookup."
  (let ((ir (nelisp-phase47-compiler--parse
             '(let ((x (+ 3 4))) (exit x)))))
    (should (eq (plist-get ir :kind) 'let))
    (should (eq (plist-get ir :var) 'x))
    (should (= (plist-get ir :value) 7))
    (let* ((body (plist-get ir :body))
           (vnode (plist-get body :value)))
      (should (eq (plist-get body :kind) 'exit))
      (should (eq (plist-get vnode :kind) 'imm))
      (should (= (plist-get vnode :value) 7)))))

(ert-deftest nelisp-phase47-compiler/parse-nested-let-arith ()
  "Nested let + chained arithmetic resolves to a single integer."
  (let ((ir (nelisp-phase47-compiler--parse
             '(let ((a 2)) (let ((b (* a 5))) (exit (+ b 1)))))))
    (should (eq (plist-get ir :kind) 'let))
    (let* ((body (plist-get ir :body))
           (inner (plist-get body :body))
           (vnode (plist-get inner :value)))
      (should (eq (plist-get vnode :kind) 'imm))
      (should (= (plist-get vnode :value) 11)))))

(ert-deftest nelisp-phase47-compiler/parse-free-symbol-errors ()
  "A free symbol reference signals `nelisp-phase47-compiler-error'."
  (should-error
   (nelisp-phase47-compiler--parse '(exit y))
   :type 'nelisp-phase47-compiler-error))

(ert-deftest nelisp-phase47-compiler/parse-unknown-form-errors ()
  "An unrecognised form head signals an error."
  (should-error
   (nelisp-phase47-compiler--parse '(if t 1 2))
   :type 'nelisp-phase47-compiler-error))

(ert-deftest nelisp-phase47-compiler/parse-status-out-of-range ()
  "An exit status outside 0..255 signals."
  (should-error
   (nelisp-phase47-compiler--parse '(exit 300))
   :type 'nelisp-phase47-compiler-error))

;; ---- §T.2 emit / byte-length tests ----

(ert-deftest nelisp-phase47-compiler/emit-exit-is-16-bytes ()
  "`(exit 0)' emits exactly 16 bytes of .text (= Doc 92 fixed length)."
  (let* ((ir (nelisp-phase47-compiler--parse '(exit 0)))
         (buf (nelisp-phase47-compiler--pass ir nil nil 0)))
    (should (= (nelisp-asm-x86_64-buffer-pos buf) 16))))

(ert-deftest nelisp-phase47-compiler/emit-write-is-33-bytes ()
  "`(write \"hi\")' emits exactly 33 bytes of .text."
  (let* ((ir (nelisp-phase47-compiler--parse '(write "hi")))
         (collected (nelisp-phase47-compiler--collect-strings ir))
         (offsets (car collected))
         (buf (nelisp-phase47-compiler--pass ir nil offsets #x401000)))
    (should (= (nelisp-asm-x86_64-buffer-pos buf) 33))))

(ert-deftest nelisp-phase47-compiler/strings-dedup ()
  "Two `(write \"x\")' forms share a single .rodata slot."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(seq (write "x") (write "x") (exit 0))))
         (collected (nelisp-phase47-compiler--collect-strings ir))
         (offsets (car collected))
         (rodata (cdr collected)))
    (should (= (length offsets) 1))
    (should (= (length rodata) 1))
    (should (equal (substring rodata 0 1) "x"))))

(ert-deftest nelisp-phase47-compiler/strings-distinct ()
  "Two distinct strings get distinct offsets summing to total length."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(seq (write "ab") (write "cd") (exit 0))))
         (collected (nelisp-phase47-compiler--collect-strings ir))
         (offsets (car collected))
         (rodata (cdr collected)))
    (should (= (length offsets) 2))
    (should (= (length rodata) 4))
    (should (equal rodata "abcd"))
    (should (= (plist-get (cdr (assoc "ab" offsets)) :offset) 0))
    (should (= (plist-get (cdr (assoc "cd" offsets)) :offset) 2))))

(ert-deftest nelisp-phase47-compiler/pass1-pass2-byte-length-parity ()
  "Pass-1 and pass-2 emit must agree on .text byte length."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(seq (write "hi") (exit 0))))
         (collected (nelisp-phase47-compiler--collect-strings ir))
         (offsets (car collected))
         (pass1 (nelisp-phase47-compiler--pass ir nil offsets 0))
         (size1 (nelisp-asm-x86_64-buffer-pos pass1))
         (pass2 (nelisp-phase47-compiler--pass ir nil offsets
                                               (+ #x400000 size1 #x78)))
         (size2 (nelisp-asm-x86_64-buffer-pos pass2)))
    (should (= size1 size2))))

;; ---- §T.3 e2e smoke (= the production engagement gate) ----

(ert-deftest nelisp-phase47-compiler/e2e-hello-world ()
  "Compile `(seq (write \"hello\\n\") (exit 0))', exec, observe."
  (unless (nelisp-phase47-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (let ((path (nelisp-phase47-compiler-test--tmp-binary "hello")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-sexp
           '(seq (write "hello\n") (exit 0)) path)
          (should (file-executable-p path))
          (let ((result (nelisp-phase47-compiler-test--run-binary path)))
            (should (equal (plist-get result :stdout) "hello\n"))
            (should (= (plist-get result :exit) 0))))
      (when (file-exists-p path) (delete-file path)))))

(ert-deftest nelisp-phase47-compiler/e2e-exit-42 ()
  "Compile `(exit 42)', exec, observe exit code 42."
  (unless (nelisp-phase47-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (let ((path (nelisp-phase47-compiler-test--tmp-binary "exit42")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-sexp '(exit 42) path)
          (let ((result (nelisp-phase47-compiler-test--run-binary path)))
            (should (= (plist-get result :exit) 42))
            (should (equal (plist-get result :stdout) ""))))
      (when (file-exists-p path) (delete-file path)))))

(ert-deftest nelisp-phase47-compiler/e2e-let-exit ()
  "Compile `(seq (let ((x 7)) (exit x)))', exec, observe exit code 7."
  (unless (nelisp-phase47-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (let ((path (nelisp-phase47-compiler-test--tmp-binary "let-exit")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-sexp
           '(seq (let ((x 7)) (exit x))) path)
          (let ((result (nelisp-phase47-compiler-test--run-binary path)))
            (should (= (plist-get result :exit) 7))))
      (when (file-exists-p path) (delete-file path)))))

(ert-deftest nelisp-phase47-compiler/e2e-arith-exit ()
  "Compile `(exit (+ 1 2))', exec, observe exit code 3."
  (unless (nelisp-phase47-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (let ((path (nelisp-phase47-compiler-test--tmp-binary "arith-exit")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-sexp '(exit (+ 1 2)) path)
          (let ((result (nelisp-phase47-compiler-test--run-binary path)))
            (should (= (plist-get result :exit) 3))))
      (when (file-exists-p path) (delete-file path)))))

(ert-deftest nelisp-phase47-compiler/e2e-multi-write ()
  "Compile two-write seq, observe concatenated stdout + exit 0."
  (unless (nelisp-phase47-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (let ((path (nelisp-phase47-compiler-test--tmp-binary "multi-write")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-sexp
           '(seq (write "hi") (write " there\n") (exit 0)) path)
          (let ((result (nelisp-phase47-compiler-test--run-binary path)))
            (should (equal (plist-get result :stdout) "hi there\n"))
            (should (= (plist-get result :exit) 0))))
      (when (file-exists-p path) (delete-file path)))))

(ert-deftest nelisp-phase47-compiler/e2e-string-dedup-execs ()
  "Duplicated string survives dedup + runs."
  (unless (nelisp-phase47-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (let ((path (nelisp-phase47-compiler-test--tmp-binary "dedup")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-sexp
           '(seq (write "ab") (write "ab") (exit 0)) path)
          (let ((result (nelisp-phase47-compiler-test--run-binary path)))
            (should (equal (plist-get result :stdout) "abab"))
            (should (= (plist-get result :exit) 0))))
      (when (file-exists-p path) (delete-file path)))))

(ert-deftest nelisp-phase47-compiler/e2e-arch-rejects-non-x86_64 ()
  "Asking for `:arch 'aarch64' signals (= v1 OOS)."
  (let ((path (nelisp-phase47-compiler-test--tmp-binary "arch-rej")))
    (unwind-protect
        (should-error
         (nelisp-phase47-compile-sexp '(exit 0) path :arch 'aarch64)
         :type 'nelisp-phase47-compiler-error)
      (when (file-exists-p path) (delete-file path)))))

;; ---- §T.4 §97.b defun + call parser tests ----

(ert-deftest nelisp-phase47-compiler/parse-defun-zero-arg ()
  "Parse `(defun seven () 7)' to a defun IR node with no params."
  (let ((ir (nelisp-phase47-compiler--parse '(defun seven () 7))))
    (should (eq (plist-get ir :kind) 'defun))
    (should (eq (plist-get ir :name) 'seven))
    (should (null (plist-get ir :params)))
    (should (null (plist-get ir :param-regs)))
    (let ((body (plist-get ir :body)))
      (should (eq (plist-get body :kind) 'imm))
      (should (= (plist-get body :value) 7)))))

(ert-deftest nelisp-phase47-compiler/parse-defun-param-ref ()
  "Parse `(defun id (x) x)' — body becomes a `:kind ref' node."
  (let ((ir (nelisp-phase47-compiler--parse '(defun id (x) x))))
    (should (eq (plist-get ir :kind) 'defun))
    (should (equal (plist-get ir :params) '(x)))
    (should (equal (plist-get ir :param-regs) '(rdi)))
    (let ((body (plist-get ir :body)))
      (should (eq (plist-get body :kind) 'ref))
      (should (eq (plist-get body :reg) 'rdi)))))

(ert-deftest nelisp-phase47-compiler/parse-defun-too-many-params ()
  "Defun with > 6 params signals (= 7+ args deferred to Doc 97.c)."
  (should-error
   (nelisp-phase47-compiler--parse
    '(defun seven-arg (a b c d e f g) a))
   :type 'nelisp-phase47-compiler-error))

(ert-deftest nelisp-phase47-compiler/parse-call-arity-mismatch ()
  "Calling a defun with wrong arg count signals."
  (should-error
   (nelisp-phase47-compiler--parse
    '(seq (defun id (x) x) (exit (id 1 2))))
   :type 'nelisp-phase47-compiler-error))

(ert-deftest nelisp-phase47-compiler/parse-runtime-arith ()
  "Parse `(+ a b)' inside a defun yields a `:kind arith' node."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun add (a b) (+ a b))))
         (body (plist-get ir :body)))
    (should (eq (plist-get body :kind) 'arith))
    (should (eq (plist-get body :op) '+))
    (should (eq (plist-get (plist-get body :a) :kind) 'ref))
    (should (eq (plist-get (plist-get body :b) :kind) 'ref))))

(ert-deftest nelisp-phase47-compiler/parse-call-in-exit ()
  "Parse `(exit (id 7))' yields an exit IR wrapping a call value."
  (let ((ir (nelisp-phase47-compiler--parse
             '(seq (defun id (x) x) (exit (id 7))))))
    (should (eq (plist-get ir :kind) 'seq))
    (let* ((forms (plist-get ir :forms))
           (exit-node (nth 1 forms))
           (vnode (plist-get exit-node :value)))
      (should (eq (plist-get exit-node :kind) 'exit))
      (should (eq (plist-get vnode :kind) 'call))
      (should (eq (plist-get vnode :name) 'id)))))

(ert-deftest nelisp-phase47-compiler/collect-defuns ()
  "`--collect-defuns' extracts every defun in encounter order."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(seq (defun a () 1) (defun b (x) x) (exit (a)))))
         (defuns (nelisp-phase47-compiler--collect-defuns ir)))
    (should (= (length defuns) 2))
    (should (eq (plist-get (nth 0 defuns) :name) 'a))
    (should (eq (plist-get (nth 1 defuns) :name) 'b))))

(ert-deftest nelisp-phase47-compiler/parse-duplicate-defun-errors ()
  "Two `(defun NAME ...)` with the same name signals."
  (should-error
   (nelisp-phase47-compiler--parse
    '(seq (defun f () 1) (defun f () 2) (exit (f))))
   :type 'nelisp-phase47-compiler-error))

;; ---- §T.5 §97.b e2e smoke ----

(defmacro nelisp-phase47-compiler-test--with-tmp-binary
    (var suffix &rest body)
  "Bind VAR to a fresh tmp binary path for SUFFIX, run BODY, clean up."
  (declare (indent 2))
  `(let ((,var (nelisp-phase47-compiler-test--tmp-binary ,suffix)))
     (unwind-protect
         (progn ,@body)
       (when (file-exists-p ,var) (delete-file ,var)))))

(ert-deftest nelisp-phase47-compiler/e2e-defun-zero-arg ()
  "`(seq (defun seven () 7) (exit (seven)))' exits 7."
  (unless (nelisp-phase47-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-phase47-compiler-test--with-tmp-binary path "seven"
    (nelisp-phase47-compile-sexp
     '(seq (defun seven () 7) (exit (seven))) path)
    (should (file-executable-p path))
    (let ((r (nelisp-phase47-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 7)))))

(ert-deftest nelisp-phase47-compiler/e2e-defun-one-arg ()
  "`(seq (defun id (x) x) (exit (id 42)))' exits 42."
  (unless (nelisp-phase47-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-phase47-compiler-test--with-tmp-binary path "id"
    (nelisp-phase47-compile-sexp
     '(seq (defun id (x) x) (exit (id 42))) path)
    (let ((r (nelisp-phase47-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 42)))))

(ert-deftest nelisp-phase47-compiler/e2e-defun-add-two ()
  "`(seq (defun add (a b) (+ a b)) (exit (add 3 4)))' exits 7."
  (unless (nelisp-phase47-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-phase47-compiler-test--with-tmp-binary path "add2"
    (nelisp-phase47-compile-sexp
     '(seq (defun add (a b) (+ a b)) (exit (add 3 4))) path)
    (let ((r (nelisp-phase47-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 7)))))

(ert-deftest nelisp-phase47-compiler/e2e-defun-sub-two ()
  "`(seq (defun sub (a b) (- a b)) (exit (sub 10 3)))' exits 7."
  (unless (nelisp-phase47-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-phase47-compiler-test--with-tmp-binary path "sub2"
    (nelisp-phase47-compile-sexp
     '(seq (defun sub (a b) (- a b)) (exit (sub 10 3))) path)
    (let ((r (nelisp-phase47-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 7)))))

(ert-deftest nelisp-phase47-compiler/e2e-defun-mul-two ()
  "`(seq (defun mul (a b) (* a b)) (exit (mul 6 7)))' exits 42."
  (unless (nelisp-phase47-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-phase47-compiler-test--with-tmp-binary path "mul2"
    (nelisp-phase47-compile-sexp
     '(seq (defun mul (a b) (* a b)) (exit (mul 6 7))) path)
    (let ((r (nelisp-phase47-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 42)))))

(ert-deftest nelisp-phase47-compiler/e2e-defun-six-arg ()
  "6-arg call exercises all SysV AMD64 arg registers — sum = 21."
  (unless (nelisp-phase47-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-phase47-compiler-test--with-tmp-binary path "sum6"
    (nelisp-phase47-compile-sexp
     '(seq (defun sum6 (a b c d e f)
             (+ (+ (+ a b) (+ c d)) (+ e f)))
           (exit (sum6 1 2 3 4 5 6)))
     path)
    (let ((r (nelisp-phase47-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 21)))))

(ert-deftest nelisp-phase47-compiler/e2e-defun-nested-call ()
  "`f` called twice via `g` returns 7 (= ((5+1)+1))."
  (unless (nelisp-phase47-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-phase47-compiler-test--with-tmp-binary path "nested"
    (nelisp-phase47-compile-sexp
     '(seq (defun f (x) (+ x 1))
           (defun g (x) (f (f x)))
           (exit (g 5)))
     path)
    (let ((r (nelisp-phase47-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 7)))))

(ert-deftest nelisp-phase47-compiler/e2e-defun-double ()
  "The Doc 97.b §6 smoke: `(double 21)' returns 42 via `(* x 2)'."
  (unless (nelisp-phase47-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-phase47-compiler-test--with-tmp-binary path "double"
    (nelisp-phase47-compile-sexp
     '(seq (defun double (x) (* x 2)) (exit (double 21))) path)
    (let ((r (nelisp-phase47-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 42)))))

(ert-deftest nelisp-phase47-compiler/e2e-defun-arith-runtime ()
  "Compose runtime arithmetic with calls: `(add (mul 3 4) 5)' = 17."
  (unless (nelisp-phase47-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-phase47-compiler-test--with-tmp-binary path "compose"
    (nelisp-phase47-compile-sexp
     '(seq (defun mul (a b) (* a b))
           (defun add (a b) (+ a b))
           (exit (add (mul 3 4) 5)))
     path)
    (let ((r (nelisp-phase47-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 17)))))

(ert-deftest nelisp-phase47-compiler/e2e-defun-call-chain ()
  "Three-deep call chain: a→b→c, each adds 1, returns 13 from 10."
  (unless (nelisp-phase47-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-phase47-compiler-test--with-tmp-binary path "chain"
    (nelisp-phase47-compile-sexp
     '(seq (defun c (x) (+ x 1))
           (defun b (x) (c (+ x 1)))
           (defun a (x) (b (+ x 1)))
           (exit (a 10)))
     path)
    (let ((r (nelisp-phase47-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 13)))))
;; ---- Doc 100 §100.D bitwise + shift grammar ----

(ert-deftest nelisp-phase47-compiler/parse-bitwise-ops ()
  "Each of `logior logand logxor' parses to a `:kind arith' node."
  (dolist (op '(logior logand logxor))
    (let* ((ir (nelisp-phase47-compiler--parse
                (list 'defun 'f '(a b) (list op 'a 'b))))
           (body (plist-get ir :body)))
      (should (eq (plist-get body :kind) 'arith))
      (should (eq (plist-get body :op) op)))))

(ert-deftest nelisp-phase47-compiler/parse-shift-ops ()
  "`(shl N C)' / `(sar N C)' parse to a `:kind shift' node."
  (dolist (op '(shl sar))
    (let* ((ir (nelisp-phase47-compiler--parse
                (list 'defun 'f '(n c) (list op 'n 'c))))
           (body (plist-get ir :body)))
      (should (eq (plist-get body :kind) 'shift))
      (should (eq (plist-get body :op) op)))))

(ert-deftest nelisp-phase47-compiler/parse-shift-arity ()
  "Shift with wrong arg count signals."
  (should-error
   (nelisp-phase47-compiler--parse '(exit (shl 1)))
   :type 'nelisp-phase47-compiler-error))

(ert-deftest nelisp-phase47-compiler/e2e-defun-logior ()
  "Doc 100 §100.D — `(logior 12 3)' = 15 via or-reg-reg inline emit."
  (unless (nelisp-phase47-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-phase47-compiler-test--with-tmp-binary path "ior"
    (nelisp-phase47-compile-sexp
     '(seq (defun ior (a b) (logior a b)) (exit (ior 12 3))) path)
    (let ((r (nelisp-phase47-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 15)))))

(ert-deftest nelisp-phase47-compiler/e2e-defun-logand ()
  "Doc 100 §100.D — `(logand 14 7)' = 6 via and-reg-reg inline emit."
  (unless (nelisp-phase47-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-phase47-compiler-test--with-tmp-binary path "iand"
    (nelisp-phase47-compile-sexp
     '(seq (defun iand (a b) (logand a b)) (exit (iand 14 7))) path)
    (let ((r (nelisp-phase47-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 6)))))

(ert-deftest nelisp-phase47-compiler/e2e-defun-logxor ()
  "Doc 100 §100.D — `(logxor 12 10)' = 6 via xor-reg-reg inline emit."
  (unless (nelisp-phase47-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-phase47-compiler-test--with-tmp-binary path "ixor"
    (nelisp-phase47-compile-sexp
     '(seq (defun ixor (a b) (logxor a b)) (exit (ixor 12 10))) path)
    (let ((r (nelisp-phase47-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 6)))))

(ert-deftest nelisp-phase47-compiler/e2e-defun-shl ()
  "Doc 100 §100.D — `(shl 1 3)' = 8 via shl rax, cl after mov rcx, r10."
  (unless (nelisp-phase47-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-phase47-compiler-test--with-tmp-binary path "shleft"
    (nelisp-phase47-compile-sexp
     '(seq (defun shleft (n c) (shl n c)) (exit (shleft 1 3))) path)
    (let ((r (nelisp-phase47-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 8)))))

(ert-deftest nelisp-phase47-compiler/e2e-defun-sar ()
  "Doc 100 §100.D — `(sar 256 4)' = 16 via sar rax, cl after mov rcx, r10."
  (unless (nelisp-phase47-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-phase47-compiler-test--with-tmp-binary path "shright"
    (nelisp-phase47-compile-sexp
     '(seq (defun shright (n c) (sar n c)) (exit (shright 256 4))) path)
    (let ((r (nelisp-phase47-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 16)))))


(ert-deftest nelisp-phase47-compiler/e2e-defun-write-and-exit ()
  "Function with side-effect `write' + computed exit code."
  (unless (nelisp-phase47-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-phase47-compiler-test--with-tmp-binary path "fn-write"
    (nelisp-phase47-compile-sexp
     '(seq (defun two () 2)
           (write "fn\n")
           (exit (two)))
     path)
    (let ((r (nelisp-phase47-compiler-test--run-binary path)))
      (should (equal (plist-get r :stdout) "fn\n"))
      (should (= (plist-get r :exit) 2)))))

;; ---- §T.6 §97.c parser tests — control flow + comparisons ----

(ert-deftest nelisp-phase47-compiler/parse-if-imm-branches ()
  "Parse `(if 1 7 9)' to an if IR node with imm THEN/ELSE."
  (let ((ir (nelisp-phase47-compiler--parse '(exit (if 1 7 9)))))
    (let* ((vnode (plist-get ir :value)))
      (should (eq (plist-get vnode :kind) 'if))
      (should (eq (plist-get (plist-get vnode :test) :kind) 'imm))
      (should (= (plist-get (plist-get vnode :then) :value) 7))
      (should (= (plist-get (plist-get vnode :else) :value) 9)))))

(ert-deftest nelisp-phase47-compiler/parse-cmp-ops ()
  "Each of `< > <= >= =' parses to a `:kind cmp' node with right op."
  (dolist (op '(< > <= >= =))
    (let* ((ir (nelisp-phase47-compiler--parse
                (list 'exit (list op 3 5))))
           (vnode (plist-get ir :value)))
      (should (eq (plist-get vnode :kind) 'cmp))
      (should (eq (plist-get vnode :op) op)))))

(ert-deftest nelisp-phase47-compiler/parse-cmp-arity ()
  "Comparison with wrong arg count signals."
  (should-error
   (nelisp-phase47-compiler--parse '(exit (< 1)))
   :type 'nelisp-phase47-compiler-error))

(ert-deftest nelisp-phase47-compiler/parse-while-body-list ()
  "`(while T B1 B2 B3)' captures all three body forms."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(seq (while 1 0 0 0) (exit 0))))
         (forms (plist-get ir :forms))
         (while-node (car forms)))
    (should (eq (plist-get while-node :kind) 'while))
    (should (= (length (plist-get while-node :body)) 3))))

(ert-deftest nelisp-phase47-compiler/parse-cond-clauses ()
  "`(cond (P1 B1) (t B2))' parses with `always' sentinel."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(exit (cond ((= 1 2) 9) (t 5)))))
         (vnode (plist-get ir :value))
         (clauses (plist-get vnode :clauses)))
    (should (eq (plist-get vnode :kind) 'cond))
    (should (= (length clauses) 2))
    (should (eq (plist-get (car (car clauses)) :kind) 'cmp))
    (should (eq (car (cadr clauses)) 'always))))

(ert-deftest nelisp-phase47-compiler/parse-cond-empty-errors ()
  "`(cond)' with no clauses signals."
  (should-error
   (nelisp-phase47-compiler--parse '(exit (cond)))
   :type 'nelisp-phase47-compiler-error))

(ert-deftest nelisp-phase47-compiler/parse-logic-and-or ()
  "`(and 1 2)' and `(or 0 5)' both parse to `:kind logic' nodes."
  (let* ((ir1 (nelisp-phase47-compiler--parse '(exit (and 1 2))))
         (ir2 (nelisp-phase47-compiler--parse '(exit (or 0 5)))))
    (should (eq (plist-get (plist-get ir1 :value) :kind) 'logic))
    (should (eq (plist-get (plist-get ir1 :value) :op) 'and))
    (should (eq (plist-get (plist-get ir2 :value) :op) 'or))))

(ert-deftest nelisp-phase47-compiler/parse-logic-empty-errors ()
  "`(and)' / `(or)' with no operands signal."
  (should-error
   (nelisp-phase47-compiler--parse '(exit (and)))
   :type 'nelisp-phase47-compiler-error)
  (should-error
   (nelisp-phase47-compiler--parse '(exit (or)))
   :type 'nelisp-phase47-compiler-error))

(ert-deftest nelisp-phase47-compiler/parse-control-flow-stable-ids ()
  "Two compiles in a row reset the label counter (= deterministic)."
  (let ((nelisp-phase47-compiler--label-counter 0))
    (let* ((ir1 (nelisp-phase47-compiler--parse '(exit (if 1 7 9)))))
      (should (eq (plist-get (plist-get ir1 :value) :id) 'if-1))))
  (let ((nelisp-phase47-compiler--label-counter 0))
    (let* ((ir2 (nelisp-phase47-compiler--parse '(exit (if 1 7 9)))))
      (should (eq (plist-get (plist-get ir2 :value) :id) 'if-1)))))

;; ---- §T.7 §97.c emit pass-1/pass-2 invariance ----

(ert-deftest nelisp-phase47-compiler/emit-if-pass-parity ()
  "if/else byte-length is identical across pass-1 and pass-2."
  (let* ((nelisp-phase47-compiler--label-counter 0)
         (ir (nelisp-phase47-compiler--parse '(exit (if 1 7 9))))
         (collected (nelisp-phase47-compiler--collect-strings ir))
         (pass1 (nelisp-phase47-compiler--pass ir nil (car collected) 0))
         (size1 (nelisp-asm-x86_64-buffer-pos pass1)))
    (setq nelisp-phase47-compiler--label-counter 0)
    (let* ((ir2 (nelisp-phase47-compiler--parse '(exit (if 1 7 9))))
           (pass2 (nelisp-phase47-compiler--pass
                   ir2 nil (car collected) #x401000))
           (size2 (nelisp-asm-x86_64-buffer-pos pass2)))
      (should (= size1 size2)))))

(ert-deftest nelisp-phase47-compiler/emit-while-pass-parity ()
  "while loop emits the same byte-count across both passes."
  (let* ((nelisp-phase47-compiler--label-counter 0)
         (ir (nelisp-phase47-compiler--parse
              '(seq (while 0 0) (exit 0))))
         (collected (nelisp-phase47-compiler--collect-strings ir))
         (pass1 (nelisp-phase47-compiler--pass ir nil (car collected) 0))
         (size1 (nelisp-asm-x86_64-buffer-pos pass1)))
    (setq nelisp-phase47-compiler--label-counter 0)
    (let* ((ir2 (nelisp-phase47-compiler--parse
                 '(seq (while 0 0) (exit 0))))
           (pass2 (nelisp-phase47-compiler--pass
                   ir2 nil (car collected) #x401000))
           (size2 (nelisp-asm-x86_64-buffer-pos pass2)))
      (should (= size1 size2)))))

;; ---- §T.8 §97.c e2e smoke (= the production gate) ----

(ert-deftest nelisp-phase47-compiler/e2e-if-then ()
  "`(exit (if 1 7 0))' exits 7."
  (unless (nelisp-phase47-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-phase47-compiler-test--with-tmp-binary path "if-then"
    (nelisp-phase47-compile-sexp '(seq (exit (if 1 7 0))) path)
    (let ((r (nelisp-phase47-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 7)))))

(ert-deftest nelisp-phase47-compiler/e2e-if-else ()
  "`(exit (if 0 7 99))' exits 99."
  (unless (nelisp-phase47-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-phase47-compiler-test--with-tmp-binary path "if-else"
    (nelisp-phase47-compile-sexp '(seq (exit (if 0 7 99))) path)
    (let ((r (nelisp-phase47-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 99)))))

(ert-deftest nelisp-phase47-compiler/e2e-cmp-lt ()
  "`(exit (if (< 3 5) 1 2))' exits 1."
  (unless (nelisp-phase47-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-phase47-compiler-test--with-tmp-binary path "cmp-lt"
    (nelisp-phase47-compile-sexp '(seq (exit (if (< 3 5) 1 2))) path)
    (let ((r (nelisp-phase47-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 1)))))

(ert-deftest nelisp-phase47-compiler/e2e-cmp-gt-false ()
  "`(exit (if (> 3 5) 1 2))' exits 2."
  (unless (nelisp-phase47-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-phase47-compiler-test--with-tmp-binary path "cmp-gt"
    (nelisp-phase47-compile-sexp '(seq (exit (if (> 3 5) 1 2))) path)
    (let ((r (nelisp-phase47-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 2)))))

(ert-deftest nelisp-phase47-compiler/e2e-cmp-eq ()
  "`(exit (if (= 4 4) 11 22))' exits 11."
  (unless (nelisp-phase47-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-phase47-compiler-test--with-tmp-binary path "cmp-eq"
    (nelisp-phase47-compile-sexp '(seq (exit (if (= 4 4) 11 22))) path)
    (let ((r (nelisp-phase47-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 11)))))

(ert-deftest nelisp-phase47-compiler/e2e-and-all-truthy ()
  "`(exit (and 1 2 3))' exits 3 (= last operand)."
  (unless (nelisp-phase47-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-phase47-compiler-test--with-tmp-binary path "and"
    (nelisp-phase47-compile-sexp '(seq (exit (and 1 2 3))) path)
    (let ((r (nelisp-phase47-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 3)))))

(ert-deftest nelisp-phase47-compiler/e2e-and-short-circuit ()
  "`(exit (and 1 0 3))' exits 0 (= short-circuit at 2nd term)."
  (unless (nelisp-phase47-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-phase47-compiler-test--with-tmp-binary path "and-sc"
    (nelisp-phase47-compile-sexp '(seq (exit (and 1 0 3))) path)
    (let ((r (nelisp-phase47-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 0)))))

(ert-deftest nelisp-phase47-compiler/e2e-or-first-truthy ()
  "`(exit (or 0 5 7))' exits 5 (= first non-zero short-circuits)."
  (unless (nelisp-phase47-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-phase47-compiler-test--with-tmp-binary path "or"
    (nelisp-phase47-compile-sexp '(seq (exit (or 0 5 7))) path)
    (let ((r (nelisp-phase47-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 5)))))

(ert-deftest nelisp-phase47-compiler/e2e-cond-second-match ()
  "`(cond ((= 1 2) 9) ((= 3 3) 7) (t 5))' exits 7."
  (unless (nelisp-phase47-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-phase47-compiler-test--with-tmp-binary path "cond"
    (nelisp-phase47-compile-sexp
     '(seq (exit (cond ((= 1 2) 9) ((= 3 3) 7) (t 5)))) path)
    (let ((r (nelisp-phase47-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 7)))))

(ert-deftest nelisp-phase47-compiler/e2e-cond-fallthrough ()
  "`(cond ((= 1 2) 9) (t 5))' takes the t clause and exits 5."
  (unless (nelisp-phase47-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-phase47-compiler-test--with-tmp-binary path "cond-t"
    (nelisp-phase47-compile-sexp
     '(seq (exit (cond ((= 1 2) 9) (t 5)))) path)
    (let ((r (nelisp-phase47-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 5)))))

(ert-deftest nelisp-phase47-compiler/e2e-factorial-recursive ()
  "Recursive factorial (= the Doc 97.c §6 production gate)."
  (unless (nelisp-phase47-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-phase47-compiler-test--with-tmp-binary path "fact"
    (nelisp-phase47-compile-sexp
     '(seq (defun fact (n)
             (if (= n 0) 1 (* n (fact (- n 1)))))
           (exit (fact 5)))
     path)
    (let ((r (nelisp-phase47-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 120)))))

(ert-deftest nelisp-phase47-compiler/e2e-nested-if ()
  "Nested ifs compute (if (< 2 5) (if (= 7 7) 42 1) 0) = 42."
  (unless (nelisp-phase47-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-phase47-compiler-test--with-tmp-binary path "nested-if"
    (nelisp-phase47-compile-sexp
     '(seq (exit (if (< 2 5) (if (= 7 7) 42 1) 0))) path)
    (let ((r (nelisp-phase47-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 42)))))

(ert-deftest nelisp-phase47-compiler/e2e-if-with-call ()
  "`if' inside a defun composes with call returns: max(3,7) = 7."
  (unless (nelisp-phase47-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-phase47-compiler-test--with-tmp-binary path "max"
    (nelisp-phase47-compile-sexp
     '(seq (defun maxf (a b) (if (> a b) a b))
           (exit (maxf 3 7)))
     path)
    (let ((r (nelisp-phase47-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 7)))))

(ert-deftest nelisp-phase47-compiler/e2e-cmp-le-ge ()
  "`<=' and `>=' boundaries: `(if (<= 5 5) 1 0)' = 1, etc."
  (unless (nelisp-phase47-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-phase47-compiler-test--with-tmp-binary path "le-eq"
    (nelisp-phase47-compile-sexp
     '(seq (exit (if (<= 5 5) (if (>= 5 5) 42 0) 0))) path)
    (let ((r (nelisp-phase47-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 42)))))

;; ====================================================================
;; Doc 99 §99.B — `nelisp-phase47-compile-to-object' (ET_REL emit)
;; ====================================================================

(defun nelisp-phase47-compiler-test--read-bytes (path)
  "Return raw unibyte bytes of PATH."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((coding-system-for-read 'no-conversion))
      (insert-file-contents-literally path))
    (buffer-substring-no-properties (point-min) (point-max))))

(ert-deftest nelisp-phase47-compiler/object-mode-smoke ()
  "Compile `(defun nelisp_spike_noop () 42)' to an ET_REL .o.
Verify ehdr: ET_REL, EM_X86_64, e_entry=0, e_phnum=0."
  (let ((path (make-temp-file "nelisp-doc99-obj-smoke-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun nelisp_spike_noop () 42)
           path)
          (let ((bytes (nelisp-phase47-compiler-test--read-bytes path)))
            ;; ELF magic + class + endian.
            (should (equal (substring bytes 0 4)
                           (unibyte-string #x7F #x45 #x4C #x46)))
            ;; e_type = ET_REL (1), e_machine = EM_X86_64 (62).
            (should (= (nelisp-elf--read-le16 bytes 16) 1))
            (should (= (nelisp-elf--read-le16 bytes 18) 62))
            ;; e_entry = 0 (= linker decides), e_phnum = 0.
            (should (zerop (nelisp-elf--read-le64 bytes 24)))
            (should (zerop (nelisp-elf--read-le16 bytes 56)))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-compiler/object-mode-readelf-s ()
  "`readelf -s' lists `nelisp_spike_noop' as GLOBAL FUNC in the .o."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc99-obj-syms-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun nelisp_spike_noop () 42)
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "-s" path)))))
            (should (string-match-p "nelisp_spike_noop" out))
            (should (string-match-p "FUNC" out))
            (should (string-match-p "GLOBAL" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-compiler/object-mode-multiple-defuns ()
  "Multiple defuns inside a `seq' each become a GLOBAL FUNC symbol."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc99-obj-multi-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(seq (defun answer () 42)
                 (defun negone () (- 0 1)))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "-s" path)))))
            (should (string-match-p "answer" out))
            (should (string-match-p "negone" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-compiler/object-mode-rejects-write ()
  "`(defun foo () (write \"hi\"))' rejected — spike disallows strings."
  (let ((path (make-temp-file "nelisp-doc99-obj-rej-w-" nil ".o")))
    (unwind-protect
        (should-error
         (nelisp-phase47-compile-to-object
          '(defun foo () (write "hi"))
          path)
         :type 'nelisp-phase47-compiler-error)
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-compiler/object-mode-rejects-non-defun ()
  "`(exit 0)' is rejected — top form must be defun or seq-of-defuns."
  (let ((path (make-temp-file "nelisp-doc99-obj-rej-nd-" nil ".o")))
    (unwind-protect
        (should-error
         (nelisp-phase47-compile-to-object '(exit 0) path)
         :type 'nelisp-phase47-compiler-error)
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-compiler/object-mode-rejects-mixed-seq ()
  "`(seq (defun foo () 42) (exit 0))' is rejected — seq must be all defuns."
  (let ((path (make-temp-file "nelisp-doc99-obj-rej-mix-" nil ".o")))
    (unwind-protect
        (should-error
         (nelisp-phase47-compile-to-object
          '(seq (defun foo () 42) (exit 0)) path)
         :type 'nelisp-phase47-compiler-error)
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-compiler/extern-call-rejects-missing-symbol ()
  "Doc 100 §100.A: `(extern-call)' without a symbol → parse error."
  (let ((path (make-temp-file "nelisp-doc100-extern-bad-" nil ".o")))
    (unwind-protect
        (should-error
         (nelisp-phase47-compile-to-object
          '(defun probe () (extern-call))
          path)
         :type 'nelisp-phase47-compiler-error)
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-compiler/extern-call-rejects-non-symbol ()
  "Doc 100 §100.A: `(extern-call \"name\")' → parse error.
The extern symbol literal must be a bare symbol so its
`symbol-name' can become the linker-visible name without quoting."
  (let ((path (make-temp-file "nelisp-doc100-extern-non-sym-" nil ".o")))
    (unwind-protect
        (should-error
         (nelisp-phase47-compile-to-object
          '(defun probe () (extern-call "ext_helper"))
          path)
         :type 'nelisp-phase47-compiler-error)
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-compiler/object-mode-extern-call-emits-plt32 ()
  "Doc 100 §100.A: compile-to-object emits SHN_UNDEF + PLT32 for extern-call.
End-to-end check that the compiler propagates an `(extern-call SYM)'
form through emit-extern-call → asm reloc → ELF writer so that
`readelf -r' shows R_X86_64_PLT32 against the symbol and
`readelf -s' shows the symbol as UND (= SHN_UNDEF)."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc100-extern-emit-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun probe () (extern-call ext_helper))
           path)
          (let ((rs-out (with-output-to-string
                          (with-current-buffer standard-output
                            (call-process "readelf" nil t nil "-r" path))))
                (ss-out (with-output-to-string
                          (with-current-buffer standard-output
                            (call-process "readelf" nil t nil "-s" path)))))
            (should (string-match-p "R_X86_64_PLT32" rs-out))
            (should (string-match-p "ext_helper" rs-out))
            (should (string-match-p "UND[ \t]+ext_helper" ss-out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-compiler/object-mode-extern-call-smoke ()
  "Doc 100 §100.A: (extern-call SYM) → .o + ld + exec end-to-end (= EXIT 99).
probe.o is generated by the Phase 47 chain and contains a PLT32
reloc against `ext_99'.  host.o is generated directly by
`nelisp-elf-write-binary' (= no C / clang dependency in the test)
and contains both `ext_99' (= returns 99) and `_start' (= calls
probe + exits with rax as status).  When linked together with
`ld' the resulting static executable exits with status 99 —
proving that PLT32 relocations emitted in BOTH directions resolve
correctly."
  (skip-unless (and (executable-find "ld")
                    (eq system-type 'gnu/linux)))
  ;; NOTE: do NOT name the output binary path `exec-path' — that
  ;; would shadow the Emacs built-in `exec-path' variable for the
  ;; duration of this `let*' and break `executable-find' /
  ;; `call-process' (= "No such file or directory" for `ld' /
  ;; `readelf' / any external program inside the body).
  (let* ((probe-path (make-temp-file "nelisp-doc100-probe-" nil ".o"))
         (host-path (make-temp-file "nelisp-doc100-host-" nil ".o"))
         (bin-path (make-temp-file "nelisp-doc100-bin-" nil ""))
         (host-text
          (concat
           ;; ext_99: mov eax, 99 ; ret      (6 bytes, offset 0..5)
           (unibyte-string #xB8 99 0 0 0)
           (unibyte-string #xC3)
           ;; _start: at offset 6 (15 bytes total)
           ;;   E8 ?? ?? ?? ??       call probe      (5 bytes; reloc at offset 7)
           ;;   48 89 C7             mov rdi, rax    (3 bytes)
           ;;   B8 3C 00 00 00       mov eax, 60     (5 bytes; SYS_exit)
           ;;   0F 05                syscall         (2 bytes)
           (unibyte-string #xE8 0 0 0 0)
           (unibyte-string #x48 #x89 #xC7)
           (unibyte-string #xB8 #x3C 0 0 0)
           (unibyte-string #x0F #x05))))
    (unwind-protect
        (progn
          ;; Generate probe.o via Phase 47 chain.
          (nelisp-phase47-compile-to-object
           '(defun probe () (extern-call ext_99))
           probe-path)
          ;; Generate host.o by hand.
          (nelisp-elf-write-binary
           host-path
           (list :e-type 'rel
                 :text host-text
                 :symbols (list
                           (list :name "ext_99" :value 0 :size 6
                                 :section 'text :bind 'global :type 'func)
                           (list :name "_start" :value 6 :size 15
                                 :section 'text :bind 'global :type 'func)
                           (list :name "probe" :section 'undef
                                 :bind 'global :type 'notype))
                 :relocs (list
                          (list :section 'text :offset 7
                                :symbol "probe" :type 'plt32 :addend -4))))
          ;; Link.
          (let ((ld-status
                 (call-process "ld" nil nil nil
                               "-o" bin-path probe-path host-path)))
            (should (zerop ld-status)))
          (set-file-modes bin-path #o755)
          ;; Exec.
          (let ((exit-status
                 (call-process bin-path nil nil nil)))
            (should (= exit-status 99))))
      (ignore-errors (delete-file probe-path))
      (ignore-errors (delete-file host-path))
      (ignore-errors (delete-file bin-path)))))

;; ---- Doc 100 v2 §100.B Sexp ABI direct-access ops ----

(ert-deftest nelisp-phase47-compiler/sexp-tag-parse-shape ()
  "Doc 100 §100.B: (sexp-tag PTR) parses to (:kind sexp-tag :ptr REF)."
  (let* ((sexp '(defun probe (p) (sexp-tag p)))
         (program (nelisp-phase47-compiler--parse (list 'seq sexp))))
    ;; Top-level program is a seq containing one defun.
    (should (eq (plist-get program :kind) 'seq))
    (let* ((forms (plist-get program :forms))
           (defun-node (car forms))
           (body (plist-get defun-node :body)))
      (should (eq (plist-get body :kind) 'sexp-tag))
      (should (eq (plist-get (plist-get body :ptr) :kind) 'ref)))))

(ert-deftest nelisp-phase47-compiler/sexp-int-unwrap-parse-shape ()
  "Doc 100 §100.B: (sexp-int-unwrap PTR) parses to (:kind sexp-int-unwrap :ptr REF)."
  (let* ((sexp '(defun probe (p) (sexp-int-unwrap p)))
         (program (nelisp-phase47-compiler--parse (list 'seq sexp))))
    (let* ((defun-node (car (plist-get program :forms)))
           (body (plist-get defun-node :body)))
      (should (eq (plist-get body :kind) 'sexp-int-unwrap))
      (should (eq (plist-get (plist-get body :ptr) :kind) 'ref)))))

(ert-deftest nelisp-phase47-compiler/sexp-int-make-parse-shape ()
  "Doc 100 §100.B: (sexp-int-make SLOT N) parses to (:kind sexp-int-make :slot :val)."
  (let* ((sexp '(defun probe (slot n) (sexp-int-make slot n)))
         (program (nelisp-phase47-compiler--parse (list 'seq sexp))))
    (let* ((defun-node (car (plist-get program :forms)))
           (body (plist-get defun-node :body)))
      (should (eq (plist-get body :kind) 'sexp-int-make))
      (should (eq (plist-get (plist-get body :slot) :kind) 'ref))
      (should (eq (plist-get (plist-get body :val) :kind) 'ref)))))

(ert-deftest nelisp-phase47-compiler/sexp-abi-ops-reject-bad-arity ()
  "Doc 100 §100.B: each Sexp ABI op rejects wrong-arity invocations."
  (dolist (case '(((defun p () (sexp-tag))           :sexp-tag-arity)
                  ((defun p (a b) (sexp-tag a b))    :sexp-tag-arity)
                  ((defun p () (sexp-int-unwrap))    :sexp-int-unwrap-arity)
                  ((defun p () (sexp-int-make))      :sexp-int-make-arity)
                  ((defun p (a) (sexp-int-make a))   :sexp-int-make-arity)))
    (let ((bad (car case))
          (expected-tag (cadr case)))
      (condition-case err
          (progn
            (nelisp-phase47-compiler--parse (list 'seq bad))
            (ert-fail (list :expected-error case)))
        (nelisp-phase47-compiler-error
         (should (eq (car (cdr err)) expected-tag)))))))

(ert-deftest nelisp-phase47-compiler/sexp-tag-emit-bytes ()
  "Doc 100 §100.B: (sexp-tag PTR) emits the documented byte sequence.
The expected tail of the .text body is `48 89 C7 48 0F B6 07' (=
mov rdi, rax + movzx rax, byte ptr [rdi]) — what `--emit-sexp-tag'
appends after the `:ptr' sub-expression has computed into rax."
  (let* ((sexp '(seq (defun probe (p) (sexp-tag p))))
         (path (make-temp-file "nelisp-doc100-sexp-tag-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object sexp path)
          (let* ((bytes (with-temp-buffer
                          (set-buffer-multibyte nil)
                          (insert-file-contents-literally path)
                          (buffer-string)))
                 ;; The 7-byte tail before the `ret' (= 0xC3) and any
                 ;; ELF trailer.  Match the substring anywhere in .text.
                 (needle (unibyte-string #x48 #x89 #xC7
                                         #x48 #x0F #xB6 #x07)))
            (should (string-search needle bytes))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-compiler/sexp-int-unwrap-emit-bytes ()
  "Doc 100 §100.B: (sexp-int-unwrap PTR) emits `mov rdi,rax + mov rax,[rdi+8]'.
Expected tail: `48 89 C7 48 8B 47 08'."
  (let* ((sexp '(seq (defun probe (p) (sexp-int-unwrap p))))
         (path (make-temp-file "nelisp-doc100-sexp-unwrap-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object sexp path)
          (let* ((bytes (with-temp-buffer
                          (set-buffer-multibyte nil)
                          (insert-file-contents-literally path)
                          (buffer-string)))
                 (needle (unibyte-string #x48 #x89 #xC7
                                         #x48 #x8B #x47 #x08)))
            (should (string-search needle bytes))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-compiler/sexp-int-make-emit-bytes ()
  "Doc 100 §100.B: (sexp-int-make SLOT N) emits the 3-instr writer sequence.
Expected substring (= the tail before `ret'):
  C6 07 02         mov byte [rdi], 2     (SEXP_TAG_INT)
  48 89 77 08      mov [rdi+8], rsi
  48 89 F8         mov rax, rdi"
  (let* ((sexp '(seq (defun probe (slot n) (sexp-int-make slot n))))
         (path (make-temp-file "nelisp-doc100-sexp-make-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object sexp path)
          (let* ((bytes (with-temp-buffer
                          (set-buffer-multibyte nil)
                          (insert-file-contents-literally path)
                          (buffer-string)))
                 (needle (unibyte-string #xC6 #x07 #x02
                                         #x48 #x89 #x77 #x08
                                         #x48 #x89 #xF8)))
            (should (string-search needle bytes))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-compiler/sexp-int-unwrap-e2e-exec ()
  "Doc 100 §100.B: probe.o reads a host-provided Sexp::Int(42) → EXIT=42.

host.o lays out at `int_value' a 16-byte block:
  off 0:  02                tag byte = SEXP_TAG_INT
  off 1-7: 00 ...            padding
  off 8:  2A 00 00 00 ...    i64 payload = 42 little-endian
The `_start' entry calls `probe' with rdi=&int_value, then exits
with rax (= the unwrapped payload) as the status code."
  (skip-unless (and (executable-find "ld")
                    (eq system-type 'gnu/linux)))
  (let* ((probe-path (make-temp-file "nelisp-doc100-unwrap-probe-" nil ".o"))
         (host-path (make-temp-file "nelisp-doc100-unwrap-host-" nil ".o"))
         (bin-path (make-temp-file "nelisp-doc100-unwrap-bin-" nil ""))
         ;; .data: 16-byte Sexp::Int(42).
         (data-bytes
          (concat
           (unibyte-string 2 0 0 0 0 0 0 0)
           (unibyte-string 42 0 0 0 0 0 0 0)))
         ;; .text: _start sets up rdi = address of `int_value', calls
         ;; probe, then `mov rdi, rax; mov eax, 60; syscall'.
         (host-text
          (concat
           ;; 48 8D 3D 00 00 00 00   lea rdi, [rip + int_value]
           ;;   reloc PC32 @ offset 3 against `int_value' addend -4
           (unibyte-string #x48 #x8D #x3D 0 0 0 0)
           ;; E8 00 00 00 00         call probe (reloc PC32 @ offset 8 (= +1 from cur))
           (unibyte-string #xE8 0 0 0 0)
           ;; 48 89 C7               mov rdi, rax
           (unibyte-string #x48 #x89 #xC7)
           ;; B8 3C 00 00 00         mov eax, 60
           (unibyte-string #xB8 #x3C 0 0 0)
           ;; 0F 05                  syscall
           (unibyte-string #x0F #x05))))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun probe (p) (sexp-int-unwrap p))
           probe-path)
          (nelisp-elf-write-binary
           host-path
           (list :e-type 'rel
                 :text host-text
                 :data data-bytes
                 :symbols (list
                           (list :name "int_value" :value 0
                                 :size 16 :section 'data
                                 :bind 'global :type 'object)
                           (list :name "_start" :value 0
                                 :size (length host-text)
                                 :section 'text :bind 'global :type 'func)
                           (list :name "probe" :section 'undef
                                 :bind 'global :type 'notype))
                 :relocs (list
                          (list :section 'text :offset 3
                                :symbol "int_value" :type 'pc32 :addend -4)
                          (list :section 'text :offset 8
                                :symbol "probe" :type 'plt32 :addend -4))))
          (let ((ld-status
                 (call-process "ld" nil nil nil
                               "-o" bin-path probe-path host-path)))
            (should (zerop ld-status)))
          (set-file-modes bin-path #o755)
          (let ((exit-status
                 (call-process bin-path nil nil nil)))
            (should (= exit-status 42))))
      (ignore-errors (delete-file probe-path))
      (ignore-errors (delete-file host-path))
      (ignore-errors (delete-file bin-path)))))

(ert-deftest nelisp-phase47-compiler/sexp-int-make-unwrap-e2e-exec ()
  "Doc 100 §100.B: probe writes Sexp::Int(77) into a host slot, reads back → EXIT=77.

probe.o's body is `(sexp-int-unwrap (sexp-int-make slot 77))' (=
write the literal 77 into the caller-provided 32-byte slot at rdi,
then read it back via the same slot pointer).  host.o reserves a
32-byte aligned `.bss' slot, passes its address to probe, and exits
with rax (= the round-tripped 77) as status."
  (skip-unless (and (executable-find "ld")
                    (eq system-type 'gnu/linux)))
  (let* ((probe-path (make-temp-file "nelisp-doc100-make-probe-" nil ".o"))
         (host-path (make-temp-file "nelisp-doc100-make-host-" nil ".o"))
         (bin-path (make-temp-file "nelisp-doc100-make-bin-" nil ""))
         ;; Initialize the slot with non-zero bytes so we know the
         ;; sexp-int-make actually wrote.  16 bytes = tag + payload
         ;; portion (the unused tail [16,32) is not read by unwrap).
         (data-bytes
          (concat (make-string 16 #xFF)
                  (make-string 16 #xFF)))
         (host-text
          (concat
           ;; 48 8D 3D 00 00 00 00   lea rdi, [rip + slot]
           (unibyte-string #x48 #x8D #x3D 0 0 0 0)
           ;; E8 00 00 00 00         call probe
           (unibyte-string #xE8 0 0 0 0)
           ;; 48 89 C7               mov rdi, rax
           (unibyte-string #x48 #x89 #xC7)
           ;; B8 3C 00 00 00         mov eax, 60
           (unibyte-string #xB8 #x3C 0 0 0)
           ;; 0F 05                  syscall
           (unibyte-string #x0F #x05))))
    (unwind-protect
        (progn
          ;; The probe body is a single value form: (sexp-int-unwrap
          ;; (sexp-int-make p 77)) — the slot pointer flows into make,
          ;; make returns slot, unwrap reads back the payload.
          (nelisp-phase47-compile-to-object
           '(defun probe (p) (sexp-int-unwrap (sexp-int-make p 77)))
           probe-path)
          (nelisp-elf-write-binary
           host-path
           (list :e-type 'rel
                 :text host-text
                 :data data-bytes
                 :symbols (list
                           (list :name "slot" :value 0 :size 32
                                 :section 'data :bind 'global :type 'object)
                           (list :name "_start" :value 0
                                 :size (length host-text)
                                 :section 'text :bind 'global :type 'func)
                           (list :name "probe" :section 'undef
                                 :bind 'global :type 'notype))
                 :relocs (list
                          (list :section 'text :offset 3
                                :symbol "slot" :type 'pc32 :addend -4)
                          (list :section 'text :offset 8
                                :symbol "probe" :type 'plt32 :addend -4))))
          (let ((ld-status
                 (call-process "ld" nil nil nil
                               "-o" bin-path probe-path host-path)))
            (should (zerop ld-status)))
          (set-file-modes bin-path #o755)
          (let ((exit-status
                 (call-process bin-path nil nil nil)))
            (should (= exit-status 77))))
      (ignore-errors (delete-file probe-path))
      (ignore-errors (delete-file host-path))
      (ignore-errors (delete-file bin-path)))))

(provide 'nelisp-phase47-compiler-test)

;;; nelisp-phase47-compiler-test.el ends here
