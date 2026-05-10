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

(provide 'nelisp-phase47-compiler-test)

;;; nelisp-phase47-compiler-test.el ends here
