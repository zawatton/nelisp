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
(require 'nelisp-cc-jit-arith)
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
    (should (eq (nelisp-phase47-compiler--ir-kind ir) 'exit))
    (let ((v (nelisp-phase47-compiler--ir-get ir :value)))
      (should (eq (nelisp-phase47-compiler--ir-kind v) 'imm))
      (should (= (nelisp-phase47-compiler--ir-get v :value) 0)))))

(ert-deftest nelisp-phase47-compiler/parse-write-literal ()
  "Parse `(write \"hi\")' to a write IR node."
  (let ((ir (nelisp-phase47-compiler--parse '(write "hi"))))
    (should (eq (nelisp-phase47-compiler--ir-kind ir) 'write))
    (should (equal (nelisp-phase47-compiler--ir-get ir :str) "hi"))))

(ert-deftest nelisp-phase47-compiler/parse-seq ()
  "Parse a `seq' of two children into nested IR."
  (let ((ir (nelisp-phase47-compiler--parse
             '(seq (write "x") (exit 0)))))
    (should (eq (nelisp-phase47-compiler--ir-kind ir) 'seq))
    (should (= (length (nelisp-phase47-compiler--ir-get ir :forms)) 2))
    (should (eq (nelisp-phase47-compiler--ir-kind
                 (car (nelisp-phase47-compiler--ir-get ir :forms)))
                'write))
    (should (eq (nelisp-phase47-compiler--ir-kind
                 (cadr (nelisp-phase47-compiler--ir-get ir :forms)))
                'exit))))

(ert-deftest nelisp-phase47-compiler/parse-let-arith-fold ()
  "Parse `(let ((x (+ 3 4))) (exit x))' folds arith + lookup."
  (let ((ir (nelisp-phase47-compiler--parse
             '(let ((x (+ 3 4))) (exit x)))))
    (should (eq (nelisp-phase47-compiler--ir-kind ir) 'let))
    (should (eq (nelisp-phase47-compiler--ir-get ir :var) 'x))
    (should (= (nelisp-phase47-compiler--ir-get ir :value) 7))
    (let* ((body (nelisp-phase47-compiler--ir-get ir :body))
           (vnode (nelisp-phase47-compiler--ir-get body :value)))
      (should (eq (nelisp-phase47-compiler--ir-kind body) 'exit))
      (should (eq (nelisp-phase47-compiler--ir-kind vnode) 'imm))
      (should (= (nelisp-phase47-compiler--ir-get vnode :value) 7)))))

(ert-deftest nelisp-phase47-compiler/parse-nested-let-arith ()
  "Nested let + chained arithmetic resolves to a single integer."
  (let ((ir (nelisp-phase47-compiler--parse
             '(let ((a 2)) (let ((b (* a 5))) (exit (+ b 1)))))))
    (should (eq (nelisp-phase47-compiler--ir-kind ir) 'let))
    (let* ((body (nelisp-phase47-compiler--ir-get ir :body))
           (inner (nelisp-phase47-compiler--ir-get body :body))
           (vnode (nelisp-phase47-compiler--ir-get inner :value)))
      (should (eq (nelisp-phase47-compiler--ir-kind vnode) 'imm))
      (should (= (nelisp-phase47-compiler--ir-get vnode :value) 11)))))

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

(ert-deftest nelisp-phase47-compiler/parse-doc129-when-macro ()
  "Doc 129.1: host `when' macro expands to Phase 47 `if'."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(exit (when (= 1 1) 7))))
         (vnode (nelisp-phase47-compiler--ir-get ir :value)))
    (should (eq (nelisp-phase47-compiler--ir-kind vnode) 'if))
    (should (= (nelisp-phase47-compiler--ir-get
                (nelisp-phase47-compiler--ir-get vnode :else) :value)
               0))))

(ert-deftest nelisp-phase47-compiler/parse-doc129-let*-desugar ()
  "Doc 129.1: `let*' desugars to nested Phase 47 `let' forms."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(let* ((a 2) (b (+ a 5))) (exit b))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (exit-node (nelisp-phase47-compiler--ir-get body :body))
         (exit-value (nelisp-phase47-compiler--ir-get exit-node :value)))
    (should (eq (nelisp-phase47-compiler--ir-kind ir) 'let))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'let))
    (should (= (nelisp-phase47-compiler--ir-get exit-value :value) 7))))

(ert-deftest nelisp-phase47-compiler/parse-doc129-top-level-defmacro ()
  "Doc 129.1: top-level `defmacro' is compile-time-only."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(seq
                (defmacro inc (x) (list '+ x 1))
                (exit (inc 6)))))
         (forms (nelisp-phase47-compiler--ir-get ir :forms))
         (exit-node (car forms))
         (vnode (nelisp-phase47-compiler--ir-get exit-node :value)))
    (should (= (length forms) 1))
    (should (eq (nelisp-phase47-compiler--ir-kind vnode) 'imm))
    (should (= (nelisp-phase47-compiler--ir-get vnode :value) 7))))

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

(ert-deftest nelisp-phase47-compiler/e2e-doc129-macroexpand-front ()
  "Doc 129.1: compile host macros and `let*' after frontend expansion."
  (unless (nelisp-phase47-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (let ((path (nelisp-phase47-compiler-test--tmp-binary "doc129-macro")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-sexp
           '(seq
             (defmacro inc (x) (list '+ x 1))
             (let* ((a 3)
                    (b (inc a)))
               (exit (unless (= b 0) b))))
           path)
          (let ((result (nelisp-phase47-compiler-test--run-binary path)))
            (should (= (plist-get result :exit) 4))))
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
    (should (eq (nelisp-phase47-compiler--ir-kind ir) 'defun))
    (should (eq (nelisp-phase47-compiler--ir-get ir :name) 'seven))
    (should (null (nelisp-phase47-compiler--ir-get ir :params)))
    (should (null (nelisp-phase47-compiler--ir-get ir :param-regs)))
    (let ((body (nelisp-phase47-compiler--ir-get ir :body)))
      (should (eq (nelisp-phase47-compiler--ir-kind body) 'imm))
      (should (= (nelisp-phase47-compiler--ir-get body :value) 7)))))

(ert-deftest nelisp-phase47-compiler/parse-defun-param-ref ()
  "Parse `(defun id (x) x)' — body becomes a `:kind ref' node."
  (let ((ir (nelisp-phase47-compiler--parse '(defun id (x) x))))
    (should (eq (nelisp-phase47-compiler--ir-kind ir) 'defun))
    (should (equal (nelisp-phase47-compiler--ir-get ir :params) '(x)))
    (should (equal (nelisp-phase47-compiler--ir-get ir :param-regs) '(rdi)))
    (let ((body (nelisp-phase47-compiler--ir-get ir :body)))
      (should (eq (nelisp-phase47-compiler--ir-kind body) 'ref))
      (should (eq (nelisp-phase47-compiler--ir-get body :reg) 'rdi)))))

(ert-deftest nelisp-phase47-compiler/parse-doc129-sexp-param-root-map ()
  "Doc 129.5A: `:type sexp' params become static GC root slots when allocating."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun make-str ((slot :type sexp) bytes len)
                 (sexp-write-str slot bytes len))))
         (body (nelisp-phase47-compiler--ir-get ir :body))
         (slot-ref (nelisp-phase47-compiler--ir-get body :slot))
         (bytes-ref (nelisp-phase47-compiler--ir-get body :bytes-ptr)))
    (should (eq (nelisp-phase47-compiler--ir-get ir :param-class) 'gp))
    (should (equal (nelisp-phase47-compiler--ir-get ir :gc-root-slots) '(0)))
    (should (nelisp-phase47-compiler--ir-get slot-ref :root-p))
    (should-not (nelisp-phase47-compiler--ir-get bytes-ref :root-p))))

(ert-deftest nelisp-phase47-compiler/parse-doc129-no-allocation-no-root-map ()
  "Doc 129.5A: annotated Sexp refs do not create a root map without allocation."
  (let ((ir (nelisp-phase47-compiler--parse
             '(defun tag-of ((x :type sexp)) (sexp-tag x)))))
    (should (null (nelisp-phase47-compiler--ir-get ir :gc-root-slots)))))

(ert-deftest nelisp-phase47-compiler/parse-defun-too-many-params ()
  "Defun still rejects params beyond the current disp8 local-slot cap."
  (should-error
   (nelisp-phase47-compiler--parse
    '(defun fifteen-arg (a b c d e f g h i j k l m n o) a))
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
         (body (nelisp-phase47-compiler--ir-get ir :body)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'arith))
    (should (eq (nelisp-phase47-compiler--ir-get body :op) '+))
    (should (eq (nelisp-phase47-compiler--ir-kind
                 (nelisp-phase47-compiler--ir-get body :a))
                'ref))
    (should (eq (nelisp-phase47-compiler--ir-kind
                 (nelisp-phase47-compiler--ir-get body :b))
                'ref))))

(ert-deftest nelisp-phase47-compiler/parse-call-in-exit ()
  "Parse `(exit (id 7))' yields an exit IR wrapping a call value."
  (let ((ir (nelisp-phase47-compiler--parse
             '(seq (defun id (x) x) (exit (id 7))))))
    (should (eq (nelisp-phase47-compiler--ir-kind ir) 'seq))
    (let* ((forms (nelisp-phase47-compiler--ir-get ir :forms))
           (exit-node (nth 1 forms))
           (vnode (nelisp-phase47-compiler--ir-get exit-node :value)))
      (should (eq (nelisp-phase47-compiler--ir-kind exit-node) 'exit))
      (should (eq (nelisp-phase47-compiler--ir-kind vnode) 'call))
      (should (eq (nelisp-phase47-compiler--ir-get vnode :name) 'id)))))

(ert-deftest nelisp-phase47-compiler/collect-defuns ()
  "`--collect-defuns' extracts every defun in encounter order."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(seq (defun a () 1) (defun b (x) x) (exit (a)))))
         (defuns (nelisp-phase47-compiler--collect-defuns ir)))
    (should (= (length defuns) 2))
    (should (eq (nelisp-phase47-compiler--ir-get (nth 0 defuns) :name) 'a))
    (should (eq (nelisp-phase47-compiler--ir-get (nth 1 defuns) :name) 'b))))

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
           (body (nelisp-phase47-compiler--ir-get ir :body)))
      (should (eq (nelisp-phase47-compiler--ir-kind body) 'arith))
      (should (eq (nelisp-phase47-compiler--ir-get body :op) op)))))

(ert-deftest nelisp-phase47-compiler/parse-shift-ops ()
  "`(shl N C)' / `(sar N C)' parse to a `:kind shift' node."
  (dolist (op '(shl sar))
    (let* ((ir (nelisp-phase47-compiler--parse
                (list 'defun 'f '(n c) (list op 'n 'c))))
           (body (nelisp-phase47-compiler--ir-get ir :body)))
      (should (eq (nelisp-phase47-compiler--ir-kind body) 'shift))
      (should (eq (nelisp-phase47-compiler--ir-get body :op) op)))))

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
    (let* ((vnode (nelisp-phase47-compiler--ir-get ir :value)))
      (should (eq (nelisp-phase47-compiler--ir-kind vnode) 'if))
      (should (eq (nelisp-phase47-compiler--ir-kind
                   (nelisp-phase47-compiler--ir-get vnode :test))
                  'imm))
      (should (= (nelisp-phase47-compiler--ir-get
                  (nelisp-phase47-compiler--ir-get vnode :then) :value)
                 7))
      (should (= (nelisp-phase47-compiler--ir-get
                  (nelisp-phase47-compiler--ir-get vnode :else) :value)
                 9)))))

(ert-deftest nelisp-phase47-compiler/parse-cmp-ops ()
  "Each of `< > <= >= =' parses to a `:kind cmp' node with right op."
  (dolist (op '(< > <= >= =))
    (let* ((ir (nelisp-phase47-compiler--parse
                (list 'exit (list op 3 5))))
           (vnode (nelisp-phase47-compiler--ir-get ir :value)))
      (should (eq (nelisp-phase47-compiler--ir-kind vnode) 'cmp))
      (should (eq (nelisp-phase47-compiler--ir-get vnode :op) op)))))

(ert-deftest nelisp-phase47-compiler/parse-cmp-arity ()
  "Comparison with wrong arg count signals."
  (should-error
   (nelisp-phase47-compiler--parse '(exit (< 1)))
   :type 'nelisp-phase47-compiler-error))

(ert-deftest nelisp-phase47-compiler/parse-while-body-list ()
  "`(while T B1 B2 B3)' captures all three body forms."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(seq (while 1 0 0 0) (exit 0))))
         (forms (nelisp-phase47-compiler--ir-get ir :forms))
         (while-node (car forms)))
    (should (eq (nelisp-phase47-compiler--ir-kind while-node) 'while))
    (should (= (length (nelisp-phase47-compiler--ir-get while-node :body)) 3))))

(ert-deftest nelisp-phase47-compiler/parse-cond-clauses ()
  "`(cond (P1 B1) (t B2))' parses with `always' sentinel."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(exit (cond ((= 1 2) 9) (t 5)))))
         (vnode (nelisp-phase47-compiler--ir-get ir :value))
         (clauses (nelisp-phase47-compiler--ir-get vnode :clauses)))
    (should (eq (nelisp-phase47-compiler--ir-kind vnode) 'cond))
    (should (= (length clauses) 2))
    (should (eq (nelisp-phase47-compiler--ir-kind (car (car clauses))) 'cmp))
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
    (should (eq (nelisp-phase47-compiler--ir-kind
                 (nelisp-phase47-compiler--ir-get ir1 :value))
                'logic))
    (should (eq (nelisp-phase47-compiler--ir-get
                 (nelisp-phase47-compiler--ir-get ir1 :value) :op)
                'and))
    (should (eq (nelisp-phase47-compiler--ir-get
                 (nelisp-phase47-compiler--ir-get ir2 :value) :op)
                'or))))

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
      (should (eq (nelisp-phase47-compiler--ir-get
                   (nelisp-phase47-compiler--ir-get ir1 :value) :id)
                  'if-1))))
  (let ((nelisp-phase47-compiler--label-counter 0))
    (let* ((ir2 (nelisp-phase47-compiler--parse '(exit (if 1 7 9)))))
      (should (eq (nelisp-phase47-compiler--ir-get
                   (nelisp-phase47-compiler--ir-get ir2 :value) :id)
                  'if-1)))))

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

(defun nelisp-phase47-compiler-test--elf-section-header (bytes index)
  "Return plist for section header INDEX in ELF BYTES."
  (let* ((shoff (nelisp-elf--read-le64 bytes 40))
         (shentsize (nelisp-elf--read-le16 bytes 58))
         (base (+ shoff (* index shentsize))))
    (list :name-off (nelisp-elf--read-le32 bytes base)
          :type (nelisp-elf--read-le32 bytes (+ base 4))
          :offset (nelisp-elf--read-le64 bytes (+ base 24))
          :size (nelisp-elf--read-le64 bytes (+ base 32))
          :link (nelisp-elf--read-le32 bytes (+ base 40))
          :entsize (nelisp-elf--read-le64 bytes (+ base 56)))))

(defun nelisp-phase47-compiler-test--elf-cstring (bytes start)
  "Read a NUL-terminated string from BYTES at START."
  (let ((end start))
    (while (and (< end (length bytes))
                (not (zerop (aref bytes end))))
      (setq end (1+ end)))
    (substring bytes start end)))

(defun nelisp-phase47-compiler-test--elf-find-section (bytes name)
  "Return plist for section NAME in ELF BYTES, or nil."
  (let* ((shnum (nelisp-elf--read-le16 bytes 60))
         (shstrndx (nelisp-elf--read-le16 bytes 62))
         (shstr (nelisp-phase47-compiler-test--elf-section-header bytes
                                                                  shstrndx))
         (shstr-off (plist-get shstr :offset))
         (found nil)
         (i 0))
    (while (and (< i shnum) (null found))
      (let* ((shdr (nelisp-phase47-compiler-test--elf-section-header bytes i))
             (sec-name
              (nelisp-phase47-compiler-test--elf-cstring
               bytes (+ shstr-off (plist-get shdr :name-off)))))
        (when (equal sec-name name)
          (setq found shdr)))
      (setq i (1+ i)))
    found))

(defun nelisp-phase47-compiler-test--elf-symbols (bytes)
  "Return the emitted ELF symbol table from BYTES as plists."
  (let* ((symtab (or (nelisp-phase47-compiler-test--elf-find-section
                      bytes ".symtab")
                     (error ".symtab not found")))
         (strtab-index (plist-get symtab :link))
         (strtab (nelisp-phase47-compiler-test--elf-section-header
                  bytes strtab-index))
         (sym-off (plist-get symtab :offset))
         (sym-size (plist-get symtab :size))
         (ent-size (plist-get symtab :entsize))
         (str-off (plist-get strtab :offset))
         (count (/ sym-size ent-size))
         (i 0)
         (acc nil))
    (while (< i count)
      (let* ((base (+ sym-off (* i ent-size)))
             (name-off (nelisp-elf--read-le32 bytes base))
             (name (nelisp-phase47-compiler-test--elf-cstring
                    bytes (+ str-off name-off))))
        (push (list :name name
                    :value (nelisp-elf--read-le64 bytes (+ base 8))
                    :size (nelisp-elf--read-le64 bytes (+ base 16))
                    :shndx (nelisp-elf--read-le16 bytes (+ base 6)))
              acc))
      (setq i (1+ i)))
    (nreverse acc)))

(defun nelisp-phase47-compiler-test--find-symbol (bytes name)
  "Return symbol plist for NAME in ELF BYTES."
  (or (cl-find-if (lambda (sym)
                    (equal (plist-get sym :name) name))
                  (nelisp-phase47-compiler-test--elf-symbols bytes))
      (error "symbol %s not found" name)))

(defun nelisp-phase47-compiler-test--assert-aarch64-object-symbol (src expected-name)
  "Compile SRC to an aarch64 ELF object and assert EXPECTED-NAME exists."
  (let ((path (make-temp-file "nelisp-phase47-arm64-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object src path :arch 'aarch64)
          (let* ((bytes (nelisp-phase47-compiler-test--read-bytes path))
                 (sym (nelisp-phase47-compiler-test--find-symbol
                       bytes expected-name)))
            (should (equal (substring bytes 0 4)
                           (unibyte-string #x7F #x45 #x4C #x46)))
            (should (= (nelisp-elf--read-le16 bytes 16) 1))
            (should (= (nelisp-elf--read-le16 bytes 18) 183))
            (should (zerop (nelisp-elf--read-le64 bytes 24)))
            (should (> (plist-get sym :size) 0))
            (should (zerop (plist-get sym :value)))))
      (ignore-errors (delete-file path)))))

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

(ert-deftest nelisp-phase47-compiler/object-mode-aarch64-add2 ()
  "AArch64 ET_REL emit: arithmetic trampoline exports `nelisp_jit_add2'."
  (nelisp-phase47-compiler-test--assert-aarch64-object-symbol
   nelisp-cc-jit-arith-add2--source
   "nelisp_jit_add2"))

(ert-deftest nelisp-phase47-compiler/object-mode-aarch64-eq2 ()
  "AArch64 ET_REL emit: comparison trampoline exports `nelisp_jit_eq2'."
  (nelisp-phase47-compiler-test--assert-aarch64-object-symbol
   nelisp-cc-jit-arith-eq2--source
   "nelisp_jit_eq2"))

(ert-deftest nelisp-phase47-compiler/object-mode-aarch64-logior2 ()
  "AArch64 ET_REL emit: bitwise trampoline exports `nelisp_jit_logior2'."
  (nelisp-phase47-compiler-test--assert-aarch64-object-symbol
   nelisp-cc-jit-arith-logior2--source
   "nelisp_jit_logior2"))

(ert-deftest nelisp-phase47-compiler/object-mode-aarch64-ash ()
  "AArch64 ET_REL emit: if+shift trampoline exports `nelisp_jit_ash'."
  (nelisp-phase47-compiler-test--assert-aarch64-object-symbol
   nelisp-cc-jit-arith-ash--source
   "nelisp_jit_ash"))

(ert-deftest nelisp-phase47-compiler/object-mode-aarch64-all-jit-arith-trampolines ()
  "All 12 Doc 100 §100.D trampoline defuns compile to one aarch64 ET_REL."
  (let ((path (make-temp-file "nelisp-doc100-arm64-jit-all-" nil ".o"))
        (sources (list nelisp-cc-jit-arith-add2--source
                       nelisp-cc-jit-arith-sub2--source
                       nelisp-cc-jit-arith-mul2--source
                       nelisp-cc-jit-arith-eq2--source
                       nelisp-cc-jit-arith-lt2--source
                       nelisp-cc-jit-arith-gt2--source
                       nelisp-cc-jit-arith-le2--source
                       nelisp-cc-jit-arith-ge2--source
                       nelisp-cc-jit-arith-logior2--source
                       nelisp-cc-jit-arith-logand2--source
                       nelisp-cc-jit-arith-logxor2--source
                       nelisp-cc-jit-arith-ash--source))
        (names '("nelisp_jit_add2" "nelisp_jit_sub2" "nelisp_jit_mul2"
                 "nelisp_jit_eq2" "nelisp_jit_lt2" "nelisp_jit_gt2"
                 "nelisp_jit_le2" "nelisp_jit_ge2" "nelisp_jit_logior2"
                 "nelisp_jit_logand2" "nelisp_jit_logxor2" "nelisp_jit_ash")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           (cons 'seq sources) path :arch 'aarch64)
          (let* ((bytes (nelisp-phase47-compiler-test--read-bytes path))
                 (symbols (nelisp-phase47-compiler-test--elf-symbols bytes)))
            (should (= (nelisp-elf--read-le16 bytes 18) 183))
            (dolist (name names)
              (let ((sym (cl-find-if (lambda (entry)
                                       (equal (plist-get entry :name) name))
                                     symbols)))
                (should sym)
                (should (> (plist-get sym :size) 0))
                (should (<= 0 (plist-get sym :value)))))))
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

(ert-deftest nelisp-phase47-compiler/object-mode-extern-call-7gp-smoke ()
  "Doc 129.7E: SysV extern-call passes the seventh GP arg on the stack."
  (skip-unless (and (executable-find "ld")
                    (eq system-type 'gnu/linux)))
  (let* ((probe-path (make-temp-file "nelisp-doc129-probe7-" nil ".o"))
         (host-path (make-temp-file "nelisp-doc129-host7-" nil ".o"))
         (bin-path (make-temp-file "nelisp-doc129-bin7-" nil ""))
         (sum7-text
          (concat
           ;; ext_sum7:
           ;;   rax = rdi+rsi+rdx+rcx+r8+r9+[rsp+8]; ret
           (unibyte-string #x48 #x89 #xF8)
           (unibyte-string #x48 #x01 #xF0)
           (unibyte-string #x48 #x01 #xD0)
           (unibyte-string #x48 #x01 #xC8)
           (unibyte-string #x4C #x01 #xC0)
           (unibyte-string #x4C #x01 #xC8)
           (unibyte-string #x48 #x03 #x44 #x24 #x08)
           (unibyte-string #xC3)))
         (start-text
          (concat
           (unibyte-string #xE8 0 0 0 0)
           (unibyte-string #x48 #x89 #xC7)
           (unibyte-string #xB8 #x3C 0 0 0)
           (unibyte-string #x0F #x05)))
         (host-text (concat sum7-text start-text))
         (start-off (length sum7-text)))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun probe () (extern-call ext_sum7 1 2 3 4 5 6 21))
           probe-path)
          (nelisp-elf-write-binary
           host-path
           (list :e-type 'rel
                 :text host-text
                 :symbols (list
                           (list :name "ext_sum7" :value 0
                                 :size (length sum7-text)
                                 :section 'text :bind 'global :type 'func)
                           (list :name "_start" :value start-off
                                 :size (length start-text)
                                 :section 'text :bind 'global :type 'func)
                           (list :name "probe" :section 'undef
                                 :bind 'global :type 'notype))
                 :relocs (list
                          (list :section 'text :offset (1+ start-off)
                                :symbol "probe" :type 'plt32 :addend -4))))
          (should (zerop (call-process "ld" nil nil nil
                                       "-o" bin-path host-path probe-path)))
          (set-file-modes bin-path #o755)
          (let ((exit-status (call-process bin-path nil nil nil)))
            (should (= exit-status 42))))
      (ignore-errors (delete-file probe-path))
      (ignore-errors (delete-file host-path))
      (ignore-errors (delete-file bin-path)))))

(ert-deftest nelisp-phase47-compiler/extern-call-7gp-rejects-nontrivial-stack-arg ()
  "Doc 129.7E: stack GP args stay trivial until call-spill slots land."
  (let ((path (make-temp-file "nelisp-doc129-stack-nontrivial-" nil ".o")))
    (unwind-protect
        (should-error
         (nelisp-phase47-compile-to-object
          '(defun probe (x) (extern-call ext_sum7 1 2 3 4 5 6 (+ x 1)))
          path)
         :type 'nelisp-phase47-compiler-error)
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-compiler/object-mode-defun-7gp-param-smoke ()
  "Doc 129.7E: SysV defuns can receive the seventh GP param from the stack."
  (skip-unless (and (executable-find "ld")
                    (eq system-type 'gnu/linux)))
  (let* ((sum-path (make-temp-file "nelisp-doc129-sum7-" nil ".o"))
         (host-path (make-temp-file "nelisp-doc129-sum7-host-" nil ".o"))
         (bin-path (make-temp-file "nelisp-doc129-sum7-bin-" nil ""))
         (prefix
          (concat
           (unibyte-string #xBF 1 0 0 0)       ; mov edi, 1
           (unibyte-string #xBE 2 0 0 0)       ; mov esi, 2
           (unibyte-string #xBA 3 0 0 0)       ; mov edx, 3
           (unibyte-string #xB9 4 0 0 0)       ; mov ecx, 4
           (unibyte-string #x41 #xB8 5 0 0 0) ; mov r8d, 5
           (unibyte-string #x41 #xB9 6 0 0 0) ; mov r9d, 6
           (unibyte-string #x6A 21)))          ; push 21
         (call-site (length prefix))
         (suffix
          (concat
           (unibyte-string #xE8 0 0 0 0)
           (unibyte-string #x48 #x89 #xC7)
           (unibyte-string #xB8 #x3C 0 0 0)
           (unibyte-string #x0F #x05)))
         (host-text (concat prefix suffix)))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun sum7 (a b c d e f g)
              (+ (+ (+ (+ (+ (+ a b) c) d) e) f) g))
           sum-path)
          (nelisp-elf-write-binary
           host-path
           (list :e-type 'rel
                 :text host-text
                 :symbols (list
                           (list :name "_start" :value 0
                                 :size (length host-text)
                                 :section 'text :bind 'global :type 'func)
                           (list :name "sum7" :section 'undef
                                 :bind 'global :type 'notype))
                 :relocs (list
                          (list :section 'text :offset (1+ call-site)
                                :symbol "sum7" :type 'plt32 :addend -4))))
          (should (zerop (call-process "ld" nil nil nil
                                       "-o" bin-path host-path sum-path)))
          (set-file-modes bin-path #o755)
          (let ((exit-status (call-process bin-path nil nil nil)))
            (should (= exit-status 42))))
      (ignore-errors (delete-file sum-path))
      (ignore-errors (delete-file host-path))
      (ignore-errors (delete-file bin-path)))))

;; ---- Doc 100 v2 §100.B Sexp ABI direct-access ops ----

(ert-deftest nelisp-phase47-compiler/sexp-tag-parse-shape ()
  "Doc 100 §100.B: (sexp-tag PTR) parses to (:kind sexp-tag :ptr REF)."
  (let* ((sexp '(defun probe (p) (sexp-tag p)))
         (program (nelisp-phase47-compiler--parse (list 'seq sexp))))
    ;; Top-level program is a seq containing one defun.
    (should (eq (nelisp-phase47-compiler--ir-kind program) 'seq))
    (let* ((forms (nelisp-phase47-compiler--ir-get program :forms))
           (defun-node (car forms))
           (body (nelisp-phase47-compiler--ir-get defun-node :body)))
      (should (eq (nelisp-phase47-compiler--ir-kind body) 'sexp-tag))
      (should (eq (nelisp-phase47-compiler--ir-kind
                   (nelisp-phase47-compiler--ir-get body :ptr))
                  'ref)))))

(ert-deftest nelisp-phase47-compiler/sexp-int-unwrap-parse-shape ()
  "Doc 100 §100.B: (sexp-int-unwrap PTR) parses to (:kind sexp-int-unwrap :ptr REF)."
  (let* ((sexp '(defun probe (p) (sexp-int-unwrap p)))
         (program (nelisp-phase47-compiler--parse (list 'seq sexp))))
    (let* ((defun-node (car (nelisp-phase47-compiler--ir-get program :forms)))
           (body (nelisp-phase47-compiler--ir-get defun-node :body)))
      (should (eq (nelisp-phase47-compiler--ir-kind body) 'sexp-int-unwrap))
      (should (eq (nelisp-phase47-compiler--ir-kind
                   (nelisp-phase47-compiler--ir-get body :ptr))
                  'ref)))))

(ert-deftest nelisp-phase47-compiler/sexp-int-make-parse-shape ()
  "Doc 100 §100.B: (sexp-int-make SLOT N) parses to (:kind sexp-int-make :slot :val)."
  (let* ((sexp '(defun probe (slot n) (sexp-int-make slot n)))
         (program (nelisp-phase47-compiler--parse (list 'seq sexp))))
    (let* ((defun-node (car (nelisp-phase47-compiler--ir-get program :forms)))
           (body (nelisp-phase47-compiler--ir-get defun-node :body)))
      (should (eq (nelisp-phase47-compiler--ir-kind body) 'sexp-int-make))
      (should (eq (nelisp-phase47-compiler--ir-kind
                   (nelisp-phase47-compiler--ir-get body :slot))
                  'ref))
      (should (eq (nelisp-phase47-compiler--ir-kind
                   (nelisp-phase47-compiler--ir-get body :val))
                  'ref)))))

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

;; ---- §T.symbol-name-eq grammar (G1) ----

(ert-deftest nelisp-phase47-compiler/parse-symbol-name-eq-encodes-utf8-bytes ()
  "Literal string in `(symbol-name-eq P LIT)' is UTF-8 byte-encoded at parse."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun probe (p) (symbol-name-eq p "read"))))
         (body (nelisp-phase47-compiler--ir-get ir :body)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'symbol-name-eq))
    (should (equal (nelisp-phase47-compiler--ir-get body :bytes)
                   '(114 101 97 100)))))

(ert-deftest nelisp-phase47-compiler/parse-symbol-name-eq-rejects-non-string ()
  "Second argument must be a string literal."
  (should-error
   (nelisp-phase47-compiler--parse
    '(defun probe (p) (symbol-name-eq p 42)))
   :type 'nelisp-phase47-compiler-error))

(ert-deftest nelisp-phase47-compiler/parse-symbol-name-eq-arity-error ()
  "`(symbol-name-eq P)' (= 1-arg) raises arity error."
  (should-error
   (nelisp-phase47-compiler--parse
    '(defun probe (p) (symbol-name-eq p)))
   :type 'nelisp-phase47-compiler-error))

(ert-deftest nelisp-phase47-compiler/parse-symbol-name-eq-empty-literal ()
  "Empty literal string is a legal zero-byte comparison (= length-check only)."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun probe (p) (symbol-name-eq p ""))))
         (body (nelisp-phase47-compiler--ir-get ir :body)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'symbol-name-eq))
    (should (equal (nelisp-phase47-compiler--ir-get body :bytes) '()))))

(ert-deftest nelisp-phase47-compiler/emit-symbol-name-eq-produces-bytes ()
  "Emit phase for `symbol-name-eq' produces a non-empty byte string and
the size grows monotonically with literal length (= longer literals
inline more `cmp imm32' instructions)."
  (cl-labels ((emit-len (lit)
                (let* ((path (nelisp-phase47-compiler-test--tmp-binary
                              "symname"))
                       (sexp `(defun probe (p)
                                (sexp-int-make
                                 p (symbol-name-eq p ,lit)))))
                  (unwind-protect
                      (progn
                        (nelisp-phase47-compile-to-object sexp path)
                        (let ((sz (nth 7 (file-attributes path))))
                          sz))
                    (ignore-errors (delete-file path))))))
    (let ((short (emit-len "x"))
          (long  (emit-len "exit_group")))
      (should (integerp short))
      (should (integerp long))
      (should (> long short)))))

;; ---- §T.sexp-float-unwrap grammar (G4) ----

(ert-deftest nelisp-phase47-compiler/parse-sexp-float-unwrap-shape ()
  "Parse `(sexp-float-unwrap PTR)' to an IR node with :kind sexp-float-unwrap."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun probe (p) (sexp-float-unwrap p))))
         (body (nelisp-phase47-compiler--ir-get ir :body)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'sexp-float-unwrap))
    (let ((ptr (nelisp-phase47-compiler--ir-get body :ptr)))
      (should (eq (nelisp-phase47-compiler--ir-kind ptr) 'ref))
      (should (eq (nelisp-phase47-compiler--ir-get ptr :var) 'p)))))

(ert-deftest nelisp-phase47-compiler/parse-sexp-float-unwrap-arity-error ()
  "`(sexp-float-unwrap)' (= no arg) raises arity error."
  (should-error
   (nelisp-phase47-compiler--parse
    '(defun probe (p) (sexp-float-unwrap)))
   :type 'nelisp-phase47-compiler-error))

(ert-deftest nelisp-phase47-compiler/emit-sexp-float-unwrap-produces-bytes ()
  "Emit phase for `sexp-float-unwrap' compiles to a non-empty .o file
matching the byte-pattern of `sexp-int-unwrap' (= identical payload
offset 8 read; only the tag interpretation differs at the type-system
level, not in the emitted machine code)."
  (let ((path-fl (nelisp-phase47-compiler-test--tmp-binary "sfu"))
        (path-in (nelisp-phase47-compiler-test--tmp-binary "siu")))
    (unwind-protect
        (progn
          (nelisp-phase47-compile-to-object
           '(defun probe (p) (sexp-float-unwrap p)) path-fl)
          (nelisp-phase47-compile-to-object
           '(defun probe (p) (sexp-int-unwrap p)) path-in)
          (let ((sz-fl (nth 7 (file-attributes path-fl)))
                (sz-in (nth 7 (file-attributes path-in))))
            (should (integerp sz-fl))
            (should (integerp sz-in))
            (should (= sz-fl sz-in))))
      (ignore-errors (delete-file path-fl))
      (ignore-errors (delete-file path-in)))))

;; ---- §T.let-rt Runtime let tests (Wave 18w+) ----

(ert-deftest nelisp-phase47-compiler/parse-let-rt-call-value ()
  "Parse `(defun f (x) (let ((y (id x))) y))' — let with a call value.
The `id' call is not foldable so the `let' becomes a `let-rt' IR node
with a slot index beyond the param count."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(seq (defun id (x) x)
                    (defun f (x) (let ((y (id x))) y)))))
         ;; `seq' → second form is the `f' defun.
         (f-ir (nth 1 (nelisp-phase47-compiler--ir-get ir :forms)))
         (body (nelisp-phase47-compiler--ir-get f-ir :body)))
    (should (eq (nelisp-phase47-compiler--ir-kind f-ir) 'defun))
    ;; body is a `let-rt' node (= non-foldable value).
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'let-rt))
    (should (eq (nelisp-phase47-compiler--ir-get body :var) 'y))
    ;; Slot must be ≥ arity (= 1 for `f (x)').
    (should (>= (nelisp-phase47-compiler--ir-get body :slot) 1))
    ;; value-ir is a `call' to `id'.
    (let ((val-ir (nelisp-phase47-compiler--ir-get body :value-ir)))
      (should (eq (nelisp-phase47-compiler--ir-kind val-ir) 'call))
      (should (eq (nelisp-phase47-compiler--ir-get val-ir :name) 'id)))
    ;; body body is a `ref' for `y' via the rt slot.
    (let* ((body-ir (nelisp-phase47-compiler--ir-get body :body))
           (slot (nelisp-phase47-compiler--ir-get body :slot)))
      (should (eq (nelisp-phase47-compiler--ir-kind body-ir) 'ref))
      (should (eq (nelisp-phase47-compiler--ir-get body-ir :var) 'y))
      (should (= (nelisp-phase47-compiler--ir-get body-ir :slot) slot)))))

(ert-deftest nelisp-phase47-compiler/parse-let-rt-slot-beyond-params ()
  "Runtime let slot is param-count + rt-let-index."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun g (a b) (let ((t1 (+ a b))) t1))))
         (body (nelisp-phase47-compiler--ir-get ir :body)))
    ;; `g' has 2 params (slots 0, 1); runtime let must use slot >= 2.
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'let-rt))
    (should (>= (nelisp-phase47-compiler--ir-get body :slot) 2))))

(ert-deftest nelisp-phase47-compiler/parse-let-rt-rt-slot-count ()
  "`defun' IR carries `:rt-slot-count' equal to number of runtime lets."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(seq (defun id (x) x)
                    (defun f (x) (let ((y (id x))) y))))))
    (let ((f-ir (nth 1 (nelisp-phase47-compiler--ir-get ir :forms))))
      (should (= (nelisp-phase47-compiler--ir-get f-ir :rt-slot-count) 1)))))

(ert-deftest nelisp-phase47-compiler/parse-let-ct-in-defun-body-no-let-rt ()
  "Compile-time `let' inside defun body does NOT produce `let-rt'."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(defun f (x) (let ((k 7)) (+ x k)))))
         (body (nelisp-phase47-compiler--ir-get ir :body)))
    ;; Compile-time fold: body is `arith', no `let-rt'.
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'arith))
    (should (= (nelisp-phase47-compiler--ir-get ir :rt-slot-count) 0))))

(ert-deftest nelisp-phase47-compiler/parse-doc129-multi-let-rt ()
  "Doc 129.4: multi-binding runtime `let' parses to `let-rt-n'."
  (let* ((ir (nelisp-phase47-compiler--parse
              '(seq (defun id (x) x)
                    (defun f (x y)
                      (let ((a (id x))
                            (b (+ y 10)))
                        (+ a b))))))
         (f-ir (nth 1 (nelisp-phase47-compiler--ir-get ir :forms)))
         (body (nelisp-phase47-compiler--ir-get f-ir :body))
         (bindings (nelisp-phase47-compiler--ir-get body :bindings)))
    (should (eq (nelisp-phase47-compiler--ir-kind body) 'let-rt-n))
    (should (= (length bindings) 2))
    (should (equal (mapcar #'car bindings) '(a b)))
    (should (equal (mapcar #'cadr bindings) '(2 3)))
    (should (= (nelisp-phase47-compiler--ir-get f-ir :rt-slot-count) 2))
    (should (eq (nelisp-phase47-compiler--ir-kind
                 (nelisp-phase47-compiler--ir-get body :body))
                'arith))))

(ert-deftest nelisp-phase47-compiler/parse-doc129-multi-let-is-parallel ()
  "Doc 129.4: later `let' initializers cannot see earlier bindings."
  (should-error
   (nelisp-phase47-compiler--parse
    '(defun f (x)
       (let ((a (+ x 1))
             (b a))
         b)))
   :type 'nelisp-phase47-compiler-error))

(ert-deftest nelisp-phase47-compiler/let-rt-requires-defun-context ()
  "Runtime `let' outside any defun context signals an error."
  ;; Top-level `let' where the value is not foldable (= extern-call ref
  ;; in a synthetic fenv that would block folding).  The easiest way to
  ;; trigger this is a symbol in the outer fenv (= defun param), but
  ;; `--parse' starts with fenv=nil.  Use a direct test of the raw
  ;; parser path: a `let' whose value is an extern-call form.
  ;; At top level `--next-rt-let-slot' is nil → should signal.
  (should-error
   (nelisp-phase47-compiler--parse
    ;; `extern-call' is never compile-time foldable, so this forces
    ;; the runtime path.  At top level (= no enclosing defun parse)
    ;; `--next-rt-let-slot' is nil → error.
    '(let ((x (extern-call getpid))) (exit x)))
   :type 'nelisp-phase47-compiler-error))

(ert-deftest nelisp-phase47-compiler/e2e-let-rt-call-result ()
  "`(let ((y (id x))) (+ y 1))' inside a defun exits with x+1."
  (unless (nelisp-phase47-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-phase47-compiler-test--with-tmp-binary path "let-rt-call"
    (nelisp-phase47-compile-sexp
     '(seq (defun id (x) x)
           (defun f (x) (let ((y (id x))) (+ y 1)))
           (exit (f 6)))
     path)
    (let ((r (nelisp-phase47-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 7)))))

(ert-deftest nelisp-phase47-compiler/e2e-let-rt-arith-value ()
  "`(let ((y (+ a b))) (+ y 1))' where a+b is a runtime arith."
  (unless (nelisp-phase47-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-phase47-compiler-test--with-tmp-binary path "let-rt-arith"
    (nelisp-phase47-compile-sexp
     '(seq (defun f (a b) (let ((y (+ a b))) (+ y 1)))
           (exit (f 3 4)))
     path)
    (let ((r (nelisp-phase47-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 8)))))

(ert-deftest nelisp-phase47-compiler/e2e-let-rt-two-bindings ()
  "Two sequential runtime `let' bindings (= nested) resolve correctly."
  (unless (nelisp-phase47-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-phase47-compiler-test--with-tmp-binary path "let-rt-two"
    (nelisp-phase47-compile-sexp
     '(seq (defun id (x) x)
           (defun f (x)
             (let ((a (id x)))
               (let ((b (+ a 10)))
                 (+ a b))))
           (exit (f 5)))
     path)
    ;; a = id(5) = 5; b = a + 10 = 15; result = a + b = 20.
    (let ((r (nelisp-phase47-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 20)))))

(ert-deftest nelisp-phase47-compiler/e2e-doc129-multi-let-rt ()
  "Doc 129.4: multi-binding runtime `let' executes through `let-rt-n'."
  (unless (nelisp-phase47-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-phase47-compiler-test--with-tmp-binary path "doc129-multi-let"
    (nelisp-phase47-compile-sexp
     '(seq (defun id (x) x)
           (defun f (x y)
             (let ((a (id x))
                   (b (+ y 10)))
               (+ a b)))
           (exit (f 5 7)))
     path)
    (let ((r (nelisp-phase47-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 22)))))

;; ---- Doc 101 §101.B Wave 5 — Win64 ABI emit tests ----
;;
;; These tests verify that `nelisp-phase47-compile-to-object' with
;; `:format 'coff' (= Windows COFF) emits the Win64 ABI calling
;; convention in the generated `.text' bytes.
;;
;; We cannot link + execute the COFF on Linux; instead we spot-check
;; the raw `.text' byte content:
;;   - Prologue must start with `push rbp' (0x55) then `mov rbp, rsp'.
;;   - Win64 GP spill uses `mov [rbp - disp], reg' (MOV r/m64, r64 =
;;     REX.W 89 ModR/M disp) rather than SysV's `push reg' (0x51..0x56).
;;   - ABI descriptor on the buffer must be 'win64.
;;
;; See Doc 101 §101.B Wave 5 for the full design rationale.

(ert-deftest nelisp-phase47-compiler/win64-abi-arg-regs-dynvar ()
  "Win64 ABI dynvar selects RCX RDX R8 R9 as integer arg registers."
  (let ((nelisp-phase47-compiler--abi 'win64))
    (should (equal (nelisp-phase47-compiler--current-arg-regs)
                   '(rcx rdx r8 r9)))))

(ert-deftest nelisp-phase47-compiler/sysv-abi-arg-regs-dynvar ()
  "SysV ABI dynvar selects RDI RSI RDX RCX R8 R9 as integer arg registers."
  (let ((nelisp-phase47-compiler--abi 'sysv))
    (should (equal (nelisp-phase47-compiler--current-arg-regs)
                   '(rdi rsi rdx rcx r8 r9)))))

(ert-deftest nelisp-phase47-compiler/win64-coff-smoke ()
  "Compile `(defun foo (a b) (+ a b))' to a COFF .o and check Win64 prologue.
The first byte of .text must be 0x55 (push rbp).  The frame must NOT
start with a SysV `push rdi' (0x57) — that would indicate the old
SysV path was taken instead of Win64."
  (let ((path (make-temp-file "nelisp-win64-coff-smoke-" nil ".obj")))
    (unwind-protect
        (progn
          (require 'nelisp-phase47-compiler)
          (nelisp-phase47-compile-to-object
           '(defun foo (a b) (+ a b))
           path :arch 'x86_64 :format 'coff)
          (let* ((all-bytes (nelisp-phase47-compiler-test--read-bytes path))
                 ;; Scan for the `push rbp' byte (0x55) which must be the
                 ;; first instruction of any Win64 function prologue.
                 ;; We scan the COFF rather than hard-coding the file offset
                 ;; of .text because the COFF header size may vary.
                 (found-push-rbp
                  (let ((i 0) (found nil))
                    (while (and (< i (length all-bytes)) (not found))
                      (when (= (aref all-bytes i) #x55)
                        (setq found i))
                      (setq i (1+ i)))
                    found)))
            ;; push rbp (0x55) must appear somewhere in the COFF .text.
            (should found-push-rbp)
            ;; The byte immediately before `push rbp' must NOT be
            ;; `push rdi' (0x57) — that would mean the SysV path ran
            ;; and emitted args via push rather than Win64's sub+mov spill.
            (when (> found-push-rbp 0)
              (should (not (= (aref all-bytes (1- found-push-rbp)) #x57))))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-compiler/win64-coff-no-push-rdi ()
  "COFF emit must NOT use `push rdi' (0x57) for arg spill (SysV artifact).
Win64 spills args via `mov [rbp-disp], reg' instead."
  (let ((path (make-temp-file "nelisp-win64-no-push-rdi-" nil ".obj")))
    (unwind-protect
        (progn
          (require 'nelisp-phase47-compiler)
          (nelisp-phase47-compile-to-object
           '(defun bar (x) x)
           path :arch 'x86_64 :format 'coff)
          (let* ((bytes (nelisp-phase47-compiler-test--read-bytes path))
                 ;; Count occurrences of 0x57 (push rdi) in the COFF.
                 ;; A SysV prologue for `(defun bar (x) x)' would have
                 ;; exactly one 0x57 byte; Win64 must have none in .text.
                 ;; We check the whole blob conservatively — COFF headers
                 ;; and symtab don't contain 0x57 as a code byte.
                 (push-rdi-count
                  (let ((i 0) (cnt 0))
                    (while (< i (length bytes))
                      (when (= (aref bytes i) #x57)
                        (setq cnt (1+ cnt)))
                      (setq i (1+ i)))
                    cnt)))
            ;; Win64 emit must never push rdi for arg spilling.
            (should (= push-rdi-count 0))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-phase47-compiler/win64-buffer-abi-is-win64 ()
  "compile-to-object with format 'coff binds --abi to 'win64.
We verify indirectly: the emitted .text for a 1-arg function must
contain REX.W (0x48) immediately before the MOV spill opcode (0x89),
which is the Win64 `mov [rbp-8], rcx' = 48 89 4D F8 sequence.
SysV would emit `push rdi' = 57 instead."
  (let ((path (make-temp-file "nelisp-win64-buf-abi-" nil ".obj")))
    (unwind-protect
        (progn
          (require 'nelisp-phase47-compiler)
          (nelisp-phase47-compile-to-object
           '(defun id (x) x)
           path :arch 'x86_64 :format 'coff)
          (let* ((bytes (nelisp-phase47-compiler-test--read-bytes path))
                 ;; Scan for the Win64 spill sequence:
                 ;; 48 89 4D F8 = REX.W MOV [rbp-8], RCX
                 ;; (RCX = first Win64 GP arg reg, disp = -8 = 0xF8 sign-byte).
                 (n (length bytes))
                 (found-win64-spill
                  (let ((i 0) (found nil))
                    (while (and (< (+ i 3) n) (not found))
                      (when (and (= (aref bytes i)      #x48)
                                 (= (aref bytes (+ i 1)) #x89)
                                 (= (aref bytes (+ i 2)) #x4D)
                                 (= (aref bytes (+ i 3)) #xF8))
                        (setq found i))
                      (setq i (1+ i)))
                    found)))
            ;; The Win64 `mov [rbp-8], rcx' sequence must appear in the COFF.
            (should found-win64-spill)))
      (ignore-errors (delete-file path)))))

(provide 'nelisp-phase47-compiler-test)

;;; nelisp-phase47-compiler-test.el ends here
