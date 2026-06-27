;;; nelisp-aot-compiler-test.el --- ert + e2e for Doc 97 compiler  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ert + end-to-end smoke for `nelisp-aot-compile-sexp'.
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
(require 'nelisp-aot-compiler)

;; ---- §T.0 helpers ----

(defun nelisp-aot-compiler-test--linux-p ()
  "Return non-nil when the host kernel can exec x86_64 ELF64 binaries."
  (and (eq system-type 'gnu/linux)
       (let ((arch (and (boundp 'system-configuration)
                        system-configuration)))
         (and (stringp arch)
              (string-match-p "x86_64\\|amd64" arch)))))

(defun nelisp-aot-compiler-test--run-binary (path)
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

(defun nelisp-aot-compiler-test--tmp-binary (suffix)
  "Return a fresh /tmp/nelisp-doc97-SUFFIX-NNNN path.
The file is not created — `nelisp-aot-compile-sexp' creates it.
Caller is responsible for `delete-file' on cleanup."
  (make-temp-file (format "nelisp-doc97-%s-" suffix)))

;; ---- §T.1 parser unit tests ----

(ert-deftest nelisp-aot-compiler/parse-exit-literal ()
  "Parse `(exit 0)' to an exit IR wrapping an imm value node."
  (let ((ir (nelisp-aot-compiler--parse '(exit 0))))
    (should (eq (nelisp-aot-compiler--ir-kind ir) 'exit))
    (let ((v (nelisp-aot-compiler--ir-get ir :value)))
      (should (eq (nelisp-aot-compiler--ir-kind v) 'imm))
      (should (= (nelisp-aot-compiler--ir-get v :value) 0)))))

(ert-deftest nelisp-aot-compiler/top-level-literal-depth-limit ()
  "Deep literal materialization should decline by returning nil."
  (should-not
   (nelisp-aot-compiler--top-level-literal-write-forms
    'out '(a (b (c (d (e (f (g (h (i (j k)))))))))) 'scratch 10)))

(ert-deftest nelisp-aot-compiler/parse-write-literal ()
  "Parse `(write \"hi\")' to a write IR node."
  (let ((ir (nelisp-aot-compiler--parse '(write "hi"))))
    (should (eq (nelisp-aot-compiler--ir-kind ir) 'write))
    (should (equal (nelisp-aot-compiler--ir-get ir :str) "hi"))))

(ert-deftest nelisp-aot-compiler/parse-seq ()
  "Parse a `seq' of two children into nested IR."
  (let ((ir (nelisp-aot-compiler--parse
             '(seq (write "x") (exit 0)))))
    (should (eq (nelisp-aot-compiler--ir-kind ir) 'seq))
    (should (= (length (nelisp-aot-compiler--ir-get ir :forms)) 2))
    (should (eq (nelisp-aot-compiler--ir-kind
                 (car (nelisp-aot-compiler--ir-get ir :forms)))
                'write))
    (should (eq (nelisp-aot-compiler--ir-kind
                 (cadr (nelisp-aot-compiler--ir-get ir :forms)))
                'exit))))

(ert-deftest nelisp-aot-compiler/parse-let-arith-fold ()
  "Parse `(let ((x (+ 3 4))) (exit x))' folds arith + lookup."
  (let ((ir (nelisp-aot-compiler--parse
             '(let ((x (+ 3 4))) (exit x)))))
    (should (eq (nelisp-aot-compiler--ir-kind ir) 'let))
    (should (eq (nelisp-aot-compiler--ir-get ir :var) 'x))
    (should (= (nelisp-aot-compiler--ir-get ir :value) 7))
    (let* ((body (nelisp-aot-compiler--ir-get ir :body))
           (vnode (nelisp-aot-compiler--ir-get body :value)))
      (should (eq (nelisp-aot-compiler--ir-kind body) 'exit))
      (should (eq (nelisp-aot-compiler--ir-kind vnode) 'imm))
      (should (= (nelisp-aot-compiler--ir-get vnode :value) 7)))))

(ert-deftest nelisp-aot-compiler/parse-nested-let-arith ()
  "Nested let + chained arithmetic resolves to a single integer."
  (let ((ir (nelisp-aot-compiler--parse
             '(let ((a 2)) (let ((b (* a 5))) (exit (+ b 1)))))))
    (should (eq (nelisp-aot-compiler--ir-kind ir) 'let))
    (let* ((body (nelisp-aot-compiler--ir-get ir :body))
           (inner (nelisp-aot-compiler--ir-get body :body))
           (vnode (nelisp-aot-compiler--ir-get inner :value)))
      (should (eq (nelisp-aot-compiler--ir-kind vnode) 'imm))
      (should (= (nelisp-aot-compiler--ir-get vnode :value) 11)))))

(ert-deftest nelisp-aot-compiler/parse-free-symbol-errors ()
  "A free symbol reference signals `nelisp-aot-compiler-error'."
  (should-error
   (nelisp-aot-compiler--parse '(exit y))
   :type 'nelisp-aot-compiler-error))

(ert-deftest nelisp-aot-compiler/parse-unknown-form-errors ()
  "An unrecognised form head signals an error."
  (should-error
   (nelisp-aot-compiler--parse '(exit (unknown-form 1)))
   :type 'nelisp-aot-compiler-error))

(ert-deftest nelisp-aot-compiler/parse-status-out-of-range ()
  "An exit status outside 0..255 signals."
  (should-error
   (nelisp-aot-compiler--parse '(exit 300))
   :type 'nelisp-aot-compiler-error))

(ert-deftest nelisp-aot-compiler/parse-doc129-when-macro ()
  "Doc 129.1: host `when' macro expands to AOT `if'."
  (let* ((ir (nelisp-aot-compiler--parse
              '(exit (when (= 1 1) 7))))
         (vnode (nelisp-aot-compiler--ir-get ir :value)))
    (should (eq (nelisp-aot-compiler--ir-kind vnode) 'if))
    (should (= (nelisp-aot-compiler--ir-get
                (nelisp-aot-compiler--ir-get vnode :else) :value)
               0))))

(ert-deftest nelisp-aot-compiler/parse-doc129-let*-desugar ()
  "Doc 129.1: `let*' desugars to nested AOT `let' forms."
  (let* ((ir (nelisp-aot-compiler--parse
              '(let* ((a 2) (b (+ a 5))) (exit b))))
         (body (nelisp-aot-compiler--ir-get ir :body))
         (exit-node (nelisp-aot-compiler--ir-get body :body))
         (exit-value (nelisp-aot-compiler--ir-get exit-node :value)))
    (should (eq (nelisp-aot-compiler--ir-kind ir) 'let))
    (should (eq (nelisp-aot-compiler--ir-kind body) 'let))
    (should (= (nelisp-aot-compiler--ir-get exit-value :value) 7))))

(ert-deftest nelisp-aot-compiler/parse-doc129-top-level-defmacro ()
  "Doc 129.1: top-level `defmacro' is compile-time-only."
  (let* ((ir (nelisp-aot-compiler--parse
              '(seq
                (defmacro inc (x) (list '+ x 1))
                (exit (inc 6)))))
         (forms (nelisp-aot-compiler--ir-get ir :forms))
         (exit-node (car forms))
         (vnode (nelisp-aot-compiler--ir-get exit-node :value)))
    (should (= (length forms) 1))
    (should (eq (nelisp-aot-compiler--ir-kind vnode) 'imm))
    (should (= (nelisp-aot-compiler--ir-get vnode :value) 7))))

;; ---- §T.2 emit / byte-length tests ----

(ert-deftest nelisp-aot-compiler/emit-exit-is-16-bytes ()
  "`(exit 0)' emits exactly 16 bytes of .text (= Doc 92 fixed length)."
  (let* ((ir (nelisp-aot-compiler--parse '(exit 0)))
         (buf (nelisp-aot-compiler--pass ir nil nil 0)))
    (should (= (nelisp-asm-x86_64-buffer-pos buf) 16))))

(ert-deftest nelisp-aot-compiler/emit-write-is-33-bytes ()
  "`(write \"hi\")' emits exactly 33 bytes of .text."
  (let* ((ir (nelisp-aot-compiler--parse '(write "hi")))
         (collected (nelisp-aot-compiler--collect-strings ir))
         (offsets (car collected))
         (buf (nelisp-aot-compiler--pass ir nil offsets #x401000)))
    (should (= (nelisp-asm-x86_64-buffer-pos buf) 33))))

(ert-deftest nelisp-aot-compiler/strings-dedup ()
  "Two `(write \"x\")' forms share a single .rodata slot."
  (let* ((ir (nelisp-aot-compiler--parse
              '(seq (write "x") (write "x") (exit 0))))
         (collected (nelisp-aot-compiler--collect-strings ir))
         (offsets (car collected))
         (rodata (cdr collected)))
    (should (= (length offsets) 1))
    (should (= (length rodata) 1))
    (should (equal (substring rodata 0 1) "x"))))

(ert-deftest nelisp-aot-compiler/strings-distinct ()
  "Two distinct strings get distinct offsets summing to total length."
  (let* ((ir (nelisp-aot-compiler--parse
              '(seq (write "ab") (write "cd") (exit 0))))
         (collected (nelisp-aot-compiler--collect-strings ir))
         (offsets (car collected))
         (rodata (cdr collected)))
    (should (= (length offsets) 2))
    (should (= (length rodata) 4))
    (should (equal rodata "abcd"))
    (should (= (plist-get (cdr (assoc "ab" offsets)) :offset) 0))
    (should (= (plist-get (cdr (assoc "cd" offsets)) :offset) 2))))

(ert-deftest nelisp-aot-compiler/pass1-pass2-byte-length-parity ()
  "Pass-1 and pass-2 emit must agree on .text byte length."
  (let* ((ir (nelisp-aot-compiler--parse
              '(seq (write "hi") (exit 0))))
         (collected (nelisp-aot-compiler--collect-strings ir))
         (offsets (car collected))
         (pass1 (nelisp-aot-compiler--pass ir nil offsets 0))
         (size1 (nelisp-asm-x86_64-buffer-pos pass1))
         (pass2 (nelisp-aot-compiler--pass ir nil offsets
                                               (+ #x400000 size1 #x78)))
         (size2 (nelisp-asm-x86_64-buffer-pos pass2)))
    (should (= size1 size2))))

;; ---- §T.3 e2e smoke (= the production engagement gate) ----

(ert-deftest nelisp-aot-compiler/e2e-hello-world ()
  "Compile `(seq (write \"hello\\n\") (exit 0))', exec, observe."
  (unless (nelisp-aot-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (let ((path (nelisp-aot-compiler-test--tmp-binary "hello")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-sexp
           '(seq (write "hello\n") (exit 0)) path)
          (should (file-executable-p path))
          (let ((result (nelisp-aot-compiler-test--run-binary path)))
            (should (equal (plist-get result :stdout) "hello\n"))
            (should (= (plist-get result :exit) 0))))
      (when (file-exists-p path) (delete-file path)))))

(ert-deftest nelisp-aot-compiler/e2e-exit-42 ()
  "Compile `(exit 42)', exec, observe exit code 42."
  (unless (nelisp-aot-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (let ((path (nelisp-aot-compiler-test--tmp-binary "exit42")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-sexp '(exit 42) path)
          (let ((result (nelisp-aot-compiler-test--run-binary path)))
            (should (= (plist-get result :exit) 42))
            (should (equal (plist-get result :stdout) ""))))
      (when (file-exists-p path) (delete-file path)))))

(ert-deftest nelisp-aot-compiler/e2e-let-exit ()
  "Compile `(seq (let ((x 7)) (exit x)))', exec, observe exit code 7."
  (unless (nelisp-aot-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (let ((path (nelisp-aot-compiler-test--tmp-binary "let-exit")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-sexp
           '(seq (let ((x 7)) (exit x))) path)
          (let ((result (nelisp-aot-compiler-test--run-binary path)))
            (should (= (plist-get result :exit) 7))))
      (when (file-exists-p path) (delete-file path)))))

(ert-deftest nelisp-aot-compiler/e2e-arith-exit ()
  "Compile `(exit (+ 1 2))', exec, observe exit code 3."
  (unless (nelisp-aot-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (let ((path (nelisp-aot-compiler-test--tmp-binary "arith-exit")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-sexp '(exit (+ 1 2)) path)
          (let ((result (nelisp-aot-compiler-test--run-binary path)))
            (should (= (plist-get result :exit) 3))))
      (when (file-exists-p path) (delete-file path)))))

(ert-deftest nelisp-aot-compiler/e2e-doc129-macroexpand-front ()
  "Doc 129.1: compile host macros and `let*' after frontend expansion."
  (unless (nelisp-aot-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (let ((path (nelisp-aot-compiler-test--tmp-binary "doc129-macro")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-sexp
           '(seq
             (defmacro inc (x) (list '+ x 1))
             (let* ((a 3)
                    (b (inc a)))
               (exit (unless (= b 0) b))))
           path)
          (let ((result (nelisp-aot-compiler-test--run-binary path)))
            (should (= (plist-get result :exit) 4))))
      (when (file-exists-p path) (delete-file path)))))

(ert-deftest nelisp-aot-compiler/e2e-multi-write ()
  "Compile two-write seq, observe concatenated stdout + exit 0."
  (unless (nelisp-aot-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (let ((path (nelisp-aot-compiler-test--tmp-binary "multi-write")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-sexp
           '(seq (write "hi") (write " there\n") (exit 0)) path)
          (let ((result (nelisp-aot-compiler-test--run-binary path)))
            (should (equal (plist-get result :stdout) "hi there\n"))
            (should (= (plist-get result :exit) 0))))
      (when (file-exists-p path) (delete-file path)))))

(ert-deftest nelisp-aot-compiler/e2e-string-dedup-execs ()
  "Duplicated string survives dedup + runs."
  (unless (nelisp-aot-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (let ((path (nelisp-aot-compiler-test--tmp-binary "dedup")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-sexp
           '(seq (write "ab") (write "ab") (exit 0)) path)
          (let ((result (nelisp-aot-compiler-test--run-binary path)))
            (should (equal (plist-get result :stdout) "abab"))
            (should (= (plist-get result :exit) 0))))
      (when (file-exists-p path) (delete-file path)))))

(ert-deftest nelisp-aot-compiler/e2e-arch-rejects-non-x86_64 ()
  "Asking for `:arch 'aarch64' signals (= v1 OOS)."
  (let ((path (nelisp-aot-compiler-test--tmp-binary "arch-rej")))
    (unwind-protect
        (should-error
         (nelisp-aot-compile-sexp '(exit 0) path :arch 'aarch64)
         :type 'nelisp-aot-compiler-error)
      (when (file-exists-p path) (delete-file path)))))

;; ---- §T.4 §97.b defun + call parser tests ----

(ert-deftest nelisp-aot-compiler/parse-defun-zero-arg ()
  "Parse `(defun seven () 7)' to a defun IR node with no params."
  (let ((ir (nelisp-aot-compiler--parse '(defun seven () 7))))
    (should (eq (nelisp-aot-compiler--ir-kind ir) 'defun))
    (should (eq (nelisp-aot-compiler--ir-get ir :name) 'seven))
    (should (null (nelisp-aot-compiler--ir-get ir :params)))
    (should (null (nelisp-aot-compiler--ir-get ir :param-regs)))
    (let ((body (nelisp-aot-compiler--ir-get ir :body)))
      (should (eq (nelisp-aot-compiler--ir-kind body) 'imm))
      (should (= (nelisp-aot-compiler--ir-get body :value) 7)))))

(ert-deftest nelisp-aot-compiler/parse-defun-docstring-metadata ()
  "Parse defun docstrings and metadata as non-runtime forms."
  (let ((ir (nelisp-aot-compiler--parse
             '(defun seven ()
                "Return seven."
                (declare (pure t))
                (interactive)
                7))))
    (should (eq (nelisp-aot-compiler--ir-kind ir) 'defun))
    (let ((body (nelisp-aot-compiler--ir-get ir :body)))
      (should (eq (nelisp-aot-compiler--ir-kind body) 'imm))
      (should (= (nelisp-aot-compiler--ir-get body :value) 7)))))

(ert-deftest nelisp-aot-compiler/parse-defun-param-ref ()
  "Parse `(defun id (x) x)' — body becomes a `:kind ref' node."
  (let ((ir (nelisp-aot-compiler--parse '(defun id (x) x))))
    (should (eq (nelisp-aot-compiler--ir-kind ir) 'defun))
    (should (equal (nelisp-aot-compiler--ir-get ir :params) '(x)))
    (should (equal (nelisp-aot-compiler--ir-get ir :param-regs) '(rdi)))
    (let ((body (nelisp-aot-compiler--ir-get ir :body)))
      (should (eq (nelisp-aot-compiler--ir-kind body) 'ref))
      (should (eq (nelisp-aot-compiler--ir-get body :reg) 'rdi)))))

(ert-deftest nelisp-aot-compiler/parse-doc129-sexp-param-root-map ()
  "Doc 129.5A: `:type sexp' params become static GC root slots when allocating."
  (let* ((ir (nelisp-aot-compiler--parse
              '(defun make-str ((slot :type sexp) bytes len)
                 (sexp-write-str slot bytes len))))
         (body (nelisp-aot-compiler--ir-get ir :body))
         (slot-ref (nelisp-aot-compiler--ir-get body :slot))
         (bytes-ref (nelisp-aot-compiler--ir-get body :bytes-ptr)))
    (should (eq (nelisp-aot-compiler--ir-get ir :param-class) 'gp))
    (should (equal (nelisp-aot-compiler--ir-get ir :gc-root-slots) '(0)))
    (should (nelisp-aot-compiler--ir-get slot-ref :root-p))
    (should-not (nelisp-aot-compiler--ir-get bytes-ref :root-p))))

(ert-deftest nelisp-aot-compiler/parse-doc129-no-allocation-no-root-map ()
  "Doc 129.5A: annotated Sexp refs do not create a root map without allocation."
  (let ((ir (nelisp-aot-compiler--parse
             '(defun tag-of ((x :type sexp)) (sexp-tag x)))))
    (should (null (nelisp-aot-compiler--ir-get ir :gc-root-slots)))))

(ert-deftest nelisp-aot-compiler/parse-defun-too-many-params ()
  "Defun still rejects params beyond the current disp8 local-slot cap."
  (should-error
   (nelisp-aot-compiler--parse
    '(defun fifteen-arg (a b c d e f g h i j k l m n o) a))
   :type 'nelisp-aot-compiler-error))

(ert-deftest nelisp-aot-compiler/parse-call-arity-mismatch ()
  "Calling a defun with wrong arg count signals."
  (should-error
   (nelisp-aot-compiler--parse
    '(seq (defun id (x) x) (exit (id 1 2))))
   :type 'nelisp-aot-compiler-error))

(ert-deftest nelisp-aot-compiler/parse-runtime-arith ()
  "Parse `(+ a b)' inside a defun yields a `:kind arith' node."
  (let* ((ir (nelisp-aot-compiler--parse
              '(defun add (a b) (+ a b))))
         (body (nelisp-aot-compiler--ir-get ir :body)))
    (should (eq (nelisp-aot-compiler--ir-kind body) 'arith))
    (should (eq (nelisp-aot-compiler--ir-get body :op) '+))
    (should (eq (nelisp-aot-compiler--ir-kind
                 (nelisp-aot-compiler--ir-get body :a))
                'ref))
    (should (eq (nelisp-aot-compiler--ir-kind
                 (nelisp-aot-compiler--ir-get body :b))
                'ref))))

(ert-deftest nelisp-aot-compiler/parse-runtime-mod ()
  "Parse `(mod a b)' inside a defun yields a `:kind arith' node."
  (let* ((ir (nelisp-aot-compiler--parse
              '(defun rem2 (a b) (mod a b))))
         (body (nelisp-aot-compiler--ir-get ir :body)))
    (should (eq (nelisp-aot-compiler--ir-kind body) 'arith))
    (should (eq (nelisp-aot-compiler--ir-get body :op) 'mod))))

(ert-deftest nelisp-aot-compiler/parse-integer-math-core-forms ()
  "Parse deterministic integer math helpers as core value expressions."
  (let* ((ir (nelisp-aot-compiler--parse
              '(defun math1 (a b)
                 (+ (floor (/ a b))
                    (max (abs a) (min b 9))))))
         (body (nelisp-aot-compiler--ir-get ir :body)))
    (should (eq (nelisp-aot-compiler--ir-kind body) 'arith))
    (should (eq (nelisp-aot-compiler--ir-get body :op) '+))))

(ert-deftest nelisp-aot-compiler/parse-call-in-exit ()
  "Parse `(exit (id 7))' yields an exit IR wrapping a call value."
  (let ((ir (nelisp-aot-compiler--parse
             '(seq (defun id (x) x) (exit (id 7))))))
    (should (eq (nelisp-aot-compiler--ir-kind ir) 'seq))
    (let* ((forms (nelisp-aot-compiler--ir-get ir :forms))
           (exit-node (nth 1 forms))
           (vnode (nelisp-aot-compiler--ir-get exit-node :value)))
      (should (eq (nelisp-aot-compiler--ir-kind exit-node) 'exit))
      (should (eq (nelisp-aot-compiler--ir-kind vnode) 'call))
      (should (eq (nelisp-aot-compiler--ir-get vnode :name) 'id)))))

(ert-deftest nelisp-aot-compiler/collect-defuns ()
  "`--collect-defuns' extracts every defun in encounter order."
  (let* ((ir (nelisp-aot-compiler--parse
              '(seq (defun a () 1) (defun b (x) x) (exit (a)))))
         (defuns (nelisp-aot-compiler--collect-defuns ir)))
    (should (= (length defuns) 2))
    (should (eq (nelisp-aot-compiler--ir-get (nth 0 defuns) :name) 'a))
    (should (eq (nelisp-aot-compiler--ir-get (nth 1 defuns) :name) 'b))))

(ert-deftest nelisp-aot-compiler/parse-duplicate-defun-errors ()
  "Two `(defun NAME ...)` with the same name signals."
  (should-error
   (nelisp-aot-compiler--parse
    '(seq (defun f () 1) (defun f () 2) (exit (f))))
   :type 'nelisp-aot-compiler-error))

;; ---- §T.5 §97.b e2e smoke ----

(defmacro nelisp-aot-compiler-test--with-tmp-binary
    (var suffix &rest body)
  "Bind VAR to a fresh tmp binary path for SUFFIX, run BODY, clean up."
  (declare (indent 2))
  `(let ((,var (nelisp-aot-compiler-test--tmp-binary ,suffix)))
     (unwind-protect
         (progn ,@body)
       (when (file-exists-p ,var) (delete-file ,var)))))

(ert-deftest nelisp-aot-compiler/e2e-defun-zero-arg ()
  "`(seq (defun seven () 7) (exit (seven)))' exits 7."
  (unless (nelisp-aot-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-aot-compiler-test--with-tmp-binary path "seven"
    (nelisp-aot-compile-sexp
     '(seq (defun seven () 7) (exit (seven))) path)
    (should (file-executable-p path))
    (let ((r (nelisp-aot-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 7)))))

(ert-deftest nelisp-aot-compiler/e2e-defun-one-arg ()
  "`(seq (defun id (x) x) (exit (id 42)))' exits 42."
  (unless (nelisp-aot-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-aot-compiler-test--with-tmp-binary path "id"
    (nelisp-aot-compile-sexp
     '(seq (defun id (x) x) (exit (id 42))) path)
    (let ((r (nelisp-aot-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 42)))))

(ert-deftest nelisp-aot-compiler/e2e-defun-add-two ()
  "`(seq (defun add (a b) (+ a b)) (exit (add 3 4)))' exits 7."
  (unless (nelisp-aot-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-aot-compiler-test--with-tmp-binary path "add2"
    (nelisp-aot-compile-sexp
     '(seq (defun add (a b) (+ a b)) (exit (add 3 4))) path)
    (let ((r (nelisp-aot-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 7)))))

(ert-deftest nelisp-aot-compiler/e2e-call-ptr-via-addr-of ()
  "Doc 133 Phase 0: indirect call through a function pointer.
`via' receives `target's address (via `addr-of') and calls it
through `call-ptr'.  target(5) = 5 + 100 = 105 -> exit 105."
  (unless (nelisp-aot-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-aot-compiler-test--with-tmp-binary path "callptr"
    (nelisp-aot-compile-sexp
     '(seq (defun target (x) (+ x 100))
           (defun via (p x) (call-ptr p x))
           (exit (via (addr-of target) 5)))
     path)
    (should (file-executable-p path))
    (let ((r (nelisp-aot-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 105)))))

(ert-deftest nelisp-aot-compiler/e2e-native-exec-mmap-call-ptr ()
  "Doc 142 §6.4 gate 6 — general native EXEC mechanism in the standalone
runtime: mmap a RWX page, memcpy a reloc-free function's machine code
into it, then CALL THE COPIED CODE through a runtime-computed pointer via
`call-ptr'.  This is the core of dynamically loading a `.neln' artifact's
native object in-process (no static link): the code is moved to a fresh
executable page and run.  target(9) = 9*9 = 81 -> exit 81."
  (unless (nelisp-aot-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-aot-compiler-test--with-tmp-binary path "native-exec"
    (nelisp-aot-compile-sexp
     '(seq
       (defun target (x) (* x x))      ; reloc-free leaf, position-independent
       (defun memcpy8 (dst src n)
         (let ((i 0))
           (while (< i n)
             (ptr-write-u64 (+ dst i) 0 (ptr-read-u64 (+ src i) 0))
             (setq i (+ i 8)))
           dst))
       (defun native-exec (x)
         ;; mmap(NULL, 4096, PROT_READ|WRITE|EXEC, MAP_PRIVATE|ANON, -1, 0)
         (let ((page (syscall-direct 9 0 4096 7 34 -1 0)))
           (if (< page 4096)
               99                        ; mmap failed sentinel
             (seq (memcpy8 page (addr-of target) 256)   ; load code into RX page
                  (call-ptr page x)))))                 ; EXEC the loaded code
       (exit (native-exec 9)))
     path)
    (should (file-executable-p path))
    (let ((r (nelisp-aot-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 81)))))

(ert-deftest nelisp-aot-compiler/e2e-native-exec-neln-artifact-in-process ()
  "Doc 142 §6.4 gate 6 — load a REAL reloc-free `.neln' artifact's native
code into an mmap'd RWX page and run it IN-PROCESS via `call-ptr', with no
static link and no host harness.  Generalizes `e2e-native-exec-mmap-call-ptr'
from in-binary `addr-of' code to bytes sourced from a compiled artifact file:
compile (defun sq (x) (* x x)) to a .neln, extract its reloc-free `.text',
copy it byte-for-byte into a fresh executable page, and `call-ptr' the entry.
sq(9) = 81 -> exit 81."
  (unless (nelisp-aot-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (require 'nelisp-artifact)
  (let* ((dir (make-temp-file "neln-inproc-" t))
         (src (expand-file-name "sq.el" dir))
         (out (expand-file-name "sq.neln" dir)))
    (unwind-protect
        (progn
          (with-temp-file src (insert "(defun sq (x) (* x x))\n(provide 'sq)\n"))
          (load src nil t)
          (nelisp-artifact-compile-file src out nil nil nil nil nil 'neln)
          (let* ((native (plist-get (nelisp-artifact--read-payload out) :native))
                 (defun0 (car (plist-get native :defuns)))
                 (offset (plist-get defun0 :offset))
                 (bytes (string-to-list
                         (base64-decode-string (plist-get native :text-base64)))))
            ;; reloc-free, extern-free precondition: pure in-process exec needs
            ;; no symbol resolution and no boundary trampoline.
            (should (null (plist-get native :relocs)))
            (should (null (plist-get native :extern-symbols)))
            (nelisp-aot-compiler-test--with-tmp-binary path "neln-inproc"
              (let* ((i -1)
                     (writes (mapcar (lambda (b)
                                       (setq i (1+ i))
                                       `(ptr-write-u8 page ,i ,b))
                                     bytes))
                     (sexp `(seq
                             (defun native-exec (x)
                               ;; mmap(NULL,4096,PROT_RWX,MAP_PRIVATE|ANON,-1,0)
                               (let ((page (syscall-direct 9 0 4096 7 34 -1 0)))
                                 (if (< page 4096)
                                     99
                                   (seq ,@writes
                                        (call-ptr (+ page ,offset) x)))))
                             (exit (native-exec 9)))))
                (nelisp-aot-compile-sexp sexp path)
                (should (file-executable-p path))
                (let ((r (nelisp-aot-compiler-test--run-binary path)))
                  (should (= (plist-get r :exit) 81)))))))
      (delete-directory dir t))))

(defun nelisp-aot-compiler-test--u32le-bytes (value)
  "Return VALUE as 4 little-endian bytes."
  (let ((u (logand value #xffffffff)))
    (list (logand u #xff)
          (logand (ash u -8) #xff)
          (logand (ash u -16) #xff)
          (logand (ash u -24) #xff))))

(defun nelisp-aot-compiler-test--native-trampoline-slot-disp (slot-index)
  "Return the rbp-relative spill displacement for SLOT-INDEX."
  (- (* 8 (1+ slot-index))))

(defun nelisp-aot-compiler-test--native-mov-rbp-disp-reg-bytes (reg disp)
  "Return `mov [rbp+DISP], REG' bytes for the x86_64 SysV trampoline."
  (let ((spec (alist-get reg '((rax . (#x48 #x45 #x85))
                               (rcx . (#x48 #x4d #x8d))
                               (rdx . (#x48 #x55 #x95))
                               (rsi . (#x48 #x75 #xb5))
                               (rdi . (#x48 #x7d #xbd))
                               (r8 . (#x4c #x45 #x85))
                               (r9 . (#x4c #x4d #x8d))))))
    (unless spec
      (error "unsupported trampoline register %S" reg))
    (pcase-let ((`(,rex ,modrm8 ,modrm32) spec))
      (append (list rex #x89 (if (<= -128 disp 127) modrm8 modrm32))
              (if (<= -128 disp 127)
                  (list (logand disp #xff))
                (nelisp-aot-compiler-test--u32le-bytes disp))))))

(defun nelisp-aot-compiler-test--ptr-write-u8-forms (base-sym bytes)
  "Return `(ptr-write-u8 ...)' forms that copy BYTES into BASE-SYM."
  (let ((idx -1))
    (mapcar (lambda (byte)
              (setq idx (1+ idx))
              `(ptr-write-u8 ,base-sym ,idx ,byte))
            bytes)))

(defun nelisp-aot-compiler-test--native-trampoline-bytes (meta)
  "Return raw trampoline bytes and imm64 patch offsets for META."
  (let* ((arity (or (plist-get meta :arity) 0))
         (frame-bytes (nelisp-artifact--native-trampoline-frame-bytes meta))
         (arg-regs '(rdi rsi rdx rcx r8 r9))
         (bytes nil)
         (imm64-offsets nil))
    (unless (<= arity (length arg-regs))
      (error "trampoline arity %d exceeds gp register support" arity))
    (cl-labels
        ((emit (chunk)
           (setq bytes (append bytes chunk)))
         (emit-imm64-placeholder ()
           (push (+ (length bytes) 2) imm64-offsets)
           (emit '(#x48 #xb8 0 0 0 0 0 0 0 0))))
      (emit '(#x55 #x48 #x89 #xe5))
      (when (> frame-bytes 0)
        (emit (append '(#x48 #x81 #xec)
                      (nelisp-aot-compiler-test--u32le-bytes frame-bytes))))
      (dotimes (i arity)
        (emit
         (nelisp-aot-compiler-test--native-mov-rbp-disp-reg-bytes
          (nth i arg-regs)
          (nelisp-aot-compiler-test--native-trampoline-slot-disp i))))
      (dotimes (i (+ 5 12))
        (emit-imm64-placeholder)
        (emit
         (nelisp-aot-compiler-test--native-mov-rbp-disp-reg-bytes
          'rax
          (nelisp-aot-compiler-test--native-trampoline-slot-disp
           (+ arity i)))))
      (emit-imm64-placeholder)
      (emit '(#xff #xe0)))
    (list :bytes bytes
          :imm64-offsets (nreverse imm64-offsets))))

(ert-deftest nelisp-aot-compiler/e2e-native-exec-neln-artifact-call1-in-process ()
  "Doc 142 §6.4 gate 6 (in-process MECHANISM) — load a builtin-calling
`.neln' artifact and execute it in-process.  The standalone program copies
the artifact `.text' into an mmap'd RX page, builds absolute-jump stubs
(a `plt32' rel32 from an mmap(NULL) page is out of signed-32-bit range of
the helpers) and patches the artifact's two `plt32' relocs to those stubs,
writes a raw machine-code trampoline mirroring the host-proof spill-frame
ABI (args + the out/mirror/frames/scratch/name_slot + 12 callback boundary
slots), then `call-ptr's the entry so `inc1(41)' -> 42.

NOTE: the extern helpers `nl_alloc_symbol' / `nelisp_aot_builtin_call1' here
are TEST-LOCAL subset shims, because `nelisp-aot-compile-sexp' builds a
MINIMAL binary with no runtime env.  This proves the in-process loader
MECHANISM end-to-end (trampoline bytes, reloc patching via abs-stubs,
boundary slots, call-ptr, boxed-result decode).  Wiring the loader to the
REAL reader-linked `nelisp_aot_builtin_call1' (which `nelisp-standalone--reader-units'
already links) inside the full standalone reader is the remaining gate-6
integration step."
  (unless (nelisp-aot-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (require 'nelisp-artifact)
  (let* ((dir (make-temp-file "neln-inproc-call1-" t))
         (src (expand-file-name "inc1.el" dir))
         (out (expand-file-name "inc1.neln" dir)))
    (unwind-protect
        (progn
          (with-temp-file src
            (insert "(defun inc1 (x) (1+ x))\n(provide 'inc1)\n"))
          (load src nil t)
          (nelisp-artifact-compile-file src out nil nil nil nil nil 'neln)
          (let* ((native (plist-get (nelisp-artifact--read-payload out) :native))
                 (defun0 (car (plist-get native :defuns)))
                 (text-bytes (string-to-list
                              (base64-decode-string
                               (plist-get native :text-base64))))
                 (relocs (plist-get native :relocs))
                 (externs (plist-get native :extern-symbols))
                 (trampoline
                  (nelisp-aot-compiler-test--native-trampoline-bytes defun0))
                 (trampoline-bytes (plist-get trampoline :bytes))
                 (imm64-offsets (plist-get trampoline :imm64-offsets))
                 (stub-template-bytes '(#x48 #xb8 0 0 0 0 0 0 0 0 #xff #xe0))
                 (stub-specs
                  (cl-loop for name in externs
                           for idx from 0
                           collect (list :name name :offset (+ 256 (* idx 16)))))
                 (body-entry (+ (plist-get defun0 :offset)
                                (plist-get defun0 :body-offset))))
            (should (equal (plist-get defun0 :name) "inc1"))
            (should (= (plist-get defun0 :arity) 1))
            (should (= (plist-get defun0 :rt-slot-count) 17))
            (should (= (plist-get defun0 :body-offset) 19))
            (should (equal (sort (copy-sequence externs) #'string<)
                           '("nelisp_aot_builtin_call1" "nl_alloc_symbol")))
            (should (= (length relocs) 2))
            (should (equal (mapcar (lambda (reloc) (plist-get reloc :type)) relocs)
                           '(plt32 plt32)))
            (should (= (length imm64-offsets) 18))
            (let* ((text-writes
                    (nelisp-aot-compiler-test--ptr-write-u8-forms
                     'codepage text-bytes))
                   (reloc-forms
                    (mapcar
                     (lambda (reloc)
                        (let* ((offset (plist-get reloc :offset))
                              (stub
                               (cl-find-if
                                (lambda (spec)
                                  (equal (plist-get spec :name)
                                         (plist-get reloc :symbol)))
                                stub-specs))
                              (stub-offset (or (plist-get stub :offset)
                                               (error "missing stub for %s"
                                                      (plist-get reloc :symbol))))
                              (addend (or (plist-get reloc :addend) 0)))
                         `(ptr-write-u32 codepage ,offset
                                         (- (+ codepage ,stub-offset ,addend)
                                            (+ codepage ,offset)))))
                     relocs))
                   (stub-writes
                    (apply
                     #'append
                     (mapcar
                      (lambda (spec)
                        (nelisp-aot-compiler-test--ptr-write-u8-forms
                         `(+ codepage ,(plist-get spec :offset))
                         stub-template-bytes))
                      stub-specs)))
                   (stub-patches
                    (mapcar
                     (lambda (spec)
                       `(ptr-write-u64 (+ codepage ,(plist-get spec :offset))
                                       2
                                       (addr-of ,(intern (plist-get spec :name)))))
                     stub-specs))
                   (trampoline-writes
                    (nelisp-aot-compiler-test--ptr-write-u8-forms
                     'trampage trampoline-bytes))
                   (slot-values
                    (append
                     (list '(+ slots 0) 0 0 '(+ slots 32) '(+ slots 64))
                     (cl-loop for i from 0 below 12
                              collect `(+ slots ,(+ 96 (* i 32))))
                     (list `(+ codepage ,body-entry))))
                   (imm64-patches
                   (cl-mapcar
                     (lambda (offset value)
                       `(ptr-write-u64 trampage ,offset ,value))
                     imm64-offsets
                     slot-values))
                   (read-int-slot-def
                    '(defun read-int-slot (slot)
                       (if (= (ptr-read-u64 slot 0) 2)
                           (ptr-read-u64 slot 8)
                         98)))
                   (zero-slot-def
                    '(defun zero-slot (slot)
                       (seq
                        (ptr-write-u64 slot 0 0)
                        (ptr-write-u64 (+ slot 8) 0 0)
                        (ptr-write-u64 (+ slot 16) 0 0)
                        (ptr-write-u64 (+ slot 24) 0 0)
                        0)))
                   (write-int-slot-def
                    '(defun write-int-slot (slot value)
                       (seq
                        (ptr-write-u64 slot 0 2)
                        (ptr-write-u64 (+ slot 8) 0 value)
                        (ptr-write-u64 (+ slot 16) 0 0)
                        (ptr-write-u64 (+ slot 24) 0 0)
                        slot)))
                   (nl-alloc-symbol-def
                    '(defun nl_alloc_symbol (bytes-ptr len result-slot)
                       (seq
                        (ptr-write-u64 result-slot 0 4)
                        (ptr-write-u64 (+ result-slot 8) 0 (ptr-read-u64 bytes-ptr 0))
                        (ptr-write-u64 (+ result-slot 16) 0 len)
                        (ptr-write-u64 (+ result-slot 24) 0 0)
                        result-slot)))
                   (builtin-call1-def
                    '(defun nelisp_aot_builtin_call1 (_mirror _frames name arg out _scratch)
                       (if (= (ptr-read-u64 (+ name 16) 0) 2)
                           (if (= (ptr-read-u8 (+ name 8) 0) 49)
                               (if (= (ptr-read-u8 (+ name 8) 1) 43)
                                   (seq (write-int-slot out (+ arg 1)) out)
                                 (seq (write-int-slot out 254) out))
                             (seq (write-int-slot out 253) out))
                         (seq (write-int-slot out 252) out))))
                   (native-exec-seq
                    `(seq
                      ,@text-writes
                      ,@stub-writes
                      ,@stub-patches
                      ,@reloc-forms
                      ,@trampoline-writes
                      ,@imm64-patches
                      (call-ptr trampage x)
                      (read-int-slot slots)))
                   (native-exec-def
                    `(defun native-exec (x)
                       (let ((codepage (syscall-direct 9 0 4096 7 34 -1 0)))
                         (if (< codepage 4096)
                             90
                           (let ((slots (syscall-direct 9 0 4096 3 34 -1 0)))
                             (if (< slots 4096)
                                 91
                               (let ((trampage (syscall-direct 9 0 4096 7 34 -1 0)))
                                 (if (< trampage 4096)
                                     92
                                   ,native-exec-seq))))))))
                   (sexp
                    `(seq
                      ,zero-slot-def
                      ,write-int-slot-def
                      ,nl-alloc-symbol-def
                      ,builtin-call1-def
                      ,read-int-slot-def
                      ,native-exec-def
                      (exit (native-exec 41)))))
              (nelisp-aot-compiler-test--with-tmp-binary path
                  "neln-inproc-call1"
                (nelisp-aot-compile-sexp sexp path)
                (should (file-executable-p path))
                (let ((r (nelisp-aot-compiler-test--run-binary path)))
                  (should (= (plist-get r :exit) 42))))))
      (delete-directory dir t)))))

(ert-deftest nelisp-aot-compiler/e2e-call-ptr-zero-arg ()
  "Doc 133 Phase 0: indirect call with no args.
`via' calls a zero-arg `answer' through its address. answer() = 42."
  (unless (nelisp-aot-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-aot-compiler-test--with-tmp-binary path "callptr0"
    (nelisp-aot-compile-sexp
     '(seq (defun answer () 42)
           (defun via (p) (call-ptr p))
           (exit (via (addr-of answer))))
     path)
    (let ((r (nelisp-aot-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 42)))))

(ert-deftest nelisp-aot-compiler/e2e-defun-sub-two ()
  "`(seq (defun sub (a b) (- a b)) (exit (sub 10 3)))' exits 7."
  (unless (nelisp-aot-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-aot-compiler-test--with-tmp-binary path "sub2"
    (nelisp-aot-compile-sexp
     '(seq (defun sub (a b) (- a b)) (exit (sub 10 3))) path)
    (let ((r (nelisp-aot-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 7)))))

(ert-deftest nelisp-aot-compiler/e2e-defun-mul-two ()
  "`(seq (defun mul (a b) (* a b)) (exit (mul 6 7)))' exits 42."
  (unless (nelisp-aot-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-aot-compiler-test--with-tmp-binary path "mul2"
    (nelisp-aot-compile-sexp
     '(seq (defun mul (a b) (* a b)) (exit (mul 6 7))) path)
    (let ((r (nelisp-aot-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 42)))))
(ert-deftest nelisp-aot-compiler/e2e-defun-div-two ()
  "`(seq (defun dv (a b) (/ a b)) (exit (dv 84 2)))' exits 42.
Doc 133: integer division (/) emits as a value expression (idiv, keeping
the quotient in rax — the companion of the `mod' remainder path)."
  (unless (nelisp-aot-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-aot-compiler-test--with-tmp-binary path "div2"
    (nelisp-aot-compile-sexp
     '(seq (defun dv (a b) (/ a b)) (exit (dv 84 2))) path)
    (let ((r (nelisp-aot-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 42)))))


(ert-deftest nelisp-aot-compiler/e2e-defun-six-arg ()
  "6-arg call exercises all SysV AMD64 arg registers — sum = 21."
  (unless (nelisp-aot-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-aot-compiler-test--with-tmp-binary path "sum6"
    (nelisp-aot-compile-sexp
     '(seq (defun sum6 (a b c d e f)
             (+ (+ (+ a b) (+ c d)) (+ e f)))
           (exit (sum6 1 2 3 4 5 6)))
     path)
    (let ((r (nelisp-aot-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 21)))))

(ert-deftest nelisp-aot-compiler/e2e-defun-nested-call ()
  "`f` called twice via `g` returns 7 (= ((5+1)+1))."
  (unless (nelisp-aot-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-aot-compiler-test--with-tmp-binary path "nested"
    (nelisp-aot-compile-sexp
     '(seq (defun f (x) (+ x 1))
           (defun g (x) (f (f x)))
           (exit (g 5)))
     path)
    (let ((r (nelisp-aot-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 7)))))

(ert-deftest nelisp-aot-compiler/e2e-defun-double ()
  "The Doc 97.b §6 smoke: `(double 21)' returns 42 via `(* x 2)'."
  (unless (nelisp-aot-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-aot-compiler-test--with-tmp-binary path "double"
    (nelisp-aot-compile-sexp
     '(seq (defun double (x) (* x 2)) (exit (double 21))) path)
    (let ((r (nelisp-aot-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 42)))))

(ert-deftest nelisp-aot-compiler/e2e-defun-arith-runtime ()
  "Compose runtime arithmetic with calls: `(add (mul 3 4) 5)' = 17."
  (unless (nelisp-aot-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-aot-compiler-test--with-tmp-binary path "compose"
    (nelisp-aot-compile-sexp
     '(seq (defun mul (a b) (* a b))
           (defun add (a b) (+ a b))
           (exit (add (mul 3 4) 5)))
     path)
    (let ((r (nelisp-aot-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 17)))))

(ert-deftest nelisp-aot-compiler/e2e-defun-mod-runtime ()
  "Compile runtime `(mod n 4)' and observe the remainder."
  (unless (nelisp-aot-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-aot-compiler-test--with-tmp-binary path "mod-runtime"
    (nelisp-aot-compile-sexp
     '(seq (defun rem4 (n) (mod n 4))
           (exit (rem4 11)))
     path)
    (let ((r (nelisp-aot-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 3)))))

(ert-deftest nelisp-aot-compiler/e2e-defun-call-chain ()
  "Three-deep call chain: a→b→c, each adds 1, returns 13 from 10."
  (unless (nelisp-aot-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-aot-compiler-test--with-tmp-binary path "chain"
    (nelisp-aot-compile-sexp
     '(seq (defun c (x) (+ x 1))
           (defun b (x) (c (+ x 1)))
           (defun a (x) (b (+ x 1)))
           (exit (a 10)))
     path)
    (let ((r (nelisp-aot-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 13)))))
;; ---- Doc 100 §100.D bitwise + shift grammar ----

(ert-deftest nelisp-aot-compiler/parse-bitwise-ops ()
  "Each of `logior logand logxor' parses to a `:kind arith' node."
  (dolist (op '(logior logand logxor))
    (let* ((ir (nelisp-aot-compiler--parse
                (list 'defun 'f '(a b) (list op 'a 'b))))
           (body (nelisp-aot-compiler--ir-get ir :body)))
      (should (eq (nelisp-aot-compiler--ir-kind body) 'arith))
      (should (eq (nelisp-aot-compiler--ir-get body :op) op)))))

(ert-deftest nelisp-aot-compiler/parse-shift-ops ()
  "`(shl N C)' / `(sar N C)' / `(shr N C)' parse to a `:kind shift' node."
  (dolist (op '(shl sar shr))
    (let* ((ir (nelisp-aot-compiler--parse
                (list 'defun 'f '(n c) (list op 'n 'c))))
           (body (nelisp-aot-compiler--ir-get ir :body)))
      (should (eq (nelisp-aot-compiler--ir-kind body) 'shift))
      (should (eq (nelisp-aot-compiler--ir-get body :op) op)))))

(ert-deftest nelisp-aot-compiler/parse-shift-arity ()
  "Shift with wrong arg count signals."
  (should-error
   (nelisp-aot-compiler--parse '(exit (shl 1)))
   :type 'nelisp-aot-compiler-error))

(ert-deftest nelisp-aot-compiler/e2e-defun-logior ()
  "Doc 100 §100.D — `(logior 12 3)' = 15 via or-reg-reg inline emit."
  (unless (nelisp-aot-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-aot-compiler-test--with-tmp-binary path "ior"
    (nelisp-aot-compile-sexp
     '(seq (defun ior (a b) (logior a b)) (exit (ior 12 3))) path)
    (let ((r (nelisp-aot-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 15)))))

(ert-deftest nelisp-aot-compiler/e2e-defun-logand ()
  "Doc 100 §100.D — `(logand 14 7)' = 6 via and-reg-reg inline emit."
  (unless (nelisp-aot-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-aot-compiler-test--with-tmp-binary path "iand"
    (nelisp-aot-compile-sexp
     '(seq (defun iand (a b) (logand a b)) (exit (iand 14 7))) path)
    (let ((r (nelisp-aot-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 6)))))

(ert-deftest nelisp-aot-compiler/e2e-defun-logxor ()
  "Doc 100 §100.D — `(logxor 12 10)' = 6 via xor-reg-reg inline emit."
  (unless (nelisp-aot-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-aot-compiler-test--with-tmp-binary path "ixor"
    (nelisp-aot-compile-sexp
     '(seq (defun ixor (a b) (logxor a b)) (exit (ixor 12 10))) path)
    (let ((r (nelisp-aot-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 6)))))

(ert-deftest nelisp-aot-compiler/e2e-defun-shl ()
  "Doc 100 §100.D — `(shl 1 3)' = 8 via shl rax, cl after mov rcx, r10."
  (unless (nelisp-aot-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-aot-compiler-test--with-tmp-binary path "shleft"
    (nelisp-aot-compile-sexp
     '(seq (defun shleft (n c) (shl n c)) (exit (shleft 1 3))) path)
    (let ((r (nelisp-aot-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 8)))))

(ert-deftest nelisp-aot-compiler/e2e-defun-sar ()
  "Doc 100 §100.D — `(sar 256 4)' = 16 via sar rax, cl after mov rcx, r10."
  (unless (nelisp-aot-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-aot-compiler-test--with-tmp-binary path "shright"
    (nelisp-aot-compile-sexp
     '(seq (defun shright (n c) (sar n c)) (exit (shright 256 4))) path)
    (let ((r (nelisp-aot-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 16)))))

(ert-deftest nelisp-aot-compiler/e2e-defun-shr ()
  "Logical-right shift `(shr (1<<63) 60)' = 8 (zero-fill), whereas the same
operands under `sar' would sign-extend to -8.  `shr' via shr rax, cl."
  (unless (nelisp-aot-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-aot-compiler-test--with-tmp-binary path "lshr"
    (nelisp-aot-compile-sexp
     '(seq (defun lshr (n c) (shr n c)) (exit (lshr (shl 1 63) 60))) path)
    (let ((r (nelisp-aot-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 8)))))

(ert-deftest nelisp-aot-compiler/e2e-imm32-mask-bit31 ()
  "A 32-bit immediate with bit 31 set (mask 0xFFFFFFFF = 4294967295) must
load zero-extended, not via the sign-extending `mov rax, imm32': otherwise
`(logand X 0xFFFFFFFF)' becomes `(logand X -1)' and fails to clear the high
32 bits.  Here `(logand (5<<32) 0xFFFFFFFF)' must be 0 (so result 100), not
the high word 5 (result 105).  Regression for the imm32 sign-extension bug."
  (unless (nelisp-aot-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-aot-compiler-test--with-tmp-binary path "imm32mask"
    (nelisp-aot-compile-sexp
     '(seq (defun f (x) (+ 100 (shr (logand x 4294967295) 32)))
           (exit (f (shl 5 32)))) path)
    (let ((r (nelisp-aot-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 100)))))


(ert-deftest nelisp-aot-compiler/e2e-defun-write-and-exit ()
  "Function with side-effect `write' + computed exit code."
  (unless (nelisp-aot-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-aot-compiler-test--with-tmp-binary path "fn-write"
    (nelisp-aot-compile-sexp
     '(seq (defun two () 2)
           (write "fn\n")
           (exit (two)))
     path)
    (let ((r (nelisp-aot-compiler-test--run-binary path)))
      (should (equal (plist-get r :stdout) "fn\n"))
      (should (= (plist-get r :exit) 2)))))

;; ---- §T.6 §97.c parser tests — control flow + comparisons ----

(ert-deftest nelisp-aot-compiler/parse-if-imm-branches ()
  "Parse `(if 1 7 9)' to an if IR node with imm THEN/ELSE."
  (let ((ir (nelisp-aot-compiler--parse '(exit (if 1 7 9)))))
    (let* ((vnode (nelisp-aot-compiler--ir-get ir :value)))
      (should (eq (nelisp-aot-compiler--ir-kind vnode) 'if))
      (should (eq (nelisp-aot-compiler--ir-kind
                   (nelisp-aot-compiler--ir-get vnode :test))
                  'imm))
      (should (= (nelisp-aot-compiler--ir-get
                  (nelisp-aot-compiler--ir-get vnode :then) :value)
                 7))
      (should (= (nelisp-aot-compiler--ir-get
                  (nelisp-aot-compiler--ir-get vnode :else) :value)
                 9)))))

(ert-deftest nelisp-aot-compiler/parse-cmp-ops ()
  "Each of `< > <= >= =' parses to a `:kind cmp' node with right op."
  (dolist (op '(< > <= >= =))
    (let* ((ir (nelisp-aot-compiler--parse
                (list 'exit (list op 3 5))))
           (vnode (nelisp-aot-compiler--ir-get ir :value)))
      (should (eq (nelisp-aot-compiler--ir-kind vnode) 'cmp))
      (should (eq (nelisp-aot-compiler--ir-get vnode :op) op)))))

(ert-deftest nelisp-aot-compiler/parse-cmp-arity ()
  "Comparison with wrong arg count signals."
  (should-error
   (nelisp-aot-compiler--parse '(exit (< 1)))
   :type 'nelisp-aot-compiler-error))

(ert-deftest nelisp-aot-compiler/parse-while-body-list ()
  "`(while T B1 B2 B3)' captures all three body forms."
  (let* ((ir (nelisp-aot-compiler--parse
              '(seq (while 1 0 0 0) (exit 0))))
         (forms (nelisp-aot-compiler--ir-get ir :forms))
         (while-node (car forms)))
    (should (eq (nelisp-aot-compiler--ir-kind while-node) 'while))
    (should (= (length (nelisp-aot-compiler--ir-get while-node :body)) 3))))

(ert-deftest nelisp-aot-compiler/parse-cond-clauses ()
  "`(cond (P1 B1) (t B2))' parses with `always' sentinel."
  (let* ((ir (nelisp-aot-compiler--parse
              '(exit (cond ((= 1 2) 9) (t 5)))))
         (vnode (nelisp-aot-compiler--ir-get ir :value))
         (clauses (nelisp-aot-compiler--ir-get vnode :clauses)))
    (should (eq (nelisp-aot-compiler--ir-kind vnode) 'cond))
    (should (= (length clauses) 2))
    (should (eq (nelisp-aot-compiler--ir-kind (car (car clauses))) 'cmp))
    (should (eq (car (cadr clauses)) 'always))))

(ert-deftest nelisp-aot-compiler/parse-cond-empty-errors ()
  "`(cond)' with no clauses folds to nil/0."
  (let* ((ir (nelisp-aot-compiler--parse '(exit (cond))))
         (vnode (nelisp-aot-compiler--ir-get ir :value)))
    (should (eq (nelisp-aot-compiler--ir-kind vnode) 'imm))
    (should (= (nelisp-aot-compiler--ir-get vnode :value) 0))))

(ert-deftest nelisp-aot-compiler/parse-logic-and-or ()
  "`(and 1 2)' and `(or 0 5)' both parse to `:kind logic' nodes."
  (let* ((ir1 (nelisp-aot-compiler--parse '(exit (and 1 2))))
         (ir2 (nelisp-aot-compiler--parse '(exit (or 0 5)))))
    (should (eq (nelisp-aot-compiler--ir-kind
                 (nelisp-aot-compiler--ir-get ir1 :value))
                'logic))
    (should (eq (nelisp-aot-compiler--ir-get
                 (nelisp-aot-compiler--ir-get ir1 :value) :op)
                'and))
    (should (eq (nelisp-aot-compiler--ir-get
                 (nelisp-aot-compiler--ir-get ir2 :value) :op)
                'or))))

(ert-deftest nelisp-aot-compiler/parse-logic-empty-errors ()
  "`(and)' / `(or)' with no operands fold to their Elisp identities."
  (let* ((and-ir (nelisp-aot-compiler--parse '(exit (and))))
         (or-ir (nelisp-aot-compiler--parse '(exit (or))))
         (and-v (nelisp-aot-compiler--ir-get and-ir :value))
         (or-v (nelisp-aot-compiler--ir-get or-ir :value)))
    (should (eq (nelisp-aot-compiler--ir-kind and-v) 'imm))
    (should (= (nelisp-aot-compiler--ir-get and-v :value) 1))
    (should (eq (nelisp-aot-compiler--ir-kind or-v) 'imm))
    (should (= (nelisp-aot-compiler--ir-get or-v :value) 0))))

(ert-deftest nelisp-aot-compiler/parse-control-flow-stable-ids ()
  "Two compiles in a row reset the label counter (= deterministic)."
  (let ((nelisp-aot-compiler--label-counter 0))
    (let* ((ir1 (nelisp-aot-compiler--parse '(exit (if 1 7 9)))))
      (should (eq (nelisp-aot-compiler--ir-get
                   (nelisp-aot-compiler--ir-get ir1 :value) :id)
                  'if-1))))
  (let ((nelisp-aot-compiler--label-counter 0))
    (let* ((ir2 (nelisp-aot-compiler--parse '(exit (if 1 7 9)))))
      (should (eq (nelisp-aot-compiler--ir-get
                   (nelisp-aot-compiler--ir-get ir2 :value) :id)
                  'if-1)))))

;; ---- §T.7 §97.c emit pass-1/pass-2 invariance ----

(ert-deftest nelisp-aot-compiler/emit-if-pass-parity ()
  "if/else byte-length is identical across pass-1 and pass-2."
  (let* ((nelisp-aot-compiler--label-counter 0)
         (ir (nelisp-aot-compiler--parse '(exit (if 1 7 9))))
         (collected (nelisp-aot-compiler--collect-strings ir))
         (pass1 (nelisp-aot-compiler--pass ir nil (car collected) 0))
         (size1 (nelisp-asm-x86_64-buffer-pos pass1)))
    (setq nelisp-aot-compiler--label-counter 0)
    (let* ((ir2 (nelisp-aot-compiler--parse '(exit (if 1 7 9))))
           (pass2 (nelisp-aot-compiler--pass
                   ir2 nil (car collected) #x401000))
           (size2 (nelisp-asm-x86_64-buffer-pos pass2)))
      (should (= size1 size2)))))

(ert-deftest nelisp-aot-compiler/emit-while-pass-parity ()
  "while loop emits the same byte-count across both passes."
  (let* ((nelisp-aot-compiler--label-counter 0)
         (ir (nelisp-aot-compiler--parse
              '(seq (while 0 0) (exit 0))))
         (collected (nelisp-aot-compiler--collect-strings ir))
         (pass1 (nelisp-aot-compiler--pass ir nil (car collected) 0))
         (size1 (nelisp-asm-x86_64-buffer-pos pass1)))
    (setq nelisp-aot-compiler--label-counter 0)
    (let* ((ir2 (nelisp-aot-compiler--parse
                 '(seq (while 0 0) (exit 0))))
           (pass2 (nelisp-aot-compiler--pass
                   ir2 nil (car collected) #x401000))
           (size2 (nelisp-asm-x86_64-buffer-pos pass2)))
      (should (= size1 size2)))))

;; ---- §T.8 §97.c e2e smoke (= the production gate) ----

(ert-deftest nelisp-aot-compiler/e2e-if-then ()
  "`(exit (if 1 7 0))' exits 7."
  (unless (nelisp-aot-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-aot-compiler-test--with-tmp-binary path "if-then"
    (nelisp-aot-compile-sexp '(seq (exit (if 1 7 0))) path)
    (let ((r (nelisp-aot-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 7)))))

(ert-deftest nelisp-aot-compiler/e2e-if-else ()
  "`(exit (if 0 7 99))' exits 99."
  (unless (nelisp-aot-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-aot-compiler-test--with-tmp-binary path "if-else"
    (nelisp-aot-compile-sexp '(seq (exit (if 0 7 99))) path)
    (let ((r (nelisp-aot-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 99)))))

(ert-deftest nelisp-aot-compiler/e2e-cmp-lt ()
  "`(exit (if (< 3 5) 1 2))' exits 1."
  (unless (nelisp-aot-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-aot-compiler-test--with-tmp-binary path "cmp-lt"
    (nelisp-aot-compile-sexp '(seq (exit (if (< 3 5) 1 2))) path)
    (let ((r (nelisp-aot-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 1)))))

(ert-deftest nelisp-aot-compiler/e2e-cmp-gt-false ()
  "`(exit (if (> 3 5) 1 2))' exits 2."
  (unless (nelisp-aot-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-aot-compiler-test--with-tmp-binary path "cmp-gt"
    (nelisp-aot-compile-sexp '(seq (exit (if (> 3 5) 1 2))) path)
    (let ((r (nelisp-aot-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 2)))))

(ert-deftest nelisp-aot-compiler/e2e-cmp-eq ()
  "`(exit (if (= 4 4) 11 22))' exits 11."
  (unless (nelisp-aot-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-aot-compiler-test--with-tmp-binary path "cmp-eq"
    (nelisp-aot-compile-sexp '(seq (exit (if (= 4 4) 11 22))) path)
    (let ((r (nelisp-aot-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 11)))))

(ert-deftest nelisp-aot-compiler/e2e-and-all-truthy ()
  "`(exit (and 1 2 3))' exits 3 (= last operand)."
  (unless (nelisp-aot-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-aot-compiler-test--with-tmp-binary path "and"
    (nelisp-aot-compile-sexp '(seq (exit (and 1 2 3))) path)
    (let ((r (nelisp-aot-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 3)))))

(ert-deftest nelisp-aot-compiler/e2e-and-short-circuit ()
  "`(exit (and 1 0 3))' exits 0 (= short-circuit at 2nd term)."
  (unless (nelisp-aot-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-aot-compiler-test--with-tmp-binary path "and-sc"
    (nelisp-aot-compile-sexp '(seq (exit (and 1 0 3))) path)
    (let ((r (nelisp-aot-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 0)))))

(ert-deftest nelisp-aot-compiler/e2e-or-first-truthy ()
  "`(exit (or 0 5 7))' exits 5 (= first non-zero short-circuits)."
  (unless (nelisp-aot-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-aot-compiler-test--with-tmp-binary path "or"
    (nelisp-aot-compile-sexp '(seq (exit (or 0 5 7))) path)
    (let ((r (nelisp-aot-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 5)))))

(ert-deftest nelisp-aot-compiler/e2e-cond-second-match ()
  "`(cond ((= 1 2) 9) ((= 3 3) 7) (t 5))' exits 7."
  (unless (nelisp-aot-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-aot-compiler-test--with-tmp-binary path "cond"
    (nelisp-aot-compile-sexp
     '(seq (exit (cond ((= 1 2) 9) ((= 3 3) 7) (t 5)))) path)
    (let ((r (nelisp-aot-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 7)))))

(ert-deftest nelisp-aot-compiler/e2e-cond-fallthrough ()
  "`(cond ((= 1 2) 9) (t 5))' takes the t clause and exits 5."
  (unless (nelisp-aot-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-aot-compiler-test--with-tmp-binary path "cond-t"
    (nelisp-aot-compile-sexp
     '(seq (exit (cond ((= 1 2) 9) (t 5)))) path)
    (let ((r (nelisp-aot-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 5)))))

(ert-deftest nelisp-aot-compiler/e2e-factorial-recursive ()
  "Recursive factorial (= the Doc 97.c §6 production gate)."
  (unless (nelisp-aot-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-aot-compiler-test--with-tmp-binary path "fact"
    (nelisp-aot-compile-sexp
     '(seq (defun fact (n)
             (if (= n 0) 1 (* n (fact (- n 1)))))
           (exit (fact 5)))
     path)
    (let ((r (nelisp-aot-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 120)))))

(ert-deftest nelisp-aot-compiler/e2e-nested-if ()
  "Nested ifs compute (if (< 2 5) (if (= 7 7) 42 1) 0) = 42."
  (unless (nelisp-aot-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-aot-compiler-test--with-tmp-binary path "nested-if"
    (nelisp-aot-compile-sexp
     '(seq (exit (if (< 2 5) (if (= 7 7) 42 1) 0))) path)
    (let ((r (nelisp-aot-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 42)))))

(ert-deftest nelisp-aot-compiler/e2e-if-with-call ()
  "`if' inside a defun composes with call returns: max(3,7) = 7."
  (unless (nelisp-aot-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-aot-compiler-test--with-tmp-binary path "max"
    (nelisp-aot-compile-sexp
     '(seq (defun maxf (a b) (if (> a b) a b))
           (exit (maxf 3 7)))
     path)
    (let ((r (nelisp-aot-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 7)))))

(ert-deftest nelisp-aot-compiler/e2e-cmp-le-ge ()
  "`<=' and `>=' boundaries: `(if (<= 5 5) 1 0)' = 1, etc."
  (unless (nelisp-aot-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-aot-compiler-test--with-tmp-binary path "le-eq"
    (nelisp-aot-compile-sexp
     '(seq (exit (if (<= 5 5) (if (>= 5 5) 42 0) 0))) path)
    (let ((r (nelisp-aot-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 42)))))

;; ====================================================================
;; Doc 99 §99.B — `nelisp-aot-compile-to-object' (ET_REL emit)
;; ====================================================================

(defun nelisp-aot-compiler-test--read-bytes (path)
  "Return raw unibyte bytes of PATH."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((coding-system-for-read 'no-conversion))
      (insert-file-contents-literally path))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun nelisp-aot-compiler-test--read-le16 (bytes offset)
  "Read unsigned 16-bit little-endian integer from BYTES at OFFSET."
  (logior (aref bytes offset)
          (ash (aref bytes (+ offset 1)) 8)))

(defun nelisp-aot-compiler-test--read-le32 (bytes offset)
  "Read unsigned 32-bit little-endian integer from BYTES at OFFSET."
  (logior (aref bytes offset)
          (ash (aref bytes (+ offset 1)) 8)
          (ash (aref bytes (+ offset 2)) 16)
          (ash (aref bytes (+ offset 3)) 24)))

(defun nelisp-aot-compiler-test--coff-section-bytes (bytes name)
  "Return raw section bytes for COFF section NAME."
  (let* ((section-count
          (nelisp-aot-compiler-test--read-le16 bytes 2))
         (optional-size
          (nelisp-aot-compiler-test--read-le16 bytes 16))
         (section-base (+ 20 optional-size))
         (found nil)
         (idx 0))
    (while (and (< idx section-count) (null found))
      (let* ((base (+ section-base (* idx 40)))
             (raw-name (substring bytes base (+ base 8)))
             (nul-pos (cl-position 0 raw-name))
             (section-name (if nul-pos
                               (substring raw-name 0 nul-pos)
                             raw-name)))
        (when (equal section-name name)
          (let ((raw-size
                 (nelisp-aot-compiler-test--read-le32 bytes (+ base 16)))
                (raw-ptr
                 (nelisp-aot-compiler-test--read-le32 bytes (+ base 20))))
            (setq found (substring bytes raw-ptr (+ raw-ptr raw-size))))))
      (setq idx (1+ idx)))
    (or found
        (error "COFF section %s not found" name))))

(defun nelisp-aot-compiler-test--bytes-contain-p (bytes needle)
  "Return non-nil when BYTES contains NEEDLE."
  (not (null (cl-search needle bytes :test #'eql))))

(defun nelisp-aot-compiler-test--coff-text-for (form)
  "Compile FORM as x86_64 COFF and return its `.text' section bytes."
  (let ((path (make-temp-file "nelisp-win64-text-" nil ".obj")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-to-object
           form path :arch 'x86_64 :format 'coff)
          (nelisp-aot-compiler-test--coff-section-bytes
           (nelisp-aot-compiler-test--read-bytes path)
           ".text"))
      (ignore-errors (delete-file path)))))

(defun nelisp-aot-compiler-test--elf-section-header (bytes index)
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

(defun nelisp-aot-compiler-test--elf-cstring (bytes start)
  "Read a NUL-terminated string from BYTES at START."
  (let ((end start))
    (while (and (< end (length bytes))
                (not (zerop (aref bytes end))))
      (setq end (1+ end)))
    (substring bytes start end)))

(defun nelisp-aot-compiler-test--elf-find-section (bytes name)
  "Return plist for section NAME in ELF BYTES, or nil."
  (let* ((shnum (nelisp-elf--read-le16 bytes 60))
         (shstrndx (nelisp-elf--read-le16 bytes 62))
         (shstr (nelisp-aot-compiler-test--elf-section-header bytes
                                                                  shstrndx))
         (shstr-off (plist-get shstr :offset))
         (found nil)
         (i 0))
    (while (and (< i shnum) (null found))
      (let* ((shdr (nelisp-aot-compiler-test--elf-section-header bytes i))
             (sec-name
              (nelisp-aot-compiler-test--elf-cstring
               bytes (+ shstr-off (plist-get shdr :name-off)))))
        (when (equal sec-name name)
          (setq found shdr)))
      (setq i (1+ i)))
    found))

(defun nelisp-aot-compiler-test--elf-symbols (bytes)
  "Return the emitted ELF symbol table from BYTES as plists."
  (let* ((symtab (or (nelisp-aot-compiler-test--elf-find-section
                      bytes ".symtab")
                     (error ".symtab not found")))
         (strtab-index (plist-get symtab :link))
         (strtab (nelisp-aot-compiler-test--elf-section-header
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
             (name (nelisp-aot-compiler-test--elf-cstring
                    bytes (+ str-off name-off))))
        (push (list :name name
                    :value (nelisp-elf--read-le64 bytes (+ base 8))
                    :size (nelisp-elf--read-le64 bytes (+ base 16))
                    :shndx (nelisp-elf--read-le16 bytes (+ base 6)))
              acc))
      (setq i (1+ i)))
    (nreverse acc)))

(defun nelisp-aot-compiler-test--find-symbol (bytes name)
  "Return symbol plist for NAME in ELF BYTES."
  (or (cl-find-if (lambda (sym)
                    (equal (plist-get sym :name) name))
                  (nelisp-aot-compiler-test--elf-symbols bytes))
      (error "symbol %s not found" name)))

(defun nelisp-aot-compiler-test--assert-aarch64-object-symbol (src expected-name)
  "Compile SRC to an aarch64 ELF object and assert EXPECTED-NAME exists."
  (let ((path (make-temp-file "nelisp-aot-arm64-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-to-object src path :arch 'aarch64)
          (let* ((bytes (nelisp-aot-compiler-test--read-bytes path))
                 (sym (nelisp-aot-compiler-test--find-symbol
                       bytes expected-name)))
            (should (equal (substring bytes 0 4)
                           (unibyte-string #x7F #x45 #x4C #x46)))
            (should (= (nelisp-elf--read-le16 bytes 16) 1))
            (should (= (nelisp-elf--read-le16 bytes 18) 183))
            (should (zerop (nelisp-elf--read-le64 bytes 24)))
            (should (> (plist-get sym :size) 0))
            (should (zerop (plist-get sym :value)))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-aot-compiler/object-mode-smoke ()
  "Compile `(defun nelisp_spike_noop () 42)' to an ET_REL .o.
Verify ehdr: ET_REL, EM_X86_64, e_entry=0, e_phnum=0."
  (let ((path (make-temp-file "nelisp-doc99-obj-smoke-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-to-object
           '(defun nelisp_spike_noop () 42)
           path)
          (let ((bytes (nelisp-aot-compiler-test--read-bytes path)))
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

(ert-deftest nelisp-aot-compiler/object-mode-readelf-s ()
  "`readelf -s' lists `nelisp_spike_noop' as GLOBAL FUNC in the .o."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc99-obj-syms-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-to-object
           '(defun nelisp_spike_noop () 42)
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "-s" path)))))
            (should (string-match-p "nelisp_spike_noop" out))
            (should (string-match-p "FUNC" out))
            (should (string-match-p "GLOBAL" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-aot-compiler/object-mode-multiple-defuns ()
  "Multiple defuns inside a `seq' each become a GLOBAL FUNC symbol."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc99-obj-multi-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-to-object
           '(seq (defun answer () 42)
                 (defun negone () (- 0 1)))
           path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "-s" path)))))
            (should (string-match-p "answer" out))
            (should (string-match-p "negone" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-aot-compiler/object-mode-rejects-write ()
  "`(defun foo () (write \"hi\"))' rejected — spike disallows strings."
  (let ((path (make-temp-file "nelisp-doc99-obj-rej-w-" nil ".o")))
    (unwind-protect
        (should-error
         (nelisp-aot-compile-to-object
          '(defun foo () (write "hi"))
          path)
         :type 'nelisp-aot-compiler-error)
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-aot-compiler/object-mode-rejects-non-defun ()
  "`(exit 0)' is rejected — top form must be defun or seq-of-defuns."
  (let ((path (make-temp-file "nelisp-doc99-obj-rej-nd-" nil ".o")))
    (unwind-protect
        (should-error
         (nelisp-aot-compile-to-object '(exit 0) path)
         :type 'nelisp-aot-compiler-error)
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-aot-compiler/object-mode-rejects-mixed-seq ()
  "`seq' object mode accepts mixed top-level forms after Doc 129 coverage."
  (let ((path (make-temp-file "nelisp-doc99-obj-rej-mix-" nil ".o")))
    (unwind-protect
        (progn
          (should (equal
                   (nelisp-aot-compile-to-object
                    '(seq (defun foo () 42) (exit 0)) path)
                   path))
          (should (file-exists-p path))
          (should (> (file-attribute-size (file-attributes path)) 0)))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-aot-compiler/object-mode-aarch64-add2 ()
  "AArch64 ET_REL emit: arithmetic trampoline exports `nelisp_jit_add2'."
  (nelisp-aot-compiler-test--assert-aarch64-object-symbol
   nelisp-cc-jit-arith-add2--source
   "nelisp_jit_add2"))

(ert-deftest nelisp-aot-compiler/object-mode-aarch64-eq2 ()
  "AArch64 ET_REL emit: comparison trampoline exports `nelisp_jit_eq2'."
  (nelisp-aot-compiler-test--assert-aarch64-object-symbol
   nelisp-cc-jit-arith-eq2--source
   "nelisp_jit_eq2"))

(ert-deftest nelisp-aot-compiler/object-mode-aarch64-logior2 ()
  "AArch64 ET_REL emit: bitwise trampoline exports `nelisp_jit_logior2'."
  (nelisp-aot-compiler-test--assert-aarch64-object-symbol
   nelisp-cc-jit-arith-logior2--source
   "nelisp_jit_logior2"))

(ert-deftest nelisp-aot-compiler/object-mode-aarch64-ash ()
  "AArch64 ET_REL emit: if+shift trampoline exports `nelisp_jit_ash'."
  (nelisp-aot-compiler-test--assert-aarch64-object-symbol
   nelisp-cc-jit-arith-ash--source
   "nelisp_jit_ash"))

(ert-deftest nelisp-aot-compiler/object-mode-aarch64-all-jit-arith-trampolines ()
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
          (nelisp-aot-compile-to-object
           (cons 'seq sources) path :arch 'aarch64)
          (let* ((bytes (nelisp-aot-compiler-test--read-bytes path))
                 (symbols (nelisp-aot-compiler-test--elf-symbols bytes)))
            (should (= (nelisp-elf--read-le16 bytes 18) 183))
            (dolist (name names)
              (let ((sym (cl-find-if (lambda (entry)
                                       (equal (plist-get entry :name) name))
                                     symbols)))
                (should sym)
                (should (> (plist-get sym :size) 0))
                (should (<= 0 (plist-get sym :value)))))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-aot-compiler/extern-call-rejects-missing-symbol ()
  "Doc 100 §100.A: `(extern-call)' without a symbol → parse error."
  (let ((path (make-temp-file "nelisp-doc100-extern-bad-" nil ".o")))
    (unwind-protect
        (should-error
         (nelisp-aot-compile-to-object
          '(defun probe () (extern-call))
          path)
         :type 'nelisp-aot-compiler-error)
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-aot-compiler/extern-call-rejects-non-symbol ()
  "Doc 100 §100.A: `(extern-call \"name\")' → parse error.
The extern symbol literal must be a bare symbol so its
`symbol-name' can become the linker-visible name without quoting."
  (let ((path (make-temp-file "nelisp-doc100-extern-non-sym-" nil ".o")))
    (unwind-protect
        (should-error
         (nelisp-aot-compile-to-object
          '(defun probe () (extern-call "ext_helper"))
          path)
         :type 'nelisp-aot-compiler-error)
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-aot-compiler/object-mode-extern-call-emits-plt32 ()
  "Doc 100 §100.A: compile-to-object emits SHN_UNDEF + PLT32 for extern-call.
End-to-end check that the compiler propagates an `(extern-call SYM)'
form through emit-extern-call → asm reloc → ELF writer so that
`readelf -r' shows R_X86_64_PLT32 against the symbol and
`readelf -s' shows the symbol as UND (= SHN_UNDEF)."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc100-extern-emit-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-to-object
           '(defun probe () (extern-call ext_helper))
           path)
          (let ((rs-out (with-output-to-string
                          (with-current-buffer standard-output
                            (call-process "readelf" nil t nil "-W" "-r" path))))
                (ss-out (with-output-to-string
                          (with-current-buffer standard-output
                            (call-process "readelf" nil t nil "-W" "-s" path)))))
            (should (string-match-p "R_X86_64_PLT32" rs-out))
            (should (string-match-p "ext_helper" rs-out))
            (should (string-match-p "UND[ \t]+ext_helper" ss-out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-aot-compiler/e2e-variadic-defun-forwards-va-list ()
  "Defined-varargs: a `defun' whose param list ends with `&c-varargs'
emits the SysV AMD64 variadic prologue (= a 176-byte register-save-area
below the param/let slots saving rdi..r9 + xmm0-7), and `va-list-init'
fills the 24-byte `__va_list_tag' (gp_offset / fp_offset /
overflow_arg_area / reg_save_area) so a forwarded `va_list' is consumed
correctly by a real `vsnprintf'.  Compiles to a `.o', links it with a C
driver via cc, runs, and compares output byte-for-byte against glibc.
Exercises the register path (3 varargs) and the overflow path (5
varargs, 2 spilled to the stack); the latter also guards the odd-arity
extern-call stack-alignment fix (= no double `sub rsp, 8')."
  (skip-unless (nelisp-aot-compiler-test--linux-p))
  (skip-unless (or (executable-find "cc") (executable-find "gcc")))
  (let* ((dir (make-temp-file "nl-va-e2e" t))
         (obj (expand-file-name "prog.o" dir))
         (cdrv (expand-file-name "drv.c" dir))
         (bin (expand-file-name "prog" dir)))
    (unwind-protect
        (progn
          (nelisp-aot-compile-to-object
           '(seq
             (defun myfmt (nlcf_buf nlcf_n nlcf_fmt &c-varargs)
               (let ((nlcf_ap (frame-alloc 24)))
                 (seq
                  (va-list-init nlcf_ap 3 0)
                  (extern-call vsnprintf nlcf_buf nlcf_n nlcf_fmt nlcf_ap)))))
           obj :arch 'x86_64 :format 'elf)
          (with-temp-file cdrv
            (insert "#include <stdio.h>\n#include <string.h>\n"
                    "extern int myfmt(char*, unsigned long, const char*, ...);\n"
                    "int main(void){\n"
                    "  char b[64], b2[64];\n"
                    "  int r  = myfmt(b,  sizeof b,  \"%d-%s-%d\", 7, \"ok\", 99);\n"
                    "  int r2 = myfmt(b2, sizeof b2, \"%d,%d,%d,%d,%d\", 1,2,3,4,5);\n"
                    "  int ok = (strcmp(b,\"7-ok-99\")==0)&&(r==7)\n"
                    "        && (strcmp(b2,\"1,2,3,4,5\")==0)&&(r2==9);\n"
                    "  printf(\"%s|%d|%s|%d|%s\\n\", b,r,b2,r2, ok?\"OK\":\"FAIL\");\n"
                    "  return ok?0:1;\n}\n"))
          (let ((cc (or (executable-find "cc") (executable-find "gcc"))))
            (should (zerop (call-process cc nil nil nil cdrv obj "-o" bin)))
            (let ((r (nelisp-aot-compiler-test--run-binary bin)))
              (should (= 0 (plist-get r :exit)))
              (should (equal "7-ok-99|7|1,2,3,4,5|9|OK"
                             (string-trim (plist-get r :stdout)))))))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest nelisp-aot-compiler/object-mode-data-addr-emits-pc32 ()
  "Doc 140 Stage 8: `(data-addr SYM)' emits a LEA + R_X86_64_PC32 reloc against
the EXTERNAL data symbol SYM (an UND symbol the static linker resolves to the
section VA).  This is the address-of-data-symbol primitive that lets the
chunked-arena allocator reach the driver-owned `nl_arena_base' bss slot at run
time without any fixed metadata immediate — the enabler for removing the fixed
arena base."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc140-data-addr-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-to-object
           '(defun probe () (data-addr nl_arena_base))
           path)
          (let ((rs-out (with-output-to-string
                          (with-current-buffer standard-output
                            (call-process "readelf" nil t nil "-r" path))))
                (ss-out (with-output-to-string
                          (with-current-buffer standard-output
                            (call-process "readelf" nil t nil "-s" path)))))
            (should (string-match-p "R_X86_64_PC32" rs-out))
            (should (string-match-p "nl_arena_base" rs-out))
            (should (string-match-p "UND[ \t]+nl_arena_base" ss-out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-aot-compiler/aarch64-data-addr-emits-adrp-add-relocs ()
  "Doc 140 Stage 8: arm64 `data-addr' lowers to ADRP+ADD reloc pair."
  (let ((nelisp-aot-compiler--arch 'aarch64)
        (buf (nelisp-asm-arm64-make-buffer))
        (node (nelisp-aot-compiler--make-ir
               'data-addr :name 'nl_ctrl_block)))
    (nelisp-aot-compiler--emit-data-addr node buf)
    (should (equal (nelisp-asm-arm64-buffer-bytes buf)
                   (concat
                    (unibyte-string #x00 #x00 #x00 #x90)
                    (unibyte-string #x00 #x00 #x00 #x91))))
    (should (equal (nelisp-asm-arm64-buffer-relocs buf)
                   (list
                    (list :type 'adr-prel-pg-hi21
                          :symbol "nl_ctrl_block"
                          :sym "nl_ctrl_block"
                          :offset 0
                          :addend 0
                          :section 'text)
                    (list :type 'add-abs-lo12-nc
                          :symbol "nl_ctrl_block"
                          :sym "nl_ctrl_block"
                          :offset 4
                          :addend 0
                          :section 'text))))))

(ert-deftest nelisp-aot-compiler/object-mode-aarch64-data-addr-emits-elf-relocs ()
  "AArch64 ET_REL output surfaces ADRP+ADD relocations for `data-addr'."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc140-arm64-data-addr-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-to-object
           '(defun probe () (data-addr nl_ctrl_block))
           path :arch 'aarch64)
          (let ((rs-out (with-output-to-string
                          (with-current-buffer standard-output
                            (call-process "readelf" nil t nil "-W" "-r" path))))
                (ss-out (with-output-to-string
                          (with-current-buffer standard-output
                            (call-process "readelf" nil t nil "-W" "-s" path)))))
            ;; This readelf build abbreviates long AArch64 reloc names in
            ;; the Type column, so match the stable type code + prefix.
            (should (string-match-p "000200000113 R_AARCH64_ADR_PRE" rs-out))
            (should (string-match-p "000200000115 R_AARCH64_ADD_ABS" rs-out))
            (should (string-match-p "nl_ctrl_block" rs-out))
            (should (string-match-p "UND[ \t]+nl_ctrl_block" ss-out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-aot-compiler/object-mode-extern-call-smoke ()
  "Doc 100 §100.A: (extern-call SYM) → .o + ld + exec end-to-end (= EXIT 99).
probe.o is generated by the AOT chain and contains a PLT32
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
          ;; Generate probe.o via AOT chain.
          (nelisp-aot-compile-to-object
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

(ert-deftest nelisp-aot-compiler/object-mode-extern-call-7gp-smoke ()
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
          (nelisp-aot-compile-to-object
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

(ert-deftest nelisp-aot-compiler/object-mode-extern-call-7gp-nontrivial-stack-smoke ()
  "Doc 129.7F: the first stack GP arg may be a computed value."
  (skip-unless (and (executable-find "ld")
                    (eq system-type 'gnu/linux)))
  (let* ((probe-path (make-temp-file "nelisp-doc129-probe7c-" nil ".o"))
         (host-path (make-temp-file "nelisp-doc129-host7c-" nil ".o"))
         (bin-path (make-temp-file "nelisp-doc129-bin7c-" nil ""))
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
         (ext21-off (length sum7-text))
         (ext21-text
          (concat
           (unibyte-string #xB8 21 0 0 0)
           (unibyte-string #xC3)))
         (start-off (+ (length sum7-text) (length ext21-text)))
         (start-text
          (concat
           (unibyte-string #xE8 0 0 0 0)
           (unibyte-string #x48 #x89 #xC7)
           (unibyte-string #xB8 #x3C 0 0 0)
           (unibyte-string #x0F #x05)))
         (host-text (concat sum7-text ext21-text start-text)))
    (unwind-protect
        (progn
          (nelisp-aot-compile-to-object
           '(defun probe () (extern-call ext_sum7 1 2 3 4 5 6 (extern-call ext_21)))
           probe-path)
          (nelisp-elf-write-binary
           host-path
           (list :e-type 'rel
                 :text host-text
                 :symbols (list
                           (list :name "ext_sum7" :value 0
                                 :size (length sum7-text)
                                 :section 'text :bind 'global :type 'func)
                           (list :name "ext_21" :value ext21-off
                                 :size (length ext21-text)
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

(ert-deftest nelisp-aot-compiler/object-mode-extern-call-8gp-nontrivial-stack-smoke ()
  "Doc 129.7G: multiple stack GP args may be computed values."
  (skip-unless (and (executable-find "ld")
                    (eq system-type 'gnu/linux)))
  (let* ((probe-path (make-temp-file "nelisp-doc129-probe8c-" nil ".o"))
         (host-path (make-temp-file "nelisp-doc129-host8c-" nil ".o"))
         (bin-path (make-temp-file "nelisp-doc129-bin8c-" nil ""))
         (sum8-text
          (concat
           ;; ext_sum8:
           ;;   rax = rdi+rsi+rdx+rcx+r8+r9+[rsp+8]+[rsp+16]; ret
           (unibyte-string #x48 #x89 #xF8)
           (unibyte-string #x48 #x01 #xF0)
           (unibyte-string #x48 #x01 #xD0)
           (unibyte-string #x48 #x01 #xC8)
           (unibyte-string #x4C #x01 #xC0)
           (unibyte-string #x4C #x01 #xC8)
           (unibyte-string #x48 #x03 #x44 #x24 #x08)
           (unibyte-string #x48 #x03 #x44 #x24 #x10)
           (unibyte-string #xC3)))
         (ext7-off (length sum8-text))
         (ext7-text
          (concat
           (unibyte-string #xB8 7 0 0 0)
           (unibyte-string #xC3)))
         (ext14-off (+ (length sum8-text) (length ext7-text)))
         (ext14-text
          (concat
           (unibyte-string #xB8 14 0 0 0)
           (unibyte-string #xC3)))
         (start-off (+ (length sum8-text)
                       (length ext7-text)
                       (length ext14-text)))
         (start-text
          (concat
           (unibyte-string #xE8 0 0 0 0)
           (unibyte-string #x48 #x89 #xC7)
           (unibyte-string #xB8 #x3C 0 0 0)
           (unibyte-string #x0F #x05)))
         (host-text (concat sum8-text ext7-text ext14-text start-text)))
    (unwind-protect
        (progn
          (nelisp-aot-compile-to-object
           '(defun probe () (extern-call ext_sum8 1 2 3 4 5 6
                                         (extern-call ext_7)
                                         (extern-call ext_14)))
           probe-path)
          (nelisp-elf-write-binary
           host-path
           (list :e-type 'rel
                 :text host-text
                 :symbols (list
                           (list :name "ext_sum8" :value 0
                                 :size (length sum8-text)
                                 :section 'text :bind 'global :type 'func)
                           (list :name "ext_7" :value ext7-off
                                 :size (length ext7-text)
                                 :section 'text :bind 'global :type 'func)
                           (list :name "ext_14" :value ext14-off
                                 :size (length ext14-text)
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

(ert-deftest nelisp-aot-compiler/object-mode-defun-7gp-param-smoke ()
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
          (nelisp-aot-compile-to-object
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

(ert-deftest nelisp-aot-compiler/sexp-tag-parse-shape ()
  "Doc 100 §100.B: (sexp-tag PTR) parses to (:kind sexp-tag :ptr REF)."
  (let* ((sexp '(defun probe (p) (sexp-tag p)))
         (program (nelisp-aot-compiler--parse (list 'seq sexp))))
    ;; Top-level program is a seq containing one defun.
    (should (eq (nelisp-aot-compiler--ir-kind program) 'seq))
    (let* ((forms (nelisp-aot-compiler--ir-get program :forms))
           (defun-node (car forms))
           (body (nelisp-aot-compiler--ir-get defun-node :body)))
      (should (eq (nelisp-aot-compiler--ir-kind body) 'sexp-tag))
      (should (eq (nelisp-aot-compiler--ir-kind
                   (nelisp-aot-compiler--ir-get body :ptr))
                  'ref)))))

(ert-deftest nelisp-aot-compiler/sexp-int-unwrap-parse-shape ()
  "Doc 100 §100.B: (sexp-int-unwrap PTR) parses to (:kind sexp-int-unwrap :ptr REF)."
  (let* ((sexp '(defun probe (p) (sexp-int-unwrap p)))
         (program (nelisp-aot-compiler--parse (list 'seq sexp))))
    (let* ((defun-node (car (nelisp-aot-compiler--ir-get program :forms)))
           (body (nelisp-aot-compiler--ir-get defun-node :body)))
      (should (eq (nelisp-aot-compiler--ir-kind body) 'sexp-int-unwrap))
      (should (eq (nelisp-aot-compiler--ir-kind
                   (nelisp-aot-compiler--ir-get body :ptr))
                  'ref)))))

(ert-deftest nelisp-aot-compiler/sexp-int-make-parse-shape ()
  "Doc 100 §100.B: (sexp-int-make SLOT N) parses to (:kind sexp-int-make :slot :val)."
  (let* ((sexp '(defun probe (slot n) (sexp-int-make slot n)))
         (program (nelisp-aot-compiler--parse (list 'seq sexp))))
    (let* ((defun-node (car (nelisp-aot-compiler--ir-get program :forms)))
           (body (nelisp-aot-compiler--ir-get defun-node :body)))
      (should (eq (nelisp-aot-compiler--ir-kind body) 'sexp-int-make))
      (should (eq (nelisp-aot-compiler--ir-kind
                   (nelisp-aot-compiler--ir-get body :slot))
                  'ref))
      (should (eq (nelisp-aot-compiler--ir-kind
                   (nelisp-aot-compiler--ir-get body :val))
                  'ref)))))

(ert-deftest nelisp-aot-compiler/sexp-abi-ops-reject-bad-arity ()
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
            (nelisp-aot-compiler--parse (list 'seq bad))
            (ert-fail (list :expected-error case)))
        (nelisp-aot-compiler-error
         (should (eq (car (cdr err)) expected-tag)))))))

(ert-deftest nelisp-aot-compiler/sexp-tag-emit-bytes ()
  "Doc 146 §3.0: (sexp-tag PTR) emits an immediate-aware tag read.
emit-sexp-tag now dispatches on the value word's low bit, so the
slot-path `mov rdi, rax' (48 89 C7) and `movzx rax, byte ptr [rdi]'
(48 0F B6 07) are both still emitted but are no longer adjacent (the
low-bit immediate dispatch sits between them).  Verify both byte groups
are present rather than the old contiguous 7-byte tail."
  (let* ((sexp '(seq (defun probe (p) (sexp-tag p))))
         (path (make-temp-file "nelisp-doc100-sexp-tag-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-to-object sexp path)
          (let* ((bytes (with-temp-buffer
                          (set-buffer-multibyte nil)
                          (insert-file-contents-literally path)
                          (buffer-string)))
                 ;; Doc 146 §3.0: immediate-aware emit keeps both the
                 ;; `mov rdi, rax' (48 89 C7) and the slot-path `movzx rax,
                 ;; byte [rdi]' (48 0F B6 07), just no longer adjacent.
                 (mov-rdi (unibyte-string #x48 #x89 #xC7))
                 (movzx   (unibyte-string #x48 #x0F #xB6 #x07)))
            (should (string-search mov-rdi bytes))
            (should (string-search movzx bytes))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-aot-compiler/sexp-int-unwrap-emit-bytes ()
  "Doc 146 §3.0: (sexp-int-unwrap PTR) emits an immediate-aware payload read.
The slot path still does `mov rdi,rax' (48 89 C7) + `mov rax,[rdi+8]'
(48 8B 47 08), but they are no longer adjacent (the low-bit immediate
dispatch + arithmetic-shift path sits between).  Verify both byte groups."
  (let* ((sexp '(seq (defun probe (p) (sexp-int-unwrap p))))
         (path (make-temp-file "nelisp-doc100-sexp-unwrap-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-to-object sexp path)
          (let* ((bytes (with-temp-buffer
                          (set-buffer-multibyte nil)
                          (insert-file-contents-literally path)
                          (buffer-string)))
                 (mov-rdi (unibyte-string #x48 #x89 #xC7))
                 (mov-pay (unibyte-string #x48 #x8B #x47 #x08)))
            (should (string-search mov-rdi bytes))
            (should (string-search mov-pay bytes))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-aot-compiler/sexp-int-make-emit-bytes ()
  "Doc 100 §100.B: (sexp-int-make SLOT N) emits the 3-instr writer sequence.
Expected substring (= the tail before `ret'):
  C6 07 02         mov byte [rdi], 2     (SEXP_TAG_INT)
  48 89 77 08      mov [rdi+8], rsi
  48 89 F8         mov rax, rdi"
  (let* ((sexp '(seq (defun probe (slot n) (sexp-int-make slot n))))
         (path (make-temp-file "nelisp-doc100-sexp-make-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-to-object sexp path)
          (let* ((bytes (with-temp-buffer
                          (set-buffer-multibyte nil)
                          (insert-file-contents-literally path)
                          (buffer-string)))
                 (needle (unibyte-string #xC6 #x07 #x02
                                         #x48 #x89 #x77 #x08
                                         #x48 #x89 #xF8)))
            (should (string-search needle bytes))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-aot-compiler/sexp-int-unwrap-e2e-exec ()
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
          (nelisp-aot-compile-to-object
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

(ert-deftest nelisp-aot-compiler/sexp-int-make-unwrap-e2e-exec ()
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
          (nelisp-aot-compile-to-object
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

(ert-deftest nelisp-aot-compiler/parse-symbol-name-eq-encodes-utf8-bytes ()
  "Literal string in `(symbol-name-eq P LIT)' is UTF-8 byte-encoded at parse."
  (let* ((ir (nelisp-aot-compiler--parse
              '(defun probe (p) (symbol-name-eq p "read"))))
         (body (nelisp-aot-compiler--ir-get ir :body)))
    (should (eq (nelisp-aot-compiler--ir-kind body) 'symbol-name-eq))
    (should (equal (nelisp-aot-compiler--ir-get body :bytes)
                   '(114 101 97 100)))))

(ert-deftest nelisp-aot-compiler/parse-symbol-name-eq-rejects-non-string ()
  "Second argument must be a string literal."
  (should-error
   (nelisp-aot-compiler--parse
    '(defun probe (p) (symbol-name-eq p 42)))
   :type 'nelisp-aot-compiler-error))

(ert-deftest nelisp-aot-compiler/parse-symbol-name-eq-arity-error ()
  "`(symbol-name-eq P)' (= 1-arg) raises arity error."
  (should-error
   (nelisp-aot-compiler--parse
    '(defun probe (p) (symbol-name-eq p)))
   :type 'nelisp-aot-compiler-error))

(ert-deftest nelisp-aot-compiler/parse-symbol-name-eq-empty-literal ()
  "Empty literal string is a legal zero-byte comparison (= length-check only)."
  (let* ((ir (nelisp-aot-compiler--parse
              '(defun probe (p) (symbol-name-eq p ""))))
         (body (nelisp-aot-compiler--ir-get ir :body)))
    (should (eq (nelisp-aot-compiler--ir-kind body) 'symbol-name-eq))
    (should (equal (nelisp-aot-compiler--ir-get body :bytes) '()))))

(ert-deftest nelisp-aot-compiler/emit-symbol-name-eq-produces-bytes ()
  "Emit phase for `symbol-name-eq' produces a non-empty byte string and
the size grows monotonically with literal length (= longer literals
inline more `cmp imm32' instructions)."
  (cl-labels ((emit-len (lit)
                (let* ((path (nelisp-aot-compiler-test--tmp-binary
                              "symname"))
                       (sexp `(defun probe (p)
                                (sexp-int-make
                                 p (symbol-name-eq p ,lit)))))
                  (unwind-protect
                      (progn
                        (nelisp-aot-compile-to-object sexp path)
                        (let ((sz (nth 7 (file-attributes path))))
                          sz))
                    (ignore-errors (delete-file path))))))
    (let ((short (emit-len "x"))
          (long  (emit-len "exit_group")))
      (should (integerp short))
      (should (integerp long))
      (should (> long short)))))

;; ---- §T.sexp-float-unwrap grammar (G4) ----

(ert-deftest nelisp-aot-compiler/parse-sexp-float-unwrap-shape ()
  "Parse `(sexp-float-unwrap PTR)' to an IR node with :kind sexp-float-unwrap."
  (let* ((ir (nelisp-aot-compiler--parse
              '(defun probe (p) (sexp-float-unwrap p))))
         (body (nelisp-aot-compiler--ir-get ir :body)))
    (should (eq (nelisp-aot-compiler--ir-kind body) 'sexp-float-unwrap))
    (let ((ptr (nelisp-aot-compiler--ir-get body :ptr)))
      (should (eq (nelisp-aot-compiler--ir-kind ptr) 'ref))
      (should (eq (nelisp-aot-compiler--ir-get ptr :var) 'p)))))

(ert-deftest nelisp-aot-compiler/parse-sexp-float-unwrap-arity-error ()
  "`(sexp-float-unwrap)' (= no arg) raises arity error."
  (should-error
   (nelisp-aot-compiler--parse
    '(defun probe (p) (sexp-float-unwrap)))
   :type 'nelisp-aot-compiler-error))

(ert-deftest nelisp-aot-compiler/emit-sexp-float-unwrap-produces-bytes ()
  "Emit phase for `sexp-float-unwrap' compiles to a non-empty .o file.

Historically this asserted byte-identity with `sexp-int-unwrap' (both
were a plain payload read at offset 8).  Since Doc 146 §3.0 step 7
\(commit 21108e63) `sexp-int-unwrap' is immediate-aware (low-bit
dispatch + `sar' path) and emits MORE code than the still-plain
float read, so the identity premise no longer holds.  Keep the
weaker invariants: both emit non-empty objects, and the
immediate-aware int unwrap is at least as large as the plain float
payload read."
  (let ((path-fl (nelisp-aot-compiler-test--tmp-binary "sfu"))
        (path-in (nelisp-aot-compiler-test--tmp-binary "siu")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-to-object
           '(defun probe (p) (sexp-float-unwrap p)) path-fl)
          (nelisp-aot-compile-to-object
           '(defun probe (p) (sexp-int-unwrap p)) path-in)
          (let ((sz-fl (nth 7 (file-attributes path-fl)))
                (sz-in (nth 7 (file-attributes path-in))))
            (should (integerp sz-fl))
            (should (integerp sz-in))
            (should (> sz-fl 0))
            (should (>= sz-in sz-fl))))
      (ignore-errors (delete-file path-fl))
      (ignore-errors (delete-file path-in)))))

;; ---- §T.let-rt Runtime let tests (Wave 18w+) ----

(ert-deftest nelisp-aot-compiler/parse-let-rt-call-value ()
  "Parse `(defun f (x) (let ((y (id x))) y))' — let with a call value.
The `id' call is not foldable so the `let' becomes a `let-rt' IR node
with a slot index beyond the param count."
  (let* ((ir (nelisp-aot-compiler--parse
              '(seq (defun id (x) x)
                    (defun f (x) (let ((y (id x))) y)))))
         ;; `seq' → second form is the `f' defun.
         (f-ir (nth 1 (nelisp-aot-compiler--ir-get ir :forms)))
         (body (nelisp-aot-compiler--ir-get f-ir :body)))
    (should (eq (nelisp-aot-compiler--ir-kind f-ir) 'defun))
    ;; body is a `let-rt' node (= non-foldable value).
    (should (eq (nelisp-aot-compiler--ir-kind body) 'let-rt))
    (should (eq (nelisp-aot-compiler--ir-get body :var) 'y))
    ;; Slot must be ≥ arity (= 1 for `f (x)').
    (should (>= (nelisp-aot-compiler--ir-get body :slot) 1))
    ;; value-ir is a `call' to `id'.
    (let ((val-ir (nelisp-aot-compiler--ir-get body :value-ir)))
      (should (eq (nelisp-aot-compiler--ir-kind val-ir) 'call))
      (should (eq (nelisp-aot-compiler--ir-get val-ir :name) 'id)))
    ;; body body is a `ref' for `y' via the rt slot.
    (let* ((body-ir (nelisp-aot-compiler--ir-get body :body))
           (slot (nelisp-aot-compiler--ir-get body :slot)))
      (should (eq (nelisp-aot-compiler--ir-kind body-ir) 'ref))
      (should (eq (nelisp-aot-compiler--ir-get body-ir :var) 'y))
      (should (= (nelisp-aot-compiler--ir-get body-ir :slot) slot)))))

(ert-deftest nelisp-aot-compiler/parse-let-rt-slot-beyond-params ()
  "Runtime let slot is param-count + rt-let-index."
  (let* ((ir (nelisp-aot-compiler--parse
              '(defun g (a b) (let ((t1 (+ a b))) t1))))
         (body (nelisp-aot-compiler--ir-get ir :body)))
    ;; `g' has 2 params (slots 0, 1); runtime let must use slot >= 2.
    (should (eq (nelisp-aot-compiler--ir-kind body) 'let-rt))
    (should (>= (nelisp-aot-compiler--ir-get body :slot) 2))))

(ert-deftest nelisp-aot-compiler/parse-let-rt-rt-slot-count ()
  "`defun' IR carries `:rt-slot-count' equal to number of runtime lets."
  (let* ((ir (nelisp-aot-compiler--parse
              '(seq (defun id (x) x)
                    (defun f (x) (let ((y (id x))) y))))))
    (let ((f-ir (nth 1 (nelisp-aot-compiler--ir-get ir :forms))))
      (should (= (nelisp-aot-compiler--ir-get f-ir :rt-slot-count) 1)))))

(ert-deftest nelisp-aot-compiler/parse-let-ct-in-defun-body-no-let-rt ()
  "Compile-time `let' inside defun body does NOT produce `let-rt'."
  (let* ((ir (nelisp-aot-compiler--parse
              '(defun f (x) (let ((k 7)) (+ x k)))))
         (body (nelisp-aot-compiler--ir-get ir :body)))
    ;; Compile-time fold: body is `arith', no `let-rt'.
    (should (eq (nelisp-aot-compiler--ir-kind body) 'arith))
    (should (= (nelisp-aot-compiler--ir-get ir :rt-slot-count) 0))))

(ert-deftest nelisp-aot-compiler/parse-let-bare-symbol-binding ()
  "Parse Emacs Lisp `(let (x) ...)' as a nil/zero initialized binding."
  (let* ((ir (nelisp-aot-compiler--parse
              '(defun f () (let (x) x))))
         (body (nelisp-aot-compiler--ir-get ir :body)))
    (should (eq (nelisp-aot-compiler--ir-kind body) 'imm))
    (should (= (nelisp-aot-compiler--ir-get body :value) 0))
    (should (= (nelisp-aot-compiler--ir-get ir :rt-slot-count) 0))))

(ert-deftest nelisp-aot-compiler/parse-doc129-multi-let-rt ()
  "Doc 129.4: multi-binding runtime `let' parses to `let-rt-n'."
  (let* ((ir (nelisp-aot-compiler--parse
              '(seq (defun id (x) x)
                    (defun f (x y)
                      (let ((a (id x))
                            (b (+ y 10)))
                        (+ a b))))))
         (f-ir (nth 1 (nelisp-aot-compiler--ir-get ir :forms)))
         (body (nelisp-aot-compiler--ir-get f-ir :body))
         (bindings (nelisp-aot-compiler--ir-get body :bindings)))
    (should (eq (nelisp-aot-compiler--ir-kind body) 'let-rt-n))
    (should (= (length bindings) 2))
    (should (equal (mapcar #'car bindings) '(a b)))
    (should (equal (mapcar #'cadr bindings) '(2 3)))
    (should (= (nelisp-aot-compiler--ir-get f-ir :rt-slot-count) 2))
    (should (eq (nelisp-aot-compiler--ir-kind
                 (nelisp-aot-compiler--ir-get body :body))
                'arith))))

(ert-deftest nelisp-aot-compiler/parse-doc129-multi-let-is-parallel ()
  "Doc 129.4: later `let' initializers cannot see earlier bindings."
  (should-error
   (nelisp-aot-compiler--parse
    '(defun f (x)
       (let ((a (+ x 1))
             (b a))
         b)))
   :type 'nelisp-aot-compiler-error))

(ert-deftest nelisp-aot-compiler/let-rt-requires-defun-context ()
  "Runtime `let' outside any defun context signals an error."
  ;; Top-level `let' where the value is not foldable (= extern-call ref
  ;; in a synthetic fenv that would block folding).  The easiest way to
  ;; trigger this is a symbol in the outer fenv (= defun param), but
  ;; `--parse' starts with fenv=nil.  Use a direct test of the raw
  ;; parser path: a `let' whose value is an extern-call form.
  ;; At top level `--next-rt-let-slot' is nil → should signal.
  (should-error
   (nelisp-aot-compiler--parse
    ;; `extern-call' is never compile-time foldable, so this forces
    ;; the runtime path.  At top level (= no enclosing defun parse)
    ;; `--next-rt-let-slot' is nil → error.
    '(let ((x (extern-call getpid))) (exit x)))
   :type 'nelisp-aot-compiler-error))

(ert-deftest nelisp-aot-compiler/e2e-let-rt-call-result ()
  "`(let ((y (id x))) (+ y 1))' inside a defun exits with x+1."
  (unless (nelisp-aot-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-aot-compiler-test--with-tmp-binary path "let-rt-call"
    (nelisp-aot-compile-sexp
     '(seq (defun id (x) x)
           (defun f (x) (let ((y (id x))) (+ y 1)))
           (exit (f 6)))
     path)
    (let ((r (nelisp-aot-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 7)))))

(ert-deftest nelisp-aot-compiler/e2e-let-rt-arith-value ()
  "`(let ((y (+ a b))) (+ y 1))' where a+b is a runtime arith."
  (unless (nelisp-aot-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-aot-compiler-test--with-tmp-binary path "let-rt-arith"
    (nelisp-aot-compile-sexp
     '(seq (defun f (a b) (let ((y (+ a b))) (+ y 1)))
           (exit (f 3 4)))
     path)
    (let ((r (nelisp-aot-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 8)))))

(ert-deftest nelisp-aot-compiler/e2e-let-rt-two-bindings ()
  "Two sequential runtime `let' bindings (= nested) resolve correctly."
  (unless (nelisp-aot-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-aot-compiler-test--with-tmp-binary path "let-rt-two"
    (nelisp-aot-compile-sexp
     '(seq (defun id (x) x)
           (defun f (x)
             (let ((a (id x)))
               (let ((b (+ a 10)))
                 (+ a b))))
           (exit (f 5)))
     path)
    ;; a = id(5) = 5; b = a + 10 = 15; result = a + b = 20.
    (let ((r (nelisp-aot-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 20)))))

(ert-deftest nelisp-aot-compiler/e2e-doc129-multi-let-rt ()
  "Doc 129.4: multi-binding runtime `let' executes through `let-rt-n'."
  (unless (nelisp-aot-compiler-test--linux-p)
    (ert-skip "Requires x86_64 Linux"))
  (nelisp-aot-compiler-test--with-tmp-binary path "doc129-multi-let"
    (nelisp-aot-compile-sexp
     '(seq (defun id (x) x)
           (defun f (x y)
             (let ((a (id x))
                   (b (+ y 10)))
               (+ a b)))
           (exit (f 5 7)))
     path)
    (let ((r (nelisp-aot-compiler-test--run-binary path)))
      (should (= (plist-get r :exit) 22)))))

;; ---- Doc 101 §101.B Wave 5 — Win64 ABI emit tests ----
;;
;; These tests verify that `nelisp-aot-compile-to-object' with
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

(ert-deftest nelisp-aot-compiler/win64-abi-arg-regs-dynvar ()
  "Win64 ABI dynvar selects RCX RDX R8 R9 as integer arg registers."
  (let ((nelisp-aot-compiler--abi 'win64))
    (should (equal (nelisp-aot-compiler--current-arg-regs)
                   '(rcx rdx r8 r9)))))

(ert-deftest nelisp-aot-compiler/sysv-abi-arg-regs-dynvar ()
  "SysV ABI dynvar selects RDI RSI RDX RCX R8 R9 as integer arg registers."
  (let ((nelisp-aot-compiler--abi 'sysv))
    (should (equal (nelisp-aot-compiler--current-arg-regs)
                   '(rdi rsi rdx rcx r8 r9)))))

(ert-deftest nelisp-aot-compiler/win64-abi-xmm-arg-regs-dynvar ()
  "Win64 ABI dynvar limits f64 arg registers to XMM0-XMM3."
  (let ((nelisp-aot-compiler--arch 'x86_64)
        (nelisp-aot-compiler--abi 'win64))
    (should (equal (nelisp-aot-compiler--current-xmm-arg-regs)
                   '(xmm0 xmm1 xmm2 xmm3)))))

(ert-deftest nelisp-aot-compiler/sysv-abi-xmm-arg-regs-dynvar ()
  "SysV ABI keeps the existing XMM0-XMM7 f64 arg register budget."
  (let ((nelisp-aot-compiler--arch 'x86_64)
        (nelisp-aot-compiler--abi 'sysv))
    (should (equal (nelisp-aot-compiler--current-xmm-arg-regs)
                   '(xmm0 xmm1 xmm2 xmm3 xmm4 xmm5 xmm6 xmm7)))))

(ert-deftest nelisp-aot-compiler/win64-defun-fifth-f64-param-spills-from-stack ()
  "Win64 f64 defuns spill a fifth f64 param from the incoming stack area."
  (let ((path (make-temp-file "nelisp-win64-f64-param5-" nil ".obj")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-to-object
           '(defun probe ((a :type f64) (b :type f64) (c :type f64)
                          (d :type f64) (e :type f64))
              e)
           path :arch 'x86_64 :format 'coff)
          (let* ((bytes (nelisp-aot-compiler-test--read-bytes path))
                 (text (nelisp-aot-compiler-test--coff-section-bytes
                        bytes ".text")))
            (should (nelisp-aot-compiler-test--bytes-contain-p
                     text
                     ;; sub rsp,48
                     (unibyte-string #x48 #x81 #xec #x30 #x00 #x00 #x00)))
            (should (nelisp-aot-compiler-test--bytes-contain-p
                     text
                     ;; movsd xmm0,[rbp+48]; movsd [rbp-40],xmm0
                     (unibyte-string #xf2 #x0f #x10 #x45 #x30
                                     #xf2 #x0f #x11 #x45 #xd8)))
            (should (nelisp-aot-compiler-test--bytes-contain-p
                     text
                     ;; body return: movsd xmm0,[rbp-40]
                     (unibyte-string #xf2 #x0f #x10 #x45 #xd8)))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-aot-compiler/win64-defun-accepts-four-f64-params ()
  "Win64 f64 defuns accept the XMM0-XMM3 argument window."
  (let ((path (make-temp-file "nelisp-win64-f64-param4-" nil ".obj")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-to-object
           '(defun probe ((a :type f64) (b :type f64) (c :type f64)
                          (d :type f64))
              a)
           path :arch 'x86_64 :format 'coff)
          (should (file-exists-p path)))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-aot-compiler/win64-parse-mixed-defun-param-regs ()
  "Win64 mixed defun params use position-paired GP/XMM registers."
  (let ((nelisp-aot-compiler--arch 'x86_64)
        (nelisp-aot-compiler--abi 'win64))
    (let ((ir (nelisp-aot-compiler--parse
               '(defun probe (a (b :type f64) c (d :type f64)) b))))
      (should (eq (nelisp-aot-compiler--ir-get ir :param-class) 'mixed))
      (should (equal (nelisp-aot-compiler--ir-get ir :param-classes)
                     '(gp f64 gp f64)))
      (should (equal (nelisp-aot-compiler--ir-get ir :param-regs)
                     '(rcx xmm1 r8 xmm3)))
      (let ((body (nelisp-aot-compiler--ir-get ir :body)))
        (should (eq (nelisp-aot-compiler--ir-kind body) 'ref))
        (should (eq (nelisp-aot-compiler--ir-get body :class) 'f64))
        (should (= (nelisp-aot-compiler--ir-get body :slot) 1))))))

(ert-deftest nelisp-aot-compiler/win64-defun-mixed-param-spills ()
  "Win64 mixed defun params spill GP/XMM registers by argument position."
  (let ((path (make-temp-file "nelisp-win64-mixed-defun-" nil ".obj")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-to-object
           '(defun probe (a (b :type f64) c (d :type f64)) b)
           path :arch 'x86_64 :format 'coff)
          (let* ((bytes (nelisp-aot-compiler-test--read-bytes path))
                 (text (nelisp-aot-compiler-test--coff-section-bytes
                        bytes ".text")))
            (should (nelisp-aot-compiler-test--bytes-contain-p
                     text
                     ;; sub rsp,32; mov [rbp-8],rcx; movsd [rbp-16],xmm1;
                     ;; mov [rbp-24],r8; movsd [rbp-32],xmm3
                     (unibyte-string #x48 #x81 #xec #x20 #x00 #x00 #x00
                                     #x48 #x89 #x4d #xf8
                                     #xf2 #x0f #x11 #x4d #xf0
                                     #x4c #x89 #x45 #xe8
                                     #xf2 #x0f #x11 #x5d #xe0)))
            (should (nelisp-aot-compiler-test--bytes-contain-p
                     text
                     ;; body return: movsd xmm0,[rbp-16]
                     (unibyte-string #xf2 #x0f #x10 #x45 #xf0)))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-aot-compiler/win64-defun-mixed-fifth-param-spills-from-stack ()
  "Win64 mixed defuns spill a fifth param from the incoming stack area."
  (let ((path (make-temp-file "nelisp-win64-mixed-defun5-" nil ".obj")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-to-object
           '(defun probe (a (b :type f64) c (d :type f64) (e :type f64)) e)
           path :arch 'x86_64 :format 'coff)
          (let* ((bytes (nelisp-aot-compiler-test--read-bytes path))
                 (text (nelisp-aot-compiler-test--coff-section-bytes
                        bytes ".text")))
            (should (nelisp-aot-compiler-test--bytes-contain-p
                     text
                     ;; movsd xmm0,[rbp+48]; movsd [rbp-40],xmm0
                     (unibyte-string #xf2 #x0f #x10 #x45 #x30
                                     #xf2 #x0f #x11 #x45 #xd8)))
            (should (nelisp-aot-compiler-test--bytes-contain-p
                     text
                     ;; body return: movsd xmm0,[rbp-40]
                     (unibyte-string #xf2 #x0f #x10 #x45 #xd8)))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-aot-compiler/win64-defun-gp-fifth-param-spills-from-stack ()
  "Win64 GP defuns spill a fifth GP param from the incoming stack area."
  (let ((path (make-temp-file "nelisp-win64-gp-defun5-" nil ".obj")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-to-object
           '(defun probe (a b c d e) e)
           path :arch 'x86_64 :format 'coff)
          (let* ((bytes (nelisp-aot-compiler-test--read-bytes path))
                 (text (nelisp-aot-compiler-test--coff-section-bytes
                        bytes ".text")))
            (should (nelisp-aot-compiler-test--bytes-contain-p
                     text
                     ;; mov rax,[rbp+48]; mov [rbp-40],rax
                     (unibyte-string #x48 #x8b #x45 #x30
                                     #x48 #x89 #x45 #xd8)))
            (should (nelisp-aot-compiler-test--bytes-contain-p
                     text
                     ;; body return: mov rax,[rbp-40]
                     (unibyte-string #x48 #x8b #x45 #xd8)))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-aot-compiler/win64-extern-call-places-fifth-f64-arg-on-stack ()
  "Win64 extern-call places a fifth f64 arg in the outgoing stack area."
  (let ((path (make-temp-file "nelisp-win64-f64-extern5-" nil ".obj")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-to-object
           '(defun probe ()
              (extern-call ext
                           (:f64 (bits-to-f64 1))
                           (:f64 (bits-to-f64 2))
                           (:f64 (bits-to-f64 3))
                           (:f64 (bits-to-f64 4))
                           (:f64 (bits-to-f64 5))))
           path :arch 'x86_64 :format 'coff)
          (let* ((bytes (nelisp-aot-compiler-test--read-bytes path))
                 (text (nelisp-aot-compiler-test--coff-section-bytes
                        bytes ".text")))
            (should (nelisp-aot-compiler-test--bytes-contain-p
                     text
                     ;; sub rsp, 40; mov r10,[rsp+40]; mov [rsp+32],r10;
                     ;; call rel32; add rsp,40.  The copied 8 bytes are
                     ;; the fifth f64 bit pattern saved by the spill path.
                     (unibyte-string #x48 #x81 #xec #x28 #x00 #x00 #x00
                                     #x4c #x8b #x54 #x24 #x28
                                     #x4c #x89 #x54 #x24 #x20
                                     #xe8 #x00 #x00 #x00 #x00
                                     #x48 #x81 #xc4 #x28 #x00 #x00 #x00)))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-aot-compiler/win64-extern-call-mixed-f64-slots ()
  "Win64 mixed extern-call assigns f64 registers by argument position."
  (let ((path (make-temp-file "nelisp-win64-f64-mixed-" nil ".obj")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-to-object
           '(defun probe ()
              (extern-call ext
                           1
                           (:f64 (bits-to-f64 2))
                           3
                           (:f64 (bits-to-f64 4))))
           path :arch 'x86_64 :format 'coff)
          (let* ((bytes (nelisp-aot-compiler-test--read-bytes path))
                 (text (nelisp-aot-compiler-test--coff-section-bytes
                        bytes ".text")))
            (should (nelisp-aot-compiler-test--bytes-contain-p
                     text
                     ;; pop rax; movq xmm3,rax
                     (unibyte-string #x58 #x66 #x48 #x0f #x6e #xd8)))
            (should (nelisp-aot-compiler-test--bytes-contain-p
                     text
                     ;; pop r8
                     (unibyte-string #x41 #x58)))
            (should (nelisp-aot-compiler-test--bytes-contain-p
                     text
                     ;; pop rax; movq xmm1,rax
                     (unibyte-string #x58 #x66 #x48 #x0f #x6e #xc8)))
            (should (nelisp-aot-compiler-test--bytes-contain-p
                     text
                     ;; pop rcx
                     (unibyte-string #x59)))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-aot-compiler/win64-coff-smoke ()
  "Compile `(defun foo (a b) (+ a b))' to a COFF .o and check Win64 prologue.
The first byte of .text must be 0x55 (push rbp).  The frame must NOT
start with a SysV `push rdi' (0x57) — that would indicate the old
SysV path was taken instead of Win64."
  (let ((path (make-temp-file "nelisp-win64-coff-smoke-" nil ".obj")))
    (unwind-protect
        (progn
          (require 'nelisp-aot-compiler)
          (nelisp-aot-compile-to-object
           '(defun foo (a b) (+ a b))
           path :arch 'x86_64 :format 'coff)
          (let* ((all-bytes (nelisp-aot-compiler-test--read-bytes path))
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

(ert-deftest nelisp-aot-compiler/win64-coff-no-push-rdi ()
  "COFF emit must NOT use `push rdi' (0x57) for arg spill (SysV artifact).
Win64 spills args via `mov [rbp-disp], reg' instead."
  (let ((path (make-temp-file "nelisp-win64-no-push-rdi-" nil ".obj")))
    (unwind-protect
        (progn
          (require 'nelisp-aot-compiler)
          (nelisp-aot-compile-to-object
           '(defun bar (x) x)
           path :arch 'x86_64 :format 'coff)
          (let* ((bytes (nelisp-aot-compiler-test--read-bytes path))
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

(ert-deftest nelisp-aot-compiler/win64-buffer-abi-is-win64 ()
  "compile-to-object with format 'coff binds --abi to 'win64.
We verify indirectly: the emitted .text for a 1-arg function must
contain REX.W (0x48) immediately before the MOV spill opcode (0x89),
which is the Win64 `mov [rbp-8], rcx' = 48 89 4D F8 sequence.
SysV would emit `push rdi' = 57 instead."
  (let ((path (make-temp-file "nelisp-win64-buf-abi-" nil ".obj")))
    (unwind-protect
        (progn
          (require 'nelisp-aot-compiler)
          (nelisp-aot-compile-to-object
           '(defun id (x) x)
           path :arch 'x86_64 :format 'coff)
          (let* ((bytes (nelisp-aot-compiler-test--read-bytes path))
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

(ert-deftest nelisp-aot-compiler/win64-defun-preserves-rdi-rsi ()
  "Win64 defuns preserve RDI/RSI in private callee-save frame slots."
  (let ((path (make-temp-file "nelisp-win64-callee-save-" nil ".obj")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-to-object
           '(defun probe () 0)
           path :arch 'x86_64 :format 'coff)
          (let* ((bytes (nelisp-aot-compiler-test--read-bytes path))
                 (text (nelisp-aot-compiler-test--coff-section-bytes
                        bytes ".text")))
            (should (nelisp-aot-compiler-test--bytes-contain-p
                     text
                     ;; mov [rbp-160],rdi; mov [rbp-168],rsi
                     ;; (shifted down 8 by the RBX callee-save slot at [rbp-152])
                     (unibyte-string #x48 #x89 #xbd #x60 #xff #xff #xff
                                     #x48 #x89 #xb5 #x58 #xff #xff #xff)))
            (should (nelisp-aot-compiler-test--bytes-contain-p
                     text
                     ;; mov rdi,[rbp-160]; mov rsi,[rbp-168]; mov rsp,rbp; pop rbp; ret
                     (unibyte-string #x48 #x8b #xbd #x60 #xff #xff #xff
                                     #x48 #x8b #xb5 #x58 #xff #xff #xff
                                     #x48 #x89 #xec #x5d #xc3)))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-aot-compiler/win64-defun-preserves-xmm6-xmm15 ()
  "Win64 defuns preserve callee-saved XMM6-XMM15 in private slots."
  (let ((path (make-temp-file "nelisp-win64-xmm-callee-save-" nil ".obj")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-to-object
           '(defun probe () 0)
           path :arch 'x86_64 :format 'coff)
          (let* ((bytes (nelisp-aot-compiler-test--read-bytes path))
                 (text (nelisp-aot-compiler-test--coff-section-bytes
                        bytes ".text")))
            (should (nelisp-aot-compiler-test--bytes-contain-p
                     text
                     ;; sub rsp,160 for XMM6-XMM15 save area.
                     (unibyte-string #x48 #x81 #xec #xa0 #x00 #x00 #x00)))
            (should (nelisp-aot-compiler-test--bytes-contain-p
                     text
                     ;; movdqu [rbp-192],xmm6 (xmm block shifted 16 by the RBX slot)
                     (unibyte-string #xf3 #x0f #x7f #xb5 #x40 #xff #xff #xff)))
            (should (nelisp-aot-compiler-test--bytes-contain-p
                     text
                     ;; movdqu [rbp-336],xmm15
                     (unibyte-string #xf3 #x44 #x0f #x7f #xbd
                                     #xb0 #xfe #xff #xff)))
            (should (nelisp-aot-compiler-test--bytes-contain-p
                     text
                     ;; movdqu xmm6,[rbp-192]
                     (unibyte-string #xf3 #x0f #x6f #xb5 #x40 #xff #xff #xff)))
            (should (nelisp-aot-compiler-test--bytes-contain-p
                     text
                     ;; movdqu xmm15,[rbp-336]
                     (unibyte-string #xf3 #x44 #x0f #x6f #xbd
                                     #xb0 #xfe #xff #xff)))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-aot-compiler/win64-defun-callee-saves-after-frame-slots ()
  "Win64 callee-save slots live below param and runtime let slots."
  (let ((path (make-temp-file "nelisp-win64-callee-save-frame-" nil ".obj")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-to-object
           '(defun probe (x) (let ((y (extern-call ext_y))) (+ x y)))
           path :arch 'x86_64 :format 'coff)
          (let* ((bytes (nelisp-aot-compiler-test--read-bytes path))
                 (text (nelisp-aot-compiler-test--coff-section-bytes
                        bytes ".text")))
            (should (nelisp-aot-compiler-test--bytes-contain-p
                     text
                     ;; mov [rbp-176],rdi; mov [rbp-184],rsi
                     ;; (shifted down 8 by the RBX callee-save slot)
                     (unibyte-string #x48 #x89 #xbd #x50 #xff #xff #xff
                                     #x48 #x89 #xb5 #x48 #xff #xff #xff)))
            (should (nelisp-aot-compiler-test--bytes-contain-p
                     text
                     ;; Restore from the same private slots before frame teardown.
                     (unibyte-string #x48 #x8b #xbd #x50 #xff #xff #xff
                                     #x48 #x8b #xb5 #x48 #xff #xff #xff
                                     #x48 #x89 #xec #x5d #xc3)))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-aot-compiler/win64-extern-call-gp-args-and-shadow ()
  "Win64 extern-call uses RCX/RDX/R8/R9 and 32-byte caller shadow space."
  (let ((path (make-temp-file "nelisp-win64-extern-call-" nil ".obj")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-to-object
           '(defun probe () (extern-call ext4 1 2 3 4))
           path :arch 'x86_64 :format 'coff)
          (let* ((bytes (nelisp-aot-compiler-test--read-bytes path))
                 (text (nelisp-aot-compiler-test--coff-section-bytes
                        bytes ".text")))
            (should (nelisp-aot-compiler-test--bytes-contain-p
                     text
                     ;; mov rcx,1; mov rdx,2; mov r8,3; mov r9,4
                     (unibyte-string #x48 #xc7 #xc1 #x01 #x00 #x00 #x00
                                     #x48 #xc7 #xc2 #x02 #x00 #x00 #x00
                                     #x49 #xc7 #xc0 #x03 #x00 #x00 #x00
                                     #x49 #xc7 #xc1 #x04 #x00 #x00 #x00)))
            (should (nelisp-aot-compiler-test--bytes-contain-p
                     text
                     ;; sub rsp, 32; call rel32; add rsp, 32
                     (unibyte-string #x48 #x81 #xec #x20 #x00 #x00 #x00
                                     #xe8 #x00 #x00 #x00 #x00
                                     #x48 #x81 #xc4 #x20 #x00 #x00 #x00)))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-aot-compiler/win64-alloc-bytes-uses-gp-args-and-shadow ()
  "Win64 alloc-bytes calls nl_alloc_bytes via RCX/RDX and shadow space."
  (let ((path (make-temp-file "nelisp-win64-alloc-bytes-" nil ".obj")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-to-object
           '(defun probe () (alloc-bytes 32 8))
           path :arch 'x86_64 :format 'coff)
          (let* ((bytes (nelisp-aot-compiler-test--read-bytes path))
                 (text (nelisp-aot-compiler-test--coff-section-bytes
                        bytes ".text")))
            (should (nelisp-aot-compiler-test--bytes-contain-p
                     text
                     ;; mov rax,32; push rax; mov rax,8; push rax;
                     ;; pop rdx; pop rcx; sub rsp,32; call; add rsp,32.
                     (unibyte-string #x48 #xc7 #xc0 #x20 #x00 #x00 #x00
                                     #x50
                                     #x48 #xc7 #xc0 #x08 #x00 #x00 #x00
                                     #x50
                                     #x5a #x59
                                     #x48 #x81 #xec #x20 #x00 #x00 #x00
                                     #xe8 #x00 #x00 #x00 #x00
                                     #x48 #x81 #xc4 #x20 #x00 #x00 #x00)))
            (should-not (nelisp-aot-compiler-test--bytes-contain-p
                         text
                         ;; Old SysV-only sequence: pop rsi; pop rdi;
                         ;; push rax; call.
                         (unibyte-string #x5e #x5f #x50 #xe8)))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-aot-compiler/win64-dealloc-bytes-uses-gp-args-and-shadow ()
  "Win64 dealloc-bytes calls nl_dealloc_bytes via RCX/RDX/R8 and shadow space."
  (let ((path (make-temp-file "nelisp-win64-dealloc-bytes-" nil ".obj")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-to-object
           '(defun probe (p) (dealloc-bytes p 32 8))
           path :arch 'x86_64 :format 'coff)
          (let* ((bytes (nelisp-aot-compiler-test--read-bytes path))
                 (text (nelisp-aot-compiler-test--coff-section-bytes
                        bytes ".text")))
            (should (nelisp-aot-compiler-test--bytes-contain-p
                     text
                     ;; pop r8; pop rdx; pop rcx; sub rsp,32 shadow;
                     ;; call; add shadow back.  Win64 prologues leave the
                     ;; body aligned, so odd arity does not need a pad here.
                     (unibyte-string #x41 #x58 #x5a #x59
                                     #x48 #x81 #xec #x20 #x00 #x00 #x00
                                     #xe8 #x00 #x00 #x00 #x00
                                     #x48 #x81 #xc4 #x20 #x00 #x00 #x00)))
            (should-not (nelisp-aot-compiler-test--bytes-contain-p
                         text
                         ;; Old SysV-only sequence: pop rdx; pop rsi;
                         ;; pop rdi; push rax; call.
                         (unibyte-string #x5a #x5e #x5f #x50 #xe8)))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-aot-compiler/win64-boxed-allocators-use-shadow ()
  "Win64 boxed alloc primitives reserve caller shadow space."
  (let ((text (nelisp-aot-compiler-test--coff-text-for
               '(seq
                 (defun probe_cons () (cons-make 4096 8192 12288))
                 (defun probe_record () (record-make 4096 2 12288))
                 (defun probe_vector () (vector-make 2 12288))
                 (defun probe_cell () (cell-make 4096 12288))))))
    (should (nelisp-aot-compiler-test--bytes-contain-p
             text
             ;; cons-make: 4 saved pushes, then shadow/call/unshadow.
             (unibyte-string #x50
                             #x48 #x81 #xec #x20 #x00 #x00 #x00
                             #xe8 #x00 #x00 #x00 #x00
                             #x48 #x81 #xc4 #x20 #x00 #x00 #x00)))
    (should (nelisp-aot-compiler-test--bytes-contain-p
             text
             ;; record-make: pop slot/count/tag -> RAX/RDX/RCX.
             (unibyte-string #x58 #x5a #x59 #x50 #x50
                             #x48 #x81 #xec #x20 #x00 #x00 #x00
                             #xe8 #x00 #x00 #x00 #x00
                             #x48 #x81 #xc4 #x20 #x00 #x00 #x00)))
    (should (nelisp-aot-compiler-test--bytes-contain-p
             text
             ;; vector-make: pop slot/cap -> RSI/RCX, preserve slot, shadow.
             (unibyte-string #x41 #x5b #x5e #x59 #x56 #x56
                             #x48 #x81 #xec #x20 #x00 #x00 #x00
                             #xe8 #x00 #x00 #x00 #x00
                             #x48 #x81 #xc4 #x20 #x00 #x00 #x00)))
    (should (nelisp-aot-compiler-test--bytes-contain-p
             text
             ;; cell-make: RCX = val-ptr before shadow/call.
             (unibyte-string #x41 #x5b #x5e #x5f #x56 #x56
                             #x48 #x89 #xf9
                             #x48 #x81 #xec #x20 #x00 #x00 #x00
                             #xe8 #x00 #x00 #x00 #x00
                             #x48 #x81 #xc4 #x20 #x00 #x00 #x00)))))

(ert-deftest nelisp-aot-compiler/win64-boxed-setters-use-gp-args-and-shadow ()
  "Win64 boxed setter primitives use RCX/RDX/R8 and caller shadow space."
  (let ((text (nelisp-aot-compiler-test--coff-text-for
               '(seq
                 (defun probe_cons_set () (cons-set-car 4096 8192))
                 (defun probe_cell_set () (cell-set-value 4096 8192))
                 (defun probe_rec_set () (record-slot-set 4096 0 8192))
                 (defun probe_vec_set () (vector-slot-set 4096 0 8192))))))
    (should (nelisp-aot-compiler-test--bytes-contain-p
             text
             ;; cons-set-car/cell-set-value: RDX = value, RCX = boxed payload.
             (unibyte-string #x5e #x5f #x57 #x56
                             #x48 #x89 #xf2
                             #x48 #x8b #x4f #x08
                             #x48 #x81 #xec #x20 #x00 #x00 #x00
                             #xe8 #x00 #x00 #x00 #x00
                             #x48 #x81 #xc4 #x20 #x00 #x00 #x00)))
    (should (nelisp-aot-compiler-test--bytes-contain-p
             text
             ;; record/vector-slot-set: R8 = value, RDX = index, RCX = payload.
             (unibyte-string #x41 #x58 #x5a #x59
                             #x48 #x8b #x49 #x08
                             #x48 #x81 #xec #x20 #x00 #x00 #x00
                             #xe8 #x00 #x00 #x00 #x00
                             #x48 #x81 #xc4 #x20 #x00 #x00 #x00)))))

(ert-deftest nelisp-aot-compiler/win64-cons-make-with-clone-uses-gp-args ()
  "Win64 cons-make-with-clone calls clone helper with RCX/RDX."
  (let ((text (nelisp-aot-compiler-test--coff-text-for
               '(defun probe ()
                  (cons-make-with-clone 4096 8192 12288)))))
    (should (nelisp-aot-compiler-test--bytes-contain-p
             text
             ;; car clone: RCX = car-ptr, RDX = box.
             (unibyte-string #x48 #x89 #xf9
                             #x4c #x89 #xd2
                             #x48 #x81 #xec #x20 #x00 #x00 #x00
                             #xe8 #x00 #x00 #x00 #x00
                             #x48 #x81 #xc4 #x20 #x00 #x00 #x00)))
    (should (nelisp-aot-compiler-test--bytes-contain-p
             text
             ;; cdr clone: RCX = cdr-ptr, RDX = box + cdr offset.
             ;; Doc 147 Phase 3: cdr offset 32 -> 8 (`add rdx, 8' =
             ;; #x48 #x81 #xc2 #x08...; the NlConsBox cdr is now an 8-byte
             ;; tagged WORD @ box+8, stored via `nl_val_clone_into').
             (unibyte-string #x48 #x89 #xd1
                             #x4c #x89 #xd2
                             #x48 #x81 #xc2 #x08 #x00 #x00 #x00
                             #x48 #x81 #xec #x20 #x00 #x00 #x00
                             #xe8 #x00 #x00 #x00 #x00
                             #x48 #x81 #xc4 #x20 #x00 #x00 #x00)))))

(ert-deftest nelisp-aot-compiler/win64-sexp-write-helpers-use-gp-args-and-shadow ()
  "Win64 sexp-write string helpers use RCX/RDX/R8 and caller shadow space."
  (let ((text (nelisp-aot-compiler-test--coff-text-for
               '(seq
                 (defun probe_symbol () (sexp-write-symbol 4096 1 12288))
                 (defun probe_symbol_lit () (sexp-write-symbol-lit 12288 "x"))))))
    (should (nelisp-aot-compiler-test--bytes-contain-p
             text
             ;; sexp-write-symbol: pop slot/len/bytes -> R8/RDX/RCX.
             (unibyte-string #x41 #x58 #x5a #x59
                             #x48 #x81 #xec #x20 #x00 #x00 #x00
                             #xe8 #x00 #x00 #x00 #x00
                             #x48 #x81 #xc4 #x20 #x00 #x00 #x00)))
    (should (nelisp-aot-compiler-test--bytes-contain-p
             text
             ;; sexp-write-symbol-lit: remap SysV staging regs to Win64.
             (unibyte-string #x49 #x89 #xd0
                             #x48 #x89 #xf2
                             #x48 #x89 #xf9
                             #x48 #x81 #xec #x20 #x00 #x00 #x00
                             #xe8 #x00 #x00 #x00 #x00
                             #x48 #x81 #xc4 #x20 #x00 #x00 #x00)))
    (should-not (nelisp-aot-compiler-test--bytes-contain-p
                 text
                 ;; Old SysV call pad after RDI/RSI/RDX setup.
                 (unibyte-string #x5a #x5e #x5f #x50 #xe8)))))

(ert-deftest nelisp-aot-compiler/win64-string-runtime-helpers-use-gp-args-and-shadow ()
  "Win64 mutable string and UTF-8 helpers use Win64 GP args plus shadow space."
  (let ((text (nelisp-aot-compiler-test--coff-text-for
               '(seq
                 (defun probe_make () (mut-str-make-empty 2 12288))
                 (defun probe_push () (mut-str-push-byte 12288 65))
                 (defun probe_len () (mut-str-len 12288))
                 (defun probe_finalize () (mut-str-finalize 12288 16384))
                 (defun probe_count () (str-char-count 4096))
                 (defun probe_codepoint () (str-codepoint-at 4096 0 12288 16384))
                 (defun probe_alnum () (str-is-alphanumeric-at 4096 0))))))
    (should (nelisp-aot-compiler-test--bytes-contain-p
             text
             ;; 2-arg helpers: pop RDX/RCX, reserve shadow, call.
             (unibyte-string #x5a #x59
                             #x48 #x81 #xec #x20 #x00 #x00 #x00
                             #xe8 #x00 #x00 #x00 #x00
                             #x48 #x81 #xc4 #x20 #x00 #x00 #x00)))
    (should (nelisp-aot-compiler-test--bytes-contain-p
             text
             ;; 1-arg helpers: pop RCX, reserve shadow, call.
             (unibyte-string #x59
                             #x48 #x81 #xec #x20 #x00 #x00 #x00
                             #xe8 #x00 #x00 #x00 #x00
                             #x48 #x81 #xc4 #x20 #x00 #x00 #x00)))
    (should (nelisp-aot-compiler-test--bytes-contain-p
             text
             ;; str-codepoint-at: pop R9/R8/RDX/RCX, reserve shadow, call.
             (unibyte-string #x41 #x59 #x41 #x58 #x5a #x59
                             #x48 #x81 #xec #x20 #x00 #x00 #x00
                             #xe8 #x00 #x00 #x00 #x00
                             #x48 #x81 #xc4 #x20 #x00 #x00 #x00)))
    (should-not (nelisp-aot-compiler-test--bytes-contain-p
                 text
                 ;; Old SysV 4-arg helper sequence.
                 (unibyte-string #x59 #x5a #x5e #x5f #x50 #xe8)))))

(ert-deftest nelisp-aot-compiler/win64-extern-call-stack-gp-arg ()
  "Win64 extern-call places the fifth GP arg above the shadow space."
  (let ((path (make-temp-file "nelisp-win64-extern-call-stack-" nil ".obj")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-to-object
           '(defun probe () (extern-call ext5 1 2 3 4 5))
           path :arch 'x86_64 :format 'coff)
          (let* ((bytes (nelisp-aot-compiler-test--read-bytes path))
                 (text (nelisp-aot-compiler-test--coff-section-bytes
                        bytes ".text")))
            (should (nelisp-aot-compiler-test--bytes-contain-p
                     text
                     ;; sub rsp, 48; mov rax,5; mov [rsp+32],rax;
                     ;; call rel32; add rsp,48
                     (unibyte-string #x48 #x81 #xec #x30 #x00 #x00 #x00
                                     #x48 #xc7 #xc0 #x05 #x00 #x00 #x00
                                     #x48 #x89 #x44 #x24 #x20
                                     #xe8 #x00 #x00 #x00 #x00
                                     #x48 #x81 #xc4 #x30 #x00 #x00 #x00)))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-aot-compiler/win64-extern-call-seven-gp-stack-args-align ()
  "Win64 7-arg extern-call reserves aligned shadow plus stack args."
  (let ((path (make-temp-file "nelisp-win64-extern-call-7gp-" nil ".obj")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-to-object
           '(defun probe (p)
              (extern-call CreateFileA p 2147483648 1 0 3 128 0))
           path :arch 'x86_64 :format 'coff)
          (let* ((bytes (nelisp-aot-compiler-test--read-bytes path))
                 (text (nelisp-aot-compiler-test--coff-section-bytes
                        bytes ".text")))
            (should (nelisp-aot-compiler-test--bytes-contain-p
                     text
                     ;; 3 stack args => 32-byte shadow + 24 bytes args +
                     ;; 8-byte alignment pad = 64 bytes.
                     (unibyte-string #x48 #x81 #xec #x40 #x00 #x00 #x00
                                     #x48 #xc7 #xc0 #x03 #x00 #x00 #x00
                                     #x48 #x89 #x44 #x24 #x20
                                     #x48 #xc7 #xc0 #x80 #x00 #x00 #x00
                                     #x48 #x89 #x44 #x24 #x28
                                     #x48 #xc7 #xc0 #x00 #x00 #x00 #x00
                                     #x48 #x89 #x44 #x24 #x30
                                     #xe8 #x00 #x00 #x00 #x00
                                     #x48 #x81 #xc4 #x40 #x00 #x00 #x00)))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-aot-compiler/win64-extern-call-nontrivial-stack-gp-arg ()
  "Win64 extern-call can copy a computed fifth GP arg into the outgoing area."
  (let ((path (make-temp-file "nelisp-win64-extern-call-stack-computed-" nil ".obj")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-to-object
           '(defun probe () (extern-call ext5 1 2 3 4 (extern-call ext_arg)))
           path :arch 'x86_64 :format 'coff)
          (let* ((bytes (nelisp-aot-compiler-test--read-bytes path))
                 (text (nelisp-aot-compiler-test--coff-section-bytes
                        bytes ".text")))
            (should (nelisp-aot-compiler-test--bytes-contain-p
                     text
                     ;; sub rsp, 40; mov r10,[rsp+40]; mov [rsp+32],r10;
                     ;; call rel32; add rsp,40
                     (unibyte-string #x48 #x81 #xec #x28 #x00 #x00 #x00
                                     #x4c #x8b #x54 #x24 #x28
                                     #x4c #x89 #x54 #x24 #x20
                                     #xe8 #x00 #x00 #x00 #x00
                                     #x48 #x81 #xc4 #x28 #x00 #x00 #x00)))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-aot-compiler/win64-internal-call-stack-gp-arg ()
  "Win64 internal calls place arg5 above shadow space instead of rejecting it."
  (let ((path (make-temp-file "nelisp-win64-call-stack-" nil ".obj")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-to-object
           '(seq
             (defun callee (a b c d e) e)
             (defun probe () (callee 1 2 3 4 5)))
           path :arch 'x86_64 :format 'coff)
          (let* ((bytes (nelisp-aot-compiler-test--read-bytes path))
                 (text (nelisp-aot-compiler-test--coff-section-bytes
                        bytes ".text")))
            (should (nelisp-aot-compiler-test--bytes-contain-p
                     text
                     ;; sub rsp,40; mov r10,[rsp+40]; mov [rsp+32],r10.
                     (unibyte-string #x48 #x81 #xec #x28 #x00 #x00 #x00
                                     #x4c #x8b #x54 #x24 #x28
                                     #x4c #x89 #x54 #x24 #x20)))
            (should (nelisp-aot-compiler-test--bytes-contain-p
                     text
                     ;; mov r10,[rsp+40]; mov [rsp+32],r10; call rel32.
                     (unibyte-string #x4c #x8b #x54 #x24 #x28
                                     #x4c #x89 #x54 #x24 #x20
                                     #xe8)))
            (should (nelisp-aot-compiler-test--bytes-contain-p
                     text
                     ;; add rsp,40 after the internal call returns.
                     (unibyte-string #x48 #x81 #xc4 #x28 #x00 #x00 #x00)))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-aot-compiler/win64-internal-call-odd-arity-no-pad ()
  "Win64 internal calls do not use SysV arity-based alignment pads."
  (let ((text (nelisp-aot-compiler-test--coff-text-for
               '(seq
                 (defun callee (a) a)
                 (defun probe (p) (callee p))))))
    (should (nelisp-aot-compiler-test--bytes-contain-p
             text
             ;; ref p -> rcx; sub rsp,32 shadow; call rel32.
             (unibyte-string #x48 #x8b #x4d #xf8
                             #x48 #x81 #xec #x20 #x00 #x00 #x00
                             #xe8)))
    (should (nelisp-aot-compiler-test--bytes-contain-p
             text
             ;; add rsp,32 after the internal call returns.
             (unibyte-string
                             #x48 #x81 #xc4 #x20 #x00 #x00 #x00)))
    (should-not (nelisp-aot-compiler-test--bytes-contain-p
                 text
                 ;; Old SysV-style pad for odd arity before shadow space.
                 (unibyte-string #x48 #x81 #xec #x08 #x00 #x00 #x00
                                 #x48 #x81 #xec #x20 #x00 #x00 #x00
                                 #xe8)))))

;; --- `f64-bits' soft-float keystone (nelisp-cfront) ---------------------
;; `(f64-bits F64-EXPR)' reinterprets an f64-class result's raw IEEE-754
;; bits as a gp-class i64 in rax (MOVQ rax, xmm0) — the inverse of
;; `bits-to-f64'.  This lets a C `double' be carried as its i64 bit
;; pattern so float ops lower to flat `(f64-bits (f64-OP (bits-to-f64 a)
;; (bits-to-f64 b)))' helpers entirely within the gp register class
;; (= no xmm spill / Doc 112 needed).

(ert-deftest nelisp-aot-compiler/f64-bits-soft-float-add-helper ()
  "A soft-float add helper emits one ADDSD then MOVQ rax, xmm0."
  (let ((text (nelisp-aot-compiler-test--coff-text-for
               '(defun dadd (a b)
                  (f64-bits (f64-add (bits-to-f64 a) (bits-to-f64 b)))))))
    ;; addsd xmm0, xmm1
    (should (nelisp-aot-compiler-test--bytes-contain-p
             text (unibyte-string #xf2 #x0f #x58 #xc1)))
    ;; movq rax, xmm0  (= 66 REX.W 0F 7E C0) — the f64-bits transfer
    (should (nelisp-aot-compiler-test--bytes-contain-p
             text (unibyte-string #x66 #x48 #x0f #x7e #xc0)))))

(ert-deftest nelisp-aot-compiler/f64-bits-int-to-double-helper ()
  "An int->double-bits helper emits CVTSI2SD then MOVQ rax, xmm0."
  (let ((text (nelisp-aot-compiler-test--coff-text-for
               '(defun i2d (x) (f64-bits (i64-to-f64 x))))))
    ;; cvtsi2sd xmm0, rax  (= F2 REX.W 0F 2A C0)
    (should (nelisp-aot-compiler-test--bytes-contain-p
             text (unibyte-string #xf2 #x48 #x0f #x2a #xc0)))
    ;; movq rax, xmm0
    (should (nelisp-aot-compiler-test--bytes-contain-p
             text (unibyte-string #x66 #x48 #x0f #x7e #xc0)))))

;; --- `frame-alloc' fixed stack block (nelisp-cfront aggregate locals) ---
;; `(frame-alloc NBYTES)' reserves NBYTES (rounded to whole 8-byte slots)
;; from the defun's frame slot pool and returns the block's lowest
;; address as a gp i64 — `mov rax, rbp; sub rax, K' (K via imm32 so large
;; blocks stay out of the disp8 slot range).  Models a C local array /
;; struct-by-value / address-taken scalar.

(ert-deftest nelisp-aot-compiler/frame-alloc-emits-rbp-minus-k ()
  "frame-alloc materialises its block address as `mov rax,rbp; sub rax,K'."
  (let ((text (nelisp-aot-compiler-test--coff-text-for
               '(defun probe () (let ((p (frame-alloc 32))) (ptr-read-u64 p 0))))))
    ;; mov rax, rbp (48 89 E8) immediately followed by sub rax, imm32 (48 81 E8 ..)
    (should (nelisp-aot-compiler-test--bytes-contain-p
             text (unibyte-string #x48 #x89 #xe8 #x48 #x81 #xe8)))))

;; --- `data-blob' same-unit static data + local-symbol address (Doc 06 A) ---
;; `(data-blob NAME BYTES SECTION)' places BYTES into `.rodata' under a
;; LOCAL OBJECT symbol NAME, and `(data-addr NAME)' takes its address via a
;; PC32 relocation against that local symbol.  This is the object-mode
;; keystone for cfront global/static data (no absolute vaddr baking).

(ert-deftest nelisp-aot-compiler/data-blob-local-rodata-symbol ()
  "`data-blob' defines a LOCAL OBJECT `.rodata' symbol with 2 PC32 relocs."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc06-blob-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-to-object
           '(seq
             (data-blob myblob (10 20 30 40 250 99 7 200) rodata)
             (defun getb (i) (ptr-read-u8 (data-addr myblob) i))
             (defun getword (i) (ptr-read-u32 (data-addr myblob) i)))
           path)
          (let ((syms (with-output-to-string
                        (with-current-buffer standard-output
                          (call-process "readelf" nil t nil "-s" path))))
                (rels (with-output-to-string
                        (with-current-buffer standard-output
                          (call-process "readelf" nil t nil "-r" path)))))
            ;; myblob is a LOCAL OBJECT (not UND / not GLOBAL extern).
            (should (string-match-p
                     "OBJECT  LOCAL  DEFAULT[ \t]+[0-9]+ myblob" syms))
            ;; two PC32 references against the local blob symbol.
            (should (= 2 (length (seq-filter
                                  (lambda (l) (string-match-p "R_X86_64_PC32" l))
                                  (split-string rels "\n")))))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-aot-compiler/data-blob-readback-e2e ()
  "Linked C driver reads back exact `data-blob' bytes via `data-addr'."
  (skip-unless (nelisp-aot-compiler-test--linux-p))
  (let ((cc (or (executable-find "cc") (executable-find "gcc"))))
    (skip-unless cc)
    (let* ((dir (make-temp-file "nelisp-doc06-blob-e2e" t))
           (obj (expand-file-name "blob.o" dir))
           (drv (expand-file-name "drv.c" dir))
           (bin (expand-file-name "prog" dir)))
      (unwind-protect
          (progn
            (nelisp-aot-compile-to-object
             '(seq
               (data-blob myblob (10 20 30 40 250 99 7 200) rodata)
               (defun getb (i) (ptr-read-u8 (data-addr myblob) i))
               (defun getword (i) (ptr-read-u32 (data-addr myblob) i)))
             obj)
            (with-temp-file drv
              (insert "#include <stdio.h>\n"
                      "extern long getb(long);\n"
                      "extern long getword(long);\n"
                      "int main(void){\n"
                      "  printf(\"%ld %ld %ld 0x%lx\\n\","
                      " getb(0), getb(4), getb(7), getword(0));\n"
                      "  return (getb(0)==10 && getb(4)==250 && getb(7)==200"
                      " && getword(0)==0x281e140aUL)?0:1;\n"
                      "}\n"))
            (should (zerop (call-process cc nil nil nil drv obj "-o" bin)))
            (let ((res (nelisp-aot-compiler-test--run-binary bin)))
              (should (zerop (plist-get res :exit)))
              (should (equal "10 250 200 0x281e140a"
                             (string-trim (plist-get res :stdout))))))
        (ignore-errors (delete-directory dir t))))))

;; --- Doc 122 §122.C/D: f64 immediate args + `extern-call-ptr' dynamic FFI ---

(ert-deftest nelisp-aot-compiler/extern-call-f64-immediate-arg-compiles ()
  "A float-literal arg `(:f64 0.07)' materialises into XMM rather than
signalling `:f64-leaf-shape-unsupported'.  Also pins the IEEE-754 encoder
against known `struct.pack(\"<d\", X)' bit patterns."
  (should (= (nelisp-aot-compiler--f64-imm-bits 0.07) 4589708452245819884))
  (should (= (nelisp-aot-compiler--f64-imm-bits 240.0) 4642648265865560064))
  (should (= (nelisp-aot-compiler--f64-imm-bits 0.0) 0))
  (let ((path (make-temp-file "nelisp-f64imm-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-to-object
           '(seq (defun probe (cr)
                   (extern-call cairo_set_source_rgb cr
                                (:f64 0.07) (:f64 0.86) (:f64 0.24))))
           path)
          (should (file-exists-p path)))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-aot-compiler/e2e-extern-call-ptr-f64-dynamic-sqrt ()
  "Doc 122 §122.D dynamic f64 FFI: `dlopen' libm, `dlsym' sqrt, then call it
through `extern-call-ptr-f64' with an f64 immediate arg (4.0) and read the f64
return (2.0).  Exercises indirect `call r11' + xmm0 arg/return + the
f64-immediate arg path end to end through a C driver linked with -ldl -lm."
  (skip-unless (nelisp-aot-compiler-test--linux-p))
  (let ((cc (or (executable-find "cc") (executable-find "gcc"))))
    (skip-unless cc)
    (let* ((dir (make-temp-file "nelisp-ecp-f64-e2e" t))
           (obj (expand-file-name "ecp.o" dir))
           (drv (expand-file-name "drv.c" dir))
           (bin (expand-file-name "prog" dir)))
      (unwind-protect
          (progn
            (nelisp-aot-compile-to-object
             '(seq
               (data-blob libm_name "libm.so.6\0" rodata)
               (data-blob sym_sqrt "sqrt\0" rodata)
               (defun probe ()
                 (let ((h (extern-call dlopen (data-addr libm_name) 2)))
                   (if (= h 0) 200
                     (let ((f (extern-call dlsym h (data-addr sym_sqrt))))
                       (if (= f 0) 201
                         (if (= (f64-bits (extern-call-ptr-f64 f (:f64 4.0)))
                                4611686018427387904)
                             0 1)))))))
             obj)
            (with-temp-file drv
              (insert "extern long probe(void);\n"
                      "int main(void){ return (int)probe(); }\n"))
            (should (zerop (call-process cc nil nil nil drv obj "-o" bin
                                         "-ldl" "-lm")))
            (let ((res (nelisp-aot-compiler-test--run-binary bin)))
              (should (= 0 (plist-get res :exit)))))
        (ignore-errors (delete-directory dir t))))))

(ert-deftest nelisp-aot-compiler/data-blob-data-and-bss-sections ()
  "`data-blob' honours its SECTION: `data' -> writable .data (PROGBITS),
`bss' -> .bss (NOBITS); both addressable via `data-addr' (Doc 06 C)."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-doc06-databss-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-to-object
           '(seq
             (data-blob dvar (1 0 0 0 0 0 0 0) data)
             (data-blob bvar (0 0 0 0 0 0 0 0) bss)
             (defun getd () (ptr-read-u64 (data-addr dvar) 0))
             (defun getbss () (ptr-read-u64 (data-addr bvar) 0)))
           path)
          (let ((secs (with-output-to-string
                        (with-current-buffer standard-output
                          (call-process "readelf" nil t nil "-S" path))))
                (syms (with-output-to-string
                        (with-current-buffer standard-output
                          (call-process "readelf" nil t nil "-s" path)))))
            (should (string-match-p "\\.data[ \t]+PROGBITS" secs))
            (should (string-match-p "\\.bss[ \t]+NOBITS" secs))
            (should (string-match-p "OBJECT  LOCAL  DEFAULT[ \t]+[0-9]+ dvar" syms))
            (should (string-match-p "OBJECT  LOCAL  DEFAULT[ \t]+[0-9]+ bvar" syms))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-aot-compiler/data-blob-data-bss-readwrite-e2e ()
  "A C driver reads/writes `data'- and `bss'-section blobs via `data-addr'."
  (skip-unless (nelisp-aot-compiler-test--linux-p))
  (let ((cc (or (executable-find "cc") (executable-find "gcc"))))
    (skip-unless cc)
    (let* ((dir (make-temp-file "nelisp-doc06-databss-e2e" t))
           (obj (expand-file-name "db.o" dir))
           (drv (expand-file-name "drv.c" dir))
           (bin (expand-file-name "prog" dir)))
      (unwind-protect
          (progn
            (nelisp-aot-compile-to-object
             '(seq
               (data-blob dvar (1 0 0 0 0 0 0 0) data)
               (data-blob bvar (0 0 0 0 0 0 0 0) bss)
               (defun getd () (ptr-read-u64 (data-addr dvar) 0))
               (defun setd (x) (ptr-write-u64 (data-addr dvar) 0 x))
               (defun getbss () (ptr-read-u64 (data-addr bvar) 0))
               (defun setbss (x) (ptr-write-u64 (data-addr bvar) 0 x)))
             obj)
            (with-temp-file drv
              (insert "#include <stdio.h>\n"
                      "extern long getd(void), getbss(void);\n"
                      "extern void setd(long), setbss(long);\n"
                      "int main(void){\n"
                      "  long d0=getd(), b0=getbss();\n"
                      "  setd(99); setbss(7);\n"
                      "  printf(\"%ld %ld %ld %ld\\n\", d0, b0, getd(), getbss());\n"
                      "  return (d0==1 && b0==0 && getd()==99 && getbss()==7)?0:1;\n"
                      "}\n"))
            (should (zerop (call-process cc nil nil nil drv obj "-o" bin)))
            (let ((res (nelisp-aot-compiler-test--run-binary bin)))
              (should (zerop (plist-get res :exit)))
              (should (equal "1 0 99 7" (string-trim (plist-get res :stdout))))))
        (ignore-errors (delete-directory dir t))))))

(ert-deftest nelisp-aot-compiler/data-blob-pointer-reloc-e2e ()
  "A pointer baked into a `.data' blob (Doc 06 C-2 RELOCS) is relocated by
the linker to point at a `.rodata' string blob and read back correctly."
  (skip-unless (nelisp-aot-compiler-test--linux-p))
  (let ((cc (or (executable-find "cc") (executable-find "gcc"))))
    (skip-unless cc)
    (let* ((dir (make-temp-file "nelisp-doc06-reloc-e2e" t))
           (obj (expand-file-name "r.o" dir))
           (drv (expand-file-name "drv.c" dir))
           (bin (expand-file-name "prog" dir)))
      (unwind-protect
          (progn
            ;; pmsg is an 8-byte .data pointer whose bytes are patched by an
            ;; abs64 reloc to the address of `msg' ("hi!\\0") in .rodata.
            (nelisp-aot-compile-to-object
             '(seq
               (data-blob msg (104 105 33 0) rodata)
               (data-blob pmsg (0 0 0 0 0 0 0 0) data ((0 msg 0)))
               (defun getmsg () (ptr-read-u64 (data-addr pmsg) 0)))
             obj)
            (with-temp-file drv
              (insert "#include <stdio.h>\n#include <string.h>\n"
                      "extern long getmsg(void);\n"
                      "int main(void){ char *p=(char*)getmsg();\n"
                      "  printf(\"%s\\n\", p);\n"
                      "  return strcmp(p,\"hi!\")==0?0:1; }\n"))
            (should (zerop (call-process cc nil nil nil drv obj "-o" bin)))
            (let ((res (nelisp-aot-compiler-test--run-binary bin)))
              (should (zerop (plist-get res :exit)))
              (should (equal "hi!" (string-trim (plist-get res :stdout))))))
        (ignore-errors (delete-directory dir t))))))

(ert-deftest nelisp-aot-compiler/data-blob-reloc-extern-symbol-e2e ()
  "A `.data' blob reloc to a symbol defined in NO blob/defun becomes an UND
extern (Doc 06 C-3 function-pointer tables), resolved by the linker."
  (skip-unless (nelisp-aot-compiler-test--linux-p))
  (let ((cc (or (executable-find "cc") (executable-find "gcc"))))
    (skip-unless cc)
    (let* ((dir (make-temp-file "nelisp-doc06-extreloc-e2e" t))
           (obj (expand-file-name "e.o" dir))
           (drv (expand-file-name "drv.c" dir))
           (bin (expand-file-name "prog" dir)))
      (unwind-protect
          (progn
            (nelisp-aot-compile-to-object
             '(seq
               (data-blob ptbl (0 0 0 0 0 0 0 0) data ((0 ext_fn 0)))
               (defun getfn () (ptr-read-u64 (data-addr ptbl) 0)))
             obj)
            ;; ext_fn must be emitted as an UND GLOBAL extern.
            (let ((syms (with-output-to-string
                          (with-current-buffer standard-output
                            (call-process "readelf" nil t nil "-s" obj)))))
              (should (string-match-p "GLOBAL[ \t]+DEFAULT[ \t]+UND ext_fn" syms)))
            (with-temp-file drv
              (insert "#include <stdio.h>\n"
                      "int ext_fn(int x){ return x+100; }\n"
                      "extern long getfn(void);\n"
                      "int main(void){ int(*f)(int)=(int(*)(int))getfn();\n"
                      "  printf(\"%d\\n\", f(5)); return f(5)==105?0:1; }\n"))
            (should (zerop (call-process cc nil nil nil drv obj "-o" bin)))
            (let ((res (nelisp-aot-compiler-test--run-binary bin)))
              (should (zerop (plist-get res :exit)))
              (should (equal "105" (string-trim (plist-get res :stdout))))))
        (ignore-errors (delete-directory dir t))))))

(provide 'nelisp-aot-compiler-test)

;;; nelisp-aot-compiler-test.el ends here
