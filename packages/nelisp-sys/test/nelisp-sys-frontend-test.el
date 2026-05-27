;;; nelisp-sys-frontend-test.el --- ERT tests for nelisp-sys frontend -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Stage 131.1 gate: parser/AST tests for representative source forms,
;; including the package fixtures.

;;; Code:

(require 'ert)
(require 'nelisp-sys-ast)
(require 'nelisp-sys-frontend)

(defconst nelisp-sys-frontend-test--dir
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "Directory of this test file, captured at load time.")

(defun nelisp-sys-frontend-test--read-forms (basename)
  "Read all s-expressions from fixtures/BASENAME."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name (concat "fixtures/" basename)
                       nelisp-sys-frontend-test--dir))
    (goto-char (point-min))
    (let ((forms '()))
      (condition-case nil
          (while t (push (read (current-buffer)) forms))
        (end-of-file nil))
      (nreverse forms))))

;;; AST basics.

(ert-deftest nelisp-sys-ast-construct-and-access ()
  (let ((n (nelisp-sys-ast-make 'int :value 42)))
    (should (nelisp-sys-ast-node-p n))
    (should (eq 'int (nelisp-sys-ast-kind n)))
    (should (= 42 (nelisp-sys-ast-prop n :value)))
    (should (eq 'fallback (nelisp-sys-ast-prop n :missing 'fallback)))
    (let ((n2 (nelisp-sys-ast-set-prop n :value 7)))
      (should (= 7 (nelisp-sys-ast-prop n2 :value)))
      (should (= 42 (nelisp-sys-ast-prop n :value)))))) ; non-destructive

;;; Top-level forms.

(ert-deftest nelisp-sys-frontend-parse-defstruct ()
  (let ((n (nelisp-sys-frontend-parse-toplevel
            '(sys:defstruct point (:repr c) (x i32) (y i32)))))
    (should (eq 'defstruct (nelisp-sys-ast-kind n)))
    (should (eq 'point (nelisp-sys-ast-prop n :name)))
    (should (eq 'c (nelisp-sys-ast-prop n :repr)))
    (should (equal '((x . i32) (y . i32)) (nelisp-sys-ast-prop n :fields)))))

(ert-deftest nelisp-sys-frontend-parse-defun-effects ()
  (let ((n (nelisp-sys-frontend-parse-toplevel
            '(sys:defun add ((a i32) (b i32)) i32
               (:abi c :export "nl_add" :alloc none :panic abort)
               (+ a b)))))
    (should (eq 'defun (nelisp-sys-ast-kind n)))
    (should (eq 'add (nelisp-sys-ast-prop n :name)))
    (should (equal '((a . i32) (b . i32)) (nelisp-sys-ast-prop n :params)))
    (should (eq 'i32 (nelisp-sys-ast-prop n :ret)))
    (should (eq 'c (nelisp-sys-ast-prop n :abi)))
    (should (eq :export (nelisp-sys-ast-prop n :linkage)))
    (should (string= "nl_add" (nelisp-sys-ast-prop n :export-name)))
    (let ((eff (nelisp-sys-ast-prop n :effects)))
      (should (eq 'none (plist-get eff :alloc)))
      (should (eq 'abort (plist-get eff :panic)))
      (should (eq 'none (plist-get eff :unsafe))))
    (let ((body (nelisp-sys-ast-prop n :body)))
      (should (= 1 (length body)))
      (should (eq 'binop (nelisp-sys-ast-kind (car body))))
      (should (eq '+ (nelisp-sys-ast-prop (car body) :op))))))

(ert-deftest nelisp-sys-frontend-parse-defun-default-effects ()
  (let* ((n (nelisp-sys-frontend-parse-toplevel
             '(sys:defun f () void () (seq))))
         (eff (nelisp-sys-ast-prop n :effects)))
    (should (eq 'heap (plist-get eff :alloc)))
    (should (eq :private (nelisp-sys-ast-prop n :linkage)))))

(ert-deftest nelisp-sys-frontend-parse-extern ()
  (let ((n (nelisp-sys-frontend-parse-toplevel
            '(sys:extern memcpy
               (:symbol "memcpy" :abi c :unsafe t)
               ((dst (ptr u8)) (src (ptr u8)) (n usize))
               (ptr u8)
               (:alloc none :panic none)))))
    (should (eq 'extern (nelisp-sys-ast-kind n)))
    (should (string= "memcpy" (nelisp-sys-ast-prop n :symbol)))
    (should (eq t (nelisp-sys-ast-prop n :unsafe)))
    (should (equal '(ptr u8) (nelisp-sys-ast-prop n :ret)))
    (should (= 3 (length (nelisp-sys-ast-prop n :params))))))

(ert-deftest nelisp-sys-frontend-parse-const-and-assert ()
  (let ((c (nelisp-sys-frontend-parse-toplevel '(sys:const MAX i32 255)))
        (a (nelisp-sys-frontend-parse-toplevel
            '(sys:static-assert (= (sys:sizeof i32) 4) "i32 is 4 bytes"))))
    (should (eq 'const (nelisp-sys-ast-kind c)))
    (should (eq 'i32 (nelisp-sys-ast-prop c :type)))
    (should (eq 'static-assert (nelisp-sys-ast-kind a)))
    (should (string= "i32 is 4 bytes" (nelisp-sys-ast-prop a :message)))))

;;; Expression forms.

(ert-deftest nelisp-sys-frontend-parse-let-and-body ()
  (let* ((n (nelisp-sys-frontend--parse-expr
             '(let ((x i32 5) (y i32 (+ x 1)))
                (* x y))))
         (binds (nelisp-sys-ast-prop n :bindings)))
    (should (eq 'let (nelisp-sys-ast-kind n)))
    (should (= 2 (length binds)))
    (should (eq 'x (car (nth 0 binds))))
    (should (eq 'i32 (nth 1 (nth 0 binds))))
    (should (nelisp-sys-ast-node-p (nth 2 (nth 0 binds))))))

(ert-deftest nelisp-sys-frontend-parse-control-flow ()
  (let ((iff (nelisp-sys-frontend--parse-expr '(if (< a b) a b)))
        (cnd (nelisp-sys-frontend--parse-expr '(cond ((< a 0) 0) (else a))))
        (whl (nelisp-sys-frontend--parse-expr '(while (< i n) (set! i (+ i 1))))))
    (should (eq 'if (nelisp-sys-ast-kind iff)))
    (should (nelisp-sys-ast-node-p (nelisp-sys-ast-prop iff :cond)))
    (should (eq 'cond (nelisp-sys-ast-kind cnd)))
    (should (eq 'else (car (nth 1 (nelisp-sys-ast-prop cnd :clauses)))))
    (should (eq 'while (nelisp-sys-ast-kind whl)))))

(ert-deftest nelisp-sys-frontend-parse-unsafe-and-borrow ()
  (let ((u (nelisp-sys-frontend--parse-expr
            '(sys:unsafe (:reason "validated") (sys:store! p 1))))
        (b (nelisp-sys-frontend--parse-expr
            '(sys:with-borrow-mut ((q (&mut point) pv))
               (sys:store-field! q x 10)))))
    (should (eq 'unsafe (nelisp-sys-ast-kind u)))
    (should (string= "validated" (nelisp-sys-ast-prop u :reason)))
    (should (eq 'ptr-store! (nelisp-sys-ast-kind (car (nelisp-sys-ast-prop u :body)))))
    (should (eq 'with-borrow (nelisp-sys-ast-kind b)))
    (should (eq t (nelisp-sys-ast-prop b :mut)))
    (should (eq 'q (nelisp-sys-ast-prop b :var)))
    (should (equal '(&mut point) (nelisp-sys-ast-prop b :ref-type)))))

(ert-deftest nelisp-sys-frontend-parse-memory-ops ()
  (should (eq 'load-field
              (nelisp-sys-ast-kind
               (nelisp-sys-frontend--parse-expr '(sys:load-field p x)))))
  (should (eq 'slice-len
              (nelisp-sys-ast-kind
               (nelisp-sys-frontend--parse-expr '(sys:slice-len s)))))
  (let ((r (nelisp-sys-frontend--parse-expr '(sys:slice-ref-raw s i))))
    (should (eq 'slice-ref (nelisp-sys-ast-kind r)))
    (should (eq t (nelisp-sys-ast-prop r :raw))))
  (should (eq 'sizeof
              (nelisp-sys-ast-kind
               (nelisp-sys-frontend--parse-expr '(sys:sizeof (struct point))))))
  (should (eq 'call
              (nelisp-sys-ast-kind
               (nelisp-sys-frontend--parse-expr '(helper 1 2 3))))))

;;; Error paths.

(ert-deftest nelisp-sys-frontend-rejects-bad-forms ()
  (should-error (nelisp-sys-frontend-parse-toplevel '(sys:bogus x))
                :type 'nelisp-sys-parse-error)
  (should-error (nelisp-sys-frontend-parse-toplevel
                 '(sys:defstruct s (:repr packed) (a i8)))
                :type 'nelisp-sys-parse-error)
  (should-error (nelisp-sys-frontend--parse-expr "a string")
                :type 'nelisp-sys-parse-error)
  ;; a bare name is a valid forward named-type ref at parse time; only a
  ;; structurally malformed type is rejected by the frontend.
  (should-error (nelisp-sys-frontend-parse-toplevel
                 '(sys:defun f ((a (array i32 -1))) void () (seq)))
                :type 'nelisp-sys-parse-error))

;;; Fixtures.

(ert-deftest nelisp-sys-frontend-parses-add-fixture ()
  (let* ((forms (nelisp-sys-frontend-test--read-forms "add.nl"))
         (mod (nelisp-sys-frontend-parse-module forms))
         (items (nelisp-sys-ast-prop mod :items)))
    (should (= 1 (length items)))
    (should (eq 'defun (nelisp-sys-ast-kind (car items))))
    (should (eq 'add (nelisp-sys-ast-prop (car items) :name)))))

(ert-deftest nelisp-sys-frontend-parses-struct-fixture ()
  (let* ((forms (nelisp-sys-frontend-test--read-forms "struct.nl"))
         (mod (nelisp-sys-frontend-parse-module forms))
         (items (nelisp-sys-ast-prop mod :items)))
    (should (= 2 (length items)))
    (should (eq 'defstruct (nelisp-sys-ast-kind (nth 0 items))))
    (should (eq 'defun (nelisp-sys-ast-kind (nth 1 items))))
    (should (eq 'distance2 (nelisp-sys-ast-prop (nth 1 items) :name)))))

;;; nelisp-sys-frontend-test.el ends here
