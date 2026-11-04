;;; nelisp-aot-wasm-genuine-alloc-rev2-test.el --- Boundary regressions -*- lexical-binding: t; -*-
(require 'nelisp-aot-wasm-genuine-alloc-test)

(ert-deftest nelisp-wasm-genuine-alloc-external-bounce ()
  (should (equal (should-error
                  (nelisp-test-genuine-alloc--compile
                   '(seq (defun nl_alloc_bytes (size align) (extern-call bounce size align))
                         (defun allocate () (alloc-bytes 24 8))) t)
                  :type 'nelisp-aot-compiler-error)
                 '(nelisp-aot-compiler-error :wasm-genuine-alloc-external bounce))))

(ert-deftest nelisp-wasm-genuine-alloc-external-transitive ()
  (should (equal (should-error
                  (nelisp-test-genuine-alloc--compile
                   '(seq (defun nl_alloc_bytes (size align) (helper size align))
                         (defun helper (size align) (extern-call bounce size align))) t)
                  :type 'nelisp-aot-compiler-error)
                 '(nelisp-aot-compiler-error :wasm-genuine-alloc-external bounce))))

(ert-deftest nelisp-wasm-genuine-alloc-external-local-name ()
  (should (equal (should-error
                  (nelisp-test-genuine-alloc--compile
                   '(seq (defun nl_alloc_bytes (size align) (extern-call bounce size align))
                         (defun bounce (size align) (+ size align))) t)
                  :type 'nelisp-aot-compiler-error)
                 '(nelisp-aot-compiler-error :wasm-genuine-alloc-external bounce))))

(ert-deftest nelisp-wasm-genuine-alloc-imports ()
  (dolist (form '((wasi-call fd_write 1 0 0 0)
                  (syscall-direct 1 1 0 0 0 0 0)))
    (should (equal (should-error
                    (nelisp-test-genuine-alloc--compile
                     `(seq (defun nl_alloc_bytes (size align) (helper size align))
                           (defun helper (size align) ,form)) t)
                    :type 'nelisp-aot-compiler-error)
                   '(nelisp-aot-compiler-error :wasm-genuine-alloc-external fd_write)))))

(ert-deftest nelisp-wasm-genuine-alloc-external-outside-closure ()
  (let ((source '(seq (defun nl_alloc_bytes (size align) (+ size align))
                      (defun unused () (extern-call bounce 24 8)))))
    (should (equal (nelisp-test-genuine-alloc--compile source nil)
                   (nelisp-test-genuine-alloc--compile source t)))))

(ert-deftest nelisp-wasm-genuine-alloc-wrong-arity ()
  (dolist (params '(() (size) (size align extra)))
    (should (equal (should-error
                    (nelisp-test-genuine-alloc--compile `(defun nl_alloc_bytes ,params 0) t)
                    :type 'nelisp-aot-compiler-error)
                   '(nelisp-aot-compiler-error :wasm-genuine-alloc-arity nl_alloc_bytes)))))

(ert-deftest nelisp-wasm-genuine-alloc-recursion-indirect ()
  (dolist (form '((call-ptr size size align) (extern-call-ptr size size align)))
    (should (equal (should-error
                    (nelisp-test-genuine-alloc--compile
                     `(seq (defun nl_alloc_bytes (size align) (helper size align))
                           (defun helper (size align) ,form)) t)
                    :type 'nelisp-aot-compiler-error)
                   '(nelisp-aot-compiler-error :wasm-genuine-alloc-recursion helper nl_alloc_bytes)))))

(ert-deftest nelisp-wasm-genuine-alloc-recursion-literal-slots ()
  ;; Exercise both public literal-slot forms (IR tags 90 and 91).
  (dolist (form '((sexp-write-symbol-lit size "symbol") (sexp-write-str-lit size "string")))
    (should (equal (should-error
                    (nelisp-test-genuine-alloc--compile
                     `(seq (defun nl_alloc_bytes (size align) (helper size align))
                           (defun helper (size align) ,form)) t)
                    :type 'nelisp-aot-compiler-error)
                   '(nelisp-aot-compiler-error :wasm-genuine-alloc-recursion helper nl_alloc_bytes)))))

(ert-deftest nelisp-wasm-genuine-alloc-no-implicit-raw-owner ()
  (let ((nelisp-aot-compiler-wasm-genuine-alloc-raw-reservers nil)
        (original (symbol-function 'nelisp-asm-wasm-op-call)) calls)
    (cl-letf (((symbol-function 'nelisp-asm-wasm-op-call)
               (lambda (buffer index) (push index calls) (funcall original buffer index))))
      (nelisp-test-genuine-alloc--compile
       '(seq (defun nl_alloc_bytes (size align) (+ size align))
             (defun nl_wasm_arena_init () (alloc-bytes 4096 4096))) t))
    (should (equal calls '(0)))))

(ert-deftest nelisp-wasm-genuine-alloc-configured-raw-owners ()
  (let ((nelisp-aot-compiler-wasm-genuine-alloc-raw-reservers '(reserve_one reserve_two))
        (original (symbol-function 'nelisp-asm-wasm-op-call)) calls)
    (cl-letf (((symbol-function 'nelisp-asm-wasm-op-call)
               (lambda (buffer index) (push index calls) (funcall original buffer index))))
      (nelisp-test-genuine-alloc--compile
       '(seq (defun nl_alloc_bytes (size align) (+ size align))
             (defun reserve_one () (alloc-bytes 4096 4096))
             (defun reserve_two () (alloc-bytes 4096 4096))) t))
    (should-not calls)))

(ert-deftest nelisp-wasm-genuine-alloc-raw-owner-requires-one-site ()
  (let ((nelisp-aot-compiler-wasm-genuine-alloc-raw-reservers '(reserve)))
    (dolist (body '(0 (seq (alloc-bytes 8 8) (alloc-bytes 16 8))
                      (sexp-write-str-lit 1024 "string")))
      (should (equal (cadr (should-error
                            (nelisp-test-genuine-alloc--compile
                             `(seq (defun nl_alloc_bytes (size align) (+ size align))
                                   (defun reserve () ,body)) t)
                            :type 'nelisp-aot-compiler-error))
                     :wasm-genuine-alloc-raw-reservation)))))

(ert-deftest nelisp-wasm-genuine-alloc-runtime-argument-recorder ()
  (let* ((directory (expand-file-name "target/wasm-genuine-alloc"))
         (_ (make-directory directory t))
         (path (make-temp-file (expand-file-name "argument-recorder-" directory) nil ".wasm"))
         (script
          "const fs=require('fs'),assert=require('assert/strict');
const size=0x100000029n,align=0x200000008n;
const {instance}=await WebAssembly.instantiate(fs.readFileSync(process.argv[1]));
const result=instance.exports.allocate();
const memory=new DataView(instance.exports.memory.buffer);
assert.equal(memory.getBigUint64(1040,true),12n);
assert.equal(memory.getBigUint64(1024,true),size);
assert.equal(memory.getBigUint64(1032,true),align);
assert.equal(result,size); console.log('argument-recorder=size,align once; i64 unchanged');"))
    (unwind-protect
        (cl-labels
            ((run (size align)
               (let ((bytes (nelisp-test-genuine-alloc--compile
                             `(seq (defun next_size ()
                                     (seq (ptr-write-u64 1040 0 (+ (* (ptr-read-u64 1040 0) 10) 1))
                                          4294967337))
                                   (defun next_align ()
                                     (seq (ptr-write-u64 1040 0 (+ (* (ptr-read-u64 1040 0) 10) 2))
                                          8589934600))
                                   (defun nl_alloc_bytes (size align)
                                     (seq (ptr-write-u64 1024 0 size)
                                          (ptr-write-u64 1024 8 align) size))
                                   (defun allocate () (alloc-bytes ,size ,align))) t)))
                 (with-temp-file path (set-buffer-multibyte nil) (insert bytes))
                 (with-temp-buffer
                   (let ((rc (call-process "node" nil t nil "--disable-wasm-trap-handler"
                                           "--input-type=commonjs" "-e"
                                           (concat "(async()=>{" script "})().catch(e=>{console.error(e);process.exitCode=1})") path)))
                     (cons rc (buffer-string)))))))
          (let ((result (run '(next_size) '(next_align))))
            (should (equal (car result) 0))
            (should (string-match-p "argument-recorder=size,align once; i64 unchanged" (cdr result)))
            (princ (cdr result)))
          ;; Negative controls: the recorder must catch order, count and width defects.
          (dolist (arguments '(((next_align) (next_size))
                               ((seq (next_size) (next_size)) (next_align))
                               ((logand (next_size) 4294967295) (next_align))))
            (let ((result (run (car arguments) (cadr arguments))))
              (should (equal (car result) 1))
              (should (string-match-p "AssertionError" (cdr result)))
              (princ (format "argument-recorder-control=%S rc=1 AssertionError\n" arguments)))))
      (delete-file path))))

(provide 'nelisp-aot-wasm-genuine-alloc-rev2-test)
