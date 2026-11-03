;;; nelisp-aot-wasm-genuine-alloc-test.el --- Opt-in allocation tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'nelisp-aot-compiler)
(require 'nelisp-asm-wasm)

(defun nelisp-test-genuine-alloc--compile (source mode)
  "Compile SOURCE with MODE, returning its exact bytes."
  (let* ((directory (expand-file-name "target/wasm-genuine-alloc"))
         (_ (make-directory directory t))
         (path (make-temp-file (expand-file-name "ert-" directory) nil ".wasm"))
         (nelisp-aot-compiler-wasm-genuine-alloc mode))
    (unwind-protect
        (progn
          (nelisp-aot-compile-to-object source path :arch 'wasm32 :format 'wasm)
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally path) (buffer-string)))
      (delete-file path))))

(ert-deftest nelisp-wasm-genuine-alloc-default-byte-identity ()
  ;; Captured from the pre-change compiler, including its entire inline bump.
  (should
   (equal (nelisp-test-genuine-alloc--compile '(seq (defun allocate () (alloc-bytes 24 8))) nil)
          (base64-decode-string
           "AGFzbQEAAAABBQFgAAF+AwIBAAQEAXAAAQUDAQABBggBfwFBgIAECwcgAwZtZW1vcnkCAAhoZWFwX3B0cgMACGFsbG9jYXRlAAAJBwEAQQALAQAKSgFIAhN+AX8jAEIIp2pBAWtBAEIIp2txIhNCGKdqJAAjAD8AQRB0SwRAAkADQEEBQABBf0YEQAALIwA/AEEQdEsNAAsLCyATrQ8L"))))

(ert-deftest nelisp-wasm-genuine-alloc-direct-call ()
  (let ((original (symbol-function 'nelisp-asm-wasm-op-call)) calls)
    (cl-letf (((symbol-function 'nelisp-asm-wasm-op-call)
               (lambda (buffer index) (push index calls) (funcall original buffer index))))
      (nelisp-test-genuine-alloc--compile
       '(seq (defun nl_alloc_bytes (size align) (+ size align))
             (defun allocate () (alloc-bytes 24 8))) t))
    (should (equal calls '(0)))))

(ert-deftest nelisp-wasm-genuine-alloc-missing-allocator ()
  (should (equal (should-error (nelisp-test-genuine-alloc--compile
				'(defun allocate () (alloc-bytes 24 8)) t)
                               :type 'nelisp-aot-compiler-error)
                 '(nelisp-aot-compiler-error :wasm-genuine-alloc-missing nl_alloc_bytes))))

(ert-deftest nelisp-wasm-genuine-alloc-recursion-direct ()
  (should (equal (should-error (nelisp-test-genuine-alloc--compile
				'(defun nl_alloc_bytes (size align) (alloc-bytes size align)) t)
                               :type 'nelisp-aot-compiler-error)
                 '(nelisp-aot-compiler-error :wasm-genuine-alloc-recursion nl_alloc_bytes nl_alloc_bytes))))

(ert-deftest nelisp-wasm-genuine-alloc-recursion-transitive ()
  (should (equal (should-error (nelisp-test-genuine-alloc--compile
				'(seq (defun nl_alloc_bytes (size align) (helper size align))
                                      (defun helper (size align) (alloc-bytes size align))) t)
                               :type 'nelisp-aot-compiler-error)
                 '(nelisp-aot-compiler-error :wasm-genuine-alloc-recursion helper nl_alloc_bytes))))

(ert-deftest nelisp-wasm-genuine-alloc-second-raw-reservation ()
  (let ((nelisp-aot-compiler-wasm-genuine-alloc-raw-reservers '(nl_wasm_arena_init)))
    (should (equal (should-error (nelisp-test-genuine-alloc--compile
				  '(seq (defun nl_alloc_bytes (size align) (+ size align))
					(defun nl_wasm_arena_init ()
					  (seq (alloc-bytes 4096 4096) (alloc-bytes 4096 4096)))) t)
				 :type 'nelisp-aot-compiler-error)
                   '(nelisp-aot-compiler-error :wasm-genuine-alloc-raw-reservation nl_wasm_arena_init 2)))))

(ert-deftest nelisp-wasm-genuine-alloc-one-raw-reservation ()
  (let ((nelisp-aot-compiler-wasm-genuine-alloc-raw-reservers '(nl_wasm_arena_init)))
    (let ((original (symbol-function 'nelisp-asm-wasm-op-call)) calls)
      (cl-letf (((symbol-function 'nelisp-asm-wasm-op-call)
		 (lambda (buffer index) (push index calls) (funcall original buffer index))))
	(nelisp-test-genuine-alloc--compile
	 '(seq (defun nl_alloc_bytes (size align) (+ size align))
               (defun nl_wasm_arena_init () (alloc-bytes 4096 4096))
               (defun allocate () (alloc-bytes 24 8))) t))
      (should (equal calls '(0))))))

(ert-deftest nelisp-wasm-genuine-alloc-cannot-hide-raw-in-closure ()
  (let ((nelisp-aot-compiler-wasm-genuine-alloc-raw-reservers '(nl_wasm_arena_init)))
    (should-error (nelisp-test-genuine-alloc--compile
                   '(seq (defun nl_alloc_bytes (size align) (nl_wasm_arena_init))
			 (defun nl_wasm_arena_init () (alloc-bytes 4096 4096))) t)
                  :type 'nelisp-aot-compiler-error)))

(ert-deftest nelisp-wasm-genuine-alloc-dealloc-unchanged ()
  (let ((source '(seq (defun nl_alloc_bytes (size align) (+ size align))
                      (defun release (p) (dealloc-bytes p 24 8)))))
    (should (equal (nelisp-test-genuine-alloc--compile source nil)
                   (nelisp-test-genuine-alloc--compile source t)))))

(ert-deftest nelisp-wasm-genuine-alloc-duplicate-raw-owner ()
  (let ((nelisp-aot-compiler-wasm-genuine-alloc-raw-reservers '(nl_wasm_arena_init)))
    (should-error (nelisp-test-genuine-alloc--compile
                   '(seq (defun nl_alloc_bytes (size align) (+ size align))
			 (defun nl_wasm_arena_init () (alloc-bytes 4096 4096))
			 (defun nl_wasm_arena_init () (alloc-bytes 4096 4096))) t)
                  :type 'nelisp-aot-compiler-error)))

(provide 'nelisp-aot-wasm-genuine-alloc-test)
