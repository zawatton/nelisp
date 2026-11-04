;;; wasm-runtime-ops-fixture.el --- Atomic and bitwise Wasm fixture -*- lexical-binding: t; -*-

(require 'nelisp-aot-compiler)

(defconst wasm-runtime-ops-fixture--atomic
  '(seq
    (defun ensure-memory () (alloc-bytes 8 8))
    (defun atomic-add (ptr delta) (atomic-fetch-add ptr delta))
    (defun operand-order ()
      (seq
       (ptr-write-u64 1024 0 0)
       (ptr-write-u64 1040 0 10)
       (let ((old (atomic-fetch-add
                    (seq (ptr-write-u64 1024 0 (+ (* 10 (ptr-read-u64 1024 0)) 1))
                         1040)
                    (seq (ptr-write-u64 1024 0 (+ (* 10 (ptr-read-u64 1024 0)) 2))
                         3))))
         (+ (* old 100) (ptr-read-u64 1024 0)))))
    (defun nested-add (ptr)
      (atomic-fetch-add ptr (atomic-fetch-add ptr 2)))
    (defun invalid-order (ptr)
      (atomic-fetch-add
       (seq (ptr-write-u64 1024 0 1) ptr)
       (seq (ptr-write-u64 1024 0 (+ (* 10 (ptr-read-u64 1024 0)) 2)) 1))))
  "Native atomic contract; JavaScript supplies only memory data and assertions.")

(defconst wasm-runtime-ops-fixture--bitwise
  '(seq
    (defun bit-and (a b) (logand a b))
    (defun bit-or (a b) (logior a b))
    (defun bit-xor (a b) (logxor a b))
    (defun bit-and-zero () (logand))
    (defun bit-or-zero () (logior))
    (defun bit-xor-zero () (logxor))
    (defun bit-and-one (a) (logand a))
    (defun bit-or-one (a) (logior a))
    (defun bit-xor-one (a) (logxor a))
    (defun bit-and-three (a b c) (logand a b c))
    (defun bit-or-three (a b c) (logior a b c))
    (defun bit-xor-three (a b c) (logxor a b c)))
  "GNU-compatible zero identities and current unary/n-ary parser arities.")

(defun wasm-runtime-ops-fixture-build ()
  "Compile the selected genuine runtime operation fixture."
  (let* ((kind (or (getenv "WASM_RUNTIME_OPS_KIND") "atomic"))
         (source (pcase kind
                   ("atomic" wasm-runtime-ops-fixture--atomic)
                   ("bitwise" wasm-runtime-ops-fixture--bitwise)
                   (_ (error "Unknown operation fixture %s" kind))))
         (path (or (getenv "WASM_RUNTIME_OPS_OUTPUT")
                   (format "target/wasm-runtime-ops/%s.wasm" kind))))
    (make-directory (file-name-directory path) t)
    (nelisp-aot-compile-to-object source path :arch 'wasm32 :format 'wasm)
    (princ (format "fixture=%s\n" path))))

(provide 'wasm-runtime-ops-fixture)
;;; wasm-runtime-ops-fixture.el ends here
