;;; wasm-sexp-tag-fixture.el --- Executed tagged-value Wasm fixture -*- lexical-binding: t; -*-

(require 'nelisp-aot-compiler)

(defconst wasm-sexp-tag-fixture--source
  '(seq
    (defun tag-word (value) (sexp-tag value))
    (defun boxed-tag (tag)
      (let ((slot (alloc-bytes 32 8)))
        (seq (ptr-write-u8 slot 0 tag) (sexp-tag slot))))
    (defun once-tag ()
      (seq
       (ptr-write-u64 1024 0 0)
       (let ((tag (sexp-tag
                   (seq (ptr-write-u64 1024 0 (+ (ptr-read-u64 1024 0) 1))
                        169))))
         (+ (* tag 100) (ptr-read-u64 1024 0)))))
    (defun nested-tag (value)
      (+ (* 100 (sexp-tag value))
         (sexp-tag (seq (sexp-tag 7) 169)))))
  "Actual runtime value words and boxed tag reads, without a host provider.")

(defun wasm-sexp-tag-fixture-build ()
  "Compile the fixture, leaving unsupported instructions as a nonzero exit."
  (let ((path (or (getenv "WASM_SEXP_TAG_OUTPUT")
                  "target/wasm-sexp-tag/fixture.wasm")))
    (make-directory (file-name-directory path) t)
    (nelisp-aot-compile-to-object wasm-sexp-tag-fixture--source path
                                  :arch 'wasm32 :format 'wasm)
    (princ (format "fixture=%s\n" path))))

(provide 'wasm-sexp-tag-fixture)
;;; wasm-sexp-tag-fixture.el ends here
