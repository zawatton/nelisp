;;; wasm-cas-fixture.el --- Native/Wasm raw CAS conformance -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'json)
(require 'nelisp-aot-compiler)
(require 'nelisp-cc-atomic-raw-mem)
(defconst wasm-cas-values '(0 1 -1 -9223372036854775808 9223372036854775807 42 -7 81985529216486895))
(defconst wasm-cas-desired '(0 1 -9223372036854775808 9223372036854775807))
(defconst wasm-cas-output "target/wasm-cas/")
(defun wasm-cas-hash (file)
  (with-temp-buffer (insert-file-contents-literally file) (secure-hash 'sha256 (current-buffer))))
(defun wasm-cas-json (file object)
  (with-temp-file file (insert (json-encode object))))
(defun wasm-cas-cases ()
  (cl-loop for old in wasm-cas-values append
           (cl-loop for expected in wasm-cas-values append
                    (cl-loop for desired in wasm-cas-desired collect (list old expected desired)))))
(defun wasm-cas-definitions ()
  "Return the genuine provider and native-compatible operand probes."
  (cons nelisp-cc-atomic-raw-mem--compare-exchange-source
        '((defun cas_nested_ptr (outer inner)
            (atomic-compare-exchange (seq (atomic-compare-exchange inner 0 1) outer) 5 9))
          (defun cas_nested_expected (outer inner)
            (atomic-compare-exchange outer (+ 4 (atomic-compare-exchange inner 0 1)) 9))
          (defun cas_nested_desired (outer inner)
            (atomic-compare-exchange outer 5 (+ 8 (atomic-compare-exchange inner 0 1))))
          (defun cas_desired_changes_slot (ptr)
            (atomic-compare-exchange ptr 5 (seq (atomic-compare-exchange ptr 5 7) 9)))
          (defun cas_import_all ()
            (atomic-compare-exchange (extern-call audit_ptr) (extern-call audit_expected)
                                     (extern-call audit_desired))))))
(defun wasm-cas-wasm-definitions ()
  (append
   (wasm-cas-definitions)
   '((defun cas_ensure_memory () (alloc-bytes 16 8))
     (defun cas_catch_ptr ()
       (atomic-compare-exchange (catch 3 (throw 3 (extern-call audit_ptr)))
                                (extern-call audit_expected) (extern-call audit_desired)))
     (defun cas_catch_expected ()
       (atomic-compare-exchange (extern-call audit_ptr)
                                (catch 4 (throw 4 (extern-call audit_expected))) (extern-call audit_desired)))
     (defun cas_catch_desired ()
       (atomic-compare-exchange (extern-call audit_ptr) (extern-call audit_expected)
                                (catch 5 (throw 5 (extern-call audit_desired)))))
     (defun cas_catch_all ()
       (atomic-compare-exchange (catch 3 (throw 3 (extern-call audit_ptr)))
                                (catch 4 (throw 4 (extern-call audit_expected)))
                                (catch 5 (throw 5 (extern-call audit_desired)))))
     (defun cas_throw_ptr ()
       (catch 22 (atomic-compare-exchange (throw 22 90) (extern-call audit_expected) (extern-call audit_desired))))
     (defun cas_throw_expected ()
       (catch 33 (atomic-compare-exchange (extern-call audit_ptr) (throw 33 91) (extern-call audit_desired))))
     (defun cas_throw_desired ()
       (catch 44 (atomic-compare-exchange (extern-call audit_ptr) (extern-call audit_expected) (throw 44 92)))))))
(defun wasm-cas-build-native ()
  "Build one native object and C scalar/assertion driver; C never performs CAS."
  (make-directory wasm-cas-output t)
  (let* ((object (concat wasm-cas-output "native.o")) (driver (concat wasm-cas-output "native.c"))
         (binary (concat wasm-cas-output "native-oracle"))
         (provider (symbol-file 'nelisp-aot-compiler--emit-atomic-compare-exchange 'defun)))
    (nelisp-aot-compile-to-object `(seq ,@(wasm-cas-definitions)) object)
    (with-temp-file driver
      (insert "#include <stdint.h>\n#include <stdio.h>\n#include <string.h>\n"
              "extern int64_t nelisp_atomic_compare_exchange(int64_t,int64_t,int64_t);\n"
              "extern int64_t cas_import_all(void);\n")
      (dolist (name '(cas_nested_ptr cas_nested_expected cas_nested_desired))
        (insert (format "extern int64_t %s(int64_t,int64_t);\n" name)))
      (insert "extern int64_t cas_desired_changes_slot(int64_t);\n"
              "static int64_t a,b;static int n;static char trace[4];\n"
              "int64_t audit_ptr(void){trace[n++]='P';return (int64_t)(uintptr_t)&a;}\n"
              "int64_t audit_expected(void){trace[n++]='E';return 5;}\n"
              "int64_t audit_desired(void){trace[n++]='D';return 9;}\nint main(void){\n")
      (dolist (row (wasm-cas-cases))
        (pcase-let ((`(,old ,expected ,desired) row))
          (insert (format "a=(int64_t)UINT64_C(0x%x);if(nelisp_atomic_compare_exchange((int64_t)(uintptr_t)&a,(int64_t)UINT64_C(0x%x),(int64_t)UINT64_C(0x%x))!=%d||a!=(int64_t)UINT64_C(0x%x))return 1;\n"
                          (logand old #xffffffffffffffff) (logand expected #xffffffffffffffff)
                          (logand desired #xffffffffffffffff) (if (= old expected) 1 0)
                          (logand (if (= old expected) desired old) #xffffffffffffffff)))))
      (insert "a=5;n=0;if(cas_import_all()!=1||a!=9||n!=3||memcmp(trace,\"PED\",3))return 2;\n")
      (dolist (name '(cas_nested_ptr cas_nested_expected cas_nested_desired))
        (insert (format "a=5;b=0;if(%s((int64_t)(uintptr_t)&a,(int64_t)(uintptr_t)&b)!=1||a!=9||b!=1)return 3;\n" name)))
      (insert "a=5;if(cas_desired_changes_slot((int64_t)(uintptr_t)&a)!=0||a!=7)return 4;\n"
              "puts(\"native-cas=256 order=PED nested=4\");return 0;}\n"))
    (unless (zerop (call-process (or (executable-find "cc") (error "cc required")) nil t nil
                                "-no-pie" driver object "-o" binary)) (error "Native CAS oracle link failed"))
    (wasm-cas-json
     (concat wasm-cas-output "native-inputs.json")
     `((schema_version . 1)
       (cases . ,(vconcat (mapcar (lambda (row) (vconcat (mapcar #'number-to-string row))) (wasm-cas-cases))))
       (native_sha256 . ,(wasm-cas-hash binary)) (native_object_sha256 . ,(wasm-cas-hash object))
       (native_driver_sha256 . ,(wasm-cas-hash driver))
       (compiler_provider . ,(file-relative-name provider)) (compiler_sha256 . ,(wasm-cas-hash provider))
       (expected_stdout . "native-cas=256 order=PED nested=4\n")))))
(defun wasm-cas-build-wasm ()
  "Build genuine Wasm CAS and bind it to exact source/native corpus identities."
  (let ((path (or (getenv "WASM_CAS_OUTPUT") (concat wasm-cas-output "fixture.wasm"))))
    (make-directory (file-name-directory path) t)
    (nelisp-aot-compile-to-object `(seq ,@(wasm-cas-wasm-definitions)) path :arch 'wasm32 :format 'wasm)
    (wasm-cas-json
     (concat path ".json")
     `((schema_version . 1) (wasm_sha256 . ,(wasm-cas-hash path))
       (native_inputs_sha256 . ,(wasm-cas-hash (concat wasm-cas-output "native-inputs.json")))
       (source_sha256 . ,(vconcat
                          (mapcar (lambda (file) (vector file (wasm-cas-hash file)))
                                  '("lisp/nelisp-aot-compiler.el" "lisp/nelisp-asm-wasm.el"
                                    "lisp/nelisp-cc-atomic-raw-mem.el" "tools/ai/wasm-cas-fixture.el"))))))))
(provide 'wasm-cas-fixture)
