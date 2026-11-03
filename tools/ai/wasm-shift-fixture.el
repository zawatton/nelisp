;;; wasm-shift-fixture.el --- Raw shift native/Wasm conformance -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'nelisp-aot-compiler)

(defconst wasm-shift-fixture--values
  '(0 1 -1 -8 9223372036854775807 -9223372036854775808
      81985529216486895 -81985529216486895))
(defconst wasm-shift-fixture--counts
  '(0 1 7 63 64 65 127 128 -1 -64 9223372036854775807 -9223372036854775808))
(defconst wasm-shift-fixture--ops '(shl sar shr))

(defun wasm-shift-fixture--cases ()
  "Return raw input rows, including nested scratch-local pressure."
  (cl-loop for op in '(shl sar shr nested)
           append (cl-loop for value in wasm-shift-fixture--values
                           append (cl-loop for count in wasm-shift-fixture--counts
                                           collect (list op value count)))))

(defun wasm-shift-fixture--definitions (&optional wasm-only)
  "Return shared native definitions and optional WASM-ONLY operand probes."
  (append
   '((defun shl-value (value count) (shl value count))
     (defun sar-value (value count) (sar value count))
     (defun shr-value (value count) (shr value count))
     (defun nested-value (value count)
       (shl (sar value 1) (shr count 1))))
   (cl-loop for op in wasm-shift-fixture--ops
            collect
            `(defun ,(intern (format "order-%s" op)) (trace)
               (seq
                (ptr-write-u64 trace 0 0)
                (let ((result
                       (,op
                        (seq (ptr-write-u64 trace 0 (+ (* 10 (ptr-read-u64 trace 0)) 1)) 8)
                        (seq (ptr-write-u64 trace 0 (+ (* 10 (ptr-read-u64 trace 0)) 2)) 1))))
                  (+ (* result 100) (ptr-read-u64 trace 0))))))
   (when wasm-only
     (append
      '((defun ensure-memory () (alloc-bytes 8 8)))
      (cl-loop for op in wasm-shift-fixture--ops
               append
               `((defun ,(intern (format "import-%s" op)) ()
                   (,op (extern-call audit_value) (extern-call audit_count)))
                 (defun ,(intern (format "catch-value-%s" op)) ()
                   (,op (catch 3 (throw 3 (extern-call audit_value))) 1))
                 (defun ,(intern (format "catch-count-%s" op)) ()
                   (,op 8 (catch 4 (throw 4 (extern-call audit_count)))))
                 (defun ,(intern (format "catch-both-%s" op)) ()
                   (,op (catch 3 (throw 3 (extern-call audit_value)))
                        (catch 4 (throw 4 (extern-call audit_count)))))))))))

(defun wasm-shift-fixture--hash (file)
  "Return SHA256 of FILE bytes."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))

(defun wasm-shift-fixture--expected (op value count)
  "Compute an independent signed-i64 raw shift table on the host."
  (let ((bits (logand count 63)))
    (cl-labels ((signed (word)
                  (let ((low (logand word #xffffffffffffffff)))
                    (if (>= low #x8000000000000000) (- low #x10000000000000000) low))))
      (pcase op
        ('shl (signed (ash value bits)))
        ('sar (ash value (- bits)))
        ('shr (signed (ash (logand value #xffffffffffffffff) (- bits))))
        ('nested (wasm-shift-fixture--expected
                  'shl (wasm-shift-fixture--expected 'sar value 1)
                  (wasm-shift-fixture--expected 'shr count 1)))))))

(defun wasm-shift-fixture-build-native ()
  "Link genuine native raw shifts to a scalar C comparison/trace driver."
  (let* ((directory "target/wasm-runtime-shift/")
         (cases (wasm-shift-fixture--cases))
         (object (concat directory "native-shifts.o"))
         (driver (concat directory "native-driver.c")))
    (make-directory directory t)
    (nelisp-aot-compile-to-object
     `(seq ,@(cl-subseq (wasm-shift-fixture--definitions) 0 4)
           ,@(cl-loop for op in wasm-shift-fixture--ops
                      collect `(defun ,(intern (format "native-order-%s" op)) ()
                                 (,op (extern-call audit_value) (extern-call audit_count)))))
     object)
    ;; Only the generated native instructions perform shifts.  The C driver
    ;; supplies scalar callbacks and compares against the independent table.
    (with-temp-file driver
      (insert "#include <stdint.h>\n#include <stdio.h>\n")
      (dolist (op '(shl sar shr nested))
        (insert (format "extern int64_t f_%s(int64_t,int64_t) __asm__(\"\\\"%s-value\\\"\");\n" op op)))
      (dolist (op wasm-shift-fixture--ops)
        (insert (format "extern int64_t order_%s(void) __asm__(\"\\\"native-order-%s\\\"\");\n" op op)))
      (insert "int64_t audit_value(void){fputs(\"A\",stdout);return 8;}\n"
              "int64_t audit_count(void){fputs(\"B\",stdout);return 1;}\nint main(void){\n")
      (dolist (row cases)
        (insert (format "if(f_%s((int64_t)UINT64_C(0x%x),(int64_t)UINT64_C(0x%x))!=(int64_t)UINT64_C(0x%x))return 1;\n"
                        (car row) (logand (cadr row) #xffffffffffffffff)
                        (logand (caddr row) #xffffffffffffffff)
                        (logand (apply #'wasm-shift-fixture--expected row) #xffffffffffffffff))))
      (dolist (op wasm-shift-fixture--ops)
        (insert (format "if(order_%s()!=%d)return 2;\n" op (wasm-shift-fixture--expected op 8 1))))
      (insert "return 0;}\n"))
    (unless (zerop (call-process (or (executable-find "cc") (error "cc required"))
                                nil t nil "-no-pie" driver object "-o" (concat directory "native-oracle")))
      (error "Native shift oracle link failed"))
    (with-temp-file (concat directory "native-inputs.json")
      (insert (json-encode
               `((cases . ,(vconcat
                             (mapcar (lambda (row)
                                       (vconcat (list (symbol-name (car row))
                                                      (number-to-string (cadr row))
                                                      (number-to-string (caddr row))
                                                      (number-to-string (apply #'wasm-shift-fixture--expected row))))) cases)))
                 (order_ops . ["shl" "sar" "shr"])
                 (oracle_kind . "native-and-wasm-agree-with-independent-raw-i64-table")
                 (native_stdout . "BABABA")
                 (native_sha256 . ,(wasm-shift-fixture--hash (concat directory "native-oracle")))
                 (native_provider . ,(file-relative-name
                                       (symbol-file 'nelisp-aot-compiler--emit-shift 'defun)))
                 (compiler_sha256 . ,(wasm-shift-fixture--hash
                                       (symbol-file 'nelisp-aot-compiler--emit-shift 'defun)))))))
    (princ (format "native-cases=%d order-cases=3\n" (length cases)))))

(defun wasm-shift-fixture-build-wasm ()
  "Compile Wasm conformance functions and bind the artifact to its source."
  (let ((path (or (getenv "WASM_SHIFT_OUTPUT") "target/wasm-runtime-shift/fixture.wasm")))
    (make-directory (file-name-directory path) t)
    (nelisp-aot-compile-to-object
     `(seq ,@(wasm-shift-fixture--definitions t)) path :arch 'wasm32 :format 'wasm)
    (with-temp-file (concat path ".json")
      (insert (json-encode
               `((wasm_sha256 . ,(wasm-shift-fixture--hash path))
                 (source_sha256 . ,(vconcat
                                    (mapcar (lambda (file)
                                              (vector file (wasm-shift-fixture--hash file)))
                                            '("lisp/nelisp-aot-compiler.el"
                                              "lisp/nelisp-asm-wasm.el"
                                              "tools/ai/wasm-shift-fixture.el"))))))))
    (princ (format "fixture=%s\n" path))))

(provide 'wasm-shift-fixture)
;;; wasm-shift-fixture.el ends here
