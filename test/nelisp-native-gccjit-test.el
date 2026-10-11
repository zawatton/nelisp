;;; nelisp-native-gccjit-test.el --- Backend host controls -*- lexical-binding: t; -*-
(require 'ert)
(require 'nelisp-native-gccjit)
(require 'nelisp-native-cache)
(require 'nelisp-bytecode-compiler-input-dialect)
(defconst nelisp-native-gccjit-test--root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))
(add-to-list 'load-path (expand-file-name "packages/nl-ffi/src" nelisp-native-gccjit-test--root))

(defmacro nelisp-native-gccjit-test--ffi (&rest body)
  (declare (indent 0))
  `(let ((calls nil) (serial 4096) (nelisp-native-gccjit--contexts nil)
         (nelisp-native-gccjit--results nil))
     (cl-letf (((symbol-function 'nelisp-native-gccjit--call)
                (lambda (name &rest args)
                  (push (cons name args) calls) (setq serial (1+ serial))))
               ((symbol-function 'nelisp-native-gccjit--cstring) #'identity)
               ((symbol-function 'nelisp-native-gccjit--array) #'identity))
       ,@body)))

(defun nelisp-native-gccjit-test--form (body)
  `(defun probe (env ticket argument-count root-count) ,body))

(ert-deftest nelisp-native-gccjit/cons-raw-v2-abi-and-indirect-import ()
  (nelisp-native-gccjit-test--ffi
    (should (integerp
             (nelisp-native-gccjit-lower
              (nelisp-native-gccjit-test--form '(extern-call nl_native_funcall_v2 env ticket 1 2 6 12))
              '(("nl_native_funcall_v2" . 8192)))))
    (should (= (nth 4 (assoc "gcc_jit_context_new_call_through_ptr" calls)) 6)))
  (nelisp-native-gccjit-test--ffi
    (let ((form '(defun probe (env ticket argument-count root-count)
                   (if (/= argument-count 2) 3
                     (if (/= root-count 4) 3
                       (let nil (let ((status (extern-call nl_native_cons_v2 env ticket 1 2 3 0)))
                                  (if (= status 0) (+ 512 3) status))))))))
      (should (integerp (nelisp-native-gccjit-lower form '(("nl_native_cons_v2" . 8192)))))
      (let ((fn (assoc "gcc_jit_context_new_function" calls))
            (import (assoc "gcc_jit_context_new_global" calls))
            (indirect (assoc "gcc_jit_context_new_call_through_ptr" calls)))
        (should (= (length (cdr fn)) 8))
        (should (= (nth 6 fn) 4))
        (should (= (length (nth 7 fn)) 4))
        (should (= (nth 4 indirect) 6))
        (should (equal (car (last import)) "nl_gccjit_import_nl_native_cons_v2")))
      (should (= (cl-count "gcc_jit_context_new_param" calls :key #'car :test #'equal) 4))
      (should (= (cl-count "gcc_jit_context_get_int_type" calls :key #'car :test #'equal) 2))
      (should (= (cl-count "gcc_jit_block_end_with_conditional" calls :key #'car :test #'equal) 3))
      (should-not (assoc "gcc_jit_context_new_call" calls)))))

;; Each operator is a separately counted test, asserting its actual enum.
(dolist (spec (append nelisp-native-gccjit--binary-ops nelisp-native-gccjit--comparisons))
  (let ((op (car spec)) (code (cdr spec)))
    (eval `(ert-deftest ,(intern (format "nelisp-native-gccjit/operator-%s" op)) ()
             (nelisp-native-gccjit-test--ffi
               (nelisp-native-gccjit-lower (nelisp-native-gccjit-test--form '(,op 40 2)) nil)
               (let ((call (assoc ,(if (assq op nelisp-native-gccjit--comparisons)
                                      "gcc_jit_context_new_comparison" "gcc_jit_context_new_binary_op") calls)))
                 (should call) (should (= (nth 3 call) ,code))))) t)))

(ert-deftest nelisp-native-gccjit/structured-forms-and-root-memory ()
  (dolist (body '((let ((x 4)) (let ((x (+ x 1)) (y x)) (+ x y)))
                  (let* ((x 4) (y (+ x 1))) (setq x y) x)
                  (if (or (= env 0) (/= ticket 0)) 3 515)
                  (if (and (> env 0) (< ticket 4)) 3 515)
                  (progn (ptr-write-u64 env 8 42) (ptr-read-u64 env 8))))
    (nelisp-native-gccjit-test--ffi
      (should (integerp (nelisp-native-gccjit-lower (nelisp-native-gccjit-test--form body) nil)))
      (should (assoc "gcc_jit_block_end_with_return" calls)))))

(ert-deftest nelisp-native-gccjit/guarded-fixnum-emitter-integration ()
  ;; Exercise the producer's actual tree, including signed payload comparisons,
  ;; through GCC JIT's grammar and lowering rather than a hand-written fixture.
  (dolist (opcode '(83 84 85 86 87 88 89 90 91 92 95))
    (nelisp-native-gccjit-test--ffi
      (should (integerp
               (nelisp-native-gccjit-lower
                (nelisp-native-gccjit-test--form
                 (nelisp-native-funcall-v2-fixnum-form opcode '(1 2) 1 0 7))
                '(("nl_root_pin_slot_v2" . 8192)))))
      (should (assoc "gcc_jit_block_end_with_return" calls)))))

(ert-deftest nelisp-native-gccjit/refuse-unsupported-and-release ()
  (dolist (body '((quote x) (funcall env) (while env 0) (list 1 2)
                  unknown (+ 1 2 3) (if 1 2) (extern-call absent 1)
                  (let ((x 1)) (setq missing 2)) (let ((x)) x)))
    (nelisp-native-gccjit-test--ffi
      (should-error (nelisp-native-gccjit-lower (nelisp-native-gccjit-test--form body) nil))
      (should (assoc "gcc_jit_context_release" calls)))))

(ert-deftest nelisp-native-gccjit/in-memory-owner-and-import-rebinding ()
  (nelisp-native-gccjit-test--ffi
    (let ((writes nil))
      (cl-letf (((symbol-function 'ptr-write-u64)
                 (lambda (&rest args) (push args writes))))
        (let ((entry (nelisp-native-gccjit-compile-in-memory
                      (nelisp-native-gccjit-test--form '(extern-call nl_native_cons_v2 1 2 3 4 5 6))
                      '(("nl_native_cons_v2" . 9000)))))
          (should (assq entry nelisp-native-gccjit--results))
          (should (= (nth 2 (car writes)) 9000))
          (should (assoc "gcc_jit_result_get_global" calls))
          (should (assoc "gcc_jit_context_release" calls))
          (should-not (assoc "gcc_jit_result_release" calls)))))))

(ert-deftest nelisp-native-gccjit/backend-key-and-in-house-dispatch ()
  (let ((nelisp-native-cache--abi (make-string 64 ?a))
        (nelisp-native-cache--compiler-revision (make-string 64 ?b))
        (directory (make-temp-file "gccjit-cache-key-" t)))
    (unwind-protect
        (cl-letf (((symbol-function 'nelisp-native-cache--root) (lambda () directory)))
          (let* ((fn (make-byte-code 514 (unibyte-string 192 135) [1] 2))
                 (nelisp-native-cache-backend 'in-house)
                 (first (nelisp-native-cache-file fn))
                 (abi (nelisp-native-cache-abi-hash)))
            (cl-letf (((symbol-function 'nelisp-native-cache--compile-in-house)
                       (lambda (arg) (should (eq arg fn)) 'old-path)))
              (should (eq (nelisp-native-cache-compile fn) 'old-path)))
            (let ((nelisp-native-cache-backend 'gccjit))
              (should-not (equal abi (nelisp-native-cache-abi-hash)))
              (should-not (equal (file-name-directory first)
                                 (file-name-directory (nelisp-native-cache-file fn))))
              (should (string-suffix-p ".so" (nelisp-native-cache-file fn))))))
      (delete-directory directory t))))

(ert-deftest nelisp-native-gccjit/header-refused-before-dlopen ()
  (let* ((directory (make-temp-file "gccjit-cache-header-" t))
         (file (expand-file-name "probe.so" directory))
         (nelisp-native-cache-backend 'gccjit) (opens 0))
    (unwind-protect
        (cl-letf (((symbol-function 'nelisp-native-cache-file) (lambda (_) file))
                  ((symbol-function 'nl-ffi--dlopen) (lambda (_) (setq opens (1+ opens)))))
          (write-region "library" nil file nil 'silent)
          (write-region "(:nelisp-native-cache 1 :backend in-house)\n" nil (concat file ".nelh") nil 'silent)
          (should-error (nelisp-native-cache-load 'dummy))
          (should (= opens 0)))
      (delete-directory directory t))))

(provide 'nelisp-native-gccjit-test)

(ert-deftest nelisp-native-gccjit/integer-range-without-native-overflow ()
  (dolist (n (list 0 515 (1- (expt 2 63)) (- (expt 2 63))))
    (nelisp-native-gccjit-test--ffi
      (should (integerp (nelisp-native-gccjit-lower (nelisp-native-gccjit-test--form n) nil)))))
  (dolist (n (list (expt 2 63) (1- (- (expt 2 63)))))
    (nelisp-native-gccjit-test--ffi
      (should-error (nelisp-native-gccjit-lower (nelisp-native-gccjit-test--form n) nil)))))

(ert-deftest nelisp-native-gccjit/compile-once-publish-sidecar-and-hit ()
  (skip-unless (equal emacs-version "31.1"))
  (let ((nelisp-bytecode-compiler-input--root nelisp-native-gccjit-test--root)
        (directory (make-temp-file "gccjit-publish-" t))
        (nelisp-native-cache-backend 'gccjit)
        (nelisp-native-cache--abi (make-string 64 ?a))
        (nelisp-bytecode-native-rooted-cfg-contract--validation-count 0)
        (function (byte-compile (lambda (a b) (cons a b))))
        (compiles 0))
    (unwind-protect
        (cl-letf (((symbol-function 'nelisp-native-cache--root) (lambda () directory))
                  ((symbol-function 'nelisp-native-cache--gccjit-imports)
                   (lambda (names) (mapcar (lambda (name) (cons name 8192)) names)))
                  ((symbol-function 'nelisp-native-gccjit-compile-to-file)
                   (lambda (form imports path)
                     (should (eq (car form) 'defun)) (should imports)
                     (setq compiles (1+ compiles))
                     (write-region "dynamic-library" nil path nil 'silent))))
          (let ((file (nelisp-native-cache-compile function)))
            (should (file-exists-p file))
            (should (= nelisp-bytecode-native-rooted-cfg-contract--validation-count 1))
            (let ((header (with-temp-buffer (insert-file-contents (concat file ".nelh")) (read (current-buffer)))))
              (should (eq (plist-get header :backend) 'gccjit))
              (should (equal (plist-get header :library-sha256) (nelisp-native-cache--file-hash file))))
            (should (equal file (nelisp-native-cache-compile function)))
            (should (= compiles 1))
            (should (= nelisp-bytecode-native-rooted-cfg-contract--validation-count 1))))
      (delete-directory directory t))))

(ert-deftest nelisp-native-gccjit/cache-load-rebinds-with-zero-validations ()
  (let* ((directory (make-temp-file "gccjit-load-" t))
         (file (expand-file-name "probe.so" directory))
         (nelisp-native-cache-backend 'gccjit)
         (nelisp-native-cache--gccjit-handles nil)
         (nelisp-native-cache--addresses '(:environment 100))
         (validations 0) (opens 0) (writes nil)
         (header '(:nelisp-native-cache 1 :backend gccjit :abi "abi" :input "input"
                   :entry "probe" :arity 2 :root-count 4 :initializers nil
                   :imports ("nl_native_cons_v2"))))
    (unwind-protect
        (progn
          ;; dlopen is stubbed below; the reservation still consumes a real
          ;; bounded ELF64 program-header shape (one two-page PT_LOAD).
          (let ((bytes (make-string 120 0)) (coding-system-for-write 'binary))
            (dotimes (index 6) (aset bytes index (aref "\177ELF\2\1" index)))
            (aset bytes 32 64) ; program-header offset
            (aset bytes 54 56) ; program-header width
            (aset bytes 56 1)  ; program-header count
            (aset bytes 64 1)  ; PT_LOAD
            (aset bytes 80 255) (aset bytes 81 15) ; vaddr page offset 4095
            (aset bytes 104 2) ; memsz=2 crosses a page
            (write-region bytes nil file nil 'silent))
          (setq header (plist-put header :library-sha256 (nelisp-native-cache--file-hash file)))
          (write-region (concat (prin1-to-string header) "\n") nil (concat file ".nelh") nil 'silent)
          ;; Load the real package; only runtime OS/pointer primitives are stubbed.
          (require 'nl-ffi)
          (cl-letf (((symbol-function 'nelisp-native-cache-file) (lambda (_) file))
                    ((symbol-function 'nelisp-native-cache-abi-hash) (lambda () "abi"))
                    ((symbol-function 'nelisp-native-cache--input-hash) (lambda (_) "input"))
                    ((symbol-function 'nelisp-native-load--raw-v2-symbol-addr-trusted)
                     (lambda (name &rest _) (should (equal name "nl_native_cons_v2")) 9000))
                    ((symbol-function 'nl-ffi--dlopen) (lambda (_) (setq opens (1+ opens)) 8000))
                    ((symbol-function 'nl-ffi--dlsym)
                     (lambda (handle name) (should (= handle 8000))
                       (if (equal name "probe") 8100 8200)))
                    ((symbol-function 'ptr-write-u64) (lambda (&rest args) (push args writes)))
                    ((symbol-function 'nelisp-native-cache--callable-from-entry)
                     (lambda (entry h addresses &optional _constants _owner)
                       (should (= entry 8100)) (should (equal h header))
                       (should (eq addresses nelisp-native-cache--addresses))
                       (lambda (&rest _) 'called)))
                    ((symbol-function 'nelisp-bytecode-native-rooted-cfg-contract-valid-p)
                     (lambda (&rest _) (setq validations (1+ validations)))))
            (should (eq (funcall (nelisp-native-cache-load
                                 (make-byte-code 514 (unibyte-string 135) [] 2)) 1 2) 'called))
            (should (= validations 0)) (should (= opens 1))
            (should (equal writes '((8200 0 9000))))
            ;; Corruption must be refused before any further dlopen.
            (write-region "corrupt" nil file nil 'silent)
            (should-error (nelisp-native-cache-load
                           (make-byte-code 514 (unibyte-string 135) [] 2)))
            (should (= opens 1))))
      (delete-directory directory t))))


(ert-deftest nelisp-native-gccjit/file-output-kind-and-context-release ()
  (nelisp-native-gccjit-test--ffi
    (let ((path (make-temp-file "gccjit-output-" nil ".so"))
          (record (symbol-function 'nelisp-native-gccjit--call)))
      (unwind-protect
          (cl-letf (((symbol-function 'nelisp-native-gccjit--call)
                     (lambda (name &rest args)
                       (if (equal name "gcc_jit_context_get_first_error") 0
                         (apply record name args)))))
            (write-region "library" nil path nil 'silent)
            (should (equal path (nelisp-native-gccjit-compile-to-file
                                 (nelisp-native-gccjit-test--form 515) nil path)))
            (let ((call (assoc "gcc_jit_context_compile_to_file" calls)))
              (should (= (nth 2 call) 2)))
            (should (assoc "gcc_jit_context_release" calls)))
        (delete-file path)))))
