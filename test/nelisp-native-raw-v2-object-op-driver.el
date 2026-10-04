;;; nelisp-native-raw-v2-object-op-driver.el --- Run object-op ELF proof -*- lexical-binding: t; -*-

(let ((root (getenv "NELISP_OBJECT_OP_REPO_ROOT")))
  (unless root (error "object-op driver root is missing"))
  (load (expand-file-name "lisp/nelisp-runtime-reload-abi.el" root) nil t)
  (load (expand-file-name "lisp/nelisp-native-load.el" root) nil t))

(defun nelisp-test-native-raw-v2-object-op-smoke ()
  (let* ((env (nelisp--native-env))
         (begin (nelisp-native-load--symbol-addr "nl_root_pin_begin_v2"))
         (reserve (nelisp-native-load--symbol-addr "nl_root_pin_reserve_v2"))
         (end (nelisp-native-load--symbol-addr "nl_root_pin_end_v2"))
         (handle (nelisp-native-load-raw-v2-artifact
                  (getenv "NELISP_OBJECT_OP_ARTIFACT")
                  "nl_native_object_probe"
                  (getenv "NELISP_OBJECT_OP_BINARY_SHA")))
         (item (cons 'car-object '(before)))
         (tail (cons 'cdr-object '(before)))
         (car-result nil) (car-before-gc nil) (cdr-result nil)
         (ticket nil) (frame nil) (input nil) (output nil)
         (expected nil)
         (bad-type nil) (bad-type-unchanged nil)
         (bad-op nil) (bad-op-unchanged nil)
         (malformed nil) (malformed-unchanged nil))
    ;; Inspect rooted input/output words before unboxing, so identity failures
    ;; are attributable to either the raw ELF dispatch or the Lisp copy-out.
    (setq ticket (ptr-call begin env 0 0 0 0 0)
          frame (ptr-call reserve env ticket 0 0 0 0)
          input (ptr-call reserve env ticket 0 0 0 0)
          output (ptr-call reserve env ticket 0 0 0 0))
    (nelisp-native-load-box frame nil env frame)
    (nelisp-native-load--zero-slot output)
    (unless (= (nelisp--native-pin-copy-v2 env ticket 1 (list item)) input)
      (error "object-op could not pin its evaluator input"))
    (setq expected (ptr-call reserve env ticket 0 0 0 0))
    (unless (and (> expected 0)
                 (= (nelisp--native-pin-copy-v2 env ticket 3 item) expected))
      (error "object-op could not pin its expected CAR"))
    (let ((expected (ptr-call (nelisp-native-load--symbol-addr
                               "nl_root_pin_slot_v2")
                              env ticket 3 0 0 0)))
      (unless (= (ptr-call (plist-get handle :entry)
                           env ticket 1 1 2 0) 0)
        (error "object-op raw ELF dispatch rejected a valid CAR"))
      (unless (and (= (ptr-read-u64 output 0) (ptr-read-u64 expected 0))
                   (= (ptr-read-u64 output 8) (ptr-read-u64 expected 8))
                   (= (ptr-read-u64 output 16) (ptr-read-u64 expected 16))
                   (= (ptr-read-u64 output 24) (ptr-read-u64 expected 24)))
        (error "object-op ELF output differs before unbox: in-tag=%S out-tag=%S expected-tag=%S"
               (ptr-read-u64 input 0) (ptr-read-u64 output 0)
               (ptr-read-u64 expected 0)))
      (setq car-result (nelisp-native-load-unbox output env frame)))
    (unless (eq car-result item)
      (error "object-op ELF unbox lost identity before GC"))
    (unless (= (ptr-call end env ticket 0 0 0 0) 1)
      (error "object-op ELF frame cleanup failed"))
    (setq ticket nil)
    (garbage-collect)
    (setcdr item '(after-direct-gc))
    (unless (and (eq car-result item)
                 (equal (cdr car-result) '(after-direct-gc)))
      (error "object-op ELF lost evaluator identity across GC"))
    (setq car-result
          (nelisp-native-load-raw-v2-object-op-call handle 1 (list item)))
    (setq car-before-gc (eq car-result item))
    (garbage-collect)
    (setcdr item '(after-car-gc))
    (unless (and (eq car-result item) (equal (cdr car-result) '(after-car-gc)))
      (error "object-op CAR identity failed: before=%S after=%S result=%S"
             car-before-gc (eq car-result item) car-result))
    (setq cdr-result
          (nelisp-native-load-raw-v2-object-op-call
           handle 2 (cons 'head tail)))
    (garbage-collect)
    (setcdr tail '(after-cdr-gc))
    (unless (and (eq cdr-result tail) (equal (cdr cdr-result) '(after-cdr-gc)))
      (error "object-op CDR lost evaluator identity across GC"))
    (unless (and (null (nelisp-native-load-raw-v2-object-op-call handle 1 nil))
                 (null (nelisp-native-load-raw-v2-object-op-call handle 2 nil)))
      (error "object-op nil semantics failed"))
    (condition-case err
        (nelisp-native-load-raw-v2-object-op-call handle 1 42)
      (wrong-type-argument (setq bad-type t)))
    (unless bad-type (error "object-op wrong type was not mapped"))
    ;; Exercise the ABI's output-preservation guarantee directly. The raw ELF
    ;; probe calls the gateway with an authenticated frame and a sentinel.
    (setq ticket (ptr-call begin env 0 0 0 0 0)
          frame (ptr-call reserve env ticket 0 0 0 0)
          input (ptr-call reserve env ticket 0 0 0 0)
          output (ptr-call reserve env ticket 0 0 0 0))
    (nelisp-native-load-box frame nil env frame)
    (nelisp-native-load-box output 777)
    (nelisp--native-pin-copy-v2 env ticket 1 42)
    (setq bad-type
          (= (ptr-call (plist-get handle :entry) env ticket 1 1 2 0) 1)
          bad-type-unchanged
          (and (= (ptr-read-u64 output 0) 2)
               (= (ptr-read-u64 output 8) 777)))
    (nelisp-native-load-box output 888)
    (setq bad-op
          (= (ptr-call (plist-get handle :entry) env ticket 99 1 2 0) 3)
          bad-op-unchanged
          (and (= (ptr-read-u64 output 0) 2)
               (= (ptr-read-u64 output 8) 888)))
    (nelisp-native-load-box output 999)
    (setq malformed
          (= (ptr-call (plist-get handle :entry) env ticket 1 16384 2 0) 2)
          malformed-unchanged
          (and (= (ptr-read-u64 output 0) 2)
               (= (ptr-read-u64 output 8) 999)))
    (unless (= (ptr-call end env ticket 0 0 0 0) 1)
      (error "object-op root frame was not released"))
    (unless (and bad-type bad-type-unchanged bad-op bad-op-unchanged
                 malformed malformed-unchanged)
      (error "object-op refusal mutated the rooted output"))
    t))

(nelisp-test-native-raw-v2-object-op-smoke)

;;; nelisp-native-raw-v2-object-op-driver.el ends here
