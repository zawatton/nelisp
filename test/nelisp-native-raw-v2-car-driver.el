;;; nelisp-native-raw-v2-car-driver.el --- Machine CAR gateway proof -*- lexical-binding: t; -*-

(let* ((root (getenv "NELISP_CAR_REPO_ROOT"))
       (artifact (getenv "NELISP_CAR_ARTIFACT"))
       (binary-sha (getenv "NELISP_CAR_BINARY_SHA")))
  (unless (and root artifact binary-sha)
    (error "CAR smoke driver environment is incomplete"))
  (load (expand-file-name "lisp/nelisp-runtime-reload-abi.el" root) nil t)
  (load (expand-file-name "lisp/nelisp-native-load.el" root) nil t))

(defun nelisp-test-native-raw-v2-car-smoke ()
  "Call the machine CAR export with authenticated slots and validate refusals."
  (let* ((env (nelisp--native-env))
         (begin (nelisp-native-load--symbol-addr "nl_root_pin_begin_v2"))
         (reserve (nelisp-native-load--symbol-addr "nl_root_pin_reserve_v2"))
         (end (nelisp-native-load--symbol-addr "nl_root_pin_end_v2"))
         (handle (nelisp-native-load-raw-v2-artifact
                  (getenv "NELISP_CAR_ARTIFACT") "nl_native_car_probe"
                  (getenv "NELISP_CAR_BINARY_SHA")))
         (ticket nil) (old-ticket nil)
         (frame-slot nil) (input-slot nil) (output-slot nil)
         (item (cons 'rooted (list 'before-gc)))
         (input (cons item nil))
         (status nil) (observed nil) (identity-ok nil) (mutation-ok nil)
         (new-ticket nil) (new-frame nil) (new-input nil) (new-output nil)
         (sentinel nil) (stale-status nil) (bad-index-status nil)
         (unchanged nil))
    (unwind-protect
        (progn
          ;; Exercise the checked evaluator-to-raw-unit bridge before the
          ;; lower-level ticket controls below.  The return is the original
          ;; nested Sexp, not a reconstructed copy.
          (let* ((wrapped-input (list item))
                 (wrapped-result
                  (nelisp-native-load-raw-v2-car-call handle wrapped-input)))
            (garbage-collect)
            (setcdr item '(wrapper-after-gc))
            (unless (and (eq item wrapped-result)
                         (equal (cdr wrapped-result) '(wrapper-after-gc)))
              (error "checked raw-v2 CAR wrapper lost evaluator identity")))
          (unless (null (nelisp-native-load-raw-v2-car-call handle nil))
            (error "checked raw-v2 CAR wrapper did not preserve nil"))
          (let ((wrong-type-refused nil))
            (condition-case error-data
                (nelisp-native-load-raw-v2-car-call handle 42)
              (wrong-type-argument (setq wrong-type-refused t)))
            (unless (and wrong-type-refused
                         (eq item (nelisp-native-load-raw-v2-car-call
                                   handle (list item))))
              (error "checked raw-v2 CAR wrapper wrong-type cleanup failed")))
          (setq ticket (ptr-call begin env 0 0 0 0 0))
          (setq frame-slot (ptr-call reserve env ticket 0 0 0 0))
          (setq input-slot (ptr-call reserve env ticket 0 0 0 0))
          (setq output-slot (ptr-call reserve env ticket 0 0 0 0))
          (unless (and (> ticket 0) (> frame-slot 0)
                       (> input-slot 0) (> output-slot 0))
            (error "CAR smoke could not reserve the initial frame"))
          (nelisp-native-load-box frame-slot nil env frame-slot)
          (unless (= (nelisp--native-pin-copy-v2 env ticket 1 input)
                     input-slot)
            (error "CAR smoke input did not stay in authenticated slot 1"))
          ;; The exported adapter consumes four operands.  Invoke its
          ;; authenticated entry through ptr-call's seven-total-argument ABI,
          ;; padding its two spare registers explicitly.  The generic raw-call
          ;; helper pads v2 calls to seven operands, one beyond ptr-call.
          (setq status (ptr-call (plist-get handle :entry)
                                 env ticket 1 2 0 0))
          (unless (= status 0)
            (error "machine CAR returned status %S" status))
          (garbage-collect)
          (setcdr item (list 'after-gc))
          (setq observed
                (nelisp-native-load-unbox output-slot env frame-slot))
          (setq identity-ok (eq item observed))
          (setq mutation-ok (equal (cdr observed) '(after-gc)))
          (setq old-ticket ticket)
          (unless (= (ptr-call end env ticket 0 0 0 0) 1)
            (error "CAR smoke could not release initial root frame"))
          (setq ticket nil)

          ;; The second frame provides an untouched output sentinel while the
          ;; old ticket and a current-ticket out-of-range index are rejected.
          (setq new-ticket (ptr-call begin env 0 0 0 0 0))
          (setq new-frame (ptr-call reserve env new-ticket 0 0 0 0))
          (setq new-input (ptr-call reserve env new-ticket 0 0 0 0))
          (setq new-output (ptr-call reserve env new-ticket 0 0 0 0))
          (unless (and (> new-ticket 0) (/= new-ticket old-ticket))
            (error "CAR smoke did not issue a fresh ticket"))
          (nelisp-native-load-box new-frame nil env new-frame)
          (nelisp-native-load-box new-output 777 env new-frame)
          (setq sentinel (ptr-read-u64 new-output 8))
          (setq stale-status (ptr-call (plist-get handle :entry)
                                       env old-ticket 1 2 0 0))
          (setq bad-index-status (ptr-call (plist-get handle :entry)
                                           env new-ticket 99 2 0 0))
          (setq unchanged (= (ptr-read-u64 new-output 8) sentinel))
          (unless (and identity-ok mutation-ok (= stale-status 2)
                       (= bad-index-status 2) unchanged)
            (error "CAR smoke proof mismatch: %S"
                   (list identity-ok mutation-ok stale-status
                         bad-index-status unchanged)))
          (unless (= (ptr-call end env new-ticket 0 0 0 0) 1)
            (error "CAR smoke could not release refusal-test frame"))
          (setq new-ticket nil)
          t)
      (when ticket
        (ignore-errors (ptr-call end env ticket 0 0 0 0)))
      (when new-ticket
        (ignore-errors (ptr-call end env new-ticket 0 0 0 0)))
      (when handle
        (nelisp-native-load-unload handle)))))

(nelisp-test-native-raw-v2-car-smoke)

;;; nelisp-native-raw-v2-car-driver.el ends here
