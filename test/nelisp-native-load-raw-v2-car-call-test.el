;;; nelisp-native-load-raw-v2-car-call-test.el --- checked CAR context -*- lexical-binding: t; -*-

(require 'ert)
(require 'nelisp-native-load)

(ert-deftest nelisp-native-load-raw-v2-car-call/releases-frame-on-success-and-wrong-type ()
  (let ((status 0)
        (ended nil)
        (next-slot 0)
        (slots nil)
        (input (cons 'kept nil))
        (result (cons 'returned nil))
        (observed-copy nil)
        (entry-args nil))
    (cl-letf (((symbol-function 'nelisp--native-env) (lambda () 77))
              ((symbol-function 'nelisp--native-pin-copy-v2)
               (lambda (_env _ticket index value)
                 (should (= index 1))
                 (setq observed-copy value)
                 (nth 1 slots)))
              ((symbol-function 'nelisp-native-load--symbol-addr)
               (lambda (name)
                 (cdr (assoc name '(("nl_root_pin_begin_v2" . 1)
                                    ("nl_root_pin_reserve_v2" . 2)
                                    ("nl_root_pin_end_v2" . 3)
                                    ("nl_root_pin_slot_v2" . 4))))))
              ((symbol-function 'ptr-call)
               (lambda (address &rest args)
                 (pcase address
                   (1 88)
                   (2 (setq next-slot (1+ next-slot)
                            slots (append slots (list (+ 1000 next-slot))))
                      (+ 1000 next-slot))
                   (3 (setq ended (equal (car args) 77)) 1)
                   (4 (nth (nth 2 args) slots))
                   (5 (setq entry-args args) status))))
              ((symbol-function 'nelisp-native-load-box) (lambda (&rest _) nil))
              ((symbol-function 'nelisp-native-load--zero-slot) (lambda (&rest _) nil))
              ((symbol-function 'nelisp-native-load-unbox) (lambda (&rest _) result)))
      (let* ((handle (list :kind 'raw-runtime-v2 :entry 5
                           :entry-name "nl_native_car_probe" :arity 4
                           :runtime-abi nelisp-native-load-raw-runtime-abi-v2
                           :raw-abi nelisp-native-load-raw-runtime-abi-v2
                           :imports '("nl_native_car_v2")))
             (nelisp-native-load-raw-mappings (list handle)))
        (should (eq (nelisp-native-load-raw-v2-car-call handle input) result))
        (should (eq observed-copy input))
        (should (equal (cl-subseq entry-args 0 4) '(77 88 1 2)))
        (should ended)
        (setq ended nil status 1 next-slot 0 slots nil)
        (should-error (nelisp-native-load-raw-v2-car-call handle 42)
                      :type 'wrong-type-argument)
        (should ended)))))

(ert-deftest nelisp-native-load-raw-v2-car-call/rejects-nearby-import-contracts ()
  (let* ((handle (list :kind 'raw-runtime-v2 :entry 5
                       :entry-name "nl_native_car_probe" :arity 4
                       :runtime-abi nelisp-native-load-raw-runtime-abi-v2
                       :raw-abi nelisp-native-load-raw-runtime-abi-v2
                       :imports '("nl_native_car_v2" "nl_native_cdr_v2")))
         (nelisp-native-load-raw-mappings (list handle)))
    (should-error (nelisp-native-load-raw-v2-car-call handle nil)
                  :type 'error)))

(provide 'nelisp-native-load-raw-v2-car-call-test)
;;; nelisp-native-load-raw-v2-car-call-test.el ends here
