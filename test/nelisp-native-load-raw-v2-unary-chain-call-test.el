;;; nelisp-native-load-raw-v2-unary-chain-call-test.el --- checked chain call -*- lexical-binding: t; -*-

(require 'ert)
(require 'nelisp-native-load)

(ert-deftest nelisp-native-load-raw-v2-unary-chain-call/roots-gcs-decodes-and-closes ()
  (let ((next-slot 0) (slots nil) (status 0) (ended nil)
        (gc-seen nil) (entry-args nil) (result (cons 'kept nil)))
    (cl-letf (((symbol-function 'nelisp--native-env) (lambda () 77))
              ((symbol-function 'nelisp--native-pin-copy-v2)
               (lambda (_env _ticket index _value)
                 (should (= index 1)) (nth 1 slots)))
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
                   (3 (setq ended (= (car args) 77)) 1)
                   (4 (nth (nth 2 args) slots))
                   (5 (setq entry-args args) status))))
              ((symbol-function 'nelisp-native-load-box) (lambda (&rest _) nil))
              ((symbol-function 'nelisp-native-load--zero-slot) (lambda (&rest _) nil))
              ((symbol-function 'nelisp-runtime-reload-contract-matches-p) (lambda () t))
              ((symbol-function 'nelisp-native-load--raw-supported-p) (lambda () t))
              ((symbol-function 'garbage-collect) (lambda () (setq gc-seen t)))
              ((symbol-function 'nelisp-native-load-unbox)
               (lambda (slot _env frame)
                 (should gc-seen) (should (= slot (nth 1 slots)))
                 (should (= frame (nth 0 slots))) result)))
      (let* ((handle (list :kind 'raw-runtime-v2 :entry 5
                           :entry-name "nl_native_chain_probe_v2" :arity 4
                           :runtime-abi nelisp-native-load-raw-runtime-abi-v2
                           :raw-abi nelisp-native-load-raw-runtime-abi-v2
                           :imports '("nl_native_car_v2" "nl_native_cdr_v2")))
             (nelisp-native-load-raw-mappings (list handle)))
        (should (eq (nelisp-native-load-raw-v2-unary-chain-call handle 'input 1)
                    result))
        (should (equal entry-args '(77 88 1 2 0 0)))
        (should ended)))))

(ert-deftest nelisp-native-load-raw-v2-unary-chain-call/refuses-untrusted-metadata-before-entry ()
  (let ((entered nil))
    (cl-letf (((symbol-function 'ptr-call)
               (lambda (&rest _) (setq entered t) 0)))
      (let ((nelisp-native-load-raw-mappings nil)
            (handle '(:kind raw-runtime-v2 :entry 5
                      :entry-name "nl_native_chain_probe_v2" :arity 4
                      :runtime-abi v2 :raw-abi v2
                      :imports ("nl_native_car_v2"))))
        (should-error (nelisp-native-load-raw-v2-unary-chain-call handle 'x 3))
        (should-not entered)))))

(ert-deftest nelisp-native-load-raw-v2-unary-chain-call/signals-actual-failing-intermediate-root ()
  (dolist (case '((17 1) (18 2)))
    (let ((next-slot 0) (slots nil) (status (car case)) (gc-seen nil)
          (offender 9))
      (cl-letf (((symbol-function 'nelisp--native-env) (lambda () 77))
                ((symbol-function 'nelisp--native-pin-copy-v2)
                 (lambda (_env _ticket _index _value) (nth 1 slots)))
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
                     (3 1)
                     (4 (nth (nth 2 args) slots))
                     (5 status))))
                ((symbol-function 'nelisp-native-load-box) (lambda (&rest _) nil))
                ((symbol-function 'nelisp-native-load--zero-slot) (lambda (&rest _) nil))
                ((symbol-function 'nelisp-runtime-reload-contract-matches-p) (lambda () t))
                ((symbol-function 'nelisp-native-load--raw-supported-p) (lambda () t))
                ((symbol-function 'garbage-collect) (lambda () (setq gc-seen t)))
                ((symbol-function 'nelisp-native-load-unbox)
                 (lambda (root _env _frame)
                   (should gc-seen)
                   (should (= root (nth (cadr case) slots)))
                   9)))
        (let* ((handle (list :kind 'raw-runtime-v2 :entry 5
                             :entry-name "nl_native_chain_probe_v2" :arity 4
                             :runtime-abi nelisp-native-load-raw-runtime-abi-v2
                             :raw-abi nelisp-native-load-raw-runtime-abi-v2
               :imports '("nl_native_car_v2" "nl_native_cdr_v2")))
               (nelisp-native-load-raw-mappings (list handle)))
          (should (equal (condition-case err
                             (nelisp-native-load-raw-v2-unary-chain-call handle offender 1)
                           (wrong-type-argument err))
                         '(wrong-type-argument listp 9))))))))

(provide 'nelisp-native-load-raw-v2-unary-chain-call-test)
;;; nelisp-native-load-raw-v2-unary-chain-call-test.el ends here
