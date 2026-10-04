;;; standalone-native-pin-copy-v2-driver.el --- v2 evaluator pin-copy smoke -*- lexical-binding: t; -*-

(require 'nelisp-native-load)

(defun nelisp-test-native-pin-copy-v2-smoke ()
  (let* ((env (nelisp--native-env))
         (begin (nelisp-native-load--symbol-addr "nl_root_pin_begin_v2"))
         (reserve (nelisp-native-load--symbol-addr "nl_root_pin_reserve_v2"))
         (end (nelisp-native-load--symbol-addr "nl_root_pin_end_v2"))
         (ticket (ptr-call begin env 0 0 0 0 0))
         (frame-slot (ptr-call reserve env ticket 0 0 0 0))
         (value-slot (ptr-call reserve env ticket 0 0 0 0))
         (value (cons 'kept (cons 'mutable nil)))
         (copied (nelisp--native-pin-copy-v2 env ticket 1 value))
         (gc (garbage-collect))
         (observed (nelisp-native-load-unbox value-slot env frame-slot))
         (identity-before (eq value observed)))
    (setcdr value (list 'after-gc))
    (let ((identity-after (eq value (nelisp-native-load-unbox
                                     value-slot env frame-slot)))
          (mutation-visible (equal (cdr observed) '(after-gc))))
      (unless (= (ptr-call end env ticket 0 0 0 0) 1)
        (error "native pin-copy v2: frame end failed"))
      (let* ((next (ptr-call begin env 0 0 0 0 0))
             (next-frame (ptr-call reserve env next 0 0 0 0))
             (next-slot (ptr-call reserve env next 0 0 0 0)))
        (nelisp-native-load-box next-frame nil env next-frame)
        (nelisp-native-load-box next-slot 777 env next-frame)
        (let ((stale-ticket-result
               (nelisp--native-pin-copy-v2 env ticket 1 value))
              (bad-index-result
               (nelisp--native-pin-copy-v2 env next 2 value))
              (stale-slot-result
               (ptr-read-u64 next-slot 8)))
          (let ((next-end (ptr-call end env next 0 0 0 0)))
            (and (> ticket 0) (> frame-slot 0) (> value-slot 0)
                 (= copied value-slot)
                 identity-before identity-after mutation-visible
                 (> next 0) (/= next ticket)
                 (= stale-ticket-result 0) (= bad-index-result 0)
                 (= stale-slot-result 777) (= next-end 1))))))))

(provide 'standalone-native-pin-copy-v2-driver)

;;; standalone-native-pin-copy-v2-driver.el ends here
