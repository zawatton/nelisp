;;; standalone-native-car-v2-driver.el --- Boxed CAR gateway smoke -*- lexical-binding: t; -*-

(require 'nelisp-native-load)

(defun nelisp-test-native-car-v2-smoke ()
  (let* ((env (nelisp--native-env))
         (begin-addr (nelisp-native-load--symbol-addr "nl_root_pin_begin_v2"))
         (reserve-addr (nelisp-native-load--symbol-addr "nl_root_pin_reserve_v2"))
         (end-addr (nelisp-native-load--symbol-addr "nl_root_pin_end_v2"))
         (car-addr (nelisp-native-load--symbol-addr "nl_native_car_v2"))
         (cons-addr (nelisp-native-load--symbol-addr "nelisp_cons_construct"))
         (ticket (ptr-call begin-addr env 0 0 0 0 0))
         (input (ptr-call reserve-addr env ticket 0 0 0 0))
         (output (ptr-call reserve-addr env ticket 0 0 0 0))
         (value (ptr-call reserve-addr env ticket 0 0 0 0))
         (tail (ptr-call reserve-addr env ticket 0 0 0 0))
         (input-index 0)
         (output-index 1)
         (status-nil nil)
         (nil-output nil)
         (status-cons nil)
         (identity-before nil)
         (gc nil)
         (identity-after nil)
         (mutation-status nil)
         (mutation-value nil)
         (wrong-status nil)
         (wrong-output nil)
         (malformed-status nil)
         (malformed-output nil)
         (stale nil)
         (stale-ticket nil)
         (cleanup nil))
    (setq status-nil
          (progn
            (nelisp-native-load-box input nil env ticket)
            (nelisp-native-load-box output 777)
            (ptr-call car-addr env ticket input-index output-index 0 0)))
    (setq nil-output (= (ptr-read-u64 output 0) 0))
    (nelisp-native-load-box value "car identity")
    (nelisp-native-load-box tail nil)
    (ptr-call cons-addr value tail input 0 0 0)
    (nelisp-native-load-box output 777)
    (setq status-cons
          (ptr-call car-addr env ticket input-index output-index 0 0))
    (setq identity-before
          (and (= (ptr-read-u64 output 0) 5)
               (progn
                 (dotimes (i 4)
                   (ptr-write-u64 tail (* i 8) (ptr-read-u64 output (* i 8))))
                 t)))
    (setq gc (garbage-collect))
    (setq status-cons
          (and (= status-cons
                  (ptr-call car-addr env ticket input-index output-index 0 0))
               status-cons))
    (setq identity-after
          (and (= (ptr-read-u64 output 0) (ptr-read-u64 tail 0))
               (= (ptr-read-u64 output 8) (ptr-read-u64 tail 8))))
    ;; The boxed cons stores its immediate CAR as (n << 2) | 1.  Mutate that
    ;; field in place, then verify CAR observes the updated fixnum.
    (ptr-write-u64 (ptr-read-u64 input 8) 0 173)
    (setq mutation-status
          (and (= (ptr-call car-addr env ticket input-index output-index 0 0) 0)
               (= (ptr-read-u64 output 0) 2)
               (= (ptr-read-u64 output 8) 43)))
    (setq mutation-value (ptr-read-u64 output 8))
    ;; Unsupported type and malformed indices preserve the destination slot.
    (nelisp-native-load-box input 9)
    (nelisp-native-load-box output 777)
    (setq wrong-status
          (ptr-call car-addr env ticket input-index output-index 0 0))
    (setq wrong-output (and (= (ptr-read-u64 output 0) 2)
                            (= (ptr-read-u64 output 8) 777)))
    (nelisp-native-load-box output 888)
    (setq malformed-status
          (ptr-call car-addr env ticket input-index 16384 0 0))
    (setq malformed-output (and (= (ptr-read-u64 output 0) 2)
                                (= (ptr-read-u64 output 8) 888)))
    (setq stale-ticket ticket)
    (setq stale (ptr-call end-addr env ticket 0 0 0 0))
    (setq cleanup
          (and (= (ptr-call car-addr env stale-ticket input-index output-index 0 0)
                  2)
               (= (ptr-read-u64 output 8) 888)))
    (and (> ticket 0) (= status-nil 0) nil-output
         (= status-cons 0) identity-before identity-after gc
         mutation-status (= mutation-value 43)
         (= wrong-status 1) wrong-output
         (= malformed-status 2) malformed-output
         (= stale 1) cleanup
         (= (ptr-call end-addr env ticket 0 0 0 0) 0))))

(provide 'standalone-native-car-v2-driver)

;;; standalone-native-car-v2-driver.el ends here
