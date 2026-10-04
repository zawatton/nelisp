;;; standalone-root-pin-v2-ticket-driver.el --- Ticket smoke driver -*- lexical-binding: t; -*-

(require 'nelisp-native-load)

(defun nelisp-test-root-pin-v2-ticket-smoke ()
  (let* ((env (nelisp--native-env))
         (begin-addr (nelisp-native-load--symbol-addr "nl_root_pin_begin_v2"))
         (reserve-addr (nelisp-native-load--symbol-addr "nl_root_pin_reserve_v2"))
         (end-addr (nelisp-native-load--symbol-addr "nl_root_pin_end_v2"))
         (begin (ptr-call begin-addr env 0 0 0 0 0))
         (slot (ptr-call reserve-addr env begin 0 0 0 0))
         (gc (garbage-collect))
         (end (ptr-call end-addr env begin 0 0 0 0))
         (next (ptr-call begin-addr env 0 0 0 0 0))
         (stale-reserve (ptr-call reserve-addr env begin 0 0 0 0))
         (stale-end (ptr-call end-addr env begin 0 0 0 0))
         (next-slot (ptr-call reserve-addr env next 0 0 0 0))
         (next-end (ptr-call end-addr env next 0 0 0 0))
         (outer (ptr-call begin-addr env 0 0 0 0 0))
         (outer-slot (ptr-call reserve-addr env outer 0 0 0 0))
         (inner (ptr-call begin-addr env 0 0 0 0 0))
         (outer-during-inner
          (list (ptr-call reserve-addr env outer 0 0 0 0)
                (ptr-call end-addr env outer 0 0 0 0)))
         (inner-slot (ptr-call reserve-addr env inner 0 0 0 0))
         (gc-nested (garbage-collect))
         (inner-end (ptr-call end-addr env inner 0 0 0 0))
         (inner-stale
          (list (ptr-call reserve-addr env inner 0 0 0 0)
                (ptr-call end-addr env inner 0 0 0 0)))
         (outer-again (ptr-call reserve-addr env outer 0 0 0 0))
         (outer-end (ptr-call end-addr env outer 0 0 0 0))
         (legacy (nelisp-native-load--pin-begin env))
         (legacy-v2 (ptr-call begin-addr env 0 0 0 0 0))
         (legacy-end (nelisp-native-load--pin-end env legacy))
         (v2 (ptr-call begin-addr env 0 0 0 0 0))
         (legacy-during-v2
          (list (nelisp-native-load--pin-begin env)
                (ptr-call (nelisp-native-load--symbol-addr
                           "nl_root_pin_reserve") env v2 0 0 0 0)
                (ptr-call (nelisp-native-load--symbol-addr
                           "nl_root_pin_end") env v2 0 0 0 0)))
         (v2-end (ptr-call end-addr env v2 0 0 0 0))
         (cleanup-token 0)
         (cleanup-result 0))
    (condition-case nil
        (unwind-protect
            (progn
              (setq cleanup-token (ptr-call begin-addr env 0 0 0 0 0))
              (error "root pin cleanup probe"))
          (setq cleanup-result
                (ptr-call end-addr env cleanup-token 0 0 0 0)))
      (error nil))
    (let* ((after-cleanup (ptr-call begin-addr env 0 0 0 0 0))
           (after-cleanup-end
            (ptr-call end-addr env after-cleanup 0 0 0 0)))
      (and (> begin 0) (> slot 0) (= end 1)
           (> next 0) (/= begin next) (= slot next-slot)
           (= stale-reserve 0) (= stale-end 0)
           (> next-slot 0) (= next-end 1)
           (> outer 0) (> outer-slot 0) (> inner 0) (/= inner outer)
           (equal outer-during-inner '(0 0))
           (> inner-slot 0) (= inner-end 1)
           (equal inner-stale '(0 0))
           (> outer-again 0) (= outer-end 1)
           (> legacy 0) (= legacy-v2 0) (null legacy-end)
           (> v2 0) (equal legacy-during-v2 '(0 0 0)) (= v2-end 1)
           (> cleanup-token 0) (= cleanup-result 1)
           (> after-cleanup 0) (= after-cleanup-end 1)))))

(provide 'standalone-root-pin-v2-ticket-driver)

;;; standalone-root-pin-v2-ticket-driver.el ends here
