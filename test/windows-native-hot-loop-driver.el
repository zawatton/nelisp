;;; windows-native-hot-loop-driver.el --- W1.4 driver  -*- lexical-binding: t; -*-
;; HOT_PHASE=compile compiles once; HOT_PHASE=time loads the trusted entry
;; and times HOT_MODE (native, interpreted) for HOT_N iterations.
(require 'nelisp-native-cache)
(require 'nelisp-bytecode-native-consumer)
(let* ((nelisp-native-cache-backend (intern (or (getenv "HOT_BACKEND") "in-house")))
       (f (cdr (assq 'p3-loop (nelisp-bytecode-native-consumer-read-elc-functions
                               (getenv "HOT_FIXTURE")))))
       (n (string-to-number (or (getenv "HOT_N") "10000"))))
  (unless f (error "p3-loop missing from fixture"))
  (if (equal (getenv "HOT_PHASE") "compile")
      (progn (nelisp-native-cache-compile f) (princ "HOT-COMPILE-PASS\n"))
    (let ((fn (if (equal (getenv "HOT_MODE") "native")
                  (or (nelisp-native-cache-load f) (error "Missing native mapping"))
                (load (getenv "HOT_SOURCE") nil t t)
                (symbol-function 'p3-loop))))
      (garbage-collect)
      (let* ((start (float-time)) (value (funcall fn n))
             (elapsed (- (float-time) start)))
        (unless (= value (* n (1- n))) (error "Wrong result %S" value))
        (princ (format "HOT-TIME mode=%s backend=%S n=%d seconds=%.6f\n"
                       (getenv "HOT_MODE") nelisp-native-cache-backend n elapsed))))))
