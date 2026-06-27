;; Phase 2: dynamic f64 FFI end-to-end.
;; dlopen("libm.so.6") -> dlsym("sqrt") -> (extern-call-ptr-f64 fnptr (:f64 4.0))
;; which must place 4.0 in xmm0, `call r11', and read the f64 result (2.0) from
;; xmm0.  `f64-bits' reinterprets the 2.0 result; bits of 2.0 = 0x4000000000000000.
;; main returns 0 on success, 1 on wrong result, 200/201 on load failure.
(seq
 (data-blob libm_name "libm.so.6\0" rodata)
 (data-blob sym_sqrt "sqrt\0" rodata)
 (defun main ()
   (let ((h (extern-call dlopen (data-addr libm_name) 2)))
     (if (= h 0)
         200
       (let ((f (extern-call dlsym h (data-addr sym_sqrt))))
         (if (= f 0)
             201
           (let ((bits (f64-bits (extern-call-ptr-f64 f (:f64 4.0)))))
             (if (= bits 4611686018427387904) 0 1))))))))
