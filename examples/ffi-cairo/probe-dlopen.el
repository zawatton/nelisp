;; Phase 1 de-risk: pure-elisp runtime symbol resolution via dlopen/dlsym.
;; dlopen("libm.so.6", RTLD_NOW=2) -> dlsym(handle, "sqrt") -> nonzero fnptr.
;; main returns 42 = both resolved; 200 = dlopen failed; 201 = dlsym failed.
;; Link with -ldl (no-op on glibc>=2.34 where dl* live in libc).
(seq
 (data-blob libm_name "libm.so.6\0" rodata)
 (data-blob sym_sqrt "sqrt\0" rodata)
 (defun main ()
   (let ((h (extern-call dlopen (data-addr libm_name) 2)))
     (if (= h 0)
         200
       (let ((f (extern-call dlsym h (data-addr sym_sqrt))))
         (if (= f 0) 201 42))))))
