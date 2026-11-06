;;; nelisp-aot-test-runtime.el --- C stand-ins for AOT native test harnesses  -*- lexical-binding: t; -*-

;;; Commentary:

;; Since c552d6bbb `sexp-tag' lowers to an extern call of the runtime
;; helper `nl_val_tag'.  Self-contained native test units that link with
;; gcc must therefore define it.  The C text below mirrors the
;; `nl_val_tag' defun in scripts/nelisp-standalone-build.el (Doc 216 WORD
;; tag dispatch) and uses only builtin types so it also links under
;; -nostdlib.

;;; Code:

(defconst nelisp-aot-test-runtime-nl-val-tag-c
  (concat
   "unsigned long long nl_val_tag(unsigned long long v){"
   "if((v&7)==0)return *(unsigned char*)v;"
   "if((v&1)==0){"
   "if((v&7)==4)return *(long long*)(v-4+8)<0?14:5;"
   "if((v&7)==6)return *(unsigned char*)(v-6)&63;"
   "if((v&7)==2)return 7;"
   "return *(unsigned char*)v;}"
   "return (v&3)==1?2:(v==3?0:1);}\n")
  "C source of an `nl_val_tag' stand-in for native test harnesses.")

(defconst nelisp-aot-test-runtime-nl-val-tag-masked-c
  (concat
   "unsigned long long nl_val_tag(unsigned long long v){"
   "if((v&7)==2)return 7;"
   "if((v&7)==6)return *(unsigned char*)(v-6)&63;"
   "if((v&1)==0)return *(unsigned char*)(v&~7ULL)&63;"
   "return (v&3)==1?2:(v==3?0:1);}\n")
  "Like `nelisp-aot-test-runtime-nl-val-tag-c' but masks every header read.
Mirrors the pre-c552d6bbb inline `sexp-tag' read, which the literal-word
fold tests exercise with header bytes whose top two bits are set.")

(defconst nelisp-aot-test-runtime-nl-val-tag-defun
  '(defun nl_val_tag (v)
     (if (= (logand v 7) 0) (heap-read-u8 v 0)
       (if (= (logand v 1) 0)
           (if (= (logand v 7) 4)
               (if (< (heap-read-u64 (- v 4) 8) 0) 14 5)
             (if (= (logand v 7) 6) (logand (heap-read-u8 (- v 6) 0) 63)
               (if (= (logand v 7) 2) 7
                 (heap-read-u8 v 0))))
         (if (= (logand v 3) 1) 2 (if (= v 3) 0 1)))))
  "Lisp source of an `nl_val_tag' stand-in for AOT test sources.")

(defconst nelisp-aot-test-runtime-link-defuns
  (list nelisp-aot-test-runtime-nl-val-tag-defun
        '(defun nl_string_read64 (ptr offset) (ptr-read-u64 ptr offset))
        ;; Mirrors the defun in scripts/nelisp-standalone-build.el.
        '(defun nl_bignum_write_w (sign limb_ptr limb_count _pad)
           (let* ((box (alloc-bytes (+ 8 (logand (+ (* limb_count 4) 7) -8)) 8)) (i 0))
             (seq (ptr-write-u64 box 0 (+ 13 (+ (* sign 64) (* limb_count 256))))
                  (while (< i limb_count)
                    (seq (ptr-write-u32 box (+ 8 (* i 4)) (heap-read-u32 limb_ptr (* i 4)))
                         (setq i (+ i 1))))
                  (if (= (logand limb_count 1) 1)
                      (ptr-write-u32 box (+ 8 (* limb_count 4)) 0)
                    0)
                  (+ box 6)))))
  "Runtime helper defuns a self-contained native fixture must link.")

(provide 'nelisp-aot-test-runtime)
;;; nelisp-aot-test-runtime.el ends here
