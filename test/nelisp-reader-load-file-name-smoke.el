;;; nelisp-reader-load-file-name-smoke.el --- Native #$ reader smoke -*- lexical-binding: t; -*-

(defun nelisp-reader-load-file-name-smoke-assert (condition label)
  (unless condition
    (error "reader #$ smoke failed: %s" label)))

(let ((load-file-name nil))
  (nelisp-reader-load-file-name-smoke-assert
   (equal (read-from-string "#$") '(nil . 2))
   "nil binding"))

(nelisp-reader-load-file-name-smoke-assert
 (eq (car (read-from-string "#$")) load-file-name)
 "load-file-name from loaded test")
(nelisp-reader-load-file-name-smoke-assert
 (eq (read "#$") load-file-name)
 "read entry point")

(let* ((marker (copy-sequence "reader-context"))
       (load-file-name marker)
       (read-value (car (read-from-string "#$")))
       (nested (car (read-from-string "(list #$)")))
       (all-form (car (nelisp--read-all-from-string-native
                       "(list #$)"))))
  (nelisp-reader-load-file-name-smoke-assert
   (eq read-value marker) "dynamic value identity")
  (nelisp-reader-load-file-name-smoke-assert
   (and (eq (car nested) 'list) (eq (cadr nested) marker))
   "nested token")
  (nelisp-reader-load-file-name-smoke-assert
   (and (eq (car all-form) 'list) (eq (cadr all-form) marker))
   "all-forms helper context"))

(let* ((marker (copy-sequence "reader-context"))
       (load-file-name marker)
       (escaped (car (read-from-string "\\#\\$"))))
  (nelisp-reader-load-file-name-smoke-assert
   (and (symbolp escaped)
        (string= (symbol-name escaped) "#$")
        (not (eq escaped marker)))
   "escaped literal symbol"))

(let* ((marker (copy-sequence "reader-context"))
       (load-file-name marker)
       (value (car (read-from-string
                    "[#1=(#$ . #1#) #1# #2=#:x #2#]"))))
  (nelisp-reader-load-file-name-smoke-assert
   (and (eq (car (aref value 0)) marker)
        (eq (aref value 0) (aref value 1))
        (eq (aref value 2) (aref value 3)))
   "mixed fallback preserves #$ value, cycle and gensym identity"))

(let* ((marker (copy-sequence "reader-context"))
       (load-file-name marker)
       (result (read-from-string "xx#$tail" 2 4)))
  (nelisp-reader-load-file-name-smoke-assert
   (and (eq (car result) marker) (= (cdr result) 4))
   "start and end positions"))

(let* ((marker (copy-sequence "reader-context"))
       (load-file-name marker)
       (result (read-from-string "#$tail")))
  (nelisp-reader-load-file-name-smoke-assert
   (and (eq (car result) marker) (= (cdr result) 2))
   "dispatch consumes exactly two characters"))

(nelisp-reader-load-file-name-smoke-assert
 (condition-case nil (progn (read-from-string "#&") nil)
   (error t))
 "malformed sharp")
(nelisp-reader-load-file-name-smoke-assert
 (equal (read-from-string "42") '(42 . 2))
 "reader state restored after malformed input")

(princ "READER-LFN-SMOKE:PASS\n")
