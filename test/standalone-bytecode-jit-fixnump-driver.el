;;; -*- lexical-binding: t; -*-
(require 'nelisp-bytecode-jit)
(load (concat (getenv "NELISP_REPO_ROOT")
              "/lisp/nelisp-bytecode-jit.el") nil nil t)
(setq nelisp-bytecode-jit-threshold 1)
(let* ((function
        (make-byte-code
         257 (unibyte-string 137 168 133 14 0 8 1 88 133 14 0
                             137 9 88 135)
         [most-negative-fixnum most-positive-fixnum] 3))
       (fingerprint
        (secure-hash 'sha256
                     (prin1-to-string
                      (list (aref function 0) (aref function 1)
                            (aref function 2) (aref function 3)))))
       (inputs (list 17 most-negative-fixnum most-positive-fixnum
                     (1+ most-positive-fixnum) 1.0 "text"))
       (vm-before (plist-get (nelisp-bytecode-jit-status) :native-calls))
       (vm-values
        (let ((nelisp-bytecode-jit--dispatch-active t))
          (mapcar (lambda (value) (funcall function value)) inputs)))
       (vm-after (plist-get (nelisp-bytecode-jit-status) :native-calls))
       (native-before vm-after)
       (started (current-time))
       (native-value (funcall function 17))
       (ended (current-time))
       (compile-seconds
        (+ (* 65536 (- (nth 0 ended) (nth 0 started)))
           (- (nth 1 ended) (nth 1 started))
           (/ (- (nth 2 ended) (nth 2 started)) 1000000.0)))
       (native-after (plist-get (nelisp-bytecode-jit-status) :native-calls))
       (string-before native-after)
       (string-value (funcall function "text"))
       (string-after (plist-get (nelisp-bytecode-jit-status) :native-calls))
       (fallbacks
        (list
         (let ((before (plist-get (nelisp-bytecode-jit-status) :native-calls)))
           (cons (funcall function most-negative-fixnum)
                 (= before (plist-get (nelisp-bytecode-jit-status) :native-calls))))
         (let ((before (plist-get (nelisp-bytecode-jit-status) :native-calls)))
           (cons (funcall function most-positive-fixnum)
                 (= before (plist-get (nelisp-bytecode-jit-status) :native-calls))))
         (let ((before (plist-get (nelisp-bytecode-jit-status) :native-calls)))
           (cons (funcall function (1+ most-positive-fixnum))
                 (= before (plist-get (nelisp-bytecode-jit-status) :native-calls))))
         (let ((before (plist-get (nelisp-bytecode-jit-status) :native-calls)))
           (cons (funcall function 1.0)
                 (= before (plist-get (nelisp-bytecode-jit-status) :native-calls)))))))
  (unless (and (equal fingerprint
                      "102e639e742351efbc457d8517db951ead880750dbc9eb1b96409f5fe063762d")
               (equal vm-values '(t t t nil nil nil))
               (= vm-before vm-after)
               (eq native-value t)
               (= (- native-after native-before) 1)
               (null string-value)
               (= (- string-after string-before) 1)
               (equal fallbacks '((t . t) (t . t) (nil . t) (nil . t))))
    (error "Vendor fixnump JIT parity mismatch: %S"
           (list fingerprint vm-before vm-after vm-values native-value
                native-before native-after fallbacks)))
  (princ (prin1-to-string
          (list :fingerprint fingerprint :vm-values vm-values
                :native-value native-value
                :native-delta (- native-after native-before)
                :string-value string-value
                :string-native-delta (- string-after string-before)
                :fallbacks fallbacks :compile-seconds compile-seconds))))
