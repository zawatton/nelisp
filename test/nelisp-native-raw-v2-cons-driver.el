;;; nelisp-native-raw-v2-cons-driver.el --- Machine rooted CONS proof -*- lexical-binding: t; -*-

(let ((root (getenv "NELISP_CONS_REPO_ROOT")))
  (unless (and root (file-directory-p root))
    (error "CONS smoke requires repository root"))
  (load (expand-file-name "lisp/nelisp-runtime-reload-abi.el" root) nil t)
  (load (expand-file-name "lisp/nelisp-native-load.el" root) nil t))

(require 'cl-lib)
(require 'nelisp-native-load)

(defun nelisp-test-native-raw-v2-cons-smoke ()
  "Call the machine CONS export on evaluator objects and validate refusal."
  (let* ((env (nelisp--native-env))
         (begin (nelisp-native-load--symbol-addr "nl_root_pin_begin_v2"))
         (reserve (nelisp-native-load--symbol-addr "nl_root_pin_reserve_v2"))
         (end (nelisp-native-load--symbol-addr "nl_root_pin_end_v2"))
         (handle (nelisp-native-load-raw-v2-artifact
                  (getenv "NELISP_CONS_ARTIFACT") "nl_native_cons_probe"
                  (getenv "NELISP_CONS_BINARY_SHA")))
         (left (cons 'left (list 'before-gc)))
         (right (cons 'right (list 'before-gc)))
         (result (nelisp-native-load-raw-v2-cons-call handle left right))
         (identity-before (and (eq (car result) left) (eq (cdr result) right)))
         (gc (garbage-collect))
         (identity-after (and (eq (car result) left) (eq (cdr result) right)))
         (mutation-ok nil)
         (ticket nil) (frame nil) (left-slot nil) (right-slot nil) (output nil)
         (bad-index-status nil) (output-unchanged nil))
    (setcdr left '(after-gc))
    (setq mutation-ok (equal (cdr (car result)) '(after-gc)))
    (let ((mutated (copy-sequence handle)))
      (setq mutated (plist-put mutated :imports '("nl_native_cons_v2" "nl_native_cdr_v2")))
      (unless (condition-case nil
                  (progn (nelisp-native-load-raw-v2-cons-call mutated left right) nil)
                (error t))
        (error "CONS caller accepted a mutated import contract")))
    (unwind-protect
        (progn
          (setq ticket (ptr-call begin env 0 0 0 0 0)
                frame (ptr-call reserve env ticket 0 0 0 0)
                left-slot (ptr-call reserve env ticket 0 0 0 0)
                right-slot (ptr-call reserve env ticket 0 0 0 0)
                output (ptr-call reserve env ticket 0 0 0 0))
          (nelisp-native-load-box frame nil env frame)
          (nelisp-native-load-box output 777 env frame)
          (setq bad-index-status
                (ptr-call (plist-get handle :entry) env ticket 1 99 3 0)
                output-unchanged
                (and (= (ptr-read-u64 output 0) 2)
                     (= (ptr-read-u64 output 8) 777))))
      (when (and (integerp ticket) (> ticket 0))
        (unless (= (ptr-call end env ticket 0 0 0 0) 1)
          (error "CONS smoke could not release refusal-test frame"))))
    (unless (and identity-before gc identity-after mutation-ok
                 (= bad-index-status 2) output-unchanged)
      (error "machine CONS proof mismatch: %S"
             (list identity-before identity-after mutation-ok
                   bad-index-status output-unchanged)))
    t))

(nelisp-test-native-raw-v2-cons-smoke)

;;; nelisp-native-raw-v2-cons-driver.el ends here
