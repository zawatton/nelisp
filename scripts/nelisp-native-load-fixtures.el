;;; nelisp-native-load-fixtures.el --- artifacts for the loader check  -*- lexical-binding: t; -*-

;;; Commentary:

;; Compiles the `.neln' artifacts `test/nelisp-native-load-driver.el'
;; loads.  Host Emacs builds them; the reader runs them.  Kept beside the
;; driver so the two lists cannot drift apart unnoticed -- a missing
;; artifact fails the driver's case rather than being skipped.

;;; Code:

(require 'nelisp-artifact)
(require 'nelisp-aot-compiler)

(defconst nelisp-native-load-fixtures
  '(("inc1" "(defun inc1 (x) (1+ x))")
    ("nested" "(defun nested (x) (1- (1+ (1+ x))))")
    ("carlist" "(defun carlist (x) (car (list (1+ x) 0)))")
    ("add3" "(defun add3 (a b c) (+ a (+ b c)))")
    ("zero" "(defun zero () 7)")
    ("six" "(defun six (a b c d e f) (+ a (+ b (+ c (+ d (+ e f))))))")
    ("strlen" "(defun strlen (s) (length s))")
    ("symname" "(defun symname (x) (symbol-name (quote abc)))")
    ("istrue" "(defun istrue (x) (integerp x))")
    ("isfalse" "(defun isfalse (x) (stringp x))")
    ;; Vector indices exercise the immediate-to-boxed boundary passed to
    ;; `nl_vector_slot_ptr'.  Index zero alone does not catch a tagged
    ;; integer being used as the raw native index.
    ("vget" "(defun vget (n) (aref (vector 7 8 9) 0))")
    ("plainref" "(defun plainref (n) (aref (vector 7 8 9) 1))")
    ("nestvec" "(defun nestvec (n) (aref (aref (vector 1 (vector 7 8 9) 3) 1) 0))")
    ("vsetget" "(defun vsetget (n) (let ((v (vector 0 0 0))) (aset v 1 n) (aref v 1)))")
    ("rawloop" "(defun rawloop (n) (let ((i 0)) (seq (while (< i n) (setq i (+ i 1))) i)))")
    ("dispatchint" "(defun dispatchint (s) (let ((i 0)) (seq (setq i (length s)) (+ i 10))))")
    ("cell-acquire" "(defun cell-acquire (n) (let ((c (vector 1 (vector 7 8 9) 0))) (let ((state (aref c 2))) (seq (aset c 2 (+ state 1)) (aref (aref c 1) 0)))))")
    ("fat-roundtrip" "(defun fat-roundtrip () (let ((p (nl-ptr-make (alloc-bytes 8 8) 8 0))) (nl-ptr-set-u8 p 2 42) (nl-ptr-ref-u8 p 2)))")
    ("fat-derived" "(defun fat-derived () (let ((p (nl-ptr-make (alloc-bytes 16 8) 16 0))) (let ((s (nl-ptr-slice p 2 8))) (let ((q (nl-ptr-slice s 3 4))) (nl-ptr-set-u8 q 1 42) (nl-ptr-ref-u8 q 1)))))")
    ("fat-derived-loop" "(defun fat-derived-loop () (let ((p (nl-ptr-make (alloc-bytes 16 8) 16 0))) (let ((s (nl-ptr-slice p 2 8))) (let ((q (nl-ptr-slice s 3 4))) (nl-ptr-set-u8 q 0 1) (nl-ptr-set-u8 q 1 2) (nl-ptr-set-u8 q 2 3) (nl-ptr-set-u8 q 3 4) (let ((i 0) (acc 0)) (seq (while (< i 4) (setq acc (+ acc (nl-ptr-ref-u8 q i))) (setq i (+ i 1))) acc))))))"))
  "(NAME SOURCE) for each artifact the loader driver calls.")

(defun nelisp-native-load-fixtures-build (dir)
  "Compile every fixture into DIR as NAME.neln."
  (make-directory dir t)
  (dolist (entry nelisp-native-load-fixtures)
    (let* ((name (car entry))
           (el (expand-file-name (concat name ".el") dir))
           (neln (expand-file-name (concat name ".neln") dir))
           ;; Vector primitives lower through the generic user-call path.
           ;; Close that path over the reader's calln provider so it can be
           ;; loaded in-process rather than leaving an `aref' relocation.
           (nelisp-aot-compiler--dynamic-user-calls t)
           (nelisp-aot-compiler--aot-fat-pointer-check-elision t))
      (with-temp-file el
        (insert (cadr entry) "\n" (format "(provide '%s)\n" name)))
      (nelisp-artifact-compile-file el neln nil nil nil nil nil 'neln)
      (let* ((native (plist-get (nelisp-artifact--read-payload neln) :native))
             (meta (car (plist-get native :defuns))))
        (message "[fixture] %-10s arity=%s rt=%s text=%s externs=%S"
                 name (plist-get meta :arity) (plist-get meta :rt-slot-count)
                 (plist-get meta :size)
                 (plist-get native :extern-symbols))))))

(defun nelisp-native-load-fixtures-main ()
  "Entry point: build into the directory named by NELISP_ARTIFACT_DIR."
  (let ((dir (getenv "NELISP_ARTIFACT_DIR")))
    (unless dir
      (error "nelisp-native-load-fixtures: NELISP_ARTIFACT_DIR is unset"))
    (nelisp-native-load-fixtures-build dir)))

(provide 'nelisp-native-load-fixtures)

;;; nelisp-native-load-fixtures.el ends here
