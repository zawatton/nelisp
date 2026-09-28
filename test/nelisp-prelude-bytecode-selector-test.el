;;; nelisp-prelude-bytecode-selector-test.el --- Selector traversal and provenance -*- lexical-binding: t; -*-

(require 'ert)
(require 'nelisp-prelude-bytecode)

(ert-deftest nelisp-prelude-bytecode-selector-walks-guarded-defuns-and-keeps-lines ()
  (let* ((source
          (concat "; retained comment\n"
                  "(progn\n"
                  "  (unless (fboundp 'foo)\n"
                  "    (defun foo (x) (car x)))\n"
                  "  (if t\n"
                  "      (defun bar (x) (cdr x))\n"
                  "    nil))\n"
                  "(defun tail (x) (car x))\n"))
         (digest (lambda (name)
                   (let* ((start (string-match
                                  (format "(defun %s\\_>" name) source))
                          (end (cdr (read-from-string source start))))
                     (secure-hash 'sha256 (substring source start end)))))
         (fixtures `((foo "(foo '(a))" "a" "pass" "fixture" "bytecode"
                          ,(funcall digest "foo"))
                     (bar "(bar '(a))" "(a)" "pass" "fixture" "bytecode"
                          ,(funcall digest "bar"))))
         (result (nelisp-prelude-bytecode-transform source "probe.el" fixtures))
         (rows (nth 2 result))
         (output (nth 0 result)))
    (should (= (nth 1 result) 2))
    (should (equal (mapcar (lambda (row) (list (nth 2 row) (nth 1 row)
                                                (nth 3 row) (nth 9 row)))
                           rows)
                   '((foo 4 "adopt" "a")
                     (bar 6 "adopt" "(a)")
                     (tail 8 "reject" ""))))
    (should (string-prefix-p "; retained comment\n(progn\n  (unless (fboundp 'foo)\n"
                             output))
    (should (string-suffix-p "\n(defun tail (x) (car x))\n" output))
    (should (equal (nelisp-prelude-bytecode-source-defuns source)
                   '((defun foo (x) (car x))
                     (defun bar (x) (cdr x))
                     (defun tail (x) (car x)))))))

(ert-deftest nelisp-prelude-bytecode-selector-rejects-stale-parity-digest ()
  (let* ((source "(defun foo (x) (car x))\n")
         (result (nelisp-prelude-bytecode-transform
                  source "stale.el"
                  '((foo "(foo '(a))" "a" "pass" "fixture" "bytecode"
                         "not-the-source-digest"))))
         (row (car (nth 2 result))))
    (should (= (nth 1 result) 0))
    (should (equal (nth 4 row) "source-form-digest-mismatch"))))

(ert-deftest nelisp-prelude-bytecode-selector-requires-explicit-pop-handler-evidence ()
  (let* ((source "(defun foo (n list) (take n list))\n")
         (digest (secure-hash 'sha256
                              (substring source 0 (1- (length source)))))
         (result (nelisp-prelude-bytecode-transform
                  source "pop-handler.el"
                  `((foo "(foo 1 '(a b))" "(a)" "pass" "fixture" "bytecode"
                         ,digest ""))))
         (row (car (nth 2 result))))
    (should (= (nth 1 result) 0))
    (should (equal (nth 4 row) "opcode-32-not-explicitly-verified"))))

(ert-run-tests-batch-and-exit)

;;; nelisp-prelude-bytecode-selector-test.el ends here
