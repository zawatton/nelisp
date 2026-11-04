;;; nelisp-dsl-literal-range-test.el --- DSL bound literals vs arena rewrite  -*- lexical-binding: t; -*-

;;; Commentary:

;; `nelisp-standalone--chunk-arena-rewrite' turns every integer atom in
;; [BASE, BASE + #x1000) into a load of the runtime arena base, for each
;; chunked target's BASE.  A literal meant as a numeric bound (2^28 on Linux,
;; 2^35 on macOS) that falls in that range silently becomes an address, so a
;; size check compares against the arena base and never fires.  This test
;; reads the build script with the real reader and rejects any comparison
;; whose direct argument is such a literal.  Address comparisons go through
;; `heap-read-u64' and similar forms and are not direct arguments.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defconst nelisp-dsl-literal-range-test--bases
  '(#x10000000 #x70000000 #x800000000)
  "Arena bases of the Linux, default Windows, and macOS targets.")

(defconst nelisp-dsl-literal-range-test--compare-ops
  '(< > <= >= = /=)
  "Comparison operators whose literal operands must be numeric values.")

(defun nelisp-dsl-literal-range-test--in-range-p (n)
  "Return non-nil if integer N is rewritten as an arena address."
  (cl-some (lambda (b) (and (<= b n) (< n (+ b #x1000))))
           nelisp-dsl-literal-range-test--bases))

(defun nelisp-dsl-literal-range-test--scan (file)
  "Return a list of offending comparison forms in FILE."
  (let ((hits nil))
    (cl-labels ((walk (form)
                  (when (consp form)
                    (when (and (memq (car form)
                                     nelisp-dsl-literal-range-test--compare-ops)
                               (proper-list-p form)
                               (cl-some (lambda (a)
                                          (and (integerp a)
                                               (nelisp-dsl-literal-range-test--in-range-p a)))
                                        (cdr form)))
                      (push form hits))
                    (while (consp form)
                      (walk (car form))
                      (setq form (cdr form))))))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (condition-case nil
            (while t (walk (read (current-buffer))))
          (end-of-file nil))))
    (nreverse hits)))

(defun nelisp-dsl-literal-range-test--file ()
  "Return the build script path, overridable by NELISP_DSL_LITERAL_FILE."
  (or (getenv "NELISP_DSL_LITERAL_FILE")
      (expand-file-name "../scripts/nelisp-standalone-build.el"
                        (file-name-directory
                         (or load-file-name buffer-file-name)))))

(defvar nelisp-dsl-literal-range-test--path (nelisp-dsl-literal-range-test--file))

(ert-deftest nelisp-dsl-literal-range-detector-sees-violation ()
  "The scanner reports a bound literal in the rewrite range."
  (let ((tmp (make-temp-file "dsl-lit" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert "(defun f (n) (if (> n 268435456) 1 (if (< n 34359738368) 2 0)))\n"
                    "(defun g (n) (if (> (- n 1) 268435455) 1 (heap-read-u64 268435456 0)))\n"))
          (should (equal (length (nelisp-dsl-literal-range-test--scan tmp)) 2)))
      (delete-file tmp))))

(ert-deftest nelisp-dsl-literal-range-build-script-clean ()
  "No comparison in the build script has an arena-rewrite-range literal."
  (should (equal (nelisp-dsl-literal-range-test--scan
                  nelisp-dsl-literal-range-test--path)
                 nil)))

(provide 'nelisp-dsl-literal-range-test)

;;; nelisp-dsl-literal-range-test.el ends here
