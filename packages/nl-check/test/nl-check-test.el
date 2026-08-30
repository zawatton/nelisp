;;; nl-check-test.el --- ERT tests for nl-check -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Coverage for `src/nl-check.el' (Doc 170 sections 6.2, 6.3 and 10):
;; the must-use registry and discard detection, resource linearity
;; (leak / double consume / untracked / moved out), the unsafe-call
;; inventory, and reporting.
;;
;; The negative tests matter more than the positive ones here: a
;; checker that reports findings on correct code is worse than no
;; checker, so every "clean" shape gets an explicit test.
;;
;; No cl-lib or ert-x, so the bodies also run under the standalone
;; harness in `test/nl-check-standalone-smoke.el'.

;;; Code:

(require 'ert)
(require 'nl-check)

;;; Helpers ------------------------------------------------------------

(defun nl-check-test--kinds (form)
  "Return the finding kinds reported for FORM, in order."
  (let ((kinds nil))
    (dolist (finding (nl-check-form form))
      (setq kinds (cons (plist-get finding :kind) kinds)))
    (nreverse kinds)))

(defun nl-check-test--register ()
  "Register the fixtures the resource tests build on."
  (nl-resource-register 'test-fd #'ignore)
  (nl-must-use nl-check-test-open))

;;; must-use registry --------------------------------------------------

(ert-deftest nl-check-must-use-registers ()
  (nl-must-use nl-check-test-alpha nl-check-test-beta)
  (should (nl-check-must-use-p 'nl-check-test-alpha))
  (should (nl-check-must-use-p 'nl-check-test-beta))
  (should-not (nl-check-must-use-p 'nl-check-test-not-registered))
  (should-not (nl-check-must-use-p 42)))

(ert-deftest nl-check-must-use-rejects-non-symbols ()
  (should-error (macroexpand '(nl-must-use "open"))))

(ert-deftest nl-check-must-use-list-is-sorted ()
  (nl-must-use nl-check-test-zzz nl-check-test-aaa)
  (let ((names (nl-check-must-use-list)))
    (should (memq 'nl-check-test-aaa names))
    (should (memq 'nl-check-test-zzz names))
    (should (equal names (sort (copy-sequence names)
                               (lambda (a b)
                                 (string< (symbol-name a)
                                          (symbol-name b))))))))

;;; must-use discard detection -----------------------------------------

(ert-deftest nl-check-must-use-discarded-in-progn ()
  (nl-check-test--register)
  (should (equal (nl-check-test--kinds
                  '(progn (nl-check-test-open "a") 1))
                 '(must-use-discarded))))

(ert-deftest nl-check-must-use-value-position-is-clean ()
  (nl-check-test--register)
  (should-not (nl-check-test--kinds '(progn 1 (nl-check-test-open "a"))))
  (should-not (nl-check-test--kinds '(let ((x (nl-check-test-open "a"))) x)))
  (should-not (nl-check-test--kinds '(setq x (nl-check-test-open "a"))))
  (should-not (nl-check-test--kinds '(list (nl-check-test-open "a")))))

(ert-deftest nl-check-must-use-ignore-is-the-escape-hatch ()
  (nl-check-test--register)
  (should-not (nl-check-test--kinds
               '(progn (ignore (nl-check-test-open "a")) 1))))

(ert-deftest nl-check-must-use-in-let-body-statement ()
  (nl-check-test--register)
  (should (equal (nl-check-test--kinds
                  '(let ((y 1)) (nl-check-test-open "a") y))
                 '(must-use-discarded))))

(ert-deftest nl-check-must-use-in-while-body ()
  (nl-check-test--register)
  ;; Every form in a `while' body is a statement, including the last.
  (should (equal (nl-check-test--kinds
                  '(while c (nl-check-test-open "a")))
                 '(must-use-discarded))))

(ert-deftest nl-check-must-use-in-branch-tail-is-clean ()
  (nl-check-test--register)
  (should-not (nl-check-test--kinds
               '(if c (nl-check-test-open "a") (nl-check-test-open "b"))))
  (should-not (nl-check-test--kinds
               '(cond (c (nl-check-test-open "a")))))
  (should-not (nl-check-test--kinds
               '(when c (nl-check-test-open "a")))))

(ert-deftest nl-check-must-use-skips-quoted-forms ()
  (nl-check-test--register)
  (should-not (nl-check-test--kinds '(progn '(nl-check-test-open "a") 1))))

(ert-deftest nl-check-must-use-in-unwind-protect-cleanup ()
  (nl-check-test--register)
  (should (equal (nl-check-test--kinds
                  '(unwind-protect 1 (nl-check-test-open "a")))
                 '(must-use-discarded))))

;;; Resource linearity -------------------------------------------------

(ert-deftest nl-check-resource-leak-is-reported ()
  (nl-check-test--register)
  (should (equal (nl-check-test--kinds
                  '(let ((r (nl-resource 'test-fd 1)))
                     (nl-resource-handle r)))
                 '(resource-leak))))

(ert-deftest nl-check-resource-dropped-once-is-clean ()
  (nl-check-test--register)
  (should-not (nl-check-test--kinds
               '(let ((r (nl-resource 'test-fd 1)))
                  (nl-resource-handle r)
                  (nl-drop r))))
  (should-not (nl-check-test--kinds
               '(let ((r (nl-resource 'test-fd 1)))
                  (nl-forget r)))))

(ert-deftest nl-check-resource-double-consume-is-reported ()
  (nl-check-test--register)
  (let ((findings (nl-check-form
                   '(let ((r (nl-resource 'test-fd 1)))
                      (nl-drop r)
                      (nl-drop r)))))
    (should (equal (list (plist-get (car findings) :kind)
                         (plist-get (car findings) :count))
                   '(resource-double 2)))))

(ert-deftest nl-check-resource-drop-in-both-branches-is-clean ()
  (nl-check-test--register)
  ;; Only one arm runs, so `if' takes the maximum, not the sum.
  (should-not (nl-check-test--kinds
               '(let ((r (nl-resource 'test-fd 1)))
                  (if c (nl-drop r) (nl-drop r)))))
  (should-not (nl-check-test--kinds
               '(let ((r (nl-resource 'test-fd 1)))
                  (cond (c (nl-drop r)) (t (nl-drop r)))))))

(ert-deftest nl-check-resource-drop-in-loop-is-reported ()
  (nl-check-test--register)
  ;; A loop body can run more than once, so one syntactic drop is two.
  (should (equal (nl-check-test--kinds
                  '(let ((r (nl-resource 'test-fd 1)))
                     (while c (nl-drop r))))
                 '(resource-double))))

(ert-deftest nl-check-resource-captured-by-lambda-is-untracked ()
  (nl-check-test--register)
  (should (equal (nl-check-test--kinds
                  '(let ((r (nl-resource 'test-fd 1)))
                     (funcall (lambda () (nl-drop r)))))
                 '(resource-untracked))))

(ert-deftest nl-check-resource-passed-to-unknown-call-is-untracked ()
  (nl-check-test--register)
  (should (equal (nl-check-test--kinds
                  '(let ((r (nl-resource 'test-fd 1)))
                     (nl-check-test-store r)))
                 '(resource-untracked))))

(ert-deftest nl-check-resource-moved-out-is-clean ()
  (nl-check-test--register)
  ;; Returning the resource moves ownership to the caller.
  (should-not (nl-check-test--kinds
               '(let ((r (nl-resource 'test-fd 1)))
                  r))))

(ert-deftest nl-check-resource-observers-do-not-count-as-moves ()
  (nl-check-test--register)
  (should-not (nl-check-test--kinds
               '(let ((r (nl-resource 'test-fd 1)))
                  (nl-resource-live-p r)
                  (nl-resource-type r)
                  (nl-resource-handle r)
                  (nl-drop r)))))

(ert-deftest nl-check-resource-non-resource-let-is-ignored ()
  (nl-check-test--register)
  (should-not (nl-check-test--kinds '(let ((r (open-file "x"))) r))))

(ert-deftest nl-check-resource-nested-let-is-scanned ()
  (nl-check-test--register)
  (should (equal (nl-check-test--kinds
                  '(defun f ()
                     (let ((a 1))
                       (let ((r (nl-resource 'test-fd 1)))
                         (nl-resource-handle r)))))
                 '(resource-leak))))

;;; Unsafe inventory ---------------------------------------------------

(ert-deftest nl-check-unsafe-call-outside-block-is-reported ()
  (should (equal (nl-check-test--kinds '(progn (ptr-read-u8 p 0)))
                 '(unsafe-call))))

(ert-deftest nl-check-unsafe-call-inside-block-is-clean ()
  (should-not (nl-check-test--kinds '(nl-unsafe (ptr-read-u8 p 0)))))

(ert-deftest nl-check-unsafe-nested-inside-block-is-clean ()
  (should-not (nl-check-test--kinds
               '(nl-unsafe (let ((x 1)) (ptr-write-u8 p 0 x))))))

(ert-deftest nl-check-unsafe-reports-each-call ()
  (let ((findings (nl-check-findings-of-kind
                   (nl-check-form '(progn (alloc-bytes 8 8)
                                          (syscall-direct 1 0 0 0 0 0 0)))
                   'unsafe-call)))
    (should (= (length findings) 2))))

;; The scan used to walk only the cdr of a form, so anything sitting in
;; car position went unseen.  A `let' binding list is (BINDING ...) with
;; BINDING a cons, which put every first binding's initialiser in exactly
;; that blind spot -- and allocating in the first `let*' binding is the
;; house idiom of the standalone build.  Found 2026-08-19; the tree's
;; gated count moved 369 -> 428 when it was fixed.
(ert-deftest nl-check-unsafe-sees-a-let-binding ()
  (should (equal (nl-check-test--kinds '(let ((x (alloc-bytes 1 1))) x))
                 '(unsafe-call))))

(ert-deftest nl-check-unsafe-sees-every-let*-binding ()
  (should (= (length (nl-check-findings-of-kind
                      (nl-check-form '(let* ((a (alloc-bytes 1 1))
                                             (b (alloc-bytes 2 2)))
                                        a))
                      'unsafe-call))
             2)))

(ert-deftest nl-check-unsafe-sees-a-head-element ()
  (should (= (length (nl-check-findings-of-kind
                      (nl-check-form '((alloc-bytes 1 1) (alloc-bytes 2 2)))
                      'unsafe-call))
             2)))

(ert-deftest nl-check-unsafe-block-still-covers-the-head ()
  (should-not (nl-check-test--kinds
               '(nl-unsafe (let ((x (alloc-bytes 1 1))) x)))))

;; Quoted forms stay out of the gated count -- an opcode table entry like
;; (ptr-read-u16 . 39) is data, not a call -- but they are counted apart
;; so the exclusion is stated rather than silent.
(ert-deftest nl-check-unsafe-skips-quoted-forms ()
  (should-not (nl-check-test--kinds '(progn '(alloc-bytes 1 1)))))

(ert-deftest nl-check-quoted-unsafe-counts-what-the-scan-skips ()
  (let ((f (make-temp-file "nl-check-quoted-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file f
            (insert "(defconst gen--source '(seq (defun a () (alloc-bytes 1 1))))\n"
                    "(defun live () (ptr-read-u8 p 0))\n"))
          (should (= (length (nl-check-findings-of-kind
                              (nl-check-file f) 'unsafe-call))
                     1))
          (should (= (length (nl-check-file-quoted-unsafe f)) 1))
          (should (eq (plist-get (car (nl-check-file-quoted-unsafe f)) :kind)
                      'unsafe-call-quoted)))
      (delete-file f))))

;;; Entry points and reporting -----------------------------------------

(ert-deftest nl-check-forms-concatenates ()
  (nl-check-test--register)
  (should (= (length (nl-check-forms
                      '((progn (nl-check-test-open "a") 1)
                        (progn (nl-check-test-open "b") 1))))
             2)))

(ert-deftest nl-check-clean-form-has-no-findings ()
  (nl-check-test--register)
  (should-not (nl-check-form '(defun f (x) (+ x 1)))))

(ert-deftest nl-check-report-of-nothing ()
  (should (equal (nl-check-report nil) "nl-check: no findings\n")))

(ert-deftest nl-check-report-lists-each-finding ()
  (nl-check-test--register)
  (let ((report (nl-check-report
                 (nl-check-form
                  '(let ((r (nl-resource 'test-fd 1)))
                     (nl-resource-handle r))))))
    (should (string-match-p "1 finding" report))
    (should (string-match-p "resource-leak" report))
    (should (string-match-p "never dropped" report))))

(ert-deftest nl-check-findings-of-kind-filters ()
  (nl-check-test--register)
  (let ((findings (nl-check-form
                   '(progn
                      (nl-check-test-open "a")
                      (let ((r (nl-resource 'test-fd 1)))
                        (nl-resource-handle r))))))
    (should (= (length (nl-check-findings-of-kind
                        findings 'must-use-discarded))
               1))
    (should (= (length (nl-check-findings-of-kind
                        findings 'resource-leak))
               1))
    (should-not (nl-check-findings-of-kind findings 'unsafe-call))))

(ert-deftest nl-check-dotted-forms-do-not-crash ()
  "Real source files contain dotted forms (alist literals in macro
positions); every walker must tolerate improper lists (found via the
unsafe-inventory scan of lisp/nelisp-stdlib-os.el)."
  (dolist (form '((while c (f . 9))
                  (when c (f . 9))
                  (progn (f . 9) (g . 9))
                  (let ((x (f . 9))) (g . 9))
                  (cond ((c) (f . 9)))
                  (unwind-protect (f . 9) (g . 9))
                  (f (g . 9) . 9)))
    (should (listp (nl-check-form form)))))
;;;; Checking after expansion (the MIR position)

(ert-deftest nl-check-expansion-finds-a-leak-a-reader-cannot-see ()
  "A leak that exists only after a macro expands must be reported.
Read-level checking sees `(m-acquire 3)' -- an ordinary call, with no
resource in sight -- so the violation goes by unreported.  This is why
Rust checks borrows on MIR rather than on surface syntax: sugar can
hide a violation from anything that reads the sugar."
  (nl-check-test--register)
  (let* ((forms '((defmacro nl-check-test--m-acquire (n)
                    `(let ((r (nl-resource 'test-fd ,n)))
                       (nl-resource-handle r)))
                  (defun nl-check-test--m-user ()
                    (nl-check-test--m-acquire 3))))
         (before (mapcar (lambda (f) (plist-get f :kind))
                         (nl-check-forms forms)))
         (after (mapcar (lambda (f) (plist-get f :kind))
                        (nl-check-expanded-forms forms))))
    (should-not (memq 'resource-leak before))
    (should (memq 'resource-leak after))))

(ert-deftest nl-check-expansion-keeps-a-correct-macro-clean ()
  "The same shape, with the expansion dropping, must stay clean."
  (nl-check-test--register)
  (should-not
   (nl-check-expanded-forms
    '((defmacro nl-check-test--m-with (n &rest body)
        `(let ((r (nl-resource 'test-fd ,n)))
           (prog1 (progn ,@body)
             (nl-drop r))))
      (defun nl-check-test--m-ok ()
        (nl-check-test--m-with 3 42))))))

(ert-deftest nl-check-expansion-does-not-define-the-macro-globally ()
  "A file's macros go into a local environment, not into this Emacs.
Checking a file must not be able to change the process doing the
checking."
  (nl-check-test--register)
  (nl-check-expanded-forms
   '((defmacro nl-check-test--m-should-not-exist (n) `(list ,n))))
  (should-not (fboundp 'nl-check-test--m-should-not-exist)))

(ert-deftest nl-check-expansion-passes-through-an-unexpandable-form ()
  "One confusing form must not cost the answer for the whole file."
  (nl-check-test--register)
  (should (equal (nl-check-expand-forms '((quote (1 . 2))))
                 '((quote (1 . 2))))))

;;;; Backquote templates are data

(ert-deftest nl-check-macro-template-is-not-code ()
  "A resource `let' inside a `defmacro' template must not be reported.
The template is what the macro OUTPUTS; nothing there runs at the
definition site.  Walking it reported a leak in every macro that builds
one, and said nothing about the expansion where the resource actually
appears -- a false alarm standing exactly where the real answer should
have been."
  (nl-check-test--register)
  (should-not (nl-check-test--kinds
               '(defmacro nl-check-test--acquire (n)
                  `(let ((r (nl-resource 'test-fd ,n)))
                     (nl-resource-handle r))))))

(ert-deftest nl-check-unquote-position-is-still-code ()
  "`,' and `,@' put live code into a template, and it must be walked."
  (nl-check-test--register)
  (should (equal (nl-check-test--kinds
                  '(defmacro nl-check-test--wrap ()
                     `(progn
                        ,(let ((r (nl-resource 'test-fd 1)))
                           (nl-resource-handle r)))))
                 '(resource-leak))))


(provide 'nl-check-test)

;;; nl-check-test.el ends here
