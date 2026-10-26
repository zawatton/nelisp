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

(ert-deftest nelisp-prelude-bytecode-vendor-transfer-preserves-unicode-and-raw-bytes ()
  (let* ((source (concat ";;; -*- lexical-binding: t; coding: utf-8 -*-\n"
                         "(defun nelisp--bytecode-unicode-roundtrip (flag)\n"
                         "  \"Unicode documentation: λ …\"\n"
                         "  (if flag '[… \"λ\" \"\\377\"] 'other))\n"))
         (entry (car (nelisp-prelude-bytecode-vendor-compile-functions
                      source "unicode-roundtrip.el" nil)))
         (value (funcall (nth 1 entry) t)))
    (should (eq (aref value 0) '…))
    (should (equal (aref value 1) "λ"))
    (should (multibyte-string-p (aref value 1)))
    (should (equal (string-to-list (aref value 2)) '(255)))
    (should-not (multibyte-string-p (aref value 2)))
    (should (string-prefix-p "Unicode documentation: λ …" (nth 2 entry)))))

;; bcadopt: cl-defstruct place lowering (the REVERT NOTE in
;; scripts/nelisp-prelude-bytecode.el).

(defconst nelisp-prelude-bytecode-selector-test--struct-source
  (concat "(cl-defstruct (nbx (:constructor nbx--make) (:copier nil))\n"
          "  \"doc\" a (b 1))\n"
          "(cl-defstruct (nby (:include nbx) (:conc-name nbyy-)) c)\n"
          "(defun nbx-bump (r)\n  (setf (nbx-b r) (1+ (nbx-b r))))\n"
          "(defun nby-push (r v)\n  (push v (nbyy-c r)))\n"
          "(defun nbx-plain (r) (nbx-a r))\n"))

(defun nelisp-prelude-bytecode-selector-test--digest (source name)
  (let* ((start (string-match (format "(defun %s\\_>" name) source))
         (end (cdr (read-from-string source start))))
    (secure-hash 'sha256 (substring source start end))))

(ert-deftest nelisp-prelude-bytecode-struct-accessors-follow-the-prelude-layout ()
  (should (equal (nelisp-prelude-bytecode--struct-accessors
                  nelisp-prelude-bytecode-selector-test--struct-source)
                 '((nbx-a . 0) (nbx-b . 1)
                   (nbyy-a . 0) (nbyy-b . 1) (nbyy-c . 2)))))

(ert-deftest nelisp-prelude-bytecode-struct-setf-lowers-to-record-set ()
  (let* ((source nelisp-prelude-bytecode-selector-test--struct-source)
         (fixtures
          (mapcar (lambda (name)
                    `(,name "" "" "pass" "fixture" "bytecode"
                            ,(nelisp-prelude-bytecode-selector-test--digest
                              source (symbol-name name)) "-"))
                  '(nbx-bump nby-push nbx-plain)))
         (result (nelisp-prelude-bytecode-transform source "probe.el" fixtures))
         (text (nth 0 result)))
    (should (= (nth 1 result) 3))
    (should (string-match-p "nelisp--record-set" text))
    (should-not (string-match-p "(setf" (replace-regexp-in-string
                                          "(defun[^\n]*\n" "" text)))
    ;; The host symbols must be left exactly as found.
    (should-not (get 'nbx-b 'gv-expander))))

(ert-deftest nelisp-prelude-bytecode-selector-rejects-host-cl-struct-artifacts ()
  (let* ((source "(defun foo (x) (cl--class-p x))\n")
         (result (nelisp-prelude-bytecode-transform
                  source "probe.el"
                  `((foo "" "" "pass" "fixture" "bytecode"
                         ,(nelisp-prelude-bytecode-selector-test--digest
                           source "foo") "-"))))
         (row (car (nth 2 result))))
    (should (= (nth 1 result) 0))
    (should (equal (nth 4 row) "host-cl-struct-artifact-in-constants"))))

(ert-run-tests-batch-and-exit)

;;; nelisp-prelude-bytecode-selector-test.el ends here
