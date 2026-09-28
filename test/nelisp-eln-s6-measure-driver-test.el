;;; nelisp-eln-s6-measure-driver-test.el --- pure-helper coverage -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Covers only the pure helpers in nelisp-eln-s6-measure-driver.el (corpus
;; reading, capture/compare, and timing arithmetic).  It never sets
;; NELISP_ELN_S6_PHASE, so loading the driver here only defines functions;
;; see the "when (getenv ...)" guard at the bottom of that file.

(require 'ert)

(load (expand-file-name
       "nelisp-eln-s6-measure-driver.el"
       (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(ert-deftest nelisp-eln-s6-measure-read-corpus-parses-argument-lists ()
  (let ((file (make-temp-file "nelisp-eln-s6-measure-corpus" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert ";; comment\n((0) (1 2) (\"x\"))\n"))
          (should (equal (nelisp-eln-s6-measure-read-corpus file)
                         '((0) (1 2) ("x")))))
      (delete-file file))))

(ert-deftest nelisp-eln-s6-measure-read-corpus-rejects-non-list-entry ()
  (let ((file (make-temp-file "nelisp-eln-s6-measure-corpus" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file file (insert "(0 (1))\n"))
          (should-error (nelisp-eln-s6-measure-read-corpus file)))
      (delete-file file))))

(ert-deftest nelisp-eln-s6-measure-capture-returns-ok-on-success ()
  (should (equal (nelisp-eln-s6-measure-capture #'+ '(1 2)) '(:ok 3))))

(ert-deftest nelisp-eln-s6-measure-capture-returns-err-on-signal ()
  (let ((capture (nelisp-eln-s6-measure-capture #'car '(5))))
    (should (eq (car capture) :err))
    (should (equal (cadr capture) '(wrong-type-argument listp 5)))))

(ert-deftest nelisp-eln-s6-measure-capture-equal-agrees-on-matching-captures ()
  (should (nelisp-eln-s6-measure-capture-equal '(:ok 3) '(:ok 3)))
  (should-not (nelisp-eln-s6-measure-capture-equal '(:ok 3) '(:ok 4)))
  (should-not (nelisp-eln-s6-measure-capture-equal
               '(:ok 3) '(:err (wrong-type-argument listp 5)))))

(ert-deftest nelisp-eln-s6-measure-ns-per-call-converts-seconds ()
  (should (= (nelisp-eln-s6-measure-ns-per-call 1.0 1000) 1000000))
  (should (= (nelisp-eln-s6-measure-ns-per-call 0.0 1000) 0))
  (should (= (nelisp-eln-s6-measure-ns-per-call 1.0 0) 0)))

(ert-deftest nelisp-eln-s6-measure-time-rounds-runs-rounds-times-calls ()
  (let ((count 0))
    (let ((elapsed (nelisp-eln-s6-measure-time-rounds
                    (lambda () (setq count (1+ count))) 5 7)))
      (should (= count 35))
      (should (numberp elapsed))
      (should (>= elapsed 0)))))

(ert-deftest nelisp-eln-s6-measure-time-rounds-reports-the-minimum-round ()
  ;; A thunk whose cost drops after the first round: the minimum elapsed
  ;; time must come from a fast round, not the first (slow) one.
  (let ((slow-rounds-left 1))
    (let ((elapsed (nelisp-eln-s6-measure-time-rounds
                    (lambda ()
                      (when (> slow-rounds-left 0)
                        (dotimes (_ 20000) (+ 1 1))))
                    3 1)))
      (should (numberp elapsed)))
    (setq slow-rounds-left 0)))

(ert-deftest nelisp-eln-s6-measure-short-condition-keeps-symbol-reason ()
  (should (equal (nelisp-eln-s6-measure-short-condition
                  '(nelisp-eln-registration-error
                    top-level-instructions-not-admitted "F00-C-NAME"))
                 "nelisp-eln-registration-error:top-level-instructions-not-admitted")))

(ert-deftest nelisp-eln-s6-measure-short-condition-drops-binary-payload ()
  ;; The second data element (here a raw byte string standing in for a
  ;; disassembled function body) must never appear in the rendering.
  (let ((rendered (nelisp-eln-s6-measure-short-condition
                   (list 'nelisp-eln-registration-error 'invalid-leaf-code-size
                         (make-string 200 ?\1)))))
    (should (equal rendered
                   "nelisp-eln-registration-error:invalid-leaf-code-size"))
    (should (< (length rendered) 100))))

(ert-deftest nelisp-eln-s6-measure-short-condition-falls-back-to-symbol-only ()
  (should (equal (nelisp-eln-s6-measure-short-condition '(error))
                 "error")))

(ert-deftest nelisp-eln-s6-measure-wrapper-symbol-prefixes-and-preserves-dashes ()
  (should (eq (nelisp-eln-s6-measure-wrapper-symbol "byte-compile-constant")
              's6-corpus--byte-compile-constant))
  (should (eq (nelisp-eln-s6-measure-wrapper-symbol "cconv--convert-function")
              's6-corpus--cconv--convert-function)))

(ert-deftest nelisp-eln-s6-measure-cycle-thunk-cycles-and-captures-errors ()
  (let* ((corpus '((1) (2) (5)))
         (thunk (nelisp-eln-s6-measure-cycle-thunk
                 (lambda (x) (if (= x 5) (error "boom") (* x 10))) corpus))
         (results (list (funcall thunk) (funcall thunk) (funcall thunk)
                        (funcall thunk))))
    (should (equal (nth 0 results) '(:ok 10)))
    (should (equal (nth 1 results) '(:ok 20)))
    (should (eq (car (nth 2 results)) :err))
    ;; The fourth call wraps back around to the first corpus entry.
    (should (equal (nth 3 results) '(:ok 10)))))

;;; nelisp-eln-s6-measure-driver-test.el ends here
