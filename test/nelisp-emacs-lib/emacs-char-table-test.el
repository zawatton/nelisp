;;; emacs-char-table-test.el --- ERT for the char-table substrate  -*- lexical-binding: t; -*-

;;; Commentary:

;; Doc 08 §3.1 — exercise `emacs-char-table.el' and, where the host
;; provides a real char-table, cross-check values against it
;; (`should (equal host mine)' style).  The prefixed `emacs-char-table-*'
;; API is representation-independent, so the core tests run in both host
;; and standalone modes; the host cross-checks are skipped under
;; standalone NeLisp (where `make-char-table' is our own implementation).

;;; Code:

(require 'ert)
(require 'emacs-char-table)

(defun emacs-char-table-test--host-p ()
  "Return non-nil when running under a host Emacs with real char-tables."
  (not (emacs-char-table--standalone-p)))

;;;; --- predicate ------------------------------------------------------

(ert-deftest emacs-char-table-test/predicate ()
  (should (emacs-char-table-p (emacs-char-table-make 'test)))
  (should-not (emacs-char-table-p nil))
  (should-not (emacs-char-table-p 42))
  (should-not (emacs-char-table-p "string"))
  (should-not (emacs-char-table-p [1 2 3]))
  (should-not (emacs-char-table-p '(keymap))))

;;;; --- single character roundtrip ------------------------------------

(ert-deftest emacs-char-table-test/ascii-roundtrip ()
  (let ((ct (emacs-char-table-make 'test)))
    (should (null (emacs-char-table-ref ct ?a)))
    (emacs-char-table-set ct ?a 'alpha)
    (should (eq 'alpha (emacs-char-table-ref ct ?a)))
    (should (null (emacs-char-table-ref ct ?b)))))

(ert-deftest emacs-char-table-test/supra-ascii-roundtrip ()
  (let ((ct (emacs-char-table-make 'test)))
    (emacs-char-table-set ct #x3042 'hiragana-a)   ; あ
    (should (eq 'hiragana-a (emacs-char-table-ref ct #x3042)))
    (should (null (emacs-char-table-ref ct #x3043)))))

(ert-deftest emacs-char-table-test/default-value ()
  (let ((ct (emacs-char-table-make 'test 'fallback)))
    (should (eq 'fallback (emacs-char-table-ref ct ?z)))
    (should (eq 'fallback (emacs-char-table-ref ct #x9999)))))

;;;; --- ranges ---------------------------------------------------------

(ert-deftest emacs-char-table-test/set-range-ascii ()
  (let ((ct (emacs-char-table-make 'test)))
    (emacs-char-table-set-range ct '(?0 . ?9) 'digit)
    (should (eq 'digit (emacs-char-table-ref ct ?0)))
    (should (eq 'digit (emacs-char-table-ref ct ?5)))
    (should (eq 'digit (emacs-char-table-ref ct ?9)))
    (should (null (emacs-char-table-ref ct ?a)))))

(ert-deftest emacs-char-table-test/set-range-huge-is-sparse ()
  ;; The isearch-mode-map pattern: setting (#x100 . (max-char)) must not
  ;; materialise ~4M slots.  If this hangs/OOMs the substrate is wrong.
  (let ((ct (emacs-char-table-make 'test)))
    (emacs-char-table-set-range ct (cons #x100 (emacs-char-table-max-char))
                                'printing)
    (should (eq 'printing (emacs-char-table-ref ct #x100)))
    (should (eq 'printing (emacs-char-table-ref ct #x3042)))
    (should (eq 'printing (emacs-char-table-ref ct (emacs-char-table-max-char))))
    ;; ASCII below the range is untouched.
    (should (null (emacs-char-table-ref ct ?a)))))

(ert-deftest emacs-char-table-test/set-range-nil-sets-default ()
  (let ((ct (emacs-char-table-make 'test)))
    (emacs-char-table-set-range ct nil 'def)
    (should (eq 'def (emacs-char-table-range ct nil)))
    (should (eq 'def (emacs-char-table-ref ct #x5000)))))

(ert-deftest emacs-char-table-test/set-range-t-sets-whole ()
  (let ((ct (emacs-char-table-make 'test)))
    (emacs-char-table-set-range ct t 'everything)
    (should (eq 'everything (emacs-char-table-ref ct ?a)))
    (should (eq 'everything (emacs-char-table-ref ct #x3042)))))

(ert-deftest emacs-char-table-test/range-query-cons-samples-from ()
  (let ((ct (emacs-char-table-make 'test)))
    (emacs-char-table-set ct ?A 'cap-a)
    (should (eq 'cap-a (emacs-char-table-range ct (cons ?A ?Z))))))

;;;; --- parent / subtype / extra slots --------------------------------

(ert-deftest emacs-char-table-test/parent-fallback ()
  (let ((parent (emacs-char-table-make 'test))
        (child (emacs-char-table-make 'test)))
    (emacs-char-table-set parent ?x 'from-parent)
    (emacs-char-table-set parent #x4000 'from-parent-hi)
    (emacs-char-table-set-parent child parent)
    (should (eq parent (emacs-char-table-parent child)))
    (should (eq 'from-parent (emacs-char-table-ref child ?x)))
    (should (eq 'from-parent-hi (emacs-char-table-ref child #x4000)))
    (emacs-char-table-set child ?x 'override)
    (should (eq 'override (emacs-char-table-ref child ?x)))))

(ert-deftest emacs-char-table-test/subtype ()
  (should (eq 'my-subtype
             (emacs-char-table-subtype (emacs-char-table-make 'my-subtype)))))

(ert-deftest emacs-char-table-test/extra-slots ()
  (let ((ct (emacs-char-table-make 'test)))
    (should (null (emacs-char-table-extra-slot ct 0)))
    (emacs-char-table-set-extra-slot ct 0 'meta)
    (emacs-char-table-set-extra-slot ct 2 'meta2)
    (should (eq 'meta (emacs-char-table-extra-slot ct 0)))
    (should (eq 'meta2 (emacs-char-table-extra-slot ct 2)))
    (should (null (emacs-char-table-extra-slot ct 1)))))

;;;; --- map ------------------------------------------------------------

(ert-deftest emacs-char-table-test/map-visits-set-entries ()
  (let ((ct (emacs-char-table-make 'test))
        (seen '()))
    (emacs-char-table-set ct ?a 'A)
    (emacs-char-table-set ct ?b 'B)
    (emacs-char-table-map (lambda (k v) (push (cons k v) seen)) ct)
    (should (equal 'A (cdr (assq ?a seen))))
    (should (equal 'B (cdr (assq ?b seen))))
    (should (= 2 (length seen)))))

;;;; --- copy independence ---------------------------------------------

(ert-deftest emacs-char-table-test/copy-is-independent ()
  (let* ((ct (emacs-char-table-make 'test)))
    (emacs-char-table-set ct ?a 'orig)
    (emacs-char-table-set ct #x4000 'orig-hi)
    (let ((copy (emacs-char-table-copy ct)))
      (should (eq 'orig (emacs-char-table-ref copy ?a)))
      (should (eq 'orig-hi (emacs-char-table-ref copy #x4000)))
      (emacs-char-table-set copy ?a 'changed)
      (should (eq 'changed (emacs-char-table-ref copy ?a)))
      (should (eq 'orig (emacs-char-table-ref ct ?a))))))

;;;; --- constants ------------------------------------------------------

(ert-deftest emacs-char-table-test/max-char ()
  (should (= #x3FFFFF (emacs-char-table-max-char)))
  (should (= #x3FFFFF (emacs-char-table-max-char t))))

;;;; --- host cross-checks (real char-table oracle) --------------------

(ert-deftest emacs-char-table-test/host-diff-default-and-set ()
  (skip-unless (emacs-char-table-test--host-p))
  (put 'emacs-char-table-test-subtype 'char-table-extra-slots 3)
  (let ((mine (emacs-char-table-make 'emacs-char-table-test-subtype 'def))
        (host (make-char-table 'emacs-char-table-test-subtype 'def)))
    ;; default value
    (should (equal (aref host ?q) (emacs-char-table-ref mine ?q)))
    ;; ascii set
    (aset host ?a 'alpha)
    (emacs-char-table-set mine ?a 'alpha)
    (should (equal (aref host ?a) (emacs-char-table-ref mine ?a)))
    ;; supra-ascii set
    (aset host #x3042 'hira)
    (emacs-char-table-set mine #x3042 'hira)
    (should (equal (aref host #x3042) (emacs-char-table-ref mine #x3042)))))

(ert-deftest emacs-char-table-test/host-diff-set-range ()
  (skip-unless (emacs-char-table-test--host-p))
  (put 'emacs-char-table-test-subtype 'char-table-extra-slots 3)
  (let ((mine (emacs-char-table-make 'emacs-char-table-test-subtype))
        (host (make-char-table 'emacs-char-table-test-subtype)))
    (set-char-table-range host (cons #x100 (max-char)) 'printing)
    (emacs-char-table-set-range mine (cons #x100 (emacs-char-table-max-char))
                                'printing)
    (dolist (ch (list #x100 #x3042 #x20000))
      (should (equal (aref host ch) (emacs-char-table-ref mine ch))))))

(provide 'emacs-char-table-test)

;;; emacs-char-table-test.el ends here
