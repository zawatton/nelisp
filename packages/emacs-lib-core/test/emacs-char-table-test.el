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

(ert-deftest emacs-char-table-test/logical-identity-query ()
  (let* ((record (record 'nelisp--syntax-table (make-hash-table :test 'eql) nil))
         (view (emacs-char-table--storage record))
         (other (emacs-char-table-make 'syntax-table)))
    (should (emacs-char-table-same-p record view))
    (should (emacs-char-table-same-p view record))
    (should-not (emacs-char-table-same-p record other))
    (should-not (emacs-char-table-parent record))
    (when (emacs-char-table-test--host-p)
      (let ((host (make-char-table 'syntax-table)))
        (should (emacs-char-table-same-p host host))
        (should-not (emacs-char-table-same-p host (copy-sequence host)))))))

(ert-deftest emacs-char-table-test/parent-bootstrap-storage-identity ()
  (let* ((record (record 'nelisp--syntax-table (make-hash-table :test 'eql) nil))
         (view (emacs-char-table--storage record)))
    (should-error (emacs-char-table-set-parent record view))
    (should-error (emacs-char-table-set-parent view record))
    (should-not (emacs-char-table-parent record))))

(ert-deftest emacs-char-table-test/parent-rejects-existing-cycle ()
  (let ((a (emacs-char-table-make 'test))
        (b (emacs-char-table-make 'test))
        (child (emacs-char-table-make 'test)))
    ;; Deliberately corrupted input: attaching CHILD must terminate without
    ;; changing its parent, even though the cycle never visits CHILD.
    (emacs-char-table--raw-set a emacs-char-table--i-parent b)
    (emacs-char-table--raw-set b emacs-char-table--i-parent a)
    (should-error (emacs-char-table-set-parent child a))
    (should-not (emacs-char-table-parent child))))

(ert-deftest emacs-char-table-test/subtype ()
  (should (eq 'my-subtype
             (emacs-char-table-subtype (emacs-char-table-make 'my-subtype)))))

(ert-deftest emacs-char-table-test/extra-slots ()
  ;; The slot count comes from the subtype's `char-table-extra-slots'
  ;; property (0 when absent), as in GNU.
  (unwind-protect
      (progn
        (put 'emacs-char-table-test-extra-subtype 'char-table-extra-slots 3)
        (let ((ct (emacs-char-table-make 'emacs-char-table-test-extra-subtype)))
          (should (null (emacs-char-table-extra-slot ct 0)))
          (emacs-char-table-set-extra-slot ct 0 'meta)
          (emacs-char-table-set-extra-slot ct 2 'meta2)
          (should (eq 'meta (emacs-char-table-extra-slot ct 0)))
          (should (eq 'meta2 (emacs-char-table-extra-slot ct 2)))
          (should (null (emacs-char-table-extra-slot ct 1)))
          (should-error (emacs-char-table-extra-slot ct 3))))
    (put 'emacs-char-table-test-extra-subtype 'char-table-extra-slots nil)))

;;;; --- category table integration ------------------------------------

(ert-deftest emacs-char-table-test/extra-slot-count-and-display-table-default ()
  "Display slot defaults bootstrap once and allocated table counts stay fixed."
  (let* ((subtype 'display-table)
         (original-plist (symbol-plist subtype)))
    (unwind-protect
        (progn
          (setplist subtype
                    (let ((plist original-plist) (clean nil))
                      (while plist
                        (unless (eq (car plist) 'char-table-extra-slots)
                          (setq clean (append clean (list (car plist) (cadr plist)))))
                        (setq plist (cddr plist)))
                      clean))
          (emacs-char-table--register-display-table-extra-slots)
          (should (= 6 (get subtype 'char-table-extra-slots)))
          (let ((six-slots (emacs-char-table-make subtype)))
            (put subtype 'char-table-extra-slots nil)
            (let ((nil-slots (emacs-char-table-make subtype)))
              (should (= 0 (emacs-char-table-extra-slot-count nil-slots)))
              (should-error (emacs-char-table-extra-slot nil-slots 0))
              (put subtype 'char-table-extra-slots 0)
              (let ((zero-slots (emacs-char-table-make subtype)))
                (should (= 0 (emacs-char-table-extra-slot-count zero-slots)))
                (put subtype 'char-table-extra-slots 6)
                (should (= 0 (emacs-char-table-extra-slot-count zero-slots)))
                (put subtype 'char-table-extra-slots nil)
                (should (= 6 (emacs-char-table-extra-slot-count six-slots)))
                (should (= 0 (emacs-char-table-extra-slot-count nil-slots))))
              (setplist subtype
                        (let ((plist (symbol-plist subtype)) (clean nil))
                          (while plist
                            (unless (eq (car plist) 'char-table-extra-slots)
                              (setq clean
                                    (append clean (list (car plist)
                                                        (cadr plist)))))
                            (setq plist (cddr plist)))
                          clean))
              (should (= 0 (emacs-char-table-extra-slot-count
                            (emacs-char-table-make subtype)))))
            (should (= 6 (emacs-char-table-extra-slot-count six-slots))))
          (let* ((generic 'emacs-char-table-test-generic)
                 (ten-slots (emacs-char-table-make generic)))
            (should (= 10 (emacs-char-table-extra-slot-count ten-slots)))
            (put generic 'char-table-extra-slots 2)
            (should (= 10 (emacs-char-table-extra-slot-count ten-slots)))) )
      (setplist subtype original-plist))))

(ert-deftest emacs-char-table-test/optimize-owned-default-preserves-table ()
  "The default optimizer is a logical no-op for Lisp-owned tables."
  (when (and (fboundp 'nelisp--chartab-standalone-p)
             (nelisp--chartab-standalone-p))
    (require 'emacs-cc-chartab-1)
    (let* ((parent (emacs-char-table-make 'test))
           (table (emacs-char-table-make 'test))
           (default 'fallback)
           (extra 'kept))
      (emacs-char-table-set parent ?B 'inherited)
      (emacs-char-table-set-range table nil default)
      (emacs-char-table-set table 0 'zero)
      (emacs-char-table-set table ?A 'alpha)
      (emacs-char-table-set-parent table parent)
      (emacs-char-table-set-extra-slot table 0 extra)
      (let ((before
             (list (emacs-char-table-subtype table)
                   (emacs-char-table--raw-ref
                    table emacs-char-table--i-default)
                   (copy-sequence (emacs-char-table-ascii-vector table))
                   (copy-tree (emacs-char-table--raw-ref
                               table emacs-char-table--i-ranges))
                   (copy-sequence (emacs-char-table--raw-ref
                                   table emacs-char-table--i-extra)))))
        (should-not (optimize-char-table table))
        (should-not (optimize-char-table table nil))
        (should (equal before
                       (list (emacs-char-table-subtype table)
                             (emacs-char-table--raw-ref
                              table emacs-char-table--i-default)
                             (emacs-char-table-ascii-vector table)
                             (emacs-char-table--raw-ref
                              table emacs-char-table--i-ranges)
                             (emacs-char-table--raw-ref
                              table emacs-char-table--i-extra))))
        (should (eq parent (emacs-char-table-parent table)))
        (should (eq extra (emacs-char-table-extra-slot table 0)))
        (should (eq 'zero (emacs-char-table-ref table 0)))
        (should (eq 'alpha (emacs-char-table-ref table ?A)))
        (should (eq 'fallback (emacs-char-table-ref table ?B)))))))

(ert-deftest emacs-char-table-test/optimize-owned-comparator-and-delegation ()
  "Owned tables use TEST; non-owned and legacy tables retain delegation."
  (when (and (fboundp 'nelisp--chartab-standalone-p)
             (nelisp--chartab-standalone-p))
    (require 'emacs-cc-chartab-1)
    (let* ((owned (emacs-char-table-make 'test))
           (empty-owned (emacs-char-table-make 'test))
           (native (funcall nelisp--chartab-native-make 'test nil))
           (ordinary [])
           (legacy7 (vector emacs-char-table--tag nil nil nil nil nil nil))
           (calls nil)
           (comparisons 0)
           (test (lambda (left right)
                   (setq comparisons (1+ comparisons))
                   (eq left right)))
           (sentinel (make-symbol "optimizer-delegated"))
           (emacs-cc-chartab-1--previous-optimize-char-table
            (lambda (table comparator)
              (setq calls (cons (list table comparator) calls))
              sentinel)))
      (emacs-char-table-set owned 0 'left)
      (emacs-char-table-set owned 1 'right)
      (should-not (optimize-char-table owned))
      (should-not calls)
      (should-not (optimize-char-table owned test))
      (should (> comparisons 0))
      (should-not calls)
      ;; GNU calls TEST only when traversal reaches a comparison.
      (should-not (optimize-char-table empty-owned 'not-a-function))
      (should-not calls)
      (setq calls nil)
      (should (eq sentinel (optimize-char-table native nil)))
      (should (equal calls (list (list native nil))))
      (setq calls nil)
      (should (eq sentinel (optimize-char-table native test)))
      (should (equal calls (list (list native test))))
      (setq calls nil)
      (should (eq sentinel (optimize-char-table ordinary nil)))
      (should (equal calls (list (list ordinary nil))))
      (setq calls nil)
      (should (eq sentinel (optimize-char-table ordinary test)))
      (should (equal calls (list (list ordinary test))))
      (setq calls nil)
      (should (eq sentinel (optimize-char-table legacy7 test)))
      (should (equal calls (list (list legacy7 test)))))))

(ert-deftest emacs-char-table-test/category-entry-mutation-and-copy ()
  "Category entries use the same sparse char-table representation.
Mutation changes only the selected category bit, and copying keeps the
source table independent."
  (let* ((table (emacs-char-table-make-category-table))
         (copy nil))
    (let ((emacs-char-table--current-category-table table))
      (should (emacs-char-table-category-table-p table))
      (should (= 128 (length (emacs-char-table-char-category-set ?!))))
      (should (eq (emacs-char-table-char-category-set ?a)
                  (emacs-char-table-char-category-set ?b)))
      (should-not (aref (emacs-char-table-char-category-set ?!) ?>))
      ;; GNU signals "Undefined category" for an undefined CATEGORY.
      (require 'emacs-cc-category-1)
      (aset (emacs-cc-category-1--docs-for table) (- ?> ?\s) "test category")
      (aset (emacs-cc-category-1--docs-for table) (- ?< ?\s) "test category")
      (emacs-char-table-modify-category-entry ?! ?> table)
      (should (aref (emacs-char-table-char-category-set ?!) ?>))
      (should-not (aref (emacs-char-table-char-category-set ?a) ?>))
      (should (equal (emacs-char-table-extra-slot table 0)
                     (emacs-cc-category-1--docs-for table)))
      (emacs-char-table-modify-category-entry '(?a . ?c) ?< table)
      (should (aref (emacs-char-table--category-set table ?b) ?<))
      (setq copy (emacs-char-table-copy-category-table table))
      (should-not (eq (emacs-char-table-extra-slot table 0)
                      (emacs-char-table-extra-slot copy 0)))
      (should (equal (emacs-char-table-extra-slot table 0)
                     (emacs-char-table-extra-slot copy 0)))
      (aset (emacs-char-table-extra-slot copy 0) (- ?> ?\s) "copy category")
      (should (equal (aref (emacs-char-table-extra-slot table 0) (- ?> ?\s))
                     "test category"))
      (emacs-char-table-modify-category-entry ?! ?> copy t)
      (should (aref (emacs-char-table-char-category-set ?!) ?>))
      (should-not (aref (emacs-char-table--category-set copy ?!) ?>))
      (should (aref (emacs-char-table--category-set copy ?b) ?<))
      ;; A direct accessor mutation in the copied table must not leak through
      ;; its independently copied default category set.
      (let ((emacs-char-table--current-category-table copy))
        (aset (emacs-char-table-char-category-set ?z) ?> t))
      (should-not (aref (emacs-char-table--category-set table ?z) ?>))
      (should (emacs-char-table-category-table-p copy)))))

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

(ert-deftest emacs-char-table-test/copy-after-interrupted-optimize-resyncs-ascii ()
  "Copy canonicalizes the ASCII view after a comparator interrupts optimize.
This follows GNU's public char-table history: the source keeps its old view
until a later ASCII write, while a copy is built from the current topology."
  (skip-unless (and (fboundp 'nelisp--chartab-standalone-p)
                    (nelisp--chartab-standalone-p)))
  (require 'emacs-cc-chartab-1)
  (let* ((table (emacs-char-table-make 'test))
         (ascii (emacs-char-table-ascii-vector table))
         copy copy-ascii)
    (emacs-char-table-set table 0 'first)
    (emacs-char-table-set table 1 'old-ascii)
    (emacs-char-table-set table 128 'boom)
    (should-error
     (optimize-char-table
      table (lambda (value _representative)
              (if (eq value 'boom)
                  (error "controlled comparator stop")
                t))))
    ;; The interrupted source retains GNU's detached cache history.
    (should (eq 'old-ascii (emacs-char-table-ref table 1)))
    (should-not (emacs-char-table-ref table 31))
    (setq copy (copy-sequence table)
          copy-ascii (emacs-char-table-ascii-vector copy))
    (should-not (eq table copy))
    (should-not (eq ascii copy-ascii))
    (should (eq 'first (emacs-char-table-ref copy 1)))
    (should (eq 'first (emacs-char-table-ref copy 31)))
    (should (eq 'first (aref copy-ascii 127)))
    (should (eq 'boom (emacs-char-table-ref copy 128)))
    (should (eq 'boom (aref copy-ascii 255)))
    (should (eq 'old-ascii (emacs-char-table-ref table 1)))
    (emacs-char-table-set copy 1 'copy-only)
    (should (eq 'old-ascii (emacs-char-table-ref table 1)))
    ;; A later unrelated ASCII write resynchronizes the captured source vector
    ;; in place, preserving the adapter's vector identity contract.
    (emacs-char-table-set-range table '(20 . 21) 'later)
    (should (eq ascii (emacs-char-table-ascii-vector table)))
    (should (eq 'first (aref ascii 31)))
    (should (eq 'first (emacs-char-table-ref table 31)))))

(ert-deftest emacs-char-table-test/range-boundaries-preserve-ascii-view ()
  "Range writes at ASCII and overlay boundaries keep public values coherent."
  (skip-unless (and (fboundp 'nelisp--chartab-standalone-p)
                    (nelisp--chartab-standalone-p)))
  (let* ((table (emacs-char-table-make 'test))
         (ascii (emacs-char-table-ascii-vector table)))
    (emacs-char-table-set-range table '(0 . 31) 'low)
    (should (eq ascii (emacs-char-table-ascii-vector table)))
    (should (eq 'low (aref ascii 0)))
    (should (eq 'low (aref ascii 31)))
    (should-not (emacs-char-table-ref table 32))
    (emacs-char-table-set-range table '(64 . 255) 'middle)
    (should (eq 'low (aref ascii 31)))
    (should (eq 'middle (aref ascii 64)))
    (should (eq 'middle (aref ascii 127)))
    (should (eq 'middle (aref ascii 128)))
    (should (eq 'middle (aref ascii 255)))
    (should-not (emacs-char-table-ref table 256))
    (emacs-char-table-set-range table '(128 . 300) 'high)
    (should (eq 'middle (aref ascii 127)))
    (should (eq 'high (aref ascii 128)))
    (should (eq 'high (aref ascii 255)))
    (should (eq 'high (emacs-char-table-ref table 256)))
    (should (eq 'high (emacs-char-table-ref table 300)))
    (should-not (emacs-char-table-ref table 301))
    (emacs-char-table-set-range table '(256 . 400) 'extra)
    (should (eq 'high (aref ascii 255)))
    (should (eq 'extra (emacs-char-table-ref table 256)))
    (should (eq 'extra (emacs-char-table-ref table 400)))
    (should-not (emacs-char-table-ref table 401))))

;;;; --- constants ------------------------------------------------------

(ert-deftest emacs-char-table-test/max-char ()
  (should (= #x3FFFFF (emacs-char-table-max-char)))
  ;; GNU: (max-char t) is the maximum Unicode code point.
  (should (= #x10FFFF (emacs-char-table-max-char t))))

;;;; --- public aref / aset bridge -------------------------------------

(ert-deftest emacs-char-table-test/public-array-access ()
  (let ((ct (emacs-char-table-make 'test 'fallback)))
    (dolist (entry '((0 . zero) (1 . one) (6 . six)
                     (97 . ascii) (#x3042 . unicode)))
      (should (eq (cdr entry)
                  (emacs-char-table-aset ct (car entry) (cdr entry))))
      (should (eq (cdr entry)
                  (emacs-char-table-aref ct (car entry)))))
    (should (eq 'fallback (emacs-char-table-aref ct ?z)))))

(ert-deftest emacs-char-table-test/public-array-access-parent ()
  (let ((parent (emacs-char-table-make 'test 'parent-default))
        (child (emacs-char-table-make 'test)))
    (emacs-char-table-aset parent ?a 'parent-ascii)
    (emacs-char-table-aset parent #x3042 'parent-unicode)
    (emacs-char-table-set-parent child parent)
    (should (eq 'parent-ascii (emacs-char-table-aref child ?a)))
    (should (eq 'parent-unicode (emacs-char-table-aref child #x3042)))
    (should (eq 'parent-default (emacs-char-table-aref child #x4000)))))

(ert-deftest emacs-char-table-test/public-array-access-default-precedes-parent ()
  (let ((parent (emacs-char-table-make 'test 'parent-default))
        (child (emacs-char-table-make 'test 'child-default)))
    (emacs-char-table-set-parent child parent)
    (dolist (index (list ?a #x3042 (emacs-char-table-max-char)))
      (should (eq 'child-default (emacs-char-table-aref child index)))
      (emacs-char-table-aset child index nil)
      (should (eq 'child-default (emacs-char-table-aref child index))))
    (emacs-char-table-set-range child nil 'changed)
    (dolist (index (list ?a #x3042 (emacs-char-table-max-char)))
      (should (eq 'changed (emacs-char-table-aref child index))))))

(ert-deftest emacs-char-table-test/public-array-access-preserves-arrays ()
  (let ((vector [a b c])
        (string "abc"))
    (should (eq 'b (emacs-char-table-aref vector 1)))
    (should (= ?b (emacs-char-table-aref string 1)))
    (should (eq 'changed (emacs-char-table-aset vector 1 'changed)))
    (should (equal [a changed c] vector))
    (should (= ?z (emacs-char-table-aset string 1 ?z)))
    (should (equal "azc" string))
    (should-error (emacs-char-table-aref vector 3)
                  :type 'args-out-of-range)))

(ert-deftest emacs-char-table-test/public-array-access-standard-syntax-table ()
  (let ((table (standard-syntax-table)))
    (should (equal (aref table ?a)
                   (emacs-char-table-aref table ?a)))))

(ert-deftest emacs-char-table-test/public-array-access-validates-character ()
  (let ((ct (emacs-char-table-make 'test)))
    (dolist (index (list -1 (1+ (emacs-char-table-max-char))))
      (should-error (emacs-char-table-aref ct index)
                    :type 'wrong-type-argument)
      (should-error (emacs-char-table-aset ct index 'bad)
                    :type 'wrong-type-argument))
    (should-error (emacs-char-table-aref ct nil)
                  :type 'wrong-type-argument)
    (should-error (emacs-char-table-aset ct nil 'bad)
                  :type 'wrong-type-argument)))

(ert-deftest emacs-char-table-test/host-primitives-remain-installed ()
  (skip-unless (emacs-char-table-test--host-p))
  (let ((host-aref (symbol-function 'aref))
        (host-aset (symbol-function 'aset)))
    (load "emacs-char-table" nil nil)
    (should (eq host-aref (symbol-function 'aref)))
    (should (eq host-aset (symbol-function 'aset)))))

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

(ert-deftest emacs-char-table-test/host-diff-default-parent-and-nil ()
  (skip-unless (emacs-char-table-test--host-p))
  (let ((host-parent (make-char-table 'test 'parent-default))
        (host-child (make-char-table 'test 'child-default))
        (mine-parent (emacs-char-table-make 'test 'parent-default))
        (mine-child (emacs-char-table-make 'test 'child-default)))
    (set-char-table-parent host-child host-parent)
    (emacs-char-table-set-parent mine-child mine-parent)
    (dolist (index (list ?a #x3042 (max-char)))
      (should (equal (aref host-child index)
                     (emacs-char-table-ref mine-child index)))
      (aset host-child index nil)
      (emacs-char-table-set mine-child index nil)
      (should (equal (aref host-child index)
                     (emacs-char-table-ref mine-child index))))
    (set-char-table-range host-child nil 'changed)
    (emacs-char-table-set-range mine-child nil 'changed)
    (dolist (index (list ?a #x3042 (max-char)))
      (should (equal (aref host-child index)
                     (emacs-char-table-ref mine-child index))))))

(provide 'emacs-char-table-test)

;;; emacs-char-table-test.el ends here

(ert-deftest emacs-char-table-test/native-extra-slot-write ()
  ;; Reading an extra slot of a native char-table already delegates to
  ;; the runtime; writing one must too.  The case-table code
  ;; (`set-case-table', `fundamental-mode') writes slot 2 of the
  ;; standard table, which is native in the standalone runtime.
  (put 'emacs-char-table-test--native 'char-table-extra-slots 3)
  (let ((ct (funcall (symbol-function 'make-char-table)
                     'emacs-char-table-test--native)))
    (skip-unless (emacs-char-table--native-p ct))
    (should (eq (emacs-char-table-set-extra-slot ct 2 'eqv) 'eqv))
    (should (eq (emacs-char-table-extra-slot ct 2) 'eqv))
    (should-error (emacs-char-table-set-extra-slot ct 3 'x)
                  :type 'args-out-of-range)))
