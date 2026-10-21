;;; syntax-rt-chartab.el --- Doc 225 S3.1 fixture: GNU 31.1 char-table family -*- lexical-binding: t; -*-

;; One line per case, NAME => VALUE, compared byte for byte with
;; syntax-rt-chartab.gnu-31.1.expected.  Printed forms pin GNU chartab.c's
;; sub-char-table structure, which depends on the history of operations,
;; not only on the mapping.  Tables here are small and private; the
;; standard tables are never printed.
;; Run: emacs -Q --batch -l FILE, or BIN --load FILE.

(defun srtc-show (name thunk)
  (princ (format "%s => %S\n" name
                 (condition-case err (funcall thunk)
                   (error (list 'ERR (car err) (cdr err)))))))

(defmacro srtc (name &rest body)
  `(srtc-show ,name (lambda () ,@body)))

(put 'srtc-zero 'char-table-extra-slots 0)
(put 'srtc-three 'char-table-extra-slots 3)
(put 'srtc-ten 'char-table-extra-slots 10)

(defun srtc-bits (bool-vector)
  "Indices of BOOL-VECTOR's set bits (its printed form is not compared)."
  (let ((i 0) out)
    (while (< i (length bool-vector))
      (when (aref bool-vector i) (push i out))
      (setq i (1+ i)))
    (nreverse out)))

(defun srtc-map (table)
  "Collect map-char-table's calls, copying each range cons."
  (let (out)
    (map-char-table (lambda (key value)
                      (push (cons (if (consp key) (cons (car key) (cdr key)) key)
                                  value)
                            out))
                    table)
    (nreverse out)))

;;; Construction, type and printing after specific histories

(srtc "make/fresh" (make-char-table 'srtc-zero))
(srtc "make/init" (make-char-table 'srtc-three 7))
(srtc "make/subtype-extras"
      (list (char-table-subtype (make-char-table 'srtc-three))
            (char-table-extra-slot (make-char-table 'srtc-three 'x) 2)
            (char-table-extra-slot (make-char-table 'srtc-ten) 9)))
(srtc "make/no-property" (make-char-table 'srtc-unknown-subtype 1))
(srtc "make/err-subtype" (make-char-table "foo"))
(srtc "make/err-arity" (make-char-table))
(srtc "type"
      (let ((table (make-char-table 'srtc-zero)))
        (list (type-of table) (char-table-p table) (arrayp table)
              (sequencep table) (vectorp table) (recordp table)
              (char-table-p [1 2]) (length table))))
(srtc "print/ascii-aset"
      (let ((table (make-char-table 'srtc-zero))) (aset table ?a 1) table))
(srtc "print/cjk-aset"
      (let ((table (make-char-table 'srtc-zero))) (aset table #x4e00 'x) table))
(srtc "print/aset-nil"
      (let ((table (make-char-table 'srtc-zero)))
        (aset table #x4e00 'x) (aset table #x4e00 nil) table))
(srtc "print/range-whole-block"
      (let ((table (make-char-table 'srtc-zero)))
        (set-char-table-range table '(#x10000 . #x1ffff) 'b) table))
(srtc "print/range-partial"
      (let ((table (make-char-table 'srtc-zero)))
        (set-char-table-range table '(#x41 . #x5a) 'u) table))
(srtc "print/range-across-blocks"
      (let ((table (make-char-table 'srtc-zero)))
        (set-char-table-range table '(#xff00 . #x10100) 'w) table))
(srtc "print/range-t"
      (let ((table (make-char-table 'srtc-zero)))
        (aset table ?a 1) (set-char-table-range table t 'all) table))
(srtc "print/range-nil"
      (let ((table (make-char-table 'srtc-zero)))
        (set-char-table-range table nil 'dflt) table))
(srtc "print/extras-and-parent"
      (let ((table (make-char-table 'srtc-three))
            (parent (make-char-table 'srtc-zero)))
        (set-char-table-extra-slot table 0 "doc")
        (set-char-table-extra-slot table 2 '(a b))
        (set-char-table-parent table parent)
        table))
(srtc "print/optimize"
      (let ((table (make-char-table 'srtc-zero)))
        (aset table ?a 1) (aset table ?a nil) (aset table #x4e00 'x)
        (aset table #x4e00 nil)
        (list (optimize-char-table table) table)))
(srtc "print/optimize-partial"
      (let ((table (make-char-table 'srtc-zero)))
        (set-char-table-range table '(0 . 127) 'k) (aset table ?b 'j)
        (optimize-char-table table)
        table))
(srtc "optimize/test"
      (let ((a (make-char-table 'srtc-zero)) (b (make-char-table 'srtc-zero)))
        (set-char-table-range a '(#x80 . #xff) (list 1))
        (set-char-table-range b '(#x80 . #xff) (list 1))
        (aset a #x90 (list 1)) (aset b #x90 (list 1))
        (optimize-char-table a)
        (optimize-char-table b 'eq)
        (list (equal a b) (aref a #x90) (aref b #x90))))
(srtc "optimize/err" (optimize-char-table [1]))

;;; aref and aset

(srtc "aref/values"
      (let ((table (make-char-table 'srtc-zero)))
        (aset table 0 'zero) (aset table 127 'del) (aset table 128 'eight)
        (aset table #x3fffff 'max) (aset table #x10ffff 'uni)
        (mapcar (lambda (c) (aref table c)) '(0 1 127 128 #x10ffff #x3fffff #x3ffffe))))
(srtc "aset/value" (aset (make-char-table 'srtc-zero) ?z 'v))
(srtc "aref/err-symbol" (aref (make-char-table 'srtc-zero) 'a))
(srtc "aref/err-negative" (aref (make-char-table 'srtc-zero) -1))
(srtc "aref/err-too-big" (aref (make-char-table 'srtc-zero) #x400000))
(srtc "aset/err-symbol" (aset (make-char-table 'srtc-zero) 'a 1))
(srtc "aset/err-too-big" (aset (make-char-table 'srtc-zero) #x400000 1))

;;; char-table-range and set-char-table-range

(srtc "range/get"
      (let ((table (make-char-table 'srtc-zero 'init)))
        (set-char-table-range table nil 'dflt)
        (aset table ?a nil)
        (list (char-table-range table nil) (char-table-range table ?a)
              (char-table-range table ?b) (char-table-range table '(?a . ?z))
              (char-table-range table '(?b . ?z)))))
(srtc "range/cons-no-parent"
      (let ((table (make-char-table 'srtc-zero))
            (parent (make-char-table 'srtc-zero 'from-parent)))
        (set-char-table-parent table parent)
        (list (char-table-range table ?a) (char-table-range table '(?a . ?b))
              (aref table ?a))))
(srtc "range/set-values"
      (let ((table (make-char-table 'srtc-zero)))
        (list (set-char-table-range table '(?a . ?c) 'abc)
              (set-char-table-range table ?q 'q)
              (set-char-table-range table nil 'd)
              (mapcar (lambda (c) (aref table c)) '(?` ?a ?c ?d ?q)))))
(srtc "range/err-get" (char-table-range (make-char-table 'srtc-zero) "a"))
(srtc "range/err-set" (set-char-table-range (make-char-table 'srtc-zero) "a" 1))
(srtc "range/err-cons" (set-char-table-range (make-char-table 'srtc-zero) '(a . 1) 1))
(srtc "range/err-table" (char-table-range [1 2] nil))

;;; Parent, default and extra slots

(srtc "lookup/order"
      (let* ((grand (make-char-table 'srtc-zero))
             (parent (make-char-table 'srtc-zero))
             (table (make-char-table 'srtc-zero)))
        (set-char-table-parent parent grand)
        (set-char-table-parent table parent)
        (aset grand ?a 'grand) (aset grand ?b 'grand) (aset grand ?c 'grand)
        (aset parent ?b 'parent)
        (set-char-table-range parent nil 'parent-default)
        (aset table ?c 'own)
        (list (aref table ?a) (aref table ?b) (aref table ?c) (aref table ?d)
              (progn (set-char-table-range table nil 'own-default)
                     (list (aref table ?a) (aref table ?c))))))
(srtc "parent/set"
      (let ((table (make-char-table 'srtc-zero)) (parent (make-char-table 'srtc-zero)))
        (list (set-char-table-parent table parent)
              (eq (char-table-parent table) parent)
              (set-char-table-parent table nil)
              (char-table-parent table))))
(srtc "parent/err-cycle"
      (let ((a (make-char-table 'srtc-zero)) (b (make-char-table 'srtc-zero)))
        (set-char-table-parent a b)
        (set-char-table-parent b a)))
(srtc "parent/err-type" (set-char-table-parent (make-char-table 'srtc-zero) 1))
(srtc "extra/set-get"
      (let ((table (make-char-table 'srtc-three)))
        (list (set-char-table-extra-slot table 1 'one)
              (char-table-extra-slot table 1) (char-table-extra-slot table 0))))
(srtc "extra/err-high" (char-table-extra-slot (make-char-table 'srtc-three) 3))
(srtc "extra/err-negative" (char-table-extra-slot (make-char-table 'srtc-three) -1))
(srtc "extra/err-type" (char-table-extra-slot (make-char-table 'srtc-three) 'a))
(srtc "extra/err-set-high" (set-char-table-extra-slot (make-char-table 'srtc-zero) 0 1))

;;; map-char-table

(srtc "map/coalesce"
      (let ((table (make-char-table 'srtc-zero)))
        (set-char-table-range table '(?a . ?z) 'lower)
        (aset table ?m 'em)
        (set-char-table-range table '(#x4e00 . #x9fff) 'cjk)
        (aset table #x3fffff 'last)
        (srtc-map table)))
(srtc "map/default-and-parent"
      (let ((table (make-char-table 'srtc-zero))
            (parent (make-char-table 'srtc-zero)))
        (set-char-table-range parent '(?0 . ?9) 'digit)
        (set-char-table-range parent '(?a . ?c) 'pa)
        (set-char-table-parent table parent)
        (aset table ?b 'own)
        (set-char-table-range table nil nil)
        (srtc-map table)))
(srtc "map/default"
      (let ((table (make-char-table 'srtc-zero)))
        (set-char-table-range table nil 'dflt)
        (aset table ?x 'x)
        (srtc-map table)))
(srtc "map/single"
      (let ((table (make-char-table 'srtc-zero)))
        (aset table ?a 1)
        (let (keys)
          (map-char-table (lambda (key _value) (push key keys)) table)
          keys)))
(srtc "map/err" (map-char-table #'ignore [1]))

;;; copy-sequence and equal

(srtc "copy/independent"
      (let* ((value (list 'shared))
             (table (make-char-table 'srtc-three)))
        (aset table ?a value) (aset table #x4e00 'x)
        (set-char-table-extra-slot table 0 'e0)
        (let ((copy (copy-sequence table)))
          (aset copy ?a 'changed) (aset copy #x4e00 'y)
          (set-char-table-extra-slot copy 0 'e1)
          (list (aref table ?a) (aref table #x4e00) (char-table-extra-slot table 0)
                (eq (aref (copy-sequence table) ?a) value)
                (equal (copy-sequence table) table)
                (eq (copy-sequence table) table)))))
(srtc "equal/structure"
      (let ((a (make-char-table 'srtc-zero)) (b (make-char-table 'srtc-zero))
            (c (make-char-table 'srtc-zero)) (d (make-char-table 'srtc-one)))
        (aset a ?a 1) (aset b ?a 1)
        (aset c ?a 1) (aset c ?b 2) (aset c ?b nil)
        (list (equal a b) (equal a c) (equal a d)
              (equal (make-char-table 'srtc-zero) (make-char-table 'srtc-zero))
              (equal (make-char-table 'srtc-zero 1) (make-char-table 'srtc-zero 2)))))
(srtc "equal/history"
      (let ((a (make-char-table 'srtc-zero)) (b (make-char-table 'srtc-zero)))
        (aset a #x4e00 'x) (aset a #x4e00 nil)
        (list (equal a b) (progn (optimize-char-table a) (equal a b)))))

;;; max-char

(srtc "max-char" (list (max-char) (max-char t) (max-char nil)))

;;; Category tables (category.c)

(srtc "category/standard"
      (with-temp-buffer
        (set-category-table (standard-category-table))
        (list (category-table-p (standard-category-table))
              (char-table-subtype (standard-category-table))
              (category-set-mnemonics (char-category-set ?a))
              (category-set-mnemonics (char-category-set #x4e00))
              (category-set-mnemonics (char-category-set #x3042))
              (category-set-mnemonics (char-category-set #x30a2))
              (category-set-mnemonics (char-category-set #xe9))
              (category-set-mnemonics (char-category-set #x3b1))
              (category-set-mnemonics (char-category-set #x5d0))
              (category-docstring ?a) (category-docstring ?C))))
(srtc "category/set-object"
      (with-temp-buffer
        (set-category-table (standard-category-table))
        (let ((set (char-category-set ?a)))
          (list (bool-vector-p set) (length set) (aref set ?l) (aref set ?C)
                (srtc-bits set)))))
(srtc "category/make-set" (srtc-bits (make-category-set "aCl")))
(srtc "category/define-modify"
      (let ((table (make-category-table)))
        (list (category-table-p table)
              (char-table-subtype table)
              (define-category ?x "Test category" table)
              (category-docstring ?x table)
              (get-unused-category table)
              (modify-category-entry ?q ?x table)
              (modify-category-entry '(?0 . ?2) ?x table)
              (with-temp-buffer
                (set-category-table table)
                (list (category-set-mnemonics (char-category-set ?q))
                      (category-set-mnemonics (char-category-set ?1))
                      (category-set-mnemonics (char-category-set ?3))
                      (progn (modify-category-entry ?q ?x table t)
                             (category-set-mnemonics (char-category-set ?q)))))
              (eq (category-table) (standard-category-table)))))
(srtc "category/copy"
      (let* ((table (make-category-table))
             (copy (progn (define-category ?y "Why" table)
                          (modify-category-entry ?w ?y table)
                          (copy-category-table table))))
        (modify-category-entry ?w ?y copy t)
        (list (category-table-p copy)
              (category-docstring ?y copy)
              (with-temp-buffer (set-category-table table)
                                (category-set-mnemonics (char-category-set ?w)))
              (with-temp-buffer (set-category-table copy)
                                (category-set-mnemonics (char-category-set ?w))))))
(srtc "category/err-undefined"
      (modify-category-entry ?a ?Z (make-category-table)))
(srtc "category/err-define-twice"
      (let ((table (make-category-table)))
        (define-category ?v "Vee" table)
        (define-category ?v "Vee again" table)))
(srtc "category/err-table" (set-category-table 1))
(srtc "category/err-mnemonic" (define-category 200 "bad" (make-category-table)))

;; End here: the standalone `--load' would otherwise print the last value.
(kill-emacs 0)

;;; syntax-rt-chartab.el ends here
