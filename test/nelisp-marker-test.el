;;; nelisp-marker-test.el --- Phase 5-B.2 ERT  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT suite for Phase 5-B.2 — marker / overlay / text-property
;; additions to `src/nelisp-buffer.el'.  The tests exercise:
;;
;;   - marker position tracking under insert and delete, with
;;     both insertion types
;;   - marker cross-buffer set + detach
;;   - overlay start / end / props storage
;;   - overlay endpoint behaviour under insert/delete, including
;;     the front-advance / rear-advance variants
;;   - text-property sparse-list put / get shadowing + shift on
;;     insert / delete + remove-text-properties
;;
;; Marker/overlay assertions use the NeLisp-local substrate.  The
;; text-property-linear cases use public APIs and run unchanged on GNU
;; Emacs and as runtime assertions on the C-core prelude.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-buffer)

;; These public-API assertions also run unchanged on GNU Emacs and, via
;; tools/nelisp-text-property-check.el, on the dynamically redefined prelude.
(ert-deftest nelisp-text-property-linear/string-directions ()
  (let ((s (make-string 12 ?x)))
    (put-text-property 2 5 'face 'a s)
    (put-text-property 5 8 'face 'a s)
    (put-text-property 5 8 'other t s)
    (put-text-property 10 12 'face 'b s)
    (should (= (next-single-property-change 3 'face s) 8))
    (should (= (next-property-change 3 s) 5))
    (should (= (previous-single-property-change 7 'face s) 2))
    (should (= (previous-property-change 7 s) 5))
    (should (= (next-property-change 8 s) 10))
    (should (= (previous-property-change 10 s) 8))
    (should (eq (get-text-property 7 'face s) 'a))
    (should (eq (plist-get (text-properties-at 7 s) 'other) t))))

(ert-deftest nelisp-text-property-linear/limits-and-edges ()
  (let ((s (make-string 8 ?x)))
    (put-text-property 2 6 'face 'a s)
    (dolist (single '(nil t))
      (let ((forward (if single 'next-single-property-change 'next-property-change))
            (backward (if single 'previous-single-property-change 'previous-property-change)))
        (dolist (case '((0 nil 2) (2 nil 6) (6 nil nil) (8 nil nil)
                       (3 5 5) (3 6 6) (3 8 6) (6 20 20) (3 1 1)))
          (should (equal (if single
                             (funcall forward (car case) 'face s (cadr case))
                           (funcall forward (car case) s (cadr case)))
                         (nth 2 case))))
        (dolist (case '((8 nil 6) (6 nil 2) (2 nil nil) (0 nil nil)
                       (5 3 3) (5 2 2) (5 0 2) (2 -5 -5) (3 7 7)))
          (should (equal (if single
                             (funcall backward (car case) 'face s (cadr case))
                           (funcall backward (car case) s (cadr case)))
                         (nth 2 case))))))))

(ert-deftest nelisp-text-property-linear/buffer-and-nil-object ()
  (with-temp-buffer
    (insert "xxxxxxxxxxxx")
    (put-text-property 3 6 'face 'a)
    (put-text-property 6 9 'face 'a)
    (put-text-property 6 9 'other t)
    (dolist (obj (list nil (current-buffer)))
      (should (= (next-single-property-change 4 'face obj) 9))
      (should (= (previous-single-property-change 8 'face obj) 3))
      (should (= (next-property-change 4 obj) 6))
      (should (= (previous-property-change 8 obj) 6))
      (should (= (next-property-change 4 obj 5) 5))
      (should (= (previous-property-change 8 obj 7) 7))
      (should-not (next-property-change (point-max) obj))
      (should-not (previous-property-change (point-min) obj)))))

(ert-deftest nelisp-text-property-linear/empty-and-identity ()
  (dolist (s (list "" (make-string 4 ?x)))
    (should-not (next-property-change 0 s))
    (should-not (previous-property-change (length s) s))
    (should (= (next-single-property-change 0 'face s 20) 20))
    (should (= (previous-single-property-change (length s) 'face s -2) -2)))
  (with-temp-buffer
    (should-not (next-property-change 1))
    (should-not (previous-single-property-change 1 'face)))
  (let* ((s (make-string 6 ?x)) (a (list 'v)) (b (list 'v)))
    (put-text-property 0 3 'face a s)
    (put-text-property 3 6 'face b s)
    (should (= (next-single-property-change 0 'face s) 3))
    (should (= (previous-property-change 6 s) 3))
    (put-text-property 3 6 'face a s)
    (should-not (next-single-property-change 0 'face s))
    (should-not (previous-property-change 6 s))))

(ert-deftest nelisp-text-property-linear/markers-and-narrowing ()
  (with-temp-buffer
    (insert "xxxxxxxxxx")
    (put-text-property 1 7 'face 'a)
    (let ((pos (copy-marker 4)) (limit (copy-marker 6)))
      (should (= (next-single-property-change pos 'face nil limit) 6))
      (should (= (previous-property-change limit nil pos) 4))
      (narrow-to-region 3 9)
      (should-not (previous-single-property-change 5 'face))
      (should (= (next-property-change 5) 7))
      (should-not (next-property-change 9))
      (should (= (previous-property-change 5 nil 1) 1)))))

(defmacro nelisp-marker-test--fresh (name &rest body)
  "Evaluate BODY with a fresh NeLisp buffer named NAME.
BODY may refer to the created buffer as `buf'."
  (declare (indent 1))
  `(progn
     (nelisp-buffer--reset-registry)
     (let ((buf (nelisp-generate-new-buffer ,name)))
       (nelisp-with-buffer buf
         ,@body))))

;;; Marker struct / basics --------------------------------------------

(ert-deftest nelisp-marker-make-detached ()
  (let ((m (nelisp-make-marker)))
    (should (nelisp-markerp m))
    (should (null (nelisp-marker-buffer m)))))

(ert-deftest nelisp-marker-copy-marker-creates-at-pos ()
  (nelisp-marker-test--fresh "cm"
    (nelisp-insert "abcde")
    (let ((m (nelisp-copy-marker buf 3)))
      (should (nelisp-markerp m))
      (should (eq buf (nelisp-marker-buffer m)))
      (should (= 3 (nelisp-marker-position m)))
      (should (memq m (nelisp-buffer--marker-list buf))))))

;;; Marker shift under insert ----------------------------------------

(ert-deftest nelisp-marker-insert-before-advances ()
  "Insertion strictly before a marker advances it by the length."
  (nelisp-marker-test--fresh "mib"
    (nelisp-insert "abcdef")
    (let ((m (nelisp-copy-marker buf 4)))
      (nelisp-goto-char 2)
      (nelisp-insert "XX")
      (should (= 6 (nelisp-marker-position m))))))

(ert-deftest nelisp-marker-insert-after-unaffected ()
  (nelisp-marker-test--fresh "mia"
    (nelisp-insert "abcdef")
    (let ((m (nelisp-copy-marker buf 2)))
      (nelisp-goto-char 5)
      (nelisp-insert "YY")
      (should (= 2 (nelisp-marker-position m))))))

(ert-deftest nelisp-marker-insert-at-stays-when-insertion-type-nil ()
  "Default insertion-type nil keeps the marker at its position
when text is inserted exactly there."
  (nelisp-marker-test--fresh "mns"
    (nelisp-insert "abcdef")
    (let ((m (nelisp-copy-marker buf 3 nil)))
      (nelisp-goto-char 3)
      (nelisp-insert "ZZ")
      (should (= 3 (nelisp-marker-position m))))))

(ert-deftest nelisp-marker-insert-at-advances-when-insertion-type-t ()
  (nelisp-marker-test--fresh "mta"
    (nelisp-insert "abcdef")
    (let ((m (nelisp-copy-marker buf 3 t)))
      (nelisp-goto-char 3)
      (nelisp-insert "ZZ")
      (should (= 5 (nelisp-marker-position m))))))

;;; Marker shift under delete ----------------------------------------

(ert-deftest nelisp-marker-delete-after-marker-unaffected ()
  (nelisp-marker-test--fresh "mdu"
    (nelisp-insert "abcdefghij")
    (let ((m (nelisp-copy-marker buf 3)))
      (nelisp-delete-region 6 9)
      (should (= 3 (nelisp-marker-position m))))))

(ert-deftest nelisp-marker-delete-before-marker-shifts ()
  (nelisp-marker-test--fresh "mds"
    (nelisp-insert "abcdefghij")
    (let ((m (nelisp-copy-marker buf 8)))
      (nelisp-delete-region 2 5)
      (should (= 5 (nelisp-marker-position m))))))

(ert-deftest nelisp-marker-delete-covering-marker-collapses ()
  "Marker inside the deleted range collapses to START."
  (nelisp-marker-test--fresh "mdc"
    (nelisp-insert "abcdefghij")
    (let ((m (nelisp-copy-marker buf 5)))
      (nelisp-delete-region 3 8)
      (should (= 3 (nelisp-marker-position m))))))

;;; Marker cross-buffer / detach -------------------------------------

(ert-deftest nelisp-marker-set-marker-moves-across-buffers ()
  (nelisp-buffer--reset-registry)
  (let ((a (nelisp-generate-new-buffer "A"))
        (b (nelisp-generate-new-buffer "B")))
    (nelisp-with-buffer a (nelisp-insert "aaaa"))
    (nelisp-with-buffer b (nelisp-insert "bbbb"))
    (let ((m (nelisp-copy-marker a 2)))
      (should (memq m (nelisp-buffer--marker-list a)))
      (nelisp-set-marker m 3 b)
      (should (eq b (nelisp-marker-buffer m)))
      (should (= 3 (nelisp-marker-position m)))
      (should-not (memq m (nelisp-buffer--marker-list a)))
      (should (memq m (nelisp-buffer--marker-list b))))))

(ert-deftest nelisp-marker-set-marker-nil-detaches ()
  (nelisp-marker-test--fresh "det"
    (nelisp-insert "xxx")
    (let ((m (nelisp-copy-marker buf 2)))
      (nelisp-set-marker m nil)
      (should (null (nelisp-marker-buffer m))))))

(ert-deftest nelisp-marker-delete-unlinks ()
  (nelisp-marker-test--fresh "mdx"
    (nelisp-insert "xxx")
    (let ((m (nelisp-copy-marker buf 2)))
      (nelisp-marker-delete m)
      (should (null (nelisp-marker-buffer m)))
      (should-not (memq m (nelisp-buffer--marker-list buf))))))

;;; Overlay basics ---------------------------------------------------

(ert-deftest nelisp-overlay-make-and-accessors ()
  (nelisp-marker-test--fresh "om"
    (nelisp-insert "abcdefghij")
    (let ((o (nelisp-make-overlay 3 7)))
      (should (nelisp-overlayp o))
      (should (= 3 (nelisp-overlay-start o)))
      (should (= 7 (nelisp-overlay-end o)))
      (should (memq o (nelisp-buffer-overlays buf))))))

(ert-deftest nelisp-overlay-put-get ()
  (nelisp-marker-test--fresh "opg"
    (nelisp-insert "abcdef")
    (let ((o (nelisp-make-overlay 2 5)))
      (nelisp-overlay-put o 'face 'warning)
      (should (eq 'warning (nelisp-overlay-get o 'face)))
      (nelisp-overlay-put o 'face 'success)
      (should (eq 'success (nelisp-overlay-get o 'face))))))

(ert-deftest nelisp-overlays-at-basic ()
  (nelisp-marker-test--fresh "oa"
    (nelisp-insert "abcdefghij")
    (let ((o1 (nelisp-make-overlay 3 7))
          (o2 (nelisp-make-overlay 5 9)))
      ;; At 6 — both cover ([3, 7) and [5, 9)).
      (should (equal (list o2 o1) (nelisp-overlays-at 6 buf)))
      ;; At 3 — only o1's range includes 3; o2 starts at 5.
      (should (equal (list o1) (nelisp-overlays-at 3 buf)))
      ;; At 7 — o1 excludes (end-exclusive), o2 still covers.
      (should (equal (list o2) (nelisp-overlays-at 7 buf)))
      ;; At 9 — neither covers (o2 ends at 9 exclusive).
      (should (null (nelisp-overlays-at 9 buf))))))

(ert-deftest nelisp-overlays-in-range ()
  "`overlays-in' uses the same open/closed convention as Emacs:
overlap is (s < end) AND (e > start).  o3 [8,10) overlaps [3,9)
because 8 < 9.  o4 [9,10) does NOT overlap because 9 is not < 9."
  (nelisp-marker-test--fresh "oi"
    (nelisp-insert "0123456789")
    (let ((o1 (nelisp-make-overlay 2 4))
          (o2 (nelisp-make-overlay 5 7))
          (o3 (nelisp-make-overlay 9 11)))
      (let ((all (nelisp-overlays-in 3 9 buf)))
        (should (memq o1 all))
        (should (memq o2 all))
        (should-not (memq o3 all))))))

(ert-deftest nelisp-overlay-delete-unlinks ()
  (nelisp-marker-test--fresh "od"
    (nelisp-insert "abc")
    (let ((o (nelisp-make-overlay 1 3)))
      (nelisp-delete-overlay o)
      (should (null (nelisp-overlay-buffer o)))
      (should-not (memq o (nelisp-buffer-overlays buf))))))

;;; Overlay shift under insert / delete -------------------------------

(ert-deftest nelisp-overlay-insert-inside-expands ()
  "Insert strictly inside an overlay expands the end endpoint."
  (nelisp-marker-test--fresh "oii"
    (nelisp-insert "abcdefghij")
    (let ((o (nelisp-make-overlay 3 7)))
      (nelisp-goto-char 5)
      (nelisp-insert "XX")
      (should (= 3 (nelisp-overlay-start o)))
      (should (= 9 (nelisp-overlay-end o))))))

(ert-deftest nelisp-overlay-insert-at-start-front-advance-nil ()
  "With FRONT-ADVANCE nil, insertion at the start leaves start put."
  (nelisp-marker-test--fresh "ois"
    (nelisp-insert "abcdefghij")
    (let ((o (nelisp-make-overlay 5 8)))
      (nelisp-goto-char 5)
      (nelisp-insert "XX")
      (should (= 5 (nelisp-overlay-start o)))
      (should (= 10 (nelisp-overlay-end o))))))

(ert-deftest nelisp-overlay-insert-at-start-front-advance-t ()
  "With FRONT-ADVANCE t, insertion at the start advances both ends."
  (nelisp-marker-test--fresh "oisf"
    (nelisp-insert "abcdefghij")
    (let ((o (nelisp-make-overlay 5 8 buf t nil)))
      (nelisp-goto-char 5)
      (nelisp-insert "YY")
      (should (= 7 (nelisp-overlay-start o)))
      (should (= 10 (nelisp-overlay-end o))))))

(ert-deftest nelisp-overlay-delete-covering-collapses ()
  (nelisp-marker-test--fresh "odc"
    (nelisp-insert "abcdefghij")
    (let ((o (nelisp-make-overlay 4 7)))
      (nelisp-delete-region 3 8)
      (should (= 3 (nelisp-overlay-start o)))
      (should (= 3 (nelisp-overlay-end o))))))

(ert-deftest nelisp-overlay-delete-before-shifts ()
  (nelisp-marker-test--fresh "odb"
    (nelisp-insert "abcdefghij")
    (let ((o (nelisp-make-overlay 6 9)))
      (nelisp-delete-region 2 5)
      (should (= 3 (nelisp-overlay-start o)))
      (should (= 6 (nelisp-overlay-end o))))))

;;; Text-property sparse list ----------------------------------------

(ert-deftest nelisp-text-property-put-get ()
  (nelisp-marker-test--fresh "tpg"
    (nelisp-insert "abcdef")
    (nelisp-put-text-property 2 5 'face 'warning)
    (should (eq 'warning (nelisp-get-text-property 2 'face)))
    (should (eq 'warning (nelisp-get-text-property 3 'face)))
    (should (eq 'warning (nelisp-get-text-property 4 'face)))
    (should (null (nelisp-get-text-property 5 'face)))
    (should (null (nelisp-get-text-property 1 'face)))))

(ert-deftest nelisp-text-property-newest-wins ()
  (nelisp-marker-test--fresh "tpnew"
    (nelisp-insert "abcdef")
    (nelisp-put-text-property 2 5 'face 'old)
    (nelisp-put-text-property 3 4 'face 'new)
    (should (eq 'new (nelisp-get-text-property 3 'face)))
    (should (eq 'old (nelisp-get-text-property 2 'face)))
    (should (eq 'old (nelisp-get-text-property 4 'face)))))

(ert-deftest nelisp-text-property-set-to-nil-distinguishable ()
  "Setting PROP to nil via a newer interval must shadow older values
(we use `plist-member' to detect the key independently of value)."
  (nelisp-marker-test--fresh "tpnil"
    (nelisp-insert "abcdef")
    (nelisp-put-text-property 2 5 'face 'old)
    (nelisp-put-text-property 2 5 'face nil)
    (should (null (nelisp-get-text-property 3 'face)))))

(ert-deftest nelisp-text-property-shift-on-insert ()
  (nelisp-marker-test--fresh "tpins"
    (nelisp-insert "abcdefghij")
    (nelisp-put-text-property 4 8 'face 'hi)
    (nelisp-goto-char 2)
    (nelisp-insert "XX")
    ;; Property should still cover the original characters, now at
    ;; positions 6..9 in the 1-based scheme.
    (should (eq 'hi (nelisp-get-text-property 6 'face)))
    (should (null (nelisp-get-text-property 5 'face)))))

(ert-deftest nelisp-text-property-shift-on-delete ()
  (nelisp-marker-test--fresh "tpdel"
    (nelisp-insert "abcdefghij")
    (nelisp-put-text-property 5 9 'face 'hi)
    (nelisp-delete-region 2 4)
    (should (eq 'hi (nelisp-get-text-property 3 'face)))
    (should (null (nelisp-get-text-property 2 'face)))))

(ert-deftest nelisp-remove-text-properties-drops-key ()
  (nelisp-marker-test--fresh "tprm"
    (nelisp-insert "abcdef")
    (nelisp-put-text-property 2 5 'face 'hi)
    (nelisp-put-text-property 2 5 'bold t)
    (nelisp-remove-text-properties 2 5 '(face) buf)
    (should (null (nelisp-get-text-property 3 'face)))
    (should (eq t (nelisp-get-text-property 3 'bold)))))

(ert-deftest nelisp-erase-clears-text-properties ()
  (nelisp-marker-test--fresh "tpe"
    (nelisp-insert "abcdef")
    (nelisp-put-text-property 2 5 'face 'hi)
    (nelisp-erase-buffer)
    (should (null (nelisp-text-property-intervals buf)))))

(provide 'nelisp-marker-test)

;;; nelisp-marker-test.el ends here
