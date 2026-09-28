;;; nelisp-buffer-test.el --- Phase 5-B.1 ERT  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT suite for `src/nelisp-buffer.el'.  Pattern is unit tests
;; for each primitive + host-parity tests per Doc 13 §2.8 LOCK B:
;; a handful of scripted mutation sequences run against both a
;; freshly spawned host buffer and a NeLisp buffer, and the text
;; / point / point-min / point-max must match at every step.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-buffer)

(defmacro nelisp-buffer-test--fresh (name &rest body)
  "Evaluate BODY with a freshly created NeLisp buffer named NAME.
The registry is reset before each test so test order is
irrelevant.  BODY can refer to the created buffer as `buf'."
  (declare (indent 1))
  `(progn
     (nelisp-buffer--reset-registry)
     (let ((buf (nelisp-generate-new-buffer ,name)))
       (nelisp-with-buffer buf
         ,@body))))

(defmacro nelisp-buffer-test--with-pair (name &rest body)
  "Set up (host . nelisp) buffer pair under NAME.
BODY can reference the host buffer as `host' and the NeLisp one
as `ne'.  Both are killed on exit."
  (declare (indent 1))
  (let ((host (make-symbol "host"))
        (ne (make-symbol "ne")))
    `(let* ((,host (generate-new-buffer
                    (concat " *nelisp-buffer-test-" ,name "*")))
            (,ne (progn (nelisp-buffer--reset-registry)
                        (nelisp-generate-new-buffer ,name))))
       (unwind-protect
           (let ((host ,host) (ne ,ne))
             ,@body)
         (when (buffer-live-p ,host) (kill-buffer ,host))
         (nelisp-buffer--reset-registry)))))

;;; Registry ----------------------------------------------------------

(ert-deftest nelisp-buffer-generate-new-uniquifies ()
  (nelisp-buffer--reset-registry)
  (let ((a (nelisp-generate-new-buffer "X"))
        (b (nelisp-generate-new-buffer "X"))
        (c (nelisp-generate-new-buffer "X")))
    (should (equal (nelisp-buffer-name a) "X"))
    (should (equal (nelisp-buffer-name b) "X<1>"))
    (should (equal (nelisp-buffer-name c) "X<2>"))))

(ert-deftest nelisp-buffer-get-create-idempotent ()
  (nelisp-buffer--reset-registry)
  (let ((a (nelisp-get-buffer-create "Y"))
        (b (nelisp-get-buffer-create "Y")))
    (should (eq a b))))

(ert-deftest nelisp-buffer-get-returns-nil-when-absent ()
  (nelisp-buffer--reset-registry)
  (should (null (nelisp-get-buffer "nope"))))

(ert-deftest nelisp-buffer-kill-removes-from-registry ()
  (nelisp-buffer--reset-registry)
  (let ((a (nelisp-generate-new-buffer "Z")))
    (should (eq a (nelisp-get-buffer "Z")))
    (should (nelisp-kill-buffer a))
    (should (null (nelisp-get-buffer "Z")))))

(ert-deftest nelisp-buffer-list-returns-live ()
  (nelisp-buffer--reset-registry)
  (nelisp-generate-new-buffer "A")
  (nelisp-generate-new-buffer "B")
  (should (= 2 (length (nelisp-buffer-list)))))

;;; Insert / size / point --------------------------------------------

(ert-deftest nelisp-buffer-fresh-is-empty ()
  (nelisp-buffer-test--fresh "empty"
    (should (= 0 (nelisp-buffer-size buf)))
    (should (= 1 (nelisp-point buf)))
    (should (= 1 (nelisp-point-min buf)))
    (should (= 1 (nelisp-point-max buf)))
    (should (equal "" (nelisp-buffer-string buf)))))

(ert-deftest nelisp-buffer-insert-basic ()
  (nelisp-buffer-test--fresh "ins"
    (nelisp-insert "hello")
    (should (equal "hello" (nelisp-buffer-string buf)))
    (should (= 6 (nelisp-point buf)))
    (should (= 5 (nelisp-buffer-size buf)))
    (should (nelisp-buffer-modified-p buf))))

(ert-deftest nelisp-buffer-insert-multi ()
  (nelisp-buffer-test--fresh "ins2"
    (nelisp-insert "hello")
    (nelisp-insert " ")
    (nelisp-insert "world")
    (should (equal "hello world" (nelisp-buffer-string buf)))
    (should (= 12 (nelisp-point buf)))))

(ert-deftest nelisp-buffer-insert-at-middle ()
  (nelisp-buffer-test--fresh "mid"
    (nelisp-insert "hello world")
    (nelisp-goto-char 7)
    (nelisp-insert "there ")
    (should (equal "hello there world" (nelisp-buffer-string buf)))
    (should (= 13 (nelisp-point buf)))))

(ert-deftest nelisp-buffer-parity-insert-while-narrowed ()
  "Insertion at an interior point extends the active restriction's end."
  (nelisp-buffer-test--with-pair "insert-narrowed"
    (with-current-buffer host
      (insert "abcd")
      (narrow-to-region 2 4)
      (goto-char 3)
      (insert "X"))
    (nelisp-insert "abcd" ne)
    (nelisp-narrow-to-region 2 4 ne)
    (nelisp-goto-char 3 ne)
    (nelisp-insert "X" ne)
    (should (= (with-current-buffer host (point-max)) (nelisp-point-max ne)))
    (should (= (with-current-buffer host (point-min)) (nelisp-point-min ne)))
    (should (= (with-current-buffer host (point)) (nelisp-point ne)))
    (should (equal (with-current-buffer host
                     (buffer-substring (point-min) (point-max)))
                   (nelisp-buffer-substring
                    (nelisp-point-min ne) (nelisp-point-max ne) ne)))))

(ert-deftest nelisp-buffer-parity-insert-metadata ()
  "Insertion keeps marker, overlay, and text-property positions in sync."
  (nelisp-buffer-test--with-pair "insert-metadata"
    (let ((host-marker (make-marker))
          (host-advancing-marker (make-marker))
          host-overlay
          (ne-marker (nelisp-marker--make :buffer ne :position 2))
          (ne-advancing-marker
           (nelisp-marker--make :buffer ne :position 2 :insertion-type t))
          (ne-overlay
           (nelisp-overlay--make :buffer ne :start 2 :end 4)))
      (with-current-buffer host
        (insert "abcd")
        (goto-char 2)
        (set-marker host-marker 2)
        (set-marker host-advancing-marker 2)
        (set-marker-insertion-type host-advancing-marker t)
        (setq host-overlay (make-overlay 2 4))
        (put-text-property 1 3 'face 'bold)
        (insert "XY"))
      (nelisp-insert "abcd" ne)
      (nelisp-goto-char 2 ne)
      (setf (nelisp-buffer-markers ne)
            (list ne-marker ne-advancing-marker)
            (nelisp-buffer-overlays ne) (list ne-overlay))
      (nelisp-put-text-property 1 3 'face 'bold ne)
      (nelisp-insert "XY" ne)
      (should (equal (nelisp-buffer-string ne)
                     (with-current-buffer host (buffer-string))))
      (should (= (nelisp-marker-position ne-marker)
                 (marker-position host-marker)))
      (should (= (nelisp-marker-position ne-advancing-marker)
                 (marker-position host-advancing-marker)))
      (should (= (nelisp-overlay-start ne-overlay)
                 (overlay-start host-overlay)))
      (should (= (nelisp-overlay-end ne-overlay)
                 (overlay-end host-overlay)))
      (should (eq (nelisp-get-text-property 1 'face ne) 'bold))
      (should (eq (with-current-buffer host (get-text-property 1 'face))
                  'bold))
      (delete-overlay host-overlay)
      (set-marker host-marker nil)
      (set-marker host-advancing-marker nil))))

(ert-deftest nelisp-buffer-insert-skips-empty-metadata-helpers ()
  "Insertion with no metadata does not invoke positional shift helpers."
  (nelisp-buffer-test--fresh "insert-empty-metadata"
    (cl-letf (((symbol-function 'nelisp-buffer--shift-markers-on-insert)
               (lambda (&rest _) (error "unexpected marker helper call")))
              ((symbol-function 'nelisp-buffer--shift-overlays-on-insert)
               (lambda (&rest _) (error "unexpected overlay helper call")))
              ((symbol-function 'nelisp-buffer--shift-text-properties-on-insert)
               (lambda (&rest _) (error "unexpected text-property helper call"))))
      (nelisp-insert "x"))
    (should (equal "x" (nelisp-buffer-string buf)))))

(ert-deftest nelisp-buffer-insert-type-errors ()
  (nelisp-buffer-test--fresh "type"
    (should-error (nelisp-insert 42) :type 'wrong-type-argument)
    (should-error (nelisp-insert '(a)) :type 'wrong-type-argument)))

;;; goto-char ---------------------------------------------------------

(ert-deftest nelisp-buffer-goto-char-clamps ()
  (nelisp-buffer-test--fresh "clamp"
    (nelisp-insert "abcd")
    (nelisp-goto-char 0)
    (should (= 1 (nelisp-point buf)))
    (nelisp-goto-char 100)
    (should (= 5 (nelisp-point buf)))))

(ert-deftest nelisp-buffer-goto-char-respects-narrow ()
  (nelisp-buffer-test--fresh "nclamp"
    (nelisp-insert "abcdefghij")
    (nelisp-narrow-to-region 3 7)
    (nelisp-goto-char 1)
    (should (= 3 (nelisp-point buf)))
    (nelisp-goto-char 20)
    (should (= 7 (nelisp-point buf)))))

;;; delete-region -----------------------------------------------------

(ert-deftest nelisp-buffer-delete-region-basic ()
  (nelisp-buffer-test--fresh "del"
    (nelisp-insert "hello world")
    (nelisp-delete-region 6 12)
    (should (equal "hello" (nelisp-buffer-string buf)))))

(ert-deftest nelisp-buffer-delete-region-across-gap ()
  "Delete a range that spans the current gap (which sits at point
after goto-char)."
  (nelisp-buffer-test--fresh "across"
    (nelisp-insert "abcdefghij")
    (nelisp-goto-char 5)
    (nelisp-delete-region 3 8)
    (should (equal "abhij" (nelisp-buffer-string buf)))))

(ert-deftest nelisp-buffer-delete-region-inverted ()
  "Swapped START / END works the same as the Emacs convention:
normalized to (min . max) then delete [min, max)."
  (nelisp-buffer-test--fresh "inv"
    (nelisp-insert "0123456789")
    (nelisp-delete-region 7 3)
    (should (equal "016789" (nelisp-buffer-string buf)))))

(ert-deftest nelisp-buffer-delete-region-out-of-range ()
  (nelisp-buffer-test--fresh "oor"
    (nelisp-insert "abc")
    (should-error (nelisp-delete-region 0 2) :type 'args-out-of-range)
    (should-error (nelisp-delete-region 1 10) :type 'args-out-of-range)))

;;; buffer-substring / char-after ------------------------------------

(ert-deftest nelisp-buffer-substring-range ()
  (nelisp-buffer-test--fresh "sub"
    (nelisp-insert "abcdef")
    (should (equal "bcd" (nelisp-buffer-substring 2 5 buf)))))

(ert-deftest nelisp-buffer-char-after-basic ()
  (nelisp-buffer-test--fresh "char"
    (nelisp-insert "xyz")
    (should (= ?x (nelisp-char-after 1 buf)))
    (should (= ?y (nelisp-char-after 2 buf)))
    (should (= ?z (nelisp-char-after 3 buf)))
    (should (null (nelisp-char-after 4 buf)))))

;;; erase / modified --------------------------------------------------

(ert-deftest nelisp-buffer-erase-resets ()
  (nelisp-buffer-test--fresh "erase"
    (nelisp-insert "something")
    (nelisp-narrow-to-region 3 5)
    (nelisp-erase-buffer)
    (should (equal "" (nelisp-buffer-string buf)))
    (should (= 1 (nelisp-point buf)))
    (should (= 1 (nelisp-point-min buf)))
    (should (= 1 (nelisp-point-max buf)))))

(ert-deftest nelisp-buffer-modified-flag ()
  (nelisp-buffer-test--fresh "mod"
    (should-not (nelisp-buffer-modified-p buf))
    (nelisp-insert "x")
    (should (nelisp-buffer-modified-p buf))
    (nelisp-buffer-set-modified nil buf)
    (should-not (nelisp-buffer-modified-p buf))))

;;; Narrowing ---------------------------------------------------------

(ert-deftest nelisp-buffer-narrow-widen ()
  (nelisp-buffer-test--fresh "nw"
    (nelisp-insert "abcdefghij")
    (nelisp-narrow-to-region 3 7)
    (should (= 3 (nelisp-point-min buf)))
    (should (= 7 (nelisp-point-max buf)))
    (nelisp-widen)
    (should (= 1 (nelisp-point-min buf)))
    (should (= 11 (nelisp-point-max buf)))))

(ert-deftest nelisp-buffer-narrowing-keeps-point-and-edit-range-in-sync ()
  (nelisp-buffer-test--with-pair "narrow-point"
    (with-current-buffer host
      (insert "abcdefghij")
      (goto-char 1)
      (narrow-to-region 3 7))
    (nelisp-with-buffer ne
      (nelisp-insert "abcdefghij")
      (nelisp-goto-char 1)
      (nelisp-narrow-to-region 3 7 ne))
    (should (equal (with-current-buffer host
                     (list (point) (point-min) (point-max)))
                   (list (nelisp-point ne) (nelisp-point-min ne)
                         (nelisp-point-max ne))))
    (with-current-buffer host
      (goto-char (point-max))
      (insert "X")
      (delete-region 4 6))
    (nelisp-with-buffer ne
      (nelisp-goto-char (nelisp-point-max ne) ne)
      (nelisp-insert "X" ne)
      (nelisp-delete-region 4 6 ne))
    (should (equal (with-current-buffer host
                     (list (point) (point-min) (point-max)))
                   (list (nelisp-point ne) (nelisp-point-min ne)
                         (nelisp-point-max ne))))
    (with-current-buffer host (widen))
    (nelisp-widen ne)
    (should (equal (with-current-buffer host (buffer-string))
                   (nelisp-buffer-string ne)))))

(ert-deftest nelisp-buffer-switch-restores-clamped-point-per-buffer ()
  (let ((nelisp-buffer--current nil)
        (a (nelisp-generate-new-buffer "switch-a"))
        (b (nelisp-generate-new-buffer "switch-b")))
    (nelisp-insert "abcdef" a)
    (nelisp-goto-char 1 a)
    (nelisp-narrow-to-region 3 5 a)
    (nelisp-insert "xyz" b)
    (nelisp-goto-char 2 b)
    (nelisp-set-buffer b)
    (nelisp-set-buffer a)
    (should (= 3 (nelisp-point a)))
    (should (<= (nelisp-point-min a) (nelisp-point a)))
    (should (<= (nelisp-point a) (nelisp-point-max a)))))

(ert-deftest nelisp-buffer-save-restriction-restores ()
  (nelisp-buffer-test--fresh "sr"
    (nelisp-insert "xxxyyyzzz")
    (nelisp-narrow-to-region 4 7)
    (let ((pre-lo (nelisp-point-min buf))
          (pre-hi (nelisp-point-max buf)))
      (nelisp-save-restriction
        (nelisp-narrow-to-region 1 3))
      (should (= pre-lo (nelisp-point-min buf)))
      (should (= pre-hi (nelisp-point-max buf))))))

(ert-deftest nelisp-buffer-save-excursion-restores ()
  (nelisp-buffer-test--fresh "se"
    (nelisp-insert "abcdef")
    (nelisp-goto-char 4)
    (nelisp-save-excursion
      (nelisp-goto-char 1))
    (should (= 4 (nelisp-point buf)))))

;;; Macro / with-buffer ----------------------------------------------

(ert-deftest nelisp-buffer-with-buffer-nests ()
  (nelisp-buffer--reset-registry)
  (let ((a (nelisp-generate-new-buffer "A"))
        (b (nelisp-generate-new-buffer "B")))
    (nelisp-with-buffer a
      (nelisp-insert "AAA")
      (nelisp-with-buffer b
        (nelisp-insert "BBB")
        (should (eq b (nelisp-current-buffer))))
      (should (eq a (nelisp-current-buffer))))
    (should (equal "AAA" (nelisp-buffer-string a)))
    (should (equal "BBB" (nelisp-buffer-string b)))))

;;; Host parity -------------------------------------------------------

(ert-deftest nelisp-buffer-parity-insert-sequence ()
  "Identical insert sequence produces identical text / point."
  (nelisp-buffer-test--with-pair "par-ins"
    (dolist (s '("hello" " " "world" "!"))
      (with-current-buffer host (insert s))
      (nelisp-insert s ne)
      (should (equal (with-current-buffer host (buffer-string))
                     (nelisp-buffer-string ne)))
      (should (= (with-current-buffer host (point))
                 (nelisp-point ne))))))

(ert-deftest nelisp-buffer-parity-goto-and-delete ()
  "goto-char + delete-region match host semantics."
  (nelisp-buffer-test--with-pair "par-del"
    (with-current-buffer host (insert "0123456789"))
    (nelisp-insert "0123456789" ne)
    (with-current-buffer host (goto-char 4) (delete-region 4 7))
    (nelisp-goto-char 4 ne) (nelisp-delete-region 4 7 ne)
    (should (equal (with-current-buffer host (buffer-string))
                   (nelisp-buffer-string ne)))
    (should (= (with-current-buffer host (point))
               (nelisp-point ne)))))

(ert-deftest nelisp-buffer-parity-narrow-bounds ()
  "point-min / point-max after narrow match host."
  (nelisp-buffer-test--with-pair "par-narrow"
    (with-current-buffer host (insert "xxxxxxxxxxx") (narrow-to-region 3 8))
    (nelisp-insert "xxxxxxxxxxx" ne) (nelisp-narrow-to-region 3 8 ne)
    (should (= (with-current-buffer host (point-min)) (nelisp-point-min ne)))
    (should (= (with-current-buffer host (point-max)) (nelisp-point-max ne)))))

(ert-deftest nelisp-buffer-parity-buffer-substring ()
  (nelisp-buffer-test--with-pair "par-sub"
    (with-current-buffer host (insert "abcdefghij"))
    (nelisp-insert "abcdefghij" ne)
    (should (equal (with-current-buffer host (buffer-substring 3 8))
                   (nelisp-buffer-substring 3 8 ne)))))

(ert-deftest nelisp-buffer-parity-multibyte-japanese ()
  "String length / point parity with host for multibyte input."
  (nelisp-buffer-test--with-pair "par-jp"
    (let ((s "こんにちは"))
      (with-current-buffer host (insert s))
      (nelisp-insert s ne)
      (should (equal (with-current-buffer host (buffer-string))
                     (nelisp-buffer-string ne)))
      (should (= (with-current-buffer host (point))
                 (nelisp-point ne))))))

(provide 'nelisp-buffer-test)

;;; nelisp-buffer-test.el ends here
