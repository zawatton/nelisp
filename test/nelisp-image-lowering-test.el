;;; nelisp-image-lowering-test.el --- ERT for image lowering -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;;; Code:

(require 'ert)
(require 'nelisp-read)
(require 'nelisp-image-lowering)

(defun nelisp-image-lowering-test--read-word (heap offset)
  (let ((word 0))
    (dotimes (i 8 word)
      (setq word (logior word (lsh (aref heap (+ offset i)) (* 8 i)))))))

(ert-deftest nelisp-image-lowering-test/nil-lowers-to-immediate ()
  (pcase-let ((`(,heap . ,relocs) (nelisp-image-lower nil)))
    (should (= (length heap) 8))
    (should (= (nelisp-image-lowering-test--read-word heap 0)
               nelisp-image-lowering--nl-value-tag-nil))
    (should (equal relocs nil))))

(ert-deftest nelisp-image-lowering-test/int-7-lowers-to-immediate ()
  (pcase-let ((`(,heap . ,relocs) (nelisp-image-lower 7)))
    (should (= (length heap) 8))
    (should (= (nelisp-image-lowering-test--read-word heap 0)
               (nelisp-image-lowering--tag-int 7)))
    (should (equal relocs nil))))

(ert-deftest nelisp-image-lowering-test/int-minus-3-lowers-to-immediate ()
  (pcase-let ((`(,heap . ,relocs) (nelisp-image-lower -3)))
    (should (= (length heap) 8))
    (should (= (nelisp-image-lowering-test--read-word heap 0)
               (nelisp-image-lowering--tag-int -3)))
    (should (equal relocs nil))))

(ert-deftest nelisp-image-lowering-test/t-lowers-to-immediate ()
  (pcase-let ((`(,heap . ,relocs) (nelisp-image-lower t)))
    (should (= (length heap) 8))
    (should (= (nelisp-image-lowering-test--read-word heap 0)
               nelisp-image-lowering--nl-immediate-t))
    (should (equal relocs nil))))

(ert-deftest nelisp-image-lowering-test/float-lowers-to-struct-plus-reloc ()
  (pcase-let ((`(,heap . ,relocs) (nelisp-image-lower 3.14)))
    (should (= (length heap) 16))
    (should (equal relocs (list (list :write-at 0 :addend 14 :kind 1))))
    (should (= (nelisp-image-lowering-test--read-word heap 8)
               #x40091eb851eb851f))))

(ert-deftest nelisp-image-lowering-test/list-one-two-three-layout ()
  (pcase-let ((`(,heap . ,relocs) (nelisp-image-lower (nelisp-read "(1 2 3)"))))
    (should (= (length heap) 56))
    (should (equal relocs
                   (list (list :write-at 0 :addend 10 :kind 1)
                         (list :write-at 16 :addend 26 :kind 1)
                         (list :write-at 32 :addend 42 :kind 1))))
    (should (= (nelisp-image-lowering-test--read-word heap 8)
               (nelisp-image-lowering--tag-int 1)))
    (should (= (nelisp-image-lowering-test--read-word heap 24)
               (nelisp-image-lowering--tag-int 2)))
    (should (= (nelisp-image-lowering-test--read-word heap 40)
               (nelisp-image-lowering--tag-int 3)))
    (should (= (nelisp-image-lowering-test--read-word heap 48)
               nelisp-image-lowering--nl-value-tag-nil))
    (should (= (nelisp-image-lowering-test--read-word heap 0) 0))
    (should (= (nelisp-image-lowering-test--read-word heap 16) 0))
    (should (= (nelisp-image-lowering-test--read-word heap 32) 0))))

(ert-deftest nelisp-image-lowering-test/dotted-pair-symbol-layout ()
  (pcase-let ((`(,heap . ,relocs) (nelisp-image-lower (nelisp-read "(a . b)"))))
    (should (= (length heap) 88))
    (should (= (length relocs) 5))
    (should (= (nelisp-image-lowering-test--read-word heap 0) 0))
    (should (= (nelisp-image-lowering-test--read-word heap 8) 0))
    (should (= (nelisp-image-lowering-test--read-word heap 16) 0))
    (should (= (nelisp-image-lowering-test--read-word heap 24) 1))
    (should (= (nelisp-image-lowering-test--read-word heap 32) ?a))
    (should (= (nelisp-image-lowering-test--read-word heap 48)
               nelisp-image-lowering--nl-value-tag-nil))
    (should (= (nelisp-image-lowering-test--read-word heap 56) 1))
    (should (= (nelisp-image-lowering-test--read-word heap 64) ?b))
    (should (= (nelisp-image-lowering-test--read-word heap 80)
               nelisp-image-lowering--nl-value-tag-nil))
    (should (equal relocs
                   (list (list :write-at 0 :addend 10 :kind 1)
                         (list :write-at 40 :addend 28 :kind 1)
                         (list :write-at 8 :addend 45 :kind 1)
                         (list :write-at 72 :addend 60 :kind 1)
                         (list :write-at 16 :addend 77 :kind 1))))))

(ert-deftest nelisp-image-lowering-test/vector-one-two-three-layout ()
  (pcase-let ((`(,heap . ,relocs) (nelisp-image-lower [1 2 3])))
    (should (= (length heap) 40))
    (should (equal relocs (list (list :write-at 0 :addend 15 :kind 1))))
    (should (= (nelisp-image-lowering-test--read-word heap 8) 3))
    (should (= (nelisp-image-lowering-test--read-word heap 16)
               (nelisp-image-lowering--tag-int 1)))
    (should (= (nelisp-image-lowering-test--read-word heap 24)
               (nelisp-image-lowering--tag-int 2)))
    (should (= (nelisp-image-lowering-test--read-word heap 32)
               (nelisp-image-lowering--tag-int 3)))))

(ert-deftest nelisp-image-lowering-test/string-hello-layout ()
  (pcase-let ((`(,heap . ,relocs) (nelisp-image-lower "hello")))
    (should (= (length heap) 24))
    (should (equal relocs (list (list :write-at 0 :addend 12 :kind 1))))
    (should (= (nelisp-image-lowering-test--read-word heap 0) 0))
    (should (= (nelisp-image-lowering-test--read-word heap 8) 5))
    (should (equal (substring heap 16 21) "hello"))
    (should (equal (substring heap 21 24) (make-string 3 0)))))

(ert-deftest nelisp-image-lowering-test/cons-chain-matches-proper-list ()
  (let* ((from-reader (nelisp-image-lower (nelisp-read "(1 2)")))
         (from-cons (nelisp-image-lower (cons 1 (cons 2 nil)))))
    (should (equal from-cons from-reader))))

(provide 'nelisp-image-lowering-test)

;;; nelisp-image-lowering-test.el ends here
