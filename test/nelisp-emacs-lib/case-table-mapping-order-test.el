;;; case-table-mapping-order-test.el --- Ordered mapping regressions -*- lexical-binding: t; -*-
(require 'ert)
(load (expand-file-name "../../packages/nelisp-emacs-foundation/src/case-table.el"
                        (file-name-directory (or load-file-name buffer-file-name))) nil t)

(ert-deftest case-table-mapping-order/character-boundaries-and-stability ()
  (let* ((pairs '((4194303 . max) (256 . first) (0 . zero) (65536 . high)
                 (255 . ascii) (65535 . middle) (256 . second) (1 . one)))
         (source (copy-tree pairs))
         (identities (copy-sequence source))
         (ordered (case-table--ordered-mappings source)))
    (should (equal ordered '((0 . zero) (1 . one) (255 . ascii) (256 . first)
                            (256 . second) (65535 . middle) (65536 . high)
                            (4194303 . max))))
    (dolist (pair ordered) (should (memq pair identities)))))

(ert-deftest case-table-mapping-order/empty-and-unbounded-keys ()
  (should-not (case-table--ordered-mappings nil))
  (dolist (pairs '(((-1 . negative) (4 . four) (0 . zero))
                   ((4194304 . outside) (4194303 . max) (5 . five))))
    (should (equal (case-table--ordered-mappings (copy-tree pairs))
                   (sort (copy-tree pairs) (lambda (a b) (< (car a) (car b))))))))

(ert-deftest case-table-mapping-order/unicode-sized-scrambled-map ()
  (let ((i 0) pairs)
    (while (< i 5000)
      (push (cons (logand (* i 7919) 4194303) i) pairs)
      (setq i (1+ i)))
    (should (equal (case-table--ordered-mappings (copy-sequence pairs))
                   (sort (copy-sequence pairs) (lambda (a b) (< (car a) (car b))))))))

(require 'emacs-char-table)

(ert-deftest case-table-mapping-order/overlap-masks-and-parent-values ()
  (let ((parent (emacs-char-table-make 'case-table nil))
        (table (emacs-char-table-make 'case-table nil)))
    (dolist (pair '((65 . 66) (501 . 601) (502 . 602) (4194303 . 410)))
      (emacs-char-table-set parent (car pair) (cdr pair)))
    (emacs-char-table-set-parent table parent)
    (emacs-char-table-set-range table '(500 . 504) 700)
    (emacs-char-table-set-range table '(501 . 502) nil)
    (emacs-char-table-set table 503 703)
    (should (equal (case-table--sparse-mappings table)
                   '((65 . 66) (500 . 700) (501 . 601) (502 . 602)
                     (503 . 703) (504 . 700) (4194303 . 410))))))

(ert-deftest case-table-mapping-order/inverse-cycles-match-gnu ()
  (let* ((pairs '((65 . 97) (90 . 97) (97 . 97) (400 . 410) (410 . 410)))
         (native (make-char-table 'case-table nil))
         (mine (case-table--inverse-table pairs)))
    (dolist (pair pairs) (set-char-table-range native (car pair) (cdr pair)))
    (with-temp-buffer
      (set-case-table native)
      (let ((up (char-table-extra-slot native 0)))
        (dolist (pair pairs)
          (should (equal (emacs-char-table-ref mine (car pair))
                         (char-table-range up (car pair)))))))))

;;; case-table-mapping-order-test.el ends here
