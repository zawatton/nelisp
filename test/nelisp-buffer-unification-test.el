;;; nelisp-buffer-unification-test.el --- Doc 188 P1+P2 against-the-bug ERT -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Host-Emacs ERT companion to
;; `test/nelisp-buffer-unification-standalone-smoke.el', per Doc 188
;; §4.1's exact test bodies (P1 subset only -- the third test in that
;; section, `nelisp-buffer/marker-shifts-on-earlier-insert', needs
;; `point-marker'/`set-marker' wired to standard names, which is Doc 188
;; P4, not this phase).  Doc 188 P2 (2026-08-23) added a readable-spec
;; subset for `current-buffer'/`set-buffer'/`buffer-substring'/
;; `erase-buffer' below; the standalone smoke carries the full edge-case
;; coverage (error wording, argument-order, out-of-range) since this
;; file, running under host Emacs, cannot tell this tree's own wiring
;; apart from Emacs's real implementation either way (see below).
;;
;; IMPORTANT (Doc 188 §1.8/§4.1): under host Emacs these forms call
;; Emacs's OWN real `generate-new-buffer'/`insert'/`buffer-string'/
;; `point'/`goto-char' -- `scripts/nelisp-stdlib-prelude.el' is never
;; loaded by `make test' at all.  This file passing says nothing about
;; whether this tree's own prelude wiring is correct; that is exactly
;; what `test/nelisp-buffer-unification-standalone-smoke.el' (run
;; against `target/nelisp' itself) exists to prove.  Keep both: this
;; file is the readable spec Doc 188 §4.1 asks for and a regression
;; check against host-Emacs semantics drifting; the standalone smoke is
;; the one that can actually fail on this tree's own defect.

;;; Code:

(require 'ert)

(ert-deftest nelisp-buffer/insert-then-buffer-string-round-trips ()
  (let ((b (generate-new-buffer "t")))
    (with-current-buffer b (insert "abc"))
    (should (equal "abc" (with-current-buffer b (buffer-string))))))

(ert-deftest nelisp-buffer/goto-char-and-point-agree ()
  (let ((b (generate-new-buffer "t")))
    (with-current-buffer b (insert "abcdef") (goto-char 3))
    (should (= 3 (with-current-buffer b (point))))))

;; ---- Doc 188 P2 additions (2026-08-23) --------------------------------

(ert-deftest nelisp-buffer/current-buffer-set-buffer-round-trip ()
  (let ((outer (current-buffer))
        (b (generate-new-buffer "t")))
    (unwind-protect
        (progn
          (set-buffer b)
          (should (eq b (current-buffer))))
      (set-buffer outer)
      (kill-buffer b))))

(ert-deftest nelisp-buffer/buffer-substring-round-trip ()
  (should (equal "el" (with-temp-buffer (insert "hello") (buffer-substring 2 4))))
  (should (equal "el" (with-temp-buffer (insert "hello") (buffer-substring 4 2)))))

(ert-deftest nelisp-buffer/erase-buffer-round-trip ()
  (should (equal "" (with-temp-buffer (insert "abc") (erase-buffer) (buffer-string)))))

;;; nelisp-buffer-unification-test.el ends here
