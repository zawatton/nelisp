;;; standalone-overlay-print-smoke.el --- overlay self-reference print smoke  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Run under the standalone runtime, not host Emacs:
;;
;;     nelisp --load scripts/standalone-overlay-print-smoke.el
;;
;; Regression smoke for the button.el hang reported from the
;; nelisp-emacs-lib coverage lane: `make-button' followed by
;; `button-at' deterministically hung (3/3) on the standalone.
;;
;; Root cause: button.el's `make-button' deliberately stores the
;; overlay in its OWN `button' property --
;;
;;     (overlay-put overlay 'button overlay)
;;
;; -- so the overlay's property list holds a self-reference.  Overlays
;; are `nelisp-overlay' records (`src/nelisp-buffer.el'); before this
;; fix, `nelisp--prn-to-string' (`lisp/nelisp-stdlib-prn.el' /
;; `scripts/nelisp-stdlib-prelude.el', the copy the standalone runs)
;; had no special case for `nelisp-overlay-p' and fell through to the
;; generic `nelisp--prn-record' arm, which recurses into every slot --
;; including `properties' -- with no cycle guard and no depth counter.
;; Printing the self-referential overlay therefore recursed forever: a
;; hang, not a `max-lisp-eval-depth' signal.  Confirmed via `timeout
;; 15' before the fix: `(prin1-to-string overlay)' never returned.
;;
;; Emacs's own `print.c' never walks an overlay's property list at
;; all -- overlays always print opaquely as `#<overlay from START to
;; END in BUFFER>' / `#<overlay in no buffer>' (verified against
;; Emacs 30.1/31.1), independent of whatever the overlay's properties
;; hold.  The fix adds that same opaque, non-recursive rendering
;; (mirroring the pre-existing `nelisp-buffer-p'/`nelisp-marker-p'
;; clauses right above it), so printing an overlay never walks its
;; property list regardless of what it contains.
;;
;; This script uses the `nelisp-overlay*'/`nelisp-make-overlay' API
;; that is already wired into the standalone prelude (bare
;; `make-overlay'/`overlay-put'/... Emacs-compat names are a separate,
;; still-open gap -- out of scope for this fix, which is specifically
;; about the printer's cycle-walk, not about the missing bare-name
;; aliases).  The real vendored `button.el' flow (`make-button' /
;; `button-at' / `button-get' / `button-label' / `insert-button') was
;; separately verified against host Emacs with a temporary bare-name
;; shim; see the button-hang investigation notes for that transcript.

;;; Code:

(defvar ovly-print-smoke--n 0)
(defvar ovly-print-smoke--bad 0)

(defmacro ovly-print-smoke--check (label expected form)
  `(progn
     (setq ovly-print-smoke--n (1+ ovly-print-smoke--n))
     (let ((got (condition-case err ,form (error (list 'ERROR err)))))
       (unless (equal got ,expected)
         (setq ovly-print-smoke--bad (1+ ovly-print-smoke--bad))
         (princ (format "MISMATCH %s: expected %S, got %S\n" ,label ,expected got))))))

;; --- Bug repro: self-referential overlay must print opaquely, and
;; return promptly -- the whole point of this smoke is that a hang
;; here fails the run (via the harness's external `timeout') rather
;; than reporting a mismatch line.

;; Tests 01/02/05 use the ambient top-level buffer directly (not
;; `with-temp-buffer') so the printed buffer name is deterministically
;; "*scratch*" -- `rename-buffer' is not part of the standalone's
;; Emacs-compat surface yet (separate, unrelated gap).

(ovly-print-smoke--check "01-self-ref-button-prop-prints-opaque"
  "#<overlay from 1 to 2 in *scratch*>"
  (let ((ov (nelisp-make-overlay 1 2)))
    (nelisp-overlay-put ov 'button ov)
    (prin1-to-string ov)))

(ovly-print-smoke--check "02-self-ref-survives-format-%S"
  "#<overlay from 1 to 2 in *scratch*>"
  (let ((ov (nelisp-make-overlay 1 2)))
    (nelisp-overlay-put ov 'button ov)
    (format "%S" ov)))

(ovly-print-smoke--check "03-dead-overlay-prints-no-buffer"
  "#<overlay in no buffer>"
  (with-temp-buffer
    (let ((ov (nelisp-make-overlay 1 1)))
      (nelisp-overlay-put ov 'button ov)
      (nelisp-delete-overlay ov)
      (prin1-to-string ov))))

(ovly-print-smoke--check "04-equal-on-self-ref-overlay-same-object"
  t
  (with-temp-buffer
    (let ((ov (nelisp-make-overlay 1 2)))
      (nelisp-overlay-put ov 'button ov)
      (equal ov ov))))

(ovly-print-smoke--check "05-real-button-el-shape-multi-property"
  "#<overlay from 1 to 6 in *scratch*>"
  (progn
    (insert "hello world")
    (let ((ov (nelisp-make-overlay 1 6)))
      ;; Mirror `make-button''s exact sequence: user properties first,
      ;; then the self-reference, then the default category.
      (nelisp-overlay-put ov 'face 'bold)
      (nelisp-overlay-put ov 'button ov)
      (unless (nelisp-overlay-get ov 'category)
        (nelisp-overlay-put ov 'category 'default-button))
      (prin1-to-string ov))))

(ovly-print-smoke--check "06-overlays-at-finds-self-ref-button"
  t
  (with-temp-buffer
    (insert "hello world")
    (let ((ov (nelisp-make-overlay 1 6)))
      (nelisp-overlay-put ov 'button ov)
      (let ((hit (car (nelisp-overlays-at 3))))
        (and hit (eq (nelisp-overlay-get hit 'button) ov))))))

(princ (format "OVLY-PRINT-SMOKE cases=%d mismatches=%d\n"
               ovly-print-smoke--n ovly-print-smoke--bad))
