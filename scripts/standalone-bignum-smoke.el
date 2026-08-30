;;; standalone-bignum-smoke.el --- Doc 190 Phase A bignum smoke -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Run under the standalone runtime, not host Emacs:
;;
;;     nelisp --load scripts/standalone-bignum-smoke.el
;;
;; Doc 190 Phase A: the bignum box type (Sexp tag 13), reading (a literal
;; past most-positive-fixnum/most-negative-fixnum parses to a bignum
;; instead of wrapping), printing (prin1/read round-trip), comparison
;; (eql/=/</> across bignum-bignum and bignum-fixnum), integerp/numberp/
;; type-of.
;;
;; Doc 190 Phase B (2026-08-23): arithmetic (+/-/*) now PROMOTES a
;; fixnum-boundary overflow or a bignum operand to an exact Bignum result
;; instead of signalling `overflow-error' (Doc 187's contract, superseded
;; for these three ops only), demoting back to a plain fixnum whenever the
;; result re-fits -- superseding Phase A's own "no promotion in this
;; phase" note.  Every check is a plain value comparison (no host-Emacs
;; cross-check here; that lives in `tools/nelisp-substrate-parity-
;; corpus.el' entries 43/44/45/46/47 and in `test/nelisp-read-test.el',
;; which is where the Phase A bignum-fallback ERT cases actually live --
;; not a separate `nelisp-bignum-test.el', a naming slip in Phase A's own
;; original commentary here, corrected in passing since this comment
;; block was already being rewritten for Phase B).
;;
;; Also runs a GC stress round (allocate many bignums across several
;; garbage-collect cycles, then re-verify every one still compares/prints
;; correctly) -- Doc 190's own GC-integration risk zone, complementing
;; (not replacing) `make standalone-reader-checked-soak'.  Phase B adds a
;; SECOND stress round over ARITHMETIC-PRODUCED bignums specifically
;; (Phase A's round only ever exercised the reader's construction path,
;; never the new promotion allocation path).
;;
;; The host-comparable half of this behavior (reading/printing/comparison/
;; arithmetic are all host-comparable) lives in `tools/nelisp-substrate-
;; parity-corpus.el' entries 43/44/45/46/47, run by `make substrate-
;; parity-smoke'.

;;; Code:

(defvar bignum-smoke--n 0)
(defvar bignum-smoke--bad 0)

(defmacro bignum-smoke--check (label form)
  `(progn
     (setq bignum-smoke--n (1+ bignum-smoke--n))
     (condition-case e
         (unless ,form
           (setq bignum-smoke--bad (1+ bignum-smoke--bad))
           (princ (concat "FAIL " ,label "\n")))
       (error
        (setq bignum-smoke--bad (1+ bignum-smoke--bad))
        (princ (concat "FAIL " ,label " signalled " (prin1-to-string e) "\n"))))))

;; -- fencepost values, straddling most-positive-fixnum/most-negative-fixnum
;; exactly (Doc 190 §4's own fixnum-boundary corpus shape).
(bignum-smoke--check "N stays fixnum"
  (eq (type-of 2305843009213693951) 'integer))
(bignum-smoke--check "N not a bignum"
  (not (bignump 2305843009213693951)))
(bignum-smoke--check "N+1 promotes to bignum"
  (bignump 2305843009213693952))
(bignum-smoke--check "N+1 integerp"
  (integerp 2305843009213693952))
(bignum-smoke--check "N+1 numberp"
  (numberp 2305843009213693952))
(bignum-smoke--check "N+1 type-of integer"
  (eq (type-of 2305843009213693952) 'integer))
(bignum-smoke--check "-N-1 (most-negative-fixnum) stays fixnum"
  (not (bignump -2305843009213693952)))
(bignum-smoke--check "-N-2 promotes to bignum"
  (bignump -2305843009213693953))

;; -- printing: exact decimal, no wrap, matching what was written.
(bignum-smoke--check "N+1 prints exactly"
  (equal (prin1-to-string 2305843009213693952) "2305843009213693952"))
(bignum-smoke--check "huge positive prints exactly"
  (equal (prin1-to-string 123456789012345678901234567890)
         "123456789012345678901234567890"))
(bignum-smoke--check "huge negative prints exactly"
  (equal (prin1-to-string -123456789012345678901234567890)
         "-123456789012345678901234567890"))

;; -- prin1/read round-trip.
(let* ((b 123456789012345678901234567890)
       (b2 (car (read-from-string (prin1-to-string b)))))
  (bignum-smoke--check "round-trip bignump" (bignump b2))
  (bignum-smoke--check "round-trip equal" (equal b b2))
  (bignum-smoke--check "round-trip =" (= b b2))
  (bignum-smoke--check "round-trip eql" (eql b b2)))

;; -- comparison: bignum-bignum.
(bignum-smoke--check "big < big+1"
  (< 123456789012345678901234567890 123456789012345678901234567891))
(bignum-smoke--check "big+1 > big"
  (> 123456789012345678901234567891 123456789012345678901234567890))
(bignum-smoke--check "neg big < pos big"
  (< -123456789012345678901234567890 123456789012345678901234567890))
(bignum-smoke--check "big = itself via round-trip"
  (= 123456789012345678901234567890
     (car (read-from-string "123456789012345678901234567890"))))

;; -- comparison: bignum-fixnum, both directions, both signs.
(bignum-smoke--check "big > small fixnum"
  (> 2305843009213693952 5))
(bignum-smoke--check "small fixnum < big"
  (< 5 2305843009213693952))
(bignum-smoke--check "neg big < small fixnum"
  (< -2305843009213693953 5))
(bignum-smoke--check "small fixnum > neg big"
  (> 5 -2305843009213693953))

;; -- eq/eql/equal: eq is identity-only (two separately read bignums with
;; the same value must NOT be eq); eql/equal compare by value.
(let ((a (car (read-from-string "2305843009213693952")))
      (b (car (read-from-string "2305843009213693952"))))
  (bignum-smoke--check "eq same-value bignums is nil" (not (eq a b)))
  (bignum-smoke--check "eql same-value bignums is t" (eql a b))
  (bignum-smoke--check "equal same-value bignums is t" (equal a b))
  (bignum-smoke--check "= same-value bignums is t" (= a b)))

;; -- Doc 190 Phase B: arithmetic (+/-/*) on a bignum operand now WORKS
;; (contagion), superseding Phase A's own non-boundary above.  Values
;; host-verified (GNU Emacs 30.1, 2026-08-23).
(bignum-smoke--check "+ on bignum operand, exact"
  (= (+ 2305843009213693952 1) 2305843009213693953))
(bignum-smoke--check "- on bignum operand, exact"
  (= (- 2305843009213693952 1) 2305843009213693951))
(bignum-smoke--check "* on bignum operand, exact"
  (= (* 2305843009213693952 1) 2305843009213693952))
(bignum-smoke--check "* on bignum operand (arg order), exact"
  (= (* 1 2305843009213693952) 2305843009213693952))

;; -- Doc 190 Phase B: fixnum-boundary overflow at +/-/* PROMOTES to an
;; exact Bignum instead of signalling `overflow-error' -- the actual
;; against-the-bug case this phase exists for.  Host-verified.
(bignum-smoke--check "+ fixnum overflow promotes, exact"
  (= (+ most-positive-fixnum 1) 2305843009213693952))
(bignum-smoke--check "+ fixnum overflow promotes, bignump"
  (bignump (+ most-positive-fixnum 1)))
(bignum-smoke--check "- (negation) overflow promotes, exact"
  (= (- most-negative-fixnum) 2305843009213693952))
(bignum-smoke--check "- (n-ary) overflow promotes, exact"
  (= (- most-negative-fixnum 1) -2305843009213693953))
(bignum-smoke--check "* true-64-bit overflow promotes, exact"
  (= (* 100000000000 100000000000) 10000000000000000000000))
(bignum-smoke--check "* narrow fixnum-boundary overflow promotes, exact"
  (= (* most-positive-fixnum 2) 4611686018427387902))
(bignum-smoke--check "* narrow boundary (arg order) promotes, exact"
  (= (* 2 most-positive-fixnum) 4611686018427387902))

;; -- bignum-bignum arithmetic: genuine multi-limb (most-positive-fixnum
;; squared needs 3 32-bit limbs, not the 2-limb fixnum-derived case
;; above) -- exercises the schoolbook multiply's carry propagation across
;; more than one limb boundary, not just the narrow 2-limb path.
(bignum-smoke--check "bignum * bignum, exact (multi-limb)"
  (= (* most-positive-fixnum most-positive-fixnum)
     5316911983139663487003542222693990401))
(bignum-smoke--check "bignum + its own negation = 0, demotes to fixnum"
  (let ((r (+ (* most-positive-fixnum most-positive-fixnum)
              (- (* most-positive-fixnum most-positive-fixnum)))))
    (and (= r 0) (not (bignump r)))))
(bignum-smoke--check "bignum - itself = 0, demotes to fixnum"
  (let* ((b (+ most-positive-fixnum 1)) (r (- b b)))
    (and (= r 0) (not (bignump r)))))
(bignum-smoke--check "bignum-fixnum multiply demotes back to fixnum"
  ;; (most-positive-fixnum+1) is a bignum; times -1 is most-negative-
  ;; fixnum, a PLAIN fixnum again -- canonicality (Doc 190 §2), not just
  ;; value equality.
  (let ((r (* -1 (+ most-positive-fixnum 1))))
    (and (= r most-negative-fixnum) (not (bignump r)))))
(bignum-smoke--check "literal bignum (reader) + fixnum, exact"
  ;; Exercises the READER's own bignum construction (Phase A) feeding
  ;; straight into Phase B's arithmetic promotion.
  (= (+ 100000000000000000000000000 1) 100000000000000000000000001))

;; -- demotion / canonicality: a bignum-producing op followed by one that
;; brings the magnitude back under 2^61 must yield a plain Sexp::Int, not
;; a Bignum that merely PRINTS/COMPARES as if it were one (Doc 190 §4's
;; own required discipline: "verified by tag inspection, not just value
;; equality").
(bignum-smoke--check "demotion fencepost: N+1-1 is a fixnum, not a bignum"
  (let ((r (- (+ most-positive-fixnum 1) 1)))
    (and (= r most-positive-fixnum) (not (bignump r)) (integerp r))))
(bignum-smoke--check "demotion fencepost matches host type-of"
  (eq (type-of (- (+ most-positive-fixnum 1) 1)) 'integer))

;; -- fencepost controls: unaffected in-range arithmetic (no false
;; positives from the new bignum-detection checks).
(bignum-smoke--check "in-range + unaffected"
  (= (+ (1- most-positive-fixnum) 1) most-positive-fixnum))
(bignum-smoke--check "in-range * unaffected"
  (= (* most-positive-fixnum 1) most-positive-fixnum))
(bignum-smoke--check "expt overflow-check unaffected (Doc 187 precedent, unchanged)"
  (eq (condition-case nil (expt 2 61) (overflow-error 'ok)) 'ok))

;; -- GC stress round (Doc 190's own risk zone): allocate many distinct
;; bignums across several garbage-collect cycles, keep every one referenced,
;; then re-verify all of them still print/compare correctly.  Complements
;; (does not replace) `make standalone-reader-checked-soak'.
(let ((kept nil) (i 0))
  (while (< i 500)
    (push (car (read-from-string
                (concat "1" (make-string (+ 25 (mod i 20)) ?0) (number-to-string i))))
          kept)
    (setq i (1+ i))
    (when (= (mod i 100) 0) (garbage-collect)))
  (garbage-collect)
  (bignum-smoke--check "GC stress: every kept bignum still a bignum"
    (let ((ok t))
      (dolist (b kept) (unless (bignump b) (setq ok nil)))
      ok))
  (bignum-smoke--check "GC stress: every kept bignum still prints with a leading '1'"
    (let ((ok t))
      (dolist (b kept)
        (unless (= (aref (prin1-to-string b) 0) ?1) (setq ok nil)))
      ok))
  (bignum-smoke--check "GC stress: count preserved"
    (= (length kept) 500)))

;; -- GC stress round, part 2 (Doc 190 Phase B): the SAME stress shape as
;; above, but every kept bignum is ARITHMETIC-PRODUCED (chained `+'/`*'),
;; not read from a literal -- Phase A's own stress round only ever
;; exercised the reader's construction path, never the promotion
;; allocation path `wf_sum_big'/`wf_prod_big'/`wf_subtail_big' add.
(let ((kept nil) (acc (+ most-positive-fixnum 1)) (i 0))
  (while (< i 300)
    (setq acc (+ acc (* most-positive-fixnum 2)))
    (push acc kept)
    (setq i (1+ i))
    (when (= (mod i 100) 0) (garbage-collect)))
  (garbage-collect)
  (bignum-smoke--check "GC stress (arithmetic-produced): every kept value still a bignum"
    (let ((ok t))
      (dolist (b kept) (unless (bignump b) (setq ok nil)))
      ok))
  (bignum-smoke--check "GC stress (arithmetic-produced): monotonically increasing"
    (let ((ok t) (prev nil))
      (dolist (b (reverse kept))
        (when (and prev (not (> b prev))) (setq ok nil))
        (setq prev b))
      ok))
  (bignum-smoke--check "GC stress (arithmetic-produced): count preserved"
    (= (length kept) 300)))

(princ (format "BIGNUM-SMOKE cases=%d mismatches=%d\n"
               bignum-smoke--n bignum-smoke--bad))
nil
;;; standalone-bignum-smoke.el ends here
