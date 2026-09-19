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

;; -- Doc 190 Phase C (this session): `ash'/`logand'/`logior'/`logxor'/
;; `lognot' accept a Bignum operand; `/'/`%'/`mod' accept a Bignum
;; DIVIDEND paired with a small fixnum divisor.  Every expected value
;; below is host-verified (GNU Emacs 31.1, this session's own
;; `emacs -Q --batch' run, not from memory) -- including the three
;; concrete motivating values from this task's own measured defect.

;; -- ash: motivating values (the AOT compiler's own imm64-splitting
;; shape, `nelisp-asm-x86_64--imm64-bytes').
(bignum-smoke--check "ash motivating value1 >> 8"
  (= (ash 8751669898145395319 -8) 34186210539630450))
(bignum-smoke--check "ash motivating value1 << 8, bignump"
  (let ((r (ash 8751669898145395319 8)))
    (and (= r 2240427493925221201664) (bignump r))))
(bignum-smoke--check "ash motivating value3 (>2^63) >> 8"
  (= (ash 16045481047390945280 -8) 62677660341370880))

;; -- ash: fixnum-operand left-shift overflow now PROMOTES instead of
;; wrapping (the concrete against-the-bug case this task names: "check
;; what shl does today with (ash 1 62) and (ash 1 63)").  The reviewer's
;; own pre-fix measurement on this tree, added here verbatim as the
;; against-the-bug corpus for this exact class: `(ash 1 61)' gave
;; -2305843009213693952, `(ash 1 62)' gave 0, `(ash 1 64)' gave 1, and
;; `(ash most-positive-fixnum 1)' gave -2 -- all four raw native-`shl'
;; wraparounds on a 64-bit register, not bignum promotions, and all four
;; WRONG NUMBERS WITH NO SIGNAL, worse than a signalled error.
(bignum-smoke--check "ash 1 61 exact and bignum (fixnum boundary itself)"
  (and (= (ash 1 61) 2305843009213693952) (bignump (ash 1 61))))
(bignum-smoke--check "ash 1 62 exact and bignum"
  (and (= (ash 1 62) 4611686018427387904) (bignump (ash 1 62))))
(bignum-smoke--check "ash 1 63 exact and bignum (was raw 64-bit wraparound)"
  (and (= (ash 1 63) 9223372036854775808) (bignump (ash 1 63))))
(bignum-smoke--check "ash 1 64 exact"
  (= (ash 1 64) 18446744073709551616))
(bignum-smoke--check "ash -1 62 exact and bignum"
  (and (= (ash -1 62) -4611686018427387904) (bignump (ash -1 62))))
(bignum-smoke--check "ash -1 63 exact and bignum"
  (and (= (ash -1 63) -9223372036854775808) (bignump (ash -1 63))))
(bignum-smoke--check "ash most-positive-fixnum 3 exact"
  (= (ash most-positive-fixnum 3) 18446744073709551608))
(bignum-smoke--check "ash most-negative-fixnum 3 exact"
  (= (ash most-negative-fixnum 3) -18446744073709551616))
(bignum-smoke--check "ash most-positive-fixnum 1, exact and bignum (reviewer's own pre-fix -2)"
  (and (= (ash most-positive-fixnum 1) 4611686018427387902)
       (bignump (ash most-positive-fixnum 1))))

;; -- ash: right shift by a count that reaches or exceeds the operand's
;; own bit length (also fixes a latent native-sar-masked-to-6-bits
;; fixnum bug this session found: a huge shift count must not be reduced
;; mod 64 by the hardware shift instruction).
(bignum-smoke--check "ash 1 -1000 is 0"
  (= (ash 1 -1000) 0))
(bignum-smoke--check "ash -1 -1000 is -1"
  (= (ash -1 -1000) -1))
(bignum-smoke--check "ash big -1000 is 0"
  (= (ash 100000000000000000000000000000 -1000) 0))
(bignum-smoke--check "ash neg big -1000 is -1"
  (= (ash -100000000000000000000000000000 -1000) -1))

;; -- ash: fixnum/bignum boundary, both shift directions, demotion
;; verified by tag (Doc 190 §4's own discipline), not just value.
(bignum-smoke--check "ash bignum boundary >> 1 demotes to fixnum"
  (let ((r (ash 2305843009213693952 -1)))
    (and (= r 1152921504606846976) (not (bignump r)))))
(bignum-smoke--check "ash neg bignum boundary >> 1, exact"
  (= (ash -2305843009213693953 -1) -1152921504606846977))
(bignum-smoke--check "ash bignum boundary << 1, exact"
  (= (ash 2305843009213693952 1) 4611686018427387904))
(bignum-smoke--check "ash neg bignum boundary << 1, exact"
  (= (ash -2305843009213693953 1) -4611686018427387906))

;; -- ash: negative-operand floor-shift semantics (rounds toward
;; -infinity, not toward zero): `(ash -5 -1)' is -3 = -(ceil(5/2)), not
;; -2 = -(floor(5/2)).
(bignum-smoke--check "ash -5 -1 is -3 (floor, not truncate)"
  (= (ash -5 -1) -3))
(bignum-smoke--check "ash neg bignum >> 37, exact (floor)"
  (= (ash -123456789012345678901234567890 -37) -898266364037013256))
(bignum-smoke--check "ash pos bignum >> 37, exact"
  (= (ash 123456789012345678901234567890 -37) 898266364037013255))
(bignum-smoke--check "ash neg bignum << 37, exact"
  (= (ash -123456789012345678901234567890 37)
     -16967771880870298596087029859591735214080))

;; -- logand/logior/logxor: bignum-bignum, all four sign combinations.
(bignum-smoke--check "logand big+ big+, exact"
  (= (logand 123456789012345678901234567890 987654321098765432109876543210)
     1943960184490269435062782658))
(bignum-smoke--check "logior big+ big+, exact"
  (= (logior 123456789012345678901234567890 987654321098765432109876543210)
     1109167149926620841576048328442))
(bignum-smoke--check "logxor big+ big+, exact"
  (= (logxor 123456789012345678901234567890 987654321098765432109876543210)
     1107223189742130572140985545784))
(bignum-smoke--check "logand big- big+, exact"
  (= (logand -123456789012345678901234567890 987654321098765432109876543210)
     985710360914275162674813760554))
(bignum-smoke--check "logior big- big+, exact"
  (= (logior -123456789012345678901234567890 987654321098765432109876543210)
     -121512828827855409466171785234))
(bignum-smoke--check "logxor big- big+, exact"
  (= (logxor -123456789012345678901234567890 987654321098765432109876543210)
     -1107223189742130572140985545788))
(bignum-smoke--check "logand big- big-, exact"
  (= (logand -123456789012345678901234567890 -987654321098765432109876543210)
     -1109167149926620841576048328442))
(bignum-smoke--check "logior big- big-, exact"
  (= (logior -123456789012345678901234567890 -987654321098765432109876543210)
     -1943960184490269435062782658))
(bignum-smoke--check "logxor big- big-, exact"
  (= (logxor -123456789012345678901234567890 -987654321098765432109876543210)
     1107223189742130572140985545784))

;; -- logand/logior/logxor: mixed bignum/fixnum operands, including the
;; motivating values, and demotion.
(bignum-smoke--check "logand big+ small fixnum mask, exact"
  (= (logand 123456789012345678901234567890 255) 210))
(bignum-smoke--check "logand big- small fixnum mask, exact"
  (= (logand -123456789012345678901234567890 255) 46))
(bignum-smoke--check "logior big+ -1 is -1"
  (= (logior 123456789012345678901234567890 -1) -1))
(bignum-smoke--check "logand big+ 0 is 0, demotes to fixnum"
  (let ((r (logand 123456789012345678901234567890 0)))
    (and (= r 0) (not (bignump r)))))
(bignum-smoke--check "logand motivating value1 255, exact"
  (= (logand 8751669898145395319 255) 119))
(bignum-smoke--check "logand motivating value2 255, exact"
  (= (logand 7161130726839247202 255) 98))
(bignum-smoke--check "logand motivating value3 (>2^63) 255, exact"
  (= (logand 16045481047390945280 255) 0))

;; -- lognot: bignum operand, both signs, and the fixnum/bignum boundary.
(bignum-smoke--check "lognot big+, exact and bignum"
  (let ((r (lognot 123456789012345678901234567890)))
    (and (= r -123456789012345678901234567891) (bignump r))))
(bignum-smoke--check "lognot big-, exact"
  (= (lognot -123456789012345678901234567890) 123456789012345678901234567889))
(bignum-smoke--check "lognot bignum boundary, exact"
  (= (lognot 2305843009213693952) -2305843009213693953))
(bignum-smoke--check "lognot neg bignum boundary, exact"
  (= (lognot -2305843009213693953) 2305843009213693952))

;; -- mod/%//: Bignum DIVIDEND with a small FIXNUM divisor, all four
;; dividend/divisor sign combinations, plus the motivating values.  Per
;; this task's own scope, `mod' itself needed NO native dispatch change
;; (it is pure Elisp over `/'/`*'/`-'/`<', scripts/nelisp-stdlib-
;; prelude.el) -- these checks also therefore prove that composition,
;; not just `/''s own new dispatch arm.
(bignum-smoke--check "mod motivating value1, exact"
  (= (mod 8751669898145395319 256) 119))
(bignum-smoke--check "mod motivating value2, exact"
  (= (mod 7161130726839247202 1000) 202))
(bignum-smoke--check "mod motivating value3 (>2^63), exact"
  (= (mod 16045481047390945280 1000) 280))
(bignum-smoke--check "mod big+ 6, exact"
  (= (mod 123456789012345678901234567891 6) 1))
(bignum-smoke--check "mod big- 6, exact"
  (= (mod -123456789012345678901234567891 6) 5))
(bignum-smoke--check "mod big+ -6, exact"
  (= (mod 123456789012345678901234567891 -6) -5))
(bignum-smoke--check "mod big- -6, exact"
  (= (mod -123456789012345678901234567891 -6) -1))
(bignum-smoke--check "% big+ 6, exact"
  (= (% 123456789012345678901234567891 6) 1))
(bignum-smoke--check "% big- 6, exact"
  (= (% -123456789012345678901234567891 6) -1))
(bignum-smoke--check "% big+ -6, exact"
  (= (% 123456789012345678901234567891 -6) 1))
(bignum-smoke--check "% big- -6, exact"
  (= (% -123456789012345678901234567891 -6) -1))
(bignum-smoke--check "/ big+ 6, exact and bignum"
  (let ((r (/ 123456789012345678901234567891 6)))
    (and (= r 20576131502057613150205761315) (bignump r))))
(bignum-smoke--check "/ big- 6, exact"
  (= (/ -123456789012345678901234567891 6) -20576131502057613150205761315))
(bignum-smoke--check "/ big+ -6, exact"
  (= (/ 123456789012345678901234567891 -6) -20576131502057613150205761315))
(bignum-smoke--check "/ big- -6, exact"
  (= (/ -123456789012345678901234567891 -6) 20576131502057613150205761315))
(bignum-smoke--check "/ bignum boundary by 2 demotes to fixnum"
  (let ((r (/ 2305843009213693952 2)))
    (and (= r 1152921504606846976) (not (bignump r)))))

;; -- mod//: divisor at the scoped bound (2^31 = 2147483648) and just
;; past it (2147483649, still a plain fixnum, but this build's
;; single-limb division loop cannot safely divide by it -- see the bound
;; derivation next to `nl_bignum_divmod_small_loop').
(bignum-smoke--check "/ big+ divisor at the 2^31 bound, exact"
  (= (/ 123456789012345678901234567890 2147483648) 57489047298368848348))
(bignum-smoke--check "mod big+ divisor at the 2^31 bound, exact"
  (= (mod 123456789012345678901234567890 2147483648) 1312754386))
(bignum-smoke--check "/ divisor just past the 2^31 bound signals the unsupported condition"
  (eq (condition-case nil
          (/ 123456789012345678901234567890 2147483649)
        (nelisp-bignum-division-unsupported 'ok))
      'ok))

;; -- /,%,mod: a genuine Bignum DIVISOR is explicitly out of scope and
;; must signal the new named condition -- not a silent wrong answer, not
;; the misleading `wrong-type-argument' (the divisor IS a number).
(bignum-smoke--check "/ bignum divisor signals nelisp-bignum-division-unsupported"
  (eq (condition-case nil
          (/ 123456789012345678901234567890 987654321098765432109876543210)
        (nelisp-bignum-division-unsupported 'ok))
      'ok))
(bignum-smoke--check "% bignum divisor signals nelisp-bignum-division-unsupported"
  (eq (condition-case nil
          (% 123456789012345678901234567890 987654321098765432109876543210)
        (nelisp-bignum-division-unsupported 'ok))
      'ok))
(bignum-smoke--check "mod bignum divisor signals nelisp-bignum-division-unsupported"
  (eq (condition-case nil
          (mod 123456789012345678901234567890 987654321098765432109876543210)
        (nelisp-bignum-division-unsupported 'ok))
      'ok))

(princ (format "BIGNUM-SMOKE cases=%d mismatches=%d\n"
               bignum-smoke--n bignum-smoke--bad))
nil
;;; standalone-bignum-smoke.el ends here
