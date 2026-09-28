;;; nelisp-eln-objects-symbol-t-test.el --- genuine GNU `t' codec support -*- lexical-binding: t; -*-

;; Before this fix, `(nelisp-eln-objects-encode unit t)' signalled
;; `(nelisp-eln-objects-unsupported unsupported-symbol-state t)': `t' is
;; interned and bound, so `nelisp-eln-objects--fresh-symbol-p' rejects it
;; the same way it rejects any other ordinary interned symbol, and nothing
;; else in the codec admitted `t' as a builtin constant.  This blocked any
;; genuine GNU native predicate (e.g. `zerop') from handing `t' back through
;; the callable-import/raw-call path; see the
;; `nelisp-eln-increment-same-artifact-zerop-nelisp' driver, whose
;; RESULT_ENCODE count only reached 2/5 (the nil- and error-returning cases)
;; before this fix.
;;
;; Evidence for the exact word this codec now uses (96, not GNU's literal
;; `t' offset of 48) is recorded on `nelisp-eln-objects--symbol-t-word'.

(require 'ert)
(require 'nelisp-eln-objects)

(ert-deftest nelisp-eln-objects-encode-t-no-longer-signals-unsupported ()
  "The smallest repro: encoding `t' used to signal `unsupported-symbol-state'."
  (let ((unit (nelisp-eln-objects-create)))
    (unwind-protect
        (should (integerp (nelisp-eln-objects-encode unit t)))
      (nelisp-eln-objects-release unit))))

(ert-deftest nelisp-eln-objects-encode-t-matches-host-derived-word ()
  "The word matches this codec's cited GNU Emacs 31.1 host evidence.
gdb, breaking at a live `Fkill_emacs' so the symbol table is fully
initialized, gives (against /usr/local/bin/emacs, GNU Emacs 31.1):
  (macro expand Qnil)     => builtin_lisp_symbol (0)   ; iQnil = 0
  (macro expand Qt)       => builtin_lisp_symbol (1)   ; iQt   = 1
  (macro expand Qunbound) => builtin_lisp_symbol (2)   ; iQunbound = 2
  (print sizeof(struct Lisp_Symbol)) => 48
So genuine GNU `Qt' is byte offset 1*48=48 from `lispsym', tag 0.  This
codec's pre-existing `nelisp-eln-objects--symbol-qunbound-offset' already
privately claimed that exact word (48) for Qunbound before `t' existed
here, so `t' is assigned this codec's next private tag-0/48-byte-stride
slot (96) instead -- see `nelisp-eln-objects--symbol-t-word' for the full
citation.  This test pins that specific, deliberate value so a future
change cannot silently drift it back onto Qunbound's word."
  (should (= nelisp-eln-objects--symbol-t-word 96))
  (should (= nelisp-eln-objects--symbol-t-word
             (* 2 nelisp-eln-objects--symbol-view-bytes)))
  ;; Tag-0 (symbol), per the ABI's `:tag-map', and distinct from nil (0)
  ;; and from Qunbound's wire word (48).
  (should (= (logand nelisp-eln-objects--symbol-t-word 7) 0))
  (should (/= nelisp-eln-objects--symbol-t-word 0))
  (should (/= nelisp-eln-objects--symbol-t-word 48))
  (let ((unit (nelisp-eln-objects-create)))
    (unwind-protect
        (should (= (nelisp-eln-objects-encode unit t)
                   nelisp-eln-objects--symbol-t-word))
      (nelisp-eln-objects-release unit))))

(ert-deftest nelisp-eln-objects-decode-t-round-trips-through-both-apis ()
  "decode(encode(t)) is `t' via both the unit and activation decode APIs."
  (let* ((unit (nelisp-eln-objects-create))
         (word (nelisp-eln-objects-encode unit t)))
    (unwind-protect
        (progn
          (should (eq (nelisp-eln-objects-decode unit word) t))
          (let ((activation (nelisp-eln-objects-activation-acquire unit)))
            (unwind-protect
                (should (eq (nelisp-eln-objects-activation-decode
                             activation word)
                            t))
              (nelisp-eln-objects-activation-release activation))))
      (nelisp-eln-objects-release unit))))

(ert-deftest nelisp-eln-objects-encode-decode-t-needs-no-symbol-base ()
  "Preserves the codec's GC/owner rules: `t', like nil, is a pure
immediate.  Encoding or decoding it must not allocate or lease the shared
Qunbound/registration `nelisp-eln-objects--symbol-base', unlike every
dynamically-created or registration-only symbol."
  (should (null nelisp-eln-objects--symbol-base))
  (let* ((unit (nelisp-eln-objects-create))
         (word (nelisp-eln-objects-encode unit t)))
    (unwind-protect
        (progn
          (should (null nelisp-eln-objects--symbol-base))
          (should (eq (nelisp-eln-objects-decode unit word) t))
          (should (null nelisp-eln-objects--symbol-base)))
      (nelisp-eln-objects-release unit))
    (should (null nelisp-eln-objects--symbol-base))))

(ert-deftest nelisp-eln-objects-t-still-fails-fresh-symbol-p-directly ()
  "Demonstrates the fix is additive, not a loosened admission rule: `t'
still fails the ordinary constructor-default admission test the same way
it did before this change (interned and bound); it is admitted only
through the new, narrowly-scoped `eq value t' branches in preflight,
encode, and decode, never through `nelisp-eln-objects--fresh-symbol-p'."
  (should-not (nelisp-eln-objects--fresh-symbol-p t)))

(ert-deftest nelisp-eln-objects-encode-unknown-symbol-still-fails-closed ()
  "Negative control: an ordinary bound/interned symbol other than `t' or
`nil' must still fail closed with `unsupported-symbol-state', exactly as
before this change."
  (let ((unit (nelisp-eln-objects-create)))
    (unwind-protect
        (should-error (nelisp-eln-objects-encode unit 'car)
                       :type 'nelisp-eln-objects-unsupported)
      (nelisp-eln-objects-release unit))))

(ert-deftest nelisp-eln-objects-nil-encoding-is-unaffected ()
  "Regression guard: nil's pre-existing immediate encoding is untouched."
  (let ((unit (nelisp-eln-objects-create)))
    (unwind-protect
        (progn
          (should (= (nelisp-eln-objects-encode unit nil) 0))
          (should (eq (nelisp-eln-objects-decode unit 0) nil)))
      (nelisp-eln-objects-release unit))))

(provide 'nelisp-eln-objects-symbol-t-test)
;;; nelisp-eln-objects-symbol-t-test.el ends here
