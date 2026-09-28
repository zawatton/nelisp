;;; nelisp-eln-chain-unwind-driver.el --- S4.6 chain owner/exit-contract probe -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Ledger S4.6.  Two sections, deliberately at different levels of
;; genuineness, both stated plainly in their own STAGE lines:
;;
;; Section 1 ("ADMIT") is fully genuine: it opens the real
;; gnu-chain.eln (NELISP_ELN_GNU_INPUT) with
;; `nelisp-eln-system-loader-open', reads its real function bytes, and
;; runs the real `nelisp-eln-native-subr-chain-import-analysis' and
;; `nelisp-eln-tail-code-analyze-chain-call' against real ELF/GOT
;; metadata -- no mocking at all.  It also independently confirms, on
;; the real dynamic-binding negative control
;; (NELISP_ELN_GNU_DYNAMIC_INPUT), that admission fails closed, and
;; that the generic `comp--register-subr' `load' path admits
;; gnu-chain.eln and publishes an arity-2 native subr (Doc 207; the live
;; native execution of that subr is covered by
;; test/nelisp-eln-nonlocal-chain-smoke.sh).
;;
;; Section 2 ("UNWIND") exercises the real
;; `nelisp-eln-callable-import--call-chain' and
;; `nelisp-eln-callable-import--dispatch' -- the actual S4.5 owner/exit
;; contract code this ledger wires the new `Ffuncall' MANY descriptor
;; through -- for every outcome (normal return, error, throw, quit),
;; with G a real Lisp closure and the real `nelisp-eln-objects-*'
;; encode/decode/lease machinery, real callback-import frame stack, and
;; real baseline bookkeeping.  Only the unavoidable native/FFI floor
;; (`ptr-call', `nelisp-native-load-*', `nelisp-eln-abi-read-word' for
;; the callback's raw memory words, and the outermost
;; `nelisp-eln-raw-call-word') is stood in for, using the same
;; technique `test/nelisp-eln-callable-import-test.el' already uses for
;; the existing unary/MANY lanes -- calling the real `--dispatch'
;; directly from the mocked outer raw call, with a fabricated 7-word
;; callback descriptor and a fabricated 2-word MANY argv, rather than a
;; live wired import table.  As found in Section 1, wiring a live table
;; for this artifact shape requires the generic load path this ledger
;; does not own and which itself does not yet admit this artifact's
;; module; the existing "same-artifact" zerop driver stops at the same
;; boundary (it calls the real `=' builtin directly rather than
;; executing zerop.eln's machine code).  G's own call to "the genuine
;; native DECREMENT" is, for the same reason, a counted Lisp stand-in
;; (1-) rather than a second live native call; the counter distinguishes
;; it from the chain call so the print line can show both counters
;; advancing on the normal-return case, as the ledger asks.

(require 'cl-lib)
(require 'nelisp-eln-system-loader)
(require 'nelisp-eln-native-subr)
(require 'nelisp-eln-callable-import)

;;; Section 1: real admission, real rejection.

(defconst nelisp-eln-chain-unwind--c-name
  "F6e656c6973702d676e752d636861696e_nelisp_gnu_chain_0")

(defun nelisp-eln-chain-unwind--admit (path)
  "Genuinely admit PATH's `nelisp-gnu-chain' leaf, or return nil."
  (let* ((handle (nelisp-eln-system-loader-open path))
         (cap (nelisp-eln-system-loader-function-capability
               handle nelisp-eln-chain-unwind--c-name))
         (size (nth 6 cap))
         (code (nelisp-eln-system-loader-read-root-function-bytes
                handle nelisp-eln-chain-unwind--c-name 0 size)))
    (nelisp-eln-native-subr-chain-import-analysis handle cap code "ba35c031")))

(let ((chain-input (getenv "NELISP_ELN_GNU_INPUT"))
      (dynamic-input (getenv "NELISP_ELN_GNU_DYNAMIC_INPUT")))
  (unless (and chain-input (file-exists-p chain-input))
    (error "NELISP_ELN_GNU_INPUT must name a readable gnu-chain.eln"))
  (unless (and dynamic-input (file-exists-p dynamic-input))
    (error "NELISP_ELN_GNU_DYNAMIC_INPUT must name a readable dynamic-binding control"))
  (let ((analysis (nelisp-eln-chain-unwind--admit chain-input)))
    (unless (and (eq (plist-get analysis :safe) t)
                 (equal (mapcar (lambda (i) (plist-get i :slot))
                                (plist-get analysis :imports))
                        '(1301 945))
                 (equal (nth 2 (plist-get analysis :unary-descriptor)) '1+)
                 (equal (nth 2 (plist-get analysis :many-descriptor)) 'funcall))
      (error "genuine gnu-chain.eln was not admitted: %S" analysis))
    (princ "NELISP_CHAIN_STAGE=admit_genuine_chain_slots_1301_945\n"))
  (when (nelisp-eln-chain-unwind--admit dynamic-input)
    (error "the dynamic-binding negative control was wrongly admitted"))
  (princ "NELISP_CHAIN_STAGE=reject_dynamic_binding_control\n")
  ;; Doc 207: the generic `comp--register-subr' load path now admits this
  ;; two-argument chain leaf (arity-2 top-level stub, chain body, two-slot
  ;; import table) and publishes a genuine arity-2 native subr.
  (load chain-input nil t t)
  (unless (and (subrp (symbol-function 'nelisp-gnu-chain))
               (equal (func-arity (symbol-function 'nelisp-gnu-chain))
                      '(2 . 2)))
    (error "the generic load path did not publish the arity-2 chain subr"))
  (princ "NELISP_CHAIN_STAGE=registration_admits_chain_via_generic_load\n"))

;;; Section 2: owner/exit contract, real dispatch, mocked native floor.

(defconst nelisp-eln-chain-unwind--descriptor-addr 8192)
(defconst nelisp-eln-chain-unwind--argv-addr 20480)

;; Cross-cutting mock state.  Must be real special variables (`defvar'),
;; not plain `let' locals, because the mocked functions below are
;; closed over at a different lexical position than where their values
;; are set, and a `throw' from deep inside G must leave them readable.
(defvar nelisp-eln-chain-unwind--chain-calls 0)
(defvar nelisp-eln-chain-unwind--encoded-words nil)
(defvar nelisp-eln-chain-unwind--x 0)
(defvar nelisp-eln-chain-unwind--g-behavior nil)

;; `nelisp-eln-objects-encode' admits only "fresh" symbols (constructor-
;; default uninterned, unbound, fmakunbound, nil-plist -- see
;; `nelisp-eln-objects--fresh-symbol-p's docstring): it has no
;; representation for an ordinary `defun'd or lambda-expression
;; callable at all, by design ("Bound/function/plist mutations are
;; rejected rather than guessed into GNU's fields").  So G is, each
;; time `--run' below is called, a fresh sanctioned symbol
;; `nelisp-eln-objects-make-uninterned-symbol' produces from a
;; fresh, never-released prep unit local to that one call (its GNU-side
;; view is owned by, and freed with, the unit that created it -- verified
;; empirically that releasing that unit erases the symbol's global
;; record and makes it unencodable again).  G is left `fboundp' nil for
;; `--call-chain's ENTIRE argument-encoding phase (both G and X) and is
;; only `fset' inside the `nelisp-eln-raw-call-word' mock below, after
;; that phase is complete and strictly before the mocked native floor's
;; `apply' reaches it: `nelisp-eln-objects-encode's own preflight
;; re-validates every root ever encoded on a unit, not just the value
;; passed to the current call, on every call, so fsetting G any
;; earlier (e.g. right after G's own encode call, before X's separate
;; encode call on the same unit) made that later call re-run
;; `nelisp-eln-objects--fresh-symbol-p' on G and reject it -- a real bug
;; in this driver's own sequencing, not in the codec (this is why an
;; isolated `--eval' of just "encode G, fset G, decode, funcall" alone
;; never reproduced the failure: it never encoded a second value
;; afterward on the same unit).

(defun nelisp-eln-chain-unwind--baseline ()
  (list (length nelisp-eln-objects--live-units)
        (length nelisp-eln-objects--activations)
        (length nelisp-eln-objects--identity-records)
        (length nelisp-eln-objects--pending-cleanups)
        (length nelisp-eln-callable-import--frames)
        (length nelisp-eln-callable-import--pending-cleanups)))

(defun nelisp-eln-chain-unwind--run (behavior x skip-release)
  "Drive one real `--call-chain'/`--dispatch' round trip for G and X.
G is always the shared `nelisp-eln-chain-unwind--g' fresh symbol;
BEHAVIOR is the function it is `fset' to for this one call, timed by
the encode spy below to land strictly after G's own real encode call
succeeds (which requires G still be `fboundp' nil) and strictly before
the mocked native floor's `apply' reaches it.  Returns (OUTCOME-TAG
VALUE-OR-CONDITION CHAIN-CALLS).  SKIP-RELEASE, when non-nil, breaks
`nelisp-eln-objects-activation-release' into a no-op for this one call
only, for the negative-control case.

The `Ffuncall' MANY argv the mocked native floor hands back to the
real `--dispatch' is (g-word, encoded-fresh-fixnum(1+x)): g-word is
captured, not independently re-encoded, from `--call-chain's own real
`nelisp-eln-objects-encode' call on ARGUMENTS, so it is bit-identical
to what the genuine artifact's own array[0] store would have carried;
the second word stands for the genuine inline fast-path's own
freshly-computed register value, which a fixnum needs no lease to
decode."
  (let* ((g-prep-unit (nelisp-eln-objects-create))
         (g (car (nelisp-eln-objects-make-uninterned-symbol
                  g-prep-unit "nelisp-eln-chain-unwind-g")))
         (nelisp-eln-chain-unwind--g-behavior behavior)
         (nelisp-eln-chain-unwind--chain-calls 0)
         (nelisp-eln-chain-unwind--encoded-words nil)
         (nelisp-eln-chain-unwind--x x)
         (nelisp-eln-callable-import--frames nil)
         (real-encode (symbol-function 'nelisp-eln-objects-encode))
         (real-release (symbol-function 'nelisp-eln-objects-activation-release))
         (real-ptr-read-u64 (symbol-function 'ptr-read-u64))
         (real-ptr-write-u64 (symbol-function 'ptr-write-u64))
         (real-ptr-call (symbol-function 'ptr-call))
         (fake-status 0))
    (cl-letf (((symbol-function 'nelisp-eln-system-loader-validate-function-capability)
                (lambda (cap) cap))
               ((symbol-function 'nelisp--native-env) (lambda () 'env))
               ((symbol-function 'nelisp-native-load--symbol-addr)
                (lambda (name)
                  (cond ((equal name "nelisp_eln_callback_context_push") 9001)
                        ((equal name "wf_bytecode_call_gateway") 9002)
                        ((equal name "nl_eln_callback7_context") 9003)
                        ((equal name "nelisp_eln_callback_context_pop") 9004)
                        (t 0))))
               ((symbol-function 'nelisp-native-load--pin-begin) (lambda (_env) 'marker))
               ((symbol-function 'nelisp-native-load--pin-reserve) (lambda (_env _m) 9100))
               ((symbol-function 'nelisp-native-load-box) (lambda (_slot _fn) t))
               ((symbol-function 'nelisp-native-load--pin-end) (lambda (_env _m) t))
               ((symbol-function 'nelisp-eln-raw-call-context-create) (lambda () 'ctx))
               ((symbol-function 'nelisp-eln-raw-call-context-release) (lambda (_c) t))
               ;; Selective: only the simulated callback-status word at
               ;; fake address 9003 is intercepted.  `nelisp-eln-objects'
               ;; own string/symbol/cons representations legitimately use
               ;; these same primitives against real native memory for
               ;; unrelated addresses; blanket-mocking them broke real
               ;; symbol-name string encoding with a spurious
               ;; `descriptor-changed' error.
               ((symbol-function 'ptr-read-u64)
                (lambda (addr off)
                  (if (and (= addr 9003) (= off 24)) fake-status
                    (funcall real-ptr-read-u64 addr off))))
               ((symbol-function 'ptr-write-u64)
                (lambda (addr off v)
                  (if (and (= addr 9003) (= off 24)) (setq fake-status v)
                    (funcall real-ptr-write-u64 addr off v))))
               ((symbol-function 'ptr-call)
                (lambda (address &rest args)
                  (cond ((= address 9001) 1)
                        ((= address 9004) 1)
                        (t (apply real-ptr-call address args)))))
               ((symbol-function 'nelisp-eln-objects-encode)
                (lambda (unit value)
                  ;; Do NOT `fset' G here.  `--call-chain' encodes G and X
                  ;; as two SEPARATE `nelisp-eln-objects-encode' calls on
                  ;; the SAME unit, and `nelisp-eln-objects-encode's own
                  ;; preflight re-validates EVERY root ever encoded on
                  ;; that unit (not just the value passed to THIS call) on
                  ;; every single call.  fsetting G immediately after its
                  ;; own encode call made the VERY NEXT encode call (for
                  ;; X) re-validate G against `--fresh-symbol-p' and find
                  ;; it no longer `fboundp' nil -- rejecting the whole
                  ;; encode with `unsupported-symbol-state', but only when
                  ;; this file is `load'-ed as part of a larger sequence
                  ;; of calls to this same spy, which is why an isolated
                  ;; `--eval' of "encode G, fset G, decode, funcall" alone
                  ;; never reproduced it.  G must stay non-`fboundp' for
                  ;; the entire encode phase; see `nelisp-eln-raw-call-word'
                  ;; below for where the `fset' actually belongs.
                  (let ((word (funcall real-encode unit value)))
                    (setq nelisp-eln-chain-unwind--encoded-words
                          (append nelisp-eln-chain-unwind--encoded-words
                                  (list word)))
                    word)))
               ((symbol-function 'nelisp-eln-objects-activation-release)
                (if skip-release (lambda (_token) t) real-release))
               ((symbol-function 'nelisp-eln-abi-read-word)
                (lambda (address offset)
                  (cond
                   ((= address nelisp-eln-chain-unwind--descriptor-addr)
                    (nth (/ offset 8)
                         (list 2 nelisp-eln-chain-unwind--argv-addr 0 0 0 0 0)))
                   ((= address nelisp-eln-chain-unwind--argv-addr)
                    (nth (/ offset 8)
                         (list (nth 0 nelisp-eln-chain-unwind--encoded-words)
                               (nelisp-eln-abi-encode-fixnum
                                (1+ nelisp-eln-chain-unwind--x)))))
                   (t (error "unexpected read-word address %S" address)))))
               ((symbol-function 'nelisp-eln-raw-call-word)
                (lambda (_context _address _words)
                  ;; All of `--call-chain's own argument encoding is
                  ;; complete by the time the outer raw call happens, so
                  ;; this is the latest safe point to `fset' G and the
                  ;; only point that matters: strictly before `--dispatch'
                  ;; decodes argv and applies `funcall' to it.
                  (fset g nelisp-eln-chain-unwind--g-behavior)
                  (setq nelisp-eln-chain-unwind--chain-calls
                        (1+ nelisp-eln-chain-unwind--chain-calls))
                  (let ((result (nelisp-eln-callable-import--dispatch
                                 nelisp-eln-chain-unwind--descriptor-addr)))
                    (+ (car result) (ash (cdr result) 32))))))
      (unwind-protect
          (let ((outcome
                 (catch 'nelisp-eln-chain-unwind-tag
                   (condition-case err
                       (list 'return
                             (nelisp-eln-callable-import--call-chain
                              '(cap nil nil 0) (list 'builtin 'funcall) (list g x)
                              'many 2))
                     (error (list 'error err))
                     (quit (list 'quit err))))))
            (append outcome (list nelisp-eln-chain-unwind--chain-calls)))
        ;; G's identity record (established by `make-uninterned-symbol'
        ;; above) is owned by, and only valid while, G-PREP-UNIT stays
        ;; open; release it now that G no longer needs to be encodable,
        ;; so this call leaves no owner/identity-record baseline delta
        ;; of its own for the assertion below to see.
        (fmakunbound g)
        (nelisp-eln-objects-release g-prep-unit)))))

;; G variants.  Each is a real two-way Lisp closure: on a normal call it
;; increments DECREMENT-CALLS (standing in for "call the genuine native
;; DECREMENT", see Commentary) and returns the decremented value.
(defvar nelisp-eln-chain-unwind--decrement-calls 0)
(defun nelisp-eln-chain-unwind--g-normal (v)
  (setq nelisp-eln-chain-unwind--decrement-calls
        (1+ nelisp-eln-chain-unwind--decrement-calls))
  (1- v))
(defun nelisp-eln-chain-unwind--g-error (_v) (signal 'error '("boom-marker")))
(defun nelisp-eln-chain-unwind--g-throw (_v)
  (throw 'nelisp-eln-chain-unwind-tag (list 'throw 'thrown-value)))
(defun nelisp-eln-chain-unwind--g-quit (_v) (signal 'quit nil))

(defun nelisp-eln-chain-unwind--assert (label got want)
  (unless (equal got want)
    (error "S4.6 unwind assertion failed for %s: got %S wanted %S" label got want)))

;; Negative control (b): confirm the assertion helper itself is not a
;; tautology -- it must reject a normal return where a signal was wanted.
(let ((caught (condition-case err
                  (progn (nelisp-eln-chain-unwind--assert
                          "self-check" '(return 2 1) '(error dummy 1))
                         nil)
                (error err))))
  (unless caught
    (error "S4.6 self-check: assertion helper accepted a normal return where an error was wanted")))
(princ "NELISP_CHAIN_STAGE=self_check_assertion_helper_rejects_wrong_outcome=PASS\n")

(let ((baseline (nelisp-eln-chain-unwind--baseline)))
  (dolist (case
           (list (list 'normal #'nelisp-eln-chain-unwind--g-normal
                       '(return 2 1))
                 (list 'error #'nelisp-eln-chain-unwind--g-error
                       '(error (error "boom-marker") 1))
                 (list 'throw #'nelisp-eln-chain-unwind--g-throw
                       '(throw thrown-value 1))
                 (list 'quit #'nelisp-eln-chain-unwind--g-quit
                       '(quit (quit) 1))))
    (let* ((label (nth 0 case)) (g (nth 1 case)) (want (nth 2 case))
           (nelisp-eln-chain-unwind--decrement-calls 0)
           (got (nelisp-eln-chain-unwind--run g 2 nil)))
      (nelisp-eln-chain-unwind--assert (symbol-name label) got want)
      (unless (equal (nelisp-eln-chain-unwind--baseline) baseline)
        (error "S4.6 owner/activation/pending baseline leaked for %s: %S vs %S"
               label (nelisp-eln-chain-unwind--baseline) baseline))
      (princ (format "NELISP_CHAIN_STAGE=outcome_%s_chain_calls_%d_decrement_calls_%d_baseline_restored=PASS\n"
                     label (nth 2 got)
                     (if (eq label 'normal)
                         nelisp-eln-chain-unwind--decrement-calls 0)))))
  ;; Negative control (c): a deliberately skipped release must be
  ;; detected by the same baseline check above, not silently pass.
  (nelisp-eln-chain-unwind--run #'nelisp-eln-chain-unwind--g-normal 2 t)
  (when (equal (nelisp-eln-chain-unwind--baseline) baseline)
    (error "S4.6 self-check: baseline comparison failed to detect a deliberately skipped release"))
  (princ "NELISP_CHAIN_STAGE=self_check_baseline_detects_skipped_release=PASS\n"))

(princ "NELISP-ELN-S46-CHAIN-UNWIND-PASS\n")

;;; nelisp-eln-chain-unwind-driver.el ends here
