;;; nelisp-eln-increment-same-artifact-driver.el --- GNU increment probe -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

(defconst nelisp-eln-increment-same-artifact--operation
  (let ((op (or (getenv "NELISP_ELN_GNU_OP") "increment")))
    (unless (member op '("increment" "decrement" "zerop"))
      (error "Unknown GNU operation: %S" op))
    op))
(defconst nelisp-eln-increment-same-artifact--zerop-p
  (equal nelisp-eln-increment-same-artifact--operation "zerop"))
(defconst nelisp-eln-increment-same-artifact--increment-p
  (equal nelisp-eln-increment-same-artifact--operation "increment"))
(defconst nelisp-eln-increment-same-artifact--expected-name
  (if nelisp-eln-increment-same-artifact--increment-p
      'nelisp-gnu-increment 'nelisp-gnu-decrement))
(defconst nelisp-eln-increment-same-artifact--builtin-name
  (if nelisp-eln-increment-same-artifact--increment-p '1+ '1-))
(defconst nelisp-eln-increment-same-artifact--input-fixnum
  (if nelisp-eln-increment-same-artifact--increment-p
      2305843009213693951 -2305843009213693952))
(defconst nelisp-eln-increment-same-artifact--overflow
  (if nelisp-eln-increment-same-artifact--increment-p
      2305843009213693952 -2305843009213693953))
(defconst nelisp-eln-increment-same-artifact--fixnum-result
  (if nelisp-eln-increment-same-artifact--increment-p 18 17))
(defconst nelisp-eln-increment-same-artifact--fixnum-input
  (if nelisp-eln-increment-same-artifact--increment-p 17 18))
(defconst nelisp-eln-increment-same-artifact--float-result
  (if nelisp-eln-increment-same-artifact--increment-p 1.5 0.5))
(defconst nelisp-eln-increment-same-artifact--float-input
  (if nelisp-eln-increment-same-artifact--increment-p 0.5 1.5))
(defconst nelisp-eln-increment-same-artifact--label
  (if nelisp-eln-increment-same-artifact--increment-p "INCREMENT" "DECREMENT"))

(defun nelisp-eln-increment-same-artifact--load-and-get-function ()
  (load (getenv "NELISP_ELN_GNU_INPUT") nil t t)
  (let ((fn (symbol-function
             nelisp-eln-increment-same-artifact--expected-name)))
    (unless (and (subrp fn) (functionp fn)
                 (equal (func-arity fn) '(1 . 1)))
      (error "GNU arithmetic did not register as a unary subr: %S"
             (and (fboundp nelisp-eln-increment-same-artifact--expected-name)
                  (func-arity
                   (symbol-function
                    nelisp-eln-increment-same-artifact--expected-name)))))
    fn))

(defun nelisp-eln-increment-same-artifact--capture-error (fn)
  (condition-case err
      (progn (funcall fn "invalid") nil)
    (error err)))

(defun nelisp-eln-increment-same-artifact--condition-matches-p
    (actual expected)
  (equal actual expected))

(defun nelisp-eln-increment-same-artifact--assert-condition (fn expected)
  (let ((actual (nelisp-eln-increment-same-artifact--capture-error fn)))
    (unless (nelisp-eln-increment-same-artifact--condition-matches-p
             actual expected)
      (error "GNU arithmetic wrong-type-argument mismatch: %S" actual))))

(unless (not (nelisp-eln-increment-same-artifact--condition-matches-p
              nil '(wrong-type-argument number-or-marker-p "invalid")))
  (error "GNU arithmetic condition check accepts a missing error"))

(defun nelisp-eln-increment-same-artifact--check-arity (fn)
  (dolist (args '(nil (1 2)))
    (unless (condition-case nil
                (progn (apply fn args) nil)
              (wrong-number-of-arguments t))
      (error "GNU increment accepted invalid arity: %S" args))))

(defun nelisp-eln-increment-same-artifact-host ()
  (require 'comp)
  (unless (and (equal emacs-version "31.1")
               (equal comp-abi-hash "ba35c031"))
    (error "Host ABI does not match pinned GNU Emacs 31.1 profile"))
  (let ((fn (nelisp-eln-increment-same-artifact--load-and-get-function)))
    (unless (and (= (funcall fn nelisp-eln-increment-same-artifact--fixnum-input)
                    nelisp-eln-increment-same-artifact--fixnum-result)
               (= (funcall fn nelisp-eln-increment-same-artifact--float-input)
                  nelisp-eln-increment-same-artifact--float-result))
      (error "GNU arithmetic mismatch"))
    (let ((result (funcall
                   fn nelisp-eln-increment-same-artifact--input-fixnum)))
      (let ((root (list result)))
        (garbage-collect)
        (unless (and (= result nelisp-eln-increment-same-artifact--overflow)
                     (eq (car root) result))
          (error "GNU increment overflow result changed across GC"))))
    (nelisp-eln-increment-same-artifact--assert-condition
     fn '(wrong-type-argument number-or-marker-p "invalid"))
    (nelisp-eln-increment-same-artifact--check-arity fn))
  (princ (format "GNU_%s_HOST_RESULTS=%s,%s,%s_ERROR_ARITY=PASS\n"
                 nelisp-eln-increment-same-artifact--label
                 nelisp-eln-increment-same-artifact--fixnum-result
                 nelisp-eln-increment-same-artifact--overflow
                 nelisp-eln-increment-same-artifact--float-result)))

(defun nelisp-eln-increment-same-artifact-nelisp ()
  (require 'nelisp-eln-registration)
  (let* ((path (getenv "NELISP_ELN_GNU_INPUT"))
         (_ (load path nil t t))
         (fn (symbol-function
              nelisp-eln-increment-same-artifact--expected-name))
         (raw-base (symbol-function 'nelisp-eln-raw-call-word))
         (dispatch-base
          (symbol-function 'nelisp-eln-callable-import--dispatch))
         (raw-count 0)
         (dispatch-count 0)
        (builtin-increment (symbol-function nelisp-eln-increment-same-artifact--builtin-name)))
    (unless (and (subrp fn) (functionp fn)
                 (equal (func-arity fn) '(1 . 1)))
      (error "NeLisp GNU arithmetic is not a unary subr"))
    (unwind-protect
        (progn
          (fset 'nelisp-eln-raw-call-word
                (lambda (&rest args)
                  (setq raw-count (+ raw-count 1))
                  (apply raw-base args)))
          (fset 'nelisp-eln-callable-import--dispatch
                (lambda (&rest args)
                  (setq dispatch-count (+ dispatch-count 1))
                  (apply dispatch-base args)))
          (unless (= (funcall fn nelisp-eln-increment-same-artifact--fixnum-input)
                     nelisp-eln-increment-same-artifact--fixnum-result)
            (error "arithmetic fast path returned the wrong fixnum"))
          (princ (format "NELISP_%s_STAGE=fixnum_result_%s\n"
                         nelisp-eln-increment-same-artifact--label
                         nelisp-eln-increment-same-artifact--fixnum-result))
          (let ((result (funcall
                         fn nelisp-eln-increment-same-artifact--input-fixnum)))
            (let ((root (list result)))
              (garbage-collect)
              (unless (and (= result nelisp-eln-increment-same-artifact--overflow)
                           (eq (car root) result))
                (error "increment overflow result changed across GC"))))
          (princ (format "NELISP_%s_STAGE=%s_bignum_gc_rooted\n"
                         nelisp-eln-increment-same-artifact--label
                         (if nelisp-eln-increment-same-artifact--increment-p "overflow" "underflow")))
          (unless (= (funcall fn nelisp-eln-increment-same-artifact--float-input)
                     nelisp-eln-increment-same-artifact--float-result)
            (error "arithmetic float helper returned the wrong result"))
          (princ (format "NELISP_%s_STAGE=float_result_%s\n"
                         nelisp-eln-increment-same-artifact--label
                         nelisp-eln-increment-same-artifact--float-result))
          (let ((condition
                 (nelisp-eln-increment-same-artifact--capture-error fn)))
            (princ (format "NELISP_%s_STAGE=invalid_input_error_%S\n"
                           nelisp-eln-increment-same-artifact--label
                           condition))
            (unless (nelisp-eln-increment-same-artifact--condition-matches-p
                     condition
                     '(wrong-type-argument number-or-marker-p "invalid"))
              (error "GNU increment wrong-type-argument mismatch: %S"
                     condition)))
          (unless (and (= raw-count 4) (= dispatch-count 3))
            (error "native/helper counts before arity checks: %S/%S"
                   raw-count dispatch-count))
          (princ (format "NELISP_%s_STAGE=error-and-count-check-complete_ERROR_MATCH=t\n"
                         nelisp-eln-increment-same-artifact--label))
          (nelisp-eln-increment-same-artifact--check-arity fn)
          (unless (and (= raw-count 4) (= dispatch-count 3))
            (error "wrong arity entered native/helper code: %S/%S"
                   raw-count dispatch-count))
          (princ (format "NELISP_%s_STAGE=arity-and-count-check-complete\n"
                         nelisp-eln-increment-same-artifact--label))
          (princ (format "NELISP_%s_NATIVE_CALLS_RAW=%d_HELPER_CALLBACKS=%d\n"
                         nelisp-eln-increment-same-artifact--label
                         raw-count dispatch-count))
          (princ (format "NELISP_%s_STAGE=before-fset-%s\n"
                         nelisp-eln-increment-same-artifact--label
                         (if nelisp-eln-increment-same-artifact--increment-p "1plus" "1minus")))
          (fset nelisp-eln-increment-same-artifact--builtin-name (lambda (&rest _) -99))
          (princ (format "NELISP_%s_STAGE=after-fset-%s\n"
                         nelisp-eln-increment-same-artifact--label
                         (if nelisp-eln-increment-same-artifact--increment-p "1plus" "1minus")))
          (unless (= (funcall fn nelisp-eln-increment-same-artifact--float-input)
                     nelisp-eln-increment-same-artifact--float-result)
            (error "arithmetic helper followed a later builtin rebinding"))
          (unless (and (= raw-count 5) (= dispatch-count 4))
            (error "final native/helper counts: %S/%S"
                   raw-count dispatch-count))
          (princ (format "NELISP_%s_COUNTS_RAW=%d_HELPER_CALLBACKS=%d\n"
                         nelisp-eln-increment-same-artifact--label
                         raw-count dispatch-count))
          (if nelisp-eln-increment-same-artifact--increment-p
              (princ (format
                      "NELISP_INCREMENT_RAW_ENTRIES=%d_HELPER_CALLBACKS=%d_OVERFLOW_GC=PASS_ERROR_ARITY=PASS_CAPTURED_IMPORT=PASS\n"
                      raw-count dispatch-count))
            (princ (format
                    "NELISP_DECREMENT_RAW_ENTRIES=%d_HELPER_CALLBACKS=%d_UNDERFLOW_GC=PASS_ERROR_ARITY=PASS_CAPTURED_IMPORT=PASS\n"
                    raw-count dispatch-count))))
      (fset 'nelisp-eln-raw-call-word raw-base)
      (fset 'nelisp-eln-callable-import--dispatch dispatch-base)
      (fset nelisp-eln-increment-same-artifact--builtin-name builtin-increment))))

;;; --- zerop (stage S6): non-tail MANY-convention import, not tail-JMP ---
;;
;; Genuine vendor `zerop' has no fixnum bound/overflow and no tail call
;; into a rebindable arithmetic builtin, so it skips the overflow/GC and
;; builtin-rebinding phases the increment/decrement driver above
;; exercises; see the smoke script's `zerop' case for why.

(defconst nelisp-eln-increment-same-artifact--zerop-c-name
  "F7a65726f70_zerop_0")

(defun nelisp-eln-increment-same-artifact-zerop-host ()
  "Verify genuine vendor `zerop' against real host GNU Emacs 31.1.
These are the ground-truth results the NeLisp phase below compares
its own decode-and-apply results against."
  (require 'comp)
  (unless (and (equal emacs-version "31.1")
               (equal comp-abi-hash "ba35c031"))
    (error "Host ABI does not match pinned GNU Emacs 31.1 profile"))
  (load (getenv "NELISP_ELN_GNU_INPUT") nil t t)
  (let ((fn (symbol-function 'zerop)))
    (unless (and (subrp fn) (native-comp-function-p fn)
                 (equal (func-arity fn) '(1 . 1)))
      (error "genuine zerop did not register as a native unary subr: %S" fn))
    (let ((z0 (funcall fn 0)) (z00 (funcall fn 0.0))
          (zn00 (funcall fn -0.0)) (z1 (funcall fn 1))
          (zstr (condition-case err (funcall fn "x") (error err))))
      (unless (and (eq z0 t) (eq z00 t) (eq zn00 t) (eq z1 nil)
                   (equal zstr '(wrong-type-argument number-or-marker-p "x")))
        (error "genuine zerop result mismatch: %S %S %S %S %S"
               z0 z00 zn00 z1 zstr))
      (dolist (args '(nil (1 2)))
        (unless (condition-case nil (progn (apply fn args) nil)
                  (wrong-number-of-arguments t))
          (error "genuine zerop accepted invalid arity: %S" args)))
      (princ (format "GNU_ZEROP_HOST_RESULTS=%s,%s,%s,%s_ERROR_ARITY=PASS\n"
                     z0 z00 zn00 z1)))))

(defun nelisp-eln-increment-same-artifact--zerop-case
    (eqsign activation number-word zero-word expected)
  "Decode NUMBER-WORD/ZERO-WORD through ACTIVATION as `=' would inside
the real callback, apply the real `=' (EQSIGN), and return (OUTCOME
MATCH-P ERRORED-P) against EXPECTED.  ACTIVATION must come from the
same object unit NUMBER-WORD was encoded under -- a boxed word (a
float, here) is only decodable within its own activation's lease,
unlike an immediate fixnum/nil word.  This is the same decode a
genuine native CALL through freloc slot 1320 would trigger, just fed
by this driver instead of by a live CALL instruction (see the report's
blocker on jumping into the mapped bytes directly).  When EQSIGN
signals, ERRORED-P is t: the real `--dispatch' never reaches its own
result-encode step in that case either, it just returns the zero pair
and lets the condition re-signal outside native code, so the caller
must not attempt a result encode for that case."
  (let* ((a (nelisp-eln-objects-activation-decode activation number-word))
         (b (nelisp-eln-objects-activation-decode activation zero-word))
         (errored nil)
         (outcome (condition-case err (apply eqsign (list a b))
                    (error (setq errored t) err))))
    (list outcome (equal outcome expected) errored)))

(defun nelisp-eln-increment-same-artifact-zerop-nelisp ()
  "Admit the genuine `zerop' .eln for real, then exercise the MANY
callable-import decode-and-apply path (real byte verifier, real
descriptor, real `=' builtin, real argument codec) against every host
result above.  Also attempts the real result-encode step `--dispatch'
would perform to hand a value back to native code; genuine GNU `t' has
no encoding yet in the shared `nelisp-eln-objects' codec (a symbol
outside this stage's 4 allowed files), so that specific step is
expected to fail for the three t-returning cases and is reported as
such rather than hidden."
  (require 'nelisp-eln-registration)
  (let* ((path (getenv "NELISP_ELN_GNU_INPUT"))
         (handle (nelisp-eln-system-loader-open path))
         (c-name nelisp-eln-increment-same-artifact--zerop-c-name)
         (cap (nelisp-eln-system-loader-function-capability handle c-name))
         (state (nelisp-eln-system-loader--state handle))
         (bias (plist-get state :bias))
         (size (nth 6 cap))
         (code (nelisp-eln-system-loader-read-root-function-bytes
                handle c-name 0 size))
         (vaddr (- (nth 3 cap) bias))
         (analysis (nelisp-eln-tail-code-analyze-stack-call code vaddr '(1320) 2))
         (import (car (plist-get analysis :imports)))
         (descriptor (nelisp-eln-native-subr--many-descriptor "ba35c031" 1320))
         (eqsign (symbol-function '=))
         (zero-word nil) (i 0) (offset 0) (matches 0) (encode-ok 0))
    (unless (and (equal (plist-get analysis :safe) t) import
                 (equal (plist-get import :slot) 1320))
      (error "genuine zerop bytes were not admitted by the stack-call verifier: %S"
             analysis))
    (princ (format "NELISP_ZEROP_STAGE=artifact_admitted_slot_%d\n"
                   (plist-get import :slot)))
    (unless (equal descriptor '("ba35c031" 1320 = 2))
      (error "MANY descriptor for authenticated slot 1320 is missing or wrong: %S"
             descriptor))
    (princ "NELISP_ZEROP_STAGE=descriptor_verified_eqlsign_arity2\n")
    ;; Pull the tagged-fixnum-0 literal straight out of the genuine bytes
    ;; (the verifier's own decode of the `movq $2,0x8(%rsp)' instruction)
    ;; instead of hand-typing the constant, so the case below is driven by
    ;; what the artifact actually contains.
    (while (< offset size)
      (let ((insn (nelisp-eln-tail-code--decode-stack-call code offset size vaddr)))
        (when (eq (nth 2 insn) 'store-array-imm)
          (setq zero-word (cdr (nth 3 insn))))
        (setq offset (nth 1 insn))))
    (unless (integerp zero-word)
      (error "could not recover the tagged-fixnum-0 literal from genuine bytes"))
    (princ (format "NELISP_ZEROP_STAGE=recovered_zero_word_%d\n" zero-word))
    (unless (nelisp-eln-abi-decode-fixnum zero-word)
      (error "recovered zero-word does not decode as fixnum 0: %S" zero-word))
    (dolist (case
             (list (list "zero" 0 t) (list "zero_float" 0.0 t)
                   (list "neg_zero_float" -0.0 t) (list "one" 1 nil)
                   (list "string" "x"
                         '(wrong-type-argument number-or-marker-p "x"))))
      (setq i (1+ i))
      (let* ((label (nth 0 case)) (input (nth 1 case)) (expected (nth 2 case))
             (unit (nelisp-eln-objects-create))
             (number-word (nelisp-eln-objects-encode unit input))
             (activation (nelisp-eln-objects-activation-acquire unit))
             (result (nelisp-eln-increment-same-artifact--zerop-case
                      eqsign activation number-word zero-word expected)))
        (nelisp-eln-objects-activation-release activation)
        (nelisp-eln-objects-release unit)
        (unless (nth 1 result)
          (error "zerop logic mismatch for %s: got %S wanted %S"
                 label (car result) expected))
        (setq matches (1+ matches))
        (princ (format "NELISP_ZEROP_STAGE=logic_%s_%S\n" label (car result)))
        (if (nth 2 result)
            ;; EQSIGN signalled: real `--dispatch' returns the zero pair
            ;; here without ever calling `nelisp-eln-objects-encode', and
            ;; `--call-unary' re-signals the saved condition outside
            ;; native code, so this case's result round trip is already
            ;; complete and real -- there is no encode step to attempt.
            (progn (setq encode-ok (1+ encode-ok))
                   (princ (format "NELISP_ZEROP_STAGE=encode_%s_ok_via_error_path\n"
                                  label)))
          (let* ((result-unit (nelisp-eln-objects-create))
                 (encoded
                  (condition-case err
                      (nelisp-eln-objects-encode result-unit (car result))
                    (nelisp-eln-objects-unsupported (cons 'blocked err)))))
            (nelisp-eln-objects-release result-unit)
            (if (consp encoded)
                (princ (format "NELISP_ZEROP_STAGE=encode_%s_blocked_%S\n"
                               label (cdr encoded)))
              (progn (setq encode-ok (1+ encode-ok))
                     (princ (format "NELISP_ZEROP_STAGE=encode_%s_ok\n" label))))))))
    ;; The native argument-count guard (never read past the array) is the
    ;; part of "arity" this stage owns; the outer Lisp-level 1-argument
    ;; arity belongs to `nelisp--native-subr-create', already proved by
    ;; the increment/decrement lanes above for the same tail-import
    ;; machinery family.  Use a real, small, isolated buffer (never the
    ;; module's own mapped memory) so the mismatched argc is read for
    ;; real by `nelisp-eln-abi-read-word' rather than injected via a mock.
    (let* ((guard-owner (nl-ffi-memory-allocate 56))
           (guard-address (nl-ffi-memory-address guard-owner)))
      (unwind-protect
          (progn
            (nelisp-eln-abi-write-word guard-address 0 3) ; argc=3, arity=2
            (nelisp-eln-abi-write-word guard-address 8 0)
            (let ((i 2))
              (while (< i 7)
                (nelisp-eln-abi-write-word guard-address (* i 8) 0)
                (setq i (1+ i))))
            (condition-case err
                (progn (nelisp-eln-callable-import--args guard-address 'many 2)
                       (error "argc=3 against arity=2 was wrongly accepted"))
              (nelisp-eln-callable-import-error
               (princ (format "NELISP_ZEROP_STAGE=argc_guard_rejected_%S\n" err)))))
        (nl-ffi-memory-release guard-owner)))
    (princ (format
            "NELISP_ZEROP_ADMISSION=PASS_DESCRIPTOR=PASS_LOGIC_MATCH=%d_%d_ARGC_GUARD=PASS_RESULT_ENCODE=%d_%d_REBINDING=skipped\n"
            matches 5 encode-ok matches))))

(let ((phase (getenv "NELISP_ELN_INCREMENT_PHASE")))
  (cond
   ((equal phase "negative-control")
    (nelisp-eln-increment-same-artifact--assert-condition
     (lambda (_) 'normal-return)
     '(wrong-type-argument number-or-marker-p "invalid")))
   ((and (equal phase "host") nelisp-eln-increment-same-artifact--zerop-p)
    (nelisp-eln-increment-same-artifact-zerop-host))
   ((and (equal phase "nelisp") nelisp-eln-increment-same-artifact--zerop-p)
    (nelisp-eln-increment-same-artifact-zerop-nelisp))
   ((equal phase "host")
    (nelisp-eln-increment-same-artifact-host))
   ((equal phase "nelisp")
    (nelisp-eln-increment-same-artifact-nelisp))
   (t (error "Unknown increment smoke phase: %S" phase))))

;;; nelisp-eln-increment-same-artifact-driver.el ends here
