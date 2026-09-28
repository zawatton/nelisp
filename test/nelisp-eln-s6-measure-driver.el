;;; nelisp-eln-s6-measure-driver.el --- generic S6 host/VM/native measurement -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A single-function, single-corpus measurement driver for the S6 ledger
;; objective (tools/ai/eln-progress.org): for a genuine GNU .eln plus its
;; vendor Lisp source and a value corpus, run one of three phases and print
;; machine-readable lines to stdout:
;;
;;   host   -- load the .eln into host GNU Emacs 31.1 (comp-abi-hash
;;             "ba35c031") and call the function directly; this is the oracle.
;;   vm     -- evaluate the vendor source file on the NeLisp standalone VM
;;             (an ordinary `load' of the .el, no native registration) and
;;             call the resulting interpreted/byte-compiled function.
;;   native -- normal-`load' the same .eln on the NeLisp standalone VM
;;             through the admitted registration path (same steps as
;;             test/nelisp-eln-same-artifact-smoke.sh and
;;             test/nelisp-eln-increment-same-artifact-driver.el), then call
;;             the registered subr while counting how many times execution
;;             actually crosses `nelisp-eln-raw-call-word' and
;;             `nelisp-eln-callable-import--dispatch'.  Counters at zero
;;             means the artifact was not admitted into the native path even
;;             if the call happened to return a plausible value some other
;;             way; that case is treated as a failure, not a silent PASS.
;;
;; test/nelisp-eln-s6-measure.sh drives all three phases as three separate
;; process invocations (one genuine GNU Emacs, two of the NeLisp binary) and
;; compares their stdout.  This file intentionally never rebinds the
;; function's own underlying builtin(s); the only fset used is the
;; count-and-restore wrap around the two native bridge functions, which is
;; the mechanism the ledger requires to prove native execution happened.
;;
;; Some vendor functions (bytecomp.el/cconv.el's byte-compile-*/cconv--*
;; handlers) read file-local, bodyless-`defvar' specials that a bare
;; `(apply FUNCTION args)' cannot supply -- see
;; test/fixtures/s6-corpus/README.md.  For those, --wrapper (NELISP_ELN_S6_
;; WRAPPER) names a corpus file that is `load'ed, identically in all three
;; phases, right after FUNCTION's own phase load; it must define
;; `s6-corpus--FUNCTION' (FUNCTION's name, dashes and all, prefixed with
;; "s6-corpus--"), which binds the required dynamic state and calls FUNCTION
;; *by symbol* so each phase's own definition of FUNCTION runs underneath
;; it.  The corpus's argument lists are then applied to that wrapper
;; function instead of to FUNCTION directly.  Admission (native-subr /
;; registration) is still checked against FUNCTION itself.
;;
;; The functions below without a "--" in their name are pure (given their
;; arguments, they neither read environment variables nor perform process
;; I/O beyond what is explicitly passed in) and are covered directly by
;; test/nelisp-eln-s6-measure-driver-test.el.  The "--" functions glue those
;; pure helpers to the environment-variable contract used by the .sh driver
;; and are exercised only by the shell-level integration runs.

;;; Code:

(defun nelisp-eln-s6-measure-read-corpus (path)
  "Read the corpus at PATH: one sexp, a list of argument lists.
Each element of the returned list is itself a list of the arguments
for one call, e.g. ((0) (1) (\"x\")) for a unary function."
  (with-temp-buffer
    (insert-file-contents path)
    (goto-char (point-min))
    (let ((form (read (current-buffer))))
      (unless (listp form)
        (error "S6 corpus must read as a list of argument lists: %s" path))
      (dolist (entry form)
        (unless (listp entry)
          (error "S6 corpus entry is not an argument list: %S" entry)))
      form)))

(defun nelisp-eln-s6-measure-capture (fn args)
  "Call FN with ARGS, capturing either the return value or a condition.
Returns (:ok VALUE) on a normal return or (:err CONDITION) if FN
signalled an error, so a corpus entry that is meant to error is a
first-class comparable result rather than a driver failure."
  (condition-case err
      (list :ok (apply fn args))
    (error (list :err err))))

(defun nelisp-eln-s6-measure-capture-equal (a b)
  "Return non-nil if captures A and B (from `nelisp-eln-s6-measure-capture')
represent the same outcome: equal values, or equal condition data."
  (equal a b))

(defun nelisp-eln-s6-measure-ns-per-call (seconds calls)
  "Convert an elapsed SECONDS over CALLS invocations to nanoseconds/call."
  (if (<= calls 0) 0 (round (/ (* seconds 1.0e9) calls))))

(defun nelisp-eln-s6-measure-time-rounds (thunk rounds calls)
  "Run THUNK CALLS times per round, ROUNDS rounds; return the minimum
elapsed wall-clock time in seconds across all rounds.  THUNK is called
with no arguments; it is expected to perform one representative call
and discard or capture its own result."
  (let (best)
    (dotimes (_round rounds)
      (let ((start (float-time)))
        (dotimes (_call calls) (funcall thunk))
        (let ((elapsed (- (float-time) start)))
          (when (or (null best) (< elapsed best)) (setq best elapsed)))))
    best))

(defun nelisp-eln-s6-measure-short-condition (err)
  "Return a short, grep-safe rendering of condition ERR: its symbol
and, if present, the first data element when that element is itself a
short symbol or string.  Registration-rejection conditions from
nelisp-eln-registration.el carry a short reason symbol followed by
detail that can be an arbitrary raw byte string (e.g. a disassembled
function body); this helper deliberately drops everything past the
first data element so diagnostics never embed binary payloads."
  (let ((symbol (car err))
        (first (and (consp (cdr err)) (car (cdr err)))))
    (if (and first (or (symbolp first) (and (stringp first)
                                             (< (length first) 80))))
        (format "%s:%s" symbol first)
      (format "%s" symbol))))

(defun nelisp-eln-s6-measure-wrapper-symbol (function-name)
  "Return the interned corpus-wrapper symbol for FUNCTION-NAME.
FUNCTION-NAME is a string (e.g. \"byte-compile-constant\"); the
convention, documented in test/fixtures/s6-corpus/README.md, is that a
--wrapper corpus file defines `s6-corpus--FUNCTION-NAME' (dashes and
all) as the entry point the harness should apply corpus argument
lists to instead of FUNCTION-NAME itself."
  (intern (concat "s6-corpus--" function-name)))

(defun nelisp-eln-s6-measure-cycle-thunk (fn corpus)
  "Return a 0-argument thunk that calls FN over CORPUS entries in turn,
cycling back to the start once exhausted, capturing (not signalling)
any error so timing is not dominated by unwind-protect/backtrace cost
on the entries that are deliberately meant to error."
  (let ((entries (vconcat corpus))
        (len (length corpus))
        (i 0))
    (lambda ()
      (prog1 (nelisp-eln-s6-measure-capture fn (aref entries (mod i len)))
        (setq i (1+ i))))))

;;; The remainder of this file glues the pure helpers above to the
;;; environment-variable contract used by test/nelisp-eln-s6-measure.sh.
;;; It performs process I/O (loading files, printing to stdout) and is not
;;; unit-tested directly; it is exercised by the shell-level calibration and
;;; negative-control runs.

(defun nelisp-eln-s6-measure--env (name &optional default)
  (or (getenv name) default))

(defun nelisp-eln-s6-measure--env-required (name)
  (or (getenv name) (error "Missing required environment variable: %s" name)))

(defun nelisp-eln-s6-measure--print (fmt &rest args)
  (princ (apply #'format fmt args))
  (princ "\n"))

(defun nelisp-eln-s6-measure--host-check-abi ()
  (require 'comp)
  (unless (and (equal emacs-version "31.1") (equal comp-abi-hash "ba35c031"))
    (error "Host ABI does not match the pinned GNU Emacs 31.1 profile: %s/%s"
           emacs-version comp-abi-hash)))

(defun nelisp-eln-s6-measure--load-eln-host (path fn-symbol)
  (nelisp-eln-s6-measure--host-check-abi)
  (load path nil t t)
  (let ((fn (and (fboundp fn-symbol) (symbol-function fn-symbol))))
    (unless (and (subrp fn) (native-comp-function-p fn))
      (error "Host did not load a native-compiled subr for %s" fn-symbol))))

(defun nelisp-eln-s6-measure--load-source-vm (path fn-symbol)
  (load path nil t t)
  (unless (fboundp fn-symbol)
    (error "VM source load did not define %s" fn-symbol)))

(defun nelisp-eln-s6-measure--load-eln-native (path fn-symbol)
  "Normal-`load' PATH on NeLisp and return FN-SYMBOL's registered subr.
When FN-SYMBOL is already a runtime-defined function (`zerop', `caar',
`cadr', ...), the runtime itself -- its loader, object codec, nl-ffi and
callable-import bridge -- calls that very name, so replacing or unbinding
the global binding would break the machinery doing the measurement.  In
that case the load runs with `nelisp-eln-registration-isolated-namespace'
bound to a fresh namespace: the artifact goes through the identical
authenticated registration path under its genuine interned name, but the
resulting subr is published into the namespace and returned here for a
direct call, and the runtime's own binding is verified untouched.  An
unbound FN-SYMBOL is registered globally, exactly as before."
  (let* ((isolated (fboundp fn-symbol))
         (namespace (and isolated
                         (nelisp-eln-registration-make-isolated-namespace)))
         (runtime-binding (and isolated (symbol-function fn-symbol)))
         (runtime-plist (and isolated (copy-sequence (symbol-plist fn-symbol))))
         (callable nil))
    (nelisp-eln-s6-measure--print "S6_NATIVE_MODE=%s"
                                  (if isolated "isolated" "global"))
    (condition-case err
        (progn
          (let ((nelisp-eln-registration-isolated-namespace namespace))
            (load path nil t t))
          (setq callable
                (if isolated
                    (nelisp-eln-registration-isolated-function namespace
                                                               fn-symbol)
                  (and (fboundp fn-symbol) (symbol-function fn-symbol))))
          (unless (subrp callable)
            (nelisp-eln-s6-measure--print
             "S6_NATIVE_STATUS=not_admitted reason=no-native-subr-registered")
            (error "S6_NOT_ADMITTED: %s did not register as a native subr"
                   fn-symbol))
          (when (and isolated
                     (not (and (eq (symbol-function fn-symbol) runtime-binding)
                               (equal (symbol-plist fn-symbol) runtime-plist))))
            (nelisp-eln-s6-measure--print
             "S6_NATIVE_STATUS=not_admitted reason=runtime-binding-changed")
            (error "S6_NOT_ADMITTED: isolated load changed %s's runtime binding"
                   fn-symbol))
          callable)
      (error
       (unless (string-prefix-p "S6_NOT_ADMITTED" (error-message-string err))
         (nelisp-eln-s6-measure--print "S6_NATIVE_STATUS=not_admitted reason=%s"
                                       (nelisp-eln-s6-measure-short-condition err)))
       (signal (car err) (cdr err))))))

(defun nelisp-eln-s6-measure--run ()
  (let* ((phase (nelisp-eln-s6-measure--env-required "NELISP_ELN_S6_PHASE"))
         (function-name (nelisp-eln-s6-measure--env-required
                          "NELISP_ELN_S6_FUNCTION"))
         (fn-symbol (intern function-name))
         (corpus-path (nelisp-eln-s6-measure--env-required
                        "NELISP_ELN_S6_CORPUS"))
         (corpus (nelisp-eln-s6-measure-read-corpus corpus-path))
         (wrapper-path (let ((v (nelisp-eln-s6-measure--env
                                  "NELISP_ELN_S6_WRAPPER")))
                          (and v (> (length v) 0) v)))
         (rounds 5)
         (calls (string-to-number
                 (nelisp-eln-s6-measure--env "NELISP_ELN_S6_TIMING_CALLS"
                                             "5")))
         (inject-wrong
          (equal (nelisp-eln-s6-measure--env "NELISP_ELN_S6_INJECT_WRONG")
                 "1"))
         (raw-count 0) (dispatch-count 0) (raw-base nil) (dispatch-base nil)
         (native-callable nil))
    (cond
     ((equal phase "host")
      (nelisp-eln-s6-measure--load-eln-host
       (nelisp-eln-s6-measure--env-required "NELISP_ELN_S6_ELN") fn-symbol))
     ((equal phase "vm")
      (nelisp-eln-s6-measure--load-source-vm
       (nelisp-eln-s6-measure--env-required "NELISP_ELN_S6_SOURCE")
       fn-symbol))
     ((equal phase "native")
      (require 'nelisp-eln-registration)
      (setq native-callable
            (nelisp-eln-s6-measure--load-eln-native
             (nelisp-eln-s6-measure--env-required "NELISP_ELN_S6_ELN")
             fn-symbol)))
     (t (error "Unknown S6 measure phase: %S" phase)))
    (when wrapper-path
      (load wrapper-path nil t t))
    (let* ((call-symbol (if wrapper-path
                             (nelisp-eln-s6-measure-wrapper-symbol
                              function-name)
                           fn-symbol))
           (fn (cond
                ;; A --wrapper calls FUNCTION by symbol, which reaches the
                ;; runtime's own binding rather than an isolated
                ;; registration, so that combination cannot measure native
                ;; execution and is refused.
                ((and wrapper-path native-callable
                      (not (eq native-callable (symbol-function fn-symbol))))
                 (error "S6 --wrapper cannot reach an isolated registration of %s"
                        fn-symbol))
                ((and (equal phase "native") (not wrapper-path))
                 native-callable)
                (t
                 (unless (fboundp call-symbol)
                   (error "S6 wrapper did not define %s" call-symbol))
                 (symbol-function call-symbol)))))
      (nelisp-eln-s6-measure--print "S6_PHASE=%s FUNCTION=%s" phase
                                    function-name)
      (when (equal phase "native")
        (setq raw-base (symbol-function 'nelisp-eln-raw-call-word))
        (when (fboundp 'nelisp-eln-callable-import--dispatch)
          (setq dispatch-base
                (symbol-function 'nelisp-eln-callable-import--dispatch)))
        (fset 'nelisp-eln-raw-call-word
              (lambda (&rest a)
                (setq raw-count (1+ raw-count)) (apply raw-base a)))
        (when dispatch-base
          (fset 'nelisp-eln-callable-import--dispatch
                (lambda (&rest a)
                  (setq dispatch-count (1+ dispatch-count))
                  (apply dispatch-base a)))))
      (unwind-protect
          (progn
            (let ((idx 0))
              (dolist (args corpus)
                (let ((capture (nelisp-eln-s6-measure-capture fn args)))
                  (when (and inject-wrong (equal phase "host") (= idx 0))
                    (setq capture (list :ok 'S6-INJECTED-WRONG-VALUE)))
                  (nelisp-eln-s6-measure--print
                   "S6_RESULT idx=%d args=%S capture=%S" idx args capture))
                (setq idx (1+ idx))))
            (when corpus
              (let* ((seconds
                      (nelisp-eln-s6-measure-time-rounds
                       (nelisp-eln-s6-measure-cycle-thunk fn corpus)
                       rounds calls))
                     (ns (nelisp-eln-s6-measure-ns-per-call seconds calls)))
                (nelisp-eln-s6-measure--print
                 "S6_TIMING calls=%d rounds=%d min_seconds=%s ns_per_call=%d"
                 calls rounds seconds ns)))
            (when (equal phase "native")
              (nelisp-eln-s6-measure--print "S6_NATIVE_CALLS raw=%d dispatch=%d"
                                            raw-count dispatch-count)
              (when (= (+ raw-count dispatch-count) 0)
                (nelisp-eln-s6-measure--print
                 "S6_NATIVE_STATUS=not_admitted reason=zero_native_calls")
                (error "S6_NOT_ADMITTED: zero native calls observed for %s"
                       function-name)))
            (nelisp-eln-s6-measure--print
             "S6_PHASE_DONE phase=%s function=%s status=ok" phase
             function-name))
        (when (equal phase "native")
          (when raw-base (fset 'nelisp-eln-raw-call-word raw-base))
          (when dispatch-base
            (fset 'nelisp-eln-callable-import--dispatch dispatch-base)))))))

(when (getenv "NELISP_ELN_S6_PHASE")
  (nelisp-eln-s6-measure--run))

(provide 'nelisp-eln-s6-measure-driver)

;;; nelisp-eln-s6-measure-driver.el ends here
