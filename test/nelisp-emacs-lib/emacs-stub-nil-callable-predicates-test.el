;;; emacs-stub-nil-callable-predicates-test.el --- ERT for the nil-symbol parity fix  -*- lexical-binding: t; -*-

;;; Commentary:

;; T75 (2026-09).  The standalone NeLisp runtime's native `fboundp' /
;; `macrop' / `commandp' / `indirect-function' primitives all incorrectly
;; signalled `(wrong-type-argument symbolp nil)' for the literal symbol
;; `nil', even though `(symbolp nil)' is t (nil *is* a symbol) and host GNU
;; Emacs simply answers "no" for all four -- nil's function cell is void by
;; default, so all four just report "not fbound / not a macro / not a
;; command / no function" without error.
;;
;; This broke `(require 'cc-mode)' on the standalone runtime -- and with it
;; every package that pulls it in, including `json-mode' (via `js.el') and
;; `omnisharp' (via `csharp-mode.el') from the T63 real-init sweep: cc-mode's
;; own `emacs-parity-cc.el' byte-compile shim (and, independently,
;; cc-defs.el / cc-engine.el's XEmacs-compat `(unless (or COND)
;; (byte-compile (lambda ...)))' shims) call `(and (symbolp form) (fboundp
;; form))' on a value that is nil whenever COND is true, so merely deciding
;; the answer aborted the load.
;;
;; The fix lives in `emacs-stub.el' (see its "T75" comment block) as a thin
;; nil-special-casing wrapper around each of the four primitives, active
;; only on the standalone runtime (guarded on `(fboundp
;; 'nelisp--eval-source-string)', so host Emacs's own — already-correct —
;; primitives are untouched).
;;
;; These assertions therefore hold identically under host Emacs (where they
;; exercise the real C primitives directly, and always did) and under
;; standalone NeLisp (where they exercise the T75 wrapper) -- there is
;; nothing to special-case per environment.  Where host and standalone
;; deliberately keep signalling (a genuinely wrong-typed, non-nil argument,
;; e.g. a string, for `fboundp'), the assertion pins that they keep
;; signalling identically too, so this fix cannot silently widen into
;; masking real type errors.

;;; Code:

(require 'ert)

;;;; A. The exact nil input class: no error, host answer

(ert-deftest emacs-stub-nil-callable-predicates-test/fboundp-nil ()
  (should (null (fboundp nil))))

(ert-deftest emacs-stub-nil-callable-predicates-test/macrop-nil ()
  (should (null (macrop nil))))

(ert-deftest emacs-stub-nil-callable-predicates-test/commandp-nil ()
  (should (null (commandp nil))))

(ert-deftest emacs-stub-nil-callable-predicates-test/indirect-function-nil ()
  (should (null (indirect-function nil))))

;;;; B. The exact call-site pattern that broke cc-mode: `(and (symbolp X)
;;;;    (fboundp X))' / `(subrp (indirect-function X))' with X = nil must
;;;;    short-circuit cleanly, never signal, and never funcall anything.

(ert-deftest emacs-stub-nil-callable-predicates-test/byte-compile-shim-and-pattern ()
  ;; Mirrors `emacs-parity-cc--byte-compile': (cond ((and (symbolp form)
  ;; (fboundp form)) (symbol-function form)) (t form)).
  (let ((form nil))
    (should (null (cond ((and (symbolp form) (fboundp form)) (symbol-function form))
                        (t form))))))

(ert-deftest emacs-stub-nil-callable-predicates-test/native-primitives-capture-pattern ()
  ;; Mirrors `emacs-process--native-primitives': (and (fboundp sym) (fboundp
  ;; 'subrp) (fboundp 'indirect-function) (subrp (indirect-function sym)))
  ;; walked with sym = nil (an alias chain that bottoms out at nil).
  (let ((sym nil))
    (should (null (and (fboundp sym) (fboundp 'subrp) (fboundp 'indirect-function)
                       (subrp (indirect-function sym)))))))

;;;; C. Real (non-nil) symbols keep their native answer -- no regression

(ert-deftest emacs-stub-nil-callable-predicates-test/fboundp-real-symbol ()
  (should (fboundp 'car))
  (should (null (fboundp 'emacs-stub-nil-callable-predicates-test--surely-unbound-xyz))))

(ert-deftest emacs-stub-nil-callable-predicates-test/macrop-real-symbol ()
  (should (macrop 'when))
  (should (null (macrop 'car))))

(ert-deftest emacs-stub-nil-callable-predicates-test/commandp-real-symbol ()
  (should (commandp 'find-file))
  (should (null (commandp 'car))))

(ert-deftest emacs-stub-nil-callable-predicates-test/indirect-function-real-symbol ()
  (should (eq (indirect-function 'car) (symbol-function 'car))))

;;;; D. A genuinely wrong-typed, non-nil argument must still signal for
;;;;    `fboundp' (its documented contract: SYMBOL must be a symbol) -- the
;;;;    nil special case must not widen into masking real type errors.

(ert-deftest emacs-stub-nil-callable-predicates-test/fboundp-string-still-signals ()
  (should-error (fboundp "not-a-symbol") :type 'wrong-type-argument))

;;;; E. `macrop' / `indirect-function' accept a non-symbol, non-nil OBJECT
;;;;    gracefully on host Emacs (they operate on any FUNCTION-shaped value,
;;;;    not just symbols) -- pin that standalone matches, so the nil fix
;;;;    does not accidentally narrow their contract.
;;;;
;;;;    `commandp' is deliberately NOT asserted here with a plain string:
;;;;    host Emacs treats a string (or vector) argument as a raw keyboard
;;;;    macro definition, so `(commandp "not-a-symbol")' => t on host
;;;;    (verified against GNU Emacs 31.1), not nil.  Standalone's `commandp'
;;;;    does not implement that keyboard-macro-string recognition at all
;;;;    (`(commandp "x")' => nil there) -- a real, separate, pre-existing gap
;;;;    unrelated to the nil-argument class T75 fixes, out of this ticket's
;;;;    scope.

(ert-deftest emacs-stub-nil-callable-predicates-test/macrop-indirect-function-non-symbol ()
  (should (null (macrop "not-a-symbol")))
  (should (equal (indirect-function "not-a-symbol") "not-a-symbol")))

(provide 'emacs-stub-nil-callable-predicates-test)

;;; emacs-stub-nil-callable-predicates-test.el ends here
