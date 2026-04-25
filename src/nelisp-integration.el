;;; nelisp-integration.el --- Phase 7.5 cross-module wire-up helpers  -*- lexical-binding: t; -*-

;; Phase 7.5.1 (Doc 32 v2 LOCKED 2026-04-25 §3.1) — partial.  Acts as a
;; central hub for the Phase 7.5 integration pieces that bridge the
;; allocator (T16 — bare-symbol family namespace) and the gc-inner
;; layer (T17 — keyword-prefixed family namespace) plus stubs for the
;; Phase 7.5.2 cold-init coordinator and Phase 7.5.3 release-artifact
;; verifier.  Real implementations land in 7.5.2 / 7.5.3; the public
;; symbols are introduced now so downstream callers can discover them
;; and signal `nelisp-integration-todo' instead of `void-function'
;; until the body fills in.
;;
;; Scope (Phase 7.5.1 partial):
;;   - keyword family map (allocator bare ↔ gc-inner keyword) with
;;     bidirectional translation helpers + fallback for unknown
;;     family names.
;;   - cold-init coordinator stub (Phase 7.5.2 4-stage bootstrap).
;;   - release-artifact verifier stub (Phase 7.5.3 stage-d-v2.0
;;     binary checksum + symbol-presence check).
;;
;; Deferred to Phase 7.5.2:
;;   - real 4-stage cold-init orchestration (Doc 28 §3.5).
;;   - bin/anvil --strict-no-emacs runtime branch.
;;
;; Deferred to Phase 7.5.3:
;;   - stage-d-v2.0 release artifact verifier (cdylib symbol probe +
;;     SHA-256 checksum + GPG signature verification per §2.10).

;;; Code:

(require 'cl-lib)

;;;; Family symbol bridge (T21 mismatch resolution)

(defconst nelisp-integration-keyword-family-map
  '((cons-pool     . :cons-pool)
    (closure-pool  . :closure-pool)
    (string-span   . :string-span)
    (vector-span   . :vector-span)
    (large-object  . :large-object))
  "Bridge: allocator (T16) bare symbol → gc-inner (T17) keyword family.
Phase 7.5 wire-up needs both namespaces to round-trip cleanly because
the allocator emits bare symbols (`cons-pool', etc.) while gc-inner
indexes families via keyword tags (`:cons-pool', etc.).  T21 surfaced
the mismatch; this map is the single authoritative bridge that
Phase 7.5 callers use.")

(defun nelisp-integration-family-to-keyword (sym)
  "Translate bare family SYM (e.g. `cons-pool') to keyword (e.g. `:cons-pool').
Falls back to `intern' on a `:'-prefixed symbol name when SYM is not
listed in `nelisp-integration-keyword-family-map' so future families
keep working without a code edit (the canonical map should still be
extended in lockstep)."
  (or (cdr (assq sym nelisp-integration-keyword-family-map))
      (intern (concat ":" (symbol-name sym)))))

(defun nelisp-integration-family-from-keyword (kw)
  "Translate keyword family KW (e.g. `:cons-pool') back to bare symbol.
Falls back to `intern' on the keyword name with the leading `:' stripped
when KW is not listed in `nelisp-integration-keyword-family-map'.  This
is the inverse of `nelisp-integration-family-to-keyword'."
  (or (car (rassq kw nelisp-integration-keyword-family-map))
      (let ((name (symbol-name kw)))
        (intern (if (and (> (length name) 0) (eq (aref name 0) ?:))
                    (substring name 1)
                  name)))))

;;;; Phase 7.5.2 / 7.5.3 stubs

(define-error 'nelisp-integration-todo
  "Phase 7.5 integration TODO not yet implemented")

(defun nelisp-integration--cold-init-coordinator-stub ()
  "Phase 7.5.2 cold-init coordinator stub.
Reserves the public entry point for the 4-stage bootstrap
orchestration described in Doc 28 §3.5 + Doc 32 v2 §3.2.  Until the
real body lands in Phase 7.5.2 this signals `nelisp-integration-todo'
with a tag identifying the deferred work."
  (signal 'nelisp-integration-todo
          (list 'cold-init-coordinator "Phase 7.5.2")))

(defun nelisp-integration--release-artifact-verifier-stub ()
  "Phase 7.5.3 release artifact verifier stub.
Reserves the public entry point for the stage-d-v2.0 binary verifier
described in Doc 32 v2 §3.3 (cdylib symbol presence + SHA-256
checksum + optional GPG signature).  Until the real body lands in
Phase 7.5.3 this signals `nelisp-integration-todo' with a tag
identifying the deferred work."
  (signal 'nelisp-integration-todo
          (list 'release-artifact-verifier "Phase 7.5.3")))

(provide 'nelisp-integration)
;;; nelisp-integration.el ends here
