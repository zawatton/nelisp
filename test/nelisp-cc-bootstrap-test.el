;;; nelisp-cc-bootstrap-test.el --- ERT for Phase 7.1.5 self-host bootstrap scaffold  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 7.1.5 *bootstrap-protocol smoke tests* for
;; `nelisp-cc-bootstrap'.  Doc 28 §3.5 commits +6 ERT smoke covering
;; the four-stage protocol (Doc 28 §3.5 stage0–stage3) and the
;; SHA-256 / byte-compare helpers.  Each test runs entirely on the
;; Phase 7.1.4 simulator pipeline; nothing here actually mmaps a
;; PROT_EXEC page or jumps into produced bytes.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-cc)
(require 'nelisp-cc-runtime)
(require 'nelisp-cc-bootstrap)

;; CI-smoke gating rationale:
;; Stages 1-3 + the full pipeline drive `nelisp-cc-runtime-compile-and-allocate`
;; which calls `nelisp-cc-runtime--platform-detect`, raising
;; `nelisp-cc-runtime-todo' on platforms outside the Phase 7.5 tier-1 set
;; (linux-x86_64 / linux-arm64 / macos-arm64).  CI runs include Windows
;; (`windows-nt'), where the simulator backend is not yet wired (Phase 7.5
;; landing).  Skip those tests on unsupported platforms; Stage 0 + the
;; SHA-256 / compare-bytes helpers + the failback test exercise host-only
;; code paths and stay enabled everywhere.

(defun nelisp-cc-bootstrap-test--platform-supported-p ()
  "Non-nil iff the current host is a Phase 7.5 tier-1 simulator target."
  (condition-case nil
      (progn (nelisp-cc-runtime--platform-detect) t)
    (nelisp-cc-runtime-todo nil)))

;;; (1) stage0 — host bytecode hash --------------------------------

(ert-deftest nelisp-cc-bootstrap-stage0-byte-compiles-and-hashes ()
  "Stage 0 returns a (PATH-OR-NIL . SHA-256) cons whose digest is a
canonical 64-character lowercase hex string and is deterministic
across runs (same form ⇒ same hash)."
  (let* ((form '(lambda (x) x))
         (a (nelisp-cc-bootstrap--stage0-compile-bytecode form))
         (b (nelisp-cc-bootstrap--stage0-compile-bytecode form)))
    (should (consp a))
    (should (null (car a)))                  ; scaffold path: no .elc
    (should (stringp (cdr a)))
    (should (= (length (cdr a)) 64))         ; SHA-256 hex digest
    (should (string-match-p "\\`[0-9a-f]+\\'" (cdr a)))
    ;; Determinism.
    (should (string= (cdr a) (cdr b)))
    ;; Different forms ⇒ different hashes (defensive — collisions
    ;; would still pass the digest-shape assertions).
    (let ((c (nelisp-cc-bootstrap--stage0-compile-bytecode '(lambda (y) y))))
      (should-not (string= (cdr a) (cdr c))))))

;;; (2) stage1 — native compile via simulator ---------------------

(ert-deftest nelisp-cc-bootstrap-stage1-native-compile-via-simulator ()
  "Stage 1 returns a plist with :hash (string), :final-bytes
(non-empty vector), :gc-metadata (Doc 28 §2.9 plist) and :backend.
The runtime layer's full result is preserved under :result so
downstream stages can introspect the simulator page."
  (skip-unless (nelisp-cc-bootstrap-test--platform-supported-p))
  (let* ((form '(lambda (x) x))
         (r (nelisp-cc-bootstrap--stage1-native-compile form 'x86_64)))
    (should (plist-get r :result))
    (should (stringp (plist-get r :hash)))
    (should (= (length (plist-get r :hash)) 64))
    (should (vectorp (plist-get r :final-bytes)))
    (should (> (length (plist-get r :final-bytes)) 0))
    (let ((meta (plist-get r :gc-metadata)))
      (should (eq (plist-get meta :gc-metadata-version)
                  nelisp-cc-runtime-gc-metadata-version))
      (should (consp (plist-get meta :safe-points))))
    (should (eq (plist-get r :backend) 'x86_64))))

;;; (3) stage2 — semantic diff (equal ⇒ pass) ---------------------

(ert-deftest nelisp-cc-bootstrap-stage2-semantic-diff-equal-passes ()
  "Stage 2 on two identical stage1 results passes with `:diff' nil."
  (skip-unless (nelisp-cc-bootstrap-test--platform-supported-p))
  (let* ((form '(lambda (x) x))
         (a (nelisp-cc-bootstrap--stage1-native-compile form 'x86_64))
         (b (nelisp-cc-bootstrap--stage1-native-compile form 'x86_64))
         (diff (nelisp-cc-bootstrap--stage2-semantic-diff a b)))
    (should (eq (plist-get diff :status) 'pass))
    (should (string= (plist-get diff :a2-hash)
                     (plist-get diff :a3-candidate-hash)))
    (should (null (plist-get diff :diff)))))

;;; (4) stage2 — semantic diff (unequal ⇒ fail) -------------------

(ert-deftest nelisp-cc-bootstrap-stage2-semantic-diff-unequal-fails ()
  "Stage 2 on two stage1 results with different `:final-bytes' fails
and surfaces a `:diff' plist whose `:first-mismatch' is an integer
position when the lengths are equal, or nil when one is a prefix."
  (skip-unless (nelisp-cc-bootstrap-test--platform-supported-p))
  (let* ((a (nelisp-cc-bootstrap--stage1-native-compile
             '(lambda (x) x) 'x86_64))
         ;; Forge a synthetic stage1 result with a mutated byte
         ;; vector so we exercise the failure branch deterministically.
         (forged-bytes (let ((v (copy-sequence (plist-get a :final-bytes))))
                         ;; Flip bit 0 of byte 0 to guarantee a diff.
                         (aset v 0 (logxor (aref v 0) #x01))
                         v))
         (forged (list :result (plist-get a :result)
                       :hash (nelisp-cc-bootstrap--sha256-of-bytes forged-bytes)
                       :final-bytes forged-bytes
                       :gc-metadata (plist-get a :gc-metadata)
                       :backend (plist-get a :backend)))
         (diff (nelisp-cc-bootstrap--stage2-semantic-diff a forged)))
    (should (eq (plist-get diff :status) 'fail))
    (should-not (string= (plist-get diff :a2-hash)
                         (plist-get diff :a3-candidate-hash)))
    (let ((d (plist-get diff :diff)))
      (should (plist-get d :length-a))
      (should (plist-get d :length-b))
      (should (= (plist-get d :first-mismatch) 0)))))

;;; (5) stage3 — self-recompile equal ⇒ promotion -----------------

(ert-deftest nelisp-cc-bootstrap-stage3-self-recompile-equal-promotes ()
  "Stage 3 re-running the simulator on the same form produces an
equal hash (the pipeline is deterministic in the scaffold) and
returns `:status pass' with `:authoritative-hash' = the original."
  (skip-unless (nelisp-cc-bootstrap-test--platform-supported-p))
  (let* ((form '(lambda (x) x))
         (s1 (nelisp-cc-bootstrap--stage1-native-compile form 'x86_64))
         (s3 (nelisp-cc-bootstrap--stage3-self-recompile s1 form 'x86_64)))
    (should (eq (plist-get s3 :status) 'pass))
    (should (string= (plist-get s3 :authoritative-hash)
                     (plist-get s1 :hash)))
    (should (null (plist-get s3 :diff)))))

;;; (6) full pipeline on tiny source -------------------------------

(ert-deftest nelisp-cc-bootstrap-run-full-pipeline-on-tiny-source ()
  "`nelisp-cc-bootstrap-run' on `(lambda (x) x)' threads stage0 →
stage1 → stage2 → stage3 successfully and returns the briefing's
status plist with `:status pass', `:stage done',
`:authoritative-promoted t', `:fallback-active nil', and SHA-256
hashes in :a2-hash / :a3-candidate-hash."
  (skip-unless (nelisp-cc-bootstrap-test--platform-supported-p))
  (let* ((form '(lambda (x) x))
         (status (nelisp-cc-bootstrap-run form 'x86_64
                                          (list form))))
    (should (eq (plist-get status :status) 'pass))
    (should (eq (plist-get status :stage) 'done))
    (should (eq (plist-get status :authoritative-promoted) t))
    (should (eq (plist-get status :fallback-active) nil))
    (should (eq (plist-get status :diff-result) 'pass))
    (should (stringp (plist-get status :a2-hash)))
    (should (stringp (plist-get status :a3-candidate-hash)))
    (should (= (length (plist-get status :a2-hash)) 64))
    (should (= (length (plist-get status :a3-candidate-hash)) 64))
    (should (numberp (plist-get status :elapsed-seconds)))
    (should (>= (plist-get status :elapsed-seconds) 0.0))
    (should (eq (plist-get status :backend) 'x86_64))
    (should (= (plist-get status :protocol-version)
               nelisp-cc-bootstrap-protocol-version))))

;;; (7) helper coverage — sha256 + compare-bytes ------------------

(ert-deftest nelisp-cc-bootstrap-sha256-helper-shape ()
  "`--sha256-of-bytes' accepts vector / list / unibyte-string and
agrees on the digest for matching byte content; produces a
64-character lowercase hex string.
The `str' is built via `unibyte-string' so the multibyte boundary
does not silently produce a different SHA — the helper itself
re-encodes via `encode-coding-string' if a multibyte string slips
in, but the test exercises the canonical unibyte path explicitly."
  (let* ((vec (vector #x90 #x90 #xC3))
         (lst (list  #x90 #x90 #xC3))
         (str (unibyte-string #x90 #x90 #xC3))
         (h-vec (nelisp-cc-bootstrap--sha256-of-bytes vec))
         (h-lst (nelisp-cc-bootstrap--sha256-of-bytes lst))
         (h-str (nelisp-cc-bootstrap--sha256-of-bytes str)))
    (should (= (length h-vec) 64))
    (should (string-match-p "\\`[0-9a-f]+\\'" h-vec))
    (should (string= h-vec h-lst))
    (should (string= h-vec h-str))))

(ert-deftest nelisp-cc-bootstrap-compare-bytes-detects-mismatch ()
  "`--compare-bytes' returns nil for equal stores, plist with
:first-mismatch INT for stores that differ at position INT, and
:first-mismatch nil + uneven lengths for prefix relationships."
  ;; Equal.
  (should (null (nelisp-cc-bootstrap--compare-bytes
                 (vector 1 2 3) (vector 1 2 3))))
  ;; Mid-sequence differ.
  (let ((d (nelisp-cc-bootstrap--compare-bytes
            (vector 1 2 3) (vector 1 9 3))))
    (should (= (plist-get d :first-mismatch) 1))
    (should (= (plist-get d :length-a) 3))
    (should (= (plist-get d :length-b) 3)))
  ;; Strict prefix.
  (let ((d (nelisp-cc-bootstrap--compare-bytes
            (vector 1 2) (vector 1 2 3))))
    (should (null (plist-get d :first-mismatch)))
    (should (= (plist-get d :length-a) 2))
    (should (= (plist-get d :length-b) 3))))

;;; (8) run on a stage1-failing form falls back gracefully --------

(ert-deftest nelisp-cc-bootstrap-run-stage1-error-falls-back ()
  "When `--stage1-native-compile' raises (e.g. the SSA frontend trips
on an unsupported form), `nelisp-cc-bootstrap-run' returns
`:status fail :stage stage1 :fallback-active t' rather than
propagating the error.  This locks the doc's failback contract:
A3 generation failure ⇒ A2 path retained."
  (cl-letf* (((symbol-function 'nelisp-cc-bootstrap--stage1-native-compile)
              (lambda (&rest _)
                (signal 'nelisp-cc-error '(:simulated-stage1-failure)))))
    (let ((status (nelisp-cc-bootstrap-run '(lambda (x) x) 'x86_64
                                           '((lambda (x) x)))))
      (should (eq (plist-get status :status) 'fail))
      (should (eq (plist-get status :stage) 'stage1))
      (should (eq (plist-get status :fallback-active) t))
      (should (eq (plist-get status :authoritative-promoted) nil))
      (should (eq (plist-get status :diff-result) 'fail)))))

(provide 'nelisp-cc-bootstrap-test)
;;; nelisp-cc-bootstrap-test.el ends here
