;;; nelisp-buffer-unification-standalone-smoke.el --- Doc 188 P1+P2 buffer smoke -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Standalone acceptance gate for Doc 188 Phase 1 (buffer unification):
;; runs the same assertions as test/nelisp-buffer-unification-test.el's
;; `nelisp-buffer/insert-then-buffer-string-round-trips' and
;; `nelisp-buffer/goto-char-and-point-agree', plus the point-min/point-max
;; forms from Doc 188 §4.2, DIRECTLY on `target/nelisp' via the mini ERT
;; shim (`scripts/nelisp-ert-shim.el', its first real consumer).  This is
;; deliberately NOT only a host-Emacs ERT case: per Doc 188 §4.1/§1.8,
;; host Emacs already has real `insert'/`buffer-string'/`point'/`goto-
;; char', so a plain ERT run of the same forms would pass whether or not
;; this tree's own prelude wiring (scripts/nelisp-stdlib-prelude.el) is
;; correct -- it is this file's job, not the host-ERT file's, to actually
;; distinguish the fix from the bug (Doc 188 §1.3's disconnected `insert').
;;
;; Doc 188 P2 (2026-08-23) grew this file: `current-buffer'/`set-buffer'/
;; `buffer-substring'/`erase-buffer' were void-function before P2 (Doc 188
;; §1.3/§1.4, reconfirmed live in this phase's own against-the-bug probe
;; before landing the fix), exactly the same "host Emacs already has the
;; real thing" blind spot P1's Commentary above describes -- so those four
;; get the same standalone-only treatment.  The last new test loads
;; `packages/nelisp-textprop-hook/src/nelisp-textprop-hook.el' and calls
;; two of its public entry points against a real buffer -- Doc 188 §3's
;; own P2 exit criterion ("the four packages' own `(current-buffer)' call
;; sites return a real buffer object standalone for the first time").
;;
;; Run from the repository root:
;;
;;   ./target/nelisp --load test/nelisp-buffer-unification-standalone-smoke.el
;;
;; The final line is `nelisp-buffer-unification-standalone-smoke: PASS
;; (N tests)'; any failure raises an error so the process exits non-zero.

;;; Code:

(load "scripts/nelisp-ert-shim.el")

(ert-deftest nelisp-buffer/insert-then-buffer-string-round-trips ()
  (let ((b (generate-new-buffer "smoke-t1")))
    (with-current-buffer b (insert "abc"))
    (should (equal "abc" (with-current-buffer b (buffer-string))))))

(ert-deftest nelisp-buffer/goto-char-and-point-agree ()
  (let ((b (generate-new-buffer "smoke-t2")))
    (with-current-buffer b (insert "abcdef") (goto-char 3))
    (should (= 3 (with-current-buffer b (point))))))

(ert-deftest nelisp-buffer/point-min-anchor ()
  ;; Doc 188 §4.2: `point-min' was already correct (1) before this
  ;; phase; must not regress the one thing that was already right.
  (should (= 1 (with-temp-buffer (insert "ab") (point-min)))))

(ert-deftest nelisp-buffer/point-max-was-hardcoded-one ()
  ;; Doc 188 §1.3/§4.2: `point-max' was hardcoded to 1 regardless of
  ;; buffer content; must now reflect the real buffer size + 1.
  (should (= 3 (with-temp-buffer (insert "hi") (point-max)))))

(ert-deftest nelisp-buffer/kill-buffer-then-not-live ()
  ;; Doc 188 §2.2: `kill-buffer'/`buffer-live-p'/`generate-new-buffer'
  ;; now read and write one coherent representation, not two.
  (let ((b (generate-new-buffer "smoke-t3")))
    (should (buffer-live-p b))
    (kill-buffer b)
    (should-not (buffer-live-p b))))

(ert-deftest nelisp-buffer/hello-world-round-trip ()
  ;; The task's own Definition-of-Done transcript: `insert' inside
  ;; `with-temp-file', then `insert-file-contents' into a fresh
  ;; `with-temp-buffer', all sharing the one ported buffer model.
  (let ((f (make-temp-file "nelisp-buffer-unification-smoke-")))
    (unwind-protect
        (progn
          (with-temp-file f (insert "hello-world"))
          (should (equal "hello-world"
                          (with-temp-buffer
                            (insert-file-contents f)
                            (buffer-string)))))
      (ignore-errors (delete-file f)))))

;; ---- Doc 188 P2 additions (2026-08-23) --------------------------------

(ert-deftest nelisp-buffer/current-buffer-tracks-with-current-buffer ()
  ;; `current-buffer' was void-function before P2; `with-current-buffer'
  ;; (P1) dynamically rebinds `nelisp--current-buffer' -- this proves
  ;; `current-buffer' actually reads that same variable, in and back out.
  (let ((outer (current-buffer))
        (b (generate-new-buffer "smoke-p2-current")))
    (should-not (eq outer b))
    (with-current-buffer b
      (should (eq b (current-buffer))))
    (should (eq outer (current-buffer)))))

(ert-deftest nelisp-buffer/set-buffer-switches-and-persists ()
  ;; Unlike `with-current-buffer', a bare `set-buffer' call does NOT
  ;; unwind on its own -- the switch outlives the statement that made
  ;; it, matching Emacs.  Cleaned up via `unwind-protect' so this test
  ;; does not leak ambient current-buffer state into whatever runs
  ;; after it in the same process.
  (let ((outer (current-buffer))
        (a (generate-new-buffer "smoke-p2-a"))
        (b (generate-new-buffer "smoke-p2-b")))
    (unwind-protect
        (progn
          (set-buffer a)
          (should (eq a (current-buffer)))
          (set-buffer b)
          (should (eq b (current-buffer))))
      (set-buffer outer))))

(ert-deftest nelisp-buffer/with-current-buffer-restores-entry-buffer-not-inner-set-buffer ()
  ;; Emacs 30.1 probe (Doc 188 P2): `with-current-buffer' restores
  ;; whatever was current on ENTRY, even when its body reassigns the
  ;; current buffer again via `set-buffer' -- it does not "chase" the
  ;; body's own reassignment.  A first draft of this smoke's previous
  ;; test asserted the opposite and was caught by running it against
  ;; real Emacs before landing, not by inspection.
  (let ((outer (current-buffer))
        (a (generate-new-buffer "smoke-p2-wcb-a"))
        (b (generate-new-buffer "smoke-p2-wcb-b")))
    (with-current-buffer a
      (set-buffer b)
      (should (eq b (current-buffer))))
    (should (eq outer (current-buffer)))))

(ert-deftest nelisp-buffer/set-buffer-returns-the-buffer ()
  (let ((b (generate-new-buffer "smoke-p2-ret")))
    (should (eq b (set-buffer b)))))

(ert-deftest nelisp-buffer/set-buffer-signals-on-missing-name ()
  ;; Probed against Emacs 30.1: `(error "No buffer named %s")'.
  (should-error (set-buffer "nelisp-buffer-unification-does-not-exist")
                :type 'error))

(ert-deftest nelisp-buffer/set-buffer-signals-on-killed-buffer ()
  ;; Probed against Emacs 30.1: `(error "Selecting deleted buffer")' --
  ;; distinct from the missing-name case above (`get-buffer' on a
  ;; buffer OBJECT returns it unchanged even when dead, Doc 188 P1).
  (let ((b (generate-new-buffer "smoke-p2-killed")))
    (kill-buffer b)
    (should-error (set-buffer b) :type 'error)))

(ert-deftest nelisp-buffer/buffer-substring-extracts-range ()
  (should (equal "el" (with-temp-buffer (insert "hello") (buffer-substring 2 4)))))

(ert-deftest nelisp-buffer/buffer-substring-handles-reversed-args ()
  ;; Emacs 30.1 probe: START/END accepted in EITHER order.
  (should (equal "el" (with-temp-buffer (insert "hello") (buffer-substring 4 2)))))

(ert-deftest nelisp-buffer/buffer-substring-fenceposts ()
  ;; Full-buffer span and the empty (start == end) span, matching
  ;; `point-min'/`point-max' exactly -- Doc 188 DoD 1-indexing fencepost.
  (with-temp-buffer
    (insert "hi")
    (should (equal "hi" (buffer-substring (point-min) (point-max))))
    (should (equal "" (buffer-substring 1 1)))))

(ert-deftest nelisp-buffer/buffer-substring-out-of-range-signals ()
  ;; Emacs 30.1 probe: `(args-out-of-range #<buffer ...> 1 10)'.
  (should-error (with-temp-buffer (insert "hi") (buffer-substring 1 10))
                :type 'args-out-of-range))

(ert-deftest nelisp-buffer/erase-buffer-clears-and-resets-point ()
  (with-temp-buffer
    (insert "abc")
    (should (equal nil (erase-buffer)))
    (should (equal "" (buffer-string)))
    ;; Point clamps back to 1 (point-min) once the buffer is empty --
    ;; Doc 188 DoD point-clamping edge case.
    (should (= 1 (point)))
    (should (= 1 (point-max)))))

(ert-deftest nelisp-buffer/textprop-hook-attaches-to-real-buffer ()
  ;; Doc 188 §3 P2 exit criterion, verbatim: "the four packages' own
  ;; `(current-buffer)' call sites (§1.5) return a real buffer object
  ;; standalone for the first time -- a smoke loading
  ;; nelisp-textprop-hook.el after P0+P2 and calling one of its public
  ;; entry points against a real buffer, not just checking it loads."
  ;; Its own top-level `(nelisp-textprop-hook--install-cleanup)' call
  ;; needs `add-hook' (P0, already landed); `nelisp-textprop-hook-run'/
  ;; `-depth' default their BUF argument to `(current-buffer)', which
  ;; this test exercises explicitly and implicitly both.
  (load "packages/nelisp-textprop-hook/src/nelisp-textprop-hook.el")
  (unwind-protect
      (let ((b (generate-new-buffer "smoke-p2-textprop"))
            (fired nil))
        (with-current-buffer b
          (nelisp-textprop-hook-add
           'modification
           (lambda (buf beg end &rest _args) (setq fired (list buf beg end))))
          (should (eq t (nelisp-textprop-hook-run 'modification (current-buffer) 1 3)))
          (should (equal fired (list b 1 3)))
          ;; Depth counter unwound back to 0 after the run completed.
          (should (= 0 (nelisp-textprop-hook-depth 'modification))))
        (kill-buffer b))
    (when (fboundp 'nelisp-textprop-hook-clear-all)
      (nelisp-textprop-hook-clear-all))))

(let* ((result (nelisp-ert-run-all "nelisp-buffer-unification"))
       (pass (nth 0 result))
       (fail (nth 1 result)))
  ;; `tools/ai/nelisp-ai.sh gate NAME -- ...' requires this exact line to
  ;; report what the gate checked; its absence is itself a hard failure
  ;; there (see tools/ai/nelisp-ai.sh's `cmd_gate').
  (princ (format "GATE-COUNT checked=%d findings=%d\n" (+ pass fail) fail))
  (if (> fail 0)
      (error "nelisp-buffer-unification-standalone-smoke: %d failure(s), %d passed"
             fail pass)
    (when (< pass 18)
      (error "nelisp-buffer-unification-standalone-smoke: only %d tests ran (expected >= 18)"
             pass))
    (princ (format "nelisp-buffer-unification-standalone-smoke: PASS (%d tests)\n" pass))))

;;; nelisp-buffer-unification-standalone-smoke.el ends here
