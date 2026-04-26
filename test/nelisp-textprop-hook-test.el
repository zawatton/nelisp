;;; nelisp-textprop-hook-test.el --- ERT for text-prop + overlay hook fan-out  -*- lexical-binding: t; -*-

;; T154 / Phase 9c.5 — exercises `nelisp-textprop-hook' per Doc 41
;; §3.5.5 ERT gate (+25 minimum).  This suite ships 35 tests covering:
;;
;;   §A. contract version + KIND domain
;;   §B. add / remove / handlers / clear
;;   §C. input validation (bad-kind / bad-callback / bad-buffer)
;;   §D. run fan-out — basic, order, no-handlers, args
;;   §E. depth=1 secondary trigger suppression (per Doc 41 §2.8)
;;   §F. cross-buffer mutation NOT suppressed (Doc 41 §2.8)
;;   §G. independent counter per CATEGORY (mod ⊥ point-motion)
;;   §H. signal-safe counter unwind (Doc 41 §3.5.4 corner case)
;;   §I. inhibit-modification-hooks / inhibit-point-motion-hooks guards
;;   §J. inhibit-* let-bind restore (= dynamic binding semantics)
;;   §K. with-suppressed macro (modification + point-motion)
;;   §L. KIND-specific fan-out scenarios — modification range overlap,
;;       insert-in-front, insert-behind, before/after-change broadcast,
;;       point-entered / point-left bidirectional
;;   §M. buffer-kill cleanup of depth counter rows
;;
;; Doc 41 §3.5.5 explicit corner cases (MAJOR-B.3 reflected):
;;   - hook 内 mutation で secondary trigger 抑制       (§E.1)
;;   - hook 内別 buffer mutation で secondary trigger 発火 (§F.1)
;;   - inhibit-modification-hooks let-bind で復活        (§J.1)
;;   - signal abort 時 counter reset                    (§H.1)
;;   - point-motion-hook と modification-hook の独立 counter (§G.1)

(require 'ert)
(require 'cl-lib)
(require 'nelisp-textprop-hook)

;;; ─────────────────────────────────────────────────────────────────────
;;; Test fixtures
;;; ─────────────────────────────────────────────────────────────────────

(defmacro nelisp-textprop-hook-test--with-clean-state (&rest body)
  "Run BODY with a clean handler registry + depth counter table."
  (declare (indent 0) (debug t))
  `(with-suppressed-warnings ((obsolete inhibit-point-motion-hooks))
     (let ((inhibit-modification-hooks nil)
           (inhibit-point-motion-hooks nil))
       (unwind-protect
           (progn
             (nelisp-textprop-hook-clear-all)
             ,@body)
         (nelisp-textprop-hook-clear-all)))))

(defmacro nelisp-textprop-hook-test--with-buffer (var text &rest body)
  "Run BODY with VAR bound to a fresh buffer pre-filled with TEXT."
  (declare (indent 2) (debug t))
  `(let ((,var (generate-new-buffer "*nelisp-textprop-hook-test*")))
     (unwind-protect
         (with-current-buffer ,var
           (insert ,text)
           (goto-char (point-min))
           ,@body)
       (when (buffer-live-p ,var)
         (kill-buffer ,var)))))

;;; ─────────────────────────────────────────────────────────────────────
;;; §A. contract version + KIND domain
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-textprop-hook-contract-version-locked ()
  (should (= nelisp-textprop-hook-contract-version 1)))

(ert-deftest nelisp-textprop-hook-kinds-domain ()
  (dolist (k '(modification insert-in-front insert-behind
                            before-change after-change point-motion))
    (should (memq k nelisp-textprop-hook--kinds))))

(ert-deftest nelisp-textprop-hook-category-mapping ()
  (should (eq (nelisp-textprop-hook--category 'modification)    'modification))
  (should (eq (nelisp-textprop-hook--category 'insert-in-front) 'modification))
  (should (eq (nelisp-textprop-hook--category 'insert-behind)   'modification))
  (should (eq (nelisp-textprop-hook--category 'before-change)   'modification))
  (should (eq (nelisp-textprop-hook--category 'after-change)    'modification))
  (should (eq (nelisp-textprop-hook--category 'point-motion)    'point-motion)))

;;; ─────────────────────────────────────────────────────────────────────
;;; §B. add / remove / handlers / clear
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-textprop-hook-add-roundtrip ()
  (nelisp-textprop-hook-test--with-clean-state
    (let ((cb (lambda (&rest _) nil)))
      (should (eq (nelisp-textprop-hook-add 'modification cb) cb))
      (should (member cb (nelisp-textprop-hook-handlers 'modification))))))

(ert-deftest nelisp-textprop-hook-add-deduplicates ()
  (nelisp-textprop-hook-test--with-clean-state
    (let ((cb (lambda (&rest _) nil)))
      (nelisp-textprop-hook-add 'modification cb)
      (nelisp-textprop-hook-add 'modification cb)
      (should (= (length (nelisp-textprop-hook-handlers 'modification)) 1)))))

(ert-deftest nelisp-textprop-hook-remove-roundtrip ()
  (nelisp-textprop-hook-test--with-clean-state
    (let ((cb (lambda (&rest _) nil)))
      (nelisp-textprop-hook-add 'modification cb)
      (should (eq (nelisp-textprop-hook-remove 'modification cb) cb))
      (should-not (nelisp-textprop-hook-handlers 'modification)))))

(ert-deftest nelisp-textprop-hook-remove-missing-is-nil ()
  (nelisp-textprop-hook-test--with-clean-state
    (should-not (nelisp-textprop-hook-remove
                 'modification (lambda (&rest _) nil)))))

(ert-deftest nelisp-textprop-hook-handlers-returns-copy ()
  (nelisp-textprop-hook-test--with-clean-state
    (let ((cb (lambda (&rest _) nil)))
      (nelisp-textprop-hook-add 'modification cb)
      (let ((lst (nelisp-textprop-hook-handlers 'modification)))
        (setcar lst 'mutated)
        (should (eq (car (nelisp-textprop-hook-handlers 'modification))
                    cb))))))

(ert-deftest nelisp-textprop-hook-clear-all-drops-everything ()
  (nelisp-textprop-hook-test--with-clean-state
    (nelisp-textprop-hook-add 'modification (lambda (&rest _) nil))
    (nelisp-textprop-hook-add 'point-motion (lambda (&rest _) nil))
    (nelisp-textprop-hook-clear-all)
    (should-not (nelisp-textprop-hook-handlers 'modification))
    (should-not (nelisp-textprop-hook-handlers 'point-motion))))

;;; ─────────────────────────────────────────────────────────────────────
;;; §C. input validation
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-textprop-hook-add-rejects-bad-kind ()
  (nelisp-textprop-hook-test--with-clean-state
    (should-error (nelisp-textprop-hook-add 'no-such-kind
                                            (lambda (&rest _) nil))
                  :type 'nelisp-textprop-hook-bad-kind)))

(ert-deftest nelisp-textprop-hook-add-rejects-non-function ()
  (nelisp-textprop-hook-test--with-clean-state
    (should-error (nelisp-textprop-hook-add 'modification 42)
                  :type 'nelisp-textprop-hook-bad-callback)))

(ert-deftest nelisp-textprop-hook-run-rejects-dead-buffer ()
  (nelisp-textprop-hook-test--with-clean-state
    (let ((b (generate-new-buffer "*hook-dead*")))
      (kill-buffer b)
      (should-error (nelisp-textprop-hook-run 'modification b 1 2)
                    :type 'nelisp-textprop-hook-bad-buffer))))

;;; ─────────────────────────────────────────────────────────────────────
;;; §D. run fan-out
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-textprop-hook-run-fans-out ()
  (nelisp-textprop-hook-test--with-clean-state
    (nelisp-textprop-hook-test--with-buffer buf "abcdef"
      (let* ((called 0)
             (cb (lambda (&rest _) (cl-incf called))))
        (nelisp-textprop-hook-add 'modification cb)
        (should (eq (nelisp-textprop-hook-run 'modification buf 1 3) t))
        (should (= called 1))))))

(ert-deftest nelisp-textprop-hook-run-no-handlers-returns-nil ()
  (nelisp-textprop-hook-test--with-clean-state
    (nelisp-textprop-hook-test--with-buffer buf "abcdef"
      (should-not (nelisp-textprop-hook-run 'modification buf 1 3)))))

(ert-deftest nelisp-textprop-hook-run-preserves-registration-order ()
  (nelisp-textprop-hook-test--with-clean-state
    (nelisp-textprop-hook-test--with-buffer buf "abcdef"
      (let* ((order '())
             (cb1 (lambda (&rest _) (push 1 order)))
             (cb2 (lambda (&rest _) (push 2 order)))
             (cb3 (lambda (&rest _) (push 3 order))))
        (nelisp-textprop-hook-add 'modification cb1)
        (nelisp-textprop-hook-add 'modification cb2)
        (nelisp-textprop-hook-add 'modification cb3)
        (nelisp-textprop-hook-run 'modification buf 1 3)
        (should (equal (nreverse order) '(1 2 3)))))))

(ert-deftest nelisp-textprop-hook-run-passes-args ()
  (nelisp-textprop-hook-test--with-clean-state
    (nelisp-textprop-hook-test--with-buffer buf "abcdef"
      (let* ((seen nil)
             (cb (lambda (b beg end &rest args)
                   (setq seen (list b beg end args)))))
        (nelisp-textprop-hook-add 'modification cb)
        (nelisp-textprop-hook-run 'modification buf 2 5 'before "XYZ")
        (should (eq (nth 0 seen) buf))
        (should (= (nth 1 seen) 2))
        (should (= (nth 2 seen) 5))
        (should (equal (nth 3 seen) '(before "XYZ")))))))

;;; ─────────────────────────────────────────────────────────────────────
;;; §E. depth=1 secondary trigger suppression (Doc 41 §2.8 LOCKED v1)
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-textprop-hook-secondary-trigger-suppressed ()
  ;; Doc 41 §3.5.5 corner case: hook 内 mutation で secondary trigger 抑制.
  (nelisp-textprop-hook-test--with-clean-state
    (nelisp-textprop-hook-test--with-buffer buf "abcdef"
      (let* ((calls 0)
             (cb nil))
        (setq cb (lambda (b _beg _end &rest _)
                   (cl-incf calls)
                   ;; Secondary trigger from inside the handler must be
                   ;; suppressed (depth=1 guard).
                   (nelisp-textprop-hook-run 'modification b 1 3)))
        (nelisp-textprop-hook-add 'modification cb)
        (nelisp-textprop-hook-run 'modification buf 1 3)
        (should (= calls 1))))))

(ert-deftest nelisp-textprop-hook-suppressed-p-during-run ()
  (nelisp-textprop-hook-test--with-clean-state
    (nelisp-textprop-hook-test--with-buffer buf "abcdef"
      (let* ((seen-suppressed nil)
             (cb (lambda (b _beg _end &rest _)
                   (setq seen-suppressed
                         (nelisp-textprop-hook-suppressed-p
                          'modification b)))))
        (nelisp-textprop-hook-add 'modification cb)
        (nelisp-textprop-hook-run 'modification buf 1 3)
        (should seen-suppressed)))))

(ert-deftest nelisp-textprop-hook-depth-zero-outside-run ()
  (nelisp-textprop-hook-test--with-clean-state
    (nelisp-textprop-hook-test--with-buffer buf "abcdef"
      (should (= (nelisp-textprop-hook-depth 'modification buf) 0)))))

(ert-deftest nelisp-textprop-hook-depth-one-during-run ()
  (nelisp-textprop-hook-test--with-clean-state
    (nelisp-textprop-hook-test--with-buffer buf "abcdef"
      (let* ((seen-depth nil)
             (cb (lambda (b _beg _end &rest _)
                   (setq seen-depth
                         (nelisp-textprop-hook-depth 'modification b)))))
        (nelisp-textprop-hook-add 'modification cb)
        (nelisp-textprop-hook-run 'modification buf 1 3)
        (should (= seen-depth 1))))))

(ert-deftest nelisp-textprop-hook-depth-zero-after-run ()
  (nelisp-textprop-hook-test--with-clean-state
    (nelisp-textprop-hook-test--with-buffer buf "abcdef"
      (let ((cb (lambda (&rest _) nil)))
        (nelisp-textprop-hook-add 'modification cb)
        (nelisp-textprop-hook-run 'modification buf 1 3)
        (should (= (nelisp-textprop-hook-depth 'modification buf) 0))))))

;;; ─────────────────────────────────────────────────────────────────────
;;; §F. cross-buffer mutation NOT suppressed (Doc 41 §2.8 LOCKED v1)
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-textprop-hook-cross-buffer-not-suppressed ()
  ;; Doc 41 §3.5.5 corner case: hook 内別 buffer mutation で
  ;; secondary trigger 発火 (cross-buffer 非抑制).
  (nelisp-textprop-hook-test--with-clean-state
    (nelisp-textprop-hook-test--with-buffer buf-a "AAA"
      (nelisp-textprop-hook-test--with-buffer buf-b "BBB"
        (let* ((nested-fired nil)
               (cb-outer
                (lambda (_b _beg _end &rest _)
                  ;; Trigger fan-out on a *different* buffer — this
                  ;; must not be suppressed (per-buffer counter).
                  (setq nested-fired
                        (nelisp-textprop-hook-run
                         'modification buf-b 1 2)))))
          (nelisp-textprop-hook-add 'modification cb-outer)
          ;; Add a no-op handler so buf-b run actually exercises the
          ;; fan-out path (otherwise no-handlers returns nil).
          (nelisp-textprop-hook-add 'modification (lambda (&rest _) nil))
          (nelisp-textprop-hook-run 'modification buf-a 1 2)
          (should (eq nested-fired t)))))))

;;; ─────────────────────────────────────────────────────────────────────
;;; §G. independent counter per CATEGORY (Doc 41 §3.5.4)
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-textprop-hook-category-counters-independent ()
  ;; Doc 41 §3.5.5 corner case: point-motion-hook と modification-hook
  ;; の独立 counter — 一方 trigger 中に他方が fire.
  (nelisp-textprop-hook-test--with-clean-state
    (nelisp-textprop-hook-test--with-buffer buf "abcdef"
      (let* ((point-motion-fired nil)
             (cb-mod
              (lambda (b _beg _end &rest _)
                ;; While inside `modification' fan-out, the
                ;; `point-motion' counter is independent and must fire.
                (setq point-motion-fired
                      (nelisp-textprop-hook-run 'point-motion b 1 2))))
             (cb-pm (lambda (&rest _) nil)))
        (nelisp-textprop-hook-add 'modification cb-mod)
        (nelisp-textprop-hook-add 'point-motion cb-pm)
        (nelisp-textprop-hook-run 'modification buf 1 2)
        (should (eq point-motion-fired t))))))

;;; ─────────────────────────────────────────────────────────────────────
;;; §H. signal-safe counter unwind (Doc 41 §3.5.4)
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-textprop-hook-counter-resets-on-signal ()
  ;; Doc 41 §3.5.5 corner case: signal abort 時 counter reset
  ;; (= 後続 mutation で hook 再 fire).  The signal-safe `unwind-protect'
  ;; in `nelisp-textprop-hook-run' must restore the counter so a
  ;; subsequent un-suppressed mutation fires the handler again.
  (nelisp-textprop-hook-test--with-clean-state
    (nelisp-textprop-hook-test--with-buffer buf "abcdef"
      (let* ((calls 0)
             (cb (lambda (&rest _)
                   (cl-incf calls)
                   (error "boom"))))
        (nelisp-textprop-hook-add 'modification cb)
        ;; First run aborts mid-fan-out via signal.
        (should-error (nelisp-textprop-hook-run 'modification buf 1 3))
        ;; Counter must be back at 0 (= NOT pinned at 1).
        (should (= (nelisp-textprop-hook-depth 'modification buf) 0))
        ;; Second run fires the handler again — counter was reset.
        (should-error (nelisp-textprop-hook-run 'modification buf 1 3))
        (should (= calls 2))
        (should (= (nelisp-textprop-hook-depth 'modification buf) 0))))))

;;; ─────────────────────────────────────────────────────────────────────
;;; §I. inhibit-* global guard variables
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-textprop-hook-inhibit-modification-suppresses ()
  (nelisp-textprop-hook-test--with-clean-state
    (nelisp-textprop-hook-test--with-buffer buf "abcdef"
      (let* ((called 0)
             (cb (lambda (&rest _) (cl-incf called))))
        (nelisp-textprop-hook-add 'modification cb)
        (let ((inhibit-modification-hooks t))
          (should-not (nelisp-textprop-hook-run 'modification buf 1 3)))
        (should (= called 0))))))

(ert-deftest nelisp-textprop-hook-inhibit-point-motion-suppresses ()
  (nelisp-textprop-hook-test--with-clean-state
    (nelisp-textprop-hook-test--with-buffer buf "abcdef"
      (let* ((called 0)
             (cb (lambda (&rest _) (cl-incf called))))
        (nelisp-textprop-hook-add 'point-motion cb)
        (with-suppressed-warnings ((obsolete inhibit-point-motion-hooks))
          (let ((inhibit-point-motion-hooks t))
            (should-not (nelisp-textprop-hook-run 'point-motion buf 1 3))))
        (should (= called 0))))))

(ert-deftest nelisp-textprop-hook-inhibit-mod-does-not-suppress-pm ()
  (nelisp-textprop-hook-test--with-clean-state
    (nelisp-textprop-hook-test--with-buffer buf "abcdef"
      (let* ((called 0)
             (cb (lambda (&rest _) (cl-incf called))))
        (nelisp-textprop-hook-add 'point-motion cb)
        (let ((inhibit-modification-hooks t))
          (should (eq (nelisp-textprop-hook-run 'point-motion buf 1 3) t)))
        (should (= called 1))))))

(ert-deftest nelisp-textprop-hook-inhibit-modification-affects-broadcast ()
  ;; before-change / after-change share the modification CATEGORY.
  (nelisp-textprop-hook-test--with-clean-state
    (nelisp-textprop-hook-test--with-buffer buf "abcdef"
      (let* ((b-called 0)
             (a-called 0)
             (cb-b (lambda (&rest _) (cl-incf b-called)))
             (cb-a (lambda (&rest _) (cl-incf a-called))))
        (nelisp-textprop-hook-add 'before-change cb-b)
        (nelisp-textprop-hook-add 'after-change  cb-a)
        (let ((inhibit-modification-hooks t))
          (nelisp-textprop-hook-run 'before-change buf 1 3)
          (nelisp-textprop-hook-run 'after-change  buf 1 3))
        (should (= b-called 0))
        (should (= a-called 0))))))

;;; ─────────────────────────────────────────────────────────────────────
;;; §J. inhibit-* let-bind dynamic restore (Doc 41 §3.5.5 corner case)
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-textprop-hook-inhibit-let-bind-restores ()
  ;; Doc 41 §3.5.5 corner case: inhibit-modification-hooks let-bind で復活.
  (nelisp-textprop-hook-test--with-clean-state
    (nelisp-textprop-hook-test--with-buffer buf "abcdef"
      (let* ((called 0)
             (cb (lambda (&rest _) (cl-incf called))))
        (nelisp-textprop-hook-add 'modification cb)
        ;; outer scope: nil  → let-bind t  → inner suppressed
        ;; → return to outer → fan-out resumes
        (let ((inhibit-modification-hooks t))
          (nelisp-textprop-hook-run 'modification buf 1 3))
        (should (= called 0))
        (nelisp-textprop-hook-run 'modification buf 1 3)
        (should (= called 1))))))

;;; ─────────────────────────────────────────────────────────────────────
;;; §K. with-suppressed macro
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-textprop-hook-with-suppressed-modification ()
  (nelisp-textprop-hook-test--with-clean-state
    (nelisp-textprop-hook-test--with-buffer buf "abcdef"
      (let* ((called 0)
             (cb (lambda (&rest _) (cl-incf called))))
        (nelisp-textprop-hook-add 'modification cb)
        (nelisp-textprop-hook-with-suppressed 'modification
          (nelisp-textprop-hook-run 'modification buf 1 3))
        (should (= called 0))))))

(ert-deftest nelisp-textprop-hook-with-suppressed-point-motion ()
  (nelisp-textprop-hook-test--with-clean-state
    (nelisp-textprop-hook-test--with-buffer buf "abcdef"
      (let* ((called 0)
             (cb (lambda (&rest _) (cl-incf called))))
        (nelisp-textprop-hook-add 'point-motion cb)
        (nelisp-textprop-hook-with-suppressed 'point-motion
          (nelisp-textprop-hook-run 'point-motion buf 1 3))
        (should (= called 0))))))

(ert-deftest nelisp-textprop-hook-with-suppressed-returns-body-value ()
  (nelisp-textprop-hook-test--with-clean-state
    (should (= (nelisp-textprop-hook-with-suppressed 'modification
                 (+ 1 2 3))
               6))))

(ert-deftest nelisp-textprop-hook-with-suppressed-restores-on-exit ()
  (nelisp-textprop-hook-test--with-clean-state
    (nelisp-textprop-hook-test--with-buffer buf "abcdef"
      (let* ((called 0)
             (cb (lambda (&rest _) (cl-incf called))))
        (nelisp-textprop-hook-add 'modification cb)
        (nelisp-textprop-hook-with-suppressed 'modification
          (nelisp-textprop-hook-run 'modification buf 1 3))
        ;; outside the macro, fan-out should fire normally.
        (nelisp-textprop-hook-run 'modification buf 1 3)
        (should (= called 1))))))

(ert-deftest nelisp-textprop-hook-with-suppressed-bad-kind-signals ()
  (nelisp-textprop-hook-test--with-clean-state
    (should-error (nelisp-textprop-hook-with-suppressed 'no-such-kind
                    (ignore))
                  :type 'nelisp-textprop-hook-bad-kind)))

;;; ─────────────────────────────────────────────────────────────────────
;;; §L. KIND-specific fan-out scenarios (Doc 41 §3.5.5 listed cases)
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-textprop-hook-modification-range-overlap-fanout ()
  ;; Doc 41 §3.5.5: modification-hooks range overlap fan-out.  The
  ;; module owns the trigger contract; *which* handler decides
  ;; whether (BEG END) overlap is up to the caller.  This test
  ;; checks the BEG/END payload is delivered intact so a caller
  ;; can implement the overlap predicate.
  (nelisp-textprop-hook-test--with-clean-state
    (nelisp-textprop-hook-test--with-buffer buf "abcdef"
      (let* ((seen nil)
             (cb (lambda (_b beg end &rest _)
                   (push (cons beg end) seen))))
        (nelisp-textprop-hook-add 'modification cb)
        (nelisp-textprop-hook-run 'modification buf 2 5)
        (should (equal seen '((2 . 5))))))))

(ert-deftest nelisp-textprop-hook-insert-in-front-trigger ()
  ;; Doc 41 §3.5.5: insert-in-front-hooks start match trigger.
  (nelisp-textprop-hook-test--with-clean-state
    (nelisp-textprop-hook-test--with-buffer buf "abcdef"
      (let* ((seen nil)
             (cb (lambda (_b beg end &rest _)
                   (setq seen (cons beg end)))))
        (nelisp-textprop-hook-add 'insert-in-front cb)
        (nelisp-textprop-hook-run 'insert-in-front buf 3 3)
        (should (equal seen '(3 . 3)))))))

(ert-deftest nelisp-textprop-hook-insert-behind-trigger ()
  ;; Doc 41 §3.5.5: insert-behind-hooks end match trigger.
  (nelisp-textprop-hook-test--with-clean-state
    (nelisp-textprop-hook-test--with-buffer buf "abcdef"
      (let* ((seen nil)
             (cb (lambda (_b beg end &rest _)
                   (setq seen (cons beg end)))))
        (nelisp-textprop-hook-add 'insert-behind cb)
        (nelisp-textprop-hook-run 'insert-behind buf 7 7)
        (should (equal seen '(7 . 7)))))))

(ert-deftest nelisp-textprop-hook-point-entered-and-left-bidirectional ()
  ;; Doc 41 §3.5.5: point-entered / point-left 双方向.
  (nelisp-textprop-hook-test--with-clean-state
    (nelisp-textprop-hook-test--with-buffer buf "abcdef"
      (let* ((entered 0)
             (left    0)
             (cb-e (lambda (&rest _) (cl-incf entered)))
             (cb-l (lambda (&rest _) (cl-incf left))))
        (nelisp-textprop-hook-add 'point-motion cb-e)
        (nelisp-textprop-hook-add 'point-motion cb-l)
        (nelisp-textprop-hook-run 'point-motion buf 1 1 'enter)
        (nelisp-textprop-hook-run 'point-motion buf 1 1 'leave)
        ;; Both handlers fire on each run (= broadcast within KIND).
        (should (= entered 2))
        (should (= left    2))))))

(ert-deftest nelisp-textprop-hook-before-change-broadcast ()
  ;; Doc 41 §4.6 LOCKED v1: before-change / after-change exposed as
  ;; KIND symbols sharing the modification CATEGORY guard.
  (nelisp-textprop-hook-test--with-clean-state
    (nelisp-textprop-hook-test--with-buffer buf "abcdef"
      (let* ((called 0)
             (cb (lambda (&rest _) (cl-incf called))))
        (nelisp-textprop-hook-add 'before-change cb)
        (nelisp-textprop-hook-run 'before-change buf 1 3)
        (should (= called 1))))))

;;; ─────────────────────────────────────────────────────────────────────
;;; §M. buffer-kill cleanup of depth counter rows
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-textprop-hook-kill-buffer-drops-depth-rows ()
  (nelisp-textprop-hook-test--with-clean-state
    (let ((buf (generate-new-buffer "*hook-kill*"))
          observed)
      (let* ((cb
              (lambda (b _beg _end &rest _)
                ;; Snapshot the (BUF . CATEGORY) keys mid-fan-out.
                (let (keys)
                  (maphash
                   (lambda (k _v) (when (eq (car-safe k) b) (push k keys)))
                   nelisp-textprop-hook--depth)
                  (setq observed keys)))))
        (nelisp-textprop-hook-add 'modification cb)
        (nelisp-textprop-hook-run 'modification buf 1 2))
      ;; During the run the per-buf row existed.
      (should observed)
      ;; After the run the counter dropped back to 0 = row removed.
      (let ((row-found nil))
        (maphash (lambda (k _v) (when (eq (car-safe k) buf)
                                  (setq row-found t)))
                 nelisp-textprop-hook--depth)
        (should-not row-found))
      ;; Killing the buffer must not leave any rows behind even when
      ;; a future bug re-introduces an entry.
      (puthash (cons buf 'modification) 7 nelisp-textprop-hook--depth)
      (kill-buffer buf)
      (let ((row-found nil))
        (maphash (lambda (k _v) (when (eq (car-safe k) buf)
                                  (setq row-found t)))
                 nelisp-textprop-hook--depth)
        (should-not row-found)))))

(provide 'nelisp-textprop-hook-test)

;;; nelisp-textprop-hook-test.el ends here
