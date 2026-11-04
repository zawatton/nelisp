;;; nemacs-init-transport-test.el --- ERT for the shared init transport consumer  -*- lexical-binding: t; -*-

;;; Commentary:

;; Loader reconcile Phase 2.  `src/nemacs-init-transport.el' is written
;; in the GUI bridge's flat `setq'/`fset' dialect and normally only runs
;; against the standalone reader's syscall primitives
;; (`nelisp--syscall-stat-field' / `nelisp--syscall-path-int') or inside
;; the GUI bridge's `exec-runtime-image' replay -- neither is available
;; under plain host Emacs ERT.  This file unit-tests the algorithm shape
;; (mtime-gated re-apply, per-form marker bookkeeping, report format)
;; under host Emacs by stubbing those few standalone-only primitives, so
;; the marker/consume logic itself gets fast host-CI coverage alongside
;; the real-binary smoke test
;; (apps/nemacs-next/scripts/init-require-smoke.sh) and the GUI bridge's
;; own `nemacs-gui-file-bridge-runtime-test/standalone-user-init-lane'
;; (exercised against the real standalone reader).
;;
;; The "failed form" case below does not use an actual erroring call:
;; the real standalone reader aborts only the enclosing top-level unit
;; when a lowered form fails, so the next `begin' still runs and flushes
;; the unfinished `pending' hint as failed -- but host Emacs `load' does
;; not resume after a genuine error partway through a file, so the
;; fixture instead calls `nemacs-init--begin' for form 1 and never
;; calls the matching `nemacs-init--ok', which is the exact state the
;; real abort would leave behind for the next `begin' to flush.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nemacs-init-transport)

;; `nemacs-init-transport.el' has `lexical-binding: nil' and only ever
;; `setq's this marker state (never `defvar's it -- the image-replay
;; evaluator does not wire `defvar' at all, so the shared file cannot
;; use it).  Declaring these symbols special here (no value: they are
;; already bound by the `require' above) is scoped to this
;; `lexical-binding: t' test file only, and is required so that
;; `let'-binding them below dynamically shadows the same global cells
;; `nemacs-init-transport-consume' mutates, instead of creating an
;; invisible lexical local.
(defvar nemacs-init--pending)
(defvar nemacs-init--applied)
(defvar nemacs-init--seen)
(defvar nemacs-init--failed)
(defvar nemacs-init--files)
(defvar nemacs-init--loaded-mtime)

(defvar nemacs-init-transport-test--stat-table nil
  "Path -> mtime table backing the stubbed standalone syscalls.")

(defvar nemacs-init-transport-test--x nil)
(defvar nemacs-init-transport-test--y nil)
(defvar nemacs-init-transport-test--z nil)

(defmacro nemacs-init-transport-test--with-fresh-state (&rest body)
  "Run BODY with fresh marker state and stubbed standalone primitives."
  (declare (indent 0) (debug (body)))
  `(let ((nemacs-init--pending "")
         (nemacs-init--applied 0)
         (nemacs-init--seen 0)
         (nemacs-init--failed "")
         (nemacs-init--files nil)
         (nemacs-init--loaded-mtime "")
         (nemacs-init-transport-test--stat-table (make-hash-table :test 'equal))
         (nemacs-init-transport-test--x nil)
         (nemacs-init-transport-test--y nil)
         (nemacs-init-transport-test--z nil))
     (cl-letf (((symbol-function 'nelisp--syscall-stat-field)
                (lambda (path _field)
                  (or (gethash path nemacs-init-transport-test--stat-table) 0)))
               ((symbol-function 'nelisp--syscall-path-int)
                (lambda (_call path _mode)
                  (if (gethash path nemacs-init-transport-test--stat-table) 0 -1)))
               ((symbol-function 'rdf)
                (lambda (path)
                  (if (file-readable-p path)
                      (with-temp-buffer
                        (insert-file-contents path)
                        (buffer-string))
                    "")))
               ((symbol-function 'nl-write-file)
                (lambda (path text)
                  (with-temp-file path (insert text)))))
       ,@body)))

(defun nemacs-init-transport-test--register (path mtime)
  "Mark PATH as existing with MTIME in the stubbed stat table."
  (puthash path mtime nemacs-init-transport-test--stat-table))

(ert-deftest nemacs-init-transport-test/no-wrapper-returns-nil ()
  "No wrapper file at all -> nil, caller falls back to raw load."
  (nemacs-init-transport-test--with-fresh-state
    (should-not (nemacs-init-transport-consume
                 "/nonexistent/nemacs-init-wrapped" nil))))

(ert-deftest nemacs-init-transport-test/no-stat-primitive-returns-nil ()
  "Runtime without the stat primitive (real host Emacs default) -> nil.
Deliberately does not use `nemacs-init-transport-test--with-fresh-state':
that macro stubs `nelisp--syscall-stat-field' for the other tests, but
this test wants the genuine host-Emacs condition where it is unbound."
  (should-not (fboundp 'nelisp--syscall-stat-field))
  (let ((nemacs-init--pending "")
        (nemacs-init--applied 0)
        (nemacs-init--seen 0)
        (nemacs-init--failed "")
        (nemacs-init--files nil)
        (nemacs-init--loaded-mtime ""))
    (should-not (nemacs-init-transport-consume "/tmp/whatever-does-not-matter" nil))))

(ert-deftest nemacs-init-transport-test/fresh-load-applies-and-reports ()
  "A wrapper with one ok'd form applies once and writes the report."
  (nemacs-init-transport-test--with-fresh-state
    (let* ((dir (make-temp-file "nemacs-init-transport-test-" t))
           (wrapper (expand-file-name "nemacs-init-wrapped" dir))
           (report (concat wrapper "-report")))
      (unwind-protect
          (progn
            (with-temp-file wrapper
              (insert "(nemacs-init--begin 1 \"(setq x 1)\")\n"
                      "(progn (setq nemacs-init-transport-test--x 1)\n"
                      "(nemacs-init--ok 1))\n"))
            (with-temp-file (concat wrapper "-packages") (insert ""))
            (nemacs-init-transport-test--register wrapper 111)
            (should (nemacs-init-transport-consume wrapper report))
            (should (= 1 nemacs-init-transport-test--x))
            (should (= 1 nemacs-init--applied))
            (should (= 1 nemacs-init--seen))
            (should (file-readable-p report))
            (let ((text (with-temp-buffer
                          (insert-file-contents report)
                          (buffer-string))))
              (should (string-match-p "^mtime\t111$" text))
              (should (string-match-p "^total\t1$" text))
              (should (string-match-p "^applied\t1$" text))
              (should (string-match-p "^skipped\t0$" text))))
        (delete-directory dir t)))))

(ert-deftest nemacs-init-transport-test/failed-form-is-counted-as-skipped ()
  "A form whose `ok' marker never fires is reported failed/skipped."
  (nemacs-init-transport-test--with-fresh-state
    (let* ((dir (make-temp-file "nemacs-init-transport-test-" t))
           (wrapper (expand-file-name "nemacs-init-wrapped" dir))
           (report (concat wrapper "-report")))
      (unwind-protect
          (progn
            ;; form 1's `ok' deliberately never runs (see Commentary);
            ;; form 2's `begin' flushes it as failed, then applies cleanly
            (with-temp-file wrapper
              (insert "(nemacs-init--begin 1 \"(bad-form)\")\n"
                      "(nemacs-init--begin 2 \"(setq y 2)\")\n"
                      "(progn (setq nemacs-init-transport-test--y 2)\n"
                      "(nemacs-init--ok 2))\n"))
            (with-temp-file (concat wrapper "-packages") (insert ""))
            (nemacs-init-transport-test--register wrapper 222)
            (should (nemacs-init-transport-consume wrapper report))
            (should (= 2 nemacs-init--seen))
            (should (= 1 nemacs-init--applied))
            (should (= 2 nemacs-init-transport-test--y))
            (let ((text (with-temp-buffer
                          (insert-file-contents report)
                          (buffer-string))))
              (should (string-match-p "^total\t2$" text))
              (should (string-match-p "^applied\t1$" text))
              (should (string-match-p "^skipped\t1$" text))
              (should (string-match-p "failed\t(bad-form)" text))))
        (delete-directory dir t)))))

(ert-deftest nemacs-init-transport-test/same-mtime-skips-reapply ()
  "A second consume call with an unchanged mtime is a no-op (still t)."
  (nemacs-init-transport-test--with-fresh-state
    (let* ((dir (make-temp-file "nemacs-init-transport-test-" t))
           (wrapper (expand-file-name "nemacs-init-wrapped" dir))
           (report (concat wrapper "-report")))
      (unwind-protect
          (progn
            (with-temp-file wrapper
              (insert "(nemacs-init--begin 1 \"(setq z 1)\")\n"
                      "(progn (setq nemacs-init-transport-test--z 0)\n"
                      "(nemacs-init--ok 1))\n"))
            (with-temp-file (concat wrapper "-packages") (insert ""))
            (nemacs-init-transport-test--register wrapper 333)
            (should (nemacs-init-transport-consume wrapper report))
            (should (= 0 nemacs-init-transport-test--z))
            (setq nemacs-init-transport-test--z 99)
            ;; second call, same mtime: must not re-load (z stays 99)
            (should (nemacs-init-transport-consume wrapper report))
            (should (= 99 nemacs-init-transport-test--z)))
        (delete-directory dir t)))))

(provide 'nemacs-init-transport-test)

;;; nemacs-init-transport-test.el ends here
