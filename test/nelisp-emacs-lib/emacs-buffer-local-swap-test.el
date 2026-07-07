;;; emacs-buffer-local-swap-test.el --- ERT tests for the buffer-local swap engine  -*- lexical-binding: t; -*-

;; Doc 33 §8 item 242, Phase 1 (approved plan).  Covers the swap engine
;; added to `emacs-buffer.el' (`emacs-buffer-declare-per-buffer',
;; `emacs-buffer-switch-current-buffer', `emacs-buffer--swap-out'/-in,
;; `emacs-buffer--inherit-new-buffer') plus its choke points wired into
;; `nelisp-emacs-compat.el' (`nelisp-ec-set-buffer',
;; `nelisp-ec-with-current-buffer', `nelisp-ec-save-current-buffer',
;; `nelisp-ec-kill-buffer', `nelisp-ec-generate-new-buffer').
;;
;; This is the honest in-repo re-run of the Doc 33 §8 item 242 minimal
;; repro: creating buffer B1, `(with-current-buffer B1 (setq
;; buffer-read-only t))', then creating a brand-new, never-touched
;; buffer B2 must NOT show `(buffer-local-value 'buffer-read-only B2)'
;; => t.
;;
;; Deliberately uses synthetic per-test symbols (NOT the real
;; `buffer-read-only' / `major-mode') for the swap-engine mechanism
;; tests: under host Emacs (this file is part of `TEST_FAST_FILES',
;; which runs under host Emacs, not standalone NeLisp) those names are
;; already genuine C-level per-buffer variables tied to whatever the
;; REAL host current buffer is — which this suite never switches, since
;; `nelisp-ec-set-buffer' only ever mutates `nelisp-ec--current-buffer'
;; bookkeeping, not the host's own current buffer.  Testing against the
;; real names would either pass vacuously (host's own per-buffer
;; machinery, not this module, would be doing the separating) or leak
;; side effects into the ERT runner's own buffer.  `default-directory'
;; is the one exception: its inherit-on-create behavior is exactly
;; `emacs-buffer--inherit-new-buffer''s own special case, so that test
;; exercises the real name on purpose, restoring it via `let'.

(require 'ert)
(require 'nelisp-emacs-compat)
(require 'emacs-buffer)

(defvar emacs-buffer-local-swap-test--var nil
  "Synthetic per-buffer test symbol (declared via `emacs-buffer-declare-per-buffer').")

(defvar emacs-buffer-local-swap-test--plain nil
  "Synthetic ordinary (non-per-buffer) buffer-local test symbol.")

(defmacro emacs-buffer-local-swap-test--with-fresh-world (&rest body)
  "Run BODY with clean nelisp-ec + emacs-buffer + swap-engine state."
  (declare (indent 0) (debug (body)))
  `(let ((nelisp-ec--buffers nil)
         (nelisp-ec--current-buffer nil)
         (emacs-buffer--state (make-hash-table :test 'eq))
         (emacs-buffer--variable-buffer-local nil)
         (emacs-buffer--default-values (make-hash-table :test 'eq))
         (emacs-buffer--per-buffer-symbols nil)
         (emacs-buffer--swapped-in (make-hash-table :test 'eq))
         (default-directory default-directory)
         (emacs-buffer-local-swap-test--var nil)
         (emacs-buffer-local-swap-test--plain nil))
     (emacs-buffer-declare-per-buffer 'emacs-buffer-local-swap-test--var 'default-val)
     ,@body))

;;;; B1/B2 separation: the Doc 33 §8 item 242 minimal repro, generalized
;;;; to a synthetic per-buffer symbol.

(ert-deftest emacs-buffer-local-swap-b1-b2-separation ()
  (emacs-buffer-local-swap-test--with-fresh-world
    (let ((b1 (nelisp-ec-generate-new-buffer "b1")))
      (nelisp-ec-set-buffer b1)
      (setq emacs-buffer-local-swap-test--var 'b1val)
      (let ((b2 (nelisp-ec-generate-new-buffer "b2")))
        ;; B2 never touched -- must read back the DECLARED DEFAULT, not
        ;; B1's live value.
        (should (eq (emacs-buffer-buffer-local-value
                     'emacs-buffer-local-swap-test--var b2)
                    'default-val))
        ;; And B1 must still see its own value once we actually switch.
        (nelisp-ec-set-buffer b2)
        (should (eq (emacs-buffer-buffer-local-value
                     'emacs-buffer-local-swap-test--var b1)
                    'b1val))
        (should (eq emacs-buffer-local-swap-test--var 'default-val))))))

;;;; make-local-variable separation: an explicitly localized ordinary
;;;; symbol (not globally per-buffer) behaves the same way.

(ert-deftest emacs-buffer-local-swap-make-local-variable-separation ()
  (emacs-buffer-local-swap-test--with-fresh-world
    (let ((b1 (nelisp-ec-generate-new-buffer "b1")))
      (nelisp-ec-set-buffer b1)
      (emacs-buffer-make-local-variable 'emacs-buffer-local-swap-test--plain)
      (setq emacs-buffer-local-swap-test--plain 'b1val)
      (let ((b2 (nelisp-ec-generate-new-buffer "b2")))
        (nelisp-ec-set-buffer b2)
        (should-not (eq emacs-buffer-local-swap-test--plain 'b1val))
        (nelisp-ec-set-buffer b1)
        (should (eq emacs-buffer-local-swap-test--plain 'b1val))))))

;;;; default-directory inheritance on buffer creation
;;;; (`emacs-buffer--inherit-new-buffer', real name on purpose).

(ert-deftest emacs-buffer-local-swap-default-directory-inherit ()
  (emacs-buffer-local-swap-test--with-fresh-world
    (let ((b1 (nelisp-ec-generate-new-buffer "b1")))
      (nelisp-ec-set-buffer b1)
      (setq default-directory "/tmp/swap-test-b1/")
      (let ((b2 (nelisp-ec-generate-new-buffer "b2")))
        (should (equal (emacs-buffer-buffer-local-value 'default-directory b2)
                       "/tmp/swap-test-b1/"))))))

;;;; with-current-buffer restore: global cell restored to OLD buffer's
;;;; value after a nested with-current-buffer body exits.

(ert-deftest emacs-buffer-local-swap-with-current-buffer-restores ()
  (emacs-buffer-local-swap-test--with-fresh-world
    (let ((b1 (nelisp-ec-generate-new-buffer "b1"))
          (b2 (nelisp-ec-generate-new-buffer "b2")))
      (nelisp-ec-set-buffer b1)
      (setq emacs-buffer-local-swap-test--var 'b1val)
      (nelisp-ec-with-current-buffer b2
        (setq emacs-buffer-local-swap-test--var 'b2val))
      ;; Back in B1: the live global must show B1's value again, not B2's.
      (should (eq (nelisp-ec-current-buffer) b1))
      (should (eq emacs-buffer-local-swap-test--var 'b1val))
      (should (eq (emacs-buffer-buffer-local-value
                   'emacs-buffer-local-swap-test--var b2)
                  'b2val)))))

;;;; save-current-buffer restore, including on a non-local exit.

(ert-deftest emacs-buffer-local-swap-save-current-buffer-restores ()
  (emacs-buffer-local-swap-test--with-fresh-world
    (let ((b1 (nelisp-ec-generate-new-buffer "b1"))
          (b2 (nelisp-ec-generate-new-buffer "b2")))
      (nelisp-ec-set-buffer b1)
      (setq emacs-buffer-local-swap-test--var 'b1val)
      (ignore-errors
        (nelisp-ec-save-current-buffer
          (nelisp-ec-set-buffer b2)
          (setq emacs-buffer-local-swap-test--var 'b2val)
          (error "boom")))
      (should (eq (nelisp-ec-current-buffer) b1))
      (should (eq emacs-buffer-local-swap-test--var 'b1val)))))

;;;; kill-buffer of the current buffer routes the selection change
;;;; through the swap engine and drops the killed buffer's ext state.

(ert-deftest emacs-buffer-local-swap-kill-current-buffer ()
  (emacs-buffer-local-swap-test--with-fresh-world
    (let ((b1 (nelisp-ec-generate-new-buffer "b1")))
      (nelisp-ec-set-buffer b1)
      (setq emacs-buffer-local-swap-test--var 'b1val)
      (nelisp-ec-kill-buffer b1)
      (should (null (nelisp-ec-current-buffer))))))

;;;; with-temp-buffer (via the prefixed primitives) sees the declared
;;;; default, not a value leaked from the enclosing buffer.

(ert-deftest emacs-buffer-local-swap-temp-buffer-sees-default ()
  (emacs-buffer-local-swap-test--with-fresh-world
    (let ((b1 (nelisp-ec-generate-new-buffer "b1")))
      (nelisp-ec-set-buffer b1)
      (setq emacs-buffer-local-swap-test--var 'b1val)
      (let ((temp (nelisp-ec-generate-new-buffer " *temp*")))
        (unwind-protect
            (nelisp-ec-with-current-buffer temp
              (should (eq emacs-buffer-local-swap-test--var 'default-val)))
          (nelisp-ec-kill-buffer temp)))
      (should (eq (nelisp-ec-current-buffer) b1))
      (should (eq emacs-buffer-local-swap-test--var 'b1val)))))

;;;; make-variable-buffer-local registers the symbol for swap tracking
;;;; too (Doc 33 item 242 step 4), freezing its pre-existing value as
;;;; the default instead of leaking a live dirty value forward.

(ert-deftest emacs-buffer-local-swap-make-variable-buffer-local-tracks ()
  (emacs-buffer-local-swap-test--with-fresh-world
    (emacs-buffer-make-variable-buffer-local 'emacs-buffer-local-swap-test--plain)
    (let ((b1 (nelisp-ec-generate-new-buffer "b1")))
      (nelisp-ec-set-buffer b1)
      (setq emacs-buffer-local-swap-test--plain 'b1val)
      (let ((b2 (nelisp-ec-generate-new-buffer "b2")))
        (nelisp-ec-set-buffer b2)
        (should-not (eq emacs-buffer-local-swap-test--plain 'b1val))))))

;;; emacs-buffer-local-swap-test.el ends here
