;;; nelisp-cl-generic-subclass-test.el --- ERT tests for the subclass-dispatch fix  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Regression coverage for the nelisp-emacs-lib magit lane's bug report:
;; on the NeLisp standalone, `(make-instance 'transient-describe-target
;; ...)' signalled `cl-no-applicable-method' even though
;; `transient-describe-target' genuinely descends (through several
;; `defclass' levels) from `eieio-default-superclass', and even though
;; `cl--find-class' on it succeeded (so the class WAS registered).
;;
;; Root cause, reproduced this session without magit/transient: a
;; consumer that vendors real `eieio.el'/`cl-generic.el'/`cl-preloaded.el'
;; (as nelisp-emacs-lib does) makes real `cl--class-allparents' fboundp.
;; That real function unconditionally calls real Emacs's own
;; `merge-ordered-lists' (subr.el) to C3-merge each parent's ancestor
;; list -- confirmed by tracing a real 4-level `cl-defstruct' `:include'
;; chain under host Emacs, EVEN for plain single inheritance (one parent
;; at every level).  `merge-ordered-lists' does not exist anywhere in
;; dev/nelisp's own tree (`grep -rn merge-ordered-lists scripts/ lisp/
;; src/' => zero hits before this fix), so on the standalone the real
;; `cl--class-allparents' always signals `void-function merge-ordered-
;; lists', and `nelisp-cl-generic--subclass-parents' (in
;; `lisp/nelisp-cl-macros.el' / `scripts/nelisp-stdlib-prelude.el')
;; swallowed that via `ignore-errors' and returned nil -- silently
;; disabling EVERY `(subclass PARENT)' specializer's ancestry match, no
;; matter how many `defclass' levels separate the value from PARENT.
;;
;; The fix has two parts, mirrored identically in both files (per this
;; subsystem's own established convention, confirmed byte-identical this
;; session):
;;   1. A `(unless (fboundp 'merge-ordered-lists) ...)'-guarded polyfill
;;      of real Emacs's own algorithm (verbatim from
;;      `vendor/staged-emacs-lisp/subr.el') -- a no-op wherever real
;;      Emacs already provides it (host Emacs, or a consumer that also
;;      vendors plain subr.el), and the real, correct implementation on
;;      the standalone.
;;   2. `nelisp-cl-generic--subclass-parents' now tries its preferred
;;      tier (`cl--class-allparents') and FALLS THROUGH to the next tier
;;      (the manual `cl--class-parents'-only walk) when the preferred
;;      one is fboundp but its actual call comes back empty -- since a
;;      real, successful `cl--class-allparents' call can never
;;      legitimately return nil (it always conses at least the class's
;;      own name onto its result), an empty result can only mean the
;;      call itself failed and was swallowed, so falling through is
;;      always safe and never masks a genuine "no ancestors" case.
;;
;; This is a REAL host-ERT test of the actual subset (see
;; `test/nelisp-cl-generic-test.el''s own commentary for why
;; `(require 'nelisp-cl-macros)' needs the snapshot/restore dance below,
;; and why a test body must run via `(eval '(progn ...) t)' rather than
;; direct `ert-deftest' splicing -- both hazards apply identically here).

;;; Code:

(require 'ert)

;; Force real Emacs's OWN `cl-lib'/`cl-macs' to finish loading COMPLETELY
;; here, before `nelisp-cl-macros' gets a chance to override any of those
;; names -- see `test/nelisp-cl-generic-test.el' lines 60-82 for the full
;; explanation (measured there, applies identically here).
(require 'cl-macs)

(unless (fboundp 'nelisp--make-record)
  (defun nelisp--make-record (type-tag &rest slots)
    (apply #'record type-tag slots))
  (defun nelisp--record-type (obj) (aref obj 0))
  (defun nelisp--record-ref (obj index) (aref obj (1+ index)))
  (defun nelisp--record-set (obj index value) (aset obj (1+ index) value)))

;; Every name `lisp/nelisp-cl-macros.el' unconditionally installs that
;; this file's tests touch, PLUS the subclass-dispatch helpers this
;; fix's own regression test needs to swap dynamically
;; (`nelisp-cl-generic--subclass-parents', `cl--find-class',
;; `cl--class-parents', `cl--class-allparents', `eieio-class-name',
;; `merge-ordered-lists').  See `test/nelisp-cl-generic-test.el''s
;; `nelisp-cl-generic-test--names' commentary for why this dynamic-extent
;; save/swap/restore dance exists at all (real Emacs's own names would
;; otherwise leak into, or be leaked into by, sibling test files sharing
;; one `make test' batch process).
(defvar nelisp-cl-generic-subclass-test--names
  '(cl-block cl-return-from cl-return cl-loop cl-defstruct cl-mapcar
    cl-mapc cl-subseq cl-remove-if-not cl-labels cl-incf defsubst
    cl-every backquote cl-case cl-position cl-set-difference cl-gensym
    setf cl-macrolet cl-symbol-macrolet
    cl-defgeneric cl-defmethod cl-call-next-method cl-next-method-p
    nelisp-cl-generic--subclass-parents nelisp-cl-generic--subclass-match-p
    cl--find-class cl--class-parents cl--class-allparents
    eieio-class-name merge-ordered-lists))

(defun nelisp-cl-generic-subclass-test--snapshot ()
  (mapcar (lambda (sym) (cons sym (and (fboundp sym) (symbol-function sym))))
          nelisp-cl-generic-subclass-test--names))

(defun nelisp-cl-generic-subclass-test--restore (snapshot)
  (dolist (pair snapshot)
    (if (cdr pair) (fset (car pair) (cdr pair)) (fmakunbound (car pair)))))

(defvar nelisp-cl-generic-subclass-test--real-emacs-fns
  (nelisp-cl-generic-subclass-test--snapshot)
  "Real Emacs's own definitions, captured before `nelisp-cl-macros' loads.")

;; Force a GENUINE reload here too, symmetric with the guard added to the
;; sibling `test/nelisp-cl-generic-test.el' this session: this file's own
;; `(setq features (delq ...))' further below (courtesy for whichever
;; file loads AFTER this one) only ever protected the OTHER direction --
;; this file being loaded SECOND, after `test/nelisp-cl-generic-test.el'
;; already `provide'd `nelisp-cl-macros' and restored real Emacs's own
;; definitions, left THIS require a silent no-op, collapsing `--subset-
;; fns' below to real Emacs's functions.  Confirmed this session: `-l
;; test/nelisp-cl-generic-test.el -l
;; test/nelisp-cl-generic-subclass-test.el' (the reverse of `make
;; test''s own alphabetical order, which loads this file first and so
;; never hit this) failed exactly the two tests below that swap in this
;; subset's own `cl-defgeneric'/`cl-defmethod' -- both `cl-defmethod
;; ((class (subclass ...)))' calls hit real Emacs's OWN cl-generic
;; instead, which signals `(error "Unknown specializer %S" ...)' for
;; that shape without `eieio' also loaded (verified against
;; `lisp/emacs-lisp/cl-generic.el': the plain `subclass' generalizer is
;; registered by `eieio.el', not by `cl-generic.el' itself).
(setq features (delq 'nelisp-cl-macros features))
(require 'nelisp-cl-macros)

(defvar nelisp-cl-generic-subclass-test--subset-fns
  (nelisp-cl-generic-subclass-test--snapshot)
  "This subset's own definitions, captured right after `nelisp-cl-macros'
loads (and before real Emacs's are restored, immediately below).")

(nelisp-cl-generic-subclass-test--restore
 nelisp-cl-generic-subclass-test--real-emacs-fns)

;; Courtesy: this file's own name (`nelisp-cl-generic-subclass-test.el')
;; sorts alphabetically BEFORE `nelisp-cl-generic-test.el', and `make
;; test' loads every `test/*-test.el' into ONE shared batch Emacs
;; process (`sort'ed) -- so without this, THIS file would become the
;; first real `(require 'nelisp-cl-macros)' caller, and the sibling
;; file's own subsequent `require' would silently no-op (feature already
;; provided), collapsing ITS "subset-fns" snapshot to be identical to
;; its "real-emacs-fns" snapshot -- confirmed this session with a
;; throwaway two-file simulation (`ours: real==subset? nil' /
;; `sibling: real==subset? t' without this line; `nil'/`nil' with it).
;; Un-`provide'-ing leaves the feature exactly as unclaimed as if this
;; file did not exist, so whichever file becomes the next real consumer
;; (today, `nelisp-cl-generic-test.el') gets a genuine fresh load, same
;; as before this file was added.
(setq features (delq 'nelisp-cl-macros features))

;; NOTE: this file does NOT define a `nelisp-cl-generic-deftest'-style
;; dynamic-swap-and-`eval' wrapper (unlike the sibling
;; `test/nelisp-cl-generic-test.el') -- every test below that needs the
;; subset's own `cl-defgeneric'/`cl-defmethod' also needs REAL Emacs's
;; own `cl-defstruct' active at some point in the SAME test (to build a
;; genuine `cl--class'-registered ancestor chain `cl--class-allparents'
;; can walk), and a single blanket swap covering both would give
;; `cl-defstruct' calls the SUBSET's own struct registry instead,
;; defeating that (confirmed this session).  Each test below manages its
;; own, narrower swap instead -- see its own commentary.

;;; --- Test 1: the `merge-ordered-lists' polyfill matches real Emacs ------

;; Real Emacs already provides `merge-ordered-lists' on host, so the
;; `(unless (fboundp ...))'-guarded polyfill never installs itself here
;; (by design -- it is a no-op wherever real Emacs already has it).  Test
;; the polyfill's own algorithm directly, under a private name, against
;; the SAME inputs `cl--class-allparents' feeds it (lists of exactly one
;; sublist, from a plain single-inheritance chain) plus a genuine multi-
;; parent merge, cross-checked against real Emacs's own answer for the
;; identical input on this same host.
(defun nelisp-cl-generic-subclass-test--merge-ordered-lists (lists &optional error-function)
  "Private copy of the polyfill body in `lisp/nelisp-cl-macros.el' /
`scripts/nelisp-stdlib-prelude.el', kept byte-identical to those (this
test asserts that identity below) so it can be exercised directly on
host Emacs without needing to hide the real, always-present function."
  (let ((result '()))
    (setq lists (remq nil lists))
    (while (cdr (setq lists (delq nil lists)))
      (let* ((next nil) (tail lists))
        (while tail
          (let ((candidate (caar tail)) (other-lists lists))
            (while other-lists
              (if (not (memql candidate (cdr (car other-lists))))
                  (setq other-lists (cdr other-lists))
                (setq candidate nil)
                (setq other-lists nil)))
            (if (not candidate)
                (setq tail (cdr tail))
              (setq next candidate)
              (setq tail nil))))
        (unless next
          (setq next (funcall (or error-function #'caar) lists))
          (unless (assoc next lists #'eql)
            (error "Invalid candidate returned by error-function: %S" next)))
        (push next result)
        (setq lists
              (mapcar (lambda (l) (if (eql (car l) next) (cdr l) l))
                      lists))))
    (if (null result) (car lists)
      (append (nreverse result) (car lists)))))

(ert-deftest nelisp-cl-generic-subclass/merge-ordered-lists-single-chain ()
  "The exact shape `cl--class-allparents' feeds `merge-ordered-lists'
for a plain single-inheritance ancestor chain: one sublist per
recursive call, each already fully merged.  Matches real Emacs's own
answer for the same input on this host."
  (let ((input '((eieio-default-superclass record atom t))))
    (should (equal (nelisp-cl-generic-subclass-test--merge-ordered-lists input)
                   (merge-ordered-lists (copy-tree input))))
    (should (equal (nelisp-cl-generic-subclass-test--merge-ordered-lists input)
                   '(eieio-default-superclass record atom t)))))

(ert-deftest nelisp-cl-generic-subclass/merge-ordered-lists-multi-parent ()
  "A genuine multi-parent (diamond-shaped) merge, matching real Emacs's
own C3-style answer for the same input on this host."
  (let ((input '((d b a) (d c a))))
    (should (equal (nelisp-cl-generic-subclass-test--merge-ordered-lists input)
                   (merge-ordered-lists (copy-tree input))))))

;;; --- Test 2: the exact repro, with the preferred tier forced to fail ----

;; Deliberately does NOT override `cl--find-class'/`cl--class-parents'/
;; `eieio-class-name' by name: real Emacs's `cl--class-parents' is a
;; `cl-defsubst' (confirmed this session, `pp'-ing the captured
;; `nelisp-cl-generic--subclass-parents' body after `(require
;; 'nelisp-cl-macros)' shows the REAL struct-accessor logic, complete
;; with its own `cl-struct-cl--class-tags' type check, baked in verbatim
;; at that call site) -- ANY later top-level `(defun cl--class-parents
;; ...)' has ZERO effect on that already-expanded call site for the rest
;; of this process, so a mock built that way silently tests nothing.
;; `cl--class-allparents' is a plain `defun' on real Emacs (confirmed:
;; no such inlining), so it alone is safely overridable, and doing so is
;; also more faithful to the actual bug: the real function genuinely
;; exists (a consumer vendors it) but genuinely fails once
;; `merge-ordered-lists' is missing -- this test simulates exactly that
;; failure on a REAL, `cl--find-class'-registered struct chain instead of
;; a same-shaped mock, so `cl--class-parents' (unmocked, unaffected by
;; the inlining hazard either way since it is never asked to accept
;; anything but a genuine `cl--class'-family object here) does real work
;; on the fallback path.
(cl-defstruct nelisp-cl-generic-subclass-test--base)
(cl-defstruct (nelisp-cl-generic-subclass-test--mid1
               (:include nelisp-cl-generic-subclass-test--base)))
(cl-defstruct (nelisp-cl-generic-subclass-test--mid2
               (:include nelisp-cl-generic-subclass-test--mid1)))

(ert-deftest nelisp-cl-generic-subclass/falls-through-when-allparents-tier-fails ()
  "Reproduces the magit lane's bug directly: `cl--class-allparents' is
fboundp (as it is once a consumer vendors real eieio/cl-generic/
cl-preloaded) but its call always fails (standing in for the missing
`merge-ordered-lists' dependency) -- BEFORE this session's fix,
`nelisp-cl-generic--subclass-parents' accepted that swallowed-error nil
at face value and every subclass match on a real ancestor silently
failed; AFTER the fix, it falls through to the `cl--class-parents'-only
tier instead.  Also covers this task's own suspects directly:
`make-instance'-shaped calls on a base class first (populating the
per-generic dispatch cache), then on a NEW, deeper class only
`cl-defstruct'-registered (standing in for `defclass') AFTER that call,
plus a user `cl-defmethod' on an intermediate `(subclass ...)' added
after the first call already ran."
  (let ((only '(nelisp-cl-generic--subclass-parents nelisp-cl-generic--subclass-match-p
                cl--class-allparents cl-defstruct
                cl-defgeneric cl-defmethod cl-call-next-method cl-next-method-p
                cl-block cl-return-from cl-return setf))
        (saved nil))
    (setq saved
          (mapcar (lambda (sym) (cons sym (and (fboundp sym) (symbol-function sym)))) only))
    (unwind-protect
        (progn
          (dolist (sym only)
            (let ((subset-def (cdr (assq sym nelisp-cl-generic-subclass-test--subset-fns))))
              (if subset-def (fset sym subset-def) (fmakunbound sym))))
          (eval
           '(progn
              ;; Stand in for the real `cl--class-allparents' being
              ;; fboundp (a real consumer vendors it) but its call
              ;; always failing once `merge-ordered-lists' is missing.
              (defun cl--class-allparents (&rest _)
                (error "simulated: merge-ordered-lists missing"))

              (cl-defgeneric cgst-make-instance (class &rest slots))
              (cl-defmethod cgst-make-instance
                  ((class (subclass nelisp-cl-generic-subclass-test--base))
                   &rest slots)
                (list 'built-in-ctor class slots))

              ;; `cl--class-allparents' is fboundp but useless here --
              ;; confirm the ancestry walk still succeeds via fallback.
              (should (nelisp-cl-generic--subclass-match-p
                       'nelisp-cl-generic-subclass-test--mid1
                       'nelisp-cl-generic-subclass-test--base))

              ;; Call on the base class first -- populates/exercises
              ;; `cgst-make-instance''s per-generic dispatch cache.
              (should (equal (cgst-make-instance 'nelisp-cl-generic-subclass-test--base)
                             '(built-in-ctor nelisp-cl-generic-subclass-test--base nil)))))
           t)
          ;; A NEW, deeper class -- `cl-defstruct' (standing in for
          ;; `defclass') run AFTER the cache-populating call above, and
          ;; genuinely with real `cl-defstruct' (temporarily un-swapped)
          ;; so it registers as a real `cl--class', exactly like a
          ;; `defclass' issued after the first `make-instance' would.
          (dolist (sym only)
            (let ((real-def (cdr (assq sym saved))))
              (if real-def (fset sym real-def) (fmakunbound sym))))
          (cl-defstruct (nelisp-cl-generic-subclass-test--deep
                         (:include nelisp-cl-generic-subclass-test--mid2)))
          (dolist (sym only)
            (let ((subset-def (cdr (assq sym nelisp-cl-generic-subclass-test--subset-fns))))
              (if subset-def (fset sym subset-def) (fmakunbound sym))))
          (fset 'cl--class-allparents
                (lambda (&rest _) (error "simulated: merge-ordered-lists missing")))
          (eval
           '(progn
              (should (equal (cgst-make-instance 'nelisp-cl-generic-subclass-test--deep)
                             '(built-in-ctor nelisp-cl-generic-subclass-test--deep nil)))

              ;; A user `cl-defmethod' on an intermediate `(subclass
              ;; ...)', added AFTER the first `cgst-make-instance' call
              ;; already ran and cached its dispatch table.
              (cl-defmethod cgst-make-instance
                  ((class (subclass nelisp-cl-generic-subclass-test--mid1))
                   &rest slots)
                (list 'user-ctor class slots))
              ;; The more specific method now wins for the deep class,
              ;; proving the ancestry walk (not merely an exact-name
              ;; shortcut) is doing the matching.
              (should (equal (cgst-make-instance 'nelisp-cl-generic-subclass-test--deep)
                             '(user-ctor nelisp-cl-generic-subclass-test--deep nil)))
              (should (equal (cgst-make-instance 'nelisp-cl-generic-subclass-test--base)
                             '(built-in-ctor nelisp-cl-generic-subclass-test--base nil))))
           t))
      (dolist (pair saved)
        (if (cdr pair) (fset (car pair) (cdr pair)) (fmakunbound (car pair))))))

;;; --- Test 3: sanity against a REAL, unforced `cl--class-allparents' ---

;; A blanket swap of the SUBSET's own `cl-defstruct' (needed for
;; `cl-defgeneric'/`cl-defmethod'-based dispatch tests) would register
;; struct types in its own private registry, not real Emacs's `cl--class'
;; property -- a struct built that way would not be something the REAL
;; `cl--class-allparents' can walk at all, defeating this test's own
;; point.  This test instead swaps in ONLY the subclass-dispatch helpers
;; (leaving `cl-defstruct' as real Emacs's own, still active from the
;; unconditional `(require 'cl-macs)' at file-load time above), so
;; `cl-defstruct' here produces a genuine, real-Emacs-registered
;; `cl--class' chain and `cl--class-allparents' genuinely succeeds on it
;; (host Emacs has `merge-ordered-lists'), exercising branch 1 for real
;; rather than the fallback this file's other test targets.
(defvar nelisp-cl-generic-subclass-test--dispatch-only-names
  '(nelisp-cl-generic--subclass-parents nelisp-cl-generic--subclass-match-p
    cl-defgeneric cl-defmethod cl-call-next-method cl-next-method-p
    cl-block cl-return-from cl-return setf))

(ert-deftest nelisp-cl-generic-subclass/dispatches-through-real-struct-ancestry ()
  "Non-regression sanity: with a genuinely working `cl--class-allparents'
(real Emacs's own, backed by a real `cl-defstruct' `:include' chain, with
`merge-ordered-lists' genuinely present on this host) subclass dispatch
through several ancestor levels still succeeds end to end -- the fix
above must not have broken the already-working case."
  (cl-defstruct cgst-real-a)
  (cl-defstruct (cgst-real-b (:include cgst-real-a)))
  (cl-defstruct (cgst-real-c (:include cgst-real-b)))
  (cl-defstruct (cgst-real-d (:include cgst-real-c)))
  (let ((saved
         (mapcar (lambda (sym) (cons sym (and (fboundp sym) (symbol-function sym))))
                 nelisp-cl-generic-subclass-test--dispatch-only-names)))
    (unwind-protect
        (progn
          (dolist (sym nelisp-cl-generic-subclass-test--dispatch-only-names)
            (let ((subset-def (cdr (assq sym nelisp-cl-generic-subclass-test--subset-fns))))
              (if subset-def (fset sym subset-def) (fmakunbound sym))))
          (eval
           '(progn
              (cl-defgeneric cgst-real-make-instance (class &rest slots))
              (cl-defmethod cgst-real-make-instance
                  ((class (subclass cgst-real-a)) &rest slots)
                (list 'ctor class slots))
              (should (equal (cgst-real-make-instance 'cgst-real-a)
                             '(ctor cgst-real-a nil)))
              (should (equal (cgst-real-make-instance 'cgst-real-d)
                             '(ctor cgst-real-d nil))))
           t))
      (dolist (pair saved)
        (if (cdr pair) (fset (car pair) (cdr pair)) (fmakunbound (car pair)))))))

(provide 'nelisp-cl-generic-subclass-test)

;;; nelisp-cl-generic-subclass-test.el ends here
