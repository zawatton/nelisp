;;; nelisp-cl-generic-test.el --- ERT tests for the Doc 185 cl-generic subset  -*- lexical-binding: t; -*-

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

;; docs/design/185-cl-generic-subset.org's `cl-defgeneric'/`cl-defmethod'
;; subset: type + eql specializers, primary methods only plus
;; `cl-call-next-method'/`cl-next-method-p', a lazily-built per-generic
;; dispatch table, and a loud macroexpansion-time `error' for every
;; unsupported form.
;;
;; This is a real host-ERT test of the actual subset, not of host Emacs's
;; own `cl-generic': `(require 'nelisp-cl-macros)' loads
;; `lisp/nelisp-cl-macros.el', whose `cl-defstruct'/`cl-defgeneric'/
;; `cl-defmethod' are plain, UNCONDITIONAL `defmacro' forms (unlike most
;; of this project's stdlib polyfills, which are `(unless (fboundp ...)
;; ...)'-gated and therefore inert under host Emacs -- see
;; docs/design/188-buffer-unification.org §1.8).  Loading this file
;; unconditionally overwrites host Emacs's own autoloaded `cl-defgeneric'/
;; `cl-defmethod' for the rest of this batch process, so every test below
;; genuinely exercises Doc 185's subset, not real Emacs's `cl-generic.el'.
;; `scripts/nelisp-stdlib-prelude.el' carries an identical copy of this
;; same block for `target/nelisp' (the standalone binary) -- see that
;; file's own "cl-generic subset (Doc 185)" section; it does not load
;; under host Emacs at all (its own bootstrap `defmacro' redefinition
;; predates this addition and conflicts with host Emacs's macro
;; expansion from the file's very first form, confirmed on the
;; UNMODIFIED tree before this change), so its coverage is the
;; standalone-binary probe cited in the tests' commentary below instead
;; of an ERT case.
;;
;; Host-Emacs CONTROLS: several tests below cite an exact transcript
;; measured this session by running the equivalent form through real,
;; unmodified Emacs 30.1's own `cl-generic' (`(require 'cl-lib)', no
;; `nelisp-cl-macros' loaded) in a throwaway batch process -- confirming
;; both that the parity-comparable cases genuinely agree with real Emacs
;; (not just internally consistent) and, for `:around', that real Emacs
;; accepts what this subset deliberately, loudly rejects.

;;; Code:

(require 'ert)

;; Force real Emacs's OWN `cl-lib'/`cl-macs' (cl-defstruct/cl-defgeneric/
;; cl-defmethod/cl-call-next-method/cl-next-method-p, all real, autoloaded
;; macros/functions) to finish loading COMPLETELY, right here, before
;; `nelisp-cl-macros' gets a chance to override any of those names.
;; Without this the override is not durable: `(require 'ert)' alone only
;; PRIMES the autoload placeholders -- a plain top-level `(symbol-function
;; 'cl-defstruct)' right after `(require 'nelisp-cl-macros)' already
;; shows this file's own interpreted macro -- but something in ERT's OWN
;; test-EXECUTION machinery (not its file-LOADING machinery: the
;; difference shows up only once a test body actually runs, never merely
;; from loading this file) forces the real `cl-macs.elc' to load AFTER
;; this file has already finished loading, and that load unconditionally
;; re-`defmacro's `cl-defstruct' back to real Emacs's own version --
;; silently.  Measured this session: the exact same `(symbol-function
;; 'cl-defstruct)' check, run from a plain top-level form, shows this
;; file's own interpreted macro; the identical check, run from inside a
;; running `ert-deftest' body, shows a byte-compiled closure carrying
;; real `cl-macs.el''s own docstring and struct-tag codegen instead.
;; Forcing the real load HERE, before this subset's own `(require
;; 'nelisp-cl-macros)', leaves nothing pending for that later mechanism
;; to trigger -- confirmed this session to make the override durable
;; through actual test execution, not merely through file loading.
(require 'cl-macs)

;; `nelisp-cl-macros''s `cl-defstruct' calls `nelisp--make-record'/
;; `nelisp--record-type'/`nelisp--record-ref', but nothing in this
;; module or its own load path defines them for host Emacs -- the one
;; existing host-compat shim, `lisp/nelisp-heap-image.el', backs them
;; with plain VECTORS instead of real Emacs `record' objects (for its
;; own heap-image serialization purposes, where nothing in that file's
;; own test suite calls `recordp' on the result -- confirmed by grep,
;; `test/nelisp-heap-image-test.el' never references `vectorp'/`recordp'/
;; `nelisp--make-record' directly).  A plain vector is never `recordp'
;; under host Emacs, so borrowing that shim here would make every struct
;; instance this file creates fail `cl-defstruct''s own generated
;; predicate AND `nelisp-cl-generic--type-match''s `(recordp val)' guard
;; -- exactly the struct-dispatch path this whole test file exists to
;; exercise.  Defining a REAL-`record'-backed version here instead (only
;; when nothing has defined it yet) keeps `recordp' host Emacs's own,
;; untouched, and gives this file's structs the same record identity
;; `cl-defstruct''s generated predicate/accessors already assume.
(unless (fboundp 'nelisp--make-record)
  (defun nelisp--make-record (type-tag &rest slots)
    (apply #'record type-tag slots))
  (defun nelisp--record-type (obj) (aref obj 0))
  (defun nelisp--record-ref (obj index) (aref obj (1+ index)))
  (defun nelisp--record-set (obj index value) (aset obj (1+ index) value)))

;; `nelisp-cl-macros' unconditionally `defmacro's/`defun's several names
;; real Emacs already owns (`cl-defstruct'/`setf'/`cl-loop'/`cl-block'/
;; `cl-return'/`cl-return-from') alongside Doc 185's own new ones
;; (`cl-defgeneric'/`cl-defmethod'/`cl-call-next-method'/`cl-next-
;; method-p').  `make test' loads every `test/*-test.el' into ONE shared
;; batch Emacs process, and leaving the FIRST group overridden for the
;; rest of that process breaks unrelated sibling files: measured this
;; session, `test/nelisp-daemon-test.el' failed to load ("Eager macro-
;; expansion failure: (error \"setf: unsupported place\"
;; nelisp-actor-status)") because `packages/nelisp-actor/src/
;; nelisp-actor.el' defines its `nelisp-actor' struct via real Emacs's
;; OWN `cl-defstruct' (it does not itself require `nelisp-cl-macros'),
;; and `nelisp-cl-macros''s own `setf' only recognises places its OWN
;; `cl-defstruct' registered in `nelisp-cl-macros--accessor-info' --
;; every struct from anywhere else in the shared process becomes an
;; "unsupported place" the moment its `setf' wins the race.
;;
;; So: snapshot BOTH real Emacs's versions and this subset's own
;; versions of every name below, restore real Emacs's immediately after
;; `require' (protecting sibling files from here on), and have
;; `nelisp-cl-generic-deftest' (a thin `ert-deftest' wrapper used for
;; every test below instead of bare `ert-deftest') temporarily re-`fset'
;; this subset's own versions for the exact dynamic extent of ONE test's
;; body, via `unwind-protect' -- so it does not matter whether a given
;; test's `cl-defstruct'/`cl-call-next-method' call was already resolved
;; at this file's own load time or is resolved lazily when the test
;; body actually runs (confirmed this session that both happen, for
;; different tests, depending on shape): either way, at the moment
;; anything in the body needs one of these names, it is this subset's
;; own.
(defvar nelisp-cl-generic-test--names
  '(cl-block cl-return-from cl-return cl-loop cl-defstruct cl-mapcar
    cl-mapc cl-subseq cl-remove-if-not cl-labels cl-incf defsubst
    cl-every backquote cl-case cl-position cl-set-difference cl-gensym
    setf cl-macrolet cl-symbol-macrolet
    cl-defgeneric cl-defmethod cl-call-next-method cl-next-method-p)
  "Every top-level `defmacro'/`defun' name `lisp/nelisp-cl-macros.el'
unconditionally installs (every `^(defmacro \\|^(defun ' match in that
file whose name is not itself `nelisp-cl-macros--'/`nelisp-cl-generic--'
-prefixed internal plumbing), that this test file's dynamic extent
management (see the commentary above) needs to save/swap/restore.  The
first 20 are real Emacs's own names, most already known to collide
\(`setf' broke `test/nelisp-daemon-test.el' this session) but one found
only by hunting down a SECOND, unrelated cross-file failure after fixing
the first: `backquote' -- overriding it breaks any OTHER file's
\\=`(...) literal containing a bare symbol next to a dotted comma-splice,
which `test/nelisp-stdlib-os-test.el' has (\\=`((,VAR . eventfd))) and
this subset's own `backquote' mishandles (\"wrong-type-argument sequencep
eventfd\").  The last 4 are Doc 185's own new names.")

(defun nelisp-cl-generic-test--snapshot ()
  "Return an alist of NAME -> current `symbol-function' (or nil) for
every name in `nelisp-cl-generic-test--names'."
  (mapcar (lambda (sym) (cons sym (and (fboundp sym) (symbol-function sym))))
          nelisp-cl-generic-test--names))

(defun nelisp-cl-generic-test--restore (snapshot)
  "Re-`fset' every NAME . DEFINITION pair in SNAPSHOT (from
`nelisp-cl-generic-test--snapshot'), `fmakunbound'-ing a NAME whose
DEFINITION was nil (unbound at snapshot time)."
  (dolist (pair snapshot)
    (if (cdr pair) (fset (car pair) (cdr pair)) (fmakunbound (car pair)))))

(defvar nelisp-cl-generic-test--real-emacs-fns
  (nelisp-cl-generic-test--snapshot)
  "Real Emacs's own definitions, captured before `nelisp-cl-macros' loads.")

(require 'nelisp-cl-macros)

(defvar nelisp-cl-generic-test--subset-fns
  (nelisp-cl-generic-test--snapshot)
  "This subset's own definitions, captured right after `nelisp-cl-macros'
loads (and before real Emacs's are restored, immediately below).")

(nelisp-cl-generic-test--restore nelisp-cl-generic-test--real-emacs-fns)

(defmacro nelisp-cl-generic-deftest (name args &rest body)
  "Like `ert-deftest', but runs BODY with this subset's own
`cl-defstruct'/`cl-defgeneric'/`cl-defmethod'/`cl-call-next-method'/
`cl-next-method-p'/`setf'/`cl-loop'/`cl-block'/`cl-return'/`cl-return-
from' temporarily re-installed for BODY's exact dynamic extent (see the
big commentary above `nelisp-cl-generic-test--names'), restoring
whatever was active before -- real Emacs's own, for every test in this
file -- via `unwind-protect' so a failing `should' still restores
correctly.

Also runs BODY via `eval' on a quoted form rather than splicing it
directly: `internal-macroexpand-for-load' eagerly, and non-uniformly
\(confirmed this session -- most `cl-defmethod' calls across this file's
tests get resolved correctly this way, but not all: one, on two builtin-
type specializers, did not) resolves some macro calls found INSIDE a
stored `ert-deftest' body at THIS FILE's own load time, using whatever
is bound then -- before the dynamic swap below ever runs even once.
`eval' on a quoted copy of BODY forces a fresh macro-expansion pass at
the moment BODY actually runs, inside the swap's dynamic extent, so it
does not matter which of the two resolution paths a given call would
otherwise have taken."
  (declare (indent 2) (debug (&define name sexp def-body)))
  `(ert-deftest ,name ,args
     (let ((nelisp-cl-generic-test--saved (nelisp-cl-generic-test--snapshot)))
       (unwind-protect
           (progn
             (nelisp-cl-generic-test--restore nelisp-cl-generic-test--subset-fns)
             (eval '(progn ,@body) t))
         (nelisp-cl-generic-test--restore nelisp-cl-generic-test--saved)))))

(defmacro nelisp-cl-generic-test--eval (form)
  "Macroexpand-and-eval FORM (unevaluated) under lexical binding.
Used for forms that must signal AT MACROEXPANSION TIME -- `should-error'
around a literal `(cl-defmethod ...)' call would macroexpand FORM before
`should-error' ever gets to run it, so the error would already have
escaped `should-error''s own dynamic extent.  `eval'-ing the quoted form
here defers macroexpansion to inside `should-error''s protection,
exactly like docs/design/185-cl-generic-subset.org §5.1's own example."
  `(eval ',form t))

;;; §5.1 against-the-bug -- verbatim from Doc 185, red on the unfixed
;;; tree (`void-function cl-defgeneric'), green once the subset lands.

(nelisp-cl-generic-deftest nelisp-cl-generic/dispatches-on-struct-type ()
  (cl-defstruct cgt-animal name)
  (cl-defstruct (cgt-dog (:include cgt-animal)) breed)
  (cl-defgeneric cgt-speak (x))
  (cl-defmethod cgt-speak ((x cgt-animal)) 'generic-noise)
  (cl-defmethod cgt-speak ((x cgt-dog)) 'woof)
  (should (eq 'woof (cgt-speak (make-cgt-dog :name "Rex" :breed "lab"))))
  (should (eq 'generic-noise (cgt-speak (make-cgt-animal :name "generic")))))

(nelisp-cl-generic-deftest nelisp-cl-generic/unsupported-qualifier-signals-at-defmethod ()
  (cl-defstruct cgt-uq-animal name)
  (cl-defgeneric cgt-uq-speak (x))
  (should-error
   (eval '(cl-defmethod cgt-uq-speak :around ((x cgt-uq-animal)) (cl-call-next-method)) t)
   :type 'error))

(nelisp-cl-generic-deftest nelisp-cl-generic/no-applicable-method-signals ()
  (cl-defstruct cgt-nam-dog name)
  (cl-defgeneric cgt-only-dogs (x))
  (cl-defmethod cgt-only-dogs ((x cgt-nam-dog)) 'ok)
  (should-error (cgt-only-dogs 5) :type 'cl-no-applicable-method))

;;; Parity form 1/2 -- `cl-no-applicable-method' signal DATA shape.
;;; Measured this session against real, unmodified Emacs 30.1:
;;;   (cl-defgeneric g (x)) (cl-defmethod g ((x cgt-dog)) 'ok)
;;;   (condition-case e (g 5) (cl-no-applicable-method e))
;;;   => (cl-no-applicable-method cgt-only-dogs2 5)
;;; i.e. DATA = `(cons generic-name args)', flat -- NOT the nested
;;; `(list generic-name args)' Doc 185 §3.5's table text describes.  This
;;; test follows the measured shape (see the matching comment at
;;; `nelisp-cl-generic--invoke' in lisp/nelisp-cl-macros.el).

(nelisp-cl-generic-deftest nelisp-cl-generic/no-applicable-method-data-shape-matches-real-emacs ()
  (cl-defstruct cgt-dshape-dog name)
  (cl-defgeneric cgt-dshape-only-dogs (x))
  (cl-defmethod cgt-dshape-only-dogs ((x cgt-dshape-dog)) 'ok)
  (should (equal (condition-case e (cgt-dshape-only-dogs 5) (cl-no-applicable-method e))
                 '(cl-no-applicable-method cgt-dshape-only-dogs 5))))

;;; Specializer kinds (§2.1)

(nelisp-cl-generic-deftest nelisp-cl-generic/eql-specializer-outranks-unspecialized ()
  (cl-defgeneric cgt-render (x))
  (cl-defmethod cgt-render ((x (eql 'special))) 'special-form)
  (cl-defmethod cgt-render (x) 'fallback)
  (should (eq 'special-form (cgt-render 'special)))
  (should (eq 'fallback (cgt-render 'anything-else))))

(nelisp-cl-generic-deftest nelisp-cl-generic/eql-specializer-outranks-type-specializer ()
  (cl-defgeneric cgt-eql-vs-type (x))
  (cl-defmethod cgt-eql-vs-type ((x integer)) 'as-integer)
  (cl-defmethod cgt-eql-vs-type ((x (eql 7))) 'as-seven)
  (should (eq 'as-seven (cgt-eql-vs-type 7)))
  (should (eq 'as-integer (cgt-eql-vs-type 8))))

(nelisp-cl-generic-deftest nelisp-cl-generic/builtin-type-specializer-outranks-unspecialized ()
  (cl-defgeneric cgt-type-vs-unspec (x))
  (cl-defmethod cgt-type-vs-unspec ((x string)) 'as-string)
  (cl-defmethod cgt-type-vs-unspec (x) 'fallback)
  (should (eq 'as-string (cgt-type-vs-unspec "hi")))
  (should (eq 'fallback (cgt-type-vs-unspec 5))))

(nelisp-cl-generic-deftest nelisp-cl-generic/struct-type-outranks-unspecialized ()
  (cl-defstruct cgt-stu-widget id)
  (cl-defgeneric cgt-stu (x))
  (cl-defmethod cgt-stu ((x cgt-stu-widget)) 'widget)
  (cl-defmethod cgt-stu (x) 'fallback)
  (should (eq 'widget (cgt-stu (make-cgt-stu-widget))))
  (should (eq 'fallback (cgt-stu 42))))

;;; Struct-ancestry ordering / `cl-call-next-method' chain (§2.2/§3.3, P1)

(nelisp-cl-generic-deftest nelisp-cl-generic/call-next-method-three-level-chain-order ()
  "Parity form 2/2 -- 3-level `:include' chain call order.
Measured this session against real, unmodified Emacs 30.1 with the same
struct hierarchy and method bodies: the reversed push-log (i.e. actual
call order) is `(child parent grandparent)' there too -- most specific
first, each method running before it calls `cl-call-next-method' into
its parent.  Doc 185 §5.2's own table text (\"grandparent-then-parent-
then-child\") describes the RAW, un-reversed push log, which for both
implementations reads `(grandparent parent child)' -- `push' always
prepends, so the raw list ends up in reverse-of-call order; both
readings agree between this subset and real Emacs."
  (cl-defstruct cgt-3l-grandparent tag)
  (cl-defstruct (cgt-3l-parent (:include cgt-3l-grandparent)) tag2)
  (cl-defstruct (cgt-3l-child (:include cgt-3l-parent)) tag3)
  (let ((log nil))
    (cl-defgeneric cgt-3l-chain (x))
    (cl-defmethod cgt-3l-chain ((x cgt-3l-grandparent))
      (push 'grandparent log))
    (cl-defmethod cgt-3l-chain ((x cgt-3l-parent))
      (push 'parent log)
      (cl-call-next-method))
    (cl-defmethod cgt-3l-chain ((x cgt-3l-child))
      (push 'child log)
      (cl-call-next-method))
    (cgt-3l-chain (make-cgt-3l-child))
    (should (equal (reverse log) '(child parent grandparent)))
    (should (equal log '(grandparent parent child)))))

(nelisp-cl-generic-deftest nelisp-cl-generic/next-method-p-false-with-single-method ()
  (cl-defstruct cgt-nmp-cat name)
  (cl-defgeneric cgt-nmp (x))
  (cl-defmethod cgt-nmp ((x cgt-nmp-cat))
    (if (cl-next-method-p) 'has-next 'no-next))
  (should (eq 'no-next (cgt-nmp (make-cgt-nmp-cat)))))

(nelisp-cl-generic-deftest nelisp-cl-generic/next-method-p-true-with-parent-method ()
  (cl-defstruct cgt-nmpt-animal name)
  (cl-defstruct (cgt-nmpt-dog (:include cgt-nmpt-animal)) breed)
  (cl-defgeneric cgt-nmpt (x))
  (cl-defmethod cgt-nmpt ((x cgt-nmpt-animal)) 'animal-said-it)
  (cl-defmethod cgt-nmpt ((x cgt-nmpt-dog))
    (if (cl-next-method-p) (cl-call-next-method) 'no-next))
  (should (eq 'animal-said-it (cgt-nmpt (make-cgt-nmpt-dog)))))

(nelisp-cl-generic-deftest nelisp-cl-generic/no-next-method-signals ()
  (cl-defstruct cgt-nnm-cat name)
  (cl-defgeneric cgt-nnm (x))
  (cl-defmethod cgt-nnm ((x cgt-nnm-cat)) (cl-call-next-method))
  (should-error (cgt-nnm (make-cgt-nnm-cat)) :type 'cl-no-next-method))

;;; Redefinition (§3.4)

(nelisp-cl-generic-deftest nelisp-cl-generic/redefining-a-method-replaces-not-duplicates ()
  (cl-defstruct cgt-redef-cat name)
  (cl-defgeneric cgt-redef (x))
  (cl-defmethod cgt-redef ((x cgt-redef-cat)) 'v1)
  (cl-defmethod cgt-redef ((x cgt-redef-cat)) 'v2)
  ;; If redefinition accumulated instead of replacing, two identically-
  ;; specific struct methods would be an ambiguous dispatch (§3.5) --
  ;; getting a plain, unambiguous `v2' back proves it replaced.
  (should (eq 'v2 (cgt-redef (make-cgt-redef-cat)))))

;;; Ambiguity (§3.3/§3.5, P3)

(nelisp-cl-generic-deftest nelisp-cl-generic/ambiguous-builtin-type-dispatch-signals ()
  (cl-defgeneric cgt-ambig (x))
  (cl-defmethod cgt-ambig ((x integer)) 'as-int)
  (cl-defmethod cgt-ambig ((x number)) 'as-num)
  (should-error (cgt-ambig 5) :type 'error))

(nelisp-cl-generic-deftest nelisp-cl-generic/unrelated-struct-types-not-ambiguous-when-neither-applies ()
  "Doc 185 §4 P3's exit criterion: two methods on unrelated struct types,
called with an instance of neither, signal plain no-applicable-method,
not an ambiguity error."
  (cl-defstruct cgt-una-dog name)
  (cl-defstruct cgt-una-cat name)
  (cl-defstruct cgt-una-fish name)
  (cl-defgeneric cgt-una (x))
  (cl-defmethod cgt-una ((x cgt-una-dog)) 'dog)
  (cl-defmethod cgt-una ((x cgt-una-cat)) 'cat)
  (should-error (cgt-una (make-cgt-una-fish)) :type 'cl-no-applicable-method))

;;; Loud-failure matrix (§3.5) beyond the against-the-bug qualifier case

(nelisp-cl-generic-deftest nelisp-cl-generic/unsupported-specializer-form-signals ()
  (cl-defgeneric cgt-usf (x))
  (should-error (nelisp-cl-generic-test--eval (cl-defmethod cgt-usf ((x (head foo))) 'nope))
                :type 'error))

(nelisp-cl-generic-deftest nelisp-cl-generic/specializer-on-position-gt-0-signals ()
  (cl-defstruct cgt-pgt0-dog name)
  (cl-defgeneric cgt-pgt0 (x y))
  (should-error
   (nelisp-cl-generic-test--eval (cl-defmethod cgt-pgt0 (x (y cgt-pgt0-dog)) 'nope))
   :type 'error))

(nelisp-cl-generic-deftest nelisp-cl-generic/unsupported-lambda-list-keyword-signals ()
  (cl-defgeneric cgt-ctx (x))
  (should-error
   (nelisp-cl-generic-test--eval
    (cl-defmethod cgt-ctx (x &context (major-mode c-mode)) 'nope))
   :type 'error))

(nelisp-cl-generic-deftest nelisp-cl-generic/before-after-qualifiers-also-signal ()
  (cl-defstruct cgt-ba-animal name)
  (cl-defgeneric cgt-ba (x))
  (should-error
   (nelisp-cl-generic-test--eval (cl-defmethod cgt-ba :before ((x cgt-ba-animal)) 'nope))
   :type 'error)
  (should-error
   (nelisp-cl-generic-test--eval (cl-defmethod cgt-ba :after ((x cgt-ba-animal)) 'nope))
   :type 'error))

(nelisp-cl-generic-deftest nelisp-cl-generic/default-method-body-signals-at-defgeneric ()
  "Doc 185's subset does not implement CLOS-style default-method bodies
on `cl-defgeneric' itself -- a non-docstring BODY form is a loud
macroexpansion-time error rather than a silently-ignored default method."
  (should-error
   (nelisp-cl-generic-test--eval (cl-defgeneric cgt-dmb (x) (+ x 1)))
   :type 'error))

(nelisp-cl-generic-deftest nelisp-cl-generic/docstring-only-cl-defgeneric-is-fine ()
  (should (eq 'cgt-doc-ok (eval '(cl-defgeneric cgt-doc-ok (x) "A docstring, nothing else.") t))))

;;; Bare `cl-defmethod' with no preceding `cl-defgeneric' (§4 P0 --
;;; matches real `cl-generic''s own auto-declare behaviour).

(nelisp-cl-generic-deftest nelisp-cl-generic/bare-cl-defmethod-without-cl-defgeneric-works ()
  (cl-defstruct cgt-bare-thing id)
  (cl-defmethod cgt-bare-only ((x cgt-bare-thing)) 'bare-ok)
  (should (eq 'bare-ok (cgt-bare-only (make-cgt-bare-thing)))))

(provide 'nelisp-cl-generic-test)
;;; nelisp-cl-generic-test.el ends here
