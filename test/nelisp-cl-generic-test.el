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
  "`(head SYMBOL)' moved to §2.1's supported set (T59 addendum -- see
the `nelisp-cl-generic/head-*' tests below); `(satisfies PRED)' (real
CLOS/`cl-generic' has it, this subset never has) stays out of scope and
still signals, exactly as `(head ...)' itself used to before T59."
  (cl-defgeneric cgt-usf (x))
  (should-error
   (nelisp-cl-generic-test--eval (cl-defmethod cgt-usf ((x (satisfies cgt-usf-pred))) 'nope))
   :type 'error))

(nelisp-cl-generic-deftest nelisp-cl-generic/specializer-on-position-gt-0-dispatches ()
  "Required-argument specializers may occur after position 0, matching
real `cl-defmethod' and the all-the-icons-completion call shape."
  (cl-defgeneric cgt-pgt0 (x y))
  (cl-defmethod cgt-pgt0 (x y) 'fallback)
  (cl-defmethod cgt-pgt0 (x (y (eql 'dog))) (list 'dog x y))
  (should (equal '(dog left dog) (cgt-pgt0 'left 'dog)))
  (should (eq 'fallback (cgt-pgt0 'left 'cat))))

(nelisp-cl-generic-deftest nelisp-cl-generic/subclass-specializer-dispatches ()
  "`(subclass CLASS)' is a class-designator specializer.  The equality
case is useful during EIEIO bootstrap, before class ancestry is available."
  (cl-defgeneric cgt-subclass (x))
  (cl-defmethod cgt-subclass (x) 'fallback)
  (cl-defmethod cgt-subclass ((x (subclass cgt-root))) 'subclass)
  (should (eq 'subclass (cgt-subclass '(subclass cgt-root))))
  (should (eq 'fallback (cgt-subclass '(subclass cgt-other)))))

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

(nelisp-cl-generic-deftest nelisp-cl-generic/default-method-body-defines-unspecialized-method ()
  "T81 addendum: this used to prove a `cl-defgeneric' default-method body
was rejected (a loud macroexpansion-time `error') -- default bodies are
now supported (Doc 185's T81 addendum), so it correctly proves the
opposite instead: a non-docstring, non-option BODY form defines an
ordinary unspecialized primary method, callable immediately, exactly
like real Emacs's own `cl-defgeneric'."
  (cl-defgeneric cgt-dmb (x) (+ x 1))
  (should (equal 6 (cgt-dmb 5))))

(nelisp-cl-generic-deftest nelisp-cl-generic/docstring-only-cl-defgeneric-is-fine ()
  (should (eq 'cgt-doc-ok (eval '(cl-defgeneric cgt-doc-ok (x) "A docstring, nothing else.") t))))

;;; Bare `cl-defmethod' with no preceding `cl-defgeneric' (§4 P0 --
;;; matches real `cl-generic''s own auto-declare behaviour).

(nelisp-cl-generic-deftest nelisp-cl-generic/bare-cl-defmethod-without-cl-defgeneric-works ()
  (cl-defstruct cgt-bare-thing id)
  (cl-defmethod cgt-bare-only ((x cgt-bare-thing)) 'bare-ok)
  (should (eq 'bare-ok (cgt-bare-only (make-cgt-bare-thing)))))

;;; §2.2 extension -- `:extra STRING' qualifier.
;;;
;;; Every ordering claim below was cross-checked this session against
;;; real, unmodified Emacs 31.1's own `cl-generic' (a throwaway batch
;;; process, `(require 'cl-lib)'/`(require 'cl-generic)', no
;;; `nelisp-cl-macros' loaded) -- these are parity forms, not just
;;; internally-consistent assertions.  This is the exact shape of the
;;; real-world bug this extension fixes: real Emacs's own
;;; `lisp/emacs-lisp/cl-lib.el' (loaded transitively by
;;; `(require 'cl-lib)', e.g. from `eat.el') has, at its own top level:
;;;   (cl-defmethod cl-generic-generalizers :extra "derived-types" (type)
;;;     ...)
;;; an UNSPECIALIZED-arg `:extra' method on the same generic function
;;; real Emacs's own `cl-generic.el' already gives a plain (non-`:extra')
;;; unspecialized primary method -- exactly the shape of `CaseB'/`CaseB2'
;;; below.

(nelisp-cl-generic-deftest nelisp-cl-generic/extra-methods-coexist-newest-first ()
  "Parity form: real Emacs 31.1, same generic/methods, defined in the
same order (primary, then `:extra' \"e1\", then `:extra' \"e2\"), gives
call order `(e2 e1 primary)' -- most-recently-defined first, ending at
the plain primary method (which is simply the chronologically oldest
member of this group, not specially fixed-last: a separate real-Emacs
probe this session, defining `:extra' \"e1\" FIRST, then the plain
primary, then `:extra' \"e2\" LAST, gave `(e2 primary e1)' -- pure
definition-recency order)."
  (let (order)
    (cl-defgeneric cgt-extra-basic (x))
    (cl-defmethod cgt-extra-basic ((x integer))
      (push 'primary order)
      (if (cl-next-method-p) (cl-call-next-method) 'done))
    (cl-defmethod cgt-extra-basic :extra "e1" ((x integer))
      (push 'e1 order) (cl-call-next-method))
    (cl-defmethod cgt-extra-basic :extra "e2" ((x integer))
      (push 'e2 order) (cl-call-next-method))
    (cgt-extra-basic 5)
    (should (equal (reverse order) '(e2 e1 primary)))))

(nelisp-cl-generic-deftest nelisp-cl-generic/extra-method-redefinition-keeps-position-not-front ()
  "Real Emacs's own `cl-generic-define-method' comment: \"Keep the
ordering; important for methods with :extra qualifiers.\" -- redefining
\"e1\" (the OLDEST of the three) after \"e2\" already exists must NOT move
it to the front; it stays between \"e2\" and the primary method.  Parity
form: real Emacs 31.1 gives the identical `(e2 re-e1 primary)', not
`(re-e1 e2 primary)'."
  (let (order)
    (cl-defgeneric cgt-extra-redef (x))
    (cl-defmethod cgt-extra-redef ((x integer))
      (push 'primary order) (if (cl-next-method-p) (cl-call-next-method) 'done))
    (cl-defmethod cgt-extra-redef :extra "e1" ((x integer))
      (push 'e1 order) (cl-call-next-method))
    (cl-defmethod cgt-extra-redef :extra "e2" ((x integer))
      (push 'e2 order) (cl-call-next-method))
    (cl-defmethod cgt-extra-redef :extra "e1" ((x integer))
      (push 're-e1 order) (cl-call-next-method))
    (cgt-extra-redef 5)
    (should (equal (reverse order) '(e2 re-e1 primary)))))

(nelisp-cl-generic-deftest nelisp-cl-generic/extra-on-unspecialized-arg-not-ambiguous ()
  "The real bug this extension fixes: an UNSPECIALIZED `:extra' method
alongside an unspecialized primary (real Emacs's own `cl-lib.el' does
exactly this to `cl-generic-generalizers').  Before this extension, two
unspecialized methods were the SAME `nelisp-cl-generic--same-specializer-p'
identity and one replaced the other; now they coexist, newest first, with
NO ambiguous-dispatch error (unspecialized has no such check regardless,
but this confirms both actually run, in order, via a counter)."
  (let ((ran 0))
    (cl-defgeneric cgt-extra-unspec (x))
    (cl-defmethod cgt-extra-unspec (specializer) 'base-generalizer)
    (cl-defmethod cgt-extra-unspec :extra "derived-types" (type)
      (setq ran (1+ ran))
      (if (cl-next-method-p) (cl-call-next-method) 'no-next))
    (should (eq 'base-generalizer (cgt-extra-unspec 'anything)))
    (should (= ran 1))))

(nelisp-cl-generic-deftest nelisp-cl-generic/extra-same-type-name-not-ambiguous ()
  "Two `:extra' variants of the SAME builtin type-name must NOT trip the
`ambiguous dispatch' check (§3.5) that guards genuinely DIFFERENT
type-names tying -- `nelisp-cl-generic/ambiguous-builtin-type-dispatch-
signals' above still covers that real ambiguity, unchanged."
  (let (order)
    (cl-defgeneric cgt-extra-same-type (x))
    (cl-defmethod cgt-extra-same-type ((x integer))
      (push 'primary order) 'done)
    (cl-defmethod cgt-extra-same-type :extra "e1" ((x integer))
      (push 'e1 order) (cl-call-next-method))
    (cgt-extra-same-type 5)
    (should (equal (reverse order) '(e1 primary)))))

(nelisp-cl-generic-deftest nelisp-cl-generic/extra-requires-a-string-signals ()
  (cl-defgeneric cgt-extra-nostr (x))
  (should-error
   (nelisp-cl-generic-test--eval (cl-defmethod cgt-extra-nostr :extra (x) 'nope))
   :type 'error))

(nelisp-cl-generic-deftest nelisp-cl-generic/extra-combined-with-before-after-around ()
  "Parity form: real Emacs 31.1, the identical generic/methods, gives
call order `(around-enter before primary after around-leave)' and a
final result of `primary-result' -- before/after run for effect only and
never change the primary chain's own return value; `:around' wraps the
whole combination and can inspect/alter what `cl-call-next-method'
returns.  `:extra' must be the FIRST qualifier when combined with a
combinator -- real Emacs's own `cl--generic-standard-method-combination'
only strips a LEADING `:extra STRING' pair, confirmed this session:
`(cl-defmethod bar :around :extra \"x\" (...) ...)' (combinator first)
signals \"Unsupported qualifiers\" in real, unmodified Emacs 31.1 too."
  (let (order)
    (cl-defgeneric cgt-extra-combo (x))
    (cl-defmethod cgt-extra-combo ((x integer))
      (push 'primary order) 'primary-result)
    (cl-defmethod cgt-extra-combo :extra "be1" :before ((x integer))
      (push 'before order))
    (cl-defmethod cgt-extra-combo :extra "ae1" :after ((x integer))
      (push 'after order))
    (cl-defmethod cgt-extra-combo :extra "ar1" :around ((x integer))
      (push 'around-enter order)
      (prog1 (cl-call-next-method)
        (push 'around-leave order)))
    (should (eq 'primary-result (cgt-extra-combo 7)))
    (should (equal (reverse order)
                   '(around-enter before primary after around-leave)))))

(nelisp-cl-generic-deftest nelisp-cl-generic/extra-combinator-wrong-order-signals ()
  "`:around :extra STRING' (combinator before `:extra') is not the same
qualifier list as `:extra STRING :around' -- real Emacs itself rejects
the former (measured this session) with \"Unsupported qualifiers\"; this
subset rejects it too, at `cl-defmethod' macroexpansion time, same as
every other unsupported qualifier shape (§3.5)."
  (cl-defgeneric cgt-extra-wrong-order (x))
  (should-error
   (nelisp-cl-generic-test--eval
    (cl-defmethod cgt-extra-wrong-order :around :extra "x" (x) 'nope))
   :type 'error))

(nelisp-cl-generic-deftest nelisp-cl-generic/no-primary-method-signals ()
  "A call whose only applicable methods are `:extra'+combinator (no
plain, no-combinator method at all, `:extra' or otherwise) signals
`cl-no-primary-method' -- real Emacs's own condition name/message, not
reachable in this subset before the `:extra' extension (previously any
qualifier other than none was rejected at `cl-defmethod' time, so \"some
methods applicable, none of them primary\" could never arise)."
  (cl-defgeneric cgt-no-primary (x))
  (cl-defmethod cgt-no-primary :extra "b" :before ((x integer))
    (ignore x))
  (should-error (cgt-no-primary 5) :type 'cl-no-primary-method))

;;; T59 addendum -- `(head SYMBOL)' specializer.
;;;
;;; The real bug this closes: `eat.el' itself uses `(cl-defmethod ...
;;; ((x (head SYMBOL))) ...)' forms, which used to hit
;;; `nelisp-cl-generic/unsupported-specializer-form-signals' above (moved
;;; to `(satisfies ...)' now that `head' is supported).  Every ordering
;;; claim below was cross-checked this session against real, unmodified
;;; Emacs 31.1's own `cl-generic' (a throwaway batch process,
;;; `(require 'cl-lib)', no `nelisp-cl-macros' loaded) -- parity forms,
;;; not just internally-consistent assertions.  Real Emacs's own
;;; generalizer priorities (`cl-generic.el'): `eql' 100, `head' 80, any
;;; `cl-typep' type match 10, the unspecialized catch-all 0 -- so `head'
;;; is strictly between `eql' and every type match, confirmed directly
;;; below rather than assumed.

(nelisp-cl-generic-deftest nelisp-cl-generic/head-matches-cons-with-eq-car ()
  "Parity form: real Emacs 31.1 dispatches `(head foo)' on any cons whose
`car' is `eq' to `foo', and never on a non-cons or a cons headed by a
different symbol."
  (cl-defgeneric cgt-head-basic (x))
  (cl-defmethod cgt-head-basic (x) 'fallback)
  (cl-defmethod cgt-head-basic ((x (head foo))) 'head-foo)
  (should (eq 'head-foo (cgt-head-basic '(foo 1 2))))
  (should (eq 'head-foo (cgt-head-basic '(foo))))
  (should (eq 'fallback (cgt-head-basic '(bar 1))))
  (should (eq 'fallback (cgt-head-basic nil)))
  (should (eq 'fallback (cgt-head-basic 'foo))))

(nelisp-cl-generic-deftest nelisp-cl-generic/head-different-symbols-never-ambiguous ()
  "Two DIFFERENT `head' specializers can never both match one `car' (only
one symbol can be `eq' to it), unlike two different builtin type-names
\(§2.1a's already-documented ambiguous case) -- so this never trips the
ambiguous-dispatch check."
  (cl-defgeneric cgt-head-two (x))
  (cl-defmethod cgt-head-two (x) 'fallback)
  (cl-defmethod cgt-head-two ((x (head foo))) 'head-foo)
  (cl-defmethod cgt-head-two ((x (head bar))) 'head-bar)
  (should (eq 'head-foo (cgt-head-two '(foo))))
  (should (eq 'head-bar (cgt-head-two '(bar))))
  (should (eq 'fallback (cgt-head-two '(baz)))))

(nelisp-cl-generic-deftest nelisp-cl-generic/head-outranks-builtin-type-match ()
  "Parity form: real Emacs 31.1, the same generic/methods (`cons' type
specializer plus `(head foo)'), calls `(head foo)' first for `(foo 1)',
continuing to the `cons' method via `cl-call-next-method' -- generalizer
priority 80 beats 10."
  (let (order)
    (cl-defgeneric cgt-head-vs-cons (x))
    (cl-defmethod cgt-head-vs-cons (x) (push 'fallback order) 'done)
    (cl-defmethod cgt-head-vs-cons ((x cons))
      (push 'cons-type order) (cl-call-next-method))
    (cl-defmethod cgt-head-vs-cons ((x (head foo)))
      (push 'head-foo order) (cl-call-next-method))
    (cgt-head-vs-cons '(foo 1))
    (should (equal (reverse order) '(head-foo cons-type fallback)))))

(defvar cgt-head-vs-eql--shared (list 'foo 1)
  "A global, not `let'-bound: real Emacs's own `eql' specializer method
evaluates its VALUE form via `(eval form t)' OUTSIDE the `cl-defmethod'
call's own lexical scope (confirmed this session -- a `let'-bound
variable of the same name is `void-variable' there), so a form capturing
a value across calls must be a global for this to be a genuine parity
form against real Emacs, not merely internally consistent.")

(nelisp-cl-generic-deftest nelisp-cl-generic/eql-outranks-head ()
  "Parity form: real Emacs 31.1, an `eql' specializer whose value is `eq'
to the very cons passed at call time outranks a `(head foo)' specializer
that ALSO matches it (same cons, `car' `foo') -- generalizer priority 100
beats 80; a call with a DIFFERENT `(foo ...)' cons (not `eql' to the
baked-in value) only reaches the `head' method."
  (let (order)
    (cl-defgeneric cgt-head-vs-eql (x))
    (cl-defmethod cgt-head-vs-eql (x) (push 'fallback order) 'done)
    (cl-defmethod cgt-head-vs-eql ((x (head foo)))
      (push 'head-foo order) (cl-call-next-method))
    (cl-defmethod cgt-head-vs-eql ((x (eql (identity cgt-head-vs-eql--shared))))
      (push 'eql-shared order) (cl-call-next-method))
    (cgt-head-vs-eql cgt-head-vs-eql--shared)
    (should (equal (reverse order) '(eql-shared head-foo fallback)))
    (setq order nil)
    (cgt-head-vs-eql (list 'foo 1))
    (should (equal (reverse order) '(head-foo fallback)))))

(nelisp-cl-generic-deftest nelisp-cl-generic/head-value-not-evaluated ()
  "The VALUE in `(head VALUE)' is taken literally, never evaluated --
matching real Emacs's own `(cadr specializer)' (no `eval' call anywhere
in `cl--generic-head-generalizer'/its `cl-generic-generalizers' method).
A lexically-bound variable named the same as the head symbol must not
leak in: this method only fires for a cons headed by the SYMBOL `quux',
never for one headed by whatever `quux' happens to be bound to."
  (let ((quux 'something-else))
    (cl-defgeneric cgt-head-literal (x))
    (cl-defmethod cgt-head-literal (x) 'fallback)
    (cl-defmethod cgt-head-literal ((x (head quux))) 'head-quux)
    (should (eq 'head-quux (cgt-head-literal '(quux 1))))
    (should (eq 'fallback (cgt-head-literal '(something-else 1))))))

(nelisp-cl-generic-deftest nelisp-cl-generic/head-extra-methods-coexist-newest-first ()
  "Same `:extra' coexistence/chaining machinery as the eql/type tiers
above, now exercised on a `head' specializer -- parity form, real Emacs
31.1 gives the identical `(e2 e1 primary)'."
  (let (order)
    (cl-defgeneric cgt-head-extra (x))
    (cl-defmethod cgt-head-extra ((x (head foo)))
      (push 'primary order) (if (cl-next-method-p) (cl-call-next-method) 'done))
    (cl-defmethod cgt-head-extra :extra "e1" ((x (head foo)))
      (push 'e1 order) (cl-call-next-method))
    (cl-defmethod cgt-head-extra :extra "e2" ((x (head foo)))
      (push 'e2 order) (cl-call-next-method))
    (cgt-head-extra '(foo))
    (should (equal (reverse order) '(e2 e1 primary)))))

(nelisp-cl-generic-deftest nelisp-cl-generic/head-extra-redefinition-keeps-position ()
  "Parity form: redefining \"e1\" after \"e2\" already exists keeps its
list position (`(e2 re-e1 primary)'), matching the eql/type tiers'
already-tested behaviour and real Emacs 31.1's own."
  (let (order)
    (cl-defgeneric cgt-head-extra-redef (x))
    (cl-defmethod cgt-head-extra-redef ((x (head foo)))
      (push 'primary order) (if (cl-next-method-p) (cl-call-next-method) 'done))
    (cl-defmethod cgt-head-extra-redef :extra "e1" ((x (head foo)))
      (push 'e1 order) (cl-call-next-method))
    (cl-defmethod cgt-head-extra-redef :extra "e2" ((x (head foo)))
      (push 'e2 order) (cl-call-next-method))
    (cl-defmethod cgt-head-extra-redef :extra "e1" ((x (head foo)))
      (push 're-e1 order) (cl-call-next-method))
    (cgt-head-extra-redef '(foo))
    (should (equal (reverse order) '(e2 re-e1 primary)))))

(nelisp-cl-generic-deftest nelisp-cl-generic/head-malformed-form-still-signals ()
  "`(head foo bar)' (more than one VALUE) is a DELIBERATE divergence, not
an oversight: measured this session, real Emacs 31.1 macroexpands it
without error and silently ignores the trailing `bar' (its own
generalizer method only ever reads `(cadr specializer)').  Doc 185's
whole loud-failure discipline (§2.2/§3.5) exists precisely to prefer a
macroexpansion-time `error' over a silently-ignored extra token, so this
subset is intentionally stricter here."
  (cl-defgeneric cgt-head-malformed (x))
  (should-error
   (nelisp-cl-generic-test--eval
    (cl-defmethod cgt-head-malformed ((x (head foo bar))) 'nope))
   :type 'error))

;;; T81 addendum -- `cl-defgeneric' default-method bodies and
;;; OPTIONS-AND-METHODS (`(declare ...)', `(:documentation ...)',
;;; `(:method ...)', `(:argument-precedence-order ...)').  Every ordering/
;;; behavior claim below was cross-checked this session against real,
;;; unmodified Emacs 31.1's own `cl-generic' (a throwaway batch process,
;;; `(require 'cl-lib)', no `nelisp-cl-macros' loaded) -- parity forms,
;;; not just internally-consistent assertions.  Gap this addendum fixes:
;;; real Emacs's own `lisp/emacs-lisp/eieio.el' -- the actual consumer
;;; blocker -- has, at its very first `cl-defgeneric' call:
;;;   (cl-defgeneric eieio-object-name-string (obj)
;;;     "Return a string which is OBJ's name."
;;;     (or (gethash obj eieio--object-names)
;;;         (format "%s-%x" (eieio-object-class obj) (sxhash-eq obj))))
;;; -- exactly the default-body shape this addendum lands.

(nelisp-cl-generic-deftest nelisp-cl-generic/default-body-struct-dispatch-order ()
  "Parity form: real Emacs 31.1, a `cl-defgeneric' default body plus
`cl-defmethod' on a struct and its parent -- the default body is just
the LEAST-specific method in the very same chain, reached via
`cl-call-next-method' exactly like any other unspecialized primary
method (no new dispatch machinery).  Measured this session: `(dog
animal default)' for a `cgt-dbo-dog' instance, `(animal default)' for a
plain `cgt-dbo-animal', `(default)' for an unrelated value."
  (cl-defstruct cgt-dbo-animal name)
  (cl-defstruct (cgt-dbo-dog (:include cgt-dbo-animal)) breed)
  (let (order)
    (cl-defgeneric cgt-dbo-speak (x)
      (push 'default order)
      'done)
    (cl-defmethod cgt-dbo-speak ((x cgt-dbo-dog))
      (push 'dog order) (cl-call-next-method))
    (cl-defmethod cgt-dbo-speak ((x cgt-dbo-animal))
      (push 'animal order) (cl-call-next-method))
    (cgt-dbo-speak (make-cgt-dbo-dog :name "Rex" :breed "lab"))
    (should (equal (reverse order) '(dog animal default)))
    (setq order nil)
    (cgt-dbo-speak (make-cgt-dbo-animal :name "generic"))
    (should (equal (reverse order) '(animal default)))
    (setq order nil)
    (cgt-dbo-speak 5)
    (should (equal (reverse order) '(default)))))

(nelisp-cl-generic-deftest nelisp-cl-generic/default-body-next-method-p-false-at-tail ()
  "Parity form: real Emacs 31.1, calling a generic whose ONLY method is
its own default body -- `cl-next-method-p' there is nil (the default
body is the least-specific method in the chain): `(default-fallback
nil)'."
  (cl-defgeneric cgt-dbo-chain (x)
    (list 'default-fallback (cl-next-method-p)))
  (should (equal '(default-fallback nil) (cgt-dbo-chain 5))))

(nelisp-cl-generic-deftest nelisp-cl-generic/default-body-redefinition-replaces-default-keeps-specialized ()
  "Parity form: real Emacs 31.1 -- redefining a `cl-defgeneric' with a
NEW default body replaces only the default (unspecialized) method,
leaving a previously-defined specialized method untouched: `(int
default-v1 42)'/`(default-v1 \"s\")' before redefinition, `(int
default-v2 42)'/`(default-v2 \"s\")' after.  This falls out for free from
the existing method table: the new default body is just another `:kind
unspecialized', no-`:extra', no-combinator entry, whose identity
\(`nelisp-cl-generic--same-specializer-p') already matches the old one,
so `nelisp-cl-generic--register-method' replaces it in place -- no new
machinery needed."
  (cl-defgeneric cgt-dbo-redef (x) (list 'default-v1 x))
  (cl-defmethod cgt-dbo-redef ((x integer))
    (cons 'int (cl-call-next-method)))
  (should (equal '(int default-v1 42) (cgt-dbo-redef 42)))
  (should (equal '(default-v1 "s") (cgt-dbo-redef "s")))
  (cl-defgeneric cgt-dbo-redef (x) (list 'default-v2 x))
  (should (equal '(int default-v2 42) (cgt-dbo-redef 42)))
  (should (equal '(default-v2 "s") (cgt-dbo-redef "s"))))

(nelisp-cl-generic-deftest nelisp-cl-generic/declare-form-accepted-with-default-body ()
  "`(declare ...)' is accepted syntactically (no error) but its content
is not applied -- this subset does not implement e.g. `(indent N)' side
effects.  Confirmed against real Emacs 31.1: an equivalent
`(declare (indent 1))' there is silently accepted the same way, with the
default body still callable afterward."
  (cl-defgeneric cgt-dbo-declared (x)
    (declare (indent 1))
    (list 'declared-default x))
  (should (equal '(declared-default 9) (cgt-dbo-declared 9))))

(nelisp-cl-generic-deftest nelisp-cl-generic/declare-only-defines-no-default-method ()
  "A `cl-defgeneric' whose only content is `(declare ...)' (nothing left
once that run ends) defines no default method at all -- real Emacs
31.1's own `eieio.el' has exactly this shape (`destructor'/
`eieio-object-set-name-string': `(declare (obsolete ...))' and nothing
else).  A subsequent call with no applicable method still signals
`cl-no-applicable-method', exactly as an empty `cl-defgeneric' already
did before this addendum."
  (cl-defgeneric cgt-dbo-declareonly (x)
    (declare (indent 1)))
  (should-error (cgt-dbo-declareonly 1) :type 'cl-no-applicable-method))

(nelisp-cl-generic-deftest nelisp-cl-generic/inline-method-forms-define-methods ()
  "Parity form: real Emacs 31.1, `(:method ...)' forms inside a
`cl-defgeneric' expand to ordinary methods, in the order written,
alongside a trailing default body: `(inline-int 3)'/`(inline-string
\"hi\")'/`(inline-default sym)'."
  (cl-defgeneric cgt-dbo-inline (x)
    (:method ((x integer)) (list 'inline-int x))
    (:method ((x string)) (list 'inline-string x))
    (list 'inline-default x))
  (should (equal '(inline-int 3) (cgt-dbo-inline 3)))
  (should (equal '(inline-string "hi") (cgt-dbo-inline "hi")))
  (should (equal '(inline-default sym) (cgt-dbo-inline 'sym))))

(nelisp-cl-generic-deftest nelisp-cl-generic/inline-method-bare-around-still-signals ()
  "A `(:method :around ...)' form expands to an ordinary `(cl-defmethod
NAME :around ...)' call -- QUALIFIERS go through `cl-defmethod''s own,
UNCHANGED grammar, so a bare (non-`:extra') `:around' is still rejected
there exactly as it would be at a top-level `cl-defmethod' call (Doc 185
§2.2's documented divergence from real Emacs, which accepts it)."
  (should-error
   (nelisp-cl-generic-test--eval
    (cl-defgeneric cgt-dbo-inline-around (x)
      (:method :around ((x integer)) (cl-call-next-method))))
   :type 'error))

(nelisp-cl-generic-deftest nelisp-cl-generic/inline-method-extra-around-combination-works ()
  "`(:method :extra STRING :around ...)' reaches `cl-defmethod''s own
ALREADY-supported `:extra'+combinator grammar unchanged (Doc 185 §2.2's
`:extra' extension) -- only a BARE `:around' (no `:extra') is rejected."
  (let (order)
    (cl-defgeneric cgt-dbo-inline-extra-around (x)
      (:method :extra "wrap" :around ((x integer))
       (push 'around order) (cl-call-next-method))
      (:method ((x integer)) (push 'primary order) 'done))
    (cgt-dbo-inline-extra-around 7)
    (should (equal (reverse order) '(around primary)))))

(nelisp-cl-generic-deftest nelisp-cl-generic/multiple-doc-strings-signals ()
  "Parity form: real Emacs 31.1 signals `(error \"Multiple doc strings
for %S\" NAME)' for a leading docstring combined with
`(:documentation ...)' -- measured this session."
  (should-error
   (nelisp-cl-generic-test--eval
    (cl-defgeneric cgt-dbo-doc2 (x) "doc1" (:documentation "doc2") (list 'd x)))
   :type 'error))

(nelisp-cl-generic-deftest nelisp-cl-generic/multiple-declare-forms-signals ()
  "Parity form: real Emacs 31.1 signals `(error \"Multiple `declare' for
%S\" NAME)' for two `(declare ...)' forms on the same `cl-defgeneric' --
measured this session."
  (should-error
   (nelisp-cl-generic-test--eval
    (cl-defgeneric cgt-dbo-decl2 (x)
      (declare (indent 1))
      (declare (indent 2))
      (list 'd x)))
   :type 'error))

(nelisp-cl-generic-deftest nelisp-cl-generic/unsupported-defgeneric-option-signals ()
  "Any keyword-headed OPTIONS-AND-METHODS form other than
`:documentation'/`:method'/`:argument-precedence-order'/`declare' is a
loud macroexpansion-time `error' naming it (Doc 185 §3.5's discipline
extended to this grammar) -- never silently collected and ignored."
  (should-error
   (nelisp-cl-generic-test--eval
    (cl-defgeneric cgt-dbo-badopt (x) (:something-else 1 2) (list 'd x)))
   :type 'error))

(nelisp-cl-generic-deftest nelisp-cl-generic/argument-precedence-order-controls-dispatch ()
  "`(:argument-precedence-order ...)' controls the lexicographic priority
of multiple dispatch positions, matching real `cl-generic'."
  (cl-defgeneric cgt-dbo-apo (x y)
    (:argument-precedence-order y x)
    (list 'apo-default x y))
  (cl-defmethod cgt-dbo-apo ((x integer) y) 'x-first)
  (cl-defmethod cgt-dbo-apo (x (y integer)) 'y-second)
  (should (eq 'y-second (cgt-dbo-apo 1 2)))
  (cl-defgeneric cgt-dbo-apo-default (x y)
    (:argument-precedence-order x y)
    (list 'apo-default x y))
  (cl-defmethod cgt-dbo-apo-default ((x integer) y) 'x-first)
  (cl-defmethod cgt-dbo-apo-default (x (y integer)) 'y-second)
  (should (eq 'x-first (cgt-dbo-apo-default 1 2))))

(provide 'nelisp-cl-generic-test)
;;; nelisp-cl-generic-test.el ends here
