;;; nelisp-emacs-magit-bridge.el --- load the real vendor Magit chain into a live session  -*- lexical-binding: t; -*-

;;; Commentary:

;; Task #17 (M1).  Session-side loader that brings the unpatched vendor
;; Magit/transient/with-editor chain into a live NeLisp session (a
;; persistent `nelisp --repl' session, or a runtime-image bake via
;; `extend-runtime-image').  This is a bridge, not a reimplementation: it
;; never patches or reimplements Magit, transient, or with-editor source.
;;
;; Mechanism: `scripts/build-nelisp-emacs-magit-bridge-bundle.el' (run under
;; host Emacs, mirroring `scripts/build-nelisp-bootstrap.el') normalizes the
;; real vendor source (same `standalone-source-normalize' path already
;; proven by `scripts/vendor-repl-standalone-replay.el' in
;; docs/design/33-emacs-core-substrate-priority-plan.org) into one
;; plain-Elisp bundle, `build/nelisp-emacs-magit-bridge-bundle.el', with each
;; file's forms wrapped in `(unless (featurep FEATURE) ...)'.  This module
;; only has to `load' that pre-normalized bundle from inside NeLisp itself;
;; normalization never runs inside the standalone reader.
;;
;; Ownership: this file owns "bring the Magit vendor closure into a NeLisp
;; session" as a reusable adapter step, per the library-first CLAUDE.md/
;; AGENTS.md rule that command/session semantics stay out of app glue.
;; `apps/nemacs-next' and `scripts/nemacs-runtime-image-preload.el' call into
;; this module; they do not duplicate its logic.

;;; Code:

(defvar nelisp-emacs-magit-bridge-repo-root nil
  "Repository root used to resolve the generated bundle path.
When nil, `nelisp-emacs-magit-bridge-load' derives it from
`load-file-name'/`buffer-file-name' (this file lives at REPO/src/).")

(defvar nelisp-emacs-magit-bridge-bundle-file nil
  "Explicit path to the generated magit bridge bundle.
When nil, resolved as build/nelisp-emacs-magit-bridge-bundle.el under
`nelisp-emacs-magit-bridge-repo-root'.")

(defvar nelisp-emacs-magit-bridge-loaded nil
  "Non-nil once `nelisp-emacs-magit-bridge-load' has run in this session.")

(defvar nelisp-emacs-magit-bridge--current-message nil
  "Last echo-area style message observed through the bridge facade.")

(defvar nelisp-emacs-magit-bridge--magit-section-forward-orig nil
  "Saved original `magit-section-forward' function.")

(defvar nelisp-emacs-magit-bridge--symbol-function-orig nil
  "Saved original `symbol-function' before bridge compatibility wrapping.")

(defvar nelisp-emacs-magit-bridge--magit-stage-orig nil
  "Saved original `magit-stage' function.")

(defvar nelisp-emacs-magit-bridge--slot-boundp-orig nil
  "Saved original `slot-boundp' before bridge compatibility wrapping.")

(defvar nelisp-emacs-magit-bridge--nelisp-setf-1-orig nil
  "Saved original standalone `nelisp--setf-1' generalized-place expander.")

(defvar nelisp-emacs-magit-bridge--section-slot-overrides nil
  "Alist mapping section objects to bridge-maintained slot override plists.")

(defun nelisp-emacs-magit-bridge--section-slot-put (section slot value)
  "Record VALUE for SECTION SLOT and try to mirror it into the object too."
  (let ((cell (assq section nelisp-emacs-magit-bridge--section-slot-overrides)))
    (if cell
        (setcdr cell (plist-put (cdr cell) slot value))
      (push (cons section (list slot value))
            nelisp-emacs-magit-bridge--section-slot-overrides)))
  (unless (memq slot '(start content end))
    (ignore-errors (oset section slot value)))
  value)

(defun nelisp-emacs-magit-bridge--section-slot-value (section slot)
  "Return bridge-maintained SECTION SLOT value when available."
  (let ((cell (assq section nelisp-emacs-magit-bridge--section-slot-overrides)))
    (if cell
        (plist-get (cdr cell) slot)
      (ignore-errors (oref section slot)))))

(defun nelisp-emacs-magit-bridge--section-slot-pos (section slot)
  "Return integer position stored in SECTION SLOT."
  (nelisp-emacs-magit-bridge--section-pos
   (nelisp-emacs-magit-bridge--section-slot-value section slot)))

(defun nelisp-emacs-magit-bridge--section-pos (value)
  "Return integer position for section slot VALUE."
  (cond
   ((and (fboundp 'nelisp-ec-marker-p)
         (ignore-errors (nelisp-ec-marker-p value))
         (fboundp 'nelisp-ec-marker-position))
    (ignore-errors (nelisp-ec-marker-position value)))
   ((markerp value) (marker-position value))
   ((integerp value) value)
   (t nil)))

(defun nelisp-emacs-magit-bridge--repair-section-properties (section &optional seen)
  "Reapply `magit-section' properties from SECTION's object tree.

The standalone runtime currently leaves some vendor
`magit-section--set-section-properties' writes unapplied when marker-backed
slot values are passed through directly.  Rewriting the property spans from
the realized section tree makes section navigation observe the same object
graph the buffer already holds."
  (unless (memq section seen)
    (let* ((seen (cons section seen))
           (start (nelisp-emacs-magit-bridge--section-slot-pos section 'start))
           (content (nelisp-emacs-magit-bridge--section-slot-pos section 'content))
           (end (nelisp-emacs-magit-bridge--section-slot-pos section 'end))
           (children (ignore-errors (oref section children))))
      (when (and start end (< start end))
        (add-text-properties
         start (or (and content (<= start content) content) end)
         (list 'magit-section section)))
      (if children
          (dolist (child children)
            (when (and child (not (eq child section)))
              (nelisp-emacs-magit-bridge--repair-section-properties child seen)))
        (when (and start end (< start end))
          (add-text-properties start end (list 'magit-section section)))))))

(defun nelisp-emacs-magit-bridge--trace (format-string &rest args)
  "Print a bridge trace line when `NEMACS_MAGIT_BRIDGE_TRACE' is enabled."
  (when (and (fboundp 'nelisp--write-stdout-bytes)
             (equal (getenv "NEMACS_MAGIT_BRIDGE_TRACE") "1"))
    (nelisp--write-stdout-bytes
     (apply #'format (concat "MAGIT-BRIDGE " format-string "\n") args))))

(defun nelisp-emacs-magit-bridge--trace-backtrace (prefix)
  "Emit the current backtrace with PREFIX on each line."
  (when (fboundp 'backtrace)
    (let ((text (with-output-to-string (backtrace))))
      (dolist (line (split-string text "\n" t))
        (nelisp-emacs-magit-bridge--trace "%s %s" prefix line)))))

(defun nelisp-emacs-magit-bridge--classinv (phase n)
  "Emit a transient class inventory for PHASE (a string) at part N."
  (let ((line nil))
    (condition-case nil
        (let ((class-parents-fn
               (cond
                ((fboundp 'eieio--class-parents) 'eieio--class-parents)
                ((fboundp 'eieio-class-parents) 'eieio-class-parents)))
              (class-slots-fn
               (cond
                ((fboundp 'eieio--class-slots) 'eieio--class-slots)
                ((fboundp 'eieio-class-slots) 'eieio-class-slots)))
              (parts nil))
          (dolist (entry '((tdt . transient-describe-target)
                           (suffix . transient-suffix)
                           (child . transient-child)
                           (preset . transient-value-preset)
                           (prefix . transient-prefix)
                           (group . transient-group)))
            (let* ((label (car entry))
                   (class-name (cdr entry))
                   (class
                    (condition-case nil
                        (if (fboundp 'find-class)
                            (find-class class-name nil)
                          'ERR)
                      (error 'ERR)))
                   (value
                    (cond
                     ((null class) "nil")
                     ((eq class 'ERR) "ERR")
                     (t
                      (let ((slots
                             (condition-case nil
                                 (if class-slots-fn
                                     (length (append (funcall class-slots-fn class) nil))
                                   'ERR)
                               (error 'ERR)))
                            (parents
                             (condition-case nil
                                 (if class-parents-fn
                                     (length (append (funcall class-parents-fn class) nil))
                                   'ERR)
                               (error 'ERR))))
                        (format "%s/%s" slots parents))))))
              (push (format " %s=%s" label value) parts)))
          (setq line
                (format "CLASSINV %s part=%s%s\n"
                        phase n (apply #'concat (nreverse parts)))))
      (error
       (setq line
             (format "CLASSINV %s part=%s tdt=ERR suffix=ERR child=ERR preset=ERR prefix=ERR group=ERR\n"
                     phase n))))
    (when (and line (fboundp 'nelisp--write-stdout-bytes))
      (condition-case nil
          (nelisp--write-stdout-bytes line)
        (error nil)))
    ;; Symbol invariant sweep at every part boundary (2026-08-13).
    ;;
    ;; The read-side guard only reports a corrupted symbol once something reads
    ;; its name: across seven magit loads it fired once.  This asks the heap
    ;; directly, so a corruption is caught whether or not anything looks at it.
    ;; The seventh field is the cap/=len count and the sixth is how many symbols
    ;; were examined -- a zero is only meaningful beside that second number.
    ;;
    ;; The sweep allocates nothing per symbol; it walks existing blocks.  That
    ;; matters: a probe that allocated at these same boundaries already changed
    ;; the failure it was watching from a named void-function into a silent
    ;; abort, so anything added here has to stay out of the allocator.
    (when (fboundp 'nelisp--arena-walk-verify)
      (condition-case nil
          (let ((wv (nelisp--arena-walk-verify)))
            (nelisp--write-stdout-bytes
             (format "SWEEP %s part=%s symbols=%s cap/=len=%s\n"
                     phase n (nth 5 wv) (nth 6 wv))))
        (error nil)))))

(defun nelisp-emacs-magit-bridge--repo-root ()
  "Return the resolved repository root."
  (or nelisp-emacs-magit-bridge-repo-root
      (expand-file-name ".." (file-name-directory
                              (or (and (boundp 'load-file-name) load-file-name)
                                  (and (boundp 'buffer-file-name) buffer-file-name)
                                  default-directory "")))))

(defun nelisp-emacs-magit-bridge--bundle-file ()
  "Return the resolved bundle file path."
  (or nelisp-emacs-magit-bridge-bundle-file
      (expand-file-name "build/nelisp-emacs-magit-bridge-bundle.el"
                        (nelisp-emacs-magit-bridge--repo-root))))

(defun nelisp-emacs-magit-bridge--ensure-process-substrate ()
  "Ensure the sync process substrate is loaded before Magit needs git.

Magit's git plumbing (`magit-git-string', `magit-process-file', ...)
goes through `call-process'/`process-file'.  The interactive/batch nemacs
bootstrap already provides these; this is a defensive no-op there and a
real `require' when the bridge runs against a bare NeLisp session that
has not loaded the bootstrap (e.g. a from-scratch `nelisp --repl' spike)."
  (unless (featurep 'emacs-process)
    (require 'emacs-process))
  (unless (featurep 'emacs-process-builtins)
    (require 'emacs-process-builtins)))

(defun nelisp-emacs-magit-bridge--ensure-standalone-runtime ()
  "Ensure standalone mode detection is active under the NeLisp runtime.

The baked runtime image can carry `emacs-standalone.el' state captured from
host-Emacs build time, which leaves `emacs-standalone-mode-p' false even
though the live runtime has native `nelisp-process-*' primitives.  In that
state the process facade delegates to the legacy synchronous path and leaks
git stdout to the parent instead of capturing it into Magit's temp buffers.
When the native NeLisp process primitives are present, force standalone mode
on for this session so the shared capture path is used."
  (let ((root (nelisp-emacs-magit-bridge--repo-root)))
    (unless (fboundp 'emacs-standalone-mode-p)
      (load (expand-file-name "src/emacs-standalone.el" root)
            nil 'no-message t t))
    (when (and (boundp 'emacs-standalone-force-mode)
               (or (fboundp 'nelisp-process-start)
                   (fboundp 'nelisp-process-start-process)))
      (setq emacs-standalone-force-mode t))))

(defun nelisp-emacs-magit-bridge--ensure-search-substrate ()
  "Ensure buffer regexp/search builtins are backed by the shared substrate."
  (let ((root (nelisp-emacs-magit-bridge--repo-root)))
    (unless (featurep 'nelisp-regex)
      (load (expand-file-name "src/nelisp-regex.el" root)
            nil 'no-message t t))
    (unless (featurep 'emacs-search-builtins)
      (load (expand-file-name "src/emacs-search-builtins.el" root)
            nil 'no-message t t))))

(defun nelisp-emacs-magit-bridge--ensure-list-runtime ()
  "Ensure record-aware list/copy helpers are loaded before EIEIO object creation."
  (let ((root (nelisp-emacs-magit-bridge--repo-root)))
    (unless (featurep 'emacs-list)
      (load (expand-file-name "src/emacs-list.el" root)
            nil 'no-message t t))))

(defun nelisp-emacs-magit-bridge--ensure-symbol-runtime ()
  "Ensure symbol helpers such as `intern-soft' are available."
  (let ((root (nelisp-emacs-magit-bridge--repo-root)))
    (unless (fboundp 'intern-soft)
      (load (expand-file-name "src/emacs-symbol.el" root)
            nil 'no-message t t))))

(defun nelisp-emacs-magit-bridge--ensure-callproc-runtime ()
  "Ensure environment helpers used by Magit process paths are available."
  (let ((root (nelisp-emacs-magit-bridge--repo-root)))
    (unless (and (boundp 'process-environment)
                 (fboundp 'getenv)
                 (fboundp 'setenv))
      (load (expand-file-name "src/emacs-callproc.el" root)
            nil 'no-message t t))))

(defun nelisp-emacs-magit-bridge--ensure-fileio-runtime ()
  "Ensure file/buffer directory globals used by Magit are available.

`default-directory' must be a real special variable for dynamic `let'
binding and per-buffer swapping to work across Magit's status/stage
workflow.  Real Emacs gets that from preloaded `files.el'; this
runtime needs the shared fileio owner loaded explicitly."
  (let ((root (nelisp-emacs-magit-bridge--repo-root)))
    (unless (boundp 'default-directory)
      (load (expand-file-name "src/emacs-fileio.el" root)
            nil 'no-message t t))
    (unless (boundp 'default-directory)
      (defvar default-directory "/"))))

(defun nelisp-emacs-magit-bridge--ensure-subr-extra-primitives ()
  "Ensure modern `subr' helpers are really present, not just phantom-provided.

Some runtime images mark `subr-x' as provided while still missing the
`emacs-subr-extras' helper family that owns `length=' / `length<' /
`length>' on this substrate.  Magit and Transient call those helpers
directly.  Load the shared substrate owner explicitly and fail with a
named missing-symbol list if the runtime still does not satisfy that
contract."
  (let ((root (nelisp-emacs-magit-bridge--repo-root))
        missing)
    (unless (featurep 'emacs-subr-extras)
      (load (expand-file-name "src/emacs-subr-extras.el" root)
            nil 'no-message t t))
    (dolist (symbol '(length= length< length>))
      (unless (fboundp symbol)
        (push symbol missing)))
    (when missing
      (error "nelisp-emacs-magit-bridge: missing subr helpers after emacs-subr-extras load: %S"
             (nreverse missing)))))

(defun nelisp-emacs-magit-bridge--ensure-time-runtime ()
  "Ensure time helpers used by Magit refresh are available."
  (let ((root (nelisp-emacs-magit-bridge--repo-root)))
    (unless (and (fboundp 'float-time)
                 (fboundp 'current-time)
                 (fboundp 'current-time-string))
      (load (expand-file-name "src/emacs-time.el" root)
            nil 'no-message t t))))

(defun nelisp-emacs-magit-bridge--ensure-emacs-version-identity ()
  "Ensure `emacs-version'/-major-/-minor-version' hold real version strings.

The nemacs batch/interactive bootstrap leaves `emacs-version' as an
unbound-marker placeholder rather than a real dotted version string
(a substrate gap, not a Magit-specific one).  Compat's own
`compat-require' macro evaluates `(version< emacs-version VERSION)' at
*macro-expansion time* while loading compat-31/compat-30 layers; with
the placeholder value that comparison spuriously succeeds and the file
tries to require a compat layer this repo's vendor snapshot never
needed to carry (real host Emacs 30.1 never loads it either, since the
same guard is false there).  Declaring a real, current identity here
keeps that guard's real-Emacs behavior; this never overwrites a
already-real value, so it becomes a no-op once the nemacs substrate
sets one itself."
  (unless (and (boundp 'emacs-version)
               (stringp emacs-version))
    (setq emacs-version "30.1"))
  (unless (and (boundp 'emacs-major-version)
               (integerp emacs-major-version))
    (setq emacs-major-version 30))
  (unless (and (boundp 'emacs-minor-version)
               (integerp emacs-minor-version))
    (setq emacs-minor-version 1)))

(defun nelisp-emacs-magit-bridge--ensure-buffer-defaults ()
  "Ensure `buffer-read-only' defaults to nil, matching real Emacs.

After `nemacs-init', this repo's batch/interactive bootstrap leaves the
global default value of `buffer-read-only' at `t' (another substrate
gap, not Magit-specific): every freshly-created buffer, including the
ones `with-temp-buffer' creates, inherits it and any `insert' into that
buffer signals `text-read-only'.  Compat's own docstring formatting
helper (`compat-macs--docstring') builds its formatted string inside a
`with-temp-buffer', so this alone was enough to silently break the
first vendor file in the chain (`compat-31.el') before Magit is ever
reached.  Restoring the real-Emacs default here is a session-level
precondition fix, not a vendor patch."
  (unless (eq (default-value 'buffer-read-only) nil)
    (setq-default buffer-read-only nil)))

(defun nelisp-emacs-magit-bridge--ensure-static-if ()
  "Ensure `static-if' (Emacs 29's own preloaded macro) is available.

Real Emacs 29+ defines `static-if' in `subr.el' as part of the dumped,
always-preloaded core, so it never shows up as a `require'd feature and
Compat never needs to shim it itself.  NeLisp's substrate does not
preload it, so `compat-31.el' (which uses `static-if' directly, relying
on the host to already provide it) hits a `void-function' otherwise.
Copied verbatim from `subr.el' (not reimplemented) and only installed
when absent."
  (unless (fboundp 'static-if)
    (defmacro static-if (condition then-form &rest else-forms)
      "A conditional compilation macro.
Evaluate CONDITION at macro-expansion time.  If it is non-nil,
expand the macro to THEN-FORM.  Otherwise expand it to ELSE-FORMS
enclosed in a `progn' form.  ELSE-FORMS may be empty."
      (declare (indent 2) (debug (sexp sexp &rest sexp)))
      (if (eval condition lexical-binding)
          then-form
        (cons 'progn else-forms)))))

(defun nelisp-emacs-magit-bridge--ensure-cl-generic-define-generalizer ()
  "Ensure `cl-generic-define-generalizer' (real Emacs `cl-generic.el') exists.

NeLisp's native `cl-generic'/`cl-defmethod'/`cl-generic-make-generalizer'
substrate is otherwise present (P0 in Doc 33), but this one small
convenience macro around `cl-generic-make-generalizer' is missing;
EIEIO's `eieio-core.el' uses it directly to teach `cl-defmethod'
dispatch about EIEIO classes (`magit-section' and friends).  Copied
verbatim from `cl-generic.el' (not reimplemented) and only installed
when absent."
  (unless (fboundp 'cl-generic-define-generalizer)
    (defmacro cl-generic-define-generalizer
        (name priority tagcode-function specializers-function)
      "Define a new kind of generalizer.
NAME is the name of the variable that will hold it.
PRIORITY defines which generalizer takes precedence.
  The catch-all generalizer has priority 0.
  Then `eql' generalizer has priority 100.
TAGCODE-FUNCTION takes as first argument a varname and should return
  a chunk of code that computes the tag of the value held in that variable.
  Further arguments are reserved for future use.
SPECIALIZERS-FUNCTION takes as first argument a tag value TAG
  and should return a list of specializers that match TAG.
  Further arguments are reserved for future use."
      (declare (indent 1) (debug (symbolp body)))
      `(defconst ,name
         (cl-generic-make-generalizer
          ',name ,priority ,tagcode-function ,specializers-function)))))

(defun nelisp-emacs-magit-bridge--ensure-cl-generic-runtime-shims ()
  "Load the local CL runtime shims that real `cl-generic.el' expects.

The runtime bootstrap phantom-provides `cl-generic' but does not install
the full built-in-class lattice or OClosure support that the real vendor
`cl-generic.el' / `eieio-core.el' path relies on.  These shims already live
in this repo and are the documented bridge substrate for loading the real
vendor CL object system; this function just makes sure they are present
before the bundle reaches `cl-generic.el'."
  (let ((root (nelisp-emacs-magit-bridge--repo-root)))
    (nelisp-emacs-magit-bridge--precond-trace "SHIM 1 before cl-preloaded\n")
    (unless (featurep 'cl-preloaded-shim)
      (load (expand-file-name "src/cl-preloaded-shim.el" root)
            nil 'no-message t t))
    (nelisp-emacs-magit-bridge--precond-trace "SHIM 2 after cl-preloaded\n")
    (nelisp-emacs-magit-bridge--precond-trace "SHIM 3 before oclosure\n")
    (unless (featurep 'oclosure-shim)
      (load (expand-file-name "src/oclosure-shim.el" root)
            nil 'no-message t t))
    (nelisp-emacs-magit-bridge--precond-trace "SHIM 4 done\n")))

(defun nelisp-emacs-magit-bridge--ensure-cl-next-method-helpers ()
  "Ensure the global next-method helpers from real `cl-generic.el' exist.

The runtime's lightweight `cl-generic' surface can dispatch methods but leaves
`cl-call-next-method' and `cl-next-method-p' unbound.  Real `cl-generic.el'
defines these as global placeholders that error outside a method body, while
its internal `cl-flet' machinery rebinds them inside applicable methods.
Installing the exact global definitions is enough to let that existing local
rebinding work on Magit's `transient-prefix-value' methods."
  (unless (fboundp 'cl-call-next-method)
    (defun cl-call-next-method (&rest _args)
      "Function to call the next applicable method.
Can only be used from within the lexical body of a primary or around method."
      (error "cl-call-next-method only allowed inside primary and around methods")))
  (unless (fboundp 'cl-next-method-p)
    (defun cl-next-method-p ()
      "Return non-nil if there is a next method.
Can only be used from within the lexical body of a primary or around method."
      (declare (obsolete "make sure there's always a next method, or catch `cl-no-next-method' instead" "25.1"))
      (error "cl-next-method-p only allowed inside primary and around methods"))))

(defun nelisp-emacs-magit-bridge--ensure-cl-find-class-setter ()
  "Ensure `setf' can write `(cl--find-class ...)' in a live session.

The standalone runtime loses `cl-simple-setter' properties baked into the
image, so vendor `defclass' forms can run without ever persisting
`(setf (cl--find-class NAME) CLASS)'.  Reinstall the setter before loading the
bundle so EIEIO class registration survives."
  (when (fboundp 'cl--set-find-class)
    (put 'cl--find-class 'cl-simple-setter 'cl--set-find-class)))

(defun nelisp-emacs-magit-bridge--ensure-eieio-core-structs ()
  "Ensure the core EIEIO class struct exists before vendor classes load.

The current runtime can load most of `eieio-core.el' while still silently
dropping the `cl-defstruct' that should create `eieio--class' and its
  constructor/accessors.  Magit's section fallback path only needs that core
class object shape plus the accessor family, and the local `cl-defstruct'
shim already expands this form correctly under the standalone reader."
  (let ((root (nelisp-emacs-magit-bridge--repo-root)))
    (unless (and (fboundp 'cl-defstruct)
                 (fboundp 'cl--class-p)
                 (fboundp 'cl--make-slot-descriptor))
      (load (expand-file-name "src/emacs-cl-macros.el" root)
            nil 'no-message t t))
    ;; `src/emacs-parity-eieio.el' registers the eieio accessor -> record
    ;; slot indices that NeLisp's `setf' consults; without it the vendor
    ;; chain's `(setf (eieio--class-parents ...) ...)' places expand to
    ;; `("setf: unsupported place" eieio--class-parents)' at macroexpansion
    ;; time (measured 2026-08-03).  The registration must happen before the
    ;; vendor eieio files are read, and nothing else loads this shim.
    (unless (featurep 'emacs-parity-eieio)
      (load (expand-file-name "src/emacs-parity-eieio.el" root)
            nil 'no-message t t))
    ;; The shim records `:include' parents only at macro-expansion time and
    ;; emits `cl-struct-setter' as top-level `put' forms; neither survives a
    ;; baked runtime image, so record `setf' silently no-ops on cons-backed
    ;; class objects rather than signalling (measured 2026-08-05).
    (when (and (boundp 'emacs-cl-macros--struct-defs)
               (not (assq 'cl--class emacs-cl-macros--struct-defs)))
      (eval
       '(cl-defstruct (cl--class
                       (:constructor nil)
                       (:copier nil))
          (name nil)
          (docstring nil)
          (parents nil)
          (slots nil)
          (index-table nil))
       t))
    (unless (boundp 'eieio-default-superclass)
      (defvar eieio-default-superclass nil))
    (unless (and (fboundp 'eieio--class-make)
                 (or (not (boundp 'emacs-cl-macros--struct-defs))
                     (memq 'name
                           (car (cdr (assq
                                      'eieio--class
                                      emacs-cl-macros--struct-defs))))))
      (eval
       '(cl-defstruct (eieio--class
                       (:constructor nil)
                       (:constructor eieio--class-make (name))
                       (:include cl--class)
                       (:copier nil))
          children
          initarg-tuples
          (class-slots nil)
          class-allocation-values
          default-object-cache
          options)
       t))
    (unless (boundp 'eieio--object-num-slots)
      (setq eieio--object-num-slots 1))
    (unless (fboundp 'eieio--object-class-tag)
      (defun eieio--object-class-tag (obj)
        (aref obj 0)))))

(defun nelisp-emacs-magit-bridge--ensure-pcase-runtime ()
  "Ensure the local pcase runtime polyfill is fully installed.

The runtime image can phantom-provide `pcase' while still missing macros such
as `pcase-exhaustive'.  Magit's buffer setup hits that macro directly."
  (let ((root (nelisp-emacs-magit-bridge--repo-root)))
    (unless (fboundp 'pcase-exhaustive)
      (load (expand-file-name "src/emacs-pcase.el" root)
            nil 'no-message t t))))

(defun nelisp-emacs-magit-bridge--ensure-macroexp-runtime ()
  "Ensure the macroexp runtime Magit/transient macros expand through.

The runtime image can execute real vendor files while still missing small
dumped helper functions that upstream macro code assumes are always
available.  `macroexp-progn' is one of those helpers and Magit's git path
still reaches it at runtime via vendor macros.  Install the real behavior
when absent.

`macroexp-let2' needs stronger treatment: `emacs-stub-bulk.el' installs it
through `(defmacro NAME (&rest _) nil)', which DISCARDS the caller's body,
and no real definition exists elsewhere in the tree.  Vendor `eieio.el'
expands `with-slots' through `macroexp-let2', so every `with-slots' form in
the vendor chain silently evaluated to nil (measured 2026-08-03: the symbol
carries the `emacs-stub-bulk' property in the baked image).  The vendored
`macroexp.el' loads cleanly here, so replace the stub with the real file.
The guard tests that property, not `fboundp': the stub is already bound."
  (unless (fboundp 'macroexp-progn)
    (defun macroexp-progn (body)
      "Return BODY as one expression, preserving side-effect order."
      (cond
       ((null body) nil)
       ((null (cdr body)) (car body))
       (t (cons 'progn body)))))
  (when (get 'macroexp-let2 'emacs-stub-bulk)
    (let ((root (nelisp-emacs-magit-bridge--repo-root)))
      (load (expand-file-name "vendor/emacs-lisp/emacs-lisp/macroexp.el" root)
            nil 'no-message t t))))

(defun nelisp-emacs-magit-bridge--ensure-advice-runtime ()
  "Ensure `add-function' / `remove-function' advice helpers exist."
  (let ((root (nelisp-emacs-magit-bridge--repo-root)))
    (unless (and (fboundp 'add-function)
                 (fboundp 'remove-function))
      (load (expand-file-name "src/emacs-stub.el" root)
            nil 'no-message t t))
    (unless (fboundp 'add-function)
      (defmacro add-function (how place function &optional props)
        "Standalone subset of `nadvice.el' `add-function'."
        (cond
         ((and (consp place)
               (eq (car place) 'local)
               (eq (car-safe (cadr place)) 'quote))
          (list 'emacs-stub--add-function-symbol how (cadr place) function props t))
         ((and (consp place)
               (eq (car place) 'var))
          (list 'setq (cadr place)
                (list 'emacs-stub--add-function-value
                      how (cadr place) function props)))
         ((symbolp place)
          (list 'emacs-stub--add-function-symbol
                how (list 'quote place) function props nil))
         (t nil))))
    (unless (fboundp 'remove-function)
      (defmacro remove-function (place function)
        "Standalone subset of `nadvice.el' `remove-function'."
        (cond
         ((and (consp place)
               (eq (car place) 'local)
               (eq (car-safe (cadr place)) 'quote))
          (list 'emacs-stub--remove-function-symbol (cadr place) function t))
         ((and (consp place)
               (eq (car place) 'var))
          (list 'setq (cadr place)
                (list 'emacs-stub--remove-function-value (cadr place) function)))
         ((symbolp place)
          (list 'emacs-stub--remove-function-symbol
                (list 'quote place) function nil))
         (t nil))))))

(defun nelisp-emacs-magit-bridge--ensure-transient-globals ()
  "Ensure host-preloaded transient persistence globals exist.

The vendor transient file expects these names to be real specials with
persisted defaults.  This runtime does not model that persistence layer yet,
so nil/ordinary defaults are the honest baseline."
  (dolist (spec '((transient-values-file . "")
                  (transient-levels-file . "")
                  (transient-history-file . "")
                  (transient-history-limit . 10)
                  (transient-save-history . t)
                  (transient-values . nil)
                  (transient-levels . nil)
                  (transient-history . nil)))
    (unless (boundp (car spec))
      (eval (list 'defvar (car spec) (list 'quote (cdr spec))) t))))

(defun nelisp-emacs-magit-bridge--ensure-message-runtime ()
  "Ensure message/current-message behave like a minimal echo-area facade."
  (unless (fboundp 'message)
    (defun message (format-string &rest args)
      "Standalone fallback for `message'."
      (let ((s (apply #'format format-string args)))
        (setq nelisp-emacs-magit-bridge--current-message s)
        (princ s)
        (princ "\n")
        s)))
  (unless (fboundp 'current-message)
    (defun current-message ()
      "Return the most recent bridge message, or nil."
      nelisp-emacs-magit-bridge--current-message))
  ;; If a native/stub `message' already exists, wrap it so
  ;; `current-message' still reflects what callers last displayed.
  (unless (advice-member-p #'nelisp-emacs-magit-bridge--message-capture 'message)
    (defun nelisp-emacs-magit-bridge--message-capture (orig format-string &rest args)
      (let ((s (apply #'format format-string args)))
        (setq nelisp-emacs-magit-bridge--current-message s)
        (apply orig format-string args)))
    (advice-add 'message :around #'nelisp-emacs-magit-bridge--message-capture)))

(defun nelisp-emacs-magit-bridge--ensure-transient-init-scope-safe ()
  "Replace generic `transient-init-scope' dispatch with a safe plain function."
  (when (featurep 'transient)
    (defun transient-init-scope (obj)
      "Standalone-safe scope initialization for transient prefix/suffix objects."
      (if (and (boundp 'transient--prefix)
               (ignore-errors (cl-typep obj 'magit--git-variable)))
          (eieio-oset obj 'scope
                      (cond
                       (transient--prefix
                        (eieio-oref transient--prefix 'scope))
                       ((slot-boundp obj 'scope)
                        (let ((scope (eieio-oref obj 'scope)))
                          (and (functionp scope)
                               (funcall scope obj))))))
        nil))))

(defun nelisp-emacs-magit-bridge--ensure-transient-init-value-safe ()
  "Replace fragile transient value/return generics with plain functions."
  (when (featurep 'transient)
    (defun transient-init-value (obj)
      "Standalone-safe value initialization for transient objects."
      (let ((init-value (and (ignore-errors (slot-boundp obj 'init-value))
                             (ignore-errors (eieio-oref obj 'init-value)))))
        (cond
         ((functionp init-value)
          (funcall init-value obj))
         ((ignore-errors (cl-typep obj 'transient-prefix))
          (unless (and (ignore-errors (slot-boundp obj 'value))
                       (ignore-errors (eieio-oref obj 'value)))
            (eieio-oset
             obj 'value
             (if-let ((saved (assq (eieio-oref obj 'command) transient-values)))
                 (cdr saved)
               (if (fboundp 'transient-default-value)
                   (transient-default-value obj)
                 nil)))))
         (t nil))))
    (defun transient-init-return (obj)
      "Standalone-safe return initialization for transient prefix objects."
      (when (and (boundp 'transient--stack)
                 transient--stack)
        (let* ((command (ignore-errors (eieio-oref obj 'command)))
               (suffix-obj (and command
                                (fboundp 'transient-suffix-object)
                                (ignore-errors
                                  (transient-suffix-object command))))
               (transient
                (cond
                 ((and suffix-obj
                       (ignore-errors (slot-boundp suffix-obj 'transient)))
                  (ignore-errors (eieio-oref suffix-obj 'transient)))
                 ((and (boundp 'transient-current-prefix)
                       transient-current-prefix)
                  (ignore-errors
                    (eieio-oref transient-current-prefix 'transient-suffix))))))
          (when (memq transient (list t 'recurse #'transient--do-recurse))
            (ignore-errors
              (eieio-oset obj 'return t))))))))

(defun nelisp-emacs-magit-bridge--ensure-transient-setup-children-safe ()
  "Replace generic `transient-setup-children' with a plain function."
  (when (featurep 'transient)
    (defun transient-setup-children (group children)
      "Standalone-safe child setup for transient GROUP."
      (let ((setup (and (ignore-errors (slot-boundp group 'setup-children))
                        (ignore-errors
                          (eieio-oref group 'setup-children)))))
        (if (functionp setup)
            (funcall setup children)
          children)))))

(defun nelisp-emacs-magit-bridge--ensure-transient-suffix-key-safe ()
  "Replace generic `transient--init-suffix-key' with a plain function."
  (when (featurep 'transient)
    (defun transient--init-suffix-key (obj)
      "Standalone-safe key initialization for transient suffix objects."
      (when (ignore-errors (cl-typep obj 'transient-argument))
        (unless (ignore-errors (slot-boundp obj 'shortarg))
          (let ((argument (ignore-errors (eieio-oref obj 'argument))))
            (when (and (stringp argument)
                       (fboundp 'transient--derive-shortarg))
              (let ((shortarg (transient--derive-shortarg argument)))
                (when shortarg
                  (ignore-errors
                    (eieio-oset obj 'shortarg shortarg)))))))
        (unless (ignore-errors (slot-boundp obj 'key))
          (let ((shortarg (and (ignore-errors (slot-boundp obj 'shortarg))
                               (ignore-errors (eieio-oref obj 'shortarg)))))
            (when shortarg
              (ignore-errors
                (eieio-oset obj 'key shortarg))))))
      (unless (ignore-errors (slot-boundp obj 'key))
        (error "No key for %s" (ignore-errors (eieio-oref obj 'command)))))))

(defun nelisp-emacs-magit-bridge--ensure-transient-setup-safe ()
  "Install a small standalone-safe `transient-setup'."
  (when (featurep 'transient)
    (defun transient-setup (&optional name layout edit &rest params)
      "Standalone-safe transient setup for prefix commands.
This initializes prefix/suffix objects and materializes a transient buffer and
window without entering the full keymap/history/redisplay path."
      (ignore edit)
      (unless name
        (setq name (and (boundp 'transient--prefix)
                        transient--prefix
                        (ignore-errors
                          (eieio-oref transient--prefix 'command)))))
      (transient--init-objects name layout params)
      (setq transient--buffer
            (get-buffer-create
             (if (and (boundp 'transient--buffer-name)
                      transient--buffer-name)
                 transient--buffer-name
               " *transient*")))
      (with-current-buffer transient--buffer
        (erase-buffer)
        (insert (format "%S\n" name))
        (dolist (suffix transient--suffixes)
          (let ((key (ignore-errors (eieio-oref suffix 'key)))
                (command (ignore-errors (eieio-oref suffix 'command))))
            (when command
              (insert (format "%s %S\n" (or key "") command))))))
      (setq transient--window (display-buffer transient--buffer))
      transient--prefix)))

(defun nelisp-emacs-magit-bridge--eieio-class-initarg-map (class)
  "Return CLASS initargs as an alist of (SLOT . INITARG)."
  (let ((pairs nil))
    (dolist (entry (ignore-errors (append (eieio--class-initarg-tuples class) nil)))
      (let ((initarg (car entry))
            (slot (cdr entry)))
        (when (and (symbolp slot)
                   (keywordp initarg)
                   (not (assq slot pairs)))
          (push (cons slot initarg) pairs))))
    (nreverse pairs)))

(defun nelisp-emacs-magit-bridge--transient-clone-instance (object &rest initargs)
  "Clone transient OBJECT into a fresh instance, applying INITARGS last."
  (let* ((class (eieio--object-class object))
         (class-name (ignore-errors (eieio-object-class-name object)))
         (slot-initargs (nelisp-emacs-magit-bridge--eieio-class-initarg-map class)))
    ;; Passing every copied slot back through the EIEIO constructor is
    ;; pathological in the standalone runtime for `transient-prefix'.  Build a
    ;; plain instance and copy slots directly, then apply caller overrides.
    (unless class-name
      (error "Cannot clone object without EIEIO class name: %S" object))
    (let ((clone (funcall class-name)))
      (dolist (desc (ignore-errors (append (eieio--class-slots class) nil)))
        (let ((slot (ignore-errors (cl--slot-descriptor-name desc))))
          (when (and slot
                     (ignore-errors (slot-boundp object slot)))
            (ignore-errors
              (eieio-oset clone slot (eieio-oref object slot))))))
      (while initargs
        (let ((initarg (pop initargs))
              (value (pop initargs)))
          (dolist (entry slot-initargs)
            (when (eq (cdr entry) initarg)
              (ignore-errors
                (eieio-oset clone (car entry) value))))))
      clone)))

(defun nelisp-emacs-magit-bridge--ensure-transient-init-prefix-safe ()
  "Replace transient prefix cloning with a registry-safe instance copier."
  (when (featurep 'transient)
    (defun transient--init-prefix (name &optional params)
      (let* ((proto (get name 'transient--prefix))
             (obj (if proto
                      (apply #'nelisp-emacs-magit-bridge--transient-clone-instance
                             proto
                             :prototype proto
                             :level (or (alist-get t (alist-get name transient-levels))
                                        transient-default-level)
                             params)
                    (error "Missing transient prefix prototype: %S" name))))
        (transient-init-value obj)
        (transient-init-return obj)
        (transient-init-scope obj)
        obj))))

(defun nelisp-emacs-magit-bridge--ensure-transient-common-commands-ready ()
  "Keep standalone transient setup independent from shared common commands."
  (when (featurep 'transient)
    (unless (fboundp 'transient--kbd)
      (defun transient--kbd (keys)
        "Normalize transient KEYS into the internal key representation."
        (when (vectorp keys)
          (setq keys (key-description keys)))
        (when (stringp keys)
          (setq keys (kbd keys)))
        keys))))

(defun nelisp-emacs-magit-bridge--canonicalize-transient-layout (spec)
  "Convert raw transient layout SPEC into the runtime init shape."
  (cond
   ((null spec) nil)
   ((stringp spec) spec)
   ((and (symbolp spec) (boundp spec))
    (nelisp-emacs-magit-bridge--canonicalize-transient-layout
     (symbol-value spec)))
   ((vectorp spec)
    (let ((items (append spec nil)))
      (if (and items (integerp (car items)))
          (let* ((level (nth 0 items))
                 (class (nth 1 items))
                 (args (nth 2 items))
                 (children (nth 3 items)))
            (vector level
                    class
                    args
                    (mapcar #'nelisp-emacs-magit-bridge--canonicalize-transient-layout
                            children)))
        (let ((class (nth 0 items))
              (args (nth 1 items))
              (children (nth 2 items)))
          (vector transient--default-child-level
                  class
                  args
                  (mapcar #'nelisp-emacs-magit-bridge--canonicalize-transient-layout
                          children))))))
   ((listp spec)
    (cond
     ((and spec (integerp (car spec)))
      (list (nth 0 spec) (nth 1 spec) (nth 2 spec)))
     ((and spec (symbolp (car spec)))
      (list transient--default-child-level (car spec) (cdr spec)))
     (t
      (mapcar #'nelisp-emacs-magit-bridge--canonicalize-transient-layout
              spec))))
   (t spec)))

(defun nelisp-emacs-magit-bridge--ensure-transient-init-suffixes-safe ()
  "Avoid transient's shared common-command layout on the standalone path."
  (when (featurep 'transient)
    (defun transient--init-suffixes (name)
      (let* ((levels (alist-get name transient-levels))
             (children (transient--get-children name)))
        (when (and children
                   (let ((child (car children)))
                     (or (and (vectorp child)
                              (> (length child) 0)
                              (not (integerp (aref child 0))))
                         (and (listp child)
                              child
                              (not (integerp (car child)))))))
          (setq children
                (mapcar #'nelisp-emacs-magit-bridge--canonicalize-transient-layout
                        children)))
        (cl-mapcan (lambda (child)
                     (transient--init-child levels child nil))
                   children)))))

(defun nelisp-emacs-magit-bridge--ensure-transient-child-init-safe ()
  "Replace transient child/group init helpers with binding-stable equivalents."
  (when (featurep 'transient)
    (defun transient--init-group (levels spec parent)
      (let* ((items (append spec nil))
             (level (nth 0 items))
             (class (nth 1 items))
             (args (nth 2 items))
             (children (nth 3 items)))
        (and-let* (((transient--use-level-p level))
                   (obj (apply class :level level args))
                   ((transient--use-suffix-p obj))
                   ((prog1 t
                      (when (or (and parent (eieio-oref parent 'inapt))
                                (transient--inapt-suffix-p obj))
                        (eieio-oset obj 'inapt t))))
                   (suffixes
                    (cl-mapcan
                     (lambda (child)
                       (transient--init-child levels child obj))
                     (transient-setup-children obj children))))
          (eieio-oset obj 'suffixes suffixes)
          (list obj))))
    (defun transient--init-suffix (levels spec parent)
      (let* ((level (nth 0 spec))
             (class (nth 1 spec))
             (args (nth 2 spec))
             (cmd (plist-get args :command))
             (key (transient--kbd (plist-get args :key)))
             (level (or (alist-get (cons cmd key) levels nil nil #'equal)
                        (alist-get cmd levels)
                        level)))
        (let ((fn (and (symbolp cmd)
                       (symbol-function cmd))))
          (when (autoloadp fn)
            (transient--debug "   autoload %s" cmd)
            (autoload-do-load fn)))
        (when (transient--use-level-p level)
          (let ((obj
                 (if (child-of-class-p class 'transient-information)
                     (apply class :level level args)
                   (unless (and cmd (symbolp cmd))
                     (error "BUG: Non-symbolic suffix command: %s" cmd))
                   (if-let ((proto (and cmd (transient--suffix-prototype cmd))))
                       (apply #'nelisp-emacs-magit-bridge--transient-clone-instance
                              proto :level level args)
                     (apply class :command cmd :level level args)))))
            (cond
             ((not cmd))
             ((commandp cmd))
             ((or (cl-typep obj 'transient-switch)
                  (cl-typep obj 'transient-option))
              (defalias cmd #'transient--default-infix-command))
             ((transient--use-suffix-p obj)
              (error "Suffix command %s is not defined or autoloaded" cmd)))
            (unless (cl-typep obj 'transient-information)
              (transient--init-suffix-key obj))
            (when (transient--use-suffix-p obj)
              (if (or (and parent (eieio-oref parent 'inapt))
                      (transient--inapt-suffix-p obj))
                  (eieio-oset obj 'inapt t)
                (transient-init-scope obj)
                (transient-init-value obj))
              (list obj))))))))

(defun nelisp-emacs-magit-bridge--transient-class-name (object)
  "Return OBJECT's EIEIO class symbol when available."
  (and (ignore-errors (eieio-object-p object))
       (ignore-errors (eieio-object-class-name object))))

(defun nelisp-emacs-magit-bridge--transient-class-symbol-p (class-name)
  "Return non-nil when CLASS-NAME names a transient class."
  (and (symbolp class-name)
       (string-prefix-p "transient-" (symbol-name class-name))))

(defun nelisp-emacs-magit-bridge--transient-group-object-p (object)
  "Return non-nil when OBJECT behaves like a transient group."
  (memq (nelisp-emacs-magit-bridge--transient-class-name object)
        '(transient-group transient-row transient-column
          transient-columns transient-subgroups)))

(defun nelisp-emacs-magit-bridge--transient-suffix-object-p (object)
  "Return non-nil when OBJECT behaves like a transient suffix."
  (memq (nelisp-emacs-magit-bridge--transient-class-name object)
        '(transient-suffix transient-infix transient-option transient-switch
          transient-switches transient-argument transient-files
          transient-value transient-value-preset transient-information
          transient-describe-target)))

(defun nelisp-emacs-magit-bridge--ensure-transient-layout-walkers-safe ()
  "Replace transient layout walkers with EIEIO-slot-safe equivalents."
  (when (featurep 'transient)
    (defun transient--flatten-suffixes (layout)
      "Return every suffix object reachable from transient LAYOUT."
      (nreverse
       (letrec ((flatten
                 (lambda (def)
                   (cond
                    ((null def) nil)
                    ((stringp def) nil)
                    ((listp def)
                     (cl-mapcan flatten def))
                    ((nelisp-emacs-magit-bridge--transient-group-object-p def)
                     (cl-mapcan flatten (or (ignore-errors (oref def suffixes))
                                            nil)))
                    ((nelisp-emacs-magit-bridge--transient-suffix-object-p def)
                     (list def))
                    (t nil)))))
         (funcall flatten layout))))
    (defun transient--active-suffixes (group)
      "Return active suffixes of GROUP, or nil when GROUP is not a group."
      (let ((suffixes (if (nelisp-emacs-magit-bridge--transient-group-object-p
                           group)
                          (or (ignore-errors (oref group suffixes)) nil)
                        nil)))
        (seq-remove
         (lambda (suffix)
           (and (nelisp-emacs-magit-bridge--transient-suffix-object-p suffix)
                (ignore-errors (oref suffix inactive))))
         suffixes)))))

(defun nelisp-emacs-magit-bridge--ensure-transient-command-key-safe ()
  "Replace `transient--command-key' with a slot-safe variant.

Some transient suffix prototypes are plain `transient-suffix' objects and do
not have a `shortarg' slot.  On this runtime, the vendor
`slot-exists-p'/`slot-boundp' sequence is not reliable enough to protect the
later `oref', so treat missing slots as nil with `ignore-errors'."
  (when (featurep 'transient)
    (defun transient--command-key (cmd)
      (and-let* ((obj (transient--suffix-prototype cmd)))
        (cond
         ((ignore-errors (slot-boundp obj 'key))
          (ignore-errors (oref obj key)))
         ((ignore-errors (slot-exists-p obj 'shortarg))
          (if (ignore-errors (slot-boundp obj 'shortarg))
              (ignore-errors (oref obj shortarg))
            (and (ignore-errors (slot-boundp obj 'argument))
                 (transient--derive-shortarg
                  (ignore-errors (oref obj argument)))))))))))

(defun nelisp-emacs-magit-bridge--register-minimal-eieio-class
    (name slot-specs &optional parent-name)
  "Register a minimal EIEIO class NAME with SLOT-SPECS.
SLOT-SPECS is a list of (SLOT INITFORM INITARG).
PARENT-NAME defaults to `eieio-default-superclass'."
  (unless (ignore-errors (find-class name nil))
    (let* ((class (eieio--class-make name))
           (parent (cl--find-class (or parent-name 'eieio-default-superclass)))
           (parent-slots (append (ignore-errors (append (eieio--class-slots parent) nil))
                                 nil))
           (parent-initargs (append (ignore-errors (eieio--class-initarg-tuples parent))
                                    nil))
           (slots parent-slots)
           (local-slots nil)
           (initargs parent-initargs)
           (local-initargs nil)
           (index-table (make-hash-table :test #'eq))
           (i (length parent-slots)))
      (setf (cl--class-parents class) (list parent))
      (when-let ((parent-index (ignore-errors (eieio--class-index-table parent))))
        (maphash (lambda (slot-name slot-index)
                   (puthash slot-name slot-index index-table))
                 parent-index))
      (dolist (spec slot-specs)
        (let* ((slot-name (nth 0 spec))
               (initform (nth 1 spec))
               (initarg (nth 2 spec))
               (desc (cl--make-slot-descriptor slot-name initform t nil)))
          (push desc local-slots)
          (when initarg
            (push (cons initarg slot-name) local-initargs))
          (puthash slot-name (+ eieio--object-num-slots i) index-table)
          (setq i (1+ i))))
      (setq slots (apply #'vector (append slots (nreverse local-slots))))
      (setf (cl--find-class name) class)
      (setf (eieio--class-slots class) slots)
      (setf (eieio--class-initarg-tuples class)
            (append initargs (nreverse local-initargs)))
      (setf (eieio--class-class-slots class) [])
      (setf (eieio--class-class-allocation-values class) [])
      (setf (eieio--class-index-table class) index-table)
      (setf (eieio--class-options class) nil)
      (setf (eieio--class-default-object-cache class)
            (make-record class
                         (+ (length slots) eieio--object-num-slots -1)
                         nil))
      (unless (fboundp name)
        (fset name
              `(lambda (&rest slots)
                 (apply #'make-instance ',name slots))))
      class)))

(defun nelisp-emacs-magit-bridge--ensure-magit-section-class-manual ()
  "Register minimal Magit section classes when vendor `defclass' dropped them."
  (nelisp-emacs-magit-bridge--register-minimal-eieio-class
   'magit-section
   '((type nil :type)
     (keymap nil nil)
     (value nil nil)
     (start nil nil)
     (content nil nil)
     (end nil nil)
     (hidden nil nil)
     (painted nil nil)
     (washer nil :washer)
     (inserter nil nil)
     (selective-highlight nil :selective-highlight)
     (heading-highlight-face nil :heading-highlight-face)
     (heading-selection-face nil :heading-selection-face)
     (parent nil nil)
     (children nil nil)))
  (nelisp-emacs-magit-bridge--register-minimal-eieio-class
   'magit-diff-section
   '((keymap 'magit-diff-section-map nil))
   'magit-section)
  (nelisp-emacs-magit-bridge--register-minimal-eieio-class
   'magit-file-section
   '((keymap 'magit-file-section-map nil)
     (source nil :source)
     (header nil :header)
     (binary nil :binary)
     (heading-highlight-face 'magit-diff-file-heading-highlight nil)
     (heading-selection-face 'magit-diff-file-heading-selection nil))
   'magit-diff-section)
  (nelisp-emacs-magit-bridge--register-minimal-eieio-class
   'magit-module-section
   '((keymap 'magit-module-section-map nil)
     (range nil :range))
   'magit-file-section)
  (nelisp-emacs-magit-bridge--register-minimal-eieio-class
   'magit-hunk-section
   '((keymap 'magit-hunk-section-map nil)
     (painted nil nil)
     (fontified nil nil)
     (refined nil nil)
     (combined nil :combined)
     (from-range nil :from-range)
     (from-ranges nil nil)
     (to-range nil :to-range)
     (about nil :about)
     (heading-highlight-face 'magit-diff-hunk-heading-highlight nil)
     (heading-selection-face 'magit-diff-hunk-heading-selection nil))
   'magit-diff-section))

(defun nelisp-emacs-magit-bridge--ensure-transient-classes-manual ()
  "Register minimal transient classes when vendor `defclass' dropped them."
  (nelisp-emacs-magit-bridge--register-minimal-eieio-class
   'transient-object
   nil)
  (nelisp-emacs-magit-bridge--register-minimal-eieio-class
   'transient-prefix
   '((prototype nil :prototype)
     (command nil :command)
     (level nil :level)
     (init-value nil :init-value)
     (value nil :value)
     (default-value nil :value)
     (return nil :return)
     (scope nil :scope)
     (history nil :history)
     (history-pos 0 :history-pos)
     (history-key nil :history-key)
     (description nil :description)
     (show-help nil :show-help)
     (info-manual nil :info-manual)
     (man-page nil :man-page)
     (summary nil :summary)
     (transient-suffix nil :transient-suffix)
     (transient-non-suffix nil :transient-non-suffix)
     (transient-switch-frame nil :transient-switch-frame)
     (refresh-suffixes nil :refresh-suffixes)
     (remember-value nil :remember-value)
     (environment nil :environment)
     (incompatible nil :incompatible)
     (suffix-description nil :suffix-description)
     (display-action nil :display-action)
     (mode-line-format nil :mode-line-format)
     (variable-pitch nil :variable-pitch)
     (column-widths nil :column-widths)
     (unwind-suffix nil nil))
   'transient-object)
  (nelisp-emacs-magit-bridge--register-minimal-eieio-class
   'transient-child
   '((parent nil :parent)
     (level nil :level)
     (inactive nil nil)
     (if nil :if)
     (if-not nil :if-not)
     (if-non-nil nil :if-non-nil)
     (if-nil nil :if-nil)
     (if-mode nil :if-mode)
     (if-not-mode nil :if-not-mode)
     (if-derived nil :if-derived)
     (if-not-derived nil :if-not-derived)
     (inapt nil nil)
     (inapt-face 'transient-inapt-suffix :inapt-face)
     (inapt-if nil :inapt-if)
     (inapt-if-not nil :inapt-if-not)
     (inapt-if-non-nil nil :inapt-if-non-nil)
     (inapt-if-nil nil :inapt-if-nil)
     (inapt-if-mode nil :inapt-if-mode)
     (inapt-if-not-mode nil :inapt-if-not-mode)
     (inapt-if-derived nil :inapt-if-derived)
     (inapt-if-not-derived nil :inapt-if-not-derived)
     (advice nil :advice)
     (advice* nil :advice*)
     (summary nil :summary))
   'transient-object)
  (nelisp-emacs-magit-bridge--register-minimal-eieio-class
   'transient-suffix
   '((definition nil nil)
     (key nil :key)
     (command nil :command)
     (transient nil :transient)
     (format " %k %d" :format)
     (accessible-format "%i%k %d" :accessible-format)
     (description nil :description)
     (face nil :face)
     (show-help nil :show-help))
   'transient-child)
  (nelisp-emacs-magit-bridge--register-minimal-eieio-class
   'transient-information
   '((key " " nil))
   'transient-suffix)
  (nelisp-emacs-magit-bridge--register-minimal-eieio-class
   'transient-information*
   '((format " %d" nil))
   'transient-information)
  (nelisp-emacs-magit-bridge--register-minimal-eieio-class
   'transient-infix
   '((transient t nil)
     (argument nil :argument)
     (shortarg nil :shortarg)
     (value nil nil)
     (init-value nil :init-value)
     (unsavable nil :unsavable)
     (multi-value nil :multi-value)
     (always-read nil :always-read)
     (allow-empty nil :allow-empty)
     (history-key nil :history-key)
     (reader nil :reader)
     (prompt nil :prompt)
     (choices nil :choices))
   'transient-suffix)
  (dolist (class '(transient-argument transient-switch transient-option))
    (nelisp-emacs-magit-bridge--register-minimal-eieio-class
     class nil 'transient-infix))
  (nelisp-emacs-magit-bridge--register-minimal-eieio-class
   'transient-switches
   '((choices nil :choices))
   'transient-argument)
  (nelisp-emacs-magit-bridge--register-minimal-eieio-class
   'transient-files
   nil
   'transient-option)
  (nelisp-emacs-magit-bridge--register-minimal-eieio-class
   'transient-value-preset
   '((value nil :value))
   'transient-suffix)
  (nelisp-emacs-magit-bridge--register-minimal-eieio-class
   'transient-group
   '((suffixes nil :suffixes)
     (hide nil :hide)
     (description nil :description)
     (pad-keys nil :pad-keys)
     (info-format nil :info-format)
     (setup-children nil :setup-children))
   'transient-child)
  (dolist (class '(transient-column transient-row transient-columns transient-subgroups))
    (nelisp-emacs-magit-bridge--register-minimal-eieio-class
     class nil 'transient-group)))

(defvar nelisp-emacs-magit-bridge--symverify-watch nil
  "Alist of (SYMBOL . NAME-AT-CAPTURE) watched for name corruption.
Holds the symbol OBJECTS, not their names: a corrupted symbol no longer
matches its own name in the intern table, so re-interning would report it
as merely missing.  Re-reading `symbol-name' from the captured object is
what actually detects a name that changed after the symbol was created.")

(defvar nelisp-emacs-magit-bridge--symverify-part 0
  "Number of bundle parts seen by `nelisp-emacs-magit-bridge--symverify'.")

(defvar nelisp-emacs-magit-bridge--osetdef-wrapped nil
  "Non-nil once `eieio-oset-default' has been wrapped for diagnosis.")

(defun nelisp-emacs-magit-bridge--wrap-oset-default ()
  "Wrap `eieio-oset-default' once, to name the class it is called with.
`invalid-slot-name' is signalled from that function with
`(cl--class-name class)' as its first datum, and when the class is an
unregistered NAME rather than a class object that datum degenerates to an
unreadable `#<nil ...>'.  The call reaches eieio through macro/gv expansion,
so it cannot be found by grepping -- report the arguments as they arrive."
  (unless nelisp-emacs-magit-bridge--osetdef-wrapped
    (when (fboundp 'eieio-oset-default)
      (setq nelisp-emacs-magit-bridge--osetdef-wrapped t)
      (fset 'nelisp-emacs-magit-bridge--osetdef-orig
            (symbol-function 'eieio-oset-default))
      (fset 'eieio-oset-default
            (lambda (class slot value)
              (when (and (symbolp class)
                         (not (ignore-errors (cl--find-class class)))
                         (fboundp 'nelisp--write-stdout-bytes))
                (nelisp--write-stdout-bytes
                 (format "OSETDEF-BADCLASS class=%S slot=%S valuep=%S\n"
                         class slot (and value t))))
              (funcall #'nelisp-emacs-magit-bridge--osetdef-orig
                       class slot value))))))

(defvar nelisp-emacs-magit-bridge--mkinst-wrapped nil
  "Non-nil once `make-instance' has been wrapped for diagnosis.")

(defun nelisp-emacs-magit-bridge--wrap-make-instance ()
  "Wrap `make-instance' once, to catch objects built with an unresolved class.
The magit failure is `invalid-slot-name (\"#<nil nil-...>\" :command)', and
that datum comes from `slot-missing' (eieio.el:787), whose first element is
`(eieio-object-name object)' -- a STRING of the form `#<CLASS NAME>'.  Its
CLASS field reads `nil', so the object reached slot access with a nil class
rather than the slot lookup being at fault.  Objects are made here, so
report at creation: both a class argument that `cl--find-class' cannot
resolve, and a returned value that is not an eieio object."
  (unless nelisp-emacs-magit-bridge--mkinst-wrapped
    (when (fboundp 'make-instance)
      (setq nelisp-emacs-magit-bridge--mkinst-wrapped t)
      (fset 'nelisp-emacs-magit-bridge--mkinst-orig
            (symbol-function 'make-instance))
      (fset 'make-instance
            (lambda (class &rest slots)
              (unless (and (symbolp class)
                           (ignore-errors (cl--find-class class)))
                (when (fboundp 'nelisp--write-stdout-bytes)
                  (nelisp--write-stdout-bytes
                   (format "MKINST-BADCLASS class=%S symbolp=%S nslots=%S\n"
                           class (symbolp class) (length slots)))))
              (let ((obj (apply #'nelisp-emacs-magit-bridge--mkinst-orig
                                class slots)))
                (unless (ignore-errors (eieio-object-p obj))
                  (when (fboundp 'nelisp--write-stdout-bytes)
                    (nelisp--write-stdout-bytes
                     (format "MKINST-BADOBJ class=%S objp=%S\n"
                             class (ignore-errors (eieio-object-p obj))))))
                obj))))))

(defvar nelisp-emacs-magit-bridge--slotmiss-wrapped nil
  "Non-nil once `slot-missing' has been wrapped for diagnosis.")

(defun nelisp-emacs-magit-bridge--slotmiss-capture (thunk)
  "Return THUNK's value, or `ERR' if it signals during diagnosis."
  (let ((probe (ignore-errors (list t (funcall thunk)))))
    (if probe
        (cadr probe)
      'ERR)))

(defun nelisp-emacs-magit-bridge--wrap-slot-missing ()
  "Wrap `slot-missing' once, to read the failing object's class tag directly.
`slot-missing' (eieio.el:787) is what actually signals the magit blocker
`invalid-slot-name (\"#<nil nil-...>\" :command)'.  Arming happens after
part 1 -- eieio does not exist before that, as the part-1 heartbeat showed
with `eiefb=nil' -- but the failing slot ACCESS happens later than the
object's creation, so this trap is early enough even though the
creation-side traps were not."
  (unless nelisp-emacs-magit-bridge--slotmiss-wrapped
    (when (fboundp 'slot-missing)
      (setq nelisp-emacs-magit-bridge--slotmiss-wrapped t)
      (fset 'nelisp-emacs-magit-bridge--slotmiss-orig
            (symbol-function 'slot-missing))
      (fset 'slot-missing
            (lambda (object slot-name &rest rest)
              (when (fboundp 'nelisp--write-stdout-bytes)
                (nelisp--write-stdout-bytes
                 (format "SLOTMISSING slot=%S objp=%S tag=%S recp=%S len=%S\n"
                         slot-name
                         (ignore-errors (eieio-object-p object))
                         (ignore-errors (eieio--object-class-tag object))
                         (ignore-errors (recordp object))
                         (ignore-errors (length object))))
                (let* ((class-name-fn
                        (cond
                         ((fboundp 'eieio-class-name) 'eieio-class-name)
                         ((fboundp 'eieio--class-name) 'eieio--class-name)))
                       (class-parents-fn
                        (cond
                         ((fboundp 'eieio--class-parents) 'eieio--class-parents)
                         ((fboundp 'eieio-class-parents) 'eieio-class-parents)))
                       (class-slots-fn
                        (cond
                         ((fboundp 'eieio--class-slots) 'eieio--class-slots)
                         ((fboundp 'eieio-class-slots) 'eieio-class-slots)))
                       (otag
                        (nelisp-emacs-magit-bridge--slotmiss-capture
                         (lambda () (eieio--object-class-tag object))))
                       (reg
                        (nelisp-emacs-magit-bridge--slotmiss-capture
                         (lambda () (find-class 'transient-describe-target))))
                       (same
                        (nelisp-emacs-magit-bridge--slotmiss-capture
                         (lambda ()
                           (if (or (eq otag 'ERR)
                                   (eq reg 'ERR))
                               'ERR
                             (eq otag reg)))))
                       (oname
                        (nelisp-emacs-magit-bridge--slotmiss-capture
                         (lambda ()
                           (if (eq otag 'ERR)
                               'ERR
                             (funcall class-name-fn otag)))))
                       (oparents
                        (nelisp-emacs-magit-bridge--slotmiss-capture
                         (lambda ()
                           (if (eq otag 'ERR)
                               'ERR
                             (funcall class-parents-fn otag)))))
                       (oslots
                        (nelisp-emacs-magit-bridge--slotmiss-capture
                         (lambda ()
                           (if (eq otag 'ERR)
                               'ERR
                             (length (funcall class-slots-fn otag))))))
                       (rslots
                        (nelisp-emacs-magit-bridge--slotmiss-capture
                         (lambda ()
                           (if (eq reg 'ERR)
                               'ERR
                             (length (funcall class-slots-fn reg))))))
                       (regp
                        (nelisp-emacs-magit-bridge--slotmiss-capture
                         (lambda ()
                           (if (eq reg 'ERR)
                               'ERR
                             (and reg t))))))
                  (nelisp--write-stdout-bytes
                   (format "SLOTMISSID same=%S oname=%S oparents=%S oslots=%S rslots=%S regp=%S\n"
                           same oname oparents oslots rslots regp))))
              (apply #'nelisp-emacs-magit-bridge--slotmiss-orig
                     object slot-name rest))))))

(defun nelisp-emacs-magit-bridge--symverify ()
  "Check every watched symbol's name; report the first part that corrupts one.
Capture happens on the first call and covers the six names seen corrupted so
far plus every `cl-defstruct' accessor registered at that point -- real,
long, already-interned symbols rather than synthetic ones, because synthetic
symbols have never reproduced the fault."
  (nelisp-emacs-magit-bridge--wrap-oset-default)
  (nelisp-emacs-magit-bridge--wrap-make-instance)
  (nelisp-emacs-magit-bridge--wrap-slot-missing)
  (setq nelisp-emacs-magit-bridge--symverify-part
        (1+ nelisp-emacs-magit-bridge--symverify-part))
  (unless nelisp-emacs-magit-bridge--symverify-watch
    (let ((syms (append
                 (list (intern "eieio-default-superclass")
                       (intern "cl--slot-descriptor-type")
                       (intern "cl--slot-descriptor-name")
                       (intern "eieio--class-index-table")
                       (intern "nelisp-ec-current-buffer")
                       (intern "nelisp-emacs-magit-bridge--ensure-advice-runtime"))
                 (and (boundp 'nelisp-cl-macros--accessor-info)
                      (mapcar #'car nelisp-cl-macros--accessor-info)))))
      (dolist (s syms)
        (when (symbolp s)
          (setq nelisp-emacs-magit-bridge--symverify-watch
                (cons (cons s (symbol-name s))
                      nelisp-emacs-magit-bridge--symverify-watch))))))
  (let ((bad 0) (first nil))
    (dolist (entry nelisp-emacs-magit-bridge--symverify-watch)
      (let ((got (symbol-name (car entry))))
        (unless (string= got (cdr entry))
          (setq bad (1+ bad))
          (unless first (setq first (cons (cdr entry) got))))))
    ;; Always print, even when clean.  A report-only-on-failure instrument
    ;; cannot distinguish "ran and found nothing" from "never ran", and that
    ;; ambiguity has already cost two full measurement cycles.  The line also
    ;; carries the oset-default wrapper's state, so one run answers both
    ;; "did the hook fire?" and "was the trap armed in time?".
    (when (fboundp 'nelisp--write-stdout-bytes)
      (nelisp--write-stdout-bytes
       (format "SYMVERIFY part=%d watched=%d bad=%d wrapped=%S mkwrapped=%S smwrapped=%S eiefb=%S want=%S got=%S\n"
               nelisp-emacs-magit-bridge--symverify-part
               (length nelisp-emacs-magit-bridge--symverify-watch)
               bad
               nelisp-emacs-magit-bridge--osetdef-wrapped
               nelisp-emacs-magit-bridge--mkinst-wrapped
               nelisp-emacs-magit-bridge--slotmiss-wrapped
               (fboundp 'eieio-oset-default)
               (car first) (cdr first))))
    ;; Keep this as a separate call so an inventory failure cannot suppress
    ;; the primary SYMVERIFY line.
    (nelisp-emacs-magit-bridge--classinv
     "symverify" nelisp-emacs-magit-bridge--symverify-part)
    bad))

(defun nelisp-emacs-magit-bridge--repair-transient-class-registry ()
  "Repair missing transient class registrations after vendor bundle load.

Calling the manual registration path before transient itself loads can break
vendor evaluation order.  On the standalone bridge path, only repair the class
registry after the real transient feature is present and only when the class
lookup/slot metadata is actually missing."
  (nelisp-emacs-magit-bridge--symverify)
  (when (featurep 'transient)
    (let* ((prefix-class (ignore-errors (find-class 'transient-prefix nil)))
           (needs-repair
            (or (null prefix-class)
                (null (ignore-errors
                        (eieio--class-slot-name-index
                         prefix-class 'prototype))))))
      (when needs-repair
        (nelisp-emacs-magit-bridge--trace
         "transient-class-registry repair BEGIN class=%S"
         prefix-class)
        (nelisp-emacs-magit-bridge--ensure-transient-classes-manual)
        (nelisp-emacs-magit-bridge--trace
         "transient-class-registry repair PASS class=%S"
         (ignore-errors (find-class 'transient-prefix nil)))))))

(defun nelisp-emacs-magit-bridge--ensure-magit-section-constructor (class-name)
  "Ensure CLASS-NAME has a callable constructor function."
  (unless (fboundp class-name)
    (fset class-name
          `(lambda (&rest slots)
             (apply #'make-instance ',class-name slots)))))

(defun nelisp-emacs-magit-bridge--ensure-magit-file-section-constructor ()
  "Ensure Magit diff section classes resolve to usable constructors."
  (when (boundp 'magit--section-type-alist)
    (setf (alist-get 'file magit--section-type-alist) 'magit-file-section)
    (setf (alist-get 'module magit--section-type-alist) 'magit-module-section)
    (setf (alist-get 'hunk magit--section-type-alist) 'magit-hunk-section))
  (dolist (class '(magit-file-section magit-module-section magit-hunk-section))
    (nelisp-emacs-magit-bridge--ensure-magit-section-constructor class))
  (unless (fboundp 'magit-file-section-p)
    (defun magit-file-section-p (object)
      "Return non-nil when OBJECT is a file-like Magit section."
      (and (ignore-errors (cl-typep object 'magit-file-section))
           (eq (ignore-errors (oref object type)) 'file))))
  (unless (fboundp 'magit-module-section-p)
    (defun magit-module-section-p (object)
      "Return non-nil when OBJECT is a module-like Magit section."
      (and (ignore-errors (cl-typep object 'magit-module-section))
           (eq (ignore-errors (oref object type)) 'module))))
  (unless (fboundp 'magit-hunk-section-p)
    (defun magit-hunk-section-p (object)
      "Return non-nil when OBJECT is a hunk-like Magit section."
      (and (ignore-errors (cl-typep object 'magit-hunk-section))
           (eq (ignore-errors (oref object type)) 'hunk))))
  nil)

(defun nelisp-emacs-magit-bridge--eieio-oref (obj slot)
  "NeLisp-safe replacement for `eieio-oref'."
  (cond
   ((or (eieio-object-p obj)
        (cl-typep obj 'cl-structure-object))
    (let* ((class (eieio--object-class obj))
           (c (eieio--slot-name-index class slot)))
      (if c
          (eieio-barf-if-slot-unbound (aref obj c) obj slot 'oref)
        (if (setq c (eieio--class-slot-name-index class slot))
            (aref (eieio--class-class-allocation-values class) c)
          ;; Real transient code treats many slots as optional probes across
          ;; the class hierarchy.  On this runtime those probes can still
          ;; reach `oref' after a missing-slot guard and should read as nil
          ;; rather than aborting the whole workflow.
          (if (or (memq slot '(shortarg pad-keys))
                  (nelisp-emacs-magit-bridge--transient-class-symbol-p
                   (ignore-errors (eieio-object-class-name obj))))
              nil
            (slot-missing obj slot 'oref))))))
   ((cl-typep obj 'oclosure)
    (oclosure--slot-value obj slot))
   (t
    (signal 'wrong-type-argument
            (list '(or eieio-object cl-structure-object oclosure) obj)))))

(defun nelisp-emacs-magit-bridge--eieio-oset (obj slot value)
  "NeLisp-safe replacement for `eieio-oset'."
  (cond
   ((or (eieio-object-p obj)
        (cl-typep obj 'cl-structure-object))
    (let* ((class (eieio--object-class obj))
           (c (eieio--slot-name-index class slot)))
      (if c
          (progn
            (eieio--validate-slot-value class c value slot)
            (aset obj c value))
        (if (setq c (eieio--class-slot-name-index class slot))
            (progn
              (eieio--validate-class-slot-value class c value slot)
              (aset (eieio--class-class-allocation-values class) c value))
          (slot-missing obj slot 'oset value)))))
   ((cl-typep obj 'oclosure)
    (oclosure--set-slot-value obj slot value))
   (t
    (signal 'wrong-type-argument
            (list '(or eieio-object cl-structure-object oclosure) obj)))))

(defun nelisp-emacs-magit-bridge--ensure-eieio-object-access-safe ()
  "Install NeLisp-safe EIEIO object accessors."
  (fset 'eieio-oref
        (symbol-function
         'nelisp-emacs-magit-bridge--eieio-oref))
  (fset 'eieio-oset
        (symbol-function
         'nelisp-emacs-magit-bridge--eieio-oset)))

(defun nelisp-emacs-magit-bridge--ensure-defalias-forward-reference ()
  "Ensure `defalias' tolerates a not-yet-defined symbol DEFINITION.

`(defalias SYM (function NOT-YET-DEFINED))' is a routine forward-alias
pattern (`define-obsolete-function-alias' uses it throughout EIEIO and
Magit, e.g. `object-class-fast' aliased to `eieio-object-class' before
that function is defined later in the same file).  `(function SYM)'
for a plain symbol evaluates to the bare symbol here exactly as in
real Emacs (confirmed: `(consp (function foo))' is nil), so this is
not a `function'-special-form bug; the native `defalias' eagerly
resolves/dereferences its DEFINITION argument and signals a
`void-variable' error when the target symbol has no function cell
yet.  `src/emacs-eval.el' already carries an Elisp polyfill for this
exact case (a late-bound forwarder via `symbolp'+`fboundp'), but it is
guarded by `(unless (fboundp (quote defalias)) ...)' and a native
`defalias' already satisfies that guard, so the polyfill never
installs.  This bridge precondition re-installs that same already-written
logic unconditionally (copied, not reimplemented) so the forward-reference
case works without waiting on the native/polyfill guard order to change
in `src/emacs-eval.el' itself."
  (defun defalias (symbol definition &optional docstring)
    "Alias SYMBOL to DEFINITION, tolerating a not-yet-defined DEFINITION.
DOCSTRING is accepted for arglist parity and currently ignored."
    (ignore docstring)
    (if (and (symbolp definition)
             (not (fboundp definition)))
        (eval (list 'defun symbol '(&rest args)
                    (list 'apply (list 'quote definition) 'args)))
      (fset symbol definition))
    symbol))

(defun nelisp-emacs-magit-bridge--ensure-symbol-function-function-form ()
  "Ensure `symbol-function' accepts `(function SYMBOL)' like real Emacs.

Magit's own code sometimes passes `#'foo' directly to `symbol-function'
(`vendor/magit/lisp/magit-base.el': `#'split-string').  The current
runtime rejects that wrapped form even though plain-symbol lookup works,
so install a narrow compatibility wrapper that unwraps `(function
SYMBOL)' before delegating to the original implementation."
  (unless nelisp-emacs-magit-bridge--symbol-function-orig
    (setq nelisp-emacs-magit-bridge--symbol-function-orig
          (symbol-function 'symbol-function))
    (defun symbol-function (symbol)
      "Return SYMBOL's function cell, accepting `(function SYMBOL)' too."
      (when (and (consp symbol)
                 (eq (car symbol) 'function)
                 (consp (cdr symbol))
                 (symbolp (car (cdr symbol))))
        (setq symbol (car (cdr symbol))))
      (funcall nelisp-emacs-magit-bridge--symbol-function-orig symbol))))

(defun nelisp-emacs-magit-bridge--ensure-function-alias-p ()
  "Ensure `function-alias-p' exists for transient's alias-chain lookups.

Transient uses `function-alias-p' only to walk command alias chains when
resolving suffix prototypes and suppressing docstrings for the default infix
command.  A narrow symbol-alias walker is sufficient here: collect successive
symbol-function links while they remain plain symbols, stopping on cycles or a
non-symbol definition."
  (unless (fboundp 'function-alias-p)
    (defun function-alias-p (function)
      "Return FUNCTION's alias chain as a list of symbols."
      (let ((seen nil)
            (aliases nil)
            (next function))
        (while (and (symbolp next)
                    (fboundp next)
                    (not (memq next seen)))
          (push next seen)
          (setq next (symbol-function next))
          (when (symbolp next)
            (push next aliases)))
        (nreverse aliases)))))

(defun nelisp-emacs-magit-bridge--ensure-autoload-runtime ()
  "Ensure autoload/command predicates exist for transient suffix init."
  (unless (fboundp 'autoloadp)
    (defun autoloadp (object)
      "Return non-nil when OBJECT is an autoload form."
      (and (consp object)
           (eq (car object) 'autoload))))
  (unless (fboundp 'commandp)
    (defun commandp (object &optional for-call-interactively)
      "Return non-nil when OBJECT names or is a callable command.
FOR-CALL-INTERACTIVELY is accepted for Emacs API parity."
      (ignore for-call-interactively)
      (cond
       ((symbolp object) (fboundp object))
       ((functionp object) t)
       (t nil)))))

(defun nelisp-emacs-magit-bridge--ensure-sxhash-eq ()
  "Ensure `sxhash-eq' exists for EIEIO/transient hash-table helpers.

For the bridge's current use sites, a plain `sxhash' fallback is sufficient:
objects are only keyed for stable in-session identity maps and button labels."
  (unless (fboundp 'sxhash-eq)
    (defun sxhash-eq (object)
      "Return a session-stable hash code for OBJECT."
      (sxhash object))))

(defun nelisp-emacs-magit-bridge--ensure-slot-boundp-safe ()
  "Ensure `slot-boundp' returns nil for missing slots instead of signalling.

Transient and Magit frequently probe optional EIEIO slots.  On this runtime,
propagating `invalid-slot-name' from those probes is less useful than the real
caller intent, which is a boolean presence test."
  (cond
   ((and (not nelisp-emacs-magit-bridge--slot-boundp-orig)
         (fboundp 'slot-boundp))
    (setq nelisp-emacs-magit-bridge--slot-boundp-orig
          (symbol-function 'slot-boundp))
    (defun slot-boundp (object slot)
      "Return non-nil when OBJECT has SLOT bound, else nil."
      (condition-case nil
          (funcall nelisp-emacs-magit-bridge--slot-boundp-orig object slot)
        (invalid-slot-name nil))))
   ((not (fboundp 'slot-boundp))
    (defun slot-boundp (object slot)
      "Return non-nil when OBJECT has SLOT bound, else nil."
      (condition-case nil
          (let ((value (slot-value object slot)))
            (not (eq value eieio--unbound)))
        (invalid-slot-name nil)
        (unbound-slot nil))))))

(defun nelisp-emacs-magit-bridge--ensure-cl-declaim ()
  "Ensure `cl-declaim' exists as the compiler-hint no-op it effectively is.

Real `cl-declaim' (`cl-lib.el') records byte-compiler optimization
declarations (`cl-proclaim') and, when compiling, wraps them in
`cl-eval-when'.  NeLisp has no byte/native compiler for these hints to
affect, so a no-op macro is a faithful runtime stand-in rather than a
partial reimplementation of `cl-proclaim' bookkeeping nothing here ever
reads."
  (unless (fboundp 'cl-declaim)
    (defmacro cl-declaim (&rest _specs) nil)))

(defun nelisp-emacs-magit-bridge--ensure-ansi-color-update-face-vec-stub ()
  "Ensure `ansi-color--update-face-vec' exists as a documented no-op.

Real `ansi-color.el' (loaded by this bridge for its ordinary 8/16-color
SGR handling, used by `magit-process'/`magit-log' to render colored git
output) contains one function, `ansi-color--update-face-vec', with a
bool-vector reader literal (`#&8\" \"') that the NeLisp reader/evaluator
cannot get through cleanly on its own (confirmed in isolation: reaching
that literal silently truncates the remainder of that `load' call, with
no relation to Magit); the bundle generator drops only that one
definition (see `nelisp-emacs-magit-bridge-bundle-excluded-defuns'),
keeping the rest of `ansi-color.el' real.  That function implements only
the extended 256-color/24-bit (SGR 38/48 with a 5- or 2-parameter
sub-sequence) face-vector bookkeeping path; ordinary 8/16-color SGR
codes (what plain `git' plumbing output realistically emits) never reach
it.  This stub is a documented, narrow functionality gap for that
extended path, not a Magit or git-correctness gap: a no-op here means an
extended-color escape sequence's *face* bookkeeping silently no-ops
(cosmetic — the affected text still renders, just without that specific
color update), not a data-correctness or crash risk."
  (unless (fboundp 'ansi-color--update-face-vec)
    (defun ansi-color--update-face-vec (_face-vec _iterator)
      "Stub: extended 256-color/24-bit SGR face bookkeeping is not supported.
See `nelisp-emacs-magit-bridge--ensure-ansi-color-update-face-vec-stub'."
      nil)))

(defun nelisp-emacs-magit-bridge--ensure-compat-maybe-require ()
  "Ensure `compat--maybe-require' exists as the harmless no-op it already is.

`compat.el' defines this helper macro inside a top-level
`eval-when-compile', which the shared normalizer (correctly, in
general) drops as compile-time-only.  Real `compat--maybe-require'
only conditionally `require's `compat-31' when
`(< emacs-major-version 31)'; this bridge already force-loads
`compat-31.el' earlier in the fixed load order regardless, so by the
time `compat.el' calls `(compat--maybe-require)' the real macro would
have been a no-op here anyway.  A literal no-op stands in for it."
  (unless (fboundp 'compat--maybe-require)
    (defmacro compat--maybe-require () nil)))

(defun nelisp-emacs-magit-bridge--ensure-default-process-coding-system ()
  "Ensure `default-process-coding-system' is a real dynamic (special) variable.

Real Emacs declares this as a built-in, always-special defvar (default
value a DECODING . ENCODING cons set during startup).  NeLisp's
substrate never declares it at all.  Magit's own lexical-binding files
only ever *bind* it locally around a process call
(`(let ((default-process-coding-system (magit--process-coding-system)))
...)' in `vendor/magit/lisp/magit-process.el'/`magit-git.el') and never
`defvar' it themselves, exactly like real Emacs magit assumes the name
is already special.  Without a prior `defvar', a lexical-binding `let'
on an unknown name creates a plain LEXICAL binding instead of a dynamic
one, so a *different* function reading the same name later in the same
dynamic extent (e.g. `magit-git.el:1342', not textually nested inside
that `let') hits `void-variable' instead of seeing Magit's own binding
-- this is what M2 probing hit running `magit-git-string' for real.
Declaring the name here with a value shaped like real Emacs' own
default (a cons of decoding and encoding coding systems, both nil =
`no-conversion') is a session-level precondition fix, not a vendor
patch: it only makes the name a genuine special variable so Magit's
existing dynamic `let' behaves the way it already assumes."
  (unless (boundp 'default-process-coding-system)
    (defvar default-process-coding-system (cons nil nil))))

(defun nelisp-emacs-magit-bridge--ensure-coding-system-change-eol-conversion ()
  "Ensure `coding-system-change-eol-conversion' exists.

`magit--process-coding-system' (`vendor/magit/lisp/magit-process.el')
calls this real-Emacs function to derive an EOL-specific variant of a
coding system (e.g. mapping a generic symbol + `unix' to `utf-8-unix')
whenever `magit-process-ensure-unix-line-ending' is non-nil (the
default).  NeLisp's substrate does not model coding systems as objects
with an EOL-conversion axis at all, and this bridge's process substrate
already always reads/writes subprocess bytes as given (no CRLF
translation applied anywhere), so returning CODING-SYSTEM unchanged (or
`utf-8-unix' when CODING-SYSTEM is nil, i.e. no coding system was
configured) is a faithful-enough stand-in for M2/M3's read-only status-
buffer use: it lets Magit's own process setup finish instead of
`void-function' aborting before any git process is even started, and it
does not change what bytes get inserted -- only what symbol Magit
records for later (here, unused) coding introspection."
  (unless (fboundp 'coding-system-change-eol-conversion)
    (defun coding-system-change-eol-conversion (coding-system eol-type)
      "See `nelisp-emacs-magit-bridge--ensure-coding-system-change-eol-conversion'."
      (ignore eol-type)
      (or coding-system 'utf-8-unix))))

(defun nelisp-emacs-magit-bridge--ensure-backquote-marker-symbols ()
  "Ensure the three advertised backquote/unquote/splice marker constants exist.

Real Emacs's `backquote.el' (always preloaded/dumped, so it never shows
up as a \"newly loaded\" file when this bridge's bundle generator
records `load-history' diffs from a clean host Emacs session -- hence
it is absent from `nelisp-emacs-magit-bridge-bundle-files') advertises
three `defconst's other packages use to introspect or build raw,
un-expanded backquote forms: `backquote-backquote-symbol' (the reader
head for `` ` ''), `backquote-unquote-symbol' (`` , ''), and
`backquote-splice-symbol' (`` ,@ '').  `vendor/llama/llama.el' (the
`##'/`llama' macro transient/magit/with-editor use pervasively for
short lambdas) and `vendor/emacs-lisp/emacs-lisp/pp.el' both reference
these three names directly as plain variables at macro-expansion or
load time, e.g. `(eq (car-safe fn) backquote-backquote-symbol)' in
`llama--collect'.  This NeLisp reader expands backquote templates
fully at READ time (`nelisp-reader--read-backquote') instead of
leaving a `` (\\=` ...) ''-headed literal for a `backquote' MACRO to
expand later the way real Emacs does, so it never produces a runtime
value whose `car' is `eq' to one of these markers -- but the mere act
of *evaluating* the bare variable reference to compare against still
requires the variable to be bound, and this bridge never provided it,
so any `##...' call site anywhere in the real vendor chain signalled
`void-variable: backquote-backquote-symbol' the first time the
enclosing (uncompiled, so lazily macro-expanded) function was actually
called (found probing M2's `magit-toplevel', which reaches
`magit-ignore-submodules-p's `(cl-find-if (##string-prefix-p ...) ...)'
by way of ordinary git-status plumbing, not anything Magit-status-
buffer specific -- this affects the `##' shorthand generally).
Declaring the three constants with real Emacs's own values is a
session-level precondition fix, not a vendor patch: because this
reader's backquote representation never round-trips through the
tagged-list shape these constants exist to recognize, the comparisons
that use them will correctly always take the \"not a nested backquote
template\" branch, which is the semantically right answer here."
  (unless (boundp 'backquote-backquote-symbol)
    (defconst backquote-backquote-symbol '\`))
  (unless (boundp 'backquote-unquote-symbol)
    (defconst backquote-unquote-symbol '\,))
  (unless (boundp 'backquote-splice-symbol)
    (defconst backquote-splice-symbol '\,@)))

(defun nelisp-emacs-magit-bridge--ensure-files-el-globals ()
  "Ensure the `files.el'-defined globals the vendor chain reads directly exist.

Real Emacs preloads/dumps `files.el' as part of the core, exactly like
`backquote.el' (see
`nelisp-emacs-magit-bridge--ensure-backquote-marker-symbols' above), so it
never shows up as a newly-loaded file when this bridge's bundle generator
diffs `load-history' against a clean host Emacs session -- `files.el'
itself is therefore correctly absent from
`nelisp-emacs-magit-bridge-bundle-files', but the NeLisp substrate never
preloads its globals either, so any of these names signals `void-variable'
the first time evaluation actually reaches it, at whatever call depth that
happens to be (M2 probing hit `find-file-visit-truename' this way, deep
inside `magit-toplevel' -- by direct code inspection this is one of many,
not an isolated case).  This list was built by a static cross-reference of
every top-level `files.el' defcustom/defvar/defconst name against the full
vendor chain (magit+transient+with-editor+compat+cond-let+llama), narrowed
to the subset a live `boundp' probe against this bridge's own baked
runtime image confirmed both referenced AND still missing -- notably,
`auto-mode-alist'/`file-name-history'/`find-file-hook'/`font-lock-keywords'/
`kill-buffer-hook'/`magic-fallback-mode-alist' also match the reference
search but are already bound elsewhere in this substrate (or, for some
Compat-shimmed names, become bound as a side effect of Compat's own
version-gated logic once `emacs-version' reads \"30.1\"; not blindly
ported here since they are not gaps).  Declaring each with real Emacs's
own default value (copied from `files.el', not reinterpreted) is a
session-level precondition fix, not a vendor patch.  A few defaults are
deliberately simplified stand-ins rather than literal copies because the
real value either names a function this read-only M2/M3 bridge never
calls (`revert-buffer-function' defaults to nil here, not
`#'revert-buffer--default', since `files.el' -- and that function with
it -- is never loaded) or duplicates the caching keybinding-prompt logic
of a function `save-some-buffers' this bridge does not need to run
(`save-some-buffers-action-alist' is left nil rather than the real
value's `##'-heavy alist); both are noted inline."
  (dolist (spec
           '((after-revert-hook . nil)
             (after-save-hook . nil)
             (before-revert-hook . nil)
             (before-save-hook . nil)
             (backup-directory-alist . nil)
             (confirm-nonexistent-file-or-buffer . after-completion)
             (directory-abbrev-alist . nil)
             (directory-files-no-dot-files-regexp . "[^.]\\|\\.\\.\\.")
             (enable-local-variables . t)
             ;; Real default is platform-conditional; this bridge only
             ;; ever runs on the non-Windows branch of `files.el''s own
             ;; `(if (memq system-type '(windows-nt cygwin)) ...)'.
             (mounted-file-systems
              . "^\\(?:/\\(?:afs/\\|m\\(?:edia/\\|nt\\)\\|\\(?:ne\\|tmp_mn\\)t/\\)\\)")
             (find-file-literally . nil)
             (find-file-not-found-functions . nil)
             (find-file-visit-truename . nil)
             (lock-file-name-transforms . nil)
             (remote-file-name-inhibit-cache . 10)
             ;; Simplified stand-in: real default is `#'revert-buffer--default',
             ;; a `files.el' function this bridge never loads or calls.
             (revert-buffer-function . nil)
             (revert-without-query . nil)
             ;; Simplified stand-in: real default is a keybinding/prompt alist
             ;; for the interactive `save-some-buffers' prompt loop, unused by
             ;; this bridge's read-only M2/M3 status-buffer path.
             (save-some-buffers-action-alist . nil)
             (trash-directory . nil)
             (trusted-content . nil)))
    (unless (boundp (car spec))
      ;; `defvar' is a special form and cannot take a runtime symbol as its
      ;; first argument, so a data-driven `dolist' has to go through `eval'
      ;; to build and run the equivalent `(defvar SYM 'VALUE)' form -- this
      ;; still makes SYM a genuine special (dynamically-scoped) variable,
      ;; unlike plain `set', which would only populate its global value
      ;; cell without marking it special for a later `let' to bind
      ;; dynamically (the same distinction
      ;; `nelisp-emacs-magit-bridge--ensure-default-process-coding-system'
      ;; documents above).
      (eval (list 'defvar (car spec) (list 'quote (cdr spec))) t)))
  ;; Two names need a non-literal default value (a hash table, a quoted
  ;; list), so they cannot live in the simple dotted-pair table above.
  (unless (boundp 'file-has-changed-p--hash-table)
    (defvar file-has-changed-p--hash-table (make-hash-table :test #'equal)))
  (unless (boundp 'ignored-local-variables)
    (defvar ignored-local-variables
      '(ignored-local-variables safe-local-variable-values
        file-local-variables-alist dir-local-variables-alist))))

(defun nelisp-emacs-magit-bridge--ensure-window-el-globals ()
  "Ensure the `window.el'-defined globals transient reads directly exist.

Real Emacs preloads/dumps `window.el' as part of the core.  The standalone
runtime does not, so transient's popup-buffer setup can hit `void-variable'
when it local-binds or reads these names."
  (unless (boundp 'cursor-in-non-selected-windows)
    (defvar cursor-in-non-selected-windows nil)))

(defun nelisp-emacs-magit-bridge--ensure-window-selection-macros ()
  "Ensure the `window.el' selection macros transient expects are available."
  (unless (fboundp 'emacs-window-parent)
    (load (expand-file-name "src/emacs-window.el"
                            (nelisp-emacs-magit-bridge--repo-root))
          nil 'no-message t t))
  (unless (fboundp 'window-parent)
    (fset 'window-parent
          (symbol-function 'emacs-window-parent)))
  (unless (fboundp 'with-selected-window)
    (defmacro with-selected-window (window &rest body)
      "Execute BODY with WINDOW selected."
      `(emacs-window-with-selected-window ,window ,@body))))

(defun nelisp-emacs-magit-bridge--ensure-window-builtins ()
  "Ensure unprefixed window helpers resolve to the shared window substrate."
  (let ((root (nelisp-emacs-magit-bridge--repo-root)))
    (unless (fboundp 'emacs-window-get-buffer-window)
      (load (expand-file-name "src/emacs-window.el" root)
            nil 'no-message t t))
    (unless (fboundp 'get-buffer-window)
      (load (expand-file-name "src/emacs-window-builtins.el" root)
            nil 'no-message t t))
    (unless (fboundp 'get-buffer-window)
      (defalias 'get-buffer-window #'emacs-window-get-buffer-window))
    (unless (fboundp 'get-buffer-window-list)
      (defalias 'get-buffer-window-list #'emacs-window-get-buffer-window-list))
    (unless (fboundp 'window-list)
      (defalias 'window-list #'emacs-window-window-list))))

(defun nelisp-emacs-magit-bridge--resolve-buffer-object (buffer-or-name)
  "Resolve BUFFER-OR-NAME to a live buffer object when possible."
  (cond
   ((and (fboundp 'nelisp-ec-buffer-p)
         (ignore-errors (nelisp-ec-buffer-p buffer-or-name)))
    buffer-or-name)
   ((bufferp buffer-or-name) buffer-or-name)
   ((stringp buffer-or-name) (get-buffer buffer-or-name))
   (t nil)))

(defun nelisp-emacs-magit-bridge--transient-buffer-p (buffer-or-name)
  "Return non-nil when BUFFER-OR-NAME resolves to the transient popup buffer."
  (let* ((buffer (nelisp-emacs-magit-bridge--resolve-buffer-object buffer-or-name))
         (name (and buffer (buffer-name buffer))))
    (and (stringp name)
         (string-prefix-p " *transient*" name))))

(defun nelisp-emacs-magit-bridge--display-buffer (buffer-or-name &optional action frame)
  "Display BUFFER-OR-NAME, simplifying transient popup routing."
  (ignore action frame)
  (let ((buffer (or (nelisp-emacs-magit-bridge--resolve-buffer-object buffer-or-name)
                    buffer-or-name)))
    (if (nelisp-emacs-magit-bridge--transient-buffer-p buffer)
        (let ((window (selected-window)))
          (set-window-buffer window buffer)
          window)
      (emacs-window-display-buffer buffer))))

(defun nelisp-emacs-magit-bridge--pop-to-buffer (buffer-or-name &optional action norecord)
  "Display BUFFER-OR-NAME and select its window."
  (ignore norecord)
  (let* ((window (nelisp-emacs-magit-bridge--display-buffer buffer-or-name action))
         (buffer (and window (window-buffer window))))
    (when window
      (select-window window))
    (when buffer
      (set-buffer buffer))
    buffer))

(defun nelisp-emacs-magit-bridge--ensure-window-display-helpers ()
  "Ensure lightweight window display helpers transient expects are available."
  (fset 'selected-window
        (symbol-function 'emacs-window-selected-window))
  (fset 'frame-selected-window
        (symbol-function 'emacs-window-frame-selected-window))
  (fset 'window-list
        (symbol-function 'emacs-window-window-list))
  (fset 'window-buffer
        (symbol-function 'emacs-window-window-buffer))
  (fset 'set-window-buffer
        (symbol-function 'emacs-window-set-window-buffer))
  (fset 'select-window
        (symbol-function 'emacs-window-select-window))
  (fset 'window-live-p
        (symbol-function 'emacs-window-window-live-p))
  (fset 'window-height
        (symbol-function 'emacs-window-window-height))
  (fset 'window-width
        (symbol-function 'emacs-window-window-width))
  (unless (fboundp 'window-body-height)
    (defun window-body-height (&optional window _pixelwise)
      "Return WINDOW body height for the standalone bridge."
      (max 1 (1- (window-height window)))))
  (unless (fboundp 'window-body-width)
    (defun window-body-width (&optional window _pixelwise)
      "Return WINDOW body width for the standalone bridge."
      (window-width window)))
  (fset 'display-buffer
        (symbol-function 'nelisp-emacs-magit-bridge--display-buffer))
  (fset 'pop-to-buffer
        (symbol-function 'nelisp-emacs-magit-bridge--pop-to-buffer))
  (fset 'quit-window
        (symbol-function 'emacs-window-quit-window))
  (unless (fboundp 'fit-frame-to-buffer)
    (defun fit-frame-to-buffer (&rest args)
      "Standalone bridge no-op for frame sizing."
      (car args)))
  (unless (fboundp 'fit-window-to-buffer)
    (defun fit-window-to-buffer (&rest args)
      "Standalone bridge no-op for popup sizing."
      (car args))))

(defun nelisp-emacs-magit-bridge--ensure-uniquify-globals ()
  "Ensure the `uniquify.el' names Magit's buffer naming touches exist.

`uniquify.el' is preloaded/dumped by real Emacs (same class as
`files.el' and `backquote.el' above: never appears in a load-history
diff, so it is correctly absent from the bundle, but this substrate
never preloads it either).  `magit--maybe-uniquify-buffer-names'
(`vendor/magit/lisp/magit-mode.el') runs unconditionally for every new
Magit buffer when `magit-uniquify-buffer-names' is non-nil (the
default): it pushes onto `uniquify-list-buffers-directory-modes',
let-binds `uniquify-buffer-name-style', sets the built-in buffer-local
`list-buffers-directory', and calls
`uniquify-rationalize-file-buffer-names'.  The two defvars and the
defcustom get real Emacs's own default values.  The rationalize
function is a documented no-op stand-in, NOT a copy: its only job is
cosmetic buffer-name disambiguation across same-named buffers, this
bridge's buffers are already unique via `generate-new-buffer', and
porting uniquify's full rationalize/rename machinery is out of scope
for a read-only status buffer."
  (unless (boundp 'uniquify-list-buffers-directory-modes)
    (defvar uniquify-list-buffers-directory-modes
      '(dired-mode cvs-mode vc-dir-mode)))
  (unless (boundp 'uniquify-buffer-name-style)
    (defvar uniquify-buffer-name-style 'post-forward-angle-brackets))
  (unless (boundp 'list-buffers-directory)
    (defvar list-buffers-directory nil))
  (unless (fboundp 'uniquify-rationalize-file-buffer-names)
    (defun uniquify-rationalize-file-buffer-names (_base _dirname _newbuf)
      "Stub: cosmetic buffer-name disambiguation is not modeled.
See `nelisp-emacs-magit-bridge--ensure-uniquify-globals'."
      nil)))

(defun nelisp-emacs-magit-bridge--ensure-simple-el-globals ()
  "Ensure the `simple.el'-defined globals the vendor chain reads exist.

Same host-preload gap class as
`nelisp-emacs-magit-bridge--ensure-files-el-globals' above, for
`simple.el' (also dumped into real Emacs, so also correctly absent from
the bundle).  Built the same way: static cross-reference of simple.el's
top-level defcustom/defvar/defconst names against the vendor chain,
narrowed by a live `boundp' probe against the baked runtime image
(`kill-ring'/`minibuffer-history'/`shell-command-switch' matched the
reference scan but are already bound here).  M2 hit
`line-move-visual' first, via `magit-section-mode''s own
`(setq-local line-move-visual t)' body.  Values are real Emacs's own
defaults; the two `redisplay-*-region-function' defaults name simple.el
functions this bridge never loads, so they are nil here (their only
consumer is region highlighting, not modeled by this substrate), and
that simplification is deliberately documented rather than silent."
  (dolist (spec
           '((deactivate-mark-hook . nil)
             (line-move-visual . t)
             (minibuffer-default . nil)
             (minibuffer-default-add-function . nil)
             (prefix-command-preserve-state-hook . nil)
             (read-extended-command-predicate . nil)
             ;; Real defaults are #'redisplay--highlight-overlay-function /
             ;; #'redisplay--unhighlight-overlay-function (simple.el
             ;; functions not loaded here); region highlighting is not
             ;; modeled, so nil.
             (redisplay-highlight-region-function . nil)
             (redisplay-unhighlight-region-function . nil)
             (shell-command-default-error-buffer . nil)
             (shift-select-mode . t)
             (tabulated-list-entries . nil)
             (tabulated-list-format . nil)
             (tabulated-list-sort-key . nil)
             (widen-automatically . t)))
    (unless (boundp (car spec))
      ;; Same `eval'-built `defvar' as -ensure-files-el-globals: makes
      ;; the name genuinely special so vendor `let'/`setq-local' work.
      (eval (list 'defvar (car spec) (list 'quote (cdr spec))) t))))

(defun nelisp-emacs-magit-bridge--ensure-third-party-soft-vars ()
  "Ensure third-party variables Magit declares value-less exist with nil.

`vendor/magit/lisp/magit-section.el' carries `(defvar
symbol-overlay-inhibit-map)' -- a value-less special declaration for an
optional third-party package -- and then does `(setq-local
symbol-overlay-inhibit-map t)' unconditionally in
`magit-section-mode''s body.  Real Emacs accepts a `setq-local' (or
`set') of a symbol that has no binding yet; this substrate's
`setq-local' fallback expands through `set', which signals
`void-variable' for a never-bound name instead of creating it.  Giving
the declared name a real nil default here matches what any Emacs
session without the symbol-overlay package effectively observes."
  (unless (boundp 'symbol-overlay-inhibit-map)
    (defvar symbol-overlay-inhibit-map nil))
  ;; Same class, found by auditing every `setq-local' target in the
  ;; magit-section/magit-mode/magit-status mode bodies against a live
  ;; `boundp' probe: hook-function variables owned by host-preloaded or
  ;; optional packages (bookmark.el, imenu.el, isearch.el) that the mode
  ;; bodies overwrite unconditionally.  Real defaults name functions
  ;; from files this bridge never loads, so nil (= "package facility not
  ;; active", exactly what the vendor code then replaces) is the honest
  ;; default here.
  (unless (boundp 'bookmark-make-record-function)
    (defvar bookmark-make-record-function nil))
  (unless (boundp 'imenu-create-index-function)
    (defvar imenu-create-index-function nil))
  (unless (boundp 'imenu-default-goto-function)
    (defvar imenu-default-goto-function nil))
  (unless (boundp 'isearch-filter-predicate)
    (defvar isearch-filter-predicate nil))
  (unless (boundp 'which-function-mode)
    (defvar which-function-mode nil))
  (unless (boundp 'which-func-mode)
    (defvar which-func-mode nil))
  (unless (boundp 'which-func-functions)
    (defvar which-func-functions nil))
  (unless (boundp 'which-function-imenu-failed)
    (defvar which-function-imenu-failed nil))
  ;; `magit-mode''s body calls this `files.el' function unconditionally.
  ;; Dir-local variables are not modeled by this substrate at all, so a
  ;; documented no-op stand-in (NOT a copy) is faithful: with no
  ;; .dir-locals machinery there is nothing to hack in.
  (unless (fboundp 'hack-dir-local-variables-non-file-buffer)
    (defun hack-dir-local-variables-non-file-buffer ()
      "Stub: dir-local variables are not modeled by this substrate.
See `nelisp-emacs-magit-bridge--ensure-third-party-soft-vars'."
      nil))
  ;; `magit-mode''s body also calls `face-remap-add-relative'
  ;; (face-remap.el, host-preloaded) to restyle the header line.  Face
  ;; remapping is purely cosmetic display state with no consumer in this
  ;; substrate, so a documented no-op stand-in returning nil (the shape
  ;; of a remapping cookie consumers may later pass to
  ;; `face-remap-remove-relative') is faithful for M2/M3.
  (unless (fboundp 'face-remap-add-relative)
    (defun face-remap-add-relative (_face &rest _specs)
      "Stub: face remapping is not modeled by this substrate.
See `nelisp-emacs-magit-bridge--ensure-third-party-soft-vars'."
      nil)))

(defun nelisp-emacs-magit-bridge--ensure-docstring-fill-helpers ()
  "Ensure subr.el's docstring fill helpers used by `defclass' exist.

EIEIO's `defclass' macro calls `internal--format-docstring-line' (a
host-preloaded `subr.el' helper, same absence class as `static-if'
above) while building each accessor's docstring -- at MACRO EXPANSION
time, so with it missing every `(defclass ...)' in the whole vendor
chain fails, and inside the bundle's per-form load tolerance those
failures were completely silent: `magit-section'/`magit-status' classes
simply never registered, leaving `magit-insert-section--create' void
and every status-buffer refresh empty (0 bytes, root section nil).
Both helpers are copied verbatim from `vendor/emacs-lisp/subr.el' (not
reimplemented) and installed only when absent.  `fill-column' gets its
real Emacs default when unbound, since the fill helper reads it."
  (unless (boundp 'fill-column)
    (defvar fill-column 70))
  (unless (fboundp 'internal--fill-string-single-line)
    (defun internal--fill-string-single-line (str)
      "Fill string STR to `fill-column'.
This is intended for very simple filling while bootstrapping
Emacs itself, and does not support all the customization options
of fill.el (for example `fill-region')."
      (if (< (length str) fill-column)
          str
        (let* ((limit (min fill-column (length str)))
               (fst (substring str 0 limit))
               (lst (substring str limit)))
          (cond ((string-match "\\( \\)$" fst)
                 (setq fst (replace-match "\n" nil nil fst 1)))
                ((string-match "^ \\(.*\\)" lst)
                 (setq fst (concat fst "\n"))
                 (setq lst (match-string 1 lst)))
                ((string-match ".*\\( \\(.+\\)\\)$" fst)
                 (setq lst (concat (match-string 2 fst) lst))
                 (setq fst (replace-match "\n" nil nil fst 1))))
          (concat fst (internal--fill-string-single-line lst))))))
  (unless (fboundp 'internal--format-docstring-line)
    (defun internal--format-docstring-line (string &rest objects)
      "Format a single line from a documentation string out of STRING and OBJECTS.
Signal an error if STRING contains a newline.
This is intended for internal use only.  Avoid using this for the
first line of a docstring; the first line should be a complete
sentence (see Info node `(elisp) Documentation Tips')."
      (when (string-match "\n" string)
        (error "Unable to fill string containing newline: %S" string))
      (internal--fill-string-single-line (apply #'format string objects)))))

(defun nelisp-emacs-magit-bridge--ensure-special-mode ()
  "Ensure `special-mode' (real Emacs `simple.el') exists as a parent mode.

`simple.el' is preloaded/dumped by real Emacs (same class as `files.el'
and `uniquify.el' above), and `magit-section-mode' is
`(define-derived-mode magit-section-mode special-mode ...)' -- so the
first `(magit-status-mode)' activation walks its parent chain into a
`void-function special-mode'.  Body and `mode-class' property are
copied from `vendor/emacs-lisp/simple.el' (not reimplemented); the
keymap is built with plain `define-key' instead of `defvar-keymap'
(which this substrate lacks) but binds the same keys to the same
commands."
  (unless (fboundp 'special-mode)
    (let ((root (nelisp-emacs-magit-bridge--repo-root)))
      (unless (fboundp 'emacs-mode-define-derived-mode)
        (load (expand-file-name "src/emacs-mode.el" root)
              nil 'no-message t t))
      (unless (fboundp 'define-derived-mode)
        (load (expand-file-name "src/emacs-mode-builtins.el" root)
              nil 'no-message t t))
      (unless (fboundp 'derived-mode-p)
        (load (expand-file-name "src/emacs-stub.el" root)
              nil 'no-message t t)))
    (unless (boundp 'special-mode-map)
      (defvar special-mode-map
        (let ((map (make-sparse-keymap)))
          (when (fboundp 'suppress-keymap)
            (suppress-keymap map))
          (define-key map "q" 'quit-window)
          (define-key map " " 'scroll-up-command)
          (define-key map "\d" 'scroll-down-command)
          (define-key map "?" 'describe-mode)
          (define-key map "h" 'describe-mode)
          (define-key map ">" 'end-of-buffer)
          (define-key map "<" 'beginning-of-buffer)
          (define-key map "g" 'revert-buffer)
          map)))
    (put 'special-mode 'mode-class 'special)
    (define-derived-mode special-mode nil "Special"
      "Parent major mode from which special major modes should inherit.

A special major mode is intended to view specially formatted data
rather than files.  These modes usually use read-only buffers."
      (setq buffer-read-only t))))

(defun nelisp-emacs-magit-bridge--ensure-current-buffer ()
  "Ensure the session has a live current buffer, like real Emacs always does.

Real Emacs guarantees `(current-buffer)' is never nil -- a bare session
starts inside *scratch*.  The nemacs batch bootstrap leaves this image
with an empty `buffer-list' and a nil `current-buffer' until the first
explicit buffer switch, so any code path that hands the \"currently
displayed\" buffer around hits `wrong-type-argument (nelisp-ec-buffer-p
nil)' -- M2 probing hit this inside `magit-get-mode-buffer', whose
window-scan maps `window-buffer' over `window-list' and passes each
result straight to `with-current-buffer' (with the window substrate's
own buffer slot falling back to `current-buffer', i.e. nil here).
Creating and selecting *scratch* is a session-level precondition fix
mirroring the real Emacs startup invariant, not a vendor patch."
  (when (and (fboundp 'current-buffer)
             (fboundp 'get-buffer-create)
             (fboundp 'set-buffer)
             (null (current-buffer)))
    (set-buffer (get-buffer-create "*scratch*"))))

(defun nelisp-emacs-magit-bridge--bufferp (object)
  "Return non-nil when OBJECT is any supported runtime buffer object."
  (or (and (consp object)
           (memq (car object) '(buffer nelisp-ec-buffer)))
      (and (fboundp 'nelisp-ec-buffer-p)
           (ignore-errors (nelisp-ec-buffer-p object)))))

(defun nelisp-emacs-magit-bridge--ensure-buffer-predicate ()
  "Ensure `bufferp' accepts standalone `nelisp-ec-buffer' objects."
  (fset 'bufferp
        (symbol-function
         'nelisp-emacs-magit-bridge--bufferp)))

(defun nelisp-emacs-magit-bridge--ensure-buffer-builtins ()
  "Ensure unprefixed buffer-editing functions use the standalone runtime."
  (let ((root (nelisp-emacs-magit-bridge--repo-root)))
    (unless (fboundp 'emacs-buffer-set-buffer-modified-p)
      (load (expand-file-name "src/emacs-buffer.el" root)
            nil 'no-message t t)))
  (dolist (cell '((generate-new-buffer . nelisp-ec-generate-new-buffer)
                  (kill-buffer . nelisp-ec-kill-buffer)
                  (bufferp . nelisp-ec-buffer-p)
                  (current-buffer . nelisp-ec-current-buffer)
                  (set-buffer . nelisp-ec-set-buffer)
                  (point . nelisp-ec-point)
                  (point-min . nelisp-ec-point-min)
                  (point-max . nelisp-ec-point-max)
                  (goto-char . nelisp-ec-goto-char)
                  (buffer-size . nelisp-ec-buffer-size)
                  (insert . nelisp-ec-insert)
                  (insert-and-inherit . nelisp-ec-insert)
                  (erase-buffer . nelisp-ec-erase-buffer)
                  (delete-region . nelisp-ec-delete-region)
                  (buffer-string . nelisp-ec-buffer-string)
                  (buffer-substring . nelisp-ec-buffer-substring)
                  (buffer-substring-no-properties . nelisp-ec-buffer-substring)
                  (narrow-to-region . nelisp-ec-narrow-to-region)
                  (widen . nelisp-ec-widen)
                  (make-marker . nelisp-ec-make-marker)
                  (markerp . nelisp-ec-marker-p)
                  (set-marker . nelisp-ec-set-marker)
                  (move-marker . nelisp-ec-set-marker)
                  (marker-position . nelisp-ec-marker-position)
                  (marker-buffer . nelisp-ec-marker-buffer)
                  (marker-insertion-type . nelisp-ec-marker-insertion-type)
                  (set-marker-insertion-type . nelisp-ec-set-marker-insertion-type)
                  (point-marker . nelisp-ec-point-marker)
                  (insert-before-markers . nelisp-ec-insert)
                  (buffer-modified-p . emacs-buffer-buffer-modified-p)
                  (set-buffer-modified-p . emacs-buffer-set-buffer-modified-p)
                  (restore-buffer-modified-p . emacs-buffer-restore-buffer-modified-p)))
    (when (fboundp (cdr cell))
      (fset (car cell) (symbol-function (cdr cell))))))

(defun nelisp-emacs-magit-bridge--ensure-buffer-selection-builtins ()
  "Ensure current-buffer selection helpers use the standalone runtime.

Keep this narrower than `nelisp-emacs-magit-bridge--ensure-buffer-builtins':
the vendor load path only needs the session-level current-buffer invariant
before Magit's bundle is loaded, while the broader point/marker/editing
surface is safer to reassert after bundle load."
  (dolist (cell '((bufferp . nelisp-ec-buffer-p)
                  (current-buffer . nelisp-ec-current-buffer)
                  (set-buffer . nelisp-ec-set-buffer)))
    (when (fboundp (cdr cell))
      (fset (car cell) (symbol-function (cdr cell))))))

(defun nelisp-emacs-magit-bridge--ensure-keymap-constructors ()
  "Ensure unprefixed keymap constructors return usable keymap objects.

The base runtime image can still carry nil-stubs for `make-keymap' /
`make-sparse-keymap' / `suppress-keymap'.  Magit builds mode maps
during feature load, so a nil-returning constructor aborts early with
`wrong-type-argument consp nil'.  Keep the repair minimal and local to
the bridge: constructors return the canonical `(keymap ...)' list shape
already accepted by the current `define-key' substrate, and
`suppress-keymap' becomes a no-op that preserves the map object."
  (let ((root (nelisp-emacs-magit-bridge--repo-root)))
    (unless (fboundp 'emacs-keymap-define-key)
      (load (expand-file-name "src/emacs-keymap.el" root)
            nil 'no-message t t))
    (unless (fboundp 'define-key)
      (load (expand-file-name "src/emacs-keymap-builtins.el" root)
            nil 'no-message t t)))
  (let ((probe
         (condition-case nil
             (and (fboundp 'make-sparse-keymap)
                  (let ((map (make-sparse-keymap)))
                    (and (consp map) (eq (car map) 'keymap) map)))
           (error nil))))
    (unless (fboundp 'keymapp)
      (fset 'keymapp
            '(lambda (object)
               (and (consp object) (eq (car object) 'keymap)))))
    (unless (fboundp 'map-keymap)
      (cond
       ((fboundp 'emacs-keymap-map-keymap)
        (fset 'map-keymap
              (symbol-function 'emacs-keymap-map-keymap)))
       (t
        (fset 'map-keymap
              '(lambda (_function _keymap)
                 nil)))))
    (unless (fboundp 'map-keymap-internal)
      (fset 'map-keymap-internal
            (symbol-function 'map-keymap)))
    (unless (fboundp 'make-composed-keymap)
      (fset 'make-composed-keymap
            '(lambda (maps &optional parent)
               (let ((head (cond
                            ((and (fboundp 'keymapp) (keymapp maps)) maps)
                            ((and (listp maps) maps) (car maps))
                            (maps maps)
                            (t parent))))
                 (or head parent (list 'keymap))))))
    (unless probe
      (fset 'make-keymap
            '(lambda (&optional _prompt)
               (list 'keymap)))
      (fset 'make-sparse-keymap
            '(lambda (&optional _prompt)
               (list 'keymap)))
      (fset 'suppress-keymap
            '(lambda (keymap &optional _nodigits)
               keymap)))))

(defun nelisp-emacs-magit-bridge--ensure-save-some-buffers ()
  "Ensure `save-some-buffers' (real Emacs `files.el') exists.

`files.el' is preloaded/dumped by real Emacs (same host-preload-gap
class as `-ensure-files-el-globals' above), so it never shows up as a
newly-loaded file in this bridge's bundle, but the substrate never
preloads it either.  `magit-save-repository-buffers'
(`vendor/magit/lisp/magit-mode.el') calls `(save-some-buffers ARG
PRED)' as a precondition before refreshing/opening the status buffer
-- found once the M2 buffer-local swap-engine fix (Doc 33 §8 item 242)
unblocked the earlier `text-read-only' abort and status-buffer setup
ran far enough to reach it.  Real `save-some-buffers' walks every live,
file-visiting, modified buffer PRED selects and interactively prompts
to save each one; this bridge's M2/M3 status-buffer smoke never opens
an Emacs buffer on a modified file (the fixture's unstaged edit and
untracked files are plain filesystem state the whole time) and has no
minibuffer-prompt loop to drive one anyway, so a no-op returning nil
-- matching what real Emacs itself returns when there is nothing to
save -- is a faithful stand-in for this read-only path, not a vendor
patch."
  (unless (fboundp 'save-some-buffers)
    (defun save-some-buffers (&optional _arg _pred)
      "Stub: no interactive buffer-save prompt loop is modeled.
See `nelisp-emacs-magit-bridge--ensure-save-some-buffers'."
      nil)))

;; Doc 155 §8.13 / nelisp Policy B (retained-generation growth-chunk boxes,
;; vendor/nelisp commit range fix/gc-retention-edge-magit) removed the need
;; for a bridge-side raw memory poke here.  Doc 33 item 244 found that
;; replaying the full vendor Magit bundle with real EIEIO class registration
;; live made the standalone runtime SIGSEGV partway through the load with
;; the Doc 155 signature (`nl_vector_slot_ptr' NULL deref under
;; `nelisp_frame_stack_find_in_frame': a live lexframe's hash-table/buckets
;; child reclaimed by the form-boundary collector while the frame record
;; survives) -- a further instance of the marker-gap class Path B
;; (b31179ce) had already closed once.  Formerly this bridge worked around
;; it with `nelisp-emacs-magit-bridge--ensure-gc-collect-disabled', a
;; session-scoped `ptr-read-u64'/`ptr-write-u64' poke of the vendor
;; binary's RECLAIMER GATE (base+160) -- effectively disabling the
;; standalone runtime's GC reclaimer for the whole session (Doc 155 §8.8
;; "Fix A", the sound-but-blunt stopgap).  The vendor core now ships the
;; real fix instead: the form-boundary collector treats every growth-chunk
;; (non-chunk-0) box it would otherwise free as belonging to a retained
;; generation, exactly like the existing chunk-0 boot watermark, so it can
;; never wrongly free a live growth-chunk child.  `(garbage-collect)' and
;; the mid-form loop collector are untouched by this policy and keep
;; reclaiming growth-chunk garbage normally.  With the core sound by
;; construction, this bridge no longer needs to (and no longer does) touch
;; the vendor binary's internal GC gate at all.

(defun nelisp-emacs-magit-bridge--ensure-lambda-documentation-form ()
  "Make a lambda-body-leading `(:documentation ...)' form evaluate harmlessly.

Real Emacs treats `(:documentation FORM)' at the head of a lambda body
as a special dynamic-docstring construct (handled by cconv/oclosure
machinery, never actually *called*).  EIEIO generates exactly that
shape for every class it defines -- `eieio-defclass-internal''s
backward-compatible `NAME-list-p' defalias and the class predicates
funnel through `(lambda (obj) (:documentation ...) ...)' -- and the
standalone NeLisp evaluator, which has no special handling for the
construct, evaluates it as an ordinary function call of the keyword
`:documentation' the first time such a predicate is INVOKED (found on
the M2 status-buffer path right after the item 244 `copy-alist' fix
let those EIEIO predicates be defined for real:
`(void-function :documentation)').  Binding the keyword's function
cell to an ignore-everything lambda makes the doc form a cheap no-op
(its argument is a docstring literal or a pure formatting call) while
leaving the body's real forms untouched -- the same observable
behavior real Emacs has at call time, where the construct contributes
nothing to the function's return value."
  (unless (fboundp :documentation)
    (defalias :documentation (lambda (&rest _) nil))))

(defun nelisp-emacs-magit-bridge--ensure-magit-insert-headers ()
  "Install a hook-and-closure-free `magit-insert-headers' after the bundle loads.

Real `magit-insert-headers' (`vendor/magit/lisp/magit-section.el',
excluded from this bundle — see `nelisp-emacs-magit-bridge-bundle-
excluded-defuns' in `scripts/build-nelisp-emacs-magit-bridge-bundle.el')
collects the top-level sections a header-hook run inserted by
`add-hook'ing a short closure onto `magit-insert-section-hook' at depth
-90 that does `(push magit-insert-section--current header-sections)',
then regroups those sections (making the first one the parent of the
rest) once the hook run finishes.

Doc 33 item 244 bisection found that this closure — created while
`magit-insert-section--current' already has an ACTIVE outer dynamic
binding (the enclosing status-buffer root section) — always reads back
that OUTER (root) value instead of the correctly re-bound INNER value
once actually invoked from within a still more deeply nested re-binding
of the same variable (each individual header line's own section): a
NeLisp interpreter gap in how closures resolve a special variable
across more than one level of nested dynamic re-binding, reproduced
with a minimal repro that needs neither Magit, EIEIO, nor `add-hook'/
`run-hooks' (plain nested `let' forms over an ordinary `defvar' already
exhibit it — see `docs/design/33-emacs-core-substrate-priority-
plan.org' item 244 for the full bisection).  Every section the original
closure collected therefore turns out to be the (status) root itself,
whose own `parent' slot is nil, so the regroup step's `(oset
header-parent children ...)' aborts with `(wrong-type-argument (or
eieio-object cl-structure-object oclosure) nil)' the moment it tries to
write a slot on that nil `header-parent'.  This is a core interpreter
gap, out of this bridge's scope to fix.

This replacement collects the SAME set of sections a different, hook-
and-closure-free way that sidesteps the gap entirely: `magit-insert-
section--finish' (an ordinary function, not a closure) already appends
each newly finished section directly onto its real parent's `children'
slot before returning — a plain, non-closure `oref'/`oset' round trip
that Doc 33 item 244 confirmed is reliable.  Snapshotting the parent's
child count before running HOOK and taking the newly appended tail
afterward yields exactly the sections HOOK inserted, in the same
creation order the original closure's `nreverse'd accumulator produced
(assumes normal append-order insertion; `magit-section-insert-in-
reverse' is a log-rendering knob that headers never bind).  The
regroup logic below is copied verbatim from the original; only the
collection mechanism differs."
  (unless (fboundp 'magit-insert-headers)
    (defun magit-insert-headers (hook)
      (let* ((parent magit-insert-section--current)
             (before (length (oref parent children)))
             header-sections)
        (magit-run-section-hook hook)
        (setq header-sections (nthcdr before (oref parent children)))
        (when header-sections
          (insert "\n")
          (when (cdr header-sections)
            (let* ((1st-header (pop header-sections))
                   (header-parent (oref 1st-header parent)))
              (oset header-parent children (list 1st-header))
              (oset 1st-header children header-sections)
              (oset 1st-header content (oref (car header-sections) start))
              (oset 1st-header end (oref (car (last header-sections)) end))
              (dolist (sub-header header-sections)
                (oset sub-header parent 1st-header))
              (magit-section-maybe-add-heading-map 1st-header))))))))

(defun nelisp-emacs-magit-bridge--magit-buffer-name-default-function
    (mode &optional value)
  "NeLisp-safe replacement for Magit's default buffer-name formatter."
  (let* ((m (substring (symbol-name mode) 0 -5))
         (v (and value (format "%s" (ensure-list value))))
         (n (if magit-uniquify-buffer-names
                (file-name-nondirectory
                 (directory-file-name default-directory))
              (abbreviate-file-name default-directory)))
         (x (if magit-uniquify-buffer-names "" "*"))
         (M (if (eq mode 'magit-status-mode) "magit" m)))
    (concat x M (or v "") ": " n x)))

(defun nelisp-emacs-magit-bridge--ensure-magit-buffer-name-default ()
  "Avoid `format-spec' for Magit's default buffer name format.

The vendor default `magit-generate-buffer-name-default-function' formats
the fixed default `magit-buffer-name-format' through `format-spec', whose
vendor implementation uses temporary buffer editing.  That path is still
fragile in NeLisp's Magit runtime.  M2 uses the default format, so generate
the equivalent name directly and leave non-default custom formats to the
vendor function once the `format-spec' substrate is fixed."
  (fset 'magit-generate-buffer-name-default-function
        (symbol-function
         'nelisp-emacs-magit-bridge--magit-buffer-name-default-function)))

(defun nelisp-emacs-magit-bridge--ensure-magit-margin-hook-safe ()
  "Disable Magit's margin hook in the non-TUI runtime image.

`magit-set-buffer-margins' is display-only.  It currently trips NeLisp's
minimal window/margin substrate while `magit-status-smoke' is proving the
TUI-independent status buffer and section model, so remove it from the setup
hook instead of letting a visual margin path abort status creation."
  (when (and (boundp 'magit-setup-buffer-hook)
             (listp magit-setup-buffer-hook))
    (setq magit-setup-buffer-hook
          (delq 'magit-set-buffer-margins magit-setup-buffer-hook))))

(defun nelisp-emacs-magit-bridge--ensure-magit-save-hooks-safe ()
  "Disable Magit's repository-save hooks in the standalone smoke path.

These hooks are interactive safety helpers, not part of the status/stage
buffer model itself.  In the standalone runtime they currently trip bridge
gaps before any save-worthy modified Emacs buffer exists."
  (dolist (symbol '(magit-setup-buffer-hook
                    magit-pre-refresh-hook
                    magit-pre-call-git-hook
                    magit-pre-start-git-hook))
    (when (and (boundp symbol)
               (listp (symbol-value symbol)))
      (set symbol
           (delq 'magit-maybe-save-repository-buffers
                 (symbol-value symbol))))))

(defun nelisp-emacs-magit-bridge--ensure-magit-status-sections-hook-safe ()
  "Trim `magit-status-sections-hook' to the stable stage-workflow subset."
  (when (boundp 'magit-status-sections-hook)
    (setq magit-status-sections-hook
          '(magit-insert-unstaged-changes
            magit-insert-staged-changes))))

(defun nelisp-emacs-magit-bridge--ensure-magit-insert-section-hook-safe ()
  "Disable `magit-insert-section-hook' for the standalone status smoke path."
  (when (boundp 'magit-insert-section-hook)
    (setq magit-insert-section-hook nil)))

(defun nelisp-emacs-magit-bridge--ensure-magit-refresh-buffer-hook-safe ()
  "Disable `magit-refresh-buffer-hook' in the standalone status smoke path."
  (when (boundp 'magit-refresh-buffer-hook)
    (setq magit-refresh-buffer-hook nil)))

(defun nelisp-emacs-magit-bridge--ensure-magit-highlight-safe ()
  "Disable section highlight work for the standalone status smoke path."
  (setq magit-section-highlight-current nil)
  (setq magit-section-highlight-selection nil)
  (fset 'magit-section-update-highlight
        (lambda (&optional _force) nil)))

(defun nelisp-emacs-magit-bridge--repo-buffer-topdir ()
  "Return the current repository topdir when cheaply available."
  (or (and (boundp 'nelisp-emacs-magit-bridge--repo-root)
           nelisp-emacs-magit-bridge--repo-root)
      (and (boundp 'magit--default-directory)
           magit--default-directory)
      (let ((dir default-directory))
        (condition-case nil
            (magit--toplevel-safe)
          (error (and dir (file-name-as-directory dir)))))))

(defun nelisp-emacs-magit-bridge--mode-buffer-match-p
    (buffer mode topdir value)
  "Return BUFFER when it matches MODE/TOPDIR/VALUE constraints."
  (when (nelisp-emacs-magit-bridge--bufferp buffer)
    (with-current-buffer buffer
      (and (eq major-mode mode)
           (or (not topdir)
               (equal magit--default-directory topdir))
           (if value
               (and magit-buffer-locked-p
                    (equal (magit-buffer-value) value))
             (not magit-buffer-locked-p))
           buffer))))

(defun nelisp-emacs-magit-bridge--find-mode-buffer-in-windows
    (windows mode topdir value)
  "Return the first matching MODE buffer visible in WINDOWS."
  (catch 'found
    (dolist (window windows)
      (let* ((buffer (window-buffer window))
             (match (nelisp-emacs-magit-bridge--mode-buffer-match-p
                     buffer mode topdir value)))
        (when match
          (throw 'found match))))
    nil))

(defun nelisp-emacs-magit-bridge--find-mode-buffer-in-frames
    (frames mode topdir value)
  "Return the first matching MODE buffer visible in FRAMES."
  (catch 'found
    (dolist (one-frame frames)
      (let ((match
             (nelisp-emacs-magit-bridge--find-mode-buffer-in-windows
              (window-list one-frame 'no-minibuf)
              mode topdir value)))
        (when match
          (throw 'found match))))
    nil))

(defun nelisp-emacs-magit-bridge--magit-get-mode-buffer
    (mode &optional value frame)
  "NeLisp-safe replacement for `magit-get-mode-buffer'."
  (let ((topdir (nelisp-emacs-magit-bridge--repo-buffer-topdir))
        (result nil))
    (setq result
          (cond
           ((null frame)
            (or (catch 'found
                  (dolist (buffer (buffer-list))
                    (when (nelisp-emacs-magit-bridge--bufferp buffer)
                      (let ((match
                             (nelisp-emacs-magit-bridge--mode-buffer-match-p
                              buffer mode topdir value)))
                        (when match
                          (throw 'found match)))))
                  nil)
                (and (not value)
                     (catch 'found
                       (dolist (buffer (buffer-list))
                         (when (nelisp-emacs-magit-bridge--bufferp buffer)
                           (with-current-buffer buffer
                             (when (eq major-mode mode)
                               (throw 'found buffer)))))
                       nil))))
           ((eq frame 'all)
            (nelisp-emacs-magit-bridge--find-mode-buffer-in-frames
             (frame-list) mode topdir value))
           ((eq frame 'visible)
            (nelisp-emacs-magit-bridge--find-mode-buffer-in-frames
             (visible-frame-list) mode topdir value))
           ((or (eq frame 'selected) (eq frame t))
            (nelisp-emacs-magit-bridge--find-mode-buffer-in-windows
             (window-list (selected-frame)) mode topdir value))
           ((framep frame)
            (nelisp-emacs-magit-bridge--find-mode-buffer-in-windows
             (window-list frame) mode topdir value))
           (t nil)))
    result))

(defun nelisp-emacs-magit-bridge--run-section-hook (hook)
  "Run Magit section HOOK with one trace marker per function."
  (dolist (fn (and (boundp hook) (symbol-value hook)))
    (nelisp-emacs-magit-bridge--trace
     "section-hook %S BEGIN %S" hook fn)
    (condition-case err
        (progn
          (funcall fn)
          (nelisp-emacs-magit-bridge--trace
           "section-hook %S PASS %S" hook fn))
      (error
       (nelisp-emacs-magit-bridge--trace
        "section-hook %S ERROR %S %S" hook fn err)
       (error "%S" err)))))

(defun nelisp-emacs-magit-bridge--magit-stage-1 (arg &optional files)
  "NeLisp-safe synchronous replacement for `magit-stage-1'."
  (nelisp-emacs-magit-bridge--trace
   "stage-1 BEGIN arg=%S files=%S" arg files)
  (magit-run-before-change-functions files "stage")
  (let* ((argv (append (list "add")
                       (delq nil (list arg))
                       (if files
                           (append '("--") files)
                         '("."))))
         (process-environment (magit-process-environment))
         (default-process-coding-system (magit--process-coding-system))
         (rc (apply #'nelisp-emacs-magit-bridge--git-exit-code argv)))
    (nelisp-emacs-magit-bridge--trace
     "stage-1 git PASS rc=%S argv=%S" rc argv)
    (when (and (boundp 'magit-auto-revert-mode)
               magit-auto-revert-mode
               files)
      (mapc #'magit-turn-on-auto-revert-mode-if-desired files))
    (magit-run-after-apply-functions files "stage")
    rc))

(defun nelisp-emacs-magit-bridge--magit-stage (&optional intent)
  "NeLisp-safe replacement for `magit-stage' on common status-buffer paths."
  (interactive "P")
  (let* ((section (and (derived-mode-p 'magit-mode)
                       (magit-current-section)))
         (type (magit-diff-type))
         (scope (magit-diff-scope)))
    (nelisp-emacs-magit-bridge--trace
     "stage BEGIN type=%S scope=%S section=%S"
     type scope (and section (oref section type)))
    (cond
     ((eq type 'untracked)
      (magit-stage-untracked intent))
     ((not (eq type 'unstaged))
      (cond
       ((eq type 'staged) (user-error "Already staged"))
       ((eq type 'committed) (user-error "Cannot stage committed changes"))
       (t (user-error "Cannot stage this change"))))
     ((eq scope 'file)
      (magit-stage-1 "-u" (and section (list (oref section value)))))
     ((eq scope 'files)
      (magit-stage-1 "-u" (magit-region-values nil t)))
     ((eq scope 'list)
      (magit-stage-1 "-u" magit-buffer-diff-files))
     (nelisp-emacs-magit-bridge--magit-stage-orig
      (funcall nelisp-emacs-magit-bridge--magit-stage-orig intent))
     (t
     (call-interactively #'magit-stage-files)))))

(defun nelisp-emacs-magit-bridge--resolve-git-executable ()
  "Return an absolute git executable path when available."
  (or (and (file-executable-p "/usr/bin/git")
           "/usr/bin/git")
      (and (file-executable-p "/bin/git")
           "/bin/git")
      (and (file-executable-p "/usr/local/bin/git")
           "/usr/local/bin/git")
      (let ((candidate (and (fboundp 'magit-git-executable)
                            (ignore-errors (magit-git-executable)))))
        (cond
         ((and (stringp candidate)
               (file-name-absolute-p candidate)
               (file-executable-p candidate))
          candidate)
         ((and (fboundp 'executable-find)
               (ignore-errors (executable-find "git"))))
         (t
         (or candidate "git"))))))

(defun nelisp-emacs-magit-bridge--ensure-magit-git-executable ()
  "Pin `magit-git-executable' to a runnable local git when possible."
  (let ((git (nelisp-emacs-magit-bridge--resolve-git-executable)))
    (when (and (boundp 'magit-git-executable)
               (stringp git)
               (not (string-empty-p git)))
      (setq magit-git-executable git))))

(defun nelisp-emacs-magit-bridge--ensure-magit-toplevel-safe ()
  "Replace `magit-toplevel' with a path-safe implementation for live sessions."
  (when (featurep 'magit-git)
    (defun magit-toplevel (&optional directory)
      "Return the repository toplevel for DIRECTORY or `default-directory'."
      (let* ((dir (file-name-as-directory
                   (or directory
                       (and (boundp 'magit--default-directory)
                            magit--default-directory)
                       default-directory)))
             (default-directory dir)
             (topdir (or (magit-git-string "rev-parse" "--show-toplevel")
                         (magit-rev-parse-safe "--show-toplevel"))))
        (cond
         (topdir
          (file-name-as-directory
           (if (file-name-absolute-p topdir)
               topdir
             (expand-file-name topdir dir))))
         ((let ((gitdir (magit-git-string "rev-parse" "--git-dir")))
            (when gitdir
              (setq gitdir
                    (file-name-as-directory
                     (if (file-name-absolute-p gitdir)
                         gitdir
                       (expand-file-name gitdir dir))))
              (if (magit-bare-repo-p)
                  gitdir
                (file-name-directory (directory-file-name gitdir))))))
         (t nil))))))

(defun nelisp-emacs-magit-bridge--ensure-cl-compat-increments ()
  "Provide legacy `incf' / `decf' names expected by some vendor paths."
  (when (and (not (fboundp 'incf))
             (fboundp 'cl-incf))
    (defalias 'incf #'cl-incf))
  (when (and (not (fboundp 'decf))
             (fboundp 'cl-decf))
    (defalias 'decf #'cl-decf)))

(defun nelisp-emacs-magit-bridge--standalone-setf-extra-place (place val)
  "Return an expanded assignment form for standalone `setf' PLACE and VAL.

This extends the bare-reader `nelisp--setf-1' place family with the common
compound list accessors and plist helpers used by real upstream packages."
  (when (consp place)
    (let ((fn (car place))
          (args (cdr place)))
      (cond
       ((eq fn 'caar)
        (list 'setcar (list 'car (car args)) val))
       ((eq fn 'cadr)
        (list 'setcar (list 'cdr (car args)) val))
       ((eq fn 'cdar)
        (list 'setcdr (list 'car (car args)) val))
       ((eq fn 'cddr)
        (list 'setcdr (list 'cdr (car args)) val))
       ((eq fn 'nthcdr)
        (list 'setcdr
              (list 'nthcdr (list '1- (car args)) (cadr args))
              val))
       ((eq fn 'caaar)
        (list 'setcar (list 'caar (car args)) val))
       ((eq fn 'caadr)
        (list 'setcar (list 'cadr (car args)) val))
       ((eq fn 'cadar)
        (list 'setcar (list 'cdar (car args)) val))
       ((eq fn 'caddr)
        (list 'setcar (list 'cddr (car args)) val))
       ((eq fn 'cdaar)
        (list 'setcdr (list 'caar (car args)) val))
       ((eq fn 'cdadr)
        (list 'setcdr (list 'cadr (car args)) val))
       ((eq fn 'cddar)
        (list 'setcdr (list 'cdar (car args)) val))
       ((eq fn 'cdddr)
        (list 'setcdr (list 'cddr (car args)) val))
       ((eq fn 'cl-getf)
        (list 'setf (car args)
              (list 'plist-put (car args) (cadr args) val)))
       ((eq fn 'plist-get)
        (list 'plist-put (car args) (cadr args) val))
       ((eq fn 'elt)
        (let ((seqsym (make-symbol "seq")))
          (list 'let
                (list (list seqsym (car args)))
                (list 'if
                      (list 'listp seqsym)
                      (list 'setcar (list 'nthcdr (cadr args) seqsym) val)
                      (list 'aset seqsym (cadr args) val)))))))))

(defun nelisp-emacs-magit-bridge--ensure-standalone-setf-places ()
  "Extend the standalone `setf' place expander used by baked runtime images."
  (when (and (fboundp 'nelisp--setf-1)
             (null nelisp-emacs-magit-bridge--nelisp-setf-1-orig))
    (setq nelisp-emacs-magit-bridge--nelisp-setf-1-orig
          (symbol-function 'nelisp--setf-1))
    (defun nelisp--setf-1 (place val)
      (or (nelisp-emacs-magit-bridge--standalone-setf-extra-place place val)
          (funcall nelisp-emacs-magit-bridge--nelisp-setf-1-orig place val)))))

(defun nelisp-emacs-magit-bridge--shell-quote (string)
  "Return STRING safely single-quoted for `/bin/sh -c'."
  (if (fboundp 'shell-quote-argument)
      (shell-quote-argument string)
    (concat "'"
            (replace-regexp-in-string "'" "'\"'\"'" string t t)
            "'")))

(defun nelisp-emacs-magit-bridge--shell-token (string)
  "Return STRING as a shell token.

Use the raw token for simple path/flag characters because the standalone
`/bin/sh -c' path has proven unreliable with aggressively single-quoted
argv fragments, while Magit's git commands here are plain ASCII tokens."
  string)

(defun nelisp-emacs-magit-bridge--temp-file (prefix)
  "Return a temporary file path for PREFIX."
  (let* ((build-dir (expand-file-name "build"
                                      (nelisp-emacs-magit-bridge--repo-root)))
         (stamp (replace-regexp-in-string
                 "[^0-9]" ""
                 (format "%s" (float-time)))))
    (make-directory build-dir t)
    (expand-file-name
     (format "%s-%s.tmp" prefix stamp)
     build-dir)))

(defun nelisp-emacs-magit-bridge--read-file-string (file)
  "Return FILE contents as a string, or an empty string when unreadable."
  (cond
   ((not (and (stringp file)
              (file-readable-p file)))
    "")
   ((fboundp 'nelisp--syscall-read-file)
    (or (nelisp--syscall-read-file file) ""))
   ((fboundp 'nl-syscall-read-file)
    (or (nl-syscall-read-file file 0 nil) ""))
   (t
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string)))))

(defun nelisp-emacs-magit-bridge--call-process-real-destination (destination)
  "Return the stdout destination part of DESTINATION."
  (if (and (consp destination)
           (not (eq (car destination) :file)))
      (car destination)
    destination))

(defun nelisp-emacs-magit-bridge--call-process-stderr-destination (destination)
  "Return the stderr destination part of DESTINATION, if any."
  (and (consp destination)
       (not (eq (car destination) :file))
       (cadr destination)))

(defun nelisp-emacs-magit-bridge--call-process-target-buffer (destination)
  "Resolve DESTINATION to a target buffer, or nil."
  (let ((real (nelisp-emacs-magit-bridge--call-process-real-destination
               destination)))
    (cond
     ((or (null real) (eq real 0)) nil)
     ((eq real t) (current-buffer))
     ((and (consp real) (eq (car real) :file)) nil)
     ((stringp real) (get-buffer-create real))
     ((and (fboundp 'bufferp) (bufferp real)) real)
     (t nil))))

(defun nelisp-emacs-magit-bridge--call-process-target-file (destination)
  "Resolve DESTINATION to a target file, or nil."
  (let ((real (nelisp-emacs-magit-bridge--call-process-real-destination
               destination)))
    (and (consp real)
         (eq (car real) :file)
         (stringp (cadr real))
         (cadr real))))

(defun nelisp-emacs-magit-bridge--write-string-to-file (text file)
  "Write TEXT to FILE, replacing any existing contents."
  (with-temp-buffer
    (insert text)
    (write-region (point-min) (point-max) file nil 'silent)))

(defun nelisp-emacs-magit-bridge--insert-call-process-output
    (destination output)
  "Insert or write OUTPUT according to DESTINATION."
  (let ((target (nelisp-emacs-magit-bridge--call-process-target-buffer
                 destination))
        (file (nelisp-emacs-magit-bridge--call-process-target-file
               destination)))
    (cond
     (file
      (nelisp-emacs-magit-bridge--write-string-to-file output file))
     (target
     (with-current-buffer target
        (insert output))))))

(defun nelisp-emacs-magit-bridge--materialize-destination (destination)
  "Replace `t' destinations in DESTINATION with the current buffer object."
  (let ((stdout (nelisp-emacs-magit-bridge--call-process-real-destination
                 destination))
        (stderr (nelisp-emacs-magit-bridge--call-process-stderr-destination
                 destination)))
    (cond
     ((and (consp destination)
           (not (eq (car destination) :file)))
      (list (if (eq stdout t) (current-buffer) stdout)
            (if (eq stderr t) (current-buffer) stderr)))
     ((eq destination t)
      (current-buffer))
     (t destination))))

(defun nelisp-emacs-magit-bridge--call-process-via-shell
    (program infile destination args)
  "Standalone fallback for sync subprocess capture.

Run PROGRAM with ARGS from `default-directory' using the low-level
`nelisp-process-call-process' primitive, redirecting stdout/stderr to temp
files and then replaying them into DESTINATION with Emacs-compatible
destination semantics."
  (let* ((stdout-file (nelisp-emacs-magit-bridge--temp-file
                       "nemacs-call-process-out"))
         (stderr-file (nelisp-emacs-magit-bridge--temp-file
                       "nemacs-call-process-err"))
         (destination
          (nelisp-emacs-magit-bridge--materialize-destination destination))
         (dir (file-name-as-directory default-directory))
         (command
          (mapconcat
           #'identity
            (append
            (list "cd"
                  (nelisp-emacs-magit-bridge--shell-token dir)
                  "&&"
                  (nelisp-emacs-magit-bridge--shell-token program))
            (mapcar #'nelisp-emacs-magit-bridge--shell-token args)
            (when (stringp infile)
              (list "<" (nelisp-emacs-magit-bridge--shell-token infile)))
            (list ">" (nelisp-emacs-magit-bridge--shell-token stdout-file)
                  "2>" (nelisp-emacs-magit-bridge--shell-token stderr-file)))
           " "))
         (rc nil))
    (unwind-protect
        (progn
          (setq rc
                (nelisp-process-call-process
                 "/bin/sh" nil nil nil "-c" command))
          (let ((stdout (nelisp-emacs-magit-bridge--read-file-string stdout-file))
                (stderr (nelisp-emacs-magit-bridge--read-file-string stderr-file))
                (stderr-destination
                 (nelisp-emacs-magit-bridge--call-process-stderr-destination
                  destination)))
            (when (> (length stdout) 0)
              (nelisp-emacs-magit-bridge--insert-call-process-output
               destination stdout))
            (when (and (> (length stderr) 0)
                       stderr-destination)
              (nelisp-emacs-magit-bridge--insert-call-process-output
               stderr-destination stderr)))
          (if (integerp rc) rc 0))
      (ignore-errors
        (when (file-exists-p stdout-file)
          (delete-file stdout-file)))
      (ignore-errors
        (when (file-exists-p stderr-file)
          (delete-file stderr-file))))))

(defun nelisp-emacs-magit-bridge--call-process-safe
    (program &optional infile destination display &rest args)
  "Bridge-owned sync subprocess capture fallback for `call-process'."
  (ignore display)
  (if (and (fboundp 'emacs-standalone-mode-p)
           (emacs-standalone-mode-p)
           (fboundp 'nelisp-process-call-process)
           (or (not (fboundp 'emacs-process--standalone-capture-available-p))
               (not (emacs-process--standalone-capture-available-p))))
      (nelisp-emacs-magit-bridge--call-process-via-shell
       program infile destination args)
    (apply #'emacs-process-call-process
           program infile destination display args)))

(defun nelisp-emacs-magit-bridge--process-file-safe
    (program &optional infile destination display &rest args)
  "Bridge-owned sync subprocess capture fallback for `process-file'."
  (ignore display)
  (if (and (fboundp 'emacs-standalone-mode-p)
           (emacs-standalone-mode-p)
           (fboundp 'nelisp-process-call-process)
           (or (not (fboundp 'emacs-process--standalone-capture-available-p))
               (not (emacs-process--standalone-capture-available-p))))
      (nelisp-emacs-magit-bridge--call-process-via-shell
       program infile destination args)
    (apply #'emacs-process-process-file
           program infile destination display args)))

(defun nelisp-emacs-magit-bridge--git-lines (&rest argv)
  "Return git stdout as trimmed lines for ARGV, or nil on non-zero exit."
  (let* ((git (nelisp-emacs-magit-bridge--resolve-git-executable))
         (dir (file-name-as-directory default-directory))
         (args (magit-process-git-arguments argv))
         (command
          (mapconcat #'identity
                     (append
                      (list "cd"
                            (nelisp-emacs-magit-bridge--shell-quote dir)
                            "&&"
                            (nelisp-emacs-magit-bridge--shell-quote git))
                      (mapcar #'nelisp-emacs-magit-bridge--shell-quote args))
                     " "))
         (proc (and (fboundp 'nelisp-process-start)
                    (nelisp-process-start "/bin/sh" "-c" command))))
    (when proc
      (let ((rc (nelisp-process-wait proc))
            (chunks nil)
            (chunk t))
        (while chunk
          (setq chunk (nelisp-process-read-output proc 65536))
          (when chunk
            (push chunk chunks)))
        (when (= rc 0)
          (split-string (apply #'concat (nreverse chunks)) "\n" t))))))

(defun nelisp-emacs-magit-bridge--ensure-sync-process-capture-safe ()
  "Use a capture-preserving sync subprocess path in standalone sessions."
  (when (and (fboundp 'emacs-standalone-mode-p)
             (emacs-standalone-mode-p)
             (fboundp 'nelisp-process-call-process)
             (or (not (fboundp 'emacs-process--standalone-capture-available-p))
                 (not (emacs-process--standalone-capture-available-p))))
    (fset 'call-process
          (symbol-function
           'nelisp-emacs-magit-bridge--call-process-safe))
    (fset 'process-file
          (symbol-function
           'nelisp-emacs-magit-bridge--process-file-safe))))

(defun nelisp-emacs-magit-bridge--git-exit-code (&rest argv)
  "Run git with ARGV and return its exit code."
  (let* ((git (nelisp-emacs-magit-bridge--resolve-git-executable))
         (dir (file-name-as-directory default-directory))
         (args (magit-process-git-arguments argv))
         (command
          (mapconcat #'identity
                     (append
                      (list "cd"
                            (nelisp-emacs-magit-bridge--shell-quote dir)
                            "&&"
                            (nelisp-emacs-magit-bridge--shell-quote git))
                      (mapcar #'nelisp-emacs-magit-bridge--shell-quote args))
                     " "))
         (proc (and (fboundp 'nelisp-process-start)
                    (nelisp-process-start "/bin/sh" "-c" command))))
    (if proc
        (nelisp-process-wait proc)
      127)))

(defun nelisp-emacs-magit-bridge--find-buffer-visiting (filename)
  "Return a live buffer visiting FILENAME, or nil."
  (catch 'found
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (boundp 'buffer-file-name)
                   (stringp buffer-file-name)
                   (equal (expand-file-name buffer-file-name)
                          (expand-file-name filename)))
          (throw 'found buffer))))
    nil))

(defun nelisp-emacs-magit-bridge--link-section-child (section)
  "Ensure SECTION is present in its parent's `children' list."
  (let ((parent (ignore-errors (oref section parent))))
    (when (and parent
               (not (memq section (ignore-errors (oref parent children)))))
      (oset parent children
            (append (ignore-errors (oref parent children))
                    (list section))))))

(defun nelisp-emacs-magit-bridge--insert-file-entry-section (file)
  "Insert one file entry section for FILE using the base `magit-section' class."
  (condition-case err
      (let* ((parent (or (and (boundp 'magit-insert-section--current)
                              magit-insert-section--current)
                         (and (boundp 'magit-root-section)
                              magit-root-section)))
             (magit-insert-section--parent parent)
             (section (magit-insert-section--create 'magit-section file nil))
             (magit-insert-section--current section)
             (magit-insert-section--oldroot
              (or magit-insert-section--oldroot magit-root-section))
             (magit-insert-section--parent section))
        (unless magit-root-section
          (setq magit-root-section section))
        (ignore-errors
          (oset section parent parent))
        (when (fboundp 'nelisp-ec-point)
          (nelisp-emacs-magit-bridge--section-slot-put
           section 'start (nelisp-ec-point)))
        (nelisp-emacs-magit-bridge--trace
         "file-entry create PASS file=%S section=%S" file section)
        (oset section type 'file)
        (nelisp-emacs-magit-bridge--trace
         "file-entry type PASS file=%S type=%S" file (oref section type))
        (insert file)
        (nelisp-emacs-magit-bridge--trace
         "file-entry insert-text PASS file=%S point=%S" file (point))
        (insert 10)
        (nelisp-emacs-magit-bridge--trace
         "file-entry insert-nl PASS file=%S point=%S" file (point))
        (magit-insert-section--finish section)
        (when (fboundp 'nelisp-ec-point)
          (nelisp-emacs-magit-bridge--section-slot-put
           section 'end (nelisp-ec-point)))
        (nelisp-emacs-magit-bridge--link-section-child section)
        (nelisp-emacs-magit-bridge--trace
         "file-entry finish PASS file=%S" file)
        section)
    (error
     (nelisp-emacs-magit-bridge--trace
      "file-entry ERROR file=%S err=%S" file err)
     (nelisp-emacs-magit-bridge--trace-backtrace
      (format "file-entry %S" file))
     (signal (car err) (cdr err)))))

(defun nelisp-emacs-magit-bridge--magit-status-refresh-buffer ()
  "Traceable replacement for `magit-status-refresh-buffer'."
  (nelisp-emacs-magit-bridge--trace "status-refresh update-index BEGIN")
  (magit-git-exit-code "update-index" "--refresh")
  (nelisp-emacs-magit-bridge--trace "status-refresh update-index PASS")
  (nelisp-emacs-magit-bridge--trace "status-refresh sections BEGIN")
  (setq magit-root-section nil)
  (setq magit-insert-section--current nil)
  (setq magit-insert-section--parent nil)
  (setq magit-insert-section--oldroot nil)
  (nelisp-emacs-magit-bridge--trace "status-refresh sections state-reset PASS")
  (let* ((section (magit-insert-section--create 'magit-section nil nil))
         (magit-insert-section--current section)
         (magit-insert-section--oldroot nil)
         (magit-insert-section--parent section))
    (nelisp-emacs-magit-bridge--trace
     "status-refresh root-create PASS section=%S"
     section)
    (setq magit-root-section section)
    (ignore-errors
      (oset section parent nil))
    (nelisp-emacs-magit-bridge--trace "status-refresh root-parent PASS")
    (oset section type 'status)
    (nelisp-emacs-magit-bridge--trace "status-refresh root-type PASS")
    (when (fboundp 'nelisp-ec-point)
      (nelisp-emacs-magit-bridge--section-slot-put
       section 'start (nelisp-ec-point))
      (nelisp-emacs-magit-bridge--section-slot-put
       section 'content (nelisp-ec-point)))
    (nelisp-emacs-magit-bridge--trace "status-refresh root-slots PASS")
    (nelisp-emacs-magit-bridge--trace "status-refresh section-hook BEGIN")
    (nelisp-emacs-magit-bridge--run-section-hook
     'magit-status-sections-hook)
    (nelisp-emacs-magit-bridge--trace "status-refresh section-hook PASS")
    (magit-insert-section--finish section)
    (nelisp-emacs-magit-bridge--trace "status-refresh root-finish PASS")
    (when (fboundp 'nelisp-ec-point)
      (nelisp-emacs-magit-bridge--section-slot-put
       section 'end (nelisp-ec-point))))
  (nelisp-emacs-magit-bridge--trace "status-refresh sections PASS"))

(defun nelisp-emacs-magit-bridge--magit-insert-status-headers ()
  "Traceable replacement for `magit-insert-status-headers'."
  (nelisp-emacs-magit-bridge--trace "status-headers rev-verify BEGIN")
  (if (= (magit-git-exit-code "rev-parse" "--verify" "HEAD") 0)
      (progn
        (nelisp-emacs-magit-bridge--trace "status-headers rev-verify PASS")
        (nelisp-emacs-magit-bridge--trace "status-headers insert-headers BEGIN")
        (magit-insert-headers 'magit-status-headers-hook)
        (nelisp-emacs-magit-bridge--trace "status-headers insert-headers PASS"))
    (nelisp-emacs-magit-bridge--trace "status-headers unborn PASS")
    (insert "In the beginning there was darkness\n\n")))

(defun nelisp-emacs-magit-bridge--magit-insert-diff-filter-header ()
  "NeLisp-safe replacement for `magit-insert-diff-filter-header'."
  (let ((ignore-modules nil))
    (when magit-buffer-diff-files
      (nelisp-emacs-magit-bridge--trace
       "diff-filter-header ignore-submodules BEGIN")
      (setq ignore-modules (magit-ignore-submodules-p))
      (nelisp-emacs-magit-bridge--trace
       "diff-filter-header ignore-submodules PASS %S"
       ignore-modules))
    (when (or ignore-modules magit-buffer-diff-files)
      (insert (propertize (format "%-10s" "Filter! ")
                          'font-lock-face
                          'magit-section-heading))
      (when ignore-modules
        (insert ignore-modules)
        (when magit-buffer-diff-files
          (insert " -- ")))
      (when magit-buffer-diff-files
        (insert (string-join magit-buffer-diff-files " ")))
      (insert 10))))

(defun nelisp-emacs-magit-bridge--magit-insert-head-branch-header
    (&optional branch)
  "NeLisp-safe replacement for `magit-insert-head-branch-header'."
  (nelisp-emacs-magit-bridge--trace "head-branch-header rev-format BEGIN")
  (let* ((rev (or branch "HEAD"))
         (output (or (magit-rev-format "%h %s" rev)
                     (magit-git-string "show" "-s" "--format=%h %s" rev))))
    (if (not (stringp output))
        (nelisp-emacs-magit-bridge--trace
         "head-branch-header rev-format SKIP %S" output)
      (nelisp-emacs-magit-bridge--trace
       "head-branch-header rev-format PASS %S" output)
      (string-match "^\\([^ ]+\\) \\(.*\\)" output)
      (let ((commit (or (match-string-no-properties 1 output) "HEAD"))
            (summary (or (match-string-no-properties 2 output)
                         "(no commit message)"))
            (branch-name (or branch (magit-get-current-branch))))
        (insert (format "%-10s" "Head: "))
        (when magit-status-show-hashes-in-headers
          (insert (propertize commit 'font-lock-face 'magit-hash) 32))
        (if branch-name
            (insert (propertize branch-name
                                'font-lock-face
                                'magit-branch-local)
                    32)
          (insert (propertize commit 'font-lock-face 'magit-hash) 32))
        (insert summary)
        (insert 10)))))

(defun nelisp-emacs-magit-bridge--magit-insert-unstaged-changes ()
  "NeLisp-safe replacement for `magit-insert-unstaged-changes'."
  (nelisp-emacs-magit-bridge--trace
   "unstaged BEGIN args=%S files=%S"
   magit-buffer-diff-args magit-buffer-diff-files)
  (let ((files (apply #'nelisp-emacs-magit-bridge--git-lines
                      (append '("diff" "--name-only" "--")
                              magit-buffer-diff-files))))
    (nelisp-emacs-magit-bridge--trace "unstaged file-list PASS files=%S" files)
    (when files
      (let* ((parent (or (and (boundp 'magit-insert-section--current)
                              magit-insert-section--current)
                         (and (boundp 'magit-root-section)
                              magit-root-section)))
             (magit-insert-section--parent parent)
             (section (magit-insert-section--create 'magit-section nil nil))
             (magit-insert-section--current section)
             (magit-insert-section--oldroot
              (or magit-insert-section--oldroot magit-root-section))
             (magit-insert-section--parent section))
        (unless magit-root-section
          (setq magit-root-section section))
        (ignore-errors
          (oset section parent parent))
        (oset section type 'unstaged)
        (when (fboundp 'nelisp-ec-point)
          (nelisp-emacs-magit-bridge--section-slot-put
           section 'start (nelisp-ec-point)))
        (nelisp-emacs-magit-bridge--trace "unstaged section PASS")
        (insert (propertize "Unstaged changes\n"
                            'font-lock-face
                            'magit-section-heading))
        (when (fboundp 'nelisp-ec-point)
          (nelisp-emacs-magit-bridge--section-slot-put
           section 'content (nelisp-ec-point)))
        (nelisp-emacs-magit-bridge--trace "unstaged heading PASS")
        (dolist (file files)
          (nelisp-emacs-magit-bridge--insert-file-entry-section file))
        (magit-insert-section--finish section)
        (when (fboundp 'nelisp-ec-point)
          (nelisp-emacs-magit-bridge--section-slot-put
           section 'end (nelisp-ec-point)))
        (nelisp-emacs-magit-bridge--link-section-child section))))
  (nelisp-emacs-magit-bridge--trace "unstaged PASS"))

(defun nelisp-emacs-magit-bridge--magit-insert-staged-changes ()
  "NeLisp-safe replacement for `magit-insert-staged-changes'."
  (nelisp-emacs-magit-bridge--trace
   "staged BEGIN args=%S files=%S"
   magit-buffer-diff-args magit-buffer-diff-files)
  (let ((files (apply #'nelisp-emacs-magit-bridge--git-lines
                      (append '("diff" "--cached" "--name-only" "--")
                              magit-buffer-diff-files))))
    (nelisp-emacs-magit-bridge--trace "staged file-list PASS files=%S" files)
    (when files
      (let* ((parent (or (and (boundp 'magit-insert-section--current)
                              magit-insert-section--current)
                         (and (boundp 'magit-root-section)
                              magit-root-section)))
             (magit-insert-section--parent parent)
             (section (magit-insert-section--create 'magit-section nil nil))
             (magit-insert-section--current section)
             (magit-insert-section--oldroot
              (or magit-insert-section--oldroot magit-root-section))
             (magit-insert-section--parent section))
        (unless magit-root-section
          (setq magit-root-section section))
        (ignore-errors
          (oset section parent parent))
        (oset section type 'staged)
        (when (fboundp 'nelisp-ec-point)
          (nelisp-emacs-magit-bridge--section-slot-put
           section 'start (nelisp-ec-point)))
        (insert (propertize "Staged changes\n"
                            'font-lock-face
                            'magit-section-heading))
        (when (fboundp 'nelisp-ec-point)
          (nelisp-emacs-magit-bridge--section-slot-put
           section 'content (nelisp-ec-point)))
        (nelisp-emacs-magit-bridge--trace "staged heading PASS")
        (dolist (file files)
          (nelisp-emacs-magit-bridge--insert-file-entry-section file))
        (magit-insert-section--finish section)
        (when (fboundp 'nelisp-ec-point)
          (nelisp-emacs-magit-bridge--section-slot-put
           section 'end (nelisp-ec-point)))
        (nelisp-emacs-magit-bridge--link-section-child section))))
  (nelisp-emacs-magit-bridge--trace "staged PASS"))

(defun nelisp-emacs-magit-bridge--magit-insert-recent-commits-unpushed ()
  "Insert the fallback \"Recent commits\" status section."
  (let* ((start (format "HEAD~%s" magit-log-section-commit-count))
         (range (and (magit-rev-verify start)
                     (concat start "..HEAD"))))
    (magit-insert-section (unpushed (or range "@{upstream}..") t)
      (magit-insert-heading "Recent commits")
      (magit--insert-log nil
        (and (member "--graph" magit-buffer-log-args) range)
        (cons (format "-n%d" magit-log-section-commit-count)
              (seq-remove
               (lambda (arg)
                 (string-prefix-p "-n" arg))
               magit-buffer-log-args))))))

(defun nelisp-emacs-magit-bridge--magit-insert-unpushed-to-upstream ()
  "NeLisp-safe replacement for `magit-insert-unpushed-to-upstream'."
  (let ((upstream (ignore-errors (magit-get-upstream-branch))))
    (nelisp-emacs-magit-bridge--trace
     "unpushed-upstream BEGIN upstream=%S" upstream)
    (when (and upstream
               (ignore-errors
                 (magit-git-success "rev-parse" "@{upstream}")))
      (magit-insert-section (unpushed "@{upstream}..")
        (magit-insert-heading
          (format (propertize "Unmerged into %s."
                              'font-lock-face 'magit-section-heading)
                  upstream))
        (magit--insert-log nil "@{upstream}.." magit-buffer-log-args)
        (magit-log-insert-child-count)))
    (nelisp-emacs-magit-bridge--trace "unpushed-upstream PASS")))

(defun nelisp-emacs-magit-bridge--magit-insert-unpushed-to-upstream-or-recent ()
  "NeLisp-safe replacement for `magit-insert-unpushed-to-upstream-or-recent'."
  (let ((upstream (ignore-errors (magit-get-upstream-branch))))
    (nelisp-emacs-magit-bridge--trace
     "unpushed-upstream-or-recent BEGIN upstream=%S" upstream)
    (cond
     ((not upstream)
      (nelisp-emacs-magit-bridge--trace
       "unpushed-upstream-or-recent SKIP no-upstream"))
     ((ignore-errors (magit-rev-ancestor-p "HEAD" upstream))
      (nelisp-emacs-magit-bridge--trace
       "unpushed-upstream-or-recent SKIP ancestor"))
     (t
      (nelisp-emacs-magit-bridge--magit-insert-unpushed-to-upstream)))
    (nelisp-emacs-magit-bridge--trace
     "unpushed-upstream-or-recent PASS")))

(defun nelisp-emacs-magit-bridge--magit-insert-unpulled-from-upstream ()
  "NeLisp-safe replacement for `magit-insert-unpulled-from-upstream'."
  (let ((upstream (ignore-errors (magit-get-upstream-branch))))
    (nelisp-emacs-magit-bridge--trace
     "unpulled-upstream BEGIN upstream=%S" upstream)
    (when upstream
      (magit-insert-section (unpulled "..@{upstream}" t)
        (magit-insert-heading
          (format (propertize "Unpulled from %s."
                              'font-lock-face 'magit-section-heading)
                  upstream))
        (magit--insert-log nil "..@{upstream}" magit-buffer-log-args)
        (magit-log-insert-child-count)))
    (nelisp-emacs-magit-bridge--trace "unpulled-upstream PASS")))

(defun nelisp-emacs-magit-bridge--magit-insert-unpushed-to-pushremote ()
  "NeLisp-safe replacement for `magit-insert-unpushed-to-pushremote'."
  (let ((target (ignore-errors (magit-get-push-branch))))
    (nelisp-emacs-magit-bridge--trace
     "unpushed-pushremote BEGIN target=%S" target)
    (when target
      (let ((range (concat target "..")))
        (magit-insert-section (unpushed range t)
          (magit-insert-heading
            (format (propertize "Unpushed to %s."
                                'font-lock-face 'magit-section-heading)
                    (propertize target
                                'font-lock-face
                                'magit-branch-remote)))
          (magit--insert-log nil range magit-buffer-log-args)
          (magit-log-insert-child-count))))
    (nelisp-emacs-magit-bridge--trace "unpushed-pushremote PASS")))

(defun nelisp-emacs-magit-bridge--magit-insert-unpulled-from-pushremote ()
  "NeLisp-safe replacement for `magit-insert-unpulled-from-pushremote'."
  (let ((target (ignore-errors (magit-get-push-branch))))
    (nelisp-emacs-magit-bridge--trace
     "unpulled-pushremote BEGIN target=%S" target)
    (when target
      (let ((range (concat ".." target)))
        (magit-insert-section (unpulled range t)
          (magit-insert-heading
            (format (propertize "Unpulled from %s."
                                'font-lock-face 'magit-section-heading)
                    (propertize target
                                'font-lock-face
                                'magit-branch-remote)))
          (magit--insert-log nil range magit-buffer-log-args)
          (magit-log-insert-child-count))))
    (nelisp-emacs-magit-bridge--trace "unpulled-pushremote PASS")))

(defun nelisp-emacs-magit-bridge--magit-status-setup-buffer
    (&optional directory)
  "Directory-stable replacement for `magit-status-setup-buffer'."
  (let ((default-directory (or directory default-directory))
        (magit-status-use-buffer-arguments 'never))
    (nelisp-emacs-magit-bridge--trace
     "status-setup values BEGIN dir=%S" default-directory)
    (let* ((dvalue (magit-diff--get-value 'magit-status-mode 'status))
           (lvalue (magit-log--get-value 'magit-status-mode 'status))
           (dargs (car dvalue))
           (dfiles (cadr dvalue))
           (largs (car lvalue))
           (lfiles (cadr lvalue)))
      (nelisp-emacs-magit-bridge--trace
       "status-setup values PASS dargs=%S dfiles=%S largs=%S lfiles=%S"
       dargs dfiles largs lfiles)
      (let ((position
             (nelisp-emacs-magit-bridge--magit-status-get-file-position)))
        (nelisp-emacs-magit-bridge--trace
         "status-setup position PASS position=%S" position)
      (magit-setup-buffer-internal
       #'magit-status-mode
       nil
       (list (list 'magit-buffer-diff-args dargs)
             (list 'magit-buffer-diff-files dfiles)
             (list 'magit-buffer-log-args largs)
             (list 'magit-buffer-log-files lfiles))
       :initial-section #'magit-status-goto-initial-section
       :select-section (and position
                            (lambda ()
                              (apply #'magit-status--goto-file-position
                                     position))))))))

(defun nelisp-emacs-magit-bridge--magit-status-get-file-position ()
  "NeLisp-safe replacement for `magit-status--get-file-position'."
  (when magit-status-goto-file-position
    (let ((file (magit-file-relative-name)))
      (when file
        (save-excursion
          (widen)
          (list file (line-number-at-pos) (current-column)))))))

(defun nelisp-emacs-magit-bridge--section-match-p (section spec)
  "Return non-nil when SECTION matches initial-section SPEC."
  (let ((type (ignore-errors (oref section type))))
    (cond
     ((symbolp spec)
      (eq type spec))
     ((listp spec)
      (equal type (car spec)))
     (t nil))))

(defun nelisp-emacs-magit-bridge--find-section-recursive (section spec)
  "Return the first descendant of SECTION matching SPEC."
  (catch 'found
    (when (nelisp-emacs-magit-bridge--section-match-p section spec)
      (throw 'found section))
    (dolist (child (ignore-errors (oref section children)))
      (let ((match (nelisp-emacs-magit-bridge--find-section-recursive
                    child spec)))
        (when match
          (throw 'found match))))
    nil))

(defun nelisp-emacs-magit-bridge--find-initial-status-section ()
  "Return the section selected by `magit-status-initial-section'."
  (let* ((root (and (boundp 'magit-root-section) magit-root-section))
         (children (and root (ignore-errors (oref root children)))))
    (catch 'found
      (dolist (initial magit-status-initial-section)
        (let ((section
               (cond
                ((integerp initial)
                 (nth (1- initial) children))
                (root
                 (nelisp-emacs-magit-bridge--find-section-recursive
                  root initial))
                (t nil))))
          (when section
            (throw 'found section))))
      nil)))

(defun nelisp-emacs-magit-bridge--reveal-initial-status-section (section)
  "Apply initial visibility override for SECTION."
  (let ((vis (cdr (assq 'magit-status-initial-section
                        magit-section-initial-visibility-alist))))
    (when vis
      (if (eq vis 'hide)
          (magit-section-hide section)
        (magit-section-show section)))))

(defun nelisp-emacs-magit-bridge--goto-section-start (section)
  "Move point to SECTION start, falling back to `point-min'."
  (let ((start (ignore-errors (oref section start))))
    (goto-char (or (nelisp-emacs-magit-bridge--section-pos start)
                   (point-min)))))

(defun nelisp-emacs-magit-bridge--first-section-at-point ()
  "Return a usable starting section at point."
  (or (nelisp-emacs-magit-bridge--find-initial-status-section)
      (car (and (boundp 'magit-root-section)
                magit-root-section
                (ignore-errors (oref magit-root-section children))))))

(defun nelisp-emacs-magit-bridge--magit-status-goto-initial-section ()
  "NeLisp-safe replacement for `magit-status-goto-initial-section'."
  (let ((section (nelisp-emacs-magit-bridge--first-section-at-point)))
    (when section
      (nelisp-emacs-magit-bridge--goto-section-start section)
      (nelisp-emacs-magit-bridge--reveal-initial-status-section section))))

(defun nelisp-emacs-magit-bridge--magit-section-show (section)
  "NeLisp-safe replacement for `magit-section-show'."
  (oset section hidden nil)
  (magit-section--opportunistic-wash section)
  (magit-section--opportunistic-paint section)
  (let ((content (ignore-errors (oref section content)))
        (end (ignore-errors (oref section end))))
    (when (and content end)
      (remove-overlays content end 'invisible t)))
  (magit-section-maybe-update-visibility-indicator section)
  (magit-section-maybe-cache-visibility section)
  (dolist (child (ignore-errors (oref section children)))
    (if (oref child hidden)
        (magit-section-hide child)
      (magit-section-show child))))

(defun nelisp-emacs-magit-bridge--magit-section-hide (section)
  "NeLisp-safe replacement for `magit-section-hide'."
  (if (eq section magit-root-section)
      (user-error "Cannot hide root section")
    (oset section hidden t)
    (let ((beg (ignore-errors (oref section content))))
      (when beg
        (let ((end (ignore-errors (oref section end))))
          (when (and end (< beg (point)) (< (point) end))
            (goto-char (oref section start)))
          (when end
            (remove-overlays beg end 'invisible t)
            (let ((overlay (make-overlay beg end)))
              (overlay-put overlay 'evaporate t)
              (overlay-put overlay 'invisible t)
              (overlay-put overlay 'cursor-intangible t))))))
    (magit-section-maybe-update-visibility-indicator section)
    (magit-section-maybe-cache-visibility section)))

(defun nelisp-emacs-magit-bridge--magit-section-set-section-properties (section)
  "NeLisp-safe replacement for `magit-section--set-section-properties'."
  (let* ((start (nelisp-emacs-magit-bridge--section-slot-pos section 'start))
         (end (nelisp-emacs-magit-bridge--section-slot-pos section 'end))
         (map (ignore-errors (oref section keymap))))
    (when (and start end (< start end))
      (nelisp-emacs-magit-bridge--repair-section-properties section)
      (when (symbolp map)
        (setq map (ignore-errors (symbol-value map))))
      (when map
        (add-text-properties start end (list 'keymap map))))))

(defun nelisp-emacs-magit-bridge--magit-insert-section-finish (obj)
  "NeLisp-safe replacement for `magit-insert-section--finish'."
  (nelisp-emacs-magit-bridge--trace
   "section-finish BEGIN obj=%S type=%S"
   obj (ignore-errors (oref obj type)))
  (run-hooks 'magit-insert-section-hook)
  (nelisp-emacs-magit-bridge--trace "section-finish hook PASS")
  (if magit-section-inhibit-markers
      (oset obj end (point))
    (oset obj end (point-marker))
    (ignore-errors
      (set-marker-insertion-type (oref obj start) t)))
  (nelisp-emacs-magit-bridge--trace
   "section-finish end PASS end=%S"
   (ignore-errors (oref obj end)))
  (if (eq obj magit-root-section)
      (when (eq magit-section-inhibit-markers 'delay)
        (setq magit-section-inhibit-markers nil))
    (magit-section--set-section-properties obj)
    (nelisp-emacs-magit-bridge--trace "section-finish props PASS")
    (ignore-errors
      (magit-section-maybe-add-heading-map obj))
    (nelisp-emacs-magit-bridge--trace "section-finish heading-map PASS")
    (let ((parent (ignore-errors (oref obj parent))))
      (nelisp-emacs-magit-bridge--trace
       "section-finish parent BEGIN parent=%S" parent)
      (when parent
        (if magit-section-insert-in-reverse
            (oset parent children
                  (cons obj (ignore-errors (oref parent children))))
          (oset parent children
                (append (ignore-errors (oref parent children))
                        (list obj)))))
      (nelisp-emacs-magit-bridge--trace "section-finish parent PASS"))
    (when (and (ignore-errors (oref obj children))
               magit-section-show-child-count)
      (ignore-errors
        (magit-insert-child-count obj))
      (nelisp-emacs-magit-bridge--trace "section-finish child-count PASS")))
  (when magit-section-insert-in-reverse
    (oset obj children
          (nreverse (ignore-errors (oref obj children))))
    (nelisp-emacs-magit-bridge--trace "section-finish reverse PASS"))
  (nelisp-emacs-magit-bridge--trace "section-finish PASS"))

(defun nelisp-emacs-magit-bridge--magit-section-content-p (section)
  "NeLisp-safe replacement for `magit-section-content-p'."
  (let ((content (ignore-errors (oref section content)))
        (end (ignore-errors (oref section end)))
        (washer (ignore-errors (oref section washer))))
    (and content
         (or (not (equal content end))
             washer))))

(defun nelisp-emacs-magit-bridge--magit-section-at (&optional position)
  "NeLisp-safe replacement for `magit-section-at'."
  (let ((pos (or position
                 (and (fboundp 'nelisp-ec-point)
                      (ignore-errors (nelisp-ec-point)))
                 (point))))
    (if (fboundp 'emacs-buffer-get-text-property)
        (ignore-errors
          (emacs-buffer-get-text-property pos 'magit-section (current-buffer)))
      (ignore-errors
        (get-text-property pos 'magit-section)))))

(defun nelisp-emacs-magit-bridge--put-current-buffer-text-property
    (start end prop value)
  "Set PROP on [START, END) in the current buffer through the shared substrate.

The standalone `put-text-property' wrapper still occasionally resolves the
implicit current-buffer path to an `emacs-buffer' error under the Magit live
bridge, even though the lower-level buffer substrate is already loaded and the
current buffer is valid.  Call the underlying `emacs-buffer' owner directly
with an explicit buffer object when available, and fall back to the unprefixed
builtin otherwise."
  (when (< start end)
    (if (fboundp 'emacs-buffer-put-text-property)
        (emacs-buffer-put-text-property start end prop value (current-buffer))
      (put-text-property start end prop value))))

(defun nelisp-emacs-magit-bridge--section-containing-position (section position)
  "Return the deepest descendant of SECTION containing POSITION."
  (let ((start (nelisp-emacs-magit-bridge--section-slot-pos section 'start))
        (end (nelisp-emacs-magit-bridge--section-slot-pos section 'end))
        found)
    (when (or (and start end (<= start position) (<= position end))
              (eq section magit-root-section))
      (dolist (child (ignore-errors (oref section children)))
        (unless found
          (setq found
                (nelisp-emacs-magit-bridge--section-containing-position
                 child position))))
      (or found section))))

(defun nelisp-emacs-magit-bridge--magit-current-section ()
  "NeLisp-safe replacement for `magit-current-section'."
  (or magit--context-menu-section
      (let ((position (or (and (fboundp 'nelisp-ec-point)
                               (ignore-errors (nelisp-ec-point)))
                          (ignore-errors (point)))))
        (or (and position
                 (boundp 'magit-root-section)
                 magit-root-section
                 (nelisp-emacs-magit-bridge--section-containing-position
                  magit-root-section position))
            (nelisp-emacs-magit-bridge--magit-section-at position)))
      magit-root-section))

(defun nelisp-emacs-magit-bridge--magit-diff-type (&optional section)
  "NeLisp-safe replacement for `magit-diff-type'."
  (let ((section (or section (magit-current-section))))
    (when section
      (cond
       ((derived-mode-p 'magit-revision-mode 'magit-stash-mode) 'committed)
       ((derived-mode-p 'magit-diff-mode)
        (cond
         (magit-buffer-diff-type)
         ((equal magit-buffer-diff-typearg "--no-index") 'undefined)
         ((not magit-buffer-diff-range) 'undefined)
         ((string-search "." magit-buffer-diff-range) 'committed)
         ((magit-rev-head-p magit-buffer-diff-range)
          (if (equal magit-buffer-diff-typearg "--cached")
              'staged
            'unstaged))
         (t 'committed)))
       ((derived-mode-p 'magit-status-mode)
        (let ((type (oref section type)))
          (cond
           ((memq type '(staged unstaged tracked untracked))
            type)
           ((memq type '(file module))
            (let* ((parent (ignore-errors (oref section parent)))
                   (parent-type (and parent (ignore-errors (oref parent type)))))
              (if (memq parent-type '(file module))
                  (nelisp-emacs-magit-bridge--magit-diff-type parent)
                parent-type)))
           ((eq type 'hunk)
            (let* ((parent (ignore-errors (oref section parent)))
                   (grand (and parent (ignore-errors (oref parent parent)))))
              (and grand (ignore-errors (oref grand type)))))
           (t nil))))
       ((derived-mode-p 'magit-log-mode)
        (if (or (and (magit-section-match 'commit section)
                     (oref section children))
                (magit-section-match [* file commit] section))
            'committed
          'undefined))
       (t 'undefined)))))

(defun nelisp-emacs-magit-bridge--magit-diff-scope (&optional section strict)
  "NeLisp-safe replacement for `magit-diff-scope'."
  (let* ((explicit-section-p (not (null section)))
         (siblings (and (not explicit-section-p)
                        (ignore-errors (magit-region-sections nil t)))))
    (setq section (or section (car siblings) (magit-current-section)))
    (when (and section
               (or (not strict)
                   (and (not (eq (magit-diff-type section) 'untracked))
                        (not (eq (let ((parent (ignore-errors (oref section parent))))
                                   (and parent (ignore-errors (oref parent type))))
                                 'diffstat)))))
      (let ((type (ignore-errors (oref section type))))
        (cond
         ((and (eq type 'hunk)
               (not siblings)
               (magit-diff-use-hunk-region-p))
          (if (magit-section-internal-region-p section) 'region 'hunk))
         ((eq type 'hunk)
          (if (and siblings (not explicit-section-p)) 'hunks 'hunk))
         ((memq type '(file module))
          (if (and siblings (not explicit-section-p)) 'files 'file))
         ((memq type '(staged unstaged untracked))
          'list)
         (t nil))))))

(defun nelisp-emacs-magit-bridge--magit-refresh-buffer
    (&optional created &rest keys)
  "Traceable replacement for `magit-refresh-buffer'."
  (let ((initial-section (plist-get keys :initial-section))
        (select-section (plist-get keys :select-section))
        (refresh (magit--refresh-buffer-function))
        (refresh-symbol
         (intern (format "%s-refresh-buffer"
                         (substring (symbol-name major-mode) 0 -5)))))
    (when refresh
      (let ((magit--refreshing-buffer-p t)
            (magit--refresh-start-time (current-time))
            (magit--refresh-cache (or magit--refresh-cache (list (cons 0 0)))))
        (nelisp-emacs-magit-bridge--trace
         "refresh-buffer refresh-function BEGIN created=%S fn=%S symbol=%S bound=%S next=%S"
         created
         refresh
         refresh-symbol
         (and (fboundp refresh-symbol)
              (symbol-function refresh-symbol))
         (fboundp 'cl-call-next-method))
        (if created
            (progn
              (condition-case err
                  (funcall refresh)
                (error
                 (nelisp-emacs-magit-bridge--trace
                  "refresh-buffer refresh-function ERROR %S" err)
                 (nelisp-emacs-magit-bridge--trace-backtrace
                  "refresh-buffer refresh-function BT")
                 (signal (car err) (cdr err))))
              (nelisp-emacs-magit-bridge--trace
               "refresh-buffer created post-refresh BEGIN size=%S root=%S"
               (ignore-errors (buffer-size))
               (and (boundp 'magit-root-section) magit-root-section))
              (when (and (> (buffer-size) 0)
                         (boundp 'magit-root-section)
                         magit-root-section)
                (nelisp-emacs-magit-bridge--trace
                 "refresh-buffer created put-text-property BEGIN")
                (nelisp-emacs-magit-bridge--put-current-buffer-text-property
                 (point-min) (min (point-max) (1+ (point-min)))
                 'magit-section
                 magit-root-section))
              (nelisp-emacs-magit-bridge--trace
               "refresh-buffer created post-properties PASS")
              (nelisp-emacs-magit-bridge--trace
               "refresh-buffer refresh-function PASS")
              (cond (initial-section
                     (nelisp-emacs-magit-bridge--trace
                      "refresh-buffer initial-section BEGIN")
                     (condition-case err
                         (funcall initial-section)
                       (error
                        (nelisp-emacs-magit-bridge--trace
                         "refresh-buffer initial-section ERROR %S" err)
                        (nelisp-emacs-magit-bridge--trace-backtrace
                         "refresh-buffer initial-section BT")
                        (signal (car err) (cdr err))))
                     (nelisp-emacs-magit-bridge--trace
                      "refresh-buffer initial-section PASS"))
                    (select-section
                     (nelisp-emacs-magit-bridge--trace
                      "refresh-buffer select-section BEGIN")
                     (condition-case err
                         (funcall select-section)
                       (error
                        (nelisp-emacs-magit-bridge--trace
                         "refresh-buffer select-section ERROR %S" err)
                        (nelisp-emacs-magit-bridge--trace-backtrace
                         "refresh-buffer select-section BT")
                        (signal (car err) (cdr err))))
                     (nelisp-emacs-magit-bridge--trace
                      "refresh-buffer select-section PASS"))))
          (deactivate-mark)
          (setq magit-section-pre-command-section nil)
          (setq magit-section-highlight-overlays nil)
          (setq magit-section-selection-overlays nil)
          (setq magit-section-highlighted-sections nil)
          (setq magit-section-focused-sections nil)
          (let ((positions
                 (condition-case err
                     (magit--refresh-buffer-get-positions)
                   (error
                    (nelisp-emacs-magit-bridge--trace
                     "refresh-buffer positions SKIP %S" err)
                    nil))))
            (condition-case err
                (funcall refresh)
              (error
               (nelisp-emacs-magit-bridge--trace
                "refresh-buffer refresh-function ERROR %S" err)
               (nelisp-emacs-magit-bridge--trace-backtrace
                "refresh-buffer refresh-function BT")
               (signal (car err) (cdr err))))
            (nelisp-emacs-magit-bridge--trace
             "refresh-buffer refresh-function PASS")
            (cond (select-section
                   (nelisp-emacs-magit-bridge--trace
                    "refresh-buffer select-section BEGIN")
                   (condition-case err
                       (funcall select-section)
                     (error
                      (nelisp-emacs-magit-bridge--trace
                       "refresh-buffer select-section ERROR %S" err)
                      (nelisp-emacs-magit-bridge--trace-backtrace
                       "refresh-buffer select-section BT")
                      (signal (car err) (cdr err))))
                   (nelisp-emacs-magit-bridge--trace
                    "refresh-buffer select-section PASS"))
                  ((magit--refresh-buffer-set-positions positions)))))
        (nelisp-emacs-magit-bridge--trace "refresh-buffer section-show BEGIN")
        (condition-case err
            (let ((magit-section-cache-visibility nil))
              (magit-section-show magit-root-section)
              (nelisp-emacs-magit-bridge--trace
               "refresh-buffer section-show PASS"))
          (error
           (nelisp-emacs-magit-bridge--trace
            "refresh-buffer section-show SKIP %S" err)))
        (when (and (boundp 'magit-root-section)
                   magit-root-section)
          (condition-case err
              (nelisp-emacs-magit-bridge--repair-section-properties
               magit-root-section)
            (error
             (nelisp-emacs-magit-bridge--trace
              "refresh-buffer repair-properties SKIP %S" err))))
        (nelisp-emacs-magit-bridge--trace "refresh-buffer hook BEGIN")
        (run-hooks 'magit-refresh-buffer-hook)
        (nelisp-emacs-magit-bridge--trace "refresh-buffer hook PASS")
        (nelisp-emacs-magit-bridge--trace "refresh-buffer highlight BEGIN")
        (condition-case err
            (progn
              (magit-section-update-highlight)
              (nelisp-emacs-magit-bridge--trace
               "refresh-buffer highlight PASS"))
          (error
           (nelisp-emacs-magit-bridge--trace
            "refresh-buffer highlight SKIP %S" err)))
        (when (and (> (buffer-size) 0)
                   (boundp 'magit-root-section)
                   magit-root-section)
          (nelisp-emacs-magit-bridge--put-current-buffer-text-property
           (point-min) (min (point-max) (1+ (point-min)))
           'magit-section
           magit-root-section)
          (goto-char (point-min))
          (nelisp-emacs-magit-bridge--trace
           "refresh-buffer point-anchor PASS %S" magit-root-section))
        (set-buffer-modified-p nil)
        (push (current-buffer) magit-section--refreshed-buffers)))))

(defun nelisp-emacs-magit-bridge--magit-section-forward-fallback (section)
  "Move to the next visible section after SECTION using the section tree."
  (let ((cursor section)
        next)
    (if (oref cursor parent)
        (progn
          (setq next (and (not (oref cursor hidden))
                          (car (oref cursor children))))
          (while (and cursor (not next))
            (unless (setq next (car (magit-section-siblings cursor 'next)))
              (setq cursor (oref cursor parent)))))
      (setq next (car (oref cursor children))))
    (when next
      (goto-char (oref next start))
      (run-hook-with-args 'magit-section-movement-hook next)
      next)))

(defun nelisp-emacs-magit-bridge--magit-section-forward ()
  "NeLisp-safe replacement for `magit-section-forward'."
  (interactive)
  (let ((section (magit-current-section)))
    (when nelisp-emacs-magit-bridge--magit-section-forward-orig
      (funcall nelisp-emacs-magit-bridge--magit-section-forward-orig))
    (when (eq section (magit-current-section))
      (or (nelisp-emacs-magit-bridge--magit-section-forward-fallback section)
          (user-error "No next section")))))

(defun nelisp-emacs-magit-bridge--ensure-magit-refresh-traceable ()
  "Replace Magit refresh functions with traceable equivalents."
  (fset 'magit-git-lines
        (symbol-function
         'nelisp-emacs-magit-bridge--git-lines))
  (fset 'magit-get-mode-buffer
        (symbol-function
         'nelisp-emacs-magit-bridge--magit-get-mode-buffer))
  (fset 'magit-refresh-buffer
        (symbol-function
         'nelisp-emacs-magit-bridge--magit-refresh-buffer))
  (fset 'magit-status-refresh-buffer
        (symbol-function
         'nelisp-emacs-magit-bridge--magit-status-refresh-buffer))
  (fset 'magit-status-setup-buffer
        (symbol-function
         'nelisp-emacs-magit-bridge--magit-status-setup-buffer))
  (unless nelisp-emacs-magit-bridge--magit-stage-orig
    (setq nelisp-emacs-magit-bridge--magit-stage-orig
          (symbol-function 'magit-stage)))
  (fset 'magit-stage
        (symbol-function
         'nelisp-emacs-magit-bridge--magit-stage))
  (fset 'magit-stage-1
        (symbol-function
         'nelisp-emacs-magit-bridge--magit-stage-1))
  (fset 'magit-insert-status-headers
        (symbol-function
         'nelisp-emacs-magit-bridge--magit-insert-status-headers))
  (fset 'magit-insert-diff-filter-header
        (symbol-function
         'nelisp-emacs-magit-bridge--magit-insert-diff-filter-header))
  (fset 'magit-insert-head-branch-header
        (symbol-function
         'nelisp-emacs-magit-bridge--magit-insert-head-branch-header))
  (fset 'magit-insert-unstaged-changes
        (symbol-function
         'nelisp-emacs-magit-bridge--magit-insert-unstaged-changes))
  (fset 'magit-insert-staged-changes
        (symbol-function
         'nelisp-emacs-magit-bridge--magit-insert-staged-changes))
  (fset 'magit-insert-unpushed-to-upstream
        (symbol-function
         'nelisp-emacs-magit-bridge--magit-insert-unpushed-to-upstream))
  (fset 'magit-insert-unpushed-to-upstream-or-recent
        (symbol-function
         'nelisp-emacs-magit-bridge--magit-insert-unpushed-to-upstream-or-recent))
  (fset 'magit-insert-unpulled-from-upstream
        (symbol-function
         'nelisp-emacs-magit-bridge--magit-insert-unpulled-from-upstream))
  (fset 'magit-insert-unpushed-to-pushremote
        (symbol-function
         'nelisp-emacs-magit-bridge--magit-insert-unpushed-to-pushremote))
  (fset 'magit-insert-unpulled-from-pushremote
        (symbol-function
         'nelisp-emacs-magit-bridge--magit-insert-unpulled-from-pushremote))
  (unless nelisp-emacs-magit-bridge--magit-section-forward-orig
    (setq nelisp-emacs-magit-bridge--magit-section-forward-orig
          (symbol-function 'magit-section-forward)))
  (fset 'magit-section-forward
        (symbol-function
         'nelisp-emacs-magit-bridge--magit-section-forward)))

(defun nelisp-emacs-magit-bridge--ensure-find-buffer-visiting ()
  "Install a standalone-compatible `find-buffer-visiting'."
  (fset 'find-buffer-visiting
        (symbol-function
         'nelisp-emacs-magit-bridge--find-buffer-visiting)))

(defun nelisp-emacs-magit-bridge--magit-setup-buffer-internal
    (mode locked bindings &rest keys)
  "Directory-stable replacement for `magit-setup-buffer-internal'."
  (let* ((buffer-key (plist-get keys :buffer))
         (directory-key (plist-get keys :directory))
         (initial-section (plist-get keys :initial-section))
         (select-section (plist-get keys :select-section))
         (dir (file-name-as-directory
               (or directory-key default-directory)))
         (default-directory dir)
         (value nil)
         (buffer nil)
         (section nil)
         (created nil))
    (when locked
      (setq value
            (with-temp-buffer
              (mapc (lambda (binding) (set (car binding) (cadr binding)))
                    bindings)
              (let ((major-mode mode))
                (magit-buffer-value)))))
    (nelisp-emacs-magit-bridge--trace
     "setup-buffer BEGIN mode=%S dir=%S buffer-key=%S"
     mode dir buffer-key)
    (setq buffer
          (and buffer-key
               (get-buffer-create buffer-key)))
    (when buffer
      (setq section (magit-current-section)))
    (unless buffer
      (setq created t)
      (nelisp-emacs-magit-bridge--trace
       "setup-buffer new-buffer mode=%S value=%S"
       mode value)
      (let ((default-directory dir))
        (setq buffer
              (or (ignore-errors (magit-generate-new-buffer mode))
                  (generate-new-buffer
                   (format "*%s*" (symbol-name mode)))))))
    (when (stringp buffer)
      (setq buffer (get-buffer-create buffer)))
    (with-current-buffer buffer
      (setq magit-previous-section section)
      (set (make-local-variable 'default-directory) dir)
      (set (make-local-variable 'magit--default-directory) dir)
      (set (make-local-variable 'nelisp-emacs-magit-bridge--repo-root) dir)
      (nelisp-emacs-magit-bridge--trace
       "setup-buffer mode-init BEGIN buffer=%S"
       (buffer-name buffer))
      (funcall mode)
      (set (make-local-variable 'default-directory) dir)
      (set (make-local-variable 'magit--default-directory) dir)
      (set (make-local-variable 'nelisp-emacs-magit-bridge--repo-root) dir)
      (magit-xref-setup #'magit-setup-buffer-internal bindings)
      (mapc (lambda (binding) (set (car binding) (cadr binding))) bindings)
      (set (make-local-variable 'default-directory) dir)
      (set (make-local-variable 'magit--default-directory) dir)
      (set (make-local-variable 'nelisp-emacs-magit-bridge--repo-root) dir)
      (when created
        (nelisp-emacs-magit-bridge--trace
         "setup-buffer create-hook BEGIN buffer=%S"
         (buffer-name buffer))
        (run-hooks 'magit-create-buffer-hook)))
    (when created
      (nelisp-emacs-magit-bridge--trace
       "setup-buffer create-hook PASS buffer=%S"
       (buffer-name buffer)))
    (nelisp-emacs-magit-bridge--trace
     "setup-buffer display BEGIN buffer=%S" (buffer-name buffer))
    (magit-display-buffer buffer)
    (nelisp-emacs-magit-bridge--trace
     "setup-buffer display PASS buffer=%S" (buffer-name buffer))
    (with-current-buffer buffer
      (set (make-local-variable 'default-directory) dir)
      (set (make-local-variable 'magit--default-directory) dir)
      (set (make-local-variable 'nelisp-emacs-magit-bridge--repo-root) dir)
      (nelisp-emacs-magit-bridge--trace
       "setup-buffer setup-hook BEGIN buffer=%S"
       (buffer-name buffer))
      (run-hooks 'magit-setup-buffer-hook)
      (nelisp-emacs-magit-bridge--trace
       "setup-buffer setup-hook PASS buffer=%S"
       (buffer-name buffer))
      (nelisp-emacs-magit-bridge--trace
       "setup-buffer refresh BEGIN buffer=%S created=%S"
       (buffer-name buffer) created)
      (magit-refresh-buffer created
                            :initial-section initial-section
                            :select-section select-section)
      (when (and (> (buffer-size) 0)
                 (boundp 'magit-root-section)
                 magit-root-section)
        (nelisp-emacs-magit-bridge--put-current-buffer-text-property
         (point-min) (min (point-max) (1+ (point-min)))
         'magit-section
         magit-root-section)
        (goto-char (point-min))
        (nelisp-emacs-magit-bridge--trace
         "setup-buffer point-anchor PASS %S" magit-root-section))
      (nelisp-emacs-magit-bridge--trace
       "setup-buffer refresh PASS buffer=%S"
       (buffer-name buffer))
      (when created
        (nelisp-emacs-magit-bridge--trace
         "setup-buffer post-create-hook BEGIN buffer=%S"
         (buffer-name buffer))
        (run-hooks 'magit-post-create-buffer-hook)))
    (when created
      (nelisp-emacs-magit-bridge--trace
       "setup-buffer post-create-hook PASS buffer=%S"
       (buffer-name buffer)))
    (with-current-buffer buffer
      (when (and (> (buffer-size) 0)
                 (boundp 'magit-root-section)
                 magit-root-section)
        (nelisp-emacs-magit-bridge--put-current-buffer-text-property
         (point-min) (min (point-max) (1+ (point-min)))
         'magit-section
         magit-root-section)
        (goto-char (point-min))
        (nelisp-emacs-magit-bridge--trace
         "setup-buffer final-point-anchor PASS %S" magit-root-section)))
    (nelisp-emacs-magit-bridge--trace
     "setup-buffer PASS buffer=%S" (buffer-name buffer))
    buffer))

(defun nelisp-emacs-magit-bridge--ensure-magit-setup-buffer-directory-safe ()
  "Apply `:directory' before Magit looks up or creates mode buffers."
  (fset 'magit-setup-buffer-internal
        (symbol-function
         'nelisp-emacs-magit-bridge--magit-setup-buffer-internal)))

(defun nelisp-emacs-magit-bridge--ensure-magit-status-position-safe ()
  "Install a NeLisp-safe `magit-status--get-file-position' replacement."
  (fset 'magit-status--get-file-position
        (symbol-function
         'nelisp-emacs-magit-bridge--magit-status-get-file-position)))

(defun nelisp-emacs-magit-bridge--ensure-magit-status-initial-section-safe ()
  "Install a NeLisp-safe `magit-status-goto-initial-section' replacement."
  (fset 'magit-status-goto-initial-section
        (symbol-function
         'nelisp-emacs-magit-bridge--magit-status-goto-initial-section)))

(defun nelisp-emacs-magit-bridge--ensure-magit-section-visibility-safe ()
  "Install NeLisp-safe section visibility helpers."
  (fset 'magit-section-at
        (symbol-function
         'nelisp-emacs-magit-bridge--magit-section-at))
  (fset 'magit-current-section
        (symbol-function
         'nelisp-emacs-magit-bridge--magit-current-section))
  (fset 'magit-section-content-p
        (symbol-function
         'nelisp-emacs-magit-bridge--magit-section-content-p))
  (fset 'magit-section-show
        (symbol-function
         'nelisp-emacs-magit-bridge--magit-section-show))
  (fset 'magit-section-hide
        (symbol-function
         'nelisp-emacs-magit-bridge--magit-section-hide))
  (fset 'magit-section--set-section-properties
        (symbol-function
         'nelisp-emacs-magit-bridge--magit-section-set-section-properties)))

(defun nelisp-emacs-magit-bridge--ensure-magit-insert-section-macro ()
  "Install a NeLisp-safe `magit-insert-section' macro."
  (defmacro magit-insert-section (&rest args)
    "NeLisp-safe replacement for Magit's `magit-insert-section' macro."
    (let* ((bind (and (symbolp (car args)) (car args)))
           (rest (if bind (cdr args) args))
           (spec (car rest))
           (body (cdr rest))
           (class (car spec))
           (value (car (cdr spec)))
           (hide (car (cdr (cdr spec))))
           (more (cdr (cdr (cdr spec))))
           (obj (gensym "section"))
           (class-form (if (and (consp class)
                                (eq (car class) 'eval))
                           (cadr class)
                         (list 'quote class)))
           (body-form
            (if bind
                (cons 'let
                      (cons (list (list bind obj))
                            body))
              (cons 'progn body))))
      (list 'let*
            (list
             (list obj
                   (cons 'magit-insert-section--create
                         (cons class-form
                               (cons value
                                     (cons hide more)))))
             (list 'magit-insert-section--current obj)
             (list 'magit-insert-section--oldroot
                   (list 'or 'magit-insert-section--oldroot
                         (list 'and (list 'not 'magit-insert-section--parent)
                               (list 'prog1 'magit-root-section
                                     (list 'setq 'magit-root-section obj)))))
             (list 'magit-insert-section--parent obj))
            (list 'catch ''cancel-section
                  body-form
                  (list 'magit-insert-section--finish obj))
            obj))))

(defun nelisp-emacs-magit-bridge--ensure-magit-insert-section-finish ()
  "Install a NeLisp-safe `magit-insert-section--finish'."
  (fset 'magit-insert-section--finish
        (symbol-function
         'nelisp-emacs-magit-bridge--magit-insert-section-finish)))

(defun nelisp-emacs-magit-bridge--activate-mode
    (symbol name map internal-hook public-hook)
  "Set the current buffer's major mode metadata to SYMBOL/NAME/MAP.
Run INTERNAL-HOOK and PUBLIC-HOOK when available."
  (when (fboundp 'emacs-mode-set-major-mode)
    (emacs-mode-set-major-mode symbol name))
  (setq major-mode symbol)
  (setq mode-name name)
  (when (and map (boundp map))
    (use-local-map (symbol-value map)))
  (cond
   ((fboundp 'emacs-mode-run-mode-hooks)
    (emacs-mode-run-mode-hooks internal-hook public-hook))
   (t
    (run-hooks public-hook))))

(defun nelisp-emacs-magit-bridge--magit-section-mode ()
  "NeLisp-safe replacement for `magit-section-mode'."
  (interactive)
  (nelisp-emacs-magit-bridge--trace "mode magit-section BEGIN")
  (when (fboundp 'kill-all-local-variables)
    (kill-all-local-variables))
  (when (fboundp 'emacs-mode-fundamental-mode)
    (emacs-mode-fundamental-mode))
  (nelisp-emacs-magit-bridge--trace "mode magit-section base PASS")
  (nelisp-emacs-magit-bridge--activate-mode
   'magit-section-mode "Magit-Sections"
   'magit-section-mode-map
   'emacs-mode-magit-section-mode-hook
   'magit-section-mode-hook)
  (nelisp-emacs-magit-bridge--trace "mode magit-section activate PASS")
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq-local line-move-visual t)
  (setq-local font-lock-defaults '(nil t))
  (setq show-trailing-whitespace nil)
  (setq-local symbol-overlay-inhibit-map t)
  (setq list-buffers-directory (abbreviate-file-name default-directory))
  (make-local-variable 'text-property-default-nonsticky)
  (push (cons 'keymap t) text-property-default-nonsticky)
  (add-hook 'pre-command-hook #'magit-section-pre-command-hook nil t)
  (add-hook 'post-command-hook #'magit-section-post-command-hook t t)
  (add-hook 'deactivate-mark-hook #'magit-section-deactivate-mark t t)
  (setq-local redisplay-highlight-region-function
              #'magit-section--highlight-region)
  (setq-local redisplay-unhighlight-region-function
              #'magit-section--unhighlight-region)
  (add-function :filter-return (local 'filter-buffer-substring-function)
                #'magit-section--remove-text-properties)
  (when (fboundp 'magit-section-context-menu)
    (add-hook 'context-menu-functions #'magit-section-context-menu 10 t))
  (when magit-section-disable-line-numbers
    (when (and (fboundp 'linum-mode)
               (bound-and-true-p global-linum-mode))
      (linum-mode -1))
    (when (and (fboundp 'nlinum-mode)
               (bound-and-true-p global-nlinum-mode))
      (nlinum-mode -1))
    (when (and (fboundp 'display-line-numbers-mode)
               (bound-and-true-p display-line-numbers-mode))
      (display-line-numbers-mode -1)))
  (nelisp-emacs-magit-bridge--trace "mode magit-section PASS"))

(defun nelisp-emacs-magit-bridge--magit-mode ()
  "NeLisp-safe replacement for `magit-mode'."
  (interactive)
  (nelisp-emacs-magit-bridge--trace "mode magit BEGIN")
  (nelisp-emacs-magit-bridge--magit-section-mode)
  (nelisp-emacs-magit-bridge--activate-mode
   'magit-mode "Magit"
   'magit-mode-map
   'emacs-mode-magit-mode-hook
   'magit-mode-hook)
  (nelisp-emacs-magit-bridge--trace "mode magit activate PASS")
  (nelisp-emacs-magit-bridge--trace "mode magit dirlocals BEGIN")
  (magit-hack-dir-local-variables)
  (nelisp-emacs-magit-bridge--trace "mode magit dirlocals PASS")
  (nelisp-emacs-magit-bridge--trace "mode magit remap BEGIN")
  (face-remap-add-relative 'header-line 'magit-header-line)
  (nelisp-emacs-magit-bridge--trace "mode magit remap PASS")
  (nelisp-emacs-magit-bridge--trace "mode magit modeline BEGIN")
  (setq mode-line-process
        (ignore-errors
          (magit-repository-local-get 'mode-line-process)))
  (nelisp-emacs-magit-bridge--trace "mode magit modeline PASS")
  (nelisp-emacs-magit-bridge--trace "mode magit locals BEGIN")
  (setq-local revert-buffer-function #'magit-revert-buffer)
  (setq-local bookmark-make-record-function #'magit--make-bookmark)
  (setq-local imenu-create-index-function #'magit--imenu-create-index)
  (setq-local imenu-default-goto-function #'magit--imenu-goto-function)
  (setq-local isearch-filter-predicate #'magit-section--open-temporarily)
  (nelisp-emacs-magit-bridge--trace "mode magit locals PASS")
  (nelisp-emacs-magit-bridge--trace "mode magit PASS"))

(defun nelisp-emacs-magit-bridge--magit-status-mode ()
  "NeLisp-safe replacement for `magit-status-mode'."
  (interactive)
  (nelisp-emacs-magit-bridge--trace "mode magit-status BEGIN")
  (nelisp-emacs-magit-bridge--magit-mode)
  (nelisp-emacs-magit-bridge--activate-mode
   'magit-status-mode "Magit"
   'magit-status-mode-map
   'emacs-mode-magit-status-mode-hook
   'magit-status-mode-hook)
  (nelisp-emacs-magit-bridge--trace "mode magit-status activate PASS")
  (magit-hack-dir-local-variables)
  (setq magit--imenu-group-types '(not branch commit))
  (nelisp-emacs-magit-bridge--trace "mode magit-status PASS"))

(defun nelisp-emacs-magit-bridge--ensure-magit-mode-activators-safe ()
  "Install NeLisp-safe replacements for the Magit mode activators."
  (fset 'magit-section-mode
        (symbol-function
         'nelisp-emacs-magit-bridge--magit-section-mode))
  (fset 'magit-mode
        (symbol-function
         'nelisp-emacs-magit-bridge--magit-mode))
  (fset 'magit-status-mode
        (symbol-function
         'nelisp-emacs-magit-bridge--magit-status-mode)))

(defun nelisp-emacs-magit-bridge--resolve-use-buffer-args (use-buffer-args)
  "Return the effective Magit buffer-args policy for USE-BUFFER-ARGS."
  (pcase-exhaustive use-buffer-args
    ('prefix magit-prefix-use-buffer-arguments)
    ('status magit-status-use-buffer-arguments)
    ('direct magit-direct-use-buffer-arguments)
    ('nil magit-direct-use-buffer-arguments)
    ((or 'always 'selected 'current 'never)
     use-buffer-args)))

(defun nelisp-emacs-magit-bridge--magit-diff-get-value
    (mode &optional use-buffer-args)
  "NeLisp-safe replacement for `magit-diff--get-value'."
  (setq use-buffer-args
        (nelisp-emacs-magit-bridge--resolve-use-buffer-args
         use-buffer-args))
  (cond
   ((and (memq use-buffer-args '(always selected current))
         (eq major-mode mode))
    (list magit-buffer-diff-args
          magit-buffer-diff-files))
   ((memq use-buffer-args '(always selected))
    (let ((buffer (magit-get-mode-buffer mode nil
                                         (eq use-buffer-args 'selected))))
      (list (buffer-local-value 'magit-buffer-diff-args buffer)
            (buffer-local-value 'magit-buffer-diff-files buffer))))
   ((plist-member (symbol-plist mode) 'magit-diff-current-arguments)
    (list (get mode 'magit-diff-current-arguments) nil))
   (t
    (let ((elt (assq (intern (format "magit-diff:%s" mode))
                     transient-values)))
      (if elt
          (list (cdr elt) nil)
        (list (get mode 'magit-diff-default-arguments) nil))))))

(defun nelisp-emacs-magit-bridge--magit-log-get-value
    (mode &optional use-buffer-args)
  "NeLisp-safe replacement for `magit-log--get-value'."
  (setq use-buffer-args
        (nelisp-emacs-magit-bridge--resolve-use-buffer-args
         use-buffer-args))
  (cond
   ((and (memq use-buffer-args '(always selected current))
         (eq major-mode mode))
    (list magit-buffer-log-args
          magit-buffer-log-files))
   ((memq use-buffer-args '(always selected))
    (let ((buffer (magit-get-mode-buffer mode nil
                                         (eq use-buffer-args 'selected))))
      (list (buffer-local-value 'magit-buffer-log-args buffer)
            (buffer-local-value 'magit-buffer-log-files buffer))))
   ((plist-member (symbol-plist mode) 'magit-log-current-arguments)
    (list (get mode 'magit-log-current-arguments) nil))
   (t
    (let ((elt (assq (intern (format "magit-log:%s" mode))
                     transient-values)))
      (if elt
          (list (cdr elt) nil)
        (list (get mode 'magit-log-default-arguments) nil))))))

(defun nelisp-emacs-magit-bridge--ensure-magit-buffer-arg-getters-safe ()
  "Install NeLisp-safe replacements for Magit's buffer-arg getters."
  (fset 'magit-diff-type
        (symbol-function
         'nelisp-emacs-magit-bridge--magit-diff-type))
  (fset 'magit-diff-scope
        (symbol-function
         'nelisp-emacs-magit-bridge--magit-diff-scope))
  (fset 'magit-diff--get-value
        (symbol-function
         'nelisp-emacs-magit-bridge--magit-diff-get-value))
  (fset 'magit-log--get-value
        (symbol-function
         'nelisp-emacs-magit-bridge--magit-log-get-value)))

(defun nelisp-emacs-magit-bridge--ensure-magit-setup-buffer-macro ()
  "Install a simpler `magit-setup-buffer' macro for the NeLisp runtime.

The vendor macro uses `pcase-lambda' over the binding tail to build the
`(VAR FORM)' pairs passed to `magit-setup-buffer-internal'.  In the
current standalone runtime that expansion can leak the temporary symbol
`var' as a free variable, aborting `magit-status-setup-buffer' with
`void-variable var'.  The semantics we need are small and stable:
split keyword args from binding pairs and build the same
`magit-setup-buffer-internal' call without `pcase-lambda'."
  (defmacro magit-setup-buffer (mode &optional locked &rest args)
    "NeLisp-safe replacement for Magit's `magit-setup-buffer' macro."
    (let ((rest args)
          (kwargs nil)
          (bindings nil))
      (while (keywordp (car rest))
        (push (pop rest) kwargs)
        (push (pop rest) kwargs))
      (while rest
        (let ((var (pop rest))
              (form (pop rest)))
          (push `(list ',var ,form) bindings)))
      `(magit-setup-buffer-internal
        ,mode
        ,locked
        (list ,@(nreverse bindings))
        ,@(nreverse kwargs)))))

(defun nelisp-emacs-magit-bridge--ensure-tool-bar-runtime ()
  "Ensure `tool-bar-local-item' exists before the vendor chain loads.
Vendor `help-mode.el' builds `help-mode-tool-bar-map' at load time with
`tool-bar-local-item', which lives in `tool-bar.el' — a file nothing in
this bridge loaded, so the bundle died with `void-function'
\(measured 2026-08-03).  The vendored `tool-bar.el' loads cleanly on the
standalone runtime, so use it instead of a stub."
  (unless (fboundp 'tool-bar-local-item)
    (let ((root (nelisp-emacs-magit-bridge--repo-root)))
      (load (expand-file-name "vendor/emacs-lisp/tool-bar.el" root)
            nil 'no-message t t))))

(defun nelisp-emacs-magit-bridge--ensure-vendor-preload-globals ()
  "Ensure vendor-file load-time globals from never-loaded Emacs files exist.

Bundle parts 4-11 execute vendor top-level forms that reference
variables and functions whose *defining* Emacs file is not part of the
bundle (desktop.el, vc-hooks.el, files.el, subr.el, mule.el).  Real
Emacs preloads or autoloads those, so the vendor files never notice;
on this substrate each one died as `void-variable'/`void-function'
\(measured 2026-08-14 by an auto-collecting walk over parts 4-20 after
the symbol-value flagless-abort core fix made these lookups signal
instead of killing the process silently -- dev/nelisp 57ee800c).

Variables (all reached from vendor top-level forms):
- `desktop-buffer-mode-handlers' (desktop.el; info.el part-4 form #298
  does `add-to-list' on it -- THE original part-4 silent stop)
- `vc-git-log-view-mode-map' (part 5), `mode-line-misc-info' (part 5)
- `idle-update-delay' (subr.el default 0.5; part 5)

Functions:
- `file-chase-links' (files.el): real symlink-chasing implementation,
  bounded like the original (100-hop safety cap, optional LIMIT).
- `process-lines-ignore-status' (subr.el): real implementation via
  `call-process' -- NOT a stub; magit's vc/git probes call it for real
  output and a nil stub would silently break them downstream.
- `coding-system-get' (mule.el): the substrate has no coding-system
  attribute table at all, so every property is honestly \"unknown\" =
  nil.  This is a documented substrate gap, not a lazy stub: callers
  in the vendor chain only use it for optional decoration (eol/BOM
  display), and nil takes their fallback branch."
  (unless (boundp 'desktop-buffer-mode-handlers)
    (defvar desktop-buffer-mode-handlers nil))
  (unless (boundp 'vc-git-log-view-mode-map)
    ;; Keymap variables must hold a real (empty) keymap, not nil: vendor
    ;; code derives child maps from them / passes them to keymap builtins,
    ;; and nil dies as `emacs-keymap-not-keymap' (measured on part 15 with
    ;; a nil `minibuffer-local-completion-map' placeholder, 2026-08-14).
    (defvar vc-git-log-view-mode-map (make-sparse-keymap)))
  (unless (boundp 'minibuffer-local-completion-map)
    (defvar minibuffer-local-completion-map (make-sparse-keymap)))
  ;; Siblings from the same minibuffer.el keymap family: the magit-status
  ;; smoke's EXECUTION phase (first phase past load-time) died on
  ;; `minibuffer-local-must-match-map' (measured 2026-08-15); provide the
  ;; base map too, the read paths chain through both.
  (unless (boundp 'minibuffer-local-must-match-map)
    (defvar minibuffer-local-must-match-map (make-sparse-keymap)))
  (unless (boundp 'minibuffer-local-map)
    (defvar minibuffer-local-map (make-sparse-keymap)))
  ;; Batch closure of the same class (static sweep of all 20 bundle
  ;; parts for *-map/*-menu reads minus bundle-defined symbols,
  ;; 2026-08-15, after isearch-mode-map died one smoke round after
  ;; minibuffer-local-must-match-map): real-Emacs preloaded keymap
  ;; VARIABLES only.  Deliberately excluded: generic names that vendor
  ;; code let-binds lexically (local-map, default-map -- a defvar would
  ;; flip those lets to dynamic), magit's own symbols (resolved inside
  ;; the bundle), and function names the sweep cannot distinguish.
  ;; overriding-local-map / overriding-terminal-local-map get nil --
  ;; that IS their real-Emacs default, not a placeholder.
  (unless (boundp 'isearch-mode-map)
    (defvar isearch-mode-map (make-sparse-keymap)))
  (unless (boundp 'button-map)
    (defvar button-map (make-sparse-keymap)))
  (unless (boundp 'button-buffer-map)
    (defvar button-buffer-map (make-sparse-keymap)))
  ;; REMOVED from the batch (measured 2026-08-15): pre-binding
  ;; `tool-bar-map' turned bundle part 16's load into a deterministic
  ;; SIGSEGV (isolated by leave-one-out + an inert same-shape filler
  ;; substitution: the dummy survives, the real name dies -- semantic,
  ;; not layout).  The real map belongs to vendor tool-bar.el, which
  ;; PRECOND 51 loads; an empty pre-bound map collides with that path.
  ;; `ctl-x-map', `local-function-key-map', and the two overriding-*
  ;; variables were dropped with it: none was a measured blocker, and
  ;; speculative over-provisioning is exactly what caused this crash.
  ;; If execution ever reads one, it now fails loudly by name.
  (unless (boundp 'read-expression-map)
    (defvar read-expression-map (make-sparse-keymap)))
  (unless (boundp 'y-or-n-p-map)
    (defvar y-or-n-p-map (make-sparse-keymap)))
  (unless (boundp 'minibuffer-visible-completions-map)
    (defvar minibuffer-visible-completions-map (make-sparse-keymap)))
  (unless (boundp 'vc-log-mode-map)
    (defvar vc-log-mode-map (make-sparse-keymap)))
  (unless (boundp 'vc-dir-filename-mouse-map)
    (defvar vc-dir-filename-mouse-map (make-sparse-keymap)))
  (unless (boundp 'vc-dir-status-mouse-map)
    (defvar vc-dir-status-mouse-map (make-sparse-keymap)))
  (unless (boundp 'bug-reference-map)
    (defvar bug-reference-map (make-sparse-keymap)))
  (unless (boundp 'tool-bar-map)
    (defvar tool-bar-map (make-sparse-keymap)))
  (unless (boundp 'mode-line-misc-info)
    (defvar mode-line-misc-info nil))
  (unless (boundp 'idle-update-delay)
    (defvar idle-update-delay 0.5))
  (unless (boundp 'completion-styles)
    ;; Real Emacs defaults to (basic partial-completion emacs22); `basic'
    ;; alone is the conservative subset whose style functions the
    ;; substrate is known to carry.
    (defvar completion-styles (list 'basic)))
  (unless (boundp 'completion-category-defaults)
    (defvar completion-category-defaults nil))
  (unless (fboundp 'file-chase-links)
    (defun file-chase-links (filename &optional limit)
      "Chase links in FILENAME until a name that is not a link.
Does not examine containing directories for links.  If LIMIT is a
number, stop after that many link expansions."
      (let ((tem filename) (count 100) (link nil) (done nil))
        (while (not done)
          (setq link (and (fboundp 'file-symlink-p) (file-symlink-p tem)))
          (if (or (not link) (<= count 0) (and limit (<= limit 0)))
              (setq done t)
            (setq count (1- count))
            (when limit (setq limit (1- limit)))
            (setq tem (if (file-name-absolute-p link)
                          link
                        (expand-file-name
                         link (file-name-directory tem))))))
        tem)))
  (unless (fboundp 'process-lines-ignore-status)
    (defun process-lines-ignore-status (program &rest args)
      "Execute PROGRAM with ARGS, returning its output as a list of lines.
Ignore the exit status of PROGRAM (unlike `process-lines')."
      (with-temp-buffer
        (apply #'call-process program nil (current-buffer) nil args)
        (goto-char (point-min))
        (let ((lines nil))
          (while (not (eobp))
            (setq lines (cons (buffer-substring-no-properties
                               (line-beginning-position)
                               (line-end-position))
                              lines))
            (forward-line 1))
          (nreverse lines)))))
  (unless (fboundp 'coding-system-get)
    (defun coding-system-get (_coding-system _prop)
      "Return nil: the substrate has no coding-system attribute table.
See `nelisp-emacs-magit-bridge--ensure-vendor-preload-globals'."
      nil))
  ;; menu-bar.el is never loaded either; log-edit's tool-bar defvar (part 15
  ;; line 582 in the bundle) reads `menu-bar-edit-menu' at load time via
  ;; (lookup-key menu-bar-edit-menu [cut]) -- an empty keymap makes that a
  ;; benign nil, and `menu-bar-separator' is its string form ("--").
  (unless (boundp 'menu-bar-edit-menu)
    (defvar menu-bar-edit-menu (make-sparse-keymap)))
  (unless (boundp 'menu-bar-separator)
    (defvar menu-bar-separator "--"))
  ;; text-mode.el is not bundled; message.el's `message-tab' consults
  ;; `text-mode-map' at RUN time (never at load), so an empty keymap keeps
  ;; the fallback chain ((lookup-key text-mode-map "\t") -> global-map)
  ;; working instead of dying on nil.
  (unless (boundp 'text-mode-map)
    (defvar text-mode-map (make-sparse-keymap)))
  ;; Parts 17-20 load-time reads (harvested by walk v9, 2026-08-15):
  ;; tabulated-list.el / project.el / startup.el are not bundled.
  ;; `after-init-time' nil is even the real-Emacs value while startup
  ;; is still in progress, so nil is honest here, not a placeholder.
  (unless (boundp 'tabulated-list-mode-map)
    (defvar tabulated-list-mode-map (make-sparse-keymap)))
  (unless (boundp 'project-prefix-map)
    (defvar project-prefix-map (make-sparse-keymap)))
  (unless (boundp 'project-switch-commands)
    (defvar project-switch-commands nil))
  (unless (boundp 'after-init-time)
    (defvar after-init-time nil))
  ;; `tool-bar-local-item-from-menu' (vendor tool-bar.el, loaded by PRECOND
  ;; 51 which runs before this) resolves menu entries through global-map's
  ;; [menu-bar] subtree -- which this substrate does not populate, so the
  ;; lookup chain hands nil to a keymap builtin and dies as
  ;; `emacs-keymap-not-keymap' (measured: part 15 log-edit-tool-bar-map,
  ;; bisected 2026-08-14).  Tool bars are inert decoration on this batch
  ;; substrate (same rationale as the PRECOND 36 ansi-color stub), so make
  ;; only the from-menu variant a no-op; `tool-bar-local-item' (help-mode's
  ;; need, the reason PRECOND 51 loads the real file) stays real.
  (fset 'tool-bar-local-item-from-menu (lambda (&rest _ignored) nil)))

(defun nelisp-emacs-magit-bridge--ensure-bundled-forward-features ()
  "Pre-provide bundled features that earlier parts `require' forward.

Bundle parts 16-19 carry vendor top-level `(require 'magit)' forms,
but the `magit' feature itself is provided by part 20: in real Emacs
the `require' would load magit.el from `load-path' on demand, while
here it dies with \"Cannot open load file: magit\" (measured on part
16, 2026-08-15).  The bundle generator already topologically orders
the REAL definition dependencies, so satisfying the feature check
up front is sound; the actual part-20 forms still execute because the
bundle's own resume guard tracks
`nelisp-emacs-magit-bridge-bundle--loaded-features', a separate
variable from `features'.

Known risk, accepted deliberately: vendor code that probes
`(featurep 'magit)' during parts 1-19 to conditionally integrate will
see t early.  Any resulting reference to a not-yet-loaded definition
now fails LOUDLY (void-function/void-variable) thanks to the
substrate's signal fixes, so this cannot regress into a silent stop."
  (unless (featurep 'magit)
    (provide 'magit)))

(defun nelisp-emacs-magit-bridge--precond-trace (text)
  "Write TEXT as a precondition progress marker."
  (if (fboundp 'nelisp--write-stdout-bytes)
      (nelisp--write-stdout-bytes text)
    (ignore-errors (princ text))))

(defun nelisp-emacs-magit-bridge--ensure-preconditions ()
  "Ensure every session precondition the vendor chain assumes is live."
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND-START\n")
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 01 lambda-documentation-form\n")
  (nelisp-emacs-magit-bridge--ensure-lambda-documentation-form)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 02 current-buffer\n")
  (nelisp-emacs-magit-bridge--ensure-current-buffer)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 03 standalone-runtime\n")
  (nelisp-emacs-magit-bridge--ensure-standalone-runtime)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 04 process-substrate\n")
  (nelisp-emacs-magit-bridge--ensure-process-substrate)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 05 sync-process-capture-safe\n")
  (nelisp-emacs-magit-bridge--ensure-sync-process-capture-safe)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 06 search-substrate\n")
  (nelisp-emacs-magit-bridge--ensure-search-substrate)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 07 list-runtime\n")
  (nelisp-emacs-magit-bridge--ensure-list-runtime)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 08 symbol-runtime\n")
  (nelisp-emacs-magit-bridge--ensure-symbol-runtime)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 09 callproc-runtime\n")
  (nelisp-emacs-magit-bridge--ensure-callproc-runtime)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 10 fileio-runtime\n")
  (nelisp-emacs-magit-bridge--ensure-fileio-runtime)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 11 subr-extra-primitives\n")
  (nelisp-emacs-magit-bridge--ensure-subr-extra-primitives)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 12 time-runtime\n")
  (nelisp-emacs-magit-bridge--ensure-time-runtime)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 13 emacs-version-identity\n")
  (nelisp-emacs-magit-bridge--ensure-emacs-version-identity)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 14 buffer-defaults\n")
  (nelisp-emacs-magit-bridge--ensure-buffer-defaults)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 15 buffer-predicate\n")
  (nelisp-emacs-magit-bridge--ensure-buffer-predicate)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 16 buffer-selection-builtins\n")
  (nelisp-emacs-magit-bridge--ensure-buffer-selection-builtins)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 17 find-buffer-visiting\n")
  (nelisp-emacs-magit-bridge--ensure-find-buffer-visiting)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 18 static-if\n")
  (nelisp-emacs-magit-bridge--ensure-static-if)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 19 pcase-runtime\n")
  (nelisp-emacs-magit-bridge--ensure-pcase-runtime)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 20 macroexp-runtime\n")
  (nelisp-emacs-magit-bridge--ensure-macroexp-runtime)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 21 advice-runtime\n")
  (nelisp-emacs-magit-bridge--ensure-advice-runtime)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 22 message-runtime\n")
  (nelisp-emacs-magit-bridge--ensure-message-runtime)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 23 transient-globals\n")
  (nelisp-emacs-magit-bridge--ensure-transient-globals)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 24 cl-generic-runtime-shims\n")
  (nelisp-emacs-magit-bridge--ensure-cl-generic-runtime-shims)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 25 cl-next-method-helpers\n")
  (nelisp-emacs-magit-bridge--ensure-cl-next-method-helpers)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 26 cl-find-class-setter\n")
  (nelisp-emacs-magit-bridge--ensure-cl-find-class-setter)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 27 eieio-core-structs\n")
  (nelisp-emacs-magit-bridge--ensure-eieio-core-structs)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 28 cl-generic-define-generalizer\n")
  (nelisp-emacs-magit-bridge--ensure-cl-generic-define-generalizer)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 29 cl-declaim\n")
  (nelisp-emacs-magit-bridge--ensure-cl-declaim)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 30 defalias-forward-reference\n")
  (nelisp-emacs-magit-bridge--ensure-defalias-forward-reference)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 31 symbol-function-function-form\n")
  (nelisp-emacs-magit-bridge--ensure-symbol-function-function-form)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 32 function-alias-p\n")
  (nelisp-emacs-magit-bridge--ensure-function-alias-p)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 33 autoload-runtime\n")
  (nelisp-emacs-magit-bridge--ensure-autoload-runtime)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 34 sxhash-eq\n")
  (nelisp-emacs-magit-bridge--ensure-sxhash-eq)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 35 slot-boundp-safe\n")
  (nelisp-emacs-magit-bridge--ensure-slot-boundp-safe)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 36 ansi-color-update-face-vec-stub\n")
  (nelisp-emacs-magit-bridge--ensure-ansi-color-update-face-vec-stub)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 37 compat-maybe-require\n")
  (nelisp-emacs-magit-bridge--ensure-compat-maybe-require)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 38 default-process-coding-system\n")
  (nelisp-emacs-magit-bridge--ensure-default-process-coding-system)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 39 coding-system-change-eol-conversion\n")
  (nelisp-emacs-magit-bridge--ensure-coding-system-change-eol-conversion)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 40 backquote-marker-symbols\n")
  (nelisp-emacs-magit-bridge--ensure-backquote-marker-symbols)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 41 files-el-globals\n")
  (nelisp-emacs-magit-bridge--ensure-files-el-globals)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 42 window-el-globals\n")
  (nelisp-emacs-magit-bridge--ensure-window-el-globals)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 43 window-selection-macros\n")
  (nelisp-emacs-magit-bridge--ensure-window-selection-macros)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 44 window-builtins\n")
  (nelisp-emacs-magit-bridge--ensure-window-builtins)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 45 window-display-helpers\n")
  (nelisp-emacs-magit-bridge--ensure-window-display-helpers)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 46 uniquify-globals\n")
  (nelisp-emacs-magit-bridge--ensure-uniquify-globals)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 47 simple-el-globals\n")
  (nelisp-emacs-magit-bridge--ensure-simple-el-globals)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 48 third-party-soft-vars\n")
  (nelisp-emacs-magit-bridge--ensure-third-party-soft-vars)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 49 docstring-fill-helpers\n")
  (nelisp-emacs-magit-bridge--ensure-docstring-fill-helpers)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 50 keymap-constructors\n")
  (nelisp-emacs-magit-bridge--ensure-keymap-constructors)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 51 tool-bar-runtime\n")
  (nelisp-emacs-magit-bridge--ensure-tool-bar-runtime)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 52 special-mode\n")
  (nelisp-emacs-magit-bridge--ensure-special-mode)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 53 magit-setup-buffer-macro\n")
  (nelisp-emacs-magit-bridge--ensure-magit-setup-buffer-macro)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 54 save-some-buffers\n")
  (nelisp-emacs-magit-bridge--ensure-save-some-buffers)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 55 vendor-preload-globals\n")
  (nelisp-emacs-magit-bridge--ensure-vendor-preload-globals)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND 56 bundled-forward-features\n")
  (nelisp-emacs-magit-bridge--ensure-bundled-forward-features)
  (nelisp-emacs-magit-bridge--precond-trace "PRECOND-ALL-OK\n"))

(defun nelisp-emacs-magit-bridge-load ()
  "Load the real vendor Magit chain into the current NeLisp session.

Idempotent: a second call is a no-op once `nelisp-emacs-magit-bridge-loaded'
is set.  Signals an error naming the missing file when the generated
bundle has not been built yet (`make bake-magit-runtime-image' or
`make -f Makefile build/nelisp-emacs-magit-bridge-bundle.el' builds it via
`scripts/build-nelisp-emacs-magit-bridge-bundle.el' under host Emacs)."
  (unless nelisp-emacs-magit-bridge-loaded
    (nelisp-emacs-magit-bridge--trace "preconditions BEGIN")
    (nelisp-emacs-magit-bridge--ensure-preconditions)
    (nelisp-emacs-magit-bridge--trace "preconditions PASS")
    ;; Arm the diagnostic traps HERE, before the first bundle part runs.
    ;; Arming them from the per-part hook was too late: that hook first fires
    ;; only after part 1, and the heartbeat showed exactly one such call
    ;; (part=1) with both traps reporting nothing -- i.e. whatever built the
    ;; nil-class object had already run inside part 1.
    (nelisp-emacs-magit-bridge--wrap-oset-default)
    (nelisp-emacs-magit-bridge--wrap-make-instance)
    (nelisp-emacs-magit-bridge--symverify)
    (let ((bundle (nelisp-emacs-magit-bridge--bundle-file)))
      (unless (file-readable-p bundle)
        (error "nelisp-emacs-magit-bridge: bundle not built: %s (run scripts/build-nelisp-emacs-magit-bridge-bundle.el under host Emacs)"
               bundle))
      (nelisp-emacs-magit-bridge--trace "bundle-load BEGIN %S" bundle)
      (load bundle nil 'no-message t t)
      ;; Some runtime images still carry stubbed unprefixed buffer builtins,
      ;; and bundle loading can reintroduce those placeholder bindings.
      ;; Re-assert the shared `nelisp-ec-*' substrate on the symbols Magit
      ;; actually executes before the post-load runtime shims run.
      (nelisp-emacs-magit-bridge--ensure-advice-runtime)
      (nelisp-emacs-magit-bridge--ensure-buffer-builtins)
      (nelisp-emacs-magit-bridge--ensure-buffer-predicate)
      (nelisp-emacs-magit-bridge--trace "bundle-load PASS")
      (unless (equal (getenv "NEMACS_MAGIT_BRIDGE_SKIP_RUNTIME_SHIMS") "1")
        (nelisp-emacs-magit-bridge--trace "runtime-shims BEGIN")
        (nelisp-emacs-magit-bridge--ensure-magit-section-class-manual)
        (nelisp-emacs-magit-bridge--ensure-magit-file-section-constructor)
        (nelisp-emacs-magit-bridge--ensure-eieio-object-access-safe)
        (nelisp-emacs-magit-bridge--repair-transient-class-registry)
        (nelisp-emacs-magit-bridge--ensure-magit-buffer-arg-getters-safe)
        (nelisp-emacs-magit-bridge--ensure-magit-insert-section-macro)
        (nelisp-emacs-magit-bridge--ensure-magit-insert-section-finish)
        (nelisp-emacs-magit-bridge--ensure-magit-mode-activators-safe)
        (nelisp-emacs-magit-bridge--ensure-magit-status-position-safe)
        (nelisp-emacs-magit-bridge--ensure-magit-status-initial-section-safe)
        (nelisp-emacs-magit-bridge--ensure-magit-section-visibility-safe)
        (nelisp-emacs-magit-bridge--ensure-transient-init-scope-safe)
        (nelisp-emacs-magit-bridge--ensure-transient-init-value-safe)
        (nelisp-emacs-magit-bridge--ensure-transient-setup-children-safe)
        (nelisp-emacs-magit-bridge--ensure-transient-suffix-key-safe)
        (nelisp-emacs-magit-bridge--ensure-transient-init-prefix-safe)
        (nelisp-emacs-magit-bridge--ensure-transient-common-commands-ready)
        (nelisp-emacs-magit-bridge--ensure-transient-init-suffixes-safe)
        (nelisp-emacs-magit-bridge--ensure-transient-child-init-safe)
        (nelisp-emacs-magit-bridge--ensure-transient-layout-walkers-safe)
        (nelisp-emacs-magit-bridge--ensure-transient-command-key-safe)
        (nelisp-emacs-magit-bridge--ensure-transient-setup-safe)
        (nelisp-emacs-magit-bridge--ensure-magit-setup-buffer-macro)
        (nelisp-emacs-magit-bridge--ensure-magit-buffer-name-default)
        (nelisp-emacs-magit-bridge--ensure-magit-git-executable)
        (nelisp-emacs-magit-bridge--ensure-sync-process-capture-safe)
        (nelisp-emacs-magit-bridge--ensure-magit-toplevel-safe)
        (nelisp-emacs-magit-bridge--ensure-standalone-setf-places)
        (nelisp-emacs-magit-bridge--ensure-cl-compat-increments)
        (nelisp-emacs-magit-bridge--ensure-magit-margin-hook-safe)
        (nelisp-emacs-magit-bridge--ensure-magit-save-hooks-safe)
        (nelisp-emacs-magit-bridge--ensure-magit-status-sections-hook-safe)
        (nelisp-emacs-magit-bridge--ensure-magit-insert-section-hook-safe)
        (nelisp-emacs-magit-bridge--ensure-magit-refresh-buffer-hook-safe)
        (nelisp-emacs-magit-bridge--ensure-magit-highlight-safe)
        (nelisp-emacs-magit-bridge--ensure-magit-setup-buffer-directory-safe)
        (nelisp-emacs-magit-bridge--ensure-magit-refresh-traceable)
        (nelisp-emacs-magit-bridge--ensure-buffer-builtins)
        (nelisp-emacs-magit-bridge--ensure-buffer-predicate)
        (nelisp-emacs-magit-bridge--trace "runtime-shims PASS"))
      (setq nelisp-emacs-magit-bridge-loaded t)))
  nelisp-emacs-magit-bridge-loaded)

(defun nelisp-emacs-magit-bridge-loaded-p ()
  "Return non-nil once the real vendor Magit chain is live in this session.

Checks `commandp' on the two known silent-drop-prone modes (per Doc 33's
established finding: `featurep' alone can be true for a mode whose
`define-derived-mode' body was silently dropped), not just `featurep'."
  (and (featurep 'magit)
       (fboundp 'magit-status)
       (commandp 'magit-status)
       (fboundp 'magit-status-mode)
       (commandp 'magit-status-mode)
       (fboundp 'magit-run-git)
       (boundp 'magit-mode-map)
       (keymapp magit-mode-map)))

(provide 'nelisp-emacs-magit-bridge)

;;; nelisp-emacs-magit-bridge.el ends here
