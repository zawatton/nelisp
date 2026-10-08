;;; nelisp-load.el --- Multi-form NeLisp loader  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; Author: zawatton <kurozawawo@gmail.com>

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

;; Phase 2 Week 1-2 entry point: read multi-form NeLisp source from a
;; string or file and evaluate every top-level form against the same
;; global state (defun installs persist, defvar values persist, etc.).
;;
;; This is the substrate the rest of Phase 2 builds on.  Once the
;; reader handles backquote / char literals / floats and the evaluator
;; can host the helpers used inside `nelisp-eval.el' itself
;; (`make-hash-table' / `define-error' / `dolist' etc.), the same
;; loader will execute NeLisp's own implementation files unchanged —
;; the cycle 1 / cycle 2 fixpoint promised in 05-roadmap §2.1.
;;
;; The `.nl' extension is convention only; any text file containing
;; valid NeLisp sexps works.

;;; Code:

(require 'nelisp-read)
(require 'nelisp-eval)
;; Phase 5-A.3: `nelisp-require' goes through `nelisp-eval' which
;; relies on core-macro install (defmacro / and / or / when / etc.).
;; Pre Phase 5-A.3, `nelisp-load-string' could skip this because the
;; Phase 2 `require' stub returned early.  Now that `require' actually
;; loads files the macro layer must be present.
(require 'nelisp-macro)
;; Doc 141 Stage 2: loader file I/O now depends only on the minimal
;; core file layer.  Path resolution, readability checks, and UTF-8
;; source reads live in `nelisp-core-fileio'; editor/buffer concepts
;; remain in the package-side Emacs compatibility layer.
(require 'nelisp-core-fileio)

(define-error 'nelisp-load-error "NeLisp load error")

(defvar nelisp-load-path nil
  "List of directories searched by `nelisp-locate-file' / `nelisp-require'.
Independent from the host Emacs `load-path' per Doc 12 §2.2 C.
When `nelisp-load-path-include-host' is non-nil this list is still
consulted first; the host path only fills in misses.

NeLisp self-host reads this file through its own evaluator, which
does not know `defcustom'.  Promoting to `defcustom' would require
adding the customize primitive to the NeLisp dispatch table; for
Phase 5-A a plain `defvar' is sufficient.")

(defvar nelisp-load-path-include-host nil
  "If non-nil, `nelisp-locate-file' falls back to host `load-path'.
Doc 12 §2.2 C.  Defaults to nil so NeLisp package loading is
deterministic — host library changes cannot silently shadow a
NeLisp-authored feature.")

(defvar nelisp-load-prefer-artifacts t
  "If non-nil, `nelisp-load-file' tries fresh adjacent artifacts first.
The probe order is NeLisp-native `.neln' first, then portable `.nelc'.
GNU Emacs `.elc' artifacts are intentionally not used here because
`nelisp-load-file' loads into the NeLisp runtime, not the host Emacs
function namespace.")

(defvar nelisp-load-auto-compile-artifacts nil
  "If non-nil, `nelisp-load-file' refreshes adjacent artifacts on demand.
When no fresh adjacent artifact exists, the loader asks
`nelisp-artifact' to compile SOURCE.el to `nelisp-load-auto-compile-kind'
and then loads the produced artifact.  Source replay remains the fallback
if compilation is unavailable or fails.")

(defvar nelisp-load-auto-compile-kind 'neln
  "Artifact kind used by `nelisp-load-auto-compile-artifacts'.")

(defvar nelisp-load-auto-compile-target nil
  "Optional native target string for on-demand artifact compilation.")

(defvar nelisp-load-auto-compile-native-policy 'opportunistic
  "Native policy for on-demand artifact compilation.
See `nelisp-artifact-default-native-policy'.")

(defvar nelisp-load-auto-compile-load-paths nil
  "Extra load-path directories recorded in on-demand artifact manifests.")

(defvar nelisp-load-auto-compile-preloads nil
  "Preload files recorded in on-demand artifact manifests.")

(defvar nelisp-load--artifact-probe-active nil
  "Non-nil while `nelisp-load-file' is probing artifact fallbacks.")

;; These references are deliberately captured from GNU Emacs itself.
;; NeLisp's `eval-after-load' adapter below delegates all registration
;; and ordering to these functions rather than maintaining a parallel queue.
(defvar nelisp-load--host-eval-after-load
  (and (fboundp 'eval-after-load) (symbol-function 'eval-after-load)))
(defvar nelisp-load--host-provide
  (and (fboundp 'provide) (symbol-function 'provide)))
(defvar nelisp-load--host-after-load-evaluation
  (and (fboundp 'do-after-load-evaluation)
       (symbol-function 'do-after-load-evaluation)))

;; GNU byte-compiled files contain reader literals (`#[...]') and
;; top-level `(byte-code ...)' forms that the NeLisp source reader and
;; evaluator do not represent as source forms.  Capture the host reader
;; and evaluator before NeLisp installs names with the same spelling.
(defvar nelisp-load--host-read-from-string
  (and (fboundp 'read-from-string) (symbol-function 'read-from-string)))
(defvar nelisp-load--host-eval
  (and (fboundp 'eval) (symbol-function 'eval)))
(defvar nelisp-load--host-check-parens
  (and (fboundp 'check-parens) (symbol-function 'check-parens)))

(defvar nelisp-load--current-file nil
  "Canonical source path currently evaluated by `nelisp-load-string'.")

(defvar nelisp-load--current-features nil
  "Features provided by the current source file.")

(declare-function nelisp-artifact-load-source-file "nelisp-artifact"
                  (source-path &optional kinds))
(declare-function nelisp-artifact-load-or-compile-source-file "nelisp-artifact"
                  (source-path &optional kinds kind target load-paths preloads
                               native-policy))

(defun nelisp-load--try-artifact (path)
  "Try to load a valid compiled artifact for source PATH.
Return a plist with `:artifact' and `:value' on a hit, or nil on a miss."
  (when (and nelisp-load-prefer-artifacts
             (not nelisp-load--artifact-probe-active))
    (let ((nelisp-load--artifact-probe-active t))
      (when (and (require 'nelisp-artifact nil t)
                 (fboundp 'nelisp-artifact-load-source-file))
        (if (and nelisp-load-auto-compile-artifacts
                 (fboundp 'nelisp-artifact-load-or-compile-source-file))
	            (nelisp-artifact-load-or-compile-source-file
	             path '(neln nelc)
	             nelisp-load-auto-compile-kind
	             nelisp-load-auto-compile-target
	             nelisp-load-auto-compile-load-paths
	             nelisp-load-auto-compile-preloads
	             nelisp-load-auto-compile-native-policy)
          (nelisp-artifact-load-source-file path '(neln nelc)))))))

(defun nelisp-locate-file (feature)
  "Resolve FEATURE to an absolute file path or nil.
FEATURE is a symbol or string.  The search walks `nelisp-load-path'
first, then host `load-path' when `nelisp-load-path-include-host'
is non-nil.  Within each directory, the `.el' suffix is tried
first; if FEATURE already ends with `.el', that exact name is used.

Doc 141 Stage 2: file resolution is routed through
`nelisp-core-expand-file-name' / `nelisp-core-file-readable-p' so
the loader no longer depends on the Emacs compatibility layer."
  (let* ((name (cond ((symbolp feature) (symbol-name feature))
                     ((stringp feature) feature)
                     (t (signal 'wrong-type-argument
                                (list 'nelisp-feature-type feature)))))
         (has-ext (let ((n (length name)))
                    (and (> n 3)
                         (equal (substring name (- n 3)) ".el"))))
         (candidates (if has-ext (list name) (list (concat name ".el"))))
         (dirs (append nelisp-load-path
                       (when nelisp-load-path-include-host load-path)))
         (found nil))
    (while (and dirs (not found))
      (let ((tail candidates)
            (dir (car dirs)))
        (while (and tail (not found))
          (let ((full (nelisp-core-expand-file-name (car tail) dir)))
            (when (nelisp-core-file-readable-p full)
              (setq found full)))
          (setq tail (cdr tail))))
      (setq dirs (cdr dirs)))
    found))

(defun nelisp-load--pos-to-line-col (str pos)
  "Return (LINE . COLUMN) for POS in STR, 1-indexed.
Used by `nelisp-load-string' / `nelisp-load-file' to annotate
read / eval failures with source positions."
  (let ((line 1)
        (line-start 0)
        (i 0))
    (while (< i pos)
      (when (eq (aref str i) ?\n)
        (setq line (1+ line)
              line-start (1+ i)))
      (setq i (1+ i)))
    (cons line (1+ (- pos line-start)))))

(defun nelisp-load--signal (source pos str form-index phase cause)
  "Re-signal CAUSE as `nelisp-load-error' with position + phase info.
STR is the original source string, POS is where the failing form
starts inside STR, SOURCE is the file path (or nil when loading a
bare string), FORM-INDEX is the 0-based sexp position within the
load, PHASE is `read' or `eval'."
  (let ((lc (nelisp-load--pos-to-line-col str pos)))
    (signal 'nelisp-load-error
            (list :source source
                  :form-index form-index
                  :line (car lc)
                  :column (cdr lc)
                  :phase phase
                  :cause cause))))

(defun nelisp-load--gnu-elc-p (contents)
  "Return non-nil when CONTENTS begins with GNU Emacs's ELC marker."
  (and (stringp contents)
       (>= (length contents) 5)
       (= (aref contents 0) ?\;)
       (= (aref contents 1) ?E)
       (= (aref contents 2) ?L)
       (= (aref contents 3) ?C)
       (= (aref contents 4) 31)))

(defun nelisp-load-gnu-elc-p (contents)
  "Return non-nil when CONTENTS has the supported GNU ELC marker."
  (nelisp-load--gnu-elc-p contents))

(defun nelisp-load--gnu-elc-skip-comments (contents pos)
  "Skip whitespace and line comments in CONTENTS starting at POS."
  (let ((len (length contents)) done)
    (while (not done)
      (while (and (< pos len)
                  (memq (aref contents pos) '(?\s ?\t ?\n ?\r ?\f)))
        (setq pos (1+ pos)))
      (if (and (< pos len) (= (aref contents pos) ?\;))
          (progn
            (while (and (< pos len) (/= (aref contents pos) ?\n))
              (setq pos (1+ pos)))
            (when (< pos len) (setq pos (1+ pos))))
        (setq done t)))
    pos))

(defun nelisp-load--read-gnu-elc (contents source-file)
  "Read all forms from GNU ELC CONTENTS before allowing evaluation.
SOURCE-FILE is attached to any read error.  Parsing the complete file
first ensures a truncated final form cannot leave earlier side effects."
  (unless (functionp nelisp-load--host-read-from-string)
    (signal 'nelisp-load-error (list :phase 'read :cause 'host-reader-unavailable)))
  ;; The embedded NeLisp reader accepts an unterminated final list by
  ;; inserting its closing delimiter.  GNU's reader instead rejects it.
  ;; Check structural completeness in the host syntax parser before the
  ;; permissive reader can turn a truncated file into a complete form.
  (unless (functionp nelisp-load--host-check-parens)
    (signal 'nelisp-load-error (list :phase 'read :cause 'host-paren-checker-unavailable)))
  (let ((original (current-buffer))
        (buffer (generate-new-buffer " *nelisp-gnu-elc-check*")))
    (unwind-protect
        (progn
          (set-buffer buffer)
          (insert contents)
          (condition-case err
              (funcall nelisp-load--host-check-parens)
            (error
             (nelisp-load--signal source-file 0 contents 0 'read err))))
      (set-buffer original)
      (kill-buffer buffer)))
  (let ((pos 0) (index 0) (len (length contents)) forms)
    (while (< (setq pos (nelisp-load--gnu-elc-skip-comments contents pos)) len)
      (let ((start pos)
            parsed)
        (condition-case err
            (setq parsed (funcall nelisp-load--host-read-from-string
                                  contents pos))
          (error
           (nelisp-load--signal source-file start contents index 'read err)))
        (unless (and (consp parsed) (> (cdr parsed) start))
          (nelisp-load--signal source-file start contents index 'read
                               '(invalid-reader-progress)))
        (push (cons start (car parsed)) forms)
        (setq pos (cdr parsed)
              index (1+ index))))
    (nreverse forms)))

(defun nelisp-load--eval-gnu-elc (contents source-file)
  "Evaluate GNU byte-compiled CONTENTS in the host namespace."
  (unless (functionp nelisp-load--host-eval)
    (signal 'nelisp-load-error (list :phase 'eval :cause 'host-evaluator-unavailable)))
  (let* ((true-file (file-truename source-file))
         (load-file-name true-file)
         (nelisp-load--current-file true-file)
         (nelisp-load--current-features nil)
         (forms (nelisp-load--read-gnu-elc contents source-file))
         (index 0) (last nil))
    (dolist (record forms)
      (let ((pos (car record))
            (form (cdr record)))
        (condition-case err
            (setq last (funcall nelisp-load--host-eval form))
          (error
           (nelisp-load--signal source-file pos contents index 'eval err)))
        ;; Keep load-history/after-load bookkeeping in step with ordinary
        ;; source loading for direct top-level `provide' forms.
        (when (and (consp form) (eq (car form) 'provide)
                   (consp (cdr form)) (consp (cadr form))
                   (eq (caadr form) 'quote) (symbolp (cadadr form)))
          (push (cadadr form) nelisp-load--current-features))
        (setq index (1+ index))))
    (nelisp-load--source-complete true-file nelisp-load--current-features)
    last))

(defun nelisp-load-gnu-elc-host (path)
  "Diagnostic opt-in: load GNU ELC PATH using GNU's host evaluator.
The effects and function cells belong to the host namespace and are
not mirrored into NeLisp's `nelisp--globals' or `nelisp--functions'.
Ordinary `nelisp-load-file' deliberately refuses GNU ELC until the
runtime has a shared byte-code execution environment."
  (unless (nelisp-core-file-readable-p path)
    (signal 'file-error (list "Cannot read GNU ELC file" path)))
  (let ((contents (nelisp-core-read-file-as-string path)))
    (unless (nelisp-load--gnu-elc-p contents)
      (signal 'nelisp-load-error (list :source path :phase 'read
                                       :cause 'not-gnu-elc)))
    (nelisp-load--eval-gnu-elc contents path)))

;;;###autoload
(defun nelisp-load--eval-string (str source-file)
  "Read and evaluate STR, attaching SOURCE-FILE to read/eval errors."
  (let ((pos 0)
        (len (length str))
        (last nil)
        (form-index 0))
    (while (progn
             (setq pos (nelisp-read--skip-ws str pos))
             (< pos len))
      (let ((form-start pos)
            form)
        (condition-case err
            (let ((res (nelisp-read--sexp str pos)))
              (setq form (car res)
                    pos (cdr res)))
          (nelisp-read-error
           (nelisp-load--signal source-file form-start str
                                form-index 'read err)))
        (condition-case err
            (setq last (nelisp-eval form))
          (error
           (nelisp-load--signal source-file form-start str
                                form-index 'eval err)))
        (setq form-index (1+ form-index))))
    last))

(defun nelisp-load-string (str &optional source-file)
  "Read every sexp in STR and evaluate them in order.
Return the value of the last form, or nil if STR contained none.
Defuns / defvars / defmacros installed during loading persist in
the global NeLisp tables exactly as if the user had typed each
form interactively.

Errors during read or eval are re-signaled as `nelisp-load-error'
carrying a plist with keys :source, :form-index, :line, :column,
:phase (either `read' or `eval'), and :cause (the original signal
data).  Forms successfully evaluated before the failure keep their
side-effects — load is not transactional.  Optional SOURCE-FILE is
attached to the signal.  For a successful source load it also drives
GNU Emacs's `do-after-load-evaluation' and `after-load-functions'."
  (unless (stringp str)
    (signal 'wrong-type-argument (list 'stringp str)))
  (if (null source-file)
      (nelisp-load--eval-string str nil)
    (let* ((true-file (file-truename source-file))
           (load-file-name true-file)
           (nelisp-load--current-file true-file)
           (nelisp-load--current-features nil)
           (value (nelisp-load--eval-string str source-file)))
      ;; Callback errors propagate after source evaluation, as they do in
      ;; GNU Emacs's load path after the file has entered load-history.
      (nelisp-load--source-complete true-file nelisp-load--current-features)
      value)))

;;;###autoload
(defun nelisp-load (str &optional source-file)
  "Doc 12 §2.1 B surface — thin wrapper around `nelisp-load-string'.
Kept as a `defun' rather than `defalias' so that NeLisp self-host
evaluation (which walks this file through its own evaluator) can
install this symbol without needing a `defalias' primitive.

See `nelisp-load-string' for the full contract; prefer
`nelisp-load-file' for disk sources and `nelisp-require' for
feature lookups."
  (nelisp-load-string str source-file))

;;;###autoload
(defun nelisp-load-file (path)
  "Read the file at PATH and evaluate every top-level sexp in order.
Return the value of the last form.  PATH must already exist; no
load-path search is performed at this layer (see `nelisp-require'
+ `nelisp-load-path' for feature resolution).

Read / eval failures are propagated as `nelisp-load-error' with
PATH recorded in the :source plist slot.

Doc 141 Stage 2: disk read goes through `nelisp-core-fileio', so
the loader no longer depends on Emacs compatibility buffers or
editor-style file APIs.  UTF-8 decoding is handled by
`nelisp-coding' inside `nelisp-core-read-file-as-string'."
  ;; Never silently execute GNU ELC in the host's distinct variable and
  ;; function namespace.  Reject this exact artifact dialect before an
  ;; adjacent generated artifact or any top-level form can run.
  (when (and (stringp path)
             (>= (length path) 4)
             (equal (substring path (- (length path) 4)) ".elc")
             (nelisp-core-file-readable-p path))
    (let ((contents (nelisp-core-read-file-as-string path)))
      (when (nelisp-load--gnu-elc-p contents)
        (nelisp-load--signal path 0 contents 0 'read '(unsupported-gnu-elc)))))
  (let ((artifact (nelisp-load--try-artifact path)))
    (if artifact
        (plist-get artifact :value)
      (unless (nelisp-core-file-readable-p path)
        (signal 'file-error (list "Cannot read NeLisp file" path)))
      (nelisp-load-string (nelisp-core-read-file-as-string path) path))))

;;; Feature registry (Doc 12 §3.3) -----------------------------------

(defun nelisp-load--run-after-load-form (form)
  "Evaluate FORM as the callback accepted by GNU `eval-after-load'."
  (cond
   ((or (nelisp--closure-p form)
        (nelisp--native-function-p form)
        (and (symbolp form)
             (or (gethash form nelisp--functions)
                 (fboundp form)))
        (and (functionp form)
             (not (and (consp form) (eq (car form) 'lambda)))))
    (nelisp--apply form nil))
   ((and (consp form) (eq (car form) 'lambda))
    (nelisp--apply (nelisp-eval form) nil))
   (t
    (nelisp-eval form))))

(defun nelisp--builtin-eval-after-load (selector form)
  "Delegate NeLisp callback registration to GNU Emacs `eval-after-load'."
  (unless (functionp nelisp-load--host-eval-after-load)
    (signal 'nelisp-load-error
            (list :phase 'eval-after-load :cause 'host-function-unavailable)))
  (funcall nelisp-load--host-eval-after-load selector
           (lambda () (nelisp-load--run-after-load-form form))))

(defun nelisp-load--source-complete (true-file features)
  "Record successful TRUE-FILE in `load-history' and notify GNU Emacs."
  (unless (functionp nelisp-load--host-after-load-evaluation)
    (signal 'nelisp-load-error
            (list :phase 'load :source true-file
                  :cause 'host-after-load-unavailable)))
  (let ((entry (assoc true-file load-history)))
    (unless entry
      (setq entry (list true-file))
      (push entry load-history))
    (dolist (feature features)
      (let ((provided (cons 'provide feature)))
        (unless (member provided (cdr entry))
          (setcdr entry (append (cdr entry) (list provided)))))))
  (funcall nelisp-load--host-after-load-evaluation true-file))

(defvar nelisp--features nil
  "List of symbols `nelisp-provide' has registered this session.")

(defvar nelisp--loading nil
  "Stack of feature symbols whose load is currently in progress.
Used by `nelisp--builtin-require' for set-based circular detection
per Doc 12 §2.3 A.")

(defun nelisp-load--runtime-features ()
  "Return the NeLisp runtime's user-visible `features' list, if bound."
  (when (and (boundp 'nelisp--globals)
             (hash-table-p nelisp--globals))
    (let ((value (gethash 'features nelisp--globals nelisp--unbound)))
      (unless (eq value nelisp--unbound)
        value))))

(defun nelisp-load--register-feature (feature)
  "Register FEATURE in both load and runtime feature registries."
  (unless (memq feature nelisp--features)
    (setq nelisp--features (cons feature nelisp--features)))
  (when (and (boundp 'nelisp--globals)
             (hash-table-p nelisp--globals))
    (let ((runtime-features (nelisp-load--runtime-features)))
      (unless (memq feature runtime-features)
        (puthash 'features
                 (cons feature runtime-features)
                 nelisp--globals))))
  feature)

(defun nelisp-load--feature-provided-p (feature)
  "Return non-nil when FEATURE is provided in any NeLisp feature registry."
  (or (memq feature nelisp--features)
      (memq feature (nelisp-load--runtime-features))))

(defun nelisp-load--reset-registry ()
  "Clear NeLisp feature / loading registers.
Invoked from `nelisp--reset' via `fboundp' guard; callable directly
in tests that want to isolate require state."
  (setq nelisp--features nil
        nelisp--loading nil)
  (when (hash-table-p nelisp--functions)
    (puthash 'eval-after-load #'nelisp--builtin-eval-after-load
             nelisp--functions)))

(defun nelisp--builtin-require (feature &optional filename noerror)
  "Phase 5-A.3 NeLisp `require'.
Load FEATURE (a symbol) unless already provided.  Circular loads,
i.e. FEATURE re-entered while its own load is in progress, signal
`nelisp-load-error' with :cause = `circular-require'.  When the
file is missing and NOERROR is non-nil, return nil; otherwise
`file-error' is signaled.  When the located file does not end with
a matching `nelisp-provide', `nelisp-load-error' with :cause =
`did-not-provide' is signaled — this catches typos in feature
names."
  (unless (symbolp feature)
    (signal 'wrong-type-argument (list 'symbolp feature)))
  (cond
   ((nelisp-load--feature-provided-p feature)
    (nelisp-load--register-feature feature))
   ((memq feature nelisp--loading)
    (signal 'nelisp-load-error
            (list :phase 'require
                  :feature feature
                  :loading (copy-sequence nelisp--loading)
                  :cause 'circular-require)))
   ;; Host parity with Phase 2 stub: when the host Emacs already has
   ;; FEATURE (e.g. during self-host where `require' runs in a NeLisp
   ;; file that the host itself loaded), acknowledge it and record the
   ;; symbol in `nelisp--features' so subsequent NeLisp requires short
   ;; circuit too.  This keeps NeLisp source files loadable through
   ;; both host `load' and `nelisp-load-file' without divergence.
   ((and (null filename) (featurep feature))
    (nelisp-load--register-feature feature))
   (t
    (let ((path (or filename (nelisp-locate-file feature))))
      (cond
       ((null path)
        (if noerror nil
          (signal 'file-error
                  (list "Cannot find NeLisp feature" feature))))
       (t
        (let ((nelisp--loading (cons feature nelisp--loading)))
          (nelisp-load-file path))
        (unless (nelisp-load--feature-provided-p feature)
          (signal 'nelisp-load-error
                  (list :phase 'require
                        :feature feature
                        :source path
                        :cause 'did-not-provide)))
        (nelisp-load--register-feature feature)))))))

(defun nelisp--builtin-provide (feature &optional subfeatures)
  "Phase 5-A.3 NeLisp `provide'.
Register FEATURE as available for `nelisp-require'.  SUBFEATURES
is forwarded to GNU `provide' when the host bridge is available."
  (unless (symbolp feature)
    (signal 'wrong-type-argument (list 'symbolp feature)))
  (nelisp-load--register-feature feature)
  (when (and nelisp-load--current-file
             (not (memq feature nelisp-load--current-features)))
    (setq nelisp-load--current-features
          (append nelisp-load--current-features (list feature))))
  ;; GNU's C `provide' is the authority for its after-load-alist feature
  ;; entries.  Keep the host feature registry in step while source is loaded.
  (when (functionp nelisp-load--host-provide)
    (if subfeatures
        (funcall nelisp-load--host-provide feature subfeatures)
      (funcall nelisp-load--host-provide feature))))

;;;###autoload
(defun nelisp-require (feature &optional filename noerror)
  "Host-facing alias of NeLisp `require' — Doc 12 §2.1 B surface."
  (nelisp--builtin-require feature filename noerror))

;;;###autoload
(defun nelisp-provide (feature &optional subfeatures)
  "Host-facing alias of NeLisp `provide'."
  (nelisp--builtin-provide feature subfeatures))

;;;###autoload
(defun nelisp--builtin-load-file (path)
  "Phase 6 NeLisp `load-file' — read PATH and eval each top-level sexp.
Pure elisp loader (= goes through `nelisp-read--sexp' so all reader
extensions, incl. chord-modifier char literals, apply).  Signals
`file-error' when PATH is unreadable."
  (nelisp-load-file path))

;;;###autoload
(defun nelisp--builtin-load (path &optional _noerror _nomessage _nosuffix _must-suffix)
  "Phase 6 NeLisp `load'.  PATH is treated as an absolute file path
when the optional suffix-search args are nil (= the only mode the
NeLisp driver currently exercises — load-path resolution lives in
`nelisp-require').  Other arg slots accept the standard Emacs
arguments for shape parity but are ignored at this layer."
  (nelisp-load-file path))

;;; Refresh the primitive dispatch table so the Phase 5-A.3 bodies
;;; are live before any test calls `nelisp--reset' (which would
;;; otherwise pick up the stubs defined in `nelisp-eval.el').
(when (hash-table-p nelisp--functions)
  (puthash 'require   #'nelisp--builtin-require   nelisp--functions)
  (puthash 'provide   #'nelisp--builtin-provide   nelisp--functions)
  (puthash 'eval-after-load #'nelisp--builtin-eval-after-load
           nelisp--functions)
  (puthash 'load-file #'nelisp--builtin-load-file nelisp--functions)
  (puthash 'load      #'nelisp--builtin-load      nelisp--functions))

(provide 'nelisp-load)

;;; nelisp-load.el ends here
