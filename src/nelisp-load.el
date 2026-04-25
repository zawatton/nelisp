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
;; T86 (Phase 7+ Wave 2 F): route file I/O through `nelisp-emacs-compat'
;; (T78 SHIPPED) so NeLisp standalone path can load .el files without
;; depending on host Emacs `with-temp-buffer' / `insert-file-contents'
;; / `file-readable-p' / `expand-file-name'.  T76 (Rust syscall) is the
;; eventual hard backend; today the `nelisp-ec-*' wrappers fall back to
;; host primitives so behaviour is preserved when running under host
;; Emacs.  This severs nelisp-load.el → host file I/O — the
;; chicken-and-egg blocker for anvil.el standalone mode.
(require 'nelisp-emacs-compat)
(require 'nelisp-emacs-compat-fileio)

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

(defun nelisp-locate-file (feature)
  "Resolve FEATURE to an absolute file path or nil.
FEATURE is a symbol or string.  The search walks `nelisp-load-path'
first, then host `load-path' when `nelisp-load-path-include-host'
is non-nil.  Within each directory, the `.el' suffix is tried
first; if FEATURE already ends with `.el', that exact name is used.

T86: file I/O routed through `nelisp-ec-expand-file-name' /
`nelisp-ec-file-readable-p' so NeLisp standalone path resolves
features without host Emacs file primitives."
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
          (let ((full (nelisp-ec-expand-file-name (car tail) dir)))
            (when (nelisp-ec-file-readable-p full)
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

;;;###autoload
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
attached to the signal so callers like `nelisp-load-file' can tell
users where the failure lives."
  (unless (stringp str)
    (signal 'wrong-type-argument (list 'stringp str)))
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

T86: disk read goes through `nelisp-ec-*' (T78 SHIPPED) — host
`with-temp-buffer' / `insert-file-contents' / `file-readable-p'
are no longer reached, so NeLisp standalone path can load .el
files without the host Emacs file I/O surface.  UTF-8 decoding is
handled by `nelisp-coding' inside `nelisp-ec-insert-file-contents'."
  (unless (nelisp-ec-file-readable-p path)
    (signal 'file-error (list "Cannot read NeLisp file" path)))
  (let ((buf (nelisp-ec-generate-new-buffer " *nelisp-load*")))
    (unwind-protect
        (nelisp-ec-with-current-buffer buf
          (nelisp-ec-insert-file-contents path)
          (nelisp-load-string (nelisp-ec-buffer-string) path))
      (nelisp-ec-kill-buffer buf))))

;;; Feature registry (Doc 12 §3.3) -----------------------------------

(defvar nelisp--features nil
  "List of symbols `nelisp-provide' has registered this session.")

(defvar nelisp--loading nil
  "Stack of feature symbols whose load is currently in progress.
Used by `nelisp--builtin-require' for set-based circular detection
per Doc 12 §2.3 A.")

(defun nelisp-load--reset-registry ()
  "Clear NeLisp feature / loading registers.
Invoked from `nelisp--reset' via `fboundp' guard; callable directly
in tests that want to isolate require state."
  (setq nelisp--features nil
        nelisp--loading nil))

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
   ((memq feature nelisp--features) feature)
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
    (push feature nelisp--features)
    feature)
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
        (unless (memq feature nelisp--features)
          (signal 'nelisp-load-error
                  (list :phase 'require
                        :feature feature
                        :source path
                        :cause 'did-not-provide)))
        feature))))))

(defun nelisp--builtin-provide (feature &optional _subfeatures)
  "Phase 5-A.3 NeLisp `provide'.
Register FEATURE as available for `nelisp-require'.  SUBFEATURES
is accepted for host-parity but not yet honoured."
  (unless (symbolp feature)
    (signal 'wrong-type-argument (list 'symbolp feature)))
  (unless (memq feature nelisp--features)
    (setq nelisp--features (cons feature nelisp--features)))
  feature)

;;;###autoload
(defun nelisp-require (feature &optional filename noerror)
  "Host-facing alias of NeLisp `require' — Doc 12 §2.1 B surface."
  (nelisp--builtin-require feature filename noerror))

;;;###autoload
(defun nelisp-provide (feature &optional subfeatures)
  "Host-facing alias of NeLisp `provide'."
  (nelisp--builtin-provide feature subfeatures))

;;; Refresh the primitive dispatch table so the Phase 5-A.3 bodies
;;; are live before any test calls `nelisp--reset' (which would
;;; otherwise pick up the stubs defined in `nelisp-eval.el').
(when (hash-table-p nelisp--functions)
  (puthash 'require #'nelisp--builtin-require nelisp--functions)
  (puthash 'provide #'nelisp--builtin-provide nelisp--functions))

(provide 'nelisp-load)

;;; nelisp-load.el ends here
