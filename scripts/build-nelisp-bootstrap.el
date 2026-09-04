;;; build-nelisp-bootstrap.el --- generate standalone bootstrap bundle  -*- lexical-binding: t; -*-

;;; Commentary:

;; Build helper for the NeLisp driver cold-start path.
;;
;; NeLisp v2 can run the runtime mostly as Elisp, but cold boot still
;; pays for many small source loads.  This script asks host Emacs to
;; load `nemacs-main', reads the resulting local `load-history', and
;; concatenates the participating src/*.el files in dependency order.
;; The generated file is still plain Elisp; it is a preload bundle, not
;; a bytecode/pdump replacement.

;;; Code:

(require 'cl-lib)
(require 'standalone-source-normalize)

(defvar nelisp-bootstrap-output-file
  (expand-file-name "build/nemacs-bootstrap.el"
                    (expand-file-name ".." (file-name-directory
                                            (or load-file-name
                                                buffer-file-name))))
  "Output path for the generated NeLisp bootstrap bundle.")

(defvar nelisp-bootstrap-repl-output-file nil
  "Output path for the generated NeLisp REPL bootstrap input.
When nil, derive it from `nelisp-bootstrap-output-file' by replacing the
final `.el' suffix with `.repl'.")

(defvar nelisp-bootstrap-repo-root
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name)))
  "Repository root used by the bundle generator.")

(defvar nelisp-bootstrap-extra-files
  '("emacs-parity-core-vars.el"
    "cl-lib.el"
    ;; Standalone runtime-speed and abbrev-table repairs.  Their prerequisites
    ;; are available by the `emacs-cl-macros.el' insertion anchor.
    "emacs-parity-addtolist.el"
    "emacs-parity-abbrev.el"
    "seq.el"
    "map.el"
    "json.el"
    "range.el"
    "let-alist.el"
    "thunk.el"
    "emacs-network-ffi.el"
    "emacs-server-client-polyfills.el"
    "generator.el"
    ;; Macro parity has to precede generator.el during standalone replay.
    ;; `nelisp-bootstrap--hoist-standalone-definers' performs that targeted
    ;; move after this completion list has made the files bundle members.
    "emacs-parity-setf-places.el"
    ;; General macro expansion must precede the macro consumers below.
    "emacs-parity-macroexpand.el"
    ;; Preserve the audit-tested clloop -> eieio -> evil sequence: eieio's
    ;; class bootstrap registrations expect clloop immediately before it,
    ;; while evil consumes the corrected destructuring helper afterward.
    "emacs-parity-clloop.el"
    "emacs-parity-eieio.el"
    "emacs-parity-evil.el"
    "emacs-parity-macros2.el"
    "emacs-parity-flycheck.el"
    "emacs-parity-cc.el"
    "rx.el"
    ;; Re-evaluate the corrected pcase-based translator only after rx helpers.
    "emacs-parity-rx.el"
    "emacs-tui-backend.el"
    "emacs-redisplay-core.el"
    "emacs-tui-event.el")
  "Local src files that host Emacs may not load but standalone NeLisp needs.")

(defvar nelisp-bootstrap-late-extra-files
  '("lisp.el"
    "emacs-fileio.el"
    "case-table.el"
    "emacs-process-events.el"
    "regi.el"
    "files-standalone-buffer.el"
    "emacs-syntax-table.el"
    "emacs-font-lock.el"
    "emacs-font-lock-builtins.el"
    ;; dev daily-driver surfaces: symbol index + jump-to-definition +
    ;; compile/grep + next-error.  Greenfield implementations first, then
    ;; the facade loaders that install the standard command names on the
    ;; standalone reader.
    "emacs-imenu.el"
    "emacs-xref.el"
    "imenu.el"
    "xref.el"
    "emacs-compile.el"
    "compile.el"
    "emacs-vc.el"
    "vc.el"
    "emacs-comint.el"
    "comint.el"
    "emacs-replace.el"
    "replace.el"
    "emacs-isearch.el"
    "isearch.el"
    "emacs-ielm.el"
    "ielm.el"
    "emacs-project.el"
    "project.el"
    "emacs-shell.el"
    "shell.el"
    "emacs-eshell.el"
    "eshell.el"
    "emacs-man.el"
    "man.el"
    "woman.el"
    "emacs-calc.el"
    "calc.el"
    ;; directory browser: greenfield `emacs-dired-min' (defines the `dired'
    ;; command on top of nelisp-ec-directory-files / -file-attributes) then
    ;; the `dired' feature facade.  Wired once the standalone reader's
    ;; readdir/stat syscalls return real entries (Doc 142 gate-5).
    "emacs-dired-min.el"
    "dired.el"
    ;; Reusable Emacs parity owners needed by vendor libraries.  Keep the
    ;; actual modules in the bootstrap rather than duplicating their behavior
    ;; as preload-local shims: Org's chain reaches replace-buffer-contents,
    ;; pcomplete-uniqify-list, newline-and-indent, and define-skeleton during
    ;; source loading.
    "emacs-parity-shims.el"
    "emacs-parity-misc.el"
    "emacs-parity-fns2.el"
    "emacs-parity-org.el"
    ;; Stock variable fills that no other wired file provides.  Both are
    ;; purely additive -- every top-level form is
    ;; `(unless (boundp 'X) (defvar X ...))' -- so a real preload still wins.
    ;; They were written for the `nemacs-next-session.el' loader, which no
    ;; longer exists, so the runtime-image path never got them.  That left
    ;; `lisp-imenu-generic-expression' void, which aborts the Magit bundle at
    ;; transient.el's top-level `cl-pushnew' onto it.  Keep vars2 ahead of
    ;; vars3: vars3 fills only what shims/vars2/core-vars did not.
    "emacs-parity-vars2.el"
    "emacs-parity-vars3.el"
    ;; Runtime primitive repairs belong late: their dependencies have loaded,
    ;; while package/user-init consumers have not run yet.
    "emacs-parity-regex-charclass.el"
    "emacs-parity-makunbound.el"
    "emacs-parity-clmacros.el"
    "emacs-parity-skk.el"
    "emacs-parity-subdirs.el")
  "Local src files inserted after buffer/face substrates are available.")

(defvar nelisp-bootstrap-vendor-extra-files
  '("emacs-lisp/emacs-lisp/ring.el"
    "emacs-lisp/org/org-version.el")
  "Vendor files injected into the bootstrap bundle as real sources.
These are existing vendor implementations, not local reimplementations.")

(defvar nelisp-bootstrap-vendor-tail-extra-files
  '("emacs-lisp/emacs-lisp/advice.el")
  "Vendor files appended as the absolute tail of bootstrap replay.
Use this for vendor sources whose dependencies are only guaranteed after the
self-healing replay phase has completed.")

(defvar nelisp-bootstrap-tail-extra-files
  '("emacs-load.el")
  "Local src files appended as the absolute tail of bootstrap replay.
These files must not run during the self-healing replay phase itself.")

(defconst nelisp-bootstrap-vendor-load-path-subdirs
  '("vendor/emacs-lisp"
    "vendor/emacs-lisp/emacs-lisp"
    "vendor/emacs-lisp/international"
    "vendor/emacs-lisp/textmodes"
    "vendor/emacs-lisp/progmodes"
    "vendor/emacs-lisp/net"
    "vendor/emacs-lisp/url"
    "vendor/emacs-lisp/vc"
    "vendor/emacs-lisp/calc"
    "vendor/emacs-lisp/calendar"
    "vendor/emacs-lisp/eshell"
    "vendor/emacs-lisp/mail"
    "vendor/emacs-lisp/cedet"
    "vendor/emacs-lisp/leim"
    "vendor/emacs-lisp/term"
    "vendor/emacs-lisp/erc"
    "vendor/emacs-lisp/org"
    "vendor/emacs-lisp/gnus")
  "Vendor directories that should be present on standalone `load-path'.")

(defvar nelisp-bootstrap-repl-direct-character-limit 1000000
  "Minimum printed form size emitted directly in diagnostic nested mode.

Large forms are already normalized before this stage.  Emitting them as direct
REPL forms avoids an extra nested source-string read in the persistent
standalone evaluator while preserving the same evaluated form.  This threshold
is consulted only when `nelisp-bootstrap-repl-nested-eval-source' is non-nil.")

(defvar nelisp-bootstrap-repl-nested-eval-source nil
  "Non-nil enables nested source-string transport for diagnostics.

Normal bootstrap generation emits every normalized form directly as
`(progn FORM nil)'.  Enabling this variable restores the historical diagnostic
split: special and large forms remain direct, while ordinary small forms use
`nelisp--eval-source-string'.")

(defun nelisp-bootstrap--src-dir ()
  "Return the absolute src directory."
  (file-name-as-directory
   (expand-file-name "src" nelisp-bootstrap-repo-root)))

(defun nelisp-bootstrap--vendor-dir ()
  "Return the absolute vendor directory."
  (file-name-as-directory
   (expand-file-name "vendor" nelisp-bootstrap-repo-root)))

(defun nelisp-bootstrap--source-file (file)
  "Return local src source file for FILE, or nil.
FILE may be either the source `.el' path or the byte-compiled `.elc'
path recorded in `load-history'."
  (let ((src (nelisp-bootstrap--src-dir))
        (abs (and (stringp file) (expand-file-name file))))
    (and abs
         (string-prefix-p src abs)
         (cond
          ((and (string-suffix-p ".el" abs)
                (file-readable-p abs))
           abs)
          ((string-suffix-p ".elc" abs)
           (let ((source (substring abs 0 -1)))
             (and (file-readable-p source) source)))))))

(defun nelisp-bootstrap--vendor-source-file (name)
  "Return absolute vendor source file for relative vendor NAME, or nil."
  (let ((file (expand-file-name name (nelisp-bootstrap--vendor-dir))))
    (and (file-readable-p file) file)))

(defun nelisp-bootstrap--collect-loaded-src-files ()
  "Return loaded local src files in dependency-first order."
  (let (files)
    (dolist (entry load-history)
      (let ((file (car-safe entry)))
        (let ((source (nelisp-bootstrap--source-file file)))
          (when source
            (push (expand-file-name source) files)))))
    (delete-dups files)))

(defun nelisp-bootstrap--insert-after (file anchor files)
  "Insert FILE after ANCHOR in FILES, unless FILE is already present."
  (let ((file (expand-file-name file))
        (anchor (expand-file-name anchor)))
    (cond
     ((member file files) files)
     ((not (member anchor files)) (cons file files))
     (t
      (let (out rest done)
        (setq rest files)
        (while rest
          (push (car rest) out)
          (when (equal (car rest) anchor)
            (push file out)
            (setq done t))
          (setq rest (cdr rest)))
        (unless done
          (push file out))
        (nreverse out))))))

(defun nelisp-bootstrap--insert-before (file anchor files)
  "Insert FILE before ANCHOR in FILES, unless FILE is already present."
  (let ((file (expand-file-name file))
        (anchor (expand-file-name anchor)))
    (cond
     ((member file files) files)
     ((not (member anchor files)) (append files (list file)))
     (t
      (let (out rest done)
        (setq rest files)
        (while rest
          (when (and (not done)
                     (equal (car rest) anchor))
            (push file out)
            (setq done t))
          (push (car rest) out)
          (setq rest (cdr rest)))
        (unless done
          (push file out))
        (nreverse out))))))

(defun nelisp-bootstrap--complete-file-list (files)
  "Add standalone-only source files to FILES in a dependency-safe spot."
  (let ((src (nelisp-bootstrap--src-dir))
        (out files)
        (anchor (expand-file-name "emacs-cl-macros.el"
                                  (nelisp-bootstrap--src-dir))))
    (dolist (name nelisp-bootstrap-extra-files)
      (let ((file (expand-file-name name src)))
        (when (file-readable-p file)
          (setq out (nelisp-bootstrap--insert-after file anchor out))
          (setq anchor file))))
    (setq anchor (expand-file-name "emacs-faces-builtins.el" src))
    (unless (member anchor out)
      (setq anchor (expand-file-name "emacs-faces.el" src)))
    (dolist (name nelisp-bootstrap-late-extra-files)
      (let ((file (expand-file-name name src)))
        (when (file-readable-p file)
          (setq out (nelisp-bootstrap--insert-after file anchor out))
          (setq anchor file))))
    ;; Keep standalone bootstrap providers ahead of the consumers that
    ;; still load them at top level.
    (let ((redisplay-core (expand-file-name "emacs-redisplay-core.el" src))
          (window (expand-file-name "emacs-window.el" src)))
      (when (and (member redisplay-core out)
                 (member window out))
        (setq out (nelisp-bootstrap--insert-before
                   window redisplay-core (delete window out)))))
    (let ((compat (expand-file-name "nelisp-emacs-compat.el" src))
          (window (expand-file-name "emacs-window.el" src)))
      (when (and (member compat out)
                 (member window out))
        (setq out (nelisp-bootstrap--insert-before
                   compat window (delete compat out)))))
    (let ((compat (expand-file-name "nelisp-emacs-compat.el" src))
          (regex (expand-file-name "nelisp-regex.el" src)))
      (when (and (member compat out)
                 (member regex out))
        (setq out (nelisp-bootstrap--insert-before
                   regex compat (delete regex out)))))
    (let ((compat (expand-file-name "nelisp-emacs-compat.el" src))
          (text-buffer (expand-file-name "nelisp-text-buffer.el" src)))
      (when (and (member compat out)
                 (member text-buffer out))
        (setq out (nelisp-bootstrap--insert-before
                   text-buffer compat (delete text-buffer out)))))
    (let ((fileio (expand-file-name "emacs-fileio.el" src))
          (fileio-gui (expand-file-name "emacs-fileio-gui.el" src))
          (mode-builtins (expand-file-name "emacs-mode-builtins.el" src)))
      (when (and (member fileio out)
                 (member fileio-gui out))
        (setq out (nelisp-bootstrap--insert-before
                   fileio-gui fileio (delete fileio-gui out))))
      (when (and (member fileio out)
                 (member mode-builtins out))
        (setq out (nelisp-bootstrap--insert-before
                   mode-builtins fileio (delete mode-builtins out)))))
    (let ((mode (expand-file-name "emacs-mode.el" src))
          (mode-builtins (expand-file-name "emacs-mode-builtins.el" src)))
      (when (and (member mode out)
                 (member mode-builtins out))
        (setq out (nelisp-bootstrap--insert-before
                   mode mode-builtins (delete mode out)))))
    ;; Systemic fix (Doc 22 A19): load emacs-stub-bulk LAST so its bulk
    ;; no-op stubs only fill names still void after every real
    ;; implementation has loaded.  Loaded early, the stubs shadow real
    ;; impls gated with `unless (fboundp ...)' (e.g. mapcan / regexp-opt).
    (let ((bulk (expand-file-name "emacs-stub-bulk.el" src)))
      (when (member bulk out)
        (setq out (append (delete bulk out) (list bulk)))))
    (setq anchor (expand-file-name "emacs-mode.el" src))
    (dolist (name nelisp-bootstrap-vendor-extra-files)
      (let ((file (nelisp-bootstrap--vendor-source-file name)))
        (unless file
          (error "Missing readable bootstrap vendor extra: %s" name))
        (setq out (nelisp-bootstrap--insert-after file anchor out))
        (setq anchor file)))
    (let ((core-vars (expand-file-name "emacs-parity-core-vars.el" src))
          (vars (expand-file-name "emacs-vars.el" src)))
      (when (and (member core-vars out)
                 (member vars out))
        (setq out (nelisp-bootstrap--insert-before
                   core-vars vars (delete core-vars out)))))
    ;; `calendar.el' eagerly loads vendor calendar UI files such as
    ;; `cal-menu.el'.  Those sources call `defface', `suppress-keymap',
    ;; `make-mode-line-mouse-map', and `define-derived-mode' at top
    ;; level, so keep the calendar wrapper behind the builtin bridges
    ;; that define those names.  The diary chain additionally evaluates
    ;; `(defcustom diary-file (locate-user-emacs-file "diary" "diary") ...)'
    ;; at top level, so it must also come after the file substrate that
    ;; owns `locate-user-emacs-file'.  `emacs-fileio.el' lands strictly
    ;; after `emacs-mode-builtins.el', so anchoring here satisfies both
    ;; constraints and only ever moves calendar later.
    (let ((calendar (expand-file-name "calendar.el" src))
          (calendar-anchor (expand-file-name "emacs-fileio.el" src)))
      (when (and (member calendar out)
                 (member calendar-anchor out))
        (setq out (nelisp-bootstrap--insert-after
                   calendar calendar-anchor
                   (delete calendar out)))))
    (dolist (name nelisp-bootstrap-tail-extra-files)
      (let ((file (expand-file-name name src)))
        (when (file-readable-p file)
          (setq out (append (delete file out) (list file))))))
    (dolist (name nelisp-bootstrap-vendor-tail-extra-files)
      (let ((file (nelisp-bootstrap--vendor-source-file name)))
        (unless file
          (error "Missing readable bootstrap vendor tail extra: %s" name))
        (setq out (append (delete file out) (list file)))))
    out))

(defun nelisp-bootstrap--file-features (file)
  "Return features provided by FILE."
  (let (features)
    (cl-labels
        ((walk (form)
           (cond
            ((atom form) nil)
            ((memq (car form) '(quote function
                                     backquote-backquote-symbol
                                     backquote-unquote-symbol
                                     backquote-splice-symbol))
             nil)
            ((and (eq (car form) 'provide)
                  (consp (cdr form))
                  (consp (cadr form))
                  (eq (car (cadr form)) 'quote)
                  (consp (cdr (cadr form)))
                  (symbolp (cadr (cadr form))))
             (push (cadr (cadr form)) features))
            (t
             (walk (car form))
             (walk (cdr form))))))
      (dolist (form (nelisp-bootstrap--read-forms-from-file file))
        (walk form)))
    (delete-dups (nreverse features))))

(defun nelisp-bootstrap--file-requires (file)
  "Return features required by FILE."
  (let (requires)
    (cl-labels
        ((walk (form)
           (cond
            ((atom form) nil)
            ((memq (car form) '(quote function
                                     backquote-backquote-symbol
                                     backquote-unquote-symbol
                                     backquote-splice-symbol))
             nil)
            ((and (eq (car form) 'require)
                  (consp (cdr form))
                  (consp (cadr form))
                  (eq (car (cadr form)) 'quote)
                  (consp (cdr (cadr form)))
                  (symbolp (cadr (cadr form))))
             (push (cadr (cadr form)) requires))
            (t
             (walk (car form))
             (walk (cdr form))))))
      (dolist (form (nelisp-bootstrap--read-forms-from-file file))
        (walk form)))
    (delete-dups (nreverse requires))))

(defun nelisp-bootstrap--dependency-sort (files)
  "Return FILES in stable dependency order using top-level `require' edges."
  (let ((providers (make-hash-table :test 'eq))
        (deps (make-hash-table :test 'equal))
        (index (make-hash-table :test 'equal))
        (pending nil)
        ordered)
    (cl-loop for file in files
             for i from 0 do
             (puthash file i index)
             (dolist (feature (nelisp-bootstrap--file-features file))
               (unless (gethash feature providers)
                 (puthash feature file providers))))
    (dolist (file files)
      (let (req-files)
        (dolist (feature (nelisp-bootstrap--file-requires file))
          (let ((provider (gethash feature providers)))
            (when (and provider (not (equal provider file)))
              (push provider req-files))))
        (puthash file (delete-dups req-files) deps)
        (when (null (gethash file deps))
          (push file pending))))
    (setq pending
          (sort pending
                (lambda (a b)
                  (< (or (gethash a index) most-positive-fixnum)
                     (or (gethash b index) most-positive-fixnum)))))
    (while pending
      (let ((file (car pending)))
        (setq pending (cdr pending))
        (push file ordered)
        (dolist (other files)
          (let ((other-deps (gethash other deps)))
            (when (member file other-deps)
              (setq other-deps (delete file other-deps))
              (puthash other other-deps deps)
              (when (null other-deps)
                (push other pending)
                (setq pending
                      (sort pending
                            (lambda (a b)
                              (< (or (gethash a index) most-positive-fixnum)
                                 (or (gethash b index) most-positive-fixnum)))))))))))
    (let ((ordered (nreverse ordered)))
      (if (= (length ordered) (length files))
          ordered
        files))))

(defun nelisp-bootstrap--enforce-bootstrap-order (files)
  "Force essential standalone bootstrap order in FILES.

The standalone cold-boot path must install `cl-defmacro' before
`generator.el' is evaluated.  Host Emacs can satisfy this through
dynamic `load' indirection inside `cl-lib', but the generated
concatenated bootstrap bundle must make the order explicit."
  (let* ((src (nelisp-bootstrap--src-dir))
         (macros (expand-file-name "emacs-cl-macros.el" src))
         (cl-lib (expand-file-name "cl-lib.el" src))
         (seq (expand-file-name "seq.el" src))
         (generator (expand-file-name "generator.el" src))
         (vars (expand-file-name "emacs-vars.el" src))
         (numeric (expand-file-name "emacs-numeric.el" src))
         (runtime (expand-file-name "files-runtime.el" src))
         (early-foundation
          (mapcar (lambda (name) (expand-file-name name src))
                  '("emacs-vars.el"
                    "emacs-fns.el"
                    "emacs-eval.el"
                    "emacs-list.el"
                    "emacs-hash.el"
                    "emacs-symbol.el"
                    "emacs-callproc.el"
                    "emacs-char-table.el"
                    "emacs-backquote.el"
                    "emacs-error.el"
                    "emacs-string.el"
                    "emacs-pcase.el"
                    "cl-lib.el"
                    "subr-x.el"
                    "emacs-cl-macros.el"
                    "emacs-stub.el"
                    "emacs-stub-bulk.el"
                    "emacs-os-detect.el"
                    "emacs-easy-mmode.el"
                    "emacs-time.el"
                    "calendar.el"
                    "emacs-numeric.el"
                    "emacs-subr-extras.el"
                    "emacs-edebug-stubs.el"
                    "seq.el"
                    "map.el"
                    "nelisp-emacs-compat.el"
                    "nelisp-emacs-compat-fileio.el"
                    "files-runtime.el"
                    "emacs-ffi.el"
                    "emacs-standalone.el"
                    "emacs-file-name-handler.el"
                    "emacs-fileio-builtins.el"
                    "nelisp-text-buffer.el"
                    "nelisp-regex.el"
                    "emacs-buffer.el"
                    "emacs-buffer-builtins.el"
                    "emacs-line-builtins.el"
                    "emacs-search-builtins.el"
                    "emacs-undo.el"
                    "emacs-undo-builtins.el"
                    "emacs-edit-builtins.el")))
         (late-loaders
          (mapcar (lambda (name) (expand-file-name name src))
                  '("emacs-foundation.el"
                    "emacs-buffer-core.el"
                    "emacs-editing.el"
                    "emacs-io.el"
                    "emacs-core.el"
                    "nelisp-emacs.el"
                    "emacs-init.el"
                    "nemacs-loadup.el"
                    "nemacs-main.el")))
         (out files))
    ;; The standalone bundle does not need package/application loaders early.
    ;; Put the concrete owner files first so later loader/facade evaluation sees
    ;; a fully populated substrate instead of recursively `require'-ing half the
    ;; same graph and emitting uncaught top-level noise.
    (dolist (file (reverse early-foundation))
      (when (member file out)
        (setq out (cons file (delete file out)))))
    (dolist (file late-loaders)
      (when (member file out)
        (setq out (append (delete file out) (list file)))))
    (when (member macros out)
      (setq out (delete macros out))
      (if (member cl-lib out)
          (setq out (nelisp-bootstrap--insert-before macros cl-lib out))
        (setq out (cons macros out))))
    (when (and (member cl-lib out)
               (member generator out))
      (setq out (delete cl-lib out))
      (setq out (nelisp-bootstrap--insert-before cl-lib generator out)))
    (when (and (member cl-lib out)
               (member seq out))
      (setq out (delete cl-lib out))
      (setq out (nelisp-bootstrap--insert-before cl-lib seq out)))
    ;; Some standalone vendor-core modules touch user-directory/runtime
    ;; predicates and numeric bit helpers during top-level evaluation.
    ;; Keep those foundational files ahead of heavier facades.
    (when (member vars out)
      (setq out (delete vars out))
      (setq out (cons vars out)))
    (when (and (member numeric out)
               (member seq out))
      (setq out (delete numeric out))
      (setq out (nelisp-bootstrap--insert-before numeric seq out)))
    (when (and (member runtime out)
               (member (expand-file-name "files.el" src) out))
      (setq out (delete runtime out))
      (setq out (nelisp-bootstrap--insert-before
                 runtime (expand-file-name "files.el" src) out)))
    (dolist (anchor '("files-standalone-buffer.el"
                      "emacs-fileio-builtins.el"
                      "emacs-file-name-handler.el"
                      "nelisp-emacs-compat-fileio.el"))
      (let ((anchor-file (expand-file-name anchor src)))
        (when (and (member runtime out) (member anchor-file out))
          (setq out (delete runtime out))
          (setq out (nelisp-bootstrap--insert-before runtime anchor-file out)))))
    ;; App-facing shims such as isearch/shell/man/help-gui can execute top-level
    ;; code that expects these core owners/bridges to exist already.
    (dolist (pair '(("emacs-faces.el" . "emacs-isearch.el")
                    ("emacs-faces-builtins.el" . "emacs-isearch.el")
                    ("emacs-minibuffer.el" . "emacs-isearch.el")
                    ("emacs-minibuffer-builtins.el" . "emacs-isearch.el")
                    ("emacs-process.el" . "emacs-man.el")
                    ("emacs-process-builtins.el" . "emacs-man.el")
                    ("emacs-comint.el" . "emacs-shell.el")
                    ("emacs-keymap-builtins.el" . "emacs-help-gui.el")
                    ("emacs-command-loop-builtins.el" . "emacs-help-gui.el")
                    ("emacs-syntax-table.el" . "emacs-imenu.el")))
      (let ((owner (expand-file-name (car pair) src))
            (anchor (expand-file-name (cdr pair) src)))
        (when (and (member owner out) (member anchor out))
          (setq out (delete owner out))
          (setq out (nelisp-bootstrap--insert-before owner anchor out)))))
    ;; Builtins often run top-level install/defvar code that expects the
    ;; owning prefixed implementation to be present already.  On the REPL
    ;; bootstrap path a wrong order leaves functions defined but features
    ;; unprovided after an early top-level error.
    (dolist (pair '(("emacs-dired-min.el" . "dired.el")
                    ("emacs-calc.el" . "calc.el")
                    ("emacs-man.el" . "man.el")
                    ("emacs-eshell.el" . "eshell.el")
                    ("emacs-shell.el" . "shell.el")
                    ("emacs-project.el" . "project.el")
                    ("emacs-ielm.el" . "ielm.el")
                    ("emacs-isearch.el" . "isearch.el")
                    ("emacs-replace.el" . "replace.el")
                    ("emacs-comint.el" . "comint.el")
                    ("emacs-vc.el" . "vc.el")
                    ("emacs-compile.el" . "compile.el")
                    ("emacs-xref.el" . "xref.el")
                    ("emacs-imenu.el" . "imenu.el")
                    ("emacs-faces.el" . "emacs-faces-builtins.el")
                    ("emacs-frame.el" . "emacs-frame-builtins.el")
                    ("emacs-window.el" . "emacs-window-builtins.el")
                    ("emacs-keymap.el" . "emacs-keymap-builtins.el")
                    ("emacs-command-loop.el" . "emacs-command-loop-builtins.el")
                    ("emacs-minibuffer.el" . "emacs-minibuffer-builtins.el")
                    ("emacs-process.el" . "emacs-process-builtins.el")
                    ("emacs-buffer.el" . "emacs-buffer-builtins.el")
                    ("emacs-undo.el" . "emacs-undo-builtins.el")
                    ("emacs-font-lock.el" . "emacs-font-lock-builtins.el")))
      (let ((owner (expand-file-name (car pair) src))
            (builtins (expand-file-name (cdr pair) src)))
        (when (and (member owner out) (member builtins out))
          (setq out (delete owner out))
          (setq out (nelisp-bootstrap--insert-before owner builtins out)))))
    out))

(defconst nelisp-bootstrap--feature-registry-prologue
  (concat
   ";; Bootstrap prologue: standalone NeLisp ships stub provide/require with no feature registry.\n"
   ";; Install the compat registry before any bundled file evaluates top-level provide/require.\n"
   "(unless (boundp 'features)\n"
   "  (defvar features nil))\n"
   "(when (or (fboundp 'nl-write-file)\n"
   "          (not (boundp 'emacs-version))\n"
   "          (not (stringp emacs-version)))\n"
   "  (defun provide (feature &optional _subfeatures)\n"
   "    (unless (memq feature features)\n"
   "      (setq features (cons feature features)))\n"
   "    feature)\n"
   "  (defun featurep (feature &optional _subfeature)\n"
   "    (if (memq feature features) t nil))\n"
   "  (defun locate-file (filename path &optional suffixes predicate)\n"
   "    (let ((suffix-list (cond\n"
   "                        ((null suffixes) (list \"\"))\n"
   "                        ((stringp suffixes) (list suffixes))\n"
   "                        (t suffixes)))\n"
   "          (dirs path)\n"
   "          (found nil))\n"
   "      (while (and dirs (not found))\n"
   "        (let ((suffixes-left suffix-list))\n"
   "          (while (and suffixes-left (not found))\n"
   "            (let ((candidate\n"
   "                   (expand-file-name\n"
   "                    (concat filename (car suffixes-left))\n"
   "                    (car dirs))))\n"
   "              (when (if predicate\n"
   "                        (funcall predicate candidate)\n"
   "                      (file-exists-p candidate))\n"
   "                (setq found candidate)))\n"
   "            (setq suffixes-left (cdr suffixes-left))))\n"
   "        (setq dirs (cdr dirs)))\n"
   "      found))\n"
   "  (defun require (feature &optional filename noerror)\n"
   "    (if (featurep feature)\n"
   "        feature\n"
   "      (let* ((base (or filename (symbol-name feature)))\n"
   "             (path (or (and (stringp base)\n"
   "                            (file-exists-p base)\n"
   "                            base)\n"
   "                       (and (boundp 'load-path)\n"
   "                            (locate-file base load-path (list \".el\" \"\"))))))\n"
   "        (cond\n"
   "         (path\n"
   "          (load-file path)\n"
   "          (cond\n"
   "           ((featurep feature) feature)\n"
   "           (noerror nil)\n"
   "           (t (error \"Required feature was not provided: %S\" feature))))\n"
   "         (noerror nil)\n"
   "         (t (error \"Cannot open load file: %S\" feature)))))))\n"))

(defconst nelisp-bootstrap--feature-registry-prologue-forms
  '((unless (boundp 'features)
      (defvar features nil))
    (when (or (fboundp 'nl-write-file)
              (not (boundp 'emacs-version))
              (not (stringp emacs-version)))
      (defun provide (feature &optional _subfeatures)
        (unless (memq feature features)
          (setq features (cons feature features)))
        feature)
      (defun featurep (feature &optional _subfeature)
        (if (memq feature features) t nil))
      (defun locate-file (filename path &optional suffixes predicate)
        (let ((suffix-list (cond
                            ((null suffixes) (list ""))
                            ((stringp suffixes) (list suffixes))
                            (t suffixes)))
              (dirs path)
              (found nil))
          (while (and dirs (not found))
            (let ((suffixes-left suffix-list))
              (while (and suffixes-left (not found))
                (let ((candidate
                       (expand-file-name
                        (concat filename (car suffixes-left))
                        (car dirs))))
                  (when (if predicate
                            (funcall predicate candidate)
                          (file-exists-p candidate))
                    (setq found candidate)))
                (setq suffixes-left (cdr suffixes-left))))
            (setq dirs (cdr dirs)))
          found))
      (defun require (feature &optional filename noerror)
        (if (featurep feature)
            feature
          (let* ((base (or filename (symbol-name feature)))
                 (path (or (and (stringp base)
                                (file-exists-p base)
                                base)
                           (and (boundp 'load-path)
                                (locate-file base load-path (list ".el" ""))))))
             (cond
              (path
               (load-file path)
               (cond
                ((featurep feature) feature)
                (noerror nil)
                (t (error "Required feature was not provided: %S" feature))))
             (noerror nil)
             (t (error "Cannot open load file: %S" feature)))))))))

(defun nelisp-bootstrap--runtime-anchor-file ()
  "Return the absolute runtime anchor file for standalone REPL bootstrap."
  (expand-file-name "src/nemacs-main.el" nelisp-bootstrap-repo-root))

(defun nelisp-bootstrap--runtime-anchor-directory ()
  "Return the absolute runtime anchor directory for standalone REPL bootstrap."
  (file-name-as-directory nelisp-bootstrap-repo-root))

(defun nelisp-bootstrap--default-load-paths ()
  "Return the default standalone load-path baked into the bootstrap."
  (cons (nelisp-bootstrap--src-dir)
        (mapcar (lambda (relative)
                  (expand-file-name relative nelisp-bootstrap-repo-root))
                nelisp-bootstrap-vendor-load-path-subdirs)))

(defun nelisp-bootstrap--runtime-anchor-prologue-forms ()
  "Return standalone REPL forms that seed source-location globals.

The raw standalone REPL path evaluates one physical line at a time with no
loader context.  Seed the common file-location globals so bootstrap helper
modules can derive a stable source directory before the higher-level launcher
overrides them for workflow tests."
  (let ((anchor (nelisp-bootstrap--runtime-anchor-file))
        (dir (nelisp-bootstrap--runtime-anchor-directory)))
    `((unless (boundp 'load-file-name)
        (defvar load-file-name nil))
      (unless (boundp 'buffer-file-name)
        (defvar buffer-file-name nil))
      (unless (boundp 'default-directory)
        (defvar default-directory ,dir))
      (setq load-file-name ,anchor)
      (setq buffer-file-name ,anchor)
      (setq default-directory ,dir))))

(defun nelisp-bootstrap--runtime-load-path-prologue-forms ()
  "Return standalone REPL forms that seed vendor-aware `load-path'."
  (let ((vendor-root (directory-file-name (nelisp-bootstrap--vendor-dir)))
        (load-paths (nelisp-bootstrap--default-load-paths)))
    `((unless (boundp 'nelisp-emacs-vendor-root)
        (defvar nelisp-emacs-vendor-root nil))
      (unless (boundp 'load-path)
        (defvar load-path nil))
      (setq nelisp-emacs-vendor-root ,vendor-root)
      (setq load-path ',load-paths))))

(defun nelisp-bootstrap--emit-post-file-bundle-forms (file)
  "Return extra bundle forms that should follow FILE."
  (let ((rel (file-relative-name file nelisp-bootstrap-repo-root)))
    (cond
     ((string= rel "src/emacs-stub.el")
      '("(provide 'custom)\n"))
     ((string= rel "src/nemacs-main.el")
      (mapcar (lambda (form)
                (concat (prin1-to-string form) "\n"))
              (nelisp-bootstrap--runtime-load-path-prologue-forms))))))

(defun nelisp-bootstrap--emit-post-file-repl-forms (file)
  "Return extra REPL forms that should follow FILE."
  (let ((rel (file-relative-name file nelisp-bootstrap-repo-root)))
    (cond
     ((string= rel "src/emacs-stub.el")
      '((provide 'custom)))
     ((string= rel "src/nemacs-main.el")
      (nelisp-bootstrap--runtime-load-path-prologue-forms)))))

(defun nelisp-bootstrap--write-bundle (files output)
  "Write FILES into OUTPUT as one lexical-binding Elisp bundle."
  (make-directory (file-name-directory output) t)
  ;; NeLisp's current `load' prefers OUTPUT.elc even when OUTPUT ends in
  ;; ".el".  Remove stale byte-compiled companions so the bootstrap
  ;; bundle stays a plain-Elisp preload file.
  (let ((compiled (concat output "c")))
    (when (file-exists-p compiled)
      (delete-file compiled)))
  (with-temp-buffer
    (insert ";;; nemacs-bootstrap.el --- generated NeLisp bootstrap bundle  -*- lexical-binding: t; -*-\n")
    (insert ";;; Generated by scripts/build-nelisp-bootstrap.el; do not edit.\n\n")
    (insert nelisp-bootstrap--feature-registry-prologue)
    (insert "\n")
    (dolist (file files)
      (let ((rel (file-relative-name file nelisp-bootstrap-repo-root)))
        (insert "\n;;; >>> " rel "\n")
        (insert-file-contents file)
        (goto-char (point-max))
        (dolist (feature (nelisp-bootstrap--file-features file))
          (insert "\n(provide '")
          (insert (symbol-name feature))
          (insert ")\n"))
        (dolist (form (nelisp-bootstrap--emit-post-file-bundle-forms file))
          (insert form))
        (insert "\n;;; <<< " rel "\n")))
    (let ((coding-system-for-write 'utf-8-emacs-unix))
      (write-region (point-min) (point-max) output nil 'silent))))

(defun nelisp-bootstrap--read-forms-from-file (file)
  "Return top-level forms read from FILE."
  (standalone-source-normalize-read-forms-from-file file))

(defun nelisp-bootstrap--one-line-string-literal (string)
  "Return STRING as an Elisp string literal that fits on one line."
  (let ((literal
         (standalone-source-normalize-escape-printed-controls
          (let ((print-quoted nil))
            (prin1-to-string string)))))
    (setq literal (replace-regexp-in-string "\n" "\\\\n" literal t t))
    (setq literal (replace-regexp-in-string "\r" "\\\\r" literal t t))
    literal))

(defun nelisp-bootstrap--standalone-repl-form (form)
  "Return FORM normalized for the standalone-reader REPL bootstrap.

The standalone prelude currently ignores definition docstrings.  Dropping
those unused arguments keeps generated REPL bootstrap input smaller and
avoids retaining large docstring literals in the persistent evaluator."
  (cond
   ((and (consp form)
         (memq (car form) '(defun defmacro))
         (>= (length form) 4)
         (stringp (nth 3 form)))
    (append (list (nth 0 form) (nth 1 form) (nth 2 form))
            (nthcdr 4 form)))
   ((and (consp form)
         (memq (car form) '(defvar defconst))
         (>= (length form) 4)
         (stringp (nth 3 form)))
    (list (nth 0 form) (nth 1 form) (nth 2 form)))
   ((and (consp form)
         (eq (car form) 'defvar-local)
         (>= (length form) 4)
         (stringp (nth 3 form)))
    (list 'defvar (nth 1 form) (nth 2 form)))
   ((and (consp form)
         (eq (car form) 'defcustom)
         (>= (length form) 4)
         (stringp (nth 3 form)))
    (list 'defvar (nth 1 form) (nth 2 form)))
   ((and (consp form)
         (eq (car form) 'cl-defstruct)
         (>= (length form) 3)
         (stringp (nth 2 form)))
    (append (list (nth 0 form) (nth 1 form))
            (nthcdr 3 form)))
   (t form)))

(defun nelisp-bootstrap--function-headed-list-p (object)
  "Return non-nil when OBJECT contains a list headed by symbol `function'."
  (cond
   ((consp object)
    (or (eq (car object) 'function)
        (nelisp-bootstrap--function-headed-list-p (car object))
        (nelisp-bootstrap--function-headed-list-p (cdr object))))
   (t nil)))

(defun nelisp-bootstrap--quoted-defun-lambda-list-risk-p (object)
  "Return non-nil when OBJECT contains a defun whose arglist prints as `#''."
  (cond
   ((and (consp object)
         (memq (car object) '(defun defmacro))
         (nelisp-bootstrap--function-headed-list-p (nth 2 object)))
    t)
   ((and (consp object)
         (eq (car object) 'quote))
    nil)
   ((consp object)
    (or (nelisp-bootstrap--quoted-defun-lambda-list-risk-p (car object))
        (nelisp-bootstrap--quoted-defun-lambda-list-risk-p (cdr object))))
   (t nil)))

(defun nelisp-bootstrap--direct-repl-form-p (rel form &optional form-string)
  "Return non-nil when FORM from REL should be emitted as a direct REPL form.

FORM-STRING, when non-nil, is the printed form used for size-based emission."
  (or (not nelisp-bootstrap-repl-nested-eval-source)
      (member rel '("src/nelisp-text-buffer.el"
                    "src/nelisp-emacs-compat.el"))
      (and (consp form)
           (eq (car form) 'cl-defstruct))
      (and form-string
           (> (length form-string)
              nelisp-bootstrap-repl-direct-character-limit))))

(defun nelisp-bootstrap--repl-form-string (form)
  "Return FORM safely printed on one REPL input line."
  (let ((print-escape-newlines t)
        (print-escape-control-characters t)
        ;; Host `prin1' abbreviates any list headed by `function' as `#'...'.
        ;; That is correct for quoted function forms, but invalid inside a
        ;; lambda list such as `(defun maphash (function table) ...)'.
        (print-quoted
         (not (nelisp-bootstrap--quoted-defun-lambda-list-risk-p form))))
    (standalone-source-normalize-escape-printed-controls
     (prin1-to-string form))))

(defun nelisp-bootstrap--insert-direct-repl-form (form)
  "Insert normalized FORM as one direct, value-discarding REPL form."
  (insert "(progn ")
  (insert (nelisp-bootstrap--repl-form-string form))
  (insert " nil)\n"))

(defun nelisp-bootstrap--insert-nested-repl-form (form-string)
  "Insert FORM-STRING through the diagnostic nested reader transport."
  (insert "(progn (nelisp--eval-source-string ")
  (insert (nelisp-bootstrap--one-line-string-literal form-string))
  (insert ") nil)\n"))

(defun nelisp-bootstrap--write-repl-bundle (files output)
  "Write FILES into OUTPUT as standalone-reader REPL input.

The standalone reader's persistent development surface is the REPL.  Normal
bootstrap forms are emitted directly so each form is parsed exactly once.
Nested source-string evaluation is reserved for explicit diagnostics."
  (make-directory (file-name-directory output) t)
  (with-temp-buffer
    (insert ";;; nemacs-bootstrap.repl --- generated NeLisp bootstrap REPL input\n")
    (insert ";;; Generated by scripts/build-nelisp-bootstrap.el; do not edit.\n")
    (insert ";; Bootstrap prologue: user-visible feature registry must exist before any bundled provide.\n")
    (dolist (form nelisp-bootstrap--feature-registry-prologue-forms)
      (nelisp-bootstrap--insert-direct-repl-form form))
    (insert ";; Bootstrap prologue: standalone REPL file-location globals.\n")
    (dolist (form (nelisp-bootstrap--runtime-anchor-prologue-forms))
      (nelisp-bootstrap--insert-direct-repl-form form))
    (insert "\n")
    (dolist (file files)
      (let ((rel (file-relative-name file nelisp-bootstrap-repo-root)))
        (insert "\n;;; >>> " rel "\n")
        (dolist (source-form (nelisp-bootstrap--read-forms-from-file file))
          (let* ((form (nelisp-bootstrap--standalone-repl-form source-form))
                 (form-string (nelisp-bootstrap--repl-form-string form)))
            (if (nelisp-bootstrap--direct-repl-form-p rel form form-string)
                (nelisp-bootstrap--insert-direct-repl-form form)
              (nelisp-bootstrap--insert-nested-repl-form form-string))))
        (dolist (feature (nelisp-bootstrap--file-features file))
          (nelisp-bootstrap--insert-direct-repl-form
           `(provide ',feature)))
        (dolist (form (nelisp-bootstrap--emit-post-file-repl-forms file))
          (nelisp-bootstrap--insert-direct-repl-form form))
        (insert ";;; <<< " rel "\n")))
    (let ((coding-system-for-write 'utf-8-emacs-unix))
      (write-region (point-min) (point-max) output nil 'silent))))

(defun nelisp-bootstrap-build-batch ()
  "Generate `nelisp-bootstrap-output-file' and print a short summary."
  (let* ((src (nelisp-bootstrap--src-dir))
         (vendor (nelisp-bootstrap--vendor-dir))
         (nelisp-emacs-vendor-root (directory-file-name vendor)))
    (add-to-list 'load-path src)
    (add-to-list 'load-path (expand-file-name "emacs-lisp" vendor) t)
    (add-to-list 'load-path (expand-file-name "emacs-lisp/emacs-lisp" vendor) t)
    (require 'nemacs-main)
    ;; Ship the host `load-history' order (as completed with the
    ;; standalone-only injected files) verbatim.  That order is
    ;; authoritative: it is the exact sequence in which host Emacs really
    ;; loaded the sources, so it already satisfies every runtime
    ;; dependency -- both explicit top-level `require' edges AND the many
    ;; implicit ones (top-level code that calls functions defined in other
    ;; files, dynamic `load' indirection inside cl-lib, etc.).
    ;;
    ;; The former `nelisp-bootstrap--dependency-sort' /
    ;; `nelisp-bootstrap--enforce-bootstrap-order' passes are intentionally
    ;; NOT applied.  The dependency sort topologically orders on the
    ;; top-level `require' graph alone, which is an INCOMPLETE model of the
    ;; real dependencies; reordering by it freely permutes files in ways
    ;; that break the unexpressed implicit dependencies the load-history
    ;; order encoded.  Measured on the standalone REPL replay
    ;; (`nelisp-package-resolution --repl'): raw completed order = 3 benign
    ;; self-healing `uncaught error' lines; dependency-sort alone = 42;
    ;; enforce-bootstrap-order applied on top = 16; both applied to the raw
    ;; order = 18.  The two passes are net-harmful on the current source
    ;; graph, so the pipeline stops at `complete-file-list'.  (The two
    ;; helper functions are retained above only as reference; they have no
    ;; other callers.)
    ;;
    ;; Two targeted exceptions are applied below:
    ;; `nelisp-bootstrap--hoist-standalone-definers' moves the few files whose
    ;; late position the standalone cannot survive, and
    ;; `nelisp-bootstrap--sink-bare-name-vendor-loaders' moves the few whose
    ;; early position it cannot survive.  See their docstrings.
    (let* ((files (nelisp-bootstrap--sink-bare-name-vendor-loaders
                   (nelisp-bootstrap--hoist-standalone-definers
                    (nelisp-bootstrap--complete-file-list
                     (nelisp-bootstrap--collect-loaded-src-files)))))
           (output (expand-file-name nelisp-bootstrap-output-file))
           (repl-output
            (expand-file-name
             (or nelisp-bootstrap-repl-output-file
                 (concat (file-name-sans-extension output) ".repl")))))
      (nelisp-bootstrap--write-bundle files output)
      (nelisp-bootstrap--write-repl-bundle files repl-output)
      (princ (format "nelisp-bootstrap bundle=%s repl=%s files=%d\n"
                     output repl-output (length files))))))

(defconst nelisp-bootstrap--standalone-definer-files
  '("emacs-eval.el"
    "emacs-pcase.el"
    "emacs-cl-macros.el"
    "emacs-parity-setf-places.el"
    "emacs-parity-macroexpand.el"
    "emacs-parity-clloop.el"
    "emacs-parity-eieio.el"
    "emacs-parity-evil.el"
    "emacs-parity-macros2.el"
    "emacs-parity-flycheck.el"
    "emacs-parity-cc.el")
  "Files `nelisp-bootstrap--hoist-standalone-definers' moves ahead of `generator.el'.
They are placed in this order.  The core definers retain their existing
symbol-presence guards; the parity files activate only behind standalone
NeLisp markers, so hoisting remains inert under host Emacs.")

(defun nelisp-bootstrap--hoist-standalone-definers (files)
  "Return FILES with the standalone's macro definers moved ahead of `generator.el'.
Host `load-history' order is authoritative and is shipped verbatim, but it
records an order host Emacs only survives because the names below are either
autoloaded or preloaded C.  The standalone has neither, so a bundle in raw
load-history order calls them thousands of lines before their definitions and
the load dies at the first one.

Each row is a call line -> definition line in the raw-order bundle, and each
was a real failure before its file was hoisted:

  emacs-cl-macros.el  cl-defmacro from generator.el     7,649 -> 18,377
  emacs-eval.el       eval-after-load from generator.el  6,613 -> 18,294
  emacs-pcase.el      pcase-defmacro from rx.el          8,310 -> 18,053
  emacs-eval.el       define-obsolete-function-alias from rx.el
                                                         8,367 -> 18,475

The first row is the original measurement that motivated hoisting
`emacs-cl-macros.el' alone; the rest were measured 2026-09-04, when the
standalone boot still reported them as four uncaught `void-function' errors.

`generator.el' is the earliest consumer of all three files, so one insertion
point covers every row; `rx.el' follows it in load-history order.

This moves those files and leaves every other file in load-history order.  It
is deliberately not the retired `nelisp-bootstrap--enforce-bootstrap-order'
pass, which permuted around forty files and measured net-harmful (16 uncaught
error lines against 3 for the raw order)."
  (let* ((src (nelisp-bootstrap--src-dir))
         (generator (expand-file-name "generator.el" src))
         (out files))
    (if (not (member generator out))
        out
      (dolist (name nelisp-bootstrap--standalone-definer-files out)
        (let ((file (expand-file-name name src)))
          (when (and (member file out)
                     ;; `member' returns the tail from the element, so a
                     ;; SHORTER tail means a LATER position.  Only move the
                     ;; file when it really is behind `generator.el'; one
                     ;; already ahead of it must keep its own position.
                     (< (length (member file out))
                        (length (member generator out))))
            (setq out (nelisp-bootstrap--insert-before
                       file generator (remove file out)))))))))

(defconst nelisp-bootstrap--bare-name-vendor-loaders
  '("calendar.el")
  "Files that `load' a vendor library by bare name, without a directory.
`nelisp-bootstrap--sink-bare-name-vendor-loaders' moves them after
`emacs-load.el'.")

(defun nelisp-bootstrap--sink-bare-name-vendor-loaders (files)
  "Return FILES with bare-name vendor loaders moved after `emacs-load.el'.
The standalone reader's native `load' resolves nothing: measured 2026-09-04
against `target/nelisp' at v1.2.0, it does not search `load-path' (a `let'
binding of it cannot work either -- `load-path' is bound there but not
`special-variable-p', so the binding is lexical -- and neither does a global
`setq'), does not use `default-directory', and does not append `.el'.  Only an
exact existing path loads.  Bare-name resolution appears only when
`emacs-load.el' installs its own `load', which goes through `locate-library'.

So a file that loads a vendor library which itself does a bare-name `load' has
to come after `emacs-load.el'.  `src/calendar.el' pulls in
`vendor/emacs-lisp/calendar/calendar.el', whose line 130 is
`(load \"cal-loaddefs\" nil t)'; in raw load-history order it ran at bundle line
53,093 while `emacs-load.el' ended at 75,136, so the native `load' took the
call and the boot reported (file-missing \"cal-loaddefs\") even though
`cal-loaddefs.el' sits beside `calendar.el'.

Moving the small leaf shim is deliberate: hoisting `emacs-load.el' instead
would put the `load' replacement ahead of ~50,000 lines that were measured
loading under the native one."
  (let* ((src (nelisp-bootstrap--src-dir))
         (loader (expand-file-name "emacs-load.el" src))
         (out files))
    (if (not (member loader out))
        out
      (dolist (name nelisp-bootstrap--bare-name-vendor-loaders out)
        (let ((file (expand-file-name name src)))
          (when (and (member file out)
                     ;; A longer `member' tail means an earlier position, so
                     ;; this is "FILE currently precedes `emacs-load.el'".
                     (> (length (member file out))
                        (length (member loader out))))
            (setq out (nelisp-bootstrap--insert-after
                       file loader (remove file out)))))))))

(provide 'build-nelisp-bootstrap)

;;; build-nelisp-bootstrap.el ends here
