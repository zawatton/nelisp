;;; nemacs-runtime-image-preload.el --- NeLisp runtime-image preload forms  -*- lexical-binding: t; -*-

;;; Commentary:

;; Shared preload entry points for `make bake-*-runtime-image'.  Keep the
;; Makefile recipes as small as possible so new runtime-image lanes can be
;; added without duplicating the load-path/bootstrap form.

;;; Code:

;; Source-v1 runtime image replay preserves top-level `setq' more reliably
;; than `defconst', while functions below need these variables after boot.
(setq nemacs-runtime-image-preload--script-directory
      (file-name-directory
       (or (and (boundp 'load-file-name) load-file-name)
           (and (boundp 'buffer-file-name) buffer-file-name)
           "")))

(setq nemacs-runtime-image-package-load-paths
      '("packages/emacs-lib-core/src"
        "packages/emacs-lib-core/src"
        "packages/emacs-lib-core/src"
        "packages/emacs-lib-core/src"
        "packages/emacs-lib-core/src"
        "packages/emacs-lib-facade/src"
        "packages/emacs-lib-core/src"
        "packages/emacs-lib-core/src"
        "packages/emacs-lib-core/src"
        "packages/emacs-lib-core/src"
        "packages/emacs-lib-text-core/src"
        "packages/emacs-lib-text-core/src"
        "packages/emacs-lib-core/src"
        "packages/emacs-lib-core/src"))

(defun nemacs-runtime-image-setup-paths (repo-root)
  "Install REPO-ROOT's package/app scaffold and vendor paths for a bake."
  (unless (boundp 'load-path)
    (defvar load-path nil))
  (setq nelisp-emacs-vendor-root (concat repo-root "/vendor"))
  (setq load-path
        (append (mapcar (lambda (path) (concat repo-root "/" path))
                        nemacs-runtime-image-package-load-paths)
                ;; Package paths stay first.  This exposes src-only facades
                ;; such as `emacs-imenu' and `emacs-xref' to runtime-image
                ;; lazy wrappers until they are promoted into packages.
                (list (concat repo-root "/src"))
                (list (concat repo-root "/vendor/emacs-lisp")
                      (concat repo-root "/vendor/emacs-lisp/emacs-lisp")
                      (concat repo-root "/vendor/emacs-lisp/vc")
                      (concat repo-root "/vendor/emacs-lisp/international")
                      (concat repo-root "/vendor/emacs-lisp/obsolete")
                      ;; Doc 37 (task #16): vendor Tramp's own
                      ;; directory plus `parse-time''s home under
                      ;; calendar/.  Path-only -- nothing here `require's
                      ;; Tramp into the baked image (Doc 37 risk #10);
                      ;; `nemacs-tramp-setup' calls `require' dynamically
                      ;; after session start.
                      (concat repo-root "/vendor/emacs-lisp/net")
                      (concat repo-root "/vendor/emacs-lisp/calendar"))
                load-path))
  t)

(defun nemacs-runtime-image--core-source-file (repo-root name)
  "Return REPO-ROOT/src/NAME."
  (concat repo-root "/src/" name))

(defun nemacs-runtime-image-preload--load-source-file (file)
  "Load FILE robustly on both host Emacs and standalone runtime images."
  (load file nil 'no-message t t))

(defun nemacs-runtime-image--ensure-core-after-bootstrap (repo-root)
  "Make the batch entry visible after a bootstrap load.

The runtime-image replay path does not currently preserve every side-effect
that the standalone bootstrap bundle relies on, especially feature-registry
updates from fallback forms.  If the bundle load returns with `nemacs-main'
still missing, load the source entry files directly and fail loudly if the
batch entry is still unavailable."
  (unless (featurep 'nemacs-main)
    (load (nemacs-runtime-image--core-source-file repo-root "emacs-fns.el")
          nil 'no-message t t)
    (load (nemacs-runtime-image--core-source-file repo-root "nemacs-main.el")
          nil 'no-message t t))
  (unless (featurep 'nemacs-main)
    (error "runtime bootstrap did not provide nemacs-main"))
  (unless (fboundp 'nemacs-batch-main)
    (error "runtime bootstrap did not define nemacs-batch-main"))
  t)

(defun nemacs-runtime-image-load-bootstrap (repo-root bootstrap-file)
  "Load BOOTSTRAP-FILE for REPO-ROOT when it is non-empty."
  (when (and bootstrap-file
             (not (string= bootstrap-file "")))
    (load bootstrap-file nil 'no-message t t))
  (nemacs-runtime-image--ensure-core-after-bootstrap repo-root)
  t)

(defun nemacs-runtime-image-preload--load-gnu-libraries (repo-root)
  "Load GNU 31.1 libraries after the NeLisp image substrate is ready.
The list follows loadup.el order.  Obarray and abbrev are already in the
bootstrap, ahead of the bundled mode definitions that create abbrev tables."
  (dolist (library '("emacs-lisp/emacs-lisp/debug-early.el"
                     "emacs-lisp/version.el"
                     "emacs-lisp/env.el"
                     "emacs-lisp/format.el"
                     "staged-emacs-lisp/window.el"
                     "staged-emacs-lisp/faces.el"
                     ;; The class registry precedes the dispatcher.
                     ;; Publish advice after the dispatcher and bundled seq.
                     "emacs-lisp/emacs-lisp/cl-preloaded.el"
                     "emacs-lisp/emacs-lisp/cl-generic.el"
                     "emacs-lisp/emacs-lisp/nadvice.el"
                     "emacs-lisp/frame.el"
                     "emacs-lisp/term/tty-colors.el"
                     "emacs-lisp/send-to.el"
                     "emacs-lisp/mouse.el"
                     "emacs-lisp/scroll-bar.el"
                     "emacs-lisp/select.el"
                     "emacs-lisp/emacs-lisp/easymenu.el"
                     "emacs-lisp/rfn-eshadow.el"
                     "emacs-lisp/frameset.el"
                     "emacs-lisp/tab-bar.el"
                     "emacs-lisp/textmodes/page.el"
                     "emacs-lisp/register.el"
                     ;; The GNU mode calls header-line-indent-mode, normally
                     ;; an autoload from display-line-numbers.el.
                     "emacs-lisp/display-line-numbers.el"
                     "emacs-lisp/emacs-lisp/tabulated-list.el"
                     "emacs-lisp/buff-menu.el"
                     "emacs-lisp/fringe.el"
                     "emacs-lisp/image.el"
                     "emacs-lisp/international/fontset.el"
                     "emacs-lisp/dynamic-setting.el"
                     "emacs-lisp/touch-screen.el"
                     "emacs-lisp/term/common-win.el"
                     "emacs-lisp/disp-table.el"
                     "emacs-lisp/mwheel.el"
                     "emacs-lisp/emacs-lisp/float-sup.el"
                     "emacs-lisp/paren.el"
                     "emacs-lisp/emacs-lisp/shorthands.el"
                     "emacs-lisp/emacs-lisp/eldoc.el"
                     "emacs-lisp/tooltip.el"
                     "emacs-lisp/emacs-lisp/rmc.el"
                     "emacs-lisp/international/mule-util.el"
                     "emacs-lisp/obsolete/cdl.el"
                     "emacs-lisp/hex-util.el"
                     ;; GNU loaddefs autoloads these functions.  The source
                     ;; deliberately has no `provide' form.
                     "emacs-lisp/emacs-lisp/map-ynp.el"
                     "emacs-lisp/international/idna-mapping.el"
                     "emacs-lisp/international/emoji-labels.el"
                     ;; The bootstrap has already loaded GNU button.el.
                     "emacs-lisp/help-mode.el"
                     ;; cl-extra's help integration needs help-mode.  Load
                     ;; both CL autoload providers before consumers run.
                     "emacs-lisp/emacs-lisp/cl-seq.el"
                     "emacs-lisp/emacs-lisp/cl-extra.el"
                     "emacs-lisp/international/iso-transl.el"
                     "emacs-lisp/case-table.el"
                     "emacs-lisp/emacs-lisp/easy-mmode.el"
                     "emacs-lisp/help.el"
                     "emacs-lisp/hl-line.el"
                     "emacs-lisp/skeleton.el"
                     "emacs-lisp/so-long.el"
                     "emacs-lisp/keymap.el"
                     "emacs-lisp/wid-edit.el"
                     "emacs-lisp/cus-face.el"
                     "emacs-lisp/term.el"
                     "emacs-lisp/epa-hook.el"
                     "emacs-lisp/bindings.el"
                     "emacs-lisp/files.el"
                     "emacs-lisp/menu-bar.el"
                     "emacs-lisp/international/ja-dic-utl.el"
                     "emacs-lisp/eshell/eshell.el"
                     "emacs-lisp/eshell/em-basic.el"
                     "emacs-lisp/progmodes/cc-fonts.elc"
                     "emacs-lisp/calc/calc.el"))
    (when (and (equal library "emacs-lisp/menu-bar.el")
               (not (featurep 'menu-bar)))
      ;; Earlier bootstrap consumers use provisional maps.  GNU initializes
      ;; the complete menus with defvar, which requires unbound variables.
      (makunbound 'menu-bar-edit-menu)
      (makunbound 'menu-bar-options-menu))
    ;; Preserve the reader's native-row functions and the setenv that
    ;; shares the reader's environment store with getenv.
    (let ((pins (cond
                 ((equal library "emacs-lisp/env.el") '(getenv setenv))))
          saved)
      (dolist (name pins)
        (push (cons name (symbol-function name)) saved))
      (unwind-protect
          (condition-case err
              (nemacs-runtime-image-preload--load-source-file
               (concat repo-root "/vendor/" library))
            (error
             (message "Error preloading GNU library %s" library)
             (signal (car err) (cdr err))))
        (dolist (entry saved)
          (fset (car entry) (cdr entry)))))))

(defun tab-bar-current-tab (&optional frame)
  "Return the selected tab's parameters on FRAME."
  (cdr (tab-bar--current-tab-find nil frame)))

(defun tab-bar-current-tab-index (&optional frame)
  "Return the zero-based index of the selected tab on FRAME."
  (tab-bar--current-tab-index nil frame))

(defun nemacs-runtime-image-preload-batch (repo-root bootstrap-file)
  "Preload the batch nemacs entry into the current NeLisp image."
  (nemacs-runtime-image-setup-paths repo-root)
  (nemacs-runtime-image-load-bootstrap repo-root bootstrap-file)
  ;; A frozen reader can retain its early cl-pushnew after bundle replay.
  ;; Use the resolved source loader to publish cl-lib in that case.
  (let ((source (concat repo-root "/vendor/emacs-lisp/emacs-lisp/cl-lib.el")))
    (unless (equal (symbol-file 'cl-pushnew 'defun) source)
      (nemacs-runtime-image-preload--load-source-file source)))
  (nemacs-runtime-image-preload--install-process-core)
  (require 'nemacs-main)
  (load (concat repo-root "/scripts/nemacs-runtime-frame-tab-preload.el")
        nil 'no-message t t)
  (nemacs-runtime-image-preload--install-daily-driver-core)
  ;; GNU's loadup queues Custom declarations whose setters may call modes
  ;; defined later in the same file. Finish that queue once all libraries
  ;; are defined, before this batch image starts accepting user calls.
  (let ((custom-delayed-init-variables nil))
    (nemacs-runtime-image-preload--load-gnu-libraries repo-root)
    (let ((settings (nreverse custom-delayed-init-variables))
          (custom-delayed-init-variables t))
      (mapc #'custom-reevaluate-setting settings)))
  ;; Publish the default from GNU newcomment.el after the reader's early
  ;; buffer-local variable initialization has supplied a provisional value.
  (custom-reevaluate-setting 'comment-column)
  (advice-add 'substitute-command-keys :around
              #'emacs-keymap-builtins--substitute-command-keys)
  ;; Avoid walking the expanded native Unicode keymap on the first Help lookup.
  (let ((slot (emacs-keymap-full-slot (current-global-map))))
    (when slot
      (emacs-keymap-builtins--where-is-char-table #'ignore slot)))
  (nemacs-runtime-image-preload--reassert-process-bridge)
  (nemacs-runtime-image-preload--retry-stale-fx)
  ;; The reconvert compiles can record a fresh `none' behind them; one more
  ;; bounded pass (itself at most 4 rounds) repairs those.
  (when (nemacs-runtime-image-preload--reconvert-native-rows)
    (nemacs-runtime-image-preload--retry-stale-fx))
  t)

(defun nemacs-runtime-image-preload--retry-stale-fx-pass ()
  "One pass over every closure function; answer how many it repaired.
A `none' entry is compiled again; an entry absent from the table and not
queued for compilation is queued."
  (let ((recovered 0))
    (dolist (sym (nelisp--intern-table-symbols))
      (condition-case nil
          (when (fboundp sym)
            (let* ((fn (symbol-function sym))
                   (cell (and (nelisp--closure-p fn) (not (subrp fn))
                              (nelisp--closure-cons fn))))
              (when (and (consp cell) (eq (car cell) 'closure))
                (let* ((body (nthcdr 3 cell))
                       (state (gethash body nelisp--fx-table 'nemacs--fxr-absent)))
                  (cond
                   ((eq state 'none)
                    (nelisp--fx-register-fn sym fn t)
                    (when (vectorp (gethash body nelisp--fx-table))
                      (setq recovered (1+ recovered))))
                   ;; Never compiled and not queued either: its first call
                   ;; would record `none'.  Queue it so that call compiles.
                   ((and (eq state 'nemacs--fxr-absent)
                         (fboundp 'nelisp--fx-defer)
                         (not (gethash body nelisp--fx-pending)))
                    (nelisp--fx-defer sym)
                    (when (gethash body nelisp--fx-pending)
                      (setq recovered (1+ recovered)))))))))
        (error nil)))
    recovered))

(defun nemacs-runtime-image-preload--retry-stale-fx ()
  "Repair stale word-VM entries of named functions.
`nelisp--fx-first-call' ends by recording `none' for a body it neither
queued nor compiled (a function first called before the compile machinery
was ready, or defined after the registration hooks and first called while a
precondition failed), and nothing retries it: the function then runs in the
interpreter for the whole life of the image, while an anonymous clone of the
same body compiles and runs 1.4x faster (closure gap, state-perf F210).  A
runtime-image replay never runs `nelisp--fx-warm-for-dump', so it kept ~200
such entries (forward-word 1.6x slower than the flat dump, F212).  A body
never compiled and not queued (`nelisp--fx-pending') gets the same `none' at
its first call, so it is queued instead.  Force one registration per `none'
function; bodies the VM still declines stay `none'.  The compiler applies helper functions while it runs, which can
record a fresh `none' behind the pass, so repeat (at most 4 passes) until a
pass recovers nothing.  Answer the total recovered, or nil off the NeLisp
runtime."
  (when (and (fboundp 'nelisp--intern-table-symbols)
             (fboundp 'nelisp--fx-register-fn)
             (fboundp 'nelisp--closure-cons)
             (boundp 'nelisp--fx-table)
             (hash-table-p nelisp--fx-table))
    (let ((total 0) (pass 0) (n 1))
      (while (and (> n 0) (< pass 4))
        (setq n (nemacs-runtime-image-preload--retry-stale-fx-pass)
              total (+ total n)
              pass (1+ pass)))
      total)))

(defun nemacs-runtime-image-preload--tree-mentions-p (tree names)
  "Non-nil when the cons TREE contains a symbol of NAMES.  Iterative walk."
  (let ((todo (list tree)) (found nil))
    (while (and todo (not found))
      (let ((x (pop todo)))
        (cond
         ((consp x)
          (while (consp x)
            (if (consp (car x)) (push (car x) todo)
              (when (memq (car x) names) (setq found t)))
            (setq x (cdr x)))
          (when (and x (symbolp x) (memq x names)) (setq found t)))
         ((and x (symbolp x) (memq x names)) (setq found t)))))
    found))

;; F420: a row is wanted when its CURRENT closure has no native record, whatever
;; `nelisp-nc--converted' says (the bootstrap re-emits stdlib functions, so a name
;; can stay listed as converted while its new closure runs on the word VM).
(defun nemacs-runtime-image-preload--row-needs-convert-p (name)
  "Non-nil when NAME's current closure lacks a native record.
Record slot 1 <= -16 is native; any other slot 1 or a missing or non-vector
record is not.  A function that is not a closure is left alone (nil)."
  (let ((cell (ignore-errors (nelisp--closure-cons (symbol-function name)))))
    (when (eq (car-safe cell) 'closure)
      (let ((rec (gethash (nthcdr 3 cell) nelisp--fx-table)))
        (not (and (vectorp rec) (integerp (aref rec 1)) (<= (aref rec 1) -16)))))))

(defun nemacs-runtime-image-preload--reconvert-row (name)
  "Compile NAME again from its closure and convert it; non-nil when converted.
The record NAME had (fx table, pending entry, converted membership) is
restored when the new registration fails or leaves a non-vector record, so a
failed attempt never demotes a row that was fine before."
  (let* ((cell (nelisp--closure-cons (symbol-function name))))
    (when (eq (car-safe cell) 'closure)
      (let* ((body (nthcdr 3 cell))
             (absent (make-symbol "absent"))
             (old (gethash body nelisp--fx-table absent))
             (old-pending (if (hash-table-p nelisp--fx-pending)
                              (gethash body nelisp--fx-pending absent)
                            absent))
             (was-converted (and (memq name nelisp-nc--converted) t))
             (ok nil))
        (remhash body nelisp--fx-table)
        (when (hash-table-p nelisp--fx-pending)
          (remhash body nelisp--fx-pending))
        ;; A stale membership must not hide the retry; restored below on failure.
        (when was-converted
          (setq nelisp-nc--converted (delq name nelisp-nc--converted)))
        (condition-case nil
            (progn
              (nelisp--fx-register name)
              ;; The install-tail wrapper converts on registration; convert
              ;; here too in case it is not installed.
              (unless (memq name nelisp-nc--converted)
                (nelisp-nc--convert name (symbol-function name)))
              (setq ok (and (memq name nelisp-nc--converted)
                            (vectorp (gethash body nelisp--fx-table))
                            t)))
          (error nil))
        (unless ok
          (if (eq old absent)
              (remhash body nelisp--fx-table)
            (puthash body old nelisp--fx-table))
          (when (and (hash-table-p nelisp--fx-pending)
                     (not (eq old-pending absent)))
            (puthash body old-pending nelisp--fx-pending))
          (when (and was-converted (not (memq name nelisp-nc--converted)))
            (push name nelisp-nc--converted)))
        ok))))

(defun nemacs-runtime-image-preload--reconvert-native-rows ()
  "Re-register every native-row function whose current record is not native; answer how many.
A row function is compiled for the word VM when it is first called, which
for `nconc' happens during the prelude load, before `cddr' is defined: its
code then holds `cddr' as a closure call, `nelisp-nc--code-same-p' rejects
the native row, and nothing converts it again once `cddr' exists, so it
stays on the word VM for the whole life of the image (F284: nconc 0.8 us
native vs 2.0 us VM, string-clean-whitespace 1.9x slower).  Drop the stale
table and pending entries, compile the function again now that its callees
exist, and convert it.  A converted row whose body mentions a function just
converted (`mapcan' applies `nconc') is compiled and converted once more so
that it links to the new record (mapcan stayed at 4.5 us, 2.3 us after).
One pass over `nelisp-nc--table' plus one over the dependents, no loop at
call time; a row the compiler or the fxir match still declines stays as it
was.  Answer the number of rows converted, or nil off the NeLisp runtime."
  (when (and (boundp 'nelisp-nc--table) nelisp-nc--table
             (boundp 'nelisp-nc--converted)
             (fboundp 'nelisp-nc--convert)
             (fboundp 'nelisp--fx-register)
             (fboundp 'nelisp--closure-cons)
             (boundp 'nelisp--fx-table)
             (hash-table-p nelisp--fx-table))
    (let ((fixed nil) (n 0))
      (dolist (row nelisp-nc--table)
        (let ((name (car row)))
          (when (and (symbolp name) (not (memq :mutant row))
                     (fboundp name)
                     (nemacs-runtime-image-preload--row-needs-convert-p name))
            (condition-case nil
                (when (nemacs-runtime-image-preload--reconvert-row name)
                  (push name fixed)
                  (setq n (1+ n)))
              (error nil)))))
      (when fixed
        (dolist (row nelisp-nc--table)
          (let ((name (car row)))
            (when (and (symbolp name) (not (memq :mutant row))
                       (fboundp name) (memq name nelisp-nc--converted)
                       (not (memq name fixed)))
              (condition-case nil
                  (let ((cell (nelisp--closure-cons (symbol-function name))))
                    (when (and (eq (car-safe cell) 'closure)
                               (nemacs-runtime-image-preload--tree-mentions-p
                                (nthcdr 3 cell) fixed))
                      (nemacs-runtime-image-preload--reconvert-row name)))
                (error nil))))))
      n)))

(defun nemacs-runtime-image-preload--reassert-process-bridge ()
  "Re-point the public process entry points at the `emacs-process' facade.
Later vendor/prelude loads can replace `shell-command-to-string' with the
GNU definition, which needs a buffer-valued `standard-output' that NeLisp's
`with-output-to-string' does not provide.  The facade versions capture
output into ec-buffers correctly."
  (dolist (pair '((shell-command-to-string
                   . emacs-process-shell-command-to-string)
                  (call-process . emacs-process-call-process)
                  (call-process-region . emacs-process-call-process-region)
                  (process-file . emacs-process-process-file)))
    (when (fboundp (cdr pair))
      (fset (car pair) (symbol-function (cdr pair))))))

(defun nemacs-runtime-image-preload-interactive (repo-root bootstrap-file)
  "Preload the interactive TUI entry into the current NeLisp image."
  (nemacs-runtime-image-preload-batch repo-root bootstrap-file)
  (require 'emacs-tui-backend)
  (require 'emacs-tui-event)
  (require 'emacs-redisplay-core)
  (nemacs-main--init-keymap)
  (nemacs-main--prepare-tui-state)
  t)

(defun nemacs-runtime-image-preload-vendor-core (repo-root bootstrap-file)
  "Preload the daily-driver vendor core lane into the current image.

This currently bakes lightweight daily-driver feature surfaces after
the Layer-2 substrate is loaded.  It is intentionally separate from
the default batch image while the vendor surface is still expanding."
  (nemacs-runtime-image-preload-batch repo-root bootstrap-file)
  (nemacs-runtime-image-preload-vendor-core-extension)
  t)

(defun nemacs-runtime-image-preload-vendor-core-extension ()
  "Extend an already-baked base runtime image with vendor core modules."
  (nemacs-runtime-image-preload--install-daily-driver-core))

(defun nemacs-runtime-image-preload--install-daily-driver-core ()
  "Install daily-driver feature facades into the current runtime image."
  (nemacs-runtime-image-preload--install-help-core)
  (nemacs-runtime-image-preload--install-ielm-core)
  (nemacs-runtime-image-preload--install-minibuffer-core)
  (nemacs-runtime-image-preload--install-dev-nav-core)
  (nemacs-runtime-image-preload--install-shell-core)
  (nemacs-runtime-image-preload--install-process-core)
  (nemacs-runtime-image-preload--install-frame-core)
  (nemacs-runtime-image-preload--install-tab-core)
  (nemacs-runtime-image-preload--install-support-core)
  (require 'charprop)
  t)

(defun nemacs-runtime-image-preload--install-file-command (public target)
  "Install PUBLIC as a lazy wrapper around TARGET.
The wrapper is quoted data instead of a closure so source-v1 images can
replay it in runtimes with minimal closure support."
  (unless (nemacs-runtime-image-preload--function-cell-live-p public)
    (fset public
          (list 'lambda '(&rest args)
                '(require 'files-standalone-buffer)
                (list 'apply (list 'quote target) 'args)))))

(defun nemacs-runtime-image-preload--function-cell-live-p (symbol)
  "Return non-nil when SYMBOL has a usable function cell."
  (and (fboundp symbol)
       (condition-case nil
           (symbol-function symbol)
         (error nil))))

(defun nemacs-runtime-image-preload--install-help-command (public)
  "Install PUBLIC as a lazy wrapper around `emacs-help'."
  (unless (nemacs-runtime-image-preload--function-cell-live-p public)
    (fset public
          (list 'lambda '(&rest args)
                '(require 'emacs-help)
                (list 'apply (list 'quote public) 'args)))))

(defun nemacs-runtime-image-preload--install-help-core ()
  "Install the remaining help command facade."
  ;; `describe-function', `describe-variable' and `describe-symbol' are GNU
  ;; help-fns.el's (autoloaded from loaddefs); only `describe-key' is still
  ;; rendered by `emacs-help'.
  (nemacs-runtime-image-preload--install-help-command 'describe-key)
  t)

(defun nemacs-runtime-image-preload--install-module-command
    (public feature target)
  "Install PUBLIC as a lazy wrapper requiring FEATURE and calling TARGET."
  (unless (nemacs-runtime-image-preload--function-cell-live-p public)
    (fset public
          (list 'lambda '(&rest args)
                (list 'require (list 'quote feature))
                (list 'apply (list 'quote target) 'args)))))

(defun nemacs-runtime-image-preload--install-ielm-core ()
  "Leave `ielm' to GNU ielm.el, autoloaded from loaddefs.
The former wrappers pointed `ielm' and `ielm-send-input' at the
`emacs-ielm' module and provided a fake `ielm' feature."
  t)

(defun nemacs-runtime-image-preload--install-minibuffer-core ()
  "Install the minimal `minibuffer' daily-driver surface."
  (unless (featurep 'minibuffer)
    (nemacs-runtime-image-preload--install-module-command
     'completing-read 'emacs-minibuffer-builtins 'completing-read)
    (unless (fboundp 'minibuffer-complete)
      (fset 'minibuffer-complete '(lambda () nil)))
    (unless (fboundp 'minibuffer-complete-and-exit)
      (fset 'minibuffer-complete-and-exit
            '(lambda ()
               (require 'emacs-minibuffer-builtins)
               (exit-minibuffer))))
    (provide 'minibuffer))
  t)

(defun nemacs-runtime-image-preload--install-dev-nav-core ()
  "Install lazy entry points for the NeLisp navigation modules."
  (nemacs-runtime-image-preload--install-module-command
   'emacs-imenu-create-index 'emacs-imenu 'emacs-imenu-create-index)
  (nemacs-runtime-image-preload--install-module-command
   'emacs-imenu 'emacs-imenu 'emacs-imenu)
  ;; `xref-find-definitions' and friends are GNU xref.el's (autoloaded
  ;; from loaddefs); only the emacs-xref names are wrapped here.
  (unless (featurep 'xref)
    (nemacs-runtime-image-preload--install-module-command
     'emacs-xref-find-definitions
     'emacs-xref
     'emacs-xref-find-definitions)
    (nemacs-runtime-image-preload--install-module-command
     'emacs-xref-pop-marker-stack
     'emacs-xref
     'emacs-xref-pop-marker-stack))
  (nemacs-runtime-image-preload--install-module-command
   'emacs-compile-run 'emacs-compile 'emacs-compile-run)
  (nemacs-runtime-image-preload--install-module-command
   'emacs-compile-errors 'emacs-compile 'emacs-compile-errors)
  (nemacs-runtime-image-preload--install-module-command
   'emacs-compile-next-error 'emacs-compile 'emacs-compile-next-error)
  (nemacs-runtime-image-preload--install-module-command
   'emacs-vc-status 'emacs-vc 'emacs-vc-status)
  (nemacs-runtime-image-preload--install-module-command
   'emacs-vc-diff 'emacs-vc 'emacs-vc-diff)
  (nemacs-runtime-image-preload--install-module-command
   'emacs-vc-print-log 'emacs-vc 'emacs-vc-print-log)
  (nemacs-runtime-image-preload--install-module-command
   'emacs-vc-dir 'emacs-vc 'emacs-vc-dir)
  (nemacs-runtime-image-preload--install-module-command
   'emacs-vc-annotate 'emacs-vc 'emacs-vc-annotate)
  (nemacs-runtime-image-preload--install-module-command
   'emacs-vc-revision-diff 'emacs-vc 'emacs-vc-revision-diff)
  t)

(defun nemacs-runtime-image-preload--install-shell-core ()
  "Install the prefixed shell entry points."
  (unless (featurep 'emacs-symbol)
    (require 'emacs-symbol))
  ;; `shell' and `shell-mode' are GNU shell.el's (autoloaded from
  ;; loaddefs); only the emacs-shell names are wrapped here.
  (unless (featurep 'shell)
    (unless (boundp 'emacs-shell-buffer-name)
      (defvar emacs-shell-buffer-name "*shell*"))
    (nemacs-runtime-image-preload--install-module-command
     'emacs-shell 'emacs-shell 'emacs-shell)
    (nemacs-runtime-image-preload--install-module-command
     'emacs-shell-mode 'emacs-shell 'emacs-shell-mode)
    (nemacs-runtime-image-preload--install-module-command
     'emacs-shell-send-input 'emacs-shell 'emacs-shell-send-input))
  t)

(defun nemacs-runtime-image-preload--install-process-core ()
  "Install process API facades without source `load'.

The source-v1 runtime image records `setq' and `fset' reliably, while
full source `defun' forms may not replay into a fresh standalone reader.
Keep this surface as data lambdas so runtime images expose the
`emacs-process' boundary even before NeLisp's real subprocess primitives are
bound.  When `nelisp-process-*' or legacy `nelisp-*' delegates exist they are
used; otherwise synchronous calls return a failure status instead of aborting
image startup."
  (let* ((base (and (boundp
                     'nemacs-runtime-image-preload--script-directory)
                    nemacs-runtime-image-preload--script-directory))
         (preload (and base
                       (expand-file-name
                        "nemacs-runtime-process-preload.el" base))))
    (when (and preload (file-readable-p preload)
               ;; The bootstrap bundle's implementation captures process
               ;; output through the async NeLisp primitives.  Do not replay
               ;; the weaker pre-bootstrap fallback over that surface.
               (not (and (fboundp 'emacs-process-call-process)
                         (fboundp 'emacs-process-call-process-region)
                         (fboundp
                          'emacs-process--standalone-call-process)))
               ;; In host Emacs the native subr `make-process' already
               ;; exists; loading the source-v1 preload would clobber it
               ;; (and the other unprefixed process primitives) with wrapper
               ;; lambdas that re-enter `emacs-process-*' and recurse.  Only
               ;; load the preload when there is no native subr to preserve
               ;; (= a real standalone reader / runtime image).
               (not (and (fboundp 'make-process)
                         (fboundp 'subrp)
                         (subrp (symbol-function 'make-process)))))
      (load preload nil 'no-message)))
  (unless (boundp 'shell-file-name)
    (defvar shell-file-name "/bin/sh"))
  (unless (boundp 'shell-command-switch)
    (defvar shell-command-switch "-c"))
  (unless (boundp 'emacs-process-shell-file-name)
    (defvar emacs-process-shell-file-name "/bin/sh"))
  (unless (boundp 'emacs-process-shell-command-switch)
    (defvar emacs-process-shell-command-switch "-c"))
  (unless (boundp 'emacs-process-call-process-region-input-file)
    (defvar emacs-process-call-process-region-input-file
      "/tmp/nemacs-call-process-region-input"))
  (unless (boundp 'emacs-process-shell-command-on-region-output-file)
    (defvar emacs-process-shell-command-on-region-output-file
      "/tmp/nemacs-shell-command-on-region-output"))
  (unless (boundp 'emacs-process--fallback-tag)
    (defvar emacs-process--fallback-tag 'emacs-process-fallback))
  (unless (boundp 'emacs-process--fallback-processes)
    (defvar emacs-process--fallback-processes nil))
  (unless (boundp 'emacs-process--fallback-next-pid)
    (defvar emacs-process--fallback-next-pid 10000))
  (unless (fboundp 'emacs-process--fallback-process-p)
    (fset 'emacs-process--fallback-process-p
          '(lambda (object)
             (if (vectorp object)
                 (if (<= 10 (length object))
                     (eq (aref object 0) emacs-process--fallback-tag)
                   nil)
               nil))))
  (unless (fboundp 'emacs-process--fallback-plist-get)
    (fset 'emacs-process--fallback-plist-get
          '(lambda (plist prop)
             (let ((value nil)
                   (found nil))
               (while (if plist (not found) nil)
                 (if (eq (car plist) prop)
                     (progn
                       (setq value (car (cdr plist)))
                       (setq found t))
                   (setq plist (cdr (cdr plist)))))
               value))))
  (unless (fboundp 'emacs-process--fallback-buffer)
    (fset 'emacs-process--fallback-buffer
          '(lambda (buffer)
             (if (if (stringp buffer) (fboundp 'get-buffer-create) nil)
                 (get-buffer-create buffer)
               buffer))))
  (unless (fboundp 'emacs-process--fallback-sentinel-event)
    (fset 'emacs-process--fallback-sentinel-event
          '(lambda (status)
             (if (if (integerp status) (= status 0) nil)
                 "finished\n"
               (concat "exited abnormally with code "
                       (number-to-string status)
                       "\n")))))
  (unless (fboundp 'emacs-process--fallback-make-process)
    (fset 'emacs-process--fallback-make-process
          '(lambda (&rest plist)
             (let ((name (or (emacs-process--fallback-plist-get plist :name)
                             "process"))
                   (buffer (emacs-process--fallback-buffer
                            (emacs-process--fallback-plist-get plist :buffer)))
                   (command (emacs-process--fallback-plist-get plist :command))
                   (sentinel (emacs-process--fallback-plist-get
                              plist :sentinel))
                   (filter (emacs-process--fallback-plist-get plist :filter))
                   (pid emacs-process--fallback-next-pid)
                   (process nil)
                   (status 1))
               (setq process
                     (vector emacs-process--fallback-tag name buffer command
                             'run nil filter sentinel nil pid))
               (setq emacs-process--fallback-next-pid
                     (+ emacs-process--fallback-next-pid 1))
               (setq emacs-process--fallback-processes
                     (cons process emacs-process--fallback-processes))
               (setq status
                     (if (if (consp command) (car command) nil)
                         (apply 'call-process
                                (car command) nil buffer nil (cdr command))
                       1))
               (aset process 4 'exit)
               (aset process 5 status)
               (if (functionp sentinel)
                   (funcall sentinel process
                            (emacs-process--fallback-sentinel-event status))
                 nil)
               process))))
  ;; Mirrors nemacs-runtime-process-preload.el: real Emacs `call-process'
  ;; runs the child in `default-directory'; the native primitive spawns in
  ;; the parent's cwd, so wrap with a `cd' shell prefix for local absolute
  ;; directories (exit 127 = shell cannot-execute convention).
  (unless (fboundp 'emacs-process--call-process-cwd)
    (fset 'emacs-process--call-process-cwd
          '(lambda ()
             (if (boundp 'default-directory)
                 (if (stringp default-directory)
                     (if (> (length default-directory) 0)
                         (if (= (aref default-directory 0) ?/)
                             default-directory
                           nil)
                       nil)
                   nil)
               nil))))
  (unless (fboundp 'emacs-process-call-process)
    (fset 'emacs-process-call-process
          '(lambda (program &optional infile destination display &rest args)
             (let ((cwd (emacs-process--call-process-cwd)))
               (if cwd
                   (progn
                     (setq args
                           (cons "-c"
                                 (cons "cd \"$1\" || exit 127; shift; exec \"$@\""
                                       (cons "emacs-process-call-process-cwd"
                                             (cons cwd (cons program args))))))
                     (setq program "/bin/sh"))
                 nil)
               (cond
                ((fboundp 'nelisp-process-call-process)
                 (apply 'nelisp-process-call-process
                        program infile destination display args))
                ((fboundp 'nelisp-call-process)
                 (apply 'nelisp-call-process
                        program infile destination display args))
                (t 1))))))
  (unless (fboundp 'call-process)
    (fset 'call-process
          '(lambda (&rest args)
             (apply 'emacs-process-call-process args))))
  (unless (fboundp 'emacs-process-call-process-region)
    (fset 'emacs-process-call-process-region
          '(lambda (start end program &optional delete destination display
                          &rest args)
             (cond
              ((fboundp 'nelisp-process-call-process-region)
               (apply 'nelisp-process-call-process-region
                      start end program delete destination display args))
              ((fboundp 'nelisp-call-process-region)
               (apply 'nelisp-call-process-region
                      start end program delete destination display args))
              ((and (fboundp 'buffer-substring-no-properties)
                    (fboundp 'nl-write-file))
               (nl-write-file
                emacs-process-call-process-region-input-file
                (buffer-substring-no-properties start end))
               (when (and delete (fboundp 'delete-region))
                 (delete-region start end))
               (apply 'call-process
                      program
                      emacs-process-call-process-region-input-file
                      destination
                      display
                      args))
              (t 1)))))
  (unless (fboundp 'call-process-region)
    (fset 'call-process-region
          '(lambda (&rest args)
             (apply 'emacs-process-call-process-region args))))
  (unless (fboundp 'emacs-process-start-process)
    (fset 'emacs-process-start-process
          '(lambda (name buffer program &rest program-args)
             (emacs-process--fallback-make-process
              :name name
              :buffer buffer
              :command (cons program program-args)))))
  (unless (fboundp 'start-process)
    (fset 'start-process
          '(lambda (&rest args)
             (apply 'emacs-process-start-process args))))
  (unless (fboundp 'emacs-process-make-process)
    (fset 'emacs-process-make-process
          '(lambda (&rest plist)
             (apply 'emacs-process--fallback-make-process plist))))
  (unless (fboundp 'make-process)
    (fset 'make-process
          '(lambda (&rest plist)
             (apply 'emacs-process-make-process plist))))
  (unless (fboundp 'emacs-process-processp)
    (fset 'emacs-process-processp
          '(lambda (object)
             (emacs-process--fallback-process-p object))))
  (unless (fboundp 'processp)
    (fset 'processp
          '(lambda (object)
             (emacs-process-processp object))))
  (unless (fboundp 'emacs-process-process-list)
    (fset 'emacs-process-process-list
          '(lambda ()
             (let ((items emacs-process--fallback-processes)
                   (result nil))
               (while items
                 (if (aref (car items) 8)
                     nil
                   (setq result (cons (car items) result)))
                 (setq items (cdr items)))
               result))))
  (unless (fboundp 'process-list)
    (fset 'process-list
          '(lambda ()
             (emacs-process-process-list))))
  (unless (fboundp 'emacs-process-process-status)
    (fset 'emacs-process-process-status
          '(lambda (process)
             (if (emacs-process--fallback-process-p process)
                 (aref process 4)
               nil))))
  (unless (fboundp 'process-status)
    (fset 'process-status
          '(lambda (process)
             (emacs-process-process-status process))))
  (unless (fboundp 'emacs-process-process-exit-status)
    (fset 'emacs-process-process-exit-status
          '(lambda (process)
             (if (emacs-process--fallback-process-p process)
                 (or (aref process 5) 0)
               0))))
  (unless (fboundp 'process-exit-status)
    (fset 'process-exit-status
          '(lambda (process)
             (emacs-process-process-exit-status process))))
  (unless (fboundp 'emacs-process-process-buffer)
    (fset 'emacs-process-process-buffer
          '(lambda (process)
             (if (emacs-process--fallback-process-p process)
                 (aref process 2)
               nil))))
  (unless (fboundp 'process-buffer)
    (fset 'process-buffer
          '(lambda (process)
             (emacs-process-process-buffer process))))
  (unless (fboundp 'emacs-process-process-name)
    (fset 'emacs-process-process-name
          '(lambda (process)
             (if (emacs-process--fallback-process-p process)
                 (aref process 1)
               ""))))
  (unless (fboundp 'process-name)
    (fset 'process-name
          '(lambda (process)
             (emacs-process-process-name process))))
  (unless (fboundp 'emacs-process-process-command)
    (fset 'emacs-process-process-command
          '(lambda (process)
             (if (emacs-process--fallback-process-p process)
                 (aref process 3)
               nil))))
  (unless (fboundp 'process-command)
    (fset 'process-command
          '(lambda (process)
             (emacs-process-process-command process))))
  (unless (fboundp 'emacs-process-process-id)
    (fset 'emacs-process-process-id
          '(lambda (process)
             (if (emacs-process--fallback-process-p process)
                 (aref process 9)
               nil))))
  (unless (fboundp 'process-id)
    (fset 'process-id
          '(lambda (process)
             (emacs-process-process-id process))))
  (unless (fboundp 'emacs-process-process-mark)
    (fset 'emacs-process-process-mark
          '(lambda (process)
             nil)))
  (unless (fboundp 'process-mark)
    (fset 'process-mark
          '(lambda (process)
             (emacs-process-process-mark process))))
  (unless (fboundp 'emacs-process-set-process-filter)
    (fset 'emacs-process-set-process-filter
          '(lambda (process filter)
             (if (emacs-process--fallback-process-p process)
                 (progn
                   (aset process 6 filter)
                   filter)
               nil))))
  (unless (fboundp 'set-process-filter)
    (fset 'set-process-filter
          '(lambda (process filter)
             (emacs-process-set-process-filter process filter))))
  (unless (fboundp 'emacs-process-set-process-sentinel)
    (fset 'emacs-process-set-process-sentinel
          '(lambda (process sentinel)
             (if (emacs-process--fallback-process-p process)
                 (progn
                   (aset process 7 sentinel)
                   sentinel)
               nil))))
  (unless (fboundp 'set-process-sentinel)
    (fset 'set-process-sentinel
          '(lambda (process sentinel)
             (emacs-process-set-process-sentinel process sentinel))))
  (unless (fboundp 'emacs-process-accept-process-output)
    (fset 'emacs-process-accept-process-output
          '(lambda (&optional process seconds millisec just-this-one)
             nil)))
  (unless (fboundp 'accept-process-output)
    (fset 'accept-process-output
          '(lambda (&optional process seconds millisec just-this-one)
             (emacs-process-accept-process-output
              process seconds millisec just-this-one))))
  (unless (fboundp 'emacs-process-signal-process)
    (fset 'emacs-process-signal-process
          '(lambda (process-or-pid signum)
             (if (emacs-process--fallback-process-p process-or-pid)
                 (progn
                   (aset process-or-pid 4 'signal)
                   (aset process-or-pid 5 1)
                   process-or-pid)
               nil))))
  (unless (fboundp 'signal-process)
    (fset 'signal-process
          '(lambda (process-or-pid signum)
             (emacs-process-signal-process process-or-pid signum))))
  (unless (fboundp 'emacs-process-kill-process)
    (fset 'emacs-process-kill-process
          '(lambda (process)
             (signal-process process 'KILL))))
  (unless (fboundp 'kill-process)
    (fset 'kill-process
          '(lambda (process)
             (emacs-process-kill-process process))))
  (unless (fboundp 'emacs-process-process-send-string)
    (fset 'emacs-process-process-send-string
          '(lambda (process string)
             nil)))
  (unless (fboundp 'process-send-string)
    (fset 'process-send-string
          '(lambda (process string)
             (emacs-process-process-send-string process string))))
  (unless (fboundp 'emacs-process-process-send-eof)
    (fset 'emacs-process-process-send-eof
          '(lambda (&optional process)
             nil)))
  (unless (fboundp 'process-send-eof)
    (fset 'process-send-eof
          '(lambda (&optional process)
             (emacs-process-process-send-eof process))))
  (unless (fboundp 'emacs-process-delete-process)
    (fset 'emacs-process-delete-process
          '(lambda (process)
             (if (emacs-process--fallback-process-p process)
                 (progn
                   (aset process 8 t)
                   process)
               nil))))
  (unless (fboundp 'delete-process)
    (fset 'delete-process
          '(lambda (process)
             (emacs-process-delete-process process))))
  (unless (fboundp 'emacs-process-shell-command)
    (fset 'emacs-process-shell-command
          '(lambda (command &optional output-buffer error-buffer)
             (call-process emacs-process-shell-file-name
                           nil
                           (if output-buffer output-buffer t)
                           nil
                           emacs-process-shell-command-switch
                           command))))
  (unless (fboundp 'shell-command)
    (fset 'shell-command
          '(lambda (command &optional output-buffer error-buffer)
             (emacs-process-shell-command
              command output-buffer error-buffer))))
  (unless (fboundp 'emacs-process-shell-command-on-region)
    (fset 'emacs-process-shell-command-on-region
          '(lambda (start end command &optional output-buffer replace-flag
                          error-buffer display-error-buffer
                          region-noncontiguous-p)
             (let ((destination
                    (if replace-flag
                        emacs-process-shell-command-on-region-output-file
                      (if output-buffer
                          output-buffer
                        emacs-process-shell-command-on-region-output-file)))
                   (status 1))
               (when (fboundp 'nl-write-file)
                 (nl-write-file destination ""))
               (setq status
                     (call-process-region
                      start end emacs-process-shell-file-name
                      nil destination nil
                      emacs-process-shell-command-switch command))
               (when (and replace-flag
                          (fboundp 'delete-region)
                          (fboundp 'insert))
                 (let ((text (if (fboundp 'rdf) (rdf destination) "")))
                   (delete-region start end)
                   (insert text)))
               status))))
  (unless (fboundp 'shell-command-on-region)
    (fset 'shell-command-on-region
          '(lambda (start end command &optional output-buffer replace-flag
                          error-buffer display-error-buffer
                          region-noncontiguous-p)
             (emacs-process-shell-command-on-region
              start end command output-buffer replace-flag error-buffer
              display-error-buffer region-noncontiguous-p))))
  (unless (fboundp 'emacs-process-async-shell-command)
    (fset 'emacs-process-async-shell-command
          '(lambda (command &optional output-buffer error-buffer)
             (make-process
              :name (concat "async-shell-command<" command ">")
              :buffer (or output-buffer "*Async Shell Command*")
              :command (list emacs-process-shell-file-name
                             emacs-process-shell-command-switch
                             command)))))
  (unless (fboundp 'async-shell-command)
    (fset 'async-shell-command
          '(lambda (command &optional output-buffer error-buffer)
             (emacs-process-async-shell-command
              command output-buffer error-buffer))))
  (unless (fboundp 'emacs-process-shell-command-to-string)
    (fset 'emacs-process-shell-command-to-string
          '(lambda (command)
             (if (fboundp 'with-temp-buffer)
                 (with-temp-buffer
                   (call-process emacs-process-shell-file-name
                                 nil t nil
                                 emacs-process-shell-command-switch
                                 command)
                   (buffer-string))
               ""))))
  (unless (fboundp 'shell-command-to-string)
    (fset 'shell-command-to-string
          '(lambda (command)
             (emacs-process-shell-command-to-string command))))
  (provide 'emacs-process)
  (provide 'emacs-process-builtins)
  t)

(defun nemacs-runtime-image-preload--install-frame-core ()
  "Install the minimal frame daily-driver surface without source `load'."
  (unless (boundp 'emacs-frame--runtime-id-counter)
    (defvar emacs-frame--runtime-id-counter 0))
  (unless (boundp 'emacs-frame--runtime-registry)
    (defvar emacs-frame--runtime-registry nil))
  (unless (boundp 'emacs-frame--runtime-selected-frame)
    (defvar emacs-frame--runtime-selected-frame nil))
  (unless (fboundp 'emacs-frame--runtime-cell)
    (fset 'emacs-frame--runtime-cell
          '(lambda (frame key)
             (assq key frame))))
  (unless (fboundp 'emacs-frame--runtime-ref)
    (fset 'emacs-frame--runtime-ref
          '(lambda (frame key)
             (cdr (emacs-frame--runtime-cell frame key)))))
  (unless (fboundp 'emacs-frame--runtime-set)
    (fset 'emacs-frame--runtime-set
          '(lambda (frame key value)
             (let ((cell (emacs-frame--runtime-cell frame key)))
               (if cell
                   (setcdr cell value)
                 (setq frame (cons (cons key value) frame)))
               value))))
  (unless (fboundp 'emacs-frame--runtime-frame-object-p)
    (fset 'emacs-frame--runtime-frame-object-p
          '(lambda (object)
             (and (consp object)
                  (assq 'nemacs-frame object)))))
  (unless (fboundp 'emacs-frame--runtime-live-p)
    (fset 'emacs-frame--runtime-live-p
          '(lambda (object)
             (and (emacs-frame--runtime-frame-object-p object)
                  (not (emacs-frame--runtime-ref object 'dead))))))
  (unless (fboundp 'emacs-frame--runtime-make-frame-object)
    (fset 'emacs-frame--runtime-make-frame-object
          '(lambda (&optional parameters)
             (setq emacs-frame--runtime-id-counter
                   (+ emacs-frame--runtime-id-counter 1))
             (let* ((id emacs-frame--runtime-id-counter)
                    (frame (list (cons 'nemacs-frame t)
                                 (cons 'id id)
                                 (cons 'backend 'stub)
                                 (cons 'name (concat "F" (number-to-string id)))
                                 (cons 'width 80)
                                 (cons 'height 24)
                                 (cons 'pixel-width 640)
                                 (cons 'pixel-height 384)
                                 (cons 'left 0)
                                 (cons 'top 0)
                                 (cons 'visible t)
                                 (cons 'parameters nil)
                                 (cons 'dead nil))))
               (when parameters
                 (emacs-frame--runtime-apply-parameters frame parameters))
               frame))))
  (unless (fboundp 'emacs-frame--runtime-ensure-initial)
    (fset 'emacs-frame--runtime-ensure-initial
          '(lambda ()
             (unless (emacs-frame--runtime-live-p
                      emacs-frame--runtime-selected-frame)
               (let ((frame (emacs-frame--runtime-make-frame-object nil)))
                 (setq emacs-frame--runtime-registry (list frame))
                 (setq emacs-frame--runtime-selected-frame frame)))
             emacs-frame--runtime-selected-frame)))
  (unless (fboundp 'emacs-frame--runtime-get)
    (fset 'emacs-frame--runtime-get
          '(lambda (&optional frame)
             (let ((target (if frame
                               frame
                             (emacs-frame--runtime-ensure-initial))))
               (if (emacs-frame--runtime-live-p target)
                   target
                 (emacs-frame--runtime-ensure-initial))))))
  (unless (fboundp 'emacs-frame--runtime-put-parameter)
    (fset 'emacs-frame--runtime-put-parameter
          '(lambda (frame key value)
             (let* ((pcell (emacs-frame--runtime-cell frame 'parameters))
                    (plist (cdr pcell))
                    (existing (assq key plist)))
               (if existing
                   (setcdr existing value)
                 (setcdr pcell (cons (cons key value) plist)))
               value))))
  (unless (fboundp 'emacs-frame--runtime-apply-parameters)
    (fset 'emacs-frame--runtime-apply-parameters
          '(lambda (frame parameters)
             (dolist (pair parameters)
               (let ((key (car pair))
                     (value (cdr pair)))
                 (cond
                  ((eq key 'width)
                   (emacs-frame--runtime-set frame 'width value)
                   (emacs-frame--runtime-set frame 'pixel-width (* value 8)))
                  ((eq key 'height)
                   (emacs-frame--runtime-set frame 'height value)
                   (emacs-frame--runtime-set frame 'pixel-height (* value 16)))
                  ((eq key 'left)
                   (emacs-frame--runtime-set frame 'left value))
                  ((eq key 'top)
                   (emacs-frame--runtime-set frame 'top value))
                  ((eq key 'name)
                   (emacs-frame--runtime-set frame 'name value))
                  ((eq key 'visibility)
                   (emacs-frame--runtime-set frame 'visible value)))
                 (emacs-frame--runtime-put-parameter frame key value)))
             frame)))
  (unless (fboundp 'emacs-frame-reset)
    (fset 'emacs-frame-reset
          '(lambda ()
             (setq emacs-frame--runtime-id-counter 0)
             (setq emacs-frame--runtime-registry nil)
             (setq emacs-frame--runtime-selected-frame nil)
             nil)))
  (unless (fboundp 'framep)
    (fset 'framep
          '(lambda (object)
             (and (emacs-frame--runtime-frame-object-p object)
                  (emacs-frame--runtime-ref object 'backend)))))
  (unless (fboundp 'frame-live-p)
    (fset 'frame-live-p
          '(lambda (object)
             (and (emacs-frame--runtime-live-p object)
                  (emacs-frame--runtime-ref object 'backend)))))
  (unless (fboundp 'selected-frame)
    (fset 'selected-frame
          '(lambda ()
             (emacs-frame--runtime-ensure-initial))))
  (unless (fboundp 'frame-list)
    (fset 'frame-list
          '(lambda ()
             (emacs-frame--runtime-ensure-initial)
             (let ((live nil))
               (dolist (frame emacs-frame--runtime-registry)
                 (when (emacs-frame--runtime-live-p frame)
                   (setq live (append live (list frame)))))
               live))))
  (unless (fboundp 'make-frame)
    (fset 'make-frame
          '(lambda (&optional parameters)
             (emacs-frame--runtime-ensure-initial)
             (let ((frame
                    (emacs-frame--runtime-make-frame-object parameters)))
               (setq emacs-frame--runtime-registry
                     (append emacs-frame--runtime-registry (list frame)))
               frame))))
  (unless (fboundp 'delete-frame)
    (fset 'delete-frame
          '(lambda (&optional frame force)
             (let ((target (emacs-frame--runtime-get frame)))
               (if (<= (length (frame-list)) 1)
                   nil
                 (emacs-frame--runtime-set target 'dead t)
                 (when (eq target emacs-frame--runtime-selected-frame)
                   (setq emacs-frame--runtime-selected-frame
                         (car (frame-list))))
                 nil)))))
  (unless (fboundp 'delete-other-frames)
    (fset 'delete-other-frames
          '(lambda (&optional frame)
             (let ((keep (emacs-frame--runtime-get frame)))
               (dolist (candidate (frame-list))
                 (unless (eq candidate keep)
                   (delete-frame candidate)))
               nil))))
  (unless (fboundp 'window-frame)
    (fset 'window-frame
          '(lambda (&optional window)
             (if (framep window)
                 window
               (selected-frame)))))
  (unless (fboundp 'frame-width)
    (fset 'frame-width
          '(lambda (&optional frame)
             (emacs-frame--runtime-ref
              (emacs-frame--runtime-get frame) 'width))))
  (unless (fboundp 'frame-height)
    (fset 'frame-height
          '(lambda (&optional frame)
             (emacs-frame--runtime-ref
              (emacs-frame--runtime-get frame) 'height))))
  (unless (fboundp 'frame-char-width)
    (fset 'frame-char-width '(lambda (&optional frame) 8)))
  (unless (fboundp 'frame-char-height)
    (fset 'frame-char-height '(lambda (&optional frame) 16)))
  (unless (fboundp 'frame-pixel-width)
    (fset 'frame-pixel-width
          '(lambda (&optional frame)
             (emacs-frame--runtime-ref
              (emacs-frame--runtime-get frame) 'pixel-width))))
  (unless (fboundp 'frame-pixel-height)
    (fset 'frame-pixel-height
          '(lambda (&optional frame)
             (emacs-frame--runtime-ref
              (emacs-frame--runtime-get frame) 'pixel-height))))
  (unless (fboundp 'set-frame-size)
    (fset 'set-frame-size
          '(lambda (frame cols lines &optional pixelwise)
             (let ((target (emacs-frame--runtime-get frame)))
               (emacs-frame--runtime-set target 'width cols)
               (emacs-frame--runtime-set target 'height lines)
               (emacs-frame--runtime-set target 'pixel-width (* cols 8))
               (emacs-frame--runtime-set target 'pixel-height (* lines 16))
               nil))))
  (unless (fboundp 'set-frame-position)
    (fset 'set-frame-position
          '(lambda (frame x y)
             (let ((target (emacs-frame--runtime-get frame)))
               (emacs-frame--runtime-set target 'left x)
               (emacs-frame--runtime-set target 'top y)
               nil))))
  (unless (fboundp 'frame-parameters)
    (fset 'frame-parameters
          '(lambda (&optional frame)
             (let ((target (emacs-frame--runtime-get frame)))
               (append
                (list (cons 'width (emacs-frame--runtime-ref target 'width))
                      (cons 'height (emacs-frame--runtime-ref target 'height))
                      (cons 'pixel-width
                            (emacs-frame--runtime-ref target 'pixel-width))
                      (cons 'pixel-height
                            (emacs-frame--runtime-ref target 'pixel-height))
                      (cons 'left (emacs-frame--runtime-ref target 'left))
                      (cons 'top (emacs-frame--runtime-ref target 'top))
                      (cons 'name (emacs-frame--runtime-ref target 'name))
                      (cons 'visibility
                            (emacs-frame--runtime-ref target 'visible)))
                (emacs-frame--runtime-ref target 'parameters))))))
  (unless (fboundp 'frame-parameter)
    (fset 'frame-parameter
          '(lambda (frame parameter)
             (cdr (assq parameter (frame-parameters frame))))))
  (unless (fboundp 'set-frame-parameter)
    (fset 'set-frame-parameter
          '(lambda (frame parameter value)
             (emacs-frame--runtime-apply-parameters
              (emacs-frame--runtime-get frame)
              (list (cons parameter value)))
             value)))
  (unless (fboundp 'modify-frame-parameters)
    (fset 'modify-frame-parameters
          '(lambda (frame alist)
             (emacs-frame--runtime-apply-parameters
              (emacs-frame--runtime-get frame) alist)
             nil)))
  (unless (fboundp 'frame-visible-p)
    (fset 'frame-visible-p
          '(lambda (&optional frame)
             (emacs-frame--runtime-ref
              (emacs-frame--runtime-get frame) 'visible))))
  (unless (fboundp 'make-frame-visible)
    (fset 'make-frame-visible
          '(lambda (&optional frame)
             (let ((target (emacs-frame--runtime-get frame)))
               (emacs-frame--runtime-set target 'visible t)
               target))))
  (unless (fboundp 'make-frame-invisible)
    (fset 'make-frame-invisible
          '(lambda (&optional frame force)
             (let ((target (emacs-frame--runtime-get frame)))
               (emacs-frame--runtime-set target 'visible nil)
               target))))
  (unless (fboundp 'raise-frame)
    (fset 'raise-frame
          '(lambda (&optional frame)
             (emacs-frame--runtime-get frame))))
  (unless (fboundp 'lower-frame)
    (fset 'lower-frame
          '(lambda (&optional frame)
             (emacs-frame--runtime-get frame))))
  (unless (fboundp 'select-frame)
    (fset 'select-frame
          '(lambda (frame &optional norecord)
             (when (emacs-frame--runtime-live-p frame)
               (setq emacs-frame--runtime-selected-frame frame))
             emacs-frame--runtime-selected-frame)))
  (unless (fboundp 'frame-focus)
    (fset 'frame-focus
          '(lambda (&optional frame)
             (if frame
                 (and (eq frame emacs-frame--runtime-selected-frame) frame)
               emacs-frame--runtime-selected-frame))))
  (unless (fboundp 'frame-windows)
    (fset 'frame-windows '(lambda (&optional frame) nil)))
  (unless (fboundp 'display-pixel-width)
    (fset 'display-pixel-width '(lambda (&optional display) 1024)))
  (unless (fboundp 'display-pixel-height)
    (fset 'display-pixel-height '(lambda (&optional display) 768)))
  (provide 'frame)
  (provide 'emacs-frame-builtins)
  t)

(defun nemacs-runtime-image-preload--install-tab-core ()
  "Install the daily-driver tab-line surface and tab-bar height primitive."
  (unless (boundp 'tab-line-mode)
    (defvar tab-line-mode nil))
  (unless (boundp 'global-tab-line-mode)
    (defvar global-tab-line-mode nil))
  (unless (boundp 'tab-line-format)
    (defvar tab-line-format nil))
  (unless (fboundp 'tab-bar-height)
    (fset 'tab-bar-height
          '(lambda (&optional frame)
             (if (bound-and-true-p tab-bar-mode) 1 0))))
  (unless (fboundp 'tab-line-mode)
    (fset 'tab-line-mode
          '(lambda (&optional arg)
             (setq tab-line-mode
                   (if arg
                       (> arg 0)
                     (not tab-line-mode)))
             (setq tab-line-format
                   (and tab-line-mode '(:eval (buffer-name))))
             tab-line-mode)))
  (unless (fboundp 'global-tab-line-mode)
    (fset 'global-tab-line-mode
          '(lambda (&optional arg)
             (setq global-tab-line-mode
                   (if arg
                       (> arg 0)
                     (not global-tab-line-mode)))
             global-tab-line-mode)))
  (unless (fboundp 'window-tab-line-height)
    (fset 'window-tab-line-height
          '(lambda (&optional window)
             (if (or tab-line-mode global-tab-line-mode) 1 0))))
  (unless (fboundp 'tab-line-tabs-buffer-list)
    (fset 'tab-line-tabs-buffer-list
          '(lambda ()
             (if (fboundp 'buffer-list)
                 (buffer-list)
               nil))))
  (unless (fboundp 'tab-line-tabs-window-buffers)
    (fset 'tab-line-tabs-window-buffers
          '(lambda ()
             (tab-line-tabs-buffer-list))))
  (unless (fboundp 'tab-line-tabs-fixed-window-buffers)
    (fset 'tab-line-tabs-fixed-window-buffers
          '(lambda ()
             (tab-line-tabs-buffer-list))))
  (unless (fboundp 'tab-line-tab-name-buffer)
    (fset 'tab-line-tab-name-buffer
          '(lambda (buffer &optional buffers)
             (if (fboundp 'buffer-name)
                 (buffer-name buffer)
               ""))))
  (provide 'tab-line)
  t)

(defun nemacs-runtime-image-preload--install-subr-x-core ()
  "Install common `subr-x' support helpers without source `load'."
  (unless (featurep 'subr-x)
    (unless (fboundp 'internal--thread-argument)
      (defmacro internal--thread-argument (first &rest forms)
        (let ((value (car forms))
              (tail (cdr forms)))
          (while tail
            (let ((form (car tail)))
              (setq value
                    (cond
                     ((consp form)
                      (if first
                          (cons (car form) (cons value (cdr form)))
                        (append form (list value))))
                     (first (list form value))
                     (t (list form value)))))
            (setq tail (cdr tail)))
          value)))
    (unless (fboundp 'thread-first)
      (defmacro thread-first (&rest forms)
        (declare (indent 0))
        (cons 'internal--thread-argument (cons t forms))))
    (unless (fboundp 'thread-last)
      (defmacro thread-last (&rest forms)
        (declare (indent 0))
        (cons 'internal--thread-argument (cons nil forms))))
    (unless (fboundp 'hash-table-empty-p)
      (fset 'hash-table-empty-p (lambda (hash-table)
                                  (= (hash-table-count hash-table) 0))))
    (unless (fboundp 'hash-table-keys)
      (fset 'hash-table-keys
            (lambda (hash-table)
              (let (keys)
                (maphash (lambda (key _value) (push key keys)) hash-table)
                keys))))
    (unless (fboundp 'hash-table-values)
      (fset 'hash-table-values
            (lambda (hash-table)
              (let (values)
                (maphash (lambda (_key value) (push value values)) hash-table)
                values))))
    (unless (fboundp 'string-remove-prefix)
      (fset 'string-remove-prefix
            (lambda (prefix string)
              (if (string-prefix-p prefix string)
                  (substring string (length prefix))
                string))))
    (unless (fboundp 'string-remove-suffix)
      (fset 'string-remove-suffix
            (lambda (suffix string)
              (if (string-suffix-p suffix string)
                  (substring string 0 (- (length string) (length suffix)))
                string))))
    (unless (fboundp 'string-replace)
      (fset 'string-replace
            (lambda (from-string to-string in-string)
              (if (= (length from-string) 0)
                  in-string
                (let ((start 0)
                      pieces
                      pos)
                  (while (setq pos (string-search from-string in-string start))
                    (push (substring in-string start pos) pieces)
                    (push to-string pieces)
                    (setq start (+ pos (length from-string))))
                  (push (substring in-string start) pieces)
                  (apply #'concat (nreverse pieces)))))))
    (unless (fboundp 'string-limit)
      (fset 'string-limit
            (lambda (string length &optional end _coding-system)
              (cond
               ((<= (length string) length) string)
               (end (substring string (- (length string) length)))
               (t (substring string 0 length))))))
    (unless (fboundp 'string-pad)
      (fset 'string-pad
            (lambda (string length &optional padding start)
              (let ((pad-length (- length (length string))))
                (if (<= pad-length 0)
                    string
                  (let ((pad (make-string pad-length (or padding ?\s))))
                    (if start (concat pad string) (concat string pad))))))))
    (unless (fboundp 'proper-list-p)
      (fset 'proper-list-p
            (lambda (object)
              (let ((tail object)
                    (len 0))
                (while (consp tail)
                  (setq len (1+ len)
                        tail (cdr tail)))
                (and (null tail) len)))))
    (unless (fboundp 'mapcan)
      (fset 'mapcan
            (lambda (function sequence &rest more-sequences)
              (apply #'nconc (apply #'mapcar function sequence more-sequences)))))
    (provide 'subr-x))
  t)

(defun nemacs-runtime-image-preload--install-support-core ()
  "Install lightweight support facades without source load."
  (nemacs-runtime-image-preload--install-subr-x-core)
  (require 'seq)
  (require 'map)
  (unless (featurep 'emacs-symbol)
    (require 'emacs-symbol))
  (require 'case-table)
  t)

;; Source-v1 runtime images replay top-level `setq' / `fset' reliably, while
;; helper `defun' forms are not a dependable runtime surface.  In standalone
;; bakes, force the frame/tab installers past earlier stub `fboundp' results so
;; the GUI-neutral API cells are recorded in the base image.  Host Emacs keeps
;; its native frame/tab functions because `nelisp--write-stdout-bytes' is not
;; present there.
(defun nemacs-runtime-image-preload--force-install-frame-tab-core ()
  "Force-install frame/tab API cells for standalone runtime-image bakes."
  (setq nemacs-runtime-image-preload--saved-fboundp (symbol-function 'fboundp))
  (fset 'fboundp '(lambda (_symbol) nil))
  (nemacs-runtime-image-preload--install-frame-core)
  (nemacs-runtime-image-preload--install-tab-core)
  (fset 'fboundp nemacs-runtime-image-preload--saved-fboundp)
  t)

(when (fboundp 'nelisp--write-stdout-bytes)
  (nemacs-runtime-image-preload--force-install-frame-tab-core))

;; Task #17 (M1) — Magit bridge extension.  Mirrors
;; `nemacs-runtime-image-preload-vendor-core-extension' above: a thin
;; dispatcher that hands off to the real logic owned by
;; `packages/emacs-lib-bridges/src/nelisp-emacs-magit-bridge.el' (per CLAUDE.md/AGENTS.md, session/image
;; wiring stays thin; the reusable "bring the real vendor Magit chain into a
;; NeLisp session" behavior belongs in a `src/' module, not here).
(defun nemacs-runtime-image-preload-magit-extension (repo-root)
  "Extend an already-baked base runtime image with the real vendor Magit chain.
REPO-ROOT is the repository root (matches the other preload entry points'
REPO-ROOT convention); the Magit bridge bundle and its own preconditions
are resolved relative to it."
  (nemacs-runtime-image-preload--load-source-file
   (expand-file-name "packages/emacs-lib-bridges/src/nelisp-emacs-magit-bridge.el" repo-root))
  (setq nelisp-emacs-magit-bridge-repo-root repo-root)
  (nelisp-emacs-magit-bridge-load)
  t)

(provide 'nemacs-runtime-image-preload)

;;; nemacs-runtime-image-preload.el ends here
