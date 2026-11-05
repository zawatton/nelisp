;;; nelisp-boot-image.el --- Source-free reader boot generation -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'cl-lib)
(declare-function nelisp-standalone-arena-rewrite-target "nelisp-standalone-arena-rewrite")
(declare-function nelisp-standalone--byte-write-forms "nelisp-standalone-build")
(declare-function nelisp-standalone--reader-tree-load-path "nelisp-standalone-build")

(defconst nelisp-boot-image--root
  (file-name-directory (directory-file-name (file-name-directory load-file-name))))
(defconst nelisp-boot-image--providers
  '((backquote . "vendor/emacs-lisp/emacs-lisp/backquote.el")
    (byte-run . "vendor/emacs-lisp/emacs-lisp/byte-run.el")
    (macroexp . "vendor/emacs-lisp/emacs-lisp/macroexp.el")
    (inline . "vendor/emacs-lisp/emacs-lisp/inline.el")
    (json . "packages/nelisp-emacs-foundation/src/json.el")
    (easy-mmode . "vendor/emacs-lisp/emacs-lisp/easy-mmode.el")
    (rx . "vendor/emacs-lisp/emacs-lisp/rx.el")
    (cl-seq . "vendor/emacs-lisp/emacs-lisp/cl-seq.el")
    (nelisp-process-buffer . "lisp/nelisp-process-buffer.el")))

(defconst nelisp-boot-image--embedded-providers
  '((pcase . "vendor/staged-emacs-lisp/pcase.el")
    (gv . "vendor/emacs-lisp/emacs-lisp/gv.el")
    (help . "vendor/emacs-lisp-api/help.el")
    (cl-macs . "vendor/staged-emacs-lisp/cl-macs.el")
    (cl-preloaded . "vendor/staged-emacs-lisp/cl-preloaded.el")
    (oclosure . "vendor/emacs-lisp/emacs-lisp/oclosure.el")
    (cl-generic . "vendor/staged-emacs-lisp/cl-generic.el")
    (nadvice . "vendor/emacs-lisp/emacs-lisp/nadvice.el"))
  "Vendored GNU files embedded at their GNU preload position, not deferred.
`pcase' follows `macroexp' in GNU's loadup.el, and `gv' follows `pcase'.  Unlike
`nelisp-boot-image--providers' these get no autoload entries.")

(defconst nelisp-boot-image--source-only-providers
  '((cl-lib . "vendor/staged-emacs-lisp/cl-lib.el")
    (cl-loaddefs . "vendor/staged-emacs-lisp/cl-loaddefs.el")
    (cl-extra . "vendor/staged-emacs-lisp/cl-extra.el")
    (cl-print . "vendor/staged-emacs-lisp/cl-print.el")
    (help-mode . "vendor/emacs-lisp/help-mode.el")
    (mule-util . "vendor/emacs-lisp/international/mule-util.el"))
  "GNU files that load on demand, as GNU loads them.
GNU does not preload cl-lib, cl-extra or cl-print: `require' loads cl-lib, and
the entry points of the others are autoloaded by loaddefs.el and cl-loaddefs.el
(see `nelisp-boot-image-cl-autoloads-source').  Unlike
`nelisp-boot-image--providers' these register no autoload for each definition,
so the natives that exist before the first load are not replaced by stubs.")

(defun nelisp-boot-image--pcase-source (source path &optional name)
  "Bake GNU SOURCE (pcase.el, or the file named NAME) like its `.elc'."
  (if (equal (getenv "NELISP_STANDALONE_PRELUDE_SOURCE_ONLY") "1") source
    (require 'nelisp-prelude-bytecode)
    (let* ((print-escape-nonascii t)
           (print-escape-multibyte t)
           (dirs (list (expand-file-name "vendor/staged-emacs-lisp"
                                         nelisp-boot-image--root)
                       (expand-file-name "vendor/emacs-lisp/emacs-lisp"
                                         nelisp-boot-image--root)
                       (expand-file-name "vendor/emacs-lisp"
                                         nelisp-boot-image--root)))
           (result (nelisp-prelude-bytecode-vendor-transform source path dirs))
           (report (expand-file-name (format "target/nelisp-boot-image/%s-bytecode.tsv"
                                             (or name 'pcase))
                                     nelisp-boot-image--root)))
      (make-directory (file-name-directory report) t)
      (nelisp-prelude-bytecode-write-report report (nth 2 result))
      (nelisp-boot-image--expand-load-time-macros (car result)))))

(defconst nelisp-boot-image--load-time-macros
  '(cl-defstruct define-inline define-minor-mode)
  "Macros whose top-level uses the bake expands, as GNU's compiler would.
GNU compiles these libraries, so `cl-defstruct' (which generates
`define-inline'), `define-inline' and `define-minor-mode' are expanded at
build time and the `.elc' never loads inline.el or easy-mmode.el.  Here the
baked forms stay source, so the same expansion is done by the build host.")

;; Host `eval-and-compile' evaluates its body at expansion time and returns the
;; quoted value, which would drop `cl-struct-define'; at load it is `progn'.
;; The customization macros stay for the runtime's own definitions (the host
;; `custom-declare-variable' machinery does not exist when these forms load).
(defconst nelisp-boot-image--expansion-environment
  '((eval-and-compile . (lambda (&rest body) (cons 'progn body)))
    (defcustom) (defgroup) (defface))
  "Macro environment for build-time expansion of load-time forms.")

(defun nelisp-boot-image--expand-rx (form)
  "Return FORM with each `rx' call expanded, skipping quoted data."
  (cond ((not (consp form)) form)
        ((memq (car form) '(quote \`)) form)
        ((eq (car form) 'rx)
         (macroexpand-all form nelisp-boot-image--expansion-environment))
        (t (let ((head (nelisp-boot-image--expand-rx (car form)))
                 (tail (cdr form)))
             (cons head
                   (if (consp tail)
                       (let ((copy (copy-sequence tail)) (cell nil))
                         (setq cell copy)
                         (while (consp cell)
                           (setcar cell (nelisp-boot-image--expand-rx (car cell)))
                           (setq cell (if (consp (cdr cell)) (cdr cell) nil)))
                         copy)
                     tail))))))

(defun nelisp-boot-image--tree-has-rx-p (form)
  "Return non-nil when FORM calls `rx' outside quoted data."
  (and (consp form) (not (memq (car form) '(quote \`)))
       (or (eq (car form) 'rx)
           (let ((cell form) (found nil))
             (while (and (consp cell) (not found))
               (setq found (nelisp-boot-image--tree-has-rx-p (car cell))
                     cell (cdr cell)))
             found))))

(defun nelisp-boot-image--expand-load-time-macros (text)
  "Return baked TEXT with compile-time-only macro uses expanded on the host.
Top-level `cl-defstruct', `define-inline' and `define-minor-mode' forms, and
`rx' calls inside kept forms, are expanded here so that booting the image does
not load inline.el, easy-mmode.el, rx.el or cl-seq.el (GNU's `.elc' expands
them at compile time)."
  (require 'cl-lib) (require 'inline) (require 'easy-mmode) (require 'rx)
  (require 'nelisp-prelude-bytecode)
  (let ((patches nil))
    (dolist (span (nelisp-prelude-bytecode--top-level-spans text))
      (let* ((form (nth 2 span))
             (new (cond ((or (memq (car-safe form) nelisp-boot-image--load-time-macros)
                             ;; cl-lib.el's `cl-defmethod' sits in this guard; its
                             ;; expansion would load cl-seq.el (GNU's `.elc' has it
                             ;; expanded).
                             (equal (nth 1 form) '(fboundp 'cl-generic-define-method)))
                         (macroexpand-all form nelisp-boot-image--expansion-environment))
                        ((and (memq (car-safe form) '(defun defalias defvar defconst))
                              (nelisp-boot-image--tree-has-rx-p form))
                         (nelisp-boot-image--expand-rx form)))))
        (when new
          (push (list (nth 0 span) (nth 1 span)
                      (let ((print-length nil) (print-level nil)
                            (print-circle t) (print-gensym t)
                            (print-escape-newlines nil)
                            (print-escape-nonascii t) (print-escape-multibyte t)
                            (print-escape-control-characters t))
                        (prin1-to-string new)))
                patches))))
    (dolist (patch patches)             ; PATCHES is latest-first: offsets stay valid
      (setq text (concat (substring text 0 (nth 0 patch)) (nth 2 patch)
                         (substring text (nth 1 patch)))))
    text))

(defun nelisp-boot-image-expand-block (text)
  "Return TEXT, cl-macs.el's derived-type block, with `cl--defnumtype' expanded.
`(cl--defnumtype real number)' is a `cl-deftype' with an argument list; its
expansion reaches `cl--make-usage-args' and so cl-seq.el.  GNU's `.elc' of
cl-macs.el has it expanded at compile time, so the build host expands it here
and booting the image loads no cl-seq.el."
  (require 'cl-lib) (require 'cl-macs)
  (let* ((call "(cl--defnumtype real number)")
         (at (string-search call text))
         (macro-start (string-search "(defmacro cl--defnumtype" text))
         (print-length nil) (print-level nil) (print-circle t) (print-gensym t)
         (print-escape-newlines nil) (print-escape-nonascii t)
         (print-escape-multibyte t) (print-escape-control-characters t))
    (unless (and at macro-start (< macro-start at))
      (error "cl-macs.el cl--defnumtype block not found"))
    (eval (car (read-from-string text macro-start)) t)
    (let ((expansion (macroexpand-all (car (read-from-string call))
                                      nelisp-boot-image--expansion-environment)))
      (when (string-match-p "#\\[" (prin1-to-string expansion))
        (error "Host byte-code leaked into the baked derived-type block"))
      (concat (substring text 0 at) (prin1-to-string expansion)
              (substring text (+ at (length call)))))))

(defun nelisp-boot-image--macroexp-source (source path)
  "Bake admitted GNU macroexp functions, retaining rejected functions as source."
  ;; GNU's primitive macroexpand follows aliases/environment entries directly.
  ;; Its Lisp port shares the provider's lexical compilation and source fallback.
  (setq source
        (concat source "\n"
                (with-temp-buffer
                  (insert-file-contents
                   (expand-file-name "lisp/nelisp-load-macroexpand.el"
                                     nelisp-boot-image--root))
                  (buffer-string))))
  ;; GNU macroexp.el:954-962 reloads compiled macroexp during bootstrap when
  ;; the preloaded provider is interpreted. Use the existing whole-file GNU
  ;; compiler and VM opcode selection rather than interpreting every helper.
  (if (equal (getenv "NELISP_STANDALONE_PRELUDE_SOURCE_ONLY") "1") source
    (require 'nelisp-prelude-bytecode)
    (let* ((print-escape-nonascii t)
           (print-escape-multibyte t)
           (dirs (list (expand-file-name "vendor/emacs-lisp/emacs-lisp"
                                         nelisp-boot-image--root)
                       (expand-file-name "vendor/emacs-lisp"
                                         nelisp-boot-image--root)))
           (result (nelisp-prelude-bytecode-vendor-transform source path dirs))
           (report (expand-file-name "target/nelisp-boot-image/macroexp-bytecode.tsv"
                                     nelisp-boot-image--root)))
      (make-directory (file-name-directory report) t)
      (nelisp-prelude-bytecode-write-report report (nth 2 result))
      (car result))))

(defun nelisp-boot-image-provider (name)
  "Return NAME's provider for embedding, with a trailing form.
GNU macroexp functions use the existing reviewed bytecode selector. Other
providers and source-only builds retain their exact source text."
  (let* ((path (or (cdr (assq name nelisp-boot-image--providers))
                   (cdr (assq name nelisp-boot-image--embedded-providers))
                   (cdr (assq name nelisp-boot-image--source-only-providers))
                   (error "Unknown boot provider: %S" name)))
         (source (with-temp-buffer
                   (insert-file-contents (expand-file-name path nelisp-boot-image--root))
                   (buffer-string))))
    ;; Snapshot exactly the file the former runtime load would have read.
    ;; The executable build digest binds these bytes to its heap companion.
    (concat
     (cond ((eq name 'macroexp) (nelisp-boot-image--macroexp-source source path))
           ((memq name '(pcase gv help cl-macs cl-preloaded oclosure cl-generic nadvice))
            (nelisp-boot-image--pcase-source source path name))
           ;; GNU's cl-lib.elc has `cl-old-struct-compat-mode' already expanded.
           ((eq name 'cl-lib) (nelisp-boot-image--expand-load-time-macros source))
           (t source))
     "\nt\n")))

;; The prelude's own uses of the cl-macs and gv macros run (or are expanded at
;; first call) before GNU's gv.el and cl-macs.el load, and those files cannot load
;; earlier.  GNU's preloaded `.elc' files have these uses expanded at compile time;
;; the prelude gets the same expansion here, on the build host, with GNU's macros.
(defconst nelisp-boot-image--prelude-expanded-macros
  '(setf cl-incf cl-decf incf decf cl-callf cl-callf2 cl-psetf cl-psetq
    cl-block cl-return cl-return-from cl-loop cl-labels cl-flet cl-case
    cl-macrolet cl-symbol-macrolet cl-defun cl-function cl-dolist cl-dotimes
    named-let with-memoization push pop cl-defstruct cl-defmethod cl-defgeneric)
  "Macros whose uses in the prelude are expanded at bake time.")

(defun nelisp-boot-image--cl-target-p (form)
  "Return non-nil when FORM, a call of an expanded macro, must be expanded.
`push', `pop', `incf' and `decf' on a plain variable need no gv."
  (cond ((eq (car form) 'push) (consp (nth 2 form)))
        ((memq (car form) '(pop incf decf)) (consp (nth 1 form)))
        (t t)))

(defun nelisp-boot-image--cl-walk (form)
  "Return FORM with the uses of `nelisp-boot-image--prelude-expanded-macros' expanded."
  (cond
   ((not (consp form)) form)
   ((not (proper-list-p form)) form)
   ((memq (car form) '(quote declare \`)) form)
   ((and (symbolp (car form))
         (memq (car form) nelisp-boot-image--prelude-expanded-macros)
         (nelisp-boot-image--cl-target-p form))
    (let ((new (macroexpand-1 form nelisp-boot-image--expansion-environment)))
      (when (equal new form)
        (error "Host cannot expand prelude %S use: %S" (car form) form))
      (nelisp-boot-image--cl-post (nelisp-boot-image--cl-walk new))))
   ((and (eq (car form) 'function) (eq (car-safe (cadr form)) 'lambda))
    (list 'function (nelisp-boot-image--cl-walk (cadr form))))
   (t (mapcar #'nelisp-boot-image--cl-walk form))))

(defun nelisp-boot-image--cl-post (form)
  "Apply GNU's compiler macros for the calls cl-macs expansions leave behind."
  (cond ((not (consp form)) form)
        ((memq (car form) '(quote function)) form)
        (t (let ((form (if (proper-list-p form)
                           (mapcar #'nelisp-boot-image--cl-post form)
                         form)))
             (cond ((and (eq (car form) 'cl--block-wrapper) (= (length form) 2))
                    (nth 1 form))
                   ((eq (car form) 'cl--block-throw) (cons 'throw (cdr form)))
                   (t form))))))

(defun nelisp-boot-image--tree-uses-p (form names)
  "Return non-nil when FORM calls one of NAMES outside quoted data."
  (and (consp form) (not (memq (car form) '(quote declare \`)))
       (or (and (symbolp (car form)) (memq (car form) names)
                (nelisp-boot-image--cl-target-p form))
           (let ((cell form) (found nil))
             (while (and (consp cell) (not found))
               (setq found (nelisp-boot-image--tree-uses-p (car cell) names)
                     cell (cdr cell)))
             found))))

(defun nelisp-boot-image--place-heads (spans)
  "Return the symbols that appear as places in the setf-family uses of SPANS."
  (let ((heads nil))
    (cl-labels
        ((walk (f)
           (when (and (consp f) (not (memq (car f) '(quote \`))))
             (cond
              ((memq (car f) '(setf cl-psetf))
               (let ((a (cdr f)))
                 (while (consp a)
                   (when (consp (car a)) (push (car (car a)) heads))
                   (setq a (cdr-safe (cdr a))))))
              ((memq (car f) '(push cl-callf cl-callf2))
               (when (consp (nth 2 f)) (push (car (nth 2 f)) heads)))
              ((memq (car f) '(pop incf decf cl-incf cl-decf))
               (when (consp (nth 1 f)) (push (car (nth 1 f)) heads))))
             (let ((c f)) (while (consp c) (walk (car c)) (setq c (cdr c)))))))
      (dolist (s spans) (walk (nth 2 s))))
    heads))

(defun nelisp-boot-image-expand-prelude-cl-macros (text)
  "Return prelude TEXT with its cl-macs and gv macro uses expanded on the host.
The place macros and the `nelisp--define-core-record' types the uses refer to
are defined on the host first, so GNU's gv resolves them as the runtime will."
  (require 'cl-lib) (require 'gv) (require 'nelisp-prelude-bytecode)
  (let* ((spans (nelisp-prelude-bytecode--top-level-spans text))
         (heads (nelisp-boot-image--place-heads spans))
         (defined nil) (patches nil)
         (lexical-binding nil))
    (unwind-protect
        (progn
          ;; Host definitions of the places, in file order.
          (dolist (span spans)
            (let ((form (nth 2 span)))
              (when (or (and (eq (car-safe form) 'defmacro)
                             (memq (cadr form) heads)
                             (not (fboundp (cadr form))))
                        (and (eq (car-safe form) 'defmacro)
                             (eq (cadr form) 'nelisp--define-core-record)))
                (push (cadr form) defined)
                (eval form nil))
              (when (eq (car-safe form) 'nelisp--define-core-record)
                (when (fboundp (intern (format "%s-p" (cadr form))))
                  (error "Host already defines %S" (cadr form)))
                (dolist (def (cdr (macroexpand-1 form)))
                  (when (memq (car-safe def) '(defun))
                    (push (cadr def) defined)))
                (eval form nil))))
          (dolist (span spans)
            (let ((form (nth 2 span)))
              (when (nelisp-boot-image--tree-uses-p
                     form nelisp-boot-image--prelude-expanded-macros)
                (let ((new (nelisp-boot-image--cl-walk form))
                      (print-length nil) (print-level nil) (print-circle t)
                      (print-gensym t) (print-escape-newlines nil)
                      (print-escape-nonascii t) (print-escape-multibyte t)
                      (print-escape-control-characters t))
                  (let ((printed (prin1-to-string new)))
                    (when (string-match-p "#\\[" printed)
                      (error "Host byte-code leaked into %S" (car-safe (cdr-safe form))))
                    (push (list (nth 0 span) (nth 1 span) printed) patches))))))
          (dolist (patch patches)
            (setq text (concat (substring text 0 (nth 0 patch)) (nth 2 patch)
                               (substring text (nth 1 patch))))))
      (dolist (name defined) (when (fboundp name) (fmakunbound name))))
    text))

(defconst nelisp-boot-image--composed-cl-macs-marker
  "\n;; --- GNU Emacs 31.1 cl-macs.el (nadvice not yet loaded) ---\n"
  "Line that starts GNU cl-macs in the composed boot source.")

(defconst nelisp-boot-image--deleted-native-macros
  '(setf cl-defstruct cl-loop cl-case cl-labels cl-incf cl-block cl-return
    cl-return-from cl-defun cl-macrolet cl-symbol-macrolet cl-psetf cl-psetq
    cl-defgeneric cl-defmethod cl-callf)
  "Macros the prelude no longer defines natively; GNU cl-macs and gv own them.")

(defun nelisp-boot-image--expand-staged-defstructs (text)
  "Return TEXT with each top-level `cl-defstruct' fully expanded on the host.
These are GNU cl-preloaded.el structs staged before cl-macs and inline.el load;
GNU's `.elc' has them expanded (through `define-inline') at compile time."
  (require 'cl-lib) (require 'inline) (require 'nelisp-prelude-bytecode)
  (let ((patches nil))
    (dolist (span (nelisp-prelude-bytecode--top-level-spans text))
      (when (eq (car-safe (nth 2 span)) 'cl-defstruct)
        (let* ((new (macroexpand-all (nth 2 span)
                                     nelisp-boot-image--expansion-environment))
               (printed (let ((print-length nil) (print-level nil)
                              (print-circle t) (print-gensym t)
                              (print-escape-newlines nil)
                              (print-escape-nonascii t) (print-escape-multibyte t)
                              (print-escape-control-characters t))
                          (prin1-to-string new))))
          (when (string-match-p "#\\[" printed)
            (error "Host byte-code leaked into staged %S" (cadr (nth 2 span))))
          (push (list (nth 0 span) (nth 1 span) printed) patches))))
    (dolist (patch patches)
      (setq text (concat (substring text 0 (nth 0 patch)) (nth 2 patch)
                         (substring text (nth 1 patch)))))
    text))

(defun nelisp-boot-image-expand-composed-prelude (text)
  "Return composed boot source TEXT with its pre-cl-macs part host-expanded.
Every piece evaluated before GNU cl-macs loads (staged vendor forms included,
not just the prelude file) must not call a deleted native macro.  Signal an
error naming the first remaining use."
  (let ((cut (string-search nelisp-boot-image--composed-cl-macs-marker text)))
    (unless cut (error "Composed boot source lacks the cl-macs boundary"))
    (let* ((head (nelisp-boot-image-expand-prelude-cl-macros
                  (nelisp-boot-image--expand-staged-defstructs
                   (substring text 0 cut))))
           (spans (nelisp-prelude-bytecode--top-level-spans head)))
      (dolist (span spans)
        (when (nelisp-boot-image--tree-uses-p
               (nth 2 span) nelisp-boot-image--deleted-native-macros)
          (error "Deleted native macro still used before cl-macs at byte %d: %.200S"
                 (nth 0 span) (nth 2 span))))
      (message "[boot-image] composed pre-cl-macs source: no deleted macro heads (%d forms)"
               (length spans))
      (concat head (substring text cut)))))

(defun nelisp-boot-image-prelude (source)
  "Embed backquote at its original bootstrap boundary in SOURCE."
  (let* ((old "(if (boundp 'nelisp-standalone--backquote-file)\n    (load nelisp-standalone--backquote-file)\n  (require 'backquote))")
         (regexp (regexp-quote old)))
    (unless (string-match regexp source)
      (error "Missing backquote bootstrap boundary"))
    (let ((begin (match-beginning 0)) (end (match-end 0)))
      (when (string-match regexp source end)
        (error "Duplicate backquote bootstrap boundary"))
      (nelisp-boot-image-expand-prelude-cl-macros
       (concat (substring source 0 begin)
               (nelisp-boot-image-provider 'backquote)
               (substring source end))))))

(defun nelisp-boot-image--autoloads (name source)
  "Collect NAME's definitions, retaining which are guarded fallbacks."
  (let ((position 0) forms definitions aliases)
    (condition-case nil
        (while t
          (let ((read (read-from-string source position)))
            (setq position (cdr read))
            (push (car read) forms)))
      (end-of-file nil))
    (cl-labels
        ((walk (form guarded)
           (cond
            ((memq (car-safe form) '(defun defsubst defmacro cl-defun cl-defsubst cl-defmacro))
             (push (list (cadr form) (symbol-name name)
                         (and (memq (car form) '(defmacro cl-defmacro)) 'macro)
                         guarded) definitions))
            ((and (eq (car-safe form) 'defalias) (eq (car-safe (cadr form)) 'quote)
                  (memq (car-safe (nth 2 form)) '(quote function)))
             (push (list (cadr (cadr form)) (cadr (nth 2 form)) guarded) aliases))
            ((and (consp form) (not (memq (car form) '(quote function lambda))))
             (dolist (child (cdr form))
               (walk child (or guarded (not (memq (car form) '(progn eval-and-compile))))))))))
      (dolist (form (nreverse forms)) (walk form nil)))
    (dolist (alias aliases)
      (let ((target (assq (nth 1 alias) definitions)))
        (when target
          (push (list (car alias) (symbol-name name) (nth 2 target)
                      (or (nth 2 alias) (nth 3 target))) definitions))))
    definitions))

(defun nelisp-boot-image-gnu-autoloads-source ()
  "Return GNU Emacs 31.1 startup `autoload' forms of vendored libraries.
The forms are copied verbatim from the vendored lisp/loaddefs.el.  A form is
kept when its library has a vendored file in a vendor/ directory of the
runtime load-path.  `autoload' leaves an existing function cell alone, so a
name that is already defined is not replaced."
  (let* ((root nelisp-boot-image--root)
         (vendor (expand-file-name "vendor/" root))
         (staged (expand-file-name "vendor/staged-emacs-lisp" root))
         (dirs (cl-remove-if-not
                (lambda (dir)
                  (and (string-prefix-p vendor (file-name-as-directory dir))
                       (not (string-prefix-p staged dir))))
                (nelisp-standalone--reader-tree-load-path)))
         (forms nil))
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8-unix))
        (insert-file-contents (expand-file-name "vendor/emacs-lisp/loaddefs.el" root)))
      (goto-char (point-min))
      (condition-case nil
          (while t
            (forward-comment (point-max))
            (let* ((start (point)) (form (read (current-buffer))))
              (when (and (eq (car-safe form) 'autoload)
                         (stringp (nth 2 form))
                         (or (member (nth 2 form) '("cl-lib" "cl-print"))
                             (cl-some (lambda (dir)
                                        (file-exists-p
                                         (expand-file-name (concat (nth 2 form) ".el") dir)))
                                      dirs)))
                (push (buffer-substring-no-properties start (point)) forms))))
        (end-of-file nil)))
    (concat "\n;; --- GNU Emacs 31.1 loaddefs.el: autoloads of vendored libraries ---\n"
            (mapconcat #'identity (nreverse forms) "\n")
            "\nt\n")))

(defun nelisp-boot-image-cl-autoloads-source ()
  "Return the `autoload' and `function-put' forms of GNU cl-loaddefs.el.
cl-lib.el loads cl-loaddefs; GNU preloads cl-generic, which makes these
registrations available before cl-lib is required.  `autoload' leaves an
existing function cell alone."
  (let (forms)
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8-unix))
        (insert-file-contents (expand-file-name "vendor/staged-emacs-lisp/cl-loaddefs.el"
                                                nelisp-boot-image--root)))
      (goto-char (point-min))
      (condition-case nil
          (while t
            (forward-comment (point-max))
            (let* ((start (point)) (form (read (current-buffer))))
              (when (memq (car-safe form) '(autoload function-put))
                (push (buffer-substring-no-properties start (point)) forms))))
        (end-of-file nil)))
    (concat "\n;; --- GNU Emacs 31.1 cl-loaddefs.el ---\n"
            (mapconcat #'identity (nreverse forms) "\n")
            "\nt\n")))

(defun nelisp-boot-image-libraries-source ()
  "Return executable-pinned providers and their genuine deferred entry points."
  (let (sources autoloads)
    (dolist (provider nelisp-boot-image--providers)
      (let* ((name (car provider)) (source (nelisp-boot-image-provider name)))
        (push (list (symbol-name name)
                    (expand-file-name (cdr provider) nelisp-boot-image--root) source) sources)
        (unless (memq name '(backquote byte-run macroexp))
          (setq autoloads (append (nelisp-boot-image--autoloads name source) autoloads)))))
    ;; Demand-loaded GNU files: sources only, no per-definition autoloads.
    (dolist (provider nelisp-boot-image--source-only-providers)
      (push (list (symbol-name (car provider))
                  (expand-file-name (cdr provider) nelisp-boot-image--root)
                  (nelisp-boot-image-provider (car provider)))
            sources))
    (concat
     (with-temp-buffer
       (insert-file-contents (expand-file-name "lisp/nelisp-boot-libraries.el" nelisp-boot-image--root))
       (buffer-string))
     ;; Preserve the process substrate owners before installing public aliases.
     ;; Deferred loading must never capture its own autoload as its base function.
     (mapconcat
      (lambda (entry) (format "(defvar %S (symbol-function '%S))\n" (car entry) (cdr entry)))
      '((nelisp-process-buffer--original-get . process-get)
        (nelisp-process-buffer--original-put . process-put)
        (nelisp-process-buffer--original-p . processp)
        (nelisp-process-buffer--original-status . process-status)
        (nelisp-process-buffer--original-live . process-live-p)
        (nelisp-process-buffer--original-buffer . process-buffer)
        (nelisp-process-buffer--original-exit . process-exit-status)
        (nelisp-process-buffer--original-markerp . markerp)) "")
     (format "\n(nelisp-boot-library-install '%S '%S)\nt\n"
             (nreverse sources) (nreverse autoloads))
     (nelisp-boot-image-gnu-autoloads-source))))

(defun nelisp-boot-image-process-source ()
  "Return the process adapter with its buffer provider deferred to first use."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "packages/nelisp-process-adapter/src/nelisp-process-adapter.el"
                       nelisp-boot-image--root))
    (goto-char (point-min))
    (unless (search-forward "(require 'nelisp-process-buffer)" nil t)
      (error "Missing process buffer dependency boundary"))
    (replace-match "" t t)
    (buffer-string)))

(defun nelisp-boot-image-native-forms ()
  "Return bootstrap-only raw OS helpers; no new Lisp primitive is exported.
Minimal native surface clauses 3/4: executable path and external scratch
memory must be available before the first Lisp form or heap restoration."
  (cond
   ((eq (nelisp-standalone-arena-rewrite-target) 'windows-x86_64)
    (nelisp-boot-image--windows-native-forms))
   ((memq (nelisp-standalone-arena-rewrite-target) '(linux-x86_64 linux-aarch64))
      (let ((arm (eq (nelisp-standalone-arena-rewrite-target) 'linux-aarch64)))
      `((defun nl_boot_image_decimal (buffer number)
          (if (< number 10)
              (seq (ptr-write-u8 buffer 0 (+ 48 number)) 1)
            (let ((n (nl_boot_image_decimal buffer (/ number 10))))
              (seq (ptr-write-u8 buffer n (+ 48 (mod number 10))) (+ n 1)))))
        (defun nl_boot_image_open (path)
          ;; O_RDONLY | O_NOFOLLOW | O_CLOEXEC: a discovered image cannot be
          ;; a symlink. Explicit --cold-load-from retains its caller's policy.
          ,(if arm '(syscall-direct 56 -100 path 655360 0 0 0)
             '(syscall-direct 2 path 655360 0 0 0 0)))
        (defun nl_boot_image_pin (scratch path)
          (let* ((executable (nl_os_open_read scratch)))
            (if (< executable 0) (seq (nl_os_free_chunk scratch 8192) 0)
              (let* ((image (nl_boot_image_open path)))
                (if (< image 0)
                    (seq (nl_os_close_handle executable)
                         (nl_os_free_chunk scratch 8192) 0)
                  (let* ((exe_stat (+ scratch 4128))
                         (image_stat (+ scratch 4384))
                         (exe_status (syscall-direct ,(if arm 80 5) executable exe_stat 0 0 0 0))
                         (image_status (syscall-direct ,(if arm 80 5) image image_stat 0 0 0 0))
                         (mode (heap-read-u32 image_stat ,(if arm 16 24)))
                         (bad (or (< exe_status 0) (< image_status 0)
                                  (/= (logand mode 61440) 32768)
                                  (/= (logand mode 18) 0)
                                  ;; Aliases share this inode's checked owner and
                                  ;; mode; only an unlinked inode is rejected.
                                  (<= ,(if arm '(heap-read-u32 image_stat 20)
                                         '(heap-read-u64 image_stat 16)) 0)
                                  (/= (heap-read-u32 exe_stat ,(if arm 24 28))
                                      (heap-read-u32 image_stat ,(if arm 24 28))))))
                    (seq
                     (nl_os_close_handle executable)
                     (if bad
                         (seq (nl_os_close_handle image)
                              (nl_os_free_chunk scratch 8192) 0)
                       (let* ((pinned (+ scratch 4672)))
                         (seq
                          ;; Keep the checked inode open across both arena and
                          ;; globals reads. A pathname replacement cannot swap it.
                          ;; This does not stop writes by the trusted owner or
                          ;; through a previously authorized writable descriptor.
                          (ptr-write-u64 scratch 4640 image)
                          ,@(nelisp-standalone--byte-write-forms 'pinned "/proc/self/fd/")
                          (let ((digits (nl_boot_image_decimal (+ pinned 14) image)))
                            (ptr-write-u8 pinned (+ 14 digits) 0))
                          pinned))))))))))
        (defun nl_boot_image_companion ()
          (let* ((scratch (nl_os_alloc_chunk 8192)))
            (if (= scratch 0) 0
              (let* ((path (+ scratch 32)))
                (seq
                 ,@(nelisp-standalone--byte-write-forms 'scratch "/proc/self/exe")
                 (ptr-write-u8 scratch 14 0)
                 (let* ((n (nl_os_readlink_path scratch path 4090)))
                   (if (or (<= n 0) (>= n 4090))
                       (seq (nl_os_free_chunk scratch 8192) 0)
                     (seq
                      ,@(nelisp-standalone--byte-write-forms '(+ path n) ".cold")
                      (ptr-write-u8 (+ path n) 5 0)
                      (nl_boot_image_pin scratch path)))))))))
        (defun nl_boot_image_release (path)
          (if (= path 0) 0
            (let ((scratch (- path 4672)))
              (seq (nl_os_close_handle (heap-read-u64 scratch 4640))
                   (nl_os_free_chunk scratch 8192))))))))
   (t '((defun nl_boot_image_companion () 0)
        (defun nl_boot_image_release (_path) 0)))))

(defun nelisp-boot-image--windows-native-forms ()
  "Return pre-heap Win32 discovery and handle-pinned cold input helpers."
  '((defun nl_boot_image_windows_valid (scratch executable image)
      ;; Match Linux's same-owner policy using the owner SID, not the current
      ;; token (an installed executable may belong to an administrator).
      ;; READ_CONTROL failure rejects discovery. FILE_SHARE_READ alone denies
      ;; writers and deletion throughout validation and both restore reads.
      ;; Positive link counts permit aliases of the same checked inode. The SID
      ;; check does not claim Linux-equivalent ACL/mode filtering.
      (let ((info (+ scratch 65536))
            (exe_sd (+ scratch 65600)) (image_sd (+ scratch 66624))
            (length (+ scratch 67648)) (exe_owner (+ scratch 67656))
            (image_owner (+ scratch 67664)) (defaulted (+ scratch 67672)))
        (if (or (/= (extern-call GetFileType image) 1)
                (= (extern-call GetFileInformationByHandle image info) 0)
                (/= (logand (heap-read-u32 info 0) 1040) 0)
                (<= (heap-read-u32 info 40) 0)
                (= (extern-call GetKernelObjectSecurity executable 1 exe_sd 1024 length) 0)
                (= (extern-call GetKernelObjectSecurity image 1 image_sd 1024 length) 0)
                (= (extern-call GetSecurityDescriptorOwner exe_sd exe_owner defaulted) 0)
                (= (extern-call GetSecurityDescriptorOwner image_sd image_owner defaulted) 0)
                (= (heap-read-u64 exe_owner 0) 0)
                (= (heap-read-u64 image_owner 0) 0))
            0
          (extern-call EqualSid (heap-read-u64 exe_owner 0)
                       (heap-read-u64 image_owner 0)))))
    (defun nl_boot_image_companion ()
      ;; 32768 UTF-16 units plus metadata. This mapping must survive arena
      ;; restoration; no pathname or handle is allocated inside the heap.
      (let ((scratch (nl_os_alloc_chunk 73728)))
        (if (= scratch 0) 0
          (let ((n (extern-call GetModuleFileNameW 0 scratch 32762)))
            (if (or (<= n 0) (>= n 32762))
                (seq (nl_os_free_chunk scratch 73728) 0)
              ;; The module path can retain a symbolic-link launch spelling.
              ;; Compare the executable target's owner, not the link's owner;
              ;; the companion itself is still opened without following links.
              (let ((executable (extern-call CreateFileW scratch 2147614720 1 0 3 128 0)))
                (if (< executable 0)
                    (seq (nl_os_free_chunk scratch 73728) 0)
                  (seq
                   (ptr-write-u16 scratch (* n 2) 46)
                   (ptr-write-u16 scratch (* (+ n 1) 2) 99)
                   (ptr-write-u16 scratch (* (+ n 2) 2) 111)
                   (ptr-write-u16 scratch (* (+ n 3) 2) 108)
                   (ptr-write-u16 scratch (* (+ n 4) 2) 100)
                   (ptr-write-u16 scratch (* (+ n 5) 2) 0)
                   (let* ((image (extern-call CreateFileW scratch 2147614720 1 0 3 2097280 0))
                          (valid (if (< image 0) 0
                                   (nl_boot_image_windows_valid scratch executable image))))
                     (seq
                      (nl_os_close_handle executable)
                      (if (= valid 0)
                          (seq (nl_os_close_handle image)
                               (nl_os_free_chunk scratch 73728) 0)
                        (seq
                         (ptr-write-u64 scratch 67680 image)
                         ;; Negative OS pointer is an opaque boot-only token;
                         ;; explicit UTF-8 path pointers remain positive.
                         (- 0 scratch)))))))))))))
    (defun nl_boot_image_open_pinned (path)
      (if (>= path 0) (nl_os_open_read path)
        (let* ((scratch (- 0 path))
               (image (heap-read-u64 scratch 67680))
               (copy (+ scratch 67688))
               (process (extern-call GetCurrentProcess)))
          ;; The loaders close their input independently. Duplicate the checked
          ;; handle, rewind its shared file position, and never reopen a path.
          (if (or (= (extern-call SetFilePointerEx image 0 0 0) 0)
                  (= (extern-call DuplicateHandle process image process copy 0 0 2) 0))
              -1 (heap-read-u64 copy 0)))))
    (defun nl_boot_image_read_header (path header fd)
      (let ((n (nl_fa_read_all fd header 64 0)))
        (seq
         (if (>= path 0) 0
           (let ((cached (+ (- 0 path) 67712)) (i 0)
                 (size (heap-read-u64 header 8))
                 (globals (heap-read-u64 header 32)))
             (seq
              ;; Retain the validated header outside the restored arena. The
              ;; globals install then has no second I/O operation that can
              ;; fail after the arena has already been published.
              (while (< i 64)
                (ptr-write-u8 cached i (heap-read-u8 header i))
                (setq i (+ i 1)))
              (if (or (/= n 64) (< globals 0) (< size 8) (> globals (- size 8)))
                  (ptr-write-u64 header 0 0) 0))))
         n)))
    (defun nl_boot_image_release (path)
      (if (>= path 0) 0
        (let ((scratch (- 0 path)))
          (seq (nl_os_close_handle (heap-read-u64 scratch 67680))
               (nl_os_free_chunk scratch 73728)))))))

(provide 'nelisp-boot-image)
