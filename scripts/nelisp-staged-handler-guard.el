;;; nelisp-staged-handler-guard.el --- check the handlers of staged GNU definitions -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; The standalone build does not load GNU's own files as a whole.  It stages
;; single definitions out of them by name (`nelisp-vendor-source-form',
;; `nelisp-vendor-source-forms', `nelisp-standalone--vendor-function-source',
;; and `nelisp-vendor-source-whole-file' for a whole file).  A staged
;; definition can carry compile-time declarations
;;
;;   (declare (compiler-macro HANDLER) (gv-expander HANDLER) (gv-setter HANDLER))
;;
;; and HANDLER, a lambda or the name of a function, calls helpers that the same
;; GNU file defines.  GNU loads the file whole, so those helpers always exist.
;; Here they exist only if somebody staged them as well.  The properties are
;; consulted late (a compiler macro when `load' expands eagerly, a gv-expander
;; when `setf' runs), so a missing helper stays hidden until then.  Batch F
;; found `keymap--compile-check' and `define-keymap--compile' void that way.
;;
;; This guard closes the class.  It reads the build script and the pinned GNU
;; sources the script names (no build state, so it works on any revision's
;; tree), derives the declarations of every staged definition, collects the
;; functions each handler calls directly, and asks the final binary whether
;; they are defined.  A function the binary lacks fails the guard, unless stock
;; GNU Emacs 31.1 (`emacs -Q --batch', the build host) lacks it too: then the
;; handler fails the same way in GNU, the exception is allowed, and the report
;; records GNU's answer next to it.
;;
;; Scope, deliberately: the handler itself (when it is a symbol) and the
;; functions and macros its body calls directly.  The callees of those
;; callees are not followed; they are the helpers' own business.
;;
;; The build runs `nelisp-staged-handler-guard-run' after the cold image and
;; fails when the guard fails.  From the shell:
;;
;;   emacs --batch -Q -L src -L scripts -l nelisp-staged-handler-guard \
;;     -f nelisp-staged-handler-guard-batch -- ROOT BINARY [REPORT-FILE]
;;
;; ROOT is the tree whose build script and vendor/ directory describe what
;; BINARY staged.  Exit status 0: PASS, 1: FAIL, 2: usage or an unreadable input.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defconst nelisp-staged-handler-guard--extractors
  '(nelisp-vendor-source-form nelisp-vendor-source-forms
    nelisp-standalone--vendor-function-source nelisp-vendor-source-whole-file)
  "Functions of the build script that stage GNU source text.")

(defconst nelisp-staged-handler-guard--definers
  '(defun defmacro defsubst cl-defun cl-defmacro cl-defsubst)
  "Definition forms whose leading `declare' clauses the guard reads.")

(defconst nelisp-staged-handler-guard--kinds '(compiler-macro gv-expander gv-setter)
  "The declarations that name a handler consulted at expansion time.")

(defvar nelisp-staged-handler-guard--forms-cache (make-hash-table :test #'equal)
  "Top-level forms of the files read so far, keyed by absolute file name.")

(defun nelisp-staged-handler-guard--read-forms (file)
  "Return the top-level forms of FILE, read with the host reader."
  (or (gethash file nelisp-staged-handler-guard--forms-cache)
      (puthash
       file
       (with-temp-buffer
         (insert-file-contents file)
         (goto-char (point-min))
         (let ((max-lisp-eval-depth 20000) forms)
           (condition-case nil
               (while t (push (read (current-buffer)) forms))
             (end-of-file nil))
           (nreverse forms)))
       nelisp-staged-handler-guard--forms-cache)))

;;;; Which definitions the build stages

(defun nelisp-staged-handler-guard--literal-names (form)
  "Return the names a literal extraction FORM stages, :whole, or `unresolved'."
  (let ((head (car form)) (file (nth 1 form)) (arg (nth 2 form)))
    (cond
     ((not (stringp file)) 'unresolved)
     ((eq head 'nelisp-vendor-source-whole-file) :whole)
     ((not (eq (car-safe arg) 'quote)) 'unresolved)
     ((memq head '(nelisp-vendor-source-form nelisp-standalone--vendor-function-source))
      (and (symbolp (cadr arg)) (list (cadr arg))))
     ((eq head 'nelisp-vendor-source-forms)
      (and (listp (cadr arg)) (cadr arg))))))

(defun nelisp-staged-handler-guard-sites (root)
  "Return (SITES . UNRESOLVED) for the extraction calls in ROOT's build script.
SITES is a list of (RELATIVE-FILE . NAMES), NAMES a list of symbols or :whole.
UNRESOLVED lists (OWNER . FORM) for calls whose arguments are not literal.
The helpers that wrap the extractors (names starting with
`nelisp-standalone--vendor-') pass their arguments on and are not sites."
  (let ((script (expand-file-name "scripts/nelisp-standalone-build.el" root))
        (sites nil) (unresolved nil) (max-lisp-eval-depth 20000))
    (unless (file-readable-p script) (error "No build script at %s" script))
    (dolist (top (nelisp-staged-handler-guard--read-forms script))
      (let ((owner (if (and (memq (car-safe top) '(defun defmacro))
                            (symbolp (nth 1 top)))
                       (symbol-name (nth 1 top)) ""))
            (stack (list top)))
        (while stack
          (let ((form (pop stack)))
            (when (consp form)
              (when (memq (car form) nelisp-staged-handler-guard--extractors)
                (let ((names (nelisp-staged-handler-guard--literal-names form)))
                  (cond
                   ((and names (not (eq names 'unresolved)))
                    (push (cons (nth 1 form) names) sites))
                   ((not (string-prefix-p "nelisp-standalone--vendor-" owner))
                    (push (cons owner form) unresolved)))))
              (let ((rest form))
                (while (consp rest)
                  (when (consp (car rest)) (push (car rest) stack))
                  (setq rest (cdr rest)))))))))
    (cons (nreverse sites) (nreverse unresolved))))

;;;; Declarations of a definition

(defun nelisp-staged-handler-guard--declarations (definition)
  "Return the (KIND . HANDLER) pairs DEFINITION declares, in order.
Only the leading body forms count, as in `byte-run--parse-body': a docstring,
`declare' forms and `interactive' forms."
  (let ((body (nthcdr 3 definition)) (found nil))
    (while (and body
                (let ((form (car body)))
                  (or (stringp form)
                      (memq (car-safe form) '(declare interactive)))))
      (let ((form (pop body)))
        (when (eq (car-safe form) 'declare)
          (dolist (clause (cdr form))
            (when (memq (car-safe clause) nelisp-staged-handler-guard--kinds)
              (push (cons (car clause) (cadr clause)) found))))))
    (nreverse found)))

(defun nelisp-staged-handler-guard--definitions (forms names)
  "Return the definitions among FORMS that define one of NAMES (or all if :whole)."
  (cl-remove-if-not
   (lambda (form)
     (and (consp form)
          (memq (car form) nelisp-staged-handler-guard--definers)
          (symbolp (nth 1 form))
          (or (eq names :whole) (memq (nth 1 form) names))))
   forms))

;;;; The functions a handler calls directly

(defun nelisp-staged-handler-guard--calls-list (forms acc)
  "Add to ACC the functions called by each of FORMS."
  (while (consp forms)
    (setq acc (nelisp-staged-handler-guard--calls (car forms) acc))
    (setq forms (cdr forms)))
  acc)

(defun nelisp-staged-handler-guard--calls (form acc)
  "Add to ACC the functions a macroexpanded FORM calls or refers to with #'."
  (cond
   ((atom form) acc)
   ((eq (car form) 'quote) acc)
   ((eq (car form) 'function)
    (let ((f (cadr form)))
      (cond ((and f (symbolp f)) (cons f acc))
            ((eq (car-safe f) 'lambda)
             (nelisp-staged-handler-guard--calls-list (cddr f) acc))
            (t acc))))
   ((memq (car form) '(let let*))
    (dolist (binding (cadr form))
      (when (consp binding)
        (setq acc (nelisp-staged-handler-guard--calls-list (cdr binding) acc))))
    (nelisp-staged-handler-guard--calls-list (cddr form) acc))
   ((eq (car form) 'cond)
    (dolist (clause (cdr form))
      (setq acc (nelisp-staged-handler-guard--calls-list clause acc)))
    acc)
   ((eq (car form) 'condition-case)
    (setq acc (nelisp-staged-handler-guard--calls (nth 2 form) acc))
    (dolist (handler (nthcdr 3 form))
      (when (consp handler)
        (setq acc (nelisp-staged-handler-guard--calls-list (cdr handler) acc))))
    acc)
   ((and (symbolp (car form)) (special-form-p (car form)))
    (nelisp-staged-handler-guard--calls-list (cdr form) acc))
   ((symbolp (car form))
    (nelisp-staged-handler-guard--calls-list (cdr form) (cons (car form) acc)))
   ((eq (car-safe (car form)) 'lambda)
    (nelisp-staged-handler-guard--calls-list
     (cdr form) (nelisp-staged-handler-guard--calls-list (cddr (car form)) acc)))
   (t (nelisp-staged-handler-guard--calls-list form acc))))

(defun nelisp-staged-handler-guard--macros-in (tree)
  "Return the symbols in TREE that name macros in this Emacs."
  (let ((found nil) (stack (list tree)))
    (while stack
      (let ((x (pop stack)))
        (cond ((consp x)
               (while (consp x)
                 (push (car x) stack)
                 (setq x (cdr x)))
               (when x (push x stack)))
              ((and x (symbolp x) (macrop x)) (cl-pushnew x found)))))
    found))

(defun nelisp-staged-handler-guard--handler-callees (handler)
  "Return the symbols HANDLER needs: itself if a name, else what its body calls.
HANDLER is a symbol, a (lambda ARGS . BODY) form, or a (function ...) of either."
  (let ((h (if (eq (car-safe handler) 'function) (cadr handler) handler)))
    (cond
     ((and h (symbolp h)) (list h))
     ((eq (car-safe h) 'lambda)
      (let* ((body (cddr h))
             (expanded (condition-case err
                           (macroexpand-all (macroexp-progn body))
                         (error (error "Cannot macroexpand handler %S: %S" h err)))))
        (delete-dups
         (append (nelisp-staged-handler-guard--macros-in body)
                 (nreverse (nelisp-staged-handler-guard--calls expanded nil))))))
     (t (error "Unsupported handler form %S" handler)))))

;;;; Specs

(defun nelisp-staged-handler-guard-specs (root)
  "Return a plist describing the handlers of the definitions ROOT's build stages.
:specs lists (:owner SYMBOL :kind KIND :handler FORM :file FILE :callees SYMBOLS),
:sites the number of extraction calls, :files and :names the numbers of GNU
files and staged names, :missing the staged names with no definition form,
:unresolved the extraction calls the scan could not read."
  (let* ((scan (nelisp-staged-handler-guard-sites root))
         (files (make-hash-table :test #'equal))
         (names 0) (specs nil) (missing nil))
    (dolist (site (car scan))
      (let* ((file (car site))
             (path (expand-file-name file root))
             (forms (progn
                      (unless (file-readable-p path) (error "No vendored source %s" path))
                      (nelisp-staged-handler-guard--read-forms path)))
             (wanted (cdr site)))
        (puthash file t files)
        (unless (eq wanted :whole) (cl-incf names (length wanted)))
        (let ((defined (nelisp-staged-handler-guard--definitions forms wanted)))
          (unless (eq wanted :whole)
            (dolist (name wanted)
              ;; Variables, aliases and structs are staged by name as well;
              ;; only definitions with a body can declare handlers.
              (unless (or (cl-find name defined :key (lambda (d) (nth 1 d)))
                          (cl-find-if (lambda (f)
                                        (and (consp f) (consp (cdr f))
                                             (or (eq (nth 1 f) name)
                                                 (equal (nth 1 f) (list 'quote name))
                                                 (eq (car-safe (nth 1 f)) name))))
                                      forms))
                (push (cons file name) missing))))
          (dolist (definition defined)
            (dolist (declaration (nelisp-staged-handler-guard--declarations definition))
              (push (list :owner (nth 1 definition) :kind (car declaration)
                          :handler (cdr declaration) :file file
                          :callees (nelisp-staged-handler-guard--handler-callees
                                    (cdr declaration)))
                    specs))))))
    (list :specs (nreverse specs) :sites (length (car scan))
          :files (hash-table-count files) :names names
          :missing (nreverse missing) :unresolved (cdr scan))))

;;;; Asking the binaries

(defun nelisp-staged-handler-guard--clean-environment ()
  "Return the scrubbed environment the build uses to run the fresh binary."
  (list "PATH=/usr/bin:/bin" "LANG=C.UTF-8" "HOME=/nonexistent"))

(defun nelisp-staged-handler-guard--probe-void (binary symbols)
  "Return the members of SYMBOLS that BINARY does not define.
Runs BINARY on a generated file and fails closed if the probe does not finish."
  (let* ((probe (make-temp-file "staged-handler-probe-" nil ".el"))
         (errfile (make-temp-file "staged-handler-probe-err-"))
         (marker "STAGED-HANDLER-PROBE-DONE"))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-8-unix) (print-length nil) (print-level nil))
            (with-temp-file probe
              (insert ";; Generated by nelisp-staged-handler-guard.\n"
                      (format "(let ((n 0))\n  (dolist (s '%S)\n" symbols)
                      "    (setq n (1+ n))\n"
                      "    (unless (or (fboundp s) (and (fboundp 'special-form-p) (special-form-p s)))\n"
                      "      (princ (format \"VOID %S\\n\" s))))\n"
                      (format "  (princ (format \"%s %%d\\n\" n)))\n" marker))))
          (let ((process-environment (nelisp-staged-handler-guard--clean-environment))
                (default-directory temporary-file-directory)
                void done status)
            (with-temp-buffer
              (setq status (call-process binary nil (list (current-buffer) errfile) nil
                                         "--load" probe))
              (goto-char (point-min))
              (while (not (eobp))
                (cond
                 ((looking-at "VOID \\(.*\\)$")
                  (push (car (read-from-string (match-string 1))) void))
                 ((looking-at (concat marker " \\([0-9]+\\)$"))
                  (setq done (string-to-number (match-string 1)))))
                (forward-line 1)))
            (unless (and (eql status 0) (eql done (length symbols)))
              (error "Probe of %s did not finish: status %S, checked %S of %d; stderr: %s"
                     binary status done (length symbols)
                     (with-temp-buffer
                       (insert-file-contents errfile)
                       (truncate-string-to-width (buffer-string) 300))))
            (nreverse void)))
      (ignore-errors (delete-file probe))
      (ignore-errors (delete-file errfile)))))

(defun nelisp-staged-handler-guard--gnu-defined (symbols)
  "Return an alist (SYMBOL . DEFINED-P) from stock GNU Emacs, `-Q --batch'.
The Emacs that runs this code is the oracle only when it is not compiling:
the build has loaded bytecomp, so a clean child process answers instead."
  (when symbols
    (let ((emacs (expand-file-name invocation-name invocation-directory))
          (process-environment (nelisp-staged-handler-guard--clean-environment))
          (default-directory temporary-file-directory)
          (answers nil))
      (with-temp-buffer
        (unless (eql 0 (call-process emacs nil t nil "--batch" "-Q" "--eval"
                                     (format "(dolist (s '%S) (princ (format \"%%S %%S\\n\" s (and (fboundp s) t))))"
                                             symbols)))
          (error "GNU oracle failed: %s" (buffer-string)))
        (goto-char (point-min))
        (while (re-search-forward "^\\(.*\\) \\(t\\|nil\\)$" nil t)
          (push (cons (car (read-from-string (match-string 1)))
                      (equal (match-string 2) "t"))
                answers)))
      answers)))

(defun nelisp-staged-handler-guard--gnu-version ()
  "Return the first line of the oracle Emacs's version output."
  (with-temp-buffer
    (call-process (expand-file-name invocation-name invocation-directory)
                  nil t nil "--version")
    (goto-char (point-min))
    (buffer-substring (point) (line-end-position))))

;;;; The guard

(defun nelisp-staged-handler-guard-check (root binary)
  "Check the handlers ROOT's build stages against BINARY; return a result plist.
:ok is non-nil when the guard passes; :failures lists (OWNER KIND CALLEE FILE),
:allowed the same for callees void in GNU as well, :report is the text."
  (let* ((scan (nelisp-staged-handler-guard-specs root))
         (specs (plist-get scan :specs))
         (callees (delete-dups
                   (cl-loop for spec in specs append (plist-get spec :callees))))
         (void (nelisp-staged-handler-guard--probe-void binary callees))
         (gnu (nelisp-staged-handler-guard--gnu-defined void))
         (failures nil) (allowed nil))
    (dolist (spec specs)
      (dolist (callee (plist-get spec :callees))
        (when (memq callee void)
          (let ((row (list (plist-get spec :owner) (plist-get spec :kind) callee
                           (plist-get spec :file))))
            (if (cdr (assq callee gnu)) (push row failures) (push row allowed))))))
    (setq failures (nreverse failures) allowed (nreverse allowed))
    (let* ((problems (append
                      (and (plist-get scan :unresolved) (list 'unresolved))
                      (and (plist-get scan :missing) (list 'missing))
                      (and (null specs) (list 'no-declarations))
                      (and failures (list 'void))))
           (ok (null problems))
           (counts (lambda (kind) (cl-count kind specs
                                           :key (lambda (s) (plist-get s :kind)))))
           (report
            (with-temp-buffer
              (insert (format "staged-handler-guard: %s\n" (if ok "PASS" "FAIL"))
                      (format "root: %s\nbinary: %s\n" (expand-file-name root) binary)
                      (format "extraction calls: %d, GNU files: %d, staged names: %d\n"
                              (plist-get scan :sites) (plist-get scan :files)
                              (plist-get scan :names))
                      (format "declarations: %d (compiler-macro %d, gv-expander %d, gv-setter %d)\n"
                              (length specs) (funcall counts 'compiler-macro)
                              (funcall counts 'gv-expander) (funcall counts 'gv-setter))
                      (format "direct callees checked: %d; void in the binary: %d (%d also void in GNU, allowed)\n"
                              (length callees) (length void) (length allowed)))
              (dolist (row failures)
                (insert (format "FAIL %S %S needs %S, which the binary lacks and GNU defines (%s)\n"
                                (nth 0 row) (nth 1 row) (nth 2 row) (nth 3 row))))
              (dolist (row allowed)
                (insert (format "ALLOWED %S %S calls %S: void in the binary and in %s `-Q --batch' (fboundp => nil) (%s)\n"
                                (nth 0 row) (nth 1 row) (nth 2 row)
                                (nelisp-staged-handler-guard--gnu-version) (nth 3 row))))
              (dolist (u (plist-get scan :unresolved))
                (insert (format "FAIL extraction call in %s has non-literal arguments: %S\n"
                                (car u) (cdr u))))
              (dolist (m (plist-get scan :missing))
                (insert (format "FAIL staged name %S not found in %s\n" (cdr m) (car m))))
              (when (null specs)
                (insert "FAIL no declarations found: the scan reads nothing\n"))
              (buffer-string))))
      (list :ok ok :failures failures :allowed allowed :report report
            :declarations (length specs) :callees (length callees)))))

(defun nelisp-staged-handler-guard-run (root binary &optional report-file)
  "Run the guard on BINARY for the tree ROOT; signal an error when it fails.
Writes the report to REPORT-FILE if given, and returns the result plist."
  (let ((result (nelisp-staged-handler-guard-check root binary)))
    (when report-file
      (let ((coding-system-for-write 'utf-8-unix))
        (with-temp-file report-file (insert (plist-get result :report)))))
    (message "[staged-handler-guard] %s"
             (string-trim (car (split-string (plist-get result :report) "\n"))))
    (unless (plist-get result :ok)
      (error "Staged GNU handler guard failed:\n%s" (plist-get result :report)))
    result))

(defun nelisp-staged-handler-guard-batch ()
  "Command-line entry: -f nelisp-staged-handler-guard-batch -- ROOT BINARY [REPORT]."
  (let ((args (cdr (member "--" command-line-args-left))))
    (setq command-line-args-left nil)
    (unless (member (length args) '(2 3))
      (princ "usage: emacs --batch -Q -L src -L scripts -l nelisp-staged-handler-guard -f nelisp-staged-handler-guard-batch -- ROOT BINARY [REPORT]\n")
      (kill-emacs 2))
    (let ((result (condition-case err
                      (nelisp-staged-handler-guard-check (nth 0 args) (nth 1 args))
                    (error (princ (format "staged-handler-guard: ERROR %s\n"
                                          (error-message-string err)))
                           (kill-emacs 2)))))
      (when (nth 2 args)
        (let ((coding-system-for-write 'utf-8-unix))
          (with-temp-file (nth 2 args) (insert (plist-get result :report)))))
      (princ (plist-get result :report))
      (kill-emacs (if (plist-get result :ok) 0 1)))))

(provide 'nelisp-staged-handler-guard)

;;; nelisp-staged-handler-guard.el ends here
