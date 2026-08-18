;;; nelisp-artifact-test.el --- ERT for Doc 142 .nelc artifacts  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'nelisp-artifact)
(require 'nelisp-elf-write)
(require 'nelisp-runtime-image)

;; Declared special so the version-pin test can dynamically bind it even
;; when `nelisp-cli' (which `defvar's it with a value) is not loaded.
(defvar nelisp--cli-version)
(declare-function nelisp-cli-main "nelisp-cli" (argv))

(defun nelisp-artifact-test--definition-macro-source ()
  "Return a source fixture with supported, unsupported, and malformed inline defs."
  (concat
   "(defmacro define-inline (name args &rest body)\n"
   "  (if (eq name 'nelisp-artifact-test--bad-inline)\n"
   "      `(progn (put ',name 'nelisp-artifact-test--bad-inline-replayed t))\n"
   "    (let ((doc (and (stringp (car-safe body)) (list (pop body))))\n"
   "          (decl (and (consp body)\n"
   "                     (consp (car body))\n"
   "                     (eq (caar body) 'declare)\n"
   "                     (list (pop body)))))\n"
   "      `(defun ,name ,args ,@doc ,@decl ,@body))))\n"
   "(defmacro nelisp-artifact-test--wrap-defun (name args &rest body)\n"
   "  `(defun ,name ,args ,@body))\n"
   "(defmacro nelisp-artifact-test--wrap-inline (name args &rest body)\n"
   "  `(define-inline ,name ,args ,@body))\n"
   "(defsubst nelisp-artifact-test--defsubst-good (x)\n"
   "  \"defsubst doc\"\n"
   "  (declare (indent defun))\n"
   "  (+ x 1))\n"
   "(define-inline nelisp-artifact-test--inline-good (x)\n"
   "  \"inline doc\"\n"
   "  (declare (indent defun))\n"
   "  (+ x 2))\n"
   "(nelisp-artifact-test--wrap-defun nelisp-artifact-test--wrapped (x)\n"
   "  (+ x 3))\n"
   "(nelisp-artifact-test--wrap-inline nelisp-artifact-test--inline-wrapped (x)\n"
   "  (+ x 4))\n"
   "(define-inline nelisp-artifact-test--bad-inline (x)\n"
   "  (+ x 5))\n"
   "(provide 'nelisp-artifact-test--definition-macros)\n"))

(defun nelisp-artifact-test--module-item-name (item)
  "Return the logical name encoded in module ITEM."
  (cond
   ((and (consp item) (eq (car item) :fn))
    (nth 1 item))
   ((and (consp item) (eq (car item) :eval))
    (let ((form (nth 1 item)))
      (cond
       ((symbolp form) form)
       ((and (consp form) (symbolp (nth 1 form)))
        (nth 1 form))
       (t nil))))
   (t nil)))

(defun nelisp-artifact-test--find-module-item (name module)
  "Return the module ITEM named NAME, or nil."
  (cl-find-if (lambda (item)
                (eq (nelisp-artifact-test--module-item-name item) name))
              module))

(ert-deftest nelisp-artifact/activate-load-paths-preserves-priority-and-order ()
  "Manifest paths lead globally while invalid and duplicate entries disappear."
  (let ((load-path '("existing-a" "duplicate" "existing-a" nil)))
    (nelisp-artifact--activate-load-paths
     '("manifest-a" nil 42 "" "duplicate" "manifest-a"))
    (should (equal load-path
                   '("manifest-a" "duplicate" "existing-a" nil)))))

(ert-deftest nelisp-artifact/read-manifest-for-load-omits-native-metadata ()
  "The load manifest keeps paths but never materializes native metadata."
  (let* ((temp-dir (make-temp-file "nelisp-artifact-manifest-path-" t))
         (artifact-path (expand-file-name "mod.el.nelc" temp-dir))
         (manifest-path
          (nelisp-artifact--sibling-manifest-path artifact-path))
         (recorded-path (expand-file-name "dependency" temp-dir)))
    (unwind-protect
        (progn
          (write-region "" nil artifact-path nil 'silent)
          (write-region
           (concat "(:format nelisp-elisp-artifact-manifest-v1"
                   " :load-path " (prin1-to-string (list recorded-path))
                   " :native (:symbols (\"tampered\") :defuns nil)"
                   ")\n")
           nil manifest-path nil 'silent)
          (let ((nelisp-artifact-fast-private-read t))
            (cl-letf (((symbol-function 'nelisp-artifact--read-manifest-full)
                       (lambda (&rest _)
                         (error "fast manifest reader must not fall back")))
                      ((symbol-function
                        'nelisp-artifact--read-private-native-metadata)
                       (lambda (&rest _)
                         (error "load validation must not read native sidecar metadata"))))
              (let ((manifest
                     (nelisp-artifact--read-manifest-for-load artifact-path)))
                (should (equal (plist-get manifest :load-path)
                               (list recorded-path)))
                (should-not (plist-member manifest :native))
                (should-not (plist-member manifest :native-sections))))))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/validate-and-load-skip-manifest-native-metadata ()
  "Fast `.neln' validation and replay do not enter native sidecar readers."
  (let* ((temp-dir (make-temp-file "nelisp-artifact-no-manifest-native-" t))
         (source-path (expand-file-name "mod.el" temp-dir))
         (artifact-path (concat source-path ".neln"))
         (nelisp-artifact--loaded nil)
         (nelisp-artifact-fast-private-read t)
         (nelisp-artifact-native-dispatch-enabled nil))
    (unwind-protect
        (progn
          (write-region
           "(defun no-manifest-native-f (x) (+ x 1))\n(provide 'no-manifest-native)\n"
           nil source-path nil 'silent)
          (nelisp-artifact-compile-file
           source-path artifact-path nil nil nil nil nil 'neln)
          (cl-letf
              (((symbol-function 'nelisp-artifact--read-manifest-full)
                (lambda (&rest _)
                  (error "full manifest reader must not run")))
               ((symbol-function
                 'nelisp-artifact--read-private-native-metadata)
                (lambda (&rest _)
                  (error "native sidecar metadata reader must not run")))
               ((symbol-function 'nelisp-artifact--parse-payload)
                (lambda (&rest _)
                  (error "fast artifact load must not fall back"))))
            (nelisp-artifact-load-file artifact-path))
          (should (= (nelisp-eval '(no-manifest-native-f 41)) 42)))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/load-activates-manifest-path-before-first-require ()
  "The first artifact form can require a manifest-only dependency path."
  (let* ((temp-dir (make-temp-file "nelisp-artifact-load-path-" t))
         (dependency-dir (expand-file-name "dependency" temp-dir))
         (dependency-path
          (expand-file-name "nelisp-artifact-manifest-dependency.el"
                            dependency-dir))
         (source-path (expand-file-name "module.el" temp-dir))
         (artifact-path (concat source-path ".nelc"))
         (dependency-feature 'nelisp-artifact-manifest-dependency)
         (module-feature 'nelisp-artifact-manifest-module)
         (original-load-path load-path)
         (original-nelisp-load-path nelisp-load-path)
         (original-include-host nelisp-load-path-include-host))
    (unwind-protect
        (progn
          (make-directory dependency-dir t)
          (write-region
           "(defun nelisp-artifact-manifest-dependency-value () 41)\n\
(provide 'nelisp-artifact-manifest-dependency)\n"
           nil dependency-path nil 'silent)
          ;; REQUIRE is deliberately the first top-level module item.
          (write-region
           "(require 'nelisp-artifact-manifest-dependency)\n\
(defvar nelisp-artifact-manifest-module-value\n\
  (nelisp-artifact-manifest-dependency-value))\n\
(provide 'nelisp-artifact-manifest-module)\n"
           nil source-path nil 'silent)
          (nelisp-artifact-compile-file
           source-path artifact-path nil nil (list dependency-dir))
          (setq features (delq dependency-feature features)
                features (delq module-feature features)
                load-path (delete dependency-dir load-path)
                nelisp-load-path nil
                nelisp-load-path-include-host t)
          (nelisp--reset)
          (setq nelisp-artifact--loaded nil)
          (should-not (member dependency-dir load-path))
          (nelisp-artifact-load-file artifact-path)
          (should (equal (car load-path) dependency-dir))
          (should (= (nelisp-eval 'nelisp-artifact-manifest-module-value) 41))
          ;; The manifest path persists for requires performed after replay.
          (should (member dependency-dir load-path)))
      (setq load-path original-load-path
            nelisp-load-path original-nelisp-load-path
            nelisp-load-path-include-host original-include-host
            features (delq dependency-feature features)
            features (delq module-feature features))
      (when (boundp 'nelisp-artifact-manifest-module-value)
        (makunbound 'nelisp-artifact-manifest-module-value))
      (when (fboundp 'nelisp-artifact-manifest-dependency-value)
        (fmakunbound 'nelisp-artifact-manifest-dependency-value))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/gate-1-loads-without-source ()
  "Doc 142 gate 1-3: compile a module, then load the `.nelc' in a fresh
NeLisp runtime WITHOUT its source, and verify the function cell (now a
precompiled bytecode closure), value cell, property, and feature were
all materialized.  Checked through `nelisp-eval' — §6.1 replays onto
the NeLisp runtime, not the host."
  (let* ((temp-dir (make-temp-file "nelisp-artifact-" t))
         (source-path (expand-file-name "sample.el" temp-dir))
         (artifact-path (concat source-path ".nelc"))
         (manifest-path (concat artifact-path ".manifest.el"))
         (moved-source-path (expand-file-name "sample.el.gone" temp-dir))
         (source
          "(defvar nelisp-artifact-test--sample-var 5)
(defun nelisp-artifact-test--sample-fn (x)
  (+ x nelisp-artifact-test--sample-var))
(put 'nelisp-artifact-test--sample-symbol
     'nelisp-artifact-test--sample-prop
     'ready)
(provide 'nelisp-artifact-test--sample-feature)\n"))
    (unwind-protect
        (progn
          (write-region source nil source-path nil 'silent)
          (should
           (equal (plist-get
                   (nelisp-artifact-compile-file source-path artifact-path)
                   :format)
                  'nelisp-elisp-artifact-manifest-v1))
          (should (file-exists-p artifact-path))
          (should (file-exists-p manifest-path))
          (rename-file source-path moved-source-path t)
          (should-not (file-exists-p source-path))
          ;; fresh NeLisp runtime, source gone
          (nelisp--reset)
          (setq nelisp-artifact--loaded nil)
          (nelisp-artifact-load-file artifact-path)
          (should (nelisp-eval '(fboundp 'nelisp-artifact-test--sample-fn)))
          (should (= (nelisp-eval '(nelisp-artifact-test--sample-fn 2)) 7))
          (should (= (nelisp-eval 'nelisp-artifact-test--sample-var) 5))
          (should (eq (nelisp-eval '(get 'nelisp-artifact-test--sample-symbol
                                         'nelisp-artifact-test--sample-prop))
                      'ready))
          (should (nelisp-eval '(featurep 'nelisp-artifact-test--sample-feature))))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/gate-4-load-time-table-materialization ()
  "Doc 142 §3 / gate 4: an artifact must reproduce load-time table
materialization.  The literal `#s(hash-table ...)' reader syntax is
not yet supported by the NeLisp reader, so this exercises the same
semantic via `make-hash-table'/`puthash' (the runtime effect a
generated table file produces), verified through the NeLisp runtime."
  (let* ((temp-dir (make-temp-file "nelisp-artifact-ht-" t))
         (source-path (expand-file-name "table.el" temp-dir))
         (artifact-path (concat source-path ".nelc"))
         (source
          "(defvar nelisp-artifact-test--table
  (let ((h (make-hash-table :test 'equal)))
    (puthash \"a\" 1 h)
    (puthash \"b\" (list 2 3) h)
    h))
(provide 'nelisp-artifact-test--table-feature)\n"))
    (unwind-protect
        (progn
          (write-region source nil source-path nil 'silent)
          (nelisp-artifact-compile-file source-path artifact-path)
          ;; Move the source away to prove the table is materialized
          ;; from the artifact, not re-read from source.
          (rename-file source-path (concat source-path ".gone") t)
          (nelisp--reset)
          (setq nelisp-artifact--loaded nil)
          (nelisp-artifact-load-file artifact-path)
          (should (= (nelisp-eval '(gethash "a" nelisp-artifact-test--table)) 1))
          (should (equal (nelisp-eval '(gethash "b" nelisp-artifact-test--table))
                         '(2 3)))
          (should (nelisp-eval '(featurep 'nelisp-artifact-test--table-feature))))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/gate-7-rejects-stale-after-source-change ()
  "Doc 142 §7 / gate 7: after the source content changes, loading the
old artifact must be rejected (signal `nelisp-artifact-stale') BEFORE
any module init runs — the side effect must NOT be applied."
  (let* ((temp-dir (make-temp-file "nelisp-artifact-stale-" t))
         (source-path (expand-file-name "mod.el" temp-dir))
         (artifact-path (concat source-path ".nelc"))
         (var 'nelisp-artifact-test--stale-var)
         (feature 'nelisp-artifact-test--stale-feature))
    (unwind-protect
        (progn
          (when (boundp var) (makunbound var))
          (setq features (delq feature features))
          (write-region
           "(defvar nelisp-artifact-test--stale-var 1)
(provide 'nelisp-artifact-test--stale-feature)\n"
           nil source-path nil 'silent)
          (nelisp-artifact-compile-file source-path artifact-path)
          ;; Mutate the source in place: artifact is now stale.
          (write-region
           "(defvar nelisp-artifact-test--stale-var 999)
(provide 'nelisp-artifact-test--stale-feature)\n"
           nil source-path nil 'silent)
          (setq nelisp-artifact--loaded nil)
          (should-error (nelisp-artifact-load-file artifact-path)
                        :type 'nelisp-artifact-stale)
          ;; Rejected before module init: the var must not be bound.
          (should-not (boundp var)))
      (when (boundp var) (makunbound var))
      (setq features (delq feature features))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/source-transform-applies-and-raw-source-stales ()
  "A source transform changes the compiled artifact, but raw source
freshness still invalidates the manifest when the file changes."
  (let* ((temp-dir (make-temp-file "nelisp-artifact-transform-" t))
         (source-path (expand-file-name "mod.el" temp-dir))
         (artifact-path (concat source-path ".nelc"))
         (transform-source nil)
         (transform-path nil)
         (source-a "(defvar nelisp-artifact-test--transform-var 1)\n")
         (source-b "(defvar nelisp-artifact-test--transform-var 2)\n;; size bump to avoid same-length source\n"))
    (unwind-protect
        (progn
          (write-region
           (concat source-a "(provide 'nelisp-artifact-test--transform-feature)\n")
           nil source-path nil 'silent)
         (let ((nelisp-artifact-source-transform-function
                 (lambda (source path)
                   (setq transform-source source
                         transform-path path)
                   (let ((transformed
                          (concat
                           "(defvar nelisp-artifact-test--transform-var 7)\n"
                           "(provide 'nelisp-artifact-test--transform-feature)\n")))
                     transformed))))
            (nelisp-artifact-compile-file source-path artifact-path))
          (should (equal transform-path source-path))
          (should (string-match-p
                   (regexp-quote source-a)
                   transform-source))
          (nelisp--reset)
          (setq nelisp-artifact--loaded nil)
          (nelisp-artifact-load-file artifact-path)
          (should (= (nelisp-eval 'nelisp-artifact-test--transform-var) 7))
          (write-region
           (concat source-b
                   "(provide 'nelisp-artifact-test--transform-feature)\n")
           nil source-path nil 'silent)
          (setq nelisp-artifact--loaded nil)
          (should-error (nelisp-artifact-load-file artifact-path)
                        :type 'nelisp-artifact-stale))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/parse-compile-args-rewrite-defalias-late-flag ()
  "The compile parser accepts the value-less rewrite flag and defaults to nil."
  (let* ((base (list "compile-elisp-artifact"
                     "--kind" "nelc"
                     "--input" "input.el"
                     "--output" "output.nelc"))
         (default-opts (nelisp-artifact--parse-compile-args base))
         (flagged-opts
          (nelisp-artifact--parse-compile-args
           (append base (list "--rewrite-defalias-late")))))
    (should (null (plist-get default-opts :rewrite-defalias-late)))
    (should (eq (plist-get flagged-opts :rewrite-defalias-late) t))
    (should-error
     (nelisp-artifact--parse-compile-args
      (append base (list "--unexpected-flag"))))))

(ert-deftest nelisp-artifact/rewrite-defalias-late-recurses-through-wrappers ()
  "Rewrite only wrapper heads, preserving quoted and function subtrees."
  (let* ((form
          '(progn
            (defalias 'top-a 'top-b)
            (when ready
              (defalias 'when-a 'when-b))
            (let ((x (defalias 'let-a 'let-b))
                  (y 2))
              (if ok
                  (defalias 'if-a 'if-b)
                (while looping
                  (defalias 'while-a 'while-b))))
            (condition-case err
                (defalias 'cc-a 'cc-b)
              (error (defalias 'handler-a 'handler-b)))
            (quote (defalias 'quoted-a 'quoted-b))
            (function (defalias 'fn-a 'fn-b))))
         (expected
          '(progn
            (nelisp--defalias-late (quote top-a) (quote top-b))
            (when ready
              (nelisp--defalias-late (quote when-a) (quote when-b)))
            (let ((x (nelisp--defalias-late (quote let-a) (quote let-b)))
                  (y 2))
              (if ok
                  (nelisp--defalias-late (quote if-a) (quote if-b))
                (while looping
                  (nelisp--defalias-late (quote while-a) (quote while-b)))))
            (condition-case err
                (nelisp--defalias-late (quote cc-a) (quote cc-b))
              (error (nelisp--defalias-late (quote handler-a) (quote handler-b))))
            (quote (defalias (quote quoted-a) (quote quoted-b)))
            (function (defalias (quote fn-a) (quote fn-b))))))
    (should (equal (nelisp-artifact--rewrite-defalias-late-form form)
                   expected))))

(ert-deftest nelisp-artifact/compile-file-rewrites-defalias-late-before-native-and-module-use ()
  "Compile-time rewrite reaches both the module replay list and native input.
Legacy compilation without the flag remains unchanged."
  (let* ((temp-dir (make-temp-file "nelisp-artifact-rewrite-flow-" t))
         (source-path (expand-file-name "rewrite-flow.el" temp-dir))
         (rewrite-artifact-path (expand-file-name "rewrite-flow.neln" temp-dir))
         (legacy-artifact-path (expand-file-name "legacy-flow.neln" temp-dir))
         (source
          "(defalias 'top-a 'top-b)
(progn
  (when ready
    (defalias 'when-a 'when-b))
  (let ((x (defalias 'let-a 'let-b))
        (y 2))
    (if ok
        (defalias 'if-a 'if-b)
      (while looping
        (defalias 'while-a 'while-b))))
  (quote (defalias 'quoted-a 'quoted-b))
  (function (defalias 'fn-a 'fn-b)))
(provide 'rewrite-flow)\n")
         (raw-forms nil)
         (rewrite-forms nil)
         (rewrite-native-forms nil)
         (legacy-native-forms nil))
    (unwind-protect
        (progn
          (write-region source nil source-path nil 'silent)
          (setq raw-forms (nelisp-artifact--read-top-level-forms source source-path))
         (setq rewrite-forms
                (nelisp-artifact--rewrite-defalias-late-forms raw-forms))
          (cl-letf (((symbol-function 'nelisp-artifact--ensure-native-compiler)
                     (lambda () t))
                    ((symbol-function 'nelisp-artifact--native-compile-section)
                     (lambda (forms _target &optional _native-policy)
                       (if nelisp-artifact--rewrite-defalias-late
                           (setq rewrite-native-forms forms)
                         (setq legacy-native-forms forms))
                       (setq nelisp-artifact--last-native-compile-report nil)
                       nil)))
            (let ((nelisp-artifact--rewrite-defalias-late t))
              (nelisp-artifact-compile-file
               source-path rewrite-artifact-path nil nil nil nil nil 'neln))
            (let* ((rewrite-payload
                    (nelisp-artifact--read-payload rewrite-artifact-path))
                   (rewrite-module (plist-get rewrite-payload :module-init)))
              (should (equal rewrite-native-forms rewrite-forms))
              (should
               (equal
                rewrite-module
                '((:eval (nelisp--defalias-late (quote top-a)
                                                (quote top-b)))
                  (:eval (progn
                           (when ready
                             (nelisp--defalias-late (quote when-a)
                                                    (quote when-b)))
                           (let ((x (nelisp--defalias-late (quote let-a)
                                                            (quote let-b)))
                                 (y 2))
                             (if ok
                                 (nelisp--defalias-late (quote if-a)
                                                        (quote if-b))
                               (while looping
                                 (nelisp--defalias-late (quote while-a)
                                                        (quote while-b)))))
                           (quote (defalias (quote quoted-a)
                                            (quote quoted-b)))
                           (function (defalias (quote fn-a)
                                               (quote fn-b)))))
                  (:eval (provide (quote rewrite-flow)))))))
            (setq rewrite-native-forms nil)
            (let ((nelisp-artifact--rewrite-defalias-late nil))
              (nelisp-artifact-compile-file
               source-path legacy-artifact-path nil nil nil nil nil 'neln))
            (let* ((legacy-payload
                    (nelisp-artifact--read-payload legacy-artifact-path))
                   (legacy-module (plist-get legacy-payload :module-init)))
              (should (equal legacy-native-forms raw-forms))
              (should
               (equal
                legacy-module
                '((:eval (defalias (quote top-a) (quote top-b)))
                  (:eval (progn
                           (when ready
                             (defalias (quote when-a) (quote when-b)))
                           (let ((x (defalias (quote let-a) (quote let-b)))
                                 (y 2))
                             (if ok
                                 (defalias (quote if-a) (quote if-b))
                               (while looping
                                 (defalias (quote while-a) (quote while-b)))))
                           (quote (defalias (quote quoted-a)
                                            (quote quoted-b)))
                           (function (defalias (quote fn-a)
                                               (quote fn-b)))))
                  (:eval (provide (quote rewrite-flow))))))))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t))))))

(ert-deftest nelisp-artifact/compile-cli-binds-rewrite-defalias-late ()
  "The direct compile CLI and runtime-image CLI both thread the new flag."
  (let* ((temp-dir (make-temp-file "nelisp-artifact-cli-bind-" t))
         (source-path (expand-file-name "source.el" temp-dir))
         (artifact-path (expand-file-name "source.neln" temp-dir))
         (image-path (expand-file-name "runtime.nlri" temp-dir))
         (image-artifact-path (expand-file-name "runtime.nelc" temp-dir))
         (compile-file-value nil)
         (runtime-file-value nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'nelisp-artifact--standalone-host-helper-compile)
                     (lambda (&rest _) nil))
                    ((symbol-function 'nelisp-artifact-compile-file)
                     (lambda (&rest _)
                       (setq compile-file-value
                             nelisp-artifact--rewrite-defalias-late)
                       0))
                    ((symbol-function 'nelisp-artifact-compile-runtime-image-file)
                     (lambda (&rest _)
                       (setq runtime-file-value
                             nelisp-artifact--rewrite-defalias-late)
                       0)))
            (should (= 0 (compile-elisp-artifact
                          (list "compile-elisp-artifact"
                                "--kind" "neln"
                                "--input" source-path
                                "--output" artifact-path
                                "--rewrite-defalias-late"))))
            (should compile-file-value)
            (setq compile-file-value nil)
            (should (= 0 (compile-elisp-artifact
                          (list "compile-elisp-artifact"
                                "--kind" "neln"
                                "--input" source-path
                                "--output" artifact-path))))
            (should-not compile-file-value)
            (should (= 0 (compile-runtime-image
                          (list "compile-runtime-image"
                                "--kind" "nelc"
                                "--input" image-path
                                "--output" image-artifact-path
                                "--rewrite-defalias-late"))))
            (should runtime-file-value)
            (setq runtime-file-value nil)
            (should (= 0 (compile-runtime-image
                          (list "compile-runtime-image"
                                "--kind" "nelc"
                                "--input" image-path
                                "--output" image-artifact-path))))
            (should-not runtime-file-value)))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/defalias-late-is-callable-and-aliases ()
  "The symbol the rewrite emits must exist and alias like `defalias'.
The two tests above only assert the substitution, so an emitted-but-undefined
`nelisp--defalias-late' passed them while every artifact compiled with
`--rewrite-defalias-late' died at replay with
\(nelisp-void-function nelisp--defalias-late)."
  (let* ((temp-dir (make-temp-file "nelisp-artifact-defalias-late-" t))
         (source-path (expand-file-name "defalias-late.el" temp-dir))
         (artifact-path (expand-file-name "defalias-late.nelc" temp-dir))
         (feature 'nelisp-artifact-test--defalias-late-feature)
         (names '(nelisp-artifact-test--late-base
                  nelisp-artifact-test--late-alias
                  nelisp-artifact-test--late-direct)))
    (unwind-protect
        (progn
          (should (fboundp 'nelisp--defalias-late))
          (setq features (delq feature features))
          (write-region
           "(defun nelisp-artifact-test--late-base (x) (* x 2))
(defalias 'nelisp-artifact-test--late-alias 'nelisp-artifact-test--late-base)
(provide 'nelisp-artifact-test--defalias-late-feature)\n"
           nil source-path nil 'silent)
          (let ((nelisp-artifact--rewrite-defalias-late t))
            (nelisp-artifact-compile-file source-path artifact-path))
          ;; The module really carries the rewritten call ...
          (should
           (member '(:eval (nelisp--defalias-late
                            (quote nelisp-artifact-test--late-alias)
                            (quote nelisp-artifact-test--late-base)))
                   (plist-get (nelisp-artifact--read-payload artifact-path)
                              :module-init)))
          ;; ... and replaying it installs a working alias.  `nelisp--reset'
          ;; first, so the entry has to come back from
          ;; `nelisp--install-primitives' rather than from an earlier test.
          (nelisp--reset)
          (setq nelisp-artifact--loaded nil)
          (nelisp-artifact-load-file artifact-path)
          (should (eq (gethash 'nelisp-artifact-test--late-alias
                               nelisp--functions)
                      'nelisp-artifact-test--late-base))
          (should (= (nelisp-eval '(nelisp-artifact-test--late-alias 21)) 42))
          ;; The runtime function cell is the entry point the replay routes
          ;; that use the plain runtime `eval' take, so it must alias too.
          (should (eq (nelisp--defalias-late 'nelisp-artifact-test--late-direct
                                             'nelisp-artifact-test--late-base)
                      'nelisp-artifact-test--late-direct))
          (should (= (nelisp-eval '(nelisp-artifact-test--late-direct 3)) 6)))
      (setq features (delq feature features))
      (dolist (name names)
        (when (fboundp name) (fmakunbound name)))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/integrity-rejects-corrupted-artifact ()
  "Doc 142 §7: a corrupted artifact (bytes no longer matching the
manifest `:artifact-sha256') must be rejected as
`nelisp-artifact-invalid' before module init."
  (let* ((temp-dir (make-temp-file "nelisp-artifact-corrupt-" t))
         (source-path (expand-file-name "mod.el" temp-dir))
         (artifact-path (concat source-path ".nelc"))
         (feature 'nelisp-artifact-test--corrupt-feature))
    (unwind-protect
        (progn
          (setq features (delq feature features))
          (write-region
           "(defvar nelisp-artifact-test--corrupt-var 7)
(provide 'nelisp-artifact-test--corrupt-feature)\n"
           nil source-path nil 'silent)
          (nelisp-artifact-compile-file source-path artifact-path)
          ;; Tamper with the artifact bytes (append a byte) so the
          ;; recorded sha256 no longer matches.
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region "\n;; tampered\n" nil artifact-path t 'silent))
          (setq nelisp-artifact--loaded nil)
          (should-error (nelisp-artifact-load-file artifact-path)
                        :type 'nelisp-artifact-invalid))
      (setq features (delq feature features))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/gate-5-rejects-version-mismatch ()
  "Doc 142 §5: an artifact compiled under one concrete nelisp-version
must be rejected when loaded under a different concrete version (the
pin is skipped only when a side is the placeholder \"unknown\")."
  (let* ((temp-dir (make-temp-file "nelisp-artifact-ver-" t))
         (source-path (expand-file-name "mod.el" temp-dir))
         (artifact-path (concat source-path ".nelc"))
         (feature 'nelisp-artifact-test--ver-feature))
    (unwind-protect
        (progn
          (setq features (delq feature features))
          (write-region
           "(defvar nelisp-artifact-test--ver-var 1)
(provide 'nelisp-artifact-test--ver-feature)\n"
           nil source-path nil 'silent)
          (let ((nelisp--cli-version "1.0.0"))
            (nelisp-artifact-compile-file source-path artifact-path))
          (setq nelisp-artifact--loaded nil)
          (let ((nelisp--cli-version "2.0.0"))
            (should-error (nelisp-artifact-load-file artifact-path)
                          :type 'nelisp-artifact-invalid)))
      (setq features (delq feature features))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/rejects-compiler-format-mismatch ()
  "Doc 142 §5: an artifact whose manifest `:compiler' descriptor no
longer matches the current compiler must be rejected before init."
  (let* ((temp-dir (make-temp-file "nelisp-artifact-cc-" t))
         (source-path (expand-file-name "mod.el" temp-dir))
         (artifact-path (concat source-path ".nelc"))
         (manifest-path (concat artifact-path ".manifest.el")))
    (unwind-protect
        (progn
          (write-region
           "(defvar nelisp-artifact-test--cc-var 1)\n" nil source-path nil 'silent)
          (nelisp-artifact-compile-file source-path artifact-path)
          ;; Patch the (un-hashed) manifest to a stale compiler version.
          (let ((m (nelisp-artifact-read-manifest artifact-path)))
            (setq m (plist-put m :compiler
                               (plist-put (copy-sequence
                                           (plist-get m :compiler))
                                          :bytecode-version 99)))
            (with-temp-file manifest-path
              (insert (prin1-to-string m) "\n")))
          (setq nelisp-artifact--loaded nil)
          (should-error (nelisp-artifact-load-file artifact-path)
                        :type 'nelisp-artifact-invalid))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/rejects-changed-preload ()
  "Doc 142 §5: a recorded preload that changed on disk must invalidate
the artifact (`nelisp-artifact-stale')."
  (let* ((temp-dir (make-temp-file "nelisp-artifact-pre-" t))
         (preload-path (expand-file-name "prelude.el" temp-dir))
         (source-path (expand-file-name "mod.el" temp-dir))
         (artifact-path (concat source-path ".nelc")))
    (unwind-protect
        (progn
          (write-region
           "(defvar nelisp-artifact-test--pre-marker 1)\n"
           nil preload-path nil 'silent)
          (write-region
           "(defvar nelisp-artifact-test--pre-var 1)\n" nil source-path nil 'silent)
          (nelisp-artifact-compile-file
           source-path artifact-path nil nil nil (list preload-path))
          ;; Mutate the preload: artifact is now stale.
          (write-region
           "(defvar nelisp-artifact-test--pre-marker 22)\n"
           nil preload-path nil 'silent)
          (setq nelisp-artifact--loaded nil)
          (should-error (nelisp-artifact-load-file artifact-path)
                        :type 'nelisp-artifact-stale))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/preload-freshness-uses-metadata-fast-path ()
  "Unchanged preload validation should not re-read the preload body.
Preloads are common for compiled runtime/image caches; checking size,
mtime, and ctime first avoids avoidable file IO, SHA-256 work, and
string allocation."
  (let* ((temp-dir (make-temp-file "nelisp-artifact-pre-fast-" t))
         (preload-path (expand-file-name "prelude.el" temp-dir))
         (source-path (expand-file-name "mod.el" temp-dir))
         (artifact-path (concat source-path ".nelc"))
         (old-read-file (symbol-function 'nelisp-artifact--read-file-as-string))
         (preload-reads 0))
    (unwind-protect
        (progn
          (write-region
           "(defvar nelisp-artifact-test--pre-fast-marker 1)\n"
           nil preload-path nil 'silent)
          (write-region
           "(defun nelisp-artifact-test--pre-fast-f (x) (+ x 1))\n"
           nil source-path nil 'silent)
          (nelisp-artifact-compile-file
           source-path artifact-path nil nil nil (list preload-path))
          (nelisp--reset)
          (setq nelisp-artifact--loaded nil)
          (cl-letf (((symbol-function 'nelisp-artifact--read-file-as-string)
                     (lambda (path)
                       (when (equal (file-truename path)
                                    (file-truename preload-path))
                         (setq preload-reads (1+ preload-reads)))
                       (funcall old-read-file path))))
            (nelisp-artifact-load-file artifact-path))
          (should (= preload-reads 0))
          (should (= (nelisp-eval '(nelisp-artifact-test--pre-fast-f 2)) 3)))
      (fset 'nelisp-artifact--read-file-as-string old-read-file)
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/freshness-fallback-hashes-raw-file-bytes ()
  "Metadata mismatch falls back to the raw file digest, not decoded text."
  (let* ((temp-dir (make-temp-file "nelisp-artifact-raw-fresh-" t))
         (source-path (expand-file-name "unicode.el" temp-dir))
         (artifact-path (concat source-path ".neln"))
         (raw-digest "raw-utf8-digest")
         (record (list :path source-path
                       :sha256 raw-digest
                       ;; Deliberately force the digest fallback.
                       :size -1
                       :mtime '(0 0 0 0)
                       :ctime '(0 0 0 0)))
         (hashed-path nil))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region ";; 日本語のソース\n" nil source-path nil 'silent))
          (cl-letf (((symbol-function 'nelisp-artifact--sha256-file)
                     (lambda (path)
                       (setq hashed-path path)
                       raw-digest))
                    ((symbol-function 'secure-hash)
                     (lambda (&rest _)
                       (error "freshness must not hash decoded source text"))))
            (should-not
             (nelisp-artifact--validate-input-record
              record "source" artifact-path)))
          (should (equal hashed-path source-path)))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/validate-source-fallback-hashes-multibyte-raw-file ()
  "Full validation shares the raw-byte source freshness fallback."
  (let* ((temp-dir (make-temp-file "nelisp-artifact-raw-source-validate-" t))
         (source-path (expand-file-name "unicode.el" temp-dir))
         (artifact-path (concat source-path ".neln"))
         (old-read-file
          (symbol-function 'nelisp-artifact--read-file-as-string)))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region
             ";; 日本語のsource\n(defun raw-source-value () 42)\n"
             nil source-path nil 'silent))
          (nelisp-artifact-compile-file
           source-path artifact-path nil nil nil nil nil 'neln)
          (let* ((content (funcall old-read-file artifact-path))
                 (manifest (copy-tree
                            (nelisp-artifact-read-manifest artifact-path)))
                 (source-record (copy-sequence
                                 (plist-get manifest :source))))
            ;; Simulate host/standalone stat representation disagreement
            ;; without changing the actual UTF-8 source bytes.
            (setq source-record (plist-put source-record :size -1))
            (setq source-record
                  (plist-put source-record :mtime '(0 0 0 0)))
            (setq source-record
                  (plist-put source-record :ctime '(0 0 0 0)))
            (setq manifest (plist-put manifest :source source-record))
            (cl-letf
                (((symbol-function 'nelisp-artifact--read-manifest-for-load)
                  (lambda (_artifact) manifest))
                 ((symbol-function 'nelisp-artifact--read-file-as-string)
                  (lambda (path)
                    (when (equal (expand-file-name path)
                                 (expand-file-name source-path))
                      (error "validation must not hash decoded source text"))
                    (funcall old-read-file path))))
              (should (eq (plist-get
                           (nelisp-artifact--validate artifact-path content)
                           :kind)
                          'neln)))))
      (fset 'nelisp-artifact--read-file-as-string old-read-file)
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/rejects-missing-manifest ()
  "Doc 142 §7: an artifact with no sibling manifest must be rejected —
the artifact+manifest pair is the cache contract."
  (let* ((temp-dir (make-temp-file "nelisp-artifact-nomf-" t))
         (source-path (expand-file-name "mod.el" temp-dir))
         (artifact-path (concat source-path ".nelc"))
         (manifest-path (concat artifact-path ".manifest.el")))
    (unwind-protect
        (progn
          (write-region
           "(defvar nelisp-artifact-test--nomf-var 1)\n" nil source-path nil 'silent)
          (nelisp-artifact-compile-file source-path artifact-path)
          (delete-file manifest-path)
          (setq nelisp-artifact--loaded nil)
          (should-error (nelisp-artifact-load-file artifact-path)
                        :type 'nelisp-artifact-invalid))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/gate-6-1-compiles-defuns-to-bytecode ()
  "Doc 142 §6.1: eligible top-level `defun's compile to NeLisp bytecode
closures (:fn / `nelisp-bcl'); `defvar' / `defmacro' / `put' / `provide'
stay (:eval) replay.  After a sourceless load the bytecode functions
run — including recursion and a forward reference to a later `defvar'."
  (let* ((temp-dir (make-temp-file "nelisp-artifact-bc-" t))
         (source-path (expand-file-name "mod.el" temp-dir))
         (artifact-path (concat source-path ".nelc"))
         (source
          "(defun nat-sq (x) (* x x))
(defun nat-fact (n) (if (< n 2) 1 (* n (nat-fact (- n 1)))))
(defun nat-add (n) (+ n nat-base))
(defvar nat-base 10)
(defmacro nat-twice (x) (list '* 2 x))
(put 'nat-s 'nat-p 'ok)
(provide 'nat-feat)\n"))
    (unwind-protect
        (progn
          (write-region source nil source-path nil 'silent)
          (nelisp-artifact-compile-file source-path artifact-path)
          (let* ((module (plist-get (nelisp-artifact--read-payload artifact-path)
                                    :module-init))
                 (tags (mapcar #'car module)))
            ;; nat-sq / nat-fact / nat-add -> bytecode; the rest -> replay
            (should (equal (list (nth 0 tags) (nth 1 tags) (nth 2 tags))
                           '(:fn :fn :fn)))
            (should (eq (nth 3 tags) :eval))    ; defvar
            (should (eq (nth 4 tags) :eval))    ; defmacro
            ;; the stored function payload is a NeLisp bytecode closure
            (should (eq (car (nth 2 (nth 0 module))) 'nelisp-bcl))
            (should (equal (nth 3 (nth 0 module))
                           '(defun nat-sq (x) (* x x)))))
          (rename-file source-path (concat source-path ".gone") t)
          (nelisp--reset)
          (setq nelisp-artifact--loaded nil)
          (nelisp-artifact-load-file artifact-path)
          (should (= (nelisp-eval '(nat-sq 9)) 81))
          (should (= (nelisp-eval '(nat-fact 5)) 120))   ; recursion via VM
          (should (= (nelisp-eval '(nat-add 7)) 17)))     ; forward ref to defvar
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/try-compile-defun-keeps-source-form-and-fallbacks ()
  "Compiled defuns keep the original source form in the module record.
Non-defuns still replay as `:eval', and `eval-only' keeps forcing replay."
  (let* ((form '(defun nelisp-artifact-test--roundtrip-fn (x y) (+ x y)))
         (compiled (nelisp-artifact--try-compile-defun form)))
    (should (equal (list (nth 0 compiled) (nth 1 compiled))
                   '(:fn nelisp-artifact-test--roundtrip-fn)))
    (should (eq (car (nth 2 compiled)) 'nelisp-bcl))
    (should (equal (nth 3 compiled) form))
    (should-not (nthcdr 4 compiled))
    (should (equal (nelisp-artifact--compile-top-level-form
                    '(put 'nelisp-artifact-test--roundtrip-fn 'ok t))
                   '(:eval (put 'nelisp-artifact-test--roundtrip-fn 'ok t))))
    (should (equal (nelisp-artifact--compile-top-level-form form 'eval-only)
                   `(:eval ,form)))))

(ert-deftest nelisp-artifact/normalize-top-level-function-definition-macros-to-fn ()
  "Supported function-definition macros compile as `:fn'; unsupported ones stay replayed."
  (let* ((temp-dir (make-temp-file "nelisp-artifact-defmacro-" t))
         (source-path (expand-file-name "mod.el" temp-dir))
         (artifact-path (concat source-path ".nelc"))
         (source (nelisp-artifact-test--definition-macro-source))
         (old-define-inline (and (fboundp 'define-inline)
                                 (symbol-function 'define-inline))))
    (unwind-protect
        (progn
          (write-region source nil source-path nil 'silent)
          (nelisp-artifact-compile-file source-path artifact-path)
          (let* ((payload (nelisp-artifact--read-payload artifact-path))
                 (module (plist-get payload :module-init))
                 (defsubst-item
                  (nelisp-artifact-test--find-module-item
                   'nelisp-artifact-test--defsubst-good module))
                 (inline-item
                  (nelisp-artifact-test--find-module-item
                   'nelisp-artifact-test--inline-good module))
                 (wrapped-item
                  (nelisp-artifact-test--find-module-item
                   'nelisp-artifact-test--wrapped module))
                 (inline-wrapped-item
                  (nelisp-artifact-test--find-module-item
                   'nelisp-artifact-test--inline-wrapped module))
                 (bad-inline-item
                  (nelisp-artifact-test--find-module-item
                   'nelisp-artifact-test--bad-inline module))
                 (expected-defsubst
                  '(defun nelisp-artifact-test--defsubst-good (x)
                     "defsubst doc"
                     (declare (indent defun))
                     (+ x 1)))
                 (expected-inline
                  '(defun nelisp-artifact-test--inline-good (x)
                     "inline doc"
                     (declare (indent defun))
                     (+ x 2))))
            (should (eq (car defsubst-item) :fn))
            (should (eq (car inline-item) :fn))
            (should (eq (car wrapped-item) :eval))
            (should (eq (car inline-wrapped-item) :eval))
            (should (eq (car bad-inline-item) :eval))
            (should (equal (nth 3 defsubst-item) expected-defsubst))
            (should (equal (nth 3 inline-item) expected-inline))
            (setq nelisp-artifact--loaded nil)
            (nelisp--reset)
            (rename-file source-path (concat source-path ".gone") t)
            (setq nelisp-artifact--loaded nil)
            (nelisp-artifact-load-file artifact-path)
            (should (= (nelisp-eval '(nelisp-artifact-test--defsubst-good 9)) 10))
            (should (= (nelisp-eval '(nelisp-artifact-test--inline-good 9)) 11))
            (should (= (nelisp-eval '(nelisp-artifact-test--wrapped 9)) 12))
            (should (= (nelisp-eval '(nelisp-artifact-test--inline-wrapped 9)) 13))
            (should (not (nelisp-eval '(fboundp 'nelisp-artifact-test--bad-inline))))
            (should (eq (nelisp-eval '(get 'nelisp-artifact-test--bad-inline
                                           'nelisp-artifact-test--bad-inline-replayed))
                        t))
            )
      (when old-define-inline
        (fset 'define-inline old-define-inline))
      (unless old-define-inline
        (fmakunbound 'define-inline))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t))))))

(ert-deftest nelisp-artifact/neln-normalizes-function-definition-macros-to-native ()
  "A `.neln' build preserves native metadata for supported inline function macros."
  (skip-unless (memq system-type '(gnu/linux berkeley-unix)))
  (skip-unless (and (executable-find "cc") (executable-find "objcopy")))
  (let* ((temp-dir (make-temp-file "nelisp-artifact-defmacro-neln-" t))
         (source-path (expand-file-name "mod.el" temp-dir))
         (artifact-path (concat source-path ".neln"))
         (source (nelisp-artifact-test--definition-macro-source))
         (old-define-inline (and (fboundp 'define-inline)
                                 (symbol-function 'define-inline))))
    (unwind-protect
        (progn
          (write-region source nil source-path nil 'silent)
          (let* ((manifest (nelisp-artifact-compile-file
                            source-path artifact-path
                            nil nil nil nil nil 'neln))
                 (payload (nelisp-artifact--read-payload artifact-path))
                 (module (plist-get payload :module-init))
                 (native (plist-get payload :native))
                 (report (plist-get native :compile-report))
                 (symbols (plist-get native :symbols))
                 (module-names
                  (delq nil
                        (mapcar #'nelisp-artifact-test--module-item-name
                                (cl-remove-if-not
                                 (lambda (item)
                                   (memq (car item) '(:fn :eval)))
                                 module)))))
            (should (eq (plist-get manifest :kind) 'neln))
            (should (equal symbols
                           '("nelisp-artifact-test--defsubst-good"
                             "nelisp-artifact-test--inline-good")))
            (should (equal (mapcar (lambda (entry) (plist-get entry :name))
                                   report)
                           '("nelisp-artifact-test--defsubst-good"
                             "nelisp-artifact-test--inline-good")))
            (should (cl-every (lambda (entry) (plist-get entry :native)) report))
            (should (member 'nelisp-artifact-test--defsubst-good module-names))
            (should (member 'nelisp-artifact-test--inline-good module-names))
            (should (member 'nelisp-artifact-test--wrapped module-names))
            (should (member 'nelisp-artifact-test--inline-wrapped module-names))
            (should (member 'nelisp-artifact-test--bad-inline module-names))
            (rename-file source-path (concat source-path ".gone") t)
            (setq nelisp-artifact--loaded nil)
            (nelisp--reset)
            (nelisp-artifact-load-file artifact-path)
            (should (= (nelisp-eval '(nelisp-artifact-test--defsubst-good 9)) 10))
            (should (= (nelisp-eval '(nelisp-artifact-test--inline-good 9)) 11))
            (should (= (nelisp-eval '(nelisp-artifact-test--wrapped 9)) 12))
            (should (= (nelisp-eval '(nelisp-artifact-test--inline-wrapped 9)) 13))
            (should (not (nelisp-eval '(fboundp 'nelisp-artifact-test--bad-inline))))
            (should (eq (nelisp-eval '(get 'nelisp-artifact-test--bad-inline
                                           'nelisp-artifact-test--bad-inline-replayed))
                        t))
            (should (= (nelisp-artifact-native-exec
                        artifact-path "nelisp-artifact-test--defsubst-good"
                        '(9))
                       10))
            (should (= (nelisp-artifact-native-exec
                        artifact-path "nelisp-artifact-test--inline-good"
                        '(9))
                       11))
            (should (equal (plist-get native :compile-report) report)))
      (when old-define-inline
        (fset 'define-inline old-define-inline))
      (unless old-define-inline
        (fmakunbound 'define-inline))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t))))))

(ert-deftest nelisp-artifact/compile-file-loads-required-macro-before-later-defun ()
  "A required dependency macro is available before compiling a later defun."
  (let* ((temp-dir (make-temp-file "nelisp-artifact-require-" t))
         (dep-path (expand-file-name "nelisp-artifact-test--dep.el" temp-dir))
         (source-path (expand-file-name "main.el" temp-dir))
         (artifact-path (concat source-path ".nelc"))
         (dep-source
          "(defmacro nelisp-artifact-test--dep-splice (&rest body)
   `(list ,@body))
(provide 'nelisp-artifact-test--dep)\n")
         (source
          "(require 'nelisp-artifact-test--dep)
(defun nelisp-artifact-test--use-dep (x y)
  (nelisp-artifact-test--dep-splice x y))
(provide 'nelisp-artifact-test--main)\n"))
    (unwind-protect
        (progn
          (write-region dep-source nil dep-path nil 'silent)
          (write-region source nil source-path nil 'silent)
          (nelisp-artifact-compile-file
           source-path artifact-path nil nil (list temp-dir))
          (let* ((module (plist-get (nelisp-artifact--read-payload artifact-path)
                                    :module-init))
                 (defun-item (cadr module)))
            (should (eq (car (car module)) :eval))
            (should (eq (car defun-item) :fn))
            (should (equal (nth 1 defun-item)
                           'nelisp-artifact-test--use-dep))
            (should (equal (nth 3 defun-item)
                           '(defun nelisp-artifact-test--use-dep (x y)
                              (nelisp-artifact-test--dep-splice x y)))))
          (rename-file source-path (concat source-path ".gone") t)
          (let ((load-path (cons temp-dir load-path))
                (nelisp-load-path (cons temp-dir nelisp-load-path)))
            (nelisp--reset)
            (setq nelisp-artifact--loaded nil)
            (nelisp-artifact-load-file artifact-path)
            (should (equal (nelisp-eval
                            '(nelisp-artifact-test--use-dep 1 2))
                           '(1 2)))
            (should (equal (nelisp-eval
                            '(nelisp-artifact-test--use-dep 'a 'b))
                           '(a b)))))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/compile-file-loads-same-file-macro-before-later-defun ()
  "A same-file `defmacro' is visible to a later defun in source order."
  (let* ((temp-dir (make-temp-file "nelisp-artifact-local-macro-" t))
         (source-path (expand-file-name "local.el" temp-dir))
         (artifact-path (concat source-path ".nelc"))
         (source
          "(defmacro nelisp-artifact-test--local-splice (&rest body)
   `(list ,@body))
(defun nelisp-artifact-test--use-local (x y)
  (nelisp-artifact-test--local-splice x y))
(provide 'nelisp-artifact-test--local)\n"))
    (unwind-protect
        (progn
          (write-region source nil source-path nil 'silent)
          (nelisp-artifact-compile-file source-path artifact-path)
          (let* ((module (plist-get (nelisp-artifact--read-payload artifact-path)
                                    :module-init))
                 (defun-item (cadr module)))
            (should (eq (car (car module)) :eval))
            (should (eq (car defun-item) :fn))
            (should (equal (nth 1 defun-item)
                           'nelisp-artifact-test--use-local))
            (should (equal (nth 3 defun-item)
                           '(defun nelisp-artifact-test--use-local (x y)
                              (nelisp-artifact-test--local-splice x y)))))
          (rename-file source-path (concat source-path ".gone") t)
          (nelisp--reset)
          (setq nelisp-artifact--loaded nil)
          (nelisp-artifact-load-file artifact-path)
          (should (equal (nelisp-eval
                          '(nelisp-artifact-test--use-local 1 2))
                         '(1 2)))
          (should (equal (nelisp-eval
                          '(nelisp-artifact-test--use-local 'a 'b))
                         '(a b))))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/gate-8-artifact-load-faster-than-source ()
  "Doc 142 gate 8: loading a function-heavy module from its compiled
artifact is measurably faster than replaying the source — bytecode is
produced at compile time, so load only installs it.

The full performance ratio is covered by `nelisp-performance-gate'.  This
ERT keeps the deterministic contract: artifact load replays the compiled
module and does not fall back to source loading."
  (let* ((temp-dir (make-temp-file "nelisp-artifact-perf-" t))
         (source-path (expand-file-name "big.el" temp-dir))
         (artifact-path (concat source-path ".nelc")))
    (unwind-protect
        (progn
          (with-temp-file source-path
            (dotimes (i 200)
              (insert (format "(defun nat-p-f%d (x) (let ((y (* x %d))) (if (> y 0) (+ y %d) (- y %d))))\n"
                              i (1+ i) i i)))
            (insert "(provide 'nat-perf)\n"))
          (nelisp-artifact-compile-file source-path artifact-path)
          ;; Source replay sanity.
          (nelisp--reset)
          (let ((nelisp-load-prefer-artifacts nil))
            (nelisp-load-file source-path))
          (should (= (nelisp-eval '(nat-p-f3 5)) 23))
          ;; Artifact replay must not fall back to source loading.  Timing is
          ;; intentionally kept out of ERT because the full suite can introduce
          ;; enough scheduler noise to make ratio assertions flaky.
          (nelisp--reset)
          (setq nelisp-artifact--loaded nil)
          (cl-letf (((symbol-function 'nelisp-load-file)
                     (lambda (&rest _)
                       (error "artifact load must not source-load"))))
            (nelisp-artifact-load-file artifact-path))
          (should (= (nelisp-eval '(nat-p-f3 5)) 23)))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/gate-6-4-neln-bundles-native-and-runs ()
  "Doc 142 §6.4: --kind neln compiles eligible top-level defuns to a REAL
ET_REL native object (embedded, base64) AND keeps the portable bytecode
module, so the artifact loads + runs on host via the bytecode lane while
carrying native code for the standalone runtime.  The manifest declares
artifact-class native + the AOT runtime-abi + wrapper-install metadata;
the integrity-covered payload retains full native code and metadata."
  (let* ((temp-dir (make-temp-file "nelisp-artifact-neln-" t))
         (source-path (expand-file-name "m.el" temp-dir))
         (artifact-path (concat source-path ".neln"))
         (source
          "(defun nat-neln-sq (x) (* x x))
(defun nat-neln-add3 (x) (+ x 3))
(defvar nat-neln-v 7)
(provide 'nat-neln-feat)\n"))
    (unwind-protect
        (progn
          (write-region source nil source-path nil 'silent)
          (let* ((m (nelisp-artifact-compile-file
                     source-path artifact-path nil nil nil nil nil 'neln))
                 (nat (plist-get m :native))
                 (payload-nat (plist-get
                               (nelisp-artifact--read-payload artifact-path)
                               :native)))
            (should (eq (plist-get m :kind) 'neln))
            (should (eq (plist-get m :artifact-class) 'native))
            (should (equal (plist-get m :runtime-abi) "nelisp-neln-aot-v1"))
            (should nat)
            (should (= (plist-get (plist-get m :compiler)
                                  :native-section-version)
                       5))
            (should (member "nat-neln-sq" (plist-get nat :symbols)))
            (should (member "nat-neln-add3" (plist-get nat :symbols)))
            (should (= (plist-get payload-nat :native-section-version) 5))
            (should (> (plist-get payload-nat :object-size) 0))
            (should (> (plist-get payload-nat :text-size) 0))
            (should (stringp (plist-get payload-nat :text-base64)))
            (should (equal (plist-get payload-nat :extern-symbols) nil))
            (should (cl-every (lambda (entry) (plist-get entry :native))
                              (plist-get payload-nat :compile-report)))
            (should-not (plist-get payload-nat :relocs))
            (should (eq (plist-get payload-nat :reloc-format)
                        'indexed-plt32-v1))
            (should (equal (mapcar (lambda (d) (plist-get d :name))
                                   (plist-get nat :defuns))
                           '("nat-neln-sq" "nat-neln-add3")))
            (dolist (entry (plist-get nat :defuns))
              (should (integerp (plist-get entry :offset)))
              (should (> (plist-get entry :size) 0))
              (should (integerp (plist-get entry :arity)))
              (should (integerp (plist-get entry :rt-slot-count)))
              (should (integerp (plist-get entry :body-offset)))))
          ;; the embedded object is a real ELF relocatable
          (let* ((payload (nelisp-artifact--read-payload artifact-path))
                 (obj (base64-decode-string
                       (plist-get (plist-get payload :native) :object-base64))))
            (should (string-prefix-p "\177ELF" obj)))
          ;; loads + runs on host through the portable bytecode lane
          (rename-file source-path (concat source-path ".gone") t)
          (nelisp--reset)
          (setq nelisp-artifact--loaded nil)
          (nelisp-artifact-load-file artifact-path)
          (should (= (nelisp-eval '(nat-neln-sq 9)) 81))
          (should (= (nelisp-eval '(nat-neln-add3 10)) 13))
          (should (nelisp-eval '(featurep 'nat-neln-feat))))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/neln-native-inc-second-offset-matches-single-defun-span ()
  "A two-defun native artifact keeps the second defun offset aligned to the
actual byte span of the first defun, and the native path still returns 10
for an `inc' smoke call."
  (skip-unless (memq system-type '(gnu/linux berkeley-unix)))
  (skip-unless (and (executable-find "cc") (executable-find "objcopy")))
  (let* ((temp-dir (make-temp-file "nelisp-artifact-neln-inc-" t))
         (single-source-path (expand-file-name "single.el" temp-dir))
         (single-artifact-path (concat single-source-path ".neln"))
         (pair-source-path (expand-file-name "pair.el" temp-dir))
         (pair-artifact-path (concat pair-source-path ".neln"))
         (single-source
          "(defun nat-neln-inc (x) (+ x 1))
(provide 'nat-neln-inc)\n")
         (pair-source
          "(defun nat-neln-inc (x) (+ x 1))
(defun nat-neln-id (x) x)
(provide 'nat-neln-id)\n"))
    (unwind-protect
        (progn
          (write-region single-source nil single-source-path nil 'silent)
          (nelisp-artifact-compile-file
           single-source-path single-artifact-path nil nil nil nil nil 'neln)
          (let* ((single-native (plist-get
                                 (nelisp-artifact--read-payload
                                  single-artifact-path)
                                 :native))
                 (single-text (base64-decode-string
                               (plist-get single-native :text-base64)))
                 (first-span (nelisp-artifact--byte-length single-text)))
            (write-region pair-source nil pair-source-path nil 'silent)
            (nelisp-artifact-compile-file
             pair-source-path pair-artifact-path nil nil nil nil nil 'neln)
            (let* ((pair-native (plist-get
                                 (nelisp-artifact--read-payload
                                  pair-artifact-path)
                                 :native))
                   (pair-defuns (plist-get pair-native :defuns))
                   (second-offset (plist-get (cadr pair-defuns) :offset)))
              (should (= (length pair-defuns) 2))
              (should (= second-offset first-span))
              (should (= (nelisp-artifact-native-exec
                          pair-artifact-path "nat-neln-inc" '(9))
                         10)))))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/byte-length-matches-string-bytes-for-unibyte-payload ()
  "The private byte-length helper should match `string-bytes' on raw payloads."
  (let ((payload (unibyte-string 0 127 128 255 65)))
    (should (= (nelisp-artifact--byte-length payload)
               (string-bytes payload)))
    (should (= (nelisp-artifact--byte-length payload)
               (length payload)))))

(ert-deftest nelisp-artifact/native-section-plist-uses-byte-lengths ()
  "Native section sizes are counted in bytes, not character units."
  (let* ((source "(defun nat-byte-length-probe (x) (* x x))\n(provide 'nat-byte-length-probe)\n")
         (_compiler-ready (should (nelisp-artifact--ensure-native-compiler)))
         (forms (nelisp-artifact--read-top-level-forms
                 source "nat-byte-length-probe.el"))
         (unit (nelisp-aot-compile-to-link-unit
                (cons 'seq (nelisp-artifact--native-defun-forms forms))
                :arch 'x86_64 :format 'elf))
         (native
          (cl-letf (((symbol-function 'nelisp-artifact--read-binary)
                     (lambda (&rest _)
                       (error "read-binary should not be used for native embedding"))))
            (nelisp-artifact--native-section-plist
             nil unit 'x86_64 '("nat-byte-length-probe")
             '((:name "nat-byte-length-probe" :native t))))))
    (should (= (plist-get native :object-size)
               (nelisp-artifact--byte-length
                (nelisp-artifact--native-object-bytes unit))))
    (should (= (plist-get native :text-size)
               (nelisp-artifact--byte-length (plist-get unit :text))))))

(ert-deftest nelisp-artifact/neln-auto-suffix-and-cli ()
  "Doc 142 §6.5: --kind auto with a .neln output resolves to the native
lane; the CLI compile/eval surface works end-to-end for .neln."
  (let* ((temp-dir (make-temp-file "nelisp-artifact-neln-cli-" t))
         (source-path (expand-file-name "m.el" temp-dir))
         (artifact-path (concat source-path ".neln")))
    (unwind-protect
        (progn
          (write-region "(defun nat-cli-dbl (x) (* x 2))\n(provide 'nat-cli)\n"
                        nil source-path nil 'silent)
          (should (= 0 (compile-elisp-artifact
                        (list "compile-elisp-artifact" "--kind" "auto"
                              "--input" source-path "--output" artifact-path))))
          (should (file-exists-p artifact-path))
          (should (eq (plist-get (nelisp-artifact-read-manifest artifact-path) :kind)
                      'neln))
          (nelisp--reset)
          (setq nelisp-artifact--loaded nil)
          (should (= 0 (eval-elisp-artifact
                        (list "eval-elisp-artifact" artifact-path
                              "(nat-cli-dbl 21)")))))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/standalone-eval-artifact-defvar-visible-to-native-call ()
  "Standalone `eval-elisp-artifact' must not lose top-level `defvar' for native calls."
  (let* ((bin (expand-file-name "target/nelisp" default-directory))
         (temp-dir (make-temp-file "nelisp-artifact-standalone-defvar-" t))
         (source-path (expand-file-name "m.el" temp-dir))
         (artifact-path (concat source-path ".neln")))
    (skip-unless (file-executable-p bin))
    (unwind-protect
        (progn
          (write-region
           "(defun standalone-artifact-getx () standalone-artifact-x)\n(defvar standalone-artifact-x nil)\n"
           nil source-path nil 'silent)
          (with-temp-buffer
            (should
             (= 0
                (call-process
                 bin nil t nil
                 "compile-elisp-artifact"
                 "--kind" "neln"
                 "--input" source-path
                 "--output" artifact-path))))
          (with-temp-buffer
            (should
             (= 0
                (call-process
                 bin nil t nil
                 "eval-elisp-artifact" artifact-path
                 "(standalone-artifact-getx)")))
            (should (equal (buffer-string) "nil\n"))))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/standalone-eval-artifact-manifest-load-path-before-require ()
  "Standalone direct artifact replay must publish manifest paths to `nelisp-load-path'."
  (let* ((bin (expand-file-name "target/nelisp" default-directory))
         (temp-dir (make-temp-file "nelisp-artifact-standalone-manifest-" t))
         (dependency-dir (expand-file-name "dependency" temp-dir))
         (dependency-path
          (expand-file-name "standalone-manifest-dependency.el" dependency-dir))
         (source-path (expand-file-name "m.el" temp-dir))
         (artifact-path (concat source-path ".neln")))
    (skip-unless (file-executable-p bin))
    (unwind-protect
        (progn
          (make-directory dependency-dir t)
          (write-region
           "(defun standalone-manifest-dependency-value () 41)\n\
(provide 'standalone-manifest-dependency)\n"
           nil dependency-path nil 'silent)
          ;; REQUIRE must be the first top-level form so replay only succeeds
          ;; when manifest paths are visible before module init continues.
          (write-region
           "(require 'standalone-manifest-dependency)\n\
(defvar standalone-manifest-module-value\n\
  (standalone-manifest-dependency-value))\n\
(provide 'standalone-manifest-module)\n"
           nil source-path nil 'silent)
          (with-temp-buffer
            (should
             (= 0
                (call-process
                 bin nil t nil
                 "compile-elisp-artifact"
                 "--kind" "neln"
                 "--input" source-path
                 "--output" artifact-path
                 "--load-path" dependency-dir))))
          (with-temp-buffer
            (should
             (= 0
                (call-process
                 bin nil t nil
                 "eval-elisp-artifact" artifact-path
                 "standalone-manifest-module-value")))
            (should (equal (buffer-string) "41\n"))))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/standalone-eval-artifact-derives-default-directory-from-manifest ()
  "Standalone direct replay must expose the artifact's project root via `default-directory'."
  (let* ((bin (expand-file-name "target/nelisp" default-directory))
         (project-dir (make-temp-file "nelisp-artifact-standalone-root-" t))
         (build-dir (expand-file-name "build" project-dir))
         (src-dir (expand-file-name "src" project-dir))
         (dependency-path
          (expand-file-name "standalone-root-dependency.el" src-dir))
         (source-path (expand-file-name "module.el" build-dir))
         (artifact-path (concat source-path ".neln")))
    (skip-unless (file-executable-p bin))
    (unwind-protect
        (progn
          (make-directory build-dir t)
          (make-directory src-dir t)
          (write-region
           "(defun standalone-root-dependency-value () 41)\n\
(provide 'standalone-root-dependency)\n"
           nil dependency-path nil 'silent)
          ;; Mirrors the cl-lib shim's repo-root probe: without a manifest-
          ;; derived `default-directory', the standalone direct loader tries the
          ;; current checkout's `src/' instead of the artifact project's `src/'.
          (write-region
           "(defconst standalone-root--load-directory\n\
  (let ((source-file\n\
         (or (and (boundp 'load-file-name) load-file-name)\n\
             (and (boundp 'buffer-file-name) buffer-file-name))))\n\
    (cond\n\
     (source-file\n\
      (file-name-directory source-file))\n\
     ((and (boundp 'default-directory)\n\
           (stringp default-directory))\n\
      (let ((src (expand-file-name \"src/\" default-directory)))\n\
        (if (and (fboundp 'file-directory-p)\n\
                 (file-directory-p src))\n\
            src\n\
          default-directory)))\n\
     (t nil))))\n\
(load (expand-file-name \"src/standalone-root-dependency.el\"\n\
                       standalone-root--load-directory)\n\
      nil t)\n\
(defun standalone-root-module-value ()\n\
  (standalone-root-dependency-value))\n\
(provide 'standalone-root-module)\n"
           nil source-path nil 'silent)
          (with-temp-buffer
            (should
             (= 0
                (call-process
                 bin nil t nil
                 "compile-elisp-artifact"
                 "--kind" "neln"
                 "--input" source-path
                 "--output" artifact-path
                 "--load-path" src-dir))))
          (with-temp-buffer
            (should
             (= 0
                (call-process
                 bin nil t nil
                 "eval-elisp-artifact" artifact-path
                 "(standalone-root-module-value)")))
            (should (equal (buffer-string) "41\n"))))
      (when (file-directory-p project-dir)
        (delete-directory project-dir t)))))

(ert-deftest nelisp-artifact/read-top-level-forms-prefers-native-read-all ()
  "`nelisp-artifact--read-top-level-forms' uses native read-all when available."
  (let ((source "(defun native-reader-smoke (x) x)\n(provide 'native-reader-smoke)\n")
        (called nil)
        (nelisp-artifact-profile-forms nil))
    (cl-letf (((symbol-function 'nelisp--read-all-from-string-native)
               (lambda (text)
                 (setq called text)
                 '((native-reader-result)))))
      (should (equal (nelisp-artifact--read-top-level-forms source "native-reader-smoke.el")
                     '((native-reader-result))))
      (should (equal called source)))))

(ert-deftest nelisp-artifact/read-top-level-forms-profile-uses-portable-reader ()
  "`nelisp-artifact-profile-forms' keeps per-form source positions available."
  (let ((source "(defun profile-reader-smoke (x) x)\n(provide 'profile-reader-smoke)\n")
        (nelisp-artifact-profile-forms t))
    (cl-letf (((symbol-function 'nelisp--read-all-from-string-native)
               (lambda (_text)
                 (error "native read-all should be skipped while profiling forms"))))
      (should (equal (mapcar #'car
                             (nelisp-artifact--read-top-level-forms
                              source "profile-reader-smoke.el"))
                     '(defun provide))))))

(ert-deftest nelisp-artifact/source-form-slices-skip-comments ()
  "`nelisp-artifact--source-form-slices' extracts replayable top-level forms."
  (should
   (equal (nelisp-artifact--source-form-slices
           ";; heading\n(defun slice-a (x) (+ x 1))\n\n; mid\n'(slice quoted)\n#'(lambda (x) x)\n")
          '("(defun slice-a (x) (+ x 1))"
            "'(slice quoted)"
            "#'(lambda (x) x)"))))

(ert-deftest nelisp-artifact/artifact-string-can-use-eval-source ()
  "Eval-only artifact serialization can avoid reprinting parsed forms."
  (let* ((source "(defun slice-serializer (x) (+ x 1))\n(provide 'slice-serializer)\n")
         (forms (nelisp-artifact--read-all-from-string source))
         (module (mapcar (lambda (form) (list :eval form)) forms))
         (payload (nelisp-artifact--artifact-payload
                   "slice-serializer.el" module '(slice-serializer)
                   (length forms) 'nelc nil nil 'eval-only))
         (artifact (nelisp-artifact--artifact-string
                    payload source))
         (parsed (nelisp-artifact--parse-payload
                  artifact "slice-serializer.el.nelc"))
         (parsed-module (plist-get parsed :module-init)))
    (should (= (length parsed-module) 1))
    (should (eq (car (car parsed-module)) :eval-source-raw))
    (should (equal (cddr (car parsed-module)) forms))
    (should (equal (plist-get parsed :features) '(slice-serializer)))))

(ert-deftest nelisp-artifact/artifact-string-puts-features-before-native-and-module ()
  "New artifacts expose features before potentially large native/module data."
  (let* ((section
          '(:native-section-version 2 :object-format elf-relocatable-v1
            :arch "x86_64" :symbols ("ordered")
            :object-size 1 :object-sha256 "ignored" :object-base64 "ww=="
            :text-size 1 :text-base64 "ww=="
            :relocs nil :extern-symbols nil :compile-report nil
            :defuns ((:name "ordered" :offset 0 :body-offset 0
                      :arity 1 :rt-slot-count 17))))
         (module
          '((:eval (quote (:native-sections ((:symbols ("nested-decoy"))))))))
         (cases (list section (list section section))))
    (dolist (native cases)
      (let* ((payload
              (nelisp-artifact--artifact-payload
               "ordered.el" module '(ordered) 1 'neln native nil 'bytecode))
             (artifact (nelisp-artifact--artifact-string payload))
             (prefix-len (length nelisp-artifact--magic))
             (list-start (nelisp-read--skip-ws artifact prefix-len))
             (positions
              (nelisp-artifact--private-list-key-positions
               artifact list-start (length artifact)
               '(:native :native-sections :features :module-init)
               "ordered.neln"))
             (native-key
              (if (nelisp-artifact--native-section-p native)
                  :native
                :native-sections))
             (native-pos (cdr (assq native-key positions)))
             (features-pos (cdr (assq :features positions)))
             (module-pos (cdr (assq :module-init positions)))
             (loaded
              (nelisp-artifact--read-serialized-native-sections-for-load
               artifact "ordered.neln")))
        (should native-pos)
        (should features-pos)
        (should module-pos)
        (should (< features-pos native-pos))
        (should (< native-pos module-pos))
        (should (= (length loaded)
                   (if (eq native-key :native-sections) 2 1)))
        (should (equal (plist-get (car loaded) :symbols) '("ordered")))
        (should-not (plist-member (car loaded) :object-base64)))))
  (let* ((payload
          (nelisp-artifact--artifact-payload
           "no-native.el" '((:eval (setq no-native 42)))
           nil 1 'nelc nil nil 'eval-only))
         (artifact (nelisp-artifact--artifact-string payload))
         (parsed
          (nelisp-artifact--parse-payload artifact "no-native.el.nelc")))
    (should-not (string-match-p " :native\\(?:-sections\\)? " artifact))
    (should (equal (plist-get parsed :module-init)
                   (plist-get payload :module-init)))))

(ert-deftest nelisp-artifact/compiled-defun-artifact-round-trips-through-serialization ()
  "A compiled `:fn' module item round-trips through printed artifact text."
  (let* ((form '(defun nelisp-artifact-test--serialized-fn (x) (* x x)))
         (compiled (nelisp-artifact--try-compile-defun form))
         (payload (nelisp-artifact--artifact-payload
                   "nelisp-artifact-test--serialized.el"
                   (list compiled)
                   '(nelisp-artifact-test--serialized-feature)
                   1 'nelc nil nil 'bytecode))
         (artifact (nelisp-artifact--artifact-string payload))
         (parsed (nelisp-artifact--parse-payload
                  artifact "nelisp-artifact-test--serialized.el.nelc"))
         (parsed-module (plist-get parsed :module-init))
         (parsed-item (car parsed-module)))
    (should (equal parsed-item compiled))
    (should (eq (car (nth 2 parsed-item)) 'nelisp-bcl))
    (should (equal (nth 3 parsed-item) form))
    (should (equal parsed-module (list compiled)))
    (should (equal (plist-get parsed :features)
                   '(nelisp-artifact-test--serialized-feature)))))

(ert-deftest nelisp-artifact/raw-eval-source-escapes-non-ascii-strings ()
  "Raw eval-source serialization keeps standalone input ASCII-readable."
  (let* ((source "(defmacro raw-nonascii (&rest body) \"dash — test\" (cons 'progn body))\n")
         (module-string (nelisp-artifact--eval-source-module-string source)))
    (should (string-match-p "\\\\u2014" module-string))
    (should-not (string-match-p "—" module-string))
    (should (equal (cddr (car (car (read-from-string module-string))))
                   (nelisp-artifact--read-all-from-string source)))))

(ert-deftest nelisp-artifact/neln-generic-allows-bytecode-only-module ()
  "A `.neln' artifact is valid even when no defun can enter native code.
This keeps the compile surface uniform for arbitrary `.el' files: native
sections are opportunistic, while bytecode/eval fallback remains required."
  (let* ((temp-dir (make-temp-file "nelisp-artifact-generic-neln-" t))
         (source-path (expand-file-name "data.el" temp-dir))
         (artifact-path (concat source-path ".neln")))
    (unwind-protect
        (progn
          (write-region
           "(defvar generic-neln-v 42)\n(provide 'generic-neln)\n"
           nil source-path nil 'silent)
          (let ((manifest (nelisp-artifact-compile-file
                           source-path artifact-path nil nil nil nil nil 'neln)))
            (should (eq (plist-get manifest :kind) 'neln))
            (should-not (plist-get manifest :native)))
          (rename-file source-path (concat source-path ".gone") t)
          (nelisp--reset)
          (setq nelisp-artifact--loaded nil)
          (nelisp-artifact-load-file artifact-path)
          (should (= (nelisp-eval 'generic-neln-v) 42))
          (should (nelisp-eval '(featurep 'generic-neln))))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/neln-records-native-coverage-report ()
  "A `.neln' manifest records why defuns did or did not become native.
This keeps native compilation generic: every `.el' can produce a `.neln'
artifact, while `inspect-elisp-artifact' can show remaining coverage gaps."
  (let* ((temp-dir (make-temp-file "nelisp-artifact-neln-report-" t))
         (source-path (expand-file-name "report.el" temp-dir))
         (artifact-path (concat source-path ".neln")))
    (unwind-protect
        (progn
          (write-region
           "(defun report-a (x) (+ x 1))
(defun report-b (x) (* x 2))
(provide 'report-native)\n"
           nil source-path nil 'silent)
          (let* ((manifest (nelisp-artifact-compile-file
                            source-path artifact-path nil "wasm32-unknown"
                            nil nil nil 'neln))
                 (payload (nelisp-artifact--read-payload artifact-path))
                 (report (plist-get manifest :native-report)))
            (should (eq (plist-get manifest :kind) 'neln))
            (should-not (plist-get manifest :native))
            (should (= (length report) 2))
            (should (equal report (plist-get payload :native-report)))
            (should (equal (mapcar (lambda (entry) (plist-get entry :name))
                                   report)
                           '("report-a" "report-b")))
            (should
             (cl-every (lambda (entry)
                         (and (not (plist-get entry :native))
                              (string-match-p "unsupported native target"
                                              (plist-get entry :reason))))
                       report))))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/neln-native-policy-required-fails-on-gaps ()
  "`--native-policy required' fails before writing a partial `.neln'.
The default `.neln' lane is deliberately opportunistic so any `.el' can be
cached.  The required policy is the CI/audit mode for proving every top-level
defun in a file entered the native section."
  (let* ((temp-dir (make-temp-file "nelisp-artifact-neln-required-" t))
         (source-path (expand-file-name "required.el" temp-dir))
         (artifact-path (concat source-path ".neln")))
    (unwind-protect
        (progn
          (write-region
           "(defun required-a (x) (+ x 1))
(defun required-b (x) (* x 2))
(provide 'required-native)\n"
           nil source-path nil 'silent)
          (should-error
           (nelisp-artifact-compile-file
            source-path artifact-path nil "wasm32-unknown" nil nil nil
            'neln 'required)
           :type 'error)
          (should-not (file-exists-p artifact-path))
          (should-not (file-exists-p
                       (nelisp-artifact--sibling-manifest-path artifact-path))))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/neln-native-policy-required-skips-probes ()
  "`--native-policy required' compiles the native section in one batch.
Required mode can fail the whole file, so it should avoid the duplicate probe
compile."
  (let* ((forms '((defun required-fast-a (x) (+ x 1))
                  (defun required-fast-b (x) (* x 2))))
         (link-count 0)
         (write-count 0)
         (native nil))
    (cl-letf (((symbol-function 'nelisp-artifact--ensure-native-compiler)
               (lambda () t))
              ((symbol-function 'nelisp-aot-compile-to-object)
               (lambda (&rest _)
                 (error "required native policy should not probe defuns")))
              ((symbol-function 'nelisp-aot-compile-to-link-unit)
               (lambda (_sexp &rest _args)
                 (setq link-count (1+ link-count))
                 (list :text "TEXT"
                       :rodata ""
                       :symbols nil
                       :relocs nil
                       :machine 'x86_64
                       :defuns '((:name "required-fast-a"
                                  :offset 0 :size 4 :arity 1
                                  :param-class gp :rt-slot-count 0
                                  :body-offset 0)
                                 (:name "required-fast-b"
                                  :offset 4 :size 4 :arity 1
                                  :param-class gp :rt-slot-count 0
                                  :body-offset 4))
                       :extern-symbols nil)))
              ((symbol-function 'nelisp-artifact--write-elf-rel-object)
               (lambda (path _unit)
                 (setq write-count (1+ write-count))
                 (write-region "OBJ" nil path nil 'silent))))
      (setq native
            (nelisp-artifact--native-compile-section
             forms nil 'required))
      (should (= link-count 1))
      (should (= write-count 1))
      (should (equal (plist-get native :symbols)
                     '("required-fast-a" "required-fast-b")))
      (should (equal nelisp-artifact--last-native-compile-report
                     '((:name "required-fast-a" :native t)
                       (:name "required-fast-b" :native t)))))))

(ert-deftest nelisp-artifact/neln-opportunistic-fast-batch-skips-probes ()
  "Opportunistic `.neln' compile uses one batch when every defun is native."
  (let* ((forms '((defun opp-fast-a (x) (+ x 1))
                  (defun opp-fast-b (x) (* x 2))))
         (link-count 0)
         (write-count 0)
         (native nil))
    (cl-letf (((symbol-function 'nelisp-artifact--ensure-native-compiler)
               (lambda () t))
              ((symbol-function 'nelisp-aot-compile-to-object)
               (lambda (&rest _)
                 (error "all-native opportunistic path should not probe")))
              ((symbol-function 'nelisp-aot-compile-to-link-unit)
               (lambda (_sexp &rest _args)
                 (setq link-count (1+ link-count))
                 (list :text "TEXT"
                       :rodata ""
                       :symbols nil
                       :relocs nil
                       :machine 'x86_64
                       :defuns '((:name "opp-fast-a"
                                  :offset 0 :size 4 :arity 1
                                  :param-class gp :rt-slot-count 0
                                  :body-offset 0)
                                 (:name "opp-fast-b"
                                  :offset 4 :size 4 :arity 1
                                  :param-class gp :rt-slot-count 0
                                  :body-offset 4))
                       :extern-symbols nil)))
              ((symbol-function 'nelisp-artifact--write-elf-rel-object)
               (lambda (path _unit)
                 (setq write-count (1+ write-count))
                 (write-region "OBJ" nil path nil 'silent))))
      (setq native
            (nelisp-artifact--native-compile-section
             forms nil 'opportunistic))
      (should (= link-count 1))
      (should (= write-count 1))
      (should (equal (plist-get native :symbols)
                     '("opp-fast-a" "opp-fast-b")))
      (should (equal nelisp-artifact--last-native-compile-report
                     '((:name "opp-fast-a" :native t)
                       (:name "opp-fast-b" :native t)))))))

(ert-deftest nelisp-artifact/neln-opportunistic-batch-falls-back-to-probes ()
  "Opportunistic `.neln' compile preserves coverage when batch compile fails."
  (let* ((forms '((defun opp-mixed-a (x) (+ x 1))
                  (defun opp-mixed-b (x) (unsupported-native x))))
         (link-count 0)
         (probe-count 0)
         (native nil))
    (cl-letf (((symbol-function 'nelisp-artifact--ensure-native-compiler)
               (lambda () t))
              ((symbol-function 'nelisp-aot-compile-to-object)
               (lambda (form path &rest _args)
                 (setq probe-count (1+ probe-count))
                 (error "singleton fallback should not probe")))
              ((symbol-function 'nelisp-aot-compile-to-link-unit)
               (lambda (sexp &rest _args)
                 (setq link-count (1+ link-count))
                 (cond
                  ((= link-count 1)
                   (error "batch failed"))
                  ((memq link-count '(2 4))
                   (should (equal sexp '(seq (defun opp-mixed-a (x) (+ x 1)))))
                   (list :text "TEXT"
                         :rodata ""
                         :symbols nil
                         :relocs nil
                         :machine 'x86_64
                         :defuns '((:name "opp-mixed-a"
                                    :offset 0 :size 4 :arity 1
                                    :param-class gp :rt-slot-count 0
                                    :body-offset 0))
                         :extern-symbols nil))
                  ((= link-count 3)
                   (error "unsupported"))
                  (t
                   (error "unexpected link-unit call")))))
              ((symbol-function 'nelisp-artifact--write-elf-rel-object)
               (lambda (path _unit)
                 (write-region "OBJ" nil path nil 'silent))))
      (setq native
            (nelisp-artifact--native-compile-section
             forms nil 'opportunistic))
      ;; Pass 2 recompiles the surviving candidate against the reduced
      ;; allowlist before declaring the graph stable.
      (should (= link-count 4))
      (should (= probe-count 0))
      (should (equal (plist-get native :symbols) '("opp-mixed-a")))
      (should (equal nelisp-artifact--last-native-compile-report
                     '((:name "opp-mixed-a" :native t)
                       (:name "opp-mixed-b" :native nil
                        :reason "unsupported")))))))

(ert-deftest nelisp-artifact/neln-opportunistic-budget-keeps-small-batches-intact ()
  "A budget above the defun count leaves opportunistic native coverage unchanged."
  (let* ((forms '((defun opp-budget-small-a (x) (+ x 1))
                  (defun opp-budget-small-b (x) (* x 2))))
         (link-count 0)
         (write-count 0)
         (native nil))
    (cl-letf (((symbol-function 'nelisp-artifact--ensure-native-compiler)
               (lambda () t))
              ((symbol-function 'nelisp-aot-compile-to-object)
               (lambda (&rest _)
                 (error "budgeted opportunistic path should not probe")))
              ((symbol-function 'nelisp-aot-compile-to-link-unit)
               (lambda (_sexp &rest _args)
                 (setq link-count (1+ link-count))
                 (list :text "TEXT"
                       :rodata ""
                       :symbols nil
                       :relocs nil
                       :machine 'x86_64
                       :defuns '((:name "opp-budget-small-a"
                                  :offset 0 :size 4 :arity 1
                                  :param-class gp :rt-slot-count 0
                                  :body-offset 0)
                                 (:name "opp-budget-small-b"
                                  :offset 4 :size 4 :arity 1
                                  :param-class gp :rt-slot-count 0
                                  :body-offset 4))
                       :extern-symbols nil)))
              ((symbol-function 'nelisp-artifact--write-elf-rel-object)
               (lambda (path _unit)
                 (setq write-count (1+ write-count))
                 (write-region "OBJ" nil path nil 'silent))))
      (let ((nelisp-artifact-default-native-defun-budget 4))
        (setq native
              (nelisp-artifact--native-compile-section
               forms nil 'opportunistic)))
      (should (= link-count 1))
      (should (= write-count 1))
      (should (equal (plist-get native :symbols)
                     '("opp-budget-small-a" "opp-budget-small-b")))
      (should (equal nelisp-artifact--last-native-compile-report
                     '((:name "opp-budget-small-a" :native t)
                       (:name "opp-budget-small-b" :native t)))))))

(ert-deftest nelisp-artifact/neln-opportunistic-default-budget-keeps-multiple-native-defuns ()
  "The default opportunistic budget stays at 32 and keeps small batches whole."
  (let* ((forms '((defun opp-default-budget-a (x) (+ x 1))
                  (defun opp-default-budget-b (x) (* x 2))))
         (temp-dir (make-temp-file "nelisp-artifact-opp-default-budget-" t))
         (source-path (expand-file-name "opp-default-budget.el" temp-dir))
         (source
          "(defun opp-default-budget-a (x) (+ x 1))
(defun opp-default-budget-b (x) (* x 2))
")
         (link-count 0)
         (write-count 0)
         (native nil)
         (manifest nil))
    (should (= nelisp-artifact-default-native-defun-budget 32))
    (unwind-protect
        (progn
          (write-region source nil source-path nil 'silent)
          (cl-letf (((symbol-function 'nelisp-artifact--ensure-native-compiler)
                     (lambda () t))
                    ((symbol-function 'nelisp-aot-compile-to-object)
                     (lambda (&rest _)
                       (error "default opportunistic budget should not probe")))
                    ((symbol-function 'nelisp-aot-compile-to-link-unit)
                     (lambda (_sexp &rest _args)
                       (setq link-count (1+ link-count))
                       (list :text "TEXT"
                             :rodata ""
                             :symbols nil
                             :relocs nil
                             :machine 'x86_64
                             :defuns '((:name "opp-default-budget-a"
                                        :offset 0 :size 4 :arity 1
                                        :param-class gp :rt-slot-count 0
                                        :body-offset 0)
                                       (:name "opp-default-budget-b"
                                        :offset 4 :size 4 :arity 1
                                        :param-class gp :rt-slot-count 0
                                        :body-offset 4))
                             :extern-symbols nil)))
                    ((symbol-function 'nelisp-artifact--write-elf-rel-object)
                     (lambda (path _unit)
                       (setq write-count (1+ write-count))
                       (write-region "OBJ" nil path nil 'silent))))
            (setq native
                  (nelisp-artifact--native-compile-section
                   forms nil 'opportunistic))
            (should (= link-count 1))
            (should (= write-count 1))
            (setq manifest
                  (nelisp-artifact--manifest-plist
                   source-path '(feature-a) 2 nil "abc123" 42
                   nil nil 'neln native nil 'required 'bytecode))
            (should (plist-get manifest :native))
            (should-not (plist-member manifest :native-sections))
            (should (equal (plist-get (plist-get manifest :native) :symbols)
                           '("opp-default-budget-a" "opp-default-budget-b")))
            (should-not (plist-member (plist-get manifest :native) :relocs))
            (should-not
             (plist-member (plist-get manifest :native) :compile-report))
            (should (equal (mapcar (lambda (entry) (plist-get entry :name))
                                   nelisp-artifact--last-native-compile-report)
                           '("opp-default-budget-a" "opp-default-budget-b")))
            (should (equal (mapcar (lambda (entry) (plist-get entry :native))
                                   nelisp-artifact--last-native-compile-report)
                           '(t t)))))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/neln-old-shard-defaults-remain-overridable ()
  "Callers can retain the former 4-defun/64-KiB conservative limits."
  (let ((nelisp-artifact-default-native-defun-budget 4)
        (nelisp-artifact-default-native-section-byte-budget 65536))
    (should (equal (mapcar #'length
                           (nelisp-artifact--chunk-list
                            '(a b c d e) nelisp-artifact-default-native-defun-budget))
                   '(4 1)))
    (should
     (<= (nelisp-artifact--native-section-serialized-byte-size
          (nelisp-artifact-test--v5-flat-section))
         nelisp-artifact-default-native-section-byte-budget))))

(ert-deftest nelisp-artifact/neln-opportunistic-default-budget-shards-over-budget ()
  "The default opportunistic budget shards batches once they exceed 32 defuns."
  (let ((budget nelisp-artifact-default-native-defun-budget)
        (link-count 0)
        (write-count 0)
        (probe-count 0)
        (batch-counts nil))
    (let ((make-batch
           (lambda (prefix count)
             (cl-loop for i from 1 to count collect
                      `(defun ,(intern (format "%s-%03d" prefix i))
                           (x)
                         (+ x ,i)))))
          (make-unit
           (lambda (sexp)
             (let ((forms (cdr sexp)))
               (list :text "TEXT"
                     :rodata ""
                     :symbols (cl-loop for form in forms
                                       for idx from 0
                                       for name = (symbol-name (nth 1 form))
                                       collect (list :name name
                                                   :value (* idx 4)
                                                   :size 4
                                                   :section 'text
                                                   :bind 'global
                                                   :type 'func))
                     :relocs nil
                     :machine 'x86_64
                     :defuns (cl-loop for form in forms
                                      for idx from 0
                                      collect (list :name (symbol-name (nth 1 form))
                                                    :offset (* idx 4)
                                                    :size 4
                                                    :arity 1
                                                    :param-class 'gp
                                                    :rt-slot-count 0
                                                    :body-offset (* idx 4)))
                     :extern-symbols nil)))))
      (cl-labels ((normalize-sections (native)
                    (nelisp-artifact--native-sections-from-native native))
                  (section-symbols (sections)
                    (mapcan (lambda (section)
                              (plist-get section :symbols))
                            sections))
                  (report-names (report)
                    (mapcar (lambda (entry) (plist-get entry :name))
                            report))
                  (report-native-flags (report)
                    (mapcar (lambda (entry) (plist-get entry :native))
                            report)))
      (cl-letf (((symbol-function 'nelisp-artifact--ensure-native-compiler)
                 (lambda () t))
                ((symbol-function 'nelisp-aot-compile-to-object)
                 (lambda (&rest _)
                   (setq probe-count (1+ probe-count))
                   (error "default opportunistic budget should not probe")))
                ((symbol-function 'nelisp-aot-compile-to-link-unit)
                 (lambda (sexp &rest _args)
                   (setq link-count (1+ link-count))
                   (push (length (cdr sexp)) batch-counts)
                   (funcall make-unit sexp)))
                ((symbol-function 'nelisp-artifact--write-elf-rel-object)
                 (lambda (path _unit)
                   (setq write-count (1+ write-count))
                   (write-region "OBJ" nil path nil 'silent))))
        (let* ((batch-4
                (funcall make-batch "opp-default-budget-at" budget))
               (batch-5
                (funcall make-batch "opp-default-budget-over" (1+ budget)))
               (expected-batch-counts
                (append (mapcar #'length (nelisp-artifact--chunk-list
                                          batch-4 budget))
                        (mapcar #'length (nelisp-artifact--chunk-list
                                          batch-5 budget)))))
          (dolist (batch (list batch-4 batch-5))
            (let* ((native (nelisp-artifact--native-compile-section
                            batch nil 'opportunistic))
                   (sections (normalize-sections native))
                   (expected-names
                    (mapcar (lambda (form) (symbol-name (nth 1 form))) batch)))
              (should (= (length sections)
                         (ceiling (length batch) budget)))
              (should (equal (section-symbols sections) expected-names))
              (should (equal (report-names
                              nelisp-artifact--last-native-compile-report)
                             expected-names))
              (should (equal (report-native-flags
                              nelisp-artifact--last-native-compile-report)
                             (make-list (length expected-names) t)))))
          (should (= nelisp-artifact-default-native-defun-budget budget))
          (should (= link-count (+ (ceiling (length batch-4) budget)
                                   (ceiling (length batch-5) budget))))
          (should (= write-count (+ (ceiling (length batch-4) budget)
                                    (ceiling (length batch-5) budget))))
          (should (= probe-count 0))
          (should (equal (nreverse batch-counts) expected-batch-counts))))))))

(ert-deftest nelisp-artifact/neln-opportunistic-budget-caps-fast-and-final-compiles ()
  "Opportunistic native compilation stays under budget and reports skipped defuns."
  (let* ((forms '((defun opp-budget-cap-a (x) (+ x 1))
                  (defun opp-budget-cap-b (x) (* x 2))
                  (defun opp-budget-cap-c (x) (- x 3))
                  (defun opp-budget-cap-d (x) (/ x 4))))
         (link-count 0)
         (probe-count 0)
         (link-defun-counts nil)
         (native nil))
    (cl-letf (((symbol-function 'nelisp-artifact--ensure-native-compiler)
               (lambda () t))
              ((symbol-function 'nelisp-aot-compile-to-object)
               (lambda (form path &rest _args)
                 (setq probe-count (1+ probe-count))
                 (error "budgeted singleton fallback should not probe")))
              ((symbol-function 'nelisp-aot-compile-to-link-unit)
               (lambda (sexp &rest _args)
                 (setq link-count (1+ link-count))
                 (push (length (cdr sexp)) link-defun-counts)
                 (cond
                  ((= link-count 1)
                   (error "batch failed"))
                  ((= link-count 2)
                   (list :text "TEXT"
                         :rodata ""
                         :symbols nil
                         :relocs nil
                         :machine 'x86_64
                         :defuns '((:name "opp-budget-cap-a"
                                    :offset 0 :size 4 :arity 1
                                    :param-class gp :rt-slot-count 0
                                    :body-offset 0))
                         :extern-symbols nil))
                  ((= link-count 3)
                   (list :text "TEXT"
                         :rodata ""
                         :symbols nil
                         :relocs nil
                         :machine 'x86_64
                         :defuns '((:name "opp-budget-cap-b"
                                    :offset 0 :size 4 :arity 1
                                    :param-class gp :rt-slot-count 0
                                    :body-offset 0))
                         :extern-symbols nil))
                  ((= link-count 4)
                   (error "batch failed"))
                  ((= link-count 5)
                   (error "budget exhausted for opp-budget-cap-c"))
                  ((= link-count 6)
                   (error "budget exhausted for opp-budget-cap-d"))
                  ((= link-count 7)
                   (list :text "TEXT"
                         :rodata ""
                         :symbols nil
                         :relocs nil
                         :machine 'x86_64
                         :defuns '((:name "opp-budget-cap-a"
                                    :offset 0 :size 4 :arity 1
                                    :param-class gp :rt-slot-count 0
                                    :body-offset 0)
                                   (:name "opp-budget-cap-b"
                                    :offset 4 :size 4 :arity 1
                                    :param-class gp :rt-slot-count 0
                                    :body-offset 0))
                         :extern-symbols nil))
                  (t
                   (error "unexpected link-unit call")))))
              ((symbol-function 'nelisp-artifact--write-elf-rel-object)
               (lambda (path _unit)
                 (write-region "OBJ" nil path nil 'silent))))
      (let ((nelisp-artifact-default-native-defun-budget 2))
        (setq native
              (nelisp-artifact--native-compile-section
               forms nil 'opportunistic)))
      (cl-labels ((normalize-sections (native)
                    (let ((sections (nelisp-artifact--native-sections-from-native
                                     native)))
                      (if (and sections (not (keywordp (car sections))))
                          sections
                        (list sections))))
                  (section-symbols (sections)
                    (mapcan (lambda (section)
                              (plist-get section :symbols))
                            sections))
                  (report-names (report)
                    (mapcar (lambda (entry) (plist-get entry :name))
                            report))
                  (report-native-flags (report)
                    (mapcar (lambda (entry) (plist-get entry :native))
                            report)))
        (let ((sections (normalize-sections native)))
          (should (equal (section-symbols sections)
                         '("opp-budget-cap-a" "opp-budget-cap-b"))))
        (should (equal (nreverse link-defun-counts)
                       '(2 1 1 2 1 1 2)))
        (should (= probe-count 0))
        (should (equal (report-names nelisp-artifact--last-native-compile-report)
                       '("opp-budget-cap-a" "opp-budget-cap-b"
                         "opp-budget-cap-c" "opp-budget-cap-d")))
        (should (equal (report-native-flags
                        nelisp-artifact--last-native-compile-report)
                       '(t t nil nil)))
        (should (cl-every
                 (lambda (entry)
                   (or (plist-get entry :native)
                       (string-match-p "budget"
                                       (plist-get entry :reason))))
                 (cddr nelisp-artifact--last-native-compile-report)))))))

(ert-deftest nelisp-artifact/native-wrapper-tries-fast-integer-with-rt-slots ()
  "Native wrappers try the direct integer path before general trampoline.
Real AOT metadata can have non-zero `:rt-slot-count' while the exported symbol
is still callable through the integer ABI; this must stay on the fast path."
  (let ((fast-count 0)
        (general-count 0)
        (report nil)
        (fn (list 'nelisp-native-function
                  "/tmp/fake.neln"
                  'native-wrapper-rt-slot
                  (lambda (&rest _) :fallback)
                  '(:name "native-wrapper-rt-slot"
                    :arity 1
                    :param-class gp
                    :rt-slot-count 17))))
    (cl-letf (((symbol-function 'nelisp-artifact-native-exec-fast-simple)
               (lambda (_artifact _symbol _args)
                 (setq fast-count (1+ fast-count))
                 42))
              ((symbol-function 'nelisp-artifact-native-exec-general)
               (lambda (&rest _)
                 (setq general-count (1+ general-count))
                 (error "general path should not run"))))
      (let ((nelisp-artifact-native-dispatch-report nil))
        (should (= (nelisp-native-function-call fn '(41)) 42))
        (setq report (nelisp-artifact-native-dispatch-report))))
    (should (= fast-count 1))
    (should (= general-count 0))
    (should
     (cl-some (lambda (entry)
                (and (eq (plist-get entry :event) 'call)
                     (eq (plist-get entry :symbol) 'native-wrapper-rt-slot)
                     (eq (plist-get entry :mode) 'native)))
              report))))

(ert-deftest nelisp-artifact/neln-required-policy-is-not-budget-capped ()
  "Required native policy still compiles every native defun in one batch."
  (let* ((forms '((defun required-budget-a (x) (+ x 1))
                  (defun required-budget-b (x) (* x 2))
                  (defun required-budget-c (x) (- x 3))))
         (link-count 0)
         (write-count 0)
         (native nil))
    (cl-letf (((symbol-function 'nelisp-artifact--ensure-native-compiler)
               (lambda () t))
              ((symbol-function 'nelisp-aot-compile-to-object)
               (lambda (&rest _)
                 (error "required native policy should not probe")))
              ((symbol-function 'nelisp-aot-compile-to-link-unit)
               (lambda (sexp &rest _args)
                 (setq link-count (1+ link-count))
                 (should (equal (length (cdr sexp)) 3))
                 (list :text "TEXT"
                       :rodata ""
                       :symbols nil
                       :relocs nil
                       :machine 'x86_64
                       :defuns '((:name "required-budget-a"
                                  :offset 0 :size 4 :arity 1
                                  :param-class gp :rt-slot-count 0
                                  :body-offset 0)
                                 (:name "required-budget-b"
                                  :offset 4 :size 4 :arity 1
                                  :param-class gp :rt-slot-count 0
                                  :body-offset 4)
                                 (:name "required-budget-c"
                                  :offset 8 :size 4 :arity 1
                                  :param-class gp :rt-slot-count 0
                                  :body-offset 8))
                       :extern-symbols nil)))
              ((symbol-function 'nelisp-artifact--write-elf-rel-object)
               (lambda (path _unit)
                 (setq write-count (1+ write-count))
                 (write-region "OBJ" nil path nil 'silent))))
      (let ((nelisp-artifact-default-native-defun-budget 1))
        (setq native
              (nelisp-artifact--native-compile-section
               forms nil 'required)))
      (should (= link-count 1))
      (should (= write-count 1))
      (should (equal (plist-get native :symbols)
                     '("required-budget-a" "required-budget-b"
                       "required-budget-c")))
      (should (equal nelisp-artifact--last-native-compile-report
                     '((:name "required-budget-a" :native t)
                       (:name "required-budget-b" :native t)
                       (:name "required-budget-c" :native t)))))))

(ert-deftest nelisp-artifact/native-exec-cli-skips-fast-simple-for-extern-artifact ()
  "CLI native exec routes extern-bearing artifacts to the general trampoline.
The whole linked object can contain unresolved externs even when the requested
integer-ABI symbol is itself simple.  In that case the simple stdout fast path
must not run first, because the general trampoline provides the extern shims.
The manifest fixture is deliberately compressed; externs come from payload."
  (let ((fast-count 0)
        (general-call nil)
        (stdout nil))
    (cl-letf (((symbol-function 'nelisp-artifact-read-manifest)
               (lambda (_path)
                 '(:kind neln
                   :native
                   (:symbols ("hot-fn")
                    :defuns
                    ((:name "hot-fn"
                      :arity 2
                      :param-class gp
                      :rt-slot-count 17
                      :body-offset 13))))))
              ((symbol-function
                'nelisp-artifact--serialized-native-section-for-symbol)
               (lambda (_path _symbol &optional _content)
                 '(:arch "x86_64"
                   :symbols ("hot-fn")
                   :extern-symbols ("nl_alloc_str")
                   :defuns
                   ((:name "hot-fn"
                     :arity 2
                     :param-class gp
                     :rt-slot-count 17
                     :body-offset 13)))))
              ((symbol-function 'nelisp-artifact-native-exec-fast-simple-stdout)
               (lambda (&rest _)
                 (setq fast-count (1+ fast-count))
                 (error "fast path should not run")))
              ((symbol-function 'nelisp-artifact-native-exec-general)
               (lambda (path symbol args)
                 (setq general-call (list path symbol args))
                 7))
              ((symbol-function 'nelisp-artifact-native-exec)
               (lambda (&rest _)
                 (error "simple fallback should not run")))
              ((symbol-function 'nelisp-artifact--write-stdout)
               (lambda (text)
                 (setq stdout (concat (or stdout "") text)))))
      (should (= 0
                 (native-exec-elisp-artifact
                  '("native-exec-elisp-artifact" "m.neln" "hot-fn" "3" "1"))))
      (should (= fast-count 0))
      (should (equal general-call '("m.neln" "hot-fn" (3 1))))
      (should (equal stdout "7\n")))))

(ert-deftest nelisp-artifact/native-exec-cli-simple-path-streams-stdout ()
  "CLI native exec should stream simple integer output without Lisp readback."
  (let ((write-call nil)
        (stdout nil))
    (cl-letf (((symbol-function 'nelisp-artifact-read-manifest)
               (lambda (_path)
                 '(:kind neln
                   :native
                   (:symbols ("hot-fn")
                    :defuns
                    ((:name "hot-fn"
                      :arity 2
                      :param-class gp
                      :rt-slot-count 0
                      :body-offset 13))))))
              ((symbol-function
                'nelisp-artifact--serialized-native-section-for-symbol)
               (lambda (_path _symbol &optional _content)
                 '(:arch "x86_64"
                   :symbols ("hot-fn")
                   :extern-symbols nil
                   :defuns
                   ((:name "hot-fn"
                     :arity 2
                     :param-class gp
                     :rt-slot-count 0
                     :body-offset 13)))))
              ((symbol-function
                'nelisp-artifact-native-exec-fast-simple-write-stdout)
               (lambda (path symbol args)
                 (setq write-call (list path symbol args))
                 0))
              ((symbol-function 'nelisp-artifact-native-exec-fast-simple-stdout)
               (lambda (&rest _)
                 (error "stdout readback path should not run")))
              ((symbol-function 'nelisp-artifact-native-exec)
               (lambda (&rest _)
                 (error "simple fallback should not run")))
              ((symbol-function 'nelisp-artifact-native-exec-general)
               (lambda (&rest _)
                 (error "general path should not run")))
              ((symbol-function 'nelisp-artifact--write-stdout)
               (lambda (text)
                 (setq stdout (concat (or stdout "") text)))))
      (should (= 0
                 (native-exec-elisp-artifact
                  '("native-exec-elisp-artifact" "m.neln" "hot-fn" "3" "1"))))
      (should (equal write-call '("m.neln" "hot-fn" (3 1))))
      (should (null stdout)))))

(ert-deftest nelisp-artifact/private-load-size-fast-path-skips-sha256 ()
  "Private `.neln' load can skip sha256 when manifest artifact size matches."
  (let* ((temp-dir (make-temp-file "nelisp-artifact-size-fast-" t))
         (source-path (expand-file-name "mod.el" temp-dir))
         (artifact-path (concat source-path ".neln"))
         (old-secure-hash (symbol-function 'secure-hash))
         (old-fast nelisp-artifact-fast-integrity-validation))
    (unwind-protect
        (progn
          (write-region
           "(defun size-fast-f (x) (+ x 1))\n(provide 'size-fast)\n"
           nil source-path nil 'silent)
          (nelisp-artifact-compile-file source-path artifact-path
                                        nil nil nil nil nil 'neln)
          (nelisp--reset)
          (setq nelisp-artifact--loaded nil)
          (cl-letf (((symbol-function 'secure-hash)
                     (lambda (&rest _)
                       (error "size fast path should not hash artifact"))))
            (setq nelisp-artifact-fast-integrity-validation t)
            (nelisp-artifact-load-file artifact-path))
          (should (= (nelisp-eval '(size-fast-f 41)) 42)))
      (setq nelisp-artifact-fast-integrity-validation old-fast)
      (fset 'secure-hash old-secure-hash)
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/nonascii-manifest-records-utf8-byte-size ()
  "A non-ASCII `.neln' manifest records the exact UTF-8 artifact bytes."
  (let* ((temp-dir (make-temp-file "nelisp-artifact-utf8-size-" t))
         (source-path (expand-file-name "mod.el" temp-dir))
         (artifact-path (concat source-path ".neln")))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region
             "(defvar utf8-size-value \"日本語\")\n(provide 'utf8-size)\n"
             nil source-path nil 'silent))
          (nelisp-artifact-compile-file source-path artifact-path
                                        nil nil nil nil nil 'neln)
          (let* ((manifest (nelisp-artifact-read-manifest artifact-path))
                 (content (nelisp-artifact--read-file-as-string artifact-path))
                 (file-size (nelisp-artifact--file-size artifact-path)))
            (should (< (length content) file-size))
            (should (= (nelisp-artifact--byte-length content) file-size))
            (should (= (plist-get manifest :artifact-size) file-size))
            (should (equal (plist-get manifest :artifact-sha256)
                           (nelisp-artifact--sha256-file artifact-path)))
            (should (eq
                     (plist-get
                      (nelisp-artifact--validate-flat-image-artifact
                       artifact-path)
                      :kind)
                     'neln))))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/private-load-fast-reader-skips-full-plist-read ()
  "Private `.neln' load uses generated-key readers instead of full plist reads."
  (let* ((temp-dir (make-temp-file "nelisp-artifact-fast-reader-" t))
         (source-path (expand-file-name "mod.el" temp-dir))
         (artifact-path (concat source-path ".neln"))
         (old-reader (symbol-function 'nelisp-artifact--read-one-private-form))
         (old-fast nelisp-artifact-fast-private-read))
    (unwind-protect
        (progn
          (write-region
           "(defun fast-reader-f (x) (+ x 1))\n(provide 'fast-reader)\n"
           nil source-path nil 'silent)
          (nelisp-artifact-compile-file source-path artifact-path
                                        nil nil nil nil nil 'neln)
          (nelisp--reset)
          (setq nelisp-artifact--loaded nil)
          (cl-letf (((symbol-function 'nelisp-artifact--read-one-private-form)
                     (lambda (&rest _)
                       (error "full private plist reader should not run"))))
            (setq nelisp-artifact-fast-private-read t)
            (nelisp-artifact-load-file artifact-path))
          (should (= (nelisp-eval '(fast-reader-f 41)) 42)))
      (setq nelisp-artifact-fast-private-read old-fast)
      (fset 'nelisp-artifact--read-one-private-form old-reader)
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/neln-native-policy-required-cli ()
  "The single-file CLI exposes required native coverage checks."
  (let* ((temp-dir (make-temp-file "nelisp-artifact-neln-required-cli-" t))
         (source-path (expand-file-name "required.el" temp-dir))
         (artifact-path (concat source-path ".neln")))
    (unwind-protect
        (progn
          (write-region
           "(defun required-cli-a (x) (+ x 1))\n(provide 'required-cli)\n"
           nil source-path nil 'silent)
          (should (= 1
                     (compile-elisp-artifact
                      (list "compile-elisp-artifact"
                            "--kind" "neln"
                            "--target" "wasm32-unknown"
                            "--native-policy" "required"
                            "--input" source-path
                            "--output" artifact-path))))
          (should-not (file-exists-p artifact-path)))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/native-exec-command-is-on-main-cli ()
  "The ordinary `nelisp' CLI dispatch exposes native `.neln' execution."
  (require 'nelisp-cli)
  (let ((seen nil))
    (cl-letf (((symbol-function 'native-exec-elisp-artifact)
               (lambda (args)
                 (setq seen args)
                 73)))
      (should (= 73 (nelisp-cli-main
                     '("nelisp" "native-exec-elisp-artifact"
                       "m.el.neln" "hot-fn" "41"))))
      (should (equal seen
                     '("native-exec-elisp-artifact"
                       "m.el.neln" "hot-fn" "41"))))))

(ert-deftest nelisp-artifact/standalone-host-helper-mode-prefers-opportunistic-neln ()
  "Standalone `.neln' compiles prefer the host helper even opportunistically."
  (let ((opts '(:native-policy opportunistic)))
    (cl-letf (((symbol-function 'nelisp-artifact--standalone-runtime-p)
               (lambda () t))
              ((symbol-function 'nelisp-artifact--standalone-windows-p)
               (lambda () nil)))
      (should (eq (nelisp-artifact--standalone-host-helper-mode opts 'neln)
                  'preferred))
      (should (eq (nelisp-artifact--standalone-host-helper-mode
                   '(:native-policy required) 'neln)
                  'preferred))
      (should-not (nelisp-artifact--standalone-host-helper-mode opts 'nelc)))))

(ert-deftest nelisp-artifact/standalone-host-helper-preferred-falls-back-when-disabled ()
  "Preferred helper mode still falls back when the helper is disabled."
  (let ((opts '(:native-policy opportunistic)))
    (cl-letf (((symbol-function 'nelisp-artifact--standalone-runtime-p)
               (lambda () t))
              ((symbol-function 'nelisp-artifact--standalone-windows-p)
               (lambda () nil))
              ((symbol-function 'nelisp-artifact--standalone-host-helper-disabled-p)
               (lambda () t))
              ((symbol-function 'nelisp-artifact--host-helper-emacs)
               (lambda ()
                 (error "host helper should not be consulted"))))
      (should (eq (nelisp-artifact--standalone-host-helper-mode opts 'neln)
                  'preferred))
      (should-not (nelisp-artifact--standalone-host-helper-compile opts 'neln)))))

(ert-deftest nelisp-artifact/standalone-host-helper-started-failure-is-hard ()
  "A failed preferred helper must not fall through to native compilation."
  (let* ((temp-dir (make-temp-file "nelisp-helper-hard-failure-" t))
         (source-path (expand-file-name "broken.el" temp-dir))
         (artifact-path (expand-file-name "broken.neln" temp-dir))
         (manifest-path (concat artifact-path ".manifest.el"))
         (native-called nil)
         (stderr ""))
    (unwind-protect
        (cl-letf (((symbol-function 'nelisp-artifact--standalone-runtime-p)
                   (lambda () t))
                  ((symbol-function 'nelisp-artifact--standalone-windows-p)
                   (lambda () nil))
                  ((symbol-function
                    'nelisp-artifact--standalone-host-helper-disabled-p)
                   (lambda () nil))
                  ((symbol-function 'nelisp-artifact--host-helper-emacs)
                   (lambda () "fake-emacs"))
                  ((symbol-function 'nelisp-artifact--make-temp-path)
                   (lambda (&rest _)
                     (expand-file-name "helper.log" temp-dir)))
                  ((symbol-function 'call-process)
                   (lambda (&rest _) 255))
                  ((symbol-function 'nelisp-artifact--read-log-if-exists)
                   (lambda (&rest _)
                     "bootstrap reader error"))
                  ((symbol-function 'nelisp-artifact--delete-if-exists)
                   (lambda (&rest _) nil))
                  ((symbol-function 'nelisp-artifact-compile-file)
                   (lambda (&rest _)
                     (setq native-called t)
                     (write-region "partial" nil artifact-path nil 'silent)))
                  ((symbol-function 'nelisp-artifact--write-stderr)
                   (lambda (text)
                     (setq stderr (concat stderr text)))))
          (should
           (= 1
              (compile-elisp-artifact
               (list "compile-elisp-artifact"
                     "--kind" "neln"
                     "--input" source-path
                     "--output" artifact-path))))
          (should-not native-called)
          (should-not (file-exists-p artifact-path))
          (should-not (file-exists-p manifest-path))
          (should (string-match-p "host-helper failed status=255" stderr))
          (should (string-match-p "bootstrap reader error" stderr)))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/nelisp-load-file-prefers-adjacent-neln ()
  "`nelisp-load-file' should use SOURCE.el.neln before reading SOURCE.el."
  (let* ((temp-dir (make-temp-file "nelisp-artifact-load-neln-" t))
         (source-path (expand-file-name "mod.el" temp-dir))
         (artifact-path (nelisp-artifact-source-artifact-path source-path 'neln)))
	    (unwind-protect
	        (progn
	          (write-region
	           "(defun load-adjacent-neln (x) (+ x 4))\n(provide 'load-adjacent-neln)\n"
           nil source-path nil 'silent)
          (nelisp-artifact-compile-file source-path artifact-path
                                        nil nil nil nil nil 'neln)
          (rename-file source-path (concat source-path ".gone") t)
          (nelisp--reset)
          (setq nelisp-artifact--loaded nil)
          (should (eq (nelisp-load-file source-path) 'load-adjacent-neln))
	          (should (= (nelisp-eval '(load-adjacent-neln 5)) 9)))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/source-loader-installs-adjacent-neln-native-wrapper ()
  "Generic source loading must use native wrappers from adjacent `.neln'.
This is the central invariant for arbitrary `.el' files: callers load the
source path, the artifact layer selects SOURCE.el.neln, native-eligible defuns
become native-first wrappers, and unsupported code remains covered by the
portable fallback."
  (skip-unless (memq system-type '(gnu/linux berkeley-unix)))
  (skip-unless (and (executable-find "cc") (executable-find "objcopy")))
  (let* ((temp-dir (make-temp-file "nelisp-artifact-source-native-" t))
         (source-path (expand-file-name "mod.el" temp-dir))
         (artifact-path (nelisp-artifact-source-artifact-path source-path 'neln)))
    (unwind-protect
        (progn
          (write-region
           "(defun source-native-add (x) (+ x 1))\n(provide 'source-native)\n"
           nil source-path nil 'silent)
          (let ((manifest (nelisp-artifact-compile-file
                           source-path artifact-path nil nil nil nil nil 'neln)))
            (should (plist-get manifest :native)))
          (nelisp--reset)
          (setq nelisp-artifact--loaded nil)
          (setq nelisp-artifact-native-dispatch-report nil)
          (should (eq (nelisp-load-file source-path) 'source-native))
          (let ((fn (gethash 'source-native-add nelisp--functions)))
            (if (fboundp 'nelisp--native-call-boundary)
                (progn
                  (should (consp fn))
                  (should (eq (car fn) 'nelisp-native-function))
                  (should (eq (nth 2 fn) 'source-native-add)))
              ;; Without a native call boundary the entry is the function the
              ;; native evaluator built from the item's SOURCE-DEFUN, not the
              ;; serialized `nelisp-bcl' closure: the bytecode form runs on an
              ;; Elisp VM that the evaluator then interprets, measured at
              ;; 60.75 ms per call against 0.27 ms for the native definition.
              (should (functionp fn))))
          (should (= (nelisp-eval '(source-native-add 41)) 42))
          (if (fboundp 'nelisp--native-call-boundary)
              (should
               (cl-some (lambda (entry)
                          (and (eq (plist-get entry :event) 'call)
                               (eq (plist-get entry :symbol) 'source-native-add)
                               (eq (plist-get entry :mode) 'native)))
                        (nelisp-artifact-native-dispatch-report)))
            (should-not (nelisp-artifact-native-dispatch-report))))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/nelisp-load-file-auto-recompiles-stale-neln ()
  "`nelisp-load-file' can refresh a missing/stale adjacent `.neln' generically."
  (let* ((temp-dir (make-temp-file "nelisp-artifact-load-refresh-" t))
         (source-path (expand-file-name "mod.el" temp-dir))
         (artifact-path (nelisp-artifact-source-artifact-path source-path 'neln)))
    (unwind-protect
        (let ((nelisp-load-auto-compile-artifacts t)
              (nelisp-load-auto-compile-kind 'neln))
          (write-region
           "(defun load-refresh-value () 1)\n(provide 'load-refresh)\n"
           nil source-path nil 'silent)
          (nelisp-artifact-compile-file source-path artifact-path
                                        nil nil nil nil nil 'neln)
          (write-region
           "(defun load-refresh-value () 2222)\n(provide 'load-refresh)\n"
           nil source-path nil 'silent)
          (nelisp--reset)
          (setq nelisp-artifact--loaded nil)
          (should (eq (nelisp-load-file source-path) 'load-refresh))
          (should (= (nelisp-eval '(load-refresh-value)) 2222))
          (let ((manifest (nelisp-artifact-read-manifest artifact-path)))
            (should (equal (plist-get (plist-get manifest :source) :path)
                           (expand-file-name source-path)))))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/load-neln-reads-manifest-once ()
  "`.neln' load should not parse the sibling manifest for kind probing.
The hot path validates once, then replays the already selected private
artifact format.  This avoids one manifest read/parse per artifact load."
  (let* ((temp-dir (make-temp-file "nelisp-artifact-manifest-once-" t))
         (source-path (expand-file-name "mod.el" temp-dir))
         (artifact-path (concat source-path ".neln"))
         (old-read-manifest (symbol-function 'nelisp-artifact--read-manifest-for-load))
         (manifest-reads 0))
    (unwind-protect
        (progn
          (write-region
           "(defun manifest-once-f (x) (+ x 1))\n(provide 'manifest-once)\n"
           nil source-path nil 'silent)
          (nelisp-artifact-compile-file source-path artifact-path
                                        nil nil nil nil nil 'neln)
          (nelisp--reset)
          (setq nelisp-artifact--loaded nil)
          (cl-letf (((symbol-function 'nelisp-artifact--read-manifest-for-load)
                     (lambda (&rest args)
                       (setq manifest-reads (1+ manifest-reads))
                       (apply old-read-manifest args))))
            (nelisp-artifact-load-file artifact-path))
          (should (= manifest-reads 1))
          (should (= (nelisp-eval '(manifest-once-f 2)) 3)))
      (fset 'nelisp-artifact--read-manifest-for-load old-read-manifest)
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/eval-private-artifact-reads-manifest-once ()
  "`eval-elisp-artifact' should not read the manifest just to decide KIND.
Private artifact command dispatch can select `.nelc' / `.neln' from the
file suffix, then let `nelisp-artifact-load-file' validate the sibling
manifest exactly once."
  (let* ((temp-dir (make-temp-file "nelisp-artifact-eval-manifest-once-" t))
         (source-path (expand-file-name "mod.el" temp-dir))
         (artifact-path (concat source-path ".nelc"))
         (old-read-manifest (symbol-function 'nelisp-artifact--read-manifest-for-load))
         (manifest-reads 0))
    (unwind-protect
        (progn
          (write-region
           "(defun eval-manifest-once-f (x) (+ x 1))\n(provide 'eval-manifest-once)\n"
           nil source-path nil 'silent)
          (nelisp-artifact-compile-file source-path artifact-path)
          (nelisp--reset)
          (setq nelisp-artifact--loaded nil)
          (cl-letf (((symbol-function 'nelisp-artifact--read-manifest-for-load)
                     (lambda (&rest args)
                       (setq manifest-reads (1+ manifest-reads))
                       (apply old-read-manifest args))))
            (should (= 0 (eval-elisp-artifact
                          (list "eval-elisp-artifact" artifact-path
                                "(eval-manifest-once-f 41)")))))
          (should (= manifest-reads 1)))
      (fset 'nelisp-artifact--read-manifest-for-load old-read-manifest)
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/native-exec-cache-key-skips-manifest-parse ()
  "Native fast cache hit detection must not parse the manifest.
The key is computed before deciding whether the linked driver already
exists.  Reading the manifest here makes cache hits pay the same slow
standalone plist parse cost as cache misses."
  (let* ((temp-dir (make-temp-file "nelisp-artifact-native-key-" t))
         (artifact-path (expand-file-name "mod.el.neln" temp-dir))
         (old-read-manifest (symbol-function 'nelisp-artifact-read-manifest))
         (old-secure-hash (symbol-function 'secure-hash)))
    (unwind-protect
        (progn
          (write-region "artifact bytes\n" nil artifact-path nil 'silent)
          (cl-letf (((symbol-function 'nelisp-artifact-read-manifest)
                     (lambda (&rest _args)
                       (error "cache key must not read manifest")))
                    ((symbol-function 'secure-hash)
                     (lambda (&rest _args)
                       (error "cache key must not call secure-hash"))))
            (let ((key (nelisp-artifact--native-exec-cache-key
                        artifact-path "native-key-f" 1)))
              (should (stringp key))
              (should (> (length key) 0)))))
      (fset 'nelisp-artifact-read-manifest old-read-manifest)
      (fset 'secure-hash old-secure-hash)
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/compile-elisp-artifacts-directory-adjacent-neln ()
  "`compile-elisp-artifacts' compiles a directory tree to adjacent `.neln'."
  (let* ((temp-dir (make-temp-file "nelisp-artifact-many-" t))
         (subdir (expand-file-name "sub" temp-dir))
         (source-a (expand-file-name "a.el" temp-dir))
         (source-b (expand-file-name "b.el" subdir)))
    (unwind-protect
        (progn
          (make-directory subdir)
          (write-region "(defun many-a (x) (+ x 1))\n(provide 'many-a)\n"
                        nil source-a nil 'silent)
          (write-region "(defvar many-b 7)\n(provide 'many-b)\n"
                        nil source-b nil 'silent)
          (should (= 0 (compile-elisp-artifacts
                        (list "compile-elisp-artifacts"
                              "--kind" "neln"
                              temp-dir))))
          (should (file-exists-p
                   (nelisp-artifact-source-artifact-path source-a 'neln)))
          (should (file-exists-p
                   (nelisp-artifact-source-artifact-path source-b 'neln))))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/cache-directory-uses-stable-nonadjacent-paths ()
  "A configured cache directory should reuse stable hashed paths without
creating adjacent artifacts, even for same-named sources in different trees."
  (let* ((temp-root (make-temp-file "nelisp-artifact-cache-root-" t))
         (cache-root (expand-file-name "nelisp-cache" temp-root))
         (source-a (expand-file-name "pkg/mod.el" temp-root))
         (source-b-root (expand-file-name "other" temp-root))
         (source-b (expand-file-name "pkg/mod.el" source-b-root))
         (artifact-a nil)
         (artifact-b nil))
    (unwind-protect
        (progn
          (make-directory (file-name-directory source-a) t)
          (make-directory (file-name-directory source-b) t)
          (write-region "(defvar cache-a 1)\n(provide 'cache-a)\n"
                        nil source-a nil 'silent)
          (write-region "(defvar cache-b 2)\n(provide 'cache-b)\n"
                        nil source-b nil 'silent)
          (let ((nelisp-artifact-cache-directory cache-root))
            (setq artifact-a (nelisp-artifact-source-artifact-path source-a 'neln)
                  artifact-b (nelisp-artifact-source-artifact-path source-b 'neln))
            (should (equal artifact-a
                           (nelisp-artifact-source-artifact-path source-a 'neln)))
            (should (not (equal artifact-a artifact-b)))
            (should-not (equal artifact-a (concat (expand-file-name source-a)
                                                  ".neln")))
            (should-not (equal artifact-b (concat (expand-file-name source-b)
                                                  ".neln")))
            (nelisp-artifact-compile-file source-a artifact-a)
            (nelisp-artifact-compile-file source-b artifact-b)
            (should (file-exists-p artifact-a))
            (should (file-exists-p artifact-b))
            (should (file-directory-p (file-name-directory artifact-a)))
            (should (file-directory-p (file-name-directory artifact-b)))
            (should (string-prefix-p (file-name-as-directory
                                      (expand-file-name cache-root))
                                     artifact-a))
            (should (string-prefix-p (file-name-as-directory
                                      (expand-file-name cache-root))
                                     artifact-b))
            (should-not (file-exists-p (concat (expand-file-name source-a)
                                               ".neln")))
            (should-not (file-exists-p (concat (expand-file-name source-b)
                                               ".neln")))
            (setq nelisp-artifact--loaded nil)
            (should (equal (plist-get
                            (nelisp-artifact-load-or-compile-source-file
                             source-a '(neln) 'neln)
                            :artifact)
                           artifact-a))))
      (when (file-directory-p temp-root)
        (delete-directory temp-root t)))))

(ert-deftest nelisp-artifact/audit-elisp-artifacts-reports-native-coverage ()
  "`audit-elisp-artifacts' reports adjacent `.neln' native coverage."
  (let* ((temp-dir (make-temp-file "nelisp-artifact-audit-" t))
         (source-path (expand-file-name "mod.el" temp-dir))
         (artifact-path (nelisp-artifact-source-artifact-path source-path 'neln))
         (stdout nil))
    (unwind-protect
        (progn
          (write-region
           "(defun audit-native-add (x) (+ x 1))\n(provide 'audit-native)\n"
           nil source-path nil 'silent)
          (nelisp-artifact-compile-file source-path artifact-path
                                        nil nil nil nil nil 'neln 'required)
          (cl-letf (((symbol-function 'nelisp-artifact--write-stdout)
                     (lambda (text)
                       (setq stdout (concat stdout text)))))
            (should (= 0 (audit-elisp-artifacts
                          (list "audit-elisp-artifacts" temp-dir)))))
          (should (string-match-p "artifact_audit status=ok" stdout))
          (should (string-match-p "artifact_audit_summary status=ok" stdout))
          (should (string-match-p "defuns=1 native=1 gaps=0" stdout)))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/audit-elisp-artifacts-uses-fast-manifest-reader ()
  "`audit-elisp-artifacts' should not parse the full native manifest payload."
  (let* ((temp-dir (make-temp-file "nelisp-artifact-audit-fast-" t))
         (source-path (expand-file-name "mod.el" temp-dir))
         (artifact-path (nelisp-artifact-source-artifact-path source-path 'neln))
         (stdout nil))
    (unwind-protect
        (progn
          (write-region
           "(defun audit-fast-add (x) (+ x 1))\n(provide 'audit-fast)\n"
           nil source-path nil 'silent)
          (nelisp-artifact-compile-file source-path artifact-path
                                        nil nil nil nil nil 'neln 'required)
          (cl-letf (((symbol-function 'nelisp-artifact--read-manifest-full)
                     (lambda (_artifact)
                       (error "full manifest reader should not run")))
                    ((symbol-function 'nelisp-artifact--write-stdout)
                     (lambda (text)
                       (setq stdout (concat stdout text)))))
            (should (= 0 (audit-elisp-artifacts
                          (list "audit-elisp-artifacts" temp-dir)))))
          (should (string-match-p "artifact_audit status=ok" stdout))
          (should (string-match-p "defuns=1 native=1 gaps=0" stdout)))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/fast-manifest-reduces-sharded-native-metadata ()
  "Fast manifest only reads install fields from large legacy native shards."
  (let* ((temp-dir (make-temp-file "nelisp-manifest-native-stream-" t))
         (artifact-path (expand-file-name "large.neln" temp-dir))
         (manifest-path (concat artifact-path ".manifest.el"))
         (large-data (make-string 200000 ?x))
         (reader-spans nil)
         (private-reader (symbol-function
                          'nelisp-artifact--read-private-item)))
    (unwind-protect
        (progn
          (write-region
           (concat
            "(:format nelisp-elisp-artifact-manifest-v1 :kind neln "
            ":native-sections "
            "((:symbols (\"a\") :relocs ((:large \"" large-data "\")) "
            ":compile-report ((:large \"" large-data "\")) "
            ":defuns ((:name \"a\" :arity 1))) "
            "(:symbols (\"b\") :relocs ((:large \"" large-data "\")) "
            ":compile-report ((:large \"" large-data "\")) "
            ":defuns ((:name \"b\" :arity 2)))) "
            ":native-report ((:name \"a\" :native t)))\n")
           nil manifest-path nil 'silent)
          (cl-letf (((symbol-function 'nelisp--string-search)
                     (lambda (needle haystack start)
                       (string-search needle haystack start)))
                    ((symbol-function
                      'nelisp-artifact--private-list-key-positions)
                     (lambda (&rest _)
                       (error "native literal search should avoid scanner")))
                    ((symbol-function 'nelisp-artifact--read-private-item)
                     (lambda (source start end)
                       (setq reader-spans (cons (- end start) reader-spans))
                       (funcall private-reader source start end))))
            (let* ((manifest
                    (nelisp-artifact--read-manifest-fast
                     artifact-path
                     '(:format :kind :native :native-sections :native-report)))
                   (sections (plist-get manifest :native-sections)))
              (should (eq (plist-get manifest :kind) 'neln))
              (should-not (plist-member manifest :native))
              (should (= (length sections) 2))
              (should (equal (plist-get (car sections) :symbols) '("a")))
              (should (equal (plist-get (cadr sections) :symbols) '("b")))
              (should-not (plist-member (car sections) :compile-report))
              (should (equal (plist-get
                              (car (plist-get (car sections) :defuns))
                              :name)
                             "a"))))
          ;; Two small fields per section enter the reader; neither 200 KB
          ;; relocation/report value is materialized.
          (should (= (length reader-spans) 4))
          (should (< (apply #'max reader-spans) 100)))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/inspect-skips-native-sections-but-keeps-report ()
  "Inspect reports coverage without parsing or printing native section data."
  (let* ((temp-dir (make-temp-file "nelisp-inspect-fast-" t))
         (artifact-path (expand-file-name "large.neln" temp-dir))
         (manifest-path (concat artifact-path ".manifest.el"))
         (nelisp-artifact-fast-private-read t)
         (stdout ""))
    (unwind-protect
        (progn
          (write-region
           (concat
            "(:format nelisp-elisp-artifact-manifest-v1 :kind neln "
            ":artifact-format nelisp-private-nelc-v2 "
            ":native-sections ((:symbols (\"huge\") "
            ":defuns ((:name \"huge\")))) "
            ":native-report ((:name \"gap\" :native nil "
            ":reason \"unsupported\")) :top-level-count 2)\n")
           nil manifest-path nil 'silent)
          (cl-letf (((symbol-function 'nelisp-artifact--read-manifest-full)
                     (lambda (&rest _)
                       (error "full manifest reader should not run")))
                    ((symbol-function 'nelisp-artifact--write-stdout)
                     (lambda (text)
                       (setq stdout (concat stdout text)))))
            (should (= (inspect-elisp-artifact
                        (list "inspect-elisp-artifact" artifact-path))
                       0)))
          (should (string-match-p ":native-report" stdout))
          (should (string-match-p "unsupported" stdout))
          (should-not (string-match-p ":native-sections" stdout))
          (should-not (string-match-p "\"huge\"" stdout)))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/audit-elisp-artifacts-required-fails-on-gaps ()
  "`audit-elisp-artifacts --required' fails when native coverage has gaps."
  (let* ((temp-dir (make-temp-file "nelisp-artifact-audit-gap-" t))
         (source-path (expand-file-name "mod.el" temp-dir))
         (artifact-path (nelisp-artifact-source-artifact-path source-path 'neln))
         (stdout nil))
    (unwind-protect
        (progn
          (write-region
           "(defun audit-gap-add (x) (+ x 1))\n(provide 'audit-gap)\n"
           nil source-path nil 'silent)
          (nelisp-artifact-compile-file source-path artifact-path
                                        nil "wasm32-unknown"
                                        nil nil nil 'neln)
          (cl-letf (((symbol-function 'nelisp-artifact--write-stdout)
                     (lambda (text)
                       (setq stdout (concat stdout text)))))
            (should (= 1 (audit-elisp-artifacts
                          (list "audit-elisp-artifacts"
                                "--required" temp-dir)))))
          (should (string-match-p "artifact_audit status=gaps" stdout))
          (should (string-match-p "gap_names=(\"audit-gap-add\")" stdout))
          (should (string-match-p "artifact_audit_summary status=gaps" stdout)))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/profile-forms-emits-reader-form-lines ()
  "`nelisp-artifact-profile-forms' emits per-form reader profile lines."
  (let* ((temp-dir (make-temp-file "nelisp-artifact-profile-forms-" t))
         (source-path (expand-file-name "mod.el" temp-dir))
         (artifact-path (concat source-path ".nelc"))
         (stderr nil))
    (unwind-protect
        (progn
          (write-region
           "(defun profile-form-a (x) x)\n(provide 'profile-form)\n"
           nil source-path nil 'silent)
          (let ((nelisp-artifact-profile-stages t)
                (nelisp-artifact-profile-forms t))
            (cl-letf (((symbol-function 'nelisp-artifact--write-stderr)
                       (lambda (text)
                         (setq stderr (concat stderr text "\n")))))
              (nelisp-artifact-compile-file
               source-path artifact-path nil nil nil nil nil 'nelc
               nil 'eval-only)))
          (should (string-match-p
                   "artifact_profile_form .* index=0 .* head=\"defun\""
                   stderr))
          (should (string-match-p
                   "artifact_profile_form .* index=1 .* head=\"provide\""
                   stderr))
          (should (string-match-p
                   "artifact_profile stage=read-forms "
                   stderr)))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/load-fast-private-streams-module-init ()
  "Fast private load replays `:module-init' without full payload parsing."
  (let* ((temp-dir (make-temp-file "nelisp-artifact-fast-load-" t))
         (source-path (expand-file-name "mod.el" temp-dir))
         (artifact-path (concat source-path ".nelc"))
         (feature 'fast-private-stream-load))
    (unwind-protect
        (progn
          (setq features (delq feature features))
          (write-region
           "(defvar fast-private-stream-load-value 17)
(provide 'fast-private-stream-load)\n"
           nil source-path nil 'silent)
          (nelisp-artifact-compile-file
           source-path artifact-path nil nil nil nil nil 'nelc nil 'eval-only)
          (nelisp--reset)
          (setq nelisp-artifact--loaded nil)
          (let ((nelisp-artifact-fast-private-read t))
            (cl-letf (((symbol-function 'nelisp-artifact--parse-payload-fast)
                       (lambda (&rest _)
                         (error "full fast payload parse must not run")))
                      ((symbol-function 'nelisp-artifact--parse-payload)
                       (lambda (&rest _)
                         (error "full payload parse must not run"))))
              (nelisp-artifact-load-file artifact-path)))
          (should (= (nelisp-eval 'fast-private-stream-load-value) 17))
          (should (nelisp-eval '(featurep 'fast-private-stream-load))))
      (when (boundp 'fast-private-stream-load-value)
        (makunbound 'fast-private-stream-load-value))
      (setq features (delq feature features))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/fast-load-error-does-not-full-parse-large-payload ()
  "A fast-reader error fails closed without materializing a large payload."
  (let* ((artifact-path "/tmp/nelisp-artifact-fast-fail-closed.neln")
         (content
          (concat nelisp-artifact--magic
                  "(:format nelisp-private-nelc-v2 :kind neln :padding \""
                  (make-string 200000 ?x)
                  "\" :features nil :module-init nil)\n"))
         (fast-calls 0)
         (full-parse-calls 0)
         (nelisp-artifact--loaded nil)
         (nelisp-artifact-fast-private-read t))
    (cl-letf (((symbol-function 'nelisp-artifact--read-file-as-string)
               (lambda (_path) content))
              ((symbol-function 'nelisp-artifact--validate)
               (lambda (_path _content) '(:kind neln :load-path nil)))
              ((symbol-function 'nelisp-artifact--load-private-fast)
               (lambda (&rest _)
                 (setq fast-calls (1+ fast-calls))
                 (error "synthetic fast reader failure")))
              ((symbol-function 'nelisp-artifact--parse-payload)
               (lambda (&rest _)
                 (setq full-parse-calls (1+ full-parse-calls))
                 (error "full payload parser must not run"))))
      (should-error (nelisp-artifact-load-file artifact-path)
                    :type 'error))
    (should (= fast-calls 1))
    (should (= full-parse-calls 0))
    (should-not (member (expand-file-name artifact-path)
                        nelisp-artifact--loaded))))

(ert-deftest nelisp-artifact/fast-load-partial-replay-is-not-retried ()
  "An error after partial streaming replay does not replay items twice."
  (let* ((artifact-path "/tmp/nelisp-artifact-fast-partial.nelc")
         (content
          (concat nelisp-artifact--magic
                  "(:format nelisp-private-nelc-v2 :kind nelc"
                  " :features nil :module-init"
                  " ((:eval first) (:eval second)))\n"))
         (original-parse
          (symbol-function 'nelisp-artifact--parse-payload))
         (replay-count 0)
         (full-parse-calls 0)
         (nelisp-artifact--loaded nil)
         (nelisp-artifact-fast-private-read t)
         (nelisp-artifact-module-replay-chunk-size nil)
         (nelisp-artifact-native-dispatch-enabled nil))
    (cl-letf (((symbol-function 'nelisp-artifact--read-file-as-string)
               (lambda (_path) content))
              ((symbol-function 'nelisp-artifact--validate)
               (lambda (_path _content) '(:kind nelc :load-path nil)))
              ((symbol-function 'nelisp-artifact--parse-payload)
               (lambda (&rest args)
                 (setq full-parse-calls (1+ full-parse-calls))
                 (apply original-parse args)))
              ((symbol-function 'nelisp-artifact--replay-module-item)
               (lambda (_item)
                 (setq replay-count (1+ replay-count))
                 (when (= replay-count 2)
                   (error "synthetic second-item failure"))
                 replay-count)))
      (should-error (nelisp-artifact-load-file artifact-path)
                    :type 'error))
    (should (= replay-count 2))
    (should (= full-parse-calls 0))
    (should-not (member (expand-file-name artifact-path)
                        nelisp-artifact--loaded))))

(ert-deftest nelisp-artifact/explicit-full-read-keeps-full-parse-path ()
  "Disabling the fast reader explicitly retains whole-payload replay."
  (let* ((artifact-path "/tmp/nelisp-artifact-explicit-full.nelc")
         (content
          (concat nelisp-artifact--magic
                  "(:format nelisp-private-nelc-v2 :kind nelc"
                  " :features nil :module-init nil)\n"))
         (fast-calls 0)
         (full-parse-calls 0)
         (original-parse
          (symbol-function 'nelisp-artifact--parse-payload))
         (nelisp-artifact--loaded nil)
         (nelisp-artifact-fast-private-read nil)
         (nelisp-artifact-native-dispatch-enabled nil))
    (cl-letf (((symbol-function 'nelisp-artifact--read-file-as-string)
               (lambda (_path) content))
              ((symbol-function 'nelisp-artifact--validate)
               (lambda (_path _content) '(:kind nelc :load-path nil)))
              ((symbol-function 'nelisp-artifact--load-private-fast)
               (lambda (&rest _)
                 (setq fast-calls (1+ fast-calls))
                 (error "fast loader must not run")))
              ((symbol-function 'nelisp-artifact--parse-payload)
               (lambda (&rest args)
                 (setq full-parse-calls (1+ full-parse-calls))
                 (apply original-parse args))))
      (should-not (nelisp-artifact-load-file artifact-path)))
    (should (= fast-calls 0))
    (should (= full-parse-calls 1))
    (should (member (expand-file-name artifact-path)
                    nelisp-artifact--loaded))))

(ert-deftest nelisp-artifact/module-streaming-chunks-reach-top-level-boundaries ()
  "Standalone streaming batches items without losing replay state across GC."
  (let* ((content
          (concat
           nelisp-artifact--magic
           "(:format nelisp-private-nelc-v2 :kind nelc :source \"proof.el\""
           " :module-init ("
           "(:eval (setq artifact-chunk-proof-value 1))"
           " (:eval (setq artifact-chunk-proof-value"
           " (+ artifact-chunk-proof-value 1)))"
           " (:eval (defun artifact-chunk-proof-add (x) (+ x 1)))"
           " (:eval (artifact-chunk-proof-add"
           " (+ artifact-chunk-proof-value 40))))"
           " :features nil :top-level-count 4"
           " :module-policy eval-only :compiler nil :entry nil)\n"))
         (chunks nil)
         (eval-count 0)
         (gc-count 0)
         (last nil))
    (unwind-protect
        (progn
          (nelisp--reset)
          (cl-letf
              (((symbol-function 'nelisp--eval-source-string)
                (lambda (source)
                  ;; Exercise the caller's CONTENT/POS/LAST roots before and
                  ;; after every small source evaluation.  The standalone
                  ;; builtin reaches its native form-boundary collector at the
                  ;; equivalent points.
                  (setq gc-count (1+ gc-count))
                  (garbage-collect)
                  (setq chunks (cons source chunks))
                  (setq eval-count (1+ eval-count))
                  (let ((forms
                         (nelisp-artifact--read-all-from-string source))
                        (value nil))
                    (dolist (form forms)
                      (setq value (eval form))
                      (setq gc-count (1+ gc-count))
                      (garbage-collect))
                    value))))
            (let ((nelisp-artifact-module-replay-chunk-size 2))
              (setq last
                    (nelisp-artifact--replay-module-streaming
                     content "chunk-proof.nelc"))))
          (setq chunks (nreverse chunks))
          (should (= eval-count 2))
          (should (= gc-count 6))
          (should (= (length chunks) 2))
          (dolist (chunk chunks)
            (should (= (length
                        (nelisp-artifact--read-all-from-string chunk))
                       2)))
          (should (= last 43))
          (should (= (nelisp-eval 'artifact-chunk-proof-value) 2))
          (should (= (nelisp-eval '(artifact-chunk-proof-add 41)) 42)))
      (when (boundp 'artifact-chunk-proof-value)
        (makunbound 'artifact-chunk-proof-value))
      (nelisp--reset))))

(ert-deftest nelisp-artifact/module-streaming-renders-canonical-fn-directly ()
  "Canonical functions omit their retained source DEFUN from chunk parsing."
  (let* ((fn "(:fn direct-fn (nelisp-bcl (x) nil) (defun direct-fn (x) x))")
         (eval "(:eval (setq direct-fn-proof 7))")
         (content (concat fn " " eval))
         (fn-end (nelisp-artifact--private-item-end
                  content 0 (length content) "direct-fn.neln"))
         (eval-start (1+ fn-end))
         (eval-end (nelisp-artifact--private-item-end
                    content eval-start (length content) "direct-fn.neln"))
         (descriptor (nelisp-artifact--module-fn-descriptor
                      content 0 fn-end "direct-fn.neln"))
         (rendered nil))
    (should (vectorp descriptor))
    (cl-letf (((symbol-function 'nelisp--eval-source-string)
               (lambda (source)
                 (setq rendered source)
                 'direct-result)))
      (should
       (eq (nelisp-artifact--replay-module-source-chunk
            content (list descriptor (cons eval-start eval-end)))
           'direct-result)))
    ;; The canonical `:fn' render now defines the function from its
    ;; SOURCE-DEFUN so the native evaluator owns it, then registers the
    ;; resulting object (measured 0.27 ms per call against 60.75 ms for
    ;; the serialized bytecode closure).
    (should (string-prefix-p "(defun direct-fn " rendered))
    (should (string-match-p
             "(puthash 'direct-fn (symbol-function 'direct-fn) nelisp--functions)"
             rendered))
            ;; Reversal of the earlier "never materialize SOURCE-DEFUN" rule
            ;; (handoff §4.3/§5): that rule was chosen when the alternative was
            ;; installing the serialized `nelisp-bcl', which runs on an Elisp
            ;; bytecode VM the native evaluator then interprets -- 60.75 ms per
            ;; call against 0.27 ms for the natively defined function.  Parsing
            ;; the source once is now the cheap side: the whole runtime-cache
            ;; load went 570 s to 24 s with this render in place.
            (should-not (string-match-p "nelisp-bcl" rendered))
    ;; Ordering is still the property under test; the canonical `:fn' render is
    ;; now the source defun itself, so match on that instead of the former
    ;; `install-function' call.
    (should
     (< (string-match "(defun direct-fn " rendered)
        (string-match "replay-module-item" rendered)))))

(ert-deftest nelisp-artifact/module-fn-descriptor-rejects-corruption ()
  "Canonical `:fn' scanning validates bytecode, source name and exact arity."
  (dolist (item
           '("(:fn bad-bcl (not-bcl nil) (defun bad-bcl () nil))"
             "(:fn bad-name (nelisp-bcl nil nil) (defun other () nil))"
             "(:fn extra (nelisp-bcl nil nil) (defun extra () nil) extra)"))
    (should-error
     (nelisp-artifact--module-fn-descriptor
      item 0 (length item) "corrupt-fn.neln"))))

(ert-deftest nelisp-artifact/load-fast-private-reads-features-with-token-reader ()
  "Fast private load reads generated feature lists without the sexp reader."
  (let* ((temp-dir (make-temp-file "nelisp-artifact-fast-feature-" t))
         (source-path (expand-file-name "mod.el" temp-dir))
         (artifact-path (concat source-path ".nelc"))
         (feature 'fast-private-feature-token))
    (unwind-protect
        (progn
          (setq features (delq feature features))
          (write-region
           "(defvar fast-private-feature-token-value 29)
(provide 'fast-private-feature-token)\n"
           nil source-path nil 'silent)
          (nelisp-artifact-compile-file
           source-path artifact-path nil nil nil nil nil 'nelc nil 'eval-only)
          (nelisp--reset)
          (setq nelisp-artifact--loaded nil)
          (let ((nelisp-artifact-fast-private-read t))
            (cl-letf (((symbol-function 'nelisp-artifact--read-private-keyword-value)
                       (lambda (_source keyword &rest _args)
                         (when (eq keyword :features)
                           (error "feature list must use token reader"))
                         nelisp-artifact--missing-key)))
              (nelisp-artifact-load-file artifact-path)))
          (should (= (nelisp-eval 'fast-private-feature-token-value) 29))
          (should (nelisp-eval '(featurep 'fast-private-feature-token))))
      (when (boundp 'fast-private-feature-token-value)
        (makunbound 'fast-private-feature-token-value))
      (setq features (delq feature features))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/load-fast-private-ignores-nested-features-key ()
  "Only the artifact outer plist can supply provided feature symbols."
  (let* ((artifact-path "/tmp/top-level-features.nelc")
         (nested 'nested-feature-decoy)
         (top 'top-level-feature-authoritative)
         (content
          (concat
           nelisp-artifact--magic
           "(:format nelisp-private-nelc-v2 :kind nelc "
           ":module-init ((:eval (quote (:features (nested-feature-decoy))))) "
           ":features (top-level-feature-authoritative) :entry nil)\n"))
         (scanner
          (symbol-function 'nelisp-artifact--private-list-key-positions))
         (scanner-calls 0)
         (provided nil)
         (nelisp-artifact-native-dispatch-enabled nil))
    (setq features (delq nested (delq top features)))
    (unwind-protect
        (cl-letf
            (((symbol-function 'nelisp-artifact--private-list-key-positions)
              (lambda (&rest args)
                (setq scanner-calls (1+ scanner-calls))
                (apply scanner args)))
             ((symbol-function 'nelisp-artifact--replay-module-streaming)
              (lambda (&rest _) nil))
             ((symbol-function 'nelisp-artifact--string-search-literal)
              (lambda (&rest _)
                (error "exact top-level key positions must not be rescanned")))
             ((symbol-function 'nelisp-provide)
              (lambda (feature)
                (setq provided (cons feature provided))
                feature)))
          (nelisp-artifact--load-private-fast
           artifact-path content '(:kind nelc))
          (should (= scanner-calls 1))
          (should (equal provided (list top)))
          (should-not (memq nested provided)))
      (setq features (delq nested (delq top features))))))

(ert-deftest nelisp-artifact/load-fast-private-bounds-header-key-scan ()
  "Header-first artifacts never scan a large native tail for load keys."
  (let* ((artifact-path "/tmp/bounded-header-key-scan.nelc")
         (content
          (concat
           nelisp-artifact--magic
           "(:format nelisp-private-nelc-v2 :kind nelc "
           ":features (bounded-header-feature) :native \""
           (make-string 100000 ?x)
           "\" :module-init nil :entry nil)\n"))
         (scanner
          (symbol-function 'nelisp-artifact--private-list-key-positions))
         (scan-ends nil)
         (nelisp-artifact-native-dispatch-enabled nil))
    (unwind-protect
        (cl-letf
            (((symbol-function 'nelisp-artifact--private-list-key-positions)
              (lambda (source start end keys label &rest options)
                (setq scan-ends (cons end scan-ends))
                (apply scanner source start end keys label options)))
             ((symbol-function 'nelisp-artifact--replay-module-streaming)
              (lambda (&rest _) nil))
             ((symbol-function 'nelisp-provide)
              (lambda (feature) feature)))
          (nelisp-artifact--load-private-fast
           artifact-path content '(:kind nelc))
          (should (equal scan-ends '(4096))))
      (setq features (delq 'bounded-header-feature features)))))

(ert-deftest nelisp-artifact/module-policy-eval-only-skips-bytecode-defuns ()
  "`--module-policy eval-only' records every top-level form as replay.
This keeps very large bootstrap substrates artifact-cacheable even when the
bytecode compiler path is too slow for the current development gate."
  (let* ((temp-dir (make-temp-file "nelisp-artifact-eval-only-" t))
         (source-path (expand-file-name "mod.el" temp-dir))
         (artifact-path (concat source-path ".nelc"))
         (manifest-path (concat artifact-path ".manifest.el")))
    (unwind-protect
        (progn
          (write-region
           "(defun eval-only-artifact-f (x) x)\n(provide 'eval-only-artifact)\n"
           nil source-path nil 'silent)
          (nelisp-artifact-compile-file
           source-path artifact-path nil nil nil nil nil 'nelc nil 'eval-only)
          (let* ((manifest (nelisp-artifact-read-manifest artifact-path))
                 (payload (nelisp-artifact--read-payload artifact-path))
                 (module (plist-get payload :module-init)))
            (should (eq (plist-get manifest :module-policy) 'eval-only))
            (should (eq (plist-get payload :module-policy) 'eval-only))
            (should-not (seq-some (lambda (item)
                                    (and (consp item) (eq (car item) :fn)))
                                  module))
            (should (seq-some (lambda (item)
                                (and (consp item) (eq (car item) :eval)))
                              module)))
          (nelisp--reset)
          (setq nelisp-artifact--loaded nil)
          (nelisp-artifact-load-file artifact-path)
          (should (= (nelisp-eval '(eval-only-artifact-f 42)) 42))
          (should (nelisp-eval '(featurep 'eval-only-artifact)))
          (should (file-exists-p manifest-path)))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/eval-elisp-source-prefers-adjacent-neln ()
  "`eval-elisp-source' uses the generic adjacent `.neln' source policy."
  (let* ((temp-dir (make-temp-file "nelisp-artifact-source-cli-" t))
         (source-path (expand-file-name "mod.el" temp-dir))
         (artifact-path (nelisp-artifact-source-artifact-path source-path 'neln))
         (stdout nil))
    (unwind-protect
        (progn
          (write-region
           "(defun source-cli-adjacent (x) (+ x 10))\n(provide 'source-cli-adjacent)\n"
           nil source-path nil 'silent)
          (nelisp-artifact-compile-file source-path artifact-path
                                        nil nil nil nil nil 'neln)
          (rename-file source-path (concat source-path ".gone") t)
          (nelisp--reset)
          (setq nelisp-artifact--loaded nil)
          (cl-letf (((symbol-function 'nelisp-artifact--write-stdout)
                     (lambda (text)
                       (setq stdout (concat stdout text)))))
            (should (= 0 (eval-elisp-source
                          (list "eval-elisp-source" source-path
                                "(source-cli-adjacent 32)")))))
          (should (equal stdout "42\n")))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/load-elisp-source-auto-compiles-neln ()
  "`load-elisp-source --auto-compile' creates and loads adjacent `.neln'."
  (let* ((temp-dir (make-temp-file "nelisp-artifact-source-auto-" t))
         (source-path (expand-file-name "mod.el" temp-dir))
         (artifact-path (nelisp-artifact-source-artifact-path source-path 'neln))
         (stdout nil))
    (unwind-protect
        (progn
          (write-region
           "(defvar source-auto-value 42)\n(provide 'source-auto)\n"
           nil source-path nil 'silent)
          (nelisp--reset)
          (setq nelisp-artifact--loaded nil)
          (cl-letf (((symbol-function 'nelisp-artifact--write-stdout)
                     (lambda (text)
                       (setq stdout (concat stdout text)))))
            (should (= 0 (load-elisp-source
                          (list "load-elisp-source" "--auto-compile"
                                "--kind" "neln" source-path)))))
          (should (file-exists-p artifact-path))
          (should (file-exists-p
                   (nelisp-artifact--sibling-manifest-path artifact-path)))
          (should (equal stdout "source-auto\n"))
          (should (= (nelisp-eval 'source-auto-value) 42)))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/runtime-image-compile-cache-loads-and-stales ()
  "A source replay runtime image can be compiled into a loadable artifact cache."
  (let* ((temp-dir (make-temp-file "nelisp-runtime-image-artifact-" t))
         (image-path (expand-file-name "runtime.nlri" temp-dir))
         (artifact-path (expand-file-name "runtime.nelc" temp-dir)))
    (unwind-protect
        (progn
          (write-region
           ";;; nelisp-runtime-image source-v1
(progn
(defvar rt-cache-var 40)
(defun rt-cache-hot (x) (+ x rt-cache-var))
(provide 'rt-cache)
)
"
           nil image-path nil 'silent)
          (should (= 0 (compile-runtime-image
                        (list "compile-runtime-image" "--kind" "nelc"
                              "--input" image-path "--output" artifact-path))))
          (should (file-exists-p artifact-path))
          (let ((manifest (nelisp-artifact-read-manifest artifact-path)))
            (should (equal (plist-get (plist-get manifest :runtime-image) :path)
                           (expand-file-name image-path)))
            (should (eq (plist-get (plist-get manifest :entry) :type)
                        'runtime-image)))
          (nelisp--reset)
          (setq nelisp-artifact--loaded nil)
          (nelisp-artifact-load-file artifact-path)
          (should (= (nelisp-eval '(rt-cache-hot 2)) 42))
          (write-region
           ";;; nelisp-runtime-image source-v1
(progn
(defvar rt-cache-var 999)
)
"
           nil image-path nil 'silent)
          (setq nelisp-artifact--loaded nil)
          (should-error (nelisp-artifact-load-file artifact-path)
                        :type 'nelisp-artifact-stale))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/flat-image-cache-warm-hit-skips-module-replay ()
  "A fresh flat image cache bypasses `.neln' module replay on warm prepare."
  (let* ((temp-dir (make-temp-file "nelisp-flat-image-cache-" t))
         (artifact-path (expand-file-name "small.neln" temp-dir))
         (image-path (expand-file-name "small.flat.nlri" temp-dir))
         (runtime-path (expand-file-name "nelisp" temp-dir))
         (manifest
          (list :format nelisp-artifact--manifest-format
                :kind 'neln
                :artifact-format nelisp-artifact--format
                :artifact-class nelisp-artifact--native-class
                :runtime-abi nelisp-artifact--native-runtime-abi
                :artifact-sha256 "artifact-a"
                :artifact-size 8
                :compiler (nelisp-artifact--compiler-plist)
                :target "synthetic"
                :source '(:path "/synthetic/source.el"
                          :sha256 "source-a")))
         (load-count 0)
         (dump-count 0))
    (unwind-protect
        (progn
          (write-region "artifact" nil artifact-path nil 'silent)
          (write-region "runtime" nil runtime-path nil 'silent)
          (cl-letf (((symbol-function
                      'nelisp-artifact--validate-flat-image-artifact)
                     (lambda (_artifact) manifest))
                    ((symbol-function 'nelisp-artifact--sha256-file)
                     (lambda (path)
                       (cond
                        ((string-suffix-p ".manifest.el" path)
                         (plist-get manifest :artifact-sha256))
                        ((equal (expand-file-name path)
                                (expand-file-name artifact-path))
                         (plist-get manifest :artifact-sha256))
                        ((equal (expand-file-name path)
                                (expand-file-name runtime-path))
                         (secure-hash
                          'sha256
                          (nelisp-artifact--read-file-as-string path)))
                        (t
                         (secure-hash
                          'sha256
                          (nelisp-artifact--read-file-as-string path))))))
                    ((symbol-function 'nelisp-artifact--flat-image-header)
                     (lambda (path &optional _known-size)
                       (list :magic 1179407692 :slen 1 :isz 0 :tlen 0
                             :globals-offset 0 :frames-offset 0
                             :unbound-offset 0
                             :expected-size
                             (nelisp-artifact--file-size path))))
                    ((symbol-function 'nelisp-artifact-load-file)
                     (lambda (_artifact)
                       (setq load-count (1+ load-count))
                       'loaded))
                    ((symbol-function 'nelisp--arena-dump-image-stream)
                     (lambda (path)
                       (setq dump-count (1+ dump-count))
                       (write-region "FLAT-image" nil path nil 'silent)
                       10)))
            (let ((cold (nelisp-artifact-prepare-flat-image-cache
                         artifact-path image-path runtime-path)))
              (should (eq (plist-get cold :status) 'rebuilt)))
            (should (= load-count 1))
            (should (= dump-count 1))
            (let ((warm (nelisp-artifact-prepare-flat-image-cache
                         artifact-path image-path runtime-path)))
              (should (eq (plist-get warm :status) 'hit)))
            ;; Warm validation reads metadata, but never calls the module
            ;; loader or heap dumper.
            (should (= load-count 1))
            (should (= dump-count 1))
            ;; Same-length corruption is caught by the image digest, not by
            ;; the size/header fields alone.
            (write-region "FLAT-xmage" nil image-path nil 'silent)
            (should (eq (plist-get
                         (nelisp-artifact-prepare-flat-image-cache
                          artifact-path image-path runtime-path)
                         :status)
                        'rebuilt))
            (should (= load-count 2))
            (should (= dump-count 2))
            ;; Runtime executable identity is part of the snapshot ABI.
            (write-region "RUNTIME" nil runtime-path nil 'silent)
            (should (eq (plist-get
                         (nelisp-artifact-prepare-flat-image-cache
                          artifact-path image-path runtime-path)
                         :status)
                        'rebuilt))
            (should (= load-count 3))
            (should (= dump-count 3))
            ;; A new validated artifact identity invalidates only the flat
            ;; image layer and rebuilds it once.
            (setq manifest (plist-put (copy-sequence manifest)
                                      :artifact-sha256 "artifact-b"))
            (let ((stale (nelisp-artifact-prepare-flat-image-cache
                          artifact-path image-path runtime-path)))
              (should (eq (plist-get stale :status) 'rebuilt)))
            (should (= load-count 4))
            (should (= dump-count 4))))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/flat-image-cache-profile-load-is-bounded ()
  "Normal load profiling is aggregate; debug detail is an explicit opt-in."
  (let* ((temp-dir (make-temp-file "nelisp-flat-profile-load-" t))
         (source-path (expand-file-name "small.el" temp-dir))
         (artifact-path (expand-file-name "small.neln" temp-dir))
         (image-path (expand-file-name "small.flat.nlri" temp-dir))
         (runtime-path (expand-file-name "nelisp" temp-dir))
         (stderr nil)
         (stdout nil)
         (expect-detail nil)
         (nelisp-artifact-profile-load nil)
         (nelisp-artifact-profile-load-detail nil))
    (unwind-protect
        (progn
          (with-temp-file source-path
            (dotimes (_ 100)
              (insert "nil\n")))
          (write-region "runtime" nil runtime-path nil 'silent)
          (nelisp-artifact-compile-file
           source-path artifact-path nil nil nil nil nil 'neln)
          (setq nelisp-artifact--loaded nil)
          (should
           (plist-get
            (nelisp-artifact--parse-flat-image-cache-args
             (list "compile-runtime-image"
                   "--flat-artifact-cache"
                   "--runtime" runtime-path
                   "--input" artifact-path
                   "--output" image-path
                   "--profile-load"))
            :profile-load))
          (should-not
           (plist-get
            (nelisp-artifact--parse-flat-image-cache-args
             (list "compile-runtime-image"
                   "--flat-artifact-cache"
                   "--runtime" runtime-path
                   "--input" artifact-path
                   "--output" image-path))
            :profile-load))
          (let ((detail
                 (nelisp-artifact--parse-flat-image-cache-args
                  (list "compile-runtime-image"
                        "--flat-artifact-cache"
                        "--runtime" runtime-path
                        "--input" artifact-path
                        "--output" image-path
                        "--profile-load-detail"))))
            (should (plist-get detail :profile-load))
            (should (plist-get detail :profile-load-detail)))
          (cl-letf
              (((symbol-function 'nelisp-artifact-prepare-flat-image-cache)
                (lambda (artifact image runtime)
                  (should nelisp-artifact-profile-load)
                  (should
                   (eq (and nelisp-artifact-profile-load-detail t)
                       expect-detail))
                  (should (equal runtime runtime-path))
                  (nelisp-artifact-load-file artifact)
                  (list :status 'rebuilt :artifact artifact :image image
                        :runtime runtime)))
               ((symbol-function 'nelisp-artifact--write-stderr)
                (lambda (text)
                  (setq stderr (concat (or stderr "") text "\n"))))
               ((symbol-function 'nelisp-artifact--write-stdout)
                (lambda (text)
                  (setq stdout (concat (or stdout "") text)))))
            (should
             (= 0
                (compile-runtime-image
                 (list "compile-runtime-image"
                       "--flat-artifact-cache"
                       "--runtime" runtime-path
                       "--input" artifact-path
                       "--output" image-path
                       "--profile-load"))))
          (should-not nelisp-artifact-profile-load)
          (should (string-match-p
                   "artifact_load_profile stage=native-total"
                   stderr))
          (should (string-match-p
                   "artifact_load_profile stage=module-total"
                   stderr))
          (should (string-match-p
                   "artifact_load_profile stage=load-total"
                   stderr))
          (should-not
           (string-match-p "artifact_load_profile stage=read-artifact"
                           stderr))
          (should-not
           (string-match-p "artifact_load_profile progress=module-item"
                           stderr))
          (setq stderr nil)
          (setq nelisp-artifact--loaded nil)
          (setq expect-detail t)
          (should
           (= 0
              (compile-runtime-image
               (list "compile-runtime-image"
                     "--flat-artifact-cache"
                     "--runtime" runtime-path
                     "--input" artifact-path
                     "--output" image-path
                     "--profile-load-detail"))))
          (should
           (string-match-p
            "artifact_load_profile progress=module-item count=100"
            stderr))
          (should
           (string-match-p "artifact_load_profile stage=read-artifact"
                           stderr))
          (should (string-match-p "flat-image-cache=rebuilt" stdout))))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/flat-image-header-requires-exact-stream-length ()
  "Flat image validation checks magic, roots, and the encoded exact length."
  (let* ((temp-dir (make-temp-file "nelisp-flat-header-" t))
         (image-path (expand-file-name "small.flat.nlri" temp-dir))
         (u64le
          (lambda (value)
            (let ((bytes nil)
                  (n value))
              (dotimes (_ 8)
                (setq bytes (cons (logand n 255) bytes))
                (setq n (lsh n -8)))
              (apply #'unibyte-string (nreverse bytes)))))
         (header
          (apply #'concat
                 (mapcar u64le
                         (list 1179407692 8 0 1 0 0 0 0))))
         ;; 8-byte relocation table + 8-byte arena region.
         (image (concat header (make-string 16 0))))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'no-conversion))
            (write-region image nil image-path nil 'silent))
          (cl-letf (((symbol-function 'call-process)
                     (lambda (&rest _)
                       (error "header validation must not spawn a process"))))
            (let ((parsed (nelisp-artifact--flat-image-header image-path)))
              (should (= (plist-get parsed :expected-size) 80))
              (should (= (plist-get parsed :tlen) 1))))
          (let ((coding-system-for-write 'no-conversion))
            (write-region (substring image 0 79)
                          nil image-path nil 'silent))
          (should-error (nelisp-artifact--flat-image-header image-path)))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/flat-image-validation-skips-native-metadata ()
  "Warm flat-image validation verifies identity without native section reads."
  (let* ((temp-dir (make-temp-file "nelisp-flat-validate-" t))
         (artifact-path (expand-file-name "small.neln" temp-dir))
         (keys-seen nil)
         (manifest
          (list :format nelisp-artifact--manifest-format
                :kind 'neln
                :artifact-format nelisp-artifact--format
                :artifact-class nelisp-artifact--native-class
                :runtime-abi nelisp-artifact--native-runtime-abi
                :artifact-sha256 "artifact-sha"
                :artifact-size 8
                :nelisp-version "unknown"
                :compiler (nelisp-artifact--compiler-plist)
                :source nil :runtime-image nil :preloads nil)))
    (unwind-protect
        (progn
          (write-region "artifact" nil artifact-path nil 'silent)
          (cl-letf (((symbol-function 'nelisp-artifact--read-manifest-fast)
                     (lambda (_artifact keys)
                       (setq keys-seen keys)
                       manifest))
                    ((symbol-function 'nelisp-artifact--sha256-file)
                     (lambda (_path) "artifact-sha")))
            (should (eq
                     (plist-get
                      (nelisp-artifact--validate-flat-image-artifact
                       artifact-path)
                      :kind)
                     'neln)))
          (should-not (memq :native keys-seen))
          (should-not (memq :native-sections keys-seen))
          (should-not (memq :native-report keys-seen)))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/runtime-image-eval-cli-cache-kind-refreshes ()
  "`eval-runtime-image --cache-kind' loads a refreshed artifact cache."
  (let* ((temp-dir (make-temp-file "nelisp-runtime-image-cache-cli-" t))
         (image-path (expand-file-name "runtime.nlri" temp-dir))
         (artifact-path (concat image-path ".nelc")))
    (unwind-protect
        (progn
          (write-region
           ";;; nelisp-runtime-image source-v1
(progn
(defvar rt-cli-cache-base 40)
(defun rt-cli-cache-hot (x) (+ x rt-cli-cache-base))
(provide 'rt-cli-cache)
)
"
           nil image-path nil 'silent)
          (nelisp--reset)
          (setq nelisp-artifact--loaded nil)
          (should (= 0 (nelisp-runtime-image-eval-cli
                        (list "exec-runtime-image" image-path
                              "--cache-kind" "nelc"
                              "(setq rt-cli-cache-result (rt-cli-cache-hot 2))")
                        nil)))
          (should (file-exists-p artifact-path))
          (should (= (nelisp-eval 'rt-cli-cache-result) 42))
          (write-region
           ";;; nelisp-runtime-image source-v1
(progn
(defvar rt-cli-cache-base 1000)
(defun rt-cli-cache-hot (x) (+ x rt-cli-cache-base))
(provide 'rt-cli-cache)
)
"
           nil image-path nil 'silent)
          (nelisp--reset)
          (setq nelisp-artifact--loaded nil)
          (should (= 0 (nelisp-runtime-image-eval-cli
                        (list "exec-runtime-image" image-path
                              "--cache-kind" "nelc"
                              "(setq rt-cli-cache-result (rt-cli-cache-hot 2))")
                        nil)))
          (should (= (nelisp-eval 'rt-cli-cache-result) 1002)))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/runtime-image-eval-cli-neln-cache-kind ()
  "`eval-runtime-image --cache-kind neln' can run through a native cache."
  (skip-unless (memq system-type '(gnu/linux berkeley-unix)))
  (skip-unless (and (executable-find "cc") (executable-find "objcopy")))
  (let* ((temp-dir (make-temp-file "nelisp-runtime-image-cache-neln-cli-" t))
         (image-path (expand-file-name "runtime.nlri" temp-dir))
         (artifact-path (concat image-path ".neln")))
    (unwind-protect
        (progn
          (write-region
           ";;; nelisp-runtime-image source-v1
(progn
(defun rt-cli-cache-native-hot (x) (+ x 1))
(provide 'rt-cli-cache-native)
)
"
           nil image-path nil 'silent)
          (nelisp--reset)
          (setq nelisp-artifact--loaded nil)
          (should (= 0 (nelisp-runtime-image-eval-cli
                        (list "exec-runtime-image" image-path
                              "--cache-kind" "neln"
                              "(setq rt-cli-cache-native-result (rt-cli-cache-native-hot 41))")
                        nil)))
          (should (file-exists-p artifact-path))
          (should (= (nelisp-eval 'rt-cli-cache-native-result) 42)))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/runtime-image-compile-neln-native-hot-defun ()
  "A runtime image can produce a `.neln' native artifact for hot defuns."
  (skip-unless (memq system-type '(gnu/linux berkeley-unix)))
  (skip-unless (and (executable-find "cc") (executable-find "objcopy")))
  (let* ((temp-dir (make-temp-file "nelisp-runtime-image-neln-" t))
         (image-path (expand-file-name "runtime.nlri" temp-dir))
         (artifact-path (expand-file-name "runtime.neln" temp-dir)))
    (unwind-protect
        (progn
          (write-region
           ";;; nelisp-runtime-image source-v1
(progn
(defun rt-native-sq (x) (* x x))
(provide 'rt-native)
)
"
           nil image-path nil 'silent)
          (should (= 0 (compile-runtime-image
                        (list "compile-runtime-image" "--kind" "neln"
                              "--input" image-path "--output" artifact-path))))
          (should (= 81 (nelisp-artifact-native-exec
                         artifact-path "rt-native-sq" '(9)))))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/runtime-image-compile-wasm32-wasi-routes-to-wasm-object ()
  "`compile-runtime-image --kind auto --target wasm32-wasi' reaches the wasm lane."
  (let* ((temp-dir (make-temp-file "nelisp-runtime-image-wasm-" t))
         (image-path (expand-file-name "runtime.nlri" temp-dir))
         (artifact-path (expand-file-name "runtime-image.wasm" temp-dir))
         (captured nil))
    (unwind-protect
        (progn
          (write-region
           ";;; nelisp-runtime-image source-v1
(progn
(defun boot-hot () 99)
(provide 'rt-wasm)
)
"
           nil image-path nil 'silent)
          (cl-letf (((symbol-function 'nelisp-artifact--ensure-native-compiler)
                     (lambda () t))
                    ((symbol-function 'nelisp-aot-compile-to-object)
                     (lambda (sexp out-path &rest keys)
                       (setq captured (list :sexp sexp
                                            :out-path out-path
                                            :keys keys))
                       (write-region "wasm" nil out-path nil 'silent)
                       out-path)))
            (should (= 0 (compile-runtime-image
                          (list "compile-runtime-image" "--kind" "auto"
                                "--target" "wasm32-wasi"
                                "--input" image-path "--output" artifact-path))))
            (should (equal (plist-get captured :sexp)
                           '(seq
                             (defun boot-hot nil 99)
                             (provide 'rt-wasm))))
            (should (equal (plist-get captured :out-path) artifact-path))
            (should (equal (plist-get captured :keys)
                           '(:arch wasm32 :format wasm)))
            (should (nelisp-artifact--runtime-image-wasm-target-p
                     "wasm32-wasi"))
            (should (equal (plist-get
                            (nelisp-artifact--parse-compile-runtime-image-args
                             (list "compile-runtime-image" "--kind" "wasm"
                                   "--target" "wasm32-wasi"
                                   "--input" image-path "--output" artifact-path))
                            :kind)
                           "wasm"))
            (should (file-exists-p artifact-path))
            (should-not
             (file-exists-p
              (nelisp-artifact--sibling-manifest-path artifact-path)))))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/runtime-image-load-neln-installs-native-wrapper ()
  "Loading a `.neln' runtime-image cache installs native wrappers.
Normal NeLisp calls then prefer the native artifact and keep the bytecode
fallback inside the wrapper."
  (skip-unless (memq system-type '(gnu/linux berkeley-unix)))
  (skip-unless (and (executable-find "cc") (executable-find "objcopy")))
  (let* ((temp-dir (make-temp-file "nelisp-runtime-image-neln-load-" t))
         (image-path (expand-file-name "runtime.nlri" temp-dir))
         (artifact-path (expand-file-name "runtime.neln" temp-dir)))
    (unwind-protect
        (progn
          (write-region
           ";;; nelisp-runtime-image source-v1
(progn
(defun rt-native-load-sq (x) (* x x))
(provide 'rt-native-load)
)
"
           nil image-path nil 'silent)
          (should (= 0 (compile-runtime-image
                        (list "compile-runtime-image" "--kind" "neln"
                              "--input" image-path "--output" artifact-path))))
          (nelisp--reset)
          (setq nelisp-artifact--loaded nil)
          (setq nelisp-artifact-native-dispatch-report nil)
          (nelisp-artifact-load-file artifact-path)
          (let ((fn (gethash 'rt-native-load-sq nelisp--functions)))
            (if (fboundp 'nelisp--native-call-boundary)
                (progn
                  (should (consp fn))
                  (should (eq (car fn) 'nelisp-native-function))
                  (should (eq (nth 2 fn) 'rt-native-load-sq)))
              ;; See the note in the source-loader test: the registered entry
              ;; is now the natively defined function, not the bytecode closure.
              (should (functionp fn))))
          (should (= (nelisp-eval '(rt-native-load-sq 9)) 81))
          (should (= (nelisp-eval '(rt-native-load-sq 10)) 100))
          (let ((report (nelisp-artifact-native-dispatch-report)))
            (if (fboundp 'nelisp--native-call-boundary)
                (progn
                  (should
                   (cl-some (lambda (entry)
                              (and (eq (plist-get entry :event) 'install)
                                   (= (plist-get entry :installed) 1)))
                            report))
                  (should
                   (cl-some (lambda (entry)
                              (and (eq (plist-get entry :event) 'call)
                                   (eq (plist-get entry :symbol)
                                       'rt-native-load-sq)
                                   (eq (plist-get entry :mode) 'native)))
                            report))
                  (should-not
                   (cl-some (lambda (entry)
                              (and (eq (plist-get entry :event) 'call)
                                   (eq (plist-get entry :symbol)
                                       'rt-native-load-sq)
                                   (eq (plist-get entry :mode) 'fallback)))
                            report)))
              (should-not report))))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/gate-6-4-neln-native-object-executes ()
  "Doc 142 §6.4 native EXEC: the native object embedded in a `.neln'
actually executes and returns the correct result — end-to-end elisp ->
AOT native .o -> embed -> extract -> link -> run.  Covers the
reloc-free leaf arithmetic subset (plain C integer ABI).  Skipped
without a host C toolchain."
  ;; The embedded object is a System V x86_64 ELF relocatable — only an
  ;; ELF host can link + exec it.  The Windows CI runner DOES expose
  ;; `cc'/`objcopy' (mingw), so the toolchain gate alone is not enough
  ;; there: the link/exec step misbehaves instead of skipping.
  (skip-unless (memq system-type '(gnu/linux berkeley-unix)))
  (skip-unless (and (executable-find "cc") (executable-find "objcopy")))
  (let* ((temp-dir (make-temp-file "nelisp-artifact-nx-" t))
         (source-path (expand-file-name "m.el" temp-dir))
         (artifact-path (concat source-path ".neln")))
    (unwind-protect
        (progn
          (write-region
           "(defun nat-nx-sq (x) (* x x))
(defun nat-nx-li (x) (let ((y (* x 2))) (if (> y 0) (+ y 1) (- y 1))))
(defun nat-nx-2 (a b) (+ (* a a) (* b b)))
(provide 'nat-nx)\n"
           nil source-path nil 'silent)
          (nelisp-artifact-compile-file
           source-path artifact-path nil nil nil nil nil 'neln)
          ;; the embedded native code runs and is correct — not just leaf
          ;; arithmetic but the reloc-free subset (let/if/compare, >1 arg)
          (should (= (nelisp-artifact-native-exec artifact-path "nat-nx-sq" '(9)) 81))
          (should (= (nelisp-artifact-native-exec artifact-path "nat-nx-sq" '(12)) 144))
          (should (= (nelisp-artifact-native-exec artifact-path "nat-nx-li" '(5)) 11))
          (should (= (nelisp-artifact-native-exec artifact-path "nat-nx-2" '(3 4)) 25))
          ;; same module still loads + runs via the portable bytecode lane
          (rename-file source-path (concat source-path ".gone") t)
          (nelisp--reset)
          (setq nelisp-artifact--loaded nil)
          (nelisp-artifact-load-file artifact-path)
          (should (= (nelisp-eval '(nat-nx-sq 9)) 81)))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/gate-6-4-neln-native-object-stays-in-memory-when-read-binary-breaks ()
  "Doc 142 §6.4 regression: native metadata is built from in-memory ET_REL
bytes, so a broken `nelisp-artifact--read-binary' must not corrupt the
embedded object.  The decoded object should still parse as ELF and expose
the compiled symbol through the existing ELF reader."
  (skip-unless (memq system-type '(gnu/linux berkeley-unix)))
  (skip-unless (and (executable-find "cc") (executable-find "objcopy")))
  (let* ((temp-dir (make-temp-file "nelisp-artifact-neln-rb-" t))
         (source-path (expand-file-name "m.el" temp-dir))
         (artifact-path (concat source-path ".neln"))
         (source
          "(defun nat-neln-rb-sq (x) (* x x))
(provide 'nat-neln-rb)\n"))
    (unwind-protect
        (progn
          (write-region source nil source-path nil 'silent)
          (should (nelisp-artifact--ensure-native-compiler))
          (let* ((forms (nelisp-artifact--read-top-level-forms
                         source source-path))
                 (unit (nelisp-aot-compile-to-link-unit
                        (cons 'seq
                              (nelisp-artifact--native-defun-forms forms))
                        :arch 'x86_64 :format 'elf))
                 (native
                  (cl-letf (((symbol-function 'nelisp-artifact--read-binary)
                             (lambda (&rest _)
                               (error "read-binary should not be used for native embedding"))))
                    (nelisp-artifact--native-section-plist
                     nil unit 'x86_64 '("nat-neln-rb-sq")
                     '((:name "nat-neln-rb-sq" :native t)))))
                 (obj (base64-decode-string
                       (plist-get native :object-base64)))
                 (obj-path (expand-file-name "native.o" temp-dir))
                 (symbol (car (plist-get native :symbols))))
            (should (string-prefix-p "\177ELF" obj))
            (should (= (length obj) (plist-get native :object-size)))
            (let ((coding-system-for-write 'binary))
              (write-region obj nil obj-path nil 'silent))
            (let ((desc (nelisp-elf-read-symbol obj-path symbol)))
              (should (equal (plist-get desc :section-name) ".text"))
              (should (> (plist-get desc :size) 0))
              (should (integerp (plist-get desc :value))))))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/gate-9-elc-loads-in-gnu-emacs ()
  "Doc 142 §6.2 / gate 9: --kind elc emits a GENUINE GNU Emacs-readable
`.elc' (the `;ELC' magic, produced by the real Emacs byte-compiler in a
clean subprocess) that a fresh `emacs -Q' — with no NeLisp loaded — can
`load' and run.  Also loadable through `nelisp-artifact-load-file'."
  (let* ((temp-dir (make-temp-file "nelisp-artifact-elc-" t))
         (source-path (expand-file-name "m.el" temp-dir))
         (artifact-path (expand-file-name "m.elc" temp-dir))
         (manifest-path (concat artifact-path ".manifest.el"))
         (emacs (expand-file-name invocation-name invocation-directory)))
    (unwind-protect
        (progn
          (write-region
           ";;; -*- lexical-binding: t; -*-\n(defun elc-g9-sq (x) (* x x))\n(defvar elc-g9-v 5)\n(provide 'elc-g9)\n"
           nil source-path nil 'silent)
          (let ((m (nelisp-artifact-compile-elc-file source-path artifact-path)))
            (should (eq (plist-get m :kind) 'elc))
            (should (eq (plist-get m :artifact-format) 'emacs-elc))
            (should (file-exists-p manifest-path)))
          ;; genuine GNU Emacs `.elc' magic header
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally artifact-path)
            (should (string-prefix-p ";ELC" (buffer-string))))
          ;; a clean `emacs -Q' (no NeLisp) loads + runs it -> gate 9
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process
                          emacs nil t nil "-Q" "--batch"
                          "--eval" (format "(load %S nil t)" artifact-path)
                          "--eval" "(princ (list (elc-g9-sq 9) (featurep 'elc-g9)))")))))
            (should (string-match-p "(81 t)" out)))
          ;; nelisp-artifact-load-file dispatches .elc to host load
          (when (fboundp 'elc-g9-sq) (fmakunbound 'elc-g9-sq))
          (setq nelisp-artifact--loaded nil)
          (nelisp-artifact-load-file artifact-path)
          (should (= (funcall (symbol-function 'elc-g9-sq) 7) 49)))
      (when (fboundp 'elc-g9-sq) (fmakunbound 'elc-g9-sq))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/elc-rejects-stale-and-corrupt ()
  "Doc 142 §6.2 cache safety: a `.elc' artifact is rejected when its
source changes (stale) or the `.elc' bytes are tampered (integrity)."
  (let* ((temp-dir (make-temp-file "nelisp-artifact-elc-s-" t))
         (source-path (expand-file-name "m.el" temp-dir))
         (artifact-path (expand-file-name "m.elc" temp-dir)))
    (unwind-protect
        (progn
          (write-region ";;; -*- lexical-binding: t; -*-\n(defvar elc-s-v 1)\n"
                        nil source-path nil 'silent)
          (nelisp-artifact-compile-elc-file source-path artifact-path)
          ;; tamper the .elc bytes -> integrity reject
          (let ((coding-system-for-write 'binary))
            (write-region "junk" nil artifact-path t 'silent))
          (setq nelisp-artifact--loaded nil)
          (should-error (nelisp-artifact-load-file artifact-path)
                        :type 'nelisp-artifact-invalid)
          ;; recompile clean, then change source -> stale reject
          (delete-file artifact-path)
          (nelisp-artifact-compile-elc-file source-path artifact-path)
          (write-region ";;; -*- lexical-binding: t; -*-\n(defvar elc-s-v 999)\n"
                        nil source-path nil 'silent)
          (setq nelisp-artifact--loaded nil)
          (should-error (nelisp-artifact-load-file artifact-path)
                        :type 'nelisp-artifact-stale))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/neln-opportunistic-sharding-preserves-order ()
  "Budgeted sharding preserves section order and symbol coverage."
  (let* ((budget 2)
         (byte-budget 4096)
         (forms '((defun shard-a (x) (+ x 1))
                  (defun shard-b (x) (* x 2))
                  (defun shard-c (x) (- x 3))
                  (defun shard-d (x) (1+ x))
                  (defun shard-e (x) (1- x))))
         (call-count 0)
         (single-count 0)
         (native nil))
    (cl-letf (((symbol-function 'nelisp-artifact--native-compile-fast-batch-section)
               (lambda (chunk _arch &optional report-tail)
                 (setq call-count (1+ call-count))
                 (let ((symbols (mapcar (lambda (defun)
                                          (symbol-name (nth 1 defun)))
                                        chunk)))
                   (list :native-section-version
                         nelisp-artifact--native-section-version
                         :object-format nelisp-artifact--native-object-format
                         :arch "x86_64"
                         :symbols symbols
                         :object-size call-count
                         :object-base64 (format "batch-%d" call-count)
                         :object-sha256 (format "sha-%d" call-count)
                         :text-size call-count
                         :relocs nil
                         :extern-symbols nil
                         :compile-report
                         (append (mapcar (lambda (name)
                                           (list :name name :native t))
                                         symbols)
                                 report-tail)
                         :defuns (mapcar (lambda (name)
                                           (list :name name :entry))
                                         symbols))))))
      (cl-letf (((symbol-function 'nelisp-artifact--native-compile-single-defun-result)
                 (lambda (defun _arch)
                   (setq single-count (1+ single-count))
                   (let ((name (symbol-name (nth 1 defun))))
                     (list :sections
                           (list (list :native-section-version
                                       nelisp-artifact--native-section-version
                                       :object-format
                                       nelisp-artifact--native-object-format
                                       :arch "x86_64"
                                       :symbols (list name)
                                       :object-size single-count
                                       :object-base64 (format "single-%d"
                                                               single-count)
                                       :object-sha256 (format "sha-single-%d"
                                                               single-count)
                                       :text-size single-count
                                       :relocs nil
                                       :extern-symbols nil
                                       :compile-report (list (list :name name
                                                                   :native t))
                                       :defuns (list (list :name name
                                                           :entry))))
                           :report (list (list :name name :native t)))))))
        (let ((nelisp-artifact-default-native-defun-budget budget)
              (nelisp-artifact-default-native-section-byte-budget byte-budget))
          (setq native
                (nelisp-artifact--native-compile-shard-result forms 'x86_64)))
        (should (= (length (plist-get native :sections)) 3))
        (should (equal (mapcan (lambda (section)
                                 (plist-get section :symbols))
                               (plist-get native :sections))
                       '("shard-a" "shard-b" "shard-c" "shard-d" "shard-e")))
        (should (equal (plist-get native :report)
                       '((:name "shard-a" :native t)
                         (:name "shard-b" :native t)
                         (:name "shard-c" :native t)
                         (:name "shard-d" :native t)
                         (:name "shard-e" :native t))))
        (should (= call-count 2))
        (should (= single-count 1))))))

(ert-deftest nelisp-artifact/neln-opportunistic-sharding-bisects-oversized-batch ()
  "An oversized successful batch is recursively bisected by byte budget."
  (let* ((forms '((defun shard-byte-a (x) (+ x 1))
                  (defun shard-byte-b (x) (* x 2))
                  (defun shard-byte-c (x) (- x 3))
                  (defun shard-byte-d (x) (1+ x))))
         (batch-calls nil)
         (native nil))
    (cl-letf (((symbol-function 'nelisp-artifact--native-compile-fast-batch-section)
               (lambda (chunk _arch &optional report-tail)
                 (let ((symbols (mapcar (lambda (defun)
                                          (symbol-name (nth 1 defun)))
                                        chunk)))
                   (setq batch-calls (append batch-calls (list symbols)))
                   (list :native-section-version
                         nelisp-artifact--native-section-version
                         :object-format nelisp-artifact--native-object-format
                         :arch "x86_64"
                         :symbols symbols
                         :object-size (length symbols)
                         :object-base64 (format "batch-%s"
                                                (mapconcat #'identity
                                                           symbols ","))
                         :object-sha256 (format "sha-batch-%s"
                                                (mapconcat #'identity
                                                           symbols ","))
                         :text-size (length symbols)
                         :relocs nil
                         :extern-symbols nil
                         :compile-report
                         (append (mapcar (lambda (name)
                                           (list :name name :native t))
                                         symbols)
                                 report-tail)
                         :defuns (mapcar (lambda (name)
                                           (list :name name :entry))
                                         symbols)))))
              ((symbol-function 'nelisp-artifact--native-section-serialized-byte-size)
               (lambda (section)
                 (if (> (length (plist-get section :symbols)) 2) 500 120))))
      (let ((nelisp-artifact-default-native-defun-budget 8)
            (nelisp-artifact-default-native-section-byte-budget 256))
        (setq native
              (nelisp-artifact--native-compile-shard-result forms 'x86_64)))
      (should (equal batch-calls
                     '(("shard-byte-a" "shard-byte-b" "shard-byte-c" "shard-byte-d")
                       ("shard-byte-a" "shard-byte-b")
                       ("shard-byte-c" "shard-byte-d"))))
      (should (equal (mapcar (lambda (section)
                               (plist-get section :symbols))
                             (plist-get native :sections))
                     '(("shard-byte-a" "shard-byte-b")
                       ("shard-byte-c" "shard-byte-d"))))
      (should (equal (plist-get native :report)
                     '((:name "shard-byte-a" :native t)
                       (:name "shard-byte-b" :native t)
                       (:name "shard-byte-c" :native t)
                       (:name "shard-byte-d" :native t)))))))

(ert-deftest nelisp-artifact/neln-opportunistic-sharding-bisects-failed-root ()
  "A failed root batch is recursively bisected around the unsupported leaf."
  (let* ((forms '((defun shard-bisect-a (x) (+ x 1))
                  (defun shard-bisect-b (x) (* x 2))
                  (defun shard-bisect-c (x) (- x 3))
                  (defun shard-bisect-d (x) (1+ x))
                  (defun shard-bisect-e (x) (1- x))
                  (defun shard-bisect-f (x) (+ x 6))
                  (defun shard-bisect-g (x) (+ x 7))
                  (defun shard-bisect-h (x) (+ x 8))))
         (batch-calls nil)
         (single-calls nil)
         (native nil))
    (cl-letf (((symbol-function 'nelisp-artifact--native-compile-fast-batch-section)
               (lambda (chunk _arch &optional report-tail)
                 (let ((symbols (mapcar (lambda (defun)
                                          (symbol-name (nth 1 defun)))
                                        chunk)))
                   (setq batch-calls (append batch-calls (list symbols)))
                   (unless (member "shard-bisect-e" symbols)
                     (list :native-section-version
                           nelisp-artifact--native-section-version
                           :object-format nelisp-artifact--native-object-format
                           :arch "x86_64"
                           :symbols symbols
                           :object-size (length symbols)
                           :object-base64 (format "batch-%s"
                                                  (mapconcat #'identity
                                                             symbols ","))
                           :object-sha256 (format "sha-batch-%s"
                                                  (mapconcat #'identity
                                                             symbols ","))
                           :text-size (length symbols)
                           :relocs nil
                           :extern-symbols nil
                           :compile-report
                           (append (mapcar (lambda (name)
                                             (list :name name :native t))
                                           symbols)
                                   report-tail)
                           :defuns (mapcar (lambda (name)
                                             (list :name name :entry))
                                           symbols))))))
              ((symbol-function 'nelisp-artifact--native-compile-single-defun-result)
               (lambda (defun _arch)
                 (let ((name (symbol-name (nth 1 defun))))
                   (setq single-calls (append single-calls (list name)))
                   (if (string= name "shard-bisect-e")
                       (list :sections nil
                             :report (list (list :name name
                                                 :native nil
                                                 :reason "unsupported leaf")))
                     (list :sections
                           (list (list :native-section-version
                                       nelisp-artifact--native-section-version
                                       :object-format
                                       nelisp-artifact--native-object-format
                                       :arch "x86_64"
                                       :symbols (list name)
                                       :object-size 1
                                       :object-base64 (format "single-%s" name)
                                       :object-sha256 (format "sha-single-%s" name)
                                       :text-size 1
                                       :relocs nil
                                       :extern-symbols nil
                                       :compile-report (list (list :name name
                                                                   :native t))
                                       :defuns (list (list :name name
                                                           :entry))))
                           :report (list (list :name name :native t))))))))
      (let ((nelisp-artifact-default-native-defun-budget 8))
        (setq native
              (nelisp-artifact--native-compile-shard-result forms 'x86_64)))
      (should (equal batch-calls
                     '(("shard-bisect-a" "shard-bisect-b"
                        "shard-bisect-c" "shard-bisect-d"
                        "shard-bisect-e" "shard-bisect-f"
                        "shard-bisect-g" "shard-bisect-h")
                       ("shard-bisect-a" "shard-bisect-b"
                        "shard-bisect-c" "shard-bisect-d")
                       ("shard-bisect-e" "shard-bisect-f"
                        "shard-bisect-g" "shard-bisect-h")
                       ("shard-bisect-e" "shard-bisect-f")
                       ("shard-bisect-g" "shard-bisect-h"))))
      (should (equal single-calls '("shard-bisect-e" "shard-bisect-f")))
      (should (equal (mapcar (lambda (section)
                               (plist-get section :symbols))
                             (plist-get native :sections))
                     '(("shard-bisect-a" "shard-bisect-b"
                        "shard-bisect-c" "shard-bisect-d")
                       ("shard-bisect-f")
                       ("shard-bisect-g" "shard-bisect-h"))))
      (should (equal (plist-get native :report)
                     '((:name "shard-bisect-a" :native t)
                       (:name "shard-bisect-b" :native t)
                       (:name "shard-bisect-c" :native t)
                       (:name "shard-bisect-d" :native t)
                       (:name "shard-bisect-e"
                              :native nil
                              :reason "unsupported leaf")
                       (:name "shard-bisect-f" :native t)
                       (:name "shard-bisect-g" :native t)
                       (:name "shard-bisect-h" :native t)))))))

(ert-deftest nelisp-artifact/neln-opportunistic-sharding-oversized-singleton-falls-back ()
  "An oversized singleton returns no native section and a clear fallback reason."
  (let* ((forms '((defun shard-one-big (x) (+ x 1))))
         (native nil))
    (cl-letf (((symbol-function 'nelisp-artifact--native-compile-single-defun-result)
               (lambda (defun _arch)
                 (let ((name (symbol-name (nth 1 defun))))
                   (list :sections
                         (list (list :native-section-version
                                     nelisp-artifact--native-section-version
                                     :object-format
                                     nelisp-artifact--native-object-format
                                     :arch "x86_64"
                                     :symbols (list name)
                                     :object-size 1
                                     :object-base64 "single-big"
                                     :object-sha256 "sha-single-big"
                                     :text-size 1
                                     :relocs nil
                                     :extern-symbols nil
                                     :compile-report (list (list :name name
                                                                 :native t))
                                     :defuns (list (list :name name
                                                         :entry))))
                         :report (list (list :name name :native t))))))
              ((symbol-function 'nelisp-artifact--native-section-serialized-byte-size)
               (lambda (_section) 1024)))
      (let ((nelisp-artifact-default-native-defun-budget 8)
            (nelisp-artifact-default-native-section-byte-budget 256))
        (setq native
              (nelisp-artifact--native-compile-shard-result forms 'x86_64)))
      (should-not (plist-get native :sections))
      (should (equal (plist-get native :report)
                     '((:name "shard-one-big"
                        :native nil
                        :reason
                        "serialized native section exceeds standalone replay byte budget (1024 bytes > 256 bytes)")))))))

(ert-deftest nelisp-artifact/neln-opportunistic-sharding-root-success-stays-one-section ()
  "A successful root batch stays one section and never recurses."
  (let* ((forms '((defun shard-root-a (x) (+ x 1))
                  (defun shard-root-b (x) (* x 2))
                  (defun shard-root-c (x) (- x 3))
                  (defun shard-root-d (x) (1+ x))
                  (defun shard-root-e (x) (1- x))
                  (defun shard-root-f (x) (+ x 6))
                  (defun shard-root-g (x) (+ x 7))
                  (defun shard-root-h (x) (+ x 8))))
         (batch-calls nil)
         (single-calls nil)
         (native nil))
    (cl-letf (((symbol-function 'nelisp-artifact--native-compile-fast-batch-section)
               (lambda (chunk _arch &optional report-tail)
                 (let ((symbols (mapcar (lambda (defun)
                                          (symbol-name (nth 1 defun)))
                                        chunk)))
                   (setq batch-calls (append batch-calls (list symbols)))
                   (list :native-section-version
                         nelisp-artifact--native-section-version
                         :object-format nelisp-artifact--native-object-format
                         :arch "x86_64"
                         :symbols symbols
                         :object-size (length symbols)
                         :object-base64 "batch-success"
                         :object-sha256 "sha-batch-success"
                         :text-size (length symbols)
                         :relocs nil
                         :extern-symbols nil
                         :compile-report
                         (append (mapcar (lambda (name)
                                           (list :name name :native t))
                                         symbols)
                                 report-tail)
                         :defuns (mapcar (lambda (name)
                                           (list :name name :entry))
                                         symbols)))))
              ((symbol-function 'nelisp-artifact--native-compile-single-defun-result)
               (lambda (defun _arch)
                 (setq single-calls (append single-calls
                                            (list (symbol-name (nth 1 defun)))))
                 (error "single-defun fallback should not run on a successful root"))))
      (let ((nelisp-artifact-default-native-defun-budget 8))
        (setq native
              (nelisp-artifact--native-compile-shard-result forms 'x86_64)))
      (should (= (length (plist-get native :sections)) 1))
      (should (equal batch-calls
                     '(("shard-root-a" "shard-root-b"
                        "shard-root-c" "shard-root-d"
                        "shard-root-e" "shard-root-f"
                        "shard-root-g" "shard-root-h"))))
      (should-not single-calls)
      (should (equal (plist-get native :report)
                     '((:name "shard-root-a" :native t)
                       (:name "shard-root-b" :native t)
                       (:name "shard-root-c" :native t)
                       (:name "shard-root-d" :native t)
                       (:name "shard-root-e" :native t)
                       (:name "shard-root-f" :native t)
                       (:name "shard-root-g" :native t)
                       (:name "shard-root-h" :native t)))))))

(ert-deftest nelisp-artifact/neln-opportunistic-sharding-output-sections-stay-within-byte-budget ()
  "Final emitted sections never exceed the serialized byte budget."
  (should (nelisp-artifact--ensure-native-compiler))
  (let* ((forms '((defun shard-cap-a (x) (+ x 1))
                  (defun shard-cap-b (x) (* x 2))
                  (defun shard-cap-c (x) (- x 3))
                  (defun shard-cap-d (x) (1+ x))
                  (defun shard-cap-e (x) (1- x))))
         (native nil))
    (cl-letf (((symbol-function 'nelisp-artifact--native-compile-fast-batch-section)
               (lambda (chunk _arch &optional report-tail)
                 (let ((symbols (mapcar (lambda (defun)
                                          (symbol-name (nth 1 defun)))
                                        chunk)))
                   (list :native-section-version
                         nelisp-artifact--native-section-version
                         :object-format nelisp-artifact--native-object-format
                         :arch "x86_64"
                         :symbols symbols
                         :object-size (length symbols)
                         :object-base64 "budget-batch"
                         :object-sha256 "budget-sha"
                         :text-size (length symbols)
                         :relocs nil
                         :extern-symbols nil
                         :compile-report
                         (append (mapcar (lambda (name)
                                           (list :name name :native t))
                                         symbols)
                                 report-tail)
                         :defuns (mapcar (lambda (name)
                                           (list :name name :entry))
                                         symbols)))))
              ((symbol-function 'nelisp-artifact--native-section-serialized-byte-size)
               (lambda (section)
                 (* 100 (length (plist-get section :symbols))))))
      (let ((nelisp-artifact-default-native-defun-budget 8)
            (nelisp-artifact-default-native-section-byte-budget 250))
        (setq native
              (nelisp-artifact--native-compile-shard-result forms 'x86_64)))
      (should (equal (mapcar (lambda (section)
                               (length (plist-get section :symbols)))
                             (plist-get native :sections))
                     '(2 1 2)))
      (should (cl-every
               (lambda (section)
                 (<= (nelisp-artifact--native-section-serialized-byte-size
                      section)
                     nelisp-artifact-default-native-section-byte-budget))
               (plist-get native :sections))))))

(ert-deftest nelisp-artifact/neln-opportunistic-sharding-keeps-legacy-native ()
  "Two defuns still serialize as legacy `:native' with no `:native-sections'."
  (let* ((budget 2)
         (byte-budget 4096)
         (temp-dir (make-temp-file "nelisp-artifact-shard-small-" t))
         (source-path (expand-file-name "shard-small.el" temp-dir))
         (forms '((defun shard-small-a (x) (+ x 1))
                  (defun shard-small-b (x) (* x 2))))
         (native nil)
         (manifest nil))
    (unwind-protect
        (progn
          (write-region
           "(defun shard-small-a (x) (+ x 1))\n(defun shard-small-b (x) (* x 2))\n"
           nil source-path nil 'silent)
          (cl-letf (((symbol-function 'nelisp-artifact--native-compile-fast-batch-section)
                     (lambda (chunk _arch &optional report-tail)
                       (let ((symbols (mapcar (lambda (defun)
                                                (symbol-name (nth 1 defun)))
                                              chunk)))
                         (list :native-section-version
                               nelisp-artifact--native-section-version
                               :object-format nelisp-artifact--native-object-format
                         :arch "x86_64"
                         :symbols symbols
                         :object-size 1
                         :object-base64 "small-batch"
                         :object-sha256 "sha-small"
                         :text-size 1
                         :relocs nil
                         :extern-symbols nil
                               :compile-report
                               (append (mapcar (lambda (name)
                                                 (list :name name :native t))
                                               symbols)
                                       report-tail)
                               :defuns (mapcar (lambda (name)
                                                 (list :name name :entry))
                                               symbols))))))
            (let ((nelisp-artifact-default-native-defun-budget budget)
                  (nelisp-artifact-default-native-section-byte-budget byte-budget))
              (setq native
                    (nelisp-artifact--native-compile-shard-result forms 'x86_64)))
            (setq manifest
                  (nelisp-artifact--manifest-plist
                   source-path '(feature-a) 2 nil "abc123" 42
                   nil nil 'neln (car (plist-get native :sections))
                   nil 'required 'bytecode))
            (should (plist-get manifest :native))
            (should-not (plist-member manifest :native-sections))
            (should (equal (plist-get (plist-get manifest :native) :symbols)
                           '("shard-small-a" "shard-small-b")))))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/native-fixed-point-cross-section-stays-direct ()
  "An artifact candidate in another shard remains a direct native extern."
  (should (nelisp-artifact--ensure-native-compiler))
  (let* ((forms
          '((defun fp-cross-helper (x) (+ x 1))
            (defun fp-cross-caller (x) (fp-cross-helper x))))
         (nelisp-artifact-default-native-defun-budget 1)
         (nelisp-artifact-default-native-section-byte-budget 65536)
         (native
          (nelisp-artifact--native-compile-section
           forms nil 'opportunistic))
         (sections
          (nelisp-artifact--native-sections-from-native native))
         (caller
          (nelisp-artifact--native-section-for-symbol
           sections "fp-cross-caller")))
    (should (= (length sections) 2))
    (should
     (equal (plist-get caller :extern-symbols)
            '("fp-cross-helper")))
    (should
     (cl-every
      (lambda (entry) (plist-get entry :native))
      nelisp-artifact--last-native-compile-report))))

(ert-deftest nelisp-artifact/native-fixed-point-unknown-call-is-generic ()
  "An unknown Elisp call uses supported boxed calln runtime externs."
  (should (nelisp-artifact--ensure-native-compiler))
  (let* ((forms
          '((defun fp-generic-caller (x) (arbitrary-elisp x))))
         (native
          (nelisp-artifact--native-compile-section
           forms nil 'opportunistic)))
    (should native)
    (should
     (equal (plist-get native :extern-symbols)
            '("nelisp_aot_builtin_calln" "nl_alloc_symbol")))
    (should
     (equal nelisp-artifact--last-native-compile-report
            '((:name "fp-generic-caller" :native t))))))

(ert-deftest nelisp-artifact/native-vararg-bridge-overflow-falls-back-one-defun ()
  "Opportunistic AOT keeps safe defuns and bytecode-falls back bridge overflow."
  (should (nelisp-artifact--ensure-native-compiler))
  (let* ((forms
          '((defun bridge-safe (x) (+ x 1))
            (defun bridge-unsafe (fn)
              (funcall fn 0 1 2 3 4 5 6 7 8))))
         (native
          (nelisp-artifact--native-compile-section
           forms nil 'opportunistic))
         (report nelisp-artifact--last-native-compile-report)
         (unsafe (nth 1 report)))
    (should (equal (plist-get native :symbols)
                   '("bridge-safe")))
    (should (equal (car report)
                   '(:name "bridge-safe" :native t)))
    (should (equal (plist-get unsafe :name) "bridge-unsafe"))
    (should-not (plist-get unsafe :native))
    (should (string-match-p
             ":aot-vararg-bridge-too-many-args"
             (plist-get unsafe :reason)))))

(ert-deftest nelisp-artifact/native-fixed-point-runtime-errorn-spelling ()
  "The resolver's real errorn symbol is supported; the old typo is rejected."
  (should
   (null
    (nelisp-artifact--native-unsupported-section-externs
     '(:extern-symbols ("nelisp_aot_errorn")) nil)))
  (should
   (equal
    (nelisp-artifact--native-unsupported-section-externs
     '(:extern-symbols ("nelisp_aot_builtin_errorn")) nil)
    '("nelisp_aot_builtin_errorn"))))

(ert-deftest nelisp-artifact/runtime-extern-allowlist-matches-resolver ()
  "Artifact native externs match the fresh standalone resolver availability."
  (let ((resolver-available
         '("nl_alloc_symbol"
           "nl_alloc_str"
           "nl_alloc_mut_str"
           "nl_mut_str_push_byte"
           "nl_mut_str_finalize"
           "nl_vector_slot_ptr"
           "nl_val_clone_into"
           "nelisp_env_lookup_value"
           "nl_alloc_consbox"
           "nelisp_env_set_value"
           "nl_alloc_vector"
           "nl_alloc_bytes"
           "nl_vector_set_slot"
           "nelisp_aot_builtin_call1"
           "nelisp_aot_builtin_calln"
           "nelisp_aot_errorn"
           "nelisp_aot_funcall1"
           "nelisp_aot_funcall2"
           "nelisp_aot_funcall3"
           "nelisp_aot_funcalln"
           "nelisp_aot_apply"
           "nelisp_aot_applyn"
           "nelisp_aot_listn")))
    (should
     (equal
      (sort (copy-sequence nelisp-artifact--supported-runtime-externs)
            #'string-lessp)
      (sort resolver-available #'string-lessp)))))

(ert-deftest nelisp-artifact/native-call-bridges-remain-opportunistic-candidates ()
  "All seven implemented call bridges remain native artifact candidates."
  (should (nelisp-artifact--ensure-native-compiler))
  (let* ((forms
          '((defun bridge-f1 (fn x) (funcall fn x))
            (defun bridge-f2 (fn x y) (funcall fn x y))
            (defun bridge-f3 (fn x y z) (funcall fn x y z))
            (defun bridge-fn (fn a b c d) (funcall fn a b c d))
            (defun bridge-a (fn tail) (apply fn tail))
            (defun bridge-an (fn x tail) (apply fn x tail))
            (defun bridge-rest (a &rest rest) rest)
            (defun bridge-ln (x y) (bridge-rest x y))))
         (native
          (nelisp-artifact--native-compile-section
           forms nil 'opportunistic))
         (sections
          (nelisp-artifact--native-sections-from-native native))
         (externs
          (apply #'append
                 (mapcar (lambda (section)
                           (copy-sequence
                            (plist-get section :extern-symbols)))
                         sections))))
    (dolist (name '("nelisp_aot_funcall1"
                    "nelisp_aot_funcall2"
                    "nelisp_aot_funcall3"
                    "nelisp_aot_funcalln"
                    "nelisp_aot_apply"
                    "nelisp_aot_applyn"
                    "nelisp_aot_listn"))
      (should (member name externs)))
    (should
     (cl-every (lambda (entry) (plist-get entry :native))
               nelisp-artifact--last-native-compile-report))))

(ert-deftest nelisp-artifact/unimplemented-call-bridges-remain-rejected ()
  "The six known-but-unlinked standalone bridges stay outside the allowlist."
  (let ((unavailable
         '("nelisp_aot_make_closure"
           "nelisp_aot_pop_handler"
           "nelisp_aot_push_catch"
           "nelisp_aot_push_condition"
           "nelisp_aot_signal"
           "nelisp_aot_throw")))
    (should
     (equal
      (nelisp-artifact--native-unsupported-section-externs
       (list :extern-symbols unavailable) nil)
      unavailable))))

(ert-deftest nelisp-artifact/native-required-unknown-call-is-generic ()
  "Required policy lowers unknown Elisp calls through its supported boundary."
  (should (nelisp-artifact--ensure-native-compiler))
  (let* ((forms
          '((defun required-generic (x) (arbitrary-elisp x))))
         (native
          (nelisp-artifact--native-compile-section
           forms nil 'required)))
    (should native)
    (should
     (equal (plist-get native :extern-symbols)
            '("nelisp_aot_builtin_calln" "nl_alloc_symbol")))
    (should
     (equal nelisp-artifact--last-native-compile-report
            '((:name "required-generic" :native t))))))

(ert-deftest nelisp-artifact/native-required-raw-extern-fails-policy ()
  "Required policy rejects a raw extern outside native/runtime resolution."
  (should (nelisp-artifact--ensure-native-compiler))
  (let* ((forms
          '((defun required-raw (x) (extern-call raw_helper x))))
         (native
          (nelisp-artifact--native-compile-section
           forms nil 'required))
         (report nelisp-artifact--last-native-compile-report))
    (should-not native)
    (should
     (equal report
            '((:name "required-raw"
               :native nil
               :reason "unsupported-runtime-externs: raw_helper"))))
    (should-error
     (nelisp-artifact--enforce-native-policy
      "/tmp/required-raw.el" 'neln 'required report))))

(ert-deftest nelisp-artifact/native-fixed-point-unsupported-raw-is-bytecode ()
  "A singleton raw extern outside the resolver set stays bytecode-only."
  (should (nelisp-artifact--ensure-native-compiler))
  (let* ((forms
          '((defun fp-raw-only (x) (extern-call raw_helper x))))
         (native
          (nelisp-artifact--native-compile-section
           forms nil 'opportunistic)))
    (should-not native)
    (should
     (equal nelisp-artifact--last-native-compile-report
            '((:name "fp-raw-only"
               :native nil
               :reason "unsupported-runtime-externs: raw_helper"))))))

(ert-deftest nelisp-artifact/native-fixed-point-final-redefinition-can-fallback ()
  "An unsupported final redefinition prevents an earlier body becoming native."
  (should (nelisp-artifact--ensure-native-compiler))
  (let* ((forms
          '((defun fp-redefined (x) (+ x 1))
            (defun fp-redefined (x)
              (catch 'tag (+ x 2)))))
         (defuns
          (nelisp-artifact--native-last-defun-forms
           (nelisp-artifact--native-defun-forms forms)))
         (native
          (nelisp-artifact--native-compile-section
           forms nil 'opportunistic)))
    (should
     (equal defuns
            '((defun fp-redefined (x)
                (catch 'tag (+ x 2))))))
    (should-not native)
    (should
     (equal
      nelisp-artifact--last-native-compile-report
      '((:name "fp-redefined"
         :native nil
         :reason
         "unsupported-runtime-externs: nelisp_aot_pop_handler, nelisp_aot_push_catch"))))
    ;; With no native section/wrapper, normal replay keeps the last callable
    ;; definition rather than allowing the earlier native-capable body to win.
    (unwind-protect
        (progn
          (nelisp--reset)
          (dolist (form forms)
            (nelisp-eval form))
          (should (= (nelisp-eval '(fp-redefined 40)) 42)))
      (nelisp--reset))))

(ert-deftest nelisp-artifact/native-fixed-point-final-redefinition-only ()
  "Two native redefinitions emit only the final body and one report entry."
  (should (nelisp-artifact--ensure-native-compiler))
  (let* ((forms
          '((defun fp-redefined-native (x) (+ x 1))
            (defun fp-redefined-native (x) (+ x 2))))
         (last-only
          '((defun fp-redefined-native (x) (+ x 2))))
         (native
          (nelisp-artifact--native-compile-section
           forms nil 'opportunistic))
         (native-last
          (nelisp-artifact--native-compile-section
           last-only nil 'opportunistic)))
    (should
     (equal (plist-get native :symbols)
            '("fp-redefined-native")))
    (should (= (length (plist-get native :defuns)) 1))
    (should
     (equal (plist-get native :text-base64)
            (plist-get native-last :text-base64)))
    (should
     (equal nelisp-artifact--last-native-compile-report
            '((:name "fp-redefined-native" :native t))))))

(ert-deftest nelisp-artifact/native-fixed-point-removed-callee-becomes-generic ()
  "A caller surviving a removed native callee is recompiled through calln."
  (should (nelisp-artifact--ensure-native-compiler))
  (let* ((forms
          '((defun fp-removed-raw (x) (extern-call raw_helper x))
            (defun fp-removed-caller (x) (fp-removed-raw x))))
         (nelisp-artifact-default-native-defun-budget 1)
         (native
          (nelisp-artifact--native-compile-section
           forms nil 'opportunistic)))
    (should
     (equal (plist-get native :symbols)
            '("fp-removed-caller")))
    (should
     (equal (plist-get native :extern-symbols)
            '("nelisp_aot_builtin_calln" "nl_alloc_symbol")))
    (should
     (equal
      nelisp-artifact--last-native-compile-report
      '((:name "fp-removed-raw"
         :native nil
         :reason "unsupported-runtime-externs: raw_helper")
        (:name "fp-removed-caller" :native t))))))

(ert-deftest nelisp-artifact/native-fixed-point-is-stable ()
  "Repeated fixed-point compilation converges to the same native graph."
  (should (nelisp-artifact--ensure-native-compiler))
  (let* ((forms
          '((defun fp-stable-bad (x) (extern-call raw_helper x))
            (defun fp-stable-caller (x) (fp-stable-bad x))
            (defun fp-stable-leaf (x) (+ x 1))))
         (nelisp-artifact-default-native-defun-budget 1)
         (first
          (nelisp-artifact--native-compile-fixed-point-result
           forms 'x86_64))
         (second
          (nelisp-artifact--native-compile-fixed-point-result
           forms 'x86_64))
         (shape
          (lambda (result)
            (list
             (plist-get result :native-symbols)
             (plist-get result :iterations)
             (mapcar
              (lambda (section)
                (list (plist-get section :symbols)
                      (plist-get section :extern-symbols)))
              (plist-get result :sections))
             (plist-get result :report)))))
    (should (equal (funcall shape first) (funcall shape second)))
    (should (= (plist-get first :iterations) 2))
    (should
     (nelisp-artifact--native-final-externs-valid-p
      (plist-get first :sections)
      (plist-get first :native-symbols)))))

(ert-deftest nelisp-artifact/flat-image-finalizer-publishes-only-valid-image ()
  "The fresh finalizer validates before publishing image and sidecar."
  (let* ((temp-dir (make-temp-file "nelisp-flat-finalize-" t))
         (artifact (expand-file-name "small.neln" temp-dir))
         (runtime (expand-file-name "nelisp" temp-dir))
         (temp-image (expand-file-name "small.tmp" temp-dir))
         (image (expand-file-name "small.flat.nlri" temp-dir))
         (sidecar (concat image ".manifest.el"))
         (u64le
          (lambda (value)
            (let ((bytes nil)
                  (n value))
              (dotimes (_ 8)
                (setq bytes (cons (logand n 255) bytes))
                (setq n (lsh n -8)))
              (apply #'unibyte-string (nreverse bytes)))))
         (header
          (apply #'concat
                 (mapcar u64le
                         (list 1179407692 8 0 1 0 0 0 0))))
         (payload (concat header (make-string 16 0))))
    (unwind-protect
        (progn
          (write-region "artifact" nil artifact nil 'silent)
          (write-region "runtime" nil runtime nil 'silent)
          (let ((coding-system-for-write 'no-conversion))
            (write-region payload nil temp-image nil 'silent))
          (cl-letf (((symbol-function
                      'nelisp-artifact--validate-flat-image-artifact)
                     (lambda (_artifact) '(:validated t)))
                    ((symbol-function
                      'nelisp-artifact--flat-image-cache-record)
                     (lambda (&rest _)
                       '(:format nelisp-flat-image-cache-v1
                         :generation "test"))))
            (let ((result
                   (nelisp-artifact--finalize-flat-image-cache
                    artifact temp-image image runtime "token")))
              (should (eq (plist-get result :status) 'rebuilt))
              (should (file-exists-p image))
              (should-not (file-exists-p temp-image))
              (should (file-exists-p sidecar)))))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/flat-image-finalizer-rejects-before-publish ()
  "An invalid temporary image never becomes the public cache image."
  (let* ((temp-dir (make-temp-file "nelisp-flat-finalize-bad-" t))
         (artifact (expand-file-name "small.neln" temp-dir))
         (runtime (expand-file-name "nelisp" temp-dir))
         (temp-image (expand-file-name "small.tmp" temp-dir))
         (image (expand-file-name "small.flat.nlri" temp-dir)))
    (unwind-protect
        (progn
          (write-region (make-string 80 0) nil temp-image nil 'silent)
          (cl-letf (((symbol-function
                      'nelisp-artifact--validate-flat-image-artifact)
                     (lambda (_artifact) '(:validated t))))
            (should-error
             (nelisp-artifact--finalize-flat-image-cache
              artifact temp-image image runtime "token"))
            (should-not (file-exists-p image))
            (should-not
             (file-exists-p (concat image ".manifest.el")))))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/flat-image-finalizer-rejects-unrelated-image ()
  "A valid flat image without the expected embedded token is not published."
  (let* ((temp-dir (make-temp-file "nelisp-flat-finalize-token-" t))
         (artifact (expand-file-name "small.neln" temp-dir))
         (runtime (expand-file-name "nelisp" temp-dir))
         (temp-image (expand-file-name "unrelated.tmp" temp-dir))
         (image (expand-file-name "small.flat.nlri" temp-dir)))
    (unwind-protect
        (cl-letf (((symbol-function 'nl-syscall-read-file)
                   (lambda (&rest _) "x"))
                  ((symbol-function
                    'nelisp-artifact--validate-flat-image-artifact)
                   (lambda (_artifact) '(:validated t)))
                  ((symbol-function
                    'nelisp-artifact--validate-flat-image-header-via-od)
                   (lambda (_image)
                     '(:magic 1179407692 :expected-size 80)))
                  ((symbol-function 'call-process)
                   (lambda (&rest _) 1))
                  ((symbol-function 'nelisp-artifact--write-stdout)
                   (lambda (_text) nil)))
          (should-error
           (nelisp-artifact--finalize-flat-image-cache
            artifact temp-image image runtime "expected-token"))
          (should-not (file-exists-p image))
          (should-not (file-exists-p (concat image ".manifest.el"))))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/flat-image-finalizer-cli-dispatches ()
  "The private finalizer CLI parses all paths and dispatches exactly once."
  (let ((seen nil))
    (cl-letf (((symbol-function 'nelisp-artifact--finalize-flat-image-cache)
               (lambda (artifact temp image runtime generation)
                 (setq seen (list artifact temp image runtime generation))
                 (list :status 'rebuilt :image image)))
              ((symbol-function 'nelisp-artifact--write-stdout)
               (lambda (_text) nil)))
      (should
       (= (compile-runtime-image
           '("compile-runtime-image"
             "--flat-artifact-finalize"
             "--runtime" "/tmp/nelisp"
             "--input" "/tmp/a.neln"
             "--temp-image" "/tmp/a.tmp"
             "--generation" "token"
             "--output" "/tmp/a.flat.nlri"))
          0))
      (should (equal seen
                     '("/tmp/a.neln" "/tmp/a.tmp"
                       "/tmp/a.flat.nlri" "/tmp/nelisp" "token"))))))

(ert-deftest nelisp-artifact/flat-image-parent-propagates-finalizer-failure ()
  "A failed fresh finalizer cannot be reported as a successful rebuild."
  (let* ((temp-dir (make-temp-file "nelisp-flat-parent-fail-" t))
         (artifact (expand-file-name "small.neln" temp-dir))
         (runtime (expand-file-name "nelisp" temp-dir))
         (image (expand-file-name "small.flat.nlri" temp-dir)))
    (unwind-protect
        (progn
          (write-region "artifact" nil artifact nil 'silent)
          (write-region "runtime" nil runtime nil 'silent)
          (cl-letf (((symbol-function 'nl-syscall-read-file)
                     (lambda (&rest _) "x"))
                    ((symbol-function
                      'nelisp-artifact--flat-image-cache-sidecar-hit)
                     (lambda (&rest _) nil))
                    ((symbol-function
                      'nelisp-artifact--validate-flat-image-artifact)
                     (lambda (_artifact) '(:validated t)))
                    ((symbol-function 'nelisp-artifact-load-file)
                     (lambda (_artifact) t))
                    ((symbol-function 'nelisp--arena-dump-image-stream)
                     (lambda (path)
                       (write-region "dump" nil path nil 'silent)
                       4))
                    ((symbol-function 'call-process)
                     (lambda (&rest _) 1))
                    ((symbol-function 'nelisp-artifact--print-error)
                     (lambda (_text) nil)))
            (should
             (= (compile-runtime-image
                 (list "compile-runtime-image"
                       "--flat-artifact-cache"
                       "--runtime" runtime
                       "--input" artifact
                       "--output" image))
                1))
            (should-not (file-exists-p image))))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-artifact/standalone-native-linker-caches-section-once ()
  "The in-process linker maps one shared section once and calls boxed args."
  (let* ((artifact "/tmp/shared-native.neln")
         (section
          (list :arch "x86_64"
                :symbols '("shared-add")
                :text-base64 (base64-encode-string
                              (unibyte-string #x90 #xc3) t)
                :relocs nil :extern-symbols nil
                :defuns
                '((:name "shared-add" :offset 0 :body-offset 1
                   :arity 1 :rt-slot-count 17))))
         (meta (car (plist-get section :defuns)))
         (mmap-count 0)
         (munmap-count 0)
         (copy-count 0)
         (boundary-bodies nil)
         (nelisp-artifact--native-section-registry nil)
         (nelisp-artifact--native-runtime-mappings nil)
         (nelisp-artifact--native-artifact-linksets nil)
         (nelisp-artifact--native-artifact-symbol-index nil)
         (nelisp-artifact--native-link-diagnostics nil))
    (cl-letf (((symbol-function 'syscall-direct)
              (lambda (&rest syscall-args)
                 (cond
                  ((= (car syscall-args) 9)
                   (setq mmap-count (1+ mmap-count))
                   4096)
                  ((= (car syscall-args) 10) 0)
                  (t
                   (setq munmap-count (1+ munmap-count))
                   0))))
              ((symbol-function 'nelisp--ptr-copy-string-bytes)
               (lambda (_address string)
                 (setq copy-count (1+ copy-count))
                 (length string)))
              ((symbol-function 'ptr-write-u64)
               (lambda (&rest _) 0))
              ((symbol-function 'ptr-write-u32)
               (lambda (&rest _) 0))
              ((symbol-function 'nelisp--runtime-symbol-address)
               (lambda (_name) 8192))
              ((symbol-function 'nelisp--native-call-boundary)
               (lambda (body arity slots arg)
                 (setq boundary-bodies
                       (cons (list body arity slots arg) boundary-bodies))
                 42)))
      (nelisp-artifact--register-native-sections artifact (list section))
      (should (= (nelisp-artifact--native-call-in-process
                  artifact 'shared-add meta '(41))
                 42))
      (should (= (nelisp-artifact--native-call-in-process
                  artifact 'shared-add meta '(41))
                 42))
      (should (= mmap-count 1))
      (should (= copy-count 1))
      (should (equal (car boundary-bodies) '(4097 1 17 41)))
      (should
       (eq (plist-get
            (cdr (assoc (expand-file-name artifact)
                        nelisp-artifact--native-artifact-linksets))
            :state)
           'ready))
      (nelisp-artifact-clear-native-runtime-mappings)
      (should-not nelisp-artifact--native-runtime-mappings)
      (should-not nelisp-artifact--native-artifact-linksets)
      (should-not nelisp-artifact--native-artifact-symbol-index)
      (should (= munmap-count 1))
      ;; Serialized section metadata remains shared and dumpable.
      (should (eq (car (nelisp-artifact--registered-native-sections artifact))
                  section)))))

(ert-deftest nelisp-artifact/serialized-native-sections-reader-keeps-payload ()
  "The artifact reader retains top-level serialized section payload once."
  (let* ((section
          '(:arch "x86_64" :symbols ("one") :text-base64 "kMM="
            :relocs nil :extern-symbols nil
            :defuns ((:name "one" :offset 0 :body-offset 1
                      :arity 1 :rt-slot-count 17))))
         (content
          (concat nelisp-artifact--magic
                  (prin1-to-string
                   (list :format nelisp-artifact--format
                         :kind 'neln :module-init nil :features nil
                         :native-sections (list section)))
                  "\n")))
    (should
     (equal
      (nelisp-artifact--read-serialized-native-sections content "one.neln")
      (list section)))))

(ert-deftest nelisp-artifact/native-load-reader-materializes-compact-sections ()
  "Native load parsing skips diagnostic payloads and nested decoy keys."
  (let* ((large-data (make-string 200000 ?x))
         (section-string
          (lambda (name offset)
            (concat
             "(:native-section-version 2 :object-format elf-relocatable-v1 "
             ":arch \"x86_64\" :symbols (\"" name "\") "
             ":object-size 200000 :object-sha256 \"ignored\" "
             ":object-base64 \"" large-data "\" "
             ":text-size 1 :text-base64 \"ww==\" "
             ":relocs ((:offset 0 :type plt32 :symbol \"runtime\" :addend -4)) "
             ":extern-symbols (\"runtime\") "
             ":compile-report ((:arch \"decoy\" :symbols (\"decoy\") "
             ":text-base64 \"decoy\" :relocs nil "
             ":extern-symbols nil :defuns nil)) "
             ":defuns ((:name \"" name "\" :offset "
             (number-to-string offset)
             " :body-offset 0 :arity 1 :rt-slot-count 17)))")))
         (content
          (concat
           nelisp-artifact--magic
           "(:format nelisp-private-nelc-v2 :kind neln :module-init nil "
           ":native-sections ("
           (funcall section-string "dup" 0) " "
           (funcall section-string "dup" 8)
           ") :entry (:type module-init :id \"compact.el\"))\n"))
         (private-reader (symbol-function
                          'nelisp-artifact--read-private-item))
         (reader-spans nil))
    (cl-letf (((symbol-function 'nelisp-artifact--read-private-item)
               (lambda (source start end)
                 (setq reader-spans (cons (- end start) reader-spans))
                 (funcall private-reader source start end))))
      (let* ((sections
              (nelisp-artifact--read-serialized-native-sections-for-load
               content "compact.neln"))
             (winner
              (nelisp-artifact--native-section-index-for-symbol
               sections "dup"))
             (index-result
              (nelisp-artifact--native-build-symbol-index sections)))
        (should (= (length sections) 2))
        (dolist (section sections)
          (should (equal (plist-get section :arch) "x86_64"))
          (should (equal (plist-get section :symbols) '("dup")))
          (should (equal (plist-get section :text-base64) "ww=="))
          (should (= (length (plist-get section :relocs)) 1))
          (should (equal (plist-get section :extern-symbols) '("runtime")))
          (should (= (length (plist-get section :defuns)) 1))
          (should-not (plist-member section :object-base64))
          (should-not (plist-member section :compile-report)))
        ;; Five structured compact fields per section enter the reader; the two
        ;; generated string fields use bounded substrings.  Neither ignored
        ;; 200 KB object payload nor the nested decoy metadata is parsed.
        (should (= (length reader-spans) 10))
        (should (< (apply #'max reader-spans) 200))
        ;; Duplicate exports retain the linker's established last-wins rule.
        (should (= (car winner) 1))
        (should (= (plist-get (cdr (assoc "dup"
                                          (plist-get index-result :index)))
                              :section-index)
                   1))
        (should (= (length (plist-get index-result :duplicates)) 1))))))

(ert-deftest nelisp-artifact/native-load-reader-supports-legacy-native ()
  "Compact native load parsing accepts the legacy singular `:native' key."
  (let* ((section
          '(:native-section-version 2 :object-format elf-relocatable-v1
            :arch "x86_64" :symbols ("legacy")
            :object-size 1 :object-sha256 "ignored" :object-base64 "ww=="
            :text-size 1 :text-base64 "ww=="
            :relocs nil :extern-symbols nil :compile-report nil
            :defuns ((:name "legacy" :offset 0 :body-offset 0
                      :arity 1 :rt-slot-count 17))))
         (content
          (concat nelisp-artifact--magic
                  (prin1-to-string
                   (list :format nelisp-artifact--format
                         :kind 'neln :module-init nil :features nil
                         :native section))
                  "\n"))
         (compact
          (car
           (nelisp-artifact--read-serialized-native-sections-for-load
            content "legacy.neln"))))
    (should (equal (plist-get compact :arch) "x86_64"))
    (should (equal (plist-get compact :symbols) '("legacy")))
    (should (equal (plist-get compact :text-base64) "ww=="))
    (should (equal (plist-get compact :relocs) nil))
    (should (equal (plist-get compact :extern-symbols) nil))
    (should (equal (plist-get compact :defuns)
                   (plist-get section :defuns)))
    (should-not (plist-member compact :object-base64))
    (should-not (plist-member compact :compile-report))))

(ert-deftest nelisp-artifact/private-fast-load-registers-compact-native-fields ()
  "The fast load path registers compact metadata without using the full reader."
  (let* ((artifact-path "/tmp/compact-load.neln")
         (section
          '(:native-section-version 2 :object-format elf-relocatable-v1
            :arch "x86_64" :symbols ("compact-load")
            :object-size 1 :object-sha256 "ignored" :object-base64 "ww=="
            :text-size 1 :text-base64 "ww=="
            :relocs nil :extern-symbols nil :compile-report nil
            :defuns ((:name "compact-load" :offset 0 :body-offset 0
                      :arity 1 :rt-slot-count 17))))
         (content
          (concat nelisp-artifact--magic
                  (prin1-to-string
                   (list :format nelisp-artifact--format
                         :kind 'neln :module-init nil :features nil
                         :native-sections (list section)))
                  "\n"))
         (nelisp-artifact--native-section-registry nil)
         (nelisp-artifact-native-dispatch-enabled nil))
    (cl-letf (((symbol-function 'nelisp--native-call-boundary)
               (lambda (&rest _) 0))
              ((symbol-function 'nelisp-artifact--read-serialized-native-sections)
               (lambda (&rest _)
                 (error "full serialized native reader should not run")))
              ((symbol-function 'nelisp-artifact--replay-module-streaming)
               (lambda (&rest _) nil)))
      (nelisp-artifact--load-private-fast
       artifact-path content '(:kind neln))
      (let ((registered
             (car
              (nelisp-artifact--registered-native-sections artifact-path))))
        (should (equal (plist-get registered :symbols) '("compact-load")))
        (should (equal (plist-get registered :text-base64) "ww=="))
        (should-not (plist-member registered :object-base64))
        (should-not (plist-member registered :compile-report))))))

(ert-deftest nelisp-artifact/private-fast-load-trusts-artifact-not-manifest-native ()
  "Manifest-only native tampering cannot select installed dispatch wrappers."
  (let* ((artifact-path "/tmp/artifact-authoritative.neln")
         (section (nelisp-artifact-test--v4-section "ww==" nil))
         (payload
          (nelisp-artifact--artifact-payload
           "artifact-authoritative.el" nil nil 0 'neln section nil 'bytecode))
         (content (nelisp-artifact--artifact-string payload))
         (manifest
          '(:kind neln
            :native (:symbols ("manifest-tamper")
                     :defuns ((:name "manifest-tamper" :arity 9)))
            :native-sections
            ((:symbols ("manifest-shard-tamper")
              :defuns ((:name "manifest-shard-tamper" :arity 8))))))
         (installed nil)
         (nelisp-artifact--native-section-registry nil)
         (nelisp-artifact-native-dispatch-enabled t))
    (cl-letf
        (((symbol-function 'nelisp--native-call-boundary)
          (lambda (&rest _) 0))
         ((symbol-function 'nelisp-artifact--replay-module-streaming)
          (lambda (&rest _) nil))
         ((symbol-function 'nelisp-artifact--install-native-functions)
          (lambda (_path native)
            (setq installed native)))
         ((symbol-function 'nelisp-artifact--read-private-native-metadata)
          (lambda (&rest _)
            (error "manifest native metadata reader must not run"))))
      (nelisp-artifact--load-private-fast artifact-path content manifest))
    (should (= (length installed) 1))
    (should (equal (plist-get (car installed) :symbols) '("v4")))
    (should-not (member "manifest-tamper"
                        (plist-get (car installed) :symbols)))
    (should (equal
             (plist-get
              (car (nelisp-artifact--registered-native-sections artifact-path))
              :symbols)
             '("v4")))))

(ert-deftest nelisp-artifact/serialized-native-sections-ignore-module-key-text ()
  "Module strings and function bodies cannot masquerade as top-level keys."
  (let* ((section-a
          '(:arch "x86_64" :symbols ("one") :text-base64 "kMM="
            :relocs nil :extern-symbols nil
            :defuns ((:name "one" :offset 0 :body-offset 1
                      :arity 1 :rt-slot-count 17))))
         (section-b
          '(:arch "x86_64" :symbols ("two") :text-base64 "kMM="
            :relocs nil :extern-symbols nil
            :defuns ((:name "two" :offset 0 :body-offset 1
                      :arity 1 :rt-slot-count 17))))
         (payload
          (list
           :format nelisp-artifact--format
           :kind 'neln
           :module-init
           '((:eval-source
              "(defun key-text () \":native-sections fake\")")
             (:eval
              (defun key-body ()
                '(:native-sections nested-not-top-level))))
           :features nil
           :native-sections (list section-a section-b)
           :entry '(:type module-init :id "collision.el")))
         (content
          (concat nelisp-artifact--magic
                  (prin1-to-string payload)
                  "\n")))
    (cl-letf (((symbol-function 'nelisp-artifact--read-one-private-form)
               (lambda (&rest _)
                 (error "full private payload reader called"))))
      (should
       (equal
        (nelisp-artifact--read-serialized-native-sections
         content "collision.neln")
        (list section-a section-b))))))

(ert-deftest nelisp-artifact/serialized-native-fallback-ignores-nested-key ()
  "A nested `:native-sections' does not hide a top-level legacy `:native'."
  (let* ((section
          '(:arch "x86_64" :symbols ("legacy") :text-base64 "ww=="
            :relocs nil :extern-symbols nil
            :defuns ((:name "legacy" :offset 0 :body-offset 0
                      :arity 0 :rt-slot-count 17))))
         (content
          (concat
           nelisp-artifact--magic
           "(:format nelisp-private-nelc-v2"
           " :module-init ((:eval (quote (:native-sections nested))))"
           " ; :native-sections comment-decoy\n"
           " :features nil :native "
           (prin1-to-string section)
           " :entry (:type module-init :id \"legacy.el\"))\n")))
    (should
     (equal
      (nelisp-artifact--read-serialized-native-sections
       content "legacy.neln")
      (list section)))))

(ert-deftest nelisp-artifact/serialized-native-section-selection-is-last-wins ()
  "Full payload lookup selects the final shard exporting a duplicate symbol."
  (let* ((first
          '(:arch "arm64" :symbols ("duplicate")
            :object-base64 "Zmlyc3Q=" :extern-symbols ("first-extern")
            :defuns ((:name "duplicate" :offset 0 :body-offset 1
                      :arity 1 :param-class gp :rt-slot-count 1))))
         (last
          '(:arch "x86_64" :symbols ("duplicate")
            :object-base64 "bGFzdA==" :extern-symbols ("nl_alloc_symbol")
            :defuns ((:name "duplicate" :offset 2 :body-offset 7
                      :arity 1 :param-class gp :rt-slot-count 3))))
         (content
          (concat nelisp-artifact--magic
                  (prin1-to-string
                   (list :format nelisp-artifact--format
                         :kind 'neln :module-init nil :features nil
                         :native-sections (list first last)))
                  "\n"))
         (selected
          (nelisp-artifact--serialized-native-section-for-symbol
           "duplicate.neln" "duplicate" content))
         (object-path (make-temp-file "nelisp-last-wins-" nil ".o")))
    (unwind-protect
        (progn
          (should (equal selected last))
          (nelisp-artifact--write-native-object-file
           "duplicate.neln" object-path selected)
          (should (equal (nelisp-artifact--read-binary object-path)
                         "last")))
      (ignore-errors (delete-file object-path)))))

(ert-deftest nelisp-artifact/native-exec-general-uses-payload-arch ()
  "General native exec validates arch from full payload, not compressed manifest."
  (cl-letf (((symbol-function 'nelisp-artifact-read-manifest)
             (lambda (&rest _)
               (error "general build must not read native metadata from manifest")))
            ((symbol-function
              'nelisp-artifact--serialized-native-section-for-symbol)
             (lambda (_path _symbol &optional _content)
               '(:arch "arm64"
                 :symbols ("hot-fn")
                 :extern-symbols nil
                 :defuns
                 ((:name "hot-fn" :offset 0 :body-offset 1
                   :arity 1 :param-class gp :rt-slot-count 0))))))
    (let ((caught nil))
      (condition-case err
          (nelisp-artifact--native-exec-general-build
           "compressed.neln" "hot-fn" '(1)
           "/tmp/unused-native-exec" "cc" "objcopy")
        (error
         (setq caught err)))
      (should caught)
      (should (string-match-p
               "only supports x86_64"
               (error-message-string caught))))))

(ert-deftest nelisp-artifact/standalone-native-decode-prefers-raw-bytes ()
  "Standalone native text decoding must not UTF-8 re-encode machine bytes."
  (let ((raw (unibyte-string #x55 #x48 #x89 #xe5)))
    (cl-letf (((symbol-function 'nelisp--base64-decode-native)
               (lambda (_text-base64) raw))
              ((symbol-function 'base64-decode-string)
               (lambda (&rest _)
                 (error "host text decoder called"))))
      (let ((decoded (nelisp-artifact--native-decode-text "VUiJ5Q==")))
        (should (= (string-bytes decoded) 4))
        (should (equal (string-to-list decoded) '(#x55 #x48 #x89 #xe5)))))))

(ert-deftest nelisp-artifact/standalone-native-wrapper-reports-native-mode ()
  "A supported in-process call cannot enter a host-helper fallback."
  (let* ((meta '(:name "native-one" :offset 0 :body-offset 1
                 :arity 1 :rt-slot-count 17))
         (wrapper
          (nelisp-artifact--native-function-wrapper
           "/tmp/native-one.neln" 'native-one
           (lambda (&rest _) (error "bytecode fallback called"))
           meta))
         (nelisp-artifact-native-dispatch-enabled t)
         (nelisp-artifact-native-dispatch-report nil))
    (cl-letf (((symbol-function 'nelisp--native-call-boundary)
               (lambda (&rest _) 42))
              ((symbol-function 'nelisp-artifact--native-call-in-process)
               (lambda (_artifact _symbol _meta _args) 42))
              ((symbol-function 'nelisp-artifact-native-exec-fast-simple)
               (lambda (&rest _)
                 (error "external fast executor called")))
              ((symbol-function 'nelisp-artifact-native-exec-general)
               (lambda (&rest _)
                 (error "host helper called"))))
      (should (= (nelisp-native-function-call wrapper '(41)) 42))
      (let ((call (car (last (nelisp-artifact-native-dispatch-report)))))
        (should (eq (plist-get call :event) 'call))
        (should (eq (plist-get call :mode) 'native))))))

(ert-deftest nelisp-artifact/standalone-native-preflight-error-does-not-map ()
  "A resolver failure occurs before mmap and leaves no committed state."
  (let* ((section
          (list :arch "x86_64" :symbols '("bad")
                :text-base64
                (base64-encode-string (unibyte-string #x90 #xc3) t)
                :extern-symbols '("unsupported")
                :relocs
                '((:offset 0 :type plt32
                   :symbol "unsupported" :addend -4))
                :defuns
                '((:name "bad" :offset 0 :body-offset 1
                   :arity 1 :rt-slot-count 17))))
         (mmap-count 0)
         (munmap-count 0)
         (nelisp-artifact--native-section-registry nil)
         (nelisp-artifact--native-runtime-mappings nil)
         (nelisp-artifact--native-artifact-linksets nil)
         (nelisp-artifact--native-artifact-symbol-index nil)
         (nelisp-artifact--native-link-diagnostics nil))
    (cl-letf (((symbol-function 'syscall-direct)
               (lambda (number &rest _)
                 (cond
                  ((= number 9)
                   (setq mmap-count (1+ mmap-count))
                   4096)
                  ((= number 11)
                   (setq munmap-count (1+ munmap-count))
                   0)
                  (t 0))))
              ((symbol-function 'nelisp--ptr-copy-string-bytes)
               (lambda (_address string) (length string)))
              ((symbol-function 'nelisp--runtime-symbol-address)
               (lambda (_name) (error "unsupported extern")))
              ((symbol-function 'ptr-write-u64)
               (lambda (&rest _) 0))
              ((symbol-function 'ptr-write-u32)
               (lambda (&rest _) 0)))
      (should-error
       (nelisp-artifact--native-link-section
        "/tmp/bad.neln" 0 section))
      (should (= mmap-count 0))
      (should (= munmap-count 0))
      (should-not nelisp-artifact--native-runtime-mappings)
      (should-not nelisp-artifact--native-artifact-linksets)
      (should-not nelisp-artifact--native-artifact-symbol-index)
      (should
       (eq (plist-get (car nelisp-artifact--native-link-diagnostics)
                      :status)
           'unavailable)))))

(ert-deftest nelisp-artifact/standalone-native-artifact-link-is-transactional-rx ()
  "Two sections link together through local stubs and become RX before commit."
  (let* ((artifact "/tmp/two-section-native.neln")
         (caller
          (list :arch "x86_64" :symbols '("caller")
                :text-base64
                (base64-encode-string
                 (unibyte-string 0 0 0 0 #xc3) t)
                :extern-symbols '("callee")
                :relocs
                '((:offset 0 :type plt32 :symbol "callee" :addend -4))
                :defuns
                '((:name "caller" :offset 4 :body-offset 0
                   :arity 0 :rt-slot-count 17))))
         (callee
          (list :arch "x86_64" :symbols '("callee")
                :text-base64
                (base64-encode-string
                 (unibyte-string #x90 #xc3) t)
                :extern-symbols nil :relocs nil
                :defuns
                '((:name "callee" :offset 1 :body-offset 0
                   :arity 0 :rt-slot-count 17))))
         (next-base 0)
         (events nil)
         (gc-count 0)
         (resolver-count 0)
         (nelisp-artifact--native-section-registry nil)
         (nelisp-artifact--native-runtime-mappings nil)
         (nelisp-artifact--native-artifact-linksets nil)
         (nelisp-artifact--native-artifact-symbol-index nil)
         (nelisp-artifact--native-link-diagnostics nil))
    (cl-letf (((symbol-function 'syscall-direct)
               (lambda (number address size arg3 &rest _)
                 (cond
                  ((= number 9)
                   (setq next-base (+ next-base 4096))
                   (setq events
                         (cons (list 'mmap next-base size arg3) events))
                   next-base)
                  ((= number 10)
                   (setq events
                         (cons (list 'mprotect address size arg3) events))
                   0)
                  ((= number 11)
                   (setq events
                         (cons (list 'munmap address size) events))
                   0)
                  (t (error "unexpected syscall %S" number)))))
              ((symbol-function 'nelisp--ptr-copy-string-bytes)
               (lambda (address string)
                 ;; Host Emacs collects immediately here.  Standalone
                 ;; `garbage-collect' records a request and services it at the
                 ;; outer form boundary, so the transaction's frame bindings
                 ;; (`plan' and `owned') remain roots in either execution
                 ;; model.
                 (garbage-collect)
                 (setq gc-count (1+ gc-count))
                 (setq events
                       (cons (list 'copy address (string-bytes string))
                             events))
                 (string-bytes string)))
              ((symbol-function 'nelisp-artifact--native-write-jump-stub)
               (lambda (address target)
                 (setq events (cons (list 'stub address target) events))
                 address))
              ((symbol-function 'ptr-write-u32)
               (lambda (base offset value)
                 (setq events
                       (cons (list 'patch base offset value) events))
                 value))
              ((symbol-function 'nelisp--runtime-symbol-address)
               (lambda (_name)
                 (setq resolver-count (1+ resolver-count))
                 (error "cross-section symbol used runtime resolver"))))
      (nelisp-artifact--register-native-sections
       artifact (list caller callee))
      (let* ((linkset
              (nelisp-artifact--native-link-artifact artifact))
             (ordered (nreverse events))
             (index
              (cdr (assoc (expand-file-name artifact)
                          nelisp-artifact--native-artifact-symbol-index))))
        (should (eq (plist-get linkset :state) 'ready))
        (should (= resolver-count 0))
        (should (= gc-count 2))
        (should
         (equal
          (mapcar (lambda (event)
                    (list (car event)
                          (and (memq (car event) '(mmap mprotect))
                               (nth 3 event))))
                  ordered)
          '((mmap 3) (copy nil) (mmap 3) (copy nil)
            (stub nil) (patch nil) (mprotect 5) (mprotect 5))))
        (should (equal (nth 4 ordered) '(stub 4112 8193)))
        (should (equal (nth 5 ordered) '(patch 4096 0 12)))
        (should (= (plist-get (cdr (assoc "callee" index)) :address)
                   8193))
        (should (= (length nelisp-artifact--native-runtime-mappings) 2))))))

(ert-deftest nelisp-artifact/standalone-native-reregister-invalidates-one-generation ()
  "Changed sections invalidate only their artifact; equal sections preserve it."
  (let* ((artifact-a (expand-file-name "/tmp/generation-a.neln"))
         (artifact-b (expand-file-name "/tmp/generation-b.neln"))
         (old-section
          '((:arch "x86_64" :symbols ("same")
             :text-base64 "ww==" :relocs nil :extern-symbols nil
             :defuns ((:name "same" :offset 0 :body-offset 0
                       :arity 0 :rt-slot-count 17)))))
         (new-section
          '((:arch "x86_64" :symbols ("same")
             :text-base64 "kMM=" :relocs nil :extern-symbols nil
             :defuns ((:name "same" :offset 1 :body-offset 0
                       :arity 0 :rt-slot-count 17)))))
         (mapping-a '(:index 0 :base 4096 :size 4096))
         (mapping-b '(:index 0 :base 8192 :size 4096))
         (unmapped nil)
         (nelisp-artifact--native-section-registry
          (list (cons artifact-a old-section)
                (cons artifact-b old-section)))
         (nelisp-artifact--native-runtime-mappings
          (list
           (cons (nelisp-artifact--native-mapping-key artifact-a 0)
                 mapping-a)
           (cons (nelisp-artifact--native-mapping-key artifact-b 0)
                 mapping-b)))
         (nelisp-artifact--native-artifact-linksets
          (list (cons artifact-a '(:state ready))
                (cons artifact-b '(:state ready))))
         (nelisp-artifact--native-artifact-symbol-index
          (list (cons artifact-a '(("same" :address 4096)))
                (cons artifact-b '(("same" :address 8192))))))
    (cl-letf (((symbol-function 'syscall-direct)
               (lambda (number address size &rest _)
                 (when (= number 11)
                   (setq unmapped (cons (list address size) unmapped)))
                 0)))
      ;; Structural equality is the generation identity for the already
      ;; serialized sections; copying the list must not discard ready code.
      (nelisp-artifact--register-native-sections
       artifact-a (copy-tree old-section))
      (should-not unmapped)
      (should
       (nelisp-artifact--native-runtime-mapping artifact-a 0))
      (should (assoc artifact-a nelisp-artifact--native-artifact-linksets))
      ;; A changed generation unmaps A, while B remains ready and mapped.
      (nelisp-artifact--register-native-sections artifact-a new-section)
      (should (equal unmapped '((4096 4096))))
      (should-not
       (nelisp-artifact--native-runtime-mapping artifact-a 0))
      (should-not
       (assoc artifact-a nelisp-artifact--native-artifact-linksets))
      (should-not
       (assoc artifact-a nelisp-artifact--native-artifact-symbol-index))
      (should
       (eq (nelisp-artifact--native-runtime-mapping artifact-b 0)
           mapping-b))
      (should (assoc artifact-b nelisp-artifact--native-artifact-linksets))
      (should
       (equal (nelisp-artifact--registered-native-sections artifact-a)
              new-section)))))

(ert-deftest nelisp-artifact/standalone-native-duplicate-is-last-wins ()
  "Duplicate exports use the final section for wrapper metadata and lookup."
  (let* ((artifact "/tmp/duplicate-native.neln")
         (fallback (lambda (x) x))
         (first
          (list :arch "x86_64" :symbols '("duplicate")
                :text-base64
                (base64-encode-string (unibyte-string #x90 #xc3) t)
                :relocs nil :extern-symbols nil
                :defuns
                '((:name "duplicate" :offset 0 :body-offset 1
                   :arity 1 :rt-slot-count 17))))
         (last
          (list :arch "x86_64" :symbols '("duplicate")
                :text-base64
                (base64-encode-string
                 (unibyte-string #x90 #x90 #xc3) t)
                :relocs nil :extern-symbols nil
                :defuns
                '((:name "duplicate" :offset 2 :body-offset 7
                   :arity 1 :rt-slot-count 19))))
         (nelisp--functions (make-hash-table :test #'eq))
         (nelisp-artifact-native-dispatch-report nil)
         (nelisp-artifact--native-section-registry nil)
         (nelisp-artifact--native-runtime-mappings nil)
         (nelisp-artifact--native-artifact-linksets nil)
         (nelisp-artifact--native-artifact-symbol-index nil)
         (nelisp-artifact--native-link-diagnostics nil))
    (puthash 'duplicate fallback nelisp--functions)
    (nelisp-artifact--register-native-sections artifact (list first last))
    (cl-letf (((symbol-function 'nelisp--native-call-boundary)
               (lambda (&rest _) 0))
              ((symbol-function 'nelisp-artifact--install-function)
               (lambda (symbol function)
                 (puthash symbol function nelisp--functions)
                 symbol)))
      (should
       (= (nelisp-artifact--install-native-functions
           artifact (list first last))
          2))
      (let* ((wrapper (gethash 'duplicate nelisp--functions))
             (meta (nelisp-artifact--native-function-meta wrapper))
             (install
              (car (nelisp-artifact-native-dispatch-report)))
             (duplicate
              (car (plist-get install :duplicates)))
             (plan
              (nelisp-artifact--native-preflight-artifact
               artifact (list first last)))
             (winner
              (cdr (assoc "duplicate"
                          (plist-get plan :symbol-index)))))
        (should (nelisp-artifact--native-wrapper-p wrapper))
        (should (eq (nelisp-artifact--native-function-fallback wrapper)
                    fallback))
        (should (= (plist-get meta :body-offset) 7))
        (should (= (plist-get meta :rt-slot-count) 19))
        (should (= (plist-get winner :section-index) 1))
        (should (= (plist-get winner :offset) 2))
        (should
         (equal duplicate
                '(:symbol "duplicate"
                  :previous-section 0 :winner-section 1)))))))

(ert-deftest nelisp-artifact/standalone-native-rollback-unmaps-all-owned ()
  "A post-mmap PLT32 failure rolls back every section without publication."
  (let* ((artifact "/tmp/rollback-native.neln")
         (bad
          (list :arch "x86_64" :symbols '("bad")
                :text-base64
                (base64-encode-string
                 (unibyte-string 0 0 0 0 #xc3) t)
                :extern-symbols '("runtime")
                :relocs
                `((:offset 0 :type plt32 :symbol "runtime"
                           :addend ,(+ 2147483648 100)))
                :defuns
                '((:name "bad" :offset 4 :body-offset 0
                   :arity 0 :rt-slot-count 17))))
         (other
          (list :arch "x86_64" :symbols '("other")
                :text-base64
                (base64-encode-string (unibyte-string #xc3) t)
                :extern-symbols nil :relocs nil
                :defuns
                '((:name "other" :offset 0 :body-offset 0
                   :arity 0 :rt-slot-count 17))))
         (next-base 0)
         (mmap-count 0)
         (mprotect-count 0)
         (unmapped nil)
         (nelisp-artifact--native-section-registry nil)
         (nelisp-artifact--native-runtime-mappings nil)
         (nelisp-artifact--native-artifact-linksets nil)
         (nelisp-artifact--native-artifact-symbol-index nil)
         (nelisp-artifact--native-link-diagnostics nil))
    (cl-letf (((symbol-function 'syscall-direct)
               (lambda (number address size &rest _)
                 (cond
                  ((= number 9)
                   (setq mmap-count (1+ mmap-count))
                   (setq next-base (+ next-base 4096))
                   next-base)
                  ((= number 10)
                   (setq mprotect-count (1+ mprotect-count))
                   0)
                  ((= number 11)
                   (setq unmapped (cons (list address size) unmapped))
                   0)
                  (t 0))))
              ((symbol-function 'nelisp--ptr-copy-string-bytes)
               (lambda (_address string) (string-bytes string)))
              ((symbol-function 'nelisp-artifact--native-write-jump-stub)
               (lambda (address _target) address))
              ((symbol-function 'ptr-write-u32)
               (lambda (&rest _) 0))
              ((symbol-function 'nelisp--runtime-symbol-address)
               (lambda (_name) 16384)))
      (nelisp-artifact--register-native-sections
       artifact (list bad other))
      (should-error
       (nelisp-artifact--native-link-artifact artifact))
      (should (= mmap-count 2))
      (should (= mprotect-count 0))
      (should (= (length unmapped) 2))
      (should-not nelisp-artifact--native-runtime-mappings)
      (should-not nelisp-artifact--native-artifact-linksets)
      (should-not nelisp-artifact--native-artifact-symbol-index)
      (should
       (eq (plist-get (car nelisp-artifact--native-link-diagnostics)
                      :status)
           'unavailable)))))

(ert-deftest nelisp-artifact/standalone-native-unresolved-keeps-bytecode ()
  "An unresolved artifact preflight leaves every bytecode function installed."
  (let* ((artifact "/tmp/unresolved-bootstrap.neln")
         (fallback (lambda (x) (+ x 1)))
         (section
          (list :arch "x86_64" :symbols '("bootstrap")
                :text-base64
                (base64-encode-string
                 (unibyte-string 0 0 0 0 #xc3) t)
                :extern-symbols '("missing_runtime")
                :relocs
                '((:offset 0 :type plt32
                   :symbol "missing_runtime" :addend -4))
                :defuns
                '((:name "bootstrap" :offset 4 :body-offset 0
                   :arity 1 :rt-slot-count 17))))
         (mmap-count 0)
         (nelisp--functions (make-hash-table :test #'eq))
         (nelisp-artifact-native-dispatch-report nil)
         (nelisp-artifact--native-section-registry nil)
         (nelisp-artifact--native-runtime-mappings nil)
         (nelisp-artifact--native-artifact-linksets nil)
         (nelisp-artifact--native-artifact-symbol-index nil)
         (nelisp-artifact--native-link-diagnostics nil))
    (puthash 'bootstrap fallback nelisp--functions)
    ;; Model the real bootstrap shape: duplicate exports can coexist with an
    ;; unresolved runtime dependency, and their diagnostics must survive the
    ;; failed preflight.
    (nelisp-artifact--register-native-sections
     artifact (list section section))
    (cl-letf (((symbol-function 'nelisp--native-call-boundary)
               (lambda (&rest _) 0))
              ((symbol-function 'nelisp--runtime-symbol-address)
               (lambda (_name) (error "not exported")))
              ((symbol-function 'syscall-direct)
               (lambda (number &rest _)
                 (when (= number 9)
                   (setq mmap-count (1+ mmap-count)))
                 0))
              ((symbol-function 'nelisp-artifact--install-function)
               (lambda (symbol function)
                 (puthash symbol function nelisp--functions)
                 symbol)))
      (should
       (= (nelisp-artifact--install-native-functions
           artifact (list section section))
          0))
      (should (eq (gethash 'bootstrap nelisp--functions) fallback))
      (should (= mmap-count 0))
      (should-not nelisp-artifact--native-runtime-mappings)
      (should-not nelisp-artifact--native-artifact-linksets)
      (should-not nelisp-artifact--native-artifact-symbol-index)
      (let ((report (nelisp-artifact-native-dispatch-report)))
        (should (eq (plist-get (car report) :event) 'link-preflight))
        (should (eq (plist-get (car report) :status) 'unavailable))
        (should
         (equal
          (plist-get (car report) :duplicates)
          '((:symbol "bootstrap"
             :previous-section 0 :winner-section 1))))
        (should (= (plist-get (cadr report) :installed) 0))
        (should (= (plist-get (cadr report) :skipped) 2))
        (should-not (plist-get (cadr report) :native-ready))))))

(ert-deftest nelisp-artifact/compact-relocs-v3-shrink-synthetic-bootstrap-table ()
  "Indexed v3 relocation data removes repeated plist keys and symbol strings."
  (let* ((count 34115)
         (externs '("runtime-a" "runtime-b" "runtime-c"))
         (relocs nil))
    (dotimes (i count)
      (setq relocs
            (cons
             (list :offset (* i 4)
                   :type 'plt32
                   :symbol (nth (% i 3) externs)
                   :addend -4)
             relocs)))
    (setq relocs (nreverse relocs))
    (let* ((unit (list :extern-symbols externs :relocs relocs))
           (compact
            (nelisp-artifact--compact-runtime-relocs unit 'x86_64))
           (legacy-bytes (string-bytes (prin1-to-string relocs)))
           (compact-bytes
            (string-bytes
             (prin1-to-string (plist-get compact :reloc-data)))))
      (should (eq (plist-get compact :reloc-format)
                  'indexed-plt32-v1))
      (should (= (plist-get compact :reloc-count) count))
      (should (= (length (plist-get compact :reloc-data)) (* count 3)))
      (should (< compact-bytes (* legacy-bytes 0.35))))))

(ert-deftest nelisp-artifact/compact-relocs-v3-falls-back-outside-runtime-subset ()
  "Non-text, non-PLT32, ARM64, and unindexed symbols retain v2 metadata."
  (dolist
      (case
       (list
        (list 'x86_64
              '(:extern-symbols ("x")
                :relocs ((:offset 0 :type pc32 :symbol "x" :addend -4))))
        (list 'x86_64
              '(:extern-symbols ("x")
                :relocs ((:section data :offset 0 :type plt32
                          :symbol "x" :addend -4))))
        (list 'aarch64
              '(:extern-symbols ("x")
                :relocs ((:offset 0 :type b26-pc :symbol "x" :addend 0))))
        (list 'x86_64
              '(:extern-symbols nil
                :relocs ((:offset 0 :type plt32 :symbol "x" :addend -4))))))
    (should-not
     (nelisp-artifact--compact-runtime-relocs (cadr case) (car case)))))

(defun nelisp-artifact-test--v4-section (&optional object-base64 report)
  "Return a synthetic self-sized version 4 native section."
  (nelisp-artifact--native-section-finalize-char-size
   (list :native-section-version
         nelisp-artifact--legacy-self-sized-native-section-version
         :serialized-char-size 0
         :arch "x86_64"
         :symbols '("v4")
         :text-base64 "AAAAAA=="
         :reloc-format nelisp-artifact--compact-reloc-format
         :reloc-count 1
         :reloc-data '(0 0 -4)
         :extern-symbols '("runtime")
         :defuns
         '((:name "v4" :offset 4 :body-offset 0
            :arity 0 :rt-slot-count 17))
         :runtime-end t
         :object-format nelisp-artifact--native-object-format
         :object-size 1
         :object-sha256 "ignored"
         :object-base64 (or object-base64 "ww==")
         :text-size 4
         :compile-report report)))

(defun nelisp-artifact-test--v5-flat-section (&optional text-base64 report)
  "Return synthetic flat metadata serialized through the version 5 wire lane."
  (list :native-section-version nelisp-artifact--native-section-version
        :arch "x86_64"
        :symbols '("v5")
        :text-base64 (or text-base64 "AAAAAA==")
        :reloc-format nelisp-artifact--compact-reloc-format
        :reloc-count 1
        :reloc-data '(0 0 -4)
        :extern-symbols '("runtime")
        :defuns
        '((:name "v5" :offset 4 :size 8 :arity 0 :param-class gp
           :rt-slot-count 17 :body-offset 0))
        :runtime-end t
        :object-format nelisp-artifact--native-object-format
        :object-size 1
        :object-sha256 "ignored"
        :object-base64 "ww=="
        :text-size 4
        :compile-report report))

(ert-deftest nelisp-artifact/v5-native-section-fixed-points-and-direct-reader ()
  "Version 5 sizes are exact and native reading uses CONTENT without slicing."
  (let* ((wire
          (nelisp-artifact--native-section-wire-v5
           (nelisp-artifact-test--v5-flat-section
            "AAAAAA==" '((:reason "tail decoy")))))
         (source (prin1-to-string wire))
         (runtime (plist-get wire :runtime-prefix))
         (runtime-source (prin1-to-string runtime))
         (runtime-pos (string-search runtime-source source))
         (calls 0))
    (should (= (plist-get wire :serialized-char-size) (length source)))
    (should (vectorp runtime))
    (should (= (aref runtime 0)
               nelisp-artifact--native-runtime-prefix-layout-version))
    (should (= (aref runtime 1) (length runtime-source)))
    (should runtime-pos)
    (cl-letf (((symbol-function 'nelisp--read-batch-from-string-native)
               (lambda (text pos count)
                 (setq calls (1+ calls))
                 (should (eq text source))
                 (should (= pos runtime-pos))
                 (should (= count 1))
                 (let ((parsed (read-from-string text pos)))
                   (cons (list (car parsed)) (cdr parsed)))))
              ((symbol-function 'read-from-string)
               (lambda (&rest _)
                 (ert-fail "canonical v5 used the generic reader"))))
      (let* ((parsed
              (nelisp-artifact--read-private-native-load-section
               source 0 (length source) "v5-direct"))
             (section (car parsed)))
        (should (= calls 0))
        (should (= (cdr parsed) (length source)))
        (should (vectorp section))
        (should
         (equal
          (nelisp-artifact--native-section-get section :symbols)
          '("v5")))
        (should
         (equal
          (nelisp-artifact--native-section-get section :reloc-data)
          [0 0 -4]))
        (should-not
         (nelisp-artifact--native-section-get section :object-base64))
        (should-not
         (nelisp-artifact--native-section-get section :compile-report))))))

(ert-deftest nelisp-artifact/v5-direct-decoder-parity-and-corruption ()
  "Canonical v5 decoding is direct, bounded, and fails closed."
  (let* ((wire
          (nelisp-artifact--native-section-wire-v5
           (nelisp-artifact-test--v5-flat-section
            "AAAAAA==" '((:reason "tail [2 9 decoy")))))
         (runtime (plist-get wire :runtime-prefix))
         (source (prin1-to-string wire))
         (calls 0))
    (cl-letf (((symbol-function 'nelisp--read-batch-from-string-native)
               (lambda (&rest _)
                 (setq calls (1+ calls))
                 (error "generic batch reader forbidden"))))
      (let ((decoded
             (car
              (nelisp-artifact--read-private-native-load-section
               source 0 (length source) "v5-parity"))))
        (should (= calls 0))
        (dolist (index '(0 1 2 3 4 5 6 8 9))
          (should (equal (aref decoded index) (aref runtime index))))
        (should (equal (append (aref decoded 7) nil)
                       (aref runtime 7))))
      (dolist
          (corrupt
           (list
            (substring source 0 (1- (length source)))
            (replace-regexp-in-string
             "AAAAAA==" "AAA!AA==" source t t)
            (replace-regexp-in-string
             "indexed-plt32-v1 1 (0 0 -4)"
             "indexed-plt32-v1 2 (0 0 -4)" source t t)
            (replace-regexp-in-string
             ":param-class gp" ":param-class xx" source t t)))
        (should-error
         (nelisp-artifact--read-private-native-load-section
          corrupt 0 (length corrupt) "v5-corrupt"))
        (should (= calls 0))))))

(ert-deftest nelisp-artifact/v5-layout1-keeps-explicit-generic-fallback ()
  "Only the parenthesized legacy v5 runtime shape uses the generic reader."
  (let* ((runtime
          (nelisp-artifact--native-runtime-prefix-finalize-char-size
           (list :runtime-prefix-char-size 0
                 :arch "x86_64"
                 :symbols '("legacy-v5")
                 :text-base64 "AAAAAA=="
                 :reloc-format nelisp-artifact--compact-reloc-format
                 :reloc-count 1
                 :reloc-data '(0 0 -4)
                 :extern-symbols '("runtime")
                 :defuns
                 '((:name "legacy-v5" :offset 4 :size 8 :arity 0
                    :param-class gp :rt-slot-count 17 :body-offset 0))
                 :runtime-end t)))
         (wire
          (nelisp-artifact--native-section-finalize-char-size
           (list :native-section-version nelisp-artifact--native-section-version
                 :serialized-char-size 0
                 :runtime-prefix runtime
                 :object-format nelisp-artifact--native-object-format)))
         (source (prin1-to-string wire))
         (calls 0))
    (cl-letf (((symbol-function 'nelisp--read-batch-from-string-native)
               (lambda (text pos _count)
                 (setq calls (1+ calls))
                 (let ((parsed (read-from-string text pos)))
                   (cons (list (car parsed)) (cdr parsed))))))
      (let ((section
             (car
              (nelisp-artifact--read-private-native-load-section
               source 0 (length source) "legacy-v5"))))
        (should (= calls 1))
        (should (listp section))
        (should
         (equal
          (nelisp-artifact--native-section-get section :symbols)
          '("legacy-v5")))))))

(ert-deftest nelisp-artifact/v5-native-section-rejects-corrupt-fixed-sizes ()
  "Both version 5 fixed-point sizes are authoritative and fail closed."
  (let* ((wire
          (nelisp-artifact--native-section-wire-v5
           (nelisp-artifact-test--v5-flat-section)))
         (source (prin1-to-string wire))
         (section-size (plist-get wire :serialized-char-size))
         (runtime (plist-get wire :runtime-prefix))
         (runtime-size (aref runtime 1)))
    (dolist (replacement
             (list
              (cons (format ":serialized-char-size %d" section-size)
                    (format ":serialized-char-size %d" (1- section-size)))
              (cons (format ":runtime-prefix [%d %d"
                            nelisp-artifact--native-runtime-prefix-layout-version
                            runtime-size)
                    (format ":runtime-prefix [%d %d"
                            nelisp-artifact--native-runtime-prefix-layout-version
                            (1+ runtime-size)))))
      (let ((corrupt
             (replace-regexp-in-string
              (regexp-quote (car replacement))
              (cdr replacement) source t t)))
        (should-error
         (nelisp-artifact--read-private-native-load-section
          corrupt 0 (length corrupt) "v5-corrupt"))))))

(ert-deftest nelisp-artifact/v5-artifact-header-offsets-are-fixed-and-exact ()
  "Generated layout offsets point to authoritative native/module values."
  (let* ((flat (nelisp-artifact-test--v5-flat-section))
         (payload
          (nelisp-artifact--artifact-payload
           "v5-offset.el"
           '((:eval (quote (:native-offset 1 :module-offset 2))))
           '(v5-offset) 1 'neln flat nil 'bytecode))
         (artifact (nelisp-artifact--artifact-string payload))
         (artifact-again (nelisp-artifact--artifact-string payload))
         (parsed (nelisp-artifact--parse-payload artifact "v5-offset.neln"))
         (native-offset (plist-get parsed :native-offset))
         (module-offset (plist-get parsed :module-offset)))
    (should (equal artifact artifact-again))
    (should (= (plist-get parsed :layout-version)
               nelisp-artifact--layout-version))
    (should (integerp native-offset))
    (should (integerp module-offset))
    (should (string-prefix-p ":native "
                             (substring artifact (- native-offset 8)
                                        native-offset)))
    (should (string-prefix-p "(:native-section-version 5 "
                             (substring artifact native-offset)))
    (should (string-prefix-p ":module-init "
                             (substring artifact (- module-offset 13)
                                        module-offset)))
    (should (string-prefix-p "((:eval "
                             (substring artifact module-offset)))
    (should (= (length
                (nelisp-artifact--read-serialized-native-sections-for-load
                 artifact "v5-offset.neln" native-offset))
               1))
    (should-error
     (nelisp-artifact--read-serialized-native-sections-for-load
      artifact "v5-offset-corrupt.neln" module-offset))
    (should-error
     (nelisp-artifact--replay-module-streaming
      artifact "v5-offset-corrupt.neln" native-offset))
    (let ((corrupt
           (replace-regexp-in-string
            (format ":module-offset %d" module-offset)
            (format ":module-offset %d" (1+ module-offset))
            artifact t t)))
      (should-error
       (nelisp-artifact--load-private-fast
        "v5-offset-corrupt.neln" corrupt '(:kind neln))))))

(ert-deftest nelisp-artifact/v4-native-section-has-self-sized-runtime-prefix ()
  "Version 4 runtime metadata precedes the diagnostic/object tail."
  (let* ((section
          (nelisp-artifact-test--v4-section
           "ww==" '((:reason "日本語 :runtime-end decoy"))))
         (source (prin1-to-string section))
         (runtime-end (string-match " :runtime-end t" source))
         (object-format (string-match " :object-format " source))
         (object-base64 (string-match " :object-base64 \"ww==\" :text-size "
                                      source))
         (parsed
          (car
           (nelisp-artifact--read-private-native-load-section
            source 0 (length source) "v4-unicode"))))
    (should (= (plist-get section :serialized-char-size) (length source)))
    (should (< (length source) (string-bytes source)))
    (should runtime-end)
    (should object-format)
    (should object-base64)
    (should (< runtime-end object-format))
    (should (< object-format object-base64))
    (should (= (plist-get parsed :serialized-char-size) (length source)))
    (should (vectorp (plist-get parsed :reloc-data)))
    (should-not (plist-member parsed :object-base64))
    (should-not (plist-member parsed :compile-report))))

(ert-deftest nelisp-artifact/v4-native-batch-prefix-skips-diagnostic-tail ()
  "The native batch proof reads one runtime plist and skips its object tail."
  (let* ((section
          (nelisp-artifact-test--v4-section
           "ww==" '((:reason "diagnostic :runtime-end t decoy"))))
         (source (prin1-to-string section))
         (calls 0))
    (cl-letf (((symbol-function 'nelisp--read-batch-from-string-native)
               (lambda (text pos count)
                 (setq calls (1+ calls))
                 (should (= pos 0))
                 (should (= count 1))
                 (should-not (string-match-p ":object-format" text))
                 (let ((parsed (read-from-string text)))
                   (cons (list (car parsed)) (cdr parsed))))))
      (let* ((parsed
              (nelisp-artifact--read-private-native-load-section-v4
               source 0 (length source) "v4-native-prefix"))
             (runtime (car parsed)))
        (should (= calls 1))
        (should (= (cdr parsed) (length source)))
        (should (equal (plist-get runtime :symbols) '("v4")))
        (should (eq (plist-get runtime :runtime-end) t))
        (should-not (plist-member runtime :object-base64))
        (should-not (plist-member runtime :compile-report))))))

(ert-deftest nelisp-artifact/generated-v4-native-key-uses-literal-bootstrap ()
  "Generated v4 input bypasses the full top-level native-key scanner."
  (let* ((section
          (prin1-to-string
           (nelisp-artifact-test--v4-section "ww==" nil)))
         (content
          (concat
           nelisp-artifact--magic
           "(:format nelisp-private-nelc-v2 :kind neln :features nil "
           ":native-sections (" section ") "
           ":module-init ((:eval (quote (:native-sections nested)))) "
           ":entry nil)\n"))
         (scans 0))
    (cl-letf (((symbol-function 'nelisp--read-batch-from-string-native)
               (lambda (text pos count)
                 (should (= pos 0))
                 (should (= count 1))
                 (let ((parsed (read-from-string text)))
                   (cons (list (car parsed)) (cdr parsed)))))
              ((symbol-function
                'nelisp-artifact--private-list-key-positions)
               (lambda (&rest _)
                 (setq scans (1+ scans))
                 (error "generated v4 must not enter full key scan"))))
      (let ((sections
             (nelisp-artifact--read-serialized-native-sections-for-load
              content "generated-v4.neln")))
        (should (= scans 0))
        (should (= (length sections) 1))
        (should (equal (plist-get (car sections) :symbols) '("v4")))))))

(ert-deftest nelisp-artifact/v4-native-section-rejects-corrupt-char-size ()
  "The self-sized loader rejects short and long section boundaries."
  (let* ((source
          (prin1-to-string
           (nelisp-artifact-test--v4-section "ww==" nil)))
         (size (length source)))
    (dolist (bad-size (list (1- size) (1+ size)))
      (let ((corrupt
             (replace-regexp-in-string
              (format ":serialized-char-size %d" size)
              (format ":serialized-char-size %d" bad-size)
              source t t)))
        (should-error
         (nelisp-artifact--read-private-native-load-section
          corrupt 0 (length corrupt) "v4-corrupt-size"))))))

(ert-deftest nelisp-artifact/v4-native-section-accepts-zero-reloc-nil ()
  "A generated zero-relocation v4 section uses nil and yields an empty vector."
  (let* ((section (nelisp-artifact-test--v4-section "ww==" nil))
         (_count (plist-put section :reloc-count 0))
         (_data (plist-put section :reloc-data nil))
         (_size (nelisp-artifact--native-section-finalize-char-size section))
         (source (prin1-to-string section))
         (parsed
          (car
           (nelisp-artifact--read-private-native-load-section
            source 0 (length source) "v4-zero-reloc"))))
    (should (vectorp (plist-get parsed :reloc-data)))
    (should (= (length (plist-get parsed :reloc-data)) 0))))

(ert-deftest nelisp-artifact/v4-native-section-jumps-over-hundreds-of-tails ()
  "Loading many v4 sections never invokes the whole-section legacy scanner."
  (let* ((section
          (prin1-to-string
           (nelisp-artifact-test--v4-section
            (make-string 10000 ?x)
            '((:reason ":arch nested-decoy")))))
         (count 200)
         (parts nil)
         (legacy-scans 0)
         (string-scans 0)
         (private-string-end
          (symbol-function 'nelisp-artifact--private-string-end)))
    (dotimes (_ count)
      (setq parts (cons section parts)))
    (let ((content
           (concat
            nelisp-artifact--magic
            "(:format nelisp-private-nelc-v2 :kind neln "
            ":source \"many.el\" :native-sections ("
            (mapconcat #'identity (nreverse parts) " ")
            ") :module-init nil :features nil :entry nil)\n")))
      (cl-letf
          (((symbol-function
             'nelisp-artifact--scan-private-native-load-section)
            (lambda (&rest _)
              (setq legacy-scans (1+ legacy-scans))
              (error "v4 must not call the legacy section scanner")))
           ((symbol-function 'nelisp-artifact--private-string-end)
            (lambda (source start limit label)
              (setq string-scans (1+ string-scans))
              (funcall private-string-end source start limit label))))
        (let ((sections
               (nelisp-artifact--read-serialized-native-sections-for-load
                content "many-v4.neln")))
          (should (= (length sections) count))
          (should (= legacy-scans 0))
          ;; Only :arch and :text-base64 enter the quote scanner.
          (should (= string-scans (* count 2))))))))

(ert-deftest nelisp-artifact/compact-reloc-reader-bypasses-generic-data-reader ()
  "The v3 numeric body enters the bounded decimal reader, never the sexp reader."
  (let* ((count 256)
         (numbers nil))
    (dotimes (i count)
      (setq numbers (cons -4 (cons 0 (cons (* i 4) numbers)))))
    (setq numbers (nreverse numbers))
    (let* ((section-source
            (concat
             "(:native-section-version 3 :arch \"x86_64\" "
             ":symbols (\"compact\") :text-base64 \"AAAAAA==\" "
             ":reloc-format indexed-plt32-v1 :reloc-count "
             (number-to-string count)
             " :reloc-data " (prin1-to-string numbers)
             " :extern-symbols (\"runtime\") "
             ":defuns ((:name \"compact\" :offset 4 :body-offset 0 "
             ":arity 0 :rt-slot-count 17)))"))
           (generic-reader
            (symbol-function 'nelisp-artifact--read-private-item))
           (largest-generic-span 0)
           (parsed nil))
      (cl-letf
          (((symbol-function 'nelisp-artifact--read-private-item)
            (lambda (source start end)
              (setq largest-generic-span
                    (max largest-generic-span (- end start)))
              (when (> (- end start) 1000)
                (error "compact relocation data reached generic reader"))
              (funcall generic-reader source start end))))
        (setq parsed
              (car
               (nelisp-artifact--read-private-native-load-section
                section-source 0 (length section-source) "compact-v3"))))
      (should (< largest-generic-span 1000))
      (should (vectorp (plist-get parsed :reloc-data)))
      (should (= (length (plist-get parsed :reloc-data)) (* count 3)))
      (should
       (equal (nelisp-artifact--native-section-relocs parsed)
              (mapcar
               (lambda (reloc)
                 (list :offset (plist-get reloc :offset)
                       :type 'plt32
                       :symbol "runtime"
                       :addend -4))
               (cl-loop for i below count
                        collect (list :offset (* i 4)))))))))

(ert-deftest nelisp-artifact/native-load-reader-supports-mixed-v2-v3-v4-v5-sections ()
  "The loader reads legacy and both self-sized section layouts together."
  (let* ((v2
          "(:native-section-version 2 :arch \"x86_64\" :symbols (\"old\") \
:text-base64 \"AAAAAA==\" \
:relocs ((:offset 0 :type plt32 :symbol \"runtime\" :addend -4)) \
:extern-symbols (\"runtime\") \
:defuns ((:name \"old\" :offset 4 :body-offset 0 :arity 0 :rt-slot-count 17)))")
         (v3
          "(:native-section-version 3 :arch \"x86_64\" :symbols (\"new\") \
:text-base64 \"AAAAAA==\" :reloc-format indexed-plt32-v1 \
:reloc-count 1 :reloc-data (0 0 -4) :extern-symbols (\"runtime\") \
:defuns ((:name \"new\" :offset 4 :body-offset 0 :arity 0 :rt-slot-count 17)))")
         (v4
          (prin1-to-string (nelisp-artifact-test--v4-section "ww==" nil)))
         (v5
          (prin1-to-string
           (nelisp-artifact--native-section-wire-v5
            (nelisp-artifact-test--v5-flat-section))))
         (content
          (concat nelisp-artifact--magic
                  "(:format nelisp-private-nelc-v2 :kind neln "
                  ":module-init nil :features nil :native-sections ("
                  v2 " " v3 " " v4 " " v5 ") :entry nil)\n"))
         (sections
          (nelisp-artifact--read-serialized-native-sections-for-load
           content "mixed.neln")))
    (should (= (length sections) 4))
    (should (plist-member (car sections) :relocs))
    (should-not (plist-member (cadr sections) :relocs))
    (should (vectorp (plist-get (cadr sections) :reloc-data)))
    (should (vectorp (plist-get (caddr sections) :reloc-data)))
    (should
     (equal
      (nelisp-artifact--native-section-get
       (cadddr sections) :reloc-data)
      [0 0 -4]))
    (should
     (equal (mapcar #'nelisp-artifact--native-section-relocs sections)
            '(((:offset 0 :type plt32 :symbol "runtime" :addend -4))
              ((:offset 0 :type plt32 :symbol "runtime" :addend -4))
              ((:offset 0 :type plt32 :symbol "runtime" :addend -4))
              ((:offset 0 :type plt32 :symbol "runtime" :addend -4)))))))

(ert-deftest nelisp-artifact/compact-reloc-link-preserves-order-and-last-wins ()
  "Indexed linking patches duplicate offsets in order and keeps export rules."
  (let* ((artifact "/tmp/compact-order.neln")
         (section
          (list :native-section-version 3
                :arch "x86_64" :symbols '("duplicate")
                :text-base64
                (base64-encode-string
                 (unibyte-string 0 0 0 0 #xc3) t)
                :reloc-format 'indexed-plt32-v1
                :reloc-count 2
                :reloc-data [0 0 -4 0 1 -4]
                :extern-symbols '("runtime-a" "runtime-b")
                :defuns
                '((:name "duplicate" :offset 4 :body-offset 0
                   :arity 0 :rt-slot-count 17))))
         (patches nil)
         (next-base 0)
         (nelisp-artifact--native-section-registry nil)
         (nelisp-artifact--native-runtime-mappings nil)
         (nelisp-artifact--native-artifact-linksets nil)
         (nelisp-artifact--native-artifact-symbol-index nil))
    (cl-letf
        (((symbol-function 'syscall-direct)
          (lambda (number address _size &rest _)
            (cond
             ((= number 9)
              (setq next-base (+ next-base 4096))
              next-base)
             ((memq number '(10 11)) 0)
             (t (error "unexpected syscall %S at %S" number address)))))
         ((symbol-function 'nelisp--ptr-copy-string-bytes)
          (lambda (_address string) (string-bytes string)))
         ((symbol-function 'nelisp-artifact--native-write-jump-stub)
          (lambda (address _target) address))
         ((symbol-function 'ptr-write-u32)
          (lambda (base offset value)
            (setq patches (append patches (list (list base offset value))))
            value))
         ((symbol-function 'nelisp--runtime-symbol-address)
          (lambda (name)
            (if (equal name "runtime-a") 16384 20480))))
      (nelisp-artifact--register-native-sections
       artifact (list section section))
      (let* ((plan
              (nelisp-artifact--native-preflight-artifact
               artifact (list section section)))
             (winner
              (cdr (assoc "duplicate"
                          (plist-get plan :symbol-index)))))
        (should (= (plist-get winner :section-index) 1))
        (should (= (length (plist-get plan :duplicates)) 1)))
      (nelisp-artifact--native-link-artifact artifact)
      ;; Two duplicate-offset entries per section remain in wire order.
      (should (= (length patches) 4))
      (should (= (nth 1 (nth 0 patches)) 0))
      (should (= (nth 1 (nth 1 patches)) 0))
      (should-not (= (nth 2 (nth 0 patches))
                     (nth 2 (nth 1 patches)))))))

(ert-deftest nelisp-artifact/compact-reloc-unresolved-and-bad-index-fail-before-mmap ()
  "Compact dependency and index validation complete before owning mappings."
  (let ((base
         (list :native-section-version 3
               :arch "x86_64" :symbols '("compact")
               :text-base64
               (base64-encode-string
                (unibyte-string 0 0 0 0 #xc3) t)
               :reloc-format 'indexed-plt32-v1
               :reloc-count 1
               :extern-symbols '("missing")
               :defuns
               '((:name "compact" :offset 4 :body-offset 0
                  :arity 0 :rt-slot-count 17))))
        (mmap-count 0)
        (nelisp-artifact--native-section-registry nil)
        (nelisp-artifact--native-runtime-mappings nil)
        (nelisp-artifact--native-artifact-linksets nil)
        (nelisp-artifact--native-artifact-symbol-index nil))
    (cl-letf (((symbol-function 'syscall-direct)
               (lambda (number &rest _)
                 (when (= number 9)
                   (setq mmap-count (1+ mmap-count)))
                 0))
              ((symbol-function 'nelisp--runtime-symbol-address)
               (lambda (_name) (error "missing"))))
      (dolist (data '([0 0 -4] [0 1 -4]))
        (let ((section (plist-put (copy-sequence base) :reloc-data data)))
          (nelisp-artifact--register-native-sections
           "/tmp/compact-invalid.neln" (list section))
          (should-error
           (nelisp-artifact--native-link-artifact
            "/tmp/compact-invalid.neln"))
          (should (= mmap-count 0)))))))

(provide 'nelisp-artifact-test)

;;; nelisp-artifact-test.el ends here
