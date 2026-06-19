;;; nelisp-artifact-test.el --- ERT for Doc 142 .nelc artifacts  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'nelisp-artifact)
(require 'nelisp-runtime-image)

;; Declared special so the version-pin test can dynamically bind it even
;; when `nelisp-cli' (which `defvar's it with a value) is not loaded.
(defvar nelisp--cli-version)
(declare-function nelisp-cli-main "nelisp-cli" (argv))

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
            (should (eq (car (nth 2 (nth 0 module))) 'nelisp-bcl)))
          (rename-file source-path (concat source-path ".gone") t)
          (nelisp--reset)
          (setq nelisp-artifact--loaded nil)
          (nelisp-artifact-load-file artifact-path)
          (should (= (nelisp-eval '(nat-sq 9)) 81))
          (should (= (nelisp-eval '(nat-fact 5)) 120))   ; recursion via VM
          (should (= (nelisp-eval '(nat-add 7)) 17)))     ; forward ref to defvar
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
artifact-class native + the AOT runtime-abi + native metadata."
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
            (should (= (plist-get nat :native-section-version) 2))
            (should (member "nat-neln-sq" (plist-get nat :symbols)))
            (should (member "nat-neln-add3" (plist-get nat :symbols)))
            (should (> (plist-get nat :object-size) 0))
            (should (> (plist-get nat :text-size) 0))
            (should (stringp (plist-get payload-nat :text-base64)))
            (should (equal (plist-get nat :extern-symbols) nil))
            (should (cl-every (lambda (entry) (plist-get entry :native))
                              (plist-get nat :compile-report)))
            (should-not (plist-get nat :relocs))
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
         (parsed-module (plist-get parsed :module-init))
         (eval-form (cadr (car parsed-module))))
    (should (= (length parsed-module) 1))
    (should (eq (car (car parsed-module)) :eval-source-raw))
    (should (equal (cddr (car parsed-module)) forms))
    (should (equal (plist-get parsed :features) '(slice-serializer)))))

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
              ((symbol-function 'nelisp-aot-compile-to-link-unit)
               (lambda (sexp &rest _args)
                 (setq link-count (1+ link-count))
                 (if (= link-count 1)
                     (error "batch failed")
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
                         :extern-symbols nil))))
              ((symbol-function 'nelisp-aot-compile-to-object)
               (lambda (form path &rest _args)
                 (setq probe-count (1+ probe-count))
                 (if (eq (nth 1 form) 'opp-mixed-a)
                     (write-region "OBJ" nil path nil 'silent)
                   (error "unsupported"))))
              ((symbol-function 'nelisp-artifact--write-elf-rel-object)
               (lambda (path _unit)
                 (write-region "OBJ" nil path nil 'silent))))
      (setq native
            (nelisp-artifact--native-compile-section
             forms nil 'opportunistic))
      (should (= link-count 2))
      (should (= probe-count 2))
      (should (equal (plist-get native :symbols) '("opp-mixed-a")))
      (should (equal nelisp-artifact--last-native-compile-report
                     '((:name "opp-mixed-a" :native t)
                       (:name "opp-mixed-b" :native nil
                        :reason "unsupported")))))))

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

(ert-deftest nelisp-artifact/native-exec-cli-skips-fast-simple-for-extern-artifact ()
  "CLI native exec routes extern-bearing artifacts to the general trampoline.
The whole linked object can contain unresolved externs even when the requested
integer-ABI symbol is itself simple.  In that case the simple stdout fast path
must not run first, because the general trampoline provides the extern shims."
  (let ((fast-count 0)
        (general-call nil)
        (stdout nil))
    (cl-letf (((symbol-function 'nelisp-artifact-read-manifest)
               (lambda (_path)
                 '(:kind neln
                   :native
                   (:extern-symbols ("nl_alloc_str")
                    :defuns
                    ((:name "hot-fn"
                      :arity 2
                      :param-class gp
                      :rt-slot-count 17
                      :body-offset 13))))))
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
                   (:extern-symbols nil
                    :defuns
                    ((:name "hot-fn"
                      :arity 2
                      :param-class gp
                      :rt-slot-count 0
                      :body-offset 13))))))
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
            (should (consp fn))
            (should (eq (car fn) 'nelisp-native-function))
            (should (eq (nth 2 fn) 'source-native-add)))
          (should (= (nelisp-eval '(source-native-add 41)) 42))
          (should
           (cl-some (lambda (entry)
                      (and (eq (plist-get entry :event) 'call)
                           (eq (plist-get entry :symbol) 'source-native-add)
                           (eq (plist-get entry :mode) 'native)))
                    (nelisp-artifact-native-dispatch-report))))
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
            (should (consp fn))
            (should (eq (car fn) 'nelisp-native-function))
            (should (eq (nth 2 fn) 'rt-native-load-sq)))
          (should (= (nelisp-eval '(rt-native-load-sq 9)) 81))
          (should (= (nelisp-eval '(rt-native-load-sq 10)) 100))
          (let ((report (nelisp-artifact-native-dispatch-report)))
            (should
             (cl-some (lambda (entry)
                        (and (eq (plist-get entry :event) 'install)
                             (= (plist-get entry :installed) 1)))
                      report))
          (should
           (cl-some (lambda (entry)
                      (and (eq (plist-get entry :event) 'call)
                           (eq (plist-get entry :symbol) 'rt-native-load-sq)
                           (eq (plist-get entry :mode) 'native)))
                     report))
          (should-not
           (cl-some (lambda (entry)
                      (and (eq (plist-get entry :event) 'call)
                           (eq (plist-get entry :symbol) 'rt-native-load-sq)
                           (eq (plist-get entry :mode) 'fallback)))
                    report))))
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

(provide 'nelisp-artifact-test)

;;; nelisp-artifact-test.el ends here
