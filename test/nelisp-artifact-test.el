;;; nelisp-artifact-test.el --- ERT for Doc 142 .nelc artifacts  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'nelisp-artifact)

;; Declared special so the version-pin test can dynamically bind it even
;; when `nelisp-cli' (which `defvar's it with a value) is not loaded.
(defvar nelisp--cli-version)

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
           "(defvar nelisp-artifact-test--pre-marker 2)\n"
           nil preload-path nil 'silent)
          (setq nelisp-artifact--loaded nil)
          (should-error (nelisp-artifact-load-file artifact-path)
                        :type 'nelisp-artifact-stale))
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
produced at compile time, so load only installs it.  A conservative 2x
margin guards against environment noise (in practice it is ~80x)."
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
          ;; source replay timing (warm runtime)
          (nelisp--reset) (nelisp-eval '(+ 1 1))
          (let ((src-t (let ((t0 (float-time)))
                         (nelisp-load-file source-path)
                         (- (float-time) t0))))
            ;; artifact load timing (warm runtime)
            (nelisp--reset) (nelisp-eval '(+ 1 1))
            (setq nelisp-artifact--loaded nil)
            (let ((art-t (let ((t0 (float-time)))
                           (nelisp-artifact-load-file artifact-path)
                           (- (float-time) t0))))
              (should (= (nelisp-eval '(nat-p-f3 5)) 23))
              (should (< art-t (/ src-t 2.0))))))
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

(ert-deftest nelisp-artifact/gate-6-4-neln-native-object-executes ()
  "Doc 142 §6.4 native EXEC: the native object embedded in a `.neln'
actually executes and returns the correct result — end-to-end elisp ->
AOT native .o -> embed -> extract -> link -> run.  Covers the
reloc-free leaf arithmetic subset (plain C integer ABI).  Skipped
without a host C toolchain."
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
