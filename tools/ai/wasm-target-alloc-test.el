;;; wasm-target-alloc-test.el --- Genuine allocator controls -*- lexical-binding: t; -*-
(require 'ert)
(defvar wasm-target-alloc--library-only t)
(require 'wasm-target-alloc-probe)

(ert-deftest wasm-target-alloc-session-macro-isolation ()
  ;; AUDIT-3's exact macro must be invisible to production and provenance.
  ;; Keep the in-session compile as a live negative control only.
  (let* ((healthy (cl-letf (((symbol-function 'nl_alloc_check_stamp) nil))
                    (wasm-target-alloc-build "target/wasm-target-alloc/session-healthy.wasm")))
         (healthy-hash (wasm-rootstack--hash healthy))
         (path "target/wasm-target-alloc/session-macro.wasm")
         (local "target/wasm-target-alloc/session-local.wasm"))
    ;; Compare the saved input's clean-child compile with the probe output.
    ;; Reproducibility is a relation, independent of a pinned module digest.
    (wasm-target-alloc-compile-file (concat healthy ".source.eld") local)
    (should (equal healthy-hash (wasm-rootstack--hash local)))
    (let* ((record (with-temp-buffer
                     (insert-file-contents (concat healthy ".compile.eld"))
                     (read (current-buffer))))
           (files (plist-get record :files))
           (harness (expand-file-name "tools/ai/wasm-target-alloc.el")))
      (dolist (bad (list (cons (cons harness (wasm-rootstack--hash harness)) files)
                        (cons (car files) files)
                        (cl-remove (expand-file-name "lisp/nelisp-aot-compiler.el")
                                   files :key #'car :test #'equal)
                        (cons (cons (caar files) (make-string 64 ?0)) (cdr files))))
        (let ((message (cadr (should-error
                              (wasm-target-alloc-compiler-files bad (plist-get record :installation-root))))))
          (should (if (equal (caar bad) harness)
                      (equal message (concat "Unexpected compiler child load " harness))
                    (equal message "Allocator compiler loaded-file identity or isolation mismatch")))))
      (should-error (wasm-target-alloc-compiler-files files "/"))
      (princ (format "control compiler-loaded-files: %d healthy; tools/ai, duplicate, missing compiler, stale hash rejected\n"
                     (length files))))
    (cl-letf (((symbol-function 'nl_alloc_check_stamp) nil))
      (eval '(defmacro nl_alloc_check_stamp (obj) obj))
      (wasm-target-alloc-build path)
      (should (equal (wasm-rootstack--hash (concat healthy ".source.eld"))
                     (wasm-rootstack--hash (concat path ".source.eld"))))
      (let ((source (with-temp-buffer
                      (insert-file-contents (concat path ".source.eld"))
                      (read (current-buffer)))))
        (nelisp-aot-compile-to-object source local :arch 'wasm32 :format 'wasm))
      (should-not (equal healthy-hash (wasm-rootstack--hash local)))
      (princ (format "control audit-3: healthy=%s child=%s in-session=%s source=%s\n"
                     healthy-hash (wasm-rootstack--hash path) (wasm-rootstack--hash local)
                     (wasm-rootstack--hash (concat path ".source.eld"))))
      (should (equal healthy-hash (wasm-rootstack--hash path)))
      (wasm-target-alloc-test-provenance path))))

(defun wasm-target-alloc-test-node (path &optional manifest)
  "Run checker within this parent's heavy slot."
  (with-temp-buffer
    (let ((rc (apply #'call-process "bash" nil t nil
                     "-c" "ulimit -v 8388608; exec \"$@\"" "node"
                     "timeout" "120" "node" "--disable-wasm-trap-handler"
                     "tools/ai/wasm-target-alloc-check.mjs"
                     (delq nil (list path manifest)))))
      (let ((output (buffer-string)))
        (with-temp-file (concat (or manifest path) ".check.log") (insert output))
        (cons rc output)))))

(ert-deftest wasm-target-alloc-rev7-native-isolation ()
  (let* ((launcher (symbol-function 'call-process))
         (process-environment (copy-sequence process-environment))
         (launches 0)
         (path (wasm-target-alloc-build)))
    (setenv "EMACSNATIVELOADPATH" (expand-file-name "tmp/native-contamination"))
    (let* ((record
            (cl-letf (((symbol-function 'call-process)
                       (lambda (&rest args)
                         (cl-incf launches)
                         (should-not (getenv "EMACSNATIVELOADPATH"))
                         ;; Disable native loading before all compiler loads.
                         (should (equal (cadr (member "--eval" args)) "(setq load-no-native t)"))
                         (apply launcher args))))
              (wasm-target-alloc-compile-file
               (concat path ".source.eld") (concat path ".nonative.wasm"))))
           (files (plist-get record :files))
           (eln (expand-file-name "rev7-control.eln" (plist-get record :installation-root))))
      (should (= launches 1))
      (should (equal (wasm-rootstack--hash path)
                     (wasm-rootstack--hash (concat path ".nonative.wasm"))))
      (should (getenv "EMACSNATIVELOADPATH"))
      (should-not (cl-some (lambda (row) (string-match-p "\\.eln\\'" (car row))) files))
      ;; A row under the trusted installation root must still be rejected.
      (let ((message (cadr (should-error
                            (wasm-target-alloc-compiler-files
                             (cons (cons eln (make-string 64 ?0)) files)
                             (plist-get record :installation-root))))))
        (should (equal message (concat "Native compiler child load forbidden " eln))))
      (let* ((manifest (json-read-file (concat path ".json")))
             (file "target/wasm-target-alloc/compiler-eln.json"))
        (setf (alist-get 'compiler_files manifest)
              (vconcat (alist-get 'compiler_files manifest)
                       (vector (vector eln (make-string 64 ?0)))))
        (with-temp-file file (insert (json-encode manifest)))
        (let ((result (wasm-target-alloc-test-node path file)))
          (should (equal (car result) 1))
          (should (string-match-p (regexp-quote (concat "Native compiler child load forbidden " eln))
                                  (cdr result)))))
      (princ (format "control audit-6 native: first eval load-no-native=t; environment cleared; %d child rows, no .eln; parent and Node reject .eln\n"
                     (length files))))))

(ert-deftest wasm-target-alloc-rev7-out-of-chunk-store ()
  ;; AUDIT-6's exact genuine-source drift, in a test-only source tree.
  ;; The real probe regenerates all source/closure/module hashes consistently.
  (let* ((original-root wasm-rootstack--root)
         (directory (make-temp-file "alloc-out-of-chunk-" t)))
    (unwind-protect
        (progn
          (dolist (dir '("lisp" "src" "scripts" "tools"))
            (copy-directory (expand-file-name dir original-root)
                            (expand-file-name dir directory) nil t t))
          (make-symbolic-link (expand-file-name "vendor" original-root)
                              (expand-file-name "vendor" directory))
          (let ((build (expand-file-name "scripts/nelisp-standalone-build.el" directory)))
            (with-temp-buffer
              (insert-file-contents build)
              (search-forward "(ptr-write-u64 (- obj 8) 0 want)  ; BT @ hdr; low bits 0 = mark 0")
              (end-of-line)
              (insert "\n                    (ptr-write-u64 (+ base size) 0 want)")
              (write-region nil nil build nil 'silent)))
          (let ((default-directory (file-name-as-directory directory)))
            (with-temp-buffer
              (let ((rc (call-process
                         "bash" nil t nil "-c" "ulimit -v 8388608; exec \"$@\"" "probe"
                         "timeout" "120" (expand-file-name invocation-name invocation-directory)
                         "-Q" "--batch" "-L" "lisp" "-L" "src" "-L" "tools/ai"
                         "--eval" "(setq load-prefer-newer t)"
                         "-l" "lisp/nelisp-asm-wasm.el" "-l" "lisp/nelisp-aot-compiler.el"
                         "-l" "tools/ai/wasm-target-alloc-probe.el")))
                (princ (buffer-string))
                (should (equal rc 0))))
            (let* ((manifest (json-read-file "target/wasm-target-alloc/alloc.wasm.json"))
                   (build-hash (wasm-rootstack--hash "scripts/nelisp-standalone-build.el"))
                   (row (cl-find "scripts/nelisp-standalone-build.el"
                                 (append (alist-get 'source_sha256 manifest) nil)
                                 :key (lambda (entry) (aref entry 0)) :test #'equal))
                   (definition (cl-find "nl_chunk_try_alloc"
                                        (append (alist-get 'closure manifest) nil)
                                        :key (lambda (entry) (alist-get 'name entry)) :test #'equal)))
              (should (equal (aref row 1) build-hash))
              (should (string-match-p (regexp-quote "(ptr-write-u64 (+ base size) 0 want)")
                                      (alist-get 'definition definition)))
              (princ (format "control audit-6 drifted-build=%s; manifest hash and genuine store agree\n" build-hash)))
            (let ((result (wasm-target-alloc-test-node "target/wasm-target-alloc/alloc.wasm")))
              (princ (format "control audit-6 out-of-chunk: %s" (cdr result)))
              (should (equal (car result) 1))
              (should (string-match-p "allocation 0 whole linear above-chunk @" (cdr result))))))
      (delete-directory directory t))))

(ert-deftest wasm-target-alloc-child-shadow-controls ()
  ;; Plant directly in isolated copies: never rely on process-exit timing.
  (let* ((input (expand-file-name "target/wasm-target-alloc/alloc.wasm.source.eld"))
         (_baseline (wasm-target-alloc-build))
         (healthy (wasm-rootstack--hash "target/wasm-target-alloc/alloc.wasm"))
         (original-root wasm-rootstack--root)
         (directory (make-temp-file "alloc-shadow-" t)))
    (unwind-protect
        (progn
          (dolist (file wasm-target-alloc-compiler-repository-files)
            (let ((destination (expand-file-name file directory)))
              (make-directory (file-name-directory destination) t)
              (copy-file (expand-file-name file original-root) destination)))
          (dolist (dir '("src" "lisp"))
            (let* ((wasm-rootstack--root (file-name-as-directory directory))
                   (shadow (expand-file-name (concat dir "/cl-lib.el") directory))
                   (marker (expand-file-name "shadow-loaded" directory))
                   (output (expand-file-name (concat dir ".wasm") directory)))
              (make-directory (file-name-directory shadow) t)
              (with-temp-file shadow
                (prin1 `(progn
                          (with-temp-file ,marker (insert "loaded"))
                          (defmacro nl_alloc_check_stamp (obj) obj)
                          (load (expand-file-name "emacs-lisp/cl-lib" lisp-directory) nil t))
                       (current-buffer)))
              (if (equal dir "src")
                  (let ((record (wasm-target-alloc-compile-file input output)))
                    (should (equal healthy (wasm-rootstack--hash output)))
                    (should-not (assoc shadow (plist-get record :files)))
                    (should-not (file-exists-p marker))
                    (princ (format "control audit-4 src/cl-lib.el: invisible; module=%s\n" healthy)))
                (let ((message (cadr (should-error (wasm-target-alloc-compile-file input output)))))
                  (should (equal message "Repository library shadows installation library cl-lib"))
                  (should-not (file-exists-p marker))
                  (should-not (file-exists-p output))
                  (should-not (file-exists-p (concat output ".compile.eld")))
                  ;; The same hashed planted file is forbidden by Node as well.
                  (let* ((manifest (json-read-file "target/wasm-target-alloc/alloc.wasm.json"))
                         (file "target/wasm-target-alloc/planted-lisp.json"))
                    (setf (alist-get 'compiler_files manifest)
                          (vconcat (alist-get 'compiler_files manifest)
                                   (vector (vector shadow (wasm-rootstack--hash shadow)))))
                    (with-temp-file file (insert (json-encode manifest)))
                    (let ((result (wasm-target-alloc-test-node "target/wasm-target-alloc/alloc.wasm" file)))
                      (should (equal (car result) 1))
                      (should (string-match-p (regexp-quote (concat "Unexpected compiler child load " shadow))
                                              (cdr result)))))
                  (princ (format "control audit-4 lisp/cl-lib.el: parent and Node reject: %s\n" message)))))))
      (delete-directory directory t))))

(ert-deftest wasm-target-alloc-self-hiding-shadow-and-appended-path ()
  ;; Exact AUDIT-5 payload, planted directly rather than by a harness kill hook.
  (let* ((baseline (wasm-target-alloc-build))
         (input (expand-file-name (concat baseline ".source.eld")))
         (healthy (wasm-rootstack--hash baseline))
         (original-root wasm-rootstack--root)
         (directory (make-temp-file "alloc-self-hiding-" t))
         (shadow (expand-file-name "lisp/cl-lib.el" directory))
         (output (expand-file-name "shadow.wasm" directory))
         (copy (expand-file-name "tools/ai/wasm-target-alloc.el" directory))
         (environment (expand-file-name "environment" directory))
         (marker (expand-file-name "environment-loaded" directory))
         (launcher (symbol-function 'call-process))
         (launches 0))
    (unwind-protect
        (progn
          (dolist (file wasm-target-alloc-compiler-repository-files)
            (let ((destination (expand-file-name file directory)))
              (make-directory (file-name-directory destination) t)
              (copy-file (expand-file-name file original-root) destination)))
          (with-temp-file shadow
            (prin1 '(progn
                      (defmacro nl_alloc_check_stamp (obj) obj)
                      (defvar nl--shadow-file load-file-name)
                      (add-hook 'after-load-functions
                                (lambda (_file) (setq load-history (assoc-delete-all nl--shadow-file load-history))))
                      (load (expand-file-name "emacs-lisp/cl-lib" lisp-directory) nil t))
                   (current-buffer)))
          (let ((wasm-rootstack--root (file-name-as-directory directory)))
            (cl-letf (((symbol-function 'call-process)
                       (lambda (&rest _args) (cl-incf launches) (error "Unexpected child launch"))))
              (let ((err (should-error (wasm-target-alloc-compile-file input output))))
                (should (equal (cadr err) "Repository library shadows installation library cl-lib")))
              (should (= launches 0)))
            (should-not (file-exists-p output))
            (should-not (file-exists-p (concat output ".compile.eld")))
            ;; Compiled library names have the same collision rule.
            (rename-file shadow (concat shadow "c"))
            (should (equal (cadr (should-error (wasm-target-alloc-check-library-shadows)))
                           "Repository library shadows installation library cl-lib"))
            (rename-file (concat shadow "c") shadow)
            (princ "control audit-5 self-hiding cl-lib.el / .elc: prelaunch rejection; launches=0\n")
            ;; Bypass only the prelaunch call in an on-disk test-only copy.
            ;; Evaluate its compiler function under a separate test-only name.
            (make-directory (file-name-directory copy) t)
            (with-temp-buffer
              (insert-file-contents (expand-file-name "tools/ai/wasm-target-alloc.el" original-root))
              (goto-char (point-min))
              (should (search-forward "(wasm-target-alloc-check-library-shadows)" nil t))
              (replace-match "nil ; test-only shadow check bypass" t t)
              (should-not (search-forward "(wasm-target-alloc-check-library-shadows)" nil t))
              (write-region (point-min) (point-max) copy nil 'silent))
            (with-temp-buffer
              (insert-file-contents copy)
              (goto-char (point-min))
              (re-search-forward "^(defun wasm-target-alloc-compile-file ")
              (goto-char (match-beginning 0))
              (let ((form (read (current-buffer))))
                (setf (cadr form) 'wasm-target-alloc-test-compile-without-shadow-check)
                (eval form t)))
            ;; An inherited EMACSLOADPATH must not reach the compiler either.
            (make-directory environment t)
            (with-temp-file (expand-file-name "cl-lib.el" environment)
              (prin1 `(progn
                        (with-temp-file ,marker (insert "loaded"))
                        (defmacro nl_alloc_check_stamp (obj) obj)
                        (load (expand-file-name "emacs-lisp/cl-lib" lisp-directory) nil t))
                     (current-buffer)))
            (let ((process-environment (copy-sequence process-environment)))
              (setenv "EMACSLOADPATH" (concat environment path-separator))
              (cl-letf (((symbol-function 'call-process)
                         (lambda (&rest args)
                           (cl-incf launches)
                           (should-not (member "-L" args))
                           (should-not (getenv "EMACSLOADPATH"))
                           (apply launcher args))))
                (let* ((record (wasm-target-alloc-test-compile-without-shadow-check input output))
                       (files (plist-get record :files))
                       (library (cl-find-if
                                 (lambda (row) (string-match-p "/cl-lib\\.elc?\\'" (car row))) files)))
                  (should (= launches 1))
                  (should (equal healthy (wasm-rootstack--hash output)))
                  (should library)
                  (should (string-prefix-p (wasm-target-alloc-emacs-lisp-root) (file-truename (car library))))
                  (should-not (assoc shadow files))
                  (should-not (file-exists-p marker))
                  (princ (format "control audit-5 bypassed precheck: installation=%s module=%s; EMACSLOADPATH cleared\n"
                                 (car library) (wasm-rootstack--hash output)))))
              (should (equal (getenv "EMACSLOADPATH") (concat environment path-separator))))))
      (when (fboundp 'wasm-target-alloc-test-compile-without-shadow-check)
        (fmakunbound 'wasm-target-alloc-test-compile-without-shadow-check))
      (delete-directory directory t))))

(ert-deftest wasm-target-alloc-probe-output-provenance-control ()
  ;; A self-consistent module/hash edit after emission must fail in the builder,
  ;; before any ERT-only provenance call or Node invocation.
  (let ((writer (symbol-function 'wasm-target-alloc-manifest))
        (path "target/wasm-target-alloc/probe-tampered.wasm"))
    (cl-letf (((symbol-function 'wasm-target-alloc-manifest)
               (lambda (&rest args)
                 (apply writer args)
                 (let ((manifest (json-read-file (concat path ".json"))))
                   (write-region "tampered" nil path t 'silent)
                   (setf (alist-get 'wasm_sha256 manifest) (wasm-rootstack--hash path))
                   (with-temp-file (concat path ".json") (insert (json-encode manifest)))))))
      (let ((message (cadr (should-error (wasm-target-alloc-build path)))))
        (should (string-prefix-p "Recompiled module identity " message))
        (princ (format "control probe output provenance: %s\n" message))))))

(defalias 'wasm-target-alloc-test-provenance #'wasm-target-alloc-provenance)

(ert-deftest wasm-target-alloc-closed-input-composer-controls ()
  ;; Inject through the composer: deriving expectations from it would accept
  ;; the exact AUDIT-2 macro even during saved-source recompilation.
  (let* ((baseline (wasm-target-alloc-build "target/wasm-target-alloc/schema-baseline.wasm"))
         (composer (symbol-function 'wasm-target-alloc-source)))
    (dolist (case '((audit-2-macro (defmacro nl_alloc_check_stamp (obj) obj))
                    (duplicate-defun (defun arena-size () 1048576))
                    (extra-harness (defun extra-harness () 0))))
      (let* ((path (format "target/wasm-target-alloc/schema-%s.wasm" (car case)))
             (manifest (json-read-file (concat baseline ".json"))))
        (when (file-exists-p path) (delete-file path))
        (cl-letf (((symbol-function 'wasm-target-alloc-source)
                   (lambda (&rest args)
                     (cons 'seq (cons (cadr case) (cdr (apply composer args)))))))
          (let ((err (should-error (wasm-target-alloc-build path))))
            (should (string-prefix-p "Unexpected compiler input form" (cadr err)))
            (should-not (file-exists-p path))
            (princ (format "control %s probe: %s\n" (car case) (cadr err))))
          ;; Supply consistent identity metadata for the saved attack input.
          ;; Provenance must reject its schema before attempting compilation.
          (setf (alist-get 'compiled_source manifest)
                `((path . ,(concat path ".source.eld"))
                  (sha256 . ,(wasm-rootstack--hash (concat path ".source.eld")))))
          (with-temp-file (concat path ".json") (insert (json-encode manifest)))
          (let ((err (should-error (wasm-target-alloc-test-provenance path))))
            (should (string-prefix-p "Unexpected compiler input form" (cadr err)))
            (princ (format "control %s provenance: %s\n" (car case) (cadr err)))))))))

(ert-deftest wasm-target-alloc-closed-input-schema-boundaries ()
  (let* ((contract (wasm-target-init-contract))
         (owners (plist-get (wasm-target-alloc-authority) :owners))
         (genuine (wasm-target-alloc-closure (wasm-target-alloc-oracle-record)))
         (source (wasm-target-alloc-source
                  contract owners (wasm-target-init-rewrite
                                   (wasm-target-alloc-closure
                                    (wasm-target-alloc-oracle-record) wasm-target-alloc-adapters))))
         (file "target/wasm-target-alloc/schema-boundaries.eld")
         (print-length nil) (print-level nil))
    (cl-labels ((validate (form &optional suffix)
                  (with-temp-file file (prin1 form (current-buffer)) (insert (or suffix "")))
                  (wasm-target-alloc-validate-compiler-input
                   file contract owners genuine wasm-target-alloc-adapters))
                (reject (label form &optional suffix)
                  (let ((err (should-error (validate form suffix))))
                    (should (string-prefix-p "Unexpected compiler input form" (cadr err)))
                    (princ (format "control schema-%s: %s\n" label (cadr err))))))
      ;; Also establish that the healthy saved input is accepted in full.
      (should (equal (validate source) source))
      (reject 'outer-progn (cons 'progn (cdr source)))
      (reject 'outer-atom 17)
      (reject 'improper-seq '(seq . bad))
      (dolist (extra '(nil 42 (defconst extra 1) (defvar extra 1)
                          (eval-when-compile (defmacro extra () 0))
                          (progn (defun extra () 0))
                          (data-blob extra "" bss)))
        (reject 'extra (append source (list extra))))
      (reject 'duplicate-storage (append source (list (cadr source))))
      (reject 'missing-storage (cons 'seq (cddr source)))
      (reject 'wrong-storage
              (cons 'seq (cons `(data-blob ,(cadadr source) "" bss) (cddr source))))
      ;; Every harness category, the mask pointer, both adapters and a genuine
      ;; allocator body must be compared as complete syntax, not just names.
      (dolist (name '(address-nl_rootstack_top address-wasm_target_alloc_failure
                     address-end arena-size nl_wasm_arena_init nl_rootstack_init
                     nl_freelist_small_mask_ptr nl_os_alloc_chunk nl_os_alloc_fail
                     nl_alloc_check_stamp))
        (let ((definition (cl-find name (cdr source) :key #'cadr)))
          (should definition)
          (reject (format "missing-%s" name) (cons 'seq (remq definition (cdr source))))
          (reject (format "changed-%s" name)
                  (cons 'seq (mapcar (lambda (form)
                                      (if (eq form definition) `(defun ,name () 0) form))
                                    (cdr source))))))
      (reject 'trailing-form source " (defmacro nl_alloc_check_stamp (obj) obj)")
      (reject 'trailing-truncated source " (defmacro")
      (reject 'truncated nil " (seq"))))

(ert-deftest wasm-target-alloc-walker-binding-and-clause-positions ()
  (should (equal
           (wasm-target-alloc-calls
            '((let ((a (first-call)))
                (let* ((b (second-call a)) (c (third-call b)))
                  (setq a (fourth-call c) b (fifth-call))
                  (while (predicate a)
                    (cond ((sixth-call b) (seventh-call)) (t (eighth-call))))))))
           '(eighth-call fifth-call first-call fourth-call predicate second-call seventh-call sixth-call third-call))))

(ert-deftest wasm-target-alloc-adapter-boundary ()
  ;; The third entry is the executed AUDIT-1 attack, including its exact body.
  ;; An invalid list must fail even when no provider record can be walked.
  (dolist (case `((,(append wasm-target-alloc-adapters
                           '((defun nl_alloc_check_stamp (obj) obj)))
                  . "nl_alloc_check_stamp")
                 (,(append wasm-target-alloc-adapters
                           (list (car wasm-target-alloc-adapters)))
                  . "nl_os_alloc_chunk")
                 (,(cdr wasm-target-alloc-adapters) . "nl_os_alloc_chunk")
                 (nil . "nl_os_alloc_chunk")))
    (let ((err (should-error (wasm-target-alloc-closure nil (car case)))))
      (should (equal (cadr err) (concat "Unpermitted allocator adapter " (cdr case))))
      (princ (format "control adapter-boundary: %s\n" (cadr err))))))

(ert-deftest wasm-target-alloc-third-adapter-build-control ()
  (let ((wasm-target-alloc-adapters
         (append wasm-target-alloc-adapters '((defun nl_alloc_check_stamp (obj) obj)))))
    (let ((err (should-error (wasm-target-alloc-build))))
      (should (equal (cadr err) "Unpermitted allocator adapter nl_alloc_check_stamp"))
      (princ (format "control third-adapter-build: %s\n" (cadr err))))))

(ert-deftest wasm-target-alloc-driver-failure-and-fresh-directory ()
  ;; Shims prevent every Emacs/Node launch. Each case has only the driver;
  ;; no generated output directory exists before the invocation.
  (dolist (driver '("accept" "regression"))
    (dolist (scenario '(all-fail first-fail last-fail success))
      (let* ((directory (make-temp-file "alloc-driver-" t))
             (script (expand-file-name
                      (format "tools/ai/wasm-target-alloc-%s.sh" driver)))
             (shim (pcase scenario
                     ('all-fail "return 7;")
                     ('first-fail "attempt=$(( ${attempt:-0} + 1 )); if (( attempt == 1 )); then return 7; fi; return 0;")
                     ('last-fail
                      (format "attempt=$(( ${attempt:-0} + 1 )); if (( attempt == %d )); then return 7; fi; return 0;"
                              (if (equal driver "accept") 3 6)))
                     ('success "return 0;")))
             (expected (if (eq scenario 'success) 0 7)))
        (unwind-protect
            (let ((default-directory directory))
              (copy-file script "driver.sh")
              (with-temp-buffer
                (let ((rc (call-process
                           "bash" nil t nil "-c"
                           (concat "timeout() { " shim " }; export -f timeout; bash driver.sh"))))
                  (should (equal rc expected))
                  (cl-loop for label in
                           (if (and (equal driver "accept") (memq scenario '(all-fail first-fail))) '("ert")
                             (if (equal driver "accept") '("ert" "current" "check")
                               '("regression-init-ert" "regression-init-probe"
                                 "regression-init-check" "regression-rootstack-ert"
                                 "regression-rootstack-fixture" "regression-rootstack-check")))
                           for i from 0 do
                           (let ((base (concat "target/wasm-target-alloc/" label))
                                 (step (if (or (eq scenario 'all-fail)
                                               (and (eq scenario 'first-fail) (= i 0))
                                               (and (eq scenario 'last-fail)
                                                    (= i (if (equal driver "accept") 2 5)))) "7" "0")))
                             (should (file-exists-p (concat base ".log")))
                             (should (equal (string-trim
                                             (with-temp-buffer
                                               (insert-file-contents (concat base ".rc"))
                                               (buffer-string))) step))))
                  (princ (format "control driver-%s-%s: aggregate rc=%s\n%s"
                                 driver scenario rc (buffer-string))))))
          (delete-directory directory t))))))

(ert-deftest wasm-target-alloc-unresolved-callee ()
  (let ((record '(:arena (seq (defun nl_alloc_bytes (size align)
                              (let ((x (missing-helper size))) x))) :support (seq))))
    (should (equal (cadr (should-error (wasm-target-alloc-closure record)))
                   "Unresolved allocator callee missing-helper"))))

(ert-deftest wasm-target-alloc-no-rewrite-control ()
  (let* ((record (wasm-target-alloc-oracle-record))
         (closure (wasm-target-alloc-closure record wasm-target-alloc-adapters))
         (err (should-error (wasm-target-init--assert-rewritten closure))))
    (should (string-match-p "raw-arena-atoms=" (cadr err)))
    (princ (format "control no-rewrite: %s\n" (cadr err)))
    (should (= 0 (wasm-target-init--assert-rewritten (wasm-target-init-rewrite closure))))))

(ert-deftest wasm-target-alloc-missing-adapter-control ()
  (let* ((record (wasm-target-alloc-oracle-record))
         (err (should-error (wasm-target-alloc-closure record (cdr wasm-target-alloc-adapters)))))
    (should (equal (cadr err) "Unpermitted allocator adapter nl_os_alloc_chunk"))
    (princ (format "control missing-adapter: %s\n" (cadr err)))))

(ert-deftest wasm-target-alloc-non-os-syscall-rejected ()
  (should (equal
           (cadr (should-error
                  (wasm-target-alloc-closure
                   '(:arena (seq (defun nl_alloc_bytes (size align)
                                   (syscall-direct 9 0 size 3 34 -1 0))) :support (seq)))))
           "Non-OS allocator definition nl_alloc_bytes needs syscall-direct")))

(defun wasm-target-alloc-test-corrupt-module (path mode)
  "Deliberately corrupt a validated fixture for the existing Node controls.
These artifacts are never accepted by the compiler-input provenance validator."
  (wasm-target-alloc-build path)
  (let* ((source-file (concat path ".source.eld"))
         (source (with-temp-buffer (insert-file-contents source-file) (read (current-buffer))))
         (manifest (json-read-file (concat path ".json")))
         (print-length nil) (print-level nil))
    (pcase mode
      ('body-mutation
       (cl-labels ((mutate (form)
                     (if (equal form '(ptr-write-u64 (- obj 8) 0 want))
                         '(ptr-write-u64 (- obj 8) 0 (+ want 8))
                       (if (consp form) (cons (mutate (car form)) (mutate (cdr form))) form))))
         (setq source (mutate source))))
      ((or 'skip-init 'no-fail)
       (setq source
             (cons 'seq (mapcar (lambda (form)
                                 (cond ((and (eq mode 'skip-init) (eq (cadr form) 'nl_wasm_arena_init))
                                        '(defun nl_wasm_arena_init () 0))
                                       ((and (eq mode 'no-fail) (eq (cadr form) 'nl_os_alloc_fail))
                                        '(defun nl_os_alloc_fail () 0))
                                       (t form))) (cdr source))))))
    (with-temp-file source-file (prin1 source (current-buffer)))
    (let ((err (should-error
                (wasm-target-alloc-validate-compiler-input
                 source-file (wasm-target-init-contract)
                 (plist-get (wasm-target-alloc-authority) :owners)
                 (wasm-target-alloc-closure (wasm-target-alloc-oracle-record))
                 wasm-target-alloc-adapters))))
      (should (string-prefix-p "Unexpected compiler input form" (cadr err)))
      (princ (format "control %s schema: %s\n" mode (cadr err))))
    ;; Explicit test-only compilation lets Node retain its semantic controls.
    (wasm-target-alloc-compile-file source-file path)
    (setf (alist-get 'wasm_sha256 manifest) (wasm-rootstack--hash path)
          (alist-get 'sha256 (alist-get 'compiled_source manifest)) (wasm-rootstack--hash source-file))
    (with-temp-file (concat path ".json") (insert (json-encode manifest)))))

(ert-deftest wasm-target-alloc-execution-and-controls ()
  (let* ((path (wasm-target-alloc-build))
         (result (wasm-target-alloc-test-node path)))
    (wasm-target-alloc-test-provenance path)
    (princ (cdr result))
    (should (equal (car result) 0))
    (should (string-match-p "GATE-COUNT checked=[1-9][0-9]* findings=0" (cdr result)))
    (princ (cdr result))
    (should (equal (string-trim (with-temp-buffer
                                (insert-file-contents "target/wasm-target-alloc/red.rc") (buffer-string))) "1"))
    (dolist (control '(stale-composed-hash third-composed-body missing-composed
                       extra-composed compiled-source-hash compiler-file-hash
                       compiler-file-missing compiler-tools-ai compiler-file-duplicate
                       compiler-unlisted-lisp compiler-src compiler-installation-root))
      (let* ((manifest (json-read-file (concat path ".json")))
             (rows (append (alist-get 'composed_closure manifest) nil))
             (stamp (cl-find "nl_alloc_check_stamp" rows :test #'equal
                             :key (lambda (row) (alist-get 'name row))))
             (file (format "target/wasm-target-alloc/%s.json" control))
             (message
              (pcase control
                ((or 'stale-composed-hash 'third-composed-body)
                 (setf (alist-get 'definition stamp) "(defun nl_alloc_check_stamp (obj) obj)")
                 (when (eq control 'third-composed-body)
                   (setf (alist-get 'sha256 stamp)
                         (secure-hash 'sha256 (alist-get 'definition stamp))))
                 (if (eq control 'stale-composed-hash) "composed hash nl_alloc_check_stamp"
                   "composed definition disagrees with genuine closure nl_alloc_check_stamp"))
                ('missing-composed
                 (setf (alist-get 'composed_closure manifest) (vconcat (delq stamp rows)))
                 "exact composed closure")
                ('extra-composed
                 (setf (alist-get 'composed_closure manifest)
                       (vconcat rows (wasm-target-alloc-definition-rows '((defun extra () 0)))))
                 "unrecorded composed definition extra")
                ('compiled-source-hash
                 (setf (alist-get 'sha256 (alist-get 'compiled_source manifest)) (make-string 64 ?0))
                 "compiled source identity")
                ('compiler-file-hash
                 (setf (aref (aref (alist-get 'compiler_files manifest) 0) 1) (make-string 64 ?0))
                 "compiler loaded identity")
                ('compiler-file-missing
                 (setf (alist-get 'compiler_files manifest)
                       (vconcat (cl-remove "lisp/nelisp-aot-compiler.el"
                                           (append (alist-get 'compiler_files manifest) nil)
                                           :key (lambda (row) (aref row 0)) :test #'equal)))
                 "compiler loaded file required lisp/nelisp-aot-compiler.el")
                ('compiler-tools-ai
                 (setf (alist-get 'compiler_files manifest)
                       (vconcat (alist-get 'compiler_files manifest)
                                (vector (vector "tools/ai/wasm-target-alloc.el"
                                                (wasm-rootstack--hash "tools/ai/wasm-target-alloc.el")))))
                 "Unexpected compiler child load tools/ai/wasm-target-alloc.el")
                ((or 'compiler-unlisted-lisp 'compiler-src)
                 (let ((file (if (eq control 'compiler-unlisted-lisp)
                                 "lisp/nelisp-cc-alloc-mem.el" "src/nelisp-allocator.el")))
                   (setf (alist-get 'compiler_files manifest)
                         (vconcat (alist-get 'compiler_files manifest)
                                  (vector (vector file (wasm-rootstack--hash file)))))
                   (concat "Unexpected compiler child load " file)))
                ('compiler-installation-root
                 (setf (alist-get 'compiler_installation_root manifest) "/")
                 "compiler installation root")
                ('compiler-file-duplicate
                 (setf (alist-get 'compiler_files manifest)
                       (vconcat (alist-get 'compiler_files manifest)
                                (vector (aref (alist-get 'compiler_files manifest) 0))))
                 "duplicate compiler loaded file"))))
        (with-temp-file file (insert (json-encode manifest)))
        (let ((result (wasm-target-alloc-test-node path file)))
          (should (equal (car result) 1))
          (should (string-match-p (regexp-quote message) (cdr result)))
          (princ (format "control %s: %s" control (cdr result))))))
    (dolist (field '(size total))
      (let* ((manifest (json-read-file (concat path ".json")))
             (cases (alist-get 'allocations manifest))
             (file (format "target/wasm-target-alloc/edit-%s.json" field)))
        (setf (alist-get field (aref cases 0)) "999")
        (with-temp-file file (insert (json-encode manifest)))
        (let ((result (wasm-target-alloc-test-node path file)))
          (should (equal (car result) 1))
          (should (string-match-p "fixed allocation cases and genuine totals" (cdr result)))
          (princ (format "control manifest-%s: %s" field (cdr result))))))
    (dolist (control '((body-mutation . "allocation 0 header")
                       (skip-init . "init returned nonzero base")
                       (no-fail . "W0 failure observed")))
      (let* ((mode (car control))
             (path (format "target/wasm-target-alloc/%s.wasm" mode)))
        (wasm-target-alloc-test-corrupt-module path mode)
        (let ((result (wasm-target-alloc-test-node path)))
          (should (equal (car result) 1))
          (should (string-match-p (regexp-quote (cdr control)) (cdr result)))
          (princ (format "control %s: %s" mode (cdr result))))))
    (let* ((manifest (json-read-file (concat path ".json")))
           (altered "target/wasm-target-alloc/body-mutation.wasm")
           (mismatch "target/wasm-target-alloc/source-module-mismatch.wasm")
           (altered-hash (wasm-rootstack--hash altered)))
      (copy-file altered mismatch t)
      (setf (alist-get 'wasm_sha256 manifest) altered-hash)
      (with-temp-file (concat mismatch ".json") (insert (json-encode manifest)))
      (let ((err (should-error (wasm-target-alloc-test-provenance mismatch) :type 'error)))
        ;; This must reach the recompiled module equality, after all oracle,
        ;; row and source checks. The real mutated module hash is its operand.
        (should (string-prefix-p "Recompiled module identity " (cadr err)))
        (should (string-match-p (regexp-quote altered-hash) (cadr err)))
        (princ "control source-module-mismatch: recompiled module identity rejected\n")))))
(provide 'wasm-target-alloc-test)
