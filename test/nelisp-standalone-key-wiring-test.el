;;; nelisp-standalone-key-wiring-test.el --- unit keys follow the closure digest -*- lexical-binding: t; -*-

;; Loads the real build script and computes `nelisp-standalone--unit-cache-key'
;; with `--this-file' pointing at an edited temp copy.  No unit is compiled.
;; Run: emacs --batch -Q -L scripts -L lisp -L src -l test/nelisp-standalone-key-wiring-test.el -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'nelisp-standalone-build)

(defconst nl-kw--src nelisp-standalone--this-file)

(defun nl-kw--key (edits &optional source)
  "Unit key of SOURCE with the script text edited by EDITS ((FROM . TO), once each)."
  (let ((tmp (make-temp-file "nl-kw" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert-file-contents nl-kw--src)
            (dolist (e edits)
              (goto-char (point-min))
              (should (search-forward (car e) nil t))
              (should-not (search-forward (car e) nil t))
              (replace-match (cdr e) t t nil)))
          ;; The temp copy must sit next to the libraries for its `require' scan.
          (let ((nelisp-standalone--this-file tmp)
                (nelisp-standalone--toolchain-digest nil)
                (nelisp-standalone--key-closure nil))
            (nelisp-standalone--unit-cache-key (or source '(seq (defun probe_a (x) x))))))
      (delete-file tmp))))

(ert-deftest nl-kw-comment-edit-keeps-every-key ()
  (let ((base (nl-kw--key nil)))
    (should (equal base (nl-kw--key '(("(defun nelisp-standalone--unit-cache-key (source)"
                                       . ";; a comment\n\n(defun nelisp-standalone--unit-cache-key   (source)")))))
    ;; Negative controls: a post-key compiler def edit changes the key; so does
    ;; FULL_KEY=1 for the same comment-only edit (global key hashes the file).
    (should-not (equal base (nl-kw--key '(("(defun nelisp-standalone--xinline-enabled-p ()"
                                           . "(defun nelisp-standalone--xinline-enabled-p () (ignore 1)")))))
    (let ((process-environment (cons "NELISP_STANDALONE_FULL_KEY=1" process-environment)))
      (should-not (equal (nl-kw--key nil)
                         (nl-kw--key '((";;; nelisp-standalone-build.el ends here" . ";; c\n;;; nelisp-standalone-build.el ends here"))))))))

(ert-deftest nl-kw-producer-edit-keeps-keys ()
  (let ((base (nl-kw--key nil)))
    (should (equal base (nl-kw--key '(("(defun nelisp-standalone--patch-binding-kinds (source)"
                                       . "(defun nelisp-standalone--patch-binding-kinds (source) (ignore 42)")))))))

(ert-deftest nl-kw-opaque-form-takes-global-key ()
  (let ((base (nl-kw--key nil))
        (msgs nil))
    (cl-letf (((symbol-function 'message) (lambda (f &rest a) (push (apply #'format f a) msgs))))
      ;; An unlisted intern in a closure def: fallback fires, so ANY edit rekeys.
      (let ((k1 (nl-kw--key '(("(defun nelisp-standalone--xinline-enabled-p ()"
                               . "(defun nelisp-standalone--xinline-enabled-p () (intern \"x\")"))))
            (k2 (nl-kw--key '(("(defun nelisp-standalone--xinline-enabled-p ()"
                               . "(defun nelisp-standalone--xinline-enabled-p () (intern \"x\")")
                              ("(defun nelisp-standalone--patch-binding-kinds (source)"
                               . "(defun nelisp-standalone--patch-binding-kinds (source) (ignore 7)")))))
        (should-not (equal k1 base))
        (should-not (equal k1 k2))))
    (should (seq-some (lambda (m) (string-match-p "global key" m)) msgs))))

(ert-deftest nl-kw-dynamic-binding-values-enter-the-key ()
  (let ((src '(seq (defun probe_a (x) x)))
        (nelisp-standalone--toolchain-digest "fixed"))
    (let ((a (let ((nelisp-aot-compiler--allow-external-user-calls nil)) (nelisp-standalone--unit-cache-key src)))
          (b (let ((nelisp-aot-compiler--allow-external-user-calls t)) (nelisp-standalone--unit-cache-key src)))
          (c (let ((nelisp-aot-compiler--x86-private-continuations (not nelisp-aot-compiler--x86-private-continuations)))
               (nelisp-standalone--unit-cache-key src))))
      (should-not (equal a b))
      (should-not (equal a c))
      ;; Negative control: an unrelated binding does not move the key.
      (should (equal a (let ((nelisp-aot-compiler--allow-external-user-calls nil)
                             (nelisp-standalone--recompiled '(x)))
                         (nelisp-standalone--unit-cache-key src)))))))

(ert-deftest nl-kw-form-level-hash-reflow-vs-constant ()
  "B: comment, blank-line and reflow edits of closure defuns keep every key; a constant does not."
  (let ((base (nl-kw--key nil)))
    (should (equal base (nl-kw--key '(("(defun nelisp-standalone--xinline-enabled-p ()\n"
                                       . "(defun nelisp-standalone--xinline-enabled-p\n    ()   ;; reflowed\n\n\n")
                                      ("(defun nelisp-standalone--unit-cache-key (source)"
                                       . "(defun nelisp-standalone--unit-cache-key (source) ;; trailing   \n")))))
    (should-not (equal base (nl-kw--key '(("(format \"inline=%s/%d\\0\"" . "(format \"inline2=%s/%d\\0\"")))))))

(ert-deftest nl-kw-new-driver-bound-variable-refuses-narrow-key ()
  "F5: a let-bound variable the closure reads, absent from the key list, takes the global key."
  (let* ((msgs nil)
         (run (lambda (driver)
                (setq msgs nil)
                (cl-letf (((symbol-function 'message) (lambda (f &rest a) (push (apply #'format f a) msgs))))
                  (nl-kw--key
                   (list (cons "(defun nelisp-standalone--xinline-enabled-p ()"
                               (concat "(defun nelisp-standalone--zz-driver () " driver ")\n"
                                       "(defun nelisp-standalone--xinline-enabled-p () nelisp-aot-compiler--zz-new-var"))))))))
    (funcall run "(let ((nelisp-aot-compiler--zz-new-var 1)) 1)")
    (should (seq-some (lambda (m) (string-match-p "unlisted driver-bound variable" m)) msgs))
    ;; Negative control: the driver does not bind the variable, narrow key stays.
    (funcall run "1")
    (should-not (seq-some (lambda (m) (string-match-p "global key" m)) msgs))))


(defmacro nl-kw--with-cold-root (&rest body)
  "Run BODY with a tiny temp repo root, a fresh bake memo and a fixed revision.
Does not bind the memo to a literal, so the real digest path runs."
  `(let* ((dir (make-temp-file "nl-cold" t))
          (root (expand-file-name "root" dir))
          (bin (expand-file-name "nelisp" dir))
          (nelisp-standalone--repo-root root)
          (nelisp-standalone--bake-input-memo nil)
          (nelisp-standalone--build-source-revision "rev-1")
          (nelisp-standalone--native-cache-source-receipt nil))
     (make-directory (expand-file-name "lisp" root) t)
     (make-directory (expand-file-name "scripts" root) t)
     (make-directory (expand-file-name "target" root) t)
     (with-temp-file (expand-file-name "lisp/a.el" root) (insert "(a 1)"))
     (unwind-protect (progn ,@body) (delete-directory dir t))))

(defun nl-kw--write (root rel text)
  (with-temp-file (expand-file-name rel root) (insert text)))

(ert-deftest nl-kw-cold-image-early-cutoff ()
  "Identical binary + bake inputs + flags skip the dump; every other case re-bakes.
Runs the real bake digest (no memo bound), so a broken accessor fails here."
  (nl-kw--with-cold-root
   (let ((dumps 0))
     (cl-letf (((symbol-function 'nelisp-standalone--cold-image-path) (lambda (b) (concat b ".cold")))
               ((symbol-function 'nelisp-standalone--build-cold-image-2)
                (lambda (_b image) (cl-incf dumps) (with-temp-file image (insert "img")))))
       (with-temp-file bin (insert "bin-1"))
       (nelisp-standalone--build-cold-image bin)
       (should (= dumps 1))
       (setq nelisp-standalone--bake-input-memo nil)     ; fresh process
       (nelisp-standalone--build-cold-image bin)
       (should (= dumps 1))                              ; skipped
       (nl-kw--write root "lisp/a.el" "(a 2)")           ; bake input changed
       (setq nelisp-standalone--bake-input-memo nil)
       (nelisp-standalone--build-cold-image bin) (should (= dumps 2))
       (let ((process-environment (cons "NELISP_STANDALONE_NATIVE_COMPILER_COLD=1" process-environment)))
         (setq nelisp-standalone--bake-input-memo nil)
         (nelisp-standalone--build-cold-image bin) (should (= dumps 3)))   ; flag
       (setq nelisp-standalone--bake-input-memo nil)
       (with-temp-file bin (insert "bin-2"))              ; binary bytes changed
       (nelisp-standalone--build-cold-image bin) (should (= dumps 4))
       (nelisp-standalone--build-cold-image bin) (should (= dumps 4))
       (delete-file (concat bin ".cold.digest"))          ; sidecar digest missing
       (nelisp-standalone--build-cold-image bin) (should (= dumps 5))))))

(ert-deftest nl-kw-cold-skip-key-failure-does-not-skip-and-is-logged ()
  "A digest failure must mean re-dump, with a log line (no ignore-errors)."
  (nl-kw--with-cold-root
   (let ((dumps 0) (msgs nil))
     (cl-letf (((symbol-function 'nelisp-standalone--cold-image-path) (lambda (b) (concat b ".cold")))
               ((symbol-function 'nelisp-standalone--cold-skip-key) (lambda (_b) (error "boom")))
               ((symbol-function 'message) (lambda (f &rest a) (push (apply #'format f a) msgs)))
               ((symbol-function 'nelisp-standalone--build-cold-image-2)
                (lambda (_b image) (cl-incf dumps) (with-temp-file image (insert "img")))))
       (with-temp-file bin (insert "bin-1"))
       (nelisp-standalone--build-cold-image bin)
       (nelisp-standalone--build-cold-image bin)
       (should (= dumps 2))
       (should (seq-some (lambda (m) (string-match-p "skip key unavailable" m)) msgs))))))

(provide 'nelisp-standalone-key-wiring-test)

(ert-deftest nl-kw-install-linked-binary-cutoff ()
  "Equal bytes keep the old binary (mtime and inode); a difference or absence installs."
  (let* ((dir (make-temp-file "nlkw" t)) (final (expand-file-name "nelisp" dir))
         (new (concat final ".new")) (coding-system-for-write 'binary))
    (unwind-protect
        (progn
          (write-region "BIN-1" nil final)
          (set-file-times final (seconds-to-time 1000))
          (write-region "BIN-1" nil new)
          (should (equal (nelisp-standalone--install-linked-binary new final) final))
          (should-not (file-exists-p new))
          (should (= 1000 (floor (float-time (file-attribute-modification-time (file-attributes final))))))
          ;; Negative control: a different binary replaces the old one.
          (write-region "BIN-2" nil new)
          (nelisp-standalone--install-linked-binary new final)
          (should-not (file-exists-p new))
          (should (equal (with-temp-buffer (insert-file-contents final) (buffer-string)) "BIN-2"))
          (should (> (float-time (file-attribute-modification-time (file-attributes final))) 2000))
          ;; Absent old binary: installed.
          (delete-file final)
          (write-region "BIN-3" nil new)
          (nelisp-standalone--install-linked-binary new final)
          (should (file-exists-p final)))
      (delete-directory dir t))))
