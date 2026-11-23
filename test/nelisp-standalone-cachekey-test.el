;;; nelisp-standalone-cachekey-test.el --- incremental build key tests -*- lexical-binding: t; -*-

;; Unit-level tests for the standalone unit-cache key, entry verification
;; and reproducible-identity helpers.  No build is run.
;; Run: emacs --batch -Q -L scripts -L lisp -L src -l test/nelisp-standalone-cachekey-test.el -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'nelisp-version)
(require 'nelisp-standalone-cachekey)

(defconst nelisp-cachekey-test--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name)))))

(defmacro nelisp-cachekey-test--with-env (bindings &rest body)
  "Run BODY with `process-environment' extended by BINDINGS (VAR . VALUE or VAR)."
  `(let ((process-environment
          (append (mapcar (lambda (b) (if (consp b) (format "%s=%s" (car b) (cdr b)) b))
                          ',bindings)
                  process-environment)))
     ,@body))

(ert-deftest nelisp-cachekey-version-pinned-ignores-git ()
  (nelisp-cachekey-test--with-env
      ((NELISP_REPRODUCIBLE_BUILD . 1) (NELISP_VERSION . "pin-1")
       (NELISP_SOURCE_REVISION . "0123456789abcdef0123456789abcdef01234567"))
    (should (equal (nelisp-build-version nelisp-cachekey-test--root) "pin-1"))
    (should (equal (nelisp-build-source-revision nelisp-cachekey-test--root)
                   "0123456789abcdef0123456789abcdef01234567"))))

(ert-deftest nelisp-cachekey-version-pinned-no-describe ()
  "Without NELISP_VERSION the pinned build still never calls git describe."
  (nelisp-cachekey-test--with-env
      ((NELISP_REPRODUCIBLE_BUILD . 1) "NELISP_VERSION="
       (NELISP_SOURCE_REVISION . "0123456789abcdef0123456789abcdef01234567"))
    (should (equal (nelisp-build-version nelisp-cachekey-test--root)
                   "unknown-0123456789abcdef0123456789abcdef01234567"))))

(ert-deftest nelisp-cachekey-version-unpinned-negative-control ()
  "Negative control: without the flag the source revision is the git HEAD."
  (skip-unless (executable-find "git"))
  (nelisp-cachekey-test--with-env
      ("NELISP_REPRODUCIBLE_BUILD=" (NELISP_SOURCE_REVISION . "0123456789abcdef0123456789abcdef01234567"))
    (let ((rev (nelisp-build-source-revision nelisp-cachekey-test--root)))
      (skip-unless rev)
      (should-not (equal rev "0123456789abcdef0123456789abcdef01234567")))))

;;;; Step 3: key completeness.

(defun nelisp-cachekey-test--real-closure ()
  (let ((load-path (mapcar (lambda (d) (expand-file-name d nelisp-cachekey-test--root))
                           '("lisp" "src" "scripts"))))
    (nelisp-standalone-cachekey-require-closure
     nelisp-standalone-cachekey-root-features nelisp-cachekey-test--root load-path)))

(defun nelisp-cachekey-test--names (files) (mapcar #'file-name-nondirectory files))

(ert-deftest nelisp-cachekey-closure-has-former-holes ()
  (let ((names (nelisp-cachekey-test--names (nelisp-cachekey-test--real-closure))))
    (dolist (n '("nelisp-asm-x86_64.el" "nelisp-asm-arm64.el" "nelisp-sexp-layout.el"
                 "nelisp-cc-fxir-rows.el" "nelisp-cc-fxir-effects.el"
                 "nelisp-aot-compiler.el" "nelisp-cc-runtime.el"))
      (should (member n names)))
    ;; Only Emacs stdlib features may stay unresolved (they live outside the repo).
    (should-not (seq-some (lambda (n) (string-prefix-p "?nelisp" n)) names))))

(ert-deftest nelisp-cachekey-build-script-requires-are-roots ()
  "Libraries required by the build script itself are in the toolchain closure."
  (let* ((bf (expand-file-name "scripts/nelisp-standalone-build.el" nelisp-cachekey-test--root))
         (feats (nelisp-standalone-cachekey-file-requires bf))
         (load-path (mapcar (lambda (d) (expand-file-name d nelisp-cachekey-test--root))
                            '("lisp" "src" "scripts")))
         (with (nelisp-cachekey-test--names
                (nelisp-standalone-cachekey-require-closure
                 (append nelisp-standalone-cachekey-root-features feats)
                 nelisp-cachekey-test--root load-path)))
         (without (nelisp-cachekey-test--names (nelisp-cachekey-test--real-closure))))
    (should (member "nelisp-cc-sort" feats))
    (dolist (n '("nelisp-cc-sort.el" "nelisp-cc-string-blocks.el" "nelisp-cc-string-compare.el"))
      (should (member n with)))
    ;; Negative control: the root list alone does not reach them.
    (should-not (member "nelisp-cc-sort.el" without))
    ;; The build script's own requires never leave a repository feature unresolved.
    (should-not (seq-some (lambda (n) (string-prefix-p "?nelisp" n)) with))))

(ert-deftest nelisp-cachekey-closure-has-no-opaque-deps ()
  "The closure uses only `(require 'x)', which the digest follows."
  (let ((hits (nelisp-standalone-cachekey-opaque-deps (nelisp-cachekey-test--real-closure))))
    ;; The one allowed hit emits a require form inside quoted generated code.
    (should (equal (seq-remove (lambda (h) (string-prefix-p "nelisp-aot-compiler.el:" h)) hits)
                   nil))
    (should (<= (length hits) 1))))

(ert-deftest nelisp-cachekey-opaque-deps-detects-forms ()
  "Negative control: each form the scan cannot follow is reported."
  (nelisp-cachekey-test--with-tree
   (dolist (form '("(load \"data.el\")" "(require (quote b))" "(require feat)"
                   "(autoload 'foo \"b\")"))
     (with-temp-file (expand-file-name "lisp/a.el" dir) (insert form "\n"))
     (should (nelisp-standalone-cachekey-opaque-deps (list (expand-file-name "lisp/a.el" dir)))))
   ;; The plain quoted require is fine.
   (with-temp-file (expand-file-name "lisp/a.el" dir) (insert "(require 'b)\n"))
   (should-not (nelisp-standalone-cachekey-opaque-deps (list (expand-file-name "lisp/a.el" dir))))))

(ert-deftest nelisp-cachekey-old-dep-list-misses-the-assembler ()
  "Negative control: the pre-audit list (8 libraries, no closure) lacks them."
  (let* ((load-path (mapcar (lambda (d) (expand-file-name d nelisp-cachekey-test--root))
                            '("lisp" "src" "scripts")))
         (old (delq nil (mapcar #'locate-library
                                '("nelisp-aot-compiler" "nelisp-standalone-arena-rewrite"
                                  "nelisp-static-linker" "nelisp-elf-write" "nelisp-cc-runtime"
                                  "nelisp-standalone-argframe.el" "nelisp-heap-accessors"
                                  "nelisp-cc-fxir-native")))))
    (should-not (member "nelisp-asm-x86_64.el" (nelisp-cachekey-test--names old)))
    (should-not (member "nelisp-sexp-layout.el" (nelisp-cachekey-test--names old)))
    (should-not (member "nelisp-cc-fxir-rows.el" (nelisp-cachekey-test--names old)))))

(defmacro nelisp-cachekey-test--with-tree (&rest body)
  "Run BODY with `dir' bound to a temp repo: lisp/a.el requires b, c (c is .elc-shadowed)."
  `(let* ((dir (make-temp-file "ck-tree-" t))
          (lisp (expand-file-name "lisp" dir)))
     (unwind-protect
         (progn
           (make-directory lisp)
           (with-temp-file (expand-file-name "a.el" lisp) (insert "(require 'b)\n(defun a () 1)\n"))
           (with-temp-file (expand-file-name "b.el" lisp) (insert "(defun b () 1)\n"))
           (with-temp-file (expand-file-name "c.el" lisp) (insert "(defun c () 1)\n"))
           (with-temp-file (expand-file-name "c.elc" lisp) (insert "stale-bytecode"))
           ,@body)
       (delete-directory dir t))))

(defun nelisp-cachekey-test--digest (dir roots)
  (let ((load-path (list (expand-file-name "lisp" dir))))
    (nelisp-standalone-cachekey-files-digest
     (nelisp-standalone-cachekey-require-closure roots dir load-path))))

(ert-deftest nelisp-cachekey-transitive-edit-changes-digest ()
  (nelisp-cachekey-test--with-tree
   (let ((d0 (nelisp-cachekey-test--digest dir '("a"))))
     (should (equal d0 (nelisp-cachekey-test--digest dir '("a"))))
     (with-temp-file (expand-file-name "lisp/b.el" dir) (insert "(defun b () 2)\n"))
     (should-not (equal d0 (nelisp-cachekey-test--digest dir '("a")))))))

(ert-deftest nelisp-cachekey-hashes-el-not-elc ()
  (nelisp-cachekey-test--with-tree
   (let ((d0 (nelisp-cachekey-test--digest dir '("c"))))
     ;; Editing the .el the loader runs must re-key even though a .elc exists.
     (with-temp-file (expand-file-name "lisp/c.el" dir) (insert "(defun c () 2)\n"))
     (should-not (equal d0 (nelisp-cachekey-test--digest dir '("c"))))
     ;; Negative control: touching only the stale .elc must NOT re-key.
     (let ((d1 (nelisp-cachekey-test--digest dir '("c"))))
       (with-temp-file (expand-file-name "lisp/c.elc" dir) (insert "other-stale-bytecode"))
       (should (equal d1 (nelisp-cachekey-test--digest dir '("c"))))))))

(ert-deftest nelisp-cachekey-missing-feature-is-recorded ()
  (nelisp-cachekey-test--with-tree
   (should (member "?nope" (let ((load-path (list (expand-file-name "lisp" dir))))
                             (nelisp-standalone-cachekey-require-closure '("nope") dir load-path))))))

(ert-deftest nelisp-cachekey-refuses-incomplete-key ()
  "An unresolved nelisp feature or unavailable native-load must not be cached."
  (should-error (nelisp-standalone-cachekey-check-files '("/x/a.el" "?nelisp-gone")))
  (should-error (nelisp-standalone-cachekey-check-settings
                 (nelisp-standalone-cachekey-settings-string
                  '(("k" . :native-load-unavailable)))))
  ;; Negative controls: a complete closure (stdlib gaps tolerated) and normal
  ;; settings pass, and the explicit opt-out restores the old behaviour.
  (should (equal (nelisp-standalone-cachekey-check-files '("/x/a.el" "?cl-lib")) '("/x/a.el" "?cl-lib")))
  (should (stringp (nelisp-standalone-cachekey-check-settings "settings=((a . 1))")))
  (let ((process-environment (cons "NELISP_STANDALONE_UNIT_CACHE_ALLOW_INCOMPLETE=1"
                                   process-environment)))
    (should (nelisp-standalone-cachekey-check-files '("?nelisp-gone")))
    (should (nelisp-standalone-cachekey-check-settings ":native-load-unavailable"))))

(ert-deftest nelisp-cachekey-settings-follow-env ()
  (let ((base (nelisp-standalone-cachekey-settings-string (nelisp-standalone-cachekey-env-alist))))
    (dolist (var '("NELISP_STACKMAP" "NELISP_ALIGNHOT_PERTURB" "NC2_LAZY" "NELISP_LINUX_ARENA_SIZE"))
      (should (member var nelisp-standalone-cachekey-env-names))
      (let ((process-environment (cons (format "%s=1" var) process-environment)))
        (should-not (equal base (nelisp-standalone-cachekey-settings-string
                                 (nelisp-standalone-cachekey-env-alist))))))
    ;; Negative control: an unrelated variable does not move the key.
    (let ((process-environment (cons "UNRELATED_VAR=1" process-environment)))
      (should (equal base (nelisp-standalone-cachekey-settings-string
                           (nelisp-standalone-cachekey-env-alist)))))
    ;; Host Emacs version and live settings are part of the text.
    (should (string-match-p (regexp-quote emacs-version) base))
    (should-not (equal base (nelisp-standalone-cachekey-settings-string
                             (nelisp-standalone-cachekey-env-alist '((regalloc . t))))))))

;;;; Step 4: self-verifying entries.

(defmacro nelisp-cachekey-test--with-dir (&rest body)
  `(let ((dir (make-temp-file "ck-cache-" t)))
     (unwind-protect (progn ,@body) (delete-directory dir t))))

(defconst nelisp-cachekey-test--key "0123456789abcdef0123456789abcdef01234567")
(defconst nelisp-cachekey-test--key2 "fedcba9876543210fedcba9876543210fedcba98")

(defun nelisp-cachekey-test--slurp (f)
  (with-temp-buffer (let ((coding-system-for-read 'utf-8-unix)) (insert-file-contents f)) (buffer-string)))
(defun nelisp-cachekey-test--spit (f text)
  (let ((coding-system-for-write 'utf-8-unix)) (with-temp-file f (insert text))))

(ert-deftest nelisp-cachekey-entry-roundtrip ()
  (nelisp-cachekey-test--with-dir
   (let ((f (expand-file-name "u.o.K.unit" dir)))
     (nelisp-standalone-cachekey-entry-write f nelisp-cachekey-test--key "(:sections \"a\\n\")")
     (should (equal (nelisp-standalone-cachekey-entry-read f nelisp-cachekey-test--key)
                    '(t . "(:sections \"a\\n\")")))
     ;; No temp file is left behind.
     (should (equal (directory-files dir nil "\\.tmp\\'") nil)))))

(ert-deftest nelisp-cachekey-entry-rejects-corruption ()
  (nelisp-cachekey-test--with-dir
   (let ((f (expand-file-name "u.unit" dir)) (k nelisp-cachekey-test--key))
     (nelisp-standalone-cachekey-entry-write f k "payload-1234")
     (let ((good (nelisp-cachekey-test--slurp f)))
       ;; Same size, one byte flipped: caught by the sha256, not the size.
       (nelisp-cachekey-test--spit f (replace-regexp-in-string "payload-1234" "payload-1235" good))
       (should (equal (nelisp-standalone-cachekey-entry-read f k) '(nil . sha)))
       ;; Truncated payload.
       (nelisp-cachekey-test--spit f (substring good 0 (- (length good) 3)))
       (should (equal (nelisp-standalone-cachekey-entry-read f k) '(nil . size)))
       ;; Header-less (old format) file.
       (nelisp-cachekey-test--spit f "(:sections nil)")
       (should (equal (nelisp-standalone-cachekey-entry-read f k) '(nil . header)))
       ;; Negative control: the intact entry still verifies.
       (nelisp-cachekey-test--spit f good)
       (should (car (nelisp-standalone-cachekey-entry-read f k)))
       ;; Missing file (another lane's prune): a miss, not an error.
       (should (equal (nelisp-standalone-cachekey-entry-read (expand-file-name "none" dir) k)
                      '(nil . missing)))))))

(ert-deftest nelisp-cachekey-entry-rejects-size-header-truncation ()
  "Each rejection reason is reachable on its own, with an intact control."
  (nelisp-cachekey-test--with-dir
   (let* ((f (expand-file-name "u.unit" dir)) (k nelisp-cachekey-test--key)
          (text (nelisp-standalone-cachekey-entry-text k "payload-abcdef"))
          (nl (string-search "\n" text)))
     (nelisp-cachekey-test--spit f text)
     (should (equal (nelisp-standalone-cachekey-entry-read f k) '(t . "payload-abcdef")))
     ;; size: header byte count disagrees, the payload itself (and its sha) is intact.
     (nelisp-cachekey-test--spit
      f (replace-regexp-in-string "bytes=14" "bytes=15" text))
     (should (equal (nelisp-standalone-cachekey-entry-read f k) '(nil . size)))
     ;; size: payload cut short while the header is whole.
     (nelisp-cachekey-test--spit f (substring text 0 (- (length text) 4)))
     (should (equal (nelisp-standalone-cachekey-entry-read f k) '(nil . size)))
     ;; header: file cut inside the header line (no newline yet).
     (nelisp-cachekey-test--spit f (substring text 0 (- nl 5)))
     (should (equal (nelisp-standalone-cachekey-entry-read f k) '(nil . header)))
     ;; header: empty file (a crash before the first write reached disk).
     (nelisp-cachekey-test--spit f "")
     (should (equal (nelisp-standalone-cachekey-entry-read f k) '(nil . header)))
     ;; header: right size of garbage.
     (nelisp-cachekey-test--spit f (make-string (length text) ?x))
     (should (equal (nelisp-standalone-cachekey-entry-read f k) '(nil . header)))
     ;; Negative control: restoring the intact text verifies again.
     (nelisp-cachekey-test--spit f text)
     (should (car (nelisp-standalone-cachekey-entry-read f k))))))

(ert-deftest nelisp-cachekey-entry-rejects-poisoned-key ()
  "An entry copied under another key's file name (the stale-hit failure) is refused."
  (nelisp-cachekey-test--with-dir
   (let ((old (expand-file-name "u.OLD.unit" dir)) (new (expand-file-name "u.NEW.unit" dir)))
     (nelisp-standalone-cachekey-entry-write old nelisp-cachekey-test--key "old-code")
     (copy-file old new)
     (should (equal (nelisp-standalone-cachekey-entry-read new nelisp-cachekey-test--key2)
                    '(nil . key)))
     ;; Negative control: with verification off the poison is accepted, which
     ;; is what the full-build gate uses to prove it can go RED.
     (let ((process-environment (cons "NELISP_STANDALONE_UNIT_CACHE_VERIFY=0" process-environment)))
       (should (equal (nelisp-standalone-cachekey-entry-read new nelisp-cachekey-test--key2)
                      '(t . "old-code")))))))

(ert-deftest nelisp-cachekey-touch-refreshes-mtime ()
  "A hit moves the mtime forward, so nl-cache-gc.sh keeps a hot entry."
  (nelisp-cachekey-test--with-dir
   (let ((f (expand-file-name "u.unit" dir))
         (old (time-subtract (current-time) (* 40 86400))))
     (nelisp-standalone-cachekey-entry-write f nelisp-cachekey-test--key "p")
     (set-file-times f old)
     ;; Negative control: reading alone leaves it 40 days old.
     (nelisp-standalone-cachekey-entry-read f nelisp-cachekey-test--key)
     (should (> (float-time (time-subtract (current-time)
                                           (file-attribute-modification-time (file-attributes f))))
                (* 30 86400)))
     (nelisp-standalone-cachekey-touch f)
     (should (< (float-time (time-subtract (current-time)
                                           (file-attribute-modification-time (file-attributes f))))
                60))
     ;; A vanished file is not an error.
     (should-not (nelisp-standalone-cachekey-touch (expand-file-name "none" dir))))))

(ert-deftest nelisp-cachekey-atomic-write-unique-temp-names ()
  (nelisp-cachekey-test--with-dir
   (let ((f (expand-file-name "x.unit" dir)) (names nil))
     (cl-letf (((symbol-function 'rename-file)
                (lambda (from to &rest _) (push from names) (copy-file from to t) (delete-file from))))
       (dotimes (_ 20) (nelisp-standalone-cachekey-atomic-write f "t")))
     ;; pid + random bits: 20 writes use (almost surely) distinct names.
     (should (> (length (delete-dups (copy-sequence names))) 15))
     (should (cl-every (lambda (n) (string-match-p (format "\\.%d\\.[0-9a-f]+\\.tmp\\'" (emacs-pid)) n)) names)))))

(ert-deftest nelisp-cachekey-prune-only-in-private-dirs ()
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "NELISP_STANDALONE_UNIT_CACHE_DIR" nil)
    (setenv "NELISP_STANDALONE_UNIT_CACHE_NOPRUNE" nil)
    (setenv "NELISP_STANDALONE_UNIT_CACHE_PRUNE" nil)
    (should (nelisp-standalone-cachekey-prune-p))
    (setenv "NELISP_STANDALONE_UNIT_CACHE_DIR" "/some/shared")
    (should-not (nelisp-standalone-cachekey-prune-p))
    (setenv "NELISP_STANDALONE_UNIT_CACHE_PRUNE" "1")
    (should (nelisp-standalone-cachekey-prune-p))
    (setenv "NELISP_STANDALONE_UNIT_CACHE_NOPRUNE" "1")
    (should-not (nelisp-standalone-cachekey-prune-p))))

(provide 'nelisp-standalone-cachekey-test)
;;; nelisp-standalone-cachekey-test.el ends here

;;;; Design2 A1: closure digest of the build script.

(defconst nelisp-cachekey-test--build
  (expand-file-name "scripts/nelisp-standalone-build.el" nelisp-cachekey-test--root))

(defun nelisp-cachekey-test--closure-after (edits)
  "Closure info of a temp copy of the build script after EDITS (list of (FROM . TO)).
Each FROM must occur exactly once: an edit that misses is a test error."
  (let ((tmp (make-temp-file "nl-closure" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert-file-contents nelisp-cachekey-test--build)
            (dolist (e edits)
              (goto-char (point-min))
              (should (search-forward (car e) nil t))
              (should-not (search-forward (car e) nil t))
              (replace-match (cdr e) t t nil)))
          (nelisp-standalone-cachekey-closure-compute tmp))
      (delete-file tmp))))

(defvar nelisp-cachekey-test--base-info nil)
(defun nelisp-cachekey-test--base ()
  (or nelisp-cachekey-test--base-info
      (setq nelisp-cachekey-test--base-info
            (nelisp-standalone-cachekey-closure-compute nelisp-cachekey-test--build))))

(ert-deftest nelisp-cachekey-closure-shape ()
  (let* ((i (nelisp-cachekey-test--base)) (defs (plist-get i :defs)))
    (should (plist-get i :digest))
    (should-not (plist-get i :opaque))
    (should (memq 'nelisp-standalone--compile-to-unit defs))
    (should (memq 'nelisp-standalone--unit-cache-key defs))
    (dolist (n '("applyfn-assemble" "glue-source" "manifest" "unit-for" "reader-units"
                 "unit-inline-imports" "xinline-prepare"))
      (should-not (memq (intern (concat "nelisp-standalone--" n)) defs)))
    ;; Size cap: a closure that swallows the producers is ~600 KB.
    (should (< (plist-get i :bytes) 150000))
    (should (< (length defs) 120))
    (should-not (nelisp-standalone-cachekey-closure-fallback-reason i))))

(ert-deftest nelisp-cachekey-closure-comment-whitespace-edit-keeps-digest ()
  (let ((base (plist-get (nelisp-cachekey-test--base) :digest)))
    (should (equal base (plist-get (nelisp-cachekey-test--closure-after
                                    '(("(defun nelisp-standalone--unit-cache-key (source)"
                                       . ";; edited comment\n\n(defun nelisp-standalone--unit-cache-key   (source)")))
                                   :digest)))
    ;; Negative controls: a body edit and a docstring edit both rekey.
    (should-not (equal base (plist-get (nelisp-cachekey-test--closure-after
                                        '(("(format \"jump-tables=%s\\0\"" . "(format \"jump-tables2=%s\\0\"")))
                                       :digest)))
    (should-not (equal base (plist-get (nelisp-cachekey-test--closure-after
                                        '(("can only make the key MORE discriminating" . "can only make the key more discriminating")))
                                       :digest)))))

(ert-deftest nelisp-cachekey-closure-producer-edit-keeps-digest ()
  (let ((base (plist-get (nelisp-cachekey-test--base) :digest)))
    (should (equal base (plist-get (nelisp-cachekey-test--closure-after
                                    '(("(defun nelisp-standalone--patch-binding-kinds (source)"
                                       . "(defun nelisp-standalone--patch-binding-kinds (source) (ignore 42)")))
                                   :digest)))
    ;; Negative control: the same kind of edit inside a post-key def rekeys.
    (should-not (equal base (plist-get (nelisp-cachekey-test--closure-after
                                        '(("(defun nelisp-standalone--xinline-enabled-p ()"
                                           . "(defun nelisp-standalone--xinline-enabled-p () (ignore 42)")))
                                       :digest)))
    ;; The top-level `setq' of a closure variable is part of C (t4).
    (should-not (equal base (plist-get (nelisp-cachekey-test--closure-after
                                        '(("(append nelisp-standalone--inline-boundaries nelisp-cc-fxir-native--boundaries))"
                                           . "(append nelisp-standalone--inline-boundaries nelisp-cc-fxir-native--boundaries '(zz)))")))
                                       :digest)))))

(ert-deftest nelisp-cachekey-closure-opaque-forms-fall-back ()
  (let ((base (nelisp-cachekey-test--base)))
    (should-not (nelisp-standalone-cachekey-closure-fallback-reason base))
    (dolist (edit '(("(defun nelisp-standalone--xinline-enabled-p ()" . "(defun nelisp-standalone--xinline-enabled-p () (intern \"x\")")
                    ("(defun nelisp-standalone--xinline-enabled-p ()" . "(defun nelisp-standalone--xinline-enabled-p () (getenv \"NELISP_UNLISTED\")")
                    ("(defun nelisp-standalone--xinline-enabled-p ()" . "(defun nelisp-standalone--xinline-enabled-p () (eval '(+ 1 2))")
                    ("(defun nelisp-standalone--xinline-enabled-p ()" . "(defun nelisp-standalone--xinline-enabled-p () (nelisp-argframe-lower nil)")))
      (let ((i (nelisp-cachekey-test--closure-after (list edit))))
        (should (plist-get i :opaque))
        (should-not (plist-get i :digest))
        (should (string-match-p "opaque" (nelisp-standalone-cachekey-closure-fallback-reason i)))))
    ;; Negative control: a listed env name is not opaque.
    (should (plist-get (nelisp-cachekey-test--closure-after
                        '(("(defun nelisp-standalone--xinline-enabled-p ()" . "(defun nelisp-standalone--xinline-enabled-p () (getenv \"NELISP_TCO\")")))
                       :digest))
    ;; FULL_KEY forces the fallback.
    (nelisp-cachekey-test--with-env ((NELISP_STANDALONE_FULL_KEY . 1))
      (should (nelisp-standalone-cachekey-closure-fallback-reason base)))))

(ert-deftest nelisp-cachekey-closure-writer-joins ()
  "A def outside the closure that mutates a closure variable joins it (t5)."
  (let* ((base (nelisp-cachekey-test--base))
         (add (lambda (mut)
                (nelisp-cachekey-test--closure-after
                 (list (cons "(defun nelisp-standalone--patch-binding-kinds (source)"
                             (format "(defun nelisp-standalone--zz-writer () %s)\n(defun nelisp-standalone--patch-binding-kinds (source)" mut)))))))
    (should-not (memq 'nelisp-standalone--zz-writer (plist-get base :defs)))
    (dolist (m '("(setq nelisp-standalone--xinline-table nil)"
                 "(push 1 nelisp-standalone--xinline-table)"
                 "(nreverse nelisp-standalone--xinline-table)"
                 "(cl-incf nelisp-standalone--xinline-table)"))
      (should (memq 'nelisp-standalone--zz-writer (plist-get (funcall add m) :defs))))
    ;; Negative control: reading, or touching a bookkeeping variable, does not join.
    (should-not (memq 'nelisp-standalone--zz-writer
                      (plist-get (funcall add "(car nelisp-standalone--xinline-table)") :defs)))
    (should-not (memq 'nelisp-standalone--zz-writer
                      (plist-get (funcall add "(push 1 nelisp-standalone--recompiled)") :defs)))))

(ert-deftest nelisp-cachekey-let-bound-vars-are-covered ()
  "F5: every variable a producer `let'-binds and the closure reads is in the key."
  (let ((lb (nelisp-standalone-cachekey-let-bound-vars nelisp-cachekey-test--build))
        (covered (append nelisp-standalone-cachekey-dynamic-vars nelisp-standalone-cachekey-printed-vars)))
    (should lb)
    (should-not (seq-remove (lambda (v) (memq v covered)) lb))
    ;; Negative control: an unlisted variable would be reported.
    (should (seq-remove (lambda (v) (memq v (remq (car lb) covered))) lb))))
