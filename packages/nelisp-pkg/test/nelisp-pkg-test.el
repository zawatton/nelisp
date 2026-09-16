;;; nelisp-pkg-test.el --- tests for nelisp-pkg -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The fixtures are built on disk rather than passed in as data, because
;; the part most likely to break is the reading: what counts as a
;; `require', what a manifest looks like, which directories are scanned.

;;; Code:

(require 'ert)
(require 'nelisp-pkg)

(defmacro nelisp-pkg-test--with-tree (spec &rest body)
  "Build a packages/ tree from SPEC in a temp dir and run BODY there.
SPEC is ((PACKAGE (FILE . CONTENT) ...) ...)."
  (declare (indent 1))
  `(let ((root (make-temp-file "nelisp-pkg-test" t)))
     (unwind-protect
         (let ((default-directory (file-name-as-directory root)))
           (dolist (package ,spec)
             (let ((dir (expand-file-name
                         (format "packages/%s/src" (car package)))))
               (make-directory dir t)
               (dolist (file (cdr package))
                 (let ((path (expand-file-name (car file)
                                               (if (string-suffix-p "manifest.el"
                                                                    (car file))
                                                   (expand-file-name
                                                    (format "packages/%s"
                                                            (car package)))
                                                 dir))))
                   (with-temp-file path (insert (cdr file)))))))
           ,@body)
       (delete-directory root t))))

(ert-deftest nelisp-pkg-reads-quoted-features ()
  (nelisp-pkg-test--with-tree
      '(("alpha" ("alpha.el" . "(require 'cl-lib)\n(provide 'alpha)\n")))
    (let ((package (car (nelisp-pkg-scan))))
      (should (equal (plist-get package :name) "alpha"))
      (should (memq 'alpha (plist-get package :provides)))
      (should (memq 'cl-lib (plist-get package :requires))))))

(ert-deftest nelisp-pkg-finds-requires-inside-forms ()
  "A require nested in `eval-when-compile' is still a dependency."
  (nelisp-pkg-test--with-tree
      '(("alpha" ("alpha.el" . "(eval-when-compile (require 'subr-x))\n(provide 'alpha)\n")))
    (should (memq 'subr-x (plist-get (car (nelisp-pkg-scan)) :requires)))))

(ert-deftest nelisp-pkg-survives-dotted-pairs-in-data ()
  "Alist literals are everywhere; a walker that assumes proper lists dies.

The first run against the real tree stopped on
`(let . bindings-and-body)' inside a defconst, with `Wrong type
argument: listp'.  None of the hand-written fixtures had a dotted pair
in them."
  (nelisp-pkg-test--with-tree
      '(("alpha" ("alpha.el" . "(defconst alpha-shapes '((let . body) (lambda . args)))\n(require 'beta)\n(provide 'alpha)\n")))
    (let ((package (car (nelisp-pkg-scan))))
      (should (memq 'beta (plist-get package :requires)))
      (should (memq 'alpha (plist-get package :provides))))))

(ert-deftest nelisp-pkg-ignores-computed-feature-names ()
  "A computed feature name is not a statically resolvable dependency."
  (nelisp-pkg-test--with-tree
      '(("alpha" ("alpha.el" . "(require feature-var)\n(provide 'alpha)\n")))
    (should (null (plist-get (car (nelisp-pkg-scan)) :requires)))))

(ert-deftest nelisp-pkg-derives-cross-package-edges ()
  (nelisp-pkg-test--with-tree
      '(("alpha" ("alpha.el" . "(require 'beta)\n(provide 'alpha)\n"))
        ("beta"  ("beta.el"  . "(provide 'beta)\n")))
    (let ((packages (nelisp-pkg-scan)))
      (should (equal (nelisp-pkg-edges packages) '(("alpha" . "beta")))))))

(ert-deftest nelisp-pkg-orders-dependencies-first ()
  (nelisp-pkg-test--with-tree
      '(("alpha" ("alpha.el" . "(require 'beta)\n(provide 'alpha)\n"))
        ("beta"  ("beta.el"  . "(require 'gamma)\n(provide 'beta)\n"))
        ("gamma" ("gamma.el" . "(provide 'gamma)\n")))
    (let ((resolution (nelisp-pkg-resolve (nelisp-pkg-scan))))
      (should (null (plist-get resolution :cycles)))
      (should (equal (plist-get resolution :order) '("gamma" "beta" "alpha"))))))

(ert-deftest nelisp-pkg-reports-a-cycle-instead-of-an-order ()
  "A ring of requires has no load order, so it must not produce one."
  (nelisp-pkg-test--with-tree
      '(("alpha" ("alpha.el" . "(require 'beta)\n(provide 'alpha)\n"))
        ("beta"  ("beta.el"  . "(require 'alpha)\n(provide 'beta)\n")))
    (let* ((packages (nelisp-pkg-scan))
           (resolution (nelisp-pkg-resolve packages)))
      (should (equal (sort (copy-sequence (plist-get resolution :cycles)) #'string<)
                     '("alpha" "beta")))
      (should (null (plist-get resolution :order)))
      (should (= 2 (length (nelisp-pkg-findings-of-kind
                            (nelisp-pkg-check packages)
                            'pkg-cycle)))))))

(ert-deftest nelisp-pkg-notices-an-undeclared-dependency ()
  (nelisp-pkg-test--with-tree
      '(("alpha" ("alpha.el" . "(require 'beta)\n(provide 'alpha)\n")
                 ("manifest.el" . "(:name \"alpha\" :version \"1.0\" :requires ())\n"))
        ("beta"  ("beta.el"  . "(provide 'beta)\n")))
    (let ((findings (nelisp-pkg-check (nelisp-pkg-scan))))
      (should (equal (plist-get (car (nelisp-pkg-findings-of-kind
                                      findings 'pkg-undeclared-dependency))
                                :dependency)
                     "beta")))))

(ert-deftest nelisp-pkg-notices-a-stale-dependency ()
  (nelisp-pkg-test--with-tree
      '(("alpha" ("alpha.el" . "(provide 'alpha)\n")
                 ("manifest.el" . "(:name \"alpha\" :version \"1.0\" :requires (\"beta\"))\n"))
        ("beta"  ("beta.el"  . "(provide 'beta)\n")))
    (let ((findings (nelisp-pkg-check (nelisp-pkg-scan))))
      (should (equal (plist-get (car (nelisp-pkg-findings-of-kind
                                      findings 'pkg-stale-dependency))
                                :dependency)
                     "beta")))))

(ert-deftest nelisp-pkg-accepts-a-declaration-that-matches ()
  (nelisp-pkg-test--with-tree
      '(("alpha" ("alpha.el" . "(require 'beta)\n(provide 'alpha)\n")
                 ("manifest.el" . "(:name \"alpha\" :version \"1.0\" :requires (\"beta\"))\n"))
        ("beta"  ("beta.el"  . "(provide 'beta)\n")
                 ("manifest.el" . "(:name \"beta\" :version \"1.0\" :requires ())\n")))
    (let ((findings (nelisp-pkg-check (nelisp-pkg-scan))))
      (should (null (nelisp-pkg-findings-of-kind findings 'pkg-undeclared-dependency)))
      (should (null (nelisp-pkg-findings-of-kind findings 'pkg-stale-dependency)))
      (should (null (nelisp-pkg-findings-of-kind findings 'pkg-missing-manifest))))))

(ert-deftest nelisp-pkg-load-sequence-puts-dependencies-first ()
  (nelisp-pkg-test--with-tree
      '(("alpha" ("alpha.el" . "(require 'beta)\n(provide 'alpha)\n"))
        ("beta"  ("beta.el"  . "(provide 'beta)\n")))
    (let ((sequence (mapcar #'file-name-nondirectory
                            (nelisp-pkg-load-sequence "alpha" (nelisp-pkg-scan)))))
      (should (equal sequence '("beta.el" "alpha.el"))))))

(ert-deftest nelisp-pkg-load-sequence-orders-files-inside-a-package ()
  "A package's own files are ordered by their intra-package requires."
  (nelisp-pkg-test--with-tree
      '(("alpha" ("a-top.el"  . "(require 'a-base)\n(provide 'a-top)\n")
                 ("a-base.el" . "(provide 'a-base)\n")))
    (let ((sequence (mapcar #'file-name-nondirectory
                            (nelisp-pkg-load-sequence "alpha" (nelisp-pkg-scan)))))
      ;; Alphabetically a-base precedes a-top, so make the dependency the
      ;; later name to prove the order comes from requires, not sorting.
      (should (equal sequence '("a-base.el" "a-top.el"))))))

(ert-deftest nelisp-pkg-load-sequence-orders-against-alphabetical ()
  (nelisp-pkg-test--with-tree
      '(("alpha" ("a-first.el" . "(require 'z-last)\n(provide 'a-first)\n")
                 ("z-last.el"  . "(provide 'z-last)\n")))
    (let ((sequence (mapcar #'file-name-nondirectory
                            (nelisp-pkg-load-sequence "alpha" (nelisp-pkg-scan)))))
      (should (equal sequence '("z-last.el" "a-first.el"))))))

(ert-deftest nelisp-pkg-load-sequence-is-transitive ()
  (nelisp-pkg-test--with-tree
      '(("alpha" ("alpha.el" . "(require 'beta)\n(provide 'alpha)\n"))
        ("beta"  ("beta.el"  . "(require 'gamma)\n(provide 'beta)\n"))
        ("gamma" ("gamma.el" . "(provide 'gamma)\n"))
        ("delta" ("delta.el" . "(provide 'delta)\n")))
    (let ((sequence (mapcar #'file-name-nondirectory
                            (nelisp-pkg-load-sequence "alpha" (nelisp-pkg-scan)))))
      (should (equal sequence '("gamma.el" "beta.el" "alpha.el")))
      ;; An unrelated package is not dragged in.
      (should-not (member "delta.el" sequence)))))

(ert-deftest nelisp-pkg-load-sequence-refuses-a-cycle ()
  (nelisp-pkg-test--with-tree
      '(("alpha" ("alpha.el" . "(require 'beta)\n(provide 'alpha)\n"))
        ("beta"  ("beta.el"  . "(require 'alpha)\n(provide 'beta)\n")))
    (should-error (nelisp-pkg-load-sequence "alpha" (nelisp-pkg-scan)))))

(ert-deftest nelisp-pkg-load-sequence-keeps-unorderable-files ()
  "A file whose intra-package requires cannot be met is still loaded.
Dropping it would move the failure somewhere else and make it harder to
read; the sequence is doubtful, not shorter."
  (nelisp-pkg-test--with-tree
      '(("alpha" ("one.el" . "(require 'two)\n(provide 'one)\n")
                 ("two.el" . "(require 'one)\n(provide 'two)\n")))
    (let ((sequence (nelisp-pkg-load-sequence "alpha" (nelisp-pkg-scan))))
      (should (= 2 (length sequence))))))

(defmacro nelisp-pkg-test--with-files (files &rest body)
  "Write FILES ((RELATIVE-PATH . CONTENT) ...) into a temp root, run BODY."
  (declare (indent 1))
  `(let ((root (make-temp-file "nelisp-pkg-list" t)))
     (unwind-protect
         (let ((default-directory (file-name-as-directory root)))
           (dolist (file ,files)
             (let ((path (expand-file-name (car file))))
               (make-directory (file-name-directory path) t)
               (with-temp-file path (insert (cdr file)))))
           ,@body)
       (delete-directory root t))))

(ert-deftest nelisp-pkg-load-list-flags-a-path-that-does-not-exist ()
  "The failure mode is silent at run time, so it must be loud here."
  (nelisp-pkg-test--with-files
      '(("smoke.el" . "(load \"src/gone.el\")\n"))
    (let ((result (nelisp-pkg-check-load-list (expand-file-name "smoke.el"))))
      (should (= 1 (length (plist-get result :findings))))
      (should (eq 'load-list-missing-file
                  (plist-get (car (plist-get result :findings)) :kind)))
      (should (= 0 (plist-get result :checked))))))

(ert-deftest nelisp-pkg-load-list-flags-a-file-loaded-too-early ()
  (nelisp-pkg-test--with-files
      '(("src/base.el" . "(provide 'base)\n")
        ("src/top.el"  . "(require 'base)\n(provide 'top)\n")
        ("smoke.el"    . "(load \"src/top.el\")\n(load \"src/base.el\")\n"))
    (let* ((result (nelisp-pkg-check-load-list (expand-file-name "smoke.el")))
           (finding (car (plist-get result :findings))))
      (should (eq 'load-list-out-of-order (plist-get finding :kind)))
      (should (eq 'base (plist-get finding :feature)))
      (should (= 2 (plist-get result :checked))))))

(ert-deftest nelisp-pkg-load-list-accepts-the-right-order ()
  (nelisp-pkg-test--with-files
      '(("src/base.el" . "(provide 'base)\n")
        ("src/top.el"  . "(require 'base)\n(provide 'top)\n")
        ("smoke.el"    . "(load \"src/base.el\")\n(load \"src/top.el\")\n"))
    (should (null (plist-get (nelisp-pkg-check-load-list
                              (expand-file-name "smoke.el"))
                             :findings)))))

(ert-deftest nelisp-pkg-load-list-ignores-unrelated-order ()
  "Two files with no dependency between them may be loaded either way.
Comparing against a derived global order instead would invent findings."
  (nelisp-pkg-test--with-files
      '(("src/one.el" . "(provide 'one)\n")
        ("src/two.el" . "(provide 'two)\n")
        ("smoke.el"   . "(load \"src/two.el\")\n(load \"src/one.el\")\n"))
    (should (null (plist-get (nelisp-pkg-check-load-list
                              (expand-file-name "smoke.el"))
                             :findings)))))

(ert-deftest nelisp-pkg-load-list-counts-a-computed-path ()
  "A list that cannot be read fully must not look fully checked."
  (nelisp-pkg-test--with-files
      '(("smoke.el" . "(load (concat dir \"x.el\"))\n"))
    (let ((result (nelisp-pkg-check-load-list (expand-file-name "smoke.el"))))
      (should (eq 'load-list-computed-path
                  (plist-get (car (plist-get result :findings)) :kind)))
      (should (= 0 (plist-get result :checked))))))

(ert-deftest nelisp-pkg-rejects-a-malformed-manifest ()
  (should (nelisp-pkg-validate-manifest '(:name "a")))
  (should (nelisp-pkg-validate-manifest '(:name a :version "1" :requires ())))
  (should (nelisp-pkg-validate-manifest '(:name "a" :version "1" :requires (b))))
  (should (null (nelisp-pkg-validate-manifest
                 '(:name "a" :version "1" :requires ("b"))))))

(ert-deftest nelisp-pkg-version-is-optional-but-typed ()
  "Nothing consumes a version yet, so requiring one would mean inventing
34 numbers.  A version that is present still has to be a string."
  (should (null (nelisp-pkg-validate-manifest '(:name "a" :requires ()))))
  (should (nelisp-pkg-validate-manifest '(:name "a" :version 1 :requires ()))))

(ert-deftest nelisp-pkg-stability-is-optional-but-one-of-four ()
  "Most manifests predate `:stability' and have none -- that is fine.
One that IS present must be a §9.2 label, not an arbitrary string."
  (should (null (nelisp-pkg-validate-manifest '(:name "a" :requires ()))))
  (should (null (nelisp-pkg-validate-manifest
                 '(:name "a" :stability "provisional" :requires ()))))
  (dolist (label '("stable" "provisional" "experimental" "internal"))
    (should (null (nelisp-pkg-validate-manifest
                   (list :name "a" :stability label :requires nil)))))
  (should (nelisp-pkg-validate-manifest
           '(:name "a" :stability "beta" :requires ()))))

(ert-deftest nelisp-pkg-render-preserves-other-keys ()
  "Regenerating dependencies must not drop what an author put there."
  (let ((text (nelisp-pkg-manifest-render
               "alpha" '("beta")
               '(:name "alpha" :version "2.1" :maintainer "someone"
                       :requires ("stale")))))
    (should (string-match-p ":version \"2.1\"" text))
    (should (string-match-p ":maintainer \"someone\"" text))
    (should (string-match-p ":requires (\"beta\")" text))
    (should-not (string-match-p "stale" text))
    ;; And it must read back as a valid manifest.
    (let ((manifest (car (read-from-string text))))
      (should (null (nelisp-pkg-validate-manifest manifest)))
      (should (equal (plist-get manifest :requires) '("beta"))))))

(ert-deftest nelisp-pkg-actual-requires-is-sorted-and-self-free ()
  (nelisp-pkg-test--with-tree
      '(("alpha" ("alpha.el" . "(require 'zeta)\n(require 'beta)\n(require 'alpha)\n(provide 'alpha)\n"))
        ("beta"  ("beta.el"  . "(provide 'beta)\n"))
        ("zeta"  ("zeta.el"  . "(provide 'zeta)\n")))
    (let* ((packages (nelisp-pkg-scan))
           (alpha (cl-find-if (lambda (p) (equal (plist-get p :name) "alpha"))
                              packages)))
      (should (equal (nelisp-pkg-actual-requires alpha packages)
                     '("beta" "zeta"))))))

(ert-deftest nelisp-pkg-treats-host-libraries-as-unresolved-not-missing ()
  "A require nothing in the tree provides is reported, not fatal."
  (nelisp-pkg-test--with-tree
      '(("alpha" ("alpha.el" . "(require 'cl-lib)\n(provide 'alpha)\n")))
    (let ((findings (nelisp-pkg-check (nelisp-pkg-scan))))
      (should (= 1 (length (nelisp-pkg-findings-of-kind
                            findings 'pkg-unresolved-require))))
      (should (null (nelisp-pkg-findings-of-kind findings 'pkg-cycle))))))

(ert-deftest nelisp-pkg-core-features-satisfy-a-require ()
  (nelisp-pkg-test--with-tree
      '(("alpha" ("alpha.el" . "(require 'nelisp-core-thing)\n(provide 'alpha)\n")))
    (let ((findings (nelisp-pkg-check (nelisp-pkg-scan) '(nelisp-core-thing))))
      (should (null (nelisp-pkg-findings-of-kind
                     findings 'pkg-unresolved-require))))))

;;; nelisp-pkg-test.el ends here
