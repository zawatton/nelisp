;;; nelisp-native-application-build-test.el --- application intake -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'ert)
(require 'nelisp-native-application-build)

(defconst nnab-test-root (file-name-directory (or load-file-name buffer-file-name)))

(defvar nnab-driver-ran nil)
;; This host option exists only in the supported consumer workspace. The seed
;; fixture binds it explicitly to test mismatch handling, not ON generation.
(defvar nelisp-cc-fxir-native-condition-facts)

(defun nnab-test--write (file text)
  (with-temp-file file (insert text)))

(ert-deftest nnab-discovers-only-top-level-defuns-without-evaluating-driver-forms ()
  (let* ((directory (make-temp-file "nnab-discovery-" t))
         (one (expand-file-name "one.el" directory))
         (two (expand-file-name "two.el" directory))
         (names (expand-file-name "names" directory))
         (manifest (expand-file-name "manifest.sexp" directory))
         (rows (expand-file-name "rows.el" directory))
         (receipt (expand-file-name "receipt.sexp" directory))
         (report (expand-file-name "report.sexp" directory))
         (baseline (nelisp-cc-fxir-native-all-rows))
         (expected-id0 (1+ (apply #'max (mapcar #'cadr baseline))))
         inputs)
    (unwind-protect
        (progn
          (setq nnab-driver-ran nil)
          (nnab-test--write one
            ";;; -*- lexical-binding: t; -*-\n(defun alpha (x) x)\n(defun main () (setq nnab-driver-ran t))\n(main)\n")
          (nnab-test--write two ";;; -*- lexical-binding: t; -*-\n(defun beta (x) (alpha x))\n")
          (let ((a (nelisp-native-application-build-discover
                    (list one two) '(main) baseline)))
            (setq inputs (nelisp-native-application-build-write-inputs
                          a names manifest rows receipt report))
            (should-not nnab-driver-ran)
            (should (equal (plist-get a :names) '(alpha beta)))
            (should (= (plist-get a :id0) expected-id0))
            (should (= (length (plist-get a :definitions)) 2))
            (should (equal (plist-get inputs :names-file) (file-truename names)))
            (should (equal (with-temp-buffer (insert-file-contents names) (buffer-string))
                           "alpha\nbeta\n"))
            (should (equal a (nelisp-native-application-build-discover
                              (list one two) '(main) baseline)))
            (should (eq (plist-get (car (plist-get a :rejections)) :reason)
                        'excluded-by-request))
            (should (stringp (plist-get (car (plist-get a :rejections)) :form-sha256)))))
      (delete-directory directory t))))

(ert-deftest nnab-refuses-duplicate-symbols-and-baseline-identities ()
  (let* ((directory (make-temp-file "nnab-conflict-" t))
         (one (expand-file-name "one.el" directory))
         (two (expand-file-name "two.el" directory)))
    (unwind-protect
        (progn
          (nnab-test--write one ";;; -*- lexical-binding: t; -*-\n(defun same (x) x)\n")
          (nnab-test--write two ";;; -*- lexical-binding: t; -*-\n(defun same (y) y)\n")
          (should-error (nelisp-native-application-build-discover (list one two) nil nil))
          (should (= (plist-get (nelisp-native-application-build-discover
                                 (list one) nil '((other 0 1 nil (:fn (:slot 0)) (1 1 -1)))) :id0)
                     1))
          (should-error (nelisp-native-application-build-discover
                         (list one) nil '((other 0 1 nil (:fn (:slot 0)) (1 1 -1))
                                          (another 0 1 nil (:fn (:slot 0)) (1 1 -1)))))
          (should-error (nelisp-native-application-build-discover
                         (list one) nil '((same 71 1 nil (:fn (:slot 0)) (1 1 -1)))))
          (should-error (nelisp-native-application-build-discover (list one one) nil nil)))
      (delete-directory directory t))))

(ert-deftest nnab-refuses-nonlexical-and-conflicting-headers-bounds-and-unreadable-inputs ()
  (let* ((directory (make-temp-file "nnab-bounds-" t))
         (plain (expand-file-name "plain.el" directory))
         (conflicting (expand-file-name "conflicting.el" directory))
         (oversized (expand-file-name "large.el" directory)))
    (unwind-protect
        (progn
          (nnab-test--write plain "(defun no-lexical (x) x)\n")
          (nnab-test--write oversized
            (concat ";;; -*- lexical-binding: t; -*-\n" (make-string 1048577 ?x)))
          (should-error (nelisp-native-application-build-discover (list plain) nil nil))
          (nnab-test--write conflicting
            ";;; -*- lexical-binding: nil; -*-\n;;; later comment: lexical-binding: t\n(defun x (a) a)\n")
          (should-error (nelisp-native-application-build-discover (list conflicting) nil nil))
          (nnab-test--write conflicting
            ";;; -*- lexical-binding: t; -*-\n;;; lexical-binding: t\n(defun x (a) a)\n")
          (should-error (nelisp-native-application-build-discover (list conflicting) nil nil))
          (should-error (nelisp-native-application-build-discover (list oversized) nil nil))
          (should-error (nelisp-native-application-build-discover
                         (list (expand-file-name "missing.el" directory)) nil nil))
          (should-error (nelisp-native-application-build-discover
                         (cl-loop for i below 33 collect (format "module-%d.el" i)) nil nil)))
      (delete-directory directory t))))

(ert-deftest nnab-refuses-canonical-input-aliases-and-output-aliases-before-writing ()
  (let* ((directory (make-temp-file "nnab-alias-" t))
         (source (expand-file-name "module.el" directory))
         (empty (expand-file-name "empty.el" directory))
         (alias (expand-file-name "alias.el" directory))
         (names (expand-file-name "names" directory))
         (manifest (expand-file-name "manifest.sexp" directory))
         (rows (expand-file-name "rows.el" directory))
         (receipt (expand-file-name "receipt.sexp" directory))
         (report (expand-file-name "report.sexp" directory)))
    (unwind-protect
        (progn
          (nnab-test--write source ";;; -*- lexical-binding: t; -*-\n(defun alias-leaf (x) x)\n")
          (nnab-test--write empty ";;; -*- lexical-binding: t; -*-\n")
          (make-symbolic-link empty alias)
          (should-error (nelisp-native-application-build-discover (list empty alias) nil nil))
          (let ((discovery (nelisp-native-application-build-discover (list source) nil nil)))
            (should-error (nelisp-native-application-build-write-inputs
                           discovery source manifest rows receipt report))
            (should-error (nelisp-native-application-build-write-inputs
                           discovery names names rows receipt report))
            (should-error (nelisp-native-application-build-write-inputs
                           discovery names manifest rows rows report))
            (nnab-test--write report "pre-existing-owner-data\n")
            (should-error (nelisp-native-application-build-write-inputs
                           discovery names manifest rows receipt report))
            (should-not (file-exists-p names))
            (should-not (file-exists-p manifest))
            (should (equal (with-temp-buffer
                             (insert-file-contents report)
                             (buffer-string))
                           "pre-existing-owner-data\n"))
            (should (string-match-p "(defun alias-leaf"
                                    (with-temp-buffer
                                      (insert-file-contents source)
                                      (buffer-string))))))
      (delete-directory directory t))))

(ert-deftest nnab-prepares-and-activates-only-after-matching-harvest-evidence ()
  (let* ((directory (make-temp-file "nnab-activate-" t))
         (source (expand-file-name "module.el" directory))
         (names (expand-file-name "names" directory))
         (manifest (expand-file-name "manifest.sexp" directory))
         (rows-file (expand-file-name "rows.el" directory))
         (receipt (expand-file-name "receipt.sexp" directory))
         (report (expand-file-name "report.sexp" directory))
         (nelisp-cc-fxir-native-windows nil)
         (nelisp-cc-fxir-native--additional-rows nil)
         (nelisp-cc-fxir-native--data-files nil)
         (nelisp-cc-fxir-native--source 'original-source)
         (nelisp-cc-fxir-native--boundaries '(original-boundary))
         (nelisp-cc-fxir-native--wrapper-names '(original-wrapper))
         producer)
    (unwind-protect
        (progn
          (nnab-test--write source
            ";;; -*- lexical-binding: t; -*-\n(defun application-declined (x) x)\n(defun application-leaf (x) x)\n")
          (let* ((discovery (nelisp-native-application-build-discover
                             (list source) nil (nelisp-cc-fxir-native-all-rows)))
                 (id (plist-get discovery :id0)))
            (let ((inputs (nelisp-native-application-build-write-inputs
                           discovery names manifest rows-file receipt report)))
            (nnab-test--write rows-file
              (format "(defconst application-rows '((application-leaf %d 1 nil (:fn (:slot 0)) (1 1 -1))))\n"
                      id))
            (dolist (role '(:binary :cold :build-manifest :stdout))
              (let ((file (expand-file-name (substring (symbol-name role) 1) directory)))
                (nnab-test--write file
                  (if (eq role :stdout)
                      (format "HARVEST application-declined skip not-a-closure\nHARVEST application-leaf ok id=%d words=3\n" id)
                    "synthetic test artifact\n"))
                (setq producer (plist-put producer role file))))
            (should-error
             (nelisp-native-application-build--require-complete-harvest
              discovery inputs rows-file 'application-rows producer))
            (cl-letf (((symbol-function 'nelisp-cc-fxir-native-all-rows)
                       (lambda () '((changed-baseline 100000 1 nil (:fn (:slot 0)) (1 1 -1))))))
              (should-error (nelisp-native-application-build-prepare-and-activate
                             discovery inputs rows-file 'application-rows producer receipt report)))
            (should-not (file-exists-p receipt))
            (should-not (file-exists-p report))
            (let ((result (nelisp-native-application-build-prepare-and-activate
                           discovery inputs rows-file 'application-rows producer receipt report)))
              (should (plist-get result :activation))
              (should-not (plist-get (plist-get result :activation) :native-qualified))
              (should (plist-get nelisp-cc-fxir-native--additional-rows :rows))
              (let ((outcome (car (plist-get (read (with-temp-buffer
                                                   (insert-file-contents report)
                                                   (buffer-string))) :definitions))))
                (should (eq (plist-get outcome :status) 'rejected))
                (should (equal (plist-get outcome :reason) "not-a-closure"))
                (should (stringp (plist-get outcome :form-sha256))))
              (should (file-exists-p report))))))
      (delete-directory directory t))))

(ert-deftest nnab-complete-harvest-rejects-partial-and-extra-evidence ()
  (let* ((directory (make-temp-file "nnab-complete-" t))
         (source (expand-file-name "module.el" directory))
         (names (expand-file-name "names" directory))
         (manifest (expand-file-name "manifest.sexp" directory))
         (rows-file (expand-file-name "rows.el" directory))
         (receipt (expand-file-name "receipt.sexp" directory))
         (report (expand-file-name "report.sexp" directory))
         (stdout (expand-file-name "stdout" directory))
         (discovery nil) (inputs nil))
    (unwind-protect
        (progn
          (nnab-test--write source
            ";;; -*- lexical-binding: t; -*-\n(defun complete-leaf (x) x)\n")
          (setq discovery
                (nelisp-native-application-build-discover
                 (list source) nil (nelisp-cc-fxir-native-all-rows)))
          (setq inputs
                (nelisp-native-application-build-write-inputs
                 discovery names manifest rows-file receipt report))
          (nnab-test--write rows-file
            (format "(defconst complete-rows '((complete-leaf %d 1 nil (:fn (:slot 0)) (1 1 -1))))\n"
                    (plist-get discovery :id0)))
          (let ((producer (list :stdout stdout)))
            (nnab-test--write stdout
              (format "HARVEST complete-leaf ok id=%d words=1\n"
                      (plist-get discovery :id0)))
            (should (nelisp-native-application-build--require-complete-harvest
                     discovery inputs rows-file 'complete-rows producer))
            (dolist (bad-output
                     (list
                      "HARVEST complete-leaf skip declined\n"
                      ""
                      (format "HARVEST complete-leaf ok id=%d words=1\nHARVEST extra ok id=%d words=1\n"
                              (plist-get discovery :id0) (1+ (plist-get discovery :id0)))
                      (format "HARVEST complete-leaf ok id=%d words=1\nHARVEST complete-leaf ok id=%d words=1\n"
                              (plist-get discovery :id0) (plist-get discovery :id0))
                      (format "HARVEST complete-leaf ok id=%d words=0\n"
                              (plist-get discovery :id0))))
              (nnab-test--write stdout bad-output)
              (should-error
               (nelisp-native-application-build--require-complete-harvest
                discovery inputs rows-file 'complete-rows producer)))
            (nnab-test--write stdout
              (format "HARVEST complete-leaf ok id=%d words=1\n"
                      (plist-get discovery :id0)))
            (nnab-test--write rows-file
              (format "(defconst complete-rows '((complete-leaf %d 1 nil (:fn (:slot 0)) (1 1 -1)) (complete-leaf %d 1 nil (:fn (:slot 0)) (1 1 -1))))\n"
                      (plist-get discovery :id0) (1+ (plist-get discovery :id0))))
            (should-error
             (nelisp-native-application-build--require-complete-harvest
              discovery inputs rows-file 'complete-rows producer))))
      (delete-directory directory t))))

(ert-deftest nnab-owned-output-mapping-rejects-swapped-foreign-and-symlink-paths ()
  (let* ((directory (make-temp-file "nnab-owned-map-" t))
         (owned (expand-file-name "owned" directory))
         (source (expand-file-name "module.el" directory))
         (names (expand-file-name "names" directory))
         (manifest (expand-file-name "manifest.sexp" directory))
         (rows (expand-file-name "rows.el" directory))
         (receipt (expand-file-name "receipt.sexp" directory))
         (report (expand-file-name "report.sexp" directory))
         (foreign (expand-file-name "foreign.el" directory))
         (discovery nil) (inputs nil) (planned nil) (owned-output nil))
    (unwind-protect
        (progn
          (make-directory owned)
          (nnab-test--write source
            ";;; -*- lexical-binding: t; -*-\n(defun map-leaf (x) x)\n")
          (setq discovery
                (nelisp-native-application-build-discover
                 (list source) nil (nelisp-cc-fxir-native-all-rows)))
          (setq inputs
                (nelisp-native-application-build-write-inputs
                 discovery names manifest rows receipt report)
                planned (copy-sequence (plist-get inputs :output-paths)))
          (nnab-test--write (expand-file-name "rows.el" owned) "rows\n")
          (nnab-test--write (expand-file-name "install-receipt.el" owned) "receipt\n")
          (setq owned-output
                (list :output owned
                      :rows (expand-file-name "rows.el" owned)
                      :install-receipt (expand-file-name "install-receipt.el" owned)))
          (let ((mapped (nelisp-native-application-build--activation-inputs
                         discovery inputs planned owned-output)))
            (should (equal (plist-get inputs :output-paths) planned))
            (should (equal (plist-get (plist-get mapped :owned-output-mapping)
                                      :planned-output-paths)
                           planned))
            (should (equal (nth 2 (plist-get mapped :output-paths))
                           (plist-get owned-output :rows))))
          (nnab-test--write foreign "foreign rows\n")
          (should-error
           (nelisp-native-application-build--activation-inputs
            discovery inputs planned (plist-put (copy-sequence owned-output) :rows foreign)))
          (make-symbolic-link foreign (expand-file-name "rows-link.el" owned))
          (should-error
           (nelisp-native-application-build--activation-inputs
            discovery inputs planned
            (plist-put (copy-sequence owned-output) :rows
                       (expand-file-name "rows-link.el" owned))))
          (nnab-test--write (expand-file-name "other.el" owned) "swapped rows\n")
          (should-error
           (nelisp-native-application-build--activation-inputs
            discovery inputs planned
            (plist-put (copy-sequence owned-output) :rows
                       (expand-file-name "other.el" owned)))))
      (delete-directory directory t))))

(ert-deftest nnab-condition-option-must-match-and-cached-source-must-be-fresh ()
  "Exercise host-option bookkeeping with a mocked fixture binding only."
  (let ((nelisp-cc-fxir-native-condition-facts nil)
        (nelisp-cc-fxir-native--source (nelisp-cc-fxir-native-source)))
    (should-not (nelisp-native-application-build--check-condition-option nil))
    (should-error (nelisp-native-application-build--check-condition-option t))
    (should (string-match-p "\\`[0-9a-f]\\{64\\}\\'"
                            (nelisp-native-application-build--condition-source-hash)))
    (setq nelisp-cc-fxir-native-condition-facts t)
    (should-not (nelisp-native-application-build--check-condition-option t))
    (should-error (nelisp-native-application-build--check-condition-option nil))
    (setq nelisp-cc-fxir-native--source '(stale-cached-source))
    (should-error (nelisp-native-application-build--condition-source-hash))))

(ert-deftest nnab-run-owned-offline-process-mock-reaches-real-activation-validator ()
  "Exercise host orchestration offline; only the child process boundary is mocked."
  (let* ((directory (make-temp-file "nnab-owned-run-" t))
         (repository (expand-file-name "repository" directory))
         (outside (expand-file-name "outside" directory))
         (module (expand-file-name "module.el" repository))
         (adapter (expand-file-name "adapter.py" directory))
         (python (or (executable-find "python3") (error "python3 unavailable")))
         (names (expand-file-name "names.txt" outside))
         (same-names (expand-file-name "same-content-names.txt" outside))
         (manifest (expand-file-name "manifest.sexp" outside))
         (planned-rows (expand-file-name "planned-rows.el" outside))
         (planned-receipt (expand-file-name "planned-receipt.sexp" outside))
         (planned-report (expand-file-name "planned-report.sexp" outside))
         (nelisp-cc-fxir-native-condition-facts nil)
         (initial-boundaries '(original-boundary))
         (initial-wrapper-names '(original-wrapper))
         (nelisp-cc-fxir-native-windows nil)
         (nelisp-cc-fxir-native--additional-rows nil)
         (nelisp-cc-fxir-native--data-files nil)
         (nelisp-cc-fxir-native--boundaries initial-boundaries)
         (nelisp-cc-fxir-native--wrapper-names initial-wrapper-names)
         (initial-source (nelisp-cc-fxir-native-source))
         (nelisp-cc-fxir-native--source initial-source)
         discovery inputs request producer-spec mutate-after-child successful-roots
         (receipt-names-mode 'staged) receipt-sha-mode mutate-staged-names
         inject-late-write-failure inject-cleanup-failure failed-metadata-path test-run-root)
    (unwind-protect
        (progn
          (make-directory repository)
          (make-directory outside)
          (nnab-test--write module
            ";;; -*- lexical-binding: t; -*-\n(defun owned-leaf (x) x)\n")
          (nnab-test--write adapter "# fixed offline seam fixture\n")
          (dolist (role '("build_manifest" "cold_image" "guard" "harvester" "helper"
                          "launcher" "owned_runner" "prelude" "producer" "producer_image"))
            (let ((path (expand-file-name (concat role ".pin") repository)))
              (cond
               ((equal role "prelude")
                (copy-file (expand-file-name "../scripts/nelisp-stdlib-prelude.el"
                                             nnab-test-root)
                           path))
               ((member role '("producer_image" "cold_image"))
                (nnab-test--write path (make-string 2097152 ?x)))
               (t (nnab-test--write path (concat role " fixture\n"))))))
          (setq discovery
                (nelisp-native-application-build-discover
                 (list module) nil (nelisp-cc-fxir-native-all-rows)))
          (setq inputs
                (nelisp-native-application-build-write-inputs
                 discovery names manifest planned-rows planned-receipt planned-report))
          (nnab-test--write same-names
            (with-temp-buffer (insert-file-contents names) (buffer-string)))
          (let ((pins (make-hash-table :test 'equal)))
            (dolist (role nelisp-native-application-build--pin-roles)
              (let* ((path (if (equal role "repository") repository
                             (pcase role
                               ("application_manifest" manifest)
                               ("names" names)
                               (_ (expand-file-name (concat role ".pin") repository)))))
                     (digest (if (equal role "repository") (make-string 64 ?a)
                               (with-temp-buffer
                                 (set-buffer-multibyte nil)
                                 (insert-file-contents-literally path)
                                 (secure-hash 'sha256 (current-buffer)))))
                     (pin (nelisp-native-application-build--json-hash
                           (list (cons "path" (file-truename path))
                                 (cons "sha256" digest)))))
                (puthash role pin pins)))
            (setq request
                  (nelisp-native-application-build--json-hash
                   (list (cons "schema" "nelisp-native-application-harvest/v1")
                         (cons "pins" pins) (cons "mode" "strict")
                         (cons "names_sha256" (plist-get inputs :names-sha256))
                         (cons "id0" (plist-get discovery :id0))
                         (cons "constant" "owned-rows")
                         (cons "output_parent" outside)))
                  producer-spec
                  (list :adapter adapter
                        :adapter-sha256
                        (nelisp-native-application-build--sha256-file adapter)
                        :python (file-truename python) :request request
                        :condition-facts nil))
          (let ((original-planned (copy-sequence (plist-get inputs :output-paths))))
            (cl-letf
                (((symbol-function 'nelisp-native-application-build--invoke-adapter)
                  (lambda (_python _adapter request-file stdout-file stderr-file)
                    (let* ((actual-request
                            (nelisp-native-application-build--read-json-file request-file))
                           (run-root (gethash "output_parent" actual-request))
                           (output (expand-file-name "owned-output" run-root))
                           (child (expand-file-name "child" output))
                           (cwd (expand-file-name "cwd" child))
                           (names-copy (expand-file-name "names.txt" output))
                           (receipt-names-file
                            (pcase receipt-names-mode
                              ('staged names-copy)
                              ('original names)
                              ('same-content same-names)
                              ('alias (concat (file-name-directory names-copy) "./"
                                              (file-name-nondirectory names-copy)))
                              ('missing nil)))
                           (receipt-names-sha256
                            (if (eq receipt-sha-mode 'wrong)
                                (make-string 64 ?0)
                              (plist-get inputs :names-sha256)))
                           (rows-copy (expand-file-name "rows.el" output))
                           (install-receipt (expand-file-name "install-receipt.el" output))
                           (stdout (expand-file-name "stdout" child))
                           (receipt-form
                            (list :status :installed :native-qualified nil
                                  :module-initialization-executed nil
                                  :whole-module-initialization-equivalence nil
                                  :names (plist-get discovery :names)
                                  :manifest-file manifest
                                  :manifest-sha256 (plist-get inputs :manifest-sha256)
                                  :names-file receipt-names-file :names-sha256 receipt-names-sha256
                                  :source-modules (plist-get discovery :modules)
                                  :baseline-sha256 (plist-get discovery :baseline-sha256)
                                  :mode :strict :omitted-definition-names nil
                                  :producer-image-sha256
                                  (gethash "sha256" (gethash "producer_image" pins))
                                  :producer-source-sha256
                                  (gethash "sha256" (gethash "producer" pins))
                                  :helper-sha256 (gethash "sha256" (gethash "helper" pins))
                                  :prelude-sha256 (gethash "sha256" (gethash "prelude" pins))
                                  :definitions
                                  (mapcar (lambda (entry)
                                            (list :name (plist-get entry :name)
                                                  :source-form-sha256
                                                  (plist-get entry :form-sha256)
                                                  :installed-identity-match t
                                                  :installed-structure-match t
                                                  :arguments-authenticated t
                                                  :body-authenticated t))
                                          (plist-get discovery :definitions))))
                           (capture-file (expand-file-name "capture.json" output)))
                      (setq test-run-root run-root)
                      (make-directory output)
                      (make-directory child)
                      (make-directory cwd)
                      (nnab-test--write names-copy
                                        (if mutate-staged-names
                                            "tampered staged names\n"
                                          (with-temp-buffer
                                            (insert-file-contents names)
                                            (buffer-string))))
                      (nnab-test--write rows-copy
                        (format "(defconst owned-rows '((owned-leaf %d 1 nil (:fn (:slot 0)) (1 1 -1))))\n"
                                (plist-get discovery :id0)))
                      (nnab-test--write install-receipt
                        (concat (nelisp-cc-fxir-admission--print receipt-form) "\n"))
                      (nnab-test--write stdout
                        (format "HARVEST owned-leaf ok id=%d words=3\n"
                                (plist-get discovery :id0)))
                      (nnab-test--write (expand-file-name "stderr" child) "")
                      (nnab-test--write (expand-file-name "rc" child) "0\n")
                      (nnab-test--write (expand-file-name "command.json" child) "{}\n")
                      (nnab-test--write (expand-file-name "launch.json" child) "{}\n")
                      (nnab-test--write (expand-file-name "producer-driver.el" output) ";; fixed\n")
                      (let ((output-hashes (make-hash-table :test 'equal)))
                        (dolist (name '("command.json" "install-receipt.el" "launch.json"
                                        "names.txt" "producer-driver.el" "rc" "rows.el"
                                        "stderr" "stdout"))
                          (let ((path (if (member name '("command.json" "launch.json" "rc"
                                                         "stderr" "stdout"))
                                          (expand-file-name name child)
                                        (expand-file-name name output))))
                            (puthash name
                                     (nelisp-native-application-build--sha256-file path)
                                     output-hashes)))
                        (nnab-test--write capture-file
                          (json-serialize
                           (nelisp-native-application-build--json-hash
                            (list
                             (cons "child_lifecycle"
                                   (nelisp-native-application-build--json-hash
                                    (list
                                     (cons "argv"
                                           (vector (gethash "path" (gethash "producer_image" pins))
                                                   "--load"
                                                   (expand-file-name "producer-driver.el" output)))
                                     (cons "cwd" cwd)
                                     (cons "environment"
                                           (let ((environment
                                                  (nelisp-native-application-build--json-hash
                                                   (list
                                                    (cons "LC_ALL" "C")
                                                    (cons "NC1_CONST" (gethash "constant" actual-request))
                                                    (cons "NC1_ID0" (number-to-string
                                                                      (gethash "id0" actual-request)))
                                                    (cons "NC1_NAMES" names-copy)
                                                    (cons "NC1_OUT" rows-copy)
                                                    (cons "NELISP_APPLICATION_INSTALL_RECEIPT"
                                                          install-receipt)
                                                    (cons "NELISP_APPLICATION_MANIFEST" manifest)
                                                    (cons "NELISP_APPLICATION_MANIFEST_SHA256"
                                                          (gethash "sha256"
                                                                   (gethash "application_manifest" pins)))
                                                    (cons "NELISP_APPLICATION_MODE"
                                                          (gethash "mode" actual-request))
                                                    (cons "NELISP_APPLICATION_NAMES_SHA256"
                                                          (gethash "names_sha256" actual-request))
                                                    (cons "NELISP_APPLICATION_ROOT" repository)
                                                    (cons "NELISP_FXIR_ROOT" repository)
                                                    (cons "NELISP_PRODUCER_IMAGE_SHA256"
                                                          (gethash "sha256"
                                                                   (gethash "producer_image" pins)))
                                                    (cons "PATH" (or (getenv "PATH") "/usr/bin:/bin"))))))
                                             environment))
                                     (cons "child"
                                           (nelisp-native-application-build--json-hash
                                            (list (cons "pid" 222)
                                                  (cons "session" 222)
                                                  (cons "start_ticks" 333)
                                                  (cons "uid" 1000))))
                                     (cons "controller"
                                           (nelisp-native-application-build--json-hash
                                            (list (cons "pid" 111)
                                                  (cons "session" 100)
                                                  (cons "start_ticks" 222)
                                                  (cons "uid" 1000))))
                                     (cons "cleanup"
                                           (nelisp-native-application-build--json-hash
                                            (list (cons "verified" t)
                                                  (cons "errors" [])
                                                  (cons "initial" [])
                                                  (cons "remaining" []))))
                                     (cons "rc" 0) (cons "timed_out" :false)
                                     (cons "cancelled" :false) (cons "timeout" 45))))
                             (cons "outputs" output-hashes)
                             (cons "owned_report" (make-hash-table :test 'equal))
                             (cons "pins" pins) (cons "rejected" :false)
                             (cons "request_sha256"
                                   (nelisp-native-application-build--sha256-file
                                    request-file))
                             (cons "runner_ast_sha256" (make-string 64 ?b))
                             (cons "status" "RAW_CAPTURED_NOT_ADMITTED"))))))
                      (nnab-test--write stdout-file
                        (json-serialize
                         (nelisp-native-application-build--json-hash
                          (list (cons "status" "RAW_CAPTURED_NOT_ADMITTED")
                                (cons "output" output) (cons "capture" capture-file)))))
                      (nnab-test--write stderr-file "")
                      (when mutate-after-child
                        (nnab-test--write module
                          ";;; -*- lexical-binding: t; -*-\n(defun owned-leaf (x) (cons x nil))\n"))
                      0)))
              ((symbol-function 'write-region)
               (let ((original-write-region (symbol-function 'write-region)))
                 (lambda (&rest arguments)
                   (let ((file (nth 2 arguments)))
                     (if (and inject-late-write-failure
                              (stringp file)
                              (equal (file-name-nondirectory file)
                                     "host-activation.sexp"))
                         (progn
                           (setq failed-metadata-path file)
                           (error "Independent late metadata write failure"))
                       (apply original-write-region arguments))))))
              ((symbol-function 'nelisp-native-application-build--delete-owned-tree)
               (let ((original-delete
                      (symbol-function 'nelisp-native-application-build--delete-owned-tree)))
                 (lambda (owned-directory)
                   (funcall original-delete owned-directory)
                   (when (and inject-cleanup-failure
                              (equal owned-directory test-run-root))
                     (setq inject-cleanup-failure nil)
                     (error "Injected owned cleanup failure"))))))
              (let ((result
                     (nelisp-native-application-build-run-owned
                      discovery inputs producer-spec planned-rows 'owned-rows
                      planned-receipt planned-report)))
                (should (plist-get result :activation))
                (should-not (plist-get (plist-get result :activation) :native-qualified))
                (should (equal (plist-get inputs :output-paths) original-planned))
                (should (eq (plist-get result :condition-facts) nil))
                (should (file-exists-p (plist-get result :host-configuration-file)))
                (should (file-exists-p (plist-get result :host-activation-file)))
                (setq successful-roots (directory-files outside nil "\\`.nnab-owned-"))
                (should (eq (plist-get (read (with-temp-buffer
                                               (insert-file-contents
                                                (plist-get result :host-configuration-file))
                                               (buffer-string)))
                                      :condition-facts)
                            nil)))
              (dolist (case '((original nil nil "Raw producer receipt does not authenticate selected discovery")
                              (same-content nil nil "Raw producer receipt does not authenticate selected discovery")
                              (alias nil nil "Raw producer receipt does not authenticate selected discovery")
                              (missing nil nil "Raw producer receipt does not authenticate selected discovery")
                              (staged wrong nil "Raw producer receipt does not authenticate selected discovery")
                              (staged nil t "Adapter-owned names copy differs from the Lisp input")))
                (setq receipt-names-mode (nth 0 case)
                      receipt-sha-mode (nth 1 case)
                      mutate-staged-names (nth 2 case)
                      nelisp-cc-fxir-native--additional-rows nil
                      nelisp-cc-fxir-native--source initial-source
                      nelisp-cc-fxir-native--boundaries initial-boundaries
                      nelisp-cc-fxir-native--wrapper-names initial-wrapper-names)
                (let ((caught
                       (condition-case condition
                           (progn
                             (nelisp-native-application-build-run-owned
                              discovery inputs producer-spec planned-rows 'owned-rows
                              planned-receipt planned-report)
                             nil)
                         (error condition))))
                  (should caught)
                  (should (equal (cadr caught) (nth 3 case))))
                (should (equal (directory-files outside nil "\\`.nnab-owned-")
                               successful-roots))
                (should-not (file-directory-p test-run-root)))
              (setq receipt-names-mode 'staged
                    receipt-sha-mode nil
                    mutate-staged-names nil)
              (setq mutate-after-child t)
              (should-error
               (nelisp-native-application-build-run-owned
                discovery inputs producer-spec planned-rows 'owned-rows
                planned-receipt planned-report))
              (should (equal (plist-get inputs :output-paths) original-planned))
              (should (file-exists-p names))
              (should (file-exists-p manifest))
              (should (equal (directory-files outside nil "\\`.nnab-owned-")
                             successful-roots))
              ;; Force a write failure after the unchanged validator has
              ;; published globals. The host must be marked non-reusable and
              ;; the original late-write error retained in the condition.
              (nnab-test--write module
                ";;; -*- lexical-binding: t; -*-\n(defun owned-leaf (x) x)\n")
              (setq nelisp-cc-fxir-native--additional-rows nil
                    nelisp-cc-fxir-native--source initial-source
                    nelisp-cc-fxir-native--boundaries initial-boundaries
                    nelisp-cc-fxir-native--wrapper-names initial-wrapper-names
                    mutate-after-child nil
                    failed-metadata-path nil inject-late-write-failure t)
              (let ((caught
                     (condition-case condition
                         (progn
                           (nelisp-native-application-build-run-owned
                            discovery inputs producer-spec planned-rows 'owned-rows
                            planned-receipt planned-report)
                           nil)
                       (nelisp-native-application-build-host-discard-required condition))))
                (should caught)
                (should (eq (car caught)
                            'nelisp-native-application-build-host-discard-required))
                (should (eq (plist-get (cdr caught) :activation-attempted) t))
                (let ((cause (plist-get (cdr caught) :cause)))
                  (should (eq (car cause) 'error))
                  (should (equal (cadr cause) "Independent late metadata write failure")))
                (should (null (plist-get (cdr caught) :owned-cleanup-errors)))
                (should-not (file-exists-p failed-metadata-path))
                (should-not
                 (file-exists-p (expand-file-name "admission-receipt.sexp"
                                                  (file-name-directory failed-metadata-path))))
                (should nelisp-cc-fxir-native--additional-rows)
                (should-not (eq nelisp-cc-fxir-native--source initial-source)))
              ;; A cleanup error is attached without replacing the activation
              ;; failure; cleanup was performed before the injected report.
              (setq nelisp-cc-fxir-native--additional-rows nil
                    nelisp-cc-fxir-native--source initial-source
                    nelisp-cc-fxir-native--boundaries initial-boundaries
                    nelisp-cc-fxir-native--wrapper-names initial-wrapper-names
                    failed-metadata-path nil inject-cleanup-failure t)
              (let ((caught
                     (condition-case condition
                         (progn
                           (nelisp-native-application-build-run-owned
                            discovery inputs producer-spec planned-rows 'owned-rows
                            planned-receipt planned-report)
                           nil)
                       (nelisp-native-application-build-host-discard-required condition))))
                (should caught)
                (let ((cause (plist-get (cdr caught) :cause))
                      (cleanup-errors (plist-get (cdr caught) :owned-cleanup-errors)))
                  (should (equal (cadr cause) "Independent late metadata write failure"))
                  (should (equal (cadar cleanup-errors) "Injected owned cleanup failure")))
                (should-not (file-exists-p failed-metadata-path))
                (should-not
                 (file-exists-p (expand-file-name "admission-receipt.sexp"
                                                  (file-name-directory failed-metadata-path))))
                (should nelisp-cc-fxir-native--additional-rows)
                (should-not (eq nelisp-cc-fxir-native--source initial-source)))))))
      (delete-directory directory t))))

(ert-deftest nnab-image-pin-hashing-is-bounded-and-role-specific ()
  (let* ((directory (make-temp-file "nnab-image-pin-" t))
         (file (expand-file-name "image.data" directory))
         (alias (expand-file-name "alias.data" directory))
         (bytes (make-string 2097152 ?x))
         (digest (secure-hash 'sha256 bytes)))
    (unwind-protect
        (progn
          (nnab-test--write file bytes)
          (dolist (role '("producer_image" "cold_image"))
            (should (equal file (nelisp-native-application-build--canonical-file-pin
                                file digest role)))
            (should-error (nelisp-native-application-build--canonical-file-pin
                           file (make-string 64 ?0) role)))
          (dolist (role '(nil "producer" "application_manifest" "build_manifest"
                             "names" "guard" "harvester" "helper" "launcher" "owned_runner"))
            (should-error (nelisp-native-application-build--canonical-file-pin
                           file digest role)))
          (should (equal file (nelisp-native-application-build--canonical-file-pin
                              file digest "prelude")))
          (should-error (nelisp-native-application-build--canonical-file-pin
                         file digest "helper"))
          ;; The real full prelude exceeds the generic metadata limit and must
          ;; pass the fixed prelude role through the same canonical hash gate.
          (let* ((prelude (expand-file-name "../scripts/nelisp-stdlib-prelude.el"
                                             nnab-test-root))
                 (prelude-hash (with-temp-buffer
                                 (set-buffer-multibyte nil)
                                 (insert-file-contents-literally prelude)
                                 (secure-hash 'sha256 (current-buffer))))
                 (oversize (expand-file-name "oversize-prelude.data" directory)))
            (should (> (file-attribute-size (file-attributes prelude)) 1048576))
            (should (equal prelude
                           (nelisp-native-application-build--canonical-file-pin
                            prelude prelude-hash "prelude")))
            (with-temp-file oversize (insert (make-string 8388609 ?x)))
            (should-error (nelisp-native-application-build--canonical-file-pin
                           oversize (make-string 64 ?0) "prelude")))
          (dolist (hash '(nil 7 "" "bad"))
            (should-error (nelisp-native-application-build--canonical-file-pin
                           file hash "producer_image")))
          (make-symbolic-link file alias)
          (dolist (path (list alias directory (expand-file-name "missing" directory)
                             (concat directory "/./image.data") "relative.data" nil 7))
            (should-error (nelisp-native-application-build--canonical-file-pin
                           path digest "producer_image")))
          ;; Exercise the exact production image-policy boundary with a small
          ;; dynamic limit; no large file or executable is needed.
          (let ((nelisp-native-application-build--max-image-pin-bytes 2097152))
            (should (equal file (nelisp-native-application-build--canonical-file-pin
                                file digest "producer_image"))))
          (let ((nelisp-native-application-build--max-image-pin-bytes 2097151))
            (should-error (nelisp-native-application-build--canonical-file-pin
                           file digest "producer_image")))
          (should-error (nelisp-cc-fxir-admission--bytes file)))
      (delete-directory directory t))))

(ert-deftest nnab-pin-hash-rejects-changes-during-bounded-read ()
  (let ((file (make-temp-file "nnab-pin-change-")))
    (unwind-protect
        (progn
          (nnab-test--write file "before")
          (let ((original (symbol-function 'insert-file-contents-literally)))
            (cl-letf (((symbol-function 'insert-file-contents-literally)
                       (lambda (&rest args)
                         (prog1 (apply original args)
                           (nnab-test--write file "changed-after-read")))))
              (should-error (nelisp-native-application-build--sha256-file file)))))
      (delete-file file))))

(provide 'nelisp-native-application-build-test)
;;; nelisp-native-application-build-test.el ends here
