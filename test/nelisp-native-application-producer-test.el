;;; nelisp-native-application-producer-test.el --- forms intake -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'ert)
;; Fixtures are read back byte-exactly; keep LF on CRLF-default hosts (Windows).
(setq-default buffer-file-coding-system 'utf-8-unix)
(require 'nelisp-native-application-build)
(require 'nelisp-native-application-producer)

(defvar nnap-side-effect nil)
(defun nnap-provider-collision () nil)

(defun nnap-test--write (file text)
  (with-temp-file file (insert text)))

(defun nnap-test--artifacts (directory module-texts)
  "Create host-planning-only discovery artifacts for MODULE-TEXTS."
  (let* ((texts (if (stringp module-texts) (list module-texts) module-texts))
         (modules (cl-loop for text in texts for index from 0
                           collect (expand-file-name (format "module-%d.el" index)
                                                     directory)))
         (names (expand-file-name "names" directory))
         (manifest (expand-file-name "manifest.sexp" directory))
         (rows (expand-file-name "rows.el" directory))
         (receipt (expand-file-name "receipt.sexp" directory))
         (report (expand-file-name "report.sexp" directory)))
    (cl-mapc #'nnap-test--write modules texts)
    (let* ((discovery (nelisp-native-application-build-discover
                       modules nil (nelisp-cc-fxir-native-all-rows)))
           (artifacts (nelisp-native-application-build-write-inputs
                       discovery names manifest rows receipt report)))
      (list :module (car modules) :modules modules
            :names (plist-get artifacts :names-file)
            :manifest (plist-get artifacts :manifest-file)
            :names-sha256 (plist-get artifacts :names-sha256)
            :manifest-sha256 (plist-get artifacts :manifest-sha256)))))

(defun nnap-test--install (artifacts)
  (nelisp-native-application-producer-install
   (plist-get artifacts :manifest) (plist-get artifacts :names)
   (plist-get artifacts :manifest-sha256) (plist-get artifacts :names-sha256)))

(defun nnap-test--fake-adapter (&optional fail-name wrong-name)
  "Return a clearly fake function-cell adapter and its private binding table."
  (let ((table (make-hash-table :test 'eq)) (calls 0))
    (list :table table
          :adapter
          (list :lookup (lambda (name) (gethash name table :unbound))
                :bind (lambda (name closure)
                        (puthash name
                                 (if (eq name wrong-name)
                                     (let ((fn (copy-sequence closure)))
                                       (setcar (nthcdr 3 fn) 'wrong-body)
                                       fn)
                                   closure)
                                 table)
                        (when (and fail-name (eq name fail-name)
                                   (= (setq calls (1+ calls)) 1))
                          (error "Injected adapter write failure")))
                :unbind (lambda (name) (remhash name table))))))

(defun nnap-test--construct-closure (form)
  "Fake the NeLisp closure constructor for host-side adapter tests."
  (cons 'closure
        (cons '(t) (cons (nth 2 form) (nthcdr 3 form)))))

(ert-deftest nnap-valid-renamed-modules-install-authenticated-closures-through-fake-adapter ()
  (let* ((directory (make-temp-file "nnap-valid-" t))
         (artifacts (nnap-test--artifacts
                     directory
                     (list ";;; -*- lexical-binding: t; -*-\n(defun nnap-first (x) (nnap-second x))\n"
                           ";;; -*- lexical-binding: t; -*-\n(defun nnap-second (x) x)\n"))))
    (unwind-protect
        (let* ((plan (nelisp-native-application-producer-preflight
                      (plist-get artifacts :manifest) (plist-get artifacts :names)
                      (plist-get artifacts :manifest-sha256)
                      (plist-get artifacts :names-sha256)))
               (fake (nnap-test--fake-adapter))
               candidates result)
          (cl-letf (((symbol-function 'nelisp-fxir-recompile)
                     (lambda (closure)
                       (if (consp (nth 3 closure))
                           (list (vector 16 (vector 'nnap-second nil nil nil nil) 1)
                                 1 '(:fn (:call 1 1 (:slot 0))))
                         (list [] 1 '(:fn (:slot 0))))))
                    ((symbol-function 'nelisp--fx-macros-used) (lambda (_) nil))
                    ((symbol-function 'nelisp-fxir-problems) (lambda (&rest _) nil))
                    ((symbol-function 'nelisp-fxir-callees)
                     (lambda (_ir code) (and (> (length code) 0) '(nnap-second)))))
            (setq candidates
                  (nelisp-native-application-producer--prepare-candidates
                   plan #'nnap-test--construct-closure))
            (setq result
                  (nelisp-native-application-producer--install-candidates
                   plan candidates (plist-get fake :adapter)
                   '(:helper-sha256 "fake-helper" :prelude-sha256 "fake-prelude"))))
          (should (eq (plist-get plan :status) :validated-inputs))
          (should (= (plist-get plan :source-form-count) 2))
          (should (eq (plist-get result :status) :installed))
          (should (= (length (plist-get result :definitions)) 2))
          (should (eq (gethash 'nnap-first (plist-get fake :table))
                      (plist-get (car candidates) :closure)))
          (should (eq (gethash 'nnap-second (plist-get fake :table))
                      (plist-get (cadr candidates) :closure)))
          (should-not (fboundp 'nnap-first))
          (should-not (fboundp 'nnap-second)))
      (delete-directory directory t))))

(ert-deftest nnap-rolls-back-only-owned-bindings-after-post-install-auth-failure ()
  (let* ((directory (make-temp-file "nnap-rollback-" t))
         (artifacts (nnap-test--artifacts
                     directory
                     ";;; -*- lexical-binding: t; -*-\n(defun nnap-roll-first (x) x)\n(defun nnap-roll-second (x) x)\n"))
         (plan (nelisp-native-application-producer-preflight
                (plist-get artifacts :manifest) (plist-get artifacts :names)
                (plist-get artifacts :manifest-sha256)
                (plist-get artifacts :names-sha256)))
         (fake (nnap-test--fake-adapter nil 'nnap-roll-second)))
    (unwind-protect
        (let (candidates)
          (cl-letf (((symbol-function 'nelisp-fxir-recompile)
                     (lambda (_closure) (list [] 1 '(:fn (:slot 0)))))
                    ((symbol-function 'nelisp-fxir-callees)
                     (lambda (_ir _code) nil)))
            (setq candidates
                  (nelisp-native-application-producer--prepare-candidates
                   plan #'nnap-test--construct-closure)))
          (should-error
           (nelisp-native-application-producer--install-candidates
            plan candidates (plist-get fake :adapter) nil))
          (should (eq (gethash 'nnap-roll-first (plist-get fake :table) :unbound)
                      :unbound))
          (should (eq (gethash 'nnap-roll-second (plist-get fake :table) :unbound)
                      :unbound)))
      (delete-directory directory t))))

(ert-deftest nnap-refuses-top-level-main-and-setq-without-running-sentinels ()
  (let* ((directory (make-temp-file "nnap-main-" t))
         (artifacts (nnap-test--artifacts
                     directory
                     ";;; -*- lexical-binding: t; -*-\n(defun nnap-safe () 1)\n(setq nnap-side-effect t)\n(defun main () (setq nnap-side-effect t))\n(main)\n")))
    (unwind-protect
        (progn
          (setq nnap-side-effect nil)
          (should-error (nnap-test--install artifacts))
          (should-not nnap-side-effect)
          (should-not (fboundp 'nnap-safe))
          (should-not (fboundp 'main)))
      (delete-directory directory t))))

(ert-deftest nnap-refuses-external-provider-and-macro-context-before-binding ()
  (let* ((directory (make-temp-file "nnap-context-" t))
         (artifacts (nnap-test--artifacts
                     directory
                     ";;; -*- lexical-binding: t; -*-\n(defun nnap-context-target (x) (+ x 1))\n"))
         (plan (nelisp-native-application-producer-preflight
                (plist-get artifacts :manifest) (plist-get artifacts :names)
                (plist-get artifacts :manifest-sha256)
                (plist-get artifacts :names-sha256)))
         (fake (nnap-test--fake-adapter)))
    (unwind-protect
        (cl-letf (((symbol-function 'nelisp--fx-macros-used) nil)
                  ((symbol-function 'nelisp-fxir-recompile)
                   (lambda (_closure) (list [] 1 '(:fn (:slot 0)))))
                  ((symbol-function 'nelisp-fxir-callees)
                   (lambda (_ir _code) '(+))))
          (should-error
           (nelisp-native-application-producer--install-candidates
            plan (nelisp-native-application-producer--prepare-candidates
                  plan #'nnap-test--construct-closure)
            (plist-get fake :adapter) nil)))
      (should (eq (gethash 'nnap-context-target (plist-get fake :table) :unbound)
                  :unbound))
      (delete-directory directory t))))

(ert-deftest nnap-compiler-admission-preserves-canonical-helper-ir-contract ()
  ;; This tuple is the genuine helper's compiled shape for (+ a b 7):
  ;; CODE, NLOCALS, and an already wrapped (:fn BODY) IR. The real helper
  ;; validators and real FXIR analyzer check the producer boundary below.
  (unless (fboundp 'nelisp-fxir-problems)
    (load (expand-file-name "test/nelisp-fxir-eval.el" default-directory) nil t))
  (let* ((code (vector 17 nil 2 0 2 1 1 1 16 (vector '+ nil nil nil nil) 3 0 5))
         (body '(:call 9 3 (:slot 0) (:slot 1) (:const 7)))
         (ir (list :fn body))
         (compiled (list code 2 ir))
         (callees (nelisp-fxir-callees ir code))
         (nested-report
          (nelisp-cc-fxir-analysis-row
           (list 'bench-call-helper 0 2 callees (list :fn ir) '(2 2 -1)) code)))
    (should (null (nelisp-fxir-problems ir code 2)))
    (should (equal (nelisp-fxir-callees ir code) '(+)))
    (let ((admission
           (nelisp-native-application-producer--compiler-admission
            'bench-call-helper '(a b) nil compiled)))
      (should (plist-get (plist-get admission :analysis) :valid))
      (should (equal (plist-get admission :callees) '(+))))
    (should-not (plist-get nested-report :valid))
    (should (memq :unknown-node (plist-get nested-report :refusals)))
    (should-error
     (nelisp-native-application-producer--compiler-admission
      'bench-call-helper '(a b) nil (list code 2 body)))
    (should-error
     (nelisp-native-application-producer--compiler-admission
      'bench-call-helper '(a b) nil (list code 2 (list :fn ir))))
    (should-error
     (nelisp-native-application-producer--compiler-admission
      'bench-call-helper '(a b) nil (list code 2 '(:fn (:not-a-node)))))
    (let ((bad-code (copy-sequence code)))
      (aset bad-code 8 17)
      (should-error
       (nelisp-native-application-producer--compiler-admission
        'bench-call-helper '(a b) nil (list bad-code 2 ir))))))

(ert-deftest nnap-public-install-refuses-gnu-host-before-loading-fx-helper ()
  (let* ((directory (make-temp-file "nnap-gnu-" t))
         (artifacts (nnap-test--artifacts
                     directory
                     ";;; -*- lexical-binding: t; -*-\n(defun nnap-gnu-target (x) x)\n"))
         (process-environment (copy-sequence process-environment)))
    (unwind-protect
        (progn
          (setenv "NELISP_PRODUCER_IMAGE_SHA256" (make-string 64 ?a))
          (should-error
           (nelisp-native-application-producer-install
            (plist-get artifacts :manifest) (plist-get artifacts :names)
            (plist-get artifacts :manifest-sha256)
            (plist-get artifacts :names-sha256) (make-string 64 ?a)))
          (should-not (fboundp 'nnap-gnu-target)))
      (delete-directory directory t))))

(ert-deftest nnap-rejects-manifest-or-names-hash-drift-before-installation ()
  (let* ((directory (make-temp-file "nnap-hash-" t))
         (artifacts (nnap-test--artifacts
                     directory
                     ";;; -*- lexical-binding: t; -*-\n(defun nnap-hash-target () 7)\n")))
    (unwind-protect
        (progn
          (should-error
           (nelisp-native-application-producer-install
            (plist-get artifacts :manifest) (plist-get artifacts :names)
            (make-string 64 ?0) (plist-get artifacts :names-sha256)))
          (should-error
           (nelisp-native-application-producer-install
            (plist-get artifacts :manifest) (plist-get artifacts :names)
            (plist-get artifacts :manifest-sha256) (make-string 64 ?0)))
          (should-not (fboundp 'nnap-hash-target)))
      (delete-directory directory t))))

(ert-deftest nnap-rejects-source-drift-and-producer-provider-collision ()
  (let* ((directory (make-temp-file "nnap-stale-" t))
         (artifacts (nnap-test--artifacts
                     directory
                     ";;; -*- lexical-binding: t; -*-\n(defun nnap-stale-target () 7)\n")))
    (unwind-protect
        (progn
          (nnap-test--write (plist-get artifacts :module)
                            ";;; -*- lexical-binding: t; -*-\n(defun nnap-stale-target () 8)\n")
          (should-error (nnap-test--install artifacts))
          (should-not (fboundp 'nnap-stale-target)))
      (delete-directory directory t)))
  (let* ((directory (make-temp-file "nnap-provider-" t))
         (artifacts (nnap-test--artifacts
                     directory
                     ";;; -*- lexical-binding: t; -*-\n(defun nnap-provider-collision () 7)\n")))
    (unwind-protect
        (should-error (nnap-test--install artifacts))
      (delete-directory directory t))))

(ert-deftest nnap-rejects-stale-admission-baseline-before-installation ()
  (let* ((directory (make-temp-file "nnap-baseline-" t))
         (artifacts (nnap-test--artifacts
                     directory
                     ";;; -*- lexical-binding: t; -*-\n(defun nnap-baseline-target () 7)\n")))
    (unwind-protect
        (cl-letf (((symbol-function 'nelisp-cc-fxir-native-all-rows)
                   (lambda () '((foreign-provider 993)))) )
          (should-error (nnap-test--install artifacts))
          (should-not (fboundp 'nnap-baseline-target)))
      (delete-directory directory t))))

;; Host tests exercise data validation only. They do not qualify a NeLisp
;; image, installed closure identity, producer binding, native execution or AOT.
