;;; nelisp-cc-fxir-admission-test.el --- Optional application admission -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'ert)
(require 'cl-lib)
(defvar nelisp-cc-fxir-native-windows nil)
(let ((nelisp-cc-fxir-native-windows t)) (require 'nelisp-cc-fxir-native))
(require 'nelisp-cc-fxir-admission nil t)

(ert-deftest nfa-admission-api-present ()
  (should (fboundp 'nelisp-cc-fxir-admission-prepare))
  (should (fboundp 'nelisp-cc-fxir-admission-activate)))

(defun nfa-admission--write (file value)
  (with-temp-file file (insert value)))

(defun nfa-admission--row-file (file rows)
  (nfa-admission--write file (format "(defconst application-rows '%S)\n(provide 'application-rows)\n" rows)))

(defmacro nfa-admission--fixture (&rest body)
  (declare (indent 0))
  `(let* ((directory (make-temp-file "fxir-admission-test-" t))
          (row-file (expand-file-name "rows.el" directory))
          (source-file (expand-file-name "module.el" directory))
          (receipt-file (expand-file-name "receipt.sexp" directory))
          (rows '((application-leaf 20000 1 nil (:fn (:slot 0)) (1 1 -1))
                  (application-caller 20001 1 (application-leaf) (:fn (:call 1 1 (:slot 0))) (1 1 -1))))
          (nelisp-cc-fxir-native--additional-rows nil)
          (nelisp-cc-fxir-native--data-files nil)
          (nelisp-cc-fxir-native-windows nil)
          (nelisp-cc-fxir-native--source 'original-source)
          (nelisp-cc-fxir-native--boundaries '(original-boundary))
          (nelisp-cc-fxir-native--wrapper-names '(original-wrapper))
          producer descriptor expected)
     (unwind-protect
         (progn
           (nfa-admission--row-file row-file rows)
           (nfa-admission--write source-file ";;; -*- lexical-binding: t; -*-\n(defun application-leaf (x) x)\n(defun application-caller (x) (application-leaf x))\n")
           ;; These are explicitly synthetic producer artifacts for unit tests.
           ;; Real producer artifact capture is a separate source-level smoke.
           (dolist (role '(:binary :cold :build-manifest :stdout))
             (let ((file (expand-file-name (substring (symbol-name role) 1) directory)))
               (nfa-admission--write file
                (if (eq role :stdout)
                    "HARVEST application-leaf ok id=20000 words=3\nHARVEST application-caller ok id=20001 words=5\n"
                  "synthetic test artifact\n"))
               (setq producer (plist-put producer role file))))
           (setq descriptor (nelisp-cc-fxir-admission-capture-receipt
                             row-file 'application-rows (list source-file) producer receipt-file)
                 expected (plist-get (nelisp-cc-fxir-admission--pin row-file) :sha256))
           ,@body)
       (delete-directory directory t))))

(ert-deftest nfa-admission-source-generation-and-install-table ()
  (nfa-admission--fixture
    (let* ((a (nelisp-cc-fxir-admission-prepare row-file 'application-rows expected descriptor))
           (b (nelisp-cc-fxir-admission-prepare row-file 'application-rows expected descriptor)))
      (should (equal a b))
      (should-not nelisp-cc-fxir-native--additional-rows)
      (let* ((result (nelisp-cc-fxir-admission-activate a))
             (text (nelisp-cc-fxir-admission--print nelisp-cc-fxir-native--source))
             (table (nelisp-cc-fxir-native-install-table-source)))
        (should-not (plist-get result :native-qualified))
        (dolist (name '(nc_20000 nc_20001 nc_d_20000))
          (should (string-match-p (symbol-name name) text)))
        (should (memq 'nc_d_20000 nelisp-cc-fxir-native--boundaries))
        (should (string-match-p "application-caller" table))
        (should (equal (nelisp-cc-fxir-admission-rows a) rows))
        ;; Returned row copies cannot mutate the registered snapshot.
        (setcar (car (nelisp-cc-fxir-admission-rows a)) 'mutated)
        (should (eq (caar (nelisp-cc-fxir-admission-rows a)) 'application-leaf))))))

(ert-deftest nfa-admission-default-source-unchanged ()
  (nfa-admission--fixture
    (let ((before (nelisp-cc-fxir-native-source))
          (table (nelisp-cc-fxir-native-install-table-source)))
      (nelisp-cc-fxir-admission-prepare row-file 'application-rows expected descriptor)
      (should (equal before (nelisp-cc-fxir-native-source)))
      (should (equal table (nelisp-cc-fxir-native-install-table-source)))
      (should (equal nelisp-cc-fxir-native--source 'original-source)))))

(ert-deftest nfa-admission-missing-or-stale-provenance-refused ()
  (nfa-admission--fixture
    (should-error (nelisp-cc-fxir-admission-prepare row-file 'application-rows expected nil))
    (should-error (nelisp-cc-fxir-admission-prepare row-file 'application-rows "wrong" descriptor))
    (nfa-admission--write source-file ";;; -*- lexical-binding: t; -*-\n(defun application-leaf (x) nil)\n")
    (should-error (nelisp-cc-fxir-admission-prepare row-file 'application-rows expected descriptor))))

(ert-deftest nfa-admission-malformed-arity-callee-controls ()
  (nfa-admission--fixture
    (dolist (bad '(((a 20000 1 nil (:fn (:slot 9)) (1 1 -1)))
                   ((a 20000 1 nil (:fn (:slot 0)) (2 1 -1)))
                   ((a 20000 1 nil (:fn (:call 1 0)) (1 1 -1)))
                   ((a 20000 1 (unused) (:fn (:slot 0)) (1 1 -1)))
                   ((a 20000 1 (callee) (:fn (:call 1 2 (:slot 0))) (1 1 -1)))
                   ((a 20000 1 nil (:fn (:slot 0)) (1 1 -1) :mutant))))
      (nfa-admission--row-file row-file bad)
      (should-error (nelisp-cc-fxir-admission-prepare
                     row-file 'application-rows
                     (plist-get (nelisp-cc-fxir-admission--pin row-file) :sha256) descriptor)))))

(ert-deftest nfa-admission-duplicate-id-name-controls ()
  (nfa-admission--fixture
    (dolist (baseline (list (list (list 'other 20000 1 nil '(:fn (:slot 0)) '(1 1 -1)))
                            (list (list 'application-leaf 19 1 nil '(:fn (:slot 0)) '(1 1 -1)))))
      (cl-letf (((symbol-function 'nelisp-cc-fxir-native-all-rows) (lambda () baseline)))
        (should-error (nelisp-cc-fxir-admission-prepare row-file 'application-rows expected descriptor))))
    (should-error (nelisp-cc-fxir-admission--unique (append rows (list (car rows))) nil))))

(ert-deftest nfa-admission-source-arity-and-harvest-controls ()
  (nfa-admission--fixture
    (nfa-admission--write source-file ";;; -*- lexical-binding: t; -*-\n(defun application-leaf (x y) x)\n(defun application-caller (x) (application-leaf x))\n")
    (should-error (nelisp-cc-fxir-admission-capture-receipt
                   row-file 'application-rows (list source-file) producer receipt-file))
    (should (equal (nelisp-cc-fxir-admission--arity '(a &optional b &rest rest)) '(1 -1 2)))
    (dolist (args '((a &rest) (a &key b) (a a) (&rest x y)))
      (should-error (nelisp-cc-fxir-admission--arity args)))))

(ert-deftest nfa-admission-literal-file-never-evaluated ()
  (nfa-admission--fixture
    (let ((executed nil))
      (nfa-admission--write row-file "(setq executed t)\n")
      (should-error (nelisp-cc-fxir-admission-prepare
                     row-file 'application-rows
                     (plist-get (nelisp-cc-fxir-admission--pin row-file) :sha256) descriptor))
      (should-not executed))
    (should-error (nelisp-cc-fxir-admission--forms "(quote good)\n(unclosed"))
    (should-error (nelisp-cc-fxir-admission--forms "#1=(x . #1#)"))))

(ert-deftest nfa-admission-transactional-generation-failure ()
  (nfa-admission--fixture
    (let ((snapshot (nelisp-cc-fxir-admission-prepare row-file 'application-rows expected descriptor)))
      (cl-letf (((symbol-function 'nelisp-cc-fxir-native-install-table-source)
                 (lambda () (error "injected install-table failure"))))
        (should-error (nelisp-cc-fxir-admission-activate snapshot)))
      (should-not nelisp-cc-fxir-native--additional-rows)
      (should (eq nelisp-cc-fxir-native--source 'original-source))
      (should (equal nelisp-cc-fxir-native--boundaries '(original-boundary)))
      (should (equal nelisp-cc-fxir-native--wrapper-names '(original-wrapper)))
      (should-not nelisp-cc-fxir-native--data-files)
      (let ((original-digest (symbol-function 'nelisp-cc-fxir-admission--digest)))
        (cl-letf (((symbol-function 'nelisp-cc-fxir-admission--digest)
                   (lambda (data)
                     (if (eq (car-safe data) 'seq) (error "injected final receipt failure")
                       (funcall original-digest data)))))
          (should-error (nelisp-cc-fxir-admission-activate snapshot))))
      (should-not nelisp-cc-fxir-native--additional-rows)
      (should (eq nelisp-cc-fxir-native--source 'original-source)))))

(ert-deftest nfa-admission-snapshot-and-reread-pins ()
  (nfa-admission--fixture
    (let* ((snapshot (nelisp-cc-fxir-admission-prepare row-file 'application-rows expected descriptor))
           (mutant (copy-tree snapshot)))
      (setcar (car (plist-get mutant :rows)) 'changed)
      (should-error (nelisp-cc-fxir-admission-rows mutant))
      (let ((manifest (copy-tree (plist-get snapshot :manifest)))
            (invalid (copy-tree snapshot)))
        (setq manifest (plist-put manifest :schema 999)
              invalid (plist-put invalid :manifest manifest)
              invalid (plist-put invalid :manifest-sha256 (nelisp-cc-fxir-admission--digest manifest)))
        (should-error (nelisp-cc-fxir-admission-rows invalid)))
      (nelisp-cc-fxir-admission-activate snapshot)
      (nfa-admission--write row-file "(error \"must not be evaluated\")\n")
      (should-error (nelisp-cc-fxir-native--rows))
      (should-error (nelisp-cc-fxir-native-install-table-source)))))

(ert-deftest nfa-admission-builder-load-and-baseline-changes-refused ()
  (nfa-admission--fixture
    (let ((snapshot (nelisp-cc-fxir-admission-prepare row-file 'application-rows expected descriptor)))
      (let ((original-featurep (symbol-function 'featurep)))
        (cl-letf (((symbol-function 'featurep)
                   (lambda (feature &optional subfeature)
                     (or (eq feature 'nelisp-standalone-build)
                         (funcall original-featurep feature subfeature)))))
          (should-error (nelisp-cc-fxir-admission-activate snapshot))))
      (cl-letf (((symbol-function 'nelisp-cc-fxir-native-all-rows)
                 (lambda () '((other 2 1 nil (:fn (:slot 0)) (1 1 -1))))))
        (should-error (nelisp-cc-fxir-admission-activate snapshot))))))

(ert-deftest nfa-admission-generation-budget-billion-locals-before-generator ()
  (nfa-admission--fixture
    (let ((oversized '((application-leaf 700000 1000000000 nil (:fn (:slot 0)) (1 1 -1))))
          (generator-called nil))
      ;; The old analyzer accepts this tiny row; never run its real generator.
      (should (plist-get (nelisp-cc-fxir-analysis-row (car oversized)) :valid))
      (nfa-admission--row-file row-file oversized)
      (nfa-admission--write (plist-get producer :stdout)
                            "HARVEST application-leaf ok id=700000 words=3\n")
      (cl-letf (((symbol-function 'nelisp-cc-fxir-native-source)
                 (lambda () (setq generator-called t)
                   (princ "BUDGET-CONTROL generator-called=yes\n") '(seq))))
        (should-error
         (let* ((binding (nelisp-cc-fxir-admission-capture-receipt
                          row-file 'application-rows (list source-file) producer receipt-file))
                (snapshot (nelisp-cc-fxir-admission-prepare
                           row-file 'application-rows
                           (plist-get (nelisp-cc-fxir-admission--pin row-file) :sha256) binding)))
           (nelisp-cc-fxir-admission-activate snapshot))))
      (should-not generator-called))))

(ert-deftest nfa-admission-generation-budget-aggregate-and-malformed ()
  (nfa-admission--fixture
    (let* ((snapshot (nelisp-cc-fxir-admission-prepare row-file 'application-rows expected descriptor))
           (generator-called nil)
           (aggregate (cl-loop for i below 9 collect
                               (list (intern (format "budget-%d" i)) (+ 30000 i)
                                     512 nil '(:fn (:slot 0)) '(1 1 -1))))
           (malformed '((application-leaf 700000 1 nil (:fn (:const 1048576)) (1 1 -1)))))
      (should (plist-get (nelisp-cc-fxir-analysis-row (car malformed)) :valid))
      (cl-letf (((symbol-function 'nelisp-cc-fxir-native-source)
                 (lambda () (setq generator-called t) (error "Generator invoked"))))
        (dolist (bad (list aggregate malformed))
          (nfa-admission--row-file row-file bad)
          (should-error (nelisp-cc-fxir-admission--read-rows
                         row-file 'application-rows
                         (plist-get (nelisp-cc-fxir-admission--pin row-file) :sha256)))
          ;; Recomputed row and manifest digests do not bypass snapshot validation.
          (let* ((mutant (copy-tree snapshot))
                 (manifest (plist-get mutant :manifest)))
            (setq mutant (plist-put mutant :rows bad)
                  manifest (plist-put manifest :rows-sha256 (nelisp-cc-fxir-admission--digest bad))
                  mutant (plist-put mutant :manifest manifest)
                  mutant (plist-put mutant :manifest-sha256 (nelisp-cc-fxir-admission--digest manifest)))
            (should-error (nelisp-cc-fxir-admission-activate mutant))))
        (should-not generator-called)
        (should-not nelisp-cc-fxir-native--additional-rows)
        (should (eq nelisp-cc-fxir-native--source 'original-source))
        (should (equal nelisp-cc-fxir-native--boundaries '(original-boundary)))
        (should (equal nelisp-cc-fxir-native--wrapper-names '(original-wrapper)))))))

(ert-deftest nfa-admission-policy-evidence-isolation ()
  (nfa-admission--fixture
    (let* ((snapshot (nelisp-cc-fxir-admission-prepare row-file 'application-rows expected descriptor))
           (policy (plist-get (plist-get (plist-get snapshot :manifest) :generation-budget) :policy))
           (original (copy-tree nelisp-cc-fxir-admission-generation-policy)))
      (unwind-protect
          (progn
            (setcdr (assq 'nlocals (plist-get policy :aggregate)) 999999999)
            (should (equal original nelisp-cc-fxir-admission-generation-policy))
            (should-error (nelisp-cc-fxir-admission-rows snapshot)))
        (setq nelisp-cc-fxir-admission-generation-policy original)))))

(ert-deftest nfa-admission-code-index-before-generator ()
  (nfa-admission--fixture
    (let ((bad '((application-leaf 700000 1 nil (:fn (:const 1048576)) (1 1 -1))) )
          (called nil))
      (should (plist-get (nelisp-cc-fxir-analysis-row (car bad)) :valid))
      (nfa-admission--row-file row-file bad)
      (cl-letf (((symbol-function 'nelisp-cc-fxir-native-source)
                 (lambda () (setq called t) (error "Generator invoked"))))
        (should-error (nelisp-cc-fxir-admission--read-rows
                       row-file 'application-rows
                       (plist-get (nelisp-cc-fxir-admission--pin row-file) :sha256)))
        (should-not called)))))

(provide 'nelisp-cc-fxir-admission-test)
