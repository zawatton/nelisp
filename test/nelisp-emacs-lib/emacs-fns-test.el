;;; emacs-fns-test.el --- Tests for emacs-fns  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the `emacs-fns' Emacs C core port (= mapcar / mapconcat /
;; mapc / nreverse / reverse / plist-{get,put,member} / provide).
;;
;; Under regular Emacs every function is already provided by the C
;; core, so the polyfill `unless (fboundp ...)' guards make our
;; definitions inert.  These tests still exercise the symbols since
;; they are bound either way, and any divergence between the polyfill
;; and the Emacs C original would surface here when the file is loaded
;; under NeLisp standalone.

;;; Code:

(require 'ert)
(require 'emacs-fns)

(defconst emacs-fns-test--source-file
  (expand-file-name
   "../src/emacs-fns.el"
   (file-name-directory (or load-file-name buffer-file-name))))

;;;; --- mapcar -------------------------------------------------------------

(ert-deftest emacs-fns-test/mapcar-basic ()
  (should (equal (mapcar #'1+ '(1 2 3)) '(2 3 4))))

(ert-deftest emacs-fns-test/mapcar-empty ()
  (should (equal (mapcar #'1+ nil) nil)))

(ert-deftest emacs-fns-test/mapcar-preserves-order ()
  (should (equal (mapcar #'identity '(:a :b :c :d)) '(:a :b :c :d))))


;;;; --- mapc ---------------------------------------------------------------

(ert-deftest emacs-fns-test/mapc-side-effects-and-returns-sequence ()
  (let ((collected nil))
    (let ((seq '(1 2 3)))
      (let ((ret (mapc (lambda (x) (setq collected (cons x collected))) seq)))
        ;; Mapc returns the original sequence.
        (should (eq ret seq))
        ;; Side-effects accumulated all elements.
        (should (equal (sort collected #'<) '(1 2 3)))))))


;;;; --- mapconcat ----------------------------------------------------------

(ert-deftest emacs-fns-test/mapconcat-joins-with-separator ()
  (should (equal (mapconcat #'identity '("a" "b" "c") ",") "a,b,c")))

(ert-deftest emacs-fns-test/mapconcat-empty-sequence-is-empty-string ()
  (should (equal (mapconcat #'identity nil ",") "")))

(ert-deftest emacs-fns-test/mapconcat-single-element-no-separator ()
  (should (equal (mapconcat #'identity '("only") "/") "only")))


;;;; --- reverse / nreverse -------------------------------------------------

(ert-deftest emacs-fns-test/reverse-does-not-mutate ()
  (let* ((src '(1 2 3))
         (rev (reverse src)))
    (should (equal rev '(3 2 1)))
    (should (equal src '(1 2 3)))))

(ert-deftest emacs-fns-test/nreverse-returns-reversed ()
  (should (equal (nreverse (list 1 2 3)) '(3 2 1))))

(ert-deftest emacs-fns-test/reverse-empty ()
  (should (equal (reverse nil) nil)))


;;;; --- plist-get / member / put -------------------------------------------

(ert-deftest emacs-fns-test/plist-get-finds-key ()
  (should (equal (plist-get '(:a 1 :b 2 :c 3) :b) 2)))

(ert-deftest emacs-fns-test/plist-get-missing-returns-nil ()
  (should (null (plist-get '(:a 1 :b 2) :z))))

(ert-deftest emacs-fns-test/plist-get-uses-eq ()
  ;; Symbols compare via eq; strings would NOT match (= same as Emacs default).
  (should (equal (plist-get '(:k "v") :k) "v"))
  (should (null (plist-get '("k" "v") "k"))))

(ert-deftest emacs-fns-test/plist-member-returns-tail ()
  (let ((tail (plist-member '(:a 1 :b 2 :c 3) :b)))
    (should (equal tail '(:b 2 :c 3)))))

(ert-deftest emacs-fns-test/plist-member-missing-returns-nil ()
  (should (null (plist-member '(:a 1) :z))))

(ert-deftest emacs-fns-test/plist-put-replaces-existing ()
  (let ((result (plist-put (list :a 1 :b 2) :a 99)))
    (should (equal (plist-get result :a) 99))
    (should (equal (plist-get result :b) 2))))

(ert-deftest emacs-fns-test/plist-put-appends-new ()
  (let ((result (plist-put (list :a 1) :b 2)))
    (should (equal (plist-get result :a) 1))
    (should (equal (plist-get result :b) 2))))

;;;; --- standalone feature registry -----------------------------------------

(ert-deftest emacs-fns-test/standalone-feature-index-initializes-from-features ()
  (let ((saved-features features))
    (setq features '(a b c))
    (emacs-fns--standalone-feature-index-reset)
    (unwind-protect
        (progn
          (should (= emacs-fns--standalone-feature-index-build-count 0))
          (should (eq (emacs-fns--standalone-featurep 'a) t))
          (should (eq (emacs-fns--standalone-featurep 'nope) nil))
          (should (= emacs-fns--standalone-feature-index-build-count 1)))
      (setq features saved-features)
      (emacs-fns--standalone-feature-index-reset))))

(ert-deftest emacs-fns-test/standalone-feature-index-fast-path-hits-and-misses ()
  (let ((saved-features features))
    (setq features '(alpha beta gamma))
    (emacs-fns--standalone-feature-index-reset)
    (unwind-protect
        (progn
          (dotimes (_ 32)
            (should (eq (emacs-fns--standalone-featurep 'alpha) t))
            (should (eq (emacs-fns--standalone-featurep 'missing) nil)))
          (should (= emacs-fns--standalone-feature-index-build-count 1)))
      (setq features saved-features)
      (emacs-fns--standalone-feature-index-reset))))

(ert-deftest emacs-fns-test/standalone-provide-like-insertion-is-idempotent ()
  (let ((saved-features features))
    (setq features '(existing))
    (emacs-fns--standalone-feature-index-reset)
    (unwind-protect
        (progn
          (should (eq (emacs-fns--standalone-provide 'added) 'added))
          (should (equal features '(added existing)))
          (should (= emacs-fns--standalone-feature-index-build-count 1))
          (should (eq (emacs-fns--standalone-provide 'added) 'added))
          (should (equal features '(added existing)))
          (should (eq (emacs-fns--standalone-featurep 'added) t))
          (should (= emacs-fns--standalone-feature-index-build-count 1)))
      (setq features saved-features)
      (emacs-fns--standalone-feature-index-reset))))

(ert-deftest emacs-fns-test/standalone-feature-index-rebind-invalidation ()
  (let ((saved-features features))
    (setq features '(first))
    (emacs-fns--standalone-feature-index-reset)
    (unwind-protect
        (progn
          (should (eq (emacs-fns--standalone-featurep 'second) nil))
          (should (= emacs-fns--standalone-feature-index-build-count 1))
          (setq features '(second third))
          (should (eq (emacs-fns--standalone-featurep 'second) t))
          (should (= emacs-fns--standalone-feature-index-build-count 2))
          (should (equal features '(second third))))
      (setq features saved-features)
      (emacs-fns--standalone-feature-index-reset))))


;;;; --- provide ------------------------------------------------------------

(ert-deftest emacs-fns-test/provide-accepts-subfeatures ()
  (let ((features (remove 'emacs-fns-test-provide-subfeatures features)))
    (should (eq (provide 'emacs-fns-test-provide-subfeatures
                         '(remote-wildcards))
                'emacs-fns-test-provide-subfeatures))
    (should (featurep 'emacs-fns-test-provide-subfeatures))))

(ert-deftest emacs-fns-test/standalone-require-rejects-nil-loader-result ()
  "A nil loader result must not bypass the required-feature check."
  (let ((missing-feature (make-symbol "emacs-fns-test-missing-feature")))
    (cl-letf (((symbol-function 'emacs-fns--load-required-file)
               (lambda (_path _noerror) nil)))
      (should-error
       (emacs-fns--load-and-check-required-feature
        missing-feature "/tmp/emacs-fns-test-missing.el" nil)
       :type 'error)
      (should-not
       (emacs-fns--load-and-check-required-feature
        missing-feature "/tmp/emacs-fns-test-missing.el" t)))))

(ert-deftest emacs-fns-test/standalone-bare-require-does-not-list-directories ()
  "Standalone bare-name `require' probes candidates without directory scans."
  (let* ((feature 'emacs-fns-test-bare-require-probe)
         (compiled-only-feature
          'emacs-fns-test-compiled-only-require-probe)
         (directory (make-temp-file "emacs-fns-require-" t))
         (file (expand-file-name (concat (symbol-name feature) ".el")
                                 directory))
         (compiled-file (concat file "c"))
         (compiled-only-file
          (expand-file-name
           (concat (symbol-name compiled-only-feature) ".elc")
           directory))
         (host-load (symbol-function 'load))
         (host-require (symbol-function 'require))
         (host-provide (symbol-function 'provide))
         (host-featurep (symbol-function 'featurep))
         (host-locate-file (symbol-function 'locate-file))
         (saved-load-suffixes load-suffixes)
         (saved-features features)
         (native-comp-enable-subr-trampolines nil)
         (directory-files-calls 0))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "(provide 'emacs-fns-test-bare-require-probe)\n"))
          (with-temp-file compiled-file
            (insert "not executable byte-code\n"))
          (with-temp-file compiled-only-file
            (insert "not executable byte-code\n"))
          ;; Re-evaluate the source under its standalone guard, then exercise
          ;; the installed `require' while retaining host file execution.
          (let ((emacs-version nil))
            (funcall host-load emacs-fns-test--source-file nil t t))
          (let ((load-path (list directory)))
            (should (equal load-suffixes '(".el")))
            (should
             (equal
              (locate-file (symbol-name feature) load-path
                           '(".elc" ".el" "")
                           #'emacs-fns--regular-file-p)
              file))
            (should-not
             (locate-file (symbol-name compiled-only-feature) load-path
                          '(".elc" ".el" "")
                          #'emacs-fns--regular-file-p))
            (cl-letf (((symbol-function 'directory-files)
                       (lambda (&rest _args)
                         (setq directory-files-calls
                               (1+ directory-files-calls))
                         nil))
                      ((symbol-function 'emacs-fns--load-required-file)
                       (lambda (path noerror)
                         (funcall host-load path noerror t t t))))
              (should (eq (require feature) feature))
              (should (= directory-files-calls 0)))))
      (fset 'require host-require)
      (fset 'provide host-provide)
      (fset 'featurep host-featurep)
      (fset 'locate-file host-locate-file)
      (setq load-suffixes saved-load-suffixes)
      (setq features saved-features)
      (when (file-directory-p directory)
        (delete-directory directory t)))))


;;;; --- Doc 200 string primitive requirement -----------------------------

(ert-deftest emacs-fns-test/missing-string-representation-primitive-signals ()
  "Missing Doc 200 string primitives must not return plausible wrong values."
  (dolist (primitive '(encode-coding-string decode-coding-string
                       multibyte-string-p string-as-multibyte
                       string-as-unibyte))
    (should-error
     (emacs-fns--missing-string-representation-primitive primitive)
     :type 'error)))


(provide 'emacs-fns-test)

;;; emacs-fns-test.el ends here
