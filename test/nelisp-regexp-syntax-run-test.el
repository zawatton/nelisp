;;; nelisp-regexp-syntax-run-test.el --- live syntax-run plans -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'nelisp-stdlib-regexp)
(defvar regex-syntax-run-matcher)
(defvar regex-syntax-run-data-reader)
(defvar regex-syntax-run-output)

(defconst nelisp-regexp-syntax-run-test-root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))

(defconst nelisp-regexp-syntax-run-test-standard-ascii
  (with-syntax-table (standard-syntax-table)
    (apply #'string (mapcar #'char-syntax (number-sequence 0 127))))
  "Fresh GNU default classifications, captured before ERT mutates shared tables.")

(ert-deftest nelisp-regexp-syntax-run-gnu-fixture ()
  "Actual matcher results equal the pinned GNU 31.1 output."
  (let ((default-directory nelisp-regexp-syntax-run-test-root))
    (should
     (equal
      (with-temp-buffer
        (let ((regex-syntax-run-matcher 'nlre-string-match)
              (regex-syntax-run-data-reader 'nlre-match-data)
              (regex-syntax-run-output (current-buffer))
              (nlre--last-buffer nil)
              (nlre--last-caps nil))
          (load (expand-file-name "test/fixtures/regex-syntax-run.el") nil t))
        (buffer-string))
      (with-temp-buffer
        (insert-file-contents "test/fixtures/regex-syntax-run.gnu-31.1.expected")
        (buffer-string))))))

(ert-deftest nelisp-regexp-syntax-run-uses-plan ()
  "Single-character alternatives bypass recursive group backtracking."
  (let ((case-fold-search nil)
        (nlre--last-buffer nil)
        (nlre--last-caps nil))
    (dolist (pattern '("\\`\\(\\sw\\|\\s_\\)*\\'"
                       "\\`\\(\\sw\\|\\s_\\)+\\'"
                       "\\`\\(?:\\sw\\|\\s_\\)*\\'"
                       "\\`\\sw*\\'"))
      (should (eq (aref (aref (nlre--compile-pattern pattern nil) 2) 0)
                  :syntax-run))
      (cl-letf (((symbol-function 'nlre--match-list)
                 (lambda (&rest _) (error "entered the backtracker"))))
        (should (eq (nlre-string-match pattern "word") 0))))))

(ert-deftest nelisp-regexp-syntax-run-live-classification ()
  "A warmed run calls the current classifier exactly once per character."
  (let* ((pattern "\\`\\(\\sw\\|\\s_\\)*\\'")
         (case-fold-search nil)
         (nlre--last-buffer nil)
         (nlre--last-caps nil)
         (original (symbol-function 'char-syntax))
         (calls 0))
    (nlre-string-match pattern "a_b")
    (cl-letf (((symbol-function 'char-syntax)
               (lambda (character)
                 (setq calls (1+ calls))
                 (funcall original character))))
      (should (eq (nlre-string-match pattern "a_b") 0))
      (should (= calls 3)))
    (should (equal nlre--standard-ascii-syntax
                   nelisp-regexp-syntax-run-test-standard-ascii))
    ;; GNU reads the live table directly.  The bootstrap representation has
    ;; class designators in hash slots rather than raw char-table entries.
    (let* ((parent (record 'nelisp--syntax-table (make-hash-table :test 'eq) nil))
           (table (record 'nelisp--syntax-table (make-hash-table :test 'eq) parent))
           (plan (vector :syntax-run '(?w) nil 0))
           (caps (vector nil))
           (nlre--syntax-table-reader (lambda () table)))
      (cl-letf (((symbol-function 'char-syntax)
                 (lambda (_) (error "Public classifier must not run for bootstrap tables"))))
        (should (eq (nlre--syntax-run-plan-match plan "a" 0 1 caps) 0))
        (puthash ?a ?_ (aref parent 1))
        (should-not (nlre--syntax-run-plan-match plan "a" 0 1 caps))
        (puthash ?a ?w (aref table 1))
        (should (eq (nlre--syntax-run-plan-match plan "a" 0 1 caps) 0))
        (puthash ?a nil (aref table 1))
        (should-not (nlre--syntax-run-plan-match plan "a" 0 1 caps))))))

(ert-deftest nelisp-regexp-syntax-run-rejects-unsupported-shapes ()
  "Capturing alternatives and variable-width continuations retain fallback."
  (dolist (pattern '("\\`\\(\\(\\sw\\)\\|\\s_\\)*\\'"
                     "\\`\\(\\sw\\|__\\)*\\'"
                     "\\`\\(\\sw\\|\\s_\\)*\\sw\\'"
                     "\\`\\(\\sw\\|\\s_\\)*?\\'"
                     "\\`\\(\\Sw\\|\\s_\\)*\\'"))
    (let ((plan (aref (nlre--compile-pattern pattern nil) 2)))
      (should-not (and plan (eq (aref plan 0) :syntax-run))))))

(ert-deftest nelisp-regexp-syntax-run-max-end-fallback ()
  "Bounded matching retains its ordinary continuation and anchor handling."
  (let ((case-fold-search nil)
        (nlre--last-buffer nil)
        (nlre--last-caps nil)
        (pattern "\\`\\(\\sw\\|\\s_\\)*\\'"))
    (cl-letf (((symbol-function 'nlre--syntax-run-plan-match)
               (lambda (&rest _) (error "entered the unbounded plan"))))
      (should (eq (nlre-string-match pattern "a_b" 0 3) 0))
      (should (equal (nlre-match-data t) '(0 3 2 3)))
      (should-not (nlre-string-match pattern "a_b" 0 2)))))

(ert-deftest nelisp-regexp-syntax-run-rejects-zero-group-numbers ()
  "Explicit group numbers must start with a nonzero digit, as in GNU."
  (let ((nlre--last-buffer nil)
        (nlre--last-caps nil))
    (dolist (pattern '("\\`\\(?0:\\sw\\|\\s_\\)*\\'"
                       "\\`\\(?01:\\sw\\|\\s_\\)*\\'"))
      (should (equal (should-error (nlre-string-match pattern "ab")
                                  :type 'invalid-regexp)
                     '(invalid-regexp "Invalid regular expression"))))))

(ert-deftest nelisp-regexp-syntax-run-capture-register-boundary ()
  "GNU records groups through 255 and ignores excess logical registers."
  (let ((case-fold-search nil)
        (nlre--last-buffer nil)
        (nlre--last-caps nil))
    (should (eq (nlre-string-match "\\`\\(?255:\\sw\\|\\s_\\)*\\'" "ab") 0))
    (should (eq (nlre-match-beginning 255) 1))
    (should (eq (nlre-match-end 255) 2))
    (dolist (group '(256 1000000))
      (let* ((pattern (format "\\`\\(?%d:\\sw\\|\\s_\\)*\\'" group))
             (compiled (nlre--compile-pattern pattern nil)))
        (should (= (aref compiled 1) 256))
        (should (eq (nlre-string-match pattern "ab") 0))
        (should-not (nlre-match-beginning group))
        (should (equal (nlre-match-data t) '(0 2)))))
    (should (eq (nlre-string-match "\\`\\(?256:a\\)\\(b\\)\\'" "ab") 0))
    (should (equal (nlre-match-data t) '(0 2)))))

(ert-deftest nelisp-regexp-syntax-run-rejects-group-number-overflow ()
  "Group numbers beyond GNU's signed int range fail before allocation."
  (dolist (number '("2147483648" "999999999999999999999999"))
    (let ((pattern (concat "\\`\\(?" number ":\\sw\\|\\s_\\)*\\'")))
      ;; Parse directly so the pre-fix negative control cannot attempt a
      ;; multi-billion-slot capture vector before reporting its acceptance.
      (should (equal (should-error (nlre--parse pattern)
                                  :type 'invalid-regexp)
                     '(invalid-regexp "Regular expression too big"))))))

(ert-deftest nelisp-regexp-syntax-run-failure-preserves-match-data ()
  "A failed run leaves the previous successful match data in place."
  (let ((case-fold-search nil)
        (nlre--last-buffer nil)
        (nlre--last-caps nil)
        (pattern "\\`\\(\\sw\\|\\s_\\)*\\'"))
    (should (eq (nlre-string-match pattern "a_b") 0))
    (should-not (nlre-string-match pattern "a b"))
    (should (equal (nlre-match-data t) '(0 3 2 3)))))

(provide 'nelisp-regexp-syntax-run-test)
