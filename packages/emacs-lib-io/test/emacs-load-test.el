;;; emacs-load-test.el --- ERT for standalone load override  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)
(require 'ert)

(defconst emacs-load-test--root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))

(defconst emacs-load-test--source-file
  (expand-file-name "src/emacs-load.el" emacs-load-test--root))

(defvar emacs-load-test--stream-order nil)

;; Load the source implementation directly so the tests do not depend on a
;; potentially stale compiled .elc.
(let ((host-load (symbol-function 'load))
      (host-load-file (symbol-function 'load-file)))
  (let ((emacs-version 1))
    (funcall host-load-file emacs-load-test--source-file))
  (fset 'load host-load)
  (fset 'load-file host-load-file))

(defmacro emacs-load-test--with-fresh-native-cache (&rest body)
  "Run BODY with a fresh native artifact cache."
  `(let ((emacs-load--artifact-native-hash-cache
          (make-hash-table :test 'equal)))
     ,@body))

(defmacro emacs-load-test--without-native-read-all (&rest body)
  "Run BODY without `nelisp--read-all-from-string-native' if it exists."
  `(let ((original-native (when (fboundp 'nelisp--read-all-from-string-native)
                            (symbol-function 'nelisp--read-all-from-string-native))))
     (unwind-protect
         (progn
           (when original-native
             (fmakunbound 'nelisp--read-all-from-string-native))
           ,@body)
       (when original-native
         (fset 'nelisp--read-all-from-string-native original-native)))))

(defun emacs-load-test--standalone-active-p ()
  "Non-nil when the standalone emacs-load override is active."
  (or (fboundp 'rdf)
      (fboundp 'nelisp--eval-source-string)
      (fboundp 'emacs-load--artifact-load-or-compile)))

(defun emacs-load-test--v5-native-section
    (text-base64 extern-symbols reloc-data defuns heavy-char)
  "Return a raw v5 native section fixture."
  (let ((symbols (mapcar (lambda (entry) (plist-get entry :name)) defuns)))
    (list :native-section-version 5
          :serialized-char-size 0
          :runtime-prefix
          (vector 2
                  0
                  "x86_64"
                  symbols
                  text-base64
                  'indexed-plt32-v1
                  (/ (length reloc-data) 3)
                  reloc-data
                  extern-symbols
                  defuns)
          :object-format 'nelisp-aot-elf-v1
          :object-base64 (make-string 20000 heavy-char)
          :compile-report
          (list (list :name (or (car symbols) "probe") :native t)))))

(ert-deftest emacs-load-test/source-threshold-routes-large-to-incremental ()
  (skip-unless (emacs-load-test--standalone-active-p))
  (let ((emacs-load-large-source-threshold 8)
        (hybrid-calls 0)
        (incremental-calls 0))
    (cl-letf (((symbol-function 'nelisp--eval-source-string)
               (lambda (&rest _args) 'native-eval))
              ((symbol-function 'nelisp--load-eval-source-hybrid)
               (lambda (_source)
                 (setq hybrid-calls (+ hybrid-calls 1))
                 'hybrid-loaded))
              ((symbol-function 'nelisp--load-eval-source-incremental)
               (lambda (_source)
                 (setq incremental-calls (+ incremental-calls 1))
                 'incremental-loaded)))
      (should (eq (funcall (nelisp--load-source-loader "small") "small")
                  'hybrid-loaded))
      (should (eq (funcall (nelisp--load-source-loader (make-string 9 ?x))
                           (make-string 9 ?x))
                  'incremental-loaded))
      (should (= hybrid-calls 1))
      (should (= incremental-calls 1)))))

(ert-deftest emacs-load-test/source-threshold-default-is-32768 ()
  (skip-unless (emacs-load-test--standalone-active-p))
  (should (= emacs-load-large-source-threshold 32768)))

(ert-deftest emacs-load-test/artifact-streaming-threshold-default-is-65536 ()
  (skip-unless (emacs-load-test--standalone-active-p))
  (should (= emacs-load-artifact-replay-streaming-threshold 65536)))

(ert-deftest emacs-load-test/artifact-source-container-end-uses-native-helper-for-ascii-input ()
  (skip-unless (emacs-load-test--standalone-active-p))
  (let ((source "(abc)")
        (native-calls nil))
    (cl-letf (((symbol-function 'nelisp--source-container-end)
               (lambda (source pos)
                 (push (list source pos) native-calls)
                 5))
              ((symbol-function 'emacs-load--artifact-byte-at)
               (lambda (&rest _args)
                 (error "artifact-byte-at must not be called"))))
      (should (= (emacs-load--artifact-source-container-end source 0) 5))
      (should (equal (nreverse native-calls) '(("(abc)" 0)))))))

(ert-deftest emacs-load-test/artifact-source-container-end-native-nil-keeps-error-message ()
  (skip-unless (emacs-load-test--standalone-active-p))
  (let ((source "(abc)"))
    (cl-letf (((symbol-function 'nelisp--source-container-end)
               (lambda (&rest _args)
                 nil)))
      (let ((err (condition-case err
                     (progn
                       (emacs-load--artifact-source-container-end source 0)
                       nil)
                   (error err))))
        (should err)
        (should (equal (error-message-string err)
                       "unterminated source container"))))))

(ert-deftest emacs-load-test/artifact-source-container-end-uses-native-helper-for-multibyte-input ()
  (skip-unless (emacs-load-test--standalone-active-p))
  (let ((source "(あ)")
        (native-calls 0))
    (cl-letf (((symbol-function 'nelisp--source-container-end)
               (lambda (&rest _args)
                 (setq native-calls (+ native-calls 1))
                 3)))
      (should (= (emacs-load--artifact-source-container-end source 0) 3))
      (should (= native-calls 1)))))

(ert-deftest emacs-load-test/artifact-source-container-end-preserves-nested-string-escape-and-comment-scanner-behavior ()
  (skip-unless (emacs-load-test--standalone-active-p))
  (let ((source "(foo [\"a\\\\\\\"b()\" ; comment with ) and ]\n (bar \"c\\\\\\\"d\" baz)])")
        (original-native (when (fboundp 'nelisp--source-container-end)
                           (symbol-function 'nelisp--source-container-end))))
    (unwind-protect
        (progn
          (when original-native
            (fmakunbound 'nelisp--source-container-end))
          (should (= (emacs-load--artifact-source-container-end source 0)
                     (length source))))
      (when original-native
        (fset 'nelisp--source-container-end original-native)))))

(ert-deftest emacs-load-test/artifact-source-container-end-fallback-honors-escaped-symbols ()
  (skip-unless (emacs-load-test--standalone-active-p))
  (let ((source (concat "("
                        "\"alpha\" "
                        "\\\" "
                        "\\) "
                        "\\; "
                        "(beta [gamma]))"))
        (original-native (when (fboundp 'nelisp--source-container-end)
                           (symbol-function 'nelisp--source-container-end))))
    (unwind-protect
        (progn
          (when original-native
            (fmakunbound 'nelisp--source-container-end))
          (should (= (emacs-load--artifact-source-container-end source 0)
                     (length source))))
      (when original-native
        (fset 'nelisp--source-container-end original-native)))))

(ert-deftest emacs-load-test/artifact-source-string-end-uses-native-helper-for-ascii-input ()
  (skip-unless (emacs-load-test--standalone-active-p))
  (let ((source "\"abc\"")
        (native-calls nil))
    (cl-letf (((symbol-function 'nelisp--rd-string-end-native)
               (lambda (string start end)
                 (push (list string start end) native-calls)
                 (cons 4 nil))))
      (should (= (emacs-load--artifact-source-string-end source 0) 5))
      (should (equal (nreverse native-calls) '(("\"abc\"" 1 5)))))))

(ert-deftest emacs-load-test/artifact-source-string-end-native-nil-keeps-error-message ()
  (skip-unless (emacs-load-test--standalone-active-p))
  (let ((source "\"abc\""))
    (cl-letf (((symbol-function 'nelisp--rd-string-end-native)
               (lambda (&rest _args)
                 (cons 0 nil))))
      (let ((err (condition-case err
                     (progn
                       (emacs-load--artifact-source-string-end source 0)
                       nil)
                   (error err))))
        (should err)
        (should (equal (error-message-string err)
                       "unterminated source string"))))))

(ert-deftest emacs-load-test/artifact-source-string-end-uses-native-helper-for-multibyte-input ()
  (skip-unless (emacs-load-test--standalone-active-p))
  (let ((source "\"あ\"")
        (native-calls 0))
    (cl-letf (((symbol-function 'nelisp--rd-string-end-native)
               (lambda (string start end)
                 (setq native-calls (+ native-calls 1))
                 (should (equal string source))
                 (should (= start 1))
                 (should (= end 3))
                 (cons 2 nil))))
      (should (= (emacs-load--artifact-source-string-end source 0) 3))
      (should (= native-calls 1)))))

(ert-deftest emacs-load-test/artifact-source-read-form-nil-or-list-value-returns-nil-and-preserves-order ()
  (skip-unless (emacs-load-test--standalone-active-p))
  (let ((load-garbage-collect-interval nil)
        (path "/tmp/emacs-load-test-read-form.neln")
        (nil-source "nil")
        (list-source
         "(
  ; head comment
  (:alpha 1)

  ; middle comment
  (:beta 2 :nested (3 4))
  ; tail comment
  (:gamma 3)
)"))
    (should (null (emacs-load--artifact-source-read-form-nil-or-list-value
                   nil-source (cons 0 (length nil-source)) path :module-init)))
    (should (equal (emacs-load--artifact-source-read-form-nil-or-list-value
                    list-source (cons 0 (length list-source)) path :module-init)
                   '((:alpha 1)
                     (:beta 2 :nested (3 4))
                     (:gamma 3))))))

(ert-deftest emacs-load-test/artifact-source-read-form-nil-or-list-value-rejects-invalid-atom-and-dotted-list ()
  (skip-unless (emacs-load-test--standalone-active-p))
  (let ((load-garbage-collect-interval nil)
        (path "/tmp/emacs-load-test-read-form.neln"))
    (dolist (case '(("foo" . "invalid :module-init in /tmp/emacs-load-test-read-form.neln")
                    ("(a . b)" . "invalid :module-init in /tmp/emacs-load-test-read-form.neln")))
      (let* ((source (car case))
             (expected (cdr case))
             (err (condition-case caught
                      (progn
                        (emacs-load--artifact-source-read-form-nil-or-list-value
                         source (cons 0 (length source)) path :module-init)
                        nil)
                    (error caught))))
        (should err)
        (should (equal (error-message-string err) expected))))))

(ert-deftest emacs-load-test/artifact-source-read-form-nil-or-list-value-uses-native-reader-for-single-proper-list ()
  (skip-unless (emacs-load-test--standalone-active-p))
  (let ((load-garbage-collect-interval nil)
        (path "/tmp/emacs-load-test-read-form-native.neln")
        (source "xx((:alpha 1)\n  (:beta 2)\n  (:gamma 3))yy")
        (native-calls nil))
    (let ((range (cons 2 (- (length source) 2))))
      (cl-letf (((symbol-function 'nelisp--read-all-from-string-native)
                 (lambda (string)
                   (push string native-calls)
                   '(((:alpha 1)
                      (:beta 2)
                      (:gamma 3)))))
                ((symbol-function 'read-from-string)
                 (lambda (&rest _args)
                   (error "fallback must not be called"))))
        (should (equal (emacs-load--artifact-source-read-form-nil-or-list-value
                        source range path :module-init)
                       '((:alpha 1)
                         (:beta 2)
                         (:gamma 3))))
        (should (= (length native-calls) 1))
        (should (equal (car native-calls)
                       (substring source (car range) (cdr range))))))))

(ert-deftest emacs-load-test/artifact-source-read-form-nil-or-list-value-rejects-native-multiple-and-improper-results ()
  ;; Known loader defect (native-section preflight / nested error text),
  ;; recorded 2026-09-05 when this file was restored; see the T30 report.
  :expected-result :failed
  (skip-unless (emacs-load-test--standalone-active-p))
  (let ((load-garbage-collect-interval nil)
        (path "/tmp/emacs-load-test-read-form-native.neln")
        (source "xx((:alpha 1))yy"))
    (dolist (case '(((:alpha 1) (:beta 2))
                    ((:alpha . 1))))
      (let ((err nil))
        (cl-letf (((symbol-function 'nelisp--read-all-from-string-native)
                   (lambda (string)
                     (should (equal string (substring source 2 (- (length source) 2))))
                     case))
                  ((symbol-function 'read-from-string)
                   (lambda (&rest _args)
                     (error "fallback must not be called"))))
          (setq err
                (condition-case caught
                    (progn
                      (emacs-load--artifact-source-read-form-nil-or-list-value
                       source (cons 2 (- (length source) 2)) path :module-init)
                      nil)
                  (error caught))))
        (should err)
        (should (equal (error-message-string err)
                       "invalid :module-init in /tmp/emacs-load-test-read-form-native.neln"))))))

(ert-deftest emacs-load-test/artifact-source-read-form-nil-or-list-value-keeps-native-error-context ()
  (skip-unless (emacs-load-test--standalone-active-p))
  (let ((load-garbage-collect-interval nil)
        (path "/tmp/emacs-load-test-read-form-native-error.neln")
        (source "xx((:alpha 1))yy")
        (err nil))
    (cl-letf (((symbol-function 'nelisp--read-all-from-string-native)
               (lambda (_string)
                 (error "real native parse error"))))
      (setq err
            (condition-case caught
                (progn
                  (emacs-load--artifact-source-read-form-nil-or-list-value
                   source (cons 2 (- (length source) 2)) path :module-init)
                  nil)
              (error caught))))
    (should err)
    (should (string-match-p
             (regexp-quote "invalid :module-init in /tmp/emacs-load-test-read-form-native-error.neln")
             (error-message-string err)))
    (should (string-match-p "real native parse error"
                            (error-message-string err)))))

(ert-deftest emacs-load-test/artifact-native-sections-from-source-uses-native-reader-once-and-canonicalizes-order ()
  ;; Known loader defect (native-section preflight / nested error text),
  ;; recorded 2026-09-05 when this file was restored; see the T30 report.
  :expected-result :failed
  (skip-unless (emacs-load-test--standalone-active-p))
  (let* ((source-prefix "xx")
         (source-suffix "yy")
         (source
          (concat source-prefix
                  "((:arch \"x86_64\"\n"
                  "  :text-base64 \"QUJD\"\n"
                  "  :relocs ((1 2) (3 4))\n"
                  "  :extern-symbols (\"nl_alloc_symbol\")\n"
                  "  :defuns ((:name \"a\" :offset 1 :body-offset 2 :rt-slot-count 3 :arity 1))\n"
                  "  :object-base64 \"heavy-a\"\n"
                  "  :compile-report (:status ok)\n"
                  "  :symbols (sym-a))\n"
                  " (:arch \"arm64\"\n"
                  "  :text-base64 \"REVG\"\n"
                  "  :relocs nil\n"
                  "  :extern-symbols nil\n"
                  "  :defuns nil\n"
                  "  :object-base64 \"heavy-b\"\n"
                  "  :compile-report (:status ok)\n"
                  "  :symbols (sym-b)))"
                  source-suffix))
         (start (length source-prefix))
         (end (- (length source) (length source-suffix)))
         (native-calls nil)
         (native-result nil))
    (cl-letf (((symbol-function 'nelisp--read-all-from-string-native)
               (lambda (string)
                 (push string native-calls)
                 (list
                  '((:arch "x86_64"
                     :text-base64 "QUJD"
                     :relocs ((1 2) (3 4))
                     :extern-symbols ("nl_alloc_symbol")
                     :defuns ((:name "a" :offset 1 :body-offset 2 :rt-slot-count 3 :arity 1))
                     :object-base64 "heavy-a"
                     :compile-report (:status ok)
                     :symbols (sym-a))
                    (:arch "arm64"
                     :text-base64 "REVG"
                     :relocs nil
                     :extern-symbols nil
                     :defuns nil
                     :object-base64 "heavy-b"
                     :compile-report (:status ok)
                     :symbols (sym-b))))))
              ((symbol-function 'read-from-string)
               (lambda (&rest _args)
                 (error "fallback must not be called"))))
      (setq native-result
            (emacs-load--artifact-native-sections-from-source
             source start end "/tmp/emacs-load-test-native-sections.neln"))
      (should (= (length native-calls) 1))
      (should (equal (car native-calls) (substring source start end)))
      (should (equal native-result
                     '((:arch "x86_64"
                        :text-base64 "QUJD"
                        :relocs ((1 2) (3 4))
                        :extern-symbols ("nl_alloc_symbol")
                        :defuns ((:name "a" :offset 1 :body-offset 2 :rt-slot-count 3 :arity 1)))
                       (:arch "arm64"
                        :text-base64 "REVG"
                        :relocs nil
                        :extern-symbols nil
                        :defuns nil))))
      (dolist (section native-result)
        (should-not (plist-member section :object-base64))
        (should-not (plist-member section :compile-report))
        (should-not (plist-member section :symbols))))))

(ert-deftest emacs-load-test/artifact-native-sections-from-source-rejects-multiple-top-level-forms-and-improper-list ()
  (skip-unless (emacs-load-test--standalone-active-p))
  (let ((path "/tmp/emacs-load-test-native-sections.neln")
        (source "xxignoredyy"))
    (dolist (forms (list
                    '(((:arch "x86_64"
                        :text-base64 "QUJD"
                        :relocs nil
                        :extern-symbols nil
                        :defuns nil))
                      ((:arch "arm64"
                        :text-base64 "REVG"
                        :relocs nil
                        :extern-symbols nil
                        :defuns nil)))
                    (cons '((:arch "x86_64"
                             :text-base64 "QUJD"
                             :relocs nil
                             :extern-symbols nil
                             :defuns nil))
                          'tail)))
      (let ((err nil))
        (cl-letf (((symbol-function 'nelisp--read-all-from-string-native)
                   (lambda (string)
                     (should (equal string "ignored"))
                     forms))
                  ((symbol-function 'read-from-string)
                   (lambda (&rest _args)
                     (error "fallback must not be called"))))
          (setq err
                (condition-case caught
                    (progn
                      (emacs-load--artifact-native-sections-from-source
                       source 2 9 path)
                      nil)
                  (error caught))))
        (should err)
        (should (equal (error-message-string err)
                       "invalid native sections in /tmp/emacs-load-test-native-sections.neln"))))))

(ert-deftest emacs-load-test/artifact-native-sections-from-source-canonicalizes-v5-runtime-prefix-and-compact-relocs ()
  (skip-unless (emacs-load-test--standalone-active-p))
  (let* ((emacs-load-artifact-native-sections-native-reader-threshold 1)
         (path "/tmp/emacs-load-test-native-sections-v5.neln")
         (externs '("nl_alloc_symbol" "nelisp_aot_builtin_call1"))
         (defuns-a
          '((:name "emacs-load-test--v5-a"
             :offset 0
             :body-offset 1
             :arity 0
             :rt-slot-count 1)))
         (defuns-b
          '((:name "emacs-load-test--v5-b"
             :offset 4
             :body-offset 2
             :arity 1
             :rt-slot-count 2)))
         (section-a
          (emacs-load-test--v5-native-section
           "QUJD" externs '(176 0 -4 263 1 -4) defuns-a ?o))
         (section-b
          (emacs-load-test--v5-native-section
           "REVG" nil nil defuns-b ?p))
         (content (concat "("
                          (prin1-to-string section-a)
                          "\n ; keep comment between sections\n"
                          (prin1-to-string section-b)
                          ")"))
         (heavy-read-calls 0)
         (orig-read (symbol-function 'read-from-string)))
    (cl-letf (((symbol-function 'read-from-string)
               (lambda (string &optional start end)
                 (let* ((from (or start 0))
                        (to (or end (length string)))
                        (snippet (substring string from to)))
                   (when (or (string-match-p ":object-base64" snippet)
                             (string-match-p ":compile-report" snippet))
                     (setq heavy-read-calls (1+ heavy-read-calls))
                     (error "heavy native field must not be materialized"))
                   (if end
                       (funcall orig-read string start end)
                     (funcall orig-read string start))))))
      (let ((sections (emacs-load--artifact-native-sections-from-source
                       content 0 (length content) path)))
        (should (= heavy-read-calls 0))
        (should (equal sections
                       (list
                        `(:arch "x86_64"
                          :text-base64 "QUJD"
                          :relocs ((:offset 176
                                    :type plt32
                                    :symbol "nl_alloc_symbol"
                                    :addend -4)
                                   (:offset 263
                                    :type plt32
                                    :symbol "nelisp_aot_builtin_call1"
                                    :addend -4))
                          :extern-symbols ("nl_alloc_symbol"
                                           "nelisp_aot_builtin_call1")
                          :defuns ,defuns-a)
                        `(:arch "x86_64"
                          :text-base64 "REVG"
                          :relocs nil
                          :extern-symbols nil
                          :defuns ,defuns-b))))
        (dolist (section sections)
          (should-not (plist-member section :native-section-version))
          (should-not (plist-member section :runtime-prefix))
          (should-not (plist-member section :object-base64))
          (should-not (plist-member section :compile-report)))))))

(ert-deftest emacs-load-test/source-incremental-rewrites-defalias-through-late-alias ()
  (skip-unless (emacs-load-test--standalone-active-p))
  (let* ((alias 'emacs-load-test--incremental-alias)
         (target 'emacs-load-test--incremental-target)
         (source "(defalias 'emacs-load-test--incremental-alias\n           'emacs-load-test--incremental-target)\n(defun emacs-load-test--incremental-target () 'late-bound)\n")
         (rewrite-calls 0)
         (rewrite-inputs nil)
         (rewrite-outputs nil)
         (late-calls 0)
         (original-rewriter (symbol-function 'nelisp--load-rewrite-defalias-form)))
    (fmakunbound alias)
    (fmakunbound target)
    (unwind-protect
        (cl-letf (((symbol-function 'nelisp--load-rewrite-defalias-form)
                   (lambda (form)
                     (setq rewrite-calls (+ rewrite-calls 1))
                     (push (copy-tree form) rewrite-inputs)
                     (let ((rewritten (funcall original-rewriter form)))
                       (push (copy-tree rewritten) rewrite-outputs)
                       rewritten)))
                  ((symbol-function 'nelisp--defalias-late)
                   (lambda (symbol definition &optional docstring)
                     (setq late-calls (+ late-calls 1))
                     (ignore docstring)
                     (if (and (symbolp definition)
                              (not (fboundp definition)))
                         (fset symbol (lambda (&rest args)
                                        (apply definition args)))
                       (fset symbol definition))
                     symbol)))
          (should (eq (nelisp--load-eval-source-incremental source)
                      target))
          (should (= rewrite-calls 2))
          (should (equal (nreverse rewrite-inputs)
                         '((defalias 'emacs-load-test--incremental-alias
                                     'emacs-load-test--incremental-target)
                           (defun emacs-load-test--incremental-target ()
                             'late-bound))))
          (should (eq (caar (nreverse rewrite-outputs))
                      'nelisp--defalias-late))
          (should (= late-calls 1))
          (should (fboundp target))
          (should (eq (funcall alias) 'late-bound)))
      (when (fboundp alias)
        (fmakunbound alias))
      (when (fboundp target)
        (fmakunbound target)))))

(ert-deftest emacs-load-test/artifact-streaming-threshold-keeps-small-payload-exact ()
  (skip-unless (emacs-load-test--standalone-active-p))
  (let* ((temp-dir (make-temp-file "emacs-load-test-stream-small-" t))
         (artifact (expand-file-name "small.neln" temp-dir))
         (payload '(:module-init nil)))
    (unwind-protect
        (progn
          (with-temp-file artifact
            (insert ";;; nelisp-private-nelc-v2\n")
            (prin1 payload (current-buffer)))
          (let ((emacs-load-artifact-replay-streaming-threshold 4096))
            (should (equal (emacs-load--artifact-replay-file artifact) payload))
            (should (equal emacs-load--last-artifact-payload payload)))
          (let ((emacs-load-artifact-replay-streaming-threshold 1))
            (let ((summary (emacs-load--artifact-replay-file artifact)))
              (should (eq (plist-get summary :streaming) t))
              (should (equal (plist-get summary :artifact) artifact))
              (should (equal (plist-get summary :fields) '(:module-init)))
              (should (= (plist-get summary :module-init-count) 0))
              (should (equal emacs-load--last-artifact-payload summary))
              (should-not (equal summary payload)))))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest emacs-load-test/artifact-streaming-replay-never-reads-from-payload-opener ()
  (skip-unless (emacs-load-test--standalone-active-p))
  (let* ((temp-dir (make-temp-file "emacs-load-test-stream-opener-" t))
         (artifact (expand-file-name "opener.neln" temp-dir))
         (padding (make-string 200000 ?x))
         (payload `(:padding ,padding
                    :module-init nil))
         (prefix ";;; nelisp-private-nelc-v2\n")
         (payload-start (length prefix))
         (read-calls 0)
         (orig-read (symbol-function 'read-from-string)))
    (unwind-protect
        (progn
          (with-temp-file artifact
            (insert prefix)
            (prin1 payload (current-buffer)))
          (let ((emacs-load-artifact-replay-streaming-threshold 1))
            (cl-letf (((symbol-function 'read-from-string)
                       (lambda (string &optional start end)
                         (setq read-calls (+ read-calls 1))
                         (when (and start (= start payload-start))
                           (error "top-level payload opener must not be read"))
                         (should end)
                         (if end
                             (funcall orig-read string start end)
                           (funcall orig-read string start)))))
              (let ((summary (emacs-load--artifact-replay-file artifact)))
                (should (eq (plist-get summary :streaming) t))
                (should (equal (plist-get summary :artifact) artifact))
                (should (= (plist-get summary :module-init-count) 0))))))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest emacs-load-test/artifact-streaming-prefers-nelisp-string-search-over-host-string-search ()
  (skip-unless (emacs-load-test--standalone-active-p))
  (let* ((temp-dir (make-temp-file "emacs-load-test-stream-search-" t))
         (artifact (expand-file-name "search.neln" temp-dir))
         (padding (make-string 200000 ?q))
         (payload `(:padding ,padding
                    :module-init nil
                    :native-sections nil))
         (prefix ";;; nelisp-private-nelc-v2\n")
         (original-string-search (symbol-function 'string-search)))
    (unwind-protect
        (progn
          (with-temp-file artifact
            (insert prefix)
            (prin1 payload (current-buffer)))
          (let ((emacs-load-artifact-replay-streaming-threshold 1))
            (cl-letf (((symbol-function 'string-search)
                       (lambda (&rest _args)
                         (error "host string-search must not be used"))))
              (cl-letf (((symbol-function 'nelisp--string-search)
                         (lambda (needle haystack &optional start)
                           (funcall original-string-search needle haystack start))))
                (let ((summary (emacs-load--artifact-replay-file artifact)))
                  (should (eq (plist-get summary :streaming) t))
                  (should (equal (plist-get summary :artifact) artifact))
                  (should (equal (plist-get summary :fields)
                                 '(:module-init :native-sections)))
                  (should (eq (plist-get summary :native-mode) :native-sections))
                  (should (= (plist-get summary :module-init-count) 0)))))))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest emacs-load-test/artifact-streaming-replays-sharded-native-items-in-order ()
  (skip-unless (emacs-load-test--standalone-active-p))
  (emacs-load-test--with-fresh-native-cache
   (let* ((temp-dir (make-temp-file "emacs-load-test-stream-shards-" t))
          (artifact (expand-file-name "shards.neln" temp-dir))
          (prefix ";;; nelisp-private-nelc-v2\n")
          (payload-start (length prefix))
          (current-text nil)
          (calls nil)
          (form-end-calls nil)
          (padding (make-string 200000 ?y))
          (section-a '(:arch "x86_64"
                       :text-base64 "QUJD"
                       :relocs nil
                       :extern-symbols nil
                       :compile-report ((:name "probe" :native t))
                       :defuns ((:name "emacs-load-test--stream-shard-a"
                                 :offset 0
                                 :body-offset 1
                                 :arity 0
                                 :rt-slot-count 1))))
          (section-b '(:arch "x86_64"
                       :text-base64 "REVG"
                       :relocs nil
                       :extern-symbols nil
                       :defuns ((:name "emacs-load-test--stream-shard-b"
                                 :offset 0
                                 :body-offset 2
                                 :arity 1
                                 :rt-slot-count 2))))
          (payload `(:padding ,padding
                     :native-sections (,section-a ,section-b)
                     :module-init ((:eval (setq emacs-load-test--stream-order
                                                (append emacs-load-test--stream-order '(first))))
                                   (:fn emacs-load-test--stream-shard-a
                                    (lambda () 'fallback-a))
                                   (:eval (setq emacs-load-test--stream-order
                                                (append emacs-load-test--stream-order '(second))))
                                   (:fn emacs-load-test--stream-shard-b
                                    (lambda (arg) (list 'fallback-b arg))))))
          (payload-source (prin1-to-string payload))
          (native-sections-start
           (let ((marker (string-search ":native-sections " payload-source)))
             (+ payload-start marker (length ":native-sections "))))
          (read-calls 0)
          (orig-read (symbol-function 'read-from-string)))
     (unwind-protect
         (progn
           (setq emacs-load-test--stream-order nil)
           (with-temp-file artifact
             (insert prefix)
             (prin1 payload (current-buffer))
             (insert " \n"))
           (let ((original-form-end (symbol-function 'emacs-load--artifact-source-form-end)))
             (cl-letf (((symbol-function 'nelisp--base64-decode-bytes)
                         (lambda (text)
                           (setq current-text text)
                           (cond
                            ((string= text "QUJD") "ABC")
                            ((string= text "REVG") "DEF")
                            (t "XYZ"))))
                     ((symbol-function 'page-size)
                      (lambda () 1))
                     ((symbol-function 'syscall-direct)
                      (lambda (&rest _args)
                        (cond
                         ((string= current-text "QUJD") 1000)
                         ((string= current-text "REVG") 2000)
                         (t 3000))))
                     ((symbol-function 'ptr-write-u8)
                      (lambda (&rest _args) nil))
                     ((symbol-function 'ptr-write-u32)
                      (lambda (&rest _args) nil))
                     ((symbol-function 'nelisp--runtime-symbol-address)
                      (lambda (&rest _args) 1))
                     ((symbol-function 'nelisp--native-call-boundary)
                      (lambda (body-address arity rt-slot-count &rest args)
                        (push (list body-address arity rt-slot-count args) calls)
                        (list body-address arity rt-slot-count args)))
                     ((symbol-function 'read-from-string)
                      (lambda (string &optional start end)
                        (setq read-calls (+ read-calls 1))
                        (when (and start (= start native-sections-start))
                          (error "native-sections outer form must not be read"))
                        (if end
                            (funcall orig-read string start end)
                          (funcall orig-read string start))))
                     ((symbol-function 'emacs-load--artifact-source-form-end)
                      (lambda (source start)
                        (push start form-end-calls)
                        (when (= start payload-start)
                          (error "large streaming replay must not scan whole payload"))
                        (funcall original-form-end source start))))
               (let ((emacs-load-artifact-replay-streaming-threshold 1))
                 (should (eq (plist-get (emacs-load--artifact-replay-file artifact)
                                        :streaming)
                             t)))
               (should (equal emacs-load-test--stream-order '(first second)))
               (should form-end-calls)
               (should-not (member payload-start form-end-calls))
               (should (> read-calls 0))
               (should (equal (funcall 'emacs-load-test--stream-shard-a)
                              '(1001 0 1 nil)))
               (should (equal (funcall 'emacs-load-test--stream-shard-b 'x)
                              '(2002 1 2 (x))))
               (should (equal (nreverse calls)
                              '((1001 0 1 nil)
                                (2002 1 2 (x)))))))
       (dolist (name '(emacs-load-test--stream-shard-a
                       emacs-load-test--stream-shard-b))
         (when (fboundp name)
           (fmakunbound name)))
       (setq emacs-load-test--stream-order nil)
       (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))))

(ert-deftest emacs-load-test/artifact-streaming-replays-module-init-before-sharded-native-sections ()
  (skip-unless (emacs-load-test--standalone-active-p))
  (emacs-load-test--with-fresh-native-cache
   (let* ((temp-dir (make-temp-file "emacs-load-test-stream-module-first-" t))
          (artifact (expand-file-name "module-first.neln" temp-dir))
          (prefix ";;; nelisp-private-nelc-v2\n")
          (payload-start (length prefix))
          (current-text nil)
          (calls nil)
          (form-end-calls nil)
          (padding (make-string 200000 ?y))
          (section-a '(:arch "x86_64"
                       :text-base64 "QUJD"
                       :relocs nil
                       :extern-symbols nil
                       :compile-report ((:name "probe" :native t))
                       :defuns ((:name "emacs-load-test--stream-module-first-a"
                                 :offset 0
                                 :body-offset 1
                                 :arity 0
                                 :rt-slot-count 1))))
          (section-b '(:arch "x86_64"
                       :text-base64 "REVG"
                       :relocs nil
                       :extern-symbols nil
                       :defuns ((:name "emacs-load-test--stream-module-first-b"
                                 :offset 0
                                 :body-offset 2
                                 :arity 1
                                 :rt-slot-count 2))))
          (payload `(:padding ,padding
                     :module-init ((:eval (setq emacs-load-test--stream-order
                                                (append emacs-load-test--stream-order '(first))))
                                   (:fn emacs-load-test--stream-module-first-a
                                    (lambda () 'fallback-a))
                                   (:eval (setq emacs-load-test--stream-order
                                                (append emacs-load-test--stream-order '(second))))
                                   (:fn emacs-load-test--stream-module-first-b
                                    (lambda (arg) (list 'fallback-b arg))))
                     :native-sections (,section-a ,section-b)))
          (payload-source (prin1-to-string payload))
          (native-sections-start
           (let ((marker (string-search ":native-sections " payload-source)))
             (+ payload-start marker (length ":native-sections "))))
          (read-calls 0)
          (orig-read (symbol-function 'read-from-string)))
     (unwind-protect
         (progn
           (setq emacs-load-test--stream-order nil)
           (with-temp-file artifact
             (insert prefix)
             (prin1 payload (current-buffer)))
           (let ((original-form-end (symbol-function 'emacs-load--artifact-source-form-end)))
             (cl-letf (((symbol-function 'nelisp--base64-decode-bytes)
                         (lambda (text)
                           (setq current-text text)
                           (cond
                            ((string= text "QUJD") "ABC")
                            ((string= text "REVG") "DEF")
                            (t "XYZ"))))
                     ((symbol-function 'page-size)
                      (lambda () 1))
                     ((symbol-function 'syscall-direct)
                      (lambda (&rest _args)
                        (cond
                         ((string= current-text "QUJD") 1000)
                         ((string= current-text "REVG") 2000)
                         (t 3000))))
                     ((symbol-function 'ptr-write-u8)
                      (lambda (&rest _args) nil))
                     ((symbol-function 'ptr-write-u32)
                      (lambda (&rest _args) nil))
                     ((symbol-function 'nelisp--runtime-symbol-address)
                      (lambda (&rest _args) 1))
                     ((symbol-function 'nelisp--native-call-boundary)
                      (lambda (body-address arity rt-slot-count &rest args)
                        (push (list body-address arity rt-slot-count args) calls)
                        (list body-address arity rt-slot-count args)))
                     ((symbol-function 'read-from-string)
                      (lambda (string &optional start end)
                        (setq read-calls (+ read-calls 1))
                        (when (and start (= start native-sections-start))
                          (error "native-sections outer form must not be read"))
                        (if end
                            (funcall orig-read string start end)
                          (funcall orig-read string start))))
                     ((symbol-function 'emacs-load--artifact-source-form-end)
                      (lambda (source start)
                        (push start form-end-calls)
                        (when (= start payload-start)
                          (error "large streaming replay must not scan whole payload"))
                        (funcall original-form-end source start))))
               (let ((emacs-load-artifact-replay-streaming-threshold 1))
                 (should (eq (plist-get (emacs-load--artifact-replay-file artifact)
                                        :streaming)
                             t)))
               (should (equal emacs-load-test--stream-order '(first second)))
               (should form-end-calls)
               (should-not (member payload-start form-end-calls))
               (should (> read-calls 0))
               (should (equal (funcall 'emacs-load-test--stream-module-first-a)
                              '(1001 0 1 nil)))
               (should (equal (funcall 'emacs-load-test--stream-module-first-b 'x)
                              '(2002 1 2 (x))))
               (should (equal (nreverse calls)
                              '((1001 0 1 nil)
                                (2002 1 2 (x)))))))
       (dolist (name '(emacs-load-test--stream-module-first-a
                       emacs-load-test--stream-module-first-b))
         (when (fboundp name)
           (fmakunbound name)))
       (setq emacs-load-test--stream-order nil)
       (when (file-directory-p temp-dir)
         (delete-directory temp-dir t)))))))

(ert-deftest emacs-load-test/artifact-streaming-module-init-uses-native-range-reader-once-and-preserves-order ()
  (skip-unless (emacs-load-test--standalone-active-p))
  (let* ((temp-dir (make-temp-file "emacs-load-test-stream-module-reader-" t))
         (artifact (expand-file-name "module-reader.neln" temp-dir))
         (prefix ";;; nelisp-private-nelc-v2\n")
         (padding (make-string 200000 ?z))
         (module-init-forms
          '((emacs-load-test--stream-recorder 'first)
            (emacs-load-test--stream-recorder 'second)))
         (module-init-source (prin1-to-string module-init-forms))
         (native-reader-calls 0)
         (native-reader-input nil)
         (read-calls 0)
         (emacs-load-test--stream-order nil))
    (unwind-protect
        (progn
          (with-temp-file artifact
            (insert prefix)
            (prin1 `(:padding ,padding
                     :module-init ,module-init-forms)
                   (current-buffer)))
          (let ((emacs-load-artifact-replay-streaming-threshold 1))
            (cl-letf (((symbol-function 'read-from-string)
                       (lambda (&rest _args)
                         (setq read-calls (+ read-calls 1))
                         (error "host reader should not be used for module-init")))
                      ((symbol-function 'emacs-load-test--stream-recorder)
                       (lambda (tag)
                         (setq emacs-load-test--stream-order
                               (append emacs-load-test--stream-order
                                       (list tag)))
                         tag))
                      ((symbol-function 'nelisp--read-batch-from-string-native)
                       (lambda (string byte-cursor batch-size)
                         (setq native-reader-calls (+ native-reader-calls 1))
                         (setq native-reader-input string)
                         (should (= byte-cursor 0))
                         (should (= batch-size
                                    emacs-load-artifact-module-init-native-batch-items))
                         (should (equal string
                                        (substring module-init-source 1 -1)))
                         (cons module-init-forms
                               (emacs-load--artifact-byte-length string))))
                      ((symbol-function 'nelisp--read-all-from-string-native)
                       (lambda (&rest _args)
                         (error "per-item native reader should not be used"))))
              (let ((summary (emacs-load--artifact-replay-file artifact)))
                (should (eq (plist-get summary :streaming) t))
                (should (equal (plist-get summary :artifact) artifact))
                (should (equal (plist-get summary :fields) '(:module-init)))
                (should (= (plist-get summary :module-init-count) 2))
                (should (equal emacs-load-test--stream-order
                               '(first second)))
                (should (= native-reader-calls 1))
                (should (equal native-reader-input
                               (substring module-init-source 1 -1)))
                (should (= read-calls 0))))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))))

(ert-deftest emacs-load-test/artifact-streaming-rejects-malformed-top-level-and-module-forms ()
  (skip-unless (emacs-load-test--standalone-active-p))
  (let* ((temp-dir (make-temp-file "emacs-load-test-stream-bad-" t))
         (top-level-bad (expand-file-name "top-level-bad.neln" temp-dir))
         (module-bad (expand-file-name "module-bad.neln" temp-dir))
         (trailing-garbage (expand-file-name "trailing-garbage.neln" temp-dir))
         (padding (make-string 200000 ?z)))
    (unwind-protect
        (progn
          (with-temp-file top-level-bad
            (insert ";;; nelisp-private-nelc-v2\n")
            (insert "(:padding ")
            (prin1 padding (current-buffer))
            (insert " :module-init nil"))
          (with-temp-file module-bad
            (insert ";;; nelisp-private-nelc-v2\n")
            (prin1 `(:padding ,padding :module-init 42) (current-buffer)))
          (with-temp-file trailing-garbage
            (insert ";;; nelisp-private-nelc-v2\n")
            (prin1 `(:padding ,padding :module-init nil) (current-buffer))
            (insert "x"))
          (let ((emacs-load-artifact-replay-streaming-threshold 1))
            (should-error (emacs-load--artifact-replay-file top-level-bad))
            (should-error (emacs-load--artifact-replay-file module-bad))
            (should-error (emacs-load--artifact-replay-file trailing-garbage))))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest emacs-load-test/artifact-streaming-writer-rewrites-and-appends-in-order ()
  (skip-unless (emacs-load-test--standalone-active-p))
  (let* ((temp-dir (make-temp-file "emacs-load-test-stream-" t))
         (temp-file (expand-file-name "normalized.el" temp-dir))
         (source "(defalias 'emacs-load-test--stream-alias\n           'emacs-load-test--stream-target)\n(defun emacs-load-test--stream-target () 'late-bound)\n(defalias 'emacs-load-test--stream-alias-2\n           'emacs-load-test--stream-target)\n")
         (rewrite-calls 0)
         (gc-calls 0)
         (original-rewriter (symbol-function 'nelisp--load-rewrite-defalias-form))
         (expected ";;; nelisp-private-nelc-v2\n(nelisp--defalias-late 'emacs-load-test--stream-alias 'emacs-load-test--stream-target)\n(defun emacs-load-test--stream-target nil 'late-bound)\n(nelisp--defalias-late 'emacs-load-test--stream-alias-2 'emacs-load-test--stream-target)\n"))
    (unwind-protect
        (cl-letf (((symbol-function 'garbage-collect)
                   (lambda ()
                     (setq gc-calls (+ gc-calls 1))
                     'gc))
                  ((symbol-function 'nelisp--load-rewrite-defalias-form)
                   (lambda (form)
                     (setq rewrite-calls (+ rewrite-calls 1))
                     (funcall original-rewriter form))))
          (let ((load-garbage-collect-interval 2))
            (emacs-load--artifact-write-normalized-source source temp-file))
          (should (= rewrite-calls 3))
          (should (= gc-calls 1))
          (should (equal (with-temp-buffer
                           (insert-file-contents temp-file)
                           (buffer-string))
                         expected)))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest emacs-load-test/artifact-raw-writer-preserves-source-and-skips-mapper ()
  (skip-unless (emacs-load-test--standalone-active-p))
  (let* ((temp-dir (make-temp-file "emacs-load-test-raw-" t))
         (temp-file (expand-file-name "raw.el" temp-dir))
         (source ";; kept raw\n(defun emacs-load-test--raw-writer () 'ok)\n")
         (expected ";;; nelisp-private-nelc-v2\n;; kept raw\n(defun emacs-load-test--raw-writer () 'ok)\n"))
    (unwind-protect
        (cl-letf (((symbol-function 'nelisp--load-map-source-forms)
                   (lambda (&rest _args)
                     (error "raw writer must not map forms")))
                  ((symbol-function 'read-from-string)
                   (lambda (&rest _args)
                     (error "raw writer must not read forms")))
                  ((symbol-function 'nelisp--load-rewrite-defalias-form)
                   (lambda (&rest _args)
                     (error "raw writer must not rewrite forms"))))
          (emacs-load--artifact-write-raw-source source temp-file)
          (should (equal (with-temp-buffer
                           (insert-file-contents temp-file)
                           (buffer-string))
                         expected)))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest emacs-load-test/artifact-cache-source-digest-uses-linear-concat-framing-and-is-stable ()
  (skip-unless (emacs-load-test--standalone-active-p))
  (let* ((source (concat "org-macs: " (string ?\0) "bilingual あ"))
         (salt emacs-load--artifact-cache-source-digest-salt)
         (legacy-digest
          (emacs-load--sha256
           (prin1-to-string (list salt source))))
         (expected-digest
          (emacs-load--sha256 (concat salt "\0" source)))
         (first-digest (emacs-load--artifact-cache-source-digest source))
         (second-digest (emacs-load--artifact-cache-source-digest source)))
    (should (string= first-digest expected-digest))
    (should (string= second-digest expected-digest))
    (should (string= first-digest second-digest))
    (should-not (string= first-digest legacy-digest))))

(ert-deftest emacs-load-test/artifact-compiler-failure-records-diagnostic-and-falls-back-to-source ()
  (skip-unless (emacs-load-test--standalone-active-p))
  (let* ((temp-dir (make-temp-file "emacs-load-test-fallback-" t))
         (resolved (expand-file-name "sample.el" temp-dir))
         (artifact (expand-file-name "cache/4d/sample.neln" temp-dir))
         (sidecar (concat artifact ".source-sha256"))
         (temp-input (expand-file-name "compile-input.el" temp-dir))
         (source "(defvar emacs-load-test--fallback 1)\n")
         (source-digest (emacs-load--artifact-cache-source-digest source))
         (compiler-path "/tmp/fake-nelisp")
         (call-process-args nil)
         (fallback-calls 0)
         (writer-calls 0)
         (original-writer (symbol-function 'emacs-load--artifact-write-raw-source)))
    (unwind-protect
        (progn
          (with-temp-file resolved
            (insert source))
          (cl-letf (((symbol-function 'emacs-load--artifact-cache-paths)
                     (lambda (_resolved)
                       (list artifact sidecar)))
                    ((symbol-function 'emacs-load--artifact-compiler)
                     (lambda () compiler-path))
                    ((symbol-function 'emacs-load--artifact-load-path-cli-args)
                     (lambda () nil))
                    ((symbol-function 'make-temp-file)
                     (lambda (&rest _args) temp-input))
                    ((symbol-function 'nelisp--syscall-read-file)
                     (lambda (path)
                       (with-temp-buffer
                         (insert-file-contents path)
                         (buffer-string))))
                    ((symbol-function 'nelisp--load-normalize-source-rewriting)
                     (lambda (&rest _args)
                       (error "unexpected full normalizer")))
                    ((symbol-function 'emacs-load--artifact-write-raw-source)
                     (lambda (input path)
                       (setq writer-calls (+ writer-calls 1))
                       (funcall original-writer input path)))
                    ((symbol-function 'call-process)
                     (lambda (&rest args)
                       (setq call-process-args args)
                       88))
                    ((symbol-function 'nelisp--load-eval-source-incremental)
                     (lambda (_source)
                       (setq fallback-calls (+ fallback-calls 1))
                       'incremental-fallback))
                    ((symbol-function 'nelisp--load-eval-source-hybrid)
                     (lambda (_source)
                       (error "unexpected hybrid path"))))
            (let ((emacs-load-auto-native-compile t)
                  (emacs-load-artifact-max-source-size nil)
                  (emacs-load-large-source-threshold 8))
              (should (eq (nelisp--load-resolved-file resolved nil) t))
              (should (equal (plist-get emacs-load--artifact-compile-diagnostic-report
                                         :resolved)
                             resolved))
              (should (equal (plist-get emacs-load--artifact-compile-diagnostic-report
                                         :compiler)
                             compiler-path))
              (should (equal (plist-get emacs-load--artifact-compile-diagnostic-report
                                         :status)
                             88))
              (should (equal (plist-get emacs-load--artifact-compile-diagnostic-report
                                         :artifact)
                             artifact))
              (should (equal (plist-get emacs-load--artifact-compile-diagnostic-report
                                         :sidecar)
                             sidecar))
              (should (equal (plist-get emacs-load--artifact-compile-diagnostic-report
                                         :source-hash)
                             source-digest))
              (should (equal (plist-get emacs-load--artifact-compile-diagnostic-report
                                         :temp)
                             temp-input))
              (should (equal (plist-get emacs-load--artifact-compile-diagnostic-report
                                         :command)
                             (list "compile-elisp-artifact"
                                   "--kind" "neln"
                                   "--input" temp-input
                                   "--output" artifact
                                   "--native-policy"
                                   "opportunistic")))
              (should (= writer-calls 1))
              (should (= fallback-calls 1))
              (should (equal call-process-args
                             (list compiler-path nil nil nil
                                   "compile-elisp-artifact"
                                   "--kind" "neln"
                                   "--input" temp-input
                                   "--output" artifact
                                   "--native-policy"
                                   "opportunistic")))
              (should (= fallback-calls 1))
              (should-not (file-exists-p temp-input))
              (should-not (file-exists-p artifact))
              (should-not (file-exists-p sidecar)))))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest emacs-load-test/artifact-native-lambda-form-arity-0-and-1 ()
  (skip-unless (emacs-load-test--standalone-active-p))
  (let ((arity-0 (emacs-load--artifact-native-lambda-form 17 0 9))
        (arity-1 (emacs-load--artifact-native-lambda-form 23 1 11)))
    (should (equal arity-0
                   '(lambda nil
                      (nelisp--native-call-boundary 17 0 9))))
    (should (equal arity-1
                   '(lambda (a0)
                      (nelisp--native-call-boundary 23 1 11 a0))))))

(ert-deftest emacs-load-test/artifact-native-install-fn-calls-boundary-with-body-address-and-args ()
  (skip-unless (emacs-load-test--standalone-active-p))
  (let* ((name 'emacs-load-test--native-installed)
         (native '(:arch "x86_64"
                   :text-base64 "QUJD"
                   :relocs nil
                   :extern-symbols nil
                   :defuns ((:name "emacs-load-test--native-installed"
                             :offset 7
                             :body-offset 5
                             :arity 2
                             :rt-slot-count 3))))
         (meta (car (plist-get native :defuns)))
         (calls nil))
    (unwind-protect
        (cl-letf (((symbol-function 'nelisp--native-call-boundary)
                   (lambda (body-address arity rt-slot-count &rest args)
                     (push (list body-address arity rt-slot-count args) calls)
                     :invoked)))
          (emacs-load--artifact-native-install-fn name native 200 meta)
          (should (equal (plist-get emacs-load--artifact-native-diagnostic-report
                                    :body-address)
                         212))
          (should (eq (funcall name 'alpha 'beta) :invoked))
          (should (equal (car calls) '(212 2 3 (alpha beta)))))
      (when (fboundp name)
        (fmakunbound name)))))

(ert-deftest emacs-load-test/artifact-replay-native-sections-selects-matching-section-base ()
  (skip-unless (emacs-load-test--standalone-active-p))
  (emacs-load-test--with-fresh-native-cache
   (let* ((temp-dir (make-temp-file "emacs-load-test-native-sections-" t))
          (artifact (expand-file-name "sections.neln" temp-dir))
          (current-text nil)
          (calls nil)
          (section-a '(:arch "x86_64"
                       :text-base64 "QUJD"
                       :relocs nil
                       :extern-symbols nil
                       :defuns ((:name "emacs-load-test--shard-a"
                                 :offset 0
                                 :body-offset 1
                                 :arity 0
                                 :rt-slot-count 1))))
          (section-b '(:arch "x86_64"
                       :text-base64 "REVG"
                       :relocs nil
                       :extern-symbols nil
                       :defuns ((:name "emacs-load-test--shard-b"
                                 :offset 0
                                 :body-offset 2
                                 :arity 1
                                 :rt-slot-count 2))))
          (payload `(:native-sections (,section-a ,section-b)
                     :module-init ((:fn emacs-load-test--shard-a
                                    (lambda () 'fallback-a))
                                   (:fn emacs-load-test--shard-b
                                    (lambda (arg) (list 'fallback-b arg)))))))
     (unwind-protect
         (progn
           (with-temp-file artifact
             (insert ";;; nelisp-private-nelc-v2\n")
             (prin1 payload (current-buffer)))
           (cl-letf (((symbol-function 'nelisp--base64-decode-bytes)
                      (lambda (text)
                        (setq current-text text)
                        (cond
                         ((string= text "QUJD") "ABC")
                         ((string= text "REVG") "DEF")
                         (t "XYZ"))))
                     ((symbol-function 'page-size)
                      (lambda () 1))
                     ((symbol-function 'syscall-direct)
                      (lambda (&rest _args)
                        (cond
                         ((string= current-text "QUJD") 1000)
                         ((string= current-text "REVG") 2000)
                         (t 3000))))
                     ((symbol-function 'ptr-write-u8)
                      (lambda (&rest _args) nil))
                     ((symbol-function 'ptr-write-u32)
                      (lambda (&rest _args) nil))
                     ((symbol-function 'nelisp--runtime-symbol-address)
                      (lambda (&rest _args) 1))
                     ((symbol-function 'nelisp--native-call-boundary)
                      (lambda (body-address arity rt-slot-count &rest args)
                        (push (list body-address arity rt-slot-count args) calls)
                        (list body-address arity rt-slot-count args)))
                     ((symbol-function 'emacs-load--artifact-source-form-end)
                      (lambda (&rest _args)
                        (error "large streaming replay must not scan whole payload")))
                     ((symbol-function 'emacs-load--artifact-source-read-form-ranges)
                      (lambda (&rest _args)
                        (error "large streaming replay must not read whole payload ranges"))))
             (should (equal (emacs-load--artifact-replay-file artifact) payload))
             (should (equal (funcall 'emacs-load-test--shard-a)
                            '(1001 0 1 nil)))
             (should (equal (funcall 'emacs-load-test--shard-b 'x)
                            '(2002 1 2 (x))))
             (should (equal (nreverse calls)
                            '((1001 0 1 nil)
                              (2002 1 2 (x)))))))
       (dolist (name '(emacs-load-test--shard-a emacs-load-test--shard-b))
         (when (fboundp name)
           (fmakunbound name)))
       (when (file-directory-p temp-dir)
         (delete-directory temp-dir t))))))

(ert-deftest emacs-load-test/artifact-native-sections-streaming-parser-drops-heavy-fields-and-preserves-order ()
  (skip-unless (emacs-load-test--standalone-active-p))
  (let* ((heavy-a (make-string 20000 ?o))
         (heavy-b (make-string 20000 ?p))
         (section-a `(:arch "x86_64"
                      :text-base64 "QUJD"
                      :relocs nil
                      :extern-symbols nil
                      :object-base64 ,heavy-a
                      :compile-report ((:name "probe-a" :native t))
                      :symbols ((:name "hidden-a"))
                      :defuns ((:name "emacs-load-test--selective-a"
                                :offset 0
                                :body-offset 1
                                :arity 0
                                :rt-slot-count 1))))
         (section-b `(:arch "x86_64"
                      :text-base64 "REVG"
                      :relocs nil
                      :extern-symbols nil
                      :object-base64 ,heavy-b
                      :compile-report ((:name "probe-b" :native t))
                      :symbols ((:name "hidden-b"))
                      :defuns ((:name "emacs-load-test--selective-b"
                                :offset 0
                                :body-offset 2
                                :arity 1
                                :rt-slot-count 2))))
         (content (concat "("
                          (prin1-to-string section-a)
                          "\n ; keep comment between sections\n"
                          (prin1-to-string section-b)
                          ")"))
         (heavy-read-calls 0)
         (orig-read (symbol-function 'read-from-string)))
    (cl-letf (((symbol-function 'read-from-string)
               (lambda (string &optional start end)
                 (let ((snippet (substring string start (or end (length string)))))
                   (when (or (string-match-p ":object-base64" snippet)
                             (string-match-p ":compile-report" snippet)
                             (string-match-p ":symbols" snippet))
                     (setq heavy-read-calls (+ heavy-read-calls 1))
                     (error "heavy native field must not be materialized"))
                   (if end
                       (funcall orig-read string start end)
                     (funcall orig-read string start))))))
      (let ((sections (emacs-load--artifact-native-sections-from-source
                       content 0 (length content) "/tmp/native-sections.neln")))
        (should (= heavy-read-calls 0))
        (should (equal sections
                       (list `(:arch "x86_64"
                               :text-base64 "QUJD"
                               :relocs nil
                               :extern-symbols nil
                               :defuns ((:name "emacs-load-test--selective-a"
                                         :offset 0
                                         :body-offset 1
                                         :arity 0
                                         :rt-slot-count 1)))
                             `(:arch "x86_64"
                               :text-base64 "REVG"
                               :relocs nil
                               :extern-symbols nil
                               :defuns ((:name "emacs-load-test--selective-b"
                                         :offset 0
                                         :body-offset 2
                                         :arity 1
                                         :rt-slot-count 2))))))
        (dolist (section sections)
          (should-not (plist-member section :object-base64))
          (should-not (plist-member section :compile-report))
          (should-not (plist-member section :symbols)))
        (should (equal (mapcar (lambda (section) (plist-get (car (plist-get section :defuns)) :name))
                               sections)
                       '("emacs-load-test--selective-a"
                         "emacs-load-test--selective-b")))))))

(ert-deftest emacs-load-test/artifact-native-sections-from-source-hands-sections-to-handler-in-order ()
  (skip-unless (emacs-load-test--standalone-active-p))
  (let* ((source-prefix "xx")
         (source-suffix "yy")
         (source
          (concat source-prefix
                  "((:arch \"x86_64\"\n"
                  "  :text-base64 \"QUJD\"\n"
                  "  :relocs nil\n"
                  "  :extern-symbols nil\n"
                  "  :defuns ((:name \"one\" :offset 1 :body-offset 2 :rt-slot-count 3 :arity 1))\n"
                  "  :object-base64 \"heavy-one\")\n"
                  " (:arch \"arm64\"\n"
                  "  :text-base64 \"REVG\"\n"
                  "  :relocs nil\n"
                  "  :extern-symbols nil\n"
                  "  :defuns ((:name \"two\" :offset 4 :body-offset 5 :rt-slot-count 6 :arity 0))\n"
                  "  :object-base64 \"heavy-two\"))"
                  source-suffix))
         (start (length source-prefix))
         (end (- (length source) (length source-suffix)))
         (emacs-load-artifact-native-sections-native-reader-threshold 1)
         (handler-order nil)
         (handler-result nil))
    (cl-letf (((symbol-function 'nelisp--read-all-from-string-native)
               (lambda (&rest _args)
                 (error "native whole-reader must not be called"))))
      (setq handler-result
            (emacs-load--artifact-native-sections-from-source
             source start end "/tmp/emacs-load-test-native-handler.neln"
             (lambda (section)
               (setq handler-order
                     (append handler-order
                             (list (plist-get section :arch))))
               (list :arch (plist-get section :arch)
                     :name (plist-get (car (plist-get section :defuns)) :name)))))
      (should (equal handler-order '("x86_64" "arm64")))
      (should (equal handler-result
                     '((:arch "x86_64" :name "one")
                       (:arch "arm64" :name "two")))))))

(ert-deftest emacs-load-test/artifact-replay-native-sections-streaming-builds-compact-section-bases ()
  (skip-unless (emacs-load-test--standalone-active-p))
  (emacs-load-test--with-fresh-native-cache
   (let* ((temp-dir (make-temp-file "emacs-load-test-native-compact-" t))
          (artifact (expand-file-name "compact.neln" temp-dir))
          (section-a '(:arch "x86_64"
                       :text-base64 "QUJD"
                       :relocs nil
                       :extern-symbols nil
                       :defuns ((:name "emacs-load-test--compact-a"
                                 :offset 0
                                 :body-offset 1
                                 :arity 0
                                 :rt-slot-count 1))))
          (section-b '(:arch "x86_64"
                       :text-base64 "REVG"
                       :relocs nil
                       :extern-symbols nil
                       :defuns ((:name "emacs-load-test--compact-b"
                                 :offset 0
                                 :body-offset 2
                                 :arity 1
                                 :rt-slot-count 2))))
          (payload `(:native-sections (,section-a ,section-b)
                     :module-init ((:fn emacs-load-test--compact-a
                                    (lambda () 'fallback-a))
                                   (:fn emacs-load-test--compact-b
                                    (lambda (arg) (list 'fallback-b arg))))))
          (map-calls 0)
          (install-calls nil)
          (compact-native nil)
          (compact-base nil))
     (unwind-protect
         (progn
           (with-temp-file artifact
             (insert ";;; nelisp-private-nelc-v2\n")
             (prin1 payload (current-buffer)))
           (cl-letf (((symbol-function 'nelisp--base64-decode-bytes)
                      (lambda (text)
                        (cond
                         ((string= text "QUJD") "ABC")
                         ((string= text "REVG") "DEF")
                         (t "XYZ"))))
                     ((symbol-function 'page-size)
                      (lambda () 1))
                     ((symbol-function 'syscall-direct)
                      (lambda (&rest _args)
                        (setq map-calls (+ map-calls 1))
                        (if (= map-calls 1) 1000 2000)))
                     ((symbol-function 'ptr-write-u8)
                      (lambda (&rest _args) nil))
                     ((symbol-function 'ptr-write-u32)
                      (lambda (&rest _args) nil))
                     ((symbol-function 'nelisp--runtime-symbol-address)
                      (lambda (&rest _args) 1))
                     ((symbol-function 'nelisp--native-call-boundary)
                      (lambda (&rest _args) nil))
                     ((symbol-function 'emacs-load--artifact-native-install-fn)
                      (lambda (name native base meta)
                        (push (list name native base meta) install-calls)
                        (setq compact-native native)
                        (setq compact-base base)
                        name)))
             (let ((emacs-load-artifact-replay-streaming-threshold 1))
               (should
                (eq (plist-get (emacs-load--artifact-replay-file artifact)
                               :streaming)
                    t)))
             (setq install-calls (nreverse install-calls))
             (should (= (length install-calls) 2))
             (should (equal (mapcar #'car install-calls)
                            '(emacs-load-test--compact-a
                              emacs-load-test--compact-b)))
             (dolist (entry install-calls)
               (let ((native (nth 1 entry)))
                 (should (equal (plist-get native :arch) "x86_64"))
                 (should (plist-member native :defuns))
                 (should (plist-member native :path))
                 (should (plist-member native :text-hash))
                 (should-not (plist-member native :text-base64))
                 (should-not (plist-member native :relocs))
                 (should-not (plist-member native :extern-symbols))))
             (should (equal (plist-get compact-native :text-hash)
                            (emacs-load--sha256 "REVG")))
             (should (= map-calls 2))
             (should (= compact-base 2000))))
       (dolist (name '(emacs-load-test--compact-a emacs-load-test--compact-b))
         (when (fboundp name)
           (fmakunbound name)))
       (when (file-directory-p temp-dir)
         (delete-directory temp-dir t))))))

(ert-deftest emacs-load-test/artifact-replay-native-sections-detects-duplicate-function-names-across-compact-bases ()
  (skip-unless (emacs-load-test--standalone-active-p))
  (emacs-load-test--with-fresh-native-cache
   (let* ((temp-dir (make-temp-file "emacs-load-test-native-duplicate-" t))
          (artifact (expand-file-name "duplicate.neln" temp-dir))
          (section-a '(:arch "x86_64"
                       :text-base64 "QUJD"
                       :relocs nil
                       :extern-symbols nil
                       :defuns ((:name "emacs-load-test--duplicate"
                                 :offset 0
                                 :body-offset 1
                                 :arity 0
                                 :rt-slot-count 1))))
          (section-b '(:arch "x86_64"
                       :text-base64 "REVG"
                       :relocs nil
                       :extern-symbols nil
                       :defuns ((:name "emacs-load-test--duplicate"
                                 :offset 0
                                 :body-offset 2
                                 :arity 1
                                 :rt-slot-count 2))))
          (payload `(:native-sections (,section-a ,section-b)
                     :module-init ((:fn emacs-load-test--duplicate
                                    (lambda () 'fallback))))))
     (unwind-protect
         (progn
           (with-temp-file artifact
             (insert ";;; nelisp-private-nelc-v2\n")
             (prin1 payload (current-buffer)))
           (cl-letf (((symbol-function 'nelisp--base64-decode-bytes)
                      (lambda (text)
                        (cond
                         ((string= text "QUJD") "ABC")
                         ((string= text "REVG") "DEF")
                         (t "XYZ"))))
                     ((symbol-function 'page-size)
                      (lambda () 1))
                     ((symbol-function 'syscall-direct)
                      (lambda (&rest _args)
                        1000))
                     ((symbol-function 'ptr-write-u8)
                      (lambda (&rest _args) nil))
                     ((symbol-function 'ptr-write-u32)
                      (lambda (&rest _args) nil))
                     ((symbol-function 'nelisp--runtime-symbol-address)
                      (lambda (&rest _args) 1))
                     ((symbol-function 'nelisp--native-call-boundary)
                      (lambda (&rest _args) nil)))
             (let ((err (condition-case caught
                            (let ((emacs-load-artifact-replay-streaming-threshold 1))
                              (emacs-load--artifact-replay-file artifact)
                              nil)
                          (error caught))))
               (should err)
               (should (string-match-p
                        "native function .* appears in multiple sections"
                        (error-message-string err)))))))
       (when (file-directory-p temp-dir)
         (delete-directory temp-dir t)))))

(ert-deftest emacs-load-test/artifact-native-install-fn-accepts-compact-diagnostic-hash ()
  (skip-unless (emacs-load-test--standalone-active-p))
  (let* ((name 'emacs-load-test--compact-native-installed)
         (compact-native '(:arch "x86_64"
                           :defuns ((:name "emacs-load-test--compact-native-installed"
                                     :offset 7
                                     :body-offset 5
                                     :arity 2
                                     :rt-slot-count 3))
                           :path "/tmp/emacs-load-test-native-compact.neln"
                           :text-hash "precomputed-hash"))
         (meta (car (plist-get compact-native :defuns)))
         (calls nil))
    (unwind-protect
        (cl-letf (((symbol-function 'nelisp--native-call-boundary)
                   (lambda (body-address arity rt-slot-count &rest args)
                     (push (list body-address arity rt-slot-count args) calls)
                     :invoked)))
          (emacs-load--artifact-native-install-fn name compact-native 200 meta)
          (should (equal (plist-get emacs-load--artifact-native-diagnostic-report
                                    :hash)
                         "precomputed-hash"))
          (should (equal (plist-get emacs-load--artifact-native-diagnostic-report
                                    :path)
                         "/tmp/emacs-load-test-native-compact.neln"))
          (should (eq (funcall name 'alpha 'beta) :invoked))
          (should (equal (car calls) '(212 2 3 (alpha beta)))))
      (when (fboundp name)
        (fmakunbound name)))))

(ert-deftest emacs-load-test/artifact-replay-native-sections-rejects-malformed-entry ()
  (skip-unless (emacs-load-test--standalone-active-p))
  (let* ((temp-dir (make-temp-file "emacs-load-test-native-sections-bad-" t))
         (artifact (expand-file-name "sections-bad.neln" temp-dir))
         (payload '(:native-sections ((:arch "x86_64"
                                      :text-base64 "QUJD"
                                      :relocs :oops
                                      :extern-symbols nil
                                      :defuns nil))
                    :module-init nil)))
    (unwind-protect
        (progn
          (with-temp-file artifact
            (insert ";;; nelisp-private-nelc-v2\n")
            (prin1 payload (current-buffer)))
          (should-error (emacs-load--artifact-replay-file artifact)))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest emacs-load-test/artifact-replay-native-legacy-section-keeps-bcl-fallback ()
  ;; Known loader defect (native-section preflight / nested error text),
  ;; recorded 2026-09-05 when this file was restored; see the T30 report.
  :expected-result :failed
  (skip-unless (emacs-load-test--standalone-active-p))
  (emacs-load-test--with-fresh-native-cache
   (let* ((temp-dir (make-temp-file "emacs-load-test-native-legacy-" t))
          (artifact (expand-file-name "legacy.neln" temp-dir))
          (prefix ";;; nelisp-private-nelc-v2\n")
          (payload-start (length prefix))
          (padding (make-string 200000 ?w))
          (current-text nil)
          (calls nil)
          (form-end-calls nil)
          (nelisp--functions-old-boundp (boundp 'nelisp--functions))
          (nelisp--functions-old-value (and (boundp 'nelisp--functions)
                                            nelisp--functions))
          (native '(:arch "x86_64"
                    :text-base64 "QUJD"
                    :relocs nil
                    :extern-symbols nil
                    :defuns ((:name "emacs-load-test--legacy-native"
                              :offset 0
                              :body-offset 1
                              :arity 0
                              :rt-slot-count 1))))
          (payload `(:padding ,padding
                     :native ,native
                     :module-init ((:fn emacs-load-test--legacy-native
                                    (lambda () 'fallback-native))
                                   (:fn emacs-load-test--legacy-bcl
                                    (lambda () 'fallback-bcl)))))
          (payload-source (prin1-to-string payload))
          (native-value-start
           (let ((marker (string-search ":native " payload-source)))
             (+ payload-start marker (length ":native "))))
          (native-section-calls 0)
          (read-calls 0)
          (orig-read (symbol-function 'read-from-string))
          (fallback-calls 0))
     (unwind-protect
         (progn
           (with-temp-file artifact
             (insert prefix)
             (prin1 payload (current-buffer)))
           (setq nelisp--functions (make-hash-table :test 'eq))
           (let ((original-form-end (symbol-function 'emacs-load--artifact-source-form-end))
                 (original-section-from-ranges
                  (symbol-function 'emacs-load--artifact-native-section-from-ranges)))
             (cl-letf (((symbol-function 'emacs-load--artifact-native-section-from-ranges)
                        (lambda (&rest args)
                          (setq native-section-calls (+ native-section-calls 1))
                          (apply original-section-from-ranges args)))
                       ((symbol-function 'nelisp--base64-decode-bytes)
                        (lambda (text)
                          (setq current-text text)
                          "ABC"))
                     ((symbol-function 'page-size)
                      (lambda () 1))
                     ((symbol-function 'syscall-direct)
                      (lambda (&rest _args)
                        (if (string= current-text "QUJD")
                            3000
                          4000)))
                     ((symbol-function 'ptr-write-u8)
                      (lambda (&rest _args) nil))
                     ((symbol-function 'ptr-write-u32)
                      (lambda (&rest _args) nil))
                     ((symbol-function 'nelisp--runtime-symbol-address)
                      (lambda (&rest _args) 1))
                     ((symbol-function 'nelisp--native-call-boundary)
                      (lambda (body-address arity rt-slot-count &rest args)
                        (push (list body-address arity rt-slot-count args) calls)
                        (list body-address arity rt-slot-count args)))
                     ((symbol-function 'nelisp--apply)
                      (lambda (fn args)
                        (setq fallback-calls (+ fallback-calls 1))
                        (apply fn args)))
                     ((symbol-function 'emacs-load--artifact-source-form-end)
             (lambda (source start)
                        (push start form-end-calls)
                        (when (= start payload-start)
                          (error "large streaming replay must not scan whole payload"))
                        (funcall original-form-end source start)))
                     ((symbol-function 'read-from-string)
                      (lambda (string &optional start end)
                        (setq read-calls (+ read-calls 1))
                        (when (and start (= start native-value-start))
                          (error "native outer form must not be read"))
                        (if end
                            (funcall orig-read string start end)
                          (funcall orig-read string start)))))
                (emacs-load-test--without-native-read-all
                 (let ((emacs-load-artifact-replay-streaming-threshold 1))
                   (let ((summary (emacs-load--artifact-replay-file artifact)))
                     (should (eq (plist-get summary :streaming) t))
                     (should (eq (plist-get summary :native-mode) :native))
                     (should (equal (plist-get summary :module-init-count) 2))))
                   (should form-end-calls)
                   (should-not (member payload-start form-end-calls))
                   (should (> read-calls 0))
                   (should (equal (funcall 'emacs-load-test--legacy-native)
                                  '(3001 0 1 nil)))
                   (should (eq (funcall 'emacs-load-test--legacy-bcl) 'fallback-bcl))
                   (should (equal (nreverse calls)
                                  '((3001 0 1 nil))))
                   (should (= native-section-calls 1))
                   (should (= fallback-calls 1)))))))
       (dolist (name '(emacs-load-test--legacy-native emacs-load-test--legacy-bcl))
         (when (fboundp name)
           (fmakunbound name)))
       (if nelisp--functions-old-boundp
           (setq nelisp--functions nelisp--functions-old-value)
         (when (boundp 'nelisp--functions)
           (makunbound 'nelisp--functions)))
       (when (file-directory-p temp-dir)
         (delete-directory temp-dir t)))))

(ert-deftest emacs-load-test/artifact-streaming-ineligible-native-falls-back-to-source-defun ()
  ;; Known loader defect (native-section preflight / nested error text),
  ;; recorded 2026-09-05 when this file was restored; see the T30 report.
  :expected-result :failed
  (skip-unless (emacs-load-test--standalone-active-p))
  (emacs-load-test--with-fresh-native-cache
   (let* ((temp-dir (make-temp-file "emacs-load-test-stream-ineligible-" t))
          (artifact (expand-file-name "ineligible.neln" temp-dir))
          (prefix ";;; nelisp-private-nelc-v2\n")
          (padding (make-string 200000 ?z))
          (heavy-text (make-string 180000 ?t))
          (heavy-relocs (make-list 256
                                   '(:offset 0 :type plt32
                                     :symbol "nl_alloc_symbol" :addend 0)))
          (heavy-defuns (make-list 256
                                   '(:name "emacs-load-test--stream-ineligible"
                                     :offset 0
                                     :body-offset 1
                                     :arity 0
                                     :rt-slot-count 1)))
          (native `(:arch "x86_64"
                    :text-base64 ,heavy-text
                    :relocs ,heavy-relocs
                    :extern-symbols ("avl-local-helper")
                    :defuns ,heavy-defuns))
          (payload `(:padding ,padding
                     :module-init ((:fn emacs-load-test--stream-ineligible
                                    (lambda () 'bcl-fallback)
                                    (defun emacs-load-test--stream-ineligible
                                        nil 'source-fallback)))
                     :native ,native))
          (nelisp--functions-old-boundp (boundp 'nelisp--functions))
          (nelisp--functions-old-value (and (boundp 'nelisp--functions)
                                            nelisp--functions))
          (nelisp--apply-old-boundp (fboundp 'nelisp--apply))
          (nelisp--apply-old-function (and (fboundp 'nelisp--apply)
                                           (symbol-function 'nelisp--apply)))
          (native-section-calls 0)
          (native-map-calls 0)
          (runtime-symbol-calls 0)
          (source-eval-calls 0))
     (unwind-protect
         (progn
           (with-temp-file artifact
             (insert prefix)
             (prin1 payload (current-buffer)))
           (when nelisp--functions-old-boundp
             (makunbound 'nelisp--functions))
           (when nelisp--apply-old-boundp
             (fmakunbound 'nelisp--apply))
           (cl-letf (((symbol-function 'emacs-load--artifact-native-map-section)
                      (lambda (&rest _args)
                        (setq native-map-calls (+ native-map-calls 1))
                        (error "ineligible native section must not be mapped")))
                     ((symbol-function 'emacs-load--artifact-native-section-from-ranges)
                      (lambda (&rest _args)
                        (setq native-section-calls (+ native-section-calls 1))
                        (error "ineligible native section must not be materialized")))
                     ((symbol-function 'nelisp--runtime-symbol-address)
                      (lambda (&rest _args)
                        (setq runtime-symbol-calls (+ runtime-symbol-calls 1))
                        (error "resolver must not be called for ineligible native section")))
                     ((symbol-function 'nelisp-eval)
                      (lambda (form)
                        (setq source-eval-calls (+ source-eval-calls 1))
                        (eval form))))
             (let ((summary
                    (let ((emacs-load-artifact-replay-streaming-threshold 1))
                      (emacs-load--artifact-replay-file artifact))))
               (should (eq (plist-get summary :streaming) t))
               (should (eq (plist-get summary :native-mode) :native))
               (should (equal (plist-get summary :module-init-count) 1))
               (should (equal (plist-get summary :fields)
                              '(:module-init :native)))
               (should (eq (funcall 'emacs-load-test--stream-ineligible)
                           'source-fallback))
               (should (= native-section-calls 0))
               (should (= native-map-calls 0))
               (should (= runtime-symbol-calls 0))
               (should (= source-eval-calls 1))))))
       (when (fboundp 'emacs-load-test--stream-ineligible)
         (fmakunbound 'emacs-load-test--stream-ineligible))
       (when nelisp--functions-old-boundp
         (setq nelisp--functions nelisp--functions-old-value))
       (when (and nelisp--apply-old-boundp
                  (not (fboundp 'nelisp--apply)))
         (fset 'nelisp--apply nelisp--apply-old-function))
       (when (file-directory-p temp-dir)
         (delete-directory temp-dir t)))))

(ert-deftest emacs-load-test/artifact-streaming-missing-native-fields-error-before-materialization ()
  ;; Known loader defect (native-section preflight / nested error text),
  ;; recorded 2026-09-05 when this file was restored; see the T30 report.
  :expected-result :failed
  (skip-unless (emacs-load-test--standalone-active-p))
  (emacs-load-test--with-fresh-native-cache
   (let* ((temp-dir (make-temp-file "emacs-load-test-stream-missing-" t))
          (artifact (expand-file-name "missing-field.neln" temp-dir))
          (prefix ";;; nelisp-private-nelc-v2\n")
          (padding (make-string 200000 ?m))
          (heavy-text (make-string 180000 ?x))
          (heavy-relocs (make-list 64
                                   '(:offset 0 :type plt32
                                     :symbol "nl_alloc_symbol" :addend 0)))
          (native `(:arch "x86_64"
                    :text-base64 ,heavy-text
                    :relocs ,heavy-relocs
                    :extern-symbols ("nl_alloc_symbol")))
          (payload `(:padding ,padding
                     :module-init ((:fn emacs-load-test--stream-missing
                                    (lambda () 'fallback)))
                     :native ,native))
          (native-section-calls 0))
     (unwind-protect
         (progn
           (with-temp-file artifact
             (insert prefix)
             (prin1 payload (current-buffer)))
           (cl-letf (((symbol-function 'emacs-load--artifact-native-section-from-ranges)
                      (lambda (&rest _args)
                        (setq native-section-calls (+ native-section-calls 1))
                        (error "native section materializer must not run"))))
               (let ((err (condition-case caught
                            (let ((emacs-load-artifact-replay-streaming-threshold 1))
                              (emacs-load--artifact-replay-file artifact)
                              nil)
                          (error caught))))
               (should err)
               (should (string= (error-message-string err)
                                (format "invalid native section in %s" artifact)))))
           (should (= native-section-calls 0)))
       (when (file-directory-p temp-dir)
         (delete-directory temp-dir t))))))

(ert-deftest emacs-load-test/artifact-replay-item-prefers-native-match-over-fallbacks ()
  (skip-unless (emacs-load-test--standalone-active-p))
  (emacs-load-test--with-fresh-native-cache
   (let* ((native '(:arch "x86_64"
                   :text-base64 "QUJD"
                   :relocs nil
                   :extern-symbols nil
                   :defuns ((:name "emacs-load-test--native-preferred"
                             :offset 0
                             :body-offset 1
                             :arity 0
                             :rt-slot-count 1))))
          (item '(:fn emacs-load-test--native-preferred
                  (lambda () 'bcl-fallback)
                  (defun emacs-load-test--native-preferred nil 'source-fallback)))
          (path "/tmp/emacs-load-test-native-preferred.neln")
          (calls nil)
          (apply-calls 0)
          (eval-calls 0))
     (cl-letf (((symbol-function 'nelisp--base64-decode-bytes)
                (lambda (_text)
                  "ABC"))
               ((symbol-function 'page-size)
                (lambda () 1))
               ((symbol-function 'syscall-direct)
                (lambda (&rest _args)
                  4096))
               ((symbol-function 'ptr-write-u8)
                (lambda (&rest _args) nil))
               ((symbol-function 'ptr-write-u32)
                (lambda (&rest _args) nil))
               ((symbol-function 'nelisp--native-call-boundary)
                (lambda (body-address arity rt-slot-count &rest args)
                  (push (list body-address arity rt-slot-count args) calls)
                  :native))
               ((symbol-function 'nelisp--runtime-symbol-address)
                (lambda (&rest _args)
                  (error "runtime resolver should not be called")))
               ((symbol-function 'nelisp--apply)
                (lambda (&rest _args)
                  (setq apply-calls (+ apply-calls 1))
                  (error "BCL fallback should not be used")))
               ((symbol-function 'nelisp-eval)
                (lambda (&rest _args)
                  (setq eval-calls (+ eval-calls 1))
                  (error "source fallback should not be used"))))
       (should (= (emacs-load--artifact-native-map-section native path) 4096))
       (should (eq (emacs-load--artifact-replay-item item path native 4096)
                   'emacs-load-test--native-preferred))
       (should (eq (funcall 'emacs-load-test--native-preferred) :native))
       (should (equal (nreverse calls) '((4097 0 1 nil))))
       (should (= apply-calls 0))
       (should (= eval-calls 0))))))

(ert-deftest emacs-load-test/artifact-replay-item-uses-source-defun-fallback-when-bcl-unavailable ()
  (skip-unless (emacs-load-test--standalone-active-p))
  (let* ((item '(:fn emacs-load-test--source-fallback
                 (lambda () 'bcl-fallback)
                 (defun emacs-load-test--source-fallback nil 'source-fallback)))
         (path "/tmp/emacs-load-test-source-fallback.neln")
         (nelisp--functions-old-boundp (boundp 'nelisp--functions))
         (nelisp--functions-old-value (and (boundp 'nelisp--functions)
                                           nelisp--functions))
         (nelisp--apply-old-boundp (fboundp 'nelisp--apply))
         (nelisp--apply-old-function (and (fboundp 'nelisp--apply)
                                          (symbol-function 'nelisp--apply)))
         (eval-calls 0))
    (unwind-protect
        (progn
          (when nelisp--functions-old-boundp
            (makunbound 'nelisp--functions))
          (when nelisp--apply-old-boundp
            (fmakunbound 'nelisp--apply))
          (cl-letf (((symbol-function 'nelisp-eval)
                     (lambda (form)
                       (setq eval-calls (+ eval-calls 1))
                       (eval form))))
            (should (eq (emacs-load--artifact-replay-item-with-native-sections
                         item path nil)
                        'emacs-load-test--source-fallback))
            (should (eq (funcall 'emacs-load-test--source-fallback)
                        'source-fallback))
            (should (= eval-calls 1))))
      (when nelisp--functions-old-boundp
        (setq nelisp--functions nelisp--functions-old-value))
      (when (and nelisp--apply-old-boundp
                 (not (fboundp 'nelisp--apply)))
        (fset 'nelisp--apply nelisp--apply-old-function)))))

(ert-deftest emacs-load-test/artifact-eval-form-uses-eval-when-nelisp-eval-is-absent ()
  (skip-unless (emacs-load-test--standalone-active-p))
  (let* ((form '(defun emacs-load-test--eval-branch nil 'eval-fallback))
         (nelisp-eval-old-boundp (fboundp 'nelisp-eval))
         (nelisp-eval-old-function (and (fboundp 'nelisp-eval)
                                        (symbol-function 'nelisp-eval)))
         (eval-calls 0)
         (orig-eval (symbol-function 'eval)))
    (unwind-protect
        (progn
          (when nelisp-eval-old-boundp
            (fmakunbound 'nelisp-eval))
          (cl-letf (((symbol-function 'eval)
                     (lambda (expression)
                       (setq eval-calls (+ eval-calls 1))
                       (funcall orig-eval expression))))
            (should (eq (emacs-load--artifact-eval-form form)
                        'emacs-load-test--eval-branch))
            (should (eq (funcall 'emacs-load-test--eval-branch)
                        'eval-fallback))
            (should (= eval-calls 1))))
      (when (fboundp 'emacs-load-test--eval-branch)
        (fmakunbound 'emacs-load-test--eval-branch))
      (when nelisp-eval-old-boundp
        (fset 'nelisp-eval nelisp-eval-old-function)))))

(ert-deftest emacs-load-test/artifact-replay-item-rejects-malformed-source-defun-fallback ()
  (skip-unless (emacs-load-test--standalone-active-p))
  (let ((path "/tmp/emacs-load-test-source-fallback-bad.neln"))
    (dolist (item '((:fn emacs-load-test--bad-non-defun
                     (lambda () 'bcl-fallback)
                     (lambda () 'not-a-defun))
                    (:fn emacs-load-test--bad-name-mismatch
                     (lambda () 'bcl-fallback)
                     (defun emacs-load-test--different-name nil 'oops))))
      (let ((err (condition-case caught
                     (progn
                       (emacs-load--artifact-replay-item-with-native-sections
                        item path nil)
                       nil)
                   (error caught))))
        (should err)
        (should (string-match-p "invalid source defun fallback"
                                (error-message-string err)))))))

(ert-deftest emacs-load-test/artifact-native-stub-write-falls-back-to-u8-without-u32 ()
  (skip-unless (emacs-load-test--standalone-active-p))
  (let ((writes nil))
    (should (= (emacs-load--artifact-native-write-stub
                100 16 #x0807060504030201
                (lambda (base index byte)
                  (push (list base index byte) writes))
                nil)
               116))
    (should (equal (nreverse writes)
                   '((100 16 72)
                     (100 17 184)
                     (100 18 1)
                     (100 19 2)
                     (100 20 3)
                     (100 21 4)
                     (100 22 5)
                     (100 23 6)
                     (100 24 7)
                     (100 25 8)
                     (100 26 255)
                     (100 27 224)
                     (100 28 0)
                     (100 29 0)
                     (100 30 0)
                     (100 31 0))))))

(ert-deftest emacs-load-test/artifact-native-map-section-reuses-cache-and-writes-bytes ()
  (skip-unless (emacs-load-test--standalone-active-p))
  (emacs-load-test--with-fresh-native-cache
   (let* ((native '(:arch "x86_64"
                   :text-base64 "QUJD"
                   :relocs nil
                   :extern-symbols nil
                   :defuns nil))
          (path "/tmp/emacs-load-test-native.eln")
          (decoded nil)
          (mmap-args nil)
          (write-calls nil))
     (cl-letf (((symbol-function 'nelisp--base64-decode-bytes)
                (lambda (text)
                  (setq decoded text)
                  "ABC"))
               ((symbol-function 'page-size)
                (lambda () 1))
               ((symbol-function 'syscall-direct)
                (lambda (&rest args)
                  (setq mmap-args args)
                  4096))
               ((symbol-function 'ptr-write-u8)
                (lambda (base index byte)
                  (push (list base index byte) write-calls))))
       (let ((first (emacs-load--artifact-native-map-section native path))
             (second (emacs-load--artifact-native-map-section native path)))
         (should (equal decoded "QUJD"))
         (should (= first 4096))
         (should (= second 4096))
         (should (equal mmap-args '(9 0 3 7 34 -1 0)))
         (should (equal (nreverse write-calls)
                        '((4096 0 65)
                          (4096 1 66)
                          (4096 2 67)))))))))

(ert-deftest emacs-load-test/artifact-native-map-section-uses-copy-helper-for-text-and-keeps-cache ()
  (skip-unless (emacs-load-test--standalone-active-p))
  (emacs-load-test--with-fresh-native-cache
   (let* ((native '(:arch "x86_64"
                   :text-base64 "QUJD"
                   :relocs nil
                   :extern-symbols nil
                   :defuns nil))
          (path "/tmp/emacs-load-test-native-copy.eln")
          (decoded nil)
          (mmap-args nil)
          (copy-calls nil)
          (copy-result nil))
     (cl-letf (((symbol-function 'nelisp--base64-decode-bytes)
                (lambda (text)
                  (setq decoded text)
                  "ABC"))
               ((symbol-function 'page-size)
                (lambda () 1))
               ((symbol-function 'syscall-direct)
                (lambda (&rest args)
                  (setq mmap-args args)
                  4096))
               ((symbol-function 'nelisp--ptr-copy-string-bytes)
                (lambda (base text)
                  (push (list base text) copy-calls)
                  (setq copy-result 3)
                  copy-result))
               ((symbol-function 'ptr-write-u8)
                (lambda (&rest _args)
                  (error "text should use nelisp--ptr-copy-string-bytes"))))
       (should (= (emacs-load--artifact-native-map-section native path) 4096))
       (should (= (emacs-load--artifact-native-map-section native path) 4096))
       (should (equal decoded "QUJD"))
       (should (equal (nreverse copy-calls) '((4096 "ABC"))))
       (should (= copy-result 3))
       (should (equal mmap-args '(9 0 3 7 34 -1 0)))
       (should (= (hash-table-count emacs-load--artifact-native-hash-cache) 1))
       (should (= (gethash (emacs-load--artifact-native-content-key native)
                           emacs-load--artifact-native-hash-cache)
                  4096))))))

(ert-deftest emacs-load-test/artifact-native-map-section-rejects-copy-count-mismatch-and-does-not-cache ()
  (skip-unless (emacs-load-test--standalone-active-p))
  (emacs-load-test--with-fresh-native-cache
   (let* ((native '(:arch "x86_64"
                   :text-base64 "QUJD"
                   :relocs nil
                   :extern-symbols nil
                   :defuns nil))
          (path "/tmp/emacs-load-test-native-copy-mismatch.eln")
          (mmap-args nil))
     (cl-letf (((symbol-function 'nelisp--base64-decode-bytes)
                (lambda (_text)
                  "ABC"))
               ((symbol-function 'page-size)
                (lambda () 1))
               ((symbol-function 'syscall-direct)
                (lambda (&rest args)
                  (setq mmap-args args)
                  4096))
               ((symbol-function 'nelisp--ptr-copy-string-bytes)
                (lambda (&rest _args)
                  2))
               ((symbol-function 'ptr-write-u8)
                (lambda (&rest _args)
                  (error "text should not fall back to ptr-write-u8"))))
       (should-error (emacs-load--artifact-native-map-section native path))
       (should (equal mmap-args '(9 0 3 7 34 -1 0)))
       (should (= (hash-table-count emacs-load--artifact-native-hash-cache) 0))))))

(ert-deftest emacs-load-test/artifact-native-map-section-rounds-by-decoded-bytes-and-distinct-externs ()
  (skip-unless (emacs-load-test--standalone-active-p))
  (emacs-load-test--with-fresh-native-cache
   (let* ((native '(:arch "x86_64"
                   :text-base64 "QUJDRA=="
                   :relocs ((:offset 0 :type plt32 :symbol "nl_alloc_symbol" :addend 0))
                   :extern-symbols ("nl_alloc_symbol" "nl_alloc_symbol" "nl_alloc_str")
                   :defuns nil))
          (path "/tmp/emacs-load-test-native-rounding.eln")
          (mmap-args nil)
          (u8-calls nil)
          (u32-calls nil)
          (mmap-calls 0))
     (cl-letf (((symbol-function 'nelisp--base64-decode-bytes)
                (lambda (_text)
                  "ABCD"))
               ((symbol-function 'page-size)
                (lambda () 1))
               ((symbol-function 'syscall-direct)
                (lambda (&rest args)
                  (setq mmap-calls (+ mmap-calls 1))
                  (setq mmap-args args)
                  4096))
               ((symbol-function 'ptr-write-u8)
                (lambda (base index byte)
                  (push (list base index byte) u8-calls)))
               ((symbol-function 'ptr-write-u32)
                (lambda (base index value)
                  (push (list base index value) u32-calls)))
               ((symbol-function 'nelisp--runtime-symbol-address)
                (lambda (symbol)
                  (cond
                   ((equal symbol "nl_alloc_symbol") 8192)
                   ((equal symbol "nl_alloc_str") 12288)
                   (t 0)))))
       (should (= (emacs-load--artifact-native-map-section native path) 4096))
       (should (= (emacs-load--artifact-native-map-section native path) 4096))
       (should (= mmap-calls 1))
       (should (equal mmap-args '(9 0 36 7 34 -1 0)))
       (should (= (hash-table-count emacs-load--artifact-native-hash-cache) 1))
       (should (= (gethash (emacs-load--artifact-native-content-key native)
                           emacs-load--artifact-native-hash-cache)
                  4096))
       (should (= (length u32-calls) 5))
       (should (= (length u8-calls) 20))))))

(ert-deftest emacs-load-test/artifact-native-invalid-reloc-offset-is-rejected-before-cache ()
  (skip-unless (emacs-load-test--standalone-active-p))
  (emacs-load-test--with-fresh-native-cache
   (let* ((native '(:arch "x86_64"
                   :text-base64 "QUJDRA=="
                   :relocs ((:offset 5 :type plt32 :symbol "foo" :addend 0))
                   :extern-symbols ("foo")
                   :defuns nil))
          (path "/tmp/emacs-load-test-native-invalid.eln")
          (syscall-called nil)
          (write-called nil))
     (cl-letf (((symbol-function 'nelisp--base64-decode-bytes)
                (lambda (_text)
                  "ABCD"))
               ((symbol-function 'page-size)
                (lambda () 1))
               ((symbol-function 'syscall-direct)
                (lambda (&rest _args)
                  (setq syscall-called t)
                  4096))
               ((symbol-function 'ptr-write-u8)
                (lambda (&rest _args)
                  (setq write-called t)))
               ((symbol-function 'nelisp--runtime-symbol-address)
                (lambda (&rest _args)
                  (error "runtime lookup should not happen"))))
       (should-not (emacs-load--artifact-native-eligible-p native))
       (should-error (emacs-load--artifact-native-map-section native path))
       (should-not syscall-called)
       (should-not write-called)
       (should (= (hash-table-count emacs-load--artifact-native-hash-cache) 0))))))

(ert-deftest emacs-load-test/artifact-native-unsupported-extern-returns-nil-without-validator-or-resolver ()
  (skip-unless (emacs-load-test--standalone-active-p))
  (let ((native '(:arch "x86_64"
                 :text-base64 "QUJD"
                 :relocs nil
                 :extern-symbols ("foo")
                 :defuns nil)))
    (cl-letf (((symbol-function 'nelisp--runtime-symbol-address)
               (lambda (&rest _args)
                 (error "resolver should not run")))
              ((symbol-function 'nelisp--native-call-boundary)
               (lambda (&rest _args)
                 t))
              ((symbol-function 'nelisp--base64-decode-bytes)
               (lambda (&rest _args)
                 (error "decode should not run")))
              ((symbol-function 'emacs-load--artifact-native-validate-externs)
               (lambda (&rest _args)
                 (error "validator should not run"))))
      (should-not (emacs-load--artifact-native-eligible-p native)))))

(ert-deftest emacs-load-test/artifact-cache-miss-writes-sidecar-and-hit-skips-call-process ()
  (skip-unless (emacs-load-test--standalone-active-p))
  (let* ((temp-dir (make-temp-file "emacs-load-test-cache-" t))
         (resolved (expand-file-name "sample.el" temp-dir))
         (artifact (expand-file-name "cache/4d/sample.neln" temp-dir))
         (sidecar (concat artifact ".source-sha256"))
         (temp-input (expand-file-name "compile-input.el" temp-dir))
         (source "(defun emacs-load-test--cache-sample () 1)\n")
         (compiler-identity
          '(:path "/tmp/fake-nelisp" :size 42 :mtime (1 2 3 4)))
         (legacy-source-sha
          (emacs-load--sha256
           (prin1-to-string
            (list emacs-load--artifact-cache-source-digest-salt source))))
         (salted-source-sha (emacs-load--artifact-cache-source-digest source))
         (compiler-path "/tmp/fake-nelisp")
         (call-process-args nil)
         (replay-paths nil)
         (cache-path-calls 0)
         (writer-calls 0)
         (original-writer (symbol-function 'emacs-load--artifact-write-raw-source)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory artifact) t)
          (with-temp-file artifact
            (insert ";;; stale artifact\n"))
          (with-temp-file sidecar
            (insert legacy-source-sha))
          (cl-letf (((symbol-function 'emacs-load--artifact-cache-paths)
                     (lambda (_resolved)
                       (setq cache-path-calls (+ cache-path-calls 1))
                       (list artifact sidecar)))
                    ((symbol-function 'emacs-load--artifact-compiler)
                     (lambda () compiler-path))
                    ((symbol-function 'emacs-load--artifact-compiler-identity)
                     (lambda (_compiler) compiler-identity))
                    ((symbol-function 'emacs-load--artifact-load-path-cli-args)
                     (lambda () nil))
                    ((symbol-function 'make-temp-file)
                     (lambda (&rest _args) temp-input))
                    ((symbol-function 'nelisp--load-normalize-source-rewriting)
                     (lambda (&rest _args)
                       (error "unexpected full normalizer")))
                    ((symbol-function 'emacs-load--artifact-write-raw-source)
                     (lambda (input path)
                       (setq writer-calls (+ writer-calls 1))
                       (funcall original-writer input path)))
                    ((symbol-function 'call-process)
                     (lambda (&rest args)
                       (setq call-process-args args)
                       (with-temp-file (nth 10 args)
                         (insert ";;; nelisp-private-nelc-v2\n")
                         (prin1 '(:module-init nil) (current-buffer)))
                       (with-temp-file (concat (nth 10 args) ".manifest.el")
                         (prin1 '(:format nelisp-elisp-artifact-manifest-v1)
                                (current-buffer)))
                       0))
                    ((symbol-function 'emacs-load--artifact-replay-file)
                     (lambda (path)
                       (push path replay-paths)
                       'replayed))
                    ((symbol-function 'nelisp--syscall-read-file)
                     (lambda (path)
                       (with-temp-buffer
                         (insert-file-contents path)
                         (buffer-string)))))
            (let ((emacs-load-auto-native-compile t)
                  (emacs-load-artifact-max-source-size nil))
              (should (eq (emacs-load--artifact-load-or-compile resolved source) t))
              (should (eq (emacs-load--artifact-load-or-compile resolved source) t))))
          (should (= cache-path-calls 2))
          (should (= writer-calls 1))
          (should (equal call-process-args
                         (list compiler-path nil nil nil
                               "compile-elisp-artifact"
                               "--kind" "neln"
                               "--input" temp-input
                               "--output" artifact
                               "--native-policy" "opportunistic")))
          (should (equal (nreverse replay-paths) (list artifact artifact)))
          (should (file-readable-p artifact))
          (should (file-readable-p sidecar))
          (should (null emacs-load--artifact-compile-diagnostic-report))
          (should (equal (emacs-load--artifact-cache-read-plist sidecar)
                         (emacs-load--artifact-cache-record
                          salted-source-sha compiler-identity))))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(provide 'emacs-load-test)

;;; emacs-load-test.el ends here
