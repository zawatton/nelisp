;;; emacs-load-artifact-compiler-test.el --- ERT for emacs-load artifact compile args  -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'ert)

(defconst emacs-load-artifact-compiler-test--root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))

(defconst emacs-load-artifact-compiler-test--source-file
  (expand-file-name "src/emacs-load.el" emacs-load-artifact-compiler-test--root))

;; Load the source implementation directly so the tests do not depend on a
;; potentially stale compiled .elc.
(let ((emacs-version 1))
  (load-file emacs-load-artifact-compiler-test--source-file))

(defmacro emacs-load-artifact-compiler-test--without-native-read-all (&rest body)
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

(defmacro emacs-load-artifact-compiler-test--without-native-source-container-end
    (&rest body)
  "Run BODY without `nelisp--source-container-end' if it exists."
  `(let ((original-native (when (fboundp 'nelisp--source-container-end)
                            (symbol-function 'nelisp--source-container-end))))
     (unwind-protect
         (progn
           (when original-native
             (fmakunbound 'nelisp--source-container-end))
           ,@body)
       (when original-native
         (fset 'nelisp--source-container-end original-native)))))

(defmacro emacs-load-artifact-compiler-test--without-native-rd-string-end-native
    (&rest body)
  "Run BODY without `nelisp--rd-string-end-native' if it exists."
  `(let ((original-native (when (fboundp 'nelisp--rd-string-end-native)
                            (symbol-function 'nelisp--rd-string-end-native))))
     (unwind-protect
         (progn
           (when original-native
             (fmakunbound 'nelisp--rd-string-end-native))
           ,@body)
       (when original-native
         (fset 'nelisp--rd-string-end-native original-native)))))

(defun emacs-load-artifact-compiler-test--make-dir (root name)
  "Create and return ROOT/NAME."
  (let ((dir (expand-file-name name root)))
    (make-directory dir t)
    dir))

(defmacro emacs-load-artifact-compiler-test--with-temporary-arena-stats
    (stats-form &rest body)
  "Run BODY with `nelisp--arena-stats' temporarily set to STATS-FORM."
  (declare (indent 1))
  `(let ((original (when (fboundp 'nelisp--arena-stats)
                     (symbol-function 'nelisp--arena-stats))))
     (unwind-protect
         (progn
           (fset 'nelisp--arena-stats ,stats-form)
           ,@body)
       (if original
         (fset 'nelisp--arena-stats original)
        (fmakunbound 'nelisp--arena-stats)))))

(defmacro emacs-load-artifact-compiler-test--with-temporary-allocation-debt
    (debt-form &rest body)
  "Run BODY with `nelisp--allocation-debt' temporarily set to DEBT-FORM."
  (declare (indent 1))
  `(let ((original (when (fboundp 'nelisp--allocation-debt)
                     (symbol-function 'nelisp--allocation-debt))))
     (unwind-protect
         (progn
           (fset 'nelisp--allocation-debt ,debt-form)
           ,@body)
       (if original
           (fset 'nelisp--allocation-debt original)
         (fmakunbound 'nelisp--allocation-debt)))))

(ert-deftest emacs-load-artifact-compiler-test/load-path-cli-args-are-stable-and-deduplicated ()
  (let* ((root (make-temp-file "emacs-load-artifact-compiler-test-" t))
         (dir-a (emacs-load-artifact-compiler-test--make-dir root "a"))
         (dir-b (emacs-load-artifact-compiler-test--make-dir root "b"))
         (rel-dir (emacs-load-artifact-compiler-test--make-dir root "rel"))
         (missing (expand-file-name "missing" root))
         (file-entry (expand-file-name "file.el" root))
         (load-path (list dir-a
                          ""
                          nil
                          "rel"
                          dir-a
                          missing
                          9
                          dir-b
                          rel-dir
                          "rel"
                          (cons 'bad 'entry)
                          file-entry)))
    (unwind-protect
        (progn
          (with-temp-file file-entry
            (insert "x"))
          (let ((default-directory root))
            (should (equal (emacs-load--artifact-load-path-cli-args)
                           (list "--load-path" dir-a
                                 "--load-path" rel-dir
                                 "--load-path" dir-b)))
            (should (equal (emacs-load--artifact-load-path-cli-args)
                           (emacs-load--artifact-load-path-cli-args)))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest emacs-load-artifact-compiler-test/artifact-source-read-form-nil-or-list-value-uses-native-reader-at-threshold ()
  (let ((load-garbage-collect-interval nil)
        (emacs-load-artifact-native-sections-native-reader-threshold 4096)
        (path "/tmp/emacs-load-artifact-compiler-test-read-form-native.neln")
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
                        source range path :defuns)
                       '((:alpha 1)
                         (:beta 2)
                         (:gamma 3))))
        (should (= (length native-calls) 1))
        (should (equal (car native-calls)
                       (substring source (car range) (cdr range))))))))

(ert-deftest emacs-load-artifact-compiler-test/artifact-source-read-form-nil-or-list-value-uses-native-batch-above-threshold-and-preserves-order ()
  (let ((load-garbage-collect-interval nil)
        (emacs-load-artifact-native-sections-native-reader-threshold 8)
        (path "/tmp/emacs-load-artifact-compiler-test-read-form-threshold.neln")
        (source "xx(\n  ; head comment\n  (:alpha 1)\n  (:beta 2 :nested (3 4))\n  (:gamma 3)\n)yy")
        (native-calls 0)
        (batch-calls nil)
        (read-single-calls 0)
        (read-incremental-calls 0)
        (full-read-calls 0)
        (pressure-calls 0))
    (let ((range (cons 2 (- (length source) 2))))
      (cl-letf (((symbol-function 'nelisp--read-all-from-string-native)
                 (lambda (&rest _args)
                   (setq native-calls (1+ native-calls))
                   (error "native whole-string read must be skipped")))
                ((symbol-function 'nelisp--read-batch-from-string-native)
                 (lambda (string byte-cursor batch-size)
                   (push (list string byte-cursor batch-size) batch-calls)
                   (should (= batch-size emacs-load-artifact-module-init-native-batch-items))
                   (should (= byte-cursor 0))
                   (should (equal string
                                  "(:alpha 1)\n  (:beta 2 :nested (3 4))\n  (:gamma 3)"))
                   (cons '((:alpha 1)
                           (:beta 2 :nested (3 4))
                           (:gamma 3))
                         (emacs-load--artifact-byte-length string))))
                ((symbol-function 'emacs-load--artifact-source-read-single-form-value)
                 (lambda (&rest _args)
                   (setq read-single-calls (1+ read-single-calls))
                   (error "single-form helper must not be called")))
                ((symbol-function 'emacs-load--artifact-source-read-incremental-form-value)
                 (lambda (&rest _args)
                   (setq read-incremental-calls (1+ read-incremental-calls))
                   (error "incremental helper must not be called")))
                ((symbol-function 'emacs-load--artifact-source-read-form)
                 (lambda (&rest _args)
                   (setq full-read-calls (1+ full-read-calls))
                   (error "full-source read helper must not be called")))
                ((symbol-function 'emacs-load--artifact-replay-pressure-maybe-gc)
                 (lambda ()
                   (setq pressure-calls (1+ pressure-calls))
                   nil)))
        (should (equal (emacs-load--artifact-source-read-form-nil-or-list-value
                        source range path :defuns)
                       '((:alpha 1)
                         (:beta 2 :nested (3 4))
                         (:gamma 3))))
        (should (= native-calls 0))
        (should (= read-single-calls 0))
        (should (= read-incremental-calls 0))
        (should (= full-read-calls 0))
        (should (= pressure-calls 1))
        (should (= (length batch-calls) 1))))))

(ert-deftest emacs-load-artifact-compiler-test/artifact-source-iterate-form-nil-or-list-value-invokes-pressure-checks-per-item ()
  (let ((load-garbage-collect-interval nil)
        (emacs-load-artifact-native-sections-native-reader-threshold 8)
        (path "/tmp/emacs-load-artifact-compiler-test-read-form-iterate-threshold.neln")
        (source "((alpha) (beta) (gamma))")
        (seen nil)
        (pressure-calls 0))
    (emacs-load-artifact-compiler-test--without-native-read-all
     (cl-letf (((symbol-function 'emacs-load--artifact-replay-pressure-maybe-gc)
               (lambda ()
                 (setq pressure-calls (1+ pressure-calls))
                 nil)))
       (should (= (emacs-load--artifact-source-iterate-form-nil-or-list-value
                   source (cons 0 (length source)) path :module-init
                   (lambda (item)
                     (push item seen)))
                  3))
       (should (= pressure-calls 3))
       (should (equal (nreverse seen) '((alpha) (beta) (gamma))))))))

(ert-deftest emacs-load-artifact-compiler-test/load-or-compile-passes-load-path-args-to-compiler ()
  (let* ((root (make-temp-file "emacs-load-artifact-compiler-test-" t))
         (dir-a (emacs-load-artifact-compiler-test--make-dir root "a"))
         (dir-b (emacs-load-artifact-compiler-test--make-dir root "b"))
         (rel-dir (emacs-load-artifact-compiler-test--make-dir root "rel"))
         (resolved (expand-file-name "sample.el" root))
         (artifact (expand-file-name "cache/4d/sample.neln" root))
         (sidecar (concat artifact ".source-sha256"))
         (temp-input (expand-file-name "compile-input.el" root))
         (source "(defun emacs-load-artifact-compiler-test--sample () t)\n")
         (compiler-path "/tmp/fake-nelisp")
         (call-process-args nil)
         (replay-path nil))
    (unwind-protect
        (progn
          (with-temp-file resolved
            (insert source))
          (make-directory (file-name-directory artifact) t)
          (cl-letf (((symbol-function 'emacs-load--artifact-cache-paths)
                     (lambda (_resolved)
                       (list artifact sidecar)))
                    ((symbol-function 'emacs-load--artifact-compiler)
                     (lambda () compiler-path))
                    ((symbol-function 'make-temp-file)
                     (lambda (&rest _args)
                       temp-input))
                    ((symbol-function 'call-process)
                     (lambda (&rest args)
                       (setq call-process-args args)
                       0))
                    ((symbol-function 'emacs-load--artifact-replay-file)
                     (lambda (path)
                       (setq replay-path path)
                       t)))
            (let ((default-directory root)
                  (emacs-load-auto-native-compile t)
                  (load-path (list dir-a
                                   nil
                                   ""
                                   "rel"
                                   dir-a
                                   dir-b
                                   rel-dir
                                   "rel"
                                   (expand-file-name "missing" root))))
              (should (eq (emacs-load--artifact-load-or-compile resolved source) t))
              (should (equal call-process-args
                             (append (list compiler-path nil nil nil)
                                     (list "compile-elisp-artifact"
                                           "--kind" "neln"
                                           "--input" temp-input
                                           "--output" artifact
                                           "--native-policy"
                                           "opportunistic"
                                           "--load-path" dir-a
                                           "--load-path" rel-dir
                                           "--load-path" dir-b))))
              (should (equal replay-path artifact))
              (should (null emacs-load--artifact-compile-diagnostic-report))
              (should (file-readable-p sidecar)))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest emacs-load-artifact-compiler-test/artifact-replay-file-prefetch-gc-runs-on-top-level ()
  (let* ((path "/tmp/emacs-load-artifact-compiler-test-top-level.neln")
         (artifact-content ";;; nelisp-private-nelc-v2\n(:module-init ())")
         (gc-calls 0)
         (seen-depths nil))
    (cl-letf (((symbol-function 'garbage-collect)
               (lambda ()
                 (setq gc-calls (1+ gc-calls))
                 'ok))
              ((symbol-function 'emacs-load--read-file-string)
               (lambda (_path)
                 artifact-content))
              ((symbol-function 'emacs-load--artifact-replay-payload-small)
               (lambda (_payload path)
                 (push (list path emacs-load--artifact-replay-depth) seen-depths)
                 :ok)))
      (let ((emacs-load-artifact-replay-preflight-gc t)
            (emacs-load-artifact-replay-streaming-threshold 0)
            (emacs-load--artifact-replay-depth 0))
        (should (eq :ok (emacs-load--artifact-replay-file path)))
        (should (= gc-calls 1))
        (should (equal seen-depths (list (list path 1))))))))

(ert-deftest emacs-load-artifact-compiler-test/artifact-replay-file-prefetch-gc-suppressed-for-nested ()
  (let* ((outer-path "/tmp/emacs-load-artifact-compiler-test-outer.neln")
         (inner-path "/tmp/emacs-load-artifact-compiler-test-inner.neln")
         (artifact-content ";;; nelisp-private-nelc-v2\n(:module-init ())")
         (gc-calls 0)
         (seen-depths nil))
    (cl-letf (((symbol-function 'garbage-collect)
               (lambda ()
                 (setq gc-calls (1+ gc-calls))
                 'ok))
              ((symbol-function 'emacs-load--read-file-string)
               (lambda (_path)
                 artifact-content))
              ((symbol-function 'emacs-load--artifact-replay-payload-small)
               (lambda (_payload path)
                 (push (list path emacs-load--artifact-replay-depth) seen-depths)
                 (when (string= path outer-path)
                   (emacs-load--artifact-replay-file inner-path))
                 :ok)))
      (let ((emacs-load-artifact-replay-preflight-gc t)
            (emacs-load-artifact-replay-streaming-threshold 0)
            (emacs-load--artifact-replay-depth 0))
        (should (eq :ok (emacs-load--artifact-replay-file outer-path)))
        (should (= gc-calls 1))
        (should (equal (sort seen-depths
                             (lambda (a b)
                               (string< (car a) (car b))))
                       (list (list inner-path 2)
                             (list outer-path 1))))))))

(ert-deftest emacs-load-artifact-compiler-test/artifact-replay-file-prefetch-gc-disabled ()
  (let* ((path "/tmp/emacs-load-artifact-compiler-test-disabled.neln")
         (artifact-content ";;; nelisp-private-nelc-v2\n(:module-init ())")
         (gc-calls 0))
    (cl-letf (((symbol-function 'garbage-collect)
               (lambda ()
                 (setq gc-calls (1+ gc-calls))
                 'ok))
              ((symbol-function 'emacs-load--read-file-string)
               (lambda (_path)
                 artifact-content))
              ((symbol-function 'emacs-load--artifact-replay-payload-small)
               (lambda (_payload _path) :ok)))
      (let ((emacs-load-artifact-replay-preflight-gc nil)
            (emacs-load-artifact-replay-streaming-threshold 0)
            (emacs-load--artifact-replay-depth 0))
        (should (eq :ok (emacs-load--artifact-replay-file path)))
	        (should (= gc-calls 0))))))

(ert-deftest emacs-load-artifact-compiler-test/artifact-replay-gc-interval-cap-default-is-20 ()
  (should (= emacs-load-artifact-replay-garbage-collect-interval-cap 20)))

(ert-deftest emacs-load-artifact-compiler-test/artifact-replay-file-gc-interval-cap-lowers-top-level-and-nested ()
  (let* ((outer-path "/tmp/emacs-load-artifact-compiler-test-gc-cap-outer.neln")
         (inner-path "/tmp/emacs-load-artifact-compiler-test-gc-cap-inner.neln")
         (artifact-content ";;; nelisp-private-nelc-v2\n(:module-init ())")
         (seen nil))
    (cl-letf (((symbol-function 'emacs-load--read-file-string)
               (lambda (_path)
                 artifact-content))
              ((symbol-function 'emacs-load--artifact-replay-payload-small)
               (lambda (_payload path)
                 (push (list path
                             emacs-load--artifact-replay-depth
                             load-garbage-collect-interval)
                       seen)
                 (when (string= path outer-path)
                   (emacs-load--artifact-replay-file inner-path))
                 :ok)))
      (let ((load-garbage-collect-interval 64)
            (emacs-load-artifact-replay-garbage-collect-interval-cap 20)
            (emacs-load-artifact-replay-preflight-gc nil)
            (emacs-load-artifact-replay-streaming-threshold 0)
            (emacs-load--artifact-replay-depth 0))
        (should (eq :ok (emacs-load--artifact-replay-file outer-path)))
        (should (= load-garbage-collect-interval 64))
        (should (equal (sort seen
                             (lambda (a b)
                               (string< (car a) (car b))))
                       (list (list inner-path 2 20)
                             (list outer-path 1 20))))))))

(ert-deftest emacs-load-artifact-compiler-test/artifact-replay-file-gc-interval-cap-keeps-lower-current-value ()
  (let* ((path "/tmp/emacs-load-artifact-compiler-test-gc-cap-lower-current.neln")
         (artifact-content ";;; nelisp-private-nelc-v2\n(:module-init ())")
         (seen nil))
    (cl-letf (((symbol-function 'emacs-load--read-file-string)
               (lambda (_path)
                 artifact-content))
              ((symbol-function 'emacs-load--artifact-replay-payload-small)
               (lambda (_payload replay-path)
                 (push (list replay-path
                             emacs-load--artifact-replay-depth
                             load-garbage-collect-interval)
                       seen)
                 :ok)))
      (let ((load-garbage-collect-interval 16)
            (emacs-load-artifact-replay-garbage-collect-interval-cap 20)
            (emacs-load-artifact-replay-preflight-gc nil)
            (emacs-load-artifact-replay-streaming-threshold 0)
            (emacs-load--artifact-replay-depth 0))
        (should (eq :ok (emacs-load--artifact-replay-file path)))
        (should (= load-garbage-collect-interval 16))
        (should (equal seen (list (list path 1 16))))))))

(ert-deftest emacs-load-artifact-compiler-test/artifact-replay-file-gc-interval-cap-preserves-disabled-semantics ()
  (let* ((cap-disabled-path "/tmp/emacs-load-artifact-compiler-test-gc-cap-disabled.neln")
         (global-disabled-path "/tmp/emacs-load-artifact-compiler-test-gc-global-disabled.neln")
         (artifact-content ";;; nelisp-private-nelc-v2\n(:module-init ())")
         (seen nil))
    (cl-letf (((symbol-function 'emacs-load--read-file-string)
               (lambda (_path)
                 artifact-content))
              ((symbol-function 'emacs-load--artifact-replay-payload-small)
               (lambda (_payload path)
                 (push (list path load-garbage-collect-interval) seen)
                 :ok)))
      (let ((load-garbage-collect-interval 64)
            (emacs-load-artifact-replay-garbage-collect-interval-cap nil)
            (emacs-load-artifact-replay-preflight-gc nil)
            (emacs-load-artifact-replay-streaming-threshold 0)
            (emacs-load--artifact-replay-depth 0))
        (should (eq :ok (emacs-load--artifact-replay-file cap-disabled-path))))
      (let ((load-garbage-collect-interval nil)
            (emacs-load-artifact-replay-garbage-collect-interval-cap 20)
            (emacs-load-artifact-replay-preflight-gc nil)
            (emacs-load-artifact-replay-streaming-threshold 0)
            (emacs-load--artifact-replay-depth 0))
        (should (eq :ok (emacs-load--artifact-replay-file global-disabled-path))))
      (should (equal (nreverse seen)
                     (list (list cap-disabled-path 64)
                           (list global-disabled-path nil)))))))

(ert-deftest emacs-load-artifact-compiler-test/artifact-replay-file-gc-interval-restores-after-success-and-error ()
  (let* ((success-path "/tmp/emacs-load-artifact-compiler-test-gc-restore-success.neln")
         (error-path "/tmp/emacs-load-artifact-compiler-test-gc-restore-error.neln")
         (artifact-content ";;; nelisp-private-nelc-v2\n(:module-init ())")
         (outside-interval 64)
         (seen nil))
    (cl-letf (((symbol-function 'emacs-load--read-file-string)
               (lambda (_path)
                 artifact-content))
              ((symbol-function 'emacs-load--artifact-replay-payload-small)
               (lambda (_payload path)
                 (push (list path load-garbage-collect-interval) seen)
                 (when (string= path error-path)
                   (error "forced artifact-replay failure"))
                 :ok)))
      (let ((load-garbage-collect-interval outside-interval)
            (emacs-load-artifact-replay-garbage-collect-interval-cap 20)
            (emacs-load-artifact-replay-preflight-gc nil)
            (emacs-load-artifact-replay-streaming-threshold 0)
            (emacs-load--artifact-replay-depth 0))
        (should (eq :ok (emacs-load--artifact-replay-file success-path)))
        (should (= load-garbage-collect-interval outside-interval))
        (should-error (emacs-load--artifact-replay-file error-path))
        (should (= load-garbage-collect-interval outside-interval))
        (should (equal (nreverse seen)
                       (list (list success-path 20)
                             (list error-path 20))))))))

(ert-deftest emacs-load-artifact-compiler-test/artifact-replay-file-pressure-baseline-initialized-at-top-level ()
  (let* ((path "/tmp/emacs-load-artifact-compiler-test-pressure-baseline.neln")
         (artifact-content ";;; nelisp-private-nelc-v2\n(:module-init ())")
         (gc-calls 0)
         (seen nil))
    (cl-letf (((symbol-function 'garbage-collect)
               (lambda ()
                 (setq gc-calls (1+ gc-calls))
                 'ok))
              ((symbol-function 'nelisp--arena-stats)
               (lambda ()
                 '(0 0 0 0 0 0 0 0 0 0 0 1234)))
              ((symbol-function 'emacs-load--read-file-string)
               (lambda (_path)
                 artifact-content))
              ((symbol-function 'emacs-load--artifact-replay-payload-small)
               (lambda (_payload path)
                 (push (list path
                             emacs-load--artifact-replay-pressure-baseline
                             emacs-load--artifact-replay-pressure-enabled)
                       seen)
                 :ok)))
      (let ((emacs-load-artifact-replay-preflight-gc t)
            (emacs-load-artifact-replay-pressure-threshold 2048)
            (emacs-load-artifact-replay-streaming-threshold 0)
            (emacs-load--artifact-replay-depth 0)
            (emacs-load--artifact-replay-pressure-baseline 'outside))
        (should (eq :ok (emacs-load--artifact-replay-file path)))
        (should (= gc-calls 1))
        (should (eq emacs-load--artifact-replay-pressure-baseline 'outside))
        (should (null emacs-load--artifact-replay-pressure-enabled))
        (should (equal seen (list (list path 1234 t))))))))

(ert-deftest emacs-load-artifact-compiler-test/artifact-replay-file-pressure-gc-below-threshold ()
  (let* ((outer-path "/tmp/emacs-load-artifact-compiler-test-pressure-below-outer.neln")
         (inner-path "/tmp/emacs-load-artifact-compiler-test-pressure-below-inner.neln")
         (artifact-content ";;; nelisp-private-nelc-v2\n(:module-init ())")
         (stats '(120 120))
         (stats-calls 0)
         (gc-calls 0)
         (seen nil))
    (cl-letf (((symbol-function 'garbage-collect)
               (lambda ()
                 (setq gc-calls (1+ gc-calls))
                 'ok))
              ((symbol-function 'nelisp--arena-stats)
               (lambda ()
                 (setq stats-calls (1+ stats-calls))
                 (let ((used (car stats)))
                   (setq stats (cdr stats))
                   (list 0 0 0 0 0 0 0 0 0 0 0 used))))
              ((symbol-function 'emacs-load--read-file-string)
               (lambda (_path)
                 artifact-content))
              ((symbol-function 'emacs-load--artifact-replay-payload-small)
               (lambda (_payload path)
                 (push (cons path emacs-load--artifact-replay-pressure-baseline) seen)
                 (when (string= path outer-path)
                   (emacs-load--artifact-replay-file inner-path))
                 :ok)))
      (let ((emacs-load-artifact-replay-preflight-gc t)
            (emacs-load-artifact-replay-pressure-threshold 1000)
            (emacs-load-artifact-replay-streaming-threshold 0)
            (emacs-load--artifact-replay-depth 0))
        (should (eq :ok (emacs-load--artifact-replay-file outer-path)))
        (should (= gc-calls 1))
        (should (= stats-calls 3))
        (should (equal (sort seen (lambda (a b) (string< (car a) (car b)))
                                 )
                       (list (cons inner-path 120)
                             (cons outer-path 120))))))))


(ert-deftest emacs-load-artifact-compiler-test/artifact-replay-file-pressure-gc-top-level-post-replay-crossing ()
  (let* ((path "/tmp/emacs-load-artifact-compiler-test-pressure-top-level-crossing.neln")
         (artifact-content ";;; nelisp-private-nelc-v2\n(:module-init ())")
         (gc-calls 0)
         (stats '(1400 7600))
         (stats-calls 0))
    (cl-letf (((symbol-function 'garbage-collect)
               (lambda ()
                 (setq gc-calls (1+ gc-calls))
                 'ok))
              ((symbol-function 'nelisp--arena-stats)
               (lambda ()
                 (setq stats-calls (1+ stats-calls))
                 (let ((used (car stats)))
                   (setq stats (cdr stats))
                   (list 0 0 0 0 0 0 0 0 0 0 0 used))))
              ((symbol-function 'emacs-load--read-file-string)
               (lambda (_path)
                 artifact-content))
              ((symbol-function 'emacs-load--artifact-replay-payload-small)
               (lambda (_payload _path)
                 :ok)))
      (let ((emacs-load-artifact-replay-preflight-gc t)
            (emacs-load-artifact-replay-pressure-threshold 5000)
            (emacs-load-artifact-replay-streaming-threshold 0)
            (emacs-load--artifact-replay-depth 0))
        (should (eq :ok (emacs-load--artifact-replay-file path)))
        (should (= gc-calls 2))
        (should (= stats-calls 3))
        (should (= emacs-load--artifact-replay-depth 0))))))

(ert-deftest emacs-load-artifact-compiler-test/artifact-replay-file-pressure-gc-below-threshold-top-level ()
  (let* ((path "/tmp/emacs-load-artifact-compiler-test-pressure-top-level-below.neln")
         (artifact-content ";;; nelisp-private-nelc-v2\n(:module-init ())")
         (gc-calls 0)
         (stats '(1400 3000))
         (stats-calls 0))
    (cl-letf (((symbol-function 'garbage-collect)
               (lambda ()
                 (setq gc-calls (1+ gc-calls))
                 'ok))
              ((symbol-function 'nelisp--arena-stats)
               (lambda ()
                 (setq stats-calls (1+ stats-calls))
                 (let ((used (car stats)))
                   (setq stats (cdr stats))
                   (list 0 0 0 0 0 0 0 0 0 0 0 used))))
              ((symbol-function 'emacs-load--read-file-string)
               (lambda (_path)
                 artifact-content))
              ((symbol-function 'emacs-load--artifact-replay-payload-small)
               (lambda (_payload _path)
                 :ok)))
      (let ((emacs-load-artifact-replay-preflight-gc t)
            (emacs-load-artifact-replay-pressure-threshold 8000)
            (emacs-load-artifact-replay-streaming-threshold 0)
            (emacs-load--artifact-replay-depth 0))
        (should (eq :ok (emacs-load--artifact-replay-file path)))
        (should (= gc-calls 1))
        (should (= stats-calls 2))
        (should (= emacs-load--artifact-replay-depth 0))))))

(ert-deftest emacs-load-artifact-compiler-test/artifact-replay-file-pressure-gc-batch-boundary-check-after-drain ()
  (let* ((path "/tmp/emacs-load-artifact-compiler-test-pressure-boundary.neln")
         (source "(alpha) (beta) (gamma) (delta)")
         (batch-sources nil)
         (drain-events nil)
         (batch-items 0))
    (cl-letf (((symbol-value 'emacs-load-artifact-module-init-native-batch-items) 2)
              ((symbol-value 'load-garbage-collect-interval) 0)
              ((symbol-function 'nelisp--read-batch-from-string-native)
               (lambda (string byte-cursor batch-size)
                 (push string batch-sources)
                 (should (= byte-cursor 0))
                 (should (= batch-size 2))
                 (cond
                  ((string= string "(alpha) (beta)")
                   (cons '((alpha) (beta))
                         (emacs-load--artifact-byte-length string)))
                  ((string= string "(gamma) (delta)")
                   (cons '((gamma) (delta))
                         (emacs-load--artifact-byte-length string)))
                  (t
                   (error "unexpected batch source %S" string)))))
              ((symbol-function 'emacs-load--artifact-replay-pressure-maybe-gc)
               (lambda ()
                 (push batch-items drain-events)
                 (setq batch-items 0)))
              ((symbol-function 'emacs-load--artifact-replay-item)
               (lambda (_payload _path _native-section _base)
                 (setq batch-items (1+ batch-items)))))
      (let ((result (emacs-load--artifact-source-iterate-form-nil-or-list-value-native-batch
                     source path :module-init
                     (lambda (item)
                       (setq batch-items (1+ batch-items))
                       item))))
        (should (= result 4))
        (should (equal (nreverse batch-sources)
                       '("(alpha) (beta)" "(gamma) (delta)")))
        (should (equal (nreverse drain-events) '(2 2)))))))

(ert-deftest emacs-load-artifact-compiler-test/artifact-replay-file-pressure-gc-malformed-stats-top-level-post-check ()
  (let* ((path "/tmp/emacs-load-artifact-compiler-test-pressure-top-level-malformed.neln")
         (artifact-content ";;; nelisp-private-nelc-v2\n(:module-init ())")
         (gc-calls 0)
         (stats '((0 0 0 0 0 0 0 0 0 0 0 1200)
                  invalid))
         (stats-calls 0))
    (cl-letf (((symbol-function 'garbage-collect)
               (lambda ()
                 (setq gc-calls (1+ gc-calls))
                 'ok))
              ((symbol-function 'nelisp--arena-stats)
               (lambda ()
                 (let ((value (pop stats)))
                   (setq stats-calls (1+ stats-calls))
                   value)))
              ((symbol-function 'emacs-load--read-file-string)
               (lambda (_path)
                 artifact-content))
              ((symbol-function 'emacs-load--artifact-replay-payload-small)
               (lambda (_payload _path)
                 :ok)))
      (let ((emacs-load-artifact-replay-preflight-gc t)
            (emacs-load-artifact-replay-pressure-threshold 64)
            (emacs-load-artifact-replay-streaming-threshold 0)
            (emacs-load--artifact-replay-depth 0))
        (should (eq :ok (emacs-load--artifact-replay-file path)))
        (should (= gc-calls 1))
        (should (= stats-calls 2))
        (should (= emacs-load--artifact-replay-depth 0))))))

(ert-deftest emacs-load-artifact-compiler-test/artifact-replay-file-pressure-gc-no-duplicate-gc-after-refresh ()
  (let* ((path "/tmp/emacs-load-artifact-compiler-test-pressure-refresh-no-duplicate.neln")
         (artifact-content ";;; nelisp-private-nelc-v2\n(:module-init ((alpha) (beta) (gamma) (delta)))")
         (source "((alpha) (beta) (gamma) (delta))")
         (source-byte-length (emacs-load--artifact-byte-length source))
         (stats '(1300 2900 2900 2920))
         (stats-calls 0)
         (gc-calls 0)
         (seen nil))
    (cl-letf (((symbol-function 'garbage-collect)
               (lambda ()
                 (setq gc-calls (1+ gc-calls))
                 'ok))
              ((symbol-function 'nelisp--arena-stats)
               (lambda ()
                 (setq stats-calls (1+ stats-calls))
                 (let ((used (car stats)))
                   (setq stats (cdr stats))
                   (list 0 0 0 0 0 0 0 0 0 0 0 used))))
              ((symbol-function 'emacs-load--read-file-string)
               (lambda (_path)
                 artifact-content))
              ((symbol-function 'nelisp--read-batch-from-string-native)
               (lambda (string byte-cursor batch-size)
                 (should (string= string source))
                 (should (= batch-size 2))
                 (cond
                  ((= byte-cursor 0)
                   (cons '((alpha) (beta)) 8))
                  ((= byte-cursor 8)
                   (cons '((gamma) (delta)) source-byte-length))
                  (t
                   (error "unexpected byte cursor %S" byte-cursor)))))
              ((symbol-value 'emacs-load-artifact-module-init-native-batch-items) 2)
              ((symbol-value 'load-garbage-collect-interval) 0)
              ((symbol-function 'emacs-load--artifact-replay-item)
               (lambda (item &rest _args)
                 (push item seen))))
      (let ((emacs-load-artifact-replay-preflight-gc nil)
            (emacs-load-artifact-replay-pressure-threshold 1000)
            (emacs-load-artifact-replay-streaming-threshold 0)
            (emacs-load--artifact-replay-depth 0))
        (should (consp (emacs-load--artifact-replay-file path)))
        (should (= gc-calls 1))
        (should (= stats-calls 3))
        (should (equal (nreverse seen) '((alpha) (beta) (gamma) (delta))))))))

(ert-deftest emacs-load-artifact-compiler-test/artifact-replay-file-pressure-gc-crossing-refreshes-baseline-once ()
  (let* ((outer-path "/tmp/emacs-load-artifact-compiler-test-pressure-refresh-outer.neln")
         (inner-path "/tmp/emacs-load-artifact-compiler-test-pressure-refresh-inner.neln")
         (grandchild-path "/tmp/emacs-load-artifact-compiler-test-pressure-refresh-grandchild.neln")
         (artifact-content ";;; nelisp-private-nelc-v2\n(:module-init ())")
         (stats '(1000 1800 2300 3000))
         (stats-calls 0)
         (gc-calls 0)
         (seen nil))
    (cl-letf (((symbol-function 'garbage-collect)
               (lambda ()
                 (setq gc-calls (1+ gc-calls))
                 'ok))
              ((symbol-function 'nelisp--arena-stats)
               (lambda ()
                 (setq stats-calls (1+ stats-calls))
                 (let ((used (car stats)))
                   (setq stats (cdr stats))
                   (list 0 0 0 0 0 0 0 0 0 0 0 used))))
              ((symbol-function 'emacs-load--read-file-string)
               (lambda (_path)
                 artifact-content))
              ((symbol-function 'emacs-load--artifact-replay-payload-small)
               (lambda (_payload path)
                 (push (list path
                             emacs-load--artifact-replay-depth
                             emacs-load--artifact-replay-pressure-baseline
                             emacs-load--artifact-replay-pressure-enabled)
                       seen)
                 (cond
                  ((string= path outer-path)
                   (emacs-load--artifact-replay-file inner-path))
                  ((string= path inner-path)
                   (emacs-load--artifact-replay-file grandchild-path))
                  (t nil))
                 :ok)))
      (let ((emacs-load-artifact-replay-preflight-gc t)
            (emacs-load-artifact-replay-pressure-threshold 1000)
            (emacs-load-artifact-replay-streaming-threshold 0)
            (emacs-load--artifact-replay-depth 0)
            (emacs-load--artifact-replay-pressure-baseline 'outside)
            (emacs-load--artifact-replay-pressure-enabled nil))
        (should (eq :ok (emacs-load--artifact-replay-file outer-path)))
        (should (= gc-calls 2))
        (should (= stats-calls 5))
        (should (eq emacs-load--artifact-replay-pressure-baseline 'outside))
        (should (null emacs-load--artifact-replay-pressure-enabled))
        (should (equal (sort seen
                             (lambda (a b)
                               (string< (nth 0 a) (nth 0 b))))
                       (list (list grandchild-path 3 3000 t)
                             (list inner-path 2 1000 t)
                             (list outer-path 1 1000 t))))))))

(ert-deftest emacs-load-artifact-compiler-test/artifact-replay-file-pressure-gc-disabled-for-malformed-stats ()
  (let* ((outer-path "/tmp/emacs-load-artifact-compiler-test-pressure-malformed-outer.neln")
         (inner-path "/tmp/emacs-load-artifact-compiler-test-pressure-malformed-inner.neln")
         (artifact-content ";;; nelisp-private-nelc-v2\n(:module-init ())")
         (gc-calls 0)
         (stats-calls 0)
         (seen nil))
    (cl-letf (((symbol-function 'garbage-collect)
               (lambda ()
                 (setq gc-calls (1+ gc-calls))
                 'ok))
              ((symbol-function 'nelisp--arena-stats)
               (lambda ()
                 (setq stats-calls (1+ stats-calls))
                 '(a b c)))
              ((symbol-function 'emacs-load--read-file-string)
               (lambda (_path)
                 artifact-content))
              ((symbol-function 'emacs-load--artifact-replay-payload-small)
               (lambda (_payload path)
                 (push (list path emacs-load--artifact-replay-pressure-enabled) seen)
                 (when (string= path outer-path)
                   (emacs-load--artifact-replay-file inner-path))
                 :ok)))
      (let ((emacs-load-artifact-replay-preflight-gc t)
            (emacs-load-artifact-replay-pressure-threshold 64)
            (emacs-load-artifact-replay-streaming-threshold 0)
            (emacs-load--artifact-replay-depth 0))
        (should (eq :ok (emacs-load--artifact-replay-file outer-path)))
        (should (= gc-calls 1))
        (should (= stats-calls 1))
        (should (equal (sort seen
                             (lambda (a b)
                               (string< (car a) (car b)))
                           )
                       (list (list inner-path nil)
                             (list outer-path nil))))))))

(ert-deftest emacs-load-artifact-compiler-test/artifact-replay-file-pressure-gc-disabled-for-erroring-stats ()
  (let* ((outer-path "/tmp/emacs-load-artifact-compiler-test-pressure-erroring-outer.neln")
         (inner-path "/tmp/emacs-load-artifact-compiler-test-pressure-erroring-inner.neln")
         (artifact-content ";;; nelisp-private-nelc-v2\n(:module-init ())")
         (gc-calls 0)
         (stats-calls 0)
         (seen nil))
    (cl-letf (((symbol-function 'garbage-collect)
               (lambda ()
                 (setq gc-calls (1+ gc-calls))
                 'ok))
              ((symbol-function 'nelisp--arena-stats)
               (lambda ()
                 (setq stats-calls (1+ stats-calls))
                 (error "arena stats failure")))
              ((symbol-function 'emacs-load--read-file-string)
               (lambda (_path)
                 artifact-content))
              ((symbol-function 'emacs-load--artifact-replay-payload-small)
               (lambda (_payload path)
                 (push (list path emacs-load--artifact-replay-pressure-enabled) seen)
                 (when (string= path outer-path)
                   (emacs-load--artifact-replay-file inner-path))
                 :ok)))
      (let ((emacs-load-artifact-replay-preflight-gc t)
            (emacs-load-artifact-replay-pressure-threshold 64)
            (emacs-load-artifact-replay-streaming-threshold 0)
            (emacs-load--artifact-replay-depth 0))
        (should (eq :ok (emacs-load--artifact-replay-file outer-path)))
        (should (= gc-calls 1))
        (should (= stats-calls 1))
        (should (equal (sort seen
                             (lambda (a b)
                               (string< (car a) (car b)))
                           )
                       (list (list inner-path nil)
                             (list outer-path nil))))))))

(ert-deftest emacs-load-artifact-compiler-test/artifact-replay-file-pressure-gc-disabled-by-threshold ()
  (let* ((outer-path "/tmp/emacs-load-artifact-compiler-test-pressure-disabled-outer.neln")
         (inner-path "/tmp/emacs-load-artifact-compiler-test-pressure-disabled-inner.neln")
         (artifact-content ";;; nelisp-private-nelc-v2\n(:module-init ())")
         (gc-calls 0)
         (seen nil))
    (cl-letf (((symbol-function 'garbage-collect)
               (lambda ()
                 (setq gc-calls (1+ gc-calls))
                 'ok))
              ((symbol-function 'nelisp--arena-stats)
               (lambda ()
                 '(0 0 0 0 0 0 0 0 0 0 0 999999999)))
              ((symbol-function 'emacs-load--read-file-string)
               (lambda (_path)
                 artifact-content))
              ((symbol-function 'emacs-load--artifact-replay-payload-small)
               (lambda (_payload path)
                 (push (list path emacs-load--artifact-replay-pressure-baseline) seen)
                 (when (string= path outer-path)
                   (emacs-load--artifact-replay-file inner-path))
                 :ok)))
      (let ((emacs-load-artifact-replay-preflight-gc t)
            (emacs-load-artifact-replay-pressure-threshold nil)
            (emacs-load-artifact-replay-streaming-threshold 0)
            (emacs-load--artifact-replay-depth 0))
        (should (eq :ok (emacs-load--artifact-replay-file outer-path)))
        (should (= gc-calls 1))
        (should (equal seen
                       (list (list inner-path nil)
                             (list outer-path nil))))))))

(ert-deftest emacs-load-artifact-compiler-test/artifact-replay-pressure-bytes-prefers-allocation-debt ()
  (let ((saved-debt (when (fboundp 'nelisp--allocation-debt)
                      (symbol-function 'nelisp--allocation-debt)))
        (saved-stats (when (fboundp 'nelisp--arena-stats)
                       (symbol-function 'nelisp--arena-stats))))
    (unwind-protect
        (cl-letf (((symbol-function 'nelisp--allocation-debt)
                   (lambda () 987654321))
                  ((symbol-function 'nelisp--arena-stats)
                   (lambda () '(0 0 0 0 0 0 0 0 0 0 0 1234))))
          (should (= (emacs-load--artifact-replay-pressure-bytes) 987654321))
          (should (= (emacs-load--artifact-replay-arena-used-bytes) 987654321)))
      (if saved-debt
          (fset 'nelisp--allocation-debt saved-debt)
        (fmakunbound 'nelisp--allocation-debt))
      (if saved-stats
          (fset 'nelisp--arena-stats saved-stats)
        (fmakunbound 'nelisp--arena-stats)))))

(ert-deftest emacs-load-artifact-compiler-test/artifact-replay-arena-used-bytes-returns-only-valid-used-bytes ()
  (let ((saved (when (fboundp 'nelisp--arena-stats)
                 (symbol-function 'nelisp--arena-stats)))
        (saved-debt (when (fboundp 'nelisp--allocation-debt)
                      (symbol-function 'nelisp--allocation-debt))))
    (unwind-protect
        (progn
          (when (fboundp 'nelisp--allocation-debt)
            (fmakunbound 'nelisp--allocation-debt))
          (when saved
            (fmakunbound 'nelisp--arena-stats))
          (should (null (emacs-load--artifact-replay-arena-used-bytes)))
          (emacs-load-artifact-compiler-test--with-temporary-arena-stats
              (lambda () '(0 0 0 0 0 0 0 0 0 0 0 1234 999))
            (should (= (emacs-load--artifact-replay-pressure-bytes) 1234))
            (should (= (emacs-load--artifact-replay-arena-used-bytes) 1234)))
          (emacs-load-artifact-compiler-test--with-temporary-arena-stats
              (lambda () '(0 1 2))
            (should (null (emacs-load--artifact-replay-arena-used-bytes))))
          (emacs-load-artifact-compiler-test--with-temporary-arena-stats
              (lambda () '(0 1 2 3 4 5 6 7 8 9 10 . 11))
            (should (null (emacs-load--artifact-replay-arena-used-bytes))))
          (emacs-load-artifact-compiler-test--with-temporary-arena-stats
              (lambda () '(0 1 2 3 4 5 6 7 8 9 10 foo))
            (should (null (emacs-load--artifact-replay-arena-used-bytes))))
          (emacs-load-artifact-compiler-test--with-temporary-arena-stats
              (lambda () '(0 1 2 3 4 5 6 7 8 9 10 -1))
            (should (null (emacs-load--artifact-replay-arena-used-bytes))))
          (emacs-load-artifact-compiler-test--with-temporary-arena-stats
              (lambda () (error "arena stats failure"))
            (should (null (emacs-load--artifact-replay-arena-used-bytes)))))
      (if saved
          (fset 'nelisp--arena-stats saved)
        (fmakunbound 'nelisp--arena-stats))
      (if saved-debt
          (fset 'nelisp--allocation-debt saved-debt)
        (fmakunbound 'nelisp--allocation-debt))))
)

(ert-deftest emacs-load-artifact-compiler-test/artifact-replay-file-prefetch-gc-restores-depth-after-error ()
  (let* ((path "/tmp/emacs-load-artifact-compiler-test-error.neln")
         (artifact-content ";;; nelisp-private-nelc-v2\n(:module-init ())")
         (gc-calls 0))
    (cl-letf (((symbol-function 'garbage-collect)
               (lambda ()
                 (setq gc-calls (1+ gc-calls))
                 'ok))
              ((symbol-function 'emacs-load--read-file-string)
               (lambda (_path)
                 artifact-content))
              ((symbol-function 'emacs-load--artifact-replay-payload-small)
               (lambda (_payload _path)
                 (should (= emacs-load--artifact-replay-depth 1))
                 (setq emacs-load--artifact-replay-pressure-baseline 4321)
                 (setq emacs-load--artifact-replay-pressure-enabled t)
                 (error "forced artifact-replay failure"))))
      (let ((emacs-load-artifact-replay-preflight-gc t)
            (emacs-load-artifact-replay-streaming-threshold 0)
            (emacs-load-artifact-replay-pressure-threshold 128)
            (emacs-load--artifact-replay-pressure-baseline 1111)
            (emacs-load--artifact-replay-pressure-enabled nil)
            (emacs-load--artifact-replay-depth 0))
        (should-error (emacs-load--artifact-replay-file path))
        (should (= gc-calls 1))
        (should (= emacs-load--artifact-replay-depth 0))
        (should (= emacs-load--artifact-replay-pressure-baseline 1111))
        (should (null emacs-load--artifact-replay-pressure-enabled))))))
(ert-deftest emacs-load-artifact-compiler-test/cache-source-digest-bumps-from-v5-salt-for-64k-native-section-budget ()
  (let* ((source (concat "org-macs: " (string ?\0) "bilingual あ"))
         (old-salt "source-defun-fallback-v5-standalone-replay-gc-safe-native-shard-budget-4-defuns-per-section")
         (old-digest (emacs-load--sha256 (concat old-salt "\0" source)))
         (new-digest (emacs-load--artifact-cache-source-digest source)))
    (should (string-match-p "v6" emacs-load--artifact-cache-source-digest-salt))
    (should (string-match-p "64KiB-serialized-native-section-replay-byte-budget"
                            emacs-load--artifact-cache-source-digest-salt))
    (should (string= new-digest
                     (emacs-load--sha256
                      (concat emacs-load--artifact-cache-source-digest-salt
                              "\0"
                              source))))
    (should-not (string= new-digest old-digest))))

(ert-deftest emacs-load-artifact-compiler-test/sha256-standalone-prefers-native-helper ()
  (let ((native-calls 0)
        (secure-calls 0))
    (cl-letf (((symbol-function 'emacs-load--standalone-runtime-p)
               (lambda () t))
              ((symbol-function 'nelisp--sha256)
               (lambda (_input)
                 (setq native-calls (1+ native-calls))
                 "native-digest"))
              ((symbol-function 'secure-hash)
               (lambda (_alg _input)
                 (setq secure-calls (1+ secure-calls))
                 (error "secure-hash unexpectedly called"))))
      (should (string= (emacs-load--sha256 "abc") "native-digest"))
      (should (= native-calls 1))
      (should (= secure-calls 0)))))

(ert-deftest emacs-load-artifact-compiler-test/sha256-host-uses-secure-hash-first ()
  (let ((native-calls 0)
        (secure-calls 0))
    (cl-letf (((symbol-function 'emacs-load--standalone-runtime-p)
               (lambda () nil))
              ((symbol-function 'secure-hash)
               (lambda (_alg _input)
                 (setq secure-calls (1+ secure-calls))
                 "secure-digest"))
              ((symbol-function 'nelisp--sha256)
               (lambda (_input)
                 (setq native-calls (1+ native-calls))
                 (error "standalone hash unexpectedly called"))))
      (should (string= (emacs-load--sha256 "abc") "secure-digest"))
      (should (= secure-calls 1))
      (should (= native-calls 0)))))

(ert-deftest emacs-load-artifact-compiler-test/sha256-host-fallback-errors-preserve-message ()
  (let ((err nil))
    (cl-letf (((symbol-function 'emacs-load--standalone-runtime-p)
               (lambda () nil))
              ((symbol-function 'secure-hash)
               (lambda (_alg _input) nil))
              ((symbol-function 'nelisp--sha256)
               (lambda (_input) nil)))
      (setq err
            (condition-case caught
                (progn
                  (emacs-load--sha256 "abc")
                  nil)
              (error caught)))
      (should err)
      (should (equal (error-message-string err)
                     "secure-hash returned nil for SHA-256")))))

(ert-deftest emacs-load-artifact-compiler-test/native-sections-threshold-keeps-small-values-on-native-path ()
  (let ((emacs-load-artifact-native-sections-native-reader-threshold 4096)
        (native-calls 0)
        (path "/tmp/emacs-load-artifact-compiler-test-native-sections.neln")
        (nil-source " nil ")
        (single-source
         (concat
          "  ((:arch \"x86_64\"\n"
          "    :text-base64 \"QUJD\"\n"
          "    :relocs nil\n"
          "    :extern-symbols nil\n"
          "    :defuns nil\n"
          "    :object-base64 \"heavy-a\"\n"
          "    :compile-report (:status ok)\n"
          "    :symbols (sym-a)))")))
    (cl-letf (((symbol-function 'nelisp--read-all-from-string-native)
               (lambda (string)
                 (setq native-calls (1+ native-calls))
                 (list (car (read-from-string string)))))
              ((symbol-function 'emacs-load--artifact-source-read-form-ranges)
               (lambda (&rest _args)
                 (error "iterative fallback must not be called"))))
      (should (null (emacs-load--artifact-native-sections-from-source
                     nil-source 0 (length nil-source) path)))
      (should (equal (emacs-load--artifact-native-sections-from-source
                      single-source 0 (length single-source) path)
                     '((:arch "x86_64"
                        :text-base64 "QUJD"
                        :relocs nil
                        :extern-symbols nil
                        :defuns nil))))
      (should (= native-calls 2)))))

(ert-deftest emacs-load-artifact-compiler-test/native-sections-threshold-routes-large-values-to-iterative-parser ()
  (let* ((emacs-load-artifact-native-sections-native-reader-threshold 1)
        (path "/tmp/emacs-load-artifact-compiler-test-native-sections-large.neln")
        (first-section
         "(:arch \"x86_64\" :text-base64 \"QUJD\" :relocs nil :extern-symbols nil :defuns nil)")
        (second-section
         "(:arch \"arm64\" :text-base64 \"REVG\" :relocs nil :extern-symbols nil :defuns nil)")
        (source (concat " (" first-section "\n " second-section ")"))
        (read-calls nil)
        (section-calls nil))
    (cl-letf (((symbol-function 'nelisp--read-all-from-string-native)
               (lambda (&rest _args)
                 (error "native whole-reader must not be called")))
              ((symbol-function 'emacs-load--artifact-source-read-form-ranges)
               (lambda (source start end call-path)
                 (push (list :source source :start start :end end :path call-path)
                       read-calls)
                 (should (member source (list first-section second-section)))
                 (should (= start 0))
                 (should (= end (length source)))
                 (if (string= source first-section)
                     '((:arch . (1 . 16))
                       (:text-base64 . (31 . 37))
                       (:relocs . (47 . 50))
                       (:extern-symbols . (68 . 71))
                       (:defuns . (80 . 83)))
                   '((:arch . (1 . 15))
                     (:text-base64 . (30 . 36))
                     (:relocs . (46 . 49))
                     (:extern-symbols . (67 . 70))
                     (:defuns . (79 . 82))))))
              ((symbol-function 'emacs-load--artifact-native-section-from-ranges)
               (lambda (source ranges call-path)
                 (push (list :source source :ranges ranges :path call-path)
                       section-calls)
                 (should (member source (list first-section second-section)))
                 (cond
                  ((string= source first-section)
                   (should (equal ranges
                                  '((:arch . (1 . 16))
                                    (:text-base64 . (31 . 37))
                                    (:relocs . (47 . 50))
                                    (:extern-symbols . (68 . 71))
                                    (:defuns . (80 . 83)))))
                   '(:arch "x86_64"
                     :text-base64 "QUJD"
                     :relocs nil
                     :extern-symbols nil
                     :defuns nil))
                  ((string= source second-section)
                   (should (equal ranges
                                  '((:arch . (1 . 15))
                                    (:text-base64 . (30 . 36))
                                    (:relocs . (46 . 49))
                                    (:extern-symbols . (67 . 70))
                                    (:defuns . (79 . 82)))))
                   '(:arch "arm64"
                     :text-base64 "REVG"
                     :relocs nil
                     :extern-symbols nil
                     :defuns nil))
                  (t
                   (error "unexpected section source %S" source))))))
      (should (equal (emacs-load--artifact-native-sections-from-source
                      source 0 (length source) path)
                     '((:arch "x86_64"
                        :text-base64 "QUJD"
                        :relocs nil
                        :extern-symbols nil
                        :defuns nil)
                       (:arch "arm64"
                        :text-base64 "REVG"
                        :relocs nil
                        :extern-symbols nil
                        :defuns nil))))
      (should (= (length read-calls) 2))
      (should (= (length section-calls) 2)))))

(ert-deftest emacs-load-artifact-compiler-test/artifact-source-iterate-form-nil-or-list-value-replays-items-in-order-and-avoids-whole-list-read ()
  (let* ((path "/tmp/emacs-load-artifact-compiler-test-module-init.neln")
         (source "((alpha) (beta))")
         (alpha-source "(alpha)")
         (beta-source "(beta)")
         (native-inputs nil)
         (seen nil))
    (cl-letf (((symbol-function 'nelisp--read-all-from-string-native)
              (lambda (string)
                 (push string native-inputs)
                 (cond
                  ((string= string source) nil)
                  ((string= string alpha-source) '((alpha)))
                  ((string= string beta-source) '((beta)))
                  (t
                   (error "unexpected native read input %S" string)))))
              ((symbol-function 'read-from-string)
               (lambda (&rest _args)
                 (error "host reader should not be used"))))
      (should (= (emacs-load--artifact-source-iterate-form-nil-or-list-value
                  source (cons 0 (length source)) path :module-init
                  (lambda (item)
                    (push item seen)))
                 2))
      (should (equal (nreverse seen) '((alpha) (beta))))
      (should (equal (nreverse native-inputs) (list alpha-source beta-source)))
      (should-not (member source native-inputs)))))

(ert-deftest emacs-load-artifact-compiler-test/artifact-source-iterate-form-nil-or-list-value-falls-back-to-host-reader-when-native-reader-is-unavailable ()
  (let* ((path "/tmp/emacs-load-artifact-compiler-test-module-init-host.neln")
         (source "((alpha) (beta))")
         (seen nil))
    (emacs-load-artifact-compiler-test--without-native-read-all
     (should (= (emacs-load--artifact-source-iterate-form-nil-or-list-value
                 source (cons 0 (length source)) path :module-init
                 (lambda (item)
                   (push item seen)))
                2))
     (should (equal (nreverse seen) '((alpha) (beta)))))))

(ert-deftest emacs-load-artifact-compiler-test/artifact-source-iterate-form-nil-or-list-value-rejects-nil-malformed-and-trailing-input ()
  (let ((path "/tmp/emacs-load-artifact-compiler-test-module-init-invalid.neln"))
    (emacs-load-artifact-compiler-test--without-native-read-all
     (dolist (case '(("nil" . 0)
                     ("(alpha . beta)" . "invalid :module-init in /tmp/emacs-load-artifact-compiler-test-module-init-invalid.neln")
                     ("(alpha) extra" . "invalid :module-init in /tmp/emacs-load-artifact-compiler-test-module-init-invalid.neln")))
       (let* ((source (car case))
              (expected (cdr case))
              (seen nil)
              (err nil))
         (setq err
               (condition-case caught
                   (progn
                     (emacs-load--artifact-source-iterate-form-nil-or-list-value
                      source (cons 0 (length source)) path :module-init
                      (lambda (item)
                        (push item seen)))
                     nil)
                 (error caught)))
         (if (numberp expected)
             (progn
               (should (null err))
               (should (= expected 0))
               (should (null seen)))
           (should err)
           (should (equal (error-message-string err) expected))))))))

(ert-deftest emacs-load-artifact-compiler-test/artifact-source-iterate-form-nil-or-list-value-summary-count-matches-replayed-items ()
  (let* ((path "/tmp/emacs-load-artifact-compiler-test-module-init-summary.neln")
         (source "((alpha) (beta))")
         (module-init-source source)
         (alpha-source "(alpha)")
         (beta-source "(beta)")
         (seen nil)
         (native-inputs nil)
         (count nil)
         (summary nil))
    (cl-letf (((symbol-function 'nelisp--read-all-from-string-native)
               (lambda (string)
                 (push string native-inputs)
                 (cond
                  ((string= string module-init-source) nil)
                  ((string= string alpha-source) (list 'alpha))
                  ((string= string beta-source) (list 'beta))
                  (t
                   (error "unexpected native read input %S" string))))))
      (setq count
            (emacs-load--artifact-source-iterate-form-nil-or-list-value
             source (cons 0 (length source)) path :module-init
             (lambda (item)
               (push item seen))))
      (setq summary
            (emacs-load--artifact-streaming-summary
             path 123456 nil count '(:module-init)))
      (should (= count 2))
      (should (equal (nreverse seen) '(alpha beta)))
      (should (equal (nreverse native-inputs) (list alpha-source beta-source)))
      (should-not (member module-init-source native-inputs))
      (should (eq (plist-get summary :streaming) t))
      (should (equal (plist-get summary :artifact) path))
      (should (= (plist-get summary :module-init-count) 2))
      (should (equal (plist-get summary :fields) '(:module-init))))))

(ert-deftest emacs-load-artifact-compiler-test/artifact-source-iterate-form-nil-or-list-value-native-batch-replays-items-in-order-and-counts ()
  (let* ((path "/tmp/emacs-load-artifact-compiler-test-module-init-native-batch.neln")
         (source "  (alpha) ; keep this separator\n(beta) (gamma)  ")
         (seen nil)
         (batch-calls nil))
    (cl-letf (((symbol-value 'emacs-load-artifact-module-init-native-batch-items) 2)
              ((symbol-function 'nelisp--read-batch-from-string-native)
               (lambda (string byte-cursor batch-size)
                 (push (list string byte-cursor batch-size) batch-calls)
                 (should (= byte-cursor 0))
                 (should (= batch-size 2))
                 (cond
                  ((string= string "(alpha) ; keep this separator\n(beta)")
                   (cons '((alpha) (beta))
                         (emacs-load--artifact-byte-length string)))
                  ((string= string "(gamma)")
                   (cons '((gamma))
                         (emacs-load--artifact-byte-length string)))
                  (t
                   (error "unexpected batch source %S" string)))))
              ((symbol-function 'read-from-string)
               (lambda (&rest _args)
                 (error "host reader should not be used"))))
      (should (= (emacs-load--artifact-source-iterate-form-nil-or-list-value-native-batch
                  source path :module-init
                  (lambda (item)
                    (push item seen)))
                 3))
      (should (equal (nreverse seen) '((alpha) (beta) (gamma))))
      (should
       (equal (nreverse batch-calls)
              '(("(alpha) ; keep this separator\n(beta)" 0 2)
                ("(gamma)" 0 2))))
      (should-not (member source (mapcar #'car batch-calls))))))

(ert-deftest emacs-load-artifact-compiler-test/artifact-source-native-batch-prefers-vector-and-clears-drained-slots ()
  (let* ((path "/tmp/emacs-load-artifact-compiler-test-module-init-native-vector.neln")
         (source "(alpha) (beta) (gamma)")
         (seen nil)
         (returned nil)
         (vector-calls nil))
    (cl-letf (((symbol-value 'emacs-load-artifact-module-init-native-batch-items) 3)
              ((symbol-function 'nelisp--read-batch-vector-from-string-native)
               (lambda (string byte-cursor batch-size)
                 (push (list string byte-cursor batch-size) vector-calls)
                 (setq returned
                       (vector (emacs-load--artifact-byte-length string)
                               3 '(alpha) '(beta) '(gamma)))
                 returned))
              ((symbol-function 'nelisp--read-batch-from-string-native)
               (lambda (&rest _args)
                 (error "list batch reader should not be used")))
              ((symbol-function 'read-from-string)
               (lambda (&rest _args)
                 (error "host reader should not be used"))))
      (should (= (emacs-load--artifact-source-iterate-form-nil-or-list-value-native-batch
                  source path :module-init
                  (lambda (item)
                    (push item seen)))
                 3))
      (should (equal (nreverse seen) '((alpha) (beta) (gamma))))
      (should (equal vector-calls (list (list source 0 3))))
      (should (equal returned [nil nil nil nil nil])))))

(ert-deftest emacs-load-artifact-compiler-test/artifact-source-native-vector-batch-validates-header ()
  (let* ((path "/tmp/emacs-load-artifact-compiler-test-module-init-native-vector-invalid.neln")
         (source "(alpha) (beta)"))
    (dolist (response
             (list
              ;; Native max is 2 here, so the exact transport length is 4.
              [(alpha) (beta)]
              [14 "2" (alpha) (beta)]
              [14 3 (alpha) (beta)]
              [0 2 (alpha) (beta)]))
      (let ((err nil))
        (setq err
              (condition-case caught
                  (progn
                    (cl-letf
                        (((symbol-value
                           'emacs-load-artifact-module-init-native-batch-items)
                          2)
                         ((symbol-function
                           'nelisp--read-batch-vector-from-string-native)
                          (lambda (&rest _args) response)))
                      (emacs-load--artifact-source-iterate-form-nil-or-list-value-native-batch
                       source path :module-init #'identity))
                    nil)
                (error caught)))
        (should err)
        (should (equal (error-message-string err)
                       (format "invalid :module-init in %s" path)))))))

(ert-deftest emacs-load-artifact-compiler-test/artifact-source-iterate-form-nil-or-list-value-native-batch-replays-items-in-order-with-gc-and-counts ()
  (let* ((path "/tmp/emacs-load-artifact-compiler-test-module-init-native-batch-gc.neln")
         (source "(alpha) (beta) (gamma) (delta)")
         (seen nil)
         (current-batch nil)
         (gc-lens nil))
    (cl-letf (((symbol-value 'emacs-load-artifact-module-init-native-batch-items) 4)
              ((symbol-value 'load-garbage-collect-interval) 2)
              ((symbol-function 'garbage-collect)
               (lambda ()
                 (push (if (consp current-batch)
                           (length (car current-batch))
                         0)
                       gc-lens)
                 'ok))
              ((symbol-function 'nelisp--read-batch-from-string-native)
               (lambda (string byte-cursor batch-size)
                 (should (= byte-cursor 0))
                 (should (= batch-size 4))
                 (should (string= string source))
                 (setq current-batch
                       (cons (list '(alpha) '(beta) '(gamma) '(delta))
                             (emacs-load--artifact-byte-length string)))
                 current-batch))
              ((symbol-function 'read-from-string)
               (lambda (&rest _args)
                 (error "host reader should not be used"))))
      (should (= (emacs-load--artifact-source-iterate-form-nil-or-list-value-native-batch
                  source path :module-init
                  (lambda (item)
                    (push item seen)))
                 4))
      (should (equal (nreverse seen) '((alpha) (beta) (gamma) (delta))))
      (should (equal (nreverse gc-lens) '(2 0))))))

(ert-deftest emacs-load-artifact-compiler-test/artifact-source-iterate-form-nil-or-list-value-native-batch-drains-batch-before-boundary ()
  (let* ((path "/tmp/emacs-load-artifact-compiler-test-module-init-native-batch-boundary.neln")
         (source "(alpha) (beta) (gamma) (delta)")
         (seen nil)
         (batch-sources nil)
         (current-batch nil)
         (gc-lens nil))
    (cl-letf (((symbol-value 'emacs-load-artifact-module-init-native-batch-items) 2)
              ((symbol-value 'load-garbage-collect-interval) 2)
              ((symbol-function 'garbage-collect)
               (lambda ()
                 (push (if (consp current-batch)
                           (length (car current-batch))
                         0)
                       gc-lens)
                 'ok))
              ((symbol-function 'nelisp--read-batch-from-string-native)
               (lambda (string byte-cursor batch-size)
                 (push string batch-sources)
                 (should (= byte-cursor 0))
                 (should (= batch-size 2))
                 (cond
                  ((string= string "(alpha) (beta)")
                   (setq current-batch
                         (cons (list '(alpha) '(beta))
                               (emacs-load--artifact-byte-length string)))
                   current-batch)
                  ((string= string "(gamma) (delta)")
                   (setq current-batch
                         (cons (list '(gamma) '(delta))
                               (emacs-load--artifact-byte-length string)))
                   current-batch)
                  (t
                   (error "unexpected batch source %S" string)))))
              ((symbol-function 'read-from-string)
               (lambda (&rest _args)
                 (error "host reader should not be used"))))
      (should (= (emacs-load--artifact-source-iterate-form-nil-or-list-value-native-batch
                  source path :module-init
                  (lambda (item)
                    (push item seen)))
                 4))
      (should (equal (nreverse seen) '((alpha) (beta) (gamma) (delta))))
      (should (equal (nreverse batch-sources)
                     '("(alpha) (beta)" "(gamma) (delta)")))
      (should (equal (nreverse gc-lens) '(0 0))))))

(ert-deftest emacs-load-artifact-compiler-test/artifact-source-iterate-form-nil-or-list-value-native-batch-validates-batch-size ()
  (let* ((path "/tmp/emacs-load-artifact-compiler-test-module-init-native-batch-invalid-size.neln")
         (source "((alpha) (beta))"))
    (dolist (batch-size '(0 -1 "4"))
      (let ((err nil)
            (native-reader-calls 0))
        (setq err
              (condition-case caught
                  (progn
                    (cl-letf (((symbol-value 'emacs-load-artifact-module-init-native-batch-items)
                               batch-size)
                              ((symbol-function 'nelisp--read-batch-from-string-native)
                               (lambda (&rest _args)
                                 (setq native-reader-calls (1+ native-reader-calls))
                                 (error "unexpected read-batch-from-string-native call"))))
                      (emacs-load--artifact-source-iterate-form-nil-or-list-value-native-batch
                       source path :module-init #'ignore))
                    nil)
                (error caught)))
        (should err)
        (should (equal (error-message-string err)
                       (format "invalid :module-init in %s" path)))
        (should (= native-reader-calls 0))))))

(ert-deftest emacs-load-artifact-compiler-test/artifact-source-iterate-form-nil-or-list-value-native-batch-allows-empty-source ()
  (let* ((path "/tmp/emacs-load-artifact-compiler-test-module-init-native-batch-empty.neln")
         (source " ; comment only\n ")
         (seen nil))
    (cl-letf (((symbol-function 'nelisp--read-batch-from-string-native)
               (lambda (&rest _args)
                 (error "native reader should not be called"))))
      (should (= (emacs-load--artifact-source-iterate-form-nil-or-list-value-native-batch
                  source path :module-init
                  (lambda (item)
                    (push item seen)))
                 0))
      (should (null seen)))))

(ert-deftest emacs-load-artifact-compiler-test/artifact-source-iterate-form-nil-or-list-value-native-batch-validates-multibyte-final-bounds ()
  (let* ((path "/tmp/emacs-load-artifact-compiler-test-module-init-native-batch-multibyte.neln")
         (source "é")
         (source-bytes (emacs-load--artifact-byte-length source))
         (source-chars (length source))
         (seen nil)
         (err nil))
    (should (and (> source-bytes source-chars) (= source-chars 1)))
    (setq err
          (condition-case caught
              (progn
                (cl-letf (((symbol-function 'nelisp--read-batch-from-string-native)
                           (lambda (_string _byte-cursor _batch-size)
                             (cons '(é) source-chars))))
                  (emacs-load--artifact-source-iterate-form-nil-or-list-value-native-batch
                   source path :module-init #'identity))
                nil)
            (error caught)))
    (should err)
    (should (equal (error-message-string err)
                   "invalid :module-init in /tmp/emacs-load-artifact-compiler-test-module-init-native-batch-multibyte.neln"))
    (should (= source-bytes 2))
    (cl-letf (((symbol-function 'nelisp--read-batch-from-string-native)
               (lambda (_string _byte-cursor _batch-size)
                 (cons '(é) source-bytes))))
      (should (= (emacs-load--artifact-source-iterate-form-nil-or-list-value-native-batch
                  source path :module-init
                  (lambda (item)
                    (push item seen)))
                 1))
      (should (equal seen '(é))))))

(ert-deftest emacs-load-artifact-compiler-test/artifact-source-iterate-form-nil-or-list-value-native-batch-rejects-malformed-response ()
  (let* ((path "/tmp/emacs-load-artifact-compiler-test-module-init-native-batch-malformed.neln")
         (source "alpha"))
    (dolist (entry (list
                    (cons :not-cons nil)
                    (cons :improper-list '(a . b))
                    (cons :next-not-int (cons '(alpha) "3"))
                    (cons :next-oob-high (cons '(alpha) 9999))
                    (cons :next-less-than-cursor (cons '(alpha) 0))
                    (cons :empty-batch
                          (cons nil
                                (emacs-load--artifact-byte-length source)))))
      (let ((got nil)
            (response (cdr entry)))
        (setq got
              (condition-case caught
                  (progn
                    (cl-letf (((symbol-function 'nelisp--read-batch-from-string-native)
                               (lambda (_string _byte-cursor _batch-size)
                                 response)))
                      (emacs-load--artifact-source-iterate-form-nil-or-list-value-native-batch
                       source path :module-init #'identity)
                      nil)
                    )
                  (error caught)))
        (should got)
        (should (string-match-p (format "^invalid :module-init in %s$" path)
                               (error-message-string got)))))))

(ert-deftest emacs-load-artifact-compiler-test/artifact-source-iterate-form-nil-or-list-value-native-batch-records-compact-diagnostic ()
  (let* ((path "/tmp/emacs-load-artifact-compiler-test-module-init-native-batch-diagnostic.neln")
         (source "(alpha) (beta)")
         (source-bytes (emacs-load--artifact-byte-length source))
         (err nil))
    (cl-letf (((symbol-value 'emacs-load-artifact-module-init-native-batch-items) 2)
              ((symbol-function 'nelisp--read-batch-from-string-native)
               (lambda (_string _byte-cursor _batch-size)
                 (cons '((alpha)) source-bytes))))
      (setq err
            (condition-case caught
                (progn
                  (emacs-load--artifact-source-iterate-form-nil-or-list-value-native-batch
                   source path :relocs #'identity)
                  nil)
              (error caught))))
    (should err)
    (should (equal (error-message-string err)
                   (format "invalid :relocs in %s" path)))
    (should
     (equal emacs-load--artifact-native-batch-diagnostic
            (list :stage :item-count-mismatch
                  :path path
                  :keyword :relocs
                  :slice-start 0
                  :slice-end (length source)
                  :slice-chars (length source)
                  :slice-bytes source-bytes
                  :expected-items 2
                  :actual-items 1
                  :next source-bytes)))
    (should-not
     (plist-member emacs-load--artifact-native-batch-diagnostic :slice-text))
    (let ((emacs-load-artifact-native-batch-capture-failed-slice t))
      (cl-letf (((symbol-value 'emacs-load-artifact-module-init-native-batch-items) 2)
                ((symbol-function 'nelisp--read-batch-from-string-native)
                 (lambda (_string _byte-cursor _batch-size)
                   (cons '((alpha)) source-bytes))))
        (should-error
         (emacs-load--artifact-source-iterate-form-nil-or-list-value-native-batch
          source path :relocs #'identity)
         :type 'error))
      (should
       (equal (plist-get emacs-load--artifact-native-batch-diagnostic
                         :slice-text)
              source)))
    (cl-letf (((symbol-value 'emacs-load-artifact-module-init-native-batch-items) 2)
              ((symbol-function 'nelisp--read-batch-from-string-native)
               (lambda (_string _byte-cursor _batch-size)
                 (cons '((alpha) (beta)) source-bytes))))
      (should
       (= (emacs-load--artifact-source-iterate-form-nil-or-list-value-native-batch
           source path :relocs #'identity)
          2)))
    (should (null emacs-load--artifact-native-batch-diagnostic))))

(ert-deftest emacs-load-artifact-compiler-test/artifact-replay-payload-streaming-module-init-uses-native-batch-replay ()
  (let* ((path "/tmp/emacs-load-artifact-compiler-test-module-init-native-batch-streaming.neln")
         (source "(:module-init ((alpha) (beta) (gamma)))")
         (content (concat ";;; nelisp-private-nelc-v2\n" source))
         (summary nil)
         (batch-calls nil)
         (seen nil))
    (cl-letf (((symbol-function 'nelisp--read-batch-from-string-native)
               (lambda (string byte-cursor batch-size)
                 (push (cons byte-cursor batch-size) batch-calls)
                 (should (= batch-size emacs-load-artifact-module-init-native-batch-items))
                 (cond
                  ((= byte-cursor 0)
                   (should (string= string "(alpha) (beta) (gamma)"))
                   (cons '((alpha) (beta) (gamma))
                         (emacs-load--artifact-byte-length string)))
                  (t
                   (error "unexpected byte cursor %S" byte-cursor)))))
              ((symbol-function 'nelisp--read-all-from-string-native)
               (lambda (&rest _args)
                 (error "per-item native read-all should not be used")))
              ((symbol-function 'emacs-load--artifact-replay-item)
               (lambda (item &rest _args)
                 (push item seen))))
      (setq summary
            (emacs-load--artifact-replay-payload-streaming
             content path (length ";;; nelisp-private-nelc-v2\n")))
      (should (equal (nreverse seen) '((alpha) (beta) (gamma))))
      (should (= (plist-get summary :module-init-count) 3))
      (should (equal (plist-get summary :fields) '(:module-init)))
      (should (= (length batch-calls) 1)))))

(ert-deftest emacs-load-artifact-compiler-test/artifact-replay-payload-streaming-native-preflight-ranges-maps-section ()
  (let* ((path "/tmp/emacs-load-artifact-compiler-test-native-streaming.neln")
         (native-section '(:arch "x86_64"
                                  :text-base64 "QUJD"
                                  :relocs nil
                                  :extern-symbols nil
                                  :defuns nil
                                  :object-base64 "heavy-native"
                                  :compile-report (:status ok)))
         (source (concat "(:native " (prin1-to-string native-section)
                         " :module-init ((alpha) (beta)))"))
         (content (concat ";;; nelisp-private-nelc-v2\n" source))
         (ranges '((:arch . (1 . 2))
                  (:text-base64 . (3 . 4))
                  (:relocs . (5 . 6))
                  (:extern-symbols . (7 . 8))
                  (:defuns . (9 . 10))))
         (native-marker (string-match ":native " content))
         (native-value-start (emacs-load--artifact-source-skip-ws-comments
                             content (+ native-marker (length ":native "))))
         (native-value-end (emacs-load--artifact-source-form-end
                            content native-value-start))
         (expected-compact (list :arch "x86_64"
                                 :defuns nil
                                 :path path
                                 :text-hash (emacs-load--sha256 "QUJD")))
         (source-read-call nil)
         (preflight-call nil)
         (from-ranges-call nil)
         (map-call nil)
         (call-order nil)
         (replayed nil))
    (cl-letf (((symbol-function 'emacs-load--artifact-source-read-form-ranges)
               (lambda (read-source start end call-path)
                 (push :source-read call-order)
                 (setq source-read-call
                       (list :source read-source
                             :start start
                             :end end
                             :path call-path))
                 ranges))
              ((symbol-function 'emacs-load--artifact-native-section-preflight-from-ranges)
               (lambda (call-source call-ranges call-path)
                 (push :preflight call-order)
                 (setq preflight-call
                       (list :source call-source
                             :ranges call-ranges
                             :path call-path))
                 t))
              ((symbol-function 'emacs-load--artifact-native-section-from-ranges)
               (lambda (call-source call-ranges call-path)
                 (push :from-ranges call-order)
                 (setq from-ranges-call
                       (list :source call-source
                             :ranges call-ranges
                             :path call-path))
                 native-section))
              ((symbol-function 'emacs-load--artifact-native-eligible-p)
               (lambda (call-native) t))
              ((symbol-function 'emacs-load--artifact-native-map-section)
               (lambda (call-native call-path)
                 (push :map call-order)
                 (setq map-call (list :path call-path :native call-native))
                 9012))
              ((symbol-function 'nelisp--read-batch-from-string-native)
               (lambda (string _byte-cursor _batch-size)
                 (cons '((alpha) (beta))
                       (emacs-load--artifact-byte-length string))))
              ((symbol-function 'read-from-string)
               (lambda (&rest _args)
                 (error "host read-from-string should not be used")))
              ((symbol-function 'emacs-load--artifact-replay-item-with-native-sections)
               (lambda (item _path section-bases &optional _defuns-index)
                 (push :replay call-order)
                 (push (list item section-bases) replayed)
                 item)))
      (let ((summary (emacs-load--artifact-replay-payload-streaming
                      content path (length ";;; nelisp-private-nelc-v2\n"))))
        (should (equal (plist-get summary :native-mode) :native))
        (should (equal (plist-get summary :module-init-count) 2))
        (should (equal (plist-get summary :fields) '(:native :module-init)))
        (should source-read-call)
        (should (= (plist-get source-read-call :start) native-value-start))
        (should (= (plist-get source-read-call :end) native-value-end))
        (should (equal (plist-get source-read-call :path) path))
        (should (equal preflight-call
                       (list :source content
                             :ranges ranges
                             :path path)))
        (should (equal from-ranges-call
                       (list :source content
                             :ranges ranges
                             :path path)))
        (should (equal (plist-get map-call :path) path))
        (should (equal (plist-get map-call :native) native-section))
        (should (equal (nreverse call-order)
                       '(:source-read :preflight :from-ranges :map :replay :replay)))
        (let ((ordered (nreverse replayed)))
          (should (equal (mapcar #'car ordered) '((alpha) (beta))))
          (should (= (length ordered) 2))
          (should (equal (cadr (car ordered))
                         (list (cons expected-compact 9012))))
          (should (equal (cadr (cadr ordered))
                         (list (cons expected-compact 9012)))))))))

(ert-deftest emacs-load-artifact-compiler-test/artifact-replay-payload-streaming-module-init-falls-back-without-native-batch ()
  (let* ((path "/tmp/emacs-load-artifact-compiler-test-module-init-native-batch-streaming-fallback.neln")
         (source "(:module-init ((alpha) (beta)))")
         (content (concat ";;; nelisp-private-nelc-v2\n" source))
         (summary nil)
         (batch-fn nil)
         (native-read-all-calls nil)
         (seen nil))
    (when (fboundp 'nelisp--read-batch-from-string-native)
      (setq batch-fn (symbol-function 'nelisp--read-batch-from-string-native))
      (fmakunbound 'nelisp--read-batch-from-string-native))
    (unwind-protect
        (cl-letf (((symbol-function 'nelisp--read-all-from-string-native)
                   (lambda (string)
                     (push string native-read-all-calls)
          (cond
                     ((string= string "(alpha)") '((alpha)))
                     ((string= string "(beta)") '((beta)))
                     (t
                      (error "unexpected read-all input %S" string)))))
                  ((symbol-function 'read-from-string)
                   (lambda (&rest _args)
                     (error "host reader should not be used")))
                  ((symbol-function 'emacs-load--artifact-replay-item)
                   (lambda (item &rest _args)
                     (push item seen))))
          (setq summary
                (emacs-load--artifact-replay-payload-streaming
                 content path (length ";;; nelisp-private-nelc-v2\n")))
          (should (equal (plist-get summary :module-init-count) 2))
          (should (equal (length native-read-all-calls) 2))
          (should (equal (nreverse native-read-all-calls) '("(alpha)" "(beta)")))
      (should (equal (nreverse seen) '((alpha) (beta))))
      (when batch-fn
        (fset 'nelisp--read-batch-from-string-native batch-fn))))))

(ert-deftest emacs-load-artifact-compiler-test/artifact-source-container-end-uses-native-helper-for-exact-index ()
  (let ((source "(abc)")
        (native-calls nil))
    (cl-letf (((symbol-function 'nelisp--source-container-end)
               (lambda (string pos)
                 (push (list string pos) native-calls)
                 5)))
      (should (= (emacs-load--artifact-source-container-end source 0) 5))
      (should (equal (nreverse native-calls) '(("(abc)" 0)))))))

(ert-deftest emacs-load-artifact-compiler-test/artifact-source-container-end-native-invalid-results-signal-unterminated-source-container ()
  (let ((source "(abc)")
        (err nil))
    (cl-letf (((symbol-function 'nelisp--source-container-end)
               (lambda (&rest _args)
                 0)))
      (setq err
            (condition-case caught
                (progn
                  (emacs-load--artifact-source-container-end source 0)
                  nil)
              (error caught))))
    (should err)
    (should (equal (error-message-string err) "unterminated source container"))))

(ert-deftest emacs-load-artifact-compiler-test/artifact-source-container-end-fallback-parses-escaped-strings-comments-and-nested-containers ()
  (let ((source "(foo [\"a\\\\\\\"b()\" ; comment with ) and ]\n (bar \"c\\\\\\\"d\" baz)])"))
    (emacs-load-artifact-compiler-test--without-native-source-container-end
     (should (= (emacs-load--artifact-source-container-end source 0)
                (length source))))))

(ert-deftest emacs-load-artifact-compiler-test/artifact-source-string-end-uses-native-helper-for-exact-index ()
  (let ((source "\"abc\"")
        (native-calls nil))
    (cl-letf (((symbol-function 'nelisp--rd-string-end-native)
               (lambda (string start len)
                 (push (list string start len) native-calls)
                 (cons 4 nil))))
      (should (= (emacs-load--artifact-source-string-end source 0) 5))
      (should (equal (nreverse native-calls) '(("\"abc\"" 1 5)))))))

(ert-deftest emacs-load-artifact-compiler-test/artifact-source-string-end-native-invalid-results-signal-unterminated-source-string ()
  (let ((source "\"abc\"")
        (err nil))
    (cl-letf (((symbol-function 'nelisp--rd-string-end-native)
               (lambda (&rest _args)
                 (cons 0 nil))))
      (setq err
            (condition-case caught
                (progn
                  (emacs-load--artifact-source-string-end source 0)
                  nil)
              (error caught))))
    (should err)
    (should (equal (error-message-string err) "unterminated source string"))))

(ert-deftest emacs-load-artifact-compiler-test/artifact-source-string-end-fallback-parses-escaped-strings ()
  (let ((source "\"a\\\"b\""))
    (emacs-load-artifact-compiler-test--without-native-rd-string-end-native
     (should (= (emacs-load--artifact-source-string-end source 0)
                (length source))))))

(ert-deftest emacs-load-artifact-compiler-test/native-sections-handler-keeps-legacy-output-unchanged ()
  (let* ((emacs-load-artifact-native-sections-native-reader-threshold 1)
        (path "/tmp/emacs-load-artifact-compiler-test-native-sections-handler.neln")
        (first-section
         "(:arch \"x86_64\" :text-base64 \"QUJD\" :relocs nil :extern-symbols nil :defuns nil)")
        (second-section
         "(:arch \"arm64\" :text-base64 \"REVG\" :relocs nil :extern-symbols nil :defuns nil)")
        (source (concat " (" first-section "\n " second-section ")")))
    (should (equal (emacs-load--artifact-native-sections-from-source
                    source 0 (length source) path)
                   '((:arch "x86_64"
                      :text-base64 "QUJD"
                      :relocs nil
                      :extern-symbols nil
                      :defuns nil)
                     (:arch "arm64"
                      :text-base64 "REVG"
                      :relocs nil
                      :extern-symbols nil
                      :defuns nil))))
    (should (equal (emacs-load--artifact-native-sections-from-source
                    source 0 (length source) path
                    (lambda (section)
                      (list :arch (plist-get section :arch)
                            :hash (emacs-load--sha256
                                   (plist-get section :text-base64)))))
                   `((:arch "x86_64"
                      :hash ,(emacs-load--sha256 "QUJD"))
                     (:arch "arm64"
                      :hash ,(emacs-load--sha256 "REVG")))))))

(ert-deftest emacs-load-artifact-compiler-test/native-sections-native-reader-p-rejects-large-ranges-without-substring ()
  (let ((emacs-load-artifact-native-sections-native-reader-threshold 4096)
        (source (make-string 4097 ?a))
        (substring-calls 0)
        (original-substring (symbol-function 'substring)))
    (cl-letf (((symbol-function 'nelisp--read-all-from-string-native)
               (lambda (&rest _args) t))
              ((symbol-function 'substring)
               (lambda (string &optional start end)
                 (setq substring-calls (1+ substring-calls))
                 (funcall original-substring string start end))))
      (should-not (emacs-load--artifact-native-sections-use-native-reader-p
                   source 0 (length source)))
      (should (= substring-calls 0)))))

(ert-deftest emacs-load-artifact-compiler-test/native-sections-native-reader-p-checks-byte-length-for-small-multibyte-ranges ()
  (let ((emacs-load-artifact-native-sections-native-reader-threshold 4)
        (source "あい")
        (substring-calls 0)
        (original-substring (symbol-function 'substring)))
    (cl-letf (((symbol-function 'nelisp--read-all-from-string-native)
               (lambda (&rest _args) t))
              ((symbol-function 'substring)
               (lambda (string &optional start end)
                 (setq substring-calls (1+ substring-calls))
                 (funcall original-substring string start end))))
      (should-not (emacs-load--artifact-native-sections-use-native-reader-p
                   source 0 (length source)))
      (should (= substring-calls 1)))))

(ert-deftest emacs-load-artifact-compiler-test/native-sections-handler-preserves-order-and-output ()
  (let* ((emacs-load-artifact-native-sections-native-reader-threshold 1)
         (source-prefix "xx")
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
         (handler-order nil)
         (handler-result nil))
    (setq handler-result
          (emacs-load--artifact-native-sections-from-source
           source start end "/tmp/emacs-load-artifact-compiler-test-handler.neln"
           (lambda (section)
             (setq handler-order
                   (append handler-order
                           (list (plist-get section :arch))))
             (list :arch (plist-get section :arch)
                   :name (plist-get (car (plist-get section :defuns)) :name)))))
    (should (equal handler-order '("x86_64" "arm64")))
    (should (equal handler-result
                   '((:arch "x86_64" :name "one")
                     (:arch "arm64" :name "two"))))))

(ert-deftest emacs-load-artifact-compiler-test/native-sections-streaming-handler-produces-compact-section-bases ()
  (let* ((emacs-load-artifact-native-sections-native-reader-threshold 1)
         (path "/tmp/emacs-load-artifact-compiler-test-streaming-compact.neln")
         (section-a '(:arch "x86_64"
                      :text-base64 "QUJD"
                      :relocs nil
                      :extern-symbols nil
                      :object-base64 "heavy-a"
                      :compile-report (:status ok)
                      :symbols (sym-a)
                      :defuns ((:name "emacs-load-artifact-compiler-test--compact-a"
                                :offset 0
                                :body-offset 1
                                :arity 0
                                :rt-slot-count 1))))
         (section-b '(:arch "arm64"
                      :text-base64 "REVG"
                      :relocs nil
                      :extern-symbols nil
                      :object-base64 "heavy-b"
                      :compile-report (:status ok)
                      :symbols (sym-b)
                      :defuns ((:name "emacs-load-artifact-compiler-test--compact-b"
                                :offset 0
                                :body-offset 2
                                :arity 1
                                :rt-slot-count 2))))
         (source (concat "("
                         (prin1-to-string section-a)
                         "\n "
                         (prin1-to-string section-b)
                         ")"))
         (base-a 1000)
         (base-b 2000)
         (compact-a (list :arch "x86_64"
                          :defuns (plist-get section-a :defuns)
                          :path path
                          :text-hash (emacs-load--sha256 "QUJD")))
         (compact-b (list :arch "arm64"
                          :defuns (plist-get section-b :defuns)
                          :path path
                          :text-hash (emacs-load--sha256 "REVG")))
         (expected-result (list (cons compact-a base-a)
                                (cons compact-b base-b)))
         (handler-result nil))
    (setq handler-result
          (emacs-load--artifact-native-sections-from-source
           source 0 (length source) path
           (lambda (section)
             (cons (emacs-load--artifact-native-section-compact section path)
                   (if (string= (plist-get section :arch) "x86_64")
                       base-a
                     base-b)))))
    (should (equal handler-result expected-result))
    (dolist (entry handler-result)
      (let ((compact (car entry)))
        (should (plist-member compact :arch))
        (should (plist-member compact :defuns))
        (should (plist-member compact :path))
        (should (plist-member compact :text-hash))
        (should-not (plist-member compact :text-base64))
        (should-not (plist-member compact :relocs))
        (should-not (plist-member compact :extern-symbols))))))

(ert-deftest emacs-load-artifact-compiler-test/native-defuns-index-stores-match-triples-with-normalized-keys ()
  (let* ((path "/tmp/emacs-load-artifact-compiler-test-native-index.neln")
         (meta-alpha '(:name "alpha" :offset 1 :body-offset 2 :arity 0 :rt-slot-count 1))
         (meta-beta '(:name "beta" :offset 3 :body-offset 4 :arity 1 :rt-slot-count 2))
         (section-a (list :arch "x86_64"
                          :defuns (list meta-alpha)))
         (section-b (list :arch "arm64"
                          :defuns (list meta-beta)))
         (section-bases (list (cons section-a 1000)
                              (cons section-b 2000)))
         (index (emacs-load--artifact-build-defuns-metadata-index section-bases path)))
    (should (equal (gethash "alpha" index)
                   (list section-a 1000 meta-alpha)))
    (should (equal (emacs-load--artifact-find-defuns-metadata-in-index 'alpha index)
                   (list section-a 1000 meta-alpha)))
    (should (equal (emacs-load--artifact-find-defuns-metadata-in-index "beta" index)
                   (list section-b 2000 meta-beta)))
    (should (equal (emacs-load--artifact-defuns-metadata-name-key 'alpha) "alpha"))
    (should (equal (emacs-load--artifact-defuns-metadata-name-key "beta") "beta"))))

(ert-deftest emacs-load-artifact-compiler-test/native-sections-duplicate-name-detection-signals-error ()
  (let* ((path "/tmp/emacs-load-artifact-compiler-test-duplicate.neln")
         (section-a '(:arch "x86_64"
                      :defuns ((:name "emacs-load-artifact-compiler-test--duplicate"
                                :offset 0
                                :body-offset 1
                                :arity 0
                                :rt-slot-count 1))))
         (section-b '(:arch "arm64"
                      :defuns ((:name "emacs-load-artifact-compiler-test--duplicate"
                                :offset 4
                                :body-offset 2
                                :arity 1
                                :rt-slot-count 2)))))
    (let ((err
           (should-error
            (emacs-load--artifact-build-defuns-metadata-index
             (list (cons section-a 1000)
                   (cons section-b 2000))
             path)
            :type 'error)))
      (should (string-match-p "appears in multiple sections"
                              (error-message-string err))))))

(ert-deftest emacs-load-artifact-compiler-test/native-sections-replay-item-falls-back-to-scan-without-index ()
  (let ((scan-calls nil)
        (install-calls nil))
    (cl-letf (((symbol-function 'emacs-load--artifact-find-defuns-metadata-in-sections)
               (lambda (name section-bases path)
                 (push (list name section-bases path) scan-calls)
                 (list 'section 123 '(:name "alpha"))))
              ((symbol-function 'emacs-load--artifact-native-install-fn)
               (lambda (name section base meta)
                 (push (list name section base meta) install-calls)
                 :installed))
              ((symbol-function 'emacs-load--artifact-fn-source-defun)
               (lambda (&rest _args) nil)))
      (emacs-load--artifact-replay-item-with-native-sections
       '(:fn alpha nil)
       "/tmp/emacs-load-artifact-compiler-test-native-fallback.neln"
       '((ignored . 1)))
      (should (= (length scan-calls) 1))
      (should (equal (caar scan-calls) 'alpha))
      (should (equal install-calls
                     '((alpha section 123 (:name "alpha"))))))))

(ert-deftest emacs-load-artifact-compiler-test/native-sections-replay-item-does-not-scan-on-index-miss ()
  (let ((index (make-hash-table :test 'equal))
        (eval-calls nil))
    (cl-letf (((symbol-function 'emacs-load--artifact-find-defuns-metadata-in-sections)
               (lambda (&rest _args)
                 (error "linear scan should not run when index is supplied")))
              ((symbol-function 'emacs-load--artifact-bcl-replay-available-p)
               (lambda () nil))
              ((symbol-function 'emacs-load--artifact-fn-source-defun)
               (lambda (_item _path)
                 '(defun emacs-load-artifact-compiler-test--index-miss-fallback ()
                    :source-installed)))
              ((symbol-function 'emacs-load--artifact-eval-form)
               (lambda (form)
                 (push form eval-calls)
                 (eval form))))
      (unwind-protect
          (progn
            (should
             (eq (emacs-load--artifact-replay-item-with-native-sections
                  '(:fn emacs-load-artifact-compiler-test--index-miss-fallback nil)
                  "/tmp/emacs-load-artifact-compiler-test-native-index-miss.neln"
                  '((ignored . 1))
                  index)
                 'emacs-load-artifact-compiler-test--index-miss-fallback))
            (should (= (length eval-calls) 1))
            (should (eq (funcall 'emacs-load-artifact-compiler-test--index-miss-fallback)
                        :source-installed)))
        (when (fboundp 'emacs-load-artifact-compiler-test--index-miss-fallback)
          (fmakunbound 'emacs-load-artifact-compiler-test--index-miss-fallback))))))

(ert-deftest emacs-load-artifact-compiler-test/artifact-replay-payload-small-builds-native-index-once-and-reuses-it ()
  (let* ((path "/tmp/emacs-load-artifact-compiler-test-small-index.neln")
         (payload '(:native-sections ignored
                    :module-init ((:fn alpha nil) (:fn beta nil))))
         (section-bases '((section-a . 1000) (section-b . 2000)))
         (shared-index (make-hash-table :test 'equal))
         (build-count 0)
         (replayed nil))
    (cl-letf (((symbol-function 'emacs-load--artifact-native-sections-from-payload)
               (lambda (_payload _path) '(native-sections)))
              ((symbol-function 'emacs-load--artifact-native-section-bases)
               (lambda (_sections _path) section-bases))
              ((symbol-function 'emacs-load--artifact-build-defuns-metadata-index)
               (lambda (bases call-path)
                 (setq build-count (1+ build-count))
                 (should (eq bases section-bases))
                 (should (equal call-path path))
                 shared-index))
              ((symbol-function 'emacs-load--artifact-replay-item-with-native-sections)
               (lambda (item call-path bases index)
                 (push (list item call-path bases index) replayed)
                 item))
              ((symbol-function 'emacs-load--artifact-native-eligible-p)
               (lambda (_section) nil)))
      (let ((emacs-load--artifact-native-hash-cache (make-hash-table :test 'equal)))
        (emacs-load--artifact-replay-payload-small payload path)
        (should (= build-count 1))
        (should (= (hash-table-count emacs-load--artifact-native-hash-cache) 0))
        (let ((ordered (nreverse replayed)))
          (should (equal (mapcar #'car ordered)
                         '((:fn alpha nil) (:fn beta nil))))
          (should (eq (nth 3 (car ordered)) shared-index))
          (should (eq (nth 3 (cadr ordered)) shared-index))
          (should (eq (nth 2 (car ordered)) section-bases))
          (should (eq (nth 2 (cadr ordered)) section-bases)))))))

(ert-deftest emacs-load-artifact-compiler-test/artifact-replay-payload-streaming-builds-native-index-once-and-reuses-it ()
  (let* ((path "/tmp/emacs-load-artifact-compiler-test-streaming-index.neln")
         (content
          ";;; nelisp-private-nelc-v2\n(:native-sections nil :module-init ((:fn alpha nil) (:fn beta nil)))")
         (section-bases '((section-a . 111) (section-b . 222)))
         (shared-index (make-hash-table :test 'equal))
         (build-count 0)
         (replayed nil))
    (cl-letf (((symbol-function 'emacs-load--artifact-native-sections-from-source)
               (lambda (_source _start _end call-path _handler)
                 (should (equal call-path path))
                 section-bases))
              ((symbol-function 'emacs-load--artifact-build-defuns-metadata-index)
               (lambda (bases call-path)
                 (setq build-count (1+ build-count))
                 (should (eq bases section-bases))
                 (should (equal call-path path))
                 shared-index))
              ((symbol-function 'emacs-load--artifact-replay-item-with-native-sections)
               (lambda (item call-path bases index)
                 (push (list item call-path bases index) replayed)
                 item)))
      (let ((emacs-load--artifact-native-hash-cache (make-hash-table :test 'equal)))
        (emacs-load--artifact-replay-payload-streaming
         content path (length ";;; nelisp-private-nelc-v2\n"))
        (should (= build-count 1))
        (should (= (hash-table-count emacs-load--artifact-native-hash-cache) 0))
        (let ((ordered (nreverse replayed)))
          (should (equal (mapcar #'car ordered)
                         '((:fn alpha nil) (:fn beta nil))))
          (should (eq (nth 3 (car ordered)) shared-index))
          (should (eq (nth 3 (cadr ordered)) shared-index))
          (should (eq (nth 2 (car ordered)) section-bases))
          (should (eq (nth 2 (cadr ordered)) section-bases)))))))

(ert-deftest emacs-load-artifact-compiler-test/native-install-diagnostic-uses-compact-hash-and-path ()
  (let* ((name 'emacs-load-artifact-compiler-test--compact-native-installed)
         (compact-native '(:arch "x86_64"
                           :defuns ((:name "emacs-load-artifact-compiler-test--compact-native-installed"
                                     :offset 7
                                     :body-offset 5
                                     :arity 2
                                     :rt-slot-count 3))
                           :path "/tmp/emacs-load-artifact-compiler-test-native-compact.neln"
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
                         "/tmp/emacs-load-artifact-compiler-test-native-compact.neln"))
          (should (eq (funcall name 'alpha 'beta) :invoked))
          (should (equal (car calls) '(212 2 3 (alpha beta)))))
      (when (fboundp name)
        (fmakunbound name)))))

(provide 'emacs-load-artifact-compiler-test)
;;; emacs-load-artifact-compiler-test.el ends here
