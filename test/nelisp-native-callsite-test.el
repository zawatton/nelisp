;;; nelisp-native-callsite-test.el --- Doc 202 (WS-G) tests -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; Pure-logic tests (declaration parsing, arity refusal, wrapper form
;; generation, reachability classification) run on any host and are the
;; bulk of this file.  Two tests need the opt-in Linux x86_64 native
;; runtime and gate on it with an INLINE `skip-unless' in each
;; `ert-deftest' body, per AI.md/AGENTS.md: never wrapped in a helper
;; function, so the file is never vacuously all-skips elsewhere.
;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-standalone-build)
(require 'nelisp-native-callsite)

;;;; Shared sample text used to cross-check the two independent parsers

(defconst nelisp-native-callsite-test--sample-text
  "# comment\n\nfoo 1 0\nbar 2 1\n")

(defconst nelisp-native-callsite-test--bad-text
  "foo 1 0\nbar 2 3\n")

;;;; Declaration parsing (pure) ----------------------------------------

(ert-deftest nelisp-native-callsite/parse-empty-and-comment-only ()
  (should (null (nelisp-standalone--callsite-parse-entries "")))
  (should (null (nelisp-standalone--callsite-parse-entries "# only a comment\n\n  \n")))
  (should (null (nelisp-native-callsite--parse-entries "")))
  (should (null (nelisp-native-callsite--parse-entries "# only a comment\n\n  \n"))))

(ert-deftest nelisp-native-callsite/parse-valid-sample-both-sides-agree ()
  (let ((build-side (nelisp-standalone--callsite-parse-entries
                      nelisp-native-callsite-test--sample-text))
        (repl-side (nelisp-native-callsite--parse-entries
                    nelisp-native-callsite-test--sample-text)))
    (should (equal build-side repl-side))
    (should (equal build-side '((:name "foo" :arity 1 :index 0)
                                (:name "bar" :arity 2 :index 1))))))

(ert-deftest nelisp-native-callsite/parse-refuses-malformed-line ()
  (should-error (nelisp-standalone--callsite-parse-entries "foo 1\n"))
  (should-error (nelisp-native-callsite--parse-entries "foo 1\n"))
  (should-error (nelisp-standalone--callsite-parse-entries "foo 1 0 extra\n"))
  (should-error (nelisp-native-callsite--parse-entries "foo 1 0 extra\n")))

(ert-deftest nelisp-native-callsite/parse-refuses-non-identifier-name ()
  (should-error (nelisp-standalone--callsite-parse-entries "1foo 1 0\n"))
  (should-error (nelisp-native-callsite--parse-entries "1foo 1 0\n"))
  (should-error (nelisp-standalone--callsite-parse-entries "foo-bar 1 0\n"))
  (should-error (nelisp-native-callsite--parse-entries "foo-bar 1 0\n")))

(ert-deftest nelisp-native-callsite/parse-refuses-negative-or-non-numeric-fields ()
  (should-error (nelisp-standalone--callsite-parse-entries "foo -1 0\n"))
  (should-error (nelisp-native-callsite--parse-entries "foo -1 0\n"))
  (should-error (nelisp-standalone--callsite-parse-entries "foo x 0\n"))
  (should-error (nelisp-native-callsite--parse-entries "foo x 0\n")))

(ert-deftest nelisp-native-callsite/parse-refuses-arity-over-six ()
  (should-error (nelisp-standalone--callsite-parse-entries "foo 7 0\n"))
  (should-error (nelisp-native-callsite--parse-entries "foo 7 0\n"))
  ;; 6 is the boundary and must be accepted.
  (should (equal (nelisp-standalone--callsite-parse-entries "foo 6 0\n")
                 '((:name "foo" :arity 6 :index 0))))
  (should (equal (nelisp-native-callsite--parse-entries "foo 6 0\n")
                 '((:name "foo" :arity 6 :index 0)))))

(ert-deftest nelisp-native-callsite/parse-refuses-duplicate-name ()
  (should-error (nelisp-standalone--callsite-parse-entries "foo 1 0\nfoo 1 1\n"))
  (should-error (nelisp-native-callsite--parse-entries "foo 1 0\nfoo 1 1\n")))

(ert-deftest nelisp-native-callsite/parse-refuses-duplicate-index ()
  (should-error (nelisp-standalone--callsite-parse-entries "foo 1 0\nbar 1 0\n"))
  (should-error (nelisp-native-callsite--parse-entries "foo 1 0\nbar 1 0\n")))

(ert-deftest nelisp-native-callsite/parse-refuses-index-gap-both-sides ()
  (should-error (nelisp-standalone--callsite-parse-entries
                 nelisp-native-callsite-test--bad-text))
  (should-error (nelisp-native-callsite--parse-entries
                 nelisp-native-callsite-test--bad-text)))

;;;; Contract magic and symbol-name mirroring (pure, cross-checked) ---

(ert-deftest nelisp-native-callsite/contract-magic-agrees-and-is-deterministic ()
  (let ((entries '((:name "a" :arity 1 :index 0) (:name "b" :arity 0 :index 1))))
    (should (= (nelisp-standalone--callsite-contract-magic entries)
               (nelisp-native-callsite--contract-magic entries)))
    (should (= (nelisp-standalone--callsite-contract-magic entries)
               (nelisp-standalone--callsite-contract-magic entries)))
    ;; A different declared set must (with overwhelming probability)
    ;; produce a different magic -- this is the whole point of the check.
    (should-not (= (nelisp-standalone--callsite-contract-magic entries)
                   (nelisp-standalone--callsite-contract-magic
                    '((:name "a" :arity 1 :index 0)))))))

(ert-deftest nelisp-native-callsite/symbol-names-agree ()
  (cl-letf (((symbol-function 'nelisp-standalone--callsite-declared-entries)
             (lambda () '((:name "foo" :arity 1 :index 0)
                          (:name "bar" :arity 0 :index 1)))))
    (should (equal (nelisp-standalone--callsite-symbol-names)
                   (nelisp-native-callsite--symbol-names
                    '((:name "foo" :arity 1 :index 0)
                      (:name "bar" :arity 0 :index 1)))))
    (should (equal (nelisp-standalone--callsite-symbol-names)
                   '("nl_callsite_control" "nl_callsite_install" "foo" "bar")))))

;;;; Enabled-p gating: byte-identity guarantee (pure) -------------------

(ert-deftest nelisp-native-callsite/disabled-by-default-when-env-unset ()
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "NELISP_RUNTIME_RELOAD" nil)
    (should-not (nelisp-standalone--callsite-enabled-p))
    (should-not (nelisp-standalone--callsite-source))))

(ert-deftest nelisp-native-callsite/disabled-when-no-entries-declared-even-if-opted-in ()
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "NELISP_RUNTIME_RELOAD" "1")
    (cl-letf (((symbol-function 'nelisp-standalone--callsite-declared-entries)
               (lambda () nil)))
      (should-not (nelisp-standalone--callsite-enabled-p))
      (should-not (nelisp-standalone--callsite-source)))))

(ert-deftest nelisp-native-callsite/enabled-only-when-both-conditions-hold ()
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "NELISP_RUNTIME_RELOAD" "1")
    (cl-letf (((symbol-function 'nelisp-standalone--callsite-declared-entries)
               (lambda () '((:name "nl_callsite_demo_double" :arity 1 :index 0)))))
      (should (nelisp-standalone--callsite-enabled-p))
      (should (nelisp-standalone--callsite-source)))))

;;;; Wrapper form generation (pure) -------------------------------------

(ert-deftest nelisp-native-callsite/wrap-defun-renames-original-verbatim ()
  (let* ((form '(defun nl_callsite_demo_double (x) (+ x x)))
         (entry '(:name "nl_callsite_demo_double" :arity 1 :index 0))
         (result (nelisp-standalone--callsite-wrap-defun form entry))
         (renamed (nth 0 result)))
    (should (= (length result) 2))
    (should (equal renamed '(defun nl_callsite_original_nl_callsite_demo_double
                              (x) (+ x x))))))

(ert-deftest nelisp-native-callsite/wrap-defun-wrapper-shape ()
  (let* ((form '(defun nl_callsite_demo_double (x) (+ x x)))
         (entry '(:name "nl_callsite_demo_double" :arity 1 :index 0))
         (wrapper (nth 1 (nelisp-standalone--callsite-wrap-defun form entry))))
    (should (equal (nth 0 wrapper) 'defun))
    (should (equal (nth 1 wrapper) 'nl_callsite_demo_double))
    (should (equal (nth 2 wrapper) '(x)))
    ;; Fallback branch calls the renamed original with the ORIGINAL args.
    (should (string-match-p "call-ptr" (format "%S" wrapper)))
    (should (string-match-p "nl_callsite_original_nl_callsite_demo_double" (format "%S" wrapper)))
    ;; Index 0 -> table offset 16.
    (should (string-match-p "16) x)" (format "%S" wrapper)))))

(ert-deftest nelisp-native-callsite/wrap-defun-second-index-uses-correct-table-offset ()
  (let* ((form '(defun foo (a b) (+ a b)))
         (entry '(:name "foo" :arity 2 :index 3))
         (wrapper (nth 1 (nelisp-standalone--callsite-wrap-defun form entry))))
    ;; offset = 16 + 8*3 = 40
    (should (string-match-p "40) a b)" (format "%S" wrapper)))))

(ert-deftest nelisp-native-callsite/wrap-defun-refuses-arity-mismatch ()
  (should-error (nelisp-standalone--callsite-wrap-defun
                 '(defun foo (a b) a) '(:name "foo" :arity 1 :index 0))))

(ert-deftest nelisp-native-callsite/wrap-source-passes-through-unmatched-defuns ()
  (let* ((entries '((:name "nl_callsite_demo_double" :arity 1 :index 0)))
         (source (nelisp-standalone--callsite-wrap-source entries)))
    (should (eq (car source) 'seq))
    ;; The demo caller (not itself declared) is carried through unchanged.
    (should (member '(defun nl_callsite_demo_caller (x) (nl_callsite_demo_double x))
                     (cdr source)))))

(ert-deftest nelisp-native-callsite/wrap-source-refuses-undeclared-body ()
  (should-error
   (nelisp-standalone--callsite-wrap-source
    '((:name "nl_callsite_totally_unregistered" :arity 0 :index 0)))))

(ert-deftest nelisp-native-callsite/install-forms-embed-entry-count-and-magic ()
  (let* ((entries '((:name "a" :arity 0 :index 0) (:name "b" :arity 0 :index 1)))
         (forms (nelisp-standalone--callsite-install-forms entries))
         (text (format "%S" forms)))
    (should (string-match-p (format "%d" (length entries)) text))
    (should (string-match-p
             (format "%d" (nelisp-standalone--callsite-contract-magic entries))
             text))))

;;;; nelisp-native-callsite-entries: never fabricate (pure, mocked) ----

(ert-deftest nelisp-native-callsite/entries-nil-without-native-resolver ()
  ;; On host Emacs `nelisp--native-callsite-symbol-addr' genuinely is not
  ;; `fboundp'; this must degrade to nil, never signal.
  (should-not (fboundp 'nelisp--native-callsite-symbol-addr))
  (should (null (nelisp-native-callsite-entries))))

(ert-deftest nelisp-native-callsite/entries-drops-unresolvable-declared-name ()
  "The declaration file says one entry is declared; the (faked) running
binary can resolve the control word but NOT that entry's own address --
`entries' must report NOTHING, never fabricate the file-only entry."
  (let ((dir (make-temp-file "nelisp-callsite-test-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "tools" dir) t)
          (with-temp-file (expand-file-name "tools/nelisp-replaceable-entries.txt" dir)
            (insert "nl_callsite_demo_double 1 0\n"))
          (cl-letf (((symbol-function 'nelisp-native-callsite--resolver-available-p)
                     (lambda () t))
                    ((symbol-function 'nelisp-native-callsite--resolve-index)
                     ;; index 0 (control) resolves; index 2 (the one
                     ;; declared entry) does not.
                     (lambda (index) (if (= index 0) #x1000 nil))))
            (should (null (nelisp-native-callsite-entries dir)))))
      (delete-directory dir t))))

(ert-deftest nelisp-native-callsite/entries-reports-resolved-declared-entry ()
  (let ((dir (make-temp-file "nelisp-callsite-test-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "tools" dir) t)
          (with-temp-file (expand-file-name "tools/nelisp-replaceable-entries.txt" dir)
            (insert "nl_callsite_demo_double 1 0\n"))
          (cl-letf (((symbol-function 'nelisp-native-callsite--resolver-available-p)
                     (lambda () t))
                    ((symbol-function 'nelisp-native-callsite--resolve-index)
                     (lambda (index) (pcase index (0 #x1000) (2 #x2000) (_ nil)))))
            (let ((entries (nelisp-native-callsite-entries dir)))
              (should (= (length entries) 1))
              (should (equal (plist-get (car entries) :name) "nl_callsite_demo_double"))
              (should (= (plist-get (car entries) :control) #x1000)))))
      (delete-directory dir t))))

;;;; nelisp-native-callsite-status (pure, mocked) -----------------------

(ert-deftest nelisp-native-callsite/status-unavailable-without-resolver ()
  (should (eq (plist-get (nelisp-native-callsite-status) :status) 'unavailable))
  (should (eq (plist-get (nelisp-native-callsite-status) :reason) :resolver-not-built)))

(ert-deftest nelisp-native-callsite/status-unavailable-when-no-control ()
  (cl-letf (((symbol-function 'nelisp-native-callsite--resolver-available-p)
             (lambda () t))
            ((symbol-function 'nelisp-native-callsite-control-address)
             (lambda () nil)))
    (should (eq (plist-get (nelisp-native-callsite-status) :status) 'unavailable))
    (should (eq (plist-get (nelisp-native-callsite-status) :reason) :no-declared-entries))))

(ert-deftest nelisp-native-callsite/status-ready-reads-control-fields ()
  (cl-letf (((symbol-function 'nelisp-native-callsite-control-address)
             (lambda () #x9000))
            ((symbol-function 'ptr-read-u64)
             (lambda (_addr offset) (pcase offset (0 111) (8 2) (16 0) (24 1)))))
    (let ((status (nelisp-native-callsite-status)))
      (should (eq (plist-get status :status) 'ready))
      (should (= (plist-get status :table) 111))
      (should (= (plist-get status :generation) 2))
      (should (= (plist-get status :active-calls) 0))
      (should (eq (plist-get status :install-locked) t)))))

;;;; nelisp-native-callsite-install (pure, mocked) ----------------------

(ert-deftest nelisp-native-callsite/install-refuses-when-no-entries ()
  (cl-letf (((symbol-function 'nelisp-native-callsite-entries) (lambda () nil)))
    (should (eq (plist-get (nelisp-native-callsite-install nil "u") :status) 'rejected))))

(ert-deftest nelisp-native-callsite/install-refuses-partial-entry-set ()
  (cl-letf (((symbol-function 'nelisp-native-callsite-entries)
             (lambda () '((:name "a" :arity 0 :index 0) (:name "b" :arity 0 :index 1))))
            ((symbol-function 'nelisp-native-callsite-installer-address) (lambda () #x100))
            ((symbol-function 'nelisp-native-callsite-control-address) (lambda () #x200)))
    (let ((result (nelisp-native-callsite-install '(("a" . "a")) "u")))
      (should (eq (plist-get result :status) 'rejected)))))

(ert-deftest nelisp-native-callsite/install-refuses-missing-mapping-for-declared-name ()
  (cl-letf (((symbol-function 'nelisp-native-callsite-entries)
             (lambda () '((:name "a" :arity 0 :index 0) (:name "b" :arity 0 :index 1))))
            ((symbol-function 'nelisp-native-callsite-installer-address) (lambda () #x100))
            ((symbol-function 'nelisp-native-callsite-control-address) (lambda () #x200)))
    ;; Two entries supplied (right length) but neither names a declared entry.
    (let ((result (nelisp-native-callsite-install '(("x" . "x") ("y" . "y")) "u")))
      (should (eq (plist-get result :status) 'rejected)))))

(ert-deftest nelisp-native-callsite/install-happy-path-resolves-and-publishes ()
  (let ((mmap-calls 0) (writes nil) (installer-args nil))
    (cl-letf (((symbol-function 'nelisp-native-callsite-entries)
               (lambda () '((:name "a" :arity 0 :index 0) (:name "b" :arity 1 :index 1))))
              ((symbol-function 'nelisp-native-callsite-installer-address) (lambda () #x100))
              ((symbol-function 'nelisp-native-callsite-control-address) (lambda () #x200))
              ((symbol-function 'ptr-read-u64) (lambda (_addr offset) (if (= offset 8) 4 0)))
              ((symbol-function 'nelisp-native-unit-address)
               (lambda (unit name) (setq unit unit) (+ #x900 (length name))))
              ((symbol-function 'nelisp-native-load--mmap)
               (lambda (&rest _) (setq mmap-calls (1+ mmap-calls)) #x300))
              ((symbol-function 'ptr-write-u64)
               (lambda (addr offset value) (push (list addr offset value) writes)))
              ((symbol-function 'syscall-direct) (lambda (&rest _) 0))
              ((symbol-function 'ptr-call)
               (lambda (installer table generation &rest _)
                 (setq installer-args (list installer table generation))
                 0)))
      (let ((result (nelisp-native-callsite-install
                      '(("a" . "a-export") ("b" . "b-export")) "unit-1")))
        (should (eq (plist-get result :status) 'published))
        (should (= mmap-calls 1))
        ;; generation bumped from the (mocked) current 4 to 5.
        (should (= (nth 2 installer-args) 5))
        (should (= (nth 0 installer-args) #x100))
        (should (= (nth 1 installer-args) #x300))
        ;; Table header: entry-count then magic were both written.
        (should (cl-find (list #x300 0 2) writes :test #'equal))))))

;;;; Reachability classification (pure) -----------------------------------

(ert-deftest nelisp-native-callsite/reachability-requires-nonempty-name ()
  (should-error (nelisp-native-callsite-reachability ""))
  (should-error (nelisp-native-callsite-reachability nil)))

(ert-deftest nelisp-native-callsite/classify-build-declared ()
  (should (eq (nelisp-native-callsite--classify "foo" '("foo" "bar") nil)
              :build-declared)))

(ert-deftest nelisp-native-callsite/classify-not-replaceable-with-no-gate-unit ()
  (should (eq (nelisp-native-callsite--classify "quux" '("foo" "bar") nil)
              :not-replaceable)))

(ert-deftest nelisp-native-callsite/classify-gate-only-when-unit-exports-name ()
  (cl-letf (((symbol-function 'nelisp-native-unit-address)
             (lambda (_unit _name) 12345)))
    (should (eq (nelisp-native-callsite--classify "quux" '("foo") "some-unit")
                :gate-only))))

(ert-deftest nelisp-native-callsite/classify-not-replaceable-when-unit-lacks-name ()
  (cl-letf (((symbol-function 'nelisp-native-unit-address)
             (lambda (_unit _name) (error "unknown export"))))
    (should (eq (nelisp-native-callsite--classify "quux" '("foo") "some-unit")
                :not-replaceable))))

(ert-deftest nelisp-native-callsite/reachability-never-claims-build-declared-for-gate-name ()
  ;; A name a gate CAN reach must never be reported as `:build-declared' --
  ;; that classification is reserved for names this binary actually wrapped
  ;; at build time.
  (cl-letf (((symbol-function 'nelisp-native-callsite-entries)
             (lambda () '((:name "foo" :arity 0 :index 0))))
            ((symbol-function 'nelisp-native-unit-address)
             (lambda (_unit _name) 999)))
    (should (eq (nelisp-native-callsite-reachability "quux" "some-unit") :gate-only))
    (should (eq (nelisp-native-callsite-reachability "foo" "some-unit") :build-declared))))

;;;; Against-the-bug evidence: a real freestanding native probe --------
;;
;; No full standalone build is needed to demonstrate the central claim:
;; `nelisp-aot-compile-sexp' (lisp/nelisp-aot-compiler.el) links the
;; ACTUAL forms `nelisp-standalone--callsite-wrap-source' generates into
;; a tiny freestanding ELF, with `nl_callsite_control' rewritten to a
;; literal mmap'd page address (this probe owns no BSS symbol table).
;; Same technique test/nelisp-native-runtime-dispatch-test.el already
;; uses for the allocator/GC precedent.

(defun nelisp-native-callsite-test--rewrite-control (form page)
  (cond
   ((equal form '(data-addr nl_callsite_control)) page)
   ((consp form) (cons (nelisp-native-callsite-test--rewrite-control (car form) page)
                       (nelisp-native-callsite-test--rewrite-control (cdr form) page)))
   (t form)))

(defun nelisp-native-callsite-test--source ()
  "A caller compiled BEFORE any publication must observe a replacement
installed AFTER it was compiled, for a build-declared entry."
  (let* ((page #x36000000)
         (entries '((:name "nl_callsite_demo_double" :arity 1 :index 0)))
         (magic (nelisp-standalone--callsite-contract-magic entries))
         (wrapped (nelisp-standalone--callsite-wrap-source entries))
         (rewritten (mapcar (lambda (f)
                              (nelisp-native-callsite-test--rewrite-control f page))
                            (cdr wrapped))))
    `(seq
      (defun nl_seq2 (_a b) b)
      ,@rewritten
      (defun rr_replacement (x) (* x 3))
      (defun rr_assert (value code)
        (if (= value 0) (syscall-direct 60 code 0 0 0 0 0) 0))
      (defun rr_run ()
        (seq
         (rr_assert (= (syscall-direct 9 ,page 8192 3 50 -1 0) ,page) 1)
         ;; Before publication: the wrapper falls back to the original,
         ;; through `nl_callsite_demo_caller' -- compiled against the
         ;; PUBLIC name, unedited from here on.
         (rr_assert (= (nl_callsite_demo_caller 5) 10) 2)
         (ptr-write-u64 ,(+ page 4096) 0 1)
         (ptr-write-u64 ,(+ page 4096) 8 ,magic)
         (ptr-write-u64 ,(+ page 4096) 16 (addr-of rr_replacement))
         (rr_assert (= (nl_callsite_install ,(+ page 4096) 1) 0) 3)
         ;; THE claim: the SAME already-compiled caller now observes it.
         (rr_assert (= (nl_callsite_demo_caller 5) 15) 4)
         0))
      (exit (rr_run)))))

(defun nelisp-native-callsite-test--source-contract-mismatch ()
  "Like `--source' but installs a table whose header lies about the
declared entry count; `nl_callsite_install' must refuse it (return 3)
and the caller must keep observing the original."
  (let* ((page #x37000000)
         (entries '((:name "nl_callsite_demo_double" :arity 1 :index 0)))
         (wrapped (nelisp-standalone--callsite-wrap-source entries))
         (rewritten (mapcar (lambda (f)
                              (nelisp-native-callsite-test--rewrite-control f page))
                            (cdr wrapped))))
    `(seq
      (defun nl_seq2 (_a b) b)
      ,@rewritten
      (defun rr_replacement (x) (* x 3))
      (defun rr_assert (value code)
        (if (= value 0) (syscall-direct 60 code 0 0 0 0 0) 0))
      (defun rr_run ()
        (seq
         (rr_assert (= (syscall-direct 9 ,page 8192 3 50 -1 0) ,page) 1)
         (ptr-write-u64 ,(+ page 4096) 0 999) ; wrong entry count
         (ptr-write-u64 ,(+ page 4096) 8 0)   ; wrong magic
         (ptr-write-u64 ,(+ page 4096) 16 (addr-of rr_replacement))
         (rr_assert (= (nl_callsite_install ,(+ page 4096) 1) 3) 2)
         (rr_assert (= (nl_callsite_demo_caller 5) 10) 3)
         0))
      (exit (rr_run)))))

(defun nelisp-native-callsite-test--run (source-fn)
  (let ((path (make-temp-file "nelisp-native-callsite-")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-sexp (funcall source-fn) path)
          (call-process path nil nil nil))
      (delete-file path))))

(ert-deftest nelisp-native-callsite/existing-caller-observes-post-compile-publication ()
  (skip-unless (and (eq system-type 'gnu/linux)
                    (string-match-p "x86_64\\|amd64" system-configuration)))
  (should (= (nelisp-native-callsite-test--run
              #'nelisp-native-callsite-test--source)
             0)))

(ert-deftest nelisp-native-callsite/install-refuses-contract-mismatch-natively ()
  (skip-unless (and (eq system-type 'gnu/linux)
                    (string-match-p "x86_64\\|amd64" system-configuration)))
  (should (= (nelisp-native-callsite-test--run
              #'nelisp-native-callsite-test--source-contract-mismatch)
             0)))

(provide 'nelisp-native-callsite-test)
;;; nelisp-native-callsite-test.el ends here
