;;; nelisp-native-runtime-reload-test.el --- raw runtime unit contracts -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; These tests cover the host-side half of Doc 191's raw runtime lane.  The
;; in-process execution proof belongs to the standalone reader: host Emacs can
;; compile and inspect a raw unit but has no `ptr-call' or runtime BSS to
;; publish into.  Keeping that distinction explicit prevents a metadata PASS
;; from being reported as a live allocator/GC replacement.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-aot-compiler)
(require 'nelisp-artifact)
(load (expand-file-name "lisp/nelisp-native-load.el") nil t)

(defconst nelisp-native-runtime-reload-test-root
  (expand-file-name ".."
                    (file-name-directory
                     (or load-file-name buffer-file-name))))

(let ((build-script (expand-file-name
                     "../scripts/nelisp-runtime-reload-build.el"
                     (file-name-directory
                      (or load-file-name buffer-file-name)))))
  (add-to-list 'load-path (file-name-directory build-script))
  (when (file-readable-p build-script)
    (load build-script nil t)))

(defun nelisp-native-runtime-reload-test--manifest
    (&optional format kind runtime-abi layout-id data-size bss-size)
  "Return a minimal valid raw manifest, with selected fields overridden."
  (let* ((text (encode-coding-string "\x55\x48\x89\xe5\x31\xc0\x5d\xc3"
                                    'no-conversion t))
         (native
          (list :raw-abi nelisp-native-load-raw-runtime-abi
                :object-format nelisp-native-load-raw-object-format
                :text-size (string-bytes text)
                :text-base64 (base64-encode-string text t)
                :object-sha256 (secure-hash 'sha256 text)
                :object-size (string-bytes text)
                :exports (list (list :name "probe" :value 0 :size 8
                                     :type 'func
                                     :abi nelisp-native-load-raw-runtime-abi
                                     :arity 0 :return 'u64))
                :imports nil
                :relocs nil
                :data-size (or data-size 0)
                :bss-size (or bss-size 0))))
    (let* ((base (list :format (or format nelisp-native-load-raw-artifact-format)
                       :kind (or kind 'raw-runtime)
                       :runtime-abi (or runtime-abi
                                        nelisp-native-load-raw-runtime-abi)
                       :runtime-opt-in t
                       :layout-id (or layout-id nelisp-native-load-raw-layout-id)
                       :binary-sha256
                       "0000000000000000000000000000000000000000000000000000000000000000"
                       :arch "x86_64"
                       :native native)))
      (append base
              (list :artifact-sha256
                    (secure-hash 'sha256 (prin1-to-string base)))))))

(ert-deftest nelisp-native-runtime-reload/raw-manifest-contract-is-loadable ()
  "The raw format is distinct and validates before any mmap call."
  (should-not
   (nelisp-native-load-raw-check
    (nelisp-native-runtime-reload-test--manifest) "probe")))

(ert-deftest nelisp-native-runtime-reload/raw-imports-use-private-runtime-namespace ()
  "Raw units cannot resolve an object-mode or arbitrary runtime symbol."
  (let* ((manifest (nelisp-native-runtime-reload-test--manifest))
         (native (copy-tree (plist-get manifest :native)))
         (native (plist-put native :imports
                            (list (list :name "nl_seq2" :kind 'func
                                        :abi nelisp-native-load-raw-runtime-abi))))
         (manifest (plist-put (copy-sequence manifest) :native native)))
    (should (assq :raw-import-not-runtime
                  (nelisp-native-load-raw-check manifest "probe")))))

(ert-deftest nelisp-native-runtime-reload/rejects-object-mode-artifact ()
  "An ordinary `.neln' must never enter the raw i64 loader."
  (let ((manifest
         (nelisp-native-runtime-reload-test--manifest
          nelisp-native-load-artifact-format 'neln)))
    (should (assq :raw-kind
                  (nelisp-native-load-raw-check manifest "probe")))
    (should (assq :raw-format
                  (nelisp-native-load-raw-check manifest "probe")))))

(ert-deftest nelisp-native-runtime-reload/rejects-private-data-and-bss ()
  "A raw candidate cannot carry a private copy of live runtime state."
  (let ((data (nelisp-native-load-raw-check
               (nelisp-native-runtime-reload-test--manifest nil nil nil nil 1)
               "probe"))
        (bss (nelisp-native-load-raw-check
              (nelisp-native-runtime-reload-test--manifest nil nil nil nil nil 8)
              "probe")))
    (should (assq :raw-data-section-unsupported data))
    (should (assq :raw-bss-section-unsupported bss))))

(ert-deftest nelisp-native-runtime-reload/rejects-mismatched-entry-abi ()
  "Every import/export descriptor must declare the raw calling convention."
  (let* ((manifest (nelisp-native-runtime-reload-test--manifest))
         (native (copy-tree (plist-get manifest :native)))
         (export (copy-tree (car (plist-get native :exports)))))
    (setq export (plist-put export :abi "wrong-runtime-abi"))
    (setq native (plist-put native :exports (list export)))
    (setq manifest (plist-put manifest :native native))
    (should (assq :raw-export-abi
                  (nelisp-native-load-raw-check manifest "probe")))))

(ert-deftest nelisp-native-runtime-reload/install-rejects-active-call-without-publish ()
  "The host contract reports an active-call refusal before ptr-call."
  (let ((calls 0)
        (ptr-calls 0))
    (cl-letf (((symbol-function 'nelisp-native-load-raw-state)
               (lambda ()
                 (list :alloc 101 :gc 202 :generation 3
                       :active-alloc 1 :active-gc 0)))
              ((symbol-function 'nelisp-native-load--raw-symbol-addr)
               (lambda (_name) 9000))
              ((symbol-function 'nelisp-native-load-raw-export-address)
               (lambda (_handle _name) (setq calls (1+ calls)) 7000))
              ((symbol-function 'ptr-call)
               (lambda (&rest _args) (setq ptr-calls (1+ ptr-calls)) 0)))
      (let ((result
             (nelisp-native-load-raw-install
              '(:entry-name "alloc") '(:entry-name "gc"))))
        (should (eq (plist-get result :status) 'rejected))
        (should (eq (plist-get result :phase) :guard))
        (should (eq (plist-get result :reason) :active-call))
        (should (= (plist-get result :generation) 3))
        (should (= calls 2))
        (should (= ptr-calls 0))))))

(ert-deftest nelisp-native-runtime-reload/install-publishes-one-generation ()
  "A successful host-side install result records both entries and generation."
  (let ((state-calls 0)
        (published-args nil))
    (cl-letf (((symbol-function 'nelisp-native-load-raw-state)
               (lambda ()
                 (setq state-calls (1+ state-calls))
                 (if (= state-calls 1)
                     (list :alloc 101 :gc 202 :generation 3
                           :active-alloc 0 :active-gc 0)
                   (list :alloc 7000 :gc 8000 :generation 4
                         :active-alloc 0 :active-gc 0))))
              ((symbol-function 'nelisp-native-load--raw-symbol-addr)
               (lambda (_name) 9000))
              ((symbol-function 'nelisp-native-load-raw-export-address)
               (lambda (handle _name)
                 (if (equal (plist-get handle :entry-name) "alloc")
                     7000
                   8000)))
              ((symbol-function 'ptr-call)
               (lambda (&rest args)
                 (setq published-args args)
                 0)))
      (let ((result
             (nelisp-native-load-raw-install
              '(:entry-name "alloc" :object-sha256 "a")
              '(:entry-name "gc" :object-sha256 "g"))))
        (should (eq (plist-get result :status) 'published))
        (should (eq (plist-get result :phase) :publish))
        (should (= (plist-get result :generation) 4))
        (should (equal published-args '(9000 7000 8000 4 0 0 0)))
        (should (= state-calls 2))))))

(ert-deftest nelisp-native-runtime-reload/restore-publishes-zero-pointer-pair ()
  "Original runtime entries are restored through the installer's zero pair."
  (let ((state-calls 0)
        (published-args nil))
    (cl-letf (((symbol-function 'nelisp-native-load-raw-state)
               (lambda ()
                 (setq state-calls (1+ state-calls))
                 (if (= state-calls 1)
                     (list :alloc 101 :gc 202 :generation 3
                           :active-alloc 0 :active-gc 0)
                   (list :alloc 0 :gc 0 :generation 4
                         :active-alloc 0 :active-gc 0))))
              ((symbol-function 'nelisp-native-load--raw-symbol-addr)
               (lambda (_name) 9000))
              ((symbol-function 'ptr-call)
               (lambda (&rest args)
                 (setq published-args args)
                 0)))
      (let ((result (nelisp-runtime-reload-restore-originals)))
        (should (eq (plist-get result :status) 'published))
        (should (eq (plist-get result :phase) :restore))
        (should (eq (plist-get result :mode) 'original))
        (should (= (plist-get result :generation) 4))
        (should (equal published-args '(9000 0 0 4 0 0 0)))
        (should (= state-calls 2))))))

(ert-deftest nelisp-native-runtime-reload/status-is-structured-when-unavailable ()
  "A non-opt-in reader reports capability absence without signalling."
  (cl-letf (((symbol-function 'nelisp-native-load-raw-state)
             (lambda () (error "named runtime resolver is unavailable"))))
    (let ((status (nelisp-runtime-reload-status)))
      (should (eq (plist-get status :status) 'unavailable))
      (should (eq (plist-get status :phase) :state))
      (should (string-match-p "runtime resolver is unavailable"
                             (plist-get status :reason))))))

(ert-deftest nelisp-native-runtime-reload/v2-production-source-compiles-and-checks ()
  "The complete exported allocator/GC source forms a checked v2 unit."
  (skip-unless (fboundp 'nelisp-runtime-reload-production-source))
  (let* ((source (expand-file-name "target/ai/runtime-production.el"
                                  nelisp-native-runtime-reload-test-root))
         (artifact (make-temp-file "nelisp-native-runtime-reload-v2-" nil ".nelr"))
         (binary (make-string 64 ?0)))
    (unwind-protect
        (progn
          (should (file-readable-p source))
          (nelisp-native-load-raw-v2-compile-file source artifact "ert" binary)
          (let* ((manifest (nelisp-native-load-manifest artifact))
                 (native (plist-get manifest :native))
                 (problems (nelisp-native-load-raw-v2-check manifest)))
            (should-not problems)
            (should (= (length (plist-get manifest :gc-entries)) 24))
            (should (= (plist-get (car (last (plist-get manifest :gc-entries)))
                                  :index)
                       23))
            (should (= (plist-get (car (last (plist-get manifest :gc-entries)))
                                  :arity)
                       7))
            (should (cl-some (lambda (entry)
                               (and (eq (plist-get entry :kind) 'data)
                                    (plist-get entry :index)))
                             (plist-get native :imports)))))
      (when (file-exists-p artifact)
        (delete-file artifact)))))

(ert-deftest nelisp-native-runtime-reload/v2-raw-call-allows-seven-args ()
  "The v2 raw ABI supports the seven argument GC entry points."
  (cl-letf (((symbol-function 'ptr-call)
             (lambda (&rest args)
               (should (= (length args) 8))
               42)))
    (should (= (nelisp-native-load-raw-call
                '(:entry-name "nl_gc_collect"
                  :runtime-abi "nelisp-runtime-raw-v2" :arity 7 :entry 100)
                '(1 2 3 4 5 6 7))
               42))))

(ert-deftest nelisp-native-runtime-reload/raw-symbol-adapter-uses-numeric-contract ()
  "Name-based raw loading translates to the reader's four numeric slots."
  (let (calls)
    (cl-letf (((symbol-function 'nelisp--native-runtime-symbol-addr)
               (lambda (index)
                 (setq calls (cons index calls))
                 (+ 10000 index))))
      (should (= (nelisp-native-load--raw-symbol-addr
                  "nl_runtime_reload_state") 10000))
      (should (= (nelisp-native-load--raw-symbol-addr
                  "nl_runtime_reload_install") 10001))
      (should (= (nelisp-native-load--raw-symbol-addr
                  "nl_runtime_reload_alloc_original") 10002))
      (should (= (nelisp-native-load--raw-symbol-addr
                  "nl_runtime_reload_gc_original") 10003))
      (should (equal (nreverse calls) '(0 1 2 3)))
      (should-error
       (nelisp-native-load--raw-symbol-addr "unexported-runtime-symbol")))))

(ert-deftest nelisp-native-runtime-reload/source-api-keeps-entry-selection-explicit ()
  "Source reload names both replacement entries and reports map refusal."
  (let ((dir (make-temp-file "nelisp-native-runtime-reload-source-" t)))
    (unwind-protect
        (let ((source (expand-file-name "candidate.el" dir)))
          (with-temp-file source
            (insert "(defun candidate-alloc (size align) (+ size align))\n"
                    "(defun candidate-gc (mode) mode)\n"))
          ;; Host Emacs can stage and validate the candidate but has no
          ;; in-process `ptr-call' runtime, so the expected boundary is the
          ;; mapping phase.  A standalone opt-in reader continues past it.
          (let ((result
                 (nelisp-runtime-reload-source-file
                  source "candidate-alloc" "candidate-gc" "source-test")))
            (should (eq (plist-get result :status) 'rejected))
            (should (eq (plist-get result :phase) :load-alloc))
            (should (equal (plist-get result :attempted) nil))
            (should (stringp (plist-get result :artifact)))
            (should (stringp (plist-get result :source-sha256)))
            (should (file-exists-p (plist-get result :artifact)))))
      (delete-directory dir t))))

(ert-deftest nelisp-native-runtime-reload/rejects-missing-opt-in-and-tampered-hash ()
  "A raw artifact must opt in and its mapped text must be intact."
  (let* ((manifest (nelisp-native-runtime-reload-test--manifest))
         (no-opt-in (copy-sequence manifest))
         (native (copy-tree (plist-get manifest :native)))
         (bad (copy-sequence manifest)))
    (setq no-opt-in (nelisp-native-load--raw-plist-without
                     no-opt-in :runtime-opt-in))
    (should (assq :raw-runtime-opt-in
                  (nelisp-native-load-raw-check no-opt-in "probe")))
    (setq native (plist-put native :text-base64
                            (base64-encode-string "corrupt" t)))
    (setq bad (plist-put bad :native native))
    (should (assq :raw-object-hash-mismatch
                  (nelisp-native-load-raw-check bad "probe")))
    (should (assq :raw-artifact-hash-mismatch
                  (nelisp-native-load-raw-check bad "probe")))))

(ert-deftest nelisp-native-runtime-reload/runtime-map-requires-explicit-binary-identity ()
  "An opt-in reader never maps an artifact whose consumer is unspecified."
  (let* ((manifest (nelisp-native-runtime-reload-test--manifest))
         (missing (nelisp-native-load--raw-plist-without
                   manifest :binary-sha256))
         (dir (make-temp-file "nelisp-native-runtime-reload-identity-" t)))
    (unwind-protect
        (let ((artifact (expand-file-name "unit.nelr" dir)))
          (should (assq :raw-binary-hash-missing
                        (nelisp-native-load-raw-check missing "probe")))
          (with-temp-file artifact
            (insert ";;; nelisp-private-nelr-v1\n")
            (prin1 (nelisp-native-runtime-reload-test--manifest)
                   (current-buffer))
            (insert "\n"))
          (cl-letf (((symbol-function 'nelisp--native-runtime-symbol-addr)
                     (lambda (_index) 10000))
                    ((symbol-function 'nelisp-native-load--running-binary-sha256)
                     (lambda () (make-string 64 ?b))))
            (should-error
             (nelisp-native-load-raw-artifact artifact "probe")
             :type 'error)
            (should-error
             (nelisp-native-load-raw-artifact
              artifact "probe" (make-string 64 ?0))
             :type 'error)))
      (delete-directory dir t))))

(ert-deftest nelisp-native-runtime-reload/install-requires-original-bindings-and-arities ()
  "Runtime installs keep the original allocator/GC entry bindings and arities."
  (let ((hash (make-string 64 ?a))
        (state (list :alloc 101 :gc 202 :generation 3
                     :active-alloc 0 :active-gc 0)))
    (cl-letf (((symbol-function 'nelisp--native-runtime-symbol-addr)
               (lambda (_index) 10000))
              ((symbol-function 'nelisp-native-load--running-binary-sha256)
               (lambda () hash))
              ((symbol-function 'nelisp-native-load-raw-state)
               (lambda () state))
              ((symbol-function 'nelisp-native-load--raw-symbol-addr)
               (lambda (_name) 9000))
              ((symbol-function 'nelisp-native-load-raw-export-address)
               (lambda (handle _name)
                 (if (equal (plist-get handle :entry-name) "alloc")
                     7000 8000)))
              ((symbol-function 'ptr-call)
               (lambda (&rest _args) 0)))
      (let ((unbound
             (nelisp-native-load--raw-install-identity-problem
              (list :arity 2 :binary-sha256 hash :imports nil)
              (list :arity 1 :binary-sha256 hash
                    :imports '("nl_runtime_reload_gc_original")))))
        (should (eq (car unbound) :raw-alloc-original-unbound)))
      (let ((unbound
             (nelisp-native-load--raw-install-identity-problem
              (list :arity 2 :binary-sha256 hash
                    :imports '("nl_runtime_reload_alloc_original"))
              (list :arity 1 :binary-sha256 hash :imports nil))))
        (should (eq (car unbound) :raw-gc-original-unbound)))
      (let ((bad-arity
             (nelisp-native-load--raw-install-identity-problem
              (list :arity 1 :binary-sha256 hash
                    :imports '("nl_runtime_reload_alloc_original"))
              (list :arity 1 :binary-sha256 hash
                    :imports '("nl_runtime_reload_gc_original")))))
        (should (eq (car bad-arity) :raw-alloc-arity))))))

(ert-deftest nelisp-native-runtime-reload/raw-source-compiler-emits-i64-unit ()
  "Strict source compilation emits raw exports, not hidden Sexp metadata."
  (let ((dir (make-temp-file "nelisp-native-runtime-reload-" t)))
    (unwind-protect
        (let ((source (expand-file-name "probe.el" dir))
              (artifact (expand-file-name "probe.nelr" dir)))
          (with-temp-file source
            (insert "(defun raw-probe (a b) (+ a b))\n"))
          (let* ((manifest
                  (nelisp-native-load-raw-compile-file source artifact
                                                       "test-layout" "test-build"))
                 (native (plist-get manifest :native))
                 (export (nelisp-native-load--raw-export native "raw-probe")))
            (should (file-exists-p artifact))
            (should (equal (plist-get manifest :kind) 'raw-runtime))
            (should (equal (plist-get manifest :runtime-abi)
                           nelisp-native-load-raw-runtime-abi))
            (should (equal (plist-get manifest :layout-id) "test-layout"))
            (should (stringp (plist-get manifest :binary-sha256)))
            (should (= (plist-get export :arity) 2))
            (should-not (plist-get export :param-repr))
            ;; The caller supplied a non-production layout id deliberately;
            ;; source compilation can produce metadata, but loading it into
            ;; this runtime must reject it before mapping.
            (should (assq :raw-layout-id
                          (nelisp-native-load-raw-check manifest "raw-probe")))))
      (delete-directory dir t))))

(ert-deftest nelisp-native-runtime-reload/raw-source-rejects-side-effects ()
  "Only top-level defuns are accepted for a raw runtime unit."
  (let ((dir (make-temp-file "nelisp-native-runtime-reload-" t)))
    (unwind-protect
        (let ((source (expand-file-name "bad.el" dir))
              (artifact (expand-file-name "bad.nelr" dir)))
          (with-temp-file source
            (insert "(provide 'bad)\n"))
          (should-error
           (nelisp-native-load-raw-compile-file source artifact)))
      (delete-directory dir t))))

(ert-deftest nelisp-native-runtime-reload/raw-source-rejects-nonordinary-parameters ()
  "Keyword and optional parameter markers cannot enter the raw ABI."
  (let ((dir (make-temp-file "nelisp-native-runtime-reload-params-" t)))
    (unwind-protect
        (let ((source (expand-file-name "bad.el" dir))
              (artifact (expand-file-name "bad.nelr" dir)))
          (with-temp-file source
            (insert "(defun bad (&key value) value)\n"))
          (should-error
           (nelisp-native-load-raw-compile-file source artifact)))
      (delete-directory dir t))))

(ert-deftest nelisp-native-runtime-reload/raw-source-does-not-read-eval ()
  "Staging a unit never executes read-time evaluation."
  (let ((dir (make-temp-file "nelisp-native-runtime-reload-read-eval-" t)))
    (unwind-protect
        (let ((source (expand-file-name "bad.el" dir))
              (artifact (expand-file-name "bad.nelr" dir)))
          (with-temp-file source
            (insert "#.(error \"read-time execution\")\n"))
          (should-error
           (nelisp-native-load-raw-compile-file source artifact)))
      (delete-directory dir t))))

(ert-deftest nelisp-native-runtime-reload/compile-command-stages-nelr ()
  "The CLI adapter emits a distinct raw artifact and compact status."
  (let ((dir (make-temp-file "nelisp-native-runtime-reload-cli-" t)))
    (unwind-protect
        (let ((source (expand-file-name "unit.el" dir))
              (artifact (expand-file-name "unit.nelr" dir)))
          (with-temp-file source
            (insert "(defun raw-cli (a) (+ a 1))\n"))
          (should (= 0
                     (compile-native-runtime-unit
                      (list "compile-native-runtime-unit"
                            "--input" source
                            "--output" artifact
                            "--build-id" "cli-test"))))
          (let ((manifest (nelisp-native-load-manifest artifact)))
            (should (equal (plist-get manifest :kind) 'raw-runtime))
            (should (equal (plist-get manifest :build-id) "cli-test"))
            (should-not (nelisp-native-load-raw-check manifest "raw-cli"))))
      (delete-directory dir t))))

(provide 'nelisp-native-runtime-reload-test)

;;; nelisp-native-runtime-reload-test.el ends here
