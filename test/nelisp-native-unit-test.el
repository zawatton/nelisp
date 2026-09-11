;;; nelisp-native-unit-test.el --- stable native unit gate contracts -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'nelisp-native-unit)

(ert-deftest nelisp-native-unit/gate-uses-32-byte-stride-and-disp32 ()
  (let ((bytes (nelisp-native-unit--gate-bytes #x123456789abcdef0 63)))
    (should (= (length bytes) 32))
    ;; The four-byte little-endian displacement begins after the two-byte
    ;; indirect-jump opcode (offset 15); trailing bytes are gate padding.
    (should (equal (cl-subseq bytes 15 19) '(#x00 #x02 #x00 #x00)))))

(ert-deftest nelisp-native-unit/gate-address-is-unavailable-before-publish ()
  (let ((nelisp-native-unit--units nil))
    (should-error (nelisp-native-unit-address "u" "f"))))

(ert-deftest nelisp-native-unit/call-checks-arity-before-pointer-call ()
  (let ((nelisp-native-unit--units
         '(("u" :manifest (:native (:exports ((:name "f" :arity 2))))
                 :contract (("f" . 2)) :exports (("f" . 99))
                 :gates (("f" . 99))))))
    (should-error (nelisp-native-unit-call "u" "f" '(1)))))

(ert-deftest nelisp-native-unit/publisher-returns-cas-shape ()
  (let ((bytes (nelisp-native-unit--publisher-bytes)))
    (should (equal (list (nth 0 bytes) (nth 1 bytes) (nth 2 bytes)
                         (nth 3 bytes) (nth 4 bytes) (nth 5 bytes)
                         (nth 6 bytes) (nth 7 bytes))
                   '(#x48 #x89 #xf0 #xf0 #x48 #x0f #xb1 #x17)))
    (should (= (length bytes) 15))))

(defun nelisp-native-unit-test--unit (table)
  (list :unit-id "u" :control 100 :publisher 200
        :contract '(("f" . 1)) :gates '(("f" . 300))
        :binary-sha256 "bin" :table table))

(ert-deftest nelisp-native-unit/publish-rejects-captured-stale-table-and-consumes ()
  (let ((nelisp-native-unit--units (list (cons "u"
                                               (nelisp-native-unit-test--unit 20))))
        (nelisp-native-unit--candidates
         '(("c" :unit-id "u" :expected-table 10 :generation 2
            :table 400 :artifact "a" :artifact-hash "h"
            :source "s" :source-hash "h" :binary-sha256 "b"
            :created 1000)))
        (calls nil))
    (cl-letf (((symbol-function 'nelisp-native-unit--hash) (lambda (_) "h"))
              ((symbol-function 'nelisp-native-load--running-binary-sha256)
               (lambda () "b"))
              ((symbol-function 'float-time) (lambda () 1001))
              ((symbol-function 'ptr-read-u64) (lambda (_ _) 20))
              ((symbol-function 'ptr-call)
               (lambda (publisher control expected next &rest _)
                 (setq calls (list publisher control expected next))
                 ;; Simulate the controller's current table being 20: the
                 ;; captured expected pointer 10 must fail the CAS.
                 (if (= expected 20) 1 0))))
      (let ((result (nelisp-native-unit-publish "c")))
        (should (eq (plist-get result :status) 'rejected))
        (should (equal calls '(200 100 10 400)))
        (should (= (ptr-read-u64 100 0) 20))
        (should-not (assoc "c" nelisp-native-unit--candidates))))))

(ert-deftest nelisp-native-unit/publish-rejects-tampered-input-before-cas ()
  (let ((nelisp-native-unit--units (list (cons "u"
                                               (nelisp-native-unit-test--unit 0))))
        (nelisp-native-unit--candidates
         '(("c" :unit-id "u" :expected-table 0 :generation 1
            :table 400 :artifact "a" :artifact-hash "expected"
            :source "s" :source-hash "expected" :binary-sha256 "b"
            :created 1000)))
        (calls 0))
    (cl-letf (((symbol-function 'nelisp-native-unit--hash)
               (lambda (path) (if (equal path "a") "expected" "tampered")))
              ((symbol-function 'nelisp-native-load--running-binary-sha256)
               (lambda () "b"))
              ((symbol-function 'float-time) (lambda () 1001))
              ((symbol-function 'ptr-call) (lambda (&rest _) (setq calls (1+ calls)) 1)))
      (let ((result (nelisp-native-unit-publish "c")))
        (should (eq (plist-get result :status) 'rejected))
        (should (= calls 0))
        (should-not (assoc "c" nelisp-native-unit--candidates))))))

(ert-deftest nelisp-native-unit/expired-candidate-cannot-be-reused ()
  (let ((nelisp-native-unit--units (list (cons "u"
                                               (nelisp-native-unit-test--unit 0))))
        (nelisp-native-unit--candidates
         '(("c" :unit-id "u" :expected-table 0 :generation 1
            :table 400 :artifact "a" :artifact-hash "h"
            :source "s" :source-hash "h" :binary-sha256 "b"
            :created 0))))
    (cl-letf (((symbol-function 'float-time) (lambda () 1000)))
      (should (eq (plist-get (nelisp-native-unit-publish "c") :status)
                  'rejected))
      (should (eq (plist-get (nelisp-native-unit-publish "c") :status)
                  'rejected)))))

(ert-deftest nelisp-native-unit/status-is-a-copy-of-export-report ()
  (let ((nelisp-native-unit--units
         (list (cons "u" (nelisp-native-unit-test--unit 500)))))
    (cl-letf (((symbol-function 'ptr-read-u64) (lambda (_ _) 500)))
      (let ((report (nelisp-native-unit-status "u")))
        (setcdr (car (plist-get report :exports)) 99)
        (should (= (cdr (assoc "f"
                               (plist-get (cdr (assoc "u" nelisp-native-unit--units))
                                          :contract)))
                   1))))))

(ert-deftest nelisp-native-unit/stable-address-is-reused-for-a-unit ()
  (let ((nelisp-native-unit--units
         '(("u" :control 10 :contract (("f" . 1))
                 :gates (("f" . 99)) :generation 2))))
    (cl-letf (((symbol-function 'ptr-read-u64) (lambda (&rest _) 4096)))
      (should (= (nelisp-native-unit-address "u" "f")
                 (nelisp-native-unit-address "u" "f"))))))

;;;; Part 1 -- provenance ----------------------------------------------
;;
;; Pure accounting/history logic below runs on every host, with no
;; native runtime.  `nelisp-native-unit-test--unit' predates this file's
;; provenance keys, so publishing/status/code-info tests below either
;; `plist-put' extra keys onto it or build a unit literal directly.

(ert-deftest nelisp-native-unit/history-push-keeps-newest-16-oldest-evicted-first ()
  (let ((history nil))
    (dotimes (i 20)
      (setq history (nelisp-native-unit--history-push history (list :generation i))))
    (should (= (length history) nelisp-native-unit-history-max))
    (should (= (plist-get (car history) :generation) 4))
    (should (= (plist-get (car (last history)) :generation) 19))))

(ert-deftest nelisp-native-unit/status-exposes-published-identity-and-export-addresses ()
  ;; Against the bug: before Part 1, `nelisp-native-unit-status' has no
  ;; :source/:artifact/:published-at/:export-addresses keys at all, so
  ;; every `plist-get' below reads nil and every `should' here fails.
  (let ((unit (nelisp-native-unit-test--unit 500)))
    (plist-put unit :published-source "src.el")
    (plist-put unit :published-source-sha256 "srch")
    (plist-put unit :published-artifact "art.nelr")
    (plist-put unit :published-artifact-sha256 "arth")
    (plist-put unit :published-at 4242)
    (let ((nelisp-native-unit--units (list (cons "u" unit))))
      (cl-letf (((symbol-function 'ptr-read-u64) (lambda (&rest _) 500)))
        (let ((status (nelisp-native-unit-status "u")))
          (should (equal (plist-get status :source) "src.el"))
          (should (equal (plist-get status :source-sha256) "srch"))
          (should (equal (plist-get status :artifact) "art.nelr"))
          (should (equal (plist-get status :artifact-sha256) "arth"))
          (should (= (plist-get status :published-at) 4242))
          (should (equal (plist-get status :export-addresses)
                         '((:name "f" :arity 1 :address 300 :generation 500)))))))))

(ert-deftest nelisp-native-unit/code-info-with-name-reports-arity-and-stable-address ()
  (let ((nelisp-native-unit--units
         (list (cons "u" (nelisp-native-unit-test--unit 500)))))
    (cl-letf (((symbol-function 'ptr-read-u64) (lambda (&rest _) 500)))
      (let ((info (nelisp-native-unit-code-info "u" "f")))
        (should (= (plist-get info :export-arity) 1))
        (should (= (plist-get info :export-address) 300))))))

(ert-deftest nelisp-native-unit/code-info-invents-no-identity-when-never-published ()
  ;; A unit-id nobody ever staged.
  (let ((nelisp-native-unit--units nil))
    (let ((info (nelisp-native-unit-code-info "ghost")))
      (should (eq (plist-get info :published) nil))
      (should (null (plist-get info :source)))
      (should (null (plist-get info :source-sha256)))
      (should (null (plist-get info :current-source-sha256)))))
  ;; A unit that exists (a gate was allocated) but whose control word is
  ;; still the zero it was initialized with -- never published.
  (let ((nelisp-native-unit--units
         (list (cons "u" (nelisp-native-unit-test--unit 0)))))
    (cl-letf (((symbol-function 'ptr-read-u64) (lambda (&rest _) 0)))
      (let ((info (nelisp-native-unit-code-info "u")))
        (should (eq (plist-get info :published) nil))
        (should (null (plist-get info :source)))
        (should (eq (plist-get info :source-current) nil))))))

(ert-deftest nelisp-native-unit/code-info-source-current-recompares-file-on-disk ()
  (let ((path (make-temp-file "nelisp-native-unit-source-")))
    (unwind-protect
        (progn
          (with-temp-file path (insert "version-1"))
          (let* ((hash1 (nelisp-native-unit--hash path))
                 (unit (nelisp-native-unit-test--unit 500)))
            (plist-put unit :published-source path)
            (plist-put unit :published-source-sha256 hash1)
            (let ((nelisp-native-unit--units (list (cons "u" unit))))
              (cl-letf (((symbol-function 'ptr-read-u64) (lambda (&rest _) 500)))
                (let ((info (nelisp-native-unit-code-info "u")))
                  (should (eq (plist-get info :published) t))
                  (should (eq (plist-get info :source-current) t))
                  (should (equal (plist-get info :current-source-sha256) hash1)))
                ;; Edited on disk: no longer current, but a fresh hash is
                ;; still reported -- never a silent t.
                (with-temp-file path (insert "version-2"))
                (let ((info (nelisp-native-unit-code-info "u")))
                  (should (eq (plist-get info :source-current) nil))
                  (should (plist-get info :current-source-sha256))
                  (should-not (equal (plist-get info :current-source-sha256) hash1)))
                ;; Deleted: :current-source-sha256 must go nil too, not a
                ;; guess and not a silent t.
                (delete-file path)
                (let ((info (nelisp-native-unit-code-info "u")))
                  (should (eq (plist-get info :source-current) nil))
                  (should (null (plist-get info :current-source-sha256))))))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-native-unit/publish-on-success-records-identity-and-history ()
  (let* ((unit (nelisp-native-unit-test--unit 0))
         (nelisp-native-unit--units (list (cons "u" unit)))
         (nelisp-native-unit--candidates
          '(("c" :unit-id "u" :expected-table 0 :generation 1
             :table 400 :artifact "a" :artifact-hash "ah"
             :source "s" :source-hash "sh" :binary-sha256 "b"
             :artifact-handle (:code-size 999) :created 1000))))
    (cl-letf (((symbol-function 'nelisp-native-unit--hash)
               (lambda (path) (cond ((equal path "a") "ah")
                                    ((equal path "s") "sh")
                                    (t "wrong"))))
              ((symbol-function 'nelisp-native-load--running-binary-sha256)
               (lambda () "b"))
              ((symbol-function 'float-time) (lambda () 1500))
              ((symbol-function 'ptr-read-u64) (lambda (&rest _) 400))
              ((symbol-function 'ptr-call) (lambda (&rest _) 1)))
      (let ((result (nelisp-native-unit-publish "c")))
        (should (eq (plist-get result :status) 'published))
        (should (= (plist-get result :generation) 1)))
      (should (equal (plist-get unit :published-source) "s"))
      (should (equal (plist-get unit :published-source-sha256) "sh"))
      (should (equal (plist-get unit :published-artifact) "a"))
      (should (equal (plist-get unit :published-artifact-sha256) "ah"))
      (should (= (plist-get unit :published-at) 1500))
      (should (equal (plist-get unit :active-handle) '(:code-size 999)))
      (should (equal (plist-get unit :history)
                     '((:generation 1 :source-sha256 "sh" :artifact-sha256 "ah"
                        :published-at 1500))))
      (let ((info (nelisp-native-unit-code-info "u")))
        (should (eq (plist-get info :published) t))
        (should (equal (plist-get info :source) "s"))))))

(ert-deftest nelisp-native-unit/cas-rejected-publish-preserves-prior-identity-and-reclaims ()
  (let* ((unit (nelisp-native-unit-test--unit 20)))
    (plist-put unit :published-source "orig.el")
    (plist-put unit :published-source-sha256 "orig-hash")
    (plist-put unit :published-artifact "orig.nelr")
    (plist-put unit :published-artifact-sha256 "orig-artifact-hash")
    (plist-put unit :published-at 42)
    (let ((nelisp-native-unit--units (list (cons "u" unit)))
          (nelisp-native-unit--candidates
           '(("c" :unit-id "u" :expected-table 10 :generation 2
              :table 400 :artifact "a" :artifact-hash "h"
              :source "s" :source-hash "h" :binary-sha256 "b"
              :artifact-handle (:code-size 111) :created 1000)))
          (nelisp-native-unit--reclaimed-tables 0)
          (munmap-calls nil))
      (cl-letf (((symbol-function 'nelisp-native-unit--hash) (lambda (_) "h"))
                ((symbol-function 'nelisp-native-load--running-binary-sha256)
                 (lambda () "b"))
                ((symbol-function 'float-time) (lambda () 1001))
                ((symbol-function 'ptr-read-u64) (lambda (&rest _) 20))
                ((symbol-function 'ptr-call) (lambda (&rest _) 0)) ; CAS loses
                ((symbol-function 'syscall-direct)
                 (lambda (n addr size &rest _) (push (list n addr size) munmap-calls) 0))
                ((symbol-function 'nelisp-native-load-unload) (lambda (_h) 1)))
        (let ((result (nelisp-native-unit-publish "c")))
          (should (eq (plist-get result :status) 'rejected)))
        ;; Identity recorded by a prior, real publication is untouched.
        (should (equal (plist-get unit :published-source) "orig.el"))
        (should (equal (plist-get unit :published-source-sha256) "orig-hash"))
        (should (equal (plist-get unit :published-artifact) "orig.nelr"))
        (should (= (plist-get unit :published-at) 42))
        (should (null (plist-get unit :history)))
        ;; The losing candidate's own table was never CAS-installed, so
        ;; it is reclaimed rather than kept until process exit.
        (should (equal munmap-calls '((11 400 4096))))
        (should (= nelisp-native-unit--reclaimed-tables 1))))))

;;;; Part 2 -- resource reclamation -------------------------------------

(ert-deftest nelisp-native-unit/unmap-table-is-a-noop-without-syscall-direct ()
  ;; Zero mocking: on any host Emacs `syscall-direct' genuinely is not
  ;; fboundp, so this exercises the real guard, not a stand-in for it.
  (should-not (fboundp 'syscall-direct))
  (let ((nelisp-native-unit--reclaimed-tables 0)
        (nelisp-native-unit--reclaimed-bytes 0))
    (should-not (nelisp-native-unit--unmap-table 123456))
    (should (= nelisp-native-unit--reclaimed-tables 0))
    (should (= nelisp-native-unit--reclaimed-bytes 0))))

(ert-deftest nelisp-native-unit/unload-handle-is-a-noop-without-syscall-direct ()
  (should-not (fboundp 'syscall-direct))
  (let ((nelisp-native-unit--reclaimed-bytes 0))
    (should-not (nelisp-native-unit--unload-handle (list :code-size 4096)))
    (should (= nelisp-native-unit--reclaimed-bytes 0))))

(ert-deftest nelisp-native-unit/discard-reclaims-table-and-artifact-and-counts-them ()
  (let ((nelisp-native-unit--candidates
         '(("c" :unit-id "u" :table 12345 :artifact-handle (:code-size 8192))))
        (nelisp-native-unit--reclaimed-tables 0)
        (nelisp-native-unit--reclaimed-bytes 0)
        (munmap-calls nil)
        (unload-calls nil))
    (cl-letf (((symbol-function 'syscall-direct)
               (lambda (n addr size &rest _) (push (list n addr size) munmap-calls) 0))
              ((symbol-function 'nelisp-native-load-unload)
               (lambda (h) (push h unload-calls) 1)))
      (nelisp-native-unit-discard "c")
      (should-not (assoc "c" nelisp-native-unit--candidates))
      (should (equal munmap-calls '((11 12345 4096))))
      (should (= (length unload-calls) 1))
      (should (= nelisp-native-unit--reclaimed-tables 1))
      (should (= nelisp-native-unit--reclaimed-bytes (+ 4096 8192))))))

(ert-deftest nelisp-native-unit/purge-reclaims-an-expired-candidates-table ()
  (let ((nelisp-native-unit--candidates
         '(("c" :unit-id "u" :table 555 :artifact-handle nil :created 0)))
        (nelisp-native-unit--reclaimed-tables 0)
        (nelisp-native-unit--reclaimed-bytes 0)
        (munmap-calls nil))
    (cl-letf (((symbol-function 'float-time) (lambda () 1000))
              ((symbol-function 'syscall-direct)
               (lambda (n addr size &rest _) (push (list n addr size) munmap-calls) 0)))
      (nelisp-native-unit--purge)
      (should-not nelisp-native-unit--candidates)
      (should (equal munmap-calls '((11 555 4096))))
      (should (= nelisp-native-unit--reclaimed-tables 1))
      (should (= nelisp-native-unit--reclaimed-bytes 4096)))))

(ert-deftest nelisp-native-unit/still-live-candidate-is-not-reclaimed-by-purge ()
  ;; Against the bug: a naive purge that reclaims by list membership
  ;; rather than by TTL would also unmap a fresh candidate's table.
  (let ((nelisp-native-unit--candidates
         '(("c" :unit-id "u" :table 555 :artifact-handle nil :created 999)))
        (nelisp-native-unit--reclaimed-tables 0)
        (munmap-calls nil))
    (cl-letf (((symbol-function 'float-time) (lambda () 1000))
              ((symbol-function 'syscall-direct)
               (lambda (n addr size &rest _) (push (list n addr size) munmap-calls) 0)))
      (nelisp-native-unit--purge)
      (should (assoc "c" nelisp-native-unit--candidates))
      (should-not munmap-calls)
      (should (= nelisp-native-unit--reclaimed-tables 0)))))

(ert-deftest nelisp-native-unit/reclaim-never-releases-the-active-generation ()
  (let* ((unit (list :unit-id "u" :control 100 :publisher 200
                     :contract '(("f" . 1)) :gates '(("f" . 300))
                     :binary-sha256 "b"
                     :active-table 999 :active-handle '(:code-size 111)
                     :active-generation 3
                     :retired (list (list :generation 2 :table 555
                                          :handle '(:code-size 222)
                                          :retired-at 10 :released nil))))
         (nelisp-native-unit--units (list (cons "u" unit)))
         (nelisp-native-unit--candidates nil)
         (calls nil))
    (cl-letf (((symbol-function 'syscall-direct) (lambda (&rest args) (push args calls) 0))
              ((symbol-function 'nelisp-native-load-unload) (lambda (h) (push h calls) 1)))
      (let ((report (nelisp-native-unit-reclaim "u")))
        (should (null (plist-get report :released)))
        (should (= (length (plist-get report :refused)) 1))
        (let ((refusal (car (plist-get report :refused))))
          (should (equal (plist-get refusal :unit-id) "u"))
          (should (= (plist-get refusal :generation) 2))
          (should (stringp (plist-get refusal :reason))))
        ;; Nothing was released -- neither the retired generation nor,
        ;; especially, the active one -- because reclaim never actually
        ;; calls the release primitives at all today.
        (should-not calls)))))

(ert-deftest nelisp-native-unit/resources-reports-retained-and-cumulative-totals ()
  (let* ((unit (list :unit-id "u"
                     :retired (list (list :generation 1 :table 1
                                          :handle '(:code-size 500) :released nil)
                                    (list :generation 2 :table 2
                                          :handle nil :released t))))
         (nelisp-native-unit--units (list (cons "u" unit)))
         (nelisp-native-unit--candidates '(("c1" :table 1) ("c2" :table 2)))
         (nelisp-native-unit--reclaimed-tables 7)
         (nelisp-native-unit--reclaimed-bytes 12345))
    (let ((report (nelisp-native-unit-resources)))
      (should (= (plist-get report :candidates) 2))
      (should (= (plist-get report :units) 1))
      ;; Only the not-yet-released retired entry counts.
      (should (= (plist-get report :retired) 1))
      (should (= (plist-get report :retained-bytes) (+ 4096 500)))
      (should (= (plist-get report :reclaimed-tables) 7))
      (should (= (plist-get report :reclaimed-bytes) 12345))
      (should (stringp (plist-get report :retained-reason))))))

;;;; Native-runtime evidence --------------------------------------------
;;
;; The rest of this file needs the opt-in Linux native runtime
;; (`ptr-call'/`syscall-direct'/mmap in-process): staging itself maps a
;; raw artifact, which `nelisp-native-load--raw-artifact-p' refuses to do
;; anywhere else.  These `skip-unless' on host Emacs, by architecture,
;; not by an accident of this machine -- see AI.md's REPL section and
;; `nelisp-native-load--raw-supported-p''s own docstring.  The pure-logic
;; tests above are the evidence that actually runs under `test-one'.

(defun nelisp-native-unit-test--write-source (fn-name body)
  "Write a temp raw-runtime source file: `(defun FN-NAME (x) BODY)'."
  (let ((path (make-temp-file "nelisp-native-unit-native-" nil ".el")))
    (with-temp-file path
      (insert (format "(defun %s (x) %s)\n" fn-name body)))
    path))

(ert-deftest nelisp-native-unit/native-discard-reclaims-a-real-table-and-artifact ()
  (skip-unless (nelisp-native-load--raw-supported-p))
  (let* ((binary (nelisp-native-load--running-binary-sha256))
         (source (nelisp-native-unit-test--write-source "nnuTestDiscardFn" "(+ x 1)"))
         (artifact (concat source ".nelr")))
    (unwind-protect
        (progn
          (nelisp-native-load-raw-compile-file
           source artifact nil "nelisp-native-unit-test" binary)
          (let ((before (plist-get (nelisp-native-unit-resources) :reclaimed-tables))
                (staged (nelisp-native-unit-stage artifact nil '("nnuTestDiscardFn"))))
            (should (eq (plist-get staged :status) 'staged))
            (nelisp-native-unit-discard (plist-get staged :candidate-id))
            (should (= (plist-get (nelisp-native-unit-resources) :reclaimed-tables)
                       (1+ before)))))
      (ignore-errors (delete-file source))
      (ignore-errors (delete-file artifact)))))

(ert-deftest nelisp-native-unit/native-republish-retires-first-generation-keeps-second-live ()
  (skip-unless (nelisp-native-load--raw-supported-p))
  (let* ((binary (nelisp-native-load--running-binary-sha256))
         (source1 (nelisp-native-unit-test--write-source "nnuTestRepublishFn" "(+ x 1)"))
         (artifact1 (concat source1 ".nelr"))
         (source2 (nelisp-native-unit-test--write-source "nnuTestRepublishFn" "(+ x 2)"))
         (artifact2 (concat source2 ".nelr")))
    (unwind-protect
        (progn
          (nelisp-native-load-raw-compile-file
           source1 artifact1 nil "nelisp-native-unit-test" binary)
          (nelisp-native-load-raw-compile-file
           source2 artifact2 nil "nelisp-native-unit-test" binary)
          (let ((staged1 (nelisp-native-unit-stage artifact1 nil '("nnuTestRepublishFn"))))
            (should (eq (plist-get staged1 :status) 'staged))
            (let* ((unit-id (plist-get staged1 :unit-id))
                   (published1 (nelisp-native-unit-publish
                                (plist-get staged1 :candidate-id))))
              (should (eq (plist-get published1 :status) 'published))
              (should (= (nelisp-native-unit-call unit-id "nnuTestRepublishFn" '(5)) 6))
              (let* ((staged2 (nelisp-native-unit-stage
                                artifact2 unit-id '("nnuTestRepublishFn")))
                     (published2 (nelisp-native-unit-publish
                                  (plist-get staged2 :candidate-id))))
                (should (eq (plist-get published2 :status) 'published))
                (should (= (nelisp-native-unit-call unit-id "nnuTestRepublishFn" '(5)) 7))
                (let ((info (nelisp-native-unit-code-info unit-id)))
                  (should (equal (plist-get info :source) source2))
                  (should (eq (plist-get info :source-current) t)))
                (let ((reclaim (nelisp-native-unit-reclaim unit-id)))
                  (should (null (plist-get reclaim :released)))
                  (should (= (length (plist-get reclaim :refused)) 1)))
                ;; DoD (e): the active (second) generation still answers
                ;; calls after a reclaim attempt that refused to touch it.
                (should (= (nelisp-native-unit-call unit-id "nnuTestRepublishFn" '(5))
                           7))))))
      (ignore-errors (delete-file source1)) (ignore-errors (delete-file artifact1))
      (ignore-errors (delete-file source2)) (ignore-errors (delete-file artifact2)))))

(provide 'nelisp-native-unit-test)
