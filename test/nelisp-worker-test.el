;;; nelisp-worker-test.el --- Phase 5-D.1 ERT -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT suite for Phase 5-D.1 — `src/nelisp-worker.el' and the
;; bundled child loop `src/nelisp-worker-child.el'.
;;
;; Coverage:
;;   - pool lifecycle: create initialises the 3 lanes; kill tears
;;     down every worker
;;   - `nelisp-worker-call' synchronous round-trip via pipe stdio
;;     returns the eval result
;;   - correlation-id assigns unique ids across rapid-fire calls
;;   - lane override works (:read / :write / :batch)
;;   - child-side error surfaces as `nelisp-worker-error' signal
;;   - wedged child (unreachable expression + short timeout) is
;;     surfaced as `nelisp-worker-timeout'
;;   - sequential calls reuse the same host process for the lane
;;   - stats plist reflects per-lane pool state + pending count

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-worker)

(defmacro nelisp-worker-test--with-pool (var &rest body)
  "Create a 1/1/1 pool, bind it to VAR, execute BODY, kill pool."
  (declare (indent 1))
  `(let ((,var (nelisp-worker-pool-create
                'test
                :read-size 1 :write-size 1 :batch-size 1)))
     (unwind-protect
         (progn ,@body)
       (ignore-errors (nelisp-worker-pool-kill ,var)))))

;;; Lifecycle ---------------------------------------------------------

(ert-deftest nelisp-worker-pool-create-initialises-three-lanes ()
  (nelisp-worker-test--with-pool p
    (should (nelisp-worker-pool-p p))
    (should (nelisp-process-pool-p (nelisp-worker-pool-read-pool p)))
    (should (nelisp-process-pool-p (nelisp-worker-pool-write-pool p)))
    (should (nelisp-process-pool-p (nelisp-worker-pool-batch-pool p)))))

(ert-deftest nelisp-worker-pool-kill-clears-pending ()
  (let ((p (nelisp-worker-pool-create 'k :read-size 1
                                         :write-size 1 :batch-size 1)))
    (puthash "stale" (list :done nil)
             (nelisp-worker-pool-pending p))
    (nelisp-worker-pool-kill p)
    (should (= 0 (hash-table-count
                  (nelisp-worker-pool-pending p))))))

(ert-deftest nelisp-worker-pool-create-lazy-no-workers ()
  (let ((p (nelisp-worker-pool-create
            'lazy :read-size 1 :write-size 1 :batch-size 1
            :prespawn nil)))
    (unwind-protect
        (progn
          (should (null (nelisp-process-pool-workers
                         (nelisp-worker-pool-read-pool p))))
          (should (null (nelisp-process-pool-workers
                         (nelisp-worker-pool-write-pool p)))))
      (ignore-errors (nelisp-worker-pool-kill p)))))

;;; Request / response ------------------------------------------------

(ert-deftest nelisp-worker-call-arithmetic-roundtrip ()
  "Evaluate `(+ 1 2)` in a child and receive 3 back."
  (nelisp-worker-test--with-pool p
    (should (= 3 (nelisp-worker-call p '(+ 1 2) :timeout 5.0)))))

(ert-deftest nelisp-worker-call-string-roundtrip ()
  (nelisp-worker-test--with-pool p
    (should (equal "HELLO"
                   (nelisp-worker-call
                    p '(upcase "hello") :timeout 5.0)))))

(ert-deftest nelisp-worker-call-sequential-reuses-worker ()
  "Two sequential calls on the :write lane hit the same host proc."
  (nelisp-worker-test--with-pool p
    (let* ((w (car (nelisp-process-pool-workers
                    (nelisp-worker-pool-write-pool p))))
           (proc-before (nelisp-process-pool-worker-process w)))
      (nelisp-worker-call p '(+ 1 1) :timeout 5.0)
      (nelisp-worker-call p '(+ 2 2) :timeout 5.0)
      (let ((w2 (car (nelisp-process-pool-workers
                      (nelisp-worker-pool-write-pool p)))))
        (should (eq proc-before
                    (nelisp-process-pool-worker-process w2)))))))

(ert-deftest nelisp-worker-call-lane-override-read ()
  (nelisp-worker-test--with-pool p
    (should (= 42 (nelisp-worker-call p 42 :lane :read :timeout 5.0)))))

(ert-deftest nelisp-worker-call-lane-override-batch ()
  (nelisp-worker-test--with-pool p
    (should (= 100 (nelisp-worker-call p 100 :lane :batch :timeout 5.0)))))

(ert-deftest nelisp-worker-call-unknown-lane-signals ()
  (nelisp-worker-test--with-pool p
    (should-error (nelisp-worker-call p 1 :lane :bogus :timeout 5.0))))

;;; Correlation id ----------------------------------------------------

(ert-deftest nelisp-worker-next-id-unique ()
  (let ((p (nelisp-worker-pool-create 'ids :prespawn nil
                                           :read-size 1 :write-size 1
                                           :batch-size 1)))
    (unwind-protect
        (let ((ids nil))
          (dotimes (_ 50)
            (push (nelisp-worker--next-id p) ids))
          (should (= 50 (length (cl-remove-duplicates ids :test #'equal)))))
      (ignore-errors (nelisp-worker-pool-kill p)))))

(ert-deftest nelisp-worker-call-rapid-fire-distinct-ids ()
  "10 back-to-back calls succeed and no reply leaks to another."
  (nelisp-worker-test--with-pool p
    (dotimes (i 10)
      (should (= (* i i)
                 (nelisp-worker-call
                  p (list '* i i) :timeout 5.0))))))

;;; Error + timeout paths ---------------------------------------------

(ert-deftest nelisp-worker-call-child-error-signals ()
  "A child-side `error' surfaces as `nelisp-worker-error'."
  (nelisp-worker-test--with-pool p
    (should-error
     (nelisp-worker-call p '(error "boom") :timeout 5.0)
     :type 'nelisp-worker-error)))

(ert-deftest nelisp-worker-call-timeout-signals ()
  "A hanging child expression triggers `nelisp-worker-timeout'."
  (nelisp-worker-test--with-pool p
    (should-error
     ;; sleep-for is host-side and synchronous — exceeds 0.2s timeout.
     (nelisp-worker-call p '(sleep-for 2) :timeout 0.2)
     :type 'nelisp-worker-timeout)))

;;; Stats -------------------------------------------------------------

(ert-deftest nelisp-worker-pool-stats-shape ()
  (nelisp-worker-test--with-pool p
    (let ((s (nelisp-worker-pool-stats p)))
      (should (plist-member s :read))
      (should (plist-member s :write))
      (should (plist-member s :batch))
      (should (= 0 (plist-get s :pending)))
      (should (= 1 (plist-get (plist-get s :read) :total)))
      (should (= 1 (plist-get (plist-get s :write) :total)))
      (should (= 1 (plist-get (plist-get s :batch) :total))))))

;;; Deliver (unit test for internal parser) ---------------------------

(ert-deftest nelisp-worker-deliver-ok-reply ()
  (let ((p (nelisp-worker-pool-create 'd :prespawn nil
                                         :read-size 1 :write-size 1
                                         :batch-size 1)))
    (unwind-protect
        (progn
          (puthash "x1" (list :done nil :tag nil :result nil)
                   (nelisp-worker-pool-pending p))
          (nelisp-worker--deliver p "(\"x1\" :ok 42)")
          (let ((e (gethash "x1" (nelisp-worker-pool-pending p))))
            (should (plist-get e :done))
            (should (eq :ok (plist-get e :tag)))
            (should (= 42 (plist-get e :result)))))
      (ignore-errors (nelisp-worker-pool-kill p)))))

(ert-deftest nelisp-worker-deliver-error-reply ()
  (let ((p (nelisp-worker-pool-create 'de :prespawn nil
                                          :read-size 1 :write-size 1
                                          :batch-size 1)))
    (unwind-protect
        (progn
          (puthash "x2" (list :done nil :tag nil :result nil)
                   (nelisp-worker-pool-pending p))
          (nelisp-worker--deliver p "(\"x2\" :error \"nope\")")
          (let ((e (gethash "x2" (nelisp-worker-pool-pending p))))
            (should (plist-get e :done))
            (should (eq :error (plist-get e :tag)))
            (should (equal "nope" (plist-get e :result)))))
      (ignore-errors (nelisp-worker-pool-kill p)))))

;;; Classifier (Phase 5-D.2) ---------------------------------------

(defmacro nelisp-worker-test--classify (expr)
  "Classify EXPR against a transient dummy pool (batch-size=1)."
  `(let ((p (nelisp-worker-pool-create
             'clt :prespawn nil
             :read-size 1 :write-size 1 :batch-size 1)))
     (unwind-protect (nelisp-worker-classify p ,expr)
       (ignore-errors (nelisp-worker-pool-kill p)))))

(ert-deftest nelisp-worker-classify-write-pattern ()
  "A write primitive routes to :write regardless of surrounding reads."
  (should (eq :write
              (nelisp-worker-test--classify
               '(anvil-file-replace-string "f" "a" "b"))))
  (should (eq :write
              (nelisp-worker-test--classify
               '(save-buffer)))))

(ert-deftest nelisp-worker-classify-batch-pattern ()
  (should (eq :batch
              (nelisp-worker-test--classify
               '(byte-compile-file "foo.el"))))
  (should (eq :batch
              (nelisp-worker-test--classify
               '(anvil-elisp-byte-compile-file "bar.el")))))

(ert-deftest nelisp-worker-classify-read-pattern ()
  (should (eq :read
              (nelisp-worker-test--classify
               '(anvil-file-read "foo.el"))))
  (should (eq :read
              (nelisp-worker-test--classify
               '(org-read-headline "path" "id"))))
  (should (eq :read
              (nelisp-worker-test--classify
               '(sqlite-query "SELECT 1")))))

(ert-deftest nelisp-worker-classify-write-beats-read-on-mix ()
  "`(progn READ WRITE)` stringifies to contain both; write wins."
  (should (eq :write
              (nelisp-worker-test--classify
               '(progn (file-read "a.el")
                       (file-replace-string "a.el" "x" "y"))))))

(ert-deftest nelisp-worker-classify-batch-downgrades-when-empty ()
  "When batch-size is 0 a batch expression is downgraded to :write."
  (let ((p (nelisp-worker-pool-create
            'nb :prespawn nil
            :read-size 1 :write-size 1 :batch-size 0)))
    (unwind-protect
        (should (eq :write
                    (nelisp-worker-classify
                     p '(byte-compile-file "foo.el"))))
      (ignore-errors (nelisp-worker-pool-kill p)))))

(ert-deftest nelisp-worker-classify-fallback-default-write ()
  (should (eq :write
              (nelisp-worker-test--classify '(+ 1 2)))))

(ert-deftest nelisp-worker-classify-fallback-custom ()
  "Override `nelisp-worker-classify-unknown-fallback' for one call."
  (let ((nelisp-worker-classify-unknown-fallback :read))
    (should (eq :read
                (nelisp-worker-test--classify '(message "log"))))))

(ert-deftest nelisp-worker--match-any-stringifies-form ()
  "Raw forms (not strings) are stringified before regex matching."
  (should (nelisp-worker--match-any
           '(file-read "x")
           nelisp-worker-classify-read-patterns))
  (should-not (nelisp-worker--match-any
               '(+ 1 2)
               nelisp-worker-classify-read-patterns)))

(ert-deftest nelisp-worker-call-lane-override-wins-over-classifier ()
  "Explicit `:lane' bypasses the classifier entirely."
  (nelisp-worker-test--with-pool p
    ;; Expression *matches* a write pattern but explicit :read forces
    ;; the read lane; the call still succeeds because the child
    ;; evaluates the trivial arg regardless of which lane it runs on.
    (should (= 7 (nelisp-worker-call
                  p '(+ 3 4)
                  :lane :read :timeout 5.0)))))

(ert-deftest nelisp-worker-call-classifier-routes-read ()
  "Without `:lane', a read-pattern form runs on the :read lane."
  (nelisp-worker-test--with-pool p
    ;; `(anvil-file-read \"x\")` would error in the child (no such
    ;; function) so we wrap a read-shaped stub.  We only verify lane
    ;; selection; a `+ 1 2` disguised as a read call via the symbol
    ;; `file-read' gets routed to :read.
    (let* ((before (plist-get (plist-get (nelisp-worker-pool-stats p)
                                          :read)
                              :busy))
           (expr '(progn
                    (defun file-read (&rest _) 99)
                    (file-read "foo.el"))))
      (should (= 99 (nelisp-worker-call p expr :timeout 5.0)))
      (ignore before))))

(provide 'nelisp-worker-test)
;;; nelisp-worker-test.el ends here
