;;; nelisp-dev-gc-test.el --- DEV GC adapter tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'nelisp-dev-gc)

(ert-deftest nelisp-dev-gc/compares-cumulative-only-and-refuses-reversed-input ()
  (let ((nelisp-dev-gc--snapshots (make-hash-table :test #'equal))
        (nelisp-dev-gc--epochs (make-hash-table :test #'equal))
        (total 10) (collections 2)
        (context '(:target "native-linux-x86_64" :session-id "comparison")))
    (cl-letf (((symbol-function 'nelisp-dev-gc--reload-status)
               (lambda () '(:status ready :generation 1)))
              ((symbol-function 'nelisp-repl-gc-snapshot)
               (lambda ()
                 (list :raw (list :abi-status :available
                                  :gc-stats (list :allocated-total total :collections collections)
                                  :conservative (list :scanned-bytes (* 10 total)))))))
      (let ((a (nelisp-dev-gc--put (nelisp-dev-gc--snapshot context))))
        (setq total 20 collections 3)
        (let* ((b (nelisp-dev-gc--put (nelisp-dev-gc--snapshot context)))
               (result (nelisp-dev-gc-dispatch
                        (nelisp-dev-gc-test--request
                         "gc.compare" (list (cons "before" a) (cons "after" b))) context))
               (delta (nelisp-dev-gc-test--data result "delta"))
               (raw (cdr (assoc "raw" delta)))
               (stats (cdr (assoc "gc-stats" raw))))
          (should (equal "ok" (nelisp-dev-gc-test--status result)))
          (should (= 10 (cdr (assoc "allocated-total" stats))))
          (should (= 1 (cdr (assoc "collections" stats))))
          (should-not (assoc "conservative" raw))
          (should (equal "inconclusive"
                         (nelisp-dev-gc-test--status
                          (nelisp-dev-gc-dispatch
                           (nelisp-dev-gc-test--request
                            "gc.compare" (list (cons "before" b) (cons "after" a))) context)))))))))

(ert-deftest nelisp-dev-gc/snapshot-generation-race-is-inconclusive ()
  (let ((generation 1))
    (cl-letf (((symbol-function 'nelisp-dev-gc--reload-status)
               (lambda () (list :status 'ready :generation generation)))
              ((symbol-function 'nelisp-repl-gc-snapshot)
               (lambda () (setq generation 2) (nelisp-dev-gc-test--raw 10 1))))
      (let ((response (nelisp-dev-gc-dispatch
                       '(("operation" . "gc.snapshot") ("request_id" . "race"))
                       '(:target "native-linux-x86_64" :session-id "race"))))
        (should (equal "inconclusive" (cdr (assoc "status" response))))))))

(ert-deftest nelisp-dev-gc/evicted-session-does-not-reuse-an-epoch ()
  (let ((nelisp-dev-gc--epochs (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'nelisp-dev-gc--reload-status)
               (lambda () '(:status ready :generation 1))))
      (let ((a (nelisp-dev-gc--annotate '(:session-id "original")
                                      (nelisp-dev-gc-test--raw 10 1))))
        (dotimes (i 70)
          (nelisp-dev-gc--annotate (list :session-id (number-to-string i))
                                   (nelisp-dev-gc-test--raw 10 1)))
        (should (<= (hash-table-count nelisp-dev-gc--epochs) 64))
        (should-not (= (plist-get a :epoch)
                       (plist-get (nelisp-dev-gc--annotate '(:session-id "original")
                                                          (nelisp-dev-gc-test--raw 10 1))
                                  :epoch)))))))

(defun nelisp-dev-gc-test--status (response)
  (cdr (assoc "status" response)))
(defun nelisp-dev-gc-test--data (response key)
  (cdr (assoc key (cdr (assoc "data" response)))))
(defun nelisp-dev-gc-test--raw (total &optional collections)
  (list :raw (list :abi-status :available
                   :gc-stats (list :allocated-total total
                                   :collections collections))))
(defun nelisp-dev-gc-test--request (operation &optional arguments)
  (append (list (cons "operation" operation) (cons "request_id" "test"))
          (and arguments (list (cons "arguments" arguments)))))
(ert-deftest nelisp-dev-gc/host-collect-is-unsupported ()
  (should (equal "unsupported"
                 (cdr (assoc "status"
                             (nelisp-dev-gc-dispatch
                              '(("operation" . "gc.collect") ("request_id" . "x"))
                              '(:target "host-emacs" :session-id "s")))))))

(ert-deftest nelisp-dev-gc/host-snapshot-real-api-serializes ()
  ;; This exercises the production observation path in a normal Emacs.  Raw
  ;; native fields may be unavailable, but that must be represented honestly
  ;; and must never make protocol JSON encoding fail.
  (let* ((response (nelisp-dev-gc-dispatch
                    (nelisp-dev-gc-test--request "gc.snapshot")
                    '(:target "host-emacs" :session-id "host")))
         (json (nelisp-dev-protocol-json response)))
    (should (member (nelisp-dev-gc-test--status response)
                    '("ok" "unsupported")))
    (should (stringp json))
    (should (string-match-p "schema_version" json))))
(ert-deftest nelisp-dev-gc/generation-and-counter-reset-advance-epoch ()
  (let ((generation 1) (total 20))
    (cl-letf (((symbol-function 'nelisp-repl-gc-snapshot)
               (lambda () (nelisp-dev-gc-test--raw total)))
              ((symbol-function 'nelisp-dev-gc--reload-status)
               (lambda () (list :status 'ready :generation generation))))
      (clrhash nelisp-dev-gc--epochs)
      (let* ((ctx (list :target "native-linux-x86_64" :session-id "s" :generation generation))
             (a (nelisp-dev-gc--snapshot ctx)))
        (setq generation 2)
        (plist-put ctx :generation generation)
        (let ((b (nelisp-dev-gc--snapshot ctx)))
          (should (> (plist-get b :epoch) (plist-get a :epoch)))
          (setq total 1)
          (should (> (plist-get (nelisp-dev-gc--snapshot ctx) :epoch)
                     (plist-get b :epoch))))))))

(ert-deftest nelisp-dev-gc/compare-refuses-cross-session-and-epoch ()
  (nelisp-dev-gc-clear)
  (let ((total 20) (generation 1))
    (cl-letf (((symbol-function 'nelisp-repl-gc-snapshot)
               (lambda () (nelisp-dev-gc-test--raw total)))
              ((symbol-function 'nelisp-dev-gc--reload-status)
               (lambda () (list :status 'ready :generation generation))))
      (let* ((ctx (list :target "native-linux-x86_64" :session-id "s1" :generation generation))
             (one (nelisp-dev-gc-dispatch (nelisp-dev-gc-test--request "gc.snapshot") ctx))
             (first (nelisp-dev-gc-test--data one "snapshot_id")))
        (setq generation 2 total 30)
        (plist-put ctx :generation generation)
        (let* ((two (nelisp-dev-gc-dispatch (nelisp-dev-gc-test--request "gc.snapshot") ctx))
               (second (nelisp-dev-gc-test--data two "snapshot_id"))
               (args (list (cons "before" first) (cons "after" second))))
          (should (equal "inconclusive"
                         (nelisp-dev-gc-test--status
                          (nelisp-dev-gc-dispatch (nelisp-dev-gc-test--request "gc.compare" args) ctx))))
          (plist-put ctx :session-id "s2")
          (should (equal "inconclusive"
                         (nelisp-dev-gc-test--status
                          (nelisp-dev-gc-dispatch (nelisp-dev-gc-test--request "gc.compare" args) ctx)))))))))

(ert-deftest nelisp-dev-gc/ttl-and-clear-bound-retained-snapshots ()
  (nelisp-dev-gc-clear)
  (let* ((record (list :session-id "ttl" :generation 1 :epoch 0 :abi :available
                       :snapshot (nelisp-dev-gc-test--raw 1)))
         (id (nelisp-dev-gc--put record)))
    (should id)
    (setcar (gethash id nelisp-dev-gc--snapshots) (- (float-time) 901))
    (should-not (nelisp-dev-gc--valid-entry id '(:session-id "ttl")))
    (should (= 0 (nelisp-dev-gc--purge-expired)))
    (should (= 0 (nelisp-dev-gc-clear "ttl"))))
  (should (nelisp-dev-gc--put (list :session-id "clear" :generation 1 :epoch 0)))
  (should (nelisp-dev-gc--put (list :session-id "clear" :generation 1 :epoch 0)))
  (should (= 2 (nelisp-dev-gc-clear "clear")))
  (should (= 0 (hash-table-count nelisp-dev-gc--snapshots))))

(ert-deftest nelisp-dev-gc/collect-is-explicit-and-reuses-collector-api ()
  (nelisp-dev-gc-clear)
  (let ((calls 0))
    (cl-letf (((symbol-function 'nelisp-repl-gc-collect)
               (lambda ()
                 (setq calls (1+ calls))
                 (list :before (nelisp-dev-gc-test--raw 10)
                       :after (nelisp-dev-gc-test--raw 12)
                       :collector-result 'collected)))
              ((symbol-function 'nelisp-dev-gc--snapshot)
               (lambda (_context)
                 (list :snapshot (nelisp-dev-gc-test--raw 9)
                       :session-id "n" :generation 7 :abi :available
                       :reload-status 'ready :epoch 0)))
              ((symbol-function 'nelisp-dev-gc--reload-status)
               (lambda () (list :status 'ready :generation 7))))
      (should (equal "unsupported"
                     (nelisp-dev-gc-test--status
                      (nelisp-dev-gc-dispatch (nelisp-dev-gc-test--request "gc.collect")
                                              '(:target "host-emacs" :session-id "h")))))
      (should (= 0 calls))
      (let ((response (nelisp-dev-gc-dispatch
                       (nelisp-dev-gc-test--request "gc.collect")
                       '(:target "native-linux-x86_64" :session-id "n" :generation 7))))
        (should (equal "ok" (nelisp-dev-gc-test--status response)))
        (should (= 1 calls))
        (should (equal t (nelisp-dev-gc-test--data response "mutating")))))))

(ert-deftest nelisp-dev-gc/unknown-generation-is-inconclusive ()
  (nelisp-dev-gc-clear)
  (let ((ctx '(:target "native-linux-x86_64" :session-id "u" :generation nil)))
    (cl-letf (((symbol-function 'nelisp-repl-gc-snapshot)
               (lambda () (nelisp-dev-gc-test--raw 1))))
      (let* ((a (nelisp-dev-gc-dispatch (nelisp-dev-gc-test--request "gc.snapshot") ctx))
             (b (nelisp-dev-gc-dispatch (nelisp-dev-gc-test--request "gc.snapshot") ctx))
             (args (list (cons "before" (nelisp-dev-gc-test--data a "snapshot_id"))
                         (cons "after" (nelisp-dev-gc-test--data b "snapshot_id")))))
        (should (equal "inconclusive"
                       (nelisp-dev-gc-test--status
                        (nelisp-dev-gc-dispatch (nelisp-dev-gc-test--request "gc.compare" args) ctx))))))))

(ert-deftest nelisp-dev-gc/wire-preserves-json-null-and-boolean-values ()
  (should (eq :null (nelisp-dev-gc--wire nil)))
  (should (eq :null (nelisp-dev-gc--wire :unknown)))
  (should (eq :null (nelisp-dev-gc--wire :unavailable)))
  (should (eq :false (nelisp-dev-gc--wire :false)))
  (should (eq t (nelisp-dev-gc--wire t))))

(ert-deftest nelisp-dev-gc/collection-counter-decrease-advances-epoch ()
  (nelisp-dev-gc-clear)
  (let ((collections 9))
    (cl-letf (((symbol-function 'nelisp-repl-gc-snapshot)
               (lambda () (nelisp-dev-gc-test--raw 10 collections)))
              ((symbol-function 'nelisp-dev-gc--reload-status)
               (lambda () (list :status 'ready :generation 4))))
      (let* ((ctx '(:target "native-linux-x86_64" :session-id "count"))
             (a (nelisp-dev-gc--snapshot ctx)))
        (setq collections 2)
        (should (> (plist-get (nelisp-dev-gc--snapshot ctx) :epoch)
                   (plist-get a :epoch)))))))

(ert-deftest nelisp-dev-gc/collect-generation-change-is-inconclusive ()
  (nelisp-dev-gc-clear)
  (let ((generation 1) (calls 0))
    (cl-letf (((symbol-function 'nelisp-dev-gc--snapshot)
               (lambda (_context)
                 (list :snapshot (nelisp-dev-gc-test--raw 1)
                       :session-id "change" :generation generation
                       :abi :available :reload-status 'ready :epoch 0)))
              ((symbol-function 'nelisp-dev-gc--reload-status)
               (lambda () (list :status 'ready :generation generation)))
              ((symbol-function 'nelisp-repl-gc-collect)
               (lambda ()
                 (setq calls (1+ calls) generation 2)
                 (list :before (nelisp-dev-gc-test--raw 1)
                       :after (nelisp-dev-gc-test--raw 2)
                       :collector-result 'collected))))
      (let ((response (nelisp-dev-gc-dispatch
                       (nelisp-dev-gc-test--request "gc.collect")
                       '(:target "native-linux-x86_64" :session-id "change"))))
        (should (= 1 calls))
        (should (equal "inconclusive" (nelisp-dev-gc-test--status response)))))))

(ert-deftest nelisp-dev-gc/unavailable-abi-is-inconclusive ()
  (nelisp-dev-gc-clear)
  (let ((raw (list :raw (list :abi-status :unavailable
                              :gc-stats (list :allocated-total 1))))
        (ctx '(:target "native-linux-x86_64" :session-id "abi" :generation 1)))
    (cl-letf (((symbol-function 'nelisp-repl-gc-snapshot) (lambda () raw)))
      (let* ((a (nelisp-dev-gc-dispatch (nelisp-dev-gc-test--request "gc.snapshot") ctx))
             (b (nelisp-dev-gc-dispatch (nelisp-dev-gc-test--request "gc.snapshot") ctx))
             (args (list (cons "before" (nelisp-dev-gc-test--data a "snapshot_id"))
                         (cons "after" (nelisp-dev-gc-test--data b "snapshot_id")))))
        (should (equal "inconclusive"
                       (nelisp-dev-gc-test--status
                        (nelisp-dev-gc-dispatch
                         (nelisp-dev-gc-test--request "gc.compare" args) ctx))))))))
