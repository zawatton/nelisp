;;; nelisp-repl-gc-test.el --- ERT for named REPL GC diagnostics -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)

(defvar nelisp-runtime-reload-gc-contract nil)
(defvar nelisp-runtime-reload-symbols nil)

(let ((file (expand-file-name "../lisp/nelisp-repl-gc.el"
                             (file-name-directory
                              (or load-file-name buffer-file-name)))))
  (load file nil t))

(defconst nelisp-repl-gc-test--arena
  '(100 200 300 400 500 600 700 0 0 1 200 180))

(defconst nelisp-repl-gc-test--debug
  '(0 0 0 0 0 0 0 2 0 0 3 4 5 6 7 0 0 0 0 0 0))

(ert-deftest nelisp-repl-gc/snapshot-is-observational ()
  "Snapshot reads the public records and does not request collection."
  (let ((gc-called nil))
    (cl-letf (((symbol-function 'nelisp--arena-stats)
               (lambda () nelisp-repl-gc-test--arena))
              ((symbol-function 'nelisp--debug-switch)
               (lambda (arg)
                 (should (= arg 0))
                 nelisp-repl-gc-test--debug))
              ((symbol-function 'garbage-collect)
               (lambda () (setq gc-called t))))
      (let* ((snapshot (nelisp-repl-gc-snapshot))
             (arena (plist-get snapshot :arena))
             (debug (plist-get snapshot :debug)))
        (should (eq (plist-get arena :status) :available))
        (should (= (plist-get arena :used-bytes) 400))
        (should (= (plist-get arena :chunk-count) 1))
        (should (= (plist-get debug :mid-form-fired) 2))
        (should (= (plist-get debug :reclaimed-bytes) 7))
        (should-not gc-called)))))

(ert-deftest nelisp-repl-gc/short-public-tuples-are-unavailable ()
  "Do not label a truncated native tuple as a valid snapshot."
  (cl-letf (((symbol-function 'nelisp--arena-stats)
             (lambda () '(1 2)))
            ((symbol-function 'nelisp--debug-switch)
             (lambda (_) '(1 2))))
    (let ((snapshot (nelisp-repl-gc-snapshot)))
      (should (eq (plist-get (plist-get snapshot :arena) :status)
                  :unavailable))
      (should (eq (plist-get (plist-get snapshot :debug) :status)
                  :unavailable)))))

(ert-deftest nelisp-repl-gc/compare-preserves-unavailable ()
  "Comparison reports numeric differences without inventing missing data."
  (let* ((before '(:timestamp-usec 10
                   :arena (:status :available :used-bytes 20)
                   :debug (:status :available :mid-form-fired 1)
                   :raw (:gc-stats (:status :unavailable))))
         (after '(:timestamp-usec 25
                  :arena (:status :available :used-bytes 32)
                  :debug (:status :available :mid-form-fired 4)
                  :raw (:gc-stats (:status :unavailable))))
         (delta (nelisp-repl-gc-compare before after)))
    (should (= (plist-get delta :timestamp-usec) 15))
    (should (= (plist-get (plist-get delta :arena) :used-bytes) 12))
    (should (= (plist-get (plist-get delta :debug) :mid-form-fired) 3))
    (should (eq (plist-get
                 (plist-get (plist-get delta :raw) :gc-stats)
                 :status)
                :unavailable))))

(ert-deftest nelisp-repl-gc/collect-has-before-after-and-timing ()
  "Collect explicitly requests GC and returns a measured transition."
  (let ((ticks '(100 100 200 200))
        (gc-called nil))
    (cl-letf (((symbol-function 'nl-unix-time-usec)
               (lambda () (prog1 (car ticks) (setq ticks (cdr ticks)))))
              ((symbol-function 'nelisp--arena-stats)
               (lambda () nelisp-repl-gc-test--arena))
              ((symbol-function 'nelisp--debug-switch)
               (lambda (_) nelisp-repl-gc-test--debug))
              ((symbol-function 'garbage-collect)
               (lambda () (setq gc-called t) '(mock-gc))))
      (let ((result (nelisp-repl-gc-collect)))
        (should gc-called)
        (should (equal (plist-get result :collector-result) '(mock-gc)))
        (should (= (plist-get result :elapsed-usec) 100))
        (should (= (plist-get (plist-get result :delta) :timestamp-usec) 100))
        (should (plist-member result :before))
        (should (plist-member result :after))))))

(ert-deftest nelisp-repl-gc/optional-raw-record-is-explicitly-unavailable ()
  "Normal host Emacs reports absent shared runtime exports honestly."
  (let* ((snapshot (nelisp-repl-gc-snapshot))
         (raw (plist-get snapshot :raw))
         (stats (plist-get raw :gc-stats)))
    (should (eq (plist-get stats :status) :unavailable))
    (should (memq (plist-get stats :reason)
                  '(:runtime-abi-unavailable :runtime-export-unavailable)))))

(ert-deftest nelisp-repl-gc/retention-is-explicit-and-contract-is-checked-once ()
  "Expose retention evidence without inferring unpublished counters."
  (let ((checks 0))
    (cl-letf (((symbol-function 'nelisp-runtime-reload-contract-matches-p)
               (lambda () (setq checks (1+ checks)) t))
              ((symbol-function 'nelisp-native-load--raw-symbol-addr)
               (lambda (_) 4096))
              ((symbol-function 'ptr-read-u64)
               (lambda (_ _offset) 0)))
      (cl-letf (((symbol-value 'nelisp-runtime-reload-gc-contract) t)
                ((symbol-value 'nelisp-runtime-reload-symbols) t))
        (let* ((snapshot (nelisp-repl-gc-snapshot))
               (raw (plist-get snapshot :raw))
               (retention (plist-get snapshot :retention)))
          (should (= checks 1))
          (should (eq (plist-get raw :abi-status) :available))
          (should-not (plist-get retention :conservative-pin-present))
          (should (eq (plist-get retention :individual-root-paths)
                      :unavailable))
          (should (eq (plist-get retention :compaction-skipped-for-pins)
                      :unavailable)))))))

(ert-deftest nelisp-repl-gc/retention-uses-last-collection-record ()
  "Use telemetry's post-clear record rather than cleared working state."
  (let* ((raw '(:runtime-reload-state
                (:status :available
                 :last-conservative-scanned-bytes 8192
                 :last-conservative-pinned-blocks 7
                 :last-conservative-flags 4
                 :conservative-completed-attempts 3)))
         (retention (nelisp-repl-gc--retention raw)))
    (should (eq (plist-get retention :conservative-pin-present) t))
    (should (= (plist-get retention :conservative-scanned-bytes) 8192))
    (should (= (plist-get retention :conservative-pinned-blocks) 7))
    (should (= (plist-get retention :conservative-completed-attempts) 3))
    (should (eq (plist-get retention :individual-root-paths) :unavailable))))

;;; nelisp-repl-gc-test.el ends here
