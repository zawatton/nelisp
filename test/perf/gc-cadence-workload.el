;;; gc-cadence-workload.el --- large restored-heap cadence probe -*- lexical-binding: t; -*-

;; No explicit collection, diagnostic policy override, or library image.
;; The host supplies the ELF's nl_gc_stats address from nm, never gdb.
;; GNU runs the same allocation and retained-data assertions without telemetry.
(defvar gc-cadence-stats (and (getenv "GC_STATS_ADDR")
                             (string-to-number (getenv "GC_STATS_ADDR"))))
(defvar gc-cadence-arena (and gc-cadence-stats (car (nelisp--arena-stats))))
(defvar gc-cadence-peak 0)
(defun gc-cadence-note-peak ()
  (when gc-cadence-arena
    (setq gc-cadence-peak
          (max gc-cadence-peak (ptr-read-u64 (+ gc-cadence-arena 728) 0)))))
(defun gc-cadence-snapshot (label)
  (when gc-cadence-stats
    (gc-cadence-note-peak)
    (princ (format "GC-STATS %s %d %d %d %d %d %d\n" label
                   (ptr-read-u64 gc-cadence-stats 24)
                   (+ (ptr-read-u64 gc-cadence-stats 32)
                      (ptr-read-u64 gc-cadence-stats 0))
                   (ptr-read-u64 gc-cadence-stats 16)
                   (ptr-read-u64 gc-cadence-stats 8)
                   gc-cadence-peak
                   (ptr-read-u64 (+ gc-cadence-arena 728) 0)))))

(defvar gc-cadence-live (make-vector 150 nil))
(dotimes (i 150)
  (aset gc-cadence-live i (make-string 1048576 120))
  (gc-cadence-note-peak))
(defvar gc-cadence-table (make-hash-table :test 'equal))
(dotimes (i 10000)
  (puthash (format "key-%d" i)
           (list i (number-to-string i) (make-vector 4 i)) gc-cadence-table)
  (gc-cadence-note-peak))
(gc-cadence-snapshot "start")
(defvar gc-cadence-checksum 0)
(dotimes (round 20)
  (let ((table (make-hash-table :test 'equal)))
    (dotimes (i 1000)
      (puthash (format "temp-%d" i) (concat (make-string 65536 121) "suffix") table)
      (gc-cadence-note-peak))
    (setq gc-cadence-checksum
          (+ gc-cadence-checksum (hash-table-count table)
             (length (gethash "temp-499" table)))))
  (gc-cadence-snapshot (number-to-string round)))
(unless (and (= (length gc-cadence-live) 150)
             (= (length (aref gc-cadence-live 149)) 1048576)
             (= (aref (aref gc-cadence-live 0) 1048575) 120)
             (equal (gethash "key-9999" gc-cadence-table) '(9999 "9999" [9999 9999 9999 9999]))
             (= gc-cadence-checksum 1330840))
  (error "Cadence workload lost live data"))
(gc-cadence-snapshot "end")
(princ (format "GC-DONE %d\n" gc-cadence-checksum))

;;; gc-cadence-workload.el ends here
