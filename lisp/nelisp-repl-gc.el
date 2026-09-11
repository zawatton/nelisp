;;; nelisp-repl-gc.el --- named GC snapshots for a live NeLisp REPL -*- lexical-binding: t; -*-

;;; Commentary:

;; This is deliberately a read-only diagnostic layer.  A snapshot reads the
;; public arena/debug tuples and, when the development raw-symbol resolver is
;; present, the explicitly documented GC records.  It never calls GC.  The
;; collect entry point is the only operation here that requests a collection.
;; Missing development exports are represented as :unavailable instead of
;; being guessed from an arena address or a build environment variable.

;;; Code:

(defconst nelisp-repl-gc--arena-fields
  '(:base :size :bump-offset :used-bytes :live-after-last-gc :next-trigger
    :free-list-head :collect-disabled :reuse-disabled :chunk-count
    :chunk-bytes-reserved :chunk-bytes-used))

(defconst nelisp-repl-gc--debug-fields
  '(:trip-count :bad-cur :bad-bt :bad-want :poison-count :poison-enabled
    :context-depth :mid-form-fired :bind-legacy-force :root-depth
    :bucket-hits :linear-hits :bump-allocations :reclaimed-chunks
    :reclaimed-bytes :failed-os-releases :registered-workers
    :current-parked :last-parked :missed-collects
    :successful-parked-collects))

(defconst nelisp-repl-gc--stats-fields
  '((:allocated-since-gc . 0)
    (:debt-threshold . 8)
    (:live-bytes . 16)
    (:collections . 24)
    (:allocated-total . 32)
    (:debt-floor . 40)
    (:debt-percent . 48)))

(defconst nelisp-repl-gc--conservative-fields
  '((:queue-base . 0)
    (:queue-capacity . 8)
    (:queue-head . 16)
    (:queue-tail . 24)
    (:flags . 32)
    (:scanned-bytes . 40)
    (:pinned-blocks . 48)
    (:cached-owner . 56)))

(defconst nelisp-repl-gc--alloc-fields
  '((:enabled . 0)
    (:bucket-allocations . 8)
    (:linear-allocations . 16)
    (:bump-allocations . 24)
    (:reclaimed-chunks . 32)
    (:reclaimed-bytes . 40)
    (:failed-os-releases . 48)))

(defconst nelisp-repl-gc--reload-state-fields
  '((:last-conservative-scanned-bytes . 64)
    (:last-conservative-pinned-blocks . 72)
    (:last-conservative-flags . 80)
    (:conservative-completed-attempts . 88)))

(defun nelisp-repl-gc--time-usec ()
  "Return a wall-clock microsecond reading, or nil when unavailable."
  (when (fboundp 'nl-unix-time-usec)
    (condition-case nil
        (nl-unix-time-usec)
      (error nil))))

(defun nelisp-repl-gc--plist-from-list (keys values)
  "Pair KEYS with VALUES and return a named plist."
  (let ((out nil)
        (ks keys)
        (vs values))
    (while ks
      (setq out (append out (list (car ks) (car vs))))
      (setq ks (cdr ks)
            vs (cdr vs)))
    out))

(defun nelisp-repl-gc--arena ()
  "Read the public arena tuple without requesting a collection."
  (if (not (fboundp 'nelisp--arena-stats))
      (list :status :unavailable :reason :arena-stats-unavailable)
    (condition-case err
        (let ((values (nelisp--arena-stats)))
          (if (and (listp values)
                   (= (length values) (length nelisp-repl-gc--arena-fields)))
              (append (list :status :available)
                      (nelisp-repl-gc--plist-from-list
                       nelisp-repl-gc--arena-fields values))
            (list :status :unavailable :reason :invalid-arena-stats
                  :value values)))
      (error (list :status :unavailable :reason :arena-stats-error
                   :error err)))))

(defun nelisp-repl-gc--debug ()
  "Read the public debug tuple without changing diagnostic switches."
  (if (not (fboundp 'nelisp--debug-switch))
      (list :status :unavailable :reason :debug-switch-unavailable)
    (condition-case err
        (let ((values (nelisp--debug-switch 0)))
          (if (and (listp values)
                   (= (length values) (length nelisp-repl-gc--debug-fields)))
              (append (list :status :available)
                      (nelisp-repl-gc--plist-from-list
                       nelisp-repl-gc--debug-fields values))
            (list :status :unavailable :reason :invalid-debug-stats
                  :value values)))
      (error (list :status :unavailable :reason :debug-switch-error
                   :error err)))))

(defun nelisp-repl-gc--raw-address (name)
  "Resolve NAME through the existing named raw-runtime resolver, if present."
  (cond
   ((fboundp 'nelisp-native-load--raw-symbol-addr)
    (condition-case nil
        (nelisp-native-load--raw-symbol-addr name)
      (error nil)))
   (t nil)))

(defun nelisp-repl-gc--raw-contract-ready-p ()
  "Return non-nil only when the shared runtime ABI is positively verified."
  (and (boundp 'nelisp-runtime-reload-gc-contract)
       (boundp 'nelisp-runtime-reload-symbols)
       (fboundp 'nelisp-runtime-reload-contract-matches-p)
       (condition-case nil
           (nelisp-runtime-reload-contract-matches-p)
         (error nil))))

(defun nelisp-repl-gc--raw-record (name fields contract-ready)
  "Read documented u64 FIELDS from raw record NAME, or mark unavailable."
  (let ((address (and contract-ready
                      (nelisp-repl-gc--raw-address name))))
    (if (not (and (integerp address) (> address 0)
                  (fboundp 'ptr-read-u64)))
        (list :status :unavailable
              :reason (if contract-ready
                          :runtime-export-unavailable
                        :runtime-abi-unavailable)
              :symbol name)
      (condition-case err
          (let ((out (list :status :available :symbol name
                           :address address))
                (rest fields))
            (while rest
              (setq out (append out
                                (list (car (car rest))
                                      (ptr-read-u64 address (cdr (car rest)))))
                    rest (cdr rest)))
            (when (equal name "nl_gc_conserv_state")
              (let ((flags (plist-get out :flags)))
                (setq out (append out
                                  (list :active (= (logand flags 1) 1)
                                        :oom (= (logand flags 2) 2)
                                        :has-pin (= (logand flags 4) 4))))))
            out)
        (error (list :status :unavailable :reason :runtime-read-error
                     :symbol name :error err))))))

(defun nelisp-repl-gc--raw-state ()
  "Read the optional shared GC records exposed by the development ABI."
  (let ((contract-ready (nelisp-repl-gc--raw-contract-ready-p)))
    (list
     :abi-status (if contract-ready :available :unavailable)
     :gc-stats (nelisp-repl-gc--raw-record "nl_gc_stats"
                                           nelisp-repl-gc--stats-fields
                                           contract-ready)
     :conservative (nelisp-repl-gc--raw-record
                    "nl_gc_conserv_state"
                    nelisp-repl-gc--conservative-fields
                    contract-ready)
     :allocation (nelisp-repl-gc--raw-record "nl_alloc_diag"
                                              nelisp-repl-gc--alloc-fields
                                              contract-ready)
     :runtime-reload-state
     (nelisp-repl-gc--raw-record "nl_runtime_reload_state"
                                  nelisp-repl-gc--reload-state-fields
                                  contract-ready))))

(defun nelisp-repl-gc--retention (raw)
  "Describe last-GC retention evidence available in RAW.

Individual root paths and a compaction-skip counter are not part of the
published ABI, so they stay explicitly unavailable instead of being inferred
from a flag or a nonzero block count."
  (let* ((last (plist-get raw :runtime-reload-state))
         (available (eq (plist-get last :status) :available))
         (flags (and available
                     (plist-get last :last-conservative-flags)))
         (pinned (and available
                      (plist-get last :last-conservative-pinned-blocks)))
         (scanned (and available
                       (plist-get last :last-conservative-scanned-bytes)))
         (attempts (and available
                        (plist-get last :conservative-completed-attempts)))
         (collection-status
          (cond ((not available) :unavailable)
                ((and (integerp attempts) (> attempts 0)) :available)
                (t :not-collected))))
    (list :last-collection-status collection-status
          :conservative-pin-present
          (if available (and (integerp flags) (= (logand flags 4) 4))
            :unavailable)
          :conservative-pinned-blocks (or pinned :unavailable)
          :conservative-scanned-bytes (or scanned :unavailable)
          :conservative-completed-attempts (or attempts :unavailable)
          :individual-root-paths :unavailable
          :compaction-skipped-for-pins :unavailable
          :compaction-skipped-for-pins-reason :counter-not-published)))

(defun nelisp-repl-gc-snapshot ()
  "Return a named snapshot of current REPL GC state.

This function is observational: it does not call `garbage-collect' or any
debug switch that changes runtime state.  Optional raw records report
`:status :unavailable' when the running reader does not publish the shared
development ABI.  Building the named result can itself allocate and may
therefore trigger the runtime's automatic GC; the result is not an atomic
stop-the-world sample." 
  (let ((raw (nelisp-repl-gc--raw-state)))
    (list :timestamp-usec (nelisp-repl-gc--time-usec)
          :arena (nelisp-repl-gc--arena)
          :debug (nelisp-repl-gc--debug)
          :raw raw
          :retention (nelisp-repl-gc--retention raw))))

(defun nelisp-repl-gc--numeric-delta (before after)
  "Return AFTER minus BEFORE when both values are numbers, else unavailable."
  (if (and (numberp before) (numberp after))
      (- after before)
    :unavailable))

(defun nelisp-repl-gc--compare-record (before after)
  "Return numeric leaf deltas between two named record plists."
  (let ((out nil)
        (rest after))
    (while rest
      (let* ((key (car rest))
             (new (cadr rest))
             (old (plist-get before key)))
        (when (keywordp key)
          (setq out (append out
                            (list key
                                  (if (and (numberp old) (numberp new))
                                      (- new old)
                                    :unavailable)))))
        (setq rest (cddr rest))))
    out))

(defun nelisp-repl-gc-compare (before after)
  "Return named deltas from snapshot BEFORE to snapshot AFTER.

Unavailable or nonnumeric fields remain `:unavailable'; this avoids treating
an absent native counter as zero." 
  (list :timestamp-usec
        (nelisp-repl-gc--numeric-delta
         (plist-get before :timestamp-usec)
         (plist-get after :timestamp-usec))
        :arena (nelisp-repl-gc--compare-record
                (plist-get before :arena) (plist-get after :arena))
        :debug (nelisp-repl-gc--compare-record
                (plist-get before :debug) (plist-get after :debug))
        :retention (nelisp-repl-gc--compare-record
                    (plist-get before :retention)
                    (plist-get after :retention))
        :raw (list
              :gc-stats (nelisp-repl-gc--compare-record
                         (plist-get (plist-get before :raw) :gc-stats)
                         (plist-get (plist-get after :raw) :gc-stats))
              :conservative (nelisp-repl-gc--compare-record
                             (plist-get (plist-get before :raw) :conservative)
                             (plist-get (plist-get after :raw) :conservative))
              :allocation (nelisp-repl-gc--compare-record
                           (plist-get (plist-get before :raw) :allocation)
                           (plist-get (plist-get after :raw) :allocation))
              :runtime-reload-state
              (nelisp-repl-gc--compare-record
               (plist-get (plist-get before :raw) :runtime-reload-state)
               (plist-get (plist-get after :raw) :runtime-reload-state)))))

(defun nelisp-repl-gc-collect ()
  "Collect explicitly and return before/after snapshots plus named deltas.

`:elapsed-usec' measures the complete call around the explicit collection and
diagnostic snapshots; it is not a strict stop-the-world pause measurement."
  (let* ((before (nelisp-repl-gc-snapshot))
         (started (nelisp-repl-gc--time-usec))
         (result (if (fboundp 'garbage-collect)
                     (garbage-collect)
                   :unavailable))
         (finished (nelisp-repl-gc--time-usec))
         (after (nelisp-repl-gc-snapshot)))
    (list :before before
          :after after
          :delta (nelisp-repl-gc-compare before after)
          :elapsed-usec (nelisp-repl-gc--numeric-delta started finished)
          :collector-result result)))

(provide 'nelisp-repl-gc)
;;; nelisp-repl-gc.el ends here
