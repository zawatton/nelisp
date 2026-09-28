;;; nelisp-eln-objects-standalone-smoke.el --- GNU cons owner probe -*- lexical-binding: t; -*-

(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (root (or (getenv "NELISP_ELN_MODULE_ROOT")
                 (getenv "NELISP_ROOT")
                 (file-name-directory test-dir))))
  (add-to-list 'load-path (expand-file-name "lisp" root))
  (add-to-list 'load-path (expand-file-name "packages/nl-ffi/src" root))
  (let ((objects-source (getenv "NELISP_ELN_OBJECTS_SOURCE")))
    (if (and objects-source (file-readable-p objects-source))
        (load objects-source nil t t)
      (require 'nelisp-eln-objects))))

(let* ((unit (nelisp-eln-objects-create))
       (graph (let ((shared (cons nil nil)))
                (setcdr shared shared)
                (cons nil shared)))
       (root-word (nelisp-eln-objects-encode unit graph))
       (root-address (- root-word 3))
       (root-record nil)
       (shared-record nil))
  ;; Drop the only direct graph variable; the registry-held owner must retain it.
  (setq graph nil)
  (garbage-collect)
  (let ((records (aref (nelisp-eln-objects--resolve unit) 3)))
    (while records
      (cond ((= (nth 1 (car records)) root-address)
             (setq root-record (car records)))
            ((= (ptr-read-u64 root-address 8)
                (+ 3 (nth 1 (car records))))
             (setq shared-record (car records))))
      (setq records (cdr records))))
  (unless (and root-record shared-record
               (= root-word (nelisp-eln-objects-encode
                             unit (car root-record)))
               (eq (nelisp-eln-objects-decode unit root-word)
                   (car root-record))
               (null (nelisp-eln-objects-decode unit
                                                (nelisp-eln-abi-encode-nil)))
               (= 17 (nelisp-eln-objects-decode
                      unit (nelisp-eln-abi-encode-fixnum 17)))
               (= root-address (nth 1 root-record))
               (= (ptr-read-u64 root-address 8)
                  (+ 3 (nth 1 shared-record)))
               (= (ptr-read-u64 (nth 1 shared-record) 8)
                  (+ 3 (nth 1 shared-record))))
    (error "GNU cons identity/sharing/cycle was not stable across GC"))
  (unless (condition-case nil
              (progn (nelisp-eln-objects-decode unit 4099) nil)
            (nelisp-eln-objects-error t))
    (error "Decoder accepted a foreign cons address"))
  ;; All edges are staged before canonical mutation: invalid unknown pointer
  ;; in the other node must leave this node's original car intact.
  (ptr-write-u64 root-address 0 (nelisp-eln-abi-encode-fixnum 42))
  (unless (and (= root-word (nelisp-eln-objects-encode
                             unit (car root-record)))
               (= (nelisp-eln-abi-encode-fixnum 42)
                  (ptr-read-u64 root-address 0)))
    (error "Repeated encode overwrote an unsynchronized native edge"))
  (ptr-write-u64 (nth 1 shared-record) 8 4099)
  (unless (condition-case nil
              (progn (nelisp-eln-objects-sync-from-native unit) nil)
            (nelisp-eln-objects-error t))
    (error "Unknown external GNU pointer was accepted"))
  (unless (null (car (nth 0 root-record)))
    (error "Failed sync partially changed canonical graph"))
  (ptr-write-u64 (nth 1 shared-record) 8 (+ 3 (nth 1 shared-record)))
  (unless (nelisp-eln-objects-sync-from-native unit)
    (error "Owned native edges failed to synchronize"))
  (unless (and (= 42 (car (nth 0 root-record)))
               (eq (cdr (nth 0 root-record)) (nth 0 shared-record))
               (eq (cdr (nth 0 shared-record)) (nth 0 shared-record)))
    (error "Canonical cons graph did not preserve identity after sync"))
  (setcar (nth 0 root-record) 17)
  (nelisp-eln-objects-sync-to-native unit)
  (unless (= (nelisp-eln-abi-encode-fixnum 17)
             (ptr-read-u64 root-address 0))
    (error "Explicit sync-to-native did not publish canonical mutation"))
  (let ((bad-unit (nelisp-eln-objects-create)))
    (unless (condition-case nil
                (progn (nelisp-eln-objects-encode bad-unit [unsupported]) nil)
              (nelisp-eln-objects-unsupported t))
      (error "Unsupported graph input was accepted"))
    (unless (null (aref (nelisp-eln-objects--resolve bad-unit) 2))
      (error "Unsupported graph allocated before preflight completed"))
    (nelisp-eln-objects-release bad-unit))
  (nelisp-eln-objects-release unit)
  (unless (condition-case nil
              (progn (nelisp-eln-objects-encode unit 0) nil)
            (nelisp-eln-objects-error t))
    (error "Released unit handle remained usable"))
  (unless (condition-case nil
              (progn (nelisp-eln-objects-decode unit root-word) nil)
            (nelisp-eln-objects-error t))
    (error "Decoder accepted a released unit handle"))
  t)

(let* ((ascii (copy-sequence "hello"))
       (shared (copy-sequence (unibyte-string 65 0 128 255)))
       (equal-but-distinct (copy-sequence shared))
       (graph (let ((node (cons shared nil)))
                (setcdr node (cons equal-but-distinct node))
                (cons ascii (cons shared (cons shared node)))))
       (unit (nelisp-eln-objects-create))
       (word (nelisp-eln-objects-encode unit graph))
       (root-address (- word 3))
       (ascii-address (aref (nelisp-eln-objects--string-record
                             (nelisp-eln-objects--resolve unit) ascii) 2))
       (shared-address (aref (nelisp-eln-objects--string-record
                              (nelisp-eln-objects--resolve unit) shared) 2))
       (distinct-address (aref (nelisp-eln-objects--string-record
                                (nelisp-eln-objects--resolve unit)
                                equal-but-distinct) 2)))
  (unless (and (/= shared-address distinct-address)
               (eq (nelisp-eln-objects-decode unit (+ shared-address 4)) shared)
               (eq (nelisp-eln-objects-decode unit (+ distinct-address 4))
                   equal-but-distinct))
    (error "String identity map collapsed equal-but-distinct strings"))
  (setq graph nil ascii nil shared nil equal-but-distinct nil)
  (garbage-collect)
  (let* ((unit-data (nelisp-eln-objects--resolve unit))
         (root-record (nelisp-eln-objects--record unit-data
                                                  (nelisp-eln-objects-decode
                                                   unit word)))
         (distinct-canonical
          (nelisp-eln-objects-decode unit (+ distinct-address 4)))
         (ascii-canonical
          (nelisp-eln-objects-decode unit (+ ascii-address 4)))
         (native-ascii (+ ascii-address 32))
         (native-shared (+ shared-address 32)))
    (unless (and root-record
                 (= (ptr-read-u64 root-address 0) (+ ascii-address 4))
                 (= (ptr-read-u8 native-ascii 0) ?h)
                 (= (ptr-read-u8 native-shared 0) 65)
                 (= (ptr-read-u8 native-shared 2) 128)
                 (= (ptr-read-u8 native-shared 3) 255))
      (error "Mixed graph lost string descriptor or bytes across GC"))
    (let* ((root-object (car root-record))
           (first-shared-node (cdr root-object))
           (second-shared-node (cdr first-shared-node))
           (cycle-node (cdr second-shared-node))
           (distinct-node (cdr cycle-node)))
      (unless (and (eq (car root-object) ascii-canonical)
                   (eq (car first-shared-node) (car second-shared-node))
                   (eq (car second-shared-node) (car cycle-node))
                   (eq (car distinct-node) distinct-canonical)
                   (eq (cdr distinct-node) cycle-node))
        (error "Mixed graph lost shared string or cyclic cons identity")))
    ;; Mutate the unibyte string in native memory, stage the entire graph, and
    ;; synchronize without changing the canonical object's identity.
    (ptr-write-u8 native-ascii 0 ?H)
    (ptr-write-u8 native-shared 0 66)
    (unless (nelisp-eln-objects-sync-from-native unit)
      (error "Mixed native-to-canonical synchronization failed"))
    (let ((canonical (nelisp-eln-objects-decode unit (+ shared-address 4))))
      (unless (and (eq canonical (car (cdr (car root-record))))
                   (= (aref canonical 0) 66))
        (error "Native string mutation did not reach canonical string"))
      (unless (and (eq ascii-canonical (car (car root-record)))
                   (= (aref ascii-canonical 0) ?H))
        (error "Native ASCII mutation did not reach canonical string"))
      (aset canonical 1 127)
      (aset ascii-canonical 1 ?A)
      (nelisp-eln-objects-sync-to-native unit)
      (unless (and (= (ptr-read-u8 native-shared 1) 127)
                   (= (ptr-read-u8 native-ascii 1) ?A))
        (error "Canonical string mutation did not reach native view"))))
  (nelisp-eln-objects-release unit))

(let* ((unit (nelisp-eln-objects-create))
       (mapping (nl-ffi-memory-allocate 8))
       (address (nl-ffi-memory-address mapping))
       ;; Literal GNU x86_64 low/high halves, independent of codec arithmetic.
       (cases (list (list nelisp-eln-abi-fixnum-min 2 2147483648
                          9223372036854775810)
                    (list -1 4294967294 4294967295
                          18446744073709551614)
                    (list nelisp-eln-abi-fixnum-max 4294967294 2147483647
                          9223372036854775806))))
  (dolist (case cases)
    (let ((value (nth 0 case))
          (expected-low (nth 1 case))
          (expected-high (nth 2 case))
          (expected-word (nth 3 case)))
      (nelisp-eln-objects--write-word
       address 0 (nelisp-eln-abi-encode-fixnum value))
      (unless (and (= (ptr-read-u32 address 0) expected-low)
                   (= (ptr-read-u32 address 4) expected-high)
                   (= (nelisp-eln-abi-normalize-word
                       (nelisp-eln-objects--read-word address 0))
                      expected-word)
                   (= (nelisp-eln-objects-decode
                       unit (nelisp-eln-objects--read-word address 0)) value))
        (error "Mapped GNU fixnum boundary mismatch: %S expected %S actual %S"
               value (list expected-low expected-high expected-word)
               (list (ptr-read-u32 address 0) (ptr-read-u32 address 4))))))
  (nl-ffi-memory-release mapping)
  (nelisp-eln-objects-release unit))

;; Exercise the two-half reader through the real unit graph boundary too.
(let* ((unit (nelisp-eln-objects-create))
       (graph (cons nelisp-eln-abi-fixnum-min
                    nelisp-eln-abi-fixnum-max))
       (root-word (nelisp-eln-objects-encode unit graph))
       (root-address (- root-word 3)))
  (unless (and (= (ptr-read-u32 root-address 0) 2)
               (= (ptr-read-u32 root-address 4) 2147483648)
               (= (ptr-read-u32 root-address 8) 4294967294)
               (= (ptr-read-u32 root-address 12) 2147483647))
    (error "GNU cons view did not contain literal fixnum boundary words"))
  (setcar graph 0)
  (setcdr graph 0)
  (nelisp-eln-objects-sync-from-native unit)
  (unless (and (= (car graph) nelisp-eln-abi-fixnum-min)
               (= (cdr graph) nelisp-eln-abi-fixnum-max))
    (error "Native-to-canonical sync lost fixnum boundary values"))
  (nelisp-eln-objects-release unit))

(unless (= (nelisp-eln-abi-normalize-word 9223372036854775810)
           9223372036854775810)
  (error "Unsigned high-bit word was narrowed in standalone normalization"))
(unless (condition-case nil
            (progn (nelisp-eln-abi-normalize-word 18446744073709551616) nil)
          (nelisp-eln-abi-error t))
  (error "Word above unsigned 64-bit range was accepted"))

(let ((unit (nelisp-eln-objects-create))
      (good (copy-sequence "ok"))
      (bad (copy-sequence "bad"))
      (original-prepare (symbol-function 'nelisp-eln-string-prepare))
      (failed nil)
      (old-word nil)
      (old-byte nil)
      (string-record nil)
      (cons-record nil))
  (unwind-protect
      (progn
        (nelisp-eln-objects-encode unit (cons good nil))
        (setq string-record
              (nelisp-eln-objects--string-record
               (nelisp-eln-objects--resolve unit) good)
              cons-record
              (nelisp-eln-objects--record
               (nelisp-eln-objects--resolve unit)
               (car (aref (nelisp-eln-objects--resolve unit) 4)))
              old-word (ptr-read-u64 (nth 1 cons-record) 0)
              old-byte (ptr-read-u8 (+ (aref string-record 2) 32) 0))
        (fset 'nelisp-eln-string-prepare
              (lambda (string)
                (if (eq string bad)
                    (signal 'nelisp-eln-string-error '(injected-preflight))
                  (funcall original-prepare string))))
        (setq failed
              (condition-case nil
                  (progn (nelisp-eln-objects-encode unit (cons good bad)) nil)
                (nelisp-eln-string-error t)))
        (unless (and failed
                     (= (length (aref (nelisp-eln-objects--resolve unit) 3)) 1)
                     (= (length (aref (nelisp-eln-objects--resolve unit) 5)) 1)
                     (= old-word (ptr-read-u64 (nth 1 cons-record) 0))
                     (= old-byte (ptr-read-u8 (+ (aref string-record 2) 32) 0)))
          (error "Invalid later string changed existing mixed graph state")))
    (fset 'nelisp-eln-string-prepare original-prepare)
    (nelisp-eln-objects-release unit)))

(let* ((unit (nelisp-eln-objects-create))
       (original-allocate
        (symbol-function 'nelisp-eln-objects--allocate-view-memory))
       (failed nil))
  (unwind-protect
      (progn
        (fset 'nelisp-eln-objects--allocate-view-memory
              (lambda (_bytes) (error "injected mmap allocation failure")))
        (setq failed
              (condition-case nil
                  (progn (nelisp-eln-objects-encode unit (cons 1 nil)) nil)
                (error t)))
        (unless (and failed
                     (null (aref (nelisp-eln-objects--resolve unit) 2))
                     (null (aref (nelisp-eln-objects--resolve unit) 3))
                     (null (aref (nelisp-eln-objects--resolve unit) 4)))
          (error "Allocation failure left partial unit state")))
    (fset 'nelisp-eln-objects--allocate-view-memory original-allocate)
    (nelisp-eln-objects-release unit)))

(let* ((unit (nelisp-eln-objects-create))
       (original-write (symbol-function 'nelisp-eln-objects--write-word))
       (writes 0)
       (failed nil))
  (unwind-protect
      (progn
        (fset 'nelisp-eln-objects--write-word
              (lambda (_address _offset _word)
                (setq writes (1+ writes))
                (if (= writes 2)
                    (error "injected second word write failure")
                  t)))
        (setq failed
              (condition-case nil
                  (progn (nelisp-eln-objects-encode unit (cons 1 nil)) nil)
                (error t)))
        (unless (and failed (= writes 2)
                     (null (aref (nelisp-eln-objects--resolve unit) 2))
                     (null (aref (nelisp-eln-objects--resolve unit) 3))
                     (null (aref (nelisp-eln-objects--resolve unit) 4)))
          (error "Failed new-view initialization was not rolled back")))
    (fset 'nelisp-eln-objects--write-word original-write)
    (nelisp-eln-objects-release unit)))

(let* ((unit (nelisp-eln-objects-create))
       (old (cons 11 nil))
       (root (cons nil old))
       (_root-word (nelisp-eln-objects-encode unit root))
       (new (cons 22 nil))
       (old-record (nelisp-eln-objects--record
                    (nelisp-eln-objects--resolve unit) old))
       (original-write (symbol-function 'nelisp-eln-objects--write-word))
       (writes 0)
       (failed nil))
  (setcar old new)
  (unwind-protect
      (progn
        ;; New mapping is written first, then old nodes. Fail after the old
        ;; node's car has been changed to reference the new mapping.
        (fset 'nelisp-eln-objects--write-word
              (lambda (address offset word)
                (setq writes (1+ writes))
                (if (= writes 6)
                    (error "injected sync write failure after pointer publish")
                  (funcall original-write address offset word))))
        (setq failed
              (condition-case nil
                  (progn (nelisp-eln-objects-sync-to-native unit) nil)
                (error t)))
        (unless (and failed (= writes 6)
                     (= (ptr-read-u64 (nth 1 old-record) 0)
                        (+ 3 (nth 1 (nelisp-eln-objects--record
                                     (nelisp-eln-objects--resolve unit) new))))
                     (eq (aref (nelisp-eln-objects--resolve unit) 1)
                         'releasing)
                     (condition-case nil
                         (progn (nelisp-eln-objects-encode unit 0) nil)
                       (nelisp-eln-objects-error t))
                     (condition-case nil
                         (progn (nelisp-eln-objects-decode unit 0) nil)
                       (nelisp-eln-objects-error t))
                     (condition-case nil
                         (progn (nelisp-eln-objects-sync-from-native unit) nil)
                       (nelisp-eln-objects-error t)))
          (error "Failed sync did not retain and quarantine published mapping"))
        (nelisp-eln-objects-release unit)
        (unless (condition-case nil
                    (progn (nelisp-eln-objects-sync-to-native unit) nil)
                  (nelisp-eln-objects-error t))
          (error "Released failed-sync unit remained usable")))
    (fset 'nelisp-eln-objects--write-word original-write)
    (when (condition-case nil
              (nelisp-eln-objects--resolve unit)
            (error nil))
      (nelisp-eln-objects-release unit))))

(let* ((unit (nelisp-eln-objects-create))
       (child (cons 11 nil))
       (root (cons nil child))
       (root-word (nelisp-eln-objects-encode unit root))
       (root-address (- root-word 3))
       (child-word (ptr-read-u64 root-address 8))
       (child-address (- child-word 3))
       (old-root-cdr (ptr-read-u64 root-address 8))
       (old-child-car (ptr-read-u64 child-address 0)))
  ;; The child is now orphaned, but is still among the records that an
  ;; explicit full sync would otherwise write.
  (setcdr root nil)
  (setcar child [unsupported-orphan-edge])
  (unless (condition-case nil
              (progn (nelisp-eln-objects-sync-to-native unit) nil)
            (nelisp-eln-objects-unsupported t))
    (error "Unsupported detached record edge was accepted"))
  (unless (and (= old-root-cdr (ptr-read-u64 root-address 8))
               (= old-child-car (ptr-read-u64 child-address 0)))
    (error "Invalid orphan graph caused partial native writes"))
  (nelisp-eln-objects-release unit))

(let* ((unit (nelisp-eln-objects-create))
       (original-allocate
        (symbol-function 'nelisp-eln-string-allocate-prepared))
       (original-release (symbol-function 'nelisp-eln-string-release))
       (allocated nil)
       (calls 0)
       (failure nil))
  (unwind-protect
      (progn
        (fset 'nelisp-eln-string-allocate-prepared
              (lambda (plan)
                (setq calls (1+ calls))
                (if (= calls 2)
                    (signal 'nelisp-eln-string-error
                            '(injected-second-allocation))
                  (let ((owner (funcall original-allocate plan)))
                    (push owner allocated)
                    owner))))
        (fset 'nelisp-eln-string-release
              (lambda (_owner) (error "injected cleanup unmap failure")))
        (setq failure
              (condition-case nil
                  (nelisp-eln-objects-encode
                   unit (cons (copy-sequence "one") (copy-sequence "two")))
                (nelisp-eln-string-error 42)
                (error 99)))
        (unless (and (= failure 42)
                     (= calls 2) (= (length allocated) 1)
                     (null (aref (nelisp-eln-objects--resolve unit) 2))
                     (null (aref (nelisp-eln-objects--resolve unit) 3))
                     (= (length (aref (nelisp-eln-objects--resolve unit) 5)) 1)
                     (eq (aref (nelisp-eln-objects--resolve unit) 1) 'releasing)
                     (= (nelisp-eln-string-address (car allocated))
                        (aref (car (aref (nelisp-eln-objects--resolve unit) 5)) 2)))
          (error "Failed cleanup did not preserve exact error and retryable owner")))
    (fset 'nelisp-eln-string-allocate-prepared original-allocate)
    (fset 'nelisp-eln-string-release original-release)
    (nelisp-eln-objects-release unit)))

(let ((unit (nelisp-eln-objects-create)))
  (unless (condition-case nil
              (progn (nelisp-eln-objects-decode unit (+ 4096 4)) nil)
            (nelisp-eln-objects-error t))
    (error "Decoder accepted a foreign tag-4 string pointer"))
  (nelisp-eln-objects-release unit))

(let* ((unit (nelisp-eln-objects-create))
       (string (copy-sequence "owned"))
       (root (cons string nil))
       (root-word (nelisp-eln-objects-encode unit root))
       (root-address (- root-word 3))
       (old-native-car (ptr-read-u64 root-address 0))
       (original-prepare (symbol-function 'nelisp-eln-string-prepare))
       (original-allocate (symbol-function 'nelisp-eln-objects--allocate-view-memory))
       (allocations 0)
       (failure nil))
  (setcar root (cons 1 nil))
  (unwind-protect
      (progn
        (fset 'nelisp-eln-string-prepare
              (lambda (candidate)
                (if (eq candidate string)
                    (signal 'nelisp-eln-string-error '(orphan-preflight))
                  (funcall original-prepare candidate))))
        (fset 'nelisp-eln-objects--allocate-view-memory
              (lambda (bytes)
                (setq allocations (1+ allocations))
                (funcall original-allocate bytes)))
        (setq failure
              (condition-case nil
                  (nelisp-eln-objects-sync-to-native unit)
                (nelisp-eln-string-error 42)
                (error 99)))
        (unless (and (= failure 42)
                     (= allocations 0)
                     (= (length (aref (nelisp-eln-objects--resolve unit) 3)) 1)
                     (= old-native-car (ptr-read-u64 root-address 0)))
          (error "Invalid orphan string was not rejected before graph allocation")))
    (fset 'nelisp-eln-string-prepare original-prepare)
    (fset 'nelisp-eln-objects--allocate-view-memory original-allocate)
    (nelisp-eln-objects-release unit)))

;; Distinct units lease the same canonical cons/string objects by identity;
;; activations keep their original graph views alive after both units close.
(let* ((unit-a (nelisp-eln-objects-create))
       (unit-b (nelisp-eln-objects-create))
       (shared-string (copy-sequence "same"))
       (equal-distinct (copy-sequence "same"))
       (cycle (cons shared-string nil))
       (distinct-node (cons equal-distinct nil))
       (graph (cons cycle distinct-node))
       (word-a (nelisp-eln-objects-encode unit-a graph))
       (word-b (nelisp-eln-objects-encode unit-b graph))
       (root-address (- word-a 3))
       (cycle-word (nelisp-eln-abi-read-word root-address 0))
       (distinct-word (nelisp-eln-abi-read-word root-address 8))
       (cycle-address (- cycle-word 3))
       (distinct-address (- distinct-word 3))
       (shared-word (nelisp-eln-abi-read-word cycle-address 0))
       (distinct-string-word (nelisp-eln-abi-read-word distinct-address 0))
       (token (nelisp-eln-objects-activation-acquire unit-a))
       (new-leaf (cons 93 nil))
       (new-word nil))
  (setcdr cycle cycle)
  (unless (and (= word-a word-b)
               (= cycle-word (nelisp-eln-abi-read-word root-address 0))
               (/= shared-word distinct-string-word)
               (eq (nelisp-eln-objects-decode unit-a shared-word) shared-string)
               (eq (nelisp-eln-objects-decode unit-b shared-word) shared-string)
               (eq (nelisp-eln-objects-decode unit-a distinct-string-word)
                   equal-distinct)
               (eq (nelisp-eln-objects-decode unit-b distinct-string-word)
                   equal-distinct))
    (error "Units did not share canonical identity/address leases"))
  ;; Detach the original cyclic conses from the canonical root, then add a
  ;; new view; the old records remain leased until their owners close.
  (setcar graph new-leaf)
  (setcdr graph nil)
  (setq new-word (nelisp-eln-objects-encode unit-a new-leaf))
  (unless (and (/= new-word cycle-word)
               (eq (nelisp-eln-objects-decode unit-a cycle-word) cycle)
               (eq (nelisp-eln-objects-decode unit-b cycle-word) cycle))
    (error "Detached or cyclic canonical object lost its unit whitelist"))
  (unless (condition-case nil
              (progn (nelisp-eln-objects-activation-decode token new-word) nil)
            (nelisp-eln-objects-error t))
    (error "Activation decoded an object added after its lease snapshot"))
  (nelisp-eln-objects-release unit-a)
  (nelisp-eln-objects-release unit-b)
  (unless (and (eq (nelisp-eln-objects-activation-decode token word-a) graph)
               (eq (nelisp-eln-objects-activation-decode token cycle-word) cycle)
               (eq (nelisp-eln-objects-activation-decode token shared-word)
                   shared-string)
               (eq (nelisp-eln-objects-activation-decode token distinct-string-word)
                   equal-distinct))
    (error "Activation lease failed to retain the original detached graph"))
  (unless (condition-case nil
              (progn (nelisp-eln-objects-activation-decode token 4099) nil)
            (nelisp-eln-objects-error t))
    (error "Activation whitelist accepted an unowned pointer"))
  (nelisp-eln-objects-activation-release token)
  (unless (and (null nelisp-eln-objects--identity-records)
               (null nelisp-eln-objects--arenas)
               (null nelisp-eln-objects--activations))
    (error "Cross-unit release left a permanent identity cache"))
  (unless (condition-case nil
              (progn (nelisp-eln-objects-activation-decode token word-a) nil)
            (nelisp-eln-objects-error t))
    (error "Released activation token remained usable")))

;; Any unexpected partial write poisons all units until all leases drain.
(let* ((unit-a (nelisp-eln-objects-create))
       (unit-b (nelisp-eln-objects-create))
       (graph (cons 1 nil))
       (word-a (nelisp-eln-objects-encode unit-a graph))
       (word-b (nelisp-eln-objects-encode unit-b graph))
       (original-write (symbol-function 'nelisp-eln-objects--write-word))
       (writes 0)
       (poisoned nil))
  (unless (= word-a word-b) (error "Shared unit words differed before poison"))
  (setcar graph 17)
  (fset 'nelisp-eln-objects--write-word
        (lambda (address offset word)
          (funcall original-write address offset word)
          (setq writes (1+ writes))
          (when (= writes 1) (error "injected post-write failure"))))
  (setq poisoned
        (condition-case nil
            (progn (nelisp-eln-objects-sync-to-native unit-a) nil)
          (error t)))
  (fset 'nelisp-eln-objects--write-word original-write)
  (unless (and poisoned
               (eq nelisp-eln-objects--registry-state 'poisoned)
               (condition-case nil
                   (progn (nelisp-eln-objects-decode unit-b word-b) nil)
                 (nelisp-eln-objects-error t))
               (condition-case nil
                   (progn (nelisp-eln-objects-encode unit-b graph) nil)
                 (nelisp-eln-objects-error t)))
    (error "Shared post-write failure did not quarantine every unit"))
  (nelisp-eln-objects-release unit-a)
  (nelisp-eln-objects-release unit-b)
  (unless (eq nelisp-eln-objects--registry-state 'open)
    (error "Registry did not recover after every lease was released"))
  (let ((unit (nelisp-eln-objects-create)))
    (nelisp-eln-objects-encode unit graph)
    (nelisp-eln-objects-release unit)))

;; A partially completed activation release resumes at its first pending
;; member and does not decrement or release an already completed member twice.
(let* ((unit (nelisp-eln-objects-create))
       (graph (cons (copy-sequence "first") (copy-sequence "second")))
       (word (nelisp-eln-objects-encode unit graph))
       (token (nelisp-eln-objects-activation-acquire unit))
       (original-release (symbol-function 'nelisp-eln-string-release))
       (calls 0)
       (first-failed nil))
  (nelisp-eln-objects-release unit)
  (fset 'nelisp-eln-string-release
        (lambda (owner)
          (setq calls (1+ calls))
          (if (= calls 2)
              (error "injected later activation release failure")
            (funcall original-release owner))))
  (setq first-failed
        (condition-case nil
            (progn (nelisp-eln-objects-activation-release token) nil)
          (error t)))
  (fset 'nelisp-eln-string-release original-release)
  (unless (and first-failed
               (= (length (aref (cdr (assq token nelisp-eln-objects--activations)) 1)) 1))
    (error "Activation did not retain only its pending release member"))
  (nelisp-eln-objects-activation-release token)
  (unless (and (= calls 2)
               (null nelisp-eln-objects--identity-records)
               (null nelisp-eln-objects--arenas)
               (null nelisp-eln-objects--activations)
               (condition-case nil
                   (progn (nelisp-eln-objects-activation-decode token word) nil)
                 (nelisp-eln-objects-error t)))
    (error "Activation retry did not finish cleanly")))

;; Setup cleanup failures retain a retryable zero-lease arena owner.
(let* ((unit (nelisp-eln-objects-create))
       (original-address (symbol-function 'nl-ffi-memory-address))
       (original-release (symbol-function 'nl-ffi-memory-release))
       (address-failed nil)
       (release-failed nil)
       (failure nil))
  (fset 'nl-ffi-memory-address
        (lambda (_owner)
          (setq address-failed t)
          (error "injected arena address failure")))
  (fset 'nl-ffi-memory-release
        (lambda (_owner)
          (setq release-failed t)
          (error "injected arena cleanup failure")))
  (setq failure
        (condition-case err
            (progn (nelisp-eln-objects-encode unit (cons 7 nil)) nil)
          (error (error-message-string err))))
  (fset 'nl-ffi-memory-address original-address)
  (fset 'nl-ffi-memory-release original-release)
  (unless (and address-failed release-failed
               (stringp failure)
               (= (length nelisp-eln-objects--arenas) 1)
               (= (aref (car nelisp-eln-objects--arenas) 1) 0)
               (eq (aref (car nelisp-eln-objects--arenas) 2) 'releasing)
               (null nelisp-eln-objects--identity-records))
    (error "Failed setup did not retain an orphan arena for retry: addr=%S rel=%S msg=%S arenas=%S globals=%S state=%S"
           address-failed release-failed failure
           nelisp-eln-objects--arenas nelisp-eln-objects--identity-records
           nelisp-eln-objects--registry-state))
  (nelisp-eln-objects-retry-pending-cleanup)
  (unless (and (null nelisp-eln-objects--arenas)
               (null nelisp-eln-objects--identity-records)
               (eq nelisp-eln-objects--registry-state 'open))
    (error "Orphan arena cleanup retry did not restore the registry"))
  (nelisp-eln-objects-release unit))

;; A codec-created uninterned symbol has a real GNU 31.1 struct view. Its
;; canonical name, symbol word, and Qunbound cell survive unit/activation
;; lease transitions; unknown or mutated symbol representations fail closed.
(let* ((unit-a (nelisp-eln-objects-create))
       (unit-b (nelisp-eln-objects-create))
       (name (copy-sequence "eln-symbol-view"))
       (pair (nelisp-eln-objects-make-uninterned-symbol unit-a name))
       (symbol (car pair))
       (word (cdr pair))
       (unit-data-a (nelisp-eln-objects--resolve unit-a))
       (local-a (nelisp-eln-objects--symbol-record unit-data-a symbol))
       (base (aref nelisp-eln-objects--symbol-base 1))
       (address (aref local-a 1))
       (name-record (nelisp-eln-objects--string-record unit-data-a name))
       (token (nelisp-eln-objects-activation-acquire unit-a)))
  (unless (nelisp-eln-objects--global-cell-free-p symbol)
    (error "Fresh uninterned symbol already has a global variable cell"))
  (unless (and (= word (nelisp-eln-objects-encode unit-b symbol))
               (eq name (symbol-name symbol))
               (= word (nelisp-eln-objects--symbol-word address))
               (= (ptr-read-u8 address 0) 0)
               (= (nelisp-eln-objects--read-word address 8)
                  (+ (aref name-record 2) 4))
               (= (nelisp-eln-objects--read-word address 16) 48)
               (= (nelisp-eln-objects--read-word address 24) 0)
               (= (nelisp-eln-objects--read-word address 32) 0)
               (= (nelisp-eln-objects--read-word address 40) 0)
               (equal (nelisp-eln-string-read (aref name-record 1)) name)
               (= (nelisp-eln-objects--read-word (+ base 48) 16) 48)
               (= (nelisp-eln-objects--read-word (+ base 48) 8)
                  (+ (nelisp-eln-string-address
                      (aref nelisp-eln-objects--symbol-base 2)) 4)))
    (error "GNU symbol layout/name/identity fields did not match profile"))
  (unless (condition-case nil
              (progn
                (nelisp-eln-objects-encode unit-b (make-symbol "not-admitted"))
                nil)
            (nelisp-eln-objects-unsupported t))
    (error "Codec accepted a symbol without fresh-constructor provenance"))
  (ptr-write-u32 address 16 2)
  (unless (condition-case nil
              (progn (nelisp-eln-objects-sync-from-native unit-a) nil)
            (nelisp-eln-objects-unsupported t))
    (error "Codec accepted a mutated unsupported symbol value cell"))
  (nelisp-eln-objects--write-word address 16 48)
  (nelisp-eln-objects-release unit-a)
  (nelisp-eln-objects-release unit-b)
  (setq symbol nil name nil pair nil local-a nil name-record nil unit-data-a nil)
  (garbage-collect)
  (unless (equal (symbol-name
                  (nelisp-eln-objects-activation-decode token word))
                 "eln-symbol-view")
    (error "Activation lost the canonical symbol/name after GC"))
  (nelisp-eln-objects-activation-release token)
  (unless (null nelisp-eln-objects--symbol-base)
    (error "Last symbol lease did not release the shared nil/Qunbound base")))

;; The private read-only query follows symbol identity: the literal interned
;; alias and base have mirror cells, while same-named uninterned symbols do
;; not become aliases merely because their printed names match.
(let* ((unit (nelisp-eln-objects-create))
       (pair (nelisp-eln-objects-make-uninterned-symbol
              unit (copy-sequence "eln-alias-query-probe")))
       (symbol (car pair))
       (base (car (nelisp-eln-objects-make-uninterned-symbol
                   unit (copy-sequence "eln-alias-query-base")))))
  (defvaralias 'eln-alias-query-probe 'eln-alias-query-base)
  (unless (and (not (boundp base))
               (nelisp-eln-objects--global-cell-free-p base)
               (nelisp-eln-objects--global-cell-free-p symbol)
               (nelisp-eln-objects--fresh-symbol-p
                base (nelisp-eln-objects--resolve unit))
               (not (nelisp-eln-objects--global-cell-free-p
                     'eln-alias-query-probe))
               (not (nelisp-eln-objects--global-cell-free-p
                     'eln-alias-query-base)))
    (error "Alias query did not preserve tag-16 identity and tag-4 cells"))
  (unless (condition-case nil
              (progn (nelisp--symbol-global-cell-p 17) nil)
            (wrong-type-argument t))
    (error "Global-cell query accepted a non-symbol"))
  (unless (condition-case nil
              (progn (nelisp--symbol-global-cell-p) nil)
            (wrong-number-of-arguments t))
    (error "Global-cell query accepted wrong arity"))
  (nelisp-eln-objects-release unit))

;; Each symbol-owned mapping survives a failed release and is closed only
;; once on retry. The base stops admitting addresses as soon as teardown starts.
(dolist (target-kind '(symbol-view symbol-base symbol-name))
  (let* ((unit (nelisp-eln-objects-create))
         (pair (nelisp-eln-objects-make-uninterned-symbol
                unit (copy-sequence "release-retry")))
         (symbol (car pair))
         (unit-data (nelisp-eln-objects--resolve unit))
         (local (nelisp-eln-objects--symbol-record unit-data symbol))
         (shared (nelisp-eln-objects--global-record symbol))
         (base (aref nelisp-eln-objects--symbol-base 0))
         (name-owner (aref nelisp-eln-objects--symbol-base 2))
         (target (cond ((eq target-kind 'symbol-view) (aref shared 3))
                       ((eq target-kind 'symbol-base) base)
                       (t (aref name-owner 2))))
         (original (symbol-function 'nl-ffi-memory-release))
         (attempts 0) (successful-releases 0) (failed nil) (after-first nil))
    (aset shared 5 0)
    (fset 'nl-ffi-memory-release
          (lambda (owner)
            (if (eq owner target)
                (progn
                  (setq attempts (1+ attempts))
                  (if (= attempts 1)
                      (error "injected symbol owner release failure")
                    (setq successful-releases (1+ successful-releases))
                    (funcall original owner)))
              (funcall original owner))))
    (setq failed
          (condition-case nil
              (progn (nelisp-eln-objects--finish-global-release shared) nil)
            (error t)))
    (setq after-first
          (list attempts successful-releases
                nelisp-eln-objects--symbol-base
                nelisp-eln-objects--identity-records
                (aref (nelisp-eln-objects--resolve unit) 6)))
    (when (memq target-kind '(symbol-base symbol-name))
      (unless (condition-case nil
                  (progn (nelisp-eln-objects--symbol-word
                          (aref shared 2)) nil)
                (nelisp-eln-objects-error t))
        (error "Closing symbol base remained addressable")))
    (nelisp-eln-objects--finish-global-release shared)
    (fset 'nl-ffi-memory-release original)
    (aset unit-data 6 (delq local (aref unit-data 6)))
    (nelisp-eln-objects-release unit)
    (unless (and failed (= attempts 2) (= successful-releases 1)
                 (null nelisp-eln-objects--symbol-base)
                 (null nelisp-eln-objects--identity-records)
                 (eq nelisp-eln-objects--registry-state 'open))
      (error "Symbol cleanup retry failed for %S: failed=%S after-first=%S attempts=%S successes=%S base=%S globals=%S state=%S"
             target-kind failed after-first attempts successful-releases
             nelisp-eln-objects--symbol-base nelisp-eln-objects--identity-records
             nelisp-eln-objects--registry-state))))

;; Failed symbol-base setup retains every owner that could not be unmapped.
(let* ((unit (nelisp-eln-objects-create))
       (original-address (symbol-function 'nl-ffi-memory-address))
       (original-release (symbol-function 'nl-ffi-memory-release))
       (mapping nil) (attempts 0) (failure nil))
  (fset 'nl-ffi-memory-address
        (lambda (owner)
          (if (= (aref owner 3) 96)
              (progn (setq mapping owner) (error "injected base address failure"))
            (funcall original-address owner))))
  (fset 'nl-ffi-memory-release
        (lambda (owner)
          (if (eq owner mapping)
              (progn
                (setq attempts (1+ attempts))
                (if (= attempts 1)
                    (error "injected base mapping cleanup failure")
                  (funcall original-release owner)))
            (funcall original-release owner))))
  (setq failure
        (condition-case err
            (progn (nelisp-eln-objects-make-uninterned-symbol
                    unit (copy-sequence "base-setup-failure")) nil)
          (error (error-message-string err))))
  (fset 'nl-ffi-memory-address original-address)
  (unless (and mapping (stringp failure)
               (= (length nelisp-eln-objects--pending-cleanups) 1)
               (eq nelisp-eln-objects--registry-state 'poisoned)
               (null nelisp-eln-objects--symbol-base))
    (error "Failed symbol-base setup lost its pending owner"))
  (nelisp-eln-objects-retry-pending-cleanup)
  (fset 'nl-ffi-memory-release original-release)
  (unless (and (= attempts 2)
               (null nelisp-eln-objects--pending-cleanups)
               (eq nelisp-eln-objects--registry-state 'open))
    (error "Symbol-base setup cleanup retry failed"))
  (nelisp-eln-objects-release unit))

;; An unpublished symbol mapping also remains owned after cleanup failure.
(let* ((unit (nelisp-eln-objects-create))
       (original-allocate (symbol-function 'nelisp-eln-objects--allocate-view-memory))
       (original-write (symbol-function 'ptr-write-u8))
       (original-release (symbol-function 'nl-ffi-memory-release))
       (owner nil) (attempts 0) (failure nil))
  (fset 'nelisp-eln-objects--allocate-view-memory
        (lambda (bytes)
          (let ((allocated (funcall original-allocate bytes)))
            (when (= bytes 48) (setq owner allocated))
            allocated)))
  (fset 'ptr-write-u8
        (lambda (address offset byte)
          (if (and owner (= address (nl-ffi-memory-address owner)))
              (error "injected unpublished symbol initialization failure")
            (funcall original-write address offset byte))))
  (fset 'nl-ffi-memory-release
        (lambda (candidate)
          (if (eq candidate owner)
              (progn
                (setq attempts (1+ attempts))
                (if (= attempts 1)
                    (error "injected unpublished symbol cleanup failure")
                  (funcall original-release candidate)))
            (funcall original-release candidate))))
  (setq failure
        (condition-case err
            (progn (nelisp-eln-objects-make-uninterned-symbol
                    unit (copy-sequence "unpublished-symbol")) nil)
          (error (error-message-string err))))
  (fset 'nelisp-eln-objects--allocate-view-memory original-allocate)
  (fset 'ptr-write-u8 original-write)
  (unless (and owner
               (stringp failure)
               (= (length nelisp-eln-objects--pending-cleanups) 1)
               (null nelisp-eln-objects--symbol-base))
    (error "Unpublished symbol cleanup failure discarded its owner: owner=%S attempts=%S pending=%S base=%S globals=%S state=%S failure=%S"
           owner attempts nelisp-eln-objects--pending-cleanups
           nelisp-eln-objects--symbol-base nelisp-eln-objects--identity-records
           nelisp-eln-objects--registry-state failure))
  (nelisp-eln-objects-retry-pending-cleanup)
  (fset 'nl-ffi-memory-release original-release)
  (unless (and (= attempts 2)
               (null nelisp-eln-objects--pending-cleanups)
               (null nelisp-eln-objects--identity-records)
               (eq nelisp-eln-objects--registry-state 'open))
    (error "Unpublished symbol mapping cleanup retry failed"))
  (nelisp-eln-objects-release unit))

;; Registration names must reuse a canonical interned symbol, but only as an
;; activation-scoped empty-cell snapshot. The GNU nil anchor is an offset base;
;; its fields are deliberately not dereferenced or claimed as a full nil view.
(let* ((unit-a (nelisp-eln-objects-create))
       (unit-b (nelisp-eln-objects-create))
       (name (intern "nelisp-eln-registration-name-view-probe"))
       (pairs-a (nelisp-eln-objects-admit-registration-symbols
                 unit-a (list name)))
       (pairs-b (nelisp-eln-objects-admit-registration-symbols
                 unit-b (list name)))
       (word-a (cdr (car pairs-a)))
       (word-b (cdr (car pairs-b)))
       (local (nelisp-eln-objects--symbol-record
               (nelisp-eln-objects--resolve unit-a) name))
       (address (aref local 1))
       (name-record (nelisp-eln-objects--string-record
                     (nelisp-eln-objects--resolve unit-a) (symbol-name name)))
       (token-a (nelisp-eln-objects-activation-acquire unit-a))
       (token-b (nelisp-eln-objects-activation-acquire unit-b)))
  (unless (and (eq (intern-soft (symbol-name name)) name)
               (= word-a word-b)
               (= (nelisp-eln-objects--symbol-word address) word-a)
               (= (ptr-read-u8 address 0) 64)
               (= (nelisp-eln-objects--read-word address 8)
                  (+ (aref name-record 2) 4))
               (= (nelisp-eln-objects--read-word address 16) 48)
               (= (nelisp-eln-objects--read-word address 24) 0)
               (= (nelisp-eln-objects--read-word address 32) 0)
               (= (nelisp-eln-objects--read-word address 40) 0)
               (eq (nelisp-eln-objects-activation-decode token-a word-a) name)
               (eq (nelisp-eln-objects-activation-decode token-b word-b) name))
    (error "Interned registration symbol view mismatch"))
  (unless (condition-case nil
              (progn (nelisp-eln-objects-encode unit-a name) nil)
            (nelisp-eln-objects-unsupported t))
    (error "Ordinary encode reused a registration-only symbol"))
  (unless (condition-case nil
              (progn (nelisp-eln-objects-sync-to-native unit-a) nil)
            (nelisp-eln-objects-unsupported t))
    (error "General sync accepted a registration-only symbol"))
  (nelisp-eln-objects-release unit-a)
  (unless (eq (nelisp-eln-objects-activation-decode token-a word-a) name)
    (error "Closing the unit invalidated its active symbol lease"))
  (nelisp-eln-objects-activation-release token-a)
  (unless (eq (nelisp-eln-objects-activation-decode token-b word-b) name)
    (error "One activation released another unit's shared symbol view"))
  (nelisp-eln-objects-activation-release token-b)
  (unless (and (null (nelisp-eln-objects--global-record name))
               (null (nelisp-eln-objects--symbol-record
                      (nelisp-eln-objects--resolve unit-b) name)))
    (error "Registration symbol view outlived its last activation"))
  (nelisp-eln-objects-release unit-b))

;; Entire request validation precedes publication; a later fbound name must
;; leave the earlier eligible canonical symbol without a GNU view.
(let* ((unit (nelisp-eln-objects-create))
       (eligible (intern "nelisp-eln-registration-eligible-probe"))
       (ineligible (intern "nelisp-eln-registration-fbound-probe"))
       (failure nil))
  (fset ineligible #'identity)
  (setq failure
        (condition-case err
            (progn
              (nelisp-eln-objects-admit-registration-symbols
               unit (list eligible ineligible))
              nil)
          (nelisp-eln-objects-unsupported t)))
  (fmakunbound ineligible)
  (unless (and failure
               (null (nelisp-eln-objects--symbol-record
                      (nelisp-eln-objects--resolve unit) eligible))
               (null (nelisp-eln-objects--global-record eligible))
               (null nelisp-eln-objects--symbol-base))
    (error "Rejected registration batch published a partial view"))
  (nelisp-eln-objects-release unit))

(progn (princ "NELISP-ELN-OBJECTS-SMOKE-PASS") t)
