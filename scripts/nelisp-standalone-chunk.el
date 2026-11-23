;;; nelisp-standalone-chunk.el --- stable chunking of the reader applyfn unit  -*- lexical-binding: t; -*-

;; Splits the NON-frame forms of the reader applyfn (the main unit of
;; `nelisp-standalone--applyfn-split-units') into K independently compiled
;; chunk units, so that editing one defun recompiles one chunk instead of the
;; whole 515 s unit (incremental-build design 2, part D).
;;
;; Properties (each has an ERT in test/nelisp-standalone-chunk-test.el):
;;  - The unit of assignment is an ATOM: one defun, or a local `data-blob'
;;    together with every defun that takes `(data-addr BLOB)' of it (blob
;;    symbols are object-local, so users must share the object).  A
;;    `data-addr' of a symbol that no `data-blob' of this source defines is an
;;    external data symbol and needs no co-location.
;;  - Assignment is a pure function of (atom representative name, K, table):
;;    the committed table (`nelisp-standalone-chunk-table') wins, else
;;    sha1(name) mod K.  Sizes never enter a normal build, so adding or
;;    editing a defun moves at most that one atom.  Sizes are only used by the
;;    offline `nelisp-standalone-chunk-rebalance', which returns a new table.
;;  - Calls between chunks are ordinary external calls (relocations), as the
;;    argv-entry units already do for main-unit helpers.  Per-unit compiler
;;    state (label counter, frame-alias proof cache, guarded-body facts,
;;    local-extern/trivial/partial call facts, symbol-literal registry) is
;;    bound afresh by each unit's compile, so every chunk has its own
;;    namespace; a callee in another chunk is an unknown extern, which only
;;    loses elision.
;;  - K = 1 returns the source unchanged (the old path, byte for byte).

(require 'cl-lib)
(require 'subr-x)

(defconst nelisp-standalone-chunk-default-count 8
  "Chunk count used when the flag asks for chunking without a number.")

(defvar nelisp-standalone-chunk-table nil
  "Alist (ATOM-REPRESENTATIVE-NAME . CHUNK-INDEX) committed in the tree.
Entries override the name hash.  Written only by an explicit rebalance.")

(defvar nelisp-standalone-chunk-table-k nil
  "K the committed table was generated for; the table is ignored at another K.")

(defvar nelisp-standalone-chunk-pin-regexps nil
  "Regexps: an atom whose representative name matches is pinned to chunk 0.
Keeps a hot cluster in one object so intra-unit inlining survives.")

(defvar nelisp-standalone-chunk-pinned-names nil
  "Defun names whose whole atom is pinned to chunk 0 (the F1 closure cluster).")

(defun nelisp-standalone-chunk-load-pins ()
  "The committed pinned-name list (tools/dev/applyfn-chunk-pins.el), or nil."
  (let ((file (expand-file-name "tools/dev/applyfn-chunk-pins.el"
                                (file-name-directory
                                 (directory-file-name
                                  (file-name-directory (or load-file-name buffer-file-name
                                                           (locate-library "nelisp-standalone-chunk"))))))))
    (when (file-readable-p file)
      (let ((nelisp-standalone-chunk-pinned-names nil))
        (load file nil t t)
        nelisp-standalone-chunk-pinned-names))))

(defun nelisp-standalone-chunk-count ()
  "Chunk count requested by NELISP_STANDALONE_APPLYFN_CHUNKS (1 = old path).
Unset, empty, 0 or 1 select the unsplit unit; \"on\" selects the default count."
  (let ((v (getenv "NELISP_STANDALONE_APPLYFN_CHUNKS")))
    (cond ((or (null v) (string-empty-p v)) 1)
          ((string= v "on") nelisp-standalone-chunk-default-count)
          (t (max 1 (string-to-number v))))))

(defun nelisp-standalone-chunk-load-table (k)
  "The committed override table for K chunks (tools/dev/applyfn-chunk-map.el), or nil.
The file sets `nelisp-standalone-chunk-table' and `-table-k'; rows are used
only when its K equals the requested K."
  (when (> k 1)
    (let ((file (expand-file-name "tools/dev/applyfn-chunk-map.el"
                                  (file-name-directory
                                   (directory-file-name
                                    (file-name-directory (or load-file-name buffer-file-name
                                                             (locate-library "nelisp-standalone-chunk"))))))))
      (when (file-readable-p file)
        (let ((nelisp-standalone-chunk-table nil) (nelisp-standalone-chunk-table-k nil))
          (load file nil t t)
          (and (eql nelisp-standalone-chunk-table-k k) nelisp-standalone-chunk-table))))))

(defun nelisp-standalone-chunk-unit-name (index)
  "Link unit name of chunk INDEX; chunk 0 keeps the historical name."
  (if (= index 0) "applyfn-reader.o" (format "applyfn-chunk-%d.o" index)))

(defun nelisp-standalone-chunk-name-hash (name)
  "Stable non-negative integer for the symbol NAME (order independent)."
  (string-to-number (substring (secure-hash 'sha1 (symbol-name name)) 0 7) 16))

(defun nelisp-standalone-chunk--data-addrs (form)
  "Symbols X of every (data-addr X) inside FORM."
  (let (out (stack (list form)))
    (while stack
      (let ((x (pop stack)))
        (when (consp x)
          (if (and (eq (car x) 'data-addr) (symbolp (cadr x)))
              (cl-pushnew (cadr x) out)
            (push (car x) stack)
            (push (cdr x) stack)))))
    out))

(defun nelisp-standalone-chunk-atoms (forms)
  "Group FORMS (a list of source forms) into atoms.
Return a list of (REP . FORMS-IN-SOURCE-ORDER), ordered by first member.  An
atom is a union of: a local data-blob and every defun taking its address, and
a split partner pair F / F_w (both defined in FORMS).  The per-unit abi box
split (`nelisp-standalone--abi-box-split-source') looks for F_w in the same
source, so the pair must share a chunk.  REP is the first local blob name of
the atom (by name), else the first defun name; a singleton's REP is its name."
  (let ((blobs (make-hash-table :test #'eq)) (defuns (make-hash-table :test #'eq))
        (parent (make-hash-table :test #'eq)) (members (make-hash-table :test #'eq))
        (key-of (make-vector (length forms) nil)) (i 0) (order nil))
    (cl-labels ((find* (x) (let ((p (gethash x parent x))) (if (eq p x) x (puthash x (find* p) parent) (gethash x parent))))
                (union* (a b) (let ((ra (find* a)) (rb (find* b))) (unless (eq ra rb) (puthash rb ra parent))))
                (sym< (a b) (string< (symbol-name a) (symbol-name b))))
      (dolist (f forms)
        (when (and (eq (car-safe f) 'data-blob) (symbolp (cadr f))) (puthash (cadr f) t blobs))
        (when (and (eq (car-safe f) 'defun) (symbolp (cadr f))) (puthash (cadr f) t defuns)))
      (dolist (f forms)
        (let* ((name (and (memq (car-safe f) '(defun data-blob)) (symbolp (cadr f)) (cadr f)))
               (key (or name (intern (format "nl-chunk-form-%d" i)))))
          (aset key-of i key) (setq i (1+ i))
          (when (and name (eq (car f) 'defun))
            (let ((used (cl-remove-if-not (lambda (s) (gethash s blobs))
                                          (nelisp-standalone-chunk--data-addrs f)))
                  (base (and (string-suffix-p "_w" (symbol-name name))
                             (intern (substring (symbol-name name) 0 -2)))))
              (when (and used (cdr used))
                (error "applyfn chunk: %s takes the addresses of several local blobs %S" name used))
              (when used (union* (car used) name))
              (when (and base (gethash base defuns)) (union* base name))))))
      ;; Representative of a class: first local blob by name, else first member by name.
      (let ((cls (make-hash-table :test #'eq)) (rep (make-hash-table :test #'eq)))
        (dotimes (j (length key-of)) (push (aref key-of j) (gethash (find* (aref key-of j)) cls)))
        (maphash (lambda (root ms)
                   (let* ((bl (cl-remove-if-not (lambda (m) (gethash m blobs)) ms))
                          (pick (car (sort (copy-sequence (or bl ms)) #'sym<))))
                     (puthash root pick rep)))
                 cls)
        (setq i 0)
        (dolist (f forms)
          (let ((r (gethash (find* (aref key-of i)) rep)))
            (setq i (1+ i))
            (puthash r (cons f (gethash r members)) members)
            (unless (memq r order) (push r order))))
        (mapcar (lambda (r) (cons r (nreverse (gethash r members)))) (nreverse order))))))

(defun nelisp-standalone-chunk-assign (rep k &optional table)
  "Chunk index in [0,K) of the atom with representative name REP."
  (let ((hit (assq rep (or table nelisp-standalone-chunk-table))))
    (cond ((= k 1) 0)
          (hit (if (< (cdr hit) k) (cdr hit)
                 (error "applyfn chunk: table row %S exceeds K=%d" hit k)))
          ((cl-some (lambda (re) (string-match-p re (symbol-name rep)))
                    nelisp-standalone-chunk-pin-regexps)
           0)
          (t (mod (nelisp-standalone-chunk-name-hash rep) k)))))

(defun nelisp-standalone-chunk-split (source k &optional table pins)
  "Split the unit SOURCE, a (seq FORM...), into K (seq ...) chunk sources.
K <= 1 returns (SOURCE).  Forms keep their source order inside a chunk.  Every
form lands in exactly one chunk, or the function signals."
  (if (<= k 1) (list source)
    (let* ((atoms (nelisp-standalone-chunk-atoms (cdr source)))
           (buckets (make-vector k nil)) (seen (make-hash-table :test #'eq)) (n 0))
      (dolist (a atoms)
        (let ((c (if (cl-some (lambda (f) (and (eq (car-safe f) 'defun) (memq (cadr f) pins))) (cdr a))
                     0
                   (nelisp-standalone-chunk-assign (car a) k table))))
          (dolist (f (cdr a))
            (when (and (eq (car-safe f) 'defun) (gethash (cadr f) seen))
              (error "applyfn chunk: %s defined twice" (cadr f)))
            (when (eq (car-safe f) 'defun) (puthash (cadr f) t seen))
            (setq n (1+ n))
            (push f (aref buckets c)))))
      (unless (= n (length (cdr source))) (error "applyfn chunk: form count mismatch"))
      (cl-loop for c below k collect (cons 'seq (nreverse (aref buckets c)))))))

(defun nelisp-standalone-chunk-check-table (table forms k)
  "Signal when TABLE names a non-representative member of a multi-form atom
with another chunk than its representative (a silent re-split), or a chunk >= K."
  (let ((atoms (nelisp-standalone-chunk-atoms forms)))
    (dolist (row table)
      (unless (< (cdr row) k) (error "applyfn chunk: table row %S exceeds K=%d" row k))
      (dolist (a atoms)
        (when (and (not (eq (car a) (car row)))
                   (cl-some (lambda (f) (and (eq (car-safe f) 'defun) (eq (cadr f) (car row)))) (cdr a))
                   (cdr (cdr a)))
          (error "applyfn chunk: table row %S splits the atom of %s" row (car a)))))
    t))

(defun nelisp-standalone-chunk-sizes (source k &optional table)
  "Prin1 byte size of each chunk of SOURCE."
  (let ((print-length nil) (print-level nil))
    (mapcar (lambda (u) (length (prin1-to-string u)))
            (nelisp-standalone-chunk-split source k table))))

(defun nelisp-standalone-chunk-rebalance (forms k &optional ratio)
  "OFFLINE: return a table moving the heaviest atoms off over-full chunks.
Starts from the hash assignment; while max/min > RATIO (default 1.15) the
largest atom of the heaviest chunk, ties broken by name hash, moves to the
lightest chunk.  The result is meant to be committed; a normal build never
calls this."
  (let* ((ratio (or ratio 1.15)) (print-length nil) (print-level nil)
         (atoms (mapcar (lambda (a) (list (car a) (length (prin1-to-string (cdr a)))
                                          (nelisp-standalone-chunk-assign (car a) k nil)))
                        (nelisp-standalone-chunk-atoms forms)))
         (loads (make-vector k 0)) (table nil) (guard 0))
    (dolist (a atoms) (cl-incf (aref loads (nth 2 a)) (nth 1 a)))
    (while (and (< (cl-incf guard) 5000)
                (> (float (seq-max loads)) (* ratio (max 1 (seq-min loads)))))
      (let* ((hi (cl-position (seq-max loads) loads)) (lo (cl-position (seq-min loads) loads))
             (cands (sort (cl-remove-if-not (lambda (a) (and (= (nth 2 a) hi)
                                                             (< (nth 1 a) (/ (- (aref loads hi) (aref loads lo)) 1))))
                                            atoms)
                          (lambda (a b) (or (> (nth 1 a) (nth 1 b))
                                            (and (= (nth 1 a) (nth 1 b))
                                                 (< (nelisp-standalone-chunk-name-hash (car a))
                                                    (nelisp-standalone-chunk-name-hash (car b)))))))))
        (if (null cands) (setq guard 5000)
          (let ((a (car cands)))
            (cl-decf (aref loads hi) (nth 1 a)) (cl-incf (aref loads lo) (nth 1 a))
            (setf (nth 2 a) lo)
            (setf table (cons (cons (car a) lo) (assq-delete-all (car a) table)))))))
    (sort table (lambda (a b) (string< (symbol-name (car a)) (symbol-name (car b)))))))

(provide 'nelisp-standalone-chunk)
;;; nelisp-standalone-chunk.el ends here
