;;; nelisp-standalone-chunk-test.el --- applyfn chunking: stable assignment, blob co-location, namespaces -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'nelisp-standalone-chunk)

(defun nl-chunk-test--src (names &optional extra)
  `(seq ,@(mapcar (lambda (n) `(defun ,n (x) (+ x ,(nelisp-standalone-chunk-name-hash n)))) names)
        ,@extra))

(defun nl-chunk-test--names (n) (cl-loop for i below n collect (intern (format "nl_fn_%04d_w" i))))

(defun nl-chunk-test--naive (source k)
  "The assignment the design REJECTS: contiguous ranges balanced by running size."
  (let* ((forms (cdr source)) (sizes (mapcar (lambda (f) (length (prin1-to-string f))) forms))
         (total (apply #'+ sizes)) (acc 0) (out (make-vector k nil)))
    (cl-loop for f in forms for s in sizes
             do (push f (aref out (min (1- k) (/ (* acc k) total)))) (setq acc (+ acc s)))
    (cl-loop for c below k collect (cons 'seq (nreverse (aref out c))))))

(defun nl-chunk-test--changed (a b)
  (cl-count-if-not #'identity (cl-mapcar #'equal a b)))

(ert-deftest nelisp-chunk-adding-one-defun-moves-at-most-one-chunk ()
  (let* ((names (nl-chunk-test--names 240))
         (base (nelisp-standalone-chunk-split (nl-chunk-test--src names) 8))
         (more (nelisp-standalone-chunk-split (nl-chunk-test--src (append names '(nl_new_helper_w))) 8)))
    (should (<= (nl-chunk-test--changed base more) 1))
    (should (= 1 (nl-chunk-test--changed base more)))
    ;; Insertion in the MIDDLE of the source order must not matter either.
    (let ((mid (nelisp-standalone-chunk-split
                (nl-chunk-test--src (append (cl-subseq names 0 100) '(nl_new_helper_w) (cl-subseq names 100))) 8)))
      (should (= 1 (nl-chunk-test--changed base mid))))
    ;; Removing and editing one defun: one chunk each.
    (should (= 1 (nl-chunk-test--changed
                  base (nelisp-standalone-chunk-split (nl-chunk-test--src (cdr names)) 8))))
    (should (= 1 (nl-chunk-test--changed
                  base (nelisp-standalone-chunk-split
                        (let ((s (nl-chunk-test--src names)))
                          (setf (nth 3 (nth 7 s)) '(+ x 99999)) s) 8))))
    ;; NEGATIVE CONTROL: the size-balancing scheme moves neighbours.
    (let ((nb (nl-chunk-test--naive (nl-chunk-test--src names) 8))
          (na (nl-chunk-test--naive (nl-chunk-test--src (append (cl-subseq names 0 5) '(nl_new_helper_w) (cl-subseq names 5))) 8)))
      (should (> (nl-chunk-test--changed nb na) 1)))))

(ert-deftest nelisp-chunk-union-is-the-original-no-loss-no-duplicate ()
  (let* ((names (nl-chunk-test--names 120)) (src (nl-chunk-test--src names))
         (chunks (nelisp-standalone-chunk-split src 8))
         (all (apply #'append (mapcar #'cdr chunks))))
    (should (= (length chunks) 8))
    (should (equal (sort (mapcar #'cadr all) (lambda (a b) (string< a b)))
                   (sort (copy-sequence names) (lambda (a b) (string< a b)))))
    ;; K = 1 is the old path: the very same source object.
    (should (eq (car (nelisp-standalone-chunk-split src 1)) src))
    ;; NEGATIVE CONTROL: a duplicate definition is refused, not merged.
    (should-error (nelisp-standalone-chunk-split (nl-chunk-test--src (cons (car names) names)) 8))))

(ert-deftest nelisp-chunk-data-addr-users-live-with-their-blob ()
  ;; Find a blob and two users whose OWN name hashes land in three chunks.
  (let* ((k 8) (cands (nl-chunk-test--names 200)) (blob 'nl_blob_x) users)
    (dolist (n cands)
      (when (and (< (length users) 2)
                 (not (memq (nelisp-standalone-chunk-assign n k) (cons (nelisp-standalone-chunk-assign blob k)
                                                                         (mapcar (lambda (u) (nelisp-standalone-chunk-assign u k)) users)))))
        (push n users)))
    (should (= (length users) 2))
    (let* ((src `(seq (data-blob ,blob (bytes 0 0) data)
                      ,@(mapcar (lambda (u) `(defun ,u (x) (heap-read-u8 (data-addr ,blob) x))) users)
                      (defun nl_ext_user_w (x) (heap-read-u8 (data-addr nelisp_external_sym) x))
                      ,@(mapcar (lambda (n) `(defun ,n (x) x)) (cl-set-difference (cl-subseq cands 50 90) users))))
           (chunks (nelisp-standalone-chunk-split src 8))
           (where (lambda (name) (cl-position-if (lambda (u) (cl-some (lambda (f) (memq name (list (cadr f)))) (cdr u))) chunks))))
      (should (= (funcall where blob) (funcall where (car users)) (funcall where (cadr users))))
      ;; NEGATIVE CONTROL: with naive per-name assignment the three are apart.
      (should-not (= (nelisp-standalone-chunk-assign blob k) (nelisp-standalone-chunk-assign (car users) k)
                     (nelisp-standalone-chunk-assign (cadr users) k)))
      ;; A user of an EXTERNAL data symbol is a singleton atom (no co-location needed).
      (should (assq 'nl_ext_user_w (nelisp-standalone-chunk-atoms (cdr src)))))))

(ert-deftest nelisp-chunk-table-validation-and-pinning ()
  (let* ((src (nl-chunk-test--src (nl-chunk-test--names 30)))
         (a 'nl_fn_0003_w))
    (should (= 5 (nelisp-standalone-chunk-assign a 8 `((,a . 5)))))
    (should-error (nelisp-standalone-chunk-assign a 4 `((,a . 5))))
    (should (nelisp-standalone-chunk-check-table `((,a . 2)) (cdr src) 8))
    (should-error (nelisp-standalone-chunk-check-table `((,a . 9)) (cdr src) 8))
    ;; A table row naming a non-representative member of a blob atom is refused.
    (let ((s '(seq (data-blob b1 (bytes 0) data) (defun u1 (x) (data-addr b1)))))
      (should-error (nelisp-standalone-chunk-check-table '((u1 . 1)) (cdr s) 8))
      (should (nelisp-standalone-chunk-check-table '((b1 . 1)) (cdr s) 8)))
    ;; The committed table only applies at its own K.
    (should-not (nelisp-standalone-chunk-load-table 4))
    (should (nelisp-standalone-chunk-load-table 8))))

(ert-deftest nelisp-chunk-label-namespace-is-per-unit ()
  ;; Generated helper names (argframe `__afN', inliner `__xN') are defined by the
  ;; source, so after splitting each lives in exactly one chunk; the label
  ;; counter and the proof/fact tables are bound afresh by every unit's compile.
  (let* ((names (append (nl-chunk-test--names 60) '(h_a__af1 h_b__af2 h_c__af3 p__x4)))
         (chunks (nelisp-standalone-chunk-split (nl-chunk-test--src names) 8))
         (owners (mapcar (lambda (n) (cl-count-if (lambda (u) (cl-some (lambda (f) (eq (cadr f) n)) (cdr u))) chunks))
                         '(h_a__af1 h_b__af2 h_c__af3 p__x4))))
    (should (equal owners '(1 1 1 1)))
    ;; NEGATIVE CONTROL: a copy of one chunk's def in another chunk is caught.
    (let ((dup (cons (cons 'seq (append (cdr (nth 0 chunks)) (list (cadr (nth 1 chunks))))) (cdr chunks))))
      (should (> (apply #'max (mapcar (lambda (n) (cl-count-if (lambda (u) (cl-some (lambda (f) (eq (cadr f) n)) (cdr u))) dup))
                                      (mapcar #'cadr (cdr (nth 1 chunks)))))
                 1))))
  (let* ((root (file-name-directory (directory-file-name (file-name-directory (locate-library "nelisp-standalone-chunk")))))
         (build (with-temp-buffer (insert-file-contents (expand-file-name "scripts/nelisp-standalone-build.el" root)) (buffer-string)))
         (aot (with-temp-buffer (insert-file-contents (expand-file-name "lisp/nelisp-aot-compiler.el" root)) (buffer-string)))
         (needles-build '("(nelisp-aot-compiler--label-counter 0)" "(nelisp-standalone--inline-counter 0)"))
         (needles-aot '("(nelisp-aot-compiler--x86-frame-alias-proof-cache (make-hash-table :test #'eq))"
                        "(nelisp-aot-compiler--x86-guarded-body-facts defuns)"
                        "(nelisp-aot-compiler--symbol-literal-registry nil)")))
    (dolist (n needles-build) (should (string-search n build)))
    (dolist (n needles-aot) (should (string-search n aot)))
    ;; NEGATIVE CONTROL: a mutated text (counter carried over) fails the same probe.
    (should-not (string-search (car needles-build)
                               (string-replace "(nelisp-aot-compiler--label-counter 0)" "(nelisp-aot-compiler--label-counter 7)" build)))))

(ert-deftest nelisp-chunk-real-applyfn-source-partition ()
  (skip-unless (require 'nelisp-standalone-build nil t))
  (let* ((split (nelisp-standalone--applyfn-split-units
                 (nelisp-standalone--patch-autoload-lookups (nelisp-standalone--applyfn-source))))
         (main (car split)) (k 8)
         (chunks (nelisp-standalone-chunk-split main k (nelisp-standalone-chunk-load-table k)))
         (where (lambda (name) (cl-position-if (lambda (u) (cl-some (lambda (f) (eq (cadr f) name)) (cdr u))) chunks))))
    (should (= (apply #'+ (mapcar (lambda (u) (length (cdr u))) chunks)) (length (cdr main))))
    ;; The one local blob and its user share a chunk.
    (should (= (funcall where 'wf_bytecode_name_equal_bytes) (funcall where 'wf_bytecode_name_equal_addr)))
    ;; Every chunk is non-trivial; no chunk is more than 1.5x the mean.
    (let* ((print-length nil) (print-level nil)
           (sizes (mapcar (lambda (u) (length (prin1-to-string u))) chunks)) (mean (/ (apply #'+ sizes) k)))
      (should (cl-every (lambda (s) (and (> s 50000) (< s (* 1.5 mean)))) sizes)))
    (should (equal (nelisp-standalone-chunk-split main 1) (list main)))))

(ert-deftest nelisp-chunk-split-partner-pairs-share-a-chunk ()
  ;; F and F_w must share a chunk: the per-unit abi box split looks for F_w in
  ;; the same source and signals otherwise (F370).
  (let* ((bases (cl-loop for i below 120 collect (intern (format "nl_pair_%03d" i))))
         (src `(seq ,@(cl-loop for b in bases
                               append (list `(defun ,b (out) (nl_abi_word_to_rc (,(intern (format "%s_w" b)) 0) out))
                                            `(defun ,(intern (format "%s_w" b)) (pad) pad)))))
         (chunks (nelisp-standalone-chunk-split src 8))
         (where (lambda (n) (cl-position-if (lambda (u) (cl-some (lambda (f) (eq (cadr f) n)) (cdr u))) chunks)))
         (straddle-by-name 0))
    (dolist (b bases)
      (let ((w (intern (format "%s_w" b))))
        (should (= (funcall where b) (funcall where w)))
        ;; NEGATIVE CONTROL: independent per-name assignment (the F370 bug)
        ;; does separate many of the same pairs.
        (unless (= (nelisp-standalone-chunk-assign b 8) (nelisp-standalone-chunk-assign w 8))
          (cl-incf straddle-by-name))))
    (should (> straddle-by-name 20))
    ;; Real source: every defun with a defined F_w twin lives with it.
    (skip-unless (require 'nelisp-standalone-build nil t))
    (let* ((main (car (nelisp-standalone--applyfn-split-units
                       (nelisp-standalone--patch-autoload-lookups (nelisp-standalone--applyfn-source)))))
           (real (nelisp-standalone-chunk-split main 8 (nelisp-standalone-chunk-load-table 8)))
           (names (make-hash-table :test #'eq)) (pairs 0))
      (cl-loop for u in real for c from 0
               do (dolist (f (cdr u)) (when (eq (car-safe f) 'defun) (puthash (cadr f) c names))))
      (maphash (lambda (n c)
                 (let ((wc (gethash (intern (format "%s_w" n)) names)))
                   (when wc (cl-incf pairs) (should (= c wc)))))
               names)
      (should (> pairs 10))
      (should (eq (gethash 'bf_env_capture_roots names) (gethash 'bf_env_capture_roots_w names))))))

(ert-deftest nelisp-chunk-gc-closure-cluster-stays-in-chunk-0 ()
  ;; The F1 prelink closure must not grow past its 384 policy (F373): the
  ;; applyfn members of the closure stay together in chunk 0.
  (skip-unless (require 'nelisp-standalone-build nil t))
  (let* ((pins (nelisp-standalone-chunk-load-pins))
         (main (car (nelisp-standalone--applyfn-split-units
                     (nelisp-standalone--patch-autoload-lookups (nelisp-standalone--applyfn-source)))))
         (k 8) (table (nelisp-standalone-chunk-load-table k))
         (defs (cl-loop for f in (cdr main) when (eq (car-safe f) 'defun) collect (cadr f)))
         (where (lambda (chunks name)
                  (cl-position-if (lambda (u) (cl-some (lambda (f) (eq (cadr f) name)) (cdr u))) chunks)))
         (pinned (cl-remove-if-not (lambda (n) (memq n defs)) pins))
         (with (nelisp-standalone-chunk-split main k table pins))
         (without (nelisp-standalone-chunk-split main k table nil)))
    (should (> (length pins) 50))
    (should (= (length pinned) (length pins)))
    (dolist (n pinned) (should (eql 0 (funcall where with n))))
    ;; Twins of a pinned defun follow it.
    (dolist (n pinned)
      (let ((w (intern (format "%s_w" n))))
        (when (memq w defs) (should (eql 0 (funcall where with w))))))
    ;; NEGATIVE CONTROL: without the pins the same cluster is spread out.
    (should (> (cl-count-if-not (lambda (n) (eql 0 (funcall where without n))) pinned) 40))))

;; A2: the committed tables are located from the file's own load-time directory.
(ert-deftest nelisp-chunk-committed-tables-ignore-call-time-load-file-name ()
  ;; Under a -l wrapper `load-file-name' / `buffer-file-name' name the wrapper
  ;; and `default-directory' is arbitrary; the tables must still be found.
  (let ((load-file-name "/nonexistent-wrapper-dir/sub/wrapper.el")
        (buffer-file-name nil)
        (default-directory (file-name-as-directory temporary-file-directory)))
    (should (nelisp-standalone-chunk-load-table 8))
    (should (nelisp-standalone-chunk-load-pins 8))))

(ert-deftest nelisp-chunk-missing-committed-file-signals-at-k-gt-1 ()
  (let* ((empty (file-name-as-directory (make-temp-file "nl-chunk-empty" t)))
         (nelisp-standalone-chunk--repo empty))
    (unwind-protect
        (progn
          (should-error (nelisp-standalone-chunk-load-table 8))
          (should-error (nelisp-standalone-chunk-load-pins 8))
          ;; K = 1 keeps the old, file-free path.
          (should-not (nelisp-standalone-chunk-load-table 1))
          (should-not (nelisp-standalone-chunk-load-pins 1))
          (should-not (nelisp-standalone-chunk-load-pins)))
      (delete-directory empty t))))

(ert-deftest nelisp-chunk-committed-table-passes-check-table ()
  ;; A row naming a non-representative member (e.g. a _w partner) is never
  ;; applied and, worse, silently re-splits the atom; the validator rejects it.
  (skip-unless (require 'nelisp-standalone-build nil t))
  (let ((main (car (nelisp-standalone--applyfn-split-units
                    (nelisp-standalone--patch-autoload-lookups (nelisp-standalone--applyfn-source))))))
    (should (nelisp-standalone-chunk-check-table (nelisp-standalone-chunk-load-table 8) (cdr main) 8))))

(provide 'nelisp-standalone-chunk-test)
