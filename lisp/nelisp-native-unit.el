;;; nelisp-native-unit.el --- stable native entries with atomic unit publication -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Closed SysV integer units only. Existing binary direct calls are not patched.
(require 'nelisp-native-load)
(declare-function ptr-read-u64 "nelisp-runtime" (address offset))
(declare-function ptr-write-u64 "nelisp-runtime" (address offset value))
(declare-function ptr-call "nelisp-runtime" (address a b c d e f))
(declare-function syscall-direct "nelisp-runtime" (number a b c d e f))

(defconst nelisp-native-unit-max-exports 64)
(defconst nelisp-native-unit-candidate-ttl 900)
(defconst nelisp-native-unit--capacity 64)
(defvar nelisp-native-unit--next-id 0)
(defvar nelisp-native-unit--candidates nil)
(defvar nelisp-native-unit--units nil)
(defvar nelisp-native-unit--publisher nil)

(defun nelisp-native-unit--copy (value)
  (cond ((stringp value) (copy-sequence value))
        ((consp value) (cons (nelisp-native-unit--copy (car value))
                            (nelisp-native-unit--copy (cdr value))))
        (t value)))

(defun nelisp-native-unit--id (prefix)
  (setq nelisp-native-unit--next-id (1+ nelisp-native-unit--next-id))
  (concat prefix "-" (secure-hash 'sha256
                                  (format "%S:%S:%S" (current-time)
                                          (random) nelisp-native-unit--next-id))))

(defun nelisp-native-unit--u32 (n)
  (list (logand n 255) (logand (ash n -8) 255)
        (logand (ash n -16) 255) (logand (ash n -24) 255)))

(defun nelisp-native-unit--u64-bytes (n)
  (append (nelisp-native-unit--u32 n) (nelisp-native-unit--u32 (ash n -32))))

(defun nelisp-native-unit--gate-bytes (control index)
  ;; SysV args remain untouched. Load the current immutable generation table once.
  (append '(#x48 #xb8) (nelisp-native-unit--u64-bytes control)
          '(#x48 #x8b #x00 #xff #xa0)
          (nelisp-native-unit--u32 (+ 8 (* 8 index))) (make-list 13 0)))

(defun nelisp-native-unit--publisher-bytes ()
  ;; rdi=control, rsi=expected, rdx=next; lock cmpxchg; return boolean.
  '(#x48 #x89 #xf0 #xf0 #x48 #x0f #xb1 #x17
    #x0f #x94 #xc0 #x0f #xb6 #xc0 #xc3))

(defun nelisp-native-unit--find (id)
  (cdr (assoc id nelisp-native-unit--units)))

(defun nelisp-native-unit--remove-candidate (id)
  (setq nelisp-native-unit--candidates
        (cl-remove-if (lambda (entry) (equal (car entry) id))
                      nelisp-native-unit--candidates)))

(defun nelisp-native-unit--purge ()
  (let ((now (float-time)))
    (setq nelisp-native-unit--candidates
          (cl-remove-if
           (lambda (entry)
             (>= (- now (plist-get (cdr entry) :created)) nelisp-native-unit-candidate-ttl))
           nelisp-native-unit--candidates))))

(defun nelisp-native-unit-discard (candidate-id)
  "Revoke CANDIDATE-ID without publishing; mapped code remains until exit."
  (nelisp-native-unit--remove-candidate candidate-id))

(defun nelisp-native-unit--hash (path)
  "Hash bounded literal bytes, never a pathname string."
  (unless (and (file-regular-p path) (file-readable-p path))
    (error "native-unit: missing input %s" path))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path nil 0 (1+ (* 16 1024 1024)))
    (when (> (- (point-max) (point-min)) (* 16 1024 1024))
      (error "native-unit: input exceeds 16 MiB"))
    (secure-hash 'sha256 (buffer-string))))

(defun nelisp-native-unit--table (unit)
  (ptr-read-u64 (plist-get unit :control) 0))

(defun nelisp-native-unit--generation (table)
  (if (= table 0) 0 (ptr-read-u64 table 0)))

(defun nelisp-native-unit--create (contract binary)
  "Allocate stable gates once. Never write an existing unit's control slot."
  (let ((control (nelisp-native-load--mmap 4096 nil))
        (page (nelisp-native-load--mmap 4096 nil))
        (id (nelisp-native-unit--id "unit")) gates (index 0))
    (ptr-write-u64 control 0 0)
    (dolist (entry contract)
      (nelisp-native-load--poke-bytes page (* index 32)
                                     (nelisp-native-unit--gate-bytes control index))
      (push (cons (car entry) (+ page (* index 32))) gates)
      (setq index (1+ index)))
    (nelisp-native-load--mprotect-rx page 4096)
    (unless nelisp-native-unit--publisher
      (let ((publisher (nelisp-native-load--mmap 4096 nil)))
        (nelisp-native-load--poke-bytes publisher 0 (nelisp-native-unit--publisher-bytes))
        (nelisp-native-load--mprotect-rx publisher 4096)
        (setq nelisp-native-unit--publisher publisher)))
    (let ((unit (list :unit-id id :control control :contract contract
                      :gates (nreverse gates) :publisher nelisp-native-unit--publisher
                      :binary-sha256 binary)))
      (push (cons id unit) nelisp-native-unit--units)
      unit)))

(defun nelisp-native-unit--stage (artifact-path unit-id export-names)
  (nelisp-native-unit--purge)
  (when (>= (length nelisp-native-unit--candidates) nelisp-native-unit--capacity)
    (error "native-unit: candidate capacity reached"))
  (when (and (null unit-id) (>= (length nelisp-native-unit--units) nelisp-native-unit--capacity))
    (error "native-unit: unit capacity reached"))
  (let* ((old (and unit-id (nelisp-native-unit--find unit-id)))
         (_ (when (and unit-id (null old)) (error "native-unit: unknown unit")))
         (path (expand-file-name artifact-path))
         (artifact-hash (nelisp-native-unit--hash path))
         (manifest (nelisp-native-load-manifest path))
         (native (plist-get manifest :native))
         (source (plist-get manifest :source))
         (source-hash (nelisp-native-unit--hash source))
         (binary (nelisp-native-load--running-binary-sha256))
         (expected (if old (nelisp-native-unit--table old) 0))
         (generation (nelisp-native-unit--generation expected))
         (names (or export-names (and old (mapcar #'car (plist-get old :contract)))
                    (mapcar (lambda (e) (plist-get e :name))
                            (nelisp-native-load--raw-exports native))))
         (contract nil))
    (unless (and (null (nelisp-native-load-raw-check manifest))
                 (null (plist-get native :imports))
                 (stringp binary) (equal binary (plist-get manifest :binary-sha256))
                 (equal source-hash (plist-get manifest :source-sha256)))
      (error "native-unit: invalid, imported, or mismatched raw-v1 artifact"))
    (unless (and (proper-list-p names) names
                 (<= (length names) nelisp-native-unit-max-exports)
                 (cl-every #'stringp names)
                 (= (length names) (length (delete-dups (copy-sequence names)))))
      (error "native-unit: expected 1..64 distinct export names"))
    (dolist (name names)
      (let ((entry (nelisp-native-load--raw-export native name)))
        (unless entry (error "native-unit: public export missing: %s" name))
        (push (cons (copy-sequence name) (plist-get entry :arity)) contract)))
    (setq contract (nreverse contract))
    (when (and old (or (not (equal contract (plist-get old :contract)))
                      (not (equal binary (plist-get old :binary-sha256)))))
      (error "native-unit: public names, order, arities or binary changed"))
    (let* ((handle (nelisp-native-load-raw-artifact path (car names) binary))
           (unit (or old (nelisp-native-unit--create contract binary)))
           (table (nelisp-native-load--mmap 4096 nil))
           (index 0)
           (id (nelisp-native-unit--id "candidate")))
      (unless (and (equal artifact-hash (nelisp-native-unit--hash path))
                   (equal source-hash (nelisp-native-unit--hash source))
                   (equal (plist-get handle :artifact-sha256)
                          (plist-get manifest :artifact-sha256))
                   (equal binary (nelisp-native-load--running-binary-sha256))
                   (= expected (nelisp-native-unit--table unit)))
        (error "native-unit: inputs or generation changed while staging"))
      (ptr-write-u64 table 0 (1+ generation))
      (dolist (entry contract)
        (ptr-write-u64 table (+ 8 (* 8 index))
                       (nelisp-native-load-raw-export-address handle (car entry)))
        (setq index (1+ index)))
      (unless (= 0 (syscall-direct 10 table 4096 1 0 0 0))
        (error "native-unit: cannot protect immutable table"))
      (push (cons id (list :unit-id (plist-get unit :unit-id) :expected-table expected
                           :generation (1+ generation) :table table
                           :artifact path :artifact-hash artifact-hash
                           :source source :source-hash source-hash
                           :binary-sha256 binary :created (float-time)))
            nelisp-native-unit--candidates)
      (nelisp-native-unit--copy
       (list :status 'staged :candidate-id id :unit-id (plist-get unit :unit-id)
             :expected-generation generation :exports contract)))))

(defun nelisp-native-unit-stage (artifact-path &optional unit-id export-names)
  "Validate and map a closed raw-v1 artifact without publication.
Reuse UNIT-ID's fixed public contract; additional internal helpers are allowed.
Returned data contains no private handles. Candidates expire after 15 minutes."
  (condition-case err
      (nelisp-native-unit--stage artifact-path unit-id export-names)
    (error (list :status 'rejected :phase :stage :reason (error-message-string err)))))

(defun nelisp-native-unit-publish (candidate-id)
  "Consume and atomically publish CANDIDATE-ID, rejecting stale identities.
Readers already executing an older generation may finish there. A new call
through a stable entry observes the current complete table. Old mappings stay."
  (condition-case err
      (progn
        (nelisp-native-unit--purge)
        (let* ((candidate (cdr (assoc candidate-id nelisp-native-unit--candidates)))
               (unit (and candidate
                          (nelisp-native-unit--find (plist-get candidate :unit-id)))))
          (unless unit (error "native-unit: unknown, expired or consumed candidate"))
          (nelisp-native-unit--remove-candidate candidate-id)
          (unless (and (equal (plist-get candidate :artifact-hash)
                              (nelisp-native-unit--hash (plist-get candidate :artifact)))
                       (equal (plist-get candidate :source-hash)
                              (nelisp-native-unit--hash (plist-get candidate :source)))
                       (< (- (float-time) (plist-get candidate :created))
                          nelisp-native-unit-candidate-ttl)
                       (equal (plist-get candidate :binary-sha256)
                              (nelisp-native-load--running-binary-sha256)))
            (error "native-unit: stale source, artifact, binary or deadline"))
          ;; Never initialize or restore control here. CAS is the only store.
          (unless (= 1 (ptr-call (plist-get unit :publisher)
                                 (plist-get unit :control)
                                 (plist-get candidate :expected-table)
                                 (plist-get candidate :table) 0 0 0))
            (error "native-unit: stale generation (CAS rejected)"))
          (nelisp-native-unit--copy
           (list :status 'published :unit-id (plist-get unit :unit-id)
                 :generation (plist-get candidate :generation)
                 :artifact-sha256 (plist-get candidate :artifact-hash)
                 :source-sha256 (plist-get candidate :source-hash)))))
    (error (list :status 'rejected :phase :publish :reason (error-message-string err)))))

(defun nelisp-native-unit-status (unit-id)
  "Report the current table's generation, not cached host metadata."
  (let ((unit (nelisp-native-unit--find unit-id)))
    (when unit
      (let ((table (nelisp-native-unit--table unit)))
        (nelisp-native-unit--copy
         (list :unit-id unit-id :generation (nelisp-native-unit--generation table)
               :binary-sha256 (plist-get unit :binary-sha256)
               :exports (plist-get unit :contract) :published (/= table 0)))))))

(defun nelisp-native-unit-address (unit-id name)
  "Return NAME's stable executable entry, available only after first publication."
  (let ((unit (nelisp-native-unit--find unit-id)))
    (unless (and unit (/= 0 (nelisp-native-unit--table unit)))
      (error "native-unit: unit is not published"))
    (or (cdr (assoc name (plist-get unit :gates)))
        (error "native-unit: unknown public export %s" name))))

(defun nelisp-native-unit-call (unit-id name args)
  "Call a stable public entry with up to six integer arguments; return a raw word."
  (let* ((unit (nelisp-native-unit--find unit-id))
         (entry (assoc name (plist-get unit :contract)))
         (arity (cdr entry)))
    (unless (and entry (proper-list-p args) (= arity (length args))
                 (cl-every #'integerp args))
      (error "native-unit: integer argument/arity mismatch"))
    (apply #'ptr-call (nelisp-native-unit-address unit-id name)
           (append args (make-list (- 6 arity) 0)))))

(provide 'nelisp-native-unit)
