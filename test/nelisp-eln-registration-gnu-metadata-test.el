;;; nelisp-eln-registration-gnu-metadata-test.el --- GNU registration metadata -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'nelisp-eln-registration)

(defun nelisp-eln-registration-gnu-metadata-test--metadata
    (profile &optional bad-size bad-eph)
  "Build small metadata for PROFILE, optionally corrupting one field."
  (let* ((name 'gnu_identity_0)
         (data (if (eq profile 'self-emitter)
                   [nil]
                 [(function (t) t) nil t consp listp symbol-with-pos-p]))
         (eph (vector (if (eq profile 'self-emitter) 0 1)
                      name (symbol-name name) '(0 nil nil))))
    (when bad-eph (aset eph 0 bad-eph))
    (list :abi-hash "ba35c031" :data-relocations data
          :ephemeral-data-relocations eph :function-docs []
          :d-reloc-size (or bad-size (* 8 (length data)))
          :d-reloc-eph-size 32)))

(ert-deftest nelisp-eln-registration-admits-gnu-data-shape-and-both-arities ()
  (cl-letf (((symbol-function 'nelisp-eln-emitter--symbol-name)
             #'symbol-name))
    (dolist (arity '(0 1))
      (should (= (nelisp-eln-registration--metadata-data-count
                  (nelisp-eln-registration-gnu-metadata-test--metadata
                   'gnu-single-leaf)
                  'gnu-single-leaf arity)
                 6)))))

(ert-deftest nelisp-eln-registration-keeps-self-emitter-shape-strict ()
  (cl-letf (((symbol-function 'nelisp-eln-emitter--symbol-name)
             #'symbol-name))
    (should (= (nelisp-eln-registration--metadata-data-count
                (nelisp-eln-registration-gnu-metadata-test--metadata
                 'self-emitter)
                'self-emitter 1)
               1))
    (should-error
     (nelisp-eln-registration--metadata-data-count
      (nelisp-eln-registration-gnu-metadata-test--metadata
       'self-emitter nil 1)
      'self-emitter 1)
     :type 'nelisp-eln-registration-error)))

(ert-deftest nelisp-eln-registration-rejects-malformed-gnu-relocations ()
  (cl-letf (((symbol-function 'nelisp-eln-emitter--symbol-name)
             #'symbol-name))
    (should-error
     (nelisp-eln-registration--metadata-data-count
      (nelisp-eln-registration-gnu-metadata-test--metadata
       'gnu-single-leaf 40)
      'gnu-single-leaf 1)
     :type 'nelisp-eln-registration-error)
    (should-error
     (nelisp-eln-registration--metadata-data-count
      (nelisp-eln-registration-gnu-metadata-test--metadata
       'gnu-single-leaf nil 0)
      'gnu-single-leaf 1)
     :type 'nelisp-eln-registration-error)
    (should-error
     (nelisp-eln-registration--metadata-data-count
      (nelisp-eln-registration-gnu-metadata-test--metadata 'gnu-single-leaf)
      'gnu-single-leaf 2)
      :type 'nelisp-eln-registration-error)))

(ert-deftest nelisp-eln-registration-scalar0-early-check-matches-factory-shape ()
  (should (nelisp-eln-registration--scalar0-code-p
           (unibyte-string #xb8 6 0 0 0 #xc3)))
  (should-not (nelisp-eln-registration--scalar0-code-p
               (unibyte-string #xb8 4 0 0 0 #xc3)))
  (should-not (nelisp-eln-registration--scalar0-code-p
               (unibyte-string #xb9 6 0 0 0 #xc3))))

(ert-deftest nelisp-eln-registration-snapshots-and-restores-all-data-slots ()
  (let ((reads nil) (writes nil))
    (cl-letf (((symbol-function 'nelisp-eln-abi-read-word)
               (lambda (_address offset)
                 (push offset reads) 0))
              ((symbol-function 'nelisp-eln-abi-write-word)
               (lambda (_address offset word) (push (list offset word) writes))))
      (should (equal (nelisp-eln-registration--snapshot-data-relocations 100 6)
                     '(0 0 0 0 0 0)))
      (nelisp-eln-registration--restore-data-relocations 100 '(11 12 13 14 15 16)))
    (should (equal (nreverse reads) '(0 8 16 24 32 40)))
    (should (equal (nreverse writes)
                   '((0 11) (8 12) (16 13) (24 14) (32 15) (40 16))))))

(ert-deftest nelisp-eln-registration-rejects-nonzero-data-slot-before-write ()
  (let ((writes 0))
    (cl-letf (((symbol-function 'nelisp-eln-abi-read-word)
               (lambda (_address offset) (if (= offset 16) 1 0)))
              ((symbol-function 'nelisp-eln-abi-write-word)
               (lambda (&rest _) (setq writes (1+ writes))))
              ((symbol-function 'nelisp-eln-registration--fail)
               (lambda (reason &rest detail)
                 (signal 'nelisp-eln-registration-error (cons reason detail)))))
      (should-error (nelisp-eln-registration--snapshot-data-relocations 100 6)
                    :type 'nelisp-eln-registration-error))
    (should (= writes 0))))

(ert-deftest nelisp-eln-registration-validates-metadata-type-provenance ()
  (let* ((source (vector 'function-type))
         (metadata (list :data-relocations (vector source)))
         (owner (make-vector nelisp-eln-registration--owner-size nil)))
    (aset owner 7 metadata)
    (aset owner 16 'token)
    (cl-letf (((symbol-function 'nelisp-eln-registration-metadata-type-word)
               (lambda (_token &optional _index) 77))
              ((symbol-function 'nelisp-eln-registration-metadata-decode)
               (lambda (_token word) (and (= word 77) source))))
      (should (eq (nelisp-eln-registration--callback-type owner 'unit 77)
                  source))
      (should-error (nelisp-eln-registration--callback-type owner 'unit 78)
                    :type 'nelisp-eln-registration-error))))

(ert-deftest nelisp-eln-registration-writes-each-authenticated-data-word ()
  (let ((writes nil))
    (cl-letf (((symbol-function 'nelisp-eln-registration-metadata-slot-word)
               (lambda (_token kind index)
                 (should (eq kind 'data)) (+ 100 index)))
              ((symbol-function 'nelisp-eln-abi-write-word)
               (lambda (_address offset word) (push (list offset word) writes))))
      (nelisp-eln-registration--write-metadata-relocations 100 'token 6))
    (should (equal (nreverse writes)
                   '((0 100) (8 101) (16 102) (24 103) (32 104) (40 105))))))

(ert-deftest nelisp-eln-registration-closes-loader-before-releasing-metadata ()
  (let* ((owner (make-vector nelisp-eln-registration--owner-size nil)) (events nil)
         (nelisp-eln-registration--owners (list owner)))
    (aset owner 16 'token)
    (cl-letf (((symbol-function 'nelisp-eln-system-loader-close)
               (lambda (_handle) (push 'close events)))
              ((symbol-function 'nelisp-eln-registration-metadata-release)
               (lambda (token)
                 (should (eq token 'token))
                 (push 'release events))))
      (nelisp-eln-registration--cleanup
       'handle nil nil nil nil nil owner nil nil nil nil))
    (should (equal (nreverse events) '(close release)))
    (should-not (aref owner 16))
    (should-not nelisp-eln-registration--owners)))

(defun nelisp-eln-registration-gnu-metadata-test--writable-object
    (value size filesz memsz &optional symbol-size)
  "Call writable-object against one synthetic writable PT_LOAD segment."
  (let* ((symbols (make-hash-table :test 'equal))
         (entry (list :type nelisp-eln-system-loader--stt-object
                      :size (or symbol-size size) :value value))
         (state (list :path "fixture.eln" :bias #x1000000
                      :elf (list :symbols symbols
                                 :loads (list (list #x3e50 0 filesz memsz 6))))))
    (puthash "object" entry symbols)
    (cl-letf (((symbol-function 'nelisp-eln-system-loader--state)
               (lambda (&rest _) state))
              ((symbol-function 'nelisp-eln-system-loader-symbol-info)
               (lambda (&rest _)
                 (list :size size :source-path "fixture.eln"
                       :address (+ #x1000000 value)))))
      (nelisp-eln-registration--writable-object 'handle "object" size))))

(ert-deftest nelisp-eln-registration-accepts-writable-bss-object-by-memsz ()
  (should (= (nelisp-eln-registration-gnu-metadata-test--writable-object
              #x41c0 48 #x31a #x3e0)
             #x10041c0)))

(ert-deftest nelisp-eln-registration-rejects-writable-object-beyond-memsz ()
  (should-error
   (nelisp-eln-registration-gnu-metadata-test--writable-object
    #x41c0 #x71 #x31a #x3e0)
   :type 'nelisp-eln-registration-error))

(provide 'nelisp-eln-registration-gnu-metadata-test)

;;; nelisp-eln-registration-gnu-metadata-test.el ends here
