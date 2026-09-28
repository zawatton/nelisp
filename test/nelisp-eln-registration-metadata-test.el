;;; nelisp-eln-registration-metadata-test.el --- bounded graph metadata -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'cl-lib)
(let ((root (getenv "NELISP_ROOT"))
      (module-root (getenv "NELISP_ELN_REGISTRATION_METADATA_MODULE_ROOT"))
      (overlay (file-name-directory
                (directory-file-name
                 (file-name-directory (or load-file-name buffer-file-name))))))
  (add-to-list 'load-path (expand-file-name "lisp" overlay))
  (when root
    (add-to-list 'load-path (expand-file-name "lisp" root))
    (add-to-list 'load-path (expand-file-name "packages/nl-ffi/src" root)))
  (when module-root
    (add-to-list 'load-path module-root)))
(require 'nelisp-eln-registration-metadata)

(defmacro nelisp-eln-registration-metadata-test--memory (&rest body)
  `(let ((memory (make-hash-table :test #'eql))
         (next-address 1048584)
         (owners nil)
         (nelisp-eln-metadata-arena--pool nil)
         (nelisp-eln-metadata-arena--live nil)
         (nelisp-eln-metadata-arena--base-lease nil)
         (nelisp-eln-registration-metadata--live nil)
         (nelisp-eln-objects--symbol-base nil)
         (nelisp-eln-objects--symbol-base-leases nil)
         (nelisp-eln-objects--pending-cleanups nil)
         (nelisp-eln-objects--identity-records nil)
         (nelisp-eln-objects--arenas nil)
         (nelisp-eln-objects--activations nil)
         (nelisp-eln-objects--registry-state 'open))
     (cl-labels ((write-byte (address offset value)
                  (puthash (+ address offset) (logand value 255) memory))
                 (read-byte (address offset)
                  (gethash (+ address offset) memory 0))
                 (write-n (address offset value bytes)
                  (let ((i 0))
                    (while (< i bytes)
                      (write-byte address (+ offset i)
                                  (logand (ash value (* -8 i)) 255))
                      (setq i (1+ i)))))
                 (read-n (address offset bytes)
                  (let ((i 0) (value 0))
                    (while (< i bytes)
                      (setq value (+ value (ash (read-byte address
                                                           (+ offset i))
                                                (* 8 i))))
                      (setq i (1+ i)))
                    value)))
       (cl-letf (((symbol-function 'nl-ffi-memory-allocate)
                  (lambda (size)
                    (let ((owner (vector 'nl-ffi-memory-owner
                                         next-address size 0)))
                      (setq owners (cons owner owners)
                            next-address (+ next-address size 4096))
                      owner)))
                 ((symbol-function 'nl-ffi-memory-address)
                  (lambda (owner) (aref owner 1)))
                 ((symbol-function 'nl-ffi-memory-release) (lambda (_) t))
                 ((symbol-function 'ptr-write-u8)
                  (lambda (address offset value)
                    (write-byte address offset value)))
                 ((symbol-function 'ptr-read-u8)
                  (lambda (address offset) (read-byte address offset)))
                 ((symbol-function 'ptr-write-u32)
                  (lambda (address offset value)
                    (write-n address offset value 4)))
                 ((symbol-function 'ptr-read-u32)
                  (lambda (address offset) (read-n address offset 4)))
                 ((symbol-function 'ptr-write-u64)
                  (lambda (address offset value)
                    (write-n address offset value 8)))
                 ((symbol-function 'ptr-read-u64)
                  (lambda (address offset) (read-n address offset 8)))
                 ((symbol-function 'syscall-direct) (lambda (&rest _) 0)))
         ,@body))))

(defun nelisp-eln-registration-metadata-test--bytes (address length)
  (let ((result (string-make-unibyte (make-string length 0))) (i 0))
    (while (< i length)
      (aset result i (ptr-read-u8 address i))
      (setq i (1+ i)))
    result))

(defun nelisp-eln-registration-metadata-test--standalone ()
  (let* ((data (vector '(function (t) t) nil t
                       'consp 'listp 'symbol-with-pos-p))
         (docs (vector (string-make-unibyte (unibyte-string 255)) "雪"))
         (token (nelisp-eln-registration-metadata-create
                 'gnu-single-leaf data docs))
         (data-address (- (nelisp-eln-registration-metadata-data-word token) 5))
         (docs-address (- (nelisp-eln-registration-metadata-docs-word token) 5))
         (byte-word (nelisp-eln-registration-metadata-slot-word token 'docs 0))
         (byte-address (- byte-word 4))
         (unicode-word (nelisp-eln-registration-metadata-slot-word token 'docs 1))
         (unicode-address (- unicode-word 4)))
    (unwind-protect
        (progn
          (unless (and (= (nelisp-eln-abi-read-word data-address 0) 6)
                       (= (nelisp-eln-abi-read-word docs-address 0) 2)
                       (= (ptr-read-u64 byte-address 8)
                          nelisp-eln-string--unibyte-size-byte)
                       (= (ptr-read-u8 (+ byte-address 32) 0) 255)
                       (= (ptr-read-u64 unicode-address 0) 1)
                       (= (ptr-read-u64 unicode-address 8) 3)
                       (equal (nelisp-eln-registration-metadata-test--bytes
                               (+ unicode-address 32) 3)
                              (string-make-unibyte (encode-coding-string "雪" 'utf-8 t))))
            (error "standalone graph/string descriptor check failed"))
          (unless (and (eq (nelisp-eln-registration-metadata-decode
                            token (nelisp-eln-registration-metadata-data-word token))
                           data)
                       (= (nelisp-eln-registration-metadata-type-word token)
                          (nelisp-eln-registration-metadata-slot-word token 'data 0)))
            (error "standalone identity/type-word check failed"))
          (princ "registration-metadata-standalone: PASS\n"))
      (nelisp-eln-registration-metadata-release token))))

(if (getenv "NELISP_REGISTRATION_METADATA_STANDALONE")
    (nelisp-eln-registration-metadata-test--standalone)
  (progn
    (require 'ert)

    (ert-deftest nelisp-eln-registration-metadata/gnu-vector-symbols-and-docstrings ()
      (nelisp-eln-registration-metadata-test--memory
       (let* ((data (vector '(function (t) t) nil t
                            'consp 'listp 'symbol-with-pos-p))
              (docs (vector "doc" "雪"))
              (token (nelisp-eln-registration-metadata-create
                      'gnu-single-leaf data docs))
              (data-word (nelisp-eln-registration-metadata-data-word token))
              (docs-word (nelisp-eln-registration-metadata-docs-word token))
              (data-address (- data-word 5))
              (docs-address (- docs-word 5))
              (base (aref nelisp-eln-objects--symbol-base 1)))
         (unwind-protect
             (progn
               (unless (= (nelisp-eln-abi-read-word data-address 0) 6)
                 (error "bad GNU data-vector header: %S"
                        (nelisp-eln-abi-read-word data-address 0)))
               (should (= (nelisp-eln-abi-read-word docs-address 0) 2))
               (should (= (nelisp-eln-registration-metadata-type-word token)
                          (nelisp-eln-registration-metadata-slot-word
                           token 'data 0)))
               (should (= (nelisp-eln-registration-metadata-slot-word
                           token 'data 1)
                          (nelisp-eln-abi-encode-nil)))
               (should (eq (nelisp-eln-registration-metadata-decode
                            token (nelisp-eln-registration-metadata-slot-word
                                   token 'data 2)) 't))
               (should (eq (nelisp-eln-registration-metadata-decode
                            token data-word) data))
               (let* ((cons-word (nelisp-eln-registration-metadata-slot-word
                                  token 'data 0))
                      (cons-address (- cons-word 3))
                      (car-word (nelisp-eln-abi-read-word cons-address 0))
                      (cdr-word (nelisp-eln-abi-read-word cons-address 8)))
                 (should (= (logand cons-word 7) 3))
                 (should (eq (nelisp-eln-registration-metadata-decode
                              token car-word) 'function))
                 (should (eq (nelisp-eln-registration-metadata-decode
                              token cdr-word) (cdr (aref data 0)))))
               (dolist (index '(2 3 4 5))
                 (let* ((word (nelisp-eln-registration-metadata-slot-word
                               token 'data index))
                        (signed (if (> word nelisp-eln-abi-signed-word-max)
                                    (- word (1+ nelisp-eln-abi-word-mask))
                                  word))
                        (address (+ base signed))
                        (name-word (nelisp-eln-abi-read-word address 8))
                        (name-address (- name-word 4)))
                   (should (= (logand word 7) 0))
                   (should (= (nelisp-eln-abi-read-word address 16) 48))
                   (should (= (nelisp-eln-abi-read-word address 24) 0))
                   (should (= (nelisp-eln-abi-read-word address 32) 0))
                   (should (= (nelisp-eln-abi-read-word address 40) 0))
                   (should (= (logand name-word 7) 4))
                   (should (equal (nelisp-eln-registration-metadata-decode
                                   token name-word)
                                  (symbol-name
                                   (nelisp-eln-registration-metadata-decode
                                    token word))))
                   (should (= (ptr-read-u64 name-address 0)
                              (length (symbol-name
                                       (nelisp-eln-registration-metadata-decode
                                        token word)))))))
                 )
               (dolist (index '(0 1))
                 (let* ((word (nelisp-eln-registration-metadata-slot-word
                               token 'docs index))
                        (address (- word 4)))
                   (should (= (logand word 7) 4))
                   (should (equal (nelisp-eln-registration-metadata-decode
                                   token word) (aref docs index)))
                   (should (= (ptr-read-u64 address 16) 0))
                   (should (= (ptr-read-u64 address 24) (+ address 32)))
                   (when (= index 1)
                     (should (= (ptr-read-u64 address 0) 1))
                     (should (= (ptr-read-u64 address 8) 3))
                     (should (equal
                              (nelisp-eln-registration-metadata-test--bytes
                               (+ address 32) 3)
                              (string-make-unibyte
                               (encode-coding-string "雪" 'utf-8 t)))))))
               (let ((handle (nelisp-eln-objects-create)))
                 (unwind-protect
                     (should-error
                      (nelisp-eln-objects--encode-word
                       (nelisp-eln-objects--resolve handle) '(unsupported . cons))
                      :type 'nelisp-eln-objects-error)
                   (nelisp-eln-objects-release handle))))
           (nelisp-eln-registration-metadata-release token)))))

    (ert-deftest nelisp-eln-registration-metadata/shared-cyclic-identity-and-mutation-guard ()
      (nelisp-eln-registration-metadata-test--memory
       (let* ((data (make-vector 2 nil))
              (docs (make-vector 1 nil))
              (cell (cons 'leaf nil))
              token)
         (aset data 0 cell)
         (aset data 1 cell)
         (setcdr cell data)
         (aset docs 0 data)
         (setq token (nelisp-eln-registration-metadata-create
                      'gnu-single-leaf data docs))
         (unwind-protect
             (let* ((cons-word (nelisp-eln-registration-metadata-slot-word
                                token 'data 0))
                    (cons-address (- cons-word 3))
                    (cdr-word (nelisp-eln-abi-read-word cons-address 8)))
               (should (= cons-word
                          (nelisp-eln-registration-metadata-slot-word
                           token 'data 1)))
               (should (= cdr-word
                          (nelisp-eln-registration-metadata-data-word token)))
               (should (eq (nelisp-eln-registration-metadata-decode
                            token cdr-word) data))
               (should (eq (nelisp-eln-registration-metadata-decode
                            token cons-word) cell))
               (should (eq (nelisp-eln-registration-metadata-decode
                            token (nelisp-eln-registration-metadata-docs-word token))
                           docs))
               (should-error
                (nelisp-eln-registration-metadata-slot-word
                 token (vector cell cell) 0)
                :type 'nelisp-eln-registration-metadata-error)
               (aset data 0 'changed)
               (should-error
                (nelisp-eln-registration-metadata-slot-word token 'data 0)
                :type 'nelisp-eln-registration-metadata-error)
               (should (eq (nelisp-eln-registration-metadata-decode
                            token (nelisp-eln-registration-metadata-data-word token))
                           data)))
           (nelisp-eln-registration-metadata-release token)))))

    (ert-deftest nelisp-eln-registration-metadata/fixnum-empty-docs-release-and-stale-reuse ()
      (nelisp-eln-registration-metadata-test--memory
       (let* ((data (vector 9))
              (empty (vector))
              (token (nelisp-eln-registration-metadata-create
                      'gnu-single-leaf data empty))
              (arena-token (aref token 1))
              (old-word (nelisp-eln-registration-metadata-data-word token)))
         (should (= (nelisp-eln-registration-metadata-slot-word token 'data 0)
                    (nelisp-eln-abi-encode-fixnum 9)))
         (should (= (nelisp-eln-abi-read-word
                     (- (nelisp-eln-registration-metadata-docs-word token) 5) 0)
                    0))
         (should-error
          (nelisp-eln-registration-metadata-slot-word token empty 0)
          :type 'nelisp-eln-registration-metadata-error)
         (nelisp-eln-registration-metadata-release token)
         (should (cl-every (lambda (index) (null (aref token index)))
                           '(1 2 3 4 5 7)))
         (should (null (aref arena-token 4)))
         (should-error
          (nelisp-eln-registration-metadata-decode token old-word)
          :type 'nelisp-eln-registration-metadata-error)
         (let ((reused (nelisp-eln-registration-metadata-create
                        'gnu-single-leaf (vector 9) empty)))
           (unwind-protect
               (progn
                 (should-not (eq token reused))
                 (should-error
                  (nelisp-eln-registration-metadata-slot-word token 'data 0)
                  :type 'nelisp-eln-registration-metadata-error)
                 (should (= (nelisp-eln-registration-metadata-type-word reused)
                            (nelisp-eln-abi-encode-fixnum 9))))
             (nelisp-eln-registration-metadata-release reused)))))))

(provide 'nelisp-eln-registration-metadata-test)
;;; nelisp-eln-registration-metadata-test.el ends here
