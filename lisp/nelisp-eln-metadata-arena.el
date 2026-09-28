;;; nelisp-eln-metadata-arena.el --- bounded metadata storage -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'nelisp-eln-abi)
(require 'nelisp-eln-objects)
(require 'nl-ffi-memory)
(require 'cl-lib)

(define-error 'nelisp-eln-metadata-arena-error "Invalid metadata arena token")
(define-error 'nelisp-eln-metadata-arena-exhausted "Metadata arena exhausted"
  'nelisp-eln-metadata-arena-error)

(defconst nelisp-eln-metadata-arena--token-marker 'nelisp-eln-metadata-arena-token)
(defconst nelisp-eln-metadata-arena--arena-marker 'nelisp-eln-metadata-arena)
(defconst nelisp-eln-metadata-arena--bytes 65536)
(defvar nelisp-eln-metadata-arena--pool nil)
(defvar nelisp-eln-metadata-arena--live nil)
(defvar nelisp-eln-metadata-arena--base-lease nil)

(defun nelisp-eln-metadata-arena--live-token (token)
  (unless (and (vectorp token) (= (length token) 5)
               (eq (aref token 0) nelisp-eln-metadata-arena--token-marker)
               (eq (aref token 3) 'open)
               (memq token nelisp-eln-metadata-arena--live)
               (aref token 1))
    (signal 'nelisp-eln-metadata-arena-error (list 'stale-token)))
  token)

(defun nelisp-eln-metadata-arena-acquire ()
  "Acquire a fresh live token for one reusable 64 KiB metadata arena."
  (unless nelisp-eln-metadata-arena--base-lease
    (setq nelisp-eln-metadata-arena--base-lease
          (nelisp-eln-objects-symbol-base-acquire)))
  (let ((arena (cl-find-if (lambda (item) (eq (aref item 4) 'idle))
                           nelisp-eln-metadata-arena--pool)))
    (unless arena
      (let ((owner (nl-ffi-memory-allocate nelisp-eln-metadata-arena--bytes)))
        (unless owner
          (signal 'nelisp-eln-metadata-arena-error (list 'allocation-failed)))
        (let ((address (nl-ffi-memory-address owner)))
          (unless (and (integerp address) (> address 0) (= (logand address 7) 0))
            (ignore-errors (nl-ffi-memory-release owner))
            (signal 'nelisp-eln-metadata-arena-error
                    (list 'invalid-arena-address address)))
          (setq arena (vector nelisp-eln-metadata-arena--arena-marker
                              owner address nelisp-eln-metadata-arena--bytes
                              'idle 0 nil))
          (setq nelisp-eln-metadata-arena--pool
                (cons arena nelisp-eln-metadata-arena--pool)))))
    (aset arena 4 'busy)
    (aset arena 5 0)
    (aset arena 6 nil)
    (let ((token (vector nelisp-eln-metadata-arena--token-marker arena nil
                         'open nil)))
      (push token nelisp-eln-metadata-arena--live)
      token)))

(defun nelisp-eln-metadata-arena-reserve (token byte-count &optional alignment)
  "Reserve BYTE-COUNT bytes at ALIGNMENT in live TOKEN; return address."
  (nelisp-eln-metadata-arena--live-token token)
  (setq alignment (or alignment 8))
  (unless (and (integerp byte-count) (> byte-count 0))
    (signal 'nelisp-eln-metadata-arena-error
            (list 'invalid-byte-count byte-count)))
  (unless (and (integerp alignment) (<= 8 alignment 4096)
               (= (logand alignment (1- alignment)) 0))
    (signal 'nelisp-eln-metadata-arena-error
            (list 'invalid-alignment alignment)))
  (let* ((arena (aref token 1))
         (cursor (aref arena 5))
         (base (aref arena 2))
         (absolute (* alignment
                      (/ (+ base cursor (1- alignment)) alignment)))
         (start (- absolute base))
         (end (+ start byte-count)))
    (when (> end (aref arena 3))
      (signal 'nelisp-eln-metadata-arena-exhausted
              (list byte-count alignment cursor (aref arena 3))))
    (aset arena 5 end)
    (+ (aref arena 2) start)))

(defun nelisp-eln-metadata-arena-retain (token object)
  "Keep OBJECT strongly reachable for TOKEN's lifetime; return OBJECT."
  (nelisp-eln-metadata-arena--live-token token)
  (aset token 4 (cons object (aref token 4)))
  object)

(defun nelisp-eln-metadata-arena-release (token)
  "Clear TOKEN roots and return its arena to the reusable pool."
  (nelisp-eln-metadata-arena--live-token token)
  (let ((arena (aref token 1)))
    (setq nelisp-eln-metadata-arena--live
          (delq token nelisp-eln-metadata-arena--live))
    (aset token 1 nil)
    (aset token 2 nil)
    (aset token 3 'closed)
    (aset token 4 nil)
    (aset arena 5 0)
    (aset arena 6 nil)
    (aset arena 4 'idle)
    t))

(defun nelisp-eln-metadata-arena-projected-word-p (word)
  "Recognize retained pool pointers and symbol displacements in WORD."
  (let* ((bits (nelisp-eln-abi-normalize-word word))
         (tag (logand bits 7)))
    (unless (or (= bits 0) (= (logand bits 3) 2))
      (let* ((direct-tag (memq tag '(1 3 4 5 7)))
             (address
              (if direct-tag
                  (logand bits (lognot 7))
                (when (= tag 0)
                  (let* ((signed (if (>= bits (ash 1 63))
                                     (- bits (ash 1 64)) bits))
                         (base (and nelisp-eln-metadata-arena--base-lease
                                    (aref (aref nelisp-eln-metadata-arena--base-lease 1)
                                          1))))
                    (and base (+ base signed)))))))
        (and address
             (cl-some (lambda (arena)
                        (and (<= (aref arena 2) address)
                             (< address (+ (aref arena 2) (aref arena 3)))))
                      nelisp-eln-metadata-arena--pool))))))

(provide 'nelisp-eln-metadata-arena)
;;; nelisp-eln-metadata-arena.el ends here
