;;; nelisp-eln-registration-vectors.el --- GNU flat vector views -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This narrow registration codec maps flat ordinary vectors to GNU Emacs
;; 31.1 vector objects.  It deliberately rejects nested/unsupported graphs.
;; Identity is canonical within one vector unit. A source already leased by a
;; different unit is rejected because its child words belong to that unit's
;; object codec. Live records are explicit strong roots; no weak-hash semantics
;; are assumed.

;;; Code:

(require 'nelisp-eln-abi)
(require 'nelisp-eln-objects)
(require 'nl-ffi-memory)

(define-error 'nelisp-eln-registration-vectors-error
  "Unsupported GNU registration vector")

(defconst nelisp-eln-registration-vectors--unit-marker
  'nelisp-eln-registration-vectors-unit)
(defconst nelisp-eln-registration-vectors--activation-marker
  'nelisp-eln-registration-vectors-activation)
(defvar nelisp-eln-registration-vectors--units nil)

(defun nelisp-eln-registration-vectors--fail (reason &optional value)
  (signal 'nelisp-eln-registration-vectors-error (list reason value)))

(defun nelisp-eln-registration-vectors-create-unit (objects-unit)
  "Create vector owner using OBJECTS-UNIT for element words."
  (nelisp-eln-objects--check-unit objects-unit)
  (let ((token (vector nelisp-eln-registration-vectors--unit-marker
                       objects-unit 'open nil nil)))
    (push token nelisp-eln-registration-vectors--units)
    token))

(defun nelisp-eln-registration-vectors--unit (token)
  (unless (and (memq token nelisp-eln-registration-vectors--units)
               (eq (aref token 0)
                   nelisp-eln-registration-vectors--unit-marker)
               (eq (aref token 2) 'open))
    (nelisp-eln-registration-vectors--fail 'stale-unit token))
  token)

(defun nelisp-eln-registration-vectors--flat-slots (vector)
  "Validate VECTOR's entire graph before any allocation, returning slot words."
  (unless (and (vectorp vector) (not (stringp vector))
               (not (bool-vector-p vector)) (not (recordp vector)))
    (nelisp-eln-registration-vectors--fail 'not-plain-vector vector))
  (let ((i 0) (n (length vector)) (words nil))
    (while (< i n)
      (let ((value (aref vector i)))
        (when (or (vectorp value) (hash-table-p value) (recordp value)
                  (bool-vector-p value))
          (nelisp-eln-registration-vectors--fail 'nested-or-unsupported value))
        (push value words))
      (setq i (1+ i)))
    (nreverse words)))

(defun nelisp-eln-registration-vectors--find (unit source)
  (let ((records (aref unit 3)) found)
    (while (and records (not found))
      (if (eq source (aref (car records) 0))
          (setq found (car records))
        (setq records (cdr records))))
    found))

(defun nelisp-eln-registration-vectors--other-unit-owns-p (unit source)
  (let ((units nelisp-eln-registration-vectors--units) found)
    (while (and units (not found))
      (unless (eq (car units) unit)
        (setq found
              (nelisp-eln-registration-vectors--find (car units) source)))
      (setq units (cdr units)))
    found))

(defun nelisp-eln-registration-vectors--same-slots-p (left right)
  (and (= (length left) (length right))
       (let ((a left) (b right) (same t))
         (while (and a same)
           (setq same (eql (car a) (car b))
                 a (cdr a) b (cdr b)))
         same)))

(defun nelisp-eln-registration-vectors-register (token source)
  "Return GNU vector word for flat SOURCE, stable by source identity."
  (setq token (nelisp-eln-registration-vectors--unit token))
  (let* ((objects-unit (aref token 1))
         (slots (nelisp-eln-registration-vectors--flat-slots source))
         (old (nelisp-eln-registration-vectors--find token source)))
    (if old
        (progn
          (unless (nelisp-eln-registration-vectors--same-slots-p
                   slots (aref old 4))
            (nelisp-eln-registration-vectors--fail 'source-changed source))
          (aref old 1))
      (when (nelisp-eln-registration-vectors--other-unit-owns-p token source)
        (nelisp-eln-registration-vectors--fail 'source-owned-by-other-unit
                                               source))
      ;; Validate nested cons graphs and all unsupported descendants before
      ;; reserving the GNU vector object.
      (nelisp-eln-objects--preflight
       slots (nelisp-eln-objects--resolve objects-unit))
      (let* ((n (length slots))
             (owner (nl-ffi-memory-allocate (* 8 (1+ n))))
             (address (nl-ffi-memory-address owner))
             (words nil) (record nil))
        ;; The complete graph was checked above. Encode elements before native
        ;; allocation contents become reachable; codec failure frees storage.
        (condition-case failure
            (progn
              (dolist (value slots)
                (push (nelisp-eln-objects-encode objects-unit value) words))
              (setq words (nreverse words))
              (nelisp-eln-abi-write-word address 0 n)
              (let ((i 0) (rest words))
                (while rest
                  (nelisp-eln-abi-write-word address (* 8 (1+ i)) (car rest))
                  (setq i (1+ i) rest (cdr rest))))
              (setq record (vector source (+ address 5) owner address slots
                                   words 'open 0))
              (aset token 3 (cons record (aref token 3)))
              (+ address 5))
          (error
           (condition-case cleanup-failure
               (nl-ffi-memory-release owner)
             (error
              ;; Keep the allocation rooted and retryable. The caller's unit
              ;; cleanup owns this record even though registration failed.
              (setq record (vector source (+ address 5) owner address slots
                                   words 'releasing 0))
              (aset token 3 (cons record (aref token 3)))))
           (signal (car failure) (cdr failure))))))))

(defun nelisp-eln-registration-vectors--validate-record (record)
  (let* ((address (aref record 3))
         (source (aref record 0))
         (slots (nelisp-eln-registration-vectors--flat-slots source))
         (expected (aref record 5))
         (i 0))
    (unless (nelisp-eln-registration-vectors--same-slots-p
             slots (aref record 4))
      (nelisp-eln-registration-vectors--fail 'source-changed source))
    (unless (= (nelisp-eln-abi-read-word address 0) (length expected))
      (nelisp-eln-registration-vectors--fail 'native-header-changed source))
    (while (< i (length expected))
      (unless (= (nelisp-eln-abi-read-word address (* 8 (1+ i)))
                 (nth i expected))
        (nelisp-eln-registration-vectors--fail 'native-slot-changed i))
      (setq i (1+ i)))
    t))

(defun nelisp-eln-registration-vectors-begin-activation (token)
  "Lease all vector records and their codec children for one activation."
  (setq token (nelisp-eln-registration-vectors--unit token))
  (let ((records (aref token 3)) (children nil) (leased nil))
    (dolist (record records) (nelisp-eln-registration-vectors--validate-record record))
    (setq children
          (nelisp-eln-objects-activation-acquire (aref token 1)))
    (condition-case failure
        (progn
          (dolist (record records)
            (aset record 7 (1+ (aref record 7)))
            (push record leased))
          (let ((activation (vector nelisp-eln-registration-vectors--activation-marker
                                    token 'open records children nil)))
            (aset token 4 (cons activation (aref token 4)))
            activation))
      (error
       (dolist (record leased) (aset record 7 (1- (aref record 7))))
       (nelisp-eln-objects-activation-release children)
       (signal (car failure) (cdr failure))))))

(defun nelisp-eln-registration-vectors-activation-decode (activation word)
  "Decode WORD under ACTIVATION, including vector tag 5."
  (unless (and (vectorp activation)
               (eq (aref activation 0)
                   nelisp-eln-registration-vectors--activation-marker)
               (eq (aref activation 2) 'open))
    (nelisp-eln-registration-vectors--fail 'stale-activation activation))
  (let ((records (aref activation 3)) found)
    (while (and records (not found))
      (let ((record (car records)))
        (when (= word (aref record 1)) (setq found record)))
      (setq records (cdr records)))
    (if found
        (progn
          (nelisp-eln-registration-vectors--validate-record found)
          (aref found 0))
      (nelisp-eln-objects-activation-decode (aref activation 4) word))))

(defun nelisp-eln-registration-vectors-end-activation (activation)
  "Release vector and child-codec leases for ACTIVATION."
  (unless (and (vectorp activation)
               (eq (aref activation 0)
                   nelisp-eln-registration-vectors--activation-marker)
               (memq (aref activation 2) '(open closing)))
    (nelisp-eln-registration-vectors--fail 'stale-activation activation))
  (when (eq (aref activation 2) 'open)
    (aset activation 2 'closing)
    (dolist (record (aref activation 3))
      (aset record 7 (1- (aref record 7))))
    (aset activation 5 t))
  (nelisp-eln-objects-activation-release (aref activation 4))
  (aset activation 4 nil)
  (let ((unit (aref activation 1)))
    (aset unit 4 (delq activation (aref unit 4))))
  (aset activation 2 'closed)
  t)

(defun nelisp-eln-registration-vectors-release-unit (token)
  "Release TOKEN's source-vector roots and native allocations."
  (unless (and (memq token nelisp-eln-registration-vectors--units)
               (eq (aref token 0)
                   nelisp-eln-registration-vectors--unit-marker)
               (memq (aref token 2) '(open closing)))
    (nelisp-eln-registration-vectors--fail 'stale-unit token))
  (when (aref token 4)
    (nelisp-eln-registration-vectors--fail 'active-leases token))
  (aset token 2 'closing)
  (while (aref token 3)
    (let ((record (car (aref token 3))))
    (when (> (aref record 7) 0)
      (nelisp-eln-registration-vectors--fail 'active-record record))
      (when (aref record 2)
        (nl-ffi-memory-release (aref record 2))
        (aset record 2 nil))
      (aset record 6 'closed)
      (aset token 3 (cdr (aref token 3)))))
  (setq nelisp-eln-registration-vectors--units
        (delq token nelisp-eln-registration-vectors--units))
  (aset token 2 'closed)
  t)

(provide 'nelisp-eln-registration-vectors)

;;; nelisp-eln-registration-vectors.el ends here
