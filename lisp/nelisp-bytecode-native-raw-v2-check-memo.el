;;; nelisp-bytecode-native-raw-v2-check-memo.el --- bounded raw-v2 check memo -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Memoize successful raw-v2 checks without replacing the validator.  Callers
;; supply the exact runtime inputs the validator reads (such as ABI,
;; address/root/GC state, and checker version).  Path-specific file SHA checks
;; stay with callers that have the path.  Refusals and uncacheable inputs
;; always run the full validator; this module does not authenticate manifests.

;;; Code:

(defconst nelisp-bytecode-native-raw-v2-check-memo-max-records 32)
(defconst nelisp-bytecode-native-raw-v2-check-memo-max-nodes 20000)
(defconst nelisp-bytecode-native-raw-v2-check-memo-max-depth 256)
(defconst nelisp-bytecode-native-raw-v2-check-memo-max-string-bytes 1048576)
(defconst nelisp-bytecode-native-raw-v2-check-memo-max-vector-length 4096)

(defvar nelisp-bytecode-native-raw-v2-check-memo--records nil)

(defun nelisp-bytecode-native-raw-v2-check-memo--copy-bounded
    (value depth active budget)
  (unless (<= depth nelisp-bytecode-native-raw-v2-check-memo-max-depth)
    (throw 'nelisp-bytecode-native-raw-v2-check-memo-uncacheable nil))
  (aset budget 0 (1+ (aref budget 0)))
  (when (> (aref budget 0) nelisp-bytecode-native-raw-v2-check-memo-max-nodes)
    (throw 'nelisp-bytecode-native-raw-v2-check-memo-uncacheable nil))
  (cond
   ((stringp value)
    (aset budget 1 (+ (aref budget 1) (string-bytes value)))
    (when (> (aref budget 1)
             nelisp-bytecode-native-raw-v2-check-memo-max-string-bytes)
      (throw 'nelisp-bytecode-native-raw-v2-check-memo-uncacheable nil))
    (copy-sequence value))
   ((consp value)
    (when (memq value active)
      (throw 'nelisp-bytecode-native-raw-v2-check-memo-uncacheable nil))
    (let ((next-active (cons value active)))
      (cons (nelisp-bytecode-native-raw-v2-check-memo--copy-bounded
             (car value) (1+ depth) next-active budget)
            (nelisp-bytecode-native-raw-v2-check-memo--copy-bounded
             (cdr value) (1+ depth) next-active budget))))
   ((vectorp value)
    (when (or (memq value active)
              (> (length value)
                 nelisp-bytecode-native-raw-v2-check-memo-max-vector-length))
      (throw 'nelisp-bytecode-native-raw-v2-check-memo-uncacheable nil))
    (let* ((next-active (cons value active))
           (copy (make-vector (length value) nil)))
      (dotimes (index (length value))
        (aset copy index
              (nelisp-bytecode-native-raw-v2-check-memo--copy-bounded
               (aref value index) (1+ depth) next-active budget)))
      copy))
   ((or (symbolp value) (integerp value) (floatp value)) value)
   (t (throw 'nelisp-bytecode-native-raw-v2-check-memo-uncacheable nil))))

(defun nelisp-bytecode-native-raw-v2-check-memo--snapshot (value)
  (catch 'nelisp-bytecode-native-raw-v2-check-memo-uncacheable
    (vector 'bounded-snapshot
            (nelisp-bytecode-native-raw-v2-check-memo--copy-bounded
             value 0 nil (vector 0 0)))))

(defun nelisp-bytecode-native-raw-v2-check-memo--validator-identity (validator)
  (if (and (symbolp validator) (fboundp validator))
      (symbol-function validator)
    validator))

(defun nelisp-bytecode-native-raw-v2-check-memo-clear ()
  "Clear successful raw-v2 check memo records."
  (setq nelisp-bytecode-native-raw-v2-check-memo--records nil))

(defun nelisp-bytecode-native-raw-v2-check-memo-run
    (manifest runtime-key validator &optional name)
  "Run VALIDATOR on MANIFEST, memoizing only a nil refusal result.

RUNTIME-KEY must include every non-manifest fact used by VALIDATOR, such as
runtime ABI, address/root/GC state, and checker version.  If VALIDATOR has an
artifact path, its caller remains responsible for the current file SHA and
mapping checks.  VALIDATOR receives MANIFEST and NAME and returns nil on
success or refusal reasons otherwise.  Inputs outside the bounded data
subset bypass memoization and are passed unchanged to VALIDATOR."
  (let* ((data (list manifest runtime-key name))
         (snapshot (nelisp-bytecode-native-raw-v2-check-memo--snapshot data))
         (identity (nelisp-bytecode-native-raw-v2-check-memo--validator-identity
                    validator))
         (hit nil))
    (when snapshot
      (dolist (record nelisp-bytecode-native-raw-v2-check-memo--records)
        (when (and (eq identity (car record))
                   (equal snapshot (cadr record)))
          (setq hit t))))
    (if hit
        nil
      (let ((refusals (funcall validator manifest name)))
        (when (and snapshot (null refusals)
                   (eq identity
                       (nelisp-bytecode-native-raw-v2-check-memo--validator-identity
                        validator))
                   (equal snapshot
                          (nelisp-bytecode-native-raw-v2-check-memo--snapshot
                           (list manifest runtime-key name))))
          (push (list identity snapshot)
                nelisp-bytecode-native-raw-v2-check-memo--records)
          (when (> (length nelisp-bytecode-native-raw-v2-check-memo--records)
                   nelisp-bytecode-native-raw-v2-check-memo-max-records)
            (setcdr (nthcdr (1- nelisp-bytecode-native-raw-v2-check-memo-max-records)
                            nelisp-bytecode-native-raw-v2-check-memo--records)
                    nil)))
        refusals))))

(provide 'nelisp-bytecode-native-raw-v2-check-memo)
;;; nelisp-bytecode-native-raw-v2-check-memo.el ends here
