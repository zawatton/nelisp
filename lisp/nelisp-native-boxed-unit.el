;;; nelisp-native-boxed-unit.el --- checked boxed .neln calls -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This adapter admits `.neln' entries whose explicit repr metadata declares
;; boxed Sexp pointers for every argument and the return value. The legacy
;; unit ABI classifier may say `integer' for an extern-free leaf; exact repr
;; metadata settles that narrow case. A unit can retain a
;; vector of Lisp constants; calls pass those constants as hidden leading
;; arguments through the loader's GC-pinned boundary. The unit's live-list
;; reference keeps the vector reachable until close.

;;; Code:

(require 'nelisp-native-load)

(defconst nelisp-native-boxed-unit--marker
  (make-symbol "nelisp-native-boxed-unit"))

(defvar nelisp-native-boxed-unit--live nil)

(defun nelisp-native-boxed-unit--unit (unit)
  (unless (and (vectorp unit)
               (= (length unit) 7)
               (eq (aref unit 0) nelisp-native-boxed-unit--marker)
               (eq (aref unit 6) 'open)
               (memq unit nelisp-native-boxed-unit--live))
    (error "native-boxed-unit: invalid or closed unit"))
  unit)

(defun nelisp-native-boxed-unit--handle (unit)
  (aref (nelisp-native-boxed-unit--unit unit) 1))

(defun nelisp-native-boxed-unit--manifest-entry (manifest name)
  "Find NAME in the public MANIFEST plist's native defun entries."
  (let ((entries (plist-get (plist-get manifest :native) :defuns))
        (found nil))
    (while (and entries (not found))
      (when (equal (plist-get (car entries) :name) name)
        (setq found (car entries)))
      (setq entries (cdr entries)))
    found))

(defun nelisp-native-boxed-unit--vector-list (vector)
  (let ((i (length vector)) (result nil))
    (while (> i 0)
      (setq i (1- i)
            result (cons (aref vector i) result)))
    result))

(defun nelisp-native-boxed-unit-open-with-constants
    (artifact name constants user-arity &optional minimum-user-arity
              rest-required-count)
  "Load NAME from .neln ARTIFACT with hidden CONSTANTS and USER-ARITY.

CONSTANTS must be a vector. The unit retains that vector until close and
passes its elements as leading boxed arguments. The artifact's declared
arity must equal the vector length plus USER-ARITY. Missing arguments down to
MINIMUM-USER-ARITY are supplied as nil."
  (setq minimum-user-arity (or minimum-user-arity user-arity))
  (unless (and (vectorp constants) (integerp user-arity) (>= user-arity 0)
               (integerp minimum-user-arity) (>= minimum-user-arity 0)
               (<= minimum-user-arity user-arity))
    (error "native-boxed-unit: expected constant vector and nonnegative arity"))
  (let ((handle (nelisp-native-load-artifact artifact name))
        (accepted nil))
    (unwind-protect
        (progn
          (unless (and (or (eq (plist-get handle :abi) 'boxed)
                           (and (eq (plist-get handle :abi) 'integer)
                                (eq (plist-get handle :param-repr) 'sexp-ptr)
                                (eq (plist-get handle :return-repr) 'sexp-ptr)))
                       (eq (plist-get handle :param-repr) 'sexp-ptr)
                       (eq (plist-get handle :return-repr) 'sexp-ptr)
                       (integerp (plist-get handle :arity)))
            (error "native-boxed-unit: entry lacks explicit boxed Sexp-to-Sexp repr: %S"
                   (list (plist-get handle :abi)
                         (plist-get handle :param-repr)
                         (plist-get handle :return-repr))))
          (unless (= (plist-get handle :arity)
                     (+ (length constants) user-arity))
            (error "native-boxed-unit: declared arity %d differs from %d hidden + %d user arguments"
                   (plist-get handle :arity) (length constants) user-arity))
          (unless (equal (plist-get handle :rest-required-count)
                         rest-required-count)
            (error "native-boxed-unit: REST call metadata mismatch (%S/%S)"
                   (plist-get handle :rest-required-count)
                   rest-required-count))
          (when rest-required-count
            (unless (and (integerp rest-required-count)
                         (>= rest-required-count 0)
                         (= user-arity (1+ rest-required-count))
                         (= minimum-user-arity user-arity))
              (error "native-boxed-unit: invalid fixed REST transport arity")))
          (let ((unit (vector nelisp-native-boxed-unit--marker handle
                              constants user-arity minimum-user-arity
                              rest-required-count 'open)))
            (push unit nelisp-native-boxed-unit--live)
            (setq accepted t)
            unit))
      (unless accepted
        (ignore-errors (nelisp-native-load-unload handle))))))

(defun nelisp-native-boxed-unit-open-rest
    (artifact name constants required-count)
  "Open NAME as a fixed boxed entry receiving REQUIRED-COUNT args and one REST list."
  (unless (and (integerp required-count) (>= required-count 0))
    (error "native-boxed-unit: invalid REST required count"))
  (nelisp-native-boxed-unit-open-with-constants
   artifact name constants (1+ required-count) (1+ required-count)
   required-count))

(defun nelisp-native-boxed-unit-open (artifact name)
  "Load boxed NAME from ARTIFACT with no hidden arguments."
  (let* ((manifest (nelisp-native-load-manifest artifact))
         (entry (nelisp-native-boxed-unit--manifest-entry manifest name))
         (arity (and entry (plist-get entry :arity))))
    (nelisp-native-boxed-unit-open-with-constants
     artifact name [] arity)))

(defun nelisp-native-boxed-unit-call (unit arguments)
  "Call UNIT with user ARGUMENTS after its hidden boxed constants."
  (let* ((unit (nelisp-native-boxed-unit--unit unit))
         (rest-required-count (aref unit 5))
         (hidden (nelisp-native-boxed-unit--vector-list (aref unit 2)))
         (user-arity (aref unit 3))
         (minimum-user-arity (aref unit 4)))
    (when (integerp rest-required-count)
      (error "native-boxed-unit: REST unit requires call-rest"))
    (unless (and (listp arguments)
                 (<= minimum-user-arity (length arguments) user-arity))
      (if (= minimum-user-arity user-arity)
          (error "native-boxed-unit: expected %d user argument(s), got %s"
                 user-arity
                 (if (listp arguments) (length arguments) "non-list"))
        (error "native-boxed-unit: expected %d..%d user argument(s), got %s"
               minimum-user-arity user-arity
               (if (listp arguments) (length arguments) "non-list"))))
    (nelisp-native-load-call
     (aref unit 1)
     (append hidden arguments (make-list (- user-arity (length arguments)) nil)))))

(defun nelisp-native-boxed-unit-call-rest (unit evaluator-arguments)
  "Call REST UNIT using original EVALUATOR-ARGUMENTS.
Pack all values after the required prefix into a fresh list. The loader pins
that list as one boxed argument before entering native code."
  (let* ((unit (nelisp-native-boxed-unit--unit unit))
         (required-count (aref unit 5))
         (user-arity (aref unit 3)))
    (unless (and (integerp required-count)
                 (= user-arity (1+ required-count)))
      (error "native-boxed-unit: unit has no authenticated REST ABI"))
    (unless (and (listp evaluator-arguments)
                 (>= (length evaluator-arguments) required-count))
      (error "native-boxed-unit: REST call has fewer than %d required argument(s)"
             required-count))
    (let ((prefix nil)
          (tail evaluator-arguments)
          (remaining required-count))
      (while (> remaining 0)
        (push (car tail) prefix)
        (setq tail (cdr tail)
              remaining (1- remaining)))
      (nelisp-native-load-call
       (aref unit 1)
       (append (nelisp-native-boxed-unit--vector-list (aref unit 2))
               (nreverse prefix)
               (list (copy-sequence tail)))))))

(defun nelisp-native-boxed-unit-close (unit)
  "Close UNIT, releasing its rooted constants and native entry."
  (let* ((unit (nelisp-native-boxed-unit--unit unit))
         (handle (aref unit 1)))
    (prog1 (nelisp-native-load-unload handle)
      (aset unit 1 nil)
      (aset unit 2 nil)
      (aset unit 3 nil)
      (aset unit 4 nil)
      (aset unit 5 nil)
      (aset unit 6 'closed)
      (setq nelisp-native-boxed-unit--live
            (delq unit nelisp-native-boxed-unit--live)))))

(provide 'nelisp-native-boxed-unit)
;;; nelisp-native-boxed-unit.el ends here
