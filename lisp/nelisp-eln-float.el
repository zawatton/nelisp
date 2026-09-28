;;; nelisp-eln-float.el --- owned GNU Lisp_Float views -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A temporary GNU Emacs 31.1 Lisp_Float backed by an owned 8-byte mapping.
;; The source float remains rooted by its owner until release. Its payload is
;; copied as raw IEEE-754 bits by the private NeLisp builtin, never arithmetic.

;;; Code:

(require 'nl-ffi-memory)
(require 'nelisp-eln-abi)

(declare-function ptr-write-u32 "ext:nelisp-runtime" (ptr offset value))
(declare-function nelisp--float-word-half "ext:nelisp-runtime" (value half))

(define-error 'nelisp-eln-float-error "Invalid GNU Lisp_Float view")
(defconst nelisp-eln-float--owner-marker 'nelisp-eln-float-owner)
(defconst nelisp-eln-float--u32-max #xffffffff)
(defvar nelisp-eln-float--live nil)
(defvar nelisp-eln-float--pending-cleanups nil)

(defun nelisp-eln-float--fail (reason &optional detail)
  (signal 'nelisp-eln-float-error (list reason detail)))

(defun nelisp-eln-float--preflight (source)
  "Return the exact low/high payload limbs for float SOURCE."
  (unless (floatp source) (nelisp-eln-float--fail 'not-a-float source))
  (unless (fboundp 'nelisp--float-word-half)
    (nelisp-eln-float--fail 'raw-float-transport-unavailable))
  (let ((low (nelisp--float-word-half source 0))
        (high (nelisp--float-word-half source 1)))
    (unless (and (integerp low) (<= 0 low nelisp-eln-float--u32-max)
                 (integerp high) (<= 0 high nelisp-eln-float--u32-max))
      (nelisp-eln-float--fail 'invalid-payload-limb (list low high)))
    (vector low high)))

(defun nelisp-eln-float-allocate (source)
  "Create an owned GNU Lisp_Float view of SOURCE."
  (let* ((limbs (nelisp-eln-float--preflight source))
         (external nil) (base nil) (owner nil) (failure nil))
    (condition-case err
        (progn
          (setq external (nl-ffi-memory-allocate 8)
                base (nl-ffi-memory-address external))
          (unless (and (integerp base) (> base 0) (= (logand base 7) 0))
            (nelisp-eln-float--fail 'invalid-aligned-address base))
          (ptr-write-u32 base 0 (aref limbs 0))
          (ptr-write-u32 base 4 (aref limbs 1))
          (setq owner (vector nelisp-eln-float--owner-marker source external
                              base 'open)
                nelisp-eln-float--live (cons owner nelisp-eln-float--live)))
      (error (setq failure err)))
    (when failure
      (when external
        (condition-case nil
            (nl-ffi-memory-release external)
          (error (push (cons 'memory external)
                       nelisp-eln-float--pending-cleanups))))
      (signal (car failure) (cdr failure)))
    owner))

(defun nelisp-eln-float--live-base (owner)
  (unless (and (vectorp owner) (= (length owner) 5)
               (eq (aref owner 0) nelisp-eln-float--owner-marker)
               (eq (aref owner 4) 'open)
               (memq owner nelisp-eln-float--live))
    (nelisp-eln-float--fail 'closed-or-invalid-owner))
  (let ((base (aref owner 3)))
    (unless (and (integerp base) (> base 0) (= (logand base 7) 0))
      (nelisp-eln-float--fail 'invalid-aligned-address base))
    base))

(defun nelisp-eln-float-source (owner)
  "Return the exact source float retained by live OWNER."
  (nelisp-eln-float--live-base owner)
  (aref owner 1))

(defun nelisp-eln-float-address (owner)
  "Return the live GNU Lisp_Float address held by OWNER."
  (nelisp-eln-float--live-base owner))

(defun nelisp-eln-float-word (owner)
  "Return OWNER's GNU tag-7 Lisp_Object word."
  (logior (nelisp-eln-float-address owner) 7))

(defun nelisp-eln-float-release (owner)
  "Release OWNER's external mapping. Releasing a closed owner is harmless."
  (unless (and (vectorp owner) (= (length owner) 5)
               (eq (aref owner 0) nelisp-eln-float--owner-marker))
    (nelisp-eln-float--fail 'invalid-owner owner))
  (when (eq (aref owner 4) 'open)
    (unless (memq owner nelisp-eln-float--live)
      (nelisp-eln-float--fail 'unregistered-owner owner))
    (setq nelisp-eln-float--live (delq owner nelisp-eln-float--live))
    (aset owner 4 'closed)
    (let ((external (aref owner 2)))
      (aset owner 1 nil)
      (aset owner 2 nil)
      (aset owner 3 nil)
      (condition-case err
          (nl-ffi-memory-release external)
        (error
         (aset owner 2 external)
         (aset owner 4 'cleanup-pending)
         (push owner nelisp-eln-float--pending-cleanups)
         (signal (car err) (cdr err))))))
  t)

(defun nelisp-eln-float-retry-pending-cleanup ()
  "Retry external mapping releases that previously failed."
  (let ((remaining nil))
    (dolist (entry (copy-sequence nelisp-eln-float--pending-cleanups))
      (condition-case nil
          (progn
            (if (and (vectorp entry)
                     (eq (aref entry 0) nelisp-eln-float--owner-marker))
                (progn
                  (nl-ffi-memory-release (aref entry 2))
                  (aset entry 2 nil)
                  (aset entry 4 'closed))
              (nl-ffi-memory-release (cdr entry)))
            (setq nelisp-eln-float--pending-cleanups
                  (delq entry nelisp-eln-float--pending-cleanups)))
        (error (push entry remaining))))
    (setq nelisp-eln-float--pending-cleanups (nreverse remaining)))
  t)

(provide 'nelisp-eln-float)
;;; nelisp-eln-float.el ends here
