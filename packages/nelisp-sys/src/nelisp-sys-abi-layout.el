;;; nelisp-sys-abi-layout.el --- Data layout engine for nelisp-sys -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Stage 132.2 + 130.2: the data layout engine.  Computes sizeof / alignof
;; / offsetof / struct layout for nelisp-sys types from a TARGET descriptor
;; (Doc 132) and a struct ENV (Doc 131).  All target-specific facts come
;; from the descriptor; the engine never consults the host.
;;
;; Struct layout follows the :repr c rules (Doc 132): source-order fields,
;; each at the next aligned offset, struct alignment = max field alignment,
;; final size rounded up to struct alignment.

;;; Code:

(require 'cl-lib)
(require 'nelisp-sys)
(require 'nelisp-sys-types)
(require 'nelisp-sys-target)

(define-error 'nelisp-sys-layout-error
  "nelisp-sys layout error" 'nelisp-sys-error)

(defun nelisp-sys-layout--round-up (n align)
  "Round N up to the next multiple of ALIGN (ALIGN 0/1 returns N)."
  (if (<= align 1) n
    (* align (/ (+ n align -1) align))))

(defun nelisp-sys-layout--scalar-bytes (type target)
  "Return (SIZE . ALIGN) byte pair for scalar TYPE on TARGET."
  (let* ((meta (nelisp-sys-type-scalar-meta type))
         (bits (plist-get meta :bits)))
    (if (eq bits 'ptr)
        (cons (/ (nelisp-sys-target-pointer-width target) 8)
              (nelisp-sys-target-pointer-align target))
      (let ((bytes (/ bits 8)))
        ;; void is the only zero-size scalar; give it alignment 1.
        (cons bytes (max bytes 1))))))

(defun nelisp-sys-layout-alignof (type target &optional env)
  "Return the alignment in bytes of TYPE on TARGET under struct ENV."
  (setq target (nelisp-sys-target-get target))
  (cond
   ((nelisp-sys-type-scalar-p type)
    (cdr (nelisp-sys-layout--scalar-bytes type target)))
   ((or (nelisp-sys-type-pointer-p type)
        (nelisp-sys-type-ref-p type)
        (nelisp-sys-type-slice-p type))
    (nelisp-sys-target-pointer-align target))
   ((nelisp-sys-type-owned-p type)
    (nelisp-sys-layout-alignof (nelisp-sys-type-element type) target env))
   ((nelisp-sys-type-array-p type)
    (nelisp-sys-layout-alignof (nth 1 type) target env))
   ((nelisp-sys-type-struct-ref-p type)
    (plist-get (nelisp-sys-layout-struct (nth 1 type) target env) :align))
   (t (signal 'nelisp-sys-layout-error
              (list (format "no alignment for type: %S" type))))))

(defun nelisp-sys-layout-sizeof (type target &optional env)
  "Return the size in bytes of TYPE on TARGET under struct ENV."
  (setq target (nelisp-sys-target-get target))
  (cond
   ((nelisp-sys-type-scalar-p type)
    (car (nelisp-sys-layout--scalar-bytes type target)))
   ((or (nelisp-sys-type-pointer-p type)
        (nelisp-sys-type-ref-p type))
    (/ (nelisp-sys-target-pointer-width target) 8))
   ((nelisp-sys-type-slice-p type)
    ;; ptr + usize length
    (* 2 (/ (nelisp-sys-target-pointer-width target) 8)))
   ((nelisp-sys-type-owned-p type)
    (nelisp-sys-layout-sizeof (nelisp-sys-type-element type) target env))
   ((nelisp-sys-type-array-p type)
    ;; C array stride = element sizeof (already includes trailing pad).
    (* (nth 2 type) (nelisp-sys-layout-sizeof (nth 1 type) target env)))
   ((nelisp-sys-type-struct-ref-p type)
    (plist-get (nelisp-sys-layout-struct (nth 1 type) target env) :size))
   (t (signal 'nelisp-sys-layout-error
              (list (format "no size for type: %S" type))))))

(defun nelisp-sys-layout-struct (name target &optional env seen)
  "Return the layout of struct NAME on TARGET under struct ENV.
Result plist: (:size N :align A :repr R
               :fields ((FIELD :type T :offset O :size S :align A)...)).
SEEN guards against by-value recursive structs."
  (setq target (nelisp-sys-target-get target))
  (let ((def (nelisp-sys-types-env-get env name)))
    (unless def
      (signal 'nelisp-sys-layout-error
              (list (format "unknown struct: %S" name))))
    (when (memq name seen)
      (signal 'nelisp-sys-layout-error
              (list (format "recursive struct (infinite size): %S" name))))
    (let* ((seen (cons name seen))
           (repr (plist-get def :repr))
           (fields (plist-get def :fields))
           (offset 0)
           (max-align 1)
           (out '()))
      (unless (memq repr '(c sys))
        (signal 'nelisp-sys-layout-error
                (list (format "unsupported :repr %S for struct %S (MVP: c|sys)"
                              repr name))))
      (dolist (f fields)
        (let* ((ftype (cdr f))
               (fsize (if (nelisp-sys-type-struct-ref-p ftype)
                          (plist-get (nelisp-sys-layout-struct
                                      (nth 1 ftype) target env seen)
                                     :size)
                        (nelisp-sys-layout-sizeof ftype target env)))
               (falign (if (nelisp-sys-type-struct-ref-p ftype)
                           (plist-get (nelisp-sys-layout-struct
                                       (nth 1 ftype) target env seen)
                                      :align)
                         (nelisp-sys-layout-alignof ftype target env))))
          (setq offset (nelisp-sys-layout--round-up offset falign))
          (push (list (car f) :type ftype :offset offset
                      :size fsize :align falign)
                out)
          (setq offset (+ offset fsize))
          (setq max-align (max max-align falign))))
      (list :size (nelisp-sys-layout--round-up offset max-align)
            :align max-align
            :repr repr
            :fields (nreverse out)))))

(defun nelisp-sys-layout-offsetof (name field target &optional env)
  "Return the byte offset of FIELD in struct NAME on TARGET under ENV."
  (let* ((layout (nelisp-sys-layout-struct name target env))
         (entry (assq field (plist-get layout :fields))))
    (unless entry
      (signal 'nelisp-sys-layout-error
              (list (format "struct %S has no field %S" name field))))
    (plist-get (cdr entry) :offset)))

(provide 'nelisp-sys-abi-layout)

;;; nelisp-sys-abi-layout.el ends here
