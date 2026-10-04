;;; nelisp-bytecode-native-compiler-raw.el --- Public raw byte-code compiler -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Compile verified, materialized GNU 31.1 byte-code functions through the
;; raw-i64 CFG backend.  No source reconstruction or function execution occurs.

;;; Code:

(require 'cl-lib)
(require 'nelisp-bytecode-compiler-input)
(require 'nelisp-bytecode-native-cfg)

(defconst nelisp-bytecode-native-compiler-raw--source
  (file-truename (or load-file-name buffer-file-name)))

(defun nelisp-bytecode-native-compiler-raw--producer (value blocks)
  "Return (BLOCK . INSTRUCTION) producing VALUE, or nil."
  (catch 'found
    (dolist (block (append blocks nil))
      (dolist (instruction (append (plist-get block :instructions) nil))
        (when (member value (append (plist-get instruction :outputs) nil))
          (throw 'found (cons block instruction)))))))

(defun nelisp-bytecode-native-compiler-raw--integer-value-p
    (value block blocks constants arity seen)
  "Prove VALUE is an integer raw slot with a grounded path through cycles.
The result is a cons of (all paths valid . at least one grounded path)."
  (let ((key (cons (plist-get block :start) value)))
    (if (member key seen)
        (cons t nil)
      (let ((seen (cons key seen)))
        (if (and (consp value) (eq (car value) :entry))
            (let ((pc (nth 1 value)) (slot (nth 2 value)))
              (if (= pc 0)
                  (and (integerp slot) (<= 0 slot) (< slot arity)
                       (cons t t))
                (let ((incoming nil))
                  (dolist (predecessor (append blocks nil))
                    (dolist (edge (append (plist-get predecessor :successors) nil))
                      (when (= (plist-get edge :target) pc)
                        (cl-mapc
                         (lambda (source target-slot)
                           (when (equal target-slot value)
                             (push (cons predecessor source) incoming)))
                         (append (plist-get edge :slots) nil)
                         (append (plist-get edge :target-slots) nil)))))
                  (when incoming
                    (let ((valid t) (grounded nil))
                      (dolist (source incoming)
                        (let ((proof
                               (nelisp-bytecode-native-compiler-raw--integer-value-p
                                (cdr source) (car source) blocks constants arity seen)))
                          (unless (and (consp proof) (car proof))
                            (setq valid nil))
                          (when (and (consp proof) (cdr proof))
                            (setq grounded t))))
                      (cons valid grounded))))))
          (let ((producer
                 (nelisp-bytecode-native-compiler-raw--producer value blocks)))
            (when producer
              (let ((instruction (cdr producer)))
                (pcase (plist-get instruction :kind)
                  ('constant
                   (let ((index (plist-get instruction :constant-index)))
                     (and (integerp index) (<= 0 index)
                          (< index (length constants))
                          (integerp (aref constants index))
                          (<= most-negative-fixnum (aref constants index))
                          (<= (aref constants index) most-positive-fixnum)
                          (cons t t))))
                  ((or 'stack-ref 'dup)
                   (nelisp-bytecode-native-compiler-raw--integer-value-p
                    (car (plist-get instruction :inputs)) (car producer)
                    blocks constants arity seen))
                  (_ nil))))))))))

(defun nelisp-bytecode-native-compiler-raw--returns-proven-p (frame constants arity)
  "Return non-nil when every FRAME return carries a proven raw integer."
  (let ((blocks (plist-get frame :blocks)) (returns 0) (valid t))
    (dolist (block (append blocks nil))
      (dolist (instruction (append (plist-get block :instructions) nil))
        (when (eq (plist-get instruction :kind) 'return)
          (setq returns (1+ returns))
          (let ((proof
                 (and (= (length (plist-get instruction :inputs)) 1)
                      (nelisp-bytecode-native-compiler-raw--integer-value-p
                       (car (plist-get instruction :inputs)) block blocks
                       constants arity nil))))
            (unless (and (consp proof) (car proof) (cdr proof))
              (setq valid nil))))))
    (and (> returns 0) valid)))

(defun nelisp-bytecode-native-compiler-raw-build
    (function artifact-path export-name argument-contract)
  "Compile materialized FUNCTION into raw-v1 ARTIFACT-PATH as EXPORT-NAME.

ARGUMENT-CONTRACT must explicitly contain one `raw-i64' entry per required
positional argument.  The return representation is inferred and accepted only
when every return is proven to carry an integer.  Returns a result plist; all
unsupported or malformed inputs are rejected before ARTIFACT-PATH is written."
  (let* ((input (nelisp-bytecode-compiler-input-build function))
         (descriptor (plist-get input :argument-descriptor))
         (arity (plist-get input :argument-count))
         (lowered nil))
    (cond
     ((eq (plist-get input :status) 'malformed)
      (list :status 'malformed :reason (plist-get input :reason) :input input))
     ((not (eq (plist-get input :status) 'complete))
      (list :status 'unsupported :reason (plist-get input :reason) :input input))
     ((not (and (integerp arity) (= arity (plist-get input :argument-min))
                (= arity (plist-get input :argument-max))
                (proper-list-p argument-contract)
                (= (length argument-contract) arity)
                (cl-every (lambda (repr) (eq repr 'raw-i64)) argument-contract)))
      (list :status 'unsupported
            :reason "required fixed arity and explicit raw-i64 argument contract must match"
            :input input))
     (t
      (setq lowered
            (nelisp-bytecode-native-cfg-lower
             (plist-get input :code) (plist-get input :constants)
             arity argument-contract))
      (cond
       ((not (eq (plist-get lowered :status) 'complete))
        (list :status 'unsupported :reason (plist-get lowered :reason)
              :input input :lowered lowered))
       ((not (nelisp-bytecode-native-compiler-raw--returns-proven-p
              (plist-get lowered :frame-ir) (plist-get input :constants) arity))
        (list :status 'unsupported
              :reason "raw-i64 return value is not proven on every exit"
              :input input :lowered lowered))
       ((not (and (stringp artifact-path) (stringp export-name)
                  (> (length export-name) 0)))
        (list :status 'malformed :reason "artifact path and export name are required"
              :input input :lowered lowered))
       (t
        (let ((manifest
               (nelisp-bytecode-native-cfg-write-raw-v1
                lowered artifact-path nelisp-bytecode-native-compiler-raw--source
                export-name)))
          (list :status 'complete :input input :lowered lowered
                :manifest manifest :user-arity arity :return-repr 'raw-i64))))))))

(provide 'nelisp-bytecode-native-compiler-raw)
;;; nelisp-bytecode-native-compiler-raw.el ends here
