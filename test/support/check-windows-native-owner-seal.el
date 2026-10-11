;;; Windows PE proof dependency mutation controls -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Disposable host process: exercise the actual admission definitions with inert
;; evidence/OS verification. Baseline reaches a counted mapping sentinel; every
;; changed decoder, binary identity, target or mapping owner must refuse first.
(setq native-comp-enable-subr-trampolines nil)
(require 'cl-extra)
(require 'cl-seq)
(require 'nelisp-native-load)
(require 'nelisp-native-windows)
(require 'nelisp-native-pe-symbols)
(require 'nelisp-native-compiler-startup-evidence)
(cl-every #'identity nil)
(defvar windows-owner-map-count 0)
(defvar nelisp-native-rooted-abi-proof--expected nil)
(let* ((system-type 'windows-nt)
       (evidence '(:layout (:size 8)))
       (nelisp-native-rooted-abi-proof--expected evidence)
       (forms (nelisp-native-compiler-startup-evidence--forms
               "templates/nelisp-native-rooted-abi-proof.el.in"))
       eligible create owner-form)
  (cl-labels ((walk (node)
                (when (consp node)
                  (when (eq (car node) 'defun)
                    (cond ((eq (cadr node) 'nelisp-native-rooted-abi-proof--eligible-p) (setq eligible node))
                          ((eq (cadr node) 'nelisp-native-rooted-abi-proof-create) (setq create node))))
                  (when (and (eq (car node) 'setq) (eq (cadr node) 'captured-module-owners))
                    (setq owner-form node))
                  (walk (car node)) (walk (cdr node))))) (walk forms))
  (fset 'nelisp--target-os-code (lambda () 2))
  (fset 'nelisp--target-arch-code (lambda () 0))
  (fset 'nelisp-native-rooted-abi-evidence (lambda () evidence))
  (fset 'nelisp-native-rooted-abi-proof--data-hash (lambda (_) "NELISP_BUILD_EVIDENCE_PIN"))
  (fset 'nelisp-bytecode-compiler-input-dialect (lambda () '(:runtime-evidence standalone-build-verified)))
  (fset 'nelisp-native-rooted-abi-proof-dependency-context (lambda () nil))
  (fset 'nelisp-native-windows-map (lambda (_) (setq windows-owner-map-count (1+ windows-owner-map-count)) 65536))
  (fset 'nelisp-native-rooted-abi-proof--verify
        (lambda (&optional _) (nelisp-native-windows-map 4096) (error "MAP_REACHED")))
  (cl-labels
      ((install (owners)
         (eval
          `(let ((captured-all-owners nil) (captured-module-owners nil)
                 (captured-primitive-owners nil) (captured-eligibility nil)
                 (captured-evidence ',evidence) (captured-evidence-hash "NELISP_BUILD_EVIDENCE_PIN")
                 (captured-evidence-owner (symbol-function 'nelisp-native-rooted-abi-evidence))
                 (captured-eq (symbol-function 'eq)) (captured-car (symbol-function 'car))
                 (captured-cdr (symbol-function 'cdr)) (captured-symbol-function (symbol-function 'symbol-function))
                 (issued-records (make-hash-table :test #'eq)))
             ,eligible ,create ,owners
             (setq captured-all-owners captured-module-owners
                   captured-eligibility (symbol-function 'nelisp-native-rooted-abi-proof--eligible-p))) t))
       (check (count message)
         (setq windows-owner-map-count 0)
         (let ((result (condition-case err (nelisp-native-rooted-abi-proof-create)
                         (error (error-message-string err)))))
           (unless (and (= count windows-owner-map-count) (equal result message))
             (error "Owner seal control failed: maps=%S result=%S" windows-owner-map-count result)))))
    (install owner-form)
    (check 1 "MAP_REACHED")
    (dolist (name '(ash downcase cl-find-if ptr-read-u32 mapc string string-match-p
                   nelisp-native-windows-map nelisp-native-windows-unmap
                   nelisp-native-load--running-binary-sha256
                   nelisp-native-load--runtime-abi-v2 nelisp-native-load--target-v2
                   nelisp--target-os-code nl-ffi-call max require fboundp copy-tree plist-put
                   nelisp-native-load--sha256 nelisp-native-load--digest
                   nelisp-native-load--mmap nelisp-native-load--unmap nelisp-native-load--protect
                   ptr-write-bytes string-byte nelisp--sha256 nelisp--build-digest))
      (let ((old (symbol-function name)))
        (unwind-protect
            (progn (fset name (lambda (&rest args) (apply old args)))
                   (check 0 "Root proof native/evidence ownership rejected"))
          (if old (fset name old) (fmakunbound name)))
        (check 1 "MAP_REACHED")))
    ;; Calibrate the test itself: deliberately remove ash only from the copied
    ;; source-owned list. The same mutation must now reach the mapping sentinel.
    (let ((broken (copy-tree owner-form)))
      (cl-labels ((remove-owner (node)
                    (when (consp node)
                      (when (and (eq (car node) 'quote) (listp (cadr node))
                                 (memq 'ash (cadr node)))
                        (setcar (cdr node) (delq 'ash (cadr node))))
                      (remove-owner (car node)) (remove-owner (cdr node))))) (remove-owner broken))
      (install broken)
      (let ((old (symbol-function 'ash)))
        (unwind-protect (progn (fset 'ash (lambda (&rest args) (apply old args))) (check 1 "MAP_REACHED"))
          (fset 'ash old))))
    (install owner-form) (check 1 "MAP_REACHED")
    (princ "WINDOWS-OWNER-SEAL-PASS mutations=28 maps=0 calibration=1\n")))
