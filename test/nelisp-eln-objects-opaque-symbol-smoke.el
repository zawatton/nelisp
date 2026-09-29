;;; nelisp-eln-objects-opaque-symbol-smoke.el --- opaque interned symbol views -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Standalone smoke for `nelisp-eln-objects--admit-opaque-interned-symbols'
;; (S6.13 `byte-compile-setq', whose corpus form `(setq foo 1)' carries the
;; special-form symbol `setq').  Run it on a standalone binary:
;;
;;   NELISP_ROOT=$PWD target/nelisp --load test/nelisp-eln-objects-opaque-symbol-smoke.el
;;
;; It proves, on real view memory:
;;   - without the opaque admission an interned symbol with non-empty state
;;     still fails closed (`unsupported-symbol-state');
;;   - with it, the symbol's word decodes back to the identical symbol, but
;;     the view GNU code would dereference (symbol base + word) exposes no
;;     value, function or plist: those cells hold the poison word, and the
;;     header carries `SYMBOL_NOWRITE';
;;   - native code that reads a cell and hands the word back (as a result,
;;     or stored into a shared cons) is rejected by every decoder;
;;   - native code that writes a cell is rejected at the boundary.
;; Prints NELISP_ELN_OPAQUE_SYMBOL_SMOKE=PASS or signals.

;;; Code:

(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (root (or (getenv "NELISP_ELN_MODULE_ROOT")
                 (getenv "NELISP_ROOT")
                 (file-name-directory (directory-file-name test-dir)))))
  (add-to-list 'load-path (expand-file-name "lisp" root))
  (add-to-list 'load-path (expand-file-name "packages/nl-ffi/src" root))
  (require 'nelisp-eln-objects))

(defvar nelisp-eln-opaque-smoke--bound 42
  "A symbol with a value, a function and a plist entry.")
(fset 'nelisp-eln-opaque-smoke--bound (lambda () 'secret-function))
(put 'nelisp-eln-opaque-smoke--bound 'secret-property 7)
;; Loading bytecomp.el gives `setq' a plist exactly like this, which is
;; what makes the S6.13 corpus symbol non-empty on this runtime (a special
;; form has no function cell here).
(put 'setq 'byte-compile 'byte-compile-setq)

(defun nelisp-eln-opaque-smoke--expect (condition reason thunk)
  "Require THUNK to signal CONDITION whose data starts with REASON."
  (let ((caught (condition-case err
                    (progn (funcall thunk) nil)
                  (error err))))
    (unless (and caught (memq condition (get (car caught) 'error-conditions))
                 (or (null reason) (equal (cadr caught) reason)))
      (error "Expected %S %S, got %S" condition reason caught))))

(defun nelisp-eln-opaque-smoke--view-address (word)
  "Return the address GNU's XSYMBOL computes for symbol WORD.
WORD is a signed displacement from the symbol base."
  (+ (aref nelisp-eln-objects--symbol-base 1)
     (if (> word nelisp-eln-abi-signed-word-max)
         (- word (1+ nelisp-eln-abi-word-mask))
       word)))

(let ((records (length nelisp-eln-objects--identity-records))
      (poison nelisp-eln-objects--opaque-symbol-cell-word))
  ;; 1. No opaque admission: fail closed, as before.
  (let ((unit (nelisp-eln-objects-create)))
    (unwind-protect
        (progn
          (nelisp-eln-opaque-smoke--expect
           'nelisp-eln-objects-unsupported 'unsupported-symbol-state
           (lambda () (nelisp-eln-objects-encode unit (list 'setq 'foo 1))))
          ;; Empty-interned admission alone still refuses non-empty state.
          (nelisp-eln-opaque-smoke--expect
           'nelisp-eln-objects-unsupported 'unsupported-symbol-state
           (lambda ()
             (nelisp-eln-objects-call-with-artifact-symbols
              nil (lambda ()
                    (nelisp-eln-objects-encode
                     unit 'nelisp-eln-opaque-smoke--bound))))))
      (nelisp-eln-objects-release unit)))
  ;; 2. Opaque admission: identity-only views.
  (let* ((unit (nelisp-eln-objects-create))
         (form (list 'setq 'nelisp-eln-opaque-smoke--bound 1))
         (word (nelisp-eln-objects-call-with-artifact-symbols
                nil (lambda () (nelisp-eln-objects-encode unit form))
                t))
         (cons-address (- word 3))
         (setq-word (nelisp-eln-objects--read-word cons-address 0))
         (bound-word (nelisp-eln-objects--read-word
                      (- (nelisp-eln-objects--read-word cons-address 8) 3) 0))
         (token nil))
    (unwind-protect
        (progn
          (unless (eq (nelisp-eln-objects-decode unit word) form)
            (error "opaque form did not decode to the identical cons"))
          (unless (and (eq (nelisp-eln-objects-decode unit setq-word) 'setq)
                       (eq (nelisp-eln-objects-decode unit bound-word)
                           'nelisp-eln-opaque-smoke--bound))
            (error "opaque symbol words did not decode to the same symbols"))
          ;; What native code would see through XSYMBOL: poison cells only.
          (dolist (symbol-word (list setq-word bound-word))
            (let ((address (nelisp-eln-opaque-smoke--view-address symbol-word)))
              (unless (= (ptr-read-u8 address 0)
                         (nelisp-eln-objects--opaque-symbol-header-byte))
                (error "opaque view header is %S" (ptr-read-u8 address 0)))
              (dolist (offset '(16 24 32))
                (unless (= (nelisp-eln-objects--read-word address offset)
                           poison)
                  (error "opaque view cell %d leaks %S" offset
                         (nelisp-eln-objects--read-word address offset))))))
          (when (= (nelisp-eln-objects--read-word
                    (nelisp-eln-opaque-smoke--view-address bound-word) 16)
                   (nelisp-eln-abi-encode-fixnum 42))
            (error "opaque view exposes the symbol value"))
          ;; 3. Native reads through the view are rejected by every decoder.
          (let ((cell (nelisp-eln-objects--read-word
                       (nelisp-eln-opaque-smoke--view-address bound-word) 24)))
            (nelisp-eln-opaque-smoke--expect
             'nelisp-eln-objects-unsupported 'unused
             (lambda () (nelisp-eln-objects-decode unit cell)))
            (setq token (nelisp-eln-objects-activation-acquire unit))
            (nelisp-eln-opaque-smoke--expect
             'nelisp-eln-objects-unsupported 'unused
             (lambda () (nelisp-eln-objects-activation-decode token cell)))
            ;; ... and so is a read word stored into a shared cons.
            (nelisp-eln-objects--write-word cons-address 0 cell)
            (nelisp-eln-opaque-smoke--expect
             'nelisp-eln-objects-unsupported 'unused
             (lambda () (nelisp-eln-objects-sync-from-native unit)))
            (nelisp-eln-objects--write-word cons-address 0 setq-word))
          ;; 4. Native writes to a poison cell are rejected at the boundary.
          (let ((address (nelisp-eln-opaque-smoke--view-address setq-word)))
            (nelisp-eln-objects--write-word
             address 16 (nelisp-eln-abi-encode-fixnum 7))
            (nelisp-eln-opaque-smoke--expect
             'nelisp-eln-objects-unsupported 'native-symbol-cell-mutation
             (lambda () (nelisp-eln-objects-sync-from-native unit)))
            (nelisp-eln-objects--write-word address 16 poison))
          ;; Restored views synchronize; state was never touched.
          (nelisp-eln-objects-sync-from-native unit)
          (unless (and (eq (car form) 'setq)
                       (= nelisp-eln-opaque-smoke--bound 42)
                       (eq (funcall (symbol-function
                                     'nelisp-eln-opaque-smoke--bound))
                           'secret-function)
                       (= (get 'nelisp-eln-opaque-smoke--bound
                               'secret-property)
                          7)
                       (eq (get 'setq 'byte-compile) 'byte-compile-setq))
            (error "opaque view changed canonical state")))
      (when token (nelisp-eln-objects-activation-release token))
      (nelisp-eln-objects-release unit)))
  (unless (= records (length nelisp-eln-objects--identity-records))
    (error "opaque views leaked identity records"))
  (princ "NELISP_ELN_OPAQUE_SYMBOL_SMOKE=PASS\n"))

;;; nelisp-eln-objects-opaque-symbol-smoke.el ends here
