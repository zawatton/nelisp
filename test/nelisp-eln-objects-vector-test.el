;;; nelisp-eln-objects-vector-test.el --- opaque vector views -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Host tests for the opaque vector kind of `nelisp-eln-objects' (S6.9: the
;; `(byte-code "\207" [] 2)' result of `byte-compile-top-level' flows through
;; the native body and back).  View memory is faked with a word table so the
;; codec's identity, poison, lease and decode logic runs on any Emacs.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-eln-objects)
(require 'nelisp-eln-native-subr)

(defvar nelisp-eln-vec-test--memory nil "Fake memory: word table.")
(defvar nelisp-eln-vec-test--live nil "Live fake allocations.")
(defvar nelisp-eln-vec-test--next #x100000)

(defmacro nelisp-eln-vec-test--with-memory (&rest body)
  "Run BODY with fake view memory and a fresh shared registry."
  (declare (indent 0))
  `(let ((nelisp-eln-vec-test--memory (make-hash-table :test 'eql))
         (nelisp-eln-vec-test--live nil)
         (nelisp-eln-objects--live-units nil)
         (nelisp-eln-objects--identity-records nil)
         (nelisp-eln-objects--arenas nil)
         (nelisp-eln-objects--activations nil)
         (nelisp-eln-objects--registry-state 'open))
     (cl-letf (((symbol-function 'nelisp-eln-objects--allocate-view-memory)
                (lambda (bytes)
                  (let ((owner (vector 'fake-memory nelisp-eln-vec-test--next
                                       bytes)))
                    (setq nelisp-eln-vec-test--next
                          (+ nelisp-eln-vec-test--next (* 16 (1+ (/ bytes 16)))))
                    (push owner nelisp-eln-vec-test--live)
                    owner)))
               ((symbol-function 'nl-ffi-memory-address)
                (lambda (owner) (aref owner 1)))
               ((symbol-function 'nl-ffi-memory-release)
                (lambda (owner)
                  (unless (memq owner nelisp-eln-vec-test--live)
                    (error "double release"))
                  (setq nelisp-eln-vec-test--live
                        (delq owner nelisp-eln-vec-test--live))))
               ((symbol-function 'nelisp-eln-objects--write-word)
                (lambda (address offset word)
                  (puthash (+ address offset) word
                           nelisp-eln-vec-test--memory)))
               ((symbol-function 'nelisp-eln-objects--read-word)
                (lambda (address offset)
                  (gethash (+ address offset) nelisp-eln-vec-test--memory))))
       ,@body)))

(defun nelisp-eln-vec-test--encode (unit value)
  "Encode VALUE in UNIT with opaque vectors admitted."
  (nelisp-eln-objects-call-with-artifact-symbols
   nil (lambda () (nelisp-eln-objects-encode unit value)) nil t))

(ert-deftest nelisp-eln-vec-fails-closed-without-admission ()
  (nelisp-eln-vec-test--with-memory
    (let ((unit (nelisp-eln-objects-create)))
      (should-error (nelisp-eln-objects-encode unit (vector 1 2))
                    :type 'nelisp-eln-objects-unsupported)
      (should-error (nelisp-eln-objects-encode unit (list 'a nil (vector)))
                    :type 'nelisp-eln-objects-unsupported)
      (should-not nelisp-eln-objects--identity-records)
      (should-not nelisp-eln-vec-test--live)
      (nelisp-eln-objects-release unit))))

(ert-deftest nelisp-eln-vec-identity-round-trip ()
  (nelisp-eln-vec-test--with-memory
    (let* ((unit (nelisp-eln-objects-create))
           (v (vector 1 2 3))
           (empty (vector))
           (form (list 7 v 2 empty v))
           (word (nelisp-eln-vec-test--encode unit form))
           (cell (- word 3))
           (rest (- (nelisp-eln-objects--read-word cell 8) 3))
           (v-word (nelisp-eln-objects--read-word rest 0)))
      (should (= 5 (logand v-word 7)))
      (should (eq form (nelisp-eln-objects-decode unit word)))
      (should (eq v (nelisp-eln-objects-decode unit v-word)))
      ;; The same vector object is the same word everywhere it occurs.
      (let* ((c3 (- (nelisp-eln-objects--read-word
                     (- (nelisp-eln-objects--read-word rest 8) 3) 8)
                    3))
             (c4 (- (nelisp-eln-objects--read-word c3 8) 3)))
        (should (= v-word (nelisp-eln-objects--read-word c4 0))))
      ;; A distinct (even `equal') vector is a distinct identity.
      (let ((other (vector 1 2 3)))
        (should-not (= v-word (nelisp-eln-vec-test--encode unit other)))
        (should (eq other (nelisp-eln-objects-decode
                           unit (nelisp-eln-vec-test--encode unit other)))))
      (nelisp-eln-objects-release unit)
      (should-not nelisp-eln-objects--identity-records)
      (should-not nelisp-eln-vec-test--live))))

(ert-deftest nelisp-eln-vec-view-is-poisoned ()
  (nelisp-eln-vec-test--with-memory
    (let* ((unit (nelisp-eln-objects-create))
           (v (vector 'secret 42))
           (word (nelisp-eln-vec-test--encode unit v))
           (address (- word 5))
           (poison nelisp-eln-objects--opaque-symbol-cell-word))
      ;; Native code dereferencing the view sees only poison, never contents.
      (should (= poison (nelisp-eln-objects--read-word address 0)))
      (should (= poison (nelisp-eln-objects--read-word address 8)))
      ;; A poison word read from it is rejected by every decoder.
      (should-error (nelisp-eln-objects-decode unit poison)
                    :type 'nelisp-eln-objects-unsupported)
      (let ((token (nelisp-eln-objects-activation-acquire unit)))
        (should-error (nelisp-eln-objects-activation-decode token poison)
                      :type 'nelisp-eln-objects-unsupported)
        (nelisp-eln-objects-activation-release token))
      ;; Native writes into the view are rejected at the boundary.
      (nelisp-eln-objects--write-word address 8 (nelisp-eln-abi-encode-fixnum 5))
      (should-error (nelisp-eln-objects-sync-from-native unit)
                    :type 'nelisp-eln-objects-unsupported)
      (should-error (nelisp-eln-objects-sync-to-native unit)
                    :type 'nelisp-eln-objects-unsupported)
      (nelisp-eln-objects--write-word address 8 poison)
      (should (nelisp-eln-objects-sync-from-native unit))
      (nelisp-eln-objects--write-word address 0 poison)
      (nelisp-eln-objects-release unit)
      (should-not nelisp-eln-vec-test--live))))

(ert-deftest nelisp-eln-vec-lease-release ()
  (nelisp-eln-vec-test--with-memory
    (let* ((unit (nelisp-eln-objects-create))
           (v (vector 1))
           (word (nelisp-eln-vec-test--encode unit v))
           (token (nelisp-eln-objects-activation-acquire unit)))
      (should (eq v (nelisp-eln-objects-activation-decode token word)))
      (should (nelisp-eln-objects-activation-leases-word-p token word))
      ;; Releasing the unit keeps the view alive for the open activation.
      (nelisp-eln-objects-release unit)
      (should (= 1 (length nelisp-eln-vec-test--live)))
      (should (eq v (nelisp-eln-objects-activation-decode token word)))
      (nelisp-eln-objects-activation-release token)
      (should-not nelisp-eln-vec-test--live)
      (should-not nelisp-eln-objects--identity-records)
      ;; A word no activation leases is refused, not dereferenced.
      (let* ((unit2 (nelisp-eln-objects-create))
             (token2 (nelisp-eln-objects-activation-acquire unit2)))
        (should-not (nelisp-eln-objects-activation-leases-word-p token2 word))
        (should-error (nelisp-eln-objects-activation-decode token2 word)
                      :type 'nelisp-eln-objects-error)
        (nelisp-eln-objects-activation-release token2)
        (nelisp-eln-objects-release unit2)))))

(ert-deftest nelisp-eln-vec-shared-view-is-refcounted ()
  (nelisp-eln-vec-test--with-memory
    (let* ((unit1 (nelisp-eln-objects-create))
           (unit2 (nelisp-eln-objects-create))
           (v (vector 1))
           (w1 (nelisp-eln-vec-test--encode unit1 v))
           (w2 (nelisp-eln-vec-test--encode unit2 v)))
      (should (= w1 w2))
      (should (= 1 (length nelisp-eln-vec-test--live)))
      (nelisp-eln-objects-release unit1)
      (should (= 1 (length nelisp-eln-vec-test--live)))
      (should (eq v (nelisp-eln-objects-decode unit2 w2)))
      (nelisp-eln-objects-release unit2)
      (should-not nelisp-eln-vec-test--live))))

(ert-deftest nelisp-eln-vec-decode-vector-returned-by-native ()
  (nelisp-eln-vec-test--with-memory
    (let* ((unit (nelisp-eln-objects-create))
           (v (vector 'from-port))
           (slot (list nil 9))
           (roots (list slot v))
           (word (nelisp-eln-vec-test--encode unit roots))
           (slot-address (- (nelisp-eln-objects--read-word (- word 3) 0) 3))
           (v-word (nelisp-eln-vec-test--encode unit v)))
      ;; Native code stores the vector word it was handed into a cons.
      (nelisp-eln-objects--write-word slot-address 0 v-word)
      (should (nelisp-eln-objects-sync-from-native unit))
      (should (eq v (car slot)))
      (should (eq (cadr roots) (car slot)))
      (nelisp-eln-objects-release unit)
      (should-not nelisp-eln-vec-test--live))))

(ert-deftest nelisp-eln-vec-admission-is-scoped-to-declared-shapes ()
  (let ((declared nil))
    (dolist (spec nelisp-eln-native-subr--multi-import-specs)
      (when (plist-get (cdr spec) :opaque-vectors)
        (push (car spec) declared)))
    ;; `lambda-form' (S6.9) and, for forms that may carry vector literals
    ;; and `Fmapcar' results, `make-closure-form' (S6.11), whose body reads
    ;; only FORM's conses and the `Fmapcar' list inline.
    ;; S10 `compile-form-form' (byte-compile-form) may be handed a vector
    ;; literal as its FORM, which its body only passes to authenticated ports.
    (should (equal declared '(compile-form-form make-closure-form lambda-form))))
  ;; Nothing leaks out of a scoped call.
  (should-not nelisp-eln-objects--admit-opaque-vectors)
  (nelisp-eln-objects-call-with-artifact-symbols
   nil (lambda () (should nelisp-eln-objects--admit-opaque-vectors)) nil t)
  (should-not nelisp-eln-objects--admit-opaque-vectors)
  (should-error
   (nelisp-eln-objects-call-with-artifact-symbols
    nil (lambda () (error "boom")) nil t))
  (should-not nelisp-eln-objects--admit-opaque-vectors))

(provide 'nelisp-eln-objects-vector-test)
;;; nelisp-eln-objects-vector-test.el ends here
