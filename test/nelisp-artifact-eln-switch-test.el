;;; nelisp-artifact-eln-switch-test.el --- S7.7 .neln migration switch  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Host ERT for the S7.7 migration switch in `nelisp-artifact.el':
;; `nelisp-artifact-neln-legacy-native', `nelisp-artifact-neln-eln-table',
;; `nelisp-artifact--neln-proven-native-shapes', and the single dispatch
;; point `nelisp-artifact--maybe-install-native' all three `.neln' load
;; paths now share.
;;
;; These tests operate at the unit level, below any real `.neln'/`.eln'
;; artifact file: `nelisp-artifact--install-native-functions' and
;; `nelisp-eln-registration-load' are replaced with `cl-letf' fakes that
;; record their calls, so no native compiler (`cc'/`objcopy') or genuine
;; GNU `.eln' artifact is needed to exercise the routing decision itself.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'nelisp-artifact)

;; `nelisp-eln-registration.el' (and its own, separately owned, dependency
;; chain: nl-ffi, nelisp-native-load, ...) is intentionally never
;; `require'd here.  Every test below replaces
;; `nelisp-eln-registration-load' with a `cl-letf' fake; this placeholder
;; only guarantees the symbol already has a function cell for `cl-letf' to
;; save and restore when the real definition has not been loaded.
(declare-function nelisp-eln-registration-load "nelisp-eln-registration" (path))
(unless (fboundp 'nelisp-eln-registration-load)
  (defalias 'nelisp-eln-registration-load
    (lambda (path)
      (error "nelisp-eln-registration-load stub called directly with %s (a test should have cl-letf'd this)"
             path))))

;; --- Synthetic `.neln' per-defun metadata, one per S7.7 seed shape -------

(defun nelisp-artifact-eln-switch-test--meta-const (name)
  "Metadata for the `identity-constant-leaf' seed shape."
  (list :name name :size 8 :arity 0 :param-class 'gp :param-repr nil
        :rt-slot-count 0 :return-repr nil :body-offset 0))

(defun nelisp-artifact-eln-switch-test--meta-unary (name)
  "Metadata for the `bounded-unary-identity-leaf' seed shape."
  (list :name name :size 6 :arity 1 :param-class 'gp :param-repr nil
        :rt-slot-count 0 :return-repr nil :body-offset 0))

(defun nelisp-artifact-eln-switch-test--meta-tail (name)
  "Metadata for the `unary-tail-import-fast-path' seed shape (1+/1- style)."
  (list :name name :size 64 :arity 1 :param-class 'gp :param-repr nil
        :rt-slot-count 2 :return-repr nil :body-offset 8))

(defun nelisp-artifact-eln-switch-test--meta-unadmitted (name)
  "Metadata for a shape none of the seed predicates admit."
  (list :name name :size 500 :arity 3 :param-class 'gp :param-repr nil
        :rt-slot-count 12 :return-repr 'sexp-ptr :body-offset 40))

;; --- (bonus) the data table itself ----------------------------------------

(ert-deftest nelisp-artifact-eln-switch/proven-shape-table-matches-seed-shapes ()
  "Each S7.7 seed shape is admitted; an unrelated shape and nil are not."
  (should (nelisp-artifact--neln-shape-proven-p
           (nelisp-artifact-eln-switch-test--meta-const "c")))
  (should (nelisp-artifact--neln-shape-proven-p
           (nelisp-artifact-eln-switch-test--meta-unary "u")))
  (should (nelisp-artifact--neln-shape-proven-p
           (nelisp-artifact-eln-switch-test--meta-tail "t")))
  (should-not (nelisp-artifact--neln-shape-proven-p
               (nelisp-artifact-eln-switch-test--meta-unadmitted "b")))
  (should-not (nelisp-artifact--neln-shape-proven-p nil)))

;; --- (a) legacy t: byte-identical to before -------------------------------

(ert-deftest nelisp-artifact-eln-switch/legacy-t-byte-identical ()
  "With the switch at its default (`nelisp-artifact-neln-legacy-native' t),
`nelisp-artifact--maybe-install-native' makes the exact same single call to
`nelisp-artifact--install-native-functions' the three pre-S7.7 call sites
made directly, with the same two arguments, and never calls
`nelisp-eln-registration-load' at all."
  (let* ((nelisp-artifact-neln-legacy-native t)
         (nelisp-artifact-neln-eln-table
          (list (cons 'nats-unary "/fake/should-be-ignored.eln")))
         (install-calls nil)
         (registration-calls nil)
         (native (list :symbols '("nats-const" "nats-unary" "nats-tail" "nats-big")
                       :defuns (list (nelisp-artifact-eln-switch-test--meta-const "nats-const")
                                     (nelisp-artifact-eln-switch-test--meta-unary "nats-unary")
                                     (nelisp-artifact-eln-switch-test--meta-tail "nats-tail")
                                     (nelisp-artifact-eln-switch-test--meta-unadmitted "nats-big")))))
    (cl-letf (((symbol-function 'nelisp-artifact--install-native-functions)
               (lambda (path nat) (push (list path nat) install-calls) 4))
              ((symbol-function 'nelisp-eln-registration-load)
               (lambda (path) (push path registration-calls) (error "must not be called: %s" path))))
      (nelisp-artifact--maybe-install-native "ARTIFACT" native)
      (should (equal install-calls (list (list "ARTIFACT" native))))
      (should (null registration-calls)))))

;; --- (b) legacy nil, unadmitted function: installer not called -----------

(ert-deftest nelisp-artifact-eln-switch/legacy-nil-unadmitted-vm-only ()
  "With legacy nil, a symbol whose metadata matches no proven shape and has
no `.eln' table entry gets neither the private installer nor eln
registration.  Plain VM byte-code replay -- simulated here exactly as
`nelisp-artifact--replay-module-item' installs a `:fn' item, since that
replay step always runs before any native-install decision -- still
produces the correct call result once the (undecorated) function is
installed."
  (let* ((sym 'nelisp-artifact-eln-switch-test--big)
         (nelisp-artifact-neln-legacy-native nil)
         (nelisp-artifact-neln-eln-table nil)
         (install-calls nil)
         (registration-calls nil)
         (native (list :symbols (list sym)
                       :defuns (list (nelisp-artifact-eln-switch-test--meta-unadmitted
                                      (symbol-name sym))))))
    (unwind-protect
        (progn
          (nelisp-artifact--install-function sym (lambda (x) (* x 3)))
          (cl-letf (((symbol-function 'nelisp-artifact--install-native-functions)
                     (lambda (path nat) (push (list path nat) install-calls) 0))
                    ((symbol-function 'nelisp-eln-registration-load)
                     (lambda (path) (push path registration-calls))))
            (nelisp-artifact--maybe-install-native "ARTIFACT" native)
            (should (null install-calls))
            (should (null registration-calls))
            (should (= (nelisp-eval (list sym 7)) 21))))
      (remhash sym nelisp--functions)
      (fmakunbound sym))))

;; --- (c) legacy nil, admitted function with a matching .eln --------------

(ert-deftest nelisp-artifact-eln-switch/legacy-nil-admitted-with-eln-routes-registration ()
  "With legacy nil, a proven-shape symbol that also has a
`nelisp-artifact-neln-eln-table' hit is routed through
`nelisp-eln-registration-load' exactly once (with that symbol's table
path), and the private installer is not called at all for this load."
  (let* ((sym 'nelisp-artifact-eln-switch-test--unary)
         (eln-path "/fake/unary.eln")
         (nelisp-artifact-neln-legacy-native nil)
         (nelisp-artifact-neln-eln-table (list (cons sym eln-path)))
         (install-calls nil)
         (registration-calls nil)
         (native (list :symbols (list sym)
                       :defuns (list (nelisp-artifact-eln-switch-test--meta-unary
                                      (symbol-name sym))))))
    (cl-letf (((symbol-function 'nelisp-artifact--install-native-functions)
               (lambda (path nat) (push (list path nat) install-calls) 0))
              ((symbol-function 'nelisp-eln-registration-load)
               (lambda (path) (push path registration-calls) nil)))
      (nelisp-artifact--maybe-install-native "ARTIFACT" native)
      (should (equal registration-calls (list eln-path)))
      (should (null install-calls)))))

;; --- (d) never both, for a symbol admitted by both routes -----------------

(ert-deftest nelisp-artifact-eln-switch/never-both-for-same-symbol ()
  "A symbol that is simultaneously proven-shape (would qualify for the
private installer) AND has an `.eln' table hit (would qualify for
registration) is routed through exactly one of the two mechanisms -- the
genuine `.eln' -- never both."
  (let* ((sym 'nelisp-artifact-eln-switch-test--overlap)
         (eln-path "/fake/overlap.eln")
         (nelisp-artifact-neln-legacy-native nil)
         (nelisp-artifact-neln-eln-table (list (cons sym eln-path)))
         (install-calls nil)
         (registration-calls nil)
         ;; `identity-constant-leaf' shape: on its own this would be
         ;; admitted for the private installer.
         (native (list :symbols (list sym)
                       :defuns (list (nelisp-artifact-eln-switch-test--meta-const
                                      (symbol-name sym))))))
    (cl-letf (((symbol-function 'nelisp-artifact--install-native-functions)
               (lambda (path nat) (push (list path nat) install-calls) 0))
              ((symbol-function 'nelisp-eln-registration-load)
               (lambda (path) (push path registration-calls) nil)))
      (nelisp-artifact--maybe-install-native "ARTIFACT" native)
      (should (equal registration-calls (list eln-path)))
      (should (null install-calls)))))

;; --- (supplementary) admitted, no .eln hit: still installs ---------------

(ert-deftest nelisp-artifact-eln-switch/legacy-nil-admitted-without-eln-keeps-install ()
  "With legacy nil and no `.eln' table hit, a proven-shape symbol still gets
the private wrapper: the switch narrows native install to the admitted
subset, it does not additionally remove installation from that subset
\(ledger S7.7: \"replace... within proven coverage\", not delete\)."
  (let* ((sym 'nelisp-artifact-eln-switch-test--unary2)
         (nelisp-artifact-neln-legacy-native nil)
         (nelisp-artifact-neln-eln-table nil)
         (install-calls nil)
         (registration-calls nil)
         (native (list :symbols (list sym)
                       :defuns (list (nelisp-artifact-eln-switch-test--meta-unary
                                      (symbol-name sym))))))
    (cl-letf (((symbol-function 'nelisp-artifact--install-native-functions)
               (lambda (path nat) (push (list path nat) install-calls) 0))
              ((symbol-function 'nelisp-eln-registration-load)
               (lambda (path) (push path registration-calls))))
      (nelisp-artifact--maybe-install-native "ARTIFACT" native)
      (should (null registration-calls))
      (should (= (length install-calls) 1))
      (should (equal (plist-get (nth 1 (car install-calls)) :symbols)
                     (list sym))))))

(provide 'nelisp-artifact-eln-switch-test)

;;; nelisp-artifact-eln-switch-test.el ends here
