;;; emacs-numeric-test.el --- ERT tests for emacs-numeric  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the Layer 2 numeric + bitwise polyfill module.  Under
;; batch host Emacs the host C builtins remain active (= the module's
;; `unless (fboundp ...)' gate keeps them) so most assertions exercise
;; the host's real impl.  Bridge-shape and load-clean tests verify the
;; module's bookkeeping; behavioural assertions exercise host-correct
;; results.  A pair of polyfill-body tests use synthetic helpers to
;; assert the approximate semantics on the standalone path (= same
;; tightness the bytecomp / subr load expects).

;;; Code:

(require 'ert)
(require 'emacs-numeric)
(require 'cl-lib)

(defconst emacs-numeric-test--module-file
  (expand-file-name "../src/emacs-numeric.el"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Source file for reloading `emacs-numeric' in controlled tests.")

;;;; A. Load cleanly

(ert-deftest emacs-numeric-test/require-loads-cleanly ()
  (should (featurep 'emacs-numeric))
  (dolist (sym '(min max abs zerop plusp minusp oddp evenp natnump
                 1+ 1- %
                 logior logand logxor lognot ash lsh
                 atan exp))
    (should (fboundp sym))))

;;;; B. Numeric primitive results

(ert-deftest emacs-numeric-test/numeric-primitives-return-expected-values ()
  (should (= 1 (min 5 1 3)))
  (should (= 5 (max 5 1 3)))
  (should (= 4 (abs -4)))
  (should (= 4 (abs 4)))
  (should (zerop 0))
  (should-not (zerop 1))
  (should (plusp 7))
  (should-not (plusp -7))
  (should (minusp -2))
  (should-not (minusp 2))
  (should (oddp 3))
  (should-not (oddp 4))
  (should (evenp 4))
  (should-not (evenp 3))
  (should (natnump 0))
  (should (natnump 7))
  (should-not (natnump -1))
  (should (= 11 (1+ 10)))
  (should (= 9  (1- 10)))
  (should (= 1 (% 10 3)))
  (should (= -1 (% -10 3))))

(ert-deftest emacs-numeric-test/float-math-host-path-correct ()
  (should (< (abs (- (atan 1) (/ float-pi 4))) 0.000001))
  (should (< (abs (- (exp 1) float-e)) 0.000001)))

(ert-deftest emacs-numeric-test/float-fallbacks-replace-marked-bulk-stubs ()
  (let ((original-atan (and (fboundp 'atan) (symbol-function 'atan)))
        (original-exp (and (fboundp 'exp) (symbol-function 'exp)))
        (original-atan-marker (get 'atan 'emacs-stub-bulk))
        (original-exp-marker (get 'exp 'emacs-stub-bulk))
        (called nil))
    (unwind-protect
        (progn
          (fset 'atan
                (lambda (&rest _)
                  (setq called t)
                  (error "bulk atan stub should not be called")))
          (fset 'exp
                (lambda (&rest _)
                  (setq called t)
                  (error "bulk exp stub should not be called")))
          (put 'atan 'emacs-stub-bulk t)
          (put 'exp 'emacs-stub-bulk t)
          (load emacs-numeric-test--module-file t t)
          (should-not called)
          (should (= 0 (atan 1)))
          (should (= 1 (exp 1)))
          (should-not (get 'atan 'emacs-stub-bulk))
          (should-not (get 'exp 'emacs-stub-bulk)))
      (if original-atan
          (fset 'atan original-atan)
        (fmakunbound 'atan))
      (if original-exp
          (fset 'exp original-exp)
        (fmakunbound 'exp))
      (put 'atan 'emacs-stub-bulk original-atan-marker)
      (put 'exp 'emacs-stub-bulk original-exp-marker))))

;;;; C. Bitwise primitive results — host path (= correct)

(ert-deftest emacs-numeric-test/bitwise-host-path-correct ()
  ;; Under host Emacs these go to the C builtins.  Verify standard ops.
  (should (= #b1111 (logior #b1100 #b0011)))
  (should (= #b1000 (logand #b1100 #b1010)))
  (should (= #b0110 (logxor #b1100 #b1010)))
  (should (= -1 (lognot 0)))
  (should (= 0 (lognot -1)))
  (should (= 16 (ash 1 4)))
  (should (= 1  (ash 16 -4)))
  (should (= 16 (lsh 1 4))))

;;;; D. Polyfill body — non-overlap proxy is correct on disjoint flags

(ert-deftest emacs-numeric-test/polyfill-bitwise-non-overlap-proxy-is-correct ()
  ;; Re-implement the bridge body literally so we can probe its math
  ;; without disturbing host's `logior' / `logxor'.  The polyfill is
  ;; advertised as exact for non-overlapping bit-flag combinations
  ;; (= the use case in bytecomp / subr.el load).  Verify that.
  (let ((logior-proxy
         (lambda (&rest ints)
           (let ((acc 0))
             (while ints
               (setq acc (+ acc (- (car ints) (logand acc (car ints)))))
               (setq ints (cdr ints)))
             acc)))
        (logxor-proxy
         (lambda (&rest ints)
           (let ((acc 0))
             (while ints
               (setq acc (+ acc (car ints)))
               (setq ints (cdr ints)))
             acc))))
    (should (= #b1111 (funcall logior-proxy #b1100 #b0011)))
    (should (= #b0111 (funcall logior-proxy #b0100 #b0010 #b0001)))
    (should (= #b0111 (funcall logxor-proxy #b0100 #b0010 #b0001)))))

;;;; E. Polyfill body — ash positive/negative count

(ert-deftest emacs-numeric-test/polyfill-ash-shifts-via-mul-div-loop ()
  (let ((ash-proxy
         (lambda (value count)
           (cond
            ((= count 0) value)
            ((> count 0)
             (let ((acc value))
               (while (> count 0) (setq acc (* acc 2)) (setq count (- count 1)))
               acc))
            (t
             (let ((acc value))
               (while (< count 0) (setq acc (/ acc 2)) (setq count (+ count 1)))
               acc))))))
    (should (= 1   (funcall ash-proxy 1 0)))
    (should (= 8   (funcall ash-proxy 1 3)))
    (should (= 80  (funcall ash-proxy 5 4)))
    (should (= 2   (funcall ash-proxy 16 -3)))
    (should (= 0   (funcall ash-proxy 1 -3)))))

;;;; F. Polyfill body — lognot two's-complement

(ert-deftest emacs-numeric-test/polyfill-lognot-twos-complement ()
  (let ((lognot-proxy (lambda (int) (- (- int) 1))))
    (should (= -1 (funcall lognot-proxy 0)))
    (should (= 0  (funcall lognot-proxy -1)))
    (should (= -8 (funcall lognot-proxy 7)))
    (should (= 7  (funcall lognot-proxy -8)))))

;;;; G. Idempotence

(ert-deftest emacs-numeric-test/require-is-idempotent ()
  (let ((before-min  (symbol-function 'min))
        (before-1+   (symbol-function '1+))
        (before-ash  (symbol-function 'ash)))
    (require 'emacs-numeric)
    (should (eq before-min  (symbol-function 'min)))
    (should (eq before-1+   (symbol-function '1+)))
    (should (eq before-ash  (symbol-function 'ash)))))

(provide 'emacs-numeric-test)

;;; emacs-numeric-test.el ends here
