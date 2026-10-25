;;; nelisp-bytecode-callbc-test.el --- In-loop call/return arm cuts -*- lexical-binding: t; -*-

;; Structure-level checks for the callbc lane: the in-loop call arm (opcodes
;; 32..37) and return arm (opcode 135) of wf_bytecode_w_fast_run.  Native
;; behaviour is covered by the differential corpus on the built binary.

(require 'ert)
(require 'cl-lib)
(require 'nelisp-bytecode-stackset-fast-test)

(defun cbc--fast-run () (sst--function 'wf_bytecode_w_fast_run))

(defun cbc--call-arm ()
  (sst--find (cbc--fast-run)
             (lambda (form)
               (and (eq (car-safe form) 'let*)
                    (equal (car-safe (car-safe (cadr form))) 'argc)
                    (equal (cadr (car (cadr form))) '(- raw 32))))))

(defun cbc--return-arm ()
  (sst--find (cbc--fast-run)
             (lambda (form)
               (and (eq (car-safe form) 'if) (equal (cadr form) '(< raw 136))))))

(defun cbc--count (tree pred)
  (cond ((funcall pred tree) 1)
        ((consp tree) (+ (cbc--count (car tree) pred) (cbc--count (cdr tree) pred)))
        (t 0)))

(defun cbc--count-form (tree form)
  (cbc--count tree (lambda (x) (equal x form))))

(defun cbc--pos (tree form)
  "Preorder index of the first subtree equal to FORM, or nil."
  (let ((n 0) (hit nil))
    (cl-labels ((walk (x)
                  (unless hit
                    (when (equal x form) (setq hit n))
                    (setq n (1+ n))
                    (when (consp x) (walk (car x)) (walk (cdr x))))))
      (walk tree))
    hit))

(ert-deftest cbc-arms-found ()
  (should (cbc--call-arm))
  (should (cbc--return-arm)))

(ert-deftest cbc-poll-checks-the-budget-word-inline ()
  "GNU maybe_gc is one load and compare; the arm tests the word before any call."
  (let ((arm (cbc--call-arm)))
    (should (= 1 (cbc--count-form
                  arm '(if (< (heap-read-u64 (data-addr nl_gc_until) 0) 0)
                           (if (= (wf_bytecode_poll) 1) (setq last-name -1) 0)
                         0))))))

(ert-deftest cbc-root-frame-header-keeps-only-read-words ()
  "The walkers read the marker and the slot count; the two padding words are not stored."
  (let ((arm (cbc--call-arm)))
    (should (= 1 (cbc--count-form arm '(ptr-write-u64 top 0 1464226381))))
    (should (= 1 (cbc--count-form arm '(ptr-write-u64 top 8 nslots))))
    (should (= 0 (cbc--count-form arm '(ptr-write-u64 top 16 0))))
    (should (= 0 (cbc--count-form arm '(ptr-write-u64 top 24 0))))
    (dolist (off '(32 40 48 56))
      (should (= 1 (cbc--count-form arm `(ptr-write-u64 top ,off 3)))))))

(ert-deftest cbc-call-saves-caller-code-state-before-the-switch ()
  (let ((arm (cbc--call-arm)))
    (dolist (f '((setq sd data) (setq sl length) (setq sk constants) (setq sn nconst)))
      (should (= 1 (cbc--count-form arm f)))
      (should (< (cbc--pos arm f) (cbc--pos arm '(setq mem fp2)))))))

(ert-deftest cbc-return-restores-instead-of-decoding ()
  (let ((arm (cbc--return-arm)))
    (dolist (f '((setq data sd) (setq length sl) (setq constants sk) (setq nconst sn)
                 (setq sd 0)))
      (should (= 1 (cbc--count-form arm f))))
    ;; The decode stays for a caller that was not saved (entry through the full loop).
    (should (= 1 (cbc--count-form arm '(setq constants (+ vp 8)))))
    (should (< (cbc--pos arm '(setq data sd)) (cbc--pos arm '(setq constants (+ vp 8)))))))

(ert-deftest cbc-saved-state-is-only-written-by-the-two-arms ()
  "Every write of the code registers is in the call or the return arm."
  (let* ((fr (cbc--fast-run))
         (total (cbc--count fr (lambda (x) (and (eq (car-safe x) 'setq) (eq (cadr x) 'constants)))))
         (in-arms (+ (cbc--count (cbc--call-arm) (lambda (x) (and (eq (car-safe x) 'setq) (eq (cadr x) 'constants))))
                     (cbc--count (cbc--return-arm) (lambda (x) (and (eq (car-safe x) 'setq) (eq (cadr x) 'constants)))))))
    (should (= total in-arms))
    (should (= total 3))))

(provide 'nelisp-bytecode-callbc-test)
