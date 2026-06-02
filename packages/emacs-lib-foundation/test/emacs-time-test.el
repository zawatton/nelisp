;;; emacs-time-test.el --- Tests for emacs-time  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the time / truncate polyfills split out of `emacs-stub.el'.
;; This file deliberately reloads the module in a controlled way so we
;; can exercise both the host-preserved path and the actual polyfill
;; path without changing the repo sources.

;;; Code:

(require 'ert)
(require 'emacs-time)

(defconst emacs-time-test--module-file
  (expand-file-name "../src/emacs-time.el"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defun emacs-time-test--with-reloaded-module (symbols thunk)
  "Reload `emacs-time' with SYMBOLS temporarily unbound, then call THUNK."
  (let ((saved nil))
    (unwind-protect
        (progn
          (dolist (sym symbols)
            (push (cons sym (and (fboundp sym) (symbol-function sym))) saved)
            (fmakunbound sym))
          (load emacs-time-test--module-file t t)
          (funcall thunk))
      (dolist (cell saved)
        (if (cdr cell)
            (fset (car cell) (cdr cell))
          (fmakunbound (car cell)))))))

;;;; Load / feature contract

(ert-deftest emacs-time-test/require-loads-cleanly ()
  (should (featurep 'emacs-time))
  (should (fboundp 'float-time))
  (should (fboundp 'current-time))
  (should (fboundp 'truncate)))

(ert-deftest emacs-time-test/guard-keeps-an-already-correct-truncate ()
  (let ((original (symbol-function 'truncate)))
    (emacs-time-test--with-reloaded-module
     nil
     (lambda ()
       (should (eq original (symbol-function 'truncate)))
       (should (= 3 (truncate 3.7)))))))

(ert-deftest emacs-time-test/guard-replaces-marked-bulk-truncate-without-calling-it ()
  (let ((original (and (fboundp 'truncate) (symbol-function 'truncate)))
        (original-marker (get 'truncate 'emacs-stub-bulk))
        (called nil))
    (unwind-protect
        (progn
          (fset 'truncate
                (lambda (&rest _)
                  (setq called t)
                  (error "bulk truncate stub should not be called")))
          (put 'truncate 'emacs-stub-bulk t)
          (load emacs-time-test--module-file t t)
          (should-not called)
          (should (= 3 (truncate 3.7)))
          (should-not (get 'truncate 'emacs-stub-bulk)))
      (if original
          (fset 'truncate original)
        (fmakunbound 'truncate))
      (put 'truncate 'emacs-stub-bulk original-marker))))

;;;; Polyfill path

(ert-deftest emacs-time-test/float-time-defaults-to-zero ()
  (emacs-time-test--with-reloaded-module
   '(truncate float-time current-time nl-current-unix-time nelisp--syscall)
   (lambda ()
     (should (numberp (float-time)))
     (should (= 0 (float-time))))))

(ert-deftest emacs-time-test/float-time-uses-nl-current-unix-time-when-available ()
  (emacs-time-test--with-reloaded-module
   '(truncate float-time current-time nl-current-unix-time)
   (lambda ()
     (fset 'nl-current-unix-time (lambda () 1234))
     (should (= 1234 (float-time))))))

(ert-deftest emacs-time-test/float-time-can-use-nelisp-syscall-time ()
  (emacs-time-test--with-reloaded-module
   '(truncate float-time current-time nl-current-unix-time nelisp--syscall)
   (lambda ()
     (fset 'nelisp--syscall
           (lambda (nr ptr)
             (and (= nr 201) (= ptr 0) 5678)))
     (should (= 5678 (float-time))))))

(ert-deftest emacs-time-test/current-time-is-a-four-element-list ()
  (emacs-time-test--with-reloaded-module
   '(truncate float-time current-time nl-current-unix-time nelisp--syscall)
   (lambda ()
     (let ((now (current-time)))
       (should (listp now))
       (should (= 4 (length now)))
       (should (= (float-time) (car now)))
       (should (equal now (list (float-time) 0 0 0)))))))

(ert-deftest emacs-time-test/truncate-integer-and-positive-float ()
  (emacs-time-test--with-reloaded-module
   '(truncate float-time current-time nl-current-unix-time)
   (lambda ()
     (should (= 4 (truncate 4)))
     (should (= 3 (truncate 3.7))))))

(ert-deftest emacs-time-test/truncate-negative-float ()
  (emacs-time-test--with-reloaded-module
   '(truncate float-time current-time nl-current-unix-time)
   (lambda ()
     (should (= -3 (truncate -3.7))))))

(ert-deftest emacs-time-test/truncate-divisor-and-nil ()
  (emacs-time-test--with-reloaded-module
   '(truncate float-time current-time nl-current-unix-time)
   (lambda ()
     (should (= 3 (truncate 7 2)))
     (should (= 0 (truncate nil))))))

(provide 'emacs-time-test)

;;; emacs-time-test.el ends here
