;;; nelisp-project-profile-test.el --- Profiling semantics -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'nelisp-project-profile)

(ert-deftest nelisp-project-profile-values-errors-and-restoration ()
  (let (report)
    (cl-letf (((symbol-function 'profile-test-step) (lambda (n) (+ n 1)))
              ((symbol-function 'profile-test-entry) (lambda () (profile-test-step 3))))
      (let ((original (symbol-function 'profile-test-step)))
        (should (= (nelisp-project-profile-run
                    'profile-test-entry '(profile-test-entry profile-test-step)
                    (lambda (json) (setq report (json-parse-string json :object-type 'alist)))) 4))
        (should (eq original (symbol-function 'profile-test-step)))
        (should (equal (alist-get 'status report) "ok"))
        (should (= (alist-get 'calls (aref (alist-get 'functions report) 1)) 1)))
      (fset 'profile-test-entry (lambda () (error "original failure")))
      (should-error (nelisp-project-profile-run
                     'profile-test-entry '(profile-test-entry)
                     (lambda (json) (setq report (json-parse-string json :object-type 'alist)))))
      (should (equal (alist-get 'status report) "error"))
      (should (= (alist-get 'completed (aref (alist-get 'functions report) 0)) 0)))))

(ert-deftest nelisp-project-profile-backward-clock-and-redefinition ()
  (let ((ticks '(2.0 1.0)) report)
    (cl-letf (((symbol-function 'float-time) (lambda () (pop ticks)))
              ((symbol-function 'profile-test-entry)
               (lambda () (fset 'profile-test-entry (lambda () 9)) 7)))
      (should (= (nelisp-project-profile-run
                  'profile-test-entry '(profile-test-entry)
                  (lambda (json) (setq report (json-parse-string json :object-type 'alist)))) 7))
      (should (= (profile-test-entry) 9))
      (let ((row (aref (alist-get 'functions report) 0)))
        (should (= (alist-get 'invalid_intervals row) 1))
        (should (= (alist-get 'elapsed_us row) 0))))))

(ert-deftest nelisp-project-profile-validates-before-installation ()
  (cl-letf (((symbol-function 'profile-test-entry) (lambda () 7)))
    (let ((original (symbol-function 'profile-test-entry)))
      (should-error (nelisp-project-profile-run 'profile-test-entry '(profile-test-entry car)))
      (should (eq original (symbol-function 'profile-test-entry))))))

(provide 'nelisp-project-profile-test)
