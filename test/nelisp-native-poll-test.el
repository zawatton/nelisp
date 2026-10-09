;;; nelisp-native-poll-test.el --- Allocator-driven rooted polls -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'ert)
(require 'cl-lib)
(defvar nelisp-native-poll-force-gc nil)
(defconst nelisp-native-poll-test--source
  (expand-file-name "../lisp/nelisp-native-poll.el"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defun nelisp-native-poll-test--run (body)
  "Run BODY with a fresh frozen provider and a controlled allocator."
  (let ((collections 0) (checks 0) (due 0) (force nil)
        (features (copy-sequence features))
        (quit-flag nil) (inhibit-quit t) (simulated-inhibit nil)
        (value (symbol-function 'symbol-value)) (nelisp-native-poll-force-gc nil)
        (source (or (getenv "POLL_TEST_SOURCE") nelisp-native-poll-test--source))
        (original (and (fboundp 'nelisp-bytecode-native-rooted-cfg-poll-function)
                       (symbol-function 'nelisp-bytecode-native-rooted-cfg-poll-function)))
        poll)
    (cl-letf (((symbol-function 'garbage-collect) (lambda () (setq collections (1+ collections))))
              ((symbol-function 'nelisp--native-symbol-addr) #'ignore)
              ((symbol-function 'nelisp-native-load--symbol-addr)
               (lambda (name) (should (equal name "nl_gc_midform_collect")) 4096))
              ((symbol-function 'ptr-call)
               (lambda (address &rest args)
                 (should (= address 4096)) (should (equal args '(0 0 0 0 0 0)))
                 (setq checks (1+ checks))
                 (when (/= due 0) (setq collections (1+ collections))) 0))
              ((symbol-function 'getenv) (lambda (name) (and force (equal name "F1_FORCE_GC") "1")))
              ((symbol-function 'symbol-value)
               (lambda (name) (if (eq name 'inhibit-quit) simulated-inhibit (funcall value name)))))
      (unwind-protect
          (progn
            (load source nil t t)
            (setq poll (nelisp-bytecode-native-rooted-cfg-poll-function))
	    ;; Bind the fixture state through lexical closures, avoiding global
	    ;; counter rewrites that could hide a stale frozen provider.
	    (funcall body poll
		     (lambda () (list collections checks quit-flag))
		     (lambda (value) (setq due value))
		     (lambda (value) (setq quit-flag value))
		     (lambda (value) (setq simulated-inhibit value))
		     (lambda () (setq force t) (nelisp-bytecode-native-rooted-cfg-poll-function))))
        (if original (fset 'nelisp-bytecode-native-rooted-cfg-poll-function original)
          (fmakunbound 'nelisp-bytecode-native-rooted-cfg-poll-function))))))

(ert-deftest nelisp-native-poll/no-periodic-full-gc ()
  (nelisp-native-poll-test--run
   (lambda (poll state &rest _)
     (dotimes (_ 130) (should-not (funcall poll)))
     (should (equal (funcall state) '(0 130 nil))))))

(ert-deftest nelisp-native-poll/allocator-due ()
  (nelisp-native-poll-test--run
   (lambda (poll state set-due &rest _)
     (funcall set-due 1) (should-not (funcall poll))
     (funcall set-due 0) (should-not (funcall poll))
     (should (equal (funcall state) '(1 2 nil))))))

(ert-deftest nelisp-native-poll/quit-every-edge ()
  (nelisp-native-poll-test--run
   (lambda (poll state _ set-quit set-inhibit &rest _ignored)
     (dotimes (_ 3) (funcall set-quit t) (should (funcall poll)))
     (should (equal (funcall state) '(0 3 nil)))
     (funcall set-quit t) (funcall set-inhibit t)
     (should-not (funcall poll))
     (should (equal (funcall state) '(0 4 t)))
     (funcall set-inhibit nil) (should (funcall poll))
     (should (equal (funcall state) '(0 5 nil))))))

(ert-deftest nelisp-native-poll/forced-stress ()
  (nelisp-native-poll-test--run
   (lambda (poll state _due _quit _inhibit force)
     (let ((forced (funcall force)))
       (dotimes (_ 3) (should-not (funcall forced))))
     (let ((nelisp-native-poll-force-gc t))
       (dotimes (_ 3) (should-not (funcall poll))))
     (should (equal (funcall state) '(6 0 nil))))))
