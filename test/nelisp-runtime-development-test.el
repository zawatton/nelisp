;;; nelisp-runtime-development-test.el --- staged native reload boundary tests -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Code:
(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path
             (expand-file-name "../lisp"
                               (file-name-directory
                                (or load-file-name buffer-file-name))))
(require 'nelisp-runtime-development)

(defconst nelisp-runtime-development-test--root
  (expand-file-name ".."
                    (file-name-directory
                     (or load-file-name buffer-file-name))))

(defun nelisp-runtime-development-test--status (generation)
  (list :status 'ready :phase :state :generation generation))

(defun nelisp-runtime-development-test--mocks
    (generation binary &optional after-generation after-binary)
  (let ((status-calls 0)
        (binary-calls 0)
        (install-calls 0)
        (load-calls 0))
    (list
     (list
      'nelisp-runtime-reload-status
      (lambda ()
        (setq status-calls (1+ status-calls))
        (nelisp-runtime-development-test--status
         (if (and after-generation (> status-calls 1))
             after-generation
           generation))))
     (list
      'nelisp-native-load--running-binary-sha256
      (lambda ()
        (setq binary-calls (1+ binary-calls))
        (if (and after-binary (> binary-calls 1)) after-binary binary)))
     (list
      'call-process
      (lambda (&rest _args) 0))
     (list
      'nelisp-native-load-raw-artifact
      (lambda (&rest _args)
        (setq load-calls (1+ load-calls))
        '(:runtime-abi "nelisp-runtime-raw-v2"
          :entry-name "nl_alloc_bytes_uncheck" :arity 2
          :exports (("nl_gc_collect_recorded_mark_sweep_body" . 200)))) )
     (list
      'nelisp-native-load-raw-export-address
      (lambda (&rest _args) 200))
     (list
      'nelisp-native-load-raw-install
      (lambda (&rest _args) (setq install-calls (1+ install-calls))
        (list :status 'published :phase :publish :generation (1+ generation))))
     (list :counts (lambda () (list status-calls binary-calls load-calls install-calls))))))

(cl-defmacro nelisp-runtime-development-test--with-mocks
    ((generation binary &optional after-generation after-binary) &rest body)
  (declare (indent 1) (debug t))
  `(let* ((mocks (nelisp-runtime-development-test--mocks
                 ,generation ,binary ,after-generation ,after-binary))
          (status-fn (cadr (nth 0 mocks)))
          (binary-fn (cadr (nth 1 mocks)))
          (call-fn (cadr (nth 2 mocks)))
          (artifact-fn (cadr (nth 3 mocks)))
          (export-fn (cadr (nth 4 mocks)))
          (install-fn (cadr (nth 5 mocks)))
          (counts (cadr (nth 6 mocks))))
     (cl-letf (((symbol-function 'nelisp-runtime-reload-status) status-fn)
               ((symbol-function 'nelisp-native-load--running-binary-sha256)
                binary-fn)
               ((symbol-function 'call-process) call-fn)
               ((symbol-function 'nelisp-native-load-raw-artifact) artifact-fn)
               ((symbol-function 'nelisp-native-load-raw-export-address)
                export-fn)
               ((symbol-function 'nelisp-native-load-raw-install) install-fn))
       ,@body)))

(ert-deftest nelisp-runtime-development/stage-does-not-install ()
  (nelisp-runtime-development-test--with-mocks (7 "aaaaaaaa")
    (let ((result (nelisp-runtime-build-and-stage
                   nelisp-runtime-development-test--root)))
      (should (eq (plist-get result :status) 'staged))
      (should (= (nth 3 (funcall counts)) 0))
      (should (plist-get result :alloc-handle))
      (should (plist-get result :gc-handle)))))

(ert-deftest nelisp-runtime-development/stage-rejects-generation-change ()
  (nelisp-runtime-development-test--with-mocks (7 "aaaaaaaa" 8)
    (let ((result (nelisp-runtime-build-and-stage
                   nelisp-runtime-development-test--root)))
      (should (eq (plist-get result :status) 'rejected))
      (should (eq (plist-get result :phase) :load))
      (should (= (nth 3 (funcall counts)) 0)))))

(ert-deftest nelisp-runtime-development/stage-rejects-binary-change ()
  (nelisp-runtime-development-test--with-mocks (7 "aaaaaaaa" nil "bbbbbbbb")
    (let ((result (nelisp-runtime-build-and-stage
                   nelisp-runtime-development-test--root)))
      (should (eq (plist-get result :status) 'rejected))
      (should (eq (plist-get result :phase) :load))
      (should (= (nth 3 (funcall counts)) 0)))))

(ert-deftest nelisp-runtime-development/compatibility-installs-once ()
  (nelisp-runtime-development-test--with-mocks (7 "aaaaaaaa")
    (let ((result (nelisp-runtime-rebuild-and-reload
                   nelisp-runtime-development-test--root)))
      (should (eq (plist-get result :status) 'published))
      (should (eq (plist-get result :phase) :publish))
      (should (= (nth 3 (funcall counts)) 1)))))

(ert-deftest nelisp-runtime-development/build-failure-does-not-install ()
  (nelisp-runtime-development-test--with-mocks (7 "aaaaaaaa")
    (cl-letf (((symbol-function 'call-process)
               (lambda (&rest _args) 1)))
      (let ((result (nelisp-runtime-rebuild-and-reload
                     nelisp-runtime-development-test--root)))
        (should (eq (plist-get result :status) 'rejected))
        (should (= (nth 3 (funcall counts)) 0))))))

(ert-deftest nelisp-runtime-development/install-error-keeps-rejected-envelope ()
  (let ((install-calls 0))
    (cl-letf (((symbol-function 'nelisp-runtime-build-and-stage)
               (lambda (&rest _args)
                 '(:status staged :repository "r" :artifact "a"
                   :source "s" :binary-sha256 "b" :generation 3
                   :alloc-handle alloc :gc-handle gc)))
              ((symbol-function 'nelisp-runtime-reload-status)
               (lambda () '(:status ready :generation 3)))
              ((symbol-function 'nelisp-native-load--running-binary-sha256)
               (lambda () "b"))
              ((symbol-function 'nelisp-native-load-raw-install)
               (lambda (&rest _args)
                 (setq install-calls (1+ install-calls))
                 (error "simulated install failure"))))
      (let ((result (nelisp-runtime-rebuild-and-reload "r")))
        (should (eq (plist-get result :status) 'rejected))
        (should (eq (plist-get result :phase) :publish))
        (should (= install-calls 1))))))

(defconst nelisp-runtime-development-test--staged
  '(:status staged :repository "r" :artifact "a" :source "s"
    :binary-sha256 "b" :generation 3 :alloc-handle alloc :gc-handle gc))

(ert-deftest nelisp-runtime-development/prepublish-generation-change-skips-install ()
  (let ((install-calls 0))
    (cl-letf (((symbol-function 'nelisp-runtime-build-and-stage)
               (lambda (&rest _args)
                 nelisp-runtime-development-test--staged))
              ((symbol-function 'nelisp-runtime-reload-status)
               (lambda () '(:status ready :generation 4)))
              ((symbol-function 'nelisp-native-load--running-binary-sha256)
               (lambda () "b"))
              ((symbol-function 'nelisp-native-load-raw-install)
               (lambda (&rest _args) (setq install-calls (1+ install-calls))
                 '(:status published))))
      (let ((result (nelisp-runtime-rebuild-and-reload "r")))
        (should (eq (plist-get result :status) 'rejected))
        (should (= install-calls 0))))))

(ert-deftest nelisp-runtime-development/prepublish-binary-change-skips-install ()
  (let ((install-calls 0))
    (cl-letf (((symbol-function 'nelisp-runtime-build-and-stage)
               (lambda (&rest _args)
                 nelisp-runtime-development-test--staged))
              ((symbol-function 'nelisp-runtime-reload-status)
               (lambda () '(:status ready :generation 3)))
              ((symbol-function 'nelisp-native-load--running-binary-sha256)
               (lambda () "changed"))
              ((symbol-function 'nelisp-native-load-raw-install)
               (lambda (&rest _args) (setq install-calls (1+ install-calls))
                 '(:status published))))
      (let ((result (nelisp-runtime-rebuild-and-reload "r")))
        (should (eq (plist-get result :status) 'rejected))
        (should (= install-calls 0))))))

(ert-deftest nelisp-runtime-development/prepublish-identity-error-is-rejected ()
  (cl-letf (((symbol-function 'nelisp-runtime-build-and-stage)
             (lambda (&rest _args)
               nelisp-runtime-development-test--staged))
            ((symbol-function 'nelisp-runtime-reload-status)
             (lambda () (error "status unavailable")))
            ((symbol-function 'nelisp-native-load--running-binary-sha256)
             (lambda () "b"))
            ((symbol-function 'nelisp-native-load-raw-install)
             (lambda (&rest _args) (error "must not install"))))
    (let ((result (nelisp-runtime-rebuild-and-reload "r")))
      (should (eq (plist-get result :status) 'rejected))
      (should (eq (plist-get result :phase) :publish)))))

(provide 'nelisp-runtime-development-test)
;;; nelisp-runtime-development-test.el ends here
