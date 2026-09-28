;;; nelisp-eln-system-loader-indirection-test.el --- root GOT validation -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-eln-system-loader)

(defun nelisp-eln-system-loader-indirection-test--call
    (address &optional target digest object closed)
  "Call the validator against a synthetic root and return result/read count."
  (let* ((reads 0)
         (file-bytes (make-string 64 0))
         (state (list :path "fixture.eln" :bias #x1000000 :state 'live
                      :file-bytes file-bytes :file-sha "digest"
                      :elf (list :loads (list (list #x1000 8 32 40
                                                     nelisp-eln-system-loader--pf-r
                                                     #x1000)))))
         (info (cond ((eq object 'missing) nil)
                     ((eq object 'bss)
                      (list :address #x1003000 :binding 1
                            :type nelisp-eln-system-loader--stt-object
                            :size 8 :file-backed-size 0))
                     (object object)
                     (t (list :address #x1003000 :binding 1
                              :type nelisp-eln-system-loader--stt-object
                              :size 8 :file-backed-size 8))))
         result condition)
    (cl-letf (((symbol-function 'nelisp-eln-system-loader--state)
               (lambda (_handle)
                 (if closed
                     (nelisp-eln-system-loader--fail 'stale-handle)
                   state)))
              ((symbol-function 'nelisp-eln-system-loader-symbol-info)
               (lambda (_handle _name) info))
              ((symbol-function 'nelisp-eln-system-loader--read-file)
               (lambda (_path) file-bytes))
              ((symbol-function 'nelisp-eln-system-loader--file-sha256)
               (lambda (_bytes) (or digest "digest")))
              ((symbol-function 'ptr-read-u64)
               (lambda (_address _offset)
                 (setq reads (1+ reads))
                 (or target #x1003000))))
      (condition-case err
          (setq result
                (nelisp-eln-system-loader-validate-root-indirection
                 'fixture address "freloc_link_table"))
        (nelisp-eln-system-loader-error (setq condition (cadr err))))
      (list result condition reads))))

(ert-deftest nelisp-eln-root-indirection-valid-last-slot-boundary ()
  (let ((result
         (nelisp-eln-system-loader-indirection-test--call
          (+ #x1000000 #x1000 24))))
    (should (equal result (list #x1003000 nil 1)))))

(ert-deftest nelisp-eln-root-indirection-accepts-zero-fill-target ()
  (let ((result
         (nelisp-eln-system-loader-indirection-test--call
          (+ #x1000000 #x1000) #x1003000 nil 'bss)))
    (should (equal result (list #x1003000 nil 1)))))

(ert-deftest nelisp-eln-root-indirection-rejects-out-of-range-before-read ()
  (dolist (address (list (+ #x1000000 #x1000 25)
                         (+ #x1000000 #x1000 32)
                         #x1000))
    (let ((result (nelisp-eln-system-loader-indirection-test--call address)))
      (should (eq (cadr result)
                  'indirection-outside-root-readable-file-load))
      (should (= (nth 2 result) 0)))))

(ert-deftest nelisp-eln-root-indirection-rejects-swapped-pointer ()
  (let ((result
         (nelisp-eln-system-loader-indirection-test--call
          (+ #x1000000 #x1000) #x1003008)))
    (should (eq (cadr result) 'root-indirection-target-mismatch))
    (should (= (nth 2 result) 1))))

(ert-deftest nelisp-eln-root-indirection-rejects-missing-object-before-read ()
  (let ((result
         (nelisp-eln-system-loader-indirection-test--call
          (+ #x1000000 #x1000) nil nil 'missing)))
    (should (eq (cadr result) 'symbol-not-found))
    (should (= (nth 2 result) 0))))

(ert-deftest nelisp-eln-root-indirection-rejects-closed-handle-before-read ()
  (let ((result
         (nelisp-eln-system-loader-indirection-test--call
          (+ #x1000000 #x1000) nil nil nil t)))
    (should (eq (cadr result) 'stale-handle))
    (should (= (nth 2 result) 0))))

(ert-deftest nelisp-eln-root-indirection-rejects-changed-file-before-read ()
  (let ((result
         (nelisp-eln-system-loader-indirection-test--call
          (+ #x1000000 #x1000) nil "changed")))
    (should (eq (cadr result) 'root-file-changed))
    (should (= (nth 2 result) 0))))

(provide 'nelisp-eln-system-loader-indirection-test)

;;; nelisp-eln-system-loader-indirection-test.el ends here
