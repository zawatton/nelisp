;;; nelisp-native-runtime-dispatch-test.el --- native publication guards -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; Execute production installer/dispatch instructions in a small ELF. Only
;; BSS addresses and original allocator/collector bodies are substituted.
;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'nelisp-aot-compiler)
(require 'nelisp-standalone-build)

(defun nelisp-native-runtime-dispatch-test--source (&optional mutation)
  "Return a freestanding native probe, optionally applying MUTATION."
  (let* ((page #x34000000)
         (addresses `((nl_runtime_reload_state . ,page)
                      (nl_thread_registry . ,(+ page 128))
                      (nl_gc_loop_ctx . ,(+ page 256))))
         (forms (nelisp-standalone--runtime-reload-forms)))
    (cl-labels ((rewrite (form)
                  (cond
                   ((and (eq mutation 'ignore-workers)
                         (equal form '(ptr-read-u64 (data-addr nl_thread_registry) 0))) 0)
                   ((eq (car-safe form) 'data-addr)
                    (or (cdr (assq (cadr form) addresses))
                        (error "Unexpected BSS dependency %S" form)))
                   ((consp form) (cons (rewrite (car form)) (rewrite (cdr form))))
                   (t form))))
      `(seq
        (defun nl_seq2 (_a b) b)
        (defun nl_runtime_reload_alloc_original (size align) (+ size align))
        (defun nl_runtime_reload_gc_original (mode) (+ mode 10))
        ,@(mapcar #'rewrite forms)
        ,(rewrite
          (nelisp-standalone--runtime-reload-gc-wrapper
           '(defun nl_gc_collect (a b c d e f g) 0)
           (cl-position "nl_gc_collect" nelisp-runtime-reload-gc-contract
                        :key #'car :test #'equal)))
        (defun nl_runtime_reload_original_nl_gc_collect (a b c d e f g)
          (+ a (+ b (+ c (+ d (+ e (+ f g)))))))
        (defun rr_alloc (size align) (+ size (+ align 100)))
        (defun rr_gc (mode) (+ mode 200))
        (defun rr_gc_seven (a b c d e f g)
          (+ 1000 (+ a (+ b (+ c (+ d (+ e (+ f g))))))))
        (defun rr_assert (value code)
          (if (= value 0) (syscall-direct 60 code 0 0 0 0 0) 0))
        (defun rr_unchanged ()
          (rr_assert
           (and (= (ptr-read-u64 ,page 0) (addr-of rr_alloc))
                (= (ptr-read-u64 ,page 8) ,(+ page 512))
                (= (ptr-read-u64 ,page 16) 1)) 31))
        (defun rr_reject (offset value reason)
          (seq
           (ptr-write-u64 ,page offset value)
           (rr_assert (= (nl_runtime_reload_install
                          (addr-of rr_alloc) ,(+ page 512) 2) reason)
                      (+ 40 reason))
           (rr_unchanged)
           (rr_assert (= (ptr-read-u64 ,page 56)
                         (if (= offset 56) value 0)) 49)
           (ptr-write-u64 ,page offset 0)))
        (defun rr_run ()
          (seq
           (rr_assert (= (syscall-direct 9 ,page 4096 3 50 -1 0) ,page) 1)
           (ptr-write-u64 ,(+ page 512) 0
                          ,(length nelisp-runtime-reload-gc-contract))
           (ptr-write-u64 ,(+ page 512) 8 ,#x4e4c474332)
           (ptr-write-u64 ,(+ page 512)
                          ,(+ 16 (* 8 (cl-position
                                       "nl_gc_collect_recorded_mark_sweep_body"
                                       nelisp-runtime-reload-gc-contract
                                       :key #'car :test #'equal)))
                          (addr-of rr_gc))
           (ptr-write-u64 ,(+ page 512)
                          ,(+ 16 (* 8 (cl-position "nl_gc_collect"
                                                   nelisp-runtime-reload-gc-contract
                                                   :key #'car :test #'equal)))
                          (addr-of rr_gc_seven))
           (rr_assert (= (nl_alloc_bytes_uncheck 7 1) 8) 2)
           (rr_assert (= (nl_gc_collect_recorded_mark_sweep_body 3) 13) 3)
           (rr_assert (= (nl_gc_collect 1 2 3 4 5 6 7) 28) 14)
           (rr_assert (= (nl_runtime_reload_install
                          (addr-of rr_alloc) ,(+ page 512) 1) 0) 4)
           (rr_assert (= (nl_alloc_bytes_uncheck 7 1) 108) 5)
           (rr_assert (= (nl_gc_collect_recorded_mark_sweep_body 3) 203) 6)
           (rr_assert (= (nl_gc_collect 1 2 3 4 5 6 7) 1028) 15)
           (rr_assert (and (= (ptr-read-u64 ,page 24) 0)
                           (= (ptr-read-u64 ,page 32) 0)
                           (= (ptr-read-u64 ,page 40) 1)
                           (= (ptr-read-u64 ,page 48) 2)) 7)
           (rr_assert (= (nl_runtime_reload_install 0 ,(+ page 512) 2) 1) 8)
           (rr_unchanged)
           (rr_assert (= (nl_runtime_reload_install
                          (addr-of rr_alloc) ,(+ page 512) 1) 2) 9)
           (rr_unchanged)
           (rr_reject 128 1 3)
           (rr_reject 280 1 4)
           (rr_reject 24 1 5)
           (rr_reject 32 1 5)
           (rr_reject 56 1 6)
           (rr_reject 520 0 7)
           (ptr-write-u64 ,(+ page 512) 8 ,#x4e4c474332)
           (rr_assert (= (nl_runtime_reload_install 0 0 2) 0) 10)
           (rr_assert (= (nl_alloc_bytes_uncheck 7 1) 8) 11)
           (rr_assert (= (nl_gc_collect_recorded_mark_sweep_body 3) 13) 12)
           (rr_assert (and (= (ptr-read-u64 ,page 0) 0)
                           (= (ptr-read-u64 ,page 8) 0)
                           (= (ptr-read-u64 ,page 16) 2)
                           (= (ptr-read-u64 ,page 56) 0)) 13)
           0))
        (exit (rr_run))))))

(defun nelisp-native-runtime-dispatch-test--run (&optional mutation)
  "Compile and execute the production native probe with MUTATION."
  (unless (and (eq system-type 'gnu/linux)
               (string-match-p "x86_64\\|amd64" system-configuration))
    (ert-skip "The native reload ABI requires Linux x86_64"))
  (let ((path (make-temp-file "nelisp-runtime-dispatch-")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-sexp
           (nelisp-native-runtime-dispatch-test--source mutation) path)
          (call-process path nil nil nil))
      (delete-file path))))

(ert-deftest nelisp-native-runtime-dispatch-native-publication-and-guards ()
  (should (= (nelisp-native-runtime-dispatch-test--run) 0)))

(ert-deftest nelisp-native-runtime-dispatch-worker-guard-mutation-is-red ()
  (should (= (nelisp-native-runtime-dispatch-test--run 'ignore-workers) 43)))

(provide 'nelisp-native-runtime-dispatch-test)
;;; nelisp-native-runtime-dispatch-test.el ends here
