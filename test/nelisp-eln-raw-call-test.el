;;; nelisp-eln-raw-call-test.el --- tests for full-width raw calls -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-eln-raw-call)

(ert-deftest nelisp-eln-raw-call/split-u32-round-trip-boundaries ()
  (dolist (word '(2 70 -1 9223372036854775810
                    9223372036854775806 18446744073709551615))
    (let ((memory (make-hash-table :test #'eql)))
      (cl-letf (((symbol-function 'ptr-write-u32)
                 (lambda (_address offset value)
                   (puthash offset value memory)))
                ((symbol-function 'ptr-read-u32)
                 (lambda (_address offset)
                   (gethash offset memory 0))))
        (nelisp-eln-raw-call--write-word 1 0 word)
        (should (= (nelisp-eln-raw-call--read-word 1 0)
                   (nelisp-eln-abi-normalize-word word)))))))

(ert-deftest nelisp-eln-raw-call/reject-out-of-range-word ()
  (should-error (nelisp-eln-raw-call--write-word
                 1 0 18446744073709551616)
                :type 'nelisp-eln-abi-error))

(ert-deftest nelisp-eln-raw-call/second-allocation-cleanup-is-retained ()
  (let ((owner (vector 'nl-ffi-memory-owner 65536 4096 48))
        (allocations 0)
        (nelisp-eln-raw-call--pending-cleanup nil))
    (cl-letf (((symbol-function 'nl-ffi-memory-allocate)
               (lambda (_size)
                 (setq allocations (1+ allocations))
                 (if (= allocations 1) owner
                   (error "injected second allocation failure"))))
              ((symbol-function 'nl-ffi-memory-release)
               (lambda (_owner) (error "injected unmap failure"))))
      (should-error (nelisp-eln-raw-call-context-create))
      (should (= allocations 2))
      (should (equal nelisp-eln-raw-call--pending-cleanup (list owner))))))

(ert-deftest nelisp-eln-raw-call/rx-failure-retains-trampoline-mapping ()
  (let ((owner (vector 'nl-ffi-memory-owner 65536 4096 48))
        (nelisp-eln-raw-call--trampoline-owner nil)
        (nelisp-eln-raw-call--pending-cleanup nil))
    (cl-letf (((symbol-function 'nl-ffi-memory-allocate)
               (lambda (_size) owner))
              ((symbol-function 'nl-ffi-memory-address)
               (lambda (_owner) 65536))
              ((symbol-function 'ptr-write-u8) (lambda (_p _o _v) nil))
              ((symbol-function 'ptr-read-u8)
               (lambda (_p offset)
                 (aref nelisp-eln-raw-call--trampoline-bytes offset)))
              ((symbol-function 'syscall-direct)
               (lambda (&rest _args) -1))
              ((symbol-function 'nl-ffi-memory-release)
               (lambda (_owner) (error "injected unmap failure"))))
      (should-error (nelisp-eln-raw-call--ensure-trampoline)
                    :type 'nelisp-eln-raw-call-error)
      (should-not nelisp-eln-raw-call--trampoline-owner)
      (should (equal nelisp-eln-raw-call--pending-cleanup (list owner))))))

(ert-deftest nelisp-eln-raw-call/construction-failure-attempts-both-unmaps ()
  (let ((argv (vector 'nl-ffi-memory-owner 65536 4096 48))
        (out (vector 'nl-ffi-memory-owner 69632 4096 8))
        (allocations 0)
        (releases nil)
        (nelisp-eln-raw-call--pending-cleanup nil))
    (cl-letf (((symbol-function 'nl-ffi-memory-allocate)
               (lambda (_size)
                 (setq allocations (1+ allocations))
                 (if (= allocations 1) argv out)))
              ((symbol-function 'nelisp-eln-raw-call--ensure-trampoline)
               (lambda ()
                 (signal 'nelisp-eln-raw-call-error '(injected-rx-failure))))
              ((symbol-function 'nl-ffi-memory-release)
               (lambda (owner)
                 (push owner releases)
                 (error "injected cleanup failure"))))
      (should-error (nelisp-eln-raw-call-context-create)
                    :type 'nelisp-eln-raw-call-error)
      (should (= (length releases) 2))
      (should (member argv nelisp-eln-raw-call--pending-cleanup))
      (should (member out nelisp-eln-raw-call--pending-cleanup)))))

(ert-deftest nelisp-eln-raw-call/partial-context-release-quarantines-all-use ()
  (let* ((first (vector 'nl-ffi-memory-owner 65536 4096 48))
         (second (vector 'nl-ffi-memory-owner 69632 4096 8))
         (context (vector nelisp-eln-raw-call--context-marker
                          first second nil nil))
         (failed-once nil)
         (nelisp-eln-raw-call--pending-cleanup nil))
    (cl-letf (((symbol-function 'nl-ffi-memory-release)
               (lambda (owner)
                 (if (and (eq owner first) (not failed-once))
                     (progn (setq failed-once t)
                            (error "injected first unmap failure"))
                   t))))
      (should-error (nelisp-eln-raw-call-context-release context)
                    :type 'nelisp-eln-raw-call-error)
      (should (eq (aref context 4) t))
      (should-not (aref context 1))
      (should-not (aref context 2))
      (should-error (nelisp-eln-raw-call-word context 65536 nil)
                    :type 'nelisp-eln-raw-call-error)
      (should (equal nelisp-eln-raw-call--pending-cleanup (list first)))
      (should (nelisp-eln-raw-call-retry-cleanup))
      (should-not nelisp-eln-raw-call--pending-cleanup))))

(ert-deftest nelisp-eln-raw-call/busy-check-precedes-argument-traversal ()
  (let ((context (vector nelisp-eln-raw-call--context-marker
                         'argv-owner 'out-owner t nil)))
    (should-error (nelisp-eln-raw-call-word context 65536 '(1 . 2))
                  :type 'nelisp-eln-raw-call-error)))

(ert-deftest nelisp-eln-raw-call/rejects-untransportable-function-address ()
  (let ((context (vector nelisp-eln-raw-call--context-marker
                         'argv-owner 'out-owner nil nil)))
    (should-error
     (nelisp-eln-raw-call-word
      context (1+ most-positive-fixnum) nil)
     :type 'nelisp-eln-raw-call-error)))

(provide 'nelisp-eln-raw-call-test)

;;; nelisp-eln-raw-call-test.el ends here
