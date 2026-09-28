;;; nelisp-eln-abi-test.el --- tests for GNU .eln word ABI -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'nelisp-eln-abi)

(ert-deftest nelisp-eln-abi/gnu-fixnum-round-trip-boundaries ()
  (dolist (value (list nelisp-eln-abi-fixnum-min -1 0 17
                       nelisp-eln-abi-fixnum-max))
    (should (= (nelisp-eln-abi-decode-fixnum
                (nelisp-eln-abi-encode-fixnum value))
               value)))
  ;; GNU lisp.h: make_fixnum(n) = (n << INTTYPEBITS) + Lisp_Int0;
  ;; INTTYPEBITS=2 and Lisp_Int0=2 for the measured LSB-tag build.
  (should (= (nelisp-eln-abi-encode-fixnum nelisp-eln-abi-fixnum-min)
             9223372036854775810))
  (should (= (nelisp-eln-abi-encode-fixnum -1) #xfffffffffffffffe))
  (should (= (nelisp-eln-abi-encode-fixnum 17) 70))
  (should (= (nelisp-eln-abi-encode-fixnum nelisp-eln-abi-fixnum-max)
             9223372036854775806)))

(ert-deftest nelisp-eln-abi/nil-is-distinct-from-fixnum-zero ()
  (should (= (nelisp-eln-abi-encode-nil) 0))
  (should (= (nelisp-eln-abi-encode-fixnum 0) 2))
  (should (eq (nelisp-eln-abi-classify-word 0) 'nil))
  (should (eq (nelisp-eln-abi-classify-word 2) 'fixnum))
  (should-not (nelisp-eln-abi-decode-immediate 0))
  (should (= (nelisp-eln-abi-decode-immediate 2) 0)))

(ert-deftest nelisp-eln-abi/signed-and-unsigned-word-normalization ()
  (should (= (nelisp-eln-abi-normalize-word -1)
             nelisp-eln-abi-word-mask))
  (should (= (nelisp-eln-abi-normalize-word
              nelisp-eln-abi-signed-word-min)
             (ash 1 63)))
  (should (= (nelisp-eln-abi-normalize-word
              nelisp-eln-abi-word-mask)
             nelisp-eln-abi-word-mask))
  (should (= (nelisp-eln-abi-normalize-word 9223372036854775810)
             9223372036854775810))
  (should-error (nelisp-eln-abi-normalize-word
                 (1+ nelisp-eln-abi-word-mask))
                :type 'nelisp-eln-abi-error)
  (should-error (nelisp-eln-abi-normalize-word
                 (1- nelisp-eln-abi-signed-word-min))
                :type 'nelisp-eln-abi-error))

(ert-deftest nelisp-eln-abi/pointer-tags-classify-but-do-not-decode ()
  (dolist (case '((#x1000 . symbol) (#x1003 . cons) (#x1004 . string)
                  (#x1005 . vectorlike) (#x1007 . float)))
    (should (eq (nelisp-eln-abi-classify-word (car case)) (cdr case)))
    (should-error (nelisp-eln-abi-decode-immediate (car case))
                  :type 'nelisp-eln-abi-unsupported-object))
  (should (eq (nelisp-eln-abi-classify-word 1) 'unused))
  (should-error (nelisp-eln-abi-decode-fixnum 0)
                :type 'nelisp-eln-abi-error)
  (should-error (nelisp-eln-abi-encode-fixnum
                 (1+ nelisp-eln-abi-fixnum-max))
                :type 'nelisp-eln-abi-error)
  (should-error (nelisp-eln-abi-encode-fixnum
                 (1- nelisp-eln-abi-fixnum-min))
                :type 'nelisp-eln-abi-error))

(ert-deftest nelisp-eln-abi/profile-match-is-not-runtime-compatibility ()
  (should (nelisp-eln-abi-producer-profile-matches-p
           nelisp-eln-abi-gnu-31-1-x86_64))
  (should (eq (plist-get nelisp-eln-abi-gnu-31-1-x86_64
                         :runtime-compatibility)
              'unsupported))
  (dolist (change '((:producer-version . "31.2")
                    (:producer-abi-hash . "00000000")
                    (:elf-class . 32) (:byte-order . big)
                    (:machine . aarch64) (:word-bits . 32)
                    (:gctypebits . 2) (:use-lsb-tag . nil)))
    (let ((metadata (copy-sequence nelisp-eln-abi-gnu-31-1-x86_64)))
      (setq metadata (plist-put metadata (car change) (cdr change)))
      (should-not (nelisp-eln-abi-producer-profile-matches-p metadata)))))

(provide 'nelisp-eln-abi-test)

;;; nelisp-eln-abi-test.el ends here
