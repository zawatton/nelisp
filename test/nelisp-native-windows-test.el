;;; nelisp-native-windows-test.el --- Host Win64 native prerequisites -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'ert)
(require 'bytecomp)
(require 'nelisp-aot-compiler)
(require 'nelisp-bytecode-native-rooted-cfg-contract)
(require 'nelisp-native-load)

(defun nelisp-native-windows-test--contains (text bytes)
  (string-match-p (regexp-quote (apply #'unibyte-string bytes)) text))

(defun nelisp-native-windows-test--bridge (format)
  (nelisp-aot-compile-to-link-unit
   '(defun probe (a b c d e f)
      (extern-call nl_native_funcall_v2 a b c d e f))
   :arch 'x86_64 :format format))

(ert-deftest nelisp-native-windows-six-word-bridge-reference-bytes ()
  "Win64 receives four registers, spills two stack arguments and reserves shadow."
  (let ((text (plist-get (nelisp-native-windows-test--bridge 'coff) :text)))
    (should (equal (substring text 0 4) (unibyte-string #x55 #x48 #x89 #xe5)))
    ;; Incoming word 1 is RCX, not RDI; word 5 is at rbp+48 after push rbp.
    (should (nelisp-native-windows-test--contains text '(#x48 #x89 #x4d #xf8)))
    (should (nelisp-native-windows-test--contains text '(#x48 #x8b #x45 #x30)))
    (should (nelisp-native-windows-test--contains text '(#x48 #x8b #x45 #x38)))
    ;; Six GP words need 32 shadow bytes plus two stack words.
    (should (nelisp-native-windows-test--contains text '(#x48 #x81 #xec #x30 0 0 0)))
    (should (nelisp-native-windows-test--contains text '(#x48 #x81 #xc4 #x30 0 0 0)))
    (should (equal (substring text -5) (unibyte-string #x48 #x89 #xec #x5d #xc3)))))

(ert-deftest nelisp-native-windows-abi-negative-control ()
  "The same reference check must reject a SysV unit and corrupted bytes."
  (let* ((windows (plist-get (nelisp-native-windows-test--bridge 'coff) :text))
         (linux (plist-get (nelisp-native-windows-test--bridge 'elf) :text))
         (reference '(#x48 #x89 #x4d #xf8)))
    (should-not (equal windows linux))
    (should (nelisp-native-windows-test--contains windows reference))
    (should-not (nelisp-native-windows-test--contains linux reference))
    (let ((offset (string-match (regexp-quote (apply #'unibyte-string reference)) windows)))
      (aset windows offset #x90)
      (should-not (nelisp-native-windows-test--contains windows reference)))))

(ert-deftest nelisp-native-windows-f1-canonical-emitter ()
  "Compile the actual rooted F1 form to a pre-writer Win64 unit on Linux."
  (should (equal emacs-version "31.1"))
  (let* ((byte-compile-warnings nil)
         (function (byte-compile '(lambda (x) (f1-user (cons (car x) (cdr x))))))
         (input (nelisp-bytecode-compiler-input-build function))
         (plan (nelisp-bytecode-native-rooted-cfg-plan input))
         (emitted (nelisp-bytecode-native-rooted-cfg-shared-emit-build
                   plan nelisp-bytecode-native-rooted-cfg-contract-shared-entry))
         (source (plist-get emitted :form))
         (unit (nelisp-aot-compile-to-link-unit source :arch 'x86_64 :format 'coff))
         (text (plist-get unit :text))
         (relocs (plist-get unit :relocs)))
    (should (eq (plist-get plan :status) 'complete))
    (should (eq (plist-get emitted :status) 'complete))
    (should-not (plist-get emitted :additional-source))
    (should (= 4 (cl-count "nl_native_funcall_v2" relocs
                          :key (lambda (r) (plist-get r :symbol)) :test #'equal)))
    (should (member "nl_root_pin_slot_v2" (plist-get unit :extern-symbols)))
    (should (nelisp-native-windows-test--contains text '(#x48 #x89 #x4d #xf8)))
    (should (nelisp-native-windows-test--contains text '(#x48 #x81 #xec #x30 0 0 0)))
    (should (equal (substring text -5) (unibyte-string #x48 #x89 #xec #x5d #xc3)))))

(provide 'nelisp-native-windows-test)
;;; nelisp-native-windows-test.el ends here
