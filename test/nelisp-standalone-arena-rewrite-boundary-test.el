;;; nelisp-standalone-arena-rewrite-boundary-test.el --- Public boundaries -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'ert)
(require 'nelisp-standalone-arena-rewrite)

(defconst nelisp-standalone-arena-rewrite-boundary-test--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name)))))
(dolist (directory '("packages/nl-prelude/src" "packages/nl-ns/src"))
  (add-to-list 'load-path
               (expand-file-name directory nelisp-standalone-arena-rewrite-boundary-test--root)))
(require 'nl-ns)

(defun nelisp-standalone-arena-rewrite-boundary-test--entries (&optional builder-forms)
  "Read the genuine module and builder without evaluating build orchestration."
  (mapcar
   (lambda (path)
     (cons path
           (if (and builder-forms (equal path "scripts/nelisp-standalone-build.el"))
               builder-forms
             (nl-ns-read-file
              (expand-file-name path nelisp-standalone-arena-rewrite-boundary-test--root)))))
   '("lisp/nelisp-standalone-arena-rewrite.el" "scripts/nelisp-standalone-build.el")))

(defun nelisp-standalone-arena-rewrite-boundary-test--declare ()
  "Use the actual inventory declarations without running its baseline gate."
  (let* ((forms (nl-ns-read-file
                 (expand-file-name "tools/nl-ns-inventory.el"
                                   nelisp-standalone-arena-rewrite-boundary-test--root)))
         (definition (cl-find-if
                      (lambda (form)
                        (and (eq (car-safe form) 'defconst)
                             (eq (cadr form) 'nl-ns-inventory--namespace-overrides))) forms)))
    (should definition)
    (dolist (pair (eval (nth 2 definition)))
      (nl-ns-declare (car pair) (cdr pair)))))

(ert-deftest nelisp-standalone-arena-rewrite-boundary/public-builder-ownership ()
  (let ((nl-ns--declared (make-hash-table :test 'equal)))
    (nelisp-standalone-arena-rewrite-boundary-test--declare)
    (let ((findings (nl-ns-check
                     (nl-ns-analyse
                      (nelisp-standalone-arena-rewrite-boundary-test--entries)))))
      (should-not (nl-ns-findings-of-kind findings 'ns-private-escape))
      (should-not (nl-ns-findings-of-kind findings 'ns-prefix-violation))
      (should-not (nl-ns-findings-of-kind findings 'ns-unreadable)))))

(ert-deftest nelisp-standalone-arena-rewrite-boundary/declaration-retains-private-refusal ()
  (let ((nl-ns--declared (make-hash-table :test 'equal)))
    (nelisp-standalone-arena-rewrite-boundary-test--declare)
    (let* ((client '("test-illegal-arena-client.el"
                     (defun test-illegal-arena-client ()
                       (nelisp-standalone--chunk-arena-rewrite nil))))
           (findings (nl-ns-check
                      (nl-ns-analyse
                       (cons client (nelisp-standalone-arena-rewrite-boundary-test--entries)))))
           (private (cl-remove-if-not
                     (lambda (finding)
                       (equal (plist-get finding :file) "test-illegal-arena-client.el"))
                     (nl-ns-findings-of-kind findings 'ns-private-escape))))
      (should (= (length private) 1))
      (should (eq (plist-get (car private) :subject)
                  'nelisp-standalone--chunk-arena-rewrite))
      (should (equal (plist-get (car private) :owner)
                     "lisp/nelisp-standalone-arena-rewrite.el")))))

(ert-deftest nelisp-standalone-arena-rewrite-boundary/dynamic-configuration-not-cached ()
  ;; Independently specified nondefault bases/span expose frozen accessor bugs.
  (let ((nelisp-standalone--target 'windows-aarch64)
        (nelisp-standalone--arena-base #x20000000)
        (nelisp-standalone--windows-arena-base #x71000000)
        (nelisp-standalone--arena-rebase-span 16))
    (should (eq (nelisp-standalone-arena-rewrite-target) 'windows-aarch64))
    (should (= (nelisp-standalone-arena-rewrite-base) #x20000000))
    (should (= (nelisp-standalone-arena-rewrite-windows-base) #x71000000))
    (should (= (nelisp-standalone-arena-rewrite-span) 16))
    (should (= (nelisp-standalone-arena-rewrite-target-base) #x71000000))
    (should
     (equal (nelisp-standalone-arena-rewrite-source '(seq #x71000000 #x7100000f #x71000010))
            '(seq (+ (ptr-read-u64 (data-addr nl_arena_base) 0) 0)
                  (+ (ptr-read-u64 (data-addr nl_arena_base) 0) 15) #x71000010)))
    (let ((nelisp-standalone--target 'linux-aarch64))
      (should (= (nelisp-standalone-arena-rewrite-target-base) #x20000000)))
    (should (= (nelisp-standalone-arena-rewrite-target-base) #x71000000))))

(provide 'nelisp-standalone-arena-rewrite-boundary-test)
