;;; nelisp-sys-cli-test.el --- ERT tests for the nelisp-sys CLI wrapper -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Doc 130 criterion 14: validates the nelisp-sys bash CLI wrapper
;; (packages/nelisp-sys/bin/nelisp-sys) against the build-flow contract.
;; Tests use call-process so they exercise the real script in a subprocess.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Compute paths from this file's location.
;; test/ is at <repo>/packages/nelisp-sys/test, so repo = two dirs up.
(defconst nelisp-sys-cli-test--test-dir
  (file-name-directory (or load-file-name (buffer-file-name))))

(defconst nelisp-sys-cli-test--repo-root
  (expand-file-name "../../.." nelisp-sys-cli-test--test-dir))

(defconst nelisp-sys-cli-test--script
  (expand-file-name "packages/nelisp-sys/bin/nelisp-sys"
                    nelisp-sys-cli-test--repo-root))

(defconst nelisp-sys-cli-test--add-source
  "(sys:defun add ((a i32) (b i32)) i32\n  (:abi c :export \"nl_add\" :alloc none) (+ a b))\n")

(defconst nelisp-sys-cli-test--bad-source
  ;; unknown variable x — analyze must reject this
  "(sys:defun f () i32 () x)\n")

;;; Tests.

(ert-deftest nelisp-sys-cli-analyze-ok ()
  "analyze on a valid .nl file exits 0."
  (let ((tmp (make-temp-file "nelisp-sys-cli-ok" nil ".nl")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert nelisp-sys-cli-test--add-source))
          (should (= 0 (call-process nelisp-sys-cli-test--script nil nil nil
                                     "analyze" tmp))))
      (ignore-errors (delete-file tmp)))))

(ert-deftest nelisp-sys-cli-analyze-rejects ()
  "analyze on an invalid .nl file exits nonzero."
  (let ((tmp (make-temp-file "nelisp-sys-cli-bad" nil ".nl")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert nelisp-sys-cli-test--bad-source))
          (should (not (= 0 (call-process nelisp-sys-cli-test--script nil nil nil
                                          "analyze" tmp)))))
      (ignore-errors (delete-file tmp)))))

(ert-deftest nelisp-sys-cli-compile-object ()
  "compile produces OUT.o and OUT.o.abi; the object links and runs returning 42."
  (skip-unless (executable-find "cc"))
  (let* ((tmpdir (make-temp-file "nelisp-sys-cli-compile" t))
         (src    (expand-file-name "add.nl" tmpdir))
         (obj    (expand-file-name "add.o"  tmpdir))
         (cfile  (expand-file-name "harness.c" tmpdir))
         (exe    (expand-file-name "harness" tmpdir)))
    (unwind-protect
        (progn
          (with-temp-file src
            (insert nelisp-sys-cli-test--add-source))
          ;; compile via the CLI wrapper
          (should (= 0 (call-process nelisp-sys-cli-test--script nil nil nil
                                     "compile" src "-o" obj)))
          ;; object and ABI sidecar must exist
          (should (file-exists-p obj))
          (should (file-exists-p (concat obj ".abi")))
          ;; link a C harness and confirm nl_add(40,2) returns 42
          (with-temp-file cfile
            (insert "long nl_add(long,long);\n"
                    "int main(void){return (int)nl_add(40,2);}\n"))
          (should (= 0 (call-process "cc" nil nil nil cfile obj "-o" exe)))
          (should (= 42 (call-process exe nil nil nil))))
      (ignore-errors (delete-directory tmpdir t)))))

(provide 'nelisp-sys-cli-test)

;;; nelisp-sys-cli-test.el ends here
