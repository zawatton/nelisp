;;; nelisp-sys-multitarget-test.el --- ERT tests for multi-target + static lib -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Stage 132.5 (multi-target ABI snapshots / object-level emission) and
;; 132.6 (hosted static-library packaging + C-harness link).  Object-level
;; and link tests run only when the toolchain / cc / ar are available.

;;; Code:

(require 'ert)
(require 'nelisp-sys-frontend)
(require 'nelisp-sys-driver)
(require 'nelisp-sys-backend)
(require 'nelisp-sys-abi-meta)

(defconst nelisp-sys-mt-test--add
  '((sys:defun add ((a i32) (b i32)) i32
      (:abi c :export "nl_add" :alloc none) (+ a b)))
  "A minimal exported C-ABI integer function used across targets.")

(defun nelisp-sys-mt-test--linux-x86_64-host-p ()
  "Return non-nil when the host can link and run the Linux static library."
  (and (eq system-type 'gnu/linux)
       (string-prefix-p "x86_64" system-configuration)))

;;; 132.5 — per-target ABI metadata snapshot (no toolchain).

(ert-deftest nelisp-sys-mt-abi-metadata-all-targets ()
  (let ((module (nelisp-sys-frontend-parse-module nelisp-sys-mt-test--add)))
    (dolist (case '(("x86_64-unknown-linux-gnu"  elf64    sysv-amd64)
                    ("aarch64-unknown-linux-gnu" elf64    aapcs64)
                    ("x86_64-apple-darwin"       mach-o64 sysv-amd64)
                    ("aarch64-apple-darwin"      mach-o64 aapcs64)
                    ("x86_64-pc-windows-msvc"    pe-coff  win64)))
      (let ((meta (nelisp-sys-abi-meta-module module (nth 0 case))))
        (should (string= (nth 0 case) (plist-get meta :target)))
        (should (eq (nth 1 case) (plist-get meta :object)))
        (should (eq (nth 2 case) (plist-get meta :c-abi)))
        (should (equal '(("nl_add" :args (i32 i32) :ret i32))
                       (plist-get meta :exports)))))))

;;; 132.5 — object-level emission for every target (needs toolchain).

(defun nelisp-sys-mt-test--head (path n)
  "Return the first N bytes of PATH as a list of integers."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path nil 0 n)
    (append (buffer-string) nil)))

(ert-deftest nelisp-sys-mt-object-level-all-targets ()
  (skip-unless (nelisp-sys-adapter-available-p))
  (let ((dir (make-temp-file "nelisp-sys-mt" t)))
    (unwind-protect
        (dolist (case '(("x86_64-unknown-linux-gnu"  elf   62)
                        ("aarch64-unknown-linux-gnu" elf   183)
                        ("x86_64-apple-darwin"       macho nil)
                        ("aarch64-apple-darwin"      macho nil)
                        ("x86_64-pc-windows-msvc"    coff  nil)))
          (let* ((triple (nth 0 case))
                 (kind (nth 1 case))
                 (machine (nth 2 case))
                 (obj (expand-file-name (concat triple ".o") dir)))
            (nelisp-sys-compile-object nelisp-sys-mt-test--add obj triple)
            (should (file-exists-p obj))
            (let ((h (nelisp-sys-mt-test--head obj 20)))
              (cl-case kind
                (elf
                 (should (equal '(127 69 76 70) (seq-take h 4)))   ; \x7fELF
                 (should (= machine (nth 18 h))))                  ; e_machine LE lo
                (macho
                 (should (equal '(207 250 237 254) (seq-take h 4)))) ; cf fa ed fe
                (coff
                 (should (equal '(100 134) (seq-take h 2))))))))    ; 0x8664 LE
      (ignore-errors (delete-directory dir t)))))

;;; 132.6 — static library packaging + C-harness link (needs toolchain+cc+ar).

(ert-deftest nelisp-sys-mt-static-lib-link-from-c ()
  (skip-unless (and (nelisp-sys-adapter-available-p)
                    (nelisp-sys-mt-test--linux-x86_64-host-p)
                    (executable-find "cc")
                    (executable-find "ar")))
  (let* ((dir (make-temp-file "nelisp-sys-lib" t))
         (lib (expand-file-name "libnl.a" dir))
         (cfile (expand-file-name "main.c" dir))
         (exe (expand-file-name "main" dir)))
    (unwind-protect
        (progn
          (nelisp-sys-compile-static-lib nelisp-sys-mt-test--add lib
                                         "x86_64-unknown-linux-gnu")
          (should (file-exists-p lib))
          (with-temp-file cfile
            (insert "long nl_add(long,long);\n"
                    "int main(void){return (int)nl_add(15,27);}\n"))
          (should (= 0 (call-process "cc" nil nil nil cfile lib "-o" exe)))
          (should (= 42 (call-process exe nil nil nil))))
      (ignore-errors (delete-directory dir t)))))

;;; nelisp-sys-multitarget-test.el ends here
