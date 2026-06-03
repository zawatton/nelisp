;;; nelisp-sys-examples-test.el --- Integration tests for the examples gallery -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Stage 132.8: end-to-end gallery integration tests.  Each example in
;; packages/nelisp-sys/examples/ is compiled to a native object, linked
;; with a small C harness, and the harness's exit code is asserted to be
;; the expected computed value.
;;
;; The toolchain tests (nelisp-sys-example-*) skip when the NeLisp backend
;; or a C compiler is unavailable; the analysis-only test
;; (nelisp-sys-examples-all-analyze) always runs and acts as a parse/
;; type-check gate for every .nl file in the gallery.

;;; Code:

(require 'ert)
(require 'nelisp-sys-driver)
(require 'nelisp-sys-adapter-nelisp)

(defconst nelisp-sys-examples-test--test-dir
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "Directory containing this test file (<pkg>/test/).")

(defconst nelisp-sys-examples-test--examples-dir
  (expand-file-name "../examples/" nelisp-sys-examples-test--test-dir)
  "Directory containing the .nl example programs (<pkg>/examples/).")

(defun nelisp-sys-examples-test--read-forms (nl-file)
  "Read all top-level forms from NL-FILE and return them as a list."
  (with-temp-buffer
    (insert-file-contents nl-file)
    (goto-char (point-min))
    (let ((forms nil))
      (condition-case nil
          (while t (push (read (current-buffer)) forms))
        (end-of-file nil))
      (nreverse forms))))

(defun nelisp-sys-examples-test--linux-x86_64-host-p ()
  "Return non-nil when the host can link and run default Linux ELF examples."
  (and (eq system-type 'gnu/linux)
       (string-prefix-p "x86_64" system-configuration)))

;;; Analysis-only gate (no toolchain needed).

(ert-deftest nelisp-sys-examples-all-analyze ()
  "Every .nl in the examples gallery must parse and pass all static checks."
  (let ((nl-files (directory-files nelisp-sys-examples-test--examples-dir
                                   t "\\.nl\\'")))
    (should (> (length nl-files) 0))
    (dolist (f nl-files)
      (let ((forms (nelisp-sys-examples-test--read-forms f)))
        (should (eq t (nelisp-sys-analyze forms)))))))

;;; End-to-end toolchain tests.

(ert-deftest nelisp-sys-example-factorial ()
  "nl_fact(5) returns 120 (5! = 120, fits in exit code 0-255)."
  (skip-unless (and (nelisp-sys-adapter-available-p)
                    (nelisp-sys-examples-test--linux-x86_64-host-p)
                    (executable-find "cc")))
  (let* ((tmp (make-temp-file "nelisp-sys-fact" t))
         (obj (expand-file-name "factorial.o" tmp))
         (cfile (expand-file-name "harness.c" tmp))
         (exe (expand-file-name "harness" tmp))
         (forms (nelisp-sys-examples-test--read-forms
                 (expand-file-name "factorial.nl"
                                   nelisp-sys-examples-test--examples-dir))))
    (unwind-protect
        (progn
          (nelisp-sys-compile-object forms obj)
          (should (file-exists-p obj))
          (with-temp-file cfile
            (insert "long nl_fact(long);\n"
                    "int main(void){return (int)nl_fact(5);}\n"))
          (should (= 0 (call-process "cc" nil nil nil cfile obj "-o" exe)))
          (should (= 120 (call-process exe nil nil nil))))
      (ignore-errors (delete-directory tmp t)))))

(ert-deftest nelisp-sys-example-max3 ()
  "nl_max3(3,9,7) returns 9."
  (skip-unless (and (nelisp-sys-adapter-available-p)
                    (nelisp-sys-examples-test--linux-x86_64-host-p)
                    (executable-find "cc")))
  (let* ((tmp (make-temp-file "nelisp-sys-max3" t))
         (obj (expand-file-name "max3.o" tmp))
         (cfile (expand-file-name "harness.c" tmp))
         (exe (expand-file-name "harness" tmp))
         (forms (nelisp-sys-examples-test--read-forms
                 (expand-file-name "max3.nl"
                                   nelisp-sys-examples-test--examples-dir))))
    (unwind-protect
        (progn
          (nelisp-sys-compile-object forms obj)
          (should (file-exists-p obj))
          (with-temp-file cfile
            (insert "long nl_max3(long,long,long);\n"
                    "int main(void){return (int)nl_max3(3,9,7);}\n"))
          (should (= 0 (call-process "cc" nil nil nil cfile obj "-o" exe)))
          (should (= 9 (call-process exe nil nil nil))))
      (ignore-errors (delete-directory tmp t)))))

(ert-deftest nelisp-sys-example-gcd ()
  "nl_gcd(48,36) returns 12."
  (skip-unless (and (nelisp-sys-adapter-available-p)
                    (nelisp-sys-examples-test--linux-x86_64-host-p)
                    (executable-find "cc")))
  (let* ((tmp (make-temp-file "nelisp-sys-gcd" t))
         (obj (expand-file-name "gcd.o" tmp))
         (cfile (expand-file-name "harness.c" tmp))
         (exe (expand-file-name "harness" tmp))
         (forms (nelisp-sys-examples-test--read-forms
                 (expand-file-name "gcd.nl"
                                   nelisp-sys-examples-test--examples-dir))))
    (unwind-protect
        (progn
          (nelisp-sys-compile-object forms obj)
          (should (file-exists-p obj))
          (with-temp-file cfile
            (insert "long nl_gcd(long,long);\n"
                    "int main(void){return (int)nl_gcd(48,36);}\n"))
          (should (= 0 (call-process "cc" nil nil nil cfile obj "-o" exe)))
          (should (= 12 (call-process exe nil nil nil))))
      (ignore-errors (delete-directory tmp t)))))

(ert-deftest nelisp-sys-example-point ()
  "nl_point_sum({3,4}) returns 7."
  (skip-unless (and (nelisp-sys-adapter-available-p)
                    (nelisp-sys-examples-test--linux-x86_64-host-p)
                    (executable-find "cc")))
  (let* ((tmp (make-temp-file "nelisp-sys-point" t))
         (obj (expand-file-name "point.o" tmp))
         (cfile (expand-file-name "harness.c" tmp))
         (exe (expand-file-name "harness" tmp))
         (forms (nelisp-sys-examples-test--read-forms
                 (expand-file-name "point.nl"
                                   nelisp-sys-examples-test--examples-dir))))
    (unwind-protect
        (progn
          (nelisp-sys-compile-object forms obj)
          (should (file-exists-p obj))
          (with-temp-file cfile
            (insert "struct point{unsigned int x,y;};\n"
                    "long nl_point_sum(struct point*);\n"
                    "int main(void){struct point p={3,4};return (int)nl_point_sum(&p);}\n"))
          (should (= 0 (call-process "cc" nil nil nil cfile obj "-o" exe)))
          (should (= 7 (call-process exe nil nil nil))))
      (ignore-errors (delete-directory tmp t)))))

;;; nelisp-sys-examples-test.el ends here
