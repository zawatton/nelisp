;;; emacs-load-elc-test.el --- GNU ELC runtime loader tests -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'bytecomp)
(require 'emacs-load-test)

(defvar emacs-load-elc-test--value nil)

(ert-deftest emacs-load-elc-test/compiled-loader-preserves-bytecode-and-context ()
  (let* ((directory (make-temp-file "emacs-load-elc-test-" t))
         (source (expand-file-name "sample.el" directory))
         (compiled (concat source "c"))
         (load-file-name "enclosing.el")
         (load-true-file-name "enclosing-real.el")
         (inhibit-file-name-operation 'outer-operation)
         (load-in-progress nil)
         (emacs-load-auto-native-compile t))
    (unwind-protect
        (progn
          (with-temp-file source
            (insert ";;; -*- lexical-binding: t; -*-\n"
                    "(defvar emacs-load-elc-test--value nil)\n"
                    "(setq emacs-load-elc-test--value (list load-file-name load-true-file-name load-in-progress inhibit-file-name-operation 42))\n"
                    "(defun emacs-load-elc-test--plus (n) \"Add two.\" (+ n 2))\n"))
          (byte-compile-file source)
          (delete-file source)
          (should (nelisp--load-resolved-file compiled nil))
          (should (byte-code-function-p
                   (symbol-function 'emacs-load-elc-test--plus)))
          (should (= (emacs-load-elc-test--plus 5) 7))
          (should (equal emacs-load-elc-test--value (list compiled compiled t nil 42)))
          (should (equal load-file-name "enclosing.el"))
          (should-not load-in-progress)
          (should (equal load-true-file-name "enclosing-real.el"))
          (should (eq inhibit-file-name-operation 'outer-operation)))
      (fmakunbound 'emacs-load-elc-test--plus)
      (delete-directory directory t))))

(ert-deftest emacs-load-elc-test/invalid-header-errors-before-effects-even-noerror ()
  (let ((path (make-temp-file "emacs-load-elc-test-" nil ".elc"))
        (emacs-load-elc-test--value nil))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "(setq emacs-load-elc-test--value 'bad)\n"))
          (let ((condition (should-error
                            (nelisp--load-resolved-file path t))))
            (should (equal condition
                           (list 'error
                                 (format-message "File `%s' was not compiled in Emacs" path)))))
          (should-not emacs-load-elc-test--value))
      (delete-file path))))

(ert-deftest emacs-load-elc-test/header-error-quotes-match-gnu ()
  (let ((path (make-temp-file "emacs-load-elc-test-quote'`-" nil ".elc")))
    (unwind-protect
        (progn
          (with-temp-file path (insert "; incompatible header\n"))
          (dolist (style '(nil grave straight curve unexpected))
            (let* ((text-quoting-style style)
                   (expected (condition-case err (load path nil t t) (error err)))
                   (actual (condition-case err
                               (nelisp--load-resolved-file path t)
                             (error err))))
              (should (equal actual expected)))))
      (delete-file path))))

(ert-deftest emacs-load-elc-test/reader-errors-match-gnu-file-context ()
  (let ((path (make-temp-file "emacs-load-elc-test-error-" nil ".elc"))
        (coding-system-for-write 'no-conversion))
    (unwind-protect
        (dolist (body '("#@4 ab\37" "(setq elc-incomplete 1"
                        "#[0 1 [] 1]" "#[0 \"x\" []]" ")"))
          (with-temp-file path
            (insert (concat ";ELC" (string 31)
                            "\n;;; in Emacs version 31.1\n" body)))
          (let ((expected (condition-case err (load path nil t t) (error err)))
                (actual (condition-case err
                            (nelisp--load-resolved-file path t)
                          (error err))))
            (should (equal actual expected))))
      (delete-file path))))

(ert-deftest emacs-load-elc-test/source-marker-does-not-imply-compiled ()
  (let ((path (make-temp-file "emacs-load-elc-test-marker-" nil ".el"))
        (emacs-load-elc-test--value nil)
        (emacs-load-auto-native-compile nil))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert ";ELC ordinary source comment\n"
                    "(setq emacs-load-elc-test--value 17)\n"))
          (should (load path nil t t))
          (should (= emacs-load-elc-test--value 17))
          (setq emacs-load-elc-test--value nil)
          (should (nelisp--load-resolved-file path nil))
          (should (= emacs-load-elc-test--value 17)))
      (delete-file path))))

(ert-deftest emacs-load-elc-test/compiler-comment-classifies-other-extensions ()
  (let ((path (make-temp-file "emacs-load-elc-test-compiled-" nil ".el"))
        (coding-system-for-write 'no-conversion))
    (unwind-protect
        (progn
          ;; GNU uses the compiler comment and byte 4, without requiring
          ;; the first four bytes to be the conventional ;ELC marker.
          (with-temp-file path
            (insert (concat ";ALT" (string 31) "\n;;; in Emacs version 31.1\n"
                            "(defalias 'emacs-load-elc-test--constant "
                            "#[0 \"\\300\\207\" [29] 1])\n")))
          (should (load path nil t t))
          (should (= (emacs-load-elc-test--constant) 29))
          (fmakunbound 'emacs-load-elc-test--constant)
          (should (nelisp--load-resolved-file path nil))
          (should (byte-code-function-p
                   (symbol-function 'emacs-load-elc-test--constant)))
          (should (= (emacs-load-elc-test--constant) 29)))
      (fmakunbound 'emacs-load-elc-test--constant)
      (delete-file path))))

(ert-deftest emacs-load-elc-test/header-matches-gnu-safety-rules ()
  (should (= (emacs-load--elc-version "") 1))
  (should (= (emacs-load--elc-version "\n") 0))
  (should (= (emacs-load--elc-version
              (concat ";ELC" (string 31) "\0\0\0\n"
                      ";;; Compiled\n;;; in Emacs version 31.1\n")) 31))
  ;; GNU's safety check validates the compiler comment, not a numeric cap.
  (should (= (emacs-load--elc-version
              (concat ";ELC" (string 99) "\n;;; in Emacs version 99.1\n")) 99))
  (should (= (emacs-load--elc-version
              (concat ";ELC" (string 31) "\n;;; not Emacs\n")) 0))
  (should (= (emacs-load--elc-version
              (concat ";ELC" (string 31) "\n" (make-string 512 ?x)
                      "\n;;; in Emacs version 31.1\n")) 0)))

(ert-deftest emacs-load-elc-test/resolver-suffixes-newer-and-directory-priority ()
  (let* ((directory (make-temp-file "emacs-load-elc-test-resolve-" t))
         (first (expand-file-name "first/" directory))
         (second (expand-file-name "second/" directory))
         (load-suffixes '(".elc" ".el"))
         (load-file-rep-suffixes '(""))
         (load-path (list first second))
         (load-prefer-newer nil))
    (unwind-protect
        (progn
          (make-directory first)
          (make-directory second)
          (dolist (path (list (concat first "sample.elc")
                              (concat first "sample.el")
                              (concat second "sample.elc")))
            (with-temp-file path (insert "; fixture\n")))
          (set-file-times (concat first "sample.elc") '(0 100))
          (set-file-times (concat first "sample.el") '(0 200))
          (set-file-times (concat second "sample.elc") '(0 300))
          (should (equal (emacs-load--resolve-file "sample" nil nil)
                         (concat first "sample.elc")))
          (let ((load-prefer-newer t))
            (should (equal (emacs-load--resolve-file "sample" nil nil)
                           (concat first "sample.el")))
            ;; GNU locate-library keeps suffix order even when load changes.
            (should (equal (emacs-load--locate-library "sample")
                           (concat first "sample.elc"))))
          (should (equal (emacs-load--locate-library "sample" nil (list second))
                         (concat second "sample.elc")))
          (should-not (emacs-load--locate-library "sample" t))
          (should (equal (emacs-load--locate-library "sample.elc" t)
                         (concat first "sample.elc")))
          (let ((load-suffixes '(".el" ".elc")))
            (should (equal (emacs-load--resolve-file "sample" nil nil)
                           (concat first "sample.el"))))
          (set-file-times (concat first "sample.elc") '(0 200))
          (let ((load-prefer-newer t))
            (should (equal (emacs-load--resolve-file "sample" nil nil)
                           (concat first "sample.elc")))))
      (delete-directory directory t))))

(ert-deftest emacs-load-elc-test/newer-file-compares-native-nanoseconds ()
  (dolist (case '((200 100 200 200 nil)
                  (200 200 200 100 t)
                  (200 100 200 100 nil)
                  (201 0 200 999999999 t)
                  (199 999999999 200 0 nil)))
    (cl-letf (((symbol-function 'nelisp--syscall-stat-field)
               (lambda (path offset)
                 (nth (if (equal path "candidate")
                          (if (= offset 88) 0 1)
                        (if (= offset 88) 2 3))
                      case)))
              ((symbol-function 'file-newer-than-file-p)
               (lambda (&rest _arguments)
                 (error "optional foundation comparator was called"))))
      (should (equal (and (emacs-load--newer-file-p "candidate" "previous") t)
                     (nth 4 case))))))

(ert-deftest emacs-load-elc-test/resolver-preserves-subsecond-order ()
  (let* ((directory (make-temp-file "emacs-load-elc-test-nanoseconds-" t))
         (compiled (expand-file-name "sample.elc" directory))
         (source (expand-file-name "sample.el" directory))
         (load-path (list directory))
         (load-suffixes '(".elc" ".el"))
         (load-file-rep-suffixes '(""))
         (load-prefer-newer t))
    (unwind-protect
        (progn
          (with-temp-file compiled (insert "; fixture\n"))
          (with-temp-file source (insert "; fixture\n"))
          (set-file-times compiled '(0 200 0 100000))
          (set-file-times source '(0 200 0 200000))
          (cl-letf (((symbol-function 'file-newer-than-file-p)
                     (lambda (&rest _arguments)
                       (error "optional foundation comparator was called"))))
            (should (equal (emacs-load--resolve-file "sample" nil nil) source)))
          (set-file-times compiled '(0 200 0 200000))
          (should (equal (emacs-load--resolve-file "sample" nil nil) compiled)))
      (delete-directory directory t))))

(ert-deftest emacs-load-elc-test/large-bounded-native-read-preserves-literals ()
  (let* ((source (concat "(when t "
                         "(setq emacs-load-elc-test--value "
                         "(list \"cc-provide\" 'cc-provide)) "
                         "(cc-provide 'emacs-load-elc-test-bounded))"))
         (emacs-load--native-read-probe-window-sizes '(16))
         (emacs-load-elc-test--value nil)
         (calls 0))
    (unwind-protect
        (cl-letf (((symbol-function 'nelisp--read-all-from-string-native)
                   (lambda (text &optional start end)
                     (setq calls (1+ calls))
                     (read-from-string text start end)))
                  ((symbol-function 'nelisp--load-eval-source-tail)
                   (lambda (&rest _arguments)
                     (error "bounded native form took the text fallback"))))
          (let ((result (nelisp--load-eval-source-declined-form
                         source 0 (length source))))
            (should (= (cdr result) (length source)))
            (should (eq (car result) 'emacs-load-elc-test-bounded)))
          (should (= calls 1))
          (should (equal emacs-load-elc-test--value '("cc-provide" cc-provide))))
      (setq features (delq 'emacs-load-elc-test-bounded features)))))

(ert-deftest emacs-load-elc-test/literal-span-replacement ()
  (dolist (case '(("a" "b" "" "")
                  ("a" "b" "ccc" "ccc")
                  ("a" "b" "aaa" "bbb")
                  ("aa" "x" "aaa" "xa")
                  (".*" "$" "a.*b.*" "a$b$")
                  ("cc-provide" "provide" "日本 cc-provide tail"
                   "日本 provide tail")))
    (should (equal (emacs-load--replace-source-literal
                    (nth 0 case) (nth 1 case) (nth 2 case))
                   (nth 3 case)))
    (should (equal (emacs-load--replace-source-literal
                    (nth 0 case) (nth 1 case) (nth 2 case))
                   (string-replace (nth 0 case) (nth 1 case) (nth 2 case)))))
  (let ((source "untouched"))
    (should (eq (emacs-load--replace-source-literal "absent" "x" source)
                source)))
  (should-error (emacs-load--replace-source-literal "" "x" "source")
                :type 'wrong-length-argument))

(provide 'emacs-load-elc-test)
;;; emacs-load-elc-test.el ends here
