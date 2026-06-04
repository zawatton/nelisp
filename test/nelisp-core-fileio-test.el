;;; nelisp-core-fileio-test.el --- ERT for core loader file I/O  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'nelisp-core-fileio)

(ert-deftest nelisp-core-fileio/file-name-absolute-p ()
  (should (nelisp-core-file-name-absolute-p "/a"))
  (should-not (nelisp-core-file-name-absolute-p "rel"))
  (should (nelisp-core-file-name-absolute-p "~"))
  (should (nelisp-core-file-name-absolute-p "~/x"))
  (should-not (nelisp-core-file-name-absolute-p ""))
  (should-error (nelisp-core-file-name-absolute-p 1)
                :type 'wrong-type-argument))

(ert-deftest nelisp-core-fileio/file-name-directory ()
  (should (equal (nelisp-core-file-name-directory "/a/b/c.el")
                 "/a/b/"))
  (should-not (nelisp-core-file-name-directory "noslash"))
  (should-error (nelisp-core-file-name-directory nil)
                :type 'wrong-type-argument))

(ert-deftest nelisp-core-fileio/file-name-as-directory ()
  (should (equal (nelisp-core-file-name-as-directory "/tmp/example")
                 "/tmp/example/"))
  (should (equal (nelisp-core-file-name-as-directory "/tmp/example/")
                 "/tmp/example/"))
  (should (equal (nelisp-core-file-name-as-directory "")
                 "/"))
  (should-error (nelisp-core-file-name-as-directory '(1 2 3))
                :type 'wrong-type-argument))

(ert-deftest nelisp-core-fileio/expand-file-name ()
  (should (equal (nelisp-core-expand-file-name "/a/./b//c/../d")
                 "/a/b/d"))
  (should (equal (nelisp-core-expand-file-name "subdir/file.el" "/tmp/base")
                 "/tmp/base/subdir/file.el"))
  (should (equal (nelisp-core-expand-file-name "../sibling/file.el"
                                               "/tmp/base/dir")
                 "/tmp/base/sibling/file.el"))
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "HOME" "/tmp/nelisp-home")
    (should (equal (nelisp-core-expand-file-name "~")
                   "/tmp/nelisp-home"))
    (should (equal (nelisp-core-expand-file-name "~/x")
                   "/tmp/nelisp-home/x")))
  (should (string-prefix-p "/"
                           (nelisp-core-expand-file-name "relative/path"
                                                         "/tmp/base")))
  (should-error (nelisp-core-expand-file-name :name "/tmp/base")
                :type 'wrong-type-argument))

(ert-deftest nelisp-core-fileio/file-exists-p-and-file-readable-p ()
  (let* ((temp-dir (make-temp-file "nelisp-core-fileio-probe-" t))
         (file-path (make-temp-file (expand-file-name "probe-" temp-dir)
                                    nil
                                    ".txt"))
         (missing-path (expand-file-name "missing.txt" temp-dir)))
    (unwind-protect
        (progn
          (write-region "probe\n" nil file-path nil 'silent)
          (should (nelisp-core-file-exists-p file-path))
          (should (nelisp-core-file-readable-p file-path))
          (should-not (nelisp-core-file-exists-p missing-path))
          (should-not (nelisp-core-file-readable-p missing-path)))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest nelisp-core-fileio/read-file-as-string ()
  (let* ((temp-dir (make-temp-file "nelisp-core-fileio-read-" t))
         (file-path (expand-file-name "sample.txt" temp-dir))
         (missing-path (expand-file-name "missing.txt" temp-dir))
         (expected "ASCII line\nあいう\nSecond ASCII line\n"))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region expected nil file-path nil 'silent))
          (should (string-equal (nelisp-core-read-file-as-string file-path)
                                expected))
          (should-error (nelisp-core-read-file-as-string missing-path)
                        :type 'file-error))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(provide 'nelisp-core-fileio-test)

;;; nelisp-core-fileio-test.el ends here
