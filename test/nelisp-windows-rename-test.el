;;; nelisp-windows-rename-test.el --- Windows rename boundary controls -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Host ERT checks of the actual emitted syscall adapter and import metadata.
;; Only its selected function definitions are evaluated.  The Win32 calls and
;; UTF-8-to-UTF-16 converter are mocked; these tests do not execute a Windows
;; binary or establish that Wine or the real filesystem rename succeeded.
;; GNU fileio.c Frename_file calls emacs_rename (Emacs 31.1 fileio.c:2803);
;; the Windows sys_rename entry requests replacement (w32.c:5001-5003).

;;; Code:

(require 'ert)
(require 'cl-lib)

(let ((root (expand-file-name ".." (file-name-directory load-file-name))))
  (dolist (directory '("lisp" "src" "scripts"))
    (add-to-list 'load-path (expand-file-name directory root))))
(require 'nelisp-standalone-build)

(defun nelisp-windows-rename-test--forms ()
  "Generate the production Windows syscall translation forms."
  (let ((nelisp-standalone--target 'windows-x86_64))
    (nelisp-standalone--os-syscall-xlat-forms)))

(defun nelisp-windows-rename-test--host-form (form)
  "Translate the emitted FORM's numeric conditions and foreign-call syntax."
  (if (atom form) form
    (pcase (car form)
      ('if `(if (let ((value ,(nelisp-windows-rename-test--host-form (nth 1 form))))
                  (if (numberp value) (/= value 0) value))
                ,(nelisp-windows-rename-test--host-form (nth 2 form))
              ,(nelisp-windows-rename-test--host-form (nth 3 form))))
      ('extern-call `(nelisp-windows-rename-test--extern ',(nth 1 form)
                     ,@(mapcar #'nelisp-windows-rename-test--host-form (nthcdr 2 form))))
      ('seq `(progn ,@(mapcar #'nelisp-windows-rename-test--host-form (cdr form))))
      (_ (mapcar #'nelisp-windows-rename-test--host-form form)))))

(defun nelisp-windows-rename-test--function (name)
  "Return the host-interpretable production adapter function NAME."
  (let ((definition
         (cl-find-if (lambda (form)
                       (and (consp form) (eq (car form) 'defun)
                            (eq (nth 1 form) name)))
                     (nelisp-windows-rename-test--forms))))
    (unless definition (error "Missing emitted function %s" name))
    (eval `(lambda ,(nth 2 definition)
             ,@(mapcar #'nelisp-windows-rename-test--host-form (nthcdr 3 definition)))
          t)))

(defun nelisp-windows-rename-test--extern (&rest arguments)
  "Reject an unmocked foreign call with ARGUMENTS."
  (error "Unmocked Windows rename foreign call: %S" arguments))

(ert-deftest nelisp-windows-rename-converts-both-paths-and-replaces ()
  "Rename dispatches converted source/destination and REPLACE_EXISTING."
  (let ((rename (nelisp-windows-rename-test--function 'nl_os_syscall_path2))
        events)
    (cl-letf (((symbol-function 'nl_win_utf8_wcs_dup)
               (lambda (path)
                 (push (list 'utf16 path) events)
                 (pcase path (101 1001) (202 2002)
                        (_ (ert-fail (format "Unexpected input path %S" path))))))
              ((symbol-function 'nl_os_win_errno)
               (lambda () (ert-fail "Successful rename queried GetLastError")))
              ((symbol-function 'nelisp-windows-rename-test--extern)
               (lambda (name &rest arguments)
                 (push (cons name arguments) events)
                 7)))
      (should (= (funcall rename 82 101 202) 0))
      (should (equal (nreverse events)
                     '((utf16 101) (utf16 202) (MoveFileExW 1001 2002 1)))))))

(ert-deftest nelisp-windows-rename-failure-preserves-win32-errno-mapping ()
  "A failed MoveFileExW delegates to the existing Windows errno adapter."
  (let ((rename (nelisp-windows-rename-test--function 'nl_os_syscall_path2))
        (errno (nelisp-windows-rename-test--function 'nl_os_win_errno)))
    (dolist (case '((183 . -17) (80 . -17) (17 . -18) (2 . -2) (3 . -2)
                    (145 . -39) (5 . -13) (12345 . -13)))
      (let (events)
        (cl-letf (((symbol-function 'nl_win_utf8_wcs_dup)
                   (lambda (path) (+ path 1000)))
                  ((symbol-function 'nl_os_win_errno) errno)
                  ((symbol-function 'nelisp-windows-rename-test--extern)
                   (lambda (name &rest arguments)
                     (push (cons name arguments) events)
                     (pcase name
                       ('MoveFileExW 0)
                       ('GetLastError (car case))
                       (_ (ert-fail (format "Unexpected foreign call %S" name)))))))
          (should (= (funcall rename 82 101 202) (cdr case)))
          (should (equal (nreverse events)
                         '((MoveFileExW 1101 1202 1) (GetLastError)))))))))

(ert-deftest nelisp-windows-rename-unsupported-syscalls-do-not-dispatch ()
  "Other two-path operations retain ENOSYS without touching either path."
  (let ((rename (nelisp-windows-rename-test--function 'nl_os_syscall_path2)))
    (cl-letf (((symbol-function 'nl_win_utf8_wcs_dup)
               (lambda (&rest _) (ert-fail "Unsupported syscall converted a path")))
              ((symbol-function 'nl_os_win_errno)
               (lambda () (ert-fail "Unsupported syscall queried errno")))
              ((symbol-function 'nelisp-windows-rename-test--extern)
               (lambda (&rest _) (ert-fail "Unsupported syscall called Win32"))))
      (dolist (number '(0 21 83 87 88 -1))
        (should (= (funcall rename number 101 202) -38))))))

(ert-deftest nelisp-windows-rename-is-a-direct-kernel32-import ()
  "The raw adapter's dependency belongs to the core PE import manifest."
  (should (member "MoveFileExW"
                  (cdr (assoc-string "kernel32.dll" nelisp-standalone--windows-reader-imports t))))
  (let ((nelisp-standalone--target 'windows-x86_64))
    (should (member "MoveFileExW"
                    (cdr (assoc-string "kernel32.dll" (nelisp-standalone--reader-pe-imports) t))))))

(ert-deftest nelisp-windows-rename-registers-win64-call-alignment ()
  "The generated native foreign call receives the Win64 alignment path."
  (should (memq 'MoveFileExW nelisp-aot-compiler--win64-dynamic-align-names)))

(provide 'nelisp-windows-rename-test)
;;; nelisp-windows-rename-test.el ends here
