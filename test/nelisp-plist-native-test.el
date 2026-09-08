;;; nelisp-plist-native-test.el --- native plist walk parity -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'ert)
(require 'cl-lib)
(require 'subr-x)

(defconst nelisp-plist-native-test--root
  (file-name-directory (directory-file-name
                        (file-name-directory (or load-file-name buffer-file-name)))))

(defun nelisp-plist-native-test--binary ()
  (let* ((names (if (eq system-type 'windows-nt)
                    '("target/nelisp.exe" "target/nelisp")
                  '("target/nelisp" "target/nelisp.exe")))
         (binary (cl-find-if #'file-executable-p
                             (mapcar (lambda (name)
                                       (expand-file-name
                                        name nelisp-plist-native-test--root))
                                     names))))
    (unless binary
      (ert-skip "no executable target/nelisp[.exe] in this worktree"))
    binary))

(defun nelisp-plist-native-test--timeout ()
  ;; Windows timeout.exe waits for console input and cannot supervise a child.
  ;; Coreutils `timeout' (or Homebrew `gtimeout') is required so a cycle bug
  ;; cannot hang the complete ERT run; skip explicitly when it is unavailable.
  (let ((command (or (executable-find "gtimeout")
                     (and (not (eq system-type 'windows-nt))
                          (executable-find "timeout")))))
    (unless command
      (ert-skip "GNU timeout/gtimeout is required for bounded standalone probes"))
    command))

(defun nelisp-plist-native-test--eval (form)
  (with-temp-buffer
    (let ((rc (call-process (nelisp-plist-native-test--timeout) nil t nil "10s"
                            (nelisp-plist-native-test--binary) "--eval" form)))
      (unless (= rc 0) (ert-fail (format "standalone rc=%S output=%S" rc (buffer-string))))
      (string-trim (buffer-string)))))

(ert-deftest nelisp-plist-native/host-oracle-edges ()
  (let ((a (copy-sequence "key")) (b (copy-sequence "key")))
    (should-not (eq a b))
    (should-not (plist-get (list a 1) b))
    (let ((p '(a 1 b nil)))
      (should (eq (plist-member p 'b) (cddr p))))
    (should-not (plist-get '(a 1 b) 'b))
    (should (equal '(b) (plist-member '(a 1 b) 'b)))
    (should (equal '(a nil b 2) (plist-member '(a nil b 2) 'a)))
    (should-not (plist-get '(a 1 . tail) 'z))
    (let ((e (should-error (plist-member '(a 1 . tail) 'z)
                           :type 'wrong-type-argument)))
      (should (equal '(wrong-type-argument plistp (a 1 . tail)) e)))
    (should (= 7 (plist-get '("KEY" 7) "key"
                            (lambda (elt key)
                              (and (equal elt "KEY") (equal key "key"))))))))

(ert-deftest nelisp-plist-native/standalone-edges-and-predicate-fallback ()
  (should
   (equal
    "t"
    (nelisp-plist-native-test--eval
     "(let ((a (copy-sequence \"key\")) (b (copy-sequence \"key\")) (p '(a 1 b nil))) (and (fboundp 'nelisp--plist-get-eq) (fboundp 'nelisp--plist-member-eq) (null (plist-get (list a 1) b)) (eq (plist-member p 'b) (cdr (cdr p))) (equal (plist-member '(a 1 b) 'b) '(b)) (equal (plist-member '(a nil b 2) 'a) '(a nil b 2)) (null (plist-get '(a 1 b) 'b)) (null (plist-get '(a 1 . tail) 'z)) (equal (condition-case e (plist-member '(a 1 . tail) 'z) (wrong-type-argument e)) '(wrong-type-argument plistp (a 1 . tail))) (= (plist-get '(\"KEY\" 7) \"key\" (lambda (elt key) (and (equal elt \"KEY\") (equal key \"key\")))) 7)))"))))

(ert-deftest nelisp-plist-native/circular-parity ()
  (should
   (equal "t"
          (nelisp-plist-native-test--eval
           "(let ((x (list 'a 1 'b 2))) (setcdr (nthcdr 3 x) x) (and (= (plist-get x 'a) 1) (null (plist-get x 'z)) (eq (condition-case nil (plist-member x 'z) (circular-list 'ok)) 'ok)))"))))

(provide 'nelisp-plist-native-test)

;;; nelisp-plist-native-test.el ends here
