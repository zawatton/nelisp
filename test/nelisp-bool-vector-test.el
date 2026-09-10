;;; nelisp-bool-vector-test.el --- focused standalone bool-vector coverage -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; These cases execute the generated standalone binary.  Host Emacs's own
;; bool-vector implementation cannot expose failures in the tag-10 allocator,
;; packed-bit ABI, or tracing-GC wiring.

;;; Code:

(require 'ert)
(require 'subr-x)

(defconst nelisp-bool-vector-test--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Checkout root containing target/nelisp.")

(defun nelisp-bool-vector-test--binary ()
  (let* ((name (if (eq system-type 'windows-nt)
                   "target/nelisp.exe"
                 "target/nelisp"))
         (binary (expand-file-name name nelisp-bool-vector-test--root)))
    (or (and (file-executable-p binary) binary)
        (ert-skip (format "%s is not built; standalone-reader-test owns it"
                         name)))))

(defun nelisp-bool-vector-test--eval (source)
  (with-temp-buffer
    (let ((rc (call-process (nelisp-bool-vector-test--binary)
                            nil t nil "--eval" source)))
      (should (= rc 0))
      (string-trim (buffer-string)))))

(ert-deftest nelisp-bool-vector/packed-order-and-mutation ()
  (should (equal "#&0\"\""
                 (nelisp-bool-vector-test--eval
                  "#&0\"\"")))
  (should (equal (concat "#&7\"" (string 127) "\"")
                 (nelisp-bool-vector-test--eval
                  "#&7\"\\377\"")))
  (should (equal
           "(t 9 t nil nil t t t t)"
           (nelisp-bool-vector-test--eval
            "(let ((v #&9\"\\001\\002\")) (list (bool-vector-p v) (length v) (aref v 0) (aref v 1) (aref v 8) (progn (aset v 8 t) (aref v 8)) (arrayp v) (sequencep v) (equal v #&9\"\\001\\003\")))")))
  (should (equal
           "(#&7\"w\" t nil #&7\"w\")"
            (nelisp-bool-vector-test--eval
            "(let ((v (make-bool-vector 7 t))) (list v (aref v 0) (progn (aset v 3 nil) (aref v 3)) v))"))))

(ert-deftest nelisp-bool-vector/variadic-and-mutability ()
  (should (equal "(4 nil t t t)"
                 (nelisp-bool-vector-test--eval
                  "(let ((v (bool-vector nil 0 t \"x\"))) (list (length v) (aref v 0) (aref v 1) (aref v 2) (aref v 3)))")))
  (should (equal "(t t nil nil)"
                 (nelisp-bool-vector-test--eval
                  "(let ((v (make-bool-vector 3 nil))) (list (aset v 1 t) (aref v 1) (aset v 1 nil) (aref v 1)))")))
  (should (equal "(t 9 t t t)"
                 (nelisp-bool-vector-test--eval
                  "(let ((v (copy-sequence #&9\"\\001\\002\"))) (list (bool-vector-p v) (length v) (aref v 0) (aset v 8 t) (aref v 8)))")))
  (should (equal "(t t nil)"
                 (nelisp-bool-vector-test--eval
                  "(let ((v (make-bool-vector 9 t))) (garbage-collect) (list (aref v 0) (aref v 8) (progn (aset v 8 nil) (aref v 8))))"))))

(ert-deftest nelisp-bool-vector/equal-and-printing ()
  (should (equal "#&8\"\\377\""
                 (nelisp-bool-vector-test--eval
                  "(make-bool-vector 8 t)")))
  (should (equal "t"
                 (nelisp-bool-vector-test--eval
                  "(equal (make-bool-vector 9 t) #&9\"\\377\\001\")"))))

(ert-deftest nelisp-bool-vector/arity-and-bounds-errors ()
  (dolist (case '("(make-bool-vector)"
                  "(make-bool-vector 1)"
                  "(make-bool-vector 1 nil 2)"
                  "(bool-vector-p)"
                  "(bool-vector-p 1 2)"))
    (should (string-match-p
             "wrong-number-of-arguments"
             (nelisp-bool-vector-test--eval
              (format "(condition-case e %s (error e))"
                      case)))))
  (should (string-match-p
           "args-out-of-range"
           (nelisp-bool-vector-test--eval
            "(condition-case e (aref #&7\"x\" 7) (error e))")))
  (should (equal "(wrong-type-argument wholenump -1)"
                 (nelisp-bool-vector-test--eval
                  "(condition-case e (make-bool-vector -1 nil) (error e))")))
  (dolist (case '("(make-bool-vector t nil)"
                  "(make-bool-vector 1.0 nil)"
                  "(aref #&7\"x\" t)"
                  "(aset #&7\"x\" t t)"))
    (should (string-match-p
             "wrong-type-argument"
             (nelisp-bool-vector-test--eval
              (format "(condition-case e %s (error e) )"
                      case))))))

(ert-deftest nelisp-bool-vector/malformed-literals-error ()
  (dolist (source '("#&7 \"x\"" "#& 7\"x\"" "#&+7\"x\""
                    "#&7\"\"" "#&7\"x" "#&7x"))
    (let ((stderr (make-temp-file "nelisp-bool-vector-stderr-")))
      (unwind-protect
          (with-temp-buffer
            (let ((rc (call-process (nelisp-bool-vector-test--binary)
                                    nil (list t stderr) nil "--eval" source)))
              (should-not (= rc 0))
              (with-temp-buffer
                (insert-file-contents stderr)
                (should (string-match-p "invalid-read-syntax"
                                        (buffer-string))))))
        (delete-file stderr)))))

(provide 'nelisp-bool-vector-test)

;;; nelisp-bool-vector-test.el ends here
