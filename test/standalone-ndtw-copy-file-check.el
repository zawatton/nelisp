;;; standalone-ndtw-copy-file-check.el -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Prints one `NAME => RESULT' line per `copy-file' case.  The same file runs
;; on GNU Emacs and on the standalone binary; tools/ai/ndtw-host-copy-check.sh
;; diffs the two transcripts.  Lines starting with `flag-' exist only when the
;; standalone driver sets HC_STANDALONE_FLAGS (options GNU honours or prompts
;; for, which this runtime refuses); the script checks them against fixed
;; expectations, never against GNU.
;; Error payloads that differ only by the OS that produced them are reduced to
;; `(file-error-class)' (any `file-error' descendant) by `hc-class'.
(defvar hc-m0 nil)
(defvar hc-root (make-temp-file "hc-copy" t))
(defun hc-path (&rest parts)
  (expand-file-name (mapconcat #'identity parts "/") hc-root))
(defun hc-norm (x)
  (cond ((stringp x)
         (let ((r (directory-file-name hc-root)) (out x) (i 0))
           (while (setq i (string-search r out))
             (setq out (concat (substring out 0 i) "<D>" (substring out (+ i (length r))))))
           out))
        ((consp x) (mapcar #'hc-norm x))
        (t x)))
(defun hc-read (f)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally f)
    (buffer-string)))
(defun hc-write (f s)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert s)
    (let ((coding-system-for-write 'no-conversion))
      (write-region (point-min) (point-max) f nil 'silent))))
(defun hc-sum (s)
  (let ((a 1) (b 0) (i 0) (n (length s)))
    (while (< i n)
      (setq a (% (+ a (aref s i)) 65521) b (% (+ b a) 65521) i (1+ i)))
    (format "%d:%d:%d" n a b)))
(defun hc-class (e)
  (if (memq 'file-error (get (car e) 'error-conditions)) '(file-error-class) (hc-norm e)))
(defun hc-run (name thunk &optional class)
  (princ (format "%s => %S\n" name
                 (condition-case e (funcall thunk)
                   (error (if class (hc-class e) (hc-norm e)))))))
;; A deliberate refusal: `(refused TYPE "copy-file: WHAT")', message cut at " is not".
(defun hc-run-refusal (name thunk)
  (princ (format "%s => %S
" name
                 (condition-case e (funcall thunk)
                   (error (let ((m (cadr e)))
                            (list 'refused (car e)
                                  (if (and (stringp m) (string-search " is not" m))
                                      (substring m 0 (string-search " is not" m))
                                    m))))))))
(defun hc-bytes (n)
  (let ((l nil) (i 0)) (while (< i n) (setq l (cons (% (* i 7) 256) l) i (1+ i)))
    (apply #'unibyte-string (nreverse l))))
(defun hc-all256 ()
  (let ((l nil) (i 0)) (while (< i 256) (setq l (cons i l) i (1+ i)))
    (apply #'unibyte-string (nreverse l))))
(let* ((s (hc-path "s")) (x (hc-path "x")) (sub (hc-path "sub")))
  (hc-write s "hi")
  (make-directory sub)
  ;; --- binary fidelity
  (let ((all (concat (hc-all256) (hc-all256) "\0\0\0")))
    (hc-write (hc-path "bin") all)
    (hc-run "bin-copy" (lambda () (copy-file (hc-path "bin") (hc-path "bin2"))))
    (hc-run "bin-equal" (lambda () (list (equal (hc-read (hc-path "bin2")) all) (hc-sum (hc-read (hc-path "bin2"))))))
    (hc-run "bin-source-intact" (lambda () (equal (hc-read (hc-path "bin")) all))))
  (hc-write (hc-path "empty") "")
  (hc-run "empty-copy" (lambda () (copy-file (hc-path "empty") (hc-path "empty2")) (list (file-exists-p (hc-path "empty2")) (length (hc-read (hc-path "empty2"))))))
  (hc-write (hc-path "big") (hc-bytes 300000))
  (hc-run "big-copy" (lambda () (copy-file (hc-path "big") (hc-path "big2")) (list (equal (hc-read (hc-path "big")) (hc-read (hc-path "big2"))) (hc-sum (hc-read (hc-path "big2"))))))
  (hc-write (hc-path "crlf") "a\r\nb\n\r\n\x1a")
  (hc-run "crlf-copy" (lambda () (copy-file (hc-path "crlf") (hc-path "crlf2")) (equal (hc-read (hc-path "crlf2")) "a\r\nb\n\r\n\x1a")))
  (hc-write (hc-path "utf8") "\xe3\x81\x82\xff\xfe")
  (hc-run "utf8-copy" (lambda () (copy-file (hc-path "utf8") (hc-path "utf8b")) (equal (hc-read (hc-path "utf8b")) "\xe3\x81\x82\xff\xfe")))
  ;; --- overwrite policy
  (hc-run "missing" (lambda () (copy-file (hc-path "nope") x)))
  (hc-write x "old")
  (hc-run "exists" (lambda () (copy-file s x)))
  (hc-run "exists-untouched" (lambda () (hc-read x)))
  (hc-run "exists-ok" (lambda () (copy-file s x t) (hc-read x)))
  (hc-run "missing-src-vs-existing-dst" (lambda () (copy-file (hc-path "nope") x)))
  ;; --- same file
  (hc-run "same-ok" (lambda () (copy-file s s t)) t)
  (hc-run "same-ok-content" (lambda () (hc-read s)))
  (hc-run "same-noOK" (lambda () (copy-file s s)))
  (hc-run "same-relative" (lambda () (let ((default-directory (file-name-as-directory hc-root))) (copy-file "s" "./s" t))) t)
  (hc-run "same-relative-content" (lambda () (hc-read s)))
  (hc-run "same-via-dotdot" (lambda () (copy-file s (hc-path "sub/../s") t)) t)
  (hc-run "same-via-dotdot-content" (lambda () (hc-read s)))
  ;; --- directories
  (hc-run "dirdest-slash" (lambda () (copy-file s (concat sub "/")) (list (hc-read (hc-path "sub" "s")) (file-directory-p sub))))
  (hc-run "dirdest-noslash-ok" (lambda () (copy-file s sub t)) t)
  (hc-run "dirdest-noslash-noOK" (lambda () (copy-file s sub)))
  (hc-run "dirdest-still-dir" (lambda () (file-directory-p sub)))
  (hc-run "srcdir" (lambda () (copy-file sub (hc-path "y"))) t)
  (hc-run "srcdir-slash" (lambda () (copy-file (concat sub "/") (hc-path "y") t)) t)
  (hc-run "srcdir-no-dest" (lambda () (file-exists-p (hc-path "y"))))
  (hc-run "nodir" (lambda () (copy-file s (hc-path "no" "such" "z"))) t)
  (hc-run "nodir-nothing-created" (lambda () (file-exists-p (hc-path "no"))))
  ;; --- argument errors
  (hc-run "badtype" (lambda () (copy-file 1 x)))
  (hc-run "badtype2" (lambda () (copy-file s nil)))
  ;; --- options this runtime refuses on purpose (KEEP-TIME, PRESERVE-UID-GID,
  ;; PRESERVE-PERMISSIONS, integer OK-IF-ALREADY-EXISTS).  GNU would honour or
  ;; prompt for them, so they run only when the standalone driver sets
  ;; HC_STANDALONE_FLAGS; the check script asserts them separately.
  (when (getenv "HC_STANDALONE_FLAGS")
    (hc-write (hc-path "ro3") "old")
    (setq hc-m0 (file-modes s))
    (hc-run-refusal "flag-keep-time" (lambda () (copy-file s (hc-path "kt") nil t)))
    (hc-run "flag-keep-time-nothing-written" (lambda () (file-exists-p (hc-path "kt"))))
    (hc-run-refusal "flag-uid-gid" (lambda () (copy-file s (hc-path "ug") nil nil t)))
    (hc-run "flag-uid-gid-nothing-written" (lambda () (file-exists-p (hc-path "ug"))))
    (hc-run-refusal "flag-perm" (lambda () (copy-file s (hc-path "pp") nil nil nil t)))
    (hc-run "flag-perm-nothing-written" (lambda () (file-exists-p (hc-path "pp"))))
    (hc-run-refusal "flag-perm-overwrite" (lambda () (copy-file s (hc-path "ro3") t nil nil t)))
    (hc-run "flag-perm-overwrite-untouched" (lambda () (hc-read (hc-path "ro3"))))
    (hc-run "flag-perm-source-modes-unchanged" (lambda () (equal (file-modes s) hc-m0)))
    (hc-run-refusal "flag-integer-ok" (lambda () (copy-file s x 5)))
    (hc-run "flag-integer-ok-untouched" (lambda () (hc-read x))))
  ;; --- cleanup
  (delete-directory hc-root t)
  (princ (format "cleanup => %S\n" (file-exists-p hc-root))))
