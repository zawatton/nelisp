;;; nelisp-emacs-compat-fileio-test.el --- Path fast paths of nelisp-ec-expand-file-name  -*- lexical-binding: t; -*-

;;; Commentary:

;; Differential checks: the string-search clean-path predicate against the
;; former per-character loop, and the plain-relative fast path against the
;; general string surgery.

;;; Code:

(require 'ert)
(require 'seq)
(require 'nelisp-emacs-compat-fileio)

(defun nelisp-ec-fast-path-test--old-clean-p (path)
  "The pre-string-search `nelisp-ec--posix-path-clean-p' loop."
  (and (> (length path) 0)
       (eq (aref path 0) ?/)
       (let ((len (length path)) (i 0) (ok t))
         (while (and ok (< i len))
           (when (eq (aref path i) ?/)
             (let ((c1 (and (< (1+ i) len) (aref path (1+ i))))
                   (c2 (and (< (+ i 2) len) (aref path (+ i 2)))))
               (cond
                ((eq c1 ?/) (setq ok nil))
                ((and (eq c1 ?.) (or (null c2) (eq c2 ?/))) (setq ok nil))
                ((and (eq c1 ?.) (eq c2 ?.)
                      (let ((c3 (and (< (+ i 3) len) (aref path (+ i 3)))))
                        (or (null c3) (eq c3 ?/))))
                 (setq ok nil)))))
           (setq i (1+ i)))
         ok)))

(ert-deftest nelisp-ec-fast-path-test/clean-p-and-relative-match-general ()
  "Fast paths agree with the loop predicate and the general expansion."
  (random "nelisp-ec-fast-path-test")
  (let ((pieces '("a" "bc" "." ".." ".h" "x.y" "~" "" "d:e" "f\\g"))
        (nelisp-ec-file-name-drive-letters nil))
    (dotimes (_ 2000)
      (let* ((parts (let (acc) (dotimes (_ (random 5)) (push (nth (random (length pieces)) pieces) acc)) acc))
             (rel (mapconcat #'identity parts "/"))
             (abs (concat "/" rel))
             (dir (concat "/" (mapconcat #'identity
                                         (seq-filter (lambda (s) (not (member s '("" "~"))))
                                                     parts)
                                         "/")
                          (if (zerop (random 2)) "/" ""))))
        (should (eq (and (nelisp-ec--posix-path-clean-p abs) t)
                    (and (nelisp-ec-fast-path-test--old-clean-p abs) t)))
        (when (and (> (length rel) 0) (not (memq (aref rel 0) (quote (?~ ?/)))))
          (should (equal (nelisp-ec-expand-file-name rel dir)
                         (nelisp-ec--expand-file-name-general rel dir))))))))

(provide 'nelisp-emacs-compat-fileio-test)

;;; nelisp-emacs-compat-fileio-test.el ends here
