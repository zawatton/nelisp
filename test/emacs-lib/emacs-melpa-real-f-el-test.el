;;; emacs-melpa-real-f-el-test.el --- Phase 4 real MELPA package: f.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; Doc 51 Phase 4 real-package onboarding 第 4 件 = f.el (Johan
;; Andersson、file/path manipulation、~799 LOC、`Package-Requires:
;; s + dash')。
;;
;; f.el は path-manipulation (= 純文字列操作) と filesystem operation
;; (= file-exists? / read-bytes / write-text 等) の 2 segment が混在。
;; 本 ERT では path subset と temp directory 内の filesystem subset を扱う:
;;   * §A path subset      — host load 経路で host 上の string/regex
;;                            primitive のみ使用、shim 不要で動作
;;   * §B filesystem subset — temp directory 内で read/write/mkdir/delete
;;                            の real package smoke を行う

;;; Code:

(require 'ert)
(require 'cl-lib)

(defconst emacs-melpa-real-f-el-test--candidates
  '("/home/madblack-21/.emacs.d/external-packages/f.el/f.el"
    "/usr/share/emacs/site-lisp/elpa/f/f.el"))

(defconst emacs-melpa-real-f-el-test--prereq-paths
  '(("dash" . ("/home/madblack-21/.emacs.d/external-packages/dash.el/dash.el"))
    ("s"    . ("/home/madblack-21/.emacs.d/external-packages/s.el/s.el"))))

(defun emacs-melpa-real-f-el-test--locate (paths)
  (cl-find-if #'file-readable-p paths))

(defmacro emacs-melpa-real-f-el-test--skip-without-source (&rest body)
  "Skip when f.el / dash.el / s.el are not on disk."
  (declare (indent 0) (debug t))
  `(let ((dash (emacs-melpa-real-f-el-test--locate
                (cdr (assoc "dash" emacs-melpa-real-f-el-test--prereq-paths))))
         (s    (emacs-melpa-real-f-el-test--locate
                (cdr (assoc "s"    emacs-melpa-real-f-el-test--prereq-paths))))
         (f    (emacs-melpa-real-f-el-test--locate
                emacs-melpa-real-f-el-test--candidates)))
     (cond
      ((null dash) (ert-skip "dash.el missing (= f.el prerequisite)"))
      ((null s)    (ert-skip "s.el missing (= f.el prerequisite)"))
      ((null f)    (ert-skip "f.el missing"))
      (t (load dash nil t)
         (load s    nil t)
         (load f    nil t)
         ,@body))))

;;;; A. path subset (= MVP coverage that works without filesystem)

(ert-deftest emacs-melpa-real-f-el-test/loads-cleanly ()
  "f.el must load and bind its public path API."
  (emacs-melpa-real-f-el-test--skip-without-source
    (should (fboundp 'f-join))
    (should (fboundp 'f-split))
    (should (fboundp 'f-filename))
    (should (fboundp 'f-dirname))
    (should (fboundp 'f-ext))
    (should (fboundp 'f-base))
    (should (fboundp 'f-swap-ext))))

(ert-deftest emacs-melpa-real-f-el-test/join ()
  (emacs-melpa-real-f-el-test--skip-without-source
    (should (string= "a/b/c"   (f-join "a" "b" "c")))
    (should (string= "/a/b"    (f-join "/a" "b")))
    (should (string= "a"       (f-join "a")))))

(ert-deftest emacs-melpa-real-f-el-test/split-and-filename-and-dirname ()
  (emacs-melpa-real-f-el-test--skip-without-source
    (should (equal '("/" "a" "b" "c.txt")
                   (f-split "/a/b/c.txt")))
    (should (string= "c.txt"   (f-filename "/a/b/c.txt")))
    (should (string= "/a/b"    (f-dirname  "/a/b/c.txt")))))

(ert-deftest emacs-melpa-real-f-el-test/ext-and-base ()
  (emacs-melpa-real-f-el-test--skip-without-source
    (should (string= "txt"     (f-ext  "foo.txt")))
    (should (string= "foo"     (f-base "foo.txt")))
    ;; f-ext / f-base on multi-extension files:
    (should (string= "gz"      (f-ext  "foo.tar.gz")))
    (should (string= "foo.tar" (f-base "foo.tar.gz")))))

(ert-deftest emacs-melpa-real-f-el-test/swap-ext ()
  (emacs-melpa-real-f-el-test--skip-without-source
    (should (string= "foo.md"  (f-swap-ext "foo.txt" "md")))
    (should (string= "/a/b/c.org"
                     (f-swap-ext "/a/b/c.txt" "org")))))

(ert-deftest emacs-melpa-real-f-el-test/full-and-relative-helpers ()
  (emacs-melpa-real-f-el-test--skip-without-source
    ;; f-relative is pure path manipulation (= no filesystem touch)
    (should (string= "c.txt" (f-relative "/a/b/c.txt" "/a/b")))
    (should (string= "b/c.txt" (f-relative "/a/b/c.txt" "/a")))))

(ert-deftest emacs-melpa-real-f-el-test/predicates-pure ()
  (emacs-melpa-real-f-el-test--skip-without-source
    ;; f-root? / f-parent — pure path predicates / queries
    (should      (f-root? "/"))
    (should-not  (f-root? "/a"))
    (should (string= "/a/b" (f-parent "/a/b/c")))))

;;;; B. filesystem subset

(ert-deftest emacs-melpa-real-f-el-test/filesystem-functions ()
  "f.el filesystem helpers must work inside a temporary directory."
  (emacs-melpa-real-f-el-test--skip-without-source
    (let ((root (make-temp-file "emacs-melpa-f-" t)))
      (unwind-protect
          (let* ((alpha (f-join root "alpha.txt"))
                 (nested (f-join root "nested"))
                 (deep (f-join nested "deeper"))
                 (beta (f-join deep "beta.txt"))
                 (touched (f-join root "touched.txt")))
            (f-write-text "alpha" 'utf-8 alpha)
            (should (f-exists? alpha))
            (should (f-readable? alpha))
            (should (f-file? alpha))
            (should (string= "alpha" (f-read-text alpha 'utf-8)))

            (f-append-text "\nbeta" 'utf-8 alpha)
            (should (string= "alpha\nbeta" (f-read-text alpha 'utf-8)))

            (f-mkdir-full-path deep)
            (should (f-directory? deep))
            (f-write-text "deep" 'utf-8 beta)
            (should (string= "deep" (f-read-text beta 'utf-8)))

            (f-touch touched)
            (should (f-file? touched))
            (should (equal '("alpha.txt" "touched.txt")
                           (sort (mapcar #'file-name-nondirectory
                                         (f-files root))
                                 #'string<)))
            (should (equal '("nested")
                           (sort (mapcar #'file-name-nondirectory
                                         (f-directories root))
                                 #'string<)))
            (should (equal '("alpha.txt" "nested" "touched.txt")
                           (sort (mapcar #'file-name-nondirectory
                                         (f-entries root))
                                 #'string<)))

            (f-delete alpha)
            (should-not (f-exists? alpha))
            (f-delete nested t)
            (should-not (f-exists? nested)))
        (when (file-exists-p root)
          (delete-directory root t))))))

(provide 'emacs-melpa-real-f-el-test)

;;; emacs-melpa-real-f-el-test.el ends here
