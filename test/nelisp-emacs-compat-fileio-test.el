;;; nelisp-emacs-compat-fileio-test.el --- ERT for fileio extension  -*- lexical-binding: t; -*-

;; T78 / Phase 9d.A4 — exercises the 13+ file I/O APIs added to
;; `nelisp-emacs-compat' (T39 SHIPPED) and proves the
;; `nelisp-coding'-integrated read/write round-trip including
;; CJK / BMP / multi-byte payloads.
;;
;; Test layout follows the grouping in `nelisp-emacs-compat-fileio.el':
;;   §A. Pure string-surgery APIs   (no filesystem)
;;   §B. Stat-backed predicates     (host stat fallback)
;;   §C. Directory operations       (mkdir / rename / unlink / readdir)
;;   §D. PATH walk                  (executable-find)
;;   §E. File I/O round-trip        (insert-file-contents / write-region)
;;   §F. Cross-API integration      (CJK round-trip, REPLACE flag)

(require 'ert)
(require 'nelisp-emacs-compat)
(require 'nelisp-emacs-compat-fileio)

;;; ─────────────────────────────────────────────────────────────────────
;;; Test fixture: clean buffer registry, no current buffer
;;; ─────────────────────────────────────────────────────────────────────

(defmacro nelisp-ec-fileio-test--with-fresh-world (&rest body)
  "Run BODY with a clean `nelisp-ec' buffer registry."
  (declare (indent 0) (debug (body)))
  `(let ((nelisp-ec--buffers nil)
         (nelisp-ec--current-buffer nil))
     ,@body))

(defmacro nelisp-ec-fileio-test--with-tmpdir (var &rest body)
  "Bind VAR to a fresh temp dir, run BODY, then `delete-directory' it."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,var (make-temp-file "nelisp-ec-fileio-" t)))
     (unwind-protect (progn ,@body)
       (when (file-directory-p ,var)
         (delete-directory ,var t)))))

(defmacro nelisp-ec-fileio-test--with-tmpfile (var &rest body)
  "Bind VAR to a fresh temp file path, run BODY, delete file on exit."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,var (make-temp-file "nelisp-ec-fileio-")))
     (unwind-protect (progn ,@body)
       (when (file-exists-p ,var) (delete-file ,var)))))

;;; ─────────────────────────────────────────────────────────────────────
;;; §A. Pure string-surgery APIs
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-ec-fileio-expand-file-name-relative ()
  (should (equal (nelisp-ec-expand-file-name "foo" "/tmp")
                 "/tmp/foo")))

(ert-deftest nelisp-ec-fileio-expand-file-name-absolute ()
  (should (equal (nelisp-ec-expand-file-name "/etc/hosts" "/anywhere")
                 "/etc/hosts")))

(ert-deftest nelisp-ec-fileio-expand-file-name-collapses-dot-dotdot ()
  (should (equal (nelisp-ec-expand-file-name "../bar" "/tmp/foo")
                 "/tmp/bar"))
  (should (equal (nelisp-ec-expand-file-name "./baz" "/tmp")
                 "/tmp/baz"))
  (should (equal (nelisp-ec-expand-file-name "a/./b/../c" "/x")
                 "/x/a/c")))

(ert-deftest nelisp-ec-fileio-expand-file-name-tilde-uses-home ()
  (let ((process-environment (cons "HOME=/home/u" process-environment)))
    (should (equal (nelisp-ec-expand-file-name "~/notes.txt")
                   "/home/u/notes.txt"))))

(ert-deftest nelisp-ec-fileio-file-name-directory-and-nondirectory ()
  (should (equal (nelisp-ec-file-name-directory "/a/b/c.txt") "/a/b/"))
  (should (equal (nelisp-ec-file-name-nondirectory "/a/b/c.txt") "c.txt"))
  (should (null (nelisp-ec-file-name-directory "noslash")))
  (should (equal (nelisp-ec-file-name-nondirectory "noslash") "noslash")))

(ert-deftest nelisp-ec-fileio-file-name-sans-extension ()
  (should (equal (nelisp-ec-file-name-sans-extension "/a/b.txt") "/a/b"))
  (should (equal (nelisp-ec-file-name-sans-extension "x.tar.gz") "x.tar"))
  (should (equal (nelisp-ec-file-name-sans-extension "noext") "noext"))
  ;; Hidden file leading dot must be preserved.
  (should (equal (nelisp-ec-file-name-sans-extension ".bashrc") ".bashrc")))

(ert-deftest nelisp-ec-fileio-file-name-as-directory-idempotent ()
  (should (equal (nelisp-ec-file-name-as-directory "/tmp") "/tmp/"))
  (should (equal (nelisp-ec-file-name-as-directory "/tmp/") "/tmp/"))
  (should (equal (nelisp-ec-file-name-as-directory "") "/")))

(ert-deftest nelisp-ec-fileio-file-name-absolute-p ()
  (should (nelisp-ec-file-name-absolute-p "/etc"))
  (should (nelisp-ec-file-name-absolute-p "~/x"))
  (should-not (nelisp-ec-file-name-absolute-p "rel/path"))
  (should-not (nelisp-ec-file-name-absolute-p "")))

;;; ─────────────────────────────────────────────────────────────────────
;;; §B. Stat-backed predicates
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-ec-fileio-file-exists-p-true-and-false ()
  (nelisp-ec-fileio-test--with-tmpfile f
    (should (nelisp-ec-file-exists-p f)))
  (should-not (nelisp-ec-file-exists-p
               "/this/path/should/not/exist/nelisp-ec-x")))

(ert-deftest nelisp-ec-fileio-file-readable-p ()
  (nelisp-ec-fileio-test--with-tmpfile f
    (should (nelisp-ec-file-readable-p f))))

(ert-deftest nelisp-ec-fileio-file-directory-p ()
  (nelisp-ec-fileio-test--with-tmpdir d
    (should (nelisp-ec-file-directory-p d)))
  (nelisp-ec-fileio-test--with-tmpfile f
    (should-not (nelisp-ec-file-directory-p f))))

(ert-deftest nelisp-ec-fileio-file-attributes-shape ()
  (nelisp-ec-fileio-test--with-tmpfile f
    (let ((attrs (nelisp-ec-file-attributes f)))
      (should (consp attrs))
      ;; Position 7 = SIZE in the Emacs file-attributes layout.
      (should (numberp (nth 7 attrs))))))

;;; ─────────────────────────────────────────────────────────────────────
;;; §C. Directory operations
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-ec-fileio-mkdir-roundtrip ()
  (nelisp-ec-fileio-test--with-tmpdir tmp
    (let ((sub (concat tmp "/sub")))
      (should (equal sub (nelisp-ec-make-directory sub)))
      (should (nelisp-ec-file-directory-p sub)))))

(ert-deftest nelisp-ec-fileio-mkdir-parents-flag ()
  (nelisp-ec-fileio-test--with-tmpdir tmp
    (let ((nested (concat tmp "/a/b/c")))
      (should (equal nested (nelisp-ec-make-directory nested t)))
      (should (nelisp-ec-file-directory-p nested)))))

(ert-deftest nelisp-ec-fileio-delete-file ()
  (let ((f (make-temp-file "nelisp-ec-fileio-del-")))
    (should (nelisp-ec-file-exists-p f))
    (should (eq t (nelisp-ec-delete-file f)))
    (should-not (nelisp-ec-file-exists-p f))))

(ert-deftest nelisp-ec-fileio-rename-file ()
  (nelisp-ec-fileio-test--with-tmpdir tmp
    (let ((a (concat tmp "/from")) (b (concat tmp "/to")))
      (with-temp-file a (insert "hello"))
      (should (eq t (nelisp-ec-rename-file a b)))
      (should-not (nelisp-ec-file-exists-p a))
      (should (nelisp-ec-file-exists-p b)))))

(ert-deftest nelisp-ec-fileio-rename-file-refuses-overwrite ()
  (nelisp-ec-fileio-test--with-tmpdir tmp
    (let ((a (concat tmp "/from")) (b (concat tmp "/to")))
      (with-temp-file a (insert "x"))
      (with-temp-file b (insert "y"))
      (should-error (nelisp-ec-rename-file a b nil)
                    :type 'nelisp-ec-file-already-exists))))

(ert-deftest nelisp-ec-fileio-directory-files-iterates ()
  (nelisp-ec-fileio-test--with-tmpdir tmp
    (with-temp-file (concat tmp "/a.txt") (insert "1"))
    (with-temp-file (concat tmp "/b.txt") (insert "2"))
    (with-temp-file (concat tmp "/c.log") (insert "3"))
    (let ((all (nelisp-ec-directory-files tmp)))
      (should (member "a.txt" all))
      (should (member "b.txt" all))
      (should (member "c.log" all)))))

(ert-deftest nelisp-ec-fileio-directory-files-match-and-full ()
  (nelisp-ec-fileio-test--with-tmpdir tmp
    (with-temp-file (concat tmp "/a.txt") (insert "1"))
    (with-temp-file (concat tmp "/b.log") (insert "2"))
    (let ((txt (nelisp-ec-directory-files tmp t "\\.txt$")))
      (should (= 1 (length txt)))
      (should (string-suffix-p "/a.txt" (car txt))))))

(ert-deftest nelisp-ec-fileio-directory-files-count-and-sort ()
  (nelisp-ec-fileio-test--with-tmpdir tmp
    (dolist (n '("c.txt" "a.txt" "b.txt"))
      (with-temp-file (concat tmp "/" n) (insert "x")))
    (let ((sorted (cl-remove-if (lambda (s) (member s '("." "..")))
                                (nelisp-ec-directory-files tmp))))
      (should (equal '("a.txt" "b.txt" "c.txt") sorted)))
    ;; Count clamps post-sort.
    (let ((two (cl-remove-if (lambda (s) (member s '("." "..")))
                             (nelisp-ec-directory-files tmp nil nil nil 2))))
      (should (<= (length two) 2)))))

;;; ─────────────────────────────────────────────────────────────────────
;;; §D. PATH walk (executable-find)
;;; ─────────────────────────────────────────────────────────────────────

;; CI-smoke gating rationale: the success / absolute-path executable-find
;; assertions chmod the temp file to 0755 and equal-compare the resolved
;; path with the original.  On Windows native Emacs `set-file-modes' is
;; effectively a no-op for the executable bit and `nelisp-ec-executable-find'
;; returns a path with `.exe' / `.cmd' / `.bat' suffix probing applied,
;; so the equal-comparison can never match.  Skip the two assertions
;; that depend on POSIX exec-bit semantics; the missing-returns-nil
;; counterpart stays enabled everywhere.

(ert-deftest nelisp-ec-fileio-executable-find-success ()
  (skip-unless (memq system-type '(gnu/linux darwin berkeley-unix)))
  (nelisp-ec-fileio-test--with-tmpdir tmp
    (let* ((bin (concat tmp "/myprog"))
           (process-environment (cons (concat "PATH=" tmp) process-environment)))
      (with-temp-file bin (insert "#!/bin/sh\n"))
      (set-file-modes bin #o755)
      (should (equal bin (nelisp-ec-executable-find "myprog"))))))

(ert-deftest nelisp-ec-fileio-executable-find-missing-returns-nil ()
  (let ((process-environment (cons "PATH=/nonexistent" process-environment)))
    (should-not (nelisp-ec-executable-find "definitely-not-installed-xyz"))))

(ert-deftest nelisp-ec-fileio-executable-find-absolute-path ()
  (skip-unless (memq system-type '(gnu/linux darwin berkeley-unix)))
  (nelisp-ec-fileio-test--with-tmpfile f
    (set-file-modes f #o755)
    (should (equal f (nelisp-ec-executable-find f)))))

;;; ─────────────────────────────────────────────────────────────────────
;;; §E. File I/O — read / write round-trip via nelisp-coding (UTF-8)
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-ec-fileio-write-read-ascii-roundtrip ()
  (nelisp-ec-fileio-test--with-fresh-world
    (nelisp-ec-fileio-test--with-tmpfile f
      (let ((buf (nelisp-ec-generate-new-buffer "src")))
        (nelisp-ec-set-buffer buf)
        (nelisp-ec-insert "hello world")
        (let ((bytes (nelisp-ec-write-region 1 12 f)))
          (should (= 11 bytes))))
      (let ((dst (nelisp-ec-generate-new-buffer "dst")))
        (nelisp-ec-set-buffer dst)
        (let ((res (nelisp-ec-insert-file-contents f)))
          (should (consp res))
          (should (equal f (car res)))
          (should (= 11 (cdr res))))
        (should (equal "hello world" (nelisp-ec-buffer-string)))))))

(ert-deftest nelisp-ec-fileio-cjk-utf8-write-read ()
  ;; Pure CJK UTF-8 round-trip (3-byte / char-class) via nelisp-coding.
  (nelisp-ec-fileio-test--with-fresh-world
    (nelisp-ec-fileio-test--with-tmpfile f
      (let ((src (nelisp-ec-generate-new-buffer "ja")))
        (nelisp-ec-set-buffer src)
        (nelisp-ec-insert "こんにちは世界")
        (nelisp-ec-write-region 1 (1+ (nelisp-ec-buffer-size)) f))
      (let ((dst (nelisp-ec-generate-new-buffer "ja-dst")))
        (nelisp-ec-set-buffer dst)
        (nelisp-ec-insert-file-contents f)
        (should (equal "こんにちは世界"
                       (nelisp-ec-buffer-string)))))))

(ert-deftest nelisp-ec-fileio-bmp-emoji-roundtrip ()
  ;; 4-byte UTF-8 (supplementary plane) round-trip.
  (nelisp-ec-fileio-test--with-fresh-world
    (nelisp-ec-fileio-test--with-tmpfile f
      (let ((src (nelisp-ec-generate-new-buffer "emoji")))
        (nelisp-ec-set-buffer src)
        (nelisp-ec-insert "ok 🎌 done")
        (nelisp-ec-write-region 1 (1+ (nelisp-ec-buffer-size)) f))
      (let ((dst (nelisp-ec-generate-new-buffer "emoji-dst")))
        (nelisp-ec-set-buffer dst)
        (nelisp-ec-insert-file-contents f)
        (should (equal "ok 🎌 done" (nelisp-ec-buffer-string)))))))

(ert-deftest nelisp-ec-fileio-write-region-append ()
  (nelisp-ec-fileio-test--with-fresh-world
    (nelisp-ec-fileio-test--with-tmpfile f
      (let ((b (nelisp-ec-generate-new-buffer "ap")))
        (nelisp-ec-set-buffer b)
        (nelisp-ec-insert "foo")
        (nelisp-ec-write-region 1 4 f))
      (let ((b (nelisp-ec-generate-new-buffer "ap2")))
        (nelisp-ec-set-buffer b)
        (nelisp-ec-insert "bar")
        (nelisp-ec-write-region 1 4 f t))
      (let ((b (nelisp-ec-generate-new-buffer "rd")))
        (nelisp-ec-set-buffer b)
        (nelisp-ec-insert-file-contents f)
        (should (equal "foobar" (nelisp-ec-buffer-string)))))))

(ert-deftest nelisp-ec-fileio-insert-file-contents-replace-flag ()
  (nelisp-ec-fileio-test--with-fresh-world
    (nelisp-ec-fileio-test--with-tmpfile f
      (let ((b (nelisp-ec-generate-new-buffer "w")))
        (nelisp-ec-set-buffer b)
        (nelisp-ec-insert "fileX")
        (nelisp-ec-write-region 1 6 f))
      (let ((b (nelisp-ec-generate-new-buffer "r")))
        (nelisp-ec-set-buffer b)
        (nelisp-ec-insert "scratch-")
        (nelisp-ec-insert-file-contents f nil nil nil t)
        ;; REPLACE erased pre-existing "scratch-" before insertion.
        (should (equal "fileX" (nelisp-ec-buffer-string)))))))

(ert-deftest nelisp-ec-fileio-insert-file-contents-missing-file ()
  (nelisp-ec-fileio-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "x")))
      (nelisp-ec-set-buffer b)
      (should-error
       (nelisp-ec-insert-file-contents
        "/nonexistent/path/nelisp-ec-fileio-missing")
       :type 'nelisp-ec-file-missing))))

(ert-deftest nelisp-ec-fileio-insert-file-contents-empty-file ()
  (nelisp-ec-fileio-test--with-fresh-world
    (nelisp-ec-fileio-test--with-tmpfile f
      (let ((b (nelisp-ec-generate-new-buffer "e")))
        (nelisp-ec-set-buffer b)
        (let ((res (nelisp-ec-insert-file-contents f)))
          (should (= 0 (cdr res)))
          (should (equal "" (nelisp-ec-buffer-string))))))))

;;; ─────────────────────────────────────────────────────────────────────
;;; §F. Cross-API integration
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-ec-fileio-mkdir-then-write-then-readdir ()
  ;; Compose mkdir → write → readdir and observe the file appear.
  (nelisp-ec-fileio-test--with-fresh-world
    (nelisp-ec-fileio-test--with-tmpdir tmp
      (let ((sub (concat tmp "/k")))
        (nelisp-ec-make-directory sub)
        (let ((b (nelisp-ec-generate-new-buffer "g")))
          (nelisp-ec-set-buffer b)
          (nelisp-ec-insert "data")
          (nelisp-ec-write-region 1 5 (concat sub "/payload.bin")))
        (let ((listed (nelisp-ec-directory-files sub)))
          (should (member "payload.bin" listed)))))))

(ert-deftest nelisp-ec-fileio-rename-then-read ()
  (nelisp-ec-fileio-test--with-fresh-world
    (nelisp-ec-fileio-test--with-tmpdir tmp
      (let* ((a (concat tmp "/a.txt"))
             (b (concat tmp "/b.txt")))
        (let ((src (nelisp-ec-generate-new-buffer "src")))
          (nelisp-ec-set-buffer src)
          (nelisp-ec-insert "abc")
          (nelisp-ec-write-region 1 4 a))
        (nelisp-ec-rename-file a b)
        (let ((dst (nelisp-ec-generate-new-buffer "dst")))
          (nelisp-ec-set-buffer dst)
          (nelisp-ec-insert-file-contents b)
          (should (equal "abc" (nelisp-ec-buffer-string))))))))

(ert-deftest nelisp-ec-fileio-syscall-fallback-active ()
  ;; T76 not yet shipped — assert simulator path is the active one.
  ;; When T76 lands and adds `nl-syscall-mkdir', this test fails
  ;; loudly so we know to delete it (= integration smoke).
  (should-not (fboundp 'nl-syscall-mkdir))
  (should-not (fboundp 'nl-syscall-stat-ex)))

(provide 'nelisp-emacs-compat-fileio-test)
;;; nelisp-emacs-compat-fileio-test.el ends here
