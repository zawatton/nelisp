;;; emacs-compile-test.el --- ERT for emacs-compile  -*- lexical-binding: t; -*-

;;; Commentary:

;; compile / grep + next-error tests.  Diagnostic parsing is a pure unit;
;; run / navigation use the host `call-process' against `printf' / `grep' and a
;; throwaway file.  Validates the Layer 2 logic independently of the reader.

;;; Code:

(require 'ert)
(require 'emacs-compile)

;;;; --- diagnostic parsing (pure) -----------------------------------

(ert-deftest emacs-compile-test/parse-gcc-and-grep-formats ()
  (let ((errs (emacs-compile--parse-errors
               (concat "foo.c:12:5: error: bad token\n"
                       "bar.c:3: warning: unused\n"
                       "baz.txt:7:hit here\n"
                       "no location line here\n"))))
    (should (= 3 (length errs)))
    (should (equal "foo.c" (plist-get (nth 0 errs) :file)))
    (should (= 12 (plist-get (nth 0 errs) :line)))
    (should (= 5 (plist-get (nth 0 errs) :col)))
    (should (equal "bar.c" (plist-get (nth 1 errs) :file)))
    (should (= 3 (plist-get (nth 1 errs) :line)))
    (should (null (plist-get (nth 1 errs) :col)))
    (should (equal "baz.txt" (plist-get (nth 2 errs) :file)))
    (should (= 7 (plist-get (nth 2 errs) :line)))))

(ert-deftest emacs-compile-test/parse-empty ()
  (should (null (emacs-compile--parse-errors "")))
  (should (null (emacs-compile--parse-errors nil)))
  (should (null (emacs-compile--parse-errors "nothing matches here\n"))))

;;;; --- run / buffer construction -----------------------------------

(ert-deftest emacs-compile-test/run-captures-output-and-errors ()
  (let ((buf (emacs-compile-run
              "printf 'a.c:1:2: error: boom\\nb.c:4: note: hi\\n'")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (let ((text (buffer-substring-no-properties (point-min) (point-max))))
              (should (string-match-p "a.c:1:2: error: boom" text))
              (should (string-match-p "Compilation exited with code 0" text))))
          (should (= 2 (length (emacs-compile-errors))))
          (should (equal "a.c" (plist-get (car (emacs-compile-errors)) :file))))
      (kill-buffer buf))))

;;;; --- navigation ---------------------------------------------------

(ert-deftest emacs-compile-test/next-error-navigation ()
  (emacs-compile-run "printf 'x.c:10: e1\\ny.c:20: e2\\nz.c:30: e3\\n'")
  ;; start before the first; next-error walks forward and clamps at the end
  (should (equal "x.c" (plist-get (emacs-compile-next-error) :file)))
  (should (equal "y.c" (plist-get (emacs-compile-next-error) :file)))
  (should (equal "z.c" (plist-get (emacs-compile-next-error) :file)))
  (should (equal "z.c" (plist-get (emacs-compile-next-error) :file))) ; clamp
  (should (equal "y.c" (plist-get (emacs-compile-previous-error) :file))))

(ert-deftest emacs-compile-test/next-error-nil-when-clean ()
  (emacs-compile-run "printf 'all good, no diagnostics\\n'")
  (should (null (emacs-compile-next-error))))

(ert-deftest emacs-compile-test/next-error-visits-file ()
  (let* ((dir (make-temp-file "emacs-compile-test-" t))
         (file (expand-file-name "src.txt" dir)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "l1\nl2\nl3\nl4\n"))
          (emacs-compile-run (format "printf '%s:3: here\\n'" file))
          (let ((buf (emacs-compile--visit (car (emacs-compile-errors)))))
            (should (bufferp buf))
            (with-current-buffer buf
              ;; point should be on line 3
              (should (= 3 (line-number-at-pos))))
            (kill-buffer buf)))
      (delete-directory dir t))))

;;;; --- real grep ----------------------------------------------------

(ert-deftest emacs-compile-test/grep-real ()
  (let* ((dir (make-temp-file "emacs-compile-test-" t))
         (file (expand-file-name "data.txt" dir))
         (default-directory (file-name-as-directory dir)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "alpha\nNEEDLE here\nbeta\nNEEDLE again\n"))
          ;; -H forces the filename prefix (single-file grep omits it),
          ;; which next-error navigation requires.
          (emacs-compile-run "grep -Hn NEEDLE data.txt")
          (let ((errs (emacs-compile-errors)))
            (should (= 2 (length errs)))
            (should (equal "data.txt" (plist-get (car errs) :file)))
            (should (= 2 (plist-get (car errs) :line)))
            (should (= 4 (plist-get (cadr errs) :line)))))
      (delete-directory dir t))))

;;;; --- recompile -----------------------------------------------------

(ert-deftest emacs-compile-test/recompile-reruns-last ()
  ;; fresh state: with no prior command, recompile errors
  (setq emacs-compile--last-command nil)
  (should-error (emacs-compile-recompile))
  ;; after a run, recompile repeats it and re-parses
  (emacs-compile-run "printf 'r.c:9: redo\\n'")
  (let ((buf (emacs-compile-recompile)))
    (unwind-protect
        (progn
          (should (= 1 (length (emacs-compile-errors))))
          (should (equal "r.c" (plist-get (car (emacs-compile-errors)) :file)))
          (with-current-buffer buf
            (should (string-match-p
                     "r.c:9: redo"
                     (buffer-substring-no-properties (point-min) (point-max))))))
      (kill-buffer buf))))

(provide 'emacs-compile-test)

;;; emacs-compile-test.el ends here
