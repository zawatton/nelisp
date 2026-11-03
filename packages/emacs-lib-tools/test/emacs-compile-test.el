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

;;;; --- compilation-error-regexp-alist (T62) ---------------------------

;; Was void anywhere in src/ (`src/compile.el' deliberately installs this
;; file's much smaller `emacs-compile' engine instead of the full real
;; `progmodes/compile.el' -- see that file's own commentary); the load
;; matrix hit `(void-variable compilation-error-regexp-alist)' for
;; `powershell', which does `(setq compilation-error-regexp-alist (cons
;; NEW-ENTRY compilation-error-regexp-alist))' at top level.  This is a
;; reduced value (one real entry: this substrate's own next-error
;; pattern, not the full ~66-entry GNU catalogue -- see the defvar
;; commentary), so the test asserts "kind, not content" (Doc 51 T52
;; precedent) plus that the one real entry actually matches this
;; substrate's own diagnostic format, and that `powershell.el''s own
;; `cons'-onto-the-front usage pattern works without error.
(ert-deftest emacs-compile-test/compilation-error-regexp-alist-is-a-real-alist ()
  (should (boundp 'compilation-error-regexp-alist))
  (should (consp compilation-error-regexp-alist))
  (let ((entry (car compilation-error-regexp-alist)))
    (should (equal emacs-compile-error-regexp (car entry)))
    (should (string-match (car entry) "foo.c:12:3: error"))
    (should (equal "foo.c" (match-string 1 "foo.c:12:3: error")))
    (should (equal "12" (match-string 2 "foo.c:12:3: error")))))

(ert-deftest emacs-compile-test/compilation-error-regexp-alist-supports-cons-onto-front ()
  "Exercises `powershell.el''s own top-level usage pattern verbatim."
  (let* ((before (length compilation-error-regexp-alist))
         (extended (cons '("At \\(.*\\):\\([0-9]+\\) char:\\([0-9]+\\)" 1 2)
                          compilation-error-regexp-alist)))
    (should (= (1+ before) (length extended)))))

;;;; --- compilation-minor-mode-map (T102) ------------------------------

;; Was void anywhere in src/ for the same reason as
;; `compilation-error-regexp-alist' above: `src/compile.el' installs this
;; file's reduced `emacs-compile' engine on the standalone runtime
;; instead of loading the full real `progmodes/compile.el', so that
;; vendor file's `compilation-minor-mode-map' defvar never runs.  The
;; load matrix hit `(void-variable compilation-minor-mode-map)' for
;; `magit-todos', whose dependency `grep.el' does `(set-keymap-parent
;; map compilation-minor-mode-map)' at top level while building
;; `grep-mode-map'.  Unlike `compilation-error-regexp-alist' (a
;; deliberately reduced value -- see that defvar's own commentary), the
;; keymap DATA other packages bind into is cheap, static, and can be
;; ported at full fidelity: bindings and parentage are copied verbatim
;; from real Emacs's `progmodes/compile.el' (verified against GNU Emacs
;; 31.1's own source in the T102 report).
(ert-deftest emacs-compile-test/compilation-minor-mode-map-is-a-real-keymap ()
  (should (boundp 'compilation-minor-mode-map))
  (should (keymapp compilation-minor-mode-map))
  (should (boundp 'compilation-menu-map))
  (should (keymapp compilation-menu-map)))

(ert-deftest emacs-compile-test/compilation-minor-mode-map-parents-special-mode-map ()
  "Ported verbatim from real Emacs `progmodes/compile.el':
`(set-keymap-parent map special-mode-map)'."
  (should (eq special-mode-map (keymap-parent compilation-minor-mode-map))))

(ert-deftest emacs-compile-test/compilation-minor-mode-map-bindings-match-host ()
  "Spot-check bindings copied verbatim from real Emacs's
`progmodes/compile.el' `compilation-minor-mode-map'."
  (should (eq 'compile-goto-error (lookup-key compilation-minor-mode-map "\C-m")))
  (should (eq 'compile-goto-error (lookup-key compilation-minor-mode-map "\C-c\C-c")))
  (should (eq 'kill-compilation (lookup-key compilation-minor-mode-map "\C-c\C-k")))
  (should (eq 'compilation-next-error (lookup-key compilation-minor-mode-map "\M-n")))
  (should (eq 'compilation-previous-error (lookup-key compilation-minor-mode-map "\M-p")))
  (should (eq 'next-error-no-select (lookup-key compilation-minor-mode-map "n")))
  (should (eq 'previous-error-no-select (lookup-key compilation-minor-mode-map "p")))
  (should (eq 'recenter-current-error (lookup-key compilation-minor-mode-map "l")))
  (should (eq 'recompile (lookup-key compilation-minor-mode-map "g")))
  (should (eq 'compile-goto-error (lookup-key compilation-minor-mode-map [mouse-2]))))

(ert-deftest emacs-compile-test/compilation-minor-mode-map-supports-grep-el-usage ()
  "Exercises `progmodes/grep.el''s own top-level usage pattern verbatim:
`(set-keymap-parent map compilation-minor-mode-map)' while building
`grep-mode-map'."
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map compilation-minor-mode-map)
    (should (eq compilation-minor-mode-map (keymap-parent map)))
    ;; A binding compilation-minor-mode-map itself has (not one grep.el
    ;; sets directly) must be reachable through the parent chain.
    (should (eq 'compile-goto-error (lookup-key map "\C-m")))))

(provide 'emacs-compile-test)

;;; emacs-compile-test.el ends here
