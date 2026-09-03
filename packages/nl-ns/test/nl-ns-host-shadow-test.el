;;; nl-ns-host-shadow-test.el --- host-only nl-ns fixture tests -*- lexical-binding: t; -*-

;;; Commentary:

;; These scan fixture FILES with the analyser, which is a host-side
;; concern: the standalone smoke loads `nl-ns-test.el' wholesale and
;; runs every body on target/nelisp, where reading a file goes through
;; a different path and the checked-in host baseline is out of reach.
;; Keeping them here leaves that smoke to the tests that mean something
;; on the reader.

;;; Code:

(require 'ert)
(require 'nl-ns)
;; No `(require 'nl-ns-test)': the batch runner loads every *-test.el by
;; name, this file sorts first, and requiring the sibling here made it
;; load twice -- which ERT reports as "Test ... redefined (or loaded
;; twice)" and exits 127.  The helpers only have to exist when a test
;; BODY runs, and by then every file is loaded.

(ert-deftest nl-ns-definition-shape-ignores-host-file-coding-system ()
  "The same multibyte definition must hash identically in every host buffer."
  (let* ((doc (concat "caf" (string #xe9)))
         (analysis
          (nl-ns-analyse
           `(("a.el" . ((defun probe () ,doc t)))
             ("b.el" . ((defun probe () ,doc nil))))))
         utf8 shift-jis)
    (with-temp-buffer
      (set-buffer-file-coding-system 'utf-8)
      (setq utf8 (nl-ns--definition-shape
                  '("a.el" "b.el") analysis 'probe)))
    (with-temp-buffer
      (set-buffer-file-coding-system 'japanese-shift-jis-dos)
      (setq shift-jis (nl-ns--definition-shape
                       '("a.el" "b.el") analysis 'probe)))
    (should (equal utf8 shift-jis))))

(ert-deftest nl-ns-host-shadow-unsafe-fixture-fires-all-four-findings ()
  (let* ((baseline (nl-ns-test--baseline-file))
         (findings
          (nl-ns-test--check-files
           (list (nl-ns-test--fixture "shadow-unsafe" "cl-lib.el"))
           nil baseline)))
    (should (equal (mapcar #'nl-ns--finding-severity findings) '(1 2 3 6)))
    (should (equal (mapcar (lambda (finding) (plist-get finding :kind)) findings)
                   '(ns-partial-override ns-unsafe-shim-guard
                     ns-file-shadows-library ns-shadows-host)))
    (let ((partial (car findings)))
      (should (eq (plist-get partial :subject) 'cl-loop))
      ;; The docstring carries several markers; the reported one is the
      ;; first marker of the first sentence that has any, so it is
      ;; "minimal" rather than the "returns nil" further along.
      (should (equal (plist-get partial :marker) "minimal"))
      (should (string-match "Stub: minimal cl-loop" (plist-get partial :sentence))))
    (let ((guard (car (cdr findings))))
      (should (eq (plist-get guard :guard-kind) 'custom))
      (should (plist-get guard :autoloadp))
      (should (string-match "autoloadp" (plist-get guard :guard-source))))
    (should (= (nl-ns-report-max-severity findings) 1))))

(ert-deftest nl-ns-host-shadow-safe-fixture-stays-at-severity-six ()
  (let* ((baseline (nl-ns-test--baseline-file))
         (findings
          (nl-ns-test--check-files
           (list (nl-ns-test--fixture "shadow-safe" "safe-loop.el"))
           nil baseline)))
    (should (equal (mapcar (lambda (finding) (plist-get finding :kind)) findings)
                   '(ns-shadows-host)))
    (should (= (nl-ns-report-max-severity findings) 6))))

(ert-deftest nl-ns-host-shadow-comment-inside-wrapper-counts-as-partial-evidence ()
  "A marker in a comment must count even when the docstring is silent.
The fixture's warning sits in a comment that a `when' wrapper has
pushed away from the top level, which is where real shims put it, and
its docstring says nothing about being partial.  Reading a file rather
than hand-building analyser records is deliberate: the hand-built
version of this test passed metadata the analyser never produces and
so proved nothing about the real path."
  (let* ((baseline (nl-ns-test--baseline-file))
         (findings
          (nl-ns-test--check-files
           (list (nl-ns-test--fixture "shadow-comment" "cl-lib.el"))
           nil baseline)))
    (should (equal (mapcar (lambda (finding) (plist-get finding :kind)) findings)
                   '(ns-partial-override ns-unsafe-shim-guard
                     ns-file-shadows-library ns-shadows-host)))
    (let ((partial (car findings)))
      (should (eq (plist-get partial :subject) 'cl-loop))
      (should (equal (plist-get partial :marker) "returns nil"))
      (should (string-match "returns nil for others"
                            (plist-get partial :sentence))))))

(ert-deftest nl-ns-load-baseline-reads-metadata ()
  (let ((baseline (nl-ns-load-baseline (nl-ns-test--baseline-file))))
    (should (equal (plist-get baseline :emacs-version) "30.1"))
    (should (equal (plist-get baseline :generated-at) "2026-08-15"))
    (should (gethash 'cl-loop (plist-get baseline :functions)))
    (should (gethash 'emacs-version (plist-get baseline :variables)))
    (should (gethash "cl-lib" (plist-get baseline :libraries)))))

(ert-deftest nl-ns-report-includes-baseline-and-severity-summary ()
  (let* ((baseline (nl-ns-test--baseline-file))
         (findings
          (nl-ns-test--check-files
           (list (nl-ns-test--fixture "shadow-unsafe" "cl-lib.el"))
           nil baseline))
         (report (nl-ns-report findings baseline)))
    (should (string-match "severity 1=1 2=1 3=1 6=1" report))
    (should (string-match "baseline 30.1 generated 2026-08-15" report))
    (should (string-match "ns-partial-override" report))
    (should (string-match "minimal" report))))

(ert-deftest nl-ns-accepted-round-trip-silences-exactly-those ()
  (let* ((path (make-temp-file "nl-ns-accepted-" nil ".el"))
         (known (list (nl-ns-test--finding 'ns-collision-divergent 'when
                                           '("prelude.el" "macros.el"))
                      (nl-ns-test--finding 'ns-collision-divergent 'cond
                                           '("prelude.el" "macros.el"))))
         (fresh (nl-ns-test--finding 'ns-collision-divergent 'brand-new
                                     '("prelude.el" "macros.el"))))
    (unwind-protect
        (progn
          (should (= 2 (nl-ns-write-accepted known path "2026-08-16" "bootstrap")))
          (let ((accepted (nl-ns-load-accepted path)))
            (should (equal (plist-get accepted :generated-at) "2026-08-16"))
            (should (equal (plist-get accepted :reason) "bootstrap"))
            (should (null (nl-ns-unaccepted known accepted)))
            (should (equal (nl-ns-unaccepted (cons fresh known) accepted)
                           (list fresh)))))
      (when (file-exists-p path) (delete-file path)))))

(ert-deftest nl-ns-stale-accepted-reports-resolved-entries ()
  "An entry for a divergence that is gone must be reported, not kept."
  (let* ((path (make-temp-file "nl-ns-accepted-" nil ".el"))
         (was (list (nl-ns-test--finding 'ns-collision-divergent 'gone
                                         '("a.el" "b.el"))
                    (nl-ns-test--finding 'ns-collision-divergent 'still
                                         '("a.el" "b.el"))))
         (now (list (nl-ns-test--finding 'ns-collision-divergent 'still
                                         '("a.el" "b.el")))))
    (unwind-protect
        (progn
          (nl-ns-write-accepted was path "2026-08-16" "bootstrap")
          (let ((accepted (nl-ns-load-accepted path)))
            (should (equal (nl-ns-stale-accepted now accepted)
                           (list (nl-ns-finding-key (car was)))))
            (should (null (nl-ns-stale-accepted was accepted)))))
      (when (file-exists-p path) (delete-file path)))))

(provide 'nl-ns-host-shadow-test)

;;; nl-ns-host-shadow-test.el ends here
