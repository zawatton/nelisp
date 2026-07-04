;;; standalone-diagnostics-test.el --- tests for standalone diagnostic helpers  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)

(load (expand-file-name
       "../scripts/standalone-bootstrap-profile.el"
       (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(load (expand-file-name
       "../scripts/vendor-form-standalone-walk.el"
       (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(load (expand-file-name
       "../scripts/vendor-load-standalone-replay.el"
       (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(load (expand-file-name
       "../scripts/vendor-repl-standalone-replay.el"
       (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(load (expand-file-name
       "../scripts/build-nelisp-bootstrap.el"
       (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(ert-deftest standalone-diagnostics-test/profile-splits-bootstrap-sections ()
  (let ((sections
         (standalone-bootstrap-profile--sections
          "header\n;;; >>> src/a.el\n(a)\n;;; >>> src/b.el\n(b)\n")))
    (should (equal (mapcar #'car sections)
                   '("src/a.el" "src/b.el")))
    (should (string-match-p "(a)" (cdr (car sections))))
    (should (string-match-p "(b)" (cdr (cadr sections))))))

(ert-deftest standalone-diagnostics-test/profile-limit-parsing ()
  (should-not (standalone-bootstrap-profile--number-or-nil nil))
  (should-not (standalone-bootstrap-profile--number-or-nil "nil"))
  (should (= 3 (standalone-bootstrap-profile--number-or-nil "3"))))

(ert-deftest standalone-diagnostics-test/vendor-form-parser-records-boundaries ()
  (let ((file (make-temp-file "vendor-form-standalone-test-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert ";;; comment\n")
            (insert "(defvar sample 1)\n")
            (insert "(provide 'sample)\n"))
          (let ((forms (vendor-form-standalone--forms file)))
            (should (= 2 (length forms)))
            (should (= 1 (plist-get (car forms) :index)))
            (should (eq 'defvar (plist-get (car forms) :head)))
            (should (eq 'provide (plist-get (cadr forms) :head)))))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest standalone-diagnostics-test/normalizes-backquote-vectors ()
  (let ((normalized
         (standalone-source-normalize-form
          (read "(defvar x `(((2 1 0) [0 2] [3 5])))"))))
    (should (equal normalized
                   '(defvar x `(((2 1 0) ,(vector 0 2)
                                         ,(vector 3 5))))))))

(ert-deftest standalone-diagnostics-test/vendor-program-ends-with-success-sentinel ()
  (let ((bootstrap (make-temp-file "vendor-form-bootstrap-" nil ".el"))
        (prelude (make-temp-file "vendor-form-prelude-" nil ".el"))
        (output (make-temp-file "vendor-form-program-" nil ".el"))
        (vendor-form-standalone-repo-root "/repo")
        vendor-form-standalone-prelude
        forms)
    (unwind-protect
        (progn
          (setq vendor-form-standalone-prelude prelude)
          (with-temp-file prelude
            (insert "(defvar prelude-loaded t)\n"))
          (with-temp-file bootstrap
            (insert "(defvar bootstrap-loaded t)\n"))
          (setq forms
                (list (list :index 1 :pos 0 :end 7 :head 'setq
                            :text "(setq sample 1)")
                      (list :index 2 :pos 8 :end 18 :head 'provide
                            :text "(provide 'sample)")))
          (vendor-form-standalone--write-program
           bootstrap "/vendor/sample.el" forms 1 output)
          (let ((program (with-temp-buffer
                           (insert-file-contents output)
                           (buffer-string))))
            (should (string-match-p
                     (regexp-quote
                      "(setq load-path '(\"/repo/src\" \"/repo/scripts\"")
                     program))
            (should (string-match-p
                     (regexp-quote "(nelisp--eval-source-string")
                     program))
            (should (string-match-p
                     (regexp-quote "(defvar prelude-loaded t)")
                     program))
            (should (string-match-p
                     (regexp-quote "(defvar bootstrap-loaded t)")
                     program))
            (should (string-match-p
                     (regexp-quote "(setq sample 1)")
                     program))
            (should-not (string-match-p
                         (regexp-quote "(provide 'sample)")
                         program))
            (should (string-match-p "\n(exit 42)\n\\'" program))))
      (dolist (file (list bootstrap prelude output))
        (when (file-exists-p file)
          (delete-file file))))))

(ert-deftest standalone-diagnostics-test/vendor-form-program-loads-preloads-before-target ()
  (let ((bootstrap (make-temp-file "vendor-form-bootstrap-" nil ".el"))
        (output (make-temp-file "vendor-form-program-" nil ".el"))
        (root (make-temp-file "vendor-form-root-" t))
        vendor-form-standalone-repo-root
        forms)
    (unwind-protect
        (progn
          (setq vendor-form-standalone-repo-root root)
          (make-directory (expand-file-name "vendor" root) t)
          (with-temp-file (expand-file-name "vendor/preload-a.el" root)
            (insert "(defvar preload-a-loaded t)\n"))
          (with-temp-file (expand-file-name "vendor/preload-b.el" root)
            (insert "(defvar preload-b-loaded t)\n"))
          (with-temp-file (expand-file-name "vendor/org-footnote.el" root)
            (insert "(defvar preload-footnote-loaded t)\n"))
          (with-temp-file bootstrap
            (insert "(defvar bootstrap-loaded t)\n"))
          (setq forms
                (list (list :index 1 :pos 0 :end 15 :head 'provide
                            :text "(provide 'sample)")))
          (vendor-form-standalone--write-program
           bootstrap "/vendor/sample.el" forms 1 output
           (list (expand-file-name "vendor/preload-a.el" root)
                 (expand-file-name "vendor/preload-b.el" root)
                 (expand-file-name "vendor/org-footnote.el" root)))
          (let* ((program (with-temp-buffer
                            (insert-file-contents output)
                            (buffer-string)))
                 (bootstrap-pos (string-match-p
                                 (regexp-quote "(defvar bootstrap-loaded t)")
                                 program))
                 (preload-a-pos (string-match-p
                                 (regexp-quote
                                  "(defvar preload-a-loaded t)")
                                 program))
                 (preload-b-pos (string-match-p
                                 (regexp-quote
                                  "(defvar preload-b-loaded t)")
                                 program))
                 (preload-footnote-pos (string-match-p
                                        (regexp-quote
                                         "(defvar preload-footnote-loaded t)")
                                        program))
                 (target-pos (string-match-p
                              (regexp-quote
                               "(setq load-file-name \"/vendor/sample.el\")")
                              program)))
            (should bootstrap-pos)
            (should preload-a-pos)
            (should preload-b-pos)
            (should preload-footnote-pos)
            (should target-pos)
            (should (string-match-p
                     (regexp-quote
                      "(setq load-file-name \"org-footnote.el\")")
                     program))
            (should (< bootstrap-pos preload-a-pos))
            (should (< preload-a-pos preload-b-pos))
            (should (< preload-b-pos preload-footnote-pos))
            (should (< preload-footnote-pos target-pos))))
      (dolist (file (list bootstrap output))
        (when (file-exists-p file)
          (delete-file file)))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest standalone-diagnostics-test/vendor-float-normalize-is-opt-in ()
  (let ((forms (list (list :index 1 :pos 0 :end 26 :head 'defun
                           :text "(defun sample () (> x 1.0))"))))
    (let ((vendor-form-standalone-normalize-floats nil))
      (should (string-match-p
               (regexp-quote "1.0")
               (vendor-form-standalone--form-text (car forms)))))
    (let ((vendor-form-standalone-normalize-floats t))
      (should (string-match-p
               (regexp-quote "(> x 1)")
               (vendor-form-standalone--form-text (car forms)))))))

(ert-deftest standalone-diagnostics-test/vendor-form-text-uses-top-level-normalizer ()
  (let ((require-form (list :index 1 :pos 0 :end 19 :head 'require
                            :text "(require 'org-macs)"))
        (inline-form (list :index 2 :pos 20 :end 83 :head 'define-inline
                           :text "(define-inline demo-inline (node) (inline-quote (car node)))")))
    (let ((standalone-source-normalize-current-file "org-element-ast.el"))
      (should (string-empty-p
               (vendor-form-standalone--form-text require-form))))
    (let ((standalone-source-normalize-current-file "org-element-ast.el"))
      (should (string-match-p
               (regexp-quote "(defun demo-inline (node) nil)")
               (vendor-form-standalone--form-text inline-form))))))

(ert-deftest standalone-diagnostics-test/vendor-form-shortens-selected-runtime-file-name ()
  (should (equal (vendor-form-standalone--runtime-file-name
                  "/repo/vendor/emacs-lisp/org/org-element-ast.el")
                 "org-element-ast.el"))
  (should (equal (vendor-form-standalone--runtime-file-name
                  "/repo/vendor/emacs-lisp/org/org-footnote.el")
                 "org-footnote.el"))
  (should (equal (vendor-form-standalone--runtime-file-name
                  "/repo/vendor/emacs-lisp/org/org-list.el")
                 "org-list.el"))
  (should (equal (vendor-form-standalone--runtime-file-name
                  "/repo/vendor/emacs-lisp/org/org-entities.el")
                 "org-entities.el"))
  (should (equal (vendor-form-standalone--runtime-file-name
                  "/repo/vendor/emacs-lisp/org/org-macro.el")
                 "org-macro.el")))

(ert-deftest standalone-diagnostics-test/bootstrap-repl-drops-cl-defstruct-docstring ()
  (let* ((form '(cl-defstruct
                 (sample-state
                  (:constructor sample-state-make)
                  (:copier sample-state-copy))
                 "Large struct docstring that is metadata only."
                 (slot-a nil)
                 (slot-b 0)))
         (normalized (nelisp-bootstrap--standalone-repl-form form)))
    (should (equal normalized
                   '(cl-defstruct
                     (sample-state
                      (:constructor sample-state-make)
                      (:copier sample-state-copy))
                     (slot-a nil)
                     (slot-b 0))))
    (should-not (member "Large struct docstring that is metadata only."
                        normalized))))

(ert-deftest standalone-diagnostics-test/bootstrap-repl-emits-cl-defstruct-directly ()
  (should (nelisp-bootstrap--direct-repl-form-p
           "src/nelisp-coding.el"
           '(cl-defstruct (sample-state (:constructor sample-state-make))
              (slot-a nil))))
  (should-not (nelisp-bootstrap--direct-repl-form-p
               "src/nelisp-coding.el"
               '(defun sample-state-make nil nil))))

(ert-deftest standalone-diagnostics-test/bootstrap-repl-emits-large-forms-directly ()
  (let ((nelisp-bootstrap-repl-direct-character-limit 24))
    (should (nelisp-bootstrap--direct-repl-form-p
             "src/nelisp-coding.el"
             '(defun sample-large nil nil)
             "(defun sample-large nil nil)"))
    (should-not (nelisp-bootstrap--direct-repl-form-p
                 "src/nelisp-coding.el"
                 '(defun x nil nil)
                 "(defun x nil nil)"))))

(ert-deftest standalone-diagnostics-test/vendor-load-files-splits-string ()
  (let ((vendor-load-standalone-files "/repo/a.el /repo/b.el"))
    (should (equal (vendor-load-standalone--files)
                   '("/repo/a.el" "/repo/b.el")))))

(ert-deftest standalone-diagnostics-test/vendor-load-shortens-selected-runtime-file-name ()
  (should (equal (vendor-load-standalone--runtime-file-name
                  "/repo/vendor/emacs-lisp/org/org-element-ast.el")
                 "org-element-ast.el"))
  (should (equal (vendor-load-standalone--runtime-file-name
                  "/repo/vendor/emacs-lisp/org/org-footnote.el")
                 "org-footnote.el"))
  (should (equal (vendor-load-standalone--runtime-file-name
                  "/repo/vendor/emacs-lisp/org/org-list.el")
                 "org-list.el"))
  (should (equal (vendor-load-standalone--runtime-file-name
                  "/repo/vendor/emacs-lisp/org/org-entities.el")
                 "org-entities.el"))
  (should (equal (vendor-load-standalone--runtime-file-name
                  "/repo/vendor/emacs-lisp/org/org-macro.el")
                 "org-macro.el"))
  (should (equal (vendor-load-standalone--runtime-file-name
                  "/repo/vendor/emacs-lisp/org/org.el")
                 "/repo/vendor/emacs-lisp/org/org.el")))

(ert-deftest standalone-diagnostics-test/vendor-load-omits-selected-runtime-file-name ()
  (should (vendor-load-standalone--omit-runtime-file-name-p
           "/repo/vendor/emacs-lisp/org/oc-bibtex.el"))
  (should (vendor-load-standalone--omit-runtime-file-name-p
           "/repo/vendor/emacs-lisp/emacs-lisp/thunk.el"))
  (should (vendor-load-standalone--omit-runtime-file-name-p
           "/repo/vendor/emacs-lisp/env.el"))
  (should (vendor-load-standalone--omit-runtime-file-name-p
           "/repo/vendor/emacs-lisp/fileloop.el"))
  (should-not (vendor-load-standalone--omit-runtime-file-name-p
               "/repo/vendor/emacs-lisp/org/org.el")))

(ert-deftest standalone-diagnostics-test/vendor-load-program-uses-top-level-loads ()
  (let ((bootstrap (make-temp-file "vendor-load-bootstrap-" nil ".el"))
        (prelude (make-temp-file "vendor-load-prelude-" nil ".el"))
        (output (make-temp-file "vendor-load-program-" nil ".el"))
        (root (make-temp-file "vendor-load-root-" t))
        vendor-load-standalone-repo-root
        vendor-load-standalone-prelude
        vendor-load-standalone-bootstrap
        (vendor-load-standalone-proof-form "(boundp (quote replay-proof))"))
    (unwind-protect
        (progn
          (setq vendor-load-standalone-repo-root root)
          (setq vendor-load-standalone-prelude prelude)
          (setq vendor-load-standalone-bootstrap bootstrap)
          (make-directory (expand-file-name "vendor" root) t)
          (with-temp-file (expand-file-name "vendor/a.el" root)
            (insert "(defvar vendor-a-loaded t)\n"))
          (with-temp-file (expand-file-name "vendor/b.el" root)
            (insert "(defvar vendor-b-loaded t)\n"))
          (with-temp-file (expand-file-name "vendor/c.el" root)
            (insert ""))
          (with-temp-file prelude
            (insert "(defvar prelude-loaded t)\n"))
          (with-temp-file bootstrap
            (insert "(defvar bootstrap-loaded t)\n"))
          (vendor-load-standalone--write-program
           (list (expand-file-name "vendor/a.el" root)
                 (expand-file-name "vendor/b.el" root)
                 (expand-file-name "vendor/c.el" root))
           output
           "/tmp/vendor-load-status")
          (let ((program (with-temp-buffer
                           (insert-file-contents output)
                           (buffer-string))))
            (should (string-match-p
                     (regexp-quote "(nelisp--eval-source-string")
                     program))
            (should (string-match-p
                     (regexp-quote "(defvar prelude-loaded t)")
                     program))
            (should (string-match-p
                     (regexp-quote
                      "(defvar bootstrap-loaded t)")
                     program))
            (should (string-match-p
                     (regexp-quote
                      "(setq vendor-standalone-load-file-count 3)")
                     program))
            (should-not (string-match-p
                         (regexp-quote "(progn (nelisp--eval-source-string")
                         program))
            (should (string-match-p
                     (regexp-quote
                      "(setq vendor-standalone-load-ok-count 0)")
                     program))
            (should (string-match-p
                     (regexp-quote
                      "(nl-write-file \"/tmp/vendor-load-status\" \"start:a.el\")")
                     program))
            (should (string-match-p
                     (regexp-quote
                      "(defvar vendor-a-loaded t)")
                     program))
            (should (string-match-p
                     (regexp-quote
                      "(nl-write-file \"/tmp/vendor-load-status\" \"ok:a.el\")")
                     program))
            (should (string-match-p
                     (regexp-quote
                      "(defvar vendor-b-loaded t)")
                     program))
            (should (string-match-p
                     (regexp-quote
                      "(nl-write-file \"/tmp/vendor-load-status\" \"start:b.el\")")
                     program))
            (should (string-match-p
                     (regexp-quote
                      "(nl-write-file \"/tmp/vendor-load-status\" \"ok:b.el\")")
                     program))
            (should-not (string-match-p
                         (regexp-quote
                          "(nl-write-file \"/tmp/vendor-load-status\" \"start:c.el\")")
                         program))
            (should-not (string-match-p
                         (regexp-quote "c.el")
                         program))
            (should (string-match-p
                     (regexp-quote
                      "(setq vendor-standalone-load-ok-count vendor-standalone-load-file-count)")
                     program))
            (should-not (string-match-p
                         (regexp-quote
                          "(setq vendor-standalone-load-ok-count (1+ vendor-standalone-load-ok-count))")
                         program))
            (should-not (string-match-p "\\`(progn" program))
            (should (string-match-p
                     (regexp-quote
                      "(setq vendor-standalone-proof-ok (boundp (quote replay-proof)))")
                     program))
            (should (string-match-p
                     (regexp-quote
                      "(if vendor-standalone-proof-ok nil (exit 13))")
                     program))
            (should (string-match-p
                     (regexp-quote
                      "(exit 42)")
                     program))))
      (dolist (file (list bootstrap prelude output))
        (when (file-exists-p file)
          (delete-file file)))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest standalone-diagnostics-test/vendor-load-program-linearizes-compound-proof ()
  (let ((bootstrap (make-temp-file "vendor-load-bootstrap-" nil ".el"))
        (prelude (make-temp-file "vendor-load-prelude-" nil ".el"))
        (source (make-temp-file "vendor-load-source-" nil ".el"))
        (output (make-temp-file "vendor-load-program-" nil ".el"))
        (root (make-temp-file "vendor-load-root-" t))
        vendor-load-standalone-repo-root
        vendor-load-standalone-prelude
        vendor-load-standalone-bootstrap
        (vendor-load-standalone-proof-form
         "(progn (org-agenda) (and (= vendor-standalone-load-ok-count vendor-standalone-load-file-count) (featurep (quote org-agenda))))"))
    (unwind-protect
        (progn
          (setq vendor-load-standalone-repo-root root)
          (setq vendor-load-standalone-prelude prelude)
          (setq vendor-load-standalone-bootstrap bootstrap)
          (with-temp-file prelude
            (insert "(defvar prelude-loaded t)\n"))
          (with-temp-file bootstrap
            (insert "(defvar bootstrap-loaded t)\n"))
          (with-temp-file source
            (insert "(defvar source-loaded t)\n"))
          (vendor-load-standalone--write-program
           (list source)
           output)
          (let ((program (with-temp-buffer
                           (insert-file-contents output)
                           (buffer-string))))
            (should (string-match-p
                     (regexp-quote "(org-agenda)")
                     program))
            (should (string-match-p
                     (regexp-quote
                      "(setq vendor-standalone-proof-ok (= vendor-standalone-load-ok-count vendor-standalone-load-file-count))")
                     program))
            (should (string-match-p
                     (regexp-quote
                      "(setq vendor-standalone-proof-ok (featurep (quote org-agenda)))")
                     program))
            (should-not (string-match-p
                         (regexp-quote
                          "(if (progn")
                         program))))
      (dolist (file (list bootstrap prelude source output))
        (when (file-exists-p file)
          (delete-file file)))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest standalone-diagnostics-test/vendor-load-program-count-only-proof-uses-sentinel ()
  (let ((bootstrap (make-temp-file "vendor-load-bootstrap-" nil ".el"))
        (prelude (make-temp-file "vendor-load-prelude-" nil ".el"))
        (source (make-temp-file "vendor-load-source-" nil ".el"))
        (output (make-temp-file "vendor-load-program-" nil ".el"))
        (root (make-temp-file "vendor-load-root-" t))
        vendor-load-standalone-repo-root
        vendor-load-standalone-prelude
        vendor-load-standalone-bootstrap
        (vendor-load-standalone-proof-form
         "(and (= vendor-standalone-load-ok-count vendor-standalone-load-file-count) (= vendor-standalone-load-ok-count 1))"))
    (unwind-protect
        (progn
          (setq vendor-load-standalone-repo-root root)
          (setq vendor-load-standalone-prelude prelude)
          (setq vendor-load-standalone-bootstrap bootstrap)
          (with-temp-file prelude
            (insert "(defvar prelude-loaded t)\n"))
          (with-temp-file bootstrap
            (insert "(defvar bootstrap-loaded t)\n"))
          (with-temp-file source
            (insert "(defvar source-loaded t)\n"))
          (vendor-load-standalone--write-program
           (list source)
           output)
          (let ((program (with-temp-buffer
                           (insert-file-contents output)
                           (buffer-string))))
            (should (string-match-p
                     (regexp-quote "(setq vendor-standalone-load-file-count 1)")
                     program))
            (should (string-match-p
                     (regexp-quote "(setq vendor-standalone-load-ok-count 0)")
                     program))
            (should-not (string-match-p
                         (regexp-quote
                          "(setq vendor-standalone-load-ok-count vendor-standalone-load-file-count)")
                         program))
            (should-not (string-match-p
                         (regexp-quote
                          "(setq vendor-standalone-proof-ok")
                         program))
            (should (string-match-p "\n(exit 42)\n\\'" program))))
      (dolist (file (list bootstrap prelude source output))
        (when (file-exists-p file)
          (delete-file file)))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest standalone-diagnostics-test/vendor-load-debug-program-is-kept ()
  (let ((bootstrap (make-temp-file "vendor-load-bootstrap-" nil ".el"))
        (prelude (make-temp-file "vendor-load-prelude-" nil ".el"))
        (source (make-temp-file "vendor-load-source-" nil ".el"))
        (output (make-temp-file "vendor-load-debug-program-" nil ".el"))
        (reader (make-temp-file "vendor-load-reader-" nil ".sh"))
        vendor-load-standalone-repo-root
        vendor-load-standalone-prelude
        vendor-load-standalone-bootstrap
        vendor-load-standalone-reader
        vendor-load-standalone-debug-program
        (vendor-load-standalone-proof-form "(boundp (quote source-loaded))"))
    (unwind-protect
        (progn
          (setq vendor-load-standalone-repo-root "/repo")
          (setq vendor-load-standalone-prelude prelude)
          (setq vendor-load-standalone-bootstrap bootstrap)
          (setq vendor-load-standalone-reader reader)
          (setq vendor-load-standalone-debug-program output)
          (with-temp-file prelude
            (insert "(defvar prelude-loaded t)\n"))
          (with-temp-file bootstrap
            (insert "(defvar bootstrap-loaded t)\n"))
          (with-temp-file source
            (insert "(defvar source-loaded t)\n"))
          (with-temp-file reader
            (insert "#!/bin/sh\nexit 42\n"))
          (set-file-modes reader #o755)
          (delete-file output)
          (should (equal 42 (car (vendor-load-standalone--run (list source)))))
          (should (file-exists-p output))
          (let ((program (with-temp-buffer
                           (insert-file-contents output)
                           (buffer-string))))
            (should (string-match-p
                     (regexp-quote "(defvar source-loaded t)")
                     program))))
      (dolist (file (list bootstrap prelude source output reader))
        (when (file-exists-p file)
          (delete-file file))))))

(ert-deftest standalone-diagnostics-test/vendor-repl-files-splits-string ()
  (let ((vendor-repl-standalone-files "/repo/a.el /repo/b.el"))
    (should (equal (vendor-repl-standalone--files)
                   '("/repo/a.el" "/repo/b.el")))))

(ert-deftest standalone-diagnostics-test/vendor-repl-files-canonicalize-symlinks ()
  (let* ((real-dir (make-temp-file "vendor-repl-real-" t))
         (link-dir (make-temp-file "vendor-repl-link-"))
         (real-file (expand-file-name "a.el" real-dir))
         (link-file (expand-file-name "a.el" link-dir)))
    (delete-file link-dir)
    (unwind-protect
        (progn
          (make-symbolic-link real-dir link-dir)
          (with-temp-file real-file
            (insert "(provide 'a)\n"))
          (let ((vendor-repl-standalone-files link-file))
            (should (equal (vendor-repl-standalone--files)
                           (list (file-truename real-file))))))
      (when (file-exists-p link-dir)
        (delete-file link-dir))
      (when (file-exists-p real-file)
        (delete-file real-file))
      (when (file-directory-p real-dir)
        (delete-directory real-dir)))))

(ert-deftest standalone-diagnostics-test/vendor-repl-load-paths-include-external-magit-vendors ()
  (let ((root (make-temp-file "vendor-repl-root-" t))
        vendor-repl-standalone-repo-root)
    (unwind-protect
        (progn
          (setq vendor-repl-standalone-repo-root root)
          (dolist (dir '("src"
                         "scripts"
                         "vendor/compat"
                         "vendor/cond-let"
                         "vendor/llama"
                         "vendor/transient/lisp"
                         "vendor/dash.el"
                         "vendor/with-editor/lisp"
                         "vendor/magit/lisp"
                         "vendor/emacs-lisp"
                         "vendor/emacs-lisp/emacs-lisp"
                         "vendor/emacs-lisp/vc"))
            (make-directory (expand-file-name dir root) t))
          (should
           (equal
            (vendor-repl-standalone--load-paths)
            (mapcar (lambda (dir)
                      (expand-file-name dir root))
                    '("src"
                      "scripts"
                      "vendor/compat"
                      "vendor/cond-let"
                      "vendor/llama"
                      "vendor/transient/lisp"
                      "vendor/dash.el"
                      "vendor/with-editor/lisp"
                      "vendor/magit/lisp"
                      "vendor/emacs-lisp"
                      "vendor/emacs-lisp/emacs-lisp"
                      "vendor/emacs-lisp/vc")))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest standalone-diagnostics-test/vendor-repl-input-uses-persistent-bootstrap ()
  (let ((bootstrap-repl (make-temp-file "vendor-repl-bootstrap-" nil ".repl"))
        (prelude (make-temp-file "vendor-repl-prelude-" nil ".el"))
        (output (make-temp-file "vendor-repl-input-" nil ".repl"))
        (root (make-temp-file "vendor-repl-root-" t))
        vendor-repl-standalone-repo-root
        (vendor-repl-standalone-bootstrap-repl nil)
        (vendor-repl-standalone-prelude nil)
        (vendor-repl-standalone-direct-character-limit most-positive-fixnum)
        (vendor-repl-standalone-proof-form
         "(boundp (quote replay-proof))")
        (vendor-repl-standalone-detail-form
         "\"replay-proof=nil\""))
    (unwind-protect
        (progn
          (setq vendor-repl-standalone-repo-root root)
          (setq vendor-repl-standalone-bootstrap-repl bootstrap-repl)
          (setq vendor-repl-standalone-prelude prelude)
          (make-directory (expand-file-name "src" root) t)
          (make-directory (expand-file-name "scripts" root) t)
          (make-directory (expand-file-name "vendor" root) t)
          (with-temp-file (expand-file-name "vendor/a.el" root)
            (insert "(defvar vendor-repl-a-loaded t)\n"))
          (with-temp-file (expand-file-name "vendor/b.el" root)
            (insert "(defvar vendor-repl-b-loaded t)\n"))
          (with-temp-file prelude
            (insert "(defvar prelude-loaded t)\n")
            (insert "(defvar prelude-second-line t)\n"))
          (with-temp-file bootstrap-repl
            (insert ";;; bootstrap repl\n")
            (insert "(setq bootstrap-repl-loaded t)\n"))
          (vendor-repl-standalone--write-input
           (list (expand-file-name "vendor/a.el" root)
                 (expand-file-name "vendor/b.el" root))
           "/tmp/vendor-repl-sentinel"
           output)
          (let ((input (with-temp-buffer
                         (insert-file-contents output)
                         (buffer-string))))
            (should (string-match-p
                     (regexp-quote
                      (format "(setq load-path '(%S %S"
                              (expand-file-name "src" root)
                              (expand-file-name "scripts" root)))
                     input))
            (should (string-match-p
                     (regexp-quote "(setq bootstrap-repl-loaded t)")
                     input))
            (should (string-match-p
                     (regexp-quote "(nelisp--eval-source-string")
                     input))
            (should (string-match-p
                     (regexp-quote "(defvar prelude-loaded t)")
                     input))
            (should (string-match-p
                     (regexp-quote
                      "(setq vendor-standalone-load-file-count 2)")
                     input))
            (should (string-match-p
                     (regexp-quote
                      "(setq vendor-standalone-load-ok-count 0)")
                     input))
            (should (string-match-p
                     (regexp-quote
                      "start:a.el")
                     input))
            (should (string-match-p
                     (regexp-quote
                      "(defvar vendor-repl-a-loaded t)")
                     input))
            (should (string-match-p
                     (regexp-quote
                      "ok:a.el")
                     input))
            (should (string-match-p
                     (regexp-quote
                      "start:b.el")
                     input))
            (should (string-match-p
                     (regexp-quote
                      "(defvar vendor-repl-b-loaded t)")
                     input))
            (should (string-match-p
                     (regexp-quote
                      "ok:b.el")
                     input))
            (should (string-match-p
                     (regexp-quote
                      "(setq vendor-standalone-load-ok-count (1+ vendor-standalone-load-ok-count))")
                     input))
            (should (string-match-p
                     (regexp-quote "(setq vendor-repl-load-status \"\")")
                     input))
            (should-not (string-match-p
                         (regexp-quote "ptr-read-u64")
                         input))
            (should (string-match-p
                     (regexp-quote
                      "(nelisp--eval-source-string \"(defvar prelude-loaded t)\")")
                     input))
            (should (string-match-p
                     (regexp-quote
                      "(nelisp--eval-source-string \"(defvar prelude-second-line t)\")")
                     input))
            (should-not (string-match-p
                         "(nelisp--eval-source-string \"[^\"]*\n"
                         input))
            (should (string-match-p
                     "^(setq vendor-repl-proof-value nil)$"
                     input))
            (should (string-match-p
                     "^(setq vendor-repl-proof-evaluated nil)$"
                     input))
            ;; The proof assignment and its ok/fail marker write must each be
            ;; exactly one physical line: this reader's --repl loop evaluates
            ;; one physical line at a time with no continuation, so a
            ;; multi-line form (as `pp-to-string' would produce) is silently
            ;; shredded into unrelated fragments (NeLisp Doc 156 section 7).
            (should (string-match-p
                     (concat "^(setq vendor-repl-proof-value "
                             "(prog1 (boundp 'replay-proof) "
                             "(setq vendor-repl-proof-evaluated t)))$")
                     input))
            (should (string-match-p
                     (concat "^(if vendor-repl-proof-value "
                             "(nl-write-file \"/tmp/vendor-repl-sentinel\" "
                             "\"VENDOR-REPL-STANDALONE=ok\") "
                             "(nl-write-file \"/tmp/vendor-repl-sentinel\" "
                             "(format \"VENDOR-REPL-STANDALONE=fail "
                             "detail=%s evaluated=%s\" \"replay-proof=nil\" "
                             "vendor-repl-proof-evaluated)))$")
                     input))
            ;; This reader's `condition-case'/`ignore-errors' discards the
            ;; protected body's return value unconditionally, even on
            ;; success, so it must not be used to capture the proof value.
            (should-not (string-match-p "condition-case" input))
            (should-not (string-match-p "vendor-repl-proof-error" input))
            (should (string-match-p ",quit\n\\'" input))))
      (dolist (file (list bootstrap-repl prelude output))
        (when (file-exists-p file)
          (delete-file file)))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest standalone-diagnostics-test/vendor-repl-proof-form-file-overrides-inline-form ()
  (let ((bootstrap-repl (make-temp-file "vendor-repl-bootstrap-" nil ".repl"))
        (proof-file (make-temp-file "vendor-repl-proof-" nil ".el"))
        (output (make-temp-file "vendor-repl-input-" nil ".repl"))
        (root (make-temp-file "vendor-repl-root-" t))
        vendor-repl-standalone-repo-root
        vendor-repl-standalone-bootstrap-repl
        (vendor-repl-standalone-prelude nil)
        (vendor-repl-standalone-proof-form "(error \"inline proof used\")")
        vendor-repl-standalone-proof-form-file)
    (unwind-protect
        (progn
          (setq vendor-repl-standalone-repo-root root)
          (setq vendor-repl-standalone-bootstrap-repl bootstrap-repl)
          (setq vendor-repl-standalone-proof-form-file proof-file)
          (make-directory (expand-file-name "vendor" root) t)
          (with-temp-file bootstrap-repl
            (insert "(setq bootstrap-repl-loaded t)\n"))
          (with-temp-file proof-file
            (insert "(progn\n")
            (insert "  (defun vendor-repl-proof-with-doc nil\n")
            (insert "    \"Docstring with newline\nand shell-sensitive `backquote'.\"\n")
            (insert "    t)\n")
            (insert "  (vendor-repl-proof-with-doc))\n"))
          (with-temp-file (expand-file-name "vendor/a.el" root)
            (insert "(defvar vendor-repl-a-loaded t)\n"))
          (vendor-repl-standalone--write-input
           (list (expand-file-name "vendor/a.el" root))
           "/tmp/vendor-repl-sentinel"
           output)
          (let ((input (with-temp-buffer
                         (insert-file-contents output)
                         (buffer-string))))
            (should (string-match-p
                     (regexp-quote "vendor-repl-proof-with-doc")
                     input))
            (should (string-match-p
                     (regexp-quote "shell-sensitive `backquote'")
                     input))
            (should-not (string-match-p
                         (regexp-quote "inline proof used")
                         input))))
      (dolist (file (list bootstrap-repl proof-file output))
        (when (file-exists-p file)
          (delete-file file)))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest standalone-diagnostics-test/vendor-repl-proof-form-file-multi-form-captures-last ()
  (let ((bootstrap-repl (make-temp-file "vendor-repl-bootstrap-" nil ".repl"))
        (proof-file (make-temp-file "vendor-repl-proof-" nil ".el"))
        (output (make-temp-file "vendor-repl-input-" nil ".repl"))
        (root (make-temp-file "vendor-repl-root-" t))
        vendor-repl-standalone-repo-root
        vendor-repl-standalone-bootstrap-repl
        (vendor-repl-standalone-prelude nil)
        vendor-repl-standalone-proof-form-file)
    (unwind-protect
        (progn
          (setq vendor-repl-standalone-repo-root root)
          (setq vendor-repl-standalone-bootstrap-repl bootstrap-repl)
          (setq vendor-repl-standalone-proof-form-file proof-file)
          (make-directory (expand-file-name "vendor" root) t)
          (with-temp-file bootstrap-repl
            (insert "(setq bootstrap-repl-loaded t)\n"))
          (with-temp-file proof-file
            (insert ";;; helper setup form, then the actual proof form\n")
            (insert "(defvar vendor-repl-proof-helper 1)\n")
            (insert "(= (setq vendor-repl-proof-helper\n")
            (insert "         (1+ vendor-repl-proof-helper))\n")
            (insert "   2)\n"))
          (with-temp-file (expand-file-name "vendor/a.el" root)
            (insert "(defvar vendor-repl-a-loaded t)\n"))
          (vendor-repl-standalone--write-input
           (list (expand-file-name "vendor/a.el" root))
           "/tmp/vendor-repl-sentinel"
           output)
          (let ((input (with-temp-buffer
                         (insert-file-contents output)
                         (buffer-string))))
            ;; The setup form runs as its own plain single-line statement...
            (should (string-match-p
                     "^(defvar vendor-repl-proof-helper 1)$"
                     input))
            ;; ...and only the FINAL form's value is captured as the proof.
            (should (string-match-p
                     (concat "^(setq vendor-repl-proof-value "
                             "(prog1 (= (setq vendor-repl-proof-helper "
                             "(1\\+ vendor-repl-proof-helper)) 2) "
                             "(setq vendor-repl-proof-evaluated t)))$")
                     input))))
      (dolist (file (list bootstrap-repl proof-file output))
        (when (file-exists-p file)
          (delete-file file)))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest standalone-diagnostics-test/vendor-repl-input-writes-utf8 ()
  (let ((bootstrap-repl (make-temp-file "vendor-repl-bootstrap-" nil ".repl"))
        (output (make-temp-file "vendor-repl-input-" nil ".repl"))
        (root (make-temp-file "vendor-repl-root-" t))
        (unicode (string (decode-char 'ucs #x3042)))
        vendor-repl-standalone-repo-root
        vendor-repl-standalone-bootstrap-repl
        (vendor-repl-standalone-prelude nil))
    (unwind-protect
        (progn
          (setq vendor-repl-standalone-repo-root root)
          (setq vendor-repl-standalone-bootstrap-repl bootstrap-repl)
          (make-directory (expand-file-name "vendor" root) t)
          (with-temp-file bootstrap-repl
            (insert "(setq bootstrap-repl-loaded t)\n"))
          (let ((coding-system-for-write 'utf-8-unix))
            (with-temp-file (expand-file-name "vendor/a.el" root)
              (insert "(defvar vendor-repl-unicode ")
              (prin1 unicode (current-buffer))
              (insert ")\n")))
          (vendor-repl-standalone--write-input
           (list (expand-file-name "vendor/a.el" root))
           "/tmp/vendor-repl-sentinel"
           output)
          (let ((input (with-temp-buffer
                         (insert-file-contents output)
                         (buffer-string))))
            (should (string-match-p
                     (regexp-quote unicode)
                     input))))
      (dolist (file (list bootstrap-repl output))
        (when (file-exists-p file)
          (delete-file file)))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest standalone-diagnostics-test/vendor-repl-emits-large-forms-directly ()
  (let ((bootstrap-repl (make-temp-file "vendor-repl-bootstrap-" nil ".repl"))
        (output (make-temp-file "vendor-repl-input-" nil ".repl"))
        (root (make-temp-file "vendor-repl-root-" t))
        vendor-repl-standalone-repo-root
        vendor-repl-standalone-bootstrap-repl
        (vendor-repl-standalone-prelude nil)
        (vendor-repl-standalone-direct-character-limit 40)
        (vendor-repl-standalone-trace-forms t))
    (unwind-protect
        (progn
          (setq vendor-repl-standalone-repo-root root)
          (setq vendor-repl-standalone-bootstrap-repl bootstrap-repl)
          (make-directory (expand-file-name "vendor" root) t)
          (with-temp-file bootstrap-repl
            (insert "(setq bootstrap-repl-loaded t)\n"))
          (with-temp-file (expand-file-name "vendor/a.el" root)
            (insert "(defvar vendor-repl-small t)\n")
            (insert "(defvar vendor-repl-large ")
            (prin1 (make-string 64 ?x) (current-buffer))
            (insert ")\n"))
          (vendor-repl-standalone--write-input
           (list (expand-file-name "vendor/a.el" root))
           "/tmp/vendor-repl-sentinel"
           output)
          (let ((input (with-temp-buffer
                         (insert-file-contents output)
                         (buffer-string))))
            (should (string-match-p
                     (regexp-quote
                      "(nelisp--eval-source-string \"(defvar vendor-repl-small t)\")")
                     input))
            (should (string-match-p
                     (regexp-quote "(defvar vendor-repl-large ")
                     input))
            (should-not
             (string-match-p
              (regexp-quote
               "(nelisp--eval-source-string \"(defvar vendor-repl-large")
              input))
            (should (string-match-p
                     (regexp-quote "form-start:a.el:2:count=")
                     input))
            (should (string-match-p
                     (regexp-quote "form-ok:a.el:2:count=")
                     input))))
      (dolist (file (list bootstrap-repl output))
        (when (file-exists-p file)
          (delete-file file)))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest standalone-diagnostics-test/vendor-repl-can-coalesce-file-forms ()
  (let ((bootstrap-repl (make-temp-file "vendor-repl-bootstrap-" nil ".repl"))
        (output (make-temp-file "vendor-repl-input-" nil ".repl"))
        (root (make-temp-file "vendor-repl-root-" t))
        vendor-repl-standalone-repo-root
        vendor-repl-standalone-bootstrap-repl
        (vendor-repl-standalone-prelude nil)
        (vendor-repl-standalone-direct-character-limit 0)
        (vendor-repl-standalone-coalesce-file-forms t))
    (unwind-protect
        (progn
          (setq vendor-repl-standalone-repo-root root)
          (setq vendor-repl-standalone-bootstrap-repl bootstrap-repl)
          (make-directory (expand-file-name "vendor" root) t)
          (with-temp-file bootstrap-repl
            (insert "(setq bootstrap-repl-loaded t)\n"))
          (with-temp-file (expand-file-name "vendor/a.el" root)
            (insert "(defvar vendor-repl-a-loaded t)\n")
            (insert "(defvar vendor-repl-b-loaded t)\n")
            (insert "(provide 'vendor-repl-a)\n"))
          (vendor-repl-standalone--write-input
           (list (expand-file-name "vendor/a.el" root))
           "/tmp/vendor-repl-sentinel"
           output)
          (let ((input (with-temp-buffer
                         (insert-file-contents output)
                         (buffer-string))))
            (should (string-match-p
                     (regexp-quote "(progn")
                     input))
            (should (string-match-p
                     (regexp-quote "(defvar vendor-repl-a-loaded t)")
                     input))
            (should (string-match-p
                     (regexp-quote "(defvar vendor-repl-b-loaded t)")
                     input))
            (should (string-match-p
                     (regexp-quote "(provide 'vendor-repl-a)")
                     input))
            (should (string-match-p
                     (regexp-quote "(provide 'vendor-repl-a)\n")
                     input))
            (should-not (string-match-p
                         (regexp-quote "form-start:a.el")
                         input))))
      (dolist (file (list bootstrap-repl output))
        (when (file-exists-p file)
          (delete-file file)))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest standalone-diagnostics-test/vendor-load-inserts-org-macro-directly ()
  (let ((bootstrap (make-temp-file "vendor-load-bootstrap-" nil ".el"))
        (prelude (make-temp-file "vendor-load-prelude-" nil ".el"))
        (root (make-temp-file "vendor-load-root-" t))
        (output (make-temp-file "vendor-load-program-" nil ".el"))
        vendor-load-standalone-repo-root
        vendor-load-standalone-prelude
        vendor-load-standalone-bootstrap)
    (unwind-protect
        (progn
          (setq vendor-load-standalone-repo-root root)
          (setq vendor-load-standalone-prelude prelude)
          (setq vendor-load-standalone-bootstrap bootstrap)
          (make-directory (expand-file-name "vendor/org" root) t)
          (with-temp-file prelude
            (insert "(defvar prelude-loaded t)\n"))
          (with-temp-file bootstrap
            (insert "(defvar bootstrap-loaded t)\n"))
          (with-temp-file (expand-file-name "vendor/org/org-macro.el" root)
            (insert "(defvar org-macro-direct \"\\000\")\n"))
          (vendor-load-standalone--write-program
           (list (expand-file-name "vendor/org/org-macro.el" root))
           output)
          (let ((program (with-temp-buffer
                           (insert-file-contents output)
                           (buffer-string))))
            (should (string-match-p
                     (regexp-quote "(setq load-file-name \"org-macro.el\")")
                     program))
            (should (string-match-p
                     (regexp-quote
                      (concat "(defvar org-macro-direct \""
                              (string 0)
                              "\")"))
                     program))
            (should-not (string-match-p
                         (regexp-quote
                          "(nelisp--eval-source-string \"(defvar org-macro-direct")
                         program))))
      (dolist (file (list bootstrap prelude output))
        (when (file-exists-p file)
          (delete-file file)))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest standalone-diagnostics-test/vendor-repl-input-canonicalizes-repo-root ()
  (let* ((real-root (make-temp-file "vendor-repl-root-real-" t))
         (link-root (make-temp-file "vendor-repl-root-link-"))
         (bootstrap-repl (make-temp-file "vendor-repl-bootstrap-" nil ".repl"))
         (output (make-temp-file "vendor-repl-input-" nil ".repl"))
         (vendor-repl-standalone-repo-root link-root)
         (vendor-repl-standalone-bootstrap-repl bootstrap-repl)
         (vendor-repl-standalone-prelude nil))
    (delete-file link-root)
    (unwind-protect
        (progn
          (make-symbolic-link real-root link-root)
          (with-temp-file bootstrap-repl
            (insert "(setq bootstrap-repl-loaded t)\n"))
          (make-directory (expand-file-name "src" real-root) t)
          (make-directory (expand-file-name "scripts" real-root) t)
          (make-directory (expand-file-name "vendor" real-root) t)
          (with-temp-file (expand-file-name "vendor/a.el" real-root)
            (insert "(defvar vendor-repl-root-a-loaded t)\n"))
          (vendor-repl-standalone--write-input
           (list (expand-file-name "vendor/a.el" real-root))
           "/tmp/vendor-repl-sentinel"
           output)
          (let* ((input (with-temp-buffer
                          (insert-file-contents output)
                          (buffer-string)))
                 (root (file-name-as-directory (file-truename real-root))))
            (should (string-match-p
                     (regexp-quote
                      (format "(setq nelisp-emacs-vendor-root %S)"
                              (expand-file-name "vendor" root)))
                     input))
            (should (string-match-p
                     (regexp-quote
                      (format "(setq load-path '(%S %S"
                              (expand-file-name "src" root)
                              (expand-file-name "scripts" root)))
                     input))))
      (when (file-exists-p link-root)
        (delete-file link-root))
      (when (file-exists-p bootstrap-repl)
        (delete-file bootstrap-repl))
      (when (file-exists-p output)
        (delete-file output))
      (when (file-directory-p real-root)
        (delete-directory real-root t)))))

(ert-deftest standalone-diagnostics-test/vendor-repl-trace-records-form-boundaries ()
  (let ((bootstrap-repl (make-temp-file "vendor-repl-bootstrap-" nil ".repl"))
        (output (make-temp-file "vendor-repl-input-" nil ".repl"))
        (root (make-temp-file "vendor-repl-root-" t))
        vendor-repl-standalone-repo-root
        vendor-repl-standalone-bootstrap-repl
        (vendor-repl-standalone-prelude nil)
        (vendor-repl-standalone-trace-forms t))
    (unwind-protect
        (progn
          (setq vendor-repl-standalone-repo-root root)
          (setq vendor-repl-standalone-bootstrap-repl bootstrap-repl)
          (make-directory (expand-file-name "vendor" root) t)
          (with-temp-file bootstrap-repl
            (insert "(setq bootstrap-repl-loaded t)\n"))
          (with-temp-file (expand-file-name "vendor/a.el" root)
            (insert "(defvar vendor-repl-a-loaded t)\n")
            (insert "(provide 'vendor-repl-a)\n"))
          (vendor-repl-standalone--write-input
           (list (expand-file-name "vendor/a.el" root))
           "/tmp/vendor-repl-sentinel"
           output)
          (let ((input (with-temp-buffer
                         (insert-file-contents output)
                         (buffer-string))))
            (should (string-match-p
                     (regexp-quote "form-start:a.el:1:count=")
                     input))
            (should (string-match-p
                     (regexp-quote "form-ok:a.el:1:count=")
                     input))
            (should (string-match-p
                     (regexp-quote "form-start:a.el:2:count=")
                     input))
            (should (string-match-p
                     (regexp-quote "form-ok:a.el:2:count=")
                     input))))
      (dolist (file (list bootstrap-repl output))
        (when (file-exists-p file)
          (delete-file file)))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest standalone-diagnostics-test/vendor-repl-internal-timeout-keeps-diagnostics ()
  (let ((bootstrap-repl (make-temp-file "vendor-repl-bootstrap-" nil ".repl"))
        (reader (make-temp-file "vendor-repl-reader-"))
        (root (make-temp-file "vendor-repl-root-" t))
        vendor-repl-standalone-reader
        vendor-repl-standalone-repo-root
        vendor-repl-standalone-bootstrap-repl
        (vendor-repl-standalone-prelude nil)
        (vendor-repl-standalone-internal-timeout-seconds 0.2)
        (vendor-repl-standalone-keep-temp nil)
        (vendor-repl-standalone-keep-temp-on-timeout t))
    (unwind-protect
        (progn
          (setq vendor-repl-standalone-reader reader)
          (setq vendor-repl-standalone-repo-root root)
          (setq vendor-repl-standalone-bootstrap-repl bootstrap-repl)
          (make-directory (expand-file-name "vendor" root) t)
          (with-temp-file bootstrap-repl
            (insert "(setq bootstrap-repl-loaded t)\n"))
          (with-temp-file (expand-file-name "vendor/a.el" root)
            (insert "(defvar vendor-repl-a-loaded t)\n"))
          (with-temp-file reader
            (insert "#!/bin/sh\n")
            (insert "printf 'reader started\\n'\n")
            (insert "sleep 5\n"))
          (set-file-modes reader #o755)
          (pcase-let ((`(,exit ,_elapsed ,output ,sentinel
                         ,input ,out ,marker ,timed-out)
                       (vendor-repl-standalone--run
                        (list (expand-file-name "vendor/a.el" root)))))
            (should (= exit 124))
            (should timed-out)
            (should (equal sentinel "reader:start"))
            (should (string-match-p "reader started" output))
            (should (file-exists-p input))
            (should (file-exists-p out))
            (should (file-exists-p marker))
            (dolist (file (list input out marker))
              (when (file-exists-p file)
                (delete-file file)))))
      (dolist (file (list bootstrap-repl reader))
        (when (file-exists-p file)
          (delete-file file)))
      (when (file-directory-p root)
        (delete-directory root t)))))

(provide 'standalone-diagnostics-test)

;;; standalone-diagnostics-test.el ends here
