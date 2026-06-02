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

(ert-deftest standalone-diagnostics-test/vendor-program-ends-with-success-sentinel ()
  (let ((bootstrap (make-temp-file "vendor-form-bootstrap-" nil ".el"))
        (output (make-temp-file "vendor-form-program-" nil ".el"))
        (vendor-form-standalone-repo-root "/repo")
        forms)
    (unwind-protect
        (progn
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
                     (regexp-quote "(defvar bootstrap-loaded t)")
                     program))
            (should (string-match-p
                     (regexp-quote "(setq sample 1)")
                     program))
            (should-not (string-match-p
                         (regexp-quote "(provide 'sample)")
                         program))
            (should (string-match-p "\n42\n\\'" program))))
      (dolist (file (list bootstrap output))
        (when (file-exists-p file)
          (delete-file file))))))

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

(ert-deftest standalone-diagnostics-test/vendor-load-files-splits-string ()
  (let ((vendor-load-standalone-files "/repo/a.el /repo/b.el"))
    (should (equal (vendor-load-standalone--files)
                   '("/repo/a.el" "/repo/b.el")))))

(ert-deftest standalone-diagnostics-test/vendor-load-program-uses-top-level-loads ()
  (let ((output (make-temp-file "vendor-load-program-" nil ".el"))
        (vendor-load-standalone-repo-root "/repo")
        (vendor-load-standalone-prelude "/nelisp/scripts/nelisp-stdlib-prelude.el")
        (vendor-load-standalone-bootstrap "/repo/build/nemacs-bootstrap.el")
        (vendor-load-standalone-proof-form "(boundp (quote replay-proof))"))
    (unwind-protect
        (progn
          (vendor-load-standalone--write-program
           '("/repo/vendor/a.el" "/repo/vendor/b.el")
           output)
          (let ((program (with-temp-buffer
                           (insert-file-contents output)
                           (buffer-string))))
            (should (string-match-p
                     (regexp-quote
                      "(load \"/nelisp/scripts/nelisp-stdlib-prelude.el\" nil (quote no-message) t t)")
                     program))
            (should (string-match-p
                     (regexp-quote
                      "(load \"/repo/build/nemacs-bootstrap.el\" nil (quote no-message) t t)")
                     program))
            (should (string-match-p
                     (regexp-quote
                      "(load \"/repo/vendor/a.el\" nil (quote no-message) t t)")
                     program))
            (should (string-match-p
                     (regexp-quote
                      "(load \"/repo/vendor/b.el\" nil (quote no-message) t t)")
                     program))
            (should-not (string-match-p "^(progn" program))
            (should (string-match-p
                     (regexp-quote
                      "(if (boundp (quote replay-proof)) 42 13)")
                     program))))
      (when (file-exists-p output)
        (delete-file output)))))

(provide 'standalone-diagnostics-test)

;;; standalone-diagnostics-test.el ends here
