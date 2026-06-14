;;; org-mode-test.el --- org-mode heading navigation checks  -*- lexical-binding: t; -*-

;;; Commentary:

;; Regression tests for org-mode heading navigation in the GUI bridge:
;; org-next-visible-heading / org-previous-visible-heading /
;; org-forward-heading-same-level / org-back-to-heading / org-at-heading-p,
;; built on the existing org helpers (files--org-heading-level-at etc.).
;; Same two-layer pattern as the other bridge suites: host source-shape +
;; an opt-in standalone gate that drives the functions on a built image.

;;; Code:

(require 'ert)

(defconst org-mode-test--repo-root
  (expand-file-name
   ".." (file-name-directory (or load-file-name buffer-file-name))))

(defun org-mode-test--path (rel)
  (expand-file-name rel org-mode-test--repo-root))

(defconst org-mode-test--bridge-source
  (org-mode-test--path "src/nemacs-gui-file-bridge-runtime.el"))

(defun org-mode-test--slurp (file)
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

;;; --- host source-shape (always runs) -------------------------------------

(ert-deftest org-mode-test/source-shape ()
  "The bridge defines the org heading navigation commands."
  (should (file-readable-p org-mode-test--bridge-source))
  (let ((source (org-mode-test--slurp org-mode-test--bridge-source)))
    (dolist (needle '("(fset 'org-at-heading-p"
                      "(fset 'org-next-visible-heading"
                      "(fset 'org-previous-visible-heading"
                      "(fset 'org-back-to-heading"
                      "(fset 'org-forward-heading-same-level"
                      "(fset 'files--org-scan-heading-forward"
                      "(fset 'files--org-scan-heading-backward"))
      (should (string-match-p (regexp-quote needle) source)))))

;;; --- standalone gate (opt-in) --------------------------------------------

(defun org-mode-test--reader ()
  (catch 'found
    (dolist (candidate
             (list (getenv "NEMACS_GUI_BRIDGE_NELISP")
                   (getenv "NELISP")
                   (org-mode-test--path "../nelisp/target/nelisp")
                   "/tmp/nelisp-snap/nelisp"))
      (when candidate
        (let ((abs (expand-file-name candidate)))
          (when (file-executable-p abs) (throw 'found abs)))))
    nil))

(defmacro org-mode-test--skip-unless-standalone (&rest body)
  (declare (indent 0) (debug t))
  `(cond
    ((not (getenv "NEMACS_RUN_ORG"))
     (ert-skip "set NEMACS_RUN_ORG=1 to run standalone org checks"))
    ((not (org-mode-test--reader))
     (ert-skip "no standalone reader; set NEMACS_GUI_BRIDGE_NELISP or NELISP"))
    (t ,@body)))

(defconst org-mode-test--vendor-core
  (mapcar #'org-mode-test--path
          '("src/json.el"
            "../nelisp/lisp/nelisp-stdlib-regexp.el"
            "src/nemacs-runtime-stdlib-extra.el"
            "src/emacs-network-syscall-shim.el"
            "src/emacs-network-ffi.el"
            "src/emacs-process.el"
            "src/emacs-process-events.el"
            "src/emacs-eventloop.el"
            "src/nemacs-runtime-cdb.el"
            "src/nemacs-runtime-skk.el")))

(defun org-mode-test--build-image ()
  (let ((image (make-temp-file "org-image-" nil ".nlri"))
        (coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8)
        (prelude (org-mode-test--path
                  "../nelisp/scripts/nelisp-stdlib-prelude.el")))
    (with-temp-file image
      (insert ";;; nelisp-runtime-image source-v1\n(progn\n")
      (when (file-readable-p prelude)
        (insert-file-contents prelude) (goto-char (point-max)))
      (dolist (f org-mode-test--vendor-core)
        (when (file-readable-p f)
          (insert-file-contents f) (goto-char (point-max))))
      (insert-file-contents org-mode-test--bridge-source)
      (goto-char (point-max))
      (insert "\n)\n"))
    image))

(defun org-mode-test--run (reader image form)
  (let ((tdir (make-temp-file "org-transport-" t)))
    (unwind-protect
        (let ((wrapped (format "(progn (setq files--transport-dir %S) %s)"
                               tdir form)))
          (with-temp-buffer
            (let ((status (call-process reader nil (current-buffer) nil
                                        "exec-runtime-image" image wrapped)))
              (unless (equal 0 status)
                (ert-fail (format "exec-runtime-image failed: status=%S\n%s"
                                  status (buffer-string))))
              (buffer-string))))
      (when (file-directory-p tdir) (delete-directory tdir t)))))

(ert-deftest org-mode-test/standalone-heading-navigation ()
  "Heading navigation on a todo.org-like buffer:
* INBOX / body / ** t1 / ** t2 / * NEXT / ** t3 (heading bols 0 13 19 25 32)."
  (org-mode-test--skip-unless-standalone
    (let ((reader (org-mode-test--reader))
          (image (org-mode-test--build-image)))
      (unwind-protect
          (let ((out (org-mode-test--run
                      reader image
                      "(progn
  (setq files--buffer-string \"* INBOX\\nbody\\n** t1\\n** t2\\n* NEXT\\n** t3\")
  (setq files--point 0) (org-next-visible-heading)
  (princ (concat \"next0=\" (number-to-string (point)) \"\\n\"))
  (setq files--point 0) (org-forward-heading-same-level)
  (princ (concat \"same0=\" (number-to-string (point)) \"\\n\"))
  (setq files--point 32) (org-previous-visible-heading)
  (princ (concat \"prev32=\" (number-to-string (point)) \"\\n\"))
  (setq files--point 10) (org-back-to-heading)
  (princ (concat \"back10=\" (number-to-string (point)) \"\\n\"))
  (setq files--point 13)
  (princ (concat \"ah=\" (if (org-at-heading-p) \"y\" \"n\")))
  (setq files--point 10)
  (princ (concat (if (org-at-heading-p) \"y\" \"n\") \"\\n\"))
  (setq files--point 32) (org-next-visible-heading)
  (princ (concat \"end=\" (number-to-string (point))
                 \"/\" (number-to-string (length files--buffer-string)) \"\\n\")))")))
            (should (string-match-p "next0=13" out))
            (should (string-match-p "same0=25" out))
            (should (string-match-p "prev32=25" out))
            (should (string-match-p "back10=0" out))
            (should (string-match-p "ah=yn" out))
            (should (string-match-p "end=37/37" out)))
        (delete-file image)))))

(provide 'org-mode-test)

;;; org-mode-test.el ends here
