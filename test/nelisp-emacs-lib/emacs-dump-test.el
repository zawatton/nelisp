;;; emacs-dump-test.el --- ERT for lisp-image dump  -*- lexical-binding: t; -*-

;;; Commentary:

;; Track L ERT.  Verifies the round-trip
;; `emacs-dump-save' → on-disk file → `emacs-dump-read' /
;; `emacs-dump-load' preserves the persisted slice of state.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emacs-dump)
(require 'nelisp-emacs-compat)

;;;; --- fixtures ------------------------------------------------------

(defmacro emacs-dump-test--with-tmpfile (var &rest body)
  "Bind VAR to a fresh tmp-file path; delete it after BODY."
  (declare (indent 1) (debug (symbol body)))
  `(let ((,var (make-temp-file "emacs-dump-test-" nil ".eld")))
     (unwind-protect (progn ,@body)
       (when (file-exists-p ,var)
         (delete-file ,var)))))

;;;; A. Load + parity

(ert-deftest emacs-dump-test/feature-loaded ()
  (should (featurep 'emacs-dump))
  (dolist (sym '(emacs-dump-save emacs-dump-load emacs-dump-read
                 emacs-dump-build-image emacs-dump-image-info))
    (should (fboundp sym))))

(ert-deftest emacs-dump-test/format-version-constant-set ()
  (should (integerp emacs-dump-format-version))
  (should (>= emacs-dump-format-version 1)))

;;;; B. readable-p classifier

(ert-deftest emacs-dump-test/readable-p-leaves ()
  (should (emacs-dump--readable-p nil))
  (should (emacs-dump--readable-p t))
  (should (emacs-dump--readable-p 42))
  (should (emacs-dump--readable-p 3.14))
  (should (emacs-dump--readable-p "hello"))
  (should (emacs-dump--readable-p 'foo)))

(ert-deftest emacs-dump-test/readable-p-recursive ()
  (should (emacs-dump--readable-p '(1 2 3)))
  (should (emacs-dump--readable-p '("a" "b" ("c" 1))))
  (should (emacs-dump--readable-p [1 "two" three]))
  (should (emacs-dump--readable-p '((:a . 1) (:b . "two")))))

(ert-deftest emacs-dump-test/readable-p-rejects-hash-table ()
  (let ((h (make-hash-table)))
    (should-not (emacs-dump--readable-p h))))

;;;; C. build-image content

(ert-deftest emacs-dump-test/build-image-keys-present ()
  (let ((img (emacs-dump-build-image)))
    (should (eq emacs-dump-format-version (plist-get img :version)))
    (should (plist-member img :timestamp))
    (should (plist-member img :features))
    (should (plist-member img :defvars))
    (should (plist-member img :buffers))
    (should (plist-member img :load-history-tail))))

(ert-deftest emacs-dump-test/build-image-features-includes-emacs-dump ()
  (let* ((img (emacs-dump-build-image))
         (feats (plist-get img :features)))
    (should (memq 'emacs-dump feats))))

(ert-deftest emacs-dump-test/build-image-defvars-includes-allowlisted ()
  (let* ((img (emacs-dump-build-image))
         (defvars (plist-get img :defvars)))
    ;; emacs-major-version should always be there.
    (should (assq 'emacs-major-version defvars))
    (should (numberp (cdr (assq 'emacs-major-version defvars))))))

;;;; D. save → read round-trip

(ert-deftest emacs-dump-test/save-then-read ()
  (emacs-dump-test--with-tmpfile path
    (let* ((written (emacs-dump-save path))
           (read    (emacs-dump-read path)))
      (should (eq (plist-get written :version)
                  (plist-get read :version)))
      (should (equal (plist-get written :defvars)
                     (plist-get read :defvars)))
      (should (equal (length (plist-get written :features))
                     (length (plist-get read :features)))))))

(ert-deftest emacs-dump-test/save-creates-file ()
  (emacs-dump-test--with-tmpfile path
    (emacs-dump-save path)
    (should (file-exists-p path))
    (should (> (nth 7 (file-attributes path)) 0))))

(ert-deftest emacs-dump-test/save-file-has-banner ()
  (emacs-dump-test--with-tmpfile path
    (emacs-dump-save path)
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (should (looking-at-p ";;; nelisp-emacs lisp-image dump")))))

;;;; E. version mismatch detection

(ert-deftest emacs-dump-test/read-rejects-version-mismatch ()
  (emacs-dump-test--with-tmpfile path
    (with-temp-buffer
      (insert ";;; bogus header\n")
      (prin1 '(:version 999 :features nil) (current-buffer))
      (insert "\n")
      (write-region (point-min) (point-max) path nil 'silent))
    (should-error (emacs-dump-read path)
                  :type 'emacs-dump-version-mismatch)))

(ert-deftest emacs-dump-test/read-rejects-corrupt ()
  (emacs-dump-test--with-tmpfile path
    (with-temp-buffer
      (insert ";;; no payload\n")
      (write-region (point-min) (point-max) path nil 'silent))
    (should-error (emacs-dump-read path) :type 'emacs-dump-corrupt)))

;;;; F. load applies defvars

(defvar emacs-dump-test--sentinel nil
  "Special variable used by `load-restores-defvars'.
Defined at top level so it lives in the dynamic obarray (= where
`emacs-dump-build-image' looks via `symbol-value').")

(ert-deftest emacs-dump-test/load-restores-defvars ()
  (emacs-dump-test--with-tmpfile path
    (let ((emacs-dump-defvar-allowlist '(emacs-dump-test--sentinel))
          (orig emacs-dump-test--sentinel))
      (unwind-protect
          (progn
            (setq emacs-dump-test--sentinel "loaded-from-dump")
            (emacs-dump-save path)
            (setq emacs-dump-test--sentinel "post-save-mutation")
            (emacs-dump-load path)
            (should (equal "loaded-from-dump" emacs-dump-test--sentinel)))
        (setq emacs-dump-test--sentinel orig)))))

(ert-deftest emacs-dump-test/load-merges-features ()
  (emacs-dump-test--with-tmpfile path
    ;; Synthesise a dump that names a feature we haven't loaded.
    (with-temp-buffer
      (insert (format ";;; format: emacs-dump v%d\n" emacs-dump-format-version))
      (prin1 (list :version emacs-dump-format-version
                   :features '(emacs-dump-test-fake-feature)
                   :defvars nil :buffers []
                   :load-history-tail nil)
             (current-buffer))
      (insert "\n")
      (write-region (point-min) (point-max) path nil 'silent))
    ;; `features' is a global dynamic var that isn't reliably
    ;; `special-variable-p' across hosts (= a `let' binding becomes
    ;; lexical and disconnects from `emacs-dump-load's `push').  We
    ;; mutate the global directly, recording the fake feature was
    ;; absent before and is present after, then unwind.
    (unwind-protect
        (progn
          (should-not (memq 'emacs-dump-test-fake-feature features))
          (emacs-dump-load path)
          (should (memq 'emacs-dump-test-fake-feature features)))
      (setq features (delq 'emacs-dump-test-fake-feature features)))))

;;;; G. buffer capture / restore

(ert-deftest emacs-dump-test/buffer-capture-and-restore ()
  (emacs-dump-test--with-tmpfile path
    (let* ((nelisp-ec--buffers nil)
           (nelisp-ec--current-buffer nil)
           (b (nelisp-ec-generate-new-buffer "ndump-cap")))
      (let ((nelisp-ec--current-buffer b))
        (nelisp-ec-insert "captured contents"))
      (let ((emacs-dump-extra-buffer-names '("ndump-cap")))
        (emacs-dump-save path))
      (let* ((img (emacs-dump-read path))
             (bufs (plist-get img :buffers)))
        (should (vectorp bufs))
        (should (>= (length bufs) 1))
        (let ((captured (cl-find-if
                         (lambda (cell)
                           (equal (car cell) "ndump-cap"))
                         (append bufs nil))))
          (should captured)
          (should (equal "captured contents" (cdr captured))))))))

;;;; H. image-info summary

(ert-deftest emacs-dump-test/image-info-counts ()
  (emacs-dump-test--with-tmpfile path
    (emacs-dump-save path)
    (let ((info (emacs-dump-image-info path)))
      (should (eq emacs-dump-format-version (plist-get info :version)))
      (should (numberp (plist-get info :feature-count)))
      (should (numberp (plist-get info :defvar-count)))
      (should (numberp (plist-get info :buffer-count))))))

;;;; I. nemacs-loadup wiring

(ert-deftest emacs-dump-test/nemacs-save-and-load-roundtrip ()
  (require 'nemacs-loadup)
  (emacs-dump-test--with-tmpfile path
    (let ((img-out (nemacs-save-dump path)))
      (should (plist-get img-out :version))
      (let ((img-in (nemacs-load-dump path)))
        (should (eq (plist-get img-out :version)
                    (plist-get img-in :version)))))))

(provide 'emacs-dump-test)

;;; emacs-dump-test.el ends here
