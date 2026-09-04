;;; emacs-text-property-substrate-test.el --- focused text-property substrate tests  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'emacs-buffer)
(require 'emacs-buffer-builtins)

(defmacro emacs-text-property-substrate-test--with-buffer (name text &rest body)
  "Run BODY in a fresh nelisp buffer NAME containing TEXT."
  (declare (indent 2) (debug (sexp form body)))
  `(let* ((nelisp-ec--buffers nil)
          (nelisp-ec--current-buffer nil)
          (emacs-buffer--state (make-hash-table :test 'eq))
          (emacs-buffer--variable-buffer-local nil)
          (emacs-buffer--default-values (make-hash-table :test 'eq))
          (b (nelisp-ec-generate-new-buffer ,name)))
     (let ((nelisp-ec--current-buffer b)
           (buffer-read-only nil)
           (inhibit-read-only nil))
       (nelisp-ec-insert ,text)
       (nelisp-ec-goto-char 1)
       ,@body)))

(ert-deftest emacs-text-properties-can-carry-dired-line-metadata ()
  (emacs-text-property-substrate-test--with-buffer
      "tp-dired" "alpha.txt\nbeta.txt\n"
    (emacs-buffer-put-text-property
     1 11 'dired-file '(:name "alpha.txt" :type regular) b)
    (should (equal '(:name "alpha.txt" :type regular)
                   (emacs-buffer-get-text-property 1 'dired-file b)))
    (should (equal '(:name "alpha.txt" :type regular)
                   (emacs-buffer-get-text-property 10 'dired-file b)))
    (should-not (emacs-buffer-get-text-property 11 'dired-file b))
    (should (equal '((1 11 (dired-file (:name "alpha.txt" :type regular))))
                   (emacs-buffer-text-property-view 1 19 '(dired-file) b)))))

(ert-deftest emacs-text-properties-can-carry-magit-section-metadata ()
  (emacs-text-property-substrate-test--with-buffer
      "tp-magit" " M src/a.el\n?? test/b.el\n"
    (emacs-buffer-put-text-property
     1 12 'magit-section '(:kind file :path "src/a.el" :status modified) b)
    (emacs-buffer-put-text-property
     12 25 'magit-section '(:kind file :path "test/b.el" :status untracked) b)
    (should (equal '(:kind file :path "src/a.el" :status modified)
                   (emacs-buffer-get-text-property 4 'magit-section b)))
    (should (equal '(:kind file :path "test/b.el" :status untracked)
                   (emacs-buffer-get-text-property 15 'magit-section b)))))

(ert-deftest emacs-text-properties-at-returns-full-property-plist ()
  (emacs-text-property-substrate-test--with-buffer
      "tp-at" "abcdef"
    (emacs-buffer-add-text-properties
     2 5 '(face bold mouse-face highlight help-echo "open") b)
    (should (equal '(face bold mouse-face highlight help-echo "open")
                   (emacs-buffer-text-property-at 3 b)))
    (should-not (emacs-buffer-text-property-at 1 b))))

(ert-deftest emacs-face-property-does-not_change-buffer-text ()
  (emacs-text-property-substrate-test--with-buffer
      "tp-face" "keyword value\n"
    (let ((before (nelisp-ec-buffer-string)))
      (emacs-buffer-put-text-property 1 8 'face 'font-lock-keyword-face b)
      (should (equal before (nelisp-ec-buffer-string)))
      (should (eq 'font-lock-keyword-face
                  (emacs-buffer-get-text-property 4 'face b))))))

(ert-deftest emacs-text-property-ranges-track-insertions-and-deletions ()
  (emacs-text-property-substrate-test--with-buffer
      "tp-shift" "alpha beta gamma"
    ;; Mark "beta", positions 7..11.
    (emacs-buffer-put-text-property 7 11 'face 'hit b)
    (nelisp-ec-goto-char 1)
    (nelisp-ec-insert ">> ")
    (should (eq 'hit (emacs-buffer-get-text-property 10 'face b)))
    (should (eq 'hit (emacs-buffer-get-text-property 13 'face b)))
    (should-not (emacs-buffer-get-text-property 7 'face b))
    ;; Insert inside the marked range; the range should expand.
    (nelisp-ec-goto-char 12)
    (nelisp-ec-insert "X")
    (should (eq 'hit (emacs-buffer-get-text-property 12 'face b)))
    (should (eq 'hit (emacs-buffer-get-text-property 14 'face b)))
    ;; Delete the start of the marked range; remaining marked text shifts down.
    (nelisp-ec-delete-region 10 12)
    (should (eq 'hit (emacs-buffer-get-text-property 10 'face b)))
    (should (eq 'hit (emacs-buffer-get-text-property 12 'face b)))
    (should-not (emacs-buffer-get-text-property 13 'face b))))

(ert-deftest emacs-read-only-text-property-blocks-buffer-mutation ()
  (emacs-text-property-substrate-test--with-buffer
      "tp-read-only" "alpha beta gamma"
    (emacs-buffer-put-text-property 7 11 'read-only t b)
    (let ((before (nelisp-ec-buffer-string)))
      (nelisp-ec-goto-char 8)
      (should-error (nelisp-ec-insert "X") :type 'text-read-only)
      (should (equal before (nelisp-ec-buffer-string)))
      (should-error (nelisp-ec-delete-region 7 8) :type 'text-read-only)
      (should (equal before (nelisp-ec-buffer-string))))
    (let ((inhibit-read-only t))
      (nelisp-ec-goto-char 8)
      (nelisp-ec-insert "X")
      (should (equal "alpha bXeta gamma" (nelisp-ec-buffer-string))))))

(provide 'emacs-text-property-substrate-test)

;;; emacs-text-property-substrate-test.el ends here
