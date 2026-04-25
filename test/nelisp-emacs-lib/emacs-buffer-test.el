;;; emacs-buffer-test.el --- ERT tests for emacs-buffer.el  -*- lexical-binding: t; -*-

;; Phase 1 module 1/6 tests per nelisp-emacs Doc 01.
;; Covers all 5 categories of `emacs-buffer-*' API across 28 tests:
;;   A. buffer-local variables  (10 tests)
;;   B. text-property MVP        (7 tests)
;;   C. undo system              (6 tests)
;;   D. modification tracking    (4 tests)
;;   E. additional buffer ops    (5 tests)

(require 'ert)
(require 'emacs-buffer)

;;; Fresh-world fixture (resets every global in both modules)

(defmacro emacs-buffer-test--with-fresh-world (&rest body)
  "Run BODY with clean nelisp-ec + emacs-buffer state."
  (declare (indent 0) (debug (body)))
  `(let ((nelisp-ec--buffers nil)
         (nelisp-ec--current-buffer nil)
         (emacs-buffer--state (make-hash-table :test 'eq))
         (emacs-buffer--variable-buffer-local nil)
         (emacs-buffer--default-values (make-hash-table :test 'eq)))
     ,@body))

;;;; A. buffer-local variables (10 tests)

(ert-deftest emacs-buffer-make-local-variable-creates-binding ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "x")))
      (nelisp-ec-set-buffer b)
      (emacs-buffer-set-default 'foo 42)
      (emacs-buffer-make-local-variable 'foo)
      (should (emacs-buffer-local-variable-p 'foo))
      (should (= 42 (emacs-buffer-buffer-local-value 'foo b))))))

(ert-deftest emacs-buffer-make-local-variable-idempotent ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "x")))
      (nelisp-ec-set-buffer b)
      (emacs-buffer-set-default 'foo 1)
      (emacs-buffer-make-local-variable 'foo)
      (emacs-buffer-set-buffer-local-value 'foo b 99)
      ;; second call must NOT clobber the local value
      (emacs-buffer-make-local-variable 'foo)
      (should (= 99 (emacs-buffer-buffer-local-value 'foo b))))))

(ert-deftest emacs-buffer-buffer-local-value-falls-back-to-default ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "x")))
      (emacs-buffer-set-default 'bar "default")
      (should (string-equal "default"
                            (emacs-buffer-buffer-local-value 'bar b))))))

(ert-deftest emacs-buffer-buffer-local-value-void-when-no-default ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "x")))
      (should-error (emacs-buffer-buffer-local-value 'absent b)
                    :type 'void-variable))))

(ert-deftest emacs-buffer-default-value-and-default-boundp ()
  (emacs-buffer-test--with-fresh-world
    (should (null (emacs-buffer-default-boundp 'absent)))
    (should-error (emacs-buffer-default-value 'absent) :type 'void-variable)
    (emacs-buffer-set-default 'present 7)
    (should (emacs-buffer-default-boundp 'present))
    (should (= 7 (emacs-buffer-default-value 'present)))))

(ert-deftest emacs-buffer-setq-default-macro-works ()
  (emacs-buffer-test--with-fresh-world
    (emacs-buffer-setq-default xxx 123)
    (should (= 123 (emacs-buffer-default-value 'xxx)))))

(ert-deftest emacs-buffer-local-variable-if-set-p ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "x")))
      (nelisp-ec-set-buffer b)
      (should-not (emacs-buffer-local-variable-if-set-p 'mode-line-format))
      (emacs-buffer-make-variable-buffer-local 'mode-line-format)
      (should (emacs-buffer-local-variable-if-set-p 'mode-line-format))
      ;; locally-bound vars also count
      (emacs-buffer-set-default 'foo 1)
      (emacs-buffer-make-local-variable 'foo)
      (should (emacs-buffer-local-variable-if-set-p 'foo)))))

(ert-deftest emacs-buffer-buffer-local-variables-lists-all ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "x")))
      (nelisp-ec-set-buffer b)
      (emacs-buffer-set-default 'a 1)
      (emacs-buffer-set-default 'b 2)
      (emacs-buffer-make-local-variable 'a)
      (emacs-buffer-make-local-variable 'b)
      (let ((alist (emacs-buffer-buffer-local-variables b)))
        (should (= 2 (length alist)))
        (should (assq 'a alist))
        (should (assq 'b alist))))))

(ert-deftest emacs-buffer-kill-local-variable-removes-binding ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "x")))
      (nelisp-ec-set-buffer b)
      (emacs-buffer-set-default 'foo 0)
      (emacs-buffer-make-local-variable 'foo)
      (should (emacs-buffer-local-variable-p 'foo))
      (emacs-buffer-kill-local-variable 'foo)
      (should-not (emacs-buffer-local-variable-p 'foo)))))

(ert-deftest emacs-buffer-kill-all-local-variables-clears-all ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "x")))
      (nelisp-ec-set-buffer b)
      (emacs-buffer-set-default 'a 1)
      (emacs-buffer-set-default 'b 2)
      (emacs-buffer-make-local-variable 'a)
      (emacs-buffer-make-local-variable 'b)
      (emacs-buffer-kill-all-local-variables)
      (should (null (emacs-buffer-buffer-local-variables b))))))

;;;; B. text-property MVP (7 tests)

(ert-deftest emacs-buffer-put-and-get-text-property ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "tp")))
      (nelisp-ec-set-buffer b)
      (nelisp-ec-insert "hello world")
      (emacs-buffer-put-text-property 1 6 'face 'bold)
      (should (eq 'bold (emacs-buffer-get-text-property 1 'face)))
      (should (eq 'bold (emacs-buffer-get-text-property 5 'face)))
      ;; outside range
      (should (null (emacs-buffer-get-text-property 6 'face)))
      (should (null (emacs-buffer-get-text-property 11 'face))))))

(ert-deftest emacs-buffer-put-text-property-overwrites ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "tp")))
      (nelisp-ec-set-buffer b)
      (nelisp-ec-insert "abcdef")
      (emacs-buffer-put-text-property 1 7 'face 'bold)
      (emacs-buffer-put-text-property 3 5 'face 'italic)
      (should (eq 'bold (emacs-buffer-get-text-property 1 'face)))
      (should (eq 'italic (emacs-buffer-get-text-property 3 'face)))
      (should (eq 'italic (emacs-buffer-get-text-property 4 'face)))
      (should (eq 'bold (emacs-buffer-get-text-property 5 'face)))
      (should (eq 'bold (emacs-buffer-get-text-property 6 'face))))))

(ert-deftest emacs-buffer-add-text-properties-merges ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "tp")))
      (nelisp-ec-set-buffer b)
      (nelisp-ec-insert "hello")
      (emacs-buffer-add-text-properties 1 6 '(face bold))
      (emacs-buffer-add-text-properties 1 6 '(weight 700))
      (should (eq 'bold (emacs-buffer-get-text-property 1 'face)))
      (should (= 700 (emacs-buffer-get-text-property 1 'weight))))))

(ert-deftest emacs-buffer-remove-text-properties-drops-keys ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "tp")))
      (nelisp-ec-set-buffer b)
      (nelisp-ec-insert "hello")
      (emacs-buffer-add-text-properties 1 6 '(face bold weight 700))
      (emacs-buffer-remove-text-properties 1 6 '(face))
      (should (null (emacs-buffer-get-text-property 1 'face)))
      (should (= 700 (emacs-buffer-get-text-property 1 'weight))))))

(ert-deftest emacs-buffer-remove-text-properties-accepts-plist ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "tp")))
      (nelisp-ec-set-buffer b)
      (nelisp-ec-insert "hello")
      (emacs-buffer-add-text-properties 1 6 '(face bold weight 700))
      ;; Emacs-style plist (PROP VALUE PROP VALUE ...) interpreted as
      ;; "drop these property names regardless of values"
      (emacs-buffer-remove-text-properties 1 6 '(face nil weight nil))
      (should (null (emacs-buffer-get-text-property 1 'face)))
      (should (null (emacs-buffer-get-text-property 1 'weight))))))

(ert-deftest emacs-buffer-text-property-at-returns-plist ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "tp")))
      (nelisp-ec-set-buffer b)
      (nelisp-ec-insert "hello")
      (emacs-buffer-add-text-properties 1 6 '(face bold weight 700))
      (let ((pl (emacs-buffer-text-property-at 1)))
        (should (equal 'bold (plist-get pl 'face)))
        (should (= 700 (plist-get pl 'weight)))))))

(ert-deftest emacs-buffer-put-text-property-rejects-empty-range ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "tp")))
      (nelisp-ec-set-buffer b)
      (nelisp-ec-insert "x")
      (should-error (emacs-buffer-put-text-property 1 1 'face 'bold)
                    :type 'nelisp-ec-args-out-of-range))))

;;;; C. undo system (6 tests)

(ert-deftest emacs-buffer-undo-list-defaults-empty ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "u")))
      (should (null (emacs-buffer-buffer-undo-list b))))))

(ert-deftest emacs-buffer-buffer-disable-and-enable-undo ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "u")))
      (emacs-buffer-buffer-disable-undo b)
      (should (eq t (emacs-buffer-buffer-undo-list b)))
      (emacs-buffer-buffer-enable-undo b)
      (should (null (emacs-buffer-buffer-undo-list b))))))

(ert-deftest emacs-buffer-undo-boundary-pushes-nil ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "u")))
      (nelisp-ec-set-buffer b)
      (emacs-buffer-undo-boundary)
      (should (equal '(nil) (emacs-buffer-buffer-undo-list b))))))

(ert-deftest emacs-buffer-undo-roundtrip-insertion ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "u")))
      (nelisp-ec-set-buffer b)
      (nelisp-ec-insert "hello")
      (emacs-buffer-record-insertion 1 6 b)
      (should (equal "hello" (nelisp-ec-buffer-string)))
      (emacs-buffer-undo b)
      (should (string-empty-p (nelisp-ec-buffer-string))))))

(ert-deftest emacs-buffer-undo-roundtrip-deletion ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "u")))
      (nelisp-ec-set-buffer b)
      (nelisp-ec-insert "hello")
      ;; simulate a deletion of "ell" at position 2
      (let ((deleted (nelisp-ec-buffer-substring 2 5)))
        (nelisp-ec-delete-region 2 5)
        (emacs-buffer-record-deletion deleted 2 b))
      (should (equal "ho" (nelisp-ec-buffer-string)))
      (emacs-buffer-undo b)
      (should (equal "hello" (nelisp-ec-buffer-string))))))

(ert-deftest emacs-buffer-undo-disabled-signals ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "u")))
      (emacs-buffer-buffer-disable-undo b)
      (should-error (emacs-buffer-undo b)
                    :type 'emacs-buffer-undo-disabled))))

;;;; D. modification tracking (4 tests)

(ert-deftest emacs-buffer-modified-p-tracks-edits ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "m")))
      (nelisp-ec-set-buffer b)
      (should-not (emacs-buffer-buffer-modified-p))
      (nelisp-ec-insert "x")
      (should (emacs-buffer-buffer-modified-p))
      (emacs-buffer-set-buffer-modified-p nil)
      (should-not (emacs-buffer-buffer-modified-p)))))

(ert-deftest emacs-buffer-restore-buffer-modified-p ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "m")))
      (nelisp-ec-set-buffer b)
      (emacs-buffer-restore-buffer-modified-p t)
      (should (emacs-buffer-buffer-modified-p))
      (emacs-buffer-restore-buffer-modified-p nil)
      (should-not (emacs-buffer-buffer-modified-p)))))

(ert-deftest emacs-buffer-chars-modified-tick-monotonic ()
  (emacs-buffer-test--with-fresh-world
    (let* ((b (nelisp-ec-generate-new-buffer "m"))
           (t0 (emacs-buffer-buffer-chars-modified-tick b)))
      (emacs-buffer-bump-modified-tick b)
      (emacs-buffer-bump-modified-tick b)
      (should (= (+ t0 2) (emacs-buffer-buffer-chars-modified-tick b))))))

(ert-deftest emacs-buffer-modify-without-undo-restores-on-error ()
  (emacs-buffer-test--with-fresh-world
    (let* ((b (nelisp-ec-generate-new-buffer "m"))
           (ext (emacs-buffer--ensure-ext b)))
      (nelisp-ec-set-buffer b)
      (emacs-buffer-buffer-enable-undo b)  ;; nil
      (ignore-errors
        (emacs-buffer-modify-without-undo
          (should (eq t (emacs-buffer--ext-undo-list ext)))
          (error "boom")))
      ;; undo-list must be restored
      (should (null (emacs-buffer--ext-undo-list ext))))))

;;;; E. additional buffer ops (5 tests)

(ert-deftest emacs-buffer-clone-indirect-buffer-shares-text ()
  (emacs-buffer-test--with-fresh-world
    (let ((base (nelisp-ec-generate-new-buffer "base")))
      (nelisp-ec-set-buffer base)
      (nelisp-ec-insert "shared")
      (let ((clone (emacs-buffer-clone-indirect-buffer "clone" base)))
        (should (eq base (emacs-buffer-buffer-base-buffer clone)))
        (nelisp-ec-set-buffer clone)
        (should (equal "shared" (nelisp-ec-buffer-string)))
        ;; mutate via base, see in clone
        (nelisp-ec-set-buffer base)
        (nelisp-ec-goto-char (1+ (length "shared")))
        (nelisp-ec-insert "!")
        (nelisp-ec-set-buffer clone)
        (should (equal "shared!" (nelisp-ec-buffer-string)))))))

(ert-deftest emacs-buffer-buffer-base-buffer-nil-for-direct ()
  (emacs-buffer-test--with-fresh-world
    (let ((b (nelisp-ec-generate-new-buffer "direct")))
      (should (null (emacs-buffer-buffer-base-buffer b))))))

(ert-deftest emacs-buffer-buffer-list-returns-all ()
  (emacs-buffer-test--with-fresh-world
    (let ((a (nelisp-ec-generate-new-buffer "a"))
          (b (nelisp-ec-generate-new-buffer "b")))
      (let ((all (emacs-buffer-buffer-list)))
        (should (= 2 (length all)))
        (should (memq a all))
        (should (memq b all))))))

(ert-deftest emacs-buffer-buffers-by-mode-filters ()
  (emacs-buffer-test--with-fresh-world
    (let ((a (nelisp-ec-generate-new-buffer "a"))
          (b (nelisp-ec-generate-new-buffer "b")))
      (emacs-buffer-set-default 'major-mode 'text-mode)
      (emacs-buffer-make-local-variable 'major-mode a)
      (emacs-buffer-set-buffer-local-value 'major-mode a 'lisp-mode)
      (emacs-buffer-make-local-variable 'major-mode b)
      (emacs-buffer-set-buffer-local-value 'major-mode b 'text-mode)
      (let ((lisp (emacs-buffer-buffers-by-mode
                   (lambda (buf)
                     (eq 'lisp-mode
                         (emacs-buffer-buffer-local-value 'major-mode buf))))))
        (should (= 1 (length lisp)))
        (should (memq a lisp))))))

(ert-deftest emacs-buffer-generate-new-buffer-name-uniquifies ()
  (emacs-buffer-test--with-fresh-world
    (should (equal "scratch" (emacs-buffer-generate-new-buffer-name "scratch")))
    (nelisp-ec-generate-new-buffer "scratch")
    (should (equal "scratch<2>" (emacs-buffer-generate-new-buffer-name "scratch")))
    (nelisp-ec-generate-new-buffer "scratch")
    (should (equal "scratch<3>" (emacs-buffer-generate-new-buffer-name "scratch")))))

(provide 'emacs-buffer-test)
;;; emacs-buffer-test.el ends here
