;;; emacs-frame-builtins-test.el --- ERT tests for emacs-frame-builtins  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the Layer 2 Emacs frame.c builtin bridge.  Under batch
;; host Emacs the host C builtins remain active (= the bridge's host
;; gate keeps them) so the substrate-direct
;; `emacs-frame-*' API is used for semantic assertions; bridge-shape
;; assertions verify featurep + fboundp parity.

;;; Code:

(require 'ert)
(require 'emacs-frame-builtins)
(require 'cl-lib)

(defmacro emacs-frame-builtins-test--with-fresh-world (&rest body)
  "Run BODY against a clean prefixed-frame registry."
  (declare (indent 0) (debug (body)))
  `(progn
     (emacs-frame-reset)
     (unwind-protect
         (progn ,@body)
       (emacs-frame-reset))))

;;;; A. Load cleanly

(ert-deftest emacs-frame-builtins-test/require-loads-cleanly ()
  (should (featurep 'emacs-frame-builtins))
  (should (featurep 'emacs-frame))
  (dolist (sym '(make-frame framep frame-live-p frame-list
                 selected-frame window-frame
                 delete-frame delete-other-frames
                 frame-width frame-height frame-char-width frame-char-height
                 frame-pixel-width frame-pixel-height
                 set-frame-size set-frame-position
                 frame-parameter frame-parameters
                 set-frame-parameter modify-frame-parameters
                 frame-visible-p make-frame-visible make-frame-invisible
                 raise-frame lower-frame select-frame frame-focus
                 frame-windows display-pixel-width display-pixel-height
                 with-selected-frame))
    (should (fboundp sym))))

;;;; B. Substrate-direct: make-frame produces a framep object

(ert-deftest emacs-frame-builtins-test/prefixed-make-frame-produces-framep ()
  (emacs-frame-builtins-test--with-fresh-world
    (let ((f (emacs-frame-make-frame)))
      (should (emacs-frame-framep f))
      (should (emacs-frame-frame-live-p f)))))

;;;; C. Substrate-direct: frame-list contains the made frame

(ert-deftest emacs-frame-builtins-test/prefixed-frame-list-contains-made-frame ()
  (emacs-frame-builtins-test--with-fresh-world
    (let ((f (emacs-frame-make-frame)))
      (should (memq f (emacs-frame-frame-list))))))

;;;; D. Substrate-direct: parameter set/get roundtrip

(ert-deftest emacs-frame-builtins-test/parameter-roundtrip-via-prefixed ()
  (emacs-frame-builtins-test--with-fresh-world
    (let ((f (emacs-frame-make-frame)))
      (emacs-frame-set-frame-parameter f 'background-color "black")
      (should (equal "black"
                     (emacs-frame-frame-parameter f 'background-color))))))

;;;; E. Substrate-direct: modify-frame-parameters bulk update

(ert-deftest emacs-frame-builtins-test/modify-frame-parameters-bulk-via-prefixed ()
  (emacs-frame-builtins-test--with-fresh-world
    (let ((f (emacs-frame-make-frame)))
      (emacs-frame-modify-frame-parameters f '((cursor-type . box)
                                               (foo . bar)))
      (should (eq 'box
                  (emacs-frame-frame-parameter f 'cursor-type)))
      (should (eq 'bar
                  (emacs-frame-frame-parameter f 'foo))))))

;;;; F. Substrate-direct: delete-frame removes from list

(ert-deftest emacs-frame-builtins-test/delete-frame-via-prefixed-removes-from-list ()
  (emacs-frame-builtins-test--with-fresh-world
    (let ((f1 (emacs-frame-make-frame))
          (f2 (emacs-frame-make-frame)))
      (should (memq f1 (emacs-frame-frame-list)))
      (should (memq f2 (emacs-frame-frame-list)))
      (emacs-frame-delete-frame f2)
      (should-not (memq f2 (emacs-frame-frame-list)))
      (should (memq f1 (emacs-frame-frame-list))))))

;;;; G. Substrate-direct: selected-frame is framep

(ert-deftest emacs-frame-builtins-test/prefixed-selected-frame-is-framep ()
  (emacs-frame-builtins-test--with-fresh-world
    (should (emacs-frame-framep (emacs-frame-selected-frame)))))

(ert-deftest emacs-frame-builtins-test/extended-prefixed-surface-is-usable ()
  (emacs-frame-builtins-test--with-fresh-world
    (let ((f (emacs-frame-make-frame '((width . 90) (height . 30)))))
      (should (= 90 (emacs-frame-frame-width f)))
      (should (= 30 (emacs-frame-frame-height f)))
      (emacs-frame-set-frame-size f 100 40)
      (should (= 100 (emacs-frame-frame-width f)))
      (should (= 40 (emacs-frame-frame-height f)))
      (emacs-frame-make-frame-invisible f)
      (should-not (emacs-frame-frame-visible-p f))
      (emacs-frame-make-frame-visible f)
      (should (emacs-frame-frame-visible-p f))
      (emacs-frame-select-frame f)
      (should (eq f (emacs-frame-selected-frame)))
      (should (= emacs-frame--display-cols
                 (emacs-frame-display-pixel-width))))))

(ert-deftest emacs-frame-builtins-test/bridge-overwrites-standalone-stubs-in-source ()
  ;; The bridge must replace `emacs-stub.el' sentinels in standalone
  ;; NeLisp, while preserving host Emacs builtins.
  (should (fboundp 'emacs-frame-builtins--install-function-p))
  (should-not (emacs-frame-builtins--install-function-p 'make-frame))
  (let ((emacs-version 'nelisp--unbound-marker))
    (should (emacs-frame-builtins--install-function-p 'make-frame)))
  (let* ((file (locate-library "emacs-frame-builtins"))
         (file (if (and file (string-match-p "\\.elc\\'" file))
                   (concat (substring file 0 (- (length file) 1)))
                 file)))
    (should (and file (file-exists-p file)))
    (with-temp-buffer
      (insert-file-contents file)
      (dolist (sym '(make-frame framep frame-live-p frame-list
                     selected-frame window-frame delete-frame
                     delete-other-frames frame-width frame-height
                     set-frame-size frame-visible-p select-frame
                     frame-windows display-pixel-width display-pixel-height
                     with-selected-frame))
        (goto-char (point-min))
        (should (search-forward
                 (format "(when (emacs-frame-builtins--install-function-p '%s)" sym)
                 nil t))))))

;;;; J. `with-selected-frame' (T87) -- macro shape parity with host `subr.el'
;;
;; Under host Emacs `with-selected-frame' is already a real C-adjacent
;; `subr.el' macro, so the `unless'-style install guard in
;; `emacs-frame-builtins.el' never lets our port install under host
;; ERT -- there is nothing to shadow.  To still exercise the actual
;; ported code (not host's own definition), read the `(defmacro
;; with-selected-frame ...)' form straight out of the source file,
;; `eval' it under a private test-only name, and compare its
;; macroexpansion against the known-good shape (verified against a
;; clean `emacs -Q --batch', Emacs 31.1, during T87).

(defun emacs-frame-builtins-test--extract-defmacro (file name)
  "Read the `(defmacro NAME ...)' form out of FILE, renamed to a gensym.
Returns (RENAMED-SYMBOL . FORM)."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (should (re-search-forward (format "(defmacro %s " (regexp-quote name)) nil t))
    (goto-char (match-beginning 0))
    (let* ((start (point))
           (end (progn (forward-sexp) (point)))
           (form (read (buffer-substring-no-properties start end)))
           (renamed (make-symbol (format "emacs-frame-builtins-test--%s" name))))
      (setcar (cdr form) renamed)
      (cons renamed form))))

(ert-deftest emacs-frame-builtins-test/with-selected-frame-macroexpansion-matches-host-shape ()
  (let* ((file (locate-library "emacs-frame-builtins"))
         (file (if (and file (string-match-p "\\.elc\\'" file))
                   (concat (substring file 0 (- (length file) 1)))
                 file)))
    (should (and file (file-exists-p file)))
    (let* ((extracted (emacs-frame-builtins-test--extract-defmacro
                       file "with-selected-frame"))
           (sym (car extracted))
           (form (cdr extracted)))
      (eval form t)
      (let* ((expansion (macroexpand-1 (list sym 'my-frame '(foo) '(bar))))
             ;; Structural shape: (let ((OLDF (selected-frame)) (OLDB
             ;; (current-buffer))) (unwind-protect (progn (select-frame
             ;; my-frame 'norecord) (foo) (bar)) (when (frame-live-p OLDF)
             ;; (select-frame OLDF 'norecord)) (when (buffer-live-p OLDB)
             ;; (set-buffer OLDB))) -- verified against host Emacs 31.1's
             ;; own `(macroexpand-1 '(with-selected-frame my-frame (foo)
             ;; (bar)))'.
             (let-bindings (cadr expansion))
             (body (cddr expansion)))
        (should (eq 'let (car expansion)))
        (should (= 2 (length let-bindings)))
        (should (equal '(selected-frame) (cadr (nth 0 let-bindings))))
        (should (equal '(current-buffer) (cadr (nth 1 let-bindings))))
        (should (= 1 (length body)))
        (let ((up (car body)))
          (should (eq 'unwind-protect (car up)))
          (should (equal '(progn (select-frame my-frame 'norecord) (foo) (bar))
                         (nth 1 up)))
          (should (equal (list 'when (list 'frame-live-p (nth 0 (nth 0 let-bindings)))
                               (list 'select-frame (nth 0 (nth 0 let-bindings)) ''norecord))
                         (nth 2 up)))
          (should (equal (list 'when (list 'buffer-live-p (nth 0 (nth 1 let-bindings)))
                               (list 'set-buffer (nth 0 (nth 1 let-bindings))))
                         (nth 3 up))))
        ;; Behavior: BODY runs with FRAME selected, restores after.  The
        ;; frame object is spliced in as literal quoted data (not a
        ;; free variable reference) so a fresh top-level `eval' -- with
        ;; no access to this `let*''s lexical scope -- can still see it.
        (let* ((f0 (selected-frame))
               (probe (eval `(lambda ()
                               (,sym ',f0 (cons (eq (selected-frame) ',f0) 42)))
                            t)))
          (should (equal '(t . 42) (funcall probe)))
          (should (eq f0 (selected-frame))))))))

;;;; H. Idempotence

(ert-deftest emacs-frame-builtins-test/require-is-idempotent ()
  (let ((before-make-frame      (symbol-function 'make-frame))
        (before-framep          (symbol-function 'framep))
        (before-frame-parameter (symbol-function 'frame-parameter))
        (before-frame-width     (symbol-function 'frame-width)))
    (require 'emacs-frame-builtins)
    (should (eq before-make-frame      (symbol-function 'make-frame)))
    (should (eq before-framep          (symbol-function 'framep)))
    (should (eq before-frame-parameter (symbol-function 'frame-parameter)))
    (should (eq before-frame-width     (symbol-function 'frame-width)))))

;;;; I. tool-bar.el wiring (T62)

;; `tool-bar-local-item' had an fboundp-guarded on-demand loader only
;; inside `src/nelisp-emacs-magit-bridge.el' (not on the default boot
;; path), so it was void for `geiser-guile'.  This covers the wiring:
;; requiring `emacs-frame-builtins' (already on the default boot path)
;; must force-load the vendored `tool-bar.el' and provide it.
(ert-deftest emacs-frame-builtins-test/tool-bar-wired-transitively ()
  (should (featurep 'tool-bar))
  (should (fboundp 'tool-bar-local-item)))

(provide 'emacs-frame-builtins-test)

;;; emacs-frame-builtins-test.el ends here
