;;; nemacs-wrap-init-test.el --- host-only tests for wrap-init lowering  -*- lexical-binding: t; -*-

;;; Commentary:

;; nemacs init loader reconcile, Phase 4 (define-minor-mode lowering).
;; `scripts/nemacs-wrap-init.el' is a host-side generator: `nemacs-wrap-init--lower'
;; runs entirely under a full host Emacs (it needs `easy-mmode'/`inline'/`cl-lib'
;; to macroexpand, but produces plain fset/setq/if primitives as output), so
;; these tests need no standalone NeLisp reader and belong in `make test-fast'
;; alongside the other host-only ERT suites.  The GUI-bridge integration tests
;; that actually replay the lowered output through the standalone reader live
;; in `test/nemacs-gui-file-bridge-runtime-test.el' (M15/M19-2, reader-gated).

;;; Code:

(require 'ert)
(require 'cl-lib)

(defconst nemacs-wrap-init-test--source
  (expand-file-name
   "../scripts/nemacs-wrap-init.el"
   (file-name-directory (or load-file-name buffer-file-name))))

(load nemacs-wrap-init-test--source nil t)

(defun nemacs-wrap-init-test--lowered-heads (lowered)
  "Return the list of top-level heads of LOWERED (a `progn' form)."
  (should (eq (car lowered) 'progn))
  (mapcar #'car-safe (cdr lowered)))

(ert-deftest nemacs-wrap-init-test/define-minor-mode-non-global-lowers-to-fset ()
  "A plain (non-`:global') `define-minor-mode' lowers its mode command to
`fset' and its mode/hook/keymap variables to boundp-guarded `setq' --
no raw `defun'/`defvar'/`defalias' should remain anywhere in the output."
  (let* ((form '(define-minor-mode probe-local-mode
                  "Toggle probe-local mode."
                  :lighter " ProbeLocal"
                  (if probe-local-mode (ignore) (ignore))))
         (lowered (nemacs-wrap-init--lower form))
         (printed (prin1-to-string lowered)))
    (should (eq (car lowered) 'progn))
    (should-not (string-match-p "(defun " printed))
    (should-not (string-match-p "(defvar " printed))
    (should-not (string-match-p "(defalias " printed))
    (should (string-match-p "(fset 'probe-local-mode" printed))
    (should (string-match-p "(setq probe-local-mode nil)" printed))))

(ert-deftest nemacs-wrap-init-test/define-minor-mode-global-group-round-trips ()
  "A `:global t :group ...' `define-minor-mode' -- the exact keyword shape
`marginalia-mode'/`which-key-mode' use -- macroexpands its mode variable
through `custom-declare-variable' rather than plain `defvar'.  Lowering
must still leave the mode function `fboundp' and the mode variable
correctly initialized, and evaluating the lowered output end to end
(fset + the eval-based custom-declare-variable unwrap) must produce a
working toggle command."
  (let* ((form '(define-minor-mode nemacs-wrap-init-test--probe-mode
                  "Toggle probe mode."
                  :global t :group 'nemacs-wrap-init-test
                  :lighter " Probe"
                  (setq nemacs-wrap-init-test--probe-log
                        (cons nemacs-wrap-init-test--probe-mode
                              nemacs-wrap-init-test--probe-log))))
         (lowered (nemacs-wrap-init--lower form)))
    (should (eq (car lowered) 'progn))
    ;; No unlowered macro-shaped node should remain: `custom-declare-variable'
    ;; only appears inside the `eval' escape hatch's quoted data, `defalias'
    ;; only inside the fset lambda hoist -- neither is itself a live node.
    (let ((printed (prin1-to-string lowered)))
      (should-not (string-match-p "^(defcustom \\|(defcustom " printed))
      (should (string-match-p "(fset 'nemacs-wrap-init-test--probe-mode" printed)))
    (unwind-protect
        (progn
          (defvar nemacs-wrap-init-test--probe-log nil)
          (setq nemacs-wrap-init-test--probe-log nil)
          (eval lowered t)
          (should (fboundp 'nemacs-wrap-init-test--probe-mode))
          (should (boundp 'nemacs-wrap-init-test--probe-mode))
          (should (null nemacs-wrap-init-test--probe-mode))
          (nemacs-wrap-init-test--probe-mode 1)
          (should (eq nemacs-wrap-init-test--probe-mode t))
          (should (equal nemacs-wrap-init-test--probe-log '(t)))
          (nemacs-wrap-init-test--probe-mode -1)
          (should (null nemacs-wrap-init-test--probe-mode))
          (should (equal nemacs-wrap-init-test--probe-log '(nil t))))
      (when (fboundp 'nemacs-wrap-init-test--probe-mode)
        (fmakunbound 'nemacs-wrap-init-test--probe-mode))
      (when (boundp 'nemacs-wrap-init-test--probe-mode)
        (makunbound 'nemacs-wrap-init-test--probe-mode))
      (when (boundp 'nemacs-wrap-init-test--probe-log)
        (makunbound 'nemacs-wrap-init-test--probe-log)))))

(ert-deftest nemacs-wrap-init-test/define-globalized-minor-mode-lowers-without-error ()
  "`define-globalized-minor-mode' (flycheck/magit-wip/evil-surround style)
macroexpands through the same defcustom/defalias shape as a `:global'
`define-minor-mode'; lowering it must not error and must leave the
globalized mode command `fboundp' after evaluating the lowered output."
  (let* ((form '(define-globalized-minor-mode nemacs-wrap-init-test--global-probe-mode
                  nemacs-wrap-init-test--probe-mode
                  (lambda () (ignore))))
         (lowered (nemacs-wrap-init--lower form)))
    (should (eq (car lowered) 'progn))
    (unwind-protect
        (progn
          (eval lowered t)
          (should (fboundp 'nemacs-wrap-init-test--global-probe-mode))
          (should (boundp 'nemacs-wrap-init-test--global-probe-mode)))
      (when (fboundp 'nemacs-wrap-init-test--global-probe-mode)
        (fmakunbound 'nemacs-wrap-init-test--global-probe-mode))
      (when (boundp 'nemacs-wrap-init-test--global-probe-mode)
        (makunbound 'nemacs-wrap-init-test--global-probe-mode))
      (when (fboundp 'nemacs-wrap-init-test--probe-mode)
        (fmakunbound 'nemacs-wrap-init-test--probe-mode))
      (when (boundp 'nemacs-wrap-init-test--probe-mode)
        (makunbound 'nemacs-wrap-init-test--probe-mode)))))

(ert-deftest nemacs-wrap-init-test/bare-defalias-lowers-to-fset ()
  "A bare top-level `defalias' (as a macroexpansion like `define-minor-mode'
leaves behind, or as a raw user form) lowers to `fset' directly, the same
dialect `defun' and `define-inline' already use -- so the bridge runtime
never has to define `defalias' itself."
  (let* ((form '(defalias 'nemacs-wrap-init-test--aliased
                  #'(lambda (x) (declare (indent 0)) (interactive) (1+ x))))
         (lowered (nemacs-wrap-init--lower form)))
    (should (equal lowered
                   '(fset 'nemacs-wrap-init-test--aliased
                          (lambda (x) (1+ x)))))))

(ert-deftest nemacs-wrap-init-test/nested-progn-from-macroexpansion-is-lowered ()
  "A `progn' produced by macroexpanding some other macro (not just
`define-minor-mode') still gets its nested defun/defvar/defconst nodes
lowered -- the generic `progn' recursion is not special-cased to only
fire from the `define-minor-mode' clause."
  (let* ((form '(progn
                  (defvar nemacs-wrap-init-test--nested-var 7)
                  (defun nemacs-wrap-init-test--nested-fn () 9)
                  (progn (defconst nemacs-wrap-init-test--nested-const 11))))
         (lowered (nemacs-wrap-init--lower form))
         (printed (prin1-to-string lowered)))
    (should (eq (car lowered) 'progn))
    (should-not (string-match-p "(defvar " printed))
    (should-not (string-match-p "(defun " printed))
    (should-not (string-match-p "(defconst " printed))
    (should (string-match-p "(setq nemacs-wrap-init-test--nested-var 7)" printed))
    (should (string-match-p "(fset 'nemacs-wrap-init-test--nested-fn" printed))
    (should (string-match-p
             "(setq nemacs-wrap-init-test--nested-const 11)" printed))))

(ert-deftest nemacs-wrap-init-test/plain-defun-defvar-require-unaffected ()
  "Regression guard for the Phase 4 changes: ordinary top-level `defun',
`defvar', and an unresolved `require' still lower exactly as before."
  (should (equal (nemacs-wrap-init--lower '(defvar my-var 1))
                 '(if (boundp 'my-var) nil (setq my-var 1))))
  (should (equal (nemacs-wrap-init--lower '(defconst my-const 2))
                 '(setq my-const 2)))
  (let ((lowered (nemacs-wrap-init--lower '(defun my-fn (x) (+ x 1)))))
    (should (eq (car lowered) 'fset))
    (should (equal (nth 1 lowered) ''my-fn)))
  (should (equal (nemacs-wrap-init--lower '(require 'no-such-feature-anywhere))
                 '(nemacs-init--require-unresolved))))

(provide 'nemacs-wrap-init-test)

;;; nemacs-wrap-init-test.el ends here
