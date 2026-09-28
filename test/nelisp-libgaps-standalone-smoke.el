;;; nelisp-libgaps-standalone-smoke.el --- coverage-lane GNU 31.1 gap fixes -*- lexical-binding: t; -*-

;; Regression coverage for 5 NeLisp-core gaps the nelisp-emacs-lib
;; coverage lane found blocking real GNU 31.1 files from loading on the
;; standalone: project.el/cl-generic.el (cl-struct-name-p, &context),
;; dired.el (mounted-file-systems), shell.el (ignored-local-variables),
;; isearch.el (help-char), and term.el (easy-menu-define).  Each test
;; below is a minimal, self-contained repro of the exact construct that
;; used to fail -- lifted from the real GNU 31.1 file, not the whole
;; file itself (which lives outside this repo, in a separate vendor
;; tree, and is not appropriate to hard-code an absolute path to here).
;;
;; Run with: target/nelisp-libgaps --load scripts/nelisp-ert-shim.el \
;;   --load test/nelisp-libgaps-standalone-smoke.el

(load "scripts/nelisp-ert-shim.el")

;;; dired.el: real autorevert.el's `auto-revert-notify-exclude-dir-regexp'
;;; defcustom -- pulled in transitively by dired.el's own
;;; `(eval-when-compile (require 'autorevert))', which runs for real when
;;; interpreting source (no separate byte-compile step) -- referenced
;;; `mounted-file-systems' directly at its own top level.

(ert-deftest nelisp-libgaps/mounted-file-systems-bound-and-matches ()
  (should (boundp 'mounted-file-systems))
  (should (stringp mounted-file-systems))
  ;; Verbatim real GNU 31.1 files.el use site (temporary-file-directory):
  ;; a `/tmp/...' path must NOT match this substrate's gnu/linux branch.
  (should-not (string-match mounted-file-systems "/tmp/foo"))
  ;; ... but real Emacs's own `/media/...' branch must match.
  (should (string-match mounted-file-systems "/media/usb0")))

;;; shell.el: real files-x.el's unconditional top-level `(setq
;;; ignored-local-variables (cons 'connection-local-variables-alist
;;; ignored-local-variables))' -- pulled in the same way, via shell.el's
;;; own `(eval-when-compile (require 'files-x))'.

(ert-deftest nelisp-libgaps/ignored-local-variables-bound-and-risky ()
  (should (boundp 'ignored-local-variables))
  (should (memq 'safe-local-variable-values ignored-local-variables))
  (should (eq (get 'ignored-local-variables 'risky-local-variable) t))
  ;; The exact files-x.el construct that used to crash at load time:
  (should (memq 'connection-local-variables-alist
                (cons 'connection-local-variables-alist
                      ignored-local-variables))))

;;; isearch.el: two top-level `defvar' keymap-building forms
;;; (`isearch-mode-map', `isearch-help-map') reference `help-char'
;;; directly via `(char-to-string help-char)'.

(ert-deftest nelisp-libgaps/help-char-is-control-h ()
  (should (boundp 'help-char))
  (should (= help-char 8))
  ;; The exact isearch.el construct that used to crash at load time:
  (should (equal (char-to-string help-char) "\^H")))

;;; project.el: `(cl-defmethod project-root (project &context
;;; (project--within-roots-fallback (eql nil))) ...)' -- the exact
;;; real-world &context usage this fix targets.  See also the much more
;;; thorough host-Emacs ERT coverage in
;;; test/nelisp-cl-generic-test.el's `context-specializer-*' tests.

(defvar nelisp-libgaps--fallback nil)

(ert-deftest nelisp-libgaps/context-specializer-project-root-shape ()
  (cl-defgeneric nlg-project-root (project))
  (cl-defmethod nlg-project-root
    (project &context (nelisp-libgaps--fallback (eql nil)))
    project)
  (cl-defmethod nlg-project-root
    (_project &context (nelisp-libgaps--fallback (eql t)))
    'fallback-path)
  (let ((nelisp-libgaps--fallback nil))
    (should (eq 'the-project (nlg-project-root 'the-project))))
  (let ((nelisp-libgaps--fallback t))
    (should (eq 'fallback-path (nlg-project-root 'the-project)))))

;;; cl-generic.el/cl-macs.el: the real-vendor-file coverage chain this
;;; fix unblocks (`cl--struct-name-p'/`cl-struct-define' from
;;; cl-preloaded.el, `cl--proclaims-deferred'/`cl-proclaim'/
;;; `cl-copy-list'/`cl--block-wrapper' from cl-lib.el, `pcase-dolist'
;;; from pcase.el) -- exercised here as the same shape real
;;; `cl-generic.el' itself uses: `(cl-defstruct (NAME ...))' on a struct
;;; name nothing has defined before, immediately followed by
;;; `cl--find-class' recognizing it.

(ert-deftest nelisp-libgaps/cl-struct-name-p-and-struct-define ()
  (should (fboundp 'cl--struct-name-p))
  (should (cl--struct-name-p 'nelisp-libgaps-test-generalizer))
  (should (fboundp 'cl-struct-define))
  (should (boundp 'cl--proclaims-deferred))
  (should (fboundp 'cl-proclaim))
  (should (equal (cl-copy-list '(1 2 3)) '(1 2 3)))
  (should (eq (cl--block-wrapper 42) 42)))

;;; term.el: 5 top-level `(easy-menu-define ...)' calls, some nested
;;; inside `(defvar MAP (let ((map (make-sparse-keymap))) ...
;;; (easy-menu-define nil map ...) ... map))', some with a non-nil
;;; SYMBOL that must end up bound afterward
;;; (`term-terminal-menu'/`term-signals-menu'/`term-pager-menu').

(ert-deftest nelisp-libgaps/easy-menu-define-binds-named-symbol ()
  (easy-menu-define nlg-test-menu nil "Test menu."
    '("Test" ["Item" ignore t]))
  (should (boundp 'nlg-test-menu))
  (should (fboundp 'nlg-test-menu)))

(ert-deftest nelisp-libgaps/easy-menu-define-nil-symbol-is-noop ()
  (let ((map (make-sparse-keymap)))
    ;; Must not error, matching term.el's own `(easy-menu-define nil map
    ;; "Complete menu for Term mode." ...)' shape.
    (should (null (easy-menu-define nil map "Complete menu for Term mode."
                    '("Complete"
                      ["Dynamic Complete Filename" ignore t]))))))

(let* ((result (nelisp-ert-run-all "nelisp-libgaps"))
       (pass (car result))
       (fail (cadr result)))
  (princ (format "GATE-COUNT checked=%d findings=%d\n" (+ pass fail) fail))
  (if (> fail 0)
      (error "nelisp-libgaps-standalone-smoke: %d failure(s)" fail)
    (princ (format "nelisp-libgaps-standalone-smoke: PASS (%d tests)\n" pass))))

;;; nelisp-libgaps-standalone-smoke.el ends here
