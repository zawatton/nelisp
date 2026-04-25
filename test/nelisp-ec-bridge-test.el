;;; nelisp-ec-bridge-test.el --- ERT for nelisp-ec-bridge  -*- lexical-binding: t; -*-

;; T90 — Wave 2 G symbol mapping bridge.  Exercises install / uninstall
;; / status semantics against a synthetic mapping (host Emacs already
;; binds every real Emacs-builtin name, so the production mapping is a
;; full-no-op here -- we use a private fake mapping to drive the
;; install path) and a smoke roundtrip on the production mapping in
;; "host" mode where every entry should land in :host-bound.

(require 'ert)
(require 'nelisp-ec-bridge)

;;; ──────────────────────────────────────────────────────────────────────
;;; Helpers
;;; ──────────────────────────────────────────────────────────────────────

(defmacro nelisp-ec-bridge-test--with-clean-state (&rest body)
  "Run BODY with `nelisp-ec-bridge--installed' temporarily cleared."
  (declare (indent 0) (debug (body)))
  `(let ((nelisp-ec-bridge--installed nil))
     ,@body))

(defmacro nelisp-ec-bridge-test--with-fake-mapping (mapping &rest body)
  "Bind `nelisp-ec-bridge--mapping' to MAPPING, run BODY, then revert.
Uninstall any entries that BODY may have installed before exit so the
host environment stays clean."
  (declare (indent 1) (debug (form body)))
  `(let ((nelisp-ec-bridge--mapping ,mapping)
         (nelisp-ec-bridge--installed nil))
     (unwind-protect (progn ,@body)
       ;; Defensively revert anything still installed.
       (nelisp-ec-bridge-uninstall))))

(defun nelisp-ec-bridge-test--unbound-sym (prefix)
  "Return a freshly-interned symbol named PREFIX-N that is unbound."
  (let ((n 0)
        sym)
    (while (progn
             (setq sym (intern (format "%s-%d" prefix n)))
             (or (fboundp sym) (boundp sym)))
      (setq n (1+ n)))
    sym))

;;; ──────────────────────────────────────────────────────────────────────
;;; §1. Mapping table sanity (the production constant)
;;; ──────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-ec-bridge-mapping-is-non-empty-alist ()
  (should (consp nelisp-ec-bridge--mapping))
  (should (>= (length nelisp-ec-bridge--mapping) 48)) ;; spec: 48 entries
  (dolist (pair nelisp-ec-bridge--mapping)
    (should (symbolp (car pair)))
    (should (symbolp (cdr pair)))
    ;; Every cdr must be a real `nelisp-ec-' symbol that the compat
    ;; modules already define -- this is the static health check.
    (should (string-prefix-p "nelisp-ec-" (symbol-name (cdr pair))))
    (should (fboundp (cdr pair)))))

(ert-deftest nelisp-ec-bridge-mapping-keys-are-unique ()
  (let ((keys (mapcar #'car nelisp-ec-bridge--mapping)))
    (should (= (length keys) (length (delete-dups (copy-sequence keys)))))))

;;; ──────────────────────────────────────────────────────────────────────
;;; §2. Install path -- synthetic mapping with unbound emacs-syms
;;; ──────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-ec-bridge-install-aliases-unbound-syms ()
  "Install must `defalias' an emacs-sym that is currently unbound."
  (let* ((fake-emacs (nelisp-ec-bridge-test--unbound-sym
                      "nelisp-ec-bridge--fake-emacs"))
         (target #'nelisp-ec-current-buffer))
    (nelisp-ec-bridge-test--with-fake-mapping
        `((,fake-emacs . nelisp-ec-current-buffer))
      (should (not (fboundp fake-emacs)))
      (let ((n (nelisp-ec-bridge-install)))
        (should (= 1 n))
        (should (fboundp fake-emacs))
        ;; The alias must be an indirect function pointing at the same
        ;; underlying definition as the target.
        (should (eq (indirect-function fake-emacs)
                    (indirect-function target)))
        (should (member (cons fake-emacs 'nelisp-ec-current-buffer)
                        nelisp-ec-bridge--installed))))))

(ert-deftest nelisp-ec-bridge-install-skips-already-bound ()
  "Install must NOT clobber an emacs-sym that is already `fboundp'.
This is the core property that makes the bridge a no-op inside a host
Emacs (where every entry is already bound)."
  (nelisp-ec-bridge-test--with-clean-state
    (let ((before (symbol-function 'current-buffer))
          (n     (nelisp-ec-bridge-install)))
      ;; Production mapping under host Emacs: every emacs-sym is bound,
      ;; so install must record zero new entries.
      (should (= 0 n))
      (should (null nelisp-ec-bridge--installed))
      ;; And `current-buffer' must still resolve to the host builtin.
      (should (eq (symbol-function 'current-buffer) before)))))

(ert-deftest nelisp-ec-bridge-install-skips-missing-ec-sym ()
  "Install must skip entries whose nelisp-ec-* sym is unbound.
This protects against partial loads (e.g. the fileio extension not yet
required) -- we don't `defalias' to a void function."
  (let* ((fake-emacs (nelisp-ec-bridge-test--unbound-sym
                      "nelisp-ec-bridge--fake-emacs-skip"))
         (fake-ec    (nelisp-ec-bridge-test--unbound-sym
                      "nelisp-ec-bridge--fake-ec-target")))
    (nelisp-ec-bridge-test--with-fake-mapping
        `((,fake-emacs . ,fake-ec))
      (should (not (fboundp fake-emacs)))
      (should (not (fboundp fake-ec)))
      (let ((n (nelisp-ec-bridge-install)))
        (should (= 0 n))
        (should (not (fboundp fake-emacs)))
        (should (null nelisp-ec-bridge--installed))))))

;;; ──────────────────────────────────────────────────────────────────────
;;; §3. Uninstall path -- precise revert
;;; ──────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-ec-bridge-uninstall-restores-unbound-state ()
  "After install + uninstall the synthetic emacs-sym must be unbound again."
  (let ((fake-emacs (nelisp-ec-bridge-test--unbound-sym
                     "nelisp-ec-bridge--fake-emacs-roundtrip")))
    (nelisp-ec-bridge-test--with-fake-mapping
        `((,fake-emacs . nelisp-ec-current-buffer))
      (should (= 1 (nelisp-ec-bridge-install)))
      (should (fboundp fake-emacs))
      (let ((n (nelisp-ec-bridge-uninstall)))
        (should (= 1 n))
        (should (not (fboundp fake-emacs)))
        (should (null nelisp-ec-bridge--installed))))))

(ert-deftest nelisp-ec-bridge-uninstall-leaves-host-bound-alone ()
  "Uninstall must only revert what install actually defaliased.
Since the production mapping installs zero entries inside a host
Emacs, uninstall must touch nothing -- in particular `current-buffer'
must still be the original builtin afterwards."
  (nelisp-ec-bridge-test--with-clean-state
    (nelisp-ec-bridge-install)
    (let ((before (symbol-function 'current-buffer))
          (n     (nelisp-ec-bridge-uninstall)))
      (should (= 0 n))
      (should (eq (symbol-function 'current-buffer) before)))))

;;; ──────────────────────────────────────────────────────────────────────
;;; §4. Roundtrip -- host-style code runs through nelisp-ec-* via alias
;;; ──────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-ec-bridge-roundtrip-fake-buffer-ops ()
  "A synthetic alias for `current-buffer' must dispatch into nelisp-ec.
We bind `nelisp-ec--current-buffer' to a known value and verify that
calling the host-style alias returns the same object as the direct
nelisp-ec-* call -- i.e. control flow really crossed through defalias."
  (let* ((fake-emacs (nelisp-ec-bridge-test--unbound-sym
                      "nelisp-ec-bridge--fake-cb"))
         (probe-buf  (nelisp-ec-generate-new-buffer
                      "nelisp-ec-bridge--probe")))
    (unwind-protect
        (nelisp-ec-bridge-test--with-fake-mapping
            `((,fake-emacs . nelisp-ec-current-buffer))
          (should (= 1 (nelisp-ec-bridge-install)))
          (let ((nelisp-ec--current-buffer probe-buf))
            ;; Call through the alias and through the direct symbol.
            (should (eq probe-buf (funcall fake-emacs)))
            (should (eq probe-buf (nelisp-ec-current-buffer)))))
      ;; Always tidy the registry even on failure.
      (when (nelisp-ec-buffer-p probe-buf)
        (nelisp-ec-kill-buffer probe-buf)))))

;;; ──────────────────────────────────────────────────────────────────────
;;; §5. Status reporter
;;; ──────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-ec-bridge-status-reports-host-bound-on-host-emacs ()
  "Inside host Emacs every production entry should land in :host-bound."
  (nelisp-ec-bridge-test--with-clean-state
    (let ((status (nelisp-ec-bridge-status)))
      (should (= (plist-get status :total)
                 (length nelisp-ec-bridge--mapping)))
      (should (= 0 (plist-get status :installed)))
      (should (= 0 (plist-get status :missing-ec)))
      ;; Host has every Emacs builtin; host-bound = total.
      (should (= (plist-get status :host-bound)
                 (plist-get status :total))))))

(ert-deftest nelisp-ec-bridge-status-tracks-installed-count ()
  "After install on a synthetic mapping :installed must reflect reality."
  (let ((fake-emacs (nelisp-ec-bridge-test--unbound-sym
                     "nelisp-ec-bridge--fake-status")))
    (nelisp-ec-bridge-test--with-fake-mapping
        `((,fake-emacs . nelisp-ec-current-buffer))
      (should (= 0 (plist-get (nelisp-ec-bridge-status) :installed)))
      (nelisp-ec-bridge-install)
      (let ((status (nelisp-ec-bridge-status)))
        (should (= 1 (plist-get status :installed)))
        (should (= 1 (plist-get status :total)))
        (should (= 0 (plist-get status :host-bound)))
        (should (= 0 (plist-get status :missing-ec)))))))

;;; ──────────────────────────────────────────────────────────────────────
;;; §6. Macro forwarding (with-current-buffer is a macro)
;;; ──────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-ec-bridge-defalias-forwards-macros ()
  "`with-current-buffer' is a macro -- defalias must forward macro form.
We synthesize a fake emacs-sym pointing at the real
`nelisp-ec-with-current-buffer' macro and verify that the alias
behaves as a macro at expansion time."
  (let ((fake-emacs (nelisp-ec-bridge-test--unbound-sym
                     "nelisp-ec-bridge--fake-wcb")))
    (nelisp-ec-bridge-test--with-fake-mapping
        `((,fake-emacs . nelisp-ec-with-current-buffer))
      (nelisp-ec-bridge-install)
      (should (fboundp fake-emacs))
      ;; `indirect-function' on a macro returns (macro lambda ...) or a
      ;; cons whose car is the symbol `macro' / a closure tagged as
      ;; macro -- the precise representation varies by Emacs version.
      ;; The portable check: the underlying definition should equal
      ;; (or be eq to) the macro's symbol-function.
      (should (eq (indirect-function fake-emacs)
                  (indirect-function 'nelisp-ec-with-current-buffer))))))

;;; nelisp-ec-bridge-test.el ends here
