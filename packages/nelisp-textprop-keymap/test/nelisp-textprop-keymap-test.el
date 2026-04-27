;;; nelisp-textprop-keymap-test.el --- ERT for keymap chain inject  -*- lexical-binding: t; -*-

;; T146 / Phase 9c.3 — exercises `nelisp-textprop-keymap' per Doc 41
;; §3.3.3 ERT gate (+18 minimum, this suite ships 30).
;;
;; Layout:
;;   §A. contract version + install/uninstall + installed-p
;;   §B. provider registration (validate / set / get / clear)
;;   §C. opt-in invariant — injection nil = Doc 34 7-stage byte-identical
;;   §D. precedence-at slot ordering (9-stage when injection on)
;;   §E. resolve-at composite keymap
;;   §F. higher-precedence-than-textprop slots beat textprop
;;   §G. lower-precedence slots (current-local, global) lose to textprop
;;   §H. overlay slot beats textprop slot (cross-slot LOCKED)
;;   §I. provider error semantics (bad return / bad set)
;;   §J. 9b regression-free (= text-property keymap absent → chain unchanged)

(require 'ert)
(require 'cl-lib)
(require 'nelisp-textprop-keymap)

;;; ─────────────────────────────────────────────────────────────────────
;;; Test fixtures
;;; ─────────────────────────────────────────────────────────────────────

(defmacro nelisp-tp-keymap-test--with-clean-state (&rest body)
  "Run BODY with the keymap module reset to its DEFAULT state.
Saves + restores: injection flag, both providers, plus the
chain special variables we manipulate in tests."
  (declare (indent 0) (debug t))
  `(let ((nelisp-textprop-keymap-with-injection nil)
         (saved-overlay nelisp-textprop-keymap--overlay-provider)
         (saved-textprop nelisp-textprop-keymap--textprop-provider)
         (overriding-terminal-local-map nil)
         (overriding-local-map nil)
         (minor-mode-overriding-map-alist nil)
         (minor-mode-map-alist nil)
         (emulation-mode-map-alists nil))
     (unwind-protect
         (progn
           (setq nelisp-textprop-keymap--overlay-provider nil)
           (setq nelisp-textprop-keymap--textprop-provider nil)
           ,@body)
       (setq nelisp-textprop-keymap--overlay-provider saved-overlay)
       (setq nelisp-textprop-keymap--textprop-provider saved-textprop))))

(defun nelisp-tp-keymap-test--mk-keymap (tag)
  "Return a fresh sparse keymap tagged via `:nelisp-tag' property TAG.
The tag lets tests assert *which* keymap came back without
depending on `eq' identity."
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "TAG") `(lambda () (interactive) ',tag))
    km))

(defun nelisp-tp-keymap-test--keymap-tag (km)
  "Return the TAG previously planted by `--mk-keymap', or nil."
  (when (keymapp km)
    (let ((binding (lookup-key km (kbd "TAG"))))
      (when (and binding (functionp binding))
        (let ((body (and (listp binding) (car (last binding)))))
          (when (and (consp body) (eq (car body) 'quote))
            (cadr body)))))))

;;; ─────────────────────────────────────────────────────────────────────
;;; §A. contract version + install / uninstall
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-tp-keymap-contract-version-locked ()
  (should (= nelisp-textprop-keymap-chain-inject-contract-version 1)))

(ert-deftest nelisp-tp-keymap-install-flips-flag ()
  (nelisp-tp-keymap-test--with-clean-state
    (should-not (nelisp-textprop-keymap-installed-p))
    (should (eq (nelisp-textprop-keymap-install) t))
    (should (nelisp-textprop-keymap-installed-p))
    (should (eq (nelisp-textprop-keymap-uninstall) t))
    (should-not (nelisp-textprop-keymap-installed-p))))

(ert-deftest nelisp-tp-keymap-install-idempotent ()
  (nelisp-tp-keymap-test--with-clean-state
    (nelisp-textprop-keymap-install)
    (nelisp-textprop-keymap-install)
    (should (nelisp-textprop-keymap-installed-p))
    (nelisp-textprop-keymap-uninstall)
    (nelisp-textprop-keymap-uninstall)
    (should-not (nelisp-textprop-keymap-installed-p))))

(ert-deftest nelisp-tp-keymap-uninstall-preserves-providers ()
  (nelisp-tp-keymap-test--with-clean-state
    (let ((fn (lambda (_p _b) nil)))
      (nelisp-textprop-keymap-set-overlay-provider fn)
      (nelisp-textprop-keymap-set-textprop-provider fn)
      (nelisp-textprop-keymap-install)
      (nelisp-textprop-keymap-uninstall)
      (should (eq (car (nelisp-textprop-keymap-chain-providers)) fn))
      (should (eq (cdr (nelisp-textprop-keymap-chain-providers)) fn)))))

;;; ─────────────────────────────────────────────────────────────────────
;;; §B. provider registration
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-tp-keymap-set-overlay-provider-returns-fn ()
  (nelisp-tp-keymap-test--with-clean-state
    (let ((fn (lambda (_p _b) nil)))
      (should (eq fn (nelisp-textprop-keymap-set-overlay-provider fn))))))

(ert-deftest nelisp-tp-keymap-set-textprop-provider-returns-fn ()
  (nelisp-tp-keymap-test--with-clean-state
    (let ((fn (lambda (_p _b) nil)))
      (should (eq fn (nelisp-textprop-keymap-set-textprop-provider fn))))))

(ert-deftest nelisp-tp-keymap-set-overlay-provider-rejects-non-fn ()
  (nelisp-tp-keymap-test--with-clean-state
    (should-error
     (nelisp-textprop-keymap-set-overlay-provider 42)
     :type 'nelisp-textprop-keymap-bad-provider)))

(ert-deftest nelisp-tp-keymap-set-textprop-provider-rejects-non-fn ()
  (nelisp-tp-keymap-test--with-clean-state
    (should-error
     (nelisp-textprop-keymap-set-textprop-provider "not-a-fn")
     :type 'nelisp-textprop-keymap-bad-provider)))

(ert-deftest nelisp-tp-keymap-set-provider-accepts-nil-clears ()
  (nelisp-tp-keymap-test--with-clean-state
    (nelisp-textprop-keymap-set-overlay-provider (lambda (_p _b) nil))
    (nelisp-textprop-keymap-set-overlay-provider nil)
    (should-not (car (nelisp-textprop-keymap-chain-providers)))))

(ert-deftest nelisp-tp-keymap-chain-providers-returns-cons ()
  (nelisp-tp-keymap-test--with-clean-state
    (let ((fn-o (lambda (_p _b) nil))
          (fn-t (lambda (_p _b) nil)))
      (nelisp-textprop-keymap-set-overlay-provider fn-o)
      (nelisp-textprop-keymap-set-textprop-provider fn-t)
      (let ((pair (nelisp-textprop-keymap-chain-providers)))
        (should (consp pair))
        (should (eq (car pair) fn-o))
        (should (eq (cdr pair) fn-t))))))

;;; ─────────────────────────────────────────────────────────────────────
;;; §C. opt-in invariant (injection nil = Doc 34 7-stage)
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-tp-keymap-injection-off-skips-overlay-slot ()
  (nelisp-tp-keymap-test--with-clean-state
    (let ((tagged (nelisp-tp-keymap-test--mk-keymap 'overlay-km)))
      (nelisp-textprop-keymap-set-overlay-provider
       (lambda (_p _b) tagged))
      ;; Injection OFF — overlay slot is invisible.
      (let ((cells (nelisp-textprop-keymap-precedence-at 1)))
        (should-not (assq 6 cells))
        (should-not (assq 7 cells))))))

(ert-deftest nelisp-tp-keymap-injection-off-skips-textprop-slot ()
  (nelisp-tp-keymap-test--with-clean-state
    (let ((tagged (nelisp-tp-keymap-test--mk-keymap 'textprop-km)))
      (nelisp-textprop-keymap-set-textprop-provider
       (lambda (_p _b) tagged))
      (let ((cells (nelisp-textprop-keymap-precedence-at 1)))
        (should-not (assq 6 cells))
        (should-not (assq 7 cells))))))

(ert-deftest nelisp-tp-keymap-injection-off-resolve-7-stage ()
  ;; With no special vars set the chain reduces to current-local +
  ;; global; assert resolve-at returns *only* slots ≤ 5 + 8 + 9.
  (nelisp-tp-keymap-test--with-clean-state
    (nelisp-textprop-keymap-set-overlay-provider
     (lambda (_p _b) (nelisp-tp-keymap-test--mk-keymap 'ovl)))
    (nelisp-textprop-keymap-set-textprop-provider
     (lambda (_p _b) (nelisp-tp-keymap-test--mk-keymap 'tp)))
    (let ((cells (nelisp-textprop-keymap-precedence-at 1)))
      (dolist (cell cells)
        (should (memq (car cell) '(1 2 3 4 5 8 9)))))))

;;; ─────────────────────────────────────────────────────────────────────
;;; §D. precedence ordering (injection ON)
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-tp-keymap-precedence-injects-textprop-slot-7 ()
  (nelisp-tp-keymap-test--with-clean-state
    (let ((tp-km (nelisp-tp-keymap-test--mk-keymap 'textprop-only)))
      (nelisp-textprop-keymap-install)
      (nelisp-textprop-keymap-set-textprop-provider
       (lambda (_p _b) tp-km))
      (let* ((cells (nelisp-textprop-keymap-precedence-at 5))
             (slot7 (assq 7 cells)))
        (should slot7)
        (should (eq (cdr slot7) tp-km))))))

(ert-deftest nelisp-tp-keymap-precedence-injects-overlay-slot-6 ()
  (nelisp-tp-keymap-test--with-clean-state
    (let ((ovl-km (nelisp-tp-keymap-test--mk-keymap 'overlay-only)))
      (nelisp-textprop-keymap-install)
      (nelisp-textprop-keymap-set-overlay-provider
       (lambda (_p _b) ovl-km))
      (let* ((cells (nelisp-textprop-keymap-precedence-at 5))
             (slot6 (assq 6 cells)))
        (should slot6)
        (should (eq (cdr slot6) ovl-km))))))

(ert-deftest nelisp-tp-keymap-precedence-orders-highest-first ()
  (nelisp-tp-keymap-test--with-clean-state
    (let ((ovl (nelisp-tp-keymap-test--mk-keymap 'ovl))
          (tp  (nelisp-tp-keymap-test--mk-keymap 'tp)))
      (nelisp-textprop-keymap-install)
      (nelisp-textprop-keymap-set-overlay-provider (lambda (_p _b) ovl))
      (nelisp-textprop-keymap-set-textprop-provider (lambda (_p _b) tp))
      (let* ((cells (nelisp-textprop-keymap-precedence-at 5))
             (slots (mapcar #'car cells))
             (filtered (cl-remove-if-not (lambda (s) (memq s '(6 7))) slots)))
        ;; Slot 6 must precede slot 7 in the returned ordering.
        (should (equal filtered '(6 7)))))))

(ert-deftest nelisp-tp-keymap-precedence-providers-receive-pos-buf ()
  (nelisp-tp-keymap-test--with-clean-state
    (let ((received nil))
      (nelisp-textprop-keymap-install)
      (nelisp-textprop-keymap-set-textprop-provider
       (lambda (p b)
         (push (list p b) received)
         nil))
      (with-temp-buffer
        (let ((buf (current-buffer)))
          (nelisp-textprop-keymap-precedence-at 42 buf)
          (should (equal (car received) (list 42 buf))))))))

(ert-deftest nelisp-tp-keymap-precedence-defaults-buf-to-current ()
  (nelisp-tp-keymap-test--with-clean-state
    (let ((received nil))
      (nelisp-textprop-keymap-install)
      (nelisp-textprop-keymap-set-textprop-provider
       (lambda (_p b) (push b received) nil))
      (with-temp-buffer
        (let ((this (current-buffer)))
          (nelisp-textprop-keymap-precedence-at 1)
          (should (eq (car received) this)))))))

;;; ─────────────────────────────────────────────────────────────────────
;;; §E. resolve-at composite
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-tp-keymap-resolve-at-returns-nil-when-empty ()
  (nelisp-tp-keymap-test--with-clean-state
    ;; In --batch the global-map / current-local-map may exist; the
    ;; only assertion we can make portably is that the resolver
    ;; returns either nil or a keymap.
    (let ((r (nelisp-textprop-keymap-resolve-at 1)))
      (should (or (null r) (keymapp r))))))

(ert-deftest nelisp-tp-keymap-resolve-at-composes-injection-slots ()
  (nelisp-tp-keymap-test--with-clean-state
    (let ((tp (nelisp-tp-keymap-test--mk-keymap 'tp)))
      (nelisp-textprop-keymap-install)
      (nelisp-textprop-keymap-set-textprop-provider (lambda (_p _b) tp))
      (let ((r (nelisp-textprop-keymap-resolve-at 1)))
        ;; The composite must be a keymap and must dispatch the
        ;; provider's TAG binding (= textprop slot is reachable).
        (should (keymapp r))
        (should (functionp (lookup-key r (kbd "TAG"))))))))

;;; ─────────────────────────────────────────────────────────────────────
;;; §F. higher-precedence slots beat textprop
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-tp-keymap-overriding-local-map-precedes-textprop ()
  (nelisp-tp-keymap-test--with-clean-state
    (let ((tp (nelisp-tp-keymap-test--mk-keymap 'tp))
          (ol (nelisp-tp-keymap-test--mk-keymap 'overriding-local)))
      (nelisp-textprop-keymap-install)
      (nelisp-textprop-keymap-set-textprop-provider (lambda (_p _b) tp))
      (let ((overriding-local-map ol))
        (let* ((cells (nelisp-textprop-keymap-precedence-at 5))
               (idx2 (cl-position 2 (mapcar #'car cells)))
               (idx7 (cl-position 7 (mapcar #'car cells))))
          (should idx2)
          (should idx7)
          (should (< idx2 idx7)))))))

(ert-deftest nelisp-tp-keymap-overriding-terminal-precedes-overlay ()
  (nelisp-tp-keymap-test--with-clean-state
    (let ((ovl (nelisp-tp-keymap-test--mk-keymap 'ovl))
          (otl (nelisp-tp-keymap-test--mk-keymap 'overriding-term)))
      (nelisp-textprop-keymap-install)
      (nelisp-textprop-keymap-set-overlay-provider (lambda (_p _b) ovl))
      (let ((overriding-terminal-local-map otl))
        (let* ((cells (nelisp-textprop-keymap-precedence-at 5))
               (slots (mapcar #'car cells)))
          (should (memq 1 slots))
          (should (memq 6 slots))
          (should (< (cl-position 1 slots) (cl-position 6 slots))))))))

;;; ─────────────────────────────────────────────────────────────────────
;;; §G. lower-precedence slots lose to textprop
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-tp-keymap-textprop-precedes-current-local ()
  (nelisp-tp-keymap-test--with-clean-state
    (let ((tp (nelisp-tp-keymap-test--mk-keymap 'tp)))
      (nelisp-textprop-keymap-install)
      (nelisp-textprop-keymap-set-textprop-provider (lambda (_p _b) tp))
      (with-temp-buffer
        ;; Install a *fake* current-local-map so slot 8 is non-empty.
        (let ((local (nelisp-tp-keymap-test--mk-keymap 'local)))
          (use-local-map local)
          (let* ((cells (nelisp-textprop-keymap-precedence-at 1
                                                              (current-buffer)))
                 (slots (mapcar #'car cells)))
            (should (memq 7 slots))
            (should (memq 8 slots))
            (should (< (cl-position 7 slots) (cl-position 8 slots)))))))))

(ert-deftest nelisp-tp-keymap-textprop-precedes-global ()
  (nelisp-tp-keymap-test--with-clean-state
    (let ((tp (nelisp-tp-keymap-test--mk-keymap 'tp)))
      (nelisp-textprop-keymap-install)
      (nelisp-textprop-keymap-set-textprop-provider (lambda (_p _b) tp))
      ;; global-map is always bound in batch.
      (let* ((cells (nelisp-textprop-keymap-precedence-at 1))
             (slots (mapcar #'car cells)))
        (should (memq 7 slots))
        (when (memq 9 slots)
          (should (< (cl-position 7 slots)
                     (cl-position 9 slots))))))))

;;; ─────────────────────────────────────────────────────────────────────
;;; §H. cross-slot — overlay > textprop (LOCKED)
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-tp-keymap-overlay-precedes-textprop ()
  (nelisp-tp-keymap-test--with-clean-state
    (let ((ovl (nelisp-tp-keymap-test--mk-keymap 'ovl))
          (tp  (nelisp-tp-keymap-test--mk-keymap 'tp)))
      (nelisp-textprop-keymap-install)
      (nelisp-textprop-keymap-set-overlay-provider  (lambda (_p _b) ovl))
      (nelisp-textprop-keymap-set-textprop-provider (lambda (_p _b) tp))
      (let* ((cells (nelisp-textprop-keymap-precedence-at 5)))
        (should (eq (cdr (assq 6 cells)) ovl))
        (should (eq (cdr (assq 7 cells)) tp))
        (should (< (cl-position 6 (mapcar #'car cells))
                   (cl-position 7 (mapcar #'car cells))))))))

;;; ─────────────────────────────────────────────────────────────────────
;;; §I. provider error semantics
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-tp-keymap-provider-returning-non-keymap-signals ()
  (nelisp-tp-keymap-test--with-clean-state
    (nelisp-textprop-keymap-install)
    (nelisp-textprop-keymap-set-textprop-provider
     (lambda (_p _b) 'not-a-keymap))
    (should-error
     (nelisp-textprop-keymap-precedence-at 1)
     :type 'nelisp-textprop-keymap-bad-provider)))

(ert-deftest nelisp-tp-keymap-provider-returning-nil-is-noop ()
  (nelisp-tp-keymap-test--with-clean-state
    (nelisp-textprop-keymap-install)
    (nelisp-textprop-keymap-set-textprop-provider (lambda (_p _b) nil))
    (let ((cells (nelisp-textprop-keymap-precedence-at 1)))
      (should-not (assq 7 cells)))))

(ert-deftest nelisp-tp-keymap-bad-provider-error-hierarchy ()
  ;; `nelisp-textprop-keymap-bad-provider' is a child of
  ;; `nelisp-textprop-keymap-error'.
  (let ((parents (get 'nelisp-textprop-keymap-bad-provider
                      'error-conditions)))
    (should (memq 'nelisp-textprop-keymap-error parents))
    (should (memq 'error parents))))

;;; ─────────────────────────────────────────────────────────────────────
;;; §J. 9b regression-free
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-tp-keymap-no-providers-no-injection ()
  (nelisp-tp-keymap-test--with-clean-state
    (nelisp-textprop-keymap-install)
    ;; No providers — slots 6 and 7 must not appear even with
    ;; injection enabled (= absence of provider = empty slot).
    (let* ((cells (nelisp-textprop-keymap-precedence-at 1))
           (slots (mapcar #'car cells)))
      (should-not (memq 6 slots))
      (should-not (memq 7 slots)))))

(ert-deftest nelisp-tp-keymap-emulation-mode-map-alist-honoured ()
  (nelisp-tp-keymap-test--with-clean-state
    (let* ((flag-sym (make-symbol "nelisp-tp-keymap-test-flag"))
           (alist-sym (make-symbol "nelisp-tp-keymap-test-alist"))
           (km (nelisp-tp-keymap-test--mk-keymap 'emul)))
      (set flag-sym t)
      (set alist-sym (list (cons flag-sym km)))
      (let ((emulation-mode-map-alists (list alist-sym)))
        (let* ((cells (nelisp-textprop-keymap-precedence-at 1))
               (slot5 (assq 5 cells)))
          (should slot5)
          (should (keymapp (cdr slot5))))))))

(ert-deftest nelisp-tp-keymap-minor-mode-map-alist-flag-respected ()
  (nelisp-tp-keymap-test--with-clean-state
    (let* ((flag-sym (make-symbol "nelisp-tp-keymap-test-flag-mm"))
           (km (nelisp-tp-keymap-test--mk-keymap 'mm)))
      ;; Flag is nil → entry must be invisible.
      (set flag-sym nil)
      (let ((minor-mode-map-alist (list (cons flag-sym km))))
        (let ((cells (nelisp-textprop-keymap-precedence-at 1)))
          (should-not (assq 4 cells))))
      ;; Flag flipped to t → entry visible.
      (set flag-sym t)
      (let ((minor-mode-map-alist (list (cons flag-sym km))))
        (let ((cells (nelisp-textprop-keymap-precedence-at 1)))
          (should (assq 4 cells)))))))

(ert-deftest nelisp-tp-keymap-slot-constants-form-1-9 ()
  (should (= nelisp-textprop-keymap-slot-overriding-terminal-local 1))
  (should (= nelisp-textprop-keymap-slot-overriding-local 2))
  (should (= nelisp-textprop-keymap-slot-minor-mode-overriding 3))
  (should (= nelisp-textprop-keymap-slot-minor-mode-map-alist 4))
  (should (= nelisp-textprop-keymap-slot-emulation-mode-map-alists 5))
  (should (= nelisp-textprop-keymap-slot-overlay 6))
  (should (= nelisp-textprop-keymap-slot-textprop 7))
  (should (= nelisp-textprop-keymap-slot-current-local 8))
  (should (= nelisp-textprop-keymap-slot-global 9)))

(ert-deftest nelisp-tp-keymap-resolve-empty-providers-equivalent-to-doc34 ()
  ;; Symmetry property: with injection ON but no providers, the
  ;; precedence list must equal the precedence list with injection OFF.
  (nelisp-tp-keymap-test--with-clean-state
    (let ((off-cells (nelisp-textprop-keymap-precedence-at 1)))
      (nelisp-textprop-keymap-install)
      (let ((on-cells (nelisp-textprop-keymap-precedence-at 1)))
        (should (equal (mapcar #'car off-cells)
                       (mapcar #'car on-cells)))))))

;;; nelisp-textprop-keymap-test.el ends here
