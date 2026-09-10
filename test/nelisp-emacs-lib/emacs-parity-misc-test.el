;;; emacs-parity-misc-test.el --- ERT for emacs-parity-misc  -*- lexical-binding: t; -*-

;;; Commentary:

;; Focused coverage for `add-variable-watcher' / `remove-variable-watcher' /
;; `get-variable-watchers' (T62 + T63): void anywhere in src/ (real init
;; load-matrix hit `(void-function add-variable-watcher)' for
;; `mixed-pitch', `whitespace' and `solaire-mode', all of which call it
;; unconditionally at top level).  The definitions in `emacs-parity-misc.el'
;; are gated `unless (fboundp ...)', so under host Emacs these tests
;; exercise the host's own real primitive; under standalone NeLisp (no
;; core `set'/`setq'/`let' watcher hook) they exercise the store-only
;; port.  Either way, the registration/query/removal data-shape contract
;; asserted here is identical -- verified against host Emacs 31.1's own
;; observed behaviour (see the commentary in `emacs-parity-misc.el' for
;; the exact round-trip captured from the host).

;;; Code:

(require 'ert)
(require 'emacs-parity-misc)

(defconst emacs-parity-misc-test--source
  (expand-file-name
   "../src/emacs-parity-misc.el"
   (file-name-directory (or load-file-name buffer-file-name))))

(defvar emacs-parity-misc-test--var-1 1)
(defvar emacs-parity-misc-test--var-2 1)

(defun emacs-parity-misc-test--w1 (&rest _args) nil)
(defun emacs-parity-misc-test--w2 (&rest _args) nil)

(ert-deftest emacs-parity-misc-test/repairs-reversed-pcomplete-alias ()
  "Repair Org's unresolved compatibility alias before adding the old alias."
  (let ((new-cell (and (fboundp 'pcomplete-uniquify-list)
                       (symbol-function 'pcomplete-uniquify-list)))
        (old-cell (and (fboundp 'pcomplete-uniqify-list)
                       (symbol-function 'pcomplete-uniqify-list))))
    (unwind-protect
        (progn
          (fmakunbound 'pcomplete-uniquify-list)
          (fmakunbound 'pcomplete-uniqify-list)
          (defalias 'pcomplete-uniquify-list 'pcomplete-uniqify-list)
          (load emacs-parity-misc-test--source nil t)
          (should (equal '("a" "b")
                         (pcomplete-uniquify-list '("b" "a" "a"))))
          (should (equal '("a" "b")
                         (pcomplete-uniqify-list '("b" "a" "a")))))
      (if new-cell
          (fset 'pcomplete-uniquify-list new-cell)
        (fmakunbound 'pcomplete-uniquify-list))
      (if old-cell
          (fset 'pcomplete-uniqify-list old-cell)
        (fmakunbound 'pcomplete-uniqify-list)))))

(ert-deftest emacs-parity-misc-test/watchers-callable-and-arity ()
  (should (fboundp 'add-variable-watcher))
  (should (fboundp 'remove-variable-watcher))
  (should (fboundp 'get-variable-watchers))
  (should (equal '(2 . 2) (func-arity (symbol-function 'add-variable-watcher))))
  (should (equal '(2 . 2) (func-arity (symbol-function 'remove-variable-watcher))))
  (should (equal '(1 . 1) (func-arity (symbol-function 'get-variable-watchers)))))

(ert-deftest emacs-parity-misc-test/unset-symbol-has-no-watchers ()
  (should (null (get-variable-watchers 'emacs-parity-misc-test--never-registered))))

(ert-deftest emacs-parity-misc-test/add-returns-nil ()
  (should (null (add-variable-watcher 'emacs-parity-misc-test--var-1
                                       #'emacs-parity-misc-test--w1)))
  (remove-variable-watcher 'emacs-parity-misc-test--var-1
                            #'emacs-parity-misc-test--w1))

(ert-deftest emacs-parity-misc-test/add-query-remove-round-trip ()
  "Matches the exact host Emacs 31.1 data shape/order:
adding w1 then w2 then re-adding w1 (a no-op -- already registered,
neither duplicated nor moved) leaves the order (w2 w1); removing w1
leaves (w2); removing w2 leaves nil."
  (unwind-protect
      (progn
        (add-variable-watcher 'emacs-parity-misc-test--var-2
                               #'emacs-parity-misc-test--w1)
        (add-variable-watcher 'emacs-parity-misc-test--var-2
                               #'emacs-parity-misc-test--w2)
        (add-variable-watcher 'emacs-parity-misc-test--var-2
                               #'emacs-parity-misc-test--w1)
        (should (equal (list #'emacs-parity-misc-test--w2
                              #'emacs-parity-misc-test--w1)
                       (get-variable-watchers 'emacs-parity-misc-test--var-2)))
        (should (null (remove-variable-watcher
                        'emacs-parity-misc-test--var-2
                        #'emacs-parity-misc-test--w1)))
        (should (equal (list #'emacs-parity-misc-test--w2)
                       (get-variable-watchers 'emacs-parity-misc-test--var-2)))
        (remove-variable-watcher 'emacs-parity-misc-test--var-2
                                  #'emacs-parity-misc-test--w2)
        (should (null (get-variable-watchers 'emacs-parity-misc-test--var-2))))
    (remove-variable-watcher 'emacs-parity-misc-test--var-2
                              #'emacs-parity-misc-test--w1)
    (remove-variable-watcher 'emacs-parity-misc-test--var-2
                              #'emacs-parity-misc-test--w2)))

(ert-deftest emacs-parity-misc-test/remove-is-idempotent-and-returns-nil ()
  (should (null (remove-variable-watcher
                 'emacs-parity-misc-test--var-1
                 #'emacs-parity-misc-test--w1)))
  (should (null (get-variable-watchers 'emacs-parity-misc-test--var-1))))

(provide 'emacs-parity-misc-test)

;;; emacs-parity-misc-test.el ends here
