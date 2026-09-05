;;; emacs-keymap-test.el --- ERT tests for emacs-keymap.el  -*- lexical-binding: t; -*-

;; Phase 1 module 4/6 tests per nelisp-emacs Doc 01.
;; Covers all 5 categories of `emacs-keymap-*' API plus Doc 41 §2.5
;; KEYMAP_CHAIN_INJECT_CONTRACT_VERSION = 1 opt-in semantics.
;;
;; Categories:
;;   A. constructors / predicates / copy           (5 tests)
;;   B. mutators / accessors                        (10 tests)
;;   C. global / local / overriding + chain         (8 tests)
;;   D. lookup helpers                              (6 tests)
;;   E. minimal command-loop scaffolding            (3 tests)
;;   F. Doc 41 §2.5 chain inject opt-in             (3 tests)
;;   G. newer kbd-style API (Phase 1 §4.4)          (17 tests)
;; Total: 52 tests

(require 'ert)
(require 'emacs-keymap)

;;; Fresh-world fixture

(defmacro emacs-keymap-test--with-fresh-world (&rest body)
  "Run BODY with clean global / local / overriding state."
  (declare (indent 0) (debug (body)))
  `(let ((emacs-keymap-global-map (emacs-keymap-make-sparse-keymap))
         (emacs-keymap-local-map nil)
         (emacs-keymap-overriding-local-map nil)
         (emacs-keymap-overriding-terminal-local-map nil)
         (emacs-keymap-minor-mode-overriding-map-alist nil)
         (emacs-keymap-minor-mode-map-alist nil)
         (emacs-keymap-emulation-mode-map-alists nil)
         (emacs-keymap-chain-with-textprop nil)
         (emacs-keymap-chain-overlay-provider nil)
         (emacs-keymap-chain-textprop-provider nil)
         (emacs-keymap--this-command-keys (vector))
         (emacs-keymap--input-queue nil)
         (emacs-keymap--read-event-fn nil))
     ,@body))

;;; T74 fixture: real host Emacs' own keymap machinery, for
;;; host-vs-substrate parity ERT below (Doc 34 §4.3 slot 5 /
;;; `emulation-mode-map-alists' symbol-dereference bug).

(defmacro emacs-keymap-test--with-host-fresh-world (&rest body)
  "Run BODY with *host* Emacs' own keymap globals swapped to a
throwaway global map and no local/overriding/minor/emulation maps,
restoring the previous global map afterwards.  Unlike
`emacs-keymap-test--with-fresh-world' (which exercises this file's
own `emacs-keymap-*' substrate state), this drives real Emacs
`current-active-maps' / `minor-mode-map-alist' /
`emulation-mode-map-alists', so results can be compared directly
against the substrate for the same scenario."
  (declare (indent 0) (debug (body)))
  `(let ((emacs-keymap-test--host-old-global (current-global-map)))
     (unwind-protect
         (with-temp-buffer
           (use-global-map (make-sparse-keymap))
           (use-local-map nil)
           (let ((overriding-local-map nil)
                 (overriding-terminal-local-map nil)
                 (minor-mode-overriding-map-alist nil)
                 (minor-mode-map-alist nil)
                 (emulation-mode-map-alists nil))
             ,@body))
       (use-global-map emacs-keymap-test--host-old-global))))

(defun emacs-keymap-test--host-binding (key)
  "Look up KEY through real Emacs' own `current-active-maps', the same
priority-ordered walk `emacs-keymap-key-binding' does for the
substrate."
  (catch 'found
    (dolist (km (current-active-maps))
      (let ((b (lookup-key km key)))
        (when (and b (not (integerp b)))
          (throw 'found b))))
    nil))

;;;; A. constructors / predicates / copy (4 tests)

(ert-deftest emacs-keymap-make-sparse-keymap-basic ()
  (let ((m (emacs-keymap-make-sparse-keymap)))
    (should (emacs-keymap-keymapp m))
    (should (eq (car m) 'keymap))
    (should (null (cdr m)))))

(ert-deftest emacs-keymap-make-sparse-keymap-with-prompt ()
  (let ((m (emacs-keymap-make-sparse-keymap "Pick:")))
    (should (emacs-keymap-keymapp m))
    (should (string-equal "Pick:" (emacs-keymap-keymap-prompt m)))))

(ert-deftest emacs-keymap-make-keymap-has-full-slot ()
  (let ((m (emacs-keymap-make-keymap)))
    (should (emacs-keymap-keymapp m))
    ;; Real-Emacs full-keymap shape: the second element is a char-table,
    ;; so vendor `(char-table-p (nth 1 map))' assertions (e.g. isearch)
    ;; pass.  Its ASCII fast-path covers the 256 low character codes.
    (should (emacs-char-table-p (nth 1 m)))
    (should (= 256 (length (emacs-char-table-ascii-vector (nth 1 m)))))
    ;; Public low-level adapter resolves it for runner/front-end fast paths.
    (should (emacs-char-table-p (emacs-keymap-full-slot m)))
    (should (emacs-char-table-p (emacs-keymap--full-slot m)))))

(ert-deftest emacs-keymap-direct-slot-vector-and-fast-define-key ()
  (let* ((m (emacs-keymap-make-keymap))
         (vec (emacs-keymap-direct-slot-vector m)))
    (should (vectorp vec))
    (should (= 256 (length vec)))
    (should (eq 'cmd-a
                (emacs-keymap-define-key-fast m (vector ?a) 'cmd-a vec)))
    (should (eq 'cmd-a (aref vec ?a)))
    (should (eq 'cmd-a (emacs-keymap-lookup-key m (vector ?a))))))

(ert-deftest emacs-keymap-make-compatible-full-keymap-has-direct-slot ()
  (let ((m (emacs-keymap-make-compatible-full-keymap)))
    (should (keymapp m))
    (emacs-keymap-define-key-fast
     m (vector ?a) 'cmd-a (emacs-keymap-direct-slot-vector m))
    (should (eq 'cmd-a (lookup-key m (vector ?a))))))

(ert-deftest emacs-keymap-build-single-key-cache-uses-slot-and-lookup ()
  (let ((m (emacs-keymap-make-keymap)))
    (emacs-keymap-define-key-fast
     m (vector ?a) 'cmd-a (emacs-keymap-direct-slot-vector m))
    (should (eq 'cmd-a
                (aref (emacs-keymap-build-single-key-cache m) ?a))))
  (let* ((sparse (emacs-keymap-make-sparse-keymap))
         (cache
          (emacs-keymap-build-single-key-cache
           sparse
           (lambda (_map key)
             (when (equal key (vector ?z))
               'fallback-z)))))
    (should (eq 'fallback-z (aref cache ?z)))))

(ert-deftest emacs-keymap-overriding-terminal-map-helpers ()
  (let ((noninteractive nil)
        (overriding-terminal-local-map nil))
    (let ((m (emacs-keymap-make-sparse-keymap)))
      (should (eq m (emacs-keymap-install-overriding-terminal-map m)))
      (should (eq m overriding-terminal-local-map))
      (emacs-keymap-clear-overriding-terminal-map)
      (should-not overriding-terminal-local-map))))

(ert-deftest emacs-keymap-keymapp-rejects-non-keymaps ()
  (should-not (emacs-keymap-keymapp nil))
  (should-not (emacs-keymap-keymapp 42))
  (should-not (emacs-keymap-keymapp '(other)))
  (should-not (emacs-keymap-keymapp "string")))

(ert-deftest emacs-keymap-copy-keymap-deep-copies ()
  (let* ((src (emacs-keymap-make-sparse-keymap))
         (sub (emacs-keymap-make-sparse-keymap)))
    (emacs-keymap-define-key sub "x" 'cmd-x)
    (emacs-keymap-define-key src "p" sub)
    (let ((dst (emacs-keymap-copy-keymap src)))
      ;; mutating dst's sub must not affect src
      (let ((dst-sub (emacs-keymap-lookup-key dst "p")))
        (should (emacs-keymap-keymapp dst-sub))
        (emacs-keymap-define-key dst-sub "y" 'cmd-y)
        (should (eq 'cmd-y (emacs-keymap-lookup-key dst "py")))
        (should (null (emacs-keymap-lookup-key src "py")))
        (should (eq 'cmd-x (emacs-keymap-lookup-key src "px")))))))

;;;; B. mutators / accessors (7 tests)

(ert-deftest emacs-keymap-define-key-and-lookup-single ()
  (let ((m (emacs-keymap-make-sparse-keymap)))
    (emacs-keymap-define-key m "a" 'cmd-a)
    (should (eq 'cmd-a (emacs-keymap-lookup-key m "a")))
    (should (null (emacs-keymap-lookup-key m "b")))))

(ert-deftest emacs-keymap-define-key-multi-char-prefix ()
  (let ((m (emacs-keymap-make-sparse-keymap)))
    (emacs-keymap-define-key m "ab" 'cmd-ab)
    ;; partial lookup returns the sub-keymap
    (should (emacs-keymap-keymapp (emacs-keymap-lookup-key m "a")))
    (should (eq 'cmd-ab (emacs-keymap-lookup-key m "ab")))))

(ert-deftest emacs-keymap-define-key-overrides ()
  (let ((m (emacs-keymap-make-sparse-keymap)))
    (emacs-keymap-define-key m "a" 'first)
    (emacs-keymap-define-key m "a" 'second)
    (should (eq 'second (emacs-keymap-lookup-key m "a")))))

(ert-deftest emacs-keymap-define-key-nil-removes ()
  (let ((m (emacs-keymap-make-sparse-keymap)))
    (emacs-keymap-define-key m "a" 'cmd-a)
    (emacs-keymap-define-key m "a" nil)
    (should (null (emacs-keymap-lookup-key m "a")))))

(ert-deftest emacs-keymap-define-key-after-appends-by-default ()
  (let ((m (emacs-keymap-make-sparse-keymap))
        (seen '()))
    (emacs-keymap-define-key m "a" 'cmd-a)
    (emacs-keymap-define-key m "b" 'cmd-b)
    (should (eq 'cmd-c (emacs-keymap-define-key-after m "c" 'cmd-c)))
    (emacs-keymap-map-keymap (lambda (k _v) (push k seen)) m)
    (should (equal (nreverse seen) (list ?b ?a ?c)))
    (should (eq 'cmd-c (emacs-keymap-lookup-key m "c")))))

(ert-deftest emacs-keymap-define-key-after-inserts-after-event ()
  (let ((m (emacs-keymap-make-sparse-keymap))
        (seen '()))
    (emacs-keymap-define-key m "a" 'cmd-a)
    (emacs-keymap-define-key m "b" 'cmd-b)
    (emacs-keymap-define-key-after m "c" 'cmd-c ?b)
    (emacs-keymap-map-keymap (lambda (k _v) (push k seen)) m)
    (should (equal (nreverse seen) (list ?b ?c ?a)))))

(ert-deftest emacs-keymap-define-key-after-multi-key-uses-prefix-map ()
  (let ((m (emacs-keymap-make-sparse-keymap))
        (seen '()))
    (emacs-keymap-define-key m "xa" 'cmd-a)
    (emacs-keymap-define-key m "xb" 'cmd-b)
    (emacs-keymap-define-key-after m "xc" 'cmd-c ?b)
    (should (eq 'cmd-c (emacs-keymap-lookup-key m "xc")))
    (emacs-keymap-map-keymap
     (lambda (k _v) (push k seen))
     (emacs-keymap-lookup-key m "x"))
    (should (equal (nreverse seen) (list ?b ?c ?a)))))

(ert-deftest emacs-keymap-set-keymap-parent-and-inherit ()
  (let ((parent (emacs-keymap-make-sparse-keymap))
        (child  (emacs-keymap-make-sparse-keymap)))
    (emacs-keymap-define-key parent "z" 'parent-z)
    (emacs-keymap-set-keymap-parent child parent)
    (should (eq parent (emacs-keymap-keymap-parent child)))
    ;; inherit lookup
    (should (eq 'parent-z (emacs-keymap-lookup-key child "z")))
    (should (eq 'parent-z (emacs-keymap-lookup-with-parent child ?z)))
    ;; child shadows parent
    (emacs-keymap-define-key child "z" 'child-z)
    (should (eq 'child-z (emacs-keymap-lookup-key child "z")))
    (should (eq 'child-z (emacs-keymap-lookup-with-parent child ?z)))))

(ert-deftest emacs-keymap-set-keymap-parent-rejects-cycle ()
  (let ((m (emacs-keymap-make-sparse-keymap)))
    (should-error (emacs-keymap-set-keymap-parent m m)
                  :type 'emacs-keymap-error)))

(ert-deftest emacs-keymap-keymap-prompt-returns-nil-when-absent ()
  (let ((m (emacs-keymap-make-sparse-keymap)))
    (should (null (emacs-keymap-keymap-prompt m)))))

(ert-deftest emacs-keymap-keymap-prompt-non-keymap-returns-nil-not-error ()
  "T74: matches GNU `keymap-prompt', which is lenient for ANY
non-keymap argument (nil, a symbol, a string, an integer) — confirmed
against host Emacs 31.1, none of these signal.  Evil's
`evil-auxiliary-keymap-p' relies on exactly this: it calls
`keymap-prompt' on whatever `lookup-key' returned, without a `keymapp'
guard."
  (should (null (emacs-keymap-keymap-prompt nil)))
  (should (null (emacs-keymap-keymap-prompt 'not-a-keymap)))
  (should (null (emacs-keymap-keymap-prompt "not a keymap")))
  (should (null (emacs-keymap-keymap-prompt 42))))

;;;; C. global / local / overriding + chain (8 tests)

(ert-deftest emacs-keymap-use-global-map-and-current ()
  (emacs-keymap-test--with-fresh-world
    (let ((m (emacs-keymap-make-sparse-keymap)))
      (emacs-keymap-define-key m "x" 'cmd-x)
      (emacs-keymap-use-global-map m)
      (should (eq m (emacs-keymap-current-global-map)))
      (should (eq 'cmd-x (emacs-keymap-key-binding "x"))))))

(ert-deftest emacs-keymap-use-local-map-shadows-global ()
  (emacs-keymap-test--with-fresh-world
    (let ((g (emacs-keymap-make-sparse-keymap))
          (l (emacs-keymap-make-sparse-keymap)))
      (emacs-keymap-define-key g "k" 'global-k)
      (emacs-keymap-define-key l "k" 'local-k)
      (emacs-keymap-use-global-map g)
      (emacs-keymap-use-local-map l)
      (should (eq 'local-k (emacs-keymap-key-binding "k"))))))

(ert-deftest emacs-keymap-overriding-local-map-takes-precedence ()
  (emacs-keymap-test--with-fresh-world
    (let ((g (emacs-keymap-make-sparse-keymap))
          (l (emacs-keymap-make-sparse-keymap))
          (o (emacs-keymap-make-sparse-keymap)))
      (emacs-keymap-define-key g "k" 'g)
      (emacs-keymap-define-key l "k" 'l)
      (emacs-keymap-define-key o "k" 'o)
      (emacs-keymap-use-global-map g)
      (emacs-keymap-use-local-map l)
      (let ((emacs-keymap-overriding-local-map o))
        (should (eq 'o (emacs-keymap-key-binding "k")))))))

(ert-deftest emacs-keymap-overriding-terminal-local-map-wins ()
  (emacs-keymap-test--with-fresh-world
    (let ((g (emacs-keymap-make-sparse-keymap))
          (o (emacs-keymap-make-sparse-keymap))
          (t-map (emacs-keymap-make-sparse-keymap)))
      (emacs-keymap-define-key g "k" 'g)
      (emacs-keymap-define-key o "k" 'o)
      (emacs-keymap-define-key t-map "k" 'terminal)
      (emacs-keymap-use-global-map g)
      (let ((emacs-keymap-overriding-local-map o)
            (emacs-keymap-overriding-terminal-local-map t-map))
        (should (eq 'terminal (emacs-keymap-key-binding "k")))))))

(ert-deftest emacs-keymap-minor-mode-map-alist-respects-active-flag ()
  (emacs-keymap-test--with-fresh-world
    (let ((g (emacs-keymap-make-sparse-keymap))
          (mm (emacs-keymap-make-sparse-keymap)))
      (emacs-keymap-define-key g "k" 'global)
      (emacs-keymap-define-key mm "k" 'minor)
      (emacs-keymap-use-global-map g)
      (defvar emacs-keymap-test--mm-flag nil)
      (let ((emacs-keymap-minor-mode-map-alist
             (list (cons 'emacs-keymap-test--mm-flag mm))))
        ;; flag off => global wins
        (setq emacs-keymap-test--mm-flag nil)
        (should (eq 'global (emacs-keymap-key-binding "k")))
        ;; flag on => minor wins
        (setq emacs-keymap-test--mm-flag t)
        (should (eq 'minor (emacs-keymap-key-binding "k")))))))

(ert-deftest emacs-keymap-minor-mode-overriding-map-alist-beats-minor-mode-map-alist ()
  (emacs-keymap-test--with-fresh-world
    (let ((g (emacs-keymap-make-sparse-keymap))
          (mm (emacs-keymap-make-sparse-keymap))
          (mmo (emacs-keymap-make-sparse-keymap)))
      (emacs-keymap-define-key g "k" 'g)
      (emacs-keymap-define-key mm "k" 'mm)
      (emacs-keymap-define-key mmo "k" 'mmo)
      (emacs-keymap-use-global-map g)
      (defvar emacs-keymap-test--flag-mm  t)
      (defvar emacs-keymap-test--flag-mmo t)
      (let ((emacs-keymap-minor-mode-map-alist
             (list (cons 'emacs-keymap-test--flag-mm mm)))
            (emacs-keymap-minor-mode-overriding-map-alist
             (list (cons 'emacs-keymap-test--flag-mmo mmo))))
        (should (eq 'mmo (emacs-keymap-key-binding "k")))))))

(ert-deftest emacs-keymap-emulation-mode-map-alists-active ()
  (emacs-keymap-test--with-fresh-world
    (let ((g (emacs-keymap-make-sparse-keymap))
          (em (emacs-keymap-make-sparse-keymap)))
      (emacs-keymap-define-key g "k" 'g)
      (emacs-keymap-define-key em "k" 'em)
      (emacs-keymap-use-global-map g)
      (defvar emacs-keymap-test--em-flag t)
      (let ((emacs-keymap-emulation-mode-map-alists
             (list (list (cons 'emacs-keymap-test--em-flag em)))))
        (should (eq 'em (emacs-keymap-key-binding "k")))))))

;;;; T74: `emulation-mode-map-alists' elements may be symbols
;;;; (GNU keymap.c `current_minor_maps' -> `find_symbol_value'), and
;;;; alist MAP cells may be keymaps given indirectly through a
;;;; symbol's function cell (`get_keymap').  8 tests + 2 helpers.

(ert-deftest emacs-keymap-emulation-mode-map-alists-symbol-element-active ()
  "Evil's own idiom: `(push (quote evil-mode-map-alist)
emulation-mode-map-alists)' pushes the *symbol*, not its value.  The
walker must dereference it via `symbol-value', matching GNU's
`find_symbol_value' in `current_minor_maps' — this is the exact
Wrong-type-argument sequencep repro from the standalone bug report."
  (emacs-keymap-test--with-fresh-world
    (let ((g (emacs-keymap-make-sparse-keymap))
          (em (emacs-keymap-make-sparse-keymap)))
      (emacs-keymap-define-key g "k" 'global)
      (emacs-keymap-define-key em "k" 'emulation)
      (emacs-keymap-use-global-map g)
      (defvar emacs-keymap-test--sym-flag t)
      (defvar emacs-keymap-test--sym-alist
        (list (cons 'emacs-keymap-test--sym-flag em)))
      (let ((emacs-keymap-emulation-mode-map-alists
             (list 'emacs-keymap-test--sym-alist)))
        (should (eq 'emulation (emacs-keymap-key-binding "k")))))))

(ert-deftest emacs-keymap-emulation-mode-map-alists-unbound-symbol-skipped ()
  "An unbound symbol element must be skipped silently (not signaled),
matching GNU: `find_symbol_value' on an unbound symbol yields Qunbound
and contributes nothing to the walk."
  (emacs-keymap-test--with-fresh-world
    (let ((g (emacs-keymap-make-sparse-keymap)))
      (emacs-keymap-define-key g "k" 'global)
      (emacs-keymap-use-global-map g)
      (should (not (boundp 'emacs-keymap-test--never-bound-alist)))
      (let ((emacs-keymap-emulation-mode-map-alists
             (list 'emacs-keymap-test--never-bound-alist)))
        (should (eq 'global (emacs-keymap-key-binding "k")))))))

(ert-deftest emacs-keymap-emulation-mode-map-alists-nil-valued-symbol-skipped ()
  "A bound symbol element whose value is nil (no alist yet, e.g. a
mode not turned on) must be skipped silently, same as an empty alist
would be."
  (emacs-keymap-test--with-fresh-world
    (let ((g (emacs-keymap-make-sparse-keymap)))
      (emacs-keymap-define-key g "k" 'global)
      (emacs-keymap-use-global-map g)
      (defvar emacs-keymap-test--nil-alist nil)
      (let ((emacs-keymap-emulation-mode-map-alists
             (list 'emacs-keymap-test--nil-alist)))
        (should (eq 'global (emacs-keymap-key-binding "k")))))))

(ert-deftest emacs-keymap-minor-mode-map-alist-map-as-symbol-indirection ()
  "A (VAR . MAP) entry's MAP may itself be a symbol whose *function*
cell holds the keymap (GNU `get_keymap' indirection, e.g. `fset' on a
mode-map symbol).  Must resolve exactly like a direct keymap MAP."
  (emacs-keymap-test--with-fresh-world
    (let ((g (emacs-keymap-make-sparse-keymap))
          (mm (emacs-keymap-make-sparse-keymap)))
      (emacs-keymap-define-key g "k" 'global)
      (emacs-keymap-define-key mm "k" 'minor)
      (emacs-keymap-use-global-map g)
      (defvar emacs-keymap-test--mm-flag2 t)
      (fset 'emacs-keymap-test--mm-symbolic-map mm)
      (unwind-protect
          (let ((emacs-keymap-minor-mode-map-alist
                 (list (cons 'emacs-keymap-test--mm-flag2
                             'emacs-keymap-test--mm-symbolic-map))))
            (should (eq 'minor (emacs-keymap-key-binding "k"))))
        (fmakunbound 'emacs-keymap-test--mm-symbolic-map)))))

(ert-deftest emacs-keymap-active-minor-mode-maps-skips-malformed-entries ()
  "A malformed ALIST element (not a cons) or a non-list ALIST
(e.g. an unresolved emulation element passed straight through) must be
skipped, not signaled."
  (should (null (emacs-keymap--active-minor-mode-maps '(not-a-cons))))
  (should (null (emacs-keymap--active-minor-mode-maps 'not-a-list-at-all))))

(ert-deftest emacs-keymap-emulation-mode-map-alists-symbol-element-beats-global-and-minor ()
  "Full repro shape: emulation (via a symbol element) still outranks
minor-mode-map-alist and the global map, matching GNU precedence."
  (emacs-keymap-test--with-fresh-world
    (let ((g (emacs-keymap-make-sparse-keymap))
          (mm (emacs-keymap-make-sparse-keymap))
          (em (emacs-keymap-make-sparse-keymap)))
      (emacs-keymap-define-key g "k" 'global)
      (emacs-keymap-define-key mm "k" 'minor)
      (emacs-keymap-define-key em "k" 'emulation)
      (emacs-keymap-use-global-map g)
      (defvar emacs-keymap-test--mm-flag3 t)
      (defvar emacs-keymap-test--em-flag2 t)
      (defvar emacs-keymap-test--em-symbolic-alist
        (list (cons 'emacs-keymap-test--em-flag2 em)))
      (let ((emacs-keymap-minor-mode-map-alist
             (list (cons 'emacs-keymap-test--mm-flag3 mm)))
            (emacs-keymap-emulation-mode-map-alists
             (list 'emacs-keymap-test--em-symbolic-alist)))
        (should (eq 'emulation (emacs-keymap-key-binding "k")))))))

;;;; T74: host-vs-substrate parity table.  Each pair below proves the
;;;; substrate's `emacs-keymap-chain-at' resolves a key exactly like
;;;; real Emacs' own `current-active-maps' for the identical scenario.

(ert-deftest emacs-keymap-host-emulation-mode-map-alists-plain-alist ()
  (emacs-keymap-test--with-host-fresh-world
    (let ((em (make-sparse-keymap)))
      (define-key em "k" 'emulation)
      (defvar emacs-keymap-test--host-flag-plain t)
      (setq emulation-mode-map-alists
            (list (list (cons 'emacs-keymap-test--host-flag-plain em))))
      (should (eq 'emulation (emacs-keymap-test--host-binding "k"))))))

(ert-deftest emacs-keymap-host-emulation-mode-map-alists-symbol-element ()
  (emacs-keymap-test--with-host-fresh-world
    (let ((em (make-sparse-keymap)))
      (define-key em "k" 'emulation)
      (defvar emacs-keymap-test--host-flag-sym t)
      (defvar emacs-keymap-test--host-sym-alist
        (list (cons 'emacs-keymap-test--host-flag-sym em)))
      (setq emulation-mode-map-alists (list 'emacs-keymap-test--host-sym-alist))
      (should (eq 'emulation (emacs-keymap-test--host-binding "k"))))))

(ert-deftest emacs-keymap-host-emulation-mode-map-alists-unbound-symbol ()
  (emacs-keymap-test--with-host-fresh-world
    (should (not (boundp 'emacs-keymap-test--host-never-bound-alist)))
    (setq emulation-mode-map-alists
          (list 'emacs-keymap-test--host-never-bound-alist))
    (should (null (emacs-keymap-test--host-binding "k")))))

(ert-deftest emacs-keymap-host-emulation-mode-map-alists-nil-valued-symbol ()
  (emacs-keymap-test--with-host-fresh-world
    (defvar emacs-keymap-test--host-nil-alist nil)
    (setq emulation-mode-map-alists (list 'emacs-keymap-test--host-nil-alist))
    (should (null (emacs-keymap-test--host-binding "k")))))

(ert-deftest emacs-keymap-host-minor-mode-map-map-as-symbol ()
  (emacs-keymap-test--with-host-fresh-world
    (let ((mm (make-sparse-keymap)))
      (define-key mm "k" 'minor)
      (defvar emacs-keymap-test--host-mm-flag t)
      (fset 'emacs-keymap-test--host-mm-symbolic-map mm)
      (unwind-protect
          (progn
            (setq minor-mode-map-alist
                  (list (cons 'emacs-keymap-test--host-mm-flag
                              'emacs-keymap-test--host-mm-symbolic-map)))
            (should (eq 'minor (emacs-keymap-test--host-binding "k"))))
        (fmakunbound 'emacs-keymap-test--host-mm-symbolic-map)))))

(ert-deftest emacs-keymap-host-minor-mode-overriding-map-alist-precedence ()
  (emacs-keymap-test--with-host-fresh-world
    (let ((mm (make-sparse-keymap))
          (mmo (make-sparse-keymap)))
      (define-key mm "k" 'minor)
      (define-key mmo "k" 'minor-overriding)
      (defvar emacs-keymap-test--host-flag-mm t)
      (defvar emacs-keymap-test--host-flag-mmo t)
      (setq minor-mode-map-alist
            (list (cons 'emacs-keymap-test--host-flag-mm mm)))
      (setq minor-mode-overriding-map-alist
            (list (cons 'emacs-keymap-test--host-flag-mmo mmo)))
      (should (eq 'minor-overriding (emacs-keymap-test--host-binding "k"))))))

(ert-deftest emacs-keymap-chain-at-7-stage-default ()
  (emacs-keymap-test--with-fresh-world
    (let ((g (emacs-keymap-make-sparse-keymap)))
      (emacs-keymap-use-global-map g)
      ;; default chain has just global + (no local)
      (let ((c (emacs-keymap-chain-at)))
        (should (= 1 (length c)))
        (should (eq g (car c)))))))

;;;; D. lookup helpers (6 tests)

(ert-deftest emacs-keymap-key-binding-uses-chain ()
  (emacs-keymap-test--with-fresh-world
    (let ((g (emacs-keymap-make-sparse-keymap)))
      (emacs-keymap-define-key g "z" 'cmd-z)
      (emacs-keymap-use-global-map g)
      (should (eq 'cmd-z (emacs-keymap-key-binding "z"))))))

(ert-deftest emacs-keymap-lookup-key-nil-keymap-returns-nil-not-error ()
  "T74: GNU `lookup-key' is lenient for a nil KEYMAP specifically
\(confirmed against host Emacs 31.1: `(lookup-key nil \"a\")' => nil,
no error), unlike any other non-keymap value.  Evil's
`evil-get-auxiliary-keymap' relies on this via `(keymap-parent map)'
being nil for a top-level keymap, feeding straight into a further
`lookup-key'-adjacent check without a nil guard."
  (should (null (emacs-keymap-lookup-key nil "a"))))

(ert-deftest emacs-keymap-lookup-key-non-keymap-non-nil-still-signals ()
  "The nil leniency above is a narrow, specific carve-out — GNU still
signals for any other non-keymap KEYMAP (a plain symbol, a string, an
integer), confirmed against host Emacs 31.1."
  (should-error (emacs-keymap-lookup-key 'not-a-keymap "a")
                :type 'emacs-keymap-not-keymap)
  (should-error (emacs-keymap-lookup-key "not a keymap" "a")
                :type 'emacs-keymap-not-keymap)
  (should-error (emacs-keymap-lookup-key 42 "a")
                :type 'emacs-keymap-not-keymap))

(ert-deftest emacs-keymap-map-keymap-visits-every-binding ()
  (let ((m (emacs-keymap-make-sparse-keymap))
        (seen '()))
    (emacs-keymap-define-key m "a" 'a)
    (emacs-keymap-define-key m "b" 'b)
    (emacs-keymap-map-keymap
     (lambda (k v) (push (cons k v) seen))
     m)
    (should (equal (sort (copy-sequence seen)
                         (lambda (x y) (< (car x) (car y))))
                   (list (cons ?a 'a) (cons ?b 'b))))))

(ert-deftest emacs-keymap-map-keymap-walks-parent ()
  (let ((parent (emacs-keymap-make-sparse-keymap))
        (child  (emacs-keymap-make-sparse-keymap))
        (seen '()))
    (emacs-keymap-define-key parent "p" 'p)
    (emacs-keymap-define-key child  "c" 'c)
    (emacs-keymap-set-keymap-parent child parent)
    (emacs-keymap-map-keymap
     (lambda (k v) (push (cons k v) seen))
     child)
    (should (cl-find ?p seen :key #'car))
    (should (cl-find ?c seen :key #'car))))

(ert-deftest emacs-keymap-where-is-internal-finds-bindings ()
  (let ((m (emacs-keymap-make-sparse-keymap)))
    (emacs-keymap-define-key m "x" 'frobnicate)
    (emacs-keymap-define-key m "y" 'frobnicate)
    (let ((found (emacs-keymap-where-is-internal 'frobnicate m)))
      (should (= 2 (length found)))
      (should (cl-some (lambda (v) (equal v (vector ?x))) found))
      (should (cl-some (lambda (v) (equal v (vector ?y))) found)))))

(ert-deftest emacs-keymap-substitute-key-definition-rewrites ()
  (let ((m (emacs-keymap-make-sparse-keymap)))
    (emacs-keymap-define-key m "a" 'old-cmd)
    (emacs-keymap-define-key m "b" 'old-cmd)
    (emacs-keymap-define-key m "c" 'untouched)
    (emacs-keymap-substitute-key-definition 'old-cmd 'new-cmd m)
    (should (eq 'new-cmd (emacs-keymap-lookup-key m "a")))
    (should (eq 'new-cmd (emacs-keymap-lookup-key m "b")))
    (should (eq 'untouched (emacs-keymap-lookup-key m "c")))))

(ert-deftest emacs-keymap-key-description-formats-control ()
  ;; ASCII single char
  (should (string-equal "a" (emacs-keymap-key-description "a")))
  ;; control char (C-a == ?\x01)
  (should (string-equal "C-a" (emacs-keymap-key-description (vector ?\C-a))))
  ;; SPC / RET / TAB / DEL
  (should (string-equal "SPC" (emacs-keymap-key-description (vector ?\s))))
  (should (string-equal "RET" (emacs-keymap-key-description (vector ?\r))))
  (should (string-equal "TAB" (emacs-keymap-key-description (vector ?\t))))
  (should (string-equal "DEL" (emacs-keymap-key-description (vector 127))))
  ;; symbol (function-key) passes through
  (should (string-equal "f1" (emacs-keymap-key-description (vector 'f1)))))

;;;; E. minimal command-loop scaffolding (3 tests)

(ert-deftest emacs-keymap-read-key-sequence-single ()
  (emacs-keymap-test--with-fresh-world
    (let ((m (emacs-keymap-make-sparse-keymap)))
      (emacs-keymap-define-key m "a" 'cmd-a)
      (emacs-keymap-use-global-map m)
      (setq emacs-keymap--input-queue (list ?a))
      (let ((seq (emacs-keymap-read-key-sequence nil)))
        (should (equal (vector ?a) seq))
        (should (equal (vector ?a) (emacs-keymap-this-command-keys-vector)))))))

(ert-deftest emacs-keymap-read-key-sequence-prefix-then-bind ()
  (emacs-keymap-test--with-fresh-world
    (let ((m (emacs-keymap-make-sparse-keymap)))
      (emacs-keymap-define-key m "ab" 'cmd-ab)
      (emacs-keymap-use-global-map m)
      (setq emacs-keymap--input-queue (list ?a ?b))
      (let ((seq (emacs-keymap-read-key-sequence nil)))
        (should (equal (vector ?a ?b) seq))))))

(ert-deftest emacs-keymap-this-command-keys-string-vs-vector ()
  (emacs-keymap-test--with-fresh-world
    (let ((m (emacs-keymap-make-sparse-keymap)))
      (emacs-keymap-define-key m "x" 'cmd-x)
      (emacs-keymap-use-global-map m)
      (setq emacs-keymap--input-queue (list ?x))
      (emacs-keymap-read-key-sequence nil)
      ;; all-character => string
      (should (stringp (emacs-keymap-this-command-keys)))
      ;; vector form regardless
      (should (vectorp (emacs-keymap-this-command-keys-vector))))))

;;;; F. Doc 41 §2.5 chain inject opt-in (3 tests)

(ert-deftest emacs-keymap-chain-inject-flag-default-7-stage ()
  (emacs-keymap-test--with-fresh-world
    ;; even with providers wired, flag = nil keeps chain Doc 34 7 段
    (setq emacs-keymap-chain-overlay-provider
          (lambda (_pt) (list (emacs-keymap-make-sparse-keymap))))
    (setq emacs-keymap-chain-textprop-provider
          (lambda (_pt) (list (emacs-keymap-make-sparse-keymap))))
    (let* ((c (emacs-keymap-chain-at 1)))
      ;; only global (no local), no overlay/textprop slot
      (should (= 1 (length c))))))

(ert-deftest emacs-keymap-chain-inject-flag-on-9-stage ()
  (emacs-keymap-test--with-fresh-world
    (let ((overlay-km  (emacs-keymap-make-sparse-keymap))
          (textprop-km (emacs-keymap-make-sparse-keymap)))
      (setq emacs-keymap-chain-with-textprop t
            emacs-keymap-chain-overlay-provider  (lambda (_pt) (list overlay-km))
            emacs-keymap-chain-textprop-provider (lambda (_pt) (list textprop-km)))
      (let ((c (emacs-keymap-chain-at 1)))
        ;; global + overlay + textprop = 3
        (should (= 3 (length c)))
        ;; precedence: overlay before textprop before global (per Doc 41
        ;; §2.5: slot 6 overlay > slot 7 textprop > slot 9 global)
        (should (eq overlay-km  (nth 0 c)))
        (should (eq textprop-km (nth 1 c)))))))

(ert-deftest emacs-keymap-contract-version-constants ()
  (should (= 1 emacs-keymap-contract-version))
  (should (= 1 emacs-keymap-chain-inject-contract-version)))

;;;; G. newer kbd-style API (Phase 1 §4.4, 16 tests)

(ert-deftest emacs-keymap-keymap-set-roundtrip-single-key ()
  (let ((m (emacs-keymap-make-sparse-keymap)))
    (emacs-keymap-keymap-set m "a" 'self-insert)
    (should (eq 'self-insert (emacs-keymap-keymap-lookup m "a")))))

(ert-deftest emacs-keymap-keymap-set-with-prefix ()
  (let ((m (emacs-keymap-make-sparse-keymap)))
    (emacs-keymap-keymap-set m "C-x C-f" 'find-file)
    (should (eq 'find-file (emacs-keymap-keymap-lookup m "C-x C-f")))
    ;; The prefix slot must itself be a keymap (so further keys land
    ;; in the same prefix).
    (should (emacs-keymap-keymapp
             (emacs-keymap-keymap-lookup m "C-x")))))

(ert-deftest emacs-keymap-define-keymap-prefix-source-shape ()
  "define-keymap :prefix is implemented (no longer a not-implemented stub).
The standalone `define-keymap' fallback is gated on the reader (`nl-write-file'),
so host ERT pins the source shape; the live behaviour (the symbol's function and
value cells become the keymap) is exercised by the standalone boot."
  (let* ((lib (locate-library "emacs-keymap"))
         (src (and lib (concat (file-name-sans-extension lib) ".el")))
         (source (and src (file-readable-p src)
                      (with-temp-buffer (insert-file-contents src)
                                        (buffer-string)))))
    (should source)
    (should-not (string-match-p
                 (regexp-quote "define-keymap :prefix is not implemented")
                 source))
    (dolist (needle '("(setq prefix value)"
                      "(fset prefix m)"
                      "(set prefix m)"))
      (should (string-match-p (regexp-quote needle) source)))))

(ert-deftest emacs-keymap-defvar-keymap-source-materializes-keymaps ()
  "Pin the standalone `defvar-keymap' fallback used by vendored mode maps."
  (let* ((lib (locate-library "emacs-keymap"))
         (src (and lib (concat (file-name-sans-extension lib) ".el")))
         (source (and src (file-readable-p src)
                      (with-temp-buffer (insert-file-contents src)
                                        (buffer-string)))))
    (should source)
    (dolist (needle '("(defun emacs-keymap--defvar-keymap-build"
                      "(defmacro defvar-keymap"
                      "((eq keyword :doc) (setq doc (pop defs)))"
                      "((eq keyword :parent) (setq parent (pop defs)))"
                      "((eq keyword :suppress) (setq suppress (pop defs)))"
                      "(let ((map (or base (emacs-keymap-make-sparse-keymap))))"
                      "(keymap-set map key def)"
                      "(list 'emacs-keymap--defvar-keymap-build"
                      "(cons 'list defs)"))
      (should (string-match-p (regexp-quote needle) source)))))

(ert-deftest emacs-keymap-keymap-set-invalid-syntax-signals ()
  (let ((m (emacs-keymap-make-sparse-keymap)))
    (should-error (emacs-keymap-keymap-set m "not a kbd" 'foo)
                  :type 'emacs-keymap-bad-key)))

(ert-deftest emacs-keymap-keymap-set-string-def-is-parsed ()
  (let ((m (emacs-keymap-make-sparse-keymap)))
    ;; DEF is a kbd string -> stored as the parsed key vector.
    (emacs-keymap-keymap-set m "C-a" "C-x C-f")
    (should (equal (key-parse "C-x C-f")
                   (emacs-keymap-keymap-lookup m "C-a")))))

(ert-deftest emacs-keymap-keymap-set-string-def-invalid-signals ()
  (let ((m (emacs-keymap-make-sparse-keymap)))
    (should-error (emacs-keymap-keymap-set m "C-a" "not a kbd")
                  :type 'emacs-keymap-bad-key)))

(ert-deftest emacs-keymap-keymap-lookup-invalid-syntax-signals ()
  (let ((m (emacs-keymap-make-sparse-keymap)))
    (should-error (emacs-keymap-keymap-lookup m "not a kbd")
                  :type 'emacs-keymap-bad-key)))

(ert-deftest emacs-keymap-keymap-lookup-absent-returns-nil ()
  (let ((m (emacs-keymap-make-sparse-keymap)))
    (should-not (emacs-keymap-keymap-lookup m "C-q"))))

(ert-deftest emacs-keymap-keymap-unset-removes-binding ()
  (let ((m (emacs-keymap-make-sparse-keymap)))
    (emacs-keymap-keymap-set m "a" 'self-insert)
    (should (eq 'self-insert (emacs-keymap-keymap-lookup m "a")))
    (emacs-keymap-keymap-unset m "a")
    (should-not (emacs-keymap-keymap-lookup m "a"))))

(ert-deftest emacs-keymap-keymap-global-set-and-lookup ()
  (emacs-keymap-test--with-fresh-world
    (emacs-keymap-keymap-global-set "C-x C-s" 'save-buffer)
    (should (eq 'save-buffer
                (emacs-keymap-keymap-lookup
                 (emacs-keymap-current-global-map) "C-x C-s")))))

(ert-deftest emacs-keymap-keymap-local-set-auto-creates ()
  (emacs-keymap-test--with-fresh-world
    (should-not (emacs-keymap-current-local-map))
    (emacs-keymap-keymap-local-set "C-c x" 'my-cmd)
    (let ((local (emacs-keymap-current-local-map)))
      (should (emacs-keymap-keymapp local))
      (should (eq 'my-cmd (emacs-keymap-keymap-lookup local "C-c x"))))))

(ert-deftest emacs-keymap-keymap-local-set-uses-existing ()
  (emacs-keymap-test--with-fresh-world
    (let ((local (emacs-keymap-make-sparse-keymap)))
      (emacs-keymap-use-local-map local)
      (emacs-keymap-keymap-local-set "C-c x" 'my-cmd)
      ;; The existing local map must be the one mutated, not a fresh one.
      (should (eq local (emacs-keymap-current-local-map)))
      (should (eq 'my-cmd (emacs-keymap-keymap-lookup local "C-c x"))))))

(ert-deftest emacs-keymap-keymap-global-unset ()
  (emacs-keymap-test--with-fresh-world
    (emacs-keymap-keymap-global-set "C-x C-s" 'save-buffer)
    (emacs-keymap-keymap-global-unset "C-x C-s")
    (should-not (emacs-keymap-keymap-lookup
                 (emacs-keymap-current-global-map) "C-x C-s"))))

(ert-deftest emacs-keymap-keymap-local-unset-no-local-map-is-noop ()
  (emacs-keymap-test--with-fresh-world
    (should-not (emacs-keymap-current-local-map))
    ;; No local map: just returns nil without error.
    (should-not (emacs-keymap-keymap-local-unset "C-c x"))))

(ert-deftest emacs-keymap-key-parse-and-key-valid-p-delegate ()
  ;; Phase 1 wrappers must agree with upstream.
  (should (equal (key-parse "C-x C-f") (emacs-keymap-key-parse "C-x C-f")))
  (should (eq (key-valid-p "C-x C-f")
              (emacs-keymap-key-valid-p "C-x C-f")))
  (should-not (emacs-keymap-key-valid-p "not a kbd")))

(ert-deftest emacs-keymap-standalone-key-parser-common-keys ()
  (should (equal [113] (emacs-keymap--standalone-key-parse "q")))
  (should (equal [24 6] (emacs-keymap--standalone-key-parse "C-x C-f")))
  (should (equal [?/ ?s] (emacs-keymap--standalone-key-parse "/ s")))
  (should (equal [?/ ?s]
                 (emacs-keymap--standalone-key-parse "  /   s  ")))
  (cl-letf (((symbol-function 'split-string)
             (lambda (&rest _args)
               (error "standalone parser must not call split-string"))))
    (should (equal [?/ ?h]
                   (emacs-keymap--standalone-key-parse "/ h"))))
  (should (equal [32] (emacs-keymap--standalone-key-parse "SPC")))
  (should (equal [127] (emacs-keymap--standalone-key-parse "DEL")))
  (should (equal [left] (emacs-keymap--standalone-key-parse "<left>")))
  (should (equal [f10] (emacs-keymap--standalone-key-parse "f10")))
  (should (equal (emacs-keymap--standalone-key-parse "f10")
                 (emacs-keymap--standalone-key-parse "<f10>")))
  (should (emacs-keymap--standalone-key-valid-p "S-SPC"))
  (should-not (emacs-keymap--standalone-key-valid-p "9bad-token")))

(ert-deftest emacs-keymap-read-kbd-macro-source-reclaimed-from-stub ()
  "Regression pin for Doc 49 T49 (real-init audit round 6, form 206,
symptom: `(require 'evil)' signalled `emacs-keymap-bad-key: (nil)').

Root cause: `read-kbd-macro' is in `emacs-stub-bulk.el's bulk stub list,
which installs a permanently nil-returning function for any symbol not
already `fboundp' when the bulk stubs load.  `kbd' and `key-parse' are
reclaimed from that same stub set a few lines below in this file, but
`read-kbd-macro' previously was not, so callers going through the older
`read-kbd-macro' name directly (e.g. evil-maps.el's three
`(read-kbd-macro evil-toggle-key)' bindings) got nil back instead of a
parsed key, and `(define-key MAP nil DEF)' signalled
`emacs-keymap-bad-key' with data `(nil)' -- indistinguishable at the
call site from a genuinely nil KEY argument.

The live standalone behaviour (evil now loads past this point) is
exercised by the require-evil boot smoke, not host ERT: the `defalias'
below is gated behind `(fboundp 'nl-write-file)' and so never fires
under host Emacs.  This test pins the source shape instead."
  (let* ((lib (locate-library "emacs-keymap"))
         (src (and lib (concat (file-name-sans-extension lib) ".el")))
         (source (and src (file-readable-p src)
                      (with-temp-buffer (insert-file-contents src)
                                        (buffer-string)))))
    (should source)
    (should (string-match-p
             (regexp-quote
              "(defalias 'read-kbd-macro #'emacs-keymap--standalone-key-parse)")
             source))))

(ert-deftest emacs-keymap-standalone-key-parser-matches-host-key-parse ()
  "Parity sweep (Doc 49 T49): the standalone kbd-style fallback parser
must produce the exact same key sequence as GNU's real `key-parse' for
every description class vendored packages actually write: plain
printing characters, multi-token sequences, the named control-char
words (SPC/TAB/RET/DEL/ESC/LFD/NUL), Control/Meta/Shift/Super/Hyper/Alt
modifiers on a plain character, angle-bracket function/event names
without a modifier, and angle-bracket names with one or many combined
modifiers.

The all-six-modifier and swapped-order cases pin a real bug found via
this exact sweep: GNU does not canonicalize modifier order for these
event symbols (`kbd \"M-C-<f1>\"' and `kbd \"C-M-<f1>\"' are the two
distinct symbols `M-C-f1' and `C-M-f1', not the same key), so the
standalone parser must preserve the written prefix order verbatim
rather than rebuild a prefix from a bitmask -- collapsing e.g. \"C-M-\"
down to a single recognized bit previously returned the bare `up'
symbol for `(kbd \"C-M-<up>\")', silently dropping both modifiers."
  (dolist (d '("q" "C-x C-f" "M-x" "C-M-f" "C-S-a" "s-x" "H-x" "A-x"
               "C-c C-c" "z RET" "C-." "M-." "C-=" "C-^" "C-6" "C-z"
               "TAB" "RET" "SPC" "DEL" "ESC" "LFD" "NUL"
               "<escape>" "<f5>" "<mouse-1>"
               "S-<f5>" "M-<return>" "C-M-<up>" "M-C-<up>"
               "C-S-<f1>" "A-C-M-H-S-s-<f1>" "s-S-H-M-C-A-<f1>"))
    (should (equal (key-parse d) (emacs-keymap--standalone-key-parse d)))))

(provide 'emacs-keymap-test)
;;; emacs-keymap-test.el ends here
