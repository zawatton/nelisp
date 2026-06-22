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
;;   G. newer kbd-style API (Phase 1 §4.4)          (15 tests)
;; Total: 50 tests

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

;;;; G. newer kbd-style API (Phase 1 §4.4, 14 tests)

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
  (should (equal [32] (emacs-keymap--standalone-key-parse "SPC")))
  (should (equal [127] (emacs-keymap--standalone-key-parse "DEL")))
  (should (equal [left] (emacs-keymap--standalone-key-parse "<left>")))
  (should (emacs-keymap--standalone-key-valid-p "S-SPC"))
  (should-not (emacs-keymap--standalone-key-valid-p "not a kbd")))

(provide 'emacs-keymap-test)
;;; emacs-keymap-test.el ends here
