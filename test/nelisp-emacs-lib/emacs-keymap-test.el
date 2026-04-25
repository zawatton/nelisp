;;; emacs-keymap-test.el --- ERT tests for emacs-keymap.el  -*- lexical-binding: t; -*-

;; Phase 1 module 4/6 tests per nelisp-emacs Doc 01.
;; Covers all 5 categories of `emacs-keymap-*' API plus Doc 41 §2.5
;; KEYMAP_CHAIN_INJECT_CONTRACT_VERSION = 1 opt-in semantics.
;;
;; Categories:
;;   A. constructors / predicates / copy           (4 tests)
;;   B. mutators / accessors                        (7 tests)
;;   C. global / local / overriding + chain         (8 tests)
;;   D. lookup helpers                              (6 tests)
;;   E. minimal command-loop scaffolding            (3 tests)
;;   F. Doc 41 §2.5 chain inject opt-in             (3 tests)
;; Total: 31 tests (≥ task spec 20+)

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
    ;; tail should have one (t . VECTOR) cell
    (should (cl-some (lambda (e)
                       (and (consp e) (eq (car e) t) (vectorp (cdr e))
                            (= 256 (length (cdr e)))))
                     (cdr m)))))

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

(ert-deftest emacs-keymap-set-keymap-parent-and-inherit ()
  (let ((parent (emacs-keymap-make-sparse-keymap))
        (child  (emacs-keymap-make-sparse-keymap)))
    (emacs-keymap-define-key parent "z" 'parent-z)
    (emacs-keymap-set-keymap-parent child parent)
    (should (eq parent (emacs-keymap-keymap-parent child)))
    ;; inherit lookup
    (should (eq 'parent-z (emacs-keymap-lookup-key child "z")))
    ;; child shadows parent
    (emacs-keymap-define-key child "z" 'child-z)
    (should (eq 'child-z (emacs-keymap-lookup-key child "z")))))

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

(provide 'emacs-keymap-test)
;;; emacs-keymap-test.el ends here
