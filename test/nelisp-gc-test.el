;;; nelisp-gc-test.el --- Phase 3c.1 root-set ERTs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 3c.1 scope: root-set enumeration only.  Mark pass,
;; finalizers, and MCP introspection get their own test files as
;; each sub-phase lands (3c.2 onward).  See
;; docs/design/09-phase3c-gc.org §3.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp)
(require 'nelisp-bytecode)
(require 'nelisp-gc)

;;; Globals root ------------------------------------------------------

(ert-deftest nelisp-gc-root-set-includes-four-global-tables ()
  "Root set lists `nelisp--globals' etc. as independent entries."
  (let ((kinds (mapcar (lambda (r) (plist-get r :kind))
                       (nelisp-gc-root-set))))
    (should (memq 'nelisp--globals   kinds))
    (should (memq 'nelisp--functions kinds))
    (should (memq 'nelisp--macros    kinds))
    (should (memq 'nelisp--specials  kinds))))

(ert-deftest nelisp-gc-root-set-global-entries-carry-hash-tables ()
  "Each global-table root entry's :value is the actual hash-table
(identity compare, not a copy)."
  (dolist (r (nelisp-gc-root-set))
    (when (memq (plist-get r :kind)
                '(nelisp--globals nelisp--functions
                  nelisp--macros nelisp--specials))
      (should (hash-table-p (plist-get r :value)))
      (should (eq (plist-get r :value)
                  (symbol-value (plist-get r :kind)))))))

;;; VM stack root -----------------------------------------------------

(ert-deftest nelisp-gc-no-vm-stack-outside-nelisp-bc-run ()
  "With no bytecode execution in flight, `vm-stack' root entries absent."
  (let ((nelisp-gc--active-vms nil))
    (should-not
     (cl-find 'vm-stack (nelisp-gc-root-set)
              :key (lambda (r) (plist-get r :kind))))))

(ert-deftest nelisp-gc-active-vm-surfaces-in-root-set ()
  "A VM bound into `nelisp-gc--active-vms' becomes a `vm-stack' root entry."
  (let* ((fake-vm (vector 'mock 'vm 'state))
         (nelisp-gc--active-vms (list fake-vm))
         (vm-roots (cl-remove-if-not
                    (lambda (r) (eq (plist-get r :kind) 'vm-stack))
                    (nelisp-gc-root-set))))
    (should (= 1 (length vm-roots)))
    (should (eq fake-vm (plist-get (car vm-roots) :value)))))

(ert-deftest nelisp-gc-vm-stack-appears-while-bc-run ()
  "`nelisp-bc-run' pushes its VM state vector onto `nelisp-gc--active-vms'
before entering dispatch.

Intercept `nelisp-bc--dispatch' via `cl-letf' to snapshot the dynamic
binding at the exact moment control reaches dispatch — that frames
the push-site guarantee without relying on CALL-opcode dispatch of a
test-local probe function (which would itself be subject to the
bcl/host-apply routing the test is meant to be orthogonal to)."
  (let* ((snapshot nil)
         (orig (symbol-function 'nelisp-bc--dispatch))
         (probe-bcl (nelisp-bc-compile '(lambda () 42))))
    (cl-letf (((symbol-function 'nelisp-bc--dispatch)
               (lambda (vm nested)
                 (unless snapshot
                   (setq snapshot nelisp-gc--active-vms))
                 (funcall orig vm nested))))
      (nelisp-bc-run probe-bcl))
    (should (consp snapshot))
    (should (vectorp (car snapshot)))
    ;; Pushed VM is the dispatching VM itself (slot 0 = code vector).
    (should (arrayp (aref (car snapshot) 0)))))

(ert-deftest nelisp-gc-vm-stack-pops-on-normal-exit ()
  "After `nelisp-bc-run' returns, `nelisp-gc--active-vms' is empty again."
  (let ((before nelisp-gc--active-vms)
        (probe (nelisp-bc-compile '(lambda () 1))))
    (nelisp-bc-run probe)
    (should (equal before nelisp-gc--active-vms))))

(ert-deftest nelisp-gc-vm-stack-pops-on-non-local-exit ()
  "A `throw' past `nelisp-bc-run' still pops the active-VM entry —
dynamic `let' + `unwind-protect' inside `nelisp-bc-run' guarantees
the cleanup regardless of how control leaves the body."
  (let ((before nelisp-gc--active-vms)
        (thrower (nelisp-bc-compile
                  '(lambda () (throw 'nelisp-gc-test--tag 'ok)))))
    (catch 'nelisp-gc-test--tag (nelisp-bc-run thrower))
    (should (equal before nelisp-gc--active-vms))))

;;; Mark pass (3c.2) -------------------------------------------------

(defun nelisp-gc-test--root-of (value)
  "Wrap VALUE in the plist shape `nelisp-gc-reachable-set' expects."
  (list (list :kind 'test :value value)))

(ert-deftest nelisp-gc-mark-single-cons ()
  (let* ((obj '(1 . 2))
         (live (nelisp-gc-reachable-set (nelisp-gc-test--root-of obj))))
    (should (gethash obj live))))

(ert-deftest nelisp-gc-mark-flat-list ()
  (let* ((obj (list 'a 'b 'c))
         (live (nelisp-gc-reachable-set (nelisp-gc-test--root-of obj))))
    ;; Each cons in the spine + the terminating nil are walked; symbols
    ;; themselves are leaves and count only if they were ever `push'ed
    ;; and then `puthash'ed.  We check spine identity to avoid tying
    ;; the assertion to the leaf treatment of symbols.
    (should (gethash obj live))
    (should (gethash (cdr obj) live))
    (should (gethash (cddr obj) live))))

(ert-deftest nelisp-gc-mark-circular-list-halts ()
  "A circular cons chain must not hang the walker — visited HT closes
the cycle."
  (let* ((a (list 'x))
         (_ (setcdr a a))          ; a -> a
         (live (nelisp-gc-reachable-set (nelisp-gc-test--root-of a))))
    ;; Single cell is the entire live set (symbol 'x is a leaf).
    (should (gethash a live))
    (should (<= (hash-table-count live) 2))))

(ert-deftest nelisp-gc-mark-nested-cons ()
  "Nested cons walks into both `car' and `cdr'."
  (let* ((inner (cons 'i 'j))
         (outer (cons inner 'tail))
         (live (nelisp-gc-reachable-set (nelisp-gc-test--root-of outer))))
    (should (gethash outer live))
    (should (gethash inner live))))

(ert-deftest nelisp-gc-mark-vector ()
  (let* ((v (vector (cons 1 2) (cons 3 4) 'leaf))
         (live (nelisp-gc-reachable-set (nelisp-gc-test--root-of v))))
    (should (gethash v live))
    (should (gethash (aref v 0) live))
    (should (gethash (aref v 1) live))))

(ert-deftest nelisp-gc-mark-hash-table-keys-and-values ()
  (let ((h (make-hash-table :test 'eq))
        (k (cons 'k 1))
        (v (cons 'v 2)))
    (puthash k v h)
    (let ((live (nelisp-gc-reachable-set (nelisp-gc-test--root-of h))))
      (should (gethash h live))
      (should (gethash k live))
      (should (gethash v live)))))

(ert-deftest nelisp-gc-mark-cross-referenced-graph ()
  "A graph where two roots share a leaf shows the leaf once only."
  (let* ((shared (cons 'shared nil))
         (root-1 (list shared 'a))
         (root-2 (list shared 'b))
         (live   (nelisp-gc-reachable-set
                  (list (list :kind 'r1 :value root-1)
                        (list :kind 'r2 :value root-2)))))
    (should (gethash shared live))
    (should (gethash root-1 live))
    (should (gethash root-2 live))))

(ert-deftest nelisp-gc-mark-closure-shape ()
  "A `nelisp-closure'-shaped cons is walked as an ordinary list —
no special-case needed per design doc §5.2."
  (let* ((env   '((x . 1) (y . 2)))
         (body  '((+ x y)))
         (cl    (list 'nelisp-closure env '(a) body))
         (live  (nelisp-gc-reachable-set (nelisp-gc-test--root-of cl))))
    (should (gethash cl live))
    (should (gethash env live))
    (should (gethash body live))))

(ert-deftest nelisp-gc-reachable-count-matches-table-size ()
  "`nelisp-gc-reachable-count' is a hash-table-count convenience."
  (let* ((obj (cons 1 2))
         (roots (nelisp-gc-test--root-of obj)))
    (should (= (nelisp-gc-reachable-count roots)
               (hash-table-count (nelisp-gc-reachable-set roots))))))

(ert-deftest nelisp-gc-mark-default-roots-includes-globals-table ()
  "The default root set (no override) always reaches the four global
hash-tables themselves as first-class objects."
  (let ((live (nelisp-gc-reachable-set)))
    (should (gethash nelisp--globals   live))
    (should (gethash nelisp--functions live))
    (should (gethash nelisp--macros    live))
    (should (gethash nelisp--specials  live))))

;;; Finalizers (3c.3) -------------------------------------------------

(defun nelisp-gc-test--clear-finalizers ()
  "Wipe all registered finalizers — isolate each ERT from leftovers."
  (clrhash nelisp-gc--finalizers))

(ert-deftest nelisp-gc-finalizer-fires-on-explicit-collect ()
  "Object unreachable from NeLisp roots → its finalizer fires on
`nelisp-gc-collect'."
  (nelisp-gc-test--clear-finalizers)
  (let* ((ran 0)
         (orphan (cons 'o 1)))
    (nelisp-gc-register-finalizer
     orphan (lambda (_obj) (cl-incf ran)))
    (should (= 1 (nelisp-gc-collect)))
    (should (= 1 ran))
    ;; Entry removed after firing so a second collect is a no-op.
    (should (= 0 (nelisp-gc-collect)))))

(ert-deftest nelisp-gc-finalizer-skipped-for-reachable-object ()
  "Finalizer does not fire while OBJ is still reachable from a root."
  (nelisp-gc-test--clear-finalizers)
  (let* ((ran 0)
         (held (cons 'held 'in-globals)))
    (unwind-protect
        (progn
          (puthash 'nelisp-gc-test--sentinel held nelisp--globals)
          (nelisp-gc-register-finalizer
           held (lambda (_) (cl-incf ran)))
          (nelisp-gc-collect)
          (should (= 0 ran)))
      (remhash 'nelisp-gc-test--sentinel nelisp--globals))
    ;; Once the sentinel is removed from globals, OBJ is unreachable
    ;; and a second collect fires the finalizer.
    (should (= 1 (nelisp-gc-collect)))
    (should (= 1 ran))))

(ert-deftest nelisp-gc-finalizer-error-does-not-abort-sweep ()
  "Exception in one THUNK must not skip subsequent finalizers."
  (nelisp-gc-test--clear-finalizers)
  (let* ((ran-2 0)
         (bad-obj  (cons 'a 1))
         (good-obj (cons 'b 2)))
    (nelisp-gc-register-finalizer
     bad-obj (lambda (_) (error "boom")))
    (nelisp-gc-register-finalizer
     good-obj (lambda (_) (cl-incf ran-2)))
    ;; Both orphans → both attempted; good one runs even if bad errored.
    (should (= 2 (nelisp-gc-collect)))
    (should (= 1 ran-2))))

(ert-deftest nelisp-gc-unregister-removes-entry ()
  "`nelisp-gc-unregister-finalizer' prevents subsequent firing."
  (nelisp-gc-test--clear-finalizers)
  (let* ((ran 0)
         (obj (cons 'x 1)))
    (nelisp-gc-register-finalizer obj (lambda (_) (cl-incf ran)))
    (should (nelisp-gc-unregister-finalizer obj))
    (nelisp-gc-collect)
    (should (= 0 ran))))

(ert-deftest nelisp-gc-post-gc-handler-respects-auto-sweep-flag ()
  "`post-gc-hook' handler is a no-op unless `nelisp-gc-auto-sweep' is t."
  (nelisp-gc-test--clear-finalizers)
  (let* ((ran 0)
         (orphan (cons 'p 1)))
    (nelisp-gc-register-finalizer orphan (lambda (_) (cl-incf ran)))
    ;; auto-sweep disabled → handler does nothing.
    (let ((nelisp-gc-auto-sweep nil))
      (nelisp-gc--post-gc-handler)
      (should (= 0 ran)))
    ;; auto-sweep enabled → handler runs a collect sweep.
    (let ((nelisp-gc-auto-sweep t))
      (nelisp-gc--post-gc-handler)
      (should (= 1 ran)))))

(provide 'nelisp-gc-test)
;;; nelisp-gc-test.el ends here
