;;; nelisp-eln-nonlocal-chain-driver.el --- Doc 207 non-local exit chains -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Ledger S4.6 (Doc 207).  Runs the same scenario matrix on host GNU Emacs
;; 31.1 and on the NeLisp binary and prints one `S46 ' transcript line per
;; scenario; the smoke requires the two transcripts to be identical.
;;
;; Native frames are the genuine GNU 31.1 artifacts gnu-chain.eln
;; (`(defun nelisp-gnu-chain (g x) (funcall g (1+ x)))': a non-tail Fadd1
;; slow-path CALL and a non-tail `Ffuncall' MANY CALL) and
;; gnu-increment.eln, loaded with ordinary `load' on both runtimes.  VM
;; frames are host-compiled byte code (NELISP_S46_VM_BYTECODE).
;;
;; On NeLisp the driver also checks, after every scenario, that every owner
;; the bridges take (object units, activations, identity records, callback
;; frames, pending cleanups, the special binding stack, and the pinned root
;; frame) is back at its baseline, and it counts callbacks to show that a
;; callback following an exit in the same native activation is suppressed.
;;
;; A callable cannot be encoded by the GNU object codec, so G is passed as
;; a fresh uninterned symbol (an encodable object) whose function cell is set
;; after the bridge has encoded it and before native code runs.  Native code
;; only stores G into the `Ffuncall' argument array; admission proves that.

;;; Code:

(defvar s46-depth)
(defvar s46-log)
(defvar s46-mode)
(defvar s46-tag)
(defvar s46-inner-g)
(defvar s46-leaf-calls)

(define-error 's46-error "S4.6 probe error")
(define-error 's46-unraised "S4.6 condition nothing raises")

(defconst s46-nelisp-p (and (fboundp 'nelisp-eln-objects-create) t)
  "Non-nil when running on NeLisp with the GNU .eln bridge.")

(defvar s46--armed nil "Alist (SYMBOL . FUNCTION) awaiting encoding.")
(defvar s46--encoded nil "Alist (UNIT . SYMBOL) encoded but not yet bound.")
(defvar s46--prep-units nil)
(defvar s46--dispatches 0)
(defvar s46--baseline nil)
(defvar s46--pin-base nil)
(defvar s46--failures 0)

(defun s46--load-inputs ()
  (load (getenv "NELISP_S46_VM_BYTECODE") nil t t)
  (load (getenv "NELISP_S46_CHAIN_ELN") nil t t)
  ;; The second genuine artifact defines `nelisp-gnu-increment'.
  (load (getenv "NELISP_S46_INCREMENT_ELN") nil t t)
  (unless (and (subrp (symbol-function 'nelisp-gnu-chain))
               (equal (func-arity (symbol-function 'nelisp-gnu-chain)) '(2 . 2))
               (subrp (symbol-function 'nelisp-gnu-increment))
               (equal (func-arity (symbol-function 'nelisp-gnu-increment))
                      '(1 . 1))
               (byte-code-function-p (symbol-function 's46-vm-outer))
               (byte-code-function-p (symbol-function 's46-vm-mid))
               (byte-code-function-p (symbol-function 's46-vm-leaf)))
    (error "S4.6 inputs are not genuine native subrs plus byte code")))

(defun s46--install-nelisp-instrumentation ()
  "Bind armed symbols after encoding; count callbacks.  NeLisp only."
  (let ((encode (symbol-function 'nelisp-eln-objects-encode))
        (acquire (symbol-function 'nelisp-eln-objects-activation-acquire))
        (dispatch (symbol-function 'nelisp-eln-callable-import--dispatch)))
    (fset 'nelisp-eln-objects-encode
          (lambda (unit value)
            (prog1 (funcall encode unit value)
              (when (and (symbolp value) (assq value s46--armed))
                (push (cons unit value) s46--encoded)))))
    (fset 'nelisp-eln-objects-activation-acquire
          (lambda (unit)
            (prog1 (funcall acquire unit)
              (let ((rest nil))
                (dolist (entry s46--encoded)
                  (if (eq (car entry) unit)
                      (let ((armed (assq (cdr entry) s46--armed)))
                        (fset (car armed) (cdr armed))
                        (setq s46--armed (delq armed s46--armed)))
                    (push entry rest)))
                (setq s46--encoded (nreverse rest))))))
    (fset 'nelisp-eln-callable-import--dispatch
          (lambda (descriptor)
            (setq s46--dispatches (1+ s46--dispatches))
            (funcall dispatch descriptor)))))

(defun s46--callable (function)
  "Return a symbol whose function is FUNCTION when native code calls it."
  (if s46-nelisp-p
      (let* ((unit (nelisp-eln-objects-create))
             (symbol (car (nelisp-eln-objects-make-uninterned-symbol
                           unit "s46-g"))))
        (push unit s46--prep-units)
        (push (cons symbol function) s46--armed)
        symbol)
    (let ((symbol (make-symbol "s46-g")))
      (fset symbol function)
      symbol)))

(defun s46--release-callables ()
  (when s46-nelisp-p
    (setq s46--armed nil s46--encoded nil)
    (dolist (unit s46--prep-units)
      (nelisp-eln-objects-release unit))
    (setq s46--prep-units nil)))

(defun s46--state ()
  (list (length nelisp-eln-objects--live-units)
        (length nelisp-eln-objects--activations)
        (length nelisp-eln-objects--identity-records)
        (length nelisp-eln-objects--pending-cleanups)
        (length nelisp-eln-callable-import--frames)
        (length nelisp-eln-callable-import--pending-cleanups)
        (nelisp-eln-runtime-services-specpdl-depth)))

(defun s46--pin-marker ()
  "Return the outermost pin marker a fresh frame gets; leave no frame."
  (let* ((env (nelisp--native-env))
         (marker (nelisp-native-load--pin-begin env)))
    (nelisp-native-load--pin-end env marker)
    marker))

(defun s46--check (name ok detail)
  (unless ok
    (setq s46--failures (1+ s46--failures))
    (princ (format "S46-FAIL %s %S\n" name detail))))

(defun s46--run (name thunk &optional dispatches leaf-calls)
  "Run THUNK as scenario NAME and print its transcript line.
DISPATCHES and LEAF-CALLS, when given, are the expected NeLisp callback
count and the expected number of leaf VM frame entries."
  (setq s46-log nil s46-leaf-calls 0 s46--dispatches 0)
  (let ((outcome
         (condition-case err
             (let ((tag s46-tag))
               (catch tag (list 'return (funcall thunk))))
           (quit (list 'quit err))
           (wrong-number-of-arguments
            ;; GNU reports the subr object, NeLisp its name: compare the
            ;; condition and the argument count only.
            (list 'error (car err) (car (last err))))
           (error (list 'error err)))))
    (s46--release-callables)
    (princ (format "S46 %s %S log=%S depth=%S leaf=%S\n"
                   name outcome (reverse s46-log) s46-depth s46-leaf-calls))
    (s46--check name (= s46-depth 0) 'depth)
    (when leaf-calls
      (s46--check name (= s46-leaf-calls leaf-calls)
                  (list 'leaf-calls s46-leaf-calls)))
    (when s46-nelisp-p
      (s46--check name (equal (s46--state) s46--baseline)
                  (list 'owners (s46--state) s46--baseline))
      (s46--check name (equal (s46--pin-marker) s46--pin-base) 'pin-frame)
      (when dispatches
        (s46--check name (= s46--dispatches dispatches)
                    (list 'dispatches s46--dispatches))))
    outcome))

(defun s46--chain (mode x &optional tag)
  "VM -> native chain -> VM leaf in MODE with X."
  (setq s46-mode mode s46-tag (or tag 's46-default-tag))
  (let ((g (s46--callable (symbol-function 's46-vm-leaf))))
    (s46-vm-outer g x)))

(defun s46--nested (mode x)
  "VM -> native -> VM mid -> native -> VM leaf in MODE with X."
  (setq s46-mode mode)
  (let ((g (s46--callable (symbol-function 's46-vm-mid)))
        (h (s46--callable (symbol-function 's46-vm-leaf))))
    (setq s46-inner-g h)
    (s46-vm-outer g x)))

(defun s46--native-native (mode x)
  "VM -> native chain -> native chain (through `apply-partially') -> VM leaf."
  (setq s46-mode mode)
  (let* ((h (s46--callable (symbol-function 's46-vm-leaf)))
         (g (s46--callable (apply-partially #'nelisp-gnu-chain h))))
    (s46-vm-outer g x)))

(s46--load-inputs)
(when s46-nelisp-p
  (s46--install-nelisp-instrumentation)
  (setq s46--baseline (s46--state)
        s46--pin-base (s46--pin-marker)))
(setq s46-tag 's46-default-tag)

;; VM -> native (non-tail) -> VM, every exit kind.  Two callbacks: none on
;; the fast path, one `Ffuncall' hop.
(s46--run "vm-native-vm/return" (lambda () (s46--chain 'return 5)) 1 1)
(s46--run "vm-native-vm/error" (lambda () (s46--chain 'error 5)) 1 1)
(let ((tag (copy-sequence "s46-string-tag")))
  ;; A non-symbol tag only matches its own `eq' object.
  (setq s46-tag tag)
  (s46--run "vm-native-vm/throw-string-tag"
            (lambda () (s46--chain 'throw 5 tag)) 1 1)
  (setq s46-tag 's46-default-tag))
(s46--run "vm-native-vm/quit" (lambda () (s46--chain 'quit 5)) 1 1)
;; The non-tail Fadd1 slow path runs first; its result (a float, then a
;; bignum) is decoded again for the `Ffuncall' hop.
(s46--run "vm-native-vm/slow-path-float-throw"
          (lambda () (s46--chain 'throw 0.5)) 2 1)
(s46--run "vm-native-vm/slow-path-bignum-return"
          (lambda () (s46--chain 'return most-positive-fixnum)) 2 1)
;; An error in the first callback: GNU never reaches the `Ffuncall' hop,
;; so the second callback is suppressed and the leaf never runs.
(s46--run "vm-native-vm/slow-path-error-suppresses-hop"
          (lambda () (s46--chain 'return "not-a-number")) 2 0)

;; Two native activations stacked on the machine stack with a VM frame
;; between them.
(s46--run "nested/return-gc" (lambda () (s46--nested 'return-gc 5)) 2 1)
(s46--run "nested/error" (lambda () (s46--nested 'error 5)) 2 1)
(s46--run "nested/throw" (lambda () (s46--nested 'throw 5)) 2 1)
(s46--run "nested/quit" (lambda () (s46--nested 'quit 5)) 2 1)
(s46--run "nested/slow-path-float-error"
          (lambda () (s46--nested 'error 0.5)) 4 1)

;; Native -> native: the outer chain's `Ffuncall' enters another native
;; frame directly, then through `apply-partially' down to the VM leaf.
(s46--run "native-native/increment"
          (lambda () (s46-vm-outer (s46--callable #'nelisp-gnu-increment) 5)))
(s46--run "native-native/increment-slow-paths"
          (lambda () (s46-vm-outer (s46--callable #'nelisp-gnu-increment) 0.5)))
(s46--run "native-native/arity-error"
          (lambda () (s46-vm-outer (s46--callable #'nelisp-gnu-chain) 5)))
(s46--run "native-native-vm/return" (lambda () (s46--native-native 'return 5)))
(s46--run "native-native-vm/error" (lambda () (s46--native-native 'error 5)))
(s46--run "native-native-vm/throw" (lambda () (s46--native-native 'throw 5)))
(s46--run "native-native-vm/quit" (lambda () (s46--native-native 'quit 5)))

(if (= s46--failures 0)
    (princ (format "S46-DRIVER-PASS %s\n" (if s46-nelisp-p "nelisp" "gnu")))
  (princ (format "S46-DRIVER-FAIL %d\n" s46--failures)))

;;; nelisp-eln-nonlocal-chain-driver.el ends here
