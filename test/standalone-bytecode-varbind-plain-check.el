;;; standalone-bytecode-varbind-plain-check.el --- bytecode Bvarbind regression -*- lexical-binding: t; -*-
;; Every case runs hand-assembled byte-code, so Bvarbind0/Bunbind are what is
;; exercised; no source-level `let' is used as a stand-in.  Run the same file
;; on GNU Emacs and on the NeLisp standalone runtime and compare.
;; BVP_BROKEN=1 flips one expected restoration so the check must FAIL.

(defvar bvp-plain 11)
(defvar bvp-unbound)
(defvar bvp-base 11)
(defvar bvp-local 11)
(defvar bvp-gc-var 11)
(defvar bvp-err-var 11)
(defvar bvp-fn nil)
(defvar bvp-buf-b nil)
(defvar bvp-count 0)
(defvar bvp-broken (equal (getenv "BVP_BROKEN") "1"))

(defun bvp-fail (name got expected)
  (let ((msg (format "BVP FAIL %s: got %S expected %S" name got expected)))
    (princ (concat (if (> (length msg) 300) (substring msg 0 300) msg) "\n"))
    (kill-emacs 1)))

(defun bvp-check (name got expected)
  (unless (equal got expected) (bvp-fail name got expected))
  (setq bvp-count (1+ bvp-count)))

;; push constants[1]; varbind constants[0]; varref0; unbind 1; return
(defun bvp-plain-fn (sym val)
  (make-byte-code 0 (unibyte-string 193 24 8 41 135) (vector sym val) 1))

;; push constants[1]; varbind constants[0]; push constants[2]; call0; unbind 1; return
(defun bvp-callback-fn (sym val observer)
  (make-byte-code 0 (unibyte-string 193 24 194 32 41 135)
                  (vector sym val observer) 1))

(defun bvp-need-bytecode (fn)
  (unless (byte-code-function-p fn) (bvp-fail "byte-code-function-p" fn t))
  fn)

(defun bvp-obs-value () (symbol-value 'bvp-plain))
(defun bvp-obs-unbound () (symbol-value 'bvp-unbound))
(defun bvp-obs-alias () (list (symbol-value 'bvp-alias) (symbol-value 'bvp-base)))
(defun bvp-obs-local ()
  (let ((first bvp-local) second other)
    (setq bvp-local 82)
    (setq second bvp-local)
    (setq other (with-current-buffer bvp-buf-b bvp-local))
    (list first second other bvp-local)))
(defun bvp-obs-gc ()
  (aset (aref bvp-fn 2) 1 nil)
  (dotimes (_ 200) (list 1 2 3))
  (garbage-collect)
  (symbol-value 'bvp-gc-var))
(defun bvp-obs-error () (signal 'error '("bvp-boom")))

;; (1) plain existing variable
(bvp-check "plain-inside"
           (funcall (bvp-need-bytecode (bvp-callback-fn 'bvp-plain 81 'bvp-obs-value)))
           81)
(bvp-check "plain-restored" bvp-plain (if bvp-broken 12 11))

;; (2) previously unbound symbol
(makunbound 'bvp-unbound)
(bvp-check "unbound-inside"
           (funcall (bvp-need-bytecode (bvp-callback-fn 'bvp-unbound 81 'bvp-obs-unbound)))
           81)
(bvp-check "unbound-restored" (boundp 'bvp-unbound) nil)
(bvp-check "unbound-plain-return"
           (funcall (bvp-need-bytecode (bvp-plain-fn 'bvp-unbound 81))) 81)
(bvp-check "unbound-plain-restored" (boundp 'bvp-unbound) nil)

;; (3) alias bound to a base variable
(defvaralias 'bvp-alias 'bvp-base)
(bvp-check "alias-inside"
           (funcall (bvp-need-bytecode (bvp-callback-fn 'bvp-alias 81 'bvp-obs-alias)))
           '(81 81))
(bvp-check "alias-restored" (list bvp-alias bvp-base) '(11 11))

;; (4) constant aliases must signal setting-constant
(defvaralias 'bvp-nil-alias nil)
(defvaralias 'bvp-true-alias t)
(dolist (spec '((bvp-nil-alias . nil) (bvp-true-alias . t)))
  (let* ((fn (bvp-need-bytecode (bvp-plain-fn (car spec) 81)))
         (res (condition-case e (list 'returned (funcall fn)) (error (car e)))))
    (bvp-check (format "constant-%s" (car spec)) res 'setting-constant)
    (bvp-check (format "constant-%s-value" (car spec))
               (symbol-value (car spec)) (cdr spec))))

;; (5) buffer-local variable, two buffers
(let ((a (generate-new-buffer " bvp-a")) (b (generate-new-buffer " bvp-b")))
  (unwind-protect
      (progn
        (setq bvp-buf-b b)
        (set-default 'bvp-local 11)
        (with-current-buffer a (set (make-local-variable 'bvp-local) 17))
        (with-current-buffer b (set (make-local-variable 'bvp-local) 23))
        (bvp-check "local-inside"
                   (with-current-buffer a
                     (funcall (bvp-need-bytecode
                               (bvp-callback-fn 'bvp-local 81 'bvp-obs-local))))
                   '(81 82 23 82))
        (bvp-check "local-restored"
                   (list (with-current-buffer a bvp-local)
                         (with-current-buffer b bvp-local)
                         (default-value 'bvp-local))
                   '(17 23 11)))
    (setq bvp-buf-b nil)
    (kill-buffer a)
    (kill-buffer b)))

;; (6) GC while bound; the only reference to (11 29) is constants[1]
(setq bvp-fn (bvp-need-bytecode
              (make-byte-code 0 (unibyte-string 193 24 194 32 41 135)
                              (vector 'bvp-gc-var (list 11 29) 'bvp-obs-gc) 1)))
(bvp-check "gc-inside" (funcall bvp-fn) '(11 29))
(bvp-check "gc-constant-cleared" (aref (aref bvp-fn 2) 1) nil)
(bvp-check "gc-restored" bvp-gc-var 11)
(setq bvp-fn nil)

;; (7) nonlocal error out of the observer
(bvp-check "error-caught"
           (condition-case e
               (funcall (bvp-need-bytecode
                         (bvp-callback-fn 'bvp-err-var 81 'bvp-obs-error)))
             (error (list 'caught e)))
           '(caught (error "bvp-boom")))
(bvp-check "error-restored" bvp-err-var 11)

(princ (format "BVP PASS checked=%d\n" bvp-count))
(kill-emacs 0)
