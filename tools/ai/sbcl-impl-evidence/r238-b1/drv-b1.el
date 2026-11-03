;;; -*- lexical-binding: t; -*-
;;; B1 negative controls.  Append to a benchmark driver AFTER the native kernels
;;; are installed and BEFORE its final (kill-emacs 0) (see run/b5/drv-subr.el,
;;; which already defines bench-input, bench-hash-get1024 and the native sites).
;;; The native site under test is the `gethash' call inside bench-hash-get1024;
;;; it is the same site on every call.  Each case warms the site (fills the
;;; cache), changes the definition between two calls, and requires the very
;;; next call to take the new definition.

(defun b1-check (tag ok) (princ (format "%s ok=%S\n" tag (and ok t))))

(let* ((k 64)
       (hin (bench-input "hash-get1024"))
       (r0 (bench-hash-get1024 k hin))
       (n 0))
  ;; Warm: several calls so the site cache is stamped for the real gethash.
  (bench-hash-get1024 k hin) (bench-hash-get1024 k hin)
  (princ (format "B1-EPOCH start=%S\n" (nelisp--function-epoch)))
  ;; (a) fset of the cached builtin to a counting lambda, then restore.
  (let ((orig (symbol-function 'gethash)) (e0 (nelisp--function-epoch)))
    (fset 'gethash (lambda (&rest a) (setq n (1+ n)) (apply orig a)))
    (let ((r (bench-hash-get1024 k hin)))
      ;; Bind the verdict before calling `b1-check': with gethash replaced, the
      ;; call of a Lisp function itself reaches gethash through the symbol
      ;; (about 40 calls, also on build21), which would inflate N.
      (let ((ok (and (= n k) (equal r r0) (> (nelisp--function-epoch) e0))))
        (b1-check "B1-FSET" ok)))
    (fset 'gethash orig)
    (setq n 0)
    (let ((r (bench-hash-get1024 k hin)))
      (b1-check "B1-RESTORE" (and (= n 0) (equal r r0)))))
  ;; (b) advice-add :around counting wrapper, then advice-remove.
  (setq n 0)
  (let ((wrap (lambda (f &rest a) (setq n (1+ n)) (apply f a))))
    (advice-add 'gethash :around wrap)
    (let ((r (bench-hash-get1024 k hin)))
      (b1-check "B1-ADVICE" (and (= n k) (equal r r0))))
    (advice-remove 'gethash wrap)
    (setq n 0)
    (let ((r (bench-hash-get1024 k hin)))
      (b1-check "B1-ADVICE-REMOVED" (and (= n 0) (equal r r0)))))
  ;; (b2) fset to a different builtin-shaped function between calls, then
  ;; defalias back: the cell word changes though the epoch is not consulted.
  (let ((orig (symbol-function 'gethash)))
    (defalias 'gethash (lambda (&rest a) (setq n (1+ n)) (apply orig a)))
    (setq n 0)
    (b1-check "B1-DEFALIAS" (progn (bench-hash-get1024 k hin) (= n k)))
    (defalias 'gethash orig)
    (setq n 0)
    (bench-hash-get1024 k hin)
    (b1-check "B1-DEFALIAS-RESTORE" (= n 0)))
  ;; (c) debug policy: only if standalone has `debug-on-next-call'.  The hit
  ;; path re-reads the policy word (env+112, mask 3145728) on every call, so
  ;; the toggle needs no epoch bump.  Informational: SKIP when unbound.
  (if (boundp 'debug-on-next-call)
      (let ((dn 0))
        (let ((debug-on-next-call t)
              (debugger (lambda (&rest _) (setq dn (1+ dn)))))
          (bench-hash-get1024 k hin))
        (princ (format "B1-DEBUG debugger-calls=%d (>0 expected; 0 means policy not observed)\n" dn))
        (b1-check "B1-DEBUG-AFTER" (equal (bench-hash-get1024 k hin) r0)))
    (princ "B1-DEBUG SKIP no debug-on-next-call\n"))
  ;; (d) mode bits read per call on the hit path (env+112, mask 3145728):
  ;; bit 20 is the variable-watcher mode (set by `add-variable-watcher', which
  ;; also bumps the epoch), bit 21 the dynamic-binding mode.  A warmed site
  ;; must give the same result while each mode is on and after it is off.
  (if (fboundp 'add-variable-watcher)
      (let ((w (lambda (&rest _) (setq b1--wn (1+ b1--wn)))))
        (defvar b1--watched 0) (defvar b1--wn 0)
        (add-variable-watcher 'b1--watched w)
        (let ((r (bench-hash-get1024 k hin)))
          (setq b1--watched 1)
          (b1-check "B1-WATCH" (and (equal r r0) (= b1--wn 1))))
        (remove-variable-watcher 'b1--watched w)
        (b1-check "B1-WATCH-REMOVED" (equal (bench-hash-get1024 k hin) r0)))
    (princ "B1-WATCH SKIP no add-variable-watcher\n"))
  (b1-check "B1-DYNAMIC"
            (equal (eval (list 'bench-hash-get1024 k (list 'quote hin)) nil) r0))
  (b1-check "B1-DYNAMIC-AFTER" (equal (bench-hash-get1024 k hin) r0))
  (princ (format "B1-EPOCH end=%S\n" (nelisp--function-epoch))))
