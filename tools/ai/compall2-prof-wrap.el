;;; compall2-prof-wrap.el --- the counting wrapper of tools/ai/compall2-prof.el  -*- lexical-binding: t; -*-
;; Compiled by GNU Emacs into byte code (tools/ai/compall2-prof-mkbc.el), because a
;; closure made at run time is not fx-compiled in NeLisp and runs about 3x slower
;; interpreted.  No `unwind-protect' (GNU compiles its handler into a closure made per
;; call): a non-local exit through a wrapped function loses that activation's time.
(defvar compall2-prof--on nil)
(defvar compall2-prof--child 0)
(defvar compall2-prof--calls nil)
(defvar compall2-prof--self nil)
(defvar compall2-prof--kids 0)
(defvar compall2-prof--nkids nil)
(defun compall2-prof--wrapper (i orig)
  "The counting wrapper of the function with index I and definition ORIG."
  (lambda (&rest args)
    (if (not compall2-prof--on)
        (apply orig args)
      (let ((t0 (nl-unix-time-usec)) (c0 compall2-prof--child) (k0 compall2-prof--kids))
        (setq compall2-prof--child 0 compall2-prof--kids 0)
        (prog1 (apply orig args)
          (let ((incl (- (nl-unix-time-usec) t0)))
            (aset compall2-prof--calls i (1+ (aref compall2-prof--calls i)))
            (aset compall2-prof--self i (+ (aref compall2-prof--self i) (- incl compall2-prof--child)))
            (aset compall2-prof--nkids i (+ (aref compall2-prof--nkids i) compall2-prof--kids))
            (setq compall2-prof--child (+ c0 incl) compall2-prof--kids (1+ k0))))))))
