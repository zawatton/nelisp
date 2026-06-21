;;; thunk.el --- delayed (lazy, memoized) evaluation  -*- lexical-binding: t; -*-

;;; Commentary:

;; Doc 16 breadth round 20.  A lean reimplementation of the GNU `thunk'
;; package for the NeLisp standalone runtime, where the macros were void.
;; A thunk is a closure that evaluates its body at most once and caches the
;; result.
;;
;; - `thunk-delay'  : wrap BODY in a thunk.
;; - `thunk-force'  : force a thunk (memoized).
;; - `thunk-let' / `thunk-let*' : bind variables to thunks forced on first
;;   reference (via `cl-symbol-macrolet').
;;
;; Definitions are UNCONDITIONAL (like the bundled `range.el' and the
;; companion `let-alist.el'): on host Emacs `thunk' is autoloaded, so a
;; `(fboundp ...)' gate could be skipped while the autoload resolves back
;; here and fails to define.  A stock host never loads this file.  Hidden
;; thunk temporaries carry an index in their name (Doc 22 A11: same-named
;; `make-symbol' temps collide on this runtime).

;;; Code:

(defmacro thunk-delay (&rest body)
  "Return a thunk that evaluates BODY once, on the first `thunk-force'."
  (let ((forced (make-symbol "--thunk-forced--"))
        (val (make-symbol "--thunk-value--")))
    (list 'let (list (list forced nil) (list val nil))
          (list 'lambda nil
                (list 'unless forced
                      (list 'setq val (cons 'progn body))
                      (list 'setq forced t))
                val))))

(defun thunk-force (delayed)
  "Force the thunk DELAYED, returning its (memoized) value."
  (funcall delayed))

(defmacro thunk-let* (bindings &rest body)
  "Like `let*' but each BINDING value is lazily evaluated on first use."
  (declare (indent 1))
  (let ((i 0)
        (result (cons 'progn body)))
    (dolist (binding (reverse bindings))
      (let* ((var (car binding))
             (expr (cdr binding))
             (thv (make-symbol (format "--thunk-let-%d--" i))))
        (setq i (1+ i))
        (setq result
              (list 'let (list (list thv (cons 'thunk-delay expr)))
                    (list 'cl-symbol-macrolet
                          (list (list var (list 'thunk-force thv)))
                          result)))))
    result))

(defmacro thunk-let (bindings &rest body)
  "Like `let' but each BINDING value is lazily evaluated on first use."
  (declare (indent 1))
  (cons 'thunk-let* (cons (reverse bindings) body)))

(provide 'thunk)

;;; thunk.el ends here
