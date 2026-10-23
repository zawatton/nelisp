;;; nelisp-n4c-reference.el --- Activation record GNU references, second set -*- lexical-binding: t; -*-
;; Run unchanged on GNU 31.1 and on the standalone, after test/nelisp-n4b-reference.el (its
;; helpers `n4-row', `n4r-mark', `n4r-norm' and `n4r-above' are used).  These rows pin what a
;; change of the activation record's layout could break and the first set does not touch: the
;; printed backtrace with arguments, frames across collections in argument evaluation and in
;; mapbacktrace callbacks, deep recursion, frames after a non-local exit, the frames seen from
;; inside backtrace-eval's swapped environment, locals and flags across interpreted and
;; byte-code activations.

;; The printed backtrace's lines that name one of this file's functions (`n4c-'): GNU and the
;; standalone differ in the frames of macros and of the load machinery, never in these.
(defun n4c-bt-lines ()
  (let ((buf (generate-new-buffer " n4c-bt" t)) lines)
    (unwind-protect
        (progn
          (let ((standard-output buf)) (backtrace))
          (with-current-buffer buf
            (dolist (l (split-string (buffer-string) "\n" t))
              (let ((l (replace-regexp-in-string "\\`[ *]+" "" l)))
                (when (string-match-p "\\`(?n4c-" l) (push l lines))))))
      (kill-buffer buf))
    (nreverse lines)))
(defun n4c-leaf (x y) (n4c-bt-lines))
(defun n4c-mid (a) (n4c-leaf a (+ a 1)))
(defun n4c-top (b) (list (n4c-mid b)))
(n4-row "text-args-interpreted" (n4c-top 5))
(defun n4c-bc (x) (n4c-leaf x 2))
(defalias 'n4c-bc #[257 "\300\1\301\"\207" [n4c-leaf 2] 4 "\n\n(fn X)"])
(defun n4c-top-bc (b) (list (n4c-bc b)))
(n4-row "text-args-bytecode" (n4c-top-bc 5))
(n4-row "text-unevaluated-arguments"
        (let (seen) (setq seen (n4c-leaf (setq seen (n4c-bt-lines)) 3)) seen))
(defun n4c-down (n) (if (= n 0) (n4c-bt-lines) (n4c-down (1- n))))
(n4-row "text-recursion-3" (n4c-down 3))
(defun n4c-count (n) (if (= n 0) (length (n4c-bt-lines)) (n4c-count (1- n))))
(n4-row "recursion-count-difference" (- (n4c-count 300) (n4c-count 100)))
(n4-row "recursion-count-linear" (list (- (n4c-count 20) (n4c-count 10)) (- (n4c-count 40) (n4c-count 20))))

;; The frames stay intact across collections: in the evaluation of a later argument, and in the
;; callbacks of mapbacktrace.
(defun n4c-gc-leaf (a b) (garbage-collect) (list a b (n4c-bt-lines)))
(n4-row "gc-in-later-argument"
        (n4c-gc-leaf (list (make-string 3 ?x) (list 1 2)) (progn (garbage-collect) (n4c-bt-lines))))
(defun n4c-gc-nested (n) (if (= n 0) (progn (garbage-collect) (n4c-bt-lines))
                           (n4c-gc-nested (progn (garbage-collect) (1- n)))))
(n4-row "gc-in-recursive-argument" (n4c-gc-nested 4))
(defun n4c-map-gc ()
  (let (out)
    (mapbacktrace
     (lambda (ev f args _flags)
       (garbage-collect)
       (make-string 40 ?z)
       (when (and (symbolp f) (string-prefix-p "n4c-" (symbol-name f)))
         (push (list ev f args) out)))
     'n4c-map-gc)
    (nreverse out)))
(defun n4c-map-outer (x) (n4c-map-inner (list x (make-string 2 ?q))))
(defun n4c-map-inner (l) (n4c-map-gc))
(n4-row "mapbacktrace-gc-callback" (n4c-map-outer 7))
(defun n4c-map-frames-inside ()
  (let (out)
    (mapbacktrace (lambda (_e f _a _flags)
                    (when (eq f 'n4c-map-frames-inside) (setq out (n4c-bt-lines))))
                  'n4c-map-frames-inside)
    out))
(n4-row "frames-inside-mapbacktrace-callback" (n4c-map-frames-inside))
(n4-row "frames-inside-frame-internal-callback"
        (backtrace-frame--internal (lambda (_e _f _a _fl) (n4c-bt-lines)) 0 'backtrace-frame--internal))

;; Frames after a non-local exit: what is left is the frames of the handler's activation.
(defun n4c-signal-deep (n) (if (= n 0) (error "n4c") (n4c-signal-deep (1- n))))
(n4-row "frames-after-condition-case"
        (list (condition-case nil (n4c-signal-deep 6) (error (n4c-bt-lines)))
              (n4c-bt-lines)))
(defun n4c-throw-deep (n) (if (= n 0) (throw 'n4c-tag 7) (n4c-throw-deep (1- n))))
(n4-row "frames-after-catch"
        (list (catch 'n4c-tag (n4c-throw-deep 6)) (n4c-bt-lines)))
(defun n4c-unwind-frames ()
  (let (seen)
    (list (condition-case nil
              (unwind-protect (n4c-signal-deep 3) (setq seen (n4c-bt-lines)))
            (error 'caught))
          seen)))
(n4-row "frames-in-unwind-form" (n4c-unwind-frames))

;; Frames and locals seen from inside backtrace-eval's swapped environment (it evaluates at the
;; frame's entry: the parameters are visible, a variable bound later in the body is not).
(defvar n4c-dynv 1)
(defun n4c-eval-outer (x) (let ((y (* x 2))) (n4c-eval-mid y)))
(defun n4c-eval-mid (z) (backtrace-eval '(list x) 1 'n4c-eval-mid))
(n4-row "eval-in-outer-frame" (n4c-eval-outer 5))
(defun n4c-eval-set (x) (backtrace-eval '(setq x (+ x 40)) 0 'n4c-eval-set) (list x (n4c-bt-lines)))
(n4-row "eval-sets-parameter" (n4c-eval-set 9))
(defun n4c-eval-late (x) (let ((y 4)) (condition-case e (backtrace-eval 'y 0 'n4c-eval-late) (error (car e)))))
(n4-row "eval-late-binding-invisible" (n4c-eval-late 1))
(defun n4c-locals-inner (a) (backtrace--locals 1 'n4c-locals-inner))
(defun n4c-locals-outer (x) (let ((n4c-dynv 3)) (n4c-locals-inner (list x))))
(n4-row "locals-across-frames" (n4c-locals-outer 4))
(defun n4c-locals-deep (a) (backtrace--locals 2 'n4c-locals-deep))
(defun n4c-locals-mid (a) (let ((n4c-dynv 6)) (n4c-locals-deep a)))
(defun n4c-locals-top (a) (let ((n4c-dynv 5)) (list (n4c-locals-mid a))))
(n4-row "locals-two-frames-up" (n4c-locals-top 4))
(defun n4c-locals-bc (x) (let ((n4c-dynv 8)) (n4c-locals-deep x)))
(defalias 'n4c-locals-bc #[257 "\301\30\302\1!)\207" [n4c-dynv 8 n4c-locals-deep] 3 "\n\n(fn X)"])
(defun n4c-locals-bc-outer (x) (let ((n4c-dynv 7)) (list (n4c-locals-bc x))))
(n4-row "locals-through-bytecode" (n4c-locals-bc-outer 8))

;; Flags: debug-on-exit set on a frame deep in a recursion and on a bytecode activation.
(defvar n4c-exits nil)
(defun n4c-exit-debugger (reason value) (push (list reason value (length (n4c-bt-lines))) n4c-exits) value)
(defun n4c-flag-deep (n) (if (= n 0) (progn (backtrace-debug 0 t 'n4c-flag-deep) 1)
                           (+ 1 (n4c-flag-deep (1- n)))))
(n4-row "debug-on-exit-in-recursion"
        (let ((debugger 'n4c-exit-debugger)) (setq n4c-exits nil)
             (list (n4c-flag-deep 3) (reverse n4c-exits))))
(defun n4c-flag-bc (x) (backtrace-debug 0 t 'n4c-flag-bc) x)
(defalias 'n4c-flag-bc #[257 "\300\301\302\303#\210\207" [backtrace-debug 0 t n4c-flag-bc] 5 "\n\n(fn X)"])
(n4-row "debug-on-exit-bytecode"
        (let ((debugger 'n4c-exit-debugger)) (setq n4c-exits nil)
             (list (n4c-flag-bc 11) (reverse n4c-exits))))

;; The frames of a thread's activation list keep the same shape as the printed ones.
(defun n4c-thread-frames () (backtrace--frames-from-thread (current-thread)))
(n4-row "frames-from-thread-shape"
        (let ((fr (n4c-thread-frames)))
          (list (consp fr) (let (names) (dolist (x fr) (when (and (symbolp (cadr x)) (string-prefix-p "n4c-" (symbol-name (cadr x)))) (push (cadr x) names))) (nreverse names)))))

(princ "N4C-REFERENCE-DONE\n")
nil
