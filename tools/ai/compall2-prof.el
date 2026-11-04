;;; compall2-prof.el --- dynamic call profile of the prelude functions a workload runs  -*- lexical-binding: t; -*-
;; usage: PROF_DRV=DRIVER.el PROF_OUT=FILE BENCH_N=N BINARY --load tools/ai/compall2-prof.el
;;        PROF_SELFTEST=1 BINARY --load tools/ai/compall2-prof.el   (red/green self-test)
;;
;; Every function of the image that is an interpreted closure or byte code when this
;; file loads (the prelude's, before the workload defines its own) is replaced by a
;; counting wrapper (byte code, tools/ai/compall2-prof-wrap.el) that calls the
;; original through `apply'.  While profiling is on,
;; the wrapper adds one call and the function's self time: the microseconds of its
;; activation (`nl-unix-time-usec' around the call, an integer clock, no allocation)
;; minus the inclusive time of the wrapped calls made under it.  The wrapper's own
;; work outside the timed interval lands in the caller's self time; tools/ai/compall2-rank.py
;; subtracts a per-child overhead measured by the self-test.
;;
;; Then DRIVER is loaded (a resurvey row: warm-up, timed loop of BENCH_N steps,
;; `kill-emacs').  `kill-emacs' is replaced to dump, before exiting, one line per
;; function called at least once:
;;   P NAME KIND CALLS SELF-USEC KIDS   (KIDS: counted calls made directly under it)
;; KIND (of the original definition, read at dump time):
;;   native    fx record lowered to native code (NLOCALS <= -16)
;;   fx        fx record run by the word VM
;;   interp    closure with no fx record (declined or never registered)
;;   bytecode  byte code
;; and `PROF-WALL USEC', the wall time from the first wrapper installation to the dump.
;; Running the row at N and N/4 and differencing (compall2-profrun.py) cancels the
;; load and warm-up calls.
;;
;; Left out (calls are not counted): the fx compiler (nelisp--fx-*), the native
;; install pass (nelisp-nc-*) and the byte-code JIT's dispatcher (lisp/nelisp-bytecode-jit.el):
;; wrapping the machinery that compiles and runs the wrappers would recurse.

(defvar compall2-prof--names
  (let ((names nil))
    (mapatoms (lambda (s)
                (when (fboundp s)
                  (let ((f (symbol-function s)))
                    (when (or (eq (car-safe f) 'closure) (byte-code-function-p f))
                      (push s names))))))
    names)
  "The image's closures and byte-code functions before this file defines anything.")

(defvar compall2-prof--on nil "Non-nil while calls are counted.")
(defvar compall2-prof--child 0 "Inclusive usec of counted calls under the current activation.")
(defvar compall2-prof--calls nil "Vector: calls per wrapped function.")
(defvar compall2-prof--self nil "Vector: self usec per wrapped function.")
(defvar compall2-prof--kids 0 "Counted calls made directly under the current activation.")
(defvar compall2-prof--nkids nil "Vector: counted calls made directly under each wrapped function.")
(defvar compall2-prof--orig nil "Vector: original definitions.")
(defvar compall2-prof--wrapped nil "Vector: wrapped names, by index.")
(defvar compall2-prof--t0 0 "Clock at installation.")

(defun compall2-prof--excluded-p (name)
  (let ((s (symbol-name name)))
    (or (string-prefix-p "nelisp--fx-" s)
        (string-prefix-p "nelisp-nc-" s)
        (string-prefix-p "compall2-prof" s)
        (memq name compall2-prof--jit-names))))

(defvar compall2-prof--jit-names
  (let ((names nil) (file (expand-file-name "lisp/nelisp-bytecode-jit.el"
                                            (or (getenv "PROF_REPO") default-directory))))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward "^(defun \\([^ \n()]+\\)" nil t)
          (push (intern (match-string 1)) names))))
    names)
  "Defuns of the byte-code JIT's dispatcher.")

;; The wrapper factory `compall2-prof--wrapper' (I ORIG): byte code compiled by GNU
;; Emacs from tools/ai/compall2-prof-wrap.el (tools/ai/compall2-prof-mkbc.el).
(load (expand-file-name "compall2-prof-bc.el" (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defun compall2-prof--install (names)
  "Wrap NAMES (closures or byte code); answer the number wrapped."
  (let* ((todo (cl-remove-if #'compall2-prof--excluded-p names))
         (n (length todo)) (i 0))
    (setq compall2-prof--calls (make-vector n 0)
          compall2-prof--self (make-vector n 0)
          compall2-prof--nkids (make-vector n 0)
          compall2-prof--orig (make-vector n nil)
          compall2-prof--wrapped (make-vector n nil))
    ;; Run the wrapper body once before any wrapper is installed, so its own
    ;; compilation never calls a wrapper.
    (funcall (compall2-prof--wrapper 0 #'ignore))
    (dolist (name todo)
      (aset compall2-prof--orig i (symbol-function name))
      (aset compall2-prof--wrapped i name)
      (setq i (1+ i)))
    (setq i 0)
    (while (< i n)
      (fset (aref compall2-prof--wrapped i) (compall2-prof--wrapper i (aref compall2-prof--orig i)))
      (setq i (1+ i)))
    n))

(defun compall2-prof--kind (fn)
  (cond ((byte-code-function-p fn) "bytecode")
        ((eq (car-safe fn) 'closure)
         (let ((rec (and (boundp 'nelisp--fx-table) (gethash (nthcdr 3 fn) nelisp--fx-table))))
           (cond ((not (vectorp rec)) "interp")
                 ((and (integerp (aref rec 1)) (<= (aref rec 1) -16)) "native")
                 (t "fx"))))
        (t "other")))

(defun compall2-prof--dump (out)
  (setq compall2-prof--on nil)
  (let ((wall (- (nl-unix-time-usec) compall2-prof--t0)) (lines nil) (i 0)
        (n (length compall2-prof--wrapped)))
    (while (< i n)
      (when (> (aref compall2-prof--calls i) 0)
        (push (format "P %s %s %d %d %d\n" (aref compall2-prof--wrapped i)
                      (compall2-prof--kind (aref compall2-prof--orig i))
                      (aref compall2-prof--calls i) (aref compall2-prof--self i)
                      (aref compall2-prof--nkids i))
              lines))
      (setq i (1+ i)))
    (push (format "PROF-WALL %d\n" wall) lines)
    (if out
        (with-temp-file out (apply #'insert (nreverse lines)))
      (princ (apply #'concat (nreverse lines))))))

(setq max-lisp-eval-depth (max max-lisp-eval-depth 20000))

(cond
 ((getenv "PROF_SELFTEST")
  ;; Red/green: a parent calling a leaf exactly 1000 times, both wrapped.  Counts
  ;; must be exact; the leaf's self time and the parent's self time per child call
  ;; (the per-child overhead the ranking subtracts) are printed.
  (defun compall2-prof-st-leaf (x) (+ x 1))
  (defun compall2-prof-st-parent (k) (let ((s 0)) (dotimes (_ k) (setq s (compall2-prof-st-leaf s))) s))
  (defun compall2-prof-st-bare (k) (let ((s 0)) (dotimes (_ k) (setq s (1+ s))) s))
  (let ((compall2-prof--jit-names nil))
    (cl-letf (((symbol-function 'compall2-prof--excluded-p) (lambda (_n) nil)))
      (compall2-prof--install '(compall2-prof-st-leaf compall2-prof-st-parent compall2-prof-st-bare))))
  (setq compall2-prof--on t compall2-prof--t0 (nl-unix-time-usec))
  (compall2-prof-st-parent 1000)
  (compall2-prof-st-bare 1000)
  (dotimes (_ 20) (compall2-prof-st-parent 1000) (compall2-prof-st-bare 1000))
  (setq compall2-prof--on nil)
  (let ((leaf (aref compall2-prof--calls 0)) (parent (aref compall2-prof--calls 1))
        (bare (aref compall2-prof--calls 2)))
    (princ (format "PROF-SELFTEST calls leaf=%d parent=%d bare=%d self-usec leaf=%d parent=%d bare=%d\n"
                   leaf parent bare (aref compall2-prof--self 0) (aref compall2-prof--self 1)
                   (aref compall2-prof--self 2)))
    ;; Per child call: (parent self - bare self) / child calls, in ns.
    (princ (format "PROF-OVERHEAD-NS %d\n"
                   (/ (* 1000 (- (aref compall2-prof--self 1) (aref compall2-prof--self 2))) (max 1 leaf))))
    (princ (format "PROF-KIDS parent=%d leaf=%d\n" (aref compall2-prof--nkids 1) (aref compall2-prof--nkids 0)))
    (princ (format "PROF-LEAF-NS %d\n" (/ (* 1000 (aref compall2-prof--self 0)) (max 1 leaf))))
    (princ (if (and (= leaf 21000) (= parent 21) (= bare 21)
                      (= (aref compall2-prof--nkids 1) 21000) (= (aref compall2-prof--nkids 0) 0)) "PROF-SELFTEST ok\n" "PROF-SELFTEST bad\n")))
  (kill-emacs 0))
 (t
  (let ((drv (getenv "PROF_DRV")) (out (getenv "PROF_OUT")) (real-kill (symbol-function 'kill-emacs)))
    (unless drv (error "compall2-prof: PROF_DRV unset"))
    (compall2-prof--install compall2-prof--names)
    (fset 'kill-emacs (lambda (&optional arg &rest _)
                        (compall2-prof--dump out)
                        (funcall real-kill (or arg 0))))
    (setq compall2-prof--t0 (nl-unix-time-usec) compall2-prof--on t)
    (load drv nil t)
    (kill-emacs 0))))
