;;; nl-prof.el --- instrumenting profiler for code in the emacs-lib runtime image  -*- lexical-binding: t -*-

;; One run -> per function: calls, inclusive and exclusive time, ns/call, and caller->callee edges.
;; Lisp-level only (no rebuild).  Each selected function's symbol-function cell is replaced by a
;; timing wrapper; everything is restored afterwards (also on error/throw out of the profiled form).
;;
;; GNU's elp.el cannot be reused: it `require's and instruments in the image, but `elp-results'
;; fails with (error "Invalid error symbol" unsupported-feature), and elp only has inclusive time.
;;
;; IN-LOAD TRAP (state-perf F95): code run while a `load' is active is ~5x slower (interpreter path).
;; Run `nl-prof-main'/`nl-prof-run' from a top-level form AFTER the loads returned (nl-prof.sh does).
;;
;; Limits: callers that captured the old function object (hook lists holding a closure, #'f stored in
;; a variable before wrapping) and primitives called by direct linked symbol bypass the wrapper.
;; Subrs reached through the symbol cell (funcall 'car) are counted; inlined primitive calls are not.

(defvar nl-prof--top nil "Innermost active frame: [childsum desc parent stat].")
(defvar nl-prof--saved nil "Alist NAME -> original function cell while wrapped.")
(defvar nl-prof--stats nil "Alist NAME -> stat vector.")
;; stat: 0 calls, 1 incl(raw, outermost only), 2 excl(raw), 3 outermost calls, 4 desc-calls of outermost,
;;       5 name, 6 depth, 7 edges (hash PARENT-NAME -> [count el-raw desc])

(defconst nl-prof--never-wrap
  '(apply funcall eval vector aset aref gethash puthash float-time + - * = < > <= >= 1+ 1- car cdr cons
    fset symbol-function make-hash-table not null eq equal unwind-protect catch throw signal
    format princ message error identity)
  "Functions the wrapper or the runner depends on; wrapping them would recurse.")

(defvar nl-prof--log (make-vector 2 0) "Event log: pairs (ID . TIME) for enter, (-1 . TIME) for exit.")
(defvar nl-prof--n 0 "Next free slot in `nl-prof--log'.")
(defvar nl-prof--cap 0 "Usable slots in `nl-prof--log'.")
(defvar nl-prof--pending 0 "Recorded enters whose exit is not yet logged (their exit slots are reserved).")
(defvar nl-prof--dropped 0 "Calls not recorded because the log was full.")

(defmacro nl-prof--now ()
  "Integer nanoseconds.  `nelisp--monotonic-ns' (Doc 235, clock_gettime) returns a fixnum, so the hot
path allocates nothing and the resolution is 1 ns; else fall back to float-time (1 us)."
  (if (fboundp 'nelisp--monotonic-ns) '(nelisp--monotonic-ns) '(floor (* 1e9 (float-time)))))

(defun nl-prof--call (id orig args)
  "Call ORIG with ARGS, logging an enter and an exit timestamp for wrapper ID.  The hot path does no
float arithmetic, no allocation and no hashing (those cost several us each in the image); all accounting is offline
in `nl-prof--process'.  The exit record runs in an unwind-protect cleanup, so throw/signal are safe."
  (let ((i nl-prof--n) (rec nil))
    (if (< (+ i (* 2 nl-prof--pending) 4) nl-prof--cap)
        (progn (setq rec t)
               (setq nl-prof--pending (1+ nl-prof--pending))
               (aset nl-prof--log i id)
               (aset nl-prof--log (1+ i) (nl-prof--now))
               (setq nl-prof--n (+ i 2)))
      (setq nl-prof--dropped (1+ nl-prof--dropped)))
    (unwind-protect (apply orig args)
      (when rec
        (let ((j nl-prof--n))
          (aset nl-prof--log j -1)
          (aset nl-prof--log (1+ j) (nl-prof--now))
          (setq nl-prof--n (+ j 2))
          (setq nl-prof--pending (1- nl-prof--pending)))))))

(defun nl-prof--make-wrapper (orig id)
  "Return a timing function that calls ORIG as wrapper ID.  ID and ORIG are embedded as constants
(a closure over them would have a non-(t) env and run in the slow interpreter, F95)."
  (eval (list 'lambda '(&rest args) (list 'nl-prof--call id (list 'quote orig) 'args)) t))

(defun nl-prof--reset-log (cap)
  (setq nl-prof--log (make-vector cap 0) nl-prof--cap cap nl-prof--n 0
        nl-prof--pending 0 nl-prof--dropped 0))

(defun nl-prof--process (names)
  "Turn the event log into stat vectors (one per name, see below) for `nl-prof--report'.
NAMES: vector ID -> symbol.  Returns an alist NAME -> stat; stat = [calls incl excl outermost
desc-of-outermost name depth edges] with edges: hash PARENT-NAME -> [count el desc]."
  (let ((stats (make-vector (length names) nil)) (stack nil) (k 0) (n nl-prof--n) (log nl-prof--log) (out nil))
    (while (< k n)
      (let ((id (aref log k)) (tm (aref log (1+ k))))
        (if (>= id 0)
            (let ((st (aref stats id)))
              (unless st (setq st (nl-prof--new-stat (aref names id))) (aset stats id st))
              (aset st 6 (1+ (aref st 6)))
              (push (vector 0.0 0 tm st) stack))
          (let* ((fr (car stack)) (st (aref fr 3)) (el (* 1e-9 (- tm (aref fr 2))))
                 (d (aref st 6)) (desc (aref fr 1)) (parent (car (cdr stack)))
                 (pn (if parent (aref (aref parent 3) 5) 'nl-prof--root))
                 (e (gethash pn (aref st 7))))
            (setq stack (cdr stack))
            (aset st 6 (1- d))
            (aset st 0 (1+ (aref st 0)))
            (aset st 2 (+ (aref st 2) (- el (aref fr 0))))
            (when (= d 1)
              (aset st 1 (+ (aref st 1) el))
              (aset st 3 (1+ (aref st 3)))
              (aset st 4 (+ (aref st 4) desc)))
            (unless e (setq e (vector 0 0.0 0)) (puthash pn e (aref st 7)))
            (aset e 0 (1+ (aref e 0)))
            (aset e 1 (+ (aref e 1) el))
            (aset e 2 (+ (aref e 2) desc))
            (when parent
              (aset parent 0 (+ (aref parent 0) el))
              (aset parent 1 (+ (aref parent 1) 1 desc))))))
      (setq k (+ k 2)))
    (setq k 0)
    (while (< k (length names))
      (when (aref stats k) (push (cons (aref names k) (aref stats k)) out))
      (setq k (1+ k)))
    out))

(defun nl-prof--new-stat (name)
  (vector 0 0.0 0.0 0 0 name 0 (make-hash-table :test 'eq)))

(defun nl-prof--wrap (name id)
  "Wrap NAME as wrapper ID.  Return non-nil when wrapped."
  (when (and (symbolp name) (fboundp name) (not (assq name nl-prof--saved))
             (not (memq name nl-prof--never-wrap))
             (not (string-prefix-p "nl-prof--" (symbol-name name)))
             (not (macrop name)) (not (special-form-p name))
             (not (autoloadp (symbol-function name))))
    (let ((orig (symbol-function name)))
      (push (cons name orig) nl-prof--saved)
      (fset name (nl-prof--make-wrapper orig id))
      t)))

(defun nl-prof--unwrap-all ()
  (dolist (c nl-prof--saved) (fset (car c) (cdr c)))
  (setq nl-prof--saved nil nl-prof--top nil))

(defun nl-prof--select (prefixes functions)
  (let ((out nil))
    (when prefixes
      (dolist (s (nelisp--intern-table-symbols))
        (when (and (fboundp s)
                   (let ((n (symbol-name s)) (hit nil))
                     (dolist (p prefixes) (when (string-prefix-p p n) (setq hit t)))
                     hit))
          (push s out))))
    (dolist (f functions) (unless (memq f out) (push f out)))
    out))

;;; Calibration -------------------------------------------------------------------------------------

(defun nl-prof--calibrate (&optional n)
  "Return (C-IN . C-OUT) seconds per wrapped call: the part of one wrapper's cost that lands inside
its own enter..exit window (inflates its inclusive time) and the part outside it (inflates its
caller's exclusive time).  Measured with an empty wrapped function in a tight loop, median of 3."
  (setq n (or n 20000))
  (defalias 'nl-prof--cal-leaf (lambda () nil))
  (defalias 'nl-prof--cal-loop
    (lambda (k) (let ((i 0)) (while (< i k) (nl-prof--cal-leaf) (setq i (1+ i))))))
  (nl-prof--cal-loop 3000)
  (let ((ins nil) (outs nil) (round 0) (orig (symbol-function 'nl-prof--cal-leaf))
        (names (vector 'nl-prof--cal-leaf)))
    (while (< round 3)
      (let* ((t0 (float-time))
             (_ (nl-prof--cal-loop n))
             (tu (- (float-time) t0)))
        (nl-prof--reset-log (+ 8 (* 4 n)))
        (fset 'nl-prof--cal-leaf (nl-prof--make-wrapper orig 0))
        (nl-prof--cal-loop 3000)
        (nl-prof--reset-log (+ 8 (* 4 n)))
        (setq t0 (float-time))
        (nl-prof--cal-loop n)
        (let ((tw (- (float-time) t0)))
          (fset 'nl-prof--cal-leaf orig)
          (let* ((st (cdr (car (nl-prof--process names))))
                 (cin (/ (aref st 1) (aref st 0)))
                 (cout (- (/ (- tw tu) n) cin)))
            (push cin ins) (push cout outs))))
      (setq round (1+ round)))
    (cons (nth 1 (sort ins #'<)) (max 0.0 (nth 1 (sort outs #'<))))))

;;; Report ------------------------------------------------------------------------------------------

(defun nl-prof--ns (s) (* s 1e9))

(defun nl-prof--report (wall wall0 runs cal top)
  "WALL: instrumented wall time, WALL0: the same runs unwrapped.  CAL (c-in . c-out) is scaled so the
predicted total overhead equals WALL - WALL0 (the empty-leaf calibration underestimates the cost of
wrappers nested in real code: cache and GC effects); the scale is printed."
  (let* ((ncalls (let ((n 0)) (dolist (c nl-prof--stats) (setq n (+ n (aref (cdr c) 0)))) n))
         (pred (* ncalls (+ (car cal) (cdr cal))))
         ;; trust the measured overhead only when it is a big share of the run (else it is timer noise)
         (scale (if (and (> pred (* 0.3 wall0)) (> wall wall0))
                    (min 3.0 (max 0.5 (/ (- wall wall0) pred))) 1.0))
         (cin (* scale (car cal))) (cout (* scale (cdr cal)))
         (nchild (make-hash-table :test 'eq))
         (rows nil) (edge-rows nil) (root-incl 0.0) (uncalled 0))
    (dolist (c nl-prof--stats)
      (let ((st (cdr c)))
        (maphash (lambda (pn e) (puthash pn (+ (gethash pn nchild 0) (aref e 0)) nchild)) (aref st 7))))
    (dolist (c nl-prof--stats)
      (let* ((st (cdr c)) (name (car c)) (n (aref st 0)))
        (if (= n 0)
            (setq uncalled (1+ uncalled))
          (let* ((excl (- (aref st 2) (* n cin) (* (gethash name nchild 0) cout)))
                 (incl (- (aref st 1) (* (aref st 3) cin) (* (aref st 4) (+ cin cout)))))
            (push (list name n (max 0.0 incl) (max 0.0 excl)) rows))
          (maphash (lambda (pn e)
                     (let ((en (aref e 0)))
                       (let ((ei (max 0.0 (- (aref e 1) (* en cin) (* (aref e 2) (+ cin cout))))))
                         (when (eq pn 'nl-prof--root) (setq root-incl (+ root-incl ei)))
                         (push (list pn name en ei) edge-rows))))
                   (aref st 7)))))
    (let ((root-excl (max 0.0 (- wall0 root-incl))))
      (setq rows (sort rows (lambda (a b) (> (nth 3 a) (nth 3 b)))))
      (princ (format "nl-prof: %d runs, unprofiled %.1f us/run, instrumented %.1f us/run, %d wrapped calls\nnl-prof: wrapper overhead subtracted: %.0f ns inside window + %.0f ns outside per call (empty-leaf calibration %.0f+%.0f ns, scale %.2f%s)\n"
                     runs (/ (* 1e6 wall0) runs) (/ (* 1e6 wall) runs) ncalls (nl-prof--ns cin) (nl-prof--ns cout)
                     (nl-prof--ns (car cal)) (nl-prof--ns (cdr cal)) scale
                     (if (or (<= scale 0.5) (>= scale 2.5)) " - CAUTION: calibration and measured overhead disagree, numbers uncertain" (if (= scale 1.0) ", overhead <30% of run: calibration only" ""))))
      (princ (format "%-46s %9s %14s %14s %10s %6s\n" "function" "calls" "incl-ns" "excl-ns" "excl/call" "excl%"))
      (princ (format "%-46s %9d %14s %14.0f %10s %5.1f%%\n" "<top-level form (not wrapped)>" runs "-"
                     (nl-prof--ns root-excl) "-" (/ (* 100.0 root-excl) wall0)))
      (let ((i 0))
        (dolist (r rows)
          (when (< i top)
            (princ (format "%-46s %9d %14.0f %14.0f %10.0f %5.1f%%\n" (symbol-name (nth 0 r)) (nth 1 r)
                           (nl-prof--ns (nth 2 r)) (nl-prof--ns (nth 3 r))
                           (/ (nl-prof--ns (nth 3 r)) (nth 1 r)) (/ (* 100.0 (nth 3 r)) wall0))))
          (setq i (1+ i)))
        (when (> i top) (princ (format "... %d more functions with calls\n" (- i top)))))
      (when (> uncalled 0) (princ (format "(%d wrapped functions were never called)\n" uncalled)))
      (setq edge-rows (sort edge-rows (lambda (a b) (> (nth 3 a) (nth 3 b)))))
      (princ (format "top edges (caller -> callee, calls, inclusive ns of the callee under that caller; recursive edges overlap):\n"))
      (let ((i 0))
        (dolist (r edge-rows)
          (when (< i 15)
            (princ (format "  %-70s %9d %14.0f\n"
                           (concat (symbol-name (nth 0 r)) " -> " (symbol-name (nth 1 r)))
                           (nth 2 r) (nl-prof--ns (nth 3 r)))))
          (setq i (1+ i)))))))

;;; Driver ------------------------------------------------------------------------------------------

(defun nl-prof--read-forms (file)
  (let ((s (with-temp-buffer (insert-file-contents file) (buffer-string))) (pos 0) (forms nil))
    (while (condition-case nil
               (let ((r (read-from-string s pos))) (push (car r) forms) (setq pos (cdr r)) t)
             (end-of-file nil)))
    (nreverse forms)))

(defun nl-prof--eval-forms (forms)
  (dolist (f forms) (eval f t)))

(defun nl-prof-run (forms runs &rest opts)
  "Profile FORMS (a list of forms) RUNS times.  OPTS: :prefixes LIST :functions LIST :warmup N :top K
:setup FORMS.  Call at top level after loads returned (see header)."
  (let* ((prefixes (plist-get opts :prefixes)) (functions (plist-get opts :functions))
         (warm (or (plist-get opts :warmup) 2)) (top (or (plist-get opts :top) 30))
         (names nil) (nlist nil) (wall 0.0) (err nil) (cal nil) (t00 0.0) (wall0 0.0) (i 0))
    (when (plist-get opts :setup) (nl-prof--eval-forms (plist-get opts :setup)))
    (setq names (nl-prof--select prefixes functions))
    (while (< i warm) (nl-prof--eval-forms forms) (setq i (1+ i)))
    (nl-prof--unwrap-all)
    (setq nl-prof--stats nil)
    (setq cal (nl-prof--calibrate))
    (dolist (f functions)
      (when (memq f nl-prof--never-wrap)
        (princ (format "nl-prof: refusing to wrap %s (the profiler depends on it)\n" f))))
    (let ((k 0) (t0 (float-time)))
      (while (< k runs) (nl-prof--eval-forms forms) (setq k (1+ k)))
      (setq wall0 (- (float-time) t0)))
    (let ((n 0) (ids nil))
      (dolist (s names) (when (nl-prof--wrap s n) (push s ids) (setq n (1+ n))))
      (setq nlist (vconcat (nreverse ids)))
      (princ (format "nl-prof: wrapped %d functions\n" n)))
    (nl-prof--reset-log (or (plist-get opts :log-slots) 4000000))
    (condition-case e
        (unwind-protect
            (let ((k 0))
              (setq t00 (float-time))
              (while (< k runs) (nl-prof--eval-forms forms) (setq k (1+ k))))
          (setq wall (- (float-time) t00))
          (nl-prof--unwrap-all))
      (error (setq err e)))
    (when (> nl-prof--dropped 0)
      (princ (format "nl-prof: WARNING log full, %d calls not recorded (raise :log-slots / lower runs)\n" nl-prof--dropped)))
    (setq nl-prof--stats (nl-prof--process nlist))
    (when err (setq wall0 wall) (princ (format "nl-prof: form signalled %S (partial run, times include wrapper overhead beyond the calibrated part)\n" err)))
    (when (> wall 0.0) (nl-prof--report wall wall0 runs cal top))
    (setq nl-prof--stats nil nl-prof--log (make-vector 2 0) nl-prof--n 0)
    nil))

(defun nl-prof-main (form-file opts)
  "Entry for nl-prof.sh.  OPTS plist: :runs :prefixes :functions :warmup :top :setup-file."
  (let ((forms (nl-prof--read-forms form-file))
        (setup (and (plist-get opts :setup-file) (nl-prof--read-forms (plist-get opts :setup-file)))))
    (apply #'nl-prof-run forms (or (plist-get opts :runs) 100)
           :setup setup opts)))

(provide 'nl-prof)
;;; nl-prof.el ends here
