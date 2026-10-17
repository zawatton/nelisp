;;; nelisp-bt.el --- bordeaux-threads-shaped OS threads for NeLisp -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 214.  A Common-Lisp (bordeaux-threads) style thread API, kept apart
;; from Emacs's own `make-thread' family so Elisp written for Emacs keeps
;; Emacs semantics.
;;
;; Two backends, chosen at load time:
;;
;; - native: the NeLisp standalone runtime on linux-x86_64 and
;;   windows-x86_64.  Threads are real OS threads (Doc 199/214 worker ID 3)
;;   that run in parallel.  Locks, condition variables and semaphores are
;;   futex-style words in shared memory (`nelisp-thread-wait' /
;;   `nelisp-thread-wake').
;; - emacs: everywhere else (host Emacs).  The same API maps onto Emacs's
;;   cooperative threads, so code and tests run unchanged; there is no
;;   speed-up.
;;
;; Semantics follow bordeaux-threads:
;; - A new thread sees global values, not its creator's dynamic bindings,
;;   plus whatever INITIAL-BINDINGS gives it.
;; - `nl-bt-join-thread' returns the thread function's value, or
;;   re-signals the error that ended it.
;; - Locks are not recursive unless made with `nl-bt-make-recursive-lock'.
;; - Condition variables may wake spuriously; callers re-check in a loop.
;;
;; Current limits on the native backend (later Doc 214 stages):
;; - Threads run inside one bounded parallel section: the collector stays
;;   off from the first live thread until every started thread has been
;;   joined (S6).
;; - Only the main thread may create threads and synchronisation objects,
;;   and threads may not assign global variables (S5).

;;; Code:

(declare-function nelisp-thread-shared-alloc "ext:nelisp" (size))
(declare-function nelisp-thread-atomic-read "ext:nelisp" (addr))
(declare-function nelisp-thread-atomic-add "ext:nelisp" (addr delta))
(declare-function nelisp-thread-atomic-cas "ext:nelisp" (addr old new))
(declare-function nelisp-thread-atomic-store "ext:nelisp" (addr value))
(declare-function nelisp-thread-wait "ext:nelisp" (addr expected ms))
(declare-function nelisp-thread-wake "ext:nelisp" (addr count))
(declare-function nelisp-thread-yield "ext:nelisp" ())
(declare-function nelisp-thread-spawn "ext:nelisp" (id top a0 a1 a2))
(declare-function nelisp-thread-gc-inhibit "ext:nelisp" (begin))
(declare-function nelisp-thread-worker-sexp "ext:nelisp" (handle offset))
(declare-function nelisp-thread-mutex-lock "ext:nelisp" (addr ms id recursive))
(declare-function nelisp-thread-mutex-unlock "ext:nelisp" (addr id))
(declare-function nelisp-thread-cond-wait "ext:nelisp" (seq lock ms id))
(declare-function nelisp-thread-cond-notify "ext:nelisp" (seq count))
(declare-function nelisp-thread-sem-wait "ext:nelisp" (addr ms))

(define-error 'nl-bt-error "nl-bt error")
(define-error 'nl-bt-timeout "nl-bt timeout" 'nl-bt-error)

(defvar nelisp--last-signal-data)

;;; Backend selection

(defconst nl-bt-backend
  (if (and (fboundp 'nelisp-thread-spawn)
           (member system-configuration
                   '("x86_64-pc-linux-gnu" "x86_64-pc-windows-msvc"))
           (condition-case nil
               (progn (nelisp-thread-atomic-read
                       (nelisp-thread-shared-alloc 64))
                      t)
             (error nil)))
      'native
    'emacs)
  "The thread backend in use: `native' (real OS threads) or `emacs'.")

(defmacro nl-bt--native-p ()
  "Non-nil on the native backend (inlined: hot paths call it per operation)."
  '(eq nl-bt-backend 'native))
;;; Shared words (native)

(defvar nl-bt--slab 0 "Current shared-memory slab for words.")

(defvar nl-bt--slab-left 0 "Words left in `nl-bt--slab'.")

(defun nl-bt--words (n)
  "Return N fresh zeroed contiguous 8-byte words of shared memory.
Main thread only (it assigns the slab globals)."
  (when (< nl-bt--slab-left n)
    (setq nl-bt--slab (nelisp-thread-shared-alloc 65536)
          nl-bt--slab-left 8192))
  (when (< nl-bt--slab 4096)
    (signal 'nl-bt-error (list "shared memory allocation failed" nl-bt--slab)))
  (prog1 nl-bt--slab
    (setq nl-bt--slab (+ nl-bt--slab (* 8 n))
          nl-bt--slab-left (- nl-bt--slab-left n))))

(defun nl-bt--word ()
  "Return a fresh zeroed 8-byte word of shared memory (main thread only)."
  (nl-bt--words 1))

(defun nl-bt--xchg (word value)
  "Atomically store VALUE in WORD and return its previous value."
  (let ((old (nelisp-thread-atomic-read word)))
    (while (/= 1 (nelisp-thread-atomic-cas word old value))
      (setq old (nelisp-thread-atomic-read word)))
    old))

(defun nl-bt--ms (wait-p timeout)
  "Return the native wait budget in ms for WAIT-P and TIMEOUT seconds.
0 means try once, -1 means no limit."
  (cond ((eq wait-p :no-wait) 0)
        (timeout (max 0 (truncate (* 1000 timeout))))
        (t -1)))

(defun nl-bt--deadline (timeout)
  "Return the `float-time' deadline for TIMEOUT seconds, or nil."
  (and timeout (+ (float-time) timeout)))

;;; Threads

;; Thread object: [nl-bt-thread NAME STATE RESULT-SLOT DONE-WORD VALUE ERROR NATIVE]
;; STATE is `running', `finished' or `failed'.  On the emacs backend NATIVE
;; holds the Emacs thread and RESULT-SLOT a cons (STATUS . VALUE).

(defun nl-bt--thread-p (x)
  (and (vectorp x) (= (length x) 8) (eq (aref x 0) 'nl-bt-thread)))

(defvar nl-bt--self nil
  "The current nl-bt thread object; let-bound in every thread it starts.")

(defvar nl-bt--main-thread (vector 'nl-bt-thread "main thread" 'running
                                   nil nil nil nil nil)
  "The thread object of the thread that loaded nl-bt.")

(defvar nl-bt--threads nil "Started threads not yet joined.")

(defvar nl-bt--section-open nil "Non-nil while the native parallel section is open.")

(defvar nl-bt-default-special-bindings nil
  "Alist of (SYMBOL . FORM) bound in every new thread, like
bordeaux-threads' `*default-special-bindings*'.  FORM is evaluated in
the new thread.")

(defmacro nl-bt--self-id ()
  "The current thread's native owner id: its done word, or 1 for main."
  '(if nl-bt--self (aref nl-bt--self 4) 1))

(defun nl-bt-current-thread ()
  "Return the current thread object."
  (or nl-bt--self nl-bt--main-thread))

(defun nl-bt-thread-name (thread)
  "Return THREAD's name."
  (aref thread 1))

(defun nl-bt-threadp (object)
  "Return non-nil when OBJECT is an nl-bt thread."
  (nl-bt--thread-p object))

(defun nl-bt--thread-form (fn thread bindings)
  "Return the form that runs FN as THREAD with BINDINGS, in that thread."
  `(let ((nelisp--last-signal-data nil)
         (nl-bt--self ',thread)
         ,@(mapcar (lambda (b) (list (car b) (cdr b))) bindings))
     (funcall ',fn)))

(defun nl-bt--section-begin ()
  (unless nl-bt--section-open
    (let ((rc (nelisp-thread-gc-inhibit 1)))
      (unless (= rc 1)
        (signal 'nl-bt-error (list "could not begin a parallel section" rc))))
    (setq nl-bt--section-open t)))

(defun nl-bt--section-maybe-end ()
  (when (and nl-bt--section-open (null nl-bt--threads))
    (setq nl-bt--section-open nil)
    (nelisp-thread-gc-inhibit 0)))

(defun nl-bt-make-thread (function &optional name initial-bindings)
  "Start a thread running FUNCTION with no arguments; return the thread.
NAME is a string.  INITIAL-BINDINGS is an alist (SYMBOL . FORM), added to
`nl-bt-default-special-bindings'; each FORM is evaluated in the new
thread."
  (let* ((name (or name "Anonymous thread"))
         (bindings (append nl-bt-default-special-bindings initial-bindings))
         (thread (vector 'nl-bt-thread name 'running nil nil nil nil nil)))
    (if (nl-bt--native-p)
        (let* ((slot (nl-bt--word))
               (_hi (nl-bt--word))
               (done (nl-bt--word)))
          (aset thread 3 slot)
          (aset thread 4 done)
          (nl-bt--section-begin)
          (let ((tid (nelisp-thread-spawn
                      3 0 (nl-bt--thread-form function thread bindings)
                      slot done)))
            (unless (> tid 0)
              (nl-bt--section-maybe-end)
              (signal 'nl-bt-error (list "thread creation failed" tid))))
          (push thread nl-bt--threads))
      (let ((cell (cons nil nil)))
        (aset thread 3 cell)
        (aset thread 7
              (make-thread
               (lambda ()
                 (condition-case err
                     (progn
                       (setcdr cell (eval (nl-bt--thread-form
                                           function thread bindings)
                                          t))
                       (setcar cell 1))
                   (error (setcdr cell err) (setcar cell 2))))
               name))
        (push thread nl-bt--threads)))
    thread))

(defun nl-bt--native-finished-p (thread)
  (> (nelisp-thread-atomic-read (aref thread 4)) 0))

(defun nl-bt--reap (thread)
  "Collect THREAD's outcome once it has finished; return non-nil if it has."
  (cond
   ((not (eq (aref thread 2) 'running)) t)
   ((nl-bt--native-p)
    (when (nl-bt--native-finished-p thread)
      (let* ((slot (aref thread 3))
             (status (nelisp-thread-atomic-read slot))
             (handle (nelisp-thread-atomic-read (+ slot 8))))
        (if (= status 1)
            (progn (aset thread 5 (nelisp-thread-worker-sexp handle 128))
                   (aset thread 2 'finished))
          (aset thread 6 (cons (nelisp-thread-worker-sexp handle 2072)
                               (nelisp-thread-worker-sexp handle 2104)))
          (aset thread 2 'failed)))
      (setq nl-bt--threads (delq thread nl-bt--threads))
      (nl-bt--section-maybe-end)
      t))
   (t
    (let ((cell (aref thread 3)))
      (when (car cell)
        (if (= (car cell) 1)
            (progn (aset thread 5 (cdr cell)) (aset thread 2 'finished))
          (aset thread 6 (cdr cell))
          (aset thread 2 'failed))
        (setq nl-bt--threads (delq thread nl-bt--threads))
        t)))))

(defun nl-bt-join-thread (thread)
  "Wait for THREAD to finish; return its value or re-signal its error."
  (unless (nl-bt--thread-p thread)
    (signal 'wrong-type-argument (list 'nl-bt-threadp thread)))
  (when (eq thread (nl-bt-current-thread))
    (signal 'nl-bt-error (list "a thread cannot join itself")))
  (if (nl-bt--native-p)
      (let ((done (aref thread 4)))
        (when done
          (while (= 0 (nelisp-thread-atomic-read done))
            (nelisp-thread-wait done 0 -1))))
    (when (aref thread 7) (thread-join (aref thread 7))))
  (nl-bt--reap thread)
  (if (eq (aref thread 2) 'failed)
      (signal (car (aref thread 6)) (cdr (aref thread 6)))
    (aref thread 5)))

(defun nl-bt-thread-alive-p (thread)
  "Return non-nil while THREAD is still running."
  (cond
   ((eq thread nl-bt--main-thread) t)
   ((not (eq (aref thread 2) 'running)) nil)
   ((nl-bt--native-p) (not (nl-bt--native-finished-p thread)))
   (t (and (aref thread 7) (thread-live-p (aref thread 7))))))

(defun nl-bt-all-threads ()
  "Return the main thread and every started thread still alive."
  (cons nl-bt--main-thread
        (let (live)
          (dolist (th nl-bt--threads (nreverse live))
            (when (nl-bt-thread-alive-p th) (push th live))))))

(defun nl-bt-thread-yield ()
  "Let other threads run."
  (if (nl-bt--native-p) (nelisp-thread-yield) (thread-yield)))

;;; Locks

;; Lock object: [nl-bt-lock NAME WORDS OWNER RECURSIVE COUNT EMACS-MUTEX]
;; Native: WORDS is three shared words (futex mutex 0/1/2, owner id, hold
;; depth), all kept by `nelisp-thread-mutex-lock' / `-unlock'; OWNER and
;; COUNT are used by the emacs backend only.

(defun nl-bt--lock-p (x)
  (and (vectorp x) (= (length x) 7) (eq (aref x 0) 'nl-bt-lock)))

(defun nl-bt-make-lock (&optional name)
  "Return a new non-recursive lock named NAME."
  (vector 'nl-bt-lock (or name "Anonymous lock")
          (and (nl-bt--native-p) (nl-bt--words 3))
          nil nil 0
          (and (not (nl-bt--native-p)) (make-mutex name))))

(defun nl-bt-make-recursive-lock (&optional name)
  "Return a new recursive lock named NAME."
  (let ((lock (nl-bt-make-lock (or name "Anonymous recursive lock"))))
    (aset lock 4 t)
    lock))

(defun nl-bt--lock-error (lock code)
  "Signal the error for native lock result CODE on LOCK."
  (signal 'nl-bt-error
          (list (if (= code -1) "lock already held by this thread"
                  "lock not held by this thread")
                (aref lock 1))))

(defun nl-bt-acquire-lock (lock &optional wait-p timeout)
  "Acquire LOCK; return non-nil on success.
By default wait, like bordeaux-threads' `acquire-lock'.  When WAIT-P is
`:no-wait', try once.  TIMEOUT, in seconds, bounds the wait."
  (if (nl-bt--native-p)
      (let ((r (nelisp-thread-mutex-lock (aref lock 2) (nl-bt--ms wait-p timeout)
                                         (nl-bt--self-id)
                                         (if (aref lock 4) 1 0))))
        (if (= r -1) (nl-bt--lock-error lock r) (= r 1)))
    (let ((self (nl-bt-current-thread)))
      (if (eq (aref lock 3) self)
          (if (aref lock 4)
              (progn (aset lock 5 (1+ (aref lock 5))) t)
            (nl-bt--lock-error lock -1))
        (let ((deadline (nl-bt--deadline timeout))
              (m (aref lock 6))
              (got nil))
          (while (null got)
            (cond
             ((null (aref lock 3)) (mutex-lock m) (setq got t))
             ((or (eq wait-p :no-wait)
                  (and deadline (> (float-time) deadline)))
              (setq got 'fail))
             (t (thread-yield))))
          (when (eq got t)
            (aset lock 3 self)
            (aset lock 5 1)
            t))))))

(defun nl-bt-release-lock (lock)
  "Release LOCK, held by the current thread."
  (if (nl-bt--native-p)
      (when (= -1 (nelisp-thread-mutex-unlock (aref lock 2) (nl-bt--self-id)))
        (nl-bt--lock-error lock -2))
    (unless (eq (aref lock 3) (nl-bt-current-thread))
      (nl-bt--lock-error lock -2))
    (aset lock 5 (1- (aref lock 5)))
    (when (= (aref lock 5) 0)
      (aset lock 3 nil)
      (mutex-unlock (aref lock 6))))
  nil)

(defmacro nl-bt-with-lock-held (lock &rest body)
  "Run BODY with LOCK held.
On the native backend this expands straight to the lock primitives: an
interpreted function call costs tens of microseconds, comparable to the
whole uncontended lock round trip."
  (declare (indent 1))
  (let ((l (make-symbol "lock"))
        (id (make-symbol "id"))
        (r (make-symbol "r")))
    (if (nl-bt--native-p)
        `(let* ((,l ,lock)
                (,id (nl-bt--self-id))
                (,r (nelisp-thread-mutex-lock (aref ,l 2) -1 ,id
                                              (if (aref ,l 4) 1 0))))
           (unless (= ,r 1) (nl-bt--lock-error ,l ,r))
           (unwind-protect (progn ,@body)
             (when (= -1 (nelisp-thread-mutex-unlock (aref ,l 2) ,id))
               (nl-bt--lock-error ,l -2))))
      `(let ((,l ,lock))
         (nl-bt-acquire-lock ,l)
         (unwind-protect (progn ,@body)
           (nl-bt-release-lock ,l))))))

(defmacro nl-bt-with-recursive-lock-held (lock &rest body)
  "Run BODY with recursive LOCK held."
  (declare (indent 1))
  `(nl-bt-with-lock-held ,lock ,@body))

;;; Condition variables

;; [nl-bt-cv NAME SEQ-WORD EMACS-SEQ]

(defun nl-bt-make-condition-variable (&optional name)
  "Return a new condition variable named NAME."
  (vector 'nl-bt-cv (or name "Anonymous condition variable")
          (and (nl-bt--native-p) (nl-bt--word)) 0))

(defun nl-bt-condition-wait (cv lock &optional timeout)
  "Release LOCK, wait for a notification on CV, re-acquire LOCK.
Return nil when TIMEOUT seconds passed first, else non-nil.  Wake-ups
may be spurious.  A recursive LOCK is released fully and re-taken at
its previous depth."
  (if (nl-bt--native-p)
      (let ((r (nelisp-thread-cond-wait (aref cv 2) (aref lock 2)
                                        (nl-bt--ms nil timeout)
                                        (nl-bt--self-id))))
        (if (= r -1) (nl-bt--lock-error lock -2) (= r 1)))
    (unless (eq (aref lock 3) (nl-bt-current-thread))
      (nl-bt--lock-error lock -2))
    (let ((count (aref lock 5))
          (seq (aref cv 3))
          (deadline (nl-bt--deadline timeout)))
      (aset lock 5 1)
      (nl-bt-release-lock lock)
      (while (and (= seq (aref cv 3))
                  (not (and deadline (> (float-time) deadline))))
        (thread-yield))
      (nl-bt-acquire-lock lock)
      (aset lock 5 count)
      (/= seq (aref cv 3)))))

(defun nl-bt-condition-notify (cv)
  "Wake at least one thread waiting on CV."
  (if (nl-bt--native-p)
      (nelisp-thread-cond-notify (aref cv 2) 1)
    (aset cv 3 (1+ (aref cv 3))))
  nil)

(defun nl-bt-condition-broadcast (cv)
  "Wake every thread waiting on CV."
  (if (nl-bt--native-p)
      (nelisp-thread-cond-notify (aref cv 2) 0)
    (aset cv 3 (1+ (aref cv 3))))
  nil)
;;; Semaphores

;; [nl-bt-semaphore NAME COUNT-WORD EMACS-COUNT]

(defun nl-bt-make-semaphore (&optional name count)
  "Return a new semaphore named NAME holding COUNT (default 0)."
  (let ((sem (vector 'nl-bt-semaphore (or name "Anonymous semaphore")
                     (and (nl-bt--native-p) (nl-bt--word)) (or count 0))))
    (when (and (nl-bt--native-p) count (> count 0))
      (nelisp-thread-atomic-store (aref sem 2) count))
    sem))

(defun nl-bt-signal-semaphore (sem &optional count)
  "Increment SEM by COUNT (default 1), waking that many waiters."
  (let ((n (or count 1)))
    (if (nl-bt--native-p)
        (progn (nelisp-thread-atomic-add (aref sem 2) n)
               (nelisp-thread-wake (aref sem 2) (if (= n 1) 1 0)))
      (aset sem 3 (+ (aref sem 3) n))))
  nil)

(defun nl-bt-wait-on-semaphore (sem &optional timeout)
  "Decrement SEM, waiting while it is zero.
Return non-nil on success, nil when TIMEOUT seconds pass first."
  (if (nl-bt--native-p)
      (= 1 (nelisp-thread-sem-wait (aref sem 2) (nl-bt--ms nil timeout)))
    (let ((deadline (nl-bt--deadline timeout))
          (got nil) (gave-up nil))
      (while (not (or got gave-up))
        (if (> (aref sem 3) 0)
            (progn (aset sem 3 (1- (aref sem 3))) (setq got t))
          (if (and deadline (> (float-time) deadline))
              (setq gave-up t)
            (thread-yield))))
      got)))

(defun nl-bt-semaphore-count (sem)
  "Return SEM's current count."
  (if (nl-bt--native-p) (nelisp-thread-atomic-read (aref sem 2)) (aref sem 3)))

;;; Atomic integers

;; [nl-bt-atomic WORD EMACS-VALUE]

(defun nl-bt-make-atomic-integer (&optional value)
  "Return a new atomic integer holding VALUE (default 0)."
  (let ((a (vector 'nl-bt-atomic (and (nl-bt--native-p) (nl-bt--word))
                   (or value 0))))
    (when (and (nl-bt--native-p) value)
      (nelisp-thread-atomic-store (aref a 1) value))
    a))

(defun nl-bt-atomic-integer-value (atomic)
  "Return ATOMIC's value."
  (if (nl-bt--native-p) (nelisp-thread-atomic-read (aref atomic 1)) (aref atomic 2)))

(defun nl-bt-atomic-integer-incf (atomic &optional delta)
  "Add DELTA (default 1) to ATOMIC; return the new value."
  (let ((d (or delta 1)))
    (if (nl-bt--native-p)
        (+ d (nelisp-thread-atomic-add (aref atomic 1) d))
      (aset atomic 2 (+ (aref atomic 2) d)))))

(defun nl-bt-atomic-integer-cas (atomic old new)
  "Set ATOMIC to NEW if it holds OLD; return non-nil on success."
  (if (nl-bt--native-p)
      (= 1 (nelisp-thread-atomic-cas (aref atomic 1) old new))
    (when (= (aref atomic 2) old) (aset atomic 2 new) t)))

(provide 'nelisp-bt)

;;; nelisp-bt.el ends here
