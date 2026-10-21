;;; word-heap-frames-parity.el --- Doc 216 S4.2 execution corpus -*- lexical-binding: t; -*-

;; One case per process lets the driver retain GNU crashes and hangs separately.
;; Only the requested activation is selected from backtraces; error data and
;; local-binding lists are printed without normalization.

(defun whf-recur (n)
  (if (= n 0) 0 (+ 1 (whf-recur (1- n)))))

(defun whf-signal-through (n)
  (if (= n 0) (signal 'error '(alpha (beta . 3)))
    (let ((retained (list n)))
      (unwind-protect (whf-signal-through (1- n))
        (unless (= (car retained) n) (error "unwind local changed"))))))

(defun whf-bt-inner (arg)
  (list arg (backtrace-frame 0 'whf-bt-inner)
        (backtrace-frame 1 'whf-bt-inner)
        (car (backtrace-frames 'whf-bt-inner))
        (whf-read-one-local)))
(defun whf-bt-outer (outer) (whf-bt-inner outer))
(defun whf-read-one-local () (backtrace--locals 1 'whf-read-one-local))
(defun whf-read-two-locals ()
  (list (backtrace--locals 1 'whf-read-two-locals)
        (backtrace--locals 2 'whf-read-two-locals)))

(defun whf-locals-inner (inner)
  (garbage-collect)
  (whf-read-two-locals))
(defun whf-locals-outer (outer)
  (let ((local '(retained local))) (whf-locals-inner 23)))

(defun whf-loop-capture ()
  (let ((i 0) (shared nil) (separate nil))
    (while (< i 4)
      (setq shared (cons (lambda () i) shared))
      (let ((iteration i))
        (setq separate (cons (lambda () iteration) separate)))
      (setq i (1+ i)))
    (garbage-collect)
    (list (mapcar #'funcall (reverse shared))
          (mapcar #'funcall (reverse separate)))))

(defun whf-make-pair ()
  (let ((value '(initial)))
    (list (lambda () value) (lambda (new) (setq value new)))))

(defun whf-capture-sharing ()
  (let* ((pair (whf-make-pair)) (read (car pair)) (write (cadr pair)))
    ;; Reusing frame depths must not overwrite the escaped binding.
    (dotimes (i 40) (let ((value (list 'pressure i))) (car value)))
    (garbage-collect)
    (let ((before (funcall read)))
      (funcall write '(updated shared))
      (garbage-collect)
      (list before (funcall read)))))

(defun whf-nonlocal ()
  (let ((events nil) (held nil))
    (setq held
          (catch 'whf-return
            (let ((saved '(captured after throw)))
              (unwind-protect
                  (throw 'whf-return (lambda () saved))
                (setq events (cons 'cleanup events))))))
    (garbage-collect)
    (list (funcall held) events
          (condition-case data (whf-signal-through 4) (error data)))))

(defun whf-recursion ()
  (list (whf-recur 64)
        (let ((max-lisp-eval-depth 100))
          (condition-case data (whf-recur 500) (error data)))
        (whf-recur 8)))

(defun whf-thread-setup ()
  (fset 'whf-thread-entry
        (let ((captured 37))
          (lambda (input)
            (let ((local (+ captured input)))
              (let ((reader (lambda () local))) (funcall reader)))))))

(defun whf-threads ()
  ;; This case exercises the registered, GC-barrier worker path.
  (whf-thread-setup)
  (let ((local '(parent binding)))
    (whf-thread-entry 0)
    (list
     (if (fboundp 'nelisp-thread-spawn)
         (let* ((shared (nelisp-thread-shared-alloc 32))
                (done (+ shared 16)) (form-a '(whf-thread-entry 5))
                (form-b '(whf-thread-entry 7)) (active nil))
           (when (< shared 0) (error "thread shared allocation: %S" shared))
           (unwind-protect
               (progn
                 (unless (= (nelisp-thread-gc-inhibit 1) 1)
                   (error "thread barrier did not begin"))
                 (setq active t)
                 (unless (> (nelisp-thread-spawn 2 0 form-a shared done) 0)
                   (error "thread A spawn failed"))
                 (unless (> (nelisp-thread-spawn 2 0 form-b (+ shared 8) done) 0)
                   (error "thread B spawn failed"))
                 (unless (= (nelisp-thread-join done 2) 2)
                   (error "thread join failed"))
                 (let ((result (list (nelisp-thread-atomic-read shared)
                                     (nelisp-thread-atomic-read (+ shared 8)))))
                   (unless (= (nelisp-thread-gc-inhibit 0) 1)
                     (error "thread barrier grew arena"))
                   (setq active nil)
                   result))
             (when active (nelisp-thread-gc-inhibit 0))))
       (let ((a (make-thread (lambda () (whf-thread-entry 5)) "whf-a"))
             (b (make-thread (lambda () (whf-thread-entry 7)) "whf-b")))
         (list (thread-join a) (thread-join b))))
     local)))

(defun whf-legacy-worker (input result done)
  (let ((local (whf-thread-entry input)))
    (ptr-write-u64 result 0 (funcall (lambda () local)))
    (nelisp-thread-atomic-add done 1)))

(defun whf-legacy-threads ()
  ;; The older public reader primitive takes a form, rather than registry ID2.
  ;; Keep its supported lexical capture/lifetime case under the existing GC
  ;; barrier. A parent activation remains live throughout the child eval.
  (whf-thread-setup)
  (let ((local '(parent binding)))
    (list
     (if (fboundp 'thread-spawn)
         (let* ((shared (nelisp-thread-shared-alloc 32)) (done (+ shared 16))
                (active nil))
           (when (< shared 0) (error "legacy thread allocation: %S" shared))
           (unwind-protect
               (progn
                 (unless (= (nelisp-thread-gc-inhibit 1) 1) (error "legacy barrier failed"))
                 (setq active t)
                 (unless (= (thread-spawn (list 'whf-legacy-worker 5 shared done)) 1)
                   (error "legacy thread A failed"))
                 (unless (= (nelisp-thread-join done 1) 1) (error "legacy join A failed"))
                 (unless (= (thread-spawn (list 'whf-legacy-worker 7 (+ shared 8) done)) 1)
                   (error "legacy thread B failed"))
                 (unless (= (nelisp-thread-join done 2) 2) (error "legacy join B failed"))
                 (let ((result (list (nelisp-thread-atomic-read shared)
                                     (nelisp-thread-atomic-read (+ shared 8)))))
                   (unless (= (nelisp-thread-gc-inhibit 0) 1) (error "legacy barrier grew arena"))
                   (setq active nil)
                   result))
             (when active (nelisp-thread-gc-inhibit 0))))
       (list (thread-join (make-thread (lambda () (whf-thread-entry 5)) "whf-legacy-a"))
             (thread-join (make-thread (lambda () (whf-thread-entry 7)) "whf-legacy-b"))))
     local)))

(defun whf-public-threads ()
  ;; Exercise the exact provider selected by the production foundation bundle.
  ;; Its fallback currently completes the callback before returning; its live
  ;; predicate describes that contract. GNU executes the same closures on its
  ;; public threads. The separate registry case above also tests concurrency.
  (let ((directory (getenv "WH_FRAME_THREAD_PROVIDER")))
    (unless directory (error "Missing production thread provider directory"))
    (dolist (file '("emacs-cc-thread-1.el" "emacs-cc-thread-predicates-1.el"
                    "emacs-cc-thread-2.el"))
      (load (expand-file-name file directory) nil t)))
  (let* ((captured 37) (result (vector nil nil)) (local '(parent binding))
         (a (make-thread
             (lambda () (let ((value (+ captured 5)))
                          (aset result 0 (funcall (lambda () value))))) "whf-public-a"))
         (b (make-thread
             (lambda () (let ((value (+ captured 7)))
                          (aset result 1 (funcall (lambda () value))))) "whf-public-b")))
    (while (or (thread-live-p a) (thread-live-p b)) (thread-yield))
    (garbage-collect)
    (list (threadp a) (threadp b) (thread-name a) (thread-name b)
          (list (aref result 0) (aref result 1)) local)))

(defun whf-witness-stats ()
  (nelisp--env-globals-op 'frame-stack-stats))

(defun whf-witness-work ()
  ;; Interpreter evaluation exercises the actual lexical-frame push path.
  ;; This body creates no escaping closure.
  (let ((local '(live))) (car local)))

(defun whf-witness ()
  (let ((before nil) (i 0) (after nil) (captured nil))
    (setq before (whf-witness-stats))
    (while (< i 200)
      (whf-witness-work)
      (setq i (1+ i)))
    (setq after (whf-witness-stats))
    (setq captured (whf-make-pair))
    (garbage-collect)
    (unless (equal (funcall (car captured)) '(initial))
      (error "witness captured cell was lost"))
    (list before after (whf-witness-stats))))

(let* ((case (getenv "WH_FRAME_CASE"))
       (answer
        (cond ((equal case "loop-capture") (whf-loop-capture))
              ((equal case "capture-sharing") (whf-capture-sharing))
              ((equal case "backtrace-frame") (whf-bt-outer 17))
              ((equal case "backtrace-locals") (whf-locals-outer 11))
              ((equal case "nonlocal") (whf-nonlocal))
              ((equal case "recursion") (whf-recursion))
              ((equal case "threads") (whf-threads))
              ((equal case "public-threads") (whf-public-threads))
              ((equal case "legacy-threads") (whf-legacy-threads))
              ((equal case "witness") (whf-witness))
              (t (error "Unknown frame case: %S" case)))))
  (princ (format "WH-FRAME %s %S\n" case answer))
  (princ "WH-FRAME-DONE 1\n"))

(kill-emacs 0)
