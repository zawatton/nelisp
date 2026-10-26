;;; nelisp-buffer-leaf-test.el --- native buffer read path (bufnative) -*- lexical-binding: t; -*-

;; `nl_buf_leaf_run' (lisp/nelisp-cc-fx-call-primitives.el) answers point (40), point-min
;; (41), point-max (42), goto-char (43), buffer-size (45), bobp (46) and eobp (47) from the
;; current buffer's `nelisp-buffer' record.  Slot K of the record is the word at payload
;; + 16 + 8K; the Lisp references are the prelude's nelisp--NAME-reference defuns.  The
;; host model below is a memory table: the record at 40006 has the 16 slots of the struct.

(require 'ert)
(require 'cl-lib)
(require 'nelisp-standalone-build)
(require 'nelisp-cc-fx-call-primitives)
(require 'nelisp-directcall-test)

(defconst nbl--rec 40006)
(defconst nbl--mem 4000)

(defun nbl--fix (n) (+ (ash n 2) 1))
(defconst nbl--slots
  '((narrow-start . 6) (narrow-end . 7) (tick . 10) (pend . 11) (blen . 12) (len . 13) (ins . 15)))

(defun nbl--model (&rest plist)
  "A memory with a 16-slot buffer record; PLIST sets slots by name to fixnum values or `nil'."
  (let ((m (make-hash-table :test #'eql)))
    (puthash 40000 (+ 12 (ash 16 8)) m) (puthash 40008 1111 m)
    (dotimes (i 16) (puthash (+ 40016 (* 8 i)) 3 m))
    (puthash (+ 40016 (* 8 12)) (nbl--fix 4) m) ; blen 4: gap at position 5
    (puthash (+ 40016 (* 8 13)) (nbl--fix 9) m) ; len 9
    (while plist
      (let ((slot (cdr (assq (pop plist) nbl--slots))) (v (pop plist)))
        (puthash (+ 40016 (* 8 slot)) (if v (nbl--fix v) 3) m)))
    m))

(defvar nbl--dirty 0)
(defvar nbl--current 'ok)
(defvar nbl--refetch 0)
(defvar nbl--alloc 0)
(defconst nbl--symobj 6006)                 ; the symbol object word, header at 6000, value cell at 6016
(defconst nbl--epoch-addr 7000)
(defconst nbl--registry-addr 7100)
(defconst nbl--pin-addr 7200)
(defconst nbl--cache-addr 7300)             ; BSS block nl_buf_cache: object word, epoch word

(defun nbl--arm (m cur)
  "Install the symbol object, its value cell CUR (a word) and the control words in M."
  (puthash 6000 4 m)                          ; kind 4, redirect 0
  (puthash 6016 cur m)
  (puthash nbl--epoch-addr 11 m)
  (puthash nbl--registry-addr 0 m)
  (puthash (+ nbl--pin-addr 48) 0 m)
  (puthash nbl--cache-addr 0 m) (puthash (+ nbl--cache-addr 8) 0 m))

(defmacro nbl--with (m &rest body)
  (declare (indent 1))
  `(cl-letf (((symbol-function 'ptr-read-u64) (lambda (p i) (gethash (+ p i) ,m 0)))
             ((symbol-function 'heap-read-u64) (lambda (p i) (gethash (+ p i) ,m 0)))
             ((symbol-function 'ptr-write-u64) (lambda (p i v) (puthash (+ p i) v ,m)))
             ((symbol-function 'nl_record_length) (lambda (w) (ash (gethash (- w 6) ,m 0) -8)))
             ((symbol-function 'wf_dirty) (lambda () (cl-incf nbl--dirty)))
             ((symbol-function 'alloc-bytes) (lambda (n _a) (cl-incf nbl--alloc 64) (+ 9000 nbl--alloc)))
             ((symbol-function 'nl_alloc_symbol) (lambda (&rest _) 0))
             ((symbol-function 'nl_symview_object)
              (lambda (_) (cl-incf nbl--refetch) nbl--symobj))
             ((symbol-function 'nl_buf_symobj)
              (lambda (_) (cl-incf nbl--refetch) nbl--symobj)))
     (setq nbl--dirty 0 nbl--refetch 0 nbl--current 'ok)
     (nbl--arm ,m nbl--rec)
     ,@body))

(defun nbl--fn (name &optional mutate)
  (let* ((f (ndc--find name (cdr nelisp-cc-fx-call-primitives--source)))
         (form (cons 'lambda (cddr f))))
    (when mutate (setq form (funcall mutate form)))
    ;; (extern-call F ARG...) is a plain call and the data-addr symbols are fixed addresses
    (setq form (nbl--unextern form))
    (eval (ndc--host form) t)))

(defun nbl--unextern (form)
  (cond ((atom form) form)
        ((eq (car form) 'extern-call) (mapcar #'nbl--unextern (cdr form)))
        ((equal form '(data-addr nl_mxcache_epoch)) nbl--epoch-addr)
        ((equal form '(data-addr nl_thread_registry)) nbl--registry-addr)
        ((equal form '(data-addr nl_root_pin_control)) nbl--pin-addr)
        ((equal form '(data-addr nl_buf_cache)) nbl--cache-addr)
        (t (cons (nbl--unextern (car form)) (nbl--unextern (cdr form))))))

(defun nbl--run (id m &optional args)
  "Run the leaf body of ID with ARGS (fixnum words) on the model M: (RC RESULT)."
  (let ((run (nbl--fn 'nl_buf_leaf_run)) (cur (nbl--fn 'nl_buf_current))
        (refetch (nbl--fn 'nl_buf_refetch)) (more (nbl--fn 'nl_buf_leaf_more)) (k 0))
    (dolist (a args) (puthash (+ nbl--mem (* 8 k)) a m) (cl-incf k))
    (puthash (+ nbl--mem 800) 12345 m)
    (cl-letf (((symbol-function 'nl_buf_current) cur)
              ((symbol-function 'nl_buf_refetch) refetch)
              ((symbol-function 'nl_buf_leaf_more) more))
      (let ((rc (funcall run id nbl--mem (length args) (length args) (+ nbl--mem 800) 0)))
        (list rc (gethash (+ nbl--mem 800) m))))))

(ert-deftest nbl-read-ids ()
  ;; pend nil: point is blen+1 = 5; no narrowing: min 1, max len+1 = 10, size 9
  (let ((m (nbl--model)))
    (nbl--with m
      (should (equal (nbl--run 40 m) (list 0 (nbl--fix 5))))
      (should (equal (nbl--run 41 m) (list 0 (nbl--fix 1))))
      (should (equal (nbl--run 42 m) (list 0 (nbl--fix 10))))
      (should (equal (nbl--run 45 m) (list 0 (nbl--fix 9))))
      (should (equal (nbl--run 46 m) (list 0 3)))
      (should (equal (nbl--run 47 m) (list 0 3)))))
  ;; a pending point, a narrowing: point 2, accessible 2..7
  (let ((m (nbl--model 'pend 2 'narrow-start 2 'narrow-end 7)))
    (nbl--with m
      (should (equal (nbl--run 40 m) (list 0 (nbl--fix 2))))
      (should (equal (nbl--run 41 m) (list 0 (nbl--fix 2))))
      (should (equal (nbl--run 42 m) (list 0 (nbl--fix 7))))
      (should (equal (nbl--run 45 m) (list 0 (nbl--fix 9))))
      (should (equal (nbl--run 46 m) (list 0 7)))
      (should (equal (nbl--run 47 m) (list 0 3)))))
  (let ((m (nbl--model 'pend 7 'narrow-start 2 'narrow-end 7)))
    (nbl--with m
      (should (equal (nbl--run 47 m) (list 0 7)))
      (should (equal (nbl--run 46 m) (list 0 3))))))

(ert-deftest nbl-goto-char ()
  ;; clamps into the accessible portion, answers the argument, pend nil at the gap position
  (dolist (c '((3 3 3) (1 1 1) (0 1 0) (-5 1 -5) (10 10 10) (99 10 99) (5 nil 5)))
    (let ((m (nbl--model)))
      (nbl--with m
        (let ((r (nbl--run 43 m (list (nbl--fix (car c))))))
          (should (equal r (list 0 (nbl--fix (nth 2 c)))))
          (should (= (gethash (+ 40016 (* 8 11)) m) (if (nth 1 c) (nbl--fix (nth 1 c)) 3)))
          (should (= nbl--dirty 1))))))
  ;; narrowing 2..7
  (dolist (c '((1 2) (2 2) (6 6) (7 7) (8 7)))
    (let ((m (nbl--model 'narrow-start 2 'narrow-end 7)))
      (nbl--with m
        (nbl--run 43 m (list (nbl--fix (car c))))
        (should (= (gethash (+ 40016 (* 8 11)) m) (nbl--fix (cadr c)))))))
  ;; a non-fixnum argument (marker, nil, float word) and a wrong argc decline, nothing written
  (dolist (arg '(3 40006 7 2))
    (let ((m (nbl--model 'pend 2)))
      (nbl--with m
        (should (equal (nbl--run 43 m (list arg)) (list -3 12345)))
        (should (= (gethash (+ 40016 (* 8 11)) m) (nbl--fix 2)))
        (should (= nbl--dirty 0)))))
  (let ((m (nbl--model 'pend 2)))
    (nbl--with m
      (should (equal (nbl--run 43 m nil) (list -3 12345))))))

(ert-deftest nbl-narrow-and-widen ()
  ;; (narrow-to-region START END): order, store, point moved in, pend nil at the gap position
  (dolist (c '((3 7 3 7 5 nil) (7 3 3 7 5 nil) (2 4 2 4 4 4) (8 9 8 9 8 8) (1 10 1 10 5 nil) (6 6 6 6 6 6)
               (5 5 5 5 5 nil)))
    (let ((m (nbl--model)))
      (nbl--with m
        (should (equal (nbl--run 48 m (list (nbl--fix (nth 0 c)) (nbl--fix (nth 1 c)))) (list 0 3)))
        (should (= (gethash (+ 40016 (* 8 6)) m) (nbl--fix (nth 2 c))))
        (should (= (gethash (+ 40016 (* 8 7)) m) (nbl--fix (nth 3 c))))
        (should (= (gethash (+ 40016 (* 8 11)) m) (if (nth 5 c) (nbl--fix (nth 5 c)) 3)))
        (should (= nbl--dirty 1)))))
  ;; a pending point inside stays, one outside is clamped
  (let ((m (nbl--model 'pend 2)))
    (nbl--with m
      (nbl--run 48 m (list (nbl--fix 4) (nbl--fix 6)))
      (should (= (gethash (+ 40016 (* 8 11)) m) (nbl--fix 4)))))
  (let ((m (nbl--model 'pend 9)))
    (nbl--with m
      (nbl--run 48 m (list (nbl--fix 4) (nbl--fix 6)))
      (should (= (gethash (+ 40016 (* 8 11)) m) (nbl--fix 6)))))
  (let ((m (nbl--model 'pend 3)))
    (nbl--with m
      (nbl--run 48 m (list (nbl--fix 3) (nbl--fix 6)))
      (should (= (gethash (+ 40016 (* 8 11)) m) (nbl--fix 3)))))
  ;; out of range, zero, negative, non-fixnum: decline with nothing written
  (dolist (args (list (list (nbl--fix 0) (nbl--fix 3)) (list (nbl--fix 3) (nbl--fix 11))
                      (list (nbl--fix -1) (nbl--fix 3)) (list 3 (nbl--fix 3)) (list (nbl--fix 3) 40006)))
    (let ((m (nbl--model 'pend 2)))
      (nbl--with m
        (should (equal (nbl--run 48 m args) (list -3 12345)))
        (should (= nbl--dirty 0))
        (should (= (gethash (+ 40016 (* 8 6)) m) 3))
        (should (= (gethash (+ 40016 (* 8 11)) m) (nbl--fix 2))))))
  (let ((m (nbl--model)))
    (nbl--with m (should (equal (nbl--run 48 m (list (nbl--fix 3))) (list -3 12345)))))
  ;; widen clears both bounds and keeps point
  (let ((m (nbl--model 'pend 3 'narrow-start 2 'narrow-end 7)))
    (nbl--with m
      (should (equal (nbl--run 49 m) (list 0 3)))
      (should (= (gethash (+ 40016 (* 8 6)) m) 3))
      (should (= (gethash (+ 40016 (* 8 7)) m) 3))
      (should (= (gethash (+ 40016 (* 8 11)) m) (nbl--fix 3)))
      (should (equal (nbl--run 49 m (list (nbl--fix 1))) (list -3 12345))))))

(ert-deftest nbl-decline-cases ()
  ;; buffer-size with an argument is the Lisp's (a buffer argument)
  (let ((m (nbl--model)))
    (nbl--with m
      (should (equal (nbl--run 45 m (list 40006)) (list -3 12345)))))
  ;; the current buffer is not a buffer record: unbound cell (an immediate), nil, a word that is no
  ;; record, a value in a worker (env+168), a thread registry, a root pin
  (dolist (cur '(2 3 99 4097 40014))
    (let ((m (nbl--model)))
      (nbl--with m
        (puthash 6016 cur m)
        (dolist (id '(40 41 42 45 46 47))
          (should (equal (nbl--run id m) (list -3 12345)))))))
  (dolist (poke '((168 . 1) (7100 . 1) (7248 . 2)))
    (let ((m (nbl--model)))
      (nbl--with m
        (puthash (car poke) (cdr poke) m)
        (dolist (id '(40 41 42 45 46 47))
          (should (equal (nbl--run id m) (list -3 12345)))))))
  (let ((m (nbl--model)))
    (puthash 40000 (+ 12 (ash 15 8)) m)
    (nbl--with m (should (equal (nbl--run 40 m) (list -3 12345)))))
  ;; a non-fixnum BLEN / LEN slot declines
  (let ((m (nbl--model)))
    (puthash (+ 40016 (* 8 12)) 3 m)
    (nbl--with m (should (equal (nbl--run 40 m) (list -3 12345)))))
  (let ((m (nbl--model)))
    (nbl--with m (should (equal (nbl--run 44 m) (list -3 12345))))))

;; The symbol object is fetched once and again only after the compaction epoch changed.
(ert-deftest nbl-symbol-cache-follows-the-compaction-epoch ()
  (let ((m (nbl--model)))
    (nbl--with m
      (nbl--run 40 m) (nbl--run 40 m) (nbl--run 41 m)
      (should (= nbl--refetch 1))
      (should (= (gethash nbl--cache-addr m) nbl--symobj))
      (puthash nbl--epoch-addr 12 m)            ; nl_gc_compact ran
      (nbl--run 40 m)
      (should (= nbl--refetch 2))
      (nbl--run 40 m)
      (should (= nbl--refetch 2)))))

;; The crash: words of the low control page hold other data (16 at +0x448) and must not be read.
(ert-deftest nbl-ignores-the-control-page ()
  (let ((m (nbl--model)))
    (nbl--with m
      (puthash 268436552 16 m) (puthash 268436560 1 m) (puthash 268436544 7 m)
      (should (equal (nbl--run 40 m) (list 0 (nbl--fix 5)))))))

;; The value cell is read on every call: rebinding the variable is seen at once.
(ert-deftest nbl-reads-the-current-value-cell-every-call ()
  (let ((m (nbl--model 'pend 3)) (m2 (nbl--model 'pend 8)))
    (nbl--with m
      (should (equal (nbl--run 40 m) (list 0 (nbl--fix 3))))
      ;; a second buffer record at 80006, bound to the variable
      (dotimes (i 18) (puthash (+ 80000 (* 8 i)) (gethash (+ 40000 (* 8 i)) m2 0) m))
      (puthash 6016 80006 m)
      (should (equal (nbl--run 40 m) (list 0 (nbl--fix 8))))
      (puthash 6016 nbl--rec m)
      (should (equal (nbl--run 40 m) (list 0 (nbl--fix 3)))))))

;; Negative control: reading the wrong slot (off by one, the Lisp `aref' index instead of the
;; data slot) must fail the read test.
(ert-deftest nbl-negative-control-slot-offset ()
  (let ((m (nbl--model 'pend 2)))
    (nbl--with m
      (let ((bad (nbl--fn 'nl_buf_leaf_run
                          (lambda (form) (cl-subst 96 104 form :test #'equal)))))
        (cl-letf (((symbol-function 'nl_buf_current) (nbl--fn 'nl_buf_current))
                  ((symbol-function 'nl_buf_refetch) (nbl--fn 'nl_buf_refetch)))
          (puthash (+ nbl--mem 800) 12345 m)
          (funcall bad 40 nbl--mem 0 0 (+ nbl--mem 800) 0)
          (should-not (equal (gethash (+ nbl--mem 800) m) (nbl--fix 2))))))))

(ert-deftest nbl-registered ()
  (dolist (spec '(("point" 40) ("point-min" 41) ("point-max" 42) ("goto-char" 43)
                  ("buffer-size" 45) ("bobp" 46) ("eobp" 47) ("narrow-to-region" 48) ("widen" 49)))
    (should (equal (assoc (car spec) nelisp-standalone--directcall-specs) spec))
    (should (assoc (car spec) (nelisp-standalone--builtin-arities)))
    (should (member (car spec) nelisp-standalone--reader-builtins))))

(provide 'nelisp-buffer-leaf-test)
