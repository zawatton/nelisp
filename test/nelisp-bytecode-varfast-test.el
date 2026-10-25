;;; nelisp-bytecode-varfast-test.el --- Direct varref/varset in the fast loop -*- lexical-binding: t; -*-

;; Lane w1var.  Runs the production `wf_bytecode_w_fast_run' IR on a model
;; memory with a nonzero ENV and a fake symbol object.  The symbol object is
;; its own value cell: header at +0 (kind bits 0-5, redirect bits 8-9,
;; trapped_write bits 10-11), value at +16; its WORD is the address + 6.

(require 'nelisp-bytecode-fastloop-test)

(defconst w1v--data 1024)
(defconst w1v--mem 4096)
(defconst w1v--cdata 2048)
(defconst w1v--cell 8192)
(defconst w1v--env 40000)
(defconst w1v--reg 50000)
(defconst w1v--pin 50100)
(defconst w1v--sym 60000)
(defconst w1v--word (+ 60000 6))
(defconst w1v--unbound 70006)

(defun w1v--run (code stack &optional opts)
  "Run the fast loop on CODE (a vector) with STACK words.
OPTS: :hdr :value :env168 :env112 :reg :pin48 :consts :keywords :env.
Return (:state :sp :pc :mem :words :dirty :entry)."
  (let* ((words (make-hash-table :test #'eql)) (dirty 0) (entry 0) (writes nil)
         (depth 16) (sp (length stack))
         (hdr (or (plist-get opts :hdr) 4))
         (value (if (plist-member opts :value) (plist-get opts :value) 21))
         (consts (or (plist-get opts :consts) (list w1v--word)))
         (env (if (plist-member opts :env) (plist-get opts :env) w1v--env))
         (keywords (plist-get opts :keywords)))
    (cl-loop for i from -1 to (+ depth 8) do (puthash (+ w1v--mem (* i 8)) 3 words))
    (cl-loop for w in stack for i from 0 do (puthash (+ w1v--mem (* i 8)) w words))
    (cl-loop for w in consts for i from 0 do (puthash (+ w1v--cdata (* i 8)) w words))
    (puthash w1v--cell 1 words)
    (puthash (+ w1v--cell 8) (1+ (* w1v--mem 4)) words)
    (puthash (+ w1v--cell 16) 3 words)
    (puthash w1v--sym hdr words)
    (puthash (+ w1v--sym 16) value words)
    (puthash (+ w1v--env 168) (or (plist-get opts :env168) 0) words)
    (puthash (+ w1v--env 112) (or (plist-get opts :env112) 0) words)
    (puthash w1v--reg (or (plist-get opts :reg) 0) words)
    (puthash (+ w1v--pin 48) (or (plist-get opts :pin48) 0) words)
    (cl-labels
        ((rd (p o) (let ((a (+ p o)))
                     (if (not (gethash a words)) (error "Invalid read %s" a)
                       (gethash a words))))
         (wr (p o w) (let ((a (+ p o)))
                       (unless (gethash a words) (error "Invalid write %s" a))
                       (push (cons a w) writes) (puthash a w words) 1))
         (byte (p o) (if (= p w1v--data)
                         (progn (unless (and (<= 0 o) (< o (length code)))
                                  (error "Invalid byte read %s" o))
                                (aref code o))
                       (rd p o)))
         (u16 (p o) (+ (byte p o) (* 256 (byte p (1+ o)))))
         (u32 (p o) (+ (u16 p o) (* 65536 (u16 p (+ o 2))))))
      (cl-letf (((symbol-function 'heap-read-u8) #'byte)
                ((symbol-function 'heap-read-u16) #'u16)
                ((symbol-function 'heap-read-u32) #'u32)
                ((symbol-function 'ptr-read-u16) #'u16)
                ((symbol-function 'ptr-read-u32) #'u32)
                ((symbol-function 'heap-read-u64) #'rd)
                ((symbol-function 'ptr-read-u64) #'rd)
                ((symbol-function 'ptr-write-u64) #'wr)
                ((symbol-function 'data-addr)
                 (cons 'macro (lambda (name)
                                (if (eq name 'nl_thread_registry) w1v--reg w1v--pin))))
                ((symbol-function 'wf_dirty) (lambda () (cl-incf dirty) 1))
                ((symbol-function 'wf_bytecode_w_unbound_p)
                 (lambda (_e w) (if (= w w1v--unbound) 1 0)))
                ((symbol-function 'bf_keyword_raw)
                 (lambda (w) (if (memq w keywords) 1 0)))
                ((symbol-function 'wf_bytecode_w_fast_entry)
                 (lambda (&rest _) (cl-incf entry) 0)))
        (let* ((fn (eval (cons 'lambda (cddr (sst--host-form
                                              (sst--function 'wf_bytecode_w_fast_run))))
                         t))
               (state (funcall fn w1v--data (length code) w1v--mem depth w1v--cdata
                               (length consts) 0 sp env w1v--cell)))
          (list :state state :pc (ash state -17) :sp (logand state 131071)
                :words words :dirty dirty :entry entry
                :writes (cl-remove-if (lambda (x) (<= w1v--cell (car x) (+ w1v--cell 31))) writes)
                :top (gethash (+ w1v--mem (* (1- (logand state 131071)) 8)) words)
                :value (gethash (+ w1v--sym 16) words)))))))

;; ---- varref ---------------------------------------------------------------

(ert-deftest w1v-varref-plain-pushes-value-without-entry-lookup ()
  (let ((r (w1v--run [8] '() )))
    (should (= (plist-get r :pc) 1))
    (should (= (plist-get r :sp) 1))
    (should (= (plist-get r :top) 21))
    (should (= (plist-get r :entry) 0))
    (should (equal (plist-get r :writes) (list (cons w1v--mem 21)))))
  ;; Four references in a row: the old cache never hit across a varset.
  (let ((r (w1v--run [8 8 8 8] '())))
    (should (= (plist-get r :sp) 4))
    (should (= (plist-get r :entry) 0))))

(ert-deftest w1v-varref-forwarded-is-read-in-line ()
  (let ((r (w1v--run [8] '() '(:hdr #x304))))
    (should (= (plist-get r :sp) 1))
    (should (= (plist-get r :top) 21))))

(ert-deftest w1v-varref-wide-operands ()
  (let ((r (w1v--run [14 1] '() (list :consts (list 3 w1v--word)))))
    (should (= (plist-get r :pc) 2)) (should (= (plist-get r :top) 21)))
  (let ((r (w1v--run [15 1 0] '() (list :consts (list 3 w1v--word)))))
    (should (= (plist-get r :pc) 3)) (should (= (plist-get r :top) 21))))

(ert-deftest w1v-varref-fused-discard-add1-goto ()
  ;; varref; discard; add1; goto 6 -- the value is never pushed.
  (let ((r (w1v--run [8 136 84 130 6 0 138] '(5))))
    (should (= (plist-get r :pc) 6))
    (should (= (plist-get r :sp) 1))
    (should (= (plist-get r :top) 9))))

(ert-deftest w1v-varref-declines-untouched ()
  (dolist (case `(("alias" (:hdr #x104))
                  ("localized" (:hdr #x204))
                  ("uninterned" (:hdr 16))
                  ("record" (:hdr 12))
                  ("void cell" (:value 0))
                  ("unbound marker" (:value ,w1v--unbound))
                  ("worker" (:env168 1))
                  ("thread registry" (:reg 1))
                  ("root pin capture" (:pin48 2))
                  ("env 0" (:env 0))))
    (let ((r (w1v--run [8] '(5) (cadr case))))
      (should (= (plist-get r :pc) 0))
      (should (= (plist-get r :sp) 1))
      (should (null (plist-get r :writes))))))

(ert-deftest w1v-varref-declines-non-symbol-constants ()
  (dolist (c '(3 7 5 4098 0 10))
    (let ((r (w1v--run [8] '() (list :consts (list c)))))
      (should (= (plist-get r :pc) 0)) (should (= (plist-get r :sp) 0)))))

;; ---- varset ---------------------------------------------------------------

(ert-deftest w1v-varset-fixnum-stores-without-barrier ()
  (let ((r (w1v--run [16] '(9))))
    (should (= (plist-get r :pc) 1))
    (should (= (plist-get r :sp) 0))
    (should (= (plist-get r :value) 9))
    (should (= (plist-get r :dirty) 0))
    (should (equal (plist-get r :writes) (list (cons (+ w1v--sym 16) 9))))))

(ert-deftest w1v-varset-boxed-value-runs-the-barrier ()
  (dolist (w '(4098 24582 12))                    ; direct cons, vectorlike, string word
    (let ((r (w1v--run [16] (list (if (= w 12) 20004 w)))))
      (should (= (plist-get r :sp) 0))
      (should (= (plist-get r :dirty) 1))
      (should (= (plist-get r :value) (if (= w 12) 20004 w))))))

(ert-deftest w1v-varset-nil-and-t-are-immediates ()
  (dolist (w '(3 7))
    (let ((r (w1v--run [16] (list w))))
      (should (= (plist-get r :value) w)) (should (= (plist-get r :dirty) 0)))))

(ert-deftest w1v-varset-then-varref-runs-in-one-batch ()
  (let ((r (w1v--run [16 8 16 8] '(9 13))))
    ;; sp starts at 2: top 13 is stored, then ref pushes 13, set pops it, ref pushes 13.
    (should (= (plist-get r :pc) 4))
    (should (= (plist-get r :sp) 2))
    (should (= (plist-get r :value) 13))
    (should (= (plist-get r :entry) 0))))

(ert-deftest w1v-varset-wide-operands ()
  (let ((r (w1v--run [22 1] '(9) (list :consts (list 3 w1v--word)))))
    (should (= (plist-get r :pc) 2)) (should (= (plist-get r :value) 9)))
  (let ((r (w1v--run [23 1 0] '(9) (list :consts (list 3 w1v--word)))))
    (should (= (plist-get r :pc) 3)) (should (= (plist-get r :value) 9))))

(ert-deftest w1v-varset-declines-untouched ()
  (dolist (case `(("alias" (:hdr #x104))
                  ("localized" (:hdr #x204))
                  ("forwarded" (:hdr #x304))
                  ("trapped nowrite" (:hdr #x404))
                  ("trapped watched" (:hdr #x804))
                  ("uninterned" (:hdr 16))
                  ("watcher mode" (:env112 1048576))
                  ("worker" (:env168 1))
                  ("thread registry" (:reg 1))
                  ("root pin capture" (:pin48 2))
                  ("keyword" (:keywords (,w1v--word)))
                  ("env 0" (:env 0))))
    (let ((r (w1v--run [16] '(9) (cadr case))))
      (should (= (plist-get r :pc) 0))
      (should (= (plist-get r :sp) 1))
      (should (= (plist-get r :value) 21))
      (should (null (plist-get r :writes)))
      (should (= (plist-get r :dirty) 0)))))

(ert-deftest w1v-varset-declines-legacy-view-value-and-bad-stack ()
  ;; A value word with low bits 000 is a legacy view: the clone keystone owns it.
  (let ((r (w1v--run [16] '(20000))))
    (should (= (plist-get r :pc) 0)) (should (= (plist-get r :sp) 1))
    (should (null (plist-get r :writes))) (should (= (plist-get r :dirty) 0)))
  ;; Empty stack and a constant index past the vector.
  (let ((r (w1v--run [16] '())))
    (should (= (plist-get r :pc) 0)) (should (null (plist-get r :writes))))
  (let ((r (w1v--run [17] '(9))))
    (should (= (plist-get r :pc) 0)) (should (null (plist-get r :writes))))
  (dolist (c '(3 7 5 4098 10))
    (let ((r (w1v--run [16] '(9) (list :consts (list c)))))
      (should (= (plist-get r :pc) 0)) (should (null (plist-get r :writes))))))

;; ---- structure ------------------------------------------------------------

(ert-deftest w1v-starter-mask-admits-varref-varset ()
  (dolist (raw '(8 9 13 14 15 16 17 22 23))
    (should (fcl--admitted raw)))
  (dolist (raw '(24 31 38 39 48 55))
    (should-not (fcl--admitted raw))))

(ert-deftest w1v-fast-loop-no-longer-uses-fast-entry ()
  (should-not (sst--find (sst--function 'wf_bytecode_w_fast_run)
                         (lambda (x) (eq x 'wf_bytecode_w_fast_entry)))))

(provide 'nelisp-bytecode-varfast-test)
;;; nelisp-bytecode-varfast-test.el ends here
