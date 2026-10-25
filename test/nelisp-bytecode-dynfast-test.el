;;; nelisp-bytecode-dynfast-test.el --- Lean varbind/unbind in the fast loop -*- lexical-binding: t; -*-

(require 'nelisp-bytecode-dynbind-test)

;; ---- Model memory -------------------------------------------------------
;; Words are integers.  Cons cells are ids >= 100000 in `dfa--cells'.  The
;; generated IR is executed with checked host memory and mocked helpers; the
;; arms under test are the production definitions.

(defvar dfa--cells nil "Hash: cons id -> (car . cdr).")
(defvar dfa--next 100000)

(defun dfa--cons (a d)
  (let ((id (cl-incf dfa--next 4))) (puthash id (cons a d) dfa--cells) id))

(defun dfa--list (&rest xs)
  (let ((tail 3)) (dolist (x (reverse xs)) (setq tail (dfa--cons x tail))) tail))

(defconst dfa--aux 20000)
(defconst dfa--env 16384)

(defun dfa--run-helper (events handlers n &optional missing)
  "Run `wf_bytecode_w_fast_unbind_list' (the list path of `wf_bytecode_w_fast_unbind`, w3spec) on EVENTS (list of (NAME . SAVED)) with the
head HANDLER's saved-unwinds given as an index into EVENTS (or nil).
Return plist :rc :unwinds (remaining event names) :writes :dirty :lean-calls."
  (setq dfa--cells (make-hash-table) dfa--next 100000)
  (let* ((words (make-hash-table :test #'eql)) (writes nil) (dirty 0) (lean 0)
         (cells (make-list (length events) nil))
         (head 3))
    ;; Build the unwinds list so that cell i holds event i.
    (let ((tail 3))
      (cl-loop for i from (1- (length events)) downto 0
               do (let ((id (dfa--cons (dfa--cons (car (nth i events)) (cdr (nth i events))) tail)))
                    (setf (nth i cells) id) (setq tail id)))
      (setq head tail))
    (puthash (+ dfa--aux 8) head words)
    (puthash dfa--aux
             (if handlers
                 (dfa--list 7 0 0 0 (nth handlers cells))
               3)
             words)
    (let ((entry-ok (lambda (name) (not (memq name missing)))))
      (cl-letf (((symbol-function 'heap-read-u64)
                 (lambda (p o) (let ((v (gethash (+ p o) words 'absent)))
                                 (when (eq v 'absent) (error "Invalid read %s" (+ p o))) v)))
                ((symbol-function 'ptr-write-u64)
                 (lambda (p o w) (puthash (+ p o) w words) (push (list (+ p o) w) writes) 1))
                ((symbol-function 'wf_bytecode_w_consp)
                 (lambda (w) (if (gethash w dfa--cells) 1 0)))
                ((symbol-function 'nl_cons_car_word) (lambda (w) (car (gethash w dfa--cells))))
                ((symbol-function 'nl_cons_cdr_word) (lambda (w) (cdr (gethash w dfa--cells))))
                ;; An event is a cons id too: (NAME . SAVED).
                ((symbol-function 'wf_bytecode_w_lean_event_p)
                 (lambda (ev) (if (and (gethash ev dfa--cells)
                                       (integerp (car (gethash ev dfa--cells)))) 1 0)))
                ((symbol-function 'nl_val_tag)
                 (lambda (w) (if (>= w 1000) 11 4)))
                ((symbol-function 'nelisp_mirror_lookup_entry)
                 (lambda (_e name) (if (funcall entry-ok name) (+ 50000 (* name 64)) 0)))
                ((symbol-function 'nl_word_payload) #'identity)
                ((symbol-function 'wf_dirty) (lambda () (cl-incf dirty) 1))
                ((symbol-function 'wf_bytecode_w_lean_unbind)
                 (lambda (_e ev) (cl-incf lean)
                   (let ((c (gethash ev dfa--cells)))
                     (when (< (car c) 1000)
                       (puthash (+ 50000 (* (car c) 64) 16) (cdr c) words)
                       (push (list :lean (car c)) writes)))
                   0)))
        (let* ((fn (eval (cons 'lambda (cddr (sst--host-form
                                              (sst--function 'wf_bytecode_w_fast_unbind_list)))) t))
               (rc (funcall fn dfa--env dfa--aux n))
               (names nil) (cur (gethash (+ dfa--aux 8) words)))
          ;; Event cells hold the event id as car; map back to the name.
          (while (gethash cur dfa--cells)
            (push (car (gethash (car (gethash cur dfa--cells)) dfa--cells)) names)
            (setq cur (cdr (gethash cur dfa--cells))))
          (list :rc rc :unwinds (nreverse names) :writes (reverse writes)
                :dirty dirty :lean lean :words words))))))

(ert-deftest dfa-fast-unbind-single-restores-and-pops ()
  (let ((r (dfa--run-helper '((5 . 77) (6 . 88)) nil 1)))
    (should (= (plist-get r :rc) 0))
    (should (equal (plist-get r :unwinds) '(6)))
    (should (= (gethash (+ 50000 (* 5 64) 16) (plist-get r :words)) 77))
    (should (= (plist-get r :dirty) 1))
    (should (= (plist-get r :lean) 0))))

(ert-deftest dfa-fast-unbind-multiple-innermost-first ()
  (let ((r (dfa--run-helper '((5 . 77) (6 . 88) (7 . 99)) nil 2)))
    (should (= (plist-get r :rc) 0))
    (should (equal (plist-get r :unwinds) '(7)))
    (should (= (plist-get r :lean) 2))
    (should (equal (mapcar #'cadr (cl-remove-if-not (lambda (w) (eq (car w) :lean))
                                                      (plist-get r :writes)))
                   '(5 6)))))

(ert-deftest dfa-fast-unbind-migrated-event-is-popped-without-restore ()
  (let ((r (dfa--run-helper '((1005 . 77) (6 . 88)) nil 1)))
    (should (= (plist-get r :rc) 0))
    (should (equal (plist-get r :unwinds) '(6)))
    (should (= (plist-get r :dirty) 0))))

(ert-deftest dfa-fast-unbind-declines-without-change ()
  ;; Fewer events than N; general (non-lean) event; a handler established
  ;; inside the event; a missing mirror entry.  Each leaves AUX untouched.
  (dolist (case `((((5 . 1)) nil 2 nil)
                  (((5 . 1) (x . 2)) nil 2 nil)
                  (((x . 2) (5 . 1)) nil 1 nil)
                  (((5 . 1) (6 . 2)) 0 1 nil)
                  (((5 . 1) (6 . 2)) 1 2 nil)
                  (((5 . 1) (6 . 2)) nil 2 (6))))
    (pcase-let ((`(,events ,handlers ,n ,missing) case))
      (let ((r (dfa--run-helper events handlers n missing)))
        (should (= (plist-get r :rc) 1))
        (should (= (length (plist-get r :unwinds)) (length events)))
        (should (= (plist-get r :dirty) 0))
        (should-not (cl-remove-if-not (lambda (w) (and (consp w) (< (car w) 100000)
                                                      (/= (car w) -1)))
                                      (plist-get r :writes)))))))

(ert-deftest dfa-fast-unbind-outer-handler-does-not-block ()
  ;; A handler established before the popped events (saved unwinds = the
  ;; older cell) is not retired by popping the newer one.
  (let ((r (dfa--run-helper '((5 . 77) (6 . 88)) 1 1)))
    (should (= (plist-get r :rc) 0))
    (should (equal (plist-get r :unwinds) '(6)))))

;; ---- Fast-loop arms -------------------------------------------------------

(defun dfa--run-arm (code stack &optional opts)
  "Run production fast_run on CODE with helper mocks.  OPTS: :aux (footer
cell, default fixnum-coded 8), :bind-rc, :unbind-rc, :nconst, :env."
  (let* ((mem 4096) (cell 8192) (data 1024) (constants 32768) (depth 16)
         (foot (+ mem (* (1+ depth) 8)))
         (words (make-hash-table :test #'eql)) (binds nil) (unbinds nil)
         (sp (length stack)) (env (or (plist-get opts :env) dfa--env))
         (nconst (or (plist-get opts :nconst) 300)))
    (cl-loop for i below 300 do (puthash (+ constants (* i 8)) (+ 24582 (* i 64)) words))
    (cl-loop for i below 40 do (puthash (+ mem (* i 8)) 3 words))
    (cl-loop for w in stack for i from 0 do (puthash (+ mem (* i 8)) w words))
    (puthash cell 1 words) (puthash (+ cell 8) (1+ (* 4 mem)) words)
    (puthash (+ foot 40) (or (plist-get opts :aux) (1+ (* 4 777))) words)
    (let ((before (copy-hash-table words)))
      (cl-letf (((symbol-function 'heap-read-u8)
                 (lambda (p o)
                   (unless (and (= p data) (<= 0 o) (< o (length code)))
                     (error "Invalid byte fetch %s %s" p o))
                   (aref code o)))
                ((symbol-function 'heap-read-u64)
                 (lambda (p o) (let ((v (gethash (+ p o) words 'absent)))
                                 (when (eq v 'absent) (error "Invalid read %s" (+ p o))) v)))
                ((symbol-function 'ptr-read-u64)
                 (lambda (p o) (let ((v (gethash (+ p o) words 'absent)))
                                 (when (eq v 'absent) (error "Invalid read %s" (+ p o))) v)))
                ((symbol-function 'ptr-write-u64)
                 (lambda (p o w) (unless (gethash (+ p o) words) (error "Invalid write %s" (+ p o)))
                   (puthash (+ p o) w words) 1))
                ((symbol-function 'sst--truth)
                 (lambda (v) (and v (not (equal v 0)))))
                ((symbol-function 'wf_bytecode_w_lean_bind)
                 (lambda (e name valw aux)
                   (push (list e name valw aux) binds) (or (plist-get opts :bind-rc) 0)))
                ((symbol-function 'wf_bytecode_w_fast_unbind)
                 (lambda (e aux n)
                   (push (list e aux n) unbinds) (or (plist-get opts :unbind-rc) 0))))
        (let ((state (apply (eval (cons 'lambda (cddr (sst--host-form
                                                      (sst--function 'wf_bytecode_w_fast_run)))) t)
                            (list data (length code) mem depth constants nconst 0 sp env cell))))
          (list :state state :binds (reverse binds) :unbinds (reverse unbinds)
                :before before :after words))))))

(ert-deftest dfa-varbind-arm-runs-lean-bind-in-loop ()
  ;; Compact index, 1-byte and 2-byte operands; 138 stops the batch.
  (dolist (case '(([24 138] 0 1) ([29 138] 5 1) ([30 200 138] 200 2) ([31 1 1 138] 257 3)))
    (pcase-let ((`(,code ,idx ,len) case))
      (let ((r (dfa--run-arm code '(4 9))))
        (should (= (plist-get r :state) (+ (ash len 17) 1)))
        (should (equal (plist-get r :binds)
                       (list (list dfa--env (+ 24582 (* idx 64)) 9 777))))))))

(ert-deftest dfa-varbind-arm-declines-untouched ()
  (dolist (case `(([24 138] (9) (:aux 3))              ; no AUX yet
                  ([24 138] (9) (:bind-rc 1))          ; lean binder refuses
                  ([24 138] () nil)                    ; empty stack
                  ([24 138] (9) (:env 0))
                  ([29 138] (9) (:nconst 5))           ; index out of range
                  ([30] (9) nil) ([31 1] (9) nil)))    ; truncated operand
    (let ((r (apply #'dfa--run-arm (list (car case) (cadr case) (caddr case)))))
      (should (= (plist-get r :state) (length (cadr case))))
      (should (sst--same-memory-p (plist-get r :before) (plist-get r :after))))))

(ert-deftest dfa-unbind-arm-runs-helper-in-loop ()
  (dolist (case '(([41 138] 1 1) ([45 138] 5 1) ([46 3 138] 3 2) ([47 1 1 138] 257 3)))
    (pcase-let ((`(,code ,n ,len) case))
      (let ((r (dfa--run-arm code '(4))))
        (should (= (plist-get r :state) (+ (ash len 17) 1)))
        (should (equal (plist-get r :unbinds) (list (list dfa--env 777 n))))))))

(ert-deftest dfa-unbind-arm-declines-untouched ()
  (dolist (case `(([40 138] (4) nil)                   ; unbind 0 stays outer
                  ([41 138] (4) (:aux 3))
                  ([41 138] (4) (:unbind-rc 1))
                  ([46] (4) nil) ([47 1] (4) nil)))
    (let ((r (apply #'dfa--run-arm (list (car case) (cadr case) (caddr case)))))
      (should (= (plist-get r :state) (length (cadr case))))
      (should (sst--same-memory-p (plist-get r :before) (plist-get r :after))))))

(ert-deftest dfa-starters-and-structure ()
  ;; Unbind (40..47) starts a batch; varbind (24..31) does not (a first-op
  ;; varbind in a frame without AUX would waste a fast-run entry); varref 14/15
  ;; and varset 16..23 start one (w1var: they run in the loop); the other
  ;; neighbours stay outside.
  (dolist (raw '(40 41 42 43 44 45 46 47)) (should (fcl--admitted raw)))
  (dolist (raw '(24 25 31 38 39 48 135 179)) (should-not (fcl--admitted raw)))
  (dolist (raw '(14 15 16 17 22 23)) (should (fcl--admitted raw)))
  (dolist (raw '(0 13 32 37 56 61 63 64 65 71 83 94 129 134 136 137 158 162 165 166 178 182 183 192))
    (should (fcl--admitted raw)))
  ;; No pre-dispatch filter: the first test of the dispatch is unchanged.
  (should (sst--find (sst--function 'wf_bytecode_w_fast_run)
                     (lambda (x) (equal x '(< raw 93)))))
  (should (sst--find (sst--function 'wf_bytecode_w_fast_run)
                     (lambda (x) (equal x '(wf_bytecode_w_fast_unbind env (sar ref 2) b)))))
  (should (sst--find (sst--function 'wf_bytecode_w_fast_run)
                     (lambda (x) (equal (car-safe x) 'wf_bytecode_w_lean_bind))))
  (should (sst--function 'wf_bytecode_w_fast_unbind_list))
  ;; w3spec: the dispatcher pops in-place records and delegates; no helper allocates.
  (should (sst--find (sst--function 'wf_bytecode_w_fast_unbind)
                     (lambda (x) (eq x 'wf_bytecode_w_pop_lean))))
  (should-not (sst--find (sst--function 'wf_bytecode_w_fast_unbind)
                         (lambda (x) (memq x '(alloc-bytes nl_alloc_consbox wf_bytecode_w_cons2)))))
  (should-not (sst--find (sst--function 'wf_bytecode_w_fast_unbind_list)
                         (lambda (x) (memq x '(alloc-bytes nl_alloc_consbox wf_bytecode_w_cons2))))))

(provide 'nelisp-bytecode-dynfast-test)
