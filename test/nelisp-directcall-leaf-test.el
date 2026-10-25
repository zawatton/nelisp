;;; nelisp-directcall-leaf-test.el --- Leaf directcall hits take no backtrace record -*- lexical-binding: t; -*-

;; w1leaf: a directcall id below 112 (no allocation beyond the table/cons
;; growth the allocator never collects for, no Lisp re-entry, no safepoint)
;; runs its body with neither `nl_n4_push_words' nor `nl_n4_finish_w' on a hit.
;; Every decline, the depth limit, a missing activation state, the debug
;; policy and the allocating ids 112 and up keep the old route.

(require 'nelisp-directcall-test)
(require 'nelisp-hashfast-test)

(defconst ndl--leaf-ids '(1 10 -16 -15 27 101 102 103 104 105 106 107 108 109 110 111))

(defun ndl--mem (depth)
  "A memory model: env 1000, activation state at 3000 holding DEPTH, policy 0."
  (let ((mem (make-hash-table :test #'eql)))
    (puthash 1176 3000 mem) (puthash 3000 depth mem) (puthash 1112 0 mem)
    mem))

(defmacro ndl--with-invoke (mem pushed finished runs run-status &rest body)
  "Run BODY with invoke's helpers mocked; PUSHED, FINISHED and RUNS count calls."
  (declare (indent 5))
  `(cl-letf (((symbol-function 'heap-read-u64) (lambda (p i) (gethash (+ p i) ,mem 0)))
             ((symbol-function 'ptr-write-u64) (lambda (p i v) (puthash (+ p i) v ,mem)))
             ((symbol-function 'ptr-read-u64) (lambda (p i) (gethash (+ p i) ,mem 0)))
             ((symbol-function 'nl_val_tag) (lambda (_) 4))
             ((symbol-function 'nl_n4_push_words)
              (lambda (&rest _) (cl-incf ,pushed) 55))
             ((symbol-function 'nl_n4_finish_w)
              (lambda (w &rest _) (cl-incf ,finished) w))
             ((symbol-function 'nl_directcall_fast_run)
              (lambda (_id _b _n _out) (cl-incf ,runs) -4))
             ((symbol-function 'nl_directcall_run)
              (lambda (_id _b _n out _e)
                (cl-incf ,runs)
                (when (= ,run-status 0) (puthash out 4242 ,mem))
                ,run-status)))
     ,@body))

(ert-deftest ndl-invoke-leaf-hit-takes-no-record ()
  (ndc--with-definitions
   (lambda (_)
     (dolist (id ndl--leaf-ids)
       (let ((mem (ndl--mem 7)) (pushed 0) (finished 0) (runs 0) (status 0))
         (ndl--with-invoke mem pushed finished runs status
           (cl-letf (((symbol-function 'nl_directcall_id) (lambda (&rest _) id)))
             (should (= (nl_directcall_invoke 50002 20022 100 2 1000 200) 0))
             (should (= (gethash 200 mem) 4242))
             (should (= pushed 0)) (should (= finished 0)) (should (>= runs 1)))))))))

(ert-deftest ndl-invoke-leaf-decline-is-2-without-a-record ()
  (ndc--with-definitions
   (lambda (_)
     (dolist (id ndl--leaf-ids)
       (let ((mem (ndl--mem 7)) (pushed 0) (finished 0) (runs 0) (status -3))
         (puthash 200 99 mem)
         (ndl--with-invoke mem pushed finished runs status
           (cl-letf (((symbol-function 'nl_directcall_id) (lambda (&rest _) id)))
             (should (= (nl_directcall_invoke 50002 20022 100 2 1000 200) 2))
             (should (= (gethash 200 mem) 99))
             (should (= pushed 0)) (should (= finished 0))))))))
  )

(ert-deftest ndl-invoke-keeps-the-record-outside-the-leaf-path ()
  (ndc--with-definitions
   (lambda (_)
     (dolist (c `((7 112) (32768 105) (40000 1) (nostate 105) (7 117)))
       (let ((mem (ndl--mem (if (eq (car c) 'nostate) 0 (car c))))
             (pushed 0) (finished 0) (runs 0) (status 0))
         (when (eq (car c) 'nostate) (puthash 1176 0 mem))
         (ndl--with-invoke mem pushed finished runs status
           (cl-letf (((symbol-function 'nl_directcall_id) (lambda (&rest _) (cadr c))))
             (should (= (nl_directcall_invoke 50002 20022 100 2 1000 200) 0))
             (should (= pushed (if (= (cadr c) 105) (if (eq (car c) 7) 0 1) 1)))
             (should (= finished pushed)))))))))

(ert-deftest ndl-invoke-debug-policy-still-declines-before-the-body ()
  (ndc--with-definitions
   (lambda (_)
     (dolist (policy '(1048576 2097152 3145728))
       (let ((mem (ndl--mem 7)) (pushed 0) (finished 0) (runs 0) (status 0))
         (puthash 1112 policy mem)
         (ndl--with-invoke mem pushed finished runs status
           (cl-letf (((symbol-function 'nl_directcall_id) (lambda (&rest _) 1)))
             (should (= (nl_directcall_invoke 50002 20022 100 1 1000 200) 2))
             (should (= runs 0)) (should (= pushed 0)))))))))

(ert-deftest ndl-walker-leaf-hit-takes-no-record ()
  (hf--install '(wf_cb_prim_call))
  (dolist (c '((105 7 0 0) (105 7 -3 2) (111 7 0 0) (112 7 0 0) (105 32768 0 0) (1 40000 0 0)))
    (let ((mem (ndl--mem (nth 1 c))) (pushed 0) (finished 0) (runs 0)
          (status (nth 2 c)) (invoked 0))
      (puthash 100 20006 mem) (puthash 140 10002 mem)
      (puthash 148 20006 mem) (puthash 156 10018 mem) (puthash 164 20030 mem)
      (puthash 10000 20006 mem) (puthash 10008 10018 mem)
      (puthash 10016 20030 mem) (puthash 10024 3 mem)
      (puthash (+ 20000 24) 10002 mem)
      (ndl--with-invoke mem pushed finished runs status
        (cl-letf (((symbol-function 'nelisp_env_lookup_function_w) (lambda (&rest _) 10002))
                ((symbol-function 'nelisp_env_lookup_function_raw_w) (lambda (&rest _) 10002))
                  ((symbol-function 'nl_directcall_invoke)
                   (lambda (&rest _) (cl-incf invoked) 2)))
          (let ((leaf (and (< (car c) 112) (< (nth 1 c) 32768))))
            (should (= (wf_cb_prim_call 1000 100 1 40 (+ 1017 (car c)))
                       (if leaf (nth 3 c) (if (= status -3) 2 0))))
            (should (= invoked 0))
            (should (= pushed (if leaf 0 1)))
            (should (= finished pushed))
            (when (and leaf (= status 0)) (should (= (gethash 124 mem) 4242)))))))))

;;; Static guard: nothing a leaf body can reach is a collector entry, a thread
;;; safepoint or a Lisp re-entry.

(defconst ndl--root (or (getenv "NDL_ROOT")   ; a mutated tree, for the negative control
                        (file-name-directory (directory-file-name
                                              (file-name-directory load-file-name)))))
(defvar ndl--index nil)

(defun ndl--build-index ()
  (or ndl--index
      (let ((table (make-hash-table :test #'eq))
            (files (cons (expand-file-name "scripts/nelisp-standalone-build.el" ndl--root)
                         (directory-files (expand-file-name "lisp" ndl--root) t "\\.el\\'"))))
        (dolist (file files)
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (while (re-search-forward "(defun \\([A-Za-z0-9_]+\\) " nil t)
              (let ((name (intern (match-string 1))) (start (match-beginning 0)))
                (save-excursion
                  (goto-char start)
                  (let ((form (condition-case nil (read (current-buffer)) (error nil))))
                    (when form (puthash name (cons form (gethash name table)) table))))))))
        (setq ndl--index table))))

(defun ndl--symbols (form acc)
  (cond ((symbolp form) (if form (cons form acc) acc))
        ((consp form) (ndl--symbols (cdr form) (ndl--symbols (car form) acc)))
        (t acc)))

(defun ndl--reach (roots skip)
  (let ((index (ndl--build-index)) (seen (make-hash-table :test #'eq)) (work roots))
    (while work
      (let ((name (pop work)))
        (unless (or (gethash name seen) (memq name skip))
          (puthash name t seen)
          (dolist (form (gethash name index))
            (dolist (s (ndl--symbols form nil))
              (when (and (gethash s index) (not (gethash s seen))) (push s work)))))))
    seen))

(defconst ndl--forbidden
  ;; Collector entries (the poll, the mid-form and form-boundary collections,
  ;; the mark/sweep drivers), thread safepoints and waits, Lisp re-entry
  ;; (apply, eval, funcall), the backtrace record, and signalling.  Read-only
  ;; helpers (`nl_gc_in_arena', the dynamic-binding lookups) are not listed.
  "\\`\\(nl_gc_\\(collect\\|midform\\|vm_poll\\|sweep\\|mark\\|run\\|budget\\)\\|wf_bytecode_\\(poll\\|back\\)\\|nl_thread_\\(park\\|join\\|wait\\)\\|nelisp_eval_call\\|nl_apply\\|nl_eval\\|m5_fx_primitive_funcall\\|m5_fx_funcall\\|nl_n4_push\\|nl_n4_finish\\|nl_signal\\|bf_funcall\\|nl_funcall\\)")

(defun ndl--hits (seen)
  (let (hits)
    (maphash (lambda (k _) (when (string-match-p ndl--forbidden (symbol-name k)) (push k hits))) seen)
    (sort hits #'string<)))

(ert-deftest ndl-leaf-bodies-reach-no-collector-safepoint-or-lisp ()
  ;; Control first: the walker does see a collector behind the VM poll.
  (should (memq 'nl_gc_midform_collect_now (ndl--hits (ndl--reach '(wf_bytecode_poll) nil))))
  (should (memq 'nl_thread_park_safepoint (ndl--hits (ndl--reach '(nl_thread_join_wait) nil))))
  ;; The ids a leaf call can carry are exactly the table's ids below 112, and
  ;; 13 (funcall, the only Lisp re-entry in `m5_fx_primitive_run') is not one.
  (let ((ids (mapcar #'cadr '(("car" 1) ("nth" 10) ("length" -16) ("aset" -15) ("logand" 27)
                              ("gethash" 101) ("puthash" 102) ("remhash" 103)
                              ("hash-table-count" 104) ("1+" 105) ("1-" 106) ("+" 107)
                              ("-" 108) ("cdr" 109) ("identity" 110) ("cons" 111)))))
    (should (equal (sort (copy-sequence ids) #'<) (sort (copy-sequence ndl--leaf-ids) #'<)))
    (should-not (memq 13 ids)))
  (with-temp-buffer
    (insert-file-contents (expand-file-name "scripts/nelisp-standalone-build.el" ndl--root))
    ;; The generator's own table is the source of the ids the guard assumes.
    (should (search-forward "(defconst nelisp-standalone--directcall-specs\n  '((\"car\" 1) (\"nth\" 10) (\"length\" -16) (\"aset\" -15) (\"logand\" 27)" nil t)))
  (let ((seen (ndl--reach '(nl_directcall_fast_run nl_directcall_callback_run
                            nl_directcall_hash_run nl_directcall_run m5_fx_primitive_run
                            nl_directcall_hash_rem nl_ht_rem_synced_w)
                          '(nl_directcall_alloc_run m5_fx_primitive_funcall))))
    (should (> (hash-table-count seen) 20))
    (should (null (ndl--hits seen)))))

(provide 'nelisp-directcall-leaf-test)
