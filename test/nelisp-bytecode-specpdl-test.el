;;; nelisp-bytecode-specpdl-test.el --- In-place lean binding records -*- lexical-binding: t; -*-

;; w3spec: a shallow (lean) dynamic binding is a fixed {NAME, SAVED} record in
;; the activation's AUX root block instead of two conses on the unwinds list.
;; The production definitions run on host memory with the primitives mocked.

(require 'nelisp-bytecode-stackset-fast-test)

(defconst spd--aux 20000)
(defconst spd--env 16384)
(defconst spd--cap 5)

(defvar spd--words nil)
(defvar spd--cells nil)
(defvar spd--next 200000)
(defvar spd--dirty 0)
(defvar spd--calls nil)
(defvar spd--keywords nil)

(defun spd--sym (i) (+ 100000 (* i 64) 6))
(defun spd--cell (i) (+ 100000 (* i 64) 16))

(defun spd--cons (a d)
  (let ((id (cl-incf spd--next 4))) (puthash id (cons a d) spd--cells) id))

(defun spd--fresh (&optional opts)
  "Reset the model.  Symbols 0..29: header kind 4, cell value 21 + 4 I."
  (setq spd--words (make-hash-table :test #'eql) spd--cells (make-hash-table)
        spd--next 200000 spd--dirty 0 spd--calls nil spd--keywords (plist-get opts :keywords))
  (cl-loop for off from 0 to 184 by 8 do (puthash (+ spd--aux off) 3 spd--words))
  (puthash (+ spd--aux 24) 1 spd--words)
  (puthash (+ spd--aux 96) 1 spd--words)
  (puthash (+ spd--env 112) (or (plist-get opts :flags) 0) spd--words)
  (puthash (+ spd--env 168) (or (plist-get opts :indirect) 0) spd--words)
  (puthash 50000 (or (plist-get opts :registry) 0) spd--words)
  (puthash (+ 50100 48) (or (plist-get opts :pin) 0) spd--words)
  (cl-loop for i below 30 do
           (puthash (+ 100000 (* i 64)) 4 spd--words)
           (puthash (+ 100000 (* i 64) 16) (+ 21 (* 4 i)) spd--words)))

(defun spd--rd (p o)
  (let ((v (gethash (+ p o) spd--words 'absent)))
    (when (eq v 'absent) (error "Invalid read: %s" (+ p o))) v))
(defun spd--wr (p o w) (spd--rd p o) (puthash (+ p o) w spd--words) 1)

(defmacro spd--with-model (names &rest body)
  "Run BODY with the production definitions NAMES installed on host memory."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'heap-read-u64) #'spd--rd)
             ((symbol-function 'ptr-read-u64) #'spd--rd)
             ((symbol-function 'ptr-write-u64) #'spd--wr)
             ((symbol-function 'data-addr)
              (cons 'macro (lambda (n) (if (eq n 'nl_thread_registry) 50000 50100))))
             ((symbol-function 'wf_dirty) (lambda () (cl-incf spd--dirty) 1))
             ((symbol-function 'nl_word_canon) #'identity)
             ((symbol-function 'bf_keyword_raw)
              (lambda (w) (if (memq w spd--keywords) 1 0)))
             ((symbol-function 'wf_bytecode_w_cons2) #'spd--cons)
             ((symbol-function 'wf_bytecode_w_consp)
              (lambda (w) (if (gethash w spd--cells) 1 0)))
             ((symbol-function 'nl_cons_car_word) (lambda (w) (car (gethash w spd--cells))))
             ((symbol-function 'nl_cons_cdr_word) (lambda (w) (cdr (gethash w spd--cells))))
             ((symbol-function 'sst--truth)
              (lambda (v) (and v (not (equal v 0))))))
     (spd--install ,names (lambda () ,@body))))

(defun spd--install (names thunk)
  (let ((saved (mapcar (lambda (n) (cons n (and (fboundp n) (symbol-function n)))) names)))
    (unwind-protect
        (progn
          (dolist (n names)
            (fset n (eval (cons 'lambda (cddr (sst--host-form (sst--function n)))) t)))
          (funcall thunk))
      (dolist (s saved) (if (cdr s) (fset (car s) (cdr s)) (fmakunbound (car s)))))))

(defconst spd--chain
  '(wf_bytecode_w_aux_spill wf_bytecode_w_aux_unwinds wf_bytecode_w_pop_lean
    wf_bytecode_w_lean_bind wf_bytecode_w_fast_unbind))

(defun spd--count () (ash (gethash (+ spd--aux 96) spd--words) -2))
(defun spd--rec (i) (list (gethash (+ spd--aux 104 (* i 16)) spd--words)
                          (gethash (+ spd--aux 112 (* i 16)) spd--words)))
(defun spd--list-names ()
  "Names of the (NAME . SAVED) events on the unwinds list, newest first."
  (let ((cur (gethash (+ spd--aux 8) spd--words)) out)
    (while (gethash cur spd--cells)
      (let ((ev (car (gethash cur spd--cells))))
        (push (list (car (gethash ev spd--cells)) (cdr (gethash ev spd--cells))) out))
      (setq cur (cdr (gethash cur spd--cells))))
    (nreverse out)))
(defun spd--cellval (i) (gethash (spd--cell i) spd--words))
(defun spd--bind (i val) (wf_bytecode_w_lean_bind spd--env (spd--sym i) val spd--aux))

(ert-deftest spd-bind-pushes-a-record-and-conses-nothing ()
  (spd--fresh)
  (spd--with-model '(wf_bytecode_w_aux_spill wf_bytecode_w_lean_bind)
    (should (= (spd--bind 3 1002) 0))
    (should (= (spd--bind 4 9) 0))
    (should (= (spd--count) 2))
    (should (equal (spd--rec 0) (list (spd--sym 3) (+ 21 12))))
    (should (equal (spd--rec 1) (list (spd--sym 4) (+ 21 16))))
    (should (= (spd--cellval 3) 1002))
    (should (= (spd--cellval 4) 9))
    (should (= (gethash (+ spd--aux 8) spd--words) 3))
    (should (= spd--next 200000))
    ;; Barrier: only the non-immediate new value (1005 has bit 0 clear).
    (should (= spd--dirty 1))))

(ert-deftest spd-bind-declines-untouched ()
  (dolist (case `((:name 7)                       ; not a symbol-object word
                  (:valw 1000)                    ; legacy view
                  (:header 16)                    ; uninterned
                  (:header ,(+ 4 256))            ; alias redirect
                  (:header ,(+ 4 1024))           ; trapped write
                  (:opts (:flags 1048576))        ; watcher mode
                  (:opts (:indirect 1))           ; worker env
                  (:opts (:registry 1))           ; threads registered
                  (:opts (:pin 2))                ; root-pin capture
                  (:opts (:keywords (,(spd--sym 3))))
                  (:cell 0)))                     ; void legacy cell
    (spd--fresh (plist-get case :opts))
    (when (plist-get case :header) (puthash 100192 (plist-get case :header) spd--words))
    (when (plist-get case :cell) (puthash (spd--cell 3) (plist-get case :cell) spd--words))
    (let ((before (copy-hash-table spd--words)))
      (spd--with-model '(wf_bytecode_w_aux_spill wf_bytecode_w_lean_bind)
        (should (= (wf_bytecode_w_lean_bind spd--env (or (plist-get case :name) (spd--sym 3))
                                            (or (plist-get case :valw) 1002) spd--aux)
                   1))
        (should (equal (hash-table-keys spd--words) (hash-table-keys before)))
        (maphash (lambda (k v) (should (equal v (gethash k before)))) spd--words)
        (should (= spd--dirty 0))))))

(ert-deftest spd-sixth-bind-spills-in-order ()
  (spd--fresh)
  (spd--with-model '(wf_bytecode_w_aux_spill wf_bytecode_w_lean_bind)
    (dotimes (i 5) (should (= (spd--bind i (+ 5 (* 4 i))) 0)))
    (should (= (spd--count) spd--cap))
    (should (= (spd--bind 5 9) 0))
    (should (= (spd--count) 1))
    (should (equal (spd--rec 0) (list (spd--sym 5) (+ 21 20))))
    (should (equal (spd--rec 1) '(3 3)))
    ;; Newest first on the list: record 4 on top, record 0 deepest.
    (should (equal (spd--list-names)
                   (cl-loop for i from 4 downto 0 collect (list (spd--sym i) (+ 21 (* 4 i))))))))

(ert-deftest spd-fast-unbind-pops-records-innermost-first ()
  (spd--fresh)
  (spd--with-model spd--chain
    (spd--bind 1 5) (spd--bind 2 9) (spd--bind 3 13)
    (setq spd--dirty 0)
    (should (= (wf_bytecode_w_fast_unbind spd--env spd--aux 2) 0))
    (should (= (spd--count) 1))
    (should (= (spd--cellval 3) (+ 21 12)))
    (should (= (spd--cellval 2) (+ 21 8)))
    (should (= (spd--cellval 1) 5))
    (should (equal (spd--rec 1) '(3 3)))
    (should (equal (spd--rec 2) '(3 3)))
    (should (= spd--dirty 0))
    (should (= (wf_bytecode_w_fast_unbind spd--env spd--aux 1) 0))
    (should (= (spd--count) 0))
    (should (= (spd--cellval 1) (+ 21 4)))
    (should (equal (spd--rec 0) '(3 3)))))

(ert-deftest spd-fast-unbind-barrier-for-boxed-saved-only ()
  (spd--fresh)
  (puthash (spd--cell 1) 1002 spd--words)
  (spd--with-model spd--chain
    (spd--bind 1 5) (spd--bind 2 9)
    (setq spd--dirty 0)
    (wf_bytecode_w_fast_unbind spd--env spd--aux 1)
    (should (= spd--dirty 0))
    (wf_bytecode_w_fast_unbind spd--env spd--aux 1)
    (should (= spd--dirty 1))
    (should (= (spd--cellval 1) 1002))))

(ert-deftest spd-fast-unbind-mixed-declines-and-empty-delegates ()
  (spd--fresh)
  (spd--with-model spd--chain
    (spd--bind 1 5)
    (let ((before (copy-hash-table spd--words)))
      (should (= (wf_bytecode_w_fast_unbind spd--env spd--aux 2) 1))
      (maphash (lambda (k v) (should (equal v (gethash k before)))) spd--words)))
  (spd--fresh)
  (spd--with-model spd--chain
    (cl-letf (((symbol-function 'wf_bytecode_w_fast_unbind_list)
               (lambda (_e _a n) (push n spd--calls) 1)))
      (should (= (wf_bytecode_w_fast_unbind spd--env spd--aux 3) 1))
      (should (equal spd--calls '(3))))))

(ert-deftest spd-aux-unwinds-folds-records-into-the-list ()
  (spd--fresh)
  (spd--with-model spd--chain
    (spd--bind 1 5) (spd--bind 2 9)
    (let ((head (wf_bytecode_w_aux_unwinds spd--aux)))
      (should (gethash head spd--cells))
      (should (= (spd--count) 0))
      (should (equal (spd--list-names)
                     (list (list (spd--sym 2) (+ 21 8)) (list (spd--sym 1) (+ 21 4)))))
      (should (equal (spd--rec 0) '(3 3))))
    ;; A second call changes nothing.
    (let ((n spd--next))
      (wf_bytecode_w_aux_unwinds spd--aux)
      (should (= n spd--next)))))

(ert-deftest spd-unwind-step-pops-records-before-the-list ()
  (spd--fresh)
  (let ((seen nil))
    (spd--with-model '(wf_bytecode_w_aux_spill wf_bytecode_w_pop_lean wf_bytecode_w_lean_bind
                      wf_bytecode_unwind_step)
      (cl-letf (((symbol-function 'wf_bytecode_handlers_cross_unwind) (lambda (&rest _) 0))
                ((symbol-function 'wf_bytecode_w_lean_event_p)
                 (lambda (ev) (if (integerp (car (gethash ev spd--cells))) 1 0)))
                ((symbol-function 'wf_bytecode_w_lean_unbind)
                 (lambda (_e ev) (push (car (gethash ev spd--cells)) seen) 0)))
        ;; A list event under two records.
        (puthash (+ spd--aux 8) (spd--cons (spd--cons (spd--sym 9) 77) 3) spd--words)
        (spd--bind 1 5) (spd--bind 2 9)
        (should (= (wf_bytecode_unwind_step spd--env spd--aux 3 0) 0))
        (should (= (spd--count) 0))
        (should (= (spd--cellval 1) (+ 21 4)))
        (should (= (spd--cellval 2) (+ 21 8)))
        ;; The list event was undone after the records.
        (should (equal seen (list (spd--sym 9))))
        (should (= (gethash (+ spd--aux 8) spd--words) 3))))))

(ert-deftest spd-structural-owners-spill-and-aux-shape ()
  (let ((has (lambda (fn sym) (sst--find (sst--function fn) (lambda (f) (eq f sym))))))
    (dolist (fn '(wf_bytecode_unwind_push wf_bytecode_handlers_push))
      (should (funcall has fn 'wf_bytecode_w_aux_spill)))
    (should (funcall has 'wf_bytecode_execute_frame 'wf_bytecode_w_aux_unwinds))
    (should (funcall has 'wf_bytecode_unwind_step 'wf_bytecode_w_pop_lean))
    ;; nl_ps_outer_saved and nl_thread_unshallow_lean_at live in another constant.
    (with-temp-buffer
      (insert-file-contents (locate-library "nelisp-standalone-build.el"))
      (should (= 2 (how-many "(wf_bytecode_w_link_events link)" (point-min)))))
    (should (sst--find (sst--function 'wf_bytecode_frame_aux)
                       (lambda (f) (equal f '(nl_root_frame env 23)))))
    ;; The lean binder neither conses nor looks anything up.
    (dolist (sym '(wf_bytecode_w_cons2 nelisp_mirror_lookup_entry nl_sb_plain_entry_p nl_cons_alloc))
      (should-not (funcall has 'wf_bytecode_w_lean_bind sym)))
    (should-not (funcall has 'wf_bytecode_w_pop_lean 'nelisp_mirror_lookup_entry))))

(provide 'nelisp-bytecode-specpdl-test)
