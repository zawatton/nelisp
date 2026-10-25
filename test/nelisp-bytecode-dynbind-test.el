;;; nelisp-bytecode-dynbind-test.el --- Return starter and store deferral -*- lexical-binding: t; -*-

(require 'nelisp-bytecode-fastloop-test)

(defun db--run (code stack &optional options outer)
  "Execute production fast IR with checked memory and setter boundaries.
OPTIONS controls unsupported symbol/environment shapes. OUTER executes the
existing varset arm for the same input, with the same helper adapters."
  (let* ((mem 4096) (cell 8192) (data 1024) (constants 32768) (env 16384)
         (words (make-hash-table :test #'eql)) (before nil) (sets nil)
         (lookups 0) (scratch 0) (fuel 1000) (sp (length stack))
         (nconst (or (plist-get options :nconst) 260))
         (raw (aref code 0)) (width (cond ((= raw 22) 1) ((= raw 23) 2) (t 0)))
         (operand (if (= width 0) (- raw 16)
                    (+ (if (> (length code) 1) (aref code 1) 0)
                       (if (and (= width 2) (> (length code) 2))
                           (* 256 (aref code 2)) 0)))))
    (cl-loop for i below 260 do (puthash (+ constants (* i 8)) (+ 24582 (* i 64)) words))
    (cl-loop for i below 16 do (puthash (+ mem (* i 8)) 3 words))
    (cl-loop for w in stack for i from 0 do (puthash (+ mem (* i 8)) w words))
    (puthash cell 1 words)
    (puthash (+ cell 8) (1+ (* 4 mem)) words)
    (puthash (+ env 112) (or (plist-get options :flags) 0) words)
    (setq before (copy-hash-table words))
    (cl-labels
        ((rd (p o) (let ((v (gethash (+ p o) words 'absent)))
                     (when (eq v 'absent) (error "Invalid read: %s" (+ p o))) v))
         (wr (p o w) (rd p o) (puthash (+ p o) w words)))
      (cl-letf (((symbol-function 'heap-read-u8)
                 (lambda (p o)
                   (unless (and (= p data) (<= 0 o) (< o (length code)))
                     (error "Invalid byte fetch: %s %s" p o)) (aref code o)))
                ((symbol-function 'heap-read-u64) #'rd)
                ((symbol-function 'ptr-read-u64) #'rd)
                ((symbol-function 'ptr-write-u64) #'wr)
                ((symbol-function 'sst--truth)
                 (lambda (v) (when (< (cl-decf fuel) 0) (error "Fuel exhausted"))
                   (and v (not (equal v 0)))))
                ((symbol-function 'nl_sym_direct_env_p)
                 (lambda (_) (if (plist-get options :indirect) 0 1)))
                ((symbol-function 'wf_bytecode_w_tag)
                 (lambda (w) (if (= w 3) 0
                               (if (= w 7) 1 (or (plist-get options :tag) 4)))))
                ((symbol-function 'bf_keyword_raw)
                 (lambda (_) (if (plist-get options :keyword) 1 0)))
                ((symbol-function 'nelisp_mirror_lookup_entry_owned)
                 (lambda (_ name) (cl-incf lookups)
                   (if (plist-get options :missing) 0 name)))
                ((symbol-function 'nl_sym_redirect)
                 (lambda (_) (or (plist-get options :redirect) 0)))
                ((symbol-function 'nl_ent_w)
                 (lambda (_ k) (should (= k 3)) (if (plist-get options :constant) 7 3)))
                ((symbol-function 'wf_bytecode_scratch)
                 (lambda (_) (cl-incf scratch) 65536))
                ((symbol-function 'wf_bytecode_w_view) (lambda (w _) w))
                ((symbol-function 'bf_set_dynamic_value)
                 (lambda (e s v) (should (= e env))
                   (push (list s v) sets) (if (plist-get options :signal) 1 0))))
        (let ((state
               (if outer
                   (eval (sst--host-form
                          `(let ((operand ,operand) (nconst ,nconst) (env ,env)
                                 (cdata ,constants) (mem ,mem) (sp ,sp)
                                 (top ,(+ mem (* (1- sp) 8))) (curfp ,cell)
                                 (done 0) (bad-op 0) (raw ,raw))
                             ,(fcl--outer '(= base 16)) (list sp done))) t)
                 (apply (eval (cons 'lambda (cddr (sst--host-form
                                                  (sst--function 'wf_bytecode_w_fast_run)))) t)
                        (list data (length code) mem 16 constants nconst 0 sp env cell)))))
          (list :state state :sets (reverse sets) :scratch scratch :lookups lookups
                :before before :after words))))))

(ert-deftest db-varset-plain-replays-outer ()
  ;; Every compact index plus both operand widths, including a high byte.
  (dolist (code '([16 138] [17 138] [18 138] [19 138] [20 138] [21 138]
                  [22 255 138] [23 1 1 138]))
    (dolist (value '(3 7 5 16386))
      (let ((fast (db--run code (list value)))
            (outer (db--run code (list value) nil t)))
        (should (= (plist-get fast :state) 1))
        (should-not (plist-get fast :sets))
        (should (= (length (plist-get outer :sets)) 1))
        (should (sst--same-memory-p (plist-get fast :before) (plist-get fast :after)))
        (should (= (car (plist-get outer :state)) 0))
        (should (= (cadr (plist-get outer :state)) 0))))))

(ert-deftest db-varset-invalid-untouched ()
  (dolist (case '(([16 138] () nil) ([22] (5) nil) ([23 1] (5) nil)
                  ([21 138] (5) (:nconst 5)) ([22 6 138] (5) (:nconst 6))
                  ([23 0 1 138] (5) (:nconst 256))))
    (let ((r (apply #'db--run case)))
      (should (= (plist-get r :state) (length (cadr case))))
      (should-not (plist-get r :sets)) (should (= (plist-get r :scratch) 0))
      (should (sst--same-memory-p (plist-get r :before) (plist-get r :after))))))

(ert-deftest db-varset-unsupported-untouched ()
  (dolist (options '((:tag 0) (:tag 1) (:tag 2) (:tag 16) (:keyword t)
                     (:constant t) (:redirect 1) (:redirect 2) (:redirect 3)
                     (:redirect 4) (:flags 1048576) (:indirect t) (:missing t)))
    (let ((r (db--run [16 138] '(5) options)))
      (should (= (plist-get r :state) 1))
      (should-not (plist-get r :sets)) (should (= (plist-get r :scratch) 0))
      (should (sst--same-memory-p (plist-get r :before) (plist-get r :after))))))

(ert-deftest db-varset-signalling-helper-not-entered ()
  (let ((r (db--run [16 138] '(5) '(:signal t))))
    (should (= (plist-get r :state) 1))
    (should-not (plist-get r :sets))))

(ert-deftest db-varset-starters-and-binding-deferral ()
  ;; dynfast: unbind (40..47) now starts a batch; see
  ;; nelisp-bytecode-dynfast-test.el.  Varset and varbind starters stay out.
  (dolist (raw '(14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31))
    (should-not (fcl--admitted raw))))

(ert-deftest db-return-starter-deferred ()
  ;; #[257 "\\207" [] 2] must return directly in the outer handler.
  (should-not (fcl--admitted 135))
  ;; Breturn is still implemented when reached within a useful batch.
  (should (sst--find (sst--function 'wf_bytecode_w_fast_run)
                    (lambda (x) (equal x '(< raw 136)))))
  (dolist (raw '(129 130 131 132 133 134 136 137))
    (should (fcl--admitted raw))))

(provide 'nelisp-bytecode-dynbind-test)
