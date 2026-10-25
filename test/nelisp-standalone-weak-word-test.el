;;; nelisp-standalone-weak-word-test.el --- Focused weak WORD checks -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'nelisp-standalone-build)

(defvar nelisp-weak-word-test--memory nil)
(defvar nelisp-weak-word-test--functions nil)
(defvar nelisp-weak-word-test--block-change 0)
(defvar nelisp-weak-word-test--boot-headers nil)
(defvar nelisp-weak-word-test--arena-child nil)

(defun nelisp-weak-word-test--mark-arena (owner)
  "Double arena traversal while exposing its actual child-mark transition."
  (let* ((header (- owner 8))
         (raw (gethash header nelisp-weak-word-test--memory)))
    (if (memq (logand raw 7) '(0 4))
        (progn
          (puthash header (logior (logand raw -8) 1) nelisp-weak-word-test--memory)
          (when nelisp-weak-word-test--arena-child
            (nelisp-weak-word-test--call 'nl_cons_mark_cell nelisp-weak-word-test--arena-child))
          1)
      0)))

(defun nelisp-weak-word-test--eval (form env)
  "Execute the small production liveness DSL against checked fixture memory."
  (cond
   ((numberp form) form)
   ((symbolp form) (or (cdr (assq form env)) (error "Unbound DSL name: %S" form)))
   (t
    (let ((op (car form)) (args (cdr form)))
      (pcase op
        ('seq (let ((result 0))
                (dolist (item args result)
                  (setq result (nelisp-weak-word-test--eval item env)))))
        ('if (nelisp-weak-word-test--eval
              (if (/= (nelisp-weak-word-test--eval (car args) env) 0)
                  (cadr args) (or (caddr args) 0)) env))
        ((or 'let 'let*)
         (let ((scope env))
           (dolist (binding (car args))
             (push (cons (car binding)
                         (nelisp-weak-word-test--eval
                          (cadr binding) (if (eq op 'let*) scope env))) scope))
           (nelisp-weak-word-test--eval (cadr args) scope)))
        ('and (if (cl-every (lambda (x) (/= (nelisp-weak-word-test--eval x env) 0)) args) 1 0))
        ('or (if (cl-some (lambda (x) (/= (nelisp-weak-word-test--eval x env) 0)) args) 1 0))
        ('extern-call (nelisp-weak-word-test--eval args env))
        ('data-addr (pcase (car args) ('nl_typed_alloc #x1000) ('nl_strings_state #x2000)))
        (_
         (let ((values (mapcar (lambda (x) (nelisp-weak-word-test--eval x env)) args)))
           (cond
            ((eq op 'ptr-write-u64)
             (puthash (+ (nth 0 values) (nth 1 values)) (nth 2 values)
                      nelisp-weak-word-test--memory)
             0)
            ((memq op '(heap-read-u64 heap-read-u8 ptr-read-u64))
             (let* ((address (apply #'+ values))
                    (value (gethash address nelisp-weak-word-test--memory 'absent)))
               (if (eq value 'absent) (error "Unmapped read: %x" address) value)))
            ((eq op 'nl_typed_find) (pcase (logand (car values) -32768) (#x200000 1) (#x210000 2) (_ 0)))
            ((eq op 'nl_gc_in_arena) (if (and (>= (car values) #x400000) (< (car values) #x410000)) 1 0))
            ((eq op 'nl_gc_is_boot) (if (memq (car values) nelisp-weak-word-test--boot-headers) 1 0))
            ((eq op 'nl_boot_start_p)
             (if (and (>= (car values) #x300000) (< (car values) #x301000)) 1 0))
            ((eq op 'nl_gc_mark_block) nelisp-weak-word-test--block-change)
            ((eq op 'nl_gc_typed_add) 0)
            ((eq op 'nl_string_nbytes)
             (gethash (+ (car values) 8) nelisp-weak-word-test--memory))
            ((eq op 'nl_gc_mark_slot)
             ;; Only the entry dispatch is doubled. The cons/string mark
             ;; writes below execute their extracted production bodies.
             (pcase (logand (car values) 7)
               (0 0)
               (2 (let ((owner (- (car values) 2)))
                    (if (and (>= owner #x400000) (< owner #x410000))
                        (nelisp-weak-word-test--mark-arena owner)
                      (nelisp-weak-word-test--call 'nl_cons_mark_cell owner))))
               (4 (nelisp-weak-word-test--call 'nl_gc_mark_string_word (car values)))
               (6 (nelisp-weak-word-test--mark-arena (- (car values) 6)))
               (_ (error "Unsupported mark fixture: %S" values))))
            ((eq op 'nl_hdr_mark) (logand (gethash (car values) nelisp-weak-word-test--memory) 7))
            ((memq op '(= /= < > <= >=)) (if (apply op values) 1 0))
            ((eq op 'sar) (ash (car values) (- (cadr values))))
            ((eq op 'shl) (ash (car values) (cadr values)))
            ((memq op '(+ - * / logand logior)) (apply op values))
            (t
             (let ((definition (assq op nelisp-weak-word-test--functions)))
               (unless definition (error "Missing production helper: %S" op))
               (nelisp-weak-word-test--eval
                (caddr definition) (cl-pairlis (cadr definition) values))))))))))))

(defun nelisp-weak-word-test--fixture ()
  (setq nelisp-weak-word-test--memory (make-hash-table :test 'eql)
        nelisp-weak-word-test--block-change 0
        nelisp-weak-word-test--boot-headers nil
        nelisp-weak-word-test--arena-child nil
        nelisp-weak-word-test--functions nil)
  (dolist (source (list nelisp-standalone--gc-source nelisp-standalone--arena-source))
    (dolist (form (cdr source))
      (when (and (consp form) (eq (car form) 'defun))
        (push (cdr form) nelisp-weak-word-test--functions))))
  (dolist (pair `((,(+ #x1000 96) . #x200000) (,(+ #x1000 104) . #x220000)
                 (,(+ #x1000 200) . #x300000) (,(+ #x1000 208) . #x301000)
                 (,(+ #x1000 216) . #x1800)
                 (,(+ #x2000 160) . #x600000) (,(+ #x2000 168) . #x601000)
                 (#x1800 . 0) (#x210030 . 16) (#x400000 . 40)
                 (#x500000 . 5) (#x500008 . 5) (#x500010 . 0) (#x600000 . 5)))
    (puthash (car pair) (cdr pair) nelisp-weak-word-test--memory))
  (puthash (+ #x200000 nelisp-standalone--cons-marks-offset) 0 nelisp-weak-word-test--memory))

(defun nelisp-weak-word-test--call (name &rest args)
  (nelisp-weak-word-test--eval (cons name args) nil))

(ert-deftest nelisp-weak-word-compact-string ()
  (nelisp-weak-word-test--fixture)
  (should (= 0 (nelisp-weak-word-test--call 'nl_gc_weak_survives #x500004)))
  (puthash #x500000 (+ (- (ash 1 63)) 5) nelisp-weak-word-test--memory)
  (should (= 1 (nelisp-weak-word-test--call 'nl_gc_weak_survives #x500004))))

(ert-deftest nelisp-weak-word-fixed-point-change ()
  (dolist (word '(#x200022 #x500004))
    (nelisp-weak-word-test--fixture)
    (should (= 0 (nelisp-weak-word-test--call 'nl_gc_weak_survives word)))
    (should (= 1 (nelisp-weak-word-test--call 'nl_gc_weak_mark_word word)))
    (should (= 1 (nelisp-weak-word-test--call 'nl_gc_weak_survives word)))
    (should (= 0 (nelisp-weak-word-test--call 'nl_gc_weak_mark_word word)))))

(ert-deftest nelisp-weak-word-legacy-view-keeps-block-change ()
  (nelisp-weak-word-test--fixture)
  (puthash #x400108 7 nelisp-weak-word-test--memory)
  (puthash #x400110 #x700000 nelisp-weak-word-test--memory)
  (should (= 1 (nelisp-weak-word-test--call 'nl_gc_weak_survives #x400108)))
  (dolist (changed '(1 2))
    (let ((nelisp-weak-word-test--block-change changed))
      (should (= 1 (nelisp-weak-word-test--call 'nl_gc_weak_mark_word #x400108))))))

(ert-deftest nelisp-weak-word-live-untraced-arena-revisits-earlier-endpoint ()
  (dolist (mark '(4 0))
    (dolist (word '(#x40020a #x40020e))
      (nelisp-weak-word-test--fixture)
      (puthash #x400200 (+ 40 mark) nelisp-weak-word-test--memory)
      (puthash #x400208 8 nelisp-weak-word-test--memory)
      (when (= mark 0) (setq nelisp-weak-word-test--boot-headers '(#x400200)))
      (setq nelisp-weak-word-test--arena-child #x200020)
      ;; An earlier table's key is initially dead. A later table retains
      ;; this lifetime-live endpoint whose precise traversal marks that key.
      (should (= 0 (nelisp-weak-word-test--call 'nl_gc_weak_survives #x200022)))
      (should (= 1 (nelisp-weak-word-test--call 'nl_gc_weak_survives word)))
      (let ((changed (nelisp-weak-word-test--call 'nl_gc_weak_mark_word word)))
        (should (= 1 changed))
        (should (= 1 (nelisp-weak-word-test--call 'nl_gc_weak_survives #x200022)))
        ;; weak_finish revisits earlier tables only on changed == 1.
        (should (= 1 (if (= changed 1)
                         (nelisp-weak-word-test--call 'nl_gc_weak_keep 1
                          (nelisp-weak-word-test--call 'nl_gc_weak_survives #x200022) 0)
                       0))))
      (should (= 0 (nelisp-weak-word-test--call 'nl_gc_weak_mark_word word))))))

(ert-deftest nelisp-weak-word-retention-and-missing-mark-controls ()
  (dolist (constant '(0 1))
    (nelisp-weak-word-test--fixture)
    (setf (alist-get 'nl_gc_weak_survives nelisp-weak-word-test--functions)
          `((word) ,constant))
    (should-error
     (progn
       (should (= 0 (nelisp-weak-word-test--call 'nl_gc_weak_survives #x200022)))
       (puthash (+ #x200000 nelisp-standalone--cons-marks-offset) 4
                nelisp-weak-word-test--memory)
       (should (= 1 (nelisp-weak-word-test--call 'nl_gc_weak_survives #x200022))))
     :type 'ert-test-failed)))

(ert-deftest nelisp-weak-word-missing-fixed-point-change-control ()
  (nelisp-weak-word-test--fixture)
  (setf (alist-get 'nl_gc_weak_mark_word nelisp-weak-word-test--functions)
        '((word) 0))
  (should-error
   (should (= 1 (nelisp-weak-word-test--call 'nl_gc_weak_mark_word #x200022)))
   :type 'ert-test-failed))

(ert-deftest nelisp-weak-word-direct-cons-and-string ()
  (nelisp-weak-word-test--fixture)
  (dolist (row `((#x200022 ,(+ #x200000 nelisp-standalone--cons-marks-offset) 4)
                 (#x300022 #x1800 16) (#x500004 #x500000 ,(+ (- (ash 1 63)) 5))
                 (#x210036 #x210030 ,(+ 16 (ash 1 24)))))
    (let ((word (nth 0 row)) (address (nth 1 row)) (marked (nth 2 row)))
      (should (= 0 (nelisp-weak-word-test--call 'nl_gc_weak_survives word)))
      (let ((old (gethash address nelisp-weak-word-test--memory)))
        (puthash address marked nelisp-weak-word-test--memory)
        (should (= 1 (nelisp-weak-word-test--call 'nl_gc_weak_survives word)))
        (puthash address old nelisp-weak-word-test--memory))))
  ;; Permanent string-image and foreign owners retain their existing lifetime.
  (should (= 1 (nelisp-weak-word-test--call 'nl_gc_weak_survives #x600004)))
  (should (= 1 (nelisp-weak-word-test--call 'nl_gc_weak_owner_live #x700000)))
  (dolist (mark '(0 1 2 4))
    (puthash #x400000 (+ 40 mark) nelisp-weak-word-test--memory)
    (should (= (if (memq mark '(1 4)) 1 0)
               (nelisp-weak-word-test--call 'nl_gc_weak_owner_live #x400008)))))

(ert-deftest nelisp-weak-word-all-weakness-modes ()
  (nelisp-weak-word-test--fixture)
  ;; Root states: neither, value only, key only, both. These are the GNU
  ;; weak semantics, independent of the candidate implementation.
  (dolist (row '((1 0 0 1 1) (2 0 1 0 1) (3 0 1 1 1) (4 0 0 0 1)))
    (dotimes (state 4)
      (should (= (nth (1+ state) row)
                 (nelisp-weak-word-test--call
                  'nl_gc_weak_keep (car row) (/ state 2) (% state 2)))))))

(provide 'nelisp-standalone-weak-word-test)
;;; nelisp-standalone-weak-word-test.el ends here
