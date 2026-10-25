;;; nelisp-directcall-id-test.el --- Constant-time descriptor hits -*- lexical-binding: t; -*-

(require 'nelisp-directcall-test)

(ert-deftest ndcid-hit-uses-one-indexed-descriptor-and-no-name-comparison ()
  (ndc--with-definitions
   (lambda (_)
     (let ((mem (make-hash-table :test #'eql)) (comparisons 0) (reads 0))
       (puthash 10000 20006 mem) (puthash 10008 10018 mem)
       (puthash 10016 20022 mem) (puthash 10024 3 mem)
       (cl-letf (((symbol-function 'ptr-read-u64) (lambda (p i) (gethash (+ p i) mem)))
                 ((symbol-function 'nl_val_tag) (lambda (w) (if (memq w '(20006 20022)) 4 7)))
                 ((symbol-function 'nl_sym_sid)
                  (lambda (w) (cl-incf reads)
                    (or (nelisp-standalone--sid (if (= w 20006) "builtin" "car")) 0)))
                 ((symbol-function 'sexp-name-eq)
                  (lambda (&rest _) (cl-incf comparisons) 1)))
         (dotimes (_ 2) (should (= (nl_directcall_id 10002 20022 1) 1)))
         (should (= comparisons 0))
         (should (= reads 4))
         ;; No function-cell identity authorizes changed cons contents.
         (puthash 10024 10002 mem)
         (should (= (nl_directcall_id 10002 20022 2) 0))
         (puthash 10024 3 mem) (puthash 10000 20022 mem)
         (should (= (nl_directcall_id 10002 20022 2) 0)))))))

(ert-deftest ndcid-hit-keeps-backtrace-with-direct-lazy-argv-registration ()
  (let ((f (ndc--find 'nl_directcall_invoke (nelisp-standalone--directcall-defuns))))
    (should (ndc--contains 'nl_n4_push_words f))
    (should (ndc--contains 'nl_n4_finish_w f))
    ;; dcproto: the result goes to the caller's rooted slot; no root frame of
    ;; our own, as GNU keeps the value in the caller's stack slot.
    (should-not (ndc--contains 'nl_root_frame f))
    (should-not (ndc--contains 'nl_root_mark f))
    (should-not (ndc--contains 'm5_fx_frame_lazy f))))

(ert-deftest ndcid-all-descriptors-use-authoritative-arities ()
  (ndc--with-definitions
   (lambda (_)
     (let ((mem (make-hash-table :test #'eql)) (name "car"))
       (puthash 10000 20006 mem) (puthash 10008 10018 mem)
       (puthash 10016 20022 mem) (puthash 10024 3 mem)
       (cl-letf (((symbol-function 'ptr-read-u64) (lambda (p i) (gethash (+ p i) mem)))
                 ((symbol-function 'nl_val_tag) (lambda (w) (if (memq w '(20006 20022)) 4 7)))
                 ((symbol-function 'nl_sym_sid)
                  (lambda (w) (or (nelisp-standalone--sid (if (= w 20006) "builtin" name)) 0)))
                 ((symbol-function 'sexp-name-eq) (lambda (_ s) (if (equal name s) 1 0))))
         (dolist (spec '(("car" 1) ("nth" 10) ("length" -16) ("aset" -15)
                         ("logand" 27) ("gethash" 101) ("puthash" 102)
                         ("remhash" 103) ("hash-table-count" 104)))
           (setq name (car spec))
           (let* ((arity (assoc name (nelisp-standalone--builtin-arities)))
                  (lo (nth 1 arity)) (hi (nth 2 arity)))
             (dolist (n (delete-dups (list -1 0 (1- lo) lo (1+ lo)
                                          (if (eq hi 'many) 255 hi)
                                          (if (eq hi 'many) 256 (1+ hi)))))
               (should (= (nl_directcall_id 10002 20022 n)
                          (if (and (>= n lo) (<= n 255)
                                   (or (eq hi 'many) (<= n hi))) (cadr spec) 0)))))))))))

(ert-deftest ndcid-actual-object-compiler-retains-readonly-descriptor-data ()
  ;; This focused path catches unsupported IR/data forms before a full build.
  (let* ((forms (nelisp-standalone--directcall-defuns))
         (blob (cl-find-if (lambda (f) (eq (car f) 'data-blob)) forms))
         (id (ndc--find 'nl_directcall_id forms))
         (unit (nelisp-standalone--compile-to-unit "dcid-id-test.o" `(seq ,blob ,id)))
         (data (cl-find-if (lambda (s) (eq (plist-get s :section) 'rodata))
                           (plist-get unit :symbols))))
    (should blob)
    (should data)
    (should (= (plist-get data :size) (* 4096 4)))))

(ert-deftest ndcid-debug-policy-declines-before-activation-and-roots ()
  (ndc--with-definitions
   (lambda (_)
     (cl-letf (((symbol-function 'nl_directcall_id) (lambda (&rest _) 1))
               ((symbol-function 'nl_root_mark) (lambda (&rest _) (error "Unexpected root mark")))
               ((symbol-function 'heap-read-u64) (lambda (&rest _) 3145728)))
       (should (= (nl_directcall_invoke 50002 20022 100 1 1000 200) 2))))))

(defun ndcid--cell (mem name-word)
  (puthash 10000 20006 mem) (puthash 10008 10018 mem)
  (puthash 10016 name-word mem) (puthash 10024 3 mem))

(ert-deftest ndcid-any-spelling-of-the-symbol-hits-by-static-id ()
  ;; dcproto: ORIGINAL may be the name's own word, the symbol OBJECT (low bits
  ;; 110) or another VIEW (low bits 000) of the same interned symbol; equal
  ;; static ids hit with no name comparison and no tag test on the hit path.
  (ndc--with-definitions
   (lambda (_)
     (let ((mem (make-hash-table :test #'eql)) (compared 0) (tags 0)
           (sids '((20006 . "builtin") (20022 . "car") (20024 . "car") (20030 . "car")
                   (20040 . "cdr"))))
       (ndcid--cell mem 20022)
       (cl-letf (((symbol-function 'ptr-read-u64) (lambda (p i) (gethash (+ p i) mem)))
                 ((symbol-function 'nl_val_tag) (lambda (_) (cl-incf tags) 4))
                 ((symbol-function 'nl_sym_sid)
                  (lambda (w) (let ((n (cdr (assq w sids)))) (if n (nelisp-standalone--sid n) 0))))
                 ((symbol-function 'sexp-name-eq) (lambda (&rest _) (cl-incf compared) 0)))
         (dolist (original '(20022 20024 20030 10002))
           (should (= (nl_directcall_id 10002 original 1) 1)))
         (should (= compared 0))
         (should (= tags 0))
         ;; Another symbol's view, an uninterned one and a fixnum do not hit
         ;; through the id; they take the cold name check, which declines here.
         (dolist (original '(20040 20050 9 3))
           (should (= (nl_directcall_id 10002 original 1) 0))))))))

(ert-deftest ndcid-name-word-is-read-through-only-as-a-symbol ()
  (ndc--with-definitions
   (lambda (_)
     (dolist (name '(0 3 5 10010 20044 4094))
       (let ((mem (make-hash-table :test #'eql)))
         (ndcid--cell mem name)
         (cl-letf (((symbol-function 'ptr-read-u64) (lambda (p i) (gethash (+ p i) mem)))
                   ((symbol-function 'nl_val_tag) (lambda (_) (ert-fail "tag read of a non-symbol name")))
                   ((symbol-function 'nl_sym_sid)
                    (lambda (w) (if (= w 20006) (nelisp-standalone--sid "builtin")
                                  (ert-fail "name word read through"))))
                   ((symbol-function 'sexp-name-eq) (lambda (&rest _) (ert-fail "name compared"))))
           (should (= (nl_directcall_id 10002 20022 1) 0))))))))

(ert-deftest ndcid-name-without-a-static-id-checks-only-the-id-less-names ()
  ;; `logand' has no static id: the cold check names it from the descriptor
  ;; and compares ORIGINAL with that one name, not with every directcall name.
  (ndc--with-definitions
   (lambda (_)
     (let ((mem (make-hash-table :test #'eql)) (seen nil))
       (ndcid--cell mem 20022)
       (cl-letf (((symbol-function 'ptr-read-u64) (lambda (p i) (gethash (+ p i) mem)))
                 ((symbol-function 'nl_val_tag) (lambda (_) 4))
                 ((symbol-function 'nl_sym_sid)
                  (lambda (w) (if (= w 20006) (nelisp-standalone--sid "builtin") 0)))
                 ((symbol-function 'sexp-name-eq)
                  (lambda (w s) (push s seen) (if (memq w '(20022 20030)) (if (equal s "logand") 1 0) 0))))
         (should (= (nl_directcall_id 10002 20022 2) 27))   ; ORIGINAL is the name's word
         (setq seen nil)
         (should (= (nl_directcall_id 10002 20030 2) 27))   ; another spelling of it
         ;; Only names that lack a static id are ever compared (`identity' is the
         ;; other one), never the dozen with ids.
         (should (equal (cl-remove "identity" seen :test #'equal) '("logand" "logand")))
         (should (= (nl_directcall_id 10002 20040 2) 0))    ; another symbol
         (should (cl-subsetp seen '("logand" "identity") :test #'equal)))))))

(provide 'nelisp-directcall-id-test)
