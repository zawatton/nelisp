;;; nelisp-parity9-test.el --- Small GNU object parity regressions -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'nelisp-fxvm-coverage-test)

(defvar p9--next 100000)
(defvar p9--error nil)
(defvar p9--defs nil)
(defvar p9--equal-def nil)
(defvar nelisp-standalone--target)
(defvar nl_empty_vector 'nl_empty_vector)

(defun p9--definitions ()
  "Extract the production DSL once per process, including a seed override."
  (or p9--defs
      (setq p9--defs
            (let ((root (or (getenv "PARITY9_SOURCE") fxvm--root)))
              (append
               (nelisp-fx-vector--extract
                (expand-file-name "scripts/nelisp-standalone-build.el" root)
                '(nl_w_vector_make bf_make_vector_w bf_vector_w bf_vec_count bf_vec_fill
                  nl_eic_prim_make_vector nl_directcall_alloc_run bf_equal2 bf_equal_words
                  bf_aset_checked_w))
               (nelisp-fx-vector--extract
                (expand-file-name "lisp/nelisp-cc-nlvector-alloc.el" root)
                '(nl_alloc_vector nl_alloc_vector_fill)))))))

(defun p9--tag (w)
  (cond ((= w 3) 0) ((= w 7) 1) ((= (logand w 3) 1) 2)
        ((= (logand w 7) 2) 7)
        ((= (logand w 7) 6) (logand (fxvm--read (- w 6) 0) 63))
        (t (fxvm--read w 0))))

(defun p9--payload (w)
  (cond ((= (logand w 3) 1) (ash w -2))
        ((memq (logand w 7) '(2 6)) (- w (logand w 7)))
        (t (fxvm--read w 8))))

(defun p9--alloc (n &optional _align)
  (prog1 p9--next (setq p9--next (+ p9--next 64 (* 8 (ceiling n 8))))))

(defun p9--args (&rest words)
  (let ((head 3))
    (dolist (w (reverse words) head)
      (let ((cell (p9--alloc 16)))
        (fxvm--write cell 0 w) (fxvm--write cell 8 head)
        (setq head (+ cell 2))))))

(defun p9--arg (args i)
  (dotimes (_ i) (setq args (fxvm--read (- args 2) 8)))
  (fxvm--read (- args 2) 0))

(defun p9--record (type &rest slots)
  (let ((b (p9--alloc (+ 16 (* 8 (length slots))))))
    (fxvm--write b 0 (+ 12 (* 256 (length slots))))
    (fxvm--write b 8 type)
    (cl-loop for w in slots for i from 2 do (fxvm--write b (* i 8) w))
    (+ b 6)))

(defun p9--with-runtime (thunk)
  (let ((fxvm--memory (make-hash-table :test 'eql)) (p9--next 100000) (p9--error nil))
    (dolist (d (p9--definitions)) (eval (fxvm--lower d) t))
    (cl-letf (((symbol-function 'data-addr) (lambda (_name) 40960))
              ((symbol-function 'alloc-bytes) #'p9--alloc)
              ((symbol-function 'heap-sexp-tag-word) #'p9--tag)
              ((symbol-function 'nl_word_tag64) #'p9--tag)
              ((symbol-function 'nl_word_payload) #'p9--payload)
              ((symbol-function 'heap-sexp-payload) #'p9--payload)
              ((symbol-function 'nl_cons_car_word) (lambda (w) (fxvm--read (- w 2) 0)))
              ((symbol-function 'nl_cons_cdr_ptr) (lambda (w) (fxvm--read (- w 2) 8)))
              ((symbol-function 'wf_arg_word) #'p9--arg)
              ((symbol-function 'wf_arg_ptr) #'p9--arg)
              ((symbol-function 'vector-make)
               (lambda (n out) (fxvm--write out 0 8) (fxvm--write out 8 (nl_alloc_vector n)) 0))
              ((symbol-function 'm5_string_tag_p) (lambda (_tag) 0))
              ((symbol-function 'bf_eq2) (lambda (a b) (if (= a b) 1 0)))
              ((symbol-function 'nl_record_length) (lambda (w) (ash (fxvm--read (p9--payload w) 0) -8)))
              ((symbol-function 'nl_record_type_word) (lambda (w) (fxvm--read (p9--payload w) 8)))
              ((symbol-function 'nl_record_slot_word) (lambda (w i) (fxvm--read (p9--payload w) (+ 16 (* 8 i)))))
              ((symbol-function 'nl_closure_shape_p) (lambda (_w) 0))
              ((symbol-function 'bf_wrong_type_arrayp) (lambda (w) (setq p9--error (list 'arrayp w)) 1))
              ((symbol-function 'nl_abi_fail) (lambda (_rc _pad) 0)))
      ;; DATA-ADDR takes a DSL symbol, not a Lisp variable.
      (let ((nl_empty_vector 'nl_empty_vector)) (funcall thunk)))))

(defun p9--with-equal-reference (thunk)
  "Run the production Lisp comparison with the native leaf's record contract."
  (unless p9--equal-def
    (setq p9--equal-def
          (car (nelisp-fx-vector--extract
                (expand-file-name "scripts/nelisp-stdlib-prelude.el"
                                  (or (getenv "PARITY9_SOURCE") fxvm--root))
                '(nelisp--equal-reference)))))
  (eval p9--equal-def t)
  (cl-letf (((symbol-function 'nelisp--native-eq) #'eq)
            ((symbol-function 'nelisp--native-equal)
             (lambda (a b) (if (recordp a) (eq a b) (equal a b))))
            ((symbol-function 'nelisp-marker-p) (lambda (_x) nil)))
    (funcall thunk)))

(ert-deftest p9-empty-vector-constructors-share-one-object ()
  (p9--with-runtime
   (lambda ()
     (let* ((a (bf_vector_w 3 0)) (b (bf_make_vector_w (p9--args 1 5) 0))
            (c (bf_make_vector_w (p9--args 1 4096) 0))
            (d (nl_eic_prim_make_vector 1 9)) (reader (nl_w_vector_make 0 0)))
       (should (= a b)) (should (= a c)) (should (= a d))
       (should (= (p9--payload a) (p9--payload reader)))
       (should (= (nl_directcall_alloc_run 112 50000 0 60000) 0))
       (should (= a (fxvm--read 60000 0)))
       (should (= (p9--tag a) 8))
       (should (= (fxvm--read (p9--payload a) 0) 8))))))

(ert-deftest p9-equal-record-compares-type-length-and-recursive-slots ()
  (p9--with-equal-reference
   (lambda ()
     (let* ((a (record 'p9 1 (record 'inner '(2)) [3 "a"]))
            (b (record 'p9 1 (record 'inner '(2)) [3 "a"])))
       (should (nelisp--equal-reference a b))
       (should-not (nelisp--equal-reference a (record 'p9 2 (record 'inner '(2)) [3 "a"])))
       (should-not (nelisp--equal-reference a (record 'other 1 (record 'inner '(2)) [3 "a"])))
       (should-not (nelisp--equal-reference a (record 'p9 1)))
       (should (nelisp--equal-reference (record 'p9) (record 'p9)))))))

(ert-deftest p9-aset-non-arrays-signal-before-any-write ()
  (p9--with-runtime
   (lambda ()
     (dolist (w (list 3 7 21 (p9--args 5 9)))
       (setq p9--error nil)
       (should (= (bf_aset_checked_w (p9--args w 1 1) 0) 0))
       (should (equal p9--error (list 'arrayp w)))))))

(ert-deftest p9-equal-record-self-cycle-retains-identity-shortcut ()
  (p9--with-equal-reference
   (lambda ()
     (let ((r (record 'p9 nil)))
       (aset r 1 r)
       (should (nelisp--equal-reference r r))))))

(ert-deftest p9-empty-vector-bss-is-aligned-and-disjoint-on-every-target ()
  (require 'nelisp-standalone-build)
  (dolist (target '(linux-x86_64 windows-x86_64 linux-aarch64 macos-aarch64 windows-aarch64))
    (let* ((nelisp-standalone--target target)
           (unit (nelisp-standalone--arena-base-slot-unit))
           (syms (plist-get unit :symbols))
           (empty (cl-find "nl_empty_vector" syms :key (lambda (s) (plist-get s :name)) :test #'equal))
           (weak (cl-find "nl_gc_weak_head" syms :key (lambda (s) (plist-get s :name)) :test #'equal))
           (strings (cl-find "nl_strings_state" syms :key (lambda (s) (plist-get s :name)) :test #'equal)))
      (should empty)
      (let ((off (plist-get empty :value)))
        (should (= (logand off 7) 0))
        (should (= off (+ 8 (plist-get weak :value))))
        (should (<= (+ off 8) (plist-get strings :value)))
        (should (<= (+ off 8) (cdr (assq 'bss (plist-get unit :sections)))))))))

(provide 'nelisp-parity9-test)
