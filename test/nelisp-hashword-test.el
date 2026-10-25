;;; nelisp-hashword-test.el --- Hash WORD reader allocation checks -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
;; require, not load: other suites require the same file, and a second
;; load redefines its tests.
(let ((load-path (cons (file-name-directory load-file-name) load-path)))
  (require 'nelisp-hashfast-test))

(defvar hw--memory)
(defvar hw--lengths)
(defvar hw--next)
(defvar hw--allocations)

(defun hw--host (form)
  "Adapt the production DSL, including extern calls, to host Elisp."
  (cond ((atom form) form)
        ((eq (car form) 'extern-call) (hw--host (cdr form)))
        ((memq (car form) '(if while))
         (cons (car form) (cons `(hf--truth ,(hw--host (cadr form)))
                                (mapcar #'hw--host (cddr form)))))
        (t (cons (pcase (car form) ('seq 'progn) ('sar 'hf--sar)
                        (_ (car form)))
                 (mapcar #'hw--host (cdr form))))))

(defun hw--install ()
  "Install the exact generator readers and the real view materializers."
  (dolist (name '(nl_val_tag nl_word_tag64 nl_word_payload
                 nl_cons_car_word nl_cons_cdr_word nl_vector_slot_word
                 nl_ht_data_slot nl_ht_meta_slot nl_ht_alist_slot
                 nl_ht_marker_word nl_ht_marker_word_set nl_ht_int nl_ht_set_int
                 nl_ht_meta_count nl_ht_meta_set_count nl_ht_test_mode
                 nl_ht_custom_function_w nl_ht_identity_sync nl_ht_find_index
                 nl_ht_find_in nl_ht_entry_value nl_ht_get_default_w
                 nl_ht_get_synced_w nl_ht_get_w nl_ht_put_values_synced_w
                 nl_ht_put_values_w nl_ht_put_w nl_ht_remove_index
                 nl_ht_rem_synced_w nl_ht_rem_w))
    (eval (hw--host (hf--definition name)) t))
  (dolist (pair '((nl_cons_car_ptr . "lisp/nelisp-cc-jit-cons-car-ptr.el")
                  (nl_cons_cdr_ptr . "lisp/nelisp-cc-jit-cons-cdr-ptr.el")))
    (eval (hw--host (hf--definition (car pair)
                                   (expand-file-name (cdr pair) hf--root))) t)))

(defun hw--read (p offset)
  (or (gethash (+ p offset) hw--memory) (error "Unmapped read %s+%s" p offset)))
(defun hw--write (p offset value)
  (puthash (+ p offset) value hw--memory))
(defun hw--vector (values)
  (let ((base hw--next))
    (setq hw--next (+ base 512))
    (hw--write base 0 8)
    (puthash base (length values) hw--lengths)
    (cl-loop for v in values for i from 0 do (hw--write base (+ 8 (* i 8)) v))
    (+ base 6)))
(defun hw--view (word)
  (let ((base hw--next))
    (setq hw--next (+ base 32))
    (hw--write base 0 (nl_val_tag word))
    (hw--write base 8 (nl_word_payload word))
    base))
(defun hw--table (mode legacy)
  "A synchronized native-layout table with one key, nil custom descriptor."
  (let* ((pairs (hw--vector '(69 93)))
         (hashes (hw--vector '(1)))
         (next (hw--vector '(-3)))
         (index (hw--vector '(1)))
         (meta (hw--vector (list 3 5 3 5 3 (+ (* (- -101 mode) 4) 1)
                                29 1 3 3 1 pairs hashes -3 next)))
         (base hw--next))
    (setq hw--next (+ base 32))
    (nl_ht_set_int meta 7 (nl_word_payload meta))
    (hw--write base 0 meta)
    (hw--write base 8 index)
    (if legacy
        (let ((view hw--next))
          (setq hw--next (+ view 32))
          (hw--write view 0 7) (hw--write view 8 base) view)
      (+ base 2))))

(defmacro hw--with-runtime (&rest body)
  "Model raw memory only; execute production accessors and hit-path logic."
  (declare (indent 0))
  `(let ((hw--memory (make-hash-table :test 'eql))
         (hw--lengths (make-hash-table :test 'eql))
         (hw--next 4096) (hw--allocations 0))
     (hw--install)
     (hw--write 268436376 0 7)
     (cl-letf (((symbol-function 'heap-read-u64) #'hw--read)
               ((symbol-function 'ptr-read-u64) #'hw--read)
               ((symbol-function 'heap-read-u8) #'hw--read)
               ((symbol-function 'ptr-write-u64) #'hw--write)
               ((symbol-function 'heap-sexp-tag-word) #'nl_val_tag)
               ((symbol-function 'heap-sexp-payload) #'nl_word_payload)
               ((symbol-function 'vector-len)
                (lambda (v) (gethash (nl_word_payload v) hw--lengths)))
               ((symbol-function 'wf_arg_word) (lambda (args i) (nth i args)))
               ((symbol-function 'bf_eq2) (lambda (a b) (if (= a b) 1 0)))
               ((symbol-function 'nl_ht_hash) (lambda (_key _mode) 0))
               ((symbol-function 'nl_ht_hash_index) (lambda (_hash _n) 0))
               ((symbol-function 'nl_ht_key_match) (lambda (a b _mode) (if (= a b) 1 0)))
               ((symbol-function 'nl_val_clone_into) (lambda (w p) (hw--write p 0 w)))
               ((symbol-function 'alloc-bytes)
                (lambda (n _align)
                  (cl-incf hw--allocations)
                  (prog1 hw--next (cl-incf hw--next n))))
               ((symbol-function 'nl_sexp_clone_into)
                (lambda (w p)
                  (hw--write p 0 (nl_val_tag w))
                  (hw--write p 8 (nl_word_payload w)) p))
               ;; These paths must not be reached by synchronized hits.
               ((symbol-function 'nl_ht_reindex) (lambda (&rest _) (error "Unexpected reindex")))
               ((symbol-function 'nl_ht_hash_put) (lambda (&rest _) (error "Unexpected insert")))
               ((symbol-function 'nl_apply_function_w) (lambda (&rest _) (error "Unexpected custom call"))))
       ,@body)))

(ert-deftest hw-adapter-and-view-allocation-control ()
  (should (= (eval (hw--host '(if 0 1 2)) t) 2))
  (hw--with-runtime
    (let* ((table (hw--table 1 nil)) (word (nl_cons_car_word table)))
      (should (= hw--allocations 0))
      (should (= (nl_val_tag word) 8))
      (should (= (nl_val_tag (nl_cons_car_ptr table)) 8))
      (should (= hw--allocations 1)))))

(ert-deftest hw-gethash-hits-allocate-zero ()
  (hw--with-runtime
    (dolist (legacy '(nil t))
      (dolist (mode '(0 1 2))
        (let* ((table (hw--table mode legacy)) (args (list 69 table))
               (before hw--allocations))
          (dotimes (_ 100) (should (= (nl_ht_get_w args 0) 93)))
          (message "HASHWORD gethash legacy=%s mode=%s allocations=%s" legacy mode
                   (- hw--allocations before))
          (should (= (- hw--allocations before) 0)))))))

(ert-deftest hw-puthash-existing-allocates-zero ()
  (hw--with-runtime
    (dolist (legacy '(nil t))
      (dolist (mode '(0 1 2))
        (let* ((table (hw--table mode legacy)) (args (list 69 97 table))
               (before hw--allocations))
          (dotimes (_ 100) (should (= (nl_ht_put_w args 0) 97)))
          (message "HASHWORD puthash legacy=%s mode=%s allocations=%s" legacy mode
                   (- hw--allocations before))
          (should (= (- hw--allocations before) 0))
          (should (= (nl_ht_get_w (list 69 table) 0) 97)))))))

(ert-deftest hw-remhash-hits-allocate-zero ()
  (hw--with-runtime
    (dolist (legacy '(nil t))
      (dolist (mode '(0 1 2))
        (let ((tables (cl-loop repeat 100 collect (hw--table mode legacy)))
              (before hw--allocations))
          (dolist (table tables)
            (should (= (nl_ht_rem_w (list 69 table) 0) 3))
            (should (= (nl_ht_meta_count table) 0)))
          (message "HASHWORD remhash legacy=%s mode=%s allocations=%s" legacy mode
                   (- hw--allocations before))
          (should (= (- hw--allocations before) 0)))))))

(ert-deftest hw-readers-accept-word-and-legacy-containers ()
  (hw--with-runtime
    (dolist (legacy '(nil t))
      (let* ((table (hw--table 1 legacy))
             (meta (nl_ht_meta_slot table)) (data (nl_ht_data_slot table)))
        (should (= (nl_val_tag meta) 8))
        (should (= (nl_val_tag data) 8))
        (should (= (nl_ht_find_in table 69) 1))
        (should (= (nl_ht_entry_value table 1) 93))
        ;; Exercise custom descriptor reads with WORDs.
        (let ((descriptor (hw--vector '(3 3 101 105 109))))
          (nl_ht_marker_word_set meta 8 descriptor)
          (should (= (nl_ht_custom_function_w table 2) 101))
          (should (= (nl_ht_custom_function_w table 3) 105))
          (should (= (nl_ht_custom_function_w table 4) 109)))
        (should (= hw--allocations 0))))))

(provide 'nelisp-hashword-test)
;;; nelisp-hashword-test.el ends here
