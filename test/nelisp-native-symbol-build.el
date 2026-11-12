;;; nelisp-native-symbol-build.el --- Internal symbol representation fixture -*- lexical-binding: t; -*-

(require 'nelisp-standalone-build)
(require 'nelisp-cc-jit-make-symbol)

(defun nelisp-native-symbol-test-build ()
  "Build an isolated reader with private tag-16 probes."
  (unless (getenv "NELISP_STANDALONE_READER_OUTPUT")
    (error "Select an isolated NELISP_STANDALONE_READER_OUTPUT"))
  (when (equal (file-truename (nelisp-standalone--output-path t))
               (file-truename (expand-file-name "target/nelisp" nelisp-standalone--repo-root)))
    (error "Symbol fixture must not replace target/nelisp"))
  (let* ((arms
          '(((:lit "symbol-test-cell") . (seq (cell-make (wf_arg_ptr args 0) out) 0))
            ((:lit "symbol-test-jit-make") .
             (nl_jit_make_symbol (wf_arg_ptr args 0) out))
            ((:lit "symbol-test-restore-declarations") .
             (seq (extern-call nl_push_captured_walk (wf_arg_ptr args 1)
                               (+ env 0) (wf_arg_ptr args 0) (+ env 64))
                  (wf_write_nil out) 0))
            ((:lit "symbol-test-local-special") .
             (seq (wf_write_int out
                                (extern-call nelisp_frame_local_special_p
                                             (wf_arg_ptr args 0) (wf_arg_ptr args 1)))
                  0))
            ((:lit "symbol-test-boxed-string") .
             (let* ((arg (wf_arg_ptr args 0)) (n (bf_str_len arg)))
               (seq
                (if (= (sexp-tag arg) 14)
                    (extern-call nl_alloc_unibyte_mut_str n out)
                  (extern-call nl_alloc_mut_str n out))
                (nl_alloc_str_copy_loop (bf_str_ptr arg)
                                        (ptr-read-u64 (ptr-read-u64 out 8) 8) 0 n)
                (ptr-write-u64 (ptr-read-u64 out 8) 16 n)
                0)))
            ((:lit "symbol-test-jit-error") .
             (let* ((scratch (alloc-bytes 32 8)))
               (seq (wf_write_int scratch 123)
                    (if (= (nl_jit_make_symbol (wf_arg_ptr args 0) scratch) 1)
                        (if (and (= (sexp-tag scratch) 2)
                                 (= (ptr-read-u64 scratch 8) 123))
                            (wf_write_t out) (wf_write_nil out))
                      (wf_write_nil out)))))
            ((:lit "symbol-test-global-set") .
             (seq (nelisp_env_setv_mirror_lazy (+ env 0) (wf_arg_ptr args 0)
                                             (wf_arg_ptr args 1) (+ env 64))
                  (wf_write_nil out) 0))
            ((:lit "symbol-test-global-get") .
             ;; A symbol object owns an entry whose value cell holds the shared
             ;; unbound marker (Doc 226 D9); report that as nil, the answer
             ;; for a symbol with no value.
             (let* ((rc (nelisp_mirror_lookup_value (+ env 0) (wf_arg_ptr args 0) out)))
               (seq (if (= (sexp-tag out) 16)
                        (if (= (ptr-read-u64 out 8)
                               (ptr-read-u64 (nl_sym_unbound) 8))
                            (wf_write_nil out) 0)
                      0)
                    rc)))
            ((:lit "symbol-test-intern-scan") .
             (let* ((scratch (alloc-bytes 32 8)))
               (if (= (nl_symbol_test_intern_scan
                       0 (wf_arg_ptr args 0) (wf_arg_ptr args 1) scratch) 1)
                   (wf_write_t out) (wf_write_nil out))))
            ((:lit "symbol-test-intern-boundary") .
             (let* ((scratch (alloc-bytes 32 8)))
               (seq
                (wf_write_int scratch 123)
                (if (= (nl_intern_next 1048576 scratch) -1)
                    (if (and (= (sexp-tag scratch) 2)
                             (= (ptr-read-u64 scratch 8) 123))
                        (if (= (nl_intern_next -1 scratch) -1)
                            (if (and (= (sexp-tag scratch) 2)
                                     (= (ptr-read-u64 scratch 8) 123))
                                (wf_write_t out) (wf_write_nil out))
                          (wf_write_nil out))
                      (wf_write_nil out))
                  (wf_write_nil out)))))
            ((:lit "symbol-test-bind") .
             (let* ((scratch (alloc-bytes 96 8)))
               (wf_write_int out (nelisp_frame_bind
                 (wf_arg_ptr args 0) (wf_arg_ptr args 1) (wf_arg_ptr args 2)
                 scratch (+ scratch 32) (+ scratch 64)))))
            ((:lit "symbol-test-find") .
             (let* ((cell (nelisp_frame_stack_find_kind
                           (wf_arg_ptr args 0) (wf_arg_ptr args 1) 0 0)))
               (if (= cell 0) (wf_write_nil out) (nl_cell_get_value cell out))))
            ((:lit "symbol-test-capture") .
             (nl_capture_descend_native (wf_arg_ptr args 0) out))
            ((:lit "symbol-test-make") .
             (let* ((name (wf_arg_ptr args 0)))
               (seq (nl_alloc_uninterned_symbol
                     (bf_str_ptr name) (bf_str_len name) (wf_argval args 1) out) 0)))
            ;; Doc 226 D9: the identity of an uninterned symbol is its backing
            ;; address, carried at +8 of the 32-byte view.  Answer the low
            ;; 48 bits so the value is always a fixnum.
            ((:lit "symbol-test-id") .
             (wf_write_int out (logand (heap-sexp-payload (wf_arg_ptr args 0))
                                       281474976710655)))
            ;; The cell is a symbol_block cell; its gcmarkbit is header bit 24
            ;; (`nelisp-standalone--symbol-mark-bit').  Save and clear the bit,
            ;; run the actual precise marker on the view, read the bit, and
            ;; restore it.
            ((:lit "symbol-test-mark") .
             (let* ((sym (wf_arg_ptr args 0))
                    (cell (nl_live_symbol_holding (heap-sexp-payload sym)))
                    (saved (logand (heap-read-u64 cell 0) 16777216)))
               (seq (if (= saved 0) 0
                      (ptr-write-u64 cell 0 (logxor (heap-read-u64 cell 0) 16777216)))
                    (nl_gc_mark_slot sym)
                    (wf_write_int out (if (= (logand (heap-read-u64 cell 0) 16777216) 0) 0 1))
                    (if (= saved 0)
                        (ptr-write-u64 cell 0 (logand (heap-read-u64 cell 0) (- 0 16777217)))
                      (ptr-write-u64 cell 0 (logior (heap-read-u64 cell 0) 16777216)))
                    0)))
            ;; Doc 226 D9: the name is the symbol object's name string, read
            ;; through the same accessors the runtime uses (GNU SYMBOL_NAME).
            ((:lit "symbol-test-name") .
             (let* ((sym (wf_arg_ptr args 0)))
               (seq (nl_alloc_str (bf_str_ptr sym) (bf_str_len sym) out) 0)))
            ((:lit "symbol-test-name-equal") .
             (if (= (str-eq (wf_arg_ptr args 0) (wf_arg_ptr args 1)) 1)
                 (wf_write_t out) (wf_write_nil out)))
            ((:lit "symbol-test-same") .
             (if (= (nl_uninterned_symbol_equal
                     (wf_arg_ptr args 0) (wf_arg_ptr args 1)) 1)
                 (wf_write_t out) (wf_write_nil out)))))
         (nelisp-standalone--applyfn-bf-helpers
          (append nelisp-standalone--applyfn-bf-helpers
                  '((defun nl_symbol_test_intern_scan
                      (cursor a b scratch)
                      (let* ((next 0) (seen-a 0) (seen-b 0)
                             (done 0) (monotonic 1))
                        (while (= done 0)
                          (setq next (nl_intern_next cursor scratch))
                          (if (< next 0)
                              (setq done 1)
                            (seq
                             (if (<= next cursor) (setq monotonic 0) 0)
                             (if (= (symbol-eq scratch a) 1)
                                 (setq seen-a (+ seen-a 1)) 0)
                             (if (= (symbol-eq scratch b) 1)
                                 (setq seen-b (+ seen-b 1)) 0)
                             (setq cursor next))))
                        (if (and (= done 1) (= monotonic 1)
                                 (= seen-a 1) (= seen-b 1)) 1 0))))
                  (cdr nelisp-cc-jit-make-symbol--source)))
         ;; Reader dispatch arms are direct WORD producers (Doc 215).  Each
         ;; probe above writes its result through OUT and answers an rc, so
         ;; adapt it locally: the WORD is the filled box, and a nonzero rc
         ;; reports failure the way every other word arm does.
         (nelisp-standalone--applyfn-bf-arms
          (append (mapcar
                   (lambda (arm)
                     (cons (car arm)
                           `(:word let* ((out (alloc-bytes 32 8))
                                         (rc ,(cdr arm)))
                              (if (= rc 0) out (nl_abi_fail rc 0)))))
                   arms)
                  nelisp-standalone--applyfn-bf-arms))
         (nelisp-standalone--reader-builtins
          (append (mapcar (lambda (arm) (cadar arm)) arms)
                  nelisp-standalone--reader-builtins))
         ;; Every reader builtin needs an arity contract
         ;; (`nelisp-standalone--builtin-arities'), private probes included.
         ;; Each probe takes exactly the number of arguments its arm reads.
         (nelisp-standalone--builtin-arity-ranges
          (append
           (mapcar
            (lambda (spec)
              (cons (cons (cdr spec) (cdr spec)) (list (intern (car spec)))))
            '(("symbol-test-cell" . 1) ("symbol-test-jit-make" . 1)
              ("symbol-test-restore-declarations" . 2)
              ("symbol-test-local-special" . 2) ("symbol-test-boxed-string" . 1)
              ("symbol-test-jit-error" . 1) ("symbol-test-global-set" . 2)
              ("symbol-test-global-get" . 1) ("symbol-test-intern-scan" . 2)
              ("symbol-test-intern-boundary" . 0) ("symbol-test-bind" . 3)
              ("symbol-test-find" . 2) ("symbol-test-capture" . 1)
              ("symbol-test-make" . 2) ("symbol-test-id" . 1)
              ("symbol-test-mark" . 1) ("symbol-test-name" . 1)
              ("symbol-test-name-equal" . 2) ("symbol-test-same" . 2)))
           nelisp-standalone--builtin-arity-ranges)))
    (nelisp-standalone-build-reader)))

;;; nelisp-native-symbol-build.el ends here
