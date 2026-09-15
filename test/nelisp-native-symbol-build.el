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
             (seq (nelisp_mirror_lookup_value (+ env 0) (wf_arg_ptr args 0) out) 0))
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
            ((:lit "symbol-test-id") .
             (wf_write_int out (ptr-read-u64 (wf_arg_ptr args 0) 8)))
            ((:lit "symbol-test-mark") .
             (let* ((sym (wf_arg_ptr args 0))
                    (header (- (ptr-read-u64 sym 16) 8))
                    (saved (nl_hdr_mark header)))
               (seq (nl_hdr_set_mark header 0)
                    (nl_gc_mark_slot sym)
                    (wf_write_int out (nl_hdr_mark header))
                    (nl_hdr_set_mark header saved)
                    0)))
            ((:lit "symbol-test-name") .
             (let* ((sym (wf_arg_ptr args 0)))
               (seq (nl_alloc_str (ptr-read-u64 sym 16) (ptr-read-u64 sym 24) out) 0)))
            ((:lit "symbol-test-name-equal") .
             (if (= (str-eq (wf_arg_ptr args 0) (wf_arg_ptr args 1)) 1)
                 (wf_write_t out) (wf_write_nil out)))
            ((:lit "symbol-test-same") .
             (if (= (nl_uninterned_symbol_equal
                     (wf_arg_ptr args 0) (wf_arg_ptr args 1)) 1)
                 (wf_write_t out) (wf_write_nil out)))))
         (nelisp-standalone--applyfn-bf-helpers
          (append nelisp-standalone--applyfn-bf-helpers
                  (cdr nelisp-cc-jit-make-symbol--source)))
         (nelisp-standalone--applyfn-bf-arms
          (append arms nelisp-standalone--applyfn-bf-arms))
         (nelisp-standalone--reader-builtins
          (append (mapcar (lambda (arm) (cadar arm)) arms)
                  nelisp-standalone--reader-builtins)))
    (nelisp-standalone-build-reader)))

;;; nelisp-native-symbol-build.el ends here
