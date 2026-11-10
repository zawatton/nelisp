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
  (let* ((legacy-arms
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
             ;; d409a23e6: a symbol's cell starts as the unbound marker, so an
             ;; unset key now answers the marker; this probe reports it as nil.
             (seq (nelisp_mirror_lookup_value (+ env 0) (wf_arg_ptr args 0) out)
                  (if (= (ptr-read-u64 out 0) 16)
                      (if (= (ptr-read-u64 out 8)
                             (ptr-read-u64 (nl_sym_unbound) 8))
                          (wf_write_nil out) 0)
                    0)
                  0))
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
            ;; d409a23e6 (Doc 226 step 5.3): an uninterned symbol is a direct
            ;; kind-16 symbol_block object.  Its identity is the backing
            ;; address (`heap-sexp-payload'; the caller-supplied identity of
            ;; the retired 32-byte layout is no longer stored), the name is the
            ;; GNU SYMBOL_NAME string read through bf_str_ptr/bf_str_len, and
            ;; the gcmarkbit is the cell's own header bit 24 (nl_symbol_mark_cell).
            ((:lit "symbol-test-id") .
             (wf_write_int out (heap-sexp-payload (wf_arg_ptr args 0))))
            ((:lit "symbol-test-mark") .
             (let* ((sym (wf_arg_ptr args 0))
                    (base (heap-sexp-payload sym))
                    (word (+ base 6)))
               (if (> (if (< base (heap-read-u64 (data-addr nl_typed_alloc) 96)) 0
                        (if (>= base (heap-read-u64 (data-addr nl_typed_alloc) 104)) 0
                          (nl_typed_find base))) 1)
                   ;; A symbol_block cell: clear the bit, run the precise
                   ;; marker, read the bit back, restore it.
                   (let* ((saved (heap-read-u64 base 0)))
                     (seq (ptr-write-u64 base 0 (logand saved (- 0 16777217)))
                          (nl_gc_mark_slot word)
                          (wf_write_int out (if (= (logand (heap-read-u64 base 0) 16777216) 0) 0 1))
                          (ptr-write-u64 base 0 saved)
                          0))
                 ;; Not a symbol_block cell: nothing to probe.  Answer -1 so a
                 ;; test cannot pass by accident (run with the marker removed,
                 ;; the image test read 0 from the cell branch, so restored
                 ;; image symbols live in symbol blocks too).
                 (seq (wf_write_int out -1) 0))))
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
         ;; bd25c1caa (2026-10-19) made every builtin arm a direct WORD
         ;; producer (`nelisp-standalone--applyfn-word-arm').  These probes
         ;; are still written against the retired (rc, OUT) ABI, so wrap each
         ;; one: run it against a zeroed private OUT box, then turn (rc, box)
         ;; into the result WORD with `nl_symbol_test_box_word'.
         (arms
          (mapcar
           (lambda (arm)
             (cons (car arm)
                   (cons :word
                         `(let* ((out (alloc-bytes 32 8)))
                            (seq (ptr-write-u64 out 0 0) (ptr-write-u64 out 8 0)
                                 (ptr-write-u64 out 16 0) (ptr-write-u64 out 24 0)
                                 (nl_symbol_test_box_word ,(cdr arm) out))))))
           legacy-arms))
         (nelisp-standalone--applyfn-bf-helpers
          (append nelisp-standalone--applyfn-bf-helpers
                  '((defun nl_symbol_test_box_word (rc out)
                      (if (/= rc 0) (nl_abi_fail rc 0)
                        (let* ((tag (ptr-read-u64 out 0)))
                          (if (= tag 0) 3
                            (if (= tag 1) 7
                              ;; A string result must be a String WORD, not
                              ;; the legacy box (`nl_sym_name_string').
                              (if (if (= tag 5) 1 (if (= tag 14) 1 (if (= tag 6) 1 (if (= tag 15) 1 0))))
                                  (nl_sym_name_string out)
                              (if (= tag 2)
                                  (wf_write_int_w 0 (ptr-read-u64 out 8))
                                ;; Doc 226 D9 (d409a23e6): a symbol view's
                                ;; payload is its object base; the symbol
                                ;; WORD is base + 6.
                                (if (= tag 16)
                                    (if (> (ptr-read-u64 out 8) 65535)
                                        (+ (ptr-read-u64 out 8) 6)
                                      out)
                                  out))))))))
                    (defun nl_symbol_test_intern_scan
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
         (nelisp-standalone--applyfn-bf-arms
          (append arms nelisp-standalone--applyfn-bf-arms))
         (nelisp-standalone--reader-builtins
          (append (mapcar (lambda (arm) (cadar arm)) arms)
                  nelisp-standalone--reader-builtins)))
    (nelisp-standalone-build-reader)))

;;; nelisp-native-symbol-build.el ends here
