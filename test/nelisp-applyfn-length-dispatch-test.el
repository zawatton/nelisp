;;; nelisp-applyfn-length-dispatch-test.el --- applyfn bucket tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(load (expand-file-name "../scripts/nelisp-standalone-build.el"
                        (file-name-directory
                         (or load-file-name buffer-file-name)))
      nil 'nomessage)

(defconst nelisp-applyfn-length-test--table
  '(((:u8 "a") . (first-a))
    ((:lit "é") . (non-ascii))
    ((:lit "a") . (second-a))))

(ert-deftest nelisp-applyfn-length-dispatch-preserves-first-match-and-byte-length ()
  "Dispatch keeps the first match per name and counts UTF-8 bytes, not characters.
Since c552d6bbb the tree is keyed: a static symbol id when the name has one,
else a dense key (1000000 + bucket order) found through the byte-length
buckets.  The duplicate \"a\" arm maps to the first key, so `second-a' is
unreachable, and \"é\" is a two-byte name."
  (cl-letf (((symbol-function 'getenv) (lambda (_name) nil)))
    (should
     (equal
      (nelisp-standalone--applyfn-build-dispatch
       nelisp-applyfn-length-test--table 99)
      `(let* ((nl_ds ,(nelisp-standalone--sid-read-form 'name_ptr))
              (nl_dk
               (if (/= nl_ds 0) nl_ds
                 (let* ((nl_name_len
                         (if (= (sexp-tag name_ptr) 4) (nl_word_length name_ptr)
                           (if (= (sexp-tag name_ptr) 5) (heap-read-u64 name_ptr 24)
                             (if (= (sexp-tag name_ptr) 14) (heap-read-u64 name_ptr 24)
                               -1)))))
                   (if (= nl_name_len 1)
                       (if (= (sexp-name-eq name_ptr "a") 1) 1000000
                         (if (= (sexp-name-eq name_ptr "a") 1) 1000000 0))
                     (if (= nl_name_len 2)
                         (if (= (sexp-name-eq name_ptr "é") 1) 1000001 0)
                       0))))))
         (if (< nl_dk 1000001)
             (if (= nl_dk 1000000) (first-a) 99)
           (if (= nl_dk 1000001) (non-ascii) 99)))))))

(ert-deftest nelisp-applyfn-flat-toggle-retains-old-dispatch-shape ()
  "The environment toggle emits the old flat first-match chain for A/B builds."
  (cl-letf (((symbol-function 'getenv)
             (lambda (name) (and (equal name "NELISP_T104_FLAT_DISPATCH") "1"))))
    (should
     (equal
      (nelisp-standalone--applyfn-build-dispatch
       nelisp-applyfn-length-test--table 99)
      '(if (= (sexp-name-eq name_ptr "a") 1) (first-a)
         (if (= (sexp-name-eq name_ptr "é") 1) (non-ascii)
           (if (= (sexp-name-eq name_ptr "a") 1) (second-a) 99)))))))

(defconst nelisp-applyfn-length-test--word-table
  ;; c552d6bbb: assembly takes :word arms and refuses a table lacking the
  ;; canonical intrinsic WORD arms, so the table carries all of them.
  (append '(((:u8 "a") :word first-a)
            ((:lit "é") :word non-ascii)
            ((:lit "a") :word second-a))
          (mapcar (lambda (n) `((:lit ,n) :word wf_arg_word args 0))
                  '("+" "-" "<" "=" ">" "1+" "1-"))))

(ert-deftest nelisp-applyfn-length-dispatch-shares-reader-fallback-only ()
  "Reader assembly emits one fallback helper; baked assembly stays standalone."
  (cl-letf (((symbol-function 'getenv) (lambda (_name) nil)))
    (let* ((table nelisp-applyfn-length-test--word-table)
           (reader (nelisp-standalone--applyfn-assemble nil table))
           (baked (nelisp-standalone--applyfn-assemble nil table 1))
           (leaf-with (lambda (sym)
                        (cl-find-if (lambda (form)
                                      (and (eq (car-safe form) 'defun)
                                           (memq sym (flatten-tree form))))
                                    baked))))
      (should (= 1 (cl-count 'nl_applyfn_unsupported reader
                             :key (lambda (form) (and (consp form) (cadr form))))))
      (should-not (cl-find 'nl_applyfn_unsupported baked
                           :key (lambda (form) (and (consp form) (cadr form)))))
      ;; The legacy (rc, OUT) entry is an adapter over the WORD dispatcher.
      (should (equal (car (last baked))
                     '(defun nelisp_apply_function (func_ptr args env out)
                        (nl_abi_word_to_rc
                         (nelisp_apply_function_w func_ptr args env 0) out))))
      ;; First match wins: the duplicate "a" arm is never emitted.
      (should (funcall leaf-with 'first-a))
      (should (funcall leaf-with 'non-ascii))
      (should-not (funcall leaf-with 'second-a)))))

(ert-deftest nelisp-applyfn-bytecode-finalizes-after-the-instruction-loop ()
  "The byte-code VM finalizes once, after its instruction loop exits.
Keeping the finalizer in the `while' body makes the general VM report an
error immediately after its first otherwise-successful instruction.
The VM body is `wf_bytecode_execute_frame' (the public `wf_bytecode_w'
wraps it).  Its instruction loop is the outer `again' loop, whose body is a
`seq' that restarts the inner dispatch loop, and a single `done' dispatch
form follows it, so the finalizer runs once after the loop exits."
  (let* ((definition
          (cl-find-if
           (lambda (form)
             (and (eq (car-safe form) 'defun)
                  (eq (cadr form) 'wf_bytecode_execute_frame)))
           nelisp-standalone--applyfn-bytecode-helpers))
         (outer-let (nth 3 definition))
         (guard (nth 2 outer-let))
         (forms (cdr (nth 3 guard)))
         (loops (cl-remove-if-not (lambda (form) (eq (car-safe form) 'while)) forms))
         (instruction-loop (car (last loops)))
         (after (cdr (memq instruction-loop forms))))
    (should (eq (car-safe guard) 'if))
    (should (= (length loops) 2))
    (should (= (length instruction-loop) 3))
    (should (equal (nth 1 instruction-loop) '(= again 1)))
    (should (eq (car-safe (nth 2 instruction-loop)) 'seq))
    (should (= (length after) 1))
    (should (equal (cadr (car after)) '(= done 5)))))

(provide 'nelisp-applyfn-length-dispatch-test)
;;; nelisp-applyfn-length-dispatch-test.el ends here
