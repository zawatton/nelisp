;;; nelisp-argframe-native-fixture.el --- Native transport allocation control -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'nelisp-standalone-build)

(defun nelisp-argframe-fixture--definition (name forms)
  "Return the actual production definition NAME from FORMS."
  (or (cl-find-if (lambda (form) (and (eq (car-safe form) 'defun)
                                    (eq (cadr form) name))) forms)
      (error "Missing production definition %S" name)))

(let* ((output (or (getenv "ARGFRAME_FIXTURE_OUTPUT")
                   (expand-file-name "../target/argframe/native-baseline"
                                     (file-name-directory load-file-name))))
       (state #x3a000000)
       (argv (+ state 4096))
       (conses (+ state 8192))
       (checks nil))
  (dolist (argc '(0 1 2 8 9))
    (push `(seq
            (ptr-write-u64 ,state 0 0)
            (ptr-write-u64 ,state 8 0)
            (ptr-write-u64 ,state 16 0)
            (let* ((result (nl_eval_arg_list_w ,argc ,state 0)))
              (seq
               (if (= (ptr-read-u64 ,state 0) ,argc) 0 (setq failed (+ failed 1)))
               (if (= (ptr-read-u64 ,state 8) ,(if (= argc 0) 0 1))
                   0 (setq failed (+ failed 1)))
               (if (= (ptr-read-u64 ,state 16) ,(if (= argc 0) 0 1))
                   0 (setq failed (+ failed 1)))
               (if ,(if (= argc 0) '(= result 3) '(> result 3))
                   0 (setq failed (+ failed 1)))))) checks))
  (let ((source
         `(seq
           ,(nelisp-argframe-fixture--definition
             'nl_eval_arg_list_w (cdr nelisp-cc-evalport-combiner-arglist--source))
           ,(nelisp-argframe-fixture--definition
             'm5_fx_slot_list nelisp-standalone--applyfn-m5-helpers)
           (defun m5_fx_arg_count (args _pad) args)
           (defun nl_root_mark (env) 0)
           (defun nl_root_frame (env n) ,argv)
           (defun nl_root_release (env mark)
             (ptr-write-u64 env 16 (+ (ptr-read-u64 env 16) 1)))
           (defun wf_bytecode_root_exhausted () 0)
           (defun m5_fx_eval_forms (tail env base n)
             (let ((i 0))
               (seq (ptr-write-u64 env 8 (+ (ptr-read-u64 env 8) 1))
                    (while (< i n)
                      (seq (ptr-write-u64 base (* i 8) (+ (* i 4) 1))
                           (setq i (+ i 1)))) 0)))
           (defun m5_fx_slot_word (slot _pad) (heap-read-u64 slot 0))
           (defun nl_word_canon (word) word)
           (defun nl_alloc_consbox ()
             (let ((n (ptr-read-u64 ,state 0)))
               (seq (ptr-write-u64 ,state 0 (+ n 1))
                    (+ ,conses (* n 16)))))
           (defun _start ()
             (let ((failed 0))
               (seq (syscall-direct 9 ,state 1048576 3 50 -1 0)
                    ,@(nreverse checks)
                    (ptr-write-u64 ,state 24 failed)
                    (syscall-direct 1 1 ,state 32 0 0 0)
                    (syscall-direct 60 failed 0 0 0 0 0)))))))
    (make-directory (file-name-directory output) t)
    (nelisp-link-units output
                       (list (nelisp-standalone--compile-to-unit "argframe-baseline.o" source))
                       "_start")
    (with-temp-file (concat output ".source.el") (prin1 source (current-buffer)))
    (princ (format "ARGFRAME-BASELINE-COMPILED cases=5 checks=20 output=%s\n" output))))
