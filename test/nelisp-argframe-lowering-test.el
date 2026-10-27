;;; nelisp-argframe-lowering-test.el --- Structural argv admission -*- lexical-binding: t; -*-

(require 'ert)
(require 'nelisp-standalone-argframe)

(defmacro nelisp-argframe-test--plain (&rest body)
  "Run BODY with the list-walk lowering's build settings off (no helper
table, no fresh lists), whatever a loaded build script set."
  `(let ((nelisp-argframe-materialize-env nil) (nelisp-argframe-helper-defs nil)
         (nelisp-argframe-inline-helpers nelisp-argframe-inline-helpers))
     ,@body))

(ert-deftest nelisp-argframe-lowers-only-literal-index-reads ()
  (nelisp-argframe-test--plain
  (should
   (equal (nelisp-argframe-lower
           '(seq (wf_arg_word args 0) (wf_argval args 2)
                 (m5_list_len args 0)) 'argv 'argc)
          '(seq (if (< 0 argc) (ptr-read-u64 argv 0) 0)
                (nl_word_payload (if (< 2 argc) (ptr-read-u64 argv 16) 0))
                argc)))
  ;; subr1: `wf_arg_ptr' is `nl_cons_car_ptr' of the cell: the element WORD's
  ;; view-or-materialise test, 0 past the end.
  (should
   (equal (nelisp-argframe-lower '(wf_arg_ptr args 1) 'argv 'argc)
          '(m5_fx_word_ptr (if (< 1 argc) (ptr-read-u64 argv 8) 0))))))

(ert-deftest nelisp-argframe-rejects-escape-mutation-and-unknown-uses ()
  (nelisp-argframe-test--plain
  (dolist (form '((identity args) (nl_cons_cdr_word args)
                  (ptr-write-u64 args 0 3) (wf_arg_ptr args i)
                  (wf_arg_word args index) (wf_arg_word args -1)
                  (quote args) (m5_list_len args 1)))
    (should-error (nelisp-argframe-lower form 'argv 'argc)
                  :type 'nelisp-argframe-ineligible))))

(ert-deftest nelisp-argframe-does-not-confuse-omitted-and-explicit-nil ()
  (nelisp-argframe-test--plain
  (should (equal (nelisp-argframe-lower '(wf_arg_word args 8) 'argv 'argc)
                 '(if (< 8 argc) (ptr-read-u64 argv 64) 0)))))

(ert-deftest nelisp-argframe-preserves-arbitrary-code-without-args ()
  (nelisp-argframe-test--plain
  (let ((body '(let* ((v (callback env))) (seq (alloc-bytes 32 8) v))))
    (should (equal (nelisp-argframe-lower body 'argv 'argc) body)))))

(ert-deftest nelisp-argframe-mode-keeps-original-accessor-and-one-body ()
  (nelisp-argframe-test--plain
  (let* ((body '(seq (callback env) (wf_argval args 1)))
         (lowered (nelisp-argframe-lower body 'args '(- _pad 1) '_pad)))
    (should (equal lowered
                   '(seq (callback env)
                         (if (= _pad 0) (wf_argval args 1)
                           (nl_word_payload
                            (if (< 1 (- _pad 1)) (ptr-read-u64 args 8) 0))))))
    (should-error (nelisp-argframe-lower
                   '(let ((_pad 9)) (wf_arg_word args 0)) 'args '(- _pad 1) '_pad)
                  :type 'nelisp-argframe-ineligible))))

(ert-deftest nelisp-argframe-mode-rejects-before-ineligible-effects ()
  (nelisp-argframe-test--plain
  (let ((table '(((:lit "escape") :word seq (callback env) (escape args))
                 ((:lit "fixed") :word wf_arg_word args 0))))
    (should (equal (nelisp-argframe-admitted-names table) '("fixed")))
    (should (equal (car (nelisp-argframe-mode-table table))
                   '((:lit "escape") :word if (= _pad 0)
                     (seq (callback env) (escape args)) 2))))))


(ert-deftest nelisp-argframe-opens-indexed-helpers ()
  (nelisp-argframe-test--plain
  ;; callpath: `(HELPER args ...)' is lowered as HELPER's body, its other
  ;; parameters renamed and bound; a helper that escapes `args' stays ineligible.
  (let ((nelisp-argframe-inline-helpers (make-hash-table :test #'eq)))
    (puthash 'h1 '((args _pad) (f (wf_arg_word args 1) _pad)) nelisp-argframe-inline-helpers)
    (puthash 'h2 '((args) (g args)) nelisp-argframe-inline-helpers)
    (should (equal (nelisp-argframe-lower '(h1 args 0) 'argv 'argc)
                   '(let* ((nl_afi_h1_0 0))
                      (f (if (< 1 argc) (ptr-read-u64 argv 8) 0) nl_afi_h1_0))))
    (should (equal (nelisp-argframe-lower '(h1 args 0) 'argv 'argc 'mode)
                   '(if (= mode 0) (h1 args 0)
                      (let* ((nl_afi_h1_0 0))
                        (f (if (< 1 argc) (ptr-read-u64 argv 8) 0) nl_afi_h1_0)))))
    (should-error (nelisp-argframe-lower '(h2 args) 'argv 'argc)
                  :type 'nelisp-argframe-ineligible)
    ;; The list-only marker: the argv side declines with 2.
    (should (equal (nelisp-argframe-lower '(nl_argframe_list_only (h2 args)) 'argv 'argc) 2))
    (should (equal (nelisp-argframe-lower '(nl_argframe_list_only (h2 args)) 'argv 'argc 'mode)
                   '(if (= mode 0) (nl_argframe_list_only (h2 args)) 2))))))

(ert-deftest nelisp-argframe-walks-argument-list-cursors ()
  ;; wordroute: a walk over the argument list (GNU's loop over args[]) runs on
  ;; the array: cdr is the next index, the cons test is index < argc, the
  ;; length argc - index.  Red before wordroute (the list walk was ineligible).
  (nelisp-argframe-test--plain
   (should
    (equal (nelisp-argframe-lower
            '(let* ((rest (nl_cons_cdr_word args)) (n (nl_cons_car_word args)))
               (seq (while (= (nl_val_tag rest) 7)
                      (seq (setq n (+ n (nl_cons_car_word rest)))
                           (setq rest (nl_cons_cdr_word rest))))
                    (m5_list_len args 0) n))
            'argv 'argc)
           '(let* ((rest 1) (n (if (< 0 argc) (ptr-read-u64 argv 0) 0)))
              (seq (while (< rest argc)
                     (seq (setq n (+ n (if (< rest argc) (ptr-read-u64 argv (* rest 8)) 0)))
                          (setq rest (+ rest 1))))
                   argc n))))
   ;; A cursor `setq' whose value is discarded keeps its effect (build A2 of
   ;; wordroute replaced it by 0, so `format' printed its first argument twice).
   (should
    (equal (nelisp-argframe-lower
            '(let* ((p args) (s 0))
               (seq (while (= (nl_val_tag p) 7)
                      (seq (setq s (+ s (nl_cons_car_word p))) (setq p (nl_cons_cdr_word p)) (setq s (+ s 1))))
                    s))
            'argv 'argc)
           '(let* ((p 0) (s 0))
              (seq (while (< p argc)
                     (seq (setq s (+ s (if (< p argc) (ptr-read-u64 argv (* p 8)) 0)))
                          (setq p (+ p 1)) (setq s (+ s 1))))
                   s))))
   ;; A rebinding of the handler's own list name is a new variable: the indexed
   ;; pass rejects it, the walk renames it so the array reads stay intact.
   (let ((form '(let ((args 3)) (wf_arg_word args 0))))
     (should-error (nelisp-argframe--lower-indexed form 'argv 'argc)
                   :type 'nelisp-argframe-ineligible)
     (should (equal (nelisp-argframe-lower form 'args 'argc)
                    '(let ((nl_afw_a_1 3)) (wf_arg_word nl_afw_a_1 0)))))
   ;; The handler's own list is never assigned; a cursor returned as a value
   ;; needs the list.
   (dolist (form '((setq args (nl_cons_cdr_word args))
                   (let ((rest args)) (setq rest 5))
                   (let ((rest args)) rest)))
     (should-error (nelisp-argframe-lower form 'argv 'argc)
                   :type 'nelisp-argframe-ineligible))))

(ert-deftest nelisp-argframe-specializes-recursive-helpers ()
  ;; wordroute: a helper recursing on a list tail is generated once with the
  ;; cursor parameter an index and the array as two leading parameters.
  (nelisp-argframe-test--plain
   (let ((nelisp-argframe-helper-defs (make-hash-table :test #'eq))
         (nelisp-argframe-generated (cons nil (make-hash-table :test #'equal))))
     (puthash 'walk '((cur k) (if (= (nl_val_tag cur) 7) (walk (nl_cons_cdr_word cur) (+ k 1)) k))
              nelisp-argframe-helper-defs)
     (should (equal (nelisp-argframe-lower '(walk args 0) 'argv 'argc)
                    '(walk__af0 argv argc 0 0)))
     (should (equal (car nelisp-argframe-generated)
                    '((defun walk__af0 (nl_afb nl_afn cur k)
                        (if (< cur nl_afn) (walk__af0 nl_afb nl_afn (+ cur 1) (+ k 1)) k)))))
     ;; The table that has nowhere to put a definition declines instead.
     (let ((nelisp-argframe-generated :forbid))
       (should-error (nelisp-argframe-lower '(walk args 0) 'argv 'argc)
                     :type 'nelisp-argframe-ineligible)))))

(ert-deftest nelisp-argframe-roots-a-fresh-list-for-list-consumers ()
  ;; wordroute: code that needs the list itself (a Lisp callback) gets a fresh
  ;; list of the remaining words, rooted for the call; without an env to root
  ;; it in, the handler stays on the list route.
  (nelisp-argframe-test--plain
   (let ((form '(if (= (nl_val_tag args) 7) (callback (nl_cons_cdr_word args) env) 3)))
     (should-error (nelisp-argframe-lower form 'argv 'argc) :type 'nelisp-argframe-ineligible)
     (let ((nelisp-argframe-materialize-env 'env))
       (should
        (equal (nelisp-argframe-lower form 'argv 'argc)
               '(if (< 0 argc)
                    (let* ((nl_afw_m_1 (nl_root_mark env))
                           (nl_afw_l_3 (nl_argframe_fresh_list env argv argc 1))
                           (nl_afw_r_2 (callback nl_afw_l_3 env)))
                      (seq (nl_root_release env nl_afw_m_1) nl_afw_r_2))
                  3)))))))

(provide 'nelisp-argframe-lowering-test)
;;; nelisp-argframe-lowering-test.el ends here
