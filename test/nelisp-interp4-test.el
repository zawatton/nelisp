;;; nelisp-interp4-test.el --- Interpreter quote arm and the GNU Flet binder -*- lexical-binding: t; -*-

;; interp4: the production IR of `nl_eic_quote_w', `nl_eic_let_hybrid_ok' and
;; `nl_eic_let_bind_w' on checked host memory, as `nelisp-interp3-test.el' runs the
;; setq and let arms.  INTERP3_SOURCE_ROOT, a directory holding scripts/, runs the same
;; probes against another copy of the build script (the seed: they must fail).

(require 'ert)
(require 'cl-lib)
(load (expand-file-name "nelisp-interp3-test.el" (file-name-directory (or load-file-name buffer-file-name))) nil t)

(ert-deftest i4-quote-answers-the-car-of-a-one-element-list ()
  (i3--setup) (i3--install 'nl_eic_quote_w)
  (let (order)
    (cl-letf (((symbol-function 'nl_apply_special_w)
               (lambda (&rest _) (push 'special order) 5))
              ((symbol-function 'nl_eic_symbol_dispatch_w)
               (lambda (w &rest _) (push (list 'dispatch w) order) w)))
      ;; (quote 53): the direct list (53), whose cdr is nil.
      (i3--cons 40002 53 3)
      (should (= (nl_eic_quote_w 1 2 40002 0) 53))
      (should-not order)
      ;; A pointer word is returned as it is stored.
      (puthash 40000 24582 i3--memory)
      (should (= (nl_eic_quote_w 1 2 40002 0) 24582))
      ;; (quote 1 2) is GNU's wrong-number-of-arguments: the ordinary dispatch.
      (puthash 40008 40018 i3--memory)
      (should (= (nl_eic_quote_w 1 2 40002 0) 5))
      (should (equal (reverse order) '(special (dispatch 5))))
      (setq order nil)
      (puthash 40008 3 i3--memory)
      ;; A redefined `quote' and a tail that is no direct cons take it too.
      (puthash 112 (nelisp-standalone--special-gate-mask "quote") i3--memory)
      (should (= (nl_eic_quote_w 1 2 40002 0) 5))
      (puthash 112 0 i3--memory)
      (should (= (nl_eic_quote_w 1 2 3 0) 5))
      (should (= (nl_eic_quote_w 1 2 40000 0) 5))
      (should (equal (cl-remove-if-not #'symbolp (reverse order)) '(special special special))))))

(ert-deftest i4-quote-gate-mask-differs-from-setq-and-let ()
  (dolist (other '("setq" "let"))
    (should-not (= (nelisp-standalone--special-gate-mask "quote")
                   (nelisp-standalone--special-gate-mask other)))))

(ert-deftest i4-hybrid-ok-needs-lexical-mode-plain-symbol-and-clean-scope ()
  (i3--setup) (i3--install 'nl_eic_let_hybrid_ok)
  (let ((clean 1) (plain 1))
    (cl-letf (((symbol-function 'nl_eic_let_scope_clean) (lambda (_) clean))
              ((symbol-function 'nl_eic_let_plain_p) (lambda (_) plain)))
      (puthash 28672 4 i3--memory)                           ; header of the symbol object 28678
      (should (= (nl_eic_let_hybrid_ok 0 28678) 1))
      (puthash 28672 16 i3--memory)                          ; an uninterned symbol
      (should (= (nl_eic_let_hybrid_ok 0 28678) 1))
      (puthash 28672 12 i3--memory)                          ; a record, not a symbol
      (should (= (nl_eic_let_hybrid_ok 0 28678) 0))
      (puthash 28672 4 i3--memory)
      (should (= (nl_eic_let_hybrid_ok 0 28672) 0))          ; not a vectorlike word
      (setq plain 0)
      (should (= (nl_eic_let_hybrid_ok 0 28678) 0))
      (setq plain 1 clean 0)
      (should (= (nl_eic_let_hybrid_ok 0 28678) 0))
      (setq clean 1)
      (dolist (flags '(1048576 2097152))                     ; watchers, dynamic mode
        (puthash 112 flags i3--memory)
        (should (= (nl_eic_let_hybrid_ok 0 28678) 0)))
      (puthash 112 0 i3--memory)
      (should (= (nl_eic_let_hybrid_ok 0 28678) 1)))))

(ert-deftest i4-let-binds-with-the-function-parameter-binder ()
  (i3--setup) (i3--install 'nl_eic_let_bind_w)
  (i3--cons 50002 24582 50018) (i3--cons 50018 50034 3)
  (i3--cons 50034 24614 50050) (i3--cons 50050 99 3)
  (puthash 70000 5 i3--memory) (puthash 70008 70010 i3--memory)
  (let (events (pair-rc 1))
    (cl-letf (((symbol-function 'nl_eic_let_scope_clean) (lambda (_) 1))
              ((symbol-function 'nl_eic_let_plain_p) (lambda (_) 1))
              ((symbol-function 'nl_sexp_clone_into) (lambda (&rest _) (error "no legacy view may be made")))
              ((symbol-function 'nl_bind_frame_fast) (lambda (&rest _) (error "no Cell binder on the fast arm")))
              ((symbol-function 'nl_eic_bind_pair_w) (lambda (_env var w) (push (list 'pair var w) events) pair-rc))
              ((symbol-function 'nl_bind_frame_param) (lambda (frames name val) (push (list frames name val) events) 1))
              ((symbol-function 'nl_let_bind_one) (lambda (&rest _) (error "generic binder not expected"))))
      ;; The pair binder takes both variables: the parameter binder is not asked.
      (should (= (nl_eic_let_bind_w 50002 0 70000) 0))
      (should (equal (reverse events) '((pair 24582 5) (pair 24614 70010))))
      ;; Where it declines, the function parameter binder binds, with the words as they are.
      (setq events nil pair-rc 0)
      (should (= (nl_eic_let_bind_w 50002 0 70000) 0))
      (should (equal (reverse events) '((pair 24582 5) (32 24582 5) (pair 24614 70010) (32 24614 70010)))))))

(ert-deftest i4-dispatch-routes-quote-before-the-special-dispatch ()
  (let ((text (prin1-to-string (i3--definition 'nl_eval_inner_cons_w))))
    (should (string-match-p "nl_eic_quote_w" text))
    (should (< (string-match "nl_eic_quote_w" text) (string-match "nl_apply_special_w" text)))))

(ert-deftest i4-generic-let-binder-tries-the-parameter-binder-first ()
  ;; `nl_let_bind_one' is built by `nelisp-standalone--patch-n3'; its source is in the build script.
  (let* ((root (or (getenv "INTERP3_SOURCE_ROOT") default-directory))
         (file (expand-file-name "scripts/nelisp-standalone-build.el" root))
         (text (with-temp-buffer (insert-file-contents file) (buffer-string)))
         (start (string-match "(push '(defun nl_let_bind_one" text))
         (end (string-match "nl_let_bind_walk" text start))
         (body (substring text start end)))
    (should (string-match-p "nl_eic_let_hybrid_ok" body))
    (should (string-match-p "nl_bind_frame_param" body))
    (should (< (string-match "nl_eic_let_hybrid_ok" body) (string-match "nl_logic_bind_local" body)))))


(ert-deftest i4-if-evaluates-the-test-then-one-branch ()
  (i3--setup) (i3--install 'nl_sp_nargs 'nl_eic_if_w)
  (let (order (test-value 7))
    (cl-letf (((symbol-function 'nl_eic_eval_w)
               (lambda (form _env) (push (list 'eval form) order)
                 (if (eql form 1001) test-value (+ form 100))))
              ((symbol-function 'nl_val_tag) (lambda (w) (if (eql w 3) 0 5)))
              ((symbol-function 'nl_sf_progn_w)
               (lambda (args _env &rest _) (push (list 'progn args) order) 55))
              ((symbol-function 'nl_apply_special_w)
               (lambda (&rest _) (push 'special order) 5))
              ((symbol-function 'nl_eic_symbol_dispatch_w)
               (lambda (w &rest _) (push (list 'dispatch w) order) w)))
      ;; (if 1001 1002 1004): cons words 40002, 40018, 40034.
      (i3--cons 40002 1001 40018) (i3--cons 40018 1002 40034) (i3--cons 40034 1004 3)
      (should (= (nl_eic_if_w 1 2 40002 0) 1102))            ; true: only the then-form is evaluated
      (should (equal (reverse order) '((eval 1001) (eval 1002))))
      (setq order nil test-value 3)                          ; nil: the progn of the else forms
      (should (= (nl_eic_if_w 1 2 40002 0) 55))
      (should (equal (reverse order) '((eval 1001) (progn 40034))))
      (setq order nil test-value 0)                          ; a signal in the test stops everything
      (should (= (nl_eic_if_w 1 2 40002 0) 0))
      (should (equal (reverse order) '((eval 1001))))
      ;; (if 1001 1002): the else list is nil, still a progn.
      (setq order nil test-value 3)
      (puthash 40024 3 i3--memory)
      (should (= (nl_eic_if_w 1 2 40002 0) 55))
      (should (equal (reverse order) '((eval 1001) (progn 3)))))))

(ert-deftest i4-if-other-shapes-take-the-ordinary-dispatch-before-evaluating ()
  (i3--setup) (i3--install 'nl_sp_nargs 'nl_eic_if_w)
  (let (order)
    (cl-letf (((symbol-function 'nl_eic_eval_w)
               (lambda (&rest _) (push 'eval order) 7))
              ((symbol-function 'nl_val_tag) (lambda (w) (if (eql w 3) 0 5)))
              ((symbol-function 'nl_sf_progn_w) (lambda (&rest _) (push 'progn order) 55))
              ((symbol-function 'nl_apply_special_w) (lambda (&rest _) (push 'special order) 5))
              ((symbol-function 'nl_eic_symbol_dispatch_w) (lambda (w &rest _) w)))
      (i3--cons 40002 1001 40018) (i3--cons 40018 1002 40034) (i3--cons 40034 1004 3)
      (puthash 112 (nelisp-standalone--special-gate-mask "if") i3--memory)   ; redefined `if'
      (should (= (nl_eic_if_w 1 2 40002 0) 5))
      (puthash 112 0 i3--memory)
      (puthash 40008 3 i3--memory)                           ; (if 1001 1002 ...) cut to (if 1001)
      (should (= (nl_eic_if_w 1 2 40002 0) 5))
      (puthash 40008 40018 i3--memory)
      (puthash 40040 9 i3--memory)                           ; a dotted tail (if 1001 1002 1004 . 9)
      (should (= (nl_eic_if_w 1 2 40002 0) 5))
      (should (= (nl_eic_if_w 1 2 3 0) 5))                   ; no arguments
      (should (equal (reverse order) '(special special special special)))
      (should-not (memq 'eval order)))))

(ert-deftest i4-dispatch-routes-if-before-the-special-dispatch ()
  (let ((text (prin1-to-string (i3--definition 'nl_eval_inner_cons_w))))
    (should (string-match-p "nl_eic_if_w" text))
    (should (< (string-match "nl_eic_if_w" text) (string-match "nl_apply_special_w" text)))))


(ert-deftest i4-setq-store-writes-a-hybrid-binding-in-its-pair ()
  (i3--setup) (i3--install 'nl_eic_setq_store_w)
  (cl-letf (((symbol-function 'nl_env_set_value)
             (lambda (&rest _) (error "The general setter must not run on a hybrid hit")))
            ((symbol-function 'nl_abi_fail) (lambda (&rest _) 0)))
    ;; The first pair of the frame is (24582 . VALUE) at 20480; make VALUE a fixnum word.
    (puthash 20488 21 i3--memory)
    (puthash 40000 24582 i3--memory)
    (should (= (nl_eic_setq_store_w 40002 0 45) 45))
    (should (= (i3--read 20488 0) 45))                       ; stored in the pair
    (should (= (i3--read 268435544 0) 8))                    ; epoch bumped
    ;; A direct cons value is plain too.
    (puthash 20488 60002 i3--memory)
    (should (= (nl_eic_setq_store_w 40002 0 61) 61))
    (should (= (i3--read 20488 0) 61))
    ;; The Cell of the second pair is still written through its Cell.
    (puthash 40000 24614 i3--memory)
    (should (= (nl_eic_setq_store_w 40002 0 77) 77))
    (should (= (i3--read 28704 8) 77))))

(ert-deftest i4-setq-store-hybrid-with-a-view-value-uses-the-setter ()
  (i3--setup) (i3--install 'nl_eic_setq_store_w)
  (let (calls)
    (cl-letf (((symbol-function 'nl_env_set_value)
               (lambda (env sym w) (push (list env sym w) calls) 0))
              ((symbol-function 'nl_abi_fail) (lambda (&rest _) 0)))
      (puthash 20488 21 i3--memory)
      (puthash 40000 24582 i3--memory)
      (should (= (nl_eic_setq_store_w 40002 0 8) 8))         ; a legacy view as the new value
      (should (equal calls '((0 24582 8))))
      (should (= (i3--read 20488 0) 21))
      (setq calls nil)
      (puthash 20488 8 i3--memory)                           ; a legacy view stored in the pair
      (should (= (nl_eic_setq_store_w 40002 0 45) 45))
      (should (equal calls '((0 24582 45)))))))


(defmacro i4--with-let-star-stubs (&rest body)
  `(let (events (ok 1) (eval-fail nil) (generic-ok 1) (pop-bad nil) (bind-pair 1))
     (cl-letf (((symbol-function 'nelisp_frame_push_direct) (lambda (&rest _) (push 'push events) 1))
               ((symbol-function 'nl_frame_push_sym0_ptr) (lambda () 0))
               ((symbol-function 'nl_frame_push_sym1_ptr) (lambda () 0))
               ((symbol-function 'nl_eic_eval_w)
                (lambda (form _env) (if (= (logand form 1) 1) form
                                      (push (list 'eval form) events) (if (eql form eval-fail) 0 (+ form 1000)))))
               ((symbol-function 'nl_eic_bind_pair_w) (lambda (_env var w) (push (list 'pair var w) events) bind-pair))
               ((symbol-function 'nl_eic_let_hybrid_ok) (lambda (_env var) (if (eql var ok) 0 1)))
               ((symbol-function 'nl_bind_frame_param) (lambda (_f var w) (push (list 'hybrid var w) events) 1))
               ((symbol-function 'nl_let_bind_one) (lambda (_env var w) (push (list 'generic var w) events) generic-ok))
               ((symbol-function 'nl_eic_body_w) (lambda (args &rest _) (push (list 'body args) events) 99))
               ((symbol-function 'nl_env_pop_frame) (lambda (_env) (push 'pop events) (if pop-bad -1 0)))
               ((symbol-function 'nl_abi_fail) (lambda (&rest _) (push 'fail events) 0)))
       ,@body)))

(defun i4--let-star-forms ()
  ;; (let* (a (b 1004) c) BODY): the body list at 40018, bindings 50002.. as for the let tests.
  (i3--symbol 24582) (i3--symbol 24614) (i3--symbol 24622)
  (i3--cons 40002 50002 40018) (i3--cons 40018 1500 3)
  (i3--cons 50002 24582 50018) (i3--cons 50018 60002 50034) (i3--cons 50034 24622 3)
  (i3--cons 60002 24614 60018) (i3--cons 60018 1004 3))

(ert-deftest i4-let-star-pushes-evaluates-binds-in-order-then-pops-all ()
  (i3--setup) (i3--install 'nl_eic_let_star_run_w 'nl_eic_let_star_pop_w 'nl_eic_let_star_finish_w)
  (i4--let-star-forms)
  (i4--with-let-star-stubs
   (should (= (nl_eic_let_star_run_w 40002 0) 99))
   (should (equal (reverse events)
                  '(push (pair 24582 3) push (eval 1004) (pair 24614 2004) push (pair 24622 3)
                    (body 40018) pop pop pop)))))

(ert-deftest i4-let-star-generic-binder-for-variables-the-fast-path-refuses ()
  (i3--setup) (i3--install 'nl_eic_let_star_run_w 'nl_eic_let_star_pop_w 'nl_eic_let_star_finish_w)
  (i4--let-star-forms)
  (i4--with-let-star-stubs
   (setq ok 24614)
   (should (= (nl_eic_let_star_run_w 40002 0) 99))
   (should (member '(generic 24614 2004) events))
   (should-not (member '(hybrid 24614 2004) events))))

(ert-deftest i4-let-star-failures-pop-every-frame-pushed ()
  (i3--setup) (i3--install 'nl_eic_let_star_run_w 'nl_eic_let_star_pop_w 'nl_eic_let_star_finish_w)
  (i4--let-star-forms)
  ;; A signal in the value of the second binding: two frames were pushed, both popped, no body.
  (i4--with-let-star-stubs
   (setq eval-fail 1004)
   (should (= (nl_eic_let_star_run_w 40002 0) 0))
   (should (equal (reverse events) '(push (pair 24582 3) push (eval 1004) pop pop fail))))
  ;; The generic binder failing on the first binding.
  (i4--with-let-star-stubs
   (setq ok 24582 generic-ok 0)
   (should (= (nl_eic_let_star_run_w 40002 0) 0))
   (should (equal (reverse events) '(push (generic 24582 3) pop fail))))
  ;; A pop that fails fails the form, after all pops ran.
  (i4--with-let-star-stubs
   (setq pop-bad t)
   (should (= (nl_eic_let_star_run_w 40002 0) 0))
   (should (= (cl-count 'pop events) 3))
   (should (eq (car events) 'fail))))

(ert-deftest i4-let-star-other-shapes-take-the-ordinary-dispatch ()
  (i3--setup) (i3--install 'nl_sp_nargs 'nl_eic_let_shape 'nl_eic_let_star_w)
  (i4--let-star-forms)
  (let (order)
    (cl-letf (((symbol-function 'nl_eic_let_star_run_w) (lambda (&rest _) (push 'run order) 77))
              ((symbol-function 'nl_apply_special_w) (lambda (&rest _) (push 'special order) 5))
              ((symbol-function 'nl_eic_symbol_dispatch_w) (lambda (w &rest _) w)))
      (should (= (nl_eic_let_star_w 1 2 40002 0) 77))
      (puthash 40000 3 i3--memory)                           ; (let* nil BODY): no binding, the generic frame
      (should (= (nl_eic_let_star_w 1 2 40002 0) 5))
      (puthash 40000 50002 i3--memory)
      (puthash 50040 9 i3--memory)                           ; a dotted binding list
      (should (= (nl_eic_let_star_w 1 2 40002 0) 5))
      (puthash 50040 3 i3--memory)
      (puthash 112 (nelisp-standalone--special-gate-mask "let*") i3--memory)
      (should (= (nl_eic_let_star_w 1 2 40002 0) 5))        ; a redefined `let*'
      (puthash 112 0 i3--memory)
      (should (= (nl_eic_let_star_w 1 2 3 0) 5))             ; no arguments
      (should (equal (reverse order) '(run special special special special))))))

(ert-deftest i4-dispatch-routes-let-star ()
  (let ((text (prin1-to-string (i3--definition 'nl_eval_inner_cons_w))))
    (should (string-match-p "nl_eic_let_star_w" text))
    (should (< (string-match "nl_eic_let_star_w" text) (string-match "nl_apply_special_w" text)))))


(defvar i4--slot-sets nil)
(defun i4--record-slot-set (frame idx val)
  (push (list frame idx val) i4--slot-sets)
  (puthash (+ (- frame 6) 16 (* idx 8)) val i3--memory) 0)

;; Memory of `i3--setup': env 0, frames record at 4096 with depth 5 (fixnum 1) and backing 8198; the
;; frame record word 12294 has header 780 and its alist at 12304.  Fresh frame: the alist is nil.
(defmacro i4--with-pair-binder (&rest body)
  `(progn
     (i3--setup) (i3--install 'nl_eic_bind_pair_w)
     (setq i4--slot-sets nil)
     (puthash 12304 3 i3--memory)
     (cl-letf (((symbol-function 'record-slot-set) #'i4--record-slot-set)
               ((symbol-function 'nelisp_cons_construct_w)
                (lambda (a d _) (let ((w (+ 800002 (* 16 (length i4--slot-sets)) (* 4096 (if (= d 3) 0 1)))))
                                  (i3--cons w a d) w))))
       ,@body)))

(ert-deftest i4-pair-binder-conses-the-pair-on-a-fresh-frame ()
  (i4--with-pair-binder
   (should (= (nl_eic_bind_pair_w 0 24582 45) 1))
   (should (= (length i4--slot-sets) 1))
   (let* ((set (car i4--slot-sets)) (alist (nth 2 set)))
     (should (= (nth 0 set) 12294))                          ; the newest frame
     (should (= (nth 1 set) 0))                              ; its alist slot
     (should (= (logand alist 7) 2))
     (should (= (i3--read (+ alist 6) 0) 3))                 ; ends in nil
     (let ((pair (i3--read (- alist 2) 0)))
       (should (= (i3--read (- pair 2) 0) 24582))
       (should (= (i3--read (+ pair 6) 0) 45))))))

(ert-deftest i4-pair-binder-declines-what-the-parameter-binder-decides ()
  (i4--with-pair-binder
   ;; A legacy view and a Cell as the value.
   (should (= (nl_eic_bind_pair_w 0 24582 8) 0))
   (puthash 28672 11 i3--memory)
   (should (= (nl_eic_bind_pair_w 0 24582 28678) 0))
   ;; A pointer word that is no Cell is bound.
   (puthash 28672 8 i3--memory)
   (should (= (nl_eic_bind_pair_w 0 24582 28678) 1))
   (setq i4--slot-sets nil)
   ;; A frame carrying the private flag bits of a shared closure chain.
   (puthash 12288 (logior 780 128) i3--memory)
   (should (= (nl_eic_bind_pair_w 0 24582 45) 0))
   (puthash 12288 780 i3--memory)
   ;; Not a record, no frames.
   (puthash 12288 77 i3--memory)
   (should (= (nl_eic_bind_pair_w 0 24582 45) 0))
   (puthash 12288 780 i3--memory)
   (puthash 4120 1 i3--memory)                               ; depth 0
   (should (= (nl_eic_bind_pair_w 0 24582 45) 0))
   (puthash 4120 5 i3--memory)
   ;; An alist that is not a proper list.
   (puthash 12304 77 i3--memory)
   (should (= (nl_eic_bind_pair_w 0 24582 45) 0))
   (should-not i4--slot-sets)))

(ert-deftest i4-pair-binder-declines-a-bound-name-and-a-full-frame ()
  (i4--with-pair-binder
   ;; The frame of `i3--setup' holds the pairs (24582 . V) and (24614 . V): the alist at 16386.
   (puthash 12304 16386 i3--memory)
   (should (= (nl_eic_bind_pair_w 0 24582 45) 0))           ; the name is bound already
   (should (= (nl_eic_bind_pair_w 0 24614 45) 0))
   (should-not i4--slot-sets)
   (should (= (nl_eic_bind_pair_w 0 24630 45) 1))           ; another name is not
   (should (= (length i4--slot-sets) 1))
   ;; Seven bindings are the most it takes on one frame; the eighth goes to the parameter binder.
   (let ((head 3))
     (dotimes (i 7)
       (let ((cell (+ 900002 (* 32 i))) (pair (+ 910002 (* 32 i))))
         (i3--cons pair (+ 30000 (* 8 i) 6) 21)
         (i3--cons cell pair head)
         (setq head cell)))
     (puthash 12304 head i3--memory)
     (setq i4--slot-sets nil)
     (should (= (nl_eic_bind_pair_w 0 24630 45) 0))
     (should-not i4--slot-sets))))

(ert-deftest i4-eval-helper-reads-symbols-and-immediates-in-line ()
  (i3--setup) (i3--install 'nl_eic_eval_w)
  (let (calls)
    (cl-letf (((symbol-function 'nelisp_eval_call_w) (lambda (f _e _) (push (list 'call f) calls) 77))
              ((symbol-function 'nl_env_lookup_val_w) (lambda (f _e _) (push (list 'lookup f) calls) (if (= f 24630) 2 99)))
              ((symbol-function 'nl_ei_var_kw_w) (lambda (w f _e _) (push (list 'kw w f) calls) 0)))
      (should (= (nl_eic_eval_w 45 0) 45))                  ; an immediate is itself
      (should-not calls)
      (puthash 28672 4 i3--memory)                          ; a symbol object
      (should (= (nl_eic_eval_w 28678 0) 99))               ; read in line
      (puthash 24576 4 i3--memory)
      (should (= (nl_eic_eval_w 24582 0) 99))
      (puthash 24608 16 i3--memory)                         ; an uninterned symbol
      (should (= (nl_eic_eval_w 24614 0) 99))
      (setq calls nil)
      (puthash 24624 4 i3--memory)
      (should (= (nl_eic_eval_w 24630 0) 0))                ; unbound: asked whether it is a keyword
      (should (equal (reverse calls) '((lookup 24630) (kw 2 24630))))
      (setq calls nil)
      (puthash 28672 12 i3--memory)                         ; a record: the evaluator
      (should (= (nl_eic_eval_w 28678 0) 77))
      (should (= (nl_eic_eval_w 40002 0) 77))               ; a cons form
      (should (= (nl_eic_eval_w 8 0) 77))                   ; a legacy view
      (should (equal (reverse calls) '((call 28678) (call 40002) (call 8)))))))

(ert-deftest i4-body-helper-evaluates-one-form-directly-and-longer-bodies-as-progn ()
  (i3--setup) (i3--install 'nl_eic_body_w)
  (let (log)
    (cl-letf (((symbol-function 'nl_eic_eval_w) (lambda (f _e) (push (list 'eval f) log) (+ f 1000)))
              ((symbol-function 'nl_val_tag) (lambda (w) (if (= (logand w 1) 1) 2 6)))
              ((symbol-function 'nl_sf_progn_w) (lambda (b &rest _) (push (list 'progn b) log) 55)))
      (i3--cons 40002 1004 3)                               ; one form
      (should (= (nl_eic_body_w 40002 0) 2004))
      (i3--cons 40002 21 3)                                 ; an immediate: its own value
      (should (= (nl_eic_body_w 40002 0) 21))
      (i3--cons 40002 1004 40018) (i3--cons 40018 1006 3)   ; two forms
      (should (= (nl_eic_body_w 40002 0) 55))
      (should (= (nl_eic_body_w 3 0) 55))                   ; no body: the progn answers nil
      (should (equal (reverse log) '((eval 1004) (progn 40002) (progn 3)))))))

(provide 'nelisp-interp4-test)

;;; nelisp-interp4-test.el ends here
