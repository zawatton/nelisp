;;; nelisp-interp3-test.el --- Interpreter setq and let fast paths -*- lexical-binding: t; -*-

;; interp3: the production IR of `nl_eic_setq_w', `nl_eic_setq_store_w' and the
;; `nl_eic_let_*' family run on checked host memory (a read of a word nothing
;; initialised is an error) with every callee that is not under test replaced by
;; a recorder.  INTERP3_SOURCE_ROOT, a directory holding scripts/, runs the same
;; probes against another copy of the build script (the seed: they must fail).

(require 'ert)
(require 'cl-lib)
(let ((root (getenv "INTERP3_SOURCE_ROOT")))
  (when root (add-to-list 'load-path (expand-file-name "scripts" root))))
(require 'nelisp-standalone-build)

(defvar i3--defs nil)
(defvar i3--memory nil)
(defvar i3--next 700000)
(defvar i3--log nil)

(defun i3--truth (v) (and v (not (eql v 0))))
(defun i3--shift (v n) (ash v (- n)))
(defun i3--read (p o)
  (or (gethash (+ p o) i3--memory) (error "Unexpected memory read %s+%s" p o)))
(defun i3--write (p o v) (puthash (+ p o) v i3--memory) 0)
(defun i3--alloc (n _a) (prog1 i3--next (setq i3--next (+ i3--next n))))

(defun i3--adapt (x)
  (cond
   ((atom x) x)
   ((memq (car x) '(if while))
    `(,(car x) (i3--truth ,(i3--adapt (cadr x))) ,@(mapcar #'i3--adapt (cddr x))))
   ((eq (car x) 'quote) x)
   (t (cons (pcase (car x)
              ('seq 'progn) ((or 'sar 'shr) 'i3--shift) ('shl 'ash)
              ((or 'ptr-read-u64 'heap-read-u64) 'i3--read)
              ('ptr-write-u64 'i3--write)
              ('alloc-bytes 'i3--alloc)
              (_ (i3--adapt (car x))))
            (mapcar #'i3--adapt (cdr x))))))

(defun i3--definition (name)
  (unless i3--defs (setq i3--defs (cons 'seq (nelisp-standalone--eval-call-defuns))))
  (or (cl-find name (cdr i3--defs) :key #'cadr :test #'eq)
      (error "Missing interpreter definition %s" name)))

(defun i3--install (&rest names)
  (dolist (name names) (eval (i3--adapt (i3--definition name)) t)))

;; One frame (env 0, frames record at 4096, backing at 8192, frame at 12288) with two
;; binding pairs whose keys are symbol words 24582 and 24614 and whose values are
;; Cells at 28678 and 28710.
(defun i3--setup ()
  (setq i3--memory (make-hash-table :test #'eql) i3--next 700000 i3--log nil)
  (dolist (kv '((32 . 12) (40 . 4096) (4112 . 8198) (4120 . 5) (112 . 0)
                (8192 . 8) (8200 . 12294) (12288 . 780) (12312 . 1)
                (12320 . 3) (12304 . 16386)
                (16384 . 20482) (16392 . 16418)
                (16416 . 20514) (16424 . 3)
                (20480 . 24582) (20488 . 28678)
                (20512 . 24614) (20520 . 28710)
                (28672 . 11) (28680 . 29) (28704 . 11) (28712 . 37)
                (268435544 . 7)))
    (puthash (car kv) (cdr kv) i3--memory)))

(defun i3--cons (w car cdr)
  "A direct cons with word W (W = 2 mod 8): car at W-2, cdr at W+6."
  (puthash (- w 2) car i3--memory) (puthash (+ w 6) cdr i3--memory) w)

(defun i3--tail (sym form)
  "The direct list (SYM FORM): cons words 40002 and 40018."
  (i3--cons 40002 sym 40018) (i3--cons 40018 form 3) 40002)

(defun i3--symbol (w) (puthash (- w 6) 4 i3--memory) w)

(ert-deftest i3-gate-mask-matches-the-special-form-dispatch ()
  ;; `nelisp-standalone--patch-n3-specials' numbers the redefinition bits by the position
  ;; among the host's special forms, plus 3, skipping bits 20 and 21 (watchers, dynamic).
  (let ((names (nelisp-standalone--host-special-form-names)))
    (dolist (name '("setq" "let"))
      (let ((index (+ 3 (cl-position name names :test #'equal))))
        (when (>= index 20) (setq index (+ index 2)))
        (should (= (nelisp-standalone--special-gate-mask name) (ash 1 index)))
        (should-not (memq index '(20 21)))))
    (should-not (= (nelisp-standalone--special-gate-mask "setq")
                   (nelisp-standalone--special-gate-mask "let")))))

(ert-deftest i3-setq-store-writes-a-plain-cell-in-line ()
  (i3--setup) (i3--install 'nl_eic_setq_store_w)
  (cl-letf (((symbol-function 'nl_env_set_value)
             (lambda (&rest _) (error "The general setter must not run on a hit")))
            ((symbol-function 'nl_abi_fail) (lambda (&rest _) 0)))
    (puthash 40000 24582 i3--memory)
    (should (= (nl_eic_setq_store_w 40002 0 45) 45))
    (should (= (i3--read 28672 8) 45))
    (should (= (i3--read 268435544 0) 8))
    (puthash 40000 24614 i3--memory)
    (should (= (nl_eic_setq_store_w 40002 0 61) 61))
    (should (= (i3--read 28704 8) 61))
    (should (= (i3--read 28672 8) 45))))

(ert-deftest i3-setq-store-general-cases-use-the-setter ()
  (i3--setup) (i3--install 'nl_eic_setq_store_w)
  (let (calls)
    (cl-letf (((symbol-function 'nl_env_set_value)
               (lambda (env sym w) (push (list env sym w) calls) 0))
              ((symbol-function 'nl_abi_fail) (lambda (&rest _) 0)))
      (puthash 40000 24630 i3--memory)                     ; not bound in the frame
      (should (= (nl_eic_setq_store_w 40002 0 45) 45))
      (should (equal calls '((0 24630 45))))
      (setq calls nil)
      (puthash 40000 24582 i3--memory)
      (puthash 112 1048576 i3--memory)                     ; watchers enabled
      (should (= (nl_eic_setq_store_w 40002 0 45) 45))
      (should (equal calls '((0 24582 45))))
      (should (= (i3--read 28680 0) 29))
      (setq calls nil)
      (puthash 112 0 i3--memory)
      (puthash 28672 267 i3--memory)                       ; pair-backed Cell
      (should (= (nl_eic_setq_store_w 40002 0 45) 45))
      (should (equal calls '((0 24582 45))))
      (setq calls nil)
      (puthash 28672 11 i3--memory)
      (should (= (nl_eic_setq_store_w 40002 0 8) 8))       ; a legacy view, low bits 0
      (should (equal calls '((0 24582 8))))
      (should (= (i3--read 28680 0) 29)))))

(ert-deftest i3-setq-store-failure-and-signalled-value ()
  (i3--setup) (i3--install 'nl_eic_setq_store_w)
  (cl-letf (((symbol-function 'nl_env_set_value) (lambda (&rest _) 1))
            ((symbol-function 'nl_abi_fail) (lambda (&rest _) 0)))
    (puthash 40000 24630 i3--memory)
    (should (= (nl_eic_setq_store_w 40002 0 45) 0))        ; the setter failed
    (should (= (nl_eic_setq_store_w 40002 0 0) 0))))       ; the value was a signal

(ert-deftest i3-setq-evaluates-then-stores-and-falls-back-before-evaluating ()
  (i3--setup) (i3--install 'nl_eic_setq_w)
  (let (order)
    (cl-letf (((symbol-function 'nelisp_eval_call_w)
               (lambda (form env _) (push (list 'eval form env) order) 77))
              ((symbol-function 'nl_eic_setq_store_w)
               (lambda (tail env w) (push (list 'store tail env w) order) w))
              ((symbol-function 'nl_apply_special_w)
               (lambda (&rest _) (push 'special order) 5))
              ((symbol-function 'nl_eic_symbol_dispatch_w)
               (lambda (w &rest _) (push (list 'dispatch w) order) w)))
      (let ((tail (i3--tail 24582 53)))
        (should (= (nl_eic_setq_w 1 2 tail 0) 77))
        (should (equal (reverse order) '((eval 53 0) (store 40002 0 77))))
        ;; A redefined `setq' (its gate bit set) takes the ordinary dispatch.
        (setq order nil)
        (puthash 112 (nelisp-standalone--special-gate-mask "setq") i3--memory)
        (should (= (nl_eic_setq_w 1 2 tail 0) 5))
        (should (equal (reverse order) '(special (dispatch 5))))
        (puthash 112 0 i3--memory)
        ;; Two pairs, a non-symbol variable and a dotted tail are not the plain shape.
        (setq order nil)
        (puthash 40024 40100 i3--memory)
        (should (= (nl_eic_setq_w 1 2 tail 0) 5))
        (puthash 40024 3 i3--memory)
        (puthash 40000 3 i3--memory)
        (should (= (nl_eic_setq_w 1 2 tail 0) 5))
        (puthash 40000 24582 i3--memory)
        (puthash 40008 9 i3--memory)
        (should (= (nl_eic_setq_w 1 2 tail 0) 5))
        (should (equal (cl-remove-if-not #'symbolp (reverse order)) '(special special special)))
        (should-not (cl-find 'eval order :key (lambda (x) (and (consp x) (car x)))))
        (should (= (nl_eic_setq_w 1 2 9 0) 5))))))

(ert-deftest i3-let-shape-accepts-only-plain-variable-lists ()
  (i3--setup) (i3--install 'nl_eic_let_shape)
  ;; (a (b F) c): cons words 50002, 50018, 50034; the pair at 60002, 60018.
  (i3--symbol 24582) (i3--symbol 24614) (i3--symbol 24622)
  (i3--cons 50002 24582 50018) (i3--cons 50018 60002 50034) (i3--cons 50034 24622 3)
  (i3--cons 60002 24614 60018) (i3--cons 60018 99 3)
  (should (= (nl_eic_let_shape 50002) 3))
  (should (= (nl_eic_let_shape 3) 0))
  (puthash 50040 7 i3--memory)                              ; improper list
  (should (= (nl_eic_let_shape 50002) -1))
  (puthash 50040 3 i3--memory)
  (puthash 60024 61000 i3--memory)                          ; (b F G)
  (should (= (nl_eic_let_shape 50002) -1))
  (puthash 60024 3 i3--memory)
  (puthash 60008 3 i3--memory)                              ; (b)
  (should (= (nl_eic_let_shape 50002) -1))
  (puthash 60008 60018 i3--memory)
  (puthash 60000 3 i3--memory)                              ; (nil F)
  (should (= (nl_eic_let_shape 50002) -1))
  (puthash 60000 24614 i3--memory)
  (puthash 24576 5 i3--memory)                              ; not a symbol object
  (should (= (nl_eic_let_shape 50002) -1)))

(ert-deftest i3-let-scope-walk-stops-at-declarations-and-boundaries ()
  (i3--setup) (i3--install 'nl_eic_let_scope_clean)
  (should (= (nl_eic_let_scope_clean 0) 1))                 ; three slots, none declared
  (puthash 12288 1036 i3--memory)                           ; four slots
  (puthash 12328 3 i3--memory)                              ; nil declarations
  (should (= (nl_eic_let_scope_clean 0) 1))
  (puthash 12328 16386 i3--memory)                          ; a declaration is present
  (should (= (nl_eic_let_scope_clean 0) 0))
  (puthash 12328 3 i3--memory)
  (puthash 12288 780 i3--memory)
  (puthash 12320 5 i3--memory)                              ; a scope boundary: stop
  (should (= (nl_eic_let_scope_clean 0) 1))
  (puthash 12288 77 i3--memory)                             ; not a frame record
  (should (= (nl_eic_let_scope_clean 0) 0))
  (puthash 12288 780 i3--memory)
  (puthash 4112 5 i3--memory)                               ; backing is no vector
  (should (= (nl_eic_let_scope_clean 0) 0)))

(ert-deftest i3-let-plain-variable-test-reads-the-header ()
  (i3--setup) (i3--install 'nl_eic_let_plain_p)
  (cl-letf (((symbol-function 'bf_keyword_raw) (lambda (_) 0)))
    (puthash 28672 4 i3--memory)
    (should (= (nl_eic_let_plain_p 28678) 1))
    (dolist (bit '(8 9 10 11 14))                           ; redirect, trapped_write, declared_special
      (puthash 28672 (logior 4 (ash 1 bit)) i3--memory)
      (should (= (nl_eic_let_plain_p 28678) 0))))
  (puthash 28672 4 i3--memory)
  (cl-letf (((symbol-function 'bf_keyword_raw) (lambda (_) 1)))
    (should (= (nl_eic_let_plain_p 28678) 0))))

(ert-deftest i3-let-binds-plain-variables-directly-and-others-generically ()
  (i3--setup) (i3--install 'nl_eic_let_bind_w)
  ;; bindings (a (b F)): a symbol object 24582 and the pair (24614 F); values in a
  ;; frame at 70000: 11 (immediate) and 70010 (a pointer word).
  (i3--cons 50002 24582 50018) (i3--cons 50018 50034 3)
  (i3--cons 50034 24614 50050) (i3--cons 50050 99 3)
  (puthash 70000 5 i3--memory) (puthash 70008 70010 i3--memory)
  (let (events (clean 1) (plain 1) (kw nil))
    (cl-letf (((symbol-function 'nl_eic_let_scope_clean) (lambda (_) clean))
              ((symbol-function 'nl_eic_let_plain_p) (lambda (v) (if (eq plain t) 1 (if (eql v plain) 1 0))))
              ((symbol-function 'nl_sexp_clone_into) (lambda (w to) (push (list 'clone w to) events) 0))
              ((symbol-function 'nl_bind_frame_fast) (lambda (frames name val) (push (list 'fast frames name val) events) 1))
              ((symbol-function 'nl_let_bind_one) (lambda (env var w) (push (list 'generic env var w) events) 1)))
      (setq plain t)
      (should (= (nl_eic_let_bind_w 50002 0 70000) 0))
      (setq events (reverse events))
      ;; The immediate value is passed as it is; the pointer word is cloned into a view.
      (should (equal (cl-remove-if-not (lambda (e) (eq (car e) 'fast)) events)
                     `((fast 32 700000 5) (fast 32 700064 ,(+ 700064 32)))))
      (should-not (cl-find 'generic events :key #'car))
      ;; A frame with local declarations, or a special variable, takes the generic binder.
      (setq events nil clean 0)
      (should (= (nl_eic_let_bind_w 50002 0 70000) 0))
      (should (equal (reverse events) '((generic 0 24582 5) (generic 0 24614 70010))))
      (setq events nil clean 1 plain 24582)
      (should (= (nl_eic_let_bind_w 50002 0 70000) 0))
      (should (equal (cl-remove-if-not (lambda (e) (eq (car e) 'generic)) events)
                     '((generic 0 24614 70010))))
      ;; Watchers and the dynamic mode take the generic binder for every variable.
      (setq events nil plain t)
      (puthash 112 2097152 i3--memory)
      (should (= (nl_eic_let_bind_w 50002 0 70000) 0))
      (should (= (length (cl-remove-if-not (lambda (e) (eq (car e) 'generic)) events)) 2))
      ;; A failing generic binder stops the loop and answers 1.
      (setq events nil)
      (cl-letf (((symbol-function 'nl_let_bind_one) (lambda (&rest a) (push (cons 'generic a) events) 0)))
        (should (= (nl_eic_let_bind_w 50002 0 70000) 1))
        (should (= (length events) 1))))))

(ert-deftest i3-let-values-are-published-in-order-and-stop-at-a-signal ()
  (i3--setup) (i3--install 'nl_eic_let_values_w)
  ;; ((a) (b 7) (c F) (d G)): a symbol alone is nil, an immediate form is its own value,
  ;; any other form is evaluated; the value is stored before the next form is evaluated.
  (i3--cons 50002 24582 50018) (i3--cons 50018 50034 50050) (i3--cons 50050 50066 50082)
  (i3--cons 50082 50098 3)
  (i3--cons 50034 24614 50114) (i3--cons 50114 15 3)          ; (b 7): 15 is the fixnum 3
  (i3--cons 50066 24622 50130) (i3--cons 50130 200 3)         ; (c F), F = 200
  (i3--cons 50098 24630 50146) (i3--cons 50146 300 3)         ; (d G), G = 300
  (let (events (fail nil))
    (cl-letf (((symbol-function 'nelisp_eval_call_w)
               (lambda (form env _)
                 (push (list 'eval form env (gethash 70008 i3--memory) (gethash 70016 i3--memory)) events)
                 (if (and fail (= form fail)) 0 (+ form 1)))))
      (should (= (nl_eic_let_values_w 50002 0 70000) 0))
      (should (equal (reverse events) '((eval 200 0 15 nil) (eval 300 0 15 201))))
      (should (equal (list (gethash 70000 i3--memory) (gethash 70008 i3--memory)
                           (gethash 70016 i3--memory) (gethash 70024 i3--memory))
                     '(3 15 201 301)))
      ;; A signalled value stops the loop: the later forms are not evaluated.
      (setq events nil fail 200)
      (should (= (nl_eic_let_values_w 50002 0 70000) 1))
      (should (equal (reverse events) '((eval 200 0 15 201)))))))

(ert-deftest i3-let-run-sequence-matches-the-generic-let ()
  (i3--setup) (i3--install 'nl_eic_let_run_w 'nl_eic_let_finish_w)
  (i3--cons 40002 49000 41000)                              ; tail: (BINDINGS . BODY)
  (let (log (bind-rc 0) (eval-rc 0) (progn-w 55) (pop-rc 0))
    (cl-letf (((symbol-function 'nl_root_mark) (lambda (_) (push 'mark log) 9))
              ((symbol-function 'nl_root_frame) (lambda (_ n) (push (list 'frame n) log) 70000))
              ((symbol-function 'nl_eic_let_values_w) (lambda (&rest _) (push 'eval log) eval-rc))
              ((symbol-function 'nelisp_frame_push_direct) (lambda (&rest _) (push 'push log) 1))
              ((symbol-function 'nl_frame_push_sym0_ptr) (lambda () 0))
              ((symbol-function 'nl_frame_push_sym1_ptr) (lambda () 0))
              ((symbol-function 'nl_eic_let_bind_w) (lambda (&rest _) (push 'bind log) bind-rc))
              ((symbol-function 'nl_root_release) (lambda (_ m) (push (list 'release m) log) 0))
              ((symbol-function 'nl_cons_root_finish) (lambda (_ m s) (push (list 'finish m s) log) s))
              ((symbol-function 'nl_sf_progn_w) (lambda (body &rest _) (push (list 'progn body) log) progn-w))
              ((symbol-function 'nl_env_pop_frame) (lambda (_) (push 'pop log) pop-rc))
              ((symbol-function 'wf_bytecode_root_exhausted) (lambda () (push 'exhausted log) 0))
              ((symbol-function 'nl_abi_fail) (lambda (&rest _) (push 'fail log) 0)))
      (should (= (nl_eic_let_run_w 40002 0 1) 55))
      (should (equal (reverse log)
                     '(mark (frame 2) eval push bind (release 9) (progn 41000) pop)))
      ;; The values fail: nothing is pushed.
      (setq log nil eval-rc 1)
      (should (= (nl_eic_let_run_w 40002 0 1) 0))
      (should (equal (reverse log) '(mark (frame 2) eval fail (finish 9 0))))
      ;; The binder fails: the frame is popped and the root released.
      (setq log nil eval-rc 0 bind-rc 1)
      (should (= (nl_eic_let_run_w 40002 0 1) 0))
      (should (equal (reverse log) '(mark (frame 2) eval push bind pop (release 9) fail)))
      ;; A failing body still pops the frame.
      (setq log nil bind-rc 0 progn-w 0)
      (should (= (nl_eic_let_run_w 40002 0 1) 0))
      (should (member 'pop log))
      ;; A failed pop fails the form.
      (setq log nil progn-w 55 pop-rc -1)
      (should (= (nl_eic_let_run_w 40002 0 1) 0))
      (should (member 'fail log)))))

(ert-deftest i3-let-falls-back-before-evaluating ()
  (i3--setup) (i3--install 'nl_eic_let_w)
  (let (events (nargs 1) (shape 2))
    (cl-letf (((symbol-function 'nl_sp_nargs) (lambda (_) nargs))
              ((symbol-function 'nl_eic_let_shape) (lambda (_) shape))
              ((symbol-function 'nl_eic_let_run_w) (lambda (tail env n) (push (list 'run tail env n) events) 31))
              ((symbol-function 'nl_apply_special_w) (lambda (&rest _) (push 'special events) 5))
              ((symbol-function 'nl_eic_symbol_dispatch_w) (lambda (w &rest _) w)))
      (i3--cons 40002 49000 41000)
      (should (= (nl_eic_let_w 1 2 40002 0) 31))
      (should (equal events '((run 40002 0 2))))
      (setq events nil shape -1)
      (should (= (nl_eic_let_w 1 2 40002 0) 5))
      (setq shape 2 nargs -1)
      (should (= (nl_eic_let_w 1 2 40002 0) 5))
      (setq nargs 1)
      (puthash 112 (nelisp-standalone--special-gate-mask "let") i3--memory)
      (should (= (nl_eic_let_w 1 2 40002 0) 5))
      (puthash 112 0 i3--memory)
      (should (= (nl_eic_let_w 1 2 9 0) 5))
      (should (equal events '(special special special special))))))

(ert-deftest i3-dispatch-routes-setq-and-let-before-the-special-dispatch ()
  (let ((def (i3--definition 'nl_eval_inner_cons_w)))
    (let ((text (prin1-to-string def)))
      (should (string-match-p "nl_eic_setq_w" text))
      (should (string-match-p "nl_eic_let_w" text))
      ;; dolist keeps its own arm and the generic dispatch stays the last arm.
      (should (string-match-p "nl_eic_dolist_w" text))
      (should (< (string-match "nl_eic_setq_w" text)
                 (string-match "nl_apply_special_w" text))))))

(provide 'nelisp-interp3-test)

;;; nelisp-interp3-test.el ends here
