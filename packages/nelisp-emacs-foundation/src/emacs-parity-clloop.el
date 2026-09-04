;;; emacs-parity-clloop.el --- cl-loop codegen override for the audit replay -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Real-init audit parity fix (b1k21 frontier).  The frozen `combined.repl'
;; baked into the audit carries a copy of `emacs-cl-macros--loop-build' whose
;; iterator-less `while'/`until' (= `has-loop-cond') path drops the loop's
;; accumulator ACTIONS (collect/append/nconc/sum/count/maximize/minimize).
;; The generated skeleton becomes an empty-bodied `(while COND)': the
;; condition's side-effecting subforms (e.g. `(pop body)' that only appear
;; inside the dropped `collect' clauses) never run, so a shape like
;;
;;   (cl-loop while (keywordp (car body)) collect (pop body) collect (pop body))
;;
;; (evil-types.el:345, `evil-define-interactive-code' for "<a>") loops
;; forever, consing without bound.  That is the 45+ minute audit hang.
;;
;; Because the buggy copy is inlined/frozen into `combined.repl', editing
;; `src/emacs-cl-macros.el' does NOT take effect for the audit.  On standalone
;; NeLisp this file redefines the cl-loop expander subsystem at runtime, and
;; is loaded by the compat-shim installer AFTER the frozen copy has been
;; evaluated but BEFORE user init (hence before `evil').  Host Emacs skips
;; every replacement behind a NeLisp-only runtime marker.  We redefine:
;;
;;   - emacs-cl-macros--loop-destructure-bindings
;;   - emacs-cl-macros--loop-wrap-body
;;   - emacs-cl-macros--loop-acc-kind
;;   - emacs-cl-macros--loop-iterate
;;   - emacs-cl-macros--loop-build       (the fix: `has-loop-cond' path now
;;                                        threads the accumulator body,
;;                                        acc let-bindings, nreverse post,
;;                                        and result form, exactly like the
;;                                        `for..in' iterator path)
;;   - cl-loop                           (re-route through the corrected
;;                                        loop-build; re-registers in
;;                                        `nelisp--macros' for the full
;;                                        self-host evaluator)
;;   - emacs-stub--cl-loop-build         (defensive alias, in case anything
;;                                        resolved cl-loop to the older
;;                                        emacs-stub builder)
;;
;; These bodies are copied verbatim from the current (correct) source of
;; `src/emacs-cl-macros.el', with the `unless fboundp' / `when' guards
;; stripped so the standalone redefinition replaces the frozen copy.

;;; Code:

(when (fboundp 'nelisp--write-stdout-bytes)

;;;; --- loop helpers (correct) -----------------------------------------

(defun emacs-cl-macros--loop-destructure-bindings (pattern source)
  "Return ordered `let*' bindings destructuring PATTERN from SOURCE.
Nested list patterns receive fresh temporaries, and an improper symbol tail
is bound to the unconsumed cdr.  This helper is shared by `cl-loop',
`cl-defmacro', and `cl-destructuring-bind'."
  (if (consp pattern)
      (let* ((item (car pattern))
             (tail (cdr pattern))
             (item-bindings
              (cond
               ((null item) nil)
               ((symbolp item)
                (list (list item (list 'car source))))
               (t
                (let ((value (make-symbol "--cl-destructure--")))
                  (cons
                   (list value (list 'car source))
                   (emacs-cl-macros--loop-destructure-bindings
                    item value)))))))
        (append
         item-bindings
         (emacs-cl-macros--loop-destructure-bindings
          tail (list 'cdr source))))
    (if pattern (list (list pattern source)) nil)))

(defun emacs-cl-macros--loop-wrap-body (pattern item forms)
  "Return one loop body form for PATTERN bound from ITEM and FORMS."
  (if (symbolp pattern)
      (cons 'progn forms)
    (cons 'let
          (cons (emacs-cl-macros--loop-destructure-bindings pattern item)
                forms))))

(defun emacs-cl-macros--loop-acc-kind (kw)
  "Normalise a cl-loop accumulator keyword KW to a KIND symbol, or nil."
  (cond ((memq kw '(collect collecting)) 'collect)
        ((memq kw '(append appending)) 'append)
        ((memq kw '(nconc nconcing)) 'nconc)
        ((memq kw '(sum summing)) 'sum)
        ((memq kw '(count counting)) 'count)
        ((memq kw '(maximize maximizing)) 'max)
        ((memq kw '(minimize minimizing)) 'min)))

(defun emacs-cl-macros--loop-iterate (iters body)
  "Generate iteration over ITERS running BODY (a list of forms).
ITERS is a list of (VAR LIST-FORM [MODE STEP]); MODE `on' binds VAR to
successive list tails advanced by STEP.  Other entries iterate over list
elements.  Multiple element entries iterate in parallel (lockstep, stopping
at the shortest list).  VAR may be a destructuring pattern.  With no
iterators the body does not run."
  (cond
   ((null iters) (list 'progn))
   ((and (null (cdr iters)) (eq (nth 2 (car iters)) 'on))
    (let* ((it (car iters)) (var (car it)) (lf (nth 1 it))
           (step (or (nth 3 it) 'cdr))
           (lv (make-symbol "--loop-tail--"))
           (bindings
            (if (symbolp var)
                (list (list var lv))
              (emacs-cl-macros--loop-destructure-bindings var lv))))
      (list 'let (list (list lv lf))
            (cons 'while
                  (cons lv
                        (append
                         (list (cons 'let (cons bindings body)))
                         (list (list 'setq lv (list 'funcall step lv)))))))))
   ((null (cdr iters))
    (let* ((it (car iters)) (var (car it)) (lf (car (cdr it)))
           (lv (if (symbolp var) var (make-symbol "--loop-item--"))))
      (list 'dolist (list lv lf)
            (emacs-cl-macros--loop-wrap-body var lv body))))
   (t
    (let ((listvars nil) (steps nil) (varbinds nil))
      (dolist (it iters)
        (let* ((var (car it)) (lf (car (cdr it)))
               (lvar (make-symbol "--loop-l--")))
          (setq listvars (append listvars (list (list lvar lf))))
          (setq steps (append steps (list (list 'setq lvar (list 'cdr lvar)))))
          (if (symbolp var)
              (setq varbinds (append varbinds (list (list var (list 'car lvar)))))
            (setq varbinds
                  (append varbinds
                          (emacs-cl-macros--loop-destructure-bindings
                           var (list 'car lvar)))))))
      (list 'let listvars
            (list 'while (cons 'and (mapcar #'car listvars))
                  (cons 'let (cons varbinds body))
                  (cons 'progn steps)))))))

(defun emacs-cl-macros--loop-build (clauses)
  "Build expansion for cl-loop CLAUSES.

Recognised shapes:
  for VAR in LIST                      iterator
  for VAR on LIST [by FUNCTION]        successive list-tail iterator
  for VAR from N to M                  numeric iterator (Phase 4 B)
  for VAR from N below M               numeric iterator (Phase 4 B)
  with VAR = VAL                       binding
  while COND / until COND              pre-test condition
  do FORM …                            unconditional side-effect
  collect FORM                         accumulate into list
  sum FORM                             accumulate sum
  count FORM                           count truthy
  thereis FORM                         first non-nil result
  when COND return FORM                early-exit with FORM
  when COND do FORM                    conditional side-effect
  when COND collect FORM               conditional accumulate

The bodyless form `(cl-loop BODY...)' (= no for/with/do keyword,
just a body to repeat forever with `cl-return' for exit) is also
recognised — Phase 4 B added it so nelisp-regex.el's parse-concat
loops work.

Unrecognised shapes return nil (= caller gets a no-op expansion)."
  (let ((iters nil)            ; list of (VAR LIST-FORM) — parallel iterators
        (with-bindings nil)    ; list of (VAR VAL)
        (actions nil)          ; ordered list of (do FORM COND) / (acc KIND INTO FORM COND)
        (finally-forms nil) (finally-ret nil) (has-finally-ret nil)
        (when-return-cond nil) (when-return-form nil)
        (loop-cond nil) (has-loop-cond nil)
        (thereis-saw nil)
        (bodyless-forms nil)
        (cur clauses) (recognised t))
    (when (and clauses
               (not (memq (car clauses)
                          '(for with do doing collect collecting append appending
                                nconc nconcing sum summing count counting
                                maximize maximizing minimize minimizing
                                thereis when unless if while until repeat finally return
                                named))))
      (setq bodyless-forms clauses cur nil))
    (while (and cur recognised)
      (let ((kw (car cur)))
        (cond
         ((eq kw 'for)
          (let ((v (car (cdr cur))) (k2 (car (cdr (cdr cur)))))
            (cond
             ((eq k2 'in)
              (setq iters (append iters (list (list v (car (cdr (cdr (cdr cur)))))))
                    cur (cdr (cdr (cdr (cdr cur))))))
             ((eq k2 'on)
              (let* ((list-form (car (cdr (cdr (cdr cur)))))
                     (r (cdr (cdr (cdr (cdr cur)))))
                     (step (if (eq (car r) 'by) (car (cdr r)) 'cdr)))
                (when (eq (car r) 'by)
                  (setq r (cdr (cdr r))))
                (setq iters (append iters (list (list v list-form 'on step)))
                      cur r)))
             ;; `for VAR being [the|each] {hash-keys|hash-values} of H'.
             ((eq k2 'being)
              (let* ((r (cdr (cdr (cdr cur))))
                     (r (if (memq (car r) '(the each)) (cdr r) r))
                     (what (car r)) (of-kw (car (cdr r))) (h-form (car (cdr (cdr r)))))
                (if (and (eq of-kw 'of)
                         (memq what '(hash-keys hash-key hash-values hash-value)))
                    (setq iters
                          (append iters
                                  (list (list v (list (if (memq what '(hash-keys hash-key))
                                                          'hash-table-keys 'hash-table-values)
                                                      h-form))))
                          cur (cdr (cdr (cdr r))))
                  (setq recognised nil))))
             ;; `for VAR from N {to,below} M' -> (number-sequence ...).
             ((eq k2 'from)
              (let ((fv (car (cdr (cdr (cdr cur)))))
                    (k3 (car (cdr (cdr (cdr (cdr cur))))))
                    (m (car (cdr (cdr (cdr (cdr (cdr cur))))))))
                (cond
                 ((eq k3 'to)
                  (setq iters (append iters (list (list v (list 'number-sequence fv m))))
                        cur (cdr (cdr (cdr (cdr (cdr (cdr cur))))))))
                 ((eq k3 'below)
                  (setq iters (append iters (list (list v (list 'number-sequence fv (list '1- m)))))
                        cur (cdr (cdr (cdr (cdr (cdr (cdr cur))))))))
                 (t (setq recognised nil)))))
             (t (setq recognised nil)))))
         ((eq kw 'with)
          (if (eq (car (cdr (cdr cur))) '=)
              (setq with-bindings
                    (append with-bindings (list (list (car (cdr cur)) (car (cdr (cdr (cdr cur)))))))
                    cur (cdr (cdr (cdr (cdr cur)))))
            (setq recognised nil)))
         ((memq kw '(while until))
          (setq has-loop-cond t
                loop-cond (if (eq kw 'until)
                              (list 'not (car (cdr cur)))
                            (car (cdr cur)))
                cur (cdr (cdr cur))))
         ((memq kw '(do doing))
          (setq actions (append actions (list (list 'do (car (cdr cur)) nil)))
                cur (cdr (cdr cur))))
         ((emacs-cl-macros--loop-acc-kind kw)
          (let* ((kind (emacs-cl-macros--loop-acc-kind kw))
                 (form (car (cdr cur))) (r (cdr (cdr cur))) (into nil))
            (when (eq (car r) 'into) (setq into (car (cdr r)) r (cdr (cdr r))))
            (setq actions (append actions (list (list 'acc kind into form nil))) cur r)))
         ((eq kw 'thereis)
          (setq thereis-saw t
                actions (append actions (list (list 'thereis (car (cdr cur)))))
                cur (cdr (cdr cur))))
         ;; `when/if/unless COND ACTION [and ACTION ...]' — each ACTION (do /
         ;; accumulator / return) runs under COND (unless = negated).
         ((memq kw '(when if unless))
          (let* ((cr (car (cdr cur)))
                 (cnd (if (eq kw 'unless) (list 'not cr) cr))
                 (r (cdr (cdr cur))) (again t))
            (while (and again r recognised)
              (let ((akw (car r)))
                (cond
                 ((eq akw 'return)
                  (setq when-return-cond cnd when-return-form (car (cdr r)) r (cdr (cdr r))))
                 ((memq akw '(do doing))
                  (setq actions (append actions (list (list 'do (car (cdr r)) cnd)))
                        r (cdr (cdr r))))
                 ((emacs-cl-macros--loop-acc-kind akw)
                  (let* ((kind (emacs-cl-macros--loop-acc-kind akw))
                         (form (car (cdr r))) (rr (cdr (cdr r))) (into nil))
                    (when (eq (car rr) 'into) (setq into (car (cdr rr)) rr (cdr (cdr rr))))
                    (setq actions (append actions (list (list 'acc kind into form cnd))) r rr)))
                 (t (setq again nil))))
              (if (and again r (eq (car r) 'and)
                       (let ((nx (car (cdr r))))
                         (or (memq nx '(do doing return))
                             (emacs-cl-macros--loop-acc-kind nx))))
                  (setq r (cdr r))
                (setq again nil)))
            ;; `else ACTION [and ACTION ...]' — the same actions under (not COND).
            (when (and recognised r (eq (car r) 'else))
              (setq r (cdr r))
              (let ((ecnd (list 'not cnd)) (eagain t))
                (while (and eagain r recognised)
                  (let ((akw (car r)))
                    (cond
                     ((eq akw 'return)
                      (setq when-return-cond ecnd when-return-form (car (cdr r)) r (cdr (cdr r))))
                     ((memq akw '(do doing))
                      (setq actions (append actions (list (list 'do (car (cdr r)) ecnd)))
                            r (cdr (cdr r))))
                     ((emacs-cl-macros--loop-acc-kind akw)
                      (let* ((kind (emacs-cl-macros--loop-acc-kind akw))
                             (form (car (cdr r))) (rr (cdr (cdr r))) (into nil))
                        (when (eq (car rr) 'into) (setq into (car (cdr rr)) rr (cdr (cdr rr))))
                        (setq actions (append actions (list (list 'acc kind into form ecnd))) r rr)))
                     (t (setq eagain nil))))
                  (if (and eagain r (eq (car r) 'and)
                           (let ((nx (car (cdr r))))
                             (or (memq nx '(do doing return))
                                 (emacs-cl-macros--loop-acc-kind nx))))
                      (setq r (cdr r))
                    (setq eagain nil)))))
            (setq cur r)))
         ((eq kw 'finally)
          (let ((r (cdr cur)))
            (cond
             ((eq (car r) 'return)
              (setq has-finally-ret t finally-ret (car (cdr r)) r (cdr (cdr r))))
             (t
              (when (memq (car r) '(do doing)) (setq r (cdr r)))
              (setq finally-forms (append finally-forms (list (car r))) r (cdr r))))
            (setq cur r)))
         (t (setq recognised nil)))))
    ;; ---- generation ----
    (cond
     ((not recognised) nil)
     (bodyless-forms
      (list 'cl-block nil (cons 'while (cons t bodyless-forms))))
     (t
      (let ((default-acc (make-symbol "--loop-acc--"))
            (acc-reg nil)          ; alist accvar -> (init . reverse-p)
            (uses-default nil)
            (body nil)
            (early-saw (or when-return-cond thereis-saw))
            (result-sym (and (or when-return-cond thereis-saw)
                             (make-symbol "--loop-r--")))
            (tag-sym (and (or when-return-cond thereis-saw)
                          (make-symbol "--loop-tag--"))))
        (dolist (act actions)
          (cond
           ((eq (car act) 'do)
            (let ((form (nth 1 act)) (cnd (nth 2 act)))
              (setq body
                    (append body
                            (list (if cnd (list 'when cnd form) form))))))
           ((eq (car act) 'thereis)
            (let ((value-sym (make-symbol "--loop-value--")))
              (setq body
                    (append
                     body
                     (list
                      (list 'let (list (list value-sym (nth 1 act)))
                            (list 'when value-sym
                                  (list 'setq result-sym value-sym)
                                  (list 'throw (list 'quote tag-sym)
                                        nil))))))))
           (t
            ;; accumulator
            (let* ((kind (nth 1 act))
                   (into (nth 2 act))
                   (form (nth 3 act))
                   (cnd (nth 4 act))
                   (av (or into default-acc))
                   (init (if (memq kind '(sum count)) 0 nil))
                   (revp (eq kind 'collect))
                   (acc-form
                    (cond
                     ((eq kind 'collect) (list 'setq av (list 'cons form av)))
                     ((eq kind 'append)  (list 'setq av (list 'append av form)))
                     ((eq kind 'nconc)   (list 'setq av (list 'nconc av form)))
                     ((eq kind 'sum)     (list 'setq av (list '+ av form)))
                     ((eq kind 'count)   (list 'when form (list 'setq av (list '+ av 1))))
                     ((eq kind 'max)     (list 'setq av (list 'if av (list 'max av form) form)))
                     (t                  (list 'setq av (list 'if av (list 'min av form) form))))))
              (unless into (setq uses-default t))
              (unless (assq av acc-reg)
                (setq acc-reg (append acc-reg (list (cons av (cons init revp))))))
              (setq body
                    (append body
                            (list (if cnd
                                      (list 'when cnd acc-form)
                                    acc-form))))))))
        (when when-return-cond
          (setq body (append body
                             (list (list 'when when-return-cond
                                         (list 'setq result-sym when-return-form)
                                         (list 'throw (list 'quote tag-sym) nil))))))
        (let ((skel (if has-loop-cond
                        (cons 'while (cons loop-cond body))
                      (emacs-cl-macros--loop-iterate iters body)))
              (post nil))
          (dolist (e acc-reg)
            (when (cdr (cdr e))        ; reverse-p: collect accs built backwards
              (setq post (append post (list (list 'setq (car e) (list 'nreverse (car e))))))))
          (let ((result
                 (cond
                  (early-saw result-sym)
                  (has-finally-ret finally-ret)
                  (finally-forms
                   (cons 'progn (append finally-forms (list (if uses-default default-acc nil)))))
                  (uses-default default-acc)
                  (t nil)))
                (let-binds
                 (append
                  (mapcar (lambda (e) (list (car e) (car (cdr e)))) acc-reg)
                  (when early-saw (list (list result-sym nil)))
                  with-bindings))
                (core (if early-saw
                          (list 'catch (list 'quote tag-sym) skel)
                        skel)))
            (cons 'let (cons let-binds (append (list core) post (list result)))))))))))

;;;; --- macro re-route (unconditional) ---------------------------------

;; Re-route cl-loop through the corrected builder.  `defmacro' re-runs the
;; runtime's macro registration (function cell + `nelisp--macros' entry for
;; the full self-host evaluator), so this overrides the frozen expander.
(defmacro cl-loop (&rest clauses)
  "Minimal cl-loop (corrected builder).
See `emacs-cl-macros--loop-build'."
  (emacs-cl-macros--loop-build clauses))

;; Defensive: if any path resolved cl-loop to the older emacs-stub builder,
;; route it through the corrected loop-build too.
(defun emacs-stub--cl-loop-build (clauses)
  "Compatibility alias: route to `emacs-cl-macros--loop-build'."
  (emacs-cl-macros--loop-build clauses))

;; ROOT CAUSE (b1k21): the NeLisp runtime prelude baked into the binary
;; (vendor/nelisp/lisp/nelisp-cl-macros.el) defines cl-loop as
;;   (macro (closure nil (&rest clauses) (nelisp-cl-macros--loop-build clauses)))
;; and THAT prelude cl-loop WINS over combined.repl's `emacs-cl-macros' cl-loop:
;; the redefinition at combined.repl is guarded by
;; `(emacs-cl-macros--define-p 'cl-loop)', which returns nil for a real
;; (non-autoload, non-placeholder) prelude macro, so the guard skips.
;; `nelisp-cl-macros--loop-build's `while-cond' path builds
;;   (let WITH-BINDINGS (while COND DO-FORMS...))
;; using ONLY do-forms — it drops collect/sum/count accumulators entirely, and
;; is checked BEFORE the collect path.  So
;;   (cl-loop while (keywordp (car body)) collect (pop body) collect (pop body))
;; expands to (let () (while (keywordp (car body)))): an empty-bodied while
;; whose condition side-effect (the dropped `(pop body)') never runs -> the
;; audit hangs forever at evil-types.el:345.  Route the prelude builder through
;; the corrected `emacs-cl-macros--loop-build' too, so even a cl-loop macro
;; that still calls the prelude builder produces a correct expansion.
(defun nelisp-cl-macros--loop-build (clauses)
  "Corrected override: delegate to `emacs-cl-macros--loop-build'.
The baked prelude version drops accumulators on the iterator-less
`while'/`until' path; this delegation restores them."
  (emacs-cl-macros--loop-build clauses))

;; Belt-and-suspenders: force the `nelisp--macros' entry (used by the full
;; self-host evaluator) to the corrected cl-loop, in case a plain `defmacro'
;; only updated the ordinary function cell.
(when (and (boundp 'nelisp--macros)
           (hash-table-p nelisp--macros))
  (puthash 'cl-loop (symbol-function 'cl-loop) nelisp--macros))

)

(provide 'emacs-parity-clloop)

;;; emacs-parity-clloop.el ends here
