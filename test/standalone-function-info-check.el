;;; standalone-function-info-check.el --- shared behavioral fixture  -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Shared GNU Emacs / NeLisp standalone fixture for `nelisp-function-info'
;; and the compiler-only `nelisp-function--source-parts'.  No ERT.
;;   GNU:        emacs -Q --batch -l test/standalone-function-info-check.el
;;   standalone: nelisp --load test/standalone-function-info-check.el
;; Final line: FUNCTION-INFO-PASS checks=N   or
;;             FUNCTION-INFO-FAIL failures=F checks=N   (exit status 1).
;; NDTW_FUNCTION_INFO_MUTATION=legacy-only swaps in the OLD cons-only
;; admission for interpreted-function pseudovectors; this same fixture must
;; then fail.

(defvar nelisp-function-info-check--n 0)
(defvar nelisp-function-info-check--fail 0)
(defvar nelisp-function-info-check--notes nil)
(defvar nelisp-function-info-check--marker 'untouched)
(defvar nelisp-function-info-check--saved-decode nil)
;; Every `nelisp-function-info-check--ok' call below must execute.
(defconst nelisp-function-info-check--expected 169)

(defun nelisp-function-info-check--ok (name ok)
  "Record check NAME as passed when OK is non-nil."
  (setq nelisp-function-info-check--n (1+ nelisp-function-info-check--n))
  (unless ok
    (setq nelisp-function-info-check--fail (1+ nelisp-function-info-check--fail))
    (princ (format "FUNCTION-INFO-CHECK-FAILED %s\n" name))))

(defun nelisp-function-info-check--case (name thunk)
  "Run THUNK; an unexpected error is a compact failure, never a skip."
  (condition-case e
      (funcall thunk)
    (error
     ;; Report only the condition symbol and a bounded message string: the
     ;; error data may hold a malformed function that cannot be printed.
     (setq nelisp-function-info-check--fail (1+ nelisp-function-info-check--fail))
     (princ (format "FUNCTION-INFO-CASE-ERROR %s %s%s\n" name
                    (let ((sym (car-safe e))) (if (symbolp sym) sym 'unknown))
                    (let ((msg (car-safe (cdr-safe e))))
                      (if (stringp msg)
                          (concat " "
                                  (if (> (length msg) 80) (substring msg 0 80) msg))
                        "")))))))

(defun nelisp-function-info-check--canon (f)
  "Return the canonical (ARGLIST BODY ENV) of interpreted function F.
Independent of the library under test.  On an actual NeLisp closure the
native closure or dynamic lambda backing form is used, because `aref' there
is a projection that allocates fresh binding pairs per call; elsewhere
the GNU slots 0, 1 and 2 are canonical."
  (if (and (fboundp 'nelisp--closure-p) (fboundp 'nelisp--closure-cons)
           (nelisp--closure-p f))
      (let ((c (nelisp--closure-cons f)))
        (if (eq (car c) 'lambda)
            (list (nth 1 c) (nthcdr 2 c) nil)
          (list (nth 2 c) (nthcdr 3 c) (nth 1 c))))
    (list (aref f 0) (aref f 1) (aref f 2))))

;; Load the library from the repository the fixture lives in.
(unless (fboundp 'nelisp-function--source-parts)
  (let* ((here (or load-file-name buffer-file-name
                   (expand-file-name "test/standalone-function-info-check.el"
                                     (or (getenv "NDTW_AOT_ROOT") default-directory))))
         (lib (expand-file-name "../lisp/nelisp-function-info.el"
                                (file-name-directory here))))
    (load lib nil t)))

;; Mutation: the OLD decoder admitted only cons cells as source.  Only the
;; admission of genuine interpreted-function pseudovectors changes; bytecode,
;; vectors, subrs, symbols and every legacy cons still use the real decoder.
(let ((mutation (getenv "NDTW_FUNCTION_INFO_MUTATION")))
  (cond
   ((or (null mutation) (equal mutation "")))
   ((equal mutation "legacy-only")
    (setq nelisp-function-info-check--saved-decode
          (symbol-function 'nelisp-function--decode))
    (fset 'nelisp-function--decode
          (lambda (function)
            (let ((object (condition-case nil (indirect-function function)
                            (error nil))))
              (if (and object (not (consp object)) (not (subrp object))
                       (not (byte-code-function-p object))
                       (interpreted-function-p object))
                  (list :kind 'non-function :reason 'not-a-function :parts nil)
                (funcall nelisp-function-info-check--saved-decode function)))))
    (princ "FUNCTION-INFO-MUTATION legacy-only applied\n"))
   (t (princ (format "FUNCTION-INFO-MUTATION unknown %S\n" mutation))
      (setq nelisp-function-info-check--fail
            (1+ nelisp-function-info-check--fail)))))

(defun nelisp-function-info-check--parts-eq (name parts arglist body env)
  "Check that PARTS reuse ARGLIST BODY ENV by identity."
  (nelisp-function-info-check--ok (concat name " parts") (and parts t))
  (nelisp-function-info-check--ok (concat name " arglist eq")
                                  (eq (plist-get parts :arglist) arglist))
  (nelisp-function-info-check--ok (concat name " body eq")
                                  (eq (plist-get parts :body) body))
  (nelisp-function-info-check--ok (concat name " env eq")
                                  (eq (plist-get parts :environment) env)))

(defmacro nelisp-function-info-check--macro ()
  "Macro whose expansion would flip the marker."
  (setq nelisp-function-info-check--marker 'macro-expanded)
  nil)

;;;; 1. Genuine lexical eval lambda with a captured binding.
(nelisp-function-info-check--case
 "captured-lexical"
 (lambda ()
   ;; Getter and setter close over the SAME lexical `lex'; liveness is proven
   ;; through ordinary language semantics, never through cell layout.
   (let* ((pair (eval '(let ((lex 19))
                         (list (lambda (x) (+ x lex))
                               (lambda (v) (setq lex v))))
                      t))
          (f (car pair))
          (setter (cadr pair))
          (canon (nelisp-function-info-check--canon f))
          (info nil) (parts nil))
     (nelisp-function-info-check--ok "cap interpreted-function-p"
                                     (interpreted-function-p f))
     (nelisp-function-info-check--ok "cap call before gc" (= (funcall f 23) 42))
     (garbage-collect)
     (setq info (nelisp-function-info f) parts (nelisp-function--source-parts f))
     (garbage-collect)
     (nelisp-function-info-check--ok "cap call after gc" (= (funcall f 23) 42))
     (nelisp-function-info-check--ok "cap parts present" (and parts t))
     (nelisp-function-info-check--ok "cap arglist eq canonical"
                                     (eq (plist-get parts :arglist) (nth 0 canon)))
     (nelisp-function-info-check--ok "cap body eq canonical"
                                     (eq (plist-get parts :body) (nth 1 canon)))
     (nelisp-function-info-check--ok "cap env eq canonical"
                                     (eq (plist-get parts :environment) (nth 2 canon)))
     (nelisp-function-info-check--ok "cap canonical env stable"
                                     (eq (nth 2 canon)
                                         (nth 2 (nelisp-function-info-check--canon f))))
     (nelisp-function-info-check--ok "cap lexical-p" (eq (plist-get parts :lexical-p) t))
     (nelisp-function-info-check--ok "cap representation"
                                     (eq (plist-get parts :representation)
                                         'interpreted-function))
     (nelisp-function-info-check--ok "cap captured cell kept"
                                     (eq (assq 'lex (plist-get parts :environment))
                                         (assq 'lex (nth 2 canon))))
     (nelisp-function-info-check--ok "cap arglist value"
                                     (equal (plist-get parts :arglist) '(x)))
     (nelisp-function-info-check--ok "cap schema" (eql (plist-get info :schema) 1))
     (nelisp-function-info-check--ok "cap kind" (eq (plist-get info :kind) 'interpreted))
     (nelisp-function-info-check--ok "cap source-available"
                                     (eq (plist-get info :source-available-p) t))
     (nelisp-function-info-check--ok "cap info arglist"
                                     (equal (plist-get info :arglist) '(x)))
     (nelisp-function-info-check--ok "cap info lexical" (eq (plist-get info :lexical-p) t))
     (nelisp-function-info-check--ok "cap info reason nil" (null (plist-get info :reason)))
     (nelisp-function-info-check--ok "cap info no env"
                                     (not (plist-member info :environment)))
     (nelisp-function-info-check--ok "cap info no route"
                                     (not (or (plist-member info :execution-route)
                                              (plist-member info :route)
                                              (plist-member info :dispatch)
                                              (plist-member info :native))))
     (nelisp-function-info-check--ok "cap info keys"
                                     (equal (let (ks (p info))
                                              (while p (push (car p) ks) (setq p (cddr p)))
                                              (sort ks (lambda (a b)
                                                         (string< (symbol-name a)
                                                                  (symbol-name b)))))
                                            '(:arglist :kind :lexical-p :reason :schema
                                                       :source-available-p)))
     ;; The captured binding is shared, not copied.
     (funcall setter 20)
     (nelisp-function-info-check--ok "cap env live" (= (funcall f 23) 43))
     (garbage-collect)
     (nelisp-function-info-check--ok "cap env live after gc" (= (funcall f 23) 43))
     (funcall setter 19)
     (nelisp-function-info-check--ok "cap env restored" (= (funcall f 23) 42))
     ;; Fresh plist per call.
     (nelisp-function-info-check--ok "cap fresh plist"
                                     (not (eq info (nelisp-function-info f)))))))

;;;; 2. No-capture marker env and dynamic eval.
(nelisp-function-info-check--case
 "marker-and-dynamic"
 (lambda ()
   (let* ((fl (eval '(lambda (x) x) t))
          (pl (nelisp-function--source-parts fl))
          (il (nelisp-function-info fl))
          (fd (eval '(lambda (x) x) nil))
          (pd (nelisp-function--source-parts fd))
          (id (nelisp-function-info fd))
          (cl (nelisp-function-info-check--canon fl))
          (cd (nelisp-function-info-check--canon fd)))
     (nelisp-function-info-check--ok "mark interpreted-function-p"
                                     (interpreted-function-p fl))
     (nelisp-function-info-check--ok "mark env is (t)" (equal (nth 2 cl) '(t)))
     (nelisp-function-info-check--parts-eq "mark" pl (nth 0 cl) (nth 1 cl) (nth 2 cl))
     (nelisp-function-info-check--ok "mark lexical-p" (eq (plist-get pl :lexical-p) t))
     (nelisp-function-info-check--ok "mark info kind" (eq (plist-get il :kind) 'interpreted))
     (nelisp-function-info-check--ok "mark info lexical" (eq (plist-get il :lexical-p) t))
     (nelisp-function-info-check--ok "mark call" (= (funcall fl 5) 5))
     (nelisp-function-info-check--ok "dyn interpreted-function-p"
                                     (interpreted-function-p fd))
     (nelisp-function-info-check--ok "dyn env nil" (null (nth 2 cd)))
     (nelisp-function-info-check--parts-eq "dyn" pd (nth 0 cd) (nth 1 cd) (nth 2 cd))
     (nelisp-function-info-check--ok "dyn lexical-p nil" (null (plist-get pd :lexical-p)))
     (nelisp-function-info-check--ok "dyn env nil in parts" (null (plist-get pd :environment)))
     (nelisp-function-info-check--ok "dyn info kind" (eq (plist-get id :kind) 'interpreted))
     (nelisp-function-info-check--ok "dyn info available"
                                     (eq (plist-get id :source-available-p) t))
     (nelisp-function-info-check--ok "dyn info lexical nil"
                                     (null (plist-get id :lexical-p)))
     (nelisp-function-info-check--ok "dyn call" (= (funcall fd 6) 6)))))

;;;; 3. Legacy representations: introspection only, never invoked.
(nelisp-function-info-check--case
 "legacy"
 (lambda ()
   (let* ((clo (list 'closure (list t) (list 'x) (list '+ 'x 1)))
          (lam (list 'lambda (list 'x) (list '+ 'x 1)))
          (pc (nelisp-function--source-parts clo))
          (pl (nelisp-function--source-parts lam))
          (ic (nelisp-function-info clo))
          (il (nelisp-function-info lam)))
     (nelisp-function-info-check--parts-eq "clo" pc (nth 2 clo) (nthcdr 3 clo) (nth 1 clo))
     (nelisp-function-info-check--ok "clo representation"
                                     (eq (plist-get pc :representation) 'legacy-closure))
     (nelisp-function-info-check--ok "clo lexical" (eq (plist-get pc :lexical-p) t))
     (nelisp-function-info-check--ok "clo info kind" (eq (plist-get ic :kind) 'interpreted))
     (nelisp-function-info-check--ok "clo info arglist" (equal (plist-get ic :arglist) '(x)))
     (nelisp-function-info-check--ok "clo info lexical" (eq (plist-get ic :lexical-p) t))
     (nelisp-function-info-check--ok "clo info available"
                                     (eq (plist-get ic :source-available-p) t))
     (nelisp-function-info-check--parts-eq "lam" pl (nth 1 lam) (nthcdr 2 lam) nil)
     (nelisp-function-info-check--ok "lam representation"
                                     (eq (plist-get pl :representation) 'legacy-lambda))
     (nelisp-function-info-check--ok "lam not lexical" (null (plist-get pl :lexical-p)))
     (nelisp-function-info-check--ok "lam info kind" (eq (plist-get il :kind) 'interpreted))
     (nelisp-function-info-check--ok "lam info lexical" (null (plist-get il :lexical-p)))
     (nelisp-function-info-check--ok "legacy untouched"
                                     (and (equal clo '(closure (t) (x) (+ x 1)))
                                          (equal lam '(lambda (x) (+ x 1))))))))

;;;; 4. Bytecode, ordinary vectors.
(nelisp-function-info-check--case
 "bytecode-and-vectors"
 (lambda ()
   (let* ((bc (car (read-from-string "#[257 \"\\300\\207\" [42] 2]")))
          (ib (nelisp-function-info bc))
          (vec (vector 1 2 3))
          (iv (nelisp-function-info vec)))
     (nelisp-function-info-check--ok "bc real byte-code" (byte-code-function-p bc))
     (nelisp-function-info-check--ok "bc not interpreted" (not (interpreted-function-p bc)))
     (nelisp-function-info-check--ok "bc kind" (eq (plist-get ib :kind) 'bytecode))
     (nelisp-function-info-check--ok "bc no source" (null (plist-get ib :source-available-p)))
     (nelisp-function-info-check--ok "bc parts nil" (null (nelisp-function--source-parts bc)))
     (nelisp-function-info-check--ok "bc lexical-p nil" (null (plist-get ib :lexical-p)))
     (nelisp-function-info-check--ok "vec kind" (eq (plist-get iv :kind) 'non-function))
     (nelisp-function-info-check--ok "vec no source" (null (plist-get iv :source-available-p)))
     (nelisp-function-info-check--ok "vec parts nil" (null (nelisp-function--source-parts vec)))
     (dolist (bad (list (vector (list 'x) (list 'x) nil)
                        (vector nil nil nil nil)
                        (vector (list 'x) (list 1) (list t) nil nil)
                        (vector)
                        (vector 'closure (list t) (list 'x) 1)))
       (nelisp-function-info-check--ok "malformed vec non-function"
                                       (eq (plist-get (nelisp-function-info bad) :kind)
                                           'non-function))
       (nelisp-function-info-check--ok "malformed vec no parts"
                                       (null (nelisp-function--source-parts bad)))))))

;;;; 5. Subr, unbound, alias, autoload, cycle rejection.
(nelisp-function-info-check--case
 "symbols"
 (lambda ()
   (let* ((car-subr (symbol-function 'car))
          (src (eval '(lambda (y) (* y 2)) (list (cons 'k 1))))
          (unbound 'nelisp-function-info-check--probe-unbound)
          (alias 'nelisp-function-info-check--probe-alias)
          (alias2 'nelisp-function-info-check--probe-alias2)
          (auto 'nelisp-function-info-check--probe-autoload)
          (cyc-a 'nelisp-function-info-check--probe-cyc-a)
          (cyc-b 'nelisp-function-info-check--probe-cyc-b)
          (probes (list unbound alias alias2 auto cyc-a cyc-b))
          (saved (mapcar (lambda (s) (and (fboundp s) (symbol-function s))) probes))
          (cell (list 'autoload "nelisp-function-info-check-no-such-file" nil nil nil))
          (cycle-rejected nil))
     (unwind-protect
         (progn
           (dolist (s probes) (fset s nil))
           ;; subr
           (nelisp-function-info-check--ok "subr real" (subrp car-subr))
           (nelisp-function-info-check--ok "subr object kind"
                                           (eq (plist-get (nelisp-function-info car-subr) :kind) 'subr))
           (nelisp-function-info-check--ok "subr symbol kind"
                                           (eq (plist-get (nelisp-function-info 'car) :kind) 'subr))
           (nelisp-function-info-check--ok "subr no source"
                                           (null (plist-get (nelisp-function-info 'car)
                                                            :source-available-p)))
           (nelisp-function-info-check--ok "subr parts nil"
                                           (null (nelisp-function--source-parts 'car)))
           ;; unbound
           (nelisp-function-info-check--ok "unbound is unbound" (not (fboundp unbound)))
           (nelisp-function-info-check--ok "unbound kind"
                                           (eq (plist-get (nelisp-function-info unbound) :kind)
                                               'unresolved))
           (nelisp-function-info-check--ok "unbound reason"
                                           (eq (plist-get (nelisp-function-info unbound) :reason)
                                               'void-function))
           (nelisp-function-info-check--ok "unbound parts nil"
                                           (null (nelisp-function--source-parts unbound)))
           ;; alias chain to a real source function
           (fset alias src)
           (fset alias2 alias)
           (nelisp-function-info-check--parts-eq "alias" (nelisp-function--source-parts alias2)
                                                 (nth 0 (nelisp-function-info-check--canon src))
                                                 (nth 1 (nelisp-function-info-check--canon src))
                                                 (nth 2 (nelisp-function-info-check--canon src)))
           (nelisp-function-info-check--ok "alias kind"
                                           (eq (plist-get (nelisp-function-info alias2) :kind)
                                               'interpreted))
           (nelisp-function-info-check--ok "alias cells unchanged"
                                           (and (eq (symbol-function alias) src)
                                                (eq (symbol-function alias2) alias)))
           ;; autoload object stays unresolved and unloaded
           (fset auto cell)
           (nelisp-function-info-check--ok "autoload kind"
                                           (eq (plist-get (nelisp-function-info auto) :kind)
                                               'unresolved))
           (nelisp-function-info-check--ok "autoload reason"
                                           (eq (plist-get (nelisp-function-info auto) :reason)
                                               'autoload))
           (nelisp-function-info-check--ok "autoload object kind"
                                           (eq (plist-get (nelisp-function-info cell) :kind)
                                               'unresolved))
           (nelisp-function-info-check--ok "autoload parts nil"
                                           (null (nelisp-function--source-parts auto)))
           (nelisp-function-info-check--ok "autoload still unloaded"
                                           (and (eq (symbol-function auto) cell)
                                                (eq (car cell) 'autoload)))
           ;; Cyclic aliases: the runtime must reject creation at fset.
           (fset cyc-a cyc-b)
           (setq cycle-rejected
                 (condition-case nil (progn (fset cyc-b cyc-a) nil) (error t)))
           (nelisp-function-info-check--ok "cycle fset rejected" cycle-rejected)
           (nelisp-function-info-check--ok "cycle cell untouched"
                                           (null (symbol-function cyc-b)))
           (nelisp-function-info-check--ok "cycle half-chain unresolved"
                                           (eq (plist-get (nelisp-function-info cyc-a) :kind)
                                               'unresolved))
           (push (format "cycle-branch=%s"
                         (if cycle-rejected "not-exercised(fset-rejected)" "injected"))
                 nelisp-function-info-check--notes))
       (let ((ss saved) (ps probes))
         (while ps
           (fset (car ps) (car ss))
           (setq ps (cdr ps) ss (cdr ss))))))))

;;;; 6. Malformed legacy sources are rejected promptly.
(defun nelisp-function-info-check--rejects (name form)
  "Check FORM is rejected by both entry points, and quickly."
  (let* ((t0 (float-time))
         (info (nelisp-function-info form))
         (parts (nelisp-function--source-parts form))
         (dt (- (float-time) t0)))
    (nelisp-function-info-check--ok (concat name " no parts") (null parts))
    (nelisp-function-info-check--ok (concat name " kind")
                                    (eq (plist-get info :kind) 'non-function))
    (nelisp-function-info-check--ok (concat name " no source")
                                    (null (plist-get info :source-available-p)))
    (nelisp-function-info-check--ok (concat name " prompt") (< dt 5.0))))

(nelisp-function-info-check--case
 "malformed-legacy"
 (lambda ()
   (let ((circ-body (list 'closure (list t) (list 'x) 1 2))
         (circ-args (list 'x 'y 'z))
         (circ-env (list (cons 'a 1) (cons 'b 2))))
     (setcdr (last circ-body) (nthcdr 3 circ-body))
     (setcdr (last circ-args) circ-args)
     (setcdr (last circ-env) circ-env)
     (nelisp-function-info-check--rejects "improper arglist"
                                          (list 'closure (list t) (cons 'x 'y) 1))
     (nelisp-function-info-check--rejects "improper body"
                                          (cons 'closure (cons (list t) (cons (list 'x) 5))))
     (nelisp-function-info-check--rejects "circular body" circ-body)
     (nelisp-function-info-check--rejects "circular arglist"
                                          (list 'closure (list t) circ-args 1))
     (nelisp-function-info-check--rejects "circular env"
                                          (list 'closure circ-env (list 'x) 1))
     (nelisp-function-info-check--rejects "unknown env atom"
                                          (list 'closure 5 (list 'x) 1))
     (nelisp-function-info-check--rejects "unknown env entry"
                                          (list 'closure (list 1) (list 'x) 1))
     (nelisp-function-info-check--rejects "unknown env string"
                                          (list 'closure "env" (list 'x) 1))
     (nelisp-function-info-check--rejects "nil legacy env"
                                          (list 'closure nil (list 'x) 1))
     (nelisp-function-info-check--rejects "bad arglist element"
                                          (list 'lambda (list 1) 1))
     (nelisp-function-info-check--rejects "dangling &optional"
                                          (list 'lambda (list 'x '&optional) 1))
     (nelisp-function-info-check--rejects "truncated closure"
                                          (list 'closure (list t)))
     (nelisp-function-info-check--rejects "truncated lambda" (list 'lambda))
     (nelisp-function-info-check--rejects "improper lambda"
                                          (cons 'lambda (cons (list 'x) 5))))))

;;;; 7. Introspection never runs bodies or macros and changes no cells.
(nelisp-function-info-check--case
 "no-execution"
 (lambda ()
   (let* ((sym 'nelisp-function-info-check--probe-effect)
          (saved (and (fboundp sym) (symbol-function sym)))
          (f (eval '(lambda ()
                      (setq nelisp-function-info-check--marker 'body-ran)
                      (nelisp-function-info-check--macro))
                   t))
          (leg (list 'closure (list t) nil
                     '(setq nelisp-function-info-check--marker 'legacy-ran)
                     '(nelisp-function-info-check--macro)))
          (auto (list 'autoload "nelisp-function-info-check-no-such-file" nil nil nil)))
     (unwind-protect
         (progn
           (setq nelisp-function-info-check--marker 'untouched)
           (fset sym f)
           (nelisp-function-info f)
           (nelisp-function--source-parts f)
           (nelisp-function-info sym)
           (nelisp-function--source-parts sym)
           (nelisp-function-info leg)
           (nelisp-function--source-parts leg)
           (nelisp-function-info auto)
           (nelisp-function-info-check--ok "marker untouched"
                                           (eq nelisp-function-info-check--marker 'untouched))
           (nelisp-function-info-check--ok "function cell identity" (eq (symbol-function sym) f))
           (nelisp-function-info-check--ok "function object identity"
                                           (eq (plist-get (nelisp-function--source-parts f) :body)
                                               (nth 1 (nelisp-function-info-check--canon f))))
           (nelisp-function-info-check--ok "legacy form unchanged"
                                           (equal leg '(closure (t) nil
                                                                (setq nelisp-function-info-check--marker
                                                                      'legacy-ran)
                                                                (nelisp-function-info-check--macro))))
           (nelisp-function-info-check--ok "marker really settable"
                                           (progn (funcall f)
                                                  (eq nelisp-function-info-check--marker
                                                      'macro-expanded))))
       (fset sym saved)
       (setq nelisp-function-info-check--marker 'untouched)))))

;;;; Completion marker.
(let ((n nelisp-function-info-check--n)
      (fail nelisp-function-info-check--fail))
  (dolist (note (reverse nelisp-function-info-check--notes))
    (princ (format "FUNCTION-INFO-NOTE %s\n" note)))
  (when (and (zerop fail) (/= n nelisp-function-info-check--expected))
    (princ (format "FUNCTION-INFO-CHECK-COUNT expected=%d ran=%d\n"
                   nelisp-function-info-check--expected n))
    (setq fail 1))
  (cond
   ((and (zerop fail) (> n 0))
    (princ (format "FUNCTION-INFO-PASS checks=%d\n" n))
    (kill-emacs 0))
   (t
    (princ (format "FUNCTION-INFO-FAIL failures=%d checks=%d\n" (max fail 1) n))
    (kill-emacs 1))))

;;; standalone-function-info-check.el ends here
