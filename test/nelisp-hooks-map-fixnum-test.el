;;; nelisp-hooks-map-fixnum-test.el --- ERT for hooks / map.el / fixnum -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Covers three stdlib completions: hooks (`add-hook' / `remove-hook' /
;; `run-hooks' / `run-hook-with-args' / `run-hook-with-args-until-success'
;; / `run-hook-with-args-until-failure'), a map.el subset (`map-elt' /
;; `map-put!' / `map-delete' / `map-keys' / `map-values' / `map-pairs' /
;; `map-length' / `map-do' / `mapp'), and the `most-positive-fixnum' /
;; `most-negative-fixnum' constants -- see lisp/nelisp-stdlib-misc.el and
;; lisp/nelisp-stdlib.el.
;;
;; Host Emacs already defines every public name tested here, and every
;; definition in those two files is `(unless (fboundp ...))'-gated so it
;; never overwrites a host builtin at ordinary load time.  This file does
;; NOT `fmakunbound' the host originals to force the guards (the
;; precedent in test/nelisp-stdlib-test.el, `(fmakunbound 'member)
;; (fmakunbound 'assoc) (load ... nil t)') -- measured here to crash
;; host Emacs's OWN internals instead: `fmakunbound'ing `run-hooks' and
;; then calling anything that creates a buffer (`load' itself, via
;; `generate-new-buffer' -> `buffer-list-update-hook'; `defalias' on a
;; name with a native-comp trampoline, which pulls in `cl-seq' and MORE
;; buffer creation) signals `void-function' from deep inside Emacs's own
;; machinery, not from this suite's code.  So instead: `nelisp-hmf-test--
;; install' parses lisp/nelisp-stdlib-misc.el's source once, evaluates
;; each internal `nelisp--'-prefixed helper under its real (collision-
;; free) name, and evaluates each of the 15 public names' `defun' body
;; under a synthetic `nelisp-hmf--impl-NAME' name -- the REAL symbol
;; (`add-hook' etc.) is never touched at install time.  Each test then
;; wraps its body in `nelisp-hmf-test--with-nelisp-defs', which `cl-letf'
;; the real names to the synthetic implementations for the test's
;; dynamic extent only: `cl-letf' assigns directly from the old value to
;; the new one and back, with no intermediate void state for anything
;; Emacs's own internals might call.
;;
;; `most-positive-fixnum' / `most-negative-fixnum' are a `defconst', not
;; a function -- nothing to swap.  NeLisp's `(unless (boundp ...))' guard
;; means the value under test is whichever of the two constants bound it
;; first; since both are measured (see the comment beside the `defconst'
;; forms) to be numerically identical to Emacs's own, checking the value
;; alone is a correct test regardless of which one won the race.
;;
;; The OVERFLOW behavior of `+' / `*' (`nelisp--add2' / `nelisp--mul2' in
;; lisp/nelisp-jit-strategy.el) is deliberately NOT exercised here: it is
;; built on `nl-jit-call-i64-i64', a native primitive of the standalone
;; runtime with no host-Emacs equivalent (calling it here would be
;; `void-function', not a meaningful test).  That behavior is covered by
;; `nelisp-standalone--reader-stdlib-completion-smoke'
;; (scripts/nelisp-standalone-build.el) against the actual binary.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defvar nelisp-hmf-test--pub-names
  '(add-hook remove-hook run-hooks run-hook-with-args
    run-hook-with-args-until-success run-hook-with-args-until-failure
    map-elt map-put! map-delete map-keys map-values map-pairs
    map-length map-do mapp)
  "Public (host-colliding) names.  Each one's `defun' is installed
under a synthetic `nelisp-hmf--impl-NAME' symbol (see
`nelisp-hmf-test--install') and only bound onto the real NAME via
`cl-letf' inside `nelisp-hmf-test--with-nelisp-defs', for one test's
dynamic extent.")

(defvar nelisp-hmf-test--helper-names
  '(nelisp--hook-depths nelisp--hook-list-p nelisp--run-hook-call
    nelisp--run-hook-value nelisp--plist-p)
  "Internal (`nelisp--'-prefixed) helper names the public functions
above call by their real names.  These never collide with anything
Emacs's own machinery calls internally, so they are installed once,
directly, under their real names.")

(defun nelisp-hmf-test--install ()
  "Parse lisp/nelisp-stdlib-misc.el once and install this suite's
target definitions -- see the Commentary above for why this does not
`fmakunbound'+`load' the file the way test/nelisp-stdlib-test.el does
for its own (non-hook, non-buffer-touching) targets."
  (let* ((src (with-temp-buffer
                (insert-file-contents
                 (expand-file-name "lisp/nelisp-stdlib-misc.el" default-directory))
                (buffer-string)))
         (pos 0) (len (length src)))
    (dolist (n nelisp-hmf-test--helper-names)
      (when (fboundp n) (fmakunbound n))
      (when (boundp n) (makunbound n)))
    (while (< pos len)
      (let ((r (condition-case nil (read-from-string src pos len) (end-of-file nil))))
        (if (not r)
            (setq pos len)
          (setq pos (cdr r))
          (let ((form (car r)))
            (cond
             ;; (unless (get 'map-not-inplace 'error-conditions) (define-error ...))
             ((and (consp form) (eq (car form) 'unless)
                   (equal (cadr form) '(get 'map-not-inplace 'error-conditions)))
              (eval (caddr form) t))
             ;; (unless (fboundp 'NAME) (defun/defvar NAME ...))
             ((and (consp form) (eq (car form) 'unless)
                   (consp (cadr form))
                   (memq (car (cadr form)) '(fboundp boundp)))
              (let* ((guarded-name (cadr (cadr (cadr form))))
                     (inner (caddr form)))
                (cond
                 ((memq guarded-name nelisp-hmf-test--helper-names)
                  (eval inner t))
                 ((memq guarded-name nelisp-hmf-test--pub-names)
                  (let* ((impl (intern (format "nelisp-hmf--impl-%s" guarded-name)))
                         (renamed (cons (car inner) (cons impl (cddr inner)))))
                    (eval renamed t)))))))))))))

(nelisp-hmf-test--install)

(defmacro nelisp-hmf-test--with-nelisp-defs (&rest body)
  "Run BODY with the real names in `nelisp-hmf-test--pub-names' bound
(via `cl-letf', for BODY's dynamic extent only) to the synthetic
`nelisp-hmf--impl-NAME' implementations `nelisp-hmf-test--install'
built.  Restores whatever was bound to each real name before -- host
Emacs's own definition in every normal run of this suite."
  `(cl-letf ,(mapcar (lambda (n)
                        `((symbol-function ',n)
                          (symbol-function ',(intern (format "nelisp-hmf--impl-%s" n)))))
                      nelisp-hmf-test--pub-names)
     ,@body))


;;; Hooks -------------------------------------------------------------

(defvar nelisp-hmf-h1 nil)
(defvar nelisp-hmf-h2 nil)
(defvar nelisp-hmf-h3 nil)

(ert-deftest nelisp-hmf-add-hook-depth-ordering ()
  "DEPTH sorts ascending; the default-depth tie goes to the newest add."
  (nelisp-hmf-test--with-nelisp-defs
   (setq nelisp-hmf-h1 nil)
   (add-hook 'nelisp-hmf-h1 (lambda () 'a))
   (add-hook 'nelisp-hmf-h1 (lambda () 'b) 95)
   (add-hook 'nelisp-hmf-h1 (lambda () 'c) -50)
   (should (equal (mapcar #'funcall nelisp-hmf-h1) '(c a b)))
   ;; Two same-(default-)depth adds: most recently added comes first.
   (setq nelisp-hmf-h1 nil)
   (add-hook 'nelisp-hmf-h1 (lambda () 'x1))
   (add-hook 'nelisp-hmf-h1 (lambda () 'x2))
   (should (equal (mapcar #'funcall nelisp-hmf-h1) '(x2 x1)))))

(ert-deftest nelisp-hmf-add-hook-dedup-and-no-move ()
  "Re-adding an already-present function is a no-op, including at a
different DEPTH -- membership is checked before DEPTH is consulted."
  (nelisp-hmf-test--with-nelisp-defs
   (setq nelisp-hmf-h1 nil)
   (defalias 'nelisp-hmf-named (lambda () 'named))
   (add-hook 'nelisp-hmf-h1 (lambda () 'z) 50)
   (add-hook 'nelisp-hmf-h1 #'nelisp-hmf-named -10)
   (add-hook 'nelisp-hmf-h1 #'nelisp-hmf-named 50)
   (should (= 2 (length nelisp-hmf-h1)))
   (should (eq (car nelisp-hmf-h1) 'nelisp-hmf-named))))

(ert-deftest nelisp-hmf-add-hook-wraps-single-function-value ()
  "A hook already holding a single function gets wrapped into a list."
  (nelisp-hmf-test--with-nelisp-defs
   (setq nelisp-hmf-h1 (lambda () 'orig))
   (add-hook 'nelisp-hmf-h1 (lambda () 'new))
   (should (= 2 (length nelisp-hmf-h1)))))

(ert-deftest nelisp-hmf-remove-hook ()
  (nelisp-hmf-test--with-nelisp-defs
   (setq nelisp-hmf-h1 nil)
   (defalias 'nelisp-hmf-p1 (lambda () 'p1))
   (defalias 'nelisp-hmf-p2 (lambda () 'p2))
   (add-hook 'nelisp-hmf-h1 #'nelisp-hmf-p1)
   (add-hook 'nelisp-hmf-h1 #'nelisp-hmf-p2)
   (remove-hook 'nelisp-hmf-h1 #'nelisp-hmf-p1)
   (should (equal nelisp-hmf-h1 '(nelisp-hmf-p2)))
   ;; Removing the sole remaining function collapses to nil, not (nil).
   (remove-hook 'nelisp-hmf-h1 #'nelisp-hmf-p2)
   (should (null nelisp-hmf-h1))))

(ert-deftest nelisp-hmf-run-hooks-void-is-noop ()
  (nelisp-hmf-test--with-nelisp-defs
   (makunbound 'nelisp-hmf-h3)
   (should (null (run-hooks 'nelisp-hmf-h3)))
   (should-not (boundp 'nelisp-hmf-h3))))

(ert-deftest nelisp-hmf-run-hook-with-args-until-success ()
  "Stops at the first non-nil return; later functions do not run."
  (nelisp-hmf-test--with-nelisp-defs
   (let ((ran nil))
     (setq nelisp-hmf-h2
           (list (lambda (x) (push (list 'a x) ran) nil)
                 (lambda (x) (push (list 'b x) ran) 'found)
                 (lambda (_x) (push 'c ran) 'must-not-run)))
     (should (eq (run-hook-with-args-until-success 'nelisp-hmf-h2 42) 'found))
     (should (equal (reverse ran) '((a 42) (b 42)))))))

(ert-deftest nelisp-hmf-run-hook-with-args-until-failure ()
  "Stops at the first nil return; later functions do not run; all-non-nil
returns non-nil (Emacs: \"do not rely on the precise return value\")."
  (nelisp-hmf-test--with-nelisp-defs
   (let ((ran nil))
     (setq nelisp-hmf-h2
           (list (lambda (x) (push (list 'a x) ran) t)
                 (lambda (x) (push (list 'b x) ran) nil)
                 (lambda (_x) (push 'c ran) t)))
     (should (null (run-hook-with-args-until-failure 'nelisp-hmf-h2 7)))
     (should (equal (reverse ran) '((a 7) (b 7)))))
   (setq nelisp-hmf-h2 (list (lambda (_x) t) (lambda (_x) t)))
   (should (run-hook-with-args-until-failure 'nelisp-hmf-h2 1))))

(ert-deftest nelisp-hmf-hook-t-element-reruns-value-once ()
  "A `t' element re-runs the hook's own current value once more, and any
`t' met during that re-run is a no-op (else this would never
terminate) -- measured against Emacs 30.1; see the block comment above
`add-hook' in lisp/nelisp-stdlib-misc.el."
  (nelisp-hmf-test--with-nelisp-defs
   (let ((calls nil))
     (setq nelisp-hmf-h2 (list (lambda () (push 'fn1 calls))
                                t
                                (lambda () (push 'fn2 calls))
                                t))
     (run-hooks 'nelisp-hmf-h2)
     (should (equal (reverse calls) '(fn1 fn1 fn2 fn2 fn1 fn2))))))

(ert-deftest nelisp-hmf-add-hook-remove-hook-local-arg-ignored ()
  "LOCAL is accepted and silently ignored (documented divergence: this
runtime has no buffers, so it cannot have a distinct local value)."
  (nelisp-hmf-test--with-nelisp-defs
   (setq nelisp-hmf-h1 nil)
   (should (progn (add-hook 'nelisp-hmf-h1 (lambda () 1) nil t) t))
   (should (= 1 (length nelisp-hmf-h1)))
   (should (progn (remove-hook 'nelisp-hmf-h1 (car nelisp-hmf-h1) t) t))
   (should (null nelisp-hmf-h1))))


;;; map.el --------------------------------------------------------------

(ert-deftest nelisp-hmf-map-elt-alist-plist-hash ()
  (nelisp-hmf-test--with-nelisp-defs
   (should (= 1 (map-elt '((a . 1) (b . 2)) 'a)))
   (should (null (map-elt '((a . 1)) 'z)))
   (should (eq 'dflt (map-elt '((a . 1)) 'z 'dflt)))
   (should (= 1 (map-elt '(:a 1 :b 2) :a)))
   (let ((h (make-hash-table)))
     (puthash 'k 'v h)
     (should (eq 'v (map-elt h 'k))))
   ;; Default comparator: `equal' for an alist, `eq' for a plist --
   ;; measured against Emacs 30.1 with a non-eq-equal string key.
   (should (= 1 (map-elt (list (cons "a" 1)) "a")))
   (should (null (map-elt (list "a" 1) "a")))))

(ert-deftest nelisp-hmf-map-put!-alist-existing-mutates ()
  (nelisp-hmf-test--with-nelisp-defs
   (let ((al (list (cons 'a 1) (cons 'b 2))))
     (should (= 99 (map-put! al 'a 99)))
     (should (equal al '((a . 99) (b . 2)))))))

(ert-deftest nelisp-hmf-map-put!-alist-new-key-signals ()
  (nelisp-hmf-test--with-nelisp-defs
   (let ((al (list (cons 'a 1))))
     (should-error (map-put! al 'z 99) :type 'map-not-inplace))))

(ert-deftest nelisp-hmf-map-put!-plist-never-signals ()
  (nelisp-hmf-test--with-nelisp-defs
   (let ((pl (list :a 1)))
     (should (= 99 (map-put! pl :a 99)))
     (should (equal pl '(:a 99)))
     (should (= 100 (map-put! pl :z 100)))
     (should (equal pl '(:a 99 :z 100))))))

(ert-deftest nelisp-hmf-map-put!-hash ()
  (nelisp-hmf-test--with-nelisp-defs
   (let ((h (make-hash-table)))
     (puthash 'a 1 h)
     (should (= 99 (map-put! h 'a 99)))
     (should (= 99 (gethash 'a h)))
     (should (= 100 (map-put! h 'z 100)))
     (should (= 100 (gethash 'z h))))))

(ert-deftest nelisp-hmf-map-delete-never-mutates-list-original ()
  "Alist/plist deletion always returns a new list; the original binding
is unchanged (Emacs's own documented advice: always `(setq map
(map-delete map key))')."
  (nelisp-hmf-test--with-nelisp-defs
   (let ((al (list (cons 'a 1) (cons 'b 2))))
     (should (equal (map-delete al 'a) '((b . 2))))
     (should (equal al '((a . 1) (b . 2)))))
   (let ((pl (list :a 1 :b 2)))
     (should (equal (map-delete pl :a) '(:b 2)))
     (should (equal pl '(:a 1 :b 2))))))

(ert-deftest nelisp-hmf-map-delete-hash-mutates-and-returns-map ()
  (nelisp-hmf-test--with-nelisp-defs
   (let ((h (make-hash-table)))
     (puthash 'a 1 h)
     (should (eq h (map-delete h 'a)))
     (should (eq 'missing (gethash 'a h 'missing))))))

(ert-deftest nelisp-hmf-map-keys-values-pairs-length ()
  (nelisp-hmf-test--with-nelisp-defs
   (should (equal (map-keys '((a . 1) (b . 2))) '(a b)))
   (should (equal (map-values '((a . 1) (b . 2))) '(1 2)))
   (should (equal (map-pairs '((a . 1) (b . 2))) '((a . 1) (b . 2))))
   (should (= 2 (map-length '((a . 1) (b . 2)))))
   (should (equal (map-keys '(:a 1 :b 2)) '(:a :b)))
   (should (= 2 (map-length '(:a 1 :b 2))))
   (let ((h (make-hash-table)))
     (puthash 'a 1 h) (puthash 'b 2 h)
     (should (equal (sort (map-keys h) #'string<) '(a b)))
     (should (= 2 (map-length h))))))

(ert-deftest nelisp-hmf-map-do ()
  (nelisp-hmf-test--with-nelisp-defs
   (let (acc)
     (map-do (lambda (k v) (push (cons k v) acc)) '((a . 1) (b . 2)))
     (should (equal (nreverse acc) '((a . 1) (b . 2)))))
   (let (acc)
     (map-do (lambda (k v) (push (cons k v) acc)) '(:a 1 :b 2))
     (should (equal (nreverse acc) '((:a . 1) (:b . 2)))))))

(ert-deftest nelisp-hmf-mapp ()
  (nelisp-hmf-test--with-nelisp-defs
   (should (mapp '((a . 1))))
   (should (mapp '(:a 1)))
   (should (mapp (make-hash-table)))
   (should (mapp nil))
   (should (mapp [1 2 3]))
   (should-not (mapp 5))
   (should-not (mapp 'sym))))


;;; Fixnum constants ------------------------------------------------

(ert-deftest nelisp-hmf-fixnum-constants ()
  "Measured (2026-08-22) via the standalone's own `ash' wraparound
boundary; also Emacs 30.1's own values, bit for bit."
  (should (= most-positive-fixnum 2305843009213693951))
  (should (= most-negative-fixnum -2305843009213693952)))

(provide 'nelisp-hooks-map-fixnum-test)
;;; nelisp-hooks-map-fixnum-test.el ends here
