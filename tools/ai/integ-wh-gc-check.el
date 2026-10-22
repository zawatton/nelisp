;;; Copied from the symbols lane (eval/symbols-20261021, target/symbols/img/gc-check.el)
;;; for the integ-wh at-scale dump and restore check (tools/ai/integ-wh-image-scale.sh).
;;; One change: `macroexp--dynvars' is volatile (its length is the defvars the loader has
;;; seen, and each run of this file loads it again); the lane's library image had the
;;; same value in the dumping and the verifying run.
;;; gc-check.el --- a restored library image keeps every symbol across collections -*- lexical-binding: t; -*-

;; BIN --cold-load-from IMAGE --load gc-check.el      (ICHK_MODE=check, default)
;;   signature of every interned symbol, then churn and collections, then the
;;   signature again; new interned and uninterned symbols beside the image's.
;; ICHK_MODE=dump ICHK_IMAGE=PATH: check, then dump a second-generation image.
;; ICHK_MODE=verify: under --cold-load-from of that second image, the
;;   signature taken before the dump still equals the live one.
;; Prints "ICHK <mode> checks=N failures=F PASS|FAIL" last.
(defvar ichk-mode nil)
(defvar ichk-checks 0)
(defvar ichk-failures 0)
(defvar ichk-sig-hash nil)
(defvar ichk-sig-count nil)
(defvar ichk-sig-list nil)
(defvar ichk-keep nil)
(setq ichk-mode (or (getenv "ICHK_MODE") "check") ichk-checks 0 ichk-failures 0)

(defun ichk-check (ok label)
  (setq ichk-checks (1+ ichk-checks))
  (unless ok
    (setq ichk-failures (1+ ichk-failures))
    (princ (format "ICHK FAIL %s\n" label))))

(defun ichk-summary (v)
  (cond ((integerp v) v)
        ((stringp v) (length v))
        ((null v) 'nil)
        ((eq v t) 't)
        ((symbolp v) (symbol-name v))
        ((consp v) (condition-case nil (length v) (error 'dotted)))
        ((vectorp v) (length v))
        ((hash-table-p v) (hash-table-count v))
        (t (type-of v))))

(defun ichk-fkind (f)
  (cond ((symbolp f) (symbol-name f))
        ((consp f) (car f))
        (t (type-of f))))

(defun ichk-class (v)
  (cond ((integerp v) 'int) ((stringp v) 'str) ((or (null v) (eq v t)) 'bool)
        ((symbolp v) 'sym) ((consp v) 'cons) ((vectorp v) 'vec) ((hash-table-p v) 'hash) (t (type-of v))))

(defun ichk-volatile-p (name)
  "Library state that printing or running changes: only its class is compared."
  (or (string-match-p "\\`\\(emacs-batch\\|ichk-\\|macroexp--dynvars\\)" name) (string-match-p "pending\\|tick\\|counter\\|count\\'\\|index\\|serial\\|-gen\\|stats\\|-hits\\|-calls\\|-misses\\|environment" name)))

(defun ichk-one (s)
  (let ((name (symbol-name s)))
    (list name
          (boundp s)
          (fboundp s)
          (length (symbol-plist s))
          (and (boundp s)
               (condition-case e (if (ichk-volatile-p name) (ichk-class (symbol-value s)) (ichk-summary (symbol-value s)))
                 (error (car e))))
          (and (fboundp s) (ichk-fkind (symbol-function s))))))

(defun ichk-signature ()
  (let ((acc nil))
    (mapatoms (lambda (s) (push (ichk-one s) acc)))
    (sort acc (lambda (a b) (string< (car a) (car b))))))

(defun ichk-digest (sig)
  "A fold of every entry's sxhash (sxhash-equal alone looks at the first seven elements)."
  (let ((acc 7))
    (dolist (e sig)
      (setq acc (logand (+ (* acc 31) (sxhash-equal e)) #x3fffffffffffff)))
    acc))

(defun ichk-list-equal (a b)
  "Element-wise equal of two long lists: NeLisp's `equal' recurses on the cdr and
signals excessive-lisp-nesting from 10000 elements (GNU 31.1 answers t)."
  (let ((ok t))
    (while (and ok (consp a) (consp b))
      (unless (equal (car a) (car b)) (setq ok nil))
      (setq a (cdr a) b (cdr b)))
    (and ok (null a) (null b))))

(defun ichk-differences (a b)
  "Names (at most 12) whose signature entry differs between A and B."
  (let ((acc nil))
    (while (and (consp a) (consp b) (< (length acc) 12))
      (unless (equal (car a) (car b)) (push (car (car a)) acc))
      (setq a (cdr a) b (cdr b)))
    (nreverse acc)))

(defun ichk-first-difference (a b)
  (let ((r nil))
    (while (and a b (not r))
      (unless (equal (car a) (car b)) (setq r (list (car a) (car b))))
      (setq a (cdr a) b (cdr b)))
    (or r (and (or a b) (list (length a) (length b))))))

(defun ichk-churn (k)
  (let ((keep nil) (n 0))
    (dotimes (i k)
      (push (make-list 40 i) keep)
      (push (make-string 30 ?x) keep)
      (push (make-symbol (format "ichk-churn-%d" i)) keep)
      (push (vector i i i i) keep)
      (setq n (1+ n))
      (when (> (length keep) 6000) (setq keep nil)))
    n))

(defun ichk-gc () (garbage-collect) (garbage-collect))

(defun ichk-block-cells ()
  (condition-case nil (nelisp--debug-switch 42) (error nil)))

(if (equal ichk-mode "verify")
    (let* ((sig (ichk-signature)))
      (princ (format "ICHK verify interned=%d stored=%S\n" (length sig) ichk-sig-count))
      (ichk-check (= (length sig) ichk-sig-count) "interned count equals the dumping run's")
      (ichk-check (equal (ichk-digest sig) ichk-sig-hash) "signature digest equals the dumping run's")
      (unless (equal (ichk-digest sig) ichk-sig-hash)
        (princ (format "ICHK differing names %S\n" (ichk-differences ichk-sig-list sig))))
      (ichk-gc)
      (ichk-check (equal (ichk-digest (ichk-signature)) ichk-sig-hash) "signature after two collections")
      (ichk-check (and (consp ichk-keep) (= (length ichk-keep) 3000)) "kept uninterned population came through")
      (ichk-check (cl-every (lambda (s) (and (symbolp s) (null (intern-soft (symbol-name s))) (equal (symbol-value s) (get s 'ichk))))
                            ichk-keep)
                  "kept uninterned symbols keep their value and property"))
  (let* ((blocks0 (ichk-block-cells))
         (sig0 (ichk-signature))
         (n0 (length sig0)))
    (princ (format "ICHK interned=%d block-cells-after-restore=%S\n" n0 blocks0))
    (ichk-check (> n0 20000) "a full-scale image (more than 20000 interned symbols)")
    (ichk-gc)
    (ichk-check (ichk-list-equal sig0 (ichk-signature)) "signature after two collections")
    (ichk-churn 60000)
    (ichk-gc)
    (let ((sig1 (ichk-signature)))
      (ichk-check (ichk-list-equal sig0 sig1) "signature after churn and collections")
      (unless (ichk-list-equal sig0 sig1)
        (princ (format "ICHK differing names %S\n" (ichk-differences sig0 sig1)))))
    ;; Old-to-young references: a restored symbol is an image object, never
    ;; swept, and its cells must still be roots.  Fresh conses and strings are
    ;; stored in the value cells of restored symbols that were void variables
    ;; (made void again after) and in plists of restored symbols; nothing else
    ;; holds them.
    (let ((cands nil) (n 0) (saved nil) (i 0) (planted nil))
      (mapatoms (lambda (s)
                  (when (and (< n 2500) (not (boundp s)) (fboundp s) (not (keywordp s))
                             (not (memq s '(nil t))))
                    (push s cands) (setq n (1+ n)))))
      (dolist (s cands)
        (when (condition-case nil (progn (set s (list i (make-string 3 ?y))) t) (error nil))
          (push (list s nil i) saved))
        (setq i (1+ i)))
      (setq n 0 i 0)
      (mapatoms (lambda (s)
                  (when (and (< n 2500) (not (keywordp s)) (not (memq s '(nil t))))
                    (put s 'ichk-young (list i (make-string 4 ?z)))
                    (push (cons s i) planted)
                    (setq n (1+ n) i (1+ i)))))
      (princ (format "ICHK young references: %d value cells, %d plists\n" (length saved) (length planted)))
      (ichk-churn 40000)
      (ichk-gc)
      (ichk-gc)
      (ichk-check (cl-every (lambda (e) (equal (symbol-value (car e)) (list (nth 2 e) (make-string 3 ?y)))) saved)
                  "fresh objects held only by restored symbols' value cells survive")
      (ichk-check (cl-every (lambda (e) (equal (get (car e) 'ichk-young) (list (cdr e) (make-string 4 ?z)))) planted)
                  "fresh objects held only by restored symbols' plists survive")
      (dolist (e saved) (makunbound (car e)))
      (ichk-check (cl-every (lambda (e) (not (boundp (car e)))) saved)
                  "the restored symbols are void variables again"))
    ;; New interned symbols beside the image's, and symbols in the blocks.
    (let ((new nil))
      (dotimes (i 5000)
        (let ((s (intern (format "ichk-new-%d" i))))
          (set s i) (put s 'ichk (* i 2)) (push s new)))
      (ichk-churn 20000)
      (ichk-gc)
      (ichk-check (cl-every (lambda (s) (and (boundp s) (equal (get s 'ichk) (* 2 (symbol-value s))))) new)
                  "5000 new interned symbols survive")
      (ichk-check (= (length (ichk-signature)) (+ n0 5000)) "interned count grew by exactly 5000")
      (setq ichk-keep nil)
      (dotimes (i 3000)
        (let ((s (make-symbol (format "ichk-keep-%d" i))))
          (set s (* i 3)) (put s 'ichk (* i 3)) (push s ichk-keep)))
      (ichk-churn 20000)
      (ichk-gc)
      (ichk-check (cl-every (lambda (s) (and (equal (symbol-value s) (get s 'ichk)) (null (intern-soft (symbol-name s))))) ichk-keep)
                  "3000 kept uninterned symbols survive")
      (let ((sig2 (ichk-signature)))
        (ichk-check (ichk-list-equal (mapcar #'car sig2) (sort (append (mapcar #'car sig0) (mapcar #'symbol-name new))
                                                                   #'string<))
                    "the original names are all still interned")))
    (princ (format "ICHK block-cells-at-end=%S\n" (ichk-block-cells)))
    (when (equal ichk-mode "dump")
      ;; The three variables are part of the signature (by class): give them the
      ;; class they have in the verify run before the dump-time signature is taken.
      (setq ichk-sig-hash 0 ichk-sig-count 0 ichk-sig-list (list 0))
      (let ((sig (ichk-signature)))
        (setq ichk-sig-hash (ichk-digest sig) ichk-sig-count (length sig) ichk-sig-list sig))
      (ichk-gc)
      (ichk-check (> (nelisp--arena-dump-image-stream (getenv "ICHK_IMAGE")) 0) "second-generation dump"))))

(princ (format "ICHK %s checks=%d failures=%d %s\n" ichk-mode ichk-checks ichk-failures
               (if (= ichk-failures 0) "PASS" "FAIL")))
