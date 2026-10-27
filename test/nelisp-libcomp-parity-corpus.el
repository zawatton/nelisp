;;; nelisp-libcomp-parity-corpus.el --- GNU parity of the libcomp hot paths  -*- lexical-binding: t; -*-

;; Lane libcomp: re-search-forward (bound / count / noerror variants and the
;; match data it leaves), markers (insertion types, deleted buffers, moves
;; between buffers), run-hooks (local hooks with t, single functions, edits
;; during the run) and cl-generic dispatch (type, eql and head specializers,
;; qualifiers, cl-call-next-method, redefinition).
;;
;; Runs unchanged on GNU Emacs (emacs -Q --batch -l FILE) and on NeLisp
;; (BINARY --load FILE); tools/ai/libcomp-parity.sh compares the transcripts.
;; Each case prints one line "NAME => VALUE", where VALUE is the result or
;; (error SYMBOL . DATA).  Markers never reach the printer: they are shown as
;; (marker POSITION INSERTION-TYPE BUFFER-LIVE-P).
;;
;; LIBCOMP_MUTANT=1 makes `marker-position' answer one too many: the negative
;; control, whose transcript must differ.

;;; Code:

(require 'cl-lib)

(when (equal (getenv "LIBCOMP_MUTANT") "1")
  (let ((orig (symbol-function 'marker-position)))
    (fset 'marker-position (lambda (m) (let ((p (funcall orig m))) (and p (1+ p)))))))

(defun libcomp-pc--show (x)
  (cond ((markerp x) (list 'marker (marker-position x) (marker-insertion-type x)
                           (and (marker-buffer x) (buffer-live-p (marker-buffer x)))))
        ((bufferp x) 'buffer)
        ((consp x) (cons (libcomp-pc--show (car x)) (libcomp-pc--show (cdr x))))
        (t x)))

(defmacro libcomp-pc (name &rest body)
  `(princ (format "%s => %S\n" ',name
                  (libcomp-pc--show
                   (condition-case err (progn ,@body)
                     (error (list 'error (car err) (cdr err))))))))

(defconst libcomp-pc--text "foo 12 bar 345 baz 6\nline2 78 Qux\nFOO 9\n")

(defmacro libcomp-pc-buf (&rest body)
  `(with-temp-buffer (insert libcomp-pc--text) (goto-char (point-min)) ,@body))

;; ---- re-search-forward ----
(libcomp-pc rs-simple (libcomp-pc-buf (list (re-search-forward "[0-9]+") (point) (match-data t))))
(libcomp-pc rs-loop
  (libcomp-pc-buf (let (r) (while (re-search-forward "[0-9]+" nil t) (push (match-string 0) r)) (nreverse r))))
(libcomp-pc rs-groups
  (libcomp-pc-buf (list (re-search-forward "\\([a-z]+\\) \\([0-9]+\\)") (match-data t)
                        (match-beginning 1) (match-end 2) (match-string 1) (match-beginning 3))))
(libcomp-pc rs-optional-group
  (libcomp-pc-buf (list (re-search-forward "\\(x\\)?\\([0-9]+\\)") (match-data t) (match-beginning 1))))
(libcomp-pc rs-match-data-markers
  (libcomp-pc-buf (re-search-forward "b\\(a\\)r") (match-data)))
(libcomp-pc rs-bound-int (libcomp-pc-buf (list (re-search-forward "[0-9]+" 6 t) (point) (match-data t))))
(libcomp-pc rs-bound-exact (libcomp-pc-buf (list (re-search-forward "[0-9]+" 7 t) (point))))
(libcomp-pc rs-bound-marker
  (libcomp-pc-buf (let ((m (copy-marker 12))) (list (re-search-forward "[0-9]+" m t) (point)))))
(libcomp-pc rs-bound-past-end (libcomp-pc-buf (list (re-search-forward "Q" 9999 t) (point))))
(libcomp-pc rs-bound-wrong-side (libcomp-pc-buf (goto-char 10) (re-search-forward "[0-9]+" 5 t)))
(libcomp-pc rs-bound-wrong-type (libcomp-pc-buf (re-search-forward "[0-9]+" "x" t)))
(libcomp-pc rs-fail-noerror-nil (libcomp-pc-buf (re-search-forward "zzz")))
(libcomp-pc rs-fail-noerror-t (libcomp-pc-buf (goto-char 3) (list (re-search-forward "zzz" nil t) (point))))
(libcomp-pc rs-fail-noerror-other
  (libcomp-pc-buf (goto-char 3) (list (re-search-forward "zzz" 20 'move) (point))))
(libcomp-pc rs-fail-noerror-other-nobound
  (libcomp-pc-buf (goto-char 3) (list (re-search-forward "zzz" nil 'move) (point))))
(libcomp-pc rs-fail-keeps-match-data
  (libcomp-pc-buf (re-search-forward "bar") (re-search-forward "zzz" nil t) (match-data t)))
(libcomp-pc rs-count-2 (libcomp-pc-buf (list (re-search-forward "[0-9]+" nil t 2) (point) (match-data t))))
(libcomp-pc rs-count-0 (libcomp-pc-buf (goto-char 4) (list (re-search-forward "[0-9]+" nil t 0) (point))))
(libcomp-pc rs-count-0-match-data
  (libcomp-pc-buf (re-search-forward "bar") (goto-char 4)
                  (list (re-search-forward "[0-9]+" nil t 0) (point) (match-data t))))
(libcomp-pc rs-count-bad-type (libcomp-pc-buf (re-search-forward "[0-9]+" nil t 1.0)))
(libcomp-pc rs-count-bad-type-and-regexp (libcomp-pc-buf (re-search-forward 'x nil t 'y)))
(libcomp-pc rs-count-negative-bound
  (libcomp-pc-buf (goto-char (point-max)) (list (re-search-forward "[0-9]+" 10 t -5) (point))))
(libcomp-pc rs-count-negative-bound-wrong-side
  (libcomp-pc-buf (goto-char 5) (re-search-forward "[0-9]+" 10 t -1)))
(libcomp-pc rs-backward-count
  (libcomp-pc-buf (goto-char (point-max)) (list (re-search-backward "[0-9]+" nil t 3) (point) (match-data t))))
(libcomp-pc rs-count-too-many
  (libcomp-pc-buf (list (re-search-forward "[0-9]+" nil t 9) (point))))
(libcomp-pc rs-count-too-many-move
  (libcomp-pc-buf (list (re-search-forward "[0-9]+" 25 'move 9) (point))))
(libcomp-pc rs-count-negative
  (libcomp-pc-buf (goto-char (point-max)) (list (re-search-forward "[0-9]+" nil t -2) (point) (match-data t))))
(libcomp-pc rs-case-fold-t
  (libcomp-pc-buf (let ((case-fold-search t)) (list (re-search-forward "qux") (match-data t)))))
(libcomp-pc rs-case-fold-nil
  (libcomp-pc-buf (let ((case-fold-search nil)) (list (re-search-forward "foo" nil t 2) (point)))))
(libcomp-pc rs-case-fold-upper
  (libcomp-pc-buf (let ((case-fold-search t)) (list (re-search-forward "FOO" nil t 2) (point)))))
(libcomp-pc rs-narrowed
  (libcomp-pc-buf (narrow-to-region 8 25) (goto-char (point-min))
                  (let (r) (while (re-search-forward "[0-9]+" nil t) (push (point) r)) (nreverse r))))
(libcomp-pc rs-narrowed-bound
  (libcomp-pc-buf (narrow-to-region 8 25) (goto-char (point-min)) (re-search-forward "Qux" 40 t)))
(libcomp-pc rs-empty-match (libcomp-pc-buf (list (re-search-forward "x*") (point) (match-data t))))
(libcomp-pc rs-anchors
  (libcomp-pc-buf (let (r) (while (re-search-forward "^[a-zA-Z]+" nil t) (push (match-string 0) r)) (nreverse r))))
(libcomp-pc rs-eol (libcomp-pc-buf (list (re-search-forward "[0-9]$") (point))))
(libcomp-pc rs-at-end (libcomp-pc-buf (goto-char (point-max)) (re-search-forward "" nil t)))
(libcomp-pc rs-backward-then-forward
  (libcomp-pc-buf (goto-char (point-max)) (re-search-backward "[0-9]+")
                  (list (point) (match-data t) (re-search-forward "[0-9]+" nil t) (match-data t))))
(libcomp-pc rs-then-string-match
  (libcomp-pc-buf (re-search-forward "bar") (list (string-match "a\\(b\\)" "xxab") (match-data t))))
(libcomp-pc rs-after-edit
  (libcomp-pc-buf (re-search-forward "12") (insert "99") (goto-char (point-min))
                  (list (re-search-forward "[0-9]+") (match-string 0))))
(libcomp-pc rs-replace-match
  (libcomp-pc-buf (re-search-forward "\\([0-9]+\\)") (replace-match "<\\1>") (buffer-string)))
(libcomp-pc rs-save-match-data
  (libcomp-pc-buf (re-search-forward "bar")
                  (save-match-data (re-search-forward "[0-9]+"))
                  (match-data t)))
(libcomp-pc rs-not-string (libcomp-pc-buf (re-search-forward 'foo nil t)))
(libcomp-pc rs-search-forward-regexp-alias
  (libcomp-pc-buf (list (search-forward-regexp "[0-9]+" nil t 2) (point))))
(libcomp-pc rs-multibyte
  (with-temp-buffer (insert "aé1 βγ22 x") (goto-char 1)
                    (list (re-search-forward "[0-9]+" nil t 2) (point) (match-data t))))
(libcomp-pc rs-looking-at
  (libcomp-pc-buf (re-search-forward "foo ") (list (looking-at "[0-9]+") (match-data t))))
(libcomp-pc rs-other-buffer-match-data
  (let ((b (generate-new-buffer " libcomp-pc-o")))
    (unwind-protect
        (progn (with-current-buffer b (insert "abc 42") (goto-char 1) (re-search-forward "[0-9]+"))
               (list (match-beginning 0) (match-end 0)))
      (kill-buffer b))))

;; ---- markers ----
(libcomp-pc mk-copy-int
  (libcomp-pc-buf (let ((m (copy-marker 5))) (list m (marker-position m) (eq (marker-buffer m) (current-buffer))))))
(libcomp-pc mk-copy-type (libcomp-pc-buf (list (copy-marker 5 t) (copy-marker 5 nil) (copy-marker 5 'other))))
(libcomp-pc mk-copy-marker
  (libcomp-pc-buf (let* ((a (copy-marker 7 t)) (b (copy-marker a))) (list b (eq a b)))))
(libcomp-pc mk-copy-nil (copy-marker nil))
(libcomp-pc mk-copy-nowhere (copy-marker (make-marker)))
(libcomp-pc mk-copy-clamp (libcomp-pc-buf (list (copy-marker 0) (copy-marker 99999))))
(libcomp-pc mk-copy-bad (copy-marker "x"))
(libcomp-pc mk-copy-float (libcomp-pc-buf (copy-marker 3.0)))
(libcomp-pc mk-insert-types
  (libcomp-pc-buf (let ((a (copy-marker 5)) (b (copy-marker 5 t)))
                    (goto-char 5) (insert "XY") (list a b))))
(libcomp-pc mk-insert-before
  (libcomp-pc-buf (let ((a (copy-marker 9))) (goto-char 2) (insert "abc") (list a))))
(libcomp-pc mk-delete-region
  (libcomp-pc-buf (let ((a (copy-marker 9)) (b (copy-marker 3)) (c (copy-marker 20)))
                    (delete-region 5 12) (list a b c))))
(libcomp-pc mk-set-nil
  (libcomp-pc-buf (let ((m (copy-marker 4))) (set-marker m nil) (list m (marker-buffer m)))))
(libcomp-pc mk-set-pos
  (libcomp-pc-buf (let ((m (make-marker))) (list (set-marker m 6) (marker-position m)))))
(libcomp-pc mk-set-marker-arg
  (libcomp-pc-buf (let ((m (make-marker)) (n (copy-marker 8))) (set-marker m n) (list m))))
(libcomp-pc mk-set-clamp
  (libcomp-pc-buf (let ((m (make-marker))) (set-marker m -4) (list m (set-marker m 1000)))))
(libcomp-pc mk-set-other-buffer
  (let ((b (generate-new-buffer " libcomp-pc-m")))
    (unwind-protect
        (progn (with-current-buffer b (insert "0123456789"))
               (libcomp-pc-buf (let ((m (copy-marker 3)))
                                 (set-marker m 5 b)
                                 (list m (eq (marker-buffer m) b)))))
      (kill-buffer b))))
(libcomp-pc mk-killed-buffer
  (let ((b (generate-new-buffer " libcomp-pc-k")) m)
    (with-current-buffer b (insert "0123456789") (setq m (copy-marker 4)))
    (kill-buffer b)
    (list (marker-position m) (marker-buffer m) (copy-marker m) (condition-case e (set-marker m 2 b) (error (car e))))))
(libcomp-pc mk-insertion-type-set
  (libcomp-pc-buf (let ((m (copy-marker 5)))
                    (set-marker-insertion-type m t) (goto-char 5) (insert "Z")
                    (list m (marker-insertion-type m)))))
(libcomp-pc mk-point-markers
  (libcomp-pc-buf (goto-char 7) (list (point-marker) (point-min-marker) (point-max-marker))))
(libcomp-pc mk-narrowed
  (libcomp-pc-buf (narrow-to-region 5 10) (list (copy-marker 2) (copy-marker 30) (point-max-marker))))
(libcomp-pc mk-position-bad (marker-position 3))
(libcomp-pc mk-loop
  (libcomp-pc-buf (let ((a 0)) (dotimes (i 300) (let ((m (copy-marker (1+ (% i 40)))))
                                                   (setq a (+ a (marker-position m))) (set-marker m nil)))
                    a)))
(libcomp-pc mk-arith (libcomp-pc-buf (let ((m (copy-marker 5))) (list (+ m 1) (1+ m) (< m 6) (= m 5)))))
(libcomp-pc mk-buffer-of-copy
  (libcomp-pc-buf (let ((m (copy-marker 4))) (list (eq (marker-buffer (copy-marker m t)) (current-buffer))))))
(libcomp-pc mk-set-same-twice
  (libcomp-pc-buf (let ((m (copy-marker 4))) (set-marker m 6) (set-marker m 6) (list m))))
(libcomp-pc mk-goto (libcomp-pc-buf (let ((m (copy-marker 9))) (goto-char m) (point))))

;; ---- run-hooks ----
(defvar libcomp-pc-log nil)
(defvar libcomp-pc-hook nil)
(defvar libcomp-pc-hook2 nil)
(defun libcomp-pc-h1 () (push 'h1 libcomp-pc-log) 1)
(defun libcomp-pc-h2 () (push 'h2 libcomp-pc-log) nil)
(defun libcomp-pc-hg () (push 'global libcomp-pc-log) 'g)
(defun libcomp-pc-hl () (push 'local libcomp-pc-log) 'l)
(defmacro libcomp-pc-hooks (&rest body)
  `(let ((libcomp-pc-log nil) (libcomp-pc-hook nil) (libcomp-pc-hook2 nil))
     (list (progn ,@body) (reverse libcomp-pc-log))))
(libcomp-pc rh-list
  (libcomp-pc-hooks (setq libcomp-pc-hook (list #'libcomp-pc-h1 #'libcomp-pc-h2 #'libcomp-pc-h1))
                    (run-hooks 'libcomp-pc-hook)))
(libcomp-pc rh-single-function
  (libcomp-pc-hooks (setq libcomp-pc-hook #'libcomp-pc-h2) (run-hooks 'libcomp-pc-hook)))
(libcomp-pc rh-lambda
  (libcomp-pc-hooks (setq libcomp-pc-hook (list (lambda () (push 'lam libcomp-pc-log))))
                    (run-hooks 'libcomp-pc-hook)))
(libcomp-pc rh-nil (libcomp-pc-hooks (run-hooks 'libcomp-pc-hook)))
(libcomp-pc rh-unbound (libcomp-pc-hooks (run-hooks 'libcomp-pc-unbound-hook)))
(libcomp-pc rh-several
  (libcomp-pc-hooks (setq libcomp-pc-hook (list #'libcomp-pc-h1) libcomp-pc-hook2 (list #'libcomp-pc-h2))
                    (run-hooks 'libcomp-pc-hook 'libcomp-pc-hook2 'libcomp-pc-hook)))
(libcomp-pc rh-add-hook-order
  (libcomp-pc-hooks (add-hook 'libcomp-pc-hook #'libcomp-pc-h1) (add-hook 'libcomp-pc-hook #'libcomp-pc-h2)
                    (add-hook 'libcomp-pc-hook #'libcomp-pc-hg t)
                    (list libcomp-pc-hook (run-hooks 'libcomp-pc-hook))))
(libcomp-pc rh-local-with-t
  (libcomp-pc-hooks
   (with-temp-buffer
     (add-hook 'libcomp-pc-hook #'libcomp-pc-hg)
     (add-hook 'libcomp-pc-hook #'libcomp-pc-hl nil t)
     (list (local-variable-p 'libcomp-pc-hook) libcomp-pc-hook (run-hooks 'libcomp-pc-hook)))))
(libcomp-pc rh-local-without-t
  (libcomp-pc-hooks
   (with-temp-buffer
     (add-hook 'libcomp-pc-hook #'libcomp-pc-hg)
     (setq-local libcomp-pc-hook (list #'libcomp-pc-hl))
     (run-hooks 'libcomp-pc-hook))))
(libcomp-pc rh-local-t-first
  (libcomp-pc-hooks
   (with-temp-buffer
     (add-hook 'libcomp-pc-hook #'libcomp-pc-hg)
     (setq-local libcomp-pc-hook (list t #'libcomp-pc-hl))
     (run-hooks 'libcomp-pc-hook))))
(libcomp-pc rh-with-args
  (libcomp-pc-hooks (setq libcomp-pc-hook (list (lambda (a b) (push (list a b) libcomp-pc-log))))
                    (run-hook-with-args 'libcomp-pc-hook 1 2)))
(libcomp-pc rh-until-success
  (libcomp-pc-hooks (setq libcomp-pc-hook (list #'libcomp-pc-h2 #'libcomp-pc-h1 #'libcomp-pc-hg))
                    (run-hook-with-args-until-success 'libcomp-pc-hook)))
(libcomp-pc rh-until-failure
  (libcomp-pc-hooks (setq libcomp-pc-hook (list #'libcomp-pc-h1 #'libcomp-pc-h2 #'libcomp-pc-hg))
                    (run-hook-with-args-until-failure 'libcomp-pc-hook)))
(libcomp-pc rh-edit-during-run
  (libcomp-pc-hooks (setq libcomp-pc-hook
                          (list (lambda () (push 'first libcomp-pc-log)
                                  (remove-hook 'libcomp-pc-hook #'libcomp-pc-h1))
                                #'libcomp-pc-h1))
                    (run-hooks 'libcomp-pc-hook)))
(libcomp-pc rh-error-in-hook
  (libcomp-pc-hooks (setq libcomp-pc-hook (list #'libcomp-pc-h1 (lambda () (error "boom")) #'libcomp-pc-h2))
                    (condition-case e (run-hooks 'libcomp-pc-hook) (error (cadr e)))))
(libcomp-pc rh-not-symbol (run-hooks "libcomp-pc-hook"))
(libcomp-pc rh-loop
  (libcomp-pc-hooks (add-hook 'libcomp-pc-hook #'libcomp-pc-h1) (add-hook 'libcomp-pc-hook #'ignore)
                    (let ((a 0)) (dotimes (_ 200) (run-hooks 'libcomp-pc-hook) (setq a (1+ a))) a)))

;; ---- cl-generic ----
(cl-defgeneric libcomp-pc-g (x))
(cl-defmethod libcomp-pc-g ((x integer)) (list 'integer x))
(cl-defmethod libcomp-pc-g ((x string)) (list 'string (length x)))
(cl-defmethod libcomp-pc-g ((x symbol)) (list 'symbol x))
(cl-defmethod libcomp-pc-g ((x null)) (list 'null (cl-call-next-method)))
(cl-defmethod libcomp-pc-g ((x cons)) (list 'cons (car x)))
(cl-defmethod libcomp-pc-g ((_x (eql 7))) (list 'seven (cl-call-next-method)))
(cl-defmethod libcomp-pc-g ((x (head tag))) (list 'head (cadr x) (cl-next-method-p) (cl-call-next-method)))
(cl-defmethod libcomp-pc-g ((x t)) (list 'default (type-of x)))
(libcomp-pc gen-types
  (mapcar #'libcomp-pc-g (list 1 -5 "abc" 'foo nil '(1 2) 7 '(tag 9) 1.5 [1 2] (make-hash-table))))
(libcomp-pc gen-loop (let ((a 0)) (dotimes (_ 300) (setq a (+ a (length (libcomp-pc-g "abc"))))) a))
(libcomp-pc gen-keyword (libcomp-pc-g :kw))
(libcomp-pc gen-t (libcomp-pc-g t))
(libcomp-pc gen-bignum (libcomp-pc-g (expt 2 70)))
(cl-defgeneric libcomp-pc-q (x))
(defvar libcomp-pc-qlog nil)
(cl-defmethod libcomp-pc-q ((x integer)) (push (list 'primary x) libcomp-pc-qlog) (* 2 x))
(cl-defmethod libcomp-pc-q ((x number)) (push (list 'number x) libcomp-pc-qlog) 'num)
(cl-defmethod libcomp-pc-q :before ((x integer)) (push (list 'before x) libcomp-pc-qlog))
(cl-defmethod libcomp-pc-q :after ((x number)) (push (list 'after x) libcomp-pc-qlog))
(cl-defmethod libcomp-pc-q :around ((x integer))
  (push (list 'around-in x) libcomp-pc-qlog)
  (let ((r (cl-call-next-method (1+ x)))) (push (list 'around-out r) libcomp-pc-qlog) (list 'wrapped r)))
(libcomp-pc gen-qualifiers
  (let ((libcomp-pc-qlog nil)) (list (libcomp-pc-q 3) (reverse libcomp-pc-qlog))))
(libcomp-pc gen-qualifiers-float
  (let ((libcomp-pc-qlog nil)) (list (libcomp-pc-q 2.5) (reverse libcomp-pc-qlog))))
(libcomp-pc gen-no-applicable (libcomp-pc-q "x"))
(cl-defgeneric libcomp-pc-only (x))
(cl-defmethod libcomp-pc-only ((x integer)) (cl-call-next-method x))
(libcomp-pc gen-no-next
  ;; GNU's signal data carries its internal method object: compare the generic only.
  (condition-case e (libcomp-pc-only 1) (cl-no-next-method (list (car e) (cadr e)))))
(cl-defgeneric libcomp-pc-r (x))
(cl-defmethod libcomp-pc-r ((x integer)) (list 'one x))
(libcomp-pc gen-redefine
  (list (libcomp-pc-r 1)
        (progn (cl-defmethod libcomp-pc-r ((x integer)) (list 'two x)) (libcomp-pc-r 1))
        (progn (cl-defmethod libcomp-pc-r ((x (eql 1))) (list 'eql x)) (libcomp-pc-r 1))
        (libcomp-pc-r 2)))
(cl-defstruct libcomp-pc-animal name)
(cl-defstruct (libcomp-pc-dog (:include libcomp-pc-animal)) breed)
(cl-defgeneric libcomp-pc-s (x))
(cl-defmethod libcomp-pc-s ((x libcomp-pc-animal)) (list 'animal (libcomp-pc-animal-name x)))
(cl-defmethod libcomp-pc-s ((x libcomp-pc-dog)) (list 'dog (libcomp-pc-dog-breed x) (cl-call-next-method)))
(libcomp-pc gen-struct
  (list (libcomp-pc-s (make-libcomp-pc-animal :name "a"))
        (libcomp-pc-s (make-libcomp-pc-dog :name "d" :breed "b"))))
(cl-defgeneric libcomp-pc-m (x y))
(cl-defmethod libcomp-pc-m ((x integer) (y string)) (list 'int-str x y))
(cl-defmethod libcomp-pc-m ((x integer) (y integer)) (list 'int-int (+ x y)))
(cl-defmethod libcomp-pc-m ((x t) (y t)) (list 'any))
(libcomp-pc gen-multi (list (libcomp-pc-m 1 "s") (libcomp-pc-m 1 2) (libcomp-pc-m "a" 2)))
(libcomp-pc gen-eql-symbol
  (progn (cl-defgeneric libcomp-pc-e (x))
         (cl-defmethod libcomp-pc-e ((_x (eql foo))) 'is-foo)
         (cl-defmethod libcomp-pc-e ((x symbol)) (list 'sym x))
         (list (libcomp-pc-e 'foo) (libcomp-pc-e 'bar))))

(princ "LIBCOMP-PARITY-END\n")
(kill-emacs 0)
;;; nelisp-libcomp-parity-corpus.el ends here
