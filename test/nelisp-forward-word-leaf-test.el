;;; nelisp-forward-word-leaf-test.el --- host model of the native forward-word scan -*- lexical-binding: t; -*-

;; `nl_fw_scan' (lisp/nelisp-cc-fx-call-primitives.el) is GNU scan_words for ASCII text: explicit
;; user instruction, and GNU implements it in C (syntax.c Fforward_word/scan_words).  Its body is
;; evaluated here on the host against a memory table, with the text and syntax stubs replaced by
;; host functions, and compared with host Emacs's own `forward-word' over many texts, points,
;; counts and narrowings (point written back and the value word).

(require 'ert)
(require 'cl-lib)
(require 'nelisp-cc-fx-call-primitives)

(defvar nfw--mem nil)
(defvar nfw--text "")
(defvar nfw--dirty 0)
(defvar nfw--ft-nil t "Non-nil: the boundary-table lookup answers nil (word 3); else declines (0).")

(defun nfw--defun (name)
  (let ((form (cl-find-if (lambda (f) (and (consp f) (eq (car f) 'defun) (eq (cadr f) name)))
                          (cdr nelisp-cc-fx-call-primitives--source))))
    (or form (error "no defun %s" name))))

(defmacro nfw--with (&rest body)
  `(progn
     (cl-letf* (((symbol-function 'ptr-read-u64) (lambda (p i) (gethash (+ p i) nfw--mem 0)))
                ((symbol-function 'ptr-write-u64) (lambda (p i v) (puthash (+ p i) v nfw--mem)))
                ((symbol-function 'shl) (lambda (x n) (ash x n)))
                ((symbol-function 'sar) (lambda (x n) (ash x (- n))))
                ((symbol-function 'wf_dirty) (lambda () (cl-incf nfw--dirty)))
                ((symbol-function 'nl_gb_at) (lambda (_r _d i) (aref nfw--text i)))
                ((symbol-function 'nl_lct_owner_contents) (lambda (_t _e) 0))
                ((symbol-function 'nl_lct_owner_ref) (lambda (_t _c _e) (if nfw--ft-nil 3 0)))
                ((symbol-function 'nl_fw_cls)
                 (lambda (_t _k c _e) (if (eq (char-syntax c) ?w) 1 0))))
       ,@body)))

(defmacro seq (&rest forms) (cons 'progn forms))

(defun nfw--b (x) (if x 1 0))

(defun nfw--xform (f)
  "The DSL form F as host Lisp: comparisons answer 1/0 and `if'/`while' test for non-zero."
  (cond
   ((not (consp f)) f)
   ((memq (car f) '(= /= < > <= >=)) (list 'nfw--b (cons (car f) (mapcar #'nfw--xform (cdr f)))))
   ((eq (car f) 'if)
    `(if (/= 0 ,(nfw--xform (cadr f))) ,@(mapcar #'nfw--xform (cddr f))))
   ((eq (car f) 'while)
    `(while (/= 0 ,(nfw--xform (cadr f))) ,@(mapcar #'nfw--xform (cddr f))))
   ((eq (car f) 'let*)
    `(let* ,(mapcar (lambda (b) (list (car b) (nfw--xform (cadr b)))) (cadr f))
       ,@(mapcar #'nfw--xform (cddr f))))
   (t (cons (car f) (mapcar #'nfw--xform (cdr f))))))

(eval (nfw--xform (nfw--defun 'nl_fw_scan)) t)

(defun nfw--run (text pt lo hi cnt ft)
  "Run `nl_fw_scan' on TEXT: (POINT . VALUE-WORD), or `decline'."
  (setq nfw--mem (make-hash-table :test #'eql) nfw--text text nfw--dirty 0)
  (let ((out 100) (r 1000) (resw 2000))
    (puthash (+ out 0) 1 nfw--mem) (puthash (+ out 8) (if ft 1 0) nfw--mem)
    (puthash (+ out 16) (1- (length text)) nfw--mem)
    (puthash (+ out 24) pt nfw--mem) (puthash (+ out 32) lo nfw--mem)
    (puthash (+ out 40) hi nfw--mem) (puthash (+ out 48) cnt nfw--mem)
    (puthash (+ r 104) 7 nfw--mem)
    (nfw--with
     (let ((rc (nl_fw_scan r 3000 0 resw out)))
       (if (/= rc 0) 'decline
         (let ((pw (gethash (+ r 104) nfw--mem)))
           (cons (if (= pw 3) (1+ (gethash (+ out 16) nfw--mem)) (ash pw -2))
                 (gethash resw nfw--mem))))))))

(defun nfw--host (text pt lo hi cnt)
  (with-temp-buffer
    (insert text)
    (narrow-to-region lo hi)
    (goto-char pt)
    (let ((v (forward-word cnt)))
      (cons (point) (if v 7 3)))))

(ert-deftest nfw-scan-matches-host ()
  (let ((texts '("" "a" "foo bar" "  foo  bar_baz-qux 12 ab\nxy\tz  " "..." "foo" "a b c d e f"))
        (n 0))
    (dolist (text texts)
      (let ((len (length text)))
        (dolist (lo (number-sequence 1 (1+ len)))
          (dolist (hi (number-sequence lo (1+ len)))
            (dolist (pt (number-sequence lo hi))
              (dolist (cnt '(0 1 -1 2 -2 3 -3 7 -7 100 -100))
                (let ((want (nfw--host text pt lo hi cnt))
                      (got (nfw--run text pt lo hi cnt nil)))
                  (cl-incf n)
                  (should (equal want got)))))))))
    (should (> n 1000))))

(ert-deftest nfw-scan-declines ()
  ;; a non-ASCII character looked at, an unknown entry, a non-nil boundary-table entry
  (should (eq 'decline (nfw--run "abあc" 1 1 5 1 nil)))
  (let ((nfw--ft-nil nil))
    (should (eq 'decline (nfw--run "ab cd" 1 1 6 1 t)))))

(ert-deftest nfw-scan-boundary-table-nil-entries-pass ()
  ;; table present, every entry nil: same result as without
  (should (equal (nfw--run "ab cd" 1 1 6 2 nil) (nfw--run "ab cd" 1 1 6 2 t))))

(provide 'nelisp-forward-word-leaf-test)
