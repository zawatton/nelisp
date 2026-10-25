;;; nelisp-hotcold-test.el --- Hot/cold block layout (hotcold) -*- lexical-binding: t; -*-
;; Cold-by-construction arms (a store that ends a guarded dispatch loop) are emitted behind the
;; function's hot code, with the branch sense chosen so that the hot arm falls through.
(require 'ert)
(require 'cl-lib)
(require 'nelisp-aot-compiler)

(defvar nelisp-aot-compiler-cold-layout)
(defvar nelisp-aot-compiler--cold-last-count)
(defvar nelisp-aot-compiler-vm-register-functions)
(defvar nelisp-aot-compiler--x86-frame-copy-aliases)

(defun hotcold-test--vm (extra)
  "A dispatch loop with all three decline shapes; EXTRA is a list of dead stores for the cold arm."
  `(defun hc_vm (n m)
     (let* ((go 128) (i 0) (acc 0) (fail 0) (k 0) (pad 0))
       (seq
        (while (> go 0)
          ;; A: the decline is the then arm
          (if (>= i n) (setq go 0)
            (seq
             (setq k (logand i 7))
             (if (= k 3) (setq acc (+ acc 5)) (setq acc (+ acc k)))
             ;; C: no else arm, the decline is the then arm
             (if (= i m) (seq (setq fail 1) ,@extra (setq go 0)) 0)
             ;; B: the decline is the else arm
             (if (< acc 60) (setq i (+ i 1)) (seq ,@extra (setq go 0))))))
        (+ acc (* 31 fail) (* 7 i))))))

(defun hotcold-test--model (n m)
  (let ((go t) (i 0) (acc 0) (fail 0))
    (while go
      (if (>= i n) (setq go nil)
        (let ((k (logand i 7)))
          (setq acc (+ acc (if (= k 3) 5 k)))
          (when (= i m) (setq fail 1 go nil))
          (if (< acc 60) (setq i (1+ i)) (setq go nil)))))
    (+ acc (* 31 fail) (* 7 i))))

(defun hotcold-test--run (n m &optional policy optin)
  "Compile and run the dispatch loop natively; return its exit code."
  (let ((path (make-temp-file "hotcold-e2e"))
        (nelisp-aot-compiler--x86-frame-copy-aliases nil)
        (nelisp-aot-compiler-vm-register-functions optin)
        (nelisp-aot-compiler-cold-layout policy))
    (unwind-protect
        (progn
          (nelisp-aot-compile-sexp
           `(seq ,(hotcold-test--vm nil) (exit (hc_vm ,n ,m))) path)
          (call-process path nil nil nil))
      (when (file-exists-p path) (delete-file path)))))

(defun hotcold-test--unit (extra &optional policy)
  "Return (LABELS . TEXT) of hc_vm compiled alone with the labels sorted by offset."
  (let* ((nelisp-aot-compiler--x86-frame-copy-aliases nil)
         (nelisp-aot-compiler--allow-external-user-calls t)
         (nelisp-aot-compiler--abi 'sysv)
         (nelisp-aot-compiler--arch 'x86_64)
         (nelisp-aot-compiler--label-counter 0)
         (nelisp-aot-compiler-cold-layout policy)
         (ir (nelisp-aot-compiler--parse `(seq ,(hotcold-test--vm extra)) nil))
         (defuns (nelisp-aot-compiler--collect-defuns ir))
         (buf (nelisp-asm-x86_64-make-buffer 'sysv)))
    (dolist (d defuns) (nelisp-aot-compiler--emit-defun d buf))
    (let ((labels (copy-sequence (nelisp-asm-x86_64-buffer-labels buf)))
          (text (nelisp-asm-x86_64-resolve-fixups buf)))
      (cons (sort labels (lambda (a b) (< (cdr a) (cdr b)))) text))))

(defun hotcold-test--dead-stores (n)
  (cl-loop for j from 0 below n collect `(setq pad ,(+ 1000 j))))

(ert-deftest hotcold/e2e-same-result-on-and-off ()
  (skip-unless (and (eq system-type 'gnu/linux) (string-match-p "x86_64" system-configuration)))
  (dolist (nm '((3 99) (40 99) (40 5) (40 2) (7 6) (0 0) (12 11)))
    (let ((want (mod (hotcold-test--model (car nm) (cadr nm)) 256)))
      (should (= (hotcold-test--run (car nm) (cadr nm) nil) want))
      (should (= (hotcold-test--run (car nm) (cadr nm) t) want)))))

(ert-deftest hotcold/e2e-with-promoted-vm-registers ()
  "The deferred arms run under the promoted-register state they were queued in."
  (skip-unless (and (eq system-type 'gnu/linux) (string-match-p "x86_64" system-configuration)))
  (dolist (nm '((40 99) (40 5) (7 6)))
    (let ((want (mod (hotcold-test--model (car nm) (cadr nm)) 256)))
      (should (= (hotcold-test--run (car nm) (cadr nm) t '((hc_vm i acc go k))) want)))))

(ert-deftest hotcold/declines-are-deferred-and-off-switch-defers-nothing ()
  (hotcold-test--unit nil t)
  (should (= 3 nelisp-aot-compiler--cold-last-count))
  (hotcold-test--unit nil nil)
  (should (= 0 nelisp-aot-compiler--cold-last-count)))

(ert-deftest hotcold/hot-labels-do-not-move-when-cold-code-grows ()
  (let* ((small (hotcold-test--unit (hotcold-test--dead-stores 1) t))
         (large (hotcold-test--unit (hotcold-test--dead-stores 40) t))
         (cold (lambda (u) (cdr (assq 'hc_vm\$cold (car u))))))
    (should (funcall cold small))
    (should (funcall cold large))
    (should (= (funcall cold small) (funcall cold large)))
    (dolist (l (car small))
      (when (< (cdr l) (funcall cold small))
        (should (eql (cdr l) (cdr (assq (car l) (car large)))))))
    ;; the cold code really grew
    (should (> (length (cdr large)) (+ 100 (length (cdr small)))))))

(ert-deftest hotcold/seed-layout-moves-with-cold-code ()
  "Negative control: with the layout off the same growth moves hot labels."
  (let* ((small (hotcold-test--unit (hotcold-test--dead-stores 1) nil))
         (large (hotcold-test--unit (hotcold-test--dead-stores 40) nil))
         (moved (cl-count-if (lambda (l) (not (eql (cdr l) (cdr (assq (car l) (car large))))))
                             (car small))))
    (should (> moved 3))))

(ert-deftest hotcold/hot-code-is-denser-with-cold-layout ()
  (let* ((on (hotcold-test--unit (hotcold-test--dead-stores 6) t))
         (off (hotcold-test--unit (hotcold-test--dead-stores 6) nil))
         (hot-on (cdr (assq 'hc_vm\$cold (car on))))
         (windows (lambda (bytes) (/ (+ bytes 63) 64))))
    (should hot-on)
    (should (< hot-on (length (cdr off))))
    (should (< (funcall windows hot-on) (funcall windows (length (cdr off)))))))

(ert-deftest hotcold/hot-arm-falls-through ()
  "The conditional branch to a decline is a forward jump into the cold section."
  (let* ((u (hotcold-test--unit nil t))
         (text (cdr u))
         (cold (cdr (assq 'hc_vm\$cold (car u))))
         (hits 0))
    ;; jcc rel32 = 0F 8x d32; count those whose target lies in the cold section.
    (cl-loop for p from 0 below (- cold 6)
             when (and (= (aref text p) #x0F) (<= #x80 (aref text (1+ p)) #x8F))
             do (let* ((d (logior (aref text (+ p 2)) (ash (aref text (+ p 3)) 8)
                                  (ash (aref text (+ p 4)) 16) (ash (aref text (+ p 5)) 24)))
                       (d (if (>= d #x80000000) (- d #x100000000) d))
                       (target (+ p 6 d)))
                  (when (>= target cold) (cl-incf hits))))
    (should (>= hits 3))))

(defconst hotcold-test--val-src
  '(seq
    (defun nl_string_read64 (p off) (+ (* p 3) off))
    (defun hc_val (n)
      (let ((i 0) (acc 0))
        (seq
         (while (< i n)
           (seq
            (let* ((g i))
              (setq acc (+ acc (if (= (logand g 7) 4) (nl_string_read64 g 8) (+ g 8)))))
            (let* ((g (+ i 1)))
              (setq acc (+ acc (if (/= (logand g 7) 4) (+ g 9) (nl_string_read64 g 9)))))
            (setq i (+ i 1))))
         acc))))
  "Value-position guarded reads: the call is the then arm, and the else arm.")

(defun hotcold-test--val-model (n)
  (let ((acc 0))
    (dotimes (i n)
      (setq acc (+ acc (if (= (logand i 7) 4) (+ (* i 3) 8) (+ i 8))))
      (let ((g (1+ i)))
        (setq acc (+ acc (if (/= (logand g 7) 4) (+ g 9) (+ (* g 3) 9))))))
    acc))

(defun hotcold-test--run-val (n policy)
  (let ((path (make-temp-file "hotcold-val"))
        (nelisp-aot-compiler--x86-frame-copy-aliases nil)
        (nelisp-aot-compiler-cold-layout policy))
    (unwind-protect
        (progn
          (nelisp-aot-compile-sexp
           `(seq ,@(cdr hotcold-test--val-src) (exit (hc_val ,n))) path)
          (call-process path nil nil nil))
      (when (file-exists-p path) (delete-file path)))))

(ert-deftest hotcold/value-position-guarded-read-call-is-deferred ()
  (skip-unless (and (eq system-type 'gnu/linux) (string-match-p "x86_64" system-configuration)))
  (dolist (n '(0 1 5 6 13 40))
    (let ((want (mod (hotcold-test--val-model n) 256)))
      (should (= (hotcold-test--run-val n nil) want))
      (should (= (hotcold-test--run-val n t) want))))
  (let ((nelisp-aot-compiler--x86-frame-copy-aliases nil)
        (nelisp-aot-compiler--allow-external-user-calls t)
        (nelisp-aot-compiler--abi 'sysv) (nelisp-aot-compiler--arch 'x86_64)
        (nelisp-aot-compiler-cold-layout t)
        (buf (nelisp-asm-x86_64-make-buffer 'sysv)))
    (dolist (d (nelisp-aot-compiler--collect-defuns
                (nelisp-aot-compiler--parse hotcold-test--val-src nil)))
      (nelisp-aot-compiler--emit-defun d buf))
    (should (= 2 nelisp-aot-compiler--cold-last-count))))

(provide 'nelisp-hotcold-test)
