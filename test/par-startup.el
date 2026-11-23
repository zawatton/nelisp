;;; -*- lexical-binding: t; -*-
;; Startup-fix parity rows (F278); GNU 31.1 is the spec.
(defmacro row (name form) `(princ (format "%-24s %S\n" ,name (condition-case e ,form (error (list 'ERR e))))))
(defun st-bare (f) (let ((e (aref f 2)) (n 0)) (dolist (x e) (when (and x (symbolp x) (not (eq x t))) (setq n (1+ n)))) n))
(defvar st-sp1 1)
(defvar st-sp2 2)
;; (a) bodyless defvar of an already-special var pushes no marker; behaviour unchanged
(row "a dyn let in closure" (let ((f (let () (defvar st-sp1) (lambda () (let ((st-sp1 7)) (symbol-value 'st-sp1)))))) (list (funcall f) st-sp1)))
(row "a marker count special" (st-bare (let () (defvar st-sp1) (lambda () 1))))
(row "a marker count nonspecial" (st-bare (let () (defvar st-ns1) (lambda () 1))))
(row "a callee sees dynamic" (let ((f (let () (defvar st-sp2) (lambda () (funcall (lambda () (let ((st-sp2 9)) (symbol-value 'st-sp2)))))))) (list (funcall f) st-sp2)))
;; locally special let after a marker (non-special var) stays dynamic
(row "b local special let" (let ((f (let () (defvar st-ns2) (lambda () (let ((st-ns2 5)) (symbol-value 'st-ns2)))))) (list (funcall f) (condition-case nil (let ((st-ns2 1)) (symbol-value 'st-ns2)) (void-variable 'void)))))
(row "b special + local mix" (let ((f (let () (defvar st-sp1) (defvar st-ns3) (lambda () (let ((st-sp1 3) (st-ns3 4)) (list (symbol-value 'st-sp1) (symbol-value 'st-ns3))))))) (list (funcall f) (funcall f) st-sp1)))
;; eval with alist: special names skipped, non-special kept, uninterned name never confused with a special one
(row "c eval alist special" (eval '(let ((st-sp1 8)) (symbol-value 'st-sp1)) '(st-sp1 st-sp2 st-ns4 t)))
(row "c eval alist nonspecial" (eval '(let ((st-ns4 8)) (symbol-value 'st-ns4)) '(st-sp1 st-ns4 st-ns4 t)))
(row "c eval alist pair+marker" (eval '(let ((st-ns5 8)) (list x (symbol-value 'st-ns5))) '(st-sp1 (x . 1) st-ns5 t)))
(row "c uninterned same name" (let ((u (make-symbol "st-sp1"))) (eval `(let ((,u 8)) (condition-case nil (symbol-value ',u) (void-variable 'void))) (list u t))))
;; defvar made special AFTER the alist eval must not leave a stale decision
(row "c special after" (list (eval '(let ((st-late 1)) (condition-case nil (symbol-value 'st-late) (void-variable 'void))) '(st-late t)) (progn (defvar st-late 0) (eval '(let ((st-late 2)) (symbol-value 'st-late)) '(st-late t)))))
;; eval-and-compile style: long macroexp--dynvars-like alist (mostly special) then a closure made inside
(defvar st-d1 1) (defvar st-d2 2) (defvar st-d3 3)
(row "d eval-and-compile closure" (let* ((al (append (mapcar (lambda (i) (intern (format "st-d%d" (1+ (% i 3)))) ) (number-sequence 1 60)) '(st-ns6 t))) (f (eval '(lambda () (let ((st-ns6 6) (st-d1 7)) (list (symbol-value 'st-ns6) (symbol-value 'st-d1)))) al))) (list (funcall f) (funcall f) st-d1)))
;; bundle-style long single load: many bodyless defvars (special and not) in ONE file, then a closure
(defvar st-load-file (make-temp-file "st-bundle" nil ".el"))
(with-temp-file st-load-file
  (insert ";;; -*- lexical-binding: t; -*-\n(defvar st-b-special 1)\n")
  (dotimes (i 80) (insert (format "(defvar st-b-ns%d)\n(defvar st-b-special)\n(eval-and-compile (defvar st-b-ea%d 1))\n" i i)))
  (insert "(setq st-b-f (lambda () (list (let ((st-b-ns79 5)) (symbol-value 'st-b-ns79)) (let ((st-b-special 6)) (symbol-value 'st-b-special)) (let ((st-b-ea3 7)) (symbol-value 'st-b-ea3)))))\n(setq st-b-g (let ((q 1)) (lambda () (let ((st-b-ns3 q)) (symbol-value 'st-b-ns3)))))\n"))
(defvar st-b-f nil) (defvar st-b-g nil)
(load st-load-file nil t)
(row "e bundle closure" (list (funcall st-b-f) (funcall st-b-g) st-b-special))
(row "e bundle marker count" (st-bare st-b-f))
(delete-file st-load-file)
