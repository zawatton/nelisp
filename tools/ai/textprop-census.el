;;; textprop-census.el --- tier of every function on the attrib10x hot paths  -*- lexical-binding: t; -*-
;; usage (repository root): BINARY --load tools/ai/textprop-census.el
;; Optional: LIBCOMP_ROWS="row1 row2" (default: all rows below), LIBCOMP_DEPTH (default 6).
;;
;; For each attrib10x representative row: run the row's work once (so lazily
;; compiled records exist), then walk the static call tree from the row's
;; roots, breadth first.  One line per function reached:
;;   TREE ROW NAME DEPTH KIND REASON
;; KIND
;;   native    fx record lowered to native code (NLOCALS <= -16)
;;   fx        fx record run by the word VM; REASON no-row (no harvested
;;             native row) or row-stale (a row exists, the install declined it)
;;   decl      closure the fx compiler declines; REASON env-captured /
;;             arglist / special-param / unsupported:FORM / error:ERR / unknown
;;   bytecode  byte code (adopted by the byte-code pass, no fx record)
;;   subr / other
;; Callees: fxir call nodes for fx/native records; for byte code and declined
;; closures, the fbound symbols among the constants / body (an upper bound).
;; A final SUMMARY line per row counts the kinds.
;; LIBCOMP_REASON=NAME prints only that function's classification.
(load (expand-file-name "test/nelisp-fxir-eval.el") nil t)

(defvar libcomp--unsupported nil "Form the instrumented compile declined on.")

;; The prelude's `nelisp--fx-compile' with its decline catch recorded, so a
;; declined body names the form it stopped at.
(let ((file (expand-file-name "scripts/nelisp-stdlib-prelude.el")) (found nil))
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((form t))
      (while (and (not found) (setq form (condition-case nil (read (current-buffer)) (end-of-file nil))))
        (when (and (eq (car-safe form) 'defun) (eq (nth 1 form) 'nelisp--fx-compile))
          (setq found form)))))
  (unless found (error "libcomp-census: nelisp--fx-compile not found"))
  (let ((walk nil))
    (setq walk (lambda (x)
                 (cond ((and (consp x) (eq (car x) 'catch)
                             (equal (cadr x) ''nelisp--fx-unsupported))
                        `(let ((r ,x)) (unless (eq r 'ok) (setq libcomp--unsupported r)) r))
                       ((consp x) (cons (funcall walk (car x)) (funcall walk (cdr x))))
                       (t x))))
    (eval `(defun libcomp--fx-compile ,@(cddr (funcall walk found))) t)))

(defun libcomp--decline-reason (fn)
  (let* ((args (nth 2 fn)) (body (nthcdr 3 fn)))
    (cond
     ((not (nelisp--fx-env-ok-p (nth 1 fn))) "env-captured")
     (t
      (let* ((nelisp--fx-declared (remq t (nth 1 fn)))
             (shape (nelisp--fx-arglist-shape args)))
        (cond
         ((null shape) "arglist")
         ((cl-some #'nelisp--fx-special-p (car shape)) "special-param")
         ((nelisp--fx-hopeless-p body) "hopeless-head")
         (t
          (let* ((nelisp--fx-missing nil) (nelisp--fx-called nil)
                 (inhibit-message t)
                 (macros (nelisp--fx-macros-used body))
                 (nelisp--fx-expand (and macros t))
                 (nelisp--fx-outer-env (nth 1 fn))
                 (nelisp--fx-want-ir nil)
                 (libcomp--unsupported nil)
                 (c (condition-case err (libcomp--fx-compile (cons 'progn body) (car shape))
                      (error (setq libcomp--unsupported (list :error err)) nil))))
            (cond
             (c "compiles-now")
             ((eq (car-safe libcomp--unsupported) :error)
              (format "error:%S" (cadr libcomp--unsupported)))
             (libcomp--unsupported
              (let ((print-length 4) (print-level 3))
                (replace-regexp-in-string
                 "[ \n\t]+" "_"
                 (format "unsupported:%S" libcomp--unsupported))))
             (t "unknown"))))))))))

(defun libcomp--row-names ()
  (let ((names nil))
    (dolist (f '("lisp/nelisp-fxir-native-data.el"))
      (when (file-readable-p f)
        (with-temp-buffer
          (insert-file-contents f)
          (goto-char (point-min))
          (let ((form t))
            (while (setq form (condition-case nil (read (current-buffer)) (end-of-file nil)))
              (when (eq (car-safe form) 'defconst)
                (dolist (r (eval (nth 2 form) t)) (push (car r) names))))))))
    names))

(defun libcomp--fbound-symbols (tree)
  (let ((todo (list tree)) (out nil) (seen (make-hash-table :test 'eq)))
    (while todo
      (let ((x (pop todo)))
        (cond ((and (symbolp x) x (not (eq x t)) (fboundp x) (not (gethash x seen)))
               (puthash x t seen) (push x out))
              ((consp x)
               (unless (eq (car x) 'quote)
                 (push (car x) todo) (push (cdr x) todo))
               (when (and (eq (car x) 'quote) (symbolp (cadr x)))
                 (push (cadr x) todo)))
              ((vectorp x) (dotimes (i (length x)) (push (aref x i) todo))))))
    (nreverse out)))

(defun libcomp--bc-callees (fn)
  (let ((c (and (> (length fn) 2) (aref fn 2))))
    (and (vectorp c) (libcomp--fbound-symbols c))))

(defconst libcomp--plan
  ;; (ROW SETUP TRIP ROOTS...): SETUP runs once, TRIP is one survey trip.
  '((tp-gtp
     (setq libcomp-b (with-current-buffer (generate-new-buffer " tpc")
                       (dotimes (_ 25) (insert "abcdefghijklmnopqrstuvwxyz0123456789ABCD"))
                       (dotimes (k 10) (put-text-property (+ 1 (* k 100)) (+ 50 (* k 100)) 'face
                                                          (if (= 0 (% k 2)) 'bold 'italic)))
                       (setq libcomp-i 0)
                       (current-buffer)))
     (with-current-buffer libcomp-b (setq libcomp-i (1+ libcomp-i)) (get-text-property (+ 20 (* 100 (% libcomp-i 5))) 'face))
     get-text-property)
    (tp-tpa nil (with-current-buffer libcomp-b (text-properties-at 120)) text-properties-at)
    (tp-nspc nil (with-current-buffer libcomp-b (next-single-property-change 120 'face)) next-single-property-change)
    (tp-nspc-walk nil (with-current-buffer libcomp-b (let ((p 1)) (while (setq p (next-single-property-change p 'face))))) next-single-property-change)
    (tp-npc-walk nil (with-current-buffer libcomp-b (let ((p 1)) (while (setq p (next-property-change p))))) next-property-change)
    (tp-prev nil (with-current-buffer libcomp-b (previous-single-property-change 420 'face)) previous-single-property-change)
    (tp-any nil (with-current-buffer libcomp-b (text-property-any 1 900 'face 'zzz)) text-property-any)
    (tp-notall nil (with-current-buffer libcomp-b (text-property-not-all 1 40 'face 'bold)) text-property-not-all)
    (tp-pt nil (with-current-buffer libcomp-b (setq libcomp-i (1+ libcomp-i)) (put-text-property 101 120 'face (if (= 0 (% libcomp-i 2)) 'bold 'italic))) put-text-property)
    (tp-atp nil (with-current-buffer libcomp-b (setq libcomp-i (1+ libcomp-i)) (add-text-properties 101 120 (list 'face (if (= 0 (% libcomp-i 2)) 'bold 'italic) 'mouse-face 'highlight))) add-text-properties)
    (tp-rtp nil (with-current-buffer libcomp-b (put-text-property 101 120 'help-echo "x") (remove-text-properties 101 120 '(help-echo nil))) remove-text-properties)
    (tp-rltp nil (with-current-buffer libcomp-b (put-text-property 101 120 'help-echo "x") (remove-list-of-text-properties 101 120 '(help-echo))) remove-list-of-text-properties)
    (tp-propertize nil (propertize "hello world" 'face 'bold 'x 1) propertize)
    (tp-concat nil (concat (propertize "ab" 'face 'bold) (propertize "cd" 'face 'italic)) concat)
    (tp-substr nil (with-current-buffer libcomp-b (buffer-substring 1 400)) buffer-substring)
    (tp-insert nil (with-current-buffer libcomp-b (goto-char 300) (insert (propertize "xy" 'face 'bold)) (delete-region 300 302)) insert)
    (tp-setup-big nil (progn (setq libcomp-big (with-current-buffer (generate-new-buffer " tpbig")
                       (dotimes (_ 2500) (insert "abcdefghijklmnopqrstuvwxyz0123456789ABCD"))
                       (dotimes (k 1000) (put-text-property (+ 1 (* k 100)) (+ 50 (* k 100)) 'face
                                                          (if (= 0 (% k 2)) 'bold 'italic)))
                       (current-buffer)))
                  (setq libcomp-ps (apply (function concat) (let (l) (dotimes (k 500) (push (propertize (format "w%d " k) 'face (if (= 0 (% k 2)) 'bold 'italic)) l)) l)))
                  (setq libcomp-p1 (propertize "hello world " 'face 'bold) libcomp-p3 (propertize "hello world " 'face 'bold)))
     nil)
    (tp-bufstr nil (with-current-buffer libcomp-big (buffer-string)) nelisp--tp-copy-buffer-range-to-string nelisp--tp-copy-clipped nelisp--tp-build)
    (tp-bufsub nil (with-current-buffer libcomp-big (buffer-substring 1000 2000)) nelisp--tp-copy-buffer-range-to-string)
    (tp-concat10 nil (concat libcomp-p1 libcomp-p3 libcomp-p1 libcomp-p3 libcomp-p1 libcomp-p3) nelisp--tp-cat-props)
    (tp-substr2 nil (substring libcomp-ps 100 600) nelisp--tp-sub-props)
    (tp-eqprops nil (equal-including-properties libcomp-p1 libcomp-p3) nelisp--tp-strings-equal)
    (tp-build nil (with-temp-buffer (insert (make-string 1010 ?a)) (dotimes (k 100) (put-text-property (+ 1 (* k 10)) (+ 5 (* k 10)) 'face 'bold))) put-text-property)
    (tp-getcp nil (with-current-buffer libcomp-big (get-char-property 120 'face)) get-char-property)
    (tp-nscpc nil (with-current-buffer libcomp-big (next-single-char-property-change 120 'face)) next-single-char-property-change)
    (tp-ins nil (with-current-buffer libcomp-big (goto-char 300) (insert (propertize "xy" 'face 'bold)) (delete-region 300 302) (insert "zz") (delete-region 300 302)) nelisp--tp-graft nelisp--tp-adjust-insert nelisp--tp-delete nelisp--tp-insert-nil)
    (tp-propertize2 nil (propertize "hello world" 'face 'bold 'help-echo "x") propertize)
    (tp-setp nil (with-current-buffer libcomp-b (set-text-properties 101 120 nil)) set-text-properties))
  "The attrib10x rows (survey drivers in ../resume-4943-20261024/target/resurvey24).")

(defvar libcomp-b nil)
(defvar libcomp-i 0)
(defvar libcomp-big nil) (defvar libcomp-ps nil) (defvar libcomp-p1 nil) (defvar libcomp-p3 nil)

(defconst libcomp--nowrap
  '(fset symbol-function apply funcall set-case-table car-safe)
  "Never wrapped: the counting wrapper itself uses them, or callers compare
their function cell's identity (`nlre--nat-fold-default-p').  `car-safe' is
an opcode primitive compiled code inlines; wrapped, it is reached about 32
times per re-search trip only through the wrappers' own interpreted calls
\(a run with `car-safe' alone wrapped counts 0 calls on v25 and on the lane
build).")

(defun libcomp--classify (name fn rows)
  "(KIND REASON CALLEES) of function NAME (definition FN)."
  (cond
   ((eq (car-safe fn) 'closure)
    (let ((rec (progn (when name (ignore-errors (nelisp--fx-register name)))
                      (gethash (nthcdr 3 fn) nelisp--fx-table))))
      (cond
       ((and (vectorp rec) (<= (aref rec 1) -16))
        (let ((c (nelisp-fxir-recompile fn)))
          (list "native" "" (and c (nth 2 c) (nelisp-fxir-callees (nth 2 c) (car c))))))
       ((vectorp rec)
        (let ((c (nelisp-fxir-recompile fn)))
          (list "fx" (if (memq name rows) "row-stale" "no-row")
                (and c (nth 2 c) (nelisp-fxir-callees (nth 2 c) (car c))))))
       (t (list "decl" (libcomp--decline-reason fn) (libcomp--fbound-symbols (nthcdr 3 fn)))))))
   ((and (consp fn) (eq (car fn) 'builtin)) (list "subr" "" nil))
   ((subrp fn) (list "subr" "" nil))
   ((byte-code-function-p fn) (list "bytecode" "bytecode-adopted" (libcomp--bc-callees fn)))
   (t (list "other" (format "%S" (car-safe fn)) nil))))

;; LIBCOMP_REASON=NAME: only print "REASON NAME KIND REASON" for that function.
(when (getenv "LIBCOMP_REASON")
  (let* ((name (intern (getenv "LIBCOMP_REASON")))
         (k (libcomp--classify name (indirect-function name) (libcomp--row-names))))
    (princ (format "REASON %s %s %s\n" name (car k) (nth 1 k)))
    (kill-emacs 0)))

(defvar libcomp--in nil "Non-nil inside a counting wrapper's bookkeeping.")
(defconst libcomp--trips 10)

(let* ((want (mapcar #'intern (split-string (or (getenv "LIBCOMP_ROWS") "") " " t)))
       (maxd (string-to-number (or (getenv "LIBCOMP_DEPTH") "6")))
       (rows (libcomp--row-names)))
  (dolist (p libcomp--plan)
    (when (or (null want) (memq (car p) want))
      (let ((row (car p)) (seen (make-hash-table :test 'eq)) (cands nil) (queue nil)
            (saved nil) (cells nil))
        (condition-case err (progn (eval (nth 1 p) t) (eval (nth 2 p) t) (eval (nth 2 p) t))
          (error (princ (format "WARMUP %s error %S\n" row err))))
        ;; 1. Static candidates: Lisp functions reachable from the roots.
        (setq queue (mapcar (lambda (r) (cons r 0)) (nthcdr 3 p)))
        (while queue
          (let* ((it (pop queue)) (name (car it)) (d (cdr it))
                 (fn (and (fboundp name) (symbol-function name))))
            (unless (gethash name seen)
              (puthash name t seen)
              (let ((k (libcomp--classify name (and fn (indirect-function name)) rows)))
                (when (and (symbolp fn) fn) (push (cons fn d) queue))
                (unless (or (member (car k) '("subr" "other")) (memq name libcomp--nowrap))
                  (push name cands))
                (when (< d maxd)
                  (dolist (c (delete-dups (copy-sequence (nth 2 k))))
                    (unless (gethash c seen) (setq queue (append queue (list (cons c (1+ d))))))))))))
        ;; 2. Count calls: a wrapper per candidate (restored afterwards).
        (dolist (name cands)
          (let ((orig (symbol-function name)) (cell (list 0)))
            (push (cons name orig) saved)
            (push (cons name cell) cells)
            (fset name (lambda (&rest a)
                         (unless libcomp--in (setcar cell (1+ (car cell))))
                         (apply orig a)))))
        (let ((libcomp--in nil))
          (condition-case err
              (dotimes (_ libcomp--trips) (eval (nth 2 p) t))
            (error (princ (format "COUNT %s error %S\n" row err)))))
        (dolist (s saved) (fset (car s) (cdr s)))
        ;; 3. Report the functions that ran.
        (let ((counts nil))
          (dolist (c (sort cells (lambda (a b) (> (car (cdr a)) (car (cdr b))))))
            (when (> (car (cdr c)) 0)
              (let* ((name (car c)) (k (libcomp--classify name (indirect-function name) rows)))
                (push (car k) counts)
                (princ (format "HOT %s %s %.1f %s %s\n" row name
                               (/ (car (cdr c)) (float libcomp--trips)) (car k) (nth 1 k))))))
          ;; Method functions of the generic row (not reachable by name).
          (when (eq row 'obj-generic-type-str)
            (dolist (m (get 'libcomp-g 'nelisp-cl-generic--methods))
              (let ((f (plist-get m :fn)))
                (let ((k (libcomp--classify nil f rows)))
                  (princ (format "HOT %s <method> - %s %s\n" row (car k) (nth 1 k)))))))
          (princ (format "SUMMARY %s%s\n" row
                         (mapconcat (lambda (k) (format " %s=%d" k (cl-count k counts :test #'equal)))
                                    '("native" "fx" "decl" "bytecode") ""))))))))
(kill-emacs 0)
