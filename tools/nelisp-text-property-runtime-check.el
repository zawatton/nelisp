;;; nelisp-text-property-runtime-check.el --- Differential and timing -*- lexical-binding: t; -*-

;; Loaded by the no-build probe prepared by nelisp-text-property-check.el.
(defvar nelisp-tp-check--seed 20261012)
(defun nelisp-tp-check--random (n)
  (setq nelisp-tp-check--seed (% (+ (* nelisp-tp-check--seed 1664525) 1013904223) 2147483647))
  (% nelisp-tp-check--seed n))

(defun nelisp-tp-check--layout ()
  (let ((pos 0) intervals
        (values (list nil '(face a) '(face a other t) '(other t face a)
                      '(face nil) '(face b) (list 'face (list 'v))
                      (list 'face (list 'v)))))
    (while (< pos 32)
      (let* ((end (min 32 (+ pos 1 (nelisp-tp-check--random 4))))
             (pl (nth (nelisp-tp-check--random 8) values)))
        (when pl (push (list pos end pl) intervals))
        (setq pos end)))
    (nreverse intervals)))

(defun nelisp-tp-check--differential ()
  (let ((s (make-string 32 ?x)) (count 0) (queries 0))
    (with-temp-buffer
      (insert (make-string 32 ?x))
      (while (< count 2000)
        (let* ((ivs (nelisp-tp-check--layout))
               (kind (% count 3))
               (obj (if (= kind 0) s (if (= kind 1) nil (current-buffer))))
               (offset (if (= kind 0) 0 1))
               (pos (+ offset (nelisp-tp-check--random 33)))
               (limit (if (= (% count 4) 0) nil
                        (+ offset (- (nelisp-tp-check--random 41) 4))))
               (prop (if (= (% count 2) 0) 'face 'other))
               (shifted (mapcar (lambda (iv) (list (+ offset (car iv))
                                                  (+ offset (cadr iv)) (nth 2 iv))) ivs)))
          (nelisp--tp-set-intervals obj shifted)
          (dolist (backward '(nil t))
            (dolist (single '(nil t))
              (let ((old (funcall nelisp-tp-check--old-change pos obj limit backward single prop))
                    (new (nelisp--w202-property-change pos obj limit backward single prop)))
                (unless (equal old new)
                  (error "Differential %s: %S pos=%s limit=%S backward=%S single=%S: %S / %S"
                         count shifted pos limit backward single old new))
                (setq queries (1+ queries)))))
          (unless (and (equal (get-text-property pos prop obj)
                             (nelisp--tp-value-at pos prop shifted))
                       (equal (text-properties-at pos obj)
                              (nelisp--tp-plist-at pos shifted)))
            (error "Lookup mismatch %s" count))
          (let* ((start (nelisp-tp-check--random 33))
                 (end (nelisp-tp-check--random 33))
                 (updater (if (= (% count 2) 0)
                              (lambda (pl) (nelisp--tp-plist-set pl 'face 'updated))
                            (lambda (pl) (nelisp--tp-plist-remove pl '(face)))))
                 (snapshot (copy-tree ivs))
                 (old (funcall nelisp-tp-check--old-rebuild ivs start end updater))
                 (new (nelisp--tp-rebuild ivs start end updater)))
            (unless (and (equal old new) (equal snapshot ivs))
              (error "Rebuild mismatch %s: %S [%s,%s): %S / %S"
                     count ivs start end old new))))
        (setq count (1+ count))
        (when (= (% count 250) 0)
          (princ (format "DIFFERENTIAL PROGRESS %s/2000\n" count))))
      (princ (format "DIFFERENTIAL PASS %s layouts %s boundary queries 4000 lookups 2000 rebuilds\n"
                     count queries)))))

(defun nelisp-tp-check--call (op n s ivs)
  (cond ((eq op 'next-single-property-change) (funcall op n 'face s))
        ((eq op 'get-text-property) (funcall op n 'face s))
        ((eq op 'put-text-property)
         (nelisp--tp-set-intervals s ivs)
         (funcall op 0 (* n 2) 'other t s))
        (t (funcall op n s))))

(defun nelisp-tp-check--timing (n)
  (let ((s (make-string (* n 2) ?x)) (i 0) ivs
        (new-change (symbol-function 'nelisp--w202-property-change))
        (new-rebuild (symbol-function 'nelisp--tp-rebuild)))
    (while (< i n)
      (push (list (* i 2) (+ (* i 2) 2) (list 'face (if (= (% i 2) 0) 'a 'b))) ivs)
      (setq i (1+ i)))
    (setq ivs (nreverse ivs))
    (dolist (op '(next-single-property-change next-property-change text-properties-at
                  get-text-property put-text-property))
      (let (old-times new-times)
        ;; Alternate order to reduce drift; each sample averages three calls.
        (dotimes (sample 3)
          (dolist (arm (if (= (% sample 2) 0) '(old new) '(new old)))
            (fset 'nelisp--w202-property-change
                  (if (eq arm 'old) nelisp-tp-check--old-change new-change))
            (fset 'nelisp--tp-rebuild
                  (if (eq arm 'old) nelisp-tp-check--old-rebuild new-rebuild))
            (nelisp--tp-set-intervals s ivs)
            (nelisp-tp-check--call op n s ivs)
            ;; Reset collection debt outside the measurement, after warm-up.
            ;; Report collection time separately; it is not per-call time.
            (let ((gc-start (float-time)))
              (garbage-collect)
              (princ (format "GC-BEFORE %s %s %s ms=%s\n"
                             n op arm (* 1000 (- (float-time) gc-start)))))
            (let ((start (float-time)) elapsed)
              (dotimes (_ 3)
                ;; Cover all intervals for puts; use the identical input.
                (nelisp-tp-check--call op n s ivs))
              (setq elapsed (/ (* 1000 (- (float-time) start)) 3.0))
              (if (eq arm 'old) (push elapsed old-times) (push elapsed new-times)))
            (let ((gc-start (float-time)))
              (garbage-collect)
              (princ (format "GC-AFTER %s %s %s ms=%s\n"
                             n op arm (* 1000 (- (float-time) gc-start)))))))
        (princ (format "TIMING %s %s old-ms=%s new-ms=%s samples-old=%S samples-new=%S\n"
                       n op (nth 1 (sort old-times #'<)) (nth 1 (sort new-times #'<))
                       old-times new-times))))
    (fset 'nelisp--w202-property-change new-change)
    (fset 'nelisp--tp-rebuild new-rebuild)))

(when (getenv "TP_CHECK_MUTATE")
  (fset 'nelisp--w202-property-change (lambda (&rest _args) 'broken)))
(unless (equal (getenv "TP_CHECK_MODE") "bench")
  (nelisp-tp-check--differential))
(unless (equal (getenv "TP_CHECK_MODE") "diff")
  (nelisp-tp-check--timing 50)
  (nelisp-tp-check--timing 200))
(princ "TEXT-PROPERTY-CHECK COMPLETE\n")
;;; nelisp-text-property-runtime-check.el ends here
