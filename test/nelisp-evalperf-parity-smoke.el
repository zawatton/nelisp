;;; nelisp-evalperf-parity-smoke.el --- host-vs-standalone probes  -*- lexical-binding: t; -*-
;; Evaluated by both GNU Emacs and the standalone binary; the driver script
;; (nelisp-evalperf-parity-smoke.sh) diffs the printed lines.  Covers the
;; behaviour touched by the eval-performance lane: native `fboundp' answers
;; for nil/t, hash lookups that miss with a vector/record/buffer/cons key,
;; and the ^ / \` fast paths of the regexp scanner.  The last probes are
;; timing bounds (printed as booleans) that fail on a binary without the
;; fixes: a miss on a non-atom key used to scan every bucket.
(setq case-fold-search t)
(defmacro ep--p (form)
  `(princ (format "%S => %S\n" ',form
                  (condition-case e ,form (error (list 'ERR (car e)))))))
;; fboundp: nil and t answer nil, non-symbols signal, unbound markers filtered.
(ep--p (fboundp nil))
(ep--p (fboundp t))
(ep--p (fboundp 'car))
(ep--p (fboundp :kw))
(ep--p (fboundp 'ep--no-such-function))
(ep--p (fboundp 1))
(ep--p (fboundp "car"))
(ep--p (fboundp '(a)))
(ep--p (fboundp [1]))
(ep--p (progn (defun ep--fb () 1) (fboundp 'ep--fb)))
(ep--p (progn (fmakunbound 'ep--fb) (fboundp 'ep--fb)))
(ep--p (progn (defconst ep--fbc 1) (fboundp 'ep--fbc)))
(ep--p (progn (fset 'ep--fbn nil) (fboundp 'ep--fbn)))
(ep--p (let ((x nil)) (and (symbolp x) (fboundp x))))
(ep--p (mapcar 'fboundp '(car nil t ep--none)))
;; Hash tables: hits and misses for keys whose hash is their bare tag.
(dolist (test '(eq eql equal))
  (let* ((h (make-hash-table :test test))
         (v1 (vector 1 2)) (v2 (vector 1 2)) (c1 (list 1 2)) (c2 (list 1 2))
         (r1 (record 'ep-rec 1)) (r2 (record 'ep-rec 1))
         (b1 (generate-new-buffer "ep-b1")) (b2 (generate-new-buffer "ep-b2")))
    (puthash v1 'v1 h) (puthash c1 'c1 h) (puthash r1 'r1 h) (puthash b1 'b1 h)
    (puthash 'sym 'sym h) (puthash 42 'int h)
    (princ (format "%S ht-basic => %S\n" test
                   (list (gethash v1 h) (if (eq test 'equal) 'skip (gethash v2 h 'nf))
                         (gethash c1 h)
                         (gethash c2 h 'nf) (gethash r1 h)
                         (if (eq test 'equal) 'skip (gethash r2 h 'nf))
                         (gethash b1 h) (gethash b2 h 'nf) (gethash 'sym h)
                         (gethash 42 h) (gethash 'zz h 'dflt)
                         (gethash (vector 9) h 'miss) (hash-table-count h))))
    (dotimes (i 200) (puthash (vector i) i h) (puthash (list i i) (- i) h))
    (princ (format "%S ht-grown => %S\n" test
                   (list (hash-table-count h) (gethash v1 h) (gethash (vector 500) h 'nf)
                         (gethash r1 h) (gethash b1 h) (gethash b2 h 'nf))))
    (remhash v1 h) (remhash b1 h)
    (princ (format "%S ht-rem => %S\n" test
                   (list (gethash v1 h 'nf) (gethash b1 h 'nf) (hash-table-count h))))
    (clrhash h)
    (princ (format "%S ht-clr => %S\n" test (list (gethash v1 h 'nf) (hash-table-count h))))
    (kill-buffer b1) (kill-buffer b2)))
;; Regexp scanner: ^ and \` anchored patterns, with and without START.
(let ((pats '("^ZZZ" "^a" "^b" "^" "^$" "\\`a" "\\`" "\\`ab" "\\`a.*c" "^ab\\|cd"
              "^[a-c]+$" "^\\(a\\)\\(b\\)" "\\(^a\\)" "\\`\\(a\\|b\\)+" "^[ \t]*b"
              "\\`[[:alpha:]]+\\'" "^.*$" "^\n" "^a\\'"))
      (strs (list "" "a" "abc" "ab\nabc" "\nabc\n" "xx\nab\nb" "b\n\nb" "Abc\nAbc"
                  "cab\nbab\n" "a\n" "\n" "  b\n\tb" "aaa\nbbb\nccc")))
  (dolist (p pats)
    (dolist (s strs)
      (dolist (st '(nil 0 1 2 3))
        (when (or (null st) (<= st (length s)))
          (let ((r (condition-case e (string-match p s st) (error (list 'ERR (car e))))))
            (princ (format "sm %S %S %S => %S %S\n" p s st r
                           (and (integerp r)
                                (list (match-beginning 0) (match-end 0)
                                      (match-beginning 1) (match-end 1)))))))))
    (dolist (s strs)
      (with-temp-buffer
        (insert s)
        (goto-char (point-min))
        (let ((res nil))
          (condition-case nil
              (while (and (< (length res) 6) (re-search-forward p nil t))
                (push (list (match-beginning 0) (match-end 0)) res)
                (when (= (match-beginning 0) (match-end 0))
                  (if (eobp) (signal 'end-of-buffer nil) (forward-char 1))))
            (error nil))
          (princ (format "rs %S %S => %S\n" p s (nreverse res))))
        (goto-char 1)
        (princ (format "la %S %S => %S\n" p s (looking-at p)))))))
;; Timing bounds (booleans, so host and standalone print the same line).  The
;; minimum of five batches keeps a loaded machine from failing them.
(defun ep--best-of (n f)
  (let ((best nil))
    (dotimes (_ 5)
      (let ((t0 (float-time)))
        (dotimes (_ n) (funcall f))
        (let ((per (/ (- (float-time) t0) n)))
          (when (or (null best) (< per best)) (setq best per)))))
    best))
(let* ((h (make-hash-table :test 'eq)) (k (vector 1 2)))
  (princ (format "timing gethash-miss-vector-under-100us => %S\n"
                 (< (ep--best-of 100 (lambda () (gethash k h))) 0.0001))))
(let ((s "libfoo-bar.so.1"))
  (princ (format "timing string-match-bol-under-2500us => %S\n"
                 (< (ep--best-of 40 (lambda () (string-match "^ZZZ" s))) 0.0025))))
nil
