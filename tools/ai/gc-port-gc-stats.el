;;; gc-port-gc-stats.el --- garbage-collect's counters equal the full census  -*- lexical-binding: t; -*-

;; Doc 223, "garbage-collect's counters": the explicit `garbage-collect' reports strings, vectors,
;; symbols and conses.  GNU takes these from its sweeps; NeLisp counts a heap object when the marker
;; first marks its block, a legacy cons the same way, an interned symbol from the intern table's
;; occupancy, and a typed cell from its sweep.  The reference is the identity-set census this replaced:
;; (nelisp--debug-switch 53) collects once with the census and once with the counters and answers
;; (MASK C0..C5 K0..K5), the counters then the census, MASK bit I set when row I differs.
;; GCS_MODE selects the run:
;;   run      the scenarios below, each followed by the comparison, with the conservative scan off; exact
;;            deltas of the reported rows for objects kept alive and for garbage that is not;
;;   dump     build the population `gcs-pop', collect, write GCS_IMAGE; prints "GCS STAT n n n n n n";
;;   restore  under --cold-load-from GCS_IMAGE: the population is image objects and must not be counted
;;            (the rows fall by what the population added, GCS_PRE_* from the dump run), objects stored
;;            into it afterwards are counted exactly, and the comparison holds.
;; Prints "GCS MODE checks=N failures=F PASS|FAIL" last.
(defvar gcs-mode nil)
(defvar gcs-checks 0)
(defvar gcs-failures 0)
(defvar gcs-pop nil)
(defvar gcs-keep nil)
(setq gcs-mode (or (getenv "GCS_MODE") "run") gcs-checks 0 gcs-failures 0)
(defun gcs-check (ok label)
  (setq gcs-checks (1+ gcs-checks))
  (unless ok
    (setq gcs-failures (1+ gcs-failures))
    (princ (format "GCS FAIL %s\n" label))))
(defun gcs-compare (label)
  ;; Counters = census.  The answer is (MASK C0..C5 K0..K5).
  (let ((r (nelisp--debug-switch 53)))
    (gcs-check (and (consp r) (eq (car r) 0)) (format "%s: counters/census %S" label r))
    r))
(defun gcs-compare-loose (label)
  ;; With the conservative scan on, the two collections pin whatever their stacks hold, so a few
  ;; cells may differ between them (typed cells are counted by their sweep, pinned or not).
  (let* ((r (nelisp--debug-switch 53)) (c (cdr r)) (k (nthcdr 6 c)) (ok (consp r)) (i 0))
    (while (and ok (< i 6))
      (when (> (abs (- (nth i c) (nth i k))) 64) (setq ok nil))
      (setq i (1+ i)))
    (gcs-check ok (format "%s: counters/census %S" label r))))
(defun gcs-stats ()
  (let ((r (garbage-collect)))
    (list (nth 2 (assq 'conses r)) (nth 2 (assq 'symbols r)) (nth 2 (assq 'strings r))
          (nth 2 (assq 'string-bytes r)) (nth 2 (assq 'vectors r)) (nth 2 (assq 'vector-slots r)))))
(defun gcs-delta (a b)
  (let ((r nil))
    (while a (setq r (cons (- (car b) (car a)) r) a (cdr a) b (cdr b)))
    (nreverse r)))
(defun gcs-str (i) (concat "gcs-s-" (number-to-string i)))

(defun gcs-scenarios ()
  (gcs-compare "start")
  ;; Strings: distinct buffers, shared objects, empty strings, mutated and multibyte ones.
  (let ((v (make-vector 600 nil)) (i 0))
    (while (< i 600)
      (aset v i (cond ((< i 300) (gcs-str i))
                      ((< i 400) (string-to-unibyte (gcs-str i)))
                      ((< i 450) (let ((s (copy-sequence (gcs-str i)))) (aset s 2 ?Z) s))
                      ((< i 500) (concat "gcs-é-" (number-to-string i)))
                      ((< i 520) (make-string 0 ?x))
                      ((< i 540) (substring "abc" 1 1))
                      ((< i 560) "")
                      (t (let ((s (gcs-str i))) (concat s s s)))))
      (setq i (1+ i)))
    (setq gcs-keep (list v v (aref v 5) (aref v 5)))
    (gcs-compare "strings"))
  ;; Vectors, records, closures, char-tables, bool-vectors, hash tables.
  (let ((l nil) (i 0))
    (while (< i 300)
      (setq l (cons (vector i (vector i) (record 'gcs-rec i i) (make-record 'gcs-rec 3 i)) l))
      (setq i (1+ i)))
    (setq gcs-keep (cons l gcs-keep))
    (setq gcs-keep (cons (list (make-vector 0 nil) (make-vector 1 'a) (make-vector 100000 0)) gcs-keep))
    (setq gcs-keep (cons (let ((x 1)) (list (lambda () x) (lambda (y) (+ x y)))) gcs-keep))
    (setq gcs-keep (cons (let ((ct (make-char-table 'gcs-ct)))
                           (set-char-table-range ct '(?a . ?z) 1)
                           (aset ct ?あ 2)
                           ct)
                         gcs-keep))
    (setq gcs-keep (cons (list (make-bool-vector 64 t) (make-bool-vector 1000 nil)) gcs-keep))
    (setq gcs-keep (cons (let ((h (make-hash-table :test 'equal)) (j 0))
                           (while (< j 200) (puthash (gcs-str j) (list j) h) (setq j (1+ j)))
                           h)
                         gcs-keep))
    (gcs-compare "vectors and friends"))
  ;; Symbols: interned (the table's occupancy), uninterned (identity), keywords, canonical names.
  (let ((l nil) (i 0))
    (while (< i 300)
      (setq l (cons (intern (format "gcs-i-%d" i)) l))
      (setq l (cons (make-symbol (format "gcs-u-%d" i)) l))
      (setq l (cons (intern (format ":gcs-k-%d" i)) l))
      (setq i (1+ i)))
    (setq gcs-keep (cons l gcs-keep))
    (setq gcs-keep (cons (mapcar #'symbol-name (list 'car 'gcs-i-3 (make-symbol "u"))) gcs-keep))
    (gcs-compare "symbols"))
  ;; Conses, floats, bignums.
  (let ((l nil) (i 0))
    (while (< i 5000)
      (setq l (cons (cons i (* i 0.5)) l))
      (setq i (1+ i)))
    (setq gcs-keep (cons (list l (+ (ash 1 70) 1) (ash 1 80)) gcs-keep))
    (gcs-compare "conses"))
  ;; Garbage is not counted: the same comparison after a lot of it died.
  (let ((i 0))
    (while (< i 20000)
      (gcs-str i) (vector i i) (list i i) (make-string 10 ?a)
      (setq i (1+ i)))
    (gcs-compare "after garbage"))
  ;; A buffer, a marker and text.
  (let ((b (generate-new-buffer " gcs")))
    (with-current-buffer b (insert "hello world") (setq gcs-keep (cons (copy-marker 3) gcs-keep)))
    (setq gcs-keep (cons b gcs-keep))
    (gcs-compare "buffer"))
  ;; The conservative scan on: the two collections see the same stack, so the rows still agree.
  (nelisp--debug-switch 29)
  (gcs-compare-loose "scan on")
  (nelisp--debug-switch 28))

(defun gcs-exact-measure (n)
  ;; Keep assertions outside this shared measurement shape: their first evaluation can populate
  ;; evaluator metadata even for N=0.  Warm this helper only with an empty population.
  (let* ((holder (make-vector 4 nil)) (i 0) (strs nil) (vecs nil) (syms nil) (conses nil)
         (a nil) (kept nil) (comparison nil))
    (setq gcs-keep (cons holder gcs-keep) a (gcs-stats))
    (while (< i n)
      (setq strs (cons (concat "exact-" (number-to-string (+ 100000 i))) strs))
      (setq vecs (cons (vector i i i) vecs))
      (setq syms (cons (make-symbol (concat "x" (number-to-string i))) syms))
      (setq conses (cons (cons i i) conses))
      (setq i (1+ i)))
    (aset holder 0 strs) (aset holder 1 vecs) (aset holder 2 syms) (aset holder 3 conses)
    (setq kept (gcs-delta a (gcs-stats)))
    ;; Compare the live population with the full census, before removing its last references.
    (setq comparison (nelisp--debug-switch 53))
    (aset holder 0 nil) (aset holder 1 nil) (aset holder 2 nil) (aset holder 3 nil)
    (setq strs nil vecs nil syms nil conses nil)
    (list kept (gcs-delta a (gcs-stats)) comparison)))

(defun gcs-exact ()
  ;; Exact deltas of N distinct strings, vectors of 3 slots, uninterned symbols and conses.
  ;; The two steady controls must agree and add no non-cons objects.  Subtract only that verified
  ;; control, never the first-use overhead: otherwise a growing evaluator delta could hide garbage.
  (gcs-exact-measure 0)
  (let* ((control (gcs-exact-measure 0)) (again (gcs-exact-measure 0)))
    (gcs-check (and (equal (car control) (car again)) (equal (nth 1 control) (nth 1 again))
                    (equal (cdr (car control)) '(0 0 0 0 0))
                    (equal (cdr (nth 1 control)) '(0 0 0 0 0))
                    (eq (car (nth 2 control)) 0) (eq (car (nth 2 again)) 0))
               (format "zero-population control %S -> %S" control again))
    (let* ((n 400) (actual (gcs-exact-measure n))
           (d (gcs-delta (car again) (car actual))))
      ;; conses: n for the objects, n for the four lists' cells (strs, vecs, syms, conses), n for the
      ;; pairs of `conses'; vectors: n; strings: n and the n names of the new symbols (a name is a
      ;; string, as in GNU); bytes: 12 * n and the names; symbols: n; slots: 3 * n.  The evaluator may
      ;; keep a handful of objects of its own between the two readings.
      (gcs-check (and (>= (nth 2 d) (* 2 n)) (<= (nth 2 d) (+ (* 2 n) 8))) (format "strings delta %S" d))
      (gcs-check (>= (nth 3 d) (* n (length "exact-100000"))) (format "string-bytes delta %S" d))
      (gcs-check (and (>= (nth 4 d) n) (<= (nth 4 d) (+ n 8))) (format "vectors delta %S" d))
      (gcs-check (and (>= (nth 5 d) (* 3 n)) (<= (nth 5 d) (+ (* 3 n) 64))) (format "vector-slots delta %S" d))
      (gcs-check (>= (nth 1 d) n) (format "symbols delta %S" d))
      (gcs-check (>= (nth 0 d) (* 3 n)) (format "conses delta %S" d))
      (let ((r (nth 2 actual)))
        (gcs-check (and (consp r) (eq (car r) 0)) (format "exact: counters/census %S" r)))
      (let ((d (gcs-delta (nth 1 again) (nth 1 actual))))
        (gcs-check (and (<= (nth 2 d) 8) (<= (nth 4 d) 8)) (format "dropped objects still counted %S" d))))))

(defun gcs-build-pop ()
  (let ((v (make-vector 1000 nil)) (i 0))
    (while (< i 1000)
      (aset v i (list (concat "pop-" (number-to-string i)) (vector i i) (make-symbol (format "pop-u-%d" i))
                      (list i) (make-record 'gcs-pop 2 i)))
      (setq i (1+ i)))
    v))

(cond
 ((equal gcs-mode "dump")
  (setq gcs-pop (gcs-build-pop))
  (nelisp--debug-switch 28)
  (gcs-compare "population")
  (let ((s (gcs-stats)))
    (princ (format "GCS STAT %s\n" (mapconcat #'number-to-string s " "))))
  (gcs-check (> (nelisp--arena-dump-image-stream (getenv "GCS_IMAGE")) 0) "image dump"))
 ((equal gcs-mode "restore")
  (nelisp--debug-switch 28)
  (gcs-check (and (vectorp gcs-pop) (= (length gcs-pop) 1000)) "the population came through the image")
  (gcs-compare "restored")
  (let* ((pre (mapcar #'string-to-number (split-string (getenv "GCS_PRE") " ")))
         (now (gcs-stats)))
    ;; The population added 1000 strings and the 1000 names of its uninterned symbols, 1000 + 1000 (the
    ;; records) vectors and 6000 cons cells; as image objects none of them is counted.  (The 1000
    ;; uninterned symbols themselves are: the census has always counted a symbol view without asking
    ;; whether its name is in the image.)
    (gcs-check (and (>= (- (nth 2 pre) (nth 2 now)) 1900) (<= (- (nth 2 pre) (nth 2 now)) 2300))
               (format "image strings %S -> %S" pre now))
    (gcs-check (>= (- (nth 4 pre) (nth 4 now)) 2000) (format "image vectors still counted %S -> %S" pre now))
    (gcs-check (>= (- (nth 0 pre) (nth 0 now)) 4000) (format "image conses still counted %S -> %S" pre now))
    ;; Heap objects stored into image objects are counted, exactly.
    (let ((i 0) (before now))
      (while (< i 300)
        (setcar (nth 3 (aref gcs-pop i)) (concat "heap-" (number-to-string (+ 7000 i))))
        (setq i (1+ i)))
      (let ((d (gcs-delta before (gcs-stats))))
        (gcs-check (and (>= (nth 2 d) 300) (<= (nth 2 d) 312)) (format "heap strings in image conses %S" d))))
    (gcs-compare "heap objects held by the image")))
 (t
  (nelisp--debug-switch 28)
  (gcs-scenarios)
  (gcs-exact)
  (nelisp--debug-switch 29)))
(princ (format "GCS %s checks=%d failures=%d %s\n" gcs-mode gcs-checks gcs-failures
               (if (= gcs-failures 0) "PASS" "FAIL")))
nil
