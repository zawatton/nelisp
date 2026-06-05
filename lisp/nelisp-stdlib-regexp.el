;;; nelisp-stdlib-regexp.el --- pure-elisp Emacs-regexp matcher  -*- lexical-binding: nil; -*-

;; Doc 143: a backtracking matcher for the common Emacs regexp subset,
;; implemented WITHOUT lexical closures so it loads on the dynamic-binding
;; standalone reader prelude.  Backtracking threads the "rest of the pattern"
;; as an explicit argument (no CPS lambdas).  Capture positions are recorded
;; via :savestart/:saveend sentinel nodes into a global vector; the last writes
;; on the successful path win (match-list returns on first success).
;;
;; Supported: literal chars, `.', `*' `+' `?' (greedy), `[...]'/`[^...]' with
;; ranges, `^' `$' anchors, `\(...\)' capture groups, `\|' alternation,
;; `\w' `\W' `\s-'(whitespace) `\.'-style escapes, and `\\C' literal escapes.
;; Out of scope (v1): `\{n,m\}', backreferences, non-greedy `*?', `\b' `\<' `\>',
;; syntax classes other than whitespace, char-class names like [[:alpha:]].
;;
;; This file uses a distinct `nlre-' prefix so it can be differential-tested in
;; host Emacs against the real `string-match' before being wired into the reader
;; prelude (where `nlre-string-match' is aliased to `string-match').

;; ---- parser: pattern string -> node list (a "seq") ----
;; node forms:
;;   (:lit C) (:any) (:set NEG RANGES) (:bol) (:eol)
;;   (:word NEG) (:space NEG)
;;   (:group N SEQ) (:alt LIST-OF-SEQ)
;;   (:star NODE) (:plus NODE) (:opt NODE)
;;   (:savestart N) (:saveend N)   ;; injected around group bodies

(defvar nlre--gcount 0 "Group counter during a parse.")

(defun nlre--parse (pat)
  "Parse PAT into a top-level node (a :seq or :alt). Sets group count."
  (setq nlre--gcount 0)
  (let ((r (nlre--parse-alt pat 0 (length pat))))
    ;; r = (NODE . pos)
    (car r)))

(defun nlre--parse-alt (pat i n)
  "Parse alternation from I; return (NODE . newpos).  Stops at \\) or end."
  (let ((branches nil) (cont t) (cur nil))
    (while cont
      (let ((r (nlre--parse-seq pat i n)))
        (setq cur (car r) i (cdr r))
        (setq branches (cons cur branches))
        (if (and (< (1+ i) n) (eq (aref pat i) ?\\) (eq (aref pat (1+ i)) ?|))
            (setq i (+ i 2))
          (setq cont nil))))
    (setq branches (nreverse branches))
    (cons (if (= (length branches) 1) (list :seq (car branches))
            (list :alt branches))
          i)))

(defun nlre--parse-seq (pat i n)
  "Parse a sequence of pieces from I; return (LIST-OF-NODES . newpos).
Stops at \\| , \\) , or end."
  (let ((nodes nil) (cont t))
    (while (and cont (< i n))
      (let ((c (aref pat i)))
        (cond
         ;; end of this seq: \| or \)
         ((and (eq c ?\\) (< (1+ i) n)
               (let ((d (aref pat (1+ i)))) (or (eq d ?|) (eq d ?\)))))
          (setq cont nil))
         (t
          (let* ((ar (nlre--parse-atom pat i n))
                 (atom (car ar)) (j (cdr ar)))
            ;; quantifier?
            (if (< j n)
                (let ((q (aref pat j)))
                  (cond
                   ((eq q ?*) (setq nodes (cons (list :star atom) nodes) j (1+ j)))
                   ((eq q ?+) (setq nodes (cons (list :plus atom) nodes) j (1+ j)))
                   ((eq q ??) (setq nodes (cons (list :opt atom) nodes) j (1+ j)))
                   ((and (eq q ?\\) (< (1+ j) n) (eq (aref pat (1+ j)) ?{))
                    (let* ((br (nlre--parse-brace atom pat (+ j 2) n)))
                      ;; br = (REVERSED-NODES . newpos)
                      (setq nodes (append (car br) nodes) j (cdr br))))
                   (t (setq nodes (cons atom nodes)))))
              (setq nodes (cons atom nodes)))
            (setq i j))))))
    (cons (nreverse nodes) i)))

(defun nlre--parse-brace (atom pat k n)
  "Parse \\{min[,[max]]\\} repetition of ATOM starting at K (after \\{).
Return (REVERSED-EXPANSION-NODES . newpos)."
  (let ((minv 0) (maxv nil) (have-comma nil) (digits ""))
    (while (and (< k n) (let ((c (aref pat k))) (and (>= c ?0) (<= c ?9))))
      (setq digits (concat digits (substring pat k (1+ k))) k (1+ k)))
    (setq minv (if (= (length digits) 0) 0 (string-to-number digits)))
    (when (and (< k n) (eq (aref pat k) ?,))
      (setq have-comma t k (1+ k))
      (setq digits "")
      (while (and (< k n) (let ((c (aref pat k))) (and (>= c ?0) (<= c ?9))))
        (setq digits (concat digits (substring pat k (1+ k))) k (1+ k)))
      (when (> (length digits) 0) (setq maxv (string-to-number digits))))
    (unless have-comma (setq maxv minv))
    ;; consume closing \}
    (when (and (< (1+ k) n) (eq (aref pat k) ?\\) (eq (aref pat (1+ k)) ?}))
      (setq k (+ k 2)))
    ;; build expansion (reversed, to prepend onto nodes accumulator)
    (let ((out nil) (i 0))
      (while (< i minv) (setq out (cons atom out) i (1+ i)))
      (if (null maxv)
          (setq out (cons (list :star atom) out))
        (let ((extra (- maxv minv)) (j 0))
          (while (< j extra) (setq out (cons (list :opt atom) out) j (1+ j)))))
      (cons out k))))

(defun nlre--parse-atom (pat i n)
  "Parse a single atom at I; return (NODE . newpos)."
  (let ((c (aref pat i)))
    (cond
     ((eq c ?.) (cons (list :any) (1+ i)))
     ((eq c ?^) (cons (list :bol) (1+ i)))
     ((eq c ?$) (cons (list :eol) (1+ i)))
     ((eq c ?\[) (nlre--parse-set pat (1+ i) n))
     ((eq c ?\\)
      (let ((d (aref pat (1+ i))))
        (cond
         ((eq d ?\() ;; group start
          (setq nlre--gcount (1+ nlre--gcount))
          (let* ((gn nlre--gcount)
                 (r (nlre--parse-alt pat (+ i 2) n))
                 (inner (car r)) (j (cdr r)))
            ;; consume \)
            (when (and (< (1+ j) n) (eq (aref pat j) ?\\) (eq (aref pat (1+ j)) ?\)))
              (setq j (+ j 2)))
            (cons (list :group gn inner) j)))
         ((eq d ?w) (cons (list :word nil) (+ i 2)))
         ((eq d ?W) (cons (list :word t) (+ i 2)))
         ((eq d ?s)
          ;; \s- = whitespace; consume the class char if present
          (let ((j (+ i 2)))
            (when (< j n) (setq j (1+ j)))
            (cons (list :space nil) j)))
         ((eq d ?S)
          (let ((j (+ i 2))) (when (< j n) (setq j (1+ j)))
               (cons (list :space t) j)))
         ((eq d 96) (cons (list :bos) (+ i 2)))  ;; \` = beginning of string
         ((eq d 39) (cons (list :eos) (+ i 2)))  ;; \' = end of string
         (t (cons (list :lit d) (+ i 2))))))
     (t (cons (list :lit c) (1+ i))))))

(defun nlre--parse-set (pat i n)
  "Parse a char class body (after the opening [) ; return (NODE . newpos)."
  (let ((neg nil) (ranges nil))
    (when (and (< i n) (eq (aref pat i) ?^)) (setq neg t i (1+ i)))
    ;; a leading ] is literal
    (when (and (< i n) (eq (aref pat i) ?\])) (setq ranges (cons (cons ?\] ?\]) ranges) i (1+ i)))
    (let ((cont t))
      (while (and cont (< i n))
        (let ((c (aref pat i)))
          (cond
           ((eq c ?\]) (setq i (1+ i) cont nil))
           ((and (< (+ i 2) n) (eq (aref pat (1+ i)) ?-) (not (eq (aref pat (+ i 2)) ?\])))
            (setq ranges (cons (cons c (aref pat (+ i 2))) ranges) i (+ i 3)))
           (t (setq ranges (cons (cons c c) ranges) i (1+ i)))))))
    (cons (list :set neg (nreverse ranges)) i)))

;; ---- matcher (no closures; rest threaded explicitly) ----

(defvar nlre--caps nil "Vector of (start . end) per group during a match.")

(defun nlre--space-p (c) (or (= c 32) (= c 9) (= c 10) (= c 13) (= c 12)))
(defun nlre--word-p (c)
  (or (and (>= c ?a) (<= c ?z)) (and (>= c ?A) (<= c ?Z))
      (and (>= c ?0) (<= c ?9)) (= c ?_)))

(defun nlre--set-match (neg ranges c)
  (let ((hit nil) (rs ranges))
    (while (and rs (not hit))
      (when (and (>= c (car (car rs))) (<= c (cdr (car rs)))) (setq hit t))
      (setq rs (cdr rs)))
    (if neg (not hit) hit)))

(defun nlre--match-atom1 (node s pos n)
  "Match a single non-quantified atom NODE at POS; return end-pos or nil.
Does NOT continue to any rest (used for one repetition)."
  (let ((tag (car node)))
    (cond
     ((eq tag :lit) (and (< pos n) (eq (aref s pos) (nth 1 node)) (1+ pos)))
     ((eq tag :any) (and (< pos n) (not (eq (aref s pos) ?\n)) (1+ pos)))
     ((eq tag :set) (and (< pos n) (nlre--set-match (nth 1 node) (nth 2 node) (aref s pos)) (1+ pos)))
     ((eq tag :word) (and (< pos n) (let ((w (nlre--word-p (aref s pos)))) (if (nth 1 node) (not w) w)) (1+ pos)))
     ((eq tag :space) (and (< pos n) (let ((w (nlre--space-p (aref s pos)))) (if (nth 1 node) (not w) w)) (1+ pos)))
     ((eq tag :bol) (and (or (= pos 0) (eq (aref s (1- pos)) ?\n)) pos))
     ((eq tag :eol) (and (or (= pos n) (eq (aref s pos) ?\n)) pos))
     ((eq tag :bos) (and (= pos 0) pos))
     ((eq tag :eos) (and (= pos n) pos))
     (t nil))))

(defun nlre--match-list (nodes s pos n)
  "Match NODES (a seq, possibly containing :star/:group/:alt/sentinels) at POS.
Return end-pos or nil."
  (if (null nodes) pos
    (let* ((nd (car nodes)) (rest (cdr nodes)) (tag (car nd)))
      (cond
       ((eq tag :star) (nlre--match-star (nth 1 nd) rest s pos n))
       ((eq tag :plus)
        (nlre--match-list (cons (nth 1 nd) (cons (list :star (nth 1 nd)) rest)) s pos n))
       ((eq tag :opt)
        (or (nlre--match-list (cons (nth 1 nd) rest) s pos n)
            (nlre--match-list rest s pos n)))
       ((eq tag :alt)
        (let ((branches (nth 1 nd)) (res nil))
          (while (and branches (not res))
            (setq res (nlre--match-list (append (car branches) rest) s pos n))
            (setq branches (cdr branches)))
          res))
       ((eq tag :group)
        (nlre--match-list
         (append (list (list :savestart (nth 1 nd)))
                 (nlre--seq-nodes (nth 2 nd))
                 (list (list :saveend (nth 1 nd)))
                 rest)
         s pos n))
       ((eq tag :savestart)
        (let* ((gn (nth 1 nd)) (old (aref nlre--caps gn)))
          (aset nlre--caps gn (cons pos (cdr old)))
          (let ((r (nlre--match-list rest s pos n)))
            (unless r (aset nlre--caps gn old))
            r)))
       ((eq tag :saveend)
        (let* ((gn (nth 1 nd)) (old (aref nlre--caps gn)))
          (aset nlre--caps gn (cons (car old) pos))
          (let ((r (nlre--match-list rest s pos n)))
            (unless r (aset nlre--caps gn old))
            r)))
       ((eq tag :seq)
        (nlre--match-list (append (nth 1 nd) rest) s pos n))
       (t ;; plain atom
        (let ((p2 (nlre--match-atom1 nd s pos n)))
          (and p2 (nlre--match-list rest s p2 n))))))))

(defun nlre--seq-nodes (node)
  "Return NODE as a list of seq nodes (unwrap :seq / wrap :alt)."
  (cond ((eq (car node) :seq) (nth 1 node))
        (t (list node))))

(defun nlre--match-star (x rest s pos n)
  "Greedy star of atom/group X then REST."
  (or (let ((p2 (nlre--match-one x s pos n)))
        (and p2 (> p2 pos) (nlre--match-star x rest s p2 n)))
      (nlre--match-list rest s pos n)))

(defun nlre--match-one (x s pos n)
  "Match exactly one X (atom or group) at POS, no rest; return end or nil."
  (cond
   ((eq (car x) :group)
    (nlre--match-list
     (append (list (list :savestart (nth 1 x)))
             (nlre--seq-nodes (nth 2 x))
             (list (list :saveend (nth 1 x))))
     s pos n))
   ((memq (car x) '(:alt :seq))
    (nlre--match-list (list x) s pos n))
   (t (nlre--match-atom1 x s pos n))))

;; ---- public entry ----

(defun nlre-string-match (regexp string &optional start)
  "Pure-elisp `string-match'.  Return match start index, or nil.
Sets `nlre--match-data' (and host match-data when available via set-match-data)."
  (let* ((ast (nlre--parse regexp))
         (top (nlre--seq-nodes ast))
         (n (length string))
         (i (or start 0))
         (ng (1+ nlre--gcount))
         (hit nil))
    (while (and (not hit) (<= i n))
      (setq nlre--caps (make-vector ng nil))
      (let ((e (nlre--match-list top string i n)))
        (when e
          (aset nlre--caps 0 (cons i e))
          (setq hit i)))
      (unless hit (setq i (1+ i))))
    (when hit
      (setq nlre--last-caps nlre--caps)
      hit)))

(defvar nlre--last-caps nil "Capture vector of the last successful match.")

(defun nlre-match-beginning (n)
  (and nlre--last-caps (< n (length nlre--last-caps))
       (let ((c (aref nlre--last-caps n))) (and c (car c)))))
(defun nlre-match-end (n)
  (and nlre--last-caps (< n (length nlre--last-caps))
       (let ((c (aref nlre--last-caps n))) (and c (cdr c)))))

;; ---- regexp-dependent string helpers (built on nlre-string-match) ----

(defun nlre-split-string (string &optional separators omit-nulls)
  "Like `split-string'.  Default SEPARATORS = whitespace run, which also
implies OMIT-NULLS and leading/trailing trim (matching GNU Emacs)."
  (let* ((default (null separators))
         (sep (or separators "[ \f\t\n\r\v]+"))
         (omit (if default t omit-nulls))
         (len (length string))
         (start 0) (parts nil) (cont t))
    (while (and cont (<= start len) (nlre-string-match sep string start))
      (let ((mb (nlre-match-beginning 0)) (me (nlre-match-end 0)))
        (cond
         ((= me mb)
          ;; empty separator match: emit one char, advance, to avoid looping
          (if (>= mb len) (setq cont nil)
            (setq parts (cons (substring string start (1+ mb)) parts))
            (setq start (1+ mb))))
         (t
          (let ((piece (substring string start mb)))
            (unless (and omit (= (length piece) 0)) (setq parts (cons piece parts))))
          (setq start me)))))
    (let ((tail (substring string (min start len) len)))
      (unless (and omit (= (length tail) 0)) (setq parts (cons tail parts))))
    (let ((res (nreverse parts)))
      ;; whitespace default also trims a leading empty produced by a leading sep
      (when default
        (while (and res (= (length (car res)) 0)) (setq res (cdr res))))
      res)))

(defun nlre-replace-regexp-in-string (regexp rep string)
  "Subset of `replace-regexp-in-string': REP is a literal string or a
function of the matched substring.  No \\N backrefs in literal REP (v1)."
  (let ((out "") (pos 0) (len (length string)) (cont t))
    (while (and cont (<= pos len) (nlre-string-match regexp string pos))
      (let* ((mb (nlre-match-beginning 0)) (me (nlre-match-end 0))
             (matched (substring string mb me))
             (piece (if (stringp rep) rep (funcall rep matched))))
        (setq out (concat out (substring string pos mb) piece))
        (cond
         ((= me mb)
          (if (>= mb len) (setq cont nil)
            (setq out (concat out (substring string mb (1+ mb))))
            (setq pos (1+ mb))))
         (t (setq pos me)))))
    (concat out (substring string (min pos len) len))))

(provide 'nelisp-stdlib-regexp)
;;; nelisp-stdlib-regexp.el ends here
