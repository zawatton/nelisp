;;; nelisp-stdlib-regexp.el --- pure-elisp Emacs-regexp matcher  -*- lexical-binding: nil; -*-

;; Doc 143's dynamically bound parser/backtracker.  Continuations are explicit
;; and captures use :savestart/:saveend nodes, so no lexical closures are needed.
;; The nlre- names support host differential tests; standalone installs aliases.

;; ---- parser: pattern string -> node list (a "seq") ----

(defvar nlre--gcount 0 "Group counter during a parse.")

(defvar nlre--compiled-cache (make-hash-table :test 'equal)
  "Pattern-keyed buckets of compiled regexp cache entries.")
(defvar nlre--compiled-cache-count 0
  "Number of entries currently tracked in `nlre--compiled-cache'.")
(defvar nlre--compiled-cache-limit 1024
  "Maximum compiled regexp patterns retained by the LRU cache.
Doc T48d: a real init.el's `with-eval-after-load' feature-name checks
alone produce hundreds of distinct load-history-shaped patterns; at the
old limit of 32 essentially every one of them was a cache miss on every
pass, so raising this is what turns repeat passes over a wide corpus of
distinct patterns from all-cold into (after the first pass) all-warm.")
(defvar nlre--compiled-cache-lru nil)
(defvar nlre--compiled-cache-tick 0
  "Monotonic counter stamped onto a cache entry's TICK slot on add/touch;
higher means more recently used.  See `nlre--cache-evict'.")
(defvar nlre--match-end-limit nil
  "Dynamic exclusive ceiling for characters consumed by the matcher.
The full input length remains available for anchors and boundary context.")
(defvar nlre--compiled-cache-last nil
  "The most recently added-or-hit entry: an O(1) repeat-call shortcut for
`nlre--compiled-pattern' that does not depend on `nlre--compiled-cache-lru'
being recency-ordered (it is not; see `nlre--cache-touch').")
(defvar nlre--syntax-table-reader
  (cond ((fboundp 'current-syntax-table) 'current-syntax-table)
        ((fboundp 'syntax-table) 'syntax-table)))
(defvar nlre--compiled-cache-hits 0
  "Number of compiled regexp cache hits.")
(defvar nlre--compiled-cache-misses 0
  "Number of compiled regexp cache misses.")
(defvar nlre--string-match-calls 0
  "Number of calls to `nlre-string-match'.")
(defvar nlre--leading-filter-calls 0
  "Number of `nlre-string-match' calls that selected the leading filter.")
(defvar nlre--string-match-counter-file nil
  "When non-nil, file path receiving periodic `nlre-string-match' call counts.")
(defvar nlre--string-match-counter-interval 1000
  "Call interval for `nlre--string-match-counter-file' updates.")

(defun nlre--compiled-cache-clear ()
  "Clear the compiled regexp cache and its entry count."
  ;; `clrhash' costs about 100ms even when this runtime's table is empty.
  (setq nlre--compiled-cache (make-hash-table :test 'equal)
        nlre--compiled-cache-count 0
        nlre--compiled-cache-lru nil
        nlre--compiled-cache-last nil))

;; Entry: [PATTERN CASE-FOLD SYNTAX-TABLE COMPILED TICK].  `nlre--compiled-cache-lru'
;; is just the unordered list of all live entries, not an ordering; TICK
;; (a per-entry stamp from the monotonic `nlre--compiled-cache-tick'
;; counter) is what recency comparisons use instead (Doc T48d).  At the
;; old 32-entry limit a plain move-to-front list was fine; raised to
;; cover a real init.el's hundreds of distinct load-history-shaped
;; patterns, a HIT that has to relocate its entry via `delq' through a
;; list that long -- the common case once the corpus exceeds one
;; screenful of patterns, since the moved-to-front entry is essentially
;; never the next one looked up -- measured at tens of milliseconds,
;; not the sub-millisecond a warm hit must stay under.  Stamping TICK is
;; O(1); only eviction scans for the minimum, and eviction is paid by
;; cold misses, not hits.
(defun nlre--cache-find (pat fold table)
  (let ((xs (gethash pat nlre--compiled-cache)) hit)
    (while (and xs (not hit))
      (let ((e (car xs)))
        (if (and (eq fold (aref e 1)) (eq table (aref e 2)))
            (setq hit e)
          (setq xs (cdr xs)))))
    hit))

(defun nlre--cache-touch (e)
  (aset e 4 (setq nlre--compiled-cache-tick (1+ nlre--compiled-cache-tick)))
  (setq nlre--compiled-cache-last e))

(defun nlre--cache-evict ()
  (let* ((xs (cdr nlre--compiled-cache-lru)) (victim (car nlre--compiled-cache-lru))
         (vtick (aref victim 4)))
    (while xs
      (let ((e (car xs)))
        (when (< (aref e 4) vtick) (setq victim e vtick (aref e 4))))
      (setq xs (cdr xs)))
    (let* ((pat (aref victim 0)) (rest (delq victim (gethash pat nlre--compiled-cache))))
      (setq nlre--compiled-cache-lru (delq victim nlre--compiled-cache-lru))
      (if rest (puthash pat rest nlre--compiled-cache)
        (remhash pat nlre--compiled-cache))
      (setq nlre--compiled-cache-count (1- nlre--compiled-cache-count)))))

(defun nlre--cache-add (pat fold table compiled)
  ;; Key on a private copy: a caller may mutate PAT in place afterwards
  ;; (GNU recompiles on content), and a mutated key would both poison the
  ;; hash and make the `eq' head shortcut return a stale program.
  (setq pat (copy-sequence pat))
  (let ((e (vector pat fold table compiled
                   (setq nlre--compiled-cache-tick (1+ nlre--compiled-cache-tick)))))
    (puthash pat (cons e (gethash pat nlre--compiled-cache))
             nlre--compiled-cache)
    (setq nlre--compiled-cache-lru (cons e nlre--compiled-cache-lru)
          nlre--compiled-cache-count (1+ nlre--compiled-cache-count)
          nlre--compiled-cache-last e)
    (when (> nlre--compiled-cache-count nlre--compiled-cache-limit)
      (nlre--cache-evict))
    compiled))

(defun nlre--compiled-pattern (pat)
  "Return cached compiled representation for PAT.
The result is [AST GROUP-COUNT FAST-PLAN]."
  (let* ((fold (and case-fold-search t))
         (table (and nlre--syntax-table-reader
                     (funcall nlre--syntax-table-reader)))
         (head nlre--compiled-cache-last)
         (entry (if (and head (or (eq pat (aref head 0))
                                  (equal pat (aref head 0)))
                         (eq fold (aref head 1)) (eq table (aref head 2)))
                    head
                  (nlre--cache-find pat fold table))))
    (if entry
      (progn
          (setq nlre--compiled-cache-hits (1+ nlre--compiled-cache-hits))
          (unless (eq entry head) (nlre--cache-touch entry))
          (aref entry 3))
      (setq nlre--compiled-cache-misses (1+ nlre--compiled-cache-misses))
      (nlre--cache-add pat fold table (nlre--compile-pattern pat fold)))))

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
                   ;; A `?' after *, + or ? makes the quantifier non-greedy.
                   ;; It was consumed as a separate `:opt' over the quantified
                   ;; node, which is not the same thing: "a.*?b" against
                   ;; "axbxb" matched "ax" instead of "axb".
                   ((eq q ?*)
                    (if (and (< (1+ j) n) (eq (aref pat (1+ j)) ??))
                        (setq nodes (cons (list :lazystar atom) nodes) j (+ j 2))
                      (setq nodes (cons (list :star atom) nodes) j (1+ j))))
                   ((eq q ?+)
                    (if (and (< (1+ j) n) (eq (aref pat (1+ j)) ??))
                        (setq nodes (cons (list :lazyplus atom) nodes) j (+ j 2))
                      (setq nodes (cons (list :plus atom) nodes) j (1+ j))))
                   ((eq q ??)
                    (if (and (< (1+ j) n) (eq (aref pat (1+ j)) ??))
                        (setq nodes (cons (list :lazyopt atom) nodes) j (+ j 2))
                      (setq nodes (cons (list :opt atom) nodes) j (1+ j))))
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
         ((eq d ?\() ;; plain, shy, or explicitly numbered group
          (let ((k (+ i 2)) (shy nil) (explicit nil))
            (when (and (< k n) (eq (aref pat k) ?\?))
              (let ((m (1+ k)) (num nil))
                (while (and (< m n) (>= (aref pat m) ?0) (<= (aref pat m) ?9))
                  (setq num (+ (* (or num 0) 10) (- (aref pat m) ?0)))
                  (setq m (1+ m)))
                (when (and (< m n) (eq (aref pat m) ?:))
                  (if num (setq explicit num) (setq shy t))
                  (setq k (1+ m)))))
            (let ((gn (cond (shy nil)
                            (explicit explicit)
                            (t (setq nlre--gcount (1+ nlre--gcount))
                               nlre--gcount))))
              (when (and explicit (> explicit nlre--gcount))
                (setq nlre--gcount explicit))
              (let* ((r (nlre--parse-alt pat k n))
                     (inner (car r)) (j (cdr r)))
                (when (and (< (1+ j) n) (eq (aref pat j) ?\\) (eq (aref pat (1+ j)) ?\)))
                  (setq j (+ j 2)))
                (cons (if shy inner (list :group gn inner)) j)))))
         ((eq d ?w) (cons (list :word nil) (+ i 2)))
         ((eq d ?W) (cons (list :word t) (+ i 2)))
         ((eq d ?b) (cons (list :wordb nil) (+ i 2)))
         ((eq d ?B) (cons (list :wordb t) (+ i 2)))
         ((eq d ?<) (cons (list :wordedge nil) (+ i 2)))
         ((eq d ?>) (cons (list :wordedge t) (+ i 2)))
         ((and (eq d ?_) (< (+ i 2) n) (eq (aref pat (+ i 2)) ?<))
          (cons (list :symedge nil) (+ i 3)))
         ((and (eq d ?_) (< (+ i 2) n) (eq (aref pat (+ i 2)) ?>))
          (cons (list :symedge t) (+ i 3)))
         ((eq d ?s)
          (let ((j (+ i 2)) (class nil))
            (when (< j n) (setq class (aref pat j)) (setq j (1+ j)))
            (cons (list :syntax class nil) j)))
         ((eq d ?S)
          (let ((j (+ i 2)) (class nil))
            (when (< j n) (setq class (aref pat j)) (setq j (1+ j)))
            (cons (list :syntax class t) j)))
         ;; \1 .. \9: back reference to a numbered group.
         ((and (>= d ?1) (<= d ?9))
          (when (> (- d ?0) nlre--gcount)
            (signal 'invalid-regexp (list "Invalid back reference")))
          (cons (list :backref (- d ?0)) (+ i 2)))
         ((eq d 96) (cons (list :bos) (+ i 2)))  ;; \` = beginning of string
         ((eq d 39) (cons (list :eos) (+ i 2)))  ;; \' = end of string
         (t (cons (list :lit d) (+ i 2))))))
     (t (cons (list :lit c) (1+ i))))))

(defun nlre--posix-ranges (name)
  "Return a list of (lo . hi) ranges for POSIX class NAME, nil if unknown."
  (cond
   ((equal name "digit")  (list (cons ?0 ?9)))
   ((equal name "alpha")  (list (cons ?a ?z) (cons ?A ?Z)))
   ((equal name "alnum")  (list (cons ?0 ?9) (cons ?a ?z) (cons ?A ?Z)))
   ((equal name "word")   (list (cons ?0 ?9) (cons ?a ?z) (cons ?A ?Z) (cons ?_ ?_)))
   ((equal name "upper")  (list (cons ?A ?Z)))
   ((equal name "lower")  (list (cons ?a ?z)))
   ((equal name "xdigit") (list (cons ?0 ?9) (cons ?a ?f) (cons ?A ?F)))
   ((equal name "space")  (list (cons 9 13) (cons 32 32)))
   ((equal name "blank")  (list (cons 9 9) (cons 32 32)))
   ((equal name "punct")  (list (cons 33 47) (cons 58 64) (cons 91 96) (cons 123 126)))
   ((equal name "cntrl")  (list (cons 0 31) (cons 127 127)))
   ((equal name "graph")  (list (cons 33 126)))
   ((equal name "print")  (list (cons 32 126)))
   ((equal name "ascii")  (list (cons 0 127)))
   (t nil)))

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
           ;; POSIX class [:name:] -> expand to (lo . hi) ranges
           ((and (eq c ?\[) (< (1+ i) n) (eq (aref pat (1+ i)) ?:))
            (let ((j (+ i 2)))
              (while (and (< (1+ j) n)
                          (not (and (eq (aref pat j) ?:) (eq (aref pat (1+ j)) ?\]))))
                (setq j (1+ j)))
              (setq ranges (append (nlre--posix-ranges (substring pat (+ i 2) j)) ranges))
              (setq i (+ j 2))))
           ((and (< (+ i 2) n) (eq (aref pat (1+ i)) ?-) (not (eq (aref pat (+ i 2)) ?\])))
            (setq ranges (cons (cons c (aref pat (+ i 2))) ranges) i (+ i 3)))
           (t (setq ranges (cons (cons c c) ranges) i (1+ i)))))))
    (cons (list :set neg (nreverse ranges)) i)))

;; A fast plan is either a literal, or a finite language whose every variant
;; ends at `\\''.  The latter covers load-history and regexp-opt file names
;; without carrying the former general-purpose finite matcher in the image.
(defvar nlre--plan-limit 256)
(defvar nlre--plan-count 0)
(defvar nlre--plan-failed nil)
(defvar nlre--fast-plan-hits 0)

;; Fragment: [ATOMS CAPS BOS-OFFSET].  An atom is a literal
;; character or (:set RANGES); sets stay compact rather than expanding.
;;
;; ATOMS is stored in REVERSE match order (Doc T48d).  `nlre--plan-cat'
;; combines two fragments A then B by consing B's (typically short,
;; freshly-expanded) reversed atoms onto A's already-reversed atoms,
;; which are shared untouched -- the large, growing side of a
;; cross-product never gets copied.  Storing atoms forward instead would
;; force copying whichever side comes first (A, the growing
;; accumulator) at every combine; that copy, plus this runtime's own
;; fixed per-call cost for `append', is what made cold-compiling an
;; anchored multi-group pattern like the load-history shape
;; (`\\(\\`\\|/\\)NAME\\(\\.elc\\|\\.el\\|\\.so\\|\\)\\(\\.gz\\)?\\'')
;; cost far more than its final ~16 variants should need.  The only
;; reader of a fragment's ATOMS in forward order is `nlre--suffix-plan',
;; which un-reverses once per finished variant.
(defun nlre--plan-new (atoms caps bos)
  (setq nlre--plan-count (1+ nlre--plan-count))
  (when (> nlre--plan-count nlre--plan-limit)
    (setq nlre--plan-failed t))
  (vector atoms caps bos))

(defun nlre--plan-empty (ng)
  (nlre--plan-new nil (make-vector ng nil) nil))

;; Fresh copy of FRESH consed onto TAIL (TAIL shared, never mutated).
;; `cons'/`setcdr' cost far less per call here than `append' (Doc T48d).
(defun nlre--plan-prepend (fresh tail)
  (if (null fresh) tail
    (let* ((head (cons (car fresh) nil)) (last head) (rest (cdr fresh)))
      (while rest
        (let ((cell (cons (car rest) nil)))
          (setcdr last cell) (setq last cell rest (cdr rest))))
      (setcdr last tail)
      head)))

(defun nlre--plan-cat (a b)
  (let* ((off (length (aref a 0))) (ca (aref a 1)) (cb (aref b 1))
         (n (length ca)) (caps (make-vector n nil)) (i 0)
         (ab (aref a 2)) (bb (aref b 2))
         (bb (and bb (+ off bb))))
    (when (and ab bb (/= ab bb)) (setq nlre--plan-failed t))
    (while (< i n)
      (let ((x (aref ca i)) (y (aref cb i)))
        (aset caps i (if y (cons (+ off (car y)) (+ off (cdr y))) x)))
      (setq i (1+ i)))
    (nlre--plan-new (nlre--plan-prepend (aref b 0) (aref a 0)) caps (or ab bb))))

(defun nlre--plan-cross (as bs)
  (let (out)
    (while (and as (not nlre--plan-failed))
      (let ((ys bs))
        (while (and ys (not nlre--plan-failed))
          (setq out (cons (nlre--plan-cat (car as) (car ys)) out)
                ys (cdr ys))))
      (setq as (cdr as)))
    (nreverse out)))

(defun nlre--plan-seq (nodes ng)
  ;; A run of consecutive single-atom nodes (a literal character, or a
  ;; non-negated character set kept compact rather than expanded here --
  ;; the common case of a literal substring) always yields exactly one
  ;; fragment, with no cross-product branching at all -- so build it
  ;; directly in one pass instead of paying one
  ;; `nlre--plan-cross'/`nlre--plan-cat' call per character (Doc T48d).
  ;; Consing each atom's value onto ATOMS while scanning forward
  ;; produces the reversed order `nlre--plan-cat' expects, with no
  ;; separate reverse step.
  (let ((out (list (nlre--plan-empty ng))))
    (while (and nodes (not nlre--plan-failed))
      (let* ((tag (car (car nodes)))
             (single (or (eq tag :lit) (and (eq tag :set) (not (nth 1 (car nodes)))))))
        (if single
            (let ((atoms nil))
              (while (and nodes
                          (let ((tag (car (car nodes))))
                            (or (eq tag :lit)
                                (and (eq tag :set) (not (nth 1 (car nodes)))))))
                (setq atoms (cons (if (eq (car (car nodes)) :lit)
                                      (nth 1 (car nodes))
                                    (list :set (nth 2 (car nodes))))
                                  atoms)
                      nodes (cdr nodes)))
              (setq out (nlre--plan-cross
                         out (list (nlre--plan-new atoms (make-vector ng nil) nil)))))
          (setq out (nlre--plan-cross out (nlre--plan-expand (car nodes) ng))
                nodes (cdr nodes)))))
    out))

(defun nlre--plan-expand (node ng)
  (let ((tag (car node)))
    (cond
     ((eq tag :lit)
      (list (nlre--plan-new (list (nth 1 node))
                            (make-vector ng nil) nil)))
     ((eq tag :set)
      (if (nth 1 node) (progn (setq nlre--plan-failed t) nil)
        (list (nlre--plan-new (list (list :set (nth 2 node)))
                              (make-vector ng nil) nil))))
     ((eq tag :bos)
      (list (nlre--plan-new nil (make-vector ng nil) 0)))
     ((eq tag :seq) (nlre--plan-seq (nth 1 node) ng))
     ((eq tag :alt)
      ;; Process branches in reverse (once; there are few of them) and
      ;; prepend each per-branch fragment-list onto the accumulator, so
      ;; only ever-small per-branch chunks get copied -- never the
      ;; growing accumulator (same rationale as `nlre--plan-cat',
      ;; applied to lists of fragments instead of atoms).
      (let ((branches (reverse (nth 1 node))) out)
        (while (and branches (not nlre--plan-failed))
          (setq out (nlre--plan-prepend (nlre--plan-seq (car branches) ng) out)
                branches (cdr branches)))
        out))
     ((eq tag :group)
      (let ((xs (nlre--plan-expand (nth 2 node) ng)) (gn (nth 1 node)))
        (let ((rest xs))
          (while rest
            (aset (aref (car rest) 1) gn
                  (cons 0 (length (aref (car rest) 0))))
            (setq rest (cdr rest))))
        xs))
     ((eq tag :opt)
      (append (nlre--plan-expand (nth 1 node) ng)
              (list (nlre--plan-empty ng))))
     ((eq tag :lazyopt)
      (cons (nlre--plan-empty ng) (nlre--plan-expand (nth 1 node) ng)))
     (t (setq nlre--plan-failed t) nil))))

(defun nlre--fold-text (s fold)
  (if (not fold) s
    (let ((i 0) (n (length s)) (out ""))
      (while (< i n)
        (let ((c (aref s i)))
          (setq out (concat out (char-to-string
                                 (if (< c 128)
                                     (if (and (>= c ?A) (<= c ?Z)) (+ c 32) c)
                                   (nlre--downcase-char c))))))
        (setq i (1+ i)))
      out)))

(defun nlre--atom-strings (atoms)
  (let ((out (list "")))
    (while (and atoms (not nlre--plan-failed))
      (let ((prefixes out) next (atom (car atoms)))
        (while prefixes
          (if (integerp atom)
              (setq next (cons (concat (car prefixes)
                                       (char-to-string atom)) next))
            (let ((ranges (nth 1 atom)))
              (while ranges
                (let ((c (car (car ranges))) (hi (cdr (car ranges))))
                  (while (<= c hi)
                    (setq nlre--plan-count (1+ nlre--plan-count)
                          next (cons (concat (car prefixes)
                                             (char-to-string c)) next)
                          c (1+ c))))
                (setq ranges (cdr ranges)))))
          (setq prefixes (cdr prefixes)))
        (setq out (nreverse next) atoms (cdr atoms))
        (when (> nlre--plan-count nlre--plan-limit)
          (setq nlre--plan-failed t))))
    out))

(defun nlre--suffix-entry-add (entry xs)
  "Insert ENTRY into XS by decreasing suffix length."
  (let ((n (length (car entry))))
  (cond ((null xs) (list entry))
          ((>= n (length (car (car xs)))) (cons entry xs))
          (t (cons (car xs) (nlre--suffix-entry-add entry (cdr xs)))))))

(defun nlre--literal-text (ast)
  (and (eq (car ast) :seq)
       (let ((nodes (nth 1 ast)) (out "") (ok t))
         (while (and nodes ok)
           (if (eq (car (car nodes)) :lit)
               (setq out (concat out (char-to-string (nth 1 (car nodes))))
                     nodes (cdr nodes))
             (setq ok nil)))
         (and ok out))))

(defun nlre--suffix-plan (ast ng fold)
  (let ((nodes (and (eq (car ast) :seq) (nth 1 ast))))
    (when (and nodes (eq (car (car (last nodes))) :eos))
      (let ((nlre--plan-count 0) (nlre--plan-failed nil))
        (let ((variants (nlre--plan-seq (butlast nodes) ng)))
          (when (and variants (not nlre--plan-failed))
            (let ((ends (make-hash-table :test 'equal)) empty
                  (xs (reverse variants)))
              (while (and xs (not nlre--plan-failed))
                (let ((v (car xs))
                      (strings (nlre--atom-strings (reverse (aref (car xs) 0)))))
                  (while strings
                    (let* ((text (nlre--fold-text (car strings) fold))
                           (len (length text)) (entry (cons text v)))
                      (if (= len 0) (setq empty (cons entry empty))
                        (let* ((last (aref text (1- len)))
                               (key (if (= len 1) last
                                      (+ last (* 1114112
                                                 (1+ (aref text (- len 2))))))))
                          (puthash key
                                   (nlre--suffix-entry-add
                                    entry (gethash key ends))
                                   ends)))
                      (setq strings (cdr strings)))))
                (setq xs (cdr xs)))
              (and (not nlre--plan-failed)
                   (vector :suffix ends fold empty)))))))))

;; A single fixed-width atom with no group/anchor/quantifier (one bare
;; `.', `[...]', `\w'/`\W', `\s-'/`\S-', or literal char) matches the same
;; way `nlre--match-atom1' already does for one candidate position, so
;; scanning positions directly is cheap and needs no new matching logic;
;; the tags listed here are exactly the ones `nlre--match-atom1' returns
;; `(1+ pos)' for on a hit -- always exactly one character wide -- so the
;; caller can compute the match end as START+1 without re-deriving it.
;; `:group' is deliberately excluded: it is how this parser tracks capture
;; groups, so keeping it out of this whitelist keeps `ng' at 1 whenever the
;; fast plan applies (no capture bookkeeping needed).
(defun nlre--single-atom-node (ast)
  "If AST is exactly one fixed-width, capture-free, quantifier-free atom,
return that atom node; else nil."
  (and (eq (car ast) :seq)
       (let ((nodes (nth 1 ast)))
         (and nodes (null (cdr nodes))
              (memq (car (car nodes)) '(:lit :any :set :word :space :syntax))
              (car nodes)))))

(defun nlre--compile-pattern (pat fold)
  (let* ((ast (nlre--parse pat)) (ng (1+ nlre--gcount))
         (literal (nlre--literal-text ast))
         (atom (and (not literal) (nlre--single-atom-node ast))))
    (vector ast ng
            (cond
             (literal (vector :literal (nlre--fold-text literal fold) fold))
             (atom (vector :atom atom))
             (t (nlre--suffix-plan ast ng fold))))))

;; ---- matcher (no closures; rest threaded explicitly) ----

(defvar nlre--caps nil "Vector of (start . end) per group during a match.")

;; Fold only comparison operands; rewriting the regexp would invert \W/\B.
(defvar nlre--fold nil "Non-nil while the current match folds case.")

(defun nlre--downcase-char (c)
  "Non-ASCII case fold of C.  A separate function so the hot callers below
keep only allowlisted bytecode opcodes (the inline `downcase' opcode is not
one, which would demote them to the interpreter)."
  (downcase c))

(defun nlre--upcase-char (c)
  "Non-ASCII upcase of C; see `nlre--downcase-char'."
  (upcase c))

(defun nlre--fold-char (c)
  ;; ASCII folds inline; anything above goes through `downcase', the same
  ;; case table GNU's translate table is built from.
  (if nlre--fold
      (if (< c 128)
          (if (and (>= c ?A) (<= c ?Z)) (+ c 32) c)
        (nlre--downcase-char c))
    c))

(defun nlre--flip-case (c)
  (cond ((and (>= c ?a) (<= c ?z)) (- c 32))
        ((and (>= c ?A) (<= c ?Z)) (+ c 32))
        ((< c 128) c)
        (t (let ((d (nlre--downcase-char c))) (if (eq d c) (nlre--upcase-char c) d)))))

(defun nlre--space-p (c) (or (= c 32) (= c 9) (= c 10) (= c 13) (= c 12)))
;; `_` is a symbol constituent, not a word constituent.
(defun nlre--word-p (c)
  (or (and (>= c ?a) (<= c ?z)) (and (>= c ?A) (<= c ?Z))
      (and (>= c ?0) (<= c ?9))))

(defun nlre--symbol-p (c)
  (or (nlre--word-p c) (= c ?_)))

;; Unknown syntax classes deliberately match nothing.
(defun nlre--syntax-p (class c)
  (cond ((eq class ?w) (nlre--word-p c))
        ((eq class ?_) (= c ?_))
        ((or (eq class ?-) (eq class 32)) (nlre--space-p c))
        ((eq class ?.) (and (> c 32) (< c 127)
                            (not (nlre--word-p c)) (/= c ?_)
                            (not (memq c '(?\( ?\) ?\[ ?\] ?{ ?} ?\" ?\\)))))
        ((eq class ?\() (memq c '(?\( ?\[ ?{)))
        ((eq class ?\)) (memq c '(?\) ?\] ?})))
        ((eq class ?\") (= c ?\"))
        ((eq class ?\\) (= c ?\\))
        (t nil)))

(defun nlre--set-in-ranges (ranges c)
  (let ((hit nil) (rs ranges))
    (while (and rs (not hit))
      (when (and (>= c (car (car rs))) (<= c (cdr (car rs)))) (setq hit t))
      (setq rs (cdr rs)))
    hit))

(defun nlre--set-match (neg ranges c)
  (let ((hit (or (nlre--set-in-ranges ranges c)
                 (and nlre--fold
                      (nlre--set-in-ranges ranges (nlre--flip-case c))))))
    (if neg (not hit) hit)))

(defun nlre--literal-search (needle string start)
  (cond
   ((and (fboundp 'nelisp--string-search)
         (not (multibyte-string-p needle))
         (not (multibyte-string-p string)))
    (nelisp--string-search needle string start))
   ((fboundp 'string-search) (string-search needle string start))
   (t
    (let ((i start) (n (length string)) (m (length needle)) hit)
      (while (and (not hit) (<= (+ i m) n))
        (if (string= needle (substring string i (+ i m)))
            (setq hit i)
          (setq i (1+ i))))
      hit))))

(defun nlre--literal-plan-match (plan string start n)
  (let ((needle (aref plan 1)) (fold (aref plan 2)))
    (if (not fold) (nlre--literal-search needle string start)
      (let ((m (length needle)))
        (cond
         ((= m 0) start)
         ;; A one-character needle needs no post-hoc substring/fold-text
         ;; verification: fold(string[P]) = fold(c) iff string[P] is C or
         ;; its flipped case, so the leftmost of two single-char native
         ;; searches from START IS the leftmost match, full stop.  This
         ;; also skips the multi-char loop's own upfront exact-needle
         ;; search, which for M=1 is redundant with the C search below.
         ((= m 1)
          (let* ((c (aref needle 0)) (other (nlre--flip-case c)))
            (if (eq c other) (nlre--literal-search needle string start)
              (let ((p (nlre--literal-search (char-to-string c) string start))
                    (q (nlre--literal-search (char-to-string other) string start)))
                (cond ((null p) q) ((null q) p) ((< p q) p) (t q))))))
         (t
          (let* ((c (aref needle 0)) (other (nlre--flip-case c))
                 (pos start) (exact (nlre--literal-search needle string start))
                 hit)
            (while (and (not hit) (<= (+ pos m) n))
              (let ((p (nlre--literal-search (char-to-string c) string pos))
                    (q (and (/= c other)
                            (nlre--literal-search
                             (char-to-string other) string pos))) at)
                (setq at (cond ((null p) q) ((null q) p)
                               ((< p q) p) (t q)))
                (if (or (null at) (> (+ at m) n))
                    (setq pos (1+ n))
                  (let ((piece (and (not (eq at exact))
                                    (substring string at (+ at m)))))
                    (if (or (eq at exact)
                            (string= piece needle)
                            (string= (nlre--fold-text piece t) needle))
                        (setq hit at)
                      (setq pos (1+ at)))))))
            hit)))))))

(defun nlre--suffix-plan-match (plan string start n)
  (let ((fold (aref plan 2)) (empty (aref plan 3)) xs short hit)
    (when (> n start)
      (let ((last (aref string (1- n))))
        (when (and fold (>= last ?A) (<= last ?Z))
          (setq last (+ last 32)))
        (setq short (gethash last (aref plan 1)))
        (when (> (- n start) 1)
          (let ((prev (aref string (- n 2))))
            (when (and fold (>= prev ?A) (<= prev ?Z))
              (setq prev (+ prev 32)))
            (setq xs (gethash (+ last (* 1114112 (1+ prev)))
                              (aref plan 1)))))))
    (while (and (not hit) (or xs short))
      (unless xs (setq xs short short nil))
      (let* ((entry (car xs)) (text (car entry)) (variant (cdr entry))
             (pos (- n (length text))) (bos (aref variant 2)))
        (when (and (>= pos start) (or (null bos) (= (+ pos bos) 0))
                   (if fold
                       (string= text (nlre--fold-text (substring string pos n) t))
                     (eq pos (nlre--literal-search text string pos))))
          (setq hit (cons pos variant))))
      (setq xs (cdr xs)))
    (when (and (not hit) empty)
      (let* ((variant (cdr (car empty))) (bos (aref variant 2)))
        (when (or (null bos) (= (+ n bos) 0))
          (setq hit (cons n variant)))))
    hit))

;; PLAN is [:atom NODE] (see `nlre--single-atom-node').  `nlre--fold' is
;; already dynamically bound by `nlre-string-match' for the whole call, so
;; `nlre--match-atom1' (via `nlre--fold-char'/`nlre--set-match') sees the
;; right case-fold state without this function re-binding anything.
(defun nlre--atom-plan-match (plan string start n)
  (let ((node (aref plan 1)) (i start) hit)
    (while (and (not hit) (< i n))
      (if (nlre--match-atom1 node string i n) (setq hit i) (setq i (1+ i))))
    hit))

(defun nlre--plan-set-caps (caps variant start end)
  (aset caps 0 (cons start end))
  (let ((relative (aref variant 1)) (i 1) (n (length caps)))
    (while (< i n)
      (let ((span (aref relative i)))
        (aset caps i
              (and span (cons (+ start (car span)) (+ start (cdr span))))))
      (setq i (1+ i)))))

(defun nlre--can-consume-p (pos n)
  "Return non-nil when a character at POS is within the matcher bounds."
  (< pos (if nlre--match-end-limit
             (min n nlre--match-end-limit)
           n)))

(defun nlre--match-atom1 (node s pos n)
  "Match a single non-quantified atom NODE at POS; return end-pos or nil.
Does NOT continue to any rest (used for one repetition)."
  (let ((tag (car node)))
    (cond
     ((eq tag :lit) (and (nlre--can-consume-p pos n)
                         (eq (nlre--fold-char (aref s pos))
                             (nlre--fold-char (nth 1 node)))
                         (1+ pos)))
     ((eq tag :any) (and (nlre--can-consume-p pos n) (not (eq (aref s pos) ?\n)) (1+ pos)))
     ((eq tag :set) (and (nlre--can-consume-p pos n) (nlre--set-match (nth 1 node) (nth 2 node) (aref s pos)) (1+ pos)))
     ((eq tag :word) (and (nlre--can-consume-p pos n) (let ((w (nlre--word-p (aref s pos)))) (if (nth 1 node) (not w) w)) (1+ pos)))
     ((eq tag :space) (and (nlre--can-consume-p pos n) (let ((w (nlre--space-p (aref s pos)))) (if (nth 1 node) (not w) w)) (1+ pos)))
     ((eq tag :syntax)
      (and (nlre--can-consume-p pos n)
           (let ((m (nlre--syntax-p (nth 1 node) (aref s pos))))
             (if (nth 2 node) (not m) m))
           (1+ pos)))
     ((eq tag :wordedge)
      (let ((before (and (> pos 0) (nlre--word-p (aref s (1- pos))) t))
            (after (and (< pos n) (nlre--word-p (aref s pos)) t)))
        (and (if (nth 1 node) (and before (not after)) (and after (not before)))
             pos)))
     ((eq tag :symedge)
      (let ((before (and (> pos 0) (nlre--symbol-p (aref s (1- pos))) t))
            (after (and (< pos n) (nlre--symbol-p (aref s pos)) t)))
        (and (if (nth 1 node) (and before (not after)) (and after (not before)))
             pos)))
     ((eq tag :wordb)
      (let* ((before (and (> pos 0) (nlre--word-p (aref s (1- pos))) t))
             (after (and (< pos n) (nlre--word-p (aref s pos)) t))
             (boundary (not (eq before after))))
        (and (if (nth 1 node) (not boundary) boundary) pos)))
     ((eq tag :backref)
      ;; Matches the text the numbered group last captured (fold-aware);
      ;; an unset group never matches, as in GNU.
      (let ((cap (aref nlre--caps (nth 1 node))))
        (and cap (car cap) (cdr cap)
             (let* ((from (car cap)) (len (- (cdr cap) from)) (end (+ pos len))
                    (lim (if nlre--match-end-limit (min n nlre--match-end-limit) n))
                    (i 0))
               (and (<= end lim)
                    (progn
                      (while (and (< i len)
                                  (eq (nlre--fold-char (aref s (+ from i)))
                                      (nlre--fold-char (aref s (+ pos i)))))
                        (setq i (1+ i)))
                      (= i len))
                    end)))))
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
       ;; Non-greedy: try the REST first, and only consume another repetition
       ;; when that fails -- the mirror image of `nlre--match-star'.
       ((eq tag :lazystar)
        (or (nlre--match-list rest s pos n)
            (let ((p2 (nlre--match-one (nth 1 nd) s pos n)))
              (and p2 (> p2 pos) (nlre--match-list (cons nd rest) s p2 n)))))
       ((eq tag :lazyplus)
        (nlre--match-list
         (cons (nth 1 nd) (cons (list :lazystar (nth 1 nd)) rest)) s pos n))
       ((eq tag :lazyopt)
        (or (nlre--match-list rest s pos n)
            (nlre--match-list (cons (nth 1 nd) rest) s pos n)))
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

;; Reuse one capture vector per scan and reject impossible leading characters
;; before entering the backtracker (Doc 201 §5.4).
(defun nlre--leading-lit-char (nodes)
  "Return the one character every match of NODES must start with, or nil."
  (and (consp nodes)
       (let ((nd (car nodes)))
         (and (consp nd) (eq (car nd) :lit) (nth 1 nd)))))

;; PERF (Doc 205 split-string follow-up, 2026-09-28): `nlre--leading-lit-char'
;; only ever fires for a bare `:lit', so a pattern like "[ \t\n]+" -- a
;; `:plus' of a `:set', with no single leading literal -- got no filter at
;; all: every rejected position paid a full `nlre--match-list' `:plus'
;; rewrite-and-recurse.  `:plus'/`:lazyplus' require >=1 repetition, so the
;; wrapped atom must hold at the candidate start exactly like a bare `:lit'
;; does; `:star'/`:opt'/lazy variants are excluded below because they allow
;; zero repetitions, so a match can start there even when the atom itself
;; does not.  The tags matched here are the same fixed-width, capture-free
;; ones `nlre--single-atom-node' already trusts for its own fast plan.
(defun nlre--leading-required-atom (nodes)
  "Return the leading fixed-width atom NODES's first match must satisfy at
its start position, or nil.  This is either a bare `:lit'/`:any'/`:set'/
`:word'/`:space'/`:syntax' node, or one wrapped in a `:plus'/`:lazyplus'."
  (and (consp nodes)
       (let ((nd (car nodes)))
         (and (consp nd)
              (cond
               ((memq (car nd) '(:lit :any :set :word :space :syntax)) nd)
               ((memq (car nd) '(:plus :lazyplus))
                (let ((inner (nth 1 nd)))
                  (and (consp inner)
                       (memq (car inner) '(:lit :any :set :word :space :syntax))
                       inner)))
               (t nil))))))

(defun nlre--caps-clear (v)
  "Set every slot of vector V to nil.
`fillarray' is not available on the standalone reader prelude."
  (let ((k (length v)))
    (while (> k 0)
      (setq k (1- k))
      (aset v k nil))))

(defun nlre-string-match (regexp string &optional start max-end)
  "Pure-elisp `string-match'; return match start index, or nil.
START is the first candidate start.  If MAX-END is non-nil, require the
match to end at or before that index while preserving anchors against all
of STRING.  Sets `nlre--match-data' as usual."
  (setq nlre--string-match-calls (1+ nlre--string-match-calls))
  (when (and nlre--string-match-counter-file
             (= (mod nlre--string-match-calls nlre--string-match-counter-interval) 0)
             (fboundp 'nl-write-file))
    (nl-write-file nlre--string-match-counter-file
                   (format "%d" nlre--string-match-calls)))
  (let* ((nlre--fold case-fold-search)
         (compiled (nlre--compiled-pattern regexp))
         (n (length string))
         (limit (if max-end (max 0 (min n max-end)) n))
         (nlre--match-end-limit limit)
         (i (or start 0))
         (ng (aref compiled 1))
         (plan (aref compiled 2))
         ;; Capture state is shared only between attempts in this call.
         (caps (make-vector ng nil))
         (hit nil))
    (setq nlre--caps caps)
    (cond
     ((and (null max-end) plan (eq (aref plan 0) :literal))
      (setq nlre--fast-plan-hits (1+ nlre--fast-plan-hits)
            hit (nlre--literal-plan-match plan string i n))
      (when hit (aset caps 0 (cons hit (+ hit (length (aref plan 1)))))))
     ((and (null max-end) plan (eq (aref plan 0) :suffix))
      (setq nlre--fast-plan-hits (1+ nlre--fast-plan-hits))
      (let ((result (nlre--suffix-plan-match plan string i n)))
        (when result
          (setq hit (car result))
          (nlre--plan-set-caps caps (cdr result) hit n))))
     ((and (null max-end) plan (eq (aref plan 0) :atom))
      (setq nlre--fast-plan-hits (1+ nlre--fast-plan-hits)
            hit (nlre--atom-plan-match plan string i n))
      (when hit (aset caps 0 (cons hit (1+ hit)))))
     (t
      (let* ((top (nlre--seq-nodes (aref compiled 0)))
             (lead (nlre--leading-lit-char top))
             (lead (and lead (nlre--fold-char lead)))
             (req-atom (and (not lead) (nlre--leading-required-atom top))))
        (when (or lead req-atom)
          (setq nlre--leading-filter-calls (1+ nlre--leading-filter-calls)))
        (cond
         (lead
          (while (and (not hit) (< i limit))
            (if (not (eq (nlre--fold-char (aref string i)) lead))
                (setq i (1+ i))
              (when (> ng 1) (nlre--caps-clear caps))
              (let ((e (nlre--match-list top string i n)))
                (if e
                    (progn (aset caps 0 (cons i e)) (setq hit i))
                  (setq i (1+ i)))))))
         (req-atom
          ;; Same cheap-reject idea as the `:lit' branch above, generalized
          ;; to any single-char atom (see `nlre--leading-required-atom').
          ;; Inlined rather than calling `nlre--match-atom1' -- that call
          ;; would repeat the bounds check this loop's own `(< i limit)'
          ;; already guarantees, and this loop runs once per rejected
          ;; character, so each avoided function call matters.
          (let ((tag (car req-atom)))
            (while (and (not hit) (< i limit))
              (let ((c (aref string i)))
                (if (not (cond
                          ((eq tag :lit)
                           (eq (nlre--fold-char c) (nlre--fold-char (nth 1 req-atom))))
                          ((eq tag :any) (not (eq c ?\n)))
                          ((eq tag :set) (nlre--set-match (nth 1 req-atom) (nth 2 req-atom) c))
                          ((eq tag :word)
                           (let ((w (nlre--word-p c))) (if (nth 1 req-atom) (not w) w)))
                          ((eq tag :space)
                           (let ((w (nlre--space-p c))) (if (nth 1 req-atom) (not w) w)))
                          ((eq tag :syntax)
                           (let ((m (nlre--syntax-p (nth 1 req-atom) c)))
                             (if (nth 2 req-atom) (not m) m)))
                          (t nil)))
                    (setq i (1+ i))
                  (when (> ng 1) (nlre--caps-clear caps))
                  (let ((e (nlre--match-list top string i n)))
                    (if e
                        (progn (aset caps 0 (cons i e)) (setq hit i))
                      (setq i (1+ i)))))))))
         ;; A pattern that starts with \` can only match at index 0, and one
         ;; that starts with ^ only at index 0 or right after a newline.
         ;; Without these two arms every rejected position paid a full
         ;; `nlre--match-list' attempt, so a failing "^ZZZ" scan cost the
         ;; same per character as a real match attempt.
         ((eq (car-safe (car top)) :bos)
          (when (= i 0)
            (when (> ng 1) (nlre--caps-clear caps))
            (let ((e (nlre--match-list top string 0 n)))
              (when e
                (aset caps 0 (cons 0 e))
                (setq hit 0)))))
         ((eq (car-safe (car top)) :bol)
          (while (and (not hit) (<= i limit))
            (if (or (= i 0) (eq (aref string (1- i)) ?\n))
                (progn
                  (when (> ng 1) (nlre--caps-clear caps))
                  (let ((e (nlre--match-list top string i n)))
                    (if e
                        (progn (aset caps 0 (cons i e)) (setq hit i))
                      (setq i (1+ i)))))
              ;; Jump to the position after the next newline (or past LIMIT).
              (let ((nl (nlre--literal-search "\n" string i)))
                (setq i (if nl (1+ nl) (1+ limit)))))))
         (t
          (while (and (not hit) (<= i limit))
            (when (> ng 1) (nlre--caps-clear caps))
            (let ((e (nlre--match-list top string i n)))
              (if e
                  (progn (aset caps 0 (cons i e)) (setq hit i))
                (setq i (1+ i))))))))))
    (when hit
      (setq nlre--last-caps caps)
      hit)))

(defvar nlre--last-caps nil "Capture vector of the last successful match.")

(defun nlre-match-beginning (n)
  (and nlre--last-caps (< n (length nlre--last-caps))
       (let ((c (aref nlre--last-caps n))) (and c (car c)))))
(defun nlre-match-end (n)
  (and nlre--last-caps (< n (length nlre--last-caps))
       (let ((c (aref nlre--last-caps n))) (and c (cdr c)))))

(defun nlre--caps-offset (offset)
  (let ((i 0) (n (length nlre--last-caps)))
    (while (< i n)
      (let ((span (aref nlre--last-caps i)))
        (when span
          (aset nlre--last-caps i
                (cons (+ offset (car span)) (+ offset (cdr span))))))
      (setq i (1+ i)))))

;; PERF (search-forward quadratic-cost fix, 2026-09-28): each of the three
;; functions below used to hand `nlre-string-match' a freshly built
;; `(buffer-substring base LIMIT)' -- a copy of everything from point out
;; to BOUND/`point-max', even when the match (or non-match) lay a handful
;; of characters away.  On a buffer with no BOUND that copy was the whole
;; remaining buffer, EVERY call, so a loop like `(while (search-forward
;; "\n" nil t) ...)' paid O(buffer size) per match instead of O(distance
;; to the next match): measured 36.7s for an 80KB buffer vs. 0.11s for a
;; 2KB buffer of the same shape (should be ~linear, i.e. ~40x, not ~334x).
;; `nlre-string-match' already takes START/MAX-END to scan a window of an
;; existing string without copying it, so the fix is to hand it `(buffer-
;; string)' (itself memoized per buffer against a content tick -- see
;; `nelisp-buffer-string' in src/nelisp-buffer.el / this file's own
;; scripts/nelisp-stdlib-prelude.el mirror) instead of a fresh substring,
;; with START/MAX-END doing the windowing that used to require a copy.
;; `nlre--caps-offset' then wants 1 (0-based full-string index -> 1-based
;; buffer position), not BASE, since index 0 of the whole-buffer string is
;; always buffer position 1, regardless of where the scan started.
;;
;; `nelisp-buffer-string' always returns BUF's UNRESTRICTED text, so a
;; narrowed buffer takes the pre-fix path (`buffer-substring' over
;; exactly [BASE, LIMIT)): narrowing is not exercised by this fix's own
;; timing case and is left at its original, correctness-preserving
;; cost.  `nlre--buffer-narrowed-p' below decides which path applies,
;; using only already-public names so this file does not need to know
;; about `nelisp-buffer''s narrow-start/narrow-end slots directly.
;;
;; One side effect, verified against Emacs 31.1 rather than assumed: the
;; pre-fix substring truncated to BOUND also truncated what `$'/`\'' saw
;; as "the end of the string", so `(re-search-forward "hello$" 6 t)' on
;; buffer text "helloworld" incorrectly matched at BOUND=6 (there is no
;; newline or buffer end there).  Real Emacs's anchors ignore BOUND (they
;; only care about a real newline or true `point-max'), and passing the
;; untruncated whole-buffer string now agrees with that -- a correctness
;; improvement of the same fix, not a separate change.
(defun nlre--buffer-narrowed-p ()
  "Non-nil if the current buffer's accessible range is not the whole
buffer."
  (or (/= (point-min) 1) (/= (point-max) (1+ (buffer-size)))))

(defun nlre--looking-at (regexp)
  (let* ((base (point))
         (narrowed (nlre--buffer-narrowed-p))
         (hit (if narrowed
                  (nlre-string-match regexp (buffer-substring base (point-max)) 0)
                (nlre-string-match regexp (buffer-string) (1- base) (1- (point-max))))))
    (when (and hit (= hit (if narrowed 0 (1- base))))
      (nlre--caps-offset (if narrowed base 1))
      t)))

(defun nlre--re-search-forward (regexp &optional bound noerror count)
  ;; Two pre-existing (not perf-related) parity gaps found while adding
  ;; the timing/parity matrix for the fix above; both probed against
  ;; Emacs 31.1 rather than assumed, and both fixed here since they sit
  ;; in the exact function the perf fix already touches.  Neither was
  ;; reachable via the search-forward/search-backward literal-string
  ;; wrappers before now, since `search-backward' itself did not exist
  ;; (see `search-backward' in scripts/nelisp-stdlib-prelude.el).
  ;;
  ;; 1) COUNT 0 used to fall through to `(or count 1)' treating 0 as
  ;;    itself (0 is non-nil in Lisp), leaving the search loop's `left'
  ;;    at 0 -- no iterations, `result' stays nil, and NOERROR t then
  ;;    answered nil instead of real Emacs's unconditional "no-op,
  ;;    return point" for COUNT 0.  Probed against Emacs 31.1: true
  ;;    under NOERROR nil/t/other alike, AND even with an otherwise
  ;;    invalid (wrong-side) BOUND -- 0 repetitions never looks at BOUND
  ;;    at all -- so this check must come before check 2 below, not
  ;;    after it.
  (if (eq count 0)
      (point)
    ;; 2) BOUND on the wrong side of point signalled nothing at all here
    ;;    (`re-search-backward' already has the mirror-image check for
    ;;    its own direction) -- real Emacs signals plain `error'
    ;;    "Invalid search bound (wrong side of point)" UNCONDITIONALLY,
    ;;    even under NOERROR t (NOERROR only covers a search that fails
    ;;    to match, not an invalid BOUND).  A BOUND past `point-max' is
    ;;    fine (Emacs just clamps it via `point-max' below, same as this
    ;;    function already did before this comment).
    (when (and bound (< bound (point)))
      (signal 'error (list "Invalid search bound (wrong side of point)")))
    (let ((left (or count 1)) (limit (or bound (point-max))) result)
      (while (> left 0)
        (let* ((base (point))
               (narrowed (nlre--buffer-narrowed-p))
               (hit (if narrowed
                        (nlre-string-match regexp (buffer-substring base limit) 0)
                      (nlre-string-match regexp (buffer-string) (1- base) (1- limit)))))
          (if (null hit) (setq left 0 result nil)
            (nlre--caps-offset (if narrowed base 1))
            (goto-char (nlre-match-end 0))
            (setq result (point) left (1- left)))))
      (if result result
        (if noerror
            (progn (unless (eq noerror t) (goto-char limit)) nil)
          (signal 'search-failed (list regexp)))))))

(defun nlre--regex-last-match-before (regexp string lower max-end)
  "Return the rightmost (START . CAPS) match in STRING.
LOWER is the minimum candidate start and MAX-END bounds match consumption.
The full STRING remains visible to anchors and boundary assertions."
  (let* ((nlre--fold case-fold-search)
         (compiled (nlre--compiled-pattern regexp))
         (nlre--match-end-limit max-end)
         (n (length string))
         (ng (aref compiled 1))
         (nodes (nlre--seq-nodes (aref compiled 0)))
         (caps (make-vector ng nil))
         (pos max-end)
         (result nil))
    (setq nlre--caps caps)
    (while (and (null result) (>= pos lower))
      (when (> ng 1) (nlre--caps-clear caps))
      (let ((end (nlre--match-list nodes string pos n)))
        (when end
          (aset caps 0 (cons pos end))
          (setq result (cons pos caps))))
      (setq pos (1- pos)))
    result))

(defun nlre--re-search-backward (regexp &optional bound noerror count)
  "Search backward for REGEXP, preserving full-buffer anchor context.
This uses `nlre-string-match' backtracking with a separate match-end ceiling."
  (setq count (or count 1))
  (unless (integerp count)
    (signal 'wrong-type-argument (list 'integerp count)))
  (if (= count 0)
      (point)
    (when (and bound (markerp bound))
      (setq bound (marker-position bound)))
    (when (and bound (not (integerp bound)))
      (signal 'wrong-type-argument (list 'integer-or-marker-p bound)))
    (let* ((origin (point))
           (base (point-min))
           (end (point-max))
           (direction (if (< count 0) 1 -1))
           (limit (if bound
                      (progn
                        (when (if (> direction 0) (< bound origin) (> bound origin))
                          (error "Invalid search bound (wrong side of point)"))
                        (max base (min end bound)))
                    (if (> direction 0) end base))))
      (if (> direction 0)
          (nlre--re-search-forward regexp limit noerror (- count))
        ;; See the PERF block comment above `nlre--looking-at': BASE/END
        ;; here are always `(point-min)'/`(point-max)', so when the buffer
        ;; is not narrowed they already denote the whole accessible text
        ;; -- `(buffer-string)' (memoized) is exactly that text, without
        ;; re-`concat'ing it on every call the way `buffer-substring' did.
        ;; OFF converts a 0-based index of whichever string was chosen
        ;; back to an absolute 1-based buffer position, generalizing the
        ;; original's hardcoded BASE.
        (let* ((narrowed (nlre--buffer-narrowed-p))
               (old-caps nlre--last-caps)
               (text (if narrowed (buffer-substring base end) (buffer-string)))
               (off (if narrowed base 1))
               (lower (- limit off))
               (cursor (- origin off))
               (remaining count)
               (last-match nil)
               (failed nil))
          (while (and (> remaining 0) (not failed))
            (setq last-match
                  (nlre--regex-last-match-before regexp text lower cursor))
            (if last-match
                (progn
                  (setq cursor (car last-match)
                        remaining (1- remaining)))
              (setq failed t)))
          (if failed
              (progn
                (setq nlre--last-caps old-caps)
                (if noerror
                    (progn (unless (eq noerror t) (goto-char limit)) nil)
                  (signal 'search-failed (list regexp))))
            (setq nlre--last-caps (cdr last-match))
            (nlre--caps-offset off)
            (goto-char (+ off (car last-match)))))))))

(unless (fboundp 'looking-at)
  (fset 'looking-at (symbol-function 'nlre--looking-at)))
(unless (fboundp 're-search-forward)
  (fset 're-search-forward (symbol-function 'nlre--re-search-forward)))
(unless (fboundp 're-search-backward)
  (fset 're-search-backward (symbol-function 'nlre--re-search-backward)))

;; ---- regexp-dependent string helpers (built on nlre-string-match) ----

;; A non-metacharacter one-byte separator is a literal split (Doc 201 §5.2).
(unless (fboundp 'nelisp--split-on-char)
  ;; Hosted users do not load the standalone prelude's identical helper.
  (defun nelisp--split-on-char (string char omit-empty)
    (let ((start 0)
          (idx 0)
          (len (length string))
          (parts nil))
      (while (<= idx len)
        (if (or (= idx len) (= (aref string idx) char))
            (let ((part (substring string start idx)))
              (unless (and omit-empty (= (length part) 0))
                (setq parts (cons part parts)))
              (setq start (1+ idx))))
        (setq idx (1+ idx)))
      (nreverse parts))))

(defconst nlre--split-single-byte-metachars '(?. ?* ?+ ?\? ?\[ ?\] ?^ ?$ ?\\)
  "Emacs-regexp metacharacters that make a would-be one-byte SEPARATOR to
`nlre-split-string' unsafe to treat as a plain literal byte.")

;; PERF: SEPARATORS nil (the overwhelmingly common call shape -- every
;; `(split-string STRING)' with no args) used to fall straight into
;; `nlre-split-string--regexp-path' against "[ \f\t\n\r\v]+", a `:plus' of
;; a `:set'.  `nlre-string-match''s leading-char filter only recognizes a
;; bare `:lit', so that path gets none: every rejected character pays a
;; full `nlre--match-list' `:plus'->`:star' rewrite-and-recurse instead of
;; one cheap comparison.  Measured on this tree (Doc 205 split-string
;; follow-up, 2026-09-28): splitting a real ~124-byte "<hex>  /tmp/...\n"
;; line this way cost 100-450 ms, independent of the string's length and
;; driven entirely by how many non-whitespace characters had to be
;; rejected before the first separator -- the same shape of bug already
;; fixed once for `executable-find' (Doc 201 §6.8) and for a literal
;; one-byte SEPARATOR just above.  A plain `aref' scan for the same fixed,
;; small character set answers in microseconds and needs no regexp engine
;; at all.
(defconst nlre--split-whitespace-chars '(?\s ?\f ?\t ?\n ?\r ?\v)
  "Characters `split-string-default-separators' (\"[ \\f\\t\\n\\r\\v]+\")
matches, used by `nlre--split-on-whitespace' to skip the regexp engine
for `split-string' calls with SEPARATORS nil.")

(defun nlre--split-on-whitespace (string)
  "Fast path for `(split-string STRING)' (SEPARATORS nil): split on runs of
`nlre--split-whitespace-chars', always omitting empty fields and trimming
any leading/trailing run -- exactly the SEPARATORS-nil contract, which
forces OMIT-NULLS to t (see `split-string''s docstring).  Also updates the
match data to the last separator run consumed, as `split-string' via
repeated `string-match' calls would."
  (let ((len (length string)) (i 0) (parts nil) (last-sep nil))
    (while (and (< i len) (memq (aref string i) nlre--split-whitespace-chars))
      (setq i (1+ i)))
    (when (> i 0) (setq last-sep (cons 0 i)))
    (while (< i len)
      (let ((start i))
        (while (and (< i len)
                    (not (memq (aref string i) nlre--split-whitespace-chars)))
          (setq i (1+ i)))
        (setq parts (cons (substring string start i) parts))
        (let ((sep-start i))
          (while (and (< i len)
                      (memq (aref string i) nlre--split-whitespace-chars))
            (setq i (1+ i)))
          (when (> i sep-start) (setq last-sep (cons sep-start i))))))
    (when last-sep (setq nlre--last-caps (vector last-sep)))
    (nreverse parts)))

(defun nlre-split-string (string &optional separators omit-nulls)
  "Like `split-string'.  Default SEPARATORS = whitespace run, which also
implies OMIT-NULLS and leading/trailing trim (matching GNU Emacs)."
  (cond
   ((null separators) (nlre--split-on-whitespace string))
   ((and (= (length separators) 1)
         (not (memq (aref separators 0) nlre--split-single-byte-metachars)))
    (nelisp--split-on-char string (aref separators 0) omit-nulls))
   (t (nlre-split-string--regexp-path string separators omit-nulls))))

(defun nlre-split-string--regexp-path (string separators omit-nulls)
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

;; Expand \N, \&, and \\ in string replacements.
(defun nlre--expand-replacement (rep string)
  (let ((i 0) (n (length rep)) (out ""))
    (while (< i n)
      (let ((c (aref rep i)))
        (if (and (eq c ?\\) (< (1+ i) n))
            (let ((d (aref rep (1+ i))))
              (cond
               ((and (>= d ?0) (<= d ?9))
                (let* ((g (- d ?0))
                       (b (nlre-match-beginning g))
                       (e (nlre-match-end g)))
                  (setq out (concat out (if (and b e) (substring string b e) ""))))
                (setq i (+ i 2)))
               ((eq d ?&)
                (setq out (concat out (substring string (nlre-match-beginning 0)
                                                 (nlre-match-end 0))))
                (setq i (+ i 2)))
               (t (setq out (concat out (char-to-string d)))
                  (setq i (+ i 2)))))
          (setq out (concat out (char-to-string c)))
          (setq i (1+ i)))))
    out))

(defun nlre-replace-regexp-in-string (regexp rep string &optional literal subexp start)
  "`replace-regexp-in-string': REP is a string or a function of the match.
Unless LITERAL, \\N / \\& / \\\\ in a string REP are expanded.  SUBEXP
replaces only that group; START omits the first START characters from the
result, as in Emacs."
  (let ((out "") (pos (or start 0)) (len (length string)) (cont t))
    (while (and cont (<= pos len) (nlre-string-match regexp string pos))
      (let* ((mb (nlre-match-beginning 0)) (me (nlre-match-end 0))
             (rb (if subexp (nlre-match-beginning subexp) mb))
             (re (if subexp (nlre-match-end subexp) me))
             (matched (substring string mb me))
             (piece (cond ((not (stringp rep)) (funcall rep matched))
                          (literal rep)
                          (t (nlre--expand-replacement rep string)))))
        (setq out (concat out (substring string pos rb) piece
                          (substring string re me)))
        (cond
         ((= me mb)
          (if (>= mb len) (setq cont nil)
            (setq out (concat out (substring string mb (1+ mb))))
            (setq pos (1+ mb))))
         (t (setq pos me)))))
    (concat out (substring string (min pos len) len))))

(provide 'nelisp-stdlib-regexp)
;;; nelisp-stdlib-regexp.el ends here
