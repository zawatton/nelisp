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

(defvar nlre--compiled-cache (make-hash-table :test 'equal)
  "Cache from regexp pattern strings to (AST . GROUP-COUNT).")
(defvar nlre--compiled-cache-count 0
  "Number of entries currently tracked in `nlre--compiled-cache'.")
(defvar nlre--compiled-cache-limit 512
  "Maximum compiled regexp patterns kept before clearing the cache.")
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
  (clrhash nlre--compiled-cache)
  (setq nlre--compiled-cache-count 0))

(defun nlre--compiled-pattern (pat)
  "Return cached compiled representation for PAT.
The result is (AST . GROUP-COUNT), where GROUP-COUNT includes match group 0."
  (let ((compiled (gethash pat nlre--compiled-cache)))
    (if compiled
        (progn
          (setq nlre--compiled-cache-hits (1+ nlre--compiled-cache-hits))
          compiled)
      (setq nlre--compiled-cache-misses (1+ nlre--compiled-cache-misses))
      (when (>= nlre--compiled-cache-count nlre--compiled-cache-limit)
        (nlre--compiled-cache-clear))
      (let ((ast (nlre--parse pat)))
        (setq compiled (cons ast (1+ nlre--gcount)))
        (puthash pat compiled nlre--compiled-cache)
        (setq nlre--compiled-cache-count (1+ nlre--compiled-cache-count))
        compiled))))

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
         ((eq d ?\() ;; group start: plain, shy \(?:..\), or numbered \(?N:..\)
          ;; A `?' directly after \( introduces the shy and the explicitly
          ;; numbered forms.  Neither was recognised, so the `?' and the `:'
          ;; were parsed as ordinary pattern characters and \(?:x+\) matched
          ;; the literal text "?:x..." -- i.e. every shy group in the tree
          ;; silently failed to match, including the ones `regexp-opt'
          ;; generates and the ones `string-trim-left' needs.  It failed by
          ;; not matching rather than by erroring, which is why it survived.
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
                ;; consume \)
                (when (and (< (1+ j) n) (eq (aref pat j) ?\\) (eq (aref pat (1+ j)) ?\)))
                  (setq j (+ j 2)))
                ;; A shy group captures nothing, so it simply IS its body.
                ;; `:seq' and `:alt' are both already node types the matcher
                ;; dispatches on, in `nlre--match-list' and `nlre--match-one'
                ;; alike, so this needs no new node type and no new arm.
                (cons (if shy inner (list :group gn inner)) j)))))
         ((eq d ?w) (cons (list :word nil) (+ i 2)))
         ((eq d ?W) (cons (list :word t) (+ i 2)))
         ;; \b / \B: zero-width word boundary.  Absent, so \b fell through
         ;; to the default arm and matched the literal character `b'.
         ((eq d ?b) (cons (list :wordb nil) (+ i 2)))
         ((eq d ?B) (cons (list :wordb t) (+ i 2)))
         ;; \< / \> word edges, \_< / \_> symbol edges.  None were parsed, so
         ;; \< fell through to the default arm and matched a literal `<'.
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

;; ---- matcher (no closures; rest threaded explicitly) ----

(defvar nlre--caps nil "Vector of (start . end) per group during a match.")

;; Emacs folds case by default -- `case-fold-search' is t globally -- and
;; this engine did not honour it at all, so (string-match "A" "a") answered
;; nil where Emacs answers 0.  Every case-insensitive search in every caller
;; silently found nothing, which is the failure mode that does not announce
;; itself.
;;
;; The fold is applied at the two comparison sites rather than baked into
;; the parsed pattern, for two reasons: `nlre--compiled-pattern' caches
;; ASTs keyed on the pattern string alone, so a folded AST would be handed
;; to a later case-sensitive match; and downcasing the pattern text would
;; rewrite \W into \w and \B into \b, turning negated classes into their
;; opposites.
;; `case-fold-search' itself is declared in scripts/nelisp-stdlib-prelude.el,
;; beside the other stock Emacs variables this runtime has to provide -- this
;; file only reads it, and reads it through `boundp' so a match that runs
;; before that declaration behaves as case-sensitive rather than erroring.
(defvar nlre--fold nil "Non-nil while the current match folds case.")

(defun nlre--fold-char (c)
  (if (and nlre--fold (>= c ?A) (<= c ?Z)) (+ c 32) c))

(defun nlre--flip-case (c)
  (cond ((and (>= c ?a) (<= c ?z)) (- c 32))
        ((and (>= c ?A) (<= c ?Z)) (+ c 32))
        (t c)))

(defun nlre--space-p (c) (or (= c 32) (= c 9) (= c 10) (= c 13) (= c 12)))
;; `_' is a SYMBOL constituent, not a word constituent, in the standard
;; syntax table -- (string-match "\\w" "_") is nil in Emacs.  Counting it as a
;; word character also moved every \\b boundary: (string-match "\\bx" "_x")
;; answered nil where Emacs answers 1.
(defun nlre--word-p (c)
  (or (and (>= c ?a) (<= c ?z)) (and (>= c ?A) (<= c ?Z))
      (and (>= c ?0) (<= c ?9))))

(defun nlre--symbol-p (c)
  (or (nlre--word-p c) (= c ?_)))

;; `\\sC' matches a character whose SYNTAX CLASS is C.  The class character was
;; parsed and thrown away, so every \\sC behaved as \\s- (whitespace) and every
;; \\SC as its negation: (string-match "\\Sw" "a") answered 0 where Emacs
;; answers nil.  A class this does not model matches nothing, which is wrong
;; in a stated direction rather than in an arbitrary one.
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

(defun nlre--match-atom1 (node s pos n)
  "Match a single non-quantified atom NODE at POS; return end-pos or nil.
Does NOT continue to any rest (used for one repetition)."
  (let ((tag (car node)))
    (cond
     ((eq tag :lit) (and (< pos n)
                         (eq (nlre--fold-char (aref s pos))
                             (nlre--fold-char (nth 1 node)))
                         (1+ pos)))
     ((eq tag :any) (and (< pos n) (not (eq (aref s pos) ?\n)) (1+ pos)))
     ((eq tag :set) (and (< pos n) (nlre--set-match (nth 1 node) (nth 2 node) (aref s pos)) (1+ pos)))
     ((eq tag :word) (and (< pos n) (let ((w (nlre--word-p (aref s pos)))) (if (nth 1 node) (not w) w)) (1+ pos)))
     ((eq tag :space) (and (< pos n) (let ((w (nlre--space-p (aref s pos)))) (if (nth 1 node) (not w) w)) (1+ pos)))
     ((eq tag :syntax)
      (and (< pos n)
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

;; Doc 201 §5.4.  `nlre-string-match' retries at every start position, and
;; each retry used to cost a fresh `make-vector' plus a walk into
;; `nlre--match-list''s dispatch chain -- even where the pattern's very
;; first node is a literal that the character at that position plainly is
;; not.  Two changes, both confined to the scan loop:
;;
;;   1. the capture vector is allocated ONCE per call and cleared per
;;      attempt, instead of once per attempt;
;;   2. when the pattern must begin with one specific character, a position
;;      whose character is not that one is skipped with a single `aref' and
;;      `eq' rather than an attempt.
;;
;; The filter fires ONLY when the first node must match exactly one known
;; character.  Anything optional (`:star'/`:opt' and the lazy forms),
;; zero-width (`:bol', `:wordb', ...), structural (`:alt'/`:group'/`:seq')
;; or multi-character (`:set'/`:any'/`:word'/`:space') answers nil and the
;; loop runs exactly as it did: guessing wrong here would skip a real
;; match, so the question is only asked where the answer is certain.
;;
;; Measured on the shape `skk-version.el' pays -- 42 `string-match' calls
;; over ~43-character strings -- on this repo's windows-x86_64 standalone,
;; 2026-08-30, three runs each side, interleaved in one stretch on an idle
;; machine:
;;
;;   never-matching `ddskk-[0-9]+\.[0-9]+'  2.68-2.88s -> 0.47-0.52s  (5.5x)
;;   matching       `package-[0-9]+/lisp'   1.91-2.00s -> 1.40-1.62s  (1.3x)
;;   lead-less      `[0-9]+/lisp'           4.67-4.73s -> 4.84-5.42s  (0.95x)
;;
;; The last row is a real, small COST, not noise: a control build carrying
;; the two new defuns below but never calling them measured 4.59-4.75s,
;; i.e. code layout does not explain it, and splitting the scan into two
;; loops (so the lead-less path executes no filter test at all) did not
;; remove it either.  Counting interpreter calls says this path should have
;; got marginally CHEAPER -- one `>' where there used to be a `make-vector'
;; -- so the remaining explanation is allocation/collection behaviour
;; rather than work done, and it is left measured but unexplained.  ~5% on
;; patterns with no leading literal buys 5.5x on those that have one, which
;; is nearly all of them.
(defun nlre--leading-lit-char (nodes)
  "Return the one character every match of NODES must start with, or nil."
  (and (consp nodes)
       (let ((nd (car nodes)))
         (and (consp nd) (eq (car nd) :lit) (nth 1 nd)))))

(defun nlre--caps-clear (v)
  "Set every slot of vector V to nil.
`fillarray' is not available on the standalone reader prelude."
  (let ((k (length v)))
    (while (> k 0)
      (setq k (1- k))
      (aset v k nil))))

(defun nlre-string-match (regexp string &optional start)
  "Pure-elisp `string-match'.  Return match start index, or nil.
Sets `nlre--match-data' (and host match-data when available via set-match-data)."
  (setq nlre--string-match-calls (1+ nlre--string-match-calls))
  (when (and nlre--string-match-counter-file
             (= (mod nlre--string-match-calls nlre--string-match-counter-interval) 0)
             (fboundp 'nl-write-file))
    (nl-write-file nlre--string-match-counter-file
                   (format "%d" nlre--string-match-calls)))
  (let* ((nlre--fold (and (boundp 'case-fold-search) case-fold-search))
         (compiled (nlre--compiled-pattern regexp))
         (ast (car compiled))
         (top (nlre--seq-nodes ast))
         (n (length string))
         (i (or start 0))
         (ng (cdr compiled))
         (lead (nlre--leading-lit-char top))
         ;; Fold the required character the same way the matcher folds the
         ;; one it is compared against, so `case-fold-search' does not make
         ;; the filter reject a position the matcher would have accepted.
         (lead (and lead (nlre--fold-char lead)))
         ;; One scratch vector for the whole scan.  `:savestart'/`:saveend'
         ;; put back whatever they overwrote when their continuation fails,
         ;; so the only state a failed attempt can leave behind is a group
         ;; that matched inside it; clearing covers that.  Patterns with no
         ;; group (ng = 1) have nothing to clear -- slot 0 is written on
         ;; success and read nowhere else.
         (caps (make-vector ng nil))
         (hit nil))
    (setq nlre--caps caps)
    (when lead
      (setq nlre--leading-filter-calls (1+ nlre--leading-filter-calls)))
    ;; Two loops rather than one with the filter test inside it.  The
    ;; filter exists to make a rejected position cost almost nothing, and a
    ;; `lead' test in a shared loop hands that cost straight back to every
    ;; pattern that has no leading literal: measured at 4-7% on a
    ;; `[0-9]+/lisp' sweep, with a control build (the two new defuns
    ;; present but never called) ruling out code layout as the cause.
    (if lead
        ;; `(< i n)': a `:lit' cannot match where there is no character, so
        ;; the i = n attempt the other loop still makes is dead here.
        (while (and (not hit) (< i n))
          (if (not (eq (nlre--fold-char (aref string i)) lead))
              (setq i (1+ i))
            (when (> ng 1) (nlre--caps-clear caps))
            (let ((e (nlre--match-list top string i n)))
              (if e
                  (progn (aset caps 0 (cons i e)) (setq hit i))
                (setq i (1+ i))))))
      (while (and (not hit) (<= i n))
        (when (> ng 1) (nlre--caps-clear caps))
        (let ((e (nlre--match-list top string i n)))
          (if e
              (progn (aset caps 0 (cons i e)) (setq hit i))
            (setq i (1+ i))))))
    (when hit
      ;; `caps' is this call's own vector -- the reuse above is within one
      ;; scan, never across calls -- so handing it straight to
      ;; `nlre--last-caps' is what the per-attempt `make-vector' did too.
      (setq nlre--last-caps caps)
      hit)))

(defvar nlre--last-caps nil "Capture vector of the last successful match.")

(defun nlre-match-beginning (n)
  (and nlre--last-caps (< n (length nlre--last-caps))
       (let ((c (aref nlre--last-caps n))) (and c (car c)))))
(defun nlre-match-end (n)
  (and nlre--last-caps (< n (length nlre--last-caps))
       (let ((c (aref nlre--last-caps n))) (and c (cdr c)))))

;; ---- regexp-dependent string helpers (built on nlre-string-match) ----

;; PERF (cold-start hand-off follow-up, 2026-08-30): a SEPARATORS of
;; exactly one byte, none of them an Emacs-regexp metacharacter, can never
;; behave differently split literally vs. through the regexp engine below
;; -- there is nothing for `nlre-string-match' to buy over a plain
;; byte-compare. Measured directly: splitting a real ~2.3KB, 59-entry
;; Windows PATH on ";" cost 3.2-3.4s through the regexp engine vs 0.4s via
;; `nelisp--split-on-char' (scripts/nelisp-stdlib-prelude.el) -- an 8x
;; difference for identical output. `executable-find' was fixed to call
;; `nelisp--split-on-char' directly (dev/nelisp commit 70cd5852); this
;; extends the same fast path to every OTHER caller of `split-string'/
;; `nlre-split-string' with a single-byte literal separator, since a
;; caller other than `executable-find' hitting this same cost was flagged
;; as a known follow-up in that commit and in docs/design/201 §5.2.
(unless (fboundp 'nelisp--split-on-char)
  ;; The standalone prelude normally supplies this helper.  Hosted users of
  ;; this library do not load that prelude, so keep an identical fallback
  ;; here rather than sending their literal separators through the regexp
  ;; engine.
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

(defun nlre-split-string (string &optional separators omit-nulls)
  "Like `split-string'.  Default SEPARATORS = whitespace run, which also
implies OMIT-NULLS and leading/trailing trim (matching GNU Emacs)."
  (if (and separators (= (length separators) 1)
           (not (memq (aref separators 0) nlre--split-single-byte-metachars)))
      (nelisp--split-on-char string (aref separators 0) omit-nulls)
    (nlre-split-string--regexp-path string separators omit-nulls)))

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

;; \N, \& and \\ in a string REPLACEMENT were not expanded -- they were
;; copied through as the two literal characters -- so
;; (replace-regexp-in-string "\\(a\\)" "[\\1]" "a") produced "[\\1]".
;; A backreference is the usual reason to write a group in the first place,
;; so the common call was the broken one.
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
