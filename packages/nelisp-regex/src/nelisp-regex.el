;;; nelisp-regex.el --- subset POSIX/Emacs regex engine on NeLisp  -*- lexical-binding: t; -*-

;; Phase 9a/9b prerequisite for `looking-at' / `search-forward' (regex mode).
;; Layer: extension package.
;; Foundation: pure Elisp NFA simulator; no Emacs C regex (`string-match' / `re-search-forward')
;; is consulted at run time.  The engine reads its own input string char-by-char
;; and walks a Thompson-construction NFA, so it can be wired directly into
;; NeLisp text buffers without depending on host Emacs regex semantics.
;;
;; MVP scope (syntax A only, see header table):
;;   literal, ., ^, $, [..], [^..], ranges, *, +, ?, \\(...\\), \\|,
;;   escapes (\\\\ \\. \\( \\) \\| \\+ \\* \\? \\^ \\$ \\[ \\]).
;; Best-effort B (\\b \\B \\w \\W) — covered by ASCII rule only.
;; Deferred to Phase 9c: backreferences \\1..\\9, bounded \\{n,m\\},
;;   unicode property classes [:alpha:] etc, multibyte-coding aware match.
;;
;; Public API (5 entries):
;;   (nelisp-rx-compile REGEX)              => opaque pattern
;;   (nelisp-rx-string-match  PAT STR &opt START)  => match-data plist or nil
;;   (nelisp-rx-string-match-all PAT STR &opt START) => list of match-data
;;   (nelisp-rx-replace      PAT STR REPL)  => string (first match)
;;   (nelisp-rx-replace-all  PAT STR REPL)  => string (all non-overlapping)
;;
;; Internal helpers use the `nelisp-rx--' prefix.  The module is intentionally
;; standalone (require 'cl-lib only) so it can be byte-compiled in the same
;; pass as the rest of `src/nelisp-*.el'.

;;; Code:

(require 'cl-lib)

;;; --------------------------------------------------------------------------
;;; AST
;;; --------------------------------------------------------------------------
;;
;; Parse tree nodes are tagged lists for cheap allocation:
;;
;;   (:lit  CHAR)              literal character
;;   (:any)                    matches any char except newline -- but our MVP
;;                              does NOT exclude newline (Emacs default).
;;   (:bol)                    ^ anchor
;;   (:eol)                    $ anchor
;;   (:wb)                     \b word boundary
;;   (:nwb)                    \B not a word boundary
;;   (:class POS-P RANGES)     [..] / [^..]; POS-P=t for positive class
;;                              RANGES = list of (lo . hi) inclusive ranges
;;                              and `:word' / `:nword' tags (\w / \W shorthand)
;;   (:concat NODES...)        concatenation
;;   (:alt   N1 N2)            N1 \| N2
;;   (:star  N)                N*
;;   (:plus  N)                N+
;;   (:opt   N)                N?
;;   (:group IDX N)            \(..\); IDX is 1-based capture index

(defun nelisp-rx--make-class (positive ranges)
  "Build a :class node with POSITIVE flag and RANGES list."
  (list :class positive ranges))

;;; --------------------------------------------------------------------------
;;; Parser
;;; --------------------------------------------------------------------------

(defvar nelisp-rx--parse-input nil
  "Input string currently being parsed (dynamic).")
(defvar nelisp-rx--parse-pos nil
  "Index into `nelisp-rx--parse-input' (dynamic).")
(defvar nelisp-rx--parse-group-counter nil
  "Next capture-group index, starting at 1 (dynamic).")

(defsubst nelisp-rx--peek ()
  "Return char at current position, or nil at end."
  (and (< nelisp-rx--parse-pos (length nelisp-rx--parse-input))
       (aref nelisp-rx--parse-input nelisp-rx--parse-pos)))

(defsubst nelisp-rx--peek2 ()
  "Return char at current-position + 1, or nil at end."
  (and (< (1+ nelisp-rx--parse-pos) (length nelisp-rx--parse-input))
       (aref nelisp-rx--parse-input (1+ nelisp-rx--parse-pos))))

(defsubst nelisp-rx--advance ()
  "Consume one char and return it."
  (let ((c (aref nelisp-rx--parse-input nelisp-rx--parse-pos)))
    (setq nelisp-rx--parse-pos (1+ nelisp-rx--parse-pos))
    c))

(defun nelisp-rx--parse (regex)
  "Parse REGEX string into AST.
Signal `nelisp-rx-syntax-error' on malformed input."
  (let ((nelisp-rx--parse-input regex)
        (nelisp-rx--parse-pos 0)
        (nelisp-rx--parse-group-counter 1))
    (let ((ast (nelisp-rx--parse-alt)))
      (when (< nelisp-rx--parse-pos (length regex))
        ;; Trailing chars (typically a stray `\)') -- error.
        (signal 'nelisp-rx-syntax-error
                (list (format "unexpected char at pos %d in %S"
                              nelisp-rx--parse-pos regex))))
      ast)))

(defun nelisp-rx--parse-alt ()
  "Parse an alternation (top level / inside group)."
  (let ((branches (list (nelisp-rx--parse-concat))))
    (while (and (eq (nelisp-rx--peek) ?\\)
                (eq (nelisp-rx--peek2) ?|))
      (nelisp-rx--advance) (nelisp-rx--advance) ; consume \|
      (push (nelisp-rx--parse-concat) branches))
    (if (= (length branches) 1)
        (car branches)
      ;; Build right-leaning alt tree to keep simulator simple.
      (let ((rev (nreverse branches)))
        (cl-reduce (lambda (a b) (list :alt a b)) rev)))))

(defun nelisp-rx--parse-concat ()
  "Parse a concatenation of atoms+quantifiers."
  (let (nodes)
    (cl-loop
     (let ((c (nelisp-rx--peek)))
       (cond
        ((null c) (cl-return))
        ;; End-of-alternation / end-of-group sentinels.
        ((and (eq c ?\\) (memq (nelisp-rx--peek2) '(?| ?\))))
         (cl-return))
        (t
         (let ((atom (nelisp-rx--parse-atom)))
           (push (nelisp-rx--parse-quant atom) nodes))))))
    (let ((rev (nreverse nodes)))
      (cond ((null rev) (list :concat))
            ((null (cdr rev)) (car rev))
            (t (cons :concat rev))))))

(defun nelisp-rx--parse-quant (atom)
  "Wrap ATOM in a quantifier if next char is one."
  (let ((c (nelisp-rx--peek)))
    (cond
     ((eq c ?*) (nelisp-rx--advance) (list :star atom))
     ((eq c ?+) (nelisp-rx--advance) (list :plus atom))
     ((eq c ??) (nelisp-rx--advance) (list :opt  atom))
     (t atom))))

(defun nelisp-rx--parse-atom ()
  "Parse a single atom (literal / class / group / anchor / escape)."
  (let ((c (nelisp-rx--peek)))
    (cond
     ((null c) (signal 'nelisp-rx-syntax-error '("unexpected EOS in atom")))
     ((eq c ?.) (nelisp-rx--advance) (list :any))
     ((eq c ?^) (nelisp-rx--advance) (list :bol))
     ((eq c ?$) (nelisp-rx--advance) (list :eol))
     ((eq c ?\[) (nelisp-rx--parse-class))
     ((eq c ?\\) (nelisp-rx--parse-backslash))
     ;; Bare ?* / ?+ / ?? / ?] etc are syntax errors here -- the quant parser
     ;; only fires *after* an atom; if we encountered them as the first thing
     ;; in an atom slot, that means the regex started with a bogus quantifier.
     ((memq c '(?* ?+ ??))
      (signal 'nelisp-rx-syntax-error
              (list (format "quantifier without operand at pos %d"
                            nelisp-rx--parse-pos))))
     (t (nelisp-rx--advance) (list :lit c)))))

(defun nelisp-rx--parse-backslash ()
  "Parse the construct introduced by `\\\\' (escape, group, anchor, alt)."
  (nelisp-rx--advance)                     ; consume the backslash itself
  (let ((c (nelisp-rx--peek)))
    (cond
     ((null c) (signal 'nelisp-rx-syntax-error '("trailing backslash")))
     ((eq c ?\() (nelisp-rx--advance) (nelisp-rx--parse-group))
     ((eq c ?\)) (signal 'nelisp-rx-syntax-error
                         (list "unmatched \\)")))
     ((eq c ?|) (signal 'nelisp-rx-syntax-error
                        (list "stray \\| outside alternation")))
     ((eq c ?b) (nelisp-rx--advance) (list :wb))
     ((eq c ?B) (nelisp-rx--advance) (list :nwb))
     ((eq c ?w) (nelisp-rx--advance)
      (nelisp-rx--make-class t (list :word)))
     ((eq c ?W) (nelisp-rx--advance)
      (nelisp-rx--make-class t (list :nword)))
     ;; `\\1' .. `\\9' -- backref deferred (Phase 9c).  Reject early so users
     ;; do not silently fall through.
     ((and (>= c ?1) (<= c ?9))
      (signal 'nelisp-rx-syntax-error
              (list (format "backreference \\%c not supported in MVP"
                            c))))
     ;; Any other char -> literal.  Covers the punctuation/escape table:
     ;;   \\\\ \\. \\* \\+ \\? \\^ \\$ \\[ \\] \\{ \\} etc.
     (t (nelisp-rx--advance) (list :lit c)))))

(defun nelisp-rx--parse-group ()
  "Parse a `\\(...\\)' group; the leading `\\(' is already consumed."
  (let ((idx nelisp-rx--parse-group-counter))
    (setq nelisp-rx--parse-group-counter (1+ nelisp-rx--parse-group-counter))
    (let ((inner (nelisp-rx--parse-alt)))
      (unless (and (eq (nelisp-rx--peek) ?\\)
                   (eq (nelisp-rx--peek2) ?\)))
        (signal 'nelisp-rx-syntax-error '("missing \\) for group")))
      (nelisp-rx--advance) (nelisp-rx--advance) ; consume \)
      (list :group idx inner))))

(defun nelisp-rx--parse-class ()
  "Parse a `[...]' character class; leading `[' is the lookahead char."
  (nelisp-rx--advance)                     ; consume [
  (let ((positive t)
        (ranges nil))
    (when (eq (nelisp-rx--peek) ?^)
      (setq positive nil)
      (nelisp-rx--advance))
    ;; A literal `]' as the very first char is taken as itself.
    (when (eq (nelisp-rx--peek) ?\])
      (push (cons ?\] ?\]) ranges)
      (nelisp-rx--advance))
    (cl-loop
     (let ((c (nelisp-rx--peek)))
       (cond
        ((null c)
         (signal 'nelisp-rx-syntax-error '("unterminated character class")))
        ((eq c ?\]) (nelisp-rx--advance) (cl-return))
        (t
         (let ((lo (nelisp-rx--class-char)))
           (if (and (eq (nelisp-rx--peek) ?-)
                    (not (eq (nelisp-rx--peek2) ?\])))
               (progn
                 (nelisp-rx--advance)            ; consume `-'
                 (let ((hi (nelisp-rx--class-char)))
                   (when (> lo hi)
                     (signal 'nelisp-rx-syntax-error
                             (list (format "inverted range %c-%c" lo hi))))
                   (push (cons lo hi) ranges)))
             (push (cons lo lo) ranges)))))))
    (nelisp-rx--make-class positive (nreverse ranges))))

(defun nelisp-rx--class-char ()
  "Read one character (or short-class escape) inside `[...]'.
Returns an integer for normal chars; for `\\w'/`\\W' the simulator picks them
up via the keyword forms but inside a character class we treat `\\w' as `w'
literally to keep MVP semantics simple -- this matches GNU Emacs's behaviour."
  (let ((c (nelisp-rx--peek)))
    (cond
     ((null c)
      (signal 'nelisp-rx-syntax-error '("unterminated class")))
     ((eq c ?\\)
      (nelisp-rx--advance)
      (let ((n (nelisp-rx--peek)))
        (unless n
          (signal 'nelisp-rx-syntax-error '("trailing backslash in class")))
        (nelisp-rx--advance)
        n))
     (t (nelisp-rx--advance) c))))

(define-error 'nelisp-rx-syntax-error "Invalid regex syntax")

;;; --------------------------------------------------------------------------
;;; NFA construction (Thompson)
;;; --------------------------------------------------------------------------
;;
;; Each state is a vector [LABEL TRANS1 TRANS2 EXTRA].
;;   LABEL  -- :char CHAR | :any | :class POS RANGES | :bol | :eol | :wb | :nwb
;;            | :gstart IDX | :gend IDX | :split | :match
;;   TRANS1 -- next state index (or nil for :match)
;;   TRANS2 -- alternate next state for :split, else nil
;;   EXTRA  -- payload (CHAR / class data / IDX), redundant with LABEL but
;;             kept for symmetry / cheap access.
;;
;; The NFA is represented as a vector of state vectors, with `start' pinned
;; at index 0 (we shuffle if needed).  We avoid linked lists to keep
;; recursion shallow.

(defun nelisp-rx--state-vec (label trans1 trans2 extra)
  "Allocate a state vector."
  (vector label trans1 trans2 extra))

(cl-defstruct (nelisp-rx--frag (:constructor nelisp-rx--mkfrag)
                               (:copier nil))
  "Sub-NFA fragment used during Thompson construction.
START is the entry state index; OUTS is a list of (STATE-IDX . SLOT) pairs
where SLOT is 1 or 2 indicating which transition slot is dangling."
  start outs)

(defvar nelisp-rx--build-states nil
  "Mutable vector of state vectors during NFA build (dynamic).")

(defsubst nelisp-rx--add-state (label trans1 trans2 extra)
  "Append a new state and return its index."
  (let ((idx (length nelisp-rx--build-states)))
    (setq nelisp-rx--build-states
          (vconcat nelisp-rx--build-states
                   (vector (nelisp-rx--state-vec label trans1 trans2 extra))))
    idx))

(defsubst nelisp-rx--patch (outs target)
  "Patch every dangling OUT in OUTS to point to TARGET state index."
  (dolist (o outs)
    (let ((s (aref nelisp-rx--build-states (car o))))
      (aset s (cdr o) target))))

(defun nelisp-rx--build (ast)
  "Build a fragment for AST."
  (pcase (car ast)
    (:lit
     (let ((s (nelisp-rx--add-state :char nil nil (cadr ast))))
       (nelisp-rx--mkfrag :start s :outs (list (cons s 1)))))
    (:any
     (let ((s (nelisp-rx--add-state :any nil nil nil)))
       (nelisp-rx--mkfrag :start s :outs (list (cons s 1)))))
    (:bol
     (let ((s (nelisp-rx--add-state :bol nil nil nil)))
       (nelisp-rx--mkfrag :start s :outs (list (cons s 1)))))
    (:eol
     (let ((s (nelisp-rx--add-state :eol nil nil nil)))
       (nelisp-rx--mkfrag :start s :outs (list (cons s 1)))))
    (:wb
     (let ((s (nelisp-rx--add-state :wb nil nil nil)))
       (nelisp-rx--mkfrag :start s :outs (list (cons s 1)))))
    (:nwb
     (let ((s (nelisp-rx--add-state :nwb nil nil nil)))
       (nelisp-rx--mkfrag :start s :outs (list (cons s 1)))))
    (:class
     (let* ((positive (nth 1 ast))
            (ranges   (nth 2 ast))
            (s (nelisp-rx--add-state :class nil nil
                                     (cons positive ranges))))
       (nelisp-rx--mkfrag :start s :outs (list (cons s 1)))))
    (:concat
     (let ((children (cdr ast)))
       (cond
        ((null children)
         ;; Empty concat — emit a no-op split with both branches identical.
         (let ((s (nelisp-rx--add-state :split nil nil nil)))
           (nelisp-rx--mkfrag :start s
                              :outs (list (cons s 1) (cons s 2)))))
        (t
         (let ((first (nelisp-rx--build (car children))))
           (dolist (rest (cdr children))
             (let ((next (nelisp-rx--build rest)))
               (nelisp-rx--patch (nelisp-rx--frag-outs first)
                                 (nelisp-rx--frag-start next))
               (setf (nelisp-rx--frag-outs first)
                     (nelisp-rx--frag-outs next))))
           first)))))
    (:alt
     (let* ((a (nelisp-rx--build (nth 1 ast)))
            (b (nelisp-rx--build (nth 2 ast)))
            (s (nelisp-rx--add-state :split
                                     (nelisp-rx--frag-start a)
                                     (nelisp-rx--frag-start b)
                                     nil)))
       (nelisp-rx--mkfrag :start s
                          :outs (append (nelisp-rx--frag-outs a)
                                        (nelisp-rx--frag-outs b)))))
    (:star
     (let* ((inner (nelisp-rx--build (nth 1 ast)))
            ;; greedy: try inner first (slot 1), else exit (slot 2).
            (s (nelisp-rx--add-state :split
                                     (nelisp-rx--frag-start inner)
                                     nil nil)))
       (nelisp-rx--patch (nelisp-rx--frag-outs inner) s)
       (nelisp-rx--mkfrag :start s :outs (list (cons s 2)))))
    (:plus
     (let* ((inner (nelisp-rx--build (nth 1 ast)))
            ;; one inner, then optional repeat via split.
            (s (nelisp-rx--add-state :split
                                     (nelisp-rx--frag-start inner)
                                     nil nil)))
       (nelisp-rx--patch (nelisp-rx--frag-outs inner) s)
       (nelisp-rx--mkfrag :start (nelisp-rx--frag-start inner)
                          :outs (list (cons s 2)))))
    (:opt
     (let* ((inner (nelisp-rx--build (nth 1 ast)))
            (s (nelisp-rx--add-state :split
                                     (nelisp-rx--frag-start inner)
                                     nil nil)))
       (nelisp-rx--mkfrag :start s
                          :outs (cons (cons s 2)
                                      (nelisp-rx--frag-outs inner)))))
    (:group
     (let* ((idx (nth 1 ast))
            (inner (nelisp-rx--build (nth 2 ast)))
            (sstart (nelisp-rx--add-state :gstart nil nil idx))
            (send   (nelisp-rx--add-state :gend nil nil idx)))
       (aset (aref nelisp-rx--build-states sstart) 1
             (nelisp-rx--frag-start inner))
       (nelisp-rx--patch (nelisp-rx--frag-outs inner) send)
       (nelisp-rx--mkfrag :start sstart :outs (list (cons send 1)))))
    (_ (error "nelisp-rx: unknown AST node %S" ast))))

(defun nelisp-rx--build-nfa (ast)
  "Compile AST to an NFA struct.
Returns plist: (:states VEC :start INT :match INT :groups INT)."
  (let ((nelisp-rx--build-states (vector)))
    (let* ((frag (nelisp-rx--build ast))
           (mtch (nelisp-rx--add-state :match nil nil nil)))
      (nelisp-rx--patch (nelisp-rx--frag-outs frag) mtch)
      (list :states nelisp-rx--build-states
            :start  (nelisp-rx--frag-start frag)
            :match  mtch
            :groups (1- (nelisp-rx--collect-max-group ast))))))

(defun nelisp-rx--collect-max-group (ast)
  "Return one-past-largest group index in AST (>=1)."
  (pcase (car ast)
    (:group (max (1+ (nth 1 ast))
                 (nelisp-rx--collect-max-group (nth 2 ast))))
    (:concat (apply #'max 1 (mapcar #'nelisp-rx--collect-max-group (cdr ast))))
    (:alt   (max (nelisp-rx--collect-max-group (nth 1 ast))
                 (nelisp-rx--collect-max-group (nth 2 ast))))
    ((or :star :plus :opt)
     (nelisp-rx--collect-max-group (nth 1 ast)))
    (_ 1)))

;;; --------------------------------------------------------------------------
;;; Compiled-pattern object
;;; --------------------------------------------------------------------------

(cl-defstruct (nelisp-rx-pattern (:constructor nelisp-rx--make-pattern)
                                 (:copier nil))
  "Compiled regex pattern."
  source         ; original regex string
  states         ; vector of state vectors
  start          ; integer start state index
  match          ; integer accept state index
  ngroups)       ; number of capture groups (>=0)

;;;###autoload
(defun nelisp-rx-compile (regex)
  "Compile REGEX (an Emacs-style regex string subset) to a pattern object.
Signal `nelisp-rx-syntax-error' on malformed input."
  (let* ((ast (nelisp-rx--parse regex))
         (nfa (nelisp-rx--build-nfa ast)))
    (nelisp-rx--make-pattern
     :source regex
     :states (plist-get nfa :states)
     :start  (plist-get nfa :start)
     :match  (plist-get nfa :match)
     :ngroups (max 0 (plist-get nfa :groups)))))

;;; --------------------------------------------------------------------------
;;; NFA simulation
;;; --------------------------------------------------------------------------
;;
;; We use a backtracking depth-first walk with capture-group bookkeeping.
;; That's strictly weaker than the pure subset-construction Thompson sim
;; (it can hit exponential blowup on adversarial input), but it gives us
;; correct group captures with minimal code.  For the regex shapes that
;; `looking-at' / `search-forward' will throw at us in Phase 9b (mostly
;; literal + `\\(...\\)' wrappers + simple `*'/`+'), the constants are
;; small enough that this is fine.

(defun nelisp-rx--word-char-p (c)
  "Return non-nil if char C is a word constituent (ASCII rule for MVP)."
  (and c
       (or (and (>= c ?a) (<= c ?z))
           (and (>= c ?A) (<= c ?Z))
           (and (>= c ?0) (<= c ?9))
           (eq c ?_))))

(defun nelisp-rx--class-match-p (positive ranges char)
  "Return non-nil if CHAR matches a [..]/[^..] class."
  (let ((hit nil))
    (dolist (r ranges)
      (cond
       ((eq r :word)  (when (nelisp-rx--word-char-p char) (setq hit t)))
       ((eq r :nword) (when (and char (not (nelisp-rx--word-char-p char)))
                        (setq hit t)))
       ((and (consp r)
             char
             (>= char (car r))
             (<= char (cdr r)))
        (setq hit t))))
    (if positive hit (not hit))))

(defun nelisp-rx--match-from (pat str start)
  "Try to match PAT against STR starting exactly at START.
Return (END . GROUPS) where GROUPS is a vector of (S . E) cells (1-based,
slot 0 unused) on success, or nil on failure.  Greedy / leftmost-first
backtracking semantics."
  (let* ((states (nelisp-rx-pattern-states pat))
         (ngrp   (nelisp-rx-pattern-ngroups pat))
         (groups (make-vector (1+ ngrp) nil))
         (slen   (length str))
         (best   nil))
    (cl-labels
        ((char-at (pos) (and (< pos slen) (aref str pos)))
         (prev-char (pos) (and (> pos 0) (aref str (1- pos))))
         (walk
          (sidx pos)
          (let* ((s (aref states sidx))
                 (label (aref s 0)))
            (pcase label
              (:match
               (setq best (cons pos (vconcat groups))) ; freeze copy
               t)
              (:char
               (let ((c (char-at pos)))
                 (and c (eq c (aref s 3))
                      (walk (aref s 1) (1+ pos)))))
              (:any
               (let ((c (char-at pos)))
                 (and c (walk (aref s 1) (1+ pos)))))
              (:class
               (let* ((c (char-at pos))
                      (data (aref s 3)))
                 (and c
                      (nelisp-rx--class-match-p (car data) (cdr data) c)
                      (walk (aref s 1) (1+ pos)))))
              (:bol
               (and (or (= pos 0)
                        (eq (prev-char pos) ?\n))
                    (walk (aref s 1) pos)))
              (:eol
               (and (or (= pos slen)
                        (eq (char-at pos) ?\n))
                    (walk (aref s 1) pos)))
              (:wb
               (let* ((before (and (> pos 0)
                                   (nelisp-rx--word-char-p (prev-char pos))))
                      (after  (and (< pos slen)
                                   (nelisp-rx--word-char-p (char-at pos)))))
                 (and (not (eq before after))
                      (walk (aref s 1) pos))))
              (:nwb
               (let* ((before (and (> pos 0)
                                   (nelisp-rx--word-char-p (prev-char pos))))
                      (after  (and (< pos slen)
                                   (nelisp-rx--word-char-p (char-at pos)))))
                 (and (eq before after)
                      (walk (aref s 1) pos))))
              (:split
               (or (walk (aref s 1) pos)
                   (and (aref s 2) (walk (aref s 2) pos))))
              (:gstart
               (let* ((idx (aref s 3))
                      (saved (aref groups idx)))
                 (aset groups idx (cons pos nil))
                 (or (walk (aref s 1) pos)
                     (progn (aset groups idx saved) nil))))
              (:gend
               (let* ((idx   (aref s 3))
                      (saved (aref groups idx))
                      (open  (and saved (car saved))))
                 (aset groups idx (cons open pos))
                 (or (walk (aref s 1) pos)
                     (progn (aset groups idx saved) nil))))
              (_ (error "nelisp-rx: unknown state label %S" label))))))
      (walk (nelisp-rx-pattern-start pat) start)
      best)))

(defun nelisp-rx--scan (pat str start)
  "Scan STR from START forward; return (ANCHOR END GROUPS) on first match, or nil.
ANCHOR is the start position where the match was found."
  (let ((slen (length str))
        (i start)
        (hit  nil))
    (catch 'done
      (while (<= i slen)
        (let ((m (nelisp-rx--match-from pat str i)))
          (when m
            (setq hit (list i (car m) (cdr m)))
            (throw 'done nil)))
        (setq i (1+ i)))
      nil)
    hit))

;;; --------------------------------------------------------------------------
;;; Public match API
;;; --------------------------------------------------------------------------

(defun nelisp-rx--make-match-data (anchor end groups)
  "Build the public match-data plist returned to callers."
  (let ((lst nil))
    (when groups
      ;; groups is a vector indexed 1..ngrp; slot 0 is unused.
      (cl-loop
       for i from 1 below (length groups)
       do (let ((cell (aref groups i)))
            (push (if (and cell (cdr cell))
                      (list :index i :start (car cell) :end (cdr cell))
                    (list :index i :start nil :end nil))
                  lst))))
    (list :start anchor :end end :groups (nreverse lst))))

;;;###autoload
(defun nelisp-rx-string-match (pattern string &optional start)
  "Search STRING for PATTERN starting at START (default 0).
Return a match-data plist with keys :start :end :groups, or nil if
no match.  PATTERN can be a string (compiled on the fly) or a
pre-compiled `nelisp-rx-pattern' object."
  (let* ((pat (if (nelisp-rx-pattern-p pattern)
                  pattern
                (nelisp-rx-compile pattern)))
         (s   (or start 0))
         (hit (nelisp-rx--scan pat string s)))
    (and hit
         (nelisp-rx--make-match-data (nth 0 hit) (nth 1 hit) (nth 2 hit)))))

;;;###autoload
(defun nelisp-rx-string-match-all (pattern string &optional start)
  "Find every non-overlapping match of PATTERN in STRING from START.
Return a list of match-data plists in left-to-right order."
  (let* ((pat (if (nelisp-rx-pattern-p pattern)
                  pattern
                (nelisp-rx-compile pattern)))
         (s   (or start 0))
         (acc nil))
    (while
        (let ((m (nelisp-rx--scan pat string s)))
          (when m
            (push (nelisp-rx--make-match-data (nth 0 m) (nth 1 m) (nth 2 m))
                  acc)
            ;; Advance: if the match consumed nothing, step by 1 to avoid
            ;; infinite loop on patterns like "a*".
            (let ((next (if (= (nth 0 m) (nth 1 m))
                            (1+ (nth 1 m))
                          (nth 1 m))))
              (and (<= next (length string))
                   (setq s next))))))
    (nreverse acc)))

;;;###autoload
(defun nelisp-rx-replace (pattern string replacement)
  "Replace the first occurrence of PATTERN in STRING with REPLACEMENT.
Return the new string.  REPLACEMENT is a literal string; backreference
substitution (`\\\\1' etc) is NOT performed in MVP — that lands with
backref support in Phase 9c."
  (let ((m (nelisp-rx-string-match pattern string)))
    (if (null m)
        string
      (concat (substring string 0 (plist-get m :start))
              replacement
              (substring string (plist-get m :end))))))

;;;###autoload
(defun nelisp-rx-replace-all (pattern string replacement)
  "Replace every non-overlapping match of PATTERN in STRING with REPLACEMENT.
Return the new string.  REPLACEMENT is treated literally (see
`nelisp-rx-replace')."
  (let ((matches (nelisp-rx-string-match-all pattern string)))
    (if (null matches)
        string
      (let ((parts nil)
            (cursor 0))
        (dolist (m matches)
          (let ((s (plist-get m :start))
                (e (plist-get m :end)))
            (push (substring string cursor s) parts)
            (push replacement parts)
            (setq cursor e)))
        (push (substring string cursor) parts)
        (apply #'concat (nreverse parts))))))

(provide 'nelisp-regex)

;;; nelisp-regex.el ends here
