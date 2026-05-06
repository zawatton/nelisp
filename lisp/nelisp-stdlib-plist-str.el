;;; nelisp-stdlib-plist-str.el --- Sweep 9 G4 plist + simple string  -*- lexical-binding: t; -*-

(defun plist-member (plist key)
  (let ((cur plist) (found nil))
    (while (and cur (not found))
      (if (eq (car cur) key)
          (setq found cur)
        (setq cur (cdr (cdr cur)))))
    found))

(defun plist-get (plist key)
  (let ((tail (plist-member plist key)))
    (if tail (car (cdr tail)) nil)))

(defun plist-put (plist key value)
  (let ((tail (plist-member plist key)))
    (if tail
        (progn (setcar (cdr tail) value) plist)
      ;; absent — append "key value" by walking to the end
      (if (null plist)
          (cons key (cons value nil))
        (let ((cur plist))
          (while (cdr (cdr cur))
            (setq cur (cdr (cdr cur))))
          (setcdr (cdr cur) (cons key (cons value nil)))
          plist)))))

(defun string-empty-p (s)
  (= (length s) 0))

;; Rust-min (2026-05-06): compare-strings as elisp.  Emacs primitive
;; signature: (compare-strings STR1 START1 END1 STR2 START2 END2
;; &optional IGNORE-CASE).  Returns:
;;   t                       — STR1[start1..end1] equals STR2[start2..end2]
;;   positive integer (1+pos) — STR1 > STR2 at that 1-based offset
;;   negative integer        — STR1 < STR2 at that offset
;; nil starts default to 0; nil ends default to (length STR).
;; IGNORE-CASE non-nil downcases each char before comparing.
(defun compare-strings (str1 start1 end1 str2 start2 end2 &optional ignore-case)
  (let* ((s1 str1) (s2 str2)
         (a (or start1 0))
         (b (or end1 (length s1)))
         (c (or start2 0))
         (d (or end2 (length s2)))
         (len1 (- b a))
         (len2 (- d c))
         (n (if (< len1 len2) len1 len2))
         (i 0)
         (result t))
    (while (and (< i n) (eq result t))
      (let* ((ch1 (aref s1 (+ a i)))
             (ch2 (aref s2 (+ c i)))
             (k1 (if ignore-case (downcase ch1) ch1))
             (k2 (if ignore-case (downcase ch2) ch2)))
        (cond
         ((< k1 k2) (setq result (- (1+ i))))
         ((> k1 k2) (setq result (1+ i)))
         (t (setq i (1+ i))))))
    (cond
     ((not (eq result t)) result)
     ((= len1 len2) t)
     ((< len1 len2) (- (1+ n)))
     (t (1+ n)))))

;; Rust-min (2026-05-06): regexp-quote — pure char-by-char escape of
;; the GNU Emacs regex meta-charset.  Migrated from build-tool/src/
;; eval/builtins.rs `bi_regexp_quote'.
(defun regexp-quote (s)
  (let ((out nil)
        (i 0)
        (n (length s)))
    (while (< i n)
      (let ((ch (aref s i)))
        (when (or (eq ch ?.) (eq ch ?*) (eq ch ?+) (eq ch ??)
                  (eq ch ?\[) (eq ch ?\]) (eq ch ?^) (eq ch ?$)
                  (eq ch ?\\) (eq ch ?\() (eq ch ?\))
                  (eq ch ?\{) (eq ch ?\}) (eq ch ?|))
          (setq out (cons ?\\ out)))
        (setq out (cons ch out)))
      (setq i (1+ i)))
    (concat (nreverse out))))

;; Rust-min (2026-05-06): file-name-* — pure path string slicing.
;; Migrated from build-tool/src/eval/builtins.rs `bi_file_name_*'.

(defun file-name-directory (path)
  "Return the directory part of PATH, or nil if PATH has no slash.
Result keeps the trailing slash."
  (let ((idx -1)
        (i 0)
        (n (length path)))
    (while (< i n)
      (when (eq (aref path i) ?/)
        (setq idx i))
      (setq i (1+ i)))
    (if (< idx 0)
        nil
      (substring path 0 (1+ idx)))))

(defun file-name-nondirectory (path)
  "Return the non-directory part of PATH (= last `/'-delimited component)."
  (let ((idx -1)
        (i 0)
        (n (length path)))
    (while (< i n)
      (when (eq (aref path i) ?/)
        (setq idx i))
      (setq i (1+ i)))
    (if (< idx 0)
        path
      (substring path (1+ idx)))))

(defun file-name-as-directory (path)
  "Return PATH with a trailing `/' appended if not already present."
  (cond
   ((= (length path) 0) "/")
   ((eq (aref path (1- (length path))) ?/) path)
   (t (concat path "/"))))

(defun directory-file-name (path)
  "Return PATH with a single trailing `/' stripped (= keeps `/' for root)."
  (let ((n (length path)))
    (cond
     ((<= n 1) path)
     ((eq (aref path (1- n)) ?/) (substring path 0 (1- n)))
     (t path))))

;; Rust-min (2026-05-06): string-trim family + string-prefix-p /
;; string-suffix-p — pure string slicing.  Migrated from
;; build-tool/src/eval/builtins.rs.

(defun nelisp-stdlib--whitespace-p (ch)
  "Return non-nil when CH (= integer codepoint) is ASCII whitespace.
Matches the Emacs default whitespace class for `string-trim'."
  (or (eq ch ?\s) (eq ch ?\t) (eq ch ?\n) (eq ch ?\r)
      (eq ch ?\f) (eq ch 11)))                ; 11 = ?\v

(defun string-trim-left (s &optional _regexp)
  "Strip leading whitespace from S.  REGEXP arg accepted for API
parity but ignored — use the polyfill in `replace-regexp-in-string'
when a custom pattern is needed."
  (let ((i 0)
        (n (length s)))
    (while (and (< i n) (nelisp-stdlib--whitespace-p (aref s i)))
      (setq i (1+ i)))
    (if (= i 0) s (substring s i))))

(defun string-trim-right (s &optional _regexp)
  "Strip trailing whitespace from S."
  (let ((n (length s))
        (i (length s)))
    (while (and (> i 0) (nelisp-stdlib--whitespace-p (aref s (1- i))))
      (setq i (1- i)))
    (if (= i n) s (substring s 0 i))))

(defun string-trim (s &optional _trim-left _trim-right)
  "Strip leading and trailing whitespace from S."
  (string-trim-left (string-trim-right s)))

(defun string-prefix-p (prefix s &optional ignore-case)
  "Return non-nil when S starts with PREFIX.
IGNORE-CASE non-nil → case-insensitive comparison."
  (let ((plen (length prefix))
        (slen (length s)))
    (if (> plen slen)
        nil
      (eq t (compare-strings prefix 0 plen s 0 plen ignore-case)))))

(defun string-suffix-p (suffix s &optional ignore-case)
  "Return non-nil when S ends with SUFFIX.
IGNORE-CASE non-nil → case-insensitive comparison."
  (let* ((suflen (length suffix))
         (slen (length s))
         (start (- slen suflen)))
    (if (< start 0)
        nil
      (eq t (compare-strings suffix 0 suflen s start slen ignore-case)))))

;; Rust-min (2026-05-06 batch 3): delete-dups / string-search /
;; mapconcat — pure-elisp implementations.  Migrated from
;; build-tool/src/eval/builtins.rs `bi_delete_dups' / `bi_string_search'
;; / `bi_mapconcat'.

(defun delete-dups (list)
  "Return LIST with duplicate elements removed (`equal' test).
First occurrence is kept; subsequent duplicates are dropped.  Pure
(= does NOT mutate LIST destructively, unlike host Emacs)."
  (let ((acc nil)
        (cur list))
    (while cur
      (let ((elt (car cur))
            (found nil)
            (a acc))
        (while (and a (not found))
          (when (equal (car a) elt) (setq found t))
          (setq a (cdr a)))
        (unless found
          (setq acc (cons elt acc))))
      (setq cur (cdr cur)))
    (nreverse acc)))

(defun string-search (needle haystack &optional from)
  "Return the index of the first occurrence of NEEDLE in HAYSTACK at
or after FROM (default 0), or nil if there is no match.

Empty NEEDLE returns FROM (matching host Emacs / Rust `str::find').
NEEDLE longer than HAYSTACK returns nil."
  (let* ((nlen (length needle))
         (hlen (length haystack))
         (start (or from 0)))
    (cond
     ((> start hlen) nil)
     ((= nlen 0) start)
     ((> nlen hlen) nil)
     (t
      (let ((i start)
            (found nil)
            (limit (- hlen nlen)))
        (while (and (not found) (<= i limit))
          (let ((j 0)
                (match t))
            (while (and match (< j nlen))
              (if (eq (aref needle j) (aref haystack (+ i j)))
                  (setq j (1+ j))
                (setq match nil)))
            (if match
                (setq found i)
              (setq i (1+ i)))))
        found)))))

;; Rust-min (2026-05-06 batch 5b): bool-vector / char-table.
;;
;; bool-vector: prior Rust impl had a dedicated `Sexp::BoolVector'
;; variant backing.  The user-facing surface (`bool-vector-p',
;; constructors) is migrated to plain elisp using regular vectors
;; with t/nil entries.  The internal Rust variant is kept ALIVE
;; only for image-format backward-compat (= legacy v2 images
;; carrying TAG_BOOL_VECTOR still decode); the elisp surface no
;; longer creates new instances of it, so practical usage routes
;; entirely through `Sexp::Vector'.
;;
;; char-table: was unused throughout NeLisp's lisp/ + test/ corpus
;; and in nelisp-emacs's elisp side.  The Rust dispatch +
;; implementations were retired wholesale.  When a future caller
;; needs char-table semantics, it can be added back as an elisp
;; tagged-plist representation; until then the symbols are
;; void-function so callers fail fast instead of silently no-op.

(defun make-bool-vector (length init)
  "Return a vector of LENGTH bool entries initialised to (and INIT t).
Implementation note: bool-vector is a regular vector here; see the
file commentary for migration history.  Pre-converts INIT to a
proper t/nil so that downstream `bool-vector-p' checks pass."
  (make-vector length (and init t)))

(defun bool-vector (&rest args)
  "Variadic constructor: each ARG is coerced to t/nil and stored in a
fresh vector."
  (let* ((n (length args))
         (out (make-vector n nil))
         (i 0)
         (tail args))
    (while tail
      (aset out i (and (car tail) t))
      (setq i (1+ i))
      (setq tail (cdr tail)))
    out))

(defun bool-vector-p (object)
  "Return non-nil when OBJECT is a vector all of whose elements are
t or nil (= bool-vector contract).  An empty vector counts as a
bool-vector."
  (and (vectorp object)
       (let ((i 0)
             (n (length object))
             (ok t))
         (while (and ok (< i n))
           (let ((v (aref object i)))
             (unless (or (eq v t) (null v))
               (setq ok nil)))
           (setq i (1+ i)))
         ok)))

;; Rust-min (2026-05-06 batch 5a): string-to-number — pure-elisp parse
;; (= no new Rust primitives required, since NeLisp's mixed-mode
;; arithmetic already promotes int op float → float).

(defun nelisp-stdlib--digit-value (ch radix)
  "Return integer 0..RADIX-1 encoded by CH, or nil if CH is not a digit
in the given RADIX (= 2..36).  Accepts both upper- and lowercase
letters for RADIX > 10."
  (let ((v (cond
            ((and (>= ch ?0) (<= ch ?9)) (- ch ?0))
            ((and (>= ch ?a) (<= ch ?z)) (+ 10 (- ch ?a)))
            ((and (>= ch ?A) (<= ch ?Z)) (+ 10 (- ch ?A)))
            (t nil))))
    (if (and v (< v radix)) v nil)))

(defun string-to-number (s &optional radix)
  "Parse a number from the leading portion of S.
RADIX (default 10) selects the integer base.  Float syntax (`.',
`e' / `E') is recognised only when RADIX is 10 or nil — otherwise
only the integer prefix is accepted (matching the host Emacs
contract).  Returns 0 when no leading digit is found.

Pure-elisp impl: int parse drives a digit loop; float branch uses
`(/ frac 1.0 ...)' for promote-on-mixed semantics and a multiply
loop for the exponent (= no `expt' / `float' primitive needed)."
  (let* ((r (or radix 10))
         (n (length s))
         (i 0))
    ;; Skip leading whitespace.
    (while (and (< i n) (nelisp-stdlib--whitespace-p (aref s i)))
      (setq i (1+ i)))
    (let ((sign 1))
      (cond
       ((and (< i n) (eq (aref s i) ?-))
        (setq sign -1)
        (setq i (1+ i)))
       ((and (< i n) (eq (aref s i) ?+))
        (setq i (1+ i))))
      (let ((int-part 0)
            (int-digits 0))
        ;; Integer-digit loop.
        (let ((continue t))
          (while (and continue (< i n))
            (let ((d (nelisp-stdlib--digit-value (aref s i) r)))
              (cond
               (d
                (setq int-part (+ (* int-part r) d))
                (setq i (1+ i))
                (setq int-digits (1+ int-digits)))
               (t (setq continue nil))))))
        (cond
         ;; Float branch (only when radix = 10 and we hit `.' / `e' / `E').
         ((and (= r 10)
               (< i n)
               (or (eq (aref s i) ?.)
                   (eq (aref s i) ?e)
                   (eq (aref s i) ?E)))
          (let ((frac-num 0)
                (frac-denom 1)
                (exp-sign 1)
                (exp-val 0)
                (has-exp nil))
            ;; Optional fractional part.
            (when (and (< i n) (eq (aref s i) ?.))
              (setq i (1+ i))
              (let ((continue t))
                (while (and continue (< i n))
                  (let ((d (nelisp-stdlib--digit-value (aref s i) 10)))
                    (cond
                     (d
                      (setq frac-num (+ (* frac-num 10) d))
                      (setq frac-denom (* frac-denom 10))
                      (setq i (1+ i)))
                     (t (setq continue nil)))))))
            ;; Optional exponent.
            (when (and (< i n)
                       (or (eq (aref s i) ?e) (eq (aref s i) ?E)))
              (setq i (1+ i))
              (cond
               ((and (< i n) (eq (aref s i) ?-))
                (setq exp-sign -1)
                (setq i (1+ i)))
               ((and (< i n) (eq (aref s i) ?+))
                (setq i (1+ i))))
              (let ((continue t))
                (while (and continue (< i n))
                  (let ((d (nelisp-stdlib--digit-value (aref s i) 10)))
                    (cond
                     (d
                      (setq exp-val (+ (* exp-val 10) d))
                      (setq i (1+ i))
                      (setq has-exp t))
                     (t (setq continue nil)))))))
            ;; Compute value: (sign * (int-part + frac-num/frac-denom)) * 10^exp.
            (let* ((mag (+ int-part (/ frac-num (* frac-denom 1.0))))
                   (val (* sign mag)))
              (when has-exp
                (let ((mul (if (>= exp-sign 0) 10.0 0.1))
                      (k exp-val))
                  (while (> k 0)
                    (setq val (* val mul))
                    (setq k (1- k)))))
              val)))
         ;; Pure integer.
         ((> int-digits 0) (* sign int-part))
         (t 0))))))

;; Rust-min (2026-05-06 batch 4): copy-tree + sort.

(defun copy-tree (tree &optional vecp)
  "Return a deep copy of TREE.  Conses are recursively copied; non-
cons leaves are returned unchanged.  When VECP is non-nil, vectors
inside TREE are also copied recursively (= matches the host Emacs
contract)."
  (cond
   ((consp tree)
    (cons (copy-tree (car tree) vecp)
          (copy-tree (cdr tree) vecp)))
   ((and vecp (vectorp tree))
    (let* ((n (length tree))
           (out (make-vector n nil))
           (i 0))
      (while (< i n)
        (aset out i (copy-tree (aref tree i) vecp))
        (setq i (1+ i)))
      out))
   (t tree)))

(defun nelisp-stdlib--sort-merge (a b pred)
  "Stable merge of two sorted lists A and B under PRED."
  (let ((acc nil))
    (while (and a b)
      (cond
       ((funcall pred (car a) (car b))
        (setq acc (cons (car a) acc))
        (setq a (cdr a)))
       (t
        (setq acc (cons (car b) acc))
        (setq b (cdr b)))))
    (while a (setq acc (cons (car a) acc)) (setq a (cdr a)))
    (while b (setq acc (cons (car b) acc)) (setq b (cdr b)))
    (nreverse acc)))

(defun nelisp-stdlib--sort-split (list)
  "Split LIST into two roughly equal halves; return (FIRST . SECOND)."
  (let ((slow list)
        (fast (cdr list)))
    (while (and fast (cdr fast))
      (setq slow (cdr slow))
      (setq fast (cdr (cdr fast))))
    (let ((second (cdr slow)))
      (setcdr slow nil)
      (cons list second))))

(defun nelisp-stdlib--sort-list (list pred)
  "Recursive merge-sort of LIST under PRED.  Mutates LIST's spine via
`setcdr' inside `--sort-split'; callers should ensure LIST is owned."
  (cond
   ((null list) nil)
   ((null (cdr list)) list)
   (t
    (let ((halves (nelisp-stdlib--sort-split list)))
      (nelisp-stdlib--sort-merge
       (nelisp-stdlib--sort-list (car halves) pred)
       (nelisp-stdlib--sort-list (cdr halves) pred)
       pred)))))

(defun sort (seq pred)
  "Sort SEQ (= list or vector) by PRED.  Returns the sorted sequence
of the same shape as SEQ.  Stable for equal elements (= merge sort).

Note: in host Emacs `sort' destructively rearranges LIST; here we
use `setcdr' in the merge-split helper, so the caller's list spine
may be modified.  Callers depending on input identity should
`copy-sequence' first."
  (cond
   ((null seq) nil)
   ((consp seq)
    ;; Copy the spine first so the input list's identity is preserved.
    (let ((work (copy-sequence seq)))
      (nelisp-stdlib--sort-list work pred)))
   ((vectorp seq)
    (let* ((n (length seq))
           (lst nil)
           (i (1- n)))
      (while (>= i 0)
        (setq lst (cons (aref seq i) lst))
        (setq i (1- i)))
      (let* ((sorted (nelisp-stdlib--sort-list lst pred))
             (out (make-vector n nil))
             (j 0)
             (cur sorted))
        (while cur
          (aset out j (car cur))
          (setq j (1+ j))
          (setq cur (cdr cur)))
        out)))
   (t
    (signal 'wrong-type-argument (list 'sequencep seq)))))

(defun mapconcat (fn seq &optional sep)
  "Apply FN to each element of SEQ; concat the resulting strings,
joined by SEP (default empty string).  SEQ is iterated as a list
(matching the Rust builtin's MVP contract — vector / string SEQ
forms are out of scope)."
  (let ((parts nil)
        (tail seq))
    (while tail
      (setq parts (cons (funcall fn (car tail)) parts))
      (setq tail (cdr tail)))
    (let ((rev (nreverse parts))
          (joiner (or sep "")))
      (cond
       ((null rev) "")
       ((null (cdr rev)) (car rev))
       (t
        (let ((acc (car rev))
              (cur (cdr rev)))
          (while cur
            (setq acc (concat acc joiner (car cur)))
            (setq cur (cdr cur)))
          acc))))))

;; nelisp-stdlib-plist-str.el ends here
