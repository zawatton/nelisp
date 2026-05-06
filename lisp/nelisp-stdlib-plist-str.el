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

;; Rust-min batch 6m (2026-05-06): `format' migrated from Rust to
;; elisp.  The previous `bi_format' (~200 LOC) + helpers (FormatSpec
;; struct, pad_field, fmt_int_with_sign, fmt_float_default) handled
;; spec parsing, flags/width/precision parsing, %s/%S/%c/%%/%d/%i/
;; %x/%X/%o/%b/%f/%F/%e/%E/%g/%G dispatch, sign + padding.  Of those,
;; ONLY the IEEE-754 float→string conversion genuinely needs Rust
;; (= the `{:.*}' / `{:.*e}' / `{:.*E}' format machinery is Grisu /
;; round-to-nearest, not reproducible in pure elisp without a 1000+
;; LOC dragon4 polyfill).  Everything else (= parser, padding, sign,
;; integer→radix-string via repeated `mod' / `/') is pure elisp.
;;
;; The Rust-side surface left after this batch:
;;   (nelisp--format-float-body CONV PREC X)
;;     CONV  — char codepoint integer (?f/?F/?e/?E/?g/?G)
;;     PREC  — non-negative integer (precision)
;;     X     — float OR integer (the magnitude — sign is added in elisp)
;;     => unsigned, unpadded body string
;;
;; Plus `truncate' (new tiny primitive) so `%d' on a float behaves
;; the same as before (= `as i64' = trunc-toward-zero).

(defun nelisp--format-int-abs-decimal (n)
  "Return the unsigned decimal digit-string of |N|.  N is an integer."
  (cond
   ((= n 0) "0")
   (t
    (let ((m (if (< n 0) (- n) n))
          (acc nil))
      (while (> m 0)
        (setq acc (cons (+ ?0 (mod m 10)) acc))
        (setq m (/ m 10)))
      (concat acc)))))

(defun nelisp--format-int-abs-radix (n base upcase)
  "Return the unsigned digit-string of |N| in BASE.
UPCASE non-nil → use A-Z for digits >= 10."
  (cond
   ((= n 0) "0")
   (t
    (let ((m (if (< n 0) (- n) n))
          (acc nil))
      (while (> m 0)
        (let ((d (mod m base)))
          (setq acc (cons (cond
                           ((< d 10) (+ ?0 d))
                           (upcase   (+ ?A (- d 10)))
                           (t        (+ ?a (- d 10))))
                          acc))
          (setq m (/ m base))))
      (concat acc)))))

(defun nelisp--format-pad (body width left-align zero-pad)
  "Pad BODY to WIDTH chars.  LEFT-ALIGN non-nil → pad on the right
with spaces.  ZERO-PAD non-nil → pad on the left with `0' (= and
keep any sign char at the front, padding after it)."
  (cond
   ((null width) body)
   (t
    (let ((blen (length body)))
      (cond
       ((>= blen width) body)
       (t
        (let* ((n (- width blen))
               (ch (if (and zero-pad (not left-align)) ?0 ?\s))
               (pad (make-string n ch)))
          (cond
           (left-align (concat body pad))
           ((and zero-pad
                 (> blen 0)
                 (or (eq (aref body 0) ?-) (eq (aref body 0) ?+)))
            (concat (substring body 0 1) pad (substring body 1)))
           (t (concat pad body))))))))))

(defun nelisp--format-int-with-sign (n plus space)
  "Return decimal string for integer N with sign prefix (`-'/`+'/` ').
PLUS / SPACE govern the non-negative case."
  (let ((abs-part (nelisp--format-int-abs-decimal n)))
    (cond
     ((< n 0) (concat "-" abs-part))
     (plus    (concat "+" abs-part))
     (space   (concat " " abs-part))
     (t       abs-part))))

(defun nelisp--format-int-radix (n conv sharp)
  "Format integer N in radix per CONV (= ?x / ?X / ?o / ?b).
SHARP non-nil → prepend the C-style 0x / 0X / 0o / 0b prefix.
Negative N renders as a minus sign + abs (= mathematical value,
not two's-complement) — matches the host Emacs / previous Rust
contract."
  (let* ((upcase (eq conv ?X))
         (base (cond ((or (eq conv ?x) (eq conv ?X)) 16)
                     ((eq conv ?o) 8)
                     ((eq conv ?b) 2)))
         (abs-part (nelisp--format-int-abs-radix n base upcase))
         (prefix (if sharp
                     (cond ((eq conv ?x) "0x")
                           ((eq conv ?X) "0X")
                           ((eq conv ?o) "0o")
                           ((eq conv ?b) "0b"))
                   "")))
    (if (< n 0)
        (concat "-" prefix abs-part)
      (concat prefix abs-part))))

(defun nelisp--format-coerce-int (arg)
  "Coerce ARG to an integer for %d/%i/%x/%X/%o/%b.  Float → trunc
toward zero; integer → identity; else signal `wrong-type-argument'."
  (cond
   ((integerp arg) arg)
   ((floatp arg) (truncate arg))
   (t (signal 'wrong-type-argument (list 'integerp arg)))))

(defun nelisp--format-coerce-num (arg)
  "Coerce ARG to a number for %f/%F/%e/%E/%g/%G.  Float / int →
identity; else signal `wrong-type-argument'."
  (cond
   ((floatp arg) arg)
   ((integerp arg) arg)
   (t (signal 'wrong-type-argument (list 'numberp arg)))))

(defun format (template &rest args)
  "Format TEMPLATE with ARGS.  Supports %s/%S/%c/%%/%d/%i/%x/%X/%o/
%b/%f/%F/%e/%E/%g/%G with flags `-+ 0#', optional WIDTH and
.PRECISION.  Pure-elisp dispatcher; only IEEE-754 float→string
goes through `nelisp--format-float-body' (Rust)."
  (let ((i 0)
        (n (length template))
        (out nil)
        (arg-i 0))
    (while (< i n)
      (let ((c (aref template i)))
        (cond
         ((not (eq c ?%))
          (setq out (cons (string c) out))
          (setq i (1+ i)))
         (t
          (setq i (1+ i))
          (let ((left-align nil) (zero-pad nil) (plus nil)
                (space nil) (sharp nil) (width nil) (precision nil))
            ;; flags
            (let ((flag-cont t))
              (while (and flag-cont (< i n))
                (let ((pc (aref template i)))
                  (cond
                   ((eq pc ?-) (setq left-align t) (setq i (1+ i)))
                   ((eq pc ?0) (setq zero-pad t) (setq i (1+ i)))
                   ((eq pc ?+) (setq plus t) (setq i (1+ i)))
                   ((eq pc ?\s) (setq space t) (setq i (1+ i)))
                   ((eq pc ?#) (setq sharp t) (setq i (1+ i)))
                   (t (setq flag-cont nil))))))
            ;; width
            (let ((w 0) (any nil) (w-cont t))
              (while (and w-cont (< i n))
                (let ((d (aref template i)))
                  (cond
                   ((and (>= d ?0) (<= d ?9))
                    (setq w (+ (* w 10) (- d ?0)))
                    (setq any t)
                    (setq i (1+ i)))
                   (t (setq w-cont nil)))))
              (when any (setq width w)))
            ;; precision
            (when (and (< i n) (eq (aref template i) ?.))
              (setq i (1+ i))
              (let ((p 0) (p-cont t))
                (while (and p-cont (< i n))
                  (let ((d (aref template i)))
                    (cond
                     ((and (>= d ?0) (<= d ?9))
                      (setq p (+ (* p 10) (- d ?0)))
                      (setq i (1+ i)))
                     (t (setq p-cont nil)))))
                (setq precision p)))
            ;; conversion
            (cond
             ((>= i n) (setq out (cons "%" out)))
             (t
              (let ((conv (aref template i)))
                (setq i (1+ i))
                (cond
                 ((eq conv ?%) (setq out (cons "%" out)))
                 ((eq conv ?s)
                  (let* ((arg (nth arg-i args))
                         (b (if (stringp arg) arg (prin1-to-string arg))))
                    (setq arg-i (1+ arg-i))
                    (when precision
                      (setq b (substring b 0 (if (< precision (length b))
                                                 precision (length b)))))
                    (setq out (cons (nelisp--format-pad b width left-align zero-pad) out))))
                 ((eq conv ?S)
                  (let* ((arg (nth arg-i args))
                         (b (prin1-to-string arg)))
                    (setq arg-i (1+ arg-i))
                    (setq out (cons (nelisp--format-pad b width left-align zero-pad) out))))
                 ((or (eq conv ?d) (eq conv ?i))
                  (let* ((arg (nth arg-i args))
                         (m (nelisp--format-coerce-int arg))
                         (b (nelisp--format-int-with-sign m plus space)))
                    (setq arg-i (1+ arg-i))
                    (setq out (cons (nelisp--format-pad b width left-align zero-pad) out))))
                 ((or (eq conv ?x) (eq conv ?X) (eq conv ?o) (eq conv ?b))
                  (let* ((arg (nth arg-i args))
                         (m (nelisp--format-coerce-int arg))
                         (b (nelisp--format-int-radix m conv sharp)))
                    (setq arg-i (1+ arg-i))
                    (setq out (cons (nelisp--format-pad b width left-align zero-pad) out))))
                 ((eq conv ?c)
                  (let* ((arg (nth arg-i args))
                         (b (string arg)))
                    (setq arg-i (1+ arg-i))
                    (setq out (cons (nelisp--format-pad b width left-align zero-pad) out))))
                 ((or (eq conv ?f) (eq conv ?F)
                      (eq conv ?e) (eq conv ?E)
                      (eq conv ?g) (eq conv ?G))
                  (let* ((arg (nth arg-i args))
                         (x (nelisp--format-coerce-num arg))
                         (mag (if (< x 0) (- x) x))
                         (abs-body (nelisp--format-float-body
                                    conv (or precision 6) mag))
                         (b (cond
                             ((< x 0) (concat "-" abs-body))
                             (plus    (concat "+" abs-body))
                             (space   (concat " " abs-body))
                             (t       abs-body))))
                    (setq arg-i (1+ arg-i))
                    (setq out (cons (nelisp--format-pad b width left-align zero-pad) out))))
                 (t (signal 'error (list (concat "format: unsupported conversion %"
                                                  (string conv))))))))))))))
    (apply (function concat) (nreverse out))))

;; Rust-min (2026-05-06 batch 6c): leaf string/sequence builtins that
;; compose trivially over `concat' / `apply' / `vector' / `append'.
;; All 5 below were thin wrappers in `bi_*' with no Sexp-internal
;; logic — pure dispatch + char-codepoint validation.  The elisp
;; versions inherit the same validation by re-using `concat' (= the
;; Rust primitive that already rejects non-int-or-string args).
(defun string (&rest args)
  (concat args))

(defun char-to-string (ch)
  (string ch))

(defun string-to-char (s)
  (if (= (length s) 0) 0 (aref s 0)))

(defalias 'unibyte-string 'string)

(defun vconcat (&rest args)
  (apply (function vector)
         (apply (function append) (append args (list nil)))))

;; Rust-min (2026-05-06 batch 6b): substring as elisp.  Walks chars
;; via `aref' (= O(N) per access in the current Sexp::Str repr) and
;; rebuilds via `concat' of an int-list — same hot-loop pattern as
;; existing `regexp-quote' / `string-trim*' / `string-search'
;; migrations.  Vector substring stays in Rust because the elisp
;; path here is string-only by design (= scope-matched to the
;; previous `bi_substring' which only accepted strings).
(defun substring (str from &optional to)
  (let* ((len (length str))
         (from (if (< from 0) (+ len from) from))
         (to (cond ((null to) len)
                   ((< to 0) (+ len to))
                   (t to))))
    (when (or (< from 0) (< to from) (> to len))
      (error "Args out of range"))
    (let ((chars nil) (i to))
      (while (> i from)
        (setq i (1- i))
        (setq chars (cons (aref str i) chars)))
      (concat chars))))

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

;; Rust-min batch 6n (2026-05-06): `string-equal' migrated from Rust
;; to elisp.  The previous `bi_string_eq' was a 4-line wrapper:
;;   (1) coerce both args via `string_value' (= Str/MutStr → string,
;;       Symbol → name, Nil → "nil", T → "t", else wrong-type)
;;   (2) `==' compare the two strings
;; All steps are pure elisp once we have `symbol-name' (= Rust
;; primitive that already coerces nil/t to "nil"/"t") and `equal'
;; (= structural compare, which on two strings reduces to char-by-
;; char).  The defalias `string=' → `string-equal' from batch 6e
;; (lisp/nelisp-stdlib-misc.el) survives unchanged.
(defun string-equal (a b)
  (let ((sa (cond ((stringp a) a)
                  ((symbolp a) (symbol-name a))
                  (t (signal 'wrong-type-argument (list 'stringp a)))))
        (sb (cond ((stringp b) b)
                  ((symbolp b) (symbol-name b))
                  (t (signal 'wrong-type-argument (list 'stringp b))))))
    (equal sa sb)))

;; Rust-min batch 6n (2026-05-06): `split-string' migrated from Rust
;; to elisp.  The previous `bi_split_string' (~25 LOC) implemented
;; the literal-separator subset of host Emacs's contract — when
;; SEPARATORS is a non-empty string, split on each non-overlapping
;; occurrence; otherwise split on runs of whitespace and drop
;; leading/trailing empties (matching Rust's `str::split_whitespace').
;; The OMIT-NULLS / TRIM args are accepted for API parity but
;; *ignored* by the Rust impl too — preserved here for behavioural
;; bug-for-bug compatibility (= callers that pass `t' for omit-nulls
;; have always silently retained empty fields on NeLisp; raising the
;; coverage to "real" omit-nulls is a separate batch).
;;
;; Whitespace classification uses `nelisp-stdlib--whitespace-p'
;; (ASCII space/tab/newline/CR/FF/VT) — slightly narrower than
;; Rust's `char::is_whitespace' which covers Unicode whitespace.
;; In practice all extant NeLisp / nelisp-emacs callers split on
;; ASCII separators so the diff is invisible.
(defun nelisp--split-on-literal (s sep)
  "Split S on each non-overlapping literal occurrence of SEP.
Returns a list of strings.  SEP must be non-empty."
  (let ((acc nil)
        (start 0)
        (slen (length s))
        (seplen (length sep))
        (continue t))
    (while continue
      (let ((idx (string-search sep s start)))
        (cond
         (idx
          (setq acc (cons (substring s start idx) acc))
          (setq start (+ idx seplen)))
         (t
          (setq acc (cons (substring s start slen) acc))
          (setq continue nil)))))
    (nreverse acc)))

(defun nelisp--split-on-whitespace (s)
  "Split S on runs of whitespace; drop leading/trailing empties.
Matches the Rust `str::split_whitespace' contract."
  (let ((acc nil)
        (i 0)
        (n (length s)))
    (while (< i n)
      (while (and (< i n) (nelisp-stdlib--whitespace-p (aref s i)))
        (setq i (1+ i)))
      (let ((start i))
        (while (and (< i n) (not (nelisp-stdlib--whitespace-p (aref s i))))
          (setq i (1+ i)))
        (when (> i start)
          (setq acc (cons (substring s start i) acc)))))
    (nreverse acc)))

(defun split-string (s &optional separators _omit-nulls _trim)
  (cond
   ((not (stringp s))
    (signal 'wrong-type-argument (list 'stringp s)))
   ((and (stringp separators) (> (length separators) 0))
    (nelisp--split-on-literal s separators))
   (t
    (nelisp--split-on-whitespace s))))

;; Rust-min batch 6p (2026-05-06): `upcase' / `downcase' / `capitalize'
;; migrated from Rust to elisp.  The previous Rust impls used
;; `char::to_uppercase' / `to_lowercase' / `is_alphabetic' (= full
;; Unicode case mapping), but NeLisp strings are stored byte-as-char
;; (= each UTF-8 byte becomes its own Latin-1 codepoint), so
;; "Unicode case mapping" was already broken for multi-byte input —
;; only ASCII case mapping was meaningful.  The elisp versions below
;; restrict to ASCII explicitly, which is functionally identical to
;; the previous Rust behaviour on the inputs callers actually pass.
;; Non-ASCII bytes pass through unchanged.

(defun nelisp--ascii-upcase-char (ch)
  "Return the ASCII upper-case form of CH (= int char), or CH itself
when CH is not an ASCII letter."
  (if (and (>= ch ?a) (<= ch ?z)) (- ch 32) ch))

(defun nelisp--ascii-downcase-char (ch)
  "Return the ASCII lower-case form of CH (= int char), or CH itself
when CH is not an ASCII letter."
  (if (and (>= ch ?A) (<= ch ?Z)) (+ ch 32) ch))

(defun nelisp--ascii-letter-p (ch)
  "Return non-nil when CH is an ASCII letter (A–Z or a–z)."
  (or (and (>= ch ?A) (<= ch ?Z))
      (and (>= ch ?a) (<= ch ?z))))

(defun nelisp--map-string (s fn)
  "Apply FN to each char of S, return the resulting string."
  (let ((out nil)
        (i 0)
        (n (length s)))
    (while (< i n)
      (setq out (cons (funcall fn (aref s i)) out))
      (setq i (1+ i)))
    (concat (nreverse out))))

(defun upcase (obj)
  "Convert OBJ to upper case.  OBJ may be a string (each ASCII letter
upcased) or an int char (single-char upcase).  Non-ASCII bytes pass
through unchanged."
  (cond
   ((stringp obj) (nelisp--map-string obj (function nelisp--ascii-upcase-char)))
   ((integerp obj) (nelisp--ascii-upcase-char obj))
   (t (signal 'wrong-type-argument (list 'stringp-or-characterp obj)))))

(defun downcase (obj)
  "Convert OBJ to lower case.  OBJ may be a string or an int char.
Non-ASCII bytes pass through unchanged."
  (cond
   ((stringp obj) (nelisp--map-string obj (function nelisp--ascii-downcase-char)))
   ((integerp obj) (nelisp--ascii-downcase-char obj))
   (t (signal 'wrong-type-argument (list 'stringp-or-characterp obj)))))

(defun capitalize (s)
  "Title-case S: upcase every word-initial ASCII letter, downcase the
rest of each word.  Word boundary = transition non-letter → letter.
Non-letter chars pass through unchanged."
  (cond
   ((not (stringp s))
    (signal 'wrong-type-argument (list 'stringp s)))
   (t
    (let ((out nil)
          (i 0)
          (n (length s))
          (at-word-start t))
      (while (< i n)
        (let ((ch (aref s i)))
          (cond
           ((not (nelisp--ascii-letter-p ch))
            (setq out (cons ch out))
            (setq at-word-start t))
           (at-word-start
            (setq out (cons (nelisp--ascii-upcase-char ch) out))
            (setq at-word-start nil))
           (t
            (setq out (cons (nelisp--ascii-downcase-char ch) out)))))
        (setq i (1+ i)))
      (concat (nreverse out))))))

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
