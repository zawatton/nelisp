;;; nelisp-stdlib-regexp.el --- pure-elisp Emacs-regexp matcher  -*- lexical-binding: t; -*-

;; Parser/backtracker state stays dynamically bound through declared specials.
;; Lexical locals admit the same Lisp bodies to the general native compiler.
;; Continuations are explicit and captures use :savestart/:saveend nodes.
;; The nlre- names support host differential tests; standalone installs aliases.

;; ---- parser: pattern string -> node list (a "seq") ----

(defvar nlre--gcount 0 "Group counter during a parse.")
(defvar nlre--fold nil "Non-nil while the current match folds case.")
(defvar nlre--inhibit-search-fastmap nil "Non-nil for direct looking-at matches.")
(defvar search-spaces-regexp nil
  "Regexp substituted for literal spaces while compiling search regexps.")
(declare-function nelisp--split-on-char "nelisp-stdlib-regexp" (string char omit-empty))
(declare-function emacs-char-table-ref "emacs-char-table" (table character))

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
(defvar nlre--case-table-reader
  (and (fboundp 'current-case-table) 'current-case-table))
(defvar nlre--syntax-table nil
  "Syntax table of the match in progress, read once per match.
GNU sets up gl_state's syntax table once per search
\(`RE_SETUP_SYNTAX_TABLE_FOR_OBJECT', regex-emacs.c:3462) and each
`syntaxspec' then reads `SYNTAX (c)' from it (regex-emacs.c:5195).
`nlre-string-match' resets this to nil (the other entries bind it); the
first syntax test stores the
table, t when the provider's table is not a char-table (its `char-syntax'
answers instead), or `none' before any `char-syntax' exists.")
(defconst nlre--syntax-designators " .w_()'\"$\\/<>@!|"
  "GNU syntax.c:1084 `syntax_code_spec': the designator of each class code.")
(defvar nlre--translate-table nil
  "Canonical case translation descriptor bound during compilation
and matching.  Nil selects the portable downcase fallback.")
(defvar nlre--syntax-cache-generation 0
  "Global generation of syntax-dependent compiled regexp entries.")
(defvar nlre--syntax-cache-wrapper nil
  "Function cell installed for syntax-cache invalidation.")
(defvar nlre--syntax-cache-original-function nil
  "Underlying mutator last captured by the syntax-cache wrapper.")
(defvar nlre--syntax-cache-installing nil)
(defvar nlre--translate-cache (make-hash-table :test 'eq :weakness 'key)
  "Canon identity -> [table, lookup, ASCII page, ASCII-fold, public lookup,
native table, unibyte-fold].  ASCII-fold is non-nil when the canon maps
0..127 as the built-in fold does, unibyte-fold when it translates a unibyte
target as the built-in fold does (`nlre--unibyte-fold-agrees-p'); both feed
`nlre--search-translate-table'.  The native
table slot is the canon itself when the runtime owns it:
`nlre--fold-char' then translates a character as GNU's `char_table_translate'
does (character.h:592), with one table read.
Case-table helpers invalidate the canon identity when changing mappings.
After raw writes to a derived canon, use `nlre--compiled-cache-clear', as
with raw syntax-table writes and the syntax cache.")
(defvar nlre--case-owner-cache (make-hash-table :test 'eq :weakness 'key)
  "Case-table identity -> extra accessor, under `nlre--case-public-extra'.")
(defvar nlre--case-public-extra nil)
(defvar nlre--current-buffer-function #'current-buffer
  "`current-buffer', called through this variable in the hot entries.
Its inline opcode (112) is not in the VM allowlist and one such opcode
demotes a whole function to the interpreter; `funcall' of #'current-buffer
is folded back into that opcode by the byte optimizer.")
(defvar nlre--translate-memo-buffer nil
  "Buffer whose case-fold translation `nlre--translate-memo' holds.")
(defvar nlre--translate-memo-epoch nil
  "`nelisp-casetab--epoch' when `nlre--translate-memo' was read.")
(defvar nlre--translate-memo nil
  "The memoized result of `nlre--read-translate-table'.")
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
        nlre--translate-cache (make-hash-table :test 'eq :weakness 'key)
        nlre--case-owner-cache (make-hash-table :test 'eq :weakness 'key)
        nlre--translate-memo-buffer nil
        nlre--compiled-cache-count 0
        nlre--compiled-cache-lru nil
        nlre--compiled-cache-last nil))

;; Entry: [PATTERN FOLD/TRANSLATE-KEY SEARCH-SPACES SYNTAX-DEPENDENT
;;         SYNTAX-TABLE COMPILED TICK MULTIBYTE ORIGINAL-PATTERN GENERATION].
;; `nlre--compiled-cache-lru'
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
(defun nlre--pattern-syntax-dependent-p (pat)
  "Non-nil when PAT contains an escape whose meaning uses the syntax table."
  (let ((i 0) (n (length pat)) hit)
    (while (and (not hit) (< i n))
      (if (and (eq (aref pat i) ?\\) (< (1+ i) n))
          (let ((c (aref pat (1+ i))))
            (when (memq c '(?s ?S ?w ?W ?b ?B ?< ?> ?_)) (setq hit t))
            (setq i (+ i 2)))
        (setq i (1+ i))))
    hit))

;;;###autoload
(defun nlre-clear-syntax-cache ()
  "Invalidate all syntax-dependent compiled regexps; return nil.
Call after successful `modify-syntax-entry', including changes to a parent
or unrelated table: inheritance can affect other cached tables.  Repeated
calls are harmless, and non-syntax entries remain usable.  Like GNU Emacs,
direct `aset' on a syntax table does not clear this cache; callers making
such changes must invalidate explicitly.  Hits compare table identity and
this global generation without walking the table."
  (setq nlre--syntax-cache-generation (1+ nlre--syntax-cache-generation))
  nil)

(defun nlre--install-syntax-cache-hook ()
  "Connect public syntax-table mutation before caching a compiled regexp.
The library may also call `nlre-clear-syntax-cache' directly from its mutator.
Observe function-cell replacement in constant time: this bootstrap's raw
defalias does not preserve advice when a later library supplies its mutator.
Capture that callable directly instead of autoloading the nadvice substrate."
  (unless (or nlre--syntax-cache-installing
              (and nlre--syntax-cache-wrapper
                   (eq nlre--syntax-cache-wrapper
                       (symbol-function 'modify-syntax-entry))))
    (when (fboundp 'modify-syntax-entry)
      (let ((nlre--syntax-cache-installing t)
            (original (symbol-function 'modify-syntax-entry)))
        ;; Retain the provider's documentation and its argument/error checks.
        ;; ORIGINAL is a lexical capture: a later installation cannot change
        ;; the callable used by an already retained wrapper.
        (when (fboundp 'documentation)
          (put 'modify-syntax-entry 'function-documentation (documentation original t)))
        (fset 'modify-syntax-entry
              (lambda (&rest args)
                (prog1 (apply original args) (nlre-clear-syntax-cache))))
        (setq nlre--syntax-cache-original-function original
              nlre--syntax-cache-wrapper (symbol-function 'modify-syntax-entry))
        ;; A replacement could have mutated tables before this next lookup.
        (nlre-clear-syntax-cache)))))

(defun nlre--native-case-table-p (table)
  "Whether TABLE is owned by the native runtime, independent of adapters."
  (and (boundp 'nelisp-casetab--native-p)
       (funcall nelisp-casetab--native-p table)))

(defun nlre--current-translate-table ()
  "The case-fold translation of the current buffer's canon.
GNU's searches read the buffer's case_canon_table slot (search.c:405-410),
which only `set-case-table' and `set-standard-case-table' change.  Memoize
the read per buffer and `nelisp-casetab--epoch', which the case-table code
bumps on every such change and on every extra-slot write.  The memo is used
only while the epoch-aware `set-case-table' is installed, so an older case
table package that does not bump the epoch falls back to reading.
`current-buffer' is reached through `nlre--current-buffer-function' here
and in `nlre-string-match'."
  (if (and (eq (funcall nlre--current-buffer-function) nlre--translate-memo-buffer)
           (eq nelisp-casetab--epoch nlre--translate-memo-epoch)
           (eq (symbol-function 'set-case-table) nelisp-casetab--epoch-owner))
      nlre--translate-memo
    (let ((buffer (funcall nlre--current-buffer-function))
          (value (nlre--read-translate-table)))
      ;; Read the epoch after the read: preparing an invalidated table
      ;; bumps it.
      (when (and (boundp 'nelisp-casetab--epoch-owner)
                 (eq (symbol-function 'set-case-table) nelisp-casetab--epoch-owner))
        (setq nlre--translate-memo-buffer buffer
              nlre--translate-memo-epoch nelisp-casetab--epoch
              nlre--translate-memo value))
      value)))

(defun nlre--read-translate-table ()
  "Read the current canon once, retaining its live owner-specific lookup.
Nil when that canon is the pristine standard one,
`nelisp-casetab--builtin-canon': its mapping is the matcher's built-in fold
for every Unicode scalar (all 1,114,112 checked on batch D''' with the
canon scan of syntax-rt's tools/ai/syntax-rt-canon-scan.el), so the search
needs no table.  GNU likewise trusts a translate table's identity
\(search.c:220)."
  (let* ((reader (or nlre--case-table-reader
                     (and (fboundp 'current-case-table)
                          (setq nlre--case-table-reader 'current-case-table))))
         (case-table (and reader (funcall reader)))
         (public-extra (symbol-function 'char-table-extra-slot))
         ;; Installing a different public owner invalidates its accessor
         ;; selection.  Type dispatch is needed only on a new table.
         (owner (when (eq public-extra nlre--case-public-extra)
                  (and case-table (gethash case-table nlre--case-owner-cache))))
         (extra
          (if owner owner
            (when case-table
              (unless (eq public-extra nlre--case-public-extra)
                (setq nlre--case-public-extra public-extra
                      nlre--case-owner-cache (make-hash-table :test 'eq :weakness 'key)))
              (let ((accessor (if (char-table-p case-table) public-extra
                                (and (nlre--native-case-table-p case-table)
                                     nelisp-casetab--native-extra))))
                (puthash case-table accessor nlre--case-owner-cache)
                accessor))))
         ;; GNU search.c reads the buffer canon once.  NeLisp stores it in
         ;; extra slot 1; prepare an invalidated case table before reading it.
         (canon (when extra
                  (or (funcall extra case-table 1)
                      (progn (set-case-table case-table)
                             (funcall extra case-table 1))))))
    (unless (and canon (boundp 'nelisp-casetab--builtin-canon)
                 (eq canon nelisp-casetab--builtin-canon))
      (let* ((public-lookup (symbol-function 'char-table-range))
             (cached (and canon (gethash canon nlre--translate-cache))))
        (if (and cached (eq public-lookup (aref cached 4))) cached
          (let ((lookup (and canon
                             (if (char-table-p canon) public-lookup
                               (and (nlre--native-case-table-p canon)
                                    nelisp-casetab--native-range)))))
            (when lookup
              (let* ((native (and (nlre--native-case-table-p canon) canon))
                     (entry (vector canon lookup nil t public-lookup native t)))
                ;; GNU's CHAR_TABLE_REF has a direct ASCII page.  Materialize
                ;; that page once for a library representation whose public
                ;; dispatch is expensive; actual native tables retain their
                ;; live native lookup.
                (let ((ascii (make-vector 256 nil)) (character 0))
                  (while (< character 256)
                    (let ((value (or (funcall lookup canon character) character)))
                      (aset ascii character value)
                      (when (and (< character 128)
                                 (or (not (integerp value))
                                     (/= value (if (and (>= character 65) (<= character 90))
                                                   (+ character 32) character))))
                        (aset entry 3 nil)))
                    (setq character (1+ character)))
                  (unless (nlre--unibyte-fold-agrees-p canon lookup ascii)
                    (aset entry 6 nil))
                  (unless native (aset entry 2 ascii)))
                (puthash canon entry nlre--translate-cache)
                entry))))))))

(defun nlre--unibyte-fold-agrees-p (canon lookup page)
  "Non-nil when CANON translates a unibyte target as the built-in fold does.
LOOKUP reads CANON and PAGE holds its values for the codes 0..255; the
caller checks ASCII (slot 3).  For a unibyte target GNU's matcher
\(regex-emacs.c) translates a byte 128..255 in only two places: anychar
translates the character of that code and compares the result with
newline (RE_STRING_CHAR_AND_LENGTH), and bcmp_translate translates the
byte's raw-byte character (GET_CHAR_AFTER, RE_CHAR_TO_MULTIBYTE); the
exactn and charset arms leave raw bytes alone (CHAR_BYTE8_P).  So only
newline membership matters in 128..255: the library canon keeps MICRO SIGN
181 where GNU and the built-in fold give 956, and neither is newline."
  (let ((code 128) (agree t))
    (while (and agree (< code 256))
      (setq agree (eq (eql (aref page code) 10)
                      (eql (nlre--translate-char code nil) 10))
            code (1+ code)))
    (setq code #x3fff80)
    (while (and agree (<= code #x3fffff))
      (setq agree (eql (or (funcall lookup canon code) code)
                       (nlre--translate-char code nil))
            code (1+ code)))
    agree))

(defun nlre--ascii-search-string-p (text table)
  "Non-nil if matching TEXT translates only characters TABLE folds as built in.
GNU's matcher (regex-emacs.c) translates the target character by character.
A multibyte TEXT qualifies when every character is ASCII: its character and
byte counts are equal exactly then, and both are O(1).  A unibyte TEXT
needs no scan.  Its bytes 128..255 are raw bytes, which the exactn and
charset arms never translate (CHAR_BYTE8_P after RE_CHAR_TO_MULTIBYTE),
but anychar translates a byte as the character of that code and
bcmp_translate translates its raw-byte character, so TABLE must agree with
the built-in fold there (slot 6, `nlre--unibyte-fold-agrees-p')."
  (if (multibyte-string-p text)
      (= (length text) (string-bytes text))
    (aref table 6)))

(defun nlre--ascii-search-regexp-p (regexp)
  "Non-nil if REGEXP translates only ASCII characters when compiled.
regex_compile (regex-emacs.c) passes through TRANSLATE a literal
\(normal_char), each ASCII range member (SETUP_ASCII_RANGE) and each class
member below 0x80; a unibyte pattern's bytes 128..255 are raw bytes that
it stores untranslated (normal_char, SETUP_UNIBYTE_RANGE).  So only a
multibyte pattern with a non-ASCII character can compile differently
under two tables that agree on ASCII."
  (or (not (multibyte-string-p regexp))
      (= (length regexp) (string-bytes regexp))))

(defun nlre--search-translate-table (regexp text)
  "The case translation for searching TEXT with REGEXP, nil for the built-in fold.
GNU compiles and matches through the buffer's canon (search.c:405-410).
When the canon folds ASCII as built in, REGEXP translates only ASCII
\(`nlre--ascii-search-regexp-p') and TEXT presents only characters the
canon folds as built in (`nlre--ascii-search-string-p'), every TRANSLATE
the search performs equals the built-in fold, so the search runs as one
under the built-in fold and keeps the fast plans the table path cannot
use.  This holds for any owner's canon, modified or not."
  (let ((table (nlre--current-translate-table)))
    (if (and table (aref table 3) (null search-spaces-regexp)
             (nlre--ascii-search-string-p text table)
             (nlre--ascii-search-regexp-p regexp))
        nil
      table)))

(defun nlre--copy-key-value (value)
  (if (stringp value) (copy-sequence value) value))

(defun nlre--cache-entry-match-p (e pat fold spaces)
  (and (string= pat (aref e 0))
       (eq (multibyte-string-p pat) (aref e 7))
       (eq (and fold (or nlre--translate-table t)) (aref e 1))
       (or (eq spaces (aref e 2)) (equal spaces (aref e 2)))
       (or (not (aref e 3))
           (progn
             (nlre--install-syntax-cache-hook)
             (and (= nlre--syntax-cache-generation (aref e 9))
                  (eq (and nlre--syntax-table-reader
                           (funcall nlre--syntax-table-reader))
                      (aref e 4)))))))

(defun nlre--cache-find (pat fold spaces)
  (let ((xs (gethash pat nlre--compiled-cache)) hit)
    (while (and xs (not hit))
      (let ((e (car xs)))
        (if (nlre--cache-entry-match-p e pat fold spaces)
            (setq hit e)
          (setq xs (cdr xs)))))
    hit))

(defun nlre--cache-touch (e)
  (aset e 6 (setq nlre--compiled-cache-tick (1+ nlre--compiled-cache-tick)))
  (setq nlre--compiled-cache-last e))

(defun nlre--cache-evict ()
  (let* ((xs (cdr nlre--compiled-cache-lru)) (victim (car nlre--compiled-cache-lru))
         (vtick (aref victim 6)))
    (while xs
      (let ((e (car xs)))
        (when (< (aref e 6) vtick) (setq victim e vtick (aref e 6))))
      (setq xs (cdr xs)))
    (let* ((pat (aref victim 0)) (rest (delq victim (gethash pat nlre--compiled-cache))))
      (setq nlre--compiled-cache-lru (delq victim nlre--compiled-cache-lru))
      (if rest (puthash pat rest nlre--compiled-cache)
        (remhash pat nlre--compiled-cache))
      (setq nlre--compiled-cache-count (1- nlre--compiled-cache-count)))))

(defun nlre--cache-add (pat fold spaces syntax-dependent syntax-table compiled)
  ;; Key on a private copy: a caller may mutate PAT in place afterwards
  ;; (GNU recompiles on content), and a mutated key would both poison the
  ;; hash and make the `eq' head shortcut return a stale program.
  (let* ((original pat)
         (pat (copy-sequence pat))
         (e (vector pat (and fold (or nlre--translate-table t))
                   (nlre--copy-key-value spaces)
                   syntax-dependent syntax-table compiled
                   (setq nlre--compiled-cache-tick (1+ nlre--compiled-cache-tick))
                   (multibyte-string-p pat) original
                   nlre--syntax-cache-generation)))
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
  (let ((entry nlre--compiled-cache-last))
    ;; String kind cannot change in place.  Identity avoids the kind query,
    ;; while the private content snapshot still detects ASET on the caller's
    ;; original string.  Syntax validation is constant-time as well.
    (if (and entry
             (string= pat (aref entry 0))
             (or (eq pat (aref entry 8))
                 (eq (multibyte-string-p pat) (aref entry 7)))
             (eq (and case-fold-search (or nlre--translate-table t)) (aref entry 1))
             (or (eq search-spaces-regexp (aref entry 2))
                 (equal search-spaces-regexp (aref entry 2)))
             (or (not (aref entry 3))
                 (progn
                   (nlre--install-syntax-cache-hook)
                   (and (= nlre--syntax-cache-generation (aref entry 9))
                        (eq (and nlre--syntax-table-reader
                                 (funcall nlre--syntax-table-reader))
                            (aref entry 4))))))
        (progn
          (setq nlre--compiled-cache-hits (1+ nlre--compiled-cache-hits))
          (aref entry 5))
      (nlre--compiled-pattern-slow pat))))

(defun nlre--compiled-pattern-slow (pat)
  "Validate syntax-sensitive or older entries, or compile PAT on a miss."
  (let* ((fold (and case-fold-search t))
         (spaces search-spaces-regexp)
         (head nlre--compiled-cache-last)
         ;; Inline the overwhelmingly common most-recent literal entry.  The
         ;; bucket path retains the shared validator for collisions and older
         ;; entries; keeping every content check here is what makes mutation
         ;; of caller-owned PAT/SPACES safe despite this shortcut.
         (entry (if (and head
                         (string= pat (aref head 0))
                         (eq (multibyte-string-p pat) (aref head 7))
                         (eq (and fold (or nlre--translate-table t)) (aref head 1))
                         (or (eq spaces (aref head 2))
                             (equal spaces (aref head 2)))
                         (or (not (aref head 3))
                             (progn
                               (nlre--install-syntax-cache-hook)
                               (and (= nlre--syntax-cache-generation (aref head 9))
                                    (eq (and nlre--syntax-table-reader
                                             (funcall nlre--syntax-table-reader))
                                        (aref head 4))))))
                    head
                  (nlre--cache-find pat fold spaces))))
    (if entry
      (progn
          (setq nlre--compiled-cache-hits (1+ nlre--compiled-cache-hits))
          (unless (eq entry head) (nlre--cache-touch entry))
          (aref entry 5))
      (nlre--install-syntax-cache-hook)
      (setq nlre--compiled-cache-misses (1+ nlre--compiled-cache-misses))
      (let* ((syntax-dependent (and (nlre--pattern-syntax-dependent-p pat) t))
             (syntax-table (and syntax-dependent nlre--syntax-table-reader
                                (funcall nlre--syntax-table-reader)))
             (compiled (nlre--compile-pattern pat fold spaces)))
        (nlre--cache-add pat fold spaces syntax-dependent syntax-table compiled)))))

(defun nlre--parse (pat)
  "Parse PAT into a top-level node (a :seq or :alt). Sets group count."
  (setq nlre--gcount 0)
  (let ((r (nlre--parse-alt pat 0 (length pat))))
    (when (< (cdr r) (length pat))
      (signal 'invalid-regexp (list "Unmatched ) or \\)")))
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
                   ;; GNU regex-emacs.c: `*', `+' and `?' right after `^' are
                   ;; ordinary characters (nothing to repeat).
                   ((and (eq (car atom) :bol) (memq q '(?* ?+ ??))
                         (eq (aref pat (1- j)) ?^))
                    (setq nodes (cons atom nodes)))
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
        (let ((extra (- maxv minv)))
          ;; The optional tail is one loop: an empty iteration ends it.
          ;; Independent :opt nodes wrongly allow empty then nonempty
          ;; iterations, changing both greediness and the final capture.
          (when (> extra 0)
            (setq out (cons (list :bounded atom extra) out)))))
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
      (when (= (1+ i) n)
        (signal 'invalid-regexp (list "Trailing backslash")))
      (let ((d (aref pat (1+ i))))
        (cond
         ((eq d ?\() ;; plain, shy, or explicitly numbered group
          (let ((k (+ i 2)) (shy nil) (explicit nil))
            (when (and (< k n) (eq (aref pat k) ?\?))
              (let ((m (1+ k)) (num nil))
                ;; GNU regex-emacs.c:2225-2229 rejects a leading zero,
                ;; including explicitly numbered group 0 and 01.
                (when (and (< m n) (eq (aref pat m) ?0))
                  (signal 'invalid-regexp (list "Invalid regular expression")))
                (while (and (< m n) (>= (aref pat m) ?0) (<= (aref pat m) ?9))
                  (setq num (+ (* (or num 0) 10) (- (aref pat m) ?0)))
                  ;; GNU's regnum_t is int; checked arithmetic rejects
                  ;; overflow here (regex-emacs.c:2233-2235).
                  (when (> num #x7fffffff)
                    (signal 'invalid-regexp (list "Regular expression too big")))
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
                (unless (and (< (1+ j) n) (eq (aref pat j) ?\\)
                             (eq (aref pat (1+ j)) ?\)))
                  (signal 'invalid-regexp (list "Unmatched ( or \\(")))
                (setq j (+ j 2))
                ;; GNU's one-byte capture opcodes omit registers above
                ;; 255, while their logical numbers still affect later groups.
                (cons (if (or shy (> gn 255)) inner (list :group gn inner)) j)))))
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

(defun nlre--posix-ranges (_name)
  "Return a list of (lo . hi) ranges for POSIX class NAME, nil if unknown."
  (cond
   (t nil)))

(defun nlre--set-kind-ranges (ranges multibyte)
  "Retain raw-byte identity when a set came from a unibyte pattern."
  (if multibyte ranges
    (let (out)
      (dolist (range ranges)
        (let ((lo (car range)) (hi (cdr range)))
          (if (< hi 128) (setq out (cons range out))
            (when (< lo 128)
              (setq out (cons (cons lo 127) out) lo 128))
            (setq out (cons (cons (+ #x3fff00 lo) (+ #x3fff00 hi)) out)))))
      (nreverse out))))

(defun nlre--parse-set (pat i n)
  "Parse a char class body (after the opening [) ; return (NODE . newpos)."
  (let ((neg nil) (ranges nil) (classes nil))
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
              (let ((name (substring pat (+ i 2) j)))
                (setq ranges (append (nlre--posix-ranges name) ranges))
                (when (member name '("digit" "xdigit" "ascii" "nonascii" "multibyte" "unibyte"
                                     "alpha" "alnum" "word" "upper" "lower"
                                     "space" "blank" "punct" "cntrl" "graph" "print"))
                  (setq classes (cons name classes))))
              (setq i (+ j 2))))
           ((and (< (+ i 2) n) (eq (aref pat (1+ i)) ?-) (not (eq (aref pat (+ i 2)) ?\])))
            (setq ranges (cons (cons c (aref pat (+ i 2))) ranges) i (+ i 3)))
           (t (setq ranges (cons (cons c c) ranges) i (1+ i))))))
      (when cont
        (signal 'invalid-regexp (list "Unmatched [ or [^"))))
    (let ((ascii t) (rs ranges))
      (while rs
        (when (>= (cdar rs) 128) (setq ascii nil))
        (setq rs (cdr rs)))
      (setq ranges (nreverse ranges) classes (nreverse classes))
      (cons (list :set neg (nlre--set-kind-ranges ranges
                                                 (multibyte-string-p pat))
                  classes ascii
                  (and ascii (nlre--ascii-set-plan ranges classes))) i))))

(defun nlre--ascii-set-plan (ranges classes)
  "Build default-fold ASCII bitmaps only for immutable ASCII membership.
Raw Unicode ranges and syntax/case-dependent POSIX classes use the live
matcher. These plans are never used with an installed canonical table."
  (let ((all ranges) (names classes) (known t))
    (while (and names known)
      (let ((name (car names)))
        (setq all
              (append
               (cond
                ((equal name "alpha") '((65 . 90) (97 . 122)))
                ((equal name "alnum") '((48 . 57) (65 . 90) (97 . 122)))
                ((equal name "digit") '((48 . 57)))
                ((equal name "xdigit") '((48 . 57) (65 . 70) (97 . 102)))
                ((equal name "ascii") '((0 . 127)))
                ((equal name "blank") '((9 . 9) (32 . 32)))
                ((equal name "cntrl") '((0 . 31)))
                ((equal name "graph") '((33 . 126)))
                ((equal name "print") '((32 . 126)))
                (t (setq known nil) nil))
               all)))
      (setq names (cdr names)))
    (when known
      (let ((plain (make-string 128 0)) (folded (make-string 128 0)))
        (dolist (range all)
          (let ((c (car range)))
            (while (<= c (cdr range))
              (aset plain c 1)
              (aset folded (if (and (>= c ?A) (<= c ?Z)) (+ c 32) c) 1)
              (setq c (1+ c)))))
        (vector plain folded)))))

;; A fast plan is either a literal, or a finite language whose every variant
;; ends at `\\''.  The latter covers load-history and regexp-opt file names
;; without carrying the former general-purpose finite matcher in the image.
(defvar nlre--plan-limit 256)
(defvar nlre--plan-count 0)
(defvar nlre--plan-failed nil)
(defvar nlre--pattern-multibyte t)

(defun nlre--ascii-text-p (text)
  (let ((i 0) (n (length text)) (ok t))
    (while (and ok (< i n))
      (when (>= (aref text i) 128) (setq ok nil))
      (setq i (1+ i)))
    ok))

(defun nlre--ascii-fold-text (text)
  (let ((i 0) (n (length text)) (out ""))
    (while (< i n)
      (let ((c (aref text i)))
        (setq out (concat out (char-to-string
                               (if (and (>= c ?A) (<= c ?Z)) (+ c 32) c)))))
      (setq i (1+ i)))
    out))
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
             (single (or (eq tag :lit)
                         (and (eq tag :set) (not (nth 1 (car nodes)))
                              (null (nth 3 (car nodes)))))))
        (if single
            (let ((atoms nil))
              (while (and nodes
                          (let ((tag (car (car nodes))))
                            (or (eq tag :lit)
                           (and (eq tag :set) (not (nth 1 (car nodes)))
                                     (null (nth 3 (car nodes)))))))
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
      (if (or (nth 1 node) (nth 3 node))
          (progn (setq nlre--plan-failed t) nil)
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
        (setq out (concat out (char-to-string
                               (nlre--translate-char
                                (aref s i) nlre--translate-table))))
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

(defun nlre--suffix-bucket-add (table entry)
  (let* ((text (car entry)) (len (length text))
         (last (aref text (1- len)))
         (key (if (= len 1) last
                (+ last (* 1114112 (1+ (aref text (- len 2))))))))
    (puthash key (nlre--suffix-entry-add entry (gethash key table)) table)))

(defun nlre--literal-text (ast)
  (and (eq (car ast) :seq)
       (let ((nodes (nth 1 ast)) (out "") (ok t))
         (while (and nodes ok)
           (if (eq (car (car nodes)) :lit)
               (setq out (concat out (char-to-string (nth 1 (car nodes))))
                     nodes (cdr nodes))
             (setq ok nil)))
         (and ok out))))

(defun nlre--pattern-kind-text (text)
  "Return TEXT with the byte/multibyte kind of the current pattern."
  (if nlre--pattern-multibyte
      ;; ASCII fragments otherwise lose the source's multibyte flag.
      (if (multibyte-string-p text) text (string-to-multibyte text))
    ;; Converting TEXT directly would encode U+00E9 as two bytes instead of
    ;; recovering raw byte 233, so append every parsed character as one byte.
    (let ((i 0) (n (length text)) (out (unibyte-string)))
      (while (< i n)
        (let ((c (aref text i)))
          (setq out (concat out (unibyte-string
                                (if (>= c #x3fff80) (- c #x3fff00) c)))))
        (setq i (1+ i)))
      out)))

(defun nlre--suffix-plan-build (variants fold tag &optional greedy-group)
  (when (and variants (not nlre--plan-failed))
    (let ((ends (make-hash-table :test 'equal)) empty
          (ascii-ends (make-hash-table :test 'equal)) ascii-empty
          (ascii-only t) contains-set all (xs (reverse variants))
          (simple-caps (and (eq tag :greedy-suffix) (eq greedy-group 1))))
      (while (and xs (not nlre--plan-failed))
        (let ((v (car xs))
              (strings (nlre--atom-strings (reverse (aref (car xs) 0)))))
          (dolist (atom (aref v 0))
            (when (and (consp atom) (eq (car atom) :set))
              (setq contains-set t)))
          (while strings
            (let* ((raw (nlre--pattern-kind-text (car strings)))
                   (ascii (nlre--ascii-fold-text raw))
                   (len (length raw)) (entry (cons raw v))
                   (ascii-entry (cons ascii v)))
              (let* ((caps (aref v 1))
                     (span (and (= (length caps) 3) (aref caps 2))))
                (unless (and span (null (aref caps 1))
                             (= (car span) 0) (= (cdr span) len))
                  (setq simple-caps nil)))
              (unless (nlre--ascii-text-p raw) (setq ascii-only nil))
              (setq all (cons entry all))
              (if (= len 0)
                  (setq empty (cons entry empty)
                        ascii-empty (cons ascii-entry ascii-empty))
                (nlre--suffix-bucket-add ends entry)
                (nlre--suffix-bucket-add ascii-ends ascii-entry))
              (setq strings (cdr strings)))))
        (setq xs (cdr xs)))
      (and (not nlre--plan-failed)
           (vector tag ends fold empty greedy-group all
                   ascii-ends ascii-empty ascii-only
                   nlre--pattern-multibyte contains-set simple-caps)))))

(defun nlre--suffix-plan (ast ng fold)
  (let ((nodes (and (eq (car ast) :seq) (nth 1 ast))))
    (when (and nodes (eq (car (car (last nodes))) :eos))
      (let ((nlre--plan-count 0) (nlre--plan-failed nil))
        (nlre--suffix-plan-build
         (nlre--plan-seq (butlast nodes) ng) fold :suffix)))))

(defun nlre--greedy-suffix-plan (ast ng fold)
  "Plan exactly \\=` GROUP(.*) FINITE-SUFFIX \\=' without backtracking."
  (let ((nodes (and (eq (car ast) :seq) (nth 1 ast))))
    (when (and (= (length nodes) 4)
               (eq (car (nth 0 nodes)) :bos)
               (eq (car (nth 1 nodes)) :group)
               (eq (car (nth 3 nodes)) :eos))
      (let* ((greedy (nth 1 nodes))
             (body (nth 2 greedy))
             (body-nodes (and (eq (car body) :seq) (nth 1 body)))
             (star (and (= (length body-nodes) 1) (car body-nodes))))
        (when (and star (eq (car star) :star)
                   (eq (car (nth 1 star)) :any))
          (let ((nlre--plan-count 0) (nlre--plan-failed nil))
            (nlre--suffix-plan-build
             (nlre--plan-seq (list (nth 2 nodes)) ng)
             fold :greedy-suffix (nth 1 greedy))))))))

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

;; ---- prefilter: cheap rejections before the backtracker ----
;;
;; Every backtracker step costs tens of microseconds in the interpreter, so a
;; pattern that cannot match must be rejected before it runs wherever that is
;; cheap to prove, as GNU regex.c does with its anchoring and fastmap.  Tramp's
;; `file-name-handler-alist' regexps start with `\(?:\`/\)' and are tried on
;; every file name: they were run at every start position, 11-15 ms per call.
;; The prefilter is (ANCHORED PREFIX REQUIRED):
;; - ANCHORED: every match starts with \`, so only position 0 can match;
;; - PREFIX: literal characters an anchored match must start with;
;; - REQUIRED: characters every match must contain somewhere.
;; Literals are kept only when case folding cannot change them.

(defun nlre--node-anchored-p (node)
  "Non-nil when every match of NODE must begin with \\=`."
  (let ((tag (car node)))
    (cond ((eq tag :bos) t)
          ((eq tag :seq) (nlre--nodes-anchored-p (nth 1 node)))
          ((eq tag :group) (nlre--node-anchored-p (nth 2 node)))
          ((eq tag :alt)
           (let ((branches (nth 1 node)) (ok (consp (nth 1 node))))
             (while (and ok branches)
               (unless (nlre--nodes-anchored-p (car branches)) (setq ok nil))
               (setq branches (cdr branches)))
             ok))
          (t nil))))

(defun nlre--nodes-anchored-p (nodes)
  "Non-nil when every match of the node list NODES must begin with \\=`."
  (and (consp nodes) (nlre--node-anchored-p (car nodes))))

(defun nlre--fold-stable-p (c fold)
  "Non-nil when character C matches only itself under FOLD.
Folding, that is an ASCII non-letter.  Above ASCII the case tables
decide, and a character can be the fold of another without folding to
it: C's own upcase and downcase being C proves nothing about the
characters that downcase to C."
  (or (not fold)
      (and (eq fold :portable) (< c 128)
           (not (and (>= c ?a) (<= c ?z)))
           (not (and (>= c ?A) (<= c ?Z))))))

(defun nlre--anchored-prefix (nodes fold)
  "Split an anchored NODES into (PREFIX-CHARS . REST-NODES).
PREFIX-CHARS are the literal characters every match starts with; leading
groups whose body is a sequence are opened, and collection stops at the
first node that is not \\=` or a fold-stable literal.  REST-NODES are the
nodes that consume everything after the prefix."
  (let ((out nil) (todo nodes) (done nil))
    (while (and todo (not done))
      (let* ((nd (car todo)) (tag (car nd)))
        (cond ((eq tag :bos) (setq todo (cdr todo)))
              ((and (eq tag :lit) (nlre--fold-stable-p (nth 1 nd) fold))
               (setq out (cons (nth 1 nd) out) todo (cdr todo)))
              ((eq tag :seq) (setq todo (append (nth 1 nd) (cdr todo))))
              ((and (eq tag :group) (eq (car (nth 2 nd)) :seq))
               (setq todo (append (nth 1 (nth 2 nd)) (cdr todo))))
              (t (setq done t)))))
    (cons (nreverse out) todo)))

(defun nlre--node-ends-eos-p (node)
  "Non-nil when every match of NODE ends with \\='."
  (let ((tag (car node)))
    (cond ((eq tag :eos) t)
          ((eq tag :seq) (nlre--nodes-end-eos-p (nth 1 node)))
          ((eq tag :group) (nlre--node-ends-eos-p (nth 2 node)))
          ((eq tag :alt)
           (let ((branches (nth 1 node)) (ok (consp (nth 1 node))))
             (while (and ok branches)
               (unless (nlre--nodes-end-eos-p (car branches)) (setq ok nil))
               (setq branches (cdr branches)))
             ok))
          (t nil))))

(defun nlre--nodes-end-eos-p (nodes)
  (and (consp nodes) (nlre--node-ends-eos-p (car (last nodes)))))

;; Prefilter probes cost one `string-search' each.  A loop over the
;; subject's characters cost the byte-code VM about 170 us a character, ten
;; times the backtracker's work it was meant to save on tramp's file names.
(defconst nlre--probe-chars "/ .\n-_:\t"
  "Characters tested as prefilter probes, the ones most common in file
names and text first.")

(defun nlre--atoms (node)
  "The atoms of NODE that consume a character, or t when NODE holds a back
reference, whose text no atom describes, or a node this does not know."
  (let ((tag (car node)))
    (cond
     ((memq tag '(:bos :eos :bol :eol :wordb :wordedge :symedge)) nil)
     ((memq tag '(:lit :any :set :word :space :syntax)) (list node))
     ((memq tag '(:seq :alt :group :star :plus :opt :lazystar :lazyplus :lazyopt :bounded))
      (let ((children (cond ((eq tag :seq) (nth 1 node))
                            ((eq tag :alt) (apply #'append (nth 1 node)))
                            ((eq tag :group) (list (nth 2 node)))
                            (t (list (nth 1 node)))))
            (out nil))
        (while (and children (not (eq out t)))
          (let ((sub (nlre--atoms (car children))))
            (setq out (if (eq sub t) t (append sub out))))
          (setq children (cdr children)))
        out))
     (t t))))

(defun nlre--forbidden-probes (nodes fold)
  "Up to three of `nlre--probe-chars' that no atom of NODES consumes, as
one-character strings; nil when NODES' atoms are not all known.  Each
atom is tested with the matcher's own `nlre--match-atom1', so case
folding and dynamic syntax predicates are excluded.  :portable FOLD permits
ASCII punctuation proofs for the default fold, with live POSIX class guards.
The caller must disable those proofs whenever a canonical table is installed."
  (let ((atoms nil) (unknown (and fold (not (eq fold :portable)))) classes)
    (dolist (x nodes)
      (let ((a (nlre--atoms x)))
        (if (eq a t) (setq unknown t)
          (dolist (atom a)
            (unless (or (memq (car atom) '(:lit :any))
                        (and (eq (car atom) :set)
                             (or (eq fold :portable) (null (nth 3 atom)))))
              (setq unknown t))
            (when (eq (car atom) :set)
              (dolist (name (nth 3 atom))
                ;; GNU's ASCII class bitmaps are fixed for these names.
                ;; Syntax/case-dependent classes still need a live guard.
                (unless (or (member name '("alpha" "alnum" "digit" "xdigit"
                                           "ascii" "blank" "cntrl" "graph" "print"))
                            (member name classes))
                  (setq classes (cons name classes))))))
          (setq atoms (append a atoms)))))
    (unless unknown
      (let ((nlre--fold nil) (nlre--match-end-limit nil)
            (probes nil) (i 0) (n (length nlre--probe-chars)))
        (while (and (< i n) (< (length probes) 3))
          (let ((s (char-to-string (aref nlre--probe-chars i))) (as atoms) (ok nil))
            (while (and as (not ok))
              (when (nlre--match-atom1 (car as) s 0 1) (setq ok t))
              (setq as (cdr as)))
            (unless ok
              (setq probes
                    (cons (if (and (eq fold :portable) classes)
                              (cons s (mapcar
                                       (lambda (name)
                                         (cons name (and (nlre--posix-class-p
                                                         name (aref s 0)) t)))
                                       classes))
                            s)
                          probes))))
          (setq i (1+ i)))
        (nreverse probes)))))

(defun nlre--char-intersection (a b)
  (let ((out nil))
    (dolist (c a) (when (memq c b) (setq out (cons c out))))
    out))

(defun nlre--required-chars (node fold)
  "Fold-stable characters every match of NODE contains, as a list."
  (let ((tag (car node)))
    (cond
     ((eq tag :lit)
      (and (nlre--fold-stable-p (nth 1 node) fold) (list (nth 1 node))))
     ((eq tag :seq)
      (let ((acc nil))
        (dolist (x (nth 1 node))
          (dolist (c (nlre--required-chars x fold))
            (unless (memq c acc) (setq acc (cons c acc)))))
        acc))
     ((eq tag :alt)
      (let ((branches (nth 1 node)) (acc nil) (first t))
        (while branches
          (let ((r (nlre--required-chars (list :seq (car branches)) fold)))
            (setq acc (if first r (nlre--char-intersection acc r)) first nil))
          (setq branches (if acc (cdr branches) nil)))
        acc))
     ((eq tag :group) (nlre--required-chars (nth 2 node) fold))
     ((memq tag '(:plus :lazyplus)) (nlre--required-chars (nth 1 node) fold))
     (t nil))))

(defun nlre--prefilter (ast fold)
  "Return (ANCHORED PREFIX REQUIRED PROBES) for AST, or nil when none applies.
PROBES is set only for patterns anchored at both ends (\\=` ... \\='): a
match then spans the whole string, so any character after PREFIX that no
remaining atom consumes rules the string out.  PROBES lists up to three
such characters (`nlre--forbidden-probes')."
  (let* ((nodes (nlre--seq-nodes ast))
         (anchored (nlre--nodes-anchored-p nodes))
         (split (and anchored (nlre--anchored-prefix nodes fold)))
         (prefix (car split))
         (probes (and anchored (nlre--nodes-end-eos-p nodes)
                      (nlre--forbidden-probes (cdr split) fold)))
         (required (nlre--required-chars ast fold)))
    (dolist (c prefix) (setq required (delq c required)))
    (and (or anchored required)
         (list anchored (and prefix (apply #'string prefix))
               (mapcar #'char-to-string required)
               probes))))

(defun nlre--prefilter-reject-p (pre string i n)
  "Non-nil when prefilter PRE proves no match of STRING starts at or after I."
  (or (and (car pre)
           (or (> i 0)
               (let* ((prefix (nth 1 pre)) (k (length prefix)) (bad (> k n)))
                 (while (and (not bad) (> k 0))
                   (setq k (1- k))
                   (unless (eq (aref prefix k) (aref string k)) (setq bad t)))
                 bad)))
      (let ((required (nth 2 pre)) (miss nil))
        (while (and required (not miss))
          (unless (string-search (car required) string i) (setq miss t))
          (setq required (cdr required)))
        miss)
      ;; Only a pattern anchored at both ends has probes, and it reaches
      ;; here with I = 0 and its prefix matched.
      (let ((probes (nth 3 pre)) (k (length (nth 1 pre))) (hit nil))
        (while (and probes (not hit))
          (let* ((probe (car probes))
                 (text (if (consp probe) (car probe) probe)))
            (when (string-search text string k)
              (let ((guards (and (consp probe) (cdr probe))) (valid t))
                (while (and guards valid)
                  (unless (eq (cdar guards)
                              (and (nlre--posix-class-p (caar guards) (aref text 0)) t))
                    (setq valid nil))
                  (setq guards (cdr guards)))
                (setq hit valid))))
          (setq probes (cdr probes)))
        hit)))

(defun nlre--regexp-storage-bytes (text)
  "Return GNU's internal bytes of TEXT, including raw-byte escapes."
  (if (not (multibyte-string-p text)) text
    (let ((i 0) (n (length text)) pieces)
      (while (< i n)
        (let ((c (aref text i)))
          (setq pieces
                (cons (if (>= c #x3fff80)
                          (unibyte-string (+ #xc0 (ash (- c #x3fff80) -6))
                                          (+ #x80 (logand c 63)))
                        (with-suppressed-warnings ((obsolete string-as-unibyte))
                          (string-as-unibyte (char-to-string c))))
                      pieces)))
        (setq i (1+ i)))
      (apply #'concat (nreverse pieces)))))

(defun nlre--expand-search-spaces (pat spaces)
  "Substitute SPACES for maximal literal-space runs in PAT."
  (if (not (stringp spaces)) pat
    ;; GNU compiles the inserted bytes with PAT's multibyte flag.  A
    ;; Unicode SPACES string in a unibyte pattern therefore contributes
    ;; its UTF-8 bytes, rather than a decoded Unicode character.
    (when (not (multibyte-string-p pat))
      (setq spaces (nlre--regexp-storage-bytes spaces)))
    (let ((i 0) (n (length pat)) (out "") (in-class nil) (in-repeat nil))
      (while (< i n)
        (let ((c (aref pat i)))
          (cond
           ((and (eq c ?\\) (< (1+ i) n))
            (let ((next (aref pat (1+ i))))
              (setq out (concat out (substring pat i (+ i 2))))
              (cond ((and (not in-class) (eq next ?{)) (setq in-repeat t))
                    ((and in-repeat (eq next ?})) (setq in-repeat nil)))
              (setq i (+ i 2))))
           ((and (not in-repeat) (eq c ?\[))
            (setq in-class t out (concat out "[") i (1+ i)))
           ((and in-class (eq c ?\]))
            (setq in-class nil out (concat out "]") i (1+ i)))
           ((and (not in-class) (not in-repeat) (eq c 32))
            (let ((begin i))
              (while (and (< i n) (eq (aref pat i) 32)) (setq i (1+ i)))
              (setq out
                    (concat out
                            (if (and (< i n)
                                     (or (memq (aref pat i) '(?* ?+ ??))
                                         (and (eq (aref pat i) ?\\) (< (1+ i) n)
                                              (eq (aref pat (1+ i)) ?{))))
                                (substring pat begin i)
                              spaces)))))
           (t
            (setq out (concat out (substring pat i (1+ i))) i (1+ i))))))
      out)))

(defun nlre--quick-reject-plan (pre)
  "Pick a sufficient portable rejection witness from PRE.
Required characters need no prefix test. An unguarded forbidden character
after the fixed prefix is impossible in any complete anchored match."
  (if (nth 2 pre)
      (vector :missing (car (nth 2 pre)))
    (let ((probes (nth 3 pre)) result)
      (while (and probes (not result))
        (when (stringp (car probes))
          (setq result (vector :forbidden (car probes) (length (nth 1 pre)))))
        (setq probes (cdr probes)))
      result)))

(defun nlre--syntax-run-classes (node)
  "Return NODE's positive one-character syntax alternatives, or nil.
Only syntax designators are retained; no syntax-table contents are read."
  (cond
   ((and (eq (car node) :syntax) (not (nth 2 node)))
    (list (if (eq (nth 1 node) ?-) 32 (nth 1 node))))
   ((eq (car node) :seq)
    (and (= (length (nth 1 node)) 1)
         (nlre--syntax-run-classes (car (nth 1 node)))))
   ((eq (car node) :alt)
    (let ((branches (nth 1 node)) (valid t) classes)
      (while (and branches valid)
        (let ((part (nlre--syntax-run-classes (list :seq (car branches)))))
          (if part (setq classes (append part classes)) (setq valid nil)))
        (setq branches (cdr branches)))
      (and valid classes)))))

(defun nlre--syntax-run-plan (ast)
  "Plan a whole-string greedy syntax run with at most one capture.
Every alternative consumes one character and has no internal captures, so
branch choice cannot affect a later match or the last repeated capture."
  (let ((nodes (and (eq (car ast) :seq) (nth 1 ast))))
    (when (and (= (length nodes) 3)
               (eq (car (nth 0 nodes)) :bos)
               (memq (car (nth 1 nodes)) '(:star :plus))
               (eq (car (nth 2 nodes)) :eos))
      (let* ((repeat (nth 1 nodes))
             (body (nth 1 repeat))
             (group-p (eq (car body) :group))
             (group (and group-p (nth 1 body)))
             (classes (and (or (not group-p) (and (integerp group) (> group 0)))
                           (nlre--syntax-run-classes
                            (if group-p (nth 2 body) body)))))
        (and classes (vector :syntax-run classes group
                             (if (eq (car repeat) :plus) 1 0)))))))

;; GNU's fastmap skips start positions that cannot begin a match.  This
;; returns a 128-slot vector (1 = a match may start with that ASCII character)
;; for the default case fold or no fold, or nil when the first character of a
;; match is not fully described by literals, ASCII sets and their
;; alternations (non-ASCII literals, nullable or anchored starts, syntax
;; classes, backreferences).  Subject characters >= 128 are never skipped.
(defun nlre--first-map-fill (node v fold)
  "Set V for every ASCII start of NODE; return nil if NODE is not handled."
  (let ((tag (car node)))
    (cond
     ((eq tag :lit)
      (let ((c (nth 1 node)))
        (and (integerp c) (< c 128)
             (progn
               (aset v c 1)
               (when (and fold (>= c ?A) (<= c ?Z)) (aset v (+ c 32) 1))
               (when (and fold (>= c ?a) (<= c ?z)) (aset v (- c 32) 1))
               t))))
     ((eq tag :set)
      (let ((bm (nth 5 node)) (neg (nth 1 node)) (c 0) (ok t))
        (when bm
          (while (< c 128)
            (let* ((l (if (and fold (>= c ?A) (<= c ?Z)) (+ c 32) c))
                   (m (eq (aref (aref bm (if fold 1 0)) l) 1)))
              (when (if neg (not m) m) (aset v c 1)))
            (setq c (1+ c))))
        (and bm ok)))
     ((eq tag :group) (nlre--first-map-fill (nth 2 node) v fold))
     ((memq tag '(:plus :lazyplus)) (nlre--first-map-fill (nth 1 node) v fold))
     ((eq tag :seq)
      (let ((children (nth 1 node)))
        (and children (nlre--first-map-fill (car children) v fold))))
     ((eq tag :alt)
      (let ((branches (nth 1 node)) (ok (nth 1 node)))
        (while (and branches ok)
          (setq ok (and (car branches)
                        (nlre--first-map-fill (list :seq (car branches)) v fold)))
          (setq branches (cdr branches)))
        ok))
     (t nil))))

(defun nlre--first-map (ast fold)
  (let ((v (make-vector 128 0)))
    (and (nlre--first-map-fill ast v fold) v)))

;; A pattern that is one ASCII character set repeated (`[0-9]+', `[a-z]*', no
;; captures) is a run scan: find the first member and extend while members
;; follow.  Membership for ASCII characters comes from a 128-slot vector built
;; with the default fold, so it applies only while no case table replaces the
;; fold; other characters ask the ordinary set matcher.
(defun nlre--setrun-plan (ast fold)
  (let* ((nodes (and (eq (car ast) :seq) (nth 1 ast)))
         (rep (and nodes (null (cdr nodes)) (car nodes)))
         (set (and rep (memq (car rep) '(:plus :star)) (nth 1 rep)))
         (bm (and set (eq (car set) :set) (nth 5 set))))
    (when bm
      (let ((v (make-vector 128 0)) (c 0) (neg (nth 1 set)))
        (while (< c 128)
          (let* ((l (if (and fold (>= c ?A) (<= c ?Z)) (+ c 32) c))
                 (m (eq (aref (aref bm (if fold 1 0)) l) 1)))
            (when (if neg (not m) m) (aset v c 1)))
          (setq c (1+ c)))
        (vector :setrun v set (if (eq (car rep) :plus) 1 0))))))

(defun nlre--setrun-match (plan string start n)
  "Return (START . END) of the first run of PLAN's set at or after START."
  (let ((v (aref plan 1)) (node (aref plan 2))
        (unibyte (not (multibyte-string-p string)))
        (i start) (j nil))
    (when (= (aref plan 3) 1)
      (while (and (< i n)
                  (let ((c (aref string i)))
                    (not (if (< c 128) (eq (aref v c) 1)
                           (nlre--set-node-match node c unibyte)))))
        (setq i (1+ i))))
    (when (<= i n)
      (setq j i)
      (while (and (< j n)
                  (let ((c (aref string j)))
                    (if (< c 128) (eq (aref v c) 1)
                      (nlre--set-node-match node c unibyte))))
        (setq j (1+ j)))
      (and (or (= (aref plan 3) 0) (> j i)) (cons i j)))))

(defun nlre--setrun-at (plan string pos limit)
  "Return the end of PLAN's set run starting exactly at POS, or nil."
  (let ((v (aref plan 1)) (node (aref plan 2))
        (unibyte (not (multibyte-string-p string)))
        (j pos))
    (when (<= pos limit)
      (while (and (< j limit)
                  (let ((c (aref string j)))
                    (if (< c 128) (eq (aref v c) 1)
                      (nlre--set-node-match node c unibyte))))
        (setq j (1+ j)))
      (and (or (= (aref plan 3) 0) (> j pos)) j))))

(defun nlre--compile-pattern (pat fold &optional spaces)
  (let* ((nlre--syntax-table nil)
         (nlre--pattern-multibyte (multibyte-string-p pat))
         (nlre--fold fold)
         (source (nlre--expand-search-spaces pat spaces))
         (ast (nlre--parse source)) (ng (1+ (min nlre--gcount 255)))
         (literal (nlre--literal-text ast))
         (literal (and literal (nlre--pattern-kind-text literal)))
         (atom (and (not literal) (nlre--single-atom-node ast)))
         (prefix (and (not literal) (nlre--leading-literal-text
                                     (nlre--seq-nodes ast))))
         (prefix (and prefix (nlre--pattern-kind-text prefix)))
         (plan (cond
             (literal (vector :literal literal fold
                              (nlre--ascii-fold-text literal)
                              (nlre--ascii-text-p literal)))
             (atom (vector :atom atom))
             (t (or (nlre--syntax-run-plan ast)
                    (nlre--setrun-plan ast fold)
                    (nlre--greedy-suffix-plan ast ng fold)
                    (nlre--suffix-plan ast ng fold)))))
            ;; The prefilter's character strings use Unicode, so they cannot
            ;; prove absence of non-ASCII bytes in a unibyte regexp.  Retain
            ;; the ordinary engine (and byte-aware leading prefix) there.
         (pre (and (not literal) (not atom)
                 (or nlre--pattern-multibyte (nlre--ascii-text-p source))
                 (nlre--prefilter ast fold)))
            ;; Unlike the table-independent filter above, these ASCII
            ;; punctuation proofs require the portable default fold.  Keep
            ;; both plans: a cached regexp may see a new table on any call.
         (portable (and fold (not literal) (not atom)
                 (or nlre--pattern-multibyte (nlre--ascii-text-p source))
                 (let ((nlre--translate-table nil))
                   (nlre--prefilter ast :portable)))))
    (vector ast ng plan pre
            (and prefix (vector :literal prefix fold
                                (nlre--ascii-fold-text prefix)
                                (nlre--ascii-text-p prefix)))
            nlre--pattern-multibyte portable
            (and (not plan) (nlre--quick-reject-plan (or portable pre)))
            ;; Slot 8: ASCII first-character map for the generic scan.
            (and (not plan) (nlre--first-map ast fold)))))

;; ---- matcher (no closures; rest threaded explicitly) ----

(defvar nlre--caps nil "Vector of (start . end) per group during a match.")

;; Fold only comparison operands; rewriting the regexp would invert \W/\B.

(defun nlre--downcase-char (c)
  "Non-ASCII case fold of C.  A separate function so the hot callers below
keep only allowlisted bytecode opcodes (the inline `downcase' opcode is not
one, which would demote them to the interpreter)."
  (downcase c))

(defun nlre--upcase-char (c)
  "Non-ASCII upcase of C; see `nlre--downcase-char'."
  (upcase c))

(defun nlre--table-char (c table)
  "Read C through TABLE's cached ASCII page or live owner lookup."
  (if (and (< c 256) (aref table 2))
      (aref (aref table 2) c)
    (or (funcall (aref table 1) (aref table 0) c) c)))

(defun nlre--translate-char (c table)
  "Canonicalize C with TABLE, falling back to ordinary case folding."
  (if (and table (not (and (< c 128) (aref table 3))))
      (nlre--table-char c table)
    (if (< c 128)
        (if (and (>= c ?A) (<= c ?Z)) (+ c 32) c)
      (if (fboundp 'nelisp-unicode-canonical-character)
          (nelisp-unicode-canonical-character c)
        (nlre--downcase-char (nlre--upcase-char c))))))

(defun nlre--fold-char (c)
  (if nlre--fold
      (let ((native (and nlre--translate-table (aref nlre--translate-table 5))))
        (if native
            ;; GNU RE_TRANSLATE = char_table_translate (character.h:592).
            (let ((v (aref native c)))
              (if (and (integerp v) (>= v 0)) v c))
          (nlre--translate-char c nlre--translate-table)))
    c))

(defun nlre--character-byte (c)
  "Return C's byte, or nil for a Unicode character without a byte."
  (cond ((< c 128) c)
        ((and (>= c #x3fff80) (<= c #x3fffff)) (- c #x3fff00))))

(defun nlre--literal-char-equal-p (pattern pmulti subject smulti)
  "Compare one literal with GNU's pattern and subject byte-kind rules."
  (if (and (< pattern 128) (< subject 128)
           (or (null nlre--translate-table) (aref nlre--translate-table 3)))
      ;; Byte-kind conversion cannot change ASCII. Avoid the general byte
      ;; translation helpers on the portable path's many failed literals.
      (= (if (and nlre--fold (>= pattern ?A) (<= pattern ?Z)) (+ pattern 32) pattern)
         (if (and nlre--fold (>= subject ?A) (<= subject ?Z)) (+ subject 32) subject))
    (let* ((pc (if pmulti (nlre--fold-char pattern)
               (if (< pattern 128)
                   (or (nlre--character-byte (nlre--fold-char pattern)) pattern)
                 pattern)))
         (sc (if smulti (nlre--fold-char subject)
               (if (< subject 128)
                   (or (nlre--character-byte (nlre--fold-char subject)) subject)
                 subject))))
    (if smulti
        (= sc (if pmulti pc (if (< pc 128) pc (+ #x3fff00 pc))))
      (equal sc (if pmulti (nlre--character-byte pc) pc))))))

(defun nlre--flip-case (c)
  (cond ((and (>= c ?a) (<= c ?z)) (- c 32))
        ((and (>= c ?A) (<= c ?Z)) (+ c 32))
        ((< c 128) c)
        (t (let ((d (nlre--downcase-char c))) (if (eq d c) (nlre--upcase-char c) d)))))

(defun nlre--space-p (c) (or (= c 32) (= c 9) (= c 10) (= c 13) (= c 12)))

(defun nlre--provider-char-syntax (c)
  "The provider's `char-syntax' for a table that is not a char-table.
A separate function: GNU compiles `char-syntax' to opcode 122, which the
standalone VM does not admit, and the predicates below stay byte-code."
  (char-syntax c))

(defun nlre--setup-syntax-table ()
  "Read the current syntax table into `nlre--syntax-table' and return it."
  (setq nlre--syntax-table
        (if (not (fboundp 'char-syntax))
            'none
          (let ((table (and nlre--syntax-table-reader
                            (funcall nlre--syntax-table-reader))))
            (if (if (fboundp 'nelisp--syntax-table-object-p)
                    (nelisp--syntax-table-object-p table)
                  (char-table-p table))
                table
              t)))))

;; Each predicate reads the class code from the raw descriptor, as GNU's
;; SYNTAX (syntax.h:116-135): an entry that is not a cons is whitespace.
;; `_` is a symbol constituent, not a word constituent.
(defun nlre--word-p (c)
  (let ((table (or nlre--syntax-table (nlre--setup-syntax-table))))
    (if (not (symbolp table))
        (let ((entry (aref table c)))
          (and (consp entry) (= (logand (car entry) 255) 2)))
      (if (eq table t) (eq (nlre--provider-char-syntax c) ?w)
        (or (and (>= c ?a) (<= c ?z)) (and (>= c ?A) (<= c ?Z))
            (and (>= c ?0) (<= c ?9)))))))

(defun nlre--symbol-p (c)
  (let ((table (or nlre--syntax-table (nlre--setup-syntax-table))))
    (if (not (symbolp table))
        (let ((entry (aref table c)))
          (and (consp entry) (memq (logand (car entry) 255) '(2 3)) t))
      (if (eq table t) (memq (nlre--provider-char-syntax c) '(?w ?_))
        (or (nlre--word-p c) (= c ?_))))))

;; Unknown syntax classes deliberately match nothing.
(defun nlre--syntax-p (class c)
  (let ((table (or nlre--syntax-table (nlre--setup-syntax-table))))
    (if (not (symbolp table))
        (let ((entry (aref table c)))
          (eq (aref nlre--syntax-designators
                    (if (consp entry) (logand (car entry) 255) 0))
              (if (eq class ?-) 32 class)))
      (if (eq table t) (eq (nlre--provider-char-syntax c) (if (eq class ?-) 32 class))
        (nlre--syntax-p-fallback class c)))))

(defun nlre--syntax-p-fallback (class c)
  "ASCII approximation used only before `char-syntax' exists."
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

(defun nlre--posix-class-p (name c)
  "Return non-nil when C belongs to dynamic POSIX class NAME."
  (let ((category (and (>= c 128)
                      (if (fboundp 'get-char-code-property)
                      (get-char-code-property c 'general-category)
                    (and (fboundp 'nelisp-unicode-category)
                         (nelisp-unicode-category c))))))
    (cond
     ((equal name "digit") (and (>= c ?0) (<= c ?9)))
     ((equal name "xdigit")
      (or (and (>= c ?0) (<= c ?9)) (and (>= c ?a) (<= c ?f))
          (and (>= c ?A) (<= c ?F))))
     ((equal name "ascii") (< c 128))
     ((equal name "nonascii") (>= c 128))
     ((equal name "multibyte") (>= c 128))
     ((equal name "unibyte") (< c 128))
     ((equal name "alpha")
      (if (< c 128)
          (or (and (>= c ?a) (<= c ?z)) (and (>= c ?A) (<= c ?Z)))
        (if category (memq category '(Lu Ll Lt Lm Lo Mn Mc Me Nl))
          (/= (nlre--upcase-char c) (nlre--downcase-char c)))))
     ((equal name "alnum")
      (if (< c 128)
          (or (and (>= c ?a) (<= c ?z)) (and (>= c ?A) (<= c ?Z))
              (and (>= c ?0) (<= c ?9)))
        (or (and category (memq category '(Lu Ll Lt Lm Lo Mn Mc Me Nl Nd)))
            (and (not category)
                 (or (and (>= c ?0) (<= c ?9))
                     (/= (nlre--upcase-char c) (nlre--downcase-char c)))))))
     ((equal name "word")
      (nlre--word-p c))
     ((equal name "upper")
      (/= c (nlre--downcase-char c)))
     ((equal name "lower")
      (and (= c (nlre--downcase-char c)) (/= c (nlre--upcase-char c))))
     ((equal name "space") (nlre--syntax-p 32 c))
     ((equal name "blank")
      (if (< c 128) (memq c '(9 32)) (eq category 'Zs)))
     ((equal name "punct")
      (if (< c 128)
          (and (> c 32) (< c 127)
               (not (or (and (>= c ?a) (<= c ?z))
                        (and (>= c ?A) (<= c ?Z))
                        (and (>= c ?0) (<= c ?9)))))
        (not (nlre--word-p c))))
     ((equal name "cntrl") (< c 32))
     ((equal name "graph")
      (if (< c 256) (and (> c 32) (not (and (>= c 127) (<= c 160))))
        (and category (not (memq category '(Zs Zl Zp Cc Cs Cn))))))
     ((equal name "print")
      (if (< c 256) (and (>= c 32) (not (and (>= c 127) (<= c 159))))
        (and category (not (memq category '(Cc Cs Cn))))))
     (t nil))))

(defun nlre--posix-match-p (name canonical bitmap original)
  "Match GNU's ASCII class bitmap or multibyte predicate."
  (if bitmap
      ;; Class bitmaps contain raw ASCII members plus translated members.
      (or (and (< canonical 128) (nlre--posix-class-p name canonical))
          (and nlre--fold
               (if (and (null nlre--translate-table) (< canonical 128))
                   ;; The only other ASCII preimage under the default fold
                   ;; is an uppercase letter. Custom tables retain the scan.
                   (and (>= canonical ?a) (<= canonical ?z)
                        (nlre--posix-class-p name (- canonical 32)))
                 (let ((ascii 0) found)
                 (while (and (< ascii 128) (not found))
                   (when (and (nlre--posix-class-p name ascii)
                              (= canonical (nlre--fold-char ascii)))
                     (setq found t))
                   (setq ascii (1+ ascii)))
                   found))))
    (if (and nlre--fold (member name '("upper" "lower")))
        (or (nlre--posix-class-p "upper" original)
            (nlre--posix-class-p "lower" original))
      (nlre--posix-class-p name canonical))))

(defun nlre--range-member-match-p (member target bitmap)
  "Test a raw set MEMBER using GNU's translated bitmap/range rules."
  (if (>= member #x3fff80)
      (and bitmap (= member target))
    (let* ((translated (nlre--fold-char member))
           (byte (nlre--character-byte translated)))
      (if bitmap
          (= target (if byte (if (< byte 128) byte (+ #x3fff00 byte))
                      ;; ASCII ranges retain their byte on a nonbyte mapping.
                      (if (< member 128) member -1)))
        (or (and (>= member 128) (= member target))
            (and (>= translated 128) (= translated target)))))))

(defun nlre--set-match (neg ranges classes c &optional unibyte ascii-ranges)
  ;; A translated unibyte character that cannot become a byte retains its
  ;; original code, but uses the range table, not the bitmap (GNU execute_charset).
  (let* ((original c)
         (raw (if (and unibyte (>= c 128) (< c 256)) (+ #x3fff00 c) c))
         (translated (nlre--fold-char raw))
         (byte (nlre--character-byte translated))
         (bitmap (and byte t))
         (target (if (and unibyte (not byte)) raw translated))
         (portable (and nlre--fold (null nlre--translate-table)
                        ascii-ranges (< target 128)))
         (hit (and (or portable (not nlre--fold)
                       (and (not bitmap) (>= target 128) (< target #x3fff80)))
                   (nlre--set-in-ranges ranges target)))
         (names classes))
    (while (and names (not hit))
      (setq hit (nlre--posix-match-p (car names) target bitmap original)
            names (cdr names)))
    (when (and portable (not hit) (>= target ?a) (<= target ?z))
      (setq hit (nlre--set-in-ranges ranges (- target 32))))
    (when (and nlre--fold (not portable) (not hit))
      ;; Canonicalization is deliberately deferred.  Enumerating the raw
      ;; ranges honors even direct edits to a canonical table without a
      ;; synchronized equivalence-cycle table.
      (let ((rs ranges))
        (while (and rs (not hit))
          (let ((member (car (car rs))) (end (cdr (car rs))))
            (while (and (<= member end) (not hit))
              (setq hit (nlre--range-member-match-p member target bitmap)
                    member (1+ member))))
          (setq rs (cdr rs)))))
    (if neg (not hit) hit)))

(defun nlre--start-map-seq (nodes c)
  "Return (ALLOWED . NULLABLE) for the candidate byte C in NODES."
  (let (allowed (nullable t))
    (while (and nodes nullable (not allowed))
      (let ((result (nlre--start-map-node (car nodes) c)))
        (setq allowed (car result) nullable (cdr result) nodes (cdr nodes))))
    (cons allowed nullable)))

(defun nlre--start-map-node (node c)
  "Model GNU's first-byte bitmap for a folded unibyte candidate."
  (let ((tag (car node)))
    (cond
     ((eq tag :lit)
      (let* ((raw (nth 1 node))
             (byte (if nlre--pattern-multibyte
                       (nlre--character-byte (nlre--fold-char raw))
                     (if (< raw 128)
                         (or (nlre--character-byte (nlre--fold-char raw)) raw)
                       raw))))
        (cons (and byte (= byte c)) nil)))
     ((eq tag :word)
      (let ((word (nlre--word-p c)))
        (cons (if (nth 1 node) (not word) word) nil)))
     ((eq tag :space)
      (let ((space (nlre--space-p c)))
        (cons (if (nth 1 node) (not space) space) nil)))
     ((eq tag :syntax)
      (let ((syntax (nlre--syntax-p (nth 1 node) c)))
        (cons (if (nth 2 node) (not syntax) syntax) nil)))
     ((eq tag :set)
      (let ((rs (nth 2 node)) (names (nth 3 node)) hit)
        (while (and rs (not hit))
          (let ((member (caar rs)) (end (cdar rs)))
            (while (and (<= member end) (not hit))
              (setq hit (nlre--range-member-match-p member c t)
                    member (1+ member))))
          (setq rs (cdr rs)))
        (while (and names (not hit))
          (setq hit (nlre--posix-match-p (car names) c t c) names (cdr names)))
        (cons (if (nth 1 node) (not hit) hit) nil)))
     ((eq tag :seq) (nlre--start-map-seq (nth 1 node) c))
     ((eq tag :group) (nlre--start-map-node (nth 2 node) c))
     ((eq tag :alt)
      (let ((branches (nth 1 node)) allowed nullable)
        (while (and branches (not allowed))
          (let ((result (nlre--start-map-seq (car branches) c)))
            (setq allowed (car result) nullable (or nullable (cdr result))
                  branches (cdr branches))))
        (cons allowed nullable)))
     ((memq tag '(:star :opt :lazystar :lazyopt :bounded))
      (cons (car (nlre--start-map-node (nth 1 node) c)) t))
     ((memq tag '(:plus :lazyplus)) (nlre--start-map-node (nth 1 node) c))
     ((memq tag '(:bos :eos :bol :eol :wordb :wordedge :symedge)) (cons nil t))
     ;; Unknown consuming nodes conservatively admit a byte.  Their normal
     ;; matcher still decides the result, as for GNU's inaccurate fastmaps.
     (t (cons t (eq tag :backref))))))

(defun nlre--candidate-start-p (nodes string pos)
  "Apply GNU's byte fastmap only where a custom fold leaves byte space."
  (or (null nlre--translate-table) (not nlre--fold)
      nlre--inhibit-search-fastmap (multibyte-string-p string)
      (>= pos (length string))
      (let ((c (aref string pos)))
        (or (>= c 128) (nlre--character-byte (nlre--fold-char c))
            (let ((result (nlre--start-map-seq nodes c)))
              (or (car result) (cdr result)))))))

(defun nlre--match-candidate (nodes string pos n)
  (and (nlre--candidate-start-p nodes string pos)
       (nlre--match-list nodes string pos n)))

(defun nlre--anchored-search-reject-p (ast byte)
  "Model GNU's extra fastmap translation after a leading buffer anchor."
  ;; GNU 31.1 regex-emacs.c: re_search_2 sets RANGE=0 after an initial
  ;; begbuf, then its backward fastmap branch translates the raw byte again.
  ;; Capturing groups emit start_memory; shy sequences emit no opcode.
  (let ((nodes (nlre--seq-nodes ast)))
    (while (and nodes (eq (caar nodes) :seq))
      (setq nodes (append (cadar nodes) (cdr nodes))))
    (and (eq (caar nodes) :bos)
         (let ((result (nlre--start-map-seq nodes (nlre--fold-char byte))))
           (not (or (car result) (cdr result)))))))

;; `fboundp' costs a call into the runtime; these never change after load.
(defvar nlre--have-search-chars (fboundp 'nelisp--string-search-chars))
(defvar nlre--have-char-syntax (fboundp 'char-syntax))

(defun nlre--literal-search (needle string start)
  (cond
   (nlre--have-search-chars
    ;; Use the existing character-index scanner directly.  Its t sentinel
    ;; retains the public fallback for representations it cannot handle.
    (let ((result (nelisp--string-search-chars needle string start)))
      (if (eq result t) (string-search needle string start) result)))
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

(defun nlre--folded-text-equal-at-p (text string pos n)
  (let ((i 0) (m (length text)) (ok t))
    (when (> (+ pos m) n) (setq ok nil))
    (while (and ok (< i m))
      (unless (nlre--literal-char-equal-p
               (aref text i) (multibyte-string-p text)
               (aref string (+ pos i)) (multibyte-string-p string))
        (setq ok nil))
      (setq i (1+ i)))
    ok))

(defun nlre--first-char-before (char string from to)
  "Index of the first CHAR in STRING in [FROM, TO), or nil.
A substring of a multibyte string with wide characters costs its start
index, so such a string is read by index, which follows the string's
char/byte cursor; any other string is cut and searched natively."
  (if (and (multibyte-string-p string) (/= (length string) (string-bytes string)))
      (let ((i from) (hit nil))
        (while (and (not hit) (< i to))
          (if (eq (aref string i) char) (setq hit i) (setq i (1+ i))))
        hit)
    (let ((r (nlre--literal-search (char-to-string char) (substring string from to) 0)))
      (and r (+ from r)))))

(defun nlre--literal-plan-match (plan string start n)
  (let ((needle (aref plan 1)) (fold (aref plan 2)))
    (if (not fold) (nlre--literal-search needle string start)
      (let* ((ascii (aref plan 3)) (m (length needle)) (pos start) hit)
        (if (and (null nlre--translate-table)
                 (aref plan 4))
            ;; Standard ASCII folding has exactly the usual upper/lower pair;
            ;; reuse the native literal search instead of scanning each byte.
            (let* ((c (and (> m 0) (aref ascii 0)))
                   (other (and c (nlre--flip-case c))))
              (if (= m 0) start
                ;; P and Q are the next occurrences of the needle's first
                ;; character in each case.  P is kept across candidates and
                ;; searched again only once POS has passed it (:none: there
                ;; is none).  While P exists Q is wanted only before it, so
                ;; that search covers just [POS, P): scanning the rest of
                ;; the subject for the rarer case on every candidate, as
                ;; this once did, made each call cost the subject's length.
                (let ((p nil) (q (if (= c other) :none nil))
                      (cs (char-to-string c)) (os (char-to-string other)))
                  (while (and (not hit) (<= (+ pos m) n))
                    (let (at)
                      (when (and (not (eq p :none)) (or (null p) (< p pos)))
                        (setq p (or (nlre--literal-search cs string pos) :none)))
                      (if (integerp p)
                          (setq at (or (and (not (eq q :none)) (> p pos)
                                            (nlre--first-char-before other string pos p))
                                       p))
                        (when (and (not (eq q :none)) (or (null q) (< q pos)))
                          (setq q (or (nlre--literal-search os string pos) :none)))
                        (setq at (and (integerp q) q)))
                      (if (or (null at) (> (+ at m) n))
                          (setq pos (1+ n))
                        (if (nlre--folded-text-equal-at-p needle string at n)
                            (setq hit at)
                          (setq pos (1+ at)))))))
                hit))
          (while (and (not hit) (<= (+ pos m) n))
            (if (nlre--folded-text-equal-at-p needle string pos n)
                (setq hit pos)
              (setq pos (1+ pos))))
          hit)))))

(defun nlre--suffix-plan-match (plan string start n)
  (let* ((fold (aref plan 2))
         (fast-fold (and fold (aref plan 8)
                         (null nlre--translate-table)))
         (table (if fast-fold (aref plan 6) (aref plan 1)))
         (empty (if fast-fold (aref plan 7) (aref plan 3)))
         xs short hit)
    (if (and fold (not fast-fold))
        (let ((entries (aref plan 5)))
          (while entries
            (let* ((entry (car entries)) (text (car entry)) (variant (cdr entry))
                   (pos (- n (length text))) (bos (aref variant 2)))
              (when (and (>= pos start) (or (null bos) (= (+ pos bos) 0))
                         (or (null hit) (< pos (car hit)))
                         (nlre--folded-text-equal-at-p text string pos n))
                (setq hit (cons pos variant))))
            (setq entries (cdr entries))))
      (when (> n start)
      (let* ((raw-last (aref string (1- n)))
             (raw-last (if (and (not (aref plan 9))
                                (>= raw-last #x3fff80)
                                (<= raw-last #x3fffff))
                           (- raw-last #x3fff00) raw-last))
             (last (if (and fast-fold (< raw-last 128))
                       (if (and (>= raw-last ?A) (<= raw-last ?Z))
                           (+ raw-last 32) raw-last)
                     (if fold (nlre--fold-char raw-last) raw-last))))
        (setq short (gethash last table))
        (when (> (- n start) 1)
          (let* ((raw-prev (aref string (- n 2)))
                 (raw-prev (if (and (not (aref plan 9))
                                    (>= raw-prev #x3fff80)
                                    (<= raw-prev #x3fffff))
                               (- raw-prev #x3fff00) raw-prev))
                 (prev (if (and fast-fold (< raw-prev 128))
                           (if (and (>= raw-prev ?A) (<= raw-prev ?Z))
                               (+ raw-prev 32) raw-prev)
                         (if fold (nlre--fold-char raw-prev) raw-prev))))
            (setq xs (gethash (+ last (* 1114112 (1+ prev)))
                              table))))))
      (while (and (not hit) (or xs short))
        (unless xs (setq xs short short nil))
        (let* ((entry (car xs)) (text (car entry)) (variant (cdr entry))
               (pos (- n (length text))) (bos (aref variant 2)))
          (when (and (>= pos start) (or (null bos) (= (+ pos bos) 0))
                     (if fast-fold
                         (or (eq pos (nlre--literal-search text string pos))
                             (nlre--folded-text-equal-at-p text string pos n))
                       (if fold
                           (nlre--folded-text-equal-at-p text string pos n)
                         (eq pos (nlre--literal-search text string pos)))))
            (setq hit (cons pos variant))))
        (setq xs (cdr xs)))
      (when (and (not hit) empty)
        (let* ((variant (cdr (car empty))) (bos (aref variant 2)))
          (when (or (null bos) (= (+ n bos) 0))
            (setq hit (cons n variant))))))
    hit))

(defun nlre--greedy-suffix-plan-match (plan string start n)
  "Return the rightmost finite suffix match for greedy-dot PLAN."
  (when (= start 0)
    (let ((single (aref plan 5)))
      ;; A single finite suffix needs neither bucket lookup nor candidate
      ;; merging.  Custom canonical tables retain the general path below.
      (if (and (null (cdr single))
               (or (not (aref plan 2)) (null nlre--translate-table)))
          (let* ((entry (car single)) (text (car entry))
                 (pos (- n (length text))))
            (when (and (>= pos 0)
                       (or (eq pos (nlre--literal-search text string pos))
                           (and (aref plan 2)
                                (nlre--folded-text-equal-at-p text string pos n)))
                       (let ((newline (nlre--literal-search "\n" string 0)))
                         (or (null newline) (>= newline pos))))
              (cons pos (cdr entry))))
    (let* ((fold (aref plan 2))
           (fast-fold (and fold (aref plan 8)
                           (null nlre--translate-table)))
           (table (if fast-fold (aref plan 6) (aref plan 1)))
           (empty (if fast-fold (aref plan 7) (aref plan 3)))
           xs short hit
          (newline (if (and nlre--fold nlre--translate-table)
                       (let ((i 0) found)
                         (while (and (< i n) (not found))
                           (when (= (nlre--fold-char (aref string i)) ?\n)
                             (setq found i))
                           (setq i (1+ i)))
                         found)
                     (nlre--literal-search "\n" string 0))))
      (if (and fold (not fast-fold))
          (setq xs (aref plan 5) empty nil)
        (when (> n 0)
        (let* ((raw-last (aref string (1- n)))
               (raw-last (if (and (not (aref plan 9))
                                  (>= raw-last #x3fff80)
                                  (<= raw-last #x3fffff))
                             (- raw-last #x3fff00) raw-last))
               (last (if (and fast-fold (< raw-last 128))
                         (if (and (>= raw-last ?A) (<= raw-last ?Z))
                             (+ raw-last 32) raw-last)
                       (if fold (nlre--fold-char raw-last) raw-last))))
          (setq short (gethash last table))
          (when (> n 1)
            (let* ((raw-prev (aref string (- n 2)))
                   (raw-prev (if (and (not (aref plan 9))
                                      (>= raw-prev #x3fff80)
                                      (<= raw-prev #x3fffff))
                                 (- raw-prev #x3fff00) raw-prev))
                   (prev (if (and fast-fold (< raw-prev 128))
                             (if (and (>= raw-prev ?A) (<= raw-prev ?Z))
                                 (+ raw-prev 32) raw-prev)
                           (if fold (nlre--fold-char raw-prev) raw-prev))))
              (setq xs (gethash (+ last (* 1114112 (1+ prev)))
                                table)))))))
      (setq xs (append xs short empty))
      (while xs
        (let* ((entry (car xs)) (text (car entry)) (variant (cdr entry))
               (pos (- n (length text))))
          (when (and (>= pos 0) (or (null newline) (>= newline pos))
                     (or (null hit) (> pos (car hit)))
                     (if fast-fold
                         (or (eq pos (nlre--literal-search text string pos))
                             (nlre--folded-text-equal-at-p text string pos n))
                       (if fold
                           (nlre--folded-text-equal-at-p text string pos n)
                         (eq pos (nlre--literal-search text string pos)))))
            (setq hit (cons pos variant))))
        (setq xs (cdr xs)))
      hit)))))

;; PLAN is [:atom NODE] (see `nlre--single-atom-node').  `nlre--fold' is
;; already dynamically bound by `nlre-string-match' for the whole call, so
;; `nlre--match-atom1' (via `nlre--fold-char'/`nlre--set-match') sees the
;; right case-fold state without this function re-binding anything.
(defun nlre--atom-plan-match (plan string start n)
  (let ((node (aref plan 1)) (i start) hit)
    (if (and (eq (car node) :set) (null nlre--translate-table))
        ;; The set test alone: `nlre--match-atom1' would add the consumption
        ;; bound check (always true here, I < N = the limit) and a call per
        ;; character to the same `nlre--set-node-match'.
        (let ((unibyte (not (multibyte-string-p string))))
          (while (and (not hit) (< i n))
            (if (nlre--set-node-match node (aref string i) unibyte)
                (setq hit i)
              (setq i (1+ i)))))
      (while (and (not hit) (< i n))
        (if (and (or (null nlre--translate-table)
                     (nlre--candidate-start-p (list node) string i))
                 (nlre--match-atom1 node string i n)) (setq hit i) (setq i (1+ i)))))
    hit))

(defun nlre--plan-set-caps (caps variant start end)
  (aset caps 0 (cons start end))
  (let ((relative (aref variant 1)) (i 1) (n (length caps)))
    (while (< i n)
      (let ((span (aref relative i)))
        (aset caps i
              (and span (cons (+ start (car span)) (+ start (cdr span))))))
      (setq i (1+ i)))))

(defvar nlre--cap-track nil
  "Non-nil while a windowed buffer search runs with a ceiling below its limit.
Then `nlre--cap-refused' records any refusal caused by that ceiling alone.")
(defvar nlre--cap-refused nil
  "Set when the matcher declined a character that exists past the ceiling.")

(defun nlre--can-consume-p (pos n)
  "Return non-nil when a character at POS is within the matcher bounds."
  (if (< pos (if nlre--match-end-limit
                 (min n nlre--match-end-limit)
               n))
      t
    ;; A refusal only the ceiling caused: the real text goes on past it.
    (when (and nlre--cap-track nlre--match-end-limit (< pos n))
      (setq nlre--cap-refused t))
    nil))

(defun nlre--syntax-run-plan-match (plan string start n caps)
  "Match PLAN using live syntax classes and fill CAPS on success.
GNU regex-emacs.c:5175-5200 reads the current syntax class per character
from the table set up for the search; its capture registers at
:4593-4628 retain the final repeated iteration.  The compiled plan
contains only class designators, so direct table edits remain visible even
when they do not invalidate the compiled cache."
  (when (and (= start 0) (>= n (aref plan 3)))
    (let ((classes (aref plan 1)) (end 0)
          (table (or nlre--syntax-table (nlre--setup-syntax-table))))
      (if (symbolp table)
          (while (and (< end n) (memq (nlre--provider-char-syntax (aref string end)) classes))
            (setq end (1+ end)))
        (while (and (< end n)
                    (memq (let ((entry (aref table (aref string end))))
                            (aref nlre--syntax-designators
                                  (if (consp entry) (logand (car entry) 255) 0)))
                          classes))
          (setq end (1+ end))))
      (when (= end n)
        (aset caps 0 (cons 0 n))
        (when (and (aref plan 2) (> n 0))
          (aset caps (aref plan 2) (cons (1- n) n)))
        0))))

(defun nlre--match-atom1 (node s pos n)
  "Match a single non-quantified atom NODE at POS; return end-pos or nil.
Does NOT continue to any rest (used for one repetition)."
  (let ((tag (car node)))
    (cond
     ((eq tag :lit) (and (nlre--can-consume-p pos n)
                         (nlre--literal-char-equal-p
                          (nth 1 node) nlre--pattern-multibyte
                          (aref s pos) (multibyte-string-p s))
                         (1+ pos)))
     ((eq tag :any) (and (nlre--can-consume-p pos n)
                         (not (eq (nlre--fold-char (aref s pos)) ?\n)) (1+ pos)))
     ((eq tag :set) (and (nlre--can-consume-p pos n)
                         (nlre--set-node-match node (aref s pos)
                                              (not (multibyte-string-p s)))
                         (1+ pos)))
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
             ;; GNU regex-emacs.c:4972-4974 accepts both string edges,
             ;; even when the adjoining character is not a word constituent.
             (boundary (or (= pos 0) (= pos n) (not (eq before after)))))
        (and (if (nth 1 node) (not boundary) boundary) pos)))
     ((eq tag :backref)
      ;; Matches the text the numbered group last captured (fold-aware);
      ;; an unset group never matches, as in GNU.
      (let ((cap (aref nlre--caps (nth 1 node))))
        (and cap (car cap) (cdr cap)
             (let* ((from (car cap)) (len (- (cdr cap) from)) (end (+ pos len))
                    (lim (if nlre--match-end-limit (min n nlre--match-end-limit) n))
                    (i 0))
               (and (or (<= end lim)
                        (progn (when (and nlre--cap-track (<= end n))
                                 (setq nlre--cap-refused t))
                               nil))
                    (progn
                      (while (and (< i len)
                                  (let ((a (aref s (+ from i))) (b (aref s (+ pos i))))
                                    (unless (multibyte-string-p s)
                                      (when (>= a 128) (setq a (+ a #x3fff00)))
                                      (when (>= b 128) (setq b (+ b #x3fff00))))
                                    (eq (nlre--fold-char a) (nlre--fold-char b))))
                        (setq i (1+ i)))
                      (= i len))
                    end)))))
     ((eq tag :bol) (and (or (= pos 0) (eq (aref s (1- pos)) ?\n)) pos))
     ((eq tag :eol) (and (or (= pos n) (eq (aref s pos) ?\n)) pos))
     ((eq tag :bos) (and (= pos 0) pos))
     ((eq tag :eos) (and (= pos n) pos))
     (t nil))))

(defun nlre--set-node-match (node c unibyte)
  "Match set NODE using its ASCII plan or its live table-dependent matcher."
  (let ((plan (and (< c 128) (null nlre--translate-table) (nth 5 node))))
    (if plan
        (let* ((byte (if (and nlre--fold (>= c ?A) (<= c ?Z)) (+ c 32) c))
               (hit (= (aref (aref plan (if nlre--fold 1 0)) byte) 1)))
          (if (nth 1 node) (not hit) hit))
      (nlre--set-match (nth 1 node) (nth 2 node) (nth 3 node) c unibyte (nth 4 node)))))

(defun nlre--match-list (nodes s pos n)
  "Match NODES (a seq, possibly containing :star/:group/:alt/sentinels) at POS.
Return end-pos or nil."
  (if (null nodes) pos
    (let* ((nd (car nodes)) (rest (cdr nodes)) (tag (car nd)))
      (cond
       ((eq tag :star) (nlre--match-star (nth 1 nd) rest s pos n))
       ((eq tag :bounded)
        (or (and (> (nth 2 nd) 0)
                 (nlre--match-list
                  (cons (nth 1 nd)
                        (cons (list :repeat-next pos
                                    (list :bounded (nth 1 nd) (1- (nth 2 nd))))
                              rest)) s pos n))
            (nlre--match-list rest s pos n)))
       ;; Non-greedy: try the REST first, and only consume another repetition
       ;; when that fails -- the mirror image of `nlre--match-star'.
       ((eq tag :lazystar)
        (or (nlre--match-list rest s pos n)
            (nlre--match-list
             (cons (nth 1 nd) (cons (list :repeat-next pos nd) rest)) s pos n)))
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
       ((eq tag :repeat-next)
        ;; Keep the repetition's continuation inside its group's search.
        ;; A failed continuation can then revisit an earlier alternative,
        ;; and savestart/saveend unwind captures on that same path.  An
        ;; empty iteration may capture, but cannot start another iteration.
        (nlre--match-list (if (> pos (nth 1 nd)) (cons (nth 2 nd) rest) rest)
                          s pos n))
       (t ;; plain atom
        (let ((p2 (nlre--match-atom1 nd s pos n)))
          (and p2 (nlre--match-list rest s p2 n))))))))

(defun nlre--seq-nodes (node)
  "Return NODE as a list of seq nodes (unwrap :seq / wrap :alt)."
  (cond ((eq (car node) :seq) (nth 1 node))
        (t (list node))))

(defun nlre--match-star (x rest s pos n)
  "Greedy star of atom/group X then REST."
  (if (memq (car x) '(:lit :any :set :word :space :syntax))
      (let ((end pos) next result
            (exclusive (nlre--mutually-exclusive-p x rest)))
        ;; Fixed-width atoms have no captures or internal alternatives.
        ;; Retain every possible split unless the first continuation
        ;; character provably cannot be consumed by the repeated atom.
        (while (setq next (nlre--match-atom1 x s end n)) (setq end next))
        (setq result (nlre--match-list rest s end n))
        (unless exclusive
          (while (and (not result) (> end pos))
            (setq end (1- end)
                  result (nlre--match-list rest s end n))))
        result)
    (or (nlre--match-list
         (cons x (cons (list :repeat-next pos (list :star x)) rest)) s pos n)
        (nlre--match-list rest s pos n))))

(defun nlre--mutually-exclusive-p (x rest)
  "Prove portable atom X disjoint from REST's first required literal.
Groups and capture sentinels consume nothing. Unknown/optional prefixes and
custom canonical tables retain ordinary backtracking. Unicode preimages of
an ASCII literal are tested with the same canonical ASCII set bitmap."
  (when (null nlre--translate-table)
    (let ((todo rest) first done)
      (while (and todo (not done))
        (let ((node (car todo)))
          (cond
           ((memq (car node) '(:savestart :saveend :bos :eos :bol :eol :wordb :wordedge :symedge))
            (setq todo (cdr todo)))
           ((eq (car node) :seq) (setq todo (append (nth 1 node) (cdr todo))))
           ((eq (car node) :group)
            (setq todo (append (nlre--seq-nodes (nth 2 node)) (cdr todo))))
           (t (setq first node done t)))))
      (when (and (eq (car first) :lit) (< (nth 1 first) 128))
        (let* ((c (nth 1 first))
               (c (if (and nlre--fold (>= c ?A) (<= c ?Z)) (+ c 32) c)))
          (cond
           ((and (eq (car x) :set) (nth 5 x))
            (let ((hit (= (aref (aref (nth 5 x) (if nlre--fold 1 0)) c) 1)))
              (if (nth 1 x) hit (not hit))))
           ((and (eq (car x) :lit) (< (nth 1 x) 128))
            (not (nlre--literal-char-equal-p (nth 1 x) t c t)))
           ((eq (car x) :any) (= c 10))
           (t nil)))))))

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

;; ---- native matcher program (rxnative) ----
;; The matcher core is the native word `nelisp--rx-exec' (scripts/nelisp-
;; standalone-build.el, `m5_rx_*'), a backtracking VM over a program kept in
;; a unibyte string, as GNU keeps a compiled pattern in a buffer
;; (regex-emacs.c, regex_compile / re_match_2_internal).  This is the compiler
;; from the parsed pattern.  Anything it does not model makes it decline
;; (value nil), and the caller then runs the Lisp matcher below instead.
;; Program words are u32, little endian:
;;   [0] total registers  [1] groups  [2] flags (1 anchored at \=`, 2 first-byte
;;   map usable, 4 fold)  [3] entry word  [4] 0  [5..36] 128-byte first map
;;   [37..] code.
;; Code: 0 MATCH, 1 CHAR c, 2 CHAR2 lc uc, 3 ANY, 4 SET hf m0..m31 (128 bytes),
;; 5 BOL, 6 EOL, 7 BOS, 8 EOS, 9 SAVE r, 10 SPLIT a b (try a, fail to b),
;; 11 JMP a, 12 LOOPSET k, 13 LOOPCHK k a (jump when no progress since
;; LOOPSET), 14 BACKREF g, 15 REP min max ATOM... (greedy single-char loop).
(defvar nlre--nat-code nil)
(defvar nlre--nat-pc 0)
(defvar nlre--nat-ng 1)
(defvar nlre--nat-nloops 0)
(defvar nlre--nat-fold nil)
(defvar nlre--nat-closed nil)
(defconst nlre--nat-inf 4294967295)
(defconst nlre--nat-header 37)
(defconst nlre--nat-max-words 6000)

(defun nlre--nat-decline () (throw 'nlre--nat-decline nil))

(defun nlre--nat-emit (&rest words)
  (while words
    (when (>= nlre--nat-pc nlre--nat-max-words) (nlre--nat-decline))
    (aset nlre--nat-code nlre--nat-pc (car words))
    (setq nlre--nat-pc (1+ nlre--nat-pc) words (cdr words))))

(defun nlre--nat-letter-p (c)
  (or (and (>= c ?a) (<= c ?z)) (and (>= c ?A) (<= c ?Z))))

(defun nlre--nat-pack (bytes)
  "Pack the 128 values of vector BYTES into 32 little-endian u32 words."
  (let ((i 0) (out nil))
    (while (< i 128)
      (setq out (cons (+ (aref bytes i) (* 256 (aref bytes (+ i 1)))
                         (* 65536 (aref bytes (+ i 2)))
                         (* 16777216 (aref bytes (+ i 3))))
                      out)
            i (+ i 4)))
    (nreverse out)))

(defun nlre--nat-set-map (node)
  "Return the 128-element 0/1 membership vector of set NODE for ASCII."
  (let ((m (make-vector 128 0)) (c 0))
    (while (< c 128)
      (when (nlre--set-node-match node c nil) (aset m c 1))
      (setq c (1+ c)))
    m))

(defun nlre--nat-set-hf (node map)
  "Non-zero when a byte >= 128 must make the native matcher decline."
  (let ((hf 0) (ranges (nth 2 node)) (classes (nth 3 node)))
    (when (nth 1 node) (setq hf 1))
    (while ranges
      (when (>= (cdr (car ranges)) 128) (setq hf 1))
      (setq ranges (cdr ranges)))
    (while classes
      (when (member (car classes) '("space" "word")) (nlre--nat-decline))
      (unless (member (car classes) '("digit" "xdigit" "ascii" "cntrl")) (setq hf 1))
      (setq classes (cdr classes)))
    ;; K and S have non-ASCII case partners (KELVIN SIGN, LONG S).
    (when (and nlre--nat-fold
               (or (= 1 (aref map ?k)) (= 1 (aref map ?K))
                   (= 1 (aref map ?s)) (= 1 (aref map ?S))))
      (setq hf 1))
    hf))

(defun nlre--nat-atom-words (node)
  "Return the program words of single-character NODE, or nil."
  (let ((tag (car node)))
    (cond
     ((eq tag :lit)
      (let ((c (nth 1 node)))
        (when (or (not (integerp c)) (>= c 128) (< c 0)) (nlre--nat-decline))
        (if (and nlre--nat-fold (nlre--nat-letter-p c))
            (list 2 (if (<= c ?Z) (+ c 32) c) (if (>= c ?a) (- c 32) c))
          (list 1 c))))
     ((eq tag :any) (list 3))
     ((eq tag :set)
      (let ((map (nlre--nat-set-map node)))
        (cons 4 (cons (nlre--nat-set-hf node map) (nlre--nat-pack map)))))
     (t nil))))

(defun nlre--nat-new-loop ()
  (prog1 (+ (* 2 nlre--nat-ng) nlre--nat-nloops)
    (setq nlre--nat-nloops (1+ nlre--nat-nloops))))

(defun nlre--nat-repeat (x min max)
  "Emit X repeated MIN..MAX times, greedily (single-character X only)."
  (apply #'nlre--nat-emit 15 min max (nlre--nat-atom-words x)))

(defun nlre--nat-star (x lazy)
  "Emit the loop of X (a star), greedy or LAZY, with GNU's empty-iteration stop."
  (let* ((l1 nlre--nat-pc) (k (nlre--nat-new-loop)) exit-a exit-b)
    (nlre--nat-emit 10 0 0)
    (if lazy
        (progn (aset nlre--nat-code (+ l1 2) (+ l1 3)) (setq exit-a (+ l1 1)))
      (aset nlre--nat-code (+ l1 1) (+ l1 3)) (setq exit-a (+ l1 2)))
    (nlre--nat-emit 12 k)
    (nlre--nat-node x)
    (nlre--nat-emit 13 k 0)
    (setq exit-b (1- nlre--nat-pc))
    (nlre--nat-emit 11 l1)
    (aset nlre--nat-code exit-a nlre--nat-pc)
    (aset nlre--nat-code exit-b nlre--nat-pc)))

(defun nlre--nat-opt (x lazy)
  (let ((l1 nlre--nat-pc))
    (nlre--nat-emit 10 0 0)
    (if lazy
        (aset nlre--nat-code (+ l1 2) (+ l1 3))
      (aset nlre--nat-code (+ l1 1) (+ l1 3)))
    (nlre--nat-node x)
    (aset nlre--nat-code (if lazy (+ l1 1) (+ l1 2)) nlre--nat-pc)))

(defun nlre--nat-bounded (x extra)
  "Emit X? nested EXTRA deep, an empty iteration ending the chain."
  (when (> extra 16) (nlre--nat-decline))
  (let ((k (nlre--nat-new-loop)) (fixups nil) (i 0))
    (while (< i extra)
      (let ((l nlre--nat-pc))
        (nlre--nat-emit 10 (+ l 3) 0)
        (setq fixups (cons (+ l 2) fixups))
        (nlre--nat-emit 12 k)
        (nlre--nat-node x)
        (when (< (1+ i) extra)
          (nlre--nat-emit 13 k 0)
          (setq fixups (cons (1- nlre--nat-pc) fixups))))
      (setq i (1+ i)))
    (dolist (f fixups) (aset nlre--nat-code f nlre--nat-pc))))

(defun nlre--nat-node (node)
  (let ((tag (car node)))
    (cond
     ((memq tag '(:lit :any :set))
      (apply #'nlre--nat-emit (nlre--nat-atom-words node)))
     ((eq tag :bol) (nlre--nat-emit 5))
     ((eq tag :eol) (nlre--nat-emit 6))
     ((eq tag :bos) (nlre--nat-emit 7))
     ((eq tag :eos) (nlre--nat-emit 8))
     ((eq tag :seq) (nlre--nat-nodes (nth 1 node)))
     ((eq tag :alt)
      (let ((branches (nth 1 node)) (jumps nil))
        (while branches
          (if (cdr branches)
              (let ((l nlre--nat-pc))
                (nlre--nat-emit 10 (+ l 3) 0)
                (nlre--nat-nodes (car branches))
                (nlre--nat-emit 11 0)
                (setq jumps (cons (1- nlre--nat-pc) jumps))
                (aset nlre--nat-code (+ l 2) nlre--nat-pc))
            (nlre--nat-nodes (car branches)))
          (setq branches (cdr branches)))
        (dolist (j jumps) (aset nlre--nat-code j nlre--nat-pc))))
     ((eq tag :group)
      (let ((g (nth 1 node)))
        (when (or (< g 1) (>= g nlre--nat-ng)) (nlre--nat-decline))
        (nlre--nat-emit 9 (* 2 g))
        (nlre--nat-node (nth 2 node))
        (nlre--nat-emit 9 (1+ (* 2 g)))
        (setq nlre--nat-closed (cons g nlre--nat-closed))))
     ((eq tag :backref)
      (unless (memq (nth 1 node) nlre--nat-closed) (nlre--nat-decline))
      (nlre--nat-emit 14 (nth 1 node)))
     ((eq tag :star)
      (if (nlre--nat-atom-words (nth 1 node))
          (nlre--nat-repeat (nth 1 node) 0 nlre--nat-inf)
        (nlre--nat-star (nth 1 node) nil)))
     ((eq tag :plus)
      (if (nlre--nat-atom-words (nth 1 node))
          (nlre--nat-repeat (nth 1 node) 1 nlre--nat-inf)
        (nlre--nat-node (nth 1 node))
        (nlre--nat-star (nth 1 node) nil)))
     ((eq tag :opt)
      (if (nlre--nat-atom-words (nth 1 node))
          (nlre--nat-repeat (nth 1 node) 0 1)
        (nlre--nat-opt (nth 1 node) nil)))
     ((eq tag :lazystar) (nlre--nat-star (nth 1 node) t))
     ((eq tag :lazyplus)
      (nlre--nat-node (nth 1 node))
      (nlre--nat-star (nth 1 node) t))
     ((eq tag :lazyopt) (nlre--nat-opt (nth 1 node) t))
     ((eq tag :bounded)
      (if (nlre--nat-atom-words (nth 1 node))
          (nlre--nat-repeat (nth 1 node) 0 (nth 2 node))
        (nlre--nat-bounded (nth 1 node) (nth 2 node))))
     (t (nlre--nat-decline)))))

(defun nlre--nat-nodes (nodes)
  (while nodes
    (nlre--nat-node (car nodes))
    (setq nodes (cdr nodes))))

(defun nlre--nat-first-atom (node fm)
  (let ((tag (car node)) (c 0))
    (cond
     ((eq tag :lit)
      (let ((ch (nth 1 node)))
        (aset fm ch 1)
        (when (and nlre--nat-fold (nlre--nat-letter-p ch))
          (aset fm (if (<= ch ?Z) (+ ch 32) (- ch 32)) 1))))
     ((eq tag :any)
      (while (< c 128) (unless (= c 10) (aset fm c 1)) (setq c (1+ c))))
     (t
      (let ((m (nlre--nat-set-map node)))
        (while (< c 128) (when (= 1 (aref m c)) (aset fm c 1)) (setq c (1+ c))))))))

(defun nlre--nat-first-node (node fm)
  "Add the possible first characters of NODE to FM; non-nil when NODE can be empty."
  (let ((tag (car node)))
    (cond
     ((memq tag '(:lit :any :set)) (nlre--nat-first-atom node fm) nil)
     ((memq tag '(:bol :eol :bos :eos)) t)
     ((eq tag :seq) (nlre--nat-first-seq (nth 1 node) fm))
     ((eq tag :alt)
      (let ((n nil))
        (dolist (b (nth 1 node)) (when (nlre--nat-first-seq b fm) (setq n t)))
        n))
     ((eq tag :group) (nlre--nat-first-node (nth 2 node) fm))
     ((memq tag '(:star :opt :lazystar :lazyopt :bounded))
      (nlre--nat-first-node (nth 1 node) fm) t)
     ((memq tag '(:plus :lazyplus)) (nlre--nat-first-node (nth 1 node) fm))
     ((eq tag :backref) (let ((c 0)) (while (< c 128) (aset fm c 1) (setq c (1+ c)))) t)
     (t (nlre--nat-decline)))))

(defun nlre--nat-first-seq (nodes fm)
  (let ((nullable t))
    (while (and nodes nullable)
      (setq nullable (nlre--nat-first-node (car nodes) fm) nodes (cdr nodes)))
    nullable))

(defun nlre--nat-words-string (words)
  (let ((out nil))
    (dolist (w words)
      (setq out (cons (logand (/ w 16777216) 255)
                      (cons (logand (/ w 65536) 255)
                            (cons (logand (/ w 256) 255)
                                  (cons (logand w 255) out))))))
    (setq out (nreverse out))
    (let ((s nil) (chunk nil) (k 0))
      (while out
        (setq chunk (cons (car out) chunk) out (cdr out) k (1+ k))
        (when (or (null out) (>= k 128))
          (let ((piece (apply #'unibyte-string (nreverse chunk))))
            (setq s (if s (concat s piece) piece) chunk nil k 0))))
      s)))

(defun nlre--nat-compile (pat fold)
  "Return the native program string for regexp PAT under FOLD, or nil."
  (let ((i 0) (n (length pat)) (ascii t))
    (while (and ascii (< i n))
      (when (>= (aref pat i) 128) (setq ascii nil))
      (setq i (1+ i)))
    (and ascii (> n 0)
     (catch 'nlre--nat-decline
      (let* ((nlre--fold fold) (nlre--translate-table nil)
             (compiled (nlre--compiled-pattern pat))
             (ast (aref compiled 0))
             (nlre--nat-ng (aref compiled 1))
             (nlre--nat-code (make-vector nlre--nat-max-words 0))
             (nlre--nat-pc nlre--nat-header)
             (nlre--nat-nloops 0)
             (nlre--nat-fold fold)
             (nlre--nat-closed nil)
             (fm (make-vector 128 0))
             (top (nlre--seq-nodes ast))
             (flags (if fold 4 0)))
        (when (> nlre--nat-ng 100) (nlre--nat-decline))
        (nlre--nat-node ast)
        (nlre--nat-emit 0)
        (unless (nlre--nat-first-node ast fm)
          (setq flags (logior flags 2)))
        (when (eq (car-safe (car top)) :bos) (setq flags (logior flags 1)))
        (let ((words (append (list (+ (* 2 nlre--nat-ng) nlre--nat-nloops)
                                   nlre--nat-ng flags nlre--nat-header 0)
                             (nlre--nat-pack fm)
                             (let ((l nil) (j nlre--nat-header))
                               (while (< j nlre--nat-pc)
                                 (setq l (cons (aref nlre--nat-code j) l) j (1+ j)))
                               (nreverse l)))))
          (nlre--nat-words-string words)))))))


;; ---- native matcher: cache and entry points ----
(defvar nlre--nat-have (fboundp 'nelisp--rx-exec)
  "Non-nil when the native matcher word is linked into this image.")
(defvar nlre--nat-progs (make-hash-table :test 'equal)
  "Regexp -> (PROGRAM . GROUPS), or :no when it declined; case-sensitive.")
(defvar nlre--nat-progs-fold (make-hash-table :test 'equal)
  "Like `nlre--nat-progs' for case-folding searches.")
(defvar nlre--nat-lits (make-hash-table :test 'equal)
  "Literal search string -> (PROGRAM . GROUPS) or :no, case-sensitive.")
(defvar nlre--nat-lits-fold (make-hash-table :test 'equal)
  "Like `nlre--nat-lits' for case-folding searches.")
(defvar nlre--nat-scratch nil
  "Preallocated registers, undo log and backtrack frames of `nelisp--rx-exec'.")
(defvar nlre--nat-caps (make-vector 204 -1)
  "Capture positions written by `nelisp--rx-exec' (two per group).")
(defvar nlre--nat-seen nil
  "(BUFFER . TICK) of the last buffer search that found no usable text memo.")

(defun nlre--nat-program (re fold literal)
  "Return (PROGRAM . GROUPS) for RE (a literal string when LITERAL), or nil."
  (let* ((tbl (if literal (if fold nlre--nat-lits-fold nlre--nat-lits)
                (if fold nlre--nat-progs-fold nlre--nat-progs)))
         (p (gethash re tbl)))
    (if p (if (eq p :no) nil p)
      (when (> (hash-table-count tbl) 2000) (clrhash tbl))
      (let* ((src (if literal (regexp-quote re) re))
             (prog (nlre--nat-compile src fold))
             (entry (and prog (cons prog (aref (nlre--compiled-pattern src) 1)))))
        (puthash (copy-sequence re) (or entry :no) tbl)
        entry))))

(defun nlre--nat-fold-default-p ()
  "Non-nil when the current buffer folds case with the built-in mapping."
  (and (eq (funcall nlre--current-buffer-function) nlre--translate-memo-buffer)
       (eq nelisp-casetab--epoch nlre--translate-memo-epoch)
       (eq (symbol-function 'set-case-table) nelisp-casetab--epoch-owner)
       (null nlre--translate-memo)))

(defun nlre--nat-run (entry string start limit anchored n)
  "Run ENTRY on STRING.  Return t (declined), nil (no match) or the packed span."
  (nelisp--rx-exec (car entry) string start limit (if anchored (1+ (* 2 n)) (* 2 n))
                   nlre--nat-caps
                   (or nlre--nat-scratch
                       (setq nlre--nat-scratch (make-string 3200000 0)))))

(defun nlre--nat-publish (ng offset)
  "Store the captures the native matcher left in `nlre--nat-caps' as
`nlre--last-caps'; return the match start.  (The unsupported division opcode
keeps this off the packed result.)"
  (let* ((s (aref nlre--nat-caps 0)) (e (aref nlre--nat-caps 1)))
    (if (= ng 1)
        (setq nlre--last-caps (vector (cons (+ s offset) (+ e offset))))
      (let ((v (make-vector ng nil)) (caps nlre--nat-caps) (g 1))
        (aset v 0 (cons (+ s offset) (+ e offset)))
        (while (< g ng)
          (let ((a (aref caps (* 2 g))))
            (when (>= a 0)
              (aset v g (cons (+ a offset) (+ (aref caps (1+ (* 2 g))) offset)))))
          (setq g (1+ g)))
        (setq nlre--last-caps v)))
    (+ s offset)))

(defun nlre--nat-sm (regexp string start max-end anchored)
  "Native `string-match'.  Return t when declined, else what it answers."
  (let ((fold (and case-fold-search t)))
   (if (or search-spaces-regexp nlre--cap-track
          (and fold (not (nlre--nat-fold-default-p))))
      t
    (let ((entry (nlre--nat-program regexp fold nil)))
      (if (null entry) t
        (let* ((n (length string))
               (r (nlre--nat-run entry string (or start 0)
                                 (if max-end (max 0 (min n max-end)) n) anchored n)))
          (cond ((eq r t) t)
                ((null r) nil)
                (t (nlre--nat-publish (cdr entry) 0)))))))))

(defun nlre--nat-buffer-text (b)
  "The memoized whole text of buffer B (an unnarrowed buffer), or nil.
Slots follow `nelisp-buffer': 11 tick, 15 text memo."
  (let ((tc (aref b 15)))
    (if (and tc (= (car tc) (aref b 11)))
        (cdr tc)
      ;; Rebuild the memo only for a buffer that did not change since the
      ;; previous search, so an edit-search loop keeps the windowed search.
      (let ((seen nlre--nat-seen) (tick (aref b 11)))
        (if (and seen (eq (car seen) b) (= (cdr seen) tick))
            (nelisp-buffer-string b)
          (setq nlre--nat-seen (cons b tick))
          nil)))))

(defun nlre--nat-bsearch (regexp bound noerror literal)
  "Native forward search from point.  Return t when declined."
  (let ((b nelisp--current-buffer))
    (if (or (null b) (not (stringp regexp)) (not (nlre--nat-layout-ok b)) (aref b 7) (aref b 8)
            search-spaces-regexp
            (and bound (not (integerp bound)))
            (and case-fold-search (not (nlre--nat-fold-default-p))))
        t
      (let ((entry (and (> (length regexp) 0)
                        (nlre--nat-program regexp (and case-fold-search t) literal))))
        (if (null entry) t
          (let* ((blen (aref b 13))
                 (pt (or (aref b 12) (1+ blen)))
                 (pmax (1+ (aref b 14)))
                 (lim (if bound (min bound pmax) pmax))
                 (text (and (>= lim pt) (nlre--nat-buffer-text b))))
            (if (null text) t
              (let ((r (nlre--nat-run entry text (1- pt) (1- lim) nil (aref b 14))))
                (cond
                 ((eq r t) t)
                 ((null r)
                  (cond ((null noerror) (signal 'search-failed (list regexp)))
                        ((eq noerror t) nil)
                        (t (aset b 12 (if (= lim (1+ blen)) nil lim)) nil)))
                 (t
                  (nlre--nat-publish (cdr entry) 1)
                  (setq nlre--last-buffer b)
                  (let ((np (1+ (aref nlre--nat-caps 1))))
                    (aset b 12 (if (= np (1+ blen)) nil np))
                    np)))))))))))

(defun nlre--nat-looking-at (regexp)
  "Native `looking-at'.  Return t when declined."
  (let ((b nelisp--current-buffer))
    (if (or (null b) (not (stringp regexp)) (not (nlre--nat-layout-ok b)) (aref b 7) (aref b 8)
            search-spaces-regexp
            (and case-fold-search (not (nlre--nat-fold-default-p))))
        t
      (let ((entry (nlre--nat-program regexp (and case-fold-search t) nil)))
        (if (null entry) t
          (let* ((pt (or (aref b 12) (1+ (aref b 13))))
                 (text (nlre--nat-buffer-text b)))
            (if (null text) t
              (let ((r (nlre--nat-run entry text (1- pt) (aref b 14) t (aref b 14))))
                (cond ((eq r t) t)
                      ((null r) nil)
                      (t (nlre--nat-publish (cdr entry) 1)
                         (setq nlre--last-buffer b)
                         'yes))))))))))

(defvar nlre--nat-layout 'unknown
  "t when `nelisp-buffer' slots are laid out as `nlre--nat-bsearch' expects.")
(defun nlre--nat-layout-ok (b)
  (when (eq nlre--nat-layout 'unknown)
    (setq nlre--nat-layout
          (and nlre--nat-have (recordp b) (fboundp 'nelisp-buffer-pend)
               (= (length b) 17)
               (eq (aref b 7) (nelisp-buffer-narrow-start b))
               (eq (aref b 8) (nelisp-buffer-narrow-end b))
               (= (aref b 11) (nelisp-buffer-tick b))
               (eq (aref b 12) (nelisp-buffer-pend b))
               (= (aref b 13) (nelisp-buffer-blen b))
               (= (aref b 14) (nelisp-buffer-len b))
               (eq (aref b 15) (nelisp-buffer-tcache b)))))
  nlre--nat-layout)

;; ---- public entry ----

(defvar nlre--last-caps nil "Capture vector of the last successful match.")
(defvar nlre--last-buffer nil
  "Buffer owning `nlre--last-caps', or nil when positions are string offsets.")

;; Reuse one capture vector per scan and reject impossible leading characters
;; before entering the backtracker (Doc 201 §5.4).
(defun nlre--leading-lit-char (nodes)
  "Return the one character every match of NODES must start with, or nil."
  (and (consp nodes)
       (let ((nd (car nodes)))
         (and (consp nd) (eq (car nd) :lit) (nth 1 nd)))))

(defun nlre--leading-literal-text (nodes)
  "Return NODES' leading run of at least two literal characters."
  (let ((rest nodes) (out "") (count 0))
    (while (and rest (consp (car rest)) (eq (car (car rest)) :lit))
      (setq out (concat out (char-to-string (nth 1 (car rest))))
            count (1+ count)
            rest (cdr rest)))
    (and (> count 1) out)))

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
  "`string-match' through the native matcher, else the Lisp one below."
  (let ((r (if nlre--nat-have (nlre--nat-sm regexp string start max-end nil) t)))
    (if (eq r t) (nlre-string-match-lisp regexp string start max-end) r)))

(defun nlre-string-match-lisp (regexp string &optional start max-end)
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
  ;; GNU sets up the search's syntax table per search; a reset is enough
  ;; here, and unlike a dynamic binding it costs nothing on the plain path.
  (setq nlre--syntax-table nil)
  (let* ((nlre--fold case-fold-search)
         (nlre--translate-table
          (and case-fold-search
               ;; The common case without a call: the memoized read of
               ;; `nlre--current-translate-table' says this buffer folds
               ;; with the built-in mapping.  Same test as there.
               (not (and (eq (funcall nlre--current-buffer-function)
                             nlre--translate-memo-buffer)
                         (eq nelisp-casetab--epoch nlre--translate-memo-epoch)
                         (eq (symbol-function 'set-case-table)
                             nelisp-casetab--epoch-owner)
                         (null nlre--translate-memo)))
               (nlre--search-translate-table regexp string)))
         (compiled (nlre--compiled-pattern regexp))
         (n (length string))
         (i (or start 0))
         (quick (and (null nlre--translate-table)
                     (> (length compiled) 7) (aref compiled 7))))
    (if (and quick
             (if (eq (aref quick 0) :missing)
                 ;; A required character lies inside any match, so none can
                 ;; start at or after I unless it occurs at or after I.
                 ;; Searching from I (not 0) keeps the cost proportional to
                 ;; the distance scanned, as GNU's search from START is.
                 (not (string-search (aref quick 1) string i))
               (and (<= (aref quick 2) n)
                    (string-search (aref quick 1) string (aref quick 2)))))
        nil
      (let* ((nlre--pattern-multibyte (aref compiled 5))
         (limit (if max-end (max 0 (min n max-end)) n))
         (nlre--match-end-limit limit)
         (ng (aref compiled 1))
         (plan (let ((p (aref compiled 2)))
                 ;; A mutable canonical table can distinguish a character
                 ;; set's retained raw ranges from a folded literal variant.
                 (if (and nlre--fold nlre--translate-table p
                          (memq (aref p 0) '(:suffix :greedy-suffix))
                          (aref p 10)) nil p)))
         ;; Finite plans with the common two full captures construct the
         ;; three spans directly, avoiding a generic capture traversal.
         (simple-caps (and (null max-end) plan (eq (aref plan 0) :greedy-suffix)
                           (aref plan 11)))
         (caps (unless simple-caps (make-vector ng nil)))
         (hit nil))
    (setq nlre--caps caps)
    (cond
     ;; Test the O(1) kind predicate before reading character 0: on a
     ;; multibyte string the read moves the string's single char/byte
     ;; cursor to the start, so the next access near START had to walk the
     ;; whole prefix again on every call.
     ((and nlre--fold (> n 0) (not nlre--inhibit-search-fastmap)
           (not (multibyte-string-p string)) (>= (aref string 0) 128)
           (nlre--anchored-search-reject-p (aref compiled 0) (aref string 0)))
      nil)
     ;; A literal and a set run only need LIMIT (N without MAX-END): their
     ;; matches cannot see past what they consume, so a buffer search that
     ;; passes MAX-END uses them too.
     ((and plan (eq (aref plan 0) :literal))
      (setq nlre--fast-plan-hits (1+ nlre--fast-plan-hits)
            hit (nlre--literal-plan-match plan string i limit))
      (if (and hit (<= (+ hit (length (aref plan 1))) limit))
          (aset caps 0 (cons hit (+ hit (length (aref plan 1)))))
        (setq hit nil)))
     ((and (null max-end) plan (eq (aref plan 0) :suffix))
      (setq nlre--fast-plan-hits (1+ nlre--fast-plan-hits))
      (let ((result (nlre--suffix-plan-match plan string i n)))
        (when result
          (setq hit (car result))
          (nlre--plan-set-caps caps (cdr result) hit n))))
     ((and (null max-end) plan (eq (aref plan 0) :greedy-suffix))
      (setq nlre--fast-plan-hits (1+ nlre--fast-plan-hits))
      (let ((result (nlre--greedy-suffix-plan-match plan string i n)))
        (when result
          (let ((suffix-start (car result)))
            (setq hit 0)
            (if simple-caps
                (setq caps (vector (cons 0 n) (cons 0 suffix-start)
                                   (cons suffix-start n)))
              (nlre--plan-set-caps caps (cdr result) suffix-start n)
              (aset caps 0 (cons 0 n))
              (aset caps (aref plan 4) (cons 0 suffix-start)))))))
     ((and plan (eq (aref plan 0) :setrun)
           (null nlre--translate-table))
      (let ((r (nlre--setrun-match plan string i limit)))
        (when r
          (setq hit (car r))
          (aset caps 0 r))))
     ((and (null max-end) plan (eq (aref plan 0) :atom))
      (setq nlre--fast-plan-hits (1+ nlre--fast-plan-hits)
            hit (nlre--atom-plan-match plan string i n))
      (when hit (aset caps 0 (cons hit (1+ hit)))))
     ((and (null max-end) plan (eq (aref plan 0) :syntax-run)
           nlre--have-char-syntax)
      (setq nlre--fast-plan-hits (1+ nlre--fast-plan-hits)
            hit (nlre--syntax-run-plan-match plan string i n caps)))
     (t
      (let* ((top (nlre--seq-nodes (aref compiled 0)))
             (pre (or (and (null nlre--translate-table)
                           (> (length compiled) 6) (aref compiled 6))
                      (and (> (length compiled) 3) (aref compiled 3))))
             (prefix (and (> (length compiled) 4) (aref compiled 4)))
             (lead (and (not prefix) (nlre--leading-lit-char top)))
             (req-atom (and (not prefix) (not lead)
                            (nlre--leading-required-atom top))))
        (when (or prefix lead req-atom)
          (setq nlre--leading-filter-calls (1+ nlre--leading-filter-calls)))
        (cond
         ((and pre (nlre--prefilter-reject-p pre string i n)) nil)
         ;; Anchored by \` (also inside leading groups): only position 0.
         ((and pre (car pre))
          (when (> ng 1) (nlre--caps-clear caps))
          (let ((e (nlre--match-list top string 0 n)))
            (when e
              (aset caps 0 (cons 0 e))
              (setq hit 0))))
         (prefix
          (let ((m (length (aref prefix 1))))
            (while (and (not hit) (<= (+ i m) limit))
              (let ((candidate (nlre--literal-plan-match prefix string i limit)))
                (if (or (null candidate) (> (+ candidate m) limit))
                    (setq i (1+ limit))
                  (setq i candidate)
                  (when (> ng 1) (nlre--caps-clear caps))
                  (let ((e (nlre--match-candidate top string i n)))
                    (if e
                        (progn (aset caps 0 (cons i e)) (setq hit i))
                      (setq i (1+ i)))))))))
         (lead
          (while (and (not hit) (< i limit))
            (if (not (nlre--literal-char-equal-p
                      lead nlre--pattern-multibyte
                      (aref string i) (multibyte-string-p string)))
                (setq i (1+ i))
              (when (> ng 1) (nlre--caps-clear caps))
              (let ((e (nlre--match-candidate top string i n)))
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
                           (nlre--literal-char-equal-p
                            (nth 1 req-atom) nlre--pattern-multibyte
                            c (multibyte-string-p string)))
                          ((eq tag :any) (not (eq (nlre--fold-char c) ?\n)))
                          ((eq tag :set) (nlre--set-node-match
                                         req-atom c (not (multibyte-string-p string))))
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
                  (let ((e (nlre--match-candidate top string i n)))
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
                  (let ((e (nlre--match-candidate top string i n)))
                    (if e
                        (progn (aset caps 0 (cons i e)) (setq hit i))
                      (setq i (1+ i)))))
              ;; Jump to the position after the next newline (or past LIMIT).
              (let ((nl (nlre--literal-search "\n" string i)))
                (setq i (if nl (1+ nl) (1+ limit)))))))
         (t
          (let ((fmap (and (null nlre--translate-table)
                           (> (length compiled) 8) (aref compiled 8))))
            (while (and (not hit) (<= i limit))
              (if (and fmap (< i n)
                       (let ((fc (aref string i)))
                         (and (< fc 128) (eq (aref fmap fc) 0))))
                  (setq i (1+ i))
                (when (> ng 1) (nlre--caps-clear caps))
                (let ((e (nlre--match-candidate top string i n)))
                  (if e
                      (progn (aset caps 0 (cons i e)) (setq hit i))
                    (setq i (1+ i))))))))))))
    (when hit
      (setq nlre--last-caps caps)
      hit)))))

(defun nlre-match-beginning (n)
  (unless (fixnump n)
    (signal 'wrong-type-argument (list 'fixnump n)))
  (when (< n 0) (signal 'args-out-of-range (list n 0)))
  (and nlre--last-caps (< n (length nlre--last-caps))
       (let ((c (aref nlre--last-caps n))) (and c (car c)))))
(defun nlre-match-end (n)
  (unless (fixnump n)
    (signal 'wrong-type-argument (list 'fixnump n)))
  (when (< n 0) (signal 'args-out-of-range (list n 0)))
  (and nlre--last-caps (< n (length nlre--last-caps))
       (let ((c (aref nlre--last-caps n))) (and c (cdr c)))))

(defun nlre--reuse-list (reuse data)
  (if (not (consp reuse)) data
    (let ((cell reuse) (values data) previous)
      (while values
        (if (consp cell)
            (progn
              (setcar cell (car values))
              (setq previous cell cell (cdr cell)))
          (setcdr previous (copy-sequence values))
          (setq cell nil values nil))
        (when values (setq values (cdr values))))
      (when previous (setcdr previous nil))
      reuse)))

(defun nlre-match-data (&optional integers reuse reseat)
  "Return flattened match data for the last NeLisp regexp match."
  (when nlre--last-caps
    ;; GNU RESEAT: the markers already in REUSE are detached, so they leave
    ;; the buffer's marker chain at once instead of at the next collection.
    (when (and reseat (consp reuse))
      (let ((cell reuse))
        (while (consp cell)
          (when (markerp (car cell)) (set-marker (car cell) nil))
          (setq cell (cdr cell)))))
    (let ((i 0) (n (length nlre--last-caps)) (out nil) (reuse-values reuse))
      ;; GNU omits unmatched groups beyond the last participating group.
      (while (and (> n 1) (null (aref nlre--last-caps (1- n))))
        (setq n (1- n)))
      (while (< i n)
        (let ((span (aref nlre--last-caps i)))
          (dolist (pos (if span (list (car span) (cdr span)) '(nil nil)))
            (let ((value pos))
              (when (and pos nlre--last-buffer (not integers))
                (setq value (set-marker (make-marker) pos nlre--last-buffer)))
              (setq out (cons value out)
                    reuse-values (and (consp reuse-values) (cdr reuse-values))))))
        (setq i (1+ i)))
      (setq out (nreverse out))
      (when (and integers nlre--last-buffer)
        (setq out (append out (list nlre--last-buffer))))
      (nlre--reuse-list reuse out))))

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
;; GNU compiles these buffer primitives to opcodes the standalone VM does not
;; admit, which demoted every function using them to the interpreter (about
;; 14 us per call).  Calling them through variables keeps the callers byte-code.
(defvar nlre--point-function #'point)
(defvar nlre--point-min-function #'point-min)
(defvar nlre--point-max-function #'point-max)
(defvar nlre--goto-char-function #'goto-char)
(defvar nlre--buffer-size-function #'buffer-size)
(defvar nlre--buffer-substring-function #'buffer-substring)
(defun nlre--buffer-narrowed-p ()
  "Non-nil if the current buffer's accessible range is not the whole
buffer."
  (or (/= (funcall nlre--point-min-function) 1) (/= (funcall nlre--point-max-function) (1+ (buffer-size)))))

;; `looking-at' needs a match starting exactly at point, so it tries that one
;; position (GNU's looking_at_1 calls re_match_2 once) instead of scanning
;; forward and discarding any later match.
(defun nlre--match-at (regexp string pos max-end)
  "Return the capture vector of REGEXP matching STRING exactly at POS, or nil.
MAX-END bounds match consumption; anchors still see all of STRING."
  (setq nlre--syntax-table nil)
  (let* ((nlre--fold case-fold-search)
         (nlre--translate-table
          (and case-fold-search (nlre--search-translate-table regexp string)))
         (compiled (nlre--compiled-pattern regexp))
         (nlre--pattern-multibyte (aref compiled 5))
         (n (length string))
         (limit (max 0 (min n max-end)))
         (nlre--match-end-limit limit)
         (caps (make-vector (aref compiled 1) nil))
         (nlre--caps caps)
         (plan (aref compiled 2))
         (end (if (and plan (eq (aref plan 0) :setrun)
                       (null nlre--translate-table))
                  (nlre--setrun-at plan string pos limit)
                (nlre--match-candidate (nlre--seq-nodes (aref compiled 0))
                                       string pos n))))
    (when end
      (aset caps 0 (cons pos end))
      caps)))

(defvar nlre--window-first 256
  "Characters of buffer text a windowed search first copies past its start.")
(defvar nlre--window-growth 8
  "Factor by which a windowed search widens its window after a miss.")

;; GNU searches the gap buffer in place (search.c, re_search_2 over the two
;; text segments), so a search costs the distance it scans.  The buffer here
;; is a pair of immutable strings, and a whole-buffer copy per call (once any
;; edit has invalidated the memoized one) made every search O(buffer).  The
;; windowed searches below hand the matcher only the text from one character
;; before the start (for `^' and `\b') to one character past a ceiling CAP (for
;; `$' and `\>'), and widen CAP geometrically while the answer might depend on
;; text beyond it.  A hit is final only when the matcher never declined text
;; past the ceiling (`nlre--cap-refused') and did not end on it; a miss below
;; the real limit is retried.  The last window ends at the real limit, where
;; this is the exact whole-text search of before.
(defun nlre--window-search (regexp base limit pmin pmax first at-point)
  "Search the buffer for REGEXP from BASE; match ends no later than LIMIT.
PMIN and PMAX bound the accessible text.  With AT-POINT only a match
starting at BASE counts (`looking-at').  FIRST is the first window's span.
On success `nlre--last-caps' holds buffer positions and the result is
non-nil; the caller owns `nlre--last-buffer' and restoring match data."
  (let* ((wb (max pmin (1- base)))
         (span first)
         (cap (min limit (+ base span)))
         (result nil) (done nil))
    (while (not done)
      (let* ((last (>= cap limit))
             (text (funcall nlre--buffer-substring-function wb (min pmax (1+ cap))))
             (nlre--cap-track (not last))
             (nlre--cap-refused nil)
             (hit (if at-point
                      (let ((caps (nlre--match-at regexp text (- base wb) (- cap wb))))
                        (when caps (setq nlre--last-caps caps) t))
                    (nlre-string-match regexp text (- base wb) (- cap wb)))))
        (if (and hit
                 (or last
                     (and (not nlre--cap-refused)
                          (< (nlre-match-end 0) (- cap wb)))))
            (setq result t done t)
          (if last
              (setq done t)
            (setq span (* span nlre--window-growth)
                  cap (min limit (+ base span)))))))
    (when result (nlre--caps-offset wb))
    result))

(defun nlre--looking-at (regexp &optional inhibit-modify)
  (if inhibit-modify
      (let ((saved nlre--last-caps) (saved-buffer nlre--last-buffer))
        (prog1 (nlre--looking-at-1 regexp)
          (setq nlre--last-caps saved nlre--last-buffer saved-buffer)))
    (nlre--looking-at-1 regexp)))

(defun nlre--looking-at-1 (regexp)
  (let ((r (if nlre--nat-have (nlre--nat-looking-at regexp) t)))
    (cond ((eq r t) (nlre--looking-at-lisp regexp))
          ((eq r 'yes) t)
          (t nil))))

(defun nlre--looking-at-lisp (regexp)
  (let* ((nlre--inhibit-search-fastmap t) (old-caps nlre--last-caps)
         (old-buffer nlre--last-buffer)
         (base (funcall nlre--point-function))
         (pmax (funcall nlre--point-max-function))
         (pmin (funcall nlre--point-min-function)))
    (if (nlre--window-search regexp base pmax pmin pmax 128 t)
        (progn
          (setq nlre--last-buffer (funcall nlre--current-buffer-function))
          t)
      (setq nlre--last-caps old-caps
            nlre--last-buffer old-buffer)
      nil)))

(defvar nlre--bsr-memo nil
  "(REGEXP TRIVIAL . REVERSED-LITERAL-CHARS) for the last regexp analysed.")

(defun nlre--bsr-analyze (regexp)
  (let ((position 0) (size (length regexp)) (trivial t) literal)
    (while (and trivial (< position size))
      (let ((character (aref regexp position)))
        (cond ((memq character '(?. ?* ?+ ?? ?\[ ?^ ?$))
               (setq trivial nil))
              ((= character ?\\)
               (setq position (1+ position))
               (when (or (>= position size)
                         (memq (aref regexp position)
                               '(?| ?\( ?\) ?` ?' ?b ?B ?< ?> ?w ?W ?s ?S
                                 ?= ?{ ?} ?_ ?c ?C ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)))
                 (setq trivial nil))
               (when trivial (setq character (aref regexp position)))))
        (when trivial (push character literal)))
      (setq position (1+ position)))
    (cons (copy-sequence regexp) (cons trivial literal))))

(defun nlre--buffer-search-regexp (regexp)
  "Adapt GNU's trivial-regexp buffer entry to the buffer's byte kind.
GNU search.c:1109-1137,1315-1343,1533 routes this case through the
non-regexp searcher, which converts the pattern before case translation.
The scan of REGEXP is memoized: it depends only on the pattern's text."
  (if (or (not (stringp regexp))
          (and (boundp 'search-spaces-regexp) search-spaces-regexp))
      regexp
    (let ((memo nlre--bsr-memo))
      (unless (and memo (string= regexp (car memo)))
        (setq memo (nlre--bsr-analyze regexp) nlre--bsr-memo memo))
      (if (not (car (cdr memo)))
          regexp
        (let ((multibyte (if (fboundp 'nelisp--buffer-multibyte-p)
                             (nelisp--buffer-multibyte-p (funcall nlre--current-buffer-function))
                           enable-multibyte-characters)))
          (if (eq (multibyte-string-p regexp) multibyte) regexp
            ;; GNU's non-regexp path unquotes before byte conversion.  Quote
            ;; its result because this adapter then enters the regexp engine.
            (let* ((text (apply #'string (reverse (cdr (cdr memo)))))
                   (converted (if multibyte (string-to-multibyte text)
                                (apply #'unibyte-string
                                       (mapcar (lambda (character) (logand character 255))
                                               (append text nil)))))
                   (quoted (regexp-quote converted)))
              (if multibyte (string-to-multibyte quoted)
                (apply #'unibyte-string (append quoted nil))))))))))

(defun nlre--re-search-forward (regexp &optional bound noerror count)
  (let ((r (if (and nlre--nat-have (or (null count) (eql count 1)))
               (nlre--nat-bsearch regexp bound noerror nil)
             t)))
    (if (eq r t) (nlre--re-search-forward-lisp regexp bound noerror count) r)))

(defun nlre--re-search-forward-lisp (regexp &optional bound noerror count)
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
      (funcall nlre--point-function)
    ;; 2) BOUND on the wrong side of point signalled nothing at all here
    ;;    (`re-search-backward' already has the mirror-image check for
    ;;    its own direction) -- real Emacs signals plain `error'
    ;;    "Invalid search bound (wrong side of point)" UNCONDITIONALLY,
    ;;    even under NOERROR t (NOERROR only covers a search that fails
    ;;    to match, not an invalid BOUND).  A BOUND past `point-max' is
    ;;    fine (Emacs just clamps it via `point-max' below, same as this
    ;;    function already did before this comment).
    ;; Each buffer primitive is a call into the buffer layer, so fetch point
    ;; and the extent once and track the search start locally between
    ;; repetitions instead of re-reading point after every goto-char.
    (when (markerp bound) (setq bound (marker-position bound)))
    (when (and bound (not (integerp bound)))
      (signal 'wrong-type-argument (list 'integer-or-marker-p bound)))
    (let* ((origin (funcall nlre--point-function))
           (pmax (funcall nlre--point-max-function))
           (pmin (funcall nlre--point-min-function))
           (left (or count 1)) (limit (if bound (min bound pmax) pmax)) result
           (found nil) (good nil)
           (base origin)
           (prepared-regexp (nlre--buffer-search-regexp regexp))
           (old-caps nlre--last-caps) (old-buffer nlre--last-buffer))
      (when (and bound (< bound origin))
        (signal 'error (list "Invalid search bound (wrong side of point)")))
      (while (> left 0)
        (if (null (nlre--window-search prepared-regexp base limit pmin pmax
                                       nlre--window-first nil))
            (setq left 0 result nil)
          (setq nlre--last-buffer (funcall nlre--current-buffer-function))
          (setq found t good nlre--last-caps)
          (setq base (nlre-match-end 0) left (1- left) result base)
          ;; Another repetition follows: leave point where the search
          ;; would have, as before.
          (when (> left 0) (funcall nlre--goto-char-function base))))
      (if result
          (progn (funcall nlre--goto-char-function result) result)
        ;; GNU leaves the match data of the last repetition that succeeded
        ;; when a later one fails (COUNT above 1); none succeeding leaves it.
        (setq nlre--last-caps (if found good old-caps)
              nlre--last-buffer (if found nlre--last-buffer old-buffer))
        (if noerror
            (progn (unless (eq noerror t) (funcall nlre--goto-char-function limit)) nil)
          (signal 'search-failed (list regexp)))))))

(defun nlre--regex-last-match-before (regexp string lower max-end &optional from)
  "Return the rightmost (START . CAPS) match in STRING.
LOWER is the minimum candidate start and MAX-END bounds match consumption.
The full STRING remains visible to anchors and boundary assertions.
FROM, default MAX-END, is the first candidate start tried."
  (let* ((nlre--syntax-table nil)
         (nlre--fold case-fold-search)
         (nlre--translate-table (and case-fold-search
                                      (nlre--search-translate-table regexp string)))
         (compiled (nlre--compiled-pattern regexp))
         (nlre--pattern-multibyte (aref compiled 5))
         (nlre--match-end-limit max-end)
         (n (length string))
         (ng (aref compiled 1))
         (nodes (nlre--seq-nodes (aref compiled 0)))
         (caps (make-vector ng nil))
         (pos (or from max-end))
         (result nil))
    (setq nlre--caps caps)
    (while (and (null result) (>= pos lower))
      (when (> ng 1) (nlre--caps-clear caps))
      (let ((end (nlre--match-candidate nodes string pos n)))
        (when end
          (aset caps 0 (cons pos end))
          (setq result (cons pos caps))))
      (setq pos (1- pos)))
    result))

(defun nlre--compiled-match-at-ending (compiled string start end)
  "Return capture vector when COMPILED matches STRING from START to END."
  (let* ((nlre--syntax-table nil)
         (nlre--fold case-fold-search)
         (nlre--translate-table (and case-fold-search
                                      (nlre--current-translate-table)))
         (nlre--pattern-multibyte (aref compiled 5))
         (nlre--match-end-limit end)
         (nodes (nlre--seq-nodes (aref compiled 0)))
         (caps (make-vector (aref compiled 1) nil))
         (nlre--caps caps)
         (match-end (nlre--match-candidate nodes string start (length string))))
    (when (and match-end (= match-end end))
      (aset caps 0 (cons start end))
      caps)))

(defun nlre--looking-back (regexp &optional limit greedy)
  "Return non-nil when REGEXP matches text ending at point.
LIMIT bounds the initial backward scan.  GREEDY then extends a successful
match one starting character at a time, as GNU Emacs 31.1 does."
  (when (and limit (markerp limit)) (setq limit (marker-position limit)))
  (when (and limit (not (integerp limit)))
    (signal 'wrong-type-argument (list 'integer-or-marker-p limit)))
  (let* ((origin (funcall nlre--point-function))
         (base (funcall nlre--point-min-function))
         (buffer-end (funcall nlre--point-max-function)))
    (when (and limit (> limit origin))
      (error "Invalid search bound (wrong side of point)"))
    (let* ((narrowed (nlre--buffer-narrowed-p))
           (text (if narrowed (funcall nlre--buffer-substring-function base buffer-end)
                   (buffer-string)))
           (off (if narrowed base 1))
           (target (- origin off))
           (lower (- (max base (or limit base)) off))
           (compiled (nlre--compiled-pattern regexp))
           (pos target)
           caps)
      ;; Backward search considers the greatest starting position first.
      (while (and (not caps) (>= pos lower))
        (setq caps (nlre--compiled-match-at-ending compiled text pos target))
        (unless caps (setq pos (1- pos))))
      ;; GNU's GREEDY loop tests only the immediately preceding start.  A
      ;; failed one-character extension stops, even if an earlier start could
      ;; independently match, and LIMIT no longer constrains this extension.
      (when (and greedy caps)
        (let ((nlre--inhibit-search-fastmap t) extended)
          (while (and (> pos 0)
                      (setq extended
                            (nlre--compiled-match-at-ending
                             compiled text (1- pos) target)))
            (setq pos (1- pos) caps extended))))
      (when caps
        (setq nlre--last-caps caps)
        (nlre--caps-offset off)
        (setq nlre--last-buffer (funcall nlre--current-buffer-function))
        t))))

(defun nlre--window-search-backward (regexp cursor lower pmin pmax)
  "Return the start of the rightmost match of REGEXP at or before CURSOR.
The match must start at or after LOWER and end at or before CURSOR; PMIN and
PMAX bound the accessible text.  `nlre--last-caps' then holds the buffer
positions of the match.  Returns nil, leaving the caps unspecified, if none."
  (let* ((we (min pmax (1+ cursor)))
         (span nlre--window-first)
         (hi cursor) (result nil) (done nil))
    (while (not done)
      (let* ((lo (max lower (- hi span)))
             (wb (max pmin (1- lo)))
             (text (funcall nlre--buffer-substring-function wb we))
             (found (nlre--regex-last-match-before
                     regexp text (- lo wb) (- cursor wb) (- hi wb))))
        (cond
         (found
          (setq nlre--last-caps (cdr found))
          (nlre--caps-offset wb)
          (setq result (+ wb (car found)) done t))
         ((<= lo lower) (setq done t))
         (t (setq hi (1- lo)
                  span (* span nlre--window-growth))))))
    result))

(defun nlre--re-search-backward (regexp &optional bound noerror count)
  "Search backward for REGEXP, preserving full-buffer anchor context.
This uses `nlre-string-match' backtracking with a separate match-end ceiling."
  (setq count (or count 1))
  (unless (integerp count)
    (signal 'wrong-type-argument (list 'integerp count)))
  (if (= count 0)
      (funcall nlre--point-function)
    (when (and bound (markerp bound))
      (setq bound (marker-position bound)))
    (when (and bound (not (integerp bound)))
      (signal 'wrong-type-argument (list 'integer-or-marker-p bound)))
    (let* ((origin (funcall nlre--point-function))
           (base (funcall nlre--point-min-function))
           (end (funcall nlre--point-max-function))
           (direction (if (< count 0) 1 -1))
           (limit (if bound
                      (progn
                        (when (if (> direction 0) (< bound origin) (> bound origin))
                          (error "Invalid search bound (wrong side of point)"))
                        (max base (min end bound)))
                    (if (> direction 0) end base))))
      (if (> direction 0)
          (nlre--re-search-forward regexp limit noerror (- 0 count))
        ;; Candidates run from the starting point down to LIMIT, and a match
        ;; may not extend past the starting point, so the window ends one
        ;; character past it and grows leftward as the scan goes (see
        ;; `nlre--window-search').
        (let* ((prepared-regexp (nlre--buffer-search-regexp regexp))
               (old-caps nlre--last-caps)
               (old-buffer nlre--last-buffer)
               (cursor origin)
               (remaining count)
               (failed nil))
          (while (and (> remaining 0) (not failed))
            (let ((start (nlre--window-search-backward
                          prepared-regexp cursor limit base end)))
              (if start
                  (setq cursor start
                        remaining (1- remaining))
                (setq failed t))))
          (if failed
              (progn
                (when (= remaining count)
                  (setq nlre--last-caps old-caps
                        nlre--last-buffer old-buffer))
                (when (< remaining count)
                  (setq nlre--last-buffer (funcall nlre--current-buffer-function)))
                (if noerror
                    (progn (unless (eq noerror t) (funcall nlre--goto-char-function limit)) nil)
                  (signal 'search-failed (list regexp))))
            (setq nlre--last-buffer (funcall nlre--current-buffer-function))
            (funcall nlre--goto-char-function cursor)))))))

;; Public entry points defined here as top-level defuns so the build admits
;; them as byte-code; the build's prelude used to define them as interpreted
;; source (about 14 us per call, a large share of a short `string-match').
;; The callers install them with `fset' under the public names.
(defun nlre--string-match-entry (re s &optional start inhibit-modify)
  (unless (stringp re) (signal 'wrong-type-argument (list 'stringp re)))
  (unless (stringp s) (signal 'wrong-type-argument (list 'stringp s)))
  (when start
    (unless (integerp start)
      (signal 'wrong-type-argument (list 'fixnump start)))
    (when (or (< start (- 0 (length s))) (> start (length s)))
      (signal 'args-out-of-range (list s start)))
    (when (< start 0) (setq start (+ (length s) start))))
  (if inhibit-modify
      ;; `nlre-string-match' stores its captures only after a complete match,
      ;; so a signal leaves them untouched; skipping `unwind-protect' also
      ;; keeps this function on the VM's fast frame path.
      (let ((saved nlre--last-caps) (saved-buffer nlre--last-buffer))
        (prog1 (nlre-string-match re s start)
          (setq nlre--last-caps saved nlre--last-buffer saved-buffer)))
    (prog1 (nlre-string-match re s start)
      (setq nlre--last-buffer nil))))

(defun nlre--string-match-p-entry (re s &optional start)
  (unless (stringp re) (signal 'wrong-type-argument (list 'stringp re)))
  (unless (stringp s) (signal 'wrong-type-argument (list 'stringp s)))
  (when start
    (unless (integerp start)
      (signal 'wrong-type-argument (list 'fixnump start)))
    (when (or (< start (- 0 (length s))) (> start (length s)))
      (signal 'args-out-of-range (list s start)))
    (when (< start 0) (setq start (+ (length s) start))))
  (let ((saved nlre--last-caps) (saved-buffer nlre--last-buffer))
    (prog1 (nlre-string-match re s start)
      (setq nlre--last-caps saved nlre--last-buffer saved-buffer))))

(defun nlre--match-string-entry (n &optional str)
  (let ((b (nlre-match-beginning n)) (e (nlre-match-end n)))
    (if (and b e)
        (if str (substring str b e)
          (funcall nlre--buffer-substring-function b e))
      nil)))

;; A case-sensitive search for ASCII text needs no matcher: the first
;; occurrence wholly inside a window is the first occurrence.  Windows are cut
;; from the buffer like `nlre--window-search's, and consecutive ones overlap
;; by the needle length less one so every start is tried once.
(defun nlre--search-forward-window (string bound noerror)
  (when (markerp bound) (setq bound (marker-position bound)))
  (when (and bound (not (integerp bound)))
    (signal 'wrong-type-argument (list 'integer-or-marker-p bound)))
  (let* ((pt (funcall nlre--point-function))
         (pmax (funcall nlre--point-max-function))
         (lim (if bound (min bound pmax) pmax))
         (m (length string))
         (from pt)
         (span (max nlre--window-first (* 2 m)))
         (hit nil))
    (when (< lim pt) (error "Invalid search bound (wrong side of point)"))
    (while (and (not hit) (<= (+ from m) lim))
      (let* ((cap (min lim (+ from span)))
             (idx (nlre--literal-search
                   string (funcall nlre--buffer-substring-function from cap) 0)))
        (if idx
            (setq hit (+ from idx))
          (setq from (max (1+ from) (- cap (1- m)))
                span (* span nlre--window-growth)))))
    (cond (hit
           (setq nlre--last-caps (vector (cons hit (+ hit m)))
                 nlre--last-buffer (funcall nlre--current-buffer-function))
           (funcall nlre--goto-char-function (+ hit m))
           (+ hit m))
          ((null noerror) (signal 'search-failed (list string)))
          ((eq noerror t) nil)
          (t (funcall nlre--goto-char-function lim) nil))))

;; `search-forward' is an interpreted closure in the standalone prelude (it sits
;; under `unless fboundp', which the byte-code pass does not see).  Defined here
;; so the build admits it.  Installed only where the prelude's literal-search
;; helper exists.  Case-folded searches take the matcher's folding literal plan.
(defun nlre--search-forward-entry (string &optional bound noerror count)
  (let ((r (if (and nlre--nat-have (or (null count) (eql count 1)))
               (nlre--nat-bsearch string bound noerror t)
             t)))
    (if (eq r t) (nlre--search-forward-entry-lisp string bound noerror count) r)))

(defun nlre--search-forward-entry-lisp (string &optional bound noerror count)
  (if (and (stringp string) (or (null count) (eql count 1))
           (> (length string) 0)
           (not case-fold-search)
           (nlre--ascii-text-p string))
      (nlre--search-forward-window string bound noerror)
    (if (and (integerp count) (< count 0))
        (nlre--re-search-backward (regexp-quote string) bound noerror (- count))
      (nlre--re-search-forward (regexp-quote string) bound noerror count))))

;; `set-match-data' was missing from the standalone image (survey2 PARITY:
;; `save-match-data' failed with void-function).  LIST is a flat list of
;; integers or markers in `match-data' order, optionally ending in a buffer.
(defun nlre--set-match-data-entry (list &optional reseat)
  (let ((caps nil) (buffer nil) (items list))
    (while (consp items)
      (let ((b (car items)) (e (car (cdr items))))
        (if (bufferp b)
            (setq buffer b items nil)
          (when (markerp b)
            (setq buffer (or buffer (marker-buffer b)) b (marker-position b)))
          (when (markerp e) (setq e (marker-position e)))
          (setq caps (cons (and b e (cons b e)) caps)
                items (cdr (cdr items))))))
    (setq nlre--last-caps (and caps (vconcat (nreverse caps)))
          nlre--last-buffer buffer)
    ;; GNU RESEAT: detach every marker of LIST and overwrite its cell with nil
    ;; (so an 'evaporate caller's markers are unchained at once, not at the
    ;; next collection).
    (when reseat
      (let ((cell list))
        (while (consp cell)
          (when (markerp (car cell))
            (set-marker (car cell) nil)
            (setcar cell nil))
          (setq cell (cdr cell)))))
    nil))

(unless (fboundp 'looking-at)
  (fset 'looking-at (symbol-function 'nlre--looking-at)))
(unless (fboundp 'looking-at-p)
  (defun looking-at-p (regexp)
    "Return non-nil if text after point matches REGEXP, leaving match data alone."
    (nlre--looking-at regexp t)))
(unless (fboundp 're-search-forward)
  (fset 're-search-forward (symbol-function 'nlre--re-search-forward)))
(unless (fboundp 're-search-backward)
  (fset 're-search-backward (symbol-function 'nlre--re-search-backward)))
(unless (fboundp 'set-match-data)
  (fset 'set-match-data (symbol-function 'nlre--set-match-data-entry)))
(when (fboundp 'nelisp--search-forward-literal)
  (fset 'search-forward (symbol-function 'nlre--search-forward-entry)))

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
        ;; Two steps: the byte compiler would fuse nested concats into the
        ;; four-argument opcode the standalone VM does not admit.
        (setq out (concat out (substring string pos rb)))
        (setq out (concat out piece (substring string re me)))
        (cond
         ((= me mb)
          (if (>= mb len) (setq cont nil)
            (setq out (concat out (substring string mb (1+ mb))))
            (setq pos (1+ mb))))
         (t (setq pos me)))))
    (concat out (substring string (min pos len) len))))

;; Source reload must discard entries with the former signature slot layout.
(nlre--compiled-cache-clear)

(provide 'nelisp-stdlib-regexp)
;;; nelisp-stdlib-regexp.el ends here
