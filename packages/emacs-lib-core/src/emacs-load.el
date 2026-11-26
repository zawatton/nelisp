;;; emacs-load.el --- Standalone load/load-file override  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Provenance: copied from
;; /home/madblack-21/Cowork/Notes/dev/nelisp/lisp/nelisp-stdlib-misc.el
;; lines 495-580.  Keep this file in sync with that donor block.
;;
;; Local delta: keep the override standalone-only in this repository.

;; Shim audit 2026-09-29: intentionally shadows native NeLisp definitions -- load/load-file implement the library artifact-cache load layer.
;;; Code:

;; The load-time rewriter turns `defalias' into `nelisp-defalias-late',
;; which `emacs-eval' defines.
(unless (fboundp 'nelisp-defalias-late)
  (require 'emacs-eval nil t))

(declare-function nelisp--syscall-stat-field "nelisp-runtime" (path offset))
(defvar user-init-file nil)
(declare-function nelisp--syscall-path-int "nelisp-runtime"
                  (number path integer))

(declare-function nelisp--eval-source-string "nelisp-standalone"
                  (source &optional compiled-start))
(defvar nelisp--lh-file)
(defvar nelisp--lh-parked)
(defvar nelisp--lh-seg)
(declare-function nelisp--lh-record "nelisp-standalone" (file entries))

(declare-function nelisp--rd-read-one "nelisp-stdlib-prelude" (string start end))
(declare-function nelisp--read-all-from-string-native "nelisp-standalone"
                  (source &optional start end encoding))
(declare-function nelisp--load-external-utf8-p "nelisp-standalone" (source))
(declare-function emacs-load--elc-decode "emacs-load" (bytes))
(declare-function emacs-load--elc-read-slice "emacs-load" (source start end))
(declare-function emacs-load--elc-read-fallback "emacs-load"
                  (source start end native-end))
(declare-function emacs-load--elc-skip-blocks "emacs-load" (source position limit))
(declare-function emacs-load--elc-doc-start-p "emacs-load" (source pos))
(declare-function emacs-load--elc-unquote-doc "emacs-load" (source start end))

(when (or (fboundp 'rdf)
          (fboundp 'nelisp--eval-source-string)
          (not (boundp 'emacs-version))
          (not (stringp emacs-version)))
  (defvar load-garbage-collect-interval 64
    "Number of forms between opportunistic GC checks in `load'.
Nil or 0 disables the periodic check.  When the standalone collector
reports pressure, collection also requires its current budget to be due.")

  (defvar emacs-load-large-source-threshold 32768
    "Byte threshold above which source loads use the incremental evaluator.
Nil or a non-positive value disables the size-based routing and preserves
the historical hybrid path for all source loads that can use it.")

  ;; GNU lread.c preloads this special variable before Fload binds it.
  (defvar load-true-file-name nil
    "The actual file currently being loaded, or nil outside a load.")
  (defvar load-in-progress nil
    "Non-nil while the loader is reading and evaluating a file.")

  (defvar emacs-load--alias-candidates nil
    "Symbolic aliases encountered by the standalone source loader.
Only currently active macro aliases affect source routing.  Ordinary
aliases and names subsequently redefined as functions remain unchanged.")

  (defun emacs-load--symbolic-macro-alias-p (name)
    "Return non-nil when NAME currently aliases a macro by symbol."
    (when (symbolp name)
      (let ((target (symbol-function name)))
        (and target (symbolp target) (macrop target)))))

  (defun emacs-load--macro-alias-source-p (source)
    "Return non-nil when SOURCE may call a loaded symbolic macro alias."
    (let ((names emacs-load--alias-candidates) found)
      (while (and names (not found))
        (let ((name (pop names)))
          (when (and (emacs-load--artifact-string-search
                      (symbol-name name) source 0)
                     (emacs-load--symbolic-macro-alias-p name))
            (setq found t))))
      found))

  (defvar nelisp--cc-replay-file nil
    "Base name (sans extension) of the CC Mode file currently being loaded.
The outer `(setq load-file-name RESOLVED)' in `nelisp--load-resolved-file'
is not visible to the interpreted elisp running inside the native source
evaluator, so `c-get-current-file' cannot recover the per-file identity of
each cc-*.el file from `load-file-name'.  A marker `setq' spliced at the
head of each cc-*.el source establishes this variable inside the same
eval-source-string, which the redefined `c-get-current-file' consults.")

  (defvar nelisp--cc-replay-file-stack nil
    "Saved `nelisp--cc-replay-file' values across nested cc-*.el loads.
A cc-*.el file may `require' another (e.g. cc-fonts -> cc-langs); the head
splice pushes the enclosing marker here and the tail splice pops it back so
the outer file's identity is restored after the nested load returns.")

  (defun emacs-load--byte-indexed-runtime-p ()
    "Return non-nil when the standalone loader should scan byte offsets."
    (or (fboundp 'rdf)
        (fboundp 'nelisp--eval-source-string)
        (not (boundp 'emacs-version))
        (not (stringp (and (boundp 'emacs-version) emacs-version)))))

  (defun emacs-load--byte-indexed-source (source)
    "Return SOURCE in the representation preferred by standalone scanners.
Standalone NeLisp indexes unibyte strings directly by byte.  Host Emacs keeps
its normal multibyte representation because its character indexing is already
constant time."
    (if (and (emacs-load--byte-indexed-runtime-p)
             (fboundp 'multibyte-string-p)
             (multibyte-string-p source))
        (string-as-unibyte source)
      source))

  (defun emacs-load--reader-slice (source start end)
    "Return SOURCE between byte positions START and END for an Elisp reader.
Byte-indexed standalone sources are converted back to multibyte only after the
slice has been isolated."
    (let ((slice (substring source start end)))
      (if (and (emacs-load--byte-indexed-runtime-p)
               (fboundp 'multibyte-string-p)
               (not (multibyte-string-p slice)))
          (string-as-multibyte slice)
        slice)))

  (defconst emacs-load--propertized-string-literal-marker
    "(nelisp-emacs--propertized-string-literal "
    "Replacement text for the two characters `#(' (see
`emacs-load--rewrite-propertized-string-literals').  One open paren, same
nesting depth as the `(' it replaces, so every paren-depth-based scanner in
this file (form-boundary, container-end, comment/whitespace skipping) keeps
working unmodified on the rewritten text.")

  (defun emacs-load--marker-search-state (source start markers)
    "Return mutable next-position state for MARKERS in SOURCE after START."
    (let ((state nil))
      (dolist (marker markers)
        (push (cons marker
                    (emacs-load--artifact-string-search marker source start))
              state))
      state))

  (defun emacs-load--next-marker-position (source i len state)
    "Return the smallest cached marker position >= I in SOURCE, or LEN.
STATE comes from `emacs-load--marker-search-state'.  Only marker positions
that I has passed are searched again, so skipping one comment line normally
costs one native search rather than one search for every marker kind.

T103: `aref' on a string costs on the order of 100 microseconds per
call on this runtime (measured directly; not merely a multibyte-offset
artifact -- confirmed the same order of magnitude on an already-unibyte,
byte-indexed string), so a hand-written scanner that visits every
character of a large file with `aref' is prohibitively slow (tens of
seconds to minutes for a real package file) even though the algorithm
itself is linear.  `emacs-load--artifact-string-search' (native substring
search) costs microseconds regardless of string length (measured:
~3ms for a 200KB haystack). The three `emacs-load--rewrite-*' scanners in
this file use this helper to jump directly between the rare characters
that can start something meaningful to them (a string quote, a comment
semicolon, a backslash escape, and their own specific trigger byte) and
copy every ordinary byte in between through in bulk via `substring',
instead of visiting it one `aref' call at a time."
    (let ((best len))
      (dolist (entry state)
        (let ((pos (cdr entry)))
          (when (and pos (< pos i))
            (setq pos
                  (emacs-load--artifact-string-search (car entry) source i))
            (setcdr entry pos))
          (when (and pos (< pos best))
            (setq best pos))))
      best))

  (defun emacs-load--labeled-object-trigger-p (source)
    "Return non-nil when SOURCE contains a possible `#N=' or `#N#' token.
Checking the ten digit prefixes with native substring search is much cheaper
than entering the Elisp scanner for ordinary `#'', `#(', or hash text in a
comment.  The syntax-aware scanner still decides whether a candidate is code."
    (let ((digit 0)
          (found nil))
      (while (and (< digit 10) (not found))
        (setq found
              (emacs-load--artifact-string-search
               (concat "#" (number-to-string digit)) source 0))
        (setq digit (1+ digit)))
      found))

  (defvar emacs-load--propertized-string-reader-native-p
    (let ((probe "#(\"a\" 0 1 (p t))"))
      (condition-case nil
          (let ((read (read-from-string probe 0 (length probe))))
            (and (consp read)
                 (integerp (cdr read))
                 (= (cdr read) (length probe))
                 (stringp (car read))
                 (equal (car read) "a")))
        (error nil)))
    "Non-nil when this runtime's `read-from-string' already reads Emacs's
`#(STR START END PLIST ...)' propertized-string syntax correctly, computed
once at load time by actually reading a small `#(...)' literal and
checking BOTH that reading raised no error AND that the result is the
right STRING value at the right end position -- not merely that no error
was signaled, since T103's finding was that this runtime's interpreted
fallback reader silently mis-parses `#(' as the bare symbol `#' followed
by a separate list without ever signaling anything, so an error-only probe
would not have caught it.  True on host Emacs (native `#(...)' support).
T105 is expected to add real `#(...)' support to the vendor NeLisp reader;
once available, this probe goes true there too, and
`nelisp--load-resolved-file' skips
`emacs-load--rewrite-propertized-string-literals' entirely (see its call
site) -- the shim is interim and self-disabling, not a permanent fork of
reader behavior.")

  (defvar emacs-load--labeled-object-reader-native-p
    (let ((probe "(#1=upper-left lower-left #1#)"))
      (condition-case nil
          (let ((read (read-from-string probe 0 (length probe))))
            (and (consp read)
                 (integerp (cdr read))
                 (= (cdr read) (length probe))
                 (listp (car read))
                 (= (length (car read)) 3)
                 (eq (nth 0 (car read)) 'upper-left)
                 (eq (nth 2 (car read)) 'upper-left)))
        (error nil)))
    "Non-nil when this runtime's `read-from-string' already reads Emacs's
`#N=OBJECT' / `#N#' labeled/shared-structure syntax correctly, computed
once at load time the same way and for the same reason as
`emacs-load--propertized-string-reader-native-p' (see its docstring):
checking that the read result has the right VALUE, not merely that
reading raised no error, since T103's finding here too was a silent
mis-tokenization (`#1=upper-left' read as one bare symbol literally named
`#1=upper-left', not as a label attached to the symbol `upper-left').
True on host Emacs.  T105 is expected to add real `#N='/`#N#' support to
the vendor NeLisp reader; once available, this probe goes true there too,
and `nelisp--load-resolved-file' skips
`emacs-load--rewrite-labeled-object-references' entirely (see its call
site).")

  (defun emacs-load--rewrite-propertized-string-literals (source)
    "Rewrite `#(...)' propertized-string read syntax in SOURCE to plain calls.
T103: when `emacs-load--propertized-string-reader-native-p' is nil, none of
this runtime's readers reachable from `load' implement Emacs's `#(STR
START END PLIST ...)' read syntax for a propertized string literal.
`read-from-string''s interpreted fallback silently mis-tokenizes it as the
bare symbol `#' immediately followed by an ordinary list -- STR and PLIST
become extra, wrongly-shaped arguments to whatever form contains the
literal, corrupting the parse without signaling anything -- while both the
native whole-string reader (`nelisp--read-all-from-string-native') and
`nelisp--eval-source-string' signal `invalid-read-syntax' on it outright.
Real files carry this syntax (e.g. `evil-common.el': `(overlay-put overlay
\\='after-string #(\"?\" 0 1 (face minibuffer-prompt cursor 1)))'), and
when the literal sits inside a form big or complex enough that the native
per-form reader in `emacs-load--native-read-one' declines to read it,
T78's escape hatch `nelisp--load-eval-source-tail' hands that form whole to
`nelisp--eval-source-string' (or, when the escaped remainder also contains
a `defalias', through `nelisp--load-normalize-source-rewriting''s
`read-from-string'-based reparse first) -- either way turning a
load-bearing but previously silent gap into a whole-file load failure.

Replacing the two-character sequence `#(' with
`emacs-load--propertized-string-literal-marker' turns the construct into
ordinary, fully-supported function-call syntax before any reader ever
sees it: `#(\"?\" 0 1 (face ...))' becomes
`(nelisp-emacs--propertized-string-literal \"?\" 0 1 (face ...))'.
`nelisp-emacs--propertized-string-literal' (below) reconstructs the
equivalent propertized string at eval time.

Ineligible bytes are left untouched: inside a string literal or a comment;
inside a `?X' or `?\\X' character literal (so `?#(' -- the character `#'
followed by a new list, not the start of `#(...)' -- is not mistaken for
propertized-string syntax); and immediately after a backslash outside a
string (an escaped `#' or `(' in a symbol's spelling).  Only a plain,
unescaped, in-code `#(' is rewritten.  This function itself always applies
the transform when asked, regardless of
`emacs-load--propertized-string-reader-native-p' -- callers decide whether
calling it is needed at all (see `nelisp--load-resolved-file').  Returns
SOURCE unchanged (the same string object, no copy) when `#(' does not
occur at all, which is the common case for most files.  Forces SOURCE to
`emacs-load--byte-indexed-source' first (a no-op when already
byte-indexed, which every real `load' caller already is): this function's
own `aref'-based scan must never run on a still-multibyte string, since on
this runtime that means each `aref' recomputes the character's byte offset
by walking from the start of the string -- quadratic in file length, and
the one class of bug this file's other scanners are already documented
\(`nelisp--load-skip-space-and-comments') and tested
\(`emacs-load-test/source-incremental-byte-indexed-source-with-non-ascii-comments-is-fast')
to avoid."
    (setq source (emacs-load--byte-indexed-source source))
    (if (not (emacs-load--artifact-string-search "#(" source 0))
        source
      (let ((len (length source))
            (out nil)
            (start 0)
            (i 0)
            (in-string nil)
            (escaped nil)
            (marker-state
             (emacs-load--marker-search-state
              source 0 '("\"" ";" "\\" "?" "#"))))
        (while (< i len)
          (if in-string
              (progn
                (let ((ch (aref source i)))
                  (cond
                   (escaped (setq escaped nil))
                   ((= ch ?\\) (setq escaped t))
                   ((= ch ?\") (setq in-string nil))))
                (setq i (1+ i)))
            (setq i (emacs-load--next-marker-position source i len marker-state))
            (when (< i len)
              (let ((ch (aref source i)))
                (cond
                 ((= ch ?\\)
                  ;; A backslash outside a string escapes the next
                  ;; character (e.g. an unusual symbol spelling): skip both,
                  ;; unconditionally, so that next character is never
                  ;; re-examined for special meaning.
                  (setq i (min len (+ i 2))))
                 ((= ch ??)
                  ;; Character literal `?X' or `?\X...': skip `?', then the
                  ;; literal character (one more when it is itself a
                  ;; backslash escape), then the base character -- so the
                  ;; whole literal is never re-examined as the start of
                  ;; `#(' syntax.
                  (setq i (1+ i))
                  (when (and (< i len) (= (aref source i) ?\\))
                    (setq i (1+ i)))
                  (setq i (min len (1+ i))))
                 ((= ch ?\;)
                  (let ((nl (emacs-load--artifact-string-search "\n" source i)))
                    (setq i (if nl (1+ nl) len))))
                 ((= ch ?\") (setq in-string t) (setq i (1+ i)))
                 ((and (= ch ?#) (< (1+ i) len)
                       (= (aref source (1+ i)) ?\())
                  (push (substring source start i) out)
                  (push emacs-load--propertized-string-literal-marker out)
                  (setq i (+ i 2))
                  (setq start i))
                 (t (setq i (1+ i))))))))
        (push (substring source start len) out)
        (apply #'concat (nreverse out)))))

  (defun emacs-load--rewrite-labeled-object-references (source)
    "Rewrite `#N=OBJECT' / `#N#' labeled/shared-structure read syntax in
SOURCE, for the case where OBJECT is a single atom (symbol, number,
`nil'/`t', or a double-quoted string), into plain repeated text the same
readers `emacs-load--rewrite-propertized-string-literals' targets can
already parse.  T103: real code uses this syntax not to build a genuinely
circular data structure but as a readability idiom for \"this is the same
value as label N\" -- e.g. `evil-commands.el''s `evil-visual-rotate':
`(let ((corners \\='(#1=upper-left upper-right lower-right lower-left
#1#))) ...)'.  Since `upper-left' is a symbol, and symbols are already
`eq' wherever the same name is written, `#1=upper-left ... #1#' has the
exact same value as writing `upper-left ... upper-left' twice; rewriting
the label away this way is a value-preserving text substitution, not an
approximation.

Like `#(...)', none of this runtime's readers reachable from `load'
implement `#N='/`#N#' at all: `nelisp--read-all-from-string-native'
declines, `nelisp--eval-source-string' signals `invalid-read-syntax', and
`read-from-string''s interpreted fallback silently mis-tokenizes
`#1=upper-left' as one bare symbol whose own name happens to start with
the four characters `#1=' (and `#1#' as another bare symbol named `#1#')
-- wrong regardless (a `memq' for the real `upper-left' symbol against a
list that never actually contains it), though unlike `#(...)' this
mis-tokenization alone does not produce a load-time error; the load-time
error T103 tracked down (`(require \\='evil)' dying on this exact form)
comes from the *other* two readers, reached whenever a big/complex enough
form containing this syntax makes the native per-form reader in
`emacs-load--native-read-one' decline (see
`emacs-load--rewrite-propertized-string-literals' for that escape-hatch
mechanism -- identical here, just a different unsupported construct
tripping the same decline).

`#N=' applied to a list or vector -- true shared/circular compound
structure, which cannot be expressed as an equivalent flat rewrite the way
a duplicated atom can -- is deliberately left untouched, and so is any
later `#N#' for a label this function did not record.  This is a known,
documented limitation, not silently wrong: every `#N='/`#N#' use is
already unreadable by every reader this file uses, so leaving the
compound case alone changes nothing for it.  A label reused for a second,
unrelated `#N=...#N#' pair elsewhere in the same file works correctly as
long as each pair's own definition textually precedes its own reference
(the universal convention in hand-written source, and the shape T103's
regression test and the real evil-commands.el both use); a `#N#' that
precedes its own file-wide-unique `#N=' (a genuine forward reference, only
ever produced by a serializer, never by a human) is left untouched like
the compound case.  Returns SOURCE unchanged (the same string object, no
copy) when `#' does not occur at all, which is the common case for most
files.  Forces SOURCE to `emacs-load--byte-indexed-source' first (a no-op
when already byte-indexed): see
`emacs-load--rewrite-propertized-string-literals''s docstring for why an
`aref'-based scanner must never run on a still-multibyte string on this
runtime."
    (setq source (emacs-load--byte-indexed-source source))
    (if (not (emacs-load--labeled-object-trigger-p source))
        source
      (let ((len (length source))
            (out nil)
            (start 0)
            (i 0)
            (in-string nil)
            (escaped nil)
            (labels (make-hash-table :test 'eql))
            (marker-state
             (emacs-load--marker-search-state
              source 0 '("\"" ";" "\\" "?" "#"))))
        (while (< i len)
          (if in-string
              (progn
                (let ((ch (aref source i)))
                  (cond
                   (escaped (setq escaped nil))
                   ((= ch ?\\) (setq escaped t))
                   ((= ch ?\") (setq in-string nil))))
                (setq i (1+ i)))
            (setq i (emacs-load--next-marker-position source i len marker-state))
            (when (< i len)
              (let ((ch (aref source i)))
                (cond
                 ((= ch ?\\) (setq i (min len (+ i 2))))
                 ((= ch ??)
                  (setq i (1+ i))
                  (when (and (< i len) (= (aref source i) ?\\))
                    (setq i (1+ i)))
                  (setq i (min len (1+ i))))
                 ((= ch ?\;)
                  (let ((nl (emacs-load--artifact-string-search "\n" source i)))
                    (setq i (if nl (1+ nl) len))))
                 ((= ch ?\") (setq in-string t) (setq i (1+ i)))
                 ((and (= ch ?#) (< (1+ i) len)
                       (<= ?0 (aref source (1+ i)) ?9))
                  (let ((digit-start (1+ i))
                        (digit-end nil))
                    (setq digit-end digit-start)
                    (while (and (< digit-end len)
                                (<= ?0 (aref source digit-end) ?9))
                      (setq digit-end (1+ digit-end)))
                    (cond
                     ;; `#N=OBJECT'
                     ((and (< digit-end len) (= (aref source digit-end) ?=))
                      (let* ((label (string-to-number
                                     (substring source digit-start digit-end)))
                             (obj-start (1+ digit-end))
                             (obj-ch (and (< obj-start len)
                                          (aref source obj-start))))
                        (if (memq obj-ch '(?\( ?\[ nil))
                            (setq i (1+ digit-end))
                          (let ((obj-end
                                 (if (= obj-ch ?\")
                                     (emacs-load--artifact-source-string-end
                                      source obj-start)
                                   (emacs-load--artifact-source-atom-end
                                    source obj-start))))
                            (puthash label
                                     (substring source obj-start obj-end)
                                     labels)
                            (push (substring source start i) out)
                            (setq start obj-start)
                            (setq i obj-start)))))
                     ;; `#N#'
                     ((and (< digit-end len) (= (aref source digit-end) ?#))
                      (let* ((label (string-to-number
                                     (substring source digit-start digit-end)))
                             (replacement (gethash label labels)))
                        (if (not replacement)
                            (setq i (1+ digit-end))
                          (push (substring source start i) out)
                          (push replacement out)
                          (setq start (1+ digit-end))
                          (setq i (1+ digit-end)))))
                     (t (setq i digit-end)))))
                 (t (setq i (1+ i))))))))
        (push (substring source start len) out)
        (apply #'concat (nreverse out)))))

  (defvar emacs-load--escaped-modifier-char-literal-reader-native-p
    (let ((probe "?\\C-\\["))
      (condition-case nil
          (= (car (read-from-string probe 0 (length probe))) 27)
        (error nil)))
    "Non-nil when this runtime's `read-from-string' already reads a
modifier-prefixed character literal whose base character is itself a
backslash escape (e.g. `?\\C-\\[', Control applied to the escaped
character `\\[') to the correct value, computed once at load time the
same way and for the same reason as
`emacs-load--propertized-string-reader-native-p' (see its docstring).
T103's finding here is a wrong VALUE, not a read failure or a
mis-tokenization: `?\\C-\\[' is read as 28 on this runtime instead of the
correct 27, because the reader applies the Control mask to the literal
backslash byte instead of first resolving `\\[' to `[' and masking that
-- so the probe checks the computed VALUE, exactly like
`emacs-load--propertized-string-reader-native-p' checks the propertized
string's VALUE rather than merely that reading did not signal.  True on
host Emacs.  T105 is expected to fix the vendor NeLisp reader's escape
resolution; once it does, this probe goes true there too, and
`nelisp--load-resolved-file' skips
`emacs-load--rewrite-escaped-modifier-char-literals' entirely (see its
call site).")

  (defun emacs-load--char-literal-escaped-base (source pos)
    "Return (BASE . NEXT-POS) for the escaped character at POS in SOURCE,
POS pointing at a backslash, resolving the small set of named escapes
`(elisp) Character Type' documents (\\n \\t \\r \\e \\d \\a \\b \\f \\v)
plus the general rule that a backslash followed by any other single
character means that character literally (covers `\\[', `\\]', `\\(',
`\\)', `\\\"', `\\;', `\\\\', `\\ ', etc.).  Return nil -- meaning \"not
resolved, leave untouched\" -- for a numeric or Unicode escape (`\\NNN'
octal, `\\xNN' hex, `\\N{...}' named/Unicode) or when POS is out of
range; those are unambiguously read correctly by every reader this file
targets today (T103 verified `?\\C-e', `?\\C-?', and plain octal/hex
character escapes all already compute the right value), so there is
nothing to fix and no reason to take on the extra parsing risk."
    (let ((len (length source)))
      (when (and (< pos len) (= (aref source pos) ?\\) (< (1+ pos) len))
        (let ((c (aref source (1+ pos))))
          (cond
           ((or (<= ?0 c ?7) (memq c '(?x ?N))) nil)
           ((= c ?n) (cons ?\n (+ pos 2)))
           ((= c ?t) (cons ?\t (+ pos 2)))
           ((= c ?r) (cons ?\r (+ pos 2)))
           ((= c ?e) (cons 27 (+ pos 2)))
           ((= c ?d) (cons 127 (+ pos 2)))
           ((= c ?a) (cons 7 (+ pos 2)))
           ((= c ?b) (cons 8 (+ pos 2)))
           ((= c ?f) (cons 12 (+ pos 2)))
           ((= c ?v) (cons 11 (+ pos 2)))
           (t (cons c (+ pos 2))))))))

  (defun emacs-load--char-literal-modifier-prefixes (source pos)
    "Return (MODIFIERS . NEXT-POS) for zero or more `\\C-'/`\\M-' Control/Meta
modifier prefixes starting at POS in SOURCE.  MODIFIERS is a list of
`control'/`meta' symbols, one per prefix matched, in source order."
    (let ((len (length source))
          (mods nil))
      (while (and (<= (+ pos 3) len)
                  (= (aref source pos) ?\\)
                  (memq (aref source (1+ pos)) '(?C ?M))
                  (= (aref source (+ pos 2)) ?-))
        (push (if (= (aref source (1+ pos)) ?C) 'control 'meta) mods)
        (setq pos (+ pos 3)))
      (cons (nreverse mods) pos)))

  (defun emacs-load--rewrite-escaped-modifier-char-literals (source)
    "Rewrite a `\\C-'/`\\M-'-prefixed character literal whose base is itself
a backslash escape (e.g. `?\\C-\\[') to its correct decimal integer value
in SOURCE, computed with the same rules Emacs's reader documents ((info
\"(elisp) Character Type\")): Control on a base in the `[?@,?_]' or
`[?a,?z]' range masks the low 5 bits (Control-`?' is the single documented
exception, giving DEL/127); Control on any other base instead sets the
Control modifier bit (`#x4000000'); Meta always sets the Meta modifier bit
(`#x8000000'), combinable with either Control outcome.  T103: this
runtime's readers get this arithmetic wrong for exactly this shape --
`?\\C-\\[' reads as 28 instead of 27, because the Control mask is applied
to the literal backslash byte instead of to `[' (see
`emacs-load--escaped-modifier-char-literal-reader-native-p' and
`emacs-load--char-literal-escaped-base') -- and `nelisp--eval-source-string'
signals `invalid-read-syntax' on most such literals outright (`?\\C-e' and
`?\\C-?', an unescaped base, read correctly everywhere and are left alone
here).  Real files carry this syntax (e.g. `evil-commands.el''s
`evil-ex-substitute': `(cond ((memq c (list ?q ?l ?\\C-\\[)) ...))'), and
when it sits inside a form the native per-form reader in
`emacs-load--native-read-one' declines to read, T78's escape hatch hands
that form to `nelisp--eval-source-string', turning this into a whole-file
load failure the same way `emacs-load--rewrite-propertized-string-literals'
and `emacs-load--rewrite-labeled-object-references' document for their own
unsupported constructs.

Only a modifier-prefixed ESCAPED base is rewritten; a bare `?\\C-e' is
already read correctly everywhere and is skipped over, not touched (see
`emacs-load--char-literal-modifier-prefixes' and
`emacs-load--char-literal-escaped-base' for what counts as \"escaped\" and
which escapes are resolved).  Returns SOURCE unchanged (the same string
object, no copy) when `?\\' does not occur at all, which is the common
case for most files.  When it does occur, the scan jumps directly between
the rare marker characters that can start something meaningful (see
`emacs-load--next-marker-position') rather than visiting every byte, so a
file with many `?\\'-shaped escapes but no actual Control/Meta modifier
still finishes in a small fraction of a normal file's overall load time
\(T103 first shipped a naive per-character version of this scan that took
10+ seconds on a real ~40KB file for exactly this reason, before this
fix).  Forces SOURCE to `emacs-load--byte-indexed-source' first (a no-op
when already byte-indexed): see
`emacs-load--rewrite-propertized-string-literals''s docstring for why an
`aref'-based scanner must never run on a still-multibyte string on this
runtime."
    (setq source (emacs-load--byte-indexed-source source))
    (if (not (emacs-load--artifact-string-search "?\\" source 0))
        source
      (let ((len (length source))
            (out nil)
            (start 0)
            (i 0)
            (in-string nil)
            (escaped nil)
            (marker-state
             (emacs-load--marker-search-state
              source 0 '("\"" ";" "\\" "?"))))
        (while (< i len)
          (if in-string
              (progn
                (let ((ch (aref source i)))
                  (cond
                   (escaped (setq escaped nil))
                   ((= ch ?\\) (setq escaped t))
                   ((= ch ?\") (setq in-string nil))))
                (setq i (1+ i)))
            (setq i (emacs-load--next-marker-position source i len marker-state))
            (when (< i len)
              (let ((ch (aref source i)))
                (cond
                 ((= ch ?\\) (setq i (min len (+ i 2))))
                 ((= ch ?\;)
                  (let ((nl (emacs-load--artifact-string-search "\n" source i)))
                    (setq i (if nl (1+ nl) len))))
                 ((= ch ?\") (setq in-string t) (setq i (1+ i)))
                 ((= ch ??)
                  (let* ((mod-result
                          (emacs-load--char-literal-modifier-prefixes
                           source (1+ i)))
                         (mods (car mod-result))
                         (base-pos (cdr mod-result)))
                    (if (null mods)
                        (progn
                          (setq i (1+ i))
                          (when (and (< i len) (= (aref source i) ?\\))
                            (setq i (1+ i)))
                          (setq i (min len (1+ i))))
                      (let ((escape (emacs-load--char-literal-escaped-base
                                     source base-pos)))
                        (if (not escape)
                            (setq i base-pos)
                          (let* ((end (cdr escape))
                                 (value (car escape)))
                            (when (memq 'control mods)
                              (cond
                               ((= value ??) (setq value 127))
                               ((or (<= ?@ value ?_) (<= ?a value ?z))
                                (setq value (logand value #x1f)))
                               (t (setq value (logior value #x4000000)))))
                            (when (memq 'meta mods)
                              (setq value (logior value #x8000000)))
                            (push (substring source start i) out)
                            (push (number-to-string value) out)
                            (setq start end)
                            (setq i end)))))))
                 (t (setq i (1+ i))))))))
        (push (substring source start len) out)
        (apply #'concat (nreverse out)))))

  (defvar emacs-load--escaped-unicode-source-reader-native-p
    (and (fboundp 'nelisp--eval-source-string)
         (condition-case nil
             (= 8216 (nelisp--eval-source-string "(+ 0 ?\\‘)"))
           (error nil)))
    "Non-nil when the source evaluator accepts escaped Unicode characters.
The ordinary reader accepts these literals even on runtimes whose fast
source evaluator rejects them; probe that evaluator separately.")

  (defun emacs-load--escaped-unicode-literal-trigger-p (source)
    "Return non-nil for a possible escaped Unicode literal in byte-indexed SOURCE.
Use native substring search to skip ASCII escapes.  The full scanner still
checks strings, comments and symbol names before rewriting a candidate."
    (let ((pos 0) (len (length source)) found)
      (while (and (not found)
                  (setq pos (emacs-load--artifact-string-search "?\\" source pos)))
        (if (and (< (+ pos 2) len) (>= (aref source (+ pos 2)) #xc0))
            (setq found t)
          (setq pos (+ pos 2))))
      found))

  (defun emacs-load--rewrite-escaped-unicode-character-literals (source)
    "Replace escaped Unicode character literals with their integer values.
Leave strings, comments and escaped symbol names untouched.  This preserves
GNU Info's literal data on source evaluators that reject forms like ?\\‘."
    (when (multibyte-string-p source)
      (setq source (string-as-unibyte source)))
    (if (not (emacs-load--escaped-unicode-literal-trigger-p source))
        source
      (let ((len (length source)) (i 0) (start 0) out
            in-string escaped
            (markers (emacs-load--marker-search-state source 0 '("\"" ";" "\\" "?"))))
        (while (< i len)
          (if in-string
              (progn
                (cond (escaped (setq escaped nil))
                      ((= (aref source i) ?\\) (setq escaped t))
                      ((= (aref source i) ?\") (setq in-string nil)))
                (setq i (1+ i)))
            (setq i (emacs-load--next-marker-position source i len markers))
            (when (< i len)
              (cond
               ((= (aref source i) ?\") (setq in-string t i (1+ i)))
               ((= (aref source i) ?\;)
                (let ((newline (emacs-load--artifact-string-search "\n" source i)))
                  (setq i (if newline (1+ newline) len))))
               ((= (aref source i) ?\\) (setq i (min len (+ i 2))))
               ((and (= (aref source i) ??) (< (+ i 2) len)
                     (= (aref source (1+ i)) ?\\)
                     (>= (aref source (+ i 2)) #xc0)
                     (or (= i 0) (memq (aref source (1- i))
                                        '(32 9 10 13 12 40 91 39 96 44))))
                (let* ((lead (aref source (+ i 2)))
                       (bytes (cond ((< lead #xe0) 2) ((< lead #xf0) 3) (t 4)))
                       (end (min len (+ i 2 bytes)))
                       (value (car (read-from-string (string-as-multibyte (substring source i end))))))
                  (push (substring source start i) out)
                  (push (number-to-string value) out)
                  (setq i end start end)))
               ((and (= (aref source i) ??)
                     (or (= i 0) (memq (aref source (1- i))
                                        '(32 9 10 13 12 40 91 39 96 44))))
                ;; The ordinary reader understands modifier chains too.
                ;; Consume the complete ASCII literal so a quote/semicolon
                ;; in ?\\C-" or ?\\C-; cannot change our string/comment state.
                (setq i (cdr (read-from-string source i))))
               (t (setq i (1+ i)))))))
        (push (substring source start len) out)
        (apply #'concat (nreverse out)))))

  (defmacro nelisp-emacs--propertized-string-literal (str &rest ranges)
    "Expand to the string value `#(STR RANGES...)' would have read as.
STR and RANGES are exactly Emacs's `#(...)' read syntax after `#(': STR a
string literal, RANGES a flat list of START END PLIST triples.  Like
`#(...)' itself, none of these are forms to evaluate -- PLIST is a literal
property list, not code (`#(\"?\" 0 1 (face bold))' means \"apply property
list (face bold)\", it does not call a function named `face') -- so this
is a macro, not a function: STR and RANGES arrive as raw, unevaluated data,
exactly as `quote''s argument would, and the propertized string is built
once, at macroexpansion time, then wrapped in `quote' -- mirroring how
`#(...)' itself produces one literal object at read time rather than
re-building it on every evaluation.  Only
`emacs-load--rewrite-propertized-string-literals' ever introduces a call
to this macro (replacing the source text `#(' with a call to it, at the
same nesting depth, before any reader sees the construct); it is not
meant to be written by hand.  Each RANGES triple sets PLIST as STR's only
text properties over [START, END) via `set-text-properties', unconditionally
-- a later triple overrides an earlier one for positions both cover,
matching the documented `#(...)' read semantics (Info node `(elisp) Basic
Char Syntax').  This never branches on `propertize'/`add-text-properties'
being fboundp instead: on a runtime where string text properties are not
implemented (a separate, pre-existing gap -- confirmed directly: T103
found `propertize', `put-text-property', and `set-text-properties' all
already silently drop properties on plain strings on this substrate,
independent of this rewrite), `set-text-properties' is a no-op and the
result is a plain string with the right character content and no visible
properties, exactly as any other string constructed on that runtime would
be; `(get-text-property POS PROP STR)' on it then correctly returns nil,
consistent with its documented contract for a string with no properties
at POS.  There is no more-capable primitive to prefer here -- switching
would not change this outcome -- so fixing that gap, if ever wanted, is
core/runtime work, not something this rewrite can or should paper over."
    (let ((result (copy-sequence str))
          (remaining ranges))
      (while remaining
        (let ((start (pop remaining))
              (end (pop remaining))
              (plist (pop remaining)))
          (set-text-properties start end plist result)))
      (list 'quote result)))

  (defun nelisp--load-skip-space-and-comments (source pos)
    "Return first non-whitespace/comment position in SOURCE at or after POS.
Whitespace is space, tab, newline, CR, or formfeed.  A comment runs from
`;' through end of line, exclusive of the newline itself (the following
iteration of the whitespace/comment run consumes that newline as
ordinary whitespace).

SOURCE and POS must already share one coordinate space: this function
does not (and must not) convert SOURCE itself, because POS may already
be a position produced by earlier calls in the same scan, and this
runtime's byte-indexed (unibyte) and multibyte representations of the
same text do not share offsets once any non-ASCII byte precedes POS
-- converting mid-scan would silently reinterpret an old, still-valid
POS in the wrong coordinate space.  All real callers already establish
this invariant once, up front: `nelisp--load-map-source-forms' and
`nelisp--load-eval-source-incremental' both call
`emacs-load--byte-indexed-source' on SOURCE before their per-form loop
ever calls this function, and `emacs-load--read-file-string' does the
same to whatever `load' reads from disk.

Search for a newline in each comment run so the Lisp scanner only visits
whitespace and comment starts."
    (let ((len (length source))
          (done nil))
      (while (and (< pos len) (not done))
        (let ((c (aref source pos)))
          (cond
           ((or (= c ?\s) (= c ?\t) (= c ?\n) (= c ?\r) (= c ?\f))
            (setq pos (+ pos 1)))
           ((= c ?\;)
            (setq pos (or (string-search "\n" source pos) len)))
           (t
            (setq done t)))))
      pos))

  (defvar emacs-load--gc-cost-ratio 10
    "Minimum ratio of mutator time to GC time between periodic `load' GCs.
A `garbage-collect' costs time proportional to the whole heap, so once a
large image is resident a fixed every-N-forms GC dominates ordinary loads
(1.5s per collection after the bootstrap bundle).  Bounding GC overhead to
1/RATIO keeps the arena guard while loads stay proportional to their size.")

  (defvar emacs-load--gc-last-end nil
    "`float-time' at which the last periodic `load' GC finished.")

  (defvar emacs-load--gc-last-cost 0.0
    "Seconds taken by the last periodic `load' GC.")

  (defvar emacs-load--gc-pid nil
    "Process id that recorded `emacs-load--gc-last-end'.")

  (defvar emacs-load--gc-last-count nil
    "Most recent collection count observed by the periodic load check.")

  (defun emacs-load--collection-due-p ()
    "Return non-nil if an opportunistic load collection may run."
    (and (fboundp 'garbage-collect)
         (or (not (fboundp 'nelisp--gc-pressure))
             (>= (nelisp--gc-pressure) 100))))

  (defun emacs-load--periodic-gc (count)
    "Run the periodic `load' GC when COUNT hits the interval and it is cheap enough."
    (when (and load-garbage-collect-interval
               (> load-garbage-collect-interval 0)
               (= (% count load-garbage-collect-interval) 0)
               (emacs-load--collection-due-p))
      (let* ((now (float-time))
             (pid (and (fboundp 'emacs-pid) (emacs-pid)))
             (observed (and (boundp 'gcs-done) gcs-done))
             (reset (or (null emacs-load--gc-last-end)
                        (not (eql pid emacs-load--gc-pid))
                        (not (eql observed emacs-load--gc-last-count)))))
        ;; A heap image carries the build process's last-GC time, so every
        ;; restored session collected on its first `load' (4-8 s on a large
        ;; heap).  Reset the clock when the process changes or an automatic
        ;; or explicit collection has already run since the last check.
        (if reset
            (setq emacs-load--gc-pid pid
                  emacs-load--gc-last-count observed
                  emacs-load--gc-last-end now)
          (when (>= (- now emacs-load--gc-last-end)
                    (* emacs-load--gc-cost-ratio emacs-load--gc-last-cost))
            (garbage-collect)
            (setq emacs-load--gc-last-count (and (boundp 'gcs-done) gcs-done)
                  emacs-load--gc-last-end (float-time)
                  emacs-load--gc-last-cost (- emacs-load--gc-last-end now)))))))

  (defun nelisp--load-map-source-forms (source callback)
    "Read SOURCE one top-level form at a time and call CALLBACK on each.
The reader uses the same whitespace/comment skipping and closing-paren
compatibility as the standalone normalizer so large forms still advance
correctly when `read-from-string' stops on a trailing close paren.
Return the last CALLBACK result."
    (setq source (emacs-load--byte-indexed-source source))
    (let ((pos 0)
          (len (length source))
          (last nil)
          (count 0)
          (read nil))
      (while (progn
               (setq pos (nelisp--load-skip-space-and-comments source pos))
               (< pos len))
        (let* ((form-end (emacs-load--artifact-source-form-end source pos))
               (slice (emacs-load--reader-slice source pos form-end))
               (slice-len (length slice)))
          (setq read (read-from-string slice 0 slice-len))
          (let ((next (and (consp read) (cdr read))))
            (when (and (integerp next)
                       (> next 0)
                       (< next slice-len)
                       (= (aref slice next) ?\)))
              (setq next (+ next 1)))
            (when (or (not (consp read))
                      (not (integerp next))
                      (<= next 0)
                      (/= next slice-len))
              (signal 'end-of-file
                      (list "rewrite load reader made no progress" pos)))
            (setq last
                  (funcall callback
                           (nelisp--load-rewrite-defalias-form (car read))))
            (setq pos form-end))
          (setq count (+ count 1))
          (emacs-load--periodic-gc count)))
      last))

  (defun emacs-load--macro-alias-form-p (form)
    "Return non-nil when executable FORM contains a symbolic macro alias.
Quoted data and bare function references are not executable subforms."
    (when (consp form)
      (let ((head (car form)))
        (cond
         ((eq head 'quote) nil)
         ((emacs-load--symbolic-macro-alias-p head) t)
         ;; Backquote data such as (function . "#'") need not be a list.
         ((and (eq head 'function)
               (not (eq (car-safe (car-safe (cdr form))) 'lambda)))
          nil)
         (t
          (let ((tail (if (consp head) form (cdr form))) found)
            (while (and (consp tail) (not found))
              (let ((subform (car tail)))
                (when (consp subform)
                  (setq found (emacs-load--macro-alias-form-p subform))))
              (setq tail (cdr tail)))
            found))))))

  (defvar nelisp--load-file-local-specials nil
    "Symbols declared by initializer-free top-level `defvar' in this file.
GNU `load' evaluates a whole lexical file in one interpreter environment,
and `(defvar SYM)' adds SYM to it, so later forms of the same file bind
SYM dynamically.  Each form here is evaluated separately, so the loader
carries these declarations forward in the environment it passes to `eval'.
`nelisp--load-eval-source-incremental' rebinds this to nil per file.")

  (defun emacs-load--bundle-member-boundary-p (gap)
    "Return non-nil when the skipped text GAP holds a bundle member marker.
scripts/build-nelisp-bootstrap.el writes `;;; >>> FILE' before and
`;;; <<< FILE' after each concatenated member."
    (or (string-search "\n;;; >>> " gap)
        (string-search "\n;;; <<< " gap)
        (string-prefix-p ";;; >>> " gap)
        (string-prefix-p ";;; <<< " gap)))

  (defun nelisp--load-note-local-specials (form)
    "Record initializer-free `defvar' declarations in top-level FORM."
    (cond
     ((and (eq (car-safe form) 'defvar)
           (consp (cdr form))
           (null (cddr form))
           (symbolp (cadr form)))
      (unless (memq (cadr form) nelisp--load-file-local-specials)
        (push (cadr form) nelisp--load-file-local-specials)))
     ((eq (car-safe form) 'progn)
      (mapc #'nelisp--load-note-local-specials (cdr form)))))

  (defun nelisp--load-eval-one-form (form)
    "Evaluate FORM, treating a top-level `cc-provide' as `provide'.
NeLisp's source evaluator bare-aborts on the CC Mode compile-time
marker even when `cc-provide' is callable.  At interpreted load time
its semantics are exactly `provide'.
In a lexical file, FORM sees the file's earlier initializer-free
`defvar' declarations (see `nelisp--load-file-local-specials')."
    (when (and (consp form) (eq (car form) 'cc-provide))
      (setcar form 'provide))
    ;; Native eval recognizes direct macros but evaluates symbolic macro
    ;; aliases as ordinary calls.  Its macroexpander follows those aliases
    ;; correctly.  Expand at the load boundary without copying the target's
    ;; macro object, so later target redefinitions still follow GNU aliases.
    ;; Function bodies need this too: the native function compiler otherwise
    ;; retains nested aliases such as `cl-flet*' as ordinary calls.
    (when (emacs-load--macro-alias-form-p form)
      (setq form (macroexpand-all form)))
    (let ((lexical (and (boundp 'lexical-binding) lexical-binding)))
      (prog1 (eval form (if (and lexical nelisp--load-file-local-specials)
                            (append nelisp--load-file-local-specials '(t))
                          lexical))
        (when lexical
          (nelisp--load-note-local-specials form)))))

  (defun emacs-load--source-lexical-binding (source)
    "Return the `lexical-binding' value SOURCE's first-line cookie selects.
Like GNU `load', only the `-*- ... -*-' cookie on the first line is
consulted; a file without one is loaded with dynamic binding (nil)."
    (let* ((nl (emacs-load--artifact-string-search "\n" source 0))
           (line (substring source 0 (or nl (min (length source) 1024)))))
      (save-match-data
        (and (string-match "-\\*-.*lexical-binding:[ \t]*\\([^ \t;]+\\)" line)
             (not (equal (match-string 1 line) "nil"))
             t))))

  (defconst emacs-load--native-read-probe-window-sizes '(512 2048 8192 32768)
    "Byte windows to try before falling back to the source boundary scanner.")

  (defun emacs-load--native-read-one (source pos len &optional byte-source)
    "Read one form from SOURCE at POS, returning (FORM . BYTE-END) or nil.
With BYTE-SOURCE, SOURCE is unibyte and the reader validates each byte
window in place.  It returns t for bytes that require conversion.  Read
such a window as multibyte and recover its byte end from the consumed
prefix."
    (let ((windows emacs-load--native-read-probe-window-sizes)
          result)
      (while (and windows (not result))
        (let* ((window-end (min len (+ pos (car windows))))
               (raw (and byte-source
                         (nelisp--read-all-from-string-native
                          source pos window-end t)))
               (slice (when (or (not byte-source) (eq raw t))
                        (emacs-load--reader-slice source pos window-end)))
               (end (if slice (length slice) window-end))
               (probe (if slice
                          (and (> end 0)
                               (nelisp--read-all-from-string-native slice 0 end))
                        raw))
               (next (cdr-safe probe)))
          ;; A token at a truncated window's end may continue in SOURCE.
          (when (and (integerp next) (> next (if slice 0 pos))
                     (or (< next end) (= window-end len)))
            (setq result
                  (if (not slice)
                      probe
                    (cons (car probe)
                          (if (= next end)
                              window-end
                            (+ pos (length (string-as-unibyte
                                            (substring slice 0 next)))))))))
          (setq windows (and (< window-end len) (cdr windows)))))
      result))

  (defvar emacs-load--source-single-form-p nil
    "Non-nil when the next source tail is already bounded to one form.")

  (defun emacs-load--single-form-rewrite-inert-p (source)
    "Return non-nil if SOURCE is one list with no eligible outer rewrite.
The structural boundary must cover the entire source.  A non-wrapper head
cannot contain load-time rewrites, even when its literal data mentions
`defalias'.  Read only the head instead of reparsing and printing that data."
    (let ((len (length source)))
      (and (> len 0)
           (= (aref source 0) ?\()
           (or emacs-load--source-single-form-p
               (and (fboundp 'nelisp--source-container-end)
                    (let ((end (nelisp--source-container-end source 0)))
                      (and (integerp end) (= end len)))))
           (let ((head (car (read-from-string source 1 len))))
             (and (symbolp head)
                  (not (eq head 'defalias))
                  (not (memq head nelisp--load-rewrite-wrapper-heads)))))))

  (defun emacs-load--replace-source-literal (from to source)
    "Replace non-overlapping literal FROM spans in SOURCE with TO.
Use character positions and join unchanged spans once, avoiding `string-replace'
implementation's growing-prefix allocation on large source containers."
    (when (= (length from) 0)
      (signal 'wrong-length-argument '(0)))
    (let ((position 0) (pieces nil) found)
      (while (setq found
                   (string-search from source position))
        (push (substring source position found) pieces)
        (push to pieces)
        (setq position (+ found (length from))))
      (if (null pieces)
          source
        (push (substring source position) pieces)
        (apply #'concat (nreverse pieces)))))

  (defun nelisp--load-eval-source-tail (source pos len)
    "Evaluate the remainder of SOURCE from POS to LEN as one native unit.
Escape hatch for `nelisp--load-eval-source-incremental': used only when
the native per-form reader has just declined to read the form starting
at POS.  Applies the same defalias/cc-provide rewrites the per-form
loop would have applied, over the whole remaining chunk at once, then
hands it to `nelisp--eval-source-string' -- which has no per-element
budget and evaluates content like one huge literal data table in
milliseconds instead of the minutes the per-character fallback path
would take (see Doc T78).

Deliberately does NOT call `nelisp--load-one-shot-source' /
`nelisp--load-rewrite-target-present-p': that helper's escaped-spelling
fallback scan (guarding against a hand-obfuscated `(d\\efalias ...)')
re-scans from every `d' in SOURCE and is itself O(source length) per
`d', which is fine for an ordinary file but becomes another multi-second
tax on exactly the content that reaches this function -- one huge
literal data table, typically full of hex-escaped strings (`\\x...')
whose names are riddled with the letter `d'.  A form large enough to
make the native reader decline is architecturally a literal, not
handwritten code trying to dodge `defalias' detection, so only the
plain substring check is applied here."
    (let ((tail (emacs-load--reader-slice source pos len))
          (single-form-p emacs-load--source-single-form-p)
          ;; This is a range hint, not an execution mode.  Nested loads must
          ;; establish their own boundaries rather than inherit this one.
          (emacs-load--source-single-form-p nil))
      (when (emacs-load--artifact-string-search "cc-provide" tail 0)
        (setq tail (emacs-load--replace-source-literal "cc-provide" "provide" tail)))
      (nelisp--eval-source-string
       (concat "(progn\n"
               (if (and (emacs-load--artifact-string-search "defalias" tail 0)
                        (not (let ((emacs-load--source-single-form-p single-form-p))
                               (emacs-load--single-form-rewrite-inert-p tail))))
                   (nelisp--load-normalize-source-rewriting tail)
                 tail)
               "\n)"))))

  (defun nelisp--load-eval-source-declined-form (source pos form-end)
    "Evaluate one form whose initial native read was declined.
Once FORM-END bounds a large container, try its complete native read once.
This avoids the fallback's Lisp read/print cycle for source files wrapped
in a single large WHEN, while preserving quoted strings and symbols.
If the native reader still declines, retain the source fallback."
    (let* ((span (- form-end pos))
           (probe
            (and (fboundp 'nelisp--read-all-from-string-native)
                 (> span (car (last emacs-load--native-read-probe-window-sizes)))
                 (let ((emacs-load--native-read-probe-window-sizes (list span)))
                   (emacs-load--native-read-one source pos form-end)))))
      (cons
       (if (and probe (= (cdr probe) form-end))
           (nelisp--load-eval-one-form
            (nelisp--load-rewrite-defalias-form (car probe)))
         (let ((emacs-load--source-single-form-p t))
           (condition-case err
               (nelisp--load-eval-source-tail source pos form-end)
             (invalid-read-syntax
              ;; Valid GNU syntax which the native reader cannot represent
              ;; must still use the Lisp reader, without evaluating twice.
              (if (emacs-load--native-read-one source pos form-end)
                  (signal (car err) (cdr err))
                (nelisp--load-eval-lisp-read-slice source pos form-end))))))
       form-end)))

  (defun nelisp--load-eval-lisp-read-slice (source pos form-end)
    "Read SOURCE between POS and FORM-END with `read-from-string' and eval it.
Fallback for a form the runtime reader rejects; return the last value."
    (let* ((slice (emacs-load--reader-slice source pos form-end))
           (length (length slice))
           (index 0)
           (last nil)
           read)
      (while (and (< index length)
                  (setq read (condition-case nil
                                 (read-from-string slice index)
                               (end-of-file nil))))
        (setq last (nelisp--load-eval-one-form
                    (nelisp--load-rewrite-defalias-form (car read)))
              index (cdr read)))
      last))

  (defun emacs-load--artifact-source-decline-boundary (reader source pos)
    "Call READER for a declined form boundary, returning nil on scan failure.
Generic scanner errors are converted to nil; non-generic condition classes
retain their original signal.  This lets the incremental loader fall back to
its large-source evaluator, which then reports malformed input in the same way
as the old path."
    (condition-case caught
        (funcall reader source pos)
      (end-of-file nil)
      (error
       (if (and (consp caught) (eq (car caught) 'error))
           nil
         (signal (car caught) (cdr caught))))))

  (defun nelisp--load-eval-source-incremental (source)
    "Read and eval SOURCE top-level forms one at a time.
Return the value of the last form.  This deliberately avoids
`nelisp--read-all-from-string', which materializes the whole AST and can
overflow the standalone arena on upstream-sized package files.

Each form is read via a direct probe of the runtime's native reader
first (`emacs-load--native-read-one'): fast, and it avoids both our own
per-character form-boundary scan (`emacs-load--artifact-source-form-end')
and `read-from-string''s elisp fallback reader for the common case of an
ordinary, moderately-sized form.  If the native reader declines for the
form starting at the current position, the loader scans its boundary with
`emacs-load--artifact-source-form-end' against the full SOURCE.  A form with
a valid boundary is evaluated alone and native probing resumes at the next
form.  If boundary scanning fails, the loader escapes to
`nelisp--load-eval-source-tail' for the remainder of SOURCE.  This retains
the T78 fast path for malformed or otherwise unscannable source while
avoiding a whole-tail fallback for one large container.
Runtimes without the native probe continue through the original per-form
scan unchanged."
    (setq source (emacs-load--byte-indexed-source source))
    (let ((lexical-binding (emacs-load--source-lexical-binding source))
          (pos 0)
          (len (length source))
          (last nil)
          (count 0)
          (nelisp--load-file-local-specials nil)
          (native-probe-available (fboundp 'nelisp--read-all-from-string-native))
          (byte-source (not (multibyte-string-p source))))
      (while (let ((before pos))
               (setq pos (nelisp--load-skip-space-and-comments source pos))
               ;; A generated bundle concatenates many files; GNU would load
               ;; each separately, so its declarations end at a member marker.
               (when (and nelisp--load-file-local-specials
                          (emacs-load--bundle-member-boundary-p
                           (substring source before pos)))
                 (setq nelisp--load-file-local-specials nil))
               (< pos len))
        (let ((probe (and native-probe-available
                           (emacs-load--native-read-one
                            source pos len byte-source))))
          (cond
           (probe
            (setq last
                  (nelisp--load-eval-one-form
                   (nelisp--load-rewrite-defalias-form (car probe))))
            (setq pos (cdr probe)))
           (native-probe-available
            ;; The native reader declined the form starting at POS.  Scan its
            ;; boundary against the full source so a large container can be
            ;; isolated without sending the following forms through tail.
            (let ((form-end
                   (emacs-load--artifact-source-decline-boundary
                    #'emacs-load--artifact-source-form-end source pos)))
              (if (and (integerp form-end)
                       (> form-end pos)
                       (<= form-end len))
                  (let ((fallback
                         (nelisp--load-eval-source-declined-form
                          source pos form-end)))
                    (setq last (car fallback)
                          pos (cdr fallback)))
                (setq last (nelisp--load-eval-source-tail source pos len)
                      pos len))))
           (t
            ;; No native probe primitive on this runtime at all: keep the
            ;; original per-form scan + `read-from-string' path exactly
            ;; as it worked before this fast path existed.
            (let* ((form-end (emacs-load--artifact-source-form-end source pos))
                   (slice (emacs-load--reader-slice source pos form-end))
                   (slice-len (length slice))
                   (read (read-from-string slice 0 slice-len))
                   (next (and (consp read) (cdr read))))
              (when (and (integerp next)
                         (> next 0)
                         (< next slice-len)
                         (= (aref slice next) ?\)))
                (setq next (+ next 1)))
              (when (or (not (consp read))
                        (not (integerp next))
                        (<= next 0)
                        (/= next slice-len))
                (signal 'end-of-file
                        (list "rewrite load reader made no progress" pos)))
              (setq last
                    (nelisp--load-eval-one-form
                     (nelisp--load-rewrite-defalias-form (car read))))
              (setq pos form-end))))
          (setq count (+ count 1))
          (emacs-load--periodic-gc count)))
      last))

  (defun nelisp--load-normalize-source-rewriting (source)
    "Return SOURCE normalized for native one-shot evaluation.
Top-level forms are read with `nelisp--load-skip-space-and-comments',
rewritten structurally with `nelisp--load-rewrite-defalias-form', and
serialized with `prin1-to-string'.  The result preserves the fast native
throughput path while fixing forward and macro alias semantics without
evaluating any forms here."
    (let ((forms nil))
      (nelisp--load-map-source-forms
       source
       (lambda (form)
         (let ((text (prin1-to-string form)))
           (push text forms)
           text)))
      (mapconcat #'identity (nreverse forms) "\n")))

  (defun nelisp--load-rewrite-target-present-p (source)
    "Return non-nil when SOURCE may contain a `defalias' rewrite target.
The ordinary spelling uses the runtime's native substring search.  The
fallback scan recognizes reader escapes within the symbol spelling so a form
such as `(d\\efalias ...)' still takes the structural normalization path.
False positives are harmless: they only retain the existing slow path."
    (or (emacs-load--artifact-string-search "defalias" source 0)
        (and (emacs-load--artifact-string-search "\\" source 0)
             (let ((needle "defalias")
                   (source-len (length source))
                   (needle-len 8)
                   (start 0)
                   (found nil))
               (while (and (not found) (< start source-len))
                 (let ((candidate
                        (emacs-load--artifact-string-search
                         "d" source start)))
                   (if (null candidate)
                       (setq start source-len)
                     (let ((source-pos candidate)
                           (needle-pos 0)
                           (escaped nil)
                           (matching t))
                       (while (and matching
                                   (< needle-pos needle-len)
                                   (< source-pos source-len))
                         (when (= (aref source source-pos) ?\\)
                           (setq escaped t)
                           (setq source-pos (+ source-pos 1))
                           (when (>= source-pos source-len)
                             (setq matching nil)))
                         (when matching
                           (if (= (aref source source-pos)
                                  (aref needle needle-pos))
                               (progn
                                 (setq source-pos (+ source-pos 1))
                                 (setq needle-pos (+ needle-pos 1)))
                             (setq matching nil))))
                       (when (and matching escaped (= needle-pos needle-len))
                         (setq found t)))
                     (setq start (+ candidate 1)))))
               found))))

  (defun nelisp--load-one-shot-source (source)
    "Return normalized SOURCE enclosed in one top-level evaluation form.
The standalone substrate may return after an early top-level form when given
a form stream.  One outer `progn' makes the complete file a single unit while
leaving artifact normalization's form-stream contract unchanged.  Sources
without a possible rewrite target retain their original body and avoid the
otherwise redundant per-form read, tree walk, and serialization pass."
    (concat "(progn\n"
            (if (nelisp--load-rewrite-target-present-p source)
                (nelisp--load-normalize-source-rewriting source)
              ;; Convert malformed external bytes before concat can treat a
              ;; raw C0/C1 and continuation as one internal byte8 character.
              (if (or (multibyte-string-p source)
                      (and (fboundp 'nelisp--load-external-utf8-p)
                           (nelisp--load-external-utf8-p source)))
                  source
                (string-as-multibyte source)))
            "\n)"))

  (defun emacs-load--artifact-write-normalized-source (source path)
    "Write SOURCE normalized for artifact compilation to PATH incrementally."
    (with-temp-file path
      (insert ";;; nelisp-private-nelc-v2\n")
      (nelisp--load-map-source-forms
       source
       (lambda (form)
         (prin1 form (current-buffer))
         (terpri (current-buffer))
         form))))

  (defun emacs-load--artifact-write-raw-source (source path &optional header)
    "Write SOURCE directly to PATH for low-memory compiler staging.
When HEADER is non-nil, insert it before SOURCE; otherwise prepend the
standard harmless artifact staging comment."
    (with-temp-file path
      (insert (or header ";;; nelisp-private-nelc-v2\n"))
      (insert source)))

  (defun nelisp--load-eval-source-hybrid (source)
    "Evaluate SOURCE as one form after normalizing top-level defalias rewrites.
This is the correctness-first standalone load path; cache and throughput
work belong elsewhere."
    (nelisp--eval-source-string
     (nelisp--load-one-shot-source source)))

  (defun nelisp--load-source-large-p (source)
    "Return non-nil when SOURCE should use incremental loading."
    (let ((threshold emacs-load-large-source-threshold))
      (and (integerp threshold)
           (> threshold 0)
           (> (emacs-load--artifact-byte-length source) threshold))))

  (defun emacs-load--dependency-source-p (source)
    "Return non-nil when SOURCE may call a dependency loader.
An isolated token also covers whitespace and comments after the opening
parenthesis.  Tokens in quoted data or comments conservatively select the
per-form evaluator; names containing `load' are not dependency tokens."
    (let ((names '("require" "load" "load-file")) found)
      (while (and names (not found))
        (let ((name (pop names)) (start 0) position)
          (while (and (not found)
                      (setq position (emacs-load--artifact-string-search name source start)))
            (let ((end (+ position (length name))))
              (when (and (or (= position 0)
                             (memq (aref source (1- position))
                                   '(32 9 10 13 40 41 34 39 96 44 59)))
                         (or (= end (length source))
                             (memq (aref source end)
                                   '(32 9 10 13 40 41 34 39 96 44 59))))
                (setq found t))
              (setq start end)))))
      found))

  (defun nelisp--load-source-loader (source)
    "Return the preferred source evaluator for SOURCE."
    ;; A source with a `defalias' rewrite target goes through the incremental
    ;; loader too: the one-shot path re-serializes every form with the
    ;; Lisp-level `prin1-to-string' (~20ms per form), which made loading e.g.
    ;; regexp-opt.el take 1.5s instead of 0.09s.
    (if (or (not (fboundp 'nelisp--eval-source-string))
            (nelisp--load-source-large-p source)
            ;; Dependencies may install symbolic macro aliases after this
            ;; decision.  Native whole-source eval does not recognize those
            ;; aliases; per-form evaluation expands them at their call site.
            ;; Match conservatively, including quoted/comment occurrences:
            ;; those merely select the same evaluator used for large files.
            (emacs-load--dependency-source-p source)
            (emacs-load--macro-alias-source-p source)
            (nelisp--load-rewrite-target-present-p source))
        #'nelisp--load-eval-source-incremental
      #'nelisp--load-eval-source-hybrid))

  (defun nelisp--load-artifact-source-transform (source _path)
    "Return SOURCE normalized for artifact-first loading."
    (nelisp--load-normalize-source-rewriting source))

  (defun emacs-load--artifact-cache-directory ()
    "Return the standalone artifact cache root."
    (cond
     ((and (boundp 'user-emacs-directory)
           (stringp user-emacs-directory)
           (> (length user-emacs-directory) 0))
      (expand-file-name "nelisp-cache/" user-emacs-directory))
     ((and (boundp 'temporary-file-directory)
           (stringp temporary-file-directory)
           (> (length temporary-file-directory) 0))
      (expand-file-name "nelisp-cache/" temporary-file-directory))
     (t "/tmp/nelisp-cache/")))

  (defvar emacs-load-auto-native-compile nil
    "When non-nil, allow standalone loads to compile and replay artifacts.
Bootstrap completes before `bin/nemacs' enables this.")

  (defcustom emacs-load-artifact-max-source-size 0
    "Maximum source byte size eligible for standalone artifact loading.
Sources larger than this go directly through the source loader, without
artifact compilation, replay, cache shard creation, or cache record writes.
Nil removes the size limit; a non-positive integer disables the artifact
route.  This setting does not affect host Emacs loading."
    :type '(choice (const :tag "No size limit" nil) integer)
    :group 'lisp)

  (defvar emacs-load-artifact-replay-streaming-threshold 65536
    "Byte threshold above which artifact replay uses the streaming reader.
Nil or a non-positive value disables the size-based streaming path and
forces the historical whole-payload reader for all artifacts.")

  (defvar emacs-load-artifact-replay-preflight-gc t
    "When non-nil, check GC pressure before top-level artifact replay.
An eligible collection runs in `emacs-load--artifact-replay-file'.")

  (defvar emacs-load-artifact-replay-pressure-threshold 536870912
    "When positive, reclaim memory on nested artifact replay once growth
meets this used-bytes delta.  Nil or non-positive disables pressure-based
nested `garbage-collect'.")

  (defvar emacs-load-artifact-replay-garbage-collect-interval-cap 20
    "Artifact-only cap for positive `load-garbage-collect-interval' values.
When this variable and `load-garbage-collect-interval' are both positive
integers, top-level artifact replay uses the smaller value so large replay
tails collect sooner without changing ordinary source loading.  Nil or a
non-positive value disables the artifact-only cap.  Nil or disabled
`load-garbage-collect-interval' semantics stay unchanged, and nested
artifact replay inherits the outer replay binding.")

  (defvar emacs-load-artifact-native-sections-native-reader-threshold 65536
    "Byte threshold for whole-string native reads during native metadata replay.
When `nelisp--read-all-from-string-native' is available, native section
spans and heavy native metadata list fields at or below this size use the
whole-string native reader.  Larger ranges use the existing incremental
range-based parsers so 300KB-1MB `:relocs'/`:defuns' lists do not
materialize as a single retained object with no safe GC boundary.  Nil or
a non-positive value disables the size-based routing and preserves the
historical whole-string native read path whenever the native reader is
available.")

  (defvar emacs-load-artifact-module-init-native-batch-items 32
    "Maximum number of `:module-init' entries to read per native batch.")

  (defvar emacs-load--artifact-native-batch-diagnostic nil
    "Compact diagnostic for the last failed native artifact batch read.")

  (defvar emacs-load-artifact-native-batch-capture-failed-slice nil
    "When non-nil, retain failed native batch source in its diagnostic.")

  (defconst emacs-load--artifact-cache-source-digest-salt
    "source-defun-fallback-v6-standalone-replay-gc-safe-native-shard-budget-4-defuns-per-section-64KiB-serialized-native-section-replay-byte-budget"
    "Schema salt mixed into cache sidecar source digests.
Salt v6 explicitly invalidates artifacts built before the 64KiB serialized
native-section replay byte budget was part of the cache contract, so old
budget=4 but oversized section artifacts do not replay as cache hits.")

  (defvar emacs-load--last-artifact-payload nil
    "Last artifact payload replayed by `emacs-load--artifact-replay-file'.
Large-artifact streaming stores a compact summary plist here instead of the
full payload, because production callers only depend on the truthy replay
result and the installed functions.")

  (defvar emacs-load--artifact-native-hash-cache (make-hash-table :test 'equal)
    "Cache of native artifact blob hashes to mapped base addresses.")

  (defvar emacs-load--artifact-native-diagnostic-report nil
    "Last native artifact replay diagnostic report.")

  (defvar emacs-load--artifact-compile-diagnostic-report nil
    "Last standalone artifact compile diagnostic report.
When non-nil, this is a plist describing the most recent compile failure
that caused the loader to fall back to source evaluation.")

  (defvar emacs-load--artifact-replay-depth 0
    "Dynamic recursion depth for artifact replay preflight GC guard.")

  (defvar emacs-load--artifact-replay-pressure-baseline nil
    "Dynamic baseline of arena used bytes for artifact replay pressure GC.")

  (defvar emacs-load--artifact-replay-pressure-enabled nil
    "Whether pressure-based nested replay GC is enabled for this outermost call.")

  (defun emacs-load--artifact-replay-garbage-collect-interval ()
    "Return the effective `load-garbage-collect-interval' for artifact replay."
    (let ((current load-garbage-collect-interval)
          (cap emacs-load-artifact-replay-garbage-collect-interval-cap))
      (if (and (integerp current)
               (> current 0)
               (integerp cap)
               (> cap 0))
          (if (< current cap) current cap)
        current)))

  (defun emacs-load--artifact-replay-pressure-bytes ()
    "Return a nonnegative replay pressure metric, or nil when unavailable.
Prefer `nelisp--allocation-debt' so pressure tracks monotonic allocation
requests including free-list reuse.  Fall back to used bytes from
`nelisp--arena-stats' on older runtimes."
    (condition-case nil
        (cond
         ((fboundp 'nelisp--allocation-debt)
          (let ((debt (nelisp--allocation-debt)))
            (and (integerp debt)
                 (>= debt 0)
                 debt)))
         ((fboundp 'nelisp--arena-stats)
          (let ((stats (nelisp--arena-stats))
                (used nil)
                (count 0)
                (tail nil))
            (when (consp stats)
              (setq tail stats)
              (while (consp tail)
                (when (= count 11)
                  (setq used (car tail)))
                (setq tail (cdr tail))
                (setq count (1+ count)))
              (when (and (null tail)
                         (>= count 12)
                         (integerp used)
                         (>= used 0))
                used)))))
      (error nil)))

  (defun emacs-load--artifact-replay-arena-used-bytes ()
    "Compatibility wrapper for `emacs-load--artifact-replay-pressure-bytes'."
    (emacs-load--artifact-replay-pressure-bytes))

  (defun emacs-load--artifact-replay-pressure-maybe-gc ()
    "Run pressure-driven GC when artifact replay usage growth exceeds threshold."
    (let ((threshold emacs-load-artifact-replay-pressure-threshold))
      (when (and emacs-load--artifact-replay-pressure-enabled
                 (integerp threshold)
                 (> threshold 0)
                 (integerp emacs-load--artifact-replay-pressure-baseline)
                 (>= emacs-load--artifact-replay-pressure-baseline 0))
        (let ((used (emacs-load--artifact-replay-pressure-bytes)))
          (if (not (integerp used))
              (setq emacs-load--artifact-replay-pressure-enabled nil)
            (when (and (>= (- used emacs-load--artifact-replay-pressure-baseline)
                           threshold)
                       (emacs-load--collection-due-p))
              (garbage-collect)
              (setq emacs-load--artifact-replay-pressure-baseline
                    (emacs-load--artifact-replay-pressure-bytes))
              (unless (integerp emacs-load--artifact-replay-pressure-baseline)
                (setq emacs-load--artifact-replay-pressure-enabled nil))))))))

  (defun emacs-load--artifact-native-reader-within-threshold-p
      (source start end)
    "Return non-nil when the native whole-string reader is allowed for SOURCE.
START and END delimit the candidate range.  The range must fit within
`emacs-load-artifact-native-sections-native-reader-threshold' bytes unless
that threshold is disabled."
    (let ((threshold emacs-load-artifact-native-sections-native-reader-threshold))
      (and (fboundp 'nelisp--read-all-from-string-native)
           (or (not (integerp threshold))
               (<= threshold 0)
               (let ((char-span (- end start)))
                 (and (<= char-span threshold)
                      (<= (emacs-load--artifact-byte-length
                           (substring source start end))
                          threshold)))))))

  (defun emacs-load--artifact-compiler ()
    "Return the first usable standalone artifact compiler executable."
    (let ((candidates
           (list (and (boundp 'nemacs-nelisp-executable)
                      nemacs-nelisp-executable)
                 (and (fboundp 'getenv) (getenv "NEMACS_NELISP"))
                 (let ((home (and (fboundp 'getenv) (getenv "NELISP_HOME"))))
                   (and home (expand-file-name "target/nelisp" home)))
                 (and (fboundp 'executable-find)
                      (executable-find "nelisp"))))
          (found nil))
      (while (and candidates (not found))
        (let ((candidate (pop candidates)))
          (when (and (stringp candidate)
                     (> (length candidate) 0)
                     (file-executable-p candidate))
            (setq found candidate))))
      found))

  (defun emacs-load--artifact-compiler-identity (compiler)
    "Return a cheap stable identity for COMPILER, or nil if unavailable."
    (let* ((path (expand-file-name compiler))
           (attributes
            (and (not (fboundp 'nelisp--syscall-stat-field))
                 (fboundp 'file-attributes)
                 (file-attributes path 'integer)))
           (size
            (if (fboundp 'nelisp--syscall-stat-field)
                (nelisp--syscall-stat-field path 48)
              (and attributes (nth 7 attributes))))
           (mtime-seconds
            (if (fboundp 'nelisp--syscall-stat-field)
                (nelisp--syscall-stat-field path 88)
              (and attributes (nth 5 attributes))))
           (mtime-nanoseconds
            (and (fboundp 'nelisp--syscall-stat-field)
                 (nelisp--syscall-stat-field path 96)))
           (mtime (if mtime-nanoseconds
                      (list mtime-seconds mtime-nanoseconds)
                    mtime-seconds)))
      (and (integerp size)
           (> size 0)
           (or (and (integerp mtime-seconds)
                    (>= mtime-seconds 0)
                    (integerp mtime-nanoseconds)
                    (>= mtime-nanoseconds 0))
               (and attributes mtime))
           (list :path path :size size :mtime mtime))))

  (defvar emacs-load--unicode-name-cache (make-hash-table :test 'equal))

  (defun emacs-load--unicode-name-code (name)
    "Resolve NAME to a character code, as the reader does for \\N{NAME}.
`char-from-name' answers from GNU Emacs's own character-name table (the
reader's source of truth, lread.c `character_name_to_code'); runs of
whitespace in NAME count as one space, and an unknown name or a surrogate is
a reader error.  Only a runtime without `char-from-name' falls back to the
host Python Unicode database, which knows different names."
    (or (gethash name emacs-load--unicode-name-cache)
        (let ((code
               (if (fboundp 'char-from-name)
                   (let ((c (char-from-name
                             (replace-regexp-in-string "[ \t\n\v\f\r]+" " " name) t)))
                     (unless (and (integerp c) (not (and (>= c #xD800) (<= c #xDFFF))))
                       (signal 'invalid-read-syntax (list (format "\\N{%s}" name))))
                     c)
               (with-temp-buffer
                 (unless (equal
                          (call-process
                           "python3" nil t nil "-c"
                           "import sys,unicodedata; c=unicodedata.lookup(sys.argv[1]); assert len(c)==1; print(ord(c))"
                           name)
                          0)
                   (signal 'invalid-read-syntax '("Unknown character name")))
                 (string-to-number (buffer-string))))))
          (puthash name code emacs-load--unicode-name-cache)
          code)))

  (defun emacs-load--rewrite-named-unicode-escapes (source)
    "Lower named Unicode escapes in character and string literals.
Comments, escaped backslashes and symbol names remain literal source text."
    (if (not (emacs-load--artifact-string-search "\\N{" source 0))
        source
      (let ((i 0) (start 0) (len (length source)) in-string out)
        (while (< i len)
          (let ((ch (aref source i)))
            (cond
             ((and (not in-string) (= ch ??)
                   (or (= i 0) (memq (aref source (1- i))
                                     '(32 9 10 13 12 40 91 39 96 44))))
              (if (and (< (+ i 3) len)
                       (= (aref source (1+ i)) ?\\)
                       (= (aref source (+ i 2)) ?N)
                       (= (aref source (+ i 3)) ?{))
                  (setq i (1+ i))
                (setq i (cdr (read-from-string source i)))))
             ((and (not in-string) (= ch ?\;))
              (setq i (or (emacs-load--artifact-string-search "\n" source i) len)))
             ((= ch ?\") (setq in-string (not in-string) i (1+ i)))
             ((= ch ?\\)
              (if (and (< (+ i 2) len)
                       (= (aref source (1+ i)) ?N)
                       (= (aref source (+ i 2)) ?{)
                       (or in-string
                           (and (> i 0) (= (aref source (1- i)) ??)
                                (or (= i 1)
                                    (memq (aref source (- i 2))
                                          '(32 9 10 13 12 40 91 39 96 44))))))
                  (let ((end (emacs-load--artifact-string-search "}" source (+ i 3))))
                    (unless end
                      (signal 'invalid-read-syntax '("Unterminated \\N escape")))
                    (let ((name (substring source (+ i 3) end)))
                      (unless (string-prefix-p "U+" name)
                        (let ((code (emacs-load--unicode-name-code name)))
                          (push (substring source start i) out)
                          (push (if (<= code #xffff)
                                    (format "\\u%04X" code)
                                  (format "\\U%08X" code)) out)
                          (setq start (1+ end)))))
                    (setq i (1+ end)))
                (setq i (min len (+ i 2)))))
             (t (setq i (1+ i))))))
        (push (substring source start len) out)
        (apply #'concat (nreverse out)))))

  (defun emacs-load--read-file-string (path &optional literal)
    "Return PATH source as a string, decoding gzip load representations.
Return nil when unreadable.  The direct file reader bypasses GNU file
handlers, so a selected .gz representation must be decoded explicitly.
With LITERAL, preserve the original bytes used by .elc file offsets."
    (let ((source
           (cond
            ((and (string-suffix-p ".gz" path) (file-readable-p path))
             (with-temp-buffer
               (when literal
                 (set-buffer-multibyte nil))
               (let ((coding-system-for-read
                      (if literal 'no-conversion coding-system-for-read)))
                 (unless (equal (call-process "gzip" nil t nil "-cd" "--" path) 0)
                   (unless (get 'compression-error 'error-conditions)
                     (define-error 'compression-error nil 'file-error))
                   (signal 'compression-error (list "Cannot decompress load file" path))))
               (buffer-string)))
            ((fboundp 'nelisp--syscall-read-file)
             (if literal
                 (nelisp--syscall-read-file path t)
               (nelisp--syscall-read-file path)))
            ((file-readable-p path)
             (with-temp-buffer
               (if literal
                   (insert-file-contents-literally path)
                 (insert-file-contents path))
               (buffer-string)))
            (t nil))))
      (and source (emacs-load--byte-indexed-source source))))

  (defun emacs-load--artifact-load-path-present-p (path seen)
    "Return non-nil when PATH already appears in SEEN."
    (let ((found nil))
      (while (and seen (not found))
        (when (string= path (car seen))
          (setq found t))
        (setq seen (cdr seen)))
      found))

  (defun emacs-load--artifact-load-path-cli-args ()
    "Return stable repeated `--load-path' args from the active `load-path'."
    (let ((paths load-path)
          (seen nil)
          (args nil))
      (while paths
        (let ((entry (car paths)))
          (when (and (stringp entry)
                     (> (length entry) 0))
            (let ((expanded (expand-file-name entry)))
              (when (and (file-directory-p expanded)
                         (not (emacs-load--artifact-load-path-present-p
                               expanded seen)))
                (setq seen (cons expanded seen))
                (setq args (cons expanded
                                 (cons "--load-path" args)))))))
        (setq paths (cdr paths)))
      (nreverse args)))

  (defun emacs-load--artifact-string-search (needle haystack start)
    "Search HAYSTACK for NEEDLE from START.
Prefer the standalone native `nelisp--string-search' when available and
fall back to host `string-search' otherwise."
    (if (fboundp 'nelisp--string-search)
        (nelisp--string-search needle haystack start)
      (string-search needle haystack start)))

  (defun emacs-load--artifact-byte-at (string index)
    "Return byte at INDEX from STRING."
    (cond
     ((fboundp 'nelisp--string-byte-at)
      (nelisp--string-byte-at string index))
     ((fboundp 'string-byte)
      (string-byte string index))
     (t
      (aref string index))))

  (defun emacs-load--artifact-source-char-at (string index)
    "Return byte-indexed character at INDEX from SOURCE string STRING."
    (aref string index))

  (defun emacs-load--artifact-source-skip-ws-comments (source pos)
    "Return first non-whitespace/comment position in SOURCE at or after POS.
Same rule as `nelisp--load-skip-space-and-comments' (this was a duplicate
character-by-character implementation of the identical scan, sharing the
same T94 superlinear-`aref' and per-iteration-interpreter-overhead cost;
delegate instead of maintaining two copies of the fix)."
    (nelisp--load-skip-space-and-comments source pos))

  (defun emacs-load--artifact-source-rskip-ws (source pos)
    "Return last non-whitespace position in SOURCE at or before POS."
    (let ((done nil))
      (while (and (>= pos 0) (not done))
        (let ((ch (aref source pos)))
          (if (or (= ch ?\s) (= ch ?\t) (= ch ?\n) (= ch ?\r) (= ch ?\f))
              (setq pos (1- pos))
            (setq done t))))
      pos))

  (defun emacs-load--artifact-source-string-end (source pos)
    "Return one past the string starting at POS in SOURCE."
    (let ((len (length source)))
      (if (fboundp 'nelisp--rd-string-end-native)
          (let* ((read (nelisp--rd-string-end-native source (1+ pos) len))
                 (quote-pos (car-safe read)))
            (if (and (integerp quote-pos)
                     (>= quote-pos (1+ pos))
                     (< quote-pos len))
                (1+ quote-pos)
              (error "unterminated source string")))
        (let ((i (1+ pos))
              (escaped nil)
              (done nil))
          (while (and (< i len) (not done))
            (let ((ch (aref source i)))
              (cond
               (escaped
                (setq escaped nil))
               ((= ch ?\\)
                (setq escaped t))
               ((= ch ?\")
                (setq done t))))
            (setq i (1+ i)))
          (unless done
            (error "unterminated source string"))
          i))))

  (defun emacs-load--artifact-source-container-end (source pos)
    "Return one past the list/vector container starting at POS in SOURCE."
    (if (fboundp 'nelisp--source-container-end)
        (let ((end (nelisp--source-container-end source pos))
              (len (length source)))
          (if (and (integerp end)
                   (> end pos)
                   (<= end len))
              end
            (error "unterminated source container")))
      (let ((len (length source))
            (i pos)
            (depth 0)
            (in-string nil)
            (atom-escaped nil)
            (escaped nil)
            (done nil))
        (while (and (< i len) (not done))
          (let ((ch (aref source i)))
            (cond
             (in-string
              (cond
               (escaped
                (setq escaped nil))
               ((= ch ?\\)
                (setq escaped t))
               ((= ch ?\")
                (setq in-string nil))))
             (atom-escaped
              (setq atom-escaped nil))
             ((= ch ?\\)
              (setq atom-escaped t))
             ((= ch ?\")
              (setq in-string t))
             ((= ch ?\;)
              (while (and (< i len)
                          (not (= (aref source i) ?\n)))
                (setq i (1+ i))))
             ((or (= ch ?\() (= ch ?\[))
              (setq depth (1+ depth)))
             ((or (= ch ?\)) (= ch ?\]))
              (setq depth (1- depth))
              (when (= depth 0)
                (setq done t)))))
          (setq i (1+ i)))
        (unless done
          (error "unterminated source container"))
        i)))

  (defun emacs-load--artifact-source-atom-end (source pos)
    "Return one past the atom starting at POS in SOURCE."
    (let ((len (length source))
          (i pos)
          (done nil))
      (while (and (< i len) (not done))
        (let ((ch (aref source i)))
          (if (or (= ch ?\s) (= ch ?\t) (= ch ?\n) (= ch ?\r)
                  (= ch ?\f) (= ch ?\;) (= ch ?\() (= ch ?\))
                  (= ch ?\[) (= ch ?\]))
              (setq done t)
            (setq i (1+ i)))))
      i))

  (defun emacs-load--artifact-source-find-marker
      (source marker start path keyword &optional required)
    "Return MARKER position in SOURCE at or after START.
Signal a payload error when KEYWORD is missing from PATH unless REQUIRED
is nil.  MARKER is the exact top-level field token without assuming a
leading space, because the first plist key after the opening `(' has no
prefix space."
      (let ((pos (emacs-load--artifact-string-search marker source start)))
      (cond
       ((null pos)
        (when required
          (error "missing %S in %s" keyword path))
        nil)
       ((and (> pos 0)
             (let ((prev (emacs-load--artifact-source-char-at source (1- pos))))
               (not (or (= prev ?\()
                        (= prev ?\s)
                        (= prev ?\t)
                        (= prev ?\n)
                        (= prev ?\r)
                        (= prev ?\f)))))
        (error "invalid %S in %s" keyword path))
       (t pos))))

  (defun emacs-load--artifact-source-form-end (source pos)
    "Return one past the top-level form starting at POS in SOURCE."
    (let* ((len (length source))
           (pos (emacs-load--artifact-source-skip-ws-comments source pos)))
      (when (>= pos len)
        (error "no source form at end of input"))
      (let ((ch (emacs-load--artifact-source-char-at source pos)))
        (cond
         ((or (= ch ?\') (= ch ?`))
          (emacs-load--artifact-source-form-end source (1+ pos)))
         ((= ch ?,)
          (emacs-load--artifact-source-form-end
           source
           (if (and (< (1+ pos) len)
                    (= (emacs-load--artifact-source-char-at source (1+ pos))
                       ?@))
               (+ pos 2)
             (1+ pos))))
         ((and (= ch ?#)
               (< (1+ pos) len)
               (= (emacs-load--artifact-source-char-at source (1+ pos))
                  ?\'))
          (emacs-load--artifact-source-form-end source (+ pos 2)))
         ((and (= ch ?#)
               (< (1+ pos) len)
               (= (emacs-load--artifact-source-char-at source (1+ pos))
                  ?\())
          (emacs-load--artifact-source-container-end source (1+ pos)))
         ((or (= ch ?\() (= ch ?\[))
          (emacs-load--artifact-source-container-end source pos))
         ((= ch ?\")
          (emacs-load--artifact-source-string-end source pos))
         (t
          (emacs-load--artifact-source-atom-end source pos))))))

  (defun emacs-load--artifact-source-read-form (source pos &optional end)
    "Read one form from SOURCE at POS and return it with the new position."
    (let* ((form-end (or end
                         (emacs-load--artifact-source-form-end source pos)))
           (slice (emacs-load--reader-slice source pos form-end))
           (read (read-from-string slice 0 (length slice))))
      (unless (consp read)
        (error "invalid artifact form at %S" pos))
      (when (or (<= (cdr read) 0)
                (/= (cdr read) (length slice)))
        (error "invalid artifact form at %S" pos))
      (cons (car read) form-end)))

  (defun emacs-load--artifact-source-read-form-value (source range path keyword)
    "Read VALUE from SOURCE at RANGE for KEYWORD in PATH."
    (unless (consp range)
      (error "missing %S in %s" keyword path))
    (let ((read (emacs-load--artifact-source-read-form
                 source (car range) (cdr range))))
      (unless (and (consp read) (>= (cdr read) (cdr range)))
        (error "invalid %S in %s" keyword path))
      (car read)))

  (defun emacs-load--artifact-signal-invalid-read (keyword path caught)
    "Signal an invalid read for KEYWORD in PATH, preserving CAUGHT."
    (error "invalid %S in %s (%s)"
           keyword path (error-message-string caught)))

  (defun emacs-load--artifact-source-read-single-form-value
      (source range path keyword)
    "Read exactly one form from SOURCE at RANGE for KEYWORD in PATH."
    (unless (consp range)
      (error "missing %S in %s" keyword path))
    (let* ((start (car range))
           (end (cdr range))
           (slice (emacs-load--reader-slice source start end)))
      (if (fboundp 'nelisp--read-all-from-string-native)
          (let ((forms (condition-case caught
                           (nelisp--read-all-from-string-native slice)
                         (error
                          (emacs-load--artifact-signal-invalid-read
                           keyword path caught)))))
            (unless (and (consp forms) (null (cdr forms)))
              (error "invalid %S in %s" keyword path))
            (car forms))
        (let ((read (read-from-string slice 0 (length slice))))
          (unless (and (consp read)
                       (= (cdr read) (length slice)))
            (error "invalid %S in %s" keyword path))
          (car read)))))

  (defun emacs-load--artifact-source-read-incremental-form-value
      (source range path keyword)
    "Read exactly one incremental form slice from SOURCE at RANGE.
Use ordinary `read-from-string' on the slice and reject trailing input."
    (unless (consp range)
      (error "missing %S in %s" keyword path))
    (let* ((start (car range))
           (end (cdr range))
           (slice (emacs-load--reader-slice source start end))
           (read (read-from-string slice 0 (length slice))))
      (unless (and (consp read)
                   (= (cdr read) (length slice)))
        (error "invalid %S in %s" keyword path))
      (car read)))

  (defun emacs-load--artifact-source-read-form-nil-or-list-value
      (source range path keyword)
    "Read a nil or proper list VALUE from SOURCE at RANGE for KEYWORD in PATH."
    (unless (consp range)
      (error "missing %S in %s" keyword path))
    (let* ((start (car range))
           (end (cdr range))
           (pos (emacs-load--artifact-source-skip-ws-comments source start)))
      (if (emacs-load--artifact-native-reader-within-threshold-p
           source start end)
          (let ((forms (condition-case caught
                           (nelisp--read-all-from-string-native
                            (emacs-load--reader-slice source start end))
                         (error
                          (emacs-load--artifact-signal-invalid-read
                           keyword path caught)))))
            (unless (and (consp forms) (null (cdr forms)))
              (error "invalid %S in %s" keyword path))
            (let ((value (car forms)))
              (unless (proper-list-p value)
                (error "invalid %S in %s" keyword path))
              value))
        (cond
         ((>= pos end)
          (error "invalid %S in %s" keyword path))
         ((and (< (+ pos 2) end)
               (= (emacs-load--artifact-source-char-at source pos) ?n)
               (= (emacs-load--artifact-source-char-at source (1+ pos)) ?i)
               (= (emacs-load--artifact-source-char-at source (+ pos 2)) ?l))
          (unless (= (+ pos 3) end)
            (error "invalid %S in %s" keyword path))
          nil)
         ((/= (emacs-load--artifact-source-char-at source pos) ?\()
          (error "invalid %S in %s" keyword path))
         (t
          (let* ((list-open pos)
                 (list-end (emacs-load--artifact-source-form-end source pos))
                 (close (1- list-end)))
            (when (> list-end end)
              (error "invalid %S in %s" keyword path))
            (setq pos (1+ pos))
            (while (progn
                     (setq pos (emacs-load--artifact-source-skip-ws-comments
                                source pos))
                     (< pos close))
              (let ((item-end (emacs-load--artifact-source-form-end source pos)))
                (when (>= item-end list-end)
                  (error "invalid %S in %s" keyword path))
                (when (and (= (emacs-load--artifact-source-char-at source pos) ?.)
                           (= item-end (1+ pos)))
                  (error "invalid %S in %s" keyword path))
                (setq pos item-end)))
            (setq pos (emacs-load--artifact-source-skip-ws-comments source pos))
            (unless (and (< pos end)
                         (= (emacs-load--artifact-source-char-at source pos)
                            ?\)))
              (error "invalid %S in %s" keyword path))
            (setq pos (1+ pos))
            (unless (= pos end)
              (error "invalid %S in %s" keyword path))
            (let ((list-source (substring source (1+ list-open) close)))
              (if (or (fboundp 'nelisp--read-batch-vector-from-string-native)
                      (fboundp 'nelisp--read-batch-from-string-native))
                  (let ((items nil))
                    (emacs-load--artifact-source-iterate-form-nil-or-list-value-native-batch
                     list-source path keyword
                     (lambda (item)
                       (push item items)))
                    (nreverse items))
                (let ((items nil)
                      (count 0)
                      (item-pos (1+ list-open)))
                  (while (progn
                           (setq item-pos (emacs-load--artifact-source-skip-ws-comments
                                           source item-pos))
                           (< item-pos close))
                    (let ((item-end (emacs-load--artifact-source-form-end
                                     source item-pos)))
                      (push (emacs-load--artifact-source-read-incremental-form-value
                             source (cons item-pos item-end) path keyword)
                            items)
                      (setq item-pos item-end)
                      (setq count (1+ count))
                      (emacs-load--artifact-replay-pressure-maybe-gc)
                      (when (and load-garbage-collect-interval
                                 (> load-garbage-collect-interval 0)
                                 (= (% count load-garbage-collect-interval) 0)
                                 (emacs-load--collection-due-p))
                        (garbage-collect))))
                  (nreverse items))))))))))

  (defun emacs-load--artifact-source-iterate-form-nil-or-list-value
      (source range path keyword handler)
    "Iterate over a nil or proper list VALUE from SOURCE at RANGE.
Call HANDLER for each item in source order and return the number of items
processed."
    (unless (consp range)
      (error "missing %S in %s" keyword path))
    (let* ((start (car range))
           (end (cdr range))
           (pos (emacs-load--artifact-source-skip-ws-comments source start))
           (count 0))
      (cond
       ((>= pos end)
        (error "invalid %S in %s" keyword path))
       ((and (= (- end pos) 3)
             (= (emacs-load--artifact-source-char-at source pos) ?n)
             (= (emacs-load--artifact-source-char-at source (1+ pos)) ?i)
             (= (emacs-load--artifact-source-char-at source (+ pos 2)) ?l))
        (unless (= (+ pos 3) end)
          (error "invalid %S in %s" keyword path))
        0)
       ((/= (emacs-load--artifact-source-char-at source pos) ?\()
        (error "invalid %S in %s" keyword path))
       (t
        (let* ((list-end (emacs-load--artifact-source-form-end source pos))
               (close (1- list-end)))
          (when (> list-end end)
            (error "invalid %S in %s" keyword path))
          (setq pos (1+ pos))
          (while (progn
                   (setq pos (emacs-load--artifact-source-skip-ws-comments
                              source pos))
                   (< pos close))
            (let* ((item-start pos)
                   (item-end (emacs-load--artifact-source-form-end
                              source item-start)))
              (when (>= item-end list-end)
                (error "invalid %S in %s" keyword path))
              (when (and (= (emacs-load--artifact-source-char-at source pos) ?.)
                         (= item-end (1+ pos)))
                (error "invalid %S in %s" keyword path))
              (funcall handler
                       (emacs-load--artifact-source-read-single-form-value
                        source (cons item-start item-end) path keyword))
              (setq pos item-end)
              (setq count (1+ count))
              (emacs-load--artifact-replay-pressure-maybe-gc)
              (when (and load-garbage-collect-interval
                         (> load-garbage-collect-interval 0)
                         (= (% count load-garbage-collect-interval) 0)
                         (emacs-load--collection-due-p))
                (garbage-collect))))
          (setq pos (emacs-load--artifact-source-skip-ws-comments source pos))
          (unless (and (< pos end)
                       (= (emacs-load--artifact-source-char-at source pos)
                          ?\)))
            (error "invalid %S in %s" keyword path))
          (setq pos (1+ pos))
          (setq pos (min end
                         (emacs-load--artifact-source-skip-ws-comments
                          source pos)))
          (when (> pos end)
            (error "invalid %S in %s" keyword path))
          (unless (= pos end)
            (error "invalid %S in %s" keyword path))
          count)))))

  (defun emacs-load--artifact-source-iterate-form-nil-or-list-value-native-batch
      (source path keyword handler)
    "Iterate over a nil or proper list VALUE from SOURCE via native batch reader.
   SOURCE is the list interior (without surrounding parentheses).
   Call HANDLER for each item in source order and return the number of items
processed."
   (setq emacs-load--artifact-native-batch-diagnostic nil)
   (let* ((source-length (length source))
           (char-cursor 0)
           (count 0)
           (batch-size emacs-load-artifact-module-init-native-batch-items)
           (native-batch-size nil))
     (unless (integerp batch-size)
       (error "invalid %S in %s" keyword path))
     (unless (> batch-size 0)
       (error "invalid %S in %s" keyword path))
     (setq native-batch-size (min batch-size 32))
     (while (progn
              (setq char-cursor
                    (emacs-load--artifact-source-skip-ws-comments
                     source char-cursor))
              (< char-cursor source-length))
       (let ((slice-start char-cursor)
             (slice-end char-cursor)
             (slice-form-count 0))
         (condition-case caught
             (while (and (< slice-form-count native-batch-size)
                         (< char-cursor source-length))
               (setq slice-end
                     (emacs-load--artifact-source-form-end
                      source char-cursor))
               (unless (> slice-end char-cursor)
                 (error "invalid source form"))
               (setq slice-form-count (1+ slice-form-count))
               (setq char-cursor
                     (emacs-load--artifact-source-skip-ws-comments
                      source slice-end)))
           (error
            (setq emacs-load--artifact-native-batch-diagnostic
                  (list :stage :scan-error
                        :path path
                        :keyword keyword
                        :slice-start slice-start
                        :slice-end slice-end
                        :slice-chars (- slice-end slice-start)
                        :expected-items slice-form-count
                        :error (error-message-string caught)))
            (error "invalid %S in %s" keyword path)))
         (let* ((slice (emacs-load--reader-slice
                        source slice-start slice-end))
                (slice-byte-length
                 (emacs-load--artifact-byte-length slice))
                (vector-response-p
                 (fboundp 'nelisp--read-batch-vector-from-string-native))
                (response (condition-case caught
                              (if vector-response-p
                                  (nelisp--read-batch-vector-from-string-native
                                   slice 0 native-batch-size)
                                (nelisp--read-batch-from-string-native
                                 slice 0 native-batch-size))
                            (error
                             (setq emacs-load--artifact-native-batch-diagnostic
                                   (append
                                    (list :stage :native-error
                                          :path path
                                          :keyword keyword
                                          :slice-start slice-start
                                          :slice-end slice-end
                                          :slice-chars (length slice)
                                          :slice-bytes slice-byte-length
                                          :expected-items slice-form-count
                                          :error (error-message-string caught))
                                    (and emacs-load-artifact-native-batch-capture-failed-slice
                                         (list :slice-text slice))))
                             (error "invalid %S in %s" keyword path))))
                (items nil)
                (next nil)
                (actual-items nil))
          (unless (if vector-response-p
                      (vectorp response)
                    (consp response))
            (setq emacs-load--artifact-native-batch-diagnostic
                  (append
                   (list :stage (if vector-response-p
                                    :response-not-vector
                                  :response-not-cons)
                         :path path
                         :keyword keyword
                         :slice-start slice-start
                         :slice-end slice-end
                         :slice-chars (length slice)
                         :slice-bytes slice-byte-length
                         :expected-items slice-form-count)
                   (and emacs-load-artifact-native-batch-capture-failed-slice
                        (list :slice-text slice))))
            (error "invalid %S in %s" keyword path))
          (when vector-response-p
            (unless (= (length response) (+ native-batch-size 2))
              (setq emacs-load--artifact-native-batch-diagnostic
                    (append
                     (list :stage :response-vector-length
                           :path path
                           :keyword keyword
                           :slice-start slice-start
                           :slice-end slice-end
                           :slice-chars (length slice)
                           :slice-bytes slice-byte-length
                           :expected-items slice-form-count
                           :response-length (length response)
                           :expected-length (+ native-batch-size 2))
                     (and emacs-load-artifact-native-batch-capture-failed-slice
                          (list :slice-text slice))))
              (error "invalid %S in %s" keyword path)))
          (if vector-response-p
              (setq next (aref response 0)
                    actual-items (aref response 1))
            (setq items (car response)
                  next (cdr response)
                  actual-items (and (proper-list-p items) (length items))))
          (unless (or vector-response-p (proper-list-p items))
            (setq emacs-load--artifact-native-batch-diagnostic
                  (append
                   (list :stage :items-improper
                         :path path
                         :keyword keyword
                         :slice-start slice-start
                         :slice-end slice-end
                         :slice-chars (length slice)
                         :slice-bytes slice-byte-length
                         :expected-items slice-form-count
                         :actual-items :improper
                         :next (if (integerp next) next :non-integer))
                   (and emacs-load-artifact-native-batch-capture-failed-slice
                        (list :slice-text slice))))
            (error "invalid %S in %s" keyword path))
          (when vector-response-p
            (unless (integerp actual-items)
              (setq emacs-load--artifact-native-batch-diagnostic
                    (append
                     (list :stage :count-not-int
                           :path path
                           :keyword keyword
                           :slice-start slice-start
                           :slice-end slice-end
                           :slice-chars (length slice)
                           :slice-bytes slice-byte-length
                           :expected-items slice-form-count
                           :actual-items :non-integer
                           :next (if (integerp next) next :non-integer))
                     (and emacs-load-artifact-native-batch-capture-failed-slice
                          (list :slice-text slice))))
              (error "invalid %S in %s" keyword path))
            (unless (and (>= actual-items 0)
                         (<= actual-items native-batch-size))
              (setq emacs-load--artifact-native-batch-diagnostic
                    (append
                     (list :stage :count-out-of-bounds
                           :path path
                           :keyword keyword
                           :slice-start slice-start
                           :slice-end slice-end
                           :slice-chars (length slice)
                           :slice-bytes slice-byte-length
                           :expected-items slice-form-count
                           :actual-items actual-items
                           :next (if (integerp next) next :non-integer))
                     (and emacs-load-artifact-native-batch-capture-failed-slice
                          (list :slice-text slice))))
              (error "invalid %S in %s" keyword path)))
          (unless (integerp next)
            (setq emacs-load--artifact-native-batch-diagnostic
                  (append
                   (list :stage :next-not-int
                         :path path
                         :keyword keyword
                         :slice-start slice-start
                         :slice-end slice-end
                         :slice-chars (length slice)
                         :slice-bytes slice-byte-length
                         :expected-items slice-form-count
                         :actual-items actual-items
                         :next :non-integer)
                   (and emacs-load-artifact-native-batch-capture-failed-slice
                        (list :slice-text slice))))
            (error "invalid %S in %s" keyword path))
          (unless (>= next 0)
            (setq emacs-load--artifact-native-batch-diagnostic
                  (append
                   (list :stage :next-negative
                         :path path
                         :keyword keyword
                         :slice-start slice-start
                         :slice-end slice-end
                         :slice-chars (length slice)
                         :slice-bytes slice-byte-length
                         :expected-items slice-form-count
                         :actual-items actual-items
                         :next next)
                   (and emacs-load-artifact-native-batch-capture-failed-slice
                        (list :slice-text slice))))
            (error "invalid %S in %s" keyword path))
          (unless (= next slice-byte-length)
            (setq emacs-load--artifact-native-batch-diagnostic
                  (append
                   (list :stage :next-mismatch
                         :path path
                         :keyword keyword
                         :slice-start slice-start
                         :slice-end slice-end
                         :slice-chars (length slice)
                         :slice-bytes slice-byte-length
                         :expected-items slice-form-count
                         :actual-items actual-items
                         :next next)
                   (and emacs-load-artifact-native-batch-capture-failed-slice
                        (list :slice-text slice))))
            (error "invalid %S in %s" keyword path))
          (unless (= actual-items slice-form-count)
            (setq emacs-load--artifact-native-batch-diagnostic
                  (append
                   (list :stage :item-count-mismatch
                         :path path
                         :keyword keyword
                         :slice-start slice-start
                         :slice-end slice-end
                         :slice-chars (length slice)
                         :slice-bytes slice-byte-length
                         :expected-items slice-form-count
                         :actual-items actual-items
                         :next next)
                   (and emacs-load-artifact-native-batch-capture-failed-slice
                        (list :slice-text slice))))
            (error "invalid %S in %s" keyword path))
          (if vector-response-p
              (let ((item-index 0))
                (while (< item-index actual-items)
                  (let ((item (aref response (+ item-index 2))))
                    (funcall handler item)
                    (aset response (+ item-index 2) nil)
                    (setq item-index (1+ item-index))
                    (setq count (+ count 1))
                    (when (and load-garbage-collect-interval
                               (> load-garbage-collect-interval 0)
                               (= (% count load-garbage-collect-interval) 0)
                               (emacs-load--collection-due-p))
                      (garbage-collect)))))
            (while items
              (let ((item (car items)))
                (funcall handler item)
                (setq items (cdr items))
                (setq count (+ count 1))
                (setf (car response) items)
                (when (and load-garbage-collect-interval
                           (> load-garbage-collect-interval 0)
                           (= (% count load-garbage-collect-interval) 0)
                           (emacs-load--collection-due-p))
                  (garbage-collect)))))
          (if vector-response-p
              (progn
                (aset response 0 nil)
                (aset response 1 nil))
            (setf (car response) nil))
          (emacs-load--artifact-replay-pressure-maybe-gc))))
      count))

  (defun emacs-load--artifact-source-read-form-ranges (source start end path)
    "Return an alist of canonical top-level plist ranges from SOURCE."
      (let ((pos (emacs-load--artifact-source-skip-ws-comments source start))
          (pairs nil))
      (unless (and (< pos end)
                   (= (emacs-load--artifact-source-char-at source pos) ?\())
        (error "invalid artifact payload in %s" path))
      (setq pos (1+ pos))
      (while (progn
               (setq pos (emacs-load--artifact-source-skip-ws-comments
                          source pos))
               (< pos end))
        (when (= (emacs-load--artifact-source-char-at source pos) ?\))
          (setq pos end))
        (when (< pos end)
          (let* ((key-end (emacs-load--artifact-source-atom-end source pos))
                 (keyword-text (substring source pos key-end)))
            (unless (and (> (length keyword-text) 0)
                         (= (emacs-load--artifact-source-char-at keyword-text 0)
                            ?:))
              (error "invalid artifact payload key in %s" path))
            (setq pos (emacs-load--artifact-source-skip-ws-comments
                       source key-end))
            (when (>= pos end)
              (error "invalid artifact payload in %s" path))
            (let* ((value-start pos)
                   (value-end (emacs-load--artifact-source-form-end
                               source value-start))
                   (keyword (intern keyword-text)))
              (push (cons keyword (cons value-start value-end)) pairs)
              (setq pos value-end)))))
      (nreverse pairs)))

  (defun emacs-load--artifact-streaming-summary
      (path byte-length native-mode module-init-count field-names)
    "Return a compact truthy summary for large streaming artifact replays."
    (list :artifact path
          :streaming t
          :byte-length byte-length
          :native-mode native-mode
          :module-init-count module-init-count
          :fields field-names))

  (defun emacs-load--standalone-runtime-p ()
    "Return non-nil when native standalone SHA helpers should be preferred."
    (and (fboundp 'nelisp--sha256)
         (or (fboundp 'nelisp--write-stdout-bytes)
             (fboundp 'nelisp--eval-source-string))
         (or (and (fboundp 'emacs-standalone-mode-p)
                  (emacs-standalone-mode-p))
             (and (fboundp 'emacs-standalone-active-p)
                  (emacs-standalone-active-p))
             (not (boundp 'emacs-version))
             (not (stringp (and (boundp 'emacs-version) emacs-version))))))


  (defun emacs-load--sha256 (object)
    "Return the SHA-256 digest for OBJECT as a string."
    (cond
     ((emacs-load--standalone-runtime-p)
      (or (let ((digest (and (fboundp 'nelisp--sha256)
                             (nelisp--sha256 object))))
            (and digest digest))
          (and (fboundp 'secure-hash)
               (or (secure-hash 'sha256 object)
                   (error "secure-hash returned nil for SHA-256")))))
     ((fboundp 'secure-hash)
      (or (secure-hash 'sha256 object)
          (and (fboundp 'nelisp--sha256)
               (nelisp--sha256 object))
          (error "secure-hash returned nil for SHA-256")))
     ((fboundp 'nelisp--sha256)
      (nelisp--sha256 object))
     (t
      (error "no SHA-256 helper available"))))

  (defun emacs-load--artifact-cache-source-digest (source)
    "Return the salted source digest used by cache sidecars."
    (emacs-load--sha256
     (concat emacs-load--artifact-cache-source-digest-salt "\0" source)))

  (defun emacs-load--artifact-cache-create-directory (directory)
    "Create DIRECTORY and its parents for the standalone artifact cache.
The early standalone file shim installs a no-op `make-directory' when its
mkdir substrate is not yet loaded.  Fall back to NeLisp's raw mkdir syscall
one path component at a time when that shim leaves DIRECTORY absent.  Return
non-nil when DIRECTORY exists."
    (unless (file-directory-p directory)
      (make-directory directory t))
    (when (and (not (file-directory-p directory))
               (fboundp 'nelisp--syscall-path-int))
      (let* ((path (directory-file-name (expand-file-name directory)))
             (parent (file-name-directory path)))
        (when (and (stringp parent)
                   (not (string= parent path))
                   (not (file-directory-p parent)))
          (emacs-load--artifact-cache-create-directory parent))
        (unless (file-directory-p path)
          (condition-case nil
              (nelisp--syscall-path-int 83 path #o700)
            (error nil)))))
    (file-directory-p directory))

  (defun emacs-load--artifact-cache-paths (resolved)
    "Return cache artifact and sidecar paths for RESOLVED."
    (let* ((cache-directory (emacs-load--artifact-cache-directory))
           (hash (emacs-load--sha256 (expand-file-name resolved)))
           (prefix (substring hash 0 2))
           (suffix (substring hash 2))
           (base (file-name-nondirectory resolved))
           (artifact-dir (expand-file-name prefix cache-directory))
           (artifact
            (expand-file-name
             (concat suffix "-" base ".neln")
             artifact-dir))
           (sidecar (concat artifact ".source-sha256")))
      (emacs-load--artifact-cache-create-directory artifact-dir)
      (list artifact sidecar)))

  (defun emacs-load--artifact-cache-read-plist (path)
    "Read one plist from PATH, returning nil for an invalid cache record."
    (condition-case nil
        (let ((text (emacs-load--read-file-string path)))
          (and (stringp text)
               (let ((read (read-from-string text)))
                 (and (consp read)
                      (listp (car read))
                      (car read)))))
      (error nil)))

  (defun emacs-load--artifact-cache-record
      (source-hash compiler-identity &optional unreplayable error-text)
    "Return a cache record for SOURCE-HASH and COMPILER-IDENTITY."
    (append (list :cache-record-version 2
                  :source-sha256 source-hash
                  :compiler compiler-identity)
            (and unreplayable
                 (list :unreplayable t :error error-text))))

  (defun emacs-load--artifact-cache-state
      (artifact sidecar source-hash compiler-identity)
    "Return `positive' or `negative' for a matching cache record, else nil."
    (when (and compiler-identity (file-readable-p sidecar))
      (let* ((record (emacs-load--artifact-cache-read-plist sidecar))
             (version (and record
                           (plist-get record :cache-record-version)))
             (matching
              (and (memq version '(1 2))
                   (equal (plist-get record :source-sha256) source-hash)
                   (equal (plist-get record :compiler) compiler-identity))))
        (cond
         ((and matching
               (= version 2)
               (eq (plist-get record :unreplayable) t)
               (stringp (plist-get record :error)))
          'negative)
         ((and matching
               (file-readable-p artifact)
               (file-readable-p (concat artifact ".manifest.el"))
               (or (equal record
                          (list :cache-record-version 1
                                :source-sha256 source-hash
                                :compiler compiler-identity))
                   (equal record
                          (emacs-load--artifact-cache-record
                           source-hash compiler-identity))))
          'positive)))))

  (defun emacs-load--artifact-cache-write-record (sidecar record)
    "Write RECORD to the cache SIDECAR."
    (write-region (prin1-to-string record) nil sidecar nil 'silent))

  (defun emacs-load--artifact-cache-invalidate (artifact sidecar)
    "Delete the cache triple belonging to ARTIFACT and SIDECAR."
    (dolist (path (list artifact (concat artifact ".manifest.el") sidecar))
      (when (file-exists-p path)
        (delete-file path))))

  (defun emacs-load--artifact-compile-and-replay
      (resolved source source-hash compiler compiler-identity artifact sidecar
                &optional cached-error)
    "Compile and replay RESOLVED once, optionally recovering from CACHED-ERROR."
    (let ((temp (make-temp-file "emacs-load-artifact-" nil ".el")))
      (unwind-protect
          (progn
            (emacs-load--artifact-write-raw-source source temp)
            ;; No `--rewrite-defalias-late': the flag is not in any released
            ;; NeLisp CLI, and passing it makes compilation exit unsuccessfully.
            (let* ((command (list "compile-elisp-artifact"
                                  "--kind" "neln"
                                  "--input" temp
                                  "--output" artifact
                                  "--native-policy" "opportunistic"))
                   (command (append command
                                    (emacs-load--artifact-load-path-cli-args)))
                   (status (apply #'call-process compiler nil nil nil command)))
              (cond
               ((not (and (integerp status) (= status 0)))
                (setq emacs-load--artifact-compile-diagnostic-report
                      (list :resolved resolved
                            :compiler compiler
                            :compiler-identity compiler-identity
                            :status status
                            :artifact artifact
                            :sidecar sidecar
                            :source-hash source-hash
                            :temp temp
                            :command command))
                (emacs-load--artifact-cache-invalidate artifact sidecar)
                ;; Record the failure the same way a failed REPLAY is recorded
                ;; just below.  Without this the two failure paths are
                ;; asymmetric: a replay failure leaves a `negative' record and
                ;; is skipped next time, while a compile failure left nothing,
                ;; so every later load of the same unchanged source ran the
                ;; same failing compile again.  Measured 2026-09-12 on
                ;; vendor/emacs-lisp/epa.el: 188 s per load, every load,
                ;; discarded, with the source route then doing the real work in
                ;; 12 s.  `cache-state' already knows how to read this record
                ;; (version 2, `:unreplayable'), so one write turns a permanent
                ;; per-load cost into a one-off.
                (condition-case nil
                    (emacs-load--artifact-cache-write-record
                     sidecar
                     (emacs-load--artifact-cache-record
                      source-hash compiler-identity t
                      (format "compile-elisp-artifact exited with status %S"
                              status)))
                  (error nil))
                (if cached-error
                    (error "cached artifact failed (%s); recompilation failed with status %S"
                           (error-message-string cached-error) status)
                  nil))
               (t
                (let ((replay-error nil))
                  (condition-case caught
                      (emacs-load--artifact-replay-file artifact)
                    (error (setq replay-error caught)))
                  (if (not replay-error)
                      (progn
                        (emacs-load--artifact-cache-write-record
                         sidecar
                         (emacs-load--artifact-cache-record
                          source-hash compiler-identity))
                        (setq emacs-load--artifact-compile-diagnostic-report nil)
                        t)
                    (let ((error-text (error-message-string replay-error)))
                      (setq emacs-load--artifact-compile-diagnostic-report
                            (list :resolved resolved
                                  :compiler compiler
                                  :compiler-identity compiler-identity
                                  :status 'artifact-replay-failed
                                  :artifact artifact
                                  :sidecar sidecar
                                  :source-hash source-hash
                                  :temp temp
                                  :command command
                                  :error error-text))
                      (emacs-load--artifact-cache-invalidate artifact sidecar)
                      (condition-case nil
                          (emacs-load--artifact-cache-write-record
                           sidecar
                           (emacs-load--artifact-cache-record
                            source-hash compiler-identity t error-text))
                        (error nil))
                      nil)))))))
        (when (file-exists-p temp)
          (delete-file temp)))))

  (defun emacs-load--artifact-load-or-compile (resolved source)
    "Replay or compile a standalone artifact for RESOLVED and SOURCE."
    (when (and emacs-load-auto-native-compile
               (let ((maximum emacs-load-artifact-max-source-size))
                 (or (null maximum)
                     (and (integerp maximum)
                          (> maximum 0)
                          (<= (emacs-load--artifact-byte-length source)
                              maximum)))))
      (let ((compiler (emacs-load--artifact-compiler)))
        (when compiler
          (let* ((source-hash (emacs-load--artifact-cache-source-digest source))
                 (paths (emacs-load--artifact-cache-paths resolved))
                 (artifact (car paths))
                 (sidecar (cadr paths))
                 (compiler-identity
                  (emacs-load--artifact-compiler-identity compiler))
                 (cache-state
                  (emacs-load--artifact-cache-state
                   artifact sidecar source-hash compiler-identity)))
            (cond
             ((eq cache-state 'negative) nil)
             ((eq cache-state 'positive)
              (condition-case cached-error
                  (progn
                    (setq emacs-load--artifact-compile-diagnostic-report nil)
                    (emacs-load--artifact-replay-file artifact)
                    t)
                (error
                 (emacs-load--artifact-cache-invalidate artifact sidecar)
                 (emacs-load--artifact-compile-and-replay
                  resolved source source-hash compiler compiler-identity
                  artifact sidecar cached-error))))
             (t
              (emacs-load--artifact-cache-invalidate artifact sidecar)
              (emacs-load--artifact-compile-and-replay
               resolved source source-hash compiler compiler-identity
               artifact sidecar))))))))

  (defconst emacs-load--artifact-native-section-version 5
    "Current compact native section wire version supported by the loader.")

  (defconst emacs-load--artifact-native-runtime-prefix-layout-version 2
    "Compact `:runtime-prefix' layout version supported by the loader.")

  (defconst emacs-load--artifact-native-compact-reloc-format 'indexed-plt32-v1
    "Compact relocation format used by the v5 native artifact wire.")

  (defun emacs-load--artifact-native-runtime-prefix-p (value)
    "Return non-nil when VALUE is a supported compact runtime prefix."
    (or (and (vectorp value)
             (= (length value) 10)
             (= (aref value 0)
                emacs-load--artifact-native-runtime-prefix-layout-version))
        (and (listp value)
             (plist-member value :runtime-prefix-char-size)
             (plist-member value :arch)
             (plist-member value :text-base64)
             (plist-member value :extern-symbols)
             (plist-member value :defuns))))

  (defun emacs-load--artifact-native-v5-section-p (value)
    "Return non-nil when VALUE is a raw v5 compact native section."
    (and (listp value)
         (= (or (plist-get value :native-section-version) 0)
            emacs-load--artifact-native-section-version)
         (emacs-load--artifact-native-runtime-prefix-p
          (plist-get value :runtime-prefix))))

  (defun emacs-load--artifact-native-section-get (section key)
    "Return KEY from SECTION across legacy and v5 compact wire shapes."
    (if (emacs-load--artifact-native-v5-section-p section)
        (let ((runtime (plist-get section :runtime-prefix)))
          (if (vectorp runtime)
              (pcase key
                (:native-section-version
                 emacs-load--artifact-native-section-version)
                (:runtime-prefix runtime)
                (:runtime-prefix-char-size (aref runtime 1))
                (:arch (aref runtime 2))
                (:symbols (aref runtime 3))
                (:text-base64 (aref runtime 4))
                (:reloc-format (aref runtime 5))
                (:reloc-count (aref runtime 6))
                (:reloc-data (aref runtime 7))
                (:extern-symbols (aref runtime 8))
                (:defuns (aref runtime 9))
                (_ (plist-get section key)))
            (or (plist-get runtime key)
                (plist-get section key))))
      (plist-get section key)))

  (defun emacs-load--artifact-native-compact-relocs-p (section)
    "Return non-nil when SECTION carries supported compact reloc metadata."
    (let ((format (emacs-load--artifact-native-section-get section :reloc-format))
          (count (emacs-load--artifact-native-section-get section :reloc-count))
          (data (emacs-load--artifact-native-section-get section :reloc-data)))
      (and (eq format emacs-load--artifact-native-compact-reloc-format)
           (integerp count)
           (>= count 0)
           (or (null data) (vectorp data) (listp data))
           (= (length data) (* count 3)))))

  (defun emacs-load--artifact-native-section-relocs (section path)
    "Return SECTION relocations in canonical legacy plist form."
    (if (and (listp section)
             (plist-member section :relocs))
        (plist-get section :relocs)
      (let ((externs (emacs-load--artifact-native-section-get
                      section :extern-symbols))
            (data (emacs-load--artifact-native-section-get
                   section :reloc-data))
            (count (emacs-load--artifact-native-section-get
                    section :reloc-count))
            (out nil))
        (unless (and (emacs-load--artifact-native-compact-relocs-p section)
                     (listp externs))
          (error "invalid native section in %s" path))
        (dotimes (i count)
          (let* ((base (* i 3))
                 (symbol-index (elt data (1+ base)))
                 (symbol (and (integerp symbol-index)
                              (>= symbol-index 0)
                              (< symbol-index (length externs))
                              (nth symbol-index externs))))
            (unless (stringp symbol)
              (error "invalid native section in %s" path))
            (push (list :offset (elt data base)
                        :type 'plt32
                        :symbol symbol
                        :addend (elt data (+ base 2)))
                  out)))
        (nreverse out))))

  (defun emacs-load--artifact-native-plist-p (value)
    "Return non-nil when VALUE looks like native function metadata."
    (or (and (listp value)
             (plist-member value :arch)
             (plist-member value :text-base64)
             (plist-member value :relocs)
             (plist-member value :extern-symbols)
             (plist-member value :defuns))
        (emacs-load--artifact-native-v5-section-p value)))

  (defconst emacs-load--artifact-native-extern-whitelist
    '("nl_alloc_symbol"
      "nl_alloc_str"
      "nl_alloc_mut_str"
      "nl_mut_str_push_byte"
      "nl_mut_str_finalize"
      "nelisp_aot_builtin_call1"
      "nelisp_aot_builtin_calln")
    "Runtime extern names that may be replayed in-process.")

  (defun emacs-load--artifact-native-content-key (native)
    "Return a cache key string for NATIVE contents."
    (emacs-load--sha256
     (prin1-to-string
      (list :text-base64
            (emacs-load--artifact-native-section-get native :text-base64)
            :relocs
            (emacs-load--artifact-native-section-relocs native "<cache-key>")
            :extern-symbols
            (emacs-load--artifact-native-section-get native :extern-symbols)))))

  (defun emacs-load--artifact-native-extern-allowed-p (symbol)
    "Return non-nil when SYMBOL is whitelisted for runtime resolution."
    (and (stringp symbol)
         (member symbol emacs-load--artifact-native-extern-whitelist)))

  (defun emacs-load--artifact-native-prefix-eligible-p (native)
    "Return non-nil when NATIVE satisfies the cheap eligibility prefix."
    (and (fboundp 'nelisp--runtime-symbol-address)
         (fboundp 'nelisp--native-call-boundary)
         (equal (emacs-load--artifact-native-section-get native :arch) "x86_64")
         (let ((extern-symbols
                (emacs-load--artifact-native-section-get
                 native :extern-symbols)))
           (and (proper-list-p extern-symbols)
                (catch 'unsupported
                  (dolist (symbol extern-symbols t)
                    (unless (emacs-load--artifact-native-extern-allowed-p symbol)
                      (throw 'unsupported nil))))))))

  (defun emacs-load--artifact-native-form-kind-from-range
      (source range path keyword)
    "Return the syntactic kind of SOURCE at RANGE for KEYWORD in PATH."
    (unless (consp range)
      (error "invalid native section in %s" path))
    (let* ((start (car range))
           (end (cdr range))
           (pos (emacs-load--artifact-source-skip-ws-comments source start)))
      (when (>= pos end)
        (error "invalid %S in %s" keyword path))
      (let ((ch (emacs-load--artifact-source-char-at source pos)))
        (cond
         ((= ch ?\") :string)
         ((= ch ?\() :list)
         ((and (= ch ?n)
               (= (+ pos 3) end)
               (= (emacs-load--artifact-source-char-at source (1+ pos)) ?i)
               (= (emacs-load--artifact-source-char-at source (+ pos 2)) ?l))
          :nil)
         (t
          (error "invalid %S in %s" keyword path))))))

  (defun emacs-load--artifact-native-section-preflight-from-ranges
      (source ranges path)
    "Return non-nil when RANGES describe a cheap-eligible native section."
    (let ((version-range (assq :native-section-version ranges))
          (runtime-prefix-range (assq :runtime-prefix ranges)))
      (if (or version-range runtime-prefix-range)
          (progn
            (unless (and version-range runtime-prefix-range
                         (= (or (emacs-load--artifact-source-read-single-form-value
                                 source (cdr version-range) path
                                 :native-section-version)
                                0)
                            emacs-load--artifact-native-section-version))
              (error "invalid native section in %s" path))
            t)
        (let ((arch-range (assq :arch ranges))
              (text-range (assq :text-base64 ranges))
              (relocs-range (assq :relocs ranges))
              (externs-range (assq :extern-symbols ranges))
              (defuns-range (assq :defuns ranges)))
          (unless (and arch-range text-range relocs-range externs-range defuns-range
                       (eq (emacs-load--artifact-native-form-kind-from-range
                            source (cdr arch-range) path :arch)
                           :string)
                       (eq (emacs-load--artifact-native-form-kind-from-range
                            source (cdr text-range) path :text-base64)
                           :string)
                       (memq (emacs-load--artifact-native-form-kind-from-range
                              source (cdr relocs-range) path :relocs)
                             '(:nil :list))
                       (memq (emacs-load--artifact-native-form-kind-from-range
                              source (cdr externs-range) path :extern-symbols)
                             '(:nil :list))
                       (memq (emacs-load--artifact-native-form-kind-from-range
                              source (cdr defuns-range) path :defuns)
                             '(:nil :list)))
            (error "invalid native section in %s" path))
          (emacs-load--artifact-native-prefix-eligible-p
           (list :arch
                 (emacs-load--artifact-source-read-form-value
                  source (cdr arch-range) path :arch)
                 :extern-symbols
                 (emacs-load--artifact-source-read-form-nil-or-list-value
                  source (cdr externs-range) path :extern-symbols)))))))

  (defun emacs-load--artifact-native-decode-text (text &optional path quiet)
    "Return decoded bytes for base64 TEXT.
When QUIET is non-nil, return nil instead of signaling on decoder failures."
    (let ((decoded
           (cond
            ((fboundp 'nelisp--base64-decode-bytes)
             (nelisp--base64-decode-bytes text))
            ((fboundp 'base64-decode-string)
             (base64-decode-string text))
            (quiet nil)
            (t
             (error "no base64 decoder available for %S" (or path text))))))
      (cond
       ((stringp decoded)
        (let ((decoded-bytes (emacs-load--artifact-byte-length decoded)))
          (if (and (> (emacs-load--artifact-byte-length text) 0)
                   (<= decoded-bytes 0))
              (if quiet
                  nil
                (error "decoded native payload is empty for %S" (or path text)))
            decoded)))
       (quiet nil)
       (t
        (error "invalid native payload decode for %S" (or path text))))))

  (defun emacs-load--artifact-native-rel32-value (stub-address addend patch-address)
    "Return the unsigned 32-bit rel32 value for STUB-ADDRESS, ADDEND, and PATCH-ADDRESS."
    (let ((delta (+ stub-address addend (- patch-address))))
      (unless (and (integerp delta)
                   (<= -2147483648 delta)
                   (<= delta 2147483647))
        (error "native rel32 relocation out of range: %S" delta))
      (logand delta #xffffffff)))

  (defun emacs-load--artifact-native-write-u64 (base offset value u8-fn u32-fn)
    "Write VALUE as an unsigned 64-bit little-endian integer at BASE+OFFSET."
    (let ((low (logand value #xffffffff))
          (high (logand (lsh value -32) #xffffffff)))
      (if u32-fn
          (progn
            (funcall u32-fn base offset low)
            (funcall u32-fn base (+ offset 4) high))
        (funcall u8-fn base offset (logand low #xff))
        (funcall u8-fn base (+ offset 1) (logand (lsh low -8) #xff))
        (funcall u8-fn base (+ offset 2) (logand (lsh low -16) #xff))
        (funcall u8-fn base (+ offset 3) (logand (lsh low -24) #xff))
        (funcall u8-fn base (+ offset 4) (logand high #xff))
        (funcall u8-fn base (+ offset 5) (logand (lsh high -8) #xff))
        (funcall u8-fn base (+ offset 6) (logand (lsh high -16) #xff))
        (funcall u8-fn base (+ offset 7) (logand (lsh high -24) #xff)))))

  (defun emacs-load--artifact-native-write-stub (base offset runtime-address u8-fn u32-fn)
    "Write a 16-byte near stub at BASE+OFFSET for RUNTIME-ADDRESS."
    (let ((stub-address (+ base offset)))
      (funcall u8-fn base offset 72)
      (funcall u8-fn base (+ offset 1) 184)
      (emacs-load--artifact-native-write-u64
       base (+ offset 2) runtime-address u8-fn u32-fn)
      (funcall u8-fn base (+ offset 10) 255)
      (funcall u8-fn base (+ offset 11) 224)
      (funcall u8-fn base (+ offset 12) 0)
      (funcall u8-fn base (+ offset 13) 0)
      (funcall u8-fn base (+ offset 14) 0)
      (funcall u8-fn base (+ offset 15) 0)
      stub-address))

  (defun emacs-load--artifact-native-validate-externs (extern-symbols path)
    "Validate extern metadata and return a deduplicated symbol list."
    (let ((seen nil)
          (result nil))
      (dolist (symbol extern-symbols)
        (unless (emacs-load--artifact-native-extern-allowed-p symbol)
          (error "unsupported native extern %S in %s"
                 symbol (or path "<native>")))
        (unless (member symbol seen)
          (push symbol seen)
          (push symbol result)))
      (nreverse result)))

  (defun emacs-load--artifact-native-validate-relocs (relocs extern-symbols text-bytes path)
    "Validate RELOCS against EXTERN-SYMBOLS for TEXT-BYTES in PATH."
    (dolist (reloc relocs)
      (unless (listp reloc)
        (error "invalid native relocation entry %S in %s"
               reloc (or path "<native>")))
      (let ((offset (plist-get reloc :offset))
            (type (plist-get reloc :type))
            (symbol (plist-get reloc :symbol))
            (addend (plist-get reloc :addend)))
        (unless (eq type 'plt32)
          (error "unsupported native relocation type %S for %S in %s"
                 type symbol (or path "<native>")))
        (unless (and (stringp symbol)
                     (member symbol extern-symbols))
          (error "native relocation references unknown extern %S in %s"
                 symbol (or path "<native>")))
        (unless (and (integerp offset)
                     (<= 0 offset)
                     (<= (+ offset 4) text-bytes))
          (error "native relocation offset %S out of range for %s"
                 offset (or path "<native>")))
        (unless (integerp addend)
          (error "native relocation addend %S is not an integer for %s"
                 addend (or path "<native>"))))))

  (defun emacs-load--artifact-native-validate-defuns (defuns text-bytes path)
    "Validate native DEFUNS metadata against TEXT-BYTES in PATH."
    (dolist (entry defuns)
      (unless (listp entry)
        (error "invalid native defun entry %S in %s"
               entry (or path "<native>")))
      (let ((name (plist-get entry :name))
            (offset (plist-get entry :offset))
            (body-offset (plist-get entry :body-offset))
            (arity (plist-get entry :arity))
            (rt-slot-count (plist-get entry :rt-slot-count)))
        (unless (and (integerp offset)
                     (integerp body-offset)
                     (integerp rt-slot-count)
                     (integerp arity)
                     (<= 0 arity 6)
                     (<= 0 offset)
                     (<= 0 body-offset)
                     (<= 0 rt-slot-count)
                     (< (+ offset body-offset) text-bytes))
          (error "invalid native metadata for %S in %s"
                 name (or path "<native>"))))))

  (defun emacs-load--artifact-byte-length (string)
    "Return the byte length of STRING."
    (if (fboundp 'string-bytes)
        (string-bytes string)
      (length string)))

  (defun emacs-load--artifact-find-defuns-metadata (name native-context)
    "Find native :defuns metadata for NAME inside NATIVE-CONTEXT."
    (let* ((target (if (symbolp name) (symbol-name name) name))
           (defuns (and (listp native-context)
                        (emacs-load--artifact-native-section-get
                         native-context :defuns)))
           (found nil))
      (while (and defuns (not found))
        (let ((entry (pop defuns)))
          (when (and (listp entry)
                     (equal (plist-get entry :name) target))
            (setq found entry))))
      found))

  (defun emacs-load--artifact-defuns-metadata-name-key (name)
    "Return the canonical metadata lookup key for NAME."
    (if (symbolp name) (symbol-name name) name))

  (defun emacs-load--artifact-native-section-well-formed-p (native)
    "Return non-nil when NATIVE has structurally valid native metadata."
    (and (emacs-load--artifact-native-plist-p native)
         (stringp (emacs-load--artifact-native-section-get native :arch))
         (stringp (emacs-load--artifact-native-section-get native :text-base64))
         (or (and (listp native)
                  (plist-member native :relocs)
                  (listp (plist-get native :relocs)))
             (emacs-load--artifact-native-compact-relocs-p native))
         (listp (emacs-load--artifact-native-section-get native :extern-symbols))
         (listp (emacs-load--artifact-native-section-get native :defuns))))

  (defun emacs-load--artifact-native-section-canonicalize (native path)
    "Return a canonical native section plist for NATIVE."
    (unless (emacs-load--artifact-native-section-well-formed-p native)
      (error "invalid native section in %s" path))
    (list :arch (emacs-load--artifact-native-section-get native :arch)
          :text-base64
          (emacs-load--artifact-native-section-get native :text-base64)
          :relocs (emacs-load--artifact-native-section-relocs native path)
          :extern-symbols
          (emacs-load--artifact-native-section-get native :extern-symbols)
          :defuns
          (emacs-load--artifact-native-section-get native :defuns)))

  (defun emacs-load--artifact-native-section-compact (native path)
    "Return compact replay metadata for NATIVE."
    (unless (emacs-load--artifact-native-section-well-formed-p native)
      (error "invalid native section in %s" path))
    (list :arch (emacs-load--artifact-native-section-get native :arch)
          :defuns (emacs-load--artifact-native-section-get native :defuns)
          :path path
          :text-hash
          (emacs-load--sha256
           (emacs-load--artifact-native-section-get native :text-base64))))

  (defun emacs-load--artifact-native-section-replay-hash (native)
    "Return the diagnostic text hash for NATIVE."
    (or (plist-get native :text-hash)
        (let ((text (emacs-load--artifact-native-section-get
                     native :text-base64)))
          (and (stringp text)
               (emacs-load--sha256 text)))))

  (defun emacs-load--artifact-native-section-from-ranges
      (source ranges path)
    "Materialize a canonical native section plist from RANGES in SOURCE."
    (let* ((version-range (assq :native-section-version ranges))
           (runtime-prefix-range (assq :runtime-prefix ranges))
           (version
            (and version-range
                 (emacs-load--artifact-source-read-single-form-value
                  source (cdr version-range) path :native-section-version))))
      (cond
       ((or runtime-prefix-range
            (= (or version 0) emacs-load--artifact-native-section-version))
        (unless (and (= (or version 0) emacs-load--artifact-native-section-version)
                     runtime-prefix-range)
          (error "invalid native section in %s" path))
        (emacs-load--artifact-native-section-canonicalize
         (list :native-section-version version
               :runtime-prefix
               (emacs-load--artifact-source-read-single-form-value
                source (cdr runtime-prefix-range) path :runtime-prefix))
         path))
       (t
        (emacs-load--artifact-native-section-canonicalize
         (list :arch (emacs-load--artifact-source-read-form-value
                      source (cdr (assq :arch ranges)) path :arch)
               :text-base64 (emacs-load--artifact-source-read-form-value
                             source (cdr (assq :text-base64 ranges))
                             path :text-base64)
               :relocs (emacs-load--artifact-source-read-form-nil-or-list-value
                        source (cdr (assq :relocs ranges)) path :relocs)
               :extern-symbols
               (emacs-load--artifact-source-read-form-nil-or-list-value
                source (cdr (assq :extern-symbols ranges))
                path :extern-symbols)
               :defuns
               (emacs-load--artifact-source-read-form-nil-or-list-value
                source (cdr (assq :defuns ranges)) path :defuns))
         path)))))

  (defun emacs-load--artifact-native-sections-use-native-reader-p
      (source start end)
    "Return non-nil when SOURCE between START and END should use native read-all."
    (emacs-load--artifact-native-reader-within-threshold-p source start end))

  (defun emacs-load--artifact-native-sections-from-source
      (source start end path &optional handler)
    "Return canonical native section plists from SOURCE between START and END.
When HANDLER is non-nil, call it for each canonical section and collect the
non-nil return values instead of retaining the original section objects."
    (let ((pos (emacs-load--artifact-source-skip-ws-comments source start))
          (sections nil)
          (count 0))
      (cond
      ((>= pos end)
        (error "invalid native sections in %s" path))
       ((emacs-load--artifact-native-sections-use-native-reader-p source start end)
        (let* ((forms (condition-case nil
                          (nelisp--read-all-from-string-native
                           (emacs-load--reader-slice source start end))
                        (error
                         (error "invalid native sections in %s" path))))
               (value nil))
          (unless (and (consp forms)
                       (null (cdr forms)))
            (error "invalid native sections in %s" path))
          (setq value (car forms))
          (unless (proper-list-p value)
            (error "invalid native sections in %s" path))
          (dolist (section value)
            (let ((canonical
                   (emacs-load--artifact-native-section-canonicalize
                    section path)))
              (if handler
                  (let ((handled (funcall handler canonical)))
                    (when handled
                      (push handled sections)))
                (push canonical sections))))))
       ((and (= (- end pos) 3)
             (= (emacs-load--artifact-source-char-at source pos) ?n)
             (= (emacs-load--artifact-source-char-at source (+ pos 1)) ?i)
             (= (emacs-load--artifact-source-char-at source (+ pos 2)) ?l))
        nil)
       ((not (= (emacs-load--artifact-source-char-at source pos) ?\())
        (error "invalid native sections in %s" path))
       (t
        (setq pos (1+ pos))
        (while (progn
                 (setq pos (emacs-load--artifact-source-skip-ws-comments
                            source pos))
                 (< pos end))
          (when (= (emacs-load--artifact-source-char-at source pos) ?\))
            (setq pos end))
          (when (< pos end)
            (let* ((section-start pos)
                   (section-end (emacs-load--artifact-source-form-end
                                 source section-start))
                   (section-source (substring source section-start section-end))
                   (section-length (length section-source)))
              (let ((section
                     (emacs-load--artifact-native-section-from-ranges
                      section-source
                      (emacs-load--artifact-source-read-form-ranges
                       section-source 0 section-length path)
                      path)))
                (if handler
                    (let ((handled (funcall handler section)))
                      (when handled
                        (push handled sections)))
                  (push section sections)))
              (setq pos section-end))))
          (setq count (1+ count))
          (emacs-load--periodic-gc count)))
        (nreverse sections)))

  (defun emacs-load--artifact-native-sections-from-payload (payload path)
    "Return sharded native sections from PAYLOAD, validating structure."
    (let ((sections (plist-get payload :native-sections)))
      (unless (listp sections)
        (error "invalid native sections in %s" path))
      (let ((result nil))
        (dolist (section sections (nreverse result))
          (push (emacs-load--artifact-native-section-canonicalize
                 section path)
                result)))))

  (defun emacs-load--artifact-native-section-bases (sections path)
    "Return eligible native SECTIONS paired with their mapped base addresses."
    (let ((result nil))
      (dolist (section sections (nreverse result))
        (when (emacs-load--artifact-native-eligible-p section)
          (push (cons section
                      (emacs-load--artifact-native-map-section section path))
                result)))))

  (defun emacs-load--artifact-find-defuns-metadata-in-sections (name section-bases path)
    "Find native :defuns metadata for NAME across SECTION-BASES.
Return a list of the matching native section, base address, and metadata."
    (let ((matches nil))
      (dolist (entry section-bases)
        (let ((section (car entry))
              (base (cdr entry)))
          (let ((meta (emacs-load--artifact-find-defuns-metadata name section)))
            (when meta
              (push (list section base meta) matches)))))
      (setq matches (nreverse matches))
      (cond
       ((null matches) nil)
       ((null (cdr matches)) (car matches))
       (t
        (error "native function %S appears in multiple sections in %s"
               name (or path "<artifact>"))))))

  (defun emacs-load--artifact-build-defuns-metadata-index (section-bases path)
    "Return a hash index of native defun metadata for SECTION-BASES."
    (let ((index (make-hash-table :test 'equal)))
      (dolist (entry section-bases index)
        (let ((section (car entry))
              (base (cdr entry))
              (defuns nil))
          (setq defuns (and (listp section)
                            (plist-get section :defuns)))
          (dolist (meta defuns)
            (let ((key (emacs-load--artifact-defuns-metadata-name-key
                        (plist-get meta :name))))
              (when key
                (when (gethash key index)
                  (error "native function %S appears in multiple sections in %s"
                         key (or path "<artifact>")))
                (puthash key (list section base meta) index))))))))

  (defun emacs-load--artifact-find-defuns-metadata-in-index (name index)
    "Return the indexed native metadata match for NAME from INDEX."
    (and index
         (gethash (emacs-load--artifact-defuns-metadata-name-key name) index)))

  (defun emacs-load--artifact-native-eligible-p (native)
    "Return non-nil when NATIVE can be replayed in-process."
    (and (emacs-load--artifact-native-plist-p native)
         (stringp (emacs-load--artifact-native-section-get native :text-base64))
         (listp (emacs-load--artifact-native-section-get native :defuns))
         (emacs-load--artifact-native-prefix-eligible-p native)
         (let ((relocs (emacs-load--artifact-native-section-relocs
                        native "<native>"))
               (extern-symbols
                (emacs-load--artifact-native-section-get
                 native :extern-symbols)))
           (let ((decoded
                  (emacs-load--artifact-native-decode-text
                   (emacs-load--artifact-native-section-get
                    native :text-base64)
                   nil t))
                 (text-bytes nil))
             (cond
              ((null decoded)
               nil)
              ((and (null relocs)
                    (null extern-symbols))
               t)
              ((or (null relocs)
                   (null extern-symbols))
               nil)
              (t
               (setq text-bytes (emacs-load--artifact-byte-length decoded))
               (let ((deduped-externs
                      (emacs-load--artifact-native-validate-externs
                       extern-symbols nil)))
                 (emacs-load--artifact-native-validate-relocs
                  relocs deduped-externs
                  text-bytes
                  nil)
                 (emacs-load--artifact-native-validate-defuns
                  (emacs-load--artifact-native-section-get native :defuns)
                  text-bytes
                  nil)
                 t)))))))

  (defun emacs-load--artifact-bcl-replay-available-p ()
    "Return non-nil when the legacy BCL replay substrate is available."
    (and (boundp 'nelisp--functions)
         (fboundp 'nelisp--apply)))

  (defun emacs-load--artifact-source-defun-fallback-valid-p (name source-defun)
    "Return non-nil when SOURCE-DEFUN is an exact defun for NAME."
    (and (consp source-defun)
         (eq (car source-defun) 'defun)
         (let ((defun-name (nth 1 source-defun)))
           (and (symbolp defun-name)
                (equal (if (symbolp name)
                           (symbol-name name)
                         name)
                       (symbol-name defun-name))))))

  (defun emacs-load--artifact-fn-source-defun (item path)
    "Return ITEM's source-defun fallback or signal if malformed."
    (let ((tail (nthcdr 3 item)))
      (when tail
        (let ((source-defun (car tail)))
          (unless (and (null (nthcdr 4 item))
                       (emacs-load--artifact-source-defun-fallback-valid-p
                        (nth 1 item) source-defun))
            (error "invalid source defun fallback for %S in artifact%s"
                   (nth 1 item)
                   (if path (format " (%s)" path) "")))
          source-defun))))

  (defun emacs-load--artifact-native-lambda-form (body-address arity rt-slot-count)
    "Build a fixed-arity native call lambda form."
    (pcase arity
      (0 `(lambda nil
            (nelisp--native-call-boundary ,body-address ,arity ,rt-slot-count)))
      (1 `(lambda (a0)
            (nelisp--native-call-boundary ,body-address ,arity ,rt-slot-count a0)))
      (2 `(lambda (a0 a1)
            (nelisp--native-call-boundary ,body-address ,arity ,rt-slot-count a0 a1)))
      (3 `(lambda (a0 a1 a2)
            (nelisp--native-call-boundary ,body-address ,arity ,rt-slot-count a0 a1 a2)))
      (4 `(lambda (a0 a1 a2 a3)
            (nelisp--native-call-boundary ,body-address ,arity ,rt-slot-count a0 a1 a2 a3)))
      (5 `(lambda (a0 a1 a2 a3 a4)
            (nelisp--native-call-boundary ,body-address ,arity ,rt-slot-count a0 a1 a2 a3 a4)))
      (6 `(lambda (a0 a1 a2 a3 a4 a5)
            (nelisp--native-call-boundary ,body-address ,arity ,rt-slot-count a0 a1 a2 a3 a4 a5)))
      (_ (error "unsupported native arity %S" arity))))

  (defun emacs-load--artifact-native-map-section (native &optional path)
    "Map the native text for NATIVE and return its base address."
    (let* ((text (emacs-load--artifact-native-section-get native :text-base64))
           (relocs (or (emacs-load--artifact-native-section-relocs
                        native (or path "<native>"))
                       nil))
           (extern-symbols
            (or (emacs-load--artifact-native-section-get
                 native :extern-symbols)
                nil))
           (cache-key (emacs-load--artifact-native-content-key native))
           (base (gethash cache-key emacs-load--artifact-native-hash-cache))
           (decoded nil)
           (text-bytes nil)
           (total-bytes nil)
           (page-size nil)
           (rounded-page-size nil)
           (syscall-direct-fn nil)
           (ptr-write-u8-fn nil)
           (ptr-copy-string-bytes-fn nil)
           (ptr-write-u32-fn nil)
           (stub-symbols nil)
           (stub-count 0)
           (stub-addresses nil))
      (unless (and (integerp base) (> base 0))
        (setq decoded
              (emacs-load--artifact-native-decode-text text path))
        (setq text-bytes (emacs-load--artifact-byte-length decoded))
        (setq page-size
              (or (and (fboundp 'page-size) (page-size))
                  4096))
        (setq stub-symbols
              (if (and (null relocs) (null extern-symbols))
                  nil
                (progn
                  (when (or (null relocs) (null extern-symbols))
                    (error "native section must provide both relocs and extern-symbols for %S"
                           (or path text)))
                  (emacs-load--artifact-native-validate-externs
                   extern-symbols path))))
        (setq stub-count (length stub-symbols))
        (emacs-load--artifact-native-validate-relocs
         relocs stub-symbols text-bytes
         path)
        (emacs-load--artifact-native-validate-defuns
         (emacs-load--artifact-native-section-get native :defuns)
         text-bytes
         path)
        (setq total-bytes (+ text-bytes (* stub-count 16)))
        (setq rounded-page-size
              (* page-size
                 (/ (+ total-bytes page-size -1)
                    page-size)))
        (setq syscall-direct-fn
              (cond
               ((fboundp 'syscall-direct) #'syscall-direct)
               ((fboundp 'nelisp--syscall-direct) #'nelisp--syscall-direct)
               (t nil)))
        (unless syscall-direct-fn
          (error "no syscall-direct helper available for %S"
                 (or path text)))
        (setq base (funcall syscall-direct-fn 9 0 rounded-page-size 7 34 -1 0))
        (unless (and (integerp base) (> base 0))
          (error "mmap failed for %S" (or path text)))
        (setq ptr-write-u8-fn
              (cond
               ((fboundp 'ptr-write-u8) #'ptr-write-u8)
               ((fboundp 'nelisp--ptr-write-u8) #'nelisp--ptr-write-u8)
               (t nil)))
        (unless ptr-write-u8-fn
          (error "no ptr-write-u8 helper available for %S" (or path text)))
        (setq ptr-copy-string-bytes-fn
              (and (fboundp 'nelisp--ptr-copy-string-bytes)
                   #'nelisp--ptr-copy-string-bytes))
        (setq ptr-write-u32-fn
              (cond
               ((fboundp 'ptr-write-u32) #'ptr-write-u32)
               ((fboundp 'nelisp--ptr-write-u32) #'nelisp--ptr-write-u32)
               (t nil)))
        (if ptr-copy-string-bytes-fn
            (let ((copied (funcall ptr-copy-string-bytes-fn base decoded)))
              (unless (and (integerp copied)
                           (= copied text-bytes))
                (error "native copy count mismatch for %s: expected %d, got %S"
                       (or path text) text-bytes copied)))
          (dotimes (i text-bytes)
            (funcall ptr-write-u8-fn base i
                     (emacs-load--artifact-byte-at decoded i))))
        (setq stub-addresses nil)
        (dolist (symbol stub-symbols)
          (let ((runtime-address (nelisp--runtime-symbol-address symbol))
                (stub-offset (+ text-bytes (* (length stub-addresses) 16))))
            (unless (and (integerp runtime-address) (> runtime-address 0))
              (error "runtime symbol %S resolved to invalid address %S"
                     symbol runtime-address))
            (emacs-load--artifact-native-write-stub
             base stub-offset runtime-address ptr-write-u8-fn ptr-write-u32-fn)
            (push (cons symbol (+ base stub-offset)) stub-addresses)))
        (dolist (reloc relocs)
          (let* ((offset (plist-get reloc :offset))
                 (symbol (plist-get reloc :symbol))
                 (addend (plist-get reloc :addend))
                 (stub-address (cdr (assoc symbol stub-addresses)))
                 (value (emacs-load--artifact-native-rel32-value
                         stub-address addend (+ base offset))))
            (if ptr-write-u32-fn
                (funcall ptr-write-u32-fn base offset value)
              (funcall ptr-write-u8-fn base offset (logand value #xff))
              (funcall ptr-write-u8-fn base (+ offset 1) (logand (lsh value -8) #xff))
              (funcall ptr-write-u8-fn base (+ offset 2) (logand (lsh value -16) #xff))
              (funcall ptr-write-u8-fn base (+ offset 3) (logand (lsh value -24) #xff)))))
        (puthash cache-key base emacs-load--artifact-native-hash-cache))
      base))

  (defun emacs-load--artifact-native-install-fn (name native base meta)
    "Install NAME from native metadata NATIVE into `nelisp--functions'."
    (let* ((offset (plist-get meta :offset))
           (arity (plist-get meta :arity))
           (body-offset (plist-get meta :body-offset))
           (rt-slot-count (plist-get meta :rt-slot-count)))
      (unless (and (integerp offset)
                   (integerp body-offset)
                   (integerp rt-slot-count)
                   (integerp arity)
                   (<= 0 arity 6)
                   (<= 0 body-offset)
                   (<= 0 rt-slot-count))
        (error "invalid native metadata for %S" name))
      (let* ((address (+ base offset))
             (body-address (+ address body-offset)))
        (setq emacs-load--artifact-native-diagnostic-report
              (list :name name
                    :arch (plist-get native :arch)
                    :hash (emacs-load--artifact-native-section-replay-hash native)
                    :offset offset
                    :body-offset body-offset
                    :address address
                    :body-address body-address
                    :arity arity
                    :rt-slot-count rt-slot-count
                    :base base
                    :path (plist-get native :path)))
        (fset name
              (emacs-load--artifact-native-lambda-form
               body-address arity rt-slot-count))
        name)))

  (defun emacs-load--artifact-install-fn (name fn)
    "Install NAME from artifact function FN into `nelisp--functions'."
    (unless (and (boundp 'nelisp--functions) (fboundp 'nelisp--apply))
      (error "no BCL replay path for %S" name))
    (puthash name fn nelisp--functions)
    (fset name
          `(lambda (&rest args)
             (nelisp--apply (gethash ',name nelisp--functions) args)))
    name)

  (defun emacs-load--artifact-literal-value (form)
    "Return a literal value for artifact FORM."
    (cond
     ((or (null form)
          (eq form t)
          (numberp form)
          (stringp form)
          (keywordp form))
      form)
     ((and (consp form) (eq (car form) 'quote))
      (nth 1 form))
     (t
      (eval form))))

  (defun emacs-load--artifact-eval-form (form)
    "Evaluate a replay artifact FORM and return its replay value."
    (cond
     ((and (consp form) (eq (car form) 'defun))
      (if (fboundp 'nelisp-eval)
          (nelisp-eval form)
        (eval form)))
     ((and (consp form) (eq (car form) 'defvar))
      (puthash (nth 1 form) t nelisp--specials)
      (nth 1 form))
     ((and (consp form) (eq (car form) 'defconst))
      (puthash (nth 1 form) t nelisp--specials)
      (puthash (nth 1 form)
               (emacs-load--artifact-literal-value (nth 2 form))
               nelisp--globals)
      (nth 1 form))
     (t
      (eval form))))

  (defun emacs-load--artifact-replay-item (item &optional path native-section base)
    "Replay one artifact ITEM and return its replay value."
    (cond
     ((and (consp item) (eq (car item) :fn))
      (let* ((name (nth 1 item))
             (fn (nth 2 item))
             (source-defun (emacs-load--artifact-fn-source-defun item path))
             (meta (and native-section
                        (emacs-load--artifact-find-defuns-metadata
                         name native-section))))
        (cond
         ((and meta (integerp base))
          (emacs-load--artifact-native-install-fn
           name native-section base meta))
         ((emacs-load--artifact-bcl-replay-available-p)
          (emacs-load--artifact-install-fn name fn))
         (source-defun
          (emacs-load--artifact-eval-form source-defun))
         (t
          (error "no native or BCL replay path for %S in artifact%s"
                 name (if path (format " (%s)" path) ""))))))
     ((and (consp item) (eq (car item) :eval))
      (eval (nth 1 item)))
     (t
      (emacs-load--artifact-eval-form item))))

  (defun emacs-load--artifact-replay-item-with-native-sections
      (item &optional path section-bases defuns-index)
    "Replay one artifact ITEM against sharded native SECTION-BASES."
    (cond
     ((and (consp item) (eq (car item) :fn))
      (let* ((name (nth 1 item))
             (fn (nth 2 item))
             (source-defun (emacs-load--artifact-fn-source-defun item path))
             (match (if defuns-index
                        (emacs-load--artifact-find-defuns-metadata-in-index
                         name defuns-index)
                      (and section-bases
                           (emacs-load--artifact-find-defuns-metadata-in-sections
                            name section-bases path)))))
        (cond
         (match
          (emacs-load--artifact-native-install-fn
           name (nth 0 match) (nth 1 match) (nth 2 match)))
         ((emacs-load--artifact-bcl-replay-available-p)
          (emacs-load--artifact-install-fn name fn))
         (source-defun
          (emacs-load--artifact-eval-form source-defun))
         (t
          (error "no native or BCL replay path for %S in artifact%s"
                 name (if path (format " (%s)" path) ""))))))
     ((and (consp item) (eq (car item) :eval))
      (eval (nth 1 item)))
     (t
      (emacs-load--artifact-eval-form item))))

  (defun emacs-load--artifact-replay-payload-small (payload path)
    "Replay PAYLOAD using the historical whole-payload reader."
    (unless (plist-member payload :module-init)
      (error "missing :module-init in %s" path))
    (setq emacs-load--last-artifact-payload payload)
    (let* ((native-sections-present (plist-member payload :native-sections))
           (native-sections (and native-sections-present
                                 (emacs-load--artifact-native-sections-from-payload
                                  payload path)))
           (section-bases (and native-sections-present
                               (emacs-load--artifact-native-section-bases
                                native-sections path)))
           (defuns-index (and section-bases
                              (emacs-load--artifact-build-defuns-metadata-index
                               section-bases path)))
           (section-raw (and (plist-member payload :native)
                             (plist-get payload :native)))
           (section (and section-raw
                         (emacs-load--artifact-native-section-canonicalize
                          section-raw path)))
           (base (and (emacs-load--artifact-native-eligible-p section)
                      (emacs-load--artifact-native-map-section
                       section path))))
      (dolist (item (plist-get payload :module-init))
        (if native-sections-present
            (emacs-load--artifact-replay-item-with-native-sections
             item path section-bases defuns-index)
          (emacs-load--artifact-replay-item
           item path section base))))
    payload)

  (defun emacs-load--artifact-read-payload (content path prefix-end)
    "Read one complete artifact payload from CONTENT after PREFIX-END.
The byte-indexed CONTENT is scanned first, then only the isolated payload is
converted for `read-from-string'.  Reject any non-comment trailing input."
    (let* ((content-length (length content))
           (payload-start (emacs-load--artifact-source-skip-ws-comments
                           content prefix-end))
           (payload-end (emacs-load--artifact-source-form-end
                         content payload-start))
           (trailing (emacs-load--artifact-source-skip-ws-comments
                      content payload-end))
           (slice (emacs-load--reader-slice
                   content payload-start payload-end))
           (read (read-from-string slice 0 (length slice))))
      (unless (= trailing content-length)
        (error "invalid trailing data in %s" path))
      (unless (and (consp read)
                   (= (cdr read) (length slice))
                   (listp (car read)))
        (error "invalid artifact payload in %s" path))
      (car read)))

  (defun emacs-load--artifact-replay-payload-streaming (content path prefix-end)
    "Replay a large artifact CONTENT without materializing the full payload."
    (let* ((byte-length (emacs-load--artifact-byte-length content))
           (content-length (length content))
           (payload-start (emacs-load--artifact-source-skip-ws-comments
                           content prefix-end))
           (payload-close (emacs-load--artifact-source-rskip-ws
                           content (1- content-length)))
           (module-marker
            (emacs-load--artifact-source-find-marker
             content ":module-init " prefix-end path :module-init t))
           (native-sections-marker
            (emacs-load--artifact-source-find-marker
             content ":native-sections " prefix-end path :native-sections nil))
       (native-marker
        (and (not native-sections-marker)
             (emacs-load--artifact-source-find-marker
              content ":native " prefix-end path :native nil)))
           (field-names nil)
           (native-mode nil)
           (section-bases nil)
           (defuns-index nil)
           (module-count 0)
           (module-pos nil)
           (module-end nil))
      (unless (and (< payload-start content-length)
                   (= (emacs-load--artifact-source-char-at
                       content payload-start) ?\())
        (error "invalid artifact payload in %s" path))
      (unless (and (<= 0 payload-close)
                   (< payload-close content-length)
                   (= (emacs-load--artifact-source-char-at
                       content payload-close) ?\)))
        (error "unterminated artifact payload in %s" path))
      (setq field-names
            (let ((entries nil))
              (push (cons module-marker :module-init) entries)
              (when native-sections-marker
                (push (cons native-sections-marker :native-sections) entries))
              (when native-marker
                (push (cons native-marker :native) entries))
              (mapcar #'cdr
                      (sort entries (lambda (a b) (< (car a) (car b)))))))
      (cond
       (native-sections-marker
        (setq native-mode :native-sections)
        (let* ((value-start (emacs-load--artifact-source-skip-ws-comments
                             content
                             (+ native-sections-marker
                                (length ":native-sections "))))
               (value-end (emacs-load--artifact-source-form-end
                           content value-start)))
          (setq section-bases
                (emacs-load--artifact-native-sections-from-source
                 content value-start value-end path
                 (lambda (section)
                   (when (emacs-load--artifact-native-eligible-p section)
                     (cons (emacs-load--artifact-native-section-compact
                            section path)
                           (emacs-load--artifact-native-map-section
                            section path)))))))
        (setq defuns-index
              (emacs-load--artifact-build-defuns-metadata-index
               section-bases path)))
       (native-marker
        (setq native-mode :native)
        (let* ((value-start (emacs-load--artifact-source-skip-ws-comments
                            content (+ native-marker (length ":native "))))
               (value-end (emacs-load--artifact-source-form-end
                           content value-start))
               (ranges (emacs-load--artifact-source-read-form-ranges
                       content value-start value-end path)))
          (when (emacs-load--artifact-native-section-preflight-from-ranges
                 content ranges path)
            (let ((section
                   (emacs-load--artifact-native-section-from-ranges
                    content ranges path)))
              (when (emacs-load--artifact-native-eligible-p section)
                (setq section-bases
                      (list (cons (emacs-load--artifact-native-section-compact
                                   section path)
                                  (emacs-load--artifact-native-map-section
                                   section path))))
                (setq defuns-index
                      (emacs-load--artifact-build-defuns-metadata-index
                       section-bases path)))))))
      )
      (setq module-pos
            (emacs-load--artifact-source-skip-ws-comments
             content (+ module-marker (length ":module-init "))))
      (unless (< module-pos content-length)
        (error "invalid :module-init in %s" path))
      (let ((module-form-end (emacs-load--artifact-source-form-end
                              content module-pos)))
        (setq module-end module-form-end)
        (setq module-count
              (if (and (= (emacs-load--artifact-source-char-at
                           content module-pos) ?\()
                       (or (fboundp 'nelisp--read-batch-vector-from-string-native)
                           (fboundp 'nelisp--read-batch-from-string-native)))
                  (emacs-load--artifact-source-iterate-form-nil-or-list-value-native-batch
                   (substring content (1+ module-pos) (1- module-form-end))
                   path :module-init
                   (lambda (item)
                     (if section-bases
                         (emacs-load--artifact-replay-item-with-native-sections
                          item path section-bases defuns-index)
                       (emacs-load--artifact-replay-item
                        item path nil nil))))
                 (emacs-load--artifact-source-iterate-form-nil-or-list-value
                 content (cons module-pos module-form-end) path :module-init
                 (lambda (item)
                   (if section-bases
                       (emacs-load--artifact-replay-item-with-native-sections
                        item path section-bases defuns-index)
                     (emacs-load--artifact-replay-item
                      item path nil nil))))))
    )
      (setq module-pos
            (emacs-load--artifact-source-skip-ws-comments content module-end))
      (when (> module-pos payload-close)
        (error "invalid trailing data in %s" path))
      (setq emacs-load--last-artifact-payload
            (emacs-load--artifact-streaming-summary
             path byte-length native-mode module-count field-names))
      emacs-load--last-artifact-payload))

(defun emacs-load--artifact-replay-file (path &optional native-context)
  "Replay a private artifact file at PATH and return its payload."
  (let* ((top-level-p (zerop emacs-load--artifact-replay-depth))
         (threshold emacs-load-artifact-replay-pressure-threshold)
         (load-garbage-collect-interval
          (if top-level-p
              (emacs-load--artifact-replay-garbage-collect-interval)
            load-garbage-collect-interval)))
    (when (and emacs-load-artifact-replay-preflight-gc
               top-level-p
               (emacs-load--collection-due-p))
      (garbage-collect))
    (if top-level-p
        (let* ((initial-baseline (and (integerp threshold)
                                      (> threshold 0)
                                      (emacs-load--artifact-replay-arena-used-bytes)))
               (pressure-enabled (and (integerp threshold)
                                     (> threshold 0)
                                     (integerp initial-baseline)
                                     (>= initial-baseline 0))))
          (let ((emacs-load--artifact-replay-pressure-baseline initial-baseline)
                (emacs-load--artifact-replay-pressure-enabled pressure-enabled)
                (emacs-load--artifact-replay-depth (1+ emacs-load--artifact-replay-depth))
                (result nil))
            (setq result
                  (let ((content (emacs-load--read-file-string path))
                        (prefix ";;; nelisp-private-nelc-v2\n"))
                    (unless (stringp content)
                      (error "invalid artifact contents in %s" path))
                    (unless (string-prefix-p prefix content)
                      (error "invalid artifact prefix in %s" path))
                    (if (and (integerp emacs-load-artifact-replay-streaming-threshold)
                             (> emacs-load-artifact-replay-streaming-threshold 0)
                             (> (emacs-load--artifact-byte-length content)
                                emacs-load-artifact-replay-streaming-threshold))
                        (emacs-load--artifact-replay-payload-streaming
                         content path (length prefix))
                      (emacs-load--artifact-replay-payload-small
                       (emacs-load--artifact-read-payload
                        content path (length prefix))
                       path))))
            (emacs-load--artifact-replay-pressure-maybe-gc)
            result))
      (let ((emacs-load--artifact-replay-depth (1+ emacs-load--artifact-replay-depth)))
        (emacs-load--artifact-replay-pressure-maybe-gc)
        (let ((content (emacs-load--read-file-string path))
              (prefix ";;; nelisp-private-nelc-v2\n"))
          (unless (stringp content)
            (error "invalid artifact contents in %s" path))
          (unless (string-prefix-p prefix content)
            (error "invalid artifact prefix in %s" path))
          (if (and (integerp emacs-load-artifact-replay-streaming-threshold)
                   (> emacs-load-artifact-replay-streaming-threshold 0)
                   (> (emacs-load--artifact-byte-length content)
                      emacs-load-artifact-replay-streaming-threshold))
              (emacs-load--artifact-replay-payload-streaming
               content path (length prefix))
            (emacs-load--artifact-replay-payload-small
             (emacs-load--artifact-read-payload
              content path (length prefix))
             path)))))))

  ;; NOTE (2026-07-12): a retry-with-rewrite fallback (rewriting top-level
  ;; `defalias' to `nelisp-defalias-late' on load failure) was attempted
  ;; here and REVERTED: wrapping the fast loader in `condition-case'
  ;; mis-captures its internal non-local exits on this substrate (known
  ;; condition-case/throw defect), silently turning successful by-name
  ;; loads into zero-form no-ops.  Any future retry design must not wrap
  ;; `nelisp--eval-source-string' in a handler.
  (defvar nelisp--load-rewrite-wrapper-heads
    '(progn prog1 prog2 when unless if while eval-when-compile
      eval-and-compile with-no-warnings with-suppressed-warnings
      condition-case let let*)
    "Wrapper heads whose nested forms are eligible for load-time rewrites.")

  (defun nelisp--load-rewrite-defalias-form (nelisp--load-rw-form)
    "Rewrite top-level DEFALIAS forms in FORM for standalone load retry.
Only descend through `nelisp--load-rewrite-wrapper-heads'.  Quoted data
subtrees are left untouched."
    (if (consp nelisp--load-rw-form)
        (let ((nelisp--load-rw-head (car nelisp--load-rw-form)))
          (cond
           ((memq nelisp--load-rw-head '(quote function))
            nelisp--load-rw-form)
           ((eq nelisp--load-rw-head 'cc-provide)
            (setcar nelisp--load-rw-form 'provide)
            nelisp--load-rw-form)
           ((eq nelisp--load-rw-head 'defalias)
            (let ((name (cadr nelisp--load-rw-form)))
              (when (and (consp name) (memq (car name) '(quote function))
                         (symbolp (cadr name))
                         (not (memq (cadr name) emacs-load--alias-candidates)))
                (push (cadr name) emacs-load--alias-candidates)))
            ;; The rewritten form calls `nelisp-defalias-late', defined in
            ;; emacs-eval.el; nothing else guarantees that file is loaded.
            (unless (fboundp 'nelisp-defalias-late)
              (require 'emacs-eval nil t))
            (setcar nelisp--load-rw-form 'nelisp-defalias-late)
            nelisp--load-rw-form)
           ((memq nelisp--load-rw-head nelisp--load-rewrite-wrapper-heads)
            (let ((nelisp--load-rw-tail (cdr nelisp--load-rw-form))
                  (nelisp--load-rw-node nil))
              (while (consp nelisp--load-rw-tail)
                (setq nelisp--load-rw-node (car nelisp--load-rw-tail))
                (when (consp nelisp--load-rw-node)
                  (setcar nelisp--load-rw-tail
                          (nelisp--load-rewrite-defalias-form
                           nelisp--load-rw-node)))
                (setq nelisp--load-rw-tail (cdr nelisp--load-rw-tail))))
            nelisp--load-rw-form)
           (t nelisp--load-rw-form)))
      nelisp--load-rw-form))

  (defun nelisp--load-eval-source-rewriting (source)
    "Retry loader for SOURCE with top-level `defalias' rewritten late.
This interpreted fallback is intentionally slow and is a future native
optimization candidate; keep the normal load path on the fast reader."
    (nelisp--load-map-source-forms source #'eval))

  (defun emacs-load--regular-file-p (resolved)
    "Return non-nil when RESOLVED names an existing non-directory file."
    (if (fboundp 'nelisp--syscall-stat)
        (eq (nelisp--syscall-stat resolved) 'file)
      (and (file-exists-p resolved)
           (not (file-directory-p resolved)))))

  (defun emacs-load--source-lexical-binding-p (source)
    "Read SOURCE's first-line lexical-binding option, including a shebang."
    (let* ((start (if (string-prefix-p "#!" source)
                      (1+ (or (emacs-load--artifact-string-search "\n" source 0)
                              (1- (length source))))
                    0))
           (end (or (emacs-load--artifact-string-search "\n" source start)
                    (length source)))
           (line (substring source start end))
           (open (emacs-load--artifact-string-search "-*-" line 0))
           (close (and open (emacs-load--artifact-string-search "-*-" line (+ open 3)))))
      (when close
        (let ((options (substring line (+ open 3) close)))
          (and (string-match
                "\\(?:\\`\\|;\\)[ \t]*lexical-binding:[ \t]*\\([^; \t]+\\)"
                options)
               (not (equal (match-string 1 options) "nil")))))))

  (defvar load-prefer-newer nil
    "Non-nil means loading prefers the newest suffix in a search directory.")

  (defvar bytecomp-version-regexp
    "^;;;.\\(in Emacs version\\|bytecomp version FSF\\)"
    "Pattern identifying a compiled-file header safe for the reader.")

  (defun emacs-load--elc-version (source)
    "Return GNU's compiled header version, zero for an unsafe header.
As in lread.c:safe_to_load_version, inspect at most 512 bytes after
skipping the first line.  Empty files have version one.  The compiler
comment is the safety check; GNU does not impose a numeric version cap."
    (if (= (length source) 0)
        1
      (let* ((header (substring source 0 (min 512 (length source))))
             (newline (string-search "\n" header))
             (case-fold-search t))
        (if (and newline
                 (string-match-p bytecomp-version-regexp
                                 (substring header newline)))
            (if (> newline 4) (aref header 4) 1)
          0))))

  (defun emacs-load--elc-decode (bytes)
    "Decode utf-8-emacs BYTES.
Return (TEXT . BYTE8-OFFSETS), where each offset marks an encoded raw
byte that will occupy one byte after `string-as-unibyte'."
    (let* ((lead-c0 (unibyte-string #xc0))
           (lead-c1 (unibyte-string #xc1))
           (next-c0 (string-search lead-c0 bytes))
           (next-c1 (string-search lead-c1 bytes))
           (limit (length bytes))
           (start 0)
           (shrink 0)
           chunks offsets next)
      ;; Unlike the external UTF-8 decoder, Emacs's .elc encoding also
      ;; accepts C0/C1 followed by a continuation as a single raw byte.
      ;; Give each lead byte its own cursor. Searching again for an absent
      ;; lead after every hit of the other one would rescan the suffix.
      (while (setq next (if (and next-c0 next-c1)
                            (min next-c0 next-c1)
                          (or next-c0 next-c1)))
        (let ((cursor (1+ next)))
          (when (and (< cursor limit) (<= #x80 (aref bytes cursor) #xbf))
            (push (emacs-load--reader-slice bytes start next) chunks)
            (push (char-to-string (+ #x3fff80
                                     (* 64 (- (aref bytes next) #xc0))
                                     (- (aref bytes cursor) #x80)))
                  chunks)
            (push (- next shrink) offsets)
            (setq cursor (1+ cursor)
                  start cursor
                  shrink (1+ shrink)))
          (when (and next-c0 (< next-c0 cursor))
            (setq next-c0 (string-search lead-c0 bytes cursor)))
          (when (and next-c1 (< next-c1 cursor))
            (setq next-c1 (string-search lead-c1 bytes cursor)))))
      (cons (if chunks
                (apply #'concat
                       (nreverse (cons (emacs-load--reader-slice bytes start limit)
                                       chunks)))
              (emacs-load--reader-slice bytes 0 limit))
            offsets)))

  (defun emacs-load--elc-read-slice (source start end)
    "Read one .elc form and return its value and byte position."
    (let* ((decoded (emacs-load--elc-decode
                     (substring source start end)))
           (text (car decoded))
           (parsed (nelisp--rd-read-one text 0 (length text))))
      (if (= (cdr parsed) (length text))
          (cons (car parsed) end)
        (let* ((prefix (substring text 0 (cdr parsed)))
               (bytes (length (emacs-load--byte-indexed-source prefix)))
               (extra 0))
          (dolist (offset (cdr decoded))
            (when (< offset bytes) (setq extra (1+ extra))))
          (cons (car parsed) (+ start bytes extra))))))

  (defun emacs-load--elc-read-fallback (source start end native-end)
    "Read the .elc form in SOURCE starting at byte START using the Lisp reader."
    (let ((scan-end (and (not native-end)
                         (memq (aref source start) '(40 91))
                         (ignore-errors
                           (emacs-load--artifact-source-form-end source start)))))
      ;; Bound the decoding work when the native reader declines a container.
      ;; The source scanner is a hint, so retry if it cut the form short.
      (if (and scan-end (< scan-end end))
          (condition-case nil
              (emacs-load--elc-read-slice source start scan-end)
            ((end-of-file invalid-read-syntax)
             (emacs-load--elc-read-slice source start end)))
        (emacs-load--elc-read-slice source start (or native-end end)))))

  (defun emacs-load--elc-skip-blocks (source position limit)
    "Return the first form start in SOURCE at or after POSITION.
Skip whitespace, comments and top-level `#@NNN' lazy-docstring blocks.  As in
lread.c, NNN counts bytes after the digits, including the marker's space, so
SOURCE must be byte-indexed.  `#@00' discards the remainder."
    (let ((p (nelisp--load-skip-space-and-comments source position))
          (done nil))
      (while (not done)
        (if (and (< (1+ p) limit) (= (aref source p) 35)
                 (= (aref source (1+ p)) 64))
            (let ((q (+ p 2)) (n 0))
              (if (and (<= (+ p 4) limit)
                       (= (aref source q) 48) (= (aref source (1+ q)) 48))
                  (setq p limit done t)
                (while (and (< q limit) (<= 48 (aref source q) 57))
                  (setq n (+ (* n 10) (- (aref source q) 48))
                        q (1+ q)))
                (setq p (nelisp--load-skip-space-and-comments
                         source (min limit (+ q n))))))
          (setq done t)))
      p))

  (defun emacs-load--eval-elc (source resolved &optional version)
    "Read GNU compiled SOURCE incrementally and execute its top-level forms.
Keep the original byte offsets and byte-code objects, without the source
loader's defalias rewrites or native artifact compilation.  Top-level
`byte-code' uses the same executor as compiled function calls."
    (when (= (or version (emacs-load--elc-version source)) 0)
      ;; Foundation bootstrap formatters may omit GNU message quoting.
      ;; Quote the template before interpolation, preserving filename bytes.
      (let* ((style (or (and (fboundp 'text-quoting-style)
                              (text-quoting-style))
                         (and (boundp 'text-quoting-style) text-quoting-style)))
             (template
              (cond ((eq style 'grave) "File `%s' was not compiled in Emacs")
                    ((eq style 'straight) "File '%s' was not compiled in Emacs")
                    (t (concat "File " (char-to-string 8216) "%s"
                               (char-to-string 8217)
                               " was not compiled in Emacs")))))
        (signal 'error (list (format template resolved)))))
    ;; GNU lread.c:1491-1494 exposes the active load to compiled callees.
    (let ((load-file-name resolved)
          (load-true-file-name resolved)
          (inhibit-file-name-operation nil)
          (load-in-progress t)
          (lexical-binding nil)
          (current-load-list (list resolved))
          (nelisp--lh-file resolved)
          (nelisp--lh-parked nil)
          (nelisp--lh-seg nil)
          (position 0)
          (limit (length source))
          (next-hash -1)
          (count 0)
          (stream (and (fboundp 'nelisp--eval-source-string)
                       (subrp (symbol-function 'nelisp--eval-source-string))
                       (equal (subr-arity (symbol-function 'nelisp--eval-source-string))
                              '(1 . 2)))))
      (while (< (setq position
                      (if stream
                          (nelisp--eval-source-string source position)
                        (emacs-load--elc-skip-blocks source position limit)))
                limit)
        (let* ((native
                (and (fboundp 'nelisp--read-all-from-string-native)
                     (condition-case nil
                         (nelisp--read-all-from-string-native source position limit 1)
                       (error nil))))
               (parsed
                (progn
                  (when (< next-hash position)
                    (setq next-hash (or (string-search "#^" source position) limit)))
                  ;; The native parser treats char-table syntax as a vector.
                  (if (and native (>= next-hash (cdr native)))
                      native
                    (condition-case nil
                        (emacs-load--elc-read-fallback source position limit (cdr native))
                      (end-of-file (signal 'end-of-file (list resolved))))))))
          (unless (and (consp parsed) (integerp (cdr parsed))
                       (> (cdr parsed) position))
            (signal 'end-of-file (list "compiled load reader made no progress")))
          (let ((form (car parsed)))
            ;; The byte compiler emits three self-evaluating operands.
            ;; Other shapes and rebound functions go through ordinary eval.
            (if (and (eq (car-safe form) 'byte-code)
                     (let* ((operands (cdr form))
                            (constants (cdr-safe operands))
                            (depth (cdr-safe constants)))
                       (and (stringp (car-safe operands))
                            (vectorp (car-safe constants))
                            (integerp (car-safe depth))
                            (null (cdr depth))))
                     (subrp (symbol-function 'byte-code))
                     (not (special-form-p 'byte-code)))
                (byte-code (cadr form) (caddr form) (cadddr form))
              (eval form nil)))
          (setq position (cdr parsed)
                count (1+ count))
          (unless stream (emacs-load--periodic-gc count))))
      (when (fboundp 'nelisp--lh-record)
        (nelisp--lh-record resolved (cdr (nreverse current-load-list))))
      :compiled))

  (defun emacs-load--elc-doc-start-p (source pos)
    "Return non-nil if byte POS follows a lazy docstring header or delimiter."
    (and (< 0 pos (length source))
         (let ((previous (aref source (1- pos))))
           (or (= previous 31)
               (and (= previous 32)
                    (let ((i (- pos 2)))
                      (while (and (>= i 0) (<= 48 (aref source i) 57))
                        (setq i (1- i)))
                      (and (> i 0)
                           (= (aref source i) 64)
                           (= (aref source (1- i)) 35))))))))

  (defun emacs-load--elc-unquote-doc (source start end)
    "Return the bytes of SOURCE from START to END after undoing ^A quoting."
    (let ((cursor start) chunks next)
      (while (and (setq next (string-search "\001" source cursor)) (< next end))
        (push (substring source cursor next) chunks)
        (let ((quoted (if (< (1+ next) end) (aref source (1+ next)) 0)))
          (push (unibyte-string
                 (cond ((= quoted 1) 1)
                       ((= quoted 48) 0)
                       ((= quoted 95) 31)
                       (t (error "Invalid data in documentation file -- %c followed by code %03o"
                                 1 quoted))))
                chunks))
        (setq cursor (+ next 2)))
      (if chunks
          (apply #'concat (nreverse (cons (substring source cursor end) chunks)))
        (substring source start end))))

  (defun get-doc-string (location &optional unibyte)
    "Return the external documentation for LOCATION.
A cons (FILE . POS) points to a byte offset in a compiled Lisp file.
Return nil for a stale location or a reference to the undumped DOC file.
When UNIBYTE is non-nil, return the unquoted bytes."
    (when (and (consp location) (stringp (car location))
               (integerp (cdr location)))
      (let* ((file (car location))
             (pos (abs (cdr location)))
             (path (if (file-name-absolute-p file) file
                     (expand-file-name
                      file (and (boundp 'lisp-directory)
                                (symbol-value 'lisp-directory)))))
             (source (emacs-load--read-file-string path t)))
        (cond
         ((null source)
          (format "Cannot open doc string file \"%s\"\n" file))
         ((emacs-load--elc-doc-start-p source pos)
          (let* ((end (or (string-search "\037" source pos) (length source)))
                 (bytes (emacs-load--elc-unquote-doc source pos end)))
            (if unibyte bytes (car (emacs-load--elc-decode bytes)))))))))

  (defun nelisp--load-resolved-file (resolved noerror)
    "Load exact absolute RESOLVED path, honoring NOERROR for open failures.
A successful load files RESOLVED in `load-history' and runs its
after-load forms, as GNU `load' does (lread.c build_load_history)."
    (let ((result (nelisp--load-resolved-file-1 resolved noerror)))
      (when (and result (fboundp 'nelisp--after-load-feature))
        (if (eq result :compiled)
            (do-after-load-evaluation resolved)
          (nelisp--after-load-feature resolved))
        ;; Resume the enclosing file before its next form can locally
        ;; bind current-load-list, as cl-generic does after cl-loaddefs.
        (when (and load-file-name (boundp 'nelisp--lh-parked)
                   (assoc load-file-name nelisp--lh-parked))
          (nelisp--lh-select load-file-name)))
      (and result t)))
  (defun nelisp--load-resolved-file-1 (resolved noerror)
    "Load exact absolute RESOLVED path, honoring NOERROR for open failures."
    (cond
     ((not (emacs-load--regular-file-p resolved))
      (if noerror nil
        (signal 'file-error (list "Cannot open load file" resolved))))
     (t
      (let* ((compiled-path (or (string-suffix-p ".elc" resolved)
                                (string-suffix-p ".elc.gz" resolved)))
             ;; GNU can recognize a compiled header in a file of any name.
             ;; Read the bytes once before deciding how to decode them.
             (source (emacs-load--read-file-string resolved t))
             ;; As in GNU Fload, publish the resolved init file before
             ;; executing it, including when evaluation subsequently fails.
             (_init-file (when (and source (eq user-init-file t))
                           (setq user-init-file resolved)))
             (version (and (stringp source) (emacs-load--elc-version source)))
             (compiled (and version (or compiled-path (> version 1))))
             (base (and (stringp source) (not compiled)
                        (file-name-nondirectory resolved)))
             (lexical-binding (and (stringp source) (not compiled)
                                   (emacs-load--source-lexical-binding-p source)))
             (source (if (and (stringp source) (not compiled))
                         (emacs-load--rewrite-named-unicode-escapes
                          ;; The cookie above can follow a script header.  The
                          ;; reader starts after that header, as in GNU load.
                          (if (string-prefix-p "#!" source)
                              (let ((newline (string-search "\n" source)))
                                (substring source (if newline
                                                      (1+ newline)
                                                    (length source))))
                            source))
                       source))
             (source (if (and (stringp source) (not compiled)
                               (not emacs-load--propertized-string-reader-native-p))
                         (emacs-load--rewrite-propertized-string-literals
                          source)
                       source))
             (source (if (and (stringp source) (not compiled)
                               (not emacs-load--labeled-object-reader-native-p))
                         (emacs-load--rewrite-labeled-object-references
                          source)
                       source))
             (source (if (and (stringp source) (not compiled)
                               (not emacs-load--escaped-modifier-char-literal-reader-native-p))
                         (emacs-load--rewrite-escaped-modifier-char-literals
                          source)
                       source))
             (source (if (and (stringp source) (not compiled)
                              (not emacs-load--escaped-unicode-source-reader-native-p))
                         (emacs-load--rewrite-escaped-unicode-character-literals source)
                       source))
             (artifact (and (not compiled) emacs-load-auto-native-compile
                            (stringp source)
                            (not (string-prefix-p "cc-" base))
                            (emacs-load--artifact-load-or-compile
                             resolved source))))
        (if artifact
            t
          (cond
           ((null source)
            (if noerror nil
              (signal 'file-error (list "read error" resolved))))
           (compiled (emacs-load--eval-elc source resolved version))
           (t
            ;; `cc-provide' is a compile-time wrapper around `provide', but
            ;; NeLisp's fast source evaluator bare-aborts on that form head.
            ;; Normalize the sole runtime form before handing off the source.
            (when (and (>= (length resolved) 10)
                       (string= (substring resolved (- (length resolved) 10))
                                "cc-defs.el"))
              (let ((tail-start (max 0 (- (length source) 256))))
                (setq source
                      (concat
                       (substring source 0 tail-start)
                       (emacs-load--replace-source-literal "(cc-provide 'cc-defs)"
                                       "(provide 'cc-defs)"
                                       (substring source tail-start))))))
            ;; Per-file replay marker for the CC Mode language-constant
            ;; machinery.  `(setq load-file-name resolved)' below is invisible
            ;; to the interpreted elisp inside the native source evaluator, so
            ;; `c-get-current-file' (cc-defs.el) would otherwise collapse every
            ;; cc-*.el onto one synthetic key.  The per-file `source' anchor
            ;; alist that `c-define-lang-constant' relies on then overwrites
            ;; prior files' bindings, producing spurious cyclic-reference
            ;; errors.  Splice a marker `setq' at the head of the source (same
            ;; eval-source-string, hence visible to the following top-level
            ;; forms -- decisively unlike the outer `setq') plus a restoring
            ;; `pop' at the tail.  For cc-defs.el itself, also append a
            ;; `c-get-current-file' redefinition (defun, so the new function
            ;; cell reaches every caller) that consults the marker.
            (when (string-prefix-p "cc-" (file-name-sans-extension base))
              (let* ((base (file-name-sans-extension base))
                     (cc-redef
                      (if (string= base "cc-defs")
                          (concat
                           "\n(defun c-get-current-file ()"
                           " (let* ((c-or-l (cc-bytecomp-compiling-or-loading))"
                           "        (file (cond ((eq c-or-l 'loading) load-file-name)"
                           "                    ((eq c-or-l 'compiling) byte-compile-dest-file)"
                           "                    ((null c-or-l) (buffer-file-name)))))"
                           "   (setq file (or file load-file-name nelisp--cc-replay-file \"cc-lang-replay\"))"
                           "   (and file (file-name-sans-extension (file-name-nondirectory file)))))")
                        "")))
                (setq source
                      (concat
                       "(push nelisp--cc-replay-file nelisp--cc-replay-file-stack)"
                       "(setq nelisp--cc-replay-file "
                       (prin1-to-string base) ")\n"
                       source
                       cc-redef
                       "\n(setq nelisp--cc-replay-file"
                       " (pop nelisp--cc-replay-file-stack))"))))
            (let* ((prior-lfn (and (boundp 'load-file-name) load-file-name))
                   (loader (nelisp--load-source-loader source)))
              (setq load-file-name resolved)
              (unwind-protect
                  (progn
                    ;; GNU Lisp can push straight onto current-load-list.
                    ;; Select this file before the first form can do so.
                    (when (fboundp 'nelisp--lh-select)
                      (nelisp--lh-select resolved))
                    (funcall loader source)
                    t)
                (setq load-file-name prior-lfn))))))))))

  (defun emacs-load--candidate-suffixes (file nosuffix must-suffix)
    "Return GNU load suffixes for FILE, NOSUFFIX and MUST-SUFFIX."
    (if nosuffix
        '("")
      (let ((suffixes nil)
            ;; GNU's value always contains ""; a bound-but-nil value
            ;; (early bootstrap) would otherwise yield no suffix.
            (representations (or (and (boundp 'load-file-rep-suffixes)
                                      load-file-rep-suffixes)
                                 '(""))))
        (dolist (suffix (if (boundp 'load-suffixes)
                           load-suffixes '(".elc" ".el")))
          ;; GNU recognizes an explicitly supplied .el/.elc even if the
          ;; configured load-suffixes do not contain that extension.
          (dolist (representation representations)
            (push (concat suffix representation) suffixes)))
        (setq suffixes (nreverse suffixes))
        (if (and must-suffix
                 (not (or (file-name-directory file)
                          (string-suffix-p ".el" file)
                          (string-suffix-p ".elc" file))))
            suffixes
          (append suffixes representations)))))

  (defun emacs-load--newer-file-p (candidate previous)
    "Return non-nil when CANDIDATE's mtime is later than PREVIOUS's.
GNU openp compares the complete timespec, preserving suffix order on ties.
Use the standalone stat fields directly; a source-only foundation does not
necessarily provide `file-newer-than-file-p'."
    (if (fboundp 'nelisp--syscall-stat-field)
        (let ((candidate-seconds (nelisp--syscall-stat-field candidate 88))
              (previous-seconds (nelisp--syscall-stat-field previous 88)))
          (or (> candidate-seconds previous-seconds)
              (and (= candidate-seconds previous-seconds)
                   (> (nelisp--syscall-stat-field candidate 96)
                      (nelisp--syscall-stat-field previous 96)))))
      (let ((candidate-attributes (file-attributes candidate 'integer))
            (previous-attributes (file-attributes previous 'integer)))
        (and candidate-attributes
             (or (not previous-attributes)
                 (time-less-p (nth 5 previous-attributes)
                              (nth 5 candidate-attributes)))))))

  (defvar emacs-load--directory-prefixes (make-hash-table :test 'equal)
    "Canonical directory spellings and remote-prefix flags for load search.
Every lookup checks the live filesystem.")
  ;; A source reload can encounter entries created by the previous loader.
  (clrhash emacs-load--directory-prefixes)

  (defun emacs-load--possible-remote-prefix-p (name)
    "Return non-nil if an anchored Tramp regexp might match NAME."
    (when (and (> (length name) 0) (eq (aref name 0) ?/))
      (let ((slash (nelisp--string-search "/" name 1))
            (colon (nelisp--string-search ":" name 1)))
        (or (not slash) (and colon (< colon slash))))))

  (defun emacs-load--directory-prefix (directory)
    "Return (PREFIX REMOTE-P . ASCII-P) for canonical DIRECTORY.
Leave spellings that require expansion to the general search."
    (when (and (stringp directory) (> (length directory) 0)
               (eq (aref directory 0) ?/)
               (not (string-prefix-p "/:" directory))
               (not (nelisp--string-search "//" directory 0))
               (not (nelisp--string-search "\0" directory 0))
               (not (nelisp--file-name-needs-normalization-p directory)))
      (let* ((prefix (if (eq (aref directory (1- (length directory))) ?/)
                         (copy-sequence directory)
                       (concat directory "/")))
             (entry (cons prefix (cons (emacs-load--possible-remote-prefix-p directory)
                                       (nelisp--file-name-ascii-p prefix)))))
        (when (>= (hash-table-count emacs-load--directory-prefixes) 8192)
          (clrhash emacs-load--directory-prefixes))
        (puthash (copy-sequence directory) entry emacs-load--directory-prefixes)
        entry)))

  (defvar emacs-load--handler-regexp-classes (make-hash-table :test 'equal)
    "Classification of filename handler regexps by their spelling.")

  (defun emacs-load--slash-free-regexp-p (regexp start end)
    "Prove that the REGEXP fragment from START to END cannot consume slash.
Accept the context-free subset used in standard filename handlers.
Reject top-level alternatives, which could bypass an adjacent anchor."
    (let ((depth 0) (valid t))
      (while (and valid (< start end))
        (let ((char (aref regexp start)))
          (setq start (1+ start))
          (cond
           ((eq char ?\[)
            (let ((begin (1- start))
                  (close nil))
              (when (and (< start end) (eq (aref regexp start) ?^))
                (setq start (1+ start)))
              (when (and (< start end) (eq (aref regexp start) ?\]))
                (setq start (1+ start)))
              (while (and valid (not close) (< start end))
                (cond
                 ((eq (aref regexp start) ?\\)
                  (setq valid nil))
                 ((eq (aref regexp start) ?\])
                  (setq close (1+ start)))
                 ((and (< (1+ start) end) (eq (aref regexp start) ?\[))
                  (cond
                   ((eq (aref regexp (1+ start)) ?:)
                    (let ((stop (string-search ":]" regexp (+ start 2))))
                      (if (and stop (< (1+ stop) end)
                               (member (substring regexp (+ start 2) stop)
                                       '("alnum" "blank" "digit")))
                          (setq start (1+ stop))
                        (setq valid nil))))
                   ((memq (aref regexp (1+ start)) '(?. ?=))
                    (setq valid nil)))))
                (setq start (1+ start)))
              (setq valid (and valid close
                               (not (string-match-p
                                     (substring regexp begin close) "/"))))))
           ((eq char ?\\)
            (if (>= start end)
                (setq valid nil)
              (let ((escape (aref regexp start)))
                (setq start (1+ start))
                (cond
                 ((eq escape ?\() (setq depth (1+ depth)))
                 ((eq escape ?\)) (setq depth (1- depth) valid (>= depth 0)))
                 ((eq escape ?|) (setq valid (> depth 0)))
                 ((not (memq escape '(?\[ ?\] ?. ?$ ?^ ?{ ?})))
                  (setq valid nil))))))
           ((memq char '(?/ ?. ?^ ?$)) (setq valid nil)))))
      (and valid (= depth 0))))

  (defun emacs-load--classify-handler-regexp (regexp)
    "Classify REGEXP by a necessary prefix or suffix condition."
    (or (gethash regexp emacs-load--handler-regexp-classes)
        (let* ((length (length regexp))
               (case-fold-search nil)
               (search-spaces-regexp nil)
               (remote-start "\\(?:\\`/\\)")
               (remote-end "\\(?::\\)\\([^\n\r]*\\'\\)")
               (class
                (condition-case nil
                    (cond
                     ((equal regexp "\\`/:") 'quoted)
                     ((equal regexp "\\`/[^/:]+:[^/:]*:") 'remote)
                     ;; Tramp's default syntax needs a colon before the
                     ;; second slash.  Completion cannot consume it at all.
                     ((and (string-prefix-p (concat remote-start "\\(") regexp)
                           (or (and (string-suffix-p remote-end regexp)
                                    (emacs-load--slash-free-regexp-p
                                     regexp (length remote-start)
                                     (- length (length remote-end))))
                               (and (string-suffix-p "\\'" regexp)
                                    (emacs-load--slash-free-regexp-p
                                     regexp (length remote-start) (- length 2)))))
                      (string-match-p regexp "") ; reject malformed regexps
                      'remote)
                     ((and (string-suffix-p "\\'" regexp)
                           (emacs-load--slash-free-regexp-p regexp 0 (- length 2)))
                      (string-match-p regexp "")
                      'suffix)
                     (t 'general))
                  (invalid-regexp 'general))))
          (when (>= (hash-table-count emacs-load--handler-regexp-classes) 64)
            (clrhash emacs-load--handler-regexp-classes))
          (puthash (copy-sequence regexp) class emacs-load--handler-regexp-classes)
          class)))

  (defun emacs-load--lookup-handlers-p (file names mode)
    "Return constraints for fast lookup of FILE and its suffixed NAMES.
Return t for full dispatch, or a vector of remote-prefix, directory-suffix
and candidate-suffix checks.  Read operations and inhibition live."
    (let ((tail file-name-handler-alist)
          (case-fold-search nil) (search-spaces-regexp nil)
          remote directory-regexps candidate-names general)
      (while (and (consp tail) (not general))
        (let ((entry (car tail)))
          (when (and (consp entry) (stringp (car entry)))
            (let* ((regexp (car entry))
                   (handler (cdr entry))
                   (class (emacs-load--classify-handler-regexp regexp))
                   (ops (and (symbolp handler) (get handler 'operations)))
                   (expand (and (or (null ops) (memq 'expand-file-name ops))
                                (not (and (eq inhibit-file-name-operation 'expand-file-name)
                                          (memq handler inhibit-file-name-handlers)))))
                   (exists (and (not mode)
                                (or (null ops) (memq 'file-exists-p ops))
                                (not (and (eq inhibit-file-name-operation 'file-exists-p)
                                          (memq handler inhibit-file-name-handlers))))))
              (cond
               ((eq class 'general) (setq general t))
               ((eq class 'remote) (setq remote (or remote expand exists)))
               ((eq class 'suffix)
                ;; Slash-free suffixes see the same end of FILE or NAME
                ;; before and after a slash-terminated directory is added.
                (when expand
                  (if (string-match-p regexp file)
                      (setq general t)
                    (push regexp directory-regexps)))
                (when exists
                  (dolist (name names)
                    (when (string-match-p regexp name)
                      (push name candidate-names)))))))))
        (setq tail (cdr tail)))
      (if general t (vector remote directory-regexps candidate-names))))

  (defun emacs-load--directory-handler-p (directory regexps)
    "Return non-nil if one of REGEXPS matches DIRECTORY for expansion."
    (let ((case-fold-search nil) (search-spaces-regexp nil))
      (while (and regexps (not (string-match-p (car regexps) directory)))
        (setq regexps (cdr regexps)))
      regexps))

  (defun emacs-load--expand-named-home (name)
    "Expand a leading ~USER in NAME using the local account database."
    (if (and (eq system-type 'gnu/linux)
             (fboundp 'emacs-subr-extras--passwd)
             (stringp name)
             (string-prefix-p "~" name))
        (let* ((end (or (string-search "/" name) (length name)))
               (home (and (> end 1)
                          (nth 5 (emacs-subr-extras--passwd
                                  (substring name 1 end))))))
          (if home (concat home (substring name end)) name))
      name))

  (defun emacs-load--expand-lookup-name (file directory)
    "Expand FILE in DIRECTORY with normal handler dispatch."
    (let* ((directory (if (or (null directory) (equal directory ""))
                          default-directory
                        directory))
           (handler (or (find-file-name-handler file 'expand-file-name)
                        (and (stringp directory)
                             (find-file-name-handler directory 'expand-file-name)))))
      (if handler
          (funcall handler 'expand-file-name file directory)
        (expand-file-name (emacs-load--expand-named-home file)
                          (emacs-load--expand-named-home directory)))))

  (defun emacs-load--find-local (file local-name suffixes directories newer mode)
    "Search DIRECTORIES for LOCAL-NAME and SUFFIXES with access MODE.
Use the original FILE for handler discovery and general expansion."
    (let* ((names (mapcar (lambda (suffix) (concat local-name suffix)) suffixes))
           (prefixes emacs-load--directory-prefixes)
           (access-mode (or mode 0))
           (checks (emacs-load--lookup-handlers-p file names mode))
           ;; Linux x86-64 access probes can share the prefix
           ;; in a scratch buffer.  Keep non-ASCII names on the ordinary
           ;; path, where concat and the syscall wrapper handle encoding.
           (tails (and (not (eq checks t))
                       (fboundp 'ptr-write-bytes)
                       (fboundp 'nelisp--target-arch-code)
                       (= (nelisp--target-arch-code) 0)
                       (let (tails non-ascii)
                         (dolist (name names)
                           (if (nelisp--file-name-ascii-p name)
                               (push (concat name "\0") tails)
                             (setq non-ascii t)))
                         (unless non-ascii (nreverse tails)))))
           (max-tail (and tails (apply #'max (mapcar #'length tails))))
           (scratch (and tails (alloc-bytes 4096 8)))
           found)
      (while (and (consp directories) (not found))
        (if (eq checks t)
            (setq found (emacs-load--search-file file suffixes directories newer mode)
                  directories nil)
          (let* ((directory (car directories))
                 (entry (or (gethash directory prefixes)
                            (emacs-load--directory-prefix directory)))
                 (prefix (car entry))
                 (names-left names)
                 (special-names (aref checks 2))
                 found-mtime)
            (if (and prefix
                     (not (and (aref checks 0) (cadr entry)))
                     (not (and (aref checks 1)
                               (emacs-load--directory-handler-p
                                directory (aref checks 1)))))
                (let ((tail-address (and scratch (> scratch 0) (cddr entry)
                                         (<= (+ (length prefix) max-tail) 4096)
                                         (+ scratch (ptr-write-bytes scratch prefix))))
                      (tails-left tails))
                  (while names-left
                    (let ((name (car names-left)))
                      (if (and special-names (memq name special-names))
                          (progn
                            (setq found (emacs-load--search-candidates
                                         prefix names-left newer mode nil found found-mtime)
                                  names-left nil)
                            (unless found
                              (setq checks (emacs-load--lookup-handlers-p file names mode))))
                        (let* ((access
                                (if tail-address
                                    (progn
                                      (ptr-write-bytes tail-address (car tails-left))
                                      ;; faccessat2(AT_FDCWD, ..., AT_EACCESS).
                                      (let ((rc (syscall-direct 439 -100 scratch
                                                                access-mode 512 0 0)))
                                        (if (= rc -38)
                                            (syscall-direct 21 scratch access-mode 0 0 0 0)
                                          rc)))
                                  (nelisp--syscall-path-int
                                   21 (concat prefix name) access-mode))))
                          (when (and (/= access -2) (/= access -20))
                            (let ((candidate (concat prefix name)))
                              (let ((probe (emacs-load--local-file-p
                                            candidate mode access newer)))
                                (when (and probe
                                           (emacs-load--prefer-file-p
                                            candidate probe found found-mtime))
                                  (setq found candidate found-mtime probe)
                                  (unless newer (setq names-left nil)))))))))
                    (setq names-left (cdr names-left)
                          tails-left (cdr tails-left))))
              ;; Expansion and handler callbacks can change the active
              ;; registrations before the next directory.
              (setq found (emacs-load--search-file
                           file suffixes (list directory) newer mode))
              (unless found
                (setq checks (emacs-load--lookup-handlers-p file names mode))))
            (setq directories (cdr directories)))))
      found))

  (defun emacs-load--prefer-file-p (candidate probe previous previous-probe)
    (or (not previous)
        (if (and (consp probe) (consp previous-probe))
            (or (> (car probe) (car previous-probe))
                (and (= (car probe) (car previous-probe))
                     (> (cdr probe) (cdr previous-probe))))
          (emacs-load--newer-file-p candidate previous))))

  (defun emacs-load--local-file-p (file access-mode &optional access-result mtime)
    "Test local FILE, optionally using an ACCESS-RESULT already obtained.
ACCESS-MODE, when non-nil, is the caller's explicit access mask.
With MTIME, return the observed modification timespec when available."
    (cond
     (access-mode
      (and (if (fboundp 'nelisp--syscall-path-int)
               (= 0 (or access-result
                        (nelisp--syscall-path-int 21 file access-mode)))
             (and (file-exists-p file)
                  (or (= 0 (logand access-mode 1)) (file-executable-p file))
                  (or (= 0 (logand access-mode 2)) (file-writable-p file))
                  (or (= 0 (logand access-mode 4)) (file-readable-p file))))
           (if (fboundp 'nelisp--syscall-stat)
               (eq (nelisp--syscall-stat file) 'file)
             (not (file-directory-p file)))))
     ((and (eq system-type 'gnu/linux)
           (fboundp 'nelisp--target-arch-code)
           (= (nelisp--target-arch-code) 0)
           (fboundp 'nelisp--syscall-path-int))
      ;; GNU's ordinary lookup opens the file.  F_OK cheaply eliminates
      ;; ENOENT and ENOTDIR on the hot path, but cannot establish readability
      ;; or rule out sockets.  Check the type on the open descriptor.
      (let* ((stat (alloc-bytes 144 8))
             (fd (nelisp--syscall-path-int 2 file #o2000000))) ; O_RDONLY | O_CLOEXEC
        (when (>= fd 0)
          (unwind-protect
              (and (= 0 (syscall-direct 5 fd stat 0 0 0 0)) ; fstat
                   (/= (logand (ptr-read-u64 stat 24) #o170000) #o40000)
                   ;; GNU openp records mtime from this same fstat.
                   (if mtime
                       (cons (ptr-read-u64 stat 88) (ptr-read-u64 stat 96))
                     t))
            (syscall-direct 3 fd 0 0 0 0 0))))) ; close
     (t (and (file-readable-p file) (not (file-directory-p file))))))

  (defun emacs-load--absolute-local-p (file directory mode)
    "Check if absolute FILE can bypass expansion and handler dispatch.
DIRECTORY is the expansion base, or nil for a literal search."
    (and (not (or (string-prefix-p "/:" file) (string-suffix-p "/" file)
                  (nelisp--string-search "//" file 0)
                  (nelisp--string-search "\0" file 0)
                  (nelisp--file-name-needs-normalization-p file)))
         (or (null directory)
             (and (stringp directory) (> (length directory) 0)
                  (eq (aref directory 0) ?/)
                  (not (string-prefix-p "/:" directory))
                  (not (nelisp--string-search "\0" directory 0))
                  (eq (multibyte-string-p directory) (multibyte-string-p file))))
         (let ((checks (emacs-load--lookup-handlers-p file (list file) mode)))
           (and (not (eq checks t))
                (not (aref checks 2))
                (not (and (aref checks 0)
                          (or (emacs-load--possible-remote-prefix-p file)
                              (and directory
                                   (emacs-load--possible-remote-prefix-p directory)))))
                (not (and directory
                          (emacs-load--directory-handler-p directory (aref checks 1))))))))

  (defun emacs-load--find-file (file suffixes path newer &optional predicate)
    "Find FILE using ordered SUFFIXES in PATH, as GNU openp does.
NEWER selects by mtime within the first matching directory.  PREDICATE
can be an access mask or a function that may return `dir-ok'."
    (unless (stringp file)
      (signal 'wrong-type-argument (list 'stringp file)))
    (let ((tail suffixes))
      (while (consp tail)
        (unless (stringp (car tail))
          (signal 'wrong-type-argument (list 'stringp (car tail))))
        (setq tail (cdr tail))))
    (let* ((absolute (and (file-name-absolute-p file)
                          (not (string-prefix-p "~" file))
                          (if path t 'literal)))
           (directories (or path '(nil)))
           (mask (and (integerp predicate) (>= predicate 0) predicate))
           (filter (and predicate (not (eq predicate t)) (not mask) predicate))
           ;; A leading ./ has the same local candidates as its tail.
           ;; A handler can still inspect FILE's original spelling.
           (local-name (if (string-prefix-p "./" file) (substring file 2) file))
           (suffixes (or suffixes '(""))))
      (cond
       ((> (or mask 0) 7) nil)
       ;; With no suffix to append, a canonical absolute name can go
       ;; directly to the filesystem if neither it nor its base is handled.
       ((and absolute (consp directories) (not filter)
             (eq system-type 'gnu/linux)
             (fboundp 'nelisp--string-search)
             (fboundp 'nelisp--file-name-needs-normalization-p)
             (equal suffixes '(""))
             (emacs-load--absolute-local-p
              file
              (unless (eq absolute 'literal)
                (let ((directory (car directories)))
                  (if (and (stringp directory) (not (equal directory "")))
                      directory default-directory)))
              mask))
        (and (emacs-load--local-file-p file mask)
             (substring-no-properties file)))
       ((and (not filter)
             (eq system-type 'gnu/linux)
             (fboundp 'nelisp--syscall-path-int)
             (fboundp 'nelisp--string-search)
             (listp suffixes)
             (fboundp 'emacs-fns--plain-relative-name-p)
             (not (nelisp--string-search "\0" file 0))
             (emacs-fns--plain-relative-name-p local-name))
        (emacs-load--find-local file local-name suffixes directories newer mask))
       (t (emacs-load--search-file
           file suffixes directories newer mask filter absolute)))))

  (defun emacs-load--search-file
      (file suffixes directories newer mode &optional filter absolute)
    "Search DIRECTORIES with full expansion and predicate handling."
    (let (found)
      (while (and (consp directories) (not found))
        (let* ((directory (car directories))
               (base (cond
                      ((eq absolute 'literal) file)
                      ((or (null directory) (stringp directory))
                       (emacs-load--expand-lookup-name file directory))
                      ((file-name-absolute-p file)
                       (emacs-load--expand-lookup-name file nil)))))
          (when (and base (> (length base) 2) (string-prefix-p "/:" base))
            (setq base (substring base 2)))
          (when base
            (setq found (emacs-load--search-candidates base suffixes newer mode filter))))
        (setq directories (and (not absolute) (cdr directories))))
      found))

  (defun emacs-load--search-candidates
      (base remaining newer mode filter &optional found found-mtime)
    "Check each suffix in REMAINING appended to the already expanded BASE."
    (while (and (consp remaining) (or newer (not found)))
      (let* ((candidate (concat base (car remaining)))
             (handler (find-file-name-handler candidate 'file-exists-p)))
        (cond
         (filter
          (let ((answer (funcall filter candidate)))
            (when (and answer
                       (or (eq answer 'dir-ok)
                           (not
                            (let ((dir-handler
                                   (find-file-name-handler candidate 'file-directory-p)))
                              (if dir-handler
                                  (funcall dir-handler 'file-directory-p candidate)
                                (file-directory-p candidate))))))
              (setq found candidate remaining nil))))
         ((and handler (not mode))
          ;; GNU's file-readable-p expands the candidate before selecting
          ;; its handler.  openp still returns the original candidate.
          (let* ((expanded (expand-file-name candidate))
                 (read-handler (find-file-name-handler expanded 'file-readable-p)))
            (when (if read-handler
                      (funcall read-handler 'file-readable-p expanded)
                    (file-readable-p expanded))
              ;; GNU returns a handler result immediately even with NEWER.
              (setq found candidate remaining nil))))
         (t
          (let ((probe (emacs-load--local-file-p candidate mode nil newer)))
            (when (and probe
                       (emacs-load--prefer-file-p candidate probe found found-mtime))
              (setq found candidate found-mtime probe))))))
      (setq remaining (cdr remaining)))
    found)

  (defun emacs-load--resolve-file (file nosuffix must-suffix)
    "Resolve FILE using GNU load suffix and search-path semantics."
    (unless (string= file "")
      (let* ((suffixes (emacs-load--candidate-suffixes file nosuffix must-suffix))
             (path (and (boundp 'load-path) load-path)))
        (when (and (boundp 'load-path-filter-function)
                   (functionp load-path-filter-function))
          (setq path (funcall load-path-filter-function path file
                              (unless nosuffix suffixes))))
        (emacs-load--find-file file suffixes path load-prefer-newer))))

  (defun locate-file-internal (filename path &optional suffixes predicate)
    "Search PATH for FILENAME with ordered SUFFIXES and optional PREDICATE."
    (emacs-load--find-file filename suffixes path nil predicate))

  (defun locate-file (filename path &optional suffixes predicate)
    "Search PATH for a readable FILENAME with optional SUFFIXES.
A function PREDICATE replaces the readability check and may return
`dir-ok' to accept a directory.  An integer is an access mask; the
symbols `exists', `executable', `writable' and `readable' also specify
access modes."
    (when (and predicate (symbolp predicate) (not (functionp predicate)))
      (setq predicate (list predicate)))
    (when (and (consp predicate) (not (functionp predicate)))
      (setq predicate (logior (if (memq 'executable predicate) 1 0)
                              (if (memq 'writable predicate) 2 0)
                              (if (memq 'readable predicate) 4 0))))
    (locate-file-internal filename path suffixes predicate))

  (defun load (file &optional noerror nomessage nosuffix must-suffix)
    "Resolve FILE through `load-path' and execute it."
    (unless (stringp file)
      (signal 'wrong-type-argument (list 'stringp file)))
    (let ((handler (find-file-name-handler file 'load)))
      (if handler
          (funcall handler 'load file noerror nomessage nosuffix must-suffix)
        (let* ((file (if noerror
                         (condition-case nil (substitute-in-file-name file)
                           (error nil))
                       (substitute-in-file-name file)))
               (resolved (and file
                              (emacs-load--resolve-file file nosuffix must-suffix))))
          (cond
           (resolved
            (let ((handler (and (find-file-name-handler resolved 'file-exists-p)
                                (find-file-name-handler resolved
                                                        (if (equal file resolved) 'load t)))))
              (if handler
                  (funcall handler 'load resolved noerror nomessage t)
                (nelisp--load-resolved-file resolved noerror))))
           (noerror nil)
           (t (signal 'file-error (list "Cannot open load file" file))))))))

(defun load-file (file)
  "Execute exactly FILE after absolute-name expansion.
Unlike `load', this never searches `load-path' or adds a suffix."
  (let ((resolved (expand-file-name file)))
    (nelisp--load-resolved-file resolved nil)))

  ;; emacs-fns selected source-only defaults before this loader existed.
  (when (equal load-suffixes '(".el"))
    (setq load-suffixes '(".elc" ".el")))
  (provide 'nelisp-elc-load)

)

;; GNU loaddefs installs this macro autoload before packages such as
;; treesit.el use it.  Keep the implementation lazy and require the genuine
;; vendored shortdoc library when expansion is first requested.
;; Byte-compiled expansions of the macro access this registry directly.
(defvar shortdoc--groups nil
  "Short documentation groups registered by loaded libraries.")

(unless (fboundp 'define-short-documentation-group)
  (autoload 'define-short-documentation-group "shortdoc" nil nil 'macro))

(provide 'emacs-load)

;;; emacs-load.el ends here
