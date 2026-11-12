;;; nelisp-parity-fuzz.el --- generative Emacs-vs-standalone differential -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; test/nelisp-shadow-differential-cases.el is a corpus I wrote by hand.  It
;; is a set of samples, not a search: every divergence it holds is one I
;; already knew to look for.  This is the search.
;;
;; The conditions for generative testing are unusually good here, which is
;; why it is worth building rather than writing more samples:
;;
;;   * the ORACLE is free and local -- stock Emacs, one subprocess;
;;   * the COMPARISON is total -- print both sides with `format "%S"' and
;;     compare bytes, no per-function expected values to maintain;
;;   * the DOMAIN is already enumerated -- docs/emacs-compat-table.txt lists
;;     every name both runtimes define.
;;
;; What it does NOT do is decide what a correct answer is.  It reports where
;; the two disagree; which one is right is still a reading job.  A generator
;; that answered that would be a second implementation of Emacs.
;;
;; DETERMINISM (host independence).  The name universe is the compat table
;; (minus the committed skip list) and nothing the host answers; every name
;; gets `NELISP_FUZZ_PER_NAME' cases (default 3) drawn from an RNG seeded by
;; a stable FNV-1a hash of (seed, name, k).  A name's cases therefore do not
;; depend on any other name, on list order, on sharding, or on which names
;; the host Emacs happens to define.  A name the host lacks is an ORACLE GAP:
;; reported and capped, never silently dropped and never a divergence.  A
;; case GNU Emacs cannot finish (waits for input) is confirmed alone and
;; recorded as a HANG; hangs are ratcheted through
;; tools/parity-fuzz-known-hangs.txt exactly like divergences.
;;
;; Environment: NELISP_FUZZ_SEED, NELISP_FUZZ_PER_NAME, NELISP_FUZZ_ONLY,
;; NELISP_FUZZ_SHARD=i/N (names with fnv(name) mod N = i), NELISP_FUZZ_RESULTS
;; (dump per-name status), NELISP_FUZZ_SHUFFLE=n and NELISP_FUZZ_HIDE=regexp
;; (test hooks: permute the name order / pretend the host lacks those names).
;;
;; Do not run this at the same time as a build.  It executes ./target/nelisp
;; continuously, and a concurrent `make standalone-reader' fails with "Text
;; file busy" relinking the binary out from under it.  That is not a build
;; defect; half an hour went into establishing which of the two it was.
;;
;; Usage:
;;   emacs --batch -Q -l tools/nelisp-parity-fuzz.el          # default budget
;;   NELISP_FUZZ_SEED=7 NELISP_FUZZ_CASES=4000 emacs --batch -Q -l ...
;;   NELISP_FUZZ_ONLY='^string-' ...                          # one family
;; or: make parity-fuzz

;;; Code:

(require 'cl-lib)

(defvar nelisp-parity-fuzz--root
  (or (getenv "NELISP_REPO_ROOT") default-directory))

(defvar nelisp-parity-fuzz--binary
  (expand-file-name "target/nelisp" nelisp-parity-fuzz--root))

(defvar nelisp-parity-fuzz--table
  (expand-file-name "docs/emacs-compat-table.txt" nelisp-parity-fuzz--root))

;; Generated calls RUN.  `make-symbolic-link', `rename-file' and friends were
;; doing so with the repository as their working directory, and two of their
;; symlinks -- `abc' and `[a-z]+' -- got committed by a later `git add -A'.
;; Worse than the mess: the corpus then measured THEM.  (file-truename "abc")
;; disagreed because a stray symlink existed, not because the runtimes did,
;; and the finding would have vanished the moment somebody cleaned up.  Both
;; sides get a scratch directory instead, so a filesystem call lands
;; somewhere disposable and both runtimes see the same empty world.
(defvar nelisp-parity-fuzz--scratch nil
  "Working directory both runtimes are launched in.")

(defun nelisp-parity-fuzz--scratch-dir ()
  "Return the scratch directory, creating it and fixing its mode.
Called before EVERY capture, not once: a generated `set-file-modes' call
took the scratch directory's own permissions away mid-run, and every
process launched after that died with \"Setting current directory:
Permission denied\" -- reported as a hundred missing results rather than as
the one call that caused them."
  (let ((d (let ((dir (expand-file-name "target/parity-fuzz-scratch/"
                                        nelisp-parity-fuzz--root)))
             ;; ONE directory, EMPTIED before every capture.  Generated
             ;; calls leave files behind, so without the clearing the second
             ;; runtime saw what the first had just created -- and with a
             ;; directory per side instead, every path-returning call
             ;; disagreed because the two paths were different.  Same name,
             ;; same empty contents, for both.
             (when (file-directory-p dir)
               (condition-case err
                   (delete-directory dir t)
                 (error (message "parity-fuzz: cannot clear %s: %S" dir err))))
             dir)))
    (unless (file-directory-p d) (make-directory d t))
    (condition-case err
        (set-file-modes d #o755)
      ;; Said out loud rather than swallowed: if the scratch directory cannot
      ;; be made usable again, every capture after this point fails to start
      ;; and the run reports missing results instead of the reason.
      (error (message "parity-fuzz: cannot reset %s: %S" d err)))
    d))

;; ---------------------------------------------------------------------------
;; Deterministic RNG.  `random' with a seed string is reproducible within one
;; Emacs version but not across them, and a fuzz report nobody can replay is
;; a bug report without a repro.  xorshift64* is fifteen lines and exact.
;; ---------------------------------------------------------------------------

(defvar nelisp-parity-fuzz--state 88172645463325252)

(defun nelisp-parity-fuzz--seed (n)
  (setq nelisp-parity-fuzz--state (if (zerop n) 88172645463325252 n)))

(defun nelisp-parity-fuzz--next ()
  "Next pseudo-random non-negative fixnum."
  (let ((x nelisp-parity-fuzz--state))
    (setq x (logxor x (ash x 13)))
    (setq x (logand x most-positive-fixnum))
    (setq x (logxor x (ash x -7)))
    (setq x (logxor x (ash x 17)))
    (setq x (logand x most-positive-fixnum))
    (setq nelisp-parity-fuzz--state x)
    x))

(defun nelisp-parity-fuzz--pick (seq)
  (nth (mod (nelisp-parity-fuzz--next) (length seq)) seq))

;; ---------------------------------------------------------------------------
;; Argument pool.  Each entry is a QUOTED FORM that both runtimes can read --
;; not a value, because the two sides are separate processes and the only
;; channel between them is source text.
;;
;; The pool is deliberately weighted toward the shapes that broke things on
;; 2026-08-19: an empty sequence, an improper list, a negative index, an
;; index past the end, a non-ASCII string, a float where an integer is
;; expected, and the wrong type entirely.  A uniform pool over "plausible"
;; values would have found none of them.
;; ---------------------------------------------------------------------------

(defconst nelisp-parity-fuzz--pool
  '(nil t 0 1 -1 2 7 -7 100 1.5 -1.5 0.0
    "" "a" "abc" "ABC" "aXbXc" " a b " "\t x \n"
    "\u00e9" "\u3042\u3044" "a.b*c" "[a-z]+"
    ?a ?A ?0 ?\s 12354
    'foo 'nil 't :key
    '(1 2 3) '(1) '() '("a" "b") '(1 . 2) '((a . 1) (b . 2))
    '(1 2 . 3)
    [] [1 2 3] ["a" "b"]
    (list 'quote 'sym)))

(defconst nelisp-parity-fuzz--skip
  ;; Names whose value is not a function of their arguments, or whose call
  ;; would end the process / touch the world.  Excluding them is not
  ;; "hiding a failure": a differential over these compares noise.
  '(;; time, environment, randomness
    current-time float-time current-time-string format-time-string random
    getenv setenv emacs-version emacs-pid system-name user-login-name
    ;; `make-temp-name' is randomness by definition, and `make-temp-file'
    ;; is that plus a file.  Neither can agree across two processes.
    make-temp-name make-temp-file
    ;; A timer records the CURRENT TIME in its own slots.
    run-at-time run-with-timer run-with-idle-timer timer-create
    ;; `lwarn'/`display-warning' accumulate `warning-suppress-log-types' as
    ;; they go, so the SAME call answers differently the second time -- a
    ;; measurement here disagreed with a clean host on the same input.
    lwarn display-warning warn
    ;; these read whichever buffer the host happens to have current, and
    ;; batch Emacs moves between *scratch* and *Messages* on its own
    buffer-substring-no-properties insert-file-contents-literally
    buffer-name buffer-size
    ;; THIS TOOL'S OWN PROTOCOL is stdout -- one "##KEY\tRESULT\n" line per
    ;; case -- and a call that writes to stdout writes INTO that line.  The
    ;; result field then holds whatever survived the split, which is how
    ;; (terpri 1.5 "x") was reported as answering nil here while a direct
    ;; measurement of the same call signals `invalid-function' every time.
    ;; Not a runtime difference; a measurement the harness cannot make.
    princ prin1 print terpri print-char write-char
    ;; reads the host obarray: which symbols exist is host state
    intern-soft
    ;; nil means the CURRENT BUFFER, and batch Emacs moves between *scratch*
    ;; and *Messages* on its own -- the answer names whichever it is
    process-status
    ;; searches the HOST's load-path, whose contents this runtime does not
    ;; share -- (locate-library "") answers an Emacs .elc there and nil here
    locate-library require-with-check
    ;; the host pre-populates properties on its own symbols -- nil carries
    ;; event-symbol-element-mask and friends -- so this reads host state,
    ;; not the argument
    symbol-plist
    ;; inode numbers and mtimes: not a function of the arguments
    file-attributes
    ;; process / file / world
    load require provide kill-emacs delete-file write-region make-directory
    insert-file-contents find-file expand-file-name file-exists-p
    file-readable-p file-directory-p file-regular-p directory-files
    start-process call-process make-process accept-process-output
    delete-process kill-process sleep-for sit-for
    ;; reads STDIN when its stream is nil, which eats the rest of the
    ;; driver file -- the case after it then goes unreported and reads as an
    ;; abort.  `read-from-string' covers the same code with a stream that
    ;; cannot do that.
    read
    ;; interactive / display / buffer state
    message read-string read-from-minibuffer y-or-n-p
    ;; wait for terminal input: in batch Emacs they block on stdin, so the
    ;; driver never finishes (read-char was reached once the arity draw no
    ;; longer stopped the run at startup)
    read-char read-char-exclusive read-event read-key read-key-sequence
    read-key-sequence-vector read-passwd read-number read-buffer
    read-file-name read-directory-name read-command read-variable
    read-answer read-multiple-choice read-char-choice
    read-char-from-minibuffer read-shell-command read-regexp
    completing-read completing-read-multiple yes-or-no-p
    insert erase-buffer current-buffer get-buffer-create set-buffer
    buffer-string buffer-substring point goto-char
    ;; control flow that a generated call cannot use meaningfully
    throw signal error quit exit-recursive-edit recursive-edit
    ;; identity is not stable across runtimes by design (Doc 22 A11/A20)
    make-symbol gensym cl-gensym sxhash sxhash-equal sxhash-eq sxhash-eql
    ;; mutators: the differential prints the RESULT, and an in-place
    ;; mutation's result is the least interesting half of what it did
    setcar setcdr aset puthash remhash clrhash sort nreverse nconc
    delete-dups nbutlast ntake))

(defun nelisp-parity-fuzz--gap (name emacs nelisp)
  "Return the missing TYPE that explains EMACS vs NELISP, or nil.

Four Emacs types this runtime does not have, and a differential over them
compares a value against a shape that cannot exist here.  They are matched
on the ANSWER rather than on the name, so a call that diverges for some
OTHER reason still counts as a disagreement -- skip-listing the names would
have thrown away every type check those functions do.

They are reported, every run, in their own section.  A gap that stops being
printed is a gap somebody has to rediscover."
  (cond
   ((and (stringp emacs) (string-prefix-p "#&" emacs))
    "bool-vector: Emacs has a packed bit type; this runtime uses a plain vector of t/nil")
   ((and (stringp emacs) (string-prefix-p "#[" emacs)
         (stringp nelisp) (string-prefix-p "#[" nelisp))
    "byte-code function: Emacs's stdlib is compiled, so the value it hands back prints as byte code; this one is an interpreted closure")
   ((and (stringp emacs) (string-match-p "\\`-?[0-9]\\{19,\\}\\'" emacs))
    "bignum: the answer needs more than 64 bits and this runtime has fixnums only")
   ;; A unibyte answer PRINTS as octal escapes where a multibyte one prints
   ;; the characters.  Matched on the shape rather than the name, so a call
   ;; that diverges for some other reason still counts.
   ((and (stringp emacs) (string-match-p "\\\\[0-3][0-7][0-7]" emacs)
         (stringp nelisp) (not (string-match-p "\\\\[0-3][0-7][0-7]" nelisp)))
    "unibyte string: Emacs tracks a per-string multibyte flag; every string here is UTF-8 bytes")
   ((and (stringp emacs) (string-match-p "Multibyte character in data" emacs))
    "unibyte string: Emacs tracks a per-string multibyte flag; every string here is UTF-8 bytes")
   ((memq name '(string-as-unibyte string-make-unibyte string-to-unibyte
                 string-as-multibyte string-to-multibyte multibyte-string-p))
    "unibyte string: Emacs tracks a per-string multibyte flag; every string here is UTF-8 bytes")
   (t nil)))

(defun nelisp-parity-fuzz--fnv (str)
  "Stable 32-bit FNV-1a hash of STR (never the host's `sxhash')."
  (let ((h 2166136261))
    (dolist (c (string-to-list str))
      (setq h (logand (* (logxor h c) 16777619) #xFFFFFFFF)))
    h))

(defun nelisp-parity-fuzz--universe ()
  "Every table name that is not skip-listed.
No host, ONLY, shard or order input: the same list on every machine."
  (let ((names nil))
    (with-temp-buffer
      (insert-file-contents nelisp-parity-fuzz--table)
      (goto-char (point-min))
      (while (re-search-forward "^shared-\\(?:shadowing\\|deferring\\) +\\([^ ]+\\)" nil t)
        (let ((sym (intern (match-string 1))))
          (when (and (not (memq sym nelisp-parity-fuzz--skip))
                     (not (string-prefix-p "nelisp" (symbol-name sym))))
            (push sym names)))))
    (sort (delete-dups names) #'string-lessp)))

(defvar nelisp-parity-fuzz--primed nil)
(defvar nelisp-parity-fuzz--prime-failures nil
  "Names whose autoload could not be resolved while priming.")

(defun nelisp-parity-fuzz--note-prime-failure (sym err)
  "Record that SYM's autoload failed with ERR (reported, not fatal)."
  (push (cons sym err) nelisp-parity-fuzz--prime-failures))

(defun nelisp-parity-fuzz--prime-host ()
  "Resolve every autoload of the whole universe once, before any arity read.
A name's arity can change when a library loaded for ANOTHER name adds advice
or turns a stub into a generic function (seq-*, org-agenda's advice), so
reading arities while loading gives order- and host-dependent cases.  After
everything is loaded the state does not depend on the order."
  (unless nelisp-parity-fuzz--primed
    (setq nelisp-parity-fuzz--primed t)
    (dolist (sym (nelisp-parity-fuzz--universe))
      (when (fboundp sym)
        (condition-case err
            (let ((f (indirect-function sym)))
              (when (autoloadp f) (autoload-do-load f sym)))
          (error
           (message "parity-fuzz: priming %s failed: %S" sym err)
           (nelisp-parity-fuzz--note-prime-failure sym err)))))))

(defun nelisp-parity-fuzz--names ()
  "Shared names from the compat table, minus the ones a differential cannot judge.
Selection never consults the host (no `fboundp'/`macrop'): the universe is
the same on every machine.  Host-dependent classification is
`nelisp-parity-fuzz--host-kind'."
  (let ((names (nelisp-parity-fuzz--universe)))
    (let ((only (getenv "NELISP_FUZZ_ONLY")))
      (when (and only (not (string-empty-p only)))
        (setq names (cl-remove-if-not
                     (lambda (s) (string-match-p only (symbol-name s))) names))))
    (setq names (sort (delete-dups names) #'string-lessp))
    (let ((sh (getenv "NELISP_FUZZ_SHARD")))
      (when (and sh (string-match "\\`\\([0-9]+\\)/\\([0-9]+\\)\\'" sh))
        (let ((i (string-to-number (match-string 1 sh)))
              (n (string-to-number (match-string 2 sh))))
          (setq names (cl-remove-if-not
                       (lambda (s) (= (mod (nelisp-parity-fuzz--fnv (symbol-name s)) n) i))
                       names)))))
    (let ((shuf (getenv "NELISP_FUZZ_SHUFFLE")))
      (when (and shuf (not (string-empty-p shuf)))
        (setq names (sort names
                          (lambda (a b)
                            (< (nelisp-parity-fuzz--fnv (format "%s:%s" shuf a))
                               (nelisp-parity-fuzz--fnv (format "%s:%s" shuf b))))))))
    names))

(defun nelisp-parity-fuzz--host-kind (sym)
  "Classify SYM on the host: `gap' (oracle lacks it), `not-callable' or `ok'.
NELISP_FUZZ_HIDE (a regexp) makes matching names look absent, to test that a
smaller host shows up as oracle gaps and not as divergences."
  (let ((hide (getenv "NELISP_FUZZ_HIDE")))
    (cond ((and hide (not (string-empty-p hide))
                (string-match-p hide (symbol-name sym)))
           'gap)
          ((not (fboundp sym)) 'gap)
          ((or (macrop sym) (special-form-p sym)) 'not-callable)
          (t 'ok))))

(defvar nelisp-parity-fuzz--no-arity nil
  "Names whose arity could not be read, so no call was generated for them.")

(defun nelisp-parity-fuzz--note-unreadable-arity (sym why)
  "Record that no call was generated for SYM, and WHY.

A named recorder rather than a bare `push' so `make fallback-inventory' can
see that this handler records: it classifies by the function a handler
calls, which is how it tells a fallback that reports itself from one that
swallows.  Reported in the run summary."
  (push (cons sym why) nelisp-parity-fuzz--no-arity))

(defun nelisp-parity-fuzz--arity (sym)
  "Return (MIN . MAX) for SYM, MAX nil when &rest, or nil when unknown.

A name whose arity cannot be read is RECORDED, not just skipped.  Dropping
it silently shrinks the search and leaves the run reporting a name count it
did not actually cover -- the same thing the resume cap is careful about a
few lines down, and what `make fallback-inventory' flags a bare handler for."
  (condition-case err
      (let ((a (progn
                 ;; An autoload object reports (0 . many) until it is loaded,
                 ;; so the arity -- and every case drawn from it -- would
                 ;; depend on what the host had loaded before.  Resolve it.
                 (let ((f (indirect-function sym)))
                   (when (autoloadp f) (autoload-do-load f sym)))
                 ;; Libraries loaded for OTHER names add advice (org loads
                 ;; `exchange-point-and-mark' :after advice), which reports
                 ;; (0 . many); read the arity of the advised original.
                 (let ((f (indirect-function sym)))
                   (while (and (fboundp 'advice--p) (advice--p f))
                     (setq f (advice--cd*r f)))
                   (func-arity f)))))
        (or (and (consp a) (integerp (car a))
                 (cons (car a) (if (integerp (cdr a)) (cdr a) nil)))
            (progn (nelisp-parity-fuzz--note-unreadable-arity sym 'unusable-arity)
                   nil)))
    (error (nelisp-parity-fuzz--note-unreadable-arity sym err) nil)))

(defun nelisp-parity-fuzz--gen-call (sym)
  "Generate one quoted call form for SYM, or nil when its arity is unusable."
  (let* ((ar (nelisp-parity-fuzz--arity sym))
         (lo (and ar (car ar)))
         (hi (and ar (or (cdr ar) (max 2 (car ar))))))
    (when (and lo (<= lo 4))
      ;; A function needing 4 arguments has no room under the cap of 3; the
      ;; draw still happens (so later cases keep their random sequence) and
      ;; the call takes exactly LO arguments.
      (let* ((n (+ lo (if (> hi lo)
                          (mod (nelisp-parity-fuzz--next) (max 1 (1+ (- (min hi 3) lo))))
                        0)))
             (args (cl-loop repeat n
                            collect (nelisp-parity-fuzz--pick nelisp-parity-fuzz--pool))))
        (cons sym args)))))

;; ---------------------------------------------------------------------------
;; Running one side.  Every case is wrapped so an error is a VALUE: a case
;; that signals in both runtimes with the same condition is agreement, and
;; the interesting failures are exactly the ones where one signals and the
;; other answers.
;; ---------------------------------------------------------------------------

;; Emacs's printer does NOT escape a tab or a newline inside a string, so
;; `prin1-to-string' of a case built from the pool entry with a tab in it
;; contains those characters literally.  Embedding that as a KEY broke the
;; one-line ##KEY<TAB>RESULT format from the inside: the key held a tab and
;; a newline, so the row matched on neither side and the whole tail of the
;; chunk read as unreported -- which the runner then attributed to an abort.
;; "47 cases NOT RUN, 0 aborts" came from that, and it also hid a real one:
;; calling a non-function does end the process, which is what stopped the
;; chunk in the first place.
;;
;; The prose lives here rather than in a docstring because it needs to quote
;; a string literal, and an unescaped quote inside a docstring ends it -- the
;; reader then reads the rest as code.  That is exactly how this function
;; failed with `void-variable (\t)' on its first run.
(defun nelisp-parity-fuzz--seed-case (seed name k)
  "Seed the RNG from a stable hash of (SEED, NAME, K) only."
  (let ((h1 (nelisp-parity-fuzz--fnv (format "%d:%s:%d" seed name k)))
        (h2 (nelisp-parity-fuzz--fnv (reverse (format "%d/%s/%d" seed name k)))))
    (nelisp-parity-fuzz--seed (logior 1 (logxor (ash h1 24) (ash h2 3) h1)))
    (dotimes (_ 4) (nelisp-parity-fuzz--next))))

(defun nelisp-parity-fuzz--cases-for (sym seed per-name)
  "The PER-NAME calls generated for SYM; a pure function of SEED and SYM."
  (nelisp-parity-fuzz--prime-host)
  (let ((out nil))
    (dotimes (k per-name)
      (nelisp-parity-fuzz--seed-case seed sym k)
      (let ((c (nelisp-parity-fuzz--gen-call sym)))
        (when (and c (not (member c out))) (push c out))))
    (nreverse out)))

(defun nelisp-parity-fuzz--key (case)
  "Line-safe identity for CASE."
  (let* ((raw (prin1-to-string case))
         (i 0) (n (length raw)) (out ""))
    (while (< i n)
      (let ((c (aref raw i)))
        (setq out (concat out (cond ((eq c 9) "\\t")
                                    ((eq c 10) "\\n")
                                    ((eq c 13) "\\r")
                                    (t (char-to-string c))))))
      (setq i (1+ i)))
    out))

(defun nelisp-parity-fuzz--driver (cases)
  "Return driver source that prints one tab-separated line per case in CASES."
  (concat
   ;; Fast path: a result with no newline, tab or return -- which is nearly
   ;; all of them -- is handed back UNCHANGED.  The character loop rebuilt
   ;; the whole string per character, so a 12KB answer cost 76MB of copying
   ;; and the two runtimes disagreed about `(string-pad "a" 12354)' because
   ;; one of them had not finished writing it.  A harness slow enough to
   ;; truncate its own measurement reports that as a finding.
   "(defun pfz--needs-flat (s) (let ((i 0) (n (length s)) (hit nil)) "
   "(while (and (< i n) (not hit)) (let ((c (aref s i))) "
   "(if (or (eq c 10) (eq c 13) (eq c 9)) (setq hit t))) (setq i (1+ i))) hit))\n"
   "(defun pfz--flat (s) (if (not (pfz--needs-flat s)) s "
   "(let ((i 0) (n (length s)) (parts nil)) (while (< i n) "
   "(let ((c (aref s i))) (setq parts (cons (cond ((eq c 10) \"\\\\n\") "
   "((eq c 13) \"\\\\r\") ((eq c 9) \"\\\\t\") (t (char-to-string c))) parts))) "
   "(setq i (1+ i))) (apply (function concat) (nreverse parts)))))\n"
   (mapconcat
    (lambda (c)
      (format "(princ \"##\")(princ %S)(princ \"\\t\")(princ (pfz--flat (condition-case e (format \"%%S\" %s) (error (format \"ERR %%S\" e)))))(princ \"\\n\")"
              (nelisp-parity-fuzz--key c) (prin1-to-string c)))
    cases "\n")
   "\n"))

(defun nelisp-parity-fuzz--run (cases)
  "Run CASES through Emacs and the standalone; return an alist of (CASE E . N)."
  (let* ((dir (expand-file-name "target" nelisp-parity-fuzz--root))
         (file (expand-file-name "parity-fuzz-driver.el" dir)))
    (make-directory dir t)
    (with-temp-file file (insert (nelisp-parity-fuzz--driver cases)))
    (let ((emacs-out (nelisp-parity-fuzz--capture
                      (list (expand-file-name invocation-name invocation-directory)
                            "--batch" "-Q" "-l" file)))
          (nelisp-out (nelisp-parity-fuzz--capture
                       (list nelisp-parity-fuzz--binary "--load" file))))
      (nelisp-parity-fuzz--zip cases emacs-out nelisp-out))))

(defconst nelisp-parity-fuzz--max-resumes 60
  "How many times one chunk may resume past an aborting call.")

(defvar nelisp-parity-fuzz--dropped 0
  "Cases a resume cap skipped; reported, never silently absorbed.")

(defun nelisp-parity-fuzz--run-recovering (cases)
  "Run CASES, resuming past any call that ends the standalone process.

A batch that aborts reports nothing after the abort point, so every later
case in it looks like a disagreement when in fact it was never run.  That
inflates the finding count AND overstates coverage at the same time: the
run claims to have checked cases it never reached.  Resuming after the
first unreported case keeps both numbers honest -- the aborting call stays
in the results as `:missing\=', which is what the abort report keys on."
  (let ((out nil) (todo cases) (resumes 0))
    (while todo
      (let* ((rows (nelisp-parity-fuzz--run todo))
             (gnu-idx (cl-position-if (lambda (r) (eq (nth 1 r) :missing)) rows))
             (idx (cl-position-if (lambda (r) (eq (nth 2 r) :missing)) rows)))
        (cond
         ;; GNU stopped first: the case waited for input or ran past the side
         ;; timeout.  It is not a disagreement (GNU gave no answer), so it is
         ;; listed separately and the chunk resumes after it.
         ((and gnu-idx (or (null idx) (<= gnu-idx idx)))
          ;; The chunk stopped here.  Under load that proves nothing, so the
          ;; case is re-run ALONE with a longer budget; only a case that
          ;; still cannot finish is a hang.
          (setq out (append out (cl-subseq rows 0 gnu-idx)))
          (let ((alone (nelisp-parity-fuzz--probe (nth 0 (nth gnu-idx rows))
                                                  nelisp-parity-fuzz--confirm-timeout)))
            (when alone (setq out (append out (list alone)))))
          (setq todo (nthcdr (1+ gnu-idx) todo)))
         ((null idx) (setq out (append out rows)) (setq todo nil))
         ;; Each resume re-runs the whole remaining tail, so a chunk with many
         ;; aborting calls costs aborts x tail evaluations.  Capped -- and
         ;; what the cap dropped is COUNTED, because a silent truncation
         ;; would make the run look like it covered cases it never reached,
         ;; which is the same dishonesty the resume exists to remove.
         ((>= resumes nelisp-parity-fuzz--max-resumes)
          (setq out (append out (cl-subseq rows 0 (1+ idx))))
          (setq nelisp-parity-fuzz--dropped
                (+ nelisp-parity-fuzz--dropped (- (length todo) (1+ idx))))
          (setq todo nil))
         (t
          ;; Keep everything up to and including the aborting case, then
          ;; resume after it.  `todo' always shrinks by at least one.
          (setq resumes (1+ resumes))
          (setq out (append out (cl-subseq rows 0 (1+ idx))))
          (setq todo (nthcdr (1+ idx) todo))))))
    out))

(defconst nelisp-parity-fuzz--side-timeout
  (string-to-number (or (getenv "NELISP_FUZZ_SIDE_TIMEOUT") "8"))
  "Seconds one side may take for one chunk before it is killed.
A normal chunk takes 1-2 s.  A call that waits for terminal input never
returns: `read-char' and the commands that read events block in batch Emacs
even with stdin at EOF.  Killing the process leaves that case and the rest of
the chunk unreported, and `nelisp-parity-fuzz--run-recovering' resumes after
it.")

(defvar nelisp-parity-fuzz--gnu-timeouts nil
  "Cases that GNU Emacs cannot finish even when run alone (confirmed hangs).
Each is a ratcheted finding: its name must be in the known-hang list.")

(defconst nelisp-parity-fuzz--confirm-timeout
  (string-to-number (or (getenv "NELISP_FUZZ_CONFIRM_TIMEOUT") "20"))
  "Seconds a case gets when re-run alone to decide whether it hangs.")

(defun nelisp-parity-fuzz--probe (case timeout)
  "Run CASE alone with TIMEOUT seconds per side.
Return its row, or nil after recording CASE as a hang when GNU Emacs did not
answer.  Only a clean answer from both sides yields a row."
  (let* ((nelisp-parity-fuzz--side-timeout timeout)
         (row (car (nelisp-parity-fuzz--run (list case)))))
    (if (and row (eq (nth 1 row) :missing))
        (progn (push case nelisp-parity-fuzz--gnu-timeouts) nil)
      row)))

(defun nelisp-parity-fuzz--capture (argv)
  "Run ARGV, return an alist of CASE-TEXT -> RESULT-TEXT from its ## lines.
ARGV runs under timeout(1) with `nelisp-parity-fuzz--side-timeout' when the
command exists, and with stdin from /dev/null."
  (with-temp-buffer
    (let* ((default-directory (nelisp-parity-fuzz--scratch-dir))
           ;; HOME and TMPDIR point into the emptied scratch directory so a
           ;; call such as `project-remember-project' cannot leave state
           ;; (~/.emacs.d/projects, /tmp/rp-*) that another name then sees.
           (process-environment
            (append (list (concat "HOME=" default-directory)
                          (concat "TMPDIR=" default-directory))
                    process-environment))
           (timeout (executable-find "timeout")))
      (if (and timeout (> nelisp-parity-fuzz--side-timeout 0))
          (apply #'call-process timeout null-device t nil "--signal=KILL"
                 (number-to-string nelisp-parity-fuzz--side-timeout) argv)
        (apply #'call-process (car argv) null-device t nil (cdr argv))))
    (goto-char (point-min))
    (let ((rows nil))
      ;; Only a line ending in a newline is a result: a case that never
      ;; returned (killed by the side timeout) leaves "##KEY\t" without one,
      ;; and counting it would name the NEXT case as the one that hung.
      (while (re-search-forward "^##\\(.*?\\)\t\\(.*\\)\n" nil t)
        (push (cons (match-string 1) (match-string 2)) rows))
      (nreverse rows))))

(defun nelisp-parity-fuzz--aborts (cases)
  "Return the subset of CASES that KILL the standalone, one process each.

A call that ends the process cannot be caught by `condition-case', so it is
worse than a wrong answer: nothing downstream can defend against it.
(symbol-value \='unbound) was one of these until today, and the standalone\='s
`nl-jit-call-out-1\=' still is.

Candidates come from a chunked run -- a case the standalone never reported
while Emacs did means the process stopped at or before it -- and each
candidate is then re-run ALONE, because in a chunk the true culprit is the
first such case and everything after it is collateral."
  (let ((confirmed nil))
    (dolist (c cases)
      (let* ((rows (nelisp-parity-fuzz--run (list c)))
             (row (car rows)))
        (when (and row
                   (not (eq (nth 1 row) :missing))
                   (or (eq (nth 2 row) :missing)
                       (and (stringp (nth 2 row))
                            (string-match-p "aborted without signal" (nth 2 row)))))
          (push (cons c (nth 1 row)) confirmed))))
    (nreverse confirmed)))

(defun nelisp-parity-fuzz--zip (cases emacs-out nelisp-out)
  "Pair CASES with both sides' answers, keyed on the printed case text."
  (let ((out nil))
    (dolist (c cases)
      (let* ((key (nelisp-parity-fuzz--key c))
             (e (assoc key emacs-out))
             (n (assoc key nelisp-out)))
        ;; A case only one side reported is itself a finding: the standalone
        ;; stops at an uncatchable abort, and the missing tail is how that
        ;; shows.  Reported as such rather than skipped.
        (push (list c (if e (cdr e) :missing) (if n (cdr n) :missing)) out)))
    (nreverse out)))

;; ---------------------------------------------------------------------------
;; Shrinking.  A failing call is reduced by replacing each argument, one at a
;; time, with a simpler one from the pool, keeping any replacement that still
;; disagrees.  This is what turns "some call with six arguments differs" into
;; a line somebody can act on.
;; ---------------------------------------------------------------------------

(defconst nelisp-parity-fuzz--simple '(nil 0 "" '() t 1 "a")
  "Candidates tried when shrinking, simplest first.")

(defun nelisp-parity-fuzz--shrink (case)
  "Return the smallest still-failing variant of CASE.

Every candidate for one round is evaluated in a SINGLE pair of subprocesses.
Testing them one at a time costs two process spawns per candidate -- for a
four-argument call that is fifty-odd spawns per round, and the shrinker
ended up slower than the search that produced the case."
  (let ((cur case) (improved t) (rounds 0))
    (while (and improved (< rounds 4))
      (setq improved nil)
      (setq rounds (1+ rounds))
      (let ((variants nil))
        (cl-loop for i from 1 below (length cur) do
                 (cl-loop for simple in nelisp-parity-fuzz--simple do
                          (let ((try (copy-sequence cur)))
                            (setf (nth i try) simple)
                            (unless (equal try cur) (push try variants)))))
        (setq variants (nreverse (delete-dups variants)))
        (when variants
          (let* ((rows (nelisp-parity-fuzz--run variants))
                 (still (cl-find-if (lambda (r) (not (equal (nth 1 r) (nth 2 r)))) rows)))
            (when still
              (setq cur (nth 0 still))
              (setq improved t))))))
    cur))

;; ---------------------------------------------------------------------------
;; Entry point
;; ---------------------------------------------------------------------------

;; Known-divergence list.  One bare function name per line; `#' lines are
;; comments.  Generated from a real run (NELISP_FUZZ_WRITE_BASELINE=1 make
;; parity-fuzz, seed 1 / 1500 cases, the CI configuration).  Ratchet: a
;; divergent name missing from the list fails the gate, and a listed name
;; that was exercised and now agrees fails it too, so the list only shrinks.
;; A listed name the sample did not draw is neither new nor stale.
(defvar nelisp-parity-fuzz--known-file
  (or (getenv "NELISP_FUZZ_KNOWN_FILE")
      (expand-file-name "tools/parity-fuzz-known-divergences.txt"
                        nelisp-parity-fuzz--root)))

(defvar nelisp-parity-fuzz--ratchet-failed nil)
(defvar nelisp-parity-fuzz--hang-fail nil)
(defvar nelisp-parity-fuzz--hang-count 0)

(defun nelisp-parity-fuzz--dump-results (run-names gap-names notcall-names by-name
                                                   rows divergent exercised hang-names)
  "When NELISP_FUZZ_RESULTS names a file, write one \"NAME STATUS\" line per name."
  (let ((file (getenv "NELISP_FUZZ_RESULTS")))
    (when (and file (not (string-empty-p file)))
      (let ((lines nil))
        (dolist (n run-names)
          (push (cons n (cond ((memq n hang-names) "hang")
                              ((null (cdr (assq n by-name))) "no-call")
                              ((memq n divergent) "divergent")
                              ((memq n exercised) "agree")
                              (t "unexercised")))
                lines))
        (dolist (n gap-names) (push (cons n "oracle-gap") lines))
        (dolist (n notcall-names) (push (cons n "not-callable") lines))
        (ignore rows)
        (with-temp-file file
          (dolist (l (sort lines (lambda (a b) (string< (symbol-name (car a))
                                                         (symbol-name (car b))))))
            (insert (format "%s %s\n" (car l) (cdr l)))))))))

(defvar nelisp-parity-fuzz--hang-file
  (or (getenv "NELISP_FUZZ_HANG_FILE")
      (expand-file-name "tools/parity-fuzz-known-hangs.txt"
                        nelisp-parity-fuzz--root)))

(defun nelisp-parity-fuzz--read-known (&optional file)
  "Return the symbols listed in FILE (default: the known-divergence file)."
  (let ((out nil) (file (or file nelisp-parity-fuzz--known-file)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (dolist (l (split-string (buffer-string) "\n" t))
          (setq l (string-trim l))
          (unless (or (string-empty-p l) (string-prefix-p "#" l))
            (push (intern l) out)))))
    (nreverse out)))

(defun nelisp-parity-fuzz--write-hangs (names)
  "Write NAMES, sorted, to the known-hang file under its header."
  (with-temp-file nelisp-parity-fuzz--hang-file
    (insert "# Names with a generated call GNU Emacs 31.1 cannot finish in batch mode\n"
            "# (it waits for input), confirmed by re-running the call alone.\n"
            "# Regenerate: NELISP_FUZZ_WRITE_BASELINE=1 make parity-fuzz\n"
            "#\n"
            "# Ratchet (tools/nelisp-parity-fuzz.el): an unlisted confirmed hang FAILS\n"
            "# (new hang); a listed name none of whose calls hangs FAILS (stale).  The\n"
            "# calls of a listed name are run alone with a short timeout, so they\n"
            "# never stall a chunk.\n")
    (dolist (n names) (insert (symbol-name n) "\n"))))

(defun nelisp-parity-fuzz--write-known (names)
  "Write NAMES, sorted, to the known-divergence file under its header."
  (with-temp-file nelisp-parity-fuzz--known-file
    (insert "# Known divergences of the standalone runtime from GNU Emacs 31.1.\n"
            "#\n"
            "# Origin: fd4c2e282 (2026-11-09) put interactive commands back into the\n"
            "# parity-fuzz name pool; most are editor commands this runtime does not\n"
            "# define (void-function), plus type-representation and value differences.\n"
            "# Generated by the deterministic per-name scheme (seed 1, 3 cases per name,\n"
            "# every name of the compat table; see tools/nelisp-parity-fuzz.el):\n"
            "#   NELISP_FUZZ_WRITE_BASELINE=1 make parity-fuzz\n"
            "#\n"
            "# Ratchet rule (tools/nelisp-parity-fuzz.el): the gate FAILS on a divergent\n"
            "# name that is not listed here (new divergence) and FAILS on a listed name\n"
            "# that was exercised and now agrees (stale entry: delete the line).  The\n"
            "# list therefore only shrinks.  Never add a name to hide a regression;\n"
            "# fix the divergence or justify it in the commit.  A listed name the\n"
            "# name the host Emacs lacks (oracle gap) or whose calls all abort is neither\n"
            "# new nor stale.\n")
    (dolist (n names) (insert (symbol-name n) "\n"))))

(defconst nelisp-parity-fuzz--max-oracle-gaps 800
  "Ceiling on names the host Emacs lacks.
Oracle gaps are a property of the host build (tree-sitter, sqlite, GUI
toolkits), so listing them would reintroduce host dependence; they are
reported every run instead.  The ceiling only catches a host that is
grossly the wrong Emacs (a GNU Emacs 31.1 build lacks fewer than this).")

(defun nelisp-parity-fuzz-run ()
  (let* ((seed (string-to-number (or (getenv "NELISP_FUZZ_SEED") "1")))
         (per-name (string-to-number (or (getenv "NELISP_FUZZ_PER_NAME") "3")))
         (shrink-cap (string-to-number (or (getenv "NELISP_FUZZ_SHRINK") "12")))
         (probe-timeout (string-to-number
                         (or (getenv "NELISP_FUZZ_HANG_PROBE_TIMEOUT") "5")))
         (names (nelisp-parity-fuzz--names))
         (kinds (mapcar (lambda (n) (cons n (nelisp-parity-fuzz--host-kind n))) names))
         (gap-names (mapcar #'car (cl-remove-if-not (lambda (k) (eq (cdr k) 'gap)) kinds)))
         (notcall-names (mapcar #'car (cl-remove-if-not
                                       (lambda (k) (eq (cdr k) 'not-callable)) kinds)))
         (run-names (mapcar #'car (cl-remove-if-not (lambda (k) (eq (cdr k) 'ok)) kinds)))
         (known-hangs (nelisp-parity-fuzz--read-known nelisp-parity-fuzz--hang-file))
         (by-name nil)
         (cases nil) (hang-cases nil) (hang-rows nil))
    (unless (file-executable-p nelisp-parity-fuzz--binary)
      (princ (format "parity-fuzz: FAIL (no standalone binary at %s -- run `make standalone-reader')\n"
                     nelisp-parity-fuzz--binary))
      (kill-emacs 1))
    (when (null names)
      (princ "parity-fuzz: FAIL (no shared names selected -- is docs/emacs-compat-table.txt present?)\n")
      (kill-emacs 1))
    (princ (format "parity-fuzz: seed=%d per-name=%d names=%d oracle-gaps=%d not-callable=%d\n"
                   seed per-name (length names) (length gap-names) (length notcall-names)))
    (setq nelisp-parity-fuzz--no-arity nil)
    (dolist (n run-names)
      (let ((cs (nelisp-parity-fuzz--cases-for n seed per-name)))
        (push (cons n cs) by-name)
        (if (memq n known-hangs)
            (setq hang-cases (append hang-cases cs))
          (setq cases (append cases cs)))))
    (setq by-name (nreverse by-name))
    (let ((rows nil) (chunk 100))
      ;; Calls of a known-hang name run alone with a short timeout so they
      ;; never stall a chunk; the ones that do answer are still compared.
      (dolist (c hang-cases)
        (let ((r (nelisp-parity-fuzz--probe c probe-timeout)))
          (when r (push r hang-rows))))
      (setq rows (nreverse hang-rows))
      ;; ONE process pair per name: state a call leaves behind (buffer-local
      ;; variables, overlays, syntax tables) can then only affect the same
      ;; name's own few calls, never another name's, so a result does not
      ;; depend on which other names run, their order, shards, or where a
      ;; stall forced a restart.
      (ignore chunk)
      (dolist (n run-names)
        (unless (memq n known-hangs)
          (let ((cs (cdr (assq n by-name))))
            (when cs
              (setq rows (append rows (nelisp-parity-fuzz--run-recovering cs)))))))
      (let* ((suspects (mapcar (lambda (r) (nth 0 r))
                               (cl-remove-if-not
                                (lambda (r)
                                  (or (and (eq (nth 2 r) :missing)
                                           (not (eq (nth 1 r) :missing)))
                                      (and (stringp (nth 2 r))
                                           (string-match-p "aborted without signal"
                                                           (nth 2 r)))))
                                rows)))
             (aborts (and suspects (nelisp-parity-fuzz--aborts suspects))))
        (princ (format "\nparity-fuzz: %d call(s) GNU Emacs cannot finish even alone (hangs; not compared, ratcheted by name)\n"
                       (length nelisp-parity-fuzz--gnu-timeouts)))
        (dolist (c (reverse nelisp-parity-fuzz--gnu-timeouts))
          (princ (format "    %S\n" c)))
        (princ (format "\nparity-fuzz: %d call(s) END THE PROCESS (uncatchable -- condition-case cannot see them)\n"
                       (length aborts)))
        (dolist (a aborts)
          (princ (format "    %S\n      emacs %s\n" (car a) (cdr a)))))
      (let* ((differing (cl-remove-if (lambda (r)
                                        (or (equal (nth 1 r) (nth 2 r))
                                            (eq (nth 2 r) :missing)
                                            (and (stringp (nth 2 r))
                                                 (string-match-p "aborted without signal"
                                                                 (nth 2 r)))))
                                      rows))
             (gaps (cl-remove-if-not
                    (lambda (r) (nelisp-parity-fuzz--gap
                                 (car (nth 0 r)) (nth 1 r) (nth 2 r)))
                    differing))
             (bad (cl-remove-if
                   (lambda (r) (nelisp-parity-fuzz--gap
                                (car (nth 0 r)) (nth 1 r) (nth 2 r)))
                   differing))
             (families (delete-dups (mapcar (lambda (r) (car (nth 0 r))) bad))))
        (when gaps
          (princ (format "\nparity-fuzz: %d case(s) need an Emacs TYPE this runtime does not have.\nThese are NOT counted below -- they are not wrong answers, they are absent types:\n"
                         (length gaps)))
          (dolist (reason (sort (delete-dups
                                 (mapcar (lambda (r)
                                           (nelisp-parity-fuzz--gap
                                            (car (nth 0 r)) (nth 1 r) (nth 2 r)))
                                         gaps))
                                #'string<))
            (princ (format "    %s\n" reason)))
          (dolist (r gaps)
            (princ (format "      %S\n        emacs %s\n        nelisp %s\n"
                           (nth 0 r) (nth 1 r) (nth 2 r)))))
        ;; Known-divergence ratchet (see tools/parity-fuzz-known-divergences.txt).
        ;; `findings' in GATE-COUNT is new + stale, so the gate fails on a
        ;; divergence nobody has listed and on a listed one that is gone.
        (let* ((divergent (sort (copy-sequence families)
                                (lambda (a b) (string< (symbol-name a) (symbol-name b)))))
               (exercised (delete-dups
                           (mapcar (lambda (r) (car (nth 0 r)))
                                   (cl-remove-if
                                    (lambda (r)
                                      (or (eq (nth 2 r) :missing)
                                          (and (stringp (nth 2 r))
                                               (string-match-p "aborted without signal"
                                                               (nth 2 r)))))
                                    rows))))
               (known (nelisp-parity-fuzz--read-known))
               (new (cl-remove-if (lambda (n) (memq n known)) divergent))
               (stale (cl-remove-if-not
                       (lambda (n) (and (memq n exercised) (not (memq n divergent))))
                       known)))
          (let* ((hang-names (delete-dups (mapcar #'car nelisp-parity-fuzz--gnu-timeouts)))
                 (new-hang (cl-remove-if (lambda (n) (memq n known-hangs)) hang-names))
                 (stale-hang (cl-remove-if-not
                              (lambda (n) (and (memq n run-names) (not (memq n hang-names))))
                              known-hangs))
                 (gap-over (> (length gap-names) nelisp-parity-fuzz--max-oracle-gaps)))
            (nelisp-parity-fuzz--dump-results
             run-names gap-names notcall-names by-name rows divergent exercised hang-names)
            (princ (format "\nparity-fuzz: %d oracle gap(s): name(s) this host Emacs lacks, not exercised (ceiling %d): %s\n"
                           (length gap-names) nelisp-parity-fuzz--max-oracle-gaps
                           (mapconcat #'symbol-name
                                      (cl-subseq gap-names 0 (min 12 (length gap-names))) " ")))
            (when (getenv "NELISP_FUZZ_WRITE_BASELINE")
              (nelisp-parity-fuzz--write-hangs
               (sort (cl-remove-duplicates
                      (append hang-names
                              (cl-remove-if (lambda (n) (memq n run-names)) known-hangs)))
                     #'string<))
              (setq new-hang nil stale-hang nil))
            (dolist (n new-hang)
              (princ (format "parity-fuzz: FAIL new hang: %s (not in %s)\n" n
                             (file-name-nondirectory nelisp-parity-fuzz--hang-file))))
            (dolist (n stale-hang)
              (princ (format "parity-fuzz: FAIL stale hang entry: %s no longer hangs; delete it from %s\n" n
                             (file-name-nondirectory nelisp-parity-fuzz--hang-file))))
            (when gap-over
              (princ (format "parity-fuzz: FAIL oracle gaps %d exceed the ceiling %d (wrong host Emacs?)\n"
                             (length gap-names) nelisp-parity-fuzz--max-oracle-gaps)))
            (unless (zerop nelisp-parity-fuzz--dropped)
              (princ (format "parity-fuzz: FAIL %d case(s) NOT RUN (resume cap)\n"
                             nelisp-parity-fuzz--dropped)))
            (setq nelisp-parity-fuzz--hang-fail
                  (or new-hang stale-hang gap-over
                      (not (zerop nelisp-parity-fuzz--dropped))))
            (setq nelisp-parity-fuzz--hang-count (length hang-names)))
          (when (getenv "NELISP_FUZZ_WRITE_BASELINE")
            (nelisp-parity-fuzz--write-known divergent)
            (princ (format "parity-fuzz: wrote %d name(s) to %s\n" (length divergent)
                           nelisp-parity-fuzz--known-file))
            (setq new nil stale nil known divergent))
          (dolist (n new)
            (princ (format "parity-fuzz: FAIL new divergence: %s (not in %s)\n" n
                           (file-name-nondirectory nelisp-parity-fuzz--known-file))))
          (dolist (n stale)
            (princ (format "parity-fuzz: FAIL stale entry: %s no longer diverges; delete it from %s\n" n
                           (file-name-nondirectory nelisp-parity-fuzz--known-file))))
          (princ (format "GATE-COUNT checked=%d findings=%d known=%d new=%d stale=%d hangs=%d oracle_gaps=%d\n"
                         (length rows) (+ (length new) (length stale))
                         (length known) (length new) (length stale)
                         nelisp-parity-fuzz--hang-count (length gap-names)))
          (setq nelisp-parity-fuzz--ratchet-failed
                (or new stale nelisp-parity-fuzz--hang-fail)))
        (let ((skipped (delete-dups (mapcar #'car nelisp-parity-fuzz--no-arity))))
          (unless (null skipped)
            (princ (format "parity-fuzz: %d name(s) generated no call (arity unreadable): %s\n"
                           (length skipped)
                           (mapconcat #'symbol-name
                                      (cl-subseq skipped 0 (min 10 (length skipped)))
                                      " ")))))
        (unless (zerop nelisp-parity-fuzz--dropped)
          (princ (format "parity-fuzz: %d case(s) NOT RUN -- a chunk hit the resume cap after repeated aborts\n"
                         nelisp-parity-fuzz--dropped)))
        (princ (format "parity-fuzz: %d/%d cases disagree, across %d name(s)\n"
                       (length bad) (length rows) (length families)))
        ;; The full list, ranked by how often the name came up.  Without it
        ;; the report shows whichever dozen the shrink cap reached, and work
        ;; gets prioritised by that accident rather than by frequency.
        (let ((counts nil))
          (dolist (r bad)
            (let* ((fam (car (nth 0 r))) (cell (assq fam counts)))
              (if cell (setcdr cell (1+ (cdr cell))) (push (cons fam 1) counts))))
          (princ "\n  divergent names, most cases first:\n")
          (let ((line "   ") (n 0))
            (dolist (c (sort counts (lambda (a b) (> (cdr a) (cdr b)))))
              (setq line (format "%s %s(%d)" line (car c) (cdr c)))
              (setq n (1+ n))
              (when (> (length line) 92) (princ (concat line "\n")) (setq line "   ")))
            (unless (string= line "   ") (princ (concat line "\n")))))
        ;; NELISP_FUZZ_RAW dumps every disagreement unshrunk.  Shrinking costs
        ;; two subprocesses per candidate and answers "what is the smallest
        ;; call that still differs"; when the question is instead "what is
        ;; the SHAPE of what is left", the raw list answers it for free.
        (when (getenv "NELISP_FUZZ_RAW")
          (princ "\n  every disagreement, unshrunk:\n")
          (dolist (r bad)
            (princ (format "    %s\n      emacs   %s\n      nelisp  %s\n"
                           (nelisp-parity-fuzz--key (nth 0 r)) (nth 1 r) (nth 2 r)))))
        ;; One shrunk representative per NAME: twenty reports of the same
        ;; defect read as twenty defects, and the first thing anybody does
        ;; with them is group by name anyway.
        (let ((shown 0))
          (dolist (fam families)
            (when (< shown shrink-cap)
              (setq shown (1+ shown))
              (let* ((rep (cl-find-if (lambda (r) (eq (car (nth 0 r)) fam)) bad))
                     (small (nelisp-parity-fuzz--shrink (nth 0 rep)))
                     (row (car (nelisp-parity-fuzz--run (list small)))))
                (princ (format "\n  %S\n    emacs   %s\n    nelisp  %s\n"
                               small (nth 1 row) (nth 2 row))))))
          (when (> (length families) shrink-cap)
            (princ (format "\n  ... and %d more name(s) not shrunk (NELISP_FUZZ_SHRINK to raise)\n"
                           (- (length families) shrink-cap)))))
        (cond
         (nelisp-parity-fuzz--ratchet-failed
          (princ "parity-fuzz: FAIL (divergence set differs from the known-divergence list)\n")
          (kill-emacs 1))
         (bad
          (princ "parity-fuzz: PASS (every divergent name is in the known-divergence list)\n"))
         (t
          (princ "parity-fuzz: PASS (no disagreement in this budget)\n")))))))

;; Loadable as a library when NELISP_FUZZ_NORUN is set, so the generator and
;; the runner can be exercised from another batch session without kicking off
;; a whole sweep -- which is how the 13-of-60 truncation below was diagnosed.
(unless (getenv "NELISP_FUZZ_NORUN")
  (nelisp-parity-fuzz-run))

;;; nelisp-parity-fuzz.el ends here
