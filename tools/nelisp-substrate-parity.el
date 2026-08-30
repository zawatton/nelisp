;;; nelisp-substrate-parity.el --- one probe corpus, every entry point, diffed  -*- lexical-binding: t; -*-

;;; Commentary:

;; This branch's costliest misdiagnosis: a primitive (`emacs-pid' /
;; `random') was probed in ONE substrate (`eval-elisp-source') and the
;; answer was generalized to ANOTHER (the compiled-artifact command
;; runtime), producing a wrong root-cause claim that had to be reverted.
;; Separately, nil/t canonicality diverged BETWEEN paths of the same
;; binary (`read-from-string' canonical; `intern'/`intern-soft' were not
;; -- fixed at the native level, commit 342d098cc).  Nothing else in the
;; gate set runs one probe corpus across every substrate and diffs; this
;; does.
;;
;; Corpus: `tools/nelisp-substrate-parity-corpus.el', a fixed list of
;; (INDEX TAG FORM).  Every FORM evaluates to 0 or 1 -- nondeterministic
;; primitives are wrapped in a normalized property check, never compared
;; by raw value, which is the exact shape of the regression above.
;;
;; Substrates, run through the SAME corpus (skipped gracefully, with a
;; line saying so, when the binary in front of this script lacks one):
;;   a. bare-file     target/nelisp FILE                    -- baseline
;;   b. --load        target/nelisp --load FILE
;;   c. runtime-image dump-runtime-image + eval-runtime-image
;;   d. artifact      compile-elisp-artifact --kind neln + exec-elisp-artifact
;;   e. source-cache / source-fallback
;;                    eval-elisp-source, with/without the artifact
;;                    runtime cache-enable marker (the same pair
;;                    `nelisp-performance-gate' exercises)
;;   f. host Emacs    emacs -Q --batch -l FILE               -- shared-tag only
;;
;; Substrate (a) is the baseline; every other substrate diffs against it,
;; line by line, form index by form index.  (f) diffs only the `shared'
;; part of the corpus -- `standalone-only' forms probe a NeLisp-internal
;; name or a behavior host Emacs has no reason to share.
;;
;; Every corpus form is run in ITS OWN process for EVERY substrate.  Not
;; an optimization skipped: the bare-file substrate does not have
;; `princ', `prin1', `intern-soft' or `read-from-string' bound (see form
;; 2/3/24/25/27/28 in the corpus and the ADDENDA known divergence below)
;; -- one file holding all N forms would abort at the first missing
;; function under that substrate and silently lose every later line's
;; result.  Per-form isolation is what lets a crash on form 5 still leave
;; forms 0-4 and 6-N reported.
;;
;; A crash (nonzero exit, no matching "IDX: 0/1" line in stdout) is
;; itself a valid, comparable result: "CRASH:<classification>" is a
;; string like any other, and a substrate whose form 27 crashes while the
;; baseline's does not is exactly the divergence this gate exists to
;; name.
;;
;; Accepted divergences live in `tools/substrate-parity-accepted.el',
;; shaped like `scripts/nl-ns-accepted.el': a key is
;; "INDEX\tSUBSTRATE\tBASELINE\tACTUAL" -- both sides of the divergence,
;; not just its existence -- so if either side's value changes the old
;; key stops matching, the acceptance goes stale, and the new shape comes
;; back as an unaccepted finding instead of being silently covered by an
;; entry that no longer describes the tree.  Regenerate from a reviewed
;; tree with:
;;
;;   NELISP_SUBSTRATE_PARITY_WRITE=1 emacs -Q --batch -L lisp -L src \
;;     -L scripts -l tools/nelisp-substrate-parity.el
;;
;; Usage (what `make substrate-parity-smoke' runs):
;;
;;   emacs -Q --batch -L lisp -L src -L scripts \
;;     -l tools/nelisp-substrate-parity.el -f nelisp-substrate-parity-gate-run

;;; Code:

(require 'cl-lib)

(defvar nelisp-substrate-parity--repo-root
  (let ((here (expand-file-name (or load-file-name buffer-file-name default-directory))))
    (file-name-as-directory (expand-file-name ".." (file-name-directory here))))
  "Repository root, derived from this file's own location under tools/.")

(defun nelisp-substrate-parity--path (relative)
  "Resolve RELATIVE against the repository root."
  (expand-file-name relative nelisp-substrate-parity--repo-root))

(defvar nelisp-substrate-parity--corpus-file
  (nelisp-substrate-parity--path "tools/nelisp-substrate-parity-corpus.el"))

(defvar nelisp-substrate-parity--accepted-file
  (nelisp-substrate-parity--path "tools/substrate-parity-accepted.el"))

;; Task A (presence sweep): a second corpus, its own ledger, its own entry
;; point (`nelisp-substrate-parity-presence-gate-run' below).  One `fboundp'
;; probe per name in the definable-name surface -- ~354 names, generated
;; from the source by `tools/nelisp-substrate-presence-gen.el' -- versus the
;; 39 hand-written behavioral forms above.  Kept as a separate file/ledger on
;; purpose, per that generator's own commentary: a presence gap must never
;; conflate with a behavioral one in a single finding count.
(defvar nelisp-substrate-parity--presence-corpus-file
  (nelisp-substrate-parity--path "tools/nelisp-substrate-presence-corpus.el"))

(defvar nelisp-substrate-parity--presence-accepted-file
  (nelisp-substrate-parity--path "tools/substrate-presence-accepted.el"))

(defconst nelisp-substrate-parity--presence-host-preamble
  "(ignore-errors (require 'cl-lib))\n(ignore-errors (require 'seq))\n(ignore-errors (require 'subr-x))\n(ignore-errors (require 'url-parse))\n"
  "Preloaded before the presence sweep's host-Emacs shim only.
`cl-*' names and `url-parse''s `cl-defstruct' accessors (url-host/url-port/
url-filename/url-type) are real Emacs library functions gated behind an
explicit `require', not missing primitives -- `nelisp--install-primitives'
already documents assuming \"a real-Emacs host\" for these entries (see
`src/nelisp-eval.el').  Without this, a bare `emacs -Q --batch' host would
report every `cl-*'/`url-host'-family name as a false presence gap instead
of the true one this sweep exists to find.  Every NeLisp substrate already
provides `cl-lib' unconditionally (`scripts/nelisp-stdlib-prelude.el' ends
with `(provide (quote cl-lib))'), so this preamble changes only what HOST
starts with, never what a NeLisp substrate does -- the 39-form behavioral
corpus's host runs stay untouched (nil preamble, see
`nelisp-substrate-parity-gate-run').")

(defvar nelisp-substrate-parity--marker-file
  (nelisp-substrate-parity--path "target/nelisp-artifact-runtime.el.nelc.enable")
  "The artifact runtime cache enable marker `nelisp-performance-gate' also
toggles to exercise the source-cache vs source-fallback pair.")

(defun nelisp-substrate-parity--nelisp-bin ()
  "Path to the standalone binary under test."
  (or (getenv "NELISP_BIN")
      (nelisp-substrate-parity--path "target/nelisp")))

(defvar nelisp-substrate-parity--emacs (or (getenv "EMACS") "emacs"))

;; Loaded once; the corpus is pure data (see its own file header).  Declared
;; here (not `require'd -- the corpus file is loaded by absolute path, not
;; through `load-path') so byte-compiling this file does not warn about a
;; free variable for every reference below.
(defvar nelisp-substrate-parity-corpus)
(load nelisp-substrate-parity--corpus-file nil t)

(defvar nelisp-substrate-presence-corpus)
(load nelisp-substrate-parity--presence-corpus-file nil t)

(defconst nelisp-substrate-parity--shim-src
  "(defun probe-emit (idx val)
  (if (fboundp 'nelisp--write-stdout-bytes)
      (nelisp--write-stdout-bytes (format \"%d: %S\\n\" idx val))
    (princ (format \"%d: %S\\n\" idx val))))
"
  "Printing shim, embeddable in any substrate.
`nelisp--write-stdout-bytes' is the one output primitive present even in
the bare-file substrate's minimal boot (measured: `princ', `prin1' and
`message' are all void-function there).  Host Emacs and the richer
NeLisp substrates fall back to `princ'.")

;; ---------------------------------------------------------------------
;; Process plumbing
;; ---------------------------------------------------------------------

(defun nelisp-substrate-parity--call (program args cwd)
  "Run PROGRAM with ARGS in CWD.  Return (EXIT STDOUT STDERR).
CWD is always a scratch directory, never the repository -- a probe that
writes its own artifacts into the tree is not a probe."
  (let* ((default-directory (file-name-as-directory cwd))
         (err-file (make-temp-file "nlsp-err-"))
         exit out err)
    (unwind-protect
        (progn
          (with-temp-buffer
            (setq exit (apply #'call-process program nil (list t err-file) nil args))
            (setq out (buffer-string)))
          (with-temp-buffer
            (insert-file-contents err-file)
            (setq err (buffer-string))))
      (ignore-errors (delete-file err-file)))
    (list exit out err)))

(defun nelisp-substrate-parity--probe-line (idx form)
  "One `probe-emit' call for corpus entry IDX/FORM, as source text."
  (format "(probe-emit %d %S)" idx form))

(defun nelisp-substrate-parity--classify (err exit)
  "Reduce ERR/EXIT to a short, machine-stable classification.
Strips anything path-shaped so the classification does not embed a
volatile temp-directory name -- otherwise every run would \"drift\" the
accepted-ledger keys even when nothing about the divergence changed.

A void-function condition is reported in at least three different
wordings measured across the CLI commands and host Emacs:
  \"nelisp: uncaught error: void-function: (X)\"          (bare/--load/image)
  \"nelisp direct artifact error: (void-function X)\"      (exec-elisp-artifact)
  \"...: symbol's function is void: X\"                    (eval-elisp-source)
  \"Symbol's function definition is void: X\"               (host Emacs)
All four are normalized to \"void-function:X\" here, so a primitive
missing identically everywhere reads as no finding -- a real
cross-substrate divergence should never hide inside a wording
difference, and a wording difference should never masquerade as one."
  (let* ((trimmed (string-trim (or err "")))
         (line (car (split-string trimmed "\n" t)))
         (sans-paths (and line (replace-regexp-in-string
                                 "/[[:graph:]]*/" "<path>/" line)))
         (case-fold-search t)
         (sym-re "\\([-A-Za-z0-9$*+=<>!?_.:]+\\)")
         (vf (and sans-paths
                 (or (and (string-match
                           (concat "void-function:? *(?" sym-re ")?") sans-paths)
                         (match-string 1 sans-paths))
                    (and (string-match
                          (concat "function\\(?: definition\\)? is void:? *" sym-re)
                          sans-paths)
                         (match-string 1 sans-paths))))))
    (cond
     (vf (format "void-function:%s" vf))
     ((and sans-paths (string-match "uncaught error: \\(.*\\)" sans-paths))
      (match-string 1 sans-paths))
     (sans-paths (substring sans-paths 0 (min (length sans-paths) 100)))
     (t (format "exit=%d no-output" exit)))))

(defun nelisp-substrate-parity--extract (idx exit out err)
  "Reduce one probe's raw (EXIT OUT ERR) to a comparable result string."
  (let ((re (format "^%d: \\([01]\\)$" idx)))
    (if (string-match re out)
        (match-string 1 out)
      (format "CRASH:%s" (nelisp-substrate-parity--classify err exit)))))

;; ---------------------------------------------------------------------
;; Substrate runners.  Each takes (CWD IDX FORM) and returns a result
;; string; setup functions below build whatever persistent state (image,
;; artifact, marker) a substrate needs once, outside the per-form loop.
;; ---------------------------------------------------------------------

(defun nelisp-substrate-parity--write-load-file (path idx form &optional preamble)
  (with-temp-file path
    (when preamble (insert preamble))
    (insert nelisp-substrate-parity--shim-src)
    (insert "\n")
    (insert (nelisp-substrate-parity--probe-line idx form))
    (insert "\n0\n")))

(defun nelisp-substrate-parity--run-bare (bin cwd idx form)
  (let ((file (expand-file-name (format "bare-%d.el" idx) cwd)))
    (nelisp-substrate-parity--write-load-file file idx form)
    (apply #'nelisp-substrate-parity--extract idx
           (nelisp-substrate-parity--call bin (list file) cwd))))

(defun nelisp-substrate-parity--run-load (bin cwd idx form)
  (let ((file (expand-file-name (format "load-%d.el" idx) cwd)))
    (nelisp-substrate-parity--write-load-file file idx form)
    (apply #'nelisp-substrate-parity--extract idx
           (nelisp-substrate-parity--call bin (list "--load" file) cwd))))

(defun nelisp-substrate-parity--run-host (cwd idx form &optional preamble)
  (let ((file (expand-file-name (format "host-%d.el" idx) cwd)))
    (nelisp-substrate-parity--write-load-file file idx form preamble)
    (apply #'nelisp-substrate-parity--extract idx
           (nelisp-substrate-parity--call nelisp-substrate-parity--emacs
                                          (list "-Q" "--batch" "-l" file) cwd))))

(defun nelisp-substrate-parity--setup-runtime-image (bin cwd)
  "Dump a runtime image with the shim loaded.  Return its path, or
\(:unavailable . REASON) if `dump-runtime-image' is not usable here."
  (let ((shim-file (expand-file-name "ri-shim.el" cwd))
        (img (expand-file-name "corpus.nlri" cwd)))
    (with-temp-file shim-file (insert nelisp-substrate-parity--shim-src))
    (cl-destructuring-bind (exit _out err)
        (nelisp-substrate-parity--call
         bin (list "dump-runtime-image" img "--load" shim-file) cwd)
      (if (and (= exit 0) (file-exists-p img))
          img
        (cons :unavailable (nelisp-substrate-parity--classify err exit))))))

(defun nelisp-substrate-parity--run-runtime-image (bin cwd img idx form)
  (apply #'nelisp-substrate-parity--extract idx
         (nelisp-substrate-parity--call
          bin (list "eval-runtime-image" img
                    (nelisp-substrate-parity--probe-line idx form))
          cwd)))

(defun nelisp-substrate-parity--setup-artifact (bin cwd)
  "Compile the shim as a .neln artifact.  Return its path, or
\(:unavailable . REASON)."
  (let ((src (expand-file-name "artifact-shim.el" cwd))
        (out (expand-file-name "artifact-shim.el.neln" cwd)))
    (with-temp-file src
      (insert nelisp-substrate-parity--shim-src)
      (insert "(provide 'substrate-parity-shim)\n"))
    (cl-destructuring-bind (exit _out err)
        (nelisp-substrate-parity--call
         bin (list "compile-elisp-artifact" "--kind" "neln"
                   "--input" src "--output" out)
         cwd)
      (if (and (= exit 0) (file-exists-p out))
          out
        (cons :unavailable (nelisp-substrate-parity--classify err exit))))))

(defun nelisp-substrate-parity--run-artifact (bin cwd art idx form)
  (apply #'nelisp-substrate-parity--extract idx
         (nelisp-substrate-parity--call
          bin (list "exec-elisp-artifact" art
                    (nelisp-substrate-parity--probe-line idx form))
          cwd)))

(defun nelisp-substrate-parity--setup-source (cwd)
  "Write the shim as a source module for `eval-elisp-source'.  Return its
path; this step cannot itself be unavailable, since it only writes a
file -- unavailability of `eval-elisp-source' shows up as a CRASH result
on the first form instead."
  (let ((src (expand-file-name "source-shim.el" cwd)))
    (with-temp-file src
      (insert nelisp-substrate-parity--shim-src)
      (insert "(provide 'substrate-parity-shim)\n"))
    src))

(defun nelisp-substrate-parity--run-source (bin cwd src idx form)
  (apply #'nelisp-substrate-parity--extract idx
         (nelisp-substrate-parity--call
          bin (list "eval-elisp-source" src
                    (nelisp-substrate-parity--probe-line idx form))
          cwd)))

;; ---------------------------------------------------------------------
;; The artifact runtime cache marker.  `nelisp-performance-gate' toggles
;; the same file the same way; reused here rather than reinvented.
;; ---------------------------------------------------------------------

(defun nelisp-substrate-parity--marker-backup ()
  (when (file-exists-p nelisp-substrate-parity--marker-file)
    (with-temp-buffer
      (insert-file-contents nelisp-substrate-parity--marker-file)
      (buffer-string))))

(defun nelisp-substrate-parity--marker-remove ()
  (when (file-exists-p nelisp-substrate-parity--marker-file)
    (delete-file nelisp-substrate-parity--marker-file)))

(defun nelisp-substrate-parity--marker-restore (backup)
  (if backup
      (with-temp-file nelisp-substrate-parity--marker-file (insert backup))
    (nelisp-substrate-parity--marker-remove)))

;; ---------------------------------------------------------------------
;; Orchestration
;; ---------------------------------------------------------------------

(defconst nelisp-substrate-parity--substrate-order
  '("load" "runtime-image" "artifact" "source-cache" "source-fallback" "host")
  "Non-baseline substrates, in report order.  \"bare\" is the baseline and
is run separately, first.")

(defun nelisp-substrate-parity--run-all (&optional corpus host-preamble)
  "Run CORPUS (default `nelisp-substrate-parity-corpus') across every
substrate.  HOST-PREAMBLE, when non-nil, is inserted before the shim in the
host-Emacs run only (see `nelisp-substrate-parity--presence-host-preamble').
Return a plist :results (SUBSTRATE . ((IDX . VALUE) ...)) :skips (list of
report lines) :checked (count of form/substrate comparisons performed)."
  (let* ((corpus (or corpus nelisp-substrate-parity-corpus))
         (bin (nelisp-substrate-parity--nelisp-bin))
         (cwd (make-temp-file "nelisp-substrate-parity-" t))
         (results nil)
         (skips nil)
         (checked 0))
    (unless (file-exists-p bin)
      (error "nelisp-substrate-parity: no binary at %s" bin))
    (unwind-protect
        (progn
          ;; -- baseline: bare-file --
          (let ((bare nil))
            (dolist (entry corpus)
              (cl-destructuring-bind (idx _tag form) entry
                (push (cons idx (nelisp-substrate-parity--run-bare bin cwd idx form))
                      bare)))
            (push (cons "bare" (nreverse bare)) results))

          ;; -- --load --
          (let ((load-res nil))
            (dolist (entry corpus)
              (cl-destructuring-bind (idx _tag form) entry
                (push (cons idx (nelisp-substrate-parity--run-load bin cwd idx form))
                      load-res)
                (setq checked (1+ checked))))
            (push (cons "load" (nreverse load-res)) results))

          ;; -- runtime-image --
          (let ((img (nelisp-substrate-parity--setup-runtime-image bin cwd)))
            (if (and (consp img) (eq (car img) :unavailable))
                (push (format "SUBSTRATE-SKIP name=runtime-image reason=%s" (cdr img)) skips)
              (let ((ri-res nil))
                (dolist (entry corpus)
                  (cl-destructuring-bind (idx _tag form) entry
                    (push (cons idx (nelisp-substrate-parity--run-runtime-image bin cwd img idx form))
                          ri-res)
                    (setq checked (1+ checked))))
                (push (cons "runtime-image" (nreverse ri-res)) results))))

          ;; -- compiled artifact --
          (let ((art (nelisp-substrate-parity--setup-artifact bin cwd)))
            (if (and (consp art) (eq (car art) :unavailable))
                (push (format "SUBSTRATE-SKIP name=artifact reason=%s" (cdr art)) skips)
              (let ((art-res nil))
                (dolist (entry corpus)
                  (cl-destructuring-bind (idx _tag form) entry
                    (push (cons idx (nelisp-substrate-parity--run-artifact bin cwd art idx form))
                          art-res)
                    (setq checked (1+ checked))))
                (push (cons "artifact" (nreverse art-res)) results))))

          ;; -- source: cache path, then fallback path --
          (let ((src (nelisp-substrate-parity--setup-source cwd))
                (backup (nelisp-substrate-parity--marker-backup)))
            (unwind-protect
                (progn
                  (unless (file-exists-p nelisp-substrate-parity--marker-file)
                    (push "SUBSTRATE-SKIP name=source-cache reason=no-runtime-cache-enable-marker-was-this-built-by-make-standalone-reader" skips))
                  (when (file-exists-p nelisp-substrate-parity--marker-file)
                    (let ((cache-res nil))
                      (dolist (entry corpus)
                        (cl-destructuring-bind (idx _tag form) entry
                          (push (cons idx (nelisp-substrate-parity--run-source bin cwd src idx form))
                                cache-res)
                          (setq checked (1+ checked))))
                      (push (cons "source-cache" (nreverse cache-res)) results)))
                  (nelisp-substrate-parity--marker-remove)
                  (let ((fallback-res nil))
                    (dolist (entry corpus)
                      (cl-destructuring-bind (idx _tag form) entry
                        (push (cons idx (nelisp-substrate-parity--run-source bin cwd src idx form))
                              fallback-res)
                        (setq checked (1+ checked))))
                    (push (cons "source-fallback" (nreverse fallback-res)) results)))
              ;; Restore exactly what was there -- this gate must not leave
              ;; the tree's own build state mutated when it exits.
              (nelisp-substrate-parity--marker-restore backup)))

          ;; -- host Emacs, shared-tag forms only --
          (let ((host-res nil))
            (dolist (entry corpus)
              (cl-destructuring-bind (idx tag form) entry
                (when (eq tag 'shared)
                  (push (cons idx (nelisp-substrate-parity--run-host cwd idx form host-preamble))
                        host-res)
                  (setq checked (1+ checked)))))
            (push (cons "host" (nreverse host-res)) results)))
      (ignore-errors (delete-directory cwd t)))
    (list :results (nreverse results) :skips (nreverse skips) :checked checked)))

(defun nelisp-substrate-parity--diff (run)
  "Diff every non-baseline substrate in RUN against \"bare\".
Return a list of findings, each (IDX SUBSTRATE BASELINE ACTUAL)."
  (let* ((results (plist-get run :results))
         (baseline (cdr (assoc "bare" results)))
         (findings nil))
    (dolist (substrate nelisp-substrate-parity--substrate-order)
      (let ((rows (cdr (assoc substrate results))))
        (when rows
          (dolist (row rows)
            (let* ((idx (car row))
                   (actual (cdr row))
                   (want (cdr (assoc idx baseline))))
              (unless (equal want actual)
                (push (list idx substrate want actual) findings)))))))
    (nreverse findings)))

;; ---------------------------------------------------------------------
;; Accepted-divergence ledger.  Same shape as `scripts/nl-ns-accepted.el'.
;; ---------------------------------------------------------------------

(defun nelisp-substrate-parity--finding-key (finding)
  (cl-destructuring-bind (idx substrate baseline actual) finding
    (format "%d\t%s\t%s\t%s" idx substrate baseline actual)))

(defun nelisp-substrate-parity--load-accepted (path)
  "Return (:generated-at .. :reason .. :notes ALIST :keys HASH-TABLE)."
  (if (not (file-exists-p path))
      (list :generated-at nil :reason nil :notes nil :keys (make-hash-table :test 'equal))
    (let* ((raw (with-temp-buffer
                  (insert-file-contents path)
                  (goto-char (point-min))
                  (read (current-buffer))))
           (keys (make-hash-table :test 'equal)))
      (dolist (k (plist-get raw :keys)) (puthash k t keys))
      (list :generated-at (plist-get raw :generated-at)
            :reason (plist-get raw :reason)
            :notes (plist-get raw :notes)
            :keys keys))))

(defun nelisp-substrate-parity--write-accepted (findings path generated-at reason notes &optional write-env-var)
  "Write FINDINGS as the accepted set at PATH.  NOTES (an alist of key ->
reason) is carried over from the file being replaced; a note whose key
is no longer present is dropped along with it -- same rule and same
reason as `nl-ns-write-accepted'.  WRITE-ENV-VAR names the env var that
regenerates THIS file (default NELISP_SUBSTRATE_PARITY_WRITE, the
behavioral corpus's) -- the presence ledger has its own,
NELISP_SUBSTRATE_PRESENCE_WRITE."
  (let* ((keys (sort (mapcar #'nelisp-substrate-parity--finding-key findings) #'string<))
         (kept nil))
    (dolist (key keys)
      (let ((note (cdr (assoc key notes))))
        (when note (push (cons key note) kept))))
    (setq kept (nreverse kept))
    (with-temp-buffer
      (insert ";; substrate-parity accepted divergences -- generated, review before commit.\n")
      (insert (format ";; Regenerate with %s=1 (see the make\n"
                       (or write-env-var "NELISP_SUBSTRATE_PARITY_WRITE")))
      (insert ";; target); do not hand-add keys to silence a finding.\n")
      (insert ";;\n")
      (insert ";; Each key is \"INDEX\\tSUBSTRATE\\tBASELINE\\tACTUAL\" -- both sides\n")
      (insert ";; of the divergence, not just its existence.  If either value\n")
      (insert ";; changes the key stops matching, the old entry goes stale, and\n")
      (insert ";; the new shape comes back as an unaccepted finding instead of\n")
      (insert ";; being silently covered (the ns-gate lesson).\n")
      (insert ";;\n")
      (insert ";; :notes is an alist of key -> why THIS entry is accepted, and it\n")
      (insert ";; survives regeneration.\n")
      (prin1 (list :generated-at generated-at :reason reason :notes kept :keys keys)
             (current-buffer))
      (insert "\n")
      (let ((coding-system-for-write 'utf-8-unix))
        (write-region (point-min) (point-max) path nil 'quiet)))
    (length keys)))

;; ---------------------------------------------------------------------
;; Entry points
;; ---------------------------------------------------------------------

(defun nelisp-substrate-parity--print-report (run findings new stale)
  (dolist (line (plist-get run :skips))
    (princ (format "%s\n" line)))
  (when findings
    (princ (format "\n%d total divergence(s) from the bare-file baseline:\n\n" (length findings)))
    (dolist (f findings)
      (cl-destructuring-bind (idx substrate baseline actual) f
        (princ (format "  form=%d substrate=%s baseline=%s actual=%s\n"
                       idx substrate baseline actual)))))
  (when new
    (princ (format "\n%d finding(s) NOT in the accepted set:\n\n" (length new)))
    (dolist (f new)
      (princ (format "  %s\n" (nelisp-substrate-parity--finding-key f)))))
  (when stale
    (princ (format "\n%d accepted entr%s no longer match anything (stale -- drop them):\n\n"
                   (length stale) (if (= (length stale) 1) "y" "ies")))
    (dolist (k stale)
      (princ (format "  %s\n" k)))))

(defun nelisp-substrate-parity--gate-run-1 (corpus accepted-file &optional host-preamble)
  "Shared body for both gate entry points below: run CORPUS across every
substrate, diff against baseline, compare to ACCEPTED-FILE's ledger, print
the GATE-COUNT line and report, return 0/1."
  (let* ((run (nelisp-substrate-parity--run-all corpus host-preamble))
         (findings (nelisp-substrate-parity--diff run))
         (accepted (nelisp-substrate-parity--load-accepted accepted-file))
         (accepted-keys (plist-get accepted :keys))
         (new (cl-remove-if (lambda (f) (gethash (nelisp-substrate-parity--finding-key f) accepted-keys))
                             findings))
         (current-keys (mapcar #'nelisp-substrate-parity--finding-key findings))
         (stale (cl-remove-if (lambda (k) (member k current-keys))
                               (let (ks) (maphash (lambda (k _v) (push k ks)) accepted-keys) ks)))
         (checked (plist-get run :checked))
         (problem-count (+ (length new) (length stale))))
    (princ (format "GATE-COUNT checked=%d findings=%d\n" checked problem-count))
    (nelisp-substrate-parity--print-report run findings new stale)
    (if (= problem-count 0)
        (progn (princ "\n[substrate-parity] PASS\n") 0)
      (progn (princ "\n[substrate-parity] FAIL\n") 1))))

(defun nelisp-substrate-parity-gate-run ()
  "Entry point for `make substrate-parity-smoke' (the 39-form behavioral corpus)."
  (nelisp-substrate-parity--gate-run-1
   nelisp-substrate-parity-corpus nelisp-substrate-parity--accepted-file))

(defun nelisp-substrate-parity-presence-gate-run ()
  "Entry point for `make substrate-presence-sweep' (Task A: one `fboundp'
probe per name in the definable-name surface, diffed the same way)."
  (nelisp-substrate-parity--gate-run-1
   nelisp-substrate-presence-corpus nelisp-substrate-parity--presence-accepted-file
   nelisp-substrate-parity--presence-host-preamble))

;; ---------------------------------------------------------------------
;; Auto-notes for first-time regeneration.  A hand-written note carried
;; over from the file being replaced always wins (see
;; `nelisp-substrate-parity--notes-for'); this only fires for a key seen
;; for the first time, so the initial ledger is self-documenting instead
;; of landing with 100+ silent, unreasoned keys nobody reviewed.
;; ---------------------------------------------------------------------

;; FIXED 2026-08-22 (scripts/nelisp-standalone-build.el, the `t' cond clause's
;; source-selection dispatcher for a bare positional FILE argument): unified
;; with `--load' by running the same `_cl'-gated `nelisp-standalone--reader-repl-prelude-forms'
;; call before reading the file, instead of falling straight to
;; `nl_os_read_file_cpath' with no prelude at all.  Bare-file was the only
;; entry point skipping it.  Kept below as two defensive classifiers -- both
;; unreachable against the current findings set (measured: `checked=209
;; findings=0' with `mod'/`%%'/`floor'/`truncate' and the 11-primitive gap
;; gone from every substrate but source-fallback, whose gap is a different,
;; still-open root cause, see the top branch) -- so a regression of either
;; bare-file symptom still gets its OWN accurate note on the next
;; regeneration instead of falling through to a generic one.
(defconst nelisp-substrate-parity--bare-arith-bug-indices '(9 11 13 14))

(defun nelisp-substrate-parity--auto-note (finding)
  (cl-destructuring-bind (idx substrate _baseline actual) finding
    (cond
     ;; FIXED 2026-08-22 (see src/nelisp-core-fileio.el and the commit that
     ;; removed the matching keys from this ledger): the pure-source
     ;; fallback's `nelisp-core-read-file-as-string' silently returned "" for
     ;; every file -- `nl-syscall-read-file' (what it tried first) is a
     ;; `declare-function' against a "nelisp-runtime" module this tree never
     ;; implements, so it fell to a buffer-based read whose
     ;; `buffer-substring-no-properties' is a `scripts/nelisp-stdlib-prelude.el'
     ;; stub that always answers "" (no real buffer exists at that layer).
     ;; `nelisp-load-file' therefore "loaded" zero forms, so every one of
     ;; this corpus's 35 forms hit the SAME void-function on the shim's own
     ;; `probe-emit', which read as one failure rather than 35.  That symptom
     ;; is gone now that the file is actually read; this branch remains only
     ;; to describe what a NEW, genuinely-different source-fallback finding
     ;; means post-fix -- it is never the old symptom, since a regression of
     ;; the old bug reproduces as void-function on the corpus's OWN shim
     ;; function, not on a corpus-form primitive.
     ;;
     ;; UPDATED 2026-08-22 (bare-file prelude-unification commit): matches
     ;; every source-fallback finding, not just a CRASH result or form 34 --
     ;; six of these (25/27/28/32's `fboundp' checks, plus what a raw call
     ;; would have hit on 2/3/4/5/15/19/22's own symbols) were previously
     ;; invisible because the OLD, bug-having bare baseline crashed on the
     ;; SAME primitive with the SAME classification, so bare and
     ;; source-fallback agreed (no diff, no ledger entry) for the wrong
     ;; reason.  Fixing bare's baseline exposed them as real, independent
     ;; source-fallback gaps.  Omitted-primitive list measured complete by
     ;; this run (`checked=209' against the fixed bare baseline): `floor',
     ;; `%', `unibyte-string', `match-data', `nelisp--write-stdout-bytes',
     ;; `intern-soft', `read-from-string', `read', `make-temp-name', `prin1'.
     ((equal substrate "source-fallback")
      "KNOWN GAP (2026-08-22, exposed by the eval-elisp-source pure-source-fallback file-read fix -- see src/nelisp-core-fileio.el -- and, for six of these forms, by the separate bare-file prelude-unification fix that stopped masking them behind a matching bare-side crash): source-fallback is the only substrate whose FORM arguments (and its loaded FILE's own top-level forms) run through the self-hosted `nelisp-eval' rather than through the native evaluator every other substrate calls directly. `nelisp--primitive-symbols' (src/nelisp-eval.el) is the list `nelisp--install-primitives' bulk-copies from native `fboundp' cells into `nelisp-eval's own function table, and it omits `floor', `%', `unibyte-string', `match-data', `nelisp--write-stdout-bytes', `intern-soft', `read-from-string', `read', `make-temp-name', and `prin1' -- all present natively (every other substrate agrees on the corpus's intended value), void-function (or `fboundp'-false) only where `nelisp-eval' has to look them up itself. Not fixed here: completing `nelisp--primitive-symbols' is a separate, broader prelude-parity sweep, not a defect in either the file-loading mechanism or the bare-file entry point this tree's two prelude fixes repair.")
     ((and (equal substrate "host") (= idx 8))
      "ACCEPTED (by design, class ii): NeLisp strings are Rust UTF-8 internally, so a raw byte >= 128 written through `unibyte-string' round-trips through UTF-8 re-encoding -- `aref' does not return the original byte the way host Emacs's unibyte strings do. Documented in memory feedback_nelisp_standalone_utf8_raw_byte_block. Every NeLisp substrate agrees with every other NeLisp substrate here; only the comparison against host Emacs differs, which is exactly the documented limitation.")
     ((equal substrate "host")
      "GAP vs host Emacs, consistent across every NeLisp substrate that can run this form -- not a cross-substrate NeLisp divergence. Form 20 (`emacs-pid') in particular is void-function in EVERY NeLisp substrate alike (bare, --load, runtime-image, artifact, source-cache), so it is a missing-primitive backlog item versus host Emacs, not a parity defect between NeLisp entry points.")
     ((memq idx nelisp-substrate-parity--bare-arith-bug-indices)
      "FIXED 2026-08-22 (scripts/nelisp-standalone-build.el: the bare-file source-selection dispatcher now runs the same prelude `--load' does, which is where `floor'/`mod'/`%'/`truncate' get their Elisp-semantics overrides installed over the native C-style builtins -- see the \"A1 floor\" cold-path comment near `nl_cold_overwrite_globals'). Was: the bare-file substrate's `mod'/`%%' computed a C-style truncating remainder (sign follows the dividend) instead of Elisp's floor-based `mod' (sign follows the divisor), and `floor'/`truncate' silently ignored a 2-argument divisor and returned the dividend unchanged -- a silent wrong answer, not a crash. Verified fixed by this gate (`checked=209 findings=0' with no idx-9/11/13/14 finding on any substrate but source-fallback, whose idx-13/14 gap is the unrelated, still-open `nelisp-eval' primitive-symbols gap named in the top branch above) and by direct exit-code probes: bare `(mod 7 -3)' now returns -2, `(floor 7 2)' now returns 3, matching `--load' exactly. This branch is unreachable against the current findings set; kept so a regression reproduces with its own accurate note rather than the generic one below.")
     (t
      "FIXED 2026-08-22 (scripts/nelisp-standalone-build.el: the bare-file source-selection dispatcher, reached via the `t' cond clause's final `nl_os_read_file_cpath' fallback, now runs `nelisp-standalone--reader-repl-prelude-forms' under the same `_cl < 0' gate `--load' uses, guarded off for runtime-image/artifact commands via `nl_runtime_image_command_p'/`nl_artifact_command_p' since those already carry their own prelude). Was: the bare positional FILE argument init path booted a smaller prelude than every other NeLisp entry point (--load, runtime-image, artifact, eval-elisp-source). Measured missing there and present everywhere else: intern-soft, read-from-string, princ, prin1, message, match-data, string-match, nreverse, read, random, make-temp-name. Extended the ADDENDA's pre-existing intern-soft finding (agent 1). Verified fixed by this gate (`checked=209 findings=0' with none of these 20 form/substrate pairs -- indices 2/3/4/5/15/18/19/21/22/24/25/26/27/28/32/33 against load/runtime-image/artifact/source-cache/host -- appearing as a finding any more) and by direct fboundp probes against the rebuilt binary. This branch is unreachable against the current findings set; kept so a regression of either the missing-prelude symptom in general, or of any ONE primitive from this list specifically, still gets a note naming the right root cause instead of a generic fallback."))))

(defun nelisp-substrate-parity--notes-for (findings previous-notes &optional note-fn)
  "Notes for FINDINGS: a note carried over from PREVIOUS-NOTES (hand-edited,
survives regeneration) wins; otherwise auto-generate one by root cause via
NOTE-FN (default `nelisp-substrate-parity--auto-note')."
  (let ((note-fn (or note-fn #'nelisp-substrate-parity--auto-note)))
    (mapcar (lambda (f)
              (let* ((key (nelisp-substrate-parity--finding-key f))
                     (prev (cdr (assoc key previous-notes))))
                (cons key (or prev (funcall note-fn f)))))
            findings)))

;; ---------------------------------------------------------------------
;; Presence-sweep auto-notes (Task A/B).  Same contract as
;; `nelisp-substrate-parity--auto-note': one classifier per finding, fired
;; only the first time a key is seen (a hand-written note surviving
;; regeneration always wins over this).
;; ---------------------------------------------------------------------

(defconst nelisp-substrate-presence--layer2-package-re
  "\\`\\(process-\\|set-process-\\|sqlite\\|url-\\|make-network-process\\'\\)"
  "Names whose presence sweep host-only gap (present on host, absent from
NeLisp's core bare-file boot) is BY DESIGN: process/sqlite/url are Layer-2
package capabilities (see memory feedback_nelisp_core_vs_layer2_boundary
and the \"SQLite primitives\"/\"URL + crypto primitives\" phase comments in
`nelisp--primitive-symbols', src/nelisp-eval.el) -- available once the
matching package loads, not part of the minimal core this sweep's shim
boots.")

(defconst nelisp-substrate-presence--native-gap-names
  '(char-or-string-p goto-char line-number-at-pos re-search-forward point
    read-from-minibuffer secure-hash send-string-to-terminal user-error)
  "Names `nelisp--primitive-symbols' (src/nelisp-eval.el) already lists as
\"borrowed wholesale\", and which DO work through every self-hosted-eval
substrate, but which the bare-file/--load/runtime-image NATIVE evaluator
has never implemented as a native op -- measured 2026-08-22: present
nowhere but host, not gated behind any package `require'.  A real,
individually small, KNOWN DEFECT backlog against host parity; fixing it
means adding native ops, which needs design work this pass did not do.")

(defun nelisp-substrate-presence--name-for (idx)
  "The symbol corpus form IDX probes, or nil."
  (let ((entry (assoc idx nelisp-substrate-presence-corpus)))
    (and entry
         (let ((form (nth 2 entry)))
           ;; (if (fboundp 'NAME) 1 0) -> NAME
           (ignore-errors (cadr (cadr (nth 1 form))))))))

(defconst nelisp-substrate-presence--host-fboundp-quirk-names
  '(defvaralias)
  "Names whose presence-sweep host divergence is neither a package
boundary nor a native-evaluator gap: `(fboundp 'defvaralias)' answers nil
on EVERY NeLisp substrate, bare-file included, yet `(defvaralias 'a 'b)'
works correctly everywhere it was tried (the 39-form behavioral corpus's
form 35 calls it and passes on bare/--load/runtime-image/artifact).
`defvaralias' is implemented as a native reader-level special-form
dispatch (`nl_sf_defvaralias', scripts/nelisp-standalone-build.el), which
NeLisp's native `fboundp' does not recognize as bound the way host
Emacs's `fboundp' recognizes a genuine special form -- a real, minor
`fboundp'-semantics difference, not a missing capability.")

(defun nelisp-substrate-presence--auto-note (finding)
  (cl-destructuring-bind (idx substrate baseline actual) finding
    (let* ((name (nelisp-substrate-presence--name-for idx))
           (name-str (and name (symbol-name name))))
      (cond
       ((equal substrate "source-fallback")
        (format "KNOWN DEFECT (class iii, presence sweep 2026-08-22 -- LARGER than the `nelisp--primitive-symbols' omission this ledger's sibling `tools/substrate-parity-accepted.el' names): source-fallback's self-hosted `nelisp-eval' (src/nelisp-eval.el) answers `(fboundp '%s)' false. For most of this class the name is a prelude-defined macro/function (`scripts/nelisp-stdlib-prelude.el's own `defmacro'/`defun'/`cond'/`when'/`push'/`cl-*'/`nelisp--'-internal helpers, etc.) or a native special form -- NOT a \"host primitive borrowed wholesale\" `nelisp--primitive-symbols' was ever meant to list. `nelisp--install-primitives' only ever copies names from that list into `nelisp-eval's own `nelisp--functions'/`nelisp--macros' tables, which is what `nelisp--builtin-fboundp' actually queries -- it does NOT fall back to the ambient (native) environment the way calling the form itself might. The form may well still be CALLABLE in source-fallback (several of Task C's unwind-protect/condition-case/catch forms are, and `fboundp' on THEM was never tested by this corpus) -- this key is specifically about `fboundp' visibility, not about whether `%s' can be invoked. Fixing this class needs a broader registry (or an ambient-environment fallback in `nelisp--builtin-fboundp' itself), not a `nelisp--primitive-symbols' extension; not undertaken this pass. idx=%d baseline=%s actual=%s." (or name-str "?") (or name-str "it") idx baseline actual))
       ((and (equal substrate "host") (equal baseline "1"))
        (format "GENERATOR-TAG BUG IF THIS SURVIVES REVIEW (idx=%d, `%s'): baseline=1 actual=0 means NeLisp has this name and host does not -- almost certainly a `nelisp-'-namespace name `tools/nelisp-substrate-presence-gen.el's `nlsp--tag' should have marked `standalone-only' (skipped against host entirely), not a real host gap. Fix the generator's tag rule and regenerate rather than accepting this key." idx (or name-str "?")))
       ((and (equal substrate "host") name (memq name nelisp-substrate-presence--host-fboundp-quirk-names))
        (format "ACCEPTED (by design-ish, real but tiny): `%s' -- see `nelisp-substrate-presence--host-fboundp-quirk-names's commentary. `fboundp' answers false on every NeLisp substrate for this native special form even though calling it works; not a missing capability." name-str))
       ((and (equal substrate "host") name-str
             (string-match-p nelisp-substrate-presence--layer2-package-re name-str))
        (format "ACCEPTED (by design, class ii): `%s' is a Layer-2 package capability (process/sqlite/url), present on host Emacs but not part of NeLisp's minimal core boot -- available once the matching package (packages/nelisp-process, nelisp-sqlite, nelisp-http/nelisp-network) is loaded, which this sweep's shim deliberately does not do. Not a defect; see `nelisp-substrate-presence--layer2-package-re's commentary." name-str))
       ((and (equal substrate "host") name (memq name nelisp-substrate-presence--native-gap-names))
        (format "KNOWN DEFECT (class iii, real but small): `%s' is listed in `nelisp--primitive-symbols' as a borrowed host primitive and works through every self-hosted-eval substrate, but the NATIVE bare-file/--load/runtime-image evaluator has never implemented it as a native op -- present nowhere in NeLisp but host. Not package-gated, not a generator artifact; a genuine native-evaluator backlog item, not fixed this pass (would need native-op design work, not a primitive-table extension)." name-str))
       ((equal substrate "host")
        (format "UNTRIAGED host gap (idx=%d, `%s'): baseline=%s actual=%s -- every NeLisp substrate agrees with each other; only host differs. Needs a reasoned classification (by-design vs. real gap) before this key ships as accepted." idx (or name-str "?") baseline actual))
       (t
        (format "UNTRIAGED (presence sweep, first regeneration): idx=%d substrate=%s baseline=%s actual=%s -- needs a reasoned classification before this key ships; see the commit that reviewed this ledger." idx substrate baseline actual))))))

(cond
 ((getenv "NELISP_SUBSTRATE_PARITY_WRITE")
  (let* ((run (nelisp-substrate-parity--run-all nelisp-substrate-parity-corpus))
         (findings (nelisp-substrate-parity--diff run))
         (previous (nelisp-substrate-parity--load-accepted nelisp-substrate-parity--accepted-file)))
    (princ (format "wrote %d accepted key(s) to %s\n"
                   (nelisp-substrate-parity--write-accepted
                    findings nelisp-substrate-parity--accepted-file
                    (format-time-string "%Y-%m-%d")
                    "Regenerated from a reviewed tree."
                    (nelisp-substrate-parity--notes-for
                     findings (plist-get previous :notes)
                     #'nelisp-substrate-parity--auto-note)
                    "NELISP_SUBSTRATE_PARITY_WRITE")
                   nelisp-substrate-parity--accepted-file))
    (kill-emacs 0)))
 ((getenv "NELISP_SUBSTRATE_PRESENCE_WRITE")
  (let* ((run (nelisp-substrate-parity--run-all
               nelisp-substrate-presence-corpus
               nelisp-substrate-parity--presence-host-preamble))
         (findings (nelisp-substrate-parity--diff run))
         (previous (nelisp-substrate-parity--load-accepted nelisp-substrate-parity--presence-accepted-file)))
    (princ (format "wrote %d accepted key(s) to %s\n"
                   (nelisp-substrate-parity--write-accepted
                    findings nelisp-substrate-parity--presence-accepted-file
                    (format-time-string "%Y-%m-%d")
                    "Regenerated from a reviewed tree."
                    (nelisp-substrate-parity--notes-for
                     findings (plist-get previous :notes)
                     #'nelisp-substrate-presence--auto-note)
                    "NELISP_SUBSTRATE_PRESENCE_WRITE")
                   nelisp-substrate-parity--presence-accepted-file))
    (kill-emacs 0)))
 ;; NELISP_SUBSTRATE_PARITY_NO_AUTORUN: escape hatch for loading this file
 ;; to poke at its internals interactively (from `--eval', an ad hoc probe)
 ;; without a full corpus run firing as a side effect of `-l'.  Unset (the
 ;; default) preserves the exact behavior every make target relies on.
 ((getenv "NELISP_SUBSTRATE_PARITY_NO_AUTORUN") nil)
 (noninteractive
  (kill-emacs
   (if (getenv "NELISP_SUBSTRATE_PRESENCE")
       (nelisp-substrate-parity-presence-gate-run)
     (nelisp-substrate-parity-gate-run)))))

(provide 'nelisp-substrate-parity)

;;; nelisp-substrate-parity.el ends here
