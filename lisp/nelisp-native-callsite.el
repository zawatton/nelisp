;;; nelisp-native-callsite.el --- REPL binding for build-declared call sites -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 202 (WS-G).  This module is the REPL-facing counterpart of the
;; build-time mechanism in scripts/nelisp-standalone-build.el: a
;; DECLARED set of native call sites (`tools/nelisp-replaceable-
;; entries.txt') whose PUBLIC name was build-time-wrapped, so an
;; existing binary's pre-existing direct `call rel32' callers reach a
;; control-word dispatch instead of the original body.  That mechanism
;; is real only for names declared at BUILD time; it does not, and does
;; not claim to, retarget any OTHER existing call site.
;;
;; This file draws a hard line the rest of this codebase already draws
;; between two categorically different replacement mechanisms:
;;
;;   :build-declared  a name in `tools/nelisp-replaceable-entries.txt',
;;                     wrapped at build time.  Every existing direct
;;                     caller in THIS binary already calls the wrapper.
;;
;;   :gate-only        a name NOT declared at build time.  The only way
;;                     to redirect ANY call to it is a NEW call site
;;                     written to go through `nelisp-native-unit-
;;                     address' (lisp/nelisp-native-unit.el) -- existing
;;                     direct callers of the original name are NEVER
;;                     reached, because the code they were compiled
;;                     into never changes (see nelisp-native-unit.el's
;;                     own header: "Existing binary direct calls are
;;                     not patched.").
;;
;;   :not-replaceable  neither of the above applies.
;;
;; `nelisp-native-callsite-reachability' is the one function in this
;; codebase whose whole job is answering that question honestly; see
;; its docstring before reading anything else here as a promise that a
;; name is "replaceable".
;;
;; This module reuses `nelisp-native-unit's PUBLIC surface
;; (`nelisp-native-unit-stage', `-publish', `-status', `-address') for
;; the hard, already-solved, already-tested part -- getting freshly
;; compiled native code a STABLE address that survives its own
;; re-publication.  It does not read or write `nelisp-native-unit's
;; private state, and it does not duplicate that state machine.  What
;; it adds is the one thing `nelisp-native-unit' explicitly does not
;; attempt: reaching EXISTING direct callers at all, for a declared set.
;;
;; Like lisp/nelisp-native-unit.el, this file is meant to be loaded into
;; the opt-in runtime-reload-reader process (`make runtime-reload-
;; reader'), where `ptr-read-u64' / `ptr-write-u64' / `ptr-call' /
;; `syscall-direct' and the `nelisp--native-callsite-symbol-addr'
;; resolver are real.  Its declaration-parsing and classification
;; helpers are ordinary Elisp and are exercised on any host by
;; test/nelisp-native-callsite-test.el; anything that needs the running
;; native process is additionally gated there with an inline
;; `skip-unless'.

;;; Code:

(require 'cl-lib)
(require 'nelisp-native-unit)

(declare-function ptr-read-u64 "nelisp-runtime" (address offset))
(declare-function ptr-write-u64 "nelisp-runtime" (address offset value))
(declare-function ptr-call "nelisp-runtime" (address a b c d e f))
(declare-function syscall-direct "nelisp-runtime" (number a b c d e f))
(declare-function nelisp--native-callsite-symbol-addr "nelisp-runtime" (index))

(defconst nelisp-native-callsite-max-arity 6
  "SysV integer argument limit a declared call site must respect.
Mirrors `nelisp-standalone--callsite-max-arity' in
scripts/nelisp-standalone-build.el.")

(defconst nelisp-native-callsite--fixed-symbol-names
  '("nl_callsite_control" "nl_callsite_install")
  "The two control symbols that always occupy indices 0 and 1 of the
`nelisp--native-callsite-symbol-addr' numeric bridge, when it exists at
all.  Every declared entry's PUBLIC (wrapper) name follows, in
declaration-file order, starting at index 2.  Mirrors
`nelisp-standalone--callsite-symbol-names' in
scripts/nelisp-standalone-build.el; see that function's docstring --
nothing links the two lists together, so
test/nelisp-native-callsite-test.el asserts they agree on a shared
sample.")

(defconst nelisp-native-callsite--entries-relative-path
  "tools/nelisp-replaceable-entries.txt"
  "Declaration file, relative to a repository checkout root.
Mirrors `nelisp-standalone--callsite-entries-file'.")

;;;; Pure declaration parsing (no I/O; run on any host) --------------

(defun nelisp-native-callsite--parse-entries (text)
  "Parse TEXT into a list of plists `(:name NAME :arity ARITY :index INDEX)'.

Same grammar, same refusals, and the same contiguous-0..N-1 requirement
on INDEX as `nelisp-standalone--callsite-parse-entries' in
scripts/nelisp-standalone-build.el (kept as an independent
implementation on purpose -- this file must stay loadable inside the
constrained opt-in runtime process without pulling in the AOT compiler
scripts/nelisp-standalone-build.el requires); a test feeds the same
sample text to both and asserts they agree."
  (let ((entries nil) (line-no 0) (seen-names nil) (seen-indices nil))
    (dolist (line (split-string text "\n"))
      (setq line-no (1+ line-no))
      (let ((trimmed (string-trim line)))
        (unless (or (= (length trimmed) 0) (= (aref trimmed 0) ?#))
          (let ((fields (split-string trimmed nil t)))
            (unless (= (length fields) 3)
              (error "nelisp-native-callsite: %s:%d: expected \"NAME ARITY INDEX\", got %S"
                     nelisp-native-callsite--entries-relative-path line-no trimmed))
            (let* ((name (nth 0 fields))
                   (arity-str (nth 1 fields))
                   (index-str (nth 2 fields)))
              (unless (string-match-p "\\`[A-Za-z_][A-Za-z0-9_]*\\'" name)
                (error "nelisp-native-callsite: %s:%d: not an identifier: %S"
                       nelisp-native-callsite--entries-relative-path line-no name))
              (unless (string-match-p "\\`[0-9]+\\'" arity-str)
                (error "nelisp-native-callsite: %s:%d: arity is not a non-negative integer: %S"
                       nelisp-native-callsite--entries-relative-path line-no arity-str))
              (unless (string-match-p "\\`[0-9]+\\'" index-str)
                (error "nelisp-native-callsite: %s:%d: index is not a non-negative integer: %S"
                       nelisp-native-callsite--entries-relative-path line-no index-str))
              (let ((arity (string-to-number arity-str))
                    (index (string-to-number index-str)))
                (when (> arity nelisp-native-callsite-max-arity)
                  (error "nelisp-native-callsite: %s:%d: arity %d exceeds the SysV integer limit of %d"
                         nelisp-native-callsite--entries-relative-path line-no arity
                         nelisp-native-callsite-max-arity))
                (when (member name seen-names)
                  (error "nelisp-native-callsite: %s:%d: duplicate name %S"
                         nelisp-native-callsite--entries-relative-path line-no name))
                (when (memq index seen-indices)
                  (error "nelisp-native-callsite: %s:%d: duplicate index %d"
                         nelisp-native-callsite--entries-relative-path line-no index))
                (push name seen-names)
                (push index seen-indices)
                (push (list :name name :arity arity :index index) entries)))))))
    (setq entries (sort entries (lambda (a b)
                                  (< (plist-get a :index) (plist-get b :index)))))
    (let ((expect 0))
      (dolist (e entries)
        (unless (= (plist-get e :index) expect)
          (error "nelisp-native-callsite: %s: indices must be exactly 0..%d contiguous; got a gap at %d"
                 nelisp-native-callsite--entries-relative-path (1- (length entries)) expect))
        (setq expect (1+ expect))))
    entries))

(defun nelisp-native-callsite--entries-path (&optional root)
  (expand-file-name nelisp-native-callsite--entries-relative-path
                     (or root default-directory)))

(defun nelisp-native-callsite--read-declared-entries (&optional root)
  "Read and parse the declaration file under ROOT (default `default-directory').
Nil when the file is missing or empty -- never signals merely for
absence, since a binary built with the mechanism disabled is a normal,
expected case, not an error."
  (let ((path (nelisp-native-callsite--entries-path root)))
    (and (file-readable-p path)
         (let ((text (with-temp-buffer
                       (insert-file-contents path)
                       (buffer-string))))
           (nelisp-native-callsite--parse-entries text)))))

(defun nelisp-native-callsite--contract-magic (entries)
  "A stable non-negative integer covering ENTRIES' names/arities/indices.
Pure function of ENTRIES; identical algorithm to
`nelisp-standalone--callsite-contract-magic'."
  (let* ((print-length nil) (print-level nil)
         (bytes (prin1-to-string
                 (list 'nelisp-native-callsite-contract-v1
                       (mapcar (lambda (e) (list (plist-get e :name)
                                                 (plist-get e :arity)
                                                 (plist-get e :index)))
                               entries)))))
    (logand (string-to-number (substring (secure-hash 'sha256 bytes) 0 15) 16)
            #xFFFFFFFFFFFFFF)))

(defun nelisp-native-callsite--symbol-names (entries)
  "Return the full resolver-order name list for ENTRIES.
Mirrors `nelisp-standalone--callsite-symbol-names'."
  (append nelisp-native-callsite--fixed-symbol-names
          (mapcar (lambda (e) (plist-get e :name)) entries)))

;;;; Native resolver bridge (real only inside the opt-in process) -----

(defun nelisp-native-callsite--resolver-available-p ()
  "Non-nil when this process's binary exposes the numeric symbol bridge.
False in host Emacs and in any binary built without >=1 declared entry;
callers must treat that as \"nothing to report\", never as an error."
  (fboundp 'nelisp--native-callsite-symbol-addr))

(defun nelisp-native-callsite--resolve-index (index)
  "Resolve INDEX through the native bridge, or nil on any failure.
Never signals: an out-of-range index, a disabled resolver, or a symbol
that failed to link all read the same way here -- absence, not error."
  (and (nelisp-native-callsite--resolver-available-p)
       (condition-case nil
           (let ((addr (nelisp--native-callsite-symbol-addr index)))
             (and (integerp addr) (> addr 0) addr))
         (error nil))))

(defun nelisp-native-callsite-control-address ()
  "Return the running binary's `nl_callsite_control' address, or nil."
  (nelisp-native-callsite--resolve-index 0))

(defun nelisp-native-callsite-installer-address ()
  "Return the running binary's `nl_callsite_install' address, or nil."
  (nelisp-native-callsite--resolve-index 1))

;;;; Public entries() -- never fabricate ------------------------------

(defun nelisp-native-callsite-entries (&optional root)
  "Return the declared entries actually present in the running binary.

Each element is `(:name NAME :arity ARITY :index INDEX :control ADDR)'.

Cross-checked, not merely read off disk: an entry is included only when
BOTH the on-disk declaration under ROOT parses it AND its own PUBLIC
name resolves through the running binary's numeric bridge.  A name
edited into (or out of) the declaration file after the binary was built
therefore never appears here on the strength of the file alone -- see
the module Commentary's promise not to fabricate an entry.

Nil whenever the running process exposes no callsite resolver at all
(host Emacs, or any binary built with zero declared entries) -- this is
the normal \"this binary doesn't have the mechanism\" case, not a
failure."
  (let ((control (nelisp-native-callsite-control-address)))
    (and control
         (let* ((declared (nelisp-native-callsite--read-declared-entries root))
                (result nil) (index 2))
           (dolist (entry declared)
             (when (nelisp-native-callsite--resolve-index index)
               (push (plist-put (copy-sequence entry) :control control) result))
             (setq index (1+ index)))
           (nreverse result)))))

(defun nelisp-native-callsite-status ()
  "Return this process's callsite control block as a diagnostic plist.
`:status' is `unavailable' (with `:reason') or `ready'.  Non-signalling,
like `nelisp-runtime-reload-status' in lisp/nelisp-native-load.el: a
REPL can explain why nothing is available without turning an expected
capability check into a crash."
  (let ((control (nelisp-native-callsite-control-address)))
    (if (not control)
        (list :status 'unavailable
              :reason (if (nelisp-native-callsite--resolver-available-p)
                          :no-declared-entries
                        :resolver-not-built))
      (list :status 'ready
            :control control
            :table (ptr-read-u64 control 0)
            :generation (ptr-read-u64 control 8)
            :active-calls (ptr-read-u64 control 16)
            :install-locked (/= (ptr-read-u64 control 24) 0)))))

;;;; Publication: reuse nelisp-native-unit for the replacement bodies -

(defun nelisp-native-callsite--build-table (resolved magic)
  "Mmap, populate, and RO-protect a table page for RESOLVED (an
ENTRIES-ordered list of resolved replacement addresses) tagged with
MAGIC.  Uses the same primitives `nelisp-native-unit--create'/`--stage'
already use for their own tables -- not `nelisp-native-unit's private
functions themselves, the shared mmap/mprotect primitives underneath
them."
  (let ((table (nelisp-native-load--mmap 4096 nil)))
    (ptr-write-u64 table 0 (length resolved))
    (ptr-write-u64 table 8 magic)
    (let ((index 0))
      (dolist (addr resolved)
        (ptr-write-u64 table (+ 16 (* 8 index)) addr)
        (setq index (1+ index))))
    (unless (= 0 (syscall-direct 10 table 4096 1 0 0 0)) ; mprotect PROT_READ
      (error "nelisp-native-callsite: cannot protect replacement table"))
    table))

(defun nelisp-native-callsite-install (entry-set table)
  "Publish replacement addresses for the running binary's declared entries.

ENTRY-SET is an alist of `(CALLSITE-NAME . UNIT-EXPORT-NAME)' (a bare
string CALLSITE-NAME is short for `(CALLSITE-NAME . CALLSITE-NAME)',
when the replacement unit exports the same name).  TABLE is the
`nelisp-native-unit' UNIT-ID whose PUBLISHED contract exports every
UNIT-EXPORT-NAME in ENTRY-SET -- stage and publish it with
`nelisp-native-unit-stage'/`-publish' first; this function never
compiles or maps native code itself.

ENTRY-SET must cover every entry `nelisp-native-callsite-entries'
currently reports, exactly once each: like `nelisp-native-unit' itself,
there is no partial install -- a table slot this build never assigns a
value stays a null `call-ptr' target the instant any table is
installed, so a partial ENTRY-SET is refused before that table is ever
built.  Each address is resolved through `nelisp-native-unit-address',
reusing that unit's OWN generation/CAS discipline for how the address
itself becomes valid and stays valid across the unit's own later
republication; this function does not reimplement that.  The resulting
table is then CAS-published into `nl_callsite_control' by
`nl_callsite_install', gated by the SAME generation-must-increase and
no-active-call discipline `nl_runtime_reload_install' already uses for
the allocator/GC precedent -- rejecting a stale generation, an in-flight
call, or a table whose entry-count/contract magic does not match this
binary's declared set."
  (condition-case err
      (let* ((entries (nelisp-native-callsite-entries))
             (installer (nelisp-native-callsite-installer-address))
             (control (nelisp-native-callsite-control-address)))
        (unless entries
          (error "nelisp-native-callsite: no declared entries in this binary"))
        (unless installer
          (error "nelisp-native-callsite: installer entry not resolvable"))
        (unless (= (length entry-set) (length entries))
          (error "nelisp-native-callsite: entry-set must cover all %d declared entries, got %d"
                 (length entries) (length entry-set)))
        (let ((resolved nil))
          (dolist (entry entries)
            (let* ((name (plist-get entry :name))
                   (mapping (assoc name entry-set))
                   (export-name (if mapping (cdr mapping) name)))
              (unless (assoc name entry-set)
                (error "nelisp-native-callsite: entry-set is missing declared entry %s" name))
              (push (nelisp-native-unit-address table export-name) resolved)))
          (setq resolved (nreverse resolved))
          (let* ((magic (nelisp-native-callsite--contract-magic entries))
                 (new-table (nelisp-native-callsite--build-table resolved magic))
                 (generation (1+ (ptr-read-u64 control 8)))
                 (rc (ptr-call installer new-table generation 0 0 0 0)))
            (if (= rc 0)
                (list :status 'published :generation (ptr-read-u64 control 8)
                      :entries (mapcar (lambda (e) (plist-get e :name)) entries))
              (list :status 'rejected
                    :reason (nth rc '(:ok :generation-regressed :active-call
                                       :contract-mismatch :install-locked))
                    :return-code rc)))))
    (error (list :status 'rejected :reason (error-message-string err)))))

;;;; Reachability classification -- the honesty boundary --------------

(defun nelisp-native-callsite--classify (name declared-names gate-unit-id)
  "Pure classifier behind `nelisp-native-callsite-reachability'.
DECLARED-NAMES is a list of build-declared names (as `entries' would
report them); GATE-UNIT-ID, when non-nil, is passed to
`nelisp-native-unit-address' to test whether NAME is published under
that unit.  Split out from the public function so tests can supply
both without a running native process."
  (cond
   ((member name declared-names) :build-declared)
   ((and gate-unit-id
         (condition-case nil
             (progn (nelisp-native-unit-address gate-unit-id name) t)
           (error nil)))
    :gate-only)
   (t :not-replaceable)))

(defun nelisp-native-callsite-reachability (name &optional gate-unit-id)
  "Classify how a replacement for the native function named NAME would
actually reach EXISTING callers.

Returns one of three symbols, and never overclaims:

  `:build-declared'  NAME is one of `nelisp-native-callsite-entries':
                      it was wrapped at build time, so every existing
                      direct caller in this binary already calls the
                      wrapper.  `nelisp-native-callsite-install' can
                      redirect what they observe.

  `:gate-only'        NAME is NOT build-declared, but GATE-UNIT-ID (an
                      already-published `nelisp-native-unit' unit-id)
                      exports it.  Only NEW call sites written to go
                      through `nelisp-native-unit-address' for that
                      unit reach it; every PRE-EXISTING direct caller
                      of the original NAME in this binary is unaffected
                      and stays that way forever (per
                      lisp/nelisp-native-unit.el's own header: existing
                      binary direct calls are not patched).

  `:not-replaceable'  neither of the above: no evidence NAME can be
                      reached by any mechanism this codebase has.  This
                      is the safe default whenever GATE-UNIT-ID is nil
                      or does not export NAME -- absence of evidence is
                      never treated as evidence of `:gate-only', let
                      alone `:build-declared'."
  (unless (and (stringp name) (> (length name) 0))
    (error "nelisp-native-callsite: name must be a non-empty string"))
  (nelisp-native-callsite--classify
   name
   (mapcar (lambda (e) (plist-get e :name)) (nelisp-native-callsite-entries))
   gate-unit-id))

(provide 'nelisp-native-callsite)

;;; nelisp-native-callsite.el ends here
