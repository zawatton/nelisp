;;; nelisp-native-unit.el --- stable native entries with atomic unit publication -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Closed SysV integer units only. Existing binary direct calls are not patched.
(require 'nelisp-native-load)
(declare-function ptr-read-u64 "nelisp-runtime" (address offset))
(declare-function ptr-write-u64 "nelisp-runtime" (address offset value))
(declare-function ptr-call "nelisp-runtime" (address a b c d e f))
(declare-function syscall-direct "nelisp-runtime" (number a b c d e f))

(defconst nelisp-native-unit-max-exports 64)
(defconst nelisp-native-unit-candidate-ttl 900)
(defconst nelisp-native-unit--capacity 64)
(defconst nelisp-native-unit--table-bytes 4096
  "Size of a candidate's generation table page.

Every `nelisp-native-load--mmap' call this file makes for a table uses
this exact size; reclamation munmaps the same extent mmap was given.")
(defconst nelisp-native-unit-history-max 16
  "Publications retained per unit in `:history', oldest evicted first.

Enough to reproduce the current published state in a new process --
source, artifact and generation for each of the last 16 publications --
without keeping an unbounded log.")
(defvar nelisp-native-unit--next-id 0)
(defvar nelisp-native-unit--candidates nil)
(defvar nelisp-native-unit--units nil)
(defvar nelisp-native-unit--publisher nil)
(defvar nelisp-native-unit--reclaimed-tables 0
  "Cumulative count of generation table pages this process has unmapped.

Never decreases: a page once released stays released, and this is a
running total for the life of the process, not a live gauge.")
(defvar nelisp-native-unit--reclaimed-bytes 0
  "Cumulative bytes this process has unmapped across tables and artifacts.

Includes both `nelisp-native-unit--table-bytes'-sized table pages and,
when the loader's `nelisp-native-load-unload' releases one, a raw
artifact's mapped code size.")

(defun nelisp-native-unit--copy (value)
  (cond ((stringp value) (copy-sequence value))
        ((consp value) (cons (nelisp-native-unit--copy (car value))
                            (nelisp-native-unit--copy (cdr value))))
        (t value)))

(defun nelisp-native-unit--id (prefix)
  (setq nelisp-native-unit--next-id (1+ nelisp-native-unit--next-id))
  (concat prefix "-" (secure-hash 'sha256
                                  (format "%S:%S:%S" (current-time)
                                          (random) nelisp-native-unit--next-id))))

(defun nelisp-native-unit--u32 (n)
  (list (logand n 255) (logand (ash n -8) 255)
        (logand (ash n -16) 255) (logand (ash n -24) 255)))

(defun nelisp-native-unit--u64-bytes (n)
  (append (nelisp-native-unit--u32 n) (nelisp-native-unit--u32 (ash n -32))))

(defun nelisp-native-unit--gate-bytes (control index)
  ;; SysV args remain untouched. Load the current immutable generation table once.
  (append '(#x48 #xb8) (nelisp-native-unit--u64-bytes control)
          '(#x48 #x8b #x00 #xff #xa0)
          (nelisp-native-unit--u32 (+ 8 (* 8 index))) (make-list 13 0)))

(defun nelisp-native-unit--publisher-bytes ()
  ;; rdi=control, rsi=expected, rdx=next; lock cmpxchg; return boolean.
  '(#x48 #x89 #xf0 #xf0 #x48 #x0f #xb1 #x17
    #x0f #x94 #xc0 #x0f #xb6 #xc0 #xc3))

(defun nelisp-native-unit--find (id)
  (cdr (assoc id nelisp-native-unit--units)))

(defun nelisp-native-unit--remove-candidate (id)
  (setq nelisp-native-unit--candidates
        (cl-remove-if (lambda (entry) (equal (car entry) id))
                      nelisp-native-unit--candidates)))

(defun nelisp-native-unit--unmap-table (address)
  "Unmap the `nelisp-native-unit--table-bytes' page this file mapped at ADDRESS.

Only ever called with an address this file itself obtained from
`nelisp-native-load--mmap' as a candidate's `:table', and only before that
candidate has ever won its CAS (see the call sites: discard, TTL expiry,
a staging failure after the table was allocated, and a refused publish).
A table that was ever CAS-installed into a unit's control word is never
passed here -- doing so would unmap memory a stable gate can still reach.

A no-op, not an error, where `syscall-direct' is unavailable: on host
Emacs ADDRESS is never a real mapping, and there is nothing to unmap."
  (when (and (integerp address) (> address 0) (fboundp 'syscall-direct))
    ;; munmap(2) is syscall 11 on Linux x86_64.
    (when (= 0 (syscall-direct 11 address nelisp-native-unit--table-bytes 0 0 0 0))
      (setq nelisp-native-unit--reclaimed-tables
            (1+ nelisp-native-unit--reclaimed-tables))
      (setq nelisp-native-unit--reclaimed-bytes
            (+ nelisp-native-unit--reclaimed-bytes nelisp-native-unit--table-bytes))
      t)))

(defun nelisp-native-unit--unload-handle (handle)
  "Release HANDLE's mapped raw-artifact pages through the loader, if it can.

`nelisp-native-load-unload' is the loader's release entry point; it is
called only when both it and `syscall-direct' are available, so a host
run without the native runtime never attempts a real munmap on a handle
that was never really mapped.  Errors from the release itself are not
swallowed silently as success: `:code-size' is only added to the
cumulative reclaimed-bytes total when the loader reports it released
something."
  (when (and handle (fboundp 'nelisp-native-load-unload) (fboundp 'syscall-direct))
    (let ((size (plist-get handle :code-size)))
      (when (and (integerp (ignore-errors (nelisp-native-load-unload handle)))
                 (integerp size) (> size 0))
        (setq nelisp-native-unit--reclaimed-bytes
              (+ nelisp-native-unit--reclaimed-bytes size))
        t))))

(defun nelisp-native-unit--reclaim-candidate (candidate)
  "Release CANDIDATE's own table page and mapped artifact.

CANDIDATE was never CAS-installed -- it is being discarded, has expired,
or lost a staging or publish attempt -- so nothing executing can reach
either mapping."
  (when candidate
    (nelisp-native-unit--unmap-table (plist-get candidate :table))
    (nelisp-native-unit--unload-handle (plist-get candidate :artifact-handle))))

(defun nelisp-native-unit--purge ()
  (let ((now (float-time))
        (expired nil))
    (setq nelisp-native-unit--candidates
          (cl-remove-if
           (lambda (entry)
             (let ((stale (>= (- now (plist-get (cdr entry) :created))
                              nelisp-native-unit-candidate-ttl)))
               (when stale (push (cdr entry) expired))
               stale))
           nelisp-native-unit--candidates))
    (dolist (candidate expired)
      (nelisp-native-unit--reclaim-candidate candidate))))

(defun nelisp-native-unit-discard (candidate-id)
  "Revoke CANDIDATE-ID without publishing.

CANDIDATE-ID's generation table was never CAS-installed into any unit's
control word, so no executing code can reach it; its table page and
mapped artifact are reclaimed here rather than kept until process exit."
  (let ((candidate (cdr (assoc candidate-id nelisp-native-unit--candidates))))
    (nelisp-native-unit--remove-candidate candidate-id)
    (nelisp-native-unit--reclaim-candidate candidate)))

(defun nelisp-native-unit--history-push (history record)
  "Append RECORD to HISTORY, keeping at most `nelisp-native-unit-history-max'.

Oldest entries are evicted first."
  (let ((updated (append history (list record))))
    (if (> (length updated) nelisp-native-unit-history-max)
        (nthcdr (- (length updated) nelisp-native-unit-history-max) updated)
      updated)))

(defun nelisp-native-unit--retire-generation (unit table handle generation)
  "Move UNIT's previously active generation onto its retire list.

Never called for a unit's first publication, where TABLE is 0 -- there is
no previous generation to retire.  A retired generation's table and
mapped artifact stay mapped: a caller already executing through the old
generation via a stable gate may still be mid-call when this runs, and
this file has no way to observe that such a call has returned.  See
`nelisp-native-unit-reclaim', the only path that ever releases a retired
generation, and why today it always refuses to."
  (when (and (integerp table) (/= table 0))
    (plist-put unit :retired
               (cons (list :generation generation :table table :handle handle
                          :retired-at (float-time) :released nil)
                     (plist-get unit :retired)))))

(defun nelisp-native-unit--hash (path)
  "Hash bounded literal bytes, never a pathname string."
  (unless (and (file-regular-p path) (file-readable-p path))
    (error "native-unit: missing input %s" path))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path nil 0 (1+ (* 16 1024 1024)))
    (when (> (- (point-max) (point-min)) (* 16 1024 1024))
      (error "native-unit: input exceeds 16 MiB"))
    (secure-hash 'sha256 (buffer-string))))

(defun nelisp-native-unit--table (unit)
  (ptr-read-u64 (plist-get unit :control) 0))

(defun nelisp-native-unit--generation (table)
  (if (= table 0) 0 (ptr-read-u64 table 0)))

(defun nelisp-native-unit--create (contract binary)
  "Allocate stable gates once. Never write an existing unit's control slot."
  (let ((control (nelisp-native-load--mmap 4096 nil))
        (page (nelisp-native-load--mmap 4096 nil))
        (id (nelisp-native-unit--id "unit")) gates (index 0))
    (ptr-write-u64 control 0 0)
    (dolist (entry contract)
      (nelisp-native-load--poke-bytes page (* index 32)
                                     (nelisp-native-unit--gate-bytes control index))
      (push (cons (car entry) (+ page (* index 32))) gates)
      (setq index (1+ index)))
    (nelisp-native-load--mprotect-rx page 4096)
    (unless nelisp-native-unit--publisher
      (let ((publisher (nelisp-native-load--mmap 4096 nil)))
        (nelisp-native-load--poke-bytes publisher 0 (nelisp-native-unit--publisher-bytes))
        (nelisp-native-load--mprotect-rx publisher 4096)
        (setq nelisp-native-unit--publisher publisher)))
    (let ((unit (list :unit-id id :control control :contract contract
                      :gates (nreverse gates) :publisher nelisp-native-unit--publisher
                      :binary-sha256 binary)))
      (push (cons id unit) nelisp-native-unit--units)
      unit)))

(defun nelisp-native-unit--stage (artifact-path unit-id export-names)
  (nelisp-native-unit--purge)
  (when (>= (length nelisp-native-unit--candidates) nelisp-native-unit--capacity)
    (error "native-unit: candidate capacity reached"))
  (when (and (null unit-id) (>= (length nelisp-native-unit--units) nelisp-native-unit--capacity))
    (error "native-unit: unit capacity reached"))
  (let* ((old (and unit-id (nelisp-native-unit--find unit-id)))
         (_ (when (and unit-id (null old)) (error "native-unit: unknown unit")))
         (path (expand-file-name artifact-path))
         (artifact-hash (nelisp-native-unit--hash path))
         (manifest (nelisp-native-load-manifest path))
         (native (plist-get manifest :native))
         (source (plist-get manifest :source))
         (source-hash (nelisp-native-unit--hash source))
         (binary (nelisp-native-load--running-binary-sha256))
         (expected (if old (nelisp-native-unit--table old) 0))
         (generation (nelisp-native-unit--generation expected))
         (names (or export-names (and old (mapcar #'car (plist-get old :contract)))
                    (mapcar (lambda (e) (plist-get e :name))
                            (nelisp-native-load--raw-exports native))))
         (contract nil))
    (unless (and (null (nelisp-native-load-raw-check manifest))
                 (null (plist-get native :imports))
                 (stringp binary) (equal binary (plist-get manifest :binary-sha256))
                 (equal source-hash (plist-get manifest :source-sha256)))
      (error "native-unit: invalid, imported, or mismatched raw-v1 artifact"))
    (unless (and (proper-list-p names) names
                 (<= (length names) nelisp-native-unit-max-exports)
                 (cl-every #'stringp names)
                 (= (length names) (length (delete-dups (copy-sequence names)))))
      (error "native-unit: expected 1..64 distinct export names"))
    (dolist (name names)
      (let ((entry (nelisp-native-load--raw-export native name)))
        (unless entry (error "native-unit: public export missing: %s" name))
        (push (cons (copy-sequence name) (plist-get entry :arity)) contract)))
    (setq contract (nreverse contract))
    (when (and old (or (not (equal contract (plist-get old :contract)))
                      (not (equal binary (plist-get old :binary-sha256)))))
      (error "native-unit: public names, order, arities or binary changed"))
    (let* ((handle (nelisp-native-load-raw-artifact path (car names) binary))
           (unit (or old (nelisp-native-unit--create contract binary)))
           (table (nelisp-native-load--mmap nelisp-native-unit--table-bytes nil))
           (index 0)
           (id (nelisp-native-unit--id "candidate"))
           (staged nil))
      ;; Staging failure past this point leaks TABLE and HANDLE unless
      ;; explicitly reclaimed: neither is CAS-installed yet, so nothing
      ;; executing can reach either, and there is no other owner to hand
      ;; them to.  Reclaiming only when `staged' stays nil mirrors
      ;; `nelisp-native-load-raw-artifact''s own unwind-protect around
      ;; its codepage.
      (unwind-protect
          (progn
            (unless (and (equal artifact-hash (nelisp-native-unit--hash path))
                         (equal source-hash (nelisp-native-unit--hash source))
                         (equal (plist-get handle :artifact-sha256)
                                (plist-get manifest :artifact-sha256))
                         (equal binary (nelisp-native-load--running-binary-sha256))
                         (= expected (nelisp-native-unit--table unit)))
              (error "native-unit: inputs or generation changed while staging"))
            (ptr-write-u64 table 0 (1+ generation))
            (dolist (entry contract)
              (ptr-write-u64 table (+ 8 (* 8 index))
                             (nelisp-native-load-raw-export-address handle (car entry)))
              (setq index (1+ index)))
            (unless (= 0 (syscall-direct 10 table nelisp-native-unit--table-bytes 1 0 0 0))
              (error "native-unit: cannot protect immutable table"))
            (push (cons id (list :unit-id (plist-get unit :unit-id) :expected-table expected
                                 :generation (1+ generation) :table table
                                 :artifact-handle handle
                                 :artifact path :artifact-hash artifact-hash
                                 :source source :source-hash source-hash
                                 :binary-sha256 binary :created (float-time)))
                  nelisp-native-unit--candidates)
            (setq staged t)
            (nelisp-native-unit--copy
             (list :status 'staged :candidate-id id :unit-id (plist-get unit :unit-id)
                   :expected-generation generation :exports contract)))
        (unless staged
          (nelisp-native-unit--unmap-table table)
          (nelisp-native-unit--unload-handle handle))))))

(defun nelisp-native-unit-stage (artifact-path &optional unit-id export-names)
  "Validate and map a closed raw-v1 artifact without publication.
Reuse UNIT-ID's fixed public contract; additional internal helpers are allowed.
Returned data contains no private handles. Candidates expire after 15 minutes."
  (condition-case err
      (nelisp-native-unit--stage artifact-path unit-id export-names)
    (error (list :status 'rejected :phase :stage :reason (error-message-string err)))))

(defun nelisp-native-unit-publish (candidate-id)
  "Consume and atomically publish CANDIDATE-ID, rejecting stale identities.
Readers already executing an older generation may finish there. A new call
through a stable entry observes the current complete table. Old mappings stay.

On success, records the published source/artifact identity and generation
on the unit (read back by `nelisp-native-unit-status' and
`nelisp-native-unit-code-info'), appends to its bounded publication
history, and moves the SUPERSEDED generation onto its retire list --
never the one just published.  A refused publish -- a stale/tampered
candidate or a lost CAS -- leaves any previously recorded identity
untouched and reclaims CANDIDATE-ID's own table and artifact, exactly as
`nelisp-native-unit-discard' would."
  (condition-case err
      (progn
        (nelisp-native-unit--purge)
        (let* ((candidate (cdr (assoc candidate-id nelisp-native-unit--candidates)))
               (unit (and candidate
                          (nelisp-native-unit--find (plist-get candidate :unit-id))))
               (published nil))
          (unless unit (error "native-unit: unknown, expired or consumed candidate"))
          (nelisp-native-unit--remove-candidate candidate-id)
          (unwind-protect
              (progn
                (unless (and (equal (plist-get candidate :artifact-hash)
                                    (nelisp-native-unit--hash (plist-get candidate :artifact)))
                             (equal (plist-get candidate :source-hash)
                                    (nelisp-native-unit--hash (plist-get candidate :source)))
                             (< (- (float-time) (plist-get candidate :created))
                                nelisp-native-unit-candidate-ttl)
                             (equal (plist-get candidate :binary-sha256)
                                    (nelisp-native-load--running-binary-sha256)))
                  (error "native-unit: stale source, artifact, binary or deadline"))
                ;; Never initialize or restore control here. CAS is the only store.
                (unless (= 1 (ptr-call (plist-get unit :publisher)
                                       (plist-get unit :control)
                                       (plist-get candidate :expected-table)
                                       (plist-get candidate :table) 0 0 0))
                  (error "native-unit: stale generation (CAS rejected)"))
                ;; CAS succeeded: CANDIDATE's table is now the reachable
                ;; generation. Everything below this line, and only this
                ;; branch in the whole file, is allowed to record identity
                ;; or move the prior generation to the retire list.
                (setq published t)
                (nelisp-native-unit--retire-generation
                 unit (plist-get candidate :expected-table)
                 (plist-get unit :active-handle) (plist-get unit :active-generation))
                (let ((now (float-time)))
                  (plist-put unit :active-table (plist-get candidate :table))
                  (plist-put unit :active-handle (plist-get candidate :artifact-handle))
                  (plist-put unit :active-generation (plist-get candidate :generation))
                  (plist-put unit :published-source (plist-get candidate :source))
                  (plist-put unit :published-source-sha256
                             (plist-get candidate :source-hash))
                  (plist-put unit :published-artifact (plist-get candidate :artifact))
                  (plist-put unit :published-artifact-sha256
                             (plist-get candidate :artifact-hash))
                  (plist-put unit :published-generation (plist-get candidate :generation))
                  (plist-put unit :published-at now)
                  (plist-put unit :history
                             (nelisp-native-unit--history-push
                              (plist-get unit :history)
                              (list :generation (plist-get candidate :generation)
                                    :source-sha256 (plist-get candidate :source-hash)
                                    :artifact-sha256 (plist-get candidate :artifact-hash)
                                    :published-at now))))
                (nelisp-native-unit--copy
                 (list :status 'published :unit-id (plist-get unit :unit-id)
                       :generation (plist-get candidate :generation)
                       :artifact-sha256 (plist-get candidate :artifact-hash)
                       :source-sha256 (plist-get candidate :source-hash))))
            (unless published
              (nelisp-native-unit--reclaim-candidate candidate)))))
    (error (list :status 'rejected :phase :publish :reason (error-message-string err)))))

(defun nelisp-native-unit-status (unit-id)
  "Report the current table's generation, not cached host metadata.

Beyond the original :unit-id/:generation/:binary-sha256/:exports/:published
keys (unchanged), also reports the identity actually published --
:source, :source-sha256, :artifact, :artifact-sha256 and :published-at --
and :export-addresses, each export's stable gate address paired with the
generation currently reachable through it (the same number for every
export, since one table swap replaces them all at once)."
  (let ((unit (nelisp-native-unit--find unit-id)))
    (when unit
      (let* ((table (nelisp-native-unit--table unit))
             (generation (nelisp-native-unit--generation table))
             (gates (plist-get unit :gates)))
        (nelisp-native-unit--copy
         (list :unit-id unit-id :generation generation
               :binary-sha256 (plist-get unit :binary-sha256)
               :exports (plist-get unit :contract) :published (/= table 0)
               :source (plist-get unit :published-source)
               :source-sha256 (plist-get unit :published-source-sha256)
               :artifact (plist-get unit :published-artifact)
               :artifact-sha256 (plist-get unit :published-artifact-sha256)
               :published-at (plist-get unit :published-at)
               :export-addresses
               (mapcar (lambda (entry)
                         (list :name (car entry) :arity (cdr entry)
                               :address (cdr (assoc (car entry) gates))
                               :generation generation))
                       (plist-get unit :contract))))))))

(defun nelisp-native-unit-code-info (unit-id &optional name)
  "Return provenance of UNIT-ID's currently published code.

:generation, :source, :source-sha256, :artifact-sha256, :binary-sha256,
:published-at and :exports describe the generation the live control table
actually points at right now -- read the same way `nelisp-native-unit--table'
already does, never a cached host field.  :source-current recomputes the
source file's hash on disk right now and compares it with the recorded
one: t only when the file is readable and the hash matches, nil for a
mismatch, an unreadable file, a deleted file, or a unit that was never
published -- never invented as t.  :current-source-sha256 is that fresh
hash, or nil when it could not be computed.  :history is the bounded
publication log (`nelisp-native-unit-history-max' entries).

When NAME is given, :export-arity and :export-address add that export's
arity and stable entry address.

A unit that has never published anything (including one that does not
exist) answers with :published nil and every source/artifact field nil:
there is no identity to report, and none is invented."
  (let ((unit (nelisp-native-unit--find unit-id)))
    (if (null unit)
        (list :unit-id unit-id :generation 0 :published nil
              :source nil :source-sha256 nil :source-current nil
              :current-source-sha256 nil :artifact-sha256 nil
              :binary-sha256 nil :published-at nil :exports nil :history nil)
      (let* ((table (nelisp-native-unit--table unit))
             (published (/= table 0))
             (source (and published (plist-get unit :published-source)))
             (recorded-hash (and published (plist-get unit :published-source-sha256)))
             (current-hash (and source
                                (condition-case nil
                                    (nelisp-native-unit--hash source)
                                  (error nil))))
             (current-p (and published source current-hash
                             (equal current-hash recorded-hash)))
             (result
              (list :unit-id unit-id
                    :generation (nelisp-native-unit--generation table)
                    :published published
                    :source source
                    :source-sha256 recorded-hash
                    :source-current (and current-p t)
                    :current-source-sha256 current-hash
                    :artifact-sha256 (and published
                                          (plist-get unit :published-artifact-sha256))
                    :binary-sha256 (plist-get unit :binary-sha256)
                    :published-at (and published (plist-get unit :published-at))
                    :exports (plist-get unit :contract)
                    :history (plist-get unit :history))))
        (when name
          (setq result
                (append result
                        (list :export-arity (cdr (assoc name (plist-get unit :contract)))
                              :export-address (cdr (assoc name (plist-get unit :gates)))))))
        (nelisp-native-unit--copy result)))))

(defun nelisp-native-unit-resources ()
  "Report candidate/unit/retired counts and cumulative reclaim totals.

:retired counts, across every unit, retired generations still mapped
because no evidence shows their in-flight calls have returned (see
`nelisp-native-unit-reclaim'); :retained-bytes is their table pages'
total size. :reclaimed-tables and :reclaimed-bytes are cumulative for the
life of this process, not a live gauge: a table or artifact once released
stays released and is never counted again."
  (let ((retired 0) (retained-bytes 0))
    (dolist (entry nelisp-native-unit--units)
      (dolist (r (plist-get (cdr entry) :retired))
        (unless (plist-get r :released)
          (setq retired (1+ retired))
          (setq retained-bytes
                (+ retained-bytes nelisp-native-unit--table-bytes
                   (or (plist-get (plist-get r :handle) :code-size) 0))))))
    (nelisp-native-unit--copy
     (list :candidates (length nelisp-native-unit--candidates)
           :units (length nelisp-native-unit--units)
           :retired retired
           :retained-bytes retained-bytes
           :reclaimed-tables nelisp-native-unit--reclaimed-tables
           :reclaimed-bytes nelisp-native-unit--reclaimed-bytes
           :retained-reason
           (if (> retired 0)
               "no evidence that in-flight calls through a retired generation have returned; kept mapped"
             "none retained")))))

(defun nelisp-native-unit-reclaim (&optional unit-id)
  "Release every provably reclaimable resource; report what stayed.

Runs candidate TTL expiry first -- an expired candidate's table and
artifact are always reclaimable, and this is a convenient place to force
that instead of waiting for the next stage or publish call to notice it.

For UNIT-ID (or every unit, when nil), each still-mapped retired
generation is then either released, when this file can show no call is
in flight through it, or left mapped and reported in :refused with a
reason.  Today it can show that for none of them -- there is no runtime
counter of pending calls through a stable gate -- so :released is always
empty and every retired generation is refused with that reason; this is
the accounting and the refusal implemented honestly, not a guess dressed
up as success.  A generation that is currently active (reachable from a
unit's control word) is never in :retired and so is never a candidate
for release here."
  (nelisp-native-unit--purge)
  (let ((refused nil))
    (dolist (entry nelisp-native-unit--units)
      (when (or (null unit-id) (equal unit-id (car entry)))
        (dolist (r (plist-get (cdr entry) :retired))
          (unless (plist-get r :released)
            (push (list :unit-id (car entry) :generation (plist-get r :generation)
                       :reason "no evidence that in-flight calls through this retired generation have returned")
                  refused)))))
    (nelisp-native-unit--copy (list :released nil :refused (nreverse refused)))))

(defun nelisp-native-unit-address (unit-id name)
  "Return NAME's stable executable entry, available only after first publication."
  (let ((unit (nelisp-native-unit--find unit-id)))
    (unless (and unit (/= 0 (nelisp-native-unit--table unit)))
      (error "native-unit: unit is not published"))
    (or (cdr (assoc name (plist-get unit :gates)))
        (error "native-unit: unknown public export %s" name))))

(defun nelisp-native-unit-call (unit-id name args)
  "Call a stable public entry with up to six integer arguments; return a raw word."
  (let* ((unit (nelisp-native-unit--find unit-id))
         (entry (assoc name (plist-get unit :contract)))
         (arity (cdr entry)))
    (unless (and entry (proper-list-p args) (= arity (length args))
                 (cl-every #'integerp args))
      (error "native-unit: integer argument/arity mismatch"))
    (apply #'ptr-call (nelisp-native-unit-address unit-id name)
           (append args (make-list (- 6 arity) 0)))))

(provide 'nelisp-native-unit)
