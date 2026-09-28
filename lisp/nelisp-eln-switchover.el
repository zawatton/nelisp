;;; nelisp-eln-switchover.el --- route .neln symbols to proven GNU .eln -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 208 (docs/design/208-neln-to-eln-switchover.org), ledger S7.7.
;;
;; A routing layer between the old private `.neln' native path and the
;; genuine GNU `.eln' registration path (`nelisp-eln-registration-load').
;; It never changes what the registration path admits; it only decides,
;; per symbol, whether that path may be tried at all, publishes the result,
;; and records every decision:
;;
;; - `nelisp-eln-switchover-load' routes one SYMBOL to one `.eln' artifact
;;   when the artifact is inside proven coverage (a pinned genuine GNU
;;   artifact from `nelisp-eln-switchover-proven-genuine-artifacts', or a
;;   self-emitted artifact whose migration record matches its bytes and
;;   whose emitter kind is in `nelisp-eln-switchover-proven-emitter-kinds').
;;   Anything else, and any rejection by the loader, falls back to the
;;   caller's fallback installer (the `.neln' module-init definition / VM).
;;   The fallback is never silent: every call appends exactly one record to
;;   `nelisp-eln-switchover-log'.
;; - Registration always publishes into a fresh isolated namespace; this
;;   module is the single writer of the global function cell, so it can
;;   save the previous definition and restore it on unload.
;; - `nelisp-eln-switchover-unload' retracts a route (restores the previous
;;   definition and plist properties, drops every reference this layer and
;;   the registration owner hold to the native subr).
;;   `nelisp-eln-switchover-finalize-unloads' then releases the owner's
;;   native resources (unit views, vector/metadata tokens, link table,
;;   symbols-with-pos cell) and dlcloses the private copy -- but only once
;;   the runtime's weak native-subr lease count for that module is zero.
;;   A still-referenced subr defers the release instead of unmapping code
;;   something can still call.
;; - `nelisp-eln-switchover-migrate-neln' migrates an existing `.neln' cache:
;;   every module-init defun whose AOT IR the self-emitter accepts in a
;;   proven kind gets a genuine GNU-format `.eln' plus a sha256 record; the
;;   rest are recorded as fallback.

;;; Code:

(require 'nelisp-eln-system-loader)
(require 'nelisp-eln-registration)
(require 'nelisp-eln-registration-objects)

(define-error 'nelisp-eln-switchover-error "NeLisp .eln switchover error")

(declare-function nelisp-eln-registration-vectors-release-unit
                  "nelisp-eln-registration-vectors" (token))
(declare-function nelisp-eln-registration-metadata-release
                  "nelisp-eln-registration-metadata" (token))
(declare-function nelisp-eln-emitter-write-ir "nelisp-eln-emitter"
                  (ir output &optional profile))
(declare-function nelisp-eln-emitter--validate-ir "nelisp-eln-emitter" (ir))
(declare-function nelisp-aot-compiler--parse-stmt "nelisp-aot-compiler"
                  (form a b c))

(defconst nelisp-eln-switchover-proven-genuine-artifacts
  '(("6a363298478b1f3b5e3f860a7f353e110e161b8a77d66d945c13c9ea7e62253a"
     :name nelisp-gnu-identity :publish global
     :evidence ("S1.3" "S5.8" "S7.7.4"))
    ("4a799ba0c69bdb6549a3bcb785e3e7878797beb2e8bd99326248d74b24e67dd2"
     :name caar :publish isolated :evidence ("S6.17" "S7.7.4"))
    ("7a4f2fad522ed4b1bdf6a2f0d7fd68ab2c8cd12e7d9917dd50555a61e88458c3"
     :name cadr :publish isolated :evidence ("S6.18" "S7.7.4"))
    ("ab5d63ed090612603b890b0e378ec644cdb58d79913947edd7e8a2e3eef0c122"
     :name fixnump :publish isolated :evidence ("S6.19" "S7.7.4"))
    ("d350b1fe060480a3ad37821ab368b276d413d9ac76b75b259c94ae2edaeb959d"
     :name bignump :publish isolated :evidence ("S6.20" "S7.7.4"))
    ("1536813e3156dc6d560315d4360093936f2d033e552daf601792a7118d475961"
     :name frame-configuration-p :publish isolated
     :evidence ("S6.21" "S7.7.4")))
  "Genuine GNU 31.1 (ABI ba35c031) artifacts inside proven coverage.
Keyed by the exact file sha256.  Each entry names the symbol the artifact
registers, where it may be published, and the ledger criteria that prove
both its native execution (host/VM/native equality or identity lane) and
the rejection of its corrupted copies (S7.7.4).  `isolated' entries are
runtime-owned names (the loader itself calls `caar', `cadr', ...): they
are proven only through an isolated namespace and are never published into
the global function cell.  Artifacts whose lane is still red (increment
S2.5, zerop S6.16) are deliberately absent.")

(defconst nelisp-eln-switchover-proven-emitter-kinds '(constant identity expression)
  "Self-emitter kinds (`nelisp-eln-emitter--validate-ir') inside proven
coverage.  `constant' is the S7.6 / S7.7.4 self-emitted fixture shape;
`identity' and `expression' are proven by the S7.7 switchover smoke
\(native result equal to the `.neln' module-init result) plus the
registration preflight's exact-shape leaf verifier.")

(defconst nelisp-eln-switchover-migration-format 'nelisp-eln-switchover-migration-v1)

(defvar nelisp-eln-switchover-log nil
  "Every routing/unload decision made in this process, newest first.
Each record is a plist with at least :op, :symbol and :route or :result.")

(defvar nelisp-eln-switchover--entries nil
  "Alist (SYMBOL . ENTRY) of live or retracted routes.")

(defvar nelisp-eln-switchover--pending-unloads nil
  "Retracted entries whose native resources are not released yet.")

(defun nelisp-eln-switchover--record (plist)
  (setq nelisp-eln-switchover-log (cons plist nelisp-eln-switchover-log))
  plist)

(defun nelisp-eln-switchover-log-for (symbol)
  "Return every recorded decision for SYMBOL, oldest first."
  (let ((out nil))
    (dolist (rec nelisp-eln-switchover-log)
      (when (eq (plist-get rec :symbol) symbol)
        (setq out (cons rec out))))
    out))

(defun nelisp-eln-switchover-entry (symbol)
  "Return SYMBOL's route entry plist, or nil."
  (cdr (assq symbol nelisp-eln-switchover--entries)))

(defun nelisp-eln-switchover-function (symbol)
  "Return SYMBOL's routed native callable, or nil when not routed live.
Isolated routes are reachable only through this function."
  (let ((entry (nelisp-eln-switchover-entry symbol)))
    (and entry (eq (plist-get entry :state) 'live)
         (plist-get entry :callable))))

(defun nelisp-eln-switchover--set-entry (symbol entry)
  (setq nelisp-eln-switchover--entries
        (cons (cons symbol entry)
              (assq-delete-all symbol nelisp-eln-switchover--entries)))
  entry)

(defun nelisp-eln-switchover--registry-blocked ()
  "Return the registration layer's blocking state, or nil when usable."
  (or (and nelisp-eln-registration--pending-cleanups 'registry-quarantined)
      (and nelisp-eln-registration--active-owner 'registration-in-progress)))

(defun nelisp-eln-switchover--coverage (symbol bytes coverage)
  "Classify BYTES for SYMBOL; return (IN-P REASON . PLIST).
COVERAGE is nil or a migration entry plist (:sha256 :kind)."
  (let* ((sha (nelisp-eln-system-loader--file-sha256 bytes))
         (genuine (cdr (assoc sha nelisp-eln-switchover-proven-genuine-artifacts))))
    (cond
     (genuine
      (if (eq (plist-get genuine :name) symbol)
          (list t 'genuine-pinned :sha256 sha
                :publish (plist-get genuine :publish)
                :evidence (plist-get genuine :evidence))
        (list nil 'genuine-name-mismatch :sha256 sha
              :expected (plist-get genuine :name))))
     ((null coverage)
      (list nil 'outside-proven-coverage :sha256 sha))
     ((not (equal (plist-get coverage :sha256) sha))
      (list nil 'artifact-hash-mismatch :sha256 sha
            :expected (plist-get coverage :sha256)))
     ((not (memq (plist-get coverage :kind)
                 nelisp-eln-switchover-proven-emitter-kinds))
      (list nil 'emitter-kind-not-proven :sha256 sha
            :kind (plist-get coverage :kind)))
     (t
      (list t 'self-emitted-migrated :sha256 sha :publish 'global
            :kind (plist-get coverage :kind)
            :evidence '("S7.7" "S7.7.4"))))))

(defun nelisp-eln-switchover--fallback (symbol path reason detail fallback)
  "Record a fallback decision for SYMBOL and run FALLBACK when non-nil."
  (let ((installed nil) (fallback-error nil))
    (when fallback
      (condition-case err
          (progn (funcall fallback) (setq installed t))
        (error
         (setq fallback-error err)
         (nelisp-eln-switchover--record
          (list :op 'fallback-install :symbol symbol :result 'failed
                :detail err)))))
    (nelisp-eln-switchover--record
     (list :op 'load :symbol symbol :path path :route 'fallback
           :reason reason :detail detail
           :fallback-installed installed :fallback-error fallback-error))
    nil))

(defun nelisp-eln-switchover--find-owner (callable)
  (let ((found nil))
    (dolist (owner nelisp-eln-registration--owners)
      (when (and (not found) (vectorp owner)
                 (= (length owner) nelisp-eln-registration--owner-size)
                 (eq (aref owner 9) callable))
        (setq found owner)))
    found))

(defun nelisp-eln-switchover-load (symbol path &optional coverage fallback)
  "Route SYMBOL to the GNU `.eln' at PATH when inside proven coverage.
COVERAGE is the symbol's migration record (see
`nelisp-eln-switchover-migrate-neln') or nil for pinned genuine artifacts.
FALLBACK, when non-nil, is a no-argument function that installs the
non-native definition; it runs on every non-`eln' outcome.  Return the
route entry plist on success, or nil after a recorded fallback.  A `quit'
is recorded and re-signalled; every `error' becomes a recorded fallback."
  (let ((blocked (nelisp-eln-switchover--registry-blocked))
        (existing (nelisp-eln-switchover-entry symbol)))
    (cond
     ((not (and symbol (symbolp symbol)))
      (nelisp-eln-switchover--fallback symbol path 'invalid-symbol nil fallback))
     ((and existing (memq (plist-get existing :state) '(live retracted)))
      (nelisp-eln-switchover--fallback
       symbol path 'already-routed (plist-get existing :state) fallback))
     (blocked
      (nelisp-eln-switchover--fallback symbol path blocked nil fallback))
     ((not (and (stringp path) (file-readable-p path)))
      (nelisp-eln-switchover--fallback symbol path 'missing-artifact nil fallback))
     (t
      (let* ((class (nelisp-eln-switchover--coverage
                     symbol (nelisp-eln-system-loader--read-file path) coverage))
             (info (cddr class)))
        (if (not (car class))
            (nelisp-eln-switchover--fallback
             symbol path (cadr class) info fallback)
          (nelisp-eln-switchover--register
           symbol path
           (append class (list :form (plist-get coverage :form)))
           fallback)))))))

(defun nelisp-eln-switchover--register (symbol path class fallback)
  (let* ((info (cddr class))
         (ns (nelisp-eln-registration-make-isolated-namespace))
         (result nil) (failure nil))
    (condition-case err
        (let ((nelisp-eln-registration-isolated-namespace ns))
          (setq result (nelisp-eln-registration-load path)))
      (quit
       (nelisp-eln-switchover--record
        (list :op 'load :symbol symbol :path path :route 'fallback
              :reason 'quit :detail nil :fallback-installed nil))
       (signal (car err) (cdr err)))
      (error
       (setq failure t)
       ;; Recorded here, in the handler: `nelisp-eln-switchover--fallback'
       ;; appends the decision (with the loader's error) to the log.
       (nelisp-eln-switchover--fallback
        symbol path
        (if nelisp-eln-registration--pending-cleanups
            'registration-rejected-quarantined
          'registration-rejected)
        err fallback)))
    (cond
     (failure nil)
     ((not (and (plist-get result :success)
                (eq (plist-get result :name) symbol)
                (null (plist-get result :name2))))
      ;; Admitted, but not the single registration of SYMBOL this route
      ;; asked for.  Nothing was published globally; release it again.
      (let ((entry (nelisp-eln-switchover--make-entry
                    symbol path info ns (plist-get result :callable) nil)))
        (nelisp-eln-switchover--set-entry symbol entry)
        (nelisp-eln-switchover--retract symbol entry)
        (nelisp-eln-switchover--fallback
         symbol path 'registered-name-mismatch
         (list :name (plist-get result :name) :name2 (plist-get result :name2))
         fallback)))
     (t
      (let* ((callable (nelisp-eln-registration-isolated-function ns symbol))
             (publish (plist-get info :publish))
             (previous (and (eq publish 'global)
                            (cons (fboundp symbol)
                                  (and (fboundp symbol)
                                       (symbol-function symbol)))))
             (props nil)
             (entry (nelisp-eln-switchover--make-entry
                     symbol path info ns callable previous))
             (guard (and (eq publish 'global)
                         (nelisp-eln-switchover--make-guard
                          symbol callable
                          (nelisp-eln-switchover--per-call-fallback
                           previous (plist-get info :form))
                          (plist-get entry :counters)))))
        (when (eq publish 'global)
          (dolist (pair (let ((cell (assq symbol (aref ns 2))) (plist nil) (out nil))
                          (setq plist (cdr cell))
                          (while plist
                            (setq out (cons (cons (car plist) (cadr plist)) out)
                                  plist (cddr plist)))
                          out))
            (setq props (cons (list (car pair) (get symbol (car pair)))
                              props))
            (put symbol (car pair) (cdr pair)))
          (setq entry (plist-put entry :saved-props props))
          (setq entry (plist-put entry :published guard))
          (fset symbol guard))
        (nelisp-eln-switchover--set-entry symbol entry)
        (nelisp-eln-switchover--record
         (list :op 'load :symbol symbol :path path :route 'eln
               :reason (cadr class) :publish publish
               :sha256 (plist-get info :sha256)
               :evidence (plist-get info :evidence)
               :module-id (plist-get entry :module-id)))
        entry)))))

(defun nelisp-eln-switchover--make-entry (symbol path info ns callable previous)
  (let* ((owner (and callable (nelisp-eln-switchover--find-owner callable)))
         (unit (and owner (aref owner 1)))
         (handle (and (vectorp unit) (aref unit 1))))
    (list :symbol symbol :path path :state 'live
          :publish (plist-get info :publish) :sha256 (plist-get info :sha256)
          :namespace ns :callable callable :owner owner :handle handle
          :module-id (and handle (nelisp-eln-system-loader-module-id handle))
          :previous previous
          ;; [native-calls per-call-fallbacks]
          :counters (vector 0 0))))

(defun nelisp-eln-switchover--per-call-fallback (previous form)
  "Return the non-native function a guarded call falls back to, or nil.
The previous definition wins; otherwise FORM (the `.neln' module-init
defun) is turned into a closure without touching any function cell."
  (cond
   ((and (car previous) (functionp (cdr previous))) (cdr previous))
   ((and (consp form) (eq (car form) 'defun) (listp (nth 2 form)))
    (eval (list 'function (cons 'lambda (nthcdr 2 form))) t))))

(defun nelisp-eln-switchover--make-guard (symbol native fallback counters)
  "Return SYMBOL's published function: NATIVE behind a per-call coverage guard.
Every admitted global route is a pure leaf (the registration preflight's
scalar0 / bounded unary-leaf grammar: no calls, no stores), and the object
codec refuses an argument or result it cannot represent with
`nelisp-eln-objects-unsupported' -- for an argument, before native entry.
Such a call is outside proven argument coverage: it is counted, recorded
once per symbol and error kind, and answered by FALLBACK (the previous
definition), never by a guessed value.  Any other error propagates."
  (lambda (&rest args)
    (condition-case err
        (prog1 (apply native args)
          (aset counters 0 (1+ (aref counters 0))))
      (nelisp-eln-objects-unsupported
       (aset counters 1 (1+ (aref counters 1)))
       (nelisp-eln-switchover--record-call-fallback symbol err)
       (if fallback
           (apply fallback args)
         (signal (car err) (cdr err)))))))

(defun nelisp-eln-switchover--record-call-fallback (symbol err)
  (let ((kind (list (car err) (car-safe (cdr err)))) (seen nil))
    (dolist (rec nelisp-eln-switchover-log)
      (when (and (eq (plist-get rec :op) 'call-fallback)
                 (eq (plist-get rec :symbol) symbol)
                 (equal (plist-get rec :kind) kind))
        (setq seen t)))
    (unless seen
      (nelisp-eln-switchover--record
       (list :op 'call-fallback :symbol symbol :kind kind :detail err)))))

(defun nelisp-eln-switchover-counters (symbol)
  "Return (NATIVE-CALLS . PER-CALL-FALLBACKS) for SYMBOL's route, or nil."
  (let ((counters (plist-get (nelisp-eln-switchover-entry symbol) :counters)))
    (and counters (cons (aref counters 0) (aref counters 1)))))

(defun nelisp-eln-switchover-load-batch (specs)
  "Route every (SYMBOL PATH COVERAGE FALLBACK) in SPECS, in order.
One item's rejection, fallback or internal error never stops the batch;
return the list of (SYMBOL . ROUTE) outcomes."
  (let ((outcomes nil))
    (dolist (spec specs)
      (let ((symbol (nth 0 spec)))
        (condition-case err
            (setq outcomes
                  (cons (cons symbol
                              (if (nelisp-eln-switchover-load
                                   symbol (nth 1 spec) (nth 2 spec) (nth 3 spec))
                                  'eln
                                'fallback))
                        outcomes))
          (error
           (nelisp-eln-switchover--record
            (list :op 'load :symbol symbol :path (nth 1 spec)
                  :route 'fallback :reason 'router-error :detail err
                  :fallback-installed nil))
           (setq outcomes (cons (cons symbol 'fallback) outcomes))))))
    (nreverse outcomes)))

;;; Unload.

(defun nelisp-eln-switchover--retract (symbol entry)
  "Drop every reference this layer and ENTRY's owner hold to its callable."
  (let ((previous (plist-get entry :previous))
        (ns (plist-get entry :namespace))
        (owner (plist-get entry :owner))
        (superseded nil))
    (when (eq (plist-get entry :publish) 'global)
      (if (and (fboundp symbol)
               (eq (symbol-function symbol) (plist-get entry :published)))
          (progn
            (if (car previous)
                (fset symbol (cdr previous))
              (fmakunbound symbol))
            (dolist (saved (plist-get entry :saved-props))
              (put symbol (car saved) (cadr saved))))
        (setq superseded t)))
    (when (vectorp ns)
      (aset ns 1 nil)
      (aset ns 2 nil))
    (when owner
      (aset owner 9 nil)
      (aset owner 19 nil))
    (setq entry (plist-put entry :callable nil))
    (setq entry (plist-put entry :published nil))
    (setq entry (plist-put entry :namespace nil))
    (setq entry (plist-put entry :state 'retracted))
    (setq entry (plist-put entry :superseded superseded))
    (nelisp-eln-switchover--set-entry symbol entry)
    (setq nelisp-eln-switchover--pending-unloads
          (append nelisp-eln-switchover--pending-unloads (list symbol)))
    entry))

(defun nelisp-eln-switchover-unload (symbol)
  "Retract SYMBOL's live route; restore its previous definition.
The native resources are released by `nelisp-eln-switchover-finalize-unloads',
which must run from a frame that no longer holds the callable.  Return
`retracted', or nil (recorded) when SYMBOL has no live route."
  (let ((entry (nelisp-eln-switchover-entry symbol)))
    (if (not (and entry (eq (plist-get entry :state) 'live)))
        (progn
          (nelisp-eln-switchover--record
           (list :op 'unload :symbol symbol :result 'not-routed))
          nil)
      (setq entry (nelisp-eln-switchover--retract symbol entry))
      (nelisp-eln-switchover--record
       (list :op 'unload :symbol symbol :result 'retracted
             :superseded (plist-get entry :superseded)
             :module-id (plist-get entry :module-id)))
      'retracted)))

(defun nelisp-eln-switchover--live-count (entry)
  (let ((module-id (plist-get entry :module-id)))
    (if (and module-id (fboundp 'nelisp--native-subr-live-count))
        (nelisp--native-subr-live-count module-id)
      0)))

(defun nelisp-eln-switchover--release-owner (owner)
  "Release a successfully registered OWNER's native resources.
Mirrors the resource order of `nelisp-eln-registration--cleanup' for the
state `nelisp-eln-registration--finish-success' leaves behind: the
activations are already retired, so only the persistent unit views, the
metadata/vector tokens, the link table and the symbols-with-pos cell
remain.  The unit release dlcloses the private copy and itself refuses
while the module still has live native subrs."
  (let ((unit (aref owner 1))
        (vector-unit (aref owner 10))
        (link-table (aref owner 12))
        (plist (aref owner 18)))
    (aset owner 17 nil)
    (when vector-unit
      (require 'nelisp-eln-registration-vectors)
      (nelisp-eln-registration-vectors-release-unit vector-unit)
      (aset owner 10 nil))
    (when (and unit (not (eq (aref unit 5) 'closed)))
      (nelisp-eln-registration-objects-release-unit unit))
    (when (aref owner 16)
      (require 'nelisp-eln-registration-metadata)
      (nelisp-eln-registration-metadata-release (aref owner 16))
      (aset owner 16 nil))
    (when link-table
      (nl-ffi-memory-release link-table)
      (aset owner 12 nil))
    (let ((memory (plist-get plist :symbols-with-pos-memory)))
      (when memory
        (aset owner 18 (plist-put plist :symbols-with-pos-memory nil))
        (nl-ffi-memory-release memory)))
    (setq nelisp-eln-registration--owners
          (delq owner nelisp-eln-registration--owners))
    t))

(defun nelisp-eln-switchover-finalize-unloads ()
  "Release every retracted route whose module has no live native subr.
Runs a GC first so unreachable subrs drop their weak leases.  A module
whose subr is still reachable is deferred (kept mapped, retried on the
next call); a release that signals is recorded as `release-failed' and
its owner stays rooted.  Return (:released N :deferred N :failed N)."
  (garbage-collect)
  (let ((released 0) (deferred 0) (failed 0) (remaining nil))
    (dolist (symbol nelisp-eln-switchover--pending-unloads)
      (let* ((entry (nelisp-eln-switchover-entry symbol))
             (owner (plist-get entry :owner))
             (live (nelisp-eln-switchover--live-count entry)))
        (cond
         ((> live 0)
          (setq deferred (1+ deferred) remaining (cons symbol remaining))
          (nelisp-eln-switchover--record
           (list :op 'finalize :symbol symbol :result 'deferred
                 :live-subrs live :module-id (plist-get entry :module-id))))
         (t
          (condition-case err
              (progn
                (when owner (nelisp-eln-switchover--release-owner owner))
                (setq entry (plist-put entry :state 'unloaded))
                (setq entry (plist-put entry :owner nil))
                (setq entry (plist-put entry :handle-released
                                       (and (plist-get entry :handle)
                                            (null (gethash
                                                   (plist-get entry :handle)
                                                   nelisp-eln-system-loader--handles)))))
                (nelisp-eln-switchover--set-entry symbol entry)
                (setq released (1+ released))
                (nelisp-eln-switchover--record
                 (list :op 'finalize :symbol symbol :result 'released
                       :module-id (plist-get entry :module-id))))
            (error
             (setq failed (1+ failed))
             (setq entry (plist-put entry :state 'release-failed))
             (setq entry (plist-put entry :error err))
             (nelisp-eln-switchover--set-entry symbol entry)
             (nelisp-eln-switchover--record
              (list :op 'finalize :symbol symbol :result 'release-failed
                    :error err :module-id (plist-get entry :module-id)))))))))
    (setq nelisp-eln-switchover--pending-unloads (nreverse remaining))
    ;; An unloaded route is forgotten so the symbol can be routed again.
    (dolist (cell nelisp-eln-switchover--entries)
      (when (eq (plist-get (cdr cell) :state) 'unloaded)
        (setq nelisp-eln-switchover--entries
              (delq cell nelisp-eln-switchover--entries))))
    (list :released released :deferred deferred :failed failed)))

(defun nelisp-eln-switchover-report ()
  "Return a plist combining route state with the registration crash report."
  (let ((live 0) (retracted 0) (failed 0))
    (dolist (cell nelisp-eln-switchover--entries)
      (pcase (plist-get (cdr cell) :state)
        ('live (setq live (1+ live)))
        ('retracted (setq retracted (1+ retracted)))
        ('release-failed (setq failed (1+ failed)))))
    (append (list :live-routes live :retracted-routes retracted
                  :release-failed failed
                  :pending-unloads (length nelisp-eln-switchover--pending-unloads)
                  :open-handles (hash-table-count
                                 nelisp-eln-system-loader--handles))
            (nelisp-eln-registration-crash-boundary-report))))

;;; Migration of existing `.neln' caches.

(defun nelisp-eln-switchover--read-sexp-file (path &optional skip-first-line)
  (with-temp-buffer
    (insert-file-contents path)
    (goto-char (point-min))
    (when skip-first-line (forward-line 1))
    (read (current-buffer))))

(defun nelisp-eln-switchover--neln-defuns (neln-path)
  "Return (NAME . DEFUN-FORM) for NELN-PATH's module-init defuns."
  (let* ((payload (nelisp-eln-switchover--read-sexp-file neln-path t))
         (out nil))
    (unless (and (listp payload) (eq (plist-get payload :kind) 'neln))
      (signal 'nelisp-eln-switchover-error (list 'not-a-neln-payload neln-path)))
    (dolist (item (plist-get payload :module-init))
      (when (and (consp item) (eq (car item) :fn) (symbolp (nth 1 item)))
        (let ((form (nth 3 item)))
          (when (and (consp form) (eq (car form) 'defun)
                     (eq (nth 1 form) (nth 1 item)))
            (setq out (cons (cons (nth 1 item) form) out))))))
    (nreverse out)))

(defun nelisp-eln-switchover--emitter-kind (form)
  "Return (KIND . IR) when the self-emitter accepts FORM, else (nil . REASON)."
  (condition-case err
      (let* ((ir (nelisp-aot-compiler--parse-stmt form nil nil nil))
             (entry (nelisp-eln-emitter--validate-ir ir)))
        (cons (nth 2 entry) ir))
    (error
     (nelisp-eln-switchover--record
      (list :op 'migrate :symbol (nth 1 form) :result 'emitter-refused
            :detail err))
     (cons nil err))))

(defun nelisp-eln-switchover-migrate-neln (neln-path eln-dir)
  "Migrate the existing `.neln' cache NELN-PATH into ELN-DIR.
The cache must be self-consistent (its sibling manifest's
:artifact-sha256 equals the file) or nothing is migrated.  Every
module-init defun whose IR the self-emitter accepts in a proven kind is
emitted as a genuine GNU-format `.eln'; every other defun is recorded as
a fallback entry.  The migration record is written next to the emitted
artifacts and returned."
  (require 'nelisp-aot-compiler)
  (require 'nelisp-eln-emitter)
  (let* ((neln-path (expand-file-name neln-path))
         (manifest-path (concat neln-path ".manifest.el"))
         (manifest (and (file-readable-p manifest-path)
                        (nelisp-eln-switchover--read-sexp-file manifest-path)))
         (neln-sha (and (file-readable-p neln-path)
                        (nelisp-eln-system-loader--file-sha256
                         (nelisp-eln-system-loader--read-file neln-path))))
         (entries nil))
    (unless (and manifest (eq (plist-get manifest :kind) 'neln)
                 (equal (plist-get manifest :artifact-sha256) neln-sha))
      (signal 'nelisp-eln-switchover-error
              (list 'neln-cache-not-self-consistent neln-path)))
    (make-directory eln-dir t)
    (dolist (cell (nelisp-eln-switchover--neln-defuns neln-path))
      (let* ((name (car cell))
             (kind (nelisp-eln-switchover--emitter-kind (cdr cell))))
        (if (not (memq (car kind) nelisp-eln-switchover-proven-emitter-kinds))
            (setq entries
                  (cons (list :symbol name :route 'fallback
                              :reason (if (car kind) 'emitter-kind-not-proven
                                        'emitter-unsupported-shape)
                              :detail (format "%S" (cdr kind))
                              :form (cdr cell))
                        entries))
          (let ((out (expand-file-name
                      (format "%s-%s.eln" name (substring neln-sha 0 12))
                      eln-dir)))
            (when (file-exists-p out) (delete-file out))
            (condition-case err
                (progn
                  (nelisp-eln-emitter-write-ir (cdr kind) out)
                  (setq entries
                        (cons (list :symbol name :route 'eln :kind (car kind)
                                    :eln out
                                    :sha256 (nelisp-eln-system-loader--file-sha256
                                             (nelisp-eln-system-loader--read-file out))
                                    :form (cdr cell))
                              entries)))
              (error
               (nelisp-eln-switchover--record
                (list :op 'migrate :symbol name :result 'emission-failed
                      :detail err))
               (setq entries
                     (cons (list :symbol name :route 'fallback
                                 :reason 'emission-failed
                                 :detail (format "%S" err) :form (cdr cell))
                           entries))))))))
    (let* ((migration (list :format nelisp-eln-switchover-migration-format
                            :neln neln-path :neln-sha256 neln-sha
                            :source-sha256 (plist-get (plist-get manifest :source)
                                                      :sha256)
                            :entries (nreverse entries)))
           (record (expand-file-name
                    (concat (file-name-nondirectory neln-path) ".switchover.el")
                    eln-dir)))
      (with-temp-file record
        (let ((print-length nil) (print-level nil))
          (prin1 migration (current-buffer))
          (insert "\n")))
      (plist-put migration :record record))))

(defun nelisp-eln-switchover-read-migration (record)
  "Read and structurally validate the migration RECORD file."
  (let ((migration (nelisp-eln-switchover--read-sexp-file record)))
    (unless (and (listp migration)
                 (eq (plist-get migration :format)
                     nelisp-eln-switchover-migration-format))
      (signal 'nelisp-eln-switchover-error (list 'invalid-migration record)))
    migration))

(defun nelisp-eln-switchover-load-migrated (migration &optional fallback-fn)
  "Route every entry of MIGRATION as one batch.
The migration is only trusted while its `.neln' still has the recorded
sha256; otherwise every symbol falls back with `stale-migration'.
FALLBACK-FN is called with (SYMBOL FORM) to install a non-native
definition; the default evaluates the `.neln' module-init defun form."
  (let* ((fallback-fn (or fallback-fn
                          (lambda (_symbol form) (eval form t))))
         (neln (plist-get migration :neln))
         (fresh (and (stringp neln) (file-readable-p neln)
                     (equal (nelisp-eln-system-loader--file-sha256
                             (nelisp-eln-system-loader--read-file neln))
                            (plist-get migration :neln-sha256))))
         (specs nil))
    (dolist (entry (plist-get migration :entries))
      (let* ((symbol (plist-get entry :symbol))
             (form (plist-get entry :form))
             (fallback (lambda () (funcall fallback-fn symbol form))))
        (cond
         ((not fresh)
          (nelisp-eln-switchover--fallback
           symbol (plist-get entry :eln) 'stale-migration neln fallback))
         ((eq (plist-get entry :route) 'eln)
          (setq specs (cons (list symbol (plist-get entry :eln) entry fallback)
                            specs)))
         (t
          (nelisp-eln-switchover--fallback
           symbol nil (plist-get entry :reason) (plist-get entry :detail)
           fallback)))))
    (nelisp-eln-switchover-load-batch (nreverse specs))))

;;; `.neln' loader hook.

(defun nelisp-eln-switchover-neln-router (symbol eln)
  "Route SYMBOL for the `.neln' loader (`nelisp-artifact-neln-eln-router').
ELN is a pinned genuine artifact path or a migration entry plist.  No
fallback installer is passed: the `.neln' module-init replay has already
installed SYMBOL's non-native definition, which stays in place (and is
restored on unload) whenever the route is refused or rejected."
  (if (stringp eln)
      (nelisp-eln-switchover-load symbol eln nil nil)
    (nelisp-eln-switchover-load symbol (plist-get eln :eln) eln nil)))

(defun nelisp-eln-switchover-neln-eln-table (migration)
  "Return an alist (SYMBOL . ENTRY) of MIGRATION's `.eln' entries, suitable
for `nelisp-artifact-neln-eln-table' together with
`nelisp-eln-switchover-neln-router'."
  (let ((out nil))
    (dolist (entry (plist-get migration :entries))
      (when (eq (plist-get entry :route) 'eln)
        (setq out (cons (cons (plist-get entry :symbol) entry) out))))
    (nreverse out)))

(provide 'nelisp-eln-switchover)

;;; nelisp-eln-switchover.el ends here
