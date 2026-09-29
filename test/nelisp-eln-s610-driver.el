;;; nelisp-eln-s610-driver.el --- Doc 210 S10 driver -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Runs on a NeLisp standalone binary (NELISP_READER_DYNAMIC flavor).
;; Environment: NELISP_ROOT, NELISP_S10_ELN (the genuine
;; gnu-byte-compile-form.eln), NELISP_S10_TESTDIR, NELISP_S10_MODE
;; (scenarios | forced | tamper | mutation), NELISP_S10_RED (non-empty:
;; pre-change control, native handlers disabled), NELISP_S10_WORKDIR
;; (scratch directory of the mutation mode).
;;
;; scenarios  The artifact goes through the ordinary registration path (this
;;   driver sets up nothing of the handler substrate: the loader binds it)
;;   and test/nelisp-eln-s610-scenarios.el runs against it; the `T '
;;   transcript must equal host GNU Emacs 31.1's byte for byte.  After every
;;   scenario the shadow handler chain is back at the sentinel and no handler
;;   block or specpdl entry is left.  In the genuine body the guarded regions
;;   are unreachable (the `any-value' variable of the inlined
;;   `macroexp--const-symbol-p' is the constant nil, see NOTES), so no
;;   push_handler happens here.
;; forced     The same scenarios with the live `d_reloc' constant of nil
;;   (slot 2) made non-nil by this test, which makes the genuine, unmodified
;;   guarded regions reachable: the native `condition-case' now really runs
;;   (push_handler, the private `_setjmp', the guarded Fsymbol_value/Fset,
;;   the landing pad, the pops).  The transcript still equals the host's; the
;;   push_handler and landing counts are the expected ones; with native
;;   handlers disabled (NELISP_S10_RED) the pushes fail and the counts differ.
;; tamper     Slots the loader authenticates (link table, `_setjmp' GOT word,
;;   `current_thread_reloc', the freloc cell) are tampered with one at a time:
;;   the call fails closed before native code runs, and works again once the
;;   slot is restored.
;; mutation   Single-byte mutants of the artifact are refused by the loader.

;;; Code:

(let ((repo (getenv "NELISP_ROOT")))
  (add-to-list 'load-path (expand-file-name "lisp" repo))
  (add-to-list 'load-path (expand-file-name "packages/nl-ffi/src" repo)))
(require 'cl-lib)
(require 'nelisp-eln-registration)
(require 'nelisp-eln-handler-port)
(require 'nelisp-eln-tail-code)
(require 'nelisp-eln-handler-frame)

(defvar s10-driver--mode (getenv "NELISP_S10_MODE"))
(defvar s10-driver--eln (getenv "NELISP_S10_ELN"))
(defvar s10-driver--testdir (getenv "NELISP_S10_TESTDIR"))
(defvar s10-driver--workdir (getenv "NELISP_S10_WORKDIR"))
(defvar s10-driver--red (let ((v (getenv "NELISP_S10_RED"))) (and v (> (length v) 0))))
(defvar s10-driver--pushes 0)
(defvar s10-driver--last-pushes 0)
(defvar s10-driver--last-landings 0)
(defvar s10-driver--forced nil)

(defun s10-driver--pass (name)
  (princ (format "S10_%s=PASS\n" name)))

(defun s10-driver--check (name condition &optional detail)
  (unless condition (error "S10 check failed: %s %S" name detail))
  (s10-driver--pass name))

(defun s10-driver--signals (name thunk reasons)
  "Check that THUNK signals an error whose first data element is one of REASONS."
  (let ((got (condition-case failure (progn (funcall thunk) 'no-error)
               (error (cadr failure)))))
    (unless (memq got reasons)
      (error "S10 negative %s: wanted one of %S, got %S" name reasons got))
    (s10-driver--pass name)))

;;;; Counters and the pre-change control -------------------------------

(defun s10-driver--install-counters ()
  (let ((orig (symbol-function 'nelisp-eln-handler-port-push)))
    (fset 'nelisp-eln-handler-port-push
          (lambda (&rest arguments)
            (setq s10-driver--pushes (1+ s10-driver--pushes))
            (apply orig arguments)))))

(defun s10-driver--disable-handlers ()
  "Pre-change control: drop the handler options of every native call."
  (let ((orig (symbol-function 'nelisp-eln-callable-import--call-unary)))
    (fset 'nelisp-eln-callable-import--call-unary
          (lambda (&rest arguments)
            (apply orig (cl-subseq arguments 0 (min 8 (length arguments))))))))

;;;; The loaded artifact -------------------------------------------------

(defun s10-driver--handle ()
  "Return the loader handle of the one registered artifact."
  (let ((owners nelisp-eln-registration--owners))
    (unless (= (length owners) 1)
      (error "expected exactly one registered artifact, have %d" (length owners)))
    (aref (aref (car owners) 1) 1)))

(defun s10-driver--symbol-address (handle name)
  (plist-get (nelisp-eln-system-loader-symbol-info handle name) :address))

(defun s10-driver--force-region-entry ()
  "Make the guarded regions reachable (test only, see the file commentary).
The inlined `macroexp--const-symbol-p' tests its `any-value' variable, which
the genuine body reads from the artifact's own `d_reloc' slot 2 (the
constant nil); the live slot is made non-nil.  No code byte is touched."
  (let* ((handle (s10-driver--handle))
         (d-reloc (s10-driver--symbol-address handle "d_reloc")))
    (s10-driver--check "FORCE_SLOT_IS_NIL" (= (ptr-read-u64 d-reloc 16) 0))
    (ptr-write-u64 d-reloc 16 (ptr-read-u64 d-reloc (* 8 51)))
    (s10-driver--check "FORCE_SLOT_NOW_NON_NIL" (/= (ptr-read-u64 d-reloc 16) 0))
    (setq s10-driver--forced t)))

;;;; Platform layer for the shared scenarios ---------------------------

(defun s10-prepare ()
  ;; GNU makes `most-positive-fixnum' a constant symbol (its `set' signals
  ;; `setting-constant'); NeLisp flags constants the same way for `defconst'.
  (nelisp--env-globals-set-constant 'most-positive-fixnum t))

(defconst s10-driver--expected-forced
  '(("constant" 0 . 0)
    ("bound-symbol-head" 1 . 0)
    ("constant-symbol-head" 1 . 1)
    ("constant-symbol-head-twice" 2 . 2)
    ("error-restores-specbinds" 0 . 0)
    ("throw-restores-specbinds" 0 . 0)
    ("quit-restores-specbinds" 0 . 0)
    ("caught-then-error-restores" 1 . 1)
    ("mixed-forms" 0 . 0)
    ("constant-after" 0 . 0))
  "(SCENARIO PUSHES . LANDINGS) once the guarded regions are reachable.")

(defun s10-after (name)
  (unless s10-driver--red
    (let* ((expected (if s10-driver--forced
                         (cdr (assoc name s10-driver--expected-forced))
                       '(0 . 0)))
           (pushes (- s10-driver--pushes s10-driver--last-pushes))
           (landings (- (nelisp-eln-handler-port-landing-count)
                        s10-driver--last-landings))
           (label (upcase (replace-regexp-in-string "-" "_" name))))
      (setq s10-driver--last-pushes s10-driver--pushes
            s10-driver--last-landings (nelisp-eln-handler-port-landing-count))
      (s10-driver--check (concat "HANDLER_ACTIVITY_" label)
                         (and expected (= pushes (car expected))
                              (= landings (cdr expected)))
                         (list :pushes pushes :landings landings
                               :expected expected))
      (s10-driver--check (concat "CHAIN_AT_SENTINEL_" label)
                         (= (nelisp-eln-handler-substrate-handlerlist)
                            (nelisp-eln-handler-substrate-sentinel-address)))
      (s10-driver--check (concat "NO_LIVE_BLOCKS_" label)
                         (= (nelisp-eln-handler-substrate-live-block-count) 0))
      (s10-driver--check (concat "NO_SPECPDL_LEFT_" label)
                         (= (nelisp-eln-runtime-services-specpdl-depth) 0)))))

(defun s10-driver--load-artifact ()
  (s10-driver--install-counters)
  (when s10-driver--red (s10-driver--disable-handlers))
  (load s10-driver--eln nil t t)
  (s10-driver--check "ARTIFACT_REGISTERED"
                     (and (fboundp 'byte-compile-form)
                          (subrp (symbol-function 'byte-compile-form))))
  (let ((handle (s10-driver--handle)))
    ;; The loader (not this driver) bound the substrate to the artifact.
    (s10-driver--check "LOADER_BOUND_THE_CHAIN"
                       (= (nelisp-eln-handler-substrate-artifact-handlerlist handle)
                          (nelisp-eln-handler-substrate-sentinel-address)))
    (s10-driver--check "LOADER_BOUND_THE_PRIVATE_SETJMP"
                       (nelisp-eln-handler-substrate-verify-setjmp-binding handle))))

(defun s10-driver--load-scenarios ()
  (load (expand-file-name "fixtures/s6-corpus/byte-compile-form.wrapper.el"
                          s10-driver--testdir) nil t t)
  (load (expand-file-name "nelisp-eln-s610-scenarios.el" s10-driver--testdir)
        nil t))

;;;; scenarios and forced ----------------------------------------------------

(defun s10-driver--scenarios (forced)
  (s10-driver--load-artifact)
  (when forced (s10-driver--force-region-entry))
  (s10-driver--load-scenarios)
  (s10-run-all)
  (unless s10-driver--red
    (s10-driver--check "SCENARIOS_COMPLETED" t)
    (princ (format "NELISP-ELN-S610-%s-PASS pushes=%d landings=%d\n"
                   (if forced "FORCED" "SCENARIOS")
                   s10-driver--pushes (nelisp-eln-handler-port-landing-count)))))

;;;; tamper ------------------------------------------------------------------

(defun s10-driver--call-once ()
  (s6-corpus--byte-compile-form 42 nil))

(defun s10-driver--tamper ()
  (s10-driver--load-artifact)
  (s10-driver--load-scenarios)
  (let* ((handle (s10-driver--handle))
         (owner (car nelisp-eln-registration--owners))
         (table (nl-ffi-memory-address (aref owner 12)))
         (freloc-cell (s10-driver--symbol-address handle "freloc_link_table"))
         (thread-cell (s10-driver--symbol-address handle "current_thread_reloc"))
         (setjmp-slot (nelisp-eln-handler-substrate--setjmp-slot handle))
         (baseline (s10-driver--call-once))
         (calls-before nil))
    (s10-driver--check "TAMPER_BASELINE_CALL_WORKS"
                       (equal baseline '(:return nil :output ((byte-constant 42)))))
    ;; 1. a link-table entry (Fsymbol_value's port) no longer the port
    (let ((saved (ptr-read-u64 table (* 8 1335))))
      (ptr-write-u64 table (* 8 1335) (logxor saved 16))
      (s10-driver--signals "TAMPER_LINK_TABLE_SLOT_REFUSED" #'s10-driver--call-once
                           '(expired-import-table))
      (ptr-write-u64 table (* 8 1335) saved))
    (s10-driver--check "TAMPER_LINK_TABLE_RESTORED"
                       (equal (s10-driver--call-once) baseline))
    ;; 2. the freloc_link_table cell pointing elsewhere
    (let ((saved (ptr-read-u64 freloc-cell 0)))
      (ptr-write-u64 freloc-cell 0 (logxor saved 64))
      (s10-driver--signals "TAMPER_FRELOC_CELL_REFUSED" #'s10-driver--call-once
                           '(expired-import-table))
      (ptr-write-u64 freloc-cell 0 saved))
    (s10-driver--check "TAMPER_FRELOC_CELL_RESTORED"
                       (equal (s10-driver--call-once) baseline))
    ;; 3. the `_setjmp' GOT word back at libc's (a non-private jump buffer)
    (let ((saved (ptr-read-u64 setjmp-slot 0))
          (libc (nelisp-eln-handler-substrate--libc-setjmp-address)))
      (ptr-write-u64 setjmp-slot 0 libc)
      (s10-driver--signals "TAMPER_SETJMP_GOT_REFUSED" #'s10-driver--call-once
                           '(setjmp-slot-readback))
      (ptr-write-u64 setjmp-slot 0 saved))
    (s10-driver--check "TAMPER_SETJMP_GOT_RESTORED"
                       (equal (s10-driver--call-once) baseline))
    ;; 4. the current_thread_reloc cell no longer &current_thread
    (let ((saved (ptr-read-u64 thread-cell 0)))
      (ptr-write-u64 thread-cell 0 0)
      (s10-driver--signals "TAMPER_CURRENT_THREAD_REFUSED" #'s10-driver--call-once
                           '(chain-current-thread-mismatch chain-got-slot-mismatch
                                                           no-current-thread-glob-dat))
      (ptr-write-u64 thread-cell 0 saved))
    (s10-driver--check "TAMPER_CURRENT_THREAD_RESTORED"
                       (equal (s10-driver--call-once) baseline))
    ;; 5. no native code ran during any refused call: the counters and the
    ;;    chain are untouched
    (setq calls-before s10-driver--pushes)
    (s10-driver--check "TAMPER_NO_HANDLER_ACTIVITY"
                       (and (= calls-before 0)
                            (= (nelisp-eln-handler-substrate-handlerlist)
                               (nelisp-eln-handler-substrate-sentinel-address))
                            (= (nelisp-eln-handler-substrate-live-block-count) 0)
                            (= (nelisp-eln-runtime-services-specpdl-depth) 0)))
    (princ "NELISP-ELN-S610-TAMPER-PASS\n")))

;;;; mutation ----------------------------------------------------------------

(defun s10-driver--file-bytes (path)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (buffer-string)))

(defun s10-driver--write-file (path bytes)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert bytes)
    (write-region (point-min) (point-max) path nil 'silent)))

(defun s10-driver--function-location (bytes name)
  "Return (VADDR FILE-OFFSET SIZE) of dynsym function NAME in BYTES."
  (let* ((text (nelisp-eln-registration--elf-section-in-bytes bytes ".text"))
         (dynsym (nelisp-eln-registration--elf-section-in-bytes bytes ".dynsym"))
         (found nil) (i 1))
    (while (and (not found) (< i (/ (nth 2 dynsym) 24)))
      (let ((entry (nelisp-eln-registration--dynsym-entry bytes i)))
        (when (equal (nth 0 entry) name)
          (setq found (list (nth 3 entry)
                            (+ (nth 1 text) (- (nth 3 entry) (nth 0 text)))
                            (nth 4 entry)))))
      (setq i (1+ i)))
    (or found (error "function %s not found" name))))

(defun s10-driver--try-load (path)
  "Load PATH; return the refusal reason symbol or `loaded'."
  (condition-case failure
      (progn (nelisp-eln-registration-load path) 'loaded)
    (error (cadr failure))))

(defun s10-driver--refuse (label path)
  "Check that the loader refuses PATH and registers nothing."
  (let ((reason (s10-driver--try-load path)))
    (when (or (eq reason 'loaded) (fboundp 'byte-compile-form)
              nelisp-eln-registration--owners)
      (error "S10 mutant %s was admitted (%S)" label reason))
    reason))

(defun s10-driver--mutation ()
  (let* ((genuine (s10-driver--file-bytes s10-driver--eln))
         (body (s10-driver--function-location
                genuine "F627974652d636f6d70696c652d666f726d_byte_compile_form_0"))
         (top (s10-driver--function-location genuine "top_level_run"))
         (dir (or s10-driver--workdir (error "NELISP_S10_WORKDIR is not set")))
         (count 0) (reasons nil)
         (body-offset (nth 1 body))
         (declared-key 'nelisp-eln-registration--setjmp-declared-body-sha256s))
    (make-directory dir t)
    (unless (fboundp 'byte-compile-form)
      (s10-driver--pass "MUTATION_START_CLEAN"))
    (s10-driver--check "MUTATION_START_NO_OWNER"
                       (null nelisp-eln-registration--owners))
    ;; Handler bytes of the body (offsets from the frame check).
    (let* ((analysis (nelisp-eln-tail-code-analyze-multi-import-call
                      (substring genuine body-offset (+ body-offset (nth 2 body)))
                      (nth 0 body)))
           (regions (nelisp-eln-handler-frame-check
                     (substring genuine body-offset (+ body-offset (nth 2 body)))
                     (plist-get analysis :plt-call-offsets) '(1335 1334)
                     (nth 0 body) (plist-get analysis :current-thread-got)))
           (offsets nil))
      (dolist (region regions)
        (let ((p (plist-get region :plt-call)))
          (dolist (range (list (cons (- (plist-get region :push-call) 24) (+ p 12))
                               (plist-get region :pad)
                               (plist-get region :guarded)))
            (let ((k (car range)))
              (while (< k (cdr range))
                (push k offsets)
                (setq k (+ k 14)))))))
      (setq offsets (sort (delete-dups offsets) #'<))
      ;; 1. every fourteenth handler byte (plus the PLT call displacements and the
      ;;    `mov $1,%esi' immediates) flipped: refused before dlopen because
      ;;    the body is no longer a declared exact body.
      (dolist (region regions)
        (let ((p (plist-get region :plt-call)))
          (dotimes (j 4) (push (+ p j) offsets))))
      (setq offsets (sort (delete-dups offsets) #'<))
      (dolist (offset offsets)
        (let ((mutant (copy-sequence genuine))
              (path (expand-file-name (format "body-%d.eln" offset) dir)))
          (aset mutant (+ body-offset offset)
                (logxor (aref mutant (+ body-offset offset)) #x01))
          (s10-driver--write-file path mutant)
          (push (s10-driver--refuse (format "body+%d" offset) path) reasons)
          (delete-file path)
          (setq count (1+ count))))
      (s10-driver--check "MUTATION_HANDLER_BYTES_REFUSED_PRE_DLOPEN"
                         (and (> count 20)
                              (cl-every (lambda (r) (eq r 'preopen-executable-region-not-admitted))
                                        reasons))
                         (list count (delete-dups (copy-sequence reasons))))
      ;; 2. the same mutants when their digest IS declared (a compromised
      ;;    declaration): the exact template / frame rule refuses them after
      ;;    dlopen but before any native code can run.
      (let ((refused 0) (reasons2 nil))
        (dolist (offset (cl-loop for o in offsets for i from 0 when (= 0 (% i 5)) collect o))
          (let* ((mutant (copy-sequence genuine))
                 (path (expand-file-name (format "declared-%d.eln" offset) dir)))
            (aset mutant (+ body-offset offset)
                  (logxor (aref mutant (+ body-offset offset)) #x20))
            (s10-driver--write-file path mutant)
            (let ((sha (secure-hash 'sha256
                                    (substring mutant body-offset
                                               (+ body-offset (nth 2 body)))))
                  (saved (symbol-value declared-key)))
              (unwind-protect
                  (progn (set declared-key (cons sha saved))
                         (push (s10-driver--refuse (format "declared body+%d" offset)
                                                   path)
                               reasons2))
                (set declared-key saved)))
            (delete-file path)
            (setq refused (1+ refused))))
        (s10-driver--check "MUTATION_DECLARED_MUTANTS_REFUSED_BY_THE_TEMPLATE"
                           (and (> refused 4)
                                (cl-every (lambda (r)
                                            (memq r '(leaf-instructions-not-admitted
                                                      preopen-executable-region-not-admitted
                                                      multi-import-not-admitted)))
                                          reasons2))
                           (delete-dups (copy-sequence reasons2)))))
    ;; 3. top_level_run: the arities and other bytes
    (let ((top-offset (nth 1 top)) (top-count 0))
      (dolist (rel '(0 3 13 27 34 49 56 61 70 101))
        (let ((mutant (copy-sequence genuine))
              (path (expand-file-name (format "top-%d.eln" rel) dir)))
          (aset mutant (+ top-offset rel) (logxor (aref mutant (+ top-offset rel)) #x01))
          (s10-driver--write-file path mutant)
          (s10-driver--refuse (format "top_level_run+%d" rel) path)
          (delete-file path)
          (setq top-count (1+ top-count))))
      (s10-driver--check "MUTATION_TOP_LEVEL_RUN_REFUSED" (= top-count 10))
      ;; arity: (max 1), (max 3), (min 0), (min 2) = max
      (let ((arity-refused 0))
        (dolist (change '((61 . 1) (61 . 3) (70 . 0) (70 . 2)))
          (let ((mutant (copy-sequence genuine))
                (path (expand-file-name (format "arity-%d-%d.eln" (car change)
                                                (cdr change)) dir)))
            (aset mutant (+ top-offset (car change)) (cdr change))
            (s10-driver--write-file path mutant)
            (s10-driver--refuse (format "arity %S" change) path)
            (delete-file path)
            (setq arity-refused (1+ arity-refused))))
        (s10-driver--check "MUTATION_ARITY_REFUSED" (= arity-refused 4))))
    ;; 4. authenticated constants: bytes of the printed constants vector
    (let* ((blob (s10-driver--function-location genuine "top_level_run"))
           (data (nelisp-eln-registration--elf-section-in-bytes genuine ".data"))
           (const-refused 0))
      (ignore blob)
      ;; the vector text starts at text_data_reloc_blob (.data + 0xc0)
      (dolist (delta '(16 300 700 1400))
        (let* ((offset (+ (nth 1 data) (- #x50e0 (nth 0 data)) delta))
               (mutant (copy-sequence genuine))
               (path (expand-file-name (format "const-%d.eln" delta) dir)))
          (aset mutant offset (logxor (aref mutant offset) #x01))
          (s10-driver--write-file path mutant)
          (s10-driver--refuse (format "constant blob+%d" delta) path)
          (delete-file path)
          (setq const-refused (1+ const-refused))))
      (s10-driver--check "MUTATION_CONSTANTS_REFUSED" (= const-refused 4)))
    ;; 5. the genuine artifact still loads after all of the refusals
    (load s10-driver--eln nil t t)
    (s10-driver--check "MUTATION_GENUINE_STILL_ADMITTED"
                       (subrp (symbol-function 'byte-compile-form)))
    (princ (format "NELISP-ELN-S610-MUTATION-PASS mutants=%d\n" count))))

;;;; dispatch --------------------------------------------------------------

(cond
 ((equal s10-driver--mode "scenarios") (s10-driver--scenarios nil))
 ((equal s10-driver--mode "forced") (s10-driver--scenarios t))
 ((equal s10-driver--mode "tamper") (s10-driver--tamper))
 ((equal s10-driver--mode "mutation") (s10-driver--mutation))
 (t (error "unknown S10 mode %S" s10-driver--mode)))
