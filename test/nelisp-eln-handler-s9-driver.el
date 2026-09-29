;;; nelisp-eln-handler-s9-driver.el --- Doc 210 S9 driver -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Runs on a NeLisp standalone binary (NELISP_READER_DYNAMIC flavor).
;; The genuine host-compiled probes (test/fixtures/eln-handler/
;; s9-handler-probes.eln) are opened by the ordinary system loader with the
;; S8 substrate bound (current_thread chain, private `_setjmp'), and their
;; native bodies are called through `nelisp-eln-callable-import--call-unary'
;; with handler options: the `push_handler' import is the real S9 port, the
;; other imports (`Ffuncall', `specbind', `helper_unbind_n') are the
;; ordinary dispatcher ports, all reached through the callback adapter.
;;
;; Environment: NELISP_ROOT, NELISP_S9_ELN, NELISP_S9_BODIES (space
;; separated NAME:SHA256 of every declared native body), NELISP_S9_TESTDIR,
;; NELISP_S9_MODE (port | match | unwind | gc | debugger | negatives | all),
;; NELISP_S9_RED (non-empty: pre-change control, native handlers disabled).
;; Prints `T ...' transcript lines (identical to the host's), `S9_<CHECK>=PASS'
;; lines and a final PASS line; any failure signals.

;;; Code:

(let ((repo (getenv "NELISP_ROOT")))
  (add-to-list 'load-path (expand-file-name "lisp" repo))
  (add-to-list 'load-path (expand-file-name "packages/nl-ffi/src" repo)))
(require 'cl-lib)
(require 'nelisp-eln-handler-port)
(load (expand-file-name "nelisp-eln-handler-s9-scenarios.el"
                        (getenv "NELISP_S9_TESTDIR")) nil t)

(defvar s9-driver--mode (getenv "NELISP_S9_MODE"))
(defvar s9-driver--handle nil)
(defvar s9-driver--constants nil)
(defvar s9-driver--capabilities nil)
(defvar s9-driver--ports nil)
(defvar s9-driver--owner nil)
(defvar s9-driver--red (let ((v (getenv "NELISP_S9_RED"))) (and v (> (length v) 0))))

(defun s9-driver--pass (name)
  (princ (format "S9_%s=PASS\n" name)))

(defun s9-driver--check (name condition)
  (unless condition (error "S9 check failed: %s" name))
  (s9-driver--pass name))

(defun s9-driver--signals (name thunk reason &optional error-symbol)
  "Check that THUNK signals ERROR-SYMBOL (default the port error) with REASON."
  (let ((got (condition-case failure (progn (funcall thunk) nil)
               (error (and (eq (car failure)
                               (or error-symbol 'nelisp-eln-handler-port-error))
                           (cadr failure))))))
    (unless (eq got reason)
      (error "S9 negative %s: wanted %S, got %S" name reason got))
    (s9-driver--pass name)))

;;;; Platform layer for the shared scenarios ---------------------------

(defun s9-gc ()
  (garbage-collect))

(defun s9-cleanup-around (cleanup body)
  "Register CLEANUP on the native specpdl analogue around BODY."
  (let ((depth (nelisp-eln-runtime-services-specpdl-depth)))
    (nelisp-eln-runtime-services-helper-unwind-protect cleanup)
    (prog1 (funcall body)
      ;; normal exit: run and pop the cleanup right away, like `unwind-protect'
      (nelisp-eln-runtime-services-helper-unbind-n
       (- (nelisp-eln-runtime-services-specpdl-depth) depth)))))

(defun s9-call (probe argument)
  (let ((capability (cdr (assq probe s9-driver--capabilities))))
    (unless capability
      (error "unknown probe %S" probe))
    ;; The probe bodies only pass their argument on to an authenticated
    ;; port, so a function symbol is admitted as an opaque interned view.
    (nelisp-eln-objects-call-with-artifact-symbols
     nil
     (lambda ()
       (nelisp-eln-callable-import--call-unary
        capability nil argument nil nil s9-driver--constants s9-driver--ports nil
        (list :handle s9-driver--handle)))
     t)))

;;;; Loading the probe artifact ---------------------------------------

(defun s9-driver--body-name (bodies probe)
  (let ((suffix (concat "_" (replace-regexp-in-string "-" "_" (symbol-name probe))
                        "_0")))
    (cl-find-if (lambda (name) (string-suffix-p suffix name))
                (mapcar (lambda (entry) (car (split-string entry ":"))) bodies))))

(defun s9-driver--read-blob (handle symbol)
  "Read the printed constants vector SYMBOL of HANDLE (an 8-byte size, text)."
  (let* ((info (nelisp-eln-system-loader-symbol-info handle symbol))
         (address (plist-get info :address))
         (size (plist-get info :size))
         (chars nil) (i 8))
    (while (and (< i size) (/= (ptr-read-u8 address i) 0))
      (push (ptr-read-u8 address i) chars)
      (setq i (1+ i)))
    (car (read-from-string (apply #'unibyte-string (nreverse chars))))))

(defun s9-driver--word (index)
  (+ #x0123456789a000 (* 16 (1+ index))))

(defun s9-driver--port (number spec)
  (cons (nelisp-eln-callable-import-port-tag number) spec))

(defun s9-driver--install (handle)
  "Fill the artifact's link table and d_reloc cells; return the owner."
  (let* ((owner (nl-ffi-memory-allocate (* 8 1400)))
         (table (nl-ffi-memory-address owner))
         (d-reloc (plist-get (nelisp-eln-system-loader-symbol-info
                              handle "d_reloc") :address))
         (vector (s9-driver--read-blob handle "text_data_reloc_blob"))
         (index 0))
    (dotimes (i 1400) (ptr-write-u64 table (* 8 i) 0))
    ;; slot -> port: push_handler 2, Ffuncall 945, specbind 12, helper_unbind_n 4
    (ptr-write-u64 table (* 8 2) (nelisp-eln-callable-import-port-entry-address 0))
    (ptr-write-u64 table (* 8 945) (nelisp-eln-callable-import-port-entry-address 1))
    (ptr-write-u64 table (* 8 12) (nelisp-eln-callable-import-port-entry-address 2))
    (ptr-write-u64 table (* 8 4) (nelisp-eln-callable-import-port-entry-address 3))
    (ptr-write-u64 (plist-get (nelisp-eln-system-loader-symbol-info
                               handle "freloc_link_table") :address)
                   0 table)
    (setq s9-driver--constants nil)
    (while (< index (length vector))
      (ptr-write-u64 d-reloc (* 8 index) (s9-driver--word index))
      (push (cons (s9-driver--word index) (aref vector index))
            s9-driver--constants)
      (setq index (1+ index)))
    (setq s9-driver--ports
          (list (s9-driver--port 0 (nelisp-eln-handler-port-push-port-spec))
                (s9-driver--port
                 1 (list :convention 'many :arity 1 :arguments '(lisp)
                         :return 'lisp
                         :implementation (lambda (f) (funcall f))))
                (s9-driver--port
                 2 (list :convention 'fixed :arity 2 :arguments '(lisp lisp)
                         :return 'void
                         :implementation
                         (lambda (symbol value)
                           (nelisp-eln-runtime-services-specbind symbol value))))
                (s9-driver--port
                 3 (list :convention 'fixed :arity 1 :arguments '(lisp)
                         :return 'void
                         :implementation
                         (lambda (n)
                           (nelisp-eln-runtime-services-helper-unbind-n n))))))
    owner))

(defun s9-driver--open ()
  (let* ((bodies (split-string (getenv "NELISP_S9_BODIES") " " t))
         (probes '(s9-probe-error s9-probe-quit s9-probe-t s9-probe-debug
                   s9-probe-debug-let s9-probe-arith s9-probe-let
                   s9-probe-nested)))
    (setq nelisp-eln-registration--setjmp-declared-body-sha256s
          (mapcar (lambda (entry) (cadr (split-string entry ":"))) bodies))
    (setq s9-driver--handle (nelisp-eln-system-loader-open (getenv "NELISP_S9_ELN")))
    (nelisp-eln-handler-substrate-bind-chain s9-driver--handle)
    (nelisp-eln-handler-substrate-bind-setjmp s9-driver--handle)
    (setq s9-driver--owner (s9-driver--install s9-driver--handle))
    (setq s9-driver--capabilities
          (mapcar (lambda (probe)
                    (cons probe (nelisp-eln-system-loader-function-capability
                                 s9-driver--handle
                                 (or (s9-driver--body-name bodies probe)
                                     (error "no declared body for %S" probe)))))
                  probes))))

(defun s9-driver--close ()
  (when s9-driver--handle
    (nelisp-eln-system-loader-close s9-driver--handle)
    (nelisp-eln-system-loader--release-owner s9-driver--owner)))

;;;; Pre-change control ------------------------------------------------

(defmacro s9-driver--maybe-red (&rest body)
  "Run BODY; with NELISP_S9_RED set, native handlers never see an exit."
  `(if s9-driver--red
       (cl-letf (((symbol-function 'nelisp-eln-handler-port-try-divert)
                  (lambda (_frame _failure) nil)))
         ,@body)
     ,@body))

;;;; Invariants after every run ----------------------------------------

(defun s9-driver--quiescent-p (baseline)
  "True when no block leaked and the chain is back at the sentinel."
  (and (= (nelisp-eln-handler-substrate-live-block-count) baseline)
       (= (nelisp-eln-handler-substrate-handlerlist)
          (nelisp-eln-handler-substrate-sentinel-address))
       (zerop (ptr-read-u64 (nelisp-eln-handler-port--resume-block) 0))
       (zerop (nelisp-eln-runtime-services-specpdl-depth))))

;;;; S9.1 unit ---------------------------------------------------------

(defun s9-driver--synthetic-frame ()
  (list :handler-range (cons #x10000 #x20000)
        :handler-base (nelisp-eln-handler-substrate-handlerlist)
        :native-handlers nil :landed-blocks nil :dummy-blocks nil
        :entry-sp #x7ff000))

(defun s9-driver--port-unit ()
  (nelisp-eln-handler-substrate-ensure)
  (let* ((baseline (nelisp-eln-handler-substrate-live-block-count))
         (sentinel (nelisp-eln-handler-substrate-sentinel-address))
         (thread (nelisp-eln-handler-substrate-thread-address))
         (tag-word #x0123456789a010))
    (s9-driver--check "PORT_RESUME_OFFSET_IS_672"
                      (= nelisp-eln-handler-port--resume-offset 672))
    ;; The port entry point fails closed.  A rejected type records the
    ;; condition on the frame and answers a linked placeholder block, never
    ;; an unusable address; the frame releases it at retire.
    (let* ((frame2 (plist-put (s9-driver--synthetic-frame) :constants
                              (list (cons tag-word '(error)))))
           (nelisp-eln-callable-import--frames (list frame2))
           (before (nelisp-eln-handler-substrate-live-block-count))
           (answer (nelisp-eln-handler-port-push tag-word 0))
           (now (car nelisp-eln-callable-import--frames)))
      (s9-driver--check "PORT_ENTRY_TYPE_0_FAILS_CLOSED"
                        (and (eq (car (plist-get now :condition))
                                 'nelisp-eln-handler-port-error)
                             (eq (cadr (plist-get now :condition))
                                 'handler-type-rejected)
                             (nelisp-eln-handler-substrate-block-live-p answer)
                             (= (ptr-read-u64 thread #x68) answer)
                             (null (plist-get now :native-handlers))
                             (= (1+ before)
                                (nelisp-eln-handler-substrate-live-block-count))))
      (nelisp-eln-handler-port-frame-retire now)
      (s9-driver--check "PORT_RETIRE_RESTORES_BASE_AND_RELEASES"
                        (and (= (nelisp-eln-handler-substrate-handlerlist) sentinel)
                             (= before
                                (nelisp-eln-handler-substrate-live-block-count)))))
    (let* ((frame3 (s9-driver--synthetic-frame))
           (nelisp-eln-callable-import--frames (list frame3)))
      (nelisp-eln-handler-port-push #x0123456789aff0 1)
      (s9-driver--check "PORT_ENTRY_UNAUTHENTICATED_TAG_FAILS_CLOSED"
                        (plist-get (car nelisp-eln-callable-import--frames)
                                   :condition))
      (nelisp-eln-handler-port-frame-retire (car nelisp-eln-callable-import--frames)))
    (s9-driver--check "PORT_FAILED_PUSHES_LEAVE_NO_TRACE"
                      (and (= baseline (nelisp-eln-handler-substrate-live-block-count))
                           (= sentinel (nelisp-eln-handler-substrate-handlerlist))))
    ;; The minting itself.
    (let* ((frame (s9-driver--synthetic-frame))
           (nelisp-eln-callable-import--frames (list frame)))
      ;; A pending specpdl entry so pdlcount is not trivially zero.
      (nelisp-eln-runtime-services-specbind 's9-unit-dyn 5)
      (let ((block (nelisp-eln-handler-port-mint frame tag-word '(error) 1)))
        (s9-driver--check "PORT_MINTS_GNU_LAYOUT"
                          (and (= (ptr-read-u64 block 0) 1)
                               (= (ptr-read-u64 block 8) tag-word)
                               (= (ptr-read-u64 block #x20) sentinel)
                               (= (ptr-read-u64 block #x10) 0)
                               (= (ptr-read-u64 block #x18) 0)))
        (s9-driver--check "PORT_RECORDS_PDLCOUNT"
                          (and (= (ptr-read-u64 block #x110) 1)
                               (= (plist-get (car (plist-get frame :native-handlers))
                                             :pdlcount)
                                  1)))
        (s9-driver--check "PORT_LINKS_THREAD_HANDLERLIST"
                          (and (= (ptr-read-u64 thread #x68) block)
                               (= (nelisp-eln-handler-substrate-handlerlist) block)))
        (s9-driver--check "PORT_BLOCK_IS_LIVE_AND_ALIGNED"
                          (and (nelisp-eln-handler-substrate-block-live-p block)
                               (= 0 (logand block 15))))
        (let ((inner (nelisp-eln-handler-port-mint frame tag-word '(error) 1)))
          (s9-driver--check "PORT_SECOND_PUSH_LINKS_TO_FIRST"
                            (and (= (ptr-read-u64 inner #x20) block)
                                 (= (ptr-read-u64 thread #x68) inner)
                                 (eq (plist-get (car (plist-get frame :native-handlers))
                                                :block)
                                     inner))))
        ;; Negative controls: everything but CONDITION_CASE is refused and
        ;; leaves neither a block nor a chain change behind.
        (let ((live (nelisp-eln-handler-substrate-live-block-count))
              (head (nelisp-eln-handler-substrate-handlerlist)))
          (dolist (type '(0 2 3 4 5 6 -1))
            (s9-driver--signals (format "PORT_REJECTS_TYPE_%d" (if (< type 0) 99 type))
                                (lambda ()
                                  (nelisp-eln-handler-port-mint
                                   frame tag-word '(error) type))
                                'handler-type-rejected))
          (s9-driver--check "PORT_REJECTION_LEAVES_NO_TRACE"
                            (and (= live (nelisp-eln-handler-substrate-live-block-count))
                                 (= head (nelisp-eln-handler-substrate-handlerlist)))))
        (s9-driver--signals "PORT_REJECTS_NON_HANDLER_ACTIVATION"
                            (lambda ()
                              (nelisp-eln-handler-port-mint
                               (list :native-handlers nil) 1 '(error) 1))
                            'activation-not-handler-bearing)
        ;; Retiring with both handlers unpopped fails closed and restores.
        (let ((failure (nelisp-eln-handler-port-frame-retire frame)))
          (s9-driver--check "PORT_RETIRE_UNPOPPED_FAILS_CLOSED"
                            (and (eq (car failure) 'nelisp-eln-handler-port-error)
                                 (eq (cadr failure) 'unpopped-handlers)
                                 (= (nelisp-eln-handler-substrate-handlerlist)
                                    sentinel))))))
    (nelisp-eln-runtime-services-helper-unbind-n 1)
    (s9-driver--check "PORT_ALL_BLOCKS_RELEASED"
                      (s9-driver--quiescent-p baseline)))
  (princ "NELISP-ELN-HANDLER-PORT-PASS 1\n"))

;;;; scenario groups ---------------------------------------------------

(defun s9-driver--group (group)
  (s9-driver--open)
  (unwind-protect
      (let ((baseline (nelisp-eln-handler-substrate-live-block-count))
            (landings (nelisp-eln-handler-port-landing-count)))
        (s9-driver--maybe-red (s9-run-group group))
        (s9-driver--check (format "GROUP_%s_QUIESCENT" (upcase group))
                          (s9-driver--quiescent-p baseline))
        (unless s9-driver--red
          (s9-driver--check
           (format "GROUP_%s_LANDED" (upcase group))
           (> (nelisp-eln-handler-port-landing-count) landings))))
    (s9-driver--close))
  (princ (format "NELISP-ELN-HANDLER-S9-%s-PASS 1\n" (upcase group))))

;;;; negatives ---------------------------------------------------------

(defun s9-driver--run-catching (probe f)
  "Run PROBE on F; return (:value V) or (:error ERROR)."
  (condition-case e (list :value (s9-call probe f))
    (error (list :error e))))

(defvar s9-driver--spare nil)
(defvar s9-driver--seen-block nil)

(defun s9-driver--spare-block (next)
  "Allocate a valid placeholder handler (type 1) whose next is NEXT."
  (let ((block (nelisp-eln-handler-substrate-allocate-block)))
    (ptr-write-u64 block 0 1)
    (ptr-write-u64 block #x20 next)
    block))

(defun s9-driver--head ()
  (nelisp-eln-handler-substrate-handlerlist))

(defun s9-f-forge-head ()
  (ptr-write-u64 (nelisp-eln-handler-substrate-thread-address) #x68 s9-driver--spare)
  (signal 'error '("s9 forged head")))
(defun s9-f-forge-next ()
  (ptr-write-u64 (s9-driver--head) #x20 s9-driver--spare)
  (signal 'error '("s9 forged next")))
(defun s9-f-forge-type ()
  (ptr-write-u64 (s9-driver--head) 0 0)
  (signal 'error '("s9 forged type")))
(defun s9-f-forge-pdlcount ()
  (ptr-write-u64 (s9-driver--head) #x110 99)
  (signal 'error '("s9 forged pdlcount")))
(defun s9-f-forge-thread-cell ()
  (ptr-write-u64 (nelisp-eln-handler-substrate-current-thread-cell) 0
                 s9-driver--spare)
  (signal 'error '("s9 forged thread cell")))
(defun s9-f-rip-outside ()
  (let ((range (plist-get (car nelisp-eln-callable-import--frames) :handler-range)))
    (ptr-write-u64 (s9-driver--head) (+ #x40 56) (- (car range) 16)))
  (signal 'error '("s9 rip outside body")))
(defun s9-f-rsp-misaligned ()
  (let ((h (s9-driver--head)))
    (ptr-write-u64 h (+ #x40 48) (+ 8 (ptr-read-u64 h (+ #x40 48)))))
  (signal 'error '("s9 rsp misaligned")))
(defun s9-f-rsp-far ()
  (let ((h (s9-driver--head)))
    (ptr-write-u64 h (+ #x40 48) (+ 1048576 (ptr-read-u64 h (+ #x40 48)))))
  (signal 'error '("s9 rsp far")))
(defun s9-f-rsp-below ()
  (let ((h (s9-driver--head)))
    (ptr-write-u64 h (+ #x40 48) (- (ptr-read-u64 h (+ #x40 48)) 65536)))
  (signal 'error '("s9 rsp below")))
(defun s9-f-leave-unpopped ()
  (nelisp-eln-handler-port-mint (car nelisp-eln-callable-import--frames)
                                (s9-driver--word 0) '(error) 1)
  'fine)
(defun s9-f-remember-block ()
  (setq s9-driver--seen-block (s9-driver--head))
  (signal 'error '("s9 remember")))

(defun s9-driver--post-request (buffer)
  "Post a landing request for BUFFER exactly as the Lisp side would."
  (let ((resume (nelisp-eln-handler-port--resume-block))
        (range (plist-get (car nelisp-eln-callable-import--frames) :handler-range))
        (bounds (nelisp-eln-handler-substrate-region-bounds)))
    (ptr-write-u64 resume 8 (nelisp-eln-handler-substrate--stub-address
                             'longjmp-consume))
    (ptr-write-u64 resume 24 (car range))
    (ptr-write-u64 resume 32 (cdr range))
    (ptr-write-u64 resume 40 (car bounds))
    (ptr-write-u64 resume 48 (cdr bounds))
    (ptr-write-u64 resume 0 buffer)))

(defun s9-f-adapter-rip-outside ()
  (let ((h (s9-driver--head))
        (range (plist-get (car nelisp-eln-callable-import--frames) :handler-range)))
    (ptr-write-u64 h (+ #x40 56) (- (car range) 16))
    (s9-driver--post-request (+ h #x40)))
  'fine)
(defun s9-f-adapter-rsp-wrong ()
  (let ((h (s9-driver--head)))
    (ptr-write-u64 h (+ #x40 48) (+ 8 (ptr-read-u64 h (+ #x40 48))))
    (s9-driver--post-request (+ h #x40)))
  'fine)
(defun s9-f-adapter-replay ()
  ;; a released (zeroed) block: what a replayed, already consumed buffer is
  (s9-driver--post-request (+ s9-driver--spare #x40))
  'fine)
(defun s9-f-adapter-outside-region ()
  (s9-driver--post-request (+ (car (nelisp-eln-handler-substrate-region-bounds))
                              (* 4 1048576)))
  'fine)
(defun s9-f-adapter-misaligned ()
  (s9-driver--post-request (+ (s9-driver--head) #x48))
  'fine)

(defun s9-driver--expect-port-error (name result reason)
  (s9-driver--check name
                    (and (eq (car result) :error)
                         (eq (car (cadr result)) 'nelisp-eln-handler-port-error)
                         (eq (cadr (cadr result)) reason))))

(defun s9-driver--negative (name probe f reason baseline landings)
  "Run PROBE on F expecting a fail-closed port error REASON and no landing."
  (let ((result (s9-driver--run-catching probe f)))
    (s9-driver--expect-port-error (format "NEG_%s_FAILS_CLOSED" name) result reason)
    (s9-driver--check (format "NEG_%s_NO_LANDING" name)
                      (= landings (nelisp-eln-handler-port-landing-count)))
    (s9-driver--check (format "NEG_%s_QUIESCENT" name)
                      (or (memq name '(FORGED-THREAD-CELL))
                          (s9-driver--quiescent-p baseline)))))

(defun s9-driver--negatives ()
  (s9-driver--open)
  (unwind-protect
      (let* ((baseline (nelisp-eln-handler-substrate-live-block-count))
             (sentinel (nelisp-eln-handler-substrate-sentinel-address))
             (cell (nelisp-eln-handler-substrate-current-thread-cell))
             (thread (nelisp-eln-handler-substrate-thread-address))
             (landings (nelisp-eln-handler-port-landing-count)))
        ;; positive control: a clean run lands exactly once
        (s9-driver--check "NEG_CONTROL_CLEAN_RUN_LANDS_ONCE"
                          (and (eq (cadr (s9-driver--run-catching
                                          's9-probe-error 's9-raise-void))
                                   'caught)
                               (= (1+ landings)
                                  (nelisp-eln-handler-port-landing-count))
                               (s9-driver--quiescent-p baseline)))
        (setq landings (nelisp-eln-handler-port-landing-count))
        (setq s9-driver--spare (s9-driver--spare-block sentinel))
        (s9-driver--negative 'FORGED-HEAD 's9-probe-error 's9-f-forge-head
                             'chain-forged (1+ baseline) landings)
        (ptr-write-u64 thread #x68 sentinel)
        (ptr-write-u64 s9-driver--spare #x20 sentinel)
        (s9-driver--negative 'FORGED-NEXT 's9-probe-error 's9-f-forge-next
                             'chain-forged (1+ baseline) landings)
        (ptr-write-u64 thread #x68 sentinel)
        (s9-driver--negative 'FORGED-TYPE 's9-probe-error 's9-f-forge-type
                             'chain-forged (1+ baseline) landings)
        (s9-driver--negative 'FORGED-PDLCOUNT 's9-probe-error 's9-f-forge-pdlcount
                             'chain-forged (1+ baseline) landings)
        ;; a forged current_thread cell: the native pop then reads a fake
        ;; thread whose handlerlist we point at the live handler
        (ptr-write-u64 s9-driver--spare #x68 sentinel)
        (let ((result (s9-driver--run-catching 's9-probe-error 's9-f-forge-thread-cell)))
          (ptr-write-u64 cell 0 thread)
          (ptr-write-u64 thread #x68 sentinel)
          (s9-driver--expect-port-error "NEG_FORGED-THREAD-CELL_FAILS_CLOSED" result
                                        'chain-thread-cell-forged)
          (s9-driver--check "NEG_FORGED-THREAD-CELL_NO_LANDING"
                            (= landings (nelisp-eln-handler-port-landing-count))))
        (nelisp-eln-handler-substrate-release-block s9-driver--spare)
        (s9-driver--check "NEG_FORGED_CHAIN_RECOVERED"
                          (s9-driver--quiescent-p baseline))
        (s9-driver--negative 'RIP-OUTSIDE-BODY 's9-probe-error 's9-f-rip-outside
                             'handler-rip-out-of-body baseline landings)
        (s9-driver--negative 'RSP-MISALIGNED 's9-probe-error 's9-f-rsp-misaligned
                             'handler-rsp-invalid baseline landings)
        (s9-driver--negative 'RSP-TOO-FAR 's9-probe-error 's9-f-rsp-far
                             'handler-rsp-invalid baseline landings)
        (s9-driver--negative 'RSP-BELOW-ADAPTER 's9-probe-error 's9-f-rsp-below
                             'handler-rsp-invalid baseline landings)
        (s9-driver--negative 'UNPOPPED 's9-probe-error 's9-f-leave-unpopped
                             'unpopped-handlers baseline landings)
        ;; the adapter's own check refuses a request the Lisp side would not
        ;; have made (status 5 = divert refused), and never jumps
        (dolist (case '((ADAPTER-RIP-OUTSIDE . s9-f-adapter-rip-outside)
                        (ADAPTER-RSP-WRONG . s9-f-adapter-rsp-wrong)
                        (ADAPTER-OUTSIDE-REGION . s9-f-adapter-outside-region)
                        (ADAPTER-MISALIGNED . s9-f-adapter-misaligned)))
          (let ((result (s9-driver--run-catching 's9-probe-error (cdr case))))
            (s9-driver--check
             (format "NEG_%s_REFUSED_BY_ADAPTER" (car case))
             (and (eq (car result) :error)
                  (eq (car (cadr result)) 'nelisp-eln-callable-import-error)
                  (equal (cadr (cadr result)) '(callback-status 5))))
            (s9-driver--check (format "NEG_%s_NO_LANDING" (car case))
                              (= landings (nelisp-eln-handler-port-landing-count)))
            (s9-driver--check (format "NEG_%s_QUIESCENT" (car case))
                              (s9-driver--quiescent-p baseline))))
        ;; replay: a consumed buffer.  Run a landing (the nested probe pops
        ;; two handlers), remember a block, then request its zeroed buffer.
        (s9-driver--run-catching 's9-probe-error 's9-f-remember-block)
        (setq landings (nelisp-eln-handler-port-landing-count))
        ;; keep the consumed block off the top of the free list so the next
        ;; activation cannot legitimately re-mint it
        (let* ((p (nelisp-eln-handler-substrate-allocate-block))
               (q (nelisp-eln-handler-substrate-allocate-block)))
          (s9-driver--check "NEG_REPLAY_BLOCK_IS_THE_LANDED_ONE"
                            (eql p s9-driver--seen-block))
          (nelisp-eln-handler-substrate-release-block p)
          (nelisp-eln-handler-substrate-release-block q))
        (setq s9-driver--spare s9-driver--seen-block)
        (s9-driver--check "NEG_REPLAY_BLOCK_WAS_RELEASED"
                          (and s9-driver--spare
                               (not (nelisp-eln-handler-substrate-block-live-p
                                     s9-driver--spare))
                               (zerop (ptr-read-u64 s9-driver--spare (+ #x40 56)))))
        (let ((result (s9-driver--run-catching 's9-probe-error 's9-f-adapter-replay)))
          (s9-driver--check "NEG_REPLAY_REFUSED_BY_ADAPTER"
                            (and (eq (car result) :error)
                                 (equal (cadr (cadr result)) '(callback-status 5))))
          (s9-driver--check "NEG_REPLAY_NO_LANDING"
                            (= landings (nelisp-eln-handler-port-landing-count))))
        (s9-driver--check "NEG_REPLAY_QUIESCENT" (s9-driver--quiescent-p baseline))
        ;; the same run without the native handlers: the pre-change control
        (let ((result (cl-letf (((symbol-function 'nelisp-eln-handler-port-try-divert)
                                 (lambda (_frame _failure) nil)))
                        (s9-driver--run-catching 's9-probe-error 's9-raise-void))))
          (s9-driver--check "NEG_RED_CONTROL_NO_HANDLERS_PROPAGATES"
                            (and (eq (car result) :error)
                                 (eq (car (cadr result)) 'void-variable)))
          (s9-driver--check "NEG_RED_CONTROL_QUIESCENT"
                            (s9-driver--quiescent-p baseline)))
        ;; an activation not opened for handlers cannot push one
        (let ((result (condition-case e
                          (list :value
                                (nelisp-eln-objects-call-with-artifact-symbols
                                 nil
                                 (lambda ()
                                   (nelisp-eln-callable-import--call-unary
                                    (cdr (assq 's9-probe-error
                                               s9-driver--capabilities))
                                    nil 's9-raise-void nil nil s9-driver--constants
                                    s9-driver--ports))
                                 t))
                        (error (list :error e)))))
          (s9-driver--expect-port-error "NEG_NON_HANDLER_ACTIVATION_REFUSED" result
                                        'activation-not-handler-bearing)
          (s9-driver--check "NEG_NON_HANDLER_ACTIVATION_QUIESCENT"
                            (s9-driver--quiescent-p baseline))))
    (s9-driver--close))
  (princ "NELISP-ELN-HANDLER-S9-NEGATIVES-PASS 1\n"))

;;;; The consuming longjmp stub -----------------------------------------

(defun s9-driver--consume-stub ()
  "The landing stub zeroes its buffer: returns twice once, then is spent."
  (let* ((buf (nelisp-asm-x86_64-make-buffer))
         (consume nil) (setjmp nil))
    ;; ENTRY(rdi=buffer, rsi=setjmp, rdx=consume): call setjmp; on the first
    ;; return call consume(buffer, 7); on the second return give the value.
    (nelisp-asm-x86_64-define-label buf 'entry)
    (nelisp-asm-x86_64-sub-imm32 buf 'rsp 8)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rax 'rsi)
    (nelisp-asm-x86_64-call-reg buf 'rax)
    (nelisp-asm-x86_64-cmp-imm32 buf 'rax 0)
    (nelisp-asm-x86_64-jnz-rel32 buf 'done)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rax 'rdx)
    (nelisp-asm-x86_64-mov-imm32 buf 'rsi 7)
    (nelisp-asm-x86_64-call-reg buf 'rax)
    (nelisp-asm-x86_64-define-label buf 'done)
    (nelisp-asm-x86_64-add-imm32 buf 'rsp 8)
    (nelisp-asm-x86_64-ret buf)
    (let* ((bytes (nelisp-asm-x86_64-resolve-fixups buf))
           (labels (nelisp-asm-x86_64-buffer-labels buf))
           (page (nelisp-native-load-map-anonymous
                  (nelisp-eln-handler-substrate--page-round (length bytes)) t)))
      (nelisp-eln-handler-substrate--write-bytes page bytes)
      (setq consume (nelisp-eln-handler-substrate--stub-address 'longjmp-consume)
            setjmp (nelisp-eln-handler-substrate--stub-address 'setjmp))
      (let* ((block (nelisp-eln-handler-substrate-allocate-block))
             (buffer (+ block #x40))
             (result (ptr-call (+ page (cdr (assq 'entry labels)))
                               buffer setjmp consume 0 0 0)))
        (s9-driver--check "STUB_CONSUME_RETURNS_TWICE_WITH_VALUE" (= result 1))
        (s9-driver--check "STUB_CONSUME_ZEROES_THE_BUFFER"
                          (let ((all t))
                            (dotimes (i 8)
                              (unless (zerop (ptr-read-u64 buffer (* 8 i)))
                                (setq all nil)))
                            all))
        (nelisp-eln-handler-substrate-release-block block)))))

;;;; main --------------------------------------------------------------

(cond
 ((equal s9-driver--mode "port")
  (s9-driver--port-unit)
  (s9-driver--consume-stub)
  (princ "NELISP-ELN-HANDLER-PORT-STUB-PASS 1\n"))
 ((equal s9-driver--mode "negatives")
  (s9-driver--consume-stub)
  (s9-driver--negatives))
 ((member s9-driver--mode '("match" "unwind" "gc" "debugger" "all"))
  (s9-driver--group s9-driver--mode))
 (t (error "unknown S9 driver mode: %s" s9-driver--mode)))

;;; nelisp-eln-handler-s9-driver.el ends here
