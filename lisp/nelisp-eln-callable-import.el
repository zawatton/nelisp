;;; nelisp-eln-callable-import.el --- bounded .eln import bridge -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Transport authenticated raw GNU words through a rooted NeLisp callback.
;; Callers must validate that the target is a supported tail-only import leaf.

;;; Code:

(require 'nelisp-eln-system-loader)
(require 'nelisp-eln-objects)
(require 'nelisp-eln-raw-call)
(require 'nelisp-eln-abi)
(require 'nl-ffi-memory)
(require 'nelisp-eln-runtime-services)
(require 'cl-lib)

;; `nelisp-native-load' (3048 lines) is NOT required eagerly: every use
;; below is inside a function body (entry-address lookups, the pin/box
;; bridge, or the callback relay), never at load time, so a process that
;; loads this file without ever calling one of these functions -- for
;; example the S7.7.4 corpus gate's own preflight-only rejections -- never
;; pays that module's load cost. Each call site does its own
;; `(require 'nelisp-native-load)' right before first use (mirroring
;; `nelisp-eln-registration.el''s identical pattern for this same module).
(declare-function ptr-call "ext:nelisp-runtime" (fn a b c d e f))
(declare-function ptr-read-u64 "ext:nelisp-runtime" (ptr offset))
(declare-function ptr-write-u64 "ext:nelisp-runtime" (ptr offset value))
(declare-function nelisp--native-env "ext:nelisp-runtime" ())
(declare-function nelisp-native-load--symbol-addr "nelisp-native-load" (name))
(declare-function nelisp-native-load--pin-begin "nelisp-native-load" (env))
(declare-function nelisp-native-load--pin-reserve
  "nelisp-native-load" (env marker))
(declare-function nelisp-native-load--pin-end "nelisp-native-load" (env marker))
(declare-function nelisp-native-load-box
  "nelisp-native-load" (addr value &optional env pin-frame))
(declare-function nelisp-native-load-unbox
  "nelisp-native-load" (addr &optional env pin-frame))

(define-error 'nelisp-eln-callable-import-error
  "Unsupported authenticated import call")

(defvar nelisp-eln-callable-import--frames nil)
(defvar nelisp-eln-callable-import--pending-cleanups nil)

(defun nelisp-eln-callable-import-entry-address ()
  "Return the callback1 adapter address for an admitted GNU import table."
  ;; `fboundp', not just `featurep': lets a test's `cl-letf' mock of this
  ;; exact function survive instead of being clobbered by a real load.
  (unless (fboundp 'nelisp-native-load--symbol-addr)
    (require 'nelisp-native-load))
  (nelisp-native-load--symbol-addr "nelisp_eln_callback1_entry_word"))

(defconst nelisp-eln-callable-import--port-count 32
  "Number of slot-identifying callback ports the standalone runtime has.
Must equal `nelisp-cc-eln-callback7-port-count'.")
(defconst nelisp-eln-callable-import--port-tag-base 1347375700
  "Descriptor word 6 of port N's callbacks is this base plus N.
Must equal `nelisp-cc-eln-callback7-port-tag-base'.")

(defun nelisp-eln-callable-import-port-count ()
  "Return how many slot-identifying callback ports the runtime has."
  nelisp-eln-callable-import--port-count)

(defun nelisp-eln-callable-import-port-tag (port)
  "Return the descriptor word-6 tag callbacks through PORT carry."
  (unless (and (integerp port) (<= 0 port)
               (< port nelisp-eln-callable-import--port-count))
    (signal 'nelisp-eln-callable-import-error (list 'invalid-port port)))
  (+ nelisp-eln-callable-import--port-tag-base port))

(defun nelisp-eln-callable-import-port-entry-address (port)
  "Return slot-identifying callback PORT's entry address.
Each port forwards all six argument registers like the seven-word entry
and stamps its own `nelisp-eln-callable-import-port-tag' into descriptor
word 6, so a native body importing several distinct freloc slots can
install a different port in each slot and the dispatcher can recover
which authenticated slot was called."
  (nelisp-eln-callable-import-port-tag port)
  (unless (fboundp 'nelisp-native-load--symbol-addr)
    (require 'nelisp-native-load))
  (nelisp-native-load--symbol-addr
   (format "nelisp_eln_callback_port%d_entry_word" port)))

(defun nelisp-eln-callable-import-wide-entry-p (convention arity)
  "Non-nil when a CONVENTION/ARITY import needs the seven-word entry.
The callback1 adapter forwards only the first argument register (the rest
of its descriptor is zero-filled), which is enough for unary callees but
drops the second and later arguments of a fixed multi-argument GNU C
callee such as `wrong_type_argument', and the argv pointer of a MANY
\(argc, argv) callee."
  (or (eq convention 'many)
      (and (eq convention 'fixed) (integerp arity) (> arity 1))))

(defun nelisp-eln-callable-import-wide-entry-address ()
  "Return the seven-word adapter address for fixed multi-argument imports.
It forwards all six argument registers; for a fixed ARITY callee only the
first ARITY descriptor words are arguments, the rest are unspecified
caller register contents that `nelisp-eln-callable-import--args' ignores."
  (unless (fboundp 'nelisp-native-load--symbol-addr)
    (require 'nelisp-native-load))
  (nelisp-native-load--symbol-addr "nelisp_eln_callback7_entry_word"))

(defun nelisp-eln-callable-import--published-exit
    (env marker args-slot out-slot)
  "Return (KIND TAG . VALUE) the callback adapter published, or nil.
Doc 207: when the gateway reports a non-local exit, the seven-word adapter
copies the runtime's in-flight exit into the bridge's pinned ARGS-SLOT, as
\(TAG . VALUE), and OUT-SLOT, as the flag 1 (signal) or 2 (throw).  ENV and
MARKER are the bridge's own active pin frame.  KIND is `throw' or `signal';
TAG and VALUE keep their identity.  Return nil when nothing was
published.  A failure to read the slots signals; the bridge then reports
that error instead of the exit it could not recover."
  (unless (fboundp 'nelisp-native-load-unbox)
    (require 'nelisp-native-load))
  (let ((flag (nelisp-native-load-unbox out-slot)))
    (when (memq flag '(1 2))
      (let ((pair (nelisp--native-unbox-reference args-slot env marker)))
        (and (consp pair)
             (cons (if (= flag 2) 'throw 'signal) pair))))))

(defun nelisp-eln-callable-import--frame-failure (frame)
  "Return the deferred exit FRAME recorded, as a failure value, or nil.
A throw is returned as (:throw TAG . VALUE); a signal as (SYMBOL . DATA)."
  (let ((exit (plist-get frame :exit)))
    (cond
     ((eq (car-safe exit) 'throw) (cons :throw (cdr exit)))
     ((eq (car-safe exit) 'signal) (cdr exit))
     (exit (list 'nelisp-eln-callable-import-error
                 (list 'uncaptured-nonlocal-exit)))
     (t (plist-get frame :condition)))))

(defun nelisp-eln-callable-import--resume (failure answer)
  "Resume FAILURE outside native code exactly as it was raised, else ANSWER."
  (cond ((eq (car-safe failure) :throw)
         (throw (cadr failure) (cddr failure)))
        (failure (signal (car failure) (cdr failure)))
        (t answer)))

(defun nelisp-eln-callable-import--args (descriptor &optional convention arity)
  "Read the callback words at DESCRIPTOR for CONVENTION and ARITY.
The `many' convention decodes the GNU (argc, argv) pair and reads no more
than ARITY words from the argument array.  The `fixed' convention (S6
caar/cadr's `wrong_type_argument' error call, and any other genuine
GNU C subr taking a small, fixed, unboxed argument count directly in
registers rather than through a MANY (argc, argv) pair) reads exactly
ARITY words straight from the callback's own raw word slots; ARITY
defaults to 1 and `unary' is a synonym for `fixed' with ARITY 1, kept
for the existing callers that already spell it that way."
  (unless (and (integerp descriptor) (> descriptor 4096))
    (signal 'nelisp-eln-callable-import-error (list 'invalid-descriptor)))
  (let ((words nil) (i 0))
    (while (< i 7)
      (push (nelisp-eln-abi-read-word descriptor (* i 8)) words)
      (setq i (1+ i)))
    (setq words (nreverse words))
    (if (eq convention 'many)
        ;; A MANY callee arrives through the seven-word entry (callback1
        ;; would zero-fill the argv pointer), so words past (ARGC, ARGV)
        ;; are unspecified caller registers: never read, never checked.
        (let ((argc (car words)) (argv (cadr words)) (decoded nil) (i 0))
          (unless (and (integerp arity) (>= arity 0)
                       (integerp argc) (>= argc 0) (<= argc arity))
            (signal 'nelisp-eln-callable-import-error
                    (list 'argument-count argc arity)))
          (unless (and (= argc arity) (integerp argv) (> argv 4096))
            (signal 'nelisp-eln-callable-import-error
                    (list 'invalid-many-arguments argc arity argv)))
          (while (< i argc)
            (push (nelisp-eln-abi-read-word argv (* i 8)) decoded)
            (setq i (1+ i)))
          (nreverse decoded))
      (unless (or (null convention) (eq convention 'unary) (eq convention 'fixed))
        (signal 'nelisp-eln-callable-import-error
                (list 'unsupported-convention convention)))
      (let ((arity (cond ((eq convention 'unary) 1)
                         ((null convention) 1)
                         (t arity))))
        (unless (and (integerp arity) (>= arity 1) (<= arity 6))
          (signal 'nelisp-eln-callable-import-error (list 'invalid-arity arity)))
        ;; Through callback1 every word past the first is zero-filled, so a
        ;; nonzero one is a protocol violation.  Through the seven-word
        ;; entry (fixed arity > 1) they are unspecified caller registers.
        (unless (or (nelisp-eln-callable-import-wide-entry-p convention arity)
                    (cl-every #'zerop (nthcdr arity words)))
          (signal 'nelisp-eln-callable-import-error
                  (list 'unexpected-extra-arguments (nthcdr arity words))))
        (let ((taken nil) (i 0))
          (while (< i arity)
            (push (nth i words) taken)
            (setq i (1+ i)))
          (nreverse taken))))))

(defun nelisp-eln-callable-import-current-frame ()
  "Return the innermost active native-call frame plist, or nil (Doc 210)."
  (car nelisp-eln-callable-import--frames))

(defun nelisp-eln-callable-import-frame-put (frame property value)
  "Public form of `nelisp-eln-callable-import--frame-put' (Doc 210)."
  (nelisp-eln-callable-import--frame-put frame property value))

(defun nelisp-eln-callable-import-decode-word (frame word)
  "Public form of `nelisp-eln-callable-import--decode-word' (Doc 210)."
  (nelisp-eln-callable-import--decode-word frame word))

(defun nelisp-eln-callable-import-adapter-context-address ()
  "Return the address of the callback adapter's `nl_eln_callback7_context'."
  (unless (fboundp 'nelisp-native-load--symbol-addr)
    (require 'nelisp-native-load))
  (nelisp-native-load--symbol-addr "nl_eln_callback7_context"))

(defun nelisp-eln-callable-import--frame-put (frame property value)
  "Set PROPERTY to VALUE in active FRAME, including the dynamic stack cell."
  (setq frame (plist-put frame property value))
  (when nelisp-eln-callable-import--frames
    (setcar nelisp-eln-callable-import--frames frame))
  frame)

(defun nelisp-eln-callable-import--port-words (descriptor spec)
  "Return SPEC's raw argument words from port callback DESCRIPTOR.
A MANY spec's :ARITY is the one admitted argc, or a list of every argc
the admitted body passes through that one slot (e.g. `Ffuncall' called
with 2 and with 3 arguments)."
  (let ((convention (plist-get spec :convention))
        (arity (plist-get spec :arity))
        (words nil) (i 0))
    (while (< i 6)
      (push (nelisp-eln-abi-read-word descriptor (* i 8)) words)
      (setq i (1+ i)))
    (setq words (nreverse words))
    (cond
     ((eq convention 'many)
      (let ((argc (car words)) (argv (cadr words)) (taken nil) (i 0))
        (unless (and (integerp argc)
                     (if (consp arity) (memql argc arity)
                       (and (integerp arity) (= argc arity)))
                     (integerp argv) (> argv 4096))
          (signal 'nelisp-eln-callable-import-error
                  (list 'invalid-many-arguments argc arity argv)))
        (while (< i argc)
          (push (nelisp-eln-abi-read-word argv (* i 8)) taken)
          (setq i (1+ i)))
        (nreverse taken)))
     ;; Arity 0 is a GNU C callee taking no argument at all (`maybe_gc',
     ;; `maybe_quit'): every register word is an unspecified caller value.
     ((and (eq convention 'fixed) (integerp arity) (<= 0 arity 6))
      (let ((taken nil) (i 0))
        (while (< i arity)
          (push (nth i words) taken)
          (setq i (1+ i)))
        (nreverse taken)))
     (t (signal 'nelisp-eln-callable-import-error
                (list 'unsupported-convention convention))))))

(defun nelisp-eln-callable-import--decode-word (frame word)
  "Decode GNU WORD for FRAME: an authenticated constant, an earlier port
callback's result, or else a view in the argument activation."
  (let ((constant
         (and (integerp word)
              (catch 'found
                (dolist (entry (plist-get frame :constants))
                  (when (and (integerp (car entry)) (= (car entry) word))
                    (throw 'found entry)))
                nil))))
    (cond
     (constant (cdr constant))
     ;; An opaque handle an earlier `handle' port answered (see
     ;; `nelisp-eln-callable-import--dispatch-port').
     ((and (integerp word) (assoc word (plist-get frame :handles)))
      (cdr (assoc word (plist-get frame :handles))))
     (t
      ;; The newest result activation leasing WORD decodes it; otherwise
      ;; the argument activation must, and signals when it does not.
      (nelisp-eln-objects-activation-decode
       (or (cl-find-if (lambda (activation)
                         (nelisp-eln-objects-activation-leases-word-p
                          activation word))
                       (plist-get frame :result-activations))
           (plist-get frame :argument-activation))
       word)))))

(declare-function nelisp-eln-handler-port-entry-sp "nelisp-eln-handler-port" ())
(declare-function nelisp-eln-handler-port-try-divert
  "nelisp-eln-handler-port" (frame failure))
(declare-function nelisp-eln-handler-port-frame-plist
  "nelisp-eln-handler-port" (capability options))
(declare-function nelisp-eln-handler-port-frame-retire
  "nelisp-eln-handler-port" (frame))

(defun nelisp-eln-callable-import--handler-hook (function &rest arguments)
  "Call the Doc 210 S9 handler-port FUNCTION with ARGUMENTS, loading it once.
Only activations opened with handler options ever reach this."
  (unless (fboundp 'nelisp-eln-handler-port-try-divert)
    (require 'nelisp-eln-handler-port))
  (apply function arguments))

(defun nelisp-eln-callable-import--divert (frame failure)
  "Offer FAILURE, raised inside a port callback of FRAME, to its native handlers.
Return nil (not handled: keep the Doc 207 path), (:diverted) when a native
CONDITION_CASE handler consumed it and the adapter will land in its pad, or
\(:failure ERROR) when the handler machinery failed closed."
  (and (plist-get frame :handler-range)
       (nelisp-eln-callable-import--handler-hook
        #'nelisp-eln-handler-port-try-divert frame failure)))

(defun nelisp-eln-callable-import--dispatch-port (descriptor frame)
  "Dispatch one slot-identifying port callback at DESCRIPTOR for FRAME.
Descriptor word 6 names the port; FRAME's `:ports' maps each port tag to
the authenticated slot spec (:convention :arity :implementation
:arguments :return) the caller installed there.  An unknown tag fails.
Argument kind `lisp' decodes a GNU word (see
`nelisp-eln-callable-import--decode-word'); `raw' passes the machine
word unchanged (a C int argument).  Return kind `lisp' encodes into the
result unit under a fresh activation, kept until the whole call returns
since native code may still hold earlier results; `bool' answers a C
bool in %al; `void' answers nothing (a zero pair)."
  (let* ((tag (nelisp-eln-abi-read-word descriptor 48))
         (spec (cdr (assoc tag (plist-get frame :ports))))
         (arguments nil))
    (unless spec
      (signal 'nelisp-eln-callable-import-error (list 'unknown-port tag)))
    (let* ((words (nelisp-eln-callable-import--port-words descriptor spec))
           (kinds (plist-get spec :arguments))
           ;; A MANY port admitting several argc values lists one
           ;; argument kind per position of its largest argc; a call with
           ;; fewer arguments uses that many leading kinds.
           (kinds (if (and (eq (plist-get spec :convention) 'many)
                           (consp (plist-get spec :arity))
                           (<= (length words) (length kinds)))
                      (cl-subseq kinds 0 (length words))
                    kinds)))
      (unless (= (length kinds) (length words))
        (signal 'nelisp-eln-callable-import-error
                (list 'port-argument-kinds kinds (length words))))
      (while words
        (push (if (eq (car kinds) 'raw)
                  (car words)
                (nelisp-eln-callable-import--decode-word frame (car words)))
              arguments)
        (setq words (cdr words) kinds (cdr kinds)))
      (setq arguments (nreverse arguments)))
    (let ((value (apply (plist-get spec :implementation) arguments)))
      (pcase (plist-get spec :return)
        ('bool (cons (if value 1 0) 0))
        ;; A C `void' callee: the native caller never reads %rax.
        ('void (cons 0 0))
        ;; A C integer or pointer result (Doc 210 `push_handler' answers the
        ;; address of its minted handler block): the machine word as is.
        ('raw
         (unless (and (integerp value) (<= 0 value) (< value 281474976710656))
           (signal 'nelisp-eln-callable-import-error
                   (list 'invalid-raw-result value)))
         (setq frame (nelisp-eln-callable-import--frame-put
                      frame :outcome :ok))
         (cons (logand value #xffffffff) (logand (ash value -32) #xffffffff)))
        ('lisp
         (let* ((word (nelisp-eln-objects-encode
                       (plist-get frame :result-unit) value))
                (activation (nelisp-eln-objects-activation-acquire
                             (plist-get frame :result-unit))))
           (setq frame (nelisp-eln-callable-import--frame-put
                        frame :result-activations
                        (cons activation
                              (plist-get frame :result-activations))))
           (setq frame (nelisp-eln-callable-import--frame-put
                        frame :result-activation activation))
           (setq frame (nelisp-eln-callable-import--frame-put
                        frame :outcome :ok))
           (cons (logand word #xffffffff)
                 (logand (ash word -32) #xffffffff))))
        ;; An object that only ever flows on to other authenticated ports
        ;; (an `Fmake_closure' result): native code gets a fresh, unique,
        ;; never-dereferenced word backed by an owned poison block, and the
        ;; frame remembers word -> object until the whole call retires.
        ;; S6.11: like `handle', except that a nil result is answered as
        ;; GNU's own nil word (0).  For a port whose result the exact body
        ;; only tests for nil, passes on to a later port or returns: a
        ;; non-nil object is never dereferenced, and nil-ness stays visible
        ;; to the body's inline `test'.
        ('handle-nil
         (if (null value)
             (progn
               (setq frame (nelisp-eln-callable-import--frame-put
                            frame :outcome :ok))
               (cons 0 0))
           (let* ((memory (nl-ffi-memory-allocate 16))
                  (address (nl-ffi-memory-address memory))
                  (word (+ address 5)))
             (ptr-write-u64 address 0 0)
             (ptr-write-u64 address 8 0)
             (setq frame (nelisp-eln-callable-import--frame-put
                          frame :handle-owners
                          (cons memory (plist-get frame :handle-owners))))
             (setq frame (nelisp-eln-callable-import--frame-put
                          frame :handles
                          (cons (cons word value)
                                (plist-get frame :handles))))
             (setq frame (nelisp-eln-callable-import--frame-put
                          frame :outcome :ok))
             (cons (logand word #xffffffff)
                   (logand (ash word -32) #xffffffff)))))
        ('handle
         (let* ((memory (nl-ffi-memory-allocate 16))
                (address (nl-ffi-memory-address memory))
                (word (+ address 5)))
           (ptr-write-u64 address 0 0)
           (ptr-write-u64 address 8 0)
           (setq frame (nelisp-eln-callable-import--frame-put
                        frame :handle-owners
                        (cons memory (plist-get frame :handle-owners))))
           (setq frame (nelisp-eln-callable-import--frame-put
                        frame :handles
                        (cons (cons word value)
                              (plist-get frame :handles))))
           (setq frame (nelisp-eln-callable-import--frame-put
                        frame :outcome :ok))
           (cons (logand word #xffffffff)
                 (logand (ash word -32) #xffffffff))))
        (_ (signal 'nelisp-eln-callable-import-error
                   (list 'unsupported-port-return
                         (plist-get spec :return))))))))

(defun nelisp-eln-callable-import--suppressed-answer (frame descriptor)
  "Return the answer pair of a callback whose Lisp is not run (S6.7).
Once an activation has recorded a non-local exit, GNU would already have left
the native frame, but the native body keeps executing until it returns; its
result is discarded and the exit resumed.  A port body may write through the
word an earlier `lisp' or `handle' port answered (an inline `setcar' of a
fresh cons), so such a port answers a word that is safe to dereference: a
cons-tagged word over a zeroed 16-byte block the frame owns until it retires
\(car and cdr both nil).  Every other callback, and a call that is not a port
call, answers the zero pair."
  (let* ((spec (and (plist-get frame :ports) (integerp descriptor)
                    (> descriptor 4096)
                    (cdr (assoc (nelisp-eln-abi-read-word descriptor 48)
                                (plist-get frame :ports)))))
         (kind (plist-get spec :return)))
    (cond
     ;; Doc 210: a raw-result port whose native caller dereferences the
     ;; answer (`push_handler') supplies its own safe placeholder.
     ((and (eq kind 'raw) (functionp (plist-get spec :suppressed-raw)))
      (let ((word (funcall (plist-get spec :suppressed-raw) frame)))
        (cons (logand word #xffffffff) (logand (ash word -32) #xffffffff))))
     ((memq kind '(lisp handle handle-nil))
        (let ((word (plist-get frame :suppressed-word)))
          (unless word
            (let* ((memory (nl-ffi-memory-allocate 16))
                   (address (nl-ffi-memory-address memory)))
              (ptr-write-u64 address 0 0)
              (ptr-write-u64 address 8 0)
              (setq word (+ address 3))
              (setq frame (nelisp-eln-callable-import--frame-put
                           frame :handle-owners
                           (cons memory (plist-get frame :handle-owners))))
              (nelisp-eln-callable-import--frame-put
               frame :suppressed-word word)))
          (cons (logand word #xffffffff) (logand (ash word -32) #xffffffff))))
     (t (cons 0 0)))))

(defun nelisp-eln-callable-import--record-failure (frame failure)
  "Handle FAILURE (an error or quit) trapped in a callback of FRAME.
A failure a native CONDITION_CASE handler of FRAME matches is consumed and
answered with a normal pair (the adapter then lands in the handler's pad).
Anything else is recorded as the frame's condition (Doc 207): the callback
ABI requires a pair, and the outer bridge ignores it when the saved
condition is present and re-signals after leaving native."
  (let ((verdict (nelisp-eln-callable-import--divert frame failure)))
    (setq frame (or (car nelisp-eln-callable-import--frames) frame))
    (if (eq (car-safe verdict) :diverted)
        (progn
          (nelisp-eln-callable-import--frame-put frame :outcome :ok)
          (cons 0 0))
      (when (eq (car-safe verdict) :failure)
        (setq failure (cadr verdict)))
      (setq frame (nelisp-eln-callable-import--frame-put
                   frame :condition failure))
      (nelisp-eln-callable-import--frame-put frame :outcome :condition)
      (cons 0 0))))

(defun nelisp-eln-callable-import--dispatch (descriptor)
  "Decode GNU arguments, invoke the active callable, and encode the result.
Doc 207 frame contract: once this native activation has recorded a
non-local exit, every later import callback of the same activation is
suppressed without running Lisp, because GNU would already have left the
native frame.  An error or quit is recorded as the frame's condition; a
throw is captured from the runtime stash while it passes this function's
`unwind-protect' and is resumed by the bridge after native code returns."
  (let ((frame (car nelisp-eln-callable-import--frames)) (completed nil))
    (unless frame
      (signal 'nelisp-eln-callable-import-error (list 'missing-call-frame)))
    (if (or (plist-get frame :exit) (plist-get frame :condition))
        (progn
          (nelisp-eln-callable-import--frame-put
           frame :suppressed (1+ (or (plist-get frame :suppressed) 0)))
          (nelisp-eln-callable-import--suppressed-answer frame descriptor))
      ;; Doc 210 S9: capture this callback's adapter stack pointer before
      ;; anything can call back into native code and overwrite it.
      (when (plist-get frame :handler-range)
        (setq frame (nelisp-eln-callable-import--frame-put
                     frame :entry-sp
                     (nelisp-eln-callable-import--handler-hook
                      #'nelisp-eln-handler-port-entry-sp))))
      (unwind-protect
          (prog1
              (condition-case failure
                  (if (plist-get frame :ports)
                      (progn
                        (unless (and (integerp descriptor) (> descriptor 4096))
                          (signal 'nelisp-eln-callable-import-error
                                  (list 'invalid-descriptor)))
                        (nelisp-eln-callable-import--dispatch-port
                         descriptor frame))
                    (let* ((words (nelisp-eln-callable-import--args
                                   descriptor (plist-get frame :convention)
                                   (plist-get frame :arity)))
                           (constants (plist-get frame :constants))
                           (arguments
                            (mapcar
                             (lambda (word)
                               ;; Compare numerically: GNU words above the
                               ;; fixnum range are not reliably `assoc'-able.
                               (let ((constant
                                      (and (integerp word)
                                           (catch 'found
                                             (dolist (entry constants)
                                               (when (and (integerp (car entry))
                                                          (= (car entry) word))
                                                 (throw 'found entry)))
                                             nil))))
                                 (if constant
                                     (cdr constant)
                                   (nelisp-eln-objects-activation-decode
                                    (plist-get frame :argument-activation)
                                    word))))
                             words))
                           (value (apply (plist-get frame :implementation)
                                         arguments))
                           (word (nelisp-eln-objects-encode
                                  (plist-get frame :result-unit) value))
                           (activation (nelisp-eln-objects-activation-acquire
                                        (plist-get frame :result-unit))))
                      (setq frame (nelisp-eln-callable-import--frame-put
                                   frame :result-activation activation))
                      (setq frame (nelisp-eln-callable-import--frame-put
                                   frame :outcome :ok))
                      (cons (logand word #xffffffff)
                            (logand (ash word -32) #xffffffff))))
                (error
                 (nelisp-eln-callable-import--record-failure frame failure))
                (quit
                 (nelisp-eln-callable-import--record-failure frame failure)))
            (setq completed t))
        (unless completed
          ;; A throw (or any exit the handlers above do not trap) is leaving
          ;; this callback.  Mark the activation so later callbacks are
          ;; suppressed; the adapter publishes the exit itself when the
          ;; gateway reports status 1, and the bridge resumes it.
          (nelisp-eln-callable-import--frame-put frame :exit :pending)
          (nelisp-eln-callable-import--frame-put frame :outcome :exit))))))

(defun nelisp-eln-callable-import--release-activations (box)
  "Release every activation in BOX, a cons whose car is the pending list.
Each activation leaves BOX only after its own release succeeds, so a
failure propagates to `nelisp-eln-callable-import--retire''s recording
handler with exactly the unreleased activations still retained in BOX
for `nelisp-eln-callable-import-retry-cleanup'."
  (while (car box)
    (nelisp-eln-objects-activation-release (car (car box)))
    (setcar box (cdr (car box))))
  t)

(defun nelisp-eln-callable-import--release-memories (box)
  "Release every owned memory block in BOX, a cons whose car is the pending
list, removing each only after its own release succeeded (as
`nelisp-eln-callable-import--release-activations' does)."
  (while (car box)
    (nl-ffi-memory-release (car (car box)))
    (setcar box (cdr (car box))))
  t)

(defun nelisp-eln-callable-import--retire (bundle)
  "Retire resources in BUNDLE in reference-safe order; return non-nil if done."
  (let ((failed nil))
    (when (plist-get bundle :callback-token)
      (unless (fboundp 'nelisp-native-load--symbol-addr)
        (require 'nelisp-native-load))
      (condition-case _err
          (if (= (ptr-call
                  (nelisp-native-load--symbol-addr
                   "nelisp_eln_callback_context_pop")
                  (plist-get bundle :callback-token) 0 0 0 0 0) 1)
              (setq bundle (plist-put bundle :callback-token nil))
            (setq failed t))
        (error (setq failed t))
        (quit (setq failed t))))
    ;; A failed pop means C may still reach the callback and every owner it
    ;; closes over; retain the entire bundle and stop cleanup here.
    (unless failed
      (when (and (plist-get bundle :pin-env) (plist-get bundle :pin-marker))
        (unless (fboundp 'nelisp-native-load--pin-end)
          (require 'nelisp-native-load))
        (condition-case _err
            (progn
              (nelisp-native-load--pin-end (plist-get bundle :pin-env)
                                           (plist-get bundle :pin-marker))
              (setq bundle (plist-put bundle :pin-marker nil)
                    bundle (plist-put bundle :pin-env nil)))
          (error (setq failed t))
          (quit (setq failed t))))
      (unless failed
        (when (and (plist-get bundle :state)
                   (plist-get bundle :old-status))
          (condition-case _err
              (progn
                (ptr-write-u64 (plist-get bundle :state) 24
                               (plist-get bundle :old-status))
                (setq bundle (plist-put bundle :old-status nil)
                      bundle (plist-put bundle :state nil)))
            (error (setq failed t))
            (quit (setq failed t))))
        (unless failed
          (dolist (field '(:result-activations :handle-owners
                           :result-activation :argument-activation
                           :result-unit :argument-unit :raw-context))
            (let ((value (plist-get bundle field)))
              (when (and value (not failed))
                (condition-case _err
                    (progn
                      (funcall (pcase field
                                 ((or :result-activation :argument-activation)
                                  #'nelisp-eln-objects-activation-release)
                                 (:result-activations
                                  #'nelisp-eln-callable-import--release-activations)
                                 (:handle-owners
                                  #'nelisp-eln-callable-import--release-memories)
                                 ((or :result-unit :argument-unit)
                                  #'nelisp-eln-objects-release)
                                 (:raw-context
                                  #'nelisp-eln-raw-call-context-release))
                               value)
                      (setq bundle (plist-put bundle field nil)))
                  (error (setq failed t))
                  (quit (setq failed t)))))))))
    (and (not failed)
         (cl-every (lambda (field) (null (plist-get bundle field)))
                   '(:callback-token :pin-marker :result-activation
                     :result-activations :handle-owners
                     :argument-activation :result-unit :argument-unit
                     :raw-context)))))

(defun nelisp-eln-callable-import-retry-cleanup ()
  "Retry safely retiring callback frames retained after cleanup failure."
  (let ((pending nelisp-eln-callable-import--pending-cleanups)
        (remaining nil))
    (while pending
      (let ((bundle (car pending)))
        (unless (nelisp-eln-callable-import--retire bundle)
          (push bundle remaining)))
      (setq pending (cdr pending)))
    (setq nelisp-eln-callable-import--pending-cleanups (nreverse remaining))
  (unless remaining
      t)))

(defun nelisp-eln-callable-import--call-unary
    (capability implementation argument &optional convention arity constants
                ports extra-arguments handler-options)
  "Call authenticated tail-import CAPABILITY with ARGUMENT via IMPLEMENTATION.
EXTRA-ARGUMENTS, when non-nil, lists further Lisp arguments passed after
ARGUMENT (a binary multi-import body; see
`nelisp-eln-native-subr-create-multi-binary'), encoded into the same
argument unit and activation.
IMPLEMENTATION is the already captured canonical unary callable object.
The caller must have validated CAPABILITY's complete code against the
tail-only import profile before invoking this internal bridge.
CONSTANTS, when non-nil, is an alist of (GNU-WORD . VALUE) for the
artifact's own load-time `d_reloc' constants whose identity the caller
has already authenticated (e.g. the S6 caar/cadr `listp' predicate the
body passes to `wrong_type_argument'); a callback argument word equal to
one of those GNU-WORDs decodes to its VALUE rather than through the
argument activation, which never contains the artifact's constants.
PORTS, when non-nil, is an alist of (PORT-TAG . SPEC) for a body whose
several authenticated imports each reach
`nelisp-eln-callable-import--dispatch' through their own port (see
`nelisp-eln-callable-import--dispatch-port'); IMPLEMENTATION, CONVENTION
and ARITY are then unused and IMPLEMENTATION must be nil.
HANDLER-OPTIONS, when non-nil, opens the activation for native
CONDITION_CASE handlers (Doc 210 S9): a plist naming the loaded artifact
\(:handle HANDLE) whose `_setjmp' binding and handler chain are verified
first.  The activation's port for `push_handler' mints the handlers, the
dispatcher offers matching errors to them, and the activation retires
fail-closed if a handler is left unpopped."
  (unless (fboundp 'nelisp-native-load--symbol-addr)
    (require 'nelisp-native-load))
  (when nelisp-eln-callable-import--pending-cleanups
    (signal 'nelisp-eln-callable-import-error
            (list 'cleanup-pending)))
  (unless (if ports
              (and (null implementation) (consp ports)
                   (cl-every (lambda (entry)
                               (let ((impl (plist-get (cdr entry)
                                                      :implementation)))
                                 (and (integerp (car entry))
                                      (or (and (functionp impl)
                                               (not (symbolp impl)))
                                          (and (consp impl)
                                               (eq (car impl) 'builtin))))))
                             ports))
            (and (functionp implementation) (not (symbolp implementation))))
    (signal 'nelisp-eln-callable-import-error
            (list 'expected-captured-callable implementation)))
  (setq capability
        (nelisp-eln-system-loader-validate-function-capability capability))
  (let ((argument-unit nil) (argument-activation nil)
        (result-unit nil) (context nil) (pin-env nil) (pin-marker nil)
        (callback-token nil) (function-slot nil) (args-slot nil) (out-slot nil)
        (state 0) (old-status nil) (raw nil) (rc nil) (frame nil)
        (failure nil) (cleanup-failure nil) (answer nil)
        (entry-specpdl-depth (nelisp-eln-runtime-services-specpdl-depth)))
    (unwind-protect
        (condition-case err
            (progn
              (setq argument-unit (nelisp-eln-objects-create)
                    result-unit (nelisp-eln-objects-create)
                    context (nelisp-eln-raw-call-context-create))
              (let ((argument-word
                     (nelisp-eln-objects-encode argument-unit argument))
                    (extra-words
                     (mapcar (lambda (extra)
                               (nelisp-eln-objects-encode argument-unit extra))
                             extra-arguments)))
                (setq argument-activation
                      (nelisp-eln-objects-activation-acquire argument-unit))
                (setq frame (append
                             (list :outcome :not-called :condition nil
                                   :result-activation nil
                                   :implementation implementation
                                   :convention (or convention 'unary)
                                   :arity (or arity 1)
                                   :constants constants
                                   :ports ports
                                   :result-activations nil
                                   :argument-activation argument-activation
                                   :result-unit result-unit)
                             (and handler-options
                                  (nelisp-eln-callable-import--handler-hook
                                   #'nelisp-eln-handler-port-frame-plist
                                   capability handler-options))))
                (let ((env (nelisp--native-env))
                      (push-address
                        (nelisp-native-load--symbol-addr
                         "nelisp_eln_callback_context_push"))
                      (gateway (nelisp-native-load--symbol-addr
                                "wf_bytecode_call_gateway")))
                  (setq state (nelisp-native-load--symbol-addr
                               "nl_eln_callback7_context"))
                  (unless (and (> push-address 0) (> state 0) (> gateway 0))
                    (signal 'nelisp-eln-callable-import-error
                            (list 'callback-runtime-unavailable)))
                  (setq old-status (ptr-read-u64 state 24))
                  (setq pin-env env
                        pin-marker (nelisp-native-load--pin-begin env)
                        function-slot (nelisp-native-load--pin-reserve env pin-marker)
                        args-slot (nelisp-native-load--pin-reserve env pin-marker)
                        out-slot (nelisp-native-load--pin-reserve env pin-marker))
                  (nelisp-native-load-box function-slot
                                          'nelisp-eln-callable-import--dispatch)
                  (setq callback-token
                        (ptr-call push-address gateway env function-slot
                                  args-slot out-slot 1))
                  (unless (> callback-token 0)
                    (signal 'nelisp-eln-callable-import-error
                            (list 'callback-context-refused)))
                  (ptr-write-u64 state 24 0)
                  (setq nelisp-eln-callable-import--frames
                        (cons frame nelisp-eln-callable-import--frames))
                  (setq raw
                        (nelisp-eln-raw-call-word
                         context (nth 3 capability)
                         (cons argument-word extra-words))
                        rc (ptr-read-u64 state 24)
                        frame (car nelisp-eln-callable-import--frames))
                  (when (eq (plist-get frame :exit) :pending)
                    (setq frame
                          (nelisp-eln-callable-import--frame-put
                           frame :exit
                           (or (nelisp-eln-callable-import--published-exit
                                pin-env pin-marker args-slot out-slot)
                               '(unknown)))))
                  (cond
                   ((nelisp-eln-callable-import--frame-failure frame)
                    (setq failure
                          (nelisp-eln-callable-import--frame-failure frame)))
                   ((not (eql rc 0))
                    (setq failure
                          (list 'nelisp-eln-callable-import-error
                                (list 'callback-status rc))))
                   ((and (eq (plist-get frame :outcome) :not-called)
                         (or (plist-get frame :ports)
                             (plist-get frame :constants)))
                    ;; The body may return one of its own authenticated
                    ;; constants (e.g. the artifact's `t') without calling.
                    (setq answer
                          (nelisp-eln-callable-import--decode-word
                           frame raw)))
                   ((eq (plist-get frame :outcome) :not-called)
                    (setq answer
                          (nelisp-eln-objects-activation-decode
                           argument-activation raw)))
                   ((and (eq (plist-get frame :outcome) :ok)
                         (plist-get frame :ports))
                    (setq answer
                          (nelisp-eln-callable-import--decode-word
                           frame raw)))
                   ((eq (plist-get frame :outcome) :ok)
                    (setq answer
                          (nelisp-eln-objects-activation-decode
                           (plist-get frame :result-activation) raw)))
                   (t
                    (setq failure
                          (list 'nelisp-eln-callable-import-error
                                (list 'invalid-callback-outcome
                                      (plist-get frame :outcome))))))))
              )
          (error (setq failure err))
          (quit (setq failure err)))
      ;; A specbind (freloc slot 12) genuine native code makes before
      ;; calling back into Lisp has no native unwind_to path back to on
      ;; a throw/signal that escapes past this whole native frame: the
      ;; native machine code's own local unwind-protect/cleanup handlers
      ;; never run, because the Lisp-level non-local exit unwinds only
      ;; through Lisp `unwind-protect'/`let' forms, never through an
      ;; opaque native call frame this bridge treats as a black box.
      ;; Force it closed here, unconditionally, before any re-signal
      ;; below, exactly as GNU's own `unbind_to' would if the native
      ;; frame's cleanup had actually run.
      (let ((leaked (- (nelisp-eln-runtime-services-specpdl-depth)
                       entry-specpdl-depth)))
        (when (> leaked 0)
          (nelisp-eln-runtime-services-helper-unbind-n leaked)))
      ;; Doc 210 S9: hand the handler chain back exactly as found, releasing
      ;; every block; a handler left unpopped is a fail-closed error.
      (when (and frame (or (plist-get frame :handler-range)
                           (plist-get frame :dummy-blocks)))
        (let ((current (car nelisp-eln-callable-import--frames)))
          (when (and current
                     (eq (plist-get current :argument-activation)
                         argument-activation))
            (setq frame current)))
        (let ((retire-failure
               (nelisp-eln-callable-import--handler-hook
                #'nelisp-eln-handler-port-frame-retire frame)))
          (when (and retire-failure (null failure))
            (setq failure retire-failure))))
      (when frame
        (when (eq (car nelisp-eln-callable-import--frames) frame)
          (setq nelisp-eln-callable-import--frames
                (cdr nelisp-eln-callable-import--frames))))
      (let ((bundle
             (list :callback-token callback-token :pin-env pin-env
                   :pin-marker pin-marker :state state :old-status old-status
                   :frame frame
                   :result-activation
                   (and (null (plist-get frame :result-activations))
                        (plist-get frame :result-activation))
                   :result-activations
                   (and (plist-get frame :result-activations)
                        (list (plist-get frame :result-activations)))
                   :handle-owners
                   (and (plist-get frame :handle-owners)
                        (list (plist-get frame :handle-owners)))
                   :argument-activation argument-activation
                   :result-unit result-unit :argument-unit argument-unit
                   :raw-context context)))
        (push bundle nelisp-eln-callable-import--pending-cleanups)
        (unless (nelisp-eln-callable-import--retire bundle)
          (setq cleanup-failure
                (list 'nelisp-eln-callable-import-error
                      (list 'cleanup-retained
                            (length nelisp-eln-callable-import--pending-cleanups)))))
        (when (and (null cleanup-failure)
                   (eq (car nelisp-eln-callable-import--pending-cleanups)
                       bundle))
          (setq nelisp-eln-callable-import--pending-cleanups
                (cdr nelisp-eln-callable-import--pending-cleanups)))))
    (if (and (null failure) cleanup-failure)
        (signal (car cleanup-failure) (cdr cleanup-failure))
      (nelisp-eln-callable-import--resume failure answer))))

(defun nelisp-eln-callable-import--call-chain
    (capability implementation arguments &optional convention arity constants
                ports)
  "Call authenticated chain-import CAPABILITY with ARGUMENTS via IMPLEMENTATION.

Ledger S4.6 sibling of `nelisp-eln-callable-import--call-unary',
generalized in how many words the OUTER native call itself sends
\(ARGUMENTS, a list, one word per element, in order).  CONVENTION,
ARITY, CONSTANTS and PORTS mean what they mean for `--call-unary'.  The
admitted chain leaf passes PORTS (Doc 207): its non-tail Fadd1 slow-path
import and its non-tail `Ffuncall' MANY import each reach the dispatcher
through their own slot-identifying port, so every callback of the one
native activation is answered by its own authenticated identity.  The
caller must have validated CAPABILITY's complete code against the
chain-import profile before invoking this internal bridge.  Cleanup and
exit discipline (exactly-once release on normal return, error, throw or
quit; later callbacks of an exited activation suppressed; the exit
resumed with its original objects) is identical to `--call-unary'."
  (unless (fboundp 'nelisp-native-load--symbol-addr)
    (require 'nelisp-native-load))
  (when nelisp-eln-callable-import--pending-cleanups
    (signal 'nelisp-eln-callable-import-error
            (list 'cleanup-pending)))
  (unless (if ports
              (and (null implementation) (consp ports)
                   (cl-every (lambda (entry)
                               (let ((impl (plist-get (cdr entry)
                                                      :implementation)))
                                 (and (integerp (car entry))
                                      (or (and (functionp impl)
                                               (not (symbolp impl)))
                                          (and (consp impl)
                                               (eq (car impl) 'builtin))))))
                             ports))
            (and (functionp implementation) (not (symbolp implementation))))
    (signal 'nelisp-eln-callable-import-error
            (list 'expected-captured-callable implementation)))
  (unless (and (listp arguments) (proper-list-p arguments) arguments)
    (signal 'nelisp-eln-callable-import-error
            (list 'expected-argument-list arguments)))
  (setq capability
        (nelisp-eln-system-loader-validate-function-capability capability))
  (let ((argument-unit nil) (argument-activation nil)
        (result-unit nil) (context nil) (pin-env nil) (pin-marker nil)
        (callback-token nil) (function-slot nil) (args-slot nil) (out-slot nil)
        (state 0) (old-status nil) (raw nil) (rc nil) (frame nil)
        (failure nil) (cleanup-failure nil) (answer nil)
        (entry-specpdl-depth (nelisp-eln-runtime-services-specpdl-depth)))
    (unwind-protect
        (condition-case err
            (progn
              (setq argument-unit (nelisp-eln-objects-create)
                    result-unit (nelisp-eln-objects-create)
                    context (nelisp-eln-raw-call-context-create))
              (let ((argument-words
                     (mapcar (lambda (argument)
                               (nelisp-eln-objects-encode argument-unit argument))
                             arguments)))
                (setq argument-activation
                      (nelisp-eln-objects-activation-acquire argument-unit))
                (setq frame (list :outcome :not-called :condition nil
                                  :constants constants
                                  :ports ports
                                  :result-activations nil
                                  :result-activation nil
                                  :implementation implementation
                                  :convention (or convention 'unary)
                                  :arity (or arity 1)
                                  :argument-activation argument-activation
                                  :result-unit result-unit))
                (let ((env (nelisp--native-env))
                      (push-address
                        (nelisp-native-load--symbol-addr
                         "nelisp_eln_callback_context_push"))
                      (gateway (nelisp-native-load--symbol-addr
                                "wf_bytecode_call_gateway")))
                  (setq state (nelisp-native-load--symbol-addr
                               "nl_eln_callback7_context"))
                  (unless (and (> push-address 0) (> state 0) (> gateway 0))
                    (signal 'nelisp-eln-callable-import-error
                            (list 'callback-runtime-unavailable)))
                  (setq old-status (ptr-read-u64 state 24))
                  (setq pin-env env
                        pin-marker (nelisp-native-load--pin-begin env)
                        function-slot (nelisp-native-load--pin-reserve env pin-marker)
                        args-slot (nelisp-native-load--pin-reserve env pin-marker)
                        out-slot (nelisp-native-load--pin-reserve env pin-marker))
                  (nelisp-native-load-box function-slot
                                          'nelisp-eln-callable-import--dispatch)
                  (setq callback-token
                        (ptr-call push-address gateway env function-slot
                                  args-slot out-slot 1))
                  (unless (> callback-token 0)
                    (signal 'nelisp-eln-callable-import-error
                            (list 'callback-context-refused)))
                  (ptr-write-u64 state 24 0)
                  (setq nelisp-eln-callable-import--frames
                        (cons frame nelisp-eln-callable-import--frames))
                  (setq raw
                        (nelisp-eln-raw-call-word
                         context (nth 3 capability) argument-words)
                        rc (ptr-read-u64 state 24)
                        frame (car nelisp-eln-callable-import--frames))
                  (when (eq (plist-get frame :exit) :pending)
                    (setq frame
                          (nelisp-eln-callable-import--frame-put
                           frame :exit
                           (or (nelisp-eln-callable-import--published-exit
                                pin-env pin-marker args-slot out-slot)
                               '(unknown)))))
                  (cond
                   ((nelisp-eln-callable-import--frame-failure frame)
                    (setq failure
                          (nelisp-eln-callable-import--frame-failure frame)))
                   ((not (eql rc 0))
                    (setq failure
                          (list 'nelisp-eln-callable-import-error
                                (list 'callback-status rc))))
                   ((eq (plist-get frame :outcome) :not-called)
                    (setq answer
                          (nelisp-eln-objects-activation-decode
                           argument-activation raw)))
                   ((and (eq (plist-get frame :outcome) :ok)
                         (plist-get frame :ports))
                    (setq answer
                          (nelisp-eln-callable-import--decode-word
                           frame raw)))
                   ((eq (plist-get frame :outcome) :ok)
                    (setq answer
                          (nelisp-eln-objects-activation-decode
                           (plist-get frame :result-activation) raw)))
                   (t
                    (setq failure
                          (list 'nelisp-eln-callable-import-error
                                (list 'invalid-callback-outcome
                                      (plist-get frame :outcome))))))))
              )
          (error (setq failure err))
          (quit (setq failure err)))
      ;; See `--call-unary's identical comment: force closed any
      ;; specpdl entries a native frame's own specbind left dangling
      ;; when a throw/signal escaped past it without running that
      ;; frame's own (never-executed) native cleanup.
      (let ((leaked (- (nelisp-eln-runtime-services-specpdl-depth)
                       entry-specpdl-depth)))
        (when (> leaked 0)
          (nelisp-eln-runtime-services-helper-unbind-n leaked)))
      (when frame
        (when (eq (car nelisp-eln-callable-import--frames) frame)
          (setq nelisp-eln-callable-import--frames
                (cdr nelisp-eln-callable-import--frames))))
      (let ((bundle
             (list :callback-token callback-token :pin-env pin-env
                   :pin-marker pin-marker :state state :old-status old-status
                   :frame frame
                   :result-activation
                   (and (null (plist-get frame :result-activations))
                        (plist-get frame :result-activation))
                   :result-activations
                   (and (plist-get frame :result-activations)
                        (list (plist-get frame :result-activations)))
                   :argument-activation argument-activation
                   :result-unit result-unit :argument-unit argument-unit
                   :raw-context context)))
        (push bundle nelisp-eln-callable-import--pending-cleanups)
        (unless (nelisp-eln-callable-import--retire bundle)
          (setq cleanup-failure
                (list 'nelisp-eln-callable-import-error
                      (list 'cleanup-retained
                            (length nelisp-eln-callable-import--pending-cleanups)))))
        (when (and (null cleanup-failure)
                   (eq (car nelisp-eln-callable-import--pending-cleanups)
                       bundle))
          (setq nelisp-eln-callable-import--pending-cleanups
                (cdr nelisp-eln-callable-import--pending-cleanups)))))
    (if (and (null failure) cleanup-failure)
        (signal (car cleanup-failure) (cdr cleanup-failure))
      (nelisp-eln-callable-import--resume failure answer))))

(provide 'nelisp-eln-callable-import)

;;; nelisp-eln-callable-import.el ends here
