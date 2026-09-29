;;; nelisp-eln-handler-port.el --- Doc 210 S9 native handler semantics -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 210 (docs/design/210-eln-native-handlers.org) stage S9: what happens
;; when GNU-native `condition-case' code (`push_handler' + an inline
;; `_setjmp') runs on NeLisp.  Built on the S8 substrate
;; (`nelisp-eln-handler-substrate'): shadow thread and handler blocks, the
;; artifact's `current_thread_reloc' chain and the private setjmp bound to
;; its GOT slot.
;;
;; - `nelisp-eln-handler-port-push' is the `push_handler' import port.  It
;;   mints a GNU-layout CONDITION_CASE handler block (type +0x00, tag +0x08,
;;   next +0x20, pdlcount +0x110), links it into the shadow thread block and
;;   records it on the rooted activation frame.  Any type other than
;;   CONDITION_CASE is refused.
;; - `nelisp-eln-handler-port-try-divert' is offered every error or quit a
;;   port callback of a handler-bearing activation raised.  It validates the
;;   native-visible chain against the frame's records, walks the records
;;   innermost first with GNU `find_handler_clause', runs the GNU
;;   `signal_or_quit' debugger decision, unwinds the specpdl handler by
;;   handler exactly like `unwind_to_catch' (a cleanup that raises replaces
;;   the exit and matching restarts from the handler being unwound), and on
;;   a match asks the callback adapter to land in the handler's pad by
;;   writing the resume request.  Anything unmatched returns nil and keeps
;;   the Doc 207 suppress-and-resume path.
;; - `nelisp-eln-handler-port-frame-retire' hands the chain back at the end
;;   of the activation and fails closed if a handler was left unpopped.
;;
;; Handlers installed by Lisp `handler-bind' inside the guarded region run
;; when the error is raised, before this code sees it, because the
;; dispatcher's own `condition-case' is outside them; handlers outside the
;; native frame are never consulted, exactly as GNU stops its walk at the
;; native CONDITION_CASE.

;;; Code:

(require 'cl-lib)
(require 'nelisp-eln-callable-import)
(require 'nelisp-eln-runtime-services)
(require 'nelisp-eln-handler-substrate)

(define-error 'nelisp-eln-handler-port-error
  "NeLisp .eln native handler error")

(defconst nelisp-eln-handler-port-condition-case 1
  "GNU `enum handlertype' CONDITION_CASE.")
(defconst nelisp-eln-handler-port-max-live 64
  "Most handlers one activation may have minted and not yet retired.")
(defconst nelisp-eln-handler-port--type-offset 0)
(defconst nelisp-eln-handler-port--tag-offset 8)
(defconst nelisp-eln-handler-port--exit-offset #x10)
(defconst nelisp-eln-handler-port--val-offset #x18)
(defconst nelisp-eln-handler-port--pdlcount-offset #x110)
(defconst nelisp-eln-handler-port--jmp-rsp-offset (+ #x40 48))
(defconst nelisp-eln-handler-port--jmp-rip-offset (+ #x40 56))
(defconst nelisp-eln-handler-port--resume-offset 672
  "Offset of the adapter's resume block from `nl_eln_callback7_context'.
Must equal `nelisp-cc-eln-callback7-resume-offset' (checked by the tests).")
(defconst nelisp-eln-handler-port--not-catchable
  '(nelisp-eln-handler-port-error nelisp-eln-handler-substrate-error
    nelisp-eln-callable-import-error)
  "Runtime fail-closed errors a native handler may never catch.")

(defun nelisp-eln-handler-port--fail (reason &optional detail)
  (signal 'nelisp-eln-handler-port-error (list reason detail)))

;;;; Adapter resume block ---------------------------------------------

(defun nelisp-eln-handler-port--resume-block ()
  (let ((context (nelisp-eln-callable-import-adapter-context-address)))
    (unless (and (integerp context) (> context 4096))
      (nelisp-eln-handler-port--fail 'adapter-context-unavailable))
    (+ context nelisp-eln-handler-port--resume-offset)))

(defun nelisp-eln-handler-port-entry-sp ()
  "Return the stack pointer the adapter published for the current port entry."
  (ptr-read-u64 (nelisp-eln-handler-port--resume-block) 16))

(defun nelisp-eln-handler-port-landing-count ()
  "Return how many landings the adapter has performed in this process."
  (ptr-read-u64 (nelisp-eln-handler-port--resume-block) 56))

;;;; Frame records ---------------------------------------------------

(defun nelisp-eln-handler-port--frame-put (frame key value)
  (nelisp-eln-callable-import-frame-put frame key value))

(defun nelisp-eln-handler-port-frame-plist (capability options)
  "Return the handler keys of a new activation of CAPABILITY.
OPTIONS is (:handle HANDLE).  The artifact's `_setjmp' binding and handler
chain are re-verified for every activation."
  (let ((handle (plist-get options :handle))
        (start (nth 3 capability))
        (size (nth 6 capability)))
    (unless (and handle (integerp start) (integerp size) (> size 0))
      (nelisp-eln-handler-port--fail 'handler-options-invalid options))
    (nelisp-eln-handler-substrate-verify-setjmp-binding handle)
    (unless (= (nelisp-eln-handler-substrate-artifact-handlerlist handle)
               (nelisp-eln-handler-substrate-handlerlist))
      (nelisp-eln-handler-port--fail 'artifact-chain-mismatch))
    (list :handler-range (cons start (+ start size))
          :handler-base (nelisp-eln-handler-substrate-handlerlist)
          :native-handlers nil
          :landed-blocks nil
          :dummy-blocks nil
          :handler-landings 0)))

(defun nelisp-eln-handler-port--records (frame)
  (plist-get frame :native-handlers))

(defun nelisp-eln-handler-port--thread-cell-ok ()
  (= (ptr-read-u64 (nelisp-eln-handler-substrate-current-thread-cell) 0)
     (nelisp-eln-handler-substrate-thread-address)))

(defun nelisp-eln-handler-port--set-handlerlist (address)
  (ptr-write-u64 (nelisp-eln-handler-substrate-thread-address)
                 nelisp-eln-handler-substrate-handlerlist-offset address))

(defun nelisp-eln-handler-port--sync (frame)
  "Validate the native-visible chain against FRAME's records; return the live ones.
The chain from `thread->handlerlist' must be a suffix of the frame's records
\(innermost first; the native pop only moves the head) followed by the
activation's base chain.  Records native code already popped, and blocks of
handlers already landed on, are released.  Anything else signals: a forged
chain never reaches a landing."
  (unless (nelisp-eln-handler-port--thread-cell-ok)
    (nelisp-eln-handler-port--fail 'chain-thread-cell-forged))
  (let* ((records (nelisp-eln-handler-port--records frame))
         (base (plist-get frame :handler-base))
         (node (nelisp-eln-handler-substrate-handlerlist))
         (remaining records)
         (popped nil))
    ;; Records the native code has popped: the head moved past them.
    (while (and remaining (not (eql node (plist-get (car remaining) :block)))
                (not (eql node base)))
      (push (car remaining) popped)
      (setq remaining (cdr remaining)))
    (when (and (null remaining) (not (eql node base)))
      (nelisp-eln-handler-port--fail 'chain-forged node))
    (when (and remaining (eql node base))
      ;; every remaining record was popped as well
      (while remaining
        (push (car remaining) popped)
        (setq remaining (cdr remaining))))
    ;; The live suffix must be exactly the record blocks, then the base.
    (let ((walk remaining))
      (while walk
        (let* ((record (car walk))
               (block (plist-get record :block))
               (expected (if (cdr walk)
                             (plist-get (cadr walk) :block)
                           base)))
          (unless (and (nelisp-eln-handler-substrate-block-live-p block)
                       (eql (ptr-read-u64 block nelisp-eln-handler-port--type-offset)
                            nelisp-eln-handler-port-condition-case)
                       (eql (ptr-read-u64
                             block nelisp-eln-handler-substrate-handler-next-offset)
                            expected)
                       (eql (ptr-read-u64
                             block nelisp-eln-handler-port--pdlcount-offset)
                            (plist-get record :pdlcount)))
            (nelisp-eln-handler-port--fail 'chain-forged block)))
        (setq walk (cdr walk))))
    (dolist (record popped)
      (when (nelisp-eln-handler-substrate-block-live-p (plist-get record :block))
        (nelisp-eln-handler-substrate-release-block (plist-get record :block))))
    (dolist (block (plist-get frame :landed-blocks))
      (when (nelisp-eln-handler-substrate-block-live-p block)
        (nelisp-eln-handler-substrate-release-block block)))
    (nelisp-eln-handler-port--frame-put frame :landed-blocks nil)
    (nelisp-eln-handler-port--frame-put frame :native-handlers remaining)
    remaining))

;;;; S9.1: the push_handler port -------------------------------------

(defun nelisp-eln-handler-port-mint (frame tag-word tag type)
  "Mint a GNU-layout handler block for FRAME; return its address.
TAG-WORD is the authenticated GNU word the body passed and TAG its decoded
condition list.  Only CONDITION_CASE is admitted."
  (unless (plist-get frame :handler-range)
    (nelisp-eln-handler-port--fail 'activation-not-handler-bearing))
  (unless (eql type nelisp-eln-handler-port-condition-case)
    (nelisp-eln-handler-port--fail 'handler-type-rejected type))
  (let ((records (nelisp-eln-handler-port--sync frame)))
    (when (>= (length records) nelisp-eln-handler-port-max-live)
      (nelisp-eln-handler-port--fail 'too-many-live-handlers (length records)))
    (let* ((block (nelisp-eln-handler-substrate-allocate-block))
           (pdlcount (nelisp-eln-runtime-services-specpdl-depth)))
      (ptr-write-u64 block nelisp-eln-handler-port--type-offset type)
      (ptr-write-u64 block nelisp-eln-handler-port--tag-offset tag-word)
      (ptr-write-u64 block nelisp-eln-handler-substrate-handler-next-offset
                     (nelisp-eln-handler-substrate-handlerlist))
      (ptr-write-u64 block nelisp-eln-handler-port--pdlcount-offset pdlcount)
      (nelisp-eln-handler-port--frame-put
       frame :native-handlers
       (cons (list :block block :type type :tag tag :tag-word tag-word
                   :pdlcount pdlcount :sp (plist-get frame :entry-sp))
             records))
      (nelisp-eln-handler-port--set-handlerlist block)
      block)))

(defun nelisp-eln-handler-port--dummy-push (frame)
  "Link a placeholder handler block for FRAME and return its address.
Native code that called `push_handler' will run `_setjmp' on the returned
block and later pop exactly one handler; when the push itself failed (or
was suppressed) it must still get writable memory and a balanced chain, or
the fail-closed path would crash the process.  The block is never recorded
as a handler, so nothing can land on it; the frame releases it at retire."
  (let ((block (nelisp-eln-handler-substrate-allocate-block)))
    ;; A frame not opened for handlers has no recorded base yet.
    (unless (plist-get frame :handler-base)
      (nelisp-eln-handler-port--frame-put
       frame :handler-base (nelisp-eln-handler-substrate-handlerlist)))
    (ptr-write-u64 block nelisp-eln-handler-port--type-offset
                   nelisp-eln-handler-port-condition-case)
    (ptr-write-u64 block nelisp-eln-handler-substrate-handler-next-offset
                   (nelisp-eln-handler-substrate-handlerlist))
    (nelisp-eln-handler-port--set-handlerlist block)
    (nelisp-eln-handler-port--frame-put
     frame :dummy-blocks (cons block (plist-get frame :dummy-blocks)))
    block))

(defun nelisp-eln-handler-port-suppressed-push (frame)
  "Answer of a `push_handler' callback made after FRAME recorded an exit."
  (nelisp-eln-handler-port--dummy-push frame))

(defun nelisp-eln-handler-port-push (tag-word type)
  "Implementation of the `push_handler' import port (both arguments raw).
TAG-WORD must be one of the body's authenticated constants.  A refused push
records the reason as the activation's condition (so it is signalled to the
caller once native code returns and every later callback is suppressed) and
answers a placeholder block, never an unusable address."
  (let ((frame (nelisp-eln-callable-import-current-frame)))
    (unless frame
      (nelisp-eln-handler-port--fail 'no-active-frame))
    (condition-case err
        (nelisp-eln-handler-port-mint
         frame tag-word
         (nelisp-eln-callable-import-decode-word frame tag-word)
         type)
      (error
       (nelisp-eln-handler-port--frame-put frame :condition err)
       (nelisp-eln-handler-port--frame-put frame :outcome :condition)
       (nelisp-eln-handler-port--dummy-push frame)))))

(defun nelisp-eln-handler-port-push-port-spec ()
  "Return the dispatcher spec of the `push_handler' import port."
  (list :convention 'fixed :arity 2 :arguments '(raw raw) :return 'raw
        :implementation (lambda (tag-word type)
                          (nelisp-eln-handler-port-push tag-word type))
        :suppressed-raw #'nelisp-eln-handler-port-suppressed-push))

;;;; S9.2: GNU handler matching --------------------------------------

(defun nelisp-eln-handler-port-find-clause (handlers conditions)
  "GNU `find_handler_clause': return HANDLERS when it matches CONDITIONS, else nil.
A non-list HANDLERS is returned as is; a list matches when one element is
`memq' in CONDITIONS or is `t'."
  (if (not (consp handlers))
      handlers
    (let ((tail handlers) (found nil))
      (while (and (consp tail) (not found))
        (when (or (memq (car tail) conditions) (eq (car tail) t))
          (setq found handlers))
        (setq tail (cdr tail)))
      found)))

(defun nelisp-eln-handler-port--conditions (failure)
  (and (symbolp (car-safe failure))
       (get (car failure) 'error-conditions)))

(defun nelisp-eln-handler-port--match (records failure)
  "Return the first of RECORDS (innermost first) whose tag matches FAILURE."
  (let ((conditions (nelisp-eln-handler-port--conditions failure)))
    (and conditions
         (cl-find-if
          (lambda (record)
            (and (eql (plist-get record :type)
                      nelisp-eln-handler-port-condition-case)
                 (nelisp-eln-handler-port-find-clause
                  (plist-get record :tag) conditions)))
          records))))

;;;; S9.6: the GNU debugger decision ---------------------------------

(defun nelisp-eln-handler-port--quit-p (failure)
  (let ((signal (car-safe failure)))
    (or (eq signal 'quit)
        (and (symbolp signal)
             (memq 'quit (get signal 'error-conditions))
             t))))

(defun nelisp-eln-handler-port--wants-debugger (list conditions)
  "GNU `wants_debugger'."
  (cond ((null list) nil)
        ((not (consp list)) t)
        (t (let ((found nil))
             (while (and (consp conditions) (not found))
               (when (memq (car conditions) list) (setq found t))
               (setq conditions (cdr conditions)))
             found))))

(defun nelisp-eln-handler-port--skip-debugger (conditions failure)
  "GNU `skip_debugger': honour `debug-ignored-errors'."
  (let ((tail debug-ignored-errors) (message nil) (skip nil))
    (while (and (consp tail) (not skip))
      (if (stringp (car tail))
          (progn
            (unless message (setq message (error-message-string failure)))
            (when (string-match-p (car tail) message) (setq skip t)))
        (let ((contail conditions))
          (while (and (consp contail) (not skip))
            (when (eq (car tail) (car contail)) (setq skip t))
            (setq contail (cdr contail)))))
      (setq tail (cdr tail)))
    skip))

(defun nelisp-eln-handler-port--maybe-call-debugger (conditions failure)
  "GNU `maybe_call_debugger'; return non-nil when the debugger was called."
  (when (and (null inhibit-debugger)
             (if (nelisp-eln-handler-port--quit-p failure)
                 debug-on-quit
               (nelisp-eln-handler-port--wants-debugger debug-on-error
                                                        conditions))
             (not (nelisp-eln-handler-port--skip-debugger conditions failure))
             ;; `num_nonmacro_input_events' is 0 outside an editing session.
             (< internal-when-entered-debugger 0))
    (setq internal-when-entered-debugger 0)
    (let ((debugger-may-continue t)
          (inhibit-redisplay nil)
          (inhibit-debugger t)
          (inhibit-changing-match-data nil))
      (apply debugger (list 'error failure)))
    t))

(defun nelisp-eln-handler-port--debugger-decision (clause conditions failure)
  "The `signal_or_quit' condition for calling the debugger, given a matched CLAUSE."
  (when (or debug-on-signal
            (eq clause 'debug)
            (and (consp clause) (memq 'debug clause))
            (eq clause 'error))
    (nelisp-eln-handler-port--maybe-call-debugger conditions failure)))

;;;; S9.3/S9.4: unwind and divert ------------------------------------

(defun nelisp-eln-handler-port--unbind-to (pdlcount)
  "GNU `unbind_to' on the native specpdl analogue."
  (let ((excess (- (nelisp-eln-runtime-services-specpdl-depth) pdlcount)))
    (when (> excess 0)
      (nelisp-eln-runtime-services-helper-unbind-n excess))))

(defun nelisp-eln-handler-port--validate-landing (frame record)
  "Fail closed unless RECORD can be landed on from the current callback."
  (let* ((block (plist-get record :block))
         (buffer (+ block #x40))
         (rip (ptr-read-u64 block nelisp-eln-handler-port--jmp-rip-offset))
         (rsp (ptr-read-u64 block nelisp-eln-handler-port--jmp-rsp-offset))
         (range (plist-get frame :handler-range))
         (sp (plist-get frame :entry-sp)))
    (unless (and (integerp sp) (eql sp (plist-get record :sp)))
      (nelisp-eln-handler-port--fail 'handler-frame-mismatch
                                     (list sp (plist-get record :sp))))
    (unless (and (>= rip (car range)) (< rip (cdr range)))
      (nelisp-eln-handler-port--fail 'handler-rip-out-of-body rip))
    (unless (and (= 0 (logand rsp 15)) (> rsp sp) (<= (- rsp sp) 4096))
      (nelisp-eln-handler-port--fail 'handler-rsp-invalid (list rsp sp)))
    (unless (= 0 (logand buffer 15))
      (nelisp-eln-handler-port--fail 'handler-buffer-misaligned buffer))))

(defun nelisp-eln-handler-port--commit-landing (frame target records)
  "Consume TARGET (a member of RECORDS): release the inner records, publish
the handler's exit state and post the landing request for the adapter."
  (let ((block (plist-get target :block))
        (rest records))
    (while (not (eq (car rest) target))
      (nelisp-eln-handler-substrate-release-block (plist-get (car rest) :block))
      (setq rest (cdr rest)))
    (setq rest (cdr rest))
    (ptr-write-u64 block nelisp-eln-handler-port--exit-offset 1)
    (ptr-write-u64 block nelisp-eln-handler-port--val-offset 0)
    (nelisp-eln-handler-port--set-handlerlist block)
    (nelisp-eln-handler-port--frame-put frame :native-handlers rest)
    (nelisp-eln-handler-port--frame-put
     frame :landed-blocks (cons block (plist-get frame :landed-blocks)))
    (nelisp-eln-handler-port--frame-put
     frame :handler-landings (1+ (or (plist-get frame :handler-landings) 0)))
    (let ((resume (nelisp-eln-handler-port--resume-block))
          (range (plist-get frame :handler-range))
          (bounds (nelisp-eln-handler-substrate-region-bounds)))
      (ptr-write-u64 resume 8
                     (nelisp-eln-handler-substrate-landing-stub-address))
      (ptr-write-u64 resume 24 (car range))
      (ptr-write-u64 resume 32 (cdr range))
      (ptr-write-u64 resume 40 (car bounds))
      (ptr-write-u64 resume 48 (cdr bounds))
      ;; The request word goes last.
      (ptr-write-u64 resume 0 (+ block #x40)))))

(defun nelisp-eln-handler-port--divert (frame failure)
  (let ((records (nelisp-eln-handler-port--sync frame))
        (original failure)
        (result nil) (done nil))
    (while (not done)
      (let ((target (nelisp-eln-handler-port--match records failure)))
        (if (null target)
            ;; Nothing (left) wants it.  A replacement exit raised by a
            ;; cleanup is reported; the original exit keeps the Doc 207 path.
            (setq done t
                  result (and (not (eq failure original))
                              (list :failure failure)))
          (nelisp-eln-handler-port--validate-landing frame target)
          (nelisp-eln-handler-port--debugger-decision
           (nelisp-eln-handler-port-find-clause
            (plist-get target :tag)
            (nelisp-eln-handler-port--conditions failure))
           (nelisp-eln-handler-port--conditions failure) failure)
          ;; unwind_to_catch: handler by handler, innermost first.
          (let ((level records) (raised nil))
            (while (and level (not raised))
              (let ((record (car level)))
                (setq raised
                      (condition-case new
                          (progn (nelisp-eln-handler-port--unbind-to
                                  (plist-get record :pdlcount))
                                 nil)
                        (error new)
                        (quit new)))
                (cond
                 (raised
                  ;; The cleanup's exit replaces the current one; matching
                  ;; restarts at the handler being unwound (GNU has not yet
                  ;; advanced `handlerlist' past it).
                  (setq failure raised records level))
                 ((eq record target)
                  (nelisp-eln-handler-port--commit-landing frame target records)
                  (setq done t result (list :diverted)
                        level nil))
                 (t (setq level (cdr level))))))
            (when (and raised (null (nelisp-eln-handler-port--match
                                     records failure)))
              ;; No remaining native handler wants the replacement exit: the
              ;; chain stays as it is and the Doc 207 path takes it.
              (setq done t result (list :failure failure)))))))
    result))

(defun nelisp-eln-handler-port-try-divert (frame failure)
  "Offer FAILURE to FRAME's native handlers.  See the file commentary.
Return nil, (:diverted) or (:failure ERROR)."
  (cond
   ((memq (car-safe failure) nelisp-eln-handler-port--not-catchable) nil)
   ((null (nelisp-eln-handler-port--records frame)) nil)
   (t (condition-case err
          (nelisp-eln-handler-port--divert frame failure)
        (error (list :failure err))))))

;;;; Retirement ------------------------------------------------------

(defun nelisp-eln-handler-port-frame-retire (frame)
  "Restore the activation's base chain and release its blocks.
Return nil, or a failure (SYMBOL . DATA) when the chain was not left as the
native code should have left it (a handler unpopped, or a moved head)."
  (let* ((base (plist-get frame :handler-base))
         (node (nelisp-eln-handler-substrate-handlerlist))
         (records (nelisp-eln-handler-port--records frame))
         (failure nil))
    (unless (eql node base)
      (setq failure
            (list 'nelisp-eln-handler-port-error
                  (if records 'unpopped-handlers 'chain-not-restored)
                  (list node base))))
    (dolist (record records)
      (when (nelisp-eln-handler-substrate-block-live-p (plist-get record :block))
        (nelisp-eln-handler-substrate-release-block (plist-get record :block))))
    (dolist (block (plist-get frame :landed-blocks))
      (when (nelisp-eln-handler-substrate-block-live-p block)
        (nelisp-eln-handler-substrate-release-block block)))
    (dolist (block (plist-get frame :dummy-blocks))
      (when (nelisp-eln-handler-substrate-block-live-p block)
        (nelisp-eln-handler-substrate-release-block block)))
    (nelisp-eln-handler-port--frame-put frame :native-handlers nil)
    (nelisp-eln-handler-port--frame-put frame :landed-blocks nil)
    (nelisp-eln-handler-port--frame-put frame :dummy-blocks nil)
    (nelisp-eln-handler-port--set-handlerlist base)
    ;; A request the adapter never consumed must not outlive its activation.
    (ptr-write-u64 (nelisp-eln-handler-port--resume-block) 0 0)
    failure))

(provide 'nelisp-eln-handler-port)

;;; nelisp-eln-handler-port.el ends here
