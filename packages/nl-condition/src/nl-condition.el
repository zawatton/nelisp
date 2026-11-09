;;; nl-condition.el --- Restartable condition system for NeLisp -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 169 (nl-prelude / nl-condition) Phase 4: a Common-Lisp-style
;; condition system with restarts, as an opt-in library on plain Elisp
;; (runs on NeLisp standalone and host Emacs alike).
;;
;; Why: Elisp `condition-case' unwinds the stack BEFORE running the
;; handler, so by the time the handler sees the error, the dynamic
;; context of the error site is gone.  CL `handler-bind' runs handlers
;; BEFORE unwinding, which is what makes restarts possible: the
;; handler, still standing on the signaling frame, picks a recovery
;; strategy that some outer frame offered.
;;
;;   (nl-restart-case
;;       (nl-handler-bind ((file-missing
;;                          (lambda (c) (nl-invoke-restart 'use-default))))
;;         (load-config "/etc/app.conf"))
;;     (use-default () default-config)
;;     (retry (path) (load-config path)))
;;
;; Public API:
;;   `nl-signal'            signal without unwinding first
;;   `nl-signal-condition'  run handlers only; nil when unhandled
;;   `nl-handler-bind'      bind handlers that run pre-unwind
;;   `nl-restart-case'      offer named restarts around a form
;;   `nl-restart-bind'      offer restarts whose function runs in place
;;   `nl-with-simple-restart'  one parameterless restart around a body
;;   `nl-invoke-restart'    transfer to a restart
;;   `nl-find-restart'      is a restart currently established?
;;   `nl-compute-restarts'  names of established restarts, innermost first
;;   `nl-cerror' `nl-warn' `nl-check-type' `nl-assert'
;;                          signalling forms with a recovery restart
;;   `nl-abort' `nl-continue' `nl-muffle-warning' `nl-use-value'
;;   `nl-store-value'       invoke the matching standard restart
;;
;; Semantics (CL model, scoped to this library):
;;   - `nl-handler-bind' handlers run for `nl-signal' and, when the
;;     runtime provides `handler-bind-1', also for errors signalled by
;;     plain `signal', `error' and primitives such as (car 1).  Without
;;     `handler-bind-1' (for example Emacs 29.4) only `nl-signal'
;;     reaches them.  No global advice is installed.
;;   - On a runtime whose `handler-bind-1' is a true primitive (GNU
;;     Emacs 30+) those handlers run pre-unwind.  On NeLisp standalone
;;     `handler-bind-1' is a documented `condition-case' approximation:
;;     a handler for a PLAIN `signal' or primitive error runs after the
;;     stack has unwound to the `nl-handler-bind' frame, so only
;;     restarts established OUTSIDE that frame are visible to it.
;;     `nl-signal' itself always runs handlers pre-unwind.
;;   - A handler that RETURNS declines: the next (outer) matching
;;     handler runs.  When every handler declines, `nl-signal' falls
;;     back to a plain `signal', so `condition-case' interop works.
;;     That fallback does not fire the same handlers again through
;;     the `handler-bind-1' bridge.
;;   - While a handler runs, only handlers OUTER to it are visible,
;;     preventing self-recursion on re-signal.
;;   - `nl-invoke-restart' on an `nl-restart-case' restart throws to
;;     its frame; the restart body runs AFTER unwinding (so
;;     `unwind-protect' cleanups between the signal point and the
;;     restart frame have run), with the frame already popped.  A
;;     restart from `nl-restart-bind' instead runs its function in
;;     place, without unwinding, and `nl-invoke-restart' returns its
;;     value.
;;
;; The condition object handed to handlers is (ERROR-SYMBOL . DATA),
;; the same shape `condition-case' binds.

;;; Code:

(require 'nl-prelude)

(define-error 'nl-control-error
  "Invoked a restart that is not established" 'nl-error)

;;;; Handler stack ----------------------------------------------------

(defvar nl--handler-stack nil
  "List of handler frames, innermost first.
Each frame is a list of (CONDITION . HANDLER-FN) pairs.")

(defvar nl--restart-stack nil
  "List of restart frames, innermost first.
Each frame is (CATCH-TAG (NAME . FN)...).")

(defun nl--condition-matches-p (condition error-symbol)
  "Return non-nil when CONDITION covers ERROR-SYMBOL.
Matching follows `condition-case': CONDITION must appear in
ERROR-SYMBOL's `error-conditions' (an undeclared ERROR-SYMBOL matches
itself and `error')."
  (let ((conditions (or (get error-symbol 'error-conditions)
                        (list error-symbol 'error))))
    (memq condition conditions)))

;; Bindings that `nl-signal' has already run, so the `handler-bind-1'
;; bridge does not run them a second time when nl-signal falls back to
;; a plain `signal'.  Entries are (BINDING ERROR-SYMBOL DATA).  The
;; list is a global, not a `let' binding, because on a runtime whose
;; `handler-bind-1' runs handlers after unwinding a dynamic binding
;; would already be gone.  It is bounded; an entry only matters while
;; its error is propagating.
(defvar nl--fired nil
  "Recently run handler bindings, to suppress double firing.")

(defconst nl--fired-limit 64
  "Maximum number of entries kept in `nl--fired'.")

(defun nl--run-handlers (error-symbol data)
  "Run the matching `nl-handler-bind' handlers for ERROR-SYMBOL and DATA.
Handlers run innermost first with only outer handlers visible.
Return the list of bindings that ran (all of them returned)."
  (let ((condition-object (cons error-symbol data))
        (frames nl--handler-stack)
        (ran nil))
    (while frames
      (let ((outer (cdr frames)))
        (dolist (binding (car frames))
          (when (nl--condition-matches-p (car binding) error-symbol)
            (let ((nl--handler-stack outer))
              (funcall (cdr binding) condition-object))
            (setq ran (cons binding ran)))))
      (setq frames (cdr frames)))
    ran))

(defun nl--note-fired (bindings error-symbol data)
  "Remember that BINDINGS already ran for the signal of ERROR-SYMBOL, DATA.
DATA must be the very object passed to `signal' (see `nl-signal')."
  (dolist (b bindings)
    (setq nl--fired (cons (list b error-symbol data) nl--fired)))
  (when (> (length nl--fired) nl--fired-limit)
    (setcdr (nthcdr (1- nl--fired-limit) nl--fired) nil)))

(defun nl--fired-p (binding condition-object)
  "Return non-nil when BINDING already ran for CONDITION-OBJECT.
Consume the matching entry.  A cons DATA is matched by `eq' against
the fresh copy `nl-signal' signalled, so it identifies exactly that
signal.  Atom or nil DATA cannot be told apart by identity; such
entries match on the symbol and `equal' data and so may, rarely,
suppress a later signal with the same symbol and the same atom data."
  (let ((entries nl--fired)
        (hit nil))
    (while (and entries (not hit))
      (let ((e (car entries)))
        (when (and (eq (car e) binding)
                   (eq (nth 1 e) (car condition-object))
                   (if (consp (nth 2 e))
                       (eq (nth 2 e) (cdr condition-object))
                     (equal (nth 2 e) (cdr condition-object))))
          (setq hit e)))
      (setq entries (cdr entries)))
    (when hit
      (setq nl--fired (delq hit nl--fired)))
    (and hit t)))

(defun nl-signal (error-symbol data)
  "Signal ERROR-SYMBOL with DATA without unwinding the stack first.
Run matching `nl-handler-bind' handlers innermost first, each with the
condition object (ERROR-SYMBOL . DATA) and with only outer handlers
visible.  A handler escapes by non-local exit (typically
`nl-invoke-restart'); a handler that returns declines.  When no
handler handles the condition, fall back to a plain `signal' of
ERROR-SYMBOL with a fresh copy of DATA, which `condition-case' can
catch as usual.  Each handler runs exactly once: the copy lets the
`handler-bind-1' bridge of `nl-handler-bind' recognise this very
signal and skip handlers that already ran."
  (let ((ran (nl--run-handlers error-symbol data))
        (fresh (if (consp data) (copy-sequence data) data)))
    (when ran
      (nl--note-fired ran error-symbol fresh))
    (signal error-symbol fresh)))

(defun nl-signal-condition (error-symbol data)
  "Run the handlers for ERROR-SYMBOL and DATA, then return nil.
Like CL `signal': handlers bound by `nl-handler-bind' run pre-unwind,
a handler may escape by non-local exit, and when none does this
returns nil instead of signalling.  ERROR-SYMBOL need not be declared
with `define-error'; an undeclared symbol matches itself and `error'."
  (nl--run-handlers error-symbol data)
  nil)

(defun nl--bridge-plist (frame outer)
  "Return the `handler-bind-1' argument list that mirrors FRAME.
OUTER is the handler stack outside FRAME.  Each wrapper skips
bindings that `nl-signal' already ran."
  (let ((plist nil))
    (dolist (binding frame)
      (let ((b binding))
        (setq plist
              (cons (lambda (err)
                      (unless (nl--fired-p b err)
                        (let ((nl--handler-stack outer))
                          (funcall (cdr b) err))))
                    (cons (list (car b)) plist)))))
    (nreverse plist)))

(defun nl--with-handlers (frame thunk)
  "Call THUNK with handler FRAME pushed on `nl--handler-stack'.
When `handler-bind-1' exists, also register FRAME with it so that
errors from plain `signal' and primitives reach the handlers."
  (let* ((outer nl--handler-stack)
         (nl--handler-stack (cons frame outer)))
    (if (and frame (fboundp 'handler-bind-1))
        (apply 'handler-bind-1 thunk (nl--bridge-plist frame outer))
      (funcall thunk))))

(defmacro nl-handler-bind (bindings &rest body)
  "Run BODY with BINDINGS as pre-unwind condition handlers.
BINDINGS is a list of (CONDITION HANDLER-FN) where CONDITION is an
error symbol (matched like `condition-case' conditions) and HANDLER-FN
a function of one argument, the condition object (ERROR-SYMBOL . DATA).
Handlers fire for `nl-signal' and, when the runtime has
`handler-bind-1', also for errors signalled by `signal', `error' and
primitives, so a handler can invoke a restart to recover from (car 1).
Without `handler-bind-1' only `nl-signal' reaches them.  A running
handler sees only outer handlers.  A handler that returns declines;
it escapes by non-local exit.  See the file commentary for what a
handler of a plain `signal' can see on NeLisp standalone."
  (declare (indent 1))
  `(nl--with-handlers
    (list ,@(mapcar (lambda (b)
                      (unless (and (consp b) (symbolp (car b))
                                   (consp (cdr b)) (null (cddr b)))
                        (error "nl-handler-bind: bad binding %S" b))
                      `(cons ',(car b) ,(nth 1 b)))
                    bindings))
    (lambda () ,@body)))

;;;; Restarts ---------------------------------------------------------

(defmacro nl-restart-case (form &rest restarts)
  "Evaluate FORM with RESTARTS established.
Each restart is (NAME ARGLIST BODY...).  When `nl-invoke-restart'
transfers here, the stack is unwound to this frame (running
`unwind-protect' cleanups on the way), the restart frame is popped,
and the restart's BODY runs with ARGLIST bound to the invocation
arguments; its value becomes the value of the whole form."
  (declare (indent 1))
  (let ((tag (gensym "nl--restart-tag-"))
        (payload (gensym "nl--restart-payload-")))
    (dolist (r restarts)
      (unless (and (consp r) (symbolp (car r)) (car r)
                   (consp (cdr r)) (listp (nth 1 r)))
        (error "nl-restart-case: bad restart clause %S" r)))
    `(let ((,payload
            (catch ',tag
              (let ((nl--restart-stack
                     (cons (list ',tag
                                 ,@(mapcar (lambda (r)
                                             `(cons ',(car r)
                                                    (lambda ,(nth 1 r)
                                                      ,@(cddr r))))
                                           restarts))
                           nl--restart-stack)))
                (cons 'nl--value ,form)))))
       (if (eq (car ,payload) 'nl--value)
           (cdr ,payload)
         (apply (cadr ,payload) (cddr ,payload))))))

;; A frame whose tag is nil belongs to `nl-restart-bind'.
(defmacro nl-restart-bind (bindings &rest body)
  "Run BODY with restarts from BINDINGS established.
BINDINGS is a list of (NAME FUNCTION-FORM).  Unlike `nl-restart-case',
`nl-invoke-restart' does not unwind: it calls the function on top of
the current stack with the invocation arguments, and returns its value
to the invoker, as in CL `restart-bind'."
  (declare (indent 1))
  (dolist (b bindings)
    (unless (and (consp b) (symbolp (car b)) (car b)
                 (consp (cdr b)) (null (cddr b)))
      (error "nl-restart-bind: bad binding %S" b)))
  `(let ((nl--restart-stack
          (cons (list nil
                      ,@(mapcar (lambda (b) `(cons ',(car b) ,(nth 1 b)))
                                bindings))
                nl--restart-stack)))
     ,@body))

(defmacro nl-with-simple-restart (name-and-format &rest body)
  "Run BODY with a parameterless restart established.
NAME-AND-FORMAT is (NAME FORMAT-STRING FORMAT-ARGS...); the format
is accepted for CL compatibility and is not evaluated.  Return a
two-element list standing in for CL's two values: (VALUE nil) when
BODY completes, with VALUE the value of its last form, and (nil t)
when the restart NAME was invoked."
  (declare (indent 1))
  (unless (and (consp name-and-format) (symbolp (car name-and-format))
               (car name-and-format))
    (error "nl-with-simple-restart: bad restart spec %S" name-and-format))
  `(nl-restart-case (list (progn ,@body) nil)
     (,(car name-and-format) (&rest _args) (list nil t))))

(defun nl--find-restart-frame (name)
  "Return (TAG . FN) for the innermost established restart NAME."
  (let ((frames nl--restart-stack)
        (found nil))
    (while (and frames (not found))
      (let ((hit (assq name (cdr (car frames)))))
        (when hit
          (setq found (cons (car (car frames)) (cdr hit)))))
      (setq frames (cdr frames)))
    found))

(defun nl-find-restart (name)
  "Return non-nil when restart NAME is currently established."
  (and (nl--find-restart-frame name) name))

(defun nl-compute-restarts ()
  "Return the names of all established restarts, innermost first."
  (let ((names nil))
    (dolist (frame nl--restart-stack)
      (dolist (r (cdr frame))
        (push (car r) names)))
    (nreverse names)))

(defun nl-invoke-restart (name &rest args)
  "Transfer control to the innermost established restart NAME.
For an `nl-restart-case' restart, unwind to its frame (running
`unwind-protect' cleanups), then run the restart body with ARGS.  For
an `nl-restart-bind' restart, call its function with ARGS in place
and return its value.  Signal `nl-control-error' when NAME is not
established."
  (let ((found (nl--find-restart-frame name)))
    (unless found
      (signal 'nl-control-error (list name)))
    (if (car found)
        (throw (car found) (cons 'nl--restart (cons (cdr found) args)))
      ;; `nl-restart-bind' restart: run in place, no unwinding.
      (apply (cdr found) args))))

;;;; Signalling forms with recovery restarts --------------------------

(define-error 'nl-assertion-failed "Assertion failed")

;; A warning is not an error: its own root, so `error' handlers skip it.
(put 'nl-warning 'error-conditions '(nl-warning))
(put 'nl-warning 'error-message "Warning")

(defun nl-cerror (continue-string error-symbol &optional data)
  "Signal ERROR-SYMBOL with DATA with a `continue' restart established.
CONTINUE-STRING describes the restart (kept for CL compatibility).
If a handler invokes `continue', return nil.  If no handler takes a
non-local exit, the error propagates as with `nl-signal'."
  (ignore continue-string)
  (nl-restart-case (nl-signal error-symbol data)
    (continue () nil)))

(defun nl--invoke-if-established (name &rest args)
  "Invoke restart NAME with ARGS if established, else return nil."
  (when (nl-find-restart name)
    (apply #'nl-invoke-restart name args)))

(defun nl-abort (&optional _condition)
  "Invoke the `abort' restart; signal `nl-control-error' if none exists."
  (nl-invoke-restart 'abort))

(defun nl-continue (&optional _condition)
  "Invoke the `continue' restart if established, else return nil."
  (nl--invoke-if-established 'continue))

(defun nl-muffle-warning (&optional _condition)
  "Invoke the `muffle-warning' restart if established, else return nil."
  (nl--invoke-if-established 'muffle-warning))

(defun nl-use-value (value &optional _condition)
  "Invoke the `use-value' restart with VALUE if established, else nil."
  (nl--invoke-if-established 'use-value value))

(defun nl-store-value (value &optional _condition)
  "Invoke the `store-value' restart with VALUE if established, else nil."
  (nl--invoke-if-established 'store-value value))

(defvar nl-warn-report-function #'nl--warn-report
  "Function called with the warning text when `nl-warn' is not muffled.")

(defun nl--warn-report (text)
  "Report warning TEXT through `message'."
  (message "Warning: %s" text))

(defun nl-warn (format-string &rest args)
  "Signal an `nl-warning' condition for the formatted message.
The condition is (nl-warning TEXT).  A `muffle-warning' restart is
established while handlers run, so `nl-muffle-warning' suppresses the
report.  Otherwise TEXT is passed to `nl-warn-report-function'
\(default: `message').  Return nil."
  (let ((text (apply #'format format-string args)))
    (nl-restart-case
        (progn
          (nl-signal-condition 'nl-warning (list text))
          (funcall nl-warn-report-function text)
          nil)
      (muffle-warning () nil))))

(defun nl--typep (value type)
  "Return non-nil when VALUE is of TYPE (`cl-typep', else TYPEp/TYPE-p)."
  (if (fboundp 'cl-typep)
      (cl-typep value type)
    (let* ((name (symbol-name type))
           (pred (or (intern-soft (concat name "p"))
                     (intern-soft (concat name "-p")))))
      (and pred (funcall pred value)))))

(defun nl--check-type-fail (place-form value type)
  "Signal `wrong-type-argument' with a `store-value' restart.
Return the replacement value chosen by the restart."
  (nl-restart-case
      (nl-signal 'wrong-type-argument (list type value place-form))
    (store-value (new) new)))

(defmacro nl-check-type (place type)
  "Signal `wrong-type-argument' unless the value of PLACE has TYPE.
Correctable: a handler may invoke `store-value' with a new value,
which is stored into PLACE with `setf' and checked again.  PLACE is
evaluated more than once."
  `(while (not (nl--typep ,place ',type))
     (setf ,place (nl--check-type-fail ',place ,place ',type))))

(defun nl--assert-fail (test-form)
  "Signal `nl-assertion-failed' for TEST-FORM with a `continue' restart.
Return the list of replacement values given to the restart."
  (nl-restart-case
      (nl-signal 'nl-assertion-failed (list test-form))
    (continue (&rest values) values)))

(defmacro nl-assert (test &optional places)
  "Signal `nl-assertion-failed' unless TEST is non-nil.
Correctable: a handler may invoke `continue', optionally with one new
value for each of PLACES in order.  The values are stored with `setf'
and TEST is evaluated again, so a handler that continues without
fixing the cause makes the assertion fail again, as in CL."
  (let ((vals (make-symbol "values")))
    `(while (not ,test)
       (let ((,vals (nl--assert-fail ',test)))
         ,@(let ((i -1))
             (mapcar (lambda (place)
                       (setq i (1+ i))
                       `(when (nthcdr ,i ,vals)
                          (setf ,place (nth ,i ,vals))))
                     places))))))

(provide 'nl-condition)

;;; nl-condition.el ends here