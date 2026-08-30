;;; nl-check.el --- Expansion-time checks for the nl-* family -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 170 section 10: the expansion-time checking pass, kept in its own
;; package so it can always be removed.  Doc 168 section 4.1 fixes the
;; dependency direction: `nl-check' depends on the feature packages and
;; NOTHING depends on `nl-check'.  Nothing here runs at run time --
;; callers invoke it explicitly, from a batch job or from CI.
;;
;; Public API:
;;
;;   Registry:   `nl-must-use' `nl-check-must-use-p' `nl-check-must-use-list'
;;   Checking:   `nl-check-form' `nl-check-forms' `nl-check-file'
;;   Reporting:  `nl-check-report' `nl-check-findings-of-kind'
;;
;; A finding is a plist:
;;
;;   (:kind KIND :subject SYMBOL :form FORM [:count N])
;;
;;   must-use-discarded  a `nl-must-use' call's value is thrown away
;;   resource-leak       a resource bound by `let' is never consumed
;;   resource-double     a path consumes the same resource twice
;;   resource-untracked  the resource left this checker's sight
;;   unsafe-call         an unsafe primitive called outside `nl-unsafe'
;;   unsafe-call-quoted  the same, inside a quoted form -- reported by
;;                       `nl-check-file-quoted-unsafe', never by
;;                       `nl-check-file', and never gated
;;
;; Soundness is deliberately partial, and says so.  Doc 170 section 6.3:
;; the moment a resource is captured by a lambda or handed to a function
;; this pass does not model, tracking is abandoned and the finding is
;; reported as `resource-untracked' rather than silently dropped.  An
;; empty `resource-leak' list therefore means "no leak among the
;; bindings I could follow", never "no leaks".

;;; Code:

(require 'nl-prelude)
(require 'nl-safe)
(require 'nl-resource)

;;;; must-use registry (Doc 170 section 6.2) ---------------------------

(defvar nl-check--must-use (make-hash-table :test 'eq)
  "Set of function symbols whose return value must be used.")

(defmacro nl-must-use (&rest names)
  "Declare each of NAMES as a function whose result must be used.
The Elisp counterpart of Rust's `#[must_use]'.  Discarding such a
call is reported by `nl-check-form' as `must-use-discarded'.  Wrap a
deliberate discard in `ignore' to silence it, the way Rust uses
`let _ ='."
  `(progn
     ,@(mapcar (lambda (name)
                 (unless (and name (symbolp name))
                   (error "nl-must-use: NAME must be a symbol, got %S" name))
                 `(puthash ',name t nl-check--must-use))
               names)
     ',names))

(defun nl-check-must-use-p (symbol)
  "Return non-nil when SYMBOL is registered as must-use."
  (and (symbolp symbol) (gethash symbol nl-check--must-use)))

(defun nl-check-must-use-list ()
  "Return the registered must-use function symbols, sorted by name."
  (let ((names nil))
    (maphash (lambda (k _v) (setq names (cons k names))) nl-check--must-use)
    (sort names (lambda (a b) (string< (symbol-name a) (symbol-name b))))))

;;;; Shared walker vocabulary ------------------------------------------

(defconst nl-check--sequence-heads
  '(progn inline save-excursion save-restriction save-match-data
          save-current-buffer with-no-warnings)
  "Heads whose whole argument list is a body sequence.")

(defconst nl-check--resource-observers
  '(nl-resource-handle nl-resource-handle-unchecked nl-resource-live-p
    nl-resource-type nl-resource-p nl-drop nl-forget
    ;; `ignore' is the sanctioned "I am deliberately not using this",
    ;; the same role `let _ =' plays in Rust, and this file already
    ;; treats it that way for must-use.  It takes nothing: reading it as
    ;; a move reported every body of `nl-with-resource' -- the RAII
    ;; macro whose whole purpose is to get the drop right -- as an
    ;; untracked resource, once the checks started looking through
    ;; expansion and could see the `(ignore r)' inside.
    ignore)
  "Calls that inspect or consume a resource without moving ownership.
Passing a tracked resource to anything else is treated as a move and
reported as `resource-untracked'.")

(defun nl-check--body-of (form)
  "Return (BODY . VALUE-INDEX) description for FORM, or nil.
BODY is the list of forms evaluated in sequence; every element but the
last sits in statement position."
  (cond
   ((memq (car form) nl-check--sequence-heads) (cdr form))
   ((memq (car form) '(let let*)) (cdr (cdr form)))
   ((memq (car form) '(when unless)) (cdr (cdr form)))
   ((eq (car form) 'lambda) (cdr (cdr form)))
   ((memq (car form) '(defun nl-defun defmacro nl-defmacro))
    (cdr (cdr (cdr form))))
   (t nil)))

;;;; must-use checking --------------------------------------------------

(defun nl-check--must-use-scan (form statement-p findings)
  "Collect must-use findings in FORM onto FINDINGS and return it.
STATEMENT-P is non-nil when FORM's value is discarded."
  (cond
   ((not (consp form)) findings)
   ((nl--walk-quoted-p form) findings)
   ((nl--walk-backquote-p form)
    (dolist (part (nl--walk-live-parts form) findings)
      (setq findings (nl-check--must-use-scan part t findings))))
   ;; `ignore' is the sanctioned discard, like Rust's `let _ ='.
   ((eq (car form) 'ignore) findings)
   (t
    (when (and statement-p (nl-check-must-use-p (car form)))
      (setq findings
            (cons (list :kind 'must-use-discarded
                        :subject (car form)
                        :form form)
                  findings)))
    (nl-check--must-use-children form findings))))

(defun nl-check--must-use-seq (body findings)
  "Scan BODY, treating every element but the last as a statement."
  (while (consp body)
    (setq findings (nl-check--must-use-scan
                    (car body) (consp (cdr body)) findings))
    (setq body (cdr body)))
  findings)

(defun nl-check--must-use-children (form findings)
  "Scan the sub-forms of the compound FORM."
  (let ((head (car form))
        (body (nl-check--body-of form)))
    (cond
     ;; Binding inits are value positions; the body is a sequence.
     ((memq head '(let let*))
      (dolist (binding (nl--walk-proper-list (car (cdr form))))
        (when (consp binding)
          (setq findings (nl-check--must-use-seq (cdr binding) findings))))
      (nl-check--must-use-seq body findings))
     ((memq head '(when unless))
      (setq findings (nl-check--must-use-scan (nth 1 form) nil findings))
      (nl-check--must-use-seq body findings))
     ((eq head 'while)
      (setq findings (nl-check--must-use-scan (nth 1 form) nil findings))
      ;; Every form in a `while' body is a statement.
      (dolist (sub (nl--walk-proper-list (cdr (cdr form))))
        (setq findings (nl-check--must-use-scan sub t findings)))
      findings)
     ((eq head 'if)
      (setq findings (nl-check--must-use-scan (nth 1 form) nil findings))
      (setq findings (nl-check--must-use-scan (nth 2 form) nil findings))
      (nl-check--must-use-seq (nthcdr 3 form) findings))
     ((eq head 'cond)
      (dolist (clause (nl--walk-proper-list (cdr form)))
        (when (consp clause)
          (setq findings (nl-check--must-use-scan (car clause) nil findings))
          (setq findings (nl-check--must-use-seq (cdr clause) findings))))
      findings)
     ((eq head 'prog1)
      (setq findings (nl-check--must-use-scan (nth 1 form) nil findings))
      (dolist (sub (nl--walk-proper-list (cdr (cdr form))))
        (setq findings (nl-check--must-use-scan sub t findings)))
      findings)
     ((eq head 'unwind-protect)
      (setq findings (nl-check--must-use-scan (nth 1 form) nil findings))
      (dolist (sub (nl--walk-proper-list (cdr (cdr form))))
        (setq findings (nl-check--must-use-scan sub t findings)))
      findings)
     ((eq head 'setq)
      (let ((tail (cdr form)))
        (while (consp tail)
          (setq findings (nl-check--must-use-scan
                          (car (cdr tail)) nil findings))
          (setq tail (cdr (cdr tail)))))
      findings)
     (body (nl-check--must-use-seq body findings))
     ;; Ordinary call: every argument is a value position.
     (t
      (dolist (sub (nl--walk-proper-list (cdr form)))
        (setq findings (nl-check--must-use-scan sub nil findings)))
      findings))))

;;;; Resource linearity (Doc 170 section 6.3) --------------------------

(defun nl-check--mentions-p (form var)
  "Return non-nil when VAR appears anywhere in FORM outside a quote."
  (cond
   ((eq form var) t)
   ((not (consp form)) nil)
   ((nl--walk-quoted-p form) nil)
   (t (let ((tail form) (found nil))
        (while (and (consp tail) (not found))
          (setq found (nl-check--mentions-p (car tail) var))
          (setq tail (cdr tail)))
        (or found (and tail (not (consp tail)) (eq tail var)))))))

(defun nl-check--escapes-p (form var)
  "Return non-nil when VAR leaves the checker's sight inside FORM.
Capture by a lambda, or being handed to any call other than the
resource observers, counts as an escape (Doc 170 section 6.3)."
  (cond
   ((not (consp form)) nil)
   ((nl--walk-quoted-p form) nil)
   ((nl--walk-backquote-p form)
    (nl-check--escapes-seq (nl--walk-live-parts form) var))
   ((memq (car form) '(lambda closure))
    (nl-check--mentions-p (cdr form) var))
   ((and (eq (car form) 'function) (consp (car (cdr form))))
    (nl-check--mentions-p (car (cdr form)) var))
   ((memq (car form) nl-check--resource-observers)
    ;; Fine as a direct argument, but not nested inside another call.
    (nl-check--escapes-seq (cdr form) var))
   ;; `cond' clauses and `let' bindings look like calls but are not:
   ;; walk their elements instead of treating the head as a callee.
   ((eq (car form) 'cond)
    (let ((clauses (cdr form)) (found nil))
      (while (and (consp clauses) (not found))
        (setq found (nl-check--escapes-seq (car clauses) var))
        (setq clauses (cdr clauses)))
      found))
   ((memq (car form) '(let let*))
    (let ((bindings (car (cdr form))) (found nil))
      (while (and (consp bindings) (not found))
        (setq found (and (consp (car bindings))
                         (nl-check--escapes-seq (cdr (car bindings)) var)))
        (setq bindings (cdr bindings)))
      (or found (nl-check--escapes-seq (cdr (cdr form)) var))))
   ((or (memq (car form) nl-check--sequence-heads)
        (memq (car form) '(when unless while if and or not
                           prog1 unwind-protect setq progn)))
    (nl-check--escapes-seq (cdr form) var))
   ;; Unknown call: ownership moves through an argument handed over
   ;; directly.  An argument that merely CONTAINS var does not hand it
   ;; over -- `(eq (nl-resource-type r) 'test-fd)' observes r and keeps
   ;; it -- so recurse into the arguments instead of treating the whole
   ;; subtree as an escape.  Treating containment as escape reported
   ;; every resource a test reads through an accessor.
   (t
    (let ((args (cdr form))
          (found nil))
      (while (and (consp args) (not found))
        (setq found (or (eq (car args) var)
                        (nl-check--escapes-p (car args) var)))
        (setq args (cdr args)))
      found))))

(defun nl-check--escapes-seq (forms var)
  "Return non-nil when VAR escapes in any element of FORMS."
  (let ((found nil))
    (while (and (consp forms) (not found))
      (setq found (nl-check--escapes-p (car forms) var))
      (setq forms (cdr forms)))
    found))

(defun nl-check--consumes (form var)
  "Return the worst-case number of times VAR is consumed in FORM.
Branches take the maximum of their arms; sequences take the sum.  A
`while' body that consumes at all is scored 2, since a loop can run
its body more than once."
  (cond
   ((not (consp form)) 0)
   ((nl--walk-quoted-p form) 0)
   ((nl--walk-backquote-p form)
    (nl-check--consumes-seq (nl--walk-live-parts form) var))
   ((and (memq (car form) '(nl-drop nl-forget))
         (eq (car (cdr form)) var))
    1)
   ((eq (car form) 'if)
    (+ (nl-check--consumes (nth 1 form) var)
       (max (nl-check--consumes (nth 2 form) var)
            (nl-check--consumes-seq (nthcdr 3 form) var))))
   ((eq (car form) 'cond)
    (let ((worst 0))
      (dolist (clause (nl--walk-proper-list (cdr form)))
        (when (consp clause)
          (setq worst (max worst (nl-check--consumes-seq clause var)))))
      worst))
   ((eq (car form) 'while)
    (let ((once (nl-check--consumes-seq (cdr form) var)))
      (if (> once 0) (max 2 once) 0)))
   (t (nl-check--consumes-seq form var))))

(defun nl-check--consumes-seq (forms var)
  "Return the summed consumption count over the list FORMS."
  (let ((total 0))
    (while (consp forms)
      (setq total (+ total (nl-check--consumes (car forms) var)))
      (setq forms (cdr forms)))
    total))

(defun nl-check--fresh-resource-p (init)
  "Return non-nil when INIT constructs a fresh resource."
  (and (consp init) (eq (car init) 'nl-resource)))

(defun nl-check--moved-out-p (body var)
  "Return non-nil when BODY's value is VAR itself, i.e. ownership moves."
  (let ((last nil))
    (while (consp body)
      (setq last (car body))
      (setq body (cdr body)))
    (eq last var)))

(defun nl-check--resource-scan (form findings)
  "Collect resource findings in FORM onto FINDINGS and return it."
  (cond
   ((not (consp form)) findings)
   ((nl--walk-quoted-p form) findings)
   ((nl--walk-backquote-p form)
    (dolist (part (nl--walk-live-parts form) findings)
      (setq findings (nl-check--resource-scan part findings))))
   (t
    (when (memq (car form) '(let let*))
      (let ((body (cdr (cdr form))))
        (dolist (binding (nl--walk-proper-list (car (cdr form))))
          (when (and (consp binding)
                     (symbolp (car binding))
                     (nl-check--fresh-resource-p (car (cdr binding))))
            (let ((var (car binding)))
              (cond
               ((nl-check--escapes-seq body var)
                (setq findings
                      (cons (list :kind 'resource-untracked
                                  :subject var :form form)
                            findings)))
               ((nl-check--moved-out-p body var) findings)
               (t
                (let ((n (nl-check--consumes-seq body var)))
                  (cond
                   ((= n 0)
                    (setq findings
                          (cons (list :kind 'resource-leak
                                      :subject var :form form :count 0)
                                findings)))
                   ((> n 1)
                    (setq findings
                          (cons (list :kind 'resource-double
                                      :subject var :form form :count n)
                                findings))))))))))))
    (let ((tail (cdr form)))
      (while (consp tail)
        (setq findings (nl-check--resource-scan (car tail) findings))
        (setq tail (cdr tail))))
    findings)))

;;;; Unsafe inventory (Doc 170 sections 4.3 and 10) --------------------

(defun nl-check--unsafe-scan (form inside-unsafe findings)
  "Collect unsafe-primitive calls in FORM outside `nl-unsafe' blocks."
  (cond
   ((not (consp form)) findings)
   ((nl--walk-quoted-p form) findings)
   ((eq (car form) 'nl-unsafe)
    (let ((tail (cdr form)))
      (while (consp tail)
        (setq findings (nl-check--unsafe-scan (car tail) t findings))
        (setq tail (cdr tail))))
    findings)
   (t
    (when (and (not inside-unsafe)
               (symbolp (car form))
               (memq (car form) nl-safe-unsafe-primitives))
      (setq findings
            (cons (list :kind 'unsafe-call :subject (car form) :form form)
                  findings)))
    ;; The head is walked too when it is itself a form.  Walking only the
    ;; cdr reads `(let ((x (alloc-bytes 1 1))) x)' as clean: the binding
    ;; list is (BINDING ...), BINDING is a cons, and the first one lives
    ;; in car position where nothing looked.  Measured on this tree the
    ;; day it was found -- 369 reported, 428 present, and nearly every one
    ;; of the 59 was an allocation in the first `let*' binding, the
    ;; house idiom of the standalone build.
    (when (consp (car form))
      (setq findings
            (nl-check--unsafe-scan (car form) inside-unsafe findings)))
    (let ((tail (cdr form)))
      (while (consp tail)
        (setq findings
              (nl-check--unsafe-scan (car tail) inside-unsafe findings))
        (setq tail (cdr tail))))
    findings)))

(defun nl-check--unsafe-scan-quoted (form findings)
  "Collect unsafe-primitive calls that `nl-check--unsafe-scan\=' skips.
Quoted forms are not code at their own site, which is why the scan
above steps over them: an opcode table like (ptr-read-u16 . 39) or a
name list is data, and counting it would be a lie in the other
direction.

But this tree writes its runtime as quoted generator bodies --
`(defconst nelisp-cc-...--source \='(seq (defun ...) ...))\=' -- so the
unsafe kernel itself lives inside quotes, and the gated count sees
almost none of it.  Reporting that as a separate number is the honest
position: the exclusion is a real one, and a reader who is told 369
without being told what it excludes will read it as the surface."
  (cond
   ((not (consp form)) findings)
   ((memq (car form) '(quote function))
    (nl-check--unsafe-collect (cdr form) findings))
   (t
    (let ((tail form))
      (while (consp tail)
        (setq findings (nl-check--unsafe-scan-quoted (car tail) findings))
        (setq tail (cdr tail))))
    findings)))

(defun nl-check--unsafe-collect (form findings)
  "Collect every unsafe-primitive head in FORM, quoted or not."
  (cond
   ((not (consp form)) findings)
   (t
    (when (and (symbolp (car form))
               (memq (car form) nl-safe-unsafe-primitives))
      (setq findings
            (cons (list :kind 'unsafe-call-quoted :subject (car form)
                        :form form)
                  findings)))
    (when (consp (car form))
      (setq findings (nl-check--unsafe-collect (car form) findings)))
    (let ((tail (cdr form)))
      (while (consp tail)
        (setq findings (nl-check--unsafe-collect (car tail) findings))
        (setq tail (cdr tail))))
    findings)))

;;;; Entry points ------------------------------------------------------

(defun nl-check-form (form)
  "Return the list of findings for FORM, in source order."
  (let ((findings nil))
    (setq findings (nl-check--must-use-scan form nil findings))
    (setq findings (nl-check--resource-scan form findings))
    (setq findings (nl-check--unsafe-scan form nil findings))
    (nreverse findings)))

(defun nl-check-forms (forms)
  "Return the concatenated findings for the list FORMS."
  (let ((all nil))
    (dolist (form forms)
      (dolist (finding (nl-check-form form))
        (setq all (cons finding all))))
    (nreverse all)))

(defun nl-check-expand-forms (forms)
  "Return FORMS macroexpanded, so the checks see what will run.

Reading is not enough.  A macro that expands into a resource `let'
hides the resource from a reader-level check completely -- the caller
looks like an ordinary call -- so a violation that exists only after
expansion goes by unreported.  This is the position Rust checks borrows
at: on MIR, after `for' and `?' and closures have been lowered, rather
than on the surface syntax where a violation can hide behind sugar.

A `defmacro' in FORMS is added to a LOCAL expansion environment rather
than evaluated, so a file's own macros are visible to the forms after
them without this having any effect on the running Emacs.  Macros that
are neither local nor already loaded stay unexpanded; `macroexpand-all'
leaves an unknown head alone, so the result is a check that sees less
rather than one that is wrong.

A form that cannot be expanded is passed through unchanged: refusing to
check the rest of a file because one form confused the expander would
trade a complete answer for no answer."
  (let ((environment nil)
        (out nil))
    (dolist (form forms)
      (when (and (consp form)
                 (eq (car form) 'defmacro)
                 (symbolp (car (cdr form)))
                 (consp (cdr (cdr form))))
        (setq environment
              (cons (cons (car (cdr form))
                          (cons 'lambda (cdr (cdr form))))
                    environment)))
      (setq out
            (cons (condition-case nil
                      (macroexpand-all form environment)
                    (error form))
                  out)))
    (nreverse out)))

(defun nl-check-expanded-forms (forms)
  "Return the findings for FORMS after `nl-check-expand-forms'."
  (nl-check-forms (nl-check-expand-forms forms)))

(defun nl-check-file (path)
  "Read PATH and return the findings for every top-level form in it.
Reading only; nothing from PATH is evaluated."
  (let ((forms nil))
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (let ((done nil))
        (while (not done)
          (let ((form (condition-case nil
                          (read (current-buffer))
                        (end-of-file 'nl-check--eof))))
            (if (eq form 'nl-check--eof)
                (setq done t)
              (setq forms (cons form forms)))))))
    (nl-check-forms (nreverse forms))))

(defun nl-check-file-quoted-unsafe (path)
  "Return PATH\='s `unsafe-call-quoted\=' findings.
The calls `nl-check-file\=' steps over because they sit inside a quoted
form.  Reported, never gated: see `nl-check--unsafe-scan-quoted\='."
  (let ((findings nil))
    (dolist (form (nl-check-file-forms path))
      (setq findings (nl-check--unsafe-scan-quoted form findings)))
    (nreverse findings)))

(defun nl-check-file-forms (path)
  "Read PATH and return its top-level forms.  Nothing is evaluated."
  (let ((forms nil))
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (let ((done nil))
        (while (not done)
          (let ((form (condition-case nil
                          (read (current-buffer))
                        (end-of-file 'nl-check--eof))))
            (if (eq form 'nl-check--eof)
                (setq done t)
              (setq forms (cons form forms)))))))
    (nreverse forms)))

(defun nl-check-file-expanded (path)
  "Return the findings for PATH after macro expansion.
The same depth the artifact path checks at.  Two checkpoints that
disagree about how deep to look are worse than one, because the shallow
one reports success on what the deep one would reject."
  (nl-check-expanded-forms (nl-check-file-forms path)))

;;;; Reporting ---------------------------------------------------------

(defun nl-check-findings-of-kind (findings kind)
  "Return the elements of FINDINGS whose `:kind' is KIND."
  (let ((out nil))
    (dolist (finding findings)
      (when (eq (plist-get finding :kind) kind)
        (setq out (cons finding out))))
    (nreverse out)))

(defun nl-check--describe (finding)
  "Return a one-line description of FINDING."
  (let ((kind (plist-get finding :kind))
        (subject (plist-get finding :subject)))
    (cond
     ((eq kind 'must-use-discarded)
      (format "must-use-discarded: value of `%s' is thrown away" subject))
     ((eq kind 'resource-leak)
      (format "resource-leak: `%s' is never dropped" subject))
     ((eq kind 'resource-double)
      (format "resource-double: `%s' consumed %s times on one path"
              subject (plist-get finding :count)))
     ((eq kind 'resource-untracked)
      (format "resource-untracked: `%s' escaped the checker; not verified"
              subject))
     ((eq kind 'unsafe-call)
      (format "unsafe-call: `%s' called outside an nl-unsafe block" subject))
     (t (format "%s: %s" kind subject)))))

(defun nl-check-report (findings)
  "Return a human-readable report string for FINDINGS."
  (if (null findings)
      "nl-check: no findings\n"
    (let ((lines nil))
      (dolist (finding findings)
        (setq lines (cons (concat "  " (nl-check--describe finding) "\n")
                          lines)))
      (apply #'concat
             (format "nl-check: %d finding(s)\n" (length findings))
             (nreverse lines)))))

(provide 'nl-check)

;;; nl-check.el ends here
