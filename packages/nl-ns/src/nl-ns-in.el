;;; nl-ns-in.el --- A namespace you can actually write in -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The writing half of nl-ns.  A namespace has a closed, declared member
;; set, so expanding one block never depends on another block's expansion.
;;
;;   (nl-ns-define text :members (limit wrap chunk))
;;   (nl-ns-in text
;;     (defvar limit 80)
;;     (defun wrap (s) (chunk s limit))
;;     (defun chunk (s n) (substring s 0 n)))
;;
;; defines text-limit, text-wrap, and text-chunk.
;;
;; Doc 189 (docs/design/189-nl-ns-enforced-namespaces.org) §4 Phase 1:
;; `nl-ns-define' can optionally refuse a declaration that would claim
;; a qualified name a *different* namespace already owns, instead of
;; silently re-binding it -- opt in with `nl-ns-enforce-collisions' or
;; a single declaration's own `:enforce' property.  Default is off,
;; matching every `nl-ns-define' call already in this tree.

;;; Code:

(require 'nl-prelude)

;;;; Registry -----------------------------------------------------------

(defvar nl-ns--namespaces (make-hash-table :test 'eq)
  "Map of namespace symbol -> plist (:prefix STRING :members LIST).")

(defvar nl-ns-enforce-collisions nil
  "When non-nil, `nl-ns-define' refuses a declaration that would claim a
qualified name a *different* namespace already owns, instead of
silently replacing it.  Default nil: every existing `nl-ns-define'
call in this tree keeps today's silent-last-wins behaviour until it,
or this variable, opts in.  A single declaration overrides this
default with its own `:enforce' property; see `nl-ns-define'.

Doc 189 (docs/design/189-nl-ns-enforced-namespaces.org) §4 Phase 1.")

(defvar nl-ns--collision-owners (make-hash-table :test 'eq)
  "Map of qualified symbol -> owning namespace symbol.
Populated, and consulted, only while enforcement is active for a given
`nl-ns-define' declaration (`nl-ns-enforce-collisions' or that
declaration's own `:enforce').  A declaration with enforcement off
never reads or writes this table -- the same \"no table, no guess\"
discipline Doc 189 Phase 0's reader hook already follows.  Cleared by
`nl-ns-clear-namespaces' alongside the namespace registry so the two
never drift out of sync.")

(define-error 'nl-ns-collision-error
  "nl-ns-define: a qualified name is already owned by another namespace"
  'nl-error)

(defun nl-ns-clear-namespaces ()
  "Forget every `nl-ns-define' declaration, including collision ownership."
  (clrhash nl-ns--namespaces)
  (clrhash nl-ns--collision-owners)
  nil)

(defun nl-ns--entry (name)
  "Return the registry entry for NAME, signalling when it is absent."
  (or (gethash name nl-ns--namespaces)
      (error "nl-ns: namespace `%s' is not defined; call nl-ns-define first"
             name)))

(defun nl-ns--release-ownership (name)
  "Remove every `nl-ns--collision-owners' entry NAME currently owns."
  (maphash (lambda (qualified owner)
             (when (eq owner name)
               (remhash qualified nl-ns--collision-owners)))
           nl-ns--collision-owners))

(defun nl-ns--register (name prefix members enforce)
  "Install NAME's namespace declaration, refusing a claimed collision.
PREFIX and MEMBERS are NAME's already-validated declared properties.
ENFORCE is the declaration's own `:enforce' value, or the keyword
`:nl-ns-unspecified' when it passed none -- in which case
`nl-ns-enforce-collisions' decides whether this declaration checks and
records collision ownership at all.

When enforcement is active, every member is checked against
`nl-ns--collision-owners' *before* anything is mutated: if a qualified
name is already owned by a namespace other than NAME, this whole
declaration signals `nl-ns-collision-error' -- naming NAME, the
existing owner, the qualified symbol and the member -- and changes
nothing, not the ownership ledger and not the namespace registry.  A
refused declaration leaves no partial trace.  Once every member
passes, NAME's previous ownership claims are released and its new
members claimed before the namespace registry itself is updated, so
the registry and the ledger never disagree about what NAME currently
owns; this is also what lets re-declaring the same namespace (with the
same members, a smaller set, or a different set entirely) succeed
instead of colliding with itself.

Enforcement inactive is exactly today's behaviour: the ledger is never
consulted or touched, and the declaration always replaces the registry
entry, silently -- the same as before this table existed."
  (let ((active (if (eq enforce :nl-ns-unspecified)
                     nl-ns-enforce-collisions
                   enforce)))
    (when active
      (dolist (member members)
        (let* ((qualified (intern (concat prefix (symbol-name member))))
               (owner (gethash qualified nl-ns--collision-owners)))
          (when (and owner (not (eq owner name)))
            (signal 'nl-ns-collision-error
                    (list (format "nl-ns-define: namespace `%s' cannot declare member `%s' (`%s'); already owned by namespace `%s'"
                                  name member qualified owner)
                          name owner qualified member)))))
      (nl-ns--release-ownership name)
      (dolist (member members)
        (puthash (intern (concat prefix (symbol-name member))) name
                 nl-ns--collision-owners)))
    (puthash name (list :prefix prefix :members members) nl-ns--namespaces))
  name)

(defmacro nl-ns-define (name &rest properties)
  "Declare NAME as a namespace with a closed `:members' set.
PROPERTIES accepts `:prefix STRING', defaulting to \"NAME-\", and
`:members (SYMBOL...)'.  Re-defining a namespace replaces its
declaration.

`:enforce BOOL' opts this declaration in or out of collision refusal,
overriding `nl-ns-enforce-collisions' for this declaration only; omit
it to follow that variable.  See `nl-ns--register' for exactly what
\"collision\" means and what a refusal does and does not change."
  (unless (and name (symbolp name))
    (error "nl-ns-define: NAME must be a non-nil symbol, got %S" name))
  (let ((prefix (plist-get properties :prefix))
        (members (plist-get properties :members))
        (enforce-cell (plist-member properties :enforce))
        (unknown nil)
        (tail properties))
    (while tail
      (unless (memq (car tail) '(:prefix :members :enforce))
        (setq unknown (cons (car tail) unknown)))
      (setq tail (cdr (cdr tail))))
    (when unknown
      (error "nl-ns-define: unknown properties %S" (nreverse unknown)))
    (when (and prefix (not (stringp prefix)))
      (error "nl-ns-define: :prefix must be a string, got %S" prefix))
    (unless (listp members)
      (error "nl-ns-define: :members must be a list, got %S" members))
    (dolist (member members)
      (unless (symbolp member)
        (error "nl-ns-define: :members must contain symbols, got %S" member)))
    (when (and enforce-cell (not (memq (car (cdr enforce-cell)) '(t nil))))
      (error "nl-ns-define: :enforce must be t or nil, got %S"
             (car (cdr enforce-cell))))
    (let ((enforce (if enforce-cell (car (cdr enforce-cell)) :nl-ns-unspecified)))
      `(eval-and-compile
         (nl-ns--register ',name
                           ,(or prefix (concat (symbol-name name) "-"))
                           ',members
                           ,enforce)
         ',name))))

(defun nl-ns-prefix (name)
  "Return the prefix string of namespace NAME."
  (plist-get (nl-ns--entry name) :prefix))

(defun nl-ns-member-list (name)
  "Return the declared member symbols of namespace NAME, in order."
  (plist-get (nl-ns--entry name) :members))

(defun nl-ns-qualify (name symbol)
  "Return SYMBOL prefixed with namespace NAME's prefix."
  (intern (concat (nl-ns-prefix name) (symbol-name symbol))))

;;;; Rewriting -----------------------------------------------------------

(define-error 'nl-ns-in-non-member-error
  "nl-ns-in definition names an undeclared namespace member" 'nl-error)
(define-error 'nl-ns-in-member-binding-error
  "nl-ns-in binding shadows a namespace member" 'nl-error)

(defconst nl-ns-in-definition-heads
  '(defun defmacro defsubst defvar defconst defcustom cl-defun cl-defmacro)
  "Heads whose second element `nl-ns-in' treats as a definition to rename.")

(defun nl-ns-in--arglist-vars (arglist)
  "Return the variables ARGLIST binds, skipping lambda-list markers."
  (let ((out nil))
    (dolist (arg (if (listp arglist) arglist nil))
      (cond
       ((and (symbolp arg) arg
             (not (eq (aref (symbol-name arg) 0) ?&)))
        (setq out (cons arg out)))
       ((consp arg)
        (let ((var (car arg)) (supplied (car (cdr (cdr arg)))))
          (when (and (consp var) (symbolp (car (cdr var))))
            (setq var (car (cdr var))))
          (when (symbolp var) (setq out (cons var out)))
          (when (symbolp supplied) (setq out (cons supplied out)))))))
    out))

(defun nl-ns-in--binding-vars (bindings)
  "Return the variables a `let'-style BINDINGS list binds."
  (let ((out nil))
    (dolist (binding bindings)
      (cond ((symbolp binding) (setq out (cons binding out)))
            ((and (consp binding) (symbolp (car binding)))
             (setq out (cons (car binding) out)))))
    out))

(defun nl-ns-in--reject-bindings (vars map)
  "Signal if VARS binds a declared member in MAP."
  (dolist (var vars)
    (when (gethash var map)
      (signal 'nl-ns-in-member-binding-error
              (list (format "nl-ns-in: `%s' is a namespace member; rename the local"
                            var)
                    var)))))

(defun nl-ns-in--rewrite-seq (forms map)
  "Rewrite each element of FORMS, preserving an improper tail."
  (cond ((null forms) nil)
        ((not (consp forms)) (nl-ns-in--rewrite forms map))
        (t (cons (nl-ns-in--rewrite (car forms) map)
                 (nl-ns-in--rewrite-seq (cdr forms) map)))))

(defun nl-ns-in--rewrite-arglist (arglist map)
  "Rewrite default forms in ARGLIST while preserving binding syntax."
  (let ((out nil))
    (dolist (arg (if (listp arglist) arglist nil))
      (setq out
            (cons (if (consp arg)
                      (cons (car arg)
                            (cons (nl-ns-in--rewrite (car (cdr arg)) map)
                                  (cdr (cdr arg))))
                    arg)
                  out)))
    (nreverse out)))

(defun nl-ns-in--rewrite-backquote (template map depth)
  "Rewrite evaluated unquotes in TEMPLATE at quasiquote DEPTH.
Literal template positions remain data.  Nested quasiquotes are retained as
data until their own evaluation, so only unquotes at depth one are rewritten."
  (cond
   ((not (consp template)) template)
   ((memq (car template) '(\` backquote))
    (list (car template) (nl-ns-in--rewrite-backquote
                           (car (cdr template)) map (1+ depth))))
   ((memq (car template) '(\, \,@ comma comma-at))
    (if (= depth 1)
        (list (car template) (nl-ns-in--rewrite (car (cdr template)) map))
      (list (car template) (nl-ns-in--rewrite-backquote
                            (car (cdr template)) map (1- depth)))))
   (t (cons (nl-ns-in--rewrite-backquote (car template) map depth)
            (nl-ns-in--rewrite-backquote (cdr template) map depth)))))

(defun nl-ns-in--rewrite (form map)
  "Rewrite FORM using MAP, rejecting definitions and bindings outside its rules."
  (cond
   ((symbolp form) (or (gethash form map) form))
   ((not (consp form)) form)
   ((eq (car form) 'quote) form)
   ((memq (car form) '(\` backquote))
    (list (car form) (nl-ns-in--rewrite-backquote (car (cdr form)) map 1)))
   ((eq (car form) 'function)
    (let ((target (car (cdr form))))
      (list 'function (if (symbolp target)
                          (or (gethash target map) target)
                        (nl-ns-in--rewrite target map)))))
   ((eq (car form) 'lambda)
    (let ((args (car (cdr form))))
      (nl-ns-in--reject-bindings (nl-ns-in--arglist-vars args) map)
      (cons 'lambda (cons (nl-ns-in--rewrite-arglist args map)
                          (nl-ns-in--rewrite-seq (cdr (cdr form)) map)))))
   ((memq (car form) '(let let*)) (nl-ns-in--rewrite-let form map))
   ((memq (car form) nl-ns-in-definition-heads)
    (nl-ns-in--rewrite-definition form map))
   ((memq (car form) '(dolist dotimes)) (nl-ns-in--rewrite-loop form map))
   ((eq (car form) 'condition-case) (nl-ns-in--rewrite-condition-case form map))
   (t (nl-ns-in--rewrite-seq form map))))

(defun nl-ns-in--rewrite-let (form map)
  "Rewrite a `let' or `let*' FORM after rejecting member bindings."
  (let ((bindings (car (cdr form))))
    (nl-ns-in--reject-bindings (nl-ns-in--binding-vars bindings) map)
    (cons (car form)
          (cons (nl-ns-in--rewrite-seq bindings map)
                (nl-ns-in--rewrite-seq (cdr (cdr form)) map)))))

(defun nl-ns-in--rewrite-loop (form map)
  "Rewrite dolist or dotimes FORM after rejecting its member binding."
  (let* ((spec (car (cdr form))) (var (and (consp spec) (car spec))))
    (when (symbolp var) (nl-ns-in--reject-bindings (list var) map))
    (cons (car form) (cons (nl-ns-in--rewrite-seq spec map)
                           (nl-ns-in--rewrite-seq (cdr (cdr form)) map)))))

(defun nl-ns-in--rewrite-condition-case (form map)
  "Rewrite condition-case FORM after rejecting its handler variable binding."
  (let ((var (car (cdr form))))
    (when (symbolp var) (nl-ns-in--reject-bindings (list var) map))
    (cons 'condition-case (nl-ns-in--rewrite-seq (cdr form) map))))

(defun nl-ns-in--rewrite-definition (form map)
  "Rewrite a definition FORM, requiring its name to be a declared member."
  (let ((head (car form)) (name (car (cdr form))))
    (unless (gethash name map)
      (signal 'nl-ns-in-non-member-error
              (list (format "nl-ns-in: `%s' is not a declared namespace member"
                            name)
                    name)))
    (if (memq head '(defvar defconst defcustom))
        (cons head (cons (gethash name map)
                         (nl-ns-in--rewrite-seq (cdr (cdr form)) map)))
      (let ((args (car (cdr (cdr form)))))
        (nl-ns-in--reject-bindings (nl-ns-in--arglist-vars args) map)
        (cons head (cons (gethash name map)
                         (cons (nl-ns-in--rewrite-arglist args map)
                               (nl-ns-in--rewrite-seq
                                (cdr (cdr (cdr form))) map))))))))

;;;; Entry points -------------------------------------------------------

(defun nl-ns-expand (name body)
  "Return BODY rewritten inside namespace NAME, without evaluating it.
This is a pure function of NAME's declaration and BODY."
  (let ((map (make-hash-table :test 'eq)))
    (dolist (sym (nl-ns-member-list name))
      (puthash sym (nl-ns-qualify name sym) map))
    (nl-ns-in--rewrite-seq body map)))

(defmacro nl-ns-in (name &rest body)
  "Evaluate BODY with NAME's declared members written unqualified.
Definitions must name declared members.  Binding a declared member is an
error; rename the local.  Quoted data and backquote templates stay literal,
but unquotes are evaluated and rewritten."
  (declare (indent 1))
  (nl-ns--entry name)
  (cons 'progn (nl-ns-expand name body)))

(provide 'nl-ns-in)

;;; nl-ns-in.el ends here
