;;; nelisp-function-info.el --- Representation-neutral function information  -*- lexical-binding: t; -*-

;;; Commentary:

;; A portable language API that describes a function without caring how
;; the running Emacs-compatible runtime represents it.
;;
;;   (nelisp-function-info FUNCTION)
;;     Public.  Returns a fresh, versioned plist:
;;       :schema 1
;;       :kind   interpreted | bytecode | subr | non-function | unresolved
;;       :source-available-p  boolean
;;       :arglist   validated source arglist, or nil
;;       :lexical-p boolean (meaningful only when :source-available-p)
;;       :reason    symbol explaining why no source is available, or nil
;;     The plist deliberately carries no execution-route claim: an
;;     interpreted function may still be served by a native cache.
;;
;;   (nelisp-function--source-parts FUNCTION)
;;     Internal, for trusted compiler callers.  Returns nil, or a plist
;;       :arglist :body :environment :lexical-p :representation
;;     whose lists are the live objects (read-only views; never copy or
;;     rebuild captured lexical values).  :representation is one of
;;     `legacy-closure', `legacy-lambda' or `interpreted-function'.
;;
;; Both entry points share one decoder, `nelisp-function--decode'.
;; Introspection never evaluates a body, expands a macro, loads an
;; autoload, or changes any function cell.  FUNCTION may be a function
;; object or a symbol (resolved through `indirect-function').
;;
;; Accepted source shapes:
;;   (closure ENV ARGLIST . BODY)   legacy closure; ENV must be non-nil
;;   (lambda ARGLIST . BODY)        legacy lambda (dynamic binding)
;;   an object for which `interpreted-function-p' is true.  A runtime
;;   closure (`nelisp--closure-p') is decoded from its canonical backing
;;   form via `nelisp--closure-cons' (:source-kind `backing-form'); its
;;   `aref' view projects lexical cells into fresh pairs and is never
;;   used.  Otherwise guarded `aref' slots 0 (arglist), 1 (body),
;;   2 (lexical env) are canonical (:source-kind `canonical-slots').
;; Real byte-code and subrs are classified first by their authoritative
;; predicates, so they are never mistaken for source.  An arbitrary
;; vector is never accepted on the strength of its length.

;;; Code:

(defconst nelisp-function-info-schema 1
  "Schema version of the plist returned by `nelisp-function-info'.")

;;;; Small shared helpers

(defun nelisp-function--test (predicate object)
  "Call PREDICATE on OBJECT when PREDICATE is defined, else return nil."
  (and (fboundp predicate) (funcall predicate object) t))

(defun nelisp-function--proper-list-length (object)
  "Return the length of OBJECT if it is a proper, acyclic list, else nil.
The empty list yields 0, which is non-nil."
  (let ((n 0) (slow object) (fast object) (result nil) (done nil))
    (while (not done)
      (cond
       ((null fast) (setq result n done t))
       ((not (consp fast)) (setq done t))
       (t
        (setq fast (cdr fast) n (1+ n))
        (cond
         ((null fast) (setq result n done t))
         ((not (consp fast)) (setq done t))
         (t
          (setq fast (cdr fast) n (1+ n) slow (cdr slow))
          (when (eq fast slow) (setq done t)))))))
    result))

(defun nelisp-function--arglist-valid-p (arglist)
  "Return non-nil when ARGLIST is a well-formed lambda list."
  (and (nelisp-function--proper-list-length arglist)
       (let ((rest arglist) (ok t) (state 'required) (dangling nil))
         (while (and ok rest)
           (let ((arg (car rest)))
             (cond
              ((not (symbolp arg)) (setq ok nil))
              ((or (null arg) (eq arg t) (keywordp arg)) (setq ok nil))
              ((eq arg '&optional)
               (if (eq state 'required)
                   (setq state 'optional dangling t)
                 (setq ok nil)))
              ((eq arg '&rest)
               (if (memq state '(required optional))
                   (setq state 'rest-wait dangling t)
                 (setq ok nil)))
              ((eq state 'rest-wait) (setq state 'rest-done dangling nil))
              ((eq state 'rest-done) (setq ok nil))
              (t (setq dangling nil))))
           (setq rest (cdr rest)))
         (and ok (not dangling)))))

(defun nelisp-function--environment-valid-p (environment)
  "Return non-nil when ENVIRONMENT is a proper lexical-environment list.
Elements are `(SYMBOL . VALUE)' conses or bare symbols (`t' marker or
dynamic-binding declarations).  Values are not inspected."
  (and (nelisp-function--proper-list-length environment)
       (let ((rest environment) (ok t))
         (while (and ok rest)
           (let ((entry (car rest)))
             (unless (or (symbolp entry)
                         (and (consp entry) (symbolp (car entry))))
               (setq ok nil)))
           (setq rest (cdr rest)))
         ok)))

;;;; Resolution

(defun nelisp-function--resolve (function)
  "Resolve FUNCTION without loading autoloads.
Return (OBJECT . nil) on success or (nil . REASON) when a symbol
chain is unbound or cyclic."
  (if (not (symbolp function))
      (cons function nil)
    (let ((seen nil) (current function) (status nil) (final nil))
      (while (null status)
        (if (memq current seen)
            (setq status 'cyclic-function-indirection)
          (push current seen)
          (let ((cell (symbol-function current)))
            (cond
             ((null cell) (setq status 'void-function))
             ((symbolp cell) (setq current cell))
             (t (setq final cell status 'resolved))))))
      (if (eq status 'resolved)
          ;; The walk proved the chain acyclic and bound, so the
          ;; authoritative primitive is safe to call.
          (cons (or (and (fboundp 'indirect-function)
                         (condition-case nil
                             (indirect-function function)
                           (error nil)))
                    final)
                nil)
        (cons nil status)))))

;;;; Canonical source decoder

(defun nelisp-function--make-parts (representation arglist body environment
                                                    lexical-p)
  "Validate the pieces and return (PARTS . nil) or (nil . REASON)."
  (cond
   ((not (nelisp-function--arglist-valid-p arglist))
    (cons nil 'malformed-arglist))
   ((not (nelisp-function--proper-list-length body))
    (cons nil 'malformed-body))
   ((not (nelisp-function--environment-valid-p environment))
    (cons nil 'malformed-environment))
   (t
    (cons (list :arglist arglist :body body :environment environment
                :lexical-p (and lexical-p t)
                :representation representation)
          nil))))

(defun nelisp-function--with-source-kind (result source-kind)
  "Tag the parts of RESULT, a (PARTS . REASON) pair, with SOURCE-KIND."
  (if (car result)
      (cons (append (car result) (list :source-kind source-kind)) nil)
    result))

;; Provided by the NeLisp runtime only; callers guard with `fboundp'.
(declare-function nelisp--closure-cons "nelisp-function-info" (function))

(defun nelisp-function--decode-native-closure (function)
  "Decode FUNCTION, a runtime closure, from its backing form.
The runtime `aref' is only a descriptive view: it projects lexical cells
into fresh pairs.  `nelisp--closure-cons' returns the original
\(closure ENV ARGLIST . BODY) or dynamic (lambda ARGLIST . BODY) form,
so ENV keeps its captured-cell identity.  A malformed backing form is
an error, never a fallback to
the projected view."
  (let ((form (condition-case nil
                  (nelisp--closure-cons function)
                (error nil))))
    (if (not (and (consp form) (memq (car form) '(closure lambda))))
        (cons nil 'malformed-shape)
      (let ((n (nelisp-function--proper-list-length form))
            (dynamic (eq (car form) 'lambda)))
        (if (or (null n) (< n (if dynamic 2 3)))
            (cons nil 'malformed-shape)
          (let ((environment (unless dynamic (nth 1 form))))
            ;; An empty environment is legitimate for a runtime closure;
            ;; it simply is not lexical.
            (nelisp-function--make-parts
             'interpreted-function (nth (if dynamic 1 2) form)
             (nthcdr (if dynamic 2 3) form)
             environment (and environment t))))))))

(defun nelisp-function--decode-interpreted (function)
  "Decode an `interpreted-function-p' FUNCTION.
The :source-kind part says how the fields were obtained.  A runtime
closure (`nelisp--closure-p') is read from its canonical backing form,
kind `backing-form'.  Otherwise slots are read through guarded `aref',
kind `canonical-slots', which is canonical on hosts whose slots are
the storage.  The backend is detected by predicate, never by version."
  (if (nelisp-function--test 'nelisp--closure-p function)
      (if (fboundp 'nelisp--closure-cons)
          (nelisp-function--with-source-kind
           (nelisp-function--decode-native-closure function)
           'backing-form)
        (cons nil 'malformed-shape))
    (nelisp-function--with-source-kind
     (condition-case nil
         (let ((n (length function)))
           (if (not (and (integerp n) (>= n 3)))
               (cons nil 'malformed-shape)
             (let ((arglist (aref function 0))
                   (body (aref function 1))
                   (environment (aref function 2)))
               ;; Older minimal runtimes expose projected slots but no
               ;; canonical backing accessor. Never claim those fresh
               ;; views preserve compiler source or captured-cell identity.
               (if (not (and (eq arglist (aref function 0))
                             (eq body (aref function 1))
                             (eq environment (aref function 2))))
                   (cons nil 'canonical-source-unavailable)
                 (nelisp-function--make-parts
                  'interpreted-function arglist body environment
                  (and environment t))))))
       (error (cons nil 'malformed-shape)))
     'canonical-slots)))

(defun nelisp-function--decode-legacy (form)
  "Decode FORM, a cons headed by `closure' or `lambda'."
  (let ((n (nelisp-function--proper-list-length form)))
    (cond
     ((null n) (cons nil 'malformed-shape))
     ((eq (car form) 'closure)
      (cond
       ((< n 3) (cons nil 'malformed-shape))
       ((null (nth 1 form)) (cons nil 'malformed-environment))
       (t (nelisp-function--make-parts
           'legacy-closure (nth 2 form) (nthcdr 3 form) (nth 1 form) t))))
     (t
      (if (< n 2)
          (cons nil 'malformed-shape)
        (nelisp-function--make-parts
         'legacy-lambda (nth 1 form) (nthcdr 2 form) nil nil))))))

(defun nelisp-function--decode (function)
  "Classify FUNCTION; the single place where field decisions are made.
Return a plist (:kind KIND :reason REASON :parts PARTS)."
  (let ((resolved (nelisp-function--resolve function)))
    (if (null (car resolved))
        (list :kind 'unresolved :reason (cdr resolved) :parts nil)
      (let ((object (car resolved)))
        (cond
         ((nelisp-function--test 'subrp object)
          (list :kind 'subr :reason 'subr :parts nil))
         ((nelisp-function--test 'byte-code-function-p object)
          (list :kind 'bytecode :reason 'bytecode :parts nil))
         ((nelisp-function--test 'interpreted-function-p object)
          (let ((decoded (nelisp-function--decode-interpreted object)))
            (list :kind 'interpreted :reason (cdr decoded)
                  :parts (car decoded))))
         ((and (consp object) (memq (car object) '(closure lambda)))
          (let ((decoded (nelisp-function--decode-legacy object)))
            (if (car decoded)
                (list :kind 'interpreted :reason nil :parts (car decoded))
              (list :kind 'non-function :reason (cdr decoded) :parts nil))))
         ((and (consp object) (eq (car object) 'autoload))
          (list :kind 'unresolved :reason 'autoload :parts nil))
         ((and (consp object) (eq (car object) 'macro))
          (list :kind 'non-function :reason 'macro :parts nil))
         (t
          (list :kind 'non-function :reason 'not-a-function :parts nil)))))))

;;;; Entry points

;;;###autoload
;;;###autoload
(defun nelisp-function-info (function)
  "Return a versioned plist describing FUNCTION (an object or a symbol).
Keys: :schema :kind :source-available-p :arglist :lexical-p :reason.
See the Commentary of nelisp-function-info.el.  No execution route is
reported.  The :arglist list is a read-only view of the source."
  (let* ((decoded (nelisp-function--decode function))
         (parts (plist-get decoded :parts)))
    (list :schema nelisp-function-info-schema
          :kind (plist-get decoded :kind)
          :source-available-p (and parts t)
          :arglist (plist-get parts :arglist)
          :lexical-p (and parts (plist-get parts :lexical-p) t)
          :reason (plist-get decoded :reason))))

(defun nelisp-function--source-parts (function)
  "Return validated source parts of FUNCTION, or nil.
Internal, for trusted compiler callers.  The returned plist holds
:arglist :body :environment :lexical-p :representation, plus
:source-kind (`backing-form' or `canonical-slots') for interpreted
objects: the environment of a `backing-form' source is the original
captured-cell list, not a projection.  Lists and the
environment are the live objects; callers must treat them as read-only
and must not rebuild captured values."
  (plist-get (nelisp-function--decode function) :parts))

(provide 'nelisp-function-info)

;;; nelisp-function-info.el ends here
