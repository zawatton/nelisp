;;; emacs-buffer.el --- Emacs C buffer.c port on top of nelisp-emacs-compat  -*- lexical-binding: t; -*-

;; Phase 1 module 1/6 per nelisp-emacs Doc 01 (DRAFT v0).
;; Layer: nelisp-emacs (Layer 2 extension on top of NeLisp).
;; Namespace: `emacs-buffer-' so loading inside a host Emacs does NOT
;; shadow `make-local-variable', `buffer-local-variables', etc.
;;
;; Foundation contract:
;;   - `nelisp-emacs-compat' (T39 SHIPPED, 31 API) provides the buffer
;;     struct (`nelisp-ec-buffer'), point/narrow/marker/search/edit
;;     primitives.  We never `setf' its struct slots from this module
;;     except to *read* them (= treat as opaque).  All buffer mutation
;;     goes through `nelisp-ec-*' API.
;;   - This module does NOT extend the `nelisp-ec-buffer' cl-defstruct
;;     (we cannot, without forking T39).  Instead we maintain a parallel
;;     side-table (`emacs-buffer--state', a hash keyed by buffer object)
;;     that holds the extended per-buffer state (buffer-local bindings,
;;     undo list, modified tick, text-property intervals, indirect
;;     base-buffer reference, etc.).
;;
;; API surface (~30 public APIs across 5 categories):
;;
;;   A. buffer-local variables  (10 APIs)
;;      make-local-variable / make-variable-buffer-local
;;      buffer-local-variables / buffer-local-value
;;      local-variable-p / local-variable-if-set-p
;;      default-value / default-boundp / setq-default (macro)
;;      kill-local-variable / kill-all-local-variables
;;
;;   B. text-property MVP  (5 APIs — Doc 41 Phase 9c will extend
;;      with face/display/overlay/keymap)
;;      put-text-property / get-text-property
;;      add-text-properties / remove-text-properties
;;      text-property-at
;;
;;   C. undo system  (5 APIs)
;;      buffer-undo-list (var)
;;      undo / undo-boundary
;;      buffer-disable-undo / buffer-enable-undo
;;
;;   D. modification tracking  (5 APIs)
;;      buffer-modified-p / set-buffer-modified-p
;;      restore-buffer-modified-p
;;      modify-without-undo (macro)
;;      buffer-chars-modified-tick
;;
;;   E. additional buffer ops  (5 APIs)
;;      clone-indirect-buffer / buffer-base-buffer
;;      buffer-list / buffers-by-mode (helper)
;;      generate-new-buffer-name
;;
;; Non-goals (deferred per task spec):
;;   - text-property face/display/overlay/keymap (Doc 41 Phase 9c)
;;   - syntax table (separate module)
;;   - multibyte buffer encoding (already nelisp-coding)
;;   - file integration (already nelisp-emacs-compat-fileio)

;;; Code:

(require 'cl-lib)
(require 'nelisp-emacs-compat)

;;; Errors

(define-error 'emacs-buffer-error "emacs-buffer error")
(define-error 'emacs-buffer-not-local
  "Variable is not buffer-local in this buffer" 'emacs-buffer-error)
(define-error 'emacs-buffer-undo-disabled
  "Undo is disabled for this buffer" 'emacs-buffer-error)

;;; Side-table: per-buffer extended state

(cl-defstruct (emacs-buffer--ext
               (:constructor emacs-buffer--ext-make)
               (:copier nil)
               (:predicate emacs-buffer--ext-p))
  "Extended per-buffer state held outside the `nelisp-ec-buffer' struct.

Slots:
- LOCALS         : alist (SYMBOL . VALUE) of buffer-local bindings.
                   Membership = bound buffer-locally in this buffer.
- AUTO-LOCAL-P   : t if `kill-all-local-variables' should preserve
                   permanent-local marks (currently nil placeholder).
- UNDO-LIST      : Emacs-style list, or t = undo-disabled.  Each
                   non-nil entry is a record like (TEXT . POSITION),
                   (BEG . END) for deletion, or nil = boundary.
- TEXT-PROPS     : sorted list of intervals (START END . PLIST) where
                   START / END are 1-based, half-open [START, END).
                   MVP only; Doc 41 will add full interval-tree.
- MODIFIED-TICK  : monotonically increasing counter incremented on
                   every text-property mutation (= read by
                   `buffer-chars-modified-tick').  Phase 3.C.1 includes
                   `face' / `display' / `invisible' propagation.
- TEXT-TICK      : Phase 3.B.7 monotonically increasing counter
                   incremented on every TEXT CONTENT mutation
                   (= insert / delete via `nelisp-ec-insert' /
                   `nelisp-ec-delete-region').  Used as a buffer-string
                   cache key — text-property changes do NOT bump it.
- BASE-BUFFER    : non-nil = this buffer is an indirect clone of the
                   given `nelisp-ec-buffer'.
- OVERLAYS       : Phase 1 §4.2 — per-buffer overlay list sorted by
                   ascending START (`emacs-buffer--overlay' records).
                   See F. overlay section below."
  (locals        nil)
  (auto-local-p  nil)
  (undo-list     nil)
  (text-props    nil)
  (modified-tick 0)
  (text-tick     0)
  (base-buffer   nil)
  (overlays      nil))

(defvar emacs-buffer--state (make-hash-table :test 'eq :weakness nil)
  "Hash table buffer-object -> `emacs-buffer--ext'.
Populated lazily by `emacs-buffer--ensure-ext'.  We keep strong refs
(non-weak) so a buffer's extended state outlives transient lookups —
explicit `kill-buffer' callers should remove the entry via
`emacs-buffer--forget'.")

(defvar emacs-buffer--variable-buffer-local nil
  "List of symbols globally declared `make-variable-buffer-local'.

When SYM is on this list, every buffer auto-creates a local binding
the first time it is `setq'-ed (Emacs semantics).  Our MVP defers the
auto-create-on-setq trick (= we do not have generalized `setq'
intercept), but `local-variable-if-set-p' consults this list so the
predicate returns the correct answer.")

(defvar emacs-buffer--default-values (make-hash-table :test 'eq)
  "Hash SYM -> default-value cell (a cons (BOUNDP . VALUE)).
We cannot touch the host Emacs `default-value' machinery, so we shadow
it for symbols that pass through this module's API.")

(defun emacs-buffer--ensure-ext (buf)
  "Return the `emacs-buffer--ext' record for BUF, creating it lazily."
  (unless (nelisp-ec-buffer-p buf)
    (signal 'wrong-type-argument (list 'nelisp-ec-buffer-p buf)))
  (or (gethash buf emacs-buffer--state)
      (puthash buf (emacs-buffer--ext-make) emacs-buffer--state)))

(defun emacs-buffer--current ()
  "Return the current buffer or signal `nelisp-ec-no-current-buffer'."
  (or (nelisp-ec-current-buffer)
      (signal 'nelisp-ec-no-current-buffer nil)))

;;;###autoload
(defun emacs-buffer-current ()
  "Return the current buffer or signal `nelisp-ec-no-current-buffer'."
  (emacs-buffer--current))

(defun emacs-buffer--forget (buf)
  "Drop the extended state for BUF.  Idempotent.
Call from a kill-buffer hook (host integration job)."
  (remhash buf emacs-buffer--state))

;;; A. buffer-local variables  (10 APIs)

;;;###autoload
(defun emacs-buffer-make-local-variable (sym &optional buf)
  "Give SYM a buffer-local binding in BUF (default = current buffer).
The initial value of the buffer-local cell is the current default
value of SYM.  Returns SYM.

If SYM already has a local binding in BUF the call is a no-op
(matches Emacs semantics)."
  (unless (symbolp sym)
    (signal 'wrong-type-argument (list 'symbolp sym)))
  (let* ((b (or buf (emacs-buffer--current)))
         (ext (emacs-buffer--ensure-ext b)))
    (unless (assq sym (emacs-buffer--ext-locals ext))
      (push (cons sym (emacs-buffer-default-value sym))
            (emacs-buffer--ext-locals ext)))
    sym))

;;;###autoload
(defun emacs-buffer-make-variable-buffer-local (sym)
  "Mark SYM so that any buffer that `setq's it gets a local binding.
In MVP this only affects the predicate `local-variable-if-set-p' and
the metadata.  Returns SYM."
  (unless (symbolp sym)
    (signal 'wrong-type-argument (list 'symbolp sym)))
  (cl-pushnew sym emacs-buffer--variable-buffer-local)
  sym)

;;;###autoload
(defun emacs-buffer-buffer-local-variables (&optional buf)
  "Return alist (SYM . VALUE) of all buffer-local bindings in BUF."
  (let* ((b (or buf (emacs-buffer--current)))
         (ext (gethash b emacs-buffer--state)))
    (if ext
        (mapcar (lambda (cell) (cons (car cell) (cdr cell)))
                (emacs-buffer--ext-locals ext))
      nil)))

;;;###autoload
(defun emacs-buffer-buffer-local-value (sym buf)
  "Return the value of SYM in BUF (= local if bound, else default).
Signals `void-variable' if neither a local binding nor a default
value exists.  Ignores narrowing / current-buffer state."
  (unless (symbolp sym)
    (signal 'wrong-type-argument (list 'symbolp sym)))
  (let* ((ext (gethash buf emacs-buffer--state))
         (cell (and ext (assq sym (emacs-buffer--ext-locals ext)))))
    (cond
     (cell (cdr cell))
     ((emacs-buffer-default-boundp sym)
      (emacs-buffer-default-value sym))
     (t (signal 'void-variable (list sym))))))

;;;###autoload
(defun emacs-buffer-set-buffer-local-value (sym buf value)
  "Set the buffer-local binding of SYM in BUF to VALUE.
If SYM is not yet local in BUF a binding is created on the fly.
Returns VALUE.

This is an explicit setter; our MVP does not intercept the host
`setq', so callers wanting buffer-local writes must funnel through
this helper (or `make-local-variable' + global `setq')."
  (unless (symbolp sym)
    (signal 'wrong-type-argument (list 'symbolp sym)))
  (let* ((ext (emacs-buffer--ensure-ext buf))
         (cell (assq sym (emacs-buffer--ext-locals ext))))
    (if cell
        (setcdr cell value)
      (push (cons sym value) (emacs-buffer--ext-locals ext)))
    value))

;;;###autoload
(defun emacs-buffer-local-variable-p (sym &optional buf)
  "Return non-nil if SYM has a buffer-local binding in BUF."
  (unless (symbolp sym)
    (signal 'wrong-type-argument (list 'symbolp sym)))
  (let* ((b (or buf (emacs-buffer--current)))
         (ext (gethash b emacs-buffer--state)))
    (and ext (assq sym (emacs-buffer--ext-locals ext)) t)))

;;;###autoload
(defun emacs-buffer-local-variable-if-set-p (sym &optional buf)
  "Return non-nil if SYM is *or would become* buffer-local in BUF on setq.
True when (a) SYM already has a local binding in BUF, or (b) SYM has
been globally marked via `make-variable-buffer-local'."
  (unless (symbolp sym)
    (signal 'wrong-type-argument (list 'symbolp sym)))
  (or (emacs-buffer-local-variable-p sym buf)
      (and (memq sym emacs-buffer--variable-buffer-local) t)))

;;;###autoload
(defun emacs-buffer-default-value (sym)
  "Return the default value of SYM.
Signals `void-variable' if SYM has no default value."
  (unless (symbolp sym)
    (signal 'wrong-type-argument (list 'symbolp sym)))
  (let ((cell (gethash sym emacs-buffer--default-values)))
    (cond
     ((and cell (car cell)) (cdr cell))
     (t (signal 'void-variable (list sym))))))

;;;###autoload
(defun emacs-buffer-default-boundp (sym)
  "Return t if SYM has a default value bound."
  (unless (symbolp sym)
    (signal 'wrong-type-argument (list 'symbolp sym)))
  (let ((cell (gethash sym emacs-buffer--default-values)))
    (and cell (car cell) t)))

;;;###autoload
(defun emacs-buffer-set-default (sym value)
  "Set the default value of SYM to VALUE and return VALUE.
The default value seeds new buffer-local bindings created by
`make-local-variable'."
  (unless (symbolp sym)
    (signal 'wrong-type-argument (list 'symbolp sym)))
  (puthash sym (cons t value) emacs-buffer--default-values)
  value)

;;;###autoload
(defmacro emacs-buffer-setq-default (sym value)
  "Set the default value of SYM to VALUE.  Macro form for symmetry with Emacs.
SYM is *not* evaluated; VALUE is."
  (declare (debug (symbolp form)))
  `(emacs-buffer-set-default ',sym ,value))

;;;###autoload
(defun emacs-buffer-kill-local-variable (sym &optional buf)
  "Remove the buffer-local binding of SYM in BUF, if any.  Return SYM."
  (unless (symbolp sym)
    (signal 'wrong-type-argument (list 'symbolp sym)))
  (let* ((b (or buf (emacs-buffer--current)))
         (ext (gethash b emacs-buffer--state)))
    (when ext
      (setf (emacs-buffer--ext-locals ext)
            (assq-delete-all sym (emacs-buffer--ext-locals ext))))
    sym))

;;;###autoload
(defun emacs-buffer-kill-all-local-variables (&optional buf)
  "Drop every buffer-local binding in BUF.  Return nil.
The MVP does not yet honor `permanent-local' markers — all locals are
removed unconditionally."
  (let* ((b (or buf (emacs-buffer--current)))
         (ext (gethash b emacs-buffer--state)))
    (when ext
      (setf (emacs-buffer--ext-locals ext) nil))
    nil))

;;; B. text-property  (11 APIs)
;;
;; Storage: a sorted list of intervals `(START END . PLIST)' kept in the
;; ext record.  Adjacent intervals with `equal' plists are coalesced on
;; insert so the list stays compact.  Doc 41 Phase 9c will swap this for
;; a balanced interval-tree once we need O(log n) lookups, but the API
;; shape is preserved.
;;
;; `category' inheritance: when a text-property `category' is set on the
;; range and points to a symbol with its own property list, lookups for
;; properties *not* explicitly set on the range fall back to the
;; symbol's plist via `(get CATEGORY-SYMBOL PROP)'.  This matches host
;; Emacs semantics — see `get-text-property' / `get-char-property'
;; below.  Inheritance is resolved lazily on lookup; the raw plist
;; returned by `text-property-at' / `text-properties-at' still carries
;; the `category' key unexpanded.

(defun emacs-buffer--tp-cell (start end plist)
  "Construct a text-prop interval cell."
  (cons start (cons end plist)))

(defun emacs-buffer--tp-start (cell) (car cell))
(defun emacs-buffer--tp-end   (cell) (cadr cell))
(defun emacs-buffer--tp-plist (cell) (cddr cell))

(defun emacs-buffer--tp-clip (intervals start end)
  "Remove the half-open range [START, END) from INTERVALS.
Returns a fresh list with overlapping intervals split as needed."
  (let (out)
    (dolist (cell intervals)
      (let ((s (emacs-buffer--tp-start cell))
            (e (emacs-buffer--tp-end cell))
            (p (emacs-buffer--tp-plist cell)))
        (cond
         ;; cell entirely before or after the clip → keep
         ((or (<= e start) (>= s end))
          (push cell out))
         ;; cell entirely inside clip → drop
         ((and (>= s start) (<= e end)) nil)
         ;; clip cuts off the cell's tail
         ((and (< s start) (<= e end))
          (push (emacs-buffer--tp-cell s start p) out))
         ;; clip cuts off the cell's head
         ((and (>= s start) (> e end))
          (push (emacs-buffer--tp-cell end e p) out))
         ;; clip splits the cell into two
         (t
          (push (emacs-buffer--tp-cell s start p) out)
          (push (emacs-buffer--tp-cell end e p) out)))))
    (sort (nreverse out) (lambda (a b)
                           (< (emacs-buffer--tp-start a)
                              (emacs-buffer--tp-start b))))))

(defun emacs-buffer--tp-merge (intervals start end plist)
  "Insert (START END PLIST) into INTERVALS, splitting overlapping cells.
Existing properties on the affected range are *replaced* (= classic
`put-text-property' semantics).  Returns a freshly-sorted list."
  (let* ((without (emacs-buffer--tp-clip intervals start end))
         (with    (cons (emacs-buffer--tp-cell start end plist) without)))
    (sort with (lambda (a b)
                 (< (emacs-buffer--tp-start a)
                    (emacs-buffer--tp-start b))))))

(defun emacs-buffer--tp-add (intervals start end plist)
  "Add the props in PLIST to existing props on [START, END) of INTERVALS.
Properties not mentioned in PLIST are left intact.  Returns a sorted
list."
  ;; 1. Snapshot existing plists across [start, end) as a list of
  ;;    (s e . p) tuples.
  (let (covered out (cur start))
    (dolist (cell intervals)
      (let ((s (emacs-buffer--tp-start cell))
            (e (emacs-buffer--tp-end cell))
            (p (emacs-buffer--tp-plist cell)))
        (cond
         ;; no overlap with [start, end)
         ((or (<= e start) (>= s end))
          (push cell out))
         (t
          ;; portion of cell before the affected range
          (when (< s start)
            (push (emacs-buffer--tp-cell s start p) out))
          ;; the overlapping piece — merge plist
          (let* ((os (max s start))
                 (oe (min e end))
                 (merged (emacs-buffer--plist-merge p plist)))
            (push (cons (cons os oe) merged) covered))
          ;; portion of cell after the affected range
          (when (> e end)
            (push (emacs-buffer--tp-cell end e p) out))))))
    ;; gaps inside [start, end) where no prior interval existed get
    ;; just PLIST.
    (let ((cells (sort (mapcar (lambda (c)
                                 (let ((rng (car c)) (pl (cdr c)))
                                   (emacs-buffer--tp-cell (car rng) (cdr rng) pl)))
                               covered)
                       (lambda (a b)
                         (< (emacs-buffer--tp-start a)
                            (emacs-buffer--tp-start b))))))
      (setq cur start)
      (dolist (c cells)
        (let ((s (emacs-buffer--tp-start c))
              (e (emacs-buffer--tp-end c)))
          (when (< cur s)
            (push (emacs-buffer--tp-cell cur s plist) out))
          (push c out)
          (setq cur e)))
      (when (< cur end)
        (push (emacs-buffer--tp-cell cur end plist) out)))
    (sort out (lambda (a b)
                (< (emacs-buffer--tp-start a)
                   (emacs-buffer--tp-start b))))))

(defun emacs-buffer--plist-merge (base extra)
  "Return a fresh plist where keys in EXTRA override BASE."
  (let ((out (copy-sequence base))
        (rest extra))
    (while rest
      (setq out (plist-put out (car rest) (cadr rest)))
      (setq rest (cddr rest)))
    out))

(defun emacs-buffer--tp-remove (intervals start end keys)
  "Drop KEYS from the props on [START, END) of INTERVALS.
KEYS is a flat list of symbol property names.  Returns a sorted list."
  (let (out)
    (dolist (cell intervals)
      (let ((s (emacs-buffer--tp-start cell))
            (e (emacs-buffer--tp-end cell))
            (p (emacs-buffer--tp-plist cell)))
        (cond
         ;; no overlap → keep
         ((or (<= e start) (>= s end))
          (push cell out))
         (t
          ;; before
          (when (< s start)
            (push (emacs-buffer--tp-cell s start p) out))
          ;; overlap with keys removed
          (let* ((os (max s start))
                 (oe (min e end))
                 (filtered (emacs-buffer--plist-drop p keys)))
            (when filtered
              (push (emacs-buffer--tp-cell os oe filtered) out)))
          ;; after
          (when (> e end)
            (push (emacs-buffer--tp-cell end e p) out))))))
    (sort out (lambda (a b)
                (< (emacs-buffer--tp-start a)
                   (emacs-buffer--tp-start b))))))

(defun emacs-buffer--plist-drop (plist keys)
  "Return a fresh plist with KEYS removed from PLIST."
  (let ((rest plist) out)
    (while rest
      (let ((k (car rest)) (v (cadr rest)))
        (unless (memq k keys)
          (setq out (plist-put out k v))))
      (setq rest (cddr rest)))
    out))

(defun emacs-buffer--tp-resolve-prop (plist prop)
  "Look up PROP in PLIST honouring `category' inheritance.
PROP set directly on PLIST wins; otherwise the value of the `category'
key (if it is a symbol) is consulted via its symbol-plist.  Returns nil
when neither carries PROP."
  (cond
   ((plist-member plist prop)
    (plist-get plist prop))
   (t
    (let ((cat (plist-get plist 'category)))
      (and (symbolp cat) cat (get cat prop))))))

(defun emacs-buffer--tp-cell-at (intervals pos)
  "Return the interval cell covering POS in INTERVALS, or nil."
  (catch 'found
    (dolist (cell intervals)
      (let ((s (emacs-buffer--tp-start cell))
            (e (emacs-buffer--tp-end cell)))
        (when (and (<= s pos) (< pos e))
          (throw 'found cell))))))

(defun emacs-buffer--tp-clamp-limit (pos limit)
  "If LIMIT is non-nil and POS > LIMIT, return LIMIT; else POS."
  (if (and limit (> pos limit)) limit pos))

;;;###autoload
(defun emacs-buffer-put-text-property (start end prop value &optional buf)
  "Set the text property PROP to VALUE on [START, END) in BUF.
START and END are 1-based.  Existing properties in PROP are
overwritten on this range; other properties are preserved."
  (unless (and (integerp start) (integerp end))
    (signal 'wrong-type-argument (list 'integerp start end)))
  (when (>= start end)
    (signal 'nelisp-ec-args-out-of-range (list start end)))
  (let* ((b (or buf (emacs-buffer--current)))
         (ext (emacs-buffer--ensure-ext b)))
    (setf (emacs-buffer--ext-text-props ext)
          (emacs-buffer--tp-add (emacs-buffer--ext-text-props ext)
                                start end (list prop value)))
    (cl-incf (emacs-buffer--ext-modified-tick ext))
    nil))

;;;###autoload
(defun emacs-buffer-get-text-property (pos prop &optional buf)
  "Return the value of property PROP at POS in BUF, or nil if unset.
POS is 1-based.  Honours `category' inheritance — when PROP is not
explicitly set on the range, the `category' value (if a symbol) is
consulted via `(get CATEGORY PROP)'."
  (unless (integerp pos)
    (signal 'wrong-type-argument (list 'integerp pos)))
  (let* ((b (or buf (emacs-buffer--current)))
         (ext (gethash b emacs-buffer--state)))
    (when ext
      (let ((cell (emacs-buffer--tp-cell-at
                   (emacs-buffer--ext-text-props ext) pos)))
        (and cell (emacs-buffer--tp-resolve-prop
                   (emacs-buffer--tp-plist cell) prop))))))

;;;###autoload
(defun emacs-buffer-add-text-properties (start end plist &optional buf)
  "Add PLIST properties to the half-open range [START, END) in BUF.
Properties not in PLIST are preserved on the affected range."
  (unless (and (integerp start) (integerp end))
    (signal 'wrong-type-argument (list 'integerp start end)))
  (when (>= start end)
    (signal 'nelisp-ec-args-out-of-range (list start end)))
  (unless (and (listp plist) (zerop (mod (length plist) 2)))
    (signal 'wrong-type-argument (list 'plist plist)))
  (let* ((b (or buf (emacs-buffer--current)))
         (ext (emacs-buffer--ensure-ext b)))
    (setf (emacs-buffer--ext-text-props ext)
          (emacs-buffer--tp-add (emacs-buffer--ext-text-props ext)
                                start end plist))
    (cl-incf (emacs-buffer--ext-modified-tick ext))
    nil))

;;;###autoload
(defun emacs-buffer-remove-text-properties (start end keys &optional buf)
  "Remove KEYS from the property plist on [START, END) in BUF.
KEYS is either a flat list of symbol names, or a plist whose keys we
look at (matching Emacs semantics where `remove-text-properties' takes
a (PROP nil ...) list)."
  (unless (and (integerp start) (integerp end))
    (signal 'wrong-type-argument (list 'integerp start end)))
  (when (>= start end)
    (signal 'nelisp-ec-args-out-of-range (list start end)))
  (let* ((b (or buf (emacs-buffer--current)))
         (ext (gethash b emacs-buffer--state)))
    (when ext
      (let ((kk (emacs-buffer--keys-from-arg keys)))
        (setf (emacs-buffer--ext-text-props ext)
              (emacs-buffer--tp-remove (emacs-buffer--ext-text-props ext)
                                       start end kk)))
      (cl-incf (emacs-buffer--ext-modified-tick ext)))
    nil))

(defun emacs-buffer--keys-from-arg (arg)
  "Return a flat list of property names from ARG.
ARG may be a list of symbols or an Emacs-style plist."
  (cond
   ((null arg) nil)
   ((cl-every #'symbolp arg) arg)
   (t
    (let (out (rest arg))
      (while rest
        (push (car rest) out)
        (setq rest (cddr rest)))
      (nreverse out)))))

;;;###autoload
(defun emacs-buffer-set-text-properties (start end plist &optional buf)
  "Replace the property plist on [START, END) in BUF with PLIST.
Existing properties on the range are discarded — only PLIST keys
remain.  When PLIST is nil the range is stripped of all properties."
  (unless (and (integerp start) (integerp end))
    (signal 'wrong-type-argument (list 'integerp start end)))
  (when (>= start end)
    (signal 'nelisp-ec-args-out-of-range (list start end)))
  (unless (and (listp plist) (zerop (mod (length plist) 2)))
    (signal 'wrong-type-argument (list 'plist plist)))
  (let* ((b (or buf (emacs-buffer--current)))
         (ext (emacs-buffer--ensure-ext b)))
    (setf (emacs-buffer--ext-text-props ext)
          (if (null plist)
              (emacs-buffer--tp-clip (emacs-buffer--ext-text-props ext)
                                     start end)
            (emacs-buffer--tp-merge (emacs-buffer--ext-text-props ext)
                                    start end plist)))
    (cl-incf (emacs-buffer--ext-modified-tick ext))
    nil))

;;;###autoload
(defun emacs-buffer-next-property-change (pos &optional buf limit)
  "Return the next position after POS at which any property changes.
Returns LIMIT (when given and reached) or nil when no further change."
  (unless (integerp pos)
    (signal 'wrong-type-argument (list 'integerp pos)))
  (let* ((b (or buf (emacs-buffer--current)))
         (ext (gethash b emacs-buffer--state))
         (intervals (and ext (emacs-buffer--ext-text-props ext))))
    (catch 'done
      (dolist (cell intervals)
        (let ((s (emacs-buffer--tp-start cell))
              (e (emacs-buffer--tp-end cell)))
          (cond
           ((and (<= s pos) (< pos e))
            (throw 'done (emacs-buffer--tp-clamp-limit e limit)))
           ((> s pos)
            (throw 'done (emacs-buffer--tp-clamp-limit s limit))))))
      limit)))

;;;###autoload
(defun emacs-buffer-previous-property-change (pos &optional buf limit)
  "Return the largest position less than POS at which any property changes.
Returns LIMIT (when given and reached) or nil when no earlier change."
  (unless (integerp pos)
    (signal 'wrong-type-argument (list 'integerp pos)))
  (let* ((b (or buf (emacs-buffer--current)))
         (ext (gethash b emacs-buffer--state))
         (intervals (and ext (emacs-buffer--ext-text-props ext)))
         (best nil))
    (dolist (cell intervals)
      (let ((s (emacs-buffer--tp-start cell))
            (e (emacs-buffer--tp-end cell)))
        (cond
         ;; Interval entirely before POS — its END is a change point.
         ((<= e pos) (setq best e))
         ;; Interval straddles POS — its START is a change point if it
         ;; is strictly before POS.
         ((and (<= s pos) (< pos e) (< s pos))
          (setq best s)))))
    (cond
     ((null best) limit)
     ((and limit (< best limit)) limit)
     (t best))))

(defun emacs-buffer--tp-prop-eq (a b)
  "Property-equality predicate matching Emacs `eq' on text-prop values."
  (eq a b))

;;;###autoload
(defun emacs-buffer-next-single-property-change (pos prop &optional buf limit)
  "Return the next position > POS where the value of PROP differs.
Honours `category' inheritance for the comparison.  Returns LIMIT
(when given and reached) or nil when no further change is found."
  (unless (integerp pos)
    (signal 'wrong-type-argument (list 'integerp pos)))
  (let* ((b (or buf (emacs-buffer--current)))
         (cur (emacs-buffer-get-text-property pos prop b))
         (scan pos)
         (next nil))
    (catch 'done
      (while t
        (setq next (emacs-buffer-next-property-change scan b limit))
        (cond
         ((null next) (throw 'done limit))
         ((and limit (>= next limit)) (throw 'done limit))
         (t
          (let ((val (emacs-buffer-get-text-property next prop b)))
            (unless (emacs-buffer--tp-prop-eq val cur)
              (throw 'done next))
            (setq scan next))))))))

;;;###autoload
(defun emacs-buffer-previous-single-property-change (pos prop &optional buf limit)
  "Return the largest position < POS where the value of PROP differs.
Honours `category' inheritance for the comparison.  Returns LIMIT
(when given and reached) or nil when no earlier change is found."
  (unless (integerp pos)
    (signal 'wrong-type-argument (list 'integerp pos)))
  (let* ((b (or buf (emacs-buffer--current)))
         (cur (emacs-buffer-get-text-property pos prop b))
         (scan pos)
         (prev nil))
    (catch 'done
      (while t
        (setq prev (emacs-buffer-previous-property-change scan b limit))
        (cond
         ((null prev) (throw 'done limit))
         ((and limit (<= prev limit)) (throw 'done limit))
         (t
          (let ((val (emacs-buffer-get-text-property (1- prev) prop b)))
            (unless (emacs-buffer--tp-prop-eq val cur)
              (throw 'done prev))
            (setq scan prev))))))))

(defun emacs-buffer--overlay-property-priority (ov)
  "Return OV's numeric overlay priority for property precedence."
  (let ((priority (plist-get (emacs-buffer--overlay-rec-properties ov)
                             'priority)))
    (if (integerp priority) priority 0)))

(defun emacs-buffer--sort-overlays-for-property (overlays)
  "Return OVERLAYS sorted by property precedence.
Higher numeric `priority' wins.  Ties are resolved by later insertion."
  (sort (copy-sequence overlays)
        (lambda (a b)
          (let ((pa (emacs-buffer--overlay-property-priority a))
                (pb (emacs-buffer--overlay-property-priority b)))
            (if (= pa pb)
                (> (emacs-buffer--overlay-rec-id a)
                   (emacs-buffer--overlay-rec-id b))
              (> pa pb))))))

;;;###autoload
(defun emacs-buffer-get-char-property (pos prop &optional buf)
  "Return the value of PROP at POS, checking overlays first, then text-props.
Overlay properties use numeric `priority' first; later insertion wins ties.
Falls back to
`emacs-buffer-get-text-property' (which honours `category' inheritance)
when no overlay carries PROP."
  (unless (integerp pos)
    (signal 'wrong-type-argument (list 'integerp pos)))
  (let* ((b (or buf (emacs-buffer--current)))
         (overlays (emacs-buffer--sort-overlays-for-property
                    (emacs-buffer-overlays-at pos b))))
    (or (catch 'hit
          (dolist (ov overlays)
            (let ((plist (emacs-buffer--overlay-rec-properties ov)))
              (when (plist-member plist prop)
                (throw 'hit (plist-get plist prop))))))
        (emacs-buffer-get-text-property pos prop b))))

;;;###autoload
(defun emacs-buffer-text-property-at (pos &optional buf)
  "Return the full property plist that holds at POS in BUF, or nil."
  (unless (integerp pos)
    (signal 'wrong-type-argument (list 'integerp pos)))
  (let* ((b (or buf (emacs-buffer--current)))
         (ext (gethash b emacs-buffer--state)))
    (when ext
      (catch 'found
        (dolist (cell (emacs-buffer--ext-text-props ext))
          (let ((s (emacs-buffer--tp-start cell))
                (e (emacs-buffer--tp-end cell)))
            (when (and (<= s pos) (< pos e))
              (throw 'found (copy-sequence (emacs-buffer--tp-plist cell))))))))))

;;;###autoload
(defun emacs-buffer-text-property-view (start end &optional properties buf)
  "Return text-property intervals intersecting [START, END) in BUF.
Each result is (SPAN-START SPAN-END PLIST), with SPAN-START and
SPAN-END clipped to the requested range.  When PROPERTIES is non-nil,
PLIST contains only those properties whose resolved value is non-nil,
honouring `category' inheritance in the same way as
`emacs-buffer-get-text-property'.  When PROPERTIES is nil, PLIST is a
copy of the raw interval plist.  The result is a snapshot; callers may
freely mutate returned plists."
  (unless (and (integerp start) (integerp end))
    (signal 'wrong-type-argument (list 'integerp start end)))
  (when (> start end)
    (signal 'nelisp-ec-args-out-of-range (list start end)))
  (let* ((b (or buf (emacs-buffer--current)))
         (ext (gethash b emacs-buffer--state))
         (props (and properties
                     (if (listp properties)
                         properties
                       (list properties))))
         out)
    (when ext
      (dolist (cell (emacs-buffer--ext-text-props ext))
        (let ((s (emacs-buffer--tp-start cell))
              (e (emacs-buffer--tp-end cell))
              (p (emacs-buffer--tp-plist cell)))
          (when (and (< s end) (> e start))
            (let ((view nil))
              (if props
                  (dolist (prop props)
                    (let ((value (emacs-buffer--tp-resolve-prop p prop)))
                      (when value
                        (setq view (plist-put view prop value)))))
                (setq view (copy-sequence p)))
              (when view
                (push (list (max s start) (min e end) view) out)))))))
    (nreverse out)))

;;; C. undo system  (5 APIs)
;;
;; Records pushed onto the undo list in chronological order:
;;   (TEXT . POS)         after a deletion of TEXT from POS
;;   (BEG . END)           after an insertion that covered [BEG, END)
;;   nil                   boundary marker
;; Undo-disabled buffers store t in the list slot (matches Emacs).

;;;###autoload
(defun emacs-buffer-buffer-undo-list (&optional buf)
  "Return the current undo list of BUF (or t if undo is disabled)."
  (let* ((b (or buf (emacs-buffer--current)))
         (ext (gethash b emacs-buffer--state)))
    (if ext (emacs-buffer--ext-undo-list ext) nil)))

;;;###autoload
(defun emacs-buffer-buffer-disable-undo (&optional buf)
  "Disable undo recording for BUF.  Returns t."
  (let* ((b (or buf (emacs-buffer--current)))
         (ext (emacs-buffer--ensure-ext b)))
    (setf (emacs-buffer--ext-undo-list ext) t)
    t))

;;;###autoload
(defun emacs-buffer-buffer-enable-undo (&optional buf)
  "Enable undo recording for BUF (clears any previous list).  Returns t."
  (let* ((b (or buf (emacs-buffer--current)))
         (ext (emacs-buffer--ensure-ext b)))
    (setf (emacs-buffer--ext-undo-list ext) nil)
    t))

;;;###autoload
(defun emacs-buffer-undo-boundary (&optional buf)
  "Push a boundary (nil) onto BUF's undo list.  Returns nil."
  (let* ((b (or buf (emacs-buffer--current)))
         (ext (emacs-buffer--ensure-ext b)))
    (unless (eq (emacs-buffer--ext-undo-list ext) t)
      (push nil (emacs-buffer--ext-undo-list ext)))
    nil))

;;;###autoload
(defun emacs-buffer-record-insertion (beg end &optional buf)
  "Record an insertion [BEG, END) into BUF's undo list.
Public helper because our MVP does not auto-instrument
`nelisp-ec-insert' (= would require modifying nelisp-emacs-compat).
Callers wanting full undo support call this immediately after each
insertion."
  (let* ((b (or buf (emacs-buffer--current)))
         (ext (emacs-buffer--ensure-ext b)))
    (unless (eq (emacs-buffer--ext-undo-list ext) t)
      (push (cons beg end) (emacs-buffer--ext-undo-list ext)))
    nil))

;;;###autoload
(defun emacs-buffer-record-deletion (text pos &optional buf)
  "Record a deletion of TEXT (string) starting at POS into BUF's undo list."
  (let* ((b (or buf (emacs-buffer--current)))
         (ext (emacs-buffer--ensure-ext b)))
    (unless (eq (emacs-buffer--ext-undo-list ext) t)
      (push (cons text pos) (emacs-buffer--ext-undo-list ext)))
    nil))

;;;###autoload
(defun emacs-buffer-undo (&optional buf)
  "Apply the most recent undo record to BUF and return what was applied.
Records are popped in chronological reverse order, skipping `nil'
boundaries.  Insertion records `(BEG . END)' delete that range.
Deletion records `(TEXT . POS)' re-insert TEXT at POS.  Signals
`emacs-buffer-undo-disabled' if undo is off.  Signals
`emacs-buffer-error' if the list is empty."
  (let* ((b (or buf (emacs-buffer--current)))
         (ext (emacs-buffer--ensure-ext b)))
    (when (eq (emacs-buffer--ext-undo-list ext) t)
      (signal 'emacs-buffer-undo-disabled (list b)))
    (let ((list (emacs-buffer--ext-undo-list ext)))
      ;; skip leading nil boundaries
      (while (and list (null (car list)))
        (setq list (cdr list)))
      (unless list
        (signal 'emacs-buffer-error '("Nothing to undo")))
      (let ((rec (car list)))
        (setf (emacs-buffer--ext-undo-list ext) (cdr list))
        (nelisp-ec-with-current-buffer b
          (cond
           ;; insertion record (BEG . END) where end > beg, both ints
           ((and (consp rec)
                 (integerp (car rec))
                 (integerp (cdr rec)))
            (nelisp-ec-delete-region (car rec) (cdr rec)))
           ;; deletion record (TEXT . POS)
           ((and (consp rec)
                 (stringp (car rec))
                 (integerp (cdr rec)))
            (let ((saved (nelisp-ec-point)))
              (nelisp-ec-goto-char (cdr rec))
              (nelisp-ec-insert (car rec))
              (nelisp-ec-goto-char (min saved (nelisp-ec-point-max)))))))
        rec))))

;;; D. modification tracking  (5 APIs)

;;;###autoload
(defun emacs-buffer-buffer-modified-p (&optional buf)
  "Return non-nil if BUF has been modified since the last save."
  (let ((b (or buf (emacs-buffer--current))))
    (nelisp-ec-buffer-modified-p b)))

;;;###autoload
(defun emacs-buffer-set-buffer-modified-p (flag &optional buf)
  "Set BUF's modified flag to FLAG (nil = unmodified, t = modified).
Returns FLAG."
  (let ((b (or buf (emacs-buffer--current))))
    (setf (nelisp-ec-buffer-modified-p b) flag)
    flag))

;;;###autoload
(defun emacs-buffer-restore-buffer-modified-p (flag &optional buf)
  "Restore BUF's modified flag to FLAG without bumping the chars tick.
Equivalent to `set-buffer-modified-p' in our MVP since we do not
automatically bump the tick on flag changes."
  (emacs-buffer-set-buffer-modified-p flag buf))

;;;###autoload
(defun emacs-buffer-toggle-read-only-direct (&optional buf)
  "Toggle BUF's `buffer-read-only' flag and return a result plist.
BUF defaults to the current buffer.  The result contains `:read-only' and
`:message', suitable for frontend echo/status display."
  (let ((buffer (or buf (emacs-buffer--current))))
    (if (and (fboundp 'nelisp-ec-buffer-p)
             (nelisp-ec-buffer-p buffer))
        (nelisp-ec-with-current-buffer buffer
          (setq buffer-read-only (not buffer-read-only))
          (list :read-only buffer-read-only
                :message
                (format "buffer-read-only: %s"
                        (if buffer-read-only "on" "off"))))
      (with-current-buffer buffer
        (setq buffer-read-only (not buffer-read-only))
        (list :read-only buffer-read-only
              :message
              (format "buffer-read-only: %s"
                      (if buffer-read-only "on" "off")))))))

;;;###autoload
(defun emacs-buffer-buffer-chars-modified-tick (&optional buf)
  "Return the chars-modification tick of BUF (monotonic counter).
Bumped via `emacs-buffer-bump-modified-tick' which callers invoke
after each text mutation (MVP cannot auto-instrument nelisp-ec-*)."
  (let* ((b (or buf (emacs-buffer--current)))
         (ext (emacs-buffer--ensure-ext b)))
    (emacs-buffer--ext-modified-tick ext)))

;;;###autoload
(defun emacs-buffer-buffer-text-tick (&optional buf)
  "Return BUF's TEXT-CONTENT modification tick (Phase 3.B.7).
Distinct from the chars-modified-tick: this counter is bumped only
when the buffer's text bytes change (= insert / delete via
`nelisp-ec-insert' / `nelisp-ec-delete-region'), not when text-
properties or overlays change.  Useful as a cache key for consumers
that cache textual buffer snapshots."
  (let* ((b (or buf (emacs-buffer--current)))
         (ext (and b (gethash b emacs-buffer--state))))
    (if ext (emacs-buffer--ext-text-tick ext) 0)))

(defun emacs-buffer--bump-text-tick-advice (&rest _args)
  "After-advice for nelisp-ec-insert / -delete-region: bump text-tick
on the current buffer's ext (= so `emacs-buffer-buffer-text-tick'
reflects every text-content mutation)."
  (let* ((b (and (boundp 'nelisp-ec--current-buffer)
                 nelisp-ec--current-buffer))
         (ext (and b (emacs-buffer--ensure-ext b))))
    (when ext
      (cl-incf (emacs-buffer--ext-text-tick ext)))))

(advice-add 'nelisp-ec-insert        :after #'emacs-buffer--bump-text-tick-advice)
(advice-add 'nelisp-ec-delete-region :after #'emacs-buffer--bump-text-tick-advice)
(advice-add 'nelisp-ec-erase-buffer  :after #'emacs-buffer--bump-text-tick-advice)

;;;###autoload
(defun emacs-buffer-bump-modified-tick (&optional buf)
  "Increment BUF's chars-modification tick.  Returns the new value."
  (let* ((b (or buf (emacs-buffer--current)))
         (ext (emacs-buffer--ensure-ext b)))
    (cl-incf (emacs-buffer--ext-modified-tick ext))))

;;;###autoload
(defmacro emacs-buffer-modify-without-undo (&rest body)
  "Run BODY with undo recording temporarily disabled in the current buffer.
Restores the prior undo-list (or t) on normal *and* non-local exit."
  (declare (indent 0) (debug (body)))
  (let ((ext (make-symbol "ext"))
        (saved (make-symbol "saved")))
    `(let* ((,ext (emacs-buffer--ensure-ext (emacs-buffer--current)))
            (,saved (emacs-buffer--ext-undo-list ,ext)))
       (unwind-protect
           (progn
             (setf (emacs-buffer--ext-undo-list ,ext) t)
             ,@body)
         (setf (emacs-buffer--ext-undo-list ,ext) ,saved)))))

;;; E. additional buffer ops  (5 APIs)

;;;###autoload
(defun emacs-buffer-clone-indirect-buffer (newname &optional buf)
  "Return a fresh `nelisp-ec-buffer' that shares text storage with BUF.
The clone has its own POINT / narrowing / locals / undo list, but the
underlying `nelisp-text-buffer' (= text storage) is shared by direct
struct-slot reference so edits in either propagate.

NEWNAME is uniquified the same way `nelisp-ec-generate-new-buffer'
does it.  Returns the clone."
  (let* ((base (or buf (emacs-buffer--current)))
         (clone (nelisp-ec-generate-new-buffer
                 (or newname (nelisp-ec-buffer-name base))))
         (ext (emacs-buffer--ensure-ext clone)))
    ;; Share the underlying text storage (= same nelisp-text-buffer).
    (setf (nelisp-ec-buffer-text clone) (nelisp-ec-buffer-text base))
    (setf (emacs-buffer--ext-base-buffer ext) base)
    clone))

;;;###autoload
(defun emacs-buffer-buffer-base-buffer (&optional buf)
  "Return the base buffer of BUF if it is an indirect clone, else nil."
  (let* ((b (or buf (emacs-buffer--current)))
         (ext (gethash b emacs-buffer--state)))
    (and ext (emacs-buffer--ext-base-buffer ext))))

;;;###autoload
(defun emacs-buffer-buffer-list ()
  "Return the list of all live buffers in the NeLisp world.
Order is most-recent-first by registration (= reverse of internal
alist so callers see a stable enumeration)."
  (mapcar #'cdr nelisp-ec--buffers))

;;;###autoload
(defun emacs-buffer-buffers-by-mode (predicate)
  "Return the list of live buffers whose ext state satisfies PREDICATE.
PREDICATE is called with one argument (the `nelisp-ec-buffer') and
should return non-nil to include the buffer.

This is a lightweight helper that lets callers keep their own concept
of `major-mode' as a buffer-local variable and filter cheaply without
maintaining a parallel index."
  (cl-remove-if-not predicate (emacs-buffer-buffer-list)))

;;;###autoload
(defun emacs-buffer-generate-new-buffer-name (base)
  "Return a buffer name based on BASE that is not currently in use.
Like `generate-new-buffer-name' but pure (= does NOT create a buffer)."
  (unless (stringp base)
    (signal 'wrong-type-argument (list 'stringp base)))
  (if (null (assoc base nelisp-ec--buffers))
      base
    (let ((n 2))
      (while (assoc (format "%s<%d>" base n) nelisp-ec--buffers)
        (setq n (1+ n)))
      (format "%s<%d>" base n))))

;;; F. overlay  (15 APIs)
;;
;; Self-contained overlay primitives keyed by `nelisp-ec-buffer'.
;; Phase 1 §4.2 — vendor/emacs-lisp/ does not provide overlay.el
;; (upstream Emacs implements it in buffer.c) and the Layer 1
;; `nelisp-overlay' package assumes host Emacs `bufferp', so there is
;; no impedance-free delegate path.  This section implements the
;; minimum viable subset directly on top of the existing
;; `emacs-buffer--ext' side-table.  Doc 41 §3.4 (Phase 9c.4) provider
;; semantics — priority / insertion-order tie-break, front/rear advance
;; endpoint behaviour — are honoured.

(defvar emacs-buffer--overlay-counter 0
  "Monotonic counter for overlay insertion stamps.
Used as the priority tie-break key (Doc 41 §2.5 LOCKED v1: ties broken
by insertion order so the LATER overlay wins).")

(cl-defstruct (emacs-buffer--overlay
               (:constructor emacs-buffer--overlay-make)
               (:copier nil)
               (:predicate emacs-buffer--overlay-record-p)
               (:conc-name emacs-buffer--overlay-rec-))
  "Overlay record stored on an `emacs-buffer--ext' OVERLAYS list.

Slots:
- ID            : monotonic insertion stamp (priority tie-break).
- START / END   : 1-based positions in BUFFER, half-open [START, END).
- BUFFER        : back-reference to the owning `nelisp-ec-buffer', or
                  nil after `delete-overlay'.
- FRONT-ADVANCE : t = START moves on insertion at START.
- REAR-ADVANCE  : t = END moves on insertion at END.
- PROPERTIES    : property plist."
  (id            0   :type integer)
  (start         0   :type integer)
  (end           0   :type integer)
  (buffer        nil)
  (front-advance nil)
  (rear-advance  nil)
  (properties    nil))

;;;###autoload
(defun emacs-buffer-overlayp (object)
  "Return non-nil if OBJECT is an `emacs-buffer' overlay record."
  (and (emacs-buffer--overlay-record-p object) t))

(defun emacs-buffer--overlay-alive-p (ov)
  "Return non-nil if OV is still attached to a buffer."
  (and (emacs-buffer--overlay-record-p ov)
       (emacs-buffer--overlay-rec-buffer ov)
       t))

(defun emacs-buffer--overlay-check-alive (ov)
  "Signal `emacs-buffer-error' unless OV is still attached."
  (unless (emacs-buffer--overlay-alive-p ov)
    (signal 'emacs-buffer-error (list "Dead overlay" ov))))

(defun emacs-buffer--overlay-insert-sorted (buf rec)
  "Insert REC into BUF's overlay list, keeping ascending START order.
Ties on START preserve insertion order (= REC goes after any existing
record with equal START)."
  (let* ((ext (emacs-buffer--ensure-ext buf))
         (lst (emacs-buffer--ext-overlays ext))
         (start (emacs-buffer--overlay-rec-start rec)))
    (cond
     ((null lst)
      (setf (emacs-buffer--ext-overlays ext) (list rec)))
     ((< start (emacs-buffer--overlay-rec-start (car lst)))
      (setf (emacs-buffer--ext-overlays ext) (cons rec lst)))
     (t
      (let ((tail lst))
        (while (and (cdr tail)
                    (<= (emacs-buffer--overlay-rec-start (cadr tail)) start))
          (setq tail (cdr tail)))
        (setcdr tail (cons rec (cdr tail))))))))

(defun emacs-buffer--overlay-remove-rec (buf rec)
  "Remove REC from BUF's overlay list (one-shot delq)."
  (let ((ext (gethash buf emacs-buffer--state)))
    (when ext
      (setf (emacs-buffer--ext-overlays ext)
            (delq rec (emacs-buffer--ext-overlays ext))))))

(defun emacs-buffer--overlay-re-sort (buf)
  "Re-sort BUF's overlay list by ascending START (stable on insertion id)."
  (let ((ext (gethash buf emacs-buffer--state)))
    (when (and ext (emacs-buffer--ext-overlays ext))
      (setf (emacs-buffer--ext-overlays ext)
            (sort (copy-sequence (emacs-buffer--ext-overlays ext))
                  (lambda (a b)
                    (let ((sa (emacs-buffer--overlay-rec-start a))
                          (sb (emacs-buffer--overlay-rec-start b)))
                      (if (= sa sb)
                          (< (emacs-buffer--overlay-rec-id a)
                             (emacs-buffer--overlay-rec-id b))
                        (< sa sb)))))))))

;;;###autoload
(defun emacs-buffer-make-overlay (beg end &optional buf front-adv rear-adv)
  "Create an overlay covering [BEG, END) in BUF (default = current).
FRONT-ADV and REAR-ADV control endpoint behaviour under insertion at
the respective endpoint, mirroring Emacs `make-overlay' semantics."
  (unless (and (integerp beg) (integerp end))
    (signal 'wrong-type-argument (list 'integerp beg end)))
  (let* ((buffer (or buf (emacs-buffer--current)))
         (s (min beg end))
         (e (max beg end))
         (id (cl-incf emacs-buffer--overlay-counter))
         (rec (emacs-buffer--overlay-make
               :id id :start s :end e :buffer buffer
               :front-advance (and front-adv t)
               :rear-advance  (and rear-adv  t)
               :properties nil)))
    (emacs-buffer--overlay-insert-sorted buffer rec)
    rec))

;;;###autoload
(defun emacs-buffer-overlay-start (ov)
  "Return the START position of OV, or nil if OV has been deleted."
  (and (emacs-buffer--overlay-alive-p ov)
       (emacs-buffer--overlay-rec-start ov)))

;;;###autoload
(defun emacs-buffer-overlay-end (ov)
  "Return the END position of OV, or nil if OV has been deleted."
  (and (emacs-buffer--overlay-alive-p ov)
       (emacs-buffer--overlay-rec-end ov)))

;;;###autoload
(defun emacs-buffer-overlay-buffer (ov)
  "Return the buffer that OV is attached to, or nil if OV has been deleted."
  (and (emacs-buffer--overlay-alive-p ov)
       (emacs-buffer--overlay-rec-buffer ov)))

;;;###autoload
(defun emacs-buffer-overlay-properties (ov)
  "Return a fresh copy of OV's property list.
Signals `emacs-buffer-error' if OV has been deleted."
  (emacs-buffer--overlay-check-alive ov)
  (copy-sequence (emacs-buffer--overlay-rec-properties ov)))

;;;###autoload
(defun emacs-buffer-overlay-put (ov prop value)
  "Set property PROP of OV to VALUE.  Returns VALUE.
Signals `emacs-buffer-error' if OV has been deleted."
  (emacs-buffer--overlay-check-alive ov)
  (setf (emacs-buffer--overlay-rec-properties ov)
        (plist-put (emacs-buffer--overlay-rec-properties ov) prop value))
  value)

;;;###autoload
(defun emacs-buffer-overlay-get (ov prop)
  "Return PROP of OV, or nil if it is not set.
Signals `emacs-buffer-error' if OV has been deleted."
  (emacs-buffer--overlay-check-alive ov)
  (plist-get (emacs-buffer--overlay-rec-properties ov) prop))

;;;###autoload
(defun emacs-buffer-move-overlay (ov beg end &optional buf)
  "Move OV to cover [BEG, END) in BUF (default = OV's current buffer).
Returns OV.  Signals `emacs-buffer-error' if OV has been deleted."
  (emacs-buffer--overlay-check-alive ov)
  (let* ((old-buf (emacs-buffer--overlay-rec-buffer ov))
         (new-buf (or buf old-buf))
         (s (min beg end))
         (e (max beg end)))
    (unless (eq old-buf new-buf)
      (emacs-buffer--overlay-remove-rec old-buf ov))
    (setf (emacs-buffer--overlay-rec-start ov) s
          (emacs-buffer--overlay-rec-end ov) e
          (emacs-buffer--overlay-rec-buffer ov) new-buf)
    (if (eq old-buf new-buf)
        (emacs-buffer--overlay-re-sort new-buf)
      (emacs-buffer--overlay-insert-sorted new-buf ov))
    ov))

;;;###autoload
(defun emacs-buffer-delete-overlay (ov)
  "Detach OV from its buffer.  Idempotent.  Returns nil."
  (when (emacs-buffer--overlay-alive-p ov)
    (let ((buf (emacs-buffer--overlay-rec-buffer ov)))
      (emacs-buffer--overlay-remove-rec buf ov)
      (setf (emacs-buffer--overlay-rec-buffer ov) nil)))
  nil)

;;;###autoload
(defun emacs-buffer-delete-all-overlays (&optional buf)
  "Remove every overlay in BUF (default = current).  Returns nil."
  (let* ((buffer (or buf (emacs-buffer--current)))
         (ext (gethash buffer emacs-buffer--state)))
    (when ext
      (dolist (ov (emacs-buffer--ext-overlays ext))
        (setf (emacs-buffer--overlay-rec-buffer ov) nil))
      (setf (emacs-buffer--ext-overlays ext) nil)))
  nil)

;;;###autoload
(defun emacs-buffer-overlays-at (pos &optional buf)
  "Return overlays at POS in BUF (default = current).
Order: by ascending START, ties broken by insertion order."
  (let* ((buffer (or buf (emacs-buffer--current)))
         (ext (gethash buffer emacs-buffer--state)))
    (when ext
      (cl-remove-if-not
       (lambda (ov)
         (and (<= (emacs-buffer--overlay-rec-start ov) pos)
              (< pos (emacs-buffer--overlay-rec-end ov))))
       (emacs-buffer--ext-overlays ext)))))

;;;###autoload
(defun emacs-buffer-overlays-in (beg end &optional buf)
  "Return overlays in BUF (default = current) that overlap [BEG, END)."
  (let* ((buffer (or buf (emacs-buffer--current)))
         (ext (gethash buffer emacs-buffer--state)))
    (when ext
      (cl-remove-if-not
       (lambda (ov)
         (and (< (emacs-buffer--overlay-rec-start ov) end)
              (> (emacs-buffer--overlay-rec-end ov) beg)))
       (emacs-buffer--ext-overlays ext)))))

;;;###autoload
(defun emacs-buffer-overlay-lists (&optional buf)
  "Return (BEFORE . AFTER) overlays in BUF (default = current).
Phase 1 — partition by START relative to BUF's current point.
BEFORE holds overlays starting at or before POINT, AFTER the rest."
  (let* ((buffer (or buf (emacs-buffer--current)))
         (ext (gethash buffer emacs-buffer--state)))
    (when ext
      (let ((point (nelisp-ec-buffer-point buffer))
            before after)
        (dolist (ov (emacs-buffer--ext-overlays ext))
          (if (<= (emacs-buffer--overlay-rec-start ov) point)
              (push ov before)
            (push ov after)))
        (cons (nreverse before) (nreverse after))))))

;;;###autoload
(defun emacs-buffer-copy-overlay (ov)
  "Return a fresh copy of OV in the same buffer with the same range / props.
The copy receives a fresh insertion stamp."
  (emacs-buffer--overlay-check-alive ov)
  (let* ((buf (emacs-buffer--overlay-rec-buffer ov))
         (id (cl-incf emacs-buffer--overlay-counter))
         (rec (emacs-buffer--overlay-make
               :id id
               :start (emacs-buffer--overlay-rec-start ov)
               :end (emacs-buffer--overlay-rec-end ov)
               :buffer buf
               :front-advance (emacs-buffer--overlay-rec-front-advance ov)
               :rear-advance  (emacs-buffer--overlay-rec-rear-advance ov)
               :properties (copy-sequence
                            (emacs-buffer--overlay-rec-properties ov)))))
    (emacs-buffer--overlay-insert-sorted buf rec)
    rec))

(provide 'emacs-buffer)
;;; emacs-buffer.el ends here
