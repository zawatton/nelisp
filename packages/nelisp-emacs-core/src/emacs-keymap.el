;;; emacs-keymap.el --- Emacs C keymap.c port on top of nelisp-emacs-compat  -*- lexical-binding: t; -*-

;; Phase 1 module 4/6 per nelisp-emacs Doc 01 (LOCKED-2026-04-25-v2).
;; Layer: nelisp-emacs (Layer 2 extension on top of NeLisp).
;; Namespace: `emacs-keymap-' so loading inside a host Emacs does NOT
;; shadow `define-key', `lookup-key', `global-map', etc.
;;
;; Foundation contracts:
;;   - Doc 34 §2.3 keymap chain (LOCKED v1, 7 段 precedence, =KEYMAP_CONTRACT_VERSION = 1=)
;;   - Doc 34 §4.3 17 件 contract (make-keymap / make-sparse-keymap /
;;     define-key / lookup-key / global-map / current-local-map /
;;     use-local-map / set-keymap-parent / keymap-parent / copy-keymap /
;;     keymap-prompt / map-keymap / overriding-terminal-local-map /
;;     overriding-local-map / minor-mode-overriding-map-alist /
;;     minor-mode-map-alist / emulation-mode-map-alists)
;;   - Doc 41 §2.5 KEYMAP_CHAIN_INJECT_CONTRACT_VERSION = 1 opt-in flag
;;     for the 9 段 chain (overlay / text-property keymap inject), gated
;;     behind `emacs-keymap-chain-with-textprop' (default nil).  The
;;     actual textprop / overlay slot providers are wired by Phase 9c
;;     code via `emacs-keymap-chain-textprop-provider' /
;;     `emacs-keymap-chain-overlay-provider' (both nil here = no-op).
;;
;; API surface (~25 public APIs across 5 categories):
;;
;;   A. constructors / predicates  (4 APIs)
;;      make-keymap / make-sparse-keymap / keymapp / copy-keymap
;;
;;   B. mutators / accessors  (5 APIs)
;;      define-key / lookup-key / keymap-parent / set-keymap-parent /
;;      keymap-prompt
;;
;;   C. global / local / overriding maps  (8 APIs + 5 special vars)
;;      global-map (var) / current-global-map / use-global-map
;;      current-local-map / use-local-map
;;      overriding-local-map (var) / overriding-terminal-local-map (var)
;;      minor-mode-overriding-map-alist (var) /
;;      minor-mode-map-alist (var) / emulation-mode-map-alists (var)
;;      Doc 41 chain provider helper: emacs-keymap-chain-at /
;;      emacs-keymap-chain-with-textprop (defcustom, default nil)
;;
;;   D. lookup helpers  (5 APIs)
;;      key-binding / map-keymap / where-is-internal /
;;      substitute-key-definition / key-description
;;
;;   E. minimal command-loop scaffolding  (3 APIs)
;;      this-command-keys / this-command-keys-vector / read-key-sequence
;;      (read-key-sequence is intentionally a thin synchronous reader
;;       that routes through `emacs-keymap--read-event-fn' so that
;;       Phase 1 emacs-minibuffer / Phase 9b event-loop can plug their
;;       own event source.  Default is a buffered FIFO suitable for
;;       ERT.)
;;
;; Non-goals (deferred per task spec):
;;   - keyboard input event 解釈 (= emacs-minibuffer.el / event handler)
;;   - keymap menu integration (= 別 phase, menu items in the (KEY . BINDING)
;;     pair are stored opaquely, not resolved)
;;   - text-property / overlay keymap actual lookup (= Phase 9c, we only
;;     provide the slot for opt-in injection; default flag = nil keeps
;;     Doc 34 7 段 chain byte-identical)
;;   - mouse / function-key remap (= deferred to event-handler module)

;;; Code:

(require 'cl-lib)

;;; Errors

(define-error 'emacs-keymap-error "emacs-keymap error")
(define-error 'emacs-keymap-not-keymap
  "Expected a keymap" 'emacs-keymap-error)
(define-error 'emacs-keymap-bad-key
  "Bad key sequence form" 'emacs-keymap-error)

;;; Contract version constants (per Doc 34 §4.3 / Doc 41 §2.5)

(defconst emacs-keymap-contract-version 1
  "KEYMAP_CONTRACT_VERSION per Doc 34 §4.3.
Bumped on incompatible binary semantic change to the 7 段 chain.")

(defconst emacs-keymap-chain-inject-contract-version 1
  "KEYMAP_CHAIN_INJECT_CONTRACT_VERSION per Doc 41 §2.5.
Bumped on incompatible change to the 9 段 chain (= 7 段 + overlay
+ text-property slot).  This module exposes the slot only; actual
provider wiring is done by Phase 9c code.")

;;; Customization

(defcustom emacs-keymap-chain-with-textprop nil
  "Non-nil = use the Doc 41 §2.5 9 段 chain provider.
When nil (default) the chain is the Doc 34 §2.3 7 段, byte-identical
to the SHIPPED v=1 contract.  When t and a non-nil provider is set
in `emacs-keymap-chain-overlay-provider' /
`emacs-keymap-chain-textprop-provider', their results are spliced
into precedence slots 6 and 7 respectively.

This is an opt-in flag — Doc 41 calls it
`nelisp-keymap-chain-with-textprop'.  The two names are kept distinct
so this module can be tested in isolation without pulling Phase 9c."
  :type 'boolean
  :group 'emacs-keymap)

(defvar emacs-keymap-chain-overlay-provider nil
  "Function returning a list of overlay keymaps active at POINT.
Phase 9c wires this; when nil the overlay slot is empty.
Signature: (FN POINT) -> (KEYMAP ...).  Provider must obey the
sub-precedence rules in Doc 41 §2.5 (overlay priority desc,
insertion order tiebreak, keymap > local-map within an overlay).")

(defvar emacs-keymap-chain-textprop-provider nil
  "Function returning a list of text-property keymaps at POINT.
Phase 9c wires this; when nil the text-property slot is empty.
Signature: (FN POINT) -> (KEYMAP ...).  Provider must obey the
sub-precedence rules in Doc 41 §2.5 (shortest range first, larger
start position wins on tie).")

;;; A. constructors / predicates

;;;###autoload
(defun emacs-keymap-make-sparse-keymap (&optional prompt)
  "Construct and return a new sparse keymap.
PROMPT, if non-nil, is a string used by `read-key-sequence' menu prompt.
The returned object satisfies `emacs-keymap-keymapp'.

Format: =(keymap [PROMPT])= initially.  Later `define-key' calls
prepend =(KEY . BINDING)= entries between the head symbol and the
optional prompt string.  Compatible with Emacs `make-sparse-keymap'."
  (if prompt
      (list 'keymap prompt)
    (list 'keymap)))

;;;###autoload
(defun emacs-keymap-make-keymap (&optional prompt)
  "Construct and return a new full keymap (= 256 ASCII slot table).
PROMPT, if non-nil, is a string used as menu prompt.

Internally this is a sparse keymap whose tail begins with a
char-table-like vector slot we model as a single =(t . SLOT-VECTOR)=
entry.  Lookup by character integer falls back to vector index.
This matches the visible Emacs API surface; full traversal via
`emacs-keymap-map-keymap' yields each non-nil slot as a (CHAR . BINDING)
pair followed by any sparse entries added later."
  (let ((slot-vec (make-vector 256 nil)))
    (if prompt
        (list 'keymap (cons t slot-vec) prompt)
      (list 'keymap (cons t slot-vec)))))

;;;###autoload
(defun emacs-keymap-keymapp (object)
  "Return non-nil if OBJECT is a keymap (i.e. starts with the symbol `keymap')."
  (and (consp object) (eq (car object) 'keymap)))

;;;###autoload
(defun emacs-keymap-copy-keymap (keymap)
  "Return a deep copy of KEYMAP.
Sub-keymaps are copied recursively.  Bindings (functions, symbols,
strings, integers) are returned as-is (= shared)."
  (unless (emacs-keymap-keymapp keymap)
    (signal 'emacs-keymap-not-keymap (list keymap)))
  (cons 'keymap
        (mapcar
         (lambda (e)
           (cond
            ((and (consp e) (eq (car e) t) (vectorp (cdr e)))
             ;; Full-keymap slot vector — copy element-wise, recurse on
             ;; nested keymap bindings.
             (let* ((src (cdr e))
                    (n (length src))
                    (dst (make-vector n nil)))
               (dotimes (i n)
                 (let ((b (aref src i)))
                   (aset dst i (if (emacs-keymap-keymapp b)
                                   (emacs-keymap-copy-keymap b)
                                 b))))
               (cons t dst)))
            ((and (consp e) (emacs-keymap-keymapp (cdr e)))
             (cons (car e) (emacs-keymap-copy-keymap (cdr e))))
            ((consp e) (cons (car e) (cdr e)))
            (t e)))
         (cdr keymap))))

;;; key sequence normalization

(defun emacs-keymap--key-seq->list (key)
  "Normalize KEY (string / vector / list) into a list of key elements.
Each element is a character (integer) or a symbol (function key).
Modifier-bearing characters are passed through as-is."
  (cond
   ((null key)
    (signal 'emacs-keymap-bad-key (list key)))
   ((vectorp key)
    (append key nil))
   ((stringp key)
    (append key nil))
   ((listp key)
    key)
   ((integerp key)
    (list key))
   ((symbolp key)
    (list key))
   (t (signal 'emacs-keymap-bad-key (list key)))))

;;; B. mutators / accessors

;;;###autoload
(defun emacs-keymap-define-key (keymap key def)
  "In KEYMAP, define key sequence KEY as DEF.
KEY is a string, vector, or list of keys.  DEF can be:
  - nil (= remove binding)
  - a command symbol
  - a function (lambda / closure / byte-compiled)
  - a keymap (= prefix)
  - a string (= keyboard macro)
  - any (KEYMAP-or-OTHER) cons that Emacs treats as menu/binding

Returns DEF.  If KEY is a multi-element sequence and the intermediate
binding is not a keymap, signal `emacs-keymap-bad-key' (= matches Emacs
\"Key sequence ... uses invalid prefix characters\")."
  (unless (emacs-keymap-keymapp keymap)
    (signal 'emacs-keymap-not-keymap (list keymap)))
  (let ((keys (emacs-keymap--key-seq->list key)))
    (when (null keys)
      (signal 'emacs-keymap-bad-key (list key)))
    (emacs-keymap--define-key-1 keymap keys def)
    def))

(defun emacs-keymap--define-key-1 (keymap keys def)
  "Recursive helper for `emacs-keymap-define-key'."
  (let ((k (car keys)) (rest (cdr keys)))
    (cond
     ((null rest)
      ;; terminal: install binding
      (emacs-keymap--set-binding keymap k def))
     (t
      ;; intermediate: ensure prefix keymap exists, recurse
      (let ((sub (emacs-keymap--get-binding keymap k)))
        (unless (emacs-keymap-keymapp sub)
          (setq sub (emacs-keymap-make-sparse-keymap))
          (emacs-keymap--set-binding keymap k sub))
        (emacs-keymap--define-key-1 sub rest def))))))

(defun emacs-keymap--full-slot (keymap)
  "Return the (t . VECTOR) cell of KEYMAP, or nil if KEYMAP is sparse."
  (let ((tail (cdr keymap)))
    (cl-loop for e in tail
             when (and (consp e) (eq (car e) t) (vectorp (cdr e)))
             return e)))

(defun emacs-keymap--set-binding (keymap k def)
  "Install K -> DEF in KEYMAP, replacing any existing binding."
  (let ((slot (emacs-keymap--full-slot keymap)))
    (if (and slot (integerp k) (>= k 0) (< k (length (cdr slot))))
        ;; full-keymap slot path
        (aset (cdr slot) k def)
      ;; sparse path: search existing pair, then mutate or prepend
      (let ((existing (cl-loop for e in (cdr keymap)
                               when (and (consp e)
                                         (not (eq (car e) t))
                                         (equal (car e) k))
                               return e)))
        (cond
         (existing
          (if def
              (setcdr existing def)
            ;; nil = remove
            (setcdr keymap (delq existing (cdr keymap)))))
         (def
          ;; Insert AFTER the (t . VEC) slot if any, else right after the
          ;; head; this preserves the prompt string at the very tail.
          (let ((tail (cdr keymap)))
            (if (and tail (consp (car tail)) (eq (caar tail) t)
                     (vectorp (cdar tail)))
                (setcdr tail (cons (cons k def) (cdr tail)))
              (setcdr keymap (cons (cons k def) tail))))))))))

(defun emacs-keymap--get-binding (keymap k)
  "Return KEYMAP's binding for K, or nil.
Walks the keymap *without* parent inheritance — that is the caller's
job (see `emacs-keymap--lookup-with-parent')."
  (let ((slot (emacs-keymap--full-slot keymap)))
    (or (and slot (integerp k) (>= k 0) (< k (length (cdr slot)))
             (aref (cdr slot) k))
        (cl-loop for e in (cdr keymap)
                 when (and (consp e)
                           (not (eq (car e) t))
                           (equal (car e) k))
                 return (cdr e)))))

(defun emacs-keymap--lookup-with-parent (keymap k)
  "Return KEYMAP's binding for K, walking parent inheritance.
Returns nil if not found in this keymap or any ancestor."
  (or (emacs-keymap--get-binding keymap k)
      (let ((p (emacs-keymap-keymap-parent keymap)))
        (and p (emacs-keymap--lookup-with-parent p k)))))

;;;###autoload
(defun emacs-keymap-lookup-key (keymap key &optional accept-default)
  "Look up KEY in KEYMAP, return its binding.
KEY is a string, vector or list (see `emacs-keymap-define-key').
If KEY is incomplete (= a strict prefix of some bound sequence), return
a sub-keymap.  If KEY is undefined return nil.  If KEY is longer than
any defined sequence, return the integer count of unused trailing
elements (= matches Emacs `lookup-key' partial-match contract).

ACCEPT-DEFAULT, when non-nil, also returns bindings of `t` (= the
default-binding fallback inside any one keymap)."
  (unless (emacs-keymap-keymapp keymap)
    (signal 'emacs-keymap-not-keymap (list keymap)))
  (let* ((keys (emacs-keymap--key-seq->list key))
         (current keymap)
         (consumed 0)
         (total (length keys))
         binding)
    (catch 'done
      (while keys
        (let* ((k (car keys))
               (b (emacs-keymap--lookup-with-parent current k)))
          (when (and (null b) accept-default)
            (setq b (emacs-keymap--lookup-with-parent current t)))
          (cond
           ((null b)
            (setq binding nil)
            (throw 'done nil))
           ((and (cdr keys) (emacs-keymap-keymapp b))
            (setq current b consumed (1+ consumed) keys (cdr keys)))
           ((cdr keys)
            ;; non-keymap binding before the sequence ends:
            ;; trailing keys are unused — Emacs returns the count.
            (setq binding (- total consumed 1))
            (throw 'done nil))
           (t
            (setq binding b consumed (1+ consumed) keys nil))))))
    binding))

;; Parent-keymap storage convention for this module:
;; the parent is held as a cons (:emacs-keymap-parent . KEYMAP) at
;; the END of the tail.  This avoids fighting Emacs' "improper list"
;; parent encoding (which would break our `mapcar`-based
;; `emacs-keymap-copy-keymap' walk).
;;;###autoload
(defun emacs-keymap-keymap-parent (keymap)
  "Return KEYMAP's parent keymap, or nil (nelisp-emacs storage convention)."
  (unless (emacs-keymap-keymapp keymap)
    (signal 'emacs-keymap-not-keymap (list keymap)))
  (cl-loop for e in (cdr keymap)
           when (and (consp e)
                     (eq (car e) :emacs-keymap-parent)
                     (emacs-keymap-keymapp (cdr e)))
           return (cdr e)))

;;;###autoload
(defun emacs-keymap-set-keymap-parent (keymap parent)
  "Set KEYMAP's parent to PARENT (a keymap, or nil to remove).
Returns PARENT.  Detects direct cycles (= keymap == parent) and
signals `emacs-keymap-error'."
  (unless (emacs-keymap-keymapp keymap)
    (signal 'emacs-keymap-not-keymap (list keymap)))
  (when (and parent (not (emacs-keymap-keymapp parent)))
    (signal 'emacs-keymap-not-keymap (list parent)))
  (when (eq keymap parent)
    (signal 'emacs-keymap-error (list "Cannot set keymap as its own parent")))
  ;; Drop any existing :emacs-keymap-parent entry.
  (setcdr keymap
          (cl-remove-if
           (lambda (e)
             (and (consp e) (eq (car e) :emacs-keymap-parent)))
           (cdr keymap)))
  (when parent
    ;; Append at end so it does not interfere with sparse bindings.
    (setcdr keymap (append (cdr keymap)
                           (list (cons :emacs-keymap-parent parent)))))
  parent)

;;;###autoload
(defun emacs-keymap-keymap-prompt (keymap)
  "Return the prompt string of KEYMAP, or nil.
The prompt is the first standalone string element in KEYMAP's tail."
  (unless (emacs-keymap-keymapp keymap)
    (signal 'emacs-keymap-not-keymap (list keymap)))
  (cl-loop for e in (cdr keymap)
           when (stringp e) return e))

;;; C. global / local / overriding maps  +  Doc 41 chain provider

(defvar emacs-keymap-global-map (emacs-keymap-make-sparse-keymap)
  "The default global keymap (Doc 34 §4.3 slot 7).
Use `emacs-keymap-current-global-map' / `emacs-keymap-use-global-map'
to read / write.  Bindings here are the fallback for all buffers.")

(defvar-local emacs-keymap-local-map nil
  "Buffer-local keymap (Doc 34 §4.3 slot 6).
nil = no buffer-local map.  Set with `emacs-keymap-use-local-map'.")

(defvar emacs-keymap-overriding-local-map nil
  "Doc 34 §4.3 slot 2 — when non-nil, takes precedence over major/minor maps.")

(defvar emacs-keymap-overriding-terminal-local-map nil
  "Doc 34 §4.3 slot 1 — terminal-level override, highest priority.")

(defvar emacs-keymap-minor-mode-overriding-map-alist nil
  "Doc 34 §4.3 slot 3 — alist (VAR . MAP).
MAP is active when the symbol-value of VAR is non-nil.")

(defvar emacs-keymap-minor-mode-map-alist nil
  "Doc 34 §4.3 slot 4 — alist (VAR . MAP) of enabled minor-mode keymaps.")

(defvar emacs-keymap-emulation-mode-map-alists nil
  "Doc 34 §4.3 slot 5 — list of alists (each like `minor-mode-map-alist').")

;;;###autoload
(defun emacs-keymap-current-global-map ()
  "Return the current value of `emacs-keymap-global-map'."
  emacs-keymap-global-map)

;;;###autoload
(defun emacs-keymap-use-global-map (keymap)
  "Set the global keymap to KEYMAP.  Returns nil (matches Emacs)."
  (unless (emacs-keymap-keymapp keymap)
    (signal 'emacs-keymap-not-keymap (list keymap)))
  (setq emacs-keymap-global-map keymap)
  nil)

;;;###autoload
(defun emacs-keymap-current-local-map ()
  "Return the buffer-local keymap or nil."
  emacs-keymap-local-map)

;;;###autoload
(defun emacs-keymap-use-local-map (keymap)
  "Set the buffer-local keymap.  KEYMAP nil clears it.  Returns nil."
  (when (and keymap (not (emacs-keymap-keymapp keymap)))
    (signal 'emacs-keymap-not-keymap (list keymap)))
  (setq emacs-keymap-local-map keymap)
  nil)

(defun emacs-keymap--active-minor-mode-maps (alist)
  "Return list of keymaps in ALIST whose VAR symbol-value is non-nil."
  (cl-loop for (var . map) in alist
           when (and (boundp var) (symbol-value var)
                     (emacs-keymap-keymapp map))
           collect map))

;;;###autoload
(defun emacs-keymap-chain-at (&optional point)
  "Return the active keymap chain at POINT (priority-ordered).
Doc 34 §2.3 7 段 by default; Doc 41 §2.5 9 段 if
`emacs-keymap-chain-with-textprop' is non-nil and the corresponding
overlay / textprop providers are wired.  POINT defaults to point in
the current buffer; it is only consulted when the 9 段 flag is on."
  (let ((chain '()))
    ;; Build in order, then collect — Emacs lookups walk in the order
    ;; we return.
    (when emacs-keymap-overriding-terminal-local-map
      (push emacs-keymap-overriding-terminal-local-map chain))
    (when emacs-keymap-overriding-local-map
      (push emacs-keymap-overriding-local-map chain))
    (dolist (m (emacs-keymap--active-minor-mode-maps
                emacs-keymap-minor-mode-overriding-map-alist))
      (push m chain))
    (dolist (m (emacs-keymap--active-minor-mode-maps
                emacs-keymap-minor-mode-map-alist))
      (push m chain))
    (dolist (alist emacs-keymap-emulation-mode-map-alists)
      (dolist (m (emacs-keymap--active-minor-mode-maps alist))
        (push m chain)))
    ;; --- Doc 41 §2.5 opt-in slots 6 (overlay) & 7 (text-property) ---
    (when emacs-keymap-chain-with-textprop
      (let ((pt (or point
                    (and (fboundp 'point) (ignore-errors (point)))
                    1)))
        (when emacs-keymap-chain-overlay-provider
          (dolist (m (funcall emacs-keymap-chain-overlay-provider pt))
            (when (emacs-keymap-keymapp m)
              (push m chain))))
        (when emacs-keymap-chain-textprop-provider
          (dolist (m (funcall emacs-keymap-chain-textprop-provider pt))
            (when (emacs-keymap-keymapp m)
              (push m chain))))))
    (when emacs-keymap-local-map
      (push emacs-keymap-local-map chain))
    (push emacs-keymap-global-map chain)
    (nreverse chain)))

;;; D. lookup helpers

;;;###autoload
(defun emacs-keymap-key-binding (key &optional accept-default no-remap position)
  "Return the binding for KEY in the active keymap chain.
ACCEPT-DEFAULT — see `emacs-keymap-lookup-key'.
NO-REMAP — accepted for arity compatibility, currently no-op (= remap is
deferred per non-goal).  POSITION — the buffer position used for
text-property / overlay slot resolution when
`emacs-keymap-chain-with-textprop' is non-nil."
  (ignore no-remap)
  (let ((chain (emacs-keymap-chain-at position))
        (result nil))
    (catch 'found
      (dolist (km chain)
        (let ((b (emacs-keymap-lookup-key km key accept-default)))
          (when (and b (not (numberp b)))
            (setq result b)
            (throw 'found t)))))
    result))

;;;###autoload
(defun emacs-keymap-map-keymap (function keymap)
  "Call FUNCTION for every binding in KEYMAP.
FUNCTION is called with two args: KEY (= integer or symbol) and
BINDING.  Walks the parent chain too (= matches Emacs full-recursion
behaviour).  Returns nil."
  (unless (emacs-keymap-keymapp keymap)
    (signal 'emacs-keymap-not-keymap (list keymap)))
  (dolist (e (cdr keymap))
    (cond
     ((and (consp e) (eq (car e) t) (vectorp (cdr e)))
      ;; full-keymap slot
      (let ((vec (cdr e)))
        (dotimes (i (length vec))
          (let ((b (aref vec i)))
            (when b (funcall function i b))))))
     ((and (consp e) (eq (car e) :emacs-keymap-parent))
      ;; parent — descend
      (emacs-keymap-map-keymap function (cdr e)))
     ((and (consp e) (not (stringp e)))
      (funcall function (car e) (cdr e)))))
  nil)

(defvar emacs-keymap--where-is-results nil
  "Dynamic accumulator for `emacs-keymap-where-is-internal'.")

;;;###autoload
(defun emacs-keymap-where-is-internal (definition &optional keymap firstonly noindirect no-remap)
  "Return list of key sequences that invoke DEFINITION in KEYMAP.
KEYMAP defaults to the current chain (`emacs-keymap-chain-at').
FIRSTONLY non-nil = return only the first match (as a vector list of one).
NOINDIRECT, NO-REMAP — accepted for arity compatibility (= no-op,
indirect / remap features deferred per non-goal)."
  (ignore noindirect no-remap)
  (let ((maps (cond
               ((null keymap) (emacs-keymap-chain-at))
               ((listp keymap)
                ;; A keymap is itself a list, so distinguish.
                (if (eq (car-safe keymap) 'keymap)
                    (list keymap)
                  keymap))
               (t (list keymap))))
        (emacs-keymap--where-is-results '()))
    (catch 'first-found
      (dolist (km maps)
        (when (emacs-keymap-keymapp km)
          (emacs-keymap--walk-collect km definition '() firstonly))))
    (nreverse emacs-keymap--where-is-results)))

(defun emacs-keymap--walk-collect (km target prefix firstonly)
  "Walk KM accumulating into dynamic var `emacs-keymap--where-is-results'."
  (emacs-keymap-map-keymap
   (lambda (k binding)
     (let ((seq (append prefix (list k))))
       (cond
        ((eq binding target)
         (push (vconcat seq) emacs-keymap--where-is-results)
         (when firstonly
           (throw 'first-found (vconcat seq))))
        ((and (emacs-keymap-keymapp binding)
              ;; avoid infinite recursion on parent self-reference
              (not (member binding prefix)))
         (emacs-keymap--walk-collect binding target seq firstonly)))))
   km))

;;;###autoload
(defun emacs-keymap-substitute-key-definition (olddef newdef keymap &optional oldmap prefix)
  "Replace OLDDEF with NEWDEF for any keys in KEYMAP currently defined as OLDDEF.
OLDMAP and PREFIX accepted for arity compatibility.  When OLDMAP is
non-nil, scan OLDMAP instead of KEYMAP for the search but install in
KEYMAP — matches Emacs precedent."
  (ignore prefix)
  (let ((source (or oldmap keymap)))
    (unless (emacs-keymap-keymapp keymap)
      (signal 'emacs-keymap-not-keymap (list keymap)))
    (unless (emacs-keymap-keymapp source)
      (signal 'emacs-keymap-not-keymap (list source)))
    (emacs-keymap--substitute-walk source keymap olddef newdef '())))

(defun emacs-keymap--substitute-walk (source dest olddef newdef prefix)
  "Walk SOURCE, installing NEWDEF in DEST for every binding matching OLDDEF."
  (emacs-keymap-map-keymap
   (lambda (k binding)
     (let ((seq (append prefix (list k))))
       (cond
        ((eq binding olddef)
         (emacs-keymap-define-key dest (vconcat seq) newdef))
        ((and (emacs-keymap-keymapp binding)
              (not (member binding prefix)))
         (emacs-keymap--substitute-walk binding dest olddef newdef seq)))))
   source))

(defun emacs-keymap--describe-modifier (mods)
  "Return prefix string for MODS list.
Each MOD is one of: control, meta, shift, super, hyper, alt."
  (mapconcat
   (lambda (m)
     (pcase m
       ('control "C-")
       ('meta    "M-")
       ('shift   "S-")
       ('super   "s-")
       ('hyper   "H-")
       ('alt     "A-")
       (_ "")))
   mods ""))

(defun emacs-keymap--describe-element (e)
  "Return human-readable string for one key element E (char or symbol)."
  (cond
   ((integerp e)
    (let* ((mods '())
           (base e))
      (when (/= 0 (logand base ?\C-\^@))
        (push 'control mods)
        (setq base (logxor base ?\C-\^@)))
      (when (and (>= base ?A) (<= base ?Z) (memq 'control mods))
        ;; canonicalize C-A -> C-a
        (setq base (+ base 32)))
      (concat (emacs-keymap--describe-modifier (nreverse mods))
              (cond
               ((= base ?\s) "SPC")
               ((= base ?\t) "TAB")
               ((= base ?\n) "LFD")
               ((= base ?\r) "RET")
               ((= base ?\e) "ESC")
               ((= base 127) "DEL")
               ((and (>= base 0) (<= base 31))
                (format "C-%c" (+ base 96)))
               (t (string base))))))
   ((symbolp e) (symbol-name e))
   (t (format "%S" e))))

;;;###autoload
(defun emacs-keymap-key-description (keys &optional prefix)
  "Return a pretty string describing KEYS (string, vector, or list).
PREFIX, if non-nil, is prepended (also normalized)."
  (let* ((all (append (and prefix (emacs-keymap--key-seq->list prefix))
                      (emacs-keymap--key-seq->list keys)))
         (parts (mapcar #'emacs-keymap--describe-element all)))
    (mapconcat #'identity parts " ")))

;;; E. minimal command-loop scaffolding

(defvar emacs-keymap--this-command-keys (vector)
  "Vector of keys consumed in the current command invocation.")

(defvar emacs-keymap--read-event-fn nil
  "Function with no args returning the next key event (= integer/symbol).
nil = pull from `emacs-keymap--input-queue' (FIFO).
emacs-minibuffer / Phase 9b event-loop will install their own fn.")

(defvar emacs-keymap--input-queue nil
  "FIFO of pending key events used by the default reader.")

(defun emacs-keymap--default-read-event ()
  "Pop one event from `emacs-keymap--input-queue', or signal an error."
  (if emacs-keymap--input-queue
      (pop emacs-keymap--input-queue)
    (signal 'emacs-keymap-error (list "no event available"))))

;;;###autoload
(defun emacs-keymap-this-command-keys ()
  "Return the keys consumed by the most recent read-key-sequence call.
Returns a string when every element is a character, else a vector."
  (let ((v emacs-keymap--this-command-keys))
    (if (cl-every #'characterp (append v nil))
        (concat v)
      v)))

;;;###autoload
(defun emacs-keymap-this-command-keys-vector ()
  "Return the keys consumed by the most recent read-key-sequence call.
Always returns a vector."
  emacs-keymap--this-command-keys)

;;;###autoload
(defun emacs-keymap-read-key-sequence (prompt &optional _continue _dont-downcase _can-return-switch _cmd-loop)
  "Read a key sequence using the active keymap chain.
PROMPT is shown as the menu prompt (printed via `message' so ERT can
ignore it).  Subsequent args are arity-compat no-ops.

Returns the key sequence (vector) once a non-prefix binding is reached
or no further keys can be consumed.  This is the Phase 1 / ERT-friendly
reader; Phase 9b event-loop will replace `emacs-keymap--read-event-fn'
to plug into a real event source."
  (when prompt (message "%s" prompt))
  (let ((reader (or emacs-keymap--read-event-fn
                    #'emacs-keymap--default-read-event))
        (consumed (vector))
        (done nil))
    (while (not done)
      (let* ((ev (funcall reader))
             (next (vconcat consumed (vector ev)))
             (b (emacs-keymap-key-binding next)))
        (setq consumed next)
        (cond
         ((null b) (setq done t))
         ((emacs-keymap-keymapp b) ) ; prefix — keep reading
         (t (setq done t)))))
    (setq emacs-keymap--this-command-keys consumed)
    consumed))

(provide 'emacs-keymap)
;;; emacs-keymap.el ends here
