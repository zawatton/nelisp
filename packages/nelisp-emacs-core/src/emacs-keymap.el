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
;;      define-key / define-key-after / lookup-key / keymap-parent /
;;      set-keymap-parent / keymap-prompt
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
;;      that routes through `emacs-keymap--read-event-fn' so that
;;      Phase 1 emacs-minibuffer / Phase 9b event-loop can plug their
;;      own event source.  Default is a buffered FIFO suitable for
;;      ERT.)
;;
;;   G. newer kbd-style API  (9 APIs, Phase 1 §4.4)
;;      keymap-set / keymap-lookup / keymap-unset
;;      keymap-global-set / keymap-local-set
;;      keymap-global-unset / keymap-local-unset
;;      key-parse (= delegate) / key-valid-p (= delegate)
;;      Vendor-first: kbd-syntax parsing is delegated to upstream
;;      `vendor/emacs-lisp/keymap.el' (= `key-parse' / `key-valid-p').
;;      The actual binding state lives in our own `emacs-keymap-*'
;;      structures via `emacs-keymap-define-key' / -lookup-key.
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
(require 'emacs-char-table)

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

Internally this matches the real Emacs full-keymap shape
=(keymap CHAR-TABLE . SPARSE-ALIST)=: the second element is a real
char-table (so vendor code such as isearch.el's
=(char-table-p (nth 1 map))= assertion passes), and character bindings
for chars below 256 are stored in its ASCII slots.  Lookup helpers also
accept the legacy =(t . SLOT-VECTOR)= cons for backward compatibility.
Full traversal via `emacs-keymap-map-keymap' yields each non-nil slot as
a (CHAR . BINDING) pair followed by any sparse entries added later."
  (let ((ct (emacs-char-table-make 'keymap)))
    (if prompt
        (list 'keymap ct prompt)
      (list 'keymap ct))))

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
            ((emacs-char-table-p e)
             ;; Full-keymap char-table slot — deep-copy, recursing on
             ;; nested keymap bindings.
             (emacs-char-table-copy
              e (lambda (b) (if (emacs-keymap-keymapp b)
                                (emacs-keymap-copy-keymap b)
                              b))))
            ((and (consp e) (eq (car e) t) (vectorp (cdr e)))
             ;; Legacy full-keymap slot vector — copy element-wise,
             ;; recurse on nested keymap bindings.
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

(defconst emacs-keymap--full-slot-size 256
  "Number of ASCII character codes a full keymap addresses directly.")

(defun emacs-keymap--slot-p (e)
  "Return non-nil when E is a full-keymap slot (char-table or legacy cons)."
  (or (emacs-char-table-p e)
      (and (consp e) (eq (car e) t) (vectorp (cdr e)))))

(defun emacs-keymap--full-slot (keymap)
  "Return KEYMAP's full slot, or nil if KEYMAP is sparse.
The slot is a char-table (current model) or a legacy =(t . VECTOR)=
cons; use `emacs-keymap--slot-ref' / `emacs-keymap--slot-set' to access
it regardless of representation."
  (cl-loop for e in (cdr keymap)
           when (emacs-keymap--slot-p e)
           return e))

;;;###autoload
(defun emacs-keymap-full-slot (keymap)
  "Return KEYMAP's full slot, or nil if KEYMAP is sparse.
This public adapter exists for runner/front-end code that needs the
full-keymap fast path without depending on private helper names."
  (emacs-keymap--full-slot keymap))

;;;###autoload
(defun emacs-keymap-direct-slot-vector (keymap)
  "Return KEYMAP's direct low-character binding vector, or nil.
This is the stable frontend/runner adapter for the full-keymap fast path.
It accepts both the current char-table slot representation and the older
legacy =(t . VECTOR)= shape."
  (let ((slot (emacs-keymap-full-slot keymap)))
    (cond
     ((emacs-char-table-p slot)
      (emacs-char-table-ascii-vector slot))
     ((and (consp slot) (vectorp (cdr slot)))
      (cdr slot))
     (t nil))))

;;;###autoload
(defun emacs-keymap-define-key-fast (keymap key def &optional slot-vector)
  "Bind KEY to DEF in KEYMAP, using SLOT-VECTOR when possible.
SLOT-VECTOR, when non-nil, is the result of
`emacs-keymap-direct-slot-vector'.  This keeps concrete event loops from
duplicating full-keymap mutation details."
  (cond
   ((and slot-vector
         (vectorp key)
         (= (length key) 1)
         (integerp (aref key 0))
         (>= (aref key 0) 0)
         (< (aref key 0) (length slot-vector)))
    (aset slot-vector (aref key 0) def)
    def)
   ((fboundp 'define-key)
    (define-key keymap key def))
   ((fboundp 'emacs-keymap-define-key)
    (emacs-keymap-define-key keymap key def))
   (t nil)))

;;;###autoload
(defun emacs-keymap-make-compatible-full-keymap ()
  "Return a full keymap using the best available runtime constructor.
Host Emacs prefers `make-keymap'; standalone images can use the
`emacs-keymap-make-keymap' substrate; sparse keymaps are a last fallback."
  (cond
   ((and (boundp 'emacs-version) (fboundp 'make-keymap))
    (make-keymap))
   ((fboundp 'emacs-keymap-make-keymap)
    (emacs-keymap-make-keymap))
   ((fboundp 'make-keymap)
    (make-keymap))
   ((fboundp 'make-sparse-keymap)
    (make-sparse-keymap))
   (t (list 'keymap))))

;;;###autoload
(defun emacs-keymap-build-single-key-cache
    (keymap &optional lookup-function)
  "Return a 256-slot direct lookup cache for KEYMAP.
LOOKUP-FUNCTION, when non-nil, is called as (LOOKUP-FUNCTION KEYMAP
KEY-VECTOR) for fallback lookup.  Otherwise `lookup-key' is used when
available."
  (let ((cache (make-vector 256 nil))
        (vec (emacs-keymap-direct-slot-vector keymap))
        (lookup (or lookup-function
                    (and (fboundp 'lookup-key) #'lookup-key)))
        (c 0))
    (while (< c 256)
      (aset cache c
            (if (and vec (< c (length vec)))
                (aref vec c)
              (and lookup (funcall lookup keymap (vector c)))))
      (setq c (1+ c)))
    cache))

;;;###autoload
(defun emacs-keymap-install-overriding-terminal-map
    (keymap &optional parent)
  "Install KEYMAP as `overriding-terminal-local-map' when available.
PARENT, when non-nil, is installed as KEYMAP's parent first.  Return
KEYMAP when installed, otherwise nil."
  (when (and (or (not (boundp 'noninteractive)) (not noninteractive))
             (boundp 'overriding-terminal-local-map))
    (when (and parent (fboundp 'set-keymap-parent))
      (set-keymap-parent keymap parent))
    (set 'overriding-terminal-local-map keymap)
    keymap))

;;;###autoload
(defun emacs-keymap-clear-overriding-terminal-map ()
  "Clear `overriding-terminal-local-map' when that variable exists."
  (when (boundp 'overriding-terminal-local-map)
    (set 'overriding-terminal-local-map nil)))

(defun emacs-keymap--slot-char-p (k)
  "Return non-nil when K is a character handled by a full slot's fast path."
  (and (integerp k) (>= k 0) (< k emacs-keymap--full-slot-size)))

(defun emacs-keymap--slot-ref (slot k)
  "Return SLOT's binding for character K (char-table or legacy cons)."
  (if (emacs-char-table-p slot)
      (emacs-char-table-ref slot k)
    (aref (cdr slot) k)))

(defun emacs-keymap--slot-set (slot k def)
  "Set SLOT's binding for character K to DEF (char-table or legacy cons)."
  (if (emacs-char-table-p slot)
      (emacs-char-table-set slot k def)
    (aset (cdr slot) k def)))

(defun emacs-keymap--key-equal-p (a b)
  "Return non-nil when key events A and B should be treated as equal."
  (if (or (integerp a) (symbolp a))
      (eq a b)
    (equal a b)))

(defun emacs-keymap--tail-state (keymap k)
  "Return (SLOT . BINDING-CELL) for KEYMAP tail and event K.
SLOT is the full-keymap slot object when present.  BINDING-CELL is the
sparse binding cons cell whose car matches K, or nil."
  (let ((tail (cdr keymap))
        slot
        binding)
    (while (and tail (or (not slot) (not binding)))
      (let ((entry (car tail)))
        (cond
         ((and (not slot) (emacs-keymap--slot-p entry))
          (setq slot entry))
         ((and (not binding)
               (consp entry)
               (not (eq (car entry) t))
               (emacs-keymap--key-equal-p (car entry) k))
          (setq binding entry))))
      (setq tail (cdr tail)))
    (cons slot binding)))

(defun emacs-keymap--set-binding (keymap k def)
  "Install K -> DEF in KEYMAP, replacing any existing binding."
  (let ((state (emacs-keymap--tail-state keymap k)))
    (let ((slot (car state))
          (existing (cdr state)))
    (if (and slot (emacs-keymap--slot-char-p k))
        ;; full-keymap slot path
        (emacs-keymap--slot-set slot k def)
      (cond
       (existing
        (if def
            (setcdr existing def)
          ;; nil = remove
          (setcdr keymap (delq existing (cdr keymap)))))
       (def
        ;; Insert AFTER the full slot (char-table or legacy cons) if any,
        ;; so the slot stays at `(nth 1 keymap)' and the prompt string
        ;; stays at the very tail.
        (if slot
            (let ((slot-cell (memq slot (cdr keymap))))
              (if slot-cell
                  (setcdr slot-cell (cons (cons k def) (cdr slot-cell)))
                (setcdr keymap (cons (cons k def) (cdr keymap)))))
          (setcdr keymap (cons (cons k def) (cdr keymap))))))))))

(defun emacs-keymap--binding-entry-p (entry)
  "Return non-nil when ENTRY is a sparse key binding cell."
  (and (consp entry)
       (not (eq (car entry) t))
       (not (eq (car entry) :emacs-keymap-parent))))

(defun emacs-keymap--define-key-after-in-sparse (keymap k def after)
  "Install K -> DEF in sparse KEYMAP after event AFTER.
AFTER nil or not found means append after the last sparse binding.
Metadata entries such as the full slot, prompt string, and parent slot
keep their relative order outside the sparse binding block."
  (let ((prefix '())
        (bindings '())
        (suffix '()))
    (dolist (entry (cdr keymap))
      (cond
       ((emacs-keymap--slot-p entry)
        (push entry prefix))
       ((emacs-keymap--binding-entry-p entry)
        (unless (equal (car entry) k)
          (push entry bindings)))
       (t
        (push entry suffix))))
    (setq prefix (nreverse prefix)
          bindings (nreverse bindings)
          suffix (nreverse suffix))
    (when def
      (let ((cell (cons k def))
            (inserted nil)
            (result '()))
        (dolist (entry bindings)
          (push entry result)
          (when (and (not inserted) after (equal (car entry) after))
            (push cell result)
            (setq inserted t)))
        (unless inserted
          (push cell result))
        (setq bindings (nreverse result))))
    (setcdr keymap (append prefix bindings suffix))))

(defun emacs-keymap--define-key-after-1 (keymap keys def after)
  "Recursive helper for `emacs-keymap-define-key-after'."
  (let ((k (car keys))
        (rest (cdr keys)))
    (if rest
        (let ((sub (emacs-keymap--get-binding keymap k)))
          (unless (emacs-keymap-keymapp sub)
            (setq sub (emacs-keymap-make-sparse-keymap))
            (emacs-keymap--set-binding keymap k sub))
          (emacs-keymap--define-key-after-1 sub rest def after))
      (let ((slot (emacs-keymap--full-slot keymap)))
        (if (and slot (emacs-keymap--slot-char-p k))
            (emacs-keymap--slot-set slot k def)
          (emacs-keymap--define-key-after-in-sparse keymap k def after))))))

;;;###autoload
(defun emacs-keymap-define-key-after (keymap key def &optional after)
  "In KEYMAP, define key sequence KEY as DEF after event AFTER.
For sparse keymaps, the terminal KEY event is placed after the binding
whose event equals AFTER.  When AFTER is nil or absent, the binding is
placed after the current sparse bindings.  Multi-event KEY sequences
create prefix keymaps the same way `emacs-keymap-define-key' does.

Returns DEF."
  (unless (emacs-keymap-keymapp keymap)
    (signal 'emacs-keymap-not-keymap (list keymap)))
  (let ((keys (emacs-keymap--key-seq->list key)))
    (when (null keys)
      (signal 'emacs-keymap-bad-key (list key)))
    (emacs-keymap--define-key-after-1 keymap keys def after)
    def))

(defun emacs-keymap--get-binding (keymap k)
  "Return KEYMAP's binding for K, or nil.
Walks the keymap *without* parent inheritance — that is the caller's
job (see `emacs-keymap--lookup-with-parent')."
  (let* ((state (emacs-keymap--tail-state keymap k))
         (slot (car state))
         (binding (cdr state)))
    (or (and slot (emacs-keymap--slot-char-p k)
             (emacs-keymap--slot-ref slot k))
        (and binding (cdr binding)))))

(defun emacs-keymap--lookup-with-parent (keymap k)
  "Return KEYMAP's binding for K, walking parent inheritance.
Returns nil if not found in this keymap or any ancestor."
  (or (emacs-keymap--get-binding keymap k)
      (let ((p (emacs-keymap-keymap-parent keymap)))
        (and p (emacs-keymap--lookup-with-parent p k)))))

;;;###autoload
(defun emacs-keymap-lookup-with-parent (keymap k)
  "Return KEYMAP's binding for K, walking parent inheritance."
  (emacs-keymap--lookup-with-parent keymap k))

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
     ((emacs-char-table-p e)
      ;; full-keymap char-table slot — visit the ASCII range only, to
      ;; mirror the legacy traversal (supra-ASCII range defaults such as
      ;; isearch's catch-all are not surfaced as bindings).
      (let ((vec (emacs-char-table-ascii-vector e)))
        (dotimes (i (length vec))
          (let ((b (aref vec i)))
            (when b (funcall function i b))))))
     ((and (consp e) (eq (car e) t) (vectorp (cdr e)))
      ;; legacy full-keymap slot
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

;;; G. newer kbd-style API  (9 APIs, Phase 1 §4.4)
;;
;; Host Emacs delegates parsing of "C-x C-f" and similar kbd-style
;; strings to upstream `keymap.el'.  Standalone NeLisp uses the small
;; fallback below instead: the upstream file is regexp-heavy and stalls
;; during bootstrap before the full regex/keymap stack is available.

(when (boundp 'emacs-version)
  (require 'keymap))

(defun emacs-keymap--standalone-key-token (token)
  "Parse one kbd-style TOKEN for the standalone NeLisp fallback."
  (let ((bits 0)
        (ctrl nil)
        (done nil))
    (while (not done)
      (cond
       ((and (>= (length token) 2)
             (equal (substring token 0 2) "A-"))
        (setq bits (+ bits ?\A-\0)
              token (substring token 2)))
       ((and (>= (length token) 2)
             (equal (substring token 0 2) "C-"))
        (setq bits (+ bits ?\C-\0)
              ctrl t
              token (substring token 2)))
       ((and (>= (length token) 2)
             (equal (substring token 0 2) "H-"))
        (setq bits (+ bits ?\H-\0)
              token (substring token 2)))
       ((and (>= (length token) 2)
             (equal (substring token 0 2) "M-"))
        (setq bits (+ bits ?\M-\0)
              token (substring token 2)))
       ((and (>= (length token) 2)
             (equal (substring token 0 2) "S-"))
        (setq bits (+ bits ?\S-\0)
              token (substring token 2)))
       ((and (>= (length token) 2)
             (equal (substring token 0 2) "s-"))
        (setq bits (+ bits ?\s-\0)
              token (substring token 2)))
       (t (setq done t))))
    (cond
     ((and (> (length token) 2)
           (equal (substring token 0 1) "<")
           (equal (substring token (- (length token) 1)) ">"))
      (let ((name (substring token 1 (- (length token) 1))))
        (intern (if (= bits 0)
                    name
                  (concat (cond
                           ((= bits ?\S-\0) "S-")
                           ((= bits ?\C-\0) "C-")
                           ((= bits ?\M-\0) "M-")
                           (t ""))
                          name)))))
     ((equal token "NUL") (+ bits 0))
     ((equal token "RET") (+ bits ?\r))
     ((equal token "LFD") (+ bits ?\n))
     ((equal token "TAB") (+ bits ?\t))
     ((equal token "ESC") (+ bits ?\e))
     ((equal token "SPC") (+ bits ?\s))
     ((equal token "DEL") (+ bits 127))
     ((= (length token) 1)
      (let ((ch (aref token 0)))
        (if ctrl
            (cond
             ((and (>= ch ?a) (<= ch ?z))
              (+ (- bits ?\C-\0) (- ch ?a -1)))
             ((and (>= ch ?@) (<= ch ?_))
              (+ (- bits ?\C-\0) (- ch ?@)))
             (t (+ bits ch)))
          (+ bits ch))))
     (t
      (signal 'emacs-keymap-bad-key (list token))))))

(defun emacs-keymap--standalone-key-parse (keys)
  "Parse common kbd-style KEYS without relying on regexp features."
  (unless (and (stringp keys) (> (length keys) 0))
    (signal 'emacs-keymap-bad-key (list keys)))
  (let ((tokens (split-string keys " "))
        events)
    (dolist (token tokens)
      (when (= (length token) 0)
        (signal 'emacs-keymap-bad-key (list keys)))
      (push (emacs-keymap--standalone-key-token token) events))
    (vconcat (nreverse events))))

(defun emacs-keymap--standalone-key-valid-p (keys)
  "Return non-nil when the standalone fallback can parse KEYS."
  (condition-case nil
      (progn (emacs-keymap--standalone-key-parse keys) t)
    (error nil)))

(when (or (fboundp 'nl-write-file)
          (not (boundp 'emacs-version))
          (get 'key-parse 'emacs-stub-bulk)
          (get 'defvar-keymap 'emacs-stub-bulk))
  ;; GNU keymap.el's regexp-heavy parser is too much for the current
  ;; standalone NeLisp regex subset.  Route the common API through the
  ;; shared keymap substrate with a small kbd parser.
  (defalias 'key-parse #'emacs-keymap--standalone-key-parse)
  (defalias 'key-valid-p #'emacs-keymap--standalone-key-valid-p)
  (defalias 'kbd #'emacs-keymap--standalone-key-parse)
  (defun keymap-set (keymap key definition)
    "Standalone NeLisp fallback for GNU `keymap-set'."
    (let ((parsed (emacs-keymap--standalone-key-parse key))
          (def (if (stringp definition)
                   (emacs-keymap--standalone-key-parse definition)
                 definition)))
      (emacs-keymap-define-key keymap parsed def)
      def))
  (defun keymap-global-set (key command &optional interactive)
    "Standalone NeLisp fallback for GNU `keymap-global-set'."
    (ignore interactive)
    (keymap-set (emacs-keymap-current-global-map) key command))
  (defun keymap-local-set (key command &optional interactive)
    "Standalone NeLisp fallback for GNU `keymap-local-set'."
    (ignore interactive)
    (let ((map (or (emacs-keymap-current-local-map)
                   (emacs-keymap-make-sparse-keymap))))
      (emacs-keymap-use-local-map map)
      (keymap-set map key command)))
  (defun keymap-lookup (keymap key &optional accept-default no-remap position)
    "Standalone NeLisp fallback for GNU `keymap-lookup'."
    (ignore no-remap position)
    (emacs-keymap-lookup-key keymap
                             (emacs-keymap--standalone-key-parse key)
                             accept-default))
  (defun keymap-unset (keymap key &optional remove)
    "Standalone NeLisp fallback for GNU `keymap-unset'."
    (ignore remove)
    (emacs-keymap-define-key keymap
                             (emacs-keymap--standalone-key-parse key)
                             nil))
  (defun keymap-global-unset (key &optional remove)
    "Standalone NeLisp fallback for GNU `keymap-global-unset'."
    (keymap-unset (emacs-keymap-current-global-map) key remove))
  (defun keymap-local-unset (key &optional remove)
    "Standalone NeLisp fallback for GNU `keymap-local-unset'."
    (let ((map (emacs-keymap-current-local-map)))
      (when map
        (keymap-unset map key remove))))
  (defun define-keymap (&rest definitions)
    "Standalone NeLisp fallback for GNU `define-keymap'."
    (let (full suppress parent name keymap prefix)
      (while (and definitions
                  (keywordp (car definitions))
                  (not (eq (car definitions) :menu)))
        (let ((keyword (pop definitions)))
          (unless definitions
            (error "Missing keyword value for %s" keyword))
          (let ((value (pop definitions)))
            (cond
             ((eq keyword :full) (setq full value))
             ((eq keyword :keymap) (setq keymap value))
             ((eq keyword :parent) (setq parent value))
             ((eq keyword :suppress) (setq suppress value))
             ((eq keyword :name) (setq name value))
             ((eq keyword :prefix)
              (setq prefix value))
             (t (error "Invalid keyword: %s" keyword))))))
      (let ((map (or keymap
                     ;; :prefix -> a sparse keymap stored as the symbol's
                     ;; function (a prefix command) and value cell, matching
                     ;; GNU `define-prefix-command'.
                     (and prefix
                          (let ((m (emacs-keymap-make-sparse-keymap name)))
                            (fset prefix m)
                            (set prefix m)
                            m))
                     (if full
                         (emacs-keymap-make-keymap name)
                       (emacs-keymap-make-sparse-keymap name)))))
        (when suppress
          (suppress-keymap map (eq suppress 'nodigits)))
        (when parent
          (emacs-keymap-set-keymap-parent map parent))
        (while definitions
          (let ((key (pop definitions)))
            (unless definitions
              (error "Uneven number of key/definition pairs"))
            (let ((def (pop definitions)))
              (unless (eq key :menu)
                (keymap-set map key def)))))
        map)))
  (defmacro defvar-keymap (variable-name &rest defs)
    "Standalone NeLisp fallback for GNU `defvar-keymap'."
    (let ((opts nil)
          doc
          parent
          suppress
          keymap
          repeat)
      (while (and defs
                  (keywordp (car defs))
                  (not (eq (car defs) :menu)))
        (let ((keyword (pop defs)))
          (unless defs
            (error "Uneven number of keywords"))
          (cond
           ((eq keyword :doc) (setq doc (pop defs)))
           ((eq keyword :repeat) (setq repeat (pop defs)))
           ((eq keyword :parent) (setq parent (pop defs)))
           ((eq keyword :suppress) (setq suppress (pop defs)))
           ((eq keyword :keymap) (setq keymap (pop defs)))
           (t
            (push keyword opts)
            (push (pop defs) opts)))))
      (ignore repeat)
      (ignore opts)
      (unless (zerop (% (length defs) 2))
        (error "Uneven number of key/definition pairs: %S" defs))
      ;; Standalone load-time materialization must not be blocked by
      ;; complex GNU key syntax such as "<remap> <foo>".  The real
      ;; binding parser remains available through `keymap-set'; this
      ;; fallback only guarantees that mode maps exist as keymaps.
      ;;
      ;; Build the expansion without backquote.  The standalone prelude
      ;; installs its own backquote/macroexpander before this file is
      ;; loaded in replay diagnostics, so avoiding backquote here keeps
      ;; the macro independent of that implementation.
      (cons 'progn
            (cons (append (list 'defvar
                                variable-name
                                (or keymap
                                    (list 'list (list 'quote 'keymap))))
                          (and doc (list doc)))
                  (cons (list 'quote variable-name) nil))))))

;;;###autoload
(defun emacs-keymap-key-parse (keys)
  "Convert KEYS, a kbd-style string, to an internal key vector.
Thin delegate to upstream `key-parse' under host Emacs.  Standalone
NeLisp calls the local `emacs-keymap--standalone-key-parse' fallback
directly instead of going through the unprefixed `key-parse' symbol,
because `emacs-keymap-builtins.el' may (re)install `key-parse' as an
alias to this very function; delegating through that symbol would
make the call recurse into itself forever."
  (if (or (fboundp 'nl-write-file)
          (fboundp 'nelisp--write-stdout-bytes)
          (not (boundp 'emacs-version)))
      (emacs-keymap--standalone-key-parse keys)
    (key-parse keys)))

;;;###autoload
(defun emacs-keymap-key-valid-p (keys)
  "Return non-nil iff KEYS is a valid kbd-style key description.
Thin delegate to upstream `key-valid-p' under host Emacs.  See
`emacs-keymap-key-parse' for why standalone NeLisp calls the local
`emacs-keymap--standalone-key-valid-p' fallback directly instead of
delegating through the unprefixed `key-valid-p' symbol."
  (if (or (fboundp 'nl-write-file)
          (fboundp 'nelisp--write-stdout-bytes)
          (not (boundp 'emacs-version)))
      (emacs-keymap--standalone-key-valid-p keys)
    (key-valid-p keys)))

;;;###autoload
(defun emacs-keymap-keymap-set (keymap key def)
  "Bind KEY (a kbd-style string) to DEF in KEYMAP.
Signals if KEY is not a valid kbd-style description.  When DEF is a
string it is treated as a key sequence and parsed via `key-parse'."
  (unless (key-valid-p key)
    (signal 'emacs-keymap-bad-key (list key)))
  (let ((d (if (stringp def)
               (progn (unless (key-valid-p def)
                        (signal 'emacs-keymap-bad-key (list def)))
                      (key-parse def))
             def)))
    (emacs-keymap-define-key keymap (key-parse key) d)
    d))

;;;###autoload
(defun emacs-keymap-keymap-lookup (keymap key &optional accept-default
                                          _no-remap _position)
  "Look up KEY (a kbd-style string) in KEYMAP.
ACCEPT-DEFAULT is forwarded to `emacs-keymap-lookup-key'.  _NO-REMAP
and _POSITION are accepted for ABI compatibility but ignored in
Phase 1 (= remap / position-aware lookup is Phase 9b scope)."
  (unless (key-valid-p key)
    (signal 'emacs-keymap-bad-key (list key)))
  (emacs-keymap-lookup-key keymap (key-parse key) accept-default))

;;;###autoload
(defun emacs-keymap-keymap-unset (keymap key &optional _remove)
  "Remove the binding for KEY (a kbd-style string) in KEYMAP.
_REMOVE is accepted for ABI compatibility but ignored — Phase 1
always sets the binding to nil rather than splicing the entry out
of the alist."
  (unless (key-valid-p key)
    (signal 'emacs-keymap-bad-key (list key)))
  (emacs-keymap-define-key keymap (key-parse key) nil))

;;;###autoload
(defun emacs-keymap-keymap-global-set (key command)
  "Bind KEY (kbd-style) to COMMAND in the current global map."
  (emacs-keymap-keymap-set (emacs-keymap-current-global-map) key command))

;;;###autoload
(defun emacs-keymap-keymap-local-set (key command)
  "Bind KEY (kbd-style) to COMMAND in the current local map.
Allocates a fresh sparse keymap and installs it via
`emacs-keymap-use-local-map' if no local map is in effect yet."
  (let ((local (emacs-keymap-current-local-map)))
    (unless local
      (setq local (emacs-keymap-make-sparse-keymap))
      (emacs-keymap-use-local-map local))
    (emacs-keymap-keymap-set local key command)))

;;;###autoload
(defun emacs-keymap-keymap-global-unset (key &optional remove)
  "Remove KEY's binding from the current global map.
REMOVE is forwarded to `emacs-keymap-keymap-unset'."
  (emacs-keymap-keymap-unset (emacs-keymap-current-global-map) key remove))

;;;###autoload
(defun emacs-keymap-keymap-local-unset (key &optional remove)
  "Remove KEY's binding from the current local map.
A no-op when no local map is in effect.  REMOVE is forwarded to
`emacs-keymap-keymap-unset'."
  (let ((local (emacs-keymap-current-local-map)))
    (when local
      (emacs-keymap-keymap-unset local key remove))))

(provide 'emacs-keymap)
;;; emacs-keymap.el ends here
