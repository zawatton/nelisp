;;; emacs-parity-evil.el --- cl-destructuring-bind dotted-tail override for evil -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Real-init audit parity fix (b1k21 frontier).  The frozen `combined.repl'
;; baked into the audit, together with the NeLisp runtime prelude, carries a
;; NAIVE copy of `cl-destructuring-bind' (from
;; `vendor/nelisp/scripts/nelisp-stdlib-prelude.el') whose expander walks the
;; arglist with `(while args (car args) ... (setq args (cdr args)))'.  That
;; loop assumes a PROPER list arglist; on a DOTTED / improper pattern such as
;;
;;   (cl-destructuring-bind (form . attrs) VALUE BODY...)      ; evil-common.el:316
;;
;; the tail `attrs' is a bare symbol, so the second iteration evaluates
;; `(car attrs)' -- car of a non-list -- and the NeLisp source evaluator
;; `nelisp-bare-abort's out of the macroexpansion.
;;
;; `evil-define-command' (evil-common.el:316) uses exactly that dotted pattern
;; to split the interactive form from its extra command properties:
;;
;;   (cl-destructuring-bind (form . attrs)
;;       (apply #'evil-interactive-form (cdr (pop body)))
;;     (setq interactive `(interactive ,form)
;;           keys (evil-concat-plists keys attrs)))
;;
;; Every command-defining evil macro routes through `evil-define-command' and
;; always emits an `interactive' form, so ALL of them hit this path and abort:
;;
;;   evil-define-motion   -> evil-define-command   (evil-macros.el:158)
;;   evil-define-operator -> evil-define-command   (evil-macros.el:504)
;;   evil-define-text-object -> evil-define-motion -> evil-define-command
;;   evil-define-avy-motion  -> evil-define-motion -> evil-define-command
;;   evil-define-visual-selection -> evil-define-command (evil-states.el:241)
;;
;; That is why the first real-init audit reports evil as the single largest
;; `nelisp-bare-abort' family (evil-define-motion 146 + evil-define-command 92
;; + evil-define-operator 68 + evil-define-text-object 56 + evil-define-avy-motion
;; 19 + evil-define-visual-selection 4 = 385, plus the direct dotted-pattern
;; users `(chars . digraph)' / `(code expr . plist)' inside evil-commands.el /
;; evil-types.el).  Evil's core mode enables (state vars/keymaps load fine) but
;; every command definition macro-aborts -> "partial evil".
;;
;; ROOT MECHANISM (identical to the cl-loop fix in emacs-parity-clloop.el):
;; `src/emacs-cl-macros.el' DOES carry a correct `cl-destructuring-bind' (line
;; 2253) that treats an improper tail as `&rest' via
;; `emacs-cl-macros--split-arglist'.  But that definition is guarded by
;;
;;   (when (or (not (boundp 'emacs-version))
;;             (emacs-cl-macros--define-p 'cl-destructuring-bind)) ...)
;;
;; and `emacs-cl-macros--define-p' returns nil for a real (non-autoload,
;; non-placeholder) prelude macro.  The prelude already bound
;; `cl-destructuring-bind' as such a real macro, so the correct definition is
;; SKIPPED and the buggy prelude version wins.
;;
;; FIX: unconditionally redefine `cl-destructuring-bind' with the correct,
;; dotted-tail-aware expander (copied verbatim from `src/emacs-cl-macros.el'
;; with the guard stripped), so it overrides the frozen prelude copy.  This is
;; NOT a no-op: it emits real `let*' bindings -- positionals as `(nth I V)',
;; the improper/`&rest' tail as `(nthcdr I V)', `&optional' with defaults and
;; `&key' scanning -- so `evil-define-command' expands and every evil command
;; is actually defined.  The helper `emacs-cl-macros--split-arglist' /
;; `emacs-cl-macros--key-bindings' / `emacs-cl-macros--loop-destructure-bindings'
;; are copied here too (the first two verbatim, the last guarded) so the shim
;; is self-contained regardless of load order.

;;; Code:

;;;; --- arglist helpers (correct, verbatim) ----------------------------

(defun emacs-cl-macros--split-arglist (arglist)
  "Split ARGLIST into (POSITIONAL OPTIONALS RESTSYM KEYS).
KEYS = list of (KEYWORD-NAME PARAM-SYM DEFAULT-FORM) triples."
  (let ((positional nil)
        (optionals nil)
        (restsym nil)
        (keys nil)
        (mode 'positional)
        (cur arglist))
    (while (consp cur)
      (let ((tok (car cur)))
        (cond
         ((eq tok '&optional) (setq mode 'optional))
         ((memq tok '(&rest &body)) (setq mode 'rest))
         ((eq tok '&key)      (setq mode 'key))
         ((eq tok '&aux)      (setq mode 'aux))
         (t
          (cond
           ((eq mode 'positional) (setq positional (cons tok positional)))
           ((eq mode 'optional)
            (setq optionals (cons tok optionals)))
           ((eq mode 'rest)
            (setq restsym tok))
           ((eq mode 'key)
            (let* ((sym (if (consp tok) (car tok) tok))
                   (default (if (consp tok) (car (cdr tok)) nil))
                   (kwname (intern
                            (concat ":"
                                    (symbol-name sym)))))
              (setq keys (cons (list kwname sym default) keys))))
           ;; &aux: drop (= local lets, rarely critical for stubs)
           ((eq mode 'aux) nil)))))
      (setq cur (cdr cur)))
    ;; An improper lambda-list tail is the rest pattern.  In particular,
    ;; `(form . attrs)' is equivalent to `(form &rest attrs)'.
    (when cur
      (setq restsym cur))
    (let ((rev-positional nil) (rev-optionals nil) (rev-keys nil)
          (p positional) (o optionals) (k keys))
      (while p (setq rev-positional (cons (car p) rev-positional)) (setq p (cdr p)))
      (while o (setq rev-optionals (cons (car o) rev-optionals)) (setq o (cdr o)))
      (while k (setq rev-keys (cons (car k) rev-keys)) (setq k (cdr k)))
      (list rev-positional rev-optionals restsym rev-keys))))

(defun emacs-cl-macros--key-bindings (keys restsym)
  "Build let-bindings for KEYS by scanning RESTSYM (= the &rest var).
Each binding is (PARAM (or (cadr (memq KW RESTSYM)) DEFAULT))."
  (let ((out nil)
        (cur keys))
    (while cur
      (let* ((entry (car cur))
             (kw (car entry))
             (sym (car (cdr entry)))
             (def (car (cdr (cdr entry)))))
        (setq out (cons (list sym
                              (list 'or
                                    (list 'car
                                          (list 'cdr
                                                (list 'memq (list 'quote kw) restsym)))
                                    def))
                        out)))
      (setq cur (cdr cur)))
    (let ((rev nil) (c out))
      (while c (setq rev (cons (car c) rev)) (setq c (cdr c)))
      rev)))

;; Nested-list destructurer.  Only exercised for nested patterns (evil's
;; failing patterns are flat-with-dotted-tail and never reach it), but
;; `cl-destructuring-bind' delegates to it for correctness with other
;; consumers.  Defined guarded so we never clobber the identical
;; `emacs-parity-clloop.el' / combined.repl copies.
(unless (fboundp 'emacs-cl-macros--loop-destructure-bindings)
  (defun emacs-cl-macros--loop-destructure-bindings (pattern source)
    "Return ordered `let*' bindings destructuring PATTERN from SOURCE.
Nested list patterns receive fresh temporaries, and an improper symbol tail
is bound to the unconsumed cdr.  This helper is shared by `cl-loop',
`cl-defmacro', and `cl-destructuring-bind'."
    (if (consp pattern)
        (let* ((item (car pattern))
               (tail (cdr pattern))
               (item-bindings
                (cond
                 ((null item) nil)
                 ((symbolp item)
                  (list (list item (list 'car source))))
                 (t
                  (let ((value (make-symbol "--cl-destructure--")))
                    (cons
                     (list value (list 'car source))
                     (emacs-cl-macros--loop-destructure-bindings
                      item value)))))))
          (append
           item-bindings
           (emacs-cl-macros--loop-destructure-bindings
            tail (list 'cdr source))))
      (if pattern (list (list pattern source)) nil))))

;;;; --- cl-destructuring-bind (unconditional, correct) -----------------

;; Overrides the buggy prelude copy that aborts on dotted patterns like
;; `(form . attrs)'.  Body copied verbatim from `src/emacs-cl-macros.el'
;; (the guarded, skipped-in-audit definition) with the `when' guard removed.
(defmacro cl-destructuring-bind (arglist expr &rest body)
  "Bind the variables in ARGLIST to successive elements of the list EXPR.
Supports &optional (with defaults), &rest/&body, &key (with defaults) and
&aux, nested list patterns, and dotted tails."
  (let* ((parts (emacs-cl-macros--split-arglist arglist))
         (positional (nth 0 parts))
         (optionals (nth 1 parts))
         (restsym (nth 2 parts))
         (keys (nth 3 parts))
         (vsym (make-symbol "--cl-ds--"))
         (restvar (or restsym
                      (and keys (make-symbol "--cl-ds-rest--"))))
         (idx 0)
         (bindings (list (list vsym expr))))
    ;; positionals: (POS (nth IDX V))
    (dolist (p positional)
      (if (symbolp p)
          ;; Preserve the flat-pattern expansion exactly.
          (setq bindings (cons (list p (list 'nth idx vsym)) bindings))
        (let ((value (make-symbol "--cl-destructure--")))
          (setq bindings
                (cons (list value (list 'nth idx vsym)) bindings))
          (dolist (binding
                   (emacs-cl-macros--loop-destructure-bindings p value))
            (setq bindings (cons binding bindings)))))
      (setq idx (1+ idx)))
    ;; optionals: token is VAR or (VAR DEFAULT); present iff the cons exists
    (dolist (o optionals)
      (let ((osym (if (consp o) (car o) o))
            (odef (if (consp o) (car (cdr o)) nil)))
        (setq bindings
              (cons (list osym
                          (list 'if (list 'nthcdr idx vsym)
                                (list 'nth idx vsym)
                                odef))
                    bindings))
        (setq idx (1+ idx))))
    ;; &rest / the list scanned for &key values
    (when restvar
      (setq bindings (cons (list restvar (list 'nthcdr idx vsym)) bindings)))
    ;; &key: (SYM (or (cadr (memq :SYM RESTVAR)) DEFAULT))
    (when keys
      (dolist (kb (emacs-cl-macros--key-bindings keys restvar))
        (setq bindings (cons kb bindings))))
    (cons 'let* (cons (nreverse bindings) body))))

;; Belt-and-suspenders: force the `nelisp--macros' entry (used by the full
;; self-host evaluator, which consults the hashtable rather than the function
;; cell) to the corrected macro, mirroring `emacs-parity-clloop.el'.
(when (and (boundp 'nelisp--macros)
           (hash-table-p nelisp--macros))
  (puthash 'cl-destructuring-bind
           (symbol-function 'cl-destructuring-bind)
           nelisp--macros))

;;;; --- generalized-place setters for evil / annalist ------------------
;;
;; SECOND real-init audit family on the evil frontier.  Once the
;; `cl-destructuring-bind' fix above lets every evil command macro EXPAND,
;; each command definition runs `evil-set-command-properties' /
;; `evil-add-command-properties' (evil-common.el:391/373), whose bodies do
;;
;;     (setf (evil-command-properties COMMAND) PROPERTIES)   ; evil-common.el:396,381
;;
;; The standalone `setf' polyfill (`nelisp--setf-1',
;; nelisp-stdlib-prelude.el:3172) resolves a generalized (non-builtin) place
;; ONLY through the `cl-simple-setter' symbol property.  Evil registers its
;; place with `gv-define-setter' (evil-common.el:351) instead, which the
;; polyfill never consults, so `(setf (evil-command-properties CMD) VAL)'
;; raised `("setf: unsupported place" evil-command-properties)' -- 412 times
;; in the audit, the single largest remaining `setf' family.  Evil commands
;; therefore ended up with no properties (:repeat / :type / :jump / :keep-visual
;; ...), i.e. defined-but-inert.
;;
;; This is the SAME class of defect, and takes the SAME fix, as
;; `emacs-parity-flycheck.el' did for `flycheck-checker-get': register a
;; `cl-simple-setter'.  A `cl-simple-setter' is a FUNCTION that
;; `nelisp--setf-1' calls with the getter's argument(s) followed by the new
;; value (`(funcall #'SETTER ARGS... VALUE)', prelude line 3200), returning
;; the assigned value.  The bodies below mirror upstream's `gv-define-setter'
;; expansions verbatim.
;;
;; Bonus: the same registration repairs the small
;; `("setf: unsupported place" annalist--get-view-settings)' family (5 in the
;; audit); annalist is pulled in by the evil-collection / general keybinding
;; stack and hits the identical `gv-define-setter'-invisible-to-polyfill wall
;; (annalist.el:311).
;;
;; Like `emacs-parity-flycheck.el' this is a no-op outside the standalone
;; runtime: the setter helpers are defined unconditionally (so the symbols are
;; always resolvable) but the `cl-simple-setter' registrations are gated on
;; `emacs-parity-evil--standalone-p', so host Emacs keeps its real gv-based
;; `setf' and its own evil/annalist behaviour.

(defvar emacs-parity-evil--standalone-p
  (fboundp 'nelisp--eval-source-string)
  "Non-nil only inside the standalone NeLisp self-host runtime.
Guards the `cl-simple-setter' registrations so host Emacs is left untouched.")

;; Forward declarations: the real specials live in evil-common.el / annalist.el
;; (loaded after this shim).  Bodyless `defvar' only marks them special for
;; this file; it does not create or clobber a value.
(defvar evil--command-properties)
(defvar annalist--tomes-views)

;; evil-common.el getter (line 343):
;;   (evil-command-properties COMMAND)
;;     => (if (symbolp COMMAND) (get COMMAND 'evil--command-plist)
;;          (cdr (assq COMMAND evil--command-properties)))
;; evil-common.el gv-define-setter (line 351):
;;   (setf (evil-command-properties COMMAND) VAL)
;;     => (if (symbolp COMMAND) (put COMMAND 'evil--command-plist VAL)
;;          (let ((p (assq COMMAND evil--command-properties)))
;;            (if p (setcdr p VAL) (push (cons COMMAND VAL) evil--command-properties))))
(defun emacs-parity-evil--command-properties-set (command value)
  "Setter for the `evil-command-properties' generalized place; return VALUE.
Mirrors the `gv-define-setter' for `evil-command-properties' in
evil-common.el: symbol COMMANDs store on the `evil--command-plist' symbol
property, non-symbol (lambda) COMMANDs update the `evil--command-properties'
alist."
  (if (symbolp command)
      (put command 'evil--command-plist value)
    (let ((cell (assq command evil--command-properties)))
      (if cell
          (setcdr cell value)
        (setq evil--command-properties
              (cons (cons command value) evil--command-properties)))))
  value)

;; annalist.el getter (line 305):
;;   (annalist--get-view-settings TYPE VIEW)
;;     => (gethash (cons TYPE (or VIEW 'default)) annalist--tomes-views)
;; annalist.el gv-define-setter (line 311):
;;   (setf (annalist--get-view-settings TYPE VIEW) VAL)
;;     => (puthash (cons TYPE (or VIEW 'default)) VAL annalist--tomes-views)
(defun emacs-parity-evil--annalist-view-settings-set (type view value)
  "Setter for the `annalist--get-view-settings' generalized place; return VALUE.
Mirrors the `gv-define-setter' for `annalist--get-view-settings' in
annalist.el."
  (puthash (cons type (or view 'default)) value annalist--tomes-views)
  value)

(when emacs-parity-evil--standalone-p
  (put 'evil-command-properties 'cl-simple-setter
       'emacs-parity-evil--command-properties-set)
  (put 'annalist--get-view-settings 'cl-simple-setter
       'emacs-parity-evil--annalist-view-settings-set))

(provide 'emacs-parity-evil)

;;; emacs-parity-evil.el ends here
