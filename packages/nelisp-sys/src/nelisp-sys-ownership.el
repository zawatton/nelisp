;;; nelisp-sys-ownership.el --- Move/ownership checker for nelisp-sys -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Stage 130.S2 / 131.3: the ownership / move checker.  Verifies that
;; move-only values are consumed exactly once per execution path.
;;
;; Model (MVP, conservative, linear):
;;   Per defun, a set `moved' (a list of variable names) tracks variables
;;   that have already been consumed.  Visiting a `var' node in a consuming
;;   position when the variable's type is move-only:
;;     - if the name is already in `moved' => E-SYS-OWN-001 (use-after-move /
;;       double consume)
;;     - else add the name to `moved'.
;;   Copy-typed vars are never consumed.
;;
;; Thread `moved' left-to-right through evaluation order.  Branch bodies are
;; walked sequentially (conservative: a name moved in any branch stays moved).
;;
;; Non-consuming positions:
;;   - :place in `with-borrow' (borrowing does not move)
;;   - :arg in `resource-op' whose :op is `sys:dup' (reads without moving)
;;
;; Uses only cl-lib and core special forms.  No `pcase'.

;;; Code:

(require 'cl-lib)
(require 'nelisp-sys)
(require 'nelisp-sys-types)
(require 'nelisp-sys-ast)
(require 'nelisp-sys-check)

;;; Error type.

(define-error 'nelisp-sys-ownership-error
  "nelisp-sys ownership error" 'nelisp-sys-error)

(defun nelisp-sys-ownership--fail (code form fmt &rest args)
  "Signal `nelisp-sys-ownership-error' with CODE for FORM and message FMT/ARGS."
  (signal 'nelisp-sys-ownership-error
          (list code (apply #'format fmt args) :form form)))

(defun nelisp-sys-ownership-error-code (err)
  "Return the stable diagnostic code from a `nelisp-sys-ownership-error' ERR."
  (nth 1 err))

(defun nelisp-sys-ownership-error-message (err)
  "Return the message string from a `nelisp-sys-ownership-error' ERR."
  (nth 2 err))

;;; Walk helpers.

(defun nelisp-sys-ownership--lookup (locals name)
  "Return the type bound to NAME in LOCALS alist, or nil if unbound."
  (cdr (assq name locals)))

(defun nelisp-sys-ownership--move-only-p (type env)
  "Return non-nil if TYPE is move-only under struct ENV."
  (nelisp-sys-type-move-only-p type env))

;;; Core walker.
;;
;; Returns the updated `moved' list after visiting NODE.
;; LOCALS  : alist (NAME . TYPE) for variables in scope.
;; MOVED   : list of variable names already consumed.
;; CONSUMING : non-nil means this position is value-consuming.

(defun nelisp-sys-ownership--walk (ctx node locals moved consuming)
  "Walk NODE under CTX/LOCALS/MOVED with CONSUMING flag; return updated moved."
  (let ((kind (nelisp-sys-ast-kind node))
        (env  (nelisp-sys-check-ctx-structs ctx)))
    (cl-case kind

      ;; --- var: the key rule ---
      (var
       (let* ((name (nelisp-sys-ast-prop node :name))
              (type (nelisp-sys-ownership--lookup locals name)))
         (when (and consuming type
                    (nelisp-sys-ownership--move-only-p type env))
           (if (memq name moved)
               (nelisp-sys-ownership--fail
                'E-SYS-OWN-001
                (nelisp-sys-ast-prop node :form)
                "use after move: variable `%S' has already been consumed" name)
             (setq moved (cons name moved))))
         moved))

      ;; --- with-borrow: :place is non-consuming, :body is consuming ---
      (with-borrow
       (let* ((var      (nelisp-sys-ast-prop node :var))
              (ref-type (nelisp-sys-ast-prop node :ref-type))
              (place    (nelisp-sys-ast-prop node :place))
              (body     (nelisp-sys-ast-prop node :body))
              ;; Walk :place with consuming=nil (borrow does not move).
              (moved1   (nelisp-sys-ownership--walk ctx place locals moved nil))
              ;; Extend locals with the borrow variable for the body.
              (locals*  (cons (cons var ref-type) locals)))
         ;; Walk body expressions in order, threading moved.
         (dolist (expr body)
           (setq moved1 (nelisp-sys-ownership--walk ctx expr locals* moved1 t)))
         moved1))

      ;; --- resource-op: sys:dup is non-consuming; sys:forget is consuming ---
      (resource-op
       (let* ((op  (nelisp-sys-ast-prop node :op))
              (arg (nelisp-sys-ast-prop node :arg)))
         (cond
          ((eq op 'sys:dup)
           ;; sys:dup reads (duplicates) without moving.
           (nelisp-sys-ownership--walk ctx arg locals moved nil))
          (t
           ;; sys:forget (and any other resource-op) consumes the value.
           (nelisp-sys-ownership--walk ctx arg locals moved t)))))

      ;; --- let: evaluate each init then extend locals ---
      (let
       (let ((bindings (nelisp-sys-ast-prop node :bindings))
             (body     (nelisp-sys-ast-prop node :body))
             (locals*  locals)
             (moved*   moved))
         ;; For each binding: walk the init expr (threading moved), then add
         ;; the bound name to locals with its declared type.
         (dolist (b bindings)
           (let ((name  (nth 0 b))
                 (type  (nth 1 b))
                 (init  (nth 2 b)))
             (setq moved* (nelisp-sys-ownership--walk ctx init locals* moved* t))
             (setq locals* (cons (cons name type) locals*))))
         ;; Now walk the body under the extended locals.
         (dolist (expr body)
           (setq moved* (nelisp-sys-ownership--walk ctx expr locals* moved* t)))
         moved*))

      ;; --- seq: walk body in order ---
      (seq
       (let ((moved* moved))
         (dolist (expr (nelisp-sys-ast-prop node :body))
           (setq moved* (nelisp-sys-ownership--walk ctx expr locals moved* consuming)))
         moved*))

      ;; --- Leaf nodes with no children: nothing to track ---
      ((int bool sizeof alignof offsetof)
       moved)

      ;; --- All other nodes: generic left-to-right child traversal ---
      ;; We rely on nelisp-sys-ast-children for non-special nodes.
      ;; Note: `let', `with-borrow', `resource-op' are handled above and we
      ;; do NOT fall through to the generic case.
      (t
       (let ((moved* moved))
         (dolist (child (nelisp-sys-ast-children node))
           (setq moved* (nelisp-sys-ownership--walk ctx child locals moved* t)))
         moved*)))))

;;; Per-defun check.

(defun nelisp-sys-ownership--check-defun (ctx item)
  "Ownership-check one defun ITEM under CTX.
Signals `nelisp-sys-ownership-error' on the first violation."
  (let* ((params  (nelisp-sys-ast-prop item :params))
         (body    (nelisp-sys-ast-prop item :body))
         ;; Build initial locals from params: ((NAME . TYPE)...).
         (locals  (mapcar (lambda (p) (cons (car p) (cdr p))) params))
         (moved   '()))
    (dolist (expr body)
      (setq moved (nelisp-sys-ownership--walk ctx expr locals moved t)))))

;;; Public API.

(defun nelisp-sys-ownership-check-module (module)
  "Ownership-check MODULE (an AST module node).
Signals `nelisp-sys-ownership-error' with a stable code on the first
violation.  Returns t when the module is clean."
  (let ((ctx (nelisp-sys-check-build-ctx module)))
    (dolist (item (nelisp-sys-ast-prop module :items))
      (when (eq (nelisp-sys-ast-kind item) 'defun)
        (nelisp-sys-ownership--check-defun ctx item)))
    t))

(defun nelisp-sys-ownership-check-collect (module)
  "Ownership-check MODULE, returning diagnostics instead of signaling.
Returns a list of (CODE MESSAGE FORM) on error, nil when clean.
Stops at the first violation (MVP: no error recovery)."
  (condition-case err
      (progn (nelisp-sys-ownership-check-module module) nil)
    (nelisp-sys-ownership-error
     (list (list (nelisp-sys-ownership-error-code err)
                 (nelisp-sys-ownership-error-message err)
                 (plist-get (nthcdr 3 err) :form))))))

(provide 'nelisp-sys-ownership)

;;; nelisp-sys-ownership.el ends here
