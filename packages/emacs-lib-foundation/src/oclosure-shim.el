;;; oclosure-shim.el --- reader-native OClosure (list-closure compatible)  -*- lexical-binding: t; -*-

;; oclosure.el implements OClosures on Emacs' internal *aref-able* closure
;; objects: it stores the type in the closure's docstring slot (aref 4) and the
;; slot values in the constants slot (aref 2), and reads them back with `aref'.
;; The bare nelisp reader represents a closure as a `(closure ENV ARGS . BODY)'
;; LIST — `(aref CLOSURE n)' on it aborts (uncatchable) — so oclosure.el's
;; constructor and accessors do not work standalone (nelisp Doc 157 §4).

;; This re-implements the OClosure surface cl-generic needs on list-closures.
;; KEY TRICK: the reader's lexical env stores captured values as opaque mutable
;; cells (unreadable from outside), so the TYPE is instead embedded as a literal
;; marker `(quote (--oclosure-type-- . TYPE))' = the FIRST body form, read back
;; positionally via `(nth 3 closure)'.  Load AFTER oclosure.el (overrides it) and
;; before / alongside cl-generic.el.

;; SCOPE: type tagging + funcall work (cl-generic's `cl--generic-nnm' is a no-slot
;; OClosure, all it needs).  SLOT access (`oclosure--get'/`--set') is best-effort:
;; slotted OClosures (e.g. oclosure accessors) would need the slot cells read out,
;; which the list-closure representation does not expose; cl-generic dispatch does
;; not use slotted OClosures (its generalizers are cl-defstructs, not OClosures).

;;; Code:

(when (not (boundp 'emacs-version))

  (defmacro oclosure-lambda (type-and-slots args &rest body)
    (let* ((type (car type-and-slots))
           (fields (cdr type-and-slots))
           (class (or (cl--find-class type)
                      (error "Unknown class: %S" type)))
           (slots (oclosure--class-slots class))
           (slot-names (and slots
                            (mapcar (lambda (s) (cl--slot-descriptor-name s)) slots)))
           (slot-binds (let ((bs nil))
                         (dolist (sn slot-names (nreverse bs))
                           (let ((f (assq sn fields)))
                             (push (list sn (if f (car (cdr f)) nil)) bs))))))
      (list 'let* slot-binds
            (append (list 'lambda args
                          ;; literal type marker = first body form (nth 3).
                          (list 'quote (cons '--oclosure-type-- type)))
                    ;; force lexical capture of any slot vars referenced by BODY.
                    (if slot-names (list (cons 'ignore slot-names)))
                    body))))

  (defun oclosure-type (oclosure)
    "Return the OClosure type of OCLOSURE, or nil if it is not one."
    (and (closurep oclosure)
         (let ((marker (nth 3 oclosure)))
           (and (eq (car-safe marker) 'quote)
                (eq (car-safe (car-safe (cdr marker))) '--oclosure-type--)
                (cdr (car-safe (cdr marker)))))))

  (defun oclosure--fix-type (_ignore oclosure) oclosure)
  (defun oclosure--copy (oclosure _mutlist &rest _args) oclosure)
  ;; Best-effort: list-closure slot cells aren't externally readable; cl-generic
  ;; never reads slots off an OClosure (only the type, above).
  (defun oclosure--get (_oclosure _index &optional _mutable) nil)
  (defun oclosure--set (value _oclosure _index) value)

  ;; cl-generic-define-method companions the reader lacks (harmless stubs).
  (unless (fboundp 'get-advertised-calling-convention)
    (defun get-advertised-calling-convention (_function) nil))
  (unless (fboundp 'set-advertised-calling-convention)
    (defun set-advertised-calling-convention (_function _cc _when) nil)))

(provide 'oclosure-shim)
;;; oclosure-shim.el ends here
