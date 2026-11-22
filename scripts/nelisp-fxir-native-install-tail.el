;;; nelisp-fxir-native-install-tail.el --- hooks for native prelude closures  -*- lexical-binding: t -*-
;; Second half of the install pass (see nelisp-fxir-native-install.el): every
;; later registration of a listed name is checked too (a function defined
;; again with the same fxir runs native code again), then the records compiled
;; before this pass are converted.
(when (and (boundp 'nelisp-nc--table) nelisp-nc--table (fboundp 'nelisp--fx-active-p)
           (nelisp--fx-active-p) (not (fboundp 'nelisp-nc--orig-register-fn)))
  (fset 'nelisp-nc--orig-register-fn (symbol-function 'nelisp--fx-register-fn))
  ;; Native code calls a closure callee without the first-call hook, so a
  ;; callee that has no record would stay interpreted (F43/F50): register the
  ;; callees of a converted function now (each goes through the wrapper below,
  ;; so a row callee is converted and its own callees follow).
  (defun nelisp-nc--register-callees (name)
    "Register the closure callees of NAME's native row that have no record."
    (let ((row (and (eq (car nelisp-nc--last-row) name) nelisp-nc--last-row)) (seen nil))
      (when row
        (dolist (c (nth 2 row))
          (unless (or (memq c seen) (eq c name))
            (push c seen)
            (let ((cf (and (fboundp c) (nelisp--closure-cons (symbol-function c)))))
              (when (and (eq (car-safe cf) 'closure)
                         (memq (gethash (nthcdr 3 cf) nelisp--fx-table) '(nil none)))
                ;; A `none' left by a call made before the compiler could run
                ;; is retried like an absent record.
                (remhash (nthcdr 3 cf) nelisp--fx-table)
                (remhash (nthcdr 3 cf) nelisp--fx-pending)
                (nelisp--fx-register c))))))))
  (fset 'nelisp--fx-register-fn
        (lambda (name fn &optional force)
          (prog1 (nelisp-nc--orig-register-fn name fn force)
            (when (and name (assq name nelisp-nc--table))
              (condition-case nil
                  (when (nelisp-nc--convert name fn) (nelisp-nc--register-callees name))
                (error nil))))))
  (dolist (row nelisp-nc--table)
    (let ((name (car row)))
      (when (and (fboundp name) (not (memq :mutant row)))
        (condition-case nil
            (when (nelisp-nc--convert name (symbol-function name))
              (nelisp-nc--register-callees name))
          (error nil))))))

;; A normal boot (runtime-image replay, `exec-runtime-image') never ran
;; `nelisp--fx-register-prelude', which only the cold-image dump calls: a row
;; function the prelude defined before the registration hooks has no record,
;; and its first call stores `none' (no name is pending for it), so it ran
;; interpreted for good, 10-40x slower than the cold-image boot (F43/F50).
;; Note each such function like a later definition: pending now, compiled on
;; its first call, and converted by the wrapper above.  A `none' left by a
;; call made before the compiler could run is retried the way
;; `nelisp--fx-register-prelude' retries it.  Nothing is compiled here, and
;; a function defined again later is noted by the definition hooks as before.
(when (and (boundp 'nelisp-nc--table) nelisp-nc--table (fboundp 'nelisp--fx-defer)
           (fboundp 'nelisp--fx-active-p) (nelisp--fx-active-p))
  (dolist (row nelisp-nc--table)
    (let ((name (car row)))
      (when (and (fboundp name) (not (memq :mutant row)))
        (condition-case nil
            (let ((fn (nelisp--closure-cons (symbol-function name))))
              (when (eq (car-safe fn) 'closure)
                (let ((cur (gethash (nthcdr 3 fn) nelisp--fx-table)))
                  (when (or (null cur) (eq cur 'none))
                    (remhash (nthcdr 3 fn) nelisp--fx-table)
                    (nelisp--fx-defer name)))))
          (error nil))))))
