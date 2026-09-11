;;; nelisp-runtime-reload-telemetry.el --- GC source instrumentation -*- lexical-binding: t; -*-

;;; Commentary:

;; The runtime-reload development builder uses this small, pure source
;; transformer to preserve the last conservative-GC result after the normal
;; collector cleanup clears its working state.  It deliberately transforms
;; only executable zero-argument calls in defun bodies.  The conservative
;; begin/clear implementations remain byte-for-byte equivalent as forms, and
;; quoted data is never interpreted as code.

;;; Code:

(defconst nelisp-runtime-reload-telemetry--excluded-defuns
  '(nl_gc_conserv_begin nl_gc_conserv_state_clear
    nl_runtime_reload_gc_finish))

(defconst nelisp-runtime-reload-telemetry--finish-form
  '(defun nl_runtime_reload_gc_finish ()
     (seq
      (ptr-write-u64 (data-addr nl_runtime_reload_state) 64
                     (ptr-read-u64 (data-addr nl_gc_conserv_state) 40))
      (ptr-write-u64 (data-addr nl_runtime_reload_state) 72
                     (ptr-read-u64 (data-addr nl_gc_conserv_state) 48))
      (ptr-write-u64 (data-addr nl_runtime_reload_state) 80
                     (ptr-read-u64 (data-addr nl_gc_conserv_state) 32))
      (ptr-write-u64
       (data-addr nl_runtime_reload_state) 88
       (+ (ptr-read-u64 (data-addr nl_runtime_reload_state) 88) 1))
      (nl_gc_conserv_state_clear)
      0)))

(defun nelisp-runtime-reload-telemetry--copy (form)
  "Copy FORM while replacing executable terminal clear calls."
  (cond
   ((atom form) form)
   ((and (eq (car form) 'nl_gc_conserv_state_clear)
         (null (cdr form)))
    '(nl_runtime_reload_gc_finish))
   ;; A quoted list is data.  In particular, a source fixture may contain the
   ;; symbol being replaced as a test value and must retain that value.
   ((eq (car form) 'quote)
    (copy-tree form))
   ;; A function symbol is a value, while a function-wrapped lambda contains
   ;; executable body forms that still belong to the enclosing defun.
   ((eq (car form) 'function)
    (if (and (consp (cadr form)) (eq (caadr form) 'lambda))
        (list 'function
              (nelisp-runtime-reload-telemetry--copy (cadr form)))
      (copy-tree form)))
   (t (mapcar #'nelisp-runtime-reload-telemetry--copy form))))

(defun nelisp-runtime-reload-telemetry--defun-p (form)
  "Return non-nil when FORM is a named defun form."
  (and (consp form)
       (eq (car form) 'defun)
       (symbolp (cadr form))))

(defun nelisp-runtime-reload-telemetry--transform-defun (form)
  "Transform one named defun FORM, preserving excluded implementations."
  (if (memq (cadr form)
            nelisp-runtime-reload-telemetry--excluded-defuns)
      (copy-tree form)
    (let ((head (list (car form) (cadr form) (nth 2 form)))
          (body (cdddr form)))
      (append head (mapcar #'nelisp-runtime-reload-telemetry--copy body)))))

(defun nelisp-runtime-reload-telemetry--top-form (form)
  "Transform a top-level FORM when it is a named defun."
  (if (nelisp-runtime-reload-telemetry--defun-p form)
      (nelisp-runtime-reload-telemetry--transform-defun form)
    (copy-tree form)))

(defun nelisp-runtime-reload-telemetry--has-finish-p (forms)
  "Return non-nil when FORMS already defines the finish helper."
  (catch 'found
    (dolist (form forms nil)
      (when (and (nelisp-runtime-reload-telemetry--defun-p form)
                 (eq (cadr form) 'nl_runtime_reload_gc_finish))
        (throw 'found t)))))

(defun nelisp-runtime-reload-instrument-gc (source)
  "Return an instrumented copy of SOURCE.

SOURCE is expected to be a `(seq DEFUN...)' source form.  Every executable
zero-argument `(nl_gc_conserv_state_clear)' in an ordinary defun body is
replaced with `(nl_runtime_reload_gc_finish)'.  The bodies of
`nl_gc_conserv_begin' and `nl_gc_conserv_state_clear' are preserved, as is
quoted data.  The finish helper is appended once when absent.  Invalid or
non-`seq' input is copied unchanged."
  (if (not (and (consp source) (eq (car source) 'seq)))
      (copy-tree source)
    (let* ((forms (cdr source))
           (transformed (mapcar
                         #'nelisp-runtime-reload-telemetry--top-form forms)))
      (append (list 'seq) transformed
              (if (nelisp-runtime-reload-telemetry--has-finish-p forms)
                  nil
                (list (copy-tree
                       nelisp-runtime-reload-telemetry--finish-form)))))))

(provide 'nelisp-runtime-reload-telemetry)
;;; nelisp-runtime-reload-telemetry.el ends here
