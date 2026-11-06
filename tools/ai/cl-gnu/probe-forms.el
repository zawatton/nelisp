;;; probe-forms.el --- load a source file form by form and count failures  -*- lexical-binding: t; -*-

;; Usage: BIN --load tools/ai/cl-gnu/probe-forms.el -- FILE [MAX-FAILURES]
;; Evaluates every top-level form of FILE in order inside condition-case and
;; prints "FAIL NNN <head> <arg> -> <error>" per failure, then
;; "PROBE-FORMS file=<base> forms=<n> failures=<m>".

(defun clgnu-probe--clip (object width)
  (let ((s (prin1-to-string object)))
    (substring s 0 (min width (length s)))))

(defun clgnu-probe--describe (form)
  (if (consp form)
      (format "%s %s" (car form)
              (if (consp (cdr form)) (clgnu-probe--clip (cadr form) 40) ""))
    (clgnu-probe--clip form 40)))

(let* ((args (member "--" command-line-args))
       (file (or (cadr args) (error "probe-forms: FILE argument required")))
       (limit (if (nth 2 args) (string-to-number (nth 2 args)) 1000000))
       (text (with-temp-buffer (insert-file-contents file) (buffer-string)))
       (pos 0)
       (n 0)
       (fails 0)
       (done nil)
       ;; File-scope lexical environment: GNU `load' makes a bare
       ;; `(defvar SYM)' special for the rest of the file, which a plain
       ;; per-form `eval' would lose.
       (env (list t)))
  (while (not done)
    (let ((r (condition-case nil (read-from-string text pos) (end-of-file nil))))
      (if (null r)
          (setq done t)
        (setq pos (cdr r))
        (setq n (1+ n))
        (condition-case err
            (progn
              (when (and (eq (car-safe (car r)) 'defvar) (null (cddr (car r)))
                         (symbolp (cadr (car r))))
                (push (cadr (car r)) env))
              (eval (car r) env))
          (error
           (setq fails (1+ fails))
           (when (<= fails limit)
             (princ (format "FAIL %03d %s -> %s\n" n
                            (clgnu-probe--describe (car r))
                            (clgnu-probe--clip err 120)))))))))
  (princ (format "PROBE-FORMS file=%s forms=%d failures=%d\n"
                 (file-name-nondirectory file) n fails)))
