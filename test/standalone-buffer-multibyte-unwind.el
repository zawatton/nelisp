;;; standalone-buffer-multibyte-unwind.el --- Authoritative buffer state regression -*- lexical-binding: t; -*-
;; Run after the genuine buffer provider loads, or in GNU Emacs.
;; This focused component check does not establish package acceptance.
(let ((a (generate-new-buffer " *multibyte-original*"))
      (b (generate-new-buffer " *multibyte-middle*"))
      (c (generate-new-buffer " *multibyte-inner*"))
      (original (current-buffer)) results)
  (unwind-protect
      (progn
        (set-buffer a)
        (set-buffer-multibyte t)
        (with-current-buffer b
          (set-buffer a)
          (set-buffer-multibyte nil))
        (push (list (eq (current-buffer) a) enable-multibyte-characters) results)
        (set-buffer-multibyte t)
        (condition-case nil
            (with-current-buffer b
              (with-current-buffer c
                (set-buffer a)
                (set-buffer-multibyte nil)
                (error "nested unwind")))
          (error nil))
        (push (list (eq (current-buffer) a) enable-multibyte-characters) results)
        (set-buffer-multibyte t)
        (catch 'exit
          (with-temp-buffer
            (with-current-buffer b
              (set-buffer a)
              (set-buffer-multibyte nil)
              (throw 'exit t))))
        (push (list (eq (current-buffer) a) enable-multibyte-characters) results)
        ;; Temporary buffer destruction precedes final public view restoration.
        (with-temp-buffer (set-buffer-multibyte t))
        (push (list (eq (current-buffer) a) enable-multibyte-characters) results)
        (setq results (nreverse results))
        (princ results)
        (unless (equal results '((t nil) (t nil) (t nil) (t nil)))
          (error "Authoritative buffer unwind parity failed: %S" results))
        (princ "\nBUFFER-UNWIND-AUTHORITY-PARITY-PASS\n"))
    (set-buffer original)
    (kill-buffer a)
    (kill-buffer b)
    (kill-buffer c)))
