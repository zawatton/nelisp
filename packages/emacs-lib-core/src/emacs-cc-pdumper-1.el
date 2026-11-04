;;; emacs-cc-pdumper-1.el --- Portable dumper primitives -*- lexical-binding: t; -*-

;; GNU Emacs exposes portable dumping through its C runtime.  The pure Lisp
;; runtime has no serialized-image writer, so this module preserves the
;; callable surface and the errors available without that facility.

(unless (fboundp 'dump-emacs-portable)
  (defun dump-emacs-portable (filename &optional track-referrers)
    "Dump current state of Emacs into dump file FILENAME."
    (ignore track-referrers)
    (unless (stringp filename)
      (signal 'wrong-type-argument (list 'stringp filename)))
    (signal 'file-error
            (list "Could not write to dump file"
                  "Portable dumping is unavailable in this runtime"
                  filename))))

(unless (fboundp 'dump-emacs-portable--sort-predicate-copied)
  (defun dump-emacs-portable--sort-predicate-copied (a b)
    "Internal relocation sorting function."
    (and (consp a) (consp b)
         (let ((left (car a)) (right (car b)))
           (and (numberp left) (numberp right) (< left right))))))

(unless (fboundp 'dump-emacs-portable--sort-predicate)
  (defun dump-emacs-portable--sort-predicate (a b)
    "Internal relocation sorting function."
    (dump-emacs-portable--sort-predicate-copied a b)))

(unless (fboundp 'pdumper-stats)
  (defun pdumper-stats ()
    "Return portable dumping statistics for this session."
    (list (cons 'dumped-with-pdumper t))))

(provide 'emacs-cc-pdumper-1)
