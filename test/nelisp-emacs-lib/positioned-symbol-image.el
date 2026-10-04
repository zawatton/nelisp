;;; positioned-symbol-image.el --- Positioned object dump/restore check -*- lexical-binding: t; -*-

;; A restored root selects validation; an initial process creates the image.
(if (boundp 'n4a-image-root)
    (progn
      (let* ((s (aref n4a-image-root 0)) (bare (bare-symbol s)))
        (garbage-collect)
        (unless (and (symbol-with-pos-p s)
                     (= (symbol-with-pos-pos s) -1)
                     (= (symbol-with-pos-pos (aref n4a-image-root 2)) most-negative-fixnum)
                     (= (symbol-with-pos-pos (aref n4a-image-root 3)) most-positive-fixnum)
                     (eq s (car (aref n4a-image-root 1)))
                     (eq bare (bare-symbol (aref n4a-image-root 2)))
                     (not (eq bare (intern "image-identity")))
                     (not (symbolp s))
                     (let ((symbols-with-pos-enabled t))
                       (and (symbolp s) (eq s bare)
                            (eq s (aref n4a-image-root 2)))))
          (error "Positioned-symbol image restore failed")))
      (princ "N4A-IMAGE-RESTORED\n"))
  (progn
    (defvar n4a-image-root
      (let* ((u (make-symbol "image-identity")) (s (position-symbol u -1)))
        (vector s (list s) (position-symbol s most-negative-fixnum)
                (position-symbol s most-positive-fixnum))))
    (garbage-collect)
    (unless (> (nelisp--arena-dump-image-stream (getenv "N4A_POSITIONED_IMAGE")) 0)
      (error "Positioned-symbol image dump failed"))
    (princ "N4A-IMAGE-CREATED\n")))
