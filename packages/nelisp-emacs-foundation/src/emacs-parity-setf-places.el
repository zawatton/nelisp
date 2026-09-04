;;; emacs-parity-setf-places.el --- extra setf places -*- lexical-binding: t; -*-

;; The prelude `setf' baked into the runtime image
;; (`vendor/nelisp/lisp/nelisp-cl-macros.el') dispatches a fixed set of place
;; shapes (symbol, car/cdr, aref, nth, registered cl-simple-setter /
;; cl-struct-setter, struct accessor-info) and signals
;; `("setf: unsupported place" HEAD)' for anything else.  Its only extension
;; point is the `cl-simple-setter' property, which cannot express places whose
;; setter mutates their own first argument (`cl-getf' -> `plist-put') or which
;; expand structurally (`if').  The real-init audit hits
;; `("setf: unsupported place" cl-getf)' and `("setf: unsupported place" if)'.
;;
;; Redefine `setf' with the SAME prelude dispatch plus structural/functional
;; places (cl-getf, if, gethash, plist-get, alist-get, elt, and the common
;; nested c[ad]{2}r accessors), matching GNU Emacs `setf' semantics.  Every
;; original prelude branch is preserved verbatim and checked first, so existing
;; expansions are unchanged; the new branches only fire where the prelude would
;; have signalled `unsupported place'.  Guarded to the standalone substrate so
;; a host Emacs (which has the real gv-based `setf') is left untouched.

(when (and (boundp 'nelisp-cl-macros--accessor-info)
           (fboundp 'nelisp--record-set))
  (defmacro setf (&rest pairs)
    "Generalised assignment (NeLisp; prelude places + cl-getf/if/gethash/...)."
    (when (null pairs) (signal 'error (list "setf: empty body")))
    (let ((forms nil))
      (while pairs
        (let ((place (car pairs))
              (val (cadr pairs)))
          (setq pairs (cdr (cdr pairs)))
          (push
           (cond
            ;; --- prelude branches (verbatim) --------------------------------
            ((symbolp place)
             (list 'setq place val))
            ((and (consp place) (eq (car place) 'car))
             (list 'setcar (cadr place) val))
            ((and (consp place) (eq (car place) 'cdr))
             (list 'setcdr (cadr place) val))
            ((and (consp place) (eq (car place) 'aref))
             (list 'aset (cadr place) (caddr place) val))
            ((and (consp place) (eq (car place) 'nth))
             (list 'setcar (list 'nthcdr (cadr place) (caddr place)) val))
            ((and (consp place) (symbolp (car place))
                  (get (car place) 'cl-simple-setter))
             (cons 'funcall
                   (cons (list 'quote (get (car place) 'cl-simple-setter))
                         (append (cdr place) (list val)))))
            ((and (consp place) (symbolp (car place))
                  (get (car place) 'cl-struct-setter))
             (list 'funcall
                   (list 'quote (get (car place) 'cl-struct-setter))
                   (cadr place)
                   val))
            ((and (consp place) (symbolp (car place))
                  (assq (car place) nelisp-cl-macros--accessor-info))
             (let ((idx (cdr (assq (car place)
                                   nelisp-cl-macros--accessor-info))))
               (list 'nelisp--record-set (cadr place) idx val)))
            ;; --- added structural / functional places -----------------------
            ((and (consp place) (eq (car place) 'caar))
             (list 'setcar (list 'car (cadr place)) val))
            ((and (consp place) (eq (car place) 'cadr))
             (list 'setcar (list 'cdr (cadr place)) val))
            ((and (consp place) (eq (car place) 'cdar))
             (list 'setcdr (list 'car (cadr place)) val))
            ((and (consp place) (eq (car place) 'cddr))
             (list 'setcdr (list 'cdr (cadr place)) val))
            ((and (consp place) (eq (car place) 'nthcdr))
             (list 'setcdr (list 'nthcdr (list '1- (cadr place)) (caddr place))
                   val))
            ((and (consp place) (eq (car place) 'elt))
             (let ((seq (make-symbol "seq")))
               (list 'let (list (list seq (cadr place)))
                     (list 'if (list 'listp seq)
                           (list 'setcar (list 'nthcdr (caddr place) seq) val)
                           (list 'aset seq (caddr place) val)))))
            ((and (consp place) (eq (car place) 'gethash))
             (list 'puthash (cadr place) val (caddr place)))
            ((and (consp place) (eq (car place) 'plist-get))
             (list 'setf (cadr place)
                   (list 'plist-put (cadr place) (caddr place) val)))
            ((and (consp place) (eq (car place) 'cl-getf))
             (list 'setf (cadr place)
                   (list 'plist-put (cadr place) (caddr place) val)))
            ((and (consp place) (eq (car place) 'alist-get))
             (let ((cell (make-symbol "cell")))
               (list 'let (list (list cell (list 'assq (cadr place)
                                                  (caddr place))))
                     (list 'if cell
                           (list 'setcdr cell val)
                           (list 'setf (caddr place)
                                 (list 'cons
                                       (list 'cons (cadr place) val)
                                       (caddr place)))))))
            ((and (consp place) (eq (car place) 'default-value))
             (list 'set-default (cadr place) val))
            ((and (consp place) (eq (car place) 'symbol-value))
             (list 'set (cadr place) val))
            ((and (consp place) (eq (car place) 'symbol-function))
             (list 'fset (cadr place) val))
            ((and (consp place) (eq (car place) 'get))
             (list 'put (cadr place) (caddr place) val))
            ((and (consp place) (eq (car place) 'overlay-get))
             (list 'overlay-put (cadr place) (caddr place) val))
            ((and (consp place) (eq (car place) 'process-get))
             (list 'process-put (cadr place) (caddr place) val))
            ((and (consp place) (eq (car place) 'if))
             (list 'if (cadr place)
                   (list 'setf (caddr place) val)
                   (list 'setf (cadddr place) val)))
            (t
             (signal 'error
                     (list "setf: unsupported place"
                           (and (consp place) (car place))))))
           forms)))
      (if (cdr forms)
          (cons 'progn (nreverse forms))
        (car forms)))))

(provide 'emacs-parity-setf-places)
;;; emacs-parity-setf-places.el ends here
