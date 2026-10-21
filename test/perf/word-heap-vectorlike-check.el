;;; word-heap-vectorlike-check.el --- S4 shared children and captured frames -*- lexical-binding: t; -*-

;; One closure is the sole outer keeper for the shared cons/vector/record
;; graph, char-table parent, high-character interval, extra slot and cycle.
;; Catch/apply also carry the same child across physical compatibility paths.
;; The expected file is the complete GNU31.1 answer, including error data.
;; Switch 28 disables the conservative native-stack scan before constructing
;; the graph. Unwind cleanup restores it with 29 before another case can run.
(princ (format "WORD-HEAP-VECTORLIKE %S\n"
  (progn
    (put 'whv-parity-table 'char-table-extra-slots 1)
    (unwind-protect
        (progn
          (when (fboundp 'nelisp--debug-switch) (nelisp--debug-switch 28))
          (eval
           '(let* ((type (make-symbol "whv-record-type"))
                   (reader
                    (let* ((shared (vector (cons 'seed nil) 17))
                           (object (make-record type 2 nil))
                           (parent (make-char-table 'whv-parent shared))
                           (table (make-char-table 'whv-parity-table nil))
                           (pair (cons shared object)))
                      (aset object 1 shared)
                      (aset object 2 table)
                      (set-char-table-parent table parent)
                      (set-char-table-extra-slot table 0 (vector object shared))
                      (aset table 97 shared)
                      (aset table 144470 pair)
                      (aset table 200 table)
                      (lambda (operation)
                        (when (eq operation 'change)
                          (aset object 0 17)
                          (aset object 1 nil)
                          (unless (and (eq (aref object 0) 17) (null (aref object 1)))
                            (error "WHV record mutation before GC: %S %S"
                                   (aref object 0) (aref object 1)))
                          (garbage-collect)
                          (unless (and (eq (aref object 0) 17) (null (aref object 1)))
                            (error "WHV record mutation after GC: %S %S"
                                   (aref object 0) (aref object 1)))
                          (aset object 0 type)
                          (aset object 1 shared)
                          (aset (aref object 1) 1 29)
                          (setcar (aref shared 0) 'changed))
                        (list (eq (aref object 0) type)
                              (eq (car pair) (aref object 1))
                              (eq (cdr pair) object)
                              (eq (aref table 97) shared)
                              (eq (aref table 98) shared)
                              (eq (aref table 144470) pair)
                              (eq (aref table 200) table)
                              (eq (aref (char-table-extra-slot table 0) 0) object)
                              (eq (aref (char-table-extra-slot table 0) 1) shared)
                              (eq (catch 'whv-result (throw 'whv-result shared)) shared)
                              (eq (apply #'identity (list (aref object 1))) shared)
                              (funcall (lambda (frame-value)
                                 (garbage-collect)
                                 (eq frame-value pair)) pair)
                              (aref shared 1) (car (aref shared 0)))))))
              (dotimes (_ 4) (garbage-collect))
              (funcall reader 'change)
              (dotimes (_ 4) (garbage-collect))
              (list (funcall reader 'read)
                    (condition-case e (aref (vector 17) 3) (error e))
                    (condition-case e (make-vector -1 nil) (error e))
                    (condition-case e (char-table-subtype 17) (error e))
                    (condition-case e (aref (make-record 'whv-bounds 1 nil) -1) (error e))
                    (condition-case e (aref (make-record 'whv-bounds 1 nil) 2) (error e))
                    (condition-case e (unibyte-string []) (error e))
                    (condition-case e (char-table-range (make-char-table 'x nil) (cons [] 65)) (error e))
                    (condition-case e (char-table-range (make-char-table 'x nil) (cons 65 [])) (error e))))
           t)
          )
      (when (fboundp 'nelisp--debug-switch) (nelisp--debug-switch 29))))))
(kill-emacs 0)
