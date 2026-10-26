;;; nelisp-nc-native-corpus.el --- transcript over the natively compiled prelude closures  -*- lexical-binding: t; -*-
;; nativecomp N1.  Prints one line per case; tools/ai/nc1-native-check.sh runs it
;; with the VM off (interpreter), with every converted record switched back to
;; the word VM (NC_MODE=vm), as built (native), and with the negative-control
;; mutant selected (NC_MODE=mutant), and compares the transcripts.  NC_GC=1
;; collects garbage between cases and inside the loops.
(defvar nc-cases nil)
(defmacro nc (label &rest body)
  `(push (cons ,label (lambda () ,@body)) nc-cases))
(defun nc-gc () (when (getenv "NC_GC") (garbage-collect)))
(defmacro nc-buf (text &rest body)
  `(with-temp-buffer (insert ,text) ,@body))

;; --- point family
(nc "point" (nc-buf "abc\ndef" (list (point) (point-min) (point-max) (buffer-size) (bobp) (eobp))))
(nc "goto-ints" (nc-buf "abcdef" (mapcar (lambda (p) (list (goto-char p) (point) (bobp) (eobp))) '(1 3 7 0 -5 100 4))))
(nc "goto-marker" (nc-buf "abcdef" (let ((m (copy-marker 3))) (list (goto-char m) (point) (markerp m) (markerp 3)))))
(nc "goto-marker-nowhere" (nc-buf "abc" (condition-case e (goto-char (make-marker)) (error (list 'err e)))))
(nc "goto-wrong" (nc-buf "abc" (list (condition-case e (goto-char "x") (error e)) (condition-case e (goto-char nil) (error e)) (condition-case e (goto-char 1.5) (error e)))))
(nc "goto-big" (nc-buf "abc" (list (goto-char (expt 2 70)) (point) (goto-char most-positive-fixnum) (point) (goto-char most-negative-fixnum) (point))))
(nc "narrow" (nc-buf "0123456789"
               (list (narrow-to-region 3 7) (point-min) (point-max) (point) (goto-char 1) (point) (goto-char 99) (point)
                     (bobp) (eobp) (buffer-size) (progn (widen) (list (point-min) (point-max) (point))))))
(nc "narrow-reversed" (nc-buf "0123456789" (list (narrow-to-region 8 2) (point-min) (point-max) (point) (progn (goto-char 5) (narrow-to-region 6 6)) (point) (point-min) (point-max))))
(nc "narrow-point-inside" (nc-buf "0123456789" (goto-char 5) (list (narrow-to-region 2 9) (point) (progn (widen) (goto-char 1) (narrow-to-region 4 6)) (point))))
(nc "narrow-errors" (nc-buf "abc" (list (condition-case e (narrow-to-region 0 2) (error e)) (condition-case e (narrow-to-region 2 9) (error e)) (condition-case e (narrow-to-region 'x 2) (error e)) (condition-case e (narrow-to-region 2 (copy-marker 3)) (error e)) (point-min) (point-max))))
(nc "nelisp-goto" (nc-buf "hello" (let ((b nelisp--current-buffer)) (list (nelisp-goto-char 3 b) (nelisp-point b) (nelisp-point-min b) (nelisp-point-max b) (nelisp-buffer-size b) (nelisp-goto-char 0) (nelisp-point)))))
(nc "accessors" (nc-buf "xyz" (let ((b nelisp--current-buffer)) (goto-char 2) (list (nelisp-buffer-pend b) (nelisp-buffer-blen b) (nelisp-buffer-len b) (nelisp-buffer-narrow-start b) (nelisp-buffer-narrow-end b) (nelisp-buffer--before-length b) (eq (nelisp-buffer--ambient nil) b) (eq (nelisp-buffer--ambient b) b)))))
(nc "accessor-wrong" (list (condition-case e (nelisp-buffer-pend 5) (error (car e))) (condition-case e (nelisp-buffer-len nil) (error (car e))) (condition-case e (nelisp-buffer-blen (vector 1 2)) (error (car e)))))
(nc "record-set" (let ((r (record 'foo 1 2 3)) (v (vector 1 2 3)))
                   (list (nelisp--record-set r 0 'a) (nelisp--record-set r 2 'c) r
                         (condition-case e (nelisp--record-set r 5 'z) (error (car e)))
                         (condition-case e (nelisp--record-set r -2 'z) (error (car e)))
                         (nelisp--record-set v 0 'q) v (condition-case e (nelisp--record-set "abc" 0 ?z) (error e))
                         (condition-case e (nelisp--record-set nil 0 1) (error e)))))
(nc "markerp" (list (markerp (make-marker)) (markerp nil) (markerp 'a) (markerp "m") (markerp '(1)) (markerp 5)))
(nc "charset" (list (nelisp--motion-charset-member-p ?b '(?a ?b)) (nelisp--motion-charset-member-p ?m '((?a . ?z))) (nelisp--motion-charset-member-p ?M '((?a . ?z) ?q))
                    (nelisp--motion-charset-member-p ?q '((?a . ?c) ?q)) (nelisp--motion-charset-member-p 1000000 '((0 . 10))) (nelisp--motion-charset-member-p ?a nil)
                    (condition-case e (nelisp--motion-charset-member-p 'x '(?a)) (error e))))
(nc "put" (let ((s (make-symbol "nc-s"))) (list (put s 'p 1) (get s 'p) (put s 'p 2) (get s 'p) (put s 'q nil) (symbol-plist s) (condition-case e (put 5 'p 1) (error e)))))
(nc "checks" (list (nelisp--check-symbol 'a) (condition-case e (nelisp--check-symbol 5) (error e)) (nelisp--check-number 5) (nelisp--check-number 1.5) (condition-case e (nelisp--check-number 'x) (error e))))
(nc "bignump" (list (bignump 5) (bignump (expt 2 70)) (bignump 1.0) (bignump nil) (bignump most-positive-fixnum) (bignump (1+ most-positive-fixnum))))
(nc "char-table-p" (list (nelisp-casetab--char-table-p (make-char-table 'test)) (nelisp-casetab--char-table-p (vector 1 2 3 4 5 6 7)) (nelisp-casetab--char-table-p nil) (nelisp-casetab--char-table-p "x")))
(nc "case" (list (upcase ?a) (downcase ?A) (upcase "MiXed é") (downcase "MiXed É") (upcase ?1) (condition-case e (upcase 'x) (error e))))
;; nativecomp N2: a buffer's text properties (`text-properties-at' compares
;; point-min and point-max with `<'; the N2 negative control lowers it as `>').
(nc "tp-at-buffer" (nc-buf "abcdef" (put-text-property 2 5 'face 'bold)
                     (list (text-properties-at 3) (text-properties-at 1) (get-text-property 4 'face)
                           (progn (narrow-to-region 3 6) (text-properties-at 3)))))
;; Which route `text-properties-at' takes is visible as calls of its full
;; validation: none while point-min < point-max holds on a buffer whose tree
;; covers it (the mutant's `>' takes the full validation, same answer).
(nc "tp-at-route" (nc-buf "abcdef" (put-text-property 2 5 'face 'bold)
                    (let ((calls 0) r)
                      (unwind-protect
                          (progn (advice-add 'nelisp--tp-validate :before (lambda (&rest _) (setq calls (1+ calls))) '((name . nc-tpv)))
                                 (setq r (list (text-properties-at 3) (text-properties-at 5))))
                        (advice-remove 'nelisp--tp-validate 'nc-tpv))
                      (list r calls))))
(nc "loop-gc" (nc-buf (make-string 200 ?x)
                (let ((acc nil) (i 0))
                  (while (< i 300)
                    (goto-char (% (* i 7) 230))
                    (when (= (% i 50) 0) (nc-gc) (push (list (point) (bobp) (eobp)) acc))
                    (setq i (1+ i)))
                  (list (point) (nreverse acc)))))
(nc "bind-inside" (let ((case-fold-search nil)) (nc-buf "ab" (list (point) (goto-char 2) (let ((case-fold-search t)) (point-max))))))
(nc "redefine-callee" (nc-buf "abcdef"
                        (let ((old (symbol-function 'nelisp-goto-char)))
                          (unwind-protect
                              (progn (fset 'nelisp-goto-char (lambda (p &optional _b) (list 'redef p)))
                                     (list (goto-char (copy-marker 2)) (goto-char 3) (point)))
                            (fset 'nelisp-goto-char old)))))

;; Runtime redefinition (S3.4).  `nelisp-point-max' and `bignump' are converted;
;; `nelisp-buffer-len' is a callee the native code reaches through a call helper.
;; The buffer is filled before the redefinition: v20 already differs between the
;; interpreter and the VM when `insert' itself runs under an advised
;; `nelisp-buffer-len' (6 vs 5), independently of native code.
(defmacro nc-pmax () '(nelisp-point-max nc-b))
(nc "redef-callee-fset" (nc-buf "abc" (let ((nc-b nelisp--current-buffer) (old (symbol-function 'nelisp-buffer-len)))
                          (list (nc-pmax)
                                (unwind-protect (progn (fset 'nelisp-buffer-len (lambda (_b) 100)) (nc-pmax))
                                  (fset 'nelisp-buffer-len old))
                                (nc-pmax)))))
(nc "redef-callee-advice" (nc-buf "abc" (let ((nc-b nelisp--current-buffer))
                            (list (unwind-protect (progn (advice-add 'nelisp-buffer-len :filter-return #'1+ '((name . nc-adv))) (nc-pmax))
                                    (advice-remove 'nelisp-buffer-len 'nc-adv))
                                  (nc-pmax)))))
(nc "redef-self-fset" (let ((old (symbol-function 'bignump)))
                        (list (unwind-protect (progn (fset 'bignump (lambda (_x) 'redef)) (list (bignump 5) (funcall 'bignump 5)))
                                (fset 'bignump old))
                              (bignump 5) (bignump (expt 2 70)))))
(nc "redef-self-advice" (list (unwind-protect (progn (advice-add 'bignump :around (lambda (f x) (list 'adv (funcall f x))) '((name . nc-adv2)))
                                                     (list (bignump (expt 2 70)) (bignump 5)))
                                (advice-remove 'bignump 'nc-adv2))
                              (bignump (expt 2 70))))
;; GNU byte-compiled code reads `car' as an opcode, so (fset 'car ...) does not
;; reach it: GNU 31.1 compiled gives t here, interpreted nil.  Native code
;; inlines `car' the same way; the VM (and the interpreter) call the function
;; cell.  Off the shared transcript; checked by nc1-native-check.sh inline-car.
(when (getenv "NC_INLINE")
  (nc "inline-car-fset" (let ((old (symbol-function 'car)) r)
                          (unwind-protect (progn (fset 'car (lambda (_x) ?z)) (setq r (nelisp--motion-charset-member-p ?b '(?a ?b))))
                            (fset 'car old))
                          r)))

(when (getenv "NC_MODE")
  (let ((mode (intern (getenv "NC_MODE"))) (n 0))
    (dolist (row nelisp-nc--table)
      (when (and (fboundp (car row)) (not (memq :mutant row)))
        (when (nelisp-nc-select (car row) (if (eq mode 'mutant) 'native mode)) (setq n (1+ n)))))
    (when (eq mode 'mutant)
      (dolist (row nelisp-nc--table)
        (when (memq :mutant row) (nelisp-nc-select (car row) 'mutant))))
    (princ (format "MODE %s selected=%d\n" mode n))))
(when (getenv "NC_REPORT")
  (princ (format "CONVERTED %d %S\n" (length nelisp-nc--converted) (reverse nelisp-nc--converted))))
(dolist (c (reverse nc-cases))
  (nc-gc)
  (princ (format "%s => %s\n" (car c)
                 (condition-case e (format "%S" (funcall (cdr c))) (error (format "ERR %S" e))))))
