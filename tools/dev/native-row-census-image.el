;;; native-row-census-image.el --- census of native rows in a booted image  -*- lexical-binding: t; -*-
;; Loaded by tools/dev/native-row-census-image.sh into a booted runtime image.  Warms a fixed
;; set of calls (word-VM records are compiled on first call), then classifies every live row of
;; `nelisp-nc--table' by the CURRENT closure's record: native (slot 1 <= -16), vm (a vector
;; record that is not native) or norec (no record).  Prints
;;   native-row-census-image: rows=N native=N vm=N norec=N
;;   VM NAME   (one per vm row)
(with-temp-buffer
  (insert "foo bar q")
  (dotimes (_ 3)
    (goto-char 1)
    (re-search-forward "q" nil t)
    (string-match "b" "abc")
    (looking-at "f")))
(let ((nat 0) (vm 0) (norec 0) (vml nil))
  (dolist (row nelisp-nc--table)
    (let ((name (car row)))
      (when (and (symbolp name) (not (memq :mutant row)) (fboundp name))
        (let* ((fn (ignore-errors (nelisp-nc--closure-list (symbol-function name))))
               (rec (and (eq (car-safe fn) 'closure) (gethash (nthcdr 3 fn) nelisp--fx-table))))
          (cond ((not (vectorp rec)) (setq norec (1+ norec)))
                ((and (integerp (aref rec 1)) (<= (aref rec 1) -16)) (setq nat (1+ nat)))
                (t (setq vm (1+ vm)) (push name vml)))))))
  (princ (format "native-row-census-image: rows=%d native=%d vm=%d norec=%d\n"
                 (length nelisp-nc--table) nat vm norec))
  (dolist (n (nreverse vml)) (princ (format "VM %S\n" n))))
