;;; standalone-buffer-list-order-check.el --- `nelisp-buffer-list' keeps creation order -*- lexical-binding: t -*-

;; Usage: nelisp --load test/standalone-buffer-list-order-check.el
;;
;; GNU's buffer list holds buffers in creation order until `record-buffer'
;; or `bury-buffer' moves one.  `nelisp-buffer-list' used to enumerate the
;; registry's hash buckets, an order that changed with the table's size:
;; 6e41c5ee7 (8 starting buckets) flipped *scratch* and *Messages* and broke
;; a parity area, and the 2048 buckets that held the old order cost 59 ms a
;; call.  Over 40 buffers, so that the registry grows and rehashes, this
;; checks creation order, a kill, a rename done the way the library does it
;; (remhash, set the name, puthash) and a buffer registered without the
;; constructors, which goes last.  Prints one BUFFER-ORDER line and exits
;; non-zero on failure.

(defvar buffer-order-check--failures nil)

(let* ((before (nelisp-buffer-list))
       (names (let ((out nil) (i 0))
                (while (< i 40)
                  (setq out (cons (format "order-check-%02d" i) out) i (1+ i)))
                (nreverse out)))
       (bufs (mapcar #'nelisp-get-buffer-create names))
       (ours (lambda ()
               (let ((r nil))
                 (dolist (b (nelisp-buffer-list))
                   (unless (memq b before) (setq r (cons (nelisp-buffer-name b) r))))
                 (nreverse r))))
       (expect (lambda (label expected)
                 (let ((got (funcall ours)))
                   (unless (equal got expected)
                     (setq buffer-order-check--failures
                           (cons (list label got) buffer-order-check--failures)))))))
  (funcall expect 'creation names)
  ;; Kill the sixth buffer.
  (nelisp-kill-buffer (nth 5 bufs))
  (setq names (append (butlast names 35) (nthcdr 6 names)))
  (funcall expect 'kill names)
  ;; Rename the first one the way the library's rename-buffer does.
  (let ((b (nth 0 bufs)))
    (remhash (nelisp-buffer-name b) nelisp-buffer--registry)
    (setf (nelisp-buffer-name b) "order-check-renamed")
    (puthash "order-check-renamed" b nelisp-buffer--registry))
  (setq names (cons "order-check-renamed" (cdr names)))
  (funcall expect 'rename names)
  ;; A buffer registered without the constructors still belongs, last.
  (puthash "order-check-stray" (nelisp-buffer--make :name "order-check-stray")
           nelisp-buffer--registry)
  (funcall expect 'stray (append names (list "order-check-stray"))))

(dolist (f (reverse buffer-order-check--failures))
  (princ (format "  MISMATCH %S\n" f)))
(princ (format "BUFFER-ORDER failures=%d %s\n" (length buffer-order-check--failures)
               (if buffer-order-check--failures "FAIL" "PASS")))
(kill-emacs (if buffer-order-check--failures 1 0))

;;; standalone-buffer-list-order-check.el ends here
