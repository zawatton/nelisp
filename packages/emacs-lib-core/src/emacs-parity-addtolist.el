;;; emacs-parity-addtolist.el --- O(1) add-to-list via a membership hash -*- lexical-binding: t; -*-

;; `add-to-list' scans the target list with `member' (O(n)) before consing.
;; Called in a loop while building a large list it is O(n^2); on this
;; substrate (whose `equal' is ~16us/op) that makes package-activation init
;; forms take tens of seconds.  Keep a per-list membership hash so the scan is
;; O(1) amortised.
;;
;; CORRECTNESS: the cache is only trusted while the list value is exactly the
;; one this function last stored.  If the list was mutated elsewhere (e.g.
;; `(setq load-path ...)'), the cached value no longer `eq's the current one,
;; so the hash is rebuilt from the live list before use -- otherwise a stale
;; "already present" hit could drop a directory that a `setq' had removed,
;; which is what broke theme / package `load-path' lookups in an earlier
;; version of this shim.  Uses the `equal' default; a caller-supplied
;; COMPARE-FN falls back to the original O(n) implementation.
(when (and (fboundp 'nelisp--write-stdout-bytes)
           (fboundp 'add-to-list))
  (defvar emacs-parity-addtolist--orig (symbol-function 'add-to-list))
  (defvar emacs-parity-addtolist--cache (make-hash-table :test 'eq)
    "Maps a list-var symbol to (MEMBER-HASH . LIST-VALUE-WE-LAST-SET).")
  (defun add-to-list (list-var element &optional append compare-fn)
    "Add ELEMENT to the list in LIST-VAR if not present (O(1) membership)."
    (if compare-fn
        (funcall emacs-parity-addtolist--orig list-var element append compare-fn)
      (let ((cur (if (boundp list-var) (symbol-value list-var) nil))
            (cell (gethash list-var emacs-parity-addtolist--cache)))
        ;; (Re)build the membership hash if the list was changed outside us.
        (unless (and cell (eq (cdr cell) cur))
          (let ((h (make-hash-table :test 'equal))
                (l cur))
            (while (consp l) (puthash (car l) t h) (setq l (cdr l)))
            (setq cell (cons h cur))
            (puthash list-var cell emacs-parity-addtolist--cache)))
        (let ((h (car cell)))
          (if (gethash element h)
              cur
            (puthash element t h)
            (let ((new (if append (append cur (list element)) (cons element cur))))
              (set list-var new)
              (setcdr cell new)         ; remember the value we just stored
              new)))))))

(provide 'emacs-parity-addtolist)
;;; emacs-parity-addtolist.el ends here
