;;; standalone-symview-mark-check.el --- the collector ignores a stale symbol view -*- lexical-binding: t; -*-

;; Doc 226 D9: a tag-16 view of a symbol carries the object's address at
;; +8, and the collector marks through it (nl_gc_mark_symview).  A view can
;; outlive its object (a dead scratch slot the conservative scan finds), so
;; the address must be validated as GNU's mark_maybe_pointer does through
;; live_symbol_holding (alloc.c:4675): a view that points into the middle of
;; a cell, or at a cell never allocated, marks nothing.  Without that, the
;; mark bit (header bit 24) lands inside a cell: in a field of a live symbol
;; or in the zeroed header of a free cell.
;;
;; (nelisp--debug-switch 43 SYM OFFSET) fabricates such a view, runs the
;; marker on it, and answers how many words of the three cells around the
;; address changed: it must be 0.  An unknown switch answers 0 as well, so
;; the check first requires the arm to answer -1 for an offset it rejects.

(defvar symview-failures 0)
(defun symview-check (ok label)
  (unless ok
    (setq symview-failures (1+ symview-failures))
    (princ (format "SYMVIEW FAIL %s\n" label))))

(let ((s (make-symbol "symview-probe")))
  (set s 1)
  (put s 'symview 2)
  (symview-check (eql (nelisp--debug-switch 43 s 99) -1)
                 "the stale-view arm rejects an offset beyond the cell")
  ;; Offsets that are neither the cell start, its WORD tag (6) nor a field
  ;; address (a multiple of 8): live_symbol_holding refuses every one.
  (dolist (off '(1 2 3 4 5 7 9 12 17 22 33 41 44 47))
    (let ((r (nelisp--debug-switch 43 s off)))
      (symview-check (eql r 0) (format "a view %d bytes into a live cell changed %S words" off r))))
  ;; The first never-allocated cell of the current block (the arm answers -1
  ;; when the block is full; one more symbol then starts a new block).
  (let ((r (nelisp--debug-switch 43 s -1)))
    (when (eql r -1)
      (make-symbol "symview-pad")
      (setq r (nelisp--debug-switch 43 s -1)))
    (symview-check (eql r 0) (format "a view of a never-allocated cell changed %S words" r)))
  (garbage-collect)
  (garbage-collect)
  (symview-check (and (eql (symbol-value s) 1) (eql (get s 'symview) 2)
                      (equal (symbol-name s) "symview-probe"))
                 "the symbol is intact after two collections"))

(princ (format "SYMVIEW failures=%d %s\n" symview-failures (if (= symview-failures 0) "PASS" "FAIL")))
(when (> symview-failures 0) (error "symview mark check failed"))
