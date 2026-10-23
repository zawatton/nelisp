;;; integ-wh-gc-string-area.el --- the conservative scan and the string areas (GNU mark_maybe_pointer)  -*- lexical-binding: t; -*-

;; NeLisp only (it uses (nelisp--debug-switch 82 ...), 83 and 84).  GNU alloc.c: a conservative word inside a string's
;; 32-byte struct marks that string (live_string_holding resolves an interior pointer), a word inside string data
;; (MEM_TYPE_NON_LISP) names no object and is dropped, and nothing is ever queued as a whole block.
;;
;; 82 and 84: the classification of a word inside a string's header and inside its sdata: (handled-by-the-string-area-path, marked, compaction guard,
;;     mapping kind of the word).  A pool string: a header word marks it and sets the guard (kind 3); an sdata word
;;     marks nothing (kind 4 or 5) but sets the guard.  A string of the restored image (kind 6, header; the sdata is
;;     outside the page registry): handled, never marked, no guard (image strings are immortal).
;; 83: a collection with the only word naming a fresh string held in the calling frame: none, a word inside its header
;;     (it survives), a word inside its sdata (collected).  Stale copies of the address on the native stack can keep a
;;     string alive by chance, so each mode runs 20 times: the header word must keep it every time, the other two must
;;     lose it in at least 15.  85 and 86 are the control with no word in any frame: a fresh string answered as a
;;     fixnum, Lisp's own (garbage-collect), then whether the string is still allocated.

(defvar iwh-ga-fail 0)
(defun iwh-ga (name ok &optional detail)
  (unless ok (setq iwh-ga-fail (1+ iwh-ga-fail)))
  (princ (format "%s %s%s\n" (if ok "ok  " "FAIL") name (if detail (format " %S" detail) ""))))
(defun iwh-ga-bits (r) (list (logand r 1) (logand (ash r -1) 1) (logand (ash r -2) 1) (ash r -4)))

(let* ((pool (copy-sequence "pool string abcdefgh"))
       (h (iwh-ga-bits (nelisp--debug-switch 82 pool)))
       (d (iwh-ga-bits (nelisp--debug-switch 84 pool))))
  (iwh-ga "pool header word: handled, marked, guard, kind 3" (equal h '(1 1 1 3)) h)
  (iwh-ga "pool sdata word of the stack: handled, not marked, guard, kind 4 or 5"
          (and (= (nth 0 d) 1) (= (nth 1 d) 0) (= (nth 2 d) 1) (memq (nth 3 d) '(4 5))) d)
  (let ((b (iwh-ga-bits (nelisp--debug-switch 87 pool))))
    (iwh-ga "pool sdata word of a pinned block: handled, marks its owner, guard"
            (and (= (nth 0 b) 1) (= (nth 1 b) 1) (= (nth 2 b) 1) (memq (nth 3 b) '(4 5))) b)))

(let* ((img emacs-version)
       (h (iwh-ga-bits (nelisp--debug-switch 82 img)))
       (d (iwh-ga-bits (nelisp--debug-switch 84 img))))
  (if (= (nth 3 h) 6)
      (progn
        (iwh-ga "image header word: handled, not marked, no guard, kind 6" (equal h '(1 0 0 6)) h)
        (iwh-ga "image sdata word: handled, not marked, no guard" (and (= (nth 0 d) 1) (= (nth 1 d) 0) (= (nth 2 d) 0)) d))
    (princ (format "skip image strings: (symbol-name 'car) is a pool string here, kind %S\n" (nth 3 h)))))

(defun iwh-ga-trials (mode)
  (let ((alive 0) (n 20) (i 0) (off nil))
    (while (< i n)
      (let ((r (nelisp--debug-switch 83 mode)))
        (cond ((eq r 1) (setq alive (1+ alive)))
              ((eq r 0) nil)
              (t (setq off r))))
      (setq i (1+ i)))
    (if off (list 'off off) alive)))
;; the control with no word in any native frame: Lisp holds a fixnum, the collection is Lisp's own
(defun iwh-ga-control ()
  (let ((alive 0) (i 0))
    (while (< i 20)
      (let ((x (nelisp--debug-switch 85 0)))
        (garbage-collect)
        (when (eq (nelisp--debug-switch 86 x) 1) (setq alive (1+ alive))))
      (setq i (1+ i)))
    alive))
(let ((control (iwh-ga-control)) (header (iwh-ga-trials 1)) (sdata (iwh-ga-trials 2)) (none (iwh-ga-trials 0)))
  (if (or (consp header) (consp sdata) (consp none))
      (iwh-ga "the conservative scan is on" nil (list none header sdata))
    (iwh-ga "control, no word anywhere: the string is collected in at least 19 of 20" (<= control 1) control)
    (iwh-ga "header word keeps the string in every trial" (= header 20) header)
    (iwh-ga "no word held in a frame: the string is collected in at least 15 of 20" (<= none 5) none)
    (iwh-ga "sdata word names no object: the string is collected in at least 15 of 20" (<= sdata 5) sdata)))

(princ (if (= iwh-ga-fail 0) "GC-STRING-AREA PASS\n" (format "GC-STRING-AREA FAIL %d\n" iwh-ga-fail)))
