;;; standalone-native-load-slot-pin-smoke.el --- strings boxed into raw slots survive GC  -*- lexical-binding: t; -*-

;;; Commentary:

;; `nelisp-native-load-box' materializes a string with the runtime's
;; `nl_alloc_str' and leaves the only reference to it in the payload word
;; of a raw Sexp slot.  The GC does not scan raw buffers, so before
;; `nelisp-native-load--slot-pins' a mid-form collection at a `while'
;; back-edge (or `garbage-collect') freed the string while the slot still
;; pointed at it: on the unfixed loader about 184 of 200 strings came back
;; wrong with no explicit GC at all, and 200 of 200 after one.
;; `nelisp-native-load--make-scratch-vector' has the same gap: the loader's
;; slots are mmap'd, outside the arena, so the scratch vector's box was
;; reclaimed (49 of 50 on a3f79db87) until it was pinned the same way.
;; Run by `make standalone-reader-native-load-slot-pin-smoke', which loads
;; lisp/nelisp-native-load.el first.

;;; Code:

(defun native-load-slot-pin--name (i)
  (format "slot-pin-%04d-abcdefghijklmnopqrstuvwxyz" i))

(defun native-load-slot-pin--box-all (n)
  (let ((i 0) (cells nil))
    (while (< i n)
      (let ((cell (alloc-bytes 32 8)))
        (nelisp-native-load-box cell (native-load-slot-pin--name i))
        (setq cells (cons cell cells)))
      (setq i (1+ i)))
    (nreverse cells)))

(defun native-load-slot-pin--bad (cells)
  (let ((i 0) (bad 0))
    (while cells
      (unless (equal (condition-case nil
                         (nelisp-native-load-unbox (car cells))
                       (error :error))
                     (native-load-slot-pin--name i))
        (setq bad (1+ bad)))
      (setq cells (cdr cells) i (1+ i)))
    bad))

(defun native-load-slot-pin--churn (n)
  (let ((i 0) (keep nil))
    (while (< i n)
      (let ((cell (alloc-bytes 32 8)))
        (nelisp-native-load-box cell "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz")
        (setq keep (cons cell keep)))
      (setq i (1+ i)))
    (length keep)))

(defun native-load-slot-pin--make-vectors (pin)
  "Make 50 scratch vectors held only by an mmap'd page; return the page.
With PIN non-nil they come from the loader (and are pinned).  With PIN nil
they are allocated the same way and never pinned: the positive control."
  (let ((page (nelisp-native-load--mmap 4096 nil)) (k 0))
    (while (< k 50)
      (if pin
          (nelisp-native-load--make-scratch-vector (+ page (* 32 k)))
        (let ((box (ptr-call (nelisp-native-load--symbol-addr "nl_alloc_vector")
                             nelisp-native-load-scratch-slots 0 0 0 0 0)))
          (ptr-write-u64 (+ page (* 32 k)) 0 nelisp-native-load-tag-vector)
          (ptr-write-u64 (+ page (* 32 k)) 8 box)))
      (setq k (1+ k)))
    page))

(defun native-load-slot-pin--drain-free-space ()
  "Allocate and poison 300000 32-byte blocks (about 0.5 s).
A freed block is only overwritten once an allocation takes it, and the
earlier string phases leave many freed 32-byte blocks that are taken
first, so the 20000 allocations the first version of this check used
were a coin flip (red 3 s in one CI run, green in the next).  Orders of
magnitude more than the free space of this process makes the outcome
independent of the heap layout; the positive control in the caller proves
that it did reach the freed vectors."
  (let ((i 0) (keep nil))
    (while (< i 300000)
      (let ((b (alloc-bytes 32 8)))
        (ptr-write-u64 b 0 99)
        (ptr-write-u64 b 8 99)
        (setq keep (cons b keep)))
      (setq i (1+ i)))
    i))

(defun native-load-slot-pin--clobbered (page)
  "How many of the 50 vectors in PAGE no longer look like fresh scratch vectors.
Reads the header (element count from bit 8) and the first elements, which a
fresh scratch vector holds as nil (word 3)."
  (let ((k 0) (bad 0))
    (while (< k 50)
      (let ((box (ptr-read-u64 (+ page (* 32 k)) 8)))
        (when (or (/= (ash (ptr-read-u64 box 0) -8)
                      nelisp-native-load-scratch-slots)
                  (/= (ptr-read-u64 box 8) 3)
                  (/= (ptr-read-u64 box 16) 3))
          (setq bad (1+ bad))))
      (setq k (1+ k)))
    bad))

(let* ((n 200)
       (first (native-load-slot-pin--box-all n))
       (first-bad (native-load-slot-pin--bad first))
       (second (native-load-slot-pin--box-all n))
       (_ (garbage-collect))
       (_ (native-load-slot-pin--churn (* 4 n)))
       (second-bad (native-load-slot-pin--bad second))
       (control-page (native-load-slot-pin--make-vectors nil))
       (pinned-page (native-load-slot-pin--make-vectors t))
       (_ (garbage-collect))
       (_ (native-load-slot-pin--drain-free-space))
       (control-bad (native-load-slot-pin--clobbered control-page))
       (scratch-bad (native-load-slot-pin--clobbered pinned-page)))
  ;; Positive control: unpinned vectors must be seen freed and overwritten
  ;; (a few may survive as conservative-root noise), else this check is blind.
  (when (< control-bad 40)
    (signal 'error
            (list (format "native-load-slot-pin: FAIL control: only %d/50 unpinned scratch vectors were seen freed, so the check cannot detect a freed vector"
                          control-bad))))
  (if (= 0 (+ first-bad second-bad scratch-bad))
      (princ (format "native-load-slot-pin: PASS (%d strings, 50 scratch vectors, control %d/50 freed)\n" n control-bad))
    (signal 'error
            (list (format "native-load-slot-pin: FAIL %d/%d wrong under mid-form GC, %d/%d after garbage-collect, %d/50 scratch vectors freed (control %d/50)"
                          first-bad n second-bad n scratch-bad control-bad)))))

;;; standalone-native-load-slot-pin-smoke.el ends here
