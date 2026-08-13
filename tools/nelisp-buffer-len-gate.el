;;; nelisp-buffer-len-gate.el --- build-time (ptr,len) agreement check -*- lexical-binding: t; -*-

;; The runtime hands raw (pointer, length) pairs to `nl_alloc_symbol' and
;; `nl_alloc_str'.  Nothing ties the length to the buffer it describes, so a
;; buffer that is allocated (or filled) with fewer bytes than the length says
;; is read past its end.  That defect is not hypothetical: the docstring of
;; `nelisp-standalone--name-words' records it verbatim --
;;
;;   "the stock single-u64 buffer corrupts names > 8 bytes because
;;    nl_alloc_symbol reads `name_len' bytes from an 8-byte buffer"
;;
;; and the `nl_alloc_symbol' NUL guard has been catching the same shape at
;; runtime since 2026-08-04 (a correct name plus 3-4 bytes of its neighbour).
;;
;; Rust expresses the tie in the type system: a `&[u8]' cannot be built with a
;; length its allocation does not cover.  This file recovers the same property
;; where it is decidable -- syntactically, inside one `let'/`let*' body:
;;
;;   (let ((buf (alloc-bytes SIZE 1)))
;;     (ptr-write-u64 buf 0 ...)          ; covers bytes [0,8)
;;     (ptr-write-u64 (+ buf 8) 0 ...)    ; covers bytes [8,16)
;;     (nl_alloc_symbol buf LEN slot))    ; must have LEN <= 16
;;
;; Two independent bounds are asserted when both sides are statically known:
;;
;;   CAPACITY  LEN <= SIZE          -- reading past the allocation
;;   COVERAGE  LEN <= filled bytes  -- reading past what was actually written
;;
;; Deliberately conservative.  A buffer is only judged when every use of it in
;; scope is understood: literal-offset `ptr-write-u8'/`ptr-write-u64' stores
;; and the terminating (ptr,len) call.  A buffer touched by a loop, a helper
;; call, or an unmodelled form is SKIPPED, because a partial view of the fill
;; would report a buffer as under-filled when it is not.  Skips are counted and
;; printed: "0 violations" out of 0 buffers examined is not a result.

;;; Code:

(require 'cl-lib)

(defconst nelisp-buffer-len-gate--sinks
  '(nl_alloc_symbol nl_alloc_str)
  "Calls of the form (SINK BUF LEN SLOT) whose LEN must be covered by BUF.")

(defvar nelisp-buffer-len-gate--checked 0)
(defvar nelisp-buffer-len-gate--skipped 0)
(defvar nelisp-buffer-len-gate--violations nil)
(defconst nelisp-buffer-len-gate--symbol-tag 4
  "SEXP_TAG_SYMBOL.  A source-level `(ptr-write-u8 X 0 4)' makes a Symbol.")

(defconst nelisp-buffer-len-gate--tag4-allowlist
  '(nl_alloc_symbol_write
    nl_intern_write_sexp
    nelisp_reader_p_build_head_symbol)
  "Functions allowed to stamp SEXP_TAG_SYMBOL directly.

This list is the point of the rule.  On 2026-08-12 a whole day's conclusions
rested on \"both symbol producers keep cap == len\"; there were six stores, not
two, and the two unexamined ones (the reader's finalize-then-retag path) break
that invariant by design -- `nl_mut_str_finalize_write' writes cap as the
allocation size, not the name length.  Any premise of the form \"every symbol
is built this way\" is only as good as the producer census behind it, so the
census is asserted at build time instead of re-counted by hand.

Adding a producer is fine.  Adding one WITHOUT updating this list, and thereby
silently invalidating whatever a reader has assumed about symbols, is not.")

(defvar nelisp-buffer-len-gate--tag4-sites nil
  "Alist of (FUNCTION . COUNT) for each source-level Symbol-tag store.")

(defvar nelisp-buffer-len-gate--defun nil
  "Name of the `defun' currently being walked, or nil at top level.")

(defvar nelisp-buffer-len-gate--sink-calls 0
  "Every (SINK BUF LEN SLOT) call seen, analysable or not.
Printed as the denominator: \"0 violations\" is only meaningful next to how
many of the tree's call sites this checker was actually able to judge.")

(defun nelisp-buffer-len-gate--alloc-size (form)
  "Return the byte size FORM allocates, or nil when it is not an allocation.
The size is an integer, or (SYM . K) meaning the symbolic size SYM + K --
enough to compare `(alloc-bytes (+ n 36) 1)' against a length of `(+ n 36)'."
  (pcase form
    (`(alloc-bytes ,size ,_align)
     (pcase size
       ((pred integerp) size)
       (`(+ ,(and s (pred symbolp)) ,(and k (pred integerp))) (cons s k))
       (`(+ ,(and k (pred integerp)) ,(and s (pred symbolp))) (cons s k))
       (_ 'unknown)))
    (_ nil)))

(defun nelisp-buffer-len-gate--len-value (form)
  "Return LEN in the same representation as `nelisp-buffer-len-gate--alloc-size'."
  (pcase form
    ((pred integerp) form)
    (`(+ ,(and s (pred symbolp)) ,(and k (pred integerp))) (cons s k))
    (`(+ ,(and k (pred integerp)) ,(and s (pred symbolp))) (cons s k))
    (_ 'unknown)))

(defun nelisp-buffer-len-gate--le (len size)
  "Return non-nil when LEN <= SIZE is statically provable, nil when unknown."
  (cond
   ((or (eq len 'unknown) (eq size 'unknown)) 'unknown)
   ((and (integerp len) (integerp size)) (<= len size))
   ;; Same symbolic base: compare the constant parts only.
   ((and (consp len) (consp size) (eq (car len) (car size)))
    (<= (cdr len) (cdr size)))
   (t 'unknown)))

(defun nelisp-buffer-len-gate--store-extent (form buf)
  "Return the byte offset one past what FORM writes into BUF, or nil.
Returns the symbol `opaque' when FORM mentions BUF in a way this checker
cannot account for."
  (pcase form
    (`(ptr-write-u64 ,dst ,off ,_v)
     (let ((base (pcase dst
                   ((pred symbolp) (and (eq dst buf) 0))
                   (`(+ ,(pred (lambda (x) (eq x buf))) ,(and k (pred integerp))) k)
                   (_ nil))))
       (cond ((and base (integerp off)) (+ base off 8))
             (base 'opaque)
             (t nil))))
    (`(ptr-write-u8 ,dst ,off ,_v)
     (let ((base (pcase dst
                   ((pred symbolp) (and (eq dst buf) 0))
                   (`(+ ,(pred (lambda (x) (eq x buf))) ,(and k (pred integerp))) k)
                   (_ nil))))
       (cond ((and base (integerp off)) (+ base off 1))
             (base 'opaque)
             (t nil))))
    (_ nil)))

(defun nelisp-buffer-len-gate--mentions-p (form sym)
  "Return non-nil when SYM appears anywhere in FORM.
Iterative along the list spine: source files hold thousand-element string
lists, and recursing down every `cdr' exhausts `max-lisp-eval-depth'."
  (let ((stack (list form)) (found nil))
    (while (and stack (not found))
      (let ((f (pop stack)))
        (cond ((eq f sym) (setq found t))
              ((consp f)
               (while (consp f)
                 (push (car f) stack)
                 (setq f (cdr f)))
               (when f (push f stack))))))
    found))

(defun nelisp-buffer-len-gate--flatten-seq (body)
  "Return BODY's forms with `seq'/`progn'/`and' wrappers flattened one level."
  (apply #'append
         (mapcar (lambda (f)
                   (if (and (consp f) (memq (car f) '(seq progn and))
                            (proper-list-p (cdr f)))
                       (nelisp-buffer-len-gate--flatten-seq (cdr f))
                     (list f)))
                 (if (proper-list-p body) body nil))))

(defun nelisp-buffer-len-gate--check-buffer (buf size body file)
  "Check every sink call on BUF inside BODY against SIZE."
  (let ((forms (nelisp-buffer-len-gate--flatten-seq body))
        (covered 0) (opaque nil) (sinks nil))
    (dolist (f forms)
      (let ((ext (nelisp-buffer-len-gate--store-extent f buf)))
        (cond
         ((eq ext 'opaque) (setq opaque t))
         ((integerp ext) (setq covered (max covered ext)))
         ;; A sink call is the terminating use; record it.
         ((and (consp f) (memq (car f) nelisp-buffer-len-gate--sinks)
               (eq (nth 1 f) buf))
          (push (cons (car f) (nelisp-buffer-len-gate--len-value (nth 2 f))) sinks))
         ;; Any other mention of BUF means the fill is not fully modelled.
         ((nelisp-buffer-len-gate--mentions-p f buf) (setq opaque t)))))
    (dolist (s (nreverse sinks))
      (let* ((sink (car s)) (len (cdr s))
             (cap-ok (nelisp-buffer-len-gate--le len size)))
        (cond
         ((eq cap-ok 'unknown)
          (cl-incf nelisp-buffer-len-gate--skipped))
         ((null cap-ok)
          (cl-incf nelisp-buffer-len-gate--checked)
          (push (format "%s: CAPACITY %s reads %s from a %s-byte buffer `%s'"
                        file sink len size buf)
                nelisp-buffer-len-gate--violations))
         (opaque
          ;; Capacity proved, coverage not modellable.
          (cl-incf nelisp-buffer-len-gate--checked))
         ((and (integerp len) (> len covered))
          (cl-incf nelisp-buffer-len-gate--checked)
          (push (format "%s: COVERAGE %s reads %d bytes from `%s' but only %d were written"
                        file sink len buf covered)
                nelisp-buffer-len-gate--violations))
         (t (cl-incf nelisp-buffer-len-gate--checked)))))))

(defun nelisp-buffer-len-gate--walk (form file)
  "Walk FORM looking for let-bound allocations feeding a sink."
  (when (consp form)
    ;; `(let . 31)' occurs as an alist pair in the IR tag table -- `nth' on an
    ;; improper list throws, so require a proper list before destructuring.
    (when (and (memq (car-safe form) '(let let*)) (proper-list-p form))
      (let ((bindings (nth 1 form))
            (body (cddr form)))
        (when (and (proper-list-p bindings) (proper-list-p body))
          (dolist (b bindings)
            (when (and (consp b) (symbolp (car b)))
              (let ((size (nelisp-buffer-len-gate--alloc-size (nth 1 b))))
                (when size
                  (nelisp-buffer-len-gate--check-buffer (car b) size body file))))))))
    (when (memq (car-safe form) nelisp-buffer-len-gate--sinks)
      (cl-incf nelisp-buffer-len-gate--sink-calls))
    (pcase form
      (`(ptr-write-u8 ,_dst 0 ,(pred (lambda (v)
                                       (equal v nelisp-buffer-len-gate--symbol-tag))))
       (let* ((fn (or nelisp-buffer-len-gate--defun '<toplevel>))
              (cell (assq fn nelisp-buffer-len-gate--tag4-sites)))
         (if cell (cl-incf (cdr cell))
           (push (cons fn 1) nelisp-buffer-len-gate--tag4-sites))))
      (_ nil))
    ;; Descend into `car's recursively but follow the `cdr' spine in a loop:
    ;; source files hold long literal lists and recursing per element blows
    ;; `max-lisp-eval-depth'.  Dotted tails are handled by the `consp' test.
    (let ((nelisp-buffer-len-gate--defun
           ;; `(defun . 21)' is an alist pair in the IR tag table, so the
           ;; proper-list test has to come before `nth'.
           (if (and (eq (car-safe form) 'defun) (proper-list-p form)
                    (symbolp (nth 1 form)) (nth 1 form))
               (nth 1 form)
             nelisp-buffer-len-gate--defun))
          (tail form))
      (while (consp tail)
        (nelisp-buffer-len-gate--walk (car tail) file)
        (setq tail (cdr tail))))))

(defun nelisp-buffer-len-gate-run (files)
  "Check FILES and return the number of violations found."
  (setq nelisp-buffer-len-gate--checked 0
        nelisp-buffer-len-gate--skipped 0
        nelisp-buffer-len-gate--sink-calls 0
        nelisp-buffer-len-gate--tag4-sites nil
        nelisp-buffer-len-gate--violations nil)
  (dolist (file files)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((short (file-name-nondirectory file)))
        (condition-case nil
            (while t
              (nelisp-buffer-len-gate--walk (read (current-buffer)) short))
          (end-of-file nil)
          (invalid-read-syntax nil)))))
  ;; Producer census: report both directions, so a producer that DISAPPEARS
  ;; (leaving a stale allowlist entry that would wave through its replacement)
  ;; is as visible as one that appears.
  (let ((found (mapcar #'car nelisp-buffer-len-gate--tag4-sites)))
    (dolist (fn found)
      (unless (memq fn nelisp-buffer-len-gate--tag4-allowlist)
        (push (format "UNLISTED symbol-tag producer `%s' (%d store(s)) -- add it to %s and recheck every premise of the form \"all symbols are built like X\""
                      fn (cdr (assq fn nelisp-buffer-len-gate--tag4-sites))
                      'nelisp-buffer-len-gate--tag4-allowlist)
              nelisp-buffer-len-gate--violations)))
    (dolist (fn nelisp-buffer-len-gate--tag4-allowlist)
      (unless (memq fn found)
        (push (format "STALE allowlist entry `%s': no symbol-tag store found" fn)
              nelisp-buffer-len-gate--violations)))
    (message "[buffer-len-gate] symbol-tag producers: %s"
             (mapconcat (lambda (c) (format "%s x%d" (car c) (cdr c)))
                        (reverse nelisp-buffer-len-gate--tag4-sites) ", ")))
  ;; Assign the reversal back: `nreverse' leaves the original head pointing at
  ;; the LAST cell, so counting the unreversed variable afterwards under-reports
  ;; (it read 1 while three violations were printed).
  (setq nelisp-buffer-len-gate--violations
        (nreverse nelisp-buffer-len-gate--violations))
  (dolist (v nelisp-buffer-len-gate--violations)
    (message "VIOLATION %s" v))
  (message (concat "[buffer-len-gate] sink-calls=%d judged=%d"
                   " skipped-unmodelled=%d skipped-unknown-len=%d violations=%d")
           nelisp-buffer-len-gate--sink-calls
           nelisp-buffer-len-gate--checked
           (- nelisp-buffer-len-gate--sink-calls
              nelisp-buffer-len-gate--checked
              nelisp-buffer-len-gate--skipped)
           nelisp-buffer-len-gate--skipped
           (length nelisp-buffer-len-gate--violations))
  (length nelisp-buffer-len-gate--violations))

(provide 'nelisp-buffer-len-gate)
;;; nelisp-buffer-len-gate.el ends here
