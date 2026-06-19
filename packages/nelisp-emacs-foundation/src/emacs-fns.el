;;; emacs-fns.el --- NeLisp port of Emacs C core fns.c primitives  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Phase 1.6 — Layer 2 (= Emacs C core in Elisp on NeLisp).
;;
;; Ports the standard sequence + property-list primitives that
;; `fns.c' provides in Emacs's C core.  These are foundation
;; functions every Elisp library assumes; they cannot be collapsed into
;; the NeLisp core without violating the "minimal substrate" rule
;; (user 2026-05-02 directive), and they cannot live in any single
;; application (= anvil.el, etc.) without forcing every other
;; nelisp-emacs consumer to duplicate them.
;;
;; Each definition is gated on `unless (fboundp ...)` so loading
;; this file under regular Emacs (= where the real C primitives
;; already exist) is a cheap no-op.  Implementations use only
;; bootstrap-eval primitives (no dependency on the very functions
;; being defined here, no `cl-lib', no `subr-x' tricks).
;;
;; Symbols ported: mapcar, mapconcat, mapc, nreverse, reverse,
;; plist-get, plist-put, plist-member, provide.
;;
;; Out of scope here: cl-* generic versions (= live in
;; `nelisp-emacs/src/emacs-cl-seq.el', not yet shipped).  Hash
;; table, string, and number primitives ship in their own
;; emacs-X.el files.

;;; Code:

;;;; --- trivial primitives -----------------------------------------------

;; Emacs's C primitive accepts an optional SUBFEATURES argument:
;; `(provide 'files '(remote-wildcards))' appears in vendored files.el.
;; NeLisp's standalone prelude may expose `provide' / `featurep' before it
;; has created the user-visible `features' variable.  Define the registry
;; at top level first: some standalone eval paths do not reliably handle
;; `defvar' inside a function body before the following `setq'.
(unless (boundp 'features)
  (defvar features nil))

;; NeLisp v2's bootstrap stdlib used to expose a one-argument `provide',
;; so vendor `require' could load a file and still report "feature not
;; provided" after arity failure.  Host Emacs keeps its native primitive;
;; the polyfill is only installed on the standalone NeLisp path, before
;; `emacs-version' exists.
(when (or (fboundp 'nl-write-file)
          (not (boundp 'emacs-version))
          (not (stringp emacs-version)))
  (defun provide (feature &optional _subfeatures)
    "Mark FEATURE as available and return FEATURE.
Optional SUBFEATURES are accepted for Emacs compatibility and ignored."
    (unless (memq feature features)
      (setq features (cons feature features)))
    feature)

  (defun featurep (feature &optional _subfeature)
    "Return non-nil if FEATURE has been provided.
Optional SUBFEATURE is accepted for Emacs compatibility and ignored."
    (if (memq feature features) t nil))

  (defun locate-file (filename path &optional suffixes predicate)
    "Find FILENAME in PATH using optional SUFFIXES and PREDICATE.
This standalone implementation covers the `require' and batch-test
lookup path: PATH is a list of directories, SUFFIXES may be nil, a
string, or a list of strings, and PREDICATE defaults to `file-exists-p'."
    (let ((suffix-list (cond
                        ((null suffixes) (list ""))
                        ((stringp suffixes) (list suffixes))
                        (t suffixes)))
          (dirs path)
          (found nil))
      (while (and dirs (not found))
        (let ((suffixes-left suffix-list))
          (while (and suffixes-left (not found))
            (let ((candidate
                   (expand-file-name
                    (concat filename (car suffixes-left))
                    (car dirs))))
              (when (if predicate
                        (funcall predicate candidate)
                      (file-exists-p candidate))
                (setq found candidate)))
            (setq suffixes-left (cdr suffixes-left))))
        (setq dirs (cdr dirs)))
      found))

  (defun require (feature &optional filename noerror)
    "Load FEATURE through `load-path' unless it is already provided.
FILENAME and NOERROR follow the common Emacs `require' surface used by
batch tests and local runtime modules."
    (if (featurep feature)
        feature
      (let* ((base (or filename (symbol-name feature)))
             (path (or (and (stringp base)
                            (file-exists-p base)
                            base)
                       (and (boundp 'load-path)
                            (locate-file base load-path (list ".el" ""))))))
        (cond
         (path
          (load path nil 'no-message)
          (cond
           ((featurep feature) feature)
           (noerror nil)
           (t (error "Required feature was not provided: %S" feature))))
         (noerror nil)
         (t (error "Cannot open load file: %S" feature)))))))

(when (and (fboundp 'rdf)
           (not (fboundp 'nl-syscall-read-file)))
  (defun nl-syscall-read-file (filename &optional beg end)
    "Read FILENAME through the standalone `rdf' primitive.
BEG and END are byte offsets accepted for compatibility with the
newer file I/O runtime surface.  The current `rdf' backend reads the
whole file, then this shim slices the string when offsets are supplied."
    (let* ((text (rdf filename))
           (len (and (stringp text) (length text)))
           (from (or beg 0))
           (to (or end len)))
      (if (not (stringp text))
          ""
        (if (or beg end)
            (substring text from to)
          text)))))

(unless (fboundp 'ignore)
  (defun ignore (&rest _ignore-args)
    "Polyfill: do nothing, return nil regardless of arguments."
    nil))

(unless (fboundp 'identity)
  (defun identity (arg)
    "Polyfill: return ARG unchanged."
    arg))

(unless (fboundp 'null)
  (defun null (object)
    "Polyfill: return t iff OBJECT is nil."
    (eq object nil)))

;; Some NeLisp eval paths look up function symbols as values too;
;; defvar them as nil so `symbol-value' / bare-symbol-eval succeed.
(defvar null nil
  "Polyfill alias of nil — works around NeLisp eval paths that fall
back to `symbol-value' lookup for symbols whose function cell is
bound but value cell is unbound.")

(unless (fboundp 'numberp)
  (defun numberp (obj) (or (integerp obj) (floatp obj))))

(unless (fboundp 'make-bool-vector)
  (defun make-bool-vector (length init)
    "Polyfill: return a boolean vector of LENGTH initialized to INIT.
Standalone NeLisp does not need bit-packed storage for editor dirty
sets; a normal vector preserves the indexing semantics used here."
    (make-vector length (and init t))))

(unless (fboundp 'bool-vector-p)
  (defun bool-vector-p (object)
    "Polyfill predicate for `make-bool-vector' values."
    (vectorp object)))


;;;; --- list iteration -----------------------------------------------------

(unless (fboundp 'mapcar)
  (defun mapcar (function sequence)
    "Apply FUNCTION to each element of SEQUENCE, return list of results.
SEQUENCE here is restricted to a proper list (= terminated by nil).
A vector-aware port belongs in `emacs-fns-seq.el' (Phase 2)."
    (let ((result nil)
          (cur sequence))
      (while cur
        (setq result (cons (funcall function (car cur)) result))
        (setq cur (cdr cur)))
      ;; Manual reverse — `nreverse' may not yet be defined when the
      ;; loader installs this file before its reverse primitive.
      (let ((reversed nil))
        (while result
          (setq reversed (cons (car result) reversed))
          (setq result (cdr result)))
        reversed))))

(unless (fboundp 'mapc)
  (defun mapc (function sequence)
    "Apply FUNCTION to each element of SEQUENCE for side effects.
Returns SEQUENCE unchanged."
    (let ((cur sequence))
      (while cur
        (funcall function (car cur))
        (setq cur (cdr cur))))
    sequence))

(unless (fboundp 'mapconcat)
  (defun mapconcat (function sequence separator)
    "Apply FUNCTION to each element of SEQUENCE, concatenate with SEPARATOR.
Each FUNCTION result must be a string; SEPARATOR is a string.  Returns
the empty string when SEQUENCE is nil (matches Emacs C behaviour)."
    (if (null sequence)
        ""
      (let ((parts nil)
            (cur sequence))
        (while cur
          (setq parts (cons (funcall function (car cur)) parts))
          (setq cur (cdr cur)))
        ;; parts is reverse-order; build forward list, then concat.
        (let ((forward nil))
          (while parts
            (setq forward (cons (car parts) forward))
            (setq parts (cdr parts)))
          ;; Interleave SEPARATOR.
          (let ((out (car forward))
                (rest (cdr forward)))
            (while rest
              (setq out (concat out separator (car rest)))
              (setq rest (cdr rest)))
            out))))))


;;;; --- list reversal ------------------------------------------------------

(unless (fboundp 'reverse)
  (defun reverse (sequence)
    "Return a new list with the elements of SEQUENCE in reverse order.
Does NOT mutate SEQUENCE.  Proper-list only (vector port: Phase 2)."
    (let ((result nil)
          (cur sequence))
      (while cur
        (setq result (cons (car cur) result))
        (setq cur (cdr cur)))
      result)))

(unless (fboundp 'nreverse)
  (defun nreverse (sequence)
    "Return SEQUENCE reversed.  In Emacs this destructively
re-uses the cons cells; the polyfill here behaves identically as
far as the return value is concerned but allocates a fresh list,
because mutating cons cells from Lisp without `setcdr' availability
would be unsafe.  Callers that depend on the original SEQUENCE
becoming garbage should not be affected because the original list
is no longer reachable through the variable they used to bind it."
    (reverse sequence)))


;;;; --- property list access -----------------------------------------------

(unless (fboundp 'plist-get)
  (defun plist-get (plist property)
    "Return the value of PROPERTY in PLIST.
PLIST is a flat alternating-key/value list `(KEY1 VAL1 KEY2 VAL2 ...)'.
Comparison uses `eq' (Emacs default).  Returns nil when PROPERTY is
absent — caller must distinguish nil-as-value from missing-property
using `plist-member'."
    (let ((cur plist)
          (found nil)
          (result nil))
      (while (and cur (not found))
        (if (eq (car cur) property)
            (progn (setq result (car (cdr cur)))
                   (setq found t))
          (setq cur (cdr (cdr cur)))))
      result)))

(unless (fboundp 'plist-member)
  (defun plist-member (plist property)
    "Return the cdr cell whose car is PROPERTY in PLIST, or nil.
The returned cell is the (PROPERTY VAL ...) sub-list, not just the
value; callers can distinguish missing from nil-valued via this."
    (let ((cur plist)
          (found nil))
      (while (and cur (not found))
        (if (eq (car cur) property)
            (setq found cur)
          (setq cur (cdr (cdr cur)))))
      found)))

(unless (fboundp 'plist-put)
  (defun plist-put (plist property value)
    "Change the value of PROPERTY in PLIST to VALUE; return the modified PLIST.
If PROPERTY is absent, append (PROPERTY VALUE) to PLIST.  This polyfill
returns a fresh list rather than mutating in place — callers that depend
on identity should re-bind the variable holding PLIST."
    (let ((acc nil)
          (cur plist)
          (replaced nil))
      ;; Walk PLIST in pairs, copying.  Replace VALUE when key matches.
      (while cur
        (let ((k (car cur))
              (v (car (cdr cur))))
          (if (eq k property)
              (progn (setq acc (cons v (cons k acc)))
                     (setq replaced t))
            (setq acc (cons v (cons k acc)))))
        (setq cur (cdr (cdr cur))))
      ;; Reverse acc back to forward order.
      (let ((forward nil))
        (while acc
          (setq forward (cons (car acc) forward))
          (setq acc (cdr acc)))
        (if replaced
            forward
          ;; Append fresh (PROPERTY VALUE).
          (let ((tail (cons property (cons value nil))))
            (if (null forward)
                tail
              ;; Build (forward... PROPERTY VALUE).  No `append' dependency.
              (let ((out nil)
                    (rev nil))
                ;; First copy forward into out via reversal.
                (let ((c forward))
                  (while c
                    (setq rev (cons (car c) rev))
                    (setq c (cdr c))))
                ;; Now reverse rev into out, prepending tail.
                (setq out tail)
                (while rev
                  (setq out (cons (car rev) out))
                  (setq rev (cdr rev)))
                out))))))))


;;;; --- coding-system polyfill (Doc 51 Track B Phase 2) ----------------
;;
;; Under host Emacs `encode-coding-string' / `decode-coding-string' /
;; `multibyte-string-p' are C builtins.  Under the nelisp driver strings
;; are internally valid UTF-8, so
;; for `'utf-8' / `'utf-8-emacs' / `nil' (= no conversion) the encode/
;; decode operations are identity.  We provide minimal polyfills here
;; because `nelisp-text-buffer.el' calls them at runtime and we are
;; loaded before that file's functions are first invoked.

(unless (fboundp 'encode-coding-string)
  (defun encode-coding-string (string _coding-system &optional _nocopy &rest _)
    (if (stringp string) string "")))

(unless (fboundp 'decode-coding-string)
  (defun decode-coding-string (string _coding-system &optional _nocopy &rest _)
    (if (stringp string) string "")))

(unless (fboundp 'multibyte-string-p)
  (defun multibyte-string-p (string)
    (when (stringp string)
      (let ((i 0) (n (length string)) found)
        (while (and (not found) (< i n))
          (when (>= (aref string i) 128) (setq found t))
          (setq i (1+ i)))
        found))))

(unless (fboundp 'string-as-multibyte)
  (defun string-as-multibyte (string)
    (if (stringp string) string "")))

(unless (fboundp 'string-as-unibyte)
  (defun string-as-unibyte (string)
    (if (stringp string) string "")))

(unless (fboundp 'string-make-multibyte)
  (defalias 'string-make-multibyte 'string-as-multibyte))

(unless (fboundp 'string-make-unibyte)
  (defalias 'string-make-unibyte 'string-as-unibyte))

(unless (fboundp 'vconcat)
  (defun vconcat (&rest sequences)
    (apply 'vector (apply 'append (mapcar (lambda (s) (append s nil)) sequences)))))
(unless (fboundp 'delete-dups)
  (defun delete-dups (list)
    (let ((tail list))
      (while tail
        (setcdr tail (let ((rest (cdr tail)) (kept nil))
                       (while rest
                         (unless (equal (car tail) (car rest))
                           (setq kept (cons (car rest) kept)))
                         (setq rest (cdr rest)))
                       (nreverse kept)))
        (setq tail (cdr tail))))
    list))

;;;; --- standalone TTY raw/input polyfill ---------------------------------

(when (and (or (fboundp 'nl-write-file)
               (not (boundp 'emacs-version))
               (not (stringp emacs-version)))
           (fboundp 'syscall-direct)
           (fboundp 'alloc-bytes)
           (fboundp 'ptr-read-u8)
           (fboundp 'ptr-read-u32)
           (fboundp 'ptr-read-u64)
           (fboundp 'ptr-write-u8)
           (fboundp 'ptr-write-u32)
           (fboundp 'ptr-write-u64))
  (defvar terminal-raw-mode--fd nil)
  (defvar terminal-raw-mode--saved-termios nil)
  (defvar terminal-raw-mode--scratch-termios nil)
  (defvar terminal-raw-mode--pollfd nil)
  (defvar terminal-raw-mode--byte nil)
  (defvar terminal-raw-mode--winsize nil)
  (defvar terminal-raw-mode--dev-tty nil)
  (defvar terminal-raw-mode--active nil)
  (defvar terminal-raw-mode--winsize-changed nil)

  (defun terminal-raw-mode--ensure-buf (name size align)
    (let ((ptr (and (boundp name) (symbol-value name))))
      (if ptr
          ptr
        (let ((fresh (alloc-bytes size align)))
          (set name fresh)
          fresh))))

  (defun terminal-raw-mode--dev-tty-path ()
    (let ((buf (terminal-raw-mode--ensure-buf
                'terminal-raw-mode--dev-tty 9 1)))
      ;; "/dev/tty\0".  Write bytes individually because standalone
      ;; interpreter integer precision is not yet reliable for this u64.
      (ptr-write-u8 buf 0 47)
      (ptr-write-u8 buf 1 100)
      (ptr-write-u8 buf 2 101)
      (ptr-write-u8 buf 3 118)
      (ptr-write-u8 buf 4 47)
      (ptr-write-u8 buf 5 116)
      (ptr-write-u8 buf 6 116)
      (ptr-write-u8 buf 7 121)
      (ptr-write-u8 buf 8 0)
      buf))

  (defun terminal-raw-mode--copy-termios (src dst)
    (ptr-write-u64 dst 0 (ptr-read-u64 src 0))
    (ptr-write-u64 dst 8 (ptr-read-u64 src 8))
    (ptr-write-u64 dst 16 (ptr-read-u64 src 16))
    (ptr-write-u64 dst 24 (ptr-read-u64 src 24))
    (ptr-write-u64 dst 32 (ptr-read-u64 src 32))
    (ptr-write-u64 dst 40 (ptr-read-u64 src 40))
    (ptr-write-u64 dst 48 (ptr-read-u64 src 48))
    (ptr-write-u32 dst 56 (ptr-read-u32 src 56))
    0)

  (defun terminal-raw-mode--make-raw (buf)
    ;; Linux x86_64 termios layout.  This mirrors cfmakeraw plus VMIN/VTIME.
    (ptr-write-u32 buf 0 (logand (ptr-read-u32 buf 0) 4294965780))
    (ptr-write-u32 buf 4 (logand (ptr-read-u32 buf 4) 4294967294))
    (ptr-write-u32 buf 8
                   (logior (logand (ptr-read-u32 buf 8) 4294966991) 48))
    (ptr-write-u32 buf 12 (logand (ptr-read-u32 buf 12) 4294934452))
    (ptr-write-u8 buf 22 0)
    (ptr-write-u8 buf 23 1)
    0)

  (defun terminal-raw-mode-enter ()
    (if terminal-raw-mode--active
        t
      (let* ((fd (syscall-direct 2 (terminal-raw-mode--dev-tty-path)
                                 2 0 0 0 0))
             (scratch (terminal-raw-mode--ensure-buf
                       'terminal-raw-mode--scratch-termios 60 4))
             (saved (terminal-raw-mode--ensure-buf
                     'terminal-raw-mode--saved-termios 60 4)))
        (if (< fd 0)
            nil
          (if (< (syscall-direct 16 fd 21505 scratch 0 0 0) 0)
              (progn (syscall-direct 3 fd 0 0 0 0 0) nil)
            (terminal-raw-mode--copy-termios scratch saved)
            (terminal-raw-mode--make-raw scratch)
            (if (< (syscall-direct 16 fd 21506 scratch 0 0 0) 0)
                (progn (syscall-direct 3 fd 0 0 0 0 0) nil)
              (setq terminal-raw-mode--fd fd)
              (setq terminal-raw-mode--active t)
              t))))))

  (defun terminal-raw-mode-leave ()
    (if (not terminal-raw-mode--active)
        nil
      (let ((fd terminal-raw-mode--fd)
            (saved terminal-raw-mode--saved-termios))
        (when (and fd saved)
          (syscall-direct 16 fd 21506 saved 0 0 0)
          (when (> fd 2)
            (syscall-direct 3 fd 0 0 0 0 0)))
        (setq terminal-raw-mode--active nil)
        (setq terminal-raw-mode--fd nil)
        t)))

  (defun read-stdin-byte-available (timeout-ms)
    (let ((fd (or terminal-raw-mode--fd 0))
          (pfd (terminal-raw-mode--ensure-buf 'terminal-raw-mode--pollfd 8 4))
          (byte (terminal-raw-mode--ensure-buf 'terminal-raw-mode--byte 1 1)))
      (ptr-write-u32 pfd 0 fd)
      (ptr-write-u32 pfd 4 1)
      (let ((rc (syscall-direct 7 pfd 1 timeout-ms 0 0 0)))
        (if (< rc 1)
            nil
          (if (= (logand (ptr-read-u8 pfd 6) 17) 0)
              nil
            (if (= (syscall-direct 0 fd byte 1 0 0 0) 1)
                (ptr-read-u8 byte 0)
              nil)))))))

  (defun terminal-raw-mode--u16 (buf off)
    (+ (ptr-read-u8 buf off)
       (* (ptr-read-u8 buf (+ off 1)) 256)))

  (defun install-winsize-handler ()
    (setq terminal-raw-mode--winsize-changed nil)
    t)

  (defun install-sigint-handler ()
    t)

  (defun install-jobctrl-handlers ()
    t)

  (defun terminal-take-winsize-changed ()
    (let ((pending terminal-raw-mode--winsize-changed))
      (setq terminal-raw-mode--winsize-changed nil)
      pending))

  (defun terminal-take-sigcont ()
    nil)

  (defun terminal-current-winsize ()
    (let* ((fd0 terminal-raw-mode--fd)
           (fd (or fd0
                   (syscall-direct 2 (terminal-raw-mode--dev-tty-path)
                                   2 0 0 0 0)))
           (buf (terminal-raw-mode--ensure-buf
                 'terminal-raw-mode--winsize 8 4)))
      (if (or (not fd) (< fd 0))
          (cons 80 24)
        (let ((rc (syscall-direct 16 fd 21523 buf 0 0 0)))
          (unless fd0
            (syscall-direct 3 fd 0 0 0 0 0))
          (if (< rc 0)
              (cons 80 24)
            (let ((rows (terminal-raw-mode--u16 buf 0))
                  (cols (terminal-raw-mode--u16 buf 2)))
              (if (and (> rows 0) (> cols 0))
                  (cons cols rows)
                (cons 80 24))))))))

(provide 'emacs-fns)

;;; emacs-fns.el ends here
