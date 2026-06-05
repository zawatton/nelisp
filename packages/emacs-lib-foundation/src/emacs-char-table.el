;;; emacs-char-table.el --- char-table substrate for standalone NeLisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 08 §2 (2026-06-05) — Layer 2 (nemacs substrate).
;;
;; The standalone NeLisp bootstrap only had `nil'-stubs for the
;; char-table API (in `emacs-stub-bulk.el'), and the lightweight
;; case-table substrate (`case-table.el', 259-element fixed vectors)
;; is not wired into the cold-boot bundle.  That left vendor code such
;; as isearch.el's `isearch-mode-map' defvar unable to evaluate:
;;
;;     (let ((map (make-keymap)))
;;       (or (char-table-p (nth 1 map))
;;           (error "..."))                       ; <- assertion failed
;;       (set-char-table-range (nth 1 map) (cons #x100 (max-char)) ...))
;;
;; This module provides a real, sparse char-table substrate that:
;;
;;   - satisfies `char-table-p' (so the assertion passes once
;;     `make-keymap' embeds one — see `emacs-keymap.el'),
;;   - stores huge ranges such as `(#x100 . #x3FFFFF)' sparsely instead
;;     of materialising ~4M slots (which would crash or OOM), and
;;   - supplies the missing `max-char' / `char-table-subtype' /
;;     `char-table-parent' primitives.
;;
;; Representation (tagged vector — `vectorp' ops are always available,
;; unlike a hypothetical reader-level char-table type):
;;
;;   [--nemacs-char-table SUBTYPE DEFAULT PARENT ASCII-VEC RANGES EXTRA]
;;     0 TAG       = `--nemacs-char-table' (the `char-table-p' key)
;;     1 SUBTYPE   = the `make-char-table' subtype argument
;;     2 DEFAULT   = value for characters with no explicit entry
;;     3 PARENT    = parent char-table, or nil
;;     4 ASCII-VEC = (make-vector 256 INIT) fast slot for char < 256
;;     5 RANGES    = list of ((FROM . TO) . VAL), newest first, char >= 256
;;     6 EXTRA     = (make-vector N nil) extra slots (subtype metadata)
;;
;; Length is 7 so `(> (length x) 6)' holds, distinguishing a char-table
;; from incidental short vectors.
;;
;; Two-mode design (mirrors `case-table.el' / `emacs-keymap-builtins.el'):
;; under host Emacs the real C primitives win (the unprefixed names are
;; only installed when not already `fboundp'); under standalone NeLisp we
;; install our implementations unconditionally so they override the
;; earlier `emacs-stub-bulk.el' nil-stubs.

;;; Code:

(defconst emacs-char-table--tag '--nemacs-char-table
  "Symbol stored in slot 0 that identifies a NeLisp char-table.")

(defconst emacs-char-table--ascii-size 256
  "Number of fast direct-indexed slots (characters 0..255).")

(defconst emacs-char-table--extra-slots 10
  "Number of extra metadata slots carried by every char-table.
Real Emacs derives this from each subtype's `char-table-extra-slots'
property (0..10); we allocate the maximum so any subtype fits.")

(defconst emacs-char-table--max-char #x3FFFFF
  "Largest character code (Doc 05: UTF-8 / Unicode range).")

;; Slot indices.
(defconst emacs-char-table--i-subtype 1)
(defconst emacs-char-table--i-default 2)
(defconst emacs-char-table--i-parent 3)
(defconst emacs-char-table--i-ascii 4)
(defconst emacs-char-table--i-ranges 5)
(defconst emacs-char-table--i-extra 6)

(defun emacs-char-table--standalone-p ()
  "Return non-nil under standalone NeLisp (no host `emacs-version')."
  (not (boundp 'emacs-version)))

(defun emacs-char-table--install-function-p (symbol)
  "Return non-nil when SYMBOL should be installed by this substrate."
  (if (emacs-char-table--standalone-p)
      t
    (not (fboundp symbol))))

;;;; --- constructor / predicate ----------------------------------------

(defun emacs-char-table-make (&optional subtype init)
  "Return a fresh char-table.
SUBTYPE labels the table; INIT is the default value for every
character (defaults to nil)."
  (let ((ct (make-vector 7 nil))
        (ascii (make-vector emacs-char-table--ascii-size init)))
    (aset ct 0 emacs-char-table--tag)
    (aset ct emacs-char-table--i-subtype subtype)
    (aset ct emacs-char-table--i-default init)
    (aset ct emacs-char-table--i-parent nil)
    (aset ct emacs-char-table--i-ascii ascii)
    (aset ct emacs-char-table--i-ranges nil)
    (aset ct emacs-char-table--i-extra
          (make-vector emacs-char-table--extra-slots nil))
    ct))

(defun emacs-char-table-p (object)
  "Return non-nil when OBJECT is a NeLisp char-table."
  (and (vectorp object)
       (> (length object) 6)
       (eq (aref object 0) emacs-char-table--tag)))

(defun emacs-char-table-ascii-vector (ct)
  "Return CT's raw 256-slot ASCII vector (characters 0..255)."
  (aref ct emacs-char-table--i-ascii))

;;;; --- single-character access ----------------------------------------

(defun emacs-char-table--range-lookup (ct char)
  "Return the stored value for CHAR from CT's RANGES, or the symbol
`emacs-char-table--unset' when no range covers CHAR."
  (let ((ranges (aref ct emacs-char-table--i-ranges))
        (result 'emacs-char-table--unset))
    (while (and ranges (eq result 'emacs-char-table--unset))
      (let* ((entry (car ranges))
             (key (car entry)))
        (when (and (<= (car key) char) (<= char (cdr key)))
          (setq result (cdr entry))))
      (setq ranges (cdr ranges)))
    result))

(defun emacs-char-table-ref (ct char)
  "Return CT's value for character CHAR (with default / parent fallback)."
  (cond
   ((not (integerp char)) (aref ct emacs-char-table--i-default))
   ((and (>= char 0) (< char emacs-char-table--ascii-size))
    (let ((v (aref (aref ct emacs-char-table--i-ascii) char)))
      (if (and (null v) (aref ct emacs-char-table--i-parent))
          (emacs-char-table-ref (aref ct emacs-char-table--i-parent) char)
        v)))
   (t
    (let ((v (emacs-char-table--range-lookup ct char)))
      (cond
       ((not (eq v 'emacs-char-table--unset)) v)
       ((aref ct emacs-char-table--i-parent)
        (emacs-char-table-ref (aref ct emacs-char-table--i-parent) char))
       (t (aref ct emacs-char-table--i-default)))))))

(defun emacs-char-table-set (ct char value)
  "Set CT's value for a single character CHAR to VALUE."
  (if (and (integerp char) (>= char 0) (< char emacs-char-table--ascii-size))
      (aset (aref ct emacs-char-table--i-ascii) char value)
    (aset ct emacs-char-table--i-ranges
          (cons (cons (cons char char) value)
                (aref ct emacs-char-table--i-ranges))))
  value)

;;;; --- range access ---------------------------------------------------

(defun emacs-char-table--fill-ascii (ct from to value)
  "Store VALUE into CT's ASCII vector for chars FROM..TO (clamped 0..255)."
  (let ((i (max from 0))
        (hi (min to (1- emacs-char-table--ascii-size)))
        (vec (aref ct emacs-char-table--i-ascii)))
    (while (<= i hi)
      (aset vec i value)
      (setq i (1+ i)))))

(defun emacs-char-table-set-range (ct range value)
  "Set CT entries selected by RANGE to VALUE.
RANGE is nil (the default value), t (the whole table), a character,
or a cons (FROM . TO).  Large supra-ASCII ranges are stored sparsely
rather than materialised."
  (cond
   ((null range)
    (aset ct emacs-char-table--i-default value))
   ((eq range t)
    (aset ct emacs-char-table--i-default value)
    (emacs-char-table--fill-ascii ct 0 (1- emacs-char-table--ascii-size) value)
    (aset ct emacs-char-table--i-ranges
          (list (cons (cons 0 emacs-char-table--max-char) value))))
   ((integerp range)
    (emacs-char-table-set ct range value))
   ((consp range)
    (let ((from (car range))
          (to (cdr range)))
      (when (and (integerp from) (integerp to) (<= from to))
        (emacs-char-table--fill-ascii ct from to value)
        (when (>= to emacs-char-table--ascii-size)
          (aset ct emacs-char-table--i-ranges
                (cons (cons (cons (max from emacs-char-table--ascii-size) to)
                            value)
                      (aref ct emacs-char-table--i-ranges))))))))
  value)

(defun emacs-char-table-range (ct range)
  "Return CT's value for RANGE.
RANGE is nil (default), t (default), a character, or a cons whose CAR
character is sampled."
  (cond
   ((null range) (aref ct emacs-char-table--i-default))
   ((eq range t) (aref ct emacs-char-table--i-default))
   ((integerp range) (emacs-char-table-ref ct range))
   ((consp range) (emacs-char-table-ref ct (car range)))
   (t nil)))

;;;; --- parent / subtype / extra slots ---------------------------------

(defun emacs-char-table-parent (ct)
  "Return CT's parent char-table, or nil."
  (aref ct emacs-char-table--i-parent))

(defun emacs-char-table-set-parent (ct parent)
  "Set CT's parent to PARENT (a char-table or nil).  Returns PARENT."
  (aset ct emacs-char-table--i-parent parent)
  parent)

(defun emacs-char-table-subtype (ct)
  "Return CT's subtype."
  (aref ct emacs-char-table--i-subtype))

(defun emacs-char-table-extra-slot (ct n)
  "Return CT's extra slot N."
  (aref (aref ct emacs-char-table--i-extra) n))

(defun emacs-char-table-set-extra-slot (ct n value)
  "Set CT's extra slot N to VALUE."
  (aset (aref ct emacs-char-table--i-extra) n value))

;;;; --- iteration ------------------------------------------------------

(defun emacs-char-table-map (function ct)
  "Call FUNCTION with (KEY VALUE) for each non-nil entry of CT.
KEY is a character for ASCII slots or a cons (FROM . TO) for a stored
range.  Mirrors the `map-char-table' calling convention."
  (let ((vec (aref ct emacs-char-table--i-ascii))
        (i 0))
    (while (< i emacs-char-table--ascii-size)
      (let ((v (aref vec i)))
        (when v (funcall function i v)))
      (setq i (1+ i))))
  (let ((ranges (reverse (aref ct emacs-char-table--i-ranges))))
    (while ranges
      (let ((entry (car ranges)))
        (when (cdr entry)
          (funcall function (car entry) (cdr entry))))
      (setq ranges (cdr ranges)))))

(defun emacs-char-table-copy (ct &optional valfn)
  "Return a copy of CT.
When VALFN is non-nil it transforms each non-nil value (used by
`copy-keymap' to recurse into nested keymaps)."
  (let ((new (emacs-char-table-make
              (aref ct emacs-char-table--i-subtype)
              (aref ct emacs-char-table--i-default))))
    (aset new emacs-char-table--i-parent (aref ct emacs-char-table--i-parent))
    (let ((src (aref ct emacs-char-table--i-ascii))
          (dst (aref new emacs-char-table--i-ascii))
          (i 0))
      (while (< i emacs-char-table--ascii-size)
        (let ((v (aref src i)))
          (aset dst i (if (and valfn v) (funcall valfn v) v)))
        (setq i (1+ i))))
    (aset new emacs-char-table--i-ranges
          (mapcar (lambda (e)
                    (cons (car e)
                          (if (and valfn (cdr e)) (funcall valfn (cdr e)) (cdr e))))
                  (aref ct emacs-char-table--i-ranges)))
    (aset new emacs-char-table--i-extra
          (copy-sequence (aref ct emacs-char-table--i-extra)))
    new))

(defun emacs-char-table-max-char (&optional _unicode)
  "Return the largest character code (#x3FFFFF)."
  emacs-char-table--max-char)

;;;; --- install unprefixed names ---------------------------------------

(when (emacs-char-table--standalone-p)
  (fset 'char-table-p #'emacs-char-table-p)
  (fset 'make-char-table #'emacs-char-table-make)
  (fset 'char-table-range #'emacs-char-table-range)
  (fset 'set-char-table-range #'emacs-char-table-set-range)
  (fset 'char-table-parent #'emacs-char-table-parent)
  (fset 'set-char-table-parent #'emacs-char-table-set-parent)
  (fset 'char-table-subtype #'emacs-char-table-subtype)
  (fset 'char-table-extra-slot #'emacs-char-table-extra-slot)
  (fset 'set-char-table-extra-slot #'emacs-char-table-set-extra-slot)
  (fset 'map-char-table #'emacs-char-table-map)
  (fset 'max-char #'emacs-char-table-max-char))

(when (emacs-char-table--install-function-p 'char-table-p)
  (defalias 'char-table-p #'emacs-char-table-p))
(when (emacs-char-table--install-function-p 'make-char-table)
  (defalias 'make-char-table #'emacs-char-table-make))
(when (emacs-char-table--install-function-p 'char-table-range)
  (defalias 'char-table-range #'emacs-char-table-range))
(when (emacs-char-table--install-function-p 'set-char-table-range)
  (defalias 'set-char-table-range #'emacs-char-table-set-range))
(when (emacs-char-table--install-function-p 'char-table-parent)
  (defalias 'char-table-parent #'emacs-char-table-parent))
(when (emacs-char-table--install-function-p 'set-char-table-parent)
  (defalias 'set-char-table-parent #'emacs-char-table-set-parent))
(when (emacs-char-table--install-function-p 'char-table-subtype)
  (defalias 'char-table-subtype #'emacs-char-table-subtype))
(when (emacs-char-table--install-function-p 'char-table-extra-slot)
  (defalias 'char-table-extra-slot #'emacs-char-table-extra-slot))
(when (emacs-char-table--install-function-p 'set-char-table-extra-slot)
  (defalias 'set-char-table-extra-slot #'emacs-char-table-set-extra-slot))
(when (emacs-char-table--install-function-p 'map-char-table)
  (defalias 'map-char-table #'emacs-char-table-map))
(when (emacs-char-table--install-function-p 'max-char)
  (defalias 'max-char #'emacs-char-table-max-char))

(provide 'emacs-char-table)

;;; emacs-char-table.el ends here
