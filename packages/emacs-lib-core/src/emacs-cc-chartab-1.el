;;; emacs-cc-chartab-1.el --- Unicode property char-table primitives -*- lexical-binding: t; -*-

;;; Code:

(require 'emacs-char-table)

(unless (fboundp 'optimize-char-table)
  (defun optimize-char-table (char-table &optional test)
    "Optimize CHAR-TABLE. TEST is the comparison function used to decide whether two entries are equivalent and can be merged. It defaults to `equal'."
    (unless (char-table-p char-table)
      (signal 'wrong-type-argument (list 'char-table-p char-table)))
    (unless (or (null test) (functionp test))
      (signal 'wrong-type-argument (list 'functionp test)))
    nil))

;; The prelude optimizer operates on NeLisp's native char-table storage.
;; The emacs-char-table package also exposes tagged Lisp-owned tables, for
;; which the default optimizer is a logical no-op: it may compact native
;; storage, but must not change any character's value.  Keep the previous
;; optimizer for native tables and foreign calls.  Owned tables with an
;; explicit comparator use their Lisp topology so partial mutations and
;; cache publication remain visible if the comparator signals.
(defvar emacs-cc-chartab-1--previous-optimize-char-table
  (and (fboundp 'optimize-char-table)
       (symbol-function 'optimize-char-table)))

(when (and (fboundp 'nelisp--chartab-standalone-p)
           (nelisp--chartab-standalone-p)
           emacs-cc-chartab-1--previous-optimize-char-table)
  (defalias 'optimize-char-table
    (lambda (char-table &optional test)
      "Optimize CHAR-TABLE, preserving owned-table mutation semantics.
TEST defaults to `equal'; custom owned-table calls use its topology helper."
      (if (and (fboundp 'emacs-char-table--owned-p)
               (emacs-char-table--owned-p char-table))
          ;; chartab.c optimize_sub_char_table: a nil TEST compares with
          ;; `equal'.
          (progn
            (if (fboundp 'emacs-char-table--owner-optimize)
                (emacs-char-table--owner-optimize char-table (or test #'equal))
              (funcall emacs-cc-chartab-1--previous-optimize-char-table
                       char-table test)))
        (funcall emacs-cc-chartab-1--previous-optimize-char-table
                 char-table test)))))

(defvar char-code-property-alist nil
  "Alist of Unicode properties and their char-tables or table file names.")

(defun emacs-cc-chartab-1--read-contents (raw offset depth)
  "Convert the slots of RAW starting at OFFSET to owned topology at DEPTH.
The native reader represents GNU's #^[ and #^^[ literals as vectors."
  (let* ((size (length raw))
         (contents (make-vector (- size offset) nil))
         (child-size (aref [16 32 128] (min depth 2)))
         (i offset))
    (while (< i size)
      (let ((entry (aref raw i)))
        (aset contents (- i offset)
              (if (and (< depth 3) (vectorp entry)
                       (= (length entry) (+ 2 child-size))
                       (eq (aref entry 0) (1+ depth)))
                  (emacs-char-table--topology-node-create
                   (emacs-cc-chartab-1--read-contents entry 2 (1+ depth)))
                entry)))
      (setq i (1+ i)))
    contents))

(defun emacs-cc-chartab-1--read-table (raw)
  "Import a generated Unicode property table from RAW's reader slots."
  ;; Standard slots are default, parent, subtype, ASCII cache, and 64
  ;; root entries.  Derive the ASCII cache from the root as uniprop_table does.
  (let* ((table (make-char-table 'char-code-property-table))
         (root (emacs-cc-chartab-1--read-contents (substring raw 0 68) 4 0)))
    (emacs-char-table--raw-set table emacs-char-table--i-owner-state
                              (emacs-char-table--owner-state-create root nil))
    (emacs-char-table--raw-set table emacs-char-table--i-default (aref raw 0))
    (emacs-char-table--raw-set table emacs-char-table--i-extra (substring raw 68))
    (when (aref raw 1)
      (set-char-table-parent table (aref raw 1)))
    (emacs-char-table--owner-refresh-ascii-cache table)
    (emacs-char-table--owner-publish-span table 128 (max-char))
    ;; Native fast reads of the ASCII cache cannot expand a compressed leaf.
    (emacs-cc-chartab-1--uncompress table 0 (aref table 0))
    (emacs-cc-chartab-1--uncompress table 128
                                    (emacs-char-table--topology-ref root 128))
    table))

(defun emacs-cc-chartab-1--compressed-p (value)
  (and (stringp value) (> (length value) 0) (memq (aref value 0) '(1 2))))

(defun emacs-cc-chartab-1--uncompress (table char value)
  "Expand GNU's simple or run-length leaf containing CHAR, if compressed.
Return the raw value at CHAR.  Word lists (prefix zero) are handled by
the Lisp functions shipped in the generated tables."
  (if (not (emacs-cc-chartab-1--compressed-p value))
      value
    (let* ((base (logand char -128))
           (entries (make-vector 128 nil))
           (size (length value))
           (i 1)
           (index 0))
      (if (= (aref value 0) 1)
          (progn
            (setq index (aref value i) i (1+ i))
            (while (and (< i size) (< index 128))
              (let ((code (aref value i)))
                (when (> code 0)
                  (aset entries index code)))
              (setq i (1+ i) index (1+ index))))
        (while (and (< i size) (< index 128))
          (let ((code (aref value i))
                (end (1+ index)))
            (setq i (1+ i))
            (when (and (< i size) (>= (aref value i) 128))
              (setq end (min 128 (+ index (- (aref value i) 128)))
                    i (1+ i)))
            (while (< index end)
              (aset entries index code)
              (setq index (1+ index))))))
      (let ((root (emacs-char-table--owner-root table)))
        ;; Replace just this leaf.  The ranges view is derived from the
        ;; topology, so a whole batch of expansions needs only one rebuild.
        (emacs-char-table--topology-set-range
         root base (+ base 127) (emacs-char-table--topology-node-create entries))
        (when (= base 0)
          (emacs-char-table--owner-refresh-ascii-cache table))
        (emacs-char-table--owner-publish-span table base (+ base 127)))
      (aref entries (- char base)))))

(defun emacs-cc-chartab-1--uncompress-at (table char)
  (emacs-cc-chartab-1--uncompress
   table char (emacs-char-table--topology-ref
               (emacs-char-table--owner-root table) char)))

(defun emacs-cc-chartab-1--decode (values value)
  "Decode VALUE using VALUES, GNU Unicode table extra slot four."
  (if (and (vectorp values) (integerp value) (<= 0 value)
           (< value (length values)))
      (aref values value)
    value))

(defun emacs-cc-chartab-1--prepare-map (table)
  "Expand TABLE's compressed leaves and return its decoder vector, if any."
  (dolist (entry (emacs-char-table--ranges table))
    (when (emacs-cc-chartab-1--compressed-p (cdr entry))
      (let* ((range (car entry))
             (char (car range)))
        (while (<= char (cdr range))
          (emacs-cc-chartab-1--uncompress table char (cdr entry))
          (setq char (+ char 128))))))
  (when (eq (char-table-extra-slot table 1) 0)
    (char-table-extra-slot table 4)))

(defun emacs-cc-chartab-1--check-unicode-table (table char)
  (unless (char-table-p table)
    (signal 'wrong-type-argument (list 'char-table-p table)))
  (unless (characterp char)
    (signal 'wrong-type-argument (list 'characterp char)))
  (unless (and (eq (char-table-subtype table) 'char-code-property-table)
               (eq (emacs-char-table-extra-slot-count table) 5))
    (error "Invalid Unicode property table")))

(defun emacs-cc-chartab-1--get (table char)
  (emacs-cc-chartab-1--check-unicode-table table char)
  (let ((value (aref table char)))
    (if (eq (char-table-extra-slot table 1) 0)
        (emacs-cc-chartab-1--decode (char-table-extra-slot table 4) value)
      value)))

(defun emacs-cc-chartab-1--encode (table value)
  (let ((encoder (char-table-extra-slot table 2)))
    (cond
     ((eq encoder 0)
      (unless (or (null value) (characterp value))
        (signal 'wrong-type-argument (list 'integerp value)))
      value)
     ((memq encoder '(1 2))
      (when (and (eq encoder 2) (not (fixnump value)))
        (signal 'wrong-type-argument (list 'fixnump value)))
      (let* ((values (char-table-extra-slot table 4))
             (size (length values))
             (i 0))
        (while (and (< i size) (not (eq (aref values i) value)))
          (setq i (1+ i)))
        (when (= i size)
          (if (eq encoder 2)
              ;; GNU 31.1 appends the assigned index when adding a value.
              (set-char-table-extra-slot table 4 (vconcat values (vector i)))
            (signal 'wrong-type-argument (list "Unicode property value" value))))
        i))
     (t value))))

(defun emacs-cc-chartab-1--put (table char value)
  (emacs-cc-chartab-1--check-unicode-table table char)
  (aset table char (emacs-cc-chartab-1--encode table value))
  nil)

(defun emacs-cc-chartab-1--property-table (property)
  (let ((table (cdr (assq property char-code-property-alist))))
    (when (stringp table)
      (save-match-data
        (load (concat "international/" table) t 'no-message t t))
      (setq table (cdr (assq property char-code-property-alist))))
    table))

(when (emacs-char-table--standalone-p)
  (defalias 'get-unicode-property-internal #'emacs-cc-chartab-1--get)
  (defalias 'put-unicode-property-internal #'emacs-cc-chartab-1--put)
  (defalias 'unicode-property-table-internal #'emacs-cc-chartab-1--property-table))

(provide 'emacs-cc-chartab-1)
