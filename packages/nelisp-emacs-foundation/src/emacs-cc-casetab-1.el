;;; emacs-cc-casetab-1.el --- casetab.c primitives -*- lexical-binding: t; -*-

;; GNU Emacs 31.1: src/casetab.c:35-169,177-240,248-285.
;; The native char-table substrate owns representation and slot access.
;; The optional foundation adapter uses the same algorithms on its tables.

(put 'case-table 'char-table-extra-slots 3)

;; GNU calls its C primitives directly.  Keep those operations when the
;; optional foundation adapter later replaces the public Lisp function cells.
(defvar nelisp-casetab--native-p (symbol-function 'char-table-p))
(defvar nelisp-casetab--native-subtype (symbol-function 'char-table-subtype))
(defvar nelisp-casetab--native-range (symbol-function 'char-table-range))
(defvar nelisp-casetab--native-set-range (symbol-function 'set-char-table-range))
(defvar nelisp-casetab--native-extra (symbol-function 'char-table-extra-slot))
(defvar nelisp-casetab--native-set-extra (symbol-function 'set-char-table-extra-slot))
(defvar nelisp-casetab--native-map (symbol-function 'map-char-table))
(defvar nelisp-casetab--native-make (symbol-function 'make-char-table))
(defvar nelisp-casetab--native-parent (symbol-function 'char-table-parent))
(defvar nelisp-casetab--native-set-parent (symbol-function 'set-char-table-parent))

(defun nelisp-casetab--standalone-p ()
  "Return non-nil in the standalone NeLisp runtime."
  (or (fboundp 'nl-write-file) (not (boundp 'emacs-version))))

(defun nelisp-casetab--sparse-api-p ()
  "Return non-nil when the optional foundation char-table adapter is loaded."
  (fboundp 'emacs-char-table-make))

(defun nelisp-casetab--sparse-table-p (table)
  "Return non-nil when TABLE belongs to the foundation adapter."
  (and (fboundp 'emacs-char-table-p) (emacs-char-table-p table)))

(defun nelisp-casetab--char-table-p (table)
  "Return non-nil for a native or foundation char-table TABLE."
  (or (funcall nelisp-casetab--native-p table) (nelisp-casetab--sparse-table-p table)))

(defun nelisp-casetab--char-table-subtype (table)
  (if (nelisp-casetab--sparse-table-p table)
      (emacs-char-table-subtype table)
    (funcall nelisp-casetab--native-subtype table)))

(defun nelisp-casetab--char-table-range (table range)
  (if (nelisp-casetab--sparse-table-p table)
      (emacs-char-table-range table range)
    (funcall nelisp-casetab--native-range table range)))

(defun nelisp-casetab--set-char-table-range (table range value)
  (if (nelisp-casetab--sparse-table-p table)
      (emacs-char-table-set-range table range value)
    (if (integerp range)
        (progn
          (unless (or (nelisp-casetab--char-table-p table) (arrayp table))
            (signal 'wrong-type-argument (list 'arrayp table)))
          (aset table range value))
      (funcall nelisp-casetab--native-set-range table range value))))

(defun nelisp-casetab--aset (table character value)
  "Set one array entry with GNU aset's type and character checks.
Unlike `set-char-table-range', this never accepts a range or default key."
  (unless (fixnump character)
    (signal 'wrong-type-argument (list 'fixnump character)))
  (unless (or (nelisp-casetab--char-table-p table) (arrayp table) (recordp table))
    (signal 'wrong-type-argument (list 'arrayp table)))
  (when (and (nelisp-casetab--char-table-p table)
             (or (< character 0) (> character 4194303)))
    (signal 'wrong-type-argument (list 'characterp character)))
  (if (nelisp-casetab--sparse-table-p table)
      (emacs-char-table-set table character value)
    (aset table character value)))

(defun nelisp-casetab--get-extra-slot (table slot)
  (if (nelisp-casetab--sparse-table-p table)
      (emacs-char-table-extra-slot table slot)
    (funcall nelisp-casetab--native-extra table slot)))

;; GNU's searches read the buffer's case_canon_table slot, which only
;; `set-case-table' and `set-standard-case-table' change.  The regexp
;; matcher memoizes its canon read per buffer and this counter, so every
;; change it could observe bumps it: those two functions and every
;; extra-slot write below.
(defvar nelisp-casetab--epoch 0
  "Count of case-table changes a case-folding search can observe.")

(defun nelisp-casetab--put-extra-slot (table slot value)
  (setq nelisp-casetab--epoch (1+ nelisp-casetab--epoch))
  (if (nelisp-casetab--sparse-table-p table)
      (emacs-char-table-set-extra-slot table slot value)
    (funcall nelisp-casetab--native-set-extra table slot value)))

(defalias 'nelisp-casetab--char-table-extra-slot #'nelisp-casetab--get-extra-slot)
(defalias 'nelisp-casetab--set-char-table-extra-slot #'nelisp-casetab--put-extra-slot)

(defun nelisp-casetab--map-char-table (function table)
  (if (nelisp-casetab--sparse-table-p table)
      (emacs-char-table-map function table)
    (funcall nelisp-casetab--native-map function table)))

(defun nelisp-casetab--ref (table character)
  (if (nelisp-casetab--sparse-table-p table)
      (emacs-char-table-ref table character)
    (aref table character)))

(defun nelisp-casetab--empty-table ()
  "Return an empty char-table of subtype `case-table'."
  (funcall nelisp-casetab--native-make 'case-table nil))

(defun case-table-p (object)
  "Return t if OBJECT is a case table, including valid auxiliary slots."
  (and (nelisp-casetab--char-table-p object)
       (eq (nelisp-casetab--char-table-subtype object) 'case-table)
       (let ((up (nelisp-casetab--get-extra-slot object 0))
             (canon (nelisp-casetab--get-extra-slot object 1))
             (eqv (nelisp-casetab--get-extra-slot object 2)))
         (and (or (null up) (nelisp-casetab--char-table-p up))
              (or (and (null canon) (null eqv))
                  (and (nelisp-casetab--char-table-p canon)
                       (or (null eqv) (nelisp-casetab--char-table-p eqv))))))))

(defun nelisp-casetab--check-table (table)
  (unless (case-table-p table)
    (signal 'wrong-type-argument (list 'case-table-p table)))
  table)

(defun nelisp-casetab--set-identity (table range value)
  "Port of casetab.c:192: initialize RANGE when VALUE is a natural fixnum."
  (when (and (fixnump value) (>= value 0))
    (let ((from (if (consp range) (car range) range))
          (to (if (consp range) (cdr range) range)))
      (while (<= from to)
        (nelisp-casetab--set-char-table-range table from from)
        (setq from (1+ from))))))

(defun nelisp-casetab--shuffle (table range value)
  "Port of casetab.c:218: permute TABLE into translation-class cycles."
  (when (and (fixnump value) (>= value 0))
    (let ((from (if (consp range) (car range) range))
          (to (if (consp range) (cdr range) range)))
      (while (<= from to)
        (let ((previous (nelisp-casetab--ref table value)))
          (nelisp-casetab--set-char-table-range table value from)
          (nelisp-casetab--set-char-table-range table from previous))
        (setq from (1+ from))))))

(defun nelisp-casetab--ensure-extra-slots (table)
  "Port GNU set_case_table's identity, shuffle and canonicalization passes."
  (let ((up (nelisp-casetab--get-extra-slot table 0))
        (canon (nelisp-casetab--get-extra-slot table 1))
        (eqv (nelisp-casetab--get-extra-slot table 2)))
    (unless up
      (setq up (nelisp-casetab--empty-table))
      (nelisp-casetab--map-char-table
       (lambda (range value) (nelisp-casetab--set-identity up range value)) table)
      (nelisp-casetab--map-char-table
       (lambda (range value) (nelisp-casetab--shuffle up range value)) table)
      (nelisp-casetab--put-extra-slot table 0 up))
    (unless canon
      (setq canon (nelisp-casetab--empty-table))
      (nelisp-casetab--put-extra-slot table 1 canon)
      (nelisp-casetab--map-char-table
       (lambda (range value)
         (when (and (fixnump value) (>= value 0))
           (nelisp-casetab--set-char-table-range
            canon range (nelisp-casetab--ref table (nelisp-casetab--ref up value)))))
       table))
    (unless eqv
      (setq eqv (nelisp-casetab--empty-table))
      (nelisp-casetab--map-char-table
       (lambda (range value) (nelisp-casetab--set-identity eqv range value)) canon)
      (nelisp-casetab--map-char-table
       (lambda (range value) (nelisp-casetab--shuffle eqv range value)) canon)
      (nelisp-casetab--put-extra-slot table 2 eqv))
    ;; GNU regex-emacs.c finds the equivalence table through CANON.
    (nelisp-casetab--put-extra-slot canon 2 eqv))
  table)

(defvar nelisp-casetab--standard nil "The standard downcase char-table.")
(defvar nelisp-casetab--initial-standard nil "The original standard table object.")
(defvar nelisp-casetab--unicode-initialized nil "Non-nil after Unicode data registration.")
(defvar nelisp-casetab--builtin-canon nil
  "The canonicalize table of the standard case table as built from GNU
31.1's Unicode data, before any change.  Its mapping is exactly the
built-in fold the regexp matcher applies without a translate table
\(`nlre--translate-char'), so a search through it needs no table.")
(defvar nelisp-casetab--current nil "The current buffer's downcase char-table.")
(make-variable-buffer-local 'nelisp-casetab--current)
(defvar nelisp--case-table nil "Compatibility name for the installed case table.")
(make-variable-buffer-local 'nelisp--case-table)
(defvar nelisp-casetab--buffer-tables (make-hash-table :test 'eq :weakness 'key)
  "Per-buffer case-table fields, mirroring GNU's buffer-owned downcase table.")

(defun nelisp-casetab--ensure-standard-initialized ()
  "Initialize the shared standard table lazily, without case conversion."
  (unless nelisp-casetab--standard
    (let ((down (nelisp-casetab--empty-table))
          (up (nelisp-casetab--empty-table)))
      (if (fboundp 'nelisp-casetab--initialize-standard)
          (progn
            (nelisp-casetab--initialize-standard down up)
            (setq nelisp-casetab--unicode-initialized t))
        ;; GNU casetab.c:248-285 initializes ASCII before Lisp Unicode data.
        (let ((character 0))
          (while (< character 128)
            (nelisp-casetab--set-char-table-range
             down character
             (if (and (>= character ?A) (<= character ?Z))
                 (+ character 32) character))
            (nelisp-casetab--set-char-table-range
             up character
             (if (and (>= character ?a) (<= character ?z))
                 (- character 32) character))
            (setq character (1+ character)))))
      (nelisp-casetab--put-extra-slot down 0 up)
      (nelisp-casetab--ensure-extra-slots down)
      (when nelisp-casetab--unicode-initialized
        (setq nelisp-casetab--builtin-canon
              (nelisp-casetab--get-extra-slot down 1)))
      (setq nelisp-casetab--standard down nelisp-casetab--initial-standard down)))
  nelisp-casetab--standard)

(defun nelisp-casetab--register-standard-data ()
  "Upgrade an already used ASCII bootstrap table when Unicode data arrives.
Keep the original object's identity and leave user-selected standard tables
alone.  An untouched bootstrap stays lazy until its first API call."
  (when (and nelisp-casetab--initial-standard
             (not nelisp-casetab--unicode-initialized))
    (let ((table nelisp-casetab--initial-standard)
          (up (nelisp-casetab--empty-table)))
      (nelisp-casetab--initialize-standard table up)
      (nelisp-casetab--put-extra-slot table 0 up)
      (nelisp-casetab--put-extra-slot table 1 nil)
      (nelisp-casetab--put-extra-slot table 2 nil)
      (nelisp-casetab--ensure-extra-slots table)
      (setq nelisp-casetab--builtin-canon
            (nelisp-casetab--get-extra-slot table 1))
      (setq nelisp-casetab--unicode-initialized t))))

(defun standard-case-table ()
  "Return the standard case table."
  (nelisp-casetab--ensure-standard-initialized))

(defun current-case-table ()
  "Return the case table of the current buffer."
  (or (gethash (current-buffer) nelisp-casetab--buffer-tables)
      (let ((table (standard-case-table)))
        (puthash (current-buffer) table nelisp-casetab--buffer-tables)
        table)))

(defun set-case-table (table)
  "Select TABLE as the case table of the current buffer.
TABLE is a `case-table' char-table with upcase, canonicalization and
equivalence tables in extra slots 0, 1 and 2.  Missing tables are derived."
  (nelisp-casetab--check-table table)
  (nelisp-casetab--ensure-extra-slots table)
  (setq nelisp-casetab--epoch (1+ nelisp-casetab--epoch))
  (puthash (current-buffer) table nelisp-casetab--buffer-tables)
  (make-local-variable 'nelisp-casetab--current)
  (make-local-variable 'nelisp--case-table)
  (setq nelisp-casetab--current table nelisp--case-table table)
  table)

(defun set-standard-case-table (table)
  "Select TABLE as the standard case table used by newly created buffers."
  (nelisp-casetab--check-table table)
  (nelisp-casetab--ensure-extra-slots table)
  (setq nelisp-casetab--epoch (1+ nelisp-casetab--epoch))
  (let ((old (standard-case-table)))
    ;; GNU stores four case-table fields at buffer creation.  Materialize
    ;; untouched buffers before changing Lisp defaults to preserve that rule.
    (dolist (buffer (if (fboundp 'buffer-list) (buffer-list) (nelisp-buffer-list)))
      (unless (gethash buffer nelisp-casetab--buffer-tables)
        (puthash buffer old nelisp-casetab--buffer-tables)))
    (setq nelisp-casetab--standard table)
    (set-default 'nelisp-casetab--current table)
    (set-default 'nelisp--case-table table))
  table)

(defalias 'nelisp-casetab--set-current #'set-case-table)
(defalias 'nelisp-casetab--set-standard #'set-standard-case-table)

(defun nelisp-casetab--copy-char-table (table)
  "Copy TABLE's local contents, default, parent and all extra slots.
This adapts chartab.c:copy_char_table until the core owns a Lisp clone."
  (if (nelisp-casetab--sparse-table-p table)
      (emacs-char-table-copy table)
    (let* ((parent (funcall nelisp-casetab--native-parent table))
           (default (nelisp-casetab--char-table-range table nil))
           (subtype (nelisp-casetab--char-table-subtype table))
           (properties (copy-sequence (symbol-plist subtype)))
           (slot 0) (extras nil) done copy)
      (while (not done)
        (condition-case nil
            (progn
              (push (nelisp-casetab--get-extra-slot table slot) extras)
              (setq slot (1+ slot)))
          (args-out-of-range (setq done t))))
      ;; A subtype's allocation property can change after TABLE was made.
      ;; GNU copies its actual slot count rather than consulting that property.
      (unwind-protect
          (progn
            (put subtype 'char-table-extra-slots slot)
            (setq copy (funcall nelisp-casetab--native-make subtype nil)))
        (setplist subtype properties))
      (unwind-protect
          (progn
            (funcall nelisp-casetab--native-set-parent table nil)
            (nelisp-casetab--set-char-table-range table nil nil)
            (nelisp-casetab--map-char-table
             (lambda (range value) (nelisp-casetab--set-char-table-range copy range value)) table))
        (nelisp-casetab--set-char-table-range table nil default)
        (funcall nelisp-casetab--native-set-parent table parent))
      (nelisp-casetab--set-char-table-range copy nil default)
      (funcall nelisp-casetab--native-set-parent copy parent)
      (setq slot 0)
      (dolist (extra (nreverse extras))
        (nelisp-casetab--put-extra-slot copy slot extra)
        (setq slot (1+ slot)))
      copy)))

;; The `set-case-table' that bumps the epoch.  A case-table package that
;; redefines it without the epoch turns the matcher's memo off.
(defvar nelisp-casetab--epoch-owner nil
  "The epoch-aware `set-case-table' function object.")
(setq nelisp-casetab--epoch-owner (symbol-function 'set-case-table))

(provide 'emacs-cc-casetab-1)
;;; emacs-cc-casetab-1.el ends here
