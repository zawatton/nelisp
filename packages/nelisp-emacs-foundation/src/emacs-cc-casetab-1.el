;;; emacs-cc-casetab-1.el --- casetab.c primitives -*- lexical-binding: t; -*-

;; GNU Emacs 31.1: src/casetab.c:35-169,177-240,248-285.
;; The native char-table substrate owns representation and slot access.
;; The optional foundation adapter uses the same algorithms on its tables.

(put 'case-table 'char-table-extra-slots 3)

(defun case-table--standalone-p ()
  "Return non-nil in the standalone NeLisp runtime."
  (or (fboundp 'nl-write-file) (not (boundp 'emacs-version))))

(defun case-table--sparse-api-p ()
  "Return non-nil when the optional foundation char-table adapter is loaded."
  (fboundp 'emacs-char-table-make))

(defun case-table--sparse-table-p (table)
  "Return non-nil when TABLE belongs to the foundation adapter."
  (and (fboundp 'emacs-char-table-p) (emacs-char-table-p table)))

(defun case-table--char-table-p (table)
  "Return non-nil for a native or foundation char-table TABLE."
  (or (char-table-p table) (case-table--sparse-table-p table)))

(defun case-table--char-table-subtype (table)
  (if (case-table--sparse-table-p table)
      (emacs-char-table-subtype table)
    (char-table-subtype table)))

(defun case-table--char-table-range (table range)
  (if (case-table--sparse-table-p table)
      (emacs-char-table-range table range)
    (char-table-range table range)))

(defun case-table--set-char-table-range (table range value)
  (if (case-table--sparse-table-p table)
      (emacs-char-table-set-range table range value)
    (if (integerp range)
        (progn
          (unless (or (case-table--char-table-p table) (arrayp table))
            (signal 'wrong-type-argument (list 'arrayp table)))
          (aset table range value))
      (set-char-table-range table range value))))

(defun case-table--aset (table character value)
  "Set one array entry with GNU aset's type and character checks.
Unlike `set-char-table-range', this never accepts a range or default key."
  (unless (fixnump character)
    (signal 'wrong-type-argument (list 'fixnump character)))
  (unless (or (case-table--char-table-p table) (arrayp table) (recordp table))
    (signal 'wrong-type-argument (list 'arrayp table)))
  (when (and (case-table--char-table-p table)
             (or (< character 0) (> character 4194303)))
    (signal 'wrong-type-argument (list 'characterp character)))
  (if (case-table--sparse-table-p table)
      (emacs-char-table-set table character value)
    (aset table character value)))

(defun case-table--get-extra-slot (table slot)
  (if (case-table--sparse-table-p table)
      (emacs-char-table-extra-slot table slot)
    (char-table-extra-slot table slot)))

(defun case-table--put-extra-slot (table slot value)
  (if (case-table--sparse-table-p table)
      (emacs-char-table-set-extra-slot table slot value)
    (set-char-table-extra-slot table slot value)))

(defalias 'case-table--char-table-extra-slot #'case-table--get-extra-slot)
(defalias 'case-table--set-char-table-extra-slot #'case-table--put-extra-slot)

(defun case-table--map-char-table (function table)
  (if (case-table--sparse-table-p table)
      (emacs-char-table-map function table)
    (map-char-table function table)))

(defun case-table--ref (table character)
  (if (case-table--sparse-table-p table)
      (emacs-char-table-ref table character)
    (aref table character)))

(defun case-table--empty-table ()
  "Return an empty char-table of subtype `case-table'."
  (make-char-table 'case-table nil))

(defun case-table-p (object)
  "Return t if OBJECT is a case table, including valid auxiliary slots."
  (and (case-table--char-table-p object)
       (eq (case-table--char-table-subtype object) 'case-table)
       (let ((up (case-table--get-extra-slot object 0))
             (canon (case-table--get-extra-slot object 1))
             (eqv (case-table--get-extra-slot object 2)))
         (and (or (null up) (case-table--char-table-p up))
              (or (and (null canon) (null eqv))
                  (and (case-table--char-table-p canon)
                       (or (null eqv) (case-table--char-table-p eqv))))))))

(defun case-table--check-table (table)
  (unless (case-table-p table)
    (signal 'wrong-type-argument (list 'case-table-p table)))
  table)

(defun case-table--set-identity (table range value)
  "Port of casetab.c:192: initialize RANGE when VALUE is a natural fixnum."
  (when (and (fixnump value) (>= value 0))
    (let ((from (if (consp range) (car range) range))
          (to (if (consp range) (cdr range) range)))
      (while (<= from to)
        (case-table--set-char-table-range table from from)
        (setq from (1+ from))))))

(defun case-table--shuffle (table range value)
  "Port of casetab.c:218: permute TABLE into translation-class cycles."
  (when (and (fixnump value) (>= value 0))
    (let ((from (if (consp range) (car range) range))
          (to (if (consp range) (cdr range) range)))
      (while (<= from to)
        (let ((previous (case-table--ref table value)))
          (case-table--set-char-table-range table value from)
          (case-table--set-char-table-range table from previous))
        (setq from (1+ from))))))

(defun case-table--ensure-extra-slots (table)
  "Port GNU set_case_table's identity, shuffle and canonicalization passes."
  (let ((up (case-table--get-extra-slot table 0))
        (canon (case-table--get-extra-slot table 1))
        (eqv (case-table--get-extra-slot table 2)))
    (unless up
      (setq up (case-table--empty-table))
      (case-table--map-char-table
       (lambda (range value) (case-table--set-identity up range value)) table)
      (case-table--map-char-table
       (lambda (range value) (case-table--shuffle up range value)) table)
      (case-table--put-extra-slot table 0 up))
    (unless canon
      (setq canon (case-table--empty-table))
      (case-table--put-extra-slot table 1 canon)
      (case-table--map-char-table
       (lambda (range value)
         (when (and (fixnump value) (>= value 0))
           (case-table--set-char-table-range
            canon range (case-table--ref table (case-table--ref up value)))))
       table))
    (unless eqv
      (setq eqv (case-table--empty-table))
      (case-table--map-char-table
       (lambda (range value) (case-table--set-identity eqv range value)) canon)
      (case-table--map-char-table
       (lambda (range value) (case-table--shuffle eqv range value)) canon)
      (case-table--put-extra-slot table 2 eqv))
    ;; GNU regex-emacs.c finds the equivalence table through CANON.
    (case-table--put-extra-slot canon 2 eqv))
  table)

(defvar case-table--standard nil "The standard downcase char-table.")
(defvar case-table--initial-standard nil "The original standard table object.")
(defvar case-table--unicode-initialized nil "Non-nil after Unicode data registration.")
(defvar case-table--current nil "The current buffer's downcase char-table.")
(make-variable-buffer-local 'case-table--current)
(defvar nelisp--case-table nil "Compatibility name for the installed case table.")
(make-variable-buffer-local 'nelisp--case-table)
(defvar case-table--buffer-tables (make-hash-table :test 'eq :weakness 'key)
  "Per-buffer case-table fields, mirroring GNU's buffer-owned downcase table.")

(defun case-table--ensure-standard-initialized ()
  "Initialize the shared standard table lazily, without case conversion."
  (unless case-table--standard
    (let ((down (case-table--empty-table))
          (up (case-table--empty-table)))
      (if (fboundp 'case-table--initialize-standard)
          (progn
            (case-table--initialize-standard down up)
            (setq case-table--unicode-initialized t))
        ;; GNU casetab.c:248-285 initializes ASCII before Lisp Unicode data.
        (let ((character 0))
          (while (< character 128)
            (case-table--set-char-table-range
             down character
             (if (and (>= character ?A) (<= character ?Z))
                 (+ character 32) character))
            (case-table--set-char-table-range
             up character
             (if (and (>= character ?a) (<= character ?z))
                 (- character 32) character))
            (setq character (1+ character)))))
      (case-table--put-extra-slot down 0 up)
      (case-table--ensure-extra-slots down)
      (setq case-table--standard down case-table--initial-standard down)))
  case-table--standard)

(defun case-table--register-standard-data ()
  "Upgrade an already used ASCII bootstrap table when Unicode data arrives.
Keep the original object's identity and leave user-selected standard tables
alone.  An untouched bootstrap stays lazy until its first API call."
  (when (and case-table--initial-standard
             (not case-table--unicode-initialized))
    (let ((table case-table--initial-standard)
          (up (case-table--empty-table)))
      (case-table--initialize-standard table up)
      (case-table--put-extra-slot table 0 up)
      (case-table--put-extra-slot table 1 nil)
      (case-table--put-extra-slot table 2 nil)
      (case-table--ensure-extra-slots table)
      (setq case-table--unicode-initialized t))))

(defun standard-case-table ()
  "Return the standard case table."
  (case-table--ensure-standard-initialized))

(defun current-case-table ()
  "Return the case table of the current buffer."
  (or (gethash (current-buffer) case-table--buffer-tables)
      (let ((table (standard-case-table)))
        (puthash (current-buffer) table case-table--buffer-tables)
        table)))

(defun set-case-table (table)
  "Select TABLE as the case table of the current buffer.
TABLE is a `case-table' char-table with upcase, canonicalization and
equivalence tables in extra slots 0, 1 and 2.  Missing tables are derived."
  (case-table--check-table table)
  (case-table--ensure-extra-slots table)
  (puthash (current-buffer) table case-table--buffer-tables)
  (make-local-variable 'case-table--current)
  (make-local-variable 'nelisp--case-table)
  (setq case-table--current table nelisp--case-table table)
  table)

(defun set-standard-case-table (table)
  "Select TABLE as the standard case table used by newly created buffers."
  (case-table--check-table table)
  (case-table--ensure-extra-slots table)
  (let ((old (standard-case-table)))
    ;; GNU stores four case-table fields at buffer creation.  Materialize
    ;; untouched buffers before changing Lisp defaults to preserve that rule.
    (dolist (buffer (if (fboundp 'buffer-list) (buffer-list) (nelisp-buffer-list)))
      (unless (gethash buffer case-table--buffer-tables)
        (puthash buffer old case-table--buffer-tables)))
    (setq case-table--standard table)
    (set-default 'case-table--current table)
    (set-default 'nelisp--case-table table))
  table)

(defalias 'case-table--set-current #'set-case-table)
(defalias 'case-table--set-standard #'set-standard-case-table)

(defun case-table--copy-char-table (table)
  "Copy TABLE's local contents, default, parent and all extra slots.
This adapts chartab.c:copy_char_table until the core owns a Lisp clone."
  (if (case-table--sparse-table-p table)
      (emacs-char-table-copy table)
    (let* ((parent (char-table-parent table))
           (default (char-table-range table nil))
           (subtype (char-table-subtype table))
           (properties (copy-sequence (symbol-plist subtype)))
           (slot 0) (extras nil) done copy)
      (while (not done)
        (condition-case nil
            (progn
              (push (char-table-extra-slot table slot) extras)
              (setq slot (1+ slot)))
          (args-out-of-range (setq done t))))
      ;; A subtype's allocation property can change after TABLE was made.
      ;; GNU copies its actual slot count rather than consulting that property.
      (unwind-protect
          (progn
            (put subtype 'char-table-extra-slots slot)
            (setq copy (make-char-table subtype nil)))
        (setplist subtype properties))
      (unwind-protect
          (progn
            (set-char-table-parent table nil)
            (set-char-table-range table nil nil)
            (map-char-table
             (lambda (range value) (set-char-table-range copy range value)) table))
        (set-char-table-range table nil default)
        (set-char-table-parent table parent))
      (set-char-table-range copy nil default)
      (set-char-table-parent copy parent)
      (setq slot 0)
      (dolist (extra (nreverse extras))
        (set-char-table-extra-slot copy slot extra)
        (setq slot (1+ slot)))
      copy)))

(provide 'emacs-cc-casetab-1)
;;; emacs-cc-casetab-1.el ends here
