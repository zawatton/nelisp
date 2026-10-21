;;; nelisp-generate-unicode.el --- Freeze GNU 31.1 Unicode facts -*- lexical-binding: t; -*-

;; Recipe:
;;   emacs -Q --batch -l tools/nelisp-generate-unicode.el \
;;     --eval '(nelisp-generate-unicode-generate)'
;;
;; The generator must run on GNU Emacs 31.1.  General categories come from
;; `unicode-property-table-internal'; simple and full case mappings come from
;; that Emacs's character and string case operations.  Standard syntax classes
;; and case canonical/equivalence cycles come from its standard syntax and case
;; tables.  The generated file records the SHA-256 of the printed semantic
;; payload.

(require 'cl-lib)

(defconst nelisp-generate-unicode--limit #x10FFFF)
(defconst nelisp-generate-unicode--root
  (expand-file-name ".." (file-name-directory
                           (or load-file-name buffer-file-name))))

(defun nelisp-generate-unicode--compress-mappings (pairs)
  "Compress sorted character PAIRS into START END STEP DELTA records."
  (let (records)
    (while pairs
      (let* ((first (pop pairs))
             (start (car first))
             (end start)
             (delta (- (cdr first) start))
             (step nil))
        (while (and pairs
                    (= (- (cdar pairs) (caar pairs)) delta)
                    (let ((gap (- (caar pairs) end)))
                      (and (<= gap 2)
                           (or (null step) (= gap step)))))
          (let ((next (pop pairs)))
            (unless step (setq step (- (car next) end)))
            (setq end (car next))))
        (setq records (cons (list start end (or step 1) delta) records))))
    (nreverse records)))

(defun nelisp-generate-unicode--flat-vector (rows)
  "Return ROWS flattened into a vector."
  (apply #'vector (apply #'append rows)))

(defun nelisp-generate-unicode--collect-syntax ()
  "Collect GNU's standard syntax classes as contiguous scalar ranges."
  (let (raw ranges start end value)
    ;; The standard table already partitions the character space into ranges.
    ;; Ask `char-syntax' once per range because the stored char-table value is
    ;; an encoded syntax descriptor, not the class character returned by the
    ;; public operation.
    (with-syntax-table (standard-syntax-table)
      (map-char-table
       (lambda (range _descriptor)
         (let ((lo (if (consp range) (car range) range))
               (hi (if (consp range) (cdr range) range)))
           (when (and (integerp lo) (<= lo nelisp-generate-unicode--limit))
             (setq raw
                   (cons (list lo (min hi nelisp-generate-unicode--limit)
                               (char-syntax lo))
                         raw)))))
       (standard-syntax-table)))
    ;; Coalesce adjacent table partitions that expose the same class.
    (dolist (row (nreverse raw))
      (if (and value (= (1+ end) (car row)) (= value (nth 2 row)))
          (setq end (nth 1 row))
        (when value (setq ranges (cons (list start end value) ranges)))
        (setq start (car row) end (nth 1 row) value (nth 2 row))))
    (nreverse (cons (list start end value) ranges))))

;; Doc 225 P1: GNU's standard syntax table itself, not only its classes.
;; `init_syntax_once' (syntax.c:3659-3738) and characters.el leave every
;; character from 0 to MAX_CHAR with an explicit raw descriptor; the
;; runtime rebuilds the same char-table from these runs once, so a lookup
;; never computes a class.
(defun nelisp-generate-unicode--collect-syntax-entries ()
  "Collect GNU's standard syntax table as raw descriptor runs.
Each run is FROM TO CODE MATCH SHARED for the descriptor (CODE . MATCH).
SHARED is t when the descriptor is GNU's shared code object for CODE
\(`Vsyntax_code_object', syntax.c:3667-3669) and nil when it is a cons of
its own, as `init_syntax_once' makes for the string and escape classes."
  (let (rows)
    (map-char-table
     (lambda (range descriptor)
       (let* ((lo (if (consp range) (car range) range))
              (hi (if (consp range) (cdr range) range))
              (code (car descriptor))
              (shared (and (null (cdr descriptor)) (< code 16) (/= code 13)
                           (eq descriptor
                               (string-to-syntax
                                (string (aref " .w_()'\"$\\/<>@!|" code))))
                           t)))
         (push (list lo hi code (cdr descriptor) shared) rows)))
     (standard-syntax-table))
    (setq rows (nreverse rows))
    ;; The runs must tile 0..MAX_CHAR: the runtime installs nothing else.
    (let ((next 0))
      (dolist (row rows)
        (unless (= (car row) next)
          (error "Standard syntax runs leave a gap at %d" next))
        (setq next (1+ (nth 1 row))))
      (unless (= next (1+ (max-char)))
        (error "Standard syntax runs stop at %d" next)))
    rows))

;; Doc 225 S3: GNU's standard category table (category.c's
;; init_category_once plus characters.el).  Runs of non-empty category sets
;; refer to a table of distinct sets by their mnemonics; the runtime builds
;; the table once, sharing equal sets as category.c's hash does.
(defun nelisp-generate-unicode--collect-categories ()
  "Return (DOCSTRINGS SETS RUNS) for GNU's standard category table."
  (let* ((table (standard-category-table))
         (docstrings (copy-sequence (char-table-extra-slot table 0)))
         (index (make-hash-table :test 'equal))
         (sets nil) (runs nil))
    (map-char-table
     (lambda (range set)
       (let ((mnemonics (category-set-mnemonics set)))
         (unless (equal mnemonics "")
           (let ((i (gethash mnemonics index)))
             (unless i
               (setq i (hash-table-count index))
               (puthash mnemonics i index)
               (push mnemonics sets))
             (push (list (if (consp range) (car range) range)
                         (if (consp range) (cdr range) range) i)
                   runs)))))
     table)
    (list docstrings (apply #'vector (nreverse sets)) (nreverse runs))))

(defun nelisp-generate-unicode--collect ()
  "Collect category, case, and syntax facts from the running GNU Emacs."
  (unless (string-prefix-p "31.1" emacs-version)
    (error "Unicode data must be generated by GNU Emacs 31.1, got %s"
           emacs-version))
  (let* ((category-table (unicode-property-table-internal 'general-category))
         (standard-case (standard-case-table))
         (canonical-table (char-table-extra-slot standard-case 1))
         (equivalence-table (char-table-extra-slot standard-case 2))
        (category-names nil)
        (c 0) category-ranges category-start category-value
        simple-up simple-down simple-title full-up full-down full-title canonical equivalence)
    ;; char-code-property tables store compact numeric indices; extra slot 4
    ;; is the stable symbol vector used by `get-char-code-property'.
    (setq category-names (char-table-extra-slot category-table 4))
    (while (<= c nelisp-generate-unicode--limit)
      (let ((category (aref category-names (aref category-table c))))
        (if (eq category category-value)
            nil
          (when category-value
            (setq category-ranges
                  (cons (list category-start (1- c) category-value)
                        category-ranges)))
          (setq category-start c category-value category))
        (let ((canonical-value (aref canonical-table c))
              (equivalence-value (aref equivalence-table c)))
          (when (and (integerp canonical-value) (/= canonical-value c))
            (setq canonical (cons (cons c canonical-value) canonical)))
          (when (and (integerp equivalence-value) (/= equivalence-value c))
            (setq equivalence (cons (cons c equivalence-value) equivalence))))
        ;; Surrogates are category Cs and are not Unicode scalar values.
        (unless (eq category 'Cs)
          (let* ((source (string c))
                 (simple-upcase (upcase c))
                 (simple-downcase (downcase c))
                 (title-property (get-char-code-property c 'titlecase))
                 (upper (upcase source))
                 (lower (downcase source))
                 (title (upcase-initials source)))
            (unless (= simple-upcase c)
              (setq simple-up (cons (cons c simple-upcase) simple-up)))
            (unless (= simple-downcase c)
              (setq simple-down (cons (cons c simple-downcase) simple-down)))
            (when title-property
              (setq simple-title (cons (cons c title-property) simple-title)))
            (unless (equal upper (string simple-upcase))
              (setq full-up (cons (list c upper) full-up)))
            (unless (equal lower (string simple-downcase))
              (setq full-down (cons (list c lower) full-down)))
            (unless (equal title (string simple-upcase))
              (setq full-title (cons (list c title) full-title)))))
        (setq c (1+ c))))
    (setq category-ranges
          (nreverse (cons (list category-start nelisp-generate-unicode--limit
                                category-value)
                          category-ranges)))
    (list
     (nelisp-generate-unicode--flat-vector category-ranges)
     (nelisp-generate-unicode--flat-vector
      (nelisp-generate-unicode--compress-mappings (nreverse simple-up)))
     (nelisp-generate-unicode--flat-vector
      (nelisp-generate-unicode--compress-mappings (nreverse simple-down)))
     (nelisp-generate-unicode--flat-vector
      (nelisp-generate-unicode--compress-mappings (nreverse simple-title)))
     (nelisp-generate-unicode--flat-vector (nreverse full-up))
     (nelisp-generate-unicode--flat-vector (nreverse full-down))
     (nelisp-generate-unicode--flat-vector (nreverse full-title))
     (nelisp-generate-unicode--flat-vector
      (nelisp-generate-unicode--compress-mappings (nreverse canonical)))
     (nelisp-generate-unicode--flat-vector
      (nelisp-generate-unicode--compress-mappings (nreverse equivalence)))
     (nelisp-generate-unicode--flat-vector
      (nelisp-generate-unicode--collect-syntax))
     (nelisp-generate-unicode--flat-vector
      (nelisp-generate-unicode--collect-syntax-entries))
     (let ((categories (nelisp-generate-unicode--collect-categories)))
       (list (nth 0 categories) (nth 1 categories)
             (nelisp-generate-unicode--flat-vector (nth 2 categories)))))))

(defconst nelisp-generate-unicode--runtime
  "
(defun nelisp--unicode-range-value (table character width value-offset)
  \"Return CHARACTER's value from a sorted flat range TABLE.\"
  (let ((lo 0) (hi (1- (/ (length table) width))) found)
    (while (<= lo hi)
      (let* ((mid (/ (+ lo hi) 2)) (base (* mid width))
             (start (aref table base)) (end (aref table (1+ base))))
        (cond ((< character start) (setq hi (1- mid)))
              ((> character end) (setq lo (1+ mid)))
              (t (setq found (aref table (+ base value-offset)) lo (1+ hi))))))
    found))

(defun nelisp-unicode-category (character)
  \"Return GNU 31.1's general-category symbol for CHARACTER.\"
  (nelisp--unicode-range-value nelisp--unicode-category-ranges character 3 2))

(defun nelisp-unicode-syntax (character)
  \"Return GNU 31.1's standard syntax class for CHARACTER.
Raw characters above the Unicode scalar range retain the bootstrap word
default because the generated table deliberately covers Unicode scalars.\"
  (or (nelisp--unicode-range-value nelisp--unicode-syntax-ranges character 3 2)
      (if (>= character 128) ?w ?_)))

(defun nelisp--unicode-simple-map (table character)
  \"Map CHARACTER through sorted START END STEP DELTA records in TABLE.\"
  (let ((lo 0) (hi (1- (/ (length table) 4))) record)
    (while (<= lo hi)
      (let* ((mid (/ (+ lo hi) 2)) (base (* mid 4))
             (start (aref table base)) (end (aref table (1+ base))))
        (cond ((< character start) (setq hi (1- mid)))
              ((> character end) (setq lo (1+ mid)))
              (t (setq record base lo (1+ hi))))))
    (if (and record
             (= 0 (% (- character (aref table record))
                     (aref table (+ record 2)))))
        (+ character (aref table (+ record 3)))
      character)))

(defun nelisp-unicode-simple-upcase (character)
  \"Return GNU 31.1's simple uppercase mapping for CHARACTER.\"
  (if (and (>= character ?a) (<= character ?z))
      (- character 32)
    (nelisp--unicode-simple-map nelisp--unicode-simple-upcase-ranges character)))

(defun nelisp-unicode-simple-downcase (character)
  \"Return GNU 31.1's simple lowercase mapping for CHARACTER.\"
  (if (and (>= character ?A) (<= character ?Z))
      (+ character 32)
    (nelisp--unicode-simple-map nelisp--unicode-simple-downcase-ranges character)))

(defun nelisp-unicode-titlecase-property (character)
  \"Return CHARACTER's GNU titlecase property, or nil when absent.\"
  (if (and (>= character ?a) (<= character ?z))
      (- character 32)
    (let ((table nelisp--unicode-titlecase-property-ranges)
          (lo 0) (hi (1- (/ (length nelisp--unicode-titlecase-property-ranges) 4))) record)
      (while (<= lo hi)
        (let* ((mid (/ (+ lo hi) 2)) (base (* mid 4)))
          (cond ((< character (aref table base)) (setq hi (1- mid)))
                ((> character (aref table (1+ base))) (setq lo (1+ mid)))
                (t (setq record base lo (1+ hi))))))
      (and record
           (= 0 (% (- character (aref table record)) (aref table (+ record 2))))
           (+ character (aref table (+ record 3)))))))

(defun nelisp-unicode-canonical-character (character)
  \"Return CHARACTER's GNU 31.1 standard case canonical representative.\"
  (nelisp--unicode-simple-map nelisp--unicode-canonical-ranges character))

(defun nelisp-unicode-equivalent-character (character)
  \"Return CHARACTER's next GNU 31.1 standard case equivalent.\"
  (nelisp--unicode-simple-map nelisp--unicode-equivalence-ranges character))

(defun nelisp--unicode-full-map (table character fallback)
  \"Return CHARACTER's string mapping from TABLE, or FALLBACK.\"
  (let ((lo 0) (hi (1- (/ (length table) 2))) value)
    (while (<= lo hi)
      (let* ((mid (/ (+ lo hi) 2)) (base (* mid 2))
             (key (aref table base)))
        (cond ((< character key) (setq hi (1- mid)))
              ((> character key) (setq lo (1+ mid)))
              (t (setq value (aref table (1+ base)) lo (1+ hi))))))
    (or value (char-to-string fallback))))

(defun nelisp--unicode-word-character-p (character)
  \"Return non-nil when CHARACTER continues a case-conversion word.\"
  (let ((syntax (char-syntax character)))
    (or (eq syntax ?w)
        (and (boundp 'case-symbols-as-words) case-symbols-as-words
             (eq syntax ?_)))))

(defun nelisp-unicode-case-string (string action)
  \"Case STRING using frozen GNU 31.1 mappings.
ACTION is `upper', `lower', or `title'; title preserves noninitial case.\"
  (let ((i 0) (n (length string)) (initial t) pieces)
    (while (< i n)
      (let* ((c (aref string i))
             (word (nelisp--unicode-word-character-p c))
             (piece
              (cond
               ((eq action 'upper)
                (nelisp--unicode-full-map nelisp--unicode-full-upcase c
                                          (nelisp-unicode-simple-upcase c)))
               ((eq action 'lower)
                ;; Unicode final sigma depends on immediate string context;
                ;; GNU casefiddle.c applies it after the ordinary full map.
                (if (and (= c #x03A3) (not initial)
                         (or (= (1+ i) n)
                             (not (nelisp--unicode-word-character-p
                                   (aref string (1+ i))))))
                    (char-to-string #x03C2)
                  (nelisp--unicode-full-map nelisp--unicode-full-downcase c
                                            (nelisp-unicode-simple-downcase c))))
               ((and (eq action 'title) initial)
                (nelisp--unicode-full-map nelisp--unicode-full-titlecase c
                                          (nelisp-unicode-simple-upcase c)))
               (t (char-to-string c)))))
        (setq pieces (cons piece pieces)
              initial (not word)
              i (1+ i))))
    (apply #'concat (nreverse pieces))))

(provide 'nelisp-stdlib-unicode)
;;; nelisp-stdlib-unicode.el ends here
")

(defun nelisp-generate-unicode-generate (&optional output)
  "Generate the private Unicode module at OUTPUT."
  (let* ((output (or output (expand-file-name "lisp/nelisp-stdlib-unicode.el"
                                               nelisp-generate-unicode--root)))
         ;; The last datum is the category triple; splice it in.
         (data (let ((all (nelisp-generate-unicode--collect)))
                 (append (butlast all) (car (last all)))))
         (payload (prin1-to-string data))
         (hash (secure-hash 'sha256 payload))
         (names '(nelisp--unicode-category-ranges
                  nelisp--unicode-simple-upcase-ranges
                  nelisp--unicode-simple-downcase-ranges
                  nelisp--unicode-titlecase-property-ranges
                  nelisp--unicode-full-upcase
                  nelisp--unicode-full-downcase
                  nelisp--unicode-full-titlecase
                  nelisp--unicode-canonical-ranges
                  nelisp--unicode-equivalence-ranges
                  nelisp--unicode-syntax-ranges
                  nelisp--unicode-syntax-entries
                  nelisp--unicode-category-docstrings
                  nelisp--unicode-category-sets
                  nelisp--unicode-category-runs)))
    (with-temp-file output
      (insert ";;; nelisp-stdlib-unicode.el --- Generated GNU Unicode facts -*- lexical-binding: t; -*-\n\n")
      (insert ";; Generated by tools/nelisp-generate-unicode.el on GNU Emacs "
              emacs-version ".\n")
      (insert ";; Semantic payload SHA-256: " hash "\n")
      (insert ";; Do not edit this file; rerun the generator recipe beside its source.\n\n")
      (insert "(defconst nelisp--unicode-data-sha256 " (prin1-to-string hash) ")\n")
      (while names
        (insert "(defconst " (symbol-name (pop names)) "\n  ")
        (prin1 (pop data) (current-buffer))
        (insert ")\n\n"))
      (insert nelisp-generate-unicode--runtime))
    (message "Wrote %s (%s)" output hash)))

(provide 'nelisp-generate-unicode)
;;; nelisp-generate-unicode.el ends here
