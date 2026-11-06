;;; nelisp-native-rooted-prelink-closure.el --- Elisp port of the rooted prelink closure proof  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The NeLisp authors.

;;; Commentary:

;; In-process port of scripts/nelisp-native-rooted-prelink-closure.py.  It
;; bounds direct native ownership of the selected compiler units and writes
;; `json.dumps(result, indent=2) + "\n"' byte for byte.  The Python script
;; remains the reference; tools/ai/prelink-closure-diff/run.sh proves the two
;; agree.  Instruction decoding comes from `nelisp-x86-64-decode', which
;; matches capstone's CS_GRP_CALL / CS_GRP_JUMP / X86_OP_IMM view exactly and
;; signals where capstone would stop decoding.
;;
;; Output values use a tiny model so the emitter can mirror Python:
;;   object  (:object ("key" . value) ...)   array  a vector
;;   string, integer as is; t = true, :false = false, :null = null.

;;; Code:

(require 'cl-lib)

(let ((dir (file-name-directory (or load-file-name buffer-file-name
                                    default-directory))))
  (unless (featurep 'nelisp-x86-64-decode)
    (unless (fboundp 'nelisp-x86-64-decode-insn)
      (let ((candidate (expand-file-name "../lisp/nelisp-x86-64-decode.el" dir)))
        (if (file-exists-p candidate)
            (load candidate nil t)
          (require 'nelisp-x86-64-decode))))))

(defconst nelisp-native-rooted-prelink-closure--missing (make-symbol "missing"))

(defconst nelisp-native-rooted-prelink-closure--hex-values
  (let ((v (make-vector 256 0)))
    (dotimes (i 10) (aset v (+ ?0 i) i))
    (dotimes (i 6) (aset v (+ ?a i) (+ 10 i)))
    v))

;;;; JSON input

(defun nelisp-native-rooted-prelink-closure--parse (bytes)
  "Parse unibyte UTF-8 JSON BYTES into hash tables, lists and strings."
  (let ((text (decode-coding-string bytes 'utf-8-unix)))
    (when (memq 'eight-bit (find-charset-string text))
      (error "Invalid UTF-8 in JSON input"))
    (json-parse-string text :object-type 'hash-table :array-type 'list
                       :null-object :null :false-object :false)))

(defun nelisp-native-rooted-prelink-closure--ref (object key)
  "Return KEY of hash table OBJECT, failing like Python's KeyError."
  (unless (hash-table-p object)
    (error "Expected a JSON object while reading %s" key))
  (let ((value (gethash key object nelisp-native-rooted-prelink-closure--missing)))
    (when (eq value nelisp-native-rooted-prelink-closure--missing)
      (error "Missing key: %s" key))
    value))

(defun nelisp-native-rooted-prelink-closure--get (object key default)
  "Return KEY of hash table OBJECT, or DEFAULT when absent."
  (unless (hash-table-p object)
    (error "Expected a JSON object while reading %s" key))
  (gethash key object default))

(defun nelisp-native-rooted-prelink-closure--has (object key)
  "Non-nil when hash table OBJECT contains KEY."
  (unless (hash-table-p object)
    (error "Expected a JSON object while reading %s" key))
  (not (eq (gethash key object nelisp-native-rooted-prelink-closure--missing)
           nelisp-native-rooted-prelink-closure--missing)))

;;;; Files and paths

(defun nelisp-native-rooted-prelink-closure--resolve (path)
  "Return the symlink-resolved absolute PATH without a trailing slash."
  (directory-file-name (file-truename (expand-file-name path))))

(defun nelisp-native-rooted-prelink-closure--resolve-in (directory-real name)
  "Resolve NAME inside the already symlink-resolved DIRECTORY-REAL.
Equal to `nelisp-native-rooted-prelink-closure--resolve' of the joined path.
A plain final component that is not a symlink is already resolved, so only
the other cases pay for `file-truename' (which walks every component and
cost ~0.2 s per proof over the unit files)."
  (let ((path (concat (file-name-as-directory directory-real) name)))
    (if (or (member name '("" "." "..")) (string-search "/" name)
            (file-symlink-p path))
        (nelisp-native-rooted-prelink-closure--resolve path)
      path)))

(defun nelisp-native-rooted-prelink-closure--parent (path)
  "Return the parent directory of resolved PATH."
  (directory-file-name (file-name-directory (directory-file-name path))))

(defun nelisp-native-rooted-prelink-closure--py-name (string)
  "Return what Python's Path(STRING).name yields."
  (let ((parts (cl-remove-if (lambda (part) (member part '("" ".")))
                             (split-string string "/"))))
    (if parts (car (last parts)) "")))

(defun nelisp-native-rooted-prelink-closure--size (path)
  "Return the size of PATH following symlinks, or fail when it is missing."
  (let ((attributes (file-attributes (file-chase-links path))))
    (unless attributes
      (error "No such file: %s" (file-name-nondirectory path)))
    (file-attribute-size attributes)))

(defun nelisp-native-rooted-prelink-closure--bounded-read (path limit)
  "Read PATH as unibyte bytes after requiring 0 < size <= LIMIT."
  (let ((size (nelisp-native-rooted-prelink-closure--size path)))
    (unless (and (> size 0) (<= size limit)
                 (not (file-directory-p path)))
      (error "File eligibility bound: %s" (file-name-nondirectory path)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally path)
      (buffer-string))))

(defun nelisp-native-rooted-prelink-closure--sha (bytes)
  "Return the sha256 hex digest of unibyte BYTES."
  (secure-hash 'sha256 bytes))

(defconst nelisp-native-rooted-prelink-closure--text-regexp
  "(text :nelisp-cache-bytes-hex \"\\([0-9a-f]+\\)\""
  "Matches the hexadecimal text section of a unit file.")

(defun nelisp-native-rooted-prelink-closure--hex-slice (raw hex-start start end)
  "Decode bytes [START, END) of the hex run at HEX-START in unibyte RAW."
  (let* ((table nelisp-native-rooted-prelink-closure--hex-values)
         (count (- end start))
         (out (make-string count 0))
         (i 0)
         (q (+ hex-start (* 2 start))))
    (while (< i count)
      (aset out i (logior (ash (aref table (aref raw q)) 4)
                          (aref table (aref raw (1+ q)))))
      (setq q (+ q 2) i (1+ i)))
    out))

;;;; Manifest verification

(defun nelisp-native-rooted-prelink-closure-verify-manifest
    (manifest metadata directory data-owner source-root)
  "Bind generation inputs to the actual active build; return the manifest digest."
  (let* ((raw (nelisp-native-rooted-prelink-closure--bounded-read manifest 65536))
         (document (nelisp-native-rooted-prelink-closure--parse raw))
         (ref #'nelisp-native-rooted-prelink-closure--ref))
    (unless (equal (nelisp-native-rooted-prelink-closure--get
                    document "domain" nil)
                   "nelisp-rooted-active-build-v1")
      (error "Unknown active build manifest"))
    (let ((relative (funcall ref document "builder-source")))
      (unless (stringp relative) (error "Builder source is not a path"))
      (when (or (string-prefix-p "/" relative)
                (member ".." (split-string relative "/")))
        (error "Builder source provenance escapes root"))
      (let* ((root (nelisp-native-rooted-prelink-closure--resolve source-root))
             (prefix (file-name-as-directory root))
             (builder (nelisp-native-rooted-prelink-closure--resolve
                       (concat prefix relative))))
        (unless (and (string-prefix-p prefix builder)
                     (> (length builder) (length prefix)))
          (error "Builder source owner escapes root"))
        (unless (equal (nelisp-native-rooted-prelink-closure--sha
                        (nelisp-native-rooted-prelink-closure--bounded-read
                         builder (* 8 1024 1024)))
                       (funcall ref document "builder-sha256"))
          (error "Missing or changed builder source"))))
    (let* ((metadata-bytes (nelisp-native-rooted-prelink-closure--bounded-read
                            metadata (* 4 1024 1024)))
           (data-bytes (nelisp-native-rooted-prelink-closure--bounded-read
                        data-owner (* 1024 1024))))
      (unless (and (equal (nelisp-native-rooted-prelink-closure--sha metadata-bytes)
                          (funcall ref document "metadata-sha256"))
                   (equal (nelisp-native-rooted-prelink-closure--sha data-bytes)
                          (funcall ref document "generated-data-sha256")))
        (error "Changed active metadata or data owner"))
      (let ((records (funcall ref document "units"))
            (selected (make-hash-table :test 'equal))
            (text-units (make-hash-table :test 'equal))
            (total 0)
            (directory-real (nelisp-native-rooted-prelink-closure--resolve directory))
            (case-fold-search nil))
        (unless (and (listp records) (<= 1 (length records) 128))
          (error "Active build unit count bound"))
        (dolist (record records)
          (let ((name (funcall ref record "name"))
                (filename (funcall ref record "path")))
            (unless (and (stringp name) (stringp filename))
              (error "Duplicate or malformed active unit selection"))
            (when (or (gethash name selected)
                      (string-search "/" filename) (equal filename ".")
                      (string-search "/" name) (equal name "."))
              (error "Duplicate or malformed active unit selection"))
            (let ((path (nelisp-native-rooted-prelink-closure--resolve-in
                         directory-real filename)))
              (unless (equal (nelisp-native-rooted-prelink-closure--parent path)
                             directory-real)
                (error "Active unit file escapes owner directory"))
              (let ((unit-bytes (nelisp-native-rooted-prelink-closure--bounded-read
                                 path (* 16 1024 1024))))
                (setq total (+ total (length unit-bytes)))
                (when (> total (* 32 1024 1024))
                  (error "Active unit aggregate input bound"))
                (unless (equal (nelisp-native-rooted-prelink-closure--sha unit-bytes)
                               (funcall ref record "unit-sha256"))
                  (error "Missing or changed active unit source"))
                (when (string-match nelisp-native-rooted-prelink-closure--text-regexp
                                    unit-bytes)
                  (puthash name t text-units))
                (puthash name record selected)))))
        (let ((units (nelisp-native-rooted-prelink-closure--parse metadata-bytes))
              (names (make-hash-table :test 'equal)))
          (unless (listp units) (error "Active unit metadata is not a list"))
          (dolist (unit units)
            (puthash (funcall ref unit "name") t names))
          (unless (and (= (length units) (hash-table-count text-units))
                       (= (hash-table-count names) (hash-table-count text-units))
                       (let ((same t))
                         (maphash (lambda (name _) (unless (gethash name text-units)
                                                     (setq same nil)))
                                  names)
                         same))
            (error "Incomplete active text unit metadata"))
          (dolist (unit units)
            (let ((record (gethash (funcall ref unit "name") selected)))
              (unless record (error "Active unit metadata differs from manifest"))
              (unless (and (equal (funcall ref unit "path") (funcall ref record "path"))
                           (equal (funcall ref unit "unit-sha256")
                                  (funcall ref record "unit-sha256")))
                (error "Active unit metadata differs from manifest")))))
        (let ((data (nelisp-native-rooted-prelink-closure--parse data-bytes)))
          (unless (and (gethash (funcall ref data "unit") selected)
                       (equal (funcall ref data "owner-source-sha256")
                              (funcall ref document "builder-sha256")))
            (error "Generated data owner is not active build source")))
        (nelisp-native-rooted-prelink-closure--sha raw)))))

;;;; Unit owners

(defun nelisp-native-rooted-prelink-closure--unit-owners (metadata directory)
  "Return a hash table of function name -> owner vector for every unit.
Owner: [UNIT START END RAW HEX-START STARTS UNIT-STATE UNIT-INDEX SHA].
UNIT-STATE is [RELOCATIONS VALUES BUCKETS] shared by the unit's owners."
  (let* ((ref #'nelisp-native-rooted-prelink-closure--ref)
         (owners (make-hash-table :test 'equal))
         (units (nelisp-native-rooted-prelink-closure--parse
                 (nelisp-native-rooted-prelink-closure--bounded-read
                  metadata (* 4 1024 1024))))
         (directory-real (nelisp-native-rooted-prelink-closure--resolve directory))
         (total-unit-bytes 0)
         (case-fold-search nil))
    (unless (and (listp units) (<= (length units) 128))
      (error "Unit metadata bound"))
    (dolist (unit units)
      (let ((relocations (funcall ref unit "relocations")))
        (if (nelisp-native-rooted-prelink-closure--has unit "relocation-symbols")
            (let ((dictionary (funcall ref unit "relocation-symbols")))
              (unless (equal (nelisp-native-rooted-prelink-closure--get
                              unit "relocation-symbol-format" nil)
                             "dictionary-v1")
                (error "Unknown relocation symbol format"))
              (unless (and (listp dictionary) (cl-every #'stringp dictionary))
                (error "Invalid relocation symbol dictionary"))
              (let ((vector (vconcat dictionary)))
                (dolist (relocation relocations)
                  (let ((index (funcall ref relocation "symbol")))
                    (unless (and (integerp index) (<= 0 index) (< index (length vector)))
                      (error "Invalid relocation symbol index"))
                    (puthash "symbol" (aref vector index) relocation)))))
          (when (or (nelisp-native-rooted-prelink-closure--has
                     unit "relocation-symbol-format")
                    (cl-some (lambda (r) (not (stringp (funcall ref r "symbol"))))
                             relocations))
            (error "Invalid relocation symbol")))
        (let* ((path (nelisp-native-rooted-prelink-closure--resolve-in
                      directory-real
                      (nelisp-native-rooted-prelink-closure--py-name
                       (funcall ref unit "path")))))
          (unless (equal (nelisp-native-rooted-prelink-closure--parent path)
                         directory-real)
            (error "Unit source escapes its owner directory"))
          (setq total-unit-bytes
                (+ total-unit-bytes (nelisp-native-rooted-prelink-closure--size path)))
          (when (> total-unit-bytes (* 32 1024 1024))
            (error "Aggregate unit source input bound"))
          (let ((raw (nelisp-native-rooted-prelink-closure--bounded-read
                      path (* 16 1024 1024))))
            (unless (equal (nelisp-native-rooted-prelink-closure--sha raw)
                           (funcall ref unit "unit-sha256"))
              (error "Unit source hash differs"))
            (unless (string-match nelisp-native-rooted-prelink-closure--text-regexp raw)
              (error "Unknown unit text encoding"))
            (let* ((hex-start (match-beginning 1))
                   (hex-length (- (match-end 1) hex-start))
                   (text-length (/ hex-length 2))
                   (functions nil))
              (when (= (% hex-length 2) 1)
                (error "Odd-length unit text hex"))
              (dolist (symbol (funcall ref unit "symbols"))
                (when (and (equal (funcall ref symbol "section") "text")
                           (equal (funcall ref symbol "type") "func"))
                  (push symbol functions)))
              (setq functions (sort (nreverse functions)
                                    (lambda (a b) (< (funcall ref a "value")
                                                     (funcall ref b "value")))))
              (let ((starts (make-hash-table :test 'equal))
                    (count (length functions)))
                (when (> count 20000)
                  (error "Ambiguous unit function boundaries"))
                (dolist (function functions)
                  (puthash (funcall ref function "value")
                           (funcall ref function "name") starts))
                (unless (= (hash-table-count starts) count)
                  (error "Ambiguous unit function boundaries"))
                (let* ((values (vconcat (mapcar (lambda (f) (funcall ref f "value"))
                                                functions)))
                       (state (vector relocations values nil text-length))
                       (index 0))
                  (dolist (function functions)
                    (let* ((start (aref values index))
                           (end (if (< (1+ index) count) (aref values (1+ index))
                                  text-length))
                           (name (funcall ref function "name")))
                      (unless (and (<= 0 start) (< start end) (<= end text-length)
                                   (not (gethash name owners)))
                        (error "Invalid or duplicate unit function owner: %s in %s"
                               name (funcall ref unit "name")))
                      (puthash name (vector (funcall ref unit "name") start end raw
                                            hex-start starts state index
                                            (funcall ref unit "unit-sha256"))
                               owners))
                    (setq index (1+ index))))))))))
    owners))

(defun nelisp-native-rooted-prelink-closure--owner-relocations (owner)
  "Return the text relocations of OWNER in their original order."
  (let* ((state (aref owner 6))
         (buckets (aref state 2)))
    (unless buckets
      (let* ((values (aref state 1))
             (count (length values))
             (text-length (aref state 3)))
        (setq buckets (make-vector count nil))
        (dolist (r (aref state 0))
          (when (equal (nelisp-native-rooted-prelink-closure--ref r "section")
                       "text")
            (let* ((offset (nelisp-native-rooted-prelink-closure--ref r "offset"))
                   (low 0) (high count))
              ;; bisect_right(values, offset) - 1
              (while (< low high)
                (let ((mid (/ (+ low high) 2)))
                  (if (< offset (aref values mid))
                      (setq high mid)
                    (setq low (1+ mid)))))
              (let ((index (1- low)))
                (when (and (>= index 0)
                           (< offset (if (< (1+ index) count)
                                         (aref values (1+ index))
                                       text-length)))
                  (aset buckets index (cons r (aref buckets index))))))))
        (dotimes (i count) (aset buckets i (nreverse (aref buckets i))))
        (aset state 2 buckets)))
    (aref buckets (aref owner 7))))

;;;; Proof

(defun nelisp-native-rooted-prelink-closure-prove
    (metadata directory roots data-owner &optional claimed max-functions
              evaluator-boundary)
  "Reject unresolved edges; return the proof as an output-model object.
EVALUATOR-BOUNDARY is nil or a list of boundary function names."
  (let* ((ref #'nelisp-native-rooted-prelink-closure--ref)
         (max-functions (or max-functions 128))
         (boundaries evaluator-boundary))
    (unless (cl-every (lambda (b) (member b '("nl_apply_function" "nl_apply_function_w")))
                      boundaries)
      (error "Unknown evaluator boundary"))
    (unless (memql max-functions '(128 192 200 272 280 288 296 304 320 352 384))
      (error "Unknown direct helper count policy"))
    (when (and (memql max-functions '(200 272 280 288 296 304 320 352 384))
               (not (member "nl_native_frame_v2" roots)))
      (error "Frame closure bound requires the authenticated frame root"))
    (let ((owners (nelisp-native-rooted-prelink-closure--unit-owners metadata directory)))
      (when (and (memql max-functions '(288 296 304 320 352 384))
                 (not (cl-every (lambda (n) (gethash n owners))
                                '("nl_n4_mark_env" "nl_n4_record" "nl_n3_notify"))))
        (error "C-core F1 closure requires activation and watcher owners"))
      (let* ((units (nelisp-native-rooted-prelink-closure--parse
                     (nelisp-native-rooted-prelink-closure--bounded-read
                      metadata (* 4 1024 1024))))
             (data-bytes (nelisp-native-rooted-prelink-closure--bounded-read
                          data-owner (* 1024 1024)))
             (data (nelisp-native-rooted-prelink-closure--parse data-bytes))
             (bss-size (nelisp-native-rooted-prelink-closure--get data "bss-size" 0))
             (data-names (make-hash-table :test 'equal))
             (os-imports nil)
             (known (make-hash-table :test 'equal)))
        (unless (and (numberp bss-size) (< 0 bss-size) (<= bss-size (* 16 1024 1024)))
          (error "Generated data owner size bound"))
        (dolist (symbol (nelisp-native-rooted-prelink-closure--get data "symbols" nil))
          (when (equal (funcall ref symbol "section") "bss")
            (let ((value (funcall ref symbol "value"))
                  (name (funcall ref symbol "name")))
              (when (or (not (and (<= 0 value) (< value bss-size)))
                        (gethash name data-names))
                (error "Generated data boundary or duplicate owner"))
              (puthash name t data-names))))
        ;; Only these reader-owned kernel32 terminals extend the Win64 closure.
        (when (equal (nelisp-native-rooted-prelink-closure--get data "target" nil)
                     "windows-x86_64")
          (setq os-imports '("ExitProcess" "VirtualAlloc" "VirtualFree")))
        (maphash (lambda (name _) (puthash name t known)) owners)
        (maphash (lambda (name _) (puthash name t known)) data-names)
        (dolist (name os-imports) (puthash name t known))
        (dolist (unit units)
          (dolist (symbol (funcall ref unit "symbols"))
            (when (member (funcall ref symbol "section") '("rodata" "data" "bss"))
              (puthash (funcall ref symbol "name") t known))))
        (let* ((pending (copy-sequence roots))
               (tail (last pending))
               (records nil)
               (seen (make-hash-table :test 'equal))
               (total 0))
          (while pending
            (let ((name (pop pending)))
              (unless pending (setq tail nil))
              (unless (gethash name seen)
                (let ((owner (gethash name owners)))
                  (unless owner (error "Missing direct function owner: %s" name))
                  (when (>= (hash-table-count seen) max-functions)
                    (error "Direct helper count bound"))
                  (let* ((start (aref owner 1))
                         (end (aref owner 2))
                         (size (- end start)))
                    ;; 640 KiB bounds the measured F1 closures (Win64 and Linux);
                    ;; the largest helper is nl_native_frame_v2.
                    (unless (and (< 0 size) (<= size 65536)
                                 (<= (+ total size) (* 640 1024)))
                      (error "Direct helper text size bound"))
                    (let* ((text (nelisp-native-rooted-prelink-closure--hex-slice
                                  (aref owner 3) (aref owner 4) start end))
                           (normalized (copy-sequence text))
                           (relocations (make-hash-table :test 'eql))
                           (starts (aref owner 5))
                           (direct nil)
                           (decoded 0)
                           (pos 0))
                      (dolist (relocation (nelisp-native-rooted-prelink-closure--owner-relocations
                                           owner))
                        (let ((position (- (funcall ref relocation "offset") start))
                              (type (funcall ref relocation "type"))
                              (symbol (funcall ref relocation "symbol")))
                          (when (or (not (member type '("pc32" "plt32")))
                                    (not (and (<= 0 position) (<= position (- size 4))))
                                    (not (gethash symbol known))
                                    (gethash position relocations))
                            (error "Unknown, overlapping or crossing prelink relocation: owner=%s offset=%s size=%s type=%s symbol=%s known=%s"
                                   name position size type symbol
                                   (if (gethash symbol known) "True" "False")))
                          (puthash position relocation relocations)
                          (aset normalized position 0)
                          (aset normalized (+ position 1) 0)
                          (aset normalized (+ position 2) 0)
                          (aset normalized (+ position 3) 0)))
                      (condition-case nil
                          (while (< pos size)
                            (let* ((insn (nelisp-x86-64-decode-insn
                                          text pos size (+ start pos)))
                                   (length (aref insn 0)))
                              (setq decoded (+ decoded length))
                              (when (aref insn 2)
                                (unless (eq (aref insn 3) 'imm)
                                  (error "Indirect control flow refused: %s" name))
                                (let* ((relocation (gethash (+ pos (aref insn 4))
                                                            relocations))
                                       (target nil))
                                  (if relocation
                                      (progn
                                        (unless (and (eql (aref insn 5) 4)
                                                     (equal (funcall ref relocation "addend") 0))
                                          (error "Unknown direct relocation encoding"))
                                        (setq target (funcall ref relocation "symbol")))
                                    (let ((address (aref insn 6)))
                                      (setq target (gethash address starts))
                                      (when (and (null target) (<= start address)
                                                 (< address end))
                                        (setq target 'internal))))
                                  (unless (eq target 'internal)
                                    (unless (or (gethash target owners)
                                                (member target os-imports))
                                      (error "Unresolved direct control flow: %s" name))
                                    (push (list :object (cons "offset" pos)
                                                (cons "mnemonic" (aref insn 1))
                                                (cons "target" target))
                                          direct)
                                    (when (and (not (member name boundaries))
                                               (not (member target os-imports)))
                                      (if tail
                                          (setcdr tail (setq tail (list target)))
                                        (setq pending (list target)
                                              tail pending))))))
                              (setq pos (+ pos length))))
                        (nelisp-x86-64-decode-error nil))
                      (unless (= decoded size)
                        (error "Incomplete prelink instruction decoding"))
                      (push (append
                             (list :object
                                   (cons "name" name)
                                   (cons "unit" (aref owner 0))
                                   (cons "unit_offset" start)
                                   (cons "size" size)
                                   (cons "normalized_sha256"
                                         (nelisp-native-rooted-prelink-closure--sha normalized))
                                   (cons "direct" (vconcat (nreverse direct)))
                                   (cons "indirect" []))
                             (when (member name boundaries)
                               (list (cons "evaluator_boundary" t))))
                            records)
                      (puthash name t seen)
                      (setq total (+ total size))))))))
          (let ((result
                 (list :object
                       (cons "domain" "nelisp-rooted-prelink-text-v1")
                       (cons "roots" (vconcat roots))
                       (cons "records" (vconcat (nreverse records)))
                       (cons "total_bytes" total)
                       (cons "status" "PRELINK_DIRECT_CLOSURE_PASS")
                       (cons "metadata_sha256"
                             (nelisp-native-rooted-prelink-closure--sha
                              (nelisp-native-rooted-prelink-closure--bounded-read
                               metadata (* 4 1024 1024))))
                       (cons "generated_data_sha256"
                             (nelisp-native-rooted-prelink-closure--sha data-bytes))
                       (cons "runtime_memory" "pending")
                       (cons "operation_eligibility" []))))
            (when os-imports
              (nelisp-native-rooted-prelink-closure-object-set
               result "os_imports" (vconcat (sort (copy-sequence os-imports) #'string<))))
            (when (and claimed (not (equal claimed result)))
              (error "Claimed prelink proof drops or changes ownership"))
            result))))))

;;;; Output model, JSON emitter

(defun nelisp-native-rooted-prelink-closure-object-set (object key value)
  "Set KEY of output-model OBJECT to VALUE in place, appending when new."
  (let ((cell (assoc key (cdr object))))
    (if cell
        (setcdr cell value)
      (nconc object (list (cons key value))))
    object))

(defconst nelisp-native-rooted-prelink-closure--escape-regexp
  (concat "[\0-\37\177\"\\\\[:nonascii:]" (string #x3fff80) "-" (string #x3fffff) "]")
  "Characters Python's ensure_ascii encoder escapes or must write specially.")

(defun nelisp-native-rooted-prelink-closure--insert-string (string)
  "Insert STRING as Python `json.dumps' (ensure_ascii) would."
  (insert ?\")
  (if (not (string-match-p nelisp-native-rooted-prelink-closure--escape-regexp
                           string))
      (insert string)
    (dolist (c (append string nil))
      (cond
       ((eq c ?\") (insert "\\\""))
       ((eq c ?\\) (insert "\\\\"))
       ((eq c ?\n) (insert "\\n"))
       ((eq c ?\r) (insert "\\r"))
       ((eq c ?\t) (insert "\\t"))
       ((eq c 8) (insert "\\b"))
       ((eq c 12) (insert "\\f"))
       ((and (>= c 32) (< c 127)) (insert c))
       ((< c #x10000) (insert (format "\\u%04x" c)))
       ((<= c #x10ffff)
        (let ((n (- c #x10000)))
          (insert (format "\\u%04x\\u%04x"
                          (logior #xd800 (logand (ash n -10) #x3ff))
                          (logior #xdc00 (logand n #x3ff))))))
       ;; Raw bytes round-trip as Python's surrogateescape code points.
       ((>= c #x3fff80) (insert (format "\\u%04x" (+ #xdc00 (- c #x3fff00)))))
       (t (error "Unencodable character in JSON string")))))
  (insert ?\"))

(defun nelisp-native-rooted-prelink-closure--insert-value (value level)
  "Insert VALUE at nesting LEVEL like `json.dumps(..., indent=2)'."
  (cond
   ((eq value :null) (insert "null"))
   ((eq value t) (insert "true"))
   ((eq value :false) (insert "false"))
   ((integerp value) (insert (number-to-string value)))
   ((stringp value)
    (nelisp-native-rooted-prelink-closure--insert-string value))
   ((vectorp value)
    (if (= (length value) 0)
        (insert "[]")
      (let ((inner (make-string (* 2 (1+ level)) ?\s))
            (first t))
        (insert "[")
        (mapc (lambda (item)
                (insert (if first "\n" ",\n") inner)
                (setq first nil)
                (nelisp-native-rooted-prelink-closure--insert-value
                 item (1+ level)))
              value)
        (insert "\n" (make-string (* 2 level) ?\s) "]"))))
   ((and (consp value) (eq (car value) :object))
    (if (null (cdr value))
        (insert "{}")
      (let ((inner (make-string (* 2 (1+ level)) ?\s))
            (first t))
        (insert "{")
        (dolist (pair (cdr value))
          (insert (if first "\n" ",\n") inner)
          (setq first nil)
          (nelisp-native-rooted-prelink-closure--insert-string (car pair))
          (insert ": ")
          (nelisp-native-rooted-prelink-closure--insert-value (cdr pair) (1+ level)))
        (insert "\n" (make-string (* 2 level) ?\s) "}"))))
   (t (error "Unsupported JSON value: %S" value))))

(defun nelisp-native-rooted-prelink-closure-encode (value)
  "Return `json.dumps(VALUE, indent=2) + \"\\n\"' as an ASCII string."
  (with-temp-buffer
    (nelisp-native-rooted-prelink-closure--insert-value value 0)
    (insert "\n")
    (buffer-string)))

(defun nelisp-native-rooted-prelink-closure-write-output (path value)
  "Write VALUE's JSON encoding to PATH as bytes; return the sha256 hex line."
  (let ((text (nelisp-native-rooted-prelink-closure-encode value)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert text)
      (let ((coding-system-for-write 'no-conversion))
        (write-region (point-min) (point-max) path nil 'silent)))
    (concat (secure-hash 'sha256 text) "\n")))

;;;; Command line

(defun nelisp-native-rooted-prelink-closure-parse-args (args options)
  "Parse ARGS like argparse for OPTIONS, a list of option names without dashes.
Return an alist of (NAME . VALUE); the name \"root\" collects a list in order.
Unknown options, positionals and missing values signal an error."
  (let (result)
    (while args
      (let* ((arg (pop args))
             (name nil) (value nil))
        (unless (and (stringp arg) (string-prefix-p "--" arg))
          (error "Unrecognized argument: %s" arg))
        (if (string-match "\\`--\\([^=]+\\)=\\(.*\\)\\'" arg)
            (setq name (match-string 1 arg) value (match-string 2 arg))
          (setq name (substring arg 2))
          (unless args (error "Option --%s expects an argument" name))
          (setq value (pop args)))
        (unless (member name options)
          (error "Unrecognized option: --%s" name))
        (if (equal name "root")
            (let ((cell (assoc name result)))
              (if cell (setcdr cell (append (cdr cell) (list value)))
                (push (list name value) result)))
          (setq result (cons (cons name value)
                             (cl-remove-if (lambda (cell) (equal (car cell) name))
                                           result))))))
    result))

(defun nelisp-native-rooted-prelink-closure--option (parsed name)
  "Return the value of option NAME in PARSED or nil."
  (cdr (assoc name parsed)))

(defun nelisp-native-rooted-prelink-closure-require-options (parsed names)
  "Fail unless every option in NAMES is present in PARSED."
  (dolist (name names)
    (unless (assoc name parsed)
      (error "The following arguments are required: --%s" name))))

(defun nelisp-native-rooted-prelink-closure-main (args)
  "Run the prelink proof for the Python CLI argument list ARGS.
Write the output file and return the digest line Python prints."
  (let* ((parsed (nelisp-native-rooted-prelink-closure-parse-args
                  args '("metadata" "unit-directory" "generated-data" "root"
                         "output" "manifest" "source-root")))
         (get (lambda (name) (nelisp-native-rooted-prelink-closure--option parsed name))))
    (nelisp-native-rooted-prelink-closure-require-options
     parsed '("metadata" "unit-directory" "generated-data" "root" "output"))
    (unless (eq (and (funcall get "manifest") t) (and (funcall get "source-root") t))
      (error "--manifest and --source-root are required together"))
    (let* ((manifest (funcall get "manifest"))
           (digest (when manifest
                     (nelisp-native-rooted-prelink-closure-verify-manifest
                      manifest (funcall get "metadata") (funcall get "unit-directory")
                      (funcall get "generated-data") (funcall get "source-root"))))
           (result (nelisp-native-rooted-prelink-closure-prove
                    (funcall get "metadata") (funcall get "unit-directory")
                    (funcall get "root") (funcall get "generated-data"))))
      (when digest
        (unless (equal digest
                       (nelisp-native-rooted-prelink-closure-verify-manifest
                        manifest (funcall get "metadata") (funcall get "unit-directory")
                        (funcall get "generated-data") (funcall get "source-root")))
          (error "Build provenance changed during prelink proof"))
        (nelisp-native-rooted-prelink-closure-object-set
         result "active_manifest_sha256" digest))
      (nelisp-native-rooted-prelink-closure-write-output
       (funcall get "output") result))))

(provide 'nelisp-native-rooted-prelink-closure)
;;; nelisp-native-rooted-prelink-closure.el ends here
