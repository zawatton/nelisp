;;; nelisp-bytecode-corpus-parity.el --- Standalone byte-code corpus parity -*- lexical-binding: t; -*-

;; Run through nelisp-bytecode-corpus-parity.sh.  Every selected definition is
;; evaluated in a fresh host Emacs and a fresh standalone process.  Values,
;; stderr, and exit status are persisted under target/ before comparison.

(require 'cl-lib)
(require 'subr-x)
(require 'nelisp-bytecode-histogram)

(defconst nelisp-bytecode-corpus-supported-opcodes
  '(byte-stack-ref byte-constant byte-dup byte-goto-if-nil byte-return
    byte-discard byte-goto byte-add1 byte-lss byte-plus byte-stack-set
    byte-call byte-car byte-cdr byte-varref byte-goto-if-not-nil
    byte-goto-if-nil-else-pop byte-cons byte-memq byte-eq
    byte-goto-if-not-nil-else-pop byte-car-safe byte-not byte-discardN
    byte-list1 byte-length byte-consp byte-unbind byte-eqlsign byte-varbind
    byte-sub1 byte-gtr byte-nth byte-setcar byte-diff byte-varset)
  "Opcodes accepted by the standalone byte-code implementation.")

(defconst nelisp-bytecode-corpus--file
  (or load-file-name buffer-file-name)
  "File from which the harness was loaded.")

(defun nelisp-bytecode-corpus--root ()
  (file-name-directory
   (directory-file-name
    (file-name-directory
     (file-name-directory nelisp-bytecode-corpus--file)))))

(defun nelisp-bytecode-corpus--object-opcodes (object)
  (let (result)
    (dolist (nested (nelisp-bytecode-objects object))
      (dolist (instruction (nelisp-bytecode-decode (aref nested 1)))
        (cl-pushnew (nth 1 instruction) result)))
    result))

(defun nelisp-bytecode-corpus--supported-p (object)
  (cl-every (lambda (opcode)
              (memq opcode nelisp-bytecode-corpus-supported-opcodes))
            (nelisp-bytecode-corpus--object-opcodes object)))

(defun nelisp-bytecode-corpus--stack-argument-count (object)
  (let ((descriptor (aref object 0)))
    (+ (ash descriptor -8) (if (zerop (logand descriptor #x80)) 0 1))))

(defun nelisp-bytecode-corpus--patch-jumps (code delta)
  "Return CODE with absolute branch targets advanced by DELTA bytes."
  (let ((patched (copy-sequence code)))
    (dolist (instruction (nelisp-bytecode-decode code))
      (when (memq (nth 1 instruction)
                  '(byte-goto byte-goto-if-nil byte-goto-if-not-nil
                    byte-goto-if-nil-else-pop
                    byte-goto-if-not-nil-else-pop))
        (let* ((offset (nth 0 instruction))
               (target (+ (nth 2 instruction) delta)))
          (aset patched (1+ offset) (logand target #xff))
          (aset patched (+ offset 2) (logand (ash target -8) #xff)))))
    patched))

(defun nelisp-bytecode-corpus--call-form (object)
  ;; `byte-code' starts with an empty value stack.  A byte-code function starts
  ;; with its required/optional arguments (and one assembled &rest list) on
  ;; that stack.  Prefix the raw program with NIL constants to reproduce that
  ;; entry state.  Absolute branch destinations are byte offsets, hence the
  ;; matching DELTA adjustment.
  (let* ((count (nelisp-bytecode-corpus--stack-argument-count object))
         (constants (aref object 2))
         (nil-index (length constants)))
    (if (>= nil-index 64)
        ;; Such a function necessarily needs byte-constant2 for the appended
        ;; fixture and is outside this phase's implemented opcode surface.
        (list 'byte-code (aref object 1) constants (aref object 3))
      (let ((prefix (string-make-unibyte
                     (make-string count (+ 192 nil-index)))))
        (list 'byte-code
              (concat prefix
                      (nelisp-bytecode-corpus--patch-jumps
                       (aref object 1) count))
              (vconcat constants [nil])
              (aref object 3))))))

(defun nelisp-bytecode-corpus--write (file value)
  (with-temp-file file (insert value)))

(defun nelisp-bytecode-corpus--run (program args stdout-file stderr-file)
  ;; Direct-to-file output avoids coding conversion of arbitrary byte strings
  ;; returned by corpus functions.  A per-case timeout prevents one library
  ;; function called with synthetic nil arguments from hanging the whole run.
  (apply #'process-file "timeout" nil
         (list (list :file stdout-file) stderr-file) nil
         "10" program args))

(defun nelisp-bytecode-corpus--result-text (file)
  (with-temp-buffer
    (insert-file-contents file)
    (string-trim-right (buffer-string))))

(defun nelisp-bytecode-corpus-run ()
  "Compile the census corpus and compare every definition.
Non-nullary definitions receive nil in every argument stack slot.  This is a
deterministic coverage fixture, not a claim that nil satisfies each API."
  (let* ((root (nelisp-bytecode-corpus--root))
         (default-directory root)
         (output (expand-file-name "target/bytecode-corpus-parity/" root))
         (standalone (expand-file-name "target/nelisp" root))
         (host (expand-file-name invocation-name invocation-directory))
         (entries (nelisp-bytecode-compile-sources
                   (nelisp-bytecode-source-paths)))
         (supported 0) (selected 0) (completed 0) (both-values 0)
         (agreed 0) disagreements)
    (make-directory output t)
    (cl-loop
     for entry in entries
     for index from 1
     for name = (nth 1 entry)
     for object = (nth 2 entry)
     for supported-p = (nelisp-bytecode-corpus--supported-p object)
     do
     (when supported-p (cl-incf supported))
     (cl-incf selected)
     (let* ((stem (expand-file-name (format "%03d-%s" index name) output))
              (form (nelisp-bytecode-corpus--call-form object))
              (printed (let ((print-escape-newlines t)
                             (print-escape-control-characters t)
                             (print-escape-nonascii t)
                             (print-quoted t)
                             (print-circle t))
                         (prin1-to-string form)))
              (expression-file (concat stem ".expr"))
              (host-expression
               (format (concat "(progn (require 'cl-lib) (require 'rx) "
                               "(require 'subr-x) (require 'time-date) "
                               "(require 'json) (prin1 %s))")
                       printed))
              (host-out (concat stem ".host.out"))
              (host-err (concat stem ".host.err"))
              (standalone-out (concat stem ".standalone.out"))
              (standalone-err (concat stem ".standalone.err"))
              (status-file (concat stem ".status"))
              (host-status (nelisp-bytecode-corpus--run
                            host (list "--batch" "-Q" "--eval" host-expression)
                            host-out host-err))
              (standalone-status (nelisp-bytecode-corpus--run
                                  standalone (list "--eval" printed)
                                  standalone-out standalone-err)))
         (nelisp-bytecode-corpus--write expression-file printed)
         (nelisp-bytecode-corpus--write
          status-file (format "host=%s\nstandalone=%s\n"
                              host-status standalone-status))
         (when (equal standalone-status 0) (cl-incf completed))
         (when (and (equal host-status 0) (equal standalone-status 0))
           (cl-incf both-values)
           (let ((host-value (nelisp-bytecode-corpus--result-text host-out))
                 (standalone-value
                  (nelisp-bytecode-corpus--result-text standalone-out)))
             (if (equal host-value standalone-value)
                 (cl-incf agreed)
               (push (list name host-value standalone-value) disagreements))))))
    (setq disagreements (nreverse disagreements))
    (let ((report (expand-file-name "report.txt" output)))
      (with-temp-file report
        (insert (format "definitions=%d\n" (length entries)))
        (insert (format "opcode-supported=%d\n" supported))
        (insert (format "selection=all-with-nil-argument-stack selected=%d\n"
                        selected))
        (insert (format "standalone-completed=%d\n" completed))
        (insert (format "both-produced-value=%d\n" both-values))
        (insert (format "agreed=%d\n" agreed))
        (insert (format "disagreements=%d\n" (length disagreements)))
        (dolist (row disagreements)
          (insert (format "%S\thost=%S\tstandalone=%S\n"
                          (nth 0 row) (nth 1 row) (nth 2 row)))))
      (princ (with-temp-buffer
               (insert-file-contents report)
               (buffer-string))))))

(provide 'nelisp-bytecode-corpus-parity)
;;; nelisp-bytecode-corpus-parity.el ends here
