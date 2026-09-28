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
    byte-stringp byte-integerp byte-listp byte-symbolp
    byte-call byte-car byte-cdr byte-varref byte-goto-if-not-nil
    byte-goto-if-nil-else-pop byte-cons byte-memq byte-eq
    byte-goto-if-not-nil-else-pop byte-car-safe byte-not byte-discardN
    ;; VM handles this disassembler alias through byte-discardN's operand flag.
    byte-discardN-preserve-tos
    byte-list1 byte-length byte-consp byte-unbind byte-eqlsign byte-varbind
    byte-sub1 byte-gtr byte-nth byte-setcar byte-diff byte-varset
    byte-list2 byte-leq byte-geq byte-equal byte-nthcdr byte-member
    byte-aref byte-substring byte-assq byte-nreverse byte-setcdr byte-rem
    byte-list3 byte-list4 byte-string= byte-string< byte-nconc
    byte-constant2 byte-numberp byte-concat2 byte-concat3 byte-max byte-min)
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
    ;; Emacs 31 stores an argument list in byte-code function slot 0; older
    ;; hosts store the packed arity descriptor.  NIL is a nullary function.
    (if (null descriptor)
        0
      (+ (ash descriptor -8)
         (if (zerop (logand descriptor #x80)) 0 1)))))

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

(defconst nelisp-bytecode-corpus--profiles
  '(nil t 0 1 -1 "" "a" x identity null consp car (1 2 3) (a b)
    ("a" "b") (0 0 0 1 1 2024 0 -1 nil))
  "Deterministic scalar and sequence values used to populate argument slots.")

(defun nelisp-bytecode-corpus--profiles-for-count (count)
  "Return deterministic argument profiles for COUNT stack slots."
  (let ((nil-args (make-list count nil)) profiles)
    (dolist (value nelisp-bytecode-corpus--profiles)
      (let ((args (make-list count value)))
        (unless (equal args nil-args) (push args profiles)))
      (cl-loop for slot below (min count 4)
               for args = (copy-sequence nil-args)
               do (setf (nth slot args) value)
               (unless (equal args nil-args) (push args profiles))))
    ;; Mixed profiles exercise common higher-order, sequence, string, and
    ;; numeric operations.  The final slot for &rest is the argument list.
    (dolist (args '((identity (1 2 3) nil) (null (1 2 3) nil)
                    (consp (1 2 3) nil) (identity (a b) nil)
                    (identity ("a" "b") nil) ("a" "b" nil)
                    ("a" 0 nil) (0 "a" nil) (2 1 nil)
                    (nil (1 2 3) nil) ("a" nil nil) (1 2 nil)
                    (1 (2 3) nil) (0 1) (1 2) ("a" "b") ((1 2) nil)
                    (nil (1 2)) (x y)))
      (when (<= (length args) count)
        (let ((full (append args (make-list (- count (length args)) nil))))
    (unless (or (equal full nil-args) (member full profiles))
            (push full profiles)))))
    (let ((seen (make-hash-table :test #'equal)))
      (nreverse (cl-remove-if
                 (lambda (profile)
                   (if (gethash profile seen) t
                     (puthash profile t seen) nil))
                 profiles)))))

(defun nelisp-bytecode-corpus--profile-encodable-p (object profile)
  (let* ((count (nelisp-bytecode-corpus--stack-argument-count object))
         (constants (aref object 2))
         (used (seq-take profile (min (length profile) count)))
         (needs (+ (length constants) (length used)
                   (if (< (length used) count) 1 0))))
    (< needs 64)))

(defun nelisp-bytecode-corpus--call-form (object &optional profile)
  ;; `byte-code' starts with an empty value stack.  A byte-code function starts
  ;; with its required/optional arguments (and one assembled &rest list) on
  ;; that stack.  Prefix the raw program with constants to reproduce that
  ;; entry state.  When PROFILE is given, append each profile value as a
  ;; byte-constant; adjust branch targets by the prefix length (count).
  (let* ((count (nelisp-bytecode-corpus--stack-argument-count object))
         (constants (aref object 2))
         (profile (or profile (make-list count nil)))
         (used (seq-take profile (min (length profile) count)))
         (needs (+ (length constants) (length used)
                   (if (< (length used) count) 1 0))))
    (if (>= needs 64)
        ;; Cannot safely encode profile; fall back to all-NIL.
        (let ((nil-index (length constants)))
          (if (>= nil-index 64)
              (list 'byte-code (aref object 1) constants (aref object 3))
            (let ((prefix (string-make-unibyte
                           (make-string count (+ 192 nil-index)))))
              (list 'byte-code
                    (concat prefix
                            (nelisp-bytecode-corpus--patch-jumps
                             (aref object 1) count))
                    (vconcat constants [nil])
                    (aref object 3)))))
      (let* ((new-constants (vconcat constants used
                                     (when (< (length used) count) [nil])))
             (nil-index (+ (length constants) (length used)))
             (prefix-bytes (mapcar
                             (lambda (i)
                               (+ 192 (if (< i (length used))
                                          (+ (length constants) i)
                                        nil-index)))
                             (number-sequence 0 (1- count)))))
        (list 'byte-code
              (concat (string-make-unibyte (apply #'string prefix-bytes))
                      (nelisp-bytecode-corpus--patch-jumps
                       (aref object 1) count))
              new-constants
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
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (let ((text (buffer-string)))
      ;; NeLisp --eval adds one record delimiter after the printed value.
      ;; Remove that one byte only; trimming arbitrary whitespace can hide a
      ;; printer mismatch.
      (if (and (> (length text) 0) (= (aref text (1- (length text))) ?\n))
          (substring text 0 -1)
        text))))

(defun nelisp-bytecode-corpus--print (value)
  (let ((print-escape-newlines t)
        (print-escape-control-characters t)
        (print-escape-nonascii t)
        (print-quoted t)
        (print-circle t))
    (prin1-to-string value)))

(defun nelisp-bytecode-corpus--host-expression (form)
  (concat ";;; -*- lexical-binding: t; -*-\n"
  (format (concat "(progn (require 'cl-lib) (require 'rx) "
                  "(require 'subr-x) (require 'time-date) "
                  "(require 'json) (prin1 (eval '%s)))")
          (nelisp-bytecode-corpus--print form))))

(defun nelisp-bytecode-corpus--screen-expression (forms)
  (format (concat
           "(progn (require 'cl-lib) (require 'rx) (require 'subr-x) "
           "(require 'time-date) (require 'json) "
           "(let ((forms '%s) (index 0)) "
           "(while forms (setq index (1+ index)) "
           "(condition-case nil (progn (eval (read (car forms))) "
           "(princ (format \"%%d\\n\" index))) ((error quit) nil)) "
           "(setq forms (cdr forms)))))")
          (nelisp-bytecode-corpus--print
           (mapcar #'nelisp-bytecode-corpus--print forms))))

(defun nelisp-bytecode-corpus--verify-candidate
    (host standalone stem index profile form)
  "Run FORM in fresh HOST and STANDALONE processes and return both outputs."
  (let* ((candidate-stem (concat stem ".candidate-" (number-to-string index)))
         (printed (nelisp-bytecode-corpus--print form))
         (expression-file (concat candidate-stem ".expr"))
         (host-expression-file (concat candidate-stem ".host.el"))
         (host-out (concat candidate-stem ".host.out"))
         (host-err (concat candidate-stem ".host.err"))
         (standalone-out (concat candidate-stem ".standalone.out"))
         (standalone-err (concat candidate-stem ".standalone.err"))
         (status-file (concat candidate-stem ".status")))
    (nelisp-bytecode-corpus--write expression-file printed)
    (nelisp-bytecode-corpus--write
     host-expression-file (nelisp-bytecode-corpus--host-expression form))
    (let* ((host-status
            (nelisp-bytecode-corpus--run
             host (list "--batch" "-Q" "-l" host-expression-file)
             host-out host-err))
           (standalone-status
            (nelisp-bytecode-corpus--run
             standalone (list "--eval" printed)
             standalone-out standalone-err)))
      (nelisp-bytecode-corpus--write
       status-file
       (format "host=%s\nstandalone=%s\nprofile=%S\n"
               host-status standalone-status profile))
      (list host-status standalone-status
            (nelisp-bytecode-corpus--result-text host-out)
            (nelisp-bytecode-corpus--result-text standalone-out)))))

(defun nelisp-bytecode-corpus-run ()
  "Compare supported vendor byte-code definitions against host Emacs.
Host screens argument profiles in one bounded process per definition.  Each
reported agreement is then rechecked in fresh host and standalone processes."
  (let* ((root (nelisp-bytecode-corpus--root))
         (default-directory root)
         (output (expand-file-name
                  (or (getenv "NELISP_BYTECODE_CORPUS_OUTPUT")
                      "target/bytecode-corpus-parity/") root))
         (standalone (expand-file-name (or (getenv "NELISP_BIN")
                                           "target/nelisp") root))
         (host (or (getenv "NELISP_EMACS")
                   (expand-file-name invocation-name invocation-directory)))
         (entries (nelisp-bytecode-compile-sources
                   (nelisp-bytecode-source-paths)))
         (supported 0) (fixtures-screened 0) (fixtures-verified 0)
         (host-valid 0) (standalone-completed 0) (both-values 0) (agreed 0)
         (host-screen-timeouts 0) (host-screen-errors 0)
         (host-no-valid-profile 0) (host-verification-failures 0)
         (standalone-failures 0) (parity-failures 0)
         (missing-opcode-functions (make-hash-table :test #'eq))
         disagreements)
    (unless (file-executable-p standalone)
      (error "NELISP_BIN is not executable: %s" standalone))
    (unless (file-executable-p host)
      (error "NELISP_EMACS is not executable: %s" host))
    (make-directory output t)
    (cl-loop for entry in entries for index from 1 do
      (let* ((source (nth 0 entry))
             (name (nth 1 entry))
             (object (nth 2 entry)))
        (unless (nelisp-bytecode-corpus--supported-p object)
          (dolist (opcode (nelisp-bytecode-corpus--object-opcodes object))
            (unless (memq opcode nelisp-bytecode-corpus-supported-opcodes)
              (puthash opcode
                       (1+ (gethash opcode missing-opcode-functions 0))
                       missing-opcode-functions))))
        (when (nelisp-bytecode-corpus--supported-p object)
          (cl-incf supported)
          (let* ((stem (expand-file-name
                        (format "%04d-%s" index name) output))
                 (arity (nelisp-bytecode-corpus--stack-argument-count object))
                 (profiles (nelisp-bytecode-corpus--profiles-for-count arity))
                 (candidates
                  (cons (cons 'nil (nelisp-bytecode-corpus--call-form object))
                        (mapcar (lambda (profile)
                                  (cons profile
                                        (nelisp-bytecode-corpus--call-form
                                         object profile)))
                                profiles)))
                 (candidates
                  (cl-remove-if-not
                   (lambda (candidate)
                     (nelisp-bytecode-corpus--profile-encodable-p
                      object (if (eq (car candidate) 'nil)
                                 (make-list arity nil)
                               (car candidate))))
                   candidates))
                 (forms (mapcar #'cdr candidates))
                 (screen-file (concat stem ".screen.el"))
                 (screen-out (concat stem ".screen.out"))
                 (screen-err (concat stem ".screen.err"))
                 (screen-status-file (concat stem ".screen.status")))
            (nelisp-bytecode-corpus--write
             screen-file (nelisp-bytecode-corpus--screen-expression forms))
            (let* ((screen-status
                    (nelisp-bytecode-corpus--run
                     host (list "--batch" "-Q" "-l" screen-file)
                     screen-out screen-err))
                   (indices
                    (when (and (numberp screen-status)
                               (zerop screen-status))
                      (cl-remove-if-not
                       (lambda (candidate-index)
                         (and (> candidate-index 0)
                              (<= candidate-index (length candidates))))
                       (mapcar #'string-to-number
                               (split-string
                                (nelisp-bytecode-corpus--result-text screen-out)
                                "\n" t)))))
                   (matched-p nil)
                   (standalone-ok-p nil)
                   (both-ok-p nil)
                   (host-verification-failed-p nil)
                   (parity-mismatch-p nil)
                   (attempted-p nil))
              (cl-incf fixtures-screened)
              (nelisp-bytecode-corpus--write
               screen-status-file
               (format "status=%s\nsource=%s\n" screen-status source))
              (cond
               ((and (numberp screen-status) (= screen-status 124))
                (cl-incf host-screen-timeouts))
               ((not (and (numberp screen-status) (zerop screen-status)))
                (cl-incf host-screen-errors))
               ((null indices)
                (cl-incf host-no-valid-profile))
               (t
                (cl-incf host-valid)
                (dolist (candidate-index indices)
                  (unless matched-p
                    (let* ((candidate (nth (1- candidate-index) candidates))
                           (profile (car candidate))
                           (verification
                            (nelisp-bytecode-corpus--verify-candidate
                             host standalone stem candidate-index profile
                             (cdr candidate)))
                           (host-status (nth 0 verification))
                           (standalone-status (nth 1 verification))
                           (host-value (nth 2 verification))
                           (standalone-value (nth 3 verification))
                           (host-ok (and (numberp host-status)
                                         (zerop host-status)))
                           (standalone-ok
                            (and (numberp standalone-status)
                                 (zerop standalone-status))))
                      (setq attempted-p t)
                      (cl-incf fixtures-verified)
                      (when standalone-ok (setq standalone-ok-p t))
                      (when (and (not host-ok) standalone-ok)
                        (setq host-verification-failed-p t))
                      (when (and host-ok standalone-ok)
                        (setq both-ok-p t)
                        (if (equal host-value standalone-value)
                            (progn
                              (setq matched-p t)
                              (cl-incf agreed)
                              (nelisp-bytecode-corpus--write
                               (concat stem ".accepted-profile")
                               (format "%S\n" profile)))
                          (setq parity-mismatch-p t)
                          (push (list source name profile host-value
                                      standalone-value)
                                disagreements))))))
                (when standalone-ok-p (cl-incf standalone-completed))
                (when both-ok-p (cl-incf both-values))
                (unless matched-p
                  (cond
                   (parity-mismatch-p (cl-incf parity-failures))
                   (host-verification-failed-p
                    (cl-incf host-verification-failures))
                   (attempted-p (cl-incf standalone-failures)))))))))))
    ;; Emit exactly one aggregate report, after every definition was screened.
    (setq disagreements (nreverse disagreements))
    (let ((report (expand-file-name "report.txt" output)))
      (with-temp-file report
        (insert (format "definitions=%d\n" (length entries)))
        (insert (format "opcode-supported=%d\n" supported))
        (insert (format "opcode-unsupported=%d\n"
                        (- (length entries) supported)))
        (insert "selection=all-opcode-supported-definitions\n")
        (insert (format "host-screened-definitions=%d\n" fixtures-screened))
        (insert (format "host-valid-definitions=%d\n" host-valid))
        (insert (format "fresh-fixtures-verified=%d\n" fixtures-verified))
        (insert (format "standalone-completed=%d\n" standalone-completed))
        (insert (format "both-produced-value=%d\n" both-values))
        (insert (format "agreed=%d\n" agreed))
        (insert (format "host-screen-timeouts=%d\n" host-screen-timeouts))
        (insert (format "host-screen-errors=%d\n" host-screen-errors))
        (insert (format "host-no-valid-profile=%d\n"
                        host-no-valid-profile))
        (insert (format "host-verification-failures=%d\n"
                        host-verification-failures))
        (insert (format "standalone-failures=%d\n" standalone-failures))
        (insert (format "parity-failures=%d\n" parity-failures))
        (insert "top-unsupported-opcodes=\n")
        (let (rows)
          (maphash (lambda (opcode count)
                     (push (cons opcode count) rows))
                   missing-opcode-functions)
          (dolist (row (seq-take
                        (sort rows (lambda (a b) (> (cdr a) (cdr b)))) 15))
            (insert (format "%s\t%d\n" (car row) (cdr row)))))
        (insert (format "disagreements=%d\n" (length disagreements)))
        (dolist (row disagreements)
          (insert (format "%S\n" row))))
      (princ (with-temp-buffer
               (insert-file-contents report)
               (buffer-string))))))

(provide 'nelisp-bytecode-corpus-parity)
;;; nelisp-bytecode-corpus-parity.el ends here
