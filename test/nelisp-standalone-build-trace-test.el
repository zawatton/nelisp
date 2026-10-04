;;; nelisp-standalone-build-trace-test.el --- Reader trace controls -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'json)

(defconst nelisp-build-trace-test--source
  (expand-file-name "../scripts/nelisp-standalone-build.el"
                    (file-name-directory (or load-file-name buffer-file-name))))

(dolist (directory '("../lisp" "../src" "../scripts"))
  (add-to-list 'load-path (expand-file-name directory
                                           (file-name-directory nelisp-build-trace-test--source))))
(require 'nelisp-aot-compiler)
(require 'nelisp-static-linker)
(require 'nelisp-standalone-arena-rewrite)

;; Load only these orchestration definitions: no compiler or native build.
(with-temp-buffer
  (insert-file-contents nelisp-build-trace-test--source)
  (dolist (name '(nelisp-standalone--build-trace-path
                  nelisp-standalone--build-trace-unit
                  nelisp-standalone--build-trace-hits
                  nelisp-standalone--build-trace-misses
                  nelisp-standalone--build-trace-event
                  nelisp-standalone--build-trace-phase
                  nelisp-standalone--toolchain-digest
                  nelisp-standalone--unit-cache-key
                  nelisp-standalone--cached-unit
                  nelisp-standalone--core-bytecode-build-cache
                  nelisp-standalone--core-bytecode-src
                  nelisp-standalone--target-arch
                  nelisp-standalone--target-abi
                  nelisp-standalone--target-os
                  nelisp-standalone--target-object-name
                  nelisp-standalone--rebase-arena-source
                  nelisp-standalone--compile-to-unit
                  nelisp-standalone--copy-lit-u64-defuns
                  nelisp-standalone-build-reader))
    (goto-char (point-min))
    (while (re-search-forward
           (format "^(\\(?:defun\\|defmacro\\|defvar\\) %s\\_>"
                   (regexp-quote (symbol-name name))) nil t)
      (goto-char (match-beginning 0))
      (eval (read (current-buffer)) t))))

(defvar nelisp-standalone--recompiled nil)
(defvar nelisp-standalone--this-file)
(defvar nelisp-standalone--repo-root)

(ert-deftest nelisp-build-trace-linux-copy-is-constant-size ()
  (let ((nelisp-standalone--target 'linux-x86_64))
    (let ((small (nelisp-standalone--copy-lit-u64-defuns 'copy "abcdefgh"))
          (large (nelisp-standalone--copy-lit-u64-defuns 'copy (make-string 4096 ?a))))
      (should (= (length small) 2))
      (should (= (length large) 2))
      (should (eq (caar large) 'data-blob)))))

(ert-deftest nelisp-build-trace-unit-materializes-qualified-data ()
  (let* ((nelisp-standalone--target 'linux-x86_64)
         (unit (nelisp-standalone--compile-to-unit
                "copy.o" '(seq (data-blob sample "abcdefgh" rodata)
                               (defun copy (dst off) (data-addr sample)))))
         (symbols (plist-get unit :symbols))
         (blob (cl-find-if (lambda (symbol) (eq (plist-get symbol :type) 'object)) symbols))
         (reloc (car (plist-get unit :relocs))))
    (should (equal (cdr (assq 'rodata (plist-get unit :sections))) "abcdefgh"))
    (should (eq (plist-get blob :bind) 'global))
    (should-not (equal (plist-get blob :name) "sample"))
    (should (equal (plist-get reloc :symbol) (plist-get blob :name)))
    (should (= (plist-get reloc :addend) 0))))

(ert-deftest nelisp-build-trace-no-data-unit-preserves-original-fields ()
  ;; These complete-unit hashes were measured against the original standalone
  ;; function, including text bytes, exported symbols and external call relocs.
  (let ((nelisp-standalone--target 'linux-x86_64)
        (process-environment (copy-sequence process-environment)))
    (setenv "NELISP_TCO" nil)
    (dolist (fixture
             '(((seq (defun fixture (n) (+ n 1)))
                "7bde3afa5d0d7902fa6f0ef231835f85951c9637f805274b24a8fa89dd31d282")
               ((seq (defun fixture (n) (extern-call external_callback n)))
                "014173b2c76ff5240a89f8f4c1be3d5eeeaef4c93dea8d785126d363c94e684e")))
      (should (equal (cadr fixture)
                     (secure-hash 'sha256
                                  (prin1-to-string
                                   (nelisp-standalone--compile-to-unit
                                    "fixture.o" (car fixture)))))))))

(defun nelisp-build-trace-test--copy-eval (form environment definitions blobs)
  "Interpret the bounded copy DSL while real vector accesses enforce bounds."
  (cl-labels ((run (value) (nelisp-build-trace-test--copy-eval value environment definitions blobs)))
    (cond
     ((numberp form) form)
     ((symbolp form) (gethash form environment))
     (t
      (pcase (car form)
        ('seq (let (result) (dolist (part (cdr form)) (setq result (run part))) result))
        ('let (let ((inner (copy-hash-table environment)))
                (dolist (binding (nth 1 form))
                  (puthash (car binding) (run (cadr binding)) inner))
                (nelisp-build-trace-test--copy-eval (nth 2 form) inner definitions blobs)))
        ('setq (puthash (nth 1 form) (run (nth 2 form)) environment))
        ('while (while (not (= 0 (run (nth 1 form)))) (run (nth 2 form))) 0)
        ('+ (apply #'+ (mapcar #'run (cdr form))))
        ('< (if (< (run (nth 1 form)) (run (nth 2 form))) 1 0))
        ('data-addr (gethash (nth 1 form) blobs))
        ((or 'ptr-read-u8 'ptr-read-u64)
         (let ((source (run (nth 1 form))) (offset (run (nth 2 form))) (word 0))
           (dotimes (index (if (eq (car form) 'ptr-read-u64) 8 1))
             (setq word (logior word (ash (aref source (+ offset index)) (* index 8)))))
           word))
        ((or 'ptr-write-u8 'ptr-write-u64)
         (let ((destination (run (nth 1 form))) (offset (run (nth 2 form)))
               (word (run (nth 3 form))))
           (dotimes (index (if (eq (car form) 'ptr-write-u64) 8 1))
             (aset destination (+ offset index) (logand (ash word (- (* index 8))) 255)))
           word))
        (_ (let* ((definition (gethash (car form) definitions))
                  (inner (make-hash-table)))
             (cl-mapc (lambda (parameter value) (puthash parameter (run value) inner))
                      (nth 2 definition) (cdr form))
             (nelisp-build-trace-test--copy-eval (nth 3 definition) inner definitions blobs))))))))

(ert-deftest nelisp-build-trace-copy-bounds-and-target-fallback ()
  (dolist (text (append (mapcar (lambda (n) (make-string n ?a)) '(0 1 7 8 9 2057))
                       (list "a\0b" "日本語🙂")))
    (dolist (target '(linux-x86_64 windows-x86_64 macos-aarch64))
      (let* ((nelisp-standalone--target target)
             (forms (nelisp-standalone--copy-lit-u64-defuns 'copy text))
             (bytes (encode-coding-string text 'utf-8 t))
             (destination (make-vector (+ 7 (length bytes) 9) 165))
             (definitions (make-hash-table)) (blobs (make-hash-table))
             (environment (make-hash-table)))
        (dolist (form forms)
          (if (eq (car form) 'data-blob)
              (puthash (nth 1 form) (vconcat (nth 2 form)) blobs)
            (puthash (nth 1 form) form definitions)))
        (puthash 'destination destination environment)
        (should (= (+ 7 (length bytes))
                   (nelisp-build-trace-test--copy-eval '(copy destination 7)
                                                      environment definitions blobs)))
        (should (equal (substring destination 7 (+ 7 (length bytes))) (vconcat bytes)))
        (should (equal (substring destination 0 7) (make-vector 7 165)))
        (should (equal (substring destination (+ 7 (length bytes))) (make-vector 9 165)))
        (unless (eq target 'linux-x86_64)
          (should-not (cl-find 'data-blob forms :key #'car)))))))

(ert-deftest nelisp-build-trace-data-reloc-and-validation-controls ()
  (let* ((nelisp-standalone--target 'linux-x86_64)
         (source '(seq (data-blob sample "abc" rodata)
                       (defun copy (dst off) (data-addr sample))))
         (unit (nelisp-standalone--compile-to-unit "one.o" source))
         (other (nelisp-standalone--compile-to-unit "two.o" source))
         (reloc (car (plist-get unit :relocs)))
         (other-reloc (car (plist-get other :relocs)))
         (symbols (nelisp-link-symtab-make))
         (vector (vconcat (cdr (assq 'text (plist-get unit :sections)))))
         (offset (plist-get reloc :offset)) (base 4096) (address 8192))
    (should-not (equal (plist-get reloc :symbol) (plist-get other-reloc :symbol)))
    (nelisp-link-symtab-add symbols (nelisp-link-symbol (plist-get reloc :symbol) address))
    (nelisp-link-apply-reloc-inplace vector reloc symbols base)
    (cl-labels ((destination (bytes)
                  (let ((value 0))
                    (dotimes (index 4) (setq value (logior value (ash (aref bytes (+ offset index)) (* index 8)))))
                    (+ base offset 4 value))))
      (should (= address (destination vector)))
      (let ((broken (copy-sequence reloc)) (bytes (copy-sequence vector)))
        (setf (plist-get broken :addend) -4)
        (nelisp-link-apply-reloc-inplace bytes broken symbols base)
        (should-not (= address (destination bytes))))
      (let ((broken (copy-sequence reloc)))
        (setf (plist-get broken :symbol) (plist-get other-reloc :symbol))
        (should-error (nelisp-link-apply-reloc-inplace vector broken symbols base)
                      :type 'nelisp-link--unresolved-symbol)))
    (dolist (bad '((seq (data-blob x "a" rodata) (data-blob x "b" rodata))
                  (seq (data-blob x "a" data))
                  (seq (data-blob x "a" rodata ((0 missing 0))))))
      (should-error (nelisp-standalone--compile-to-unit "bad.o" bad)
                    :type 'nelisp-aot-compiler-error))))

(ert-deftest nelisp-build-trace-core-generation-context ()
  (let* ((directory (make-temp-file "nelisp-core-context-" t))
         (nelisp-standalone--repo-root directory)
         (nelisp-standalone--core-bytecode-build-cache (make-hash-table :test #'equal))
         (calls 0) (reports 0) (fail nil) (mutable-cell (list 0))
         (original-require (symbol-function 'require)))
    (unwind-protect
        (progn
          (dolist (path '("lisp/nelisp-native-load.el" "packages/nl-ffi/src/nl-ffi.el"
                          "vendor/emacs-lisp/emacs-lisp/cl-seq.el"))
            (let ((file (expand-file-name path directory)))
              (make-directory (file-name-directory file) t)
              (with-temp-file file (insert "(defun sample () 1)\n"))))
          (cl-letf (((symbol-function 'require)
                     (lambda (feature &rest args)
                       (if (eq feature 'nelisp-prelude-bytecode) t
                         (apply original-require feature args))))
                    ((symbol-function 'nelisp-prelude-bytecode-transform)
                     (lambda (source &rest _) (cl-incf calls)
                       (cl-incf (car mutable-cell))
                       (when fail (error "compiler failure"))
                       (list source nil '(report))))
                    ((symbol-function 'nelisp-prelude-bytecode--skip-trivia)
                     (lambda (text position)
                       (while (and (< position (length text))
                                   (memq (aref text position) '(32 10)))
                         (cl-incf position)) position))
                    ((symbol-function 'nelisp-prelude-bytecode-write-report)
                     (lambda (&rest _) (cl-incf reports))))
            (let ((first (nelisp-standalone--core-bytecode-src)))
              ;; Mutating a captured compiler cell must not corrupt hash keys.
              (setcar mutable-cell 100)
              (should (equal first (nelisp-standalone--core-bytecode-src)))
              (should (= calls 3))
              (should (= reports 2))
              ;; The uncached original path independently yields identical bytes.
              (let ((nelisp-standalone--core-bytecode-build-cache nil))
                (should (equal first (nelisp-standalone--core-bytecode-src))))
              (should (= calls 6))
              (with-temp-file (expand-file-name "lisp/nelisp-native-load.el" directory)
                (insert "(defun sample () 2)\n"))
              (should-not (equal first (nelisp-standalone--core-bytecode-src)))
              (should (= calls 9))
              ;; A new compilation context never inherits earlier generations.
              (let ((nelisp-standalone--core-bytecode-build-cache (make-hash-table :test #'equal)))
                (nelisp-standalone--core-bytecode-src))
              (should (= calls 12))
              (cl-letf (((symbol-function 'nelisp-prelude-bytecode-transform)
                         (lambda (source &rest _) (cl-incf calls)
                           (list source nil '(report)))))
                (nelisp-standalone--core-bytecode-src))
              (should (= calls 15))
              (setq fail t)
              (with-temp-file (expand-file-name "lisp/nelisp-native-load.el" directory)
                (insert "(defun sample () 3)\n"))
              (should-error (nelisp-standalone--core-bytecode-src))
              (should-error (nelisp-standalone--core-bytecode-src))
              (should (= calls 17)))))
      (delete-directory directory t))))

(defun nelisp-build-trace-test--events (path)
  (when (file-exists-p path)
    (with-temp-buffer
      (insert-file-contents path)
      (let ((json-object-type 'alist) (json-array-type 'list) events)
        (goto-char (point-min))
        (while (< (point) (point-max))
          (push (json-read) events)
          (forward-line 1))
        (nreverse events)))))

(defmacro nelisp-build-trace-test--fixture (&rest body)
  (declare (indent 0))
  `(let* ((directory (make-temp-file "nelisp-build-trace-" t))
          (trace (expand-file-name "trace.jsonl" directory))
          (dependency (expand-file-name "compiler.el" directory))
          (output (expand-file-name "reader" directory))
          (process-environment (copy-sequence process-environment))
          (nelisp-standalone--toolchain-digest nil)
          (nelisp-standalone--recompiled nil)
          (compiled 0) (cold 0) (artifact 0))
     (unwind-protect
         (progn
           (with-temp-file dependency (insert "original compiler"))
           (setenv "NELISP_READER_DYNAMIC" nil)
           (cl-letf (((symbol-function 'nelisp-standalone--validate-reader-registrations) #'ignore)
                     ((symbol-function 'nelisp-standalone--output-path) (lambda (&rest _) output))
                     ((symbol-function 'nelisp-standalone-arena-rewrite-target) (lambda () 'linux-x86_64))
                     ((symbol-function 'nelisp-standalone--target-object-name) #'identity)
                     ((symbol-function 'nelisp-standalone--target-cache-dir) (lambda () directory))
                     ((symbol-function 'nelisp-standalone--dep-files) (lambda () (list dependency)))
                     ((symbol-function 'nelisp-standalone--compile-to-unit)
                      (lambda (name source) (cl-incf compiled) (list name source)))
                     ((symbol-function 'nelisp-standalone--unit-cache-encode) #'identity)
                     ((symbol-function 'nelisp-standalone--unit-cache-decode) #'identity)
                     ((symbol-function 'nelisp-standalone--reader-units)
                      (lambda () (list (nelisp-standalone--cached-unit "test.o" '(original))
                                       (nelisp-standalone--cached-unit "test.o" '(original)))))
                     ((symbol-function 'nelisp-link-units) (lambda (&rest _) (with-temp-file output)))
                     ((symbol-function 'nelisp-standalone--reader-src) (lambda () "test"))
                     ((symbol-function 'nelisp-standalone--stamp-build-digest) (lambda (_) "digest"))
                     ((symbol-function 'nelisp-standalone--build-cold-image) (lambda (_) (cl-incf cold)))
                     ((symbol-function 'nelisp-standalone-build-artifact-runtime-cache) (lambda () (cl-incf artifact))))
             ,@body))
       (delete-directory directory t))))

(ert-deftest nelisp-build-trace-events-and-content-misses ()
  (nelisp-build-trace-test--fixture
    (setenv "NELISP_STANDALONE_BUILD_TRACE" trace)
    (should (equal output (nelisp-standalone-build-reader)))
    (should (= compiled 1))
    (should (= cold 1))
    (should (= artifact 1))
    (let* ((events (nelisp-build-trace-test--events trace))
           (last (car (last events))))
      (dolist (phase '("overall" "prepare" "link-stamp" "cold-image" "artifact-runtime" "cache-key" "cache-decode" "cache-compile"))
        (should (cl-find-if (lambda (event)
                             (and (equal (alist-get 'phase event) phase)
                                  (equal (alist-get 'event event) "end"))) events)))
      (should (equal (alist-get 'phase last) "overall"))
      (should (= (alist-get 'hits last) 1))
      (should (= (alist-get 'misses last) 1)))
    ;; Actual source content and actual toolchain file content remain key inputs.
    (nelisp-standalone--cached-unit "test.o" '(changed-source))
    (should (= compiled 2))
    (with-temp-file dependency (insert "changed compiler"))
    (setq nelisp-standalone--toolchain-digest nil)
    (nelisp-standalone--cached-unit "test.o" '(changed-source))
    (should (= compiled 3))
    (should (= 1 (length (directory-files directory nil "\\.unit\\'"))))))

(ert-deftest nelisp-build-trace-errors-propagate ()
  (nelisp-build-trace-test--fixture
    (setenv "NELISP_STANDALONE_BUILD_TRACE" trace)
    (cl-letf (((symbol-function 'nelisp-standalone--reader-units)
               (lambda () (error "prepare failed"))))
      (should-error (nelisp-standalone-build-reader) :type 'error))
    (let ((events (nelisp-build-trace-test--events trace)))
      (dolist (phase '("prepare" "overall"))
        (should (cl-find-if (lambda (event)
                             (and (equal (alist-get 'phase event) phase)
                                  (equal (alist-get 'event event) "error"))) events))))
    (should (= cold 0))
    (should (= artifact 0))))

(ert-deftest nelisp-build-trace-disabled-no-clock-or-file ()
  (nelisp-build-trace-test--fixture
    (setenv "NELISP_STANDALONE_BUILD_TRACE" nil)
    (cl-letf (((symbol-function 'float-time) (lambda (&rest _) (error "unexpected clock"))))
      (should (equal output (nelisp-standalone-build-reader))))
    (should-not (file-exists-p trace))
    (should (= compiled 1))
    (should (= cold 1))
    (should (= artifact 1))))

(ert-deftest nelisp-build-trace-reader-owns-fresh-core-context ()
  (nelisp-build-trace-test--fixture
    (setenv "NELISP_STANDALONE_BUILD_TRACE" nil)
    (let (contexts)
      (cl-letf (((symbol-function 'nelisp-standalone--reader-units)
                 (lambda ()
                   (should (hash-table-p nelisp-standalone--core-bytecode-build-cache))
                   (should-not (gethash 'prior nelisp-standalone--core-bytecode-build-cache))
                   (puthash 'prior t nelisp-standalone--core-bytecode-build-cache)
                   (push nelisp-standalone--core-bytecode-build-cache contexts)
                   nil)))
        (nelisp-standalone-build-reader)
        (nelisp-standalone-build-reader))
      (should (= (length contexts) 2))
      (should-not (eq (car contexts) (cadr contexts))))))

;;; nelisp-standalone-build-trace-test.el ends here
