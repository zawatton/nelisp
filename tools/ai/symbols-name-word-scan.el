;;; symbols-name-word-scan.el --- the pinned enumerations of the name-cell writers and readers -*- lexical-binding: t; -*-

;; emacs -Q --batch -l tools/ai/symbols-name-word-scan.el -- writers|readers
;;
;; Decision item A (tools/ai/symbols-name-word-progress.org, S1.1 and S2.1).  Reads scripts/nelisp-standalone-build.el
;; and every Elisp file of lisp/ with the Elisp reader and finds, by form shape, the defuns that
;;   writers  allocate a symbol cell (`nl_symbol_alloc') or store into the name word of one (offset 8 of a cell
;;            reached as (- WORD 6) or as the variable bound by `nl_symbol_alloc', or through `nl_val_clone_into'
;;            at (+ b 8)),
;;   readers  load the name word of a symbol object ((heap-read-u64 (- WORD 6) 8), `nl_str_target', the emitters'
;;            redirects named below).
;; The set must equal the one pinned in tools/ai/symbols-name-word-writers.txt or -readers.txt (a defun name and a
;; word on how it handles the name per line).  A new writer or reader fails the check until it is looked at and
;; listed.  This is a tripwire by shape, not a proof: the dynamic census (symbols-name-word-census.el) is the
;; proof that every producer yields a String WORD.
;;
;; The hard invariants of S1.1 are checked here too: the legacy name-box writer and `nl_symname_make' are gone, and
;; `nl_sym_make_uninterned' has no arena-box branch (`nl_gc_in_arena').

(require 'cl-lib)

(defvar snw-scan-files nil)
(defvar snw-scan-defuns (make-hash-table :test 'equal) "name -> list of (FILE . FORM)")

(defun snw-scan-strip (form)
  "FORM with backquote commas removed, so (\\, v) reads as v."
  (cond ((and (consp form) (memq (car form) '(\, \,@)) (consp (cdr form))) (snw-scan-strip (cadr form)))
        ((consp form) (cons (snw-scan-strip (car form)) (snw-scan-strip (cdr form))))
        (t form)))

(defun snw-scan-collect (form file)
  (when (consp form)
    (when (and (eq (car form) 'defun) (consp (cdr form)) (symbolp (cadr form)))
      (push (cons file form) (gethash (symbol-name (cadr form)) snw-scan-defuns)))
    (let ((x form))
      (while (consp x)
        (snw-scan-collect (car x) file)
        (setq x (cdr x))))))

(defun snw-scan-read-file (file)
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8)) (insert-file-contents file))
    (goto-char (point-min))
    (condition-case nil
        (while t (snw-scan-collect (read (current-buffer)) file))
      (end-of-file nil))))

(defun snw-scan-walk (form fn)
  (when (consp form)
    (funcall fn form)
    (let ((x form))
      (while (consp x)
        (snw-scan-walk (car x) fn)
        (setq x (cdr x))))))

(defun snw-scan-boundary-patterns ()
  "The names of tools/ai/symbols-name-word-boundary.txt: the other lane's code, whose shapes are not scanned."
  (with-temp-buffer
    (insert-file-contents "tools/ai/symbols-name-word-boundary.txt")
    (let (out)
      (dolist (line (split-string (buffer-string) "\n" t))
        (let ((l (string-trim (replace-regexp-in-string "#.*" "" line))))
          (unless (string-empty-p l) (push l out))))
      out)))

(defun snw-scan-boundary-p (name patterns)
  (cl-some (lambda (p) (if (string-suffix-p "*" p) (string-prefix-p (substring p 0 -1) name) (string= p name))) patterns))

(defun snw-scan-cell-base-p (x)
  "Whether X names the base of a symbol cell: the cell variables b and c, or (- WORD 6)."
  (or (memq x '(b c))
      (and (consp x) (eq (car x) '-) (consp (cdr x)) (consp (cddr x)) (eql (nth 2 x) 6) (symbolp (cadr x)))))

(defun snw-scan-name-word-p (form)
  "FORM reads or writes the name word: (OP BASE 8 ...) over a cell base."
  (and (consp form) (memq (car form) '(ptr-read-u64 heap-read-u64 ptr-write-u64 heap-write-u64))
       (consp (cdr form)) (consp (cddr form))
       (eql (nth 2 form) 8) (snw-scan-cell-base-p (cadr form))))

(defun snw-scan-wrapper-p (form)
  "Whether the defun FORM contains a defun: a build-tool function that returns DSL source, not a DSL function."
  (let ((found nil))
    (snw-scan-walk (cdddr form) (lambda (f) (when (and (eq (car f) 'defun) (consp (cdr f)) (symbolp (cadr f))) (setq found t))))
    found))

(defun snw-scan-defun-facts (form)
  "(:alloc N :write N :read N :clone N :target N) of one defun FORM."
  (let ((alloc 0) (write 0) (read 0) (clone 0) (target 0))
    (snw-scan-walk
     (snw-scan-strip form)
     (lambda (f)
       (cond ((equal f '(nl_symbol_alloc)) (cl-incf alloc))
             ((and (snw-scan-name-word-p f) (memq (car f) '(ptr-write-u64 heap-write-u64))) (cl-incf write))
             ((and (snw-scan-name-word-p f) (memq (car f) '(ptr-read-u64 heap-read-u64))) (cl-incf read))
             ((and (eq (car f) 'nl_val_clone_into) (consp (cddr f)) (equal (nth 2 f) '(+ b 8))) (cl-incf clone))
             ((eq (car f) 'nl_str_target) (cl-incf target)))))
    (list :alloc alloc :write write :read read :clone clone :target target)))

(defun snw-scan-pinned (file)
  (with-temp-buffer
    (insert-file-contents file)
    (let (names)
      (dolist (line (split-string (buffer-string) "\n" t))
        (unless (string-prefix-p "#" line)
          (let ((parts (split-string line)))
            (when (< (length parts) 2) (error "%s: line without a way: %s" file line))
            (push (car parts) names))))
      (sort names #'string<))))

(let* ((mode (car (last command-line-args-left))))
  (dolist (f (append (list "scripts/nelisp-standalone-build.el")
                     (directory-files "lisp" t "\\`nelisp-.*\\.el\\'")))
    (snw-scan-read-file f))
  (let ((found nil) (hits nil) (skip (snw-scan-boundary-patterns)))
    (maphash
     (lambda (name entries)
       (dolist (e (if (snw-scan-boundary-p name skip) nil (cl-remove-if (lambda (e) (snw-scan-wrapper-p (cdr e))) entries)))
         (let* ((facts (snw-scan-defun-facts (cdr e)))
                (writer (or (> (plist-get facts :alloc) 0) (> (plist-get facts :write) 0) (> (plist-get facts :clone) 0)))
                (reader (or (> (plist-get facts :read) 0) (> (plist-get facts :target) 0))))
           (when (if (equal mode "writers") writer reader)
             (push name found)
             (push (format "%s (%s) %S" name (car e) facts) hits)))))
     snw-scan-defuns)
    (setq found (sort (delete-dups found) #'string<))
    ;; The readers also include the emitters' redirects and the lowering helper, which are not shape-detected.
    (when (equal mode "readers")
      (dolist (n '("nelisp-aot-compiler--emit-symbol-object-redirect" "nelisp-aot-compiler--emit-symbol-object-redirect-arm64"
                   "nelisp-aot-compiler--emit-symbol-name-redirect" "nelisp-aot-compiler--emit-symbol-name-redirect-arm64"
                   "nelisp-heap-accessors--symbol-name-operand"))
        (unless (gethash n snw-scan-defuns) (error "reader %s is gone" n))
        (setq found (sort (delete-dups (cons n found)) #'string<))))
    (let* ((pinned-file (format "tools/ai/symbols-name-word-%s.txt" mode))
           (pinned (snw-scan-pinned pinned-file))
           (extra (cl-set-difference found pinned :test #'string=))
           (missing (cl-set-difference pinned found :test #'string=)))
      (dolist (h (sort hits #'string<)) (princ (concat "found " h "\n")))
      (when extra (princ (format "NOT PINNED in %s: %s\n" pinned-file (mapconcat #'identity (sort extra #'string<) " "))))
      (when missing (princ (format "PINNED but not found: %s\n" (mapconcat #'identity (sort missing #'string<) " "))))
      (when (equal mode "writers")
        (let ((bad nil))
          (dolist (n '("nl_alloc_str_write_legacy" "nl_symname_make"))
            (when (gethash n snw-scan-defuns) (push (format "%s is still defined" n) bad)))
          ;; No file still calls them.
          (dolist (f (append (list "scripts/nelisp-standalone-build.el") (directory-files "lisp" t "\\`nelisp-.*\\.el\\'")))
            (with-temp-buffer
              (insert-file-contents f)
              (dolist (n '("nl_alloc_str_write_legacy" "(nl_symname_make"))
                (goto-char (point-min))
                (when (search-forward n nil t) (push (format "%s mentions %s" f n) bad)))))
          (let ((m (car (gethash "nl_sym_make_uninterned" snw-scan-defuns))))
            (when (and m (let ((found nil))
                           (snw-scan-walk (cdr m) (lambda (f) (when (eq (car f) 'nl_gc_in_arena) (setq found t))))
                           found))
              (push "nl_sym_make_uninterned still tests nl_gc_in_arena (an arena-box branch)" bad)))
          (dolist (b bad) (princ (format "INVARIANT BROKEN: %s\n" b)))
          (when bad (setq extra (append extra bad)))))
      (princ (format "%s: %d defuns by shape, %d pinned, %d not pinned, %d missing\n"
                     mode (length found) (length pinned) (length extra) (length missing)))
      (kill-emacs (if (or extra missing) 1 0)))))
