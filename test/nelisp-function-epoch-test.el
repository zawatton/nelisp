;;; nelisp-function-epoch-test.el --- Doc 238 B0 function epoch (structural)  -*- lexical-binding: t; -*-

;;; Commentary:

;; Structural checks for stage B0 of Doc 238: a global function epoch
;; (`nl_function_epoch', one u64 in BSS) bumped by every operation that can
;; change what a symbol call resolves to, and a build-time scan that rejects any
;; write to a symbol entry's function cell (slot 1, object +24) outside the one
;; primitive `nl_symbol_set_function'.  Nothing here runs a NeLisp image.
;;
;; Overrides: NELISP_EPOCH_BUILD (build script path), NELISP_EPOCH_LISP
;; (directory holding the lisp/*.el DSL sources).

;;; Code:

(require 'ert)
(require 'cl-lib)

(defconst nelisp-function-epoch-test--root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))
  "Repository root, captured at load time.")

(defun nelisp-function-epoch-test--build ()
  (or (getenv "NELISP_EPOCH_BUILD")
      (expand-file-name "scripts/nelisp-standalone-build.el"
                        nelisp-function-epoch-test--root)))

(defun nelisp-function-epoch-test--lisp-dir ()
  (or (getenv "NELISP_EPOCH_LISP")
      (expand-file-name "lisp" nelisp-function-epoch-test--root)))

(defun nelisp-function-epoch-test--forms (file)
  "Return every top-level form of FILE, read with the real reader."
  (let (forms)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (condition-case nil
          (while t (push (read (current-buffer)) forms))
        (end-of-file nil)))
    (nreverse forms)))

(defvar nelisp-function-epoch-test--cache nil)

(defun nelisp-function-epoch-test--forms-cached (file)
  (let ((hit (assoc file nelisp-function-epoch-test--cache)))
    (unless hit
      (setq hit (cons file (nelisp-function-epoch-test--forms file)))
      (push hit nelisp-function-epoch-test--cache))
    (cdr hit)))

(defun nelisp-function-epoch-test--walk (form fn &optional defun-name)
  "Call FN with each cons sub-FORM and the name of its enclosing DSL defun."
  (when (consp form)
    (let ((dn defun-name))
      (when (and (eq (car form) 'defun) (consp (cdr form)) (symbolp (cadr form)))
        (setq dn (cadr form)))
      (funcall fn form dn)
      (while (consp form)
        (nelisp-function-epoch-test--walk (car form) fn dn)
        (setq form (cdr form))))))

(defun nelisp-function-epoch-test--defun (forms name)
  "Return the first (defun NAME ...) found in FORMS."
  (catch 'found
    (dolist (f forms)
      (nelisp-function-epoch-test--walk
       f (lambda (x _dn)
           (when (and (eq (car x) 'defun) (consp (cdr x)) (eq (cadr x) name))
             (throw 'found x)))))
    nil))

(defun nelisp-function-epoch-test--atoms (form)
  (cond ((consp form) (append (nelisp-function-epoch-test--atoms (car form))
                              (nelisp-function-epoch-test--atoms (cdr form))))
        ((null form) nil)
        (t (list form))))

(defun nelisp-function-epoch-test--contains-call-p (form head)
  (catch 'hit
    (nelisp-function-epoch-test--walk
     form (lambda (x _dn) (when (eq (car x) head) (throw 'hit t))))
    nil))

(defun nelisp-function-epoch-test--lisp-files ()
  (directory-files (nelisp-function-epoch-test--lisp-dir) t "\\.el\\'"))

;; ---- write-site scan ----

(defconst nelisp-function-epoch-test--writers
  '(record-slot-set nl_ent_set nl_record_set_slot)
  "Operators that can store into a symbol entry's slot K.")

(defun nelisp-function-epoch-test--entry-name-p (x)
  (and (symbolp x) (string-match-p "entry\\|\\`sym\\(bol\\)?\\'\\|\\`w\\'" (symbol-name x))))

(defun nelisp-function-epoch-test--scan-file (file)
  "Return (FILE DEFUN FORM) for each direct function-cell write in FILE.
A direct write is a writer call whose slot index is the literal 1 and whose
target is entry-named, or a raw ptr/heap write at object offset 24 through
\(- X 6), outside `nl_symbol_set_function'."
  (let (hits)
    (dolist (f (nelisp-function-epoch-test--forms-cached file))
      (nelisp-function-epoch-test--walk
       f (lambda (x dn)
           (unless (eq dn 'nl_symbol_set_function)
             (when (or
                    (and (memq (car x) nelisp-function-epoch-test--writers)
                         (proper-list-p x) (= (length x) 4)
                         (eql (nth 2 x) 1)
                         (nelisp-function-epoch-test--entry-name-p (nth 1 x)))
                    (and (memq (car x) '(ptr-write-u64 heap-write-u64))
                         (proper-list-p x) (>= (length x) 4)
                         (eql (nth 2 x) 24)
                         (consp (nth 1 x)) (eq (car (nth 1 x)) '-)
                         (eql (nth 2 (nth 1 x)) 6)))
               (push (list (file-name-nondirectory file) dn x) hits))))))
    (nreverse hits)))

(defun nelisp-function-epoch-test--scan-all ()
  (append (nelisp-function-epoch-test--scan-file (nelisp-function-epoch-test--build))
          (apply #'append
                 (mapcar #'nelisp-function-epoch-test--scan-file
                         (nelisp-function-epoch-test--lisp-files)))))

(ert-deftest nelisp-function-epoch-no-direct-function-cell-writes ()
  (let ((hits (nelisp-function-epoch-test--scan-all)))
    (should-not hits)))

;; ---- structure ----

(ert-deftest nelisp-function-epoch-primitive-bumps ()
  (let* ((forms (nelisp-function-epoch-test--forms-cached
                 (nelisp-function-epoch-test--build)))
         (prim (nelisp-function-epoch-test--defun forms 'nl_symbol_set_function))
         (bump (nelisp-function-epoch-test--defun forms 'nl_function_epoch_bump))
         (restore (nelisp-function-epoch-test--defun forms 'nl_function_epoch_restore)))
    (should prim)
    (should (nelisp-function-epoch-test--contains-call-p prim 'nl_ent_set))
    (should (nelisp-function-epoch-test--contains-call-p prim 'nl_function_epoch_bump))
    (should bump)
    (should (member '(data-addr nl_function_epoch)
                    (let (acc)
                      (nelisp-function-epoch-test--walk
                       bump (lambda (x _) (when (eq (car x) 'data-addr) (push x acc))))
                      acc)))
    (should restore)
    (should (memq 'nl_os_monotonic_ns (nelisp-function-epoch-test--atoms restore)))))

(ert-deftest nelisp-function-epoch-bss-slot-after-ctl-slots ()
  (let* ((forms (nelisp-function-epoch-test--forms-cached
                 (nelisp-function-epoch-test--build)))
         links size)
    (dolist (f forms)
      (nelisp-function-epoch-test--walk
       f (lambda (x _)
           (when (and (eq (car x) 'nelisp-link-symbol) (consp (cdr x)) (stringp (cadr x)))
             (push x links)))))
    (let ((ctl (cl-find "nl_ctl_slots" links :key #'cadr :test #'equal))
          (ep (cl-find "nl_function_epoch" links :key #'cadr :test #'equal)))
      (should ctl)
      (should ep)
      ;; Offset = ctl_slots offset + its 64 bytes + the census table's
      ;; (if (nelisp-standalone--census-p) 512 0) (Doc 238 A0).
      (should (equal (caddr ep)
                     (append (caddr ctl)
                             '(64 (if (nelisp-standalone--census-p) 512 0))))))
    (setq size (nelisp-function-epoch-test--defun
                forms 'nelisp-standalone--driver-bss-size))
    (let ((tail (memq 2072 (nelisp-function-epoch-test--atoms size))))
      (should (equal (cl-subseq tail 0 8) '(2072 64 if nelisp-standalone--census-p 512 0 8 16))))))

(ert-deftest nelisp-function-epoch-builtin-registered ()
  (with-temp-buffer
    (insert-file-contents (nelisp-function-epoch-test--build))
    (let ((text (buffer-string)))
      (should (string-match-p "(:lit \"nelisp--function-epoch\") :word \\. (nl_function_epoch_w 0)" text))
      (should (string-match-p "nelisp--region-stats nelisp--function-epoch)" text))
      (should (string-match-p "\"nelisp--region-stats\" \"nelisp--function-epoch\"" text))
      (should (string-match-p "(nl_boot_phase_mark 1 1)\n *(nl_function_epoch_restore)" text)))))

(ert-deftest nelisp-function-epoch-policy-writers-bump ()
  "Every DSL defun that rewrites the debug-policy bit 1048576 bumps the epoch."
  (let ((forms (nelisp-function-epoch-test--forms-cached
                (nelisp-function-epoch-test--build)))
        (writers nil))
    (dolist (f forms)
      (nelisp-function-epoch-test--walk
       f (lambda (x dn)
           (when (and dn (eq (car x) 'ptr-write-u64)
                      (memq -1048577 (nelisp-function-epoch-test--atoms x)))
             (cl-pushnew dn writers)))))
    (should writers)
    (dolist (dn writers)
      (should (nelisp-function-epoch-test--contains-call-p
               (nelisp-function-epoch-test--defun forms dn)
               'nl_function_epoch_bump)))))

(ert-deftest nelisp-function-epoch-mirror-routes ()
  "The mirror function-cell writers call the primitive; insert paths bump."
  (let ((dir (nelisp-function-epoch-test--lisp-dir)))
    (dolist (spec '(("nelisp-cc-mirror-set-function.el" nl_symbol_set_function)
                    ("nelisp-cc-mirror-clear-function.el" nl_symbol_set_function)
                    ("nelisp-cc-mirror-install-entry.el" nl_symbol_set_function)
                    ("nelisp-cc-mirror-install-entry-or-insert.el" nl_symbol_set_function nl_function_epoch_bump)
                    ("nelisp-cc-mirror-set-function-or-insert.el" nl_symbol_set_function nl_function_epoch_bump)
                    ("nelisp-cc-mirror-alias-install.el" nl_symbol_set_function)
                    ("nelisp-cc-variable-alias-public.el" nl_function_epoch_bump)))
      (let ((forms (nelisp-function-epoch-test--forms-cached
                    (expand-file-name (car spec) dir))))
        (dolist (head (cdr spec))
          (should (cl-some (lambda (f) (nelisp-function-epoch-test--contains-call-p f head))
                           forms)))))))

(provide 'nelisp-function-epoch-test)
;;; nelisp-function-epoch-test.el ends here
