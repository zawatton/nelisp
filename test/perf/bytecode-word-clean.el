;;; bytecode-word-clean.el --- word-cleanliness audit of the bytecode VM -*- lexical-binding: t -*-

;; Usage (host GNU Emacs, repository root):
;;   emacs --batch -Q -l test/perf/bytecode-word-clean.el [--eval '(setq bwcl-verbose t)']
;;
;; Reads every `(defun wf_bytecode_* ...)' in scripts/nelisp-standalone-build.el
;; and counts the 32-byte box transports the word interpreter must not use:
;; `(alloc-bytes 32 8)' scratch boxes and calls to box-view materializers
;; (symbols ending in `_ptr' or `-ptr', e.g. nl_cons_car_ptr,
;; record-slot-ref-ptr, wf_arg_ptr), other than raw data pointers listed in
;; `bwcl-allowed'.  Functions in `bwcl-boundary-fns' are skipped: they
;; are the named boundary helpers where a heap word's own 32-byte box is
;; allocated (the value itself, not scratch) or where a still-boxed callee
;; is entered.  Prints one BYTECODE-WORD-CLEAN line per function with a
;; hit and a total; exits non-zero when the total is above BWCL_MAX
;; (default 0).

(defvar bwcl-verbose nil)
(defconst bwcl-allowed '(nl_bi_strptr)
  "Symbols ending in ptr that answer raw data addresses, not box views.")

(defconst bwcl-boundary-fns '(wf_bytecode_w_newbox)
  "Named boundary helpers allowed to allocate a heap word's 32-byte box.
A heap object's word is a pointer to its own immutable 32-byte box, so
building a new value (a cons result, a boxed callee's heap result)
allocates exactly one; every such site goes through these helpers.")

(defun bwcl-count (form)
  (let ((n 0))
    (cond
     ((equal form '(alloc-bytes 32 8)) (setq n 1))
     ((and (symbolp form) (not (memq form bwcl-allowed))
           (string-match-p "[-_]ptr\\'" (symbol-name form)))
      (setq n 1))
     ((consp form)
      (while (consp form)
        (setq n (+ n (bwcl-count (car form))))
        (setq form (cdr form)))
      (when form (setq n (+ n (bwcl-count form))))))
    n))

(let* ((root (file-name-directory (directory-file-name
                                   (file-name-directory
                                    (directory-file-name
                                     (file-name-directory load-file-name))))))
       (file (expand-file-name "scripts/nelisp-standalone-build.el" root))
       (total 0) (fns 0)
       (max (string-to-number (or (getenv "BWCL_MAX") "0"))))
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (while (re-search-forward "(defun \\(wf_bytecode[a-z_0-9]*\\) " nil t)
      (let* ((start (match-beginning 0))
             (name (match-string 1))
             (form (save-excursion (goto-char start) (read (current-buffer))))
             ;; Skip the name and argument list.
             (hits (if (memq (intern name) bwcl-boundary-fns) 0
                     (bwcl-count (cdddr form)))))
        (setq fns (1+ fns))
        (when (> hits 0)
          (setq total (+ total hits))
          (when bwcl-verbose (princ (format "BYTECODE-WORD-CLEAN %s %d\n" name hits)))))))
  (princ (format "BYTECODE-WORD-CLEAN total %d in %d wf_bytecode functions (max %d) %s\n"
                 total fns max (if (<= total max) "PASS" "FAIL")))
  (kill-emacs (if (<= total max) 0 1)))
