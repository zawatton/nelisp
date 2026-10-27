;;; emit1.el --- Host-emit runtime defuns through the build's unit compiler -*- lexical-binding: t; -*-
;; Usage (from the worktree root):
;;   emacs -Q --batch -L lisp -L src -L scripts -l tools/ai/regalloc/emit1.el OUT.bin NAME...
;; Reads each `(defun NAME' form from scripts/nelisp-standalone-build.el, compiles them as one unit
;; with `nelisp-standalone--compile-to-unit' (Linux x86-64; other units' functions stay external
;; calls) and writes the unit's text bytes to OUT.bin, plus OUT.bin.labels (offset name).
(require 'cl-lib)
(setq load-prefer-newer t)
(setenv "NELISP_STANDALONE_TARGET" "linux-x86_64")
(require 'nelisp-standalone-build)
(let* ((out (car command-line-args-left))
       (names (cdr command-line-args-left))
       (max-lisp-eval-depth 40000)
       (forms (if (equal (car names) ":applyfn") nil (with-temp-buffer
                (insert-file-contents "scripts/nelisp-standalone-build.el")
                (mapcar (lambda (name)
                          (goto-char (point-min))
                          (re-search-forward (concat "(defun " (regexp-quote name) "[ \n]"))
                          (goto-char (match-beginning 0))
                          (read (current-buffer)))
                        names))))
       (t0 (float-time))
       (unit (if (equal (car names) ":applyfn")
                 ;; The whole reader applyfn unit, as `nelisp-standalone-build-reader' builds it.
                 (let ((nelisp-aot-compiler--x86-direct-only-defuns
                        nelisp-standalone--direct-only-reader-defuns))
                   (nelisp-standalone--compile-to-unit
                    "applyfn-reader.o" (nelisp-standalone--patch-autoload-lookups
                                        (nelisp-standalone--applyfn-source))))
               (nelisp-standalone--compile-to-unit "regalloc-probe" (cons 'seq forms)))))
  (message "emit1: secs %S" (mapcar (lambda (x) (if (consp x) (mapcar (lambda (y) (if (or (stringp y) (vectorp y)) (list (type-of y) (length y)) y)) (if (proper-list-p x) x (list (car x) (cdr x)))) x)) (plist-get unit :sections)))
  (message "emit1: unit type %S keys %S in %.1fs" (type-of unit)
           (and (consp unit) (cl-loop for x in unit by #'cddr when (keywordp x) collect x))
           (- (float-time) t0))
  (let ((text (let ((v (cdr (assq 'text (plist-get unit :sections))))) (if (stringp v) v (car v)))))
    (when (or (stringp text) (vectorp text))
      (let ((coding-system-for-write 'no-conversion))
        (write-region (concat text) nil out nil 'silent))
      (with-temp-file (concat out ".labels")
        (dolist (s (and (consp unit) (plist-get unit :symbols)))
          (insert (format "%S\n" s)))))))
(when (getenv "REGALLOC_REFUSALS")
  (dolist (r (and (boundp 'nelisp-aot-compiler--vm-reg-refusals) nelisp-aot-compiler--vm-reg-refusals))
    (message "refusal: slot %S site %s" (car r) (let ((print-length 12) (print-level 5)) (format "%S" (cdr r))))))
