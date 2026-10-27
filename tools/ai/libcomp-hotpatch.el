;;; libcomp-hotpatch.el --- define the libcomp lane's changed defuns on a v25 binary  -*- lexical-binding: t; -*-
;; usage (repository root): BINARY --load tools/ai/libcomp-hotpatch.el [--load ...]
;; Evaluates, from the working tree's sources, the defuns and macros the lane
;; changed, plus the ones the byte-code pass now keeps as source (the binary
;; holds those as byte code), as lexical closures -- how the image defines
;; them (their closures' environment is (t)).  Top-level definitions and those
;; under `(unless (fboundp ...))' are both found.  Used to harvest native rows
;; (tools/ai/libcomp-harvest.el) and to run the parity corpus on the word VM.
(defvar nlre--nat-last nil)
(defvar nlre--nat-last-syn nil)
(defconst libcomp-hotpatch-names
  '(;; lisp/nelisp-stdlib-regexp.el: changed, and moved from byte code to source
    nlre--re-search-forward nlre--re-search-forward-lisp nlre--nat-fold-default-p
    nlre--nat-bsearch nlre--nat-layout-ok nlre--nat-buffer-text nlre--nat-program
    nlre--nat-program-syn nlre--nat-syn-ensure nlre--nat-looking-at
    ;; scripts/nelisp-stdlib-prelude.el
    copy-marker nelisp--run-hook-call nelisp--run-hook-value
    nelisp-cl-generic--uses-cnm-p nelisp-cl-generic--compute-em nelisp-cl-generic--call
    cl-defmethod type-of nelisp--record-type))

(defun libcomp-hotpatch--file (file)
  (with-temp-buffer
    (insert-file-contents (expand-file-name file))
    (goto-char (point-min))
    (let ((form t))
      (while (setq form (condition-case nil (read (current-buffer)) (end-of-file nil)))
        (dolist (f (if (and (eq (car-safe form) 'unless)
                            (eq (car-safe (nth 1 form)) 'fboundp))
                       (nthcdr 2 form)
                     (list form)))
          (when (and (memq (car-safe f) '(defun defmacro))
                     (memq (nth 1 f) libcomp-hotpatch-names))
            (eval f t)
            (princ (format "HOTPATCH %s\n" (nth 1 f)))))))))

(libcomp-hotpatch--file "lisp/nelisp-stdlib-regexp.el")
(libcomp-hotpatch--file "scripts/nelisp-stdlib-prelude.el")
