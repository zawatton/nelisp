;;; textprop-hotpatch.el --- define the textprop lane's changed definitions on a v28 binary  -*- lexical-binding: t; -*-
;; usage (repository root): BINARY --load tools/ai/textprop-hotpatch.el [--load ...]
;; Evaluates, from scripts/nelisp-stdlib-prelude.el in the working tree, the macros and defuns the lane
;; changed or added, as lexical closures (how the image defines them).  Top-level definitions and those
;; under `(unless (fboundp ...))' are both found.  Used to harvest native rows
;; (tools/ai/textprop-harvest.el) and to run the parity corpus on the word VM before a build.
(defconst textprop-hotpatch-names
  '(nelisp--tp-bufp nelisp--tp-bmin nelisp--tp-bmax nelisp--tp-broot nelisp--tp-blen nelisp--tp-locate
    nelisp--tp-do-span nelisp--tp-cut nelisp--tp-add nelisp--tp-remove nelisp--tp-key-listed
    nelisp--tp-remove-props nelisp--tp-graft nelisp--tp-copy-clipped nelisp--tp-list nelisp--tp-cat-props
    nelisp--tp-sub-props nelisp--tp-boundaries-between nelisp--tp-copy-range nelisp--tp-rebuild
    nelisp--w202-property-find nelisp--tp-scan-fwd nelisp--tp-scan-back nelisp--w202-property-change
    nelisp--tp-check-object nelisp--tp-get-intervals nelisp--tp-set-intervals nelisp--tp-validate
    text-properties-at get-text-property nelisp--tp-fast put-text-property add-text-properties
    set-text-properties remove-text-properties remove-list-of-text-properties nelisp--tp-build propertize))

(defun textprop-hotpatch--file (file)
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
                     (memq (nth 1 f) textprop-hotpatch-names))
            (eval f t)
            (when (getenv "TEXTPROP_VERBOSE") (princ (format "HOTPATCH %s\n" (nth 1 f))))))))))
(textprop-hotpatch--file "scripts/nelisp-stdlib-prelude.el")
