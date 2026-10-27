;;; textprop-mutant.el --- deliberate defects for the textprop lane's negative controls  -*- lexical-binding: t; -*-
;; Loaded before a corpus, it must make the corpus differ from GNU Emacs.  It wraps public names (a call
;; through the function cell), so it works on a binary that runs the engine as native code too.
(let ((f (symbol-function 'next-single-property-change)))
  (fset 'next-single-property-change
        (lambda (&rest a) (let ((r (apply f a))) (if (integerp r) (1+ r) r)))))
(let ((f (symbol-function 'remove-text-properties)))
  (fset 'remove-text-properties (lambda (&rest a) (apply f a) nil)))
