;;; batch-f-keymap-cmacro-invalid.el --- keymap.el compiler macros on invalid keys -*- lexical-binding: t; -*-

;; GNU keymap--compile-check and define-keymap--compile warn through
;; `byte-compile-warn' and `byte-compile-warn-x', which only bytecomp.el defines
;; and a plain `emacs -Q --batch' has not loaded: both are void there.  The
;; eager expansion that `load' does (lread.c readevalloop_eager_expand_eval)
;; therefore reaches the handler's error path, and macroexp.el reports
;;   Warning: Optimization failure for keymap-set: Handler: keymap-set--anon-cmacro
;;   (void-function byte-compile-warn)
;; on stderr, once per bad form and before the form runs.  The runtime must
;; stage the same GNU helpers, leave the two warn functions as void as GNU does,
;; and print the same text.  The golden .gnu-31.1.stderr is GNU's own stderr.
;; Nothing is printed to stdout until the last form: GNU's batch `message' puts
;; a separator newline on stderr after any stdout output, which is not part of
;; the warning.

(defvar bki-map (make-sparse-keymap))
(defvar bki-results nil)

(defun bki-try (label thunk)
  (push (condition-case err
            (progn (funcall thunk) (format "%s: no error" label))
          (error (format "%s: %S" label err)))
        bki-results))

(bki-try "string key" (lambda () (keymap-set bki-map "C-cc a" #'ignore)))
(bki-try "vector key" (lambda () (keymap-set bki-map [f1] #'ignore)))
(bki-try "valid key" (lambda () (keymap-set bki-map "C-c a" #'ignore)))
(bki-try "define-keymap key" (lambda () (define-keymap "C-cc a" #'ignore)))
(bki-try "define-keymap keyword" (lambda () (define-keymap :bogus 1 "C-c a" #'ignore)))
(bki-try "define-keymap uneven" (lambda () (define-keymap "C-c a")))
(bki-try "define-keymap valid" (lambda () (define-keymap "C-c a" #'ignore)))

(princ (format "void in this session: %S\n"
               (mapcar #'fboundp '(byte-compile-warn byte-compile-warn-x
                                   keymap--compile-check define-keymap--compile))))
(dolist (line (reverse bki-results)) (princ line) (princ "\n"))
