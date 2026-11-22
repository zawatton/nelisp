;;; nelisp-aot-buffer-leaf-image.el --- buffer leaf differential cases  -*- lexical-binding: nil; -*-

;; Data and a runner loadable by GNU Emacs and by the NeLisp runtime image
;; (like nelisp-aot-lisp-arith-cases.el).  NOT RUN until the batch build: the
;; bridged `nl_native_buf_leaf' exists only in a binary built with it.
;;
;; `nelisp-aot-buffer-leaf-image-run' calls every defun of the source under
;; test through CALL-FN in a set of buffer states and returns one text line
;; per call: the value (or signal) and the point afterwards.  GNU's lines
;; come from the byte-compiled source (bytecode opcodes), the native lines
;; from the artifact compiled with `nelisp-aot-compiler-buffer-leaves'.

;;; Code:

(defconst nelisp-aot-buffer-leaf-image-source
  "(defun blf-point (x) (point))
(defun blf-point-min (x) (point-min))
(defun blf-point-max (x) (point-max))
(defun blf-bobp (x) (bobp))
(defun blf-eobp (x) (eobp))
(defun blf-following-char (x) (following-char))
(defun blf-preceding-char (x) (preceding-char))
(defun blf-char-after (x) (char-after x))
(defun blf-char-after0 (x) (char-after))
(defun blf-goto-char (x) (goto-char x))
(defun blf-char-before (x) (char-before x))
(defun blf-char-before0 (x) (char-before))
(defun blf-bolp (x) (bolp))
(defun blf-eolp (x) (eolp))
(defun blf-buffer-size (x) (buffer-size))
(defun blf-loop (n) (let ((i 0) (s 0)) (while (< i n) (setq s (+ s (char-after 3))) (setq i (1+ i))) s))
"
  "The defuns under test; char-before is lowered like the others.")

(defconst nelisp-aot-buffer-leaf-image-names
  '("blf-point" "blf-point-min" "blf-point-max" "blf-bobp" "blf-eobp"
    "blf-following-char" "blf-preceding-char" "blf-char-after" "blf-char-after0"
    "blf-goto-char" "blf-char-before" "blf-char-before0" "blf-bolp" "blf-eolp"
    "blf-buffer-size"))

(defconst nelisp-aot-buffer-leaf-image-states
  ;; (LABEL INIT-FORMS...): run in a fresh buffer before the calls.
  '(("empty")
    ("plain-end" (insert "hello\nworld"))
    ("plain-start" (insert "hello\nworld") (goto-char 1))
    ("mid-insert-gap" (insert "hello\nworld") (goto-char 6) (insert "XY") (goto-char 3))
    ("gap-after-point" (insert "hello\nworld") (goto-char 3) (insert "XY") (goto-char 9))
    ("narrowed" (insert "hello\nworld") (narrow-to-region 3 9) (goto-char 5))
    ("narrowed-point-at-min" (insert "hello\nworld") (narrow-to-region 3 9) (goto-char 3))
    ("narrowed-point-at-max" (insert "hello\nworld") (narrow-to-region 3 9) (goto-char 9))
    ("multibyte" (insert "abあいうcd") (goto-char 3))
    ("point-at-max" (insert "hello") (goto-char (point-max))))
  "Buffer states: narrowing, a gap on both sides of point, multibyte text.")

(defconst nelisp-aot-buffer-leaf-image-args
  ;; Argument specs: literal values, (:marker N), (:pmax K) = point-max + K.
  '(nil 0 1 2 3 5 6 9 10 12 -1 100 (:pmax -1) (:pmax 0) (:pmax 1) (:pmax 2)
    (:marker 1) (:marker 4) (:marker 8) (:marker 12)
    2305843009213693951 -2305843009213693952 4611686018427387904
    a "s" 1.5 (1) (:unset-marker))
  "Positions, markers, out-of-range, bignums and wrong types.")

(defun nelisp-aot-buffer-leaf-image--arg (spec)
  (cond ((and (consp spec) (eq (car spec) :marker))
         (copy-marker (min (cadr spec) (1+ (buffer-size)))))
        ((and (consp spec) (eq (car spec) :pmax)) (+ (point-max) (cadr spec)))
        ((and (consp spec) (eq (car spec) :unset-marker)) (make-marker))
        (t spec)))

(defun nelisp-aot-buffer-leaf-image--show (v)
  (if (markerp v) (format "M%S" (marker-position v)) (format "%S" v)))

(defun nelisp-aot-buffer-leaf-image-run (call-fn)
  "Return the lines of every defun under every state and argument."
  (let ((lines nil))
    (dolist (st nelisp-aot-buffer-leaf-image-states)
      (dolist (name nelisp-aot-buffer-leaf-image-names)
        (dolist (spec nelisp-aot-buffer-leaf-image-args)
          (let ((buf (generate-new-buffer " blf")))
            (set-buffer buf)
            (dolist (f (cdr st)) (eval f))
            (let* ((arg (nelisp-aot-buffer-leaf-image--arg spec))
                   (res (condition-case err
                            (list 'ok (funcall call-fn name (list arg)))
                          (error (list 'signal (car err)
                                       (mapcar (lambda (d) (if (markerp d) 'marker d))
                                               (cdr err))))))
                   (pt (point)))
              (setq lines
                    (cons (format "%s %s %S => %s pt=%S" (car st) name spec
                                  (if (eq (car res) 'ok)
                                      (nelisp-aot-buffer-leaf-image--show (cadr res))
                                    (format "%S" res))
                                  pt)
                          lines)))
            (kill-buffer buf)))))
    (nreverse lines)))

(provide 'nelisp-aot-buffer-leaf-image)
;;; nelisp-aot-buffer-leaf-image.el ends here
