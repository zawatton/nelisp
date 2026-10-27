;;; slots.el --- the textprop macros' hard-coded nelisp-buffer slots equal the struct accessors  -*- lexical-binding: t; -*-
;; usage: BINARY --load tools/ai/textprop/slots.el   prints SLOTS-OK or SLOTS-BAD
(with-temp-buffer
  (insert (make-string 30 ?a))
  (put-text-property 3 9 'face 'bold)
  (narrow-to-region 5 20)
  (let* ((b (current-buffer))
         (ok (and (eq (aref b 0) 'nelisp-buffer)
                  (eq (aref b 7) (nelisp-buffer-narrow-start b))
                  (eq (aref b 8) (nelisp-buffer-narrow-end b))
                  (eq (aref b 9) (nelisp-buffer-text-properties b))
                  (eq (aref b 14) (nelisp-buffer-len b))
                  (aref b 9) (aref b 7) (aref b 8) (= (aref b 14) 30))))
    (widen)
    (princ (if (and ok (not (aref b 7)) (not (aref b 8))) "SLOTS-OK\n" "SLOTS-BAD\n"))))
(kill-emacs 0)
