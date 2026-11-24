;;; -*- lexical-binding: t; -*-
;; Batch terminals without glyph codes let char-displayable-p reach the codec.
(with-temp-buffer
  (prin1
   (list (condition-case err (internal-char-font nil ?») (error err))
         (condition-case err (progn (char-displayable-p ?») 'returned)
           (error err))
         (condition-case err (internal-char-font nil -1) (error err))))
  (terpri)
  (insert "abc")
  (narrow-to-region 2 3)
  (prin1
   (list (condition-case err (internal-char-font (copy-marker 2)) (error err))
         (condition-case err (internal-char-font 3) (error err))
         (condition-case err (internal-char-font 2 -1) (error err))))
  (terpri))
