(next-char-property-change
 (with-temp-buffer
   (insert "abcdef")
   (put-text-property 3 5 'face 'bold)
   (list (next-char-property-change 1)
         (next-char-property-change 3)
         (next-char-property-change 5)))
 (let ((first
        (with-temp-buffer
          (insert "abcdef")
          (let ((overlay (make-overlay 4 5)))
            (overlay-put overlay 'face 'bold)
            (next-char-property-change 3))))
       (second
        (with-temp-buffer
          (insert "abcdef")
          (let ((overlay (make-overlay 3 3)))
            (overlay-put overlay 'face 'bold)
            (next-char-property-change 3)))))
   (list first second)))
(previous-char-property-change
 (with-temp-buffer
   (insert "abcdef")
   (put-text-property 3 5 'face 'bold)
   (list (previous-char-property-change 6)
         (previous-char-property-change 5)
         (previous-char-property-change 3))))
