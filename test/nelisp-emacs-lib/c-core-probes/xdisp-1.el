(bidi-find-overridden-directionality
  (bidi-find-overridden-directionality 1 4 "abc")
  (bidi-find-overridden-directionality 1 4 "a‮bc"))
 (bidi-resolved-levels
  (vectorp (bidi-resolved-levels))
  (bidi-resolved-levels 999))
 (buffer-text-pixel-size
  (buffer-text-pixel-size)
  (buffer-text-pixel-size (get-buffer-create " *xdisp-pixels*") nil 8 4))
 (current-bidi-paragraph-direction
  (current-bidi-paragraph-direction)
  (with-temp-buffer (insert "אabc") (current-bidi-paragraph-direction)))
 (display--line-is-continued-p
  (display--line-is-continued-p)
  (with-temp-buffer (insert (make-string 1000 ?x)) (display--line-is-continued-p)))
 (format-mode-line
  (format-mode-line "%b")
  (with-temp-buffer (insert "xyz") (format-mode-line "[%b] %l")))
 (get-display-property
  (get-display-property 1 'height "abc")
  (let ((s (propertize "x" 'display '(height 3)))) (get-display-property 1 'height nil '(height 3))))
 (line-pixel-height
  (line-pixel-height)
  (with-temp-buffer (insert "tall\nnext") (line-pixel-height)))
 (long-line-optimizations-p
  (long-line-optimizations-p)
  (with-temp-buffer (setq-local long-line-threshold 1) (insert "long") (long-line-optimizations-p)))
 (lookup-image-map
  (lookup-image-map nil 0 0)
  (lookup-image-map '(((rect . ((1 . 1) . (5 . 5))) id (:x 1))) 3 4))
 (move-point-visually
  (with-temp-buffer (insert "abc") (goto-char 2) (condition-case e (move-point-visually 1) (error (list (car e) (cdr e)))))
  (with-temp-buffer (insert "abc") (goto-char 2) (condition-case e (move-point-visually -1) (error (list (car e) (cdr e))))))
 (remember-mouse-glyph
  (condition-case e (remember-mouse-glyph nil 0 0) (error (car e)))
  (let ((f (selected-frame))) (condition-case e (remember-mouse-glyph f 1 1) (error (car e)))))
