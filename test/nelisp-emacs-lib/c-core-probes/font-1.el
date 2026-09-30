(clear-font-cache
  (clear-font-cache)
  (let ((before (font-family-list))) (clear-font-cache) (list before (font-family-list))))
 (close-font
  (condition-case e (close-font nil) (error e))
  (condition-case e (close-font nil (selected-frame)) (error e)))
 (find-font
  (condition-case e (find-font nil) (error e))
  (condition-case e (find-font "serif") (error e)))
 (font-at
  (condition-case e (font-at "x") (error e))
  (let ((s (propertize "abc" 'face 'bold))) (list (font-at 1 nil s) (font-at 2 nil s))))
 (font-face-attributes
  (font-face-attributes "serif")
  (condition-case e (font-face-attributes nil) (error e)))
 (font-family-list
  (font-family-list)
  (condition-case e (font-family-list 1) (error e)))
 (font-get
  (condition-case e (font-get nil :family) (error e))
  (condition-case e (font-get nil :family) (error e)))
 (font-get-glyphs
  (condition-case e (font-get-glyphs nil 1 1) (error e))
  (condition-case e (font-get-glyphs nil 0 3 "abc") (error e)))
 (font-has-char-p
  (condition-case e (font-has-char-p nil ?a) (error e))
  (condition-case e (font-has-char-p nil ?z) (error e)))
 (font-info
  (condition-case e (font-info "serif") (error e))
  (condition-case e (font-info nil) (error e)))
 (font-match-p
  (condition-case e (font-match-p nil nil) (error e))
  (condition-case e (font-match-p "serif" nil) (error e)))
 (fontp
  (list (fontp "not-a-font") (fontp nil 'font-spec))
  (condition-case e (fontp nil 'bad-font-kind) (error e)))
