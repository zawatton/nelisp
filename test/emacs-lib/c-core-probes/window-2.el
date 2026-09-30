(set-window-combination-limit
  (condition-case e (set-window-combination-limit (selected-window) nil) (error e))
  (condition-case e (set-window-combination-limit nil nil) (error e)))
 (set-window-cursor-type
  (condition-case e (progn (set-window-cursor-type nil t) t) (error e))
  (condition-case e (set-window-cursor-type 1 t) (error e)))
 (set-window-new-normal
  (condition-case e (set-window-new-normal nil 3) (error e))
  (condition-case e (set-window-new-normal 1 3) (error e)))
 (set-window-new-pixel
  (condition-case e (set-window-new-pixel nil 3) (error e))
  (condition-case e (set-window-new-pixel nil -1) (error e)))
 (set-window-new-total
  (condition-case e (set-window-new-total nil 3) (error e))
  (condition-case e (set-window-new-total 1 3) (error e)))
 (set-window-scroll-bars
  (condition-case e (set-window-scroll-bars nil) (error e))
  (condition-case e (set-window-scroll-bars 1) (error e)))
 (set-window-vscroll
  (condition-case e (set-window-vscroll nil 3 t) (error e))
  (condition-case e (set-window-vscroll nil nil) (error e)))
 (split-window-internal
  (condition-case e (split-window-internal (selected-window) 3 nil 1) (error e))
  (condition-case e (split-window-internal 1 3 nil 1) (error e)))
 (uncombine-window
  (condition-case e (uncombine-window (selected-window)) (error e))
  (condition-case e (uncombine-window 1) (error e)))
 (window-at
  (condition-case e (windowp (window-at 0 0)) (error e))
  (condition-case e (window-at nil 0) (error e)))
 (window-bottom-divider-width
  (condition-case e (window-bottom-divider-width) (error e))
  (condition-case e (window-bottom-divider-width 1) (error e)))
 (window-bump-use-time
  (condition-case e (window-bump-use-time) (error e))
  (condition-case e (window-bump-use-time 1) (error e)))
