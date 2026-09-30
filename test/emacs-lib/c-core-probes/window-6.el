(window-text-height
 (condition-case e (window-text-height) (error e))
 (condition-case e (window-text-height 1) (error e)))
(window-text-width
 (condition-case e (window-text-width) (error e))
 (condition-case e (window-text-width nil t) (error e)))
(window-top-child
 (condition-case e (windowp (window-top-child)) (error e))
 (condition-case e (window-top-child 1) (error e)))
(window-top-line
 (condition-case e (window-top-line) (error e))
 (condition-case e (window-top-line 1) (error e)))
(window-total-height
 (condition-case e (window-total-height) (error e))
 (condition-case e (window-total-height nil 'ceiling) (error e)))
(window-total-width
 (condition-case e (window-total-width) (error e))
 (condition-case e (window-total-width nil 'floor) (error e)))
(window-use-time
 (condition-case e (window-use-time) (error e))
 (condition-case e (window-use-time 1) (error e)))
(window-vscroll
 (condition-case e (window-vscroll) (error e))
 (condition-case e (window-vscroll nil t) (error e)))
