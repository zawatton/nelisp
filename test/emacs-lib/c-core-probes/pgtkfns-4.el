(xw-color-defined-p
 (condition-case e (xw-color-defined-p "red") (error e))
 (condition-case e (xw-color-defined-p "#123456" (selected-frame)) (error e))
 (condition-case e (xw-color-defined-p "red" 3) (error e)))
(xw-color-values
 (condition-case e (xw-color-values "red") (error e))
 (condition-case e (xw-color-values "not-a-color" (selected-frame)) (error e))
 (condition-case e (xw-color-values nil 3) (error e)))
(xw-display-color-p
 (condition-case e (xw-display-color-p) (error e))
 (condition-case e (xw-display-color-p nil) (error e))
 (condition-case e (xw-display-color-p 3) (error e)))
