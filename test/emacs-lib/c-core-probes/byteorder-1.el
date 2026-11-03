(byteorder
 (byteorder)
 (condition-case err
     (byteorder 1)
   (wrong-number-of-arguments (list 'ERR (car err)))))
