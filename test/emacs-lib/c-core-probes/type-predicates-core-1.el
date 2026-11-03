(arrayp
 (arrayp [a])
 (arrayp "a")
 (arrayp '(a)))
(atom
 (atom nil)
 (atom 'a)
 (atom '(a)))
(bufferp
 (bufferp (current-buffer))
 (bufferp 'buffer))
(consp
 (consp '(a))
 (consp nil))
(floatp
 (floatp 1.0)
 (floatp 1))
(functionp
 (functionp (symbol-function 'car))
 (functionp 3))
(integerp
 (integerp 1)
 (integerp 1.0))
(keywordp
 (keywordp :name)
 (keywordp 'name))
(listp
 (listp nil)
 (listp '(a . b))
 (listp [a]))
(markerp
 (markerp (make-marker))
 (markerp 1))
(natnump
 (natnump 0)
 (natnump -1))
(null
 (null nil)
 (null 0))
(numberp
 (numberp 1.5)
 (numberp 'one))
(sequencep
 (sequencep '(a))
 (sequencep [a])
 (sequencep "a")
 (sequencep 1))
(stringp
 (stringp "a")
 (stringp 'a))
(symbolp
 (symbolp nil)
 (symbolp 'a)
 (symbolp "a"))
(vectorp
 (vectorp [a])
 (vectorp '(a)))
