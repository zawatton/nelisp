(internal-subr-documentation
 (internal-subr-documentation 'car)
 (internal-subr-documentation 'not-a-primitive))
(Snarf-documentation
 (condition-case e (Snarf-documentation "") (error e))
 (condition-case e (Snarf-documentation nil) (error e)))
(text-quoting-style
 (let ((text-quoting-style nil)) (text-quoting-style))
 (let ((text-quoting-style 'straight)) (text-quoting-style))
 (let ((text-quoting-style 'grave)) (text-quoting-style)))
