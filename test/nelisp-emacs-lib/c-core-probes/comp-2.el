(native-elisp-load
  '(condition-case e (native-elisp-load "") (error e))
  '(condition-case e (native-elisp-load nil) (error e))
  '(condition-case e (native-elisp-load "/definitely/missing/comp-2.eln" t) (error e))
  '(let ((file (make-temp-file "comp-2-")))
     (unwind-protect
         (condition-case e (native-elisp-load file t) (error e))
       (delete-file file))))
