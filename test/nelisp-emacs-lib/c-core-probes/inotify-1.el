(inotify-add-watch
  (let ((d (inotify-add-watch "/tmp" '(create modify) (lambda (_) nil))))
    (prog1 (and (consp d) (integerp (car d)) (inotify-valid-p d)) (inotify-rm-watch d)))
  (condition-case e (inotify-add-watch nil t nil) (error e))
  (condition-case e (inotify-add-watch "/no/such/inotify-probe-path" t nil) (error e))
  (condition-case e (inotify-add-watch "/tmp" 'unknown nil) (error e))
  (let ((d (inotify-add-watch (make-temp-file "inotify-probe-" t) 'onlydir nil)))
    (prog1 (inotify-valid-p d) (inotify-rm-watch d))))
 (inotify-rm-watch
  (condition-case e (inotify-rm-watch nil) (error e))
  (let ((d (inotify-add-watch "/tmp" 'create nil)))
    (list (inotify-rm-watch d) (inotify-valid-p d))))
 (inotify-valid-p
  (inotify-valid-p nil)
  (let ((d (inotify-add-watch "/tmp" 'modify nil)))
    (prog1 (inotify-valid-p d) (inotify-rm-watch d))))
