(setq print-escape-newlines t)
(princ (format "%S\n"
  (condition-case e
      (let ((r (nby--make :a 0 :b 1)))
        (nbx-bump r) (nbx-bump r) (nby-push r 'x) (nby-push r 'y) (nbx-incf r) (nbx-incf r)
        (list (nbx-b r) (nby-c r) (nbx-a r) (nbx-bump r)))
    (error (list :error e)))))
