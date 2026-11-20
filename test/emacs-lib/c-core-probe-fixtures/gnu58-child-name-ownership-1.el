;;; gnu58-child-name-ownership-1.el --- private name ownership regressions -*- lexical-binding: t; -*-
;; Use a symmetric fixture driver when one is provided; the current --extra
;; smoke path is asymmetric. This directory stays outside the automatic census.
(process-name
  (let* ((n1 "sol-cross-one") (n2 "sol-cross-two") p1 p2)
    (unwind-protect
        (progn
          (setq p1 (make-process :name n1 :buffer nil :command (list (executable-find "cat"))
                                 :connection-type 'pipe :noquery t)
                p2 (make-process :name n2 :buffer nil :command (list (executable-find "cat"))
                                 :connection-type 'pipe :noquery t))
          (process-put p1 :name n2)
          (process-put p2 :name n1)
          (list (process-name p1) (process-name p2)
                (process-get p1 :name) (process-get p2 :name)
                (eq (get-process n1) p1) (eq (get-process n2) p2)))
      (when p1 (ignore-errors (delete-process p1)))
      (when p2 (ignore-errors (delete-process p2)))))
  (let* ((name "sol-deleted-held")
         (p (make-process :name name :buffer nil :command (list (executable-find "cat"))
                          :connection-type 'pipe :noquery t
                          :sentinel (lambda (_process _message) nil)))
         (held (list p)) (before (process-name p)))
    (unwind-protect
        (progn
          (delete-process p)
          (setq p nil)
          (garbage-collect)
          (let ((child (car held)))
            (list before (process-name child) (process-get child :name)
                  (not (process-live-p child)))))
      (ignore-errors (delete-process (car held)))))
  (let* ((name "sol-name-property")
         (p (make-process :name name :buffer nil
                          :command (list (executable-find "cat"))
                          :connection-type 'pipe :noquery t)))
    (unwind-protect
        (progn
          (process-put p :name "public-property")
          (list (process-name p) (process-get p :name)
                (eq (get-process name) p) (process-status name)))
      (ignore-errors (delete-process p))))
  (let* ((name "sol-name-held-gc")
         (p (start-process name nil (executable-find "cat")))
         (held (list p)))
    (unwind-protect
        (progn
          (setq p nil)
          (garbage-collect)
          (let ((child (car held)))
            (list (process-name child) (process-get child :name)
                  (eq (get-process name) child) (process-status name))))
      (ignore-errors (delete-process (car held))))))

;;; gnu58-child-name-ownership-1.el ends here
