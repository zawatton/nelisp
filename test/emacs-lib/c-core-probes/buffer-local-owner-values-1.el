;;; buffer-local-owner-values-1.el --- Live owner reads and defaults -*- lexical-binding: t; -*-

(buffer-local-value
 (let ((symbol (make-symbol "owner-live-read")))
   (set symbol 10)
   (with-temp-buffer
     (make-local-variable symbol)
     (set symbol 20)
     (let ((outer (current-buffer)))
       (list (buffer-local-value symbol outer)
             (with-temp-buffer
               (list (buffer-local-value symbol outer)
                     (buffer-local-value symbol (current-buffer))))
             (buffer-local-value symbol outer))))))

(default-value
 (let ((symbol (make-symbol "owner-default-write")))
   (set symbol 10)
   (let ((inside
          (with-temp-buffer
            (make-local-variable symbol)
            (set symbol 20)
            (eval (list 'setq-default symbol 30))
            (list (symbol-value symbol)
                  (buffer-local-value symbol (current-buffer))
                  (default-value symbol)))))
     (list inside (symbol-value symbol) (default-value symbol)))))
