(frame-initial-p
 (frame-initial-p)
 (frame-initial-p (selected-frame))
 (let ((f (selected-frame))) (list (framep f) (frame-initial-p f)))
 (condition-case e (frame-initial-p 'bad) (error (list (car e) (cdr e)))))
(terminal-list
 (length (terminal-list))
 (let ((f (selected-frame))) (list (framep f) (> (length (terminal-list)) 0))))
(terminal-name
 (terminal-name)
 (terminal-name (selected-frame))
 (condition-case e (terminal-name 'bad) (error (list (car e) (cdr e))))
 (let ((w (split-window))) (prog1 (terminal-name) (delete-window w))))
(terminal-parameters
 (listp (terminal-parameters))
 (assq 'normal-erase-is-backspace (terminal-parameters))
 (condition-case e (terminal-parameters 'bad) (error (list (car e) (cdr e))))
 (let ((w (split-window))) (prog1 (listp (terminal-parameters)) (delete-window w))))
