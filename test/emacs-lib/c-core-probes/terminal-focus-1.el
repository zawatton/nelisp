(tty-top-frame
 (tty-top-frame)
 (tty-top-frame (selected-frame))
 (tty-top-frame nil)
 (condition-case err
     (tty-top-frame 'bad-terminal)
   (error (list 'ERR (car err) (cdr err))))
 (condition-case err
     (apply #'tty-top-frame '(nil nil))
   (wrong-number-of-arguments
    (list 'ERR (car err) '(tty-top-frame 2))))
)
(terminal-parameter
 (terminal-parameter nil 'normal-erase-is-backspace)
 (terminal-parameter (selected-frame) 'normal-erase-is-backspace)
 (terminal-parameter nil 'tty-focus-state)
 (condition-case err
     (terminal-parameter 'bad-terminal 'tty-focus-state)
   (error (list 'ERR (car err) (cdr err))))
 (condition-case err
     (apply #'terminal-parameter '(nil))
   (wrong-number-of-arguments
    (list 'ERR (car err) '(terminal-parameter 1))))
)
