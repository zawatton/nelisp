(abort-minibuffers
 (condition-case e (abort-minibuffers) (error (list (car e) (cadr e))))
 (with-temp-buffer (condition-case e (abort-minibuffers) (error (list (car e) (cadr e))))))
(completion--flex-cost-gotoh
 (completion--flex-cost-gotoh "foo" "foo")
 (completion--flex-cost-gotoh "ac" "abc")
 (completion--flex-cost-gotoh "z" "abc")
 (condition-case e (completion--flex-cost-gotoh nil "abc") (error (list (car e) (cdr e))))
 (completion--flex-cost-gotoh "fB" "fooBar"))
(innermost-minibuffer-p
 (innermost-minibuffer-p)
 (with-temp-buffer (innermost-minibuffer-p (current-buffer)))
 (condition-case e (innermost-minibuffer-p 7) (error (list (car e) (cdr e)))))
(internal-complete-buffer
 (internal-complete-buffer "*scr" nil nil)
 (let ((b (get-buffer-create "minibuf-1-complete"))) (unwind-protect (internal-complete-buffer "minibuf-1-c" nil t) (kill-buffer b)))
 (internal-complete-buffer "*scratch*" nil t)
 (condition-case e (internal-complete-buffer nil nil nil) (error (list (car e) (cdr e))))
 (internal-complete-buffer "minibuf-1" (lambda (name) (string-match-p "complete" name)) t))
(minibuffer-contents-no-properties
 (minibuffer-contents-no-properties)
 (with-temp-buffer (insert (propertize "hello" 'face 'bold)) (let ((s (minibuffer-contents-no-properties))) (list s (text-properties-at 1 s)))))
(minibuffer-innermost-command-loop-p
 (minibuffer-innermost-command-loop-p)
 (with-temp-buffer (minibuffer-innermost-command-loop-p (current-buffer)))
 (condition-case e (minibuffer-innermost-command-loop-p 7) (error (list (car e) (cdr e)))))
(read-variable
 (condition-case e (read-variable nil) (error (list (car e) (cdr e))))
 (condition-case e (read-variable 3) (error (list (car e) (cdr e))))
 (condition-case e (read-variable 3 "fill-column") (error (list (car e) (cdr e)))))
(set-minibuffer-window
 (condition-case e (set-minibuffer-window nil) (error (list (car e) (cdr e))))
 (condition-case e (set-minibuffer-window (selected-window)) (error (list (car e) (cdr e))))
 (condition-case e (set-minibuffer-window (selected-window)) (error (list (car e) (cadr e)))))
