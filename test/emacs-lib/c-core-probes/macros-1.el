(call-last-kbd-macro
 (condition-case e (call-last-kbd-macro) (error e))
 (let ((last-kbd-macro "a")) (condition-case e (call-last-kbd-macro 2) (error e)))
 (let ((last-kbd-macro "a")) (call-last-kbd-macro 3)))
(cancel-kbd-macro-events
 (cancel-kbd-macro-events)
 (let ((last-kbd-macro "abc")) (cancel-kbd-macro-events) last-kbd-macro))
(end-kbd-macro
 (condition-case e (end-kbd-macro) (error e))
 (progn (start-kbd-macro nil) (store-kbd-macro-event ?a) (end-kbd-macro) (stringp last-kbd-macro)))
(execute-kbd-macro
 (condition-case e (execute-kbd-macro 1) (error e))
 (execute-kbd-macro "abc" 2)
 (let ((n 0)) (execute-kbd-macro [97] 5 (lambda () (< (setq n (1+ n)) 3))) n)
 (let ((macro (make-vector 1 ?x))) (execute-kbd-macro macro) (length macro))
 (let ((w (split-window))) (unwind-protect (progn (execute-kbd-macro []) (list (windowp w) (windowp (selected-window)))) (delete-window w))))
(start-kbd-macro
 (progn (start-kbd-macro nil) (let ((v (bound-and-true-p defining-kbd-macro))) (end-kbd-macro) v))
 (let ((last-kbd-macro [?a])) (start-kbd-macro t t) (store-kbd-macro-event ?b) (end-kbd-macro) last-kbd-macro))
(store-kbd-macro-event
 (store-kbd-macro-event ?a)
 (let ((last-kbd-macro [])) (start-kbd-macro nil) (store-kbd-macro-event ?x) (store-kbd-macro-event ?y) (end-kbd-macro) last-kbd-macro))
