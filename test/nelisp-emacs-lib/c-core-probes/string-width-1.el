;;; string-width-1.el --- supplementary string display width probes -*- lexical-binding: t; -*-

(string-width
 (with-temp-buffer (string-width "abc"))
 (with-temp-buffer
   (let ((tab-width 4)) (list (string-width "a\tb") (string-width "abcdef" 1 4))))
 (condition-case e (string-width 17) (error e))
 (string-width "abcdef" 1 4)
 (string-width "abcdef" -3 -1)
 (condition-case e (string-width "abc" 3 1) (error e))
 (condition-case e (string-width) (error e))
 (condition-case e (string-width "abc" 0 1 2) (error e))
 (string-width "日")
 (string-width "é")
 (with-temp-buffer
   (let ((table (make-display-table)))
     (aset table ?x (vector ?z ?日))
     (setq buffer-display-table table)
     (list (string-width "x") (eq buffer-display-table table))))
 (with-temp-buffer
   (let ((buffer-display-table nil))
     (let ((before buffer-display-table))
       (list (string-width "abc") (eq before buffer-display-table))))))
