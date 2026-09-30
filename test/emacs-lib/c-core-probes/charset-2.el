(get-unused-iso-final-char
 (get-unused-iso-final-char 1 94)
 (condition-case e (get-unused-iso-final-char 1 95) (error e)))
(iso-charset
 (iso-charset 1 94 48)
 (condition-case e (iso-charset 1 95 48) (error e)))
(map-charset-chars
 (let (out) (map-charset-chars (lambda (range extra) (push (list range extra) out)) 'ascii 'tag 65 67) (nreverse out))
 (condition-case e (map-charset-chars 1 'ascii) (error e)))
(set-charset-plist
 (progn (set-charset-plist 'ascii '(:probe 7)) (get 'ascii :probe))
 (condition-case e (set-charset-plist 'not-a-charset nil) (error e)))
(set-charset-priority
 (progn (set-charset-priority 'unicode 'ascii) (car (sort-charsets (list 'unicode 'ascii))))
 (condition-case e (set-charset-priority 'not-a-charset) (error e)))
(sort-charsets
 (sort-charsets (list 'unicode 'ascii))
 (condition-case e (sort-charsets 3) (error e)))
(split-char
 (split-char ?A)
 (condition-case e (split-char -1) (error e)))
(unify-charset
 (condition-case e (unify-charset 'ascii) (error e))
 (condition-case e (unify-charset 'not-a-charset) (error e)))
