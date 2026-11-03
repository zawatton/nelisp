;;; GNU comparison: escape values, consumed length, string representation and errors.
(read-from-string
 (list 'char "?\\0"
       (condition-case err
           (let* ((r (read-from-string "?\\0")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\0\""
       (condition-case err
           (let* ((r (read-from-string "\"\\0\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\00"
       (condition-case err
           (let* ((r (read-from-string "?\\00")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\00\""
       (condition-case err
           (let* ((r (read-from-string "\"\\00\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\000"
       (condition-case err
           (let* ((r (read-from-string "?\\000")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\000\""
       (condition-case err
           (let* ((r (read-from-string "\"\\000\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\1"
       (condition-case err
           (let* ((r (read-from-string "?\\1")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\1\""
       (condition-case err
           (let* ((r (read-from-string "\"\\1\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\12"
       (condition-case err
           (let* ((r (read-from-string "?\\12")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\12\""
       (condition-case err
           (let* ((r (read-from-string "\"\\12\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\040"
       (condition-case err
           (let* ((r (read-from-string "?\\040")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\040\""
       (condition-case err
           (let* ((r (read-from-string "\"\\040\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\377"
       (condition-case err
           (let* ((r (read-from-string "?\\377")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\377\""
       (condition-case err
           (let* ((r (read-from-string "\"\\377\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\400"
       (condition-case err
           (let* ((r (read-from-string "?\\400")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\400\""
       (condition-case err
           (let* ((r (read-from-string "\"\\400\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\777"
       (condition-case err
           (let* ((r (read-from-string "?\\777")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\777\""
       (condition-case err
           (let* ((r (read-from-string "\"\\777\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\0400"
       (condition-case err
           (let* ((r (read-from-string "?\\0400")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\0400\""
       (condition-case err
           (let* ((r (read-from-string "\"\\0400\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\8"
       (condition-case err
           (let* ((r (read-from-string "?\\8")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\8\""
       (condition-case err
           (let* ((r (read-from-string "\"\\8\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\x0"
       (condition-case err
           (let* ((r (read-from-string "?\\x0")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\x0\""
       (condition-case err
           (let* ((r (read-from-string "\"\\x0\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\x20"
       (condition-case err
           (let* ((r (read-from-string "?\\x20")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\x20\""
       (condition-case err
           (let* ((r (read-from-string "\"\\x20\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\xff"
       (condition-case err
           (let* ((r (read-from-string "?\\xff")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\xff\""
       (condition-case err
           (let* ((r (read-from-string "\"\\xff\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\x100"
       (condition-case err
           (let* ((r (read-from-string "?\\x100")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\x100\""
       (condition-case err
           (let* ((r (read-from-string "\"\\x100\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\x1f600"
       (condition-case err
           (let* ((r (read-from-string "?\\x1f600")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\x1f600\""
       (condition-case err
           (let* ((r (read-from-string "\"\\x1f600\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\u0041"
       (condition-case err
           (let* ((r (read-from-string "?\\u0041")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\u0041\""
       (condition-case err
           (let* ((r (read-from-string "\"\\u0041\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\u00ff"
       (condition-case err
           (let* ((r (read-from-string "?\\u00ff")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\u00ff\""
       (condition-case err
           (let* ((r (read-from-string "\"\\u00ff\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\u3042"
       (condition-case err
           (let* ((r (read-from-string "?\\u3042")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\u3042\""
       (condition-case err
           (let* ((r (read-from-string "\"\\u3042\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\U0001F600"
       (condition-case err
           (let* ((r (read-from-string "?\\U0001F600")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\U0001F600\""
       (condition-case err
           (let* ((r (read-from-string "\"\\U0001F600\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\N{U+0041}"
       (condition-case err
           (let* ((r (read-from-string "?\\N{U+0041}")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\N{U+0041}\""
       (condition-case err
           (let* ((r (read-from-string "\"\\N{U+0041}\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\N{U+3042}"
       (condition-case err
           (let* ((r (read-from-string "?\\N{U+3042}")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\N{U+3042}\""
       (condition-case err
           (let* ((r (read-from-string "\"\\N{U+3042}\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\^A"
       (condition-case err
           (let* ((r (read-from-string "?\\^A")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\^A\""
       (condition-case err
           (let* ((r (read-from-string "\"\\^A\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\^?"
       (condition-case err
           (let* ((r (read-from-string "?\\^?")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\^?\""
       (condition-case err
           (let* ((r (read-from-string "\"\\^?\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\C-a"
       (condition-case err
           (let* ((r (read-from-string "?\\C-a")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\C-a\""
       (condition-case err
           (let* ((r (read-from-string "\"\\C-a\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\C- "
       (condition-case err
           (let* ((r (read-from-string "?\\C- ")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\C- \""
       (condition-case err
           (let* ((r (read-from-string "\"\\C- \"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\M-a"
       (condition-case err
           (let* ((r (read-from-string "?\\M-a")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\M-a\""
       (condition-case err
           (let* ((r (read-from-string "\"\\M-a\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\S-a"
       (condition-case err
           (let* ((r (read-from-string "?\\S-a")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\S-a\""
       (condition-case err
           (let* ((r (read-from-string "\"\\S-a\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\H-a"
       (condition-case err
           (let* ((r (read-from-string "?\\H-a")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\H-a\""
       (condition-case err
           (let* ((r (read-from-string "\"\\H-a\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\s-a"
       (condition-case err
           (let* ((r (read-from-string "?\\s-a")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\s-a\""
       (condition-case err
           (let* ((r (read-from-string "\"\\s-a\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\A-a"
       (condition-case err
           (let* ((r (read-from-string "?\\A-a")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\A-a\""
       (condition-case err
           (let* ((r (read-from-string "\"\\A-a\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\d"
       (condition-case err
           (let* ((r (read-from-string "?\\d")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\d\""
       (condition-case err
           (let* ((r (read-from-string "\"\\d\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\e"
       (condition-case err
           (let* ((r (read-from-string "?\\e")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\e\""
       (condition-case err
           (let* ((r (read-from-string "\"\\e\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\s"
       (condition-case err
           (let* ((r (read-from-string "?\\s")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\s\""
       (condition-case err
           (let* ((r (read-from-string "\"\\s\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\a"
       (condition-case err
           (let* ((r (read-from-string "?\\a")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\a\""
       (condition-case err
           (let* ((r (read-from-string "\"\\a\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\b"
       (condition-case err
           (let* ((r (read-from-string "?\\b")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\b\""
       (condition-case err
           (let* ((r (read-from-string "\"\\b\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\f"
       (condition-case err
           (let* ((r (read-from-string "?\\f")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\f\""
       (condition-case err
           (let* ((r (read-from-string "\"\\f\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\n"
       (condition-case err
           (let* ((r (read-from-string "?\\n")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\n\""
       (condition-case err
           (let* ((r (read-from-string "\"\\n\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\r"
       (condition-case err
           (let* ((r (read-from-string "?\\r")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\r\""
       (condition-case err
           (let* ((r (read-from-string "\"\\r\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\t"
       (condition-case err
           (let* ((r (read-from-string "?\\t")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\t\""
       (condition-case err
           (let* ((r (read-from-string "\"\\t\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\v"
       (condition-case err
           (let* ((r (read-from-string "?\\v")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\v\""
       (condition-case err
           (let* ((r (read-from-string "\"\\v\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\C-\\M-a"
       (condition-case err
           (let* ((r (read-from-string "?\\C-\\M-a")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\C-\\M-a\""
       (condition-case err
           (let* ((r (read-from-string "\"\\C-\\M-a\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\M-\\C-a"
       (condition-case err
           (let* ((r (read-from-string "?\\M-\\C-a")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\M-\\C-a\""
       (condition-case err
           (let* ((r (read-from-string "\"\\M-\\C-a\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\C-\\040"
       (condition-case err
           (let* ((r (read-from-string "?\\C-\\040")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\C-\\040\""
       (condition-case err
           (let* ((r (read-from-string "\"\\C-\\040\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\M-\\040"
       (condition-case err
           (let* ((r (read-from-string "?\\M-\\040")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\M-\\040\""
       (condition-case err
           (let* ((r (read-from-string "\"\\M-\\040\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\S-\\040"
       (condition-case err
           (let* ((r (read-from-string "?\\S-\\040")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\S-\\040\""
       (condition-case err
           (let* ((r (read-from-string "\"\\S-\\040\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\C-\\u0041"
       (condition-case err
           (let* ((r (read-from-string "?\\C-\\u0041")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\C-\\u0041\""
       (condition-case err
           (let* ((r (read-from-string "\"\\C-\\u0041\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\M-\\u3042"
       (condition-case err
           (let* ((r (read-from-string "?\\M-\\u3042")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\M-\\u3042\""
       (condition-case err
           (let* ((r (read-from-string "\"\\M-\\u3042\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\x"
       (condition-case err
           (let* ((r (read-from-string "?\\x")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\x\""
       (condition-case err
           (let* ((r (read-from-string "\"\\x\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\u123"
       (condition-case err
           (let* ((r (read-from-string "?\\u123")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\u123\""
       (condition-case err
           (let* ((r (read-from-string "\"\\u123\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\U00110000"
       (condition-case err
           (let* ((r (read-from-string "?\\U00110000")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\U00110000\""
       (condition-case err
           (let* ((r (read-from-string "\"\\U00110000\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\N{U+XYZ}"
       (condition-case err
           (let* ((r (read-from-string "?\\N{U+XYZ}")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\N{U+XYZ}\""
       (condition-case err
           (let* ((r (read-from-string "\"\\N{U+XYZ}\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\C-"
       (condition-case err
           (let* ((r (read-from-string "?\\C-")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\C-\""
       (condition-case err
           (let* ((r (read-from-string "\"\\C-\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\H-\\M-a"
       (condition-case err
           (let* ((r (read-from-string "?\\H-\\M-a")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\H-\\M-a\""
       (condition-case err
           (let* ((r (read-from-string "\"\\H-\\M-a\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\S-\\C-a"
       (condition-case err
           (let* ((r (read-from-string "?\\S-\\C-a")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\S-\\C-a\""
       (condition-case err
           (let* ((r (read-from-string "\"\\S-\\C-a\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\x000ff"
       (condition-case err
           (let* ((r (read-from-string "?\\x000ff")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\x000ff\""
       (condition-case err
           (let* ((r (read-from-string "\"\\x000ff\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\u0000"
       (condition-case err
           (let* ((r (read-from-string "?\\u0000")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\u0000\""
       (condition-case err
           (let* ((r (read-from-string "\"\\u0000\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\U000000FF"
       (condition-case err
           (let* ((r (read-from-string "?\\U000000FF")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\U000000FF\""
       (condition-case err
           (let* ((r (read-from-string "\"\\U000000FF\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\N{U+00FF}"
       (condition-case err
           (let* ((r (read-from-string "?\\N{U+00FF}")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\N{U+00FF}\""
       (condition-case err
           (let* ((r (read-from-string "\"\\N{U+00FF}\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\C-\\C-a"
       (condition-case err
           (let* ((r (read-from-string "?\\C-\\C-a")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\C-\\C-a\""
       (condition-case err
           (let* ((r (read-from-string "\"\\C-\\C-a\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\C-\\M-\\040"
       (condition-case err
           (let* ((r (read-from-string "?\\C-\\M-\\040")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\C-\\M-\\040\""
       (condition-case err
           (let* ((r (read-from-string "\"\\C-\\M-\\040\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\M-\\C-\\040"
       (condition-case err
           (let* ((r (read-from-string "?\\M-\\C-\\040")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\M-\\C-\\040\""
       (condition-case err
           (let* ((r (read-from-string "\"\\M-\\C-\\040\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\S-A"
       (condition-case err
           (let* ((r (read-from-string "?\\S-A")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\S-A\""
       (condition-case err
           (let* ((r (read-from-string "\"\\S-A\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\M-\\S-a"
       (condition-case err
           (let* ((r (read-from-string "?\\M-\\S-a")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\M-\\S-a\""
       (condition-case err
           (let* ((r (read-from-string "\"\\M-\\S-a\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\S-\\M-a"
       (condition-case err
           (let* ((r (read-from-string "?\\S-\\M-a")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\S-\\M-a\""
       (condition-case err
           (let* ((r (read-from-string "\"\\S-\\M-a\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\C-\\M-\\S-a"
       (condition-case err
           (let* ((r (read-from-string "?\\C-\\M-\\S-a")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\C-\\M-\\S-a\""
       (condition-case err
           (let* ((r (read-from-string "\"\\C-\\M-\\S-a\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\C-\\["
       (condition-case err
           (let* ((r (read-from-string "?\\C-\\[")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\C-\\[\""
       (condition-case err
           (let* ((r (read-from-string "\"\\C-\\[\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\M-\\u00e9"
       (condition-case err
           (let* ((r (read-from-string "?\\M-\\u00e9")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\M-\\u00e9\""
       (condition-case err
           (let* ((r (read-from-string "\"\\M-\\u00e9\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\M-é"
       (condition-case err
           (let* ((r (read-from-string "?\\M-é")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\M-é\""
       (condition-case err
           (let* ((r (read-from-string "\"\\M-é\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\x110000"
       (condition-case err
           (let* ((r (read-from-string "?\\x110000")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\x110000\""
       (condition-case err
           (let* ((r (read-from-string "\"\\x110000\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\uD800"
       (condition-case err
           (let* ((r (read-from-string "?\\uD800")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\uD800\""
       (condition-case err
           (let* ((r (read-from-string "\"\\uD800\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\U0010FFFF"
       (condition-case err
           (let* ((r (read-from-string "?\\U0010FFFF")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\U0010FFFF\""
       (condition-case err
           (let* ((r (read-from-string "\"\\U0010FFFF\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\u12zz"
       (condition-case err
           (let* ((r (read-from-string "?\\u12zz")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\u12zz\""
       (condition-case err
           (let* ((r (read-from-string "\"\\u12zz\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\xg"
       (condition-case err
           (let* ((r (read-from-string "?\\xg")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\xg\""
       (condition-case err
           (let* ((r (read-from-string "\"\\xg\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\M"
       (condition-case err
           (let* ((r (read-from-string "?\\M")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\M\""
       (condition-case err
           (let* ((r (read-from-string "\"\\M\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'char "?\\C-xz"
       (condition-case err
           (let* ((r (read-from-string "?\\C-xz")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err)))))
 (list 'string "\"\\C-xz\""
       (condition-case err
           (let* ((r (read-from-string "\"\\C-xz\"")) (v (car r)))
             (list (if (stringp v) (string-to-list v) v) (cdr r)
                   (if (stringp v) (multibyte-string-p v) nil)))
         (error (list 'error (car err))))))
