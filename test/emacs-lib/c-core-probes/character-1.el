;;; character-1.el --- character.c primitive probes -*- lexical-binding: t; -*-

(char-resolve-modifiers
 (list (char-resolve-modifiers ?a) (char-resolve-modifiers ?\C-a)
       (char-resolve-modifiers ?\S-a) (char-resolve-modifiers ?\M-a))
 (condition-case e (char-resolve-modifiers "x") (error e)))

(get-byte
 (with-temp-buffer (insert "Aλ") (list (get-byte 1) (get-byte 2)
                                         (condition-case e (get-byte 3) (error e))))
 (list (get-byte 1 "A") (get-byte 0 (unibyte-string 233))
       (condition-case e (get-byte 0 "λ") (error e))
       (condition-case e (get-byte 1 "A") (error e))))
