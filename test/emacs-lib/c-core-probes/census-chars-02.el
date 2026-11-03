;;; census-chars-02.el --- canonical probes  -*- lexical-binding: t; -*-

(make-char-table
 (let ((table (make-char-table 'probe 7)))
   (list (char-table-p table) (char-table-subtype table) (aref table 65)))
 (let ((table (make-char-table 'probe))) (aref table 0))
 (condition-case e (make-char-table 17) (error e)))

(map-char-table
 (let ((table (make-char-table 'probe)) (rows nil))
   (set-char-table-range table '(65 . 66) 'letters)
   (map-char-table (lambda (key value)
                     (push (list (if (consp key) (cons (car key) (cdr key)) key)
                                 value) rows)) table)
   (nreverse rows))
 (let ((table (make-char-table 'probe)) (count 0))
   (map-char-table (lambda (key value) (setq count (1+ count))) table)
   count)
 (condition-case e (map-char-table #'ignore 17) (error e)))

(max-char
 (max-char)
 (max-char t)
 (condition-case e (max-char nil nil) (error (car e))))

(multibyte-char-to-unibyte
 (multibyte-char-to-unibyte 65)
 (multibyte-char-to-unibyte #x100)
 (condition-case e (multibyte-char-to-unibyte 'bad) (error e)))

(multibyte-string-p
 (multibyte-string-p "é")
 (multibyte-string-p (unibyte-string 65 255))
 (multibyte-string-p 17))

(number-to-string
 (number-to-string -12345)
 (number-to-string 1.5)
 (condition-case e (number-to-string 'bad) (error e)))

(propertize
 (let ((s (propertize "abc" 'probe 7)))
   (list (substring-no-properties s) (get-text-property 1 'probe s)))
 (propertize "" 'probe t)
 (condition-case e (propertize 17 'probe t) (error e)))

(set-char-table-extra-slot
 (let* ((purpose (make-symbol "probe"))
        (unused (put purpose 'char-table-extra-slots 2))
        (table (make-char-table purpose)))
   (list (set-char-table-extra-slot table 0 'first)
         (char-table-extra-slot table 0)))
 (let* ((purpose (make-symbol "probe"))
        (unused (put purpose 'char-table-extra-slots 2))
        (table (make-char-table purpose)))
   (set-char-table-extra-slot table 1 '(last))
   (char-table-extra-slot table 1))
 (condition-case e (set-char-table-extra-slot 17 0 nil) (error e)))

(set-char-table-parent
 (let ((table (make-char-table 'probe)) (parent (make-char-table 'probe 9)))
   (list (eq (set-char-table-parent table parent) parent) (aref table 65)))
 (let ((table (make-char-table 'probe)))
   (list (set-char-table-parent table nil) (char-table-parent table)))
 (condition-case e (set-char-table-parent 17 nil) (error e)))

(set-char-table-range
 (let ((table (make-char-table 'probe)))
   (list (set-char-table-range table '(65 . 67) 'letters)
         (aref table 64) (aref table 65) (aref table 67) (aref table 68)))
 (let ((table (make-char-table 'probe)))
   (set-char-table-range table t 7)
   (list (aref table 0) (aref table (max-char))))
 (condition-case e (set-char-table-range 17 65 t) (error e)))

(string-bytes
 (string-bytes "abc")
 (list (string-bytes "é") (string-bytes (unibyte-string 255)))
 (condition-case e (string-bytes 17) (error e)))

(string-collate-equalp
 (string-collate-equalp "abc" "abc" "C")
 (string-collate-equalp "abc" "abd" "C")
 (condition-case e (string-collate-equalp 17 "abc" "C") (error e)))

(string-collate-lessp
 (string-collate-lessp "abc" "abd" "C")
 (string-collate-lessp "abc" "abc" "C")
 (condition-case e (string-collate-lessp 17 "abc" "C") (error e)))

(string-distance
 (string-distance "kitten" "sitting")
 (list (string-distance "é" "e") (string-distance "é" "e" t))
 (condition-case e (string-distance 17 "abc") (error e)))

(string-equal
 (string-equal "abc" "abc")
 (list (string-equal 'abc "abc") (string-equal "abc" "ABC"))
 (condition-case e (string-equal 17 "abc") (error e)))

(string-lessp
 (string-lessp "abc" "abd")
 (list (string-lessp "abc" "abc") (string-lessp 'ab "abc"))
 (condition-case e (string-lessp 17 "abc") (error e)))

(string-make-multibyte
 (let ((s (string-make-multibyte (unibyte-string 65 255))))
   (list (multibyte-string-p s) (length s) (aref s 0) (aref s 1)))
 (string-make-multibyte "")
 (condition-case e (string-make-multibyte 17) (error e)))

(string-search
 (string-search "bc" "abcabc")
 (list (string-search "bc" "abcabc" 2) (string-search "" "abc" 3)
       (string-search "z" "abc"))
 (condition-case e (string-search "a" "abc" -1) (error e)))

(string-to-char
 (string-to-char "abc")
 (list (string-to-char "") (string-to-char "é"))
 (condition-case e (string-to-char 17) (error e)))

(string-to-number
 (string-to-number " -12.5tail")
 (list (string-to-number "ff" 16) (string-to-number "nonsense"))
 (condition-case e (string-to-number 17) (error e)))

(string-version-lessp
 (string-version-lessp "v2" "v10")
 (list (string-version-lessp "v10" "v2") (string-version-lessp "v2" "v2"))
 (condition-case e (string-version-lessp 17 "v2") (error e)))

(string-width
 (with-temp-buffer (string-width "abc"))
 (with-temp-buffer
   (let ((tab-width 4)) (list (string-width "a\tb") (string-width "abcdef" 1 4))))
 (condition-case e (string-width 17) (error e)))

(unibyte-char-to-multibyte
 (unibyte-char-to-multibyte 65)
 (unibyte-char-to-multibyte 255)
 (condition-case e (unibyte-char-to-multibyte 'bad) (error e)))

(unibyte-string
 (let ((s (unibyte-string 65 255)))
   (list (multibyte-string-p s) (length s) (aref s 0) (aref s 1)))
 (unibyte-string)
 (condition-case e (unibyte-string 256) (error e)))
