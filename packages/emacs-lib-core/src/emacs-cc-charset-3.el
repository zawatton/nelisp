;;; emacs-cc-charset-3.el --- GNU charset.c port: DEFUNs -*- lexical-binding: t; -*-

;;; Code:

(defun charsetp (object)
  "Return non-nil if and only if OBJECT is a charset."
  (and (emacs-cc-charset--attrs-of object) t))

(defun emacs-cc-charset--map-scan (function table arg cs from to lo hi)
  "Port of map_char_table_for_charset by scanning LO..HI of TABLE.
As in chartab.c, one RANGE cons is passed to every call of FUNCTION; its
car is reset to nil after each call."
  (let ((c lo) (range (cons nil nil)))
    (while (<= c hi)
      (let ((v (aref table c)))
        (if (or (null v)
                (and cs (let ((code (emacs-cc-charset--encode cs c))) (or (< code from) (> code to)))))
            (when (car range)
              (setcdr range (1- c))
              (funcall function range arg)
              (setcar range nil))
          (unless (car range) (setcar range c))))
      (setq c (1+ c)))
    (when (car range)
      (setcdr range hi)
      (funcall function range arg))))

(defun emacs-cc-charset--bounds-of (table)
  (or (cdr (assq table emacs-cc-charset--bounds)) (cons 0 emacs-cc-charset--max-char)))

(defun emacs-cc-charset--map-chars (function arg cs from to)
  "Port of map_charset_chars."
  (let ((partial (or (> from (emacs-cc-charset--cs cs min-code))
                     (< to (emacs-cc-charset--cs cs max-code))))
        (method (emacs-cc-charset--cs cs method))
        (attrs (emacs-cc-charset--cs cs attrs)))
    (cond
     ((eq method 'offset)
      (let* ((from-idx (emacs-cc-charset--c2i cs from)) (to-idx (emacs-cc-charset--c2i cs to))
             (from-c (+ from-idx (emacs-cc-charset--cs cs offset)))
             (to-c (+ to-idx (emacs-cc-charset--cs cs offset))))
        (when (emacs-cc-charset--cs cs unified)
          (unless (char-table-p (emacs-cc-charset--at attrs deunifier))
            (emacs-cc-charset--load cs 2))
          (let ((tbl (emacs-cc-charset--at attrs deunifier)))
            (when (char-table-p tbl)
              (let ((b (emacs-cc-charset--bounds-of tbl)))
                (emacs-cc-charset--map-scan function tbl arg (and partial cs) from to (car b) (cdr b))))))
        (funcall function (cons from-c to-c) arg)))
     ((eq method 'map)
      (unless (char-table-p (emacs-cc-charset--at attrs encoder))
        (emacs-cc-charset--load cs 2))
      (let ((tbl (emacs-cc-charset--at attrs encoder)))
        (when (char-table-p tbl)
          (let ((b (emacs-cc-charset--bounds-of tbl)))
            (emacs-cc-charset--map-scan function tbl arg (and partial cs) from to (car b) (cdr b))))))
     ((eq method 'subset)
      (let* ((sub (emacs-cc-charset--at attrs subset))
             (pcs (emacs-cc-charset--from-id (aref sub 0))) (offset (aref sub 3)))
        (setq from (- from offset))
        (when (< from (aref sub 1)) (setq from (aref sub 1)))
        (setq to (- to offset))
        (when (> to (aref sub 2)) (setq to (aref sub 2)))
        (emacs-cc-charset--map-chars function arg pcs from to)))
     (t
      (dolist (p (emacs-cc-charset--at attrs superset))
        (let* ((pcs (emacs-cc-charset--from-id (car p))) (offset (cdr p))
               (tf (if (> from offset) (- from offset) 0))
               (tt (if (> to offset) (- to offset) 0)))
          (when (< tf (emacs-cc-charset--cs pcs min-code)) (setq tf (emacs-cc-charset--cs pcs min-code)))
          (when (> tt (emacs-cc-charset--cs pcs max-code)) (setq tt (emacs-cc-charset--cs pcs max-code)))
          (emacs-cc-charset--map-chars function arg pcs tf tt)))))))

(defun map-charset-chars (function charset &optional arg from-code to-code)
  "Call FUNCTION for all characters in CHARSET.
FUNCTION is called with two arguments: RANGE (FROM . TO) and ARG."
  (let* ((cs (emacs-cc-charset--check-cs charset)) from to)
    (if (null from-code) (setq from (emacs-cc-charset--cs cs min-code))
      (emacs-cc-charset--check-fixnat from-code)
      (setq from (max from-code (emacs-cc-charset--cs cs min-code))))
    (if (null to-code) (setq to (emacs-cc-charset--cs cs max-code))
      (emacs-cc-charset--check-fixnat to-code)
      (setq to (min to-code (emacs-cc-charset--cs cs max-code))))
    (emacs-cc-charset--map-chars function arg cs from to)
    nil))

(defun define-charset-alias (alias charset)
  "Define ALIAS as an alias for charset CHARSET."
  (let ((attr (emacs-cc-charset--check-attrs charset)))
    (puthash alias attr emacs-cc-charset--hash)
    (setq charset-list (cons alias charset-list))
    nil))

(defun charset-plist (charset)
  "Return the property list of CHARSET."
  (aref (emacs-cc-charset--check-attrs charset) 2))

(defun set-charset-plist (charset plist)
  "Set CHARSET's property list to PLIST."
  (aset (emacs-cc-charset--check-attrs charset) 2 plist)
  plist)

(defun unify-charset (charset &optional unify-map deunify)
  "Unify characters of CHARSET with Unicode."
  (let* ((cs (emacs-cc-charset--check-cs charset))
         (attrs (emacs-cc-charset--cs cs attrs)))
    (if (if (null deunify)
            (and (emacs-cc-charset--cs cs unified) (emacs-cc-charset--at attrs deunifier))
          (not (emacs-cc-charset--cs cs unified)))
        nil
      (emacs-cc-charset--cs-set cs unified nil)
      (if (null deunify)
          (progn
            (when (or (not (eq (emacs-cc-charset--cs cs method) 'offset))
                      (< (emacs-cc-charset--cs cs offset) #x110000))
              (error "Can't unify charset: %s" (symbol-name charset)))
            (if (null unify-map)
                (setq unify-map (emacs-cc-charset--at attrs unify-map))
              (unless (or (stringp unify-map) (vectorp unify-map))
                (signal 'error (list "Bad unify-map" unify-map)))
              (emacs-cc-charset--at-set attrs unify-map unify-map))
            (unless emacs-cc-charset--unify-table
              (setq emacs-cc-charset--unify-table (make-char-table nil)))
            (set-char-table-range emacs-cc-charset--unify-table
                                  (cons (emacs-cc-charset--cs cs min-char) (emacs-cc-charset--cs cs max-char))
                                  charset)
            (emacs-cc-charset--cs-set cs unified t))
        (when emacs-cc-charset--unify-table
          (let ((minc (emacs-cc-charset--decode cs (emacs-cc-charset--cs cs min-code)))
                (maxc (emacs-cc-charset--decode cs (emacs-cc-charset--cs cs max-code))))
            (set-char-table-range emacs-cc-charset--unify-table (cons minc maxc) nil))))
      nil)))

(defun emacs-cc-charset--check-iso (dimension chars final-char)
  "Return 1 for a 96-character set, 0 for 94."
  (unless (integerp dimension) (signal 'wrong-type-argument (list 'fixnump dimension)))
  (unless (integerp chars) (signal 'wrong-type-argument (list 'fixnump chars)))
  (emacs-cc-charset--check-character final-char)
  (unless (<= 1 dimension 3) (error "Invalid DIMENSION %d, it should be 1, 2, or 3" dimension))
  (unless (or (= chars 96) (= chars 94)) (error "Invalid CHARS %d, it should be 94 or 96" chars))
  (unless (and (<= ?0 final-char) (<= final-char ?~))
    (error "Invalid FINAL-CHAR `%c', it should be `0'..`~'" final-char))
  (if (= chars 96) 1 0))

(defun get-unused-iso-final-char (dimension chars)
  "Return an unused ISO final char for a charset of DIMENSION and CHARS."
  (let ((flag (emacs-cc-charset--check-iso dimension chars ?0)) (final ?0) r)
    (while (and (<= final ??) (not r))
      (when (< (aref emacs-cc-charset--iso (emacs-cc-charset--iso-index dimension flag final)) 0)
        (setq r final))
      (setq final (1+ final)))
    r))

(defun declare-equiv-charset (dimension chars final-char charset)
  "Declare an equivalent charset for ISO-2022 decoding."
  (let* ((id (emacs-cc-charset--check-id charset))
         (flag (emacs-cc-charset--check-iso dimension chars final-char)))
    (aset emacs-cc-charset--iso (emacs-cc-charset--iso-index dimension flag final-char) id)
    nil))

(defun emacs-cc-charset--translate (table c)
  (if (char-table-p table)
      (let ((ch (aref table c))) (if (characterp ch) ch c))
    c))

(defun emacs-cc-charset--char-charset (c &optional clist)
  "Port of char_charset.  Return a charset struct or nil."
  (let* ((maybe-null (and clist t))
         (lst (or clist emacs-cc-charset--ordered)) (found nil))
    (while (and (consp lst) (not found))
      (let* ((cs (emacs-cc-charset--from-id (car lst)))
             (code (emacs-cc-charset--encode cs c)))
        (if (/= code (emacs-cc-charset--cs cs invalid))
            (setq found cs)
          (setq lst (cdr lst))
          (when (and (not maybe-null) (<= c emacs-cc-charset--max-unicode)
                     (eq lst emacs-cc-charset--non-preferred-head))
            (setq found (emacs-cc-charset--from-id emacs-cc-charset--id-unicode))))))
    (or found
        (and (not maybe-null)
             (if (<= c emacs-cc-charset--max-5-byte)
                 (emacs-cc-charset--from-id emacs-cc-charset--id-emacs)
               (emacs-cc-charset--from-id emacs-cc-charset--id-eight-bit))))))

(defun emacs-cc-charset--CHAR-CHARSET (c)
  (if (< c #x80) (emacs-cc-charset--from-id emacs-cc-charset--id-ascii)
    (emacs-cc-charset--char-charset c)))

(defun emacs-cc-charset--find-in-text (chars-list unibyte-p table found)
  "Mark in FOUND (a vector) the charsets of CHARS-LIST."
  (let ((all-ascii t))
    (dolist (c chars-list) (when (>= c 128) (setq all-ascii nil)))
    (cond
     ((and all-ascii (not unibyte-p))
      (aset found emacs-cc-charset--id-ascii t))
     (unibyte-p
      (dolist (c chars-list)
        (setq c (emacs-cc-charset--translate table c))
        (aset found (if (< c 128) emacs-cc-charset--id-ascii emacs-cc-charset--id-eight-bit) t)))
     (t
      (dolist (c chars-list)
        (setq c (emacs-cc-charset--translate table c))
        (aset found (emacs-cc-charset--cs (emacs-cc-charset--CHAR-CHARSET c) id) t))))))

(defun emacs-cc-charset--found-list (found)
  (let ((val nil) (i (1- emacs-cc-charset--used)))
    (while (>= i 0)
      (when (aref found i) (push (emacs-cc-charset--name (emacs-cc-charset--from-id i)) val))
      (setq i (1- i)))
    val))

(defun find-charset-region (beg end &optional table)
  "Return a list of charsets in the region between BEG and END."
  (let ((b (if (markerp beg) (marker-position beg) beg))
        (e (if (markerp end) (marker-position end) end)))
    (unless (integerp b) (signal 'wrong-type-argument (list 'integer-or-marker-p beg)))
    (unless (integerp e) (signal 'wrong-type-argument (list 'integer-or-marker-p end)))
    (when (> b e) (let ((tmp b)) (setq b e e tmp)))
    (unless (and (<= (point-min) b) (<= e (point-max)))
      (signal 'args-out-of-range (list beg end)))
    (let ((found (make-vector emacs-cc-charset--used nil)) (chars nil) (p b))
      (while (< p e) (push (char-after p) chars) (setq p (1+ p)))
      (emacs-cc-charset--find-in-text (nreverse chars) (not enable-multibyte-characters) table found)
      (emacs-cc-charset--found-list found))))

(defun find-charset-string (str &optional table)
  "Return a list of charsets in STR."
  (unless (stringp str) (signal 'wrong-type-argument (list 'stringp str)))
  (let ((found (make-vector emacs-cc-charset--used nil)))
    (emacs-cc-charset--find-in-text (append str nil) (not (multibyte-string-p str)) table found)
    (emacs-cc-charset--found-list found)))

(defun decode-char (charset code-point)
  "Decode the pair of CHARSET and CODE-POINT into a character."
  (let* ((id (emacs-cc-charset--check-id charset))
         (code (emacs-cc-charset--cons-to-unsigned code-point))
         (c (emacs-cc-charset--decode (emacs-cc-charset--from-id id) code)))
    (if (>= c 0) c nil)))

(defun encode-char (ch charset)
  "Encode the character CH into a code-point of CHARSET."
  (let ((id (emacs-cc-charset--check-id charset)))
    (emacs-cc-charset--check-character ch)
    (let* ((cs (emacs-cc-charset--from-id id)) (code (emacs-cc-charset--encode cs ch)))
      (if (= code (emacs-cc-charset--cs cs invalid)) nil code))))

(defun make-char (charset &optional code1 code2 code3 code4)
  "Return a character of CHARSET whose position codes are CODEn."
  (let* ((cs (emacs-cc-charset--check-cs charset))
         (dimension (emacs-cc-charset--cs cs dimension))
         (sp (emacs-cc-charset--cs cs code-space)) code)
    (if (null code1)
        (setq code (if (emacs-cc-charset--cs cs ascii-compat) 0 (emacs-cc-charset--cs cs min-code)))
      (setq code (emacs-cc-charset--make-char-byte code1))
      (when (> dimension 1)
        (setq code (ash code 8))
        (setq code (logior code (if code2 (emacs-cc-charset--make-char-byte code2) (aref sp (* (- dimension 2) 4)))))
        (when (> dimension 2)
          (setq code (ash code 8))
          (setq code (logior code (if code3 (emacs-cc-charset--make-char-byte code3) (aref sp (* (- dimension 3) 4)))))
          (when (> dimension 3)
            (setq code (ash code 8))
            (setq code (logior code (if code4 (emacs-cc-charset--make-char-byte code4) (aref sp 0))))))))
    (when (>= (emacs-cc-charset--cs cs iso-final) 0)
      (setq code (logand code #x7F7F7F7F)))
    (let ((c (emacs-cc-charset--decode cs code)))
      (when (< c 0) (error "Invalid code(s)"))
      c)))

(defun emacs-cc-charset--make-char-byte (x)
  (emacs-cc-charset--check-fixnat x)
  (when (>= x #x100) (signal 'args-out-of-range (list #xFF x)))
  x)

(defun split-char (ch)
  "Return list of charset and one to four position-codes of CH."
  (emacs-cc-charset--check-character ch)
  (let* ((cs (emacs-cc-charset--CHAR-CHARSET ch))
         (code (emacs-cc-charset--encode cs ch))
         (dimension (emacs-cc-charset--cs cs dimension)) (val nil))
    (while (> dimension 0)
      (push (logand code 255) val)
      (setq code (ash code -8) dimension (1- dimension)))
    (cons (emacs-cc-charset--name cs) val)))

(defun char-charset (ch &optional restriction)
  "Return the charset of highest priority that contains CH."
  (emacs-cc-charset--check-character ch)
  (if (null restriction)
      (emacs-cc-charset--name (emacs-cc-charset--CHAR-CHARSET ch))
    (if (consp restriction)
        (let ((found nil))
          (while (and (consp restriction) (not found))
            (let ((rcs (emacs-cc-charset--check-cs (car restriction))))
              (if (/= (emacs-cc-charset--encode rcs ch) (emacs-cc-charset--cs rcs invalid))
                  (setq found (car restriction))
                (setq restriction (cdr restriction)))))
          found)
      (let* ((lst (emacs-cc-charset--coding-system-charset-list restriction))
             (cs (emacs-cc-charset--char-charset ch (mapcar (lambda (n) (emacs-cc-charset--check-id n)) lst))))
        (and cs (emacs-cc-charset--name cs))))))

(defun emacs-cc-charset--coding-system-charset-list (coding-system)
  "Port of coding_system_charset_list (needs the coding.c registry)."
  (unless (fboundp 'coding-system-get)
    (signal 'wrong-type-argument (list 'coding-system-p coding-system)))
  (let ((type (coding-system-get coding-system :coding-type)))
    (cond
     ((eq type 'iso-2022)
      (if (memq 'full-support (coding-system-get coding-system :flags))
          (mapcar (lambda (id) (emacs-cc-charset--name (emacs-cc-charset--from-id id)))
                  emacs-cc-charset--iso-2022-list)
        (coding-system-get coding-system :charset-list)))
     ((eq type 'emacs-mule)
      (mapcar (lambda (id) (emacs-cc-charset--name (emacs-cc-charset--from-id id)))
              emacs-cc-charset--mule-list))
     (t (coding-system-get coding-system :charset-list)))))

(defun charset-after (&optional pos)
  "Return charset of a character in the current buffer at position POS."
  (let ((ch (char-after pos)))
    (if (integerp ch)
        (emacs-cc-charset--name (emacs-cc-charset--CHAR-CHARSET ch))
      ch)))

(defun iso-charset (dimension chars final-char)
  "Return charset of ISO's specification DIMENSION, CHARS, and FINAL-CHAR."
  (let* ((flag (emacs-cc-charset--check-iso dimension chars final-char))
         (id (aref emacs-cc-charset--iso (emacs-cc-charset--iso-index dimension flag final-char))))
    (and (>= id 0) (emacs-cc-charset--name (emacs-cc-charset--from-id id)))))

(defun clear-charset-maps ()
  "Internal use only.  Clear temporary charset mapping tables."
  (setq emacs-cc-charset--work nil)
  nil)

(defun charset-priority-list (&optional highestp)
  "Return the list of charsets ordered by priority."
  (if highestp
      (emacs-cc-charset--name (emacs-cc-charset--from-id (car emacs-cc-charset--ordered)))
    (mapcar (lambda (id) (emacs-cc-charset--name (emacs-cc-charset--from-id id)))
            emacs-cc-charset--ordered)))

(defun set-charset-priority (&rest charsets)
  "Assign higher priority to the charsets given as arguments."
  (when (null charsets)
    (signal 'wrong-number-of-arguments (list 'set-charset-priority 0)))
  (let ((old-list (copy-sequence emacs-cc-charset--ordered)) (new-head nil) l2022 lmule)
    (dolist (c charsets)
      (let ((id (emacs-cc-charset--check-id c)))
        (when (memq id old-list)
          (setq old-list (delq id old-list))
          (setq new-head (cons id new-head)))))
    (setq emacs-cc-charset--non-preferred-head old-list)
    (setq emacs-cc-charset--ordered (nconc (nreverse new-head) old-list))
    (setq emacs-cc-charset--tick (1+ emacs-cc-charset--tick))
    (setq emacs-cc-charset--id-unibyte -1)
    (dolist (id emacs-cc-charset--ordered)
      (when (memq id emacs-cc-charset--iso-2022-list) (push id l2022))
      (when (memq id emacs-cc-charset--mule-list) (push id lmule))
      (when (< emacs-cc-charset--id-unibyte 0)
        (let ((cs (emacs-cc-charset--from-id id)))
          (when (and (= (emacs-cc-charset--cs cs dimension) 1)
                     (emacs-cc-charset--cs cs ascii-compat)
                     (progn (emacs-cc-charset--ensure-ranges cs)
                            (>= (emacs-cc-charset--cs cs max-char) #x80)))
            (setq emacs-cc-charset--id-unibyte id)))))
    (setq emacs-cc-charset--iso-2022-list (nreverse l2022))
    (setq emacs-cc-charset--mule-list (nreverse lmule))
    (when (< emacs-cc-charset--id-unibyte 0)
      (setq emacs-cc-charset--id-unibyte emacs-cc-charset--id-iso-8859-1))
    nil))

(defun charset-id-internal (&optional charset)
  "Internal use only.  Return charset identification number of CHARSET."
  (emacs-cc-charset--check-id charset))

(defun sort-charsets (charsets)
  "Sort charset list CHARSETS by a priority of each charset."
  (let ((n (length charsets)))
    (if (= n 0) nil
      (let ((data nil))
        (dolist (elt charsets)
          (let ((attrs (emacs-cc-charset--check-attrs elt)))
            (push (list elt (aref attrs 0) 0) data)))
        (setq data (nreverse data))
        (let ((i 0))
          (dolist (id emacs-cc-charset--ordered)
            (dolist (d data) (when (= (nth 1 d) id) (setcar (nthcdr 2 d) i)))
            (setq i (1+ i))))
        (setq data (sort data (lambda (a b) (< (nth 2 a) (nth 2 b)))))
        (let ((tail charsets))
          (dolist (d data) (setcar tail (car d)) (setq tail (cdr tail))))
        charsets))))

;;; init_charset / syms_of_charset

(let ((dir (and (boundp 'data-directory) (stringp data-directory)
                (expand-file-name "charsets" data-directory))))
  (setq charset-map-path
        (list (if (and dir (file-directory-p dir)) dir
                (expand-file-name "emacs-etc/charsets/"
                                  (if (and (boundp 'nelisp-emacs-vendor-root) nelisp-emacs-vendor-root)
                                      nelisp-emacs-vendor-root
                                    (expand-file-name "vendor/")))))))

(setq emacs-cc-charset--table (make-vector 190 nil) emacs-cc-charset--used 0
      emacs-cc-charset--hash (make-hash-table :test 'eq)
      emacs-cc-charset--iso (make-vector 768 -1) emacs-cc-charset--mule (make-vector 256 -1)
      emacs-cc-charset--ordered nil emacs-cc-charset--non-preferred-head nil
      emacs-cc-charset--iso-2022-list nil emacs-cc-charset--mule-list nil
      emacs-cc-charset--unify-table nil emacs-cc-charset--bounds nil
      emacs-cc-charset--work nil charset-list nil)

(setq emacs-cc-charset--id-ascii
      (emacs-cc-charset--define-builtin 'ascii 1 '(0 127 0 0 0 0 0 0) 0 127 ?B -1 0 t nil 0)
      emacs-cc-charset--id-iso-8859-1
      (emacs-cc-charset--define-builtin 'iso-8859-1 1 '(0 255 0 0 0 0 0 0) 0 255 -1 -1 -1 t nil 0)
      emacs-cc-charset--id-unicode
      (emacs-cc-charset--define-builtin 'unicode 3 '(0 255 0 255 0 16 0 0) 0 #x10FFFF -1 0 -1 t nil 0)
      emacs-cc-charset--id-emacs
      (emacs-cc-charset--define-builtin 'emacs 3 '(0 255 0 255 0 63 0 0) 0 #x3FFF7F -1 0 -1 t t 0)
      emacs-cc-charset--id-eight-bit
      (emacs-cc-charset--define-builtin 'eight-bit 1 '(128 255 0 0 0 0 0 0) 128 255 -1 0 -1 nil t #x3FFF80)
      emacs-cc-charset--id-unibyte emacs-cc-charset--id-iso-8859-1)

(provide 'emacs-cc-charset-3)
