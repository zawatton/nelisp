;;; emacs-cc-charset-1.el --- GNU charset.c port: registry and maps -*- lexical-binding: t; -*-

;; Port of GNU Emacs 31.1 src/charset.c (charset table, map loading,
;; `define-charset-internal').  The DEFUNs live in emacs-cc-charset-2.el.

;;; Code:

(eval-and-compile
  (defconst emacs-cc-charset--slots
    '((id . 0) (dimension . 1) (code-space . 2) (linear . 3) (mask . 4)
      (chars-96 . 5) (min-code . 6) (max-code . 7) (index-offset . 8)
      (compact . 9) (invalid . 10) (iso-final . 11) (iso-rev . 12)
      (mule-id . 13) (ascii-compat . 14) (suppl . 15) (unified . 16)
      (fast-map . 17) (method . 18) (offset . 19) (min-char . 20)
      (max-char . 21) (attrs . 22) (ready . 23))
    "Slot indices of the charset struct vector.")
  (defconst emacs-cc-charset--attrs
    '((id . 0) (name . 1) (plist . 2) (map . 3) (decoder . 4) (encoder . 5)
      (subset . 6) (superset . 7) (unify-map . 8) (deunifier . 9))
    "Slot indices of the charset attribute vector."))

(defmacro emacs-cc-charset--cs (cs slot)
  (list 'aref cs (cdr (assq slot emacs-cc-charset--slots))))
(defmacro emacs-cc-charset--cs-set (cs slot val)
  (list 'aset cs (cdr (assq slot emacs-cc-charset--slots)) val))
(defmacro emacs-cc-charset--at (attrs slot)
  (list 'aref attrs (cdr (assq slot emacs-cc-charset--attrs))))
(defmacro emacs-cc-charset--at-set (attrs slot val)
  (list 'aset attrs (cdr (assq slot emacs-cc-charset--attrs)) val))

(defconst emacs-cc-charset--max-char #x3FFFFF)
(defconst emacs-cc-charset--max-unicode #x10FFFF)
(defconst emacs-cc-charset--max-5-byte #x3FFF7F)

(defvar emacs-cc-charset--table (make-vector 190 nil))
(defvar emacs-cc-charset--used 0)
(defvar emacs-cc-charset--hash (make-hash-table :test 'eq))
(defvar emacs-cc-charset--iso (make-vector 768 -1))
(defvar emacs-cc-charset--mule (make-vector 256 -1))
(defvar emacs-cc-charset--ordered nil)
(defvar emacs-cc-charset--non-preferred-head nil)
(defvar emacs-cc-charset--iso-2022-list nil)
(defvar emacs-cc-charset--mule-list nil)
(defvar emacs-cc-charset--tick 0)
(defvar emacs-cc-charset--unify-table nil)
(defvar emacs-cc-charset--work nil "State of temp_charset_work.")
(defvar emacs-cc-charset--id-ascii -1)
(defvar emacs-cc-charset--id-eight-bit -1)
(defvar emacs-cc-charset--id-iso-8859-1 -1)
(defvar emacs-cc-charset--id-unicode -1)
(defvar emacs-cc-charset--id-emacs -1)
(defvar emacs-cc-charset--id-unibyte -1)
(defvar emacs-cc-charset--id-jisx0201-roman -1)
(defvar emacs-cc-charset--id-jisx0208-1978 -1)
(defvar emacs-cc-charset--id-jisx0208 -1)
(defvar emacs-cc-charset--id-ksc5601 -1)

;; DEFVARs of charset.c.
(setq charset-list nil)
(defvar charset-list nil "List of all charsets ever defined.")
(defvar charset-map-path nil "List of directories to search for charset map files.")
(defvar inhibit-load-charset-map nil
  "Inhibit loading of charset maps.  Used when dumping Emacs.")
(defvar current-iso639-language nil
  "ISO639 language mnemonic symbol for the current language environment.")

(defun emacs-cc-charset--from-id (id) (aref emacs-cc-charset--table id))

(defun emacs-cc-charset--iso-index (dimension chars-96 final)
  (+ (* (+ (* (1- dimension) 2) chars-96) 128) final))

(defun emacs-cc-charset--attrs-of (x)
  (and (symbolp x) x (gethash x emacs-cc-charset--hash)))

(defun emacs-cc-charset--check-id (x)
  (let ((attrs (emacs-cc-charset--attrs-of x)))
    (unless attrs (signal 'wrong-type-argument (list 'charsetp x)))
    (emacs-cc-charset--at attrs id)))

(defun emacs-cc-charset--check-cs (x)
  (aref emacs-cc-charset--table (emacs-cc-charset--check-id x)))

(defun emacs-cc-charset--check-attrs (x)
  (or (emacs-cc-charset--attrs-of x)
      (signal 'wrong-type-argument (list 'charsetp x))))

(defun emacs-cc-charset--name (cs)
  (emacs-cc-charset--at (emacs-cc-charset--cs cs attrs) name))

(defun emacs-cc-charset--check-int-range (x lo hi)
  (unless (integerp x) (signal 'wrong-type-argument (list 'integerp x)))
  (unless (and (<= lo x) (<= x hi))
    (signal 'args-out-of-range (list x lo hi)))
  x)

(defun emacs-cc-charset--check-fixnat (x)
  (unless (and (integerp x) (>= x 0)) (signal 'wrong-type-argument (list 'wholenump x)))
  x)

(defun emacs-cc-charset--check-character (x)
  (unless (and (integerp x) (>= x 0) (<= x emacs-cc-charset--max-char))
    (signal 'wrong-type-argument (list 'characterp x)))
  x)

(defun emacs-cc-charset--cons-to-unsigned (obj)
  "Port of cons_to_unsigned with UINT_MAX."
  (cond ((and (integerp obj) (>= obj 0) (<= obj #xFFFFFFFF)) obj)
        ((and (consp obj) (integerp (car obj)) (integerp (cdr obj)))
         (let ((hi (car obj)) (lo (cdr obj)))
           (unless (and (<= 0 hi #xFFFF) (<= 0 lo #xFFFF))
             (signal 'args-out-of-range (list obj 0 #xFFFFFFFF)))
           (logior (ash hi 16) lo)))
        ((integerp obj) (signal 'overflow-error (list obj)))
        (t (signal 'wrong-type-argument (list 'integerp obj)))))

;;; Fast map (CHARSET_FAST_MAP_REF / SET).

(defun emacs-cc-charset--fm-set (fm c)
  (if (< c #x10000)
      (let ((i (ash c -10)))
        (aset fm i (logior (aref fm i) (ash 1 (logand (ash c -7) 7)))))
    (let ((i (+ (ash c -15) 62)))
      (aset fm i (logior (aref fm i) (ash 1 (logand (ash c -12) 7)))))))

(defun emacs-cc-charset--fm-ref (fm c)
  (/= 0 (if (< c #x10000)
            (logand (aref fm (ash c -10)) (ash 1 (logand (ash c -7) 7)))
          (logand (aref fm (+ (ash c -15) 62)) (ash 1 (logand (ash c -12) 7))))))

(defun emacs-cc-charset--fm-set-range (fm from to)
  "Set the fast-map bits for every character in FROM..TO."
  (let ((c from))
    (while (<= c to)
      (emacs-cc-charset--fm-set fm c)
      (setq c (if (< c #x10000) (ash (1+ (ash c -7)) 7) (ash (1+ (ash c -12)) 12))))
    (when (and (<= from to) (/= c (1+ to)))
      (emacs-cc-charset--fm-set fm to))))

;;; Code point <-> index.

(defun emacs-cc-charset--c2i (cs code)
  "CODE_POINT_TO_INDEX."
  (if (emacs-cc-charset--cs cs linear)
      (- code (emacs-cc-charset--cs cs min-code))
    (let ((m (emacs-cc-charset--cs cs mask))
          (sp (emacs-cc-charset--cs cs code-space)))
      (if (and (/= 0 (logand (aref m (ash code -24)) 8))
               (/= 0 (logand (aref m (logand (ash code -16) 255)) 4))
               (/= 0 (logand (aref m (logand (ash code -8) 255)) 2))
               (/= 0 (logand (aref m (logand code 255)) 1)))
          (- (+ (* (- (ash code -24) (aref sp 12)) (aref sp 11))
                (* (- (logand (ash code -16) 255) (aref sp 8)) (aref sp 7))
                (* (- (logand (ash code -8) 255) (aref sp 4)) (aref sp 3))
                (- (logand code 255) (aref sp 0)))
             (emacs-cc-charset--cs cs index-offset))
        -1))))

(defun emacs-cc-charset--i2c (cs idx)
  "INDEX_TO_CODE_POINT."
  (if (emacs-cc-charset--cs cs linear)
      (+ idx (emacs-cc-charset--cs cs min-code))
    (let ((sp (emacs-cc-charset--cs cs code-space)))
      (setq idx (+ idx (emacs-cc-charset--cs cs index-offset)))
      (logior (+ (aref sp 0) (% idx (aref sp 2)))
              (ash (+ (aref sp 4) (% (/ idx (aref sp 3)) (aref sp 6))) 8)
              (ash (+ (aref sp 8) (% (/ idx (aref sp 7)) (aref sp 10))) 16)
              (ash (+ (aref sp 12) (/ idx (aref sp 11))) 24)))))

;;; Map loading.

(defun emacs-cc-charset--unify-table-ref (c)
  (and emacs-cc-charset--unify-table (aref emacs-cc-charset--unify-table c)))

(defun emacs-cc-charset--load-map (cs entries n control-flag)
  "Port of load_charset_map.  ENTRIES is a vector [FROM TO C ...]."
  (when (> n 0)
    (let* ((attrs (emacs-cc-charset--cs cs attrs))
           (method (emacs-cc-charset--cs cs method))
           (max-code (emacs-cc-charset--cs cs max-code))
           (ascii-compat (emacs-cc-charset--cs cs ascii-compat))
           (fast-map (emacs-cc-charset--cs cs fast-map))
           (vec nil) (table nil) (min-char 0) (max-char 0)
           (nonascii-min-char emacs-cc-charset--max-char) (i 0))
      (when (/= control-flag 0)
        (if (not inhibit-load-charset-map)
            (if (= control-flag 1)
                (if (eq method 'map)
                    (progn
                      (setq vec (make-vector (1+ (emacs-cc-charset--c2i cs max-code)) -1))
                      (emacs-cc-charset--at-set attrs decoder vec))
                  (unless emacs-cc-charset--unify-table
                    (setq emacs-cc-charset--unify-table (make-char-table nil)))
                  (set-char-table-range emacs-cc-charset--unify-table
                                        (cons (emacs-cc-charset--cs cs min-char)
                                              (emacs-cc-charset--cs cs max-char))
                                        nil))
              (setq table (make-char-table nil))
              (if (eq method 'map)
                  (emacs-cc-charset--at-set attrs encoder table)
                (emacs-cc-charset--at-set attrs deunifier table)))
          (setq emacs-cc-charset--work
                (vector cs (= control-flag 2) (make-hash-table) (make-hash-table) -1 0 0))
          (setq control-flag (+ control-flag 2)))
        (setq emacs-cc-charset--map-loaded t))
      (setq min-char (aref entries 2) max-char min-char)
      (while (< i n)
        (let* ((from (aref entries (* i 3)))
               (to (aref entries (+ (* i 3) 1)))
               (from-c (aref entries (+ (* i 3) 2)))
               (from-index (emacs-cc-charset--c2i cs from))
               (to-index (if (= from to) from-index (emacs-cc-charset--c2i cs to)))
               (to-c (if (= from to) from-c (+ from-c (- to-index from-index)))))
          (unless (or (< from-index 0) (< to-index 0))
            (let ((lim-index (1+ to-index)))
              (if (> to-c max-char) (setq max-char to-c)
                (when (< from-c min-char) (setq min-char from-c)))
              (cond
               ((= control-flag 1)
                (if (eq method 'map)
                    (while (< from-index lim-index)
                      (aset vec from-index from-c)
                      (setq from-index (1+ from-index) from-c (1+ from-c)))
                  (while (< from-index lim-index)
                    (aset emacs-cc-charset--unify-table
                          (+ (emacs-cc-charset--cs cs offset) from-index) from-c)
                    (setq from-index (1+ from-index) from-c (1+ from-c)))))
               ((= control-flag 2)
                (while (< from-index lim-index)
                  (when (null (aref table from-c))
                    (aset table from-c
                          (if (and (eq method 'map) (emacs-cc-charset--cs cs compact))
                              (emacs-cc-charset--i2c cs from-index)
                            from-index)))
                  (setq from-index (1+ from-index) from-c (1+ from-c))))
               ((= control-flag 3)
                (while (< from-index lim-index)
                  (puthash from-index from-c (aref emacs-cc-charset--work 2))
                  (setq from-index (1+ from-index) from-c (1+ from-c))))
               ((= control-flag 4)
                (while (< from-index lim-index)
                  (if (= from-index 0)
                      (aset emacs-cc-charset--work 4 from-c)
                    (puthash from-c from-index (aref emacs-cc-charset--work 3)))
                  (setq from-index (1+ from-index) from-c (1+ from-c))))
               (t
                (when ascii-compat
                  (if (>= from-c 128)
                      (when (< from-c nonascii-min-char) (setq nonascii-min-char from-c))
                    (when (>= to-c 128) (setq nonascii-min-char 128))))
                (emacs-cc-charset--fm-set-range fast-map from-c to-c))))))
        (setq i (1+ i)))
      (cond
       ((= control-flag 0)
        (emacs-cc-charset--cs-set cs min-char (if ascii-compat nonascii-min-char min-char))
        (emacs-cc-charset--cs-set cs max-char max-char))
       ((= control-flag 4)
        (aset emacs-cc-charset--work 5 min-char)
        (aset emacs-cc-charset--work 6 max-char))))))

(defvar emacs-cc-charset--map-loaded nil)

(defun emacs-cc-charset--map-file (name)
  "Locate NAME.map or NAME.txt in `charset-map-path' (openp)."
  (let (found)
    (dolist (dir charset-map-path)
      (dolist (suffix '(".map" ".txt"))
        (let ((f (expand-file-name (concat name suffix) dir)))
          (when (and (not found) (file-readable-p f) (not (file-directory-p f)))
            (setq found f)))))
    (or found
        (signal 'file-missing
                (list "Loading charset map" "No such file or directory" name)))))

;; Tokenize like read_hex: numbers are "0x" hex runs, `#' starts a comment.
(defun emacs-cc-charset--read-numbers ()
  "Return a list of (VALUE . DASH-P) for the 0x tokens in the buffer."
  (let (toks)
    (goto-char (point-min))
    (while (re-search-forward "#.*\\|0x\\([0-9A-Fa-f]+\\)" nil t)
      (when (match-beginning 1)
        (push (cons (string-to-number (match-string 1) 16) (eq (char-after) ?-))
              toks)))
    (nreverse toks)))

(defun emacs-cc-charset--load-from-file (cs mapfile control-flag)
  "Port of load_charset_map_from_file."
  (let ((min-code (emacs-cc-charset--cs cs min-code))
        (max-code (emacs-cc-charset--cs cs max-code))
        (file (emacs-cc-charset--map-file mapfile))
        (acc nil) (n 0) toks (file-name-handler-alist nil))
    (with-temp-buffer
      (insert-file-contents file)
      (setq toks (emacs-cc-charset--read-numbers)))
    (while toks
      (let* ((first (pop toks)) (from (car first))
             (to (if (cdr first) (car (pop toks)) from))
             (c (car (pop toks))))
        (when (and c to
                   (not (or (> from #xFFFFFFFF) (> to #xFFFFFFFF) (> c #xFFFFFFFF)))
                   (not (or (< from min-code) (> to max-code) (> from to)
                            (> c emacs-cc-charset--max-char))))
          (push (list from to c) acc)
          (setq n (1+ n)))))
    (let ((entries (make-vector (* n 3) 0)) (i (* n 3)))
      (dolist (e acc)
        (setq i (- i 3))
        (aset entries i (nth 0 e)) (aset entries (+ i 1) (nth 1 e))
        (aset entries (+ i 2) (nth 2 e)))
      (emacs-cc-charset--load-map cs entries n control-flag))))

(defun emacs-cc-charset--load-from-vector (cs vec control-flag)
  "Port of load_charset_map_from_vector."
  (let ((min-code (emacs-cc-charset--cs cs min-code))
        (max-code (emacs-cc-charset--cs cs max-code))
        (len (length vec)) (i 0) (acc nil) (n 0))
    (if (= (% len 2) 1)
        nil
      (while (< i len)
        (let* ((val (aref vec i)) from to c)
          (if (consp val)
              (setq from (emacs-cc-charset--check-fixnat (car val))
                    to (emacs-cc-charset--check-fixnat (cdr val)))
            (setq from (emacs-cc-charset--check-fixnat val) to from))
          (setq c (emacs-cc-charset--check-fixnat (aref vec (1+ i))))
          (unless (or (< from min-code) (> to max-code) (> from to)
                      (> c emacs-cc-charset--max-char))
            (push (list from to c) acc)
            (setq n (1+ n))))
        (setq i (+ i 2)))
      (let ((entries (make-vector (* n 3) 0)) (j (* n 3)))
        (dolist (e acc)
          (setq j (- j 3))
          (aset entries j (nth 0 e)) (aset entries (+ j 1) (nth 1 e))
          (aset entries (+ j 2) (nth 2 e)))
        (emacs-cc-charset--load-map cs entries n control-flag)))))

(defun emacs-cc-charset--load (cs control-flag)
  "Port of load_charset."
  (unless (and inhibit-load-charset-map emacs-cc-charset--work
               (eq cs (aref emacs-cc-charset--work 0))
               (eq (= control-flag 2) (aref emacs-cc-charset--work 1)))
    (let* ((attrs (emacs-cc-charset--cs cs attrs))
           (map (if (eq (emacs-cc-charset--cs cs method) 'map)
                    (emacs-cc-charset--at attrs map)
                  (emacs-cc-charset--at attrs unify-map))))
      (if (stringp map)
          (emacs-cc-charset--load-from-file cs map control-flag)
        (emacs-cc-charset--load-from-vector cs map control-flag)))))

(defun emacs-cc-charset--ensure-ranges (cs)
  "Make CS's min/max char and fast map valid (load_charset (cs, 0))."
  (unless (emacs-cc-charset--cs cs ready)
    (emacs-cc-charset--cs-set cs ready t)
    (let ((method (emacs-cc-charset--cs cs method)))
      (cond
       ((eq method 'map) (emacs-cc-charset--load cs 0))
       ((eq method 'subset)
        (let* ((sub (emacs-cc-charset--at (emacs-cc-charset--cs cs attrs) subset))
               (parent (emacs-cc-charset--from-id (aref sub 0))))
          (emacs-cc-charset--ensure-ranges parent)
          (emacs-cc-charset--cs-set cs fast-map (copy-sequence (emacs-cc-charset--cs parent fast-map)))
          (emacs-cc-charset--cs-set cs min-char (emacs-cc-charset--cs parent min-char))
          (emacs-cc-charset--cs-set cs max-char (emacs-cc-charset--cs parent max-char))))
       ((eq method 'superset)
        (let ((minc emacs-cc-charset--max-char) (maxc 0)
              (fm (make-vector 190 0)))
          (dolist (p (emacs-cc-charset--at (emacs-cc-charset--cs cs attrs) superset))
            (let ((parent (emacs-cc-charset--from-id (car p))) (i 0))
              (emacs-cc-charset--ensure-ranges parent)
              (when (> minc (emacs-cc-charset--cs parent min-char))
                (setq minc (emacs-cc-charset--cs parent min-char)))
              (when (< maxc (emacs-cc-charset--cs parent max-char))
                (setq maxc (emacs-cc-charset--cs parent max-char)))
              (while (< i 190)
                (aset fm i (logior (aref fm i) (aref (emacs-cc-charset--cs parent fast-map) i)))
                (setq i (1+ i)))))
          (emacs-cc-charset--cs-set cs fast-map fm)
          (emacs-cc-charset--cs-set cs min-char minc)
          (emacs-cc-charset--cs-set cs max-char maxc)))))))

(provide 'emacs-cc-charset-1)
