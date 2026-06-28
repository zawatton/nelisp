;;; nelisp-dired.el --- a directory editor for the standalone -*- lexical-binding: t; -*-
;;
;; Reads directories and file metadata straight from the kernel via
;; `syscall-direct' (open=2, getdents64=217, stat=4, close=3) -- no host fs API.
;; Produces an ls-l-style listing and a small interactive browser that runs in
;; the TTY or in the pure-elisp X11 window.
;;
;; A file record is (NAME DTYPE MODE SIZE MTIME).

(defconst nelisp-dired--o-directory 65536)   ; O_DIRECTORY (0x10000)

;;; Raw fs access -----------------------------------------------------

(defun nelisp-dired--cpath (path)
  "Return a NUL-terminated C-string buffer for PATH (raw bytes)."
  (let* ((n (string-bytes path)) (pb (alloc-bytes (+ n 1) 1)) (i 0))
    (while (< i n) (ptr-write-u8 pb i (string-byte path i)) (setq i (1+ i)))
    (ptr-write-u8 pb n 0)
    pb))

(defun nelisp-dired--cstr (buf off)
  "Read a NUL-terminated byte string from BUF at OFF as a UTF-8 string."
  (let ((bytes nil) (j off))
    (while (/= (ptr-read-u8 buf j) 0)
      (setq bytes (cons (ptr-read-u8 buf j) bytes) j (1+ j)))
    (apply #'unibyte-string (reverse bytes))))

(defun nelisp-dired--stat (path)
  "stat(2) PATH; return (MODE SIZE MTIME) or nil."
  (let* ((st (alloc-bytes 160 8))
         (r (syscall-direct 4 (nelisp-dired--cpath path) st 0 0 0 0)))
    (if (< r 0) nil
      (list (ptr-read-u32 st 24) (ptr-read-u64 st 48) (ptr-read-u64 st 88)))))

(defun nelisp-dired-read (path)
  "Return file records (NAME DTYPE MODE SIZE MTIME) for entries in PATH.
Entries are sorted with directories first, then by name."
  (let ((fd (syscall-direct 2 (nelisp-dired--cpath path)
                            nelisp-dired--o-directory 0 0 0 0))
        (names nil))
    (if (< fd 0) nil
      (let ((buf (alloc-bytes 32768 8)) (go t))
        (while go
          (let ((n (syscall-direct 217 fd buf 32768 0 0 0)))
            (if (<= n 0) (setq go nil)
              (let ((o 0))
                (while (< o n)
                  (let* ((reclen (+ (ptr-read-u8 buf (+ o 16))
                                    (* (ptr-read-u8 buf (+ o 17)) 256)))
                         (dtype (ptr-read-u8 buf (+ o 18)))
                         (name (nelisp-dired--cstr buf (+ o 19))))
                    (setq names (cons (cons name dtype) names))
                    (setq o (+ o reclen))))))))
        (syscall-direct 3 fd 0 0 0 0 0)
        (let* ((recs (mapcar
                      (lambda (e)
                        (let* ((name (car e)) (dtype (cdr e))
                               (st (nelisp-dired--stat
                                    (concat path "/" name)))
                               (mode (if st (nth 0 st) 0))
                               (size (if st (nth 1 st) 0))
                               (mtime (if st (nth 2 st) 0)))
                          (list name dtype mode size mtime)))
                      names)))
          (nelisp-dired--sort recs))))))

(defun nelisp-dired--dir-p (rec)
  ;; d_type 4 = DT_DIR, or S_IFDIR in the mode (for filesystems with DT_UNKNOWN)
  (or (= (nth 1 rec) 4)
      (= (logand (nth 2 rec) 61440) 16384)))

(defun nelisp-dired--sort (recs)
  "Sort RECS: directories first, then lexicographically by name."
  (sort recs
        (lambda (a b)
          (let ((da (nelisp-dired--dir-p a)) (db (nelisp-dired--dir-p b)))
            (cond ((and da (not db)) t)
                  ((and db (not da)) nil)
                  (t (string-lessp (car a) (car b))))))))

;;; Formatting (ls -l-ish) -------------------------------------------

(defun nelisp-dired--rwx (bits)
  (concat (if (= (logand bits 4) 4) "r" "-")
          (if (= (logand bits 2) 2) "w" "-")
          (if (= (logand bits 1) 1) "x" "-")))

(defun nelisp-dired--mode-string (mode)
  "Render MODE as a 10-char `drwxr-xr-x' string."
  (concat (cond ((= (logand mode 61440) 16384) "d")   ; S_IFDIR
                ((= (logand mode 61440) 40960) "l")   ; S_IFLNK
                (t "-"))
          (nelisp-dired--rwx (/ (logand mode 448) 64))
          (nelisp-dired--rwx (/ (logand mode 56) 8))
          (nelisp-dired--rwx (logand mode 7))))

(defun nelisp-dired--pad-left (s w)
  (let ((s (if (stringp s) s (number-to-string s))))
    (while (< (length s) w) (setq s (concat " " s)))
    s))

(defun nelisp-dired--format-line (rec)
  "Format REC as `MODE  SIZE  NAME[/]'."
  (concat (nelisp-dired--mode-string (nth 2 rec)) " "
          (nelisp-dired--pad-left (nth 3 rec) 10) " "
          (car rec)
          (if (nelisp-dired--dir-p rec) "/" "")))

(defun nelisp-dired-listing (path)
  "Return the full listing string for PATH (a header line + one line per entry)."
  (let* ((recs (nelisp-dired-read path))
         (out (concat path ":\n")))
    (dolist (r recs)
      (setq out (concat out (nelisp-dired--format-line r) "\n")))
    out))

;;; Navigation model -------------------------------------------------
;; State is a vector [DIR RECS CURSOR].

(defun nelisp-dired--join (dir name)
  "Join DIR and NAME, resolving `.' and `..' textually."
  (cond
   ((string-equal name ".") dir)
   ((string-equal name "..")
    (let ((slash -1) (i 0) (n (length dir)))
      (while (< i n) (when (= (aref dir i) ?/) (setq slash i)) (setq i (1+ i)))
      (cond ((<= slash 0) "/")
            (t (substring dir 0 slash)))))
   ((string-equal dir "/") (concat "/" name))
   (t (concat dir "/" name))))

(defun nelisp-dired-open (path)
  "Return a dired state vector [DIR RECS CURSOR] for PATH."
  (vector path (nelisp-dired-read path) 0))

(defun nelisp-dired-dir (s) (aref s 0))
(defun nelisp-dired-recs (s) (aref s 1))
(defun nelisp-dired-cursor (s) (aref s 2))
(defun nelisp-dired-current (s) (nth (aref s 2) (aref s 1)))

(defun nelisp-dired-move (s delta)
  "Move the cursor by DELTA (clamped).  Return S."
  (let ((n (length (aref s 1))))
    (when (> n 0)
      (aset s 2 (min (max (+ (aref s 2) delta) 0) (1- n))))
    s))

(defun nelisp-dired-enter (s)
  "Act on the cursor entry of S.
A directory -> a fresh state for it; a file -> (visit . FULLPATH)."
  (let* ((rec (nelisp-dired-current s)) (name (car rec))
         (full (nelisp-dired--join (aref s 0) name)))
    (if (nelisp-dired--dir-p rec) (nelisp-dired-open full) (cons 'visit full))))

(defun nelisp-dired-parent (s)
  "Return a fresh state for S's parent directory."
  (nelisp-dired-open (nelisp-dired--join (aref s 0) "..")))

;;; Render (cursor-marked listing) -----------------------------------

(defun nelisp-dired-render-lines (s)
  "Return a list of display strings for state S, marking the cursor line."
  (let ((recs (aref s 1)) (cur (aref s 2)) (i 0) (out nil))
    (setq out (cons (concat "  " (aref s 0) ":") out))   ; header (offset by 1)
    (dolist (r recs)
      (setq out (cons (concat (if (= i cur) "> " "  ")
                              (nelisp-dired--format-line r))
                      out)
            i (1+ i)))
    (reverse out)))

(provide 'nelisp-dired)
;;; nelisp-dired.el ends here
