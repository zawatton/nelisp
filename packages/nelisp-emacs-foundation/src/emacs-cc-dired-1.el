;;; emacs-cc-dired-1.el --- Dired C-core replacements  -*- lexical-binding: t; -*-

(defun emacs-cc-dired-1--attributes-lessp (left right)
  "Compare attribute fields LEFT and RIGHT in order."
  (cond
   ((equal left right) nil)
   ((and (numberp left) (numberp right)) (< left right))
   ((and (stringp left) (stringp right)) (string-lessp left right))
   ((and (consp left) (consp right))
    (or (emacs-cc-dired-1--attributes-lessp (car left) (car right))
        (and (not (equal (car left) (car right))) nil)
        (emacs-cc-dired-1--attributes-lessp (cdr left) (cdr right))))
   (t (string-lessp (format "%s" left) (format "%s" right)))))

(unless (fboundp 'file-attributes-lessp)
  (defun file-attributes-lessp (f1 f2)
    "Return t if first arg file attributes list is less than second.
Comparison is in lexicographic order and case is significant.

(fn F1 F2)"
    (unless (listp f1) (signal 'wrong-type-argument (list 'listp f1)))
    (unless (listp f2) (signal 'wrong-type-argument (list 'listp f2)))
    (let ((a f1) (b f2) result done)
      (while (and a b (not done))
        (unless (equal (car a) (car b))
          (setq result (emacs-cc-dired-1--attributes-lessp (car a) (car b)) done t))
        (setq a (cdr a) b (cdr b)))
      (if done result (and (null a) b)))))

(defun emacs-cc-dired-1--common-size (left right pos end ignore-case)
  "Return the matching character count up to END without slicing strings.
File names bound this recursion; avoid interpreted compare-strings backedges
which can repeatedly charge a pending whole-heap collection to completion."
  (if (>= pos end) pos
    (let ((a (aref left pos)) (b (aref right pos)))
      (if (if ignore-case (= (upcase a) (upcase b)) (= a b))
          (emacs-cc-dired-1--common-size left right (1+ pos) end ignore-case)
        pos))))

(defun emacs-cc-dired-1--prefix-p (file name size ignore-case)
  "Match FILE against the first SIZE characters of bare NAME."
  (or (= size 0)
      (and (<= size (length name))
           (if ignore-case
               (= size (emacs-cc-dired-1--common-size file name 0 size t))
             (string= file (substring name 0 size))))))

(defun emacs-cc-dired-1--regexps-p (name regexps)
  "Require bare NAME to match every element of REGEXPS."
  (or (null regexps)
      (and (string-match-p (car regexps) name)
           (emacs-cc-dired-1--regexps-p name (cdr regexps)))))

(defun emacs-cc-dired-1--ignored-p (name directoryp size extensions)
  "Whether NAME is an inexact completion ignored by EXTENSIONS."
  (if (and directoryp (or (string= name ".") (string= name ".."))) t
    (when (and (> (length name) size) extensions)
      (let* ((extension (car extensions))
             (n (and (stringp extension) (length extension)))
             (slash (and n (> n 0) (= (aref extension (1- n)) ?/))))
        (or (and n
                 (if directoryp (and slash (> n 1)) (not slash))
                 (let* ((suffix (if directoryp (substring extension 0 -1) extension))
                        (length (length suffix))
                        (skip (- (length name) length)))
                   (and (>= skip 0)
                        (if completion-ignore-case
                            (eq t (compare-strings name skip nil suffix 0 nil t))
                          (string= suffix (substring name skip))))))
            (emacs-cc-dired-1--ignored-p name directoryp size (cdr extensions)))))))

(defun emacs-cc-dired-1--read-batch (buffer pos end remaining visit)
  "Visit up to REMAINING Linux dirent64 records, returning the next offset.
Bound recursion to keep stack use independent of directory size.  BUFFER is
an mmap mapping, so interpreted safepoints cannot reclaim the raw storage."
  (if (or (>= pos end) (= remaining 0)) pos
    (let* ((header (ptr-read-u32 buffer (+ pos 16)))
           (size (logand header 65535)))
      (unless (and (>= size 20) (<= (+ pos size) end))
        (error "Invalid directory entry"))
      (let* ((name (car (nelisp--readdir-scan-raw
                         (ptr-read-bytes (+ buffer pos 19) (- size 19)) nil t)))
             (type (ptr-read-u8 buffer (+ pos 18))))
        (funcall visit name type))
      (emacs-cc-dired-1--read-batch buffer (+ pos size) end
                                    (1- remaining) visit))))

(defun emacs-cc-dired-1--visit-directory (directory visit)
  "Call VISIT with each bare name and directory-entry type in DIRECTORY.
Use the existing Linux x86-64 raw OS boundary on standalone.  Unlike the
flat readdir primitive, this drains getdents64 rather than clipping at 64 KiB.
Only symlinks and DT_UNKNOWN entries need a following directory test."
  (if (and (fboundp 'syscall-direct) (fboundp 'ptr-read-bytes)
           (fboundp 'nelisp--syscall-path-int))
      (let ((fd (nelisp--syscall-path-int 2 directory 65536))
            (buffer nil))
        (when (< fd 0)
          (signal (if (memq fd '(-2 -20)) 'file-missing 'file-error)
                  (list "Opening directory" directory)))
        (unwind-protect
            (progn
              (setq buffer (syscall-direct 9 0 32768 3 34 -1 0))
              (when (< buffer 0) (error "Cannot allocate directory buffer"))
              (let ((end 1))
                (while (> end 0)
                  (setq end (syscall-direct 217 fd buffer 32768 0 0 0))
                  (when (< end 0)
                    (signal 'file-error (list "Reading directory" directory)))
                  (let ((pos 0))
                    (while (< pos end)
                      (setq pos (emacs-cc-dired-1--read-batch
                                 buffer pos end 64 visit)))))))
          (syscall-direct 3 fd 0 0 0 0 0)
          (when (and buffer (>= buffer 0))
            (syscall-direct 11 buffer 32768 0 0 0 0))))
    ;; GNU's NOSORT list is accumulated in reverse enumeration order.
    (mapcar (lambda (name) (funcall visit name 0))
          (nreverse (directory-files directory nil nil t)))))

(defun emacs-cc-dired-1--simple-completion (file directory names)
  "Complete case-sensitive FILE from bare NAMES without per-entry stat.
Directory suffixes cannot change the common prefix of distinct bare names;
only a sole surviving match needs to be tested for a trailing slash."
  (let* ((size (length file))
         (matches (if (= size 0) names
                    (delq nil (mapcar
                               (lambda (name)
                                 (when (emacs-cc-dired-1--prefix-p file name size nil) name))
                               names)))))
    (unless matches
      (setq matches
            (delq nil (mapcar (lambda (name)
                               (when (emacs-cc-dired-1--prefix-p file name size nil) name))
                             '("." "..")))))
    (cond
     ((null matches) nil)
     ((null (cdr matches))
      (let* ((name (car matches))
             (mode (nelisp--syscall-stat-field (concat directory name) 24))
             (completion (if (= (logand mode 61440) 16384) (concat name "/") name)))
        (if (string= file completion) t completion)))
     (t
      (let* ((prefix (car matches)) (length (length prefix)))
        (catch 'emacs-cc-dired-1--prefix-done
          (mapcar
           (lambda (name)
             (unless (eq 0 (nelisp--string-search prefix name 0))
               (let ((n (emacs-cc-dired-1--common-size
                         prefix name 0 (min length (length name)) nil)))
                 (setq length n prefix (substring prefix 0 n))))
             (when (<= length size)
               (throw 'emacs-cc-dired-1--prefix-done nil)))
           (cdr matches)))
        prefix)))))

(defun emacs-cc-dired-1--native-complete (file directory all)
  "Use the flat native snapshot for the common unfiltered completion paths.
Return (handled . value); a clipped snapshot falls back to the full reader."
  (when (and (fboundp 'nelisp--syscall-readdir-names)
             (fboundp 'nelisp--readdir-scan-raw)
             (fboundp 'nelisp--syscall-stat-field)
             (null completion-regexp-list)
             (or (and all (= (length file) 0))
                 (and (not all) (not completion-ignore-case)
                      (null completion-ignored-extensions))))
    (let* ((directory (file-name-as-directory (expand-file-name directory)))
           (raw (nelisp--syscall-readdir-names directory t)))
      (when (and raw (< (string-bytes raw) 65535))
        (cons t
              (if all
                  ;; No interpreted loops, sorting, path dispatch, or full
                  ;; attribute construction; just one raw stat per name.
                  (nreverse
                   (mapcar
                    (lambda (name)
                      (if (= (logand (nelisp--syscall-stat-field
                                      (concat directory name) 24) 61440) 16384)
                          (concat name "/") name))
                    (nelisp--readdir-scan-raw raw nil t)))
                (emacs-cc-dired-1--simple-completion
                 file directory (nelisp--readdir-scan-raw raw t t))))))))

(defun emacs-cc-dired-1--complete (file directory all predicate)
  "Implement file completion by visiting DIRECTORY once without sorting."
  (unless (stringp file) (signal 'wrong-type-argument (list 'stringp file)))
  (unless (stringp directory) (signal 'wrong-type-argument (list 'stringp directory)))
  (let* ((directory (file-name-as-directory (expand-file-name directory)))
         (default-directory directory)
         (size (length file))
         (case-fold-search completion-ignore-case)
         (best nil) (bestsize 0) (count 0) (includeall t) result)
    (catch 'emacs-cc-dired-1--done
      (emacs-cc-dired-1--visit-directory
       directory
       (lambda (bare type)
         (when (emacs-cc-dired-1--prefix-p file bare size completion-ignore-case)
           (let* ((directoryp
                   (or (= type 4)
                       (and (or (= type 0) (= type 10))
                            (if (fboundp 'nelisp--syscall-stat)
                                (eq (nelisp--syscall-stat (concat directory bare)) 'directory)
                              (file-directory-p (concat directory bare))))))
                  (ignored (and (not all)
                                (emacs-cc-dired-1--ignored-p
                                 bare directoryp size completion-ignored-extensions))))
             ;; GNU changes the preferred set before regexp/predicate filtering.
             (when (or includeall (not ignored) all)
               (when (and (not all) includeall (not ignored))
                 (setq includeall nil best nil bestsize 0 count 0))
               (when (emacs-cc-dired-1--regexps-p bare completion-regexp-list)
                 (let ((name (if directoryp (concat bare "/") bare)))
                   ;; GNU calls predicates with relative names and binds the
                   ;; completed directory as default-directory, including '/'.
                   (when (or (null predicate) (funcall predicate name))
                     (if all (push name result)
                       (setq count (min 2 (1+ count)))
                       (if (null best)
                           (setq best name bestsize (length name))
                         (let* ((compare (min bestsize (length name)))
                                (matchsize (emacs-cc-dired-1--common-size
                                            best name 0 compare completion-ignore-case)))
                           (when (and completion-ignore-case
                                      (or (and (= matchsize (length name))
                                               (< (+ matchsize (if directoryp 1 0))
                                                  (length best)))
                                          (and (eq (= matchsize (length name))
                                                   (= (+ matchsize (if directoryp 1 0))
                                                      (length best)))
                                               (emacs-cc-dired-1--prefix-p file name size nil)
                                               (not (emacs-cc-dired-1--prefix-p
                                                     file best size nil)))))
                             (setq best name))
                           (setq bestsize matchsize)
                           (when (and (<= bestsize size) (not includeall)
                                      (or (not completion-ignore-case) (= bestsize 0)))
                             (throw 'emacs-cc-dired-1--done nil))))))))))))))
    (if all result
      (cond ((null best) nil)
            ((and (= count 1) (string= best file)) t)
            (t (substring best 0 bestsize))))))

(unless (fboundp 'file-name-all-completions)
  (defun file-name-all-completions (file directory)
    "Return all completions of FILE in DIRECTORY, with directories ending in '/'.
Match bare names against every element of `completion-regexp-list', and
honor `completion-ignore-case'."
    (unless (stringp file) (signal 'wrong-type-argument (list 'stringp file)))
    (unless (stringp directory) (signal 'wrong-type-argument (list 'stringp directory)))
    (let ((fast (emacs-cc-dired-1--native-complete file directory t)))
      (if fast (cdr fast) (emacs-cc-dired-1--complete file directory t nil)))))

(unless (fboundp 'file-name-completion)
  (defun file-name-completion (file directory &optional predicate)
    "Return the common prefix of completions of FILE in DIRECTORY.
Return t for a unique exact match, nil for no match.  Honor PREDICATE,
`completion-ignore-case', `completion-regexp-list' and
`completion-ignored-extensions', retrying ignored names when needed."
    (unless (stringp file) (signal 'wrong-type-argument (list 'stringp file)))
    (unless (stringp directory) (signal 'wrong-type-argument (list 'stringp directory)))
    (let ((fast (and (null predicate)
                     (emacs-cc-dired-1--native-complete file directory nil))))
      (if fast (cdr fast) (emacs-cc-dired-1--complete file directory nil predicate)))))

(defun emacs-cc-dired-1--account-names-tail (text start search)
  "Parse TEXT from START using SEARCH, without growing the Lisp stack."
  (let (result)
    (while (< start (length text))
      (let* ((end (or (funcall search "\n" text start) (length text)))
             (colon (funcall search ":" text start))
             (name-end (if (and colon (< colon end)) colon end)))
        (when (> name-end start)
          (push (substring text start name-end) result))
        (setq start (1+ end))))
    (nreverse result)))

(defun emacs-cc-dired-1--account-names-small (text start search remaining)
  "Parse up to REMAINING lines recursively, then use the iterative parser.
The bounded small-file path avoids interpreted while backedges, which can
charge an unrelated pending full-heap collection to this small query.
Larger databases use a bounded stack and retain normal collection points."
  (if (>= start (length text)) nil
    (if (= remaining 0)
        (emacs-cc-dired-1--account-names-tail text start search)
      (let* ((end (or (funcall search "\n" text start) (length text)))
             (colon (funcall search ":" text start))
             (name-end (if (and colon (< colon end)) colon end))
             (name (and (> name-end start) (substring text start name-end)))
             (tail (emacs-cc-dired-1--account-names-small
                    text (1+ end) search (1- remaining))))
        (if name (cons name tail) tail)))))

(defun emacs-cc-dired-1--account-names (file)
  "Return the first colon-delimited field of each nonempty line in FILE.
Read standalone files directly: decoding an ASCII account database through
an editor buffer needlessly runs the general coding and editing machinery."
  (let ((text (if (fboundp 'nl-syscall-read-file)
                  (nl-syscall-read-file file)
                (with-temp-buffer
                  (insert-file-contents file)
                  (buffer-string))))
        (search (if (fboundp 'nelisp--string-search)
                    #'nelisp--string-search #'string-search)))
    ;; Slice only names, rather than all unused password/home/shell fields.
    (emacs-cc-dired-1--account-names-small text 0 search 100)))

(unless (fboundp 'system-groups)
  (defun system-groups ()
    "Return a list of user group names currently registered in the system.
The value may be nil if not supported on this platform.

(fn)"
    (when (and (fboundp 'insert-file-contents) (file-readable-p "/etc/group"))
      (emacs-cc-dired-1--account-names "/etc/group"))))

(unless (fboundp 'system-users)
  (defun system-users ()
    "Return a list of user names currently registered in the system.
If we don't know how to determine that on this platform, just
return a list with one element, taken from `user-real-login-name'.

(fn)"
    (if (and (fboundp 'insert-file-contents) (file-readable-p "/etc/passwd"))
        (emacs-cc-dired-1--account-names "/etc/passwd")
      (list (user-real-login-name)))))

(provide 'emacs-cc-dired-1)
