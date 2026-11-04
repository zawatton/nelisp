;;; emacs-cc-census-files-w101.el --- File primitive compatibility  -*- lexical-binding: t; -*-

(defun emacs-cc-census-files-w101--truename (path original &optional links)
  "Resolve symlinks in PATH, reporting failed resolution against ORIGINAL."
  (let ((parts (split-string path "/" t))
        (resolved "/")
        (count (or links 0)))
    (while parts
      (let* ((candidate (expand-file-name (car parts) resolved))
             (target (nelisp--syscall-readlink candidate)))
        (setq parts (cdr parts))
        (if target
            (progn
              (when (>= count 40)
                (signal 'file-missing (list original)))
              (setq resolved
                    (emacs-cc-census-files-w101--truename
                     (concat (if (file-name-absolute-p target)
                                 target
                               (concat (file-name-as-directory resolved) target))
                             (if parts
                                 (concat "/" (mapconcat #'identity parts "/"))
                               ""))
                     original (1+ count))
                    parts nil))
          (setq resolved candidate))))
    resolved))

(defun emacs-cc-census-files-w101--file-md5 (path)
  "Hash PATH as raw bytes, without decoding it through a Lisp buffer."
  (with-temp-buffer
    ;; Use standard input so unusual file names do not escape the digest.
    (unless (equal (call-process "md5sum" path t nil) 0)
      (signal 'file-notify-error (list "hashing failed" path)))
    (substring (buffer-string) 0 8)))

(defun emacs-cc-census-files-w101--contents-hash (path compressed)
  "Return the first eight MD5 digits of PATH's optionally COMPRESSED contents."
  (condition-case nil
      (if (not compressed)
          (emacs-cc-census-files-w101--file-md5 path)
        (let ((temporary (make-temp-file "nelisp-eln-contents-")))
          (unwind-protect
              (progn
                ;; The process adapter redirects a string destination directly
                ;; to a file, preserving bytes that the text reader would decode.
                ;; Limitation: gzip reads concatenated members, whereas GNU's
                ;; zlib decoder stops after the first member and ignores its tail.
                (unless (equal (call-process "gzip" nil temporary nil
                                            "-cd" "--" path) 0)
                  (signal 'file-notify-error (list "hashing failed" path)))
                (emacs-cc-census-files-w101--file-md5 temporary))
            (delete-file temporary))))
    (error (signal 'file-notify-error (list "hashing failed" path)))))

(unless (fboundp 'comp-el-to-eln-rel-filename)
  (defun comp-el-to-eln-rel-filename (filename)
    "Return the relative .eln name derived from FILENAME and its contents.
FILENAME must exist.  Resolve symlinks before hashing its full name, and
decompress files ending in .gz before hashing their contents."
    (unless (stringp filename)
      (signal 'wrong-type-argument (list 'stringp filename)))
    (let* ((expanded (expand-file-name filename))
           (stat (nelisp--syscall-stat-buf expanded)))
      (when (< stat 0)
        (signal 'file-missing (list expanded)))
      (let ((path (emacs-cc-census-files-w101--truename expanded expanded)))
        (when (= (logand (ptr-read-u32 stat 24) #o170000) #o40000)
          (signal 'file-notify-error (list "hashing failed" path)))
        (let* ((compressed (string-suffix-p ".gz" path))
               (source (if compressed (substring path 0 -3) path))
               (base (file-name-nondirectory (substring source 0 -3)))
               (name-hash (substring (secure-hash 'md5 source) 0 8))
               (contents-hash
                (emacs-cc-census-files-w101--contents-hash path compressed)))
          (concat base "-" name-hash "-" contents-hash ".eln"))))))

(unless (fboundp 'default-file-modes)
  (defun default-file-modes ()
    "Return the default permission bits for newly created files."
    (let* ((os (nelisp--target-os-code))
           (arch (nelisp--target-arch-code))
           (number (cond ((= os 1) #x200003c)
                         ((= os 0) (if (= arch 1) 166 95))
                         (t (error "The process creation mask is unavailable"))))
           (mask (syscall-direct number 0 0 0 0 0 0)))
      (unwind-protect
          (logand (lognot mask) #o777)
        (syscall-direct number mask 0 0 0 0 0)))))

(unless (fboundp 'file-newer-than-file-p)
  (defun file-newer-than-file-p (file1 file2)
    "Return t if FILE1 exists and is newer than FILE2 or FILE2 does not exist.
Compare modification times including their subsecond components, and follow
symbolic links when inspecting either file."
    (unless (stringp file1)
      (signal 'wrong-type-argument (list 'stringp file1)))
    (unless (stringp file2)
      (signal 'wrong-type-argument (list 'stringp file2)))
    (let* ((first (expand-file-name file1))
           (second (expand-file-name file2))
           (handler (or (find-file-name-handler first 'file-newer-than-file-p)
                        (find-file-name-handler second 'file-newer-than-file-p))))
      (if handler
          (funcall handler 'file-newer-than-file-p first second)
        (let ((a (nelisp--syscall-stat-buf first))
              (b (nelisp--syscall-stat-buf second)))
          (and (>= a 0)
               (or (< b 0)
                   (let ((seconds-a (ptr-read-u64 a 88))
                         (seconds-b (ptr-read-u64 b 88)))
                     (or (> seconds-a seconds-b)
                         (and (= seconds-a seconds-b)
                              (> (ptr-read-u64 a 96)
                                 (ptr-read-u64 b 96))))))))))))

(provide 'emacs-cc-census-files-w101)
;;; emacs-cc-census-files-w101.el ends here
