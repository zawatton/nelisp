;;; emacs-cc-census-files-w401.el --- Input event dribble files  -*- lexical-binding: t; -*-

;;; Code:

(defvar emacs-cc-census-files-w401--fd nil
  "Descriptor of the process-wide dribble file, or nil.")
(defvar emacs-cc-census-files-w401--bytes nil
  "Reusable 4096-byte syscall buffer, allocated on the first open.")
(defvar emacs-cc-census-files-w401--reading nil
  "Non-nil while an advised input reader is active.")

(defun emacs-cc-census-files-w401--syscall (operation a b c)
  "Call the target's file OPERATION with arguments A, B and C."
  (let ((os (nelisp--target-os-code))
        (arch (nelisp--target-arch-code)))
    (cond
     ((= os 0)
      (cond
       ((eq operation 'open)
        (if (= arch 1)
            (syscall-direct 56 -100 a b c 0 0)
          (syscall-direct 2 a b c 0 0 0)))
       ((eq operation 'write)
        (syscall-direct (if (= arch 1) 64 1) a b c 0 0 0))
       ((eq operation 'close)
        (syscall-direct (if (= arch 1) 57 3) a 0 0 0 0 0))))
     ((= os 1)
      (syscall-direct (cond ((eq operation 'open) #x2000005)
                            ((eq operation 'write) #x2000004)
                            ((eq operation 'close) #x2000006))
                      a b c 0 0 0))
     (t (error "Dribble file descriptors are unavailable on this target")))))

(defun emacs-cc-census-files-w401--open-error (rc path)
  "Signal GNU's opening error for negative errno RC and PATH."
  (let* ((darwin (= (nelisp--target-os-code) 1))
         (errno (- rc))
         ;; The basic POSIX errors have the same numbers on both targets.
         (message
          (or (cdr (assq errno
                         '((1 . "Operation not permitted")
                           (2 . "No such file or directory")
                           (4 . "Interrupted system call")
                           (5 . "Input/output error")
                           (6 . "No such device or address")
                           (12 . "Cannot allocate memory")
                           (13 . "Permission denied")
                           (14 . "Bad address")
                           (17 . "File exists")
                           (19 . "No such device")
                           (20 . "Not a directory")
                           (21 . "Is a directory")
                           (22 . "Invalid argument")
                           (23 . "Too many open files in system")
                           (24 . "Too many open files")
                           (26 . "Text file busy")
                           (28 . "No space left on device")
                           (30 . "Read-only file system"))))
              (cond
               ((= errno (if darwin 63 36)) "File name too long")
               ((= errno (if darwin 62 40)) "Too many levels of symbolic links")
               ((= errno (if darwin 69 122)) "Disk quota exceeded")
               ((= errno (if darwin 84 75)) "Value too large for defined data type")
               ((= errno (if darwin 45 95)) "Operation not supported")
               (t (format "Unknown error %d" errno))))))
    (signal (if (= errno 2) 'file-missing 'file-error)
            (list "Opening dribble" message path))))

(defun emacs-cc-census-files-w401--close ()
  "Close the previous descriptor and detach its input readers."
  (when emacs-cc-census-files-w401--fd
    (let ((fd emacs-cc-census-files-w401--fd))
      (setq emacs-cc-census-files-w401--fd nil)
      (emacs-cc-census-files-w401--syscall 'close fd 0 0))
    (advice-remove 'emacs-command-loop--read-event-raw
                   #'emacs-cc-census-files-w401--read-raw)
    (advice-remove 'emacs-frame--tui-read-event
                   #'emacs-cc-census-files-w401--read)
    (advice-remove 'emacs-keymap--default-read-event
                   #'emacs-cc-census-files-w401--read)))

(defun emacs-cc-census-files-w401--event-bytes (event)
  "Return GNU's unibyte dribble representation of EVENT, or nil."
  (cond
   ((integerp event)
    (if (and (>= event 0) (< event 256))
        (unibyte-string event)
      ;; GNU prints the unsigned, 62-bit Lisp integer payload.  Splitting
      ;; negative payloads avoids requiring a positive bignum here.
      (if (< event 0)
          (format " 0x%x%015x" (logand (ash event -60) 3)
                  (logand event #x0fffffffffffffff))
        (format " 0x%x" event))))
   ((symbolp event) (concat "<" (symbol-name event) ">"))
   ((and (consp event) (symbolp (car event)))
    (concat "<" (symbol-name (car event)) ">"))))

(defun emacs-cc-census-files-w401--record (event)
  "Immediately write EVENT, excluding macro and no-record events."
  (unless (or (and (boundp 'executing-kbd-macro) executing-kbd-macro)
              (and (consp event) (eq (car event) 'no-record)))
    (when (and (consp event) (eq (car event) t))
      (setq event (cdr event)))
    (let ((bytes (emacs-cc-census-files-w401--event-bytes event)))
      (when bytes
        (setq bytes (encode-coding-string bytes 'utf-8-unix))
        (let ((offset 0) (length (length bytes)) (active t))
          (while (and active (< offset length))
            (let ((count (min 4096 (- length offset))) (index 0))
              (while (< index count)
                (ptr-write-u8 emacs-cc-census-files-w401--bytes index
                              (aref bytes (+ offset index)))
                (setq index (1+ index)))
              (let ((written 0))
                (while (and active (< written count))
                  (let ((rc (emacs-cc-census-files-w401--syscall
                             'write emacs-cc-census-files-w401--fd
                             (+ emacs-cc-census-files-w401--bytes written)
                             (- count written))))
                    (cond ((> rc 0) (setq written (+ written rc)))
                          ((= rc -4) nil)
                          ;; Like GNU's dribble stream, an I/O failure
                          ;; must not prevent the input event being read.
                          (t (setq active nil)))))
                (setq offset (+ offset written))))))))))

(defun emacs-cc-census-files-w401--read (reader &rest args)
  "Call READER with ARGS and record its event once across nested readers."
  (if emacs-cc-census-files-w401--reading
      (apply reader args)
    (let ((emacs-cc-census-files-w401--reading t))
      (let ((event (apply reader args)))
        (when emacs-cc-census-files-w401--fd
          (emacs-cc-census-files-w401--record event))
        event))))

(defun emacs-cc-census-files-w401--read-raw (reader &rest args)
  "Record raw input from READER, before input-method translation."
  (if emacs-cc-census-files-w401--reading
      (apply reader args)
    (let ((emacs-cc-census-files-w401--reading t))
      (let ((event (apply reader args)))
        (when (and emacs-cc-census-files-w401--fd
                   (not emacs-command-loop--post-input-event-p))
          (emacs-cc-census-files-w401--record event))
        event))))

(unless (fboundp 'open-dribble-file)
  (defun open-dribble-file (&rest args)
    "Record input events immediately in FILE, the sole argument in ARGS.
Replace any previous dribble file.
If FILE is nil, close the current dribble file.  Keyboard macro events
are excluded.  The descriptor is also closed by the OS at process exit."
    (unless (= (length args) 1)
      (signal 'wrong-number-of-arguments (list 'open-dribble-file (length args))))
    (let ((file (car args)))
      ;; GNU closes the old stream even when the new argument is invalid.
      (emacs-cc-census-files-w401--close)
      (when file
	(unless (stringp file)
          (signal 'wrong-type-argument (list 'stringp file)))
	(let ((index 0))
          (while (< index (length file))
            (when (= (aref file index) 0)
              (signal 'wrong-type-argument (list 'filenamep file)))
            (setq index (1+ index))))
	(let* ((path (expand-file-name file))
               (coding (or (and (boundp 'file-name-coding-system)
				file-name-coding-system)
                           (and (boundp 'default-file-name-coding-system)
				default-file-name-coding-system)
                           'utf-8-unix))
               (bytes (encode-coding-string path coding))
               (darwin (= (nelisp--target-os-code) 1)))
          (when (>= (length bytes) 4096)
            (emacs-cc-census-files-w401--open-error (if darwin -63 -36) path))
          (unless emacs-cc-census-files-w401--bytes
            (setq emacs-cc-census-files-w401--bytes (alloc-bytes 4096 1)))
          (let ((index 0))
            (while (< index (length bytes))
              (ptr-write-u8 emacs-cc-census-files-w401--bytes index (aref bytes index))
              (setq index (1+ index)))
            (ptr-write-u8 emacs-cc-census-files-w401--bytes index 0))
          (let ((fd (emacs-cc-census-files-w401--syscall
                     'open emacs-cc-census-files-w401--bytes
                     ;; GNU creates private files and prevents inheritance
                     ;; of the descriptor by subsequently exec'd children.
                     (if darwin #x1000601 #o2001101) #o600)))
            (when (< fd 0)
              (emacs-cc-census-files-w401--open-error fd path))
            (setq emacs-cc-census-files-w401--fd fd)
            (advice-add 'emacs-command-loop--read-event-raw :around
			#'emacs-cc-census-files-w401--read-raw)
            (advice-add 'emacs-frame--tui-read-event :around
			#'emacs-cc-census-files-w401--read)
            (advice-add 'emacs-keymap--default-read-event :around
			#'emacs-cc-census-files-w401--read)))))
    nil))

(provide 'emacs-cc-census-files-w401)
;;; emacs-cc-census-files-w401.el ends here
