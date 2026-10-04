;;; emacs-cc-census-files-w301.el --- File lookup and modification times  -*- lexical-binding: t; -*-

;;; Code:

(defvar emacs-cc-census-files-w301--visited-time 0
  "Recorded modification time, made local when a buffer records a value.")

(unless (fboundp 'get-load-suffixes)
  (defun get-load-suffixes ()
    "Return the ordered combinations of load and representation suffixes."
    (let ((suffixes load-suffixes) result)
      (while (consp suffixes)
        (let ((representations load-file-rep-suffixes))
          (while (consp representations)
            (setq result (cons (concat (car suffixes) (car representations))
                               result)
                  representations (cdr representations))))
        (setq suffixes (cdr suffixes)))
      (nreverse result))))

(defun emacs-cc-census-files-w301--accessible-p (filename predicate)
  "Test FILENAME with PREDICATE, respecting its directory acceptance rule."
  (let* ((access-mode (cond ((or (null predicate) (eq predicate t)) 4)
                            ((and (integerp predicate) (>= predicate 0))
                             predicate)))
         (answer
          (if access-mode
              (if (and (null predicate)
                       (find-file-name-handler filename 'file-readable-p))
                  (file-readable-p filename)
                (= 0 (nelisp--syscall-path-int 21 filename access-mode)))
            (funcall predicate filename))))
    (and answer
         (or (eq answer 'dir-ok)
             (if (and (integerp predicate) (>= predicate 0))
                 (not (eq (nelisp--syscall-stat filename) 'directory))
               (not (file-directory-p filename)))))))

(unless (fboundp 'locate-file-internal)
  (defun locate-file-internal (filename path &optional suffixes predicate)
    "Search PATH for FILENAME with SUFFIXES and optional PREDICATE.
An empty PATH searches the current directory.  Integer predicates are
access modes; functional predicates may return `dir-ok' for directories."
    (unless (stringp filename)
      (signal 'wrong-type-argument (list 'stringp filename)))
    (let ((directories (if (or (null path) (file-name-absolute-p filename))
                           (list nil)
                         path))
          result)
      (while (and (consp directories) (not result))
        (let ((directory (car directories))
              (tails (if (null suffixes) (list "") suffixes)))
          (when (or (null directory) (stringp directory))
            (let ((base (expand-file-name filename directory)))
              (while (and (consp tails) (not result))
                (unless (stringp (car tails))
                  (signal 'wrong-type-argument (list 'stringp (car tails))))
                (let ((candidate (concat base (car tails))))
                  (when (emacs-cc-census-files-w301--accessible-p
                         candidate predicate)
                    (setq result candidate)))
                (setq tails (cdr tails))))))
        (setq directories (cdr directories)))
      result)))

(unless (fboundp 'native-comp-available-p)
  (defun native-comp-available-p ()
    "Return whether GNU native compilation support is present."
    (featurep 'native-compile)))

(unless (fboundp 'native-comp-unit-file)
  (defun native-comp-unit-file (comp-unit)
    "Return COMP-UNIT's file, requiring a GNU native compilation unit.
This runtime does not allocate GNU native compilation unit objects."
    (signal 'wrong-type-argument (list 'native-comp-unit comp-unit))))

(unless (fboundp 'native-comp-unit-set-file)
  (defun native-comp-unit-set-file (comp-unit new-file)
    "Set COMP-UNIT's file to NEW-FILE, requiring a GNU native unit.
Unit validation precedes file validation.  GNU unit objects are not
allocated by this runtime."
    (signal 'wrong-type-argument (list 'native-comp-unit comp-unit))))

(unless (fboundp 'set-default-file-modes)
  (defun set-default-file-modes (mode)
    "Set the process creation mask from the low nine permission bits of MODE."
    (unless (integerp mode)
      (signal 'wrong-type-argument (list 'fixnump mode)))
    (let* ((os (nelisp--target-os-code))
           (arch (nelisp--target-arch-code))
           (number (cond ((= os 1) #x200003c)
                         ((= os 0) (if (= arch 1) 166 95))
                         (t (error "The process creation mask is unavailable")))))
      (syscall-direct number (logand (lognot mode) #o777) 0 0 0 0 0))
    nil))

(defun emacs-cc-census-files-w301--fraction (numerator denominator scale)
  "Floor NUMERATOR times SCALE over DENOMINATOR without integer overflow.
NUMERATOR is nonnegative and smaller than the positive DENOMINATOR."
  (let ((bit 1) (remainder 0) (quotient 0))
    (while (<= (* bit 2) scale)
      (setq bit (* bit 2)))
    (while (> bit 0)
      (setq quotient (* quotient 2))
      (if (>= remainder (- denominator remainder))
          (setq remainder (- remainder (- denominator remainder))
                quotient (1+ quotient))
        (setq remainder (+ remainder remainder)))
      (when (/= 0 (logand scale bit))
        (if (>= remainder (- denominator numerator))
            (setq remainder (- remainder (- denominator numerator))
                  quotient (1+ quotient))
          (setq remainder (+ remainder numerator))))
      (setq bit (/ bit 2)))
    quotient))

(defun emacs-cc-census-files-w301--timestamp (time)
  "Convert TIME to a normalized four-part timestamp at nanosecond precision."
  (let ((seconds 0) (nanoseconds 0))
    (cond
     ((floatp time)
      (unless (= time time)
        (error "Invalid time specification"))
      (when (or (>= time 1.0e+INF) (<= time -1.0e+INF))
        (error "Specified time is not representable"))
      (setq seconds (floor time)
            nanoseconds (floor (* (- time seconds) 1000000000))))
     ((and (consp time) (integerp (car time)) (integerp (cdr time))
           (> (cdr time) 0))
      (setq seconds (floor (car time) (cdr time))
            nanoseconds
            (emacs-cc-census-files-w301--fraction
             (mod (car time) (cdr time)) (cdr time) 1000000000)))
     ((and (consp time) (integerp (car time))
           (consp (cdr time)) (integerp (cadr time)))
      (let* ((tail (cddr time))
             (usec (if (consp tail) (car tail) (or tail 0)))
             (psec (if (and (consp tail) (consp (cdr tail)))
                       (cadr tail) 0)))
        (unless (and (integerp usec) (integerp psec))
          (error "Invalid time specification"))
        (setq seconds (+ (* (car time) 65536) (cadr time)
                         (floor usec 1000000) (floor psec 1000000000000))
              nanoseconds (+ (* (mod usec 1000000) 1000)
                             (floor (mod psec 1000000000000) 1000)))))
     (t (error "Invalid time specification")))
    (setq seconds (+ seconds (floor nanoseconds 1000000000))
          nanoseconds (mod nanoseconds 1000000000))
    (list (floor seconds 65536) (mod seconds 65536)
          (/ nanoseconds 1000) (* (mod nanoseconds 1000) 1000))))

(defun emacs-cc-census-files-w301--file-time (filename)
  "Return FILENAME's modification time, following symbolic links, or zero."
  (unless (stringp filename)
    (signal 'wrong-type-argument (list 'stringp filename)))
  (let ((stat (nelisp--syscall-stat-buf (expand-file-name filename))))
    (if (< stat 0)
        0
      (emacs-cc-census-files-w301--timestamp
       (nelisp--stat-lisp-time (ptr-read-u64 stat 88)
                               (ptr-read-u64 stat 96))))))

(unless (fboundp 'set-visited-file-modtime)
  (defun set-visited-file-modtime (&optional time-flag)
    "Record TIME-FLAG, or the visited file's current modification time.
Integer flags must be -1 or 0.  Other arguments are Lisp timestamps."
    (let ((handler (and (null time-flag) (stringp buffer-file-name)
                        (find-file-name-handler buffer-file-name
                                                'set-visited-file-modtime))))
      (if handler
          (funcall handler 'set-visited-file-modtime time-flag)
        (let ((time (cond ((null time-flag)
                          (emacs-cc-census-files-w301--file-time buffer-file-name))
                         ((integerp time-flag)
                          (unless (and (<= -1 time-flag) (<= time-flag 0))
                            (signal 'args-out-of-range (list time-flag -1 0)))
                          time-flag)
                         (t (emacs-cc-census-files-w301--timestamp time-flag)))))
          (make-local-variable 'emacs-cc-census-files-w301--visited-time)
          (put 'emacs-cc-census-files-w301--visited-time 'permanent-local t)
          (setq emacs-cc-census-files-w301--visited-time time))))
    nil))

(unless (fboundp 'subr-native-comp-unit)
  (defun subr-native-comp-unit (subr)
    "Return SUBR's GNU native unit, or nil for ordinary builtin subrs.
The runtime's subrs have no GNU native compilation unit."
    (unless (subrp subr)
      (signal 'wrong-type-argument (list 'subrp subr)))))

(unless (fboundp 'visited-file-modtime)
  (defun visited-file-modtime ()
    "Return the current buffer's recorded modification time or integer flag."
    (if (consp emacs-cc-census-files-w301--visited-time)
        (copy-sequence emacs-cc-census-files-w301--visited-time)
      emacs-cc-census-files-w301--visited-time)))

;; open-dribble-file needs an input-reader recording hook.  The shared
;; command-loop reader has none, so its implementation is outside this unit.

(provide 'emacs-cc-census-files-w301)
;;; emacs-cc-census-files-w301.el ends here
