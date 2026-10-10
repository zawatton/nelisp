;;; nelisp-native-raw-file.el --- Bounded executable byte windows -*- lexical-binding: t; -*-
;; Uses existing native syscall and pointer operations.  No decoding is applied.
(defun nelisp-native-raw-file--read (path offset size)
  "Read exactly SIZE bytes at OFFSET from bounded ASCII PATH."
  (unless (and (stringp path) (<= 1 (length path) 4095)
               (integerp offset) (<= 0 offset) (integerp size)
               (<= 0 size 1048576) (<= (+ offset size) 134217728))
    (error "Raw file window bound rejected"))
  (let ((index 0))
    (while (< index (length path))
      (unless (<= 1 (aref path index) 127) (error "Raw file path rejected"))
      (setq index (+ index 1))))
  (let* ((prefix (+ (length path) 1)) (allocation (+ prefix size))
         (scratch (syscall-direct 9 0 allocation 3 34 -1 0)) (fd -1))
    (unless (> scratch 0) (error "Raw file mapping failed"))
    (unwind-protect
        (progn
          (let ((index 0))
            (while (< index (length path))
              (ptr-write-u8 scratch index (aref path index))
              (setq index (+ index 1)))
            (ptr-write-u8 scratch index 0))
          (setq fd (syscall-direct 2 scratch 0 0 0 0 0))
          (unless (>= fd 0) (error "Raw file open failed"))
          (unless (= (syscall-direct 17 fd (+ scratch prefix) size offset 0 0) size)
            (error "Raw file window truncated"))
          (let ((bytes (ptr-read-bytes (+ scratch prefix) size)))
            (unless (and (stringp bytes) (= (length bytes) size)
                         (= (string-bytes bytes) size))
              (error "Raw file byte count differs"))
            bytes))
      ;; Always attempt both operations, including when close fails.
      (let ((close-status 0) (unmap-status 0))
        (when (>= fd 0)
          (setq close-status (syscall-direct 3 fd 0 0 0 0 0)))
        (setq unmap-status (syscall-direct 11 scratch allocation 0 0 0 0))
        (unless (and (= close-status 0) (= unmap-status 0))
          (error "Raw file cleanup failed"))))))

(defconst nelisp-native-raw-file--owners
  (mapcar (lambda (name) (cons name (symbol-function name)))
          '(nelisp-native-raw-file--read syscall-direct ptr-write-u8 ptr-read-bytes
            stringp integerp length string-bytes aref + < <= > >= =
            symbol-function eq subrp
            nelisp--target-os-code nelisp--target-arch-code)))

(defun nelisp-native-raw-file-dependency-context ()
  "Return actual helper identities for sealing the bounded byte reader."
  (mapcar (lambda (owner) (cons (car owner) (symbol-function (car owner))))
          nelisp-native-raw-file--owners))

(defun nelisp-native-raw-file-read (path offset size)
  "Read a bounded raw window with unchanged native primitive owners."
  (dolist (owner nelisp-native-raw-file--owners)
    (unless (eq (cdr owner) (symbol-function (car owner)))
      (error "Raw file helper ownership changed")))
  (dolist (name '(syscall-direct ptr-write-u8 ptr-read-bytes
                 nelisp--target-os-code nelisp--target-arch-code))
    (unless (subrp (symbol-function name)) (error "Raw file native owner absent")))
  ;; These native public helpers return the actual binary's build target.
  ;; Mutable compatibility variables are not platform evidence.
  (unless (and (memq (nelisp--target-os-code) '(0 2))
               (= (nelisp--target-arch-code) 0))
    (error "Raw file protocol requires Linux or Windows x86_64"))
  (if (= (nelisp--target-os-code) 2)
      (nelisp-native-windows-file-bytes path nil offset size)
    (nelisp-native-raw-file--read path offset size)))
(provide 'nelisp-native-raw-file)
