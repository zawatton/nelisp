;;; nelisp-native-linux.el --- Linux native mapping OS adapter -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Doc 151: callers use the x86 Linux syscall vocabulary only at this boundary.
(declare-function nelisp-native-raw-file-read "nelisp-native-raw-file" (path offset size))
(declare-function nelisp--target-os-code nil ())
(declare-function nelisp--target-arch-code nil ())
(declare-function syscall-direct nil (nr a b c d e f))

(declare-function ptr-read-bytes nil (address size))
(declare-function ptr-read-u32 nil (address offset))
(declare-function ptr-read-u64 nil (address offset))
(declare-function ptr-write-u8 nil (address offset value))
(declare-function nelisp-native-load--poke-string "nelisp-native-load" (address offset bytes))

(defun nelisp-native-linux-syscall (nr a b c d e f)
  "Translate the bounded native loader vocabulary to the sealed Linux target."
  (unless (and (fboundp 'nelisp--target-os-code) (= (nelisp--target-os-code) 0))
    (error "Linux native syscall target absent"))
  (if (= (nelisp--target-arch-code) 0)
      (syscall-direct nr a b c d e f)
    (unless (= (nelisp--target-arch-code) 1) (error "Linux native architecture refused"))
    (cond ((= nr 9) (syscall-direct 222 a b c d e f))
          ((= nr 10) (syscall-direct 226 a b c d e f))
          ((= nr 11) (syscall-direct 215 a b c d e f))
          ((= nr 2) (syscall-direct 56 -100 a b c 0 0))
          ((= nr 17) (syscall-direct 67 a b c d e f))
          ((= nr 3) (syscall-direct 57 a b c d e f))
          ((= nr 5) (syscall-direct 80 a b c d e f))
          ((= nr 73) (syscall-direct 32 a b c d e f))
          ((= nr 74) (syscall-direct 82 a b c d e f))
          ((= nr 102) (syscall-direct 174 a b c d e f))
          ((= nr 86) (syscall-direct 37 -100 a -100 b 0 0))
          (t (error "Unknown native syscall vocabulary: %S" nr)))))

(defun nelisp-native-linux-page-bytes ()
  "Read AT_PAGESZ from this process's bounded auxv, never a cold-image value."
  (require 'nelisp-native-raw-file)
  (let ((offset 0) (page nil) (done nil))
    (while (and (not done) (< offset 4096))
      (let* ((pair (nelisp-native-raw-file-read "/proc/self/auxv" offset 16))
             (tag 0) (value 0) (i 7))
        (while (>= i 0)
          (setq tag (+ (* tag 256) (aref pair i))
                value (+ (* value 256) (aref pair (+ i 8))) i (- i 1)))
        (cond ((= tag 6) (setq page value done t)) ((= tag 0) (setq done t))))
      (setq offset (+ offset 16)))
    (unless (memq page '(4096 16384 65536)) (error "Linux AT_PAGESZ refused: %S" page))
    page))


(defun nelisp-native-linux--path-buffer (memory path)
  "Write a bounded UTF-8 PATH and its terminator to MEMORY."
  (let ((n (string-bytes path)))
    (unless (and (< 0 n 4096) (not (string-match-p "\0" path))) (error "Linux cache path refused"))
    (nelisp-native-load--poke-string memory 0 path)
    (ptr-write-u8 memory n 0)))

(defun nelisp-native-linux--directory-chain (directory scratch)
  "Open every DIRECTORY component without symlinks and pin all ancestors.
Return handles newest first. Unwind partial handles on any refusal."
  (let ((handles nil) (ok nil) (root-owner nil)
        (uid (nelisp-native-linux-syscall 102 0 0 0 0 0 0))
        (parts (split-string (expand-file-name directory) "/" t)))
    (unless (<= (length parts) 64) (error "Linux cache ancestor bound"))
    (unwind-protect
        (progn
          (nelisp-native-linux--path-buffer scratch "/")
          (let ((root (syscall-direct 56 -100 scratch 573440 0 0 0))) ; CLOEXEC|NOFOLLOW|DIRECTORY
            (unless (>= root 0) (error "Linux cache root open refused"))
            (push root handles)
            (unless (= 0 (nelisp-native-linux-syscall 5 root (+ scratch 4096) 0 0 0 0))
              (error "Linux cache root stat refused"))
            (unless (and (= (logand (ptr-read-u32 scratch 4112) 61440) 16384)
                         (zerop (logand (ptr-read-u32 scratch 4112) 18)))
              (error "Linux cache writable root refused"))
            ;; The pinned / is the filesystem trust anchor. User namespaces can
            ;; expose its administrator as an overflow UID rather than uid 0.
            ;; Derive this identity from that FD; never from a cache component.
            (setq root-owner (ptr-read-u32 scratch 4120)))
          (dolist (part parts)
            (nelisp-native-linux--path-buffer scratch part)
            (let ((fd (syscall-direct 56 (car handles) scratch 573440 0 0 0)))
              (unless (>= fd 0) (error "Linux cache ancestor open refused"))
              (push fd handles)
              (unless (= 0 (nelisp-native-linux-syscall 5 fd (+ scratch 4096) 0 0 0 0))
                (error "Linux cache ancestor stat refused"))
              (let ((mode (ptr-read-u32 scratch 4112)) (owner (ptr-read-u32 scratch 4120)))
                (unless (and (or (= owner root-owner) (= owner uid))
                             (or (zerop (logand mode 18))
                                 (and (= owner root-owner) (/= 0 (logand mode 512)))))
                  (error "Linux cache writable/foreign ancestor refused")))))
          (setq ok t) handles)
      (unless ok (dolist (fd handles) (nelisp-native-linux-syscall 3 fd 0 0 0 0 0))))))

(defun nelisp-native-linux-private-file-bytes (path)
  "Read one pinned private cache file snapshot under a shared advisory lock.
Do not follow symlinks; fstat before and after, with exact bounded pread."
  (let* ((scratch (nelisp-native-linux-syscall 9 0 8192 3 34 -1 0))
         (handles nil) (fd nil) (payload nil) (size nil))
    (unless (> scratch 0) (error "Linux cache scratch mmap failed"))
    (unwind-protect
        (progn
          (setq handles (nelisp-native-linux--directory-chain (file-name-directory path) scratch))
          (nelisp-native-linux--path-buffer scratch (file-name-nondirectory path))
          (setq fd (syscall-direct 56 (car handles) scratch 557056 0 0 0)) ; CLOEXEC|NOFOLLOW
          (unless (>= fd 0) (error "Linux private cache file open refused"))
          (unless (= 0 (nelisp-native-linux-syscall 73 fd 1 0 0 0 0)) (error "Linux cache lock refused"))
          (unless (= 0 (nelisp-native-linux-syscall 5 fd (+ scratch 4096) 0 0 0 0)) (error "Linux cache stat refused"))
          (setq size (ptr-read-u64 scratch 4144))
          (unless (and (<= 1 size 134217728)
                       (= (ptr-read-u32 scratch 4120) (nelisp-native-linux-syscall 102 0 0 0 0 0 0))
                       (= (logand (ptr-read-u32 scratch 4112) 61440) 32768)
                       (zerop (logand (ptr-read-u32 scratch 4112) 18)))
            (error "Linux private cache file owner/mode/size refused"))
          (let ((before (ptr-read-bytes (+ scratch 4096) 128)))
            (setq payload (nelisp-native-linux-syscall 9 0 size 3 34 -1 0))
            (unless (> payload 0) (error "Linux cache snapshot mmap failed"))
            (unless (= size (nelisp-native-linux-syscall 17 fd payload size 0 0 0)) (error "Linux cache snapshot truncated"))
            (unless (and (= 0 (nelisp-native-linux-syscall 5 fd (+ scratch 4096) 0 0 0 0))
                         (let ((after (ptr-read-bytes (+ scratch 4096) 128)))
                           (and (equal (substring before 0 72) (substring after 0 72))
                                (equal (substring before 88) (substring after 88)))))
              (error "Linux cache file identity changed during read"))
            (ptr-read-bytes payload size)))
      (when (and fd (>= fd 0)) (nelisp-native-linux-syscall 3 fd 0 0 0 0 0))
      (dolist (handle handles) (nelisp-native-linux-syscall 3 handle 0 0 0 0 0))
      (when (and payload (> payload 0)) (nelisp-native-linux-syscall 11 payload size 0 0 0 0))
      (nelisp-native-linux-syscall 11 scratch 8192 0 0 0 0))))


(defun nelisp-native-linux-temporary (prefix)
  "Exclusively create a mode-0600 temporary under pinned private ancestors."
  (let* ((scratch (nelisp-native-linux-syscall 9 0 8192 3 34 -1 0))
         (handles nil) (fd nil) (path nil) (attempts 0))
    (unless (> scratch 0) (error "Linux temporary scratch mapping failed"))
    (unwind-protect
        (progn
          (setq handles (nelisp-native-linux--directory-chain (file-name-directory prefix) scratch))
          (while (and (not path) (< attempts 100))
            (let ((candidate (make-temp-name prefix)))
              (nelisp-native-linux--path-buffer scratch (file-name-nondirectory candidate))
              (setq fd (syscall-direct 56 (car handles) scratch 557249 384 0 0))
              (cond ((>= fd 0) (setq path candidate))
                    ((/= fd -17) (error "Linux exclusive temporary creation refused"))))
            (setq attempts (+ attempts 1)))
          (or path (error "Linux temporary collision bound exceeded")))
      (when (and fd (>= fd 0)) (nelisp-native-linux-syscall 3 fd 0 0 0 0 0))
      (dolist (handle handles) (nelisp-native-linux-syscall 3 handle 0 0 0 0 0))
      (nelisp-native-linux-syscall 11 scratch 8192 0 0 0 0))))
(provide 'nelisp-native-linux)
