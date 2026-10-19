;;; standalone-abi-io-word-check.el --- Socket WORD I/O using pipes -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Exercise the socket producers' real byte I/O without opening a network
;; socket. Linux socket send/recv use the ordinary write/read fd helpers.
(let* ((fds (alloc-bytes 8 8))
       (rc (syscall-direct 22 fds 0 0 0 0 0)))
  (unless (= rc 0) (error "pipe failed: %S" rc))
  (let ((r (ptr-read-u32 fds 0)) (w (ptr-read-u32 fds 4)))
    (unwind-protect
        (progn
          (when (nelisp-socket-poll r nil 0) (error "empty pipe ready"))
          (unless (= (nelisp-socket-send w (unibyte-string 65 0 255 66)) 4)
            (error "send did not return the byte count WORD"))
          (unless (nelisp-socket-poll r nil 0) (error "filled pipe not ready"))
          (let ((value (nelisp-socket-recv r 4)))
            (unless (and (= (length value) 4) (= (string-bytes value) 4)
                         (= (aref value 0) 65) (= (aref value 1) 0)
                         (= (aref value 2) 255) (= (aref value 3) 66)
                         (not (multibyte-string-p value)))
              (error "recv corrupted unibyte payload: %S" value)))
          (unless (null (nelisp-socket-close w)) (error "close not nil"))
          (setq w -1)
          (unless (= (length (nelisp-socket-recv r 4)) 0) (error "EOF not empty string"))
          (dolist (form '((nelisp-socket-send -1 "x") (nelisp-socket-recv -1 1)))
            (unless (eq (condition-case e (progn (eval form) 'no-error)
                          (error (car e))) 'nelisp-socket-error)
              (error "fd error lost its condition: %S" form)))
          (princ "ABI-IO-WORD checks=9 PASS\n"))
      (when (>= w 0) (nelisp-socket-close w))
      (nelisp-socket-close r))))
