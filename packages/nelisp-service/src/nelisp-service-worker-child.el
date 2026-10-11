;;; nelisp-service-worker-child.el --- Worker side of the NeLisp worker protocol -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 213 §4.2.  A worker is a NeLisp (or host Emacs) process that reads
;; requests from stdin and writes replies to stdout, one framed object per
;; line (see `nelisp-service-encode'):
;;
;;   worker -> pool   (ready)                 once, before the first request
;;   pool -> worker   (req ID FORM)           evaluate FORM
;;   worker -> pool   (res ID ok VALUE)       FORM returned VALUE
;;   worker -> pool   (res ID error MESSAGE)  FORM signalled
;;   pool -> worker   (quit)                  finish and exit
;;
;; End of file on stdin also ends the loop, so a worker whose pool died
;; exits on its own instead of lingering.  The entry script
;; `bin/nelisp-service-worker-main.el' loads this file and calls
;; `nelisp-service-worker-child-main'.

;;; Code:

(require 'nelisp-service)

(defun nelisp-service-worker-child--readable (value)
  "Return VALUE when its printed form reads back as VALUE, else that string.
Equality, not just the absence of a read error, is required: the
standalone reader accepts `#<buffer x>' without signalling (measured
2026-10-10), and a reply the pool cannot decode would never complete."
  (let* ((printed (prin1-to-string value))
         (back (condition-case nil
                   (read-from-string printed)
                 (error nil))))
    (if (and back (equal (car back) value)) value printed)))

(defun nelisp-service-worker-child-handle (message)
  "Return the reply object for request MESSAGE, or nil when none is due."
  (when (and (consp message) (eq (car message) 'req))
    (let ((id (nth 1 message))
          (form (nth 2 message)))
      (condition-case err
          (list 'res id 'ok
                (nelisp-service-worker-child--readable (eval form t)))
        (error (list 'res id 'error (error-message-string err)))))))

(defun nelisp-service-worker-child-main ()
  "Serve requests from stdin until `(quit)' or end of file.  Return 0."
  (nelisp-service-setup-stdio)
  (nelisp-service-write-stdout (nelisp-service-encode '(ready)))
  (let ((running t))
    (while running
      (let ((message (nelisp-service-read-stdin-message)))
        (if (eq message :eof)
            (setq running nil)
          (progn
            (if (equal message '(quit))
                (setq running nil)
              (let ((reply (nelisp-service-worker-child-handle message)))
                (when reply
                  (nelisp-service-write-stdout
                   (nelisp-service-encode reply))))))))))
  0)

(provide 'nelisp-service-worker-child)

;;; nelisp-service-worker-child.el ends here
