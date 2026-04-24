;;; nelisp-worker-child.el --- NeLisp worker child loop -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 5-D.1 per Doc 15.  Entry point for the `emacs --batch'
;; subprocess spawned by `nelisp-worker-pool-create'.
;;
;; Wire protocol (line-delimited sexps on stdin/stdout):
;;
;;   request : (ID . EXPR)          ID is a string, EXPR is a form
;;   reply   : (ID :ok  RESULT)     success, RESULT is a value
;;           | (ID :error MESSAGE)  evaluation failed, MESSAGE is string
;;
;; A well-formed line that fails to parse emits no reply; the parent
;; relies on `timeout' for wedge detection.  Errors inside EXPR are
;; caught and surfaced as `:error'.
;;
;; The child reads from stdin via `read-from-minibuffer', which in
;; `emacs --batch' semantics returns one line at a time (stripping the
;; terminator).  End-of-input returns nil and terminates the loop.

;;; Code:

(defun nelisp-worker-child--emit (obj)
  "Print OBJ in readable form followed by a newline, flush."
  (princ (prin1-to-string obj))
  (princ "\n"))

(defun nelisp-worker-child--handle (line)
  "Parse LINE as (ID . EXPR) and emit the reply."
  (let ((parsed (condition-case _
                    (car (read-from-string line))
                  (error nil))))
    (when (and (consp parsed) (stringp (car parsed)))
      (let ((id (car parsed))
            (expr (cdr parsed)))
        (condition-case err
            (let ((result (eval expr t)))
              (nelisp-worker-child--emit (list id :ok result)))
          (error
           (nelisp-worker-child--emit
            (list id :error (format "%S" err)))))))))

(defun nelisp-worker-child-loop ()
  "Read request lines from stdin forever; emit replies to stdout."
  (let ((keep-going t))
    (while keep-going
      (let ((line (ignore-errors (read-from-minibuffer ""))))
        (cond
         ((null line) (setq keep-going nil))
         ((string-empty-p line) nil)
         (t (nelisp-worker-child--handle line)))))))

(provide 'nelisp-worker-child)
;;; nelisp-worker-child.el ends here
