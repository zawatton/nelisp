;;; process-parity-cases.el --- process behaviour cases for GNU vs NeLisp  -*- lexical-binding: t -*-
;; Each case prints one `CASE NAME RESULT' row.  Cases wait for process exit
;; and compare final buffer text/status, never timing.  Loaded by
;; process-parity.sh on host `emacs -Q --batch' and on the standalone after
;; build/nemacs-bootstrap.el.


(defun pp--wait (proc)
  "Wait until PROC exits, then drain pending output."
  (let ((n 0))
    (while (and (process-live-p proc) (< n 100))
      (accept-process-output proc 0.1)
      (setq n (1+ n))))
  (accept-process-output proc 0.1)
  (accept-process-output proc 0.1))

(defvar pp--only nil)

(defmacro pp--case (name &rest body)
  `(when (or (null pp--only) (equal pp--only ,name))
     (let ((res (condition-case err (progn ,@body)
                  (error (list 'error (car err))))))
       (let ((print-escape-newlines t)) (princ (format "CASE %s %S\n" ,name res))))))

(defun pp--buf () (generate-new-buffer "pp"))

(pp--case "default-filter"
  (let* ((b (pp--buf))
         (p (make-process :name "pb" :command '("printf" "buf") :buffer b)))
    (pp--wait p)
    (list (with-current-buffer b (buffer-string))
          (process-filter p))))

(pp--case "default-filter-mark"
  (let* ((b (pp--buf))
         (p (make-process :name "pm" :command '("printf" "abcdef") :buffer b
                          :sentinel #'ignore)))
    (pp--wait p)
    (list (marker-position (process-mark p))
          (with-current-buffer b (point)))))

(pp--case "set-filter-nil"
  (let* ((b (pp--buf))
         (p (make-process :name "pn" :command '("printf" "xy") :buffer b
                          :filter #'ignore)))
    (set-process-filter p nil)
    (pp--wait p)
    (list (process-filter p) (with-current-buffer b (buffer-string)))))

(pp--case "custom-filter"
  (let* ((acc "")
         (p (make-process :name "pc" :command '("printf" "hello")
                          :filter (lambda (_p s) (setq acc (concat acc s))))))
    (pp--wait p)
    acc))

(pp--case "filter-getter"
  (let* ((f (lambda (_p _s) nil))
         (p (make-process :name "pg" :command '("true") :filter f)))
    (pp--wait p)
    (eq (process-filter p) f)))

(pp--case "sentinel-default"
  (let* ((b (pp--buf))
         (p (make-process :name "sd" :command '("true") :buffer b)))
    (pp--wait p)
    (with-current-buffer b (buffer-string))))

(pp--case "sentinel-custom"
  (let* ((ev nil)
         (p (make-process :name "sc" :command '("true")
                          :sentinel (lambda (_p e) (push e ev)))))
    (pp--wait p)
    ev))

(pp--case "status-exit"
  (let ((p (make-process :name "se" :command '("sh" "-c" "exit 3"))))
    (pp--wait p)
    (list (process-status p) (process-exit-status p) (if (process-live-p p) (quote live) (quote dead)))))

(pp--case "status-run"
  (let ((p (make-process :name "sr" :command '("sleep" "5"))))
    (prog1 (list (process-status p) (if (process-live-p p) (quote live) (quote dead)))
      (delete-process p))))

(pp--case "send-string-cat"
  (let* ((b (pp--buf))
         (p (make-process :name "cat" :command '("cat") :buffer b)))
    (process-send-string p "ping\n")
    (process-send-eof p)
    (pp--wait p)
    (with-current-buffer b (buffer-string))))

(pp--case "delete-process"
  (let ((p (make-process :name "dp" :command '("sleep" "5"))))
    (delete-process p)
    ;; Exact GNU value: a deleted child reports `signal'.
    (list (if (process-live-p p) (quote live) (quote dead))
          (process-status p))))

(pp--case "accept-timeout"
  (let ((p (make-process :name "at" :command '("sleep" "5"))))
    (prog1 (accept-process-output p 0.05)
      (delete-process p))))

(pp--case "stderr-buffer"
  (let* ((b (pp--buf)) (e (pp--buf))
         (p (make-process :name "er" :buffer b :stderr e
                          :command '("sh" "-c" "echo out; echo err >&2"))))
    (pp--wait p)
    ;; Exact GNU text, including the status lines GNU inserts.
    (list (with-current-buffer b (buffer-string))
          (with-current-buffer e (buffer-string)))))

(pp--case "start-process"
  (let* ((b (pp--buf))
         (p (start-process "sp" b "printf" "a" )))
    (pp--wait p)
    (list (process-name p) (process-command p)
          (eq (process-buffer p) b)
          (with-current-buffer b (buffer-string)))))

(pp--case "process-buffer-none"
  (let ((p (make-process :name "nb" :command '("true"))))
    (pp--wait p)
    (list (process-buffer p) (processp p))))
