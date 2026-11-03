;;; emacs-undo.el --- Undo subsystem (Track E.2)  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Track E.2 (2026-05-03) — Layer 2.
;;
;; Substrate for `buffer-undo-list', `undo-boundary',
;; `primitive-undo', and the user-facing `undo' command.  Records
;; are appended by the editing-command bridge (`emacs-edit-builtins')
;; via `emacs-undo-record-insert' / `emacs-undo-record-delete' guards.
;;
;; Storage: per-buffer state lives in an alist
;; `emacs-undo--lists' keyed by `nelisp-ec-buffer' record.  This
;; keeps the upstream struct unmodified (= same approach Track D
;; took for `buffer-file-name').
;;
;; Record shapes accepted by `primitive-undo':
;;
;;   nil                — undo-group boundary
;;   (BEG . END)        — text was inserted spanning [BEG,END);
;;                         undo by `delete-region BEG END'
;;   (STRING . POS)     — text was deleted at POS;
;;                         undo by `goto-char POS' + `insert STRING'
;;
;; Out of scope (= deferred): marker records, text-property records,
;; `(apply ...)' records, modtime stamps.  The tests that survive
;; these omissions exercise the insertion / deletion roundtrip
;; pathways which cover the hot path of self-insert + kill +
;; delete-backward-char + yank.

;;; Code:

(require 'cl-lib)
(require 'nelisp-emacs-compat)

(define-error 'emacs-undo-error "Undo error")

;;;; --- per-buffer undo-list storage ----------------------------------

(defvar emacs-undo--lists nil
  "Alist (BUFFER-RECORD . UNDO-LIST).
The cdr is either a list of records (nil = boundary, cons-cells =
records) OR the symbol `t' (= recording disabled).")

(defun emacs-undo--current-buffer ()
  "Return the current `nelisp-ec' buffer record, or nil."
  (cond
   ((fboundp 'nelisp-ec--ensure-current)
    (nelisp-ec--ensure-current))
   ((fboundp 'nelisp-ec-current-buffer)
    (nelisp-ec-current-buffer))
   (t nil)))

(defun emacs-undo--clean-killed ()
  "Drop alist entries whose buffer was killed."
  (setq emacs-undo--lists
        (cl-remove-if (lambda (cell)
                        (let ((b (car cell)))
                          (or (null b)
                              (and (fboundp 'nelisp-ec-buffer-killed-p)
                                   (nelisp-ec-buffer-killed-p b)))))
                      emacs-undo--lists)))

(defun emacs-undo--cell-for (buf)
  "Return the alist cell for BUF, creating it with empty list if absent."
  (or (assq buf emacs-undo--lists)
      (let ((c (cons buf nil)))
        (push c emacs-undo--lists)
        c)))

(defun emacs-undo-buffer-undo-list ()
  "Return the current buffer's undo list (= the cdr of its alist cell)."
  (let ((buf (emacs-undo--current-buffer)))
    (when buf
      (cdr (emacs-undo--cell-for buf)))))

(defun emacs-undo-set-buffer-undo-list (lst)
  "Replace the current buffer's undo list with LST.
LST may be nil, a list of records, or `t' (= disable recording)."
  (let* ((buf (emacs-undo--current-buffer))
         (cell (and buf (emacs-undo--cell-for buf))))
    (when cell
      (setcdr cell lst)))
  lst)

(defun emacs-undo-disabled-p ()
  "Return non-nil when recording is disabled for the current buffer."
  (eq t (emacs-undo-buffer-undo-list)))

(defun emacs-undo-reset ()
  "Drop the entire alist (= for tests)."
  (setq emacs-undo--lists nil))

;;;; --- public API: undo-boundary + record helpers --------------------

(defun emacs-undo-undo-boundary ()
  "Append a boundary (nil) to the current buffer's undo list.

Idempotent: if the list head is already nil, a second boundary is
NOT pushed (= matches Emacs behaviour where consecutive boundaries
collapse)."
  (unless (emacs-undo-disabled-p)
    (let ((lst (emacs-undo-buffer-undo-list)))
      (unless (and (consp lst) (null (car lst)))
        (emacs-undo-set-buffer-undo-list (cons nil lst)))))
  nil)

(defun emacs-undo-record-insert (beg end)
  "Record an inserted span [BEG, END) on the current buffer's undo list.
Adjacent insertion records are coalesced by extending the list head.
The inverse operation is `delete-region BEG END'."
  (unless (or (emacs-undo-disabled-p) (= beg end))
    (let* ((lst (emacs-undo-buffer-undo-list))
           (head (and (consp lst) (car lst))))
      (if (and (consp head)
               (integerp (car head))
               (integerp (cdr head))
               (= (cdr head) beg))
          (setcdr head end)
        (emacs-undo-set-buffer-undo-list
         (cons (cons beg end) lst)))))
  nil)

(defun emacs-undo-record-delete (string pos)
  "Push (STRING . POS) onto the current buffer's undo list.
Recording the fact that STRING was just deleted from POS; the
inverse operation is `goto-char POS' + `insert STRING'."
  (unless (or (emacs-undo-disabled-p) (zerop (length string)))
    (emacs-undo-set-buffer-undo-list
     (cons (cons string pos) (emacs-undo-buffer-undo-list))))
  nil)

;;;; --- primitive-undo / undo --------------------------------------

(defun emacs-undo--apply-record (r)
  "Apply one undo record R to the current buffer.
Recognised shapes: (BEG . END), (STRING . POS).  Other shapes are
silently ignored (= MVP)."
  (cond
   ((and (consp r) (integerp (car r)) (integerp (cdr r)))
    (nelisp-ec-delete-region (car r) (cdr r))
    (nelisp-ec-goto-char (car r)))
   ((and (consp r) (stringp (car r)) (integerp (cdr r)))
    (nelisp-ec-goto-char (cdr r))
    (nelisp-ec-insert (car r))
    (nelisp-ec-goto-char (cdr r)))
   (t nil)))

(defun emacs-undo-primitive-undo (count list)
  "Apply COUNT undo-groups starting at the head of LIST.
A boundary (= nil entry) ends a group.  Returns the unconsumed
tail of LIST.  Records are applied without reading the
buffer-undo-list directly so callers can pass an arbitrary tail
(= matches Emacs `primitive-undo' contract)."
  (let ((rest list)
        (groups-done 0))
    (while (and rest (< groups-done count))
      (let ((r (car rest)))
        (cond
         ((null r)
          (setq groups-done (1+ groups-done))
          (setq rest (cdr rest)))
         (t
          (emacs-undo--apply-record r)
          (setq rest (cdr rest))))))
    rest))

(defun emacs-undo-undo (&optional arg)
  "Phase E.2 MVP: undo one undo-group on the current buffer.

ARG (= prefix) accepted for API parity but currently ignored —
each call undoes exactly one group.

Skips leading boundaries, then runs `primitive-undo' for one group
and writes back the remaining tail with a fresh boundary on top so
the next `undo' starts a clean group."
  (interactive "*P")
  (ignore arg)
  (when (emacs-undo-disabled-p)
    (signal 'emacs-undo-error '(buffer-undo-list-disabled)))
  (let ((lst (emacs-undo-buffer-undo-list)))
    ;; Skip leading boundaries.
    (while (and lst (null (car lst)))
      (setq lst (cdr lst)))
    (when (null lst)
      (signal 'emacs-undo-error '(no-further-undo-information)))
    (let ((remaining (emacs-undo-primitive-undo 1 lst)))
      (emacs-undo-set-buffer-undo-list (cons nil remaining))))
  nil)

(defun emacs-undo-undo-direct (&optional arg)
  "Undo ARG groups and return a frontend-neutral result plist.
The result contains `:status' and `:message'.  `:status' is `ok' or
`error'.  Frontends can display `:message' directly while keeping undo
error normalization in this shared substrate layer."
  (condition-case err
      (progn
        (emacs-undo-undo arg)
        (list :status 'ok
              :message "undo"))
    (emacs-undo-error
     (let ((reason (or (cadr err) (car err))))
       (list :status 'error
             :condition (car err)
             :data (cdr err)
             :message (format "undo: %s" reason))))
    (error
     (let ((reason (or (cadr err) (car err))))
       (list :status 'error
	     :condition (car err)
	     :data (cdr err)
	     :message (format "undo: %s" reason))))))

(defun emacs-undo-run-command (&rest plist)
  "Run a frontend undo command through the shared undo API.
PLIST accepts `:current-buffer', `:arg', `:status-function', and
`:after-success'.  The result plist from `emacs-undo-undo-direct' is
returned."
  (let* ((current-buffer-function (plist-get plist :current-buffer))
         (arg (plist-get plist :arg))
         (status-function (plist-get plist :status-function))
         (after-success (plist-get plist :after-success))
         (buffer (if current-buffer-function
                     (funcall current-buffer-function)
                   (current-buffer)))
         (result (with-current-buffer buffer
                   (emacs-undo-undo-direct arg))))
    (when (and after-success (eq 'ok (plist-get result :status)))
      (funcall after-success result))
    (when status-function
      (funcall status-function (plist-get result :message)))
    result))

(provide 'emacs-undo)

;;; emacs-undo.el ends here
