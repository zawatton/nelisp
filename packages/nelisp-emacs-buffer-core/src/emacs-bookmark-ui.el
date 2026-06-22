;;; emacs-bookmark-ui.el --- Shared bookmark UI helpers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Frontend-neutral helpers for bookmark completion and display text.
;; The owning frontend keeps storage, buffer switching, and prompt wiring;
;; this module owns reusable bookmark presentation semantics.

;;; Code:

(defun emacs-bookmark-ui--bookmark-name (cell)
  "Return bookmark name from CELL."
  (car cell))

(defun emacs-bookmark-ui--bookmark-buffer (cell)
  "Return bookmark buffer name from CELL."
  (plist-get (cdr cell) :buffer))

(defun emacs-bookmark-ui--bookmark-position (cell)
  "Return bookmark position from CELL."
  (plist-get (cdr cell) :pos))

(defun emacs-bookmark-ui-sorted (bookmarks)
  "Return BOOKMARKS sorted alphabetically by bookmark name."
  (sort (copy-sequence bookmarks)
        (lambda (a b)
          (string< (emacs-bookmark-ui--bookmark-name a)
                   (emacs-bookmark-ui--bookmark-name b)))))

(defun emacs-bookmark-ui-completion-candidates (bookmarks input)
  "Return sorted bookmark names from BOOKMARKS starting with INPUT."
  (let ((prefix (or input ""))
        candidates)
    (dolist (cell bookmarks)
      (let ((name (emacs-bookmark-ui--bookmark-name cell)))
        (when (and (stringp name)
                   (string-prefix-p prefix name))
          (push name candidates))))
    (sort candidates #'string<)))

(defun emacs-bookmark-ui-listing (bookmarks)
  "Return frontend-neutral listing data for BOOKMARKS.
The result plist contains `:entries', `:count', and `:text'.  Text
matches the simple GTK/TUI bookmark list format."
  (let ((entries (emacs-bookmark-ui-sorted bookmarks)))
    (list
     :entries entries
     :count (length entries)
     :text
     (if (null entries)
         "No bookmarks set.\n"
       (let ((text "Bookmarks:\n\n"))
         (dolist (cell entries)
           (setq text
                 (concat text
                         (format "  %-30s -> %s:%d\n"
                                 (emacs-bookmark-ui--bookmark-name cell)
                                 (emacs-bookmark-ui--bookmark-buffer cell)
                                 (emacs-bookmark-ui--bookmark-position cell)))))
         text)))))

(defun emacs-bookmark-ui-jump-plan (bookmarks input &optional buffer-exists-p)
  "Return a frontend-neutral bookmark jump plan.
BOOKMARKS is an alist whose values carry `:buffer' and `:pos'.  INPUT is
the requested bookmark name.  BUFFER-EXISTS-P, when non-nil, is called
with the target buffer name; otherwise `get-buffer' is used.

The result plist always carries `:status' and `:message'.  Successful
plans carry `:bookmark', `:buffer-name', and `:point'."
  (cond
   ((null bookmarks)
    (list :status 'no-bookmarks
          :message "bookmark-jump: no bookmarks"))
   (t
    (let ((cell (assoc input bookmarks)))
      (cond
       ((null cell)
        (list :status 'missing
              :bookmark input
              :message (format "bookmark-jump: %s not found" input)))
       (t
        (let* ((bn (emacs-bookmark-ui--bookmark-buffer cell))
               (pos (emacs-bookmark-ui--bookmark-position cell))
               (exists-p (or buffer-exists-p #'get-buffer)))
          (cond
           ((not (funcall exists-p bn))
            (list :status 'buffer-missing
                  :bookmark input
                  :buffer-name bn
                  :point pos
                  :message (format "bookmark-jump: buffer %s gone" bn)))
           (t
            (list :status 'ok
                  :bookmark input
                  :buffer-name bn
                  :point pos
                  :message (format "bookmark-jump: %s -> %s:%d"
                                   input bn pos)))))))))))

(provide 'emacs-bookmark-ui)

;;; emacs-bookmark-ui.el ends here
