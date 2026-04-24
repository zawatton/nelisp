;;; 16-phase5e-primitive-gap.data.el -- raw audit -*- lexical-binding: t; -*-
;; Generated: 2026-04-24 22:42:07+0900
;; Scope files: 1
;; MISSING host subrs (add to nelisp--primitive-symbols): 11
;; COVERED host subrs (already in table): 34

;;; Code:

(defconst phase5e-primitive-missing
  '(
    (alist-get :arity (2 . 5) :sites (("docs/design/16-forecast/nelisp-server-sketch.el" . 59) ("docs/design/16-forecast/nelisp-server-sketch.el" . 103)))
    (buffer-string :arity (0 . 0) :sites (("docs/design/16-forecast/nelisp-server-sketch.el" . 161) ("docs/design/16-forecast/nelisp-server-sketch.el" . 207)))
    (file-name-extension :arity (1 . 2) :sites (("docs/design/16-forecast/nelisp-server-sketch.el" . 173)))
    (generate-new-buffer :arity (1 . 2) :sites (("docs/design/16-forecast/nelisp-server-sketch.el" . 207)))
    (insert-file-contents :arity (1 . 5) :sites (("docs/design/16-forecast/nelisp-server-sketch.el" . 161) ("docs/design/16-forecast/nelisp-server-sketch.el" . 173)))
    (kill-buffer :arity (0 . 1) :sites (("docs/design/16-forecast/nelisp-server-sketch.el" . 207) ("docs/design/16-forecast/nelisp-server-sketch.el" . 227)))
    (line-number-at-pos :arity (0 . 2) :sites (("docs/design/16-forecast/nelisp-server-sketch.el" . 173) ("docs/design/16-forecast/nelisp-server-sketch.el" . 173)))
    (match-string :arity (1 . 2) :sites (("docs/design/16-forecast/nelisp-server-sketch.el" . 173) ("docs/design/16-forecast/nelisp-server-sketch.el" . 173)))
    (princ :arity (1 . 2) :sites (("docs/design/16-forecast/nelisp-server-sketch.el" . 123)))
    (read-from-minibuffer :arity (1 . 7) :sites (("docs/design/16-forecast/nelisp-server-sketch.el" . 123)))
    (terpri :arity (0 . 2) :sites (("docs/design/16-forecast/nelisp-server-sketch.el" . 123)))
    ))

(defconst phase5e-primitive-covered
  '(
    (accept-process-output :sites (("docs/design/16-forecast/nelisp-server-sketch.el" . 207) ("docs/design/16-forecast/nelisp-server-sketch.el" . 227)))
    (cadr :sites (("docs/design/16-forecast/nelisp-server-sketch.el" . 88)))
    (cddr :sites (("docs/design/16-forecast/nelisp-server-sketch.el" . 88)))
    (cdr :sites (("docs/design/16-forecast/nelisp-server-sketch.el" . 279)))
    (clrhash :sites (("docs/design/16-forecast/nelisp-server-sketch.el" . 25) ("docs/design/16-forecast/nelisp-server-sketch.el" . 33)))
    (concat :sites (("docs/design/16-forecast/nelisp-server-sketch.el" . 252) ("docs/design/16-forecast/nelisp-server-sketch.el" . 263)))
    (cons :sites (("docs/design/16-forecast/nelisp-server-sketch.el" . 88) ("docs/design/16-forecast/nelisp-server-sketch.el" . 103)))
    (error :sites (("docs/design/16-forecast/nelisp-server-sketch.el" . 59) ("docs/design/16-forecast/nelisp-server-sketch.el" . 144)))
    (format :sites (("docs/design/16-forecast/nelisp-server-sketch.el" . 59) ("docs/design/16-forecast/nelisp-server-sketch.el" . 59)))
    (funcall :sites (("docs/design/16-forecast/nelisp-server-sketch.el" . 79)))
    (gethash :sites (("docs/design/16-forecast/nelisp-server-sketch.el" . 59) ("docs/design/16-forecast/nelisp-server-sketch.el" . 144)))
    (goto-char :sites (("docs/design/16-forecast/nelisp-server-sketch.el" . 173)))
    (intern :sites (("docs/design/16-forecast/nelisp-server-sketch.el" . 88) ("docs/design/16-forecast/nelisp-server-sketch.el" . 252)))
    (length :sites (("docs/design/16-forecast/nelisp-server-sketch.el" . 173) ("docs/design/16-forecast/nelisp-server-sketch.el" . 173)))
    (list :sites (("docs/design/16-forecast/nelisp-server-sketch.el" . 25) ("docs/design/16-forecast/nelisp-server-sketch.el" . 40)))
    (make-hash-table :sites (("docs/design/16-forecast/nelisp-server-sketch.el" . 23) ("docs/design/16-forecast/nelisp-server-sketch.el" . 24)))
    (make-process :sites (("docs/design/16-forecast/nelisp-server-sketch.el" . 207)))
    (mapcar :sites (("docs/design/16-forecast/nelisp-server-sketch.el" . 88) ("docs/design/16-forecast/nelisp-server-sketch.el" . 263)))
    (maphash :sites (("docs/design/16-forecast/nelisp-server-sketch.el" . 48)))
    (not :sites (("docs/design/16-forecast/nelisp-server-sketch.el" . 123)))
    (nreverse :sites (("docs/design/16-forecast/nelisp-server-sketch.el" . 48) ("docs/design/16-forecast/nelisp-server-sketch.el" . 88)))
    (plist-get :sites (("docs/design/16-forecast/nelisp-server-sketch.el" . 48) ("docs/design/16-forecast/nelisp-server-sketch.el" . 48)))
    (plist-put :sites (("docs/design/16-forecast/nelisp-server-sketch.el" . 279)))
    (point-min :sites (("docs/design/16-forecast/nelisp-server-sketch.el" . 173)))
    (process-exit-status :sites (("docs/design/16-forecast/nelisp-server-sketch.el" . 207) ("docs/design/16-forecast/nelisp-server-sketch.el" . 227)))
    (process-live-p :sites (("docs/design/16-forecast/nelisp-server-sketch.el" . 207) ("docs/design/16-forecast/nelisp-server-sketch.el" . 227)))
    (provide :sites (("docs/design/16-forecast/nelisp-server-sketch.el" . 289)))
    (puthash :sites (("docs/design/16-forecast/nelisp-server-sketch.el" . 37) ("docs/design/16-forecast/nelisp-server-sketch.el" . 144)))
    (re-search-forward :sites (("docs/design/16-forecast/nelisp-server-sketch.el" . 173) ("docs/design/16-forecast/nelisp-server-sketch.el" . 173)))
    (require :sites (("docs/design/16-forecast/nelisp-server-sketch.el" . 1) ("docs/design/16-forecast/nelisp-server-sketch.el" . 18)))
    (split-string :sites (("docs/design/16-forecast/nelisp-server-sketch.el" . 252) ("docs/design/16-forecast/nelisp-server-sketch.el" . 263)))
    (stringp :sites (("docs/design/16-forecast/nelisp-server-sketch.el" . 123)))
    (substring :sites (("docs/design/16-forecast/nelisp-server-sketch.el" . 88)))
    (symbol-name :sites (("docs/design/16-forecast/nelisp-server-sketch.el" . 88) ("docs/design/16-forecast/nelisp-server-sketch.el" . 144)))
    ))

(provide '16-phase5e-primitive-gap)
;;; 16-phase5e-primitive-gap.data.el ends here
