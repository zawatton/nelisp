;;; 14-phase5c-primitive-gap.data.el -- raw audit -*- lexical-binding: t; -*-
;; Generated: 2026-04-24 18:48:17+0900
;; Scope files: 3
;; MISSING host subrs (add to nelisp--primitive-symbols): 25
;; COVERED host subrs (already in table): 11

;;; Code:

(defconst phase5c-primitive-missing
  '(
    (accept-process-output :arity (0 . 4) :sites (("docs/design/14-forecast/nelisp-process-sketch.el" . 72)))
    (assq-delete-all :arity (2 . 2) :sites (("docs/design/14-forecast/nelisp-file-notify-sketch.el" . 29)))
    (buffer-substring-no-properties :arity (2 . 2) :sites (("docs/design/14-forecast/nelisp-network-sketch.el" . 52) ("docs/design/14-forecast/nelisp-network-sketch.el" . 67)))
    (delete-file :arity (1 . 2) :sites (("docs/design/14-forecast/nelisp-file-notify-sketch.el" . 51)))
    (delete-process :arity (0 . 1) :sites (("docs/design/14-forecast/nelisp-network-sketch.el" . 41) ("docs/design/14-forecast/nelisp-process-sketch.el" . 58)))
    (file-attributes :arity (1 . 2) :sites (("docs/design/14-forecast/nelisp-file-notify-sketch.el" . 40)))
    (file-directory-p :arity (1 . 1) :sites (("docs/design/14-forecast/nelisp-file-notify-sketch.el" . 48)))
    (file-exists-p :arity (1 . 1) :sites (("docs/design/14-forecast/nelisp-file-notify-sketch.el" . 45)))
    (goto-char :arity (1 . 1) :sites (("docs/design/14-forecast/nelisp-network-sketch.el" . 52) ("docs/design/14-forecast/nelisp-network-sketch.el" . 67)))
    (kill-process :arity (0 . 2) :sites (("docs/design/14-forecast/nelisp-process-sketch.el" . 55)))
    (make-network-process :arity (0 . many) :sites (("docs/design/14-forecast/nelisp-network-sketch.el" . 15)))
    (make-process :arity (0 . many) :sites (("docs/design/14-forecast/nelisp-process-sketch.el" . 17)))
    (point :arity (0 . 0) :sites (("docs/design/14-forecast/nelisp-network-sketch.el" . 52)))
    (point-max :arity (0 . 0) :sites (("docs/design/14-forecast/nelisp-network-sketch.el" . 52) ("docs/design/14-forecast/nelisp-network-sketch.el" . 67)))
    (point-min :arity (0 . 0) :sites (("docs/design/14-forecast/nelisp-network-sketch.el" . 52) ("docs/design/14-forecast/nelisp-network-sketch.el" . 67)))
    (process-command :arity (1 . 1) :sites (("docs/design/14-forecast/nelisp-process-sketch.el" . 81)))
    (process-exit-status :arity (1 . 1) :sites (("docs/design/14-forecast/nelisp-process-sketch.el" . 52)))
    (process-id :arity (1 . 1) :sites (("docs/design/14-forecast/nelisp-process-sketch.el" . 75)))
    (process-live-p :arity (1 . 1) :sites (("docs/design/14-forecast/nelisp-network-sketch.el" . 46) ("docs/design/14-forecast/nelisp-process-sketch.el" . 46)))
    (process-name :arity (1 . 1) :sites (("docs/design/14-forecast/nelisp-process-sketch.el" . 78)))
    (process-send-eof :arity (0 . 1) :sites (("docs/design/14-forecast/nelisp-process-sketch.el" . 43)))
    (process-send-string :arity (2 . 2) :sites (("docs/design/14-forecast/nelisp-network-sketch.el" . 38) ("docs/design/14-forecast/nelisp-process-sketch.el" . 40)))
    (process-status :arity (1 . 1) :sites (("docs/design/14-forecast/nelisp-process-sketch.el" . 49)))
    (re-search-forward :arity (1 . 4) :sites (("docs/design/14-forecast/nelisp-network-sketch.el" . 52)))
    (rename-file :arity (2 . 3) :sites (("docs/design/14-forecast/nelisp-file-notify-sketch.el" . 54)))
    ))

(defconst phase5c-primitive-covered
  '(
    (1+ :sites (("docs/design/14-forecast/nelisp-network-sketch.el" . 52)))
    (car :sites (("docs/design/14-forecast/nelisp-file-notify-sketch.el" . 37)))
    (cons :sites (("docs/design/14-forecast/nelisp-file-notify-sketch.el" . 9) ("docs/design/14-forecast/nelisp-network-sketch.el" . 52)))
    (copy-sequence :sites (("docs/design/14-forecast/nelisp-network-sketch.el" . 49) ("docs/design/14-forecast/nelisp-process-sketch.el" . 62)))
    (delq :sites (("docs/design/14-forecast/nelisp-network-sketch.el" . 41) ("docs/design/14-forecast/nelisp-process-sketch.el" . 58)))
    (funcall :sites (("docs/design/14-forecast/nelisp-process-sketch.el" . 17) ("docs/design/14-forecast/nelisp-process-sketch.el" . 17)))
    (mapcar :sites (("docs/design/14-forecast/nelisp-file-notify-sketch.el" . 37)))
    (plist-get :sites (("docs/design/14-forecast/nelisp-network-sketch.el" . 15) ("docs/design/14-forecast/nelisp-network-sketch.el" . 15)))
    (plist-put :sites (("docs/design/14-forecast/nelisp-process-sketch.el" . 68)))
    (provide :sites (("docs/design/14-forecast/nelisp-file-notify-sketch.el" . 57) ("docs/design/14-forecast/nelisp-network-sketch.el" . 74)))
    (require :sites (("docs/design/14-forecast/nelisp-file-notify-sketch.el" . 1) ("docs/design/14-forecast/nelisp-file-notify-sketch.el" . 5)))
    ))

(provide '14-phase5c-primitive-gap)
;;; 14-phase5c-primitive-gap.data.el ends here
