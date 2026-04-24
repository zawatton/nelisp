;;; 13-phase5b-primitive-gap.data.el -- raw audit -*- lexical-binding: t; -*-
;; Generated: 2026-04-24 17:50:22+0900
;; Scope files: 4
;; MISSING host subrs (add to nelisp--primitive-symbols): 12
;; COVERED host subrs (already in table): 27

;;; Code:

(defconst phase5b-primitive-missing
  '(
    (copy-sequence :arity (1 . 1) :sites (("docs/design/13-forecast/nelisp-buffer-sketch.el" . 143)))
    (delq :arity (2 . 2) :sites (("docs/design/13-forecast/nelisp-marker-sketch.el" . 25) ("docs/design/13-forecast/nelisp-marker-sketch.el" . 56)))
    (elt :arity (2 . 2) :sites (("docs/design/13-forecast/nelisp-buffer-sketch.el" . 120)))
    (frame-height :arity (0 . 1) :sites (("docs/design/13-forecast/nelisp-redisplay-sketch.el" . 78)))
    (frame-width :arity (0 . 1) :sites (("docs/design/13-forecast/nelisp-redisplay-sketch.el" . 78)))
    (mark :arity (0 . 1) :sites (("docs/design/13-forecast/nelisp-buffer-sketch.el" . 9)))
    (nconc :arity (0 . many) :sites (("docs/design/13-forecast/nelisp-eventloop-sketch.el" . 21)))
    (point :arity (0 . 0) :sites (("docs/design/13-forecast/nelisp-buffer-sketch.el" . 9)))
    (run-at-time :arity (3 . many) :sites (("docs/design/13-forecast/nelisp-eventloop-sketch.el" . 59)))
    (send-string-to-terminal :arity (1 . 2) :sites (("docs/design/13-forecast/nelisp-redisplay-sketch.el" . 17) ("docs/design/13-forecast/nelisp-redisplay-sketch.el" . 22)))
    (split-string :arity (1 . 4) :sites (("docs/design/13-forecast/nelisp-redisplay-sketch.el" . 25)))
    (string-search :arity (2 . 3) :sites (("docs/design/13-forecast/nelisp-buffer-sketch.el" . 132)))
    ))

(defconst phase5b-primitive-covered
  '(
    (+ :sites (("docs/design/13-forecast/nelisp-buffer-sketch.el" . 64) ("docs/design/13-forecast/nelisp-buffer-sketch.el" . 68)))
    (1+ :sites (("docs/design/13-forecast/nelisp-buffer-sketch.el" . 27) ("docs/design/13-forecast/nelisp-redisplay-sketch.el" . 29)))
    (1- :sites (("docs/design/13-forecast/nelisp-buffer-sketch.el" . 68) ("docs/design/13-forecast/nelisp-buffer-sketch.el" . 94)))
    (< :sites (("docs/design/13-forecast/nelisp-buffer-sketch.el" . 120) ("docs/design/13-forecast/nelisp-marker-sketch.el" . 47)))
    (<= :sites (("docs/design/13-forecast/nelisp-buffer-sketch.el" . 132)))
    (>= :sites (("docs/design/13-forecast/nelisp-buffer-sketch.el" . 80) ("docs/design/13-forecast/nelisp-buffer-sketch.el" . 120)))
    (cdr :sites (("docs/design/13-forecast/nelisp-eventloop-sketch.el" . 41) ("docs/design/13-forecast/nelisp-marker-sketch.el" . 43)))
    (concat :sites (("docs/design/13-forecast/nelisp-buffer-sketch.el" . 27) ("docs/design/13-forecast/nelisp-buffer-sketch.el" . 68)))
    (cons :sites (("docs/design/13-forecast/nelisp-marker-sketch.el" . 36) ("docs/design/13-forecast/nelisp-redisplay-sketch.el" . 78)))
    (format :sites (("docs/design/13-forecast/nelisp-redisplay-sketch.el" . 17) ("docs/design/13-forecast/nelisp-redisplay-sketch.el" . 22)))
    (funcall :sites (("docs/design/13-forecast/nelisp-eventloop-sketch.el" . 29) ("docs/design/13-forecast/nelisp-eventloop-sketch.el" . 29)))
    (gethash :sites (("docs/design/13-forecast/nelisp-buffer-sketch.el" . 27) ("docs/design/13-forecast/nelisp-buffer-sketch.el" . 39)))
    (length :sites (("docs/design/13-forecast/nelisp-buffer-sketch.el" . 64) ("docs/design/13-forecast/nelisp-buffer-sketch.el" . 64)))
    (list :sites (("docs/design/13-forecast/nelisp-eventloop-sketch.el" . 21)))
    (make-hash-table :sites (("docs/design/13-forecast/nelisp-buffer-sketch.el" . 21) ("docs/design/13-forecast/nelisp-eventloop-sketch.el" . 15)))
    (max :sites (("docs/design/13-forecast/nelisp-buffer-sketch.el" . 68)))
    (memq :sites (("docs/design/13-forecast/nelisp-redisplay-sketch.el" . 62)))
    (min :sites (("docs/design/13-forecast/nelisp-buffer-sketch.el" . 68) ("docs/design/13-forecast/nelisp-redisplay-sketch.el" . 40)))
    (nreverse :sites (("docs/design/13-forecast/nelisp-marker-sketch.el" . 47) ("docs/design/13-forecast/nelisp-redisplay-sketch.el" . 29)))
    (nth :sites (("docs/design/13-forecast/nelisp-redisplay-sketch.el" . 29)))
    (null :sites (("docs/design/13-forecast/nelisp-buffer-sketch.el" . 132)))
    (number-to-string :sites (("docs/design/13-forecast/nelisp-buffer-sketch.el" . 27)))
    (provide :sites (("docs/design/13-forecast/nelisp-buffer-sketch.el" . 147) ("docs/design/13-forecast/nelisp-eventloop-sketch.el" . 66)))
    (puthash :sites (("docs/design/13-forecast/nelisp-buffer-sketch.el" . 27) ("docs/design/13-forecast/nelisp-buffer-sketch.el" . 39)))
    (require :sites (("docs/design/13-forecast/nelisp-buffer-sketch.el" . 1) ("docs/design/13-forecast/nelisp-eventloop-sketch.el" . 1)))
    (setcdr :sites (("docs/design/13-forecast/nelisp-marker-sketch.el" . 36)))
    (substring :sites (("docs/design/13-forecast/nelisp-buffer-sketch.el" . 68) ("docs/design/13-forecast/nelisp-buffer-sketch.el" . 68)))
    ))

(provide '13-phase5b-primitive-gap)
;;; 13-phase5b-primitive-gap.data.el ends here
