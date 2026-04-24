;;; 15-phase5d-primitive-gap.data.el -- raw audit -*- lexical-binding: t; -*-
;; Generated: 2026-04-24 20:30:31+0900
;; Scope files: 1
;; MISSING host subrs (add to nelisp--primitive-symbols): 4
;; COVERED host subrs (already in table): 18

;;; Code:

(defconst phase5d-primitive-missing
  '(
    (float-time :arity (0 . 1) :sites (("docs/design/15-forecast/nelisp-worker-sketch.el" . 57)))
    (format-time-string :arity (1 . 3) :sites (("docs/design/15-forecast/nelisp-worker-sketch.el" . 85)))
    (random :arity (0 . 1) :sites (("docs/design/15-forecast/nelisp-worker-sketch.el" . 35)))
    (truncate :arity (1 . 2) :sites (("docs/design/15-forecast/nelisp-worker-sketch.el" . 57) ("docs/design/15-forecast/nelisp-worker-sketch.el" . 62)))
    ))

(defconst phase5d-primitive-covered
  '(
    (* :sites (("docs/design/15-forecast/nelisp-worker-sketch.el" . 57) ("docs/design/15-forecast/nelisp-worker-sketch.el" . 62)))
    (/ :sites (("docs/design/15-forecast/nelisp-worker-sketch.el" . 62)))
    (1+ :sites (("docs/design/15-forecast/nelisp-worker-sketch.el" . 35)))
    (1- :sites (("docs/design/15-forecast/nelisp-worker-sketch.el" . 62) ("docs/design/15-forecast/nelisp-worker-sketch.el" . 69)))
    (cons :sites (("docs/design/15-forecast/nelisp-worker-sketch.el" . 69) ("docs/design/15-forecast/nelisp-worker-sketch.el" . 69)))
    (eq :sites (("docs/design/15-forecast/nelisp-worker-sketch.el" . 89)))
    (format :sites (("docs/design/15-forecast/nelisp-worker-sketch.el" . 35)))
    (ignore :sites (("docs/design/15-forecast/nelisp-worker-sketch.el" . 101) ("docs/design/15-forecast/nelisp-worker-sketch.el" . 104)))
    (length :sites (("docs/design/15-forecast/nelisp-worker-sketch.el" . 62)))
    (max :sites (("docs/design/15-forecast/nelisp-worker-sketch.el" . 62)))
    (message :sites (("docs/design/15-forecast/nelisp-worker-sketch.el" . 85)))
    (min :sites (("docs/design/15-forecast/nelisp-worker-sketch.el" . 62)))
    (nth :sites (("docs/design/15-forecast/nelisp-worker-sketch.el" . 62)))
    (plist-get :sites (("docs/design/15-forecast/nelisp-worker-sketch.el" . 69) ("docs/design/15-forecast/nelisp-worker-sketch.el" . 104)))
    (plist-put :sites (("docs/design/15-forecast/nelisp-worker-sketch.el" . 69) ("docs/design/15-forecast/nelisp-worker-sketch.el" . 69)))
    (prin1-to-string :sites (("docs/design/15-forecast/nelisp-worker-sketch.el" . 97)))
    (provide :sites (("docs/design/15-forecast/nelisp-worker-sketch.el" . 116)))
    (run-at-time :sites (("docs/design/15-forecast/nelisp-worker-sketch.el" . 79) ("docs/design/15-forecast/nelisp-worker-sketch.el" . 89)))
    ))

(provide '15-phase5d-primitive-gap)
;;; 15-phase5d-primitive-gap.data.el ends here
