;;; standalone-condition-case-root-check.el --- Retain catch scratch across GC -*- lexical-binding: t; -*-

;; The protected form can collect before the evaluator clears its error
;; slot. That slot must still be owned, even on the success path. Reuse of
;; an unrooted slot used to zero a neighbour's block header, after which
;; later collections silently refused the malformed heap.
(defvar cc-root-payload nil)
(defvar cc-root-failures 0)
(defvar cc-root-before-depth nil)
(defvar cc-root-after-depth nil)
(setq cc-root-before-depth (nth 9 (nelisp--debug-switch 0)))
(nelisp--debug-switch 28)
(dotimes (_ 8)
  (unless (eq (condition-case nil
                  (progn
                    (garbage-collect)
                    (setq cc-root-payload (make-list 4000 'retained))
                    'completed)
                (error 'unexpected))
              'completed)
    (setq cc-root-failures (1+ cc-root-failures)))
  (let ((tail cc-root-payload) (count 0))
    (while (consp tail)
      (unless (eq (car tail) 'retained)
        (setq cc-root-failures (1+ cc-root-failures)))
      (setq count (1+ count) tail (cdr tail)))
    (unless (= count 4000) (setq cc-root-failures (1+ cc-root-failures)))))
(unless (equal (condition-case problem
                   (progn (garbage-collect) (signal 'error '(payload)))
                 (error (garbage-collect) problem))
               '(error payload))
  (setq cc-root-failures (1+ cc-root-failures)))
(unless (equal (catch 'cc-root-exit
                 (condition-case nil
                     (progn (garbage-collect) (throw 'cc-root-exit 'thrown))
                   (error 'unexpected)))
               'thrown)
  (setq cc-root-failures (1+ cc-root-failures)))
(unless (= (nth 4 (nelisp--arena-walk-verify)) 1)
  (setq cc-root-failures (1+ cc-root-failures)))
(setq cc-root-after-depth (nth 9 (nelisp--debug-switch 0)))
(unless (= cc-root-after-depth cc-root-before-depth)
  (setq cc-root-failures (1+ cc-root-failures)))
(nelisp--debug-switch 29)
(princ (format "CONDITION-CASE-ROOT checks=20 failures=%d %s\n"
               cc-root-failures (if (= cc-root-failures 0) "PASS" "FAIL")))
(kill-emacs (if (= cc-root-failures 0) 0 1))

;;; standalone-condition-case-root-check.el ends here
