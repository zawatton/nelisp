;;; nelisp-thread-mirror-guard-smoke.el --- Doc 199 worker mirror guard -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Interpreter-driven proof of the Tier-3a/3b write ceiling.  A registered
;; worker must still call and read globals, but hit-path and miss-path `setq'
;; mutations must signal `nelisp-worker-mirror-mutation' and leave the shared
;; mirror unchanged.  Creating a new interned name is the same shared-global
;; mutation class and is refused too; interning an existing name remains a
;; lookup.  Every form is built before clone(2), and every section uses worker
;; registry ID 2 under the Doc 199 GC-inhibit boundary.

;;; Code:

(defmacro nl-thread-mirror-guard--should-unsupported (form)
  "Require FORM to signal `nelisp-unsupported-primitive'."
  `(let ((nl-thread-mirror-guard--outcome
          (condition-case nl-thread-mirror-guard--error
              (progn ,form 'nl-thread-mirror-guard--no-error)
            (error (car nl-thread-mirror-guard--error)))))
     (unless (eq nl-thread-mirror-guard--outcome
                 'nelisp-unsupported-primitive)
       (error "expected nelisp-unsupported-primitive from %S, got %S"
              ',form nl-thread-mirror-guard--outcome))))

(defvar nl-thread-mirror-guard--target 17)
(defvar nl-thread-mirror-guard--bias 1)

(defun nl-thread-mirror-guard--global-function (value)
  "Return VALUE plus a global bias; workers exercise two mirror reads."
  (+ value nl-thread-mirror-guard--bias))

(defun nl-thread-mirror-guard--run (form)
  "Evaluate FORM in one registered worker and return its integer result."
  (let* ((shared (nelisp-thread-shared-alloc 32))
         (result shared)
         (done (+ shared 8))
         (section-active nil))
    (when (< shared 0)
      (error "nelisp-thread-shared-alloc failed: %S" shared))
    (unwind-protect
        (progn
          (unless (= (nelisp-thread-gc-inhibit 1) 1)
            (error "worker mirror-guard parallel section did not begin"))
          (setq section-active t)
          (let ((tid (nelisp-thread-spawn 2 0 form result done)))
            (unless (> tid 0)
              (error "worker mirror-guard spawn failed: %S" tid)))
          (unless (= (nelisp-thread-join done 1) 1)
            (error "worker mirror-guard join returned early"))
          (let ((answer (nelisp-thread-atomic-read result)))
            (unless (= (nelisp-thread-gc-inhibit 0) 1)
              (error "worker mirror-guard section grew the arena"))
            (setq section-active nil)
            answer))
      (when section-active
        (nelisp-thread-gc-inhibit 0)))))

(let ((checked 0)
      (names '(nelisp-thread-shared-alloc
               nelisp-thread-atomic-add
               nelisp-thread-atomic-read
               nelisp-thread-spawn
               nelisp-thread-join
               nelisp-thread-gc-inhibit)))
  (dolist (name names)
    (unless (fboundp name)
      (error "worker mirror-guard primitive is not fboundp: %S" name))
    (setq checked (+ checked 1)))
  (if (and (eq system-type 'gnu/linux)
           (string= system-configuration "x86_64-pc-linux-gnu"))
      (progn
        ;; Read direction: ordinary global function and value lookup still work.
        (unless (= (nl-thread-mirror-guard--run
                    '(nl-thread-mirror-guard--global-function 41))
                   42)
          (error "registered worker could not read/call global state"))
        (setq checked (+ checked 1))

        ;; Existing-entry hit path: catch the named condition and prove the
        ;; original value cell did not change.
        (unless (= (nl-thread-mirror-guard--run
                    '(condition-case nl-thread-mirror-guard--error
                         (progn
                           (setq nl-thread-mirror-guard--target 99)
                           -20)
                       (nelisp-worker-mirror-mutation
                        (if (eq (cadr nl-thread-mirror-guard--error)
                                'nl-thread-mirror-guard--target)
                            23
                          24))))
                   23)
          (error "worker hit-path mirror write was not catchably refused"))
        (setq checked (+ checked 1))
        (unless (= nl-thread-mirror-guard--target 17)
          (error "worker changed shared global to %S"
                 nl-thread-mirror-guard--target))
        (setq checked (+ checked 1))

        ;; Absent-entry miss path: the bucket vector/count must remain intact.
        (unless (= (nl-thread-mirror-guard--run
                    '(condition-case nl-thread-mirror-guard--error
                         (progn
                           (setq nl-thread-mirror-guard--new-target 101)
                           -25)
                       (nelisp-worker-mirror-mutation
                        (if (eq (cadr nl-thread-mirror-guard--error)
                                'nl-thread-mirror-guard--new-target)
                            25
                          26))))
                   25)
          (error "worker miss-path mirror write was not catchably refused"))
        (setq checked (+ checked 1))
        (when (boundp 'nl-thread-mirror-guard--new-target)
          (error "worker inserted an absent global into the shared mirror"))
        (setq checked (+ checked 1))

        ;; Intern lookup is allowed, but a name-table insertion is a write.
        (when (intern-soft "nl-thread-mirror-guard--fresh-intern")
          (error "fresh intern probe unexpectedly existed before worker"))
        (setq checked (+ checked 1))
        (unless (= (nl-thread-mirror-guard--run
                    '(condition-case nl-thread-mirror-guard--error
                         (progn
                           (intern "nl-thread-mirror-guard--fresh-intern")
                           -27)
                       (nelisp-worker-mirror-mutation
                        (if (string= (cadr nl-thread-mirror-guard--error)
                                     "nl-thread-mirror-guard--fresh-intern")
                            27
                          28))))
                   27)
          (error "worker intern-table write was not catchably refused"))
        (setq checked (+ checked 1))
        (when (intern-soft "nl-thread-mirror-guard--fresh-intern")
          (error "worker inserted a new name into the shared intern table"))
        (setq checked (+ checked 1)))
    (progn
      (nl-thread-mirror-guard--should-unsupported
       (nelisp-thread-shared-alloc 32))
      (setq checked (+ checked 1))
      (nl-thread-mirror-guard--should-unsupported
       (nelisp-thread-atomic-read 0))
      (setq checked (+ checked 1))
      (nl-thread-mirror-guard--should-unsupported
       (nelisp-thread-spawn 2 0 nil 0 0))
      (setq checked (+ checked 1))
      (nl-thread-mirror-guard--should-unsupported
       (nelisp-thread-join 0 1))
      (setq checked (+ checked 1))
      (nl-thread-mirror-guard--should-unsupported
       (nelisp-thread-gc-inhibit 1))
      (setq checked (+ checked 1))))
  (princ (format "GATE-COUNT checked=%d findings=0\n" checked))
  (princ "nelisp-thread-mirror-guard-smoke: PASS\n"))

;;; nelisp-thread-mirror-guard-smoke.el ends here
