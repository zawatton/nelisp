;;; color-capacity-smoke.el --- Shared visual and terminal capacity -*- lexical-binding: t; -*-
;; Run from the already built GUI heap. No display or package is replaced.
(let* ((frame (emacs-frame-selected-frame))
       (saved (emacs-frame-frame-parameter frame 'display-depth))
       (shim (symbol-function 'display-color-cells)))
  (unwind-protect
      (progn
        (emacs-frame-set-frame-parameter frame 'display-depth nil)
        (unless (= (display-color-cells) (tty-display-color-cells))
          (error "Headless color capacity differs from the terminal provider"))
        (dolist (depth '(8 24))
          (emacs-frame-set-frame-parameter frame 'display-depth depth)
          (let ((cells (display-color-cells)))
            (unless (= cells (expt 2 depth))
              (error "Visual depth %s has incorrect capacity %s" depth cells))
            (princ (format "K1-COLOR|depth=%d|cells=%d|\n" depth cells))))
        ;; This is the previous faulty dispatch: the early generic graphic
        ;; predicate remains nil even after the GUI installs a real visual.
        (fset 'display-color-cells
              (lambda (&optional display)
                (if (and (display-graphic-p display)
                         (fboundp 'emacs-frame-display-color-cells))
                    (emacs-frame-display-color-cells display)
                  (tty-display-color-cells display))))
        (when (= (display-color-cells) (expt 2 24))
          (error "Faulty generic-predicate control unexpectedly passed"))
        (princ "K1-COLOR|old-dispatch-rejected|\n"))
    (fset 'display-color-cells shim)
    (emacs-frame-set-frame-parameter frame 'display-depth saved)))
(princ "K1-COLOR-DONE\n")
