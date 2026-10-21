;;; standalone-casetab-fileio-probe.el --- Case/buffer integration regression -*- lexical-binding: t -*-

;; Set CASETAB_LIBRARY_BUNDLE for a fresh library load on the tested runtime.
;; Leave it unset under GNU, or when testing an already loaded library image.
(condition-case problem
    (progn
      (let ((bundle (getenv "CASETAB_LIBRARY_BUNDLE")))
        (when bundle (load bundle nil t)))
      (with-temp-buffer
        (let ((ticks (list (buffer-modified-tick) (buffer-chars-modified-tick)))
              (undo buffer-undo-list))
          (set-buffer-auto-saved)
          (when (recent-auto-save-p)
            (error "Fresh buffer incorrectly reports a recent auto-save"))
          (set-buffer-multibyte enable-multibyte-characters)
          (unless (eq undo buffer-undo-list)
            (error "An unchanged multibyte flag changed undo state"))
          (set-buffer-multibyte nil)
          (set-buffer-multibyte t)
          (unless (equal ticks (list (buffer-modified-tick)
                                    (buffer-chars-modified-tick)))
            (error "Empty-buffer multibyte changes altered modification ticks"))
          (when (or (buffer-modified-p) (recent-auto-save-p))
            (error "Empty-buffer multibyte changes altered saved state"))))
      (princ "CASETAB-FILEIO2 PASS recent=nil ticks-unchanged=t\n")
      (kill-emacs 0))
  (error
   (princ (format "CASETAB-FILEIO2 FAIL %S\n" problem))
   (kill-emacs 1)))

;;; standalone-casetab-fileio-probe.el ends here
