;;; emacs-startup-screen-test.el --- ERT for emacs-startup-screen  -*- lexical-binding: t; -*-

;;; Commentary:

;; Focused tests for the startup splash screen owner (UX #18 session B):
;; buffer creation, read-only state, point position, the display gate,
;; and the splash step wiring in `nemacs-init'.
;;
;; The splash buffer lives on the `nelisp-ec-*' compat substrate (the
;; same registry as the bootstrap's *scratch*), so assertions use the
;; `nelisp-ec-*' accessors rather than host buffer functions.

;;; Code:

(require 'ert)
(require 'emacs-startup-screen)
(require 'nemacs-loadup)

(defun emacs-startup-screen-test--kill-splash ()
  "Remove any existing splash buffer from the compat registry."
  (let ((buf (emacs-startup-screen-buffer)))
    (when buf
      (cond
       ((and (fboundp 'nelisp-ec-buffer-p) (nelisp-ec-buffer-p buf))
        (nelisp-ec-kill-buffer buf))
       ((bufferp buf)
        (kill-buffer buf))))))

(defun emacs-startup-screen-test--splash-text ()
  "Return the splash buffer text, or nil when the buffer is absent."
  (let ((buf (emacs-startup-screen-buffer)))
    (when buf
      (nelisp-ec-with-current-buffer buf
        (nelisp-ec-buffer-string)))))

(defmacro emacs-startup-screen-test--with-clean-state (&rest body)
  "Run BODY with no pre-existing splash buffer and neutral gate globals."
  (declare (indent 0) (debug (body)))
  `(let ((inhibit-startup-screen nil)
         (user-init-file nil)
         (emacs-startup-screen-file-arguments nil))
     (emacs-startup-screen-test--kill-splash)
     (unwind-protect
         (progn ,@body)
       (emacs-startup-screen-test--kill-splash))))

;;;; A. Module surface

(ert-deftest emacs-startup-screen-test/module-surface ()
  (should (featurep 'emacs-startup-screen))
  (should (equal emacs-startup-screen-buffer-name "*GNU Emacs*"))
  (should (stringp emacs-startup-screen-text))
  (should (> (length emacs-startup-screen-text) 0))
  (should (fboundp 'emacs-startup-screen-create))
  (should (fboundp 'emacs-startup-screen-select))
  (should (fboundp 'emacs-startup-screen-use-p)))

;;;; B. Buffer creation

(ert-deftest emacs-startup-screen-test/create-returns-named-buffer ()
  (emacs-startup-screen-test--with-clean-state
    (let ((buf (emacs-startup-screen-create)))
      (should buf)
      (should (nelisp-ec-buffer-p buf))
      (should (equal (nelisp-ec-buffer-name buf)
                     emacs-startup-screen-buffer-name))
      (should (eq buf (emacs-startup-screen-buffer))))))

(ert-deftest emacs-startup-screen-test/create-body-and-about-line ()
  (emacs-startup-screen-test--with-clean-state
    (emacs-startup-screen-create)
    (let ((text (emacs-startup-screen-test--splash-text)))
      (should (stringp text))
      (should (string-match-p "Welcome to nemacs" text))
      (should (string-match-p "C-x C-c" text))
      (should (string-match-p "C-x C-f" text))
      (should (string-match-p "ABSOLUTELY NO WARRANTY" text))
      (should (string-match-p "This is nemacs" text)))))

(ert-deftest emacs-startup-screen-test/create-point-at-min-unmodified ()
  (emacs-startup-screen-test--with-clean-state
    (let ((buf (emacs-startup-screen-create)))
      (should (= (nelisp-ec-with-current-buffer buf (nelisp-ec-point)) 1))
      (should-not (nelisp-ec-buffer-modified-p buf)))))

(ert-deftest emacs-startup-screen-test/host-path-read-only-point-at-min ()
  ;; The compat-substrate path has no per-buffer read-only cell (the
  ;; standalone reader uses the global-flag convention, asserted by
  ;; init-smoke.sh); the host buffer path carries the real read-only
  ;; contract, so exercise it directly.
  (let ((host-buf nil))
    (unwind-protect
        (progn
          (setq host-buf (emacs-startup-screen--create-host))
          (should (bufferp host-buf))
          (with-current-buffer host-buf
            (should buffer-read-only)
            (should (= (point) (point-min)))
            (should-not (buffer-modified-p))
            (should (string-match-p "Welcome to nemacs" (buffer-string)))))
      (when (buffer-live-p host-buf)
        (kill-buffer host-buf)))))

(ert-deftest emacs-startup-screen-test/create-does-not-switch-current-buffer ()
  (emacs-startup-screen-test--with-clean-state
    (let ((before (nelisp-ec-current-buffer)))
      (emacs-startup-screen-create)
      (should (eq (nelisp-ec-current-buffer) before)))))

(ert-deftest emacs-startup-screen-test/create-twice-refreshes-same-buffer ()
  (emacs-startup-screen-test--with-clean-state
    (let ((first (emacs-startup-screen-create))
          (second (emacs-startup-screen-create)))
      (should (eq first second))
      (should (string-match-p "Welcome to nemacs"
                              (emacs-startup-screen-test--splash-text))))))

(ert-deftest emacs-startup-screen-test/select-makes-splash-current ()
  (emacs-startup-screen-test--with-clean-state
    (let ((previous (nelisp-ec-current-buffer))
          (buf (emacs-startup-screen-select)))
      (unwind-protect
          (progn
            (should buf)
            (should (eq (nelisp-ec-current-buffer) buf)))
        (when previous
          (nelisp-ec-set-buffer previous))))))

;;;; C. Display gate

(ert-deftest emacs-startup-screen-test/gate-open-by-default ()
  (emacs-startup-screen-test--with-clean-state
    (should (emacs-startup-screen-use-p))))

(ert-deftest emacs-startup-screen-test/gate-closed-by-inhibit ()
  (emacs-startup-screen-test--with-clean-state
    (let ((inhibit-startup-screen t))
      (should-not (emacs-startup-screen-use-p)))))

(ert-deftest emacs-startup-screen-test/gate-closed-by-loaded-init-file ()
  (emacs-startup-screen-test--with-clean-state
    (let ((user-init-file "/tmp/emacs-startup-screen-test-init.el"))
      (should-not (emacs-startup-screen-use-p)))))

(ert-deftest emacs-startup-screen-test/gate-closed-by-file-args ()
  (emacs-startup-screen-test--with-clean-state
    (should-not (emacs-startup-screen-use-p '("/tmp/some-file.txt")))
    (let ((emacs-startup-screen-file-arguments '("/tmp/some-file.txt")))
      (should-not (emacs-startup-screen-use-p)))))

;;;; D. Image asset helper (report-only)

(ert-deftest emacs-startup-screen-test/image-path-reports-vendored-asset ()
  (let* ((root (locate-dominating-file
                (or (locate-library "emacs-startup-screen") default-directory)
                "vendor"))
         (dir (and root (expand-file-name "vendor/emacs-etc/images/" root))))
    (skip-unless (and dir (file-directory-p dir)))
    (let ((path (emacs-startup-screen-image-path dir)))
      (should (stringp path))
      (should (file-exists-p path))
      (should (string-match-p "splash\\." path)))))

(ert-deftest emacs-startup-screen-test/image-path-nil-when-absent ()
  (should-not (emacs-startup-screen-image-path "/nonexistent-dir-for-test/")))

;;;; E. nemacs-init splash step wiring

(defmacro emacs-startup-screen-test--with-fresh-bootstrap (&rest body)
  "Run BODY against a clean bootstrap with init files disabled."
  (declare (indent 0) (debug (body)))
  `(emacs-startup-screen-test--with-clean-state
     (nemacs-uninit)
     (let ((nemacs-startup-hook nil)
           (init-file-user nil))
       (unwind-protect
           (progn ,@body)
         (nemacs-uninit)))))

(ert-deftest emacs-startup-screen-test/batch-init-skips-splash ()
  (emacs-startup-screen-test--with-fresh-bootstrap
    (nemacs-init t)
    (should-not (emacs-startup-screen-buffer))))

(ert-deftest emacs-startup-screen-test/interactive-init-creates-splash ()
  (emacs-startup-screen-test--with-fresh-bootstrap
    (nemacs-init)
    (let ((buf (emacs-startup-screen-buffer)))
      (should buf)
      (should (eq (nelisp-ec-current-buffer) buf))
      (should (string-match-p "Welcome to nemacs"
                              (emacs-startup-screen-test--splash-text))))))

(ert-deftest emacs-startup-screen-test/interactive-init-inhibited-splash ()
  (emacs-startup-screen-test--with-fresh-bootstrap
    (let ((inhibit-startup-screen t))
      (nemacs-init))
    (should-not (emacs-startup-screen-buffer))))

(provide 'emacs-startup-screen-test)

;;; emacs-startup-screen-test.el ends here
