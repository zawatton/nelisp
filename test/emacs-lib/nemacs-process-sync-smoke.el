;;; nemacs-process-sync-smoke.el --- standalone sync process smoke -*- lexical-binding: nil; -*-

;; Run on the NeLisp standalone reader.  It loads the library process facade
;; from src/ and proves the Emacs-shaped synchronous process gateway used by
;; Magit-style callers: `call-process' output capture, non-zero exits,
;; explicit buffer destinations, and `git --version'.

(let ((src (or (getenv "NEMACS_PROCESS_SRC") "src")))
  (setq load-path (cons src load-path))
  (load (concat src "/emacs-vars.el") nil t)
  (load (concat src "/emacs-symbol.el") nil t)
  (load (concat src "/emacs-standalone.el") nil t)
  (load (concat src "/nelisp-text-buffer.el") nil t)
  (load (concat src "/nelisp-emacs-compat.el") nil t)
  ;; `emacs-buffer.el' (required transitively by `emacs-buffer-builtins.el')
  ;; calls `advice-add' at top level.  Host Emacs preloads `nadvice.el', but
  ;; this smoke test's hand-picked load list does not, so the standalone
  ;; reader hits `void-function: advice-add' unless the repo's own
  ;; `unless (fboundp ...)'-guarded substrate for it is loaded first.
  (load (concat src "/emacs-stub.el") nil t)
  (load (concat src "/emacs-buffer-builtins.el") nil t)
  (load (concat src "/emacs-process.el") nil t)
  (load (concat src "/emacs-process-builtins.el") nil t))

(fset 'nemacs-process-sync-smoke--print
      '(lambda (line)
         (if (fboundp 'nelisp--write-stdout-bytes)
             (nelisp--write-stdout-bytes (concat line "\n"))
           (princ (concat line "\n")))))

;; Pass/fail is decided here in Lisp via `equal'/prefix comparisons on
;; the actual process results, not by a shell `grep' on the diagnostic
;; lines printed below.  `print-escape-newlines' defaults to nil in GNU
;; Emacs too (verified on host 31.1: `(prin1-to-string "a\nb")' prints a
;; literal embedded newline byte, not a `\n' escape) -- and this
;; standalone image does not even bind the variable at all (`boundp' is
;; nil), so there is no printer knob here to force single-line output.
;; A CALL-PROCESS-* diagnostic line below spanning more than one physical
;; line is therefore expected and correct; only `PROC-SMOKE-RESULT'
;; (fixed words plus failure names, never printed buffer content) is
;; safe for the Makefile `proc-smoke' target to anchor a `grep' on.
(defvar nemacs-process-sync-smoke--failures nil)

(fset 'nemacs-process-sync-smoke--check
      '(lambda (name ok)
         (unless ok
           (setq nemacs-process-sync-smoke--failures
                 (cons name nemacs-process-sync-smoke--failures)))))

(let ((buf (get-buffer-create "*proc-smoke*")))
  (with-current-buffer buf
    (erase-buffer)
    (let* ((rc (call-process "echo" nil t nil "hello"))
           (out (buffer-string)))
      (nemacs-process-sync-smoke--print
       (concat "CALL-PROCESS-ECHO rc=" (number-to-string rc)
               " output=" (prin1-to-string out)))
      (nemacs-process-sync-smoke--check
       "CALL-PROCESS-ECHO" (and (= rc 0) (equal out "hello\n")))))
  (with-current-buffer buf
    (erase-buffer)
    (let* ((rc (call-process "false" nil t nil))
           (out (buffer-string)))
      (nemacs-process-sync-smoke--print
       (concat "CALL-PROCESS-FALSE rc=" (number-to-string rc)
               " output=" (prin1-to-string out)))
      (nemacs-process-sync-smoke--check
       "CALL-PROCESS-FALSE" (and (= rc 1) (equal out "")))))
  (let ((dest-name "*proc-smoke-destination*")
        (dest (get-buffer-create "*proc-smoke-destination*")))
    (with-current-buffer dest
      (erase-buffer))
    (let* ((rc (call-process "echo" nil dest-name nil "buffer-destination"))
           (out (with-current-buffer dest (buffer-string))))
      (nemacs-process-sync-smoke--print
       (concat "CALL-PROCESS-BUFFER rc=" (number-to-string rc)
               " output=" (prin1-to-string out)))
      (nemacs-process-sync-smoke--check
       "CALL-PROCESS-BUFFER" (and (= rc 0) (equal out "buffer-destination\n")))))
  (with-current-buffer buf
    (erase-buffer)
    (let* ((rc (call-process "git" nil t nil "--version"))
           (out (buffer-string))
           (prefix "git version "))
      (nemacs-process-sync-smoke--print
       (concat "CALL-PROCESS-GIT rc=" (number-to-string rc)
               " output=" (prin1-to-string out)))
      (nemacs-process-sync-smoke--check
       "CALL-PROCESS-GIT"
       (and (= rc 0)
            (>= (length out) (length prefix))
            (equal (substring out 0 (length prefix)) prefix))))))

(nemacs-process-sync-smoke--print
 (if nemacs-process-sync-smoke--failures
     (concat "PROC-SMOKE-RESULT: FAIL "
             (prin1-to-string (reverse nemacs-process-sync-smoke--failures)))
   "PROC-SMOKE-RESULT: PASS"))

;;; nemacs-process-sync-smoke.el ends here
