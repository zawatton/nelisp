;;; nemacs-process-bidi-smoke.el --- interactive subprocess smoke -*- lexical-binding: nil; -*-

;; Regression smoke for the pure-elisp bidirectional subprocess layer in
;; `scripts/nemacs-runtime-process-preload.el' (Doc 142 anvil-wl).
;;
;; This is the exact capability the portable IMAP engine
;; (anvil.el/anvil-wl-imap.el) needs: drive a LIVE interactive child via
;; `process-send-string', then `accept-process-output' in a loop until the
;; reply arrives, dispatched to the `:filter' — all while the process is
;; live.  Before the bidi layer, `process-send-string' was a no-op on the
;; output-only native process object, so this would hang / never echo.
;;
;; Run on the NeLisp standalone reader / nemacs runtime (NOT host Emacs):
;;
;;   ./vendor/nelisp/target/nelisp --load \
;;     scripts/nemacs-runtime-process-preload.el \
;;     --load test/nemacs-process-bidi-smoke.el
;;
;; or via the Makefile target `test-nemacs-process-bidi-smoke', which prints
;; a single line ending in `=PASS' or `=FAIL'.

;; Load the process preload first (the bidi facades live there).  Honour
;; NEMACS_PROCESS_PRELOAD, else fall back to the repo-relative path.  The
;; standalone reader executes a single `--load FILE'; it does not accept a
;; second `--load', so the smoke is responsible for pulling in the preload.
(let ((p (or (getenv "NEMACS_PROCESS_PRELOAD")
             "scripts/nemacs-runtime-process-preload.el")))
  (when (file-readable-p p)
    (load p nil 'no-message t t)))

(setq nemacs-process-bidi-smoke--acc "")

(fset 'nemacs-process-bidi-smoke--filter
      '(lambda (_proc chunk)
         (when (stringp chunk)
           (setq nemacs-process-bidi-smoke--acc
                 (concat nemacs-process-bidi-smoke--acc chunk)))))

(fset 'nemacs-process-bidi-smoke--report
      '(lambda (line)
         (if (fboundp 'nelisp--write-stdout-bytes)
             (nelisp--write-stdout-bytes (concat line "\n"))
           (princ (concat line "\n")))))

;; --- Test 1: interactive round-trip through a live `/bin/cat' ----------
(let ((proc (make-process
             :name "bidi-smoke-cat"
             :command (list "/bin/cat")
             :connection-type 'pipe
             :coding 'binary
             :noquery t
             :filter 'nemacs-process-bidi-smoke--filter)))
  (let ((live-after-spawn (process-live-p proc))
        (got nil)
        (tries 0))
    (process-send-string proc "bidi-round-trip\n")
    (while (if (< tries 50)
               (not (setq got (string-match-p
                               "bidi-round-trip"
                               nemacs-process-bidi-smoke--acc)))
             nil)
      (accept-process-output proc 0.1)
      (setq tries (+ tries 1)))
    (let ((live-during (process-live-p proc)))
      (delete-process proc)
      (let ((live-after-delete (process-live-p proc)))
        (if (if live-after-spawn
                (if got
                    (if live-during
                        (not live-after-delete)
                      nil)
                  nil)
              nil)
            (nemacs-process-bidi-smoke--report "ROUNDTRIP=PASS")
          (nemacs-process-bidi-smoke--report
           (concat "ROUNDTRIP=FAIL"
                   " live-after-spawn=" (if live-after-spawn "t" "nil")
                   " got=" (if got "t" "nil")
                   " live-during=" (if live-during "t" "nil")
                   " live-after-delete=" (if live-after-delete "t" "nil"))))))))

;; --- Test 2: lambda :filter (closure) + sentinel on self-exit ----------
(setq nemacs-process-bidi-smoke--acc2 "")
(setq nemacs-process-bidi-smoke--sentinel-event nil)
(let ((proc (make-process
             :name "bidi-smoke-exit"
             :command (list "/bin/sh" "-c" "echo bidi-bye; exit 3")
             :connection-type 'pipe
             :coding 'binary
             :noquery t
             :filter (lambda (_p s)
                       (when (stringp s)
                         (setq nemacs-process-bidi-smoke--acc2
                               (concat nemacs-process-bidi-smoke--acc2 s))))
             :sentinel (lambda (_p e)
                         (setq nemacs-process-bidi-smoke--sentinel-event e)))))
  (let ((tries 0))
    (while (if (< tries 50) (process-live-p proc) nil)
      (accept-process-output proc 0.1)
      (setq tries (+ tries 1))))
  (let ((es (process-exit-status proc))
        (saw (string-match-p "bidi-bye" nemacs-process-bidi-smoke--acc2)))
    (if (if saw
            (if nemacs-process-bidi-smoke--sentinel-event
                (= es 3)
              nil)
          nil)
        (nemacs-process-bidi-smoke--report "EXIT-SENTINEL=PASS")
      (nemacs-process-bidi-smoke--report
       (concat "EXIT-SENTINEL=FAIL"
               " saw-output=" (if saw "t" "nil")
               " sentinel=" (if nemacs-process-bidi-smoke--sentinel-event "t" "nil")
               " exit-status=" (number-to-string es))))))

;;; nemacs-process-bidi-smoke.el ends here
