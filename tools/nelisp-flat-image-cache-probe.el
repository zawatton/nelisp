;;; nelisp-flat-image-cache-probe.el --- cheap warm flat-image cache probe -*- lexical-binding: t; -*-

;; Answer one question -- "is IMAGE already a fresh flat snapshot of ARTIFACT
;; for RUNTIME?" -- without starting the artifact command runtime.
;;
;; `compile-runtime-image --flat-artifact-cache' answers the same question, but
;; in the standalone reader it first replays the AOT artifact-runtime cache
;; module, and that replay is a fixed cost paid before any freshness check
;; runs.  Measured 2026-08-10 with a deliberately absent input, so nothing but
;; the fixed cost could be timed:
;;
;;     total                                     531 s
;;     nine interpreted preamble sources           5.7 s
;;     target/nelisp-artifact-runtime.el.neln    525 s
;;
;; The 525 s is one `nl_eval_source_all' form, ~200 frames deep, entirely in
;; `nl_eval_inner' / `nl_apply_lambda_inner' with reader and GC frames under
;; it.  Twelve stack samples contained no bytecode and no native frame, so the
;; AOT cache buys nothing on this route: it is re-interpreted as source.
;;
;; The freshness decision itself only reads a 1,874-byte plist and hashes four
;; files, so it runs here in about 6 s -- the same predicate, an 80x cheaper
;; bootstrap.
;;
;; The predicate is deliberately NOT reimplemented.  This loads
;; `nelisp-artifact' and calls `nelisp-artifact--flat-image-cache-sidecar-hit',
;; the function `nelisp-artifact-prepare-flat-image-cache' itself consults, so
;; the two paths cannot drift into disagreeing about what "fresh" means.  Only
;; the bootstrap differs.
;;
;; Usage (bare reader route -- do NOT run this under an artifact subcommand):
;;
;;     NELISP_FLAT_CACHE_ARTIFACT=... \
;;     NELISP_FLAT_CACHE_IMAGE=...    \
;;     NELISP_FLAT_CACHE_RUNTIME=...  \
;;     nelisp --load tools/nelisp-flat-image-cache-probe.el
;;
;; Set NELISP_FLAT_CACHE_LISP to override the `nelisp-artifact.el' location;
;; it otherwise resolves against NELISP_HOME.
;;
;; Exactly one verdict line reaches stdout:
;;
;;     flat-image-cache-probe=hit     image is current; skip the rebuild
;;     flat-image-cache-probe=miss    not current
;;     flat-image-cache-probe=error   probe could not decide -- treat as miss
;;
;; `hit' is printed only when the predicate returned non-nil.  Every other
;; outcome, including a void function or an unreadable file, is `error', so a
;; broken probe degrades into doing the full rebuild rather than into booting a
;; stale image.

(defvar nelisp-flat-image-cache-probe--verdict "error"
  "Verdict printed on exit.  Starts pessimistic on purpose.")

(defun nelisp-flat-image-cache-probe--env (name)
  "Return non-empty environment variable NAME, or nil."
  (let ((v (getenv name)))
    (and (stringp v) (> (length v) 0) v)))

(defun nelisp-flat-image-cache-probe--home ()
  "Return the nelisp repository root."
  (or (nelisp-flat-image-cache-probe--env "NELISP_HOME")
      (error "set NELISP_HOME")))

(defun nelisp-flat-image-cache-probe--artifact-lisp ()
  "Return the path to `nelisp-artifact.el'."
  (or (nelisp-flat-image-cache-probe--env "NELISP_FLAT_CACHE_LISP")
      (expand-file-name "lisp/nelisp-artifact.el"
                        (nelisp-flat-image-cache-probe--home))))

(defun nelisp-flat-image-cache-probe--prepare-load-path ()
  "Put `lisp' and `src' on `load-path' for `nelisp-artifact''s requires.
Without this the requires fail one by one, the load still finishes, and the
predicate then answers nil for a perfectly fresh pair -- a false miss that
costs a needless rebuild.  Verified 2026-08-10: unset load-path turned a known
hit into a miss."
  (let ((home (nelisp-flat-image-cache-probe--home)))
    (setq load-path
          (cons (expand-file-name "lisp" home)
                (cons (expand-file-name "src" home)
                      (if (boundp 'load-path) load-path nil))))))

(defun nelisp-flat-image-cache-probe--install-reader-shims ()
  "Supply the two host facilities `nelisp-artifact' needs that the reader lacks.
The bare reader route has no buffer layer and no `emacs-pid'; the standalone
artifact bootstraps polyfill the same two names for the same reason."
  (defun nelisp-artifact--read-file-as-string (path)
    (or (nelisp--syscall-read-file path)
        (error "cannot read %s" path)))
  (unless (fboundp 'emacs-pid)
    ;; SYS_getpid on x86_64/aarch64 Linux.  Only feeds temp-file naming.
    (defun emacs-pid () (syscall-direct 39 0 0 0 0 0 0))))

(condition-case nelisp-flat-image-cache-probe--err
    (let ((artifact (nelisp-flat-image-cache-probe--env
                     "NELISP_FLAT_CACHE_ARTIFACT"))
          (image (nelisp-flat-image-cache-probe--env
                  "NELISP_FLAT_CACHE_IMAGE"))
          (runtime (nelisp-flat-image-cache-probe--env
                    "NELISP_FLAT_CACHE_RUNTIME")))
      (unless (and artifact image runtime)
        (error "NELISP_FLAT_CACHE_ARTIFACT/IMAGE/RUNTIME must all be set"))
      (nelisp-flat-image-cache-probe--prepare-load-path)
      (load (nelisp-flat-image-cache-probe--artifact-lisp))
      (nelisp-flat-image-cache-probe--install-reader-shims)
      (unless (fboundp 'nelisp-artifact--flat-image-cache-sidecar-hit)
        (error "nelisp-artifact did not provide the cache predicate"))
      (setq nelisp-flat-image-cache-probe--verdict
            (if (nelisp-artifact--flat-image-cache-sidecar-hit
                 (expand-file-name artifact)
                 (expand-file-name image)
                 (expand-file-name runtime))
                "hit"
              "miss")))
  (error
   (setq nelisp-flat-image-cache-probe--verdict "error")
   (nelisp--write-stderr-line
    (concat "flat-image-cache-probe: "
            (prin1-to-string nelisp-flat-image-cache-probe--err)))))

(nelisp--write-stdout-bytes
 (concat "flat-image-cache-probe=" nelisp-flat-image-cache-probe--verdict "\n"))

0
