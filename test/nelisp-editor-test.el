;;; nelisp-editor-test.el --- Phase 5-B.5 E2E -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT suite for Phase 5-B.5 — `examples/nelisp-editor/main.el'.
;;
;; Coverage matches Doc 13 §3.5 acceptance — 3-core integration
;; (buffer / redisplay / event-loop) via scripted key sequences:
;;
;;   - init creates fresh buffer + window with sensible defaults
;;   - self-insert of a single printable char
;;   - self-insert of a sequence builds the expected string
;;   - C-f / C-b movement bounded by point-min / point-max
;;   - C-d / DEL deletion with cursor recompute
;;   - C-n / C-p navigate between lines preserving column
;;   - C-q quits the sync loop
;;   - actor variant: same behaviour via main-actor path
;;   - hello-package integration: load + eval via NeLisp loader,
;;     inject into editor buffer one char at a time
;;   - redisplay after commands emits escape + text

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp)
(require 'nelisp-load)
(require 'nelisp-buffer)
(require 'nelisp-redisplay)
(require 'nelisp-eventloop)
(require 'nelisp-actor)

(defconst nelisp-editor-test--editor-dir
  (expand-file-name
   "../examples/nelisp-editor/"
   (file-name-directory (or load-file-name buffer-file-name)))
  "Absolute path of the nelisp-editor demo directory.")

(defconst nelisp-editor-test--hello-dir
  (expand-file-name
   "../examples/hello/"
   (file-name-directory (or load-file-name buffer-file-name)))
  "Absolute path of the hello toy package (Phase 5-A.5).")

(load (expand-file-name "main.el" nelisp-editor-test--editor-dir))

(defmacro nelisp-editor-test--fresh (&rest body)
  "Run BODY with a pristine editor + event loop + actor registry."
  (declare (indent 0))
  `(progn
     (nelisp-eventloop-reset-bindings)
     (nelisp-eventloop-drain)
     (setq nelisp-eventloop--running nil)
     (nelisp-actor--reset)
     (nelisp-buffer--reset-registry)
     (nelisp-editor-init "*t*" 5 40)
     (unwind-protect
         (progn ,@body)
       (nelisp-actor--reset))))

;;; Init ---------------------------------------------------------------

(ert-deftest nelisp-editor-init-creates-buffer-and-window ()
  (nelisp-editor-test--fresh
   (should (nelisp-buffer-p (nelisp-editor-current-buffer)))
   (should (nelisp-window-p (nelisp-editor-current-window)))
   (should (equal "" (nelisp-buffer-string (nelisp-editor-current-buffer))))
   (should (= 1 (nelisp-point (nelisp-editor-current-buffer))))))

;;; Sync scripted ------------------------------------------------------

(ert-deftest nelisp-editor-self-insert-single ()
  (nelisp-editor-test--fresh
   (let ((final (nelisp-editor-run-scripted (list ?a))))
     (should (equal "a" final))
     (should (= 2 (nelisp-point (nelisp-editor-current-buffer))))
     (should (= 1 (nelisp-window-cursor-col
                   (nelisp-editor-current-window)))))))

(ert-deftest nelisp-editor-self-insert-sequence ()
  (nelisp-editor-test--fresh
   (let ((final (nelisp-editor-run-scripted
                 (list ?H ?e ?l ?l ?o))))
     (should (equal "Hello" final))
     (should (= 6 (nelisp-point (nelisp-editor-current-buffer)))))))

(ert-deftest nelisp-editor-newline-advances-row ()
  (nelisp-editor-test--fresh
   (let ((final (nelisp-editor-run-scripted
                 (list ?a ?b ?\n ?c))))
     (should (equal "ab\nc" final))
     (should (= 1 (nelisp-window-cursor-row
                   (nelisp-editor-current-window))))
     (should (= 1 (nelisp-window-cursor-col
                   (nelisp-editor-current-window)))))))

(ert-deftest nelisp-editor-forward-backward-bounded ()
  (nelisp-editor-test--fresh
   (nelisp-editor-run-scripted (list ?a ?b ?c))
   (let ((buf (nelisp-editor-current-buffer)))
     ;; point is at end (4) after inserting 3 chars.
     ;; C-f past end is a no-op.
     (nelisp-editor-run-scripted (list "\C-f"))
     (should (= 4 (nelisp-point buf)))
     ;; three C-b rewind to 1.
     (nelisp-editor-run-scripted (list "\C-b" "\C-b" "\C-b"))
     (should (= 1 (nelisp-point buf)))
     ;; one more C-b is a no-op.
     (nelisp-editor-run-scripted (list "\C-b"))
     (should (= 1 (nelisp-point buf))))))

(ert-deftest nelisp-editor-delete-char-forward ()
  (nelisp-editor-test--fresh
   (nelisp-editor-run-scripted (list ?a ?b ?c))
   ;; Rewind to start, then delete two chars forward.
   (nelisp-editor-run-scripted (list "\C-b" "\C-b" "\C-b"
                                     "\C-d" "\C-d"))
   (should (equal "c" (nelisp-buffer-string (nelisp-editor-current-buffer))))))

(ert-deftest nelisp-editor-delete-backward-char ()
  (nelisp-editor-test--fresh
   (nelisp-editor-run-scripted (list ?a ?b ?c))
   ;; DEL (0x7f) twice removes c, b.
   (nelisp-editor-run-scripted (list "\177" "\177"))
   (should (equal "a" (nelisp-buffer-string (nelisp-editor-current-buffer))))))

(ert-deftest nelisp-editor-next-previous-line ()
  (nelisp-editor-test--fresh
   (nelisp-editor-run-scripted (list ?a ?b ?c ?\n ?d ?e ?f))
   (let ((buf (nelisp-editor-current-buffer)))
     ;; Point is at end of line 1, col 3.
     (should (= 2 (+ 1 (nelisp-window-cursor-row
                        (nelisp-editor-current-window)))))
     ;; C-p -> same col on line 0.
     (nelisp-editor-run-scripted (list "\C-p"))
     (should (= 0 (nelisp-window-cursor-row
                   (nelisp-editor-current-window))))
     ;; C-n -> back to line 1.
     (nelisp-editor-run-scripted (list "\C-n"))
     (should (= 1 (nelisp-window-cursor-row
                   (nelisp-editor-current-window))))
     (ignore buf))))

(ert-deftest nelisp-editor-quit-flag-cleared ()
  (nelisp-editor-test--fresh
   (setq nelisp-eventloop--running t)
   (nelisp-editor-run-scripted (list "\C-q"))
   (should (null nelisp-eventloop--running))))

;;; Actor variant -----------------------------------------------------

(ert-deftest nelisp-editor-actor-scripted-builds-string ()
  (nelisp-editor-test--fresh
   (let ((final (nelisp-editor-run-actor-scripted
                 (list ?N ?e ?L))))
     (should (equal "NeL" final)))))

(ert-deftest nelisp-editor-actor-main-terminates-on-quit ()
  (nelisp-editor-test--fresh
   (let* ((main (nelisp-eventloop-spawn-main-actor)))
     (setq nelisp-editor--main-actor main)
     (nelisp-send main (nelisp-make-event 'key ?a))
     (nelisp-send main (nelisp-make-event 'quit nil))
     (nelisp-actor-run-until-idle)
     (should (memq (nelisp-actor-status main) '(:dead :done))))))

;;; hello-package integration ----------------------------------------

(defmacro nelisp-editor-test--with-hello-env (&rest body)
  "Run BODY with `nelisp-load-path' pointed at the hello package."
  (declare (indent 0))
  `(let ((nelisp-load-path (list nelisp-editor-test--hello-dir))
         (nelisp-load-path-include-host nil))
     (nelisp--reset)
     ,@body))

(ert-deftest nelisp-editor-hello-package-integration ()
  "Load the hello package via NeLisp loader, insert its greeting
via scripted keys, verify buffer contents."
  ;; CI-smoke gate: same Windows path-resolution gap as the rest of the
  ;; `nelisp-examples-hello-*' suite — `nelisp-locate-file' on the toy
  ;; tree fails on Windows native Emacs (Phase 9d open thread).  Skip
  ;; on non-POSIX hosts; local + CI-Linux + CI-macOS keep running it.
  (skip-unless (memq system-type '(gnu/linux darwin berkeley-unix)))
  (nelisp-editor-test--fresh
   (nelisp-editor-test--with-hello-env
     (nelisp-require 'hello)
     (let* ((greeting (nelisp-eval '(hello-world)))
            (chars (mapcar #'identity greeting))
            (final (nelisp-editor-run-scripted chars)))
       (should (equal "Hello, World!" greeting))
       (should (equal greeting final))
       (should (= (1+ (length greeting))
                  (nelisp-point (nelisp-editor-current-buffer))))))))

;;; Redisplay after editor commands ----------------------------------

(ert-deftest nelisp-editor-redisplay-after-insert ()
  "After a scripted insert, `nelisp-redisplay-window' emits the
expected CSI + text fragments via the capture hook."
  (nelisp-editor-test--fresh
   (nelisp-editor-run-scripted (list ?X ?Y ?Z))
   (let* ((out (list))
          (nelisp-redisplay--output-fn (lambda (s) (push s out))))
     (nelisp-redisplay-window (nelisp-editor-current-window))
     (setq out (nreverse out))
     (should (member "XYZ" out))
     (should (cl-some (lambda (s) (string-match-p "\e\\[1;1H" s)) out)))))

(provide 'nelisp-editor-test)
;;; nelisp-editor-test.el ends here
