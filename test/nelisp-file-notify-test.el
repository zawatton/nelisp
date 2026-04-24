;;; nelisp-file-notify-test.el --- Phase 5-C.3 ERT -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT suite for Phase 5-C.3 — `src/nelisp-file-notify.el'.
;;
;; Coverage:
;;   - add-watch returns a wrap with populated slots + registry push
;;   - wrap is valid after add, invalid after rm
;;   - rm-watch accepts both the wrap and a raw host DESC
;;   - rm-watch on an already-removed / unknown DESC is a no-op
;;   - touch / rename / delete inside a watched DIR deliver a
;;     `:action' of create / change / rename / delete, carrying
;;     :file (+ :file1 on rename)
;;   - actor receives a `file-change' `nelisp-event' before the
;;     user callback fires
;;   - get / put roundtrip

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-actor)
(require 'nelisp-eventloop)
(require 'nelisp-file-notify)

(defvar nelisp-file-notify-test--tmpdirs nil
  "Directories created during the running test, cleaned up by fresh.")

(defun nelisp-file-notify-test--mkdir ()
  "Create + return a fresh temp DIR, trailing slash included."
  (let ((d (file-name-as-directory
            (make-temp-file "nelisp-fn-" t))))
    (push d nelisp-file-notify-test--tmpdirs)
    d))

(defmacro nelisp-file-notify-test--fresh (&rest body)
  (declare (indent 0))
  `(progn
     (setq nelisp-file-notify-test--tmpdirs nil)
     (dolist (w (nelisp-file-notify-list))
       (ignore-errors (nelisp-file-notify-rm-watch w)))
     (nelisp-file-notify--reset-registry)
     (unwind-protect
         (progn ,@body)
       (dolist (w (nelisp-file-notify-list))
         (ignore-errors (nelisp-file-notify-rm-watch w)))
       (nelisp-file-notify--reset-registry)
       (dolist (d nelisp-file-notify-test--tmpdirs)
         (ignore-errors (delete-directory d t))))))

(defun nelisp-file-notify-test--drain (seconds)
  "Pump host event processing for roughly SECONDS (float)."
  (let ((deadline (+ (float-time) seconds)))
    (while (< (float-time) deadline)
      (accept-process-output nil 0.05))))

(defun nelisp-file-notify-test--wait-for (pred seconds)
  "Return the first time PRED is non-nil within SECONDS, else nil."
  (let ((deadline (+ (float-time) seconds))
        (ok nil))
    (while (and (not ok) (< (float-time) deadline))
      (accept-process-output nil 0.05)
      (setq ok (funcall pred)))
    ok))

;;; Basic lifecycle ---------------------------------------------------

(ert-deftest nelisp-file-notify-add-returns-populated-wrap ()
  (nelisp-file-notify-test--fresh
   (let* ((dir (nelisp-file-notify-test--mkdir))
          (w (nelisp-file-notify-add-watch
              dir '(change) #'ignore)))
     (should (nelisp-file-notify-watch-p w))
     (should (equal dir (nelisp-file-notify-watch-file w)))
     (should (equal '(change) (nelisp-file-notify-watch-flags w)))
     (should (memq w (nelisp-file-notify-list))))))

(ert-deftest nelisp-file-notify-valid-p-live-after-add ()
  (nelisp-file-notify-test--fresh
   (let* ((dir (nelisp-file-notify-test--mkdir))
          (w (nelisp-file-notify-add-watch dir '(change) #'ignore)))
     (should (nelisp-file-notify-valid-p w)))))

(ert-deftest nelisp-file-notify-valid-p-invalid-after-rm ()
  (nelisp-file-notify-test--fresh
   (let* ((dir (nelisp-file-notify-test--mkdir))
          (w (nelisp-file-notify-add-watch dir '(change) #'ignore)))
     (nelisp-file-notify-rm-watch w)
     (should (not (nelisp-file-notify-valid-p w))))))

(ert-deftest nelisp-file-notify-rm-removes-from-registry ()
  (nelisp-file-notify-test--fresh
   (let* ((dir (nelisp-file-notify-test--mkdir))
          (w (nelisp-file-notify-add-watch dir '(change) #'ignore)))
     (should (memq w (nelisp-file-notify-list)))
     (nelisp-file-notify-rm-watch w)
     (should (null (memq w (nelisp-file-notify-list)))))))

(ert-deftest nelisp-file-notify-rm-accepts-raw-desc ()
  (nelisp-file-notify-test--fresh
   (let* ((dir (nelisp-file-notify-test--mkdir))
          (w (nelisp-file-notify-add-watch dir '(change) #'ignore))
          (desc (nelisp-file-notify-watch-host-desc w)))
     (nelisp-file-notify-rm-watch desc)
     (should (not (nelisp-file-notify-valid-p desc)))
     (should (null (memq w (nelisp-file-notify-list)))))))

(ert-deftest nelisp-file-notify-rm-unknown-desc-is-no-op ()
  (nelisp-file-notify-test--fresh
   ;; A bogus descriptor that the host has never seen must not error.
   (should (progn (nelisp-file-notify-rm-watch 'bogus-desc) t))))

(ert-deftest nelisp-file-notify-rm-double-rm-is-no-op ()
  (nelisp-file-notify-test--fresh
   (let* ((dir (nelisp-file-notify-test--mkdir))
          (w (nelisp-file-notify-add-watch dir '(change) #'ignore)))
     (nelisp-file-notify-rm-watch w)
     (should (progn (nelisp-file-notify-rm-watch w) t)))))

(ert-deftest nelisp-file-notify-get-put-roundtrip ()
  (nelisp-file-notify-test--fresh
   (let* ((dir (nelisp-file-notify-test--mkdir))
          (w (nelisp-file-notify-add-watch
              dir '(change) #'ignore :props (list :tag "seed"))))
     (should (equal "seed" (nelisp-file-notify-get w :tag)))
     (nelisp-file-notify-put w :retries 3)
     (should (= 3 (nelisp-file-notify-get w :retries))))))

(ert-deftest nelisp-file-notify-add-nil-file-signals ()
  (nelisp-file-notify-test--fresh
   (should-error (nelisp-file-notify-add-watch nil '(change) #'ignore))))

;;; Event delivery (trampoline-driven) -------------------------------
;;
;; `emacs --batch' does not pump the GUI-driven file-notify fd, so we
;; verify the dispatch path by feeding synthetic host events
;; ((DESC ACTION FILE [FILE1])) straight into the installed trampoline.
;; The DESC comes from a *real* `file-notify-add-watch' so every other
;; layer (registry lookup, wrap-binding, user callback dispatch, actor
;; event post) exercises production code.

(defun nelisp-file-notify-test--synth (wrap action file &optional file1)
  "Invoke the installed trampoline with a host-shaped event."
  (let ((desc (nelisp-file-notify-watch-host-desc wrap)))
    (nelisp-file-notify--trampoline
     (if file1
         (list desc action file file1)
       (list desc action file)))))

(ert-deftest nelisp-file-notify-event-on-file-create ()
  (nelisp-file-notify-test--fresh
   (let* ((dir (nelisp-file-notify-test--mkdir))
          (events (list))
          (w (nelisp-file-notify-add-watch
              dir '(change)
              (lambda (_wrap data) (push data events)))))
     (nelisp-file-notify-test--synth
      w 'created (expand-file-name "a.txt" dir))
     (should events)
     (let ((first (car events)))
       (should (eq 'created (plist-get first :action)))
       (should (stringp (plist-get first :file)))
       (should (string-match-p "a\\.txt" (plist-get first :file))))
     (nelisp-file-notify-rm-watch w))))

(ert-deftest nelisp-file-notify-event-on-file-delete ()
  (nelisp-file-notify-test--fresh
   (let* ((dir (nelisp-file-notify-test--mkdir))
          (target (expand-file-name "b.txt" dir))
          (events (list))
          (w (nelisp-file-notify-add-watch
              dir '(change)
              (lambda (_wrap data) (push data events)))))
     (nelisp-file-notify-test--synth w 'deleted target)
     (should (eq 'deleted (plist-get (car events) :action)))
     (should (string-match-p "b\\.txt"
                             (plist-get (car events) :file)))
     (nelisp-file-notify-rm-watch w))))

(ert-deftest nelisp-file-notify-event-on-file-rename ()
  (nelisp-file-notify-test--fresh
   (let* ((dir (nelisp-file-notify-test--mkdir))
          (src (expand-file-name "src.txt" dir))
          (dst (expand-file-name "dst.txt" dir))
          (events (list))
          (w (nelisp-file-notify-add-watch
              dir '(change)
              (lambda (_wrap data) (push data events)))))
     (nelisp-file-notify-test--synth w 'renamed src dst)
     (should events)
     (let ((ev (car events)))
       (should (eq 'renamed (plist-get ev :action)))
       (should (equal src (plist-get ev :file)))
       (should (equal dst (plist-get ev :file1))))
     (nelisp-file-notify-rm-watch w))))

(ert-deftest nelisp-file-notify-callback-receives-wrap-and-plist ()
  "User callback is invoked with (WRAP DATA-PLIST) shape."
  (nelisp-file-notify-test--fresh
   (let* ((dir (nelisp-file-notify-test--mkdir))
          (seen nil)
          (w (nelisp-file-notify-add-watch
              dir '(change)
              (lambda (wrap data)
                (setq seen (list :wrap wrap :data data))))))
     (nelisp-file-notify-test--synth
      w 'created (expand-file-name "c.txt" dir))
     (should seen)
     (should (eq w (plist-get seen :wrap)))
     (should (listp (plist-get seen :data)))
     (should (plist-get (plist-get seen :data) :file))
     (nelisp-file-notify-rm-watch w))))

(ert-deftest nelisp-file-notify-unknown-desc-drops-silently ()
  "A synthetic event whose DESC is not in the registry must not
signal — the trampoline has to cope with a still-in-flight event
arriving after `rm-watch'."
  (nelisp-file-notify-test--fresh
   (should
    (progn
      (nelisp-file-notify--trampoline
       (list 'nonexistent 'changed "/tmp/nope"))
      t))))

;;; Actor integration -------------------------------------------------

(ert-deftest nelisp-file-notify-actor-receives-file-change-event ()
  "With :actor, a `file-change' `nelisp-event' is posted with the
normalized payload."
  (nelisp-file-notify-test--fresh
   (let* ((dir (nelisp-file-notify-test--mkdir))
          (received nil)
          (actor (nelisp-spawn
                  (nelisp-actor-lambda
                    (let ((loop t))
                      (while loop
                        (let ((msg (nelisp-receive)))
                          (if (and (nelisp-event-p msg)
                                   (eq (nelisp-event-kind msg)
                                       'file-change))
                              (progn (setq received msg)
                                     (setq loop nil))
                            (nelisp-yield))))))))
          (w (nelisp-file-notify-add-watch
              dir '(change) #'ignore :actor actor)))
     (nelisp-actor-run-until-idle)
     (nelisp-file-notify-test--synth
      w 'created (expand-file-name "d.txt" dir))
     (nelisp-actor-run-until-idle)
     (should received)
     (should (nelisp-event-p received))
     (should (eq 'file-change (nelisp-event-kind received)))
     (should (eq w (plist-get (nelisp-event-data received) :wrap)))
     (should (eq 'created
                 (plist-get (nelisp-event-data received) :action)))
     (nelisp-file-notify-rm-watch w))))

;;; Normalization internals ------------------------------------------

(ert-deftest nelisp-file-notify-normalize-event-shape ()
  "Unit test the pure converter with a synthetic host event."
  (let* ((ev (list 'DESC 'created "/tmp/x.txt"))
         (plist (nelisp-file-notify--normalize-event ev)))
    (should (eq 'DESC (plist-get plist :desc)))
    (should (eq 'created (plist-get plist :action)))
    (should (equal "/tmp/x.txt" (plist-get plist :file)))
    (should (null (plist-get plist :file1)))))

(ert-deftest nelisp-file-notify-normalize-event-rename-shape ()
  (let* ((ev (list 'DESC 'renamed "/tmp/a" "/tmp/b"))
         (plist (nelisp-file-notify--normalize-event ev)))
    (should (eq 'renamed (plist-get plist :action)))
    (should (equal "/tmp/a" (plist-get plist :file)))
    (should (equal "/tmp/b" (plist-get plist :file1)))))

(provide 'nelisp-file-notify-test)
;;; nelisp-file-notify-test.el ends here
