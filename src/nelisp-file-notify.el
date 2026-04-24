;;; nelisp-file-notify.el --- NeLisp file-notify substrate -*- lexical-binding: t; -*-
;;
;; Phase 5-C.3 per Doc 14.  Thin wrapper over host
;; `file-notify-add-watch' / `file-notify-rm-watch' / `file-notify-
;; valid-p' that normalises the host CALLBACK event
;;
;;     (DESC ACTION FILE [FILE1])
;;
;; into a `nelisp-event' of kind `file-change' with a plist payload,
;; and dispatches optional user callbacks via `nelisp--apply' (so
;; either host lambdas or NeLisp closures may be used).
;;
;; Scope per Doc 14 §2.4 A:
;;   - inotify / kqueue / Win32 backend choice is left to host
;;     `file-notify' — we do NOT reimplement any backend
;;   - event-loop integration is optional (`:actor' kwarg); the
;;     wrapper remains usable as a plain callback shim

;;; Code:

(require 'cl-lib)
(require 'nelisp-eval)
(require 'nelisp-actor)
(require 'filenotify)
(autoload 'nelisp-make-event "nelisp-eventloop")

(cl-defstruct (nelisp-file-notify-watch
               (:constructor nelisp-file-notify-watch--make)
               (:copier nil))
  id                   ; symbol / string label, defaults to the DIR
  host-desc            ; opaque descriptor returned by host
  file
  flags
  (actor nil)
  user-callback
  (props nil))

(defvar nelisp-file-notify--registry nil
  "List of live `nelisp-file-notify-watch' wraps.")

(defun nelisp-file-notify--reset-registry ()
  "Drop every wrap from the registry (test helper).
Does NOT call `file-notify-rm-watch' — callers that need a clean
host state should kill watches first via `nelisp-file-notify-rm-watch'."
  (setq nelisp-file-notify--registry nil))

(defun nelisp-file-notify--find-by-desc (host-desc)
  (cl-find host-desc nelisp-file-notify--registry
           :key #'nelisp-file-notify-watch-host-desc :test #'equal))

(defun nelisp-file-notify--normalize-event (host-event)
  "Convert host file-notify event HOST-EVENT to a data plist.
HOST-EVENT is of the shape (DESC ACTION FILE [FILE1])."
  (let ((desc   (nth 0 host-event))
        (action (nth 1 host-event))
        (file   (nth 2 host-event))
        (file1  (nth 3 host-event)))
    (list :desc desc :action action :file file :file1 file1)))

(defun nelisp-file-notify--post-event (wrap data)
  (let ((actor (nelisp-file-notify-watch-actor wrap)))
    (when (and actor
               (nelisp-actor-p actor)
               (fboundp 'nelisp-make-event)
               (not (memq (nelisp-actor-status actor)
                          '(:dead :crashed))))
      (nelisp-send actor (nelisp-make-event 'file-change data)))))

(defun nelisp-file-notify--trampoline (host-event)
  "Installed as the host CALLBACK for every wrap.  Looks up the
owning wrap by DESC, posts a `file-change' event to the wrap's
actor, then dispatches the user callback via `nelisp--apply'."
  (let* ((desc (car host-event))
         (wrap (nelisp-file-notify--find-by-desc desc)))
    (when wrap
      (let* ((data (nelisp-file-notify--normalize-event host-event))
             (payload (append (list :wrap wrap) data)))
        (nelisp-file-notify--post-event wrap payload)
        (let ((cb (nelisp-file-notify-watch-user-callback wrap)))
          (when cb
            (condition-case err
                (nelisp--apply cb (list wrap data))
              (error (message "nelisp-file-notify callback error: %S"
                              err)))))))))

;;; Public API --------------------------------------------------------

(cl-defun nelisp-file-notify-add-watch (file flags callback
                                             &key name actor props)
  "Install a file-notify watch on FILE and return a wrap.

Arguments:
  FILE      Path to watch (directory or file).  Required.
  FLAGS     List accepted by host `file-notify-add-watch', e.g.
            `(change)' or `(change attribute-change)'.
  CALLBACK  Function of (WRAP DATA-PLIST) — host lambda or NeLisp
            closure, dispatched via `nelisp--apply'.  DATA-PLIST
            carries :desc / :action / :file / :file1.
  NAME      Optional label; defaults to FILE.
  ACTOR     Optional Phase 4 actor; when non-nil a `file-change'
            `nelisp-event' is posted before the user callback fires.
  PROPS     Initial plist for `-get' / `-put'."
  (unless file
    (signal 'wrong-type-argument (list 'stringp file)))
  (let* ((host-desc
          (file-notify-add-watch
           file flags #'nelisp-file-notify--trampoline))
         (wrap (nelisp-file-notify-watch--make
                :id (or name file)
                :host-desc host-desc
                :file file
                :flags flags
                :actor actor
                :user-callback callback
                :props props)))
    (push wrap nelisp-file-notify--registry)
    wrap))

(defun nelisp-file-notify-rm-watch (wrap-or-desc)
  "Remove the watch for WRAP-OR-DESC.

Accepts either a `nelisp-file-notify-watch' wrap or a raw host
descriptor (for symmetry with host `file-notify-rm-watch').  A
wrap whose host descriptor is already invalid is quietly removed
from the registry — this mirrors the host no-op contract."
  (let* ((wrap (cond
                ((nelisp-file-notify-watch-p wrap-or-desc) wrap-or-desc)
                (t (nelisp-file-notify--find-by-desc wrap-or-desc))))
         (desc (cond
                ((nelisp-file-notify-watch-p wrap-or-desc)
                 (nelisp-file-notify-watch-host-desc wrap-or-desc))
                (t wrap-or-desc))))
    (when desc
      (ignore-errors (file-notify-rm-watch desc)))
    (when wrap
      (setq nelisp-file-notify--registry
            (delq wrap nelisp-file-notify--registry)))
    nil))

(defun nelisp-file-notify-valid-p (wrap-or-desc)
  "Return t if the watch is still live on the host side."
  (let ((desc
         (cond
          ((nelisp-file-notify-watch-p wrap-or-desc)
           (nelisp-file-notify-watch-host-desc wrap-or-desc))
          (t wrap-or-desc))))
    (and desc (file-notify-valid-p desc))))

(defun nelisp-file-notify-list ()
  "Return a fresh copy of the live watch registry."
  (copy-sequence nelisp-file-notify--registry))

(defun nelisp-file-notify-get (wrap key)
  (plist-get (nelisp-file-notify-watch-props wrap) key))

(defun nelisp-file-notify-put (wrap key value)
  (setf (nelisp-file-notify-watch-props wrap)
        (plist-put (nelisp-file-notify-watch-props wrap) key value))
  value)

(provide 'nelisp-file-notify)
;;; nelisp-file-notify.el ends here
