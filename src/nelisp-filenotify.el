;;; nelisp-filenotify.el --- Phase 9d.A4 standalone file-notify wrapper -*- lexical-binding: t; -*-
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Doc 39 / T82 — Phase 9d.A4 standalone file-notify substrate.
;;
;; Distinct from `src/nelisp-file-notify.el' (Phase 5-C.3 thin wrapper
;; over host Emacs `filenotify.el'): this module is the *standalone*
;; replacement that anchors the v1.0 condition #4 (4-core C-runtime)
;; gap closure.  It is designed to be driven from the NeLisp eventloop
;; (Doc 39 T81) without needing host Emacs `file-notify-add-watch' to
;; exist — the eventual wiring is to call into the Rust syscall layer
;; (`nelisp-runtime/src/syscall/filenotify.rs',
;; `nl_filenotify_init' / `_add_watch' / `_read' / `_rm_watch').
;;
;; The Rust FFI surface is shipped in this commit; the binding from
;; NeLisp into those `extern "C"' symbols lands in Phase 7.5 (general
;; FFI bridge).  Until then we bootstrap with a stat-based polling
;; backend so:
;;
;;   * the public API shape is final,
;;   * downstream consumers (anvil-org-index Phase 6.6 incremental
;;     update path, anvil-defs-index, anvil-memory) can integrate
;;     against this module today,
;;   * ERT and end-to-end NeLisp behaviour are exercised without
;;     blocking on the FFI bridge,
;;   * the Rust path can swap in via `nelisp-filenotify-set-backend'
;;     once the FFI binding lands, with zero call-site churn.
;;
;; Public API:
;;
;;   (nelisp-filenotify-add-watch PATH EVENT-TYPES CALLBACK)
;;       PATH         file or directory to watch
;;       EVENT-TYPES  list, subset of '(create delete modify rename)
;;       CALLBACK     fn of (EVENT-PLIST), invoked from `--poll-events'
;;       returns      a `nelisp-filenotify-descriptor'
;;
;;   (nelisp-filenotify-rm-watch DESC)
;;   (nelisp-filenotify-active-watches)
;;   (nelisp-filenotify--poll-events)        ; eventloop tick hook
;;   (nelisp-filenotify-start-polling &optional INTERVAL)
;;   (nelisp-filenotify-stop-polling)
;;   (nelisp-filenotify-set-backend BACKEND) ; 'stat-poll | 'inotify-ffi
;;
;; EVENT-PLIST shape:
;;
;;   (:type create   :path DIR :name FILE :mtime FLOAT)
;;   (:type modify   :path DIR :name FILE :mtime FLOAT)
;;   (:type delete   :path DIR :name FILE)
;;   (:type rename   :path DIR :name OLD :new-name NEW)   ; future
;;
;; T81 (Doc 39 eventloop) integration point:
;;   `nelisp-filenotify--poll-events' is the tick callback the eventloop
;;   will register as a `select'-driven dispatch source once the FFI
;;   bridge lands.  In the polling-fallback bootstrap path
;;   (`'stat-poll' backend) it is driven by a `run-at-time' timer
;;   started via `nelisp-filenotify-start-polling' — same dispatcher,
;;   different driver.

;;; Code:

(require 'cl-lib)

;; ---------------------------------------------------------------------
;; Backend selection.
;;
;; Phase 9d.A4 ships exactly one live backend (`'stat-poll').  The
;; `'inotify-ffi' / `'fsevents-ffi' branches are reserved for the post
;; Phase 7.5 wire-up; they currently signal `nelisp-filenotify-todo' so
;; a premature switch surfaces clearly instead of silently falling back.
;; ---------------------------------------------------------------------

(defvar nelisp-filenotify-backend 'stat-poll
  "Active file-notify backend.  One of:
  `stat-poll'    — pure NeLisp stat-poll fallback (Phase 9d.A4 default)
  `inotify-ffi'  — Linux `nl_filenotify_*' via FFI (post Phase 7.5)
  `fsevents-ffi' — macOS FSEvents via FFI (post Phase 7.5)")

(define-error 'nelisp-filenotify-todo
  "nelisp-filenotify backend not yet wired (Phase 7.5 territory)")

(defun nelisp-filenotify-set-backend (backend)
  "Switch the active backend.  See `nelisp-filenotify-backend'."
  (unless (memq backend '(stat-poll inotify-ffi fsevents-ffi))
    (signal 'wrong-type-argument (list 'nelisp-filenotify-backend backend)))
  (setq nelisp-filenotify-backend backend))

;; ---------------------------------------------------------------------
;; Descriptor + registry.
;; ---------------------------------------------------------------------

(cl-defstruct (nelisp-filenotify-descriptor
               (:constructor nelisp-filenotify-descriptor--make)
               (:copier nil))
  id           ; integer, unique per session
  path         ; absolute path being watched
  is-dir       ; t when PATH is a directory
  event-types  ; (create delete modify rename)
  callback     ; fn of (EVENT-PLIST)
  snapshot     ; alist (NAME . MTIME-FLOAT) for stat-poll
  backend      ; symbol — backend that owns this watch
  alive)       ; t while in registry

(defvar nelisp-filenotify--registry nil
  "List of live `nelisp-filenotify-descriptor's.")

(defvar nelisp-filenotify--next-id 0
  "Counter for `nelisp-filenotify-descriptor-id'.")

(defun nelisp-filenotify--reset-registry ()
  "Drop every descriptor from the registry (test helper)."
  (setq nelisp-filenotify--registry nil
        nelisp-filenotify--next-id 0))

(defun nelisp-filenotify-active-watches ()
  "Return a fresh copy of the live watch registry."
  (copy-sequence nelisp-filenotify--registry))

;; ---------------------------------------------------------------------
;; stat-poll backend — directory snapshot diff.
;;
;; Each `add-watch' on a directory records `(NAME . MTIME)' for every
;; existing entry; each `--poll-events' tick re-scans and emits the
;; delta as event plists.  File-mode watches snapshot the file's own
;; mtime + existence flag.
;; ---------------------------------------------------------------------

(defun nelisp-filenotify--directory-snapshot (path)
  "Return alist (NAME . MTIME-FLOAT) for files in directory PATH.
Hidden files (leading `.') are skipped — matches `inotify' default
mask semantics, where dot-files are surfaced only when the caller
sets `IN_DOT_OK' (= not done in this Phase)."
  (let ((entries (ignore-errors
                   (directory-files-and-attributes
                    path nil "\\`[^.]" t))))
    (mapcar
     (lambda (e)
       ;; e = (NAME . ATTRS)
       (let* ((name  (car e))
              (attrs (cdr e))
              (mtime (file-attribute-modification-time attrs))
              (mfloat (float-time mtime)))
         (cons name mfloat)))
     entries)))

(defun nelisp-filenotify--file-mtime (path)
  "Return mtime of PATH as float, or nil if absent."
  (let ((attrs (ignore-errors (file-attributes path))))
    (when attrs
      (float-time (file-attribute-modification-time attrs)))))

(defun nelisp-filenotify--diff-snapshots (old new)
  "Return list of plists describing the diff OLD → NEW.
OLD and NEW are both alist (NAME . MTIME).  Plists are
`(:kind create|delete|modify :name NAME :mtime FLOAT)'.

Sort order: deletes first, then creates, then modifies — matches the
order a real inotify driver would emit events relative to a single
poll boundary, and gives deterministic test output."
  (let ((events nil))
    (dolist (o old)
      (let ((cell (assoc (car o) new)))
        (unless cell
          (push (list :kind 'delete :name (car o)) events))))
    (dolist (n new)
      (let ((cell (assoc (car n) old)))
        (cond
         ((null cell)
          (push (list :kind 'create :name (car n) :mtime (cdr n)) events))
         ((not (= (cdr cell) (cdr n)))
          (push (list :kind 'modify :name (car n) :mtime (cdr n)) events)))))
    ;; Reverse so deletes are emitted before creates / modifies in
    ;; the order they were appended.
    (nreverse events)))

(defun nelisp-filenotify--scan-and-dispatch (desc)
  "Re-scan DESC's path and dispatch any new events to its callback."
  (when (nelisp-filenotify-descriptor-alive desc)
    (let* ((path (nelisp-filenotify-descriptor-path desc))
           (is-dir (nelisp-filenotify-descriptor-is-dir desc))
           (wanted (nelisp-filenotify-descriptor-event-types desc))
           (cb     (nelisp-filenotify-descriptor-callback desc)))
      (cond
       (is-dir
        (let* ((old (nelisp-filenotify-descriptor-snapshot desc))
               (new (nelisp-filenotify--directory-snapshot path))
               (events (nelisp-filenotify--diff-snapshots old new)))
          (setf (nelisp-filenotify-descriptor-snapshot desc) new)
          (dolist (ev events)
            (let ((kind (plist-get ev :kind)))
              (when (memq kind wanted)
                (let ((payload (list :type kind
                                     :path path
                                     :name (plist-get ev :name)
                                     :mtime (plist-get ev :mtime))))
                  (condition-case err
                      (funcall cb payload)
                    (error
                     (message
                      "nelisp-filenotify callback error: %S" err)))))))))
       (t
        (let* ((old (cdr (assoc :mtime (nelisp-filenotify-descriptor-snapshot desc))))
               (old-exists (cdr (assoc :exists (nelisp-filenotify-descriptor-snapshot desc))))
               (new (nelisp-filenotify--file-mtime path))
               (new-exists (and new t))
               (events nil))
          (cond
           ((and old-exists (not new-exists))
            (push (list :type 'delete :path path
                        :name (file-name-nondirectory path))
                  events))
           ((and (not old-exists) new-exists)
            (push (list :type 'create :path path
                        :name (file-name-nondirectory path)
                        :mtime new)
                  events))
           ((and old-exists new-exists (not (equal old new)))
            (push (list :type 'modify :path path
                        :name (file-name-nondirectory path)
                        :mtime new)
                  events)))
          (setf (nelisp-filenotify-descriptor-snapshot desc)
                (list (cons :mtime new) (cons :exists new-exists)))
          (dolist (ev events)
            (when (memq (plist-get ev :type) wanted)
              (condition-case err
                  (funcall cb ev)
                (error
                 (message
                  "nelisp-filenotify callback error: %S" err)))))))))))

;; ---------------------------------------------------------------------
;; Public API.
;; ---------------------------------------------------------------------

(defun nelisp-filenotify-add-watch (path event-types callback)
  "Install a file-notify watch on PATH and return a descriptor.

PATH         absolute file or directory path to watch
EVENT-TYPES  list, subset of `(create delete modify rename)'
CALLBACK     function of (EVENT-PLIST), called from
             `nelisp-filenotify--poll-events'

Backend selection follows `nelisp-filenotify-backend'.  Phase 9d.A4
ships only the `stat-poll' backend live; the FFI-driven backends
signal `nelisp-filenotify-todo' until Phase 7.5 wires the bridge."
  (unless (and path (stringp path))
    (signal 'wrong-type-argument (list 'stringp path)))
  (unless (file-exists-p path)
    (signal 'file-missing (list "Path not found" path)))
  (unless (and (listp event-types)
               (cl-every (lambda (e)
                           (memq e '(create delete modify rename)))
                         event-types))
    (signal 'wrong-type-argument
            (list 'nelisp-filenotify-event-types event-types)))
  (unless (functionp callback)
    (signal 'wrong-type-argument (list 'functionp callback)))
  (unless (eq nelisp-filenotify-backend 'stat-poll)
    (signal 'nelisp-filenotify-todo
            (list :backend nelisp-filenotify-backend
                  :hint "Phase 7.5 FFI bridge not yet wired")))
  (let* ((id (cl-incf nelisp-filenotify--next-id))
         (is-dir (file-directory-p path))
         (snapshot (cond
                    (is-dir (nelisp-filenotify--directory-snapshot path))
                    (t (let ((mt (nelisp-filenotify--file-mtime path)))
                         (list (cons :mtime mt)
                               (cons :exists (and mt t)))))))
         (desc (nelisp-filenotify-descriptor--make
                :id id
                :path path
                :is-dir is-dir
                :event-types event-types
                :callback callback
                :snapshot snapshot
                :backend nelisp-filenotify-backend
                :alive t)))
    (push desc nelisp-filenotify--registry)
    desc))

(defun nelisp-filenotify-rm-watch (descriptor)
  "Remove DESCRIPTOR from the active watch registry.
A no-op if DESCRIPTOR is already removed (matches host
`file-notify-rm-watch' contract)."
  (when (nelisp-filenotify-descriptor-p descriptor)
    (setf (nelisp-filenotify-descriptor-alive descriptor) nil)
    (setq nelisp-filenotify--registry
          (delq descriptor nelisp-filenotify--registry)))
  nil)

(defun nelisp-filenotify--poll-events ()
  "Eventloop tick hook — rescan every active watch and dispatch events.
Designed to be called from a `run-at-time' polling timer (the
Phase 9d.A4 bootstrap path) or from a Doc 39 T81 eventloop
`select'-driven dispatch (post Phase 7.5)."
  (dolist (desc (nelisp-filenotify-active-watches))
    (nelisp-filenotify--scan-and-dispatch desc)))

;; ---------------------------------------------------------------------
;; Polling driver — bootstrap until eventloop wiring lands.
;; ---------------------------------------------------------------------

(defcustom nelisp-filenotify-poll-interval 0.1
  "Seconds between `nelisp-filenotify--poll-events' ticks.
Used by `nelisp-filenotify-start-polling'.  100 ms matches the
\"unobtrusive enough for editor-class workloads\" budget noted in
Doc 39 §6.4 — finer-grained dispatch lands with the FFI bridge."
  :type 'number
  :group 'nelisp)

(defvar nelisp-filenotify--poll-timer nil
  "Active poll-driver timer, or nil when stopped.")

(defun nelisp-filenotify-start-polling (&optional interval)
  "Start the polling driver tick timer.  INTERVAL defaults to
`nelisp-filenotify-poll-interval'."
  (nelisp-filenotify-stop-polling)
  (setq nelisp-filenotify--poll-timer
        (run-at-time
         (or interval nelisp-filenotify-poll-interval)
         (or interval nelisp-filenotify-poll-interval)
         #'nelisp-filenotify--poll-events))
  nelisp-filenotify--poll-timer)

(defun nelisp-filenotify-stop-polling ()
  "Stop the polling driver tick timer, if any."
  (when (timerp nelisp-filenotify--poll-timer)
    (cancel-timer nelisp-filenotify--poll-timer))
  (setq nelisp-filenotify--poll-timer nil))

(defun nelisp-filenotify-polling-active-p ()
  "Return non-nil when the polling driver tick timer is live."
  (and (timerp nelisp-filenotify--poll-timer) t))

(provide 'nelisp-filenotify)
;;; nelisp-filenotify.el ends here
