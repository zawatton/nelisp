;;; nelisp-filenotify-test.el --- Phase 9d.A4 ERT -*- lexical-binding: t; -*-
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; ERT suite for `src/nelisp-filenotify.el' — the Phase 9d.A4
;; standalone file-notify wrapper (Doc 39 / T82).
;;
;; Distinct from `nelisp-file-notify-test.el', which exercises the
;; Phase 5-C.3 thin wrapper over host `filenotify.el'.  This suite
;; targets the standalone polling backend that bootstraps the v1.0
;; condition #4 (4-core C-runtime) gap closure.
;;
;; Coverage:
;;   * add-watch input validation (path, event-types, callback)
;;   * descriptor structure + registry membership
;;   * stat-poll snapshot diff (create / delete / modify / quiet)
;;   * directory + single-file watch shapes
;;   * event-types subset filtering
;;   * rm-watch contract (idempotent, registry drop)
;;   * polling timer lifecycle (start / stop / active-p)
;;   * backend switch validation (only `stat-poll' live in 9d.A4)

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-filenotify)

(defvar nelisp-filenotify-test--tmpdirs nil
  "Temp directories created during the running test, cleaned up by --fresh.")

(defun nelisp-filenotify-test--mkdir ()
  "Create + return a fresh temp DIR (no trailing slash)."
  (let ((d (make-temp-file "nelisp-fn-9d-a4-" t)))
    (push d nelisp-filenotify-test--tmpdirs)
    d))

(defun nelisp-filenotify-test--mkfile (dir name &optional content)
  "Write CONTENT (or 'init') to DIR/NAME and return the path."
  (let ((p (expand-file-name name dir)))
    (with-temp-file p
      (insert (or content "init")))
    p))

(defmacro nelisp-filenotify-test--fresh (&rest body)
  (declare (indent 0))
  `(let ((nelisp-filenotify-test--tmpdirs nil))
     (nelisp-filenotify-stop-polling)
     (nelisp-filenotify-set-backend 'stat-poll)
     (nelisp-filenotify--reset-registry)
     (unwind-protect
         (progn ,@body)
       (nelisp-filenotify-stop-polling)
       (dolist (d nelisp-filenotify-test--tmpdirs)
         (ignore-errors (delete-directory d t)))
       (nelisp-filenotify--reset-registry))))

;;; Input validation --------------------------------------------------

(ert-deftest nelisp-filenotify-add-rejects-nil-path ()
  (nelisp-filenotify-test--fresh
    (should-error (nelisp-filenotify-add-watch nil '(create) #'ignore))))

(ert-deftest nelisp-filenotify-add-rejects-missing-path ()
  (nelisp-filenotify-test--fresh
    (should-error
     (nelisp-filenotify-add-watch
      "/nelisp/9d-a4/no/such/path" '(create) #'ignore))))

(ert-deftest nelisp-filenotify-add-rejects-unknown-event-type ()
  (nelisp-filenotify-test--fresh
    (let ((d (nelisp-filenotify-test--mkdir)))
      (should-error
       (nelisp-filenotify-add-watch d '(bogus-event) #'ignore)))))

(ert-deftest nelisp-filenotify-add-rejects-non-function-callback ()
  (nelisp-filenotify-test--fresh
    (let ((d (nelisp-filenotify-test--mkdir)))
      (should-error
       (nelisp-filenotify-add-watch d '(create) "not-a-fn")))))

;;; Descriptor + registry ---------------------------------------------

(ert-deftest nelisp-filenotify-add-returns-populated-descriptor ()
  (nelisp-filenotify-test--fresh
    (let* ((d (nelisp-filenotify-test--mkdir))
           (desc (nelisp-filenotify-add-watch
                  d '(create modify) #'ignore)))
      (should (nelisp-filenotify-descriptor-p desc))
      (should (equal d (nelisp-filenotify-descriptor-path desc)))
      (should (nelisp-filenotify-descriptor-is-dir desc))
      (should (equal '(create modify)
                     (nelisp-filenotify-descriptor-event-types desc)))
      (should (memq desc (nelisp-filenotify-active-watches))))))

(ert-deftest nelisp-filenotify-rm-removes-from-registry ()
  (nelisp-filenotify-test--fresh
    (let* ((d (nelisp-filenotify-test--mkdir))
           (desc (nelisp-filenotify-add-watch d '(create) #'ignore)))
      (should (memq desc (nelisp-filenotify-active-watches)))
      (nelisp-filenotify-rm-watch desc)
      (should (null (memq desc (nelisp-filenotify-active-watches))))
      (should (not (nelisp-filenotify-descriptor-alive desc))))))

(ert-deftest nelisp-filenotify-rm-double-rm-is-no-op ()
  (nelisp-filenotify-test--fresh
    (let* ((d (nelisp-filenotify-test--mkdir))
           (desc (nelisp-filenotify-add-watch d '(create) #'ignore)))
      (nelisp-filenotify-rm-watch desc)
      (should (progn (nelisp-filenotify-rm-watch desc) t)))))

;;; stat-poll snapshot diff -------------------------------------------

(ert-deftest nelisp-filenotify-detects-file-create ()
  (nelisp-filenotify-test--fresh
    (let* ((dir (nelisp-filenotify-test--mkdir))
           (events nil)
           (desc (nelisp-filenotify-add-watch
                  dir '(create modify)
                  (lambda (ev) (push ev events)))))
      (nelisp-filenotify-test--mkfile dir "foo.el")
      (nelisp-filenotify--poll-events)
      (should (= 1 (length events)))
      (should (eq 'create (plist-get (car events) :type)))
      (should (equal "foo.el" (plist-get (car events) :name)))
      (should (numberp (plist-get (car events) :mtime)))
      (nelisp-filenotify-rm-watch desc))))

(ert-deftest nelisp-filenotify-detects-file-delete ()
  (nelisp-filenotify-test--fresh
    (let* ((dir (nelisp-filenotify-test--mkdir))
           (path (nelisp-filenotify-test--mkfile dir "victim.el"))
           (events nil)
           (desc (nelisp-filenotify-add-watch
                  dir '(create delete)
                  (lambda (ev) (push ev events)))))
      (delete-file path)
      (nelisp-filenotify--poll-events)
      (should (= 1 (length events)))
      (should (eq 'delete (plist-get (car events) :type)))
      (should (equal "victim.el" (plist-get (car events) :name)))
      (nelisp-filenotify-rm-watch desc))))

(ert-deftest nelisp-filenotify-detects-file-modify ()
  (nelisp-filenotify-test--fresh
    (let* ((dir (nelisp-filenotify-test--mkdir))
           (path (nelisp-filenotify-test--mkfile dir "data.el" "v1"))
           (events nil)
           (desc (nelisp-filenotify-add-watch
                  dir '(modify)
                  (lambda (ev) (push ev events)))))
      ;; Force a mtime jump — float-time precision is ms-class, so a
      ;; 1.1s sleep is the simplest portable way to guarantee the
      ;; snapshot diff sees a change without depending on filesystem
      ;; nanosecond support.
      (sleep-for 1.1)
      (with-temp-file path (insert "v2-different-content"))
      (nelisp-filenotify--poll-events)
      (should (= 1 (length events)))
      (should (eq 'modify (plist-get (car events) :type)))
      (should (equal "data.el" (plist-get (car events) :name)))
      (nelisp-filenotify-rm-watch desc))))

(ert-deftest nelisp-filenotify-quiet-poll-emits-nothing ()
  (nelisp-filenotify-test--fresh
    (let* ((dir (nelisp-filenotify-test--mkdir))
           (events nil)
           (desc (nelisp-filenotify-add-watch
                  dir '(create modify delete)
                  (lambda (ev) (push ev events)))))
      (nelisp-filenotify-test--mkfile dir "stable.el")
      ;; First poll: emits the initial create.
      (nelisp-filenotify--poll-events)
      ;; Second poll with no fs change: should be a no-op.
      (let ((after (length events)))
        (nelisp-filenotify--poll-events)
        (should (= after (length events))))
      (nelisp-filenotify-rm-watch desc))))

(ert-deftest nelisp-filenotify-event-types-filter-suppresses-create ()
  "A descriptor that does NOT request `create' must not see one."
  (nelisp-filenotify-test--fresh
    (let* ((dir (nelisp-filenotify-test--mkdir))
           (events nil)
           (desc (nelisp-filenotify-add-watch
                  dir '(delete)
                  (lambda (ev) (push ev events)))))
      (nelisp-filenotify-test--mkfile dir "appears.el")
      (nelisp-filenotify--poll-events)
      (should (null events))
      (nelisp-filenotify-rm-watch desc))))

(ert-deftest nelisp-filenotify-multi-event-batch ()
  "Multiple file changes between polls are reported together."
  (nelisp-filenotify-test--fresh
    (let* ((dir (nelisp-filenotify-test--mkdir))
           (events nil)
           (desc (nelisp-filenotify-add-watch
                  dir '(create modify delete)
                  (lambda (ev) (push ev events)))))
      (nelisp-filenotify-test--mkfile dir "a.el")
      (nelisp-filenotify-test--mkfile dir "b.el")
      (nelisp-filenotify-test--mkfile dir "c.el")
      (nelisp-filenotify--poll-events)
      (should (= 3 (length events)))
      (should (cl-every (lambda (ev) (eq 'create (plist-get ev :type)))
                        events))
      (let ((names (sort (mapcar (lambda (e) (plist-get e :name)) events)
                         #'string<)))
        (should (equal '("a.el" "b.el" "c.el") names)))
      (nelisp-filenotify-rm-watch desc))))

;;; Single-file watch shape -------------------------------------------

(ert-deftest nelisp-filenotify-single-file-watch-detects-modify ()
  (nelisp-filenotify-test--fresh
    (let* ((dir  (nelisp-filenotify-test--mkdir))
           (path (nelisp-filenotify-test--mkfile dir "lone.el" "v1"))
           (events nil)
           (desc (nelisp-filenotify-add-watch
                  path '(modify)
                  (lambda (ev) (push ev events)))))
      (should-not (nelisp-filenotify-descriptor-is-dir desc))
      (sleep-for 1.1)
      (with-temp-file path (insert "v2"))
      (nelisp-filenotify--poll-events)
      (should (= 1 (length events)))
      (should (eq 'modify (plist-get (car events) :type)))
      (should (equal "lone.el" (plist-get (car events) :name)))
      (nelisp-filenotify-rm-watch desc))))

(ert-deftest nelisp-filenotify-single-file-watch-detects-delete ()
  (nelisp-filenotify-test--fresh
    (let* ((dir  (nelisp-filenotify-test--mkdir))
           (path (nelisp-filenotify-test--mkfile dir "ephemeral.el"))
           (events nil)
           (desc (nelisp-filenotify-add-watch
                  path '(delete)
                  (lambda (ev) (push ev events)))))
      (delete-file path)
      (nelisp-filenotify--poll-events)
      (should (= 1 (length events)))
      (should (eq 'delete (plist-get (car events) :type)))
      (nelisp-filenotify-rm-watch desc))))

;;; Polling timer lifecycle -------------------------------------------

(ert-deftest nelisp-filenotify-start-stop-polling-roundtrip ()
  (nelisp-filenotify-test--fresh
    (should-not (nelisp-filenotify-polling-active-p))
    (nelisp-filenotify-start-polling 60.0) ; long interval — no fire
    (should (nelisp-filenotify-polling-active-p))
    (nelisp-filenotify-stop-polling)
    (should-not (nelisp-filenotify-polling-active-p))))

(ert-deftest nelisp-filenotify-start-polling-twice-replaces-timer ()
  "Calling start a second time must not leak the first timer."
  (nelisp-filenotify-test--fresh
    (nelisp-filenotify-start-polling 60.0)
    (let ((first (symbol-value 'nelisp-filenotify--poll-timer)))
      (nelisp-filenotify-start-polling 60.0)
      (let ((second (symbol-value 'nelisp-filenotify--poll-timer)))
        (should (timerp second))
        (should (not (eq first second)))
        (should-not (memq first timer-list))))
    (nelisp-filenotify-stop-polling)))

;;; Backend selection -------------------------------------------------

(ert-deftest nelisp-filenotify-default-backend-is-stat-poll ()
  (nelisp-filenotify-test--fresh
    (should (eq 'stat-poll nelisp-filenotify-backend))))

(ert-deftest nelisp-filenotify-set-backend-rejects-unknown ()
  (nelisp-filenotify-test--fresh
    (should-error (nelisp-filenotify-set-backend 'totally-bogus))))

(ert-deftest nelisp-filenotify-ffi-backends-signal-todo ()
  (nelisp-filenotify-test--fresh
    (let ((d (nelisp-filenotify-test--mkdir)))
      (nelisp-filenotify-set-backend 'inotify-ffi)
      (should-error
       (nelisp-filenotify-add-watch d '(create) #'ignore))
      (nelisp-filenotify-set-backend 'fsevents-ffi)
      (should-error
       (nelisp-filenotify-add-watch d '(create) #'ignore))
      (nelisp-filenotify-set-backend 'stat-poll))))

;;; Diff helper unit tests --------------------------------------------

(ert-deftest nelisp-filenotify-diff-snapshots-empty-empty ()
  (should (null (nelisp-filenotify--diff-snapshots nil nil))))

(ert-deftest nelisp-filenotify-diff-snapshots-create-and-delete ()
  (let* ((old '(("a.el" . 1.0) ("b.el" . 2.0)))
         (new '(("a.el" . 1.0) ("c.el" . 3.0)))
         (diff (nelisp-filenotify--diff-snapshots old new)))
    (should (= 2 (length diff)))
    (should (cl-find-if (lambda (e)
                          (and (eq 'delete (plist-get e :kind))
                               (equal "b.el" (plist-get e :name))))
                        diff))
    (should (cl-find-if (lambda (e)
                          (and (eq 'create (plist-get e :kind))
                               (equal "c.el" (plist-get e :name))))
                        diff))))

(ert-deftest nelisp-filenotify-diff-snapshots-modify ()
  (let* ((old '(("file.el" . 1.0)))
         (new '(("file.el" . 2.0)))
         (diff (nelisp-filenotify--diff-snapshots old new)))
    (should (= 1 (length diff)))
    (should (eq 'modify (plist-get (car diff) :kind)))
    (should (equal "file.el" (plist-get (car diff) :name)))))

(provide 'nelisp-filenotify-test)
;;; nelisp-filenotify-test.el ends here
