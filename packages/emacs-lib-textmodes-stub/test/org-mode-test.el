;;; org-mode-test.el --- org-mode heading navigation checks  -*- lexical-binding: t; -*-

;;; Commentary:

;; Regression tests for org-mode heading navigation in the GUI bridge:
;; org-next-visible-heading / org-previous-visible-heading /
;; org-forward-heading-same-level / org-back-to-heading / org-at-heading-p,
;; built on the existing org helpers (files--org-heading-level-at etc.).
;; Same two-layer pattern as the other bridge suites: host source-shape +
;; an opt-in standalone gate that drives the functions on a built image.

;;; Code:

(require 'ert)

(defconst org-mode-test--repo-root
  (expand-file-name
   ".." (file-name-directory (or load-file-name buffer-file-name))))

(defun org-mode-test--path (rel)
  (expand-file-name rel org-mode-test--repo-root))

(defconst org-mode-test--bridge-source
  (org-mode-test--path "src/nemacs-gui-file-bridge-runtime.el"))

(defun org-mode-test--slurp (file)
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

;;; --- host source-shape (always runs) -------------------------------------

(ert-deftest org-mode-test/source-shape ()
  "The bridge defines the org heading navigation commands."
  (should (file-readable-p org-mode-test--bridge-source))
  (let ((source (org-mode-test--slurp org-mode-test--bridge-source)))
    (dolist (needle '("(fset 'org-at-heading-p"
                      "(fset 'org-next-visible-heading"
                      "(fset 'org-previous-visible-heading"
                      "(fset 'org-back-to-heading"
                      "(fset 'org-forward-heading-same-level"
                      "(fset 'files--org-scan-heading-forward"
                      "(fset 'files--org-scan-heading-backward"
                      "(fset 'org-insert-heading"
                      "(fset 'org-meta-return"
                      "(fset 'org-demote"
                      "(fset 'org-promote"
                      "(fset 'org-move-subtree-down"
                      "(fset 'org-move-subtree-up"
                      "(fset 'org-priority"
                      "(fset 'org-schedule"
                      "(fset 'org-deadline"
                      "(fset 'org-toggle-checkbox"
                      "(fset 'org-set-tags"
                      "(fset 'org-toggle-tag"
                      "(fset 'org-refile-to-title"
                      "(fset 'files--org-relevel-subtree"
                      "(fset 'org-refile-to-file"))
      (should (string-match-p (regexp-quote needle) source)))))

;;; --- standalone gate (opt-in) --------------------------------------------

(defun org-mode-test--reader ()
  (catch 'found
    (dolist (candidate
             (list (getenv "NEMACS_GUI_BRIDGE_NELISP")
                   (getenv "NELISP")
                   (org-mode-test--path "../nelisp/target/nelisp")
                   "/tmp/nelisp-snap/nelisp"))
      (when candidate
        (let ((abs (expand-file-name candidate)))
          (when (file-executable-p abs) (throw 'found abs)))))
    nil))

(defmacro org-mode-test--skip-unless-standalone (&rest body)
  (declare (indent 0) (debug t))
  `(cond
    ((not (getenv "NEMACS_RUN_ORG"))
     (ert-skip "set NEMACS_RUN_ORG=1 to run standalone org checks"))
    ((not (org-mode-test--reader))
     (ert-skip "no standalone reader; set NEMACS_GUI_BRIDGE_NELISP or NELISP"))
    (t ,@body)))

(defconst org-mode-test--vendor-core
  (mapcar #'org-mode-test--path
          '("src/json.el"
            "../nelisp/lisp/nelisp-stdlib-regexp.el"
            "src/nemacs-runtime-stdlib-extra.el"
            "src/emacs-network-syscall-shim.el"
            "src/emacs-network-ffi.el"
            "src/emacs-process.el"
            "src/emacs-process-events.el"
            "src/emacs-eventloop.el"
            "src/nemacs-runtime-cdb.el"
            "src/nemacs-runtime-skk.el")))

(defun org-mode-test--build-image ()
  (let ((image (make-temp-file "org-image-" nil ".nlri"))
        (coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8)
        (prelude (org-mode-test--path
                  "../nelisp/scripts/nelisp-stdlib-prelude.el")))
    (with-temp-file image
      (insert ";;; nelisp-runtime-image source-v1\n(progn\n")
      (when (file-readable-p prelude)
        (insert-file-contents prelude) (goto-char (point-max)))
      (dolist (f org-mode-test--vendor-core)
        (when (file-readable-p f)
          (insert-file-contents f) (goto-char (point-max))))
      (insert-file-contents org-mode-test--bridge-source)
      (goto-char (point-max))
      (insert "\n)\n"))
    image))

(defun org-mode-test--run (reader image form)
  (let ((tdir (make-temp-file "org-transport-" t)))
    (unwind-protect
        (let ((wrapped (format "(progn (setq files--transport-dir %S) %s)"
                               tdir form)))
          (with-temp-buffer
            (let ((status (call-process reader nil (current-buffer) nil
                                        "exec-runtime-image" image wrapped)))
              (unless (equal 0 status)
                (ert-fail (format "exec-runtime-image failed: status=%S\n%s"
                                  status (buffer-string))))
              (buffer-string))))
      (when (file-directory-p tdir) (delete-directory tdir t)))))

(ert-deftest org-mode-test/standalone-heading-navigation ()
  "Heading navigation on a todo.org-like buffer:
* INBOX / body / ** t1 / ** t2 / * NEXT / ** t3 (heading bols 0 13 19 25 32)."
  (org-mode-test--skip-unless-standalone
    (let ((reader (org-mode-test--reader))
          (image (org-mode-test--build-image)))
      (unwind-protect
          (let ((out (org-mode-test--run
                      reader image
                      "(progn
  (setq files--buffer-string \"* INBOX\\nbody\\n** t1\\n** t2\\n* NEXT\\n** t3\")
  (setq files--point 0) (org-next-visible-heading)
  (princ (concat \"next0=\" (number-to-string (point)) \"\\n\"))
  (setq files--point 0) (org-forward-heading-same-level)
  (princ (concat \"same0=\" (number-to-string (point)) \"\\n\"))
  (setq files--point 32) (org-previous-visible-heading)
  (princ (concat \"prev32=\" (number-to-string (point)) \"\\n\"))
  (setq files--point 10) (org-back-to-heading)
  (princ (concat \"back10=\" (number-to-string (point)) \"\\n\"))
  (setq files--point 13)
  (princ (concat \"ah=\" (if (org-at-heading-p) \"y\" \"n\")))
  (setq files--point 10)
  (princ (concat (if (org-at-heading-p) \"y\" \"n\") \"\\n\"))
  (setq files--point 32) (org-next-visible-heading)
  (princ (concat \"end=\" (number-to-string (point))
                 \"/\" (number-to-string (length files--buffer-string)) \"\\n\")))")))
            (should (string-match-p "next0=13" out))
            (should (string-match-p "same0=25" out))
            (should (string-match-p "prev32=25" out))
            (should (string-match-p "back10=0" out))
            (should (string-match-p "ah=yn" out))
            (should (string-match-p "end=37/37" out)))
        (delete-file image)))))

(ert-deftest org-mode-test/standalone-structure-editing ()
  "org-insert-heading / org-demote / org-promote restructure headings."
  (org-mode-test--skip-unless-standalone
    (let ((reader (org-mode-test--reader))
          (image (org-mode-test--build-image)))
      (unwind-protect
          (let ((out (org-mode-test--run
                      reader image
                      "(progn
  (setq files--buffer-string \"* INBOX\") (setq files--point 3)
  (org-insert-heading) (insert \"task A\")
  (princ (concat \"ins=\" files--buffer-string \"\\n\"))
  (setq files--buffer-string \"* heading\") (setq files--point 5)
  (org-demote)
  (princ (concat \"dem=\" files--buffer-string \"|\" (number-to-string (point)) \"\\n\"))
  (setq files--buffer-string \"** sub\") (setq files--point 5)
  (org-promote)
  (princ (concat \"pro=\" files--buffer-string \"|\" (number-to-string (point)) \"\\n\"))
  (setq files--buffer-string \"* top\") (setq files--point 3)
  (org-promote)
  (princ (concat \"top=\" files--buffer-string \"\\n\")))")))
            (should (string-match-p "ins=\\* INBOX\n\\* task A" out))
            (should (string-match-p "dem=\\*\\* heading|6" out))
            (should (string-match-p "pro=\\* sub|4" out))
            (should (string-match-p "top=\\* top" out)))
        (delete-file image)))))

(ert-deftest org-mode-test/standalone-subtree-move ()
  "org-move-subtree-down / -up reorder whole subtrees (with children, and the
trailing-no-newline last-subtree edge)."
  (org-mode-test--skip-unless-standalone
    (let ((reader (org-mode-test--reader))
          (image (org-mode-test--build-image)))
      (unwind-protect
          (let ((out (org-mode-test--run
                      reader image
                      "(progn
  (setq files--buffer-string \"* A\\nbody A\\n* B\\nbody B\") (setq files--point 0)
  (org-move-subtree-down)
  (princ (concat \"d1=\" files--buffer-string \"|\" (number-to-string (point)) \"\\n\"))
  (setq files--buffer-string \"* A\\n** a1\\n* B\\n** b1\") (setq files--point 0)
  (org-move-subtree-down)
  (princ (concat \"dc=\" files--buffer-string \"\\n\"))
  (setq files--buffer-string \"* A\\n* B\") (setq files--point 0)
  (org-move-subtree-down)
  (princ (concat \"dt=\" files--buffer-string \"\\n\"))
  (setq files--buffer-string \"* A\\n* B\") (setq files--point 4)
  (org-move-subtree-up)
  (princ (concat \"up=\" files--buffer-string \"|\" (number-to-string (point)) \"\\n\")))")))
            (should (string-match-p "d1=\\* B\nbody B\n\\* A\nbody A|11" out))
            (should (string-match-p "dc=\\* B\n\\*\\* b1\n\\* A\n\\*\\* a1" out))
            (should (string-match-p "dt=\\* B\n\\* A" out))
            (should (string-match-p "up=\\* B\n\\* A|0" out)))
        (delete-file image)))))

(ert-deftest org-mode-test/standalone-priority ()
  "org-priority cycles none -> A -> B -> C -> none, after any TODO/DONE keyword."
  (org-mode-test--skip-unless-standalone
    (let ((reader (org-mode-test--reader))
          (image (org-mode-test--build-image)))
      (unwind-protect
          (let ((out (org-mode-test--run
                      reader image
                      "(progn
  (setq files--buffer-string \"* task\") (setq files--point 3)
  (org-priority) (princ (concat \"a=\" files--buffer-string \"\\n\"))
  (org-priority) (princ (concat \"b=\" files--buffer-string \"\\n\"))
  (org-priority) (princ (concat \"c=\" files--buffer-string \"\\n\"))
  (org-priority) (princ (concat \"n=\" files--buffer-string \"\\n\"))
  (setq files--buffer-string \"* TODO task\") (setq files--point 3)
  (org-priority) (princ (concat \"td=\" files--buffer-string \"\\n\"))
  (setq files--buffer-string \"* DONE finish report\") (setq files--point 3)
  (org-priority) (princ (concat \"dn=\" files--buffer-string \"\\n\")))")))
            (should (string-match-p "a=\\* \\[#A\\] task" out))
            (should (string-match-p "b=\\* \\[#B\\] task" out))
            (should (string-match-p "c=\\* \\[#C\\] task" out))
            (should (string-match-p "n=\\* task" out))
            (should (string-match-p "td=\\* TODO \\[#A\\] task" out))
            (should (string-match-p "dn=\\* DONE \\[#A\\] finish report" out)))
        (delete-file image)))))

(ert-deftest org-mode-test/standalone-schedule-deadline ()
  "org-schedule / org-deadline add a planning line after the heading; a second
org-schedule replaces (does not duplicate); both coexist; the body is kept."
  (org-mode-test--skip-unless-standalone
    (let ((reader (org-mode-test--reader))
          (image (org-mode-test--build-image)))
      (unwind-protect
          (let ((out (org-mode-test--run
                      reader image
                      "(progn
  (setq files--buffer-string \"* TODO task\\nbody line\") (setq files--point 3)
  (org-schedule)
  (princ (concat \"after-sched-line2=\"
                 (let ((s files--buffer-string) (i 12) (e 0))
                   (setq e i) (while (if (< e (length s)) (if (= (aref s e) 10) nil t) nil) (setq e (+ e 1)))
                   (substring s 12 e)) \"\\n\"))
  (org-schedule)
  (let ((i 0) (c 0) (s files--buffer-string))
    (while (< i (- (length s) 9))
      (if (equal (substring s i (+ i 10)) \"SCHEDULED:\") (setq c (+ c 1)) nil)
      (setq i (+ i 1)))
    (princ (concat \"sched-count=\" (number-to-string c) \"\\n\")))
  (setq files--point 3) (org-deadline)
  (princ (concat \"has-dl=\" (if (nlre-string-match \"DEADLINE: <\" files--buffer-string) \"y\" \"n\")
                 \" has-sc=\" (if (nlre-string-match \"SCHEDULED: <\" files--buffer-string) \"y\" \"n\")
                 \" has-body=\" (if (nlre-string-match \"body line\" files--buffer-string) \"y\" \"n\") \"\\n\")))")))
            (should (string-match-p "after-sched-line2=SCHEDULED: <" out))
            (should (string-match-p "sched-count=1" out))
            (should (string-match-p "has-dl=y has-sc=y has-body=y" out)))
        (delete-file image)))))

(ert-deftest org-mode-test/standalone-checkbox ()
  "org-toggle-checkbox flips [ ] <-> [X] on the current line; a [#A] priority
cookie is not a checkbox."
  (org-mode-test--skip-unless-standalone
    (let ((reader (org-mode-test--reader))
          (image (org-mode-test--build-image)))
      (unwind-protect
          (let ((out (org-mode-test--run
                      reader image
                      "(progn
  (setq files--buffer-string \"- [ ] buy milk\") (setq files--point 5)
  (org-toggle-checkbox) (princ (concat \"on=\" files--buffer-string \"\\n\"))
  (org-toggle-checkbox) (princ (concat \"off=\" files--buffer-string \"\\n\"))
  (setq files--buffer-string \"- [x] lower\") (setq files--point 5)
  (org-toggle-checkbox) (princ (concat \"low=\" files--buffer-string \"\\n\"))
  (setq files--buffer-string \"* [#A] heading\") (setq files--point 5)
  (setq files--bridge-status \"\") (org-toggle-checkbox)
  (princ (concat \"cookie=\" files--buffer-string \"|\" files--bridge-status \"\\n\"))
  (setq files--buffer-string \"* TODO project\\n- [ ] step 1\\n- [ ] step 2\") (setq files--point 18)
  (org-toggle-checkbox) (princ (concat \"sub=\" files--buffer-string \"\\n\")))")))
            (should (string-match-p "on=- \\[X\\] buy milk" out))
            (should (string-match-p "off=- \\[ \\] buy milk" out))
            (should (string-match-p "low=- \\[ \\] lower" out))
            (should (string-match-p "cookie=\\* \\[#A\\] heading|unsupported" out))
            (should (string-match-p "sub=\\* TODO project\n- \\[X\\] step 1\n- \\[ \\] step 2" out)))
        (delete-file image)))))

(ert-deftest org-mode-test/standalone-tags ()
  "org-set-tags sets/replaces :tag: groups; org-toggle-tag adds/removes one tag
\(including from the middle of a group) without leaving stray separators."
  (org-mode-test--skip-unless-standalone
    (let ((reader (org-mode-test--reader))
          (image (org-mode-test--build-image)))
      (unwind-protect
          (let ((out (org-mode-test--run
                      reader image
                      "(progn
  (setq files--buffer-string \"* task\") (setq files--point 3)
  (org-set-tags \"work:urgent\") (princ (concat \"set=\" files--buffer-string \"\\n\"))
  (org-set-tags \"\") (princ (concat \"clr=\" files--buffer-string \"\\n\"))
  (setq files--buffer-string \"* TODO [#A] important\") (setq files--point 3)
  (org-set-tags \"home\") (princ (concat \"meta=\" files--buffer-string \"\\n\"))
  (setq files--buffer-string \"* task\") (setq files--point 3)
  (org-toggle-tag \"urgent\") (princ (concat \"ton=\" files--buffer-string \"\\n\"))
  (org-toggle-tag \"urgent\") (princ (concat \"tof=\" files--buffer-string \"\\n\"))
  (setq files--buffer-string \"* task :a:urgent:b:\") (setq files--point 3)
  (org-toggle-tag \"urgent\") (princ (concat \"tmid=\" files--buffer-string \"\\n\")))")))
            (should (string-match-p "set=\\* task :work:urgent:" out))
            (should (string-match-p "clr=\\* task" out))
            (should (string-match-p "meta=\\* TODO \\[#A\\] important :home:" out))
            (should (string-match-p "ton=\\* task :urgent:" out))
            (should (string-match-p "tof=\\* task" out))
            (should (string-match-p "tmid=\\* task :a:b:" out)))
        (delete-file image)))))

(ert-deftest org-mode-test/standalone-refile ()
  "org-refile-to-title moves the current subtree (with children, re-leveled)
under a target heading; an unknown target leaves the buffer unchanged."
  (org-mode-test--skip-unless-standalone
    (let ((reader (org-mode-test--reader))
          (image (org-mode-test--build-image)))
      (unwind-protect
          (let ((out (org-mode-test--run
                      reader image
                      "(progn
  (setq files--buffer-string \"* INBOX\\n* item\\n* PROJECTS\") (setq files--point 8)
  (org-refile-to-title \"PROJECTS\")
  (princ (concat \"r1=\" files--buffer-string \"\\nEND\\n\"))
  (setq files--buffer-string \"* INBOX\\n* item\\n** sub\\n* PROJECTS\") (setq files--point 8)
  (org-refile-to-title \"PROJECTS\")
  (princ (concat \"r2=\" files--buffer-string \"\\nEND\\n\"))
  (setq files--buffer-string \"* INBOX\\n* item\") (setq files--point 8)
  (setq files--bridge-status \"\") (org-refile-to-title \"NOPE\")
  (princ (concat \"r4=\" files--buffer-string \"|\" files--bridge-status \"\\n\")))")))
            (should (string-match-p "r1=\\* INBOX\n\\* PROJECTS\n\\*\\* item\nEND" out))
            (should (string-match-p
                     "r2=\\* INBOX\n\\* PROJECTS\n\\*\\* item\n\\*\\*\\* sub\nEND" out))
            (should (string-match-p "r4=\\* INBOX\n\\* item|unsupported" out)))
        (delete-file image)))))

(ert-deftest org-mode-test/standalone-refile-cross-file ()
  "org-refile-to-file moves the current subtree (re-leveled) under a target
heading in ANOTHER file, writes that file, and removes it from the buffer."
  (org-mode-test--skip-unless-standalone
    (let ((reader (org-mode-test--reader))
          (image (org-mode-test--build-image))
          (tdir (make-temp-file "org-refile-" t)))
      (unwind-protect
          (let ((tgt (expand-file-name "projects.org" tdir)))
            (with-temp-file tgt (insert "* PROJECTS\nstuff"))
            (let ((out (org-mode-test--run
                        reader image
                        (format "(progn
  (setq files--buffer-string \"* INBOX\\n* item\\n** sub\\n* other\") (setq files--point 8)
  (org-refile-to-file %S \"PROJECTS\")
  (princ (concat \"buf=\" files--buffer-string \"\\nEND\\n\")))" tgt))))
              (should (string-match-p "buf=\\* INBOX\n\\* other\nEND" out))
              ;; the target file on disk got the re-leveled subtree appended
              (let ((disk (with-temp-buffer (insert-file-contents tgt) (buffer-string))))
                (should (string-match-p
                         "\\* PROJECTS\nstuff\n\\*\\* item\n\\*\\*\\* sub" disk)))))
        (delete-file image)
        (when (file-directory-p tdir) (delete-directory tdir t))))))

(ert-deftest org-mode-test/source-shape-keybindings ()
  "org-mode buffers bind the new commands to standard C-c keys."
  ;; the source holds the literal backslash-t escape, not a tab character.
  (let ((source (org-mode-test--slurp org-mode-test--bridge-source)))
    (dolist (needle '("C-c C-n\\torg-next-visible-heading"
                      "C-c C-t\\torg-todo"
                      "C-c C-s\\torg-schedule"
                      "C-c C-c\\torg-toggle-checkbox"
                      "C-c ,\\torg-priority"
                      "M-RET\\torg-meta-return"))
      (should (string-match-p (regexp-quote needle) source)))))

(ert-deftest org-mode-test/standalone-keybinding-dispatch ()
  "In a .org buffer, the org C-c keys dispatch to the commands (via the
command-execute fboundp fallback); a non-org buffer gets no org bindings."
  (org-mode-test--skip-unless-standalone
    (let ((reader (org-mode-test--reader))
          (image (org-mode-test--build-image)))
      (unwind-protect
          (let ((out (org-mode-test--run
                      reader image
                      "(progn
  (setq files--current-file-name \"todo.org\") (setq files--input-method \"\")
  (setq files--buffer-string \"* INBOX\\nbody\\n* NEXT\") (setq files--point 0)
  (setq files--bridge-keys \"C-c C-n\") (setq files--bridge-arg \"\") (files--dispatch-key-sequence)
  (princ (concat \"n=\" (number-to-string files--point) \"\\n\"))
  (setq files--buffer-string \"* INBOX\") (setq files--point 0)
  (setq files--bridge-keys \"C-c C-t\") (setq files--bridge-arg \"\") (files--dispatch-key-sequence)
  (princ (concat \"t=\" files--buffer-string \"\\n\"))
  (setq files--buffer-string \"* task\") (setq files--point 3)
  (setq files--bridge-keys \"C-c ,\") (setq files--bridge-arg \"\") (files--dispatch-key-sequence)
  (princ (concat \"p=\" files--buffer-string \"\\n\"))
  (setq files--current-file-name \"foo.txt\")
  (princ (concat \"txt=\" (if (files--org-buffer-p) \"y\" \"n\") \"\\n\")))")))
            (should (string-match-p "n=13" out))
            (should (string-match-p "t=\\* TODO INBOX" out))
            (should (string-match-p "p=\\* \\[#A\\] task" out))
            (should (string-match-p "txt=n" out)))
        (delete-file image)))))

(provide 'org-mode-test)

;;; org-mode-test.el ends here
