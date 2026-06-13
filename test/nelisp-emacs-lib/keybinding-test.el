;;; keybinding-test.el --- user keybinding customization checks  -*- lexical-binding: t; -*-

;;; Commentary:

;; Regression tests for user keybinding customization in the GUI bridge:
;; global-set-key / define-key / local-set-key / kbd write a persistent
;; user-keymap overlay that the key lookup consults before the static keymap,
;; so a user binding overrides the default and can run a built-in OR a
;; user-defined command.  Two layers (same pattern as skk-okuri-conversion-test):
;;
;;   1. Host ERT pins the source shape (the fset definitions + the overlay
;;      prepended into the lookup).
;;   2. An opt-in standalone gate drives the real files--dispatch-key-sequence
;;      against a built image: bind a key, dispatch it, observe the effect.

;;; Code:

(require 'ert)

(defconst keybinding-test--repo-root
  (expand-file-name
   ".." (file-name-directory (or load-file-name buffer-file-name))))

(defun keybinding-test--path (rel)
  (expand-file-name rel keybinding-test--repo-root))

(defconst keybinding-test--bridge-source
  (keybinding-test--path "src/nemacs-gui-file-bridge-runtime.el"))

(defun keybinding-test--slurp (file)
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

;;; --- host source-shape (always runs) -------------------------------------

(ert-deftest keybinding-test/source-shape ()
  "The bridge defines the keybinding API and prepends the user-keymap overlay."
  (should (file-readable-p keybinding-test--bridge-source))
  (let ((source (keybinding-test--slurp keybinding-test--bridge-source)))
    (dolist (needle '("(fset 'global-set-key"
                      "(fset 'define-key"
                      "(fset 'local-set-key"
                      "(fset 'files--user-keymap-path"
                      "(fset 'files--user-keymap-remove"
                      "(rdf (files--user-keymap-path))"))
      (should (string-match-p (regexp-quote needle) source)))
    ;; the funcall fallback so a user defun bound to a key actually runs
    (should (string-match-p
             (regexp-quote "(fboundp files--bridge-command)") source))))

;;; --- standalone gate (opt-in) --------------------------------------------

(defun keybinding-test--reader ()
  (catch 'found
    (dolist (candidate
             (list (getenv "NEMACS_GUI_BRIDGE_NELISP")
                   (getenv "NELISP")
                   (keybinding-test--path "../nelisp/target/nelisp")
                   "/tmp/nelisp-snap/nelisp"))
      (when candidate
        (let ((abs (expand-file-name candidate)))
          (when (file-executable-p abs) (throw 'found abs)))))
    nil))

(defmacro keybinding-test--skip-unless-standalone (&rest body)
  (declare (indent 0) (debug t))
  `(cond
    ((not (getenv "NEMACS_RUN_KEYBINDING"))
     (ert-skip "set NEMACS_RUN_KEYBINDING=1 to run standalone keybinding checks"))
    ((not (keybinding-test--reader))
     (ert-skip "no standalone reader; set NEMACS_GUI_BRIDGE_NELISP or NELISP"))
    (t ,@body)))

(defconst keybinding-test--vendor-core
  (mapcar #'keybinding-test--path
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

(defun keybinding-test--build-image ()
  "Write a source-v1 image of prelude + vendor core + the bridge (UTF-8)."
  (let ((image (make-temp-file "keybinding-image-" nil ".nlri"))
        (coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8)
        (prelude (keybinding-test--path
                  "../nelisp/scripts/nelisp-stdlib-prelude.el")))
    (with-temp-file image
      (insert ";;; nelisp-runtime-image source-v1\n(progn\n")
      (when (file-readable-p prelude)
        (insert-file-contents prelude) (goto-char (point-max)))
      (dolist (f keybinding-test--vendor-core)
        (when (file-readable-p f)
          (insert-file-contents f) (goto-char (point-max))))
      (insert-file-contents keybinding-test--bridge-source)
      (goto-char (point-max))
      (insert "\n)\n"))
    image))

(defun keybinding-test--run (reader image form)
  "Run FORM in an isolated transport dir; return captured output."
  (let ((tdir (make-temp-file "keybinding-transport-" t)))
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

(ert-deftest keybinding-test/standalone-global-set-key ()
  "global-set-key binds built-in and user commands, overrides defaults."
  (keybinding-test--skip-unless-standalone
    (let ((reader (keybinding-test--reader))
          (image (keybinding-test--build-image)))
      (unwind-protect
          (let ((out (keybinding-test--run
                      reader image
                      "(progn
  (setq files--buffer-string \"abc\") (setq files--point 0)
  (nl-write-file (files--user-keymap-path) \"\")
  (setq files--bridge-keys \"C-f\") (setq files--bridge-arg \"\") (files--dispatch-key-sequence)
  (princ (concat \"default=\" (number-to-string files--point) \"\\n\"))
  (global-set-key (kbd \"C-t\") 'forward-char)
  (setq files--bridge-keys \"C-t\") (setq files--bridge-arg \"\") (files--dispatch-key-sequence)
  (princ (concat \"builtin=\" (number-to-string files--point) \"\\n\"))
  (defun kb-test-append () (setq files--buffer-string (concat files--buffer-string \"X\")))
  (global-set-key (kbd \"C-t\") 'kb-test-append)
  (setq files--bridge-keys \"C-t\") (setq files--bridge-arg \"\") (files--dispatch-key-sequence)
  (princ (concat \"userfn=\" files--buffer-string \"\\n\"))
  (global-set-key \"C-f\" 'kb-test-append)
  (setq files--bridge-keys \"C-f\") (setq files--bridge-arg \"\") (files--dispatch-key-sequence)
  (princ (concat \"override=\" files--buffer-string \"\\n\"))
  (global-set-key (kbd \"C-c a\") 'kb-test-append)
  (setq files--bridge-keys \"C-c a\") (setq files--bridge-arg \"\") (files--dispatch-key-sequence)
  (princ (concat \"multikey=\" files--buffer-string \"\\n\")))")))
            (should (string-match-p "default=1" out))
            (should (string-match-p "builtin=2" out))
            (should (string-match-p "userfn=abcX" out))
            (should (string-match-p "override=abcXX" out))
            (should (string-match-p "multikey=abcXXX" out)))
        (delete-file image)))))

(ert-deftest keybinding-test/standalone-config-binding ()
  "A user init's global-set-key takes effect on a dispatched key.
Seeds a wrapped user init (defun + global-set-key) into the transport dir, runs
the real files--load-user-init lane, then dispatches the bound key -- proving
keybindings from the user's own config work, not just programmatic calls."
  (keybinding-test--skip-unless-standalone
    (let* ((reader (keybinding-test--reader))
           (image (keybinding-test--build-image))
           (tdir (make-temp-file "keybinding-config-" t)))
      (unwind-protect
          (progn
            ;; the wrapped init the launcher would generate (marker-bracketed)
            (with-temp-file (expand-file-name "nemacs-init-wrapped" tdir)
              (insert "(nemacs-init--begin 1 \"defun\")\n")
              (insert "(defun kb-cfg-cmd () (setq files--buffer-string"
                      " (concat files--buffer-string \"Z\")))\n")
              (insert "(nemacs-init--ok 1)\n")
              (insert "(nemacs-init--begin 2 \"global-set-key\")\n")
              (insert "(global-set-key \"C-t\" (quote kb-cfg-cmd))\n")
              (insert "(nemacs-init--ok 2)\n"))
            (let ((out (with-temp-buffer
                         (let ((status
                                (call-process
                                 reader nil (current-buffer) nil
                                 "exec-runtime-image" image
                                 (format "(progn (setq files--transport-dir %S)
  (setq files--buffer-string \"abc\") (setq files--point 0)
  (nl-write-file (files--user-keymap-path) \"\")
  (files--load-user-init)
  (setq files--bridge-keys \"C-t\") (setq files--bridge-arg \"\")
  (files--dispatch-key-sequence)
  (princ (concat \"config=\" files--buffer-string \"\\n\")))" tdir))))
                           (unless (equal 0 status)
                             (ert-fail (format "status=%S\n%s"
                                               status (buffer-string))))
                           (buffer-string)))))
              (should (string-match-p "config=abcZ" out))))
        (delete-file image)
        (when (file-directory-p tdir) (delete-directory tdir t))))))

(provide 'keybinding-test)

;;; keybinding-test.el ends here
