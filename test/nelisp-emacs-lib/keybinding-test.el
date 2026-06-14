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
                      "(fset 'files--char-len-at"
                      "(fset 'files--char-len-before"
                      "(rdf (files--user-keymap-path))"))
      (should (string-match-p (regexp-quote needle) source)))
    ;; describe-key / where-is must include the user overlay in their source
    (should (string-match-p (regexp-quote "so describe-key") source))
    (should (string-match-p (regexp-quote "so where-is reports") source))
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

(ert-deftest keybinding-test/standalone-discoverability ()
  "describe-key / where-is reflect user bindings (the global-set-key overlay)."
  (keybinding-test--skip-unless-standalone
    (let ((reader (keybinding-test--reader))
          (image (keybinding-test--build-image)))
      (unwind-protect
          (let ((out (keybinding-test--run
                      reader image
                      "(progn
  (setq files--buffer-string \"abc\") (setq files--point 0)
  (nl-write-file (files--user-keymap-path) \"\")
  (defun kb-disc-cmd () nil)
  (global-set-key \"C-t\" 'kb-disc-cmd)
  (setq files--bridge-arg \"C-t\") (describe-key)
  (princ (concat \"dk=\" files--buffer-string \"\\n\"))
  (setq files--bridge-arg \"kb-disc-cmd\") (where-is)
  (princ (concat \"wi=\" files--buffer-string \"\\n\"))
  (describe-bindings)
  (princ (concat \"db=\" (if (nlre-string-match \"C-t.*kb-disc-cmd\" files--buffer-string) \"has-binding\" \"missing\") \"\\n\")))")))
            (should (string-match-p "dk=C-t runs the command kb-disc-cmd" out))
            (should (string-match-p "wi=kb-disc-cmd is on C-t" out))
            (should (string-match-p "db=has-binding" out)))
        (delete-file image)))))

(ert-deftest keybinding-test/standalone-custom-command-edits ()
  "A custom command bound to a key does real editing via the query/insert
primitives (insert at point, line-beginning-position) -- the payoff of
keybinding customization + editing primitives together."
  (keybinding-test--skip-unless-standalone
    (let ((reader (keybinding-test--reader))
          (image (keybinding-test--build-image)))
      (unwind-protect
          (let ((out (keybinding-test--run
                      reader image
                      "(progn
  (nl-write-file (files--user-keymap-path) \"\")
  ;; a command that prefixes the current line with \"> \"
  (defun kb-quote-line ()
    (goto-char (line-beginning-position))
    (insert \"> \"))
  (global-set-key (kbd \"C-c q\") 'kb-quote-line)
  (setq files--buffer-string \"hello\\nworld\") (setq files--point 8)
  (setq files--bridge-keys \"C-c q\") (setq files--bridge-arg \"\")
  (files--dispatch-key-sequence)
  ;; point was on the 2nd line (world); it should become \"> world\"
  (princ (concat \"edited=\" files--buffer-string \"\\n\"))
  ;; query primitives reflect the edit
  (princ (concat \"pmax=\" (number-to-string (point-max)) \"\\n\")))")))
            (should (string-match-p "edited=hello\n> world" out))
            (should (string-match-p "pmax=13" out)))
        (delete-file image)))))

(ert-deftest keybinding-test/standalone-structural-editing ()
  "save-excursion / looking-at / re-search-forward / delete-region (function
form) compose into a non-trivial custom command."
  (keybinding-test--skip-unless-standalone
    (let ((reader (keybinding-test--reader))
          (image (keybinding-test--build-image)))
      (unwind-protect
          (let ((out (keybinding-test--run
                      reader image
                      "(progn
  ;; delete-region function form
  (setq files--buffer-string \"hello world\") (setq files--point 11)
  (delete-region 5 11)
  (princ (concat \"del=\" files--buffer-string \"\\n\"))
  ;; save-excursion restores point across an edit
  (setq files--buffer-string \"abcdef\") (setq files--point 1)
  (save-excursion (goto-char 4) (insert \"X\"))
  (princ (concat \"save=\" files--buffer-string \"|\" (number-to-string (point)) \"\\n\"))
  ;; looking-at / re-search-forward
  (setq files--buffer-string \"foo123bar\") (setq files--point 0)
  (princ (concat \"la=\" (if (looking-at \"foo\") \"y\" \"n\")
                 (if (looking-at \"bar\") \"y\" \"n\") \"\\n\"))
  (setq files--point 0) (re-search-forward \"[0-9]+\")
  (princ (concat \"rsf=\" (number-to-string (point)) \"\\n\"))
  ;; realistic command composing them
  (setq files--buffer-string \"a1b2c3\") (setq files--point 0)
  (defun strip-first-digit ()
    (save-excursion (goto-char 0)
      (if (re-search-forward \"[0-9]\") (delete-region (- (point) 1) (point)) nil)))
  (strip-first-digit)
  (princ (concat \"strip=\" files--buffer-string \"\\n\")))")))
            (should (string-match-p "del=hello\n" out))
            (should (string-match-p "save=abcdXef|1" out))
            (should (string-match-p "la=yn" out))
            (should (string-match-p "rsf=6" out))
            (should (string-match-p "strip=ab2c3" out)))
        (delete-file image)))))

(ert-deftest keybinding-test/standalone-search-replace ()
  "re-search-forward + replace-match compose into search-and-replace commands,
including loops with length-changing replacements."
  (keybinding-test--skip-unless-standalone
    (let ((reader (keybinding-test--reader))
          (image (keybinding-test--build-image)))
      (unwind-protect
          (let ((out (keybinding-test--run
                      reader image
                      "(progn
  ;; single replace
  (setq files--buffer-string \"say foo now\") (setq files--point 0)
  (re-search-forward \"foo\") (replace-match \"bar\")
  (princ (concat \"one=\" files--buffer-string \"\\n\"))
  ;; replace-all loop
  (setq files--buffer-string \"a foo b foo c foo\") (setq files--point 0)
  (defun rep-all () (goto-char 0)
    (while (re-search-forward \"foo\") (replace-match \"X\")))
  (rep-all)
  (princ (concat \"all=\" files--buffer-string \"\\n\"))
  ;; length-changing replacement in a loop (offsets must advance correctly)
  (setq files--buffer-string \"1 2 3\") (setq files--point 0)
  (defun pad () (goto-char 0)
    (while (re-search-forward \"[0-9]\") (replace-match \"[N]\")))
  (pad)
  (princ (concat \"pad=\" files--buffer-string \"\\n\")))")))
            (should (string-match-p "one=say bar now" out))
            (should (string-match-p "all=a X b X c X" out))
            (should (string-match-p "pad=\\[N\\] \\[N\\] \\[N\\]" out)))
        (delete-file image)))))

(ert-deftest keybinding-test/standalone-kill-yank ()
  "kill-new / current-kill / yank and the function forms of kill-region /
copy-region-as-kill give custom commands full copy-paste."
  (keybinding-test--skip-unless-standalone
    (let ((reader (keybinding-test--reader))
          (image (keybinding-test--build-image)))
      (unwind-protect
          (let ((out (keybinding-test--run
                      reader image
                      "(progn
  (nl-write-file (progn (setq files--transport-name \"nemacs-last-command\")
                        (files--transport-path)) \"\")
  ;; kill-new + current-kill + yank
  (kill-new \"clip\")
  (setq files--buffer-string \"abXcd\") (setq files--point 2) (yank)
  (princ (concat \"yank=\" files--buffer-string \"\\n\"))
  ;; kill-region function form (kill + delete)
  (setq files--buffer-string \"hello world\") (setq files--point 11)
  (kill-region 0 5)
  (princ (concat \"killfn=\" files--buffer-string \"|\" (current-kill 0) \"\\n\"))
  ;; copy-region-as-kill function form (copy, no delete)
  (setq files--buffer-string \"abcdef\") (setq files--point 0)
  (copy-region-as-kill 1 4)
  (princ (concat \"copyfn=\" files--buffer-string \"|\" (current-kill 0) \"\\n\"))
  ;; kill-region INTERACTIVE (no args) still works on point/mark
  (setq files--buffer-string \"hello world\") (setq files--point 2) (setq files--mark 5)
  (kill-region)
  (princ (concat \"killint=\" files--buffer-string \"\\n\"))
  ;; realistic command: move text via kill + yank
  (setq files--buffer-string \"AB12\") (setq files--point 0)
  (defun move-digits () (kill-region 2 4) (goto-char 0) (yank))
  (move-digits)
  (princ (concat \"move=\" files--buffer-string \"\\n\")))")))
            (should (string-match-p "yank=abclipXcd" out))
            (should (string-match-p "killfn= world|hello" out))
            (should (string-match-p "copyfn=abcdef|bcd" out))
            (should (string-match-p "killint=he world" out))
            (should (string-match-p "move=12AB" out)))
        (delete-file image)))))

(ert-deftest keybinding-test/standalone-registers-motion ()
  "set-register / get-register, line predicates (bolp/eolp/bobp/eobp), and
forward-line for custom commands."
  (keybinding-test--skip-unless-standalone
    (let* ((reader (keybinding-test--reader))
           (image (keybinding-test--build-image))
           (tdir (make-temp-file "keybinding-reg-" t)))
      (unwind-protect
          (progn
            ;; the launcher creates the register store dir; mirror that here
            (make-directory (expand-file-name "nemacs-register-store" tdir) t)
            (let ((out (with-temp-buffer
                         (let ((status
                                (call-process
                                 reader nil (current-buffer) nil
                                 "exec-runtime-image" image
                                 (format "(progn (setq files--transport-dir %S)
  (set-register 97 \"saved\") (set-register 98 42)
  (princ (concat \"rega=\" (get-register 97)
                 \" regb=\" (number-to-string (get-register 98)) \"\\n\"))
  (setq files--buffer-string \"ab\\ncd\") (setq files--point 0)
  (princ (concat \"p0=\" (if (bolp) \"B\" \"-\") (if (bobp) \"b\" \"-\") \"\\n\"))
  (setq files--point 2)
  (princ (concat \"p2=\" (if (eolp) \"E\" \"-\") \"\\n\"))
  (setq files--buffer-string \"L0\\nL1\\nL2\\nL3\") (setq files--point 0)
  (forward-line 2)
  (princ (concat \"fl2=\" (number-to-string (point))
                 \" short=\" (number-to-string (forward-line 9)) \"\\n\")))" tdir))))
                           (unless (equal 0 status)
                             (ert-fail (format "status=%S\n%s"
                                               status (buffer-string))))
                           (buffer-string)))))
              (should (string-match-p "rega=saved regb=42" out))
              (should (string-match-p "p0=Bb" out))
              (should (string-match-p "p2=E" out))
              (should (string-match-p "fl2=6 short=" out))))
        (delete-file image)
        (when (file-directory-p tdir) (delete-directory tdir t))))))

(ert-deftest keybinding-test/standalone-word-motion ()
  "forward-word / backward-word honour the count arg; word-at-point /
thing-at-point extract the thing at point."
  (keybinding-test--skip-unless-standalone
    (let ((reader (keybinding-test--reader))
          (image (keybinding-test--build-image)))
      (unwind-protect
          (let ((out (keybinding-test--run
                      reader image
                      "(progn
  (setq files--buffer-string \"foo bar baz\") (setq files--point 0)
  (forward-word 3) (princ (concat \"fw3=\" (number-to-string (point)) \"\\n\"))
  (setq files--point 11) (backward-word 2)
  (princ (concat \"bw2=\" (number-to-string (point)) \"\\n\"))
  (setq files--point 5)
  (princ (concat \"wap=\" (or (word-at-point) \"nil\") \"\\n\"))
  (setq files--buffer-string \"line one\\nline two\") (setq files--point 3)
  (princ (concat \"line=\" (or (thing-at-point 'line) \"nil\")
                 \" word=\" (or (thing-at-point 'word) \"nil\") \"\\n\")))")))
            (should (string-match-p "fw3=11" out))
            (should (string-match-p "bw2=4" out))
            (should (string-match-p "wap=bar" out))
            (should (string-match-p "line=line one word=line" out)))
        (delete-file image)))))

(ert-deftest keybinding-test/standalone-count-case ()
  "count-lines / count-words / line-number-at-pos and the function forms of
upcase-region / downcase-region for custom commands."
  (keybinding-test--skip-unless-standalone
    (let ((reader (keybinding-test--reader))
          (image (keybinding-test--build-image)))
      (unwind-protect
          (let ((out (keybinding-test--run
                      reader image
                      "(progn
  (setq files--buffer-string \"a\\nb\\nc\")
  (princ (concat \"cl=\" (number-to-string (count-lines 0 5)) \"\\n\"))
  (setq files--buffer-string \"foo bar baz qux\")
  (princ (concat \"cw=\" (number-to-string (count-words 0 15)) \"\\n\"))
  (setq files--buffer-string \"L0\\nL1\\nL2\") (setq files--point 6)
  (princ (concat \"ln=\" (number-to-string (line-number-at-pos)) \"\\n\"))
  (setq files--buffer-string \"abcdef\") (upcase-region 1 4)
  (princ (concat \"up=\" files--buffer-string \"\\n\"))
  (setq files--buffer-string \"ABCDEF\") (downcase-region 1 4)
  (princ (concat \"dn=\" files--buffer-string \"\\n\"))
  (setq files--buffer-string \"hello\") (setq files--point 0) (setq files--mark 5)
  (upcase-region)
  (princ (concat \"ui=\" files--buffer-string \"\\n\")))")))
            (should (string-match-p "cl=3" out))
            (should (string-match-p "cw=4" out))
            (should (string-match-p "ln=3" out)) ; point 6 = start of L2 (line 3)
            (should (string-match-p "up=aBCDef" out))
            (should (string-match-p "dn=AbcdEF" out))
            (should (string-match-p "ui=HELLO" out)))
        (delete-file image)))))

(ert-deftest keybinding-test/standalone-multibyte-char-motion ()
  "Char motion/deletion respect UTF-8 boundaries -- point is a byte offset but
the user's text is Japanese (3-byte chars), so forward/backward-char and
delete/backward-delete-char must step a whole character, never one byte (which
would corrupt the buffer mid-edit, e.g. backspacing a kanji).  Buffer
\"ab漏電cd\": a=0 b=1 漏=2..4 電=5..7 c=8 d=9 (len 10)."
  (keybinding-test--skip-unless-standalone
    (let ((reader (keybinding-test--reader))
          (image (keybinding-test--build-image))
          (coding-system-for-read 'utf-8)
          (coding-system-for-write 'utf-8)
          (default-process-coding-system '(utf-8-unix . utf-8-unix)))
      (unwind-protect
          (let ((out (keybinding-test--run
                      reader image
                      "(progn
  (setq files--buffer-string \"ab漏電cd\")
  (setq files--point 2) (forward-char)
  (princ (concat \"f1=\" (number-to-string files--point) \"\\n\"))   ; 5 (past 漏)
  (forward-char)
  (princ (concat \"f2=\" (number-to-string files--point) \"\\n\"))   ; 8 (past 電)
  (setq files--point 8) (backward-char)
  (princ (concat \"b1=\" (number-to-string files--point) \"\\n\"))   ; 5
  ;; backspace over 電 removes all 3 bytes -> ab漏cd, point 5
  (setq files--buffer-string \"ab漏電cd\") (setq files--point 8) (backward-delete-char)
  (princ (concat \"bd=[\" files--buffer-string \"]|\" (number-to-string files--point) \"\\n\"))
  ;; forward-delete of 漏 -> ab電cd, point 2
  (setq files--buffer-string \"ab漏電cd\") (setq files--point 2) (delete-char)
  (princ (concat \"fd=[\" files--buffer-string \"]|\" (number-to-string files--point) \"\\n\"))
  ;; ASCII regression: still one byte
  (setq files--buffer-string \"abc\") (setq files--point 3) (backward-delete-char)
  (princ (concat \"asc=[\" files--buffer-string \"]|\" (number-to-string files--point) \"\\n\")))")))
            (should (string-match-p "f1=5" out))
            (should (string-match-p "f2=8" out))
            (should (string-match-p "b1=5" out))
            (should (string-match-p "bd=\\[ab漏cd\\]|5" out))
            (should (string-match-p "fd=\\[ab電cd\\]|2" out))
            (should (string-match-p "asc=\\[ab\\]|2" out)))
        (delete-file image)))))

(provide 'keybinding-test)

;;; keybinding-test.el ends here
