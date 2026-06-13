;;; skk-okuri-conversion-test.el --- SKK kana-kanji conversion checks  -*- lexical-binding: t; -*-

;;; Commentary:

;; Regression tests for the local SKK conversion baked into the GUI bridge
;; runtime image: nouns (okuri-nasi: みらい -> 未来) and conjugated verbs /
;; adjectives (okuri-ari: はしる -> 走る, たかい -> 高い).  Two layers:
;;
;;   1. Host ERT pins the source shape of nemacs-runtime-skk.el (the okuri
;;      API and the gojuon column table) -- always runs.
;;
;;   2. An opt-in standalone gate (NEMACS_RUN_SKK_CONVERSION=1 + a built NeLisp
;;      reader + python3) builds the vendor-core runtime image and an ISOLATED
;;      fixture CDB (its own temp dict + temp .cdb -- never the host's
;;      /usr/share/skk dictionary or /tmp/skk.cdb), then asserts the actual
;;      conversions on the standalone runtime: the engine (skk-convert-okuri*)
;;      and the editor keystroke loop (files--ime-convert buffer transform).
;;
;; The standalone layer is what makes these byte-string-model behaviours real
;; (substring is byte-indexed; a hiragana is 3 bytes) -- they cannot be
;; exercised under host Emacs, hence the subprocess gate.

;;; Code:

(require 'ert)

(defconst skk-okuri-conversion-test--repo-root
  (expand-file-name
   ".."
   (file-name-directory (or load-file-name buffer-file-name))))

(defun skk-okuri-conversion-test--path (rel)
  "Absolute path for REL under the repo root."
  (expand-file-name rel skk-okuri-conversion-test--repo-root))

(defconst skk-okuri-conversion-test--skk-source
  (skk-okuri-conversion-test--path "src/nemacs-runtime-skk.el"))

(defconst skk-okuri-conversion-test--bridge-source
  (skk-okuri-conversion-test--path "src/nemacs-gui-file-bridge-runtime.el"))

(defconst skk-okuri-conversion-test--cdb-builder
  (skk-okuri-conversion-test--path "scripts/skk-jisyo-to-cdb.py"))

;; Vendor-core files baked before the bridge, in the launcher's dependency
;; order (nemacs-mx.sh NEMACS_STDLIB_PRELUDE + NEMACS_VENDOR_CORE).
(defconst skk-okuri-conversion-test--prelude
  (skk-okuri-conversion-test--path "../nelisp/scripts/nelisp-stdlib-prelude.el"))

(defconst skk-okuri-conversion-test--vendor-core
  (mapcar #'skk-okuri-conversion-test--path
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

(defun skk-okuri-conversion-test--slurp (file)
  "Return FILE contents as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

;;; --- host source-shape (always runs) -------------------------------------

(ert-deftest skk-okuri-conversion-test/source-shape ()
  "nemacs-runtime-skk.el exposes the okuri API and balances parens."
  (should (file-readable-p skk-okuri-conversion-test--skk-source))
  (with-temp-buffer
    (insert-file-contents skk-okuri-conversion-test--skk-source)
    (goto-char (point-min))
    (check-parens))
  (let ((source (skk-okuri-conversion-test--slurp
                 skk-okuri-conversion-test--skk-source)))
    (dolist (needle '("(defconst skk-okuri--column"
                      "(defun skk-okuri--lookup"
                      "(defun skk-okuri-letter"
                      "(defun skk-convert-okuri "
                      "(defun skk-convert-okuri-first"
                      "(defun skk-convert-okuri-string"
                      "(defun skk-convert-auto"))
      (should (string-match-p (regexp-quote needle) source)))))

(ert-deftest skk-okuri-conversion-test/column-table-covers-gojuon ()
  "The okuri column table maps every gojuon column + the five vowels."
  (let ((source (skk-okuri-conversion-test--slurp
                 skk-okuri-conversion-test--skk-source)))
    ;; one representative per consonant column ...
    (dolist (pair '(("く" . "k") ("ぐ" . "g") ("す" . "s") ("ず" . "z")
                    ("つ" . "t") ("づ" . "d") ("ぬ" . "n") ("ふ" . "h")
                    ("ぶ" . "b") ("ぷ" . "p") ("む" . "m") ("ゆ" . "y")
                    ("る" . "r") ("わ" . "w")
                    ;; ... and the vowel-initial okurigana (i-adj / u-verb)
                    ("あ" . "a") ("い" . "i") ("う" . "u") ("え" . "e")
                    ("お" . "o")))
      (should (string-match-p
               (regexp-quote (format "(\"%s\" . \"%s\")" (car pair) (cdr pair)))
               source)))))

(ert-deftest skk-okuri-conversion-test/bridge-fetch-prefers-auto ()
  "The editor fetch path prefers `skk-convert-auto' (nouns + verbs)."
  (let ((source (skk-okuri-conversion-test--slurp
                 skk-okuri-conversion-test--bridge-source)))
    (should (string-match-p
             (regexp-quote "(fboundp 'skk-convert-auto)") source))
    (should (string-match-p
             (regexp-quote "(skk-convert-auto reading)") source))))

;;; --- standalone gate (opt-in) --------------------------------------------

(defun skk-okuri-conversion-test--reader ()
  "Return an absolute, executable standalone NeLisp reader path, or nil.
Candidates are expanded to absolute paths so a relative env value (e.g.
NELISP=../nelisp/target/nelisp) still resolves -- `call-process' treats a
relative program name as a PATH lookup, not a CWD-relative path."
  (catch 'found
    (dolist (candidate
             (list (getenv "NEMACS_GUI_BRIDGE_NELISP")
                   (getenv "NELISP")
                   (skk-okuri-conversion-test--path "../nelisp/target/nelisp")
                   "/tmp/nelisp-snap/nelisp"))
      (when candidate
        (let ((abs (expand-file-name candidate)))
          (when (file-executable-p abs)
            (throw 'found abs)))))
    nil))

(defmacro skk-okuri-conversion-test--skip-unless-standalone (&rest body)
  "Run BODY only when the opt-in standalone conversion gate is enabled."
  (declare (indent 0) (debug t))
  `(cond
    ((not (getenv "NEMACS_RUN_SKK_CONVERSION"))
     (ert-skip "set NEMACS_RUN_SKK_CONVERSION=1 to run standalone SKK checks"))
    ((not (skk-okuri-conversion-test--reader))
     (ert-skip "no standalone reader; set NEMACS_GUI_BRIDGE_NELISP or NELISP"))
    ((not (executable-find "python3"))
     (ert-skip "python3 required to build the fixture CDB"))
    ((not (file-readable-p skk-okuri-conversion-test--cdb-builder))
     (ert-skip "scripts/skk-jisyo-to-cdb.py not found"))
    (t ,@body)))

(defun skk-okuri-conversion-test--build-image (with-bridge)
  "Write a source-v1 vendor-core image; bake the bridge too when WITH-BRIDGE.
Reads + writes as UTF-8 so the embedded hiragana (e.g. the okuri column table)
round-trip byte-for-byte -- a literal raw-byte read would re-encode them."
  (let ((image (make-temp-file "skk-okuri-image-" nil ".nlri"))
        (coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8))
    (with-temp-file image
      (insert ";;; nelisp-runtime-image source-v1\n(progn\n")
      (when (file-readable-p skk-okuri-conversion-test--prelude)
        (insert-file-contents skk-okuri-conversion-test--prelude)
        (goto-char (point-max)))
      (dolist (f skk-okuri-conversion-test--vendor-core)
        (when (file-readable-p f)
          (insert-file-contents f)
          (goto-char (point-max))))
      (when with-bridge
        (insert-file-contents skk-okuri-conversion-test--bridge-source)
        (goto-char (point-max)))
      (insert "\n)\n"))
    image))

(defun skk-okuri-conversion-test--build-fixture-cdb ()
  "Build an isolated fixture SKK CDB; return its path.
A handful of okuri-ari (はしr/かk/たかi) and okuri-nasi (みらい) entries -- the
exact key forms the engine derives -- so the test depends on no host dict."
  (let ((dict (make-temp-file "skk-okuri-fixture-" nil ".dict"))
        (cdb (make-temp-file "skk-okuri-fixture-" nil ".cdb")))
    (with-temp-file dict
      ;; key = yomi (+ okuri column letter); value = candidate string.
      (insert ";; okuri-ari entries.\n")
      (insert "はしr /走/迸/奔/\n")
      (insert "かk /書/描/\n")
      (insert "たかi /高/\n")
      (insert ";; okuri-nasi entries.\n")
      (insert "みらい /未来/味蕾/\n")
      (insert "かく /核/格/各/角/\n"))
    (let ((status (call-process "python3" nil nil nil
                                skk-okuri-conversion-test--cdb-builder
                                dict cdb)))
      (delete-file dict)
      (unless (equal 0 status)
        (ert-fail (format "fixture CDB build failed: status=%S" status))))
    cdb))

(defun skk-okuri-conversion-test--run (reader image form)
  "Run READER exec-runtime-image IMAGE FORM; return captured output (fail on error).
Captures into a buffer (stdout + stderr merged).  A buffer destination is used
rather than `(list stdout-file stderr-file)' because the file-redirect form of
`call-process' yields empty files under some sandboxed nested-Emacs hosts,
whereas buffer capture is reliable; the assertions match substrings, so the
merged stream is fine."
  (with-temp-buffer
    (let ((status (call-process reader nil (current-buffer) nil
                                "exec-runtime-image" image form)))
      (unless (equal 0 status)
        (ert-fail
         (format "exec-runtime-image failed: status=%S\noutput:\n%s"
                 status (buffer-string))))
      (buffer-string))))

(ert-deftest skk-okuri-conversion-test/standalone-engine ()
  "Standalone runtime converts nouns + conjugated verbs / adjectives."
  (skk-okuri-conversion-test--skip-unless-standalone
    (let ((reader (skk-okuri-conversion-test--reader))
          (image (skk-okuri-conversion-test--build-image nil))
          (cdb (skk-okuri-conversion-test--build-fixture-cdb)))
      (unwind-protect
          (let ((out (skk-okuri-conversion-test--run
                      reader image
                      (format "(progn (setq skk-cdb-dict-path %S)
  (princ (concat
    \"nasi=\" (or (skk-convert-first \"みらい\") \"NIL\") \"\\n\"
    \"kaku=\" (or (skk-convert-okuri-first \"か\" \"く\") \"NIL\") \"\\n\"
    \"hashiru=\" (or (skk-convert-okuri-first \"はし\" \"る\") \"NIL\") \"\\n\"
    \"takai=\" (or (skk-convert-okuri-first \"たか\" \"い\") \"NIL\") \"\\n\"
    \"letter-ku=\" (or (skk-okuri-letter \"く\") \"NIL\") \"\\n\")))" cdb))))
            (should (string-match-p "nasi=未来" out))
            (should (string-match-p "kaku=書く" out))
            (should (string-match-p "hashiru=走る" out))
            (should (string-match-p "takai=高い" out))
            (should (string-match-p "letter-ku=k" out)))
        (delete-file image)
        (delete-file cdb)))))

(ert-deftest skk-okuri-conversion-test/standalone-keystroke-e2e ()
  "Editor IME loop: buffer はしる + SPC -> 走る, cycle -> 迸る (verb okuri)."
  (skk-okuri-conversion-test--skip-unless-standalone
    (let ((reader (skk-okuri-conversion-test--reader))
          (image (skk-okuri-conversion-test--build-image t))
          (cdb (skk-okuri-conversion-test--build-fixture-cdb)))
      (unwind-protect
          (let ((out (skk-okuri-conversion-test--run
                      reader image
                      (format "(progn (setq skk-cdb-dict-path %S)
  (setq files--buffer-string \"はしる\")
  (setq files--point (length files--buffer-string))
  (nl-write-file (files--ime-seg-path) \"0\")
  (nl-write-file (files--ime-cands-path) \"\")
  (nl-write-file (files--ime-idx-path) \"\")
  (files--ime-convert) (princ (concat \"spc1=\" files--buffer-string \"\\n\"))
  (files--ime-convert) (princ (concat \"spc2=\" files--buffer-string \"\\n\")))"
                              cdb))))
            (should (string-match-p "spc1=走る" out))
            (should (string-match-p "spc2=迸る" out)))
        (delete-file image)
        (delete-file cdb)))))

(ert-deftest skk-okuri-conversion-test/standalone-okuri-marking-keystrokes ()
  "SKK okuri marking: a mid-reading capital routes the okurigana so the verb
converts directly.  か K u SPC -> 書く (not the noun 核 the plain reading gives).
Drives the real self-insert-command dispatch (lowercase compose, capital marker,
SPC convert)."
  (skk-okuri-conversion-test--skip-unless-standalone
    (let ((reader (skk-okuri-conversion-test--reader))
          (image (skk-okuri-conversion-test--build-image t))
          (cdb (skk-okuri-conversion-test--build-fixture-cdb)))
      (unwind-protect
          (let ((out (skk-okuri-conversion-test--run
                      reader image
                      (format "(progn (setq skk-cdb-dict-path %S)
  (setq files--input-method \"default\")
  ;; --- か K u SPC : capital K marks the okurigana start ---
  (setq files--buffer-string \"\") (setq files--point 0)
  (files--ime-commit-state) (nl-write-file (files--ime-pending-path) \"\")
  (setq files--bridge-arg \"k\") (self-insert-command)
  (setq files--bridge-arg \"a\") (self-insert-command)
  (setq files--bridge-arg \"K\") (self-insert-command)
  (setq files--bridge-arg \"u\") (self-insert-command)
  (princ (concat \"reading=\" files--buffer-string
                 \" mark=\" (number-to-string (files--ime-read-num (files--ime-okuri-path))) \"\\n\"))
  (setq files--bridge-arg \" \") (self-insert-command)
  (princ (concat \"okuri=\" files--buffer-string \"\\n\"))
  ;; --- plain か く SPC : no capital -> the noun, unchanged ---
  (setq files--buffer-string \"\") (setq files--point 0)
  (files--ime-commit-state) (nl-write-file (files--ime-pending-path) \"\")
  (setq files--bridge-arg \"k\") (self-insert-command)
  (setq files--bridge-arg \"a\") (self-insert-command)
  (setq files--bridge-arg \"k\") (self-insert-command)
  (setq files--bridge-arg \"u\") (self-insert-command)
  (setq files--bridge-arg \" \") (self-insert-command)
  (princ (concat \"noun=\" files--buffer-string \"\\n\")))"
                              cdb))))
            (should (string-match-p "reading=かく mark=3" out))
            (should (string-match-p "okuri=書く" out))
            (should (string-match-p "noun=核" out)))
        (delete-file image)
        (delete-file cdb)))))

(provide 'skk-okuri-conversion-test)

;;; skk-okuri-conversion-test.el ends here
