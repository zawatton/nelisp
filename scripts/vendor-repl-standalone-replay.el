;;; vendor-repl-standalone-replay.el --- standalone REPL vendor load replay  -*- lexical-binding: t; -*-

;;; Code:

(require 'standalone-source-normalize)

(defvar vendor-repl-standalone-reader nil
  "Path to target/nelisp or a compatible standalone reader binary.")

(defvar vendor-repl-standalone-bootstrap-repl nil
  "Path to the generated nemacs bootstrap REPL input.")

(defvar vendor-repl-standalone-prelude nil
  "Path to the standalone reader stdlib prelude.")

(defvar vendor-repl-standalone-files nil
  "Whitespace-separated string or list of vendor files to load in the REPL.")

(defvar vendor-repl-standalone-proof-form
  "(fboundp (quote emacs-keymap-define-key-after))"
  "Raw Lisp form that must be true after REPL load replay.")

(defvar vendor-repl-standalone-detail-form "nil"
  "Raw Lisp form returning a diagnostic string when the proof form is false.")

(defvar vendor-repl-standalone-repo-root
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name)))
  "Repository root.")

(defvar vendor-repl-standalone-keep-temp nil
  "When non-nil, keep generated REPL input/output files for diagnostics.")

(defvar vendor-repl-standalone-trace-forms nil
  "When non-nil, record per-form progress in the sentinel marker.")

(defconst vendor-repl-standalone--success
  "VENDOR-REPL-STANDALONE=ok"
  "Marker-file sentinel written by a successful REPL replay.")

(defconst vendor-repl-standalone--failure
  "VENDOR-REPL-STANDALONE=fail"
  "Marker-file sentinel written by a failed REPL replay proof.")

(defun vendor-repl-standalone--true-name (file)
  "Return FILE as a canonical absolute path."
  (file-truename (expand-file-name file)))

(defun vendor-repl-standalone--repo-root ()
  "Return the canonical repository root for replay-generated paths."
  (file-name-as-directory
   (vendor-repl-standalone--true-name vendor-repl-standalone-repo-root)))

(defun vendor-repl-standalone--record-load-form (file marker)
  "Return REPL forms that load FILE and record the outcome."
  (let ((name (file-name-nondirectory file))
        (index 0))
    (concat
     (format "(setq vendor-repl-load-status (concat %S (number-to-string vendor-standalone-load-ok-count) %S (number-to-string (ptr-read-u64 268435456 0))))\n"
             (concat "start:" name ":count=") ":bump=")
     (format "(nl-write-file %S vendor-repl-load-status)\n" marker)
     (format "(setq load-file-name %S)\n" file)
     (format "(setq buffer-file-name %S)\n" file)
     (mapconcat (lambda (source)
                  (setq index (1+ index))
                  (vendor-repl-standalone--eval-source-form
                   source marker name index))
                (standalone-source-normalize-file-to-form-strings file)
                "")
     "(setq vendor-standalone-load-ok-count (1+ vendor-standalone-load-ok-count))\n"
     (format "(setq vendor-repl-load-status (concat %S (number-to-string vendor-standalone-load-ok-count) %S (number-to-string (ptr-read-u64 268435456 0))))\n"
             (concat "ok:" name ":count=") ":bump=")
     (format "(nl-write-file %S vendor-repl-load-status)\n" marker)
     "")))

(defun vendor-repl-standalone--eval-source-form (source &optional marker file-name index)
  "Return a standalone form that evaluates SOURCE through NeLisp's reader."
  (let ((print-escape-newlines t)
        (eval-form (format "(nelisp--eval-source-string %s)\n"
                           (prin1-to-string source))))
    (if (and vendor-repl-standalone-trace-forms
             marker file-name index)
        (concat
         (format "(setq vendor-repl-load-status (concat %S (number-to-string vendor-standalone-load-ok-count) %S (number-to-string (ptr-read-u64 268435456 0))))\n"
                 (format "form-start:%s:%d:count=" file-name index)
                 ":bump=")
         (format "(nl-write-file %S vendor-repl-load-status)\n" marker)
         eval-form
         (format "(setq vendor-repl-load-status (concat %S (number-to-string vendor-standalone-load-ok-count) %S (number-to-string (ptr-read-u64 268435456 0))))\n"
                 (format "form-ok:%s:%d:count=" file-name index)
                 ":bump=")
         (format "(nl-write-file %S vendor-repl-load-status)\n" marker))
      eval-form)))

(defun vendor-repl-standalone--files ()
  "Return normalized absolute vendor file list."
  (cond
   ((stringp vendor-repl-standalone-files)
    (mapcar #'vendor-repl-standalone--true-name
            (split-string vendor-repl-standalone-files "[ \t\n]+" t)))
   ((listp vendor-repl-standalone-files)
    (mapcar #'vendor-repl-standalone--true-name vendor-repl-standalone-files))
   (t nil)))

(defun vendor-repl-standalone--load-paths ()
  "Return the load paths needed for standalone REPL vendor replay."
  (let ((root (vendor-repl-standalone--repo-root)))
    (list (expand-file-name "src" root)
          (expand-file-name "scripts" root)
          (expand-file-name "vendor/emacs-lisp" root)
          (expand-file-name "vendor/emacs-lisp/emacs-lisp"
                            root)
          (expand-file-name "vendor/emacs-lisp/vc"
                            root))))

(defun vendor-repl-standalone--read-file (file)
  "Return FILE contents as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun vendor-repl-standalone--pretty-form (source)
  "Return SOURCE as a multi-line form string when it is readable."
  (condition-case _err
      (pp-to-string (read source))
    (error source)))

(defun vendor-repl-standalone--write-input (files marker output)
  "Write standalone-reader REPL input for FILES to OUTPUT."
  (let ((coding-system-for-write 'utf-8-unix))
    (with-temp-file output
      (insert ";;; standalone vendor REPL replay probe\n")
      (insert (format "(setq nelisp-emacs-vendor-root %S)\n"
                      (expand-file-name "vendor"
                                        (vendor-repl-standalone--repo-root))))
      (insert (format "(setq load-path '%S)\n"
                      (vendor-repl-standalone--load-paths)))
      (when vendor-repl-standalone-prelude
        (dolist (source (standalone-source-normalize-file-to-form-strings
                         vendor-repl-standalone-prelude))
          (insert (vendor-repl-standalone--eval-source-form source))))
      (insert (vendor-repl-standalone--read-file
               vendor-repl-standalone-bootstrap-repl))
      (unless (bolp)
        (insert "\n"))
      (insert (format "(setq vendor-standalone-load-file-count %d)\n"
                      (length files)))
      (insert "(setq vendor-standalone-load-ok-count 0)\n")
      (insert "(setq vendor-repl-load-status \"\")\n")
      (dolist (file files)
        (insert (vendor-repl-standalone--record-load-form file marker)))
      (insert (format "(setq vendor-repl-standalone-marker-file %S)\n"
                      marker))
      (insert (format "(nl-write-file %S %S)\n" marker "proof:start"))
      (insert "(setq vendor-repl-proof-error nil)\n")
      (insert "(setq vendor-repl-proof-value\n")
      (insert "      (condition-case err\n")
      (insert "          ")
      (insert (vendor-repl-standalone--pretty-form
               vendor-repl-standalone-proof-form))
      (insert "        (error (setq vendor-repl-proof-error err) nil)))\n")
      (insert
       (format "(if vendor-repl-proof-value\n    (nl-write-file %S %S)\n"
               marker vendor-repl-standalone--success))
      (insert "  (progn\n")
      (insert "    (setq vendor-repl-proof-detail\n")
      (insert "          ")
      (insert (vendor-repl-standalone--pretty-form
               vendor-repl-standalone-detail-form))
      (insert "    )\n")
      (insert
       (format "    (nl-write-file %S (format %S vendor-repl-proof-detail vendor-repl-proof-error))))\n"
               marker
               (concat vendor-repl-standalone--failure
                       " detail=%s error=%s")))
      (insert ",quit\n"))))

(defun vendor-repl-standalone--run (files)
  "Run standalone reader REPL on generated input for FILES."
  (let ((tmp (make-temp-file "nemacs-vendor-repl-standalone-" nil ".repl"))
        (out (make-temp-file "nemacs-vendor-repl-standalone-" nil ".out"))
        (marker (make-temp-file "nemacs-vendor-repl-standalone-" nil ".sentinel"))
        (start (float-time))
        exit elapsed output sentinel)
    (delete-file marker)
    (unwind-protect
        (progn
          (vendor-repl-standalone--write-input files marker tmp)
          (setq exit
                (call-process
                 "/bin/sh" nil (list out t) nil
                 "-c" "exec \"$1\" --repl --no-prompt --no-print < \"$2\""
                 "vendor-repl-standalone"
                 vendor-repl-standalone-reader
                 tmp))
          (setq elapsed (- (float-time) start))
          (setq output (vendor-repl-standalone--read-file out))
          (setq sentinel (and (file-exists-p marker)
                              (vendor-repl-standalone--read-file marker)))
          (list exit elapsed output sentinel tmp out marker))
      (unless vendor-repl-standalone-keep-temp
        (dolist (file (list tmp out marker))
          (when (file-exists-p file)
            (delete-file file)))))))

(defun vendor-repl-standalone-batch ()
  "Load vendor files through a persistent standalone-reader REPL."
  (unless (and vendor-repl-standalone-reader
               (file-executable-p vendor-repl-standalone-reader))
    (error "vendor-repl-standalone-reader is not executable: %S"
           vendor-repl-standalone-reader))
  (unless (and vendor-repl-standalone-bootstrap-repl
               (file-readable-p vendor-repl-standalone-bootstrap-repl))
    (error "vendor-repl-standalone-bootstrap-repl is not readable: %S"
           vendor-repl-standalone-bootstrap-repl))
  (when (and vendor-repl-standalone-prelude
             (not (file-readable-p vendor-repl-standalone-prelude)))
    (error "vendor-repl-standalone-prelude is not readable: %S"
           vendor-repl-standalone-prelude))
  (let ((files (vendor-repl-standalone--files)))
    (unless files
      (error "vendor-repl-standalone-files is empty"))
    (dolist (file files)
      (unless (file-readable-p file)
        (error "vendor REPL load file is not readable: %S" file)))
    (princ (format "vendor-repl-standalone files=%S proof=%s detail=%s status=start\n"
                   files vendor-repl-standalone-proof-form
                   vendor-repl-standalone-detail-form))
    (pcase-let ((`(,exit ,elapsed ,output ,sentinel ,tmp ,out ,marker)
                 (vendor-repl-standalone--run files)))
      (if (and (numberp exit)
               (= exit 0)
               (equal sentinel vendor-repl-standalone--success))
          (princ (format "vendor-repl-standalone files=%S proof=%s detail=%s status=done elapsed=%S exit=%S\n"
                         files vendor-repl-standalone-proof-form
                         vendor-repl-standalone-detail-form elapsed exit))
        (princ (format "vendor-repl-standalone files=%S proof=%s detail=%s status=fail elapsed=%S exit=%S sentinel=%S expected-sentinel=%S input=%S output=%S marker=%S\n"
                       files vendor-repl-standalone-proof-form
                       vendor-repl-standalone-detail-form elapsed exit
                       sentinel
                       vendor-repl-standalone--success
                       tmp out marker))
        (princ output)
        (unless (string-suffix-p "\n" output)
          (princ "\n"))
        (kill-emacs 1)))))

(provide 'vendor-repl-standalone-replay)

;;; vendor-repl-standalone-replay.el ends here
