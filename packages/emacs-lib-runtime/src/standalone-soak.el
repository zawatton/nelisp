;;; standalone-soak.el --- In-process soak diagnostic for nelisp-emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 11 M8 (Stability Release Gate): an in-process soak diagnostic that
;; repeats a buffer create / insert / search / kill scenario and reports the
;; outcome in failure buckets, plus a dependency-free RSS probe.  This is the
;; opt-in long-diagnostic half of the release gate (the fast `make test'
;; stays the default); `scripts/release-preflight.sh' is the operational
;; entry point that chains the gates.

;;; Code:

(require 'nelisp-emacs-compat)
(require 'nelisp-emacs-compat-fileio)

(defun standalone-soak--iteration (n)
  "Run one soak iteration N: create a buffer, insert, search, kill it.
Signals an error if the scenario does not behave as expected; the buffer is
always killed.  Returns nil on success."
  (let ((buf (nelisp-ec-generate-new-buffer (format " *soak-%d*" n))))
    (unwind-protect
        (progn
          (nelisp-ec-set-buffer buf)
          (nelisp-ec-insert (format "line one %d\nline two %d\n" n n))
          (let ((text (nelisp-ec-buffer-string)))
            (unless (string-match-p "line two" text)
              (error "soak: search miss in iteration %d" n)))
          nil)
      (when (nelisp-ec-buffer-p buf)
        (nelisp-ec-kill-buffer buf)))))

(defun standalone-soak-run (iterations)
  "Run ITERATIONS soak iterations and return a bucketed report plist.

The report is (:iterations N :ok K :errors E :buckets ALIST) where ALIST maps
each error-symbol name to its count -- the failure-bucket reporting the
release gate uses to tell a single flake from a systemic failure."
  (let ((ok 0) (errors 0) (buckets nil))
    (dotimes (i iterations)
      (condition-case err
          (progn (standalone-soak--iteration i)
                 (setq ok (1+ ok)))
        (error
         (setq errors (1+ errors))
         (let* ((key (symbol-name (car err)))
                (cell (assoc key buckets)))
           (if cell
               (setcdr cell (1+ (cdr cell)))
             (push (cons key 1) buckets))))))
    (list :iterations iterations
          :ok ok
          :errors errors
          :buckets (nreverse buckets))))

(defun standalone-soak-rss-kb ()
  "Return this process's resident set size in KB, or nil when unavailable.
Reads /proc/self/status (Linux) with no external process, so it is safe to
sample inside a soak loop.  Returns nil on platforms without /proc."
  (condition-case nil
      (when (and (fboundp 'insert-file-contents)
                 (file-readable-p "/proc/self/status"))
        (with-temp-buffer
          (insert-file-contents "/proc/self/status")
          (goto-char (point-min))
          (when (re-search-forward "^VmRSS:[ \t]*\\([0-9]+\\)" nil t)
            (string-to-number (match-string 1)))))
    (error nil)))

(defun standalone-soak-large-file (lines)
  "Soak a large buffer of LINES lines: build it, search its tail, sample RSS.
Returns a plist (:lines N :found BOOL :rss-kb RSS) where FOUND is whether the
last line is locatable in the buffer.  Exercises the large-file path of the
release gate without an external fixture."
  (let ((buf (nelisp-ec-generate-new-buffer " *soak-large*")))
    (unwind-protect
        (progn
          (nelisp-ec-set-buffer buf)
          (dotimes (i lines)
            (nelisp-ec-insert (format "line %d content\n" i)))
          (let* ((text (nelisp-ec-buffer-string))
                 (needle (format "line %d content" (max 0 (1- lines))))
                 (found (and (> lines 0)
                             (string-match-p (regexp-quote needle) text)
                             t)))
            (list :lines lines :found found :rss-kb (standalone-soak-rss-kb))))
      (when (nelisp-ec-buffer-p buf)
        (nelisp-ec-kill-buffer buf)))))

(defun standalone-soak-process ()
  "Process diagnostic: run a trivial subprocess and verify its output.
Returns a plist (:ran BOOL :ok BOOL :output STRING).  Exercises the process
layer so the release gate catches a broken subprocess path."
  (condition-case err
      (if (fboundp 'call-process)
          (let ((out (with-temp-buffer
                       (call-process "echo" nil t nil "soak-process-ok")
                       (buffer-string))))
            (list :ran t
                  :ok (and (stringp out)
                           (string-match-p "soak-process-ok" out) t)
                  :output (string-trim-right out)))
        (list :ran nil :ok nil))
    (error (list :ran nil :ok nil :error (format "%S" err)))))

(defun standalone-soak-project-scan (dir)
  "Project-scan diagnostic: recursively count files and directories under DIR.
Returns a plist (:dir DIR :files N :dirs M).  Exercises directory traversal
the way project-wide commands do."
  (let ((files 0) (dirs 0) (stack (list dir)))
    (while stack
      (let ((d (pop stack)))
        (dolist (name (condition-case nil
                          (nelisp-ec-directory-files d nil nil nil nil)
                        (error nil)))
          (unless (member name '("." ".."))
            (let ((path (nelisp-ec-expand-file-name name d)))
              (if (nelisp-ec-file-directory-p path)
                  (progn (setq dirs (1+ dirs))
                         (push path stack))
                (setq files (1+ files))))))))
    (list :dir dir :files files :dirs dirs)))

(defun standalone-soak-report-string (report)
  "Format a soak REPORT plist as a human-readable bucket summary string."
  (let ((buckets (plist-get report :buckets)))
    (concat
     (format "soak: %d iterations, %d ok, %d errors"
             (plist-get report :iterations)
             (plist-get report :ok)
             (plist-get report :errors))
     (if buckets
         (concat "; buckets: "
                 (mapconcat (lambda (c) (format "%s=%d" (car c) (cdr c)))
                            buckets ", "))
       ""))))

(provide 'standalone-soak)

;;; standalone-soak.el ends here
