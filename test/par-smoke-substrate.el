;;; par-smoke-substrate.el --- G4 substrate parity -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'replace)

(defmacro sf-par-row (name &rest body)
  `(princ (format "%s %S\n" ,name
                  (condition-case fault
                      (progn ,@body)
                    (error (car fault))))))

(load (expand-file-name "textmodes/text-mode.elc"
                        (or (getenv "NELISP_GNU_LISP")
                            "/usr/local/share/emacs/31.1/lisp"))
      nil t t)
(sf-par-row "GNU bytecode mode"
  (with-temp-buffer
    (text-mode)
    (list major-mode mode-name delay-mode-hooks)))

(defvar sf-par-local-a 'outer-a)
(defvar sf-par-local-b 'outer-b)
(defvar sf-par-binding-trace nil)

(defun sf-par-recreate-local (target)
  (push sf-par-local-a sf-par-binding-trace)
  (kill-local-variable 'sf-par-local-a)
  (push sf-par-local-a sf-par-binding-trace)
  (set (make-local-variable target) 'replacement))

(defun sf-par-local-unwind (target)
  (with-temp-buffer
    (setq-local sf-par-local-a 'original)
    (let ((sf-par-binding-trace nil))
      ;; constant temporary, varbind a, constant recreate, stack-ref 1,
      ;; call 1, unbind 1, return.
      (funcall (make-byte-code 257 (unibyte-string 193 24 194 1 33 41 135)
                               [sf-par-local-a temporary sf-par-recreate-local] 3)
               target)
      (list (nreverse sf-par-binding-trace) sf-par-local-a sf-par-local-b))))

(sf-par-row "local identity at unwind"
  (list (sf-par-local-unwind 'sf-par-local-b)
        (sf-par-local-unwind 'sf-par-local-a)))

(sf-par-row "skip character classes"
  (with-temp-buffer
    (insert "one two")
    (goto-char (point-min))
    (let ((start (skip-chars-forward "[:word:]")))
      (list start
            (skip-chars-forward "^[:word:]")
            (skip-chars-forward "[:word:]")
            (skip-chars-backward "[:word:]")
            (skip-chars-backward "^[:word:]")
            (skip-chars-backward "[:word:]")))))

(sf-par-row "skip class follows syntax changes"
  (with-temp-buffer
    (set-syntax-table (copy-syntax-table))
    (insert "_x")
    (goto-char 1)
    (let ((first (skip-chars-forward "[:word:]")))
      (modify-syntax-entry ?_ "w")
      (list first (skip-chars-forward "[:word:]")))))

(sf-par-row "skip respects literal sets and position limits"
  (with-temp-buffer
    (insert "xxab12cdyy")
    (narrow-to-region 3 9)
    (goto-char (point-min))
    (let ((forward (skip-chars-forward "a-z0-9" (copy-marker 7)))
          (backward (skip-chars-backward "0-9" 0))
          (huge (skip-chars-forward "a-z0-9" (expt 10 30)))
          (small (skip-chars-backward "a-z0-9" (- (expt 10 30)))))
      (list forward backward huge small
            (skip-chars-forward "a-z" 2)
            (condition-case err (skip-chars-forward "a-z" 1.0)
              (wrong-type-argument (cadr err)))
            (condition-case err (skip-chars-backward "a-z" (make-marker))
              (error (cadr err)))))))

(sf-par-row "skip distinguishes encoded bytes and characters"
  (with-temp-buffer
    (insert (string #xe9 #x3fffe9 ?日))
    (goto-char 1)
    (let ((first (skip-chars-forward "[:unibyte:]"))
          (rest (skip-chars-forward "[:multibyte:]"))
          (unicode (skip-chars-backward (string ?日))))
      (list first rest unicode
            (skip-chars-backward (unibyte-string #xe9))
            (skip-chars-backward (string #xe9))
            (skip-chars-forward (unibyte-string #xe9))
            (skip-chars-forward (string #xe9))))))

(sf-par-row "skip maps a multibyte set in a unibyte buffer"
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert (unibyte-string #xe9 #xe9))
    (goto-char 1)
    (list (skip-chars-forward (string #xe9))
          (skip-chars-forward (string #x3fffe9))
          (skip-chars-backward (unibyte-string #xe9)))))

(sf-par-row "skip sees an edited set string"
  (with-temp-buffer
    (insert "abc123")
    (goto-char 1)
    (let* ((spec (copy-sequence "a-z"))
           (first (skip-chars-forward spec)))
      (aset spec 0 ?0)
      (aset spec 2 ?9)
      (list first (skip-chars-forward spec)
            (skip-chars-backward spec)))))

(cl-letf (((symbol-function 'message) #'ignore))
  (sf-par-row "query replace event"
    (with-temp-buffer
      (insert "x12 x34")
      (goto-char 1)
      (let ((unread-command-events '(?!)))
        (query-replace-regexp "x\\([0-9]+\\)" "id-\\1")
        (list (buffer-string) unread-command-events))))

  (sf-par-row "occur after killing its buffer"
    (save-window-excursion
      (with-temp-buffer
        (insert "needle\n")
        (occur "needle")
        (kill-buffer "*Occur*")
        (garbage-collect)
        (occur "needle")
        (prog1 (buffer-local-value 'major-mode (get-buffer "*Occur*"))
          (kill-buffer "*Occur*"))))))

(sf-par-row "binary file and printer"
  (let ((source (make-temp-file "par-smoke-src-"))
        (target (make-temp-file "par-smoke-dst-"))
        (bytes (unibyte-string 31 139 8 0 10 203 172 255)))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'no-conversion))
            (write-region bytes nil source nil 'silent))
          (copy-file source target t)
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally target)
            (list (equal bytes (buffer-string))
                  (equal bytes (read (prin1-to-string (buffer-string)))))))
      (delete-file source)
      (delete-file target))))

(sf-par-row "internal coding round trip"
  (let ((file (make-temp-file "par-smoke-internal-"))
        (text "return 日本語\n"))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'emacs-internal))
            (write-region text nil file nil 'silent))
          (with-temp-buffer
            (let ((coding-system-for-read 'emacs-internal))
              (insert-file-contents file))
            (equal text (buffer-string))))
      (delete-file file))))

(defun sf-par-property-witnesses (table)
  (dotimes (index 128)
    (puthash (propertize (format "ephemeral-%d" index) 'face 'bold) t table))
  (let ((live (propertize (copy-sequence "live") 'face 'italic)))
    (puthash live t table)
    live))

(sf-par-row "ephemeral strings release their properties"
  (let* ((witnesses (make-hash-table :test 'eq :weakness 'key))
         (live (sf-par-property-witnesses witnesses)))
    (garbage-collect)
    (list (> (hash-table-count witnesses) 1)
          (gethash live witnesses)
          (get-text-property 0 'face live))))

(sf-par-row "message logging clears the log buffer undo list"
  (save-current-buffer
    (let ((buffer (messages-buffer))
          (inhibit-message t)
          (message-log-max 10))
      (message "Parity log entry one")
      (let ((first (buffer-local-value 'buffer-undo-list buffer)))
        (buffer-enable-undo buffer)
        (let ((enabled (buffer-local-value 'buffer-undo-list buffer)))
          (message "Parity log entry two")
          (list (eq first t) (null enabled)
                (eq (buffer-local-value 'buffer-undo-list buffer) t)))))))

(sf-par-row "message trimming preserves markers in retained lines"
  (with-temp-buffer
    (let ((messages-buffer-name (buffer-name))
          (inhibit-message t)
          (message-log-max 2))
      (insert "discard\nkeep\n")
      (let ((marker (copy-marker 10)))
        (message "another")
        (let ((kept (list (buffer-string) (marker-position marker)))
              (message-log-max 0))
          (message "zero")
          (list kept (buffer-string) (marker-position marker)))))))

(cl-letf (((symbol-function 'message) #'ignore))
  (sf-par-row "GC counter after opening Dired"
    (let ((directory (make-temp-file "par-smoke-dired-" t))
          (buffer nil))
      (unwind-protect
          (progn
            (write-region "fixture\n" nil (expand-file-name "file.txt" directory) nil 'silent)
            (setq buffer (dired-noselect directory))
            (let ((before gcs-done))
              (garbage-collect)
              (> gcs-done before)))
        (when (buffer-live-p buffer) (kill-buffer buffer))
        (delete-directory directory t)))))
