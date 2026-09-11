;;; nelisp-dev-replay-worker.el --- explicit host recipe worker -*- lexical-binding: t; -*-
(require 'nelisp-dev-session)

(defun nelisp-dev-replay-worker--bytes (path limit)
  "Read at most LIMIT bytes, refusing oversized or growing inputs."
  (unless (and (file-regular-p path) (file-readable-p path))
    (error "Unreadable replay input"))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path nil 0 (1+ limit))
    (when (> (buffer-size) limit) (error "Replay input exceeds byte limit"))
    (buffer-string)))

(defun nelisp-dev-replay-worker--run (manifest-path expected-hash)
  "Validate again, then evaluate the exact recipe bytes checked here."
  (let* ((validation (nelisp-dev-session--validate (list :manifest manifest-path)))
         (manifest-bytes (nelisp-dev-replay-worker--bytes manifest-path 1048576)))
    (unless (and (eq (plist-get validation :status) :valid)
                 (equal expected-hash (secure-hash 'sha256 manifest-bytes)))
      (error "Manifest validation failed or manifest changed"))
    (let* ((manifest (nelisp-dev-protocol-string-keys
                     (json-parse-string (decode-coding-string manifest-bytes 'utf-8-unix)
                                      :object-type 'alist :array-type 'array
                                      :null-object :null :false-object :false)))
           (recipe (expand-file-name (cdr (assoc "recipe" manifest))
                                     (file-name-directory manifest-path)))
           (expected-size (cdr (assoc "recipe_bytes" manifest)))
           (bytes (nelisp-dev-replay-worker--bytes recipe expected-size))
           (text (decode-coding-string bytes 'utf-8-unix))
           (count 0) (phase "read") (line 1))
      (unless (and (= (length bytes) expected-size)
                   (equal (downcase (cdr (assoc "recipe_sha256" manifest)))
                          (secure-hash 'sha256 bytes)))
        (error "Recipe changed before execution"))
      (when (cl-some (lambda (ch) (> ch #x10ffff)) (string-to-list text))
        (error "Recipe must contain valid UTF-8"))
      (with-temp-buffer
        (insert text)
        (goto-char (point-min))
        (let ((load-file-name recipe) forms)
          (condition-case err
              (progn
                (while (progn
                         (skip-chars-forward " \t\r\n")
                         (while (eq (char-after) ?\;)
                           (forward-line 1) (skip-chars-forward " \t\r\n"))
                         (not (eobp)))
                  (setq phase "read" line (line-number-at-pos))
                  (push (cons line (read (current-buffer))) forms)
                  (when (> (length forms) 128) (error "Too many replay forms")))
                (unless (= (length forms) (cdr (assoc "record_count" manifest)))
                  (error "Recipe form count does not match manifest"))
                ;; Parsing finishes before application code can change buffers
                ;; or their contents.  Evaluating a buffer-editing recipe must
                ;; not change which already-verified forms are executed.
                (setq phase "execute")
                (dolist (entry (nreverse forms))
                  (setq line (car entry))
                  (eval (cdr entry) nil)
                  (setq count (1+ count)))
                (list (cons "status" (if (> count 0) "ok" "inconclusive"))
                      (cons "phase" "complete") (cons "executed_forms" count)))
            (error
             (list '("status" . "failed") (cons "phase" phase)
                   (cons "executed_forms" count) (cons "form_index" (1+ count))
                   (cons "line" line)
                   (cons "condition" (symbol-name (car err)))
                   (cons "message" (if (stringp (cadr err))
                                       (substring (cadr err) 0 (min 512 (length (cadr err))))
                                     "Recipe evaluation did not complete"))))))))))

(defun nelisp-dev-replay-worker-main ()
  "Write a terminal record only after validation and evaluation finish."
  (let* ((locale-coding-system 'utf-8-unix)
         (coding-system-for-write 'utf-8-unix)
         (manifest (pop command-line-args-left))
         (result-path (pop command-line-args-left))
         (expected-hash (pop command-line-args-left))
         (nonce (pop command-line-args-left))
         (_ (set-terminal-coding-system 'utf-8-unix))
         (result
          (condition-case err
              (nelisp-dev-replay-worker--run manifest expected-hash)
            (error
             (list '("status" . "failed") '("phase" . "validate")
                   '("executed_forms" . 0)
                   (cons "condition" (symbol-name (car err)))
                   '("message" . "Worker validation failed before execution"))))))
    (setq result (append result (list (cons "nonce" nonce)
                                     (cons "worker_pid" (emacs-pid)))))
    (let ((coding-system-for-write 'utf-8-unix))
      (with-temp-file result-path
        (insert (nelisp-dev-protocol-json result) "\n")))
    (kill-emacs (pcase (cdr (assoc "status" result))
                  ("ok" 0) ("inconclusive" 3) (_ 1)))))

(provide 'nelisp-dev-replay-worker)
