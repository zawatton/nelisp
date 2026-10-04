;;; nelisp-bytecode-native-consumer.el --- Read compiled inputs without producer dependencies -*- lexical-binding: t; -*-
;; Parser bodies are extracted unchanged from the public package reader.
(require 'cl-lib)

(defun nelisp-bytecode-native-consumer-read-elc-forms (path)
  "Read GNU byte-code forms from PATH without evaluating them."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (goto-char (point-min))
    (unless (looking-at ";ELC")
      (error "bytecode-native-package: not a GNU .elc file: %s" path))
    (forward-line 1)
    (let (forms done)
      (while (not done)
        (skip-chars-forward " \t\r\n")
        (while (eq (char-after) ?\;)
          (forward-line 1)
          (skip-chars-forward " \t\r\n"))
        (while (looking-at "#@\\([0-9]+\\)[ \t]")
          (let* ((length (string-to-number (match-string 1)))
                 ;; GNU counts the separator itself, beginning immediately
                 ;; after the decimal digits. The trailing LF is not counted.
                 (payload-start (match-end 1))
                 (payload-end (+ payload-start length)))
            (when (<= length 0)
              (error "bytecode-native-package: invalid zero-length .elc docstring"))
            (when (> payload-end (point-max))
              (error "bytecode-native-package: truncated .elc docstring"))
            (goto-char payload-end)
            (skip-chars-forward " \t\r\n")))
        (if (= (point) (point-max))
            (setq done t)
          ;; Any EOF raised after a form begins is malformed truncation.
          (push (read (current-buffer)) forms)))
      (nreverse forms))))

(defun nelisp-bytecode-native-consumer-read-elc-definitions (forms)
  "Return (SYMBOL . BYTE-CODE-FUNCTION) pairs found in unread FORMS."
  (let (definitions)
    (dolist (form forms)
      (when (and (consp form) (eq (car form) 'defalias)
                 (consp (cdr form)) (consp (cddr form)))
        (let* ((quoted-name (cadr form))
               (name (if (and (consp quoted-name)
                              (eq (car quoted-name) 'quote))
                         (cadr quoted-name)
                       quoted-name))
               (function (nth 2 form)))
          (when (and (symbolp name) (byte-code-function-p function))
            (push (cons name function) definitions)))))
    (nreverse definitions)))

(defun nelisp-bytecode-native-consumer-read-elc-function (path name)
  "Read byte-code definition NAME from PATH without evaluating the .elc."
  (cdr (assq name
             (nelisp-bytecode-native-consumer-read-elc-definitions
              (nelisp-bytecode-native-consumer-read-elc-forms path)))))

(defun nelisp-bytecode-native-consumer-read-elc-functions (path)
  "Read byte-code function definitions from PATH without evaluating the .elc."
  (nelisp-bytecode-native-consumer-read-elc-definitions
   (nelisp-bytecode-native-consumer-read-elc-forms path)))

(provide 'nelisp-bytecode-native-consumer)
