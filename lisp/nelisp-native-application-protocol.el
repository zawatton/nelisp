;;; nelisp-native-application-protocol.el --- Pure producer source data -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This module reads source as data. It does not initialize editor libraries.
(defconst nelisp-native-application-protocol-max-files 32)
(defconst nelisp-native-application-protocol-max-definitions 256)
(defconst nelisp-native-application-protocol-max-file-bytes 1048576)
(defconst nelisp-native-application-protocol-max-total-bytes 8388608)

(defun nelisp-native-application-protocol-lexical-header-p (bytes)
  "Recognize the unchanged bounded producer lexical-header contract."
  (let* ((source (decode-coding-string bytes 'utf-8-unix))
         (first-line (car (split-string source "\n")))
         (start 0) (count 0) opener opener-end closer block)
    (while (string-match "lexical-binding[ \t]*:" source start)
      (setq count (1+ count) start (match-end 0)))
    (setq opener (and (string-prefix-p ";;;" first-line)
                      (string-match "-\\*-" first-line))
          opener-end (and opener (match-end 0))
          closer (and opener-end (string-match "-\\*-" first-line opener-end))
          block (and closer (substring first-line opener-end closer)))
    (and (= count 1) block
         (string-match-p "\\`[ \t]*lexical-binding:[ \t]*t;?[ \t]*\\'" block))))

(defun nelisp-native-application-protocol-print (data)
  "Print DATA with the unchanged admission canonical printer bindings."
  (let ((print-length nil) (print-level nil) (print-circle nil)
        (print-quoted t) (print-gensym t) (print-escape-nonascii t))
    (prin1-to-string data)))

(defun nelisp-native-application-protocol-digest (data)
  "Hash the exact canonical representation of DATA."
  (secure-hash 'sha256 (nelisp-native-application-protocol-print data)))

(defun nelisp-native-application-protocol-bytes (file &optional limit)
  "Read literal FILE bytes under LIMIT, checking file identity and growth."
  (let* ((limit (or limit 1048576))
         (before (and (file-regular-p file) (not (file-symlink-p file))
                      (equal file (file-truename file)) (file-attributes file)))
         (size (and before (file-attribute-size before))))
    (unless (and before (<= 0 size limit))
      (error "Missing, aliased or oversized protocol input: %s" file))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally file nil 0 (1+ limit))
      (let ((after (file-attributes file)))
        (unless (and after (= (buffer-size) size (file-attribute-size after))
                     (equal (nth 10 before) (nth 10 after))
                     (equal (nth 11 before) (nth 11 after))
                     (equal (nth 5 before) (nth 5 after))
                     (equal (nth 6 before) (nth 6 after)))
          (error "Protocol input changed during bounded read: %s" file)))
      (buffer-string))))

(defun nelisp-native-application-protocol--trivia-end (text position)
  "Skip exactly GNU31 Lisp top-level whitespace and semicolon comments.
The pinned GNU syntax policy includes U+2000..U+200B and U+3000, but
excludes CR, vertical tab, U+0085, U+1680 and BOM. The genuine reader
still handles those characters inside or at the start of a form."
  (let ((end (length text)) done)
    (while (and (< position end) (not done))
      (let ((char (aref text position)))
        (cond
         ((or (= char 9) (= char 10) (= char 12) (= char 32) (= char 160)
              (and (<= 8192 char) (<= char 8203))
              (= char 8239) (= char 8287) (= char 12288))
          (setq position (1+ position)))
         ((= char 59)
          (while (and (< position end) (/= (aref text position) 10))
            (setq position (1+ position))))
         (t (setq done t)))))
    position))

(defun nelisp-native-application-protocol-forms (bytes)
  "Read source forms as data without editor initialization or evaluation.
Only clean top-level trivia may terminate input. EOF inside a form is
propagated. Reader syntax, quoting and escaping belong to read-from-string."
  (defvar read-eval)
  (let* ((text (decode-coding-string bytes 'utf-8-unix))
         (end (length text)) (position 0)
         (read-circle nil) (read-symbol-shorthands nil) (read-eval nil) forms)
    (while (progn
             (setq position (nelisp-native-application-protocol--trivia-end text position))
             (< position end))
      (let ((result (read-from-string text position)))
        (unless (and (consp result) (integerp (cdr result))
                     (< position (cdr result)) (<= (cdr result) end))
          (error "Protocol reader did not advance"))
        (push (car result) forms)
        (setq position (cdr result))))
    (nreverse forms)))

(provide 'nelisp-native-application-protocol)
;;; nelisp-native-application-protocol.el ends here
