;;; dump-elisp.el --- TSV dump of the Elisp x86-64 decoder  -*- lexical-binding: t; -*-

;; Usage:
;;   emacs --batch -Q -L lisp -l tools/ai/x86-decode-diff/dump-elisp.el \
;;     MODE RANGES_TSV OUT_TSV
;; MODE is "compiled" (byte-compile a temp copy of the decoder first; used
;; for the diff) or "interpreted" (load the .el source as is and time it; no
;; TSV is written when OUT_TSV is "-").
;;
;; RANGES_TSV is written by dump-capstone.py: unit-file unit function start end.
;; The output matches dump-capstone.py's instruction TSV byte for byte when
;; the decoders agree.

;;; Code:

(defvar x86-dump--args command-line-args-left)
(setq command-line-args-left nil)

(let ((mode (nth 0 x86-dump--args)))
  (cond
   ((equal mode "compiled")
    (let* ((source (locate-library "nelisp-x86-64-decode.el"))
           (dir (make-temp-file "x86dec" t))
           (copy (expand-file-name "nelisp-x86-64-decode.el" dir)))
      (copy-file source copy t)
      (byte-compile-file copy)
      (load (expand-file-name "nelisp-x86-64-decode.elc" dir) nil t)))
   (t (load (locate-library "nelisp-x86-64-decode.el") nil t))))

(defvar x86-dump--texts (make-hash-table :test 'equal)
  "Unit file name -> unibyte text string.")

(defun x86-dump--text (file)
  "Return the text section bytes of unit FILE as a unibyte string."
  (or (gethash file x86-dump--texts)
      (puthash
       file
       (with-temp-buffer
         (set-buffer-multibyte nil)
         (insert-file-contents-literally file)
         (goto-char (point-min))
         (unless (re-search-forward "(text :nelisp-cache-bytes-hex \"\\([0-9a-f]+\\)\"" nil t)
           (error "No text section in %s" file))
         (let* ((hex-start (match-beginning 1))
                (hex-end (match-end 1))
                (size (/ (- hex-end hex-start) 2))
                (out (make-string size 0))
                (i 0)
                (q hex-start))
           (while (< i size)
             (aset out i (+ (* 16 (x86-dump--nibble (char-after q)))
                            (x86-dump--nibble (char-after (1+ q)))))
             (setq q (+ q 2))
             (setq i (1+ i)))
           out))
       x86-dump--texts)))

(defun x86-dump--nibble (c)
  "Value of hex digit character C."
  (if (<= c ?9) (- c ?0) (- c (- ?a 10))))

(defun x86-dump--read-ranges (file)
  "Return the list of (UNIT-FILE UNIT FUNCTION START END) rows of FILE."
  (let ((rows nil))
    (with-temp-buffer
      (insert-file-contents file)
      (dolist (line (split-string (buffer-string) "\n" t))
        (let ((f (split-string line "\t")))
          (push (list (nth 0 f) (nth 1 f) (nth 2 f)
                      (string-to-number (nth 3 f)) (string-to-number (nth 4 f)))
                rows))))
    (nreverse rows)))

(defun x86-dump--insn-line (unit name insn)
  "Insert the TSV line of INSN for function NAME of UNIT."
  (let ((control (aref insn 2)))
    (insert unit "\t" name "\t" (number-to-string (aref insn 7)) "\t"
            (number-to-string (aref insn 0)) "\t"
            (if control (symbol-name control) "-") "\t"
            (if (aref insn 3) (symbol-name (aref insn 3)) "-") "\t"
            (if (aref insn 4) (number-to-string (aref insn 4)) "-") "\t"
            (if (aref insn 5) (number-to-string (aref insn 5)) "-") "\t"
            (if (aref insn 6) (number-to-string (aref insn 6)) "-") "\t"
            (if control (aref insn 1) "-") "\n")))

(defun x86-dump--run (ranges-file out-file)
  "Decode every range of RANGES-FILE; write TSV to OUT-FILE unless it is \"-\"."
  (let ((rows (x86-dump--read-ranges ranges-file))
        (write (not (equal out-file "-")))
        (count 0) (bytes-total 0) (elapsed 0.0) (errors nil))
    ;; Pre-load texts so the timing covers decoding only.
    (dolist (row rows) (x86-dump--text (nth 0 row)))
    (with-temp-buffer
      (dolist (row rows)
        (let* ((unit (nth 1 row)) (name (nth 2 row))
               (start (nth 3 row)) (end (nth 4 row))
               (text (x86-dump--text (nth 0 row)))
               (t0 (float-time))
               (insns (condition-case nil
                          (nelisp-x86-64-decode-range text start end 0)
                        (nelisp-x86-64-decode-error nil))))
          (setq elapsed (+ elapsed (- (float-time) t0)))
          (setq bytes-total (+ bytes-total (- end start)))
          (cond
           (insns
            (setq count (+ count (length insns)))
            (when write (dolist (insn insns) (x86-dump--insn-line unit name insn))))
           (t
            ;; Partial decode, mirroring capstone which stops at the first
            ;; undecodable byte.
            (let ((pos start) (done 0) (stop nil))
              (while (and (< pos end) (not stop))
                (condition-case err
                    (let ((insn (nelisp-x86-64-decode-insn text pos end pos)))
                      (when write (x86-dump--insn-line unit name insn))
                      (setq count (1+ count))
                      (setq pos (+ pos (aref insn 0)))
                      (setq done (+ done (aref insn 0))))
                  (nelisp-x86-64-decode-error
                   (setq stop err)
                   (push (cons name (cdr err)) errors))))
              (when (and write (/= done (- end start)))
                (insert (format "RANGE-MISMATCH\t%s\t%s\t%d\t%d\n"
                                unit name done (- end start)))))))))
      (when write
        (let ((coding-system-for-write 'no-conversion))
          (write-region (point-min) (point-max) out-file))))
    (let ((n (length errors)))
      (dolist (e (seq-take (nreverse errors) 40))
        (message "stop: %S" e))
      (message "stops=%d" n))
    (message "ranges=%d instructions=%d bytes=%d decode-seconds=%.3f"
             (length rows) count bytes-total elapsed)))

(x86-dump--run (nth 1 x86-dump--args) (nth 2 x86-dump--args))
