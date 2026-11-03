;;; nelisp-cc-fxir-rows.el --- shared FXIR native row registry -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Pure access to the harvested FXIR rows shared by the native compiler and
;; the bounded application producer.  Loading this file does not generate
;; native source or load application definitions.

;;; Code:

(require 'cl-lib)

(defconst nelisp-cc-fxir-native--data-files
  (mapcar (lambda (f)
            (cons (expand-file-name (concat f ".el")
                                    (file-name-directory (or load-file-name buffer-file-name
                                                             default-directory)))
                  (intern f)))
          '("nelisp-fxir-native-data" "nelisp-fxir-native-corpus-data"))
  "Harvested rows: the prelude hot set, then the fx corpus functions
\(test/nelisp-fx-adopt-corpus.el) whose native transcripts
tools/ai/nc1-native-check.sh compares; a corpus row is dormant unless a
function of that name with the same fxir is defined.")

(defvar nelisp-cc-fxir-native-windows
  (string-prefix-p "windows" (or (getenv "NELISP_STANDALONE_TARGET") ""))
  "Non-nil generates no rows, so the word VM keeps every record: Win64 builds
\(the Win64 lowering is not validated yet).")

(defun nelisp-cc-fxir-native--read-data (file const)
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((found nil) (form nil))
        (condition-case nil
            (while (not found)
              (setq form (read (current-buffer)))
              (when (and (eq (car-safe form) 'defconst) (eq (nth 1 form) const))
                (setq found (eval (nth 2 form) t))))
          (end-of-file nil))
        found))))

(defvar nelisp-cc-fxir-native--additional-rows nil
  "Optional validated admission snapshot; nil keeps the legacy row set.
Only `nelisp-cc-fxir-admission' prepares and activates this host-side value.")

(defun nelisp-cc-fxir-native--rows ()
  "All harvested rows (NAME ID NLOCALS CALLEES IR ARITY); nil for Win64."
  (unless nelisp-cc-fxir-native-windows
    (append
     (apply #'append
            (mapcar (lambda (fc) (nelisp-cc-fxir-native--read-data (car fc) (cdr fc)))
                    nelisp-cc-fxir-native--data-files))
     (when nelisp-cc-fxir-native--additional-rows
       (unless (fboundp 'nelisp-cc-fxir-admission-rows)
         (error "FXIR admission snapshot requires its validator"))
       (nelisp-cc-fxir-admission-rows nelisp-cc-fxir-native--additional-rows)))))

(defun nelisp-cc-fxir-native--mutant-row (rows)
  "The negative-control row: a copy of the first row using `<' with a new id."
  (let ((src (cl-find-if (lambda (r) (string-match-p "(:inline 6 " (prin1-to-string (nth 4 r)))) rows)))
    (when src
      (list (car src) (1+ (apply #'max (mapcar #'cadr rows))) (nth 2 src) (nth 3 src) (nth 4 src)
            (nth 5 src) :mutant))))

(defun nelisp-cc-fxir-native-all-rows ()
  "Harvested rows plus the negative-control row (marked :mutant)."
  (let* ((rows (nelisp-cc-fxir-native--rows))
         (m (and rows (nelisp-cc-fxir-native--mutant-row rows))))
    (if m (append rows (list m)) rows)))

(provide 'nelisp-cc-fxir-rows)

;;; nelisp-cc-fxir-rows.el ends here
