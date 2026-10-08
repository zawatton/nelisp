;;; nelisp-native-compile-profile.el --- Profile compiler owners before sealing -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Host: emacs -Q --batch -l tools/ai/nelisp-native-compile-profile.el
;;       --eval '(nelisp-native-compile-profile-prepare "target/compile-profile")'
;; Reader driver: load this file, activate that directory, then load the normal
;; compile driver. NELISP_NATIVE_PROFILE_ROOT overrides the source checkout.
;; Use an ordinary cold image: an image with a frozen compiler correctly refuses
;; instrumented source. Keep profiling caches separate from acceptance caches.
;; These timings include counter overhead and source reload is outside the
;; measured compile. Counters never replace a published owner after capture.

(require 'cl-lib)
(defconst nelisp-native-compile-profile--root
  (expand-file-name "../.." (file-name-directory (or load-file-name buffer-file-name))))
(defconst nelisp-native-compile-profile--modules
  '(nelisp-bytecode-ir nelisp-bytecode-frame-ir nelisp-bytecode-compiler-input
    nelisp-bytecode-native-rooted-cfg-plan nelisp-bytecode-native-rooted-cfg-postdom
    nelisp-bytecode-native-rooted-cfg-shared-emit
    nelisp-bytecode-native-rooted-cfg-contract nelisp-native-load nelisp-native-cache))
(defvar nelisp-native-compile-profile--records nil)
(defvar nelisp-native-compile-profile--stack nil)

(defun nelisp-native-compile-profile--instrument (form)
  (cond
   ((and (consp form) (memq (car form) '(defun cl-defun))
         (symbolp (cadr form)) (string-prefix-p "nelisp-" (symbol-name (cadr form))))
    (let ((body (cdddr form)) prefix)
      (when (stringp (car body)) (push (pop body) prefix))
      (while (and (consp (car body)) (memq (caar body) '(declare interactive)))
        (push (pop body) prefix))
      (append (cl-subseq form 0 3) (nreverse prefix)
              (list `(let* ((profile-start (float-time)) (profile-frame (cons 0.0 nil))
                           (nelisp-native-compile-profile--stack
                            (cons profile-frame nelisp-native-compile-profile--stack)))
                       (unwind-protect (progn ,@body)
                         (nelisp-native-compile-profile--record
                          ',(cadr form) profile-start profile-frame)))))))
   ;; Quoted compiler DSL and templates must retain their exact bytes.
   ((and (consp form) (eq (car form) 'quote)) form)
   ((consp form) (cons (nelisp-native-compile-profile--instrument (car form))
                      (nelisp-native-compile-profile--instrument (cdr form))))
   (t form)))

(defun nelisp-native-compile-profile-prepare (directory)
  "Write instrumented compiler modules to DIRECTORY using host GNU Emacs."
  (make-directory directory t)
  (dolist (module nelisp-native-compile-profile--modules)
    (let ((source (expand-file-name (format "lisp/%s.el" module)
                                    nelisp-native-compile-profile--root)) forms)
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert-file-contents source)
        (goto-char (point-min))
        (while (progn (forward-comment (point-max)) (< (point) (point-max)))
          (let ((form (read (current-buffer))))
            (when (and (eq (car-safe form) 'defconst)
                       (eq (cadr form) 'nelisp-bytecode-compiler-input--root))
              (setq form '(defconst nelisp-bytecode-compiler-input--root
                            (expand-file-name (or (getenv "NELISP_NATIVE_PROFILE_ROOT") ".")))))
            (push (nelisp-native-compile-profile--instrument form) forms))))
      (with-temp-file (expand-file-name (format "%s.el" module) directory)
        (insert ";;; -*- lexical-binding: t; -*-\n")
        (let ((print-length nil) (print-level nil))
          (dolist (form (nreverse forms)) (prin1 form (current-buffer)) (insert "\n"))))))
  (length nelisp-native-compile-profile--modules))

(defun nelisp-native-compile-profile--record (name start frame)
  (let* ((elapsed (- (float-time) start))
         (row (or (gethash name nelisp-native-compile-profile--records) (vector 0 0.0 0.0))))
    (aset row 0 (1+ (aref row 0)))
    (aset row 1 (+ (aref row 1) elapsed))
    (aset row 2 (+ (aref row 2) (- elapsed (car frame))))
    (puthash name row nelisp-native-compile-profile--records)
    (when (cdr nelisp-native-compile-profile--stack)
      (setcar (cadr nelisp-native-compile-profile--stack)
              (+ (car (cadr nelisp-native-compile-profile--stack)) elapsed)))))

(defun nelisp-native-compile-profile-print ()
  "Print one bounded counter row per callee; refuse an empty profile."
  (unless (and nelisp-native-compile-profile--records
               (> (hash-table-count nelisp-native-compile-profile--records) 0))
    (error "No compiler calls were profiled"))
  (maphash (lambda (name row)
             (princ (format "NATIVE-COMPILE-PROFILE %s calls=%d inclusive=%.6f exclusive=%.6f\n"
                            name (aref row 0) (aref row 1) (aref row 2))))
           nelisp-native-compile-profile--records))

(defun nelisp-native-compile-profile-activate (directory)
  "Reload instrumented owners from DIRECTORY in dependency order."
  (when (and (boundp 'nelisp-native-cache--cold-source-check)
             nelisp-native-cache--cold-source-check)
    (error "Compiler source profiling requires an ordinary cold image"))
  (setq nelisp-native-compile-profile--records (make-hash-table :test 'eq)
        nelisp-native-compile-profile--stack nil)
  (add-to-list 'load-path (expand-file-name directory))
  (dolist (module nelisp-native-compile-profile--modules)
    (load (expand-file-name (format "%s.el" module) directory) nil t t))
  ;; Recapture the producer's independently owned validator after its reload.
  (load (expand-file-name "lisp/nelisp-bytecode-native-rooted-cfg-native.el"
                          nelisp-native-compile-profile--root) nil t t)
  ;; Remove module initialization from compile counters.
  (setq nelisp-native-compile-profile--records (make-hash-table :test 'eq))
  t)

(provide 'nelisp-native-compile-profile)
