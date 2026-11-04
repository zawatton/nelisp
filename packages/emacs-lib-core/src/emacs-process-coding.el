;;; emacs-process-coding.el --- Shared process coding owner -*- lexical-binding: t; -*-

(require 'emacs-process)

(defun emacs-process-coding-get (process)
  "Return the directional coding pair owned by PROCESS."
  (if (emacs-process--process-object-p process)
      (or (emacs-process--native-metadata process :coding)
          '(utf-8-unix . utf-8-unix))
    (if (and (fboundp 'emacs-process-events--processp)
             (emacs-process-events--processp process))
        (let ((coding (emacs-process-events--get process 9)))
          (if (consp coding) coding (cons coding coding)))
      (signal 'wrong-type-argument (list 'processp process)))))

(defun emacs-process-coding-set (process &optional decoding encoding)
  "Validate both coding directions before publishing a new pair."
  (unless (or (emacs-process--process-object-p process)
              (and (fboundp 'emacs-process-events--processp)
                   (emacs-process-events--processp process)))
    (signal 'wrong-type-argument (list 'processp process)))
  (check-coding-system decoding)
  (check-coding-system encoding)
  (if (emacs-process--process-object-p process)
      (emacs-process--native-set-metadata process :coding (cons decoding encoding))
    (if (and (fboundp 'emacs-process-events--processp)
             (emacs-process-events--processp process))
        (emacs-process-events--set process 9 (cons decoding encoding))
      (signal 'wrong-type-argument (list 'processp process))))
  nil)

(defun emacs-process-coding-convert (text coding encode)
  "Convert TEXT using one directional coding system."
  (if (memq coding '(nil binary no-conversion)) text
    (if encode (encode-coding-string text coding t)
      (decode-coding-string text coding t))))

(provide 'emacs-process-coding)
