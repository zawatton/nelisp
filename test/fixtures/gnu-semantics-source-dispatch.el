;;; gnu-semantics-source-dispatch.el --- Source load dispatch -*- lexical-binding: t; -*-

(defvar gs-dispatch-count 0)
(defvar gs-dispatch-ensures 0)
(let* ((directory (expand-file-name "gnu-semantics-source-dispatch/"
                                     (file-name-directory
                                      (or load-file-name buffer-file-name))))
       (file (expand-file-name "nelisp-native-raw-file.el" directory))
       (had-ensure (fboundp 'nelisp-native-startup-ensure))
       (original-ensure (and had-ensure (symbol-function 'nelisp-native-startup-ensure)))
       (features features))
  (unwind-protect
      (progn
        ;; The counter makes an unintended compiler startup observable without
        ;; running its proofs. GNU never invokes this private function.
        (fset 'nelisp-native-startup-ensure
              (lambda ()
                (setq gs-dispatch-ensures (1+ gs-dispatch-ensures))
                (provide 'nelisp-native-raw-file)))
        (load file nil t)
        (let ((load-path (cons directory load-path)))
          (load "nelisp-native-raw-file" nil t))
        (require 'nelisp-native-raw-file)
        (prin1 (list 'source-dispatch 'loads gs-dispatch-count
                     'ensures gs-dispatch-ensures))
        (terpri))
    (if had-ensure (fset 'nelisp-native-startup-ensure original-ensure)
      (fmakunbound 'nelisp-native-startup-ensure))))

(kill-emacs 0)

;;; gnu-semantics-source-dispatch.el ends here
