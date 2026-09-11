;;; nelisp-repl-code.el --- provenance for live REPL definitions -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Keep the small amount of provenance needed to explain which source and
;; reload generation supplied a function currently callable in a REPL.
;; Function identity is sampled at publication time; a later `fset' therefore
;; reports stale instead of presenting old source metadata as current.

;;; Code:

(require 'nelisp-artifact)
(require 'cl-lib)

(defvar nelisp-repl-code--records (make-hash-table :test #'eq))
(defvar nelisp-repl-code--reload-hook-installed nil)

(defun nelisp-repl-code--source-sha (path)
  (when (and (stringp path) (file-readable-p path))
    (secure-hash 'sha256
                 (with-temp-buffer
                   (insert-file-contents-literally path)
                   (buffer-string)))))

(defun nelisp-repl-code--record (name result definition)
  (let ((source (plist-get result :source)))
    (puthash name
             (list :name name
                   :source source
                   :source-span (plist-get definition :source-span)
                   :source-sha256 (plist-get result :source-sha256)
                   :artifact-sha256 (plist-get result :artifact-sha256)
                   :reload-generation (plist-get result :generation)
                   :function-identity (and (fboundp name)
                                           (symbol-function name))
                   ;; This hook observes module replay.  It does not inspect
                   ;; machine code or infer a native execution route.
                   :execution-route :artifact-replay
                   :loaded-definition name)
             nelisp-repl-code--records)))

(defun nelisp-repl-code--reload-advice (orig source &optional build-id)
  (let ((result (funcall orig source build-id)))
    (when (memq (plist-get result :status) '(ok partial))
      (dolist (name (plist-get result :published))
        (let ((definition (cl-find name (plist-get result :definitions)
                                   :key (lambda (x) (plist-get x :name))
                                   :test #'equal)))
          (nelisp-repl-code--record name result definition))))
    result))

(unless nelisp-repl-code--reload-hook-installed
  (if (fboundp 'advice-add)
      (advice-add 'nelisp-artifact-reload-source-file :around
                  #'nelisp-repl-code--reload-advice)
  ;; The standalone runtime has no advice.el.  Install the same narrow hook
  ;; with an explicit alias, preserving the original callable function.
  (defalias 'nelisp-repl-code--reload-original
    (symbol-function 'nelisp-artifact-reload-source-file))
    (fset 'nelisp-artifact-reload-source-file
          (lambda (source &optional build-id)
            (nelisp-repl-code--reload-advice
             #'nelisp-repl-code--reload-original source build-id))))
  (setq nelisp-repl-code--reload-hook-installed t))

;;;###autoload
(defun nelisp-repl-code-forget (&optional function)
  "Forget provenance for FUNCTION, or all records when FUNCTION is nil."
  (if function
      (remhash (if (symbolp function) function (intern function))
               nelisp-repl-code--records)
    (clrhash nelisp-repl-code--records))
  nil)

;;;###autoload
(defun nelisp-repl-code-info (function)
  "Return provenance and freshness information for FUNCTION.

`:source-current' and `:function-current' are separate: an unchanged source
can have been replaced by `fset', and a function can still be callable after
its source file has changed.  Unknown source spans are reported as nil; this
API does not infer an exact failing expression location."
  (let* ((name (if (symbolp function) function (intern function)))
         (record (gethash name nelisp-repl-code--records))
         (source (and record (plist-get record :source)))
         (want (and record (plist-get record :source-sha256)))
         (actual (nelisp-repl-code--source-sha source))
         (identity (and (fboundp name) (symbol-function name))))
    (if (null record)
        (list :name name :status :unknown :execution-route :unknown
              :loaded-definition nil :source-current nil
              :function-current nil :stale t)
      (let ((source-current (and want actual (equal want actual)))
            (function-current (eq identity
                                  (plist-get record :function-identity))))
        (list :name name :status (if (and source-current function-current)
                                     :current :stale)
              :source source :source-span (plist-get record :source-span)
              :source-sha256 want :current-source-sha256 actual
              :source-current source-current
              :artifact-sha256 (plist-get record :artifact-sha256)
              :reload-generation (plist-get record :reload-generation)
              :execution-route (if function-current
                                   (plist-get record :execution-route)
                                 :unknown)
              :recorded-execution-route (plist-get record :execution-route)
              :loaded-definition (plist-get record :loaded-definition)
              :function-current function-current
              :stale (not (and source-current function-current)))))))

(provide 'nelisp-repl-code)
;;; nelisp-repl-code.el ends here
