;;; nemacs-vendor-cache-set-test.el --- Doc 142 vendor cache set ERT -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'nemacs-vendor-cache-set)

(defun nemacs-vendor-cache-set-test--entry (proof name)
  "Return NAME's entry result from PROOF."
  (cl-find name (plist-get proof :entries)
           :key (lambda (entry) (plist-get entry :name))))

(defun nemacs-vendor-cache-set-test--copy-source (entry temp-dir)
  "Copy ENTRY source into TEMP-DIR and return the new path."
  (let* ((source (plist-get entry :source-path))
         (target (expand-file-name (file-name-nondirectory source) temp-dir)))
    (copy-file source target t)
    target))

(defun nemacs-vendor-cache-set-test--print-proof (label proof)
  "Print LABEL timing and cache details for PROOF."
  (dolist (entry (plist-get proof :entries))
    (princ
     (format
      "VENDOR-NELC-CACHE-SET %s file=%S key=%s mode=%S cache=%S compiled=%S time=%.6fs reads=%S\n"
      label
      (plist-get entry :name)
      (plist-get entry :key)
      (plist-get entry :mode)
      (plist-get entry :cache-status)
      (plist-get entry :compiled)
      (plist-get entry :elapsed)
      (plist-get entry :source-read-paths))))
  (princ
   (format
    "VENDOR-NELC-CACHE-SET %s aggregate time=%.6fs proof=%S dropped=%S\n"
    label
    (plist-get proof :aggregate-elapsed)
    (plist-get proof :aggregate-proof)
    (plist-get proof :dropped-candidates))))

(ert-deftest nemacs-vendor-cache-set-cold-warm-invalidation ()
  "Doc 142 §11 set proof: cold build, warm artifact load, invalidation."
  (let* ((entries (nemacs-vendor-cache-set-default-entries))
         (cache-root (make-temp-file "nemacs-vendor-cache-set-root-" t))
         (temp-dir (make-temp-file "nemacs-vendor-cache-set-src-" t))
         (source-overrides
          (mapcar (lambda (entry)
                    (cons (plist-get entry :name)
                          (nemacs-vendor-cache-set-test--copy-source entry temp-dir)))
                  entries))
         (invalidated-name 'org-version)
         (invalidated-path (cdr (assq invalidated-name source-overrides)))
         cold-proof warm-proof invalidated-proof
         cold-entry warm-entry invalidated-entry
         cold-format warm-format invalidated-format
         cold-total warm-total invalidated-total aggregate-speedup)
    (setq cold-proof
          (nemacs-vendor-cache-set-run-subprocess
           'nemacs-vendor-cache-set-batch-proof
           `((nemacs-vendor-cache-set-batch-root-override . ,cache-root)
             (nemacs-vendor-cache-set-batch-source-overrides . ,source-overrides))))
    (setq warm-proof
          (nemacs-vendor-cache-set-run-subprocess
           'nemacs-vendor-cache-set-batch-proof
           `((nemacs-vendor-cache-set-batch-root-override . ,cache-root)
             (nemacs-vendor-cache-set-batch-source-overrides . ,source-overrides))))
    (with-temp-buffer
      (insert-file-contents invalidated-path)
      (goto-char (point-max))
      (insert "\n;; cache set invalidation proof\n")
      (let ((coding-system-for-write 'utf-8-unix))
        (write-region (point-min) (point-max) invalidated-path nil 'silent)))
    (setq invalidated-proof
          (nemacs-vendor-cache-set-run-subprocess
           'nemacs-vendor-cache-set-batch-proof
           `((nemacs-vendor-cache-set-batch-root-override . ,cache-root)
             (nemacs-vendor-cache-set-batch-source-overrides . ,source-overrides))))
    (setq cold-entry (nemacs-vendor-cache-set-test--entry cold-proof invalidated-name)
          warm-entry (nemacs-vendor-cache-set-test--entry warm-proof invalidated-name)
          invalidated-entry (nemacs-vendor-cache-set-test--entry
                             invalidated-proof invalidated-name)
          cold-format (nemacs-vendor-cache-set-test--entry cold-proof 'format-spec)
          warm-format (nemacs-vendor-cache-set-test--entry warm-proof 'format-spec)
          invalidated-format (nemacs-vendor-cache-set-test--entry
                              invalidated-proof 'format-spec)
          cold-total (plist-get cold-proof :aggregate-elapsed)
          warm-total (plist-get warm-proof :aggregate-elapsed)
          invalidated-total (plist-get invalidated-proof :aggregate-elapsed)
          aggregate-speedup (if (> warm-total 0.0) (/ cold-total warm-total) 0.0))
    (princ
     (format
      "VENDOR-NELC-CACHE-SET selected=%S preload=%S invalidated=%S\n"
      (plist-get cold-proof :selected-set)
      (plist-get cold-proof :preloads)
      invalidated-name))
    (nemacs-vendor-cache-set-test--print-proof "cold" cold-proof)
    (nemacs-vendor-cache-set-test--print-proof "warm" warm-proof)
    (nemacs-vendor-cache-set-test--print-proof "invalidated" invalidated-proof)
    (princ
     (format
      "VENDOR-NELC-CACHE-SET aggregate cold=%.6fs warm=%.6fs invalidated=%.6fs speedup=%.2fx\n"
      cold-total warm-total invalidated-total aggregate-speedup))
    (dolist (entry (plist-get cold-proof :entries))
      (should (equal (plist-get entry :tuple)
                     (cadr (assoc (plist-get entry :name)
                                  (plist-get cold-proof :aggregate-proof))))))
    (should (equal (plist-get cold-proof :selected-set)
                   '(format-spec org-version org-macs)))
    (should (equal (plist-get warm-proof :selected-set)
                   (plist-get cold-proof :selected-set)))
    (should (equal (plist-get invalidated-proof :selected-set)
                   (plist-get cold-proof :selected-set)))
    ;; No drops: the reader string-escape fix + defalias/cl-defun/defvar-local
    ;; eval builtins (dev/nelisp) make org-macs.el fully cacheable, so the full
    ;; format-spec -> org-version -> org-macs chain is cached.
    (should (null (plist-get cold-proof :dropped-candidates)))
    (should (equal (plist-get cold-proof :aggregate-proof)
                   '((format-spec (t t "x"))
                     (org-version (t t "9.7.11"))
                     (org-macs (t t " x ")))))
    (should (equal (plist-get warm-proof :aggregate-proof)
                   (plist-get cold-proof :aggregate-proof)))
    (should (equal (plist-get invalidated-proof :aggregate-proof)
                   (plist-get cold-proof :aggregate-proof)))
    (dolist (entry (plist-get cold-proof :entries))
      (should (eq (plist-get entry :mode) 'source))
      (should (eq (plist-get entry :cache-status) 'miss))
      (should (plist-get entry :compiled))
      (should (plist-get entry :artifact-exists))
      (should (plist-get entry :manifest-exists)))
    (dolist (entry (plist-get warm-proof :entries))
      (should (eq (plist-get entry :mode) 'artifact))
      (should (eq (plist-get entry :cache-status) 'hit))
      (should-not (plist-get entry :compiled))
      (should-not (plist-get entry :source-read-paths)))
    (should-not (plist-get warm-proof :source-read-events))
    (should (equal (plist-get warm-format :key)
                   (plist-get cold-format :key)))
    (should (eq (plist-get invalidated-format :mode) 'artifact))
    (should (eq (plist-get invalidated-format :cache-status) 'hit))
    (should-not (plist-get invalidated-format :compiled))
    (should-not (plist-get invalidated-format :source-read-paths))
    (should (equal (plist-get invalidated-format :key)
                   (plist-get warm-format :key)))
    (should (eq (plist-get invalidated-entry :mode) 'source))
    (should (eq (plist-get invalidated-entry :cache-status) 'recompiled))
    (should (plist-get invalidated-entry :compiled))
    (should (not (equal (plist-get invalidated-entry :key)
                        (plist-get warm-entry :key))))
    (should (equal (plist-get warm-entry :key)
                   (plist-get cold-entry :key)))
    (should (> cold-total 0.0))
    (should (> warm-total 0.0))
    (should (> invalidated-total 0.0))
    (should (> aggregate-speedup 0.0))))

(provide 'nemacs-vendor-cache-set-test)

;;; nemacs-vendor-cache-set-test.el ends here
