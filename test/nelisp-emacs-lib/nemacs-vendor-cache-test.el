;;; nemacs-vendor-cache-test.el --- Doc 142 vendor cache ERT -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'nemacs-vendor-cache)

(ert-deftest nemacs-vendor-cache-format-spec-cold-warm-invalidation ()
  "Doc 142 §11: cold compile, warm artifact load, and invalidation work."
  (let* ((temp-source (make-temp-file "nemacs-vendor-cache-format-spec-" nil ".el"))
         (real-source (plist-get (nemacs-vendor-cache-format-spec-entry) :source-path))
         source-proof cold-proof warm-proof invalidated-proof
         cold-time warm-time speedup)
    (unwind-protect
        (progn
          (copy-file real-source temp-source t)
          (setq source-proof
                (nemacs-vendor-cache-run-subprocess
                 'nemacs-vendor-cache-batch-source-proof
                 `((nemacs-vendor-cache-batch-source-path . ,temp-source))))
          (setq cold-proof
                (nemacs-vendor-cache-run-subprocess
                 'nemacs-vendor-cache-batch-load-proof
                 `((nemacs-vendor-cache-batch-source-path . ,temp-source))))
          (setq warm-proof
                (nemacs-vendor-cache-run-subprocess
                 'nemacs-vendor-cache-batch-load-proof
                 `((nemacs-vendor-cache-batch-source-path . ,temp-source))))
          (with-temp-buffer
            (insert-file-contents temp-source)
            (goto-char (point-max))
            (insert "\n;; cache invalidation proof\n")
            (let ((coding-system-for-write 'utf-8-unix))
              (write-region (point-min) (point-max) temp-source nil 'silent)))
          (setq invalidated-proof
                (nemacs-vendor-cache-run-subprocess
                 'nemacs-vendor-cache-batch-load-proof
                 `((nemacs-vendor-cache-batch-source-path . ,temp-source))))
          (setq cold-time (plist-get cold-proof :elapsed)
                warm-time (plist-get warm-proof :elapsed)
                speedup (if (and (numberp cold-time)
                                 (numberp warm-time)
                                 (> warm-time 0.0))
                            (/ cold-time warm-time)
                          0.0))
          (princ (format "VENDOR-NELC-CACHE source=%S\n" temp-source))
          (princ (format "VENDOR-NELC-CACHE cold key=%s mode=%S cache=%S compiled=%S time=%.6fs\n"
                         (plist-get cold-proof :key)
                         (plist-get cold-proof :mode)
                         (plist-get cold-proof :cache-status)
                         (plist-get cold-proof :compiled)
                         cold-time))
          (princ (format "VENDOR-NELC-CACHE warm key=%s mode=%S cache=%S compiled=%S time=%.6fs reads=%S\n"
                         (plist-get warm-proof :key)
                         (plist-get warm-proof :mode)
                         (plist-get warm-proof :cache-status)
                         (plist-get warm-proof :compiled)
                         warm-time
                         (plist-get warm-proof :source-read-paths)))
          (princ (format "VENDOR-NELC-CACHE invalidated key=%s mode=%S cache=%S compiled=%S reads=%S\n"
                         (plist-get invalidated-proof :key)
                         (plist-get invalidated-proof :mode)
                         (plist-get invalidated-proof :cache-status)
                         (plist-get invalidated-proof :compiled)
                         (plist-get invalidated-proof :source-read-paths)))
          (princ (format "VENDOR-NELC-CACHE speedup cold/warm=%.2fx\n" speedup))
          (should-not (plist-get source-proof :before-feature))
          (should-not (plist-get source-proof :before-fboundp))
          (should-not (plist-get cold-proof :before-feature))
          (should-not (plist-get cold-proof :before-fboundp))
          (should-not (plist-get warm-proof :before-feature))
          (should-not (plist-get warm-proof :before-fboundp))
          (should-not (plist-get invalidated-proof :before-feature))
          (should-not (plist-get invalidated-proof :before-fboundp))
          (should (equal (plist-get source-proof :tuple) '(t t "x")))
          (should (equal (plist-get cold-proof :tuple)
                         (plist-get source-proof :tuple)))
          (should (equal (plist-get warm-proof :tuple)
                         (plist-get source-proof :tuple)))
          (should (equal (plist-get invalidated-proof :tuple)
                         (plist-get source-proof :tuple)))
          (should (eq (plist-get cold-proof :mode) 'source))
          (should (eq (plist-get cold-proof :cache-status) 'miss))
          (should (plist-get cold-proof :compiled))
          (should (file-readable-p (plist-get cold-proof :artifact-path)))
          (should (file-readable-p (plist-get cold-proof :manifest-path)))
          (should (eq (plist-get warm-proof :mode) 'artifact))
          (should (eq (plist-get warm-proof :cache-status) 'hit))
          (should-not (plist-get warm-proof :compiled))
          (should-not (plist-get warm-proof :source-read-paths))
          (should (equal (plist-get warm-proof :key)
                         (plist-get cold-proof :key)))
          (should (eq (plist-get invalidated-proof :mode) 'source))
          (should (eq (plist-get invalidated-proof :cache-status) 'recompiled))
          (should (plist-get invalidated-proof :compiled))
          (should (not (equal (plist-get invalidated-proof :key)
                              (plist-get cold-proof :key))))
          (should (plist-get invalidated-proof :source-read-paths))
          (should (> cold-time 0.0))
          (should (> warm-time 0.0)))
      (when (file-exists-p temp-source)
        (delete-file temp-source)))))

(provide 'nemacs-vendor-cache-test)

;;; nemacs-vendor-cache-test.el ends here
