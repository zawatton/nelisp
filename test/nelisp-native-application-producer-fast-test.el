;;; nelisp-native-application-producer-fast-test.el --- install plan reuse -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; Structural checks on the producer's install path:
;; - install may reuse the caller's preflight plan instead of computing it again;
;; - after helper setup it proves the plan's inputs unchanged (manifest and names
;;   bytes, every pinned module's bytes, the native-row baseline digest) instead
;;   of running a full second preflight, and the recheck runs after the helper.
;; Set NELISP_PRODUCER_UNDER_TEST to point at another producer (negative control).
;;; Code:
(require 'ert)
(defconst nelisp-producer-fast-test--source
  (or (getenv "NELISP_PRODUCER_UNDER_TEST")
      (expand-file-name "../tools/nelisp-native-application-producer.el"
                        (file-name-directory (or load-file-name buffer-file-name)))))

(defun nelisp-producer-fast-test--text ()
  (with-temp-buffer
    (insert-file-contents nelisp-producer-fast-test--source)
    (buffer-string)))

(defun nelisp-producer-fast-test--install-text ()
  (let ((s (nelisp-producer-fast-test--text)))
    (substring s (string-match "^(defun nelisp-native-application-producer-install" s))))

(ert-deftest nelisp-producer-install-accepts-supplied-plan ()
  (let ((text (nelisp-producer-fast-test--install-text)))
    (should (string-match-p "&optional expected-image-sha256 mode supplied-plan" text))
    (should (string-match-p "(plan (or supplied-plan" text))))

(ert-deftest nelisp-producer-install-rechecks-inputs-after-helper ()
  (let ((text (nelisp-producer-fast-test--install-text)))
    (should (string-match-p "(nelisp-native-application-producer--recheck-inputs\n *plan expected-manifest-sha256 expected-names-sha256)" text))
    (should-not (string-match-p "(current-plan (nelisp-native-application-producer-preflight" text))
    (should (< (string-match "initialize-fx-helper" text)
               (string-match "--recheck-inputs" text)))))

(ert-deftest nelisp-producer-recheck-covers-every-plan-input ()
  (let* ((s (nelisp-producer-fast-test--text))
         (start (string-match "^(defun nelisp-native-application-producer--recheck-inputs" s))
         (body (and start (substring s start (string-match "^(defun " s (1+ start))))))
    (should body)
    (should (string-match-p "expected-manifest-sha256" body))
    (should (string-match-p "expected-names-sha256" body))
    (should (string-match-p "(plist-get manifest :modules)" body))
    (should (string-match-p "(plist-get pin :sha256)" body))
    (should (string-match-p ":baseline-sha256" body))
    ;; The baseline is proven either by recomputing the rows digest or by the
    ;; pinned-digest shortcut, whose fallback recomputes it.
    (should (string-match-p
             "(nelisp-cc-fxir-native-all-rows)\\|--baseline-matches-p" body))))

(provide 'nelisp-native-application-producer-fast-test)
;;; nelisp-native-application-producer-fast-test.el ends here
