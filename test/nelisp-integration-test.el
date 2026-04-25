;;; nelisp-integration-test.el --- ERT for nelisp-integration  -*- lexical-binding: t; -*-

;; Doc 32 v2 LOCKED 2026-04-25 §3.1 sub-phase 7.5.1 partial — +4 ERT
;; covering the Phase 7.5 wire-up helper hub:
;;
;;   1. keyword-family-map-bidirectional       — 5 family symbol round-trip
;;   2. cold-init-stub-signals-todo            — Phase 7.5.2 stub fires
;;   3. release-artifact-stub-signals-todo     — Phase 7.5.3 stub fires
;;   4. family-keyword-fallback-for-unknown    — unknown symbol intern fallback

(require 'ert)
(require 'cl-lib)
(require 'nelisp-integration)

;;;; 1. keyword-family-map round-trip

(ert-deftest nelisp-integration-keyword-family-map-bidirectional ()
  "Each entry in `nelisp-integration-keyword-family-map' must round-trip.
Both directions of translation should be inverse functions over the
five canonical family symbols introduced in T17."
  (dolist (entry nelisp-integration-keyword-family-map)
    (let ((bare (car entry))
          (kw   (cdr entry)))
      (should (eq (nelisp-integration-family-to-keyword bare) kw))
      (should (eq (nelisp-integration-family-from-keyword kw) bare))
      ;; double round-trip = identity
      (should (eq bare
                  (nelisp-integration-family-from-keyword
                   (nelisp-integration-family-to-keyword bare))))
      (should (eq kw
                  (nelisp-integration-family-to-keyword
                   (nelisp-integration-family-from-keyword kw)))))))

;;;; 2. cold-init coordinator stub

(ert-deftest nelisp-integration-cold-init-stub-signals-todo ()
  "The Phase 7.5.2 cold-init coordinator stub must signal
`nelisp-integration-todo' with the tag (cold-init-coordinator
\"Phase 7.5.2\").  The signal protects callers from silently calling
into a not-yet-implemented entry point."
  (let ((err (should-error
              (nelisp-integration--cold-init-coordinator-stub)
              :type 'nelisp-integration-todo)))
    ;; `should-error' returns (ERROR-SYMBOL . DATA), where DATA is the
    ;; *spliced* data list (signal flattens the list arg into the cdr).
    ;; So (cdr err) here equals (cold-init-coordinator "Phase 7.5.2").
    (should (equal (cdr err) '(cold-init-coordinator "Phase 7.5.2")))
    (should (eq (cadr err) 'cold-init-coordinator))
    (should (equal (caddr err) "Phase 7.5.2"))))

;;;; 3. release artifact verifier stub

(ert-deftest nelisp-integration-release-artifact-stub-signals-todo ()
  "The Phase 7.5.3 release artifact verifier stub must signal
`nelisp-integration-todo' with the tag (release-artifact-verifier
\"Phase 7.5.3\")."
  (let ((err (should-error
              (nelisp-integration--release-artifact-verifier-stub)
              :type 'nelisp-integration-todo)))
    (should (equal (cdr err)
                   '(release-artifact-verifier "Phase 7.5.3")))
    (should (eq (cadr err) 'release-artifact-verifier))
    (should (equal (caddr err) "Phase 7.5.3"))))

;;;; 4. fallback for unknown family

(ert-deftest nelisp-integration-family-keyword-fallback-for-unknown ()
  "Unknown family symbols must intern through `:'-prefix fallback.
This guards against new family names being silently rejected if the
canonical map has not yet been extended; the round-trip should still
hold for the synthesized keyword."
  (let* ((fresh (intern (format "nelisp-integration-test-fresh-%d"
                                (random 1000000))))
         (kw   (nelisp-integration-family-to-keyword fresh))
         (back (nelisp-integration-family-from-keyword kw)))
    (should (keywordp kw))
    (should (string= (symbol-name kw)
                     (concat ":" (symbol-name fresh))))
    (should (eq back fresh))
    ;; map must NOT have been mutated by the fallback path
    (should-not (assq fresh nelisp-integration-keyword-family-map))
    (should-not (rassq kw nelisp-integration-keyword-family-map))))

(provide 'nelisp-integration-test)
;;; nelisp-integration-test.el ends here
