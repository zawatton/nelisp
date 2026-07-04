;;; emacs-process-coding-test.el --- ERT for process-coding-system (Doc 06 C4)  -*- lexical-binding: t; -*-

;;; Commentary:

;; The decode-on-filter / encode-on-send conversion helpers are defined ungated
;; in emacs-process-events.el so they run under host Emacs, where they delegate
;; to the real `decode-coding-string' / `encode-coding-string' — making these
;; tests genuinely host-matching.  The standalone recv/send wiring that calls
;; them is exercised on the binary.

;;; Code:

(require 'ert)
(require 'emacs-process-events)

(ert-deftest emacs-process-coding-test/decoder-encoder-extraction ()
  "DECODE/ENCODE are pulled from a cons; a bare symbol means both directions."
  (should (eq 'utf-8 (emacs-process-events--coding-decoder '(utf-8 . latin-1))))
  (should (eq 'latin-1 (emacs-process-events--coding-encoder '(utf-8 . latin-1))))
  (should (eq 'utf-8 (emacs-process-events--coding-decoder 'utf-8)))
  (should (eq 'utf-8 (emacs-process-events--coding-encoder 'utf-8)))
  (should (null (emacs-process-events--coding-decoder nil))))

(ert-deftest emacs-process-coding-test/no-conversion-passthrough ()
  "No-conversion coding systems pass the bytes through unchanged."
  (dolist (cs '(nil binary no-conversion raw-text))
    (should (emacs-process-events--no-conversion-p cs))
    (should (equal "\xe3\x81\x82"
                   (emacs-process-events--decode-output "\xe3\x81\x82" cs)))
    (should (equal "\xe3\x81\x82"
                   (emacs-process-events--encode-input "\xe3\x81\x82" cs))))
  (should-not (emacs-process-events--no-conversion-p 'utf-8)))

(ert-deftest emacs-process-coding-test/utf8-roundtrip-matches-host ()
  "decode-output / encode-input delegate to the real coding machinery, so a
UTF-8 round-trip reproduces the original multibyte string (host-matching)."
  (let* ((text "café—日本語")
         (bytes (encode-coding-string text 'utf-8)))
    ;; Raw UTF-8 bytes decode back to the multibyte string.
    (should (equal text (emacs-process-events--decode-output bytes 'utf-8)))
    ;; And encoding the multibyte string yields the same raw bytes.
    (should (equal bytes (emacs-process-events--encode-input text 'utf-8)))
    ;; The cons form selects the right direction.
    (should (equal text
                   (emacs-process-events--decode-output bytes '(utf-8 . utf-8))))
    (should (equal bytes
                   (emacs-process-events--encode-input text '(utf-8 . utf-8))))))

(ert-deftest emacs-process-coding-test/non-string-is-safe ()
  "Non-string CHUNK / STRING is returned unchanged (defensive)."
  (should (equal 42 (emacs-process-events--decode-output 42 'utf-8)))
  (should (equal nil (emacs-process-events--encode-input nil 'utf-8))))

(provide 'emacs-process-coding-test)
;;; emacs-process-coding-test.el ends here
