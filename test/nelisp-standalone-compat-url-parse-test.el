;;; nelisp-standalone-compat-url-parse-test.el --- url-parse compat tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `standalone-compat/url-parse.el' supplies `url-generic-parse-url' and its
;; accessors for the standalone binary, which has no host Emacs underneath it
;; to provide the real `url-parse.el'.  `standalone-compat/' is not on this
;; host suite's `-L' path (see the Makefile's `test' target), so plain
;; `(require 'url-parse)' resolves to Emacs's own real library here and to
;; the standalone-compat shim on `target/nelisp' -- these assertions are a
;; differential fixture verified against Emacs 31.1's real `url-parse.el',
;; covering exactly the shapes
;; ../nelisp-agent/examples/semantic-render-example.el's
;; `nl-agent-semantic-render-example--loopback-url-p' calls: a bare loopback
;; host, a scheme's default port, a bracketed IPv6 host with an explicit
;; port, two host-confusable domains, and two userinfo-confusable attack
;; URLs.  Passing here on the host suite is a sanity check that the fixture
;; itself is right; passing under `target/nelisp' (run this file directly,
;; e.g. via `tools/ai/nelisp-ai.sh test-one') is the actual regression cover
;; for the shim.

;;; Code:

(require 'ert)
(require 'url-parse)

(defun nelisp-standalone-compat-url-parse-test--fields (url-string)
  "Return (TYPE HOST USER PASSWORD PORT) for URL-STRING."
  (let ((url (url-generic-parse-url url-string)))
    (list (url-type url) (url-host url) (url-user url)
          (url-password url) (url-port url))))

(ert-deftest nelisp-standalone-compat-url-parse-loopback-hosts ()
  "Loopback authorities the semantic-render example must recognize."
  (should (equal (nelisp-standalone-compat-url-parse-test--fields
                  "http://127.0.0.1:11434/v1")
                 '("http" "127.0.0.1" nil nil 11434)))
  (should (equal (nelisp-standalone-compat-url-parse-test--fields
                  "https://localhost/v1")
                 '("https" "localhost" nil nil 443)))
  (should (equal (nelisp-standalone-compat-url-parse-test--fields
                  "http://[::1]:11434/v1")
                 '("http" "[::1]" nil nil 11434))))

(ert-deftest nelisp-standalone-compat-url-parse-confusable-hosts ()
  "Domains that merely contain a loopback name must not parse as loopback."
  (should (equal (nelisp-standalone-compat-url-parse-test--fields
                  "http://localhost.evil.test/v1")
                 '("http" "localhost.evil.test" nil nil 80)))
  (should (equal (nelisp-standalone-compat-url-parse-test--fields
                  "http://127.0.0.1.evil.test/v1")
                 '("http" "127.0.0.1.evil.test" nil nil 80)))
  (should (equal (nelisp-standalone-compat-url-parse-test--fields
                  "http://example.com/v1")
                 '("http" "example.com" nil nil 80))))

(ert-deftest nelisp-standalone-compat-url-parse-userinfo-confusable-hosts ()
  "A userinfo component before `@' must bind to :user, never to the host."
  (should (equal (nelisp-standalone-compat-url-parse-test--fields
                  "http://127.0.0.1@evil.test/v1")
                 '("http" "evil.test" "127.0.0.1" nil 80)))
  (should (equal (nelisp-standalone-compat-url-parse-test--fields
                  "http://user@localhost/v1")
                 '("http" "localhost" "user" nil 80))))

(provide 'nelisp-standalone-compat-url-parse-test)

;;; nelisp-standalone-compat-url-parse-test.el ends here
