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
;;
;; The fragment-splitting and no-path-filename cases below were added after
;; the loopback-hosts fixture above shipped without either: every one of the
;; 8 loopback URLs happened to have a path and no fragment, so a `#url-target'
;; split gap (fragments stayed inside `url-filename', `url-target' always
;; nil) and a `url-filename' default-value gap (a bare-host URL with no path
;; at all got `"/"' here vs real Emacs's `""') both went unnoticed until a
;; second differential pass against Emacs 31.1 caught them.  Every value
;; below (including which ones do NOT split, and what the un-set `url-silent'/
;; `url-use-cookies'/`url-asynchronous' accessors default to) was read off a
;; real `emacs -Q --batch' run of `url-generic-parse-url', not assumed from
;; the RFC.

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

(defun nelisp-standalone-compat-url-parse-test--filename-and-target (url-string)
  "Return (FILENAME . TARGET) for URL-STRING."
  (let ((url (url-generic-parse-url url-string)))
    (cons (url-filename url) (url-target url))))

(ert-deftest nelisp-standalone-compat-url-parse-fragment-plain ()
  "A plain `#fragment' splits out of `url-filename' into `url-target'."
  (should (equal (nelisp-standalone-compat-url-parse-test--filename-and-target
                  "https://example.com/path#frag")
                 '("/path" . "frag"))))

(ert-deftest nelisp-standalone-compat-url-parse-fragment-after-query ()
  "A `#fragment' after a `?query' splits; the query stays in `url-filename'."
  (should (equal (nelisp-standalone-compat-url-parse-test--filename-and-target
                  "https://example.com/path?q=1#frag")
                 '("/path?q=1" . "frag"))))

(ert-deftest nelisp-standalone-compat-url-parse-fragment-bare-hash ()
  "A bare trailing `#' with no text after it is an EMPTY `url-target'
\(the empty string, not nil -- nil means \"no `#' at all\", verified
against Emacs 31.1: `(url-target (url-generic-parse-url \"http://h/p#\"))'
is \"\", not nil)."
  (should (equal (nelisp-standalone-compat-url-parse-test--filename-and-target
                  "https://example.com/path#")
                 '("/path" . ""))))

(ert-deftest nelisp-standalone-compat-url-parse-fragment-second-hash-not-resplit ()
  "Only the FIRST `#' splits; a second `#' stays inside `url-target' verbatim
\(verified against Emacs 31.1: it does not treat the second `#' as another
delimiter or reject it)."
  (should (equal (nelisp-standalone-compat-url-parse-test--filename-and-target
                  "https://example.com/path?a=1#b=2#c=3")
                 '("/path?a=1" . "b=2#c=3"))))

(ert-deftest nelisp-standalone-compat-url-parse-percent-encoded-hash-not-split ()
  "A `#' that was percent-encoded as `%23' must NOT be treated as the
fragment delimiter -- checked against Emacs 31.1 rather than assumed:
real `url-generic-parse-url' does not unescape before splitting, so
`%23' here stays inside `url-filename' and `url-target' is nil."
  (should (equal (nelisp-standalone-compat-url-parse-test--filename-and-target
                  "https://example.com/path?q=1%23notfrag")
                 '("/path?q=1%23notfrag" . nil))))

(ert-deftest nelisp-standalone-compat-url-parse-no-fragment-is-nil-target ()
  "No `#' at all leaves `url-target' nil (distinct from a bare `#', which
is the empty string -- see the bare-hash test above)."
  (should (equal (nelisp-standalone-compat-url-parse-test--filename-and-target
                  "https://example.com/path")
                 '("/path" . nil))))

(ert-deftest nelisp-standalone-compat-url-parse-no-path-filename-is-empty ()
  "A bare host with no path at all has `url-filename' \"\" (empty string),
not \"/\" -- verified against Emacs 31.1, which distinguishes this from an
explicit trailing `/' (`url-filename' of `https://example.com/' is \"/\")."
  (should (equal (url-filename (url-generic-parse-url "https://example.com"))
                 ""))
  (should (equal (url-filename (url-generic-parse-url "https://example.com/"))
                 "/")))

(ert-deftest nelisp-standalone-compat-url-parse-unset-accessors-match-defaults ()
  "`url-generic-parse-url' never sets `:silent'/`:use-cookies'/
`:asynchronous'; each carries its struct default, matching Emacs 31.1's
real `url' struct (`silent' nil, `use-cookies' t, `asynchronous' t) and
`url-attributes' stays nil (query strings are not parsed into it)."
  (let ((url (url-generic-parse-url "https://example.com/path?q=1#frag")))
    (should-not (url-silent url))
    (should (url-use-cookies url))
    (should (url-asynchronous url))
    (should-not (url-attributes url))))

(provide 'nelisp-standalone-compat-url-parse-test)

;;; nelisp-standalone-compat-url-parse-test.el ends here
