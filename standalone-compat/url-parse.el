;;; url-parse.el --- minimal url-parse for the standalone NeLisp binary  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; feat/standalone-agent-loadpath: same shape as `standalone-compat/subr-
;; x.el' -- discovered only once `-L' let more of the ~80 host-only test
;; files' load closure run far enough to reach it (absent from the
;; bare-`--load' "Measured facts" this segment started from; present
;; under `-L lisp -L ../nelisp-llm/lisp -L ../nelisp-photon/lisp').  The
;; standalone `require' searches `load-path' for a FILE named after the
;; feature and never finds one named "url-parse" (no `lisp/url-parse.el'
;; or similar), so `(require 'url-parse)' died `file-missing: url-parse'
;; before any of its actual names were ever looked up.  This directory is
;; on the standalone's default `load-path' (see `nelisp-standalone--
;; reader-default-load-path' in scripts/nelisp-standalone-build.el) but
;; NOT on any host `-L' path used by `make test' (`make -n test | grep -c
;; standalone-compat' is 0), so it never shadows anything for the host
;; Emacs suite.
;;
;; What actually needs to exist, checked two ways:
;;
;;   1. `(require 'url-parse)' (unconditional, no NOERROR) appears at the
;;      top level of THREE files under ../nelisp-agent/examples/ that the
;;      80 host-only test files `load' directly (not `require' --
;;      `semantic-render-test.el'/`bulk-eval-test.el'/
;;      `bulk-policy-eval-test.el' each `(load (expand-file-name
;;      "examples/....el" ...) nil t)'): `semantic-render-example.el',
;;      `evaluate-bulk-reader.el', `evaluate-semantic-render.el'.  Every
;;      `url-*' name they call (`url-generic-parse-url', `url-type',
;;      `url-host', `url-user', `url-password') is called ONLY from
;;      inside a `defun' body (`nl-agent-*-loopback-url-p', each guarding
;;      a live network request against loopback -- irrelevant to this
;;      segment, whose tests never make one), never at the file's own
;;      top level, so none of them can block `--load' from reaching `Ran
;;      N tests' -- only a later test that actually CALLS one of those
;;      functions could (out of scope: "whether the tests PASS or FAIL
;;      inside ERT is NOT this segment").  Neither nelisp-llm nor
;;      nelisp-photon requires `url-parse' anywhere in their own `lisp/'.
;;
;;   2. Probe on target/nelisp with `fboundp' (2026-09-19 22:52 build,
;;      before this file existed): `url-generic-parse-url'/`url-type'/
;;      `url-host'/`url-user'/`url-password'/`url-recreate-url'/
;;      `url-hexify-string'/`url-retrieve-synchronously' are all
;;      fboundp=nil -- genuinely absent, not merely un-`provide'd like
;;      `seq'/`simple' turned out to be.
;;
;; segment E (feat/standalone-behaviour-diffs) is that later segment: the
;; agent-host census showed `nl-agent-semantic-render-example--loopback-url-p'
;; (examples/semantic-render-example.el) actually CALLING
;; `url-generic-parse-url' / `url-type' / `url-host' / `url-user' /
;; `url-password', so a body is now required.  What follows is a minimal
;; RFC-3986-ish parser -- `scheme://[user[:password]@]host[:port][/path]'
;; only, no query/fragment splitting, no relative-URL merging, no percent
;; encoding -- differentially verified against Emacs 31.1's real
;; `url-parse.el' on the exact 8 URLs this corpus's test actually exercises
;; (test/semantic-render-test.el's
;; `nl-agent-semantic-render-example-validates-loopback-authority', covering
;; a bare host, a default port lookup, a bracketed IPv6 host with an
;; explicit port, two host-confusable domains, and two userinfo-confusable
;; attack URLs): identical `:type'/`:host'/`:user'/`:password'/`:port' on
;; every one, including that `url-host' keeps the IPv6 host's brackets (the
;; caller strips them itself) and that a userinfo component before an `@'
;; is bound to `url-user'/`url-password', never folded into the host.  See
;; test/nelisp-standalone-compat-url-parse-test.el for the fixture-by-
;; fixture comparison.
;;
;; Deliberately NOT covered (signals a wrong answer rather than pretending
;; one): query strings and fragments (kept inside `url-filename' rather
;; than split into `url-target'/parsed query params), relative URLs
;; (`url-expand-file-name'), percent-decoding of the parsed components,
;; `url-recreate-url', and any scheme beyond http/https/ftp for the
;; default-port table in `url-port'.

;;; Code:

(require 'cl-lib)

(cl-defstruct (url (:constructor url--make) (:copier nil))
  type user password host portspec filename target attributes fullness)

(defun url-port (url)
  "Return URL's explicit port, or the scheme's default (http 80, https 443,
ftp 21), or nil."
  (or (url-portspec url)
      (let ((type (url-type url)))
        (cond ((equal type "http") 80)
              ((equal type "https") 443)
              ((equal type "ftp") 21)
              (t nil)))))

(defun nelisp--url-last-index (str char)
  "Return the index of the last occurrence of CHAR in STR, or nil.
Used to split `user:password@host' authorities on the LAST `@' the way
real Emacs's URL parser does, so a `@' inside a percent-undecoded
userinfo component cannot be mistaken for the host separator."
  (let ((i (1- (length str))) (found nil))
    (while (and (>= i 0) (not found))
      (if (eq (aref str i) char) (setq found i) (setq i (1- i))))
    found))

(defun url-generic-parse-url (url)
  "Minimal RFC-3986-ish parse of URL: `scheme://[user[:pass]@]host[:port][/path]'.
Returns a `url' struct with the fields Emacs's real `url-parse.el' returns
for that same shape; see this file's header comment for what is not
covered."
  (let* ((fullness nil)
         (rest url)
         type user password host portspec filename)
    (when (string-match "\\`\\([a-zA-Z][a-zA-Z0-9+.-]*\\):" rest)
      (setq type (match-string 1 rest))
      (setq rest (substring rest (match-end 0))))
    (when (string-prefix-p "//" rest)
      (setq fullness t)
      (setq rest (substring rest 2))
      (let* ((auth-end (or (string-match "[/?#]" rest) (length rest)))
             (authority (substring rest 0 auth-end)))
        (setq rest (substring rest auth-end))
        (let ((at (nelisp--url-last-index authority ?@)))
          (when at
            (let ((userinfo (substring authority 0 at)))
              (setq authority (substring authority (1+ at)))
              (let ((colon (string-match ":" userinfo)))
                (if colon
                    (setq user (substring userinfo 0 colon)
                          password (substring userinfo (1+ colon)))
                  (setq user userinfo))))))
        ;; A bracketed IPv6 host keeps its brackets in `url-host' (matching
        ;; real Emacs); only the port after `]:' is stripped out of it.
        (if (string-prefix-p "[" authority)
            (let ((close (string-match "\\]" authority)))
              (if close
                  (progn
                    (setq host (substring authority 0 (1+ close)))
                    (let ((after (substring authority (1+ close))))
                      (when (string-prefix-p ":" after)
                        (setq portspec (string-to-number (substring after 1))))))
                (setq host authority)))
          (let ((colon (string-match ":" authority)))
            (if colon
                (progn
                  (setq host (substring authority 0 colon))
                  (setq portspec (string-to-number (substring authority (1+ colon)))))
              (setq host authority))))))
    (setq filename (if (> (length rest) 0) rest "/"))
    (url--make :type type :user user :password password :host host
               :portspec portspec :filename filename :target nil
               :attributes nil :fullness fullness)))

(provide 'url-parse)

;;; url-parse.el ends here
