;;; nelisp-generated-source-parse.el --- generated Elisp sources must parse -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `scripts/nelisp-standalone-build.el' builds several Emacs Lisp programs as
;; concatenated string literals and embeds them in the standalone binary.
;; Their parens are inside strings, so `parens-check' -- which reads the .el
;; file, not the text it produces -- cannot see them.
;;
;; On 2026-08-19 a rewrite of the generated `string-match' dropped one closing
;; paren, leaving `(unless (fboundp (quote string-match)) ...' open.  The 44
;; forms after it, the artifact command dispatch among them, became its body.
;; `string-match' is fbound natively, so the body never ran: `nelisp
;; compile-elisp-artifact' silently did nothing and answered 0.  It took a
;; two-day bisect to find, because nothing in the tree ever parsed that text.
;;
;; So: parse it.  Read forms until the source is consumed; whatever is left
;; over must be whitespace.  A dropped paren leaves the rest of the program
;; sitting unread inside one unterminated form, which is exactly what this
;; measures.
;;
;; The generators are listed by name rather than discovered.  Adding one is a
;; line here, and having to add it is the point at which somebody looks.
;;
;; Run: make generated-source-parse

;;; Code:

(require 'nelisp-standalone-build)

(defconst nelisp-generated-source-parse--generators
  '(nelisp-standalone--artifact-command-runtime-src
    nelisp-standalone--artifact-command-src
    nelisp-standalone--artifact-command-cache-src
    nelisp-standalone--artifact-source-command-cache-src
    nelisp-standalone--reader-repl-prelude-source)
  "Functions returning Emacs Lisp source as a string.")

(defun nelisp-generated-source-parse--leftover (source)
  "Return the tail of SOURCE that no top-level form consumed.
Reads forms from the front until reading fails or SOURCE runs out."
  (let ((pos 0)
        (done nil))
    (while (not done)
      (condition-case nil
          (if (>= pos (length source))
              (setq done t)
            (setq pos (cdr (read-from-string source pos))))
        (error (setq done t))))
    (substring source (min pos (length source)))))

(defun nelisp-generated-source-parse--findings ()
  "Return one finding per generator whose source does not fully parse."
  (let ((findings nil))
    (dolist (generator nelisp-generated-source-parse--generators)
      (if (not (fboundp generator))
          (push (format "%s: not fbound -- the list in %s is stale"
                        generator "tools/nelisp-generated-source-parse.el")
                findings)
        (let ((leftover (nelisp-generated-source-parse--leftover
                         (funcall generator))))
          (unless (string-match-p "\\`[ \t\n]*\\'" leftover)
            (push (format "%s: %d character(s) never parsed, starting: %s"
                          generator (length leftover)
                          (substring leftover 0 (min 70 (length leftover))))
                  findings)))))
    (nreverse findings)))

(defun nelisp-generated-source-parse-run ()
  "Print the findings and exit non-zero when there are any."
  (let ((findings (nelisp-generated-source-parse--findings)))
    (dolist (finding findings)
      (princ (format "generated-source-parse: %s\n" finding)))
    (princ (format "GATE-COUNT checked=%d findings=%d\n"
                   (length nelisp-generated-source-parse--generators)
                   (length findings)))
    (when findings (kill-emacs 1))))

(provide 'nelisp-generated-source-parse)

;;; nelisp-generated-source-parse.el ends here
