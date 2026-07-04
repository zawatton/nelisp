;;; standalone-source-normalize-test.el --- tests for standalone source rewrites  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'standalone-source-normalize)

(ert-deftest standalone-source-normalize-test/expands-setq-local ()
  (should
   (equal
    (standalone-source-normalize-form
     '(setq-local foo 1 bar (+ foo 2)))
    '(progn
       (set (make-local-variable 'foo) 1)
       (set (make-local-variable 'bar) (+ foo 2))))))

(ert-deftest standalone-source-normalize-test/preserves-quoted-data ()
  (should
   (equal
    (standalone-source-normalize-form
     '(quote (setq-local foo 1)))
    '(quote (setq-local foo 1)))))

(ert-deftest standalone-source-normalize-test/normalizes-genuine-backquote-data ()
  "A real reader-emitted `(backquote DATUM)' still normalizes its datum."
  (should
   (equal
    (standalone-source-normalize-form
     (read "`(a ,b ,@c)"))
    (read "`(a ,b ,@c)"))))

(ert-deftest standalone-source-normalize-test/preserves-defmacro-named-backquote ()
  "Doc 33 item 225 regression: a definition literally named `backquote'
must not be mistaken for reader-emitted backquote syntax and truncated
to its name + arglist.  `(defmacro backquote (form) DOC BODY)' has
`(backquote (form) DOC BODY)' as its cdr -- same head symbol as genuine
`(backquote DATUM)' syntax, but a 4-element list, not 2.  The full
docstring and body must survive normalization."
  (should
   (equal
    (standalone-source-normalize-form
     '(defmacro backquote (form)
        "Polyfill: expand FORM under backquote semantics."
        (emacs-backquote--expand form)))
    '(defmacro backquote (form)
       "Polyfill: expand FORM under backquote semantics."
       (emacs-backquote--expand form)))))

(ert-deftest standalone-source-normalize-test/preserves-defun-named-backquote-in-guard ()
  "Same collision, wrapped in the `unless (fboundp ...)' polyfill-guard
shape used across src/*.el; the guard form's cdr also is not itself a
top-level defmacro, so this exercises the generic-cons recursion path
that walks down into the guarded definition."
  (should
   (equal
    (standalone-source-normalize-form
     '(unless (fboundp 'backquote)
        (defmacro backquote (form)
          "doc"
          (emacs-backquote--expand form))))
    '(unless (fboundp 'backquote)
       (defmacro backquote (form)
         "doc"
         (emacs-backquote--expand form))))))

(ert-deftest standalone-source-normalize-test/rewrites-inside-defun-body ()
  (should
   (equal
    (standalone-source-normalize-form
     '(defun demo ()
        (setq-local foo 1)
        foo))
    '(defun demo ()
       (set (make-local-variable 'foo) 1)
       foo))))

(ert-deftest standalone-source-normalize-test/leaves-malformed-setq-local ()
  (should
   (equal
    (standalone-source-normalize-form
     '(setq-local foo))
    '(setq-local foo))))

(ert-deftest standalone-source-normalize-test/drops-top-level-declare-function ()
  (should
   (null
    (standalone-source-normalize-top-level-forms
     '(declare-function org-list-struct "org-list" nil)))))

(ert-deftest standalone-source-normalize-test/preserves-nested-declare-function ()
  (should
   (equal
    (standalone-source-normalize-form
     '(progn (declare-function demo "demo") done))
    '(progn (declare-function demo "demo") done))))

(ert-deftest standalone-source-normalize-test/drops-top-level-eval-when-compile ()
  (should
   (null
    (standalone-source-normalize-top-level-forms
     '(eval-when-compile (require 'gnus-sum))))))

(ert-deftest standalone-source-normalize-test/drops-top-level-org-version-assertion ()
  (should
   (null
    (standalone-source-normalize-top-level-forms
     '(org-assert-version)))))

(ert-deftest standalone-source-normalize-test/preserves-top-level-require-by-default ()
  (should
   (equal
    (standalone-source-normalize-top-level-forms
     '(require 'org-macs))
    '((require 'org-macs)))))

(ert-deftest standalone-source-normalize-test/drops-file-scoped-top-level-require ()
  (let ((standalone-source-normalize-current-file "org-element-ast.el"))
    (should
     (null
      (standalone-source-normalize-top-level-forms
       '(require 'org-macs)))))
  (let ((standalone-source-normalize-current-file "org.el"))
    (should
     (equal
      (standalone-source-normalize-top-level-forms
       '(require 'org-macs))
      '((require 'org-macs))))))

(ert-deftest standalone-source-normalize-test/drops-org-list-top-level-require ()
  (let ((standalone-source-normalize-current-file "org-list.el"))
    (dolist (feature '(org-macs cl-lib org-compat org-fold-core org-footnote))
      (should
       (null
        (standalone-source-normalize-top-level-forms
         `(require ',feature)))))))

(ert-deftest standalone-source-normalize-test/drops-org-entities-top-level-require ()
  (let ((standalone-source-normalize-current-file "org-entities.el"))
    (dolist (feature '(org-macs seq))
      (should
       (null
        (standalone-source-normalize-top-level-forms
         `(require ',feature)))))))

(ert-deftest standalone-source-normalize-test/drops-help-macro-top-level-require ()
  (let ((standalone-source-normalize-current-file "help-macro.el"))
    (should
     (null
      (standalone-source-normalize-top-level-forms
       '(require 'backquote))))))

(ert-deftest standalone-source-normalize-test/drops-org-macro-top-level-require ()
  (let ((standalone-source-normalize-current-file "org-macro.el"))
    (dolist (feature '(org-macs cl-lib org-compat))
      (should
       (null
        (standalone-source-normalize-top-level-forms
         `(require ',feature)))))))

(ert-deftest standalone-source-normalize-test/drops-ob-eval-top-level-require ()
  (let ((standalone-source-normalize-current-file "ob-eval.el"))
    (should
     (null
      (standalone-source-normalize-top-level-forms
       '(require 'org-macs))))))

(ert-deftest standalone-source-normalize-test/drops-org-faces-top-level-require ()
  (let ((standalone-source-normalize-current-file "org-faces.el"))
    (should
     (null
      (standalone-source-normalize-top-level-forms
       '(require 'org-macs))))))

(ert-deftest standalone-source-normalize-test/drops-oc-bibtex-top-level-requires ()
  (let ((standalone-source-normalize-current-file "oc-bibtex.el"))
    (dolist (feature '(org-macs oc))
      (should
       (null
        (standalone-source-normalize-top-level-forms
         `(require ',feature)))))))

(ert-deftest standalone-source-normalize-test/drops-org-citation-backend-top-level-requires ()
  (dolist (entry '(("oc-natbib.el" org-macs oc)
                   ("oc-biblatex.el" org-macs map oc)))
    (let ((standalone-source-normalize-current-file (car entry)))
      (dolist (feature (cdr entry))
        (should
         (null
          (standalone-source-normalize-top-level-forms
           `(require ',feature))))))))

(ert-deftest standalone-source-normalize-test/drops-org-inlinetask-top-level-requires ()
  (let ((standalone-source-normalize-current-file "org-inlinetask.el"))
    (dolist (feature '(org-macs org))
      (should
       (null
        (standalone-source-normalize-top-level-forms
         `(require ',feature)))))))

(ert-deftest standalone-source-normalize-test/drops-ol-doi-top-level-requires ()
  (let ((standalone-source-normalize-current-file "ol-doi.el"))
    (dolist (feature '(org-macs ol))
      (should
       (null
        (standalone-source-normalize-top-level-forms
         `(require ',feature)))))))

(ert-deftest standalone-source-normalize-test/drops-ol-mhe-top-level-requires ()
  (let ((standalone-source-normalize-current-file "ol-mhe.el"))
    (dolist (feature '(org-macs ol))
      (should
       (null
        (standalone-source-normalize-top-level-forms
         `(require ',feature)))))))

(ert-deftest standalone-source-normalize-test/drops-ol-w3m-top-level-requires ()
  (let ((standalone-source-normalize-current-file "ol-w3m.el"))
    (dolist (feature '(org-macs ol))
      (should
       (null
        (standalone-source-normalize-top-level-forms
         `(require ',feature)))))))

(ert-deftest standalone-source-normalize-test/drops-ol-irc-top-level-requires ()
  (let ((standalone-source-normalize-current-file "ol-irc.el"))
    (dolist (feature '(org-macs ol))
      (should
       (null
        (standalone-source-normalize-top-level-forms
         `(require ',feature)))))))

(ert-deftest standalone-source-normalize-test/drops-org-tempo-top-level-requires ()
  (let ((standalone-source-normalize-current-file "org-tempo.el"))
    (dolist (feature '(org-macs tempo cl-lib org))
      (should
       (null
        (standalone-source-normalize-top-level-forms
         `(require ',feature)))))))

(ert-deftest standalone-source-normalize-test/drops-inline-top-level-requires ()
  (let ((standalone-source-normalize-current-file "inline.el"))
    (should
     (null
      (standalone-source-normalize-top-level-forms
       '(require 'macroexp))))))

(ert-deftest standalone-source-normalize-test/drops-sh-script-let-alist-require ()
  (let ((standalone-source-normalize-current-file "sh-script.el"))
    (should
     (null
      (standalone-source-normalize-top-level-forms
       '(require 'let-alist))))))

(ert-deftest standalone-source-normalize-test/drops-easymenu-load-only-forms ()
  (let ((standalone-source-normalize-current-file "easymenu.el"))
    (dolist (form '((defmacro easy-menu-define (symbol maps doc menu)
                      (list symbol maps doc menu))
                    (defun easy-menu-create-menu (menu-name menu-items)
                      (list menu-name menu-items))
                    (defvar easy-menu-converted-items-table
                      (make-hash-table :test 'equal))
                    (provide 'easymenu)))
      (should
       (null
        (standalone-source-normalize-top-level-forms form))))))

(ert-deftest standalone-source-normalize-test/drops-let-alist-load-only-forms ()
  (let ((standalone-source-normalize-current-file "let-alist.el"))
    (dolist (form '((defun let-alist--deep-dot-search (data)
                      data)
                    (defun let-alist--access-sexp (symbol variable)
                      (list symbol variable))
                    (defmacro let-alist (alist &rest body)
                      `(let ((alist ,alist)) ,@body))
                    (provide 'let-alist)))
      (should
       (null
        (standalone-source-normalize-top-level-forms form))))))

(ert-deftest standalone-source-normalize-test/drops-radix-tree-load-only-forms ()
  (let ((standalone-source-normalize-current-file "radix-tree.el"))
    (dolist (form '((defconst radix-tree-empty nil
                      "The empty radix-tree.")
                    (defun radix-tree-insert (tree key val)
                      (list tree key val))
                    (defun radix-tree-lookup (tree key)
                      (list tree key))
                    (defun radix-tree-prefixes (tree string)
                      (list tree string))
                    (provide 'radix-tree)))
      (should
       (null
        (standalone-source-normalize-top-level-forms form))))))

(ert-deftest standalone-source-normalize-test/drops-text-property-search-load-only-forms ()
  (let ((standalone-source-normalize-current-file "text-property-search.el"))
    (dolist (form '((eval-when-compile (require 'cl-lib))
                    (cl-defstruct (prop-match) beginning end value)
                    (defun text-property-search-forward (property &optional value)
                      (list property value))
                    (defun text-property-search-backward (property &optional value)
                      (list property value))
                    (provide 'text-property-search)))
      (should
       (null
        (standalone-source-normalize-top-level-forms form))))))

(ert-deftest standalone-source-normalize-test/drops-thunk-load-only-forms ()
  (let ((standalone-source-normalize-current-file "thunk.el"))
    (dolist (form '((require 'cl-lib)
                    (defmacro thunk-delay (&rest body)
                      `(lambda (&optional check) (if check t ,@body)))
                    (defun thunk-force (delayed)
                      (funcall delayed))
                    (defun thunk-evaluated-p (delayed)
                      (funcall delayed t))
                    (provide 'thunk)))
      (should
       (null
        (standalone-source-normalize-top-level-forms form))))))

(ert-deftest standalone-source-normalize-test/drops-rmc-load-only-forms ()
  (let ((standalone-source-normalize-current-file "rmc.el"))
    (dolist (form '((defun read-multiple-choice (prompt choices)
                      (list prompt choices))
                    (defun rmc--add-key-description (elem)
                      elem)
                    (provide 'rmc)))
      (should
       (null
        (standalone-source-normalize-top-level-forms form))))))

(ert-deftest standalone-source-normalize-test/drops-generate-lisp-file-load-only-forms ()
  (let ((standalone-source-normalize-current-file "generate-lisp-file.el"))
    (dolist (form '((cl-defun generate-lisp-file-heading
                       (file generator &key title)
                      (list file generator title))
                    (cl-defun generate-lisp-file-trailer
                       (file &key version)
                      (list file version))
                    (provide 'generate-lisp-file)))
      (should
       (null
        (standalone-source-normalize-top-level-forms form))))))

(ert-deftest standalone-source-normalize-test/drops-obarray-load-only-forms ()
  (let ((standalone-source-normalize-current-file "obarray.el"))
    (dolist (form '((defconst obarray-default-size 4)
                    (make-obsolete-variable 'obarray-default-size
                                            "obsolete" "30.1")
                    (defun obarray-size (_ob) 4)
                    (defun obarray-get (ob name)
                      (intern-soft name ob))
                    (provide 'obarray)))
      (should
       (null
        (standalone-source-normalize-top-level-forms form))))))

(ert-deftest standalone-source-normalize-test/drops-soundex-load-only-forms ()
  (let ((standalone-source-normalize-current-file "soundex.el"))
    (dolist (form '((defconst soundex-alist nil)
                    (defun soundex (word)
                      word)
                    (provide 'soundex)))
      (should
       (null
        (standalone-source-normalize-top-level-forms form))))))

(ert-deftest standalone-source-normalize-test/drops-cursor-sensor-load-only-forms ()
  (let ((standalone-source-normalize-current-file "cursor-sensor.el"))
    (dolist (form '((defvar cursor-sensor-inhibit nil)
                    (defun cursor-sensor-tangible-pos
                        (curpos window &optional second-chance)
                      (list curpos window second-chance))
                    (define-minor-mode cursor-sensor-mode
                      "Handle the `cursor-sensor-functions' text property."
                      :global nil)
                    (provide 'cursor-sensor)))
      (should
       (null
        (standalone-source-normalize-top-level-forms form))))))

(ert-deftest standalone-source-normalize-test/drops-indent-aux-load-only-forms ()
  (let ((standalone-source-normalize-current-file "indent-aux.el"))
    (dolist (form '((defun kill-ring-deindent-buffer-substring-function
                        (beg end delete)
                      (list beg end delete))
                    (define-minor-mode kill-ring-deindent-mode
                      "Toggle removal of indentation from text saved to the kill ring."
                      :global 't)
                    (provide 'indent-aux)))
      (should
       (null
        (standalone-source-normalize-top-level-forms form))))))

(ert-deftest standalone-source-normalize-test/drops-display-fill-column-indicator-load-only-forms ()
  (let ((standalone-source-normalize-current-file
         "display-fill-column-indicator.el"))
    (dolist (form '((defgroup display-fill-column-indicator nil
                      "Display a fill column indicator in the buffer."
                      :group 'convenience)
                    (define-minor-mode display-fill-column-indicator-mode
                      "Toggle display of `fill-column' indicator."
                      :lighter nil)
                    (defun display-fill-column-indicator--turn-on ()
                      (display-fill-column-indicator-mode))
                    (provide 'display-fill-column-indicator)))
      (should
       (null
        (standalone-source-normalize-top-level-forms form))))))

(ert-deftest standalone-source-normalize-test/drops-thingatpt-load-only-forms ()
  (let ((standalone-source-normalize-current-file "thingatpt.el"))
    (dolist (form '((defvar thing-at-point-provider-alist nil)
                    (defun forward-thing (thing &optional n)
                      (list thing n))
                    (defun thing-at-point (thing &optional no-properties)
                      (list thing no-properties))
                    (provide 'thingatpt)))
      (should
       (null
        (standalone-source-normalize-top-level-forms form))))))

(ert-deftest standalone-source-normalize-test/drops-time-date-load-only-forms ()
  (let ((standalone-source-normalize-current-file "time-date.el"))
    (dolist (form '((defmacro with-decoded-time-value (varlist &rest body)
                      (cons 'let (cons varlist body)))
                    (make-obsolete 'with-decoded-time-value nil "25.1")
                    (defvar seconds-to-string nil)
                    (defun date-leap-year-p (year)
                      (or (and (zerop (% year 4))
                               (not (zerop (% year 100))))
                          (zerop (% year 400))))
                    (defun date-days-in-month (year month)
                      (list year month))
                    (cl-defun make-decoded-time (&key second)
                      second)
                    (provide 'time-date)))
      (should
       (null
        (standalone-source-normalize-top-level-forms form))))))

(ert-deftest standalone-source-normalize-test/drops-iso8601-load-only-forms ()
  (let ((standalone-source-normalize-current-file "iso8601.el"))
    (dolist (form '((require 'time-date)
                    (require 'cl-lib)
                    (defconst iso8601--date-match "date")
                    (defun iso8601-parse (string &optional form)
                      (list string form))
                    (defun iso8601-valid-p (string)
                      (stringp string))
                    (provide 'iso8601)))
      (should
       (null
        (standalone-source-normalize-top-level-forms form))))))

(ert-deftest standalone-source-normalize-test/drops-parse-time-load-only-forms ()
  (let ((standalone-source-normalize-current-file "parse-time.el"))
    (dolist (form '((require 'cl-lib)
                    (require 'iso8601)
                    (defvar parse-time-months nil)
                    (defvar parse-time-weekdays nil)
                    (defvar parse-time-zoneinfo nil)
                    (defsubst parse-time-string-chars (char)
                      char)
                    (defun parse-time-tokenize (string)
                      (list string))
                    (defun parse-time-string (string &optional form)
                      (list string form))
                    (defalias 'parse-iso8601-time-string #'parse-time-string)
                    (provide 'parse-time)))
      (should
       (null
        (standalone-source-normalize-top-level-forms form))))))

(ert-deftest standalone-source-normalize-test/drops-unicode-table-load-only-forms ()
  (dolist (file '("uni-lowercase.el"
                  "uni-mirrored.el"
                  "uni-special-lowercase.el"
                  "uni-special-titlecase.el"
                  "uni-special-uppercase.el"
                  "uni-titlecase.el"
                  "uni-uppercase.el"))
    (let ((standalone-source-normalize-current-file file))
      (dolist (form '((defconst unicode-property-table '(#x41 #x61))
                      (defvar unicode-property-table nil)
                      (provide 'uni-lowercase)))
        (should
         (null
          (standalone-source-normalize-top-level-forms form)))))))

(ert-deftest standalone-source-normalize-test/drops-small-ui-utility-load-only-forms ()
  (dolist (entry '(("tabify.el"
                   ((defvar tabify-regexp " [ \t]+")
                    (defun untabify (start end &optional arg)
                      (list start end arg))
                    (defun tabify (start end &optional arg)
                      (list start end arg))
                    (provide 'tabify)))
                  ("rot13.el"
                   ((defconst rot13-translate-table nil)
                    (defun rot13-string (string)
                      string)
                    (defun rot13-region (start end)
                      (list start end))
                    (provide 'rot13)))
                  ("underline.el"
                   ((defun underline-region (start end)
                      (list start end))
                    (defun ununderline-region (start end)
                      (list start end))
                    (provide 'underline)))
                  ("widget.el"
                   ((defmacro define-widget-keywords (&rest keys)
                      keys)
                    (defun define-widget (name class doc &rest args)
                      (list name class doc args))
                    (provide 'widget)))
                  ("dos-vars.el"
                   ((defgroup dos-fns nil
                      "MS-DOS specific functions."
                      :group 'environment)
                    (defcustom msdos-shells '("command.com")
                      "DOS shells."
                      :type '(repeat string))
                    (provide 'dos-vars)))
                  ("mb-depth.el"
                   ((defcustom minibuffer-depth-indicator-function nil
                      "Minibuffer depth indicator."
                      :type '(choice (const nil) (function))
                      :group 'minibuffer)
                    (defface minibuffer-depth-indicator '((t :inherit highlight))
                      "Minibuffer depth face."
                      :group 'minibuffer)
                    (defvar minibuffer-depth-overlay)
                    (defun minibuffer-depth-setup () nil)
                    (define-minor-mode minibuffer-depth-indicate-mode
                      "Toggle Minibuffer Depth Indication mode."
                      :global t)
                    (provide 'mb-depth)))
                  ("ietf-drums.el"
                   ((defvar ietf-drums-no-ws-ctl-token "x")
                    (defun ietf-drums-parse-address (string &optional decode)
                      (list string decode))
                    (defun ietf-drums-parse-addresses (string &optional rawp)
                      (list string rawp))
                    (provide 'ietf-drums)))
                  ("rfc2045.el"
                   ((require 'ietf-drums)
                    (defun rfc2045-encode-string (param value)
                      (list param value))
                    (provide 'rfc2045)))
                  ("hmac-def.el"
                   ((defmacro define-hmac-function (name h b l &optional bit)
                      (list name h b l bit))
                    (provide 'hmac-def)))
                  ("hmac-md5.el"
                   ((require 'hmac-def)
                    (defun md5-binary (string) string)
                    (define-hmac-function hmac-md5 md5-binary 64 16)
                    (define-hmac-function hmac-md5-96 md5-binary 64 16 96)
                    (provide 'hmac-md5)))
                  ("rfc2104.el"
                   ((defconst rfc2104-ipad ?x)
                    (defun rfc2104-hash (hash block-length hash-length key text)
                      (list hash block-length hash-length key text))
                    (provide 'rfc2104)))
                  ("md4.el"
                   ((defvar md4-buffer nil)
                    (defun md4 (in n)
                      (list in n))
                    (provide 'md4)))
                  ("compat.el"
                   ((defmacro compat-function (name args &rest body)
                      (list name args body))
                    (defmacro compat-call (function &rest args)
                      (list function args))
                    (provide 'compat)))
                  ("shorthands.el"
                   ((defun hack-read-symbol-shorthands () nil)
                    (defun shorthands-font-lock-shorthands () nil)
                    (provide 'shorthands)))
                  ("dynamic-setting.el"
                   ((defun font-setting-change-default-font () nil)
                    (defun dynamic-setting-handle-config-changed-event () nil)
                    (provide 'dynamic-setting)))
                  ("uni-decimal.el"
                   ((defconst unicode-property-table nil)
                    (provide 'uni-decimal)))
                  ("uni-digit.el"
                   ((defconst unicode-property-table nil)
                    (provide 'uni-digit)))
                  ("uni-numeric.el"
                   ((defconst unicode-property-table nil)
                    (provide 'uni-numeric)))
                  ("benchmark.el"
                   ((defmacro benchmark-run (&rest forms) forms)
                    (defun benchmark-call (func &optional repetitions)
                      (list func repetitions))
                    (provide 'benchmark)))
                  ("password-cache.el"
                   ((defcustom password-cache t "Cache passwords." :type 'boolean)
                    (defun password-cache-add (key password)
                      (list key password))
                    (defun password-read-from-cache (key)
                      key)
                    (provide 'password-cache)))
                  ("double.el"
                   ((defun double-translate-key (key)
                      key)
                    (provide 'double)))
                  ("chistory.el"
                   ((defun command-history () nil)
                    (defun list-command-history () nil)
                    (provide 'chistory)))
                  ("scroll-lock.el"
                   ((defun scroll-lock-next-line () nil)
                    (provide 'scroll-lock)))
                  ("thread.el"
                   ((defun list-threads () nil)
                    (defun thread-list--get-entries () nil)
                    (provide 'thread)))
                  ("qp.el"
                   ((defun quoted-printable-decode-region (start end)
                      (list start end))
                    (defun quoted-printable-encode-string (string)
                      string)
                    (provide 'qp)))
                  ("mailheader.el"
                   ((defun mail-header-extract () nil)
                    (defun mail-header-format (header)
                      header)
                    (provide 'mailheader)))
                  ("yenc.el"
                   ((defun yenc-decode-region (start end)
                      (list start end))
                    (defun yenc-parse-line (line)
                      line)
                    (provide 'yenc)))
                  ("flow-fill.el"
                   ((defun fill-flowed () nil)
                    (defun fill-flowed-encode () nil)
                    (provide 'flow-fill)))
                  ("uudecode.el"
                   ((defun uudecode-decode-region (start end)
                      (list start end))
                    (defun uudecode-decode-region-internal (start end)
                      (list start end))
                    (provide 'uudecode)))
                  ("tq.el"
                   ((defun tq-create (process) process)
                    (defun tq-enqueue (&rest args) args)
                    (defun tq-filter (&rest args) args)
                    (provide 'tq)))
                  ("mail-prsvr.el"
                   ((defvar mail-parse-charset nil)
                    (provide 'mail-prsvr)))
                  ("mm-util.el"
                   ((defun mm-charset-to-coding-system (charset &rest args)
                      (cons charset args))
                    (defun mm-mime-charset (&rest args)
                      args)
                    (provide 'mm-util)))
                  ("rfc2047.el"
                   ((defun rfc2047-encode-string (string &rest args)
                      (cons string args))
                    (defun rfc2047-decode-string (string &rest args)
                      (cons string args))
                    (provide 'rfc2047)))
                  ("rfc2231.el"
                   ((defun rfc2231-parse-string (string)
                      string)
                    (defun rfc2231-encode-string (string)
                      string)
                    (provide 'rfc2231)))
                  ("mail-parse.el"
                   ((defun mail-header-parse-addresses-lax (string)
                      string)
                    (defun mail-header-parse-address-lax (string)
                      string)
                    (provide 'mail-parse)))
                  ("rfc6068.el"
                   ((defun rfc6068-parse-mailto-url (url)
                      url)
                    (defun rfc6068-unhexify-string (string)
                      string)
                    (provide 'rfc6068)))
                  ("mail-utils.el"
                   ((defun mail-file-babyl-p (file)
                      file)
                    (defun mail-fetch-field (field)
                      field)
                    (defun mail-strip-quoted-names (address)
                      address)
                    (provide 'mail-utils)))
                  ("rfc822.el"
                   ((defun rfc822-addresses () nil)
                    (defun rfc822-nuke-whitespace () nil)
                    (provide 'rfc822)))
                  ("ietf-drums-date.el"
                   ((defun ietf-drums-parse-date-string (string)
                      string)
                    (provide 'ietf-drums-date)))
                  ("binhex.el"
                   ((defun binhex-decode-region (start end)
                      (list start end))
                    (defun binhex-decode-region-internal (start end)
                      (list start end))
                    (defun binhex-string-big-endian (string)
                      string)
                    (provide 'binhex)))
                  ("sasl.el"
                   ((defun sasl-make-client (&rest args) args)
                    (defun sasl-next-step (&rest args) args)
                    (defun sasl-find-mechanism (&rest args) args)
                    (provide 'sasl)
                    (provide 'sasl-plain)
                    (provide 'sasl-login)
                    (provide 'sasl-anonymous)))
                  ("sasl-cram.el"
                   ((defun sasl-cram-md5-response (&rest args) args)
                    (provide 'sasl-cram)))
                  ("sasl-digest.el"
                   ((defun sasl-digest-md5-response (&rest args) args)
                    (provide 'sasl-digest)))
                  ("sasl-scram-rfc.el"
                   ((defun sasl-scram-sha-1-client-final-message (&rest args)
                      args)
                    (provide 'sasl-scram-rfc)
                    (provide 'sasl-scram-sha-1)))
                  ("sasl-scram-sha256.el"
                   ((defun sasl-scram-sha-256-client-final-message (&rest args)
                      args)
                    (provide 'sasl-scram-sha256)))
                  ("ntlm.el"
                   ((defun ntlm-build-auth-request (&rest args) args)
                    (defun ntlm-build-auth-response (&rest args) args)
                    (defun ntlm-get-password-hashes (&rest args) args)
                    (defun ntlm-md4hash (&rest args) args)
                    (provide 'ntlm)))
                  ("sasl-ntlm.el"
                   ((defun sasl-ntlm-request (&rest args) args)
                    (defun sasl-ntlm-response (&rest args) args)
                    (provide 'sasl-ntlm)))
                  ("compface.el"
                   ((defun uncompface (&rest args) args)
                    (provide 'compface)))
                  ("tramp-uu.el"
                   ((defun tramp-uuencode-region (start end)
                      (list start end))
                    (defun tramp-uu-byte-to-uu-char (byte)
                      byte)
                    (defun tramp-uu-b64-char-to-byte (char)
                      char)
                    (provide 'tramp-uu)))
                  ("trampver.el"
                   ((defvar tramp-version "0")
                    (defun tramp-inside-emacs () nil)
                    (provide 'trampver)))
                  ("bobcat.el"
                   ((defun terminal-init-bobcat () nil)
                    (provide 'term/bobcat)))
                  ("cygwin.el"
                   ((defun terminal-init-cygwin () nil)
                    (provide 'term/cygwin)))
                  ("vt200.el"
                   ((defun terminal-init-vt200 () nil)
                    (provide 'term/vt200)))
                  ("linux.el"
                   ((defun terminal-init-linux () nil)
                    (provide 'term/linux)))
                  ("vt100.el"
                   ((defun terminal-init-vt100 () nil)
                    (provide 'term/vt100)))
                  ("AT386.el"
                   ((defun terminal-init-AT386 () nil)
                    (provide 'term/AT386)))
                  ("news.el"
                   ((defun terminal-init-news () nil)
                    (provide 'term/news)))
                  ("lk201.el"
                   ((defvar lk201-function-map nil)
                    (defun terminal-init-lk201 () nil)
                    (provide 'term/lk201)))
                  ("w32console.el"
                   ((defvar w32-tty-standard-colors nil)
                    (defun terminal-init-w32console () nil)
                    (provide 'term/w32console)))
                  ("meese.el"
                   ((defun protect-innocence-hook () nil)
                    (provide 'meese)))
                  ("ps-def.el"
                   ((defun ps-mark-active-p () nil)
                    (defun ps-face-foreground-name () nil)
                    (defun ps-face-background-name () nil)
                    (provide 'ps-def)))
                  ("ps-print-loaddefs.el"
                   ((defvar ps-multibyte-buffer nil)
                    (provide 'ps-print-loaddefs)))
                  ("glyphless-mode.el"
                   ((defvar glyphless-mode-types nil)
                    (defun glyphless-mode--setup () nil)
                    (provide 'glyphless-mode)))
                  ("word-wrap-mode.el"
                   ((defvar word-wrap-whitespace-characters nil)
                    (defvar word-wrap-mode--previous-state nil)
                    (provide 'word-wrap-mode)))
                  ("sqlite.el"
                   ((defmacro with-sqlite-transaction (&rest body)
                      body)
                    (provide 'sqlite)))
                  ("url-future.el"
                   ((defun make-url-future (&rest args) args)
                    (defun url-future-call (&rest args) args)
                    (provide 'url-future)))
                  ("url-domsuf.el"
                   ((defvar url-domsuf-domains nil)
                    (defun url-domsuf-cookie-allowed-p (&rest args) args)
                    (provide 'url-domsuf)))
                  ("vt100-led.el"
                   ((defvar led-state nil)
                    (defun led-on () nil)
                    (defun led-off () nil)
                    (defun led-flash () nil)
                    (defun led-update () nil)
                    (provide 'vt100-led)))
                  ("khmer.el"
                   ((provide 'khmer)))
                  ("cham.el"
                   ((provide 'cham)))
                  ("czech.el"
                   ((provide 'czech)))
                  ("slovak.el"
                   ((provide 'slovak)))
                  ("georgian.el"
                   ((provide 'georgian)))
                  ("sinhala.el"
                   ((provide 'sinhala)))
                  ("romanian.el"
                   ((provide 'romanian)))
                  ("utf-8-lang.el"
                   ((provide 'utf-8-lang)))
                  ("burmese.el"
                   ((defvar burmese-composable-pattern nil)
                    (provide 'burmese)))
                  ("tai-viet.el"
                   ((provide 'tai-viet)))
                  ("english.el"
                   ((provide 'english)))
                  ("lao.el"
                   ((provide 'lao)))
                  ("greek.el"
                   ((provide 'greek)))
                  ("ethiopic.el"
                   ((provide 'ethiopic)))
                  ("philippine.el"
                   ((provide 'philippine)))
                  ("korean.el"
                   ((provide 'korean)))
                  ("vietnamese.el"
                   ((provide 'vietnamese)))
                  ("thai.el"
                   ((defvar tai-tham-composable-pattern nil)
                    (provide 'thai)))
                  ("tv-util.el"
                   ((defvar tai-viet-re nil)
                    (defun tai-viet-compose-region (&rest args) args)
                    (provide 'tai-viet-util)))
                  ("cyril-util.el"
                   ((defvar cyrillic-language-alist nil)
                    (defun standard-display-cyrillic-translit () nil)
                    (provide 'cyril-util)))
                  ("indonesian.el"
                   ((provide 'indonesian)))
                  ("korea-util.el"
                   ((defun setup-korean-environment-internal () nil)
                    (provide 'korea-util)))
                  ("china-util.el"
                   ((defun decode-hz-region (&rest args) args)
                    (defun encode-hz-region (&rest args) args)
                    (provide 'china-util)))
                  ("cyrillic.el"
                   ((provide 'cyrillic)))
                  ("hebrew.el"
                   ((defun hebrew-shape-gstring (&rest args) args)
                    (provide 'hebrew)))
                  ("japanese.el"
                   ((provide 'japanese)))
                  ("viet-util.el"
                   ((defun viet-decode-viqr-region (&rest args) args)
                    (defun viet-encode-viqr-region (&rest args) args)
                    (provide 'viet-util)))
                  ("chinese.el"
                   ((provide 'chinese)))
                  ("japan-util.el"
                   ((defun setup-japanese-environment-internal () nil)
                    (defun japanese-katakana (&rest args) args)
                    (defun japanese-hiragana (&rest args) args)
                    (provide 'japan-util)))
                  ("misc-lang.el"
                   ((defun arabic-shape-gstring (&rest args) args)
                    (defun egyptian-shape-grouping (&rest args) args)
                    (provide 'misc-lang)))
                  ("studly.el"
                   ((defun studlify-region (start end) (list start end))
                    (defun studlify-word (&rest args) args)
                    (provide 'studly)))
                  ("dissociate.el"
                   ((defun dissociated-press (&rest args) args)
                    (provide 'dissociate)))
                  ("makesum.el"
                   ((defun make-command-summary (&rest args) args)
                    (defun double-column (&rest args) args)
                    (provide 'makesum)))
                  ("vt-control.el"
                   ((defvar vt-applications-keypad-p t)
                    (defun vt-wide () nil)
                    (defun vt-keypad-on (&optional tell) tell)
                    (provide 'vt-control)))
                  ("flow-ctrl.el"
                   ((defvar flow-control-c-s-replacement ?\034)
                    (defun enable-flow-control (&optional argument) argument)
                    (provide 'flow-ctrl)))
                  ("talk.el"
                   ((defvar talk-display-alist nil)
                    (defun talk-connect (display) display)
                    (defun talk (&rest args) args)
                    (provide 'talk)))
                  ("nxml-maint.el"
                   ((defun nxml-insert-target-repertoire-glyph-set (file var)
                      (list file var))
                    (provide 'nxml-maint)))
                  ("nxml-util.el"
                   ((defconst nxml-debug nil)
                    (defmacro nxml-debug-change (name start end)
                      (list name start end))
                    (defun nxml-make-namespace (str) str)
                    (define-error 'nxml-error nil)
                    (provide 'nxml-util)))
                  ("vc-filewise.el"
                   ((defun vc-master-name (file) file)
                    (defun vc-filewise-registered (backend file)
                      (list backend file))
                    (provide 'vc-filewise)))
                  ("pgg-def.el"
                   ((defgroup pgg () "PGG" :group 'mail)
                    (defcustom pgg-default-scheme 'gpg "Scheme." :type 'symbol)
                    (defmacro pgg-truncate-key-identifier (key) key)
                    (provide 'pgg-def)))
                  ("autoconf.el"
                   ((defvar-keymap autoconf-mode-map)
                    (defun autoconf-current-defun-function () nil)
                    (define-derived-mode autoconf-mode prog-mode "Autoconf")
                    (provide 'autoconf)))
                  ("gssapi.el"
                   ((defcustom gssapi-program nil "Program." :type 'list)
                    (defun open-gssapi-stream
                        (name buffer server port user)
                      (list name buffer server port user))
                    (provide 'gssapi)))
                  ("scroll-all.el"
                   ((defun scroll-all-function-all (func arg)
                      (list func arg))
                    (define-minor-mode scroll-all-mode
                      "Scroll all windows together.")
                    (provide 'scroll-all)))
                  ("utf-7.el"
                   ((defun utf-7-decode (len imap) (list len imap))
                    (defun utf-7-encode (from to imap) (list from to imap))
                    (provide 'utf-7)))
                  ("rfc2368.el"
                   ((defconst rfc2368-mailto-regexp "mailto:")
                    (defun rfc2368-parse-mailto-url (mailto-url) mailto-url)
                    (provide 'rfc2368)))
                  ("timer-list.el"
                   ((defun list-timers (&optional _ignore-auto _nonconfirm)
                      nil)
                    (define-derived-mode timer-list-mode tabulated-list-mode
                      "Timer-List")
                    (provide 'timer-list)))
                  ("master.el"
                   ((defvar master-of nil)
                    (define-minor-mode master-mode
                      "Control a slave buffer.")
                    (defun master-says (&optional command arg)
                      (list command arg))
                    (provide 'master)))
                  ("helper.el"
                   ((defvar Helper-return-blurb nil)
                    (defun Helper-help () nil)
                    (provide 'helper)))
                  ("holiday-loaddefs.el"
                   ((autoload 'holiday-bahai "cal-bahai" nil t)
                    (provide 'holiday-loaddefs)))
                  ("loaddefs.el"
                   ((autoload 'ede-customize-project "ede/custom" nil t)
                    (provide 'loaddefs)))
                  ("theme-loaddefs.el"
                   ((provide 'theme-loaddefs)))
                  ("esh-module-loaddefs.el"
                   ((defgroup eshell-alias nil "Aliases." :group 'eshell)
                    (provide 'esh-module-loaddefs)))
                  ("diary-loaddefs.el"
                   ((autoload 'diary-bahai-list-entries "cal-bahai" nil t)
                    (provide 'diary-loaddefs)))
                  ("texinfo-loaddefs.el"
                   ((autoload 'makeinfo-region "makeinfo" nil t)
                    (provide 'texinfo-loaddefs)))
                  ("calc-loaddefs.el"
                   ((autoload 'calc-do-quick-calc "calc-aent" nil t)
                    (provide 'calc-loaddefs)))
                  ("rfc1843.el"
                   ((defcustom rfc1843-decode-loosely nil
                      "Decode loosely." :type 'boolean)
                    (defun rfc1843-decode-region (from to) (list from to))
                    (provide 'rfc1843)))
                  ("nxml-enc.el"
                   ((defvar nxml-file-name-ignore-case nil)
                    (defun nxml-set-auto-coding (file-name size)
                      (list file-name size))
                    (provide 'nxml-enc)))
                  ("bibtex-style.el"
                   ((defconst bibtex-style-commands nil)
                    (define-derived-mode bibtex-style-mode nil "BibStyle")
                    (defun bibtex-style-indent-line () nil)
                    (provide 'bibtex-style)))
                  ("dictionary-connection.el"
                   ((defun dictionary-connection-open (server port)
                      (list server port))
                    (defun dictionary-connection-close (connection)
                      connection)
                    (provide 'dictionary-connection)))
                  ("m4-mode.el"
                   ((defgroup m4 nil "M4." :group 'languages)
                    (defcustom m4-program "m4" "Program." :type 'string)
                    (define-derived-mode m4-mode prog-mode "m4")
                    (provide 'm4-mode)))
                  ("cookie1.el"
                   ((defgroup cookie nil "Cookie." :group 'games)
                    (defun cookie (phrase-file &optional startmsg endmsg)
                      (list phrase-file startmsg endmsg))
                    (provide 'cookie1)))
                  ("spook.el"
                   ((require 'cookie1)
                    (defgroup spook nil "Spook." :group 'games)
                    (defun spook () nil)
                    (provide 'spook)))
                  ("yow.el"
                   ((require 'cookie1)
                    (defgroup yow nil "Yow." :group 'games)
                    (defun yow (&optional insert display)
                      (list insert display))
                    (provide 'yow)))
                  ("bruce.el"
                   ((require 'cookie1)
                    (defgroup bruce nil "Bruce." :group 'games)
                    (defun bruce () nil)
                    (provide 'bruce)))
                  ("autoarg.el"
                   ((defun autoarg-kp-digit-argument (arg) arg)
                    (define-minor-mode autoarg-mode "Auto argument.")
                    (provide 'autoarg)))
                  ("tvi970.el"
                   ((defvar tvi970-terminal-map nil)
                    (defun terminal-init-tvi970 () nil)
                    (define-minor-mode tvi970-set-keypad-mode
                      "Set keypad mode.")
                    (provide 'term/tvi970)))
                  ("sun.el"
                   ((defun scroll-down-in-place (n) n)
                    (defun terminal-init-sun () nil)
                    (provide 'term/sun)))
                  ("subdirs.el"
                   ((normal-top-level-add-subdirs-to-load-path)))
                  ("edt-lk201.el"
                   ((defconst *EDT-keys* nil)))
                  ("edt-vt100.el"
                   ((defun edt-set-term-width-80 () nil)
                    (defun edt-set-term-width-132 () nil)))
                  ("rng-util.el"
                   ((require 'cl-lib)
                    (defun rng-make-datatypes-uri (uri) uri)
                    (define-error 'rng-error nil)
                    (provide 'rng-util)))
                  ("rng-dt.el"
                   ((require 'rng-util)
                    (defvar rng-dt-error-reporter nil)
                    (defun rng-dt-builtin-compile (name params)
                      (list name params))
                    (provide 'rng-dt)))
                  ("url-vars.el"
                   ((defgroup url nil "URL." :group 'comm)
                    (defvar-local url-current-object nil)
                    (defcustom url-honor-refresh-requests t
                      "Honor refresh." :type 'boolean)))
                  ("url-privacy.el"
                   ((require 'url-vars)
                    (defun url-device-type (&optional _device) nil)
                    (defun url-setup-privacy-info () nil)
                    (provide 'url-privacy)))
                  ("edt-pc.el"
                   ((defconst *EDT-keys* nil)))
                  ("w32-vars.el"
                   ((defgroup w32 nil "MS-Windows." :group 'environment)
                    (defcustom w32-use-w32-font-dialog t
                      "Use font dialog." :type 'boolean)
                    (provide 'w32-vars)))
                  ("novice.el"
                   ((defvar disabled-command-function
                      'disabled-command-function)
                    (defun disabled-command-function (&optional cmd keys)
                      (list cmd keys))
                    (defun enable-command (command) command)
                    (provide 'novice)))
                  ("page.el"
                   ((defun forward-page (&optional count) count)
                    (defun backward-page (&optional count) count)
                    (defun what-page () nil)
                    (provide 'page)))
                  ("cl-compat.el"
                   ((require 'cl-lib)
                    (defmacro defkeyword (x &optional doc)
                      (list x doc))
                    (defun Values (&rest val-forms) val-forms)
                    (provide 'cl-compat)))
                  ("elide-head.el"
                   ((defgroup elide-head nil "Hide headers." :group 'mail)
                    (define-minor-mode elide-head-mode
                      "Hide a buffer header.")
                    (defun elide-head (&optional arg) arg)
                    (provide 'elide-head)))
                  ("iimage.el"
                   ((defgroup iimage nil "Inline images." :group 'multimedia)
                    (defun iimage-recenter (&optional arg) arg)
                    (define-minor-mode iimage-mode nil)
                    (provide 'iimage)))
                  ("emacs-authors-mode.el"
                   ((require 'subr-x)
                    (defgroup emacs-authors-mode nil
                      "Authors view." :group 'help)
                    (defun emacs-authors-next-author (&optional arg) arg)
                    (define-derived-mode emacs-authors-mode special-mode
                      "Authors View")
                    (provide 'emacs-authors-mode)))
                  ("textsec-check.el"
                   ((defgroup textsec nil "Text security." :group 'i18n)
                    (defcustom textsec-check t "Check text." :type 'boolean)
                    (defun textsec-suspicious-p (object type)
                      (list object type))
                    (provide 'textsec-check)))
                  ("debug-early.el"
                   ((setq debug-early-backtrace nil)))
                  ("calc-macs.el"
                   ((defmacro calc-wrapper (&rest body) body)
                    (defmacro math-with-extra-prec (delta &rest body)
                      (cons delta body))
                    (provide 'calc-macs)))
                  ("kinsoku.el"
                   ((defvar kinsoku-limit 4)
                    (defun kinsoku-longer () nil)
                    (defun kinsoku (linebeg) linebeg)
                    (provide 'kinsoku)))
                  ("latexenc.el"
                   ((defcustom latex-inputenc-coding-alist nil
                      "Inputenc map." :type 'alist)
                    (defun latexenc-inputenc-to-coding-system (inputenc)
                      inputenc)
                    (provide 'latexenc)))
                  ("reposition.el"
                   ((defun reposition-window (&optional arg interactive)
                      (list arg interactive))
                    (defun repos-count-screen-lines (start end)
                      (list start end))
                    (provide 'reposition)))
                  ("ansi-osc.el"
                   ((defconst ansi-osc-control-seq-regexp "")
                    (defvar-local ansi-osc-window-title nil)
                    (defun ansi-osc-filter-region (begin end)
                      (list begin end))
                    (provide 'ansi-osc)))
                  ("morse.el"
                   ((defvar morse-code nil)
                    (defun morse-region (beg end) (list beg end))
                    (defun unmorse-region (beg end) (list beg end))
                    (provide 'morse)))
                  ("mh-buffers.el"
                   ((defconst mh-temp-buffer " *mh-temp*")
                    (defun mh-truncate-log-buffer () nil)
                    (provide 'mh-buffers)))
                  ("make.el"
                   ((defvar ede-make-min-version "3.0")
                    (defcustom ede-make-command "make"
                      "Make command." :type 'string)
                    (defun ede-make-check-version (&optional noerror)
                      noerror)
                    (provide 'ede/make)))
                  ("cedet-files.el"
                   ((defun cedet-directory-name-to-file-name
                        (referencedir &optional testmode)
                      (list referencedir testmode))
                    (provide 'cedet-files)))
                  ("epa-hook.el"
                   ((defgroup epa-file nil "EPA file." :group 'epa)
                    (defcustom epa-file-inhibit-auto-save t
                      "Inhibit auto-save." :type 'boolean)
                    (define-minor-mode auto-encryption-mode
                      "Auto encryption.")
                    (provide 'epa-hook)))
                  ("makefile-edit.el"
                   ((defun makefile-beginning-of-command () nil)
                    (defun makefile-move-to-macro (macro &optional next)
                      (list macro next))
                    (provide 'ede/makefile-edit)))
                  ("isearch-x.el"
                   ((defun isearch-toggle-input-method () nil)
                    (defun isearch-with-keyboard-coding () nil)))
                  ("wyse50.el"
                   ((defvar wyse50-terminal-map nil)
                    (defun terminal-init-wyse50 () nil)
                    (provide 'term/wyse50)))
                  ("gulp.el"
                   ((defgroup gulp nil "Gulp." :group 'mail)
                    (defcustom gulp-max-len 2000
                      "Maximum length." :type 'integer)
                    (defun gulp-send-requests (dir &optional time)
                      (list dir time))
                    (provide 'gulp)))
                  ("ediff-hook.el"
                   ((defvar menu-bar-ediff-misc-menu nil)
                    (defvar-keymap menu-bar-ediff-menu
                      :name "Compare")))
                  ("ld-script.el"
                   ((defgroup ld-script nil "LD script." :group 'languages)
                    (define-derived-mode ld-script-mode prog-mode
                      "LD-Script")
                    (provide 'ld-script)))
                  ("dig.el"
                   ((defgroup dig nil "Dig." :group 'net)
                    (defcustom dig-program "dig" "Program." :type 'string)
                    (define-derived-mode dig-mode special-mode "Dig")
                    (defun dig (domain &optional type class server)
                      (list domain type class server))
                    (provide 'dig)))
                  ("rng-pttrn.el"
                   ((defvar rng-schema-change-hook nil)
                    (defun rng-make-ref (name) name)
                    (defun rng-make-choice (patterns) patterns)))
                  ("sieve-mode.el"
                   ((autoload 'sieve-manage "sieve")
                    (defgroup sieve nil "Sieve." :group 'mail)
                    (define-derived-mode sieve-mode prog-mode "Sieve")
                    (provide 'sieve-mode)))
                  ("bat-mode.el"
                   ((defgroup bat-mode nil "BAT." :group 'languages)
                    (defun bat-run () nil)
                    (define-derived-mode bat-mode prog-mode "Bat")
                    (provide 'bat-mode)))
                  ("netrc.el"
                   ((defgroup netrc nil "Netrc." :group 'net)
                    (defcustom netrc-file "~/.authinfo"
                      "Netrc file." :type 'file)
                    (defun netrc-parse (&optional file) file)
                    (provide 'netrc)))
                  ("minibuf-eldef.el"
                   ((defvar minibuffer-eldef-shorten-default nil)
                    (defun minibuf-eldef-setup-minibuffer () nil)
                    (define-minor-mode minibuffer-electric-default-mode
                      "Show defaults in minibuffer prompts.")
                    (provide 'minibuf-eldef)))
                  ("visual-wrap.el"
                   ((defcustom visual-wrap-extra-indent 0
                      "Extra indent." :type 'integer)
                    (define-minor-mode visual-wrap-prefix-mode
                      "Wrap with visual prefix.")
                    (provide 'visual-wrap)))
                  ("display-line-numbers.el"
                   ((defgroup display-line-numbers nil
                      "Display line numbers." :group 'convenience)
                    (defun display-line-numbers-update-width () nil)
                    (define-minor-mode display-line-numbers-mode
                      "Display line numbers.")
                    (provide 'display-line-numbers)))
                  ("mouse-copy.el"
                   ((defvar mouse-copy-last-paste-start nil)
                    (defun mouse-copy-work-around-drag-bug
                        (start-event end-event)
                      (list start-event end-event))
                    (provide 'mouse-copy)))
                  ("animate.el"
                   ((defgroup animate nil "Animate." :group 'games)
                    (defun animate-string (string vpos &optional hpos)
                      (list string vpos hpos))
                    (provide 'animate)))
                  ("gmm-utils.el"
                   ((defgroup gmm nil "Gnus menu mode." :group 'gnus)
                    (defcustom gmm-verbose 7 "Verbose." :type 'integer)
                    (defun gmm-message (level &rest args)
                      (cons level args))
                    (provide 'gmm-utils)))
                  ("userlock.el"
                   ((define-error 'file-locked "File is locked" 'file-error)
                    (defun ask-user-about-lock (file opponent)
                      (list file opponent))
                    (define-error 'file-supersession nil 'file-error)))
                  ("rfn-eshadow.el"
                   ((defconst file-name-shadow-properties-custom-type nil)
                    (defcustom file-name-shadow-properties nil
                      "Properties." :type 'list)
                    (defun rfn-eshadow-setup-minibuffer () nil)
                    (define-minor-mode file-name-shadow-mode
                      "Shadow file names.")
                    (provide 'rfn-eshadow)))
                  ("asm-mode.el"
                   ((defgroup asm nil "Assembler." :group 'languages)
                    (defcustom asm-comment-char ?\;
                      "Comment character." :type 'character)
                    (define-derived-mode asm-mode prog-mode "Assembler")
                    (defun asm-indent-line () nil)
                    (provide 'asm-mode)))
                  ("bib-mode.el"
                   ((defgroup bib nil "Bibliography." :group 'text)
                    (defcustom bib-file "~/my-bibliography.bib"
                      "Bibliography file." :type 'file)
                    (defun bib-add () nil)
                    (define-derived-mode bib-mode text-mode "Bib")
                    (provide 'bib)))
                  ("reveal.el"
                   ((defgroup reveal nil "Reveal." :group 'outlines)
                    (defcustom reveal-around-mark t
                      "Reveal around mark." :type 'boolean)
                    (defun reveal-post-command () nil)
                    (define-minor-mode reveal-mode "Reveal overlays.")
                    (provide 'reveal)))
                  ("emacs-lock.el"
                   ((defgroup emacs-lock nil "Buffer locking." :group 'convenience)
                    (defcustom emacs-lock-default-locking-mode 'all
                      "Default mode." :type 'symbol)
                    (defun emacs-lock-live-process-p (buffer-or-name)
                      buffer-or-name)
                    (define-minor-mode emacs-lock-mode "Lock buffer.")
                    (provide 'emacs-lock)))
                  ("linum.el"
                   ((defvar-local linum-overlays nil)
                    (defgroup linum nil "Line numbers." :group 'convenience)
                    (define-minor-mode linum-mode "Display line numbers.")
                    (defun linum-on () nil)
                    (provide 'linum)))
                  ("refill.el"
                   ((defvar-local refill-ignorable-overlay nil)
                    (defun refill-fill-paragraph (arg) arg)
                    (define-minor-mode refill-mode "Refill text.")
                    (provide 'refill)))
                  ("nnnil.el"
                   ((defvar nnnil-status-string "")
                    (defun nnnil-open-server (_server &optional _definitions)
                      t)
                    (defun nnnil-request-post (&optional _server) nil)
                    (provide 'nnnil)))
                  ("po.el"
                   ((defconst po-content-type-charset-alist nil)
                    (defun po-find-charset (filename) filename)
                    (provide 'po)))
                  ("cedet.el"
                   ((defconst cedet-version "2.0")
                    (defun cedet-version () cedet-version)
                    (provide 'cedet)))
                  ("cc-compat.el"
                   ((defvar c-indent-level 2)
                    (defun cc-block-intro-offset (langelem) langelem)))
                  ("cedet-cscope.el"
                   ((defvar cedet-cscope-min-version "15.7")
                    (defcustom cedet-cscope-command "cscope"
                      "Cscope command." :type 'string)
                    (defun cedet-cscope-search
                        (searchtext texttype type _scope)
                      (list searchtext texttype type))
                    (provide 'cedet-cscope)))
                  ("metamail.el"
                   ((defgroup metamail nil "Metamail." :group 'mail)
                    (defcustom metamail-program-name "metamail"
                      "Program." :type 'string)
                    (defun metamail-buffer (&optional viewmode buffer nodisplay)
                      (list viewmode buffer nodisplay))
                    (provide 'metamail)))
                  ("string-edit.el"
                   ((require 'cl-lib)
                    (cl-defun string-edit
                        (prompt string success-callback &key abort-callback)
                      (list prompt string success-callback abort-callback))
                    (define-derived-mode string-edit-mode text-mode "String")
                    (provide 'string-edit)))
                  ("flymake-cc.el"
                   ((require 'cl-lib)
                    (defcustom flymake-cc-command nil
                      "Command." :type 'sexp)
                    (defun flymake-cc (report-fn &rest _args) report-fn)
                    (provide 'flymake-cc)))
                  ("external-completion.el"
                   ((require 'cl-lib)
                    (defun external-completion-table
                        (category lookup &optional metadata)
                      (list category lookup metadata))
                    (provide 'external-completion)))
                  ("yank-media.el"
                   ((require 'cl-lib)
                    (require 'seq)
                    (defvar yank-media--registered-handlers nil)
                    (defun yank-media () nil)
                    (provide 'yank-media)))
                  ("cyril-jis.el"
                   ((quail-define-package "cyrillic-jis" "Cyrillic")))
                  ("cedet-idutils.el"
                   ((defvar cedet-idutils-min-version "4.0")
                    (defcustom cedet-idutils-file-command "fnid"
                      "File command." :type 'string)
                    (defun cedet-idutils-search
                        (searchtext texttype type _scope)
                      (list searchtext texttype type))
                    (provide 'cedet-idutils)))
                  ("sup-mouse.el"
                   ((defcustom sup-mouse-fast-select-window nil
                      "Fast select." :type 'boolean)
                    (defconst mouse-left 0)
                    (defun sup-mouse-report () nil)
                    (provide 'sup-mouse)))
                  ("cedet-global.el"
                   ((defvar cedet-global-min-version "5.0")
                    (defcustom cedet-global-command "global"
                      "Global command." :type 'string)
                    (defun cedet-gnu-global-search
                        (searchtext texttype type scope)
                      (list searchtext texttype type scope))
                    (provide 'cedet-global)))
                  ("mantemp.el"
                   ((defun mantemp-remove-comments () nil)
                    (defun mantemp-make-mantemps-region () nil)
                    (provide 'mantemp)))
                  ("ediff-vers.el"
                   ((defcustom ediff-keep-tmp-versions nil
                      "Keep temp versions." :type 'boolean)
                    (defun ediff-vc-latest-version (file) file)
                    (provide 'ediff-vers)))
                  ("gs.el"
                   ((defvar gs-program "gs")
                    (defun gs-options (device file) (list device file))
                    (provide 'gs)))
                  ("unrmail.el"
                   ((defcustom unrmail-mbox-format 'mboxrd
                      "Mbox format." :type 'symbol)
                    (defun unrmail (file to-file) (list file to-file))
                    (provide 'unrmail)))
                  ("backquote.el"
                   ((provide 'backquote)
                    (defun backquote-list*-function (first &rest list)
                      (cons first list))
                    (defmacro backquote (structure) structure)))
                  ("dirtrack.el"
                   ((defgroup dirtrack nil "Directory tracking." :group 'shell)
                    (defcustom dirtrack-debug nil
                      "Debug." :type 'boolean)
                    (define-minor-mode dirtrack-mode
                      "Track directory.")
                    (provide 'dirtrack)))
                  ("keypad.el"
                   ((provide 'keypad)
                    (defcustom keypad-setup nil
                      "Keypad setup." :type 'sexp)
                    (defun keypad-setup
                        (setup &optional numlock shift decimal)
                      (list setup numlock shift decimal))))
                  ("rtree.el"
                   ((defmacro rtree-make-node () '(vector nil nil nil))
                    (defun rtree-make (range) range)
                    (defun rtree-memq (tree number) (list tree number))))
                  ("executable.el"
                   ((defgroup executable nil "Executable scripts." :group 'files)
                    (defcustom executable-insert t
                      "Insert magic." :type 'boolean)
                    (defun executable-chmod () nil)
                    (provide 'executable)))
                  ("shadow.el"
                   ((defgroup lisp-shadow nil
                      "Load path shadows." :group 'lisp)
                    (defun load-path-shadows-find (&optional path) path)
                    (define-derived-mode load-path-shadows-mode special-mode
                      "LP-Shadows")
                    (provide 'shadow)))
                  ("cl-font-lock.el"
                   ((defvar cl-font-lock-built-in--functions nil)
                    (define-minor-mode cl-font-lock-built-in-mode
                      "Fontify CL symbols.")
                    (provide 'cl-font-lock)))
                  ("starttls.el"
                   ((defgroup starttls nil "STARTTLS." :group 'net)
                    (defcustom starttls-program "starttls"
                      "Program." :type 'string)
                    (defun starttls-open-stream (name buffer host port)
                      (list name buffer host port))
                    (provide 'starttls)))
                  ("diff.el"
                   ((defgroup diff nil "Diff." :group 'tools)
                    (defcustom diff-command "diff"
                      "Diff command." :type 'string)
                    (defun diff (old new &optional switches no-async)
                      (list old new switches no-async))
                    (provide 'diff)))
                  ("dos-fns.el"
                   ((defun dos-convert-standard-filename (filename) filename)
                    (defun dos-8+3-filename (filename) filename)
                    (provide 'dos-fns)))
                  ("crm.el"
                   ((defvar crm-separator "[ \t]*,[ \t]*")
                    (defun completing-read-multiple
                        (prompt table &optional predicate require-match
                                initial-input hist def inherit-input-method)
                      (list prompt table predicate require-match initial-input
                            hist def inherit-input-method))
                    (provide 'crm)))
                  ("epg-config.el"
                   ((defconst epg-package-name "epg")
                    (defgroup epg () "EasyPG." :group 'applications)
                    (defun epg-find-configuration
                        (protocol &optional no-cache program-alist)
                      (list protocol no-cache program-alist))))
                  ("subword.el"
                   ((defvar subword-forward-function
                      'subword-forward-internal)
                    (define-minor-mode subword-mode
                      "Move through subwords.")
                    (defun subword-forward (&optional arg) arg)))
                  ("font-core.el"
                   ((defvar-local font-lock-defaults nil)
                    (define-minor-mode font-lock-mode
                      "Toggle Font Lock mode.")
                    (defun turn-on-font-lock () nil)
                    (provide 'font-core)))))
    (let ((standalone-source-normalize-current-file (car entry)))
      (dolist (form (cadr entry))
        (should
         (null
          (standalone-source-normalize-top-level-forms form)))))))

(ert-deftest standalone-source-normalize-test/drops-org-inlinetask-load-only-forms ()
  (let ((standalone-source-normalize-current-file "org-inlinetask.el"))
    (dolist (form '((defgroup org-inlinetask nil
                      "UI metadata."
                      :group 'org-structure)
                    (defcustom org-inlinetask-min-level 15
                      "Documentation."
                      :group 'org-inlinetask
                      :type 'integer)
                    (defvar org-odd-levels-only)
                    (defun org-inlinetask-outline-regexp ()
                      "Documentation."
                      "regexp")
                    (add-hook 'org-cycle-hook
                              'org-inlinetask-hide-tasks)))
      (should
       (null
        (standalone-source-normalize-top-level-forms form))))))

(ert-deftest standalone-source-normalize-test/drops-ol-doi-load-only-forms ()
  (let ((standalone-source-normalize-current-file "ol-doi.el"))
    (dolist (form '((defcustom org-link-doi-server-url "https://doi.org/"
                      "Documentation."
                      :group 'org-link-follow
                      :type 'string)
                    (defun org-link-doi-open (path arg)
                      (browse-url path arg))
                    (provide 'ol-doi)))
      (should
       (null
        (standalone-source-normalize-top-level-forms form))))))

(ert-deftest standalone-source-normalize-test/drops-ol-mhe-load-only-forms ()
  (let ((standalone-source-normalize-current-file "ol-mhe.el"))
    (dolist (form '((defcustom org-mhe-search-all-folders nil
                      "Documentation."
                      :group 'org-link-follow
                      :type 'boolean)
                    (org-link-set-parameters "mhe"
                                             :follow #'org-mhe-open
                                             :store #'org-mhe-store-link)
                    (defun org-mhe-store-link (&optional _interactive?)
                      "Documentation."
                      nil)
                    (defun org-mhe-open (path _)
                      "Documentation."
                      (org-mhe-follow-link path nil))
                    (defun org-mhe-get-message-real-folder ()
                      "Documentation."
                      nil)
                    (defun org-mhe-get-message-folder ()
                      "Documentation."
                      nil)
                    (defun org-mhe-get-message-num ()
                      "Documentation."
                      nil)
                    (defun org-mhe-get-header (header)
                      "Documentation."
                      header)
                    (defun org-mhe-follow-link (folder article)
                      "Documentation."
                      (list folder article))
                    (provide 'ol-mhe)))
      (should
       (null
        (standalone-source-normalize-top-level-forms form))))))

(ert-deftest standalone-source-normalize-test/drops-ol-w3m-load-only-forms ()
  (let ((standalone-source-normalize-current-file "ol-w3m.el"))
    (dolist (form '((defvar w3m-current-url)
                    (org-link-set-parameters "w3m"
                                             :store #'org-w3m-store-link)
                    (defun org-w3m-store-link ()
                      "Documentation."
                      nil)
                    (defun org-w3m-copy-for-org-mode ()
                      "Documentation."
                      (interactive)
                      nil)
                    (defun org-w3m-get-anchor-start ()
                      "Documentation."
                      (point))
                    (defun org-w3m-get-next-link-start ()
                      "Documentation."
                      (point))
                    (defun org-w3m-no-next-link-p ()
                      "Documentation."
                      t)
                    (add-hook 'w3m-mode-hook
                              (lambda ()
                                (define-key w3m-mode-map "k"
                                  'org-w3m-copy-for-org-mode)))
                    (provide 'ol-w3m)))
      (should
       (null
        (standalone-source-normalize-top-level-forms form))))))

(ert-deftest standalone-source-normalize-test/drops-ol-irc-load-only-forms ()
  (let ((standalone-source-normalize-current-file "ol-irc.el"))
    (dolist (form '((defvar org-irc-client 'erc
                      "Documentation.")
                    (defvar org-irc-link-to-logs nil
                      "Documentation.")
                    (org-link-set-parameters "irc"
                                             :follow #'org-irc-visit
                                             :store #'org-irc-store-link
                                             :export #'org-irc-export)
                    (defun org-irc-visit (link _)
                      "Documentation."
                      link)
                    (defun org-irc-parse-link (link)
                      "Documentation."
                      link)
                    (defun org-irc-store-link (&optional _interactive?)
                      "Documentation."
                      nil)
                    (defun org-irc-ellipsify-description (string &optional after)
                      "Documentation."
                      (ignore after)
                      string)
                    (defun org-irc-get-current-erc-port ()
                      "Documentation."
                      nil)
                    (defun org-irc-export (link description format)
                      "Documentation."
                      (list link description format))
                    (provide 'ol-irc)))
      (should
       (null
        (standalone-source-normalize-top-level-forms form))))))

(ert-deftest standalone-source-normalize-test/drops-tempo-load-only-forms ()
  (let ((standalone-source-normalize-current-file "tempo.el"))
    (dolist (form '((defvar-local tempo-collection nil
                      "Documentation.")
                    (defun tempo-define-template
                        (name elements &optional tag documentation taglist)
                      "Documentation."
                      (list name elements tag documentation taglist))
                    (defun tempo-complete-tag (&optional silent)
                      "Documentation."
                      silent)
                    (provide 'tempo)))
      (should
       (null
        (standalone-source-normalize-top-level-forms form))))))

(ert-deftest standalone-source-normalize-test/drops-org-tempo-load-only-forms ()
  (let ((standalone-source-normalize-current-file "org-tempo.el"))
    (dolist (form '((defgroup org-tempo nil
                      "Documentation."
                      :group 'org)
                    (defvar org-tempo-tags nil
                      "Documentation.")
                    (defcustom org-tempo-keywords-alist nil
                      "Documentation."
                      :type 'list)
                    (defun org-tempo-setup ()
                      "Documentation."
                      nil)
                    (tempo-define-template "org-include" nil "<I"
                                           "Include keyword"
                                           'org-tempo-tags)
                    (add-hook 'org-mode-hook #'org-tempo-setup)
                    (provide 'org-tempo)))
      (should
       (null
        (standalone-source-normalize-top-level-forms form))))))

(ert-deftest standalone-source-normalize-test/drops-inline-load-only-forms ()
  (let ((standalone-source-normalize-current-file "inline.el"))
    (dolist (form '((defmacro inline-quote (_exp)
                      "Documentation."
                      nil)
                    (defmacro define-inline (name args &rest body)
                      "Documentation."
                      (list name args body))
                    (defun inline--do-quote (exp)
                      "Documentation."
                      exp)
                    (provide 'inline)))
      (should
       (null
        (standalone-source-normalize-top-level-forms form))))))

(ert-deftest standalone-source-normalize-test/appends-org-link-surfaces-after-ol ()
  (let ((source (make-temp-file "standalone-source-normalize-" t)))
    (unwind-protect
        (let ((file (expand-file-name "ol.el" source)))
          (with-temp-file file
            (insert "(defvar org-link-parameters nil)\n"))
          (let ((forms (standalone-source-normalize-read-forms-from-file file)))
            (should (member '(defvar org-link-doi-server-url "https://doi.org/")
                            forms))
            (should (member '(provide 'org-link-doi) forms))
            (should (member '(provide 'ol-doi) forms))
            (should
             (cl-some
              (lambda (form)
                (and (consp form)
                     (eq (car form) 'defun)
                     (eq (cadr form) 'org-link-doi-export)))
              forms))
            (should (member '(defvar org-mhe-search-all-folders nil)
                            forms))
            (should (member '(provide 'ol-mhe) forms))
            (should
             (cl-some
              (lambda (form)
                (and (consp form)
                     (eq (car form) 'defun)
                     (eq (cadr form) 'org-mhe-follow-link)))
              forms))
            (should (member '(provide 'ol-w3m) forms))
            (should
             (cl-some
              (lambda (form)
                (and (consp form)
                     (eq (car form) 'defun)
                     (eq (cadr form) 'org-w3m-copy-for-org-mode)))
              forms))
            (should (member '(defvar org-irc-client 'erc) forms))
            (should (member '(defvar org-irc-link-to-logs nil) forms))
            (should (member '(provide 'ol-irc) forms))
	            (should
	             (cl-some
	              (lambda (form)
	                (and (consp form)
	                     (eq (car form) 'defun)
	                     (eq (cadr form) 'org-irc-export)))
	              forms))))
      (when (file-directory-p source)
        (delete-directory source t)))))

(ert-deftest standalone-source-normalize-test/appends-tempo-surface-after-tempo ()
  (let ((source (make-temp-file "standalone-source-normalize-" t)))
    (unwind-protect
        (let ((file (expand-file-name "tempo.el" source)))
          (with-temp-file file
            (insert "(defvar-local tempo-collection nil)\n"))
          (let ((forms (standalone-source-normalize-read-forms-from-file file)))
            (let ((surface (car forms)))
              (should (eq (car surface) 'progn))
              (should (member '(defvar tempo-tags nil) surface))
              (should (member '(defvar tempo-local-tags '((tempo-tags . nil)))
                              surface))
              (should (member '(provide 'tempo) surface))
              (should (member '(defvar org-tempo-tags nil) surface))
              (should (member '(provide 'org-tempo) surface))
              (should (member '(provide 'inline) surface))
              (should
               (cl-some
                (lambda (form)
                  (and (consp form)
                       (eq (car form) 'defun)
                       (eq (cadr form) 'tempo-define-template)))
                surface))
              (should
               (cl-some
                (lambda (form)
                  (and (consp form)
                       (eq (car form) 'defun)
                       (eq (cadr form) 'org-tempo-setup)))
                surface))
              (should
               (cl-some
                (lambda (form)
                  (and (consp form)
                       (eq (car form) 'fset)
                       (equal (cadr form) ''define-inline)))
                surface))
              (should
               (cl-some
                (lambda (form)
                  (and (consp form)
                       (eq (car form) 'fset)
                       (equal (cadr form) ''inline--do-quote)))
                surface)))))
      (when (file-directory-p source)
        (delete-directory source t)))))

(ert-deftest standalone-source-normalize-test/drops-org-tempo-without-synthetic-forms ()
  (let ((source (make-temp-file "standalone-source-normalize-" t)))
    (unwind-protect
        (let ((file (expand-file-name "org-tempo.el" source)))
          (with-temp-file file
            (insert "(require 'tempo)\n"))
          (should (null (standalone-source-normalize-read-forms-from-file file))))
      (when (file-directory-p source)
        (delete-directory source t)))))

(ert-deftest standalone-source-normalize-test/drops-inline-without-synthetic-forms ()
  (let ((source (make-temp-file "standalone-source-normalize-" t)))
    (unwind-protect
        (let ((file (expand-file-name "inline.el" source)))
          (with-temp-file file
            (insert "(require 'macroexp)\n"))
          (should (null (standalone-source-normalize-read-forms-from-file file))))
      (when (file-directory-p source)
        (delete-directory source t)))))

(ert-deftest standalone-source-normalize-test/drops-easymenu-without-synthetic-forms ()
  (let ((source (make-temp-file "standalone-source-normalize-" t)))
    (unwind-protect
        (let ((file (expand-file-name "easymenu.el" source)))
          (with-temp-file file
            (insert "(defun easy-menu-create-menu (name items) (list name items))\n"))
          (should (null (standalone-source-normalize-read-forms-from-file file))))
      (when (file-directory-p source)
        (delete-directory source t)))))

(ert-deftest standalone-source-normalize-test/drops-let-alist-without-synthetic-forms ()
  (let ((source (make-temp-file "standalone-source-normalize-" t)))
    (unwind-protect
        (let ((file (expand-file-name "let-alist.el" source)))
          (with-temp-file file
            (insert "(defmacro let-alist (alist &rest body) `(let ((alist ,alist)) ,@body))\n"))
          (should (null (standalone-source-normalize-read-forms-from-file file))))
      (when (file-directory-p source)
        (delete-directory source t)))))

(ert-deftest standalone-source-normalize-test/drops-radix-tree-without-synthetic-forms ()
  (let ((source (make-temp-file "standalone-source-normalize-" t)))
    (unwind-protect
        (let ((file (expand-file-name "radix-tree.el" source)))
          (with-temp-file file
            (insert "(defun radix-tree-insert (tree key val) (list tree key val))\n"))
          (should (null (standalone-source-normalize-read-forms-from-file file))))
      (when (file-directory-p source)
        (delete-directory source t)))))

(ert-deftest standalone-source-normalize-test/drops-text-property-search-without-synthetic-forms ()
  (let ((source (make-temp-file "standalone-source-normalize-" t)))
    (unwind-protect
        (let ((file (expand-file-name "text-property-search.el" source)))
          (with-temp-file file
            (insert "(defun text-property-search-forward (property) property)\n"))
          (should (null (standalone-source-normalize-read-forms-from-file file))))
      (when (file-directory-p source)
        (delete-directory source t)))))

(ert-deftest standalone-source-normalize-test/drops-thunk-without-synthetic-forms ()
  (let ((source (make-temp-file "standalone-source-normalize-" t)))
    (unwind-protect
        (let ((file (expand-file-name "thunk.el" source)))
          (with-temp-file file
            (insert "(defun thunk-force (delayed) (funcall delayed))\n"))
          (should (null (standalone-source-normalize-read-forms-from-file file))))
      (when (file-directory-p source)
        (delete-directory source t)))))

(ert-deftest standalone-source-normalize-test/drops-env-without-synthetic-forms ()
  (let ((source (make-temp-file "standalone-source-normalize-" t)))
    (unwind-protect
        (let ((file (expand-file-name "env.el" source)))
          (with-temp-file file
            (insert "(defun substitute-env-vars (string) string)\n"))
          (should (null (standalone-source-normalize-read-forms-from-file file))))
      (when (file-directory-p source)
        (delete-directory source t)))))

(ert-deftest standalone-source-normalize-test/drops-fileloop-without-synthetic-forms ()
  (let ((source (make-temp-file "standalone-source-normalize-" t)))
    (unwind-protect
        (let ((file (expand-file-name "fileloop.el" source)))
          (with-temp-file file
            (insert "(iter-defun fileloop--list-to-iterator (list) list)\n")
            (insert "(defun fileloop-initialize (files scan operate) files)\n"))
          (should (null (standalone-source-normalize-read-forms-from-file file))))
      (when (file-directory-p source)
        (delete-directory source t)))))

(ert-deftest standalone-source-normalize-test/drops-oc-bibtex-late-callables ()
  (let ((standalone-source-normalize-current-file "oc-bibtex.el"))
    (dolist (symbol '(org-cite-bibtex-export-bibliography
                      org-cite-bibtex-export-citation))
      (should
       (null
        (standalone-source-normalize-top-level-forms
         `(defun ,symbol (&rest _args) nil)))))
    (should
     (null
      (standalone-source-normalize-top-level-forms
       '(org-cite-register-processor
         'bibtex
         :export-bibliography #'org-cite-bibtex-export-bibliography))))))

(ert-deftest standalone-source-normalize-test/drops-org-citation-backend-late-callables ()
  (dolist (entry '(("oc-natbib.el"
                    org-cite-natbib--style-to-command
                    org-cite-natbib--build-optional-arguments
                    org-cite-natbib--build-arguments
                    org-cite-natbib-export-bibliography
                    org-cite-natbib-export-citation
                    org-cite-natbib-use-package)
                   ("oc-biblatex.el"
                    org-cite-biblatex--package-options
                    org-cite-biblatex--multicite-p
                    org-cite-biblatex--atomic-arguments
                    org-cite-biblatex--multi-arguments
                    org-cite-biblatex--command
                    org-cite-biblatex--expand-shortcuts
                    org-cite-biblatex-list-styles
                    org-cite-biblatex-export-bibliography
                    org-cite-biblatex-export-citation
                    org-cite-biblatex-prepare-preamble)))
    (let ((standalone-source-normalize-current-file (car entry)))
      (dolist (symbol (cdr entry))
        (should
         (null
          (standalone-source-normalize-top-level-forms
           `(defun ,symbol (&rest _args) nil)))))
      (should
       (null
        (standalone-source-normalize-top-level-forms
         '(org-cite-register-processor
           'backend
           :export-bibliography #'backend-export-bibliography)))))))

(ert-deftest standalone-source-normalize-test/drops-file-scoped-top-level-provide ()
  (let ((standalone-source-normalize-current-file "org-element-ast.el"))
    (should
     (null
      (standalone-source-normalize-top-level-forms
       '(provide 'org-element-ast)))))
  (let ((standalone-source-normalize-current-file "org.el"))
    (should
     (equal
      (standalone-source-normalize-top-level-forms
       '(provide 'org-element-ast))
      '((provide 'org-element-ast))))))

(ert-deftest standalone-source-normalize-test/drops-org-footnote-top-level-provide ()
  (let ((standalone-source-normalize-current-file "org-footnote.el"))
    (should
     (null
      (standalone-source-normalize-top-level-forms
       '(provide 'org-footnote))))))

(ert-deftest standalone-source-normalize-test/drops-org-list-top-level-provide ()
  (let ((standalone-source-normalize-current-file "org-list.el"))
    (should
     (null
      (standalone-source-normalize-top-level-forms
       '(provide 'org-list))))))

(ert-deftest standalone-source-normalize-test/drops-org-entities-top-level-provide ()
  (let ((standalone-source-normalize-current-file "org-entities.el"))
    (should
     (null
      (standalone-source-normalize-top-level-forms
       '(provide 'org-entities))))))

(ert-deftest standalone-source-normalize-test/drops-help-macro-top-level-provide ()
  (let ((standalone-source-normalize-current-file "help-macro.el"))
    (should
     (null
      (standalone-source-normalize-top-level-forms
       '(provide 'help-macro))))))

(ert-deftest standalone-source-normalize-test/drops-org-macro-top-level-provide ()
  (let ((standalone-source-normalize-current-file "org-macro.el"))
    (should
     (null
      (standalone-source-normalize-top-level-forms
       '(provide 'org-macro))))))

(ert-deftest standalone-source-normalize-test/drops-ob-eval-top-level-provide ()
  (let ((standalone-source-normalize-current-file "ob-eval.el"))
    (should
     (null
      (standalone-source-normalize-top-level-forms
       '(provide 'ob-eval))))))

(ert-deftest standalone-source-normalize-test/drops-org-faces-top-level-provide ()
  (let ((standalone-source-normalize-current-file "org-faces.el"))
    (should
     (null
      (standalone-source-normalize-top-level-forms
       '(provide 'org-faces))))))

(ert-deftest standalone-source-normalize-test/drops-oc-bibtex-top-level-provide ()
  (let ((standalone-source-normalize-current-file "oc-bibtex.el"))
    (should
     (null
      (standalone-source-normalize-top-level-forms
       '(provide 'oc-bibtex))))))

(ert-deftest standalone-source-normalize-test/drops-org-citation-backend-top-level-provides ()
  (dolist (entry '(("oc-natbib.el" oc-natbib)
                   ("oc-biblatex.el" oc-biblatex)))
    (let ((standalone-source-normalize-current-file (car entry)))
      (should
       (null
        (standalone-source-normalize-top-level-forms
         `(provide ',(cadr entry))))))))

(ert-deftest standalone-source-normalize-test/drops-org-inlinetask-top-level-provide ()
  (let ((standalone-source-normalize-current-file "org-inlinetask.el"))
    (should
     (null
      (standalone-source-normalize-top-level-forms
       '(provide 'org-inlinetask))))))

(ert-deftest standalone-source-normalize-test/rewrites-top-level-defsubst-to-defun ()
  (let ((standalone-source-normalize-current-file "org-element-ast.el"))
    (should
     (equal
      (standalone-source-normalize-top-level-forms
       '(defsubst demo-subst (node)
          "Documentation."
          (setq-local demo-value node)
          demo-value))
      '((defun demo-subst (node)
          (set (make-local-variable 'demo-value) node)
          demo-value))))))

(ert-deftest standalone-source-normalize-test/drops-shimmed-top-level-defsubst ()
  (should
   (null
    (standalone-source-normalize-top-level-forms
     '(defsubst org-element-properties-resolve (node &optional force)
        node))))
  (should
   (null
    (standalone-source-normalize-top-level-forms
     '(defsubst org-element-contents (node)
        (if (consp node) (cdr node) nil)))))
  (should
   (null
    (standalone-source-normalize-top-level-forms
     '(defsubst org-item-re ()
        "Documentation."
        "regexp"))))
  (should
   (null
    (standalone-source-normalize-top-level-forms
     '(defsubst org-item-beginning-re ()
        "Documentation."
        (concat "^" (org-item-re))))))
  (should
   (null
    (standalone-source-normalize-top-level-forms
	     '(defsubst org-entity-get (name)
	        "Documentation."
	        (assoc name org-entities))))))

(ert-deftest standalone-source-normalize-test/drops-shimmed-top-level-define-inline ()
  (should
   (null
    (standalone-source-normalize-top-level-forms
     '(define-inline org-element-property (property node)
        (inline-quote nil))))))

(ert-deftest standalone-source-normalize-test/drops-shimmed-top-level-defun ()
  (should
   (null
    (standalone-source-normalize-top-level-forms
     '(defun org-element-contents (node)
        (if (consp node) (cdr node) nil)))))
  (should
   (null
    (standalone-source-normalize-top-level-forms
     '(defun org-element--property (property node &optional dflt force)
        (list property node dflt force)))))
  (should
   (null
    (standalone-source-normalize-top-level-forms
     '(defun org-entities-help ()
        (message "help"))))))

(ert-deftest standalone-source-normalize-test/drops-version-top-level-defun ()
  (dolist (symbol '(android-read-build-system
                    android-read-build-time
                    emacs-version
                    emacs-repository-version-git
                    emacs-repository-version-android
                    emacs-repository-get-version
                    emacs-repository-branch-android
                    emacs-repository-branch-git
                    emacs-repository-get-branch))
    (should
     (null
      (standalone-source-normalize-top-level-forms
       `(defun ,symbol (&optional arg)
          "Documentation."
          arg))))))

(ert-deftest standalone-source-normalize-test/drops-help-macro-top-level-defun ()
  (should
   (null
    (standalone-source-normalize-top-level-forms
     '(defun help--help-screen (help-line help-text helped-map buffer-name)
        "Documentation."
        (list help-line help-text helped-map buffer-name))))))

(ert-deftest standalone-source-normalize-test/drops-org-macro-top-level-defuns ()
  (dolist (symbol '(org-macro--makeargs
                    org-macro--set-templates
                    org-macro--collect-macros
                    org-macro-initialize-templates
                    org-macro-expand
                    org-macro-replace-all
                    org-macro-escape-arguments
                    org-macro-extract-arguments
                    org-macro--get-property
                    org-macro--find-keyword-value
                    org-macro--find-date
                    org-macro--vc-modified-time
                    org-macro--counter-initialize
                    org-macro--counter-increment))
    (should
     (null
      (standalone-source-normalize-top-level-forms
       `(defun ,symbol (&rest args)
          "Documentation."
          args))))))

(ert-deftest standalone-source-normalize-test/drops-ob-eval-top-level-defuns ()
  (dolist (symbol '(org-babel-eval-error-notify
                    org-babel-eval
                    org-babel-eval-read-file
                    org-babel--shell-command-on-region
                    org-babel--write-temp-buffer-input-file
                    org-babel-eval-wipe-error-buffer
                    org-babel--get-shell-file-name))
    (should
     (null
      (standalone-source-normalize-top-level-forms
       `(defun ,symbol (&rest args)
          "Documentation."
          args))))))

(ert-deftest standalone-source-normalize-test/drops-help-macro-top-level-defmacro ()
  (should
   (null
    (standalone-source-normalize-top-level-forms
     '(defmacro make-help-screen (fname help-line help-text helped-map)
        "Documentation."
        (list 'defun fname nil help-line help-text helped-map))))))

(ert-deftest standalone-source-normalize-test/drops-shimmed-top-level-defalias ()
  (should
   (null
    (standalone-source-normalize-top-level-forms
     '(defalias 'org-element-resolve-deferred
        'org-element-properties-resolve))))
  (should
   (null
    (standalone-source-normalize-top-level-forms
     '(defalias 'org-list-get-item-begin
        'org-in-item-p))))
  (should
   (null
    (standalone-source-normalize-top-level-forms
     '(defalias 'org-list-get-first-item
        'org-list-get-list-begin))))
  (should
   (null
    (standalone-source-normalize-top-level-forms
     '(defalias 'version
        'emacs-version)))))

(ert-deftest standalone-source-normalize-test/drops-shimmed-obsolete-alias ()
  (should
   (null
    (standalone-source-normalize-top-level-forms
     '(define-obsolete-function-alias
        'emacs-bzr-get-version
        'emacs-repository-get-version
        "24.4"))))
  (should
   (null
    (standalone-source-normalize-top-level-forms
     '(define-obsolete-variable-alias
        'emacs-bzr-version
        'emacs-repository-version
        "24.4")))))

(ert-deftest standalone-source-normalize-test/rewrites-top-level-define-inline-to-callable-stub ()
  (let ((standalone-source-normalize-current-file "org-element-ast.el"))
    (should
     (equal
      (standalone-source-normalize-top-level-forms
       '(define-inline demo-inline (node)
          "Documentation."
          (inline-quote (car node))))
      '((defun demo-inline (node) nil))))))

(ert-deftest standalone-source-normalize-test/drops-top-level-ui-key-and-menu-wiring ()
  (should
   (null
    (standalone-source-normalize-top-level-forms
     '(org-defkey org-agenda-mode-map "q" #'org-agenda-quit))))
  (should
   (null
    (standalone-source-normalize-top-level-forms
     '(easy-menu-define org-agenda-menu org-agenda-mode-map
        "Agenda menu." '("Agenda")))))
  (should
   (null
    (standalone-source-normalize-top-level-forms
     '(gv-define-setter org-element-property (value property node)
        `(org-element-put-property ,node ,property ,value))))))

(ert-deftest standalone-source-normalize-test/drops-files-top-level-key-wiring ()
  (let ((standalone-source-normalize-current-file "files.el"))
    (should-not
     (standalone-source-normalize-top-level-forms
      '(define-key ctl-x-map "i" 'insert-file)))
    (should-not
     (standalone-source-normalize-top-level-forms
      '(define-key esc-map "~" 'not-modified)))
    (should
     (standalone-source-normalize-top-level-forms
      '(define-key unrelated-map "i" 'insert-file)))))

(ert-deftest standalone-source-normalize-test/drops-window-top-level-key-wiring ()
  (let ((standalone-source-normalize-current-file "window.el"))
    (should-not
     (standalone-source-normalize-top-level-forms
      '(define-key global-map [?\C-l] 'recenter-top-bottom)))
    (should-not
     (standalone-source-normalize-top-level-forms
      '(define-key ctl-x-map "2" 'split-window-below)))
    (should-not
     (standalone-source-normalize-top-level-forms
      '(define-key ctl-x-4-map "0" 'kill-buffer-and-window)))
    (should
     (standalone-source-normalize-top-level-forms
      '(define-key unrelated-map "2" 'split-window-below)))))

(ert-deftest standalone-source-normalize-test/drops-uninitialized-top-level-defvar ()
  (should
   (null
    (standalone-source-normalize-top-level-forms
     '(defvar org-element-cache-map-continue-from)))))

(ert-deftest standalone-source-normalize-test/preserves-initialized-top-level-defvar ()
  (should
   (equal
    (standalone-source-normalize-top-level-forms
     '(defvar org-outline-regexp "\\*+ "))
    '((defvar org-outline-regexp "\\*+ ")))))

(ert-deftest standalone-source-normalize-test/drops-shimmed-top-level-defvar ()
  (dolist (symbol '(org-checkbox-statistics-hook
                    org-list-forbidden-blocks
                    org--item-re-cache
                    org-last-indent-begin-marker
                    org-last-indent-end-marker
                    org-macro--counter-table
                    org-babel-error-buffer-name
                    emacs-repository-version
                    emacs-repository-branch))
    (should
     (null
      (standalone-source-normalize-top-level-forms
       `(defvar ,symbol nil
          "Documentation."))))))

(ert-deftest standalone-source-normalize-test/drops-top-level-defvar-docstring ()
  (should
   (equal
    (standalone-source-normalize-top-level-forms
     '(defvar org-datetree-base-level 1 "Documentation."))
    '((defvar org-datetree-base-level 1)))))

(ert-deftest standalone-source-normalize-test/rewrites-top-level-defvar-local-to-defvar ()
  (should
   (equal
    (standalone-source-normalize-top-level-forms
     '(defvar-local org-macro-templates nil
        "Documentation."))
    '((defvar org-macro-templates nil)))))

(ert-deftest standalone-source-normalize-test/rewrites-top-level-defconst-to-setq ()
  (should
   (equal
    (standalone-source-normalize-top-level-forms
     '(defconst org-ts-regexp (format "<\\(%s\\)>" org-ts--internal-regexp)
        "Regular expression."))
    '((progn
        (setq org-ts-regexp
              (format "<\\(%s\\)>" org-ts--internal-regexp))
        'org-ts-regexp)))))

(ert-deftest standalone-source-normalize-test/elides-listed-defconst-data ()
  (let ((standalone-source-normalize-elided-defconst-symbols '(demo-table)))
    (should
     (equal
      (standalone-source-normalize-top-level-forms
       '(defconst demo-table '(("large" "data"))))
      '((progn
          (setq demo-table nil)
          'demo-table))))))

(ert-deftest standalone-source-normalize-test/elides-listed-hash-defconst-data ()
  (let ((standalone-source-normalize-elided-defconst-symbols '(demo-table))
        (table (make-hash-table :test 'equal)))
    (puthash "key" "value" table)
    (should
     (equal
      (standalone-source-normalize-top-level-forms
       (list 'defconst 'demo-table (list 'quote table)))
      '((progn
          (setq demo-table (make-hash-table :test 'equal))
          'demo-table))))))

(ert-deftest standalone-source-normalize-test/elides-emoji-names-defconst-data ()
  (let ((table (make-hash-table :test 'equal)))
    (puthash "key" "value" table)
    (should
     (equal
      (standalone-source-normalize-top-level-forms
       (list 'defconst 'emoji--names (list 'quote table)))
      '((progn
          (setq emoji--names (make-hash-table :test 'equal))
          'emoji--names))))))

(ert-deftest standalone-source-normalize-test/drops-shimmed-top-level-defconst ()
  (dolist (symbol '(org-list-end-re
                    org-list-full-item-re
                    org-entities
                    org-footnote-re
                    org-footnote-definition-re
                    org-footnote-forbidden-blocks
                    emacs-major-version
                    emacs-minor-version
                    emacs-build-system
                    emacs-build-time
                    emacs-build-number))
    (should
     (null
      (standalone-source-normalize-top-level-forms
       `(defconst ,symbol "regexp"
          "Documentation."))))))

(ert-deftest standalone-source-normalize-test/rewrites-top-level-defcustom-lightly ()
  (should
   (equal
    (standalone-source-normalize-top-level-forms
     '(defcustom org-modules '(ol-doi ol-w3m)
        "Modules that should always be loaded together with org.el."
        :group 'org
        :type '(set (const :tag "DOI links" ol-doi)
                    (const :tag "W3M links" ol-w3m))))
      '((progn
        (defvar org-modules
          '(ol-doi ol-w3m)
          nil)
        (put 'org-modules 'standard-value (list ''(ol-doi ol-w3m)))
        (put 'org-modules 'custom-args t)
        'org-modules)))))

(ert-deftest standalone-source-normalize-test/rewrites-text-mode-defcustoms-to-bindings ()
  (dolist (form '((defcustom text-mode-hook '(text-mode-hook-identify)
                    "Documentation."
                    :type 'hook
                    :options '(turn-on-auto-fill turn-on-flyspell)
                    :group 'text)
                  (defcustom text-mode-ispell-word-completion 'completion-at-point
                    "Documentation."
                    :type '(choice symbol function)
                    :group 'text)))
    (should
     (equal
      (standalone-source-normalize-top-level-forms form)
      (list (list 'defvar (cadr form) (caddr form)))))))

(ert-deftest standalone-source-normalize-test/drops-ol-doi-defcustom-after-synthetic-binding ()
  (let ((standalone-source-normalize-current-file "ol-doi.el"))
    (should
     (null
      (standalone-source-normalize-top-level-forms
       '(defcustom org-link-doi-server-url "https://doi.org/"
          "Documentation."
          :group 'org-link-follow
          :type 'string))))))

(ert-deftest standalone-source-normalize-test/drops-shimmed-top-level-defcustom ()
  (dolist (symbol '(org-footnote-section
                    org-cycle-include-plain-lists
                    org-entities-user
                    org-cite-natbib-options
                    org-cite-natbib-bibliography-style
                    org-cite-biblatex-options
                    org-cite-biblatex-styles
                    org-cite-biblatex-style-shortcuts
                    three-step-help
                    help-for-help-use-variable-pitch))
    (should
     (null
      (standalone-source-normalize-top-level-forms
       `(defcustom ,symbol t
          "Documentation."
          :group 'org))))))

(ert-deftest standalone-source-normalize-test/drops-shimmed-top-level-define-minor-mode ()
  (should
   (null
    (standalone-source-normalize-top-level-forms
     '(define-minor-mode org-list-checkbox-radio-mode
        "Documentation."
        :lighter " CheckBoxRadio"
        nil)))))

(ert-deftest standalone-source-normalize-test/rewrites-top-level-defgroup-lightly ()
  (should
   (equal
    (standalone-source-normalize-top-level-forms
     '(defgroup org nil
        "Outline-based notes management and organizer."
        :group 'outlines))
	    '((progn
	        (put 'org 'custom-group t)
	        (put 'org 'custom-args t)
	        'org)))))

(ert-deftest standalone-source-normalize-test/rewrites-top-level-defface-lightly ()
  (should
   (equal
    (standalone-source-normalize-top-level-forms
     '(defface org-demo-face
        '((t (:foreground "red" :inherit custom-face-attributes)))
        "Documentation."))
    '((progn
        (emacs-faces-make-face 'org-demo-face)
        'org-demo-face)))))

(ert-deftest standalone-source-normalize-test/rewrites-org-set-tag-faces-as-callable-fset ()
  (should
   (equal
    (standalone-source-normalize-top-level-forms
     '(defun org-set-tag-faces (var value)
        (set-default-toplevel-value var value)
        (if (not value)
          (setq org-tags-special-faces-re nil)
          (setq org-tags-special-faces-re
                (concat ":" (regexp-opt (mapcar #'car value) t) ":")))))
    '((fset
       'org-set-tag-faces
       'ignore)))))

(ert-deftest standalone-source-normalize-test/drops-shimmed-org-faces-defcustom ()
  (should
   (null
    (standalone-source-normalize-top-level-forms
     '(defcustom org-tag-faces nil
        "Documentation."
        :group 'org-faces
        :type '(repeat sexp))))))

(ert-deftest standalone-source-normalize-test/drops-org-faces-late-defcustoms ()
  (dolist (symbol '(org-faces-easy-properties
                    org-todo-keyword-faces
                    org-priority-faces
                    org-tag-faces
                    org-fontify-quote-and-verse-blocks
                    org-agenda-deadline-faces
                    org-n-level-faces
                    org-cycle-level-faces))
    (should
     (null
      (standalone-source-normalize-top-level-forms
       `(defcustom ,symbol nil
          "Documentation."
          :group 'org-faces
          :type '(repeat sexp)))))))

(ert-deftest standalone-source-normalize-test/drops-late-org-face-definition ()
  (should
   (null
    (standalone-source-normalize-top-level-forms
     '(defface org-block-begin-line
        '((t (:inherit shadow)))
        "Documentation.")))))

(ert-deftest standalone-source-normalize-test/drops-shimmed-top-level-defgroup ()
  (dolist (group '(org-footnote org-plain-lists org-entities))
    (should
     (null
      (standalone-source-normalize-top-level-forms
       `(defgroup ,group nil
          "UI metadata."
          :group 'org))))))

(ert-deftest standalone-source-normalize-test/rewrites-defalias-function-symbol ()
  (should
   (equal
    (standalone-source-normalize-top-level-forms
     '(defalias 'org-force-cycle-archived #'org-cycle-force-archived))
    '((defalias 'org-force-cycle-archived 'org-cycle-force-archived)))))

(ert-deftest standalone-source-normalize-test/preserves-defalias-function-lambda ()
  (should
   (equal
    (standalone-source-normalize-top-level-forms
     '(defalias 'demo #'(lambda () (setq-local demo-value 1))))
    '((defalias 'demo
        #'(lambda ()
            (set (make-local-variable 'demo-value) 1)))))))

(ert-deftest standalone-source-normalize-test/drops-top-level-defun-docstring ()
  (should
   (equal
    (standalone-source-normalize-top-level-forms
     '(defun demo (context)
        "Long vendor documentation."
        (setq-local demo-value context)
        demo-value))
    '((defun demo (context)
        (set (make-local-variable 'demo-value) context)
        demo-value)))))

(ert-deftest standalone-source-normalize-test/elides-large-top-level-defun-body ()
  (let ((standalone-source-normalize-large-defun-character-limit 20))
    (should
     (equal
      (standalone-source-normalize-top-level-forms
       '(defun demo-large (&optional arg)
          (let ((value arg))
            (setq value (cons value value))
            value)))
      '((progn
          (defun demo-large (&optional arg) nil)
          (put 'demo-large 'standalone-source-elided-body t)
          'demo-large))))))

(ert-deftest standalone-source-normalize-test/elides-listed-top-level-defun-body ()
  (let ((standalone-source-normalize-large-defun-character-limit 10000)
        (standalone-source-normalize-elided-defun-symbols '(demo-listed)))
    (should
     (equal
      (standalone-source-normalize-top-level-forms
       '(defun demo-listed (arg)
          (cons arg arg)))
      '((progn
          (defun demo-listed (arg) nil)
          (put 'demo-listed 'standalone-source-elided-body t)
          'demo-listed))))))

(ert-deftest standalone-source-normalize-test/elided-interactive-defun-keeps-command-shape ()
  (let ((standalone-source-normalize-large-defun-character-limit 10000)
        (standalone-source-normalize-elided-defun-symbols '(demo-command)))
    (should
     (equal
      (standalone-source-normalize-top-level-forms
       '(defun demo-command (&optional arg)
          "Documentation."
          (declare (indent 0))
          (interactive "P")
          (message "%S" arg)))
      '((progn
          (defun demo-command (&optional arg) (interactive "P") nil)
          (put 'demo-command 'standalone-source-elided-body t)
          'demo-command))))))

(ert-deftest standalone-source-normalize-test/elides-unmarked-listed-defun-body ()
  (let ((standalone-source-normalize-large-defun-character-limit 10000)
        (standalone-source-normalize-elided-defun-symbols nil)
        (standalone-source-normalize-unmarked-elided-defun-symbols
         '(demo-unmarked)))
    (should
     (equal
      (standalone-source-normalize-top-level-forms
       '(defun demo-unmarked (arg)
          (cons arg arg)))
      '((progn
          (fset 'demo-unmarked 'ignore)
          'demo-unmarked))))))

(ert-deftest standalone-source-normalize-test/drops-listed-top-level-defun ()
  (let ((standalone-source-normalize-dropped-defun-symbols '(demo-dropped)))
    (should-not
     (standalone-source-normalize-top-level-forms
      '(defun demo-dropped (arg)
         (message "%S" arg))))))

(ert-deftest standalone-source-normalize-test/drops-dired-desktop-handler-registration ()
  (should-not
   (standalone-source-normalize-top-level-forms
    '(add-to-list 'desktop-buffer-mode-handlers
                  '(dired-mode . dired-restore-desktop-buffer)))))

(ert-deftest standalone-source-normalize-test/elides-help-fns-analyze-function ()
  (should
   (equal
    (standalone-source-normalize-top-level-forms
     '(defun help-fns--analyze-function (function)
        (symbol-function function)))
    '((progn
        (defun help-fns--analyze-function (function) nil)
        (put 'help-fns--analyze-function 'standalone-source-elided-body t)
        'help-fns--analyze-function)))))

(ert-deftest standalone-source-normalize-test/elides-org-offer-links-in-entry ()
  (should
   (equal
    (standalone-source-normalize-top-level-forms
     '(defun org-offer-links-in-entry (buffer marker)
        (with-current-buffer buffer
          (goto-char marker))))
    '((progn
        (defun org-offer-links-in-entry (buffer marker) nil)
        (put 'org-offer-links-in-entry 'standalone-source-elided-body t)
        'org-offer-links-in-entry)))))

(ert-deftest standalone-source-normalize-test/elides-org-mark-ring-goto ()
  (should
   (equal
    (standalone-source-normalize-top-level-forms
     '(defun org-mark-ring-goto (&optional n)
        (interactive "p")
        (goto-char n)))
    '((progn
        (defun org-mark-ring-goto (&optional n) (interactive "p") nil)
        (put 'org-mark-ring-goto 'standalone-source-elided-body t)
        'org-mark-ring-goto)))))

(ert-deftest standalone-source-normalize-test/drops-org-mark-ring-initialization ()
  (should-not
   (standalone-source-normalize-top-level-forms
    '(dotimes (_ org-mark-ring-length)
       (push (make-marker) org-mark-ring))))
  (should-not
   (standalone-source-normalize-top-level-forms
    '(setcdr (nthcdr (1- org-mark-ring-length) org-mark-ring)
             org-mark-ring))))

(ert-deftest standalone-source-normalize-test/bundles-ignore-defun-group ()
  (let ((standalone-source-normalize-bundled-ignore-defun-groups
         '((demo-first demo-alias demo-second))))
    (should
     (equal
      (standalone-source-normalize-top-level-forms
       '(defun demo-first () (first)))
      '((progn
          (fset 'demo-first 'ignore)
          (fset 'demo-alias 'ignore)
          (fset 'demo-second 'ignore)
          'demo-first))))
    (should
     (null
      (standalone-source-normalize-top-level-forms
       '(defalias 'demo-alias 'demo-first))))
    (should
     (null
      (standalone-source-normalize-top-level-forms
       '(defun demo-second () (second)))))))

(ert-deftest standalone-source-normalize-test/drops-file-scoped-bundled-defun ()
  (let ((standalone-source-normalize-current-file "org-list.el"))
    (should
     (null
      (standalone-source-normalize-top-level-forms
       '(defun org-list-at-regexp-after-bullet-p (regexp)
          regexp)))))
  (should
   (consp
    (standalone-source-normalize-top-level-forms
     '(defun org-list-at-regexp-after-bullet-p (regexp)
        regexp)))))

(ert-deftest standalone-source-normalize-test/caches-file-form-strings ()
  (let ((source (make-temp-file "standalone-source-normalize-" nil ".el"))
        (cache-dir (make-temp-file "standalone-source-normalize-cache-" t))
        standalone-source-normalize-cache-directory)
    (unwind-protect
        (progn
          (setq standalone-source-normalize-cache-directory cache-dir)
          (with-temp-file source
            (insert "(defvar cache-a 1)\n"))
          (should (equal (standalone-source-normalize-file-to-form-strings
                          source)
                         '("(defvar cache-a 1)")))
          (should (directory-files cache-dir nil "\\.elcache\\'"))
          (should (equal (standalone-source-normalize-file-to-form-strings
                          source)
                         '("(defvar cache-a 1)"))))
      (when (file-exists-p source)
        (delete-file source))
      (when (file-directory-p cache-dir)
        (delete-directory cache-dir t)))))

(ert-deftest standalone-source-normalize-test/cache-invalidates-on-change ()
  (let ((source (make-temp-file "standalone-source-normalize-" nil ".el"))
        (cache-dir (make-temp-file "standalone-source-normalize-cache-" t))
        standalone-source-normalize-cache-directory)
    (unwind-protect
        (progn
          (setq standalone-source-normalize-cache-directory cache-dir)
          (with-temp-file source
            (insert "(defvar cache-a 1)\n"))
          (standalone-source-normalize-file-to-form-strings source)
          (sleep-for 0.01)
          (with-temp-file source
            (insert "(defvar cache-b 2)\n"))
          (should (equal (standalone-source-normalize-file-to-form-strings
                          source)
                         '("(defvar cache-b 2)"))))
      (when (file-exists-p source)
        (delete-file source))
      (when (file-directory-p cache-dir)
        (delete-directory cache-dir t)))))

(ert-deftest standalone-source-normalize-test/corrupt-cache-is-miss ()
  (let ((source (make-temp-file "standalone-source-normalize-" nil ".el"))
        (cache-dir (make-temp-file "standalone-source-normalize-cache-" t))
        standalone-source-normalize-cache-directory)
    (unwind-protect
        (progn
          (setq standalone-source-normalize-cache-directory cache-dir)
          (with-temp-file source
            (insert "(defvar cache-a 1)\n"))
          (standalone-source-normalize-file-to-form-strings source)
          (with-temp-file (standalone-source-normalize--cache-file source)
            (insert "(:version"))
          (should (equal (standalone-source-normalize-file-to-form-strings
                          source)
                         '("(defvar cache-a 1)"))))
      (when (file-exists-p source)
        (delete-file source))
      (when (file-directory-p cache-dir)
        (delete-directory cache-dir t)))))

(ert-deftest standalone-source-normalize-test/splits-quoted-hash-table-defconst ()
  (let ((table (make-hash-table :test 'equal)))
    (puthash "a" 1 table)
    (puthash "b" '(2 3) table)
    (let ((forms (standalone-source-normalize-top-level-forms
                  (list 'defconst 'sample (list 'quote table)))))
      (should (= 3 (length forms)))
      (should (equal (car forms)
                     '(setq sample
                            (make-hash-table :test 'equal))))
      (should (member '(puthash "a" 1 sample) (cdr forms)))
      (should (member '(puthash "b" '(2 3) sample) (cdr forms))))))

(provide 'standalone-source-normalize-test)

;;; standalone-source-normalize-test.el ends here
