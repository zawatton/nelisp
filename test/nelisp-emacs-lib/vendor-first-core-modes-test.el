;;; vendor-first-core-modes-test.el --- Vendor-first core mode smoke -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)

(defconst vendor-first-core-modes-test--repo-root
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name)))
  "Repository root for vendor-first core mode tests.")

(defun vendor-first-core-modes-test--vendor-load-path ()
  "Return load paths with vendored Emacs Lisp before local shims."
  (list
   (expand-file-name "vendor/emacs-lisp" vendor-first-core-modes-test--repo-root)
   (expand-file-name "vendor/emacs-lisp/emacs-lisp"
                     vendor-first-core-modes-test--repo-root)
   (expand-file-name "vendor/emacs-lisp/textmodes"
                     vendor-first-core-modes-test--repo-root)
   (expand-file-name "vendor/emacs-lisp/vc"
                     vendor-first-core-modes-test--repo-root)
   (expand-file-name "vendor/emacs-lisp/calendar"
                     vendor-first-core-modes-test--repo-root)
   (expand-file-name "vendor/emacs-lisp/mail"
                     vendor-first-core-modes-test--repo-root)
   (expand-file-name "vendor/emacs-lisp/international"
                     vendor-first-core-modes-test--repo-root)
   (expand-file-name "vendor/emacs-lisp/url"
                     vendor-first-core-modes-test--repo-root)
   (expand-file-name "vendor/emacs-lisp/nxml"
                     vendor-first-core-modes-test--repo-root)
   (expand-file-name "vendor/emacs-lisp/gnus"
                     vendor-first-core-modes-test--repo-root)
   (expand-file-name "vendor/emacs-lisp/net"
                     vendor-first-core-modes-test--repo-root)
   (expand-file-name "src" vendor-first-core-modes-test--repo-root)))

(defun vendor-first-core-modes-test--load-source (relative)
  "Load vendored source file RELATIVE and return its absolute path."
  (let ((file (expand-file-name relative vendor-first-core-modes-test--repo-root))
        (load-path (append (vendor-first-core-modes-test--vendor-load-path)
                           load-path)))
    (load file nil nil t)
    file))

(ert-deftest vendor-first-core-modes-test/text-outline-dired-vc-load-vendor-source ()
  "Core mode progress is measured by vendored Emacs Lisp loading first."
  (let ((loaded-files
         (mapcar #'vendor-first-core-modes-test--load-source
	                 '("vendor/emacs-lisp/textmodes/text-mode.el"
	                   "vendor/emacs-lisp/outline.el"
	                   "vendor/emacs-lisp/dired.el"
	                   "vendor/emacs-lisp/dired-x.el"
	                   "vendor/emacs-lisp/register.el"
	                   "vendor/emacs-lisp/vc/vc.el"
	                   "vendor/emacs-lisp/vc/vc-git.el"
	                   "vendor/emacs-lisp/emacs-lisp/eieio.el"))))
    (dolist (file loaded-files)
      (should (file-exists-p file)))
    (dolist (feature '(text-mode outline dired dired-x register vc vc-git eieio))
      (should (featurep feature)))
    (dolist (symbol '(text-mode outline-mode dired dired-mode
                                dired-jump get-register set-register
	                                vc-dir vc-diff vc-print-log
	                                vc-git-registered vc-git-state
	                                defclass make-instance cl--find-class))
      (should (fboundp symbol)))))

(ert-deftest vendor-first-core-modes-test/lightweight-support-libraries-load-vendor-source ()
  "Small upstream libraries stay as vendor-first substrate gates."
  (let ((loaded-files
         (mapcar #'vendor-first-core-modes-test--load-source
                 '("vendor/emacs-lisp/button.el"
                   "vendor/emacs-lisp/thingatpt.el"
                   "vendor/emacs-lisp/case-table.el"
                   "vendor/emacs-lisp/help-macro.el"
                   "vendor/emacs-lisp/emacs-lisp/subr-x.el"
                   "vendor/emacs-lisp/cdl.el"
                   "vendor/emacs-lisp/emacs-lisp/regi.el"
                   "vendor/emacs-lisp/emacs-lisp/generator.el"
                   "vendor/emacs-lisp/emacs-lisp/avl-tree.el"
                   "vendor/emacs-lisp/hex-util.el"
                   "vendor/emacs-lisp/array.el"
                   "vendor/emacs-lisp/color.el"
                   "vendor/emacs-lisp/composite.el"
                   "vendor/emacs-lisp/disp-table.el"
                   "vendor/emacs-lisp/display-line-numbers.el"
                   "vendor/emacs-lisp/delim-col.el"
                   "vendor/emacs-lisp/descr-text.el"
                   "vendor/emacs-lisp/electric.el"
                   "vendor/emacs-lisp/elec-pair.el"
                   "vendor/emacs-lisp/chistory.el"
                   "vendor/emacs-lisp/emacs-lisp/lisp.el"
                   "vendor/emacs-lisp/emacs-lisp/map-ynp.el"
                   "vendor/emacs-lisp/international/charprop.el"
                   "vendor/emacs-lisp/international/charscript.el"
                   "vendor/emacs-lisp/international/emoji-labels.el"
                   "vendor/emacs-lisp/international/idna-mapping.el"
                   "vendor/emacs-lisp/international/iso-transl.el"
                   "vendor/emacs-lisp/international/uni-confusable.el"
                   "vendor/emacs-lisp/international/uni-comment.el"
                   "vendor/emacs-lisp/international/uni-lowercase.el"
                   "vendor/emacs-lisp/international/uni-titlecase.el"
                   "vendor/emacs-lisp/international/uni-uppercase.el"
                   "vendor/emacs-lisp/international/uni-bidi.el"
                   "vendor/emacs-lisp/international/uni-combining.el"
                   "vendor/emacs-lisp/international/uni-brackets.el"
                   "vendor/emacs-lisp/international/uni-mirrored.el"
                   "vendor/emacs-lisp/international/uni-category.el"
                   "vendor/emacs-lisp/international/uni-decomposition.el"
                   "vendor/emacs-lisp/international/uni-special-lowercase.el"
                   "vendor/emacs-lisp/international/uni-special-titlecase.el"
                   "vendor/emacs-lisp/international/uni-special-uppercase.el"
                   "vendor/emacs-lisp/international/uni-old-name.el"
                   "vendor/emacs-lisp/international/uni-decimal.el"
                   "vendor/emacs-lisp/international/uni-digit.el"
                   "vendor/emacs-lisp/international/uni-numeric.el"
                   "vendor/emacs-lisp/international/uni-name.el"
                   "vendor/emacs-lisp/international/textsec-check.el"
                   "vendor/emacs-lisp/international/kinsoku.el"
                   "vendor/emacs-lisp/international/latexenc.el"
                   "vendor/emacs-lisp/international/utf-7.el"
                   "src/emacs-translation-table.el"
                   "vendor/emacs-lisp/ansi-osc.el"
                   "vendor/emacs-lisp/textmodes/glyphless-mode.el"
                   "vendor/emacs-lisp/textmodes/word-wrap-mode.el"
                   "vendor/emacs-lisp/url/url-vars.el"
                   "vendor/emacs-lisp/url/url-future.el"
                   "vendor/emacs-lisp/url/url-privacy.el"
                   "vendor/emacs-lisp/url/url-domsuf.el"
                   "vendor/emacs-lisp/url/url-file.el"
                   "vendor/emacs-lisp/url/url-auth.el"
                   "vendor/emacs-lisp/url/url-ftp.el"
                   "vendor/emacs-lisp/url/url-nfs.el"
                   "vendor/emacs-lisp/url/url-gw.el"
                   "vendor/emacs-lisp/url/url-irc.el"
                   "vendor/emacs-lisp/url/url-handlers.el"
                   "vendor/emacs-lisp/nxml/nxml-util.el"
                   "vendor/emacs-lisp/nxml/rng-util.el"
                   "vendor/emacs-lisp/nxml/rng-dt.el"
                   "vendor/emacs-lisp/nxml/rng-pttrn.el"
                   "vendor/emacs-lisp/nxml/nxml-enc.el"
                   "vendor/emacs-lisp/textmodes/page.el"
                   "vendor/emacs-lisp/reposition.el"
                   "vendor/emacs-lisp/emacs-lisp/inline.el"
                   "vendor/emacs-lisp/emacs-lisp/easymenu.el"
                   "vendor/emacs-lisp/emacs-lisp/let-alist.el"
                   "vendor/emacs-lisp/emacs-lisp/radix-tree.el"
                   "vendor/emacs-lisp/emacs-lisp/text-property-search.el"
                   "vendor/emacs-lisp/emacs-lisp/thunk.el"
                   "vendor/emacs-lisp/fileloop.el"
                   "vendor/emacs-lisp/emacs-lisp/rmc.el"
                   "vendor/emacs-lisp/obarray.el"
                   "vendor/emacs-lisp/soundex.el"
                   "vendor/emacs-lisp/emacs-lisp/cursor-sensor.el"
                   "vendor/emacs-lisp/indent-aux.el"
                   "vendor/emacs-lisp/display-fill-column-indicator.el"
                   "vendor/emacs-lisp/calendar/parse-time.el"
                   "vendor/emacs-lisp/calendar/iso8601.el"
                   "vendor/emacs-lisp/mail/mail-prsvr.el"
                   "vendor/emacs-lisp/gnus/mm-util.el"
                   "vendor/emacs-lisp/mail/rfc2047.el"
                   "vendor/emacs-lisp/mail/rfc2231.el"
                   "vendor/emacs-lisp/mail/rfc6068.el"
                   "vendor/emacs-lisp/international/rfc1843.el"
                   "vendor/emacs-lisp/url/url-parse.el"
                   "vendor/emacs-lisp/url/url-methods.el"
                   "vendor/emacs-lisp/url/url-proxy.el"
                   "vendor/emacs-lisp/url/url-misc.el"
                   "vendor/emacs-lisp/url/url-history.el"
                   "vendor/emacs-lisp/url/url-util.el"
                   "vendor/emacs-lisp/url/url-cookie.el"
                   "vendor/emacs-lisp/url/url-mailto.el"
                   "vendor/emacs-lisp/net/puny.el"
                   "vendor/emacs-lisp/mail/ietf-drums.el"
                   "vendor/emacs-lisp/mail/rfc2045.el"
                   "vendor/emacs-lisp/mail/mail-parse.el"
                   "vendor/emacs-lisp/emacs-lisp/generate-lisp-file.el"
                   "vendor/emacs-lisp/url/url-expand.el"
                   "vendor/emacs-lisp/net/mailcap.el"
                   "vendor/emacs-lisp/url/url.el"
                   "vendor/emacs-lisp/international/ucs-normalize.el"
                   "vendor/emacs-lisp/international/textsec.el"
                   "vendor/emacs-lisp/international/uni-scripts.el"
                   "vendor/emacs-lisp/net/hmac-def.el"
                   "vendor/emacs-lisp/net/hmac-md5.el"
                   "vendor/emacs-lisp/net/rfc2104.el"
                   "vendor/emacs-lisp/net/sasl.el"
                   "vendor/emacs-lisp/net/sasl-cram.el"
                   "vendor/emacs-lisp/net/sasl-digest.el"
                   "vendor/emacs-lisp/net/sasl-scram-rfc.el"
                   "vendor/emacs-lisp/net/sasl-scram-sha256.el"
                   "vendor/emacs-lisp/md4.el"
                   "vendor/emacs-lisp/net/ntlm.el"
                   "vendor/emacs-lisp/net/sasl-ntlm.el"
                   "vendor/emacs-lisp/mail/qp.el"
                   "vendor/emacs-lisp/mail/mailheader.el"
                   "vendor/emacs-lisp/mail/yenc.el"
                   "vendor/emacs-lisp/mail/flow-fill.el"
                   "vendor/emacs-lisp/mail/uudecode.el"
                   "vendor/emacs-lisp/emacs-lisp/compat.el"
                   "vendor/emacs-lisp/emacs-lisp/shorthands.el"
                   "vendor/emacs-lisp/dynamic-setting.el"
                   "vendor/emacs-lisp/emacs-lisp/benchmark.el"
                   "vendor/emacs-lisp/password-cache.el"
                   "vendor/emacs-lisp/scroll-lock.el"
                   "vendor/emacs-lisp/thread.el"
                   "vendor/emacs-lisp/tabify.el"
                   "vendor/emacs-lisp/rot13.el"
                   "vendor/emacs-lisp/textmodes/underline.el"
                   "vendor/emacs-lisp/widget.el"
                   "vendor/emacs-lisp/emacs-lisp/seq.el"
                   "vendor/emacs-lisp/emacs-lisp/map.el"
                   "vendor/emacs-lisp/emacs-lisp/ring.el"
                   "vendor/emacs-lisp/emacs-lisp/easy-mmode.el"
                   "vendor/emacs-lisp/emacs-lisp/derived.el"
                   "vendor/emacs-lisp/emacs-lisp/syntax.el"
                   "vendor/emacs-lisp/emacs-lisp/range.el"
                   "vendor/emacs-lisp/textmodes/paragraphs.el"
                   "vendor/emacs-lisp/textmodes/fill.el"
                   "vendor/emacs-lisp/textmodes/page-ext.el"
                   "vendor/emacs-lisp/emacs-lisp/tabulated-list.el"
                   "vendor/emacs-lisp/wid-edit.el"
                   "vendor/emacs-lisp/font-core.el"
                   "vendor/emacs-lisp/hl-line.el"
                   "vendor/emacs-lisp/newcomment.el"
                   "vendor/emacs-lisp/replace.el"
                   "vendor/emacs-lisp/sort.el"
                   "vendor/emacs-lisp/view.el"
                   "vendor/emacs-lisp/recentf.el"
                   "vendor/emacs-lisp/savehist.el"
                   "vendor/emacs-lisp/saveplace.el"
                   "vendor/emacs-lisp/calendar/time-date.el"
                   "vendor/emacs-lisp/ansi-color.el"
                   "vendor/emacs-lisp/tree-widget.el"
                   "vendor/emacs-lisp/bookmark.el"
                   "vendor/emacs-lisp/abbrev.el"
                   "vendor/emacs-lisp/dabbrev.el"
                   "vendor/emacs-lisp/completion.el"
                   "vendor/emacs-lisp/version.el"
                   "vendor/emacs-lisp/env.el"
                   "vendor/emacs-lisp/files-x.el"
                   "vendor/emacs-lisp/uniquify.el"
                   "vendor/emacs-lisp/minibuf-eldef.el"
                   "vendor/emacs-lisp/delsel.el"
                   "vendor/emacs-lisp/rfn-eshadow.el"
                   "vendor/emacs-lisp/format-spec.el"
                   "vendor/emacs-lisp/keymap.el"
                   "vendor/emacs-lisp/char-fold.el"
                   "vendor/emacs-lisp/imenu.el"
                   "vendor/emacs-lisp/help-fns.el"
                   "vendor/emacs-lisp/isearch.el"
                   "vendor/emacs-lisp/skeleton.el"
                   "vendor/emacs-lisp/rect.el"
                   "vendor/emacs-lisp/kmacro.el"
                   "vendor/emacs-lisp/double.el"
                   "vendor/emacs-lisp/mouse.el"
                   "vendor/emacs-lisp/menu-bar.el"
                   "vendor/emacs-lisp/facemenu.el"
                   "vendor/emacs-lisp/help-at-pt.el"
                   "vendor/emacs-lisp/find-cmd.el"
                   "vendor/emacs-lisp/ehelp.el"
                   "vendor/emacs-lisp/man.el"
                   "vendor/emacs-lisp/info-look.el"
                   "vendor/emacs-lisp/ibuf-macs.el"
                   "vendor/emacs-lisp/ebuff-menu.el"
                   "vendor/emacs-lisp/dirtrack.el"
                   "vendor/emacs-lisp/icomplete.el"
                   "vendor/emacs-lisp/ibuffer.el"
                   "vendor/emacs-lisp/comint.el"
                   "vendor/emacs-lisp/shell.el"))))
    (dolist (file loaded-files)
      (should (file-exists-p file)))
    (dolist (feature '(button thingatpt case-table help-macro
                              subr-x cdl regi generator avl-tree hex-util
                              array color composite disp-table
                              display-line-numbers delim-col descr-text
                              electric elec-pair chistory
                              charprop charscript
                              emoji-labels idna-mapping iso-transl
                              uni-confusable textsec-check kinsoku latexenc
                              utf-7
                              emacs-translation-table
                              ansi-osc glyphless-mode word-wrap-mode
                              url-vars url-future url-privacy url-domsuf
                              url-file url-auth url-ftp url-nfs url-gw
                              url-irc url-handlers
                              nxml-util rng-util rng-dt rng-pttrn nxml-enc
                              page reposition
                              inline easymenu let-alist radix-tree
                              text-property-search thunk fileloop rmc
                              obarray soundex cursor-sensor indent-aux
                              display-fill-column-indicator parse-time
                              iso8601
                              mail-prsvr mm-util rfc2047 rfc2231 rfc6068
                              rfc1843 url-parse url-methods url-proxy
                              url-misc url-history url-util url-cookie
                              url-mailto puny
                              ietf-drums rfc2045 mail-parse
                              generate-lisp-file url-expand mailcap url
                              ucs-normalize textsec uni-scripts
                              hmac-def hmac-md5
                              rfc2104 sasl sasl-plain sasl-login
                              sasl-anonymous sasl-cram sasl-digest
                              sasl-scram-rfc sasl-scram-sha-1
                              sasl-scram-sha256
                              md4 ntlm sasl-ntlm qp mailheader yenc flow-fill
                              uudecode
                              compat dynamic-setting benchmark
                              password-cache scroll-lock thread tabify
                              rot13 underline widget
                              syntax range
                              seq map ring easy-mmode derived time-date
                              page-ext tabulated-list
                              wid-edit font-core hl-line newcomment sort
                              replace view recentf savehist saveplace
                              ansi-color tree-widget bookmark abbrev
                              dabbrev completion env files-x
                              uniquify minibuf-eldef delsel rfn-eshadow
                              format-spec keymap char-fold imenu help-fns
                              isearch skeleton rect kmacro double mouse
                              menu-bar facemenu help-at-pt find-cmd ehelp man
                              info-look ibuf-macs ebuff-menu dirtrack
                              icomplete ibuffer comint shell))
      (should (featurep feature)))
    (dolist (symbol '(insert-button thing-at-point set-case-table
                                    make-help-screen syntax-ppss
                                    string-trim cdl-get-file regi-interpret
                                    iter-next avl-tree-enter
                                    encode-hex-string
                                    array-mode color-rgb-to-hex
                                    compose-string make-display-table
                                    display-line-numbers-mode
                                    delimit-columns-region
                                    describe-text-properties
                                    electric-indent-mode
                                    electric-pair-mode command-history
                                    forward-sexp mark-defun map-y-or-n-p
                                    read-answer iso-transl-define-keys
                                    iso-transl-set-language
                                    textsec-suspicious-p kinsoku
                                    latexenc-inputenc-to-coding-system
                                    utf-7-encode utf-7-decode
                                    make-translation-table
                                    ansi-osc-filter-region
                                    glyphless-display-mode
                                    word-wrap-whitespace-mode
                                    url-mime-charset-string
                                    url-future-finish url-device-type
                                    url-domsuf-cookie-allowed-p
                                    url-file-build-filename
                                    url-file-host-is-local-p
                                    url-get-authentication
                                    url-auth-registered
                                    url-ftp
                                    url-ftp-expand-file-name
                                    url-nfs
                                    url-nfs-build-filename
                                    url-open-stream
                                    url-gateway-nslookup-host
                                    url-irc
                                    url-ircs
                                    url-file-handler
                                    url-handler-expand-file-name
                                    nxml-make-namespace rng-escape-string
                                    rng-dt-builtin-compile rng-make-ref
                                    nxml-detect-coding-system
                                    forward-page reposition-window
                                    define-inline easy-menu-create-menu
                                    let-alist radix-tree-lookup
                                    text-property-search-forward
                                    thunk-force fileloop-initialize
                                    read-multiple-choice
                                    obarray-get soundex cursor-sensor-mode
                                    kill-ring-deindent-mode
                                    display-fill-column-indicator-mode
                                    parse-time-string iso8601-parse
                                    mm-charset-to-coding-system
                                    rfc2047-decode-string
                                    rfc2231-parse-string
                                    rfc6068-parse-mailto-url
                                    rfc1843-decode-string
                                    url-generic-parse-url
                                    url-scheme-get-property
                                    url-find-proxy-for-url
                                    url-data
                                    url-history-update-url
                                    url-hexify-string
                                    url-build-query-string
                                    url-domain
                                    url-cookie-store
                                    url-cookie-retrieve
                                    url-cookie-generate-header-lines
                                    url-mailto
                                    puny-encode-domain
                                    ietf-drums-strip rfc2045-encode-string
                                    mail-header-parse
                                    generate-lisp-file-trailer
                                    url-expand-file-name
                                    mailcap-parse-mailcaps
                                    url-retrieve
                                    ucs-normalize-NFC-string
                                    textsec-scripts
                                    hmac-md5 rfc2104-hash md4
                                    sasl-make-client
                                    sasl-find-mechanism
                                    sasl-cram-md5-response
                                    sasl-digest-md5-parse-string
                                    sasl-digest-md5-digest-uri
                                    sasl-scram-client-first-message
                                    sasl-scram-sha256
                                    sasl-scram-sha-256-client-final-message
                                    ntlm-build-auth-request
                                    ntlm-get-password-hashes
                                    sasl-ntlm-request
                                    quoted-printable-encode-string
                                    mail-header-parse yenc-parse-line
                                    fill-flowed uudecode-decode-region
                                    compat-function
                                    hack-read-symbol-shorthands
                                    dynamic-setting-handle-config-changed-event
                                    benchmark-call password-read-from-cache
                                    scroll-lock-mode list-threads
                                    untabify rot13-string underline-region
                                    define-widget
                                    seq-filter map-elt make-ring
                                    define-minor-mode define-derived-mode
                                    forward-paragraph fill-region
                                    pages-directory tabulated-list-mode
                                    widget-create font-lock-default-fontify-region
                                    global-hl-line-mode comment-dwim sort-lines
                                    query-replace view-mode recentf-mode
                                    savehist-mode save-place-mode
                                    range-normalize time-to-seconds
                                    ansi-color-apply tree-widget-set-theme
                                    bookmark-set bookmark-jump
                                    define-abbrev dabbrev-expand
                                    completion-in-region-mode emacs-version
                                    substitute-env-vars
                                    add-file-local-variable
                                    uniquify-buffer-base-name
                                    minibuffer-electric-default-mode
                                    delete-selection-mode
                                    file-name-shadow-mode
                                    format-spec keymap-set
                                    char-fold-to-regexp imenu
                                    describe-function isearch-forward
                                    skeleton-insert rectangle-mark-mode
                                    kmacro-start-macro double-mode
                                    mouse-set-point menu-bar-mode
                                    facemenu-update facemenu-add-new-face
                                    help-at-pt-string find-cmd
                                    electric-help-mode man
                                    info-lookup-symbol
                                    electric-buffer-list dirtrack-mode
                                    icomplete-mode ibuffer
                                    comint-mode comint-run comint-send-input
                                    shell))
      (should (fboundp symbol)))
    (dolist (symbol '(uni-confusable-table ansi-osc-control-seq-regexp
                                     kinsoku-limit mail-parse-charset))
      (should (boundp symbol)))
    (should (equal (get-char-code-property ?a 'uppercase) ?A))
    (should (equal (get-char-code-property ?A 'lowercase) ?a))
    (should (equal (get-char-code-property ?a 'titlecase) ?A))
    (should (eq (get-char-code-property ?A 'bidi-class) 'L))
    (should (eq (get-char-code-property ?A 'general-category) 'Lu))
    (should (equal (get-char-code-property ?A 'decomposition) (list ?A)))
    (should (equal (get-char-code-property ?0 'decimal-digit-value) 0))
    (should (equal (get-char-code-property ?0 'digit-value) 0))
    (should (equal (get-char-code-property ?0 'numeric-value) 0))
    (should (equal (get-char-code-property ?A 'name) "LATIN CAPITAL LETTER A"))
    (should (equal (get-char-code-property ?0 'name) "DIGIT ZERO"))
    (should (equal (rfc6068-parse-mailto-url "mailto:a@example.test?subject=Hi")
                   '(("To" . "a@example.test") ("Subject" . "Hi"))))
    (should (equal (sasl-mechanism-name (get 'sasl-cram 'sasl-mechanism))
                   "CRAM-MD5"))
    (should (equal (sasl-mechanism-name (get 'sasl-digest 'sasl-mechanism))
                   "DIGEST-MD5"))
    (should (equal (sasl-mechanism-name (get 'sasl-scram-sha256 'sasl-mechanism))
                   "SCRAM-SHA-256"))
    (should (equal (sasl-mechanism-name (get 'sasl-ntlm 'sasl-mechanism))
                   "NTLM"))
    (should (string-prefix-p "NTLMSSP" (ntlm-build-auth-request "user@example")))
    (should (equal (get-char-code-property ?ß 'special-uppercase) "SS"))))

(ert-deftest vendor-first-core-modes-test/eieio-basic-subclass-after-vendor-load ()
  "Vendor EIEIO should support the small subclass shape Magit later needs."
  (let ((load-path (append (vendor-first-core-modes-test--vendor-load-path)
                           load-path)))
    (vendor-first-core-modes-test--load-source
     "vendor/emacs-lisp/emacs-lisp/eieio.el")
    (defclass vendor-first-core-modes-test-parent nil
      ((name :initform nil)
       (value :initform nil :initarg :value)))
    (defclass vendor-first-core-modes-test-child
      (vendor-first-core-modes-test-parent)
      ((keymap :initform nil)))
    (defclass vendor-first-core-modes-test-override-child
      (vendor-first-core-modes-test-child)
      ((keymap :initform 'vendor-first-core-modes-test-map)))
    (defvar vendor-first-core-modes-test-section-type-alist nil)
    (defclass vendor-first-core-modes-test-abstract-child
      (vendor-first-core-modes-test-parent)
      ((keymap :initform 'vendor-first-core-modes-test-abstract-map))
      :abstract t)
    (defclass vendor-first-core-modes-test-file-child
      (vendor-first-core-modes-test-abstract-child)
      ((keymap :initform 'vendor-first-core-modes-test-file-map)
       (source :initform nil :initarg :source)
       (header :initform nil :initarg :header)
       (binary :initform nil :initarg :binary)
       (heading-highlight-face :initform 'vendor-first-core-modes-test-highlight)
       (heading-selection-face :initform 'vendor-first-core-modes-test-selection)))
    (defclass vendor-first-core-modes-test-module-child
      (vendor-first-core-modes-test-file-child)
      ((keymap :initform 'vendor-first-core-modes-test-module-map)
       (range :initform nil :initarg :range)))
    (defclass vendor-first-core-modes-test-hunk-child
      (vendor-first-core-modes-test-abstract-child)
      ((keymap :initform 'vendor-first-core-modes-test-hunk-map)
       (painted :initform nil)
       (fontified :initform nil)
       (refined :initform nil)
       (combined :initform nil :initarg :combined)
       (from-range :initform nil :initarg :from-range)
       (from-ranges :initform nil)
       (to-range :initform nil :initarg :to-range)
       (about :initform nil :initarg :about)
       (heading-highlight-face :initform 'vendor-first-core-modes-test-hunk-highlight)
       (heading-selection-face :initform 'vendor-first-core-modes-test-hunk-selection)))
    (defun vendor-first-core-modes-test--meta-hunk-p (section)
      (not (cdr (oref section value))))
    (defun vendor-first-core-modes-test--completion-table (collection)
      (lambda (string pred action)
        (if (eq action 'metadata)
            '(metadata (display-sort-function . identity))
          (complete-with-action action collection string pred))))
    (defclass vendor-first-core-modes-test-log-child
      (vendor-first-core-modes-test-parent)
      ((keymap :initform 'vendor-first-core-modes-test-log-map))
      :abstract t)
    (defclass vendor-first-core-modes-test-unpulled-child
      (vendor-first-core-modes-test-log-child)
      nil)
    (defclass vendor-first-core-modes-test-unpushed-child
      (vendor-first-core-modes-test-log-child)
      nil)
    (defclass vendor-first-core-modes-test-unmerged-child
      (vendor-first-core-modes-test-log-child)
      nil)
    (setf (alist-get 'abstract vendor-first-core-modes-test-section-type-alist)
          'vendor-first-core-modes-test-abstract-child)
    (setf (alist-get 'hunk vendor-first-core-modes-test-section-type-alist)
          'vendor-first-core-modes-test-hunk-child)
    (setf (alist-get 'unpulled vendor-first-core-modes-test-section-type-alist)
          'vendor-first-core-modes-test-unpulled-child)
    (setf (alist-get 'unmerged vendor-first-core-modes-test-section-type-alist)
          'vendor-first-core-modes-test-unmerged-child)
    (defclass vendor-first-core-modes-test-transient-like-child
      (vendor-first-core-modes-test-parent)
      ((reader :initform #'vendor-first-core-modes-test--vendor-load-path)
       (always-read :initform t)
       (set-value :initarg :set-value :initform #'set)))
    (should (cl--find-class 'vendor-first-core-modes-test-parent))
    (should (cl--find-class 'vendor-first-core-modes-test-child))
    (should (cl--find-class 'vendor-first-core-modes-test-override-child))
    (should (cl--find-class 'vendor-first-core-modes-test-abstract-child))
    (should (eq (alist-get 'abstract
                           vendor-first-core-modes-test-section-type-alist)
                'vendor-first-core-modes-test-abstract-child))
    (should (cl--find-class 'vendor-first-core-modes-test-file-child))
    (should (cl--find-class 'vendor-first-core-modes-test-module-child))
    (should (cl--find-class 'vendor-first-core-modes-test-hunk-child))
    (should (cl--find-class 'vendor-first-core-modes-test-unmerged-child))
    (should (eq (alist-get 'hunk vendor-first-core-modes-test-section-type-alist)
                'vendor-first-core-modes-test-hunk-child))
    (should (eq (alist-get 'unmerged
                           vendor-first-core-modes-test-section-type-alist)
                'vendor-first-core-modes-test-unmerged-child))
    (should (equal (funcall (vendor-first-core-modes-test--completion-table
                             nil)
                            "" nil 'metadata)
                   '(metadata (display-sort-function . identity))))
    (should (cl--find-class 'vendor-first-core-modes-test-transient-like-child))
    (should (object-of-class-p
             (make-instance 'vendor-first-core-modes-test-child)
             'vendor-first-core-modes-test-parent))
    (should (object-of-class-p
             (make-instance 'vendor-first-core-modes-test-override-child)
             'vendor-first-core-modes-test-child))
    (let ((object (make-instance
                   'vendor-first-core-modes-test-transient-like-child
                   :set-value #'ignore)))
      (should (object-of-class-p
               object 'vendor-first-core-modes-test-parent))
      (should (eq (slot-value object 'reader)
                  #'vendor-first-core-modes-test--vendor-load-path))
      (should (eq (slot-value object 'always-read) t))
      (should (eq (slot-value object 'set-value) #'ignore)))
    (let ((object (make-instance
                   'vendor-first-core-modes-test-module-child
                   :source 'source
                   :range 'range)))
      (should (object-of-class-p
               object 'vendor-first-core-modes-test-file-child))
      (should (eq (slot-value object 'source) 'source))
      (should (eq (slot-value object 'range) 'range)))
    (let ((object (make-instance
                   'vendor-first-core-modes-test-hunk-child
                   :combined t
                   :from-range 'from
                   :to-range 'to
                   :about 'about
                   :value '(meta))))
      (should (object-of-class-p
               object 'vendor-first-core-modes-test-abstract-child))
      (should (eq (slot-value object 'combined) t))
      (should (eq (slot-value object 'from-range) 'from))
      (should (eq (slot-value object 'to-range) 'to))
      (should (eq (slot-value object 'about) 'about))
      (should (vendor-first-core-modes-test--meta-hunk-p object)))))

(ert-deftest vendor-first-core-modes-test/text-and-outline-buffer-behavior ()
  "Exercise vendored text/outline code through real buffer primitives."
  (let ((load-path (append (vendor-first-core-modes-test--vendor-load-path)
                           load-path)))
    (vendor-first-core-modes-test--load-source
     "vendor/emacs-lisp/textmodes/text-mode.el")
    (vendor-first-core-modes-test--load-source
     "vendor/emacs-lisp/outline.el")
    (with-temp-buffer
      (insert "* Root\nbody\n** Child\nleaf\n")
      (goto-char (point-min))
      (text-mode)
      (outline-minor-mode 1)
      (setq-local outline-regexp "\\*+")
      (should (outline-on-heading-p))
      (should (= 1 (outline-level)))
      (outline-next-heading)
      (should (= 2 (outline-level))))))

(provide 'vendor-first-core-modes-test)

;;; vendor-first-core-modes-test.el ends here
