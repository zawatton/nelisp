;;; par-vendor-a.el --- GNU vendor Batch A behavior and provenance -*- lexical-binding: t; -*-

;; The NeLisp side tests the existing image without loading any missing
;; library; otherwise the baseline could load the same source as the fix.
(unless (fboundp 'nemacs-runtime-dump-reinit)
  (dolist (feature '(hex-util range thunk regi mule-util lisp-mode
                    elisp-mode imenu compile vc help-mode idna-mapping
                    emoji-labels))
    (require feature))
  (load "map-ynp" nil t)
  (require 'cdl))

(defun par-vendor-a-owner-p (file symbol &optional kind)
  (let ((owner (symbol-file symbol (or kind 'defun))))
    (and (stringp owner)
         (or (eq kind 'defvar)
             (and (fboundp symbol)
                  (not (autoloadp (symbol-function symbol)))))
         (or (string= (file-name-nondirectory owner) (concat file ".el"))
             (string= (file-name-nondirectory owner) (concat file ".elc")))
         (or (not (fboundp 'nemacs-runtime-dump-reinit))
             (string-match-p "/vendor/emacs-lisp/" owner))
         t)))

(defmacro par-vendor-a-row (name owner &rest body)
  (declare (indent 2))
  `(progn
     (prin1 (list ',name (par-vendor-a-owner-p ,@owner)
                  (condition-case err
                      (progn ,@body)
                    (error (cons 'error err)))))
     (terpri)))

(par-vendor-a-row hex-util ("hex-util" 'decode-hex-string)
  (list (encode-hex-string (unibyte-string 0 65 255))
        (string-to-list (decode-hex-string "00aBff"))
        (condition-case err (decode-hex-string "G0") (error (car err)))))

(par-vendor-a-row map-ynp ("map-ynp" 'map-y-or-n-p)
  (let ((use-dialog-box nil)
        seen)
    (list (map-y-or-n-p (lambda (n) (zerop (% n 2)))
                        (lambda (n) (push n seen))
                        '(1 2 3 4))
          (nreverse seen)
          (par-vendor-a-owner-p "map-ynp" 'read-answer))))

(par-vendor-a-row range ("range" 'range-concat)
  (list (range-compress-list '(1 2 4 6 7 8))
        (range-intersection '(4 . 8) '(2 . 5))
        (range-uncompress (range-concat '((1 . 2)) '(3 5)))
        (range-member-p 6 '((4 . 8)))))

(par-vendor-a-row thunk ("thunk" 'thunk-evaluated-p)
  (let* ((calls 0)
         (delayed (thunk-delay (cl-incf calls) :value)))
    (list (thunk-evaluated-p delayed)
          (thunk-force delayed)
          (thunk-force delayed)
          (thunk-evaluated-p delayed)
          calls)))

(par-vendor-a-row regi ("regi" 'regi-pos)
  (with-temp-buffer
    (insert "  alpha\nbeta\n")
    (goto-char (point-min))
    (list (regi-pos 'boi t) (regi-pos 'bonl)
          (regi-mapcar '("alpha" "beta") 'ignore nil t))))

(par-vendor-a-row mule-util ("mule-util" 'store-substring)
  (let ((text (copy-sequence "abcdef")))
    (store-substring text 1 "XY")
    (store-substring text 4 ?Z)
    (list text (truncate-string-to-width "abcdef" 4 1 nil))))

(defvar native-comp-enable-subr-trampolines)

(par-vendor-a-row cdl ("cdl" 'cdl-get-file)
  (let ((native-comp-enable-subr-trampolines nil)
        (call (symbol-function 'call-process))
        (call-region (symbol-function 'call-process-region))
        get-args put-args)
    (unwind-protect
        (with-temp-buffer
          (fset 'call-process-region
                (lambda (&rest args) (setq put-args args)))
          (fset 'call-process
                (lambda (&rest args) (setq get-args args) (insert "CDF")))
          (cdl-get-file "/tmp/par-vendor-a.cdf")
          (cdl-put-region "/tmp/par-vendor-a.cdf" 1 4)
          (list (buffer-string) (point) get-args put-args
                (par-vendor-a-owner-p "cdl" 'cdl-put-region)))
      (fset 'call-process call)
      (fset 'call-process-region call-region))))

(par-vendor-a-row lisp-mode ("lisp-mode" 'lisp-mode)
  (with-temp-buffer
    (lisp-mode)
    (list major-mode indent-line-function
          (char-syntax ?\;) (char-syntax ?\()
          (par-vendor-a-owner-p "lisp-mode" 'lisp-mode-variables))))

(par-vendor-a-row elisp-mode ("elisp-mode" 'emacs-lisp-mode)
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(+ 10 20)")
    (list major-mode indent-line-function
          (elisp--preceding-sexp)
          (par-vendor-a-owner-p "elisp-mode" 'elisp-mode-syntax-propertize))))

(par-vendor-a-row imenu ("imenu" 'imenu)
  (with-temp-buffer
    (insert "one\ntwo\n")
    (imenu-default-goto-function "two" 5)
    (list (point) (imenu-find-default "test" '(("other" . 1) ("test" . 5)))
          (par-vendor-a-owner-p "imenu" 'imenu-default-goto-function))))

(par-vendor-a-row compile ("compile" 'compile)
  (list (compilation-buffer-name "Review" 'compilation-mode nil)
        (compilation-buffer-name "Unused" 'compilation-mode
                                 (lambda (mode) (concat mode "-test")))
        (par-vendor-a-owner-p "compile" 'compilation-start)
        (get 'compilation-start 'standalone-source-elided-body)))

(par-vendor-a-row vc ("vc" 'vc-diff)
  (let ((vc-clone-heuristic-alist '(("\\.git\\'" . Git)))
        (vc-diff-switches "-u"))
    (list (vc-guess-url-backend "https://example.invalid/demo.git")
          (vc-guess-url-backend "https://example.invalid/demo")
          (vc-switches nil 'diff)
          (par-vendor-a-owner-p "vc" 'vc-switches))))

(par-vendor-a-row help-mode ("help-mode" 'help-mode)
  (with-temp-buffer
    (help-mode)
    (let ((help-xref-following t))
      (list major-mode (equal (help-buffer) (buffer-name))
            (functionp (button-type-get 'help-function-def 'help-function))
            (par-vendor-a-owner-p "help-mode" 'help-go-back)))))

;; These two GNU libraries publish data rather than functions.
(par-vendor-a-row idna-mapping ("idna-mapping" 'idna-mapping-table 'defvar)
  (list (elt idna-mapping-table ?A)
        (elt idna-mapping-table #x00ad)
        (elt idna-mapping-table #x212a)
        (elt idna-mapping-table #xff3a)))

(par-vendor-a-row emoji-labels ("emoji-labels" 'emoji--names 'defvar)
  (list (caar emoji--labels)
        (gethash (string #x1f603) emoji--names)
        (length (gethash (string #x1f44b) emoji--derived))
        (par-vendor-a-owner-p "emoji-labels" 'emoji--labels 'defvar)))
