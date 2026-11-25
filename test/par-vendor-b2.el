;;; par-vendor-b2.el --- GNU 31.1 vendor boundary parity -*- lexical-binding: t; -*-

(defmacro par-vendor-b2--row (label &rest body)
  `(progn
     (prin1 (cons ',label (condition-case err (progn ,@body)
                           (error (list 'error (car err))))))
     (terpri)))

(defun par-vendor-b2--owner (name filename &optional type)
  (let ((file (symbol-file name (or type 'defun))))
    (and file (equal (file-name-base file) filename)
         (or (not (fboundp 'nemacs-runtime-dump-reinit))
             (string-match-p "/vendor/emacs-lisp/" file))
         t)))

(par-vendor-b2--row eshell
  (require 'eshell)
  (let ((eshell-module-loading-messages nil))
    (list (par-vendor-b2--owner 'eshell-command-result "eshell")
          (eshell-command-result "echo hello")
          (eshell-command-result "echo 5")
          (par-vendor-b2--owner 'eshell/echo "em-basic"))))

(par-vendor-b2--row calc
  (require 'calc)
  (list (par-vendor-b2--owner 'calc-eval "calc")
        (calc-eval "2+3")
        (par-vendor-b2--owner 'calc-init-extensions "calc-ext")
        (char-table-p (nth 1 calc-mode-map))
        (lookup-key calc-mode-map ":")
        calc-embedded-open-mode))

(par-vendor-b2--row cc-fonts
  (require 'cc-fonts)
  (list (par-vendor-b2--owner 'c-font-lock-declarations "cc-fonts")
        (with-temp-buffer
          (c-mode)
          (c-font-lock-declarations (point-max)))))

(par-vendor-b2--row menu-bar
  (require 'menu-bar)
  (let ((map (make-sparse-keymap)))
    (define-key map [file] '(menu-item "File" ignore))
    (list (par-vendor-b2--owner 'menu-bar-keymap "menu-bar")
          (keymapp (menu-bar-keymap map))
          (lookup-key (menu-bar-keymap map) [file])
          (lookup-key menu-bar-edit-menu [execute-extended-command]))))

(par-vendor-b2--row startup
  (let ((window-system nil))
    (list (par-vendor-b2--owner 'tty-handle-args "startup")
          (not (eq (symbol-function 'handle-args-function) 'tty-handle-args))
          (handle-args-function '("sample.txt" "--" "-fg" "green")))))

(par-vendor-b2--row ja-dic-utl
  (require 'ja-dic-utl)
  (list (par-vendor-b2--owner 'skkdic-merge-head-and-tail "ja-dic-utl")
        (skkdic-merge-head-and-tail '("a" "bc") '("d" "ef") t)
        (car skkdic-jisx0208-hiragana-block)))

(par-vendor-b2--row translations
  (require 'cp51932)
  (require 'eucjp-ms)
  (list (mapcar (lambda (feature)
                  (let ((file (catch 'owner
                                (dolist (entry load-history)
                                  (when (member (cons 'provide feature) (cdr entry))
                                    (throw 'owner (car entry)))))))
                    (and file (file-name-base file))))
                '(cp51932 eucjp-ms))
        (let ((char (decode-char 'japanese-jisx0208 #x2d21)))
          (mapcar (lambda (name)
                    (let ((table (get name 'translation-table)))
                      (list (char-table-subtype table)
                            (char-table-range table char))))
                  '(cp51932-decode eucjp-ms-decode)))))

(par-vendor-b2--row mule-properties
  (list (mapcar (lambda (symbol)
                  (par-vendor-b2--owner symbol "mule-cmds"))
                '(get-char-code-property put-char-code-property
                  char-code-property-description))
        (get-char-code-property ?A 'general-category)
        (put-char-code-property ?A 'par-vendor-b2 'test-value)
        (get-char-code-property ?A 'par-vendor-b2)
        (char-code-property-description 'general-category 'Lu)))

(par-vendor-b2--row dense-undefined
  (let ((map (make-keymap)) (parent (make-sparse-keymap)))
    (define-key parent "A" 'inherited)
    (set-keymap-parent map parent)
    (define-key map "A" nil)
    (define-key map [button-test] 'button-command)
    (list (char-table-p (nth 1 map))
          (aref (nth 1 map) ?A)
          (lookup-key map "A")
          (progn (define-key map "A" nil t) (lookup-key map "A")))))

(par-vendor-b2--row reverse-keymap
  (let ((low (make-keymap)) (high (make-sparse-keymap)))
    (define-key low [remap par-b2-old] 'par-b2-command)
    (define-key low "a" 'par-b2-old)
    (define-key low "b" 'par-b2-shadowed)
    (define-key low [menu-bar entry] '(menu-item "Entry" par-b2-command))
    (define-key high "b" 'par-b2-command)
    (define-key high (kbd "M-c") 'par-b2-command)
    (list (where-is-internal 'par-b2-command (list high low))
          (where-is-internal 'par-b2-command (list high low) t)
          (where-is-internal 'par-b2-command (list high low) nil nil t)
          (where-is-internal 'par-b2-shadowed (list high low))
          (progn (define-key high "b" 'ignore)
                 (where-is-internal 'par-b2-command (list high low) t)))))

(par-vendor-b2--row menu-filter
  (let ((map (make-sparse-keymap)))
    (define-key map [menu-bar entry]
      '(menu-item "Example" par-b2-original
                  :filter (lambda (_) 'par-b2-filtered)))
    (list (lookup-key map [menu-bar entry])
          (where-is-internal 'par-b2-filtered (list map))
          (where-is-internal 'par-b2-original (list map)))))

(par-vendor-b2--row substitute-command-keys
  (let ((par-vendor-b2--map (make-sparse-keymap)))
    (define-key par-vendor-b2--map (kbd "M-a") 'par-b2-command)
    (let ((overriding-local-map par-vendor-b2--map)
          (text-quoting-style 'straight))
      (substitute-command-keys
       "\\[par-b2-command] \\[par-b2-absent] \\=\\[literal] `word' \\`C-f'" t))))

(par-vendor-b2--row vc-backends
  (unless (fboundp 'nemacs-runtime-dump-reinit)
    (require 'vc-git)
    (require 'vc-hg))
  (let ((root (file-name-as-directory (make-temp-file "par-vendor-b2-vc-" t)))
        (vc-ignore-dir-regexp "/unlikely-par-vendor-b2-ignore/"))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" root))
          (make-directory (expand-file-name ".hg" root))
          (let ((git-root (vc-git-root root))
                (hg-root (vc-hg-root root)))
            (list (par-vendor-b2--owner 'vc-git-root "vc-git")
                  (par-vendor-b2--owner 'vc-hg-root "vc-hg")
                  (and git-root (equal (expand-file-name git-root) root))
                  (and hg-root (equal (expand-file-name hg-root) root)))))
      (delete-directory root t))))
