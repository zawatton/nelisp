;;; nelisp-vendor-source.el --- pinned GNU source-form provider -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Return exact top-level forms from the repository's pinned GNU Emacs
;; sources. Both the source evaluator and standalone build use this selector.

;;; Code:

(defconst nelisp-vendor-source--root
  (expand-file-name ".." (file-name-directory load-file-name)))

(defconst nelisp-vendor-source--pins
  '(("vendor/staged-emacs-lisp/subr.el"
     . "410c34e030bdd667ff21842a8513e41100394662dacf5e54b40938106f0d6327")
    ("vendor/staged-emacs-lisp/simple.el"
     . "c19d208e61100fc9ff692cef0c95d895e54e17fc1f9031500ac8474c9c2e48f2")
    ("vendor/staged-emacs-lisp/files.el"
     . "00b8b0ee9e718a34bc8b9ca04526d8ce7449fb3ebf613bef28e4ca01b0a7ad9e")
    ("vendor/staged-emacs-lisp/macroexp.el"
     . "716a3ac7bd9756ad901f4307075cc4bd0e371248ab94c7a200962c7a1e2614e3")
    ("vendor/staged-emacs-lisp/bindings.el"
     . "479dd97f6b78f644f036b8279a2594da7913f404c71f34451a26ed3642342b3b")
    ("vendor/staged-emacs-lisp/cl-macs.el"
     . "f58f3f9b88755549a7713b59045059affda99c2e22251e22e5c40fbe017f2394")
    ("vendor/staged-emacs-lisp/cl-preloaded.el"
     . "73c114b7a8899ae278fdd1eae29c902703f20bc0662f99c3edb391362a35514c")
    ("vendor/staged-emacs-lisp/custom.el"
     . "363b7f408c88c1788aa53f35a15ef46c07ad86fb20a9d110f381b0cd0e85a5c0")
    ("vendor/staged-emacs-lisp/cus-face.el"
     . "fe8718def781adfdbd3b8b87d9d30f8715827e1d264b854802ed8182625cd892")
    ("vendor/staged-emacs-lisp/faces.el"
     . "d6464ee14c8c6e2b6146cecce9bdfb6513ced36fd6ebac172fc84e00be9d22f3")
    ("vendor/staged-emacs-lisp/keymap.el"
     . "77337ec8988f4279b9d08a12cbb7af84bd1113e97c2807e63c76c8175475ac11")
    ("vendor/emacs-lisp/ansi-osc.el"
     . "75df0e972ace2f9ca255c8d88ee917398f51b702dccba1afca5eda809deb9f61")
    ("vendor/emacs-lisp/button.el"
     . "8316efddb342a6ed9473475e5db69f61a52cc63587985d7cad03345f6c27676f")
    ("vendor/emacs-lisp/menu-bar.el"
     . "cfcbfb991eaea631b66b79f0e4419318703eb9f52006db5d4962fd29562d49d4")
    ("vendor/emacs-lisp/emacs-lisp/regexp-opt.el"
     . "90e9e70c583c3b18036353b593e22bcd5510b32ce017dcb27acd6ce22ec449ae")
    ("vendor/emacs-lisp/emacs-lisp/subr-x.el"
     . "b1e7797646f6af8c30fe506b4f098e79a9568610ae6d3e8e78dfaa690042456b")
    ("vendor/emacs-lisp/emacs-lisp/byte-run.el"
     . "cbac268cd6cc2f9ca2fefd28e7888a3dfa72896514d2652c7ef5addb4c5b73f7")
    ("vendor/emacs-lisp/international/mule-conf.el"
     . "c706015365dc9763d26c10ba487ba014517b58eb08e2d4101107a3f094119f94"))
  "SHA-256 pins for exact GNU Emacs 31.1 source providers.")

(defvar nelisp-vendor-source--cache (make-hash-table :test #'equal)
  "Cache decoded source and exact top-level forms, keyed by relative file.")

(defun nelisp-vendor-source--form-name (form)
  "Return the static function name defined or aliased by FORM.
For `defalias' and single-variable `setq', index only a literal symbol name."
  (pcase (car-safe form)
    ((or 'defun 'defsubst 'defmacro 'defvar 'defconst 'defcustom)
     (and (symbolp (cadr form)) (cadr form)))
    ('cl-defstruct
     (let ((name (cadr form)))
       (cond
        ((symbolp name) name)
        ((and (consp name) (symbolp (car name))) (car name)))))
    ('defalias
     (let ((name (cadr form)))
       (cond
        ((symbolp name) name)
        ((and (consp name) (eq (car name) 'quote)
              (symbolp (cadr name)))
         (cadr name)))))
    ('setq
     (and (= (length form) 3)
          (symbolp (cadr form))
          (cadr form)))))

(defun nelisp-vendor-source--definitions (relative-file)
  "Return (SOURCE . DEFINITIONS) for pinned RELATIVE-FILE, cached once."
  (or (gethash relative-file nelisp-vendor-source--cache)
      (let* ((expected (cdr (assoc relative-file nelisp-vendor-source--pins)))
             (file (expand-file-name relative-file nelisp-vendor-source--root)))
        (unless expected
          (error "No GNU source hash pinned for %s" relative-file))
        (let* ((digest (with-temp-buffer
                         (insert-file-contents-literally file)
                         (secure-hash 'sha256 (current-buffer))))
               (source (with-temp-buffer
                         (insert-file-contents file)
                         (buffer-string)))
               (length (length source))
               (position 0)
               (definitions (make-hash-table :test #'eq)))
          (unless (equal digest expected)
            (error "Vendored %s SHA-256 mismatch: %s" relative-file digest))
          (while (< position length)
            (let ((skipping t))
              (while (and skipping (< position length))
                (let ((char (aref source position)))
                  (cond
                   ((memq char '(9 10 12 13 32))
                    (setq position (1+ position)))
                   ((= char 59)
                    (while (and (< position length)
                                (/= (aref source position) 10))
                      (setq position (1+ position))))
                   (t (setq skipping nil))))))
            (when (< position length)
              (let* ((start position)
                     (parsed (read-from-string source position))
                     (form (car parsed))
                     (end (cdr parsed)))
                (let ((name (nelisp-vendor-source--form-name form)))
                  (when name
                    (puthash name
                             (cons (cons start (substring source start end))
                                   (gethash name definitions))
                             definitions)))
                (setq position end))))
          (let ((result (cons source definitions)))
            (puthash relative-file result nelisp-vendor-source--cache)
            result)))))

(defun nelisp-vendor-source-form (relative-file symbol)
  "Return exact pinned source text for SYMBOL in RELATIVE-FILE.
Fail closed unless the whole file matches its recorded SHA-256 and SYMBOL
occurs in exactly one recognized top-level definition or assignment."
  (let* ((entries (gethash symbol
                           (cdr (nelisp-vendor-source--definitions relative-file)))))
    (unless (= (length entries) 1)
      (error "Expected one top-level form %S in %s; found %d"
             symbol relative-file (length entries)))
    (cdar entries)))

(defun nelisp-vendor-source-whole-file (relative-file)
  "Return the full pinned source text of RELATIVE-FILE.
Gives the same fail-closed SHA-256 guarantee as `nelisp-vendor-source-form',
for a consumer that wants the entire file rather than one extracted form."
  (car (nelisp-vendor-source--definitions relative-file)))

(defun nelisp-vendor-source-forms (relative-file symbols)
  "Return exact forms for SYMBOLS in their order in RELATIVE-FILE."
  (let* ((definitions (cdr (nelisp-vendor-source--definitions relative-file)))
         (forms (mapcar (lambda (symbol)
                          (let ((entries (gethash symbol definitions)))
                            (unless (= (length entries) 1)
                              (error "Expected one top-level form %S in %s; found %d"
                                     symbol relative-file (length entries)))
                            (car entries)))
                        symbols))
         (ordered (sort forms (lambda (a b) (< (car a) (car b))))))
    (mapconcat #'cdr ordered "\n")))

(defun nelisp-vendor-source-form-position (relative-file symbol)
  "Return SYMBOL's source position in RELATIVE-FILE."
  (car (car (gethash symbol
                     (cdr (nelisp-vendor-source--definitions relative-file))))))

(provide 'nelisp-vendor-source)

;;; nelisp-vendor-source.el ends here
