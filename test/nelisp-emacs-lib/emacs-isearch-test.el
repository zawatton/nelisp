;;; emacs-isearch-test.el --- ERT for emacs-isearch -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emacs-isearch)

(defmacro emacs-isearch-test--with-fresh-world (&rest body)
  "Run BODY with clean nelisp, keymap, face, and minibuffer state."
  (declare (indent 0) (debug (body)))
  `(let ((nelisp-ec--buffers nil)
         (nelisp-ec--current-buffer nil)
         (nelisp-ec--match-data nil)
         (emacs-buffer--state (make-hash-table :test 'eq))
         (emacs-buffer--variable-buffer-local nil)
         (emacs-buffer--default-values (make-hash-table :test 'eq))
         (emacs-keymap-global-map (emacs-keymap-make-sparse-keymap))
         (emacs-keymap-local-map nil)
         (emacs-keymap--input-queue nil)
         (emacs-keymap--read-event-fn nil)
         (emacs-minibuffer--depth 0)
         (emacs-minibuffer--buffers nil)
         (emacs-minibuffer--prompts nil)
         (emacs-minibuffer--prompt-ends nil)
         (emacs-minibuffer--window nil)
         (emacs-minibuffer--saved-window nil)
         (emacs-minibuffer--input-queue nil)
         (emacs-minibuffer--read-fn nil)
         (emacs-minibuffer--key-fn nil)
         (emacs-minibuffer--y-or-n-fn nil)
         (emacs-minibuffer-history nil)
         (emacs-minibuffer-default nil)
         (minibuffer-completion-table nil)
         (minibuffer-completion-confirm nil)
         (emacs-redisplay--face-registry (make-hash-table :test 'eq))
         (emacs-redisplay--face-cache (make-hash-table :test 'equal)))
     (emacs-isearch-reset)
     (emacs-isearch--ensure-face)
     (emacs-isearch--ensure-global-bindings)
     ,@body))

(defun emacs-isearch-test--make-buffer (text &optional point)
  "Create a nelisp buffer with TEXT and optional POINT."
  (let ((buf (nelisp-ec-generate-new-buffer " *isearch-test*")))
    (nelisp-ec-with-current-buffer buf
      (nelisp-ec-insert text)
      (nelisp-ec-goto-char (or point (nelisp-ec-point-min))))
    buf))

(defun emacs-isearch-test--point (buffer)
  "Return BUFFER's point."
  (nelisp-ec-with-current-buffer buffer
    (nelisp-ec-point)))

(defun emacs-isearch-test--run (fn text point &rest events)
  "Run FN in a fresh nelisp buffer containing TEXT at POINT.
EVENTS are fed to the minibuffer key reader."
  (let ((buf (emacs-isearch-test--make-buffer text point)))
    (setq emacs-minibuffer--input-queue (copy-sequence events))
    (nelisp-ec-set-buffer buf)
    (funcall fn)
    buf))

(ert-deftest isearch-forward-finds-first-match ()
  (emacs-isearch-test--with-fresh-world
    (let ((buf (emacs-isearch-test--run
                #'isearch-forward
                "foo bar foo baz"
                1
                "foo" 'return)))
      (should (= 4 (emacs-isearch-test--point buf))))))

(ert-deftest isearch-forward-cycles-to-next-match ()
  (emacs-isearch-test--with-fresh-world
    (let ((buf (emacs-isearch-test--run
                #'isearch-forward
                "foo bar foo baz foo"
                1
                "foo" ?\C-s 'return)))
      (should (= 12 (emacs-isearch-test--point buf))))))

(ert-deftest isearch-backward-finds-prev-match ()
  (emacs-isearch-test--with-fresh-world
    (let ((buf (emacs-isearch-test--run
                #'isearch-backward
                "foo bar foo baz foo"
                20
                "foo" 'return)))
      (should (= 17 (emacs-isearch-test--point buf))))))

(ert-deftest isearch-abort-restores-point ()
  (emacs-isearch-test--with-fresh-world
    (let ((buf (emacs-isearch-test--run
                #'isearch-forward
                "foo bar foo"
                5
                "foo" ?\C-g)))
      (should (= 5 (emacs-isearch-test--point buf))))))

(ert-deftest isearch-RET-commits-point ()
  (emacs-isearch-test--with-fresh-world
    (let ((buf (emacs-isearch-test--run
                #'isearch-forward
                "alpha beta beta"
                1
                "beta" 'return)))
      (should (= 11 (emacs-isearch-test--point buf))))))

(ert-deftest isearch-DEL-shrinks-pattern ()
  (emacs-isearch-test--with-fresh-world
    (let ((captured-face nil)
          (captured-prompt nil)
          (buf (emacs-isearch-test--make-buffer "food fool foot" 1)))
      (nelisp-ec-set-buffer buf)
      (setq emacs-minibuffer--key-fn
            (let ((events (list ?f ?o ?o ?l ?\d 'return)))
              (lambda (_prompt)
                (let ((ev (pop events)))
                  (when (and (eq ev ?\d) emacs-isearch--match-beg)
                    (setq captured-face
                          (emacs-buffer-get-text-property
                           emacs-isearch--match-beg 'face buf)))
                  ev))))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq captured-prompt (apply #'format fmt args)))))
        (isearch-forward))
      (should (eq 'isearch captured-face))
      (should (string-match-p "I-search: foo\\'" captured-prompt))
      (should (= 4 (emacs-isearch-test--point buf))))))

(ert-deftest isearch-search-from-start-direct-reports-states ()
  (emacs-isearch-test--with-fresh-world
    (let ((buf (emacs-isearch-test--make-buffer "foo bar foo" 5)))
      (nelisp-ec-with-current-buffer buf
        (let ((empty (emacs-isearch-search-from-start-direct
                      "" 'forward 5)))
          (should (eq 'empty (plist-get empty :status)))
          (should-not (plist-get empty :failing))
          (should (= 5 (plist-get empty :point)))
          (should (= 5 (nelisp-ec-point))))
        (let ((found (emacs-isearch-search-from-start-direct
                      "foo" 'forward 1)))
          (should (eq 'found (plist-get found :status)))
          (should-not (plist-get found :failing))
          (should (= 4 (plist-get found :point)))
          (should (= 4 (nelisp-ec-point))))
        (let ((backward (emacs-isearch-search-from-start-direct
                         "foo" 'backward (nelisp-ec-point-max))))
          (should (eq 'found (plist-get backward :status)))
          (should (= 9 (plist-get backward :point)))
          (should (= 9 (nelisp-ec-point))))
        (let ((missing (emacs-isearch-search-from-start-direct
                        "zzz" 'forward 5)))
          (should (eq 'failing (plist-get missing :status)))
          (should (plist-get missing :failing))
          (should (= 5 (plist-get missing :point)))
          (should (= 5 (nelisp-ec-point))))))))

(ert-deftest isearch-restore-start-direct-moves-point ()
  (emacs-isearch-test--with-fresh-world
    (let ((buf (emacs-isearch-test--make-buffer "foo bar" 7)))
      (nelisp-ec-with-current-buffer buf
        (let ((result (emacs-isearch-restore-start-direct 3)))
          (should (eq 'restored (plist-get result :status)))
          (should (= 3 (plist-get result :point)))
          (should (= 3 (nelisp-ec-point))))))))

(ert-deftest isearch-repeat-direct-reports-states ()
  (emacs-isearch-test--with-fresh-world
    (let ((buf (emacs-isearch-test--make-buffer "foo bar foo" 4)))
      (nelisp-ec-with-current-buffer buf
        (let ((empty (emacs-isearch-repeat-direct "" 'forward)))
          (should (eq 'empty (plist-get empty :status)))
          (should-not (plist-get empty :failing))
          (should (= 4 (plist-get empty :point)))
          (should (= 4 (nelisp-ec-point))))
        (let ((found (emacs-isearch-repeat-direct "foo" 'forward)))
          (should (eq 'found (plist-get found :status)))
          (should-not (plist-get found :failing))
          (should (= 12 (plist-get found :point)))
          (should (= 12 (nelisp-ec-point))))
        (let ((backward (emacs-isearch-repeat-direct "foo" 'backward)))
          (should (eq 'found (plist-get backward :status)))
          (should (= 9 (plist-get backward :point)))
          (should (= 9 (nelisp-ec-point))))
        (let ((missing (emacs-isearch-repeat-direct "zzz" 'forward)))
          (should (eq 'failing (plist-get missing :status)))
          (should (plist-get missing :failing))
          (should (= 9 (plist-get missing :point)))
          (should (= 9 (nelisp-ec-point))))))))

(provide 'emacs-isearch-test)

;;; emacs-isearch-test.el ends here
