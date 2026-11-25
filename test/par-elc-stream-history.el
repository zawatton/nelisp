;;; par-elc-stream-history.el --- Compiled stream load boundaries -*- lexical-binding: t; -*-

(defvar par-elc-stream--outer)
(defvar par-elc-stream--inner)
(defvar par-elc-stream--broken)
(defvar par-elc-stream--events)
(defvar par-elc-stream--property)
(defvar par-elc-stream--expand-count)

(defun par-elc-stream--history (history)
  (mapcar (lambda (entry)
            (if (stringp entry) (file-name-nondirectory entry) entry))
          history))

(defun par-elc-stream--record (event)
  (push (list event
              (and load-file-name (file-name-nondirectory load-file-name))
              (par-elc-stream--history current-load-list))
        par-elc-stream--events))

(defun par-elc-stream--print (&rest items)
  (prin1 items)
  (terpri))

(defmacro par-elc-stream--macro ()
  (setq par-elc-stream--expand-count (1+ par-elc-stream--expand-count))
  '(par-elc-stream--record 'expanded))

(let* ((directory (make-temp-file "par-elc-stream-" t))
       (par-elc-stream--outer (expand-file-name "outer.elc" directory))
       (par-elc-stream--inner (expand-file-name "inner.elc" directory))
       (par-elc-stream--broken (expand-file-name "broken.elc" directory))
       (par-elc-stream--events nil)
       (par-elc-stream--expand-count 0)
       (after-load-alist nil)
       (after-load-functions
        (list (lambda (file)
                (when (member file (list par-elc-stream--outer par-elc-stream--inner
                                         par-elc-stream--broken))
                  (par-elc-stream--record
                   (intern (concat "hook-" (file-name-base file))))))))
       (header ";ELC\037\0\0\0\n;;; Compiled\n;;; in Emacs version 31.1\n")
       (doc " Héllo 日本\037")
       (prefix (concat header "#@" (number-to-string (string-bytes doc))))
       (coding-system-for-write 'no-conversion))
  (unwind-protect
      (progn
        (write-region
         (concat header "(par-elc-stream--record 'inner-start)\n"
                 "(provide 'par-elc-stream-inner)\n"
                 "(par-elc-stream--record 'inner-end)\n")
         nil par-elc-stream--inner nil 'silent)
        (write-region
         (concat header "(defalias 'par-elc-stream-broken #[0 \"\\300\\207\" [partial] 1])\n"
                 "(error \"broken compiled load\")\n"
                 "(par-elc-stream--record 'unreachable)\n")
         nil par-elc-stream--broken nil 'silent)
        (write-region
         (encode-coding-string
          (concat prefix doc
                  "(par-elc-stream--record 'outer-start)\n"
                  (format "(defalias 'par-elc-stream-function #[0 \"\\300\\207\" [[one two]] 1 (#$ . %d)])\n"
                          (1+ (string-bytes prefix)))
                  "(fset 'par-elc-stream-raw #[0 \"\\300\\207\" [raw] 1])\n"
                  "(setq par-elc-stream--property #(\"é\" 0 1 (test-property yes)))\n"
                  "(par-elc-stream--record 'before-inner)\n"
                  "(load par-elc-stream--inner nil t t)\n"
                  "(condition-case nil (load par-elc-stream--broken nil t t) (error nil))\n"
                  "(par-elc-stream--record 'outer-resumed)\n"
                  "(eval-when-compile (par-elc-stream--record 'compile-form))\n"
                  "(par-elc-stream--macro)\n"
                  "(provide 'par-elc-stream-outer)\n"
                  "(par-elc-stream--record 'outer-end)\n"
                  "#@00 (par-elc-stream--record 'unreachable)\n")
          'utf-8)
         nil par-elc-stream--outer nil 'silent)
        (with-eval-after-load 'par-elc-stream-inner
          (par-elc-stream--record 'feature-inner))
        (with-eval-after-load par-elc-stream--outer
          (par-elc-stream--record 'file-outer))
        (let ((current-load-list '(caller-state)))
          (load par-elc-stream--outer nil t t)
          (par-elc-stream--print 'caller current-load-list))
        (dolist (event (nreverse par-elc-stream--events))
          (apply #'par-elc-stream--print event))
        (par-elc-stream--print 'function
                               (byte-code-function-p (symbol-function 'par-elc-stream-function))
                               (par-elc-stream-function) (par-elc-stream-raw)
                               (documentation 'par-elc-stream-function t)
                               (equal (symbol-file 'par-elc-stream-function 'defun)
                                      par-elc-stream--outer)
                               (symbol-file 'par-elc-stream-raw 'defun))
        (par-elc-stream--print 'history
                               (par-elc-stream--history
                                (assoc par-elc-stream--outer load-history))
                               (par-elc-stream--history
                                (assoc par-elc-stream--inner load-history))
                               (assoc par-elc-stream--broken load-history))
        (par-elc-stream--print 'reader
                               (get-text-property 0 'test-property par-elc-stream--property)
                               par-elc-stream--expand-count))
    (delete-directory directory t)))
