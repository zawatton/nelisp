;;; nelisp-emacs-stdio-test.el --- ERT for emacs-stdio chunked reader  -*- lexical-binding: t; -*-

;; Phase A2 (= Doc anvil-runtime pure-elisp roadmap, 2026-05-09)
;; Tests cover the chunked stdin reader's elisp logic in isolation.
;; `read-stdin-bytes' is mocked via fluid `cl-letf' so the suite runs
;; under Emacs --batch where the real stdin primitive (= NeLisp's
;; libc.read fd 0 wrapper) is not available.  End-to-end smoke against
;; the actual standalone NeLisp binary lives in
;; `tests/phase-a2-stdio-shim-smoke.sh'.

(require 'ert)
(require 'cl-lib)
(require 'emacs-stdio)

(defvar nelisp-emacs-stdio-test--feed nil
  "Remaining byte chunks the mock `read-stdin-bytes' will return.

Each call pops the head and returns it.  When the list is empty the
mock returns nil (= EOF).")

(defmacro nelisp-emacs-stdio-test--with-feed (chunks &rest body)
  "Run BODY with `read-stdin-bytes' mocked to return CHUNKS in order."
  (declare (indent 1) (debug (form body)))
  `(let ((nelisp-emacs-stdio-test--feed ,chunks))
     (cl-letf (((symbol-function 'read-stdin-bytes)
                (lambda (_limit)
                  (pop nelisp-emacs-stdio-test--feed))))
       (emacs-stdio-reset-buffer)
       ,@body)))

;;;; basic line splitting

(ert-deftest nelisp-emacs-stdio-reads-single-line ()
  (nelisp-emacs-stdio-test--with-feed (list "hello\n")
    (should (string= "hello" (emacs-stdio-read-line)))
    (should (null (emacs-stdio-read-line)))))

(ert-deftest nelisp-emacs-stdio-reads-multiple-lines-from-one-chunk ()
  (nelisp-emacs-stdio-test--with-feed (list "a\nb\nc\n")
    (should (string= "a" (emacs-stdio-read-line)))
    (should (string= "b" (emacs-stdio-read-line)))
    (should (string= "c" (emacs-stdio-read-line)))
    (should (null (emacs-stdio-read-line)))))

(ert-deftest nelisp-emacs-stdio-handles-empty-line ()
  (nelisp-emacs-stdio-test--with-feed (list "first\n\nthird\n")
    (should (string= "first" (emacs-stdio-read-line)))
    (should (string= "" (emacs-stdio-read-line)))
    (should (string= "third" (emacs-stdio-read-line)))
    (should (null (emacs-stdio-read-line)))))

;;;; chunk-crossing reassembly

(ert-deftest nelisp-emacs-stdio-reassembles-line-across-chunks ()
  (nelisp-emacs-stdio-test--with-feed (list "hel" "lo," " wor" "ld\n")
    (should (string= "hello, world" (emacs-stdio-read-line)))
    (should (null (emacs-stdio-read-line)))))

(ert-deftest nelisp-emacs-stdio-handles-newline-at-chunk-boundary ()
  (nelisp-emacs-stdio-test--with-feed (list "alpha\n" "beta\n")
    (should (string= "alpha" (emacs-stdio-read-line)))
    (should (string= "beta" (emacs-stdio-read-line)))
    (should (null (emacs-stdio-read-line)))))

;;;; EOF semantics

(ert-deftest nelisp-emacs-stdio-pure-eof-returns-nil ()
  (nelisp-emacs-stdio-test--with-feed nil
    (should (null (emacs-stdio-read-line)))))

(ert-deftest nelisp-emacs-stdio-partial-tail-returned-then-eof ()
  (nelisp-emacs-stdio-test--with-feed (list "complete\n" "partial-no-LF")
    (should (string= "complete" (emacs-stdio-read-line)))
    (should (string= "partial-no-LF" (emacs-stdio-read-line)))
    (should (null (emacs-stdio-read-line)))))

(ert-deftest nelisp-emacs-stdio-empty-string-chunk-treated-as-eof ()
  (nelisp-emacs-stdio-test--with-feed (list "x\n" "")
    (should (string= "x" (emacs-stdio-read-line)))
    (should (null (emacs-stdio-read-line)))))

;;;; find-newline helper

(ert-deftest nelisp-emacs-stdio-find-newline-locates-first-lf ()
  (should (= 0 (emacs-stdio--find-newline "\n")))
  (should (= 3 (emacs-stdio--find-newline "abc\ndef")))
  (should (null (emacs-stdio--find-newline "no terminator")))
  (should (null (emacs-stdio--find-newline ""))))

;;;; install-stdin-shim policy

(ert-deftest nelisp-emacs-stdio-install-refuses-real-subr ()
  ;; Emacs --batch has a real `read-from-minibuffer' subr; the shim
  ;; must refuse to overwrite it (returns nil, leaves binding alone).
  (let ((before (symbol-function 'read-from-minibuffer)))
    (unwind-protect
        (when (subrp before)
          (should (null (emacs-stdio-install-stdin-shim)))
          (should (eq before (symbol-function 'read-from-minibuffer))))
      (when (subrp before)
        (defalias 'read-from-minibuffer before)))))

(ert-deftest nelisp-emacs-stdio-install-overrides-non-subr ()
  ;; A closure / lambda binding (e.g. emacs-stub-bulk's nil shim or a
  ;; previous shim) MUST be replaced by our reader.
  (let ((saved (and (fboundp 'read-from-minibuffer)
                    (symbol-function 'read-from-minibuffer))))
    (unwind-protect
        (cl-letf (((symbol-function 'read-from-minibuffer)
                   (lambda (&rest _) 'old-stub-marker)))
          (should (eq (emacs-stdio-install-stdin-shim) t))
          (should (not (eq 'old-stub-marker
                           (let ((nelisp-emacs-stdio-test--feed
                                  (list "shim-active\n")))
                             (cl-letf (((symbol-function 'read-stdin-bytes)
                                        (lambda (_)
                                          (pop nelisp-emacs-stdio-test--feed))))
                               (emacs-stdio-reset-buffer)
                               (read-from-minibuffer "")))))))
      (when saved
        (defalias 'read-from-minibuffer saved)))))

(provide 'nelisp-emacs-stdio-test)
;;; nelisp-emacs-stdio-test.el ends here
