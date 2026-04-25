;;; nelisp-emacs-compat-test.el --- ERT tests for nelisp-emacs-compat  -*- lexical-binding: t; -*-

;; Phase 9a MVP per Doc 33 LOCKED-2026-04-25-v2 §4.2a — Emacs editor API
;; layered on top of T36 `nelisp-text-buffer'.  Tests cover all 30
;; public APIs and exercise the cross-API invariants (point /
;; narrowing / marker / save-* unwind on non-local exit).

(require 'ert)
(require 'nelisp-emacs-compat)

;;; Test fixtures: each test gets a fresh registry / current-buffer

(defmacro nelisp-ec-test--with-fresh-world (&rest body)
  "Run BODY with a clean buffer registry and no current buffer."
  (declare (indent 0) (debug (body)))
  `(let ((nelisp-ec--buffers nil)
         (nelisp-ec--current-buffer nil))
     ,@body))

;;;; A. buffer registry + current buffer (5 APIs)

(ert-deftest nelisp-ec-generate-new-buffer-creates-buffer ()
  (nelisp-ec-test--with-fresh-world
   (let ((b (nelisp-ec-generate-new-buffer "scratch")))
     (should (nelisp-ec-buffer-p b))
     (should (string-equal "scratch" (nelisp-ec-buffer-name b)))
     (should (= 1 (nelisp-ec-buffer-point b)))
     (should (eq b (cdr (assoc "scratch" nelisp-ec--buffers))))
     (should (null (nelisp-ec-current-buffer))))))

(ert-deftest nelisp-ec-generate-new-buffer-uniquifies-name ()
  (nelisp-ec-test--with-fresh-world
   (let ((a (nelisp-ec-generate-new-buffer "x"))
         (b (nelisp-ec-generate-new-buffer "x"))
         (c (nelisp-ec-generate-new-buffer "x")))
     (should (string-equal "x"   (nelisp-ec-buffer-name a)))
     (should (string-equal "x<2>" (nelisp-ec-buffer-name b)))
     (should (string-equal "x<3>" (nelisp-ec-buffer-name c))))))

(ert-deftest nelisp-ec-set-buffer-and-current-buffer ()
  (nelisp-ec-test--with-fresh-world
   (let ((a (nelisp-ec-generate-new-buffer "a"))
         (b (nelisp-ec-generate-new-buffer "b")))
     (should (eq a (nelisp-ec-set-buffer a)))
     (should (eq a (nelisp-ec-current-buffer)))
     (nelisp-ec-set-buffer b)
     (should (eq b (nelisp-ec-current-buffer))))))

(ert-deftest nelisp-ec-with-current-buffer-restores-on-normal-exit ()
  (nelisp-ec-test--with-fresh-world
   (let ((a (nelisp-ec-generate-new-buffer "a"))
         (b (nelisp-ec-generate-new-buffer "b")))
     (nelisp-ec-set-buffer a)
     (let ((inside-buf nil))
       (nelisp-ec-with-current-buffer b
         (setq inside-buf (nelisp-ec-current-buffer)))
       (should (eq b inside-buf))
       (should (eq a (nelisp-ec-current-buffer)))))))

(ert-deftest nelisp-ec-with-current-buffer-restores-on-non-local-exit ()
  (nelisp-ec-test--with-fresh-world
   (let ((a (nelisp-ec-generate-new-buffer "a"))
         (b (nelisp-ec-generate-new-buffer "b")))
     (nelisp-ec-set-buffer a)
     (catch 'tag
       (nelisp-ec-with-current-buffer b
         (throw 'tag t)))
     (should (eq a (nelisp-ec-current-buffer))))))

(ert-deftest nelisp-ec-kill-buffer-removes-from-registry ()
  (nelisp-ec-test--with-fresh-world
   (let ((b (nelisp-ec-generate-new-buffer "doomed")))
     (nelisp-ec-set-buffer b)
     (should (eq t (nelisp-ec-kill-buffer b)))
     (should (null (assoc "doomed" nelisp-ec--buffers)))
     (should (null (nelisp-ec-current-buffer)))
     (should (nelisp-ec-buffer-killed-p b))
     (should-error (nelisp-ec-set-buffer b)
                   :type 'nelisp-ec-buffer-killed))))

;;;; B. point + cursor control (7 APIs)

(ert-deftest nelisp-ec-point-on-empty-buffer ()
  (nelisp-ec-test--with-fresh-world
   (let ((b (nelisp-ec-generate-new-buffer "p")))
     (nelisp-ec-set-buffer b)
     (should (= 1 (nelisp-ec-point)))
     (should (= 1 (nelisp-ec-point-min)))
     (should (= 1 (nelisp-ec-point-max)))
     (should (= 0 (nelisp-ec-buffer-size))))))

(ert-deftest nelisp-ec-goto-char-and-bounds ()
  (nelisp-ec-test--with-fresh-world
   (let ((b (nelisp-ec-generate-new-buffer "g")))
     (nelisp-ec-set-buffer b)
     (nelisp-ec-insert "hello")
     (should (= 6 (nelisp-ec-point)))
     (should (= 6 (nelisp-ec-point-max)))
     (should (= 1 (nelisp-ec-goto-char 1)))
     (should (= 1 (nelisp-ec-point)))
     (should (= 4 (nelisp-ec-goto-char 4)))
     (should-error (nelisp-ec-goto-char 99)
                   :type 'nelisp-ec-args-out-of-range)
     (should-error (nelisp-ec-goto-char 0)
                   :type 'nelisp-ec-args-out-of-range))))

(ert-deftest nelisp-ec-forward-and-backward-char ()
  (nelisp-ec-test--with-fresh-world
   (let ((b (nelisp-ec-generate-new-buffer "f")))
     (nelisp-ec-set-buffer b)
     (nelisp-ec-insert "abcdef")
     (nelisp-ec-goto-char 2)
     (nelisp-ec-forward-char)
     (should (= 3 (nelisp-ec-point)))
     (nelisp-ec-forward-char 2)
     (should (= 5 (nelisp-ec-point)))
     (nelisp-ec-backward-char 4)
     (should (= 1 (nelisp-ec-point)))
     (should-error (nelisp-ec-backward-char)
                   :type 'nelisp-ec-args-out-of-range)
     (nelisp-ec-goto-char (nelisp-ec-point-max))
     (should-error (nelisp-ec-forward-char)
                   :type 'nelisp-ec-args-out-of-range))))

(ert-deftest nelisp-ec-buffer-size-ignores-narrowing ()
  (nelisp-ec-test--with-fresh-world
   (let ((b (nelisp-ec-generate-new-buffer "bs")))
     (nelisp-ec-set-buffer b)
     (nelisp-ec-insert "abcdefgh")
     (nelisp-ec-narrow-to-region 3 6)
     (should (= 8 (nelisp-ec-buffer-size)))
     (should (= 3 (nelisp-ec-point-min)))
     (should (= 6 (nelisp-ec-point-max))))))

;;;; C. text editing (6 APIs)

(ert-deftest nelisp-ec-insert-and-point-advance ()
  (nelisp-ec-test--with-fresh-world
   (let ((b (nelisp-ec-generate-new-buffer "i")))
     (nelisp-ec-set-buffer b)
     (nelisp-ec-insert "hello")
     (should (string-equal "hello" (nelisp-ec-buffer-string)))
     (should (= 6 (nelisp-ec-point)))
     (should (nelisp-ec-buffer-modified-p b)))))

(ert-deftest nelisp-ec-insert-multiple-strings ()
  (nelisp-ec-test--with-fresh-world
   (let ((b (nelisp-ec-generate-new-buffer "im")))
     (nelisp-ec-set-buffer b)
     (nelisp-ec-insert "a" "b" "c" nil "d")
     (should (string-equal "abcd" (nelisp-ec-buffer-string)))
     (should (= 5 (nelisp-ec-point))))))

(ert-deftest nelisp-ec-insert-multibyte-utf8 ()
  (nelisp-ec-test--with-fresh-world
   (let ((b (nelisp-ec-generate-new-buffer "mb")))
     (nelisp-ec-set-buffer b)
     (nelisp-ec-insert "日本語")
     (should (string-equal "日本語" (nelisp-ec-buffer-string)))
     (should (= 4 (nelisp-ec-point)))
     (should (= 3 (nelisp-ec-buffer-size))))))

(ert-deftest nelisp-ec-delete-region-shifts-point ()
  (nelisp-ec-test--with-fresh-world
   (let ((b (nelisp-ec-generate-new-buffer "d")))
     (nelisp-ec-set-buffer b)
     (nelisp-ec-insert "ABCDEFG")
     (nelisp-ec-goto-char 7)
     (nelisp-ec-delete-region 2 4)
     (should (string-equal "ADEFG" (nelisp-ec-buffer-string)))
     (should (= 5 (nelisp-ec-point))))))

(ert-deftest nelisp-ec-delete-region-collapses-point-inside ()
  (nelisp-ec-test--with-fresh-world
   (let ((b (nelisp-ec-generate-new-buffer "d2")))
     (nelisp-ec-set-buffer b)
     (nelisp-ec-insert "ABCDEFG")
     (nelisp-ec-goto-char 4)
     (nelisp-ec-delete-region 2 6)
     (should (string-equal "AFG" (nelisp-ec-buffer-string)))
     (should (= 2 (nelisp-ec-point))))))

(ert-deftest nelisp-ec-delete-char-positive-and-negative ()
  (nelisp-ec-test--with-fresh-world
   (let ((b (nelisp-ec-generate-new-buffer "dc")))
     (nelisp-ec-set-buffer b)
     (nelisp-ec-insert "abcdef")
     (nelisp-ec-goto-char 3)
     (nelisp-ec-delete-char 2)
     (should (string-equal "abef" (nelisp-ec-buffer-string)))
     (should (= 3 (nelisp-ec-point)))
     (nelisp-ec-delete-char -1)
     (should (string-equal "aef" (nelisp-ec-buffer-string)))
     (should (= 2 (nelisp-ec-point))))))

(ert-deftest nelisp-ec-erase-buffer-resets ()
  (nelisp-ec-test--with-fresh-world
   (let ((b (nelisp-ec-generate-new-buffer "e")))
     (nelisp-ec-set-buffer b)
     (nelisp-ec-insert "to be erased")
     (nelisp-ec-erase-buffer)
     (should (string-equal "" (nelisp-ec-buffer-string)))
     (should (= 1 (nelisp-ec-point)))
     (should (= 0 (nelisp-ec-buffer-size))))))

(ert-deftest nelisp-ec-buffer-substring-respects-args ()
  (nelisp-ec-test--with-fresh-world
   (let ((b (nelisp-ec-generate-new-buffer "s")))
     (nelisp-ec-set-buffer b)
     (nelisp-ec-insert "0123456789")
     (should (string-equal "234" (nelisp-ec-buffer-substring 3 6)))
     (should (string-equal "" (nelisp-ec-buffer-substring 5 5)))
     (should-error (nelisp-ec-buffer-substring 0 5)
                   :type 'nelisp-ec-args-out-of-range))))

;;;; D. save-* family (3 macros)

(ert-deftest nelisp-ec-save-excursion-restores-point ()
  (nelisp-ec-test--with-fresh-world
   (let ((b (nelisp-ec-generate-new-buffer "se")))
     (nelisp-ec-set-buffer b)
     (nelisp-ec-insert "abcdef")
     (nelisp-ec-goto-char 2)
     (nelisp-ec-save-excursion
       (nelisp-ec-goto-char 5)
       (should (= 5 (nelisp-ec-point))))
     (should (= 2 (nelisp-ec-point))))))

(ert-deftest nelisp-ec-save-excursion-restores-on-non-local-exit ()
  (nelisp-ec-test--with-fresh-world
   (let ((b (nelisp-ec-generate-new-buffer "sex")))
     (nelisp-ec-set-buffer b)
     (nelisp-ec-insert "abcdef")
     (nelisp-ec-goto-char 3)
     (catch 'esc
       (nelisp-ec-save-excursion
         (nelisp-ec-goto-char 1)
         (throw 'esc t)))
     (should (= 3 (nelisp-ec-point))))))

(ert-deftest nelisp-ec-save-restriction-restores-narrowing ()
  (nelisp-ec-test--with-fresh-world
   (let ((b (nelisp-ec-generate-new-buffer "sr")))
     (nelisp-ec-set-buffer b)
     (nelisp-ec-insert "0123456789")
     (nelisp-ec-narrow-to-region 3 7)
     (nelisp-ec-save-restriction
       (nelisp-ec-widen)
       (should (= 1 (nelisp-ec-point-min)))
       (should (= 11 (nelisp-ec-point-max))))
     (should (= 3 (nelisp-ec-point-min)))
     (should (= 7 (nelisp-ec-point-max))))))

(ert-deftest nelisp-ec-save-current-buffer-restores ()
  (nelisp-ec-test--with-fresh-world
   (let ((a (nelisp-ec-generate-new-buffer "a"))
         (b (nelisp-ec-generate-new-buffer "b")))
     (nelisp-ec-set-buffer a)
     (nelisp-ec-save-current-buffer
       (nelisp-ec-set-buffer b)
       (should (eq b (nelisp-ec-current-buffer))))
     (should (eq a (nelisp-ec-current-buffer))))))

;;;; E. narrowing (2 APIs)

(ert-deftest nelisp-ec-narrowing-restricts-point-min-max ()
  (nelisp-ec-test--with-fresh-world
   (let ((b (nelisp-ec-generate-new-buffer "n")))
     (nelisp-ec-set-buffer b)
     (nelisp-ec-insert "0123456789")
     (nelisp-ec-narrow-to-region 3 7)
     (should (= 3 (nelisp-ec-point-min)))
     (should (= 7 (nelisp-ec-point-max)))
     (should-error (nelisp-ec-goto-char 1)
                   :type 'nelisp-ec-args-out-of-range)
     (should-error (nelisp-ec-goto-char 9)
                   :type 'nelisp-ec-args-out-of-range)
     (nelisp-ec-widen)
     (should (= 1 (nelisp-ec-point-min)))
     (should (= 11 (nelisp-ec-point-max))))))

(ert-deftest nelisp-ec-narrow-clamps-point ()
  (nelisp-ec-test--with-fresh-world
   (let ((b (nelisp-ec-generate-new-buffer "nc")))
     (nelisp-ec-set-buffer b)
     (nelisp-ec-insert "0123456789")
     (nelisp-ec-goto-char 1)
     (nelisp-ec-narrow-to-region 4 7)
     (should (= 4 (nelisp-ec-point))))))

(ert-deftest nelisp-ec-narrow-then-insert-pushes-end ()
  (nelisp-ec-test--with-fresh-world
   (let ((b (nelisp-ec-generate-new-buffer "ni")))
     (nelisp-ec-set-buffer b)
     (nelisp-ec-insert "0123456789")
     (nelisp-ec-narrow-to-region 3 7)
     (nelisp-ec-goto-char 4)
     (nelisp-ec-insert "ZZ")
     (should (= 9 (nelisp-ec-point-max)))
     (should (string-equal "2ZZ345" (nelisp-ec-buffer-string)))
     (nelisp-ec-widen)
     (should (string-equal "012ZZ3456789" (nelisp-ec-buffer-string))))))

;;;; F. marker (5 APIs)

(ert-deftest nelisp-ec-make-marker-detached ()
  (let ((m (nelisp-ec-make-marker)))
    (should (nelisp-ec-marker-p m))
    (should (null (nelisp-ec-marker-position m)))
    (should (null (nelisp-ec-marker-buffer m)))))

(ert-deftest nelisp-ec-set-marker-and-position ()
  (nelisp-ec-test--with-fresh-world
   (let ((b (nelisp-ec-generate-new-buffer "m"))
         (m (nelisp-ec-make-marker)))
     (nelisp-ec-set-buffer b)
     (nelisp-ec-insert "abcdef")
     (nelisp-ec-set-marker m 4 b)
     (should (= 4 (nelisp-ec-marker-position m)))
     (should (eq b (nelisp-ec-marker-buffer m)))
     (nelisp-ec-set-marker m nil)
     (should (null (nelisp-ec-marker-position m)))
     (should (null (nelisp-ec-marker-buffer m))))))

(ert-deftest nelisp-ec-point-marker-snapshots-point ()
  (nelisp-ec-test--with-fresh-world
   (let ((b (nelisp-ec-generate-new-buffer "pm")))
     (nelisp-ec-set-buffer b)
     (nelisp-ec-insert "hello")
     (nelisp-ec-goto-char 3)
     (let ((m (nelisp-ec-point-marker)))
       (should (= 3 (nelisp-ec-marker-position m)))
       (should (eq b (nelisp-ec-marker-buffer m)))
       ;; MVP marker is static — moving point does NOT update the marker.
       (nelisp-ec-goto-char 5)
       (should (= 3 (nelisp-ec-marker-position m)))))))

(ert-deftest nelisp-ec-set-marker-rejects-out-of-range ()
  (nelisp-ec-test--with-fresh-world
   (let ((b (nelisp-ec-generate-new-buffer "moor"))
         (m (nelisp-ec-make-marker)))
     (nelisp-ec-set-buffer b)
     (nelisp-ec-insert "abc")
     (should-error (nelisp-ec-set-marker m 99 b)
                   :type 'nelisp-ec-args-out-of-range))))

;;;; G. search (3 APIs)

(ert-deftest nelisp-ec-search-forward-literal ()
  (nelisp-ec-test--with-fresh-world
   (let ((b (nelisp-ec-generate-new-buffer "sf")))
     (nelisp-ec-set-buffer b)
     (nelisp-ec-insert "hello world goodbye world")
     (nelisp-ec-goto-char 1)
     (let ((p (nelisp-ec-search-forward "world")))
       (should (= 12 p))
       (should (= 12 (nelisp-ec-point))))
     (let ((p (nelisp-ec-search-forward "world")))
       (should (= 26 p)))
     (should (null (nelisp-ec-search-forward "world" nil t)))
     (should-error (nelisp-ec-search-forward "world")
                   :type 'nelisp-ec-error))))

(ert-deftest nelisp-ec-search-backward-literal ()
  (nelisp-ec-test--with-fresh-world
   (let ((b (nelisp-ec-generate-new-buffer "sb")))
     (nelisp-ec-set-buffer b)
     (nelisp-ec-insert "alpha beta gamma beta delta")
     (nelisp-ec-goto-char (nelisp-ec-point-max))
     (let ((p (nelisp-ec-search-backward "beta")))
       (should (= 18 p))
       (should (= 18 (nelisp-ec-point))))
     (let ((p (nelisp-ec-search-backward "beta")))
       (should (= 7 p)))
     (should (null (nelisp-ec-search-backward "beta" nil t))))))

(ert-deftest nelisp-ec-search-forward-bound-rejected ()
  (nelisp-ec-test--with-fresh-world
   (let ((b (nelisp-ec-generate-new-buffer "sfb")))
     (nelisp-ec-set-buffer b)
     (nelisp-ec-insert "find me here")
     (nelisp-ec-goto-char 1)
     (should (null (nelisp-ec-search-forward "here" 5 t))))))

(ert-deftest nelisp-ec-looking-at-p-literal ()
  (nelisp-ec-test--with-fresh-world
   (let ((b (nelisp-ec-generate-new-buffer "la")))
     (nelisp-ec-set-buffer b)
     (nelisp-ec-insert "hello world")
     (nelisp-ec-goto-char 1)
     (should (nelisp-ec-looking-at-p "hello"))
     (should-not (nelisp-ec-looking-at-p "world"))
     (should (nelisp-ec-looking-at-p ""))
     (nelisp-ec-goto-char 7)
     (should (nelisp-ec-looking-at-p "world"))
     (nelisp-ec-goto-char (nelisp-ec-point-max))
     (should-not (nelisp-ec-looking-at-p "x")))))

;;;; cross-API integration

(ert-deftest nelisp-ec-end-to-end-edit-search-narrow ()
  "Smoke test exercising buffer / point / insert / search / narrow / save."
  (nelisp-ec-test--with-fresh-world
   (let ((b (nelisp-ec-generate-new-buffer "e2e")))
     (nelisp-ec-set-buffer b)
     (nelisp-ec-insert "header\nbody1\nbody2\nfooter")
     (nelisp-ec-goto-char 1)
     (let ((hit (nelisp-ec-search-forward "body1")))
       (should (= 13 hit)))
     (nelisp-ec-save-restriction
       (nelisp-ec-narrow-to-region 8 19)
       (should (string-equal "body1\nbody2" (nelisp-ec-buffer-string)))
       (nelisp-ec-goto-char (nelisp-ec-point-min))
       (nelisp-ec-insert "X")
       (should (string-equal "Xbody1\nbody2" (nelisp-ec-buffer-string))))
     (should (string-equal "header\nXbody1\nbody2\nfooter"
                           (nelisp-ec-buffer-string))))))

(ert-deftest nelisp-ec-no-current-buffer-signals ()
  (nelisp-ec-test--with-fresh-world
   (should-error (nelisp-ec-point) :type 'nelisp-ec-no-current-buffer)
   (should-error (nelisp-ec-insert "x") :type 'nelisp-ec-no-current-buffer)))

(provide 'nelisp-emacs-compat-test)
;;; nelisp-emacs-compat-test.el ends here
