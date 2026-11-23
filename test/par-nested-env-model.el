;;; -*- lexical-binding: t; -*-
;; Host-only model of the F298 sentinel algorithm (slot 3 install + defvar + nested emit; view = defvars ++ env), checked against the
;; GNU expectations of par-nested-env.gnu.  Mutants: MUT=fwd emits L forward, MUT=slot3 emits slot 3 at S,
;; MUT=nosent never prepends S.  Usage: emacs --batch -l this.el ; prints "MODEL OK" or "MODEL RED <rows>".
(defvar nm-specials '(nv-s1 nv-s2))
(defvar nm-mut (getenv "MUT"))
(defun nm-sp (s) (memq s nm-specials))
(defun nm-name-p (x) (and x (symbolp x) (not (eq x t))))
(defun nm-install (L)                  ; fresh frame slot 3 and the skipped flag
  (let (slot3 skipped)
    (dolist (x L) (cond ((not (nm-name-p x)))
                        ((nm-sp x) (setq skipped t))
                        (t (push x slot3))))
    (when (and skipped (not (equal nm-mut "nosent"))) (push (cons 'SENT L) slot3))
    slot3))
(defun nm-defvar (slot3 name)          ; declare_one: contains-check (sentinel never matches), then prepend
  (if (memq name (seq-remove #'consp slot3)) slot3 (cons name slot3)))
(defun nm-emit-real (slot3)
  (if (not (seq-find #'consp slot3)) (reverse slot3)   ; LMODE 0: head-first consing
  (let (pre tail)
    (while (and slot3 (not (consp (car slot3)))) (push (car slot3) pre) (setq slot3 (cdr slot3)))
    (setq tail (when slot3 (let ((L (cdr (car slot3)))) (if (equal nm-mut "fwd") (reverse (seq-filter #'nm-name-p L))
                                                          (if (equal nm-mut "slot3") (cdr slot3) (seq-filter #'nm-name-p L))))))
    ;; LMODE 3: defvars (slot-3 order, newest first) lead, then the expanded env
    (append (reverse pre) tail))))
(defun nm-view (L &optional defvars) (let ((s (nm-install L))) (dolist (d defvars) (setq s (nm-defvar s d))) (nm-emit-real s)))
(defvar nm-fail nil)
(defmacro nm-row (n got want) `(unless (equal ,got ',want) (push ,n nm-fail)))
(nm-row "A1" (nm-view '(nv-s1 nv-ns1 nv-s2)) (nv-s1 nv-ns1 nv-s2))
(nm-row "A2" (nm-view '(nv-ns2 nv-s1 nv-ns2 nv-s2 nv-s1 nv-ns3)) (nv-ns2 nv-s1 nv-ns2 nv-s2 nv-s1 nv-ns3))
(nm-row "A4" (nm-view '(nv-s1 nv-ns1) '(nv-late)) (nv-late nv-s1 nv-ns1))
(nm-row "A5" (nm-view '(nv-s1 nv-ns1) nil) (nv-s1 nv-ns1))
(nm-row "A8" (nm-view '(nv-s1 nv-s2)) (nv-s1 nv-s2))
(nm-row "A9" (nm-view '(nv-s1 nv-ns1)) (nv-s1 nv-ns1))
(nm-row "nospecial" (nm-view '(nv-ns1 nv-ns2)) (nv-ns1 nv-ns2))
(princ (if nm-fail (format "MODEL RED %S\n" (nreverse nm-fail)) "MODEL OK\n"))
