;;; standalone-textprop-smoke.el --- Doc 210 text-property smoke -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Run under the standalone runtime, not host Emacs:
;;
;;     nelisp --load scripts/standalone-textprop-smoke.el
;;
;; Doc 210: fixes two standalone-only bugs reported from a nelisp-emacs-
;; lib lane running with zero custom logic against `target/nelisp':
;;
;;   1. `(get-text-property POS PROP)' read back nil right after
;;      `(put-text-property START END PROP VAL)' in a fresh buffer --
;;      `put-text-property'/`get-text-property' (`scripts/nelisp-stdlib-
;;      prelude.el') were no-op/always-nil stubs, never wired to the
;;      buffer struct's own `text-properties' slot the "buffer core"
;;      lane had already built (`nelisp-buffer-text-properties').
;;
;;   2. `(next-single-property-change POS PROP OBJECT)' hung when OBJECT
;;      was a string.  Root cause: the function did not exist at all
;;      (`void-function') -- textprop.c's primitives have no Elisp
;;      Provider anywhere in this tree; a caller retrying across a
;;      `void-function' in a loop is what produced the reported hang.
;;      It is now a real, terminating implementation.
;;
;; Every EXPECTED value below is what host GNU Emacs 31.1 answers for
;; the identical form (captured via `emacs --batch --load'-ing the same
;; 35 cases against real buffers/strings); this script hardcodes those
;; answers instead of cross-checking a live host process, so it can run
;; standalone-only, same as `standalone-bignum-smoke.el'.  Plist-shaped
;; results are canonicalized (sorted by key name) before comparing, so
;; an internally-different-but-semantically-equal property order is not
;; reported as a mismatch.  Case 23/24 build their two-region test
;; string via two `put-text-property' calls rather than `concat', since
;; whether `concat' carries over properties from propertized arguments
;; is a separate concern outside this fix's scope.

;;; Code:

(defvar textprop-smoke--n 0)
(defvar textprop-smoke--bad 0)

(defun textprop-smoke--canon (pl)
  (let (keys (l pl))
    (while l (push (car l) keys) (setq l (cddr l)))
    (mapcar (lambda (k) (cons k (plist-get pl k)))
            (sort keys (lambda (a b) (string< (symbol-name a) (symbol-name b)))))))

(defmacro textprop-smoke--check (label expected form)
  `(progn
     (setq textprop-smoke--n (1+ textprop-smoke--n))
     (let ((got (condition-case err ,form (error (list 'ERROR err)))))
       (unless (equal got ,expected)
         (setq textprop-smoke--bad (1+ textprop-smoke--bad))
         (princ (format "MISMATCH %s: expected %S, got %S\n" ,label ,expected got))))))

;; --- Bug 1 repro: buffer put/get, plus boundary and merged-plist cases
(textprop-smoke--check "01-put-then-get" 'bold
  (with-temp-buffer
    (insert "hello world")
    (put-text-property 1 6 'face 'bold)
    (get-text-property 1 'face)))

(textprop-smoke--check "02-outside-range-nil" nil
  (with-temp-buffer
    (insert "hello world")
    (put-text-property 1 6 'face 'bold)
    (get-text-property 6 'face)))

(textprop-smoke--check "03-inside-range" 'bold
  (with-temp-buffer
    (insert "hello world")
    (put-text-property 1 6 'face 'bold)
    (get-text-property 5 'face)))

(textprop-smoke--check "04-text-properties-at-merges" '((bold . t) (face . bold))
  (with-temp-buffer
    (insert "hello world")
    (put-text-property 2 5 'face 'bold)
    (put-text-property 2 5 'bold t)
    (textprop-smoke--canon (text-properties-at 3))))

(textprop-smoke--check "05-overlapping-ranges"
    '(outer outer inner nil outer)
  (with-temp-buffer
    (insert "abcdefgh")
    (put-text-property 2 8 'x 'outer)
    (put-text-property 4 6 'y 'inner)
    (list (get-text-property 3 'x) (get-text-property 5 'x) (get-text-property 5 'y)
          (get-text-property 3 'y) (get-text-property 7 'x))))

;; --- Bug 2 repro, plus the rest of `next-/previous-(single-)property-
;; change'/`text-property-any'/`text-property-not-all''s contract
(textprop-smoke--check "06-adjacent-equal-values-merge" 8
  (with-temp-buffer
    (insert "abcdefgh")
    (put-text-property 2 5 'face 'a)
    (put-text-property 5 8 'face 'a)
    (next-single-property-change 2 'face)))

(textprop-smoke--check "07-adjacent-different-values-boundary" 5
  (with-temp-buffer
    (insert "abcdefgh")
    (put-text-property 2 5 'face 'a)
    (put-text-property 5 8 'face 'b)
    (next-single-property-change 2 'face)))

(textprop-smoke--check "08-constant-to-end-nil" nil
  (with-temp-buffer
    (insert "abcdefgh")
    (put-text-property 2 9 'face 'a)
    (next-single-property-change 3 'face)))

(textprop-smoke--check "09-constant-to-limit-returns-limit" 6
  (with-temp-buffer
    (insert "abcdefgh")
    (put-text-property 2 9 'face 'a)
    (next-single-property-change 3 'face nil 6)))

(textprop-smoke--check "10-limit-before-real-boundary" 5
  (with-temp-buffer
    (insert "abcdefgh")
    (put-text-property 2 5 'face 'a)
    (next-single-property-change 2 'face nil 9)))

(textprop-smoke--check "11-previous-single-basic" 3
  (with-temp-buffer
    (insert "abcdefgh")
    (put-text-property 3 6 'face 'a)
    (previous-single-property-change 6 'face)))

(textprop-smoke--check "12-previous-constant-to-min-nil" nil
  (with-temp-buffer
    (insert "abcdefgh")
    (put-text-property 1 9 'face 'a)
    (previous-single-property-change 5 'face)))

(textprop-smoke--check "13-previous-with-limit" 3
  (with-temp-buffer
    (insert "abcdefgh")
    (put-text-property 1 9 'face 'a)
    (previous-single-property-change 5 'face nil 3)))

(textprop-smoke--check "14-next-property-change-plist-wide" 5
  (with-temp-buffer
    (insert "abcdefgh")
    (put-text-property 2 5 'face 'a)
    (put-text-property 5 8 'bold t)
    (next-property-change 2)))

(textprop-smoke--check "15-previous-property-change" 5
  (with-temp-buffer
    (insert "abcdefgh")
    (put-text-property 2 5 'face 'a)
    (put-text-property 5 8 'bold t)
    (previous-property-change 8)))

(textprop-smoke--check "16-text-property-any-found" 4
  (with-temp-buffer
    (insert "abcdefgh")
    (put-text-property 4 6 'face 'hit)
    (text-property-any 1 9 'face 'hit)))

(textprop-smoke--check "17-text-property-any-none" nil
  (with-temp-buffer
    (insert "abcdefgh")
    (put-text-property 4 6 'face 'hit)
    (text-property-any 1 4 'face 'hit)))

(textprop-smoke--check "18-text-property-not-all-found" 5
  (with-temp-buffer
    (insert "abcdefgh")
    (put-text-property 1 9 'face 'a)
    (put-text-property 5 6 'face 'b)
    (text-property-not-all 1 9 'face 'a)))

(textprop-smoke--check "19-text-property-not-all-none" nil
  (with-temp-buffer
    (insert "abcdefgh")
    (put-text-property 1 9 'face 'a)
    (text-property-not-all 1 9 'face 'a)))

;; --- Bug 2's actual OBJECT-is-a-string surface, plus `propertize'/
;; `put-text-property' on strings (0-based, independent objects)
(textprop-smoke--check "20-propertize-get" '(bold bold)
  (let ((s (propertize "hello" 'face 'bold)))
    (list (get-text-property 0 'face s) (get-text-property 4 'face s))))

(textprop-smoke--check "21-put-text-property-on-string" '(bold nil)
  (let ((s (copy-sequence "hello world")))
    (put-text-property 0 5 'face 'bold s)
    (list (get-text-property 0 'face s) (get-text-property 6 'face s))))

(textprop-smoke--check "22-next-single-on-string-constant-to-end-nil" nil
  (let ((s (propertize "hello" 'face 'bold)))
    (next-single-property-change 0 'face s)))

(textprop-smoke--check "23-next-single-on-string-real-boundary" 5
  (let ((s (copy-sequence "hello world")))
    (put-text-property 0 5 'face 'a s)
    (put-text-property 5 11 'face 'b s)
    (next-single-property-change 0 'face s)))

(textprop-smoke--check "24-previous-single-on-string" 5
  (let ((s (copy-sequence "hello world")))
    (put-text-property 0 5 'face 'a s)
    (put-text-property 5 11 'face 'b s)
    (previous-single-property-change 11 'face s)))

;; --- multibyte, insert/delete shifting, buffer-substring carrying
;; properties (or not), and the add/remove/set family's return values
(textprop-smoke--check "25-multibyte-buffer" '(jp jp nil)
  (with-temp-buffer
    (insert "あいうabc")
    (put-text-property 1 4 'face 'jp)
    (list (get-text-property 1 'face) (get-text-property 3 'face)
          (get-text-property 4 'face))))

(textprop-smoke--check "26-insert-shifts-properties-forward" '(hi nil)
  (with-temp-buffer
    (insert "abcdefgh")
    (put-text-property 4 8 'face 'hi)
    (goto-char 2)
    (insert "XX")
    (list (get-text-property 6 'face) (get-text-property 5 'face))))

(textprop-smoke--check "27-delete-shifts-properties-backward" '(hi nil)
  (with-temp-buffer
    (insert "abcdefghij")
    (put-text-property 5 9 'face 'hi)
    (delete-region 2 4)
    (list (get-text-property 3 'face) (get-text-property 2 'face))))

(textprop-smoke--check "28-buffer-substring-carries-properties" '((face . bold))
  (with-temp-buffer
    (insert "hello world")
    (put-text-property 1 6 'face 'bold)
    (let ((s (buffer-substring 1 6)))
      (textprop-smoke--canon (text-properties-at 0 s)))))

(textprop-smoke--check "29-buffer-substring-no-properties-strips" nil
  (with-temp-buffer
    (insert "hello world")
    (put-text-property 1 6 'face 'bold)
    (let ((s (buffer-substring-no-properties 1 6)))
      (text-properties-at 0 s))))

(textprop-smoke--check "30-remove-text-properties-drops-key" '(t nil t)
  (with-temp-buffer
    (insert "abcdefgh")
    (put-text-property 2 6 'face 'a)
    (put-text-property 2 6 'bold t)
    (list (remove-text-properties 2 6 '(face))
          (get-text-property 3 'face) (get-text-property 3 'bold))))

(textprop-smoke--check "31-remove-text-properties-noop-nil" nil
  (with-temp-buffer
    (insert "abcdefgh")
    (put-text-property 2 6 'bold t)
    (remove-text-properties 2 6 '(face))))

(textprop-smoke--check "32-add-text-properties-merges" '(t a t)
  (with-temp-buffer
    (insert "abcdefgh")
    (put-text-property 2 6 'face 'a)
    (list (add-text-properties 2 6 '(bold t))
          (get-text-property 3 'face) (get-text-property 3 'bold))))

(textprop-smoke--check "33-add-text-properties-noop-nil" nil
  (with-temp-buffer
    (insert "abcdefgh")
    (put-text-property 2 6 'face 'a)
    (add-text-properties 2 6 '(face a))))

(textprop-smoke--check "34-set-text-properties-wholesale-replace" '(nil nil t)
  (with-temp-buffer
    (insert "abcdefgh")
    (put-text-property 2 6 'face 'a)
    (put-text-property 2 6 'bold t)
    (set-text-properties 2 6 '(italic t))
    (list (get-text-property 3 'face) (get-text-property 3 'bold) (get-text-property 3 'italic))))

(textprop-smoke--check "35-set-text-properties-nil-clears" nil
  (with-temp-buffer
    (insert "abcdefgh")
    (put-text-property 2 6 'face 'a)
    (set-text-properties 2 6 nil)
    (text-properties-at 3)))

(princ (format "TEXTPROP-SMOKE cases=%d mismatches=%d\n"
               textprop-smoke--n textprop-smoke--bad))
