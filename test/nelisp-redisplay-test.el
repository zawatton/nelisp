;;; nelisp-redisplay-test.el --- Phase 5-B.3 ERT -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT suite for Phase 5-B.3 — `src/nelisp-redisplay.el'.
;;
;; The suite exercises the redisplay module *without* actually
;; touching stdout: `nelisp-redisplay--output-fn' is rebound to a
;; capturing closure and assertions run against the recorded
;; escape / text fragments.
;;
;; Coverage:
;;   - nelisp-window struct + constructor defaults
;;   - mark-dirty idempotence, mark-all-dirty full sweep
;;   - ANSI CSI escapes: \\e[row;colH / \\e[2K / \\e[2J
;;   - dirty rows are painted in ascending order
;;   - width-clamp truncates long lines, empty rows pad blank
;;   - top-line scroll offsets the visible region
;;   - cursor position is re-placed after paint
;;   - dirty list is cleared post-paint
;;   - clear-screen, terminal-size, nil buffer

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-buffer)
(require 'nelisp-redisplay)

(defmacro nelisp-redisplay-test--capture (sink &rest body)
  "Run BODY with `nelisp-redisplay--output-fn' capturing into SINK.
SINK is bound inside BODY to a list of strings collected in
emission order (i.e. already reversed)."
  (declare (indent 1))
  (let ((acc (make-symbol "acc")))
    `(let* ((,acc (list))
            (nelisp-redisplay--output-fn
             (lambda (s) (push s ,acc))))
       (let ((,sink nil))
         ,@body
         (setq ,sink (nreverse ,acc))
         ,sink))))

(ert-deftest nelisp-redisplay-window-struct-defaults ()
  "Constructor defaults match spec."
  (let ((w (nelisp-make-window)))
    (should (null (nelisp-window-buffer w)))
    (should (= 1 (nelisp-window-top-line w)))
    (should (= 24 (nelisp-window-height w)))
    (should (= 80 (nelisp-window-width w)))
    (should (= 0 (nelisp-window-cursor-row w)))
    (should (= 0 (nelisp-window-cursor-col w)))
    (should (null (nelisp-window-dirty-rows w)))))

(ert-deftest nelisp-redisplay-window-custom-size ()
  "Height / width override via `nelisp-make-window'."
  (let ((w (nelisp-make-window nil 10 40)))
    (should (= 10 (nelisp-window-height w)))
    (should (= 40 (nelisp-window-width w)))))

(ert-deftest nelisp-redisplay-mark-dirty-basic ()
  (let ((w (nelisp-make-window)))
    (nelisp-window-mark-dirty w 3)
    (nelisp-window-mark-dirty w 5)
    (should (equal '(5 3) (nelisp-window-dirty-rows w)))))

(ert-deftest nelisp-redisplay-mark-dirty-idempotent ()
  (let ((w (nelisp-make-window)))
    (nelisp-window-mark-dirty w 2)
    (nelisp-window-mark-dirty w 2)
    (nelisp-window-mark-dirty w 2)
    (should (equal '(2) (nelisp-window-dirty-rows w)))))

(ert-deftest nelisp-redisplay-mark-all-dirty-covers-height ()
  (let ((w (nelisp-make-window nil 4 10)))
    (nelisp-window-mark-all-dirty w)
    (should (equal '(0 1 2 3)
                   (sort (copy-sequence (nelisp-window-dirty-rows w)) #'<)))))

(ert-deftest nelisp-redisplay-emits-goto-xy-escape ()
  "Redisplay emits `\\e[row;colH' for each dirty row + final cursor."
  (let* ((buf (nelisp-generate-new-buffer "rd-goto"))
         (w (nelisp-make-window buf 5 20)))
    (nelisp-insert "line1\nline2\nline3" buf)
    (nelisp-window-mark-dirty w 0)
    (nelisp-window-mark-dirty w 2)
    (let ((out (nelisp-redisplay-test--capture out
                 (nelisp-redisplay-window w))))
      (should (cl-some (lambda (s) (string-match-p "\e\\[1;1H" s)) out))
      (should (cl-some (lambda (s) (string-match-p "\e\\[3;1H" s)) out))
      ;; Trailing cursor (row 0 col 0 → 1;1H)
      (should (string-match-p "\e\\[1;1H" (car (last out)))))))

(ert-deftest nelisp-redisplay-emits-clear-line-escape ()
  (let* ((buf (nelisp-generate-new-buffer "rd-clr"))
         (w (nelisp-make-window buf 3 20)))
    (nelisp-insert "hello" buf)
    (nelisp-window-mark-dirty w 0)
    (let ((out (nelisp-redisplay-test--capture out
                 (nelisp-redisplay-window w))))
      (should (cl-some (lambda (s) (string-match-p "\e\\[2K" s)) out)))))

(ert-deftest nelisp-redisplay-emits-content ()
  (let* ((buf (nelisp-generate-new-buffer "rd-txt"))
         (w (nelisp-make-window buf 3 20)))
    (nelisp-insert "hello\nworld" buf)
    (nelisp-window-mark-dirty w 0)
    (nelisp-window-mark-dirty w 1)
    (let ((out (nelisp-redisplay-test--capture out
                 (nelisp-redisplay-window w))))
      (should (member "hello" out))
      (should (member "world" out)))))

(ert-deftest nelisp-redisplay-clamps-to-width ()
  "Content longer than `width' is truncated."
  (let* ((buf (nelisp-generate-new-buffer "rd-clamp"))
         (w (nelisp-make-window buf 3 5)))
    (nelisp-insert "abcdefghij" buf)
    (nelisp-window-mark-dirty w 0)
    (let ((out (nelisp-redisplay-test--capture out
                 (nelisp-redisplay-window w))))
      (should (member "abcde" out))
      (should (not (member "abcdefghij" out))))))

(ert-deftest nelisp-redisplay-clears-dirty-after-paint ()
  (let* ((buf (nelisp-generate-new-buffer "rd-dirtyclr"))
         (w (nelisp-make-window buf 3 10)))
    (nelisp-insert "a\nb\nc" buf)
    (nelisp-window-mark-dirty w 0)
    (nelisp-window-mark-dirty w 2)
    (nelisp-redisplay-test--capture out
      (nelisp-redisplay-window w))
    (should (null (nelisp-window-dirty-rows w)))))

(ert-deftest nelisp-redisplay-dirty-rows-emitted-in-ascending-order ()
  "Paint order matches ascending row index."
  (let* ((buf (nelisp-generate-new-buffer "rd-order"))
         (w (nelisp-make-window buf 5 10)))
    (nelisp-insert "r0\nr1\nr2\nr3\nr4" buf)
    (nelisp-window-mark-dirty w 3)
    (nelisp-window-mark-dirty w 0)
    (nelisp-window-mark-dirty w 2)
    (let* ((out (nelisp-redisplay-test--capture out
                  (nelisp-redisplay-window w)))
           (gotos (cl-remove-if-not
                   (lambda (s) (string-match-p "\\`\e\\[[0-9]+;1H\\'" s))
                   out))
           (rows (mapcar (lambda (s)
                           (string-match "\e\\[\\([0-9]+\\);" s)
                           (string-to-number (match-string 1 s)))
                         gotos)))
      ;; (r0 -> 1), (r2 -> 3), (r3 -> 4), trailing cursor (-> 1).
      (should (equal '(1 3 4 1) rows)))))

(ert-deftest nelisp-redisplay-empty-row-padded-blank ()
  "Rows beyond buffer end are painted as empty strings."
  (let* ((buf (nelisp-generate-new-buffer "rd-pad"))
         (w (nelisp-make-window buf 4 10)))
    (nelisp-insert "only" buf)
    (nelisp-window-mark-all-dirty w)
    (let ((out (nelisp-redisplay-test--capture out
                 (nelisp-redisplay-window w))))
      ;; Row 0 shows "only"; rows 1..3 are empty — their content
      ;; emission is the empty string, which is elided in the
      ;; trace.  Four paint-goto + four clear-line + trailing cursor
      ;; goto (=\e[1;1H) — 5 total gotos in [1-4];1H range.
      (should (= 5 (cl-count-if
                    (lambda (s) (string-match-p "\\`\e\\[[1-4];1H\\'" s))
                    out)))
      (should (= 4 (cl-count-if
                    (lambda (s) (string-match-p "\\`\e\\[2K\\'" s))
                    out))))))

(ert-deftest nelisp-redisplay-top-line-scroll ()
  "top-line=3 starts rendering at 1-based line 3."
  (let* ((buf (nelisp-generate-new-buffer "rd-scroll"))
         (w (nelisp-make-window buf 3 10)))
    (nelisp-insert "a\nb\nc\nd\ne" buf)
    (setf (nelisp-window-top-line w) 3)
    (nelisp-window-mark-all-dirty w)
    (let ((out (nelisp-redisplay-test--capture out
                 (nelisp-redisplay-window w))))
      (should (member "c" out))
      (should (member "d" out))
      (should (member "e" out))
      (should (not (member "a" out)))
      (should (not (member "b" out))))))

(ert-deftest nelisp-redisplay-cursor-position-after-paint ()
  (let* ((buf (nelisp-generate-new-buffer "rd-cur"))
         (w (nelisp-make-window buf 5 20)))
    (nelisp-insert "xy" buf)
    (setf (nelisp-window-cursor-row w) 2)
    (setf (nelisp-window-cursor-col w) 4)
    (let* ((out (nelisp-redisplay-test--capture out
                  (nelisp-redisplay-window w))))
      (should (equal "\e[3;5H" (car (last out)))))))

(ert-deftest nelisp-redisplay-skips-out-of-range-dirty ()
  "Dirty row ≥ height is silently ignored."
  (let* ((buf (nelisp-generate-new-buffer "rd-oor"))
         (w (nelisp-make-window buf 2 10)))
    (nelisp-insert "a\nb" buf)
    (nelisp-window-mark-dirty w 0)
    (nelisp-window-mark-dirty w 99)
    (let* ((out (nelisp-redisplay-test--capture out
                  (nelisp-redisplay-window w)))
           (goto-count
            (cl-count-if
             (lambda (s) (string-match-p "\\`\e\\[[0-9]+;1H\\'" s))
             out)))
      ;; 1 goto for row 0 + 1 trailing goto for cursor = 2.
      (should (= 2 goto-count)))))

(ert-deftest nelisp-redisplay-clear-screen-emits-2j ()
  (let ((out (nelisp-redisplay-test--capture out
               (nelisp-redisplay-clear-screen))))
    (should (equal "\e[2J" (car out)))
    (should (equal "\e[1;1H" (cadr out)))))

(ert-deftest nelisp-redisplay-nil-buffer-no-crash ()
  "Redisplay of a window with no buffer paints empty rows only."
  (let ((w (nelisp-make-window nil 2 10)))
    (nelisp-window-mark-all-dirty w)
    (let ((out (nelisp-redisplay-test--capture out
                 (nelisp-redisplay-window w))))
      (should (listp out))
      (should (cl-some (lambda (s) (string-match-p "\e\\[2K" s)) out)))))

(ert-deftest nelisp-redisplay-terminal-size ()
  (let ((size (nelisp-redisplay-terminal-size)))
    (should (consp size))
    (should (integerp (car size)))
    (should (integerp (cdr size)))
    (should (> (car size) 0))
    (should (> (cdr size) 0))))

(provide 'nelisp-redisplay-test)
;;; nelisp-redisplay-test.el ends here
