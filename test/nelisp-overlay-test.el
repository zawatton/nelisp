;;; nelisp-overlay-test.el --- ERT for overlay storage + Emacs-compat API  -*- lexical-binding: t; -*-

;; T150 / Phase 9c.4 — exercises `nelisp-overlay' (the `nelisp-ovly-'
;; module-prefix) per Doc 41 §3.4.5 ERT gate (+30 minimum, this suite
;; ships 49 storage + provider + precedence + dead-overlay tests,
;; comfortably above the 30-test scope target).
;;
;; Layout:
;;   §A. contract version + classify + construct round-trip
;;   §B. property put / get / properties (round-trip + plist semantics)
;;   §C. range queries — overlays-at / overlays-in / lists /
;;       next-change / previous-change
;;   §D. priority + insertion-order precedence (Doc 41 §2.5 LOCKED v1)
;;   §E. front-advance / rear-advance endpoint behaviour on insert
;;   §F. shift-on-insert / shift-on-delete bookkeeping
;;   §G. move (in-buffer + cross-buffer migration)
;;   §H. delete + delete-all + remove-many
;;   §I. copy (= fresh id, equal range, plist clone)
;;   §J. provider hooks — keymap-provider / display-provider /
;;       get-char-property overlay-aware semantics
;;   §K. T146 install / uninstall round-trip
;;   §L. dead-overlay tolerance + signal semantics

(require 'ert)
(require 'cl-lib)
(require 'nelisp-overlay)
(require 'nelisp-textprop-keymap)

;;; ─────────────────────────────────────────────────────────────────────
;;; Test fixtures
;;; ─────────────────────────────────────────────────────────────────────

(defmacro nelisp-ovly-test--with-clean-state (&rest body)
  "Run BODY with a clean overlay registry + saved/restored T146 state."
  (declare (indent 0) (debug t))
  `(let ((nelisp-textprop-keymap-with-injection nil)
         (saved-overlay-prov nelisp-textprop-keymap--overlay-provider)
         (saved-textprop-prov nelisp-textprop-keymap--textprop-provider))
     (unwind-protect
         (progn
           (nelisp-ovly--registry-clear)
           (setq nelisp-textprop-keymap--overlay-provider nil)
           (setq nelisp-textprop-keymap--textprop-provider nil)
           ,@body)
       (nelisp-ovly--registry-clear)
       (setq nelisp-textprop-keymap--overlay-provider saved-overlay-prov)
       (setq nelisp-textprop-keymap--textprop-provider saved-textprop-prov))))

(defmacro nelisp-ovly-test--with-buffer (var text &rest body)
  "Run BODY with VAR bound to a fresh buffer pre-filled with TEXT."
  (declare (indent 2) (debug t))
  `(let ((,var (generate-new-buffer "*nelisp-ovly-test*")))
     (unwind-protect
         (with-current-buffer ,var
           (insert ,text)
           (goto-char (point-min))
           ,@body)
       (when (buffer-live-p ,var)
         (kill-buffer ,var)))))

;;; ─────────────────────────────────────────────────────────────────────
;;; §A. contract version + construct
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-ovly-contract-version-locked ()
  (should (= nelisp-ovly-contract-version 1)))

(ert-deftest nelisp-ovly-make-roundtrip ()
  (nelisp-ovly-test--with-clean-state
    (nelisp-ovly-test--with-buffer buf "hello world"
      (let ((o (nelisp-ovly-make 2 6 buf)))
        (should (nelisp-ovly-overlayp o))
        (should (= (nelisp-ovly-start o) 2))
        (should (= (nelisp-ovly-end o)   6))
        (should (eq (nelisp-ovly-buffer o) buf))))))

(ert-deftest nelisp-ovly-make-defaults-to-current-buffer ()
  (nelisp-ovly-test--with-clean-state
    (nelisp-ovly-test--with-buffer buf "hello"
      (let ((o (nelisp-ovly-make 1 3)))
        (should (eq (nelisp-ovly-buffer o) buf))))))

(ert-deftest nelisp-ovly-make-swaps-inverted-range ()
  (nelisp-ovly-test--with-clean-state
    (nelisp-ovly-test--with-buffer buf "abcdef"
      (let ((o (nelisp-ovly-make 5 2 buf)))
        (should (= (nelisp-ovly-start o) 2))
        (should (= (nelisp-ovly-end o)   5))))))

(ert-deftest nelisp-ovly-make-rejects-non-integer ()
  (nelisp-ovly-test--with-clean-state
    (nelisp-ovly-test--with-buffer buf "abc"
      (should-error (nelisp-ovly-make "x" 3 buf)
                    :type 'nelisp-ovly-bad-range))))

(ert-deftest nelisp-ovly-overlayp-rejects-non-overlay ()
  (should-not (nelisp-ovly-overlayp nil))
  (should-not (nelisp-ovly-overlayp "string"))
  (should-not (nelisp-ovly-overlayp 42))
  (should-not (nelisp-ovly-overlayp '(a . b))))

;;; ─────────────────────────────────────────────────────────────────────
;;; §B. properties
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-ovly-put-get-roundtrip ()
  (nelisp-ovly-test--with-clean-state
    (nelisp-ovly-test--with-buffer buf "abcdef"
      (let ((o (nelisp-ovly-make 1 3 buf)))
        (should (eq (nelisp-ovly-put o 'face 'bold) 'bold))
        (should (eq (nelisp-ovly-get o 'face) 'bold))
        (should (eq (nelisp-ovly-put o 'face 'italic) 'italic))
        (should (eq (nelisp-ovly-get o 'face) 'italic))))))

(ert-deftest nelisp-ovly-properties-returns-fresh-copy ()
  (nelisp-ovly-test--with-clean-state
    (nelisp-ovly-test--with-buffer buf "abc"
      (let* ((o (nelisp-ovly-make 1 2 buf))
             (_ (nelisp-ovly-put o 'face 'bold))
             (_ (nelisp-ovly-put o 'priority 5))
             (pl1 (nelisp-ovly-properties o))
             (pl2 (nelisp-ovly-properties o)))
        (should (equal (plist-get pl1 'face) 'bold))
        (should (= (plist-get pl1 'priority) 5))
        (should-not (eq pl1 pl2))))))

(ert-deftest nelisp-ovly-get-absent-property-is-nil ()
  (nelisp-ovly-test--with-clean-state
    (nelisp-ovly-test--with-buffer buf "abc"
      (let ((o (nelisp-ovly-make 1 2 buf)))
        (should-not (nelisp-ovly-get o 'no-such))))))

;;; ─────────────────────────────────────────────────────────────────────
;;; §C. range queries
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-ovly-overlays-at-position ()
  (nelisp-ovly-test--with-clean-state
    (nelisp-ovly-test--with-buffer buf "abcdefghij"
      (let ((o1 (nelisp-ovly-make 1 5 buf))
            (o2 (nelisp-ovly-make 3 8 buf))
            (_o3 (nelisp-ovly-make 6 9 buf)))
        ;; pos=4 covers o1 (1..5) + o2 (3..8)
        (let ((hits (nelisp-ovly-overlays-at 4)))
          (should (= (length hits) 2))
          (should (memq o1 hits))
          (should (memq o2 hits)))
        ;; pos=5 covers o2 only (o1 ends at 5 exclusive)
        (let ((hits (nelisp-ovly-overlays-at 5)))
          (should (= (length hits) 1))
          (should (memq o2 hits)))))))

(ert-deftest nelisp-ovly-overlays-in-range ()
  (nelisp-ovly-test--with-clean-state
    (nelisp-ovly-test--with-buffer buf "abcdefghij"
      (let ((o1 (nelisp-ovly-make 1 3 buf))
            (o2 (nelisp-ovly-make 4 6 buf))
            (o3 (nelisp-ovly-make 8 10 buf)))
        (let ((hits (nelisp-ovly-overlays-in 2 7)))
          (should (memq o1 hits))
          (should (memq o2 hits))
          (should-not (memq o3 hits)))))))

(ert-deftest nelisp-ovly-overlays-in-zero-length-overlay-at-boundary ()
  (nelisp-ovly-test--with-clean-state
    (nelisp-ovly-test--with-buffer buf "abcdef"
      (let ((o (nelisp-ovly-make 3 3 buf)))
        ;; Zero-length overlay at point 3 should be picked up by
        ;; overlays-in covering point 3.
        (should (memq o (nelisp-ovly-overlays-in 3 5)))))))

(ert-deftest nelisp-ovly-lists-splits-on-point ()
  (nelisp-ovly-test--with-clean-state
    (nelisp-ovly-test--with-buffer buf "abcdefghij"
      (let ((o-before (nelisp-ovly-make 1 3 buf))
            (o-after  (nelisp-ovly-make 6 9 buf)))
        (goto-char 5)
        (let* ((cells (nelisp-ovly-lists buf))
               (before (car cells))
               (after  (cdr cells)))
          (should (memq o-before before))
          (should (memq o-after  after))
          (should-not (memq o-after  before))
          (should-not (memq o-before after)))))))

(ert-deftest nelisp-ovly-next-change-finds-next-endpoint ()
  (nelisp-ovly-test--with-clean-state
    (nelisp-ovly-test--with-buffer buf "abcdefghij"
      (nelisp-ovly-make 3 5 buf)
      (nelisp-ovly-make 7 9 buf)
      (should (= (nelisp-ovly-next-change 1) 3))
      (should (= (nelisp-ovly-next-change 3) 5))
      (should (= (nelisp-ovly-next-change 5) 7))
      ;; past last endpoint -> point-max (= 11 here, "abcdefghij" len 10)
      (should (= (nelisp-ovly-next-change 9) (point-max))))))

(ert-deftest nelisp-ovly-previous-change-finds-prev-endpoint ()
  (nelisp-ovly-test--with-clean-state
    (nelisp-ovly-test--with-buffer buf "abcdefghij"
      (nelisp-ovly-make 3 5 buf)
      (nelisp-ovly-make 7 9 buf)
      (should (= (nelisp-ovly-previous-change 10) 9))
      (should (= (nelisp-ovly-previous-change 9)  7))
      (should (= (nelisp-ovly-previous-change 7)  5))
      ;; before first endpoint -> point-min
      (should (= (nelisp-ovly-previous-change 3) (point-min))))))

;;; ─────────────────────────────────────────────────────────────────────
;;; §D. priority + insertion-order precedence
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-ovly-priority-nil-is-bottom ()
  (nelisp-ovly-test--with-clean-state
    (nelisp-ovly-test--with-buffer buf "abcdef"
      (let ((o-nil  (nelisp-ovly-make 1 5 buf))
            (o-num  (nelisp-ovly-make 1 5 buf)))
        (nelisp-ovly-put o-num 'priority 1)
        (let ((sorted (nelisp-ovly-overlays-at 2 t)))
          (should (eq (car sorted) o-num))
          (should (eq (cadr sorted) o-nil)))))))

(ert-deftest nelisp-ovly-higher-priority-wins ()
  (nelisp-ovly-test--with-clean-state
    (nelisp-ovly-test--with-buffer buf "abcdef"
      (let ((lo (nelisp-ovly-make 1 5 buf))
            (hi (nelisp-ovly-make 1 5 buf)))
        (nelisp-ovly-put lo 'priority 1)
        (nelisp-ovly-put hi 'priority 10)
        (let ((sorted (nelisp-ovly-overlays-at 2 t)))
          (should (eq (car sorted) hi))
          (should (eq (cadr sorted) lo)))))))

(ert-deftest nelisp-ovly-insertion-order-as-secondary-key ()
  (nelisp-ovly-test--with-clean-state
    (nelisp-ovly-test--with-buffer buf "abcdef"
      (let ((early (nelisp-ovly-make 1 5 buf))
            (late  (nelisp-ovly-make 1 5 buf)))
        (nelisp-ovly-put early 'priority 5)
        (nelisp-ovly-put late  'priority 5)
        (let ((sorted (nelisp-ovly-overlays-at 2 t)))
          ;; Same priority: later insertion wins.
          (should (eq (car sorted) late))
          (should (eq (cadr sorted) early)))))))

(ert-deftest nelisp-ovly-insertion-order-secondary-when-both-nil ()
  (nelisp-ovly-test--with-clean-state
    (nelisp-ovly-test--with-buffer buf "abcdef"
      (let ((early (nelisp-ovly-make 1 5 buf))
            (late  (nelisp-ovly-make 1 5 buf)))
        (let ((sorted (nelisp-ovly-overlays-at 2 t)))
          (should (eq (car sorted) late))
          (should (eq (cadr sorted) early)))))))

;;; ─────────────────────────────────────────────────────────────────────
;;; §E. front-advance / rear-advance
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-ovly-front-advance-on-insert ()
  (nelisp-ovly-test--with-clean-state
    (nelisp-ovly-test--with-buffer buf "abcdef"
      (let ((stay  (nelisp-ovly-make 3 5 buf nil nil))
            (adv   (nelisp-ovly-make 3 5 buf t   nil)))
        (nelisp-ovly-shift-on-insert buf 3 2)
        ;; STAY: start unchanged at 3 (front-advance nil = stay put)
        (should (= (nelisp-ovly-start stay) 3))
        ;; ADV: start advances by +2 → 5 (front-advance t)
        (should (= (nelisp-ovly-start adv)  5))))))

(ert-deftest nelisp-ovly-rear-advance-on-insert ()
  (nelisp-ovly-test--with-clean-state
    (nelisp-ovly-test--with-buffer buf "abcdef"
      (let ((stay (nelisp-ovly-make 1 4 buf nil nil))
            (adv  (nelisp-ovly-make 1 4 buf nil t)))
        (nelisp-ovly-shift-on-insert buf 4 2)
        ;; STAY: end stays at 4 (rear-advance nil)
        (should (= (nelisp-ovly-end stay) 4))
        ;; ADV: end advances → 6
        (should (= (nelisp-ovly-end adv)  6))))))

;;; ─────────────────────────────────────────────────────────────────────
;;; §F. shift-on-insert / shift-on-delete bookkeeping
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-ovly-shift-on-insert-far-after ()
  (nelisp-ovly-test--with-clean-state
    (nelisp-ovly-test--with-buffer buf "abcdefghij"
      (let ((o (nelisp-ovly-make 5 8 buf)))
        (nelisp-ovly-shift-on-insert buf 2 3)
        (should (= (nelisp-ovly-start o) 8))
        (should (= (nelisp-ovly-end   o) 11))))))

(ert-deftest nelisp-ovly-shift-on-insert-far-before ()
  (nelisp-ovly-test--with-clean-state
    (nelisp-ovly-test--with-buffer buf "abcdefghij"
      (let ((o (nelisp-ovly-make 2 4 buf)))
        (nelisp-ovly-shift-on-insert buf 8 3)
        (should (= (nelisp-ovly-start o) 2))
        (should (= (nelisp-ovly-end   o) 4))))))

(ert-deftest nelisp-ovly-shift-on-delete-collapses-to-start ()
  (nelisp-ovly-test--with-clean-state
    (nelisp-ovly-test--with-buffer buf "abcdefghij"
      (let ((o (nelisp-ovly-make 4 7 buf)))
        ;; delete [3, 8) should collapse o to (3, 3)
        (nelisp-ovly-shift-on-delete buf 3 8)
        (should (= (nelisp-ovly-start o) 3))
        (should (= (nelisp-ovly-end   o) 3))))))

(ert-deftest nelisp-ovly-shift-on-delete-shifts-tail ()
  (nelisp-ovly-test--with-clean-state
    (nelisp-ovly-test--with-buffer buf "abcdefghij"
      (let ((o (nelisp-ovly-make 7 9 buf)))
        (nelisp-ovly-shift-on-delete buf 2 5)
        (should (= (nelisp-ovly-start o) 4))
        (should (= (nelisp-ovly-end   o) 6))))))

(ert-deftest nelisp-ovly-shift-on-insert-resorts-list ()
  (nelisp-ovly-test--with-clean-state
    (nelisp-ovly-test--with-buffer buf "abcdefghij"
      (let ((o-late  (nelisp-ovly-make 6 8 buf nil nil))
            (o-early (nelisp-ovly-make 2 3 buf t   nil)))
        ;; Insert at start of o-early so its start advances to 5.
        ;; Verify the post-shift list is still sorted by start.
        (nelisp-ovly-shift-on-insert buf 2 3)
        (let* ((cells (nelisp-ovly-lists buf))
               (all   (append (car cells) (cdr cells))))
          (should (cl-position o-late all))
          (should (cl-position o-early all))
          (should (<= (nelisp-ovly-start (nth 0 all))
                      (nelisp-ovly-start (nth 1 all)))))))))

;;; ─────────────────────────────────────────────────────────────────────
;;; §G. move
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-ovly-move-in-buffer ()
  (nelisp-ovly-test--with-clean-state
    (nelisp-ovly-test--with-buffer buf "abcdefghij"
      (let ((o (nelisp-ovly-make 2 4 buf)))
        (nelisp-ovly-move o 6 9)
        (should (= (nelisp-ovly-start o) 6))
        (should (= (nelisp-ovly-end   o) 9))
        (should (eq (nelisp-ovly-buffer o) buf))))))

(ert-deftest nelisp-ovly-move-cross-buffer ()
  (nelisp-ovly-test--with-clean-state
    (let ((b1 (generate-new-buffer "*nelisp-ovly-test-b1*"))
          (b2 (generate-new-buffer "*nelisp-ovly-test-b2*")))
      (unwind-protect
          (progn
            (with-current-buffer b1 (insert "abcdefghij"))
            (with-current-buffer b2 (insert "klmnop"))
            (let ((o (nelisp-ovly-make 2 4 b1)))
              (nelisp-ovly-move o 1 3 b2)
              (should (eq (nelisp-ovly-buffer o) b2))
              (should (= (nelisp-ovly-start o) 1))
              (should (= (nelisp-ovly-end   o) 3))
              ;; b1 registry no longer references o
              (should-not
               (memq o
                     (let ((c (nelisp-ovly-lists b1)))
                       (append (car c) (cdr c)))))))
        (kill-buffer b1)
        (kill-buffer b2)))))

;;; ─────────────────────────────────────────────────────────────────────
;;; §H. delete / delete-all / remove-many
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-ovly-delete-removes-from-registry ()
  (nelisp-ovly-test--with-clean-state
    (nelisp-ovly-test--with-buffer buf "abcdef"
      (let ((o (nelisp-ovly-make 2 4 buf)))
        (should (memq o (nelisp-ovly-overlays-at 3)))
        (nelisp-ovly-delete o)
        (should-not (memq o (nelisp-ovly-overlays-at 3)))
        (should-not (nelisp-ovly-buffer o))))))

(ert-deftest nelisp-ovly-delete-all-detaches-each ()
  (nelisp-ovly-test--with-clean-state
    (nelisp-ovly-test--with-buffer buf "abcdef"
      (let ((o1 (nelisp-ovly-make 1 3 buf))
            (o2 (nelisp-ovly-make 2 5 buf)))
        (nelisp-ovly-delete-all buf)
        (should-not (nelisp-ovly-buffer o1))
        (should-not (nelisp-ovly-buffer o2))
        (let ((cells (nelisp-ovly-lists buf)))
          (should (null (car cells)))
          (should (null (cdr cells))))))))

(ert-deftest nelisp-ovly-remove-many-by-name-value ()
  (nelisp-ovly-test--with-clean-state
    (nelisp-ovly-test--with-buffer buf "abcdef"
      (let ((kept   (nelisp-ovly-make 1 3 buf))
            (killed (nelisp-ovly-make 2 5 buf)))
        (nelisp-ovly-put kept   'tag 'keep)
        (nelisp-ovly-put killed 'tag 'kill)
        (nelisp-ovly-remove-many nil nil 'tag 'kill)
        (should (nelisp-ovly-buffer kept))
        (should-not (nelisp-ovly-buffer killed))))))

(ert-deftest nelisp-ovly-remove-many-no-name-removes-everything ()
  (nelisp-ovly-test--with-clean-state
    (nelisp-ovly-test--with-buffer buf "abcdefghij"
      (let ((o1 (nelisp-ovly-make 1 3 buf))
            (o2 (nelisp-ovly-make 7 9 buf)))
        (nelisp-ovly-remove-many 5 (point-max))
        (should (nelisp-ovly-buffer o1))
        (should-not (nelisp-ovly-buffer o2))))))

;;; ─────────────────────────────────────────────────────────────────────
;;; §I. copy
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-ovly-copy-clones-range-and-plist ()
  (nelisp-ovly-test--with-clean-state
    (nelisp-ovly-test--with-buffer buf "abcdef"
      (let* ((src (nelisp-ovly-make 2 4 buf))
             (_   (nelisp-ovly-put src 'face 'bold))
             (dst (nelisp-ovly-copy src)))
        (should-not (eq src dst))
        (should (= (nelisp-ovly-start dst) 2))
        (should (= (nelisp-ovly-end   dst) 4))
        (should (eq (nelisp-ovly-get dst 'face) 'bold))
        ;; Mutating dst's props doesn't bleed into src.
        (nelisp-ovly-put dst 'face 'italic)
        (should (eq (nelisp-ovly-get src 'face) 'bold))
        (should (eq (nelisp-ovly-get dst 'face) 'italic))))))

(ert-deftest nelisp-ovly-copy-fresh-id-is-later ()
  (nelisp-ovly-test--with-clean-state
    (nelisp-ovly-test--with-buffer buf "abcdef"
      (let* ((src (nelisp-ovly-make 1 5 buf))
             (dst (nelisp-ovly-copy src)))
        ;; Same priority (= nil) → later id (dst) wins.
        (let ((sorted (nelisp-ovly-overlays-at 2 t)))
          (should (eq (car sorted) dst))
          (should (eq (cadr sorted) src)))))))

;;; ─────────────────────────────────────────────────────────────────────
;;; §J. provider hooks (display + keymap + get-char-property)
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-ovly-keymap-provider-returns-highest-priority ()
  (nelisp-ovly-test--with-clean-state
    (nelisp-ovly-test--with-buffer buf "abcdef"
      (let* ((km1 (make-sparse-keymap))
             (km2 (make-sparse-keymap))
             (lo  (nelisp-ovly-make 1 5 buf))
             (hi  (nelisp-ovly-make 1 5 buf)))
        (nelisp-ovly-put lo 'keymap km1)
        (nelisp-ovly-put lo 'priority 1)
        (nelisp-ovly-put hi 'keymap km2)
        (nelisp-ovly-put hi 'priority 5)
        (should (eq (nelisp-ovly-keymap-provider 2 buf) km2))))))

(ert-deftest nelisp-ovly-keymap-provider-falls-back-to-local-map ()
  (nelisp-ovly-test--with-clean-state
    (nelisp-ovly-test--with-buffer buf "abcdef"
      (let* ((km (make-sparse-keymap))
             (o (nelisp-ovly-make 1 5 buf)))
        (nelisp-ovly-put o 'local-map km)
        (should (eq (nelisp-ovly-keymap-provider 2 buf) km))))))

(ert-deftest nelisp-ovly-keymap-provider-prefers-keymap-over-local-map ()
  (nelisp-ovly-test--with-clean-state
    (nelisp-ovly-test--with-buffer buf "abcdef"
      (let* ((km1 (make-sparse-keymap))
             (km2 (make-sparse-keymap))
             (o (nelisp-ovly-make 1 5 buf)))
        (nelisp-ovly-put o 'keymap km1)
        (nelisp-ovly-put o 'local-map km2)
        (should (eq (nelisp-ovly-keymap-provider 2 buf) km1))))))

(ert-deftest nelisp-ovly-keymap-provider-returns-nil-when-absent ()
  (nelisp-ovly-test--with-clean-state
    (nelisp-ovly-test--with-buffer buf "abcdef"
      (nelisp-ovly-make 1 5 buf)
      (should-not (nelisp-ovly-keymap-provider 2 buf)))))

(ert-deftest nelisp-ovly-display-provider-roundtrip ()
  (nelisp-ovly-test--with-clean-state
    (nelisp-ovly-test--with-buffer buf "abcdef"
      (let ((o (nelisp-ovly-make 1 5 buf)))
        (nelisp-ovly-put o 'display "REPLACED")
        (should (equal (nelisp-ovly-display-provider 2 buf) "REPLACED"))))))

(ert-deftest nelisp-ovly-get-char-property-overlay-beats-textprop ()
  (nelisp-ovly-test--with-clean-state
    (nelisp-ovly-test--with-buffer buf "abcdef"
      (put-text-property 1 4 'face 'underline)
      (let ((o (nelisp-ovly-make 1 4 buf)))
        (nelisp-ovly-put o 'face 'bold)
        (should (eq (nelisp-ovly-get-char-property 2 'face buf) 'bold))))))

(ert-deftest nelisp-ovly-get-char-property-falls-back-to-textprop ()
  (nelisp-ovly-test--with-clean-state
    (nelisp-ovly-test--with-buffer buf "abcdef"
      (put-text-property 1 4 'face 'underline)
      ;; No overlay carries `face' at pos 2 → fallback to text-property.
      (should (eq (nelisp-ovly-get-char-property 2 'face buf) 'underline)))))

(ert-deftest nelisp-ovly-get-char-property-byte-identical-without-overlays ()
  (nelisp-ovly-test--with-clean-state
    (nelisp-ovly-test--with-buffer buf "abcdef"
      (put-text-property 2 5 'face 'underline)
      (dolist (pos '(1 2 3 4 5 6))
        (should
         (eq (nelisp-ovly-get-char-property pos 'face buf)
             (get-text-property pos 'face)))))))

;;; ─────────────────────────────────────────────────────────────────────
;;; §K. T146 install / uninstall
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-ovly-install-providers-wires-keymap-slot ()
  (nelisp-ovly-test--with-clean-state
    (nelisp-ovly-test--with-buffer buf "abcdef"
      (nelisp-ovly-install-providers)
      (should (eq nelisp-textprop-keymap--overlay-provider
                  #'nelisp-ovly-keymap-provider))
      (let ((km (make-sparse-keymap))
            (o (nelisp-ovly-make 1 5 buf)))
        (nelisp-ovly-put o 'keymap km)
        (nelisp-textprop-keymap-install)
        (let ((cells (nelisp-textprop-keymap-precedence-at 2 buf)))
          (should (assq nelisp-textprop-keymap-slot-overlay cells))
          (should (eq km
                      (cdr (assq nelisp-textprop-keymap-slot-overlay
                                 cells)))))
        (nelisp-textprop-keymap-uninstall)))))

(ert-deftest nelisp-ovly-uninstall-providers-clears-slot ()
  (nelisp-ovly-test--with-clean-state
    (nelisp-ovly-install-providers)
    (nelisp-ovly-uninstall-providers)
    (should-not nelisp-textprop-keymap--overlay-provider)))

;;; ─────────────────────────────────────────────────────────────────────
;;; §L. dead-overlay tolerance + signal semantics
;;; ─────────────────────────────────────────────────────────────────────

(ert-deftest nelisp-ovly-start-end-buffer-of-dead-is-nil ()
  (nelisp-ovly-test--with-clean-state
    (nelisp-ovly-test--with-buffer buf "abcdef"
      (let ((o (nelisp-ovly-make 1 3 buf)))
        (nelisp-ovly-delete o)
        (should-not (nelisp-ovly-start o))
        (should-not (nelisp-ovly-end o))
        (should-not (nelisp-ovly-buffer o))))))

(ert-deftest nelisp-ovly-move-on-dead-signals ()
  (nelisp-ovly-test--with-clean-state
    (nelisp-ovly-test--with-buffer buf "abcdef"
      (let ((o (nelisp-ovly-make 1 3 buf)))
        (nelisp-ovly-delete o)
        (should-error (nelisp-ovly-move o 2 4)
                      :type 'nelisp-ovly-dead)))))

(ert-deftest nelisp-ovly-copy-on-dead-signals ()
  (nelisp-ovly-test--with-clean-state
    (nelisp-ovly-test--with-buffer buf "abcdef"
      (let ((o (nelisp-ovly-make 1 3 buf)))
        (nelisp-ovly-delete o)
        (should-error (nelisp-ovly-copy o)
                      :type 'nelisp-ovly-dead)))))

(ert-deftest nelisp-ovly-put-get-on-dead-is-tolerant ()
  (nelisp-ovly-test--with-clean-state
    (nelisp-ovly-test--with-buffer buf "abcdef"
      (let ((o (nelisp-ovly-make 1 3 buf)))
        (nelisp-ovly-delete o)
        ;; Should NOT signal — Emacs precedent.
        (should (eq (nelisp-ovly-put o 'face 'bold) 'bold))
        (should (eq (nelisp-ovly-get o 'face) 'bold))))))

(ert-deftest nelisp-ovly-delete-is-idempotent ()
  (nelisp-ovly-test--with-clean-state
    (nelisp-ovly-test--with-buffer buf "abcdef"
      (let ((o (nelisp-ovly-make 1 3 buf)))
        (nelisp-ovly-delete o)
        (nelisp-ovly-delete o)
        (should-not (nelisp-ovly-buffer o))))))

(provide 'nelisp-overlay-test)

;;; nelisp-overlay-test.el ends here
