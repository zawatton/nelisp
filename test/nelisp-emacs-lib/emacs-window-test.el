;;; emacs-window-test.el --- ERT tests for emacs-window.el  -*- lexical-binding: t; -*-

;; Phase 1 module 2/6 tests per nelisp-emacs Doc 01 (LOCKED v2).
;; Covers all 5 categories of `emacs-window-*' API across 28+ tests:
;;   A. window query              (8 tests)
;;   B. window split / delete     (9 tests)
;;   C. window size / position    (5 tests)
;;   D. window-local config       (5 tests)
;;   E. window selection          (4 tests)
;;   X. error / edge-case         (3 tests)

(require 'ert)
(require 'emacs-window)

;;; Fresh-world fixture (resets every global in both modules)

(defmacro emacs-window-test--with-fresh-world (&rest body)
  "Run BODY with a clean nelisp-ec + emacs-window state."
  (declare (indent 0) (debug (body)))
  `(let ((nelisp-ec--buffers nil)
         (nelisp-ec--current-buffer nil)
         (emacs-window--id-counter 0)
         (emacs-window--root nil)
         (emacs-window--selected nil))
     ,@body))

(defmacro emacs-window-test--with-3-buffers (vars &rest body)
  "Bind VARS to three fresh `nelisp-ec-buffer' objects."
  (declare (indent 1) (debug ((&rest symbolp) body)))
  `(let* ,(cl-loop for v in vars for i from 1
                   collect `(,v (nelisp-ec-generate-new-buffer
                                 ,(format "t-%d" i))))
     ,@body))

;;;; A. window query (8 tests)

(ert-deftest emacs-window-windowp-true-and-false ()
  (emacs-window-test--with-fresh-world
    (let ((w (emacs-window-selected-window)))
      (should (emacs-window-windowp w))
      (should-not (emacs-window-windowp 'symbol))
      (should-not (emacs-window-windowp nil)))))

(ert-deftest emacs-window-create-and-select ()
  (emacs-window-test--with-fresh-world
    (let ((w (emacs-window-selected-window)))
      (should (emacs-window-p w))
      (should (emacs-window-leaf-p w))
      (should (eq w (emacs-window-selected-window))))))

(ert-deftest emacs-window-buffer-binding ()
  (emacs-window-test--with-fresh-world
    (emacs-window-test--with-3-buffers (b1)
      (let ((w (emacs-window-selected-window)))
        (emacs-window-set-window-buffer w b1)
        (should (eq b1 (emacs-window-window-buffer w)))))))

(ert-deftest emacs-window-frame-stub ()
  (emacs-window-test--with-fresh-world
    (let ((w (emacs-window-selected-window)))
      (should (eq emacs-window--frame (emacs-window-window-frame w))))))

(ert-deftest emacs-window-list-iterates ()
  (emacs-window-test--with-fresh-world
    (emacs-window-test--with-3-buffers (b1 b2 b3)
      (let ((w (emacs-window-selected-window)))
        (emacs-window-set-window-buffer w b1))
      (emacs-window-set-window-buffer (emacs-window-split-window) b2)
      (emacs-window-set-window-buffer (emacs-window-split-window) b3)
      (should (= 3 (length (emacs-window-window-list)))))))

(ert-deftest emacs-window-next-and-previous-wrap ()
  (emacs-window-test--with-fresh-world
    (let ((w1 (emacs-window-selected-window)))
      (let ((w2 (emacs-window-split-window))
            (w3 (emacs-window-split-window)))
        ;; tree order = (w1 w3 w2) since w3 inserted between w1 and w2.
        (let ((order (emacs-window-window-list nil nil w1)))
          (should (eq w1 (car order)))
          (should (eq w1 (emacs-window-next-window
                          (car (last order)))))
          (should (eq (car (last order))
                      (emacs-window-previous-window w1))))))))

(ert-deftest emacs-window-get-buffer-window-finds-it ()
  (emacs-window-test--with-fresh-world
    (emacs-window-test--with-3-buffers (b1 b2)
      (let ((w (emacs-window-selected-window)))
        (emacs-window-set-window-buffer w b1)
        (let ((w2 (emacs-window-split-window)))
          (emacs-window-set-window-buffer w2 b2)
          (should (eq w  (emacs-window-get-buffer-window b1)))
          (should (eq w2 (emacs-window-get-buffer-window b2)))
          (should (null (emacs-window-get-buffer-window
                         (nelisp-ec-generate-new-buffer "z"))))
          (should (= 1 (length (emacs-window-get-buffer-window-list b1)))))))))

(ert-deftest emacs-window-get-buffer-window-by-name ()
  (emacs-window-test--with-fresh-world
    (emacs-window-test--with-3-buffers (b1)
      (let ((w (emacs-window-selected-window)))
        (emacs-window-set-window-buffer w b1)
        (should (eq w (emacs-window-get-buffer-window
                       (nelisp-ec-buffer-name b1))))))))

;;;; B. window split / delete (9 tests)

(ert-deftest emacs-window-split-vertical ()
  (emacs-window-test--with-fresh-world
    (let* ((w1 (emacs-window-selected-window))
           (w2 (emacs-window-split-window nil nil 'below)))
      (should (= 2 (length (emacs-window-window-list))))
      (should (= 12 (emacs-window-window-height w1)))
      (should (= 12 (emacs-window-window-height w2)))
      (should (= 80 (emacs-window-window-width  w1)))
      (should (= 80 (emacs-window-window-width  w2)))
      ;; share a parent of direction = vertical
      (should (eq 'vertical
                  (emacs-window-direction (emacs-window-parent w1)))))))

(ert-deftest emacs-window-split-horizontal ()
  (emacs-window-test--with-fresh-world
    (let* ((w1 (emacs-window-selected-window))
           (w2 (emacs-window-split-window nil nil 'right)))
      (should (= 2 (length (emacs-window-window-list))))
      (should (= 40 (emacs-window-window-width w1)))
      (should (= 40 (emacs-window-window-width w2)))
      (should (eq 'horizontal
                  (emacs-window-direction (emacs-window-parent w2)))))))

(ert-deftest emacs-window-split-with-explicit-size ()
  (emacs-window-test--with-fresh-world
    (let* ((w1 (emacs-window-selected-window))
           (w2 (emacs-window-split-window nil 5 'below)))
      (should (= 5  (emacs-window-window-height w2)))
      (should (= 19 (emacs-window-window-height w1))))))

(ert-deftest emacs-window-split-too-small-errors ()
  (emacs-window-test--with-fresh-world
    (emacs-window-selected-window)
    (should-error (emacs-window-split-window nil 1000 'below)
                  :type 'emacs-window-too-small)))

(ert-deftest emacs-window-delete-window ()
  (emacs-window-test--with-fresh-world
    (let* ((w1 (emacs-window-selected-window))
           (w2 (emacs-window-split-window nil nil 'below)))
      (emacs-window-delete-window w2)
      (should (= 1 (length (emacs-window-window-list))))
      (should (eq w1 (emacs-window-selected-window)))
      ;; The surviving window has reabsorbed the other's lines.
      (should (= 24 (emacs-window-window-height w1))))))

(ert-deftest emacs-window-delete-window-rejects-sole-window ()
  (emacs-window-test--with-fresh-world
    (let ((w (emacs-window-selected-window)))
      (should-error (emacs-window-delete-window w)
                    :type 'emacs-window-only))))

(ert-deftest split-window-below-creates-second-window ()
  (emacs-window-test--with-fresh-world
    (let ((w1 (emacs-window-selected-window)))
      (let ((w2 (split-window-below)))
        (should (= 2 (length (emacs-window-window-list))))
        (should (eq w1 (emacs-window-selected-window)))
        (should (= 12 (emacs-window-window-height w1)))
        (should (= 12 (emacs-window-window-height w2)))
        (should (eq 'vertical
                    (emacs-window-direction (emacs-window-parent w2))))))))

(ert-deftest split-window-right-creates-second-window ()
  (emacs-window-test--with-fresh-world
    (let ((w1 (emacs-window-selected-window)))
      (let ((w2 (split-window-right)))
        (should (= 2 (length (emacs-window-window-list))))
        (should (eq w1 (emacs-window-selected-window)))
        (should (= 40 (emacs-window-window-width w1)))
        (should (= 40 (emacs-window-window-width w2)))
        (should (eq 'horizontal
                    (emacs-window-direction (emacs-window-parent w2))))))))

(ert-deftest other-window-cycles-forward ()
  (emacs-window-test--with-fresh-world
    (let* ((w1 (emacs-window-selected-window))
           (w2 (split-window-below)))
      (should (eq w1 (emacs-window-selected-window)))
      (should (eq w2 (other-window)))
      (should (eq w2 (emacs-window-selected-window)))
      (should (eq w1 (other-window 1)))
      (should (eq w1 (emacs-window-selected-window))))))

(ert-deftest other-window-with-negative-cycles-back ()
  (emacs-window-test--with-fresh-world
    (let* ((w1 (emacs-window-selected-window))
           (w2 (split-window-below))
           (w3 (split-window-right)))
      ;; tree order = (w1 w3 w2); selected = w1
      (should (eq w2 (other-window -1)))
      (should (eq w2 (emacs-window-selected-window)))
      (should (eq w3 (other-window -1)))
      ;; from w3, +1 step forward in (w1 w3 w2) → w2
      (should (eq w2 (other-window 1)))
      ;; from w2, +1 step forward → w1 (wrap)
      (should (eq w1 (other-window 1))))))

(ert-deftest emacs-window-delete-other-windows ()
  (emacs-window-test--with-fresh-world
    (let ((w (emacs-window-selected-window)))
      (emacs-window-split-window)
      (emacs-window-split-window)
      (should (= 3 (length (emacs-window-window-list))))
      (emacs-window-delete-other-windows w)
      (should (= 1 (length (emacs-window-window-list))))
      (should (eq w (emacs-window-selected-window))))))

(ert-deftest delete-window-leaves-one ()
  (emacs-window-test--with-fresh-world
    (let ((w1 (emacs-window-selected-window)))
      (let ((w2 (split-window-below)))
        (delete-window)
        (should (= 1 (length (emacs-window-window-list))))
        (should (eq w2 (emacs-window-selected-window)))
        (should-not (emacs-window-windowp w1))
        (should (= 24 (emacs-window-window-height w2)))))))

(ert-deftest delete-other-windows-leaves-one ()
  (emacs-window-test--with-fresh-world
    (let ((w1 (emacs-window-selected-window)))
      (split-window-below)
      (other-window)
      (split-window-right)
      (other-window -1)
      (delete-other-windows)
      (should (= 1 (length (emacs-window-window-list))))
      (should (eq w1 (emacs-window-selected-window)))
      (should (= 80 (emacs-window-window-width w1)))
      (should (= 24 (emacs-window-window-height w1))))))

(ert-deftest split-window-then-delete-window-restores-single-window ()
  (emacs-window-test--with-fresh-world
    (let ((w1 (emacs-window-selected-window)))
      (split-window-right)
      (other-window)
      (delete-window)
      (should (= 1 (length (emacs-window-window-list))))
      (should (eq w1 (emacs-window-selected-window)))
      (should (= 80 (emacs-window-window-width w1)))
      (should (= 24 (emacs-window-window-height w1))))))

(ert-deftest emacs-window-delete-windows-on ()
  (emacs-window-test--with-fresh-world
    (emacs-window-test--with-3-buffers (b1 b2)
      (let ((w (emacs-window-selected-window)))
        (emacs-window-set-window-buffer w b1)
        (emacs-window-set-window-buffer (emacs-window-split-window) b2)
        (emacs-window-delete-windows-on b2)
        (should (= 1 (length (emacs-window-window-list))))
        (should (eq w (emacs-window-selected-window)))))))

(ert-deftest emacs-window-one-window-p ()
  (emacs-window-test--with-fresh-world
    (emacs-window-selected-window)
    (should (emacs-window-one-window-p))
    (emacs-window-split-window)
    (should-not (emacs-window-one-window-p))))

;;;; C. window size / position (5 tests)

(ert-deftest emacs-window-default-dimensions ()
  (emacs-window-test--with-fresh-world
    (let ((w (emacs-window-selected-window)))
      (should (= 80 (emacs-window-window-width  w)))
      (should (= 24 (emacs-window-window-height w))))))

(ert-deftest emacs-window-pixel-derivations ()
  (emacs-window-test--with-fresh-world
    (let ((w (emacs-window-selected-window)))
      (should (= (* 80 emacs-window--pixel-col-px)
                 (emacs-window-window-pixel-width  w)))
      (should (= (* 24 emacs-window--pixel-line-px)
                 (emacs-window-window-pixel-height w))))))

(ert-deftest emacs-window-edges-after-vsplit ()
  (emacs-window-test--with-fresh-world
    (let* ((w1 (emacs-window-selected-window))
           (w2 (emacs-window-split-window nil nil 'below)))
      (should (equal '(0 0 80 12)  (emacs-window-window-edges w1)))
      (should (equal '(0 12 80 24) (emacs-window-window-edges w2))))))

(ert-deftest emacs-window-edges-after-hsplit ()
  (emacs-window-test--with-fresh-world
    (let* ((w1 (emacs-window-selected-window))
           (w2 (emacs-window-split-window nil nil 'right)))
      (should (equal '(0  0 40 24) (emacs-window-window-edges w1)))
      (should (equal '(40 0 80 24) (emacs-window-window-edges w2))))))

(ert-deftest emacs-window-resizable-clamps ()
  (emacs-window-test--with-fresh-world
    (let ((w (emacs-window-selected-window)))
      (should (= 5  (emacs-window-window-resizable w 5)))
      ;; minimum is 1, current = 24 ⇒ delta -23 still legal,
      ;; -100 is clamped to (- min cur) = -23.
      (should (= -23 (emacs-window-window-resizable w -100))))))

(ert-deftest emacs-window-point-tracking ()
  (emacs-window-test--with-fresh-world
    (let ((w (emacs-window-selected-window)))
      (emacs-window-set-window-point w 42)
      (emacs-window-set-window-start w 17)
      (should (= 42 (emacs-window-window-point w)))
      (should (= 17 (emacs-window-window-start w))))))

;;;; D. window-local config (5 tests)

(ert-deftest emacs-window-set-window-buffer-by-string-name ()
  (emacs-window-test--with-fresh-world
    (emacs-window-test--with-3-buffers (b1)
      (let ((w (emacs-window-selected-window)))
        (emacs-window-set-window-buffer w (nelisp-ec-buffer-name b1))
        (should (eq b1 (emacs-window-window-buffer w)))))))

(ert-deftest emacs-window-set-window-buffer-unknown-name-errors ()
  (emacs-window-test--with-fresh-world
    (let ((w (emacs-window-selected-window)))
      (should-error (emacs-window-set-window-buffer w "no-such-buffer")
                    :type 'emacs-window-error))))

(ert-deftest emacs-window-parameter-roundtrip ()
  (emacs-window-test--with-fresh-world
    (let ((w (emacs-window-selected-window)))
      (should (null (emacs-window-window-parameter w 'k)))
      (emacs-window-set-window-parameter w 'k 'v)
      (should (eq 'v (emacs-window-window-parameter w 'k)))
      ;; overwrite
      (emacs-window-set-window-parameter w 'k 99)
      (should (= 99 (emacs-window-window-parameter w 'k))))))

(ert-deftest emacs-window-config-save-restore ()
  (emacs-window-test--with-fresh-world
    (emacs-window-test--with-3-buffers (b1 b2)
      (let ((w1 (emacs-window-selected-window)))
        (emacs-window-set-window-buffer w1 b1)
        (let ((w2 (emacs-window-split-window)))
          (emacs-window-set-window-buffer w2 b2)
          (emacs-window-select-window w2)
          (let ((cfg (emacs-window-current-window-configuration)))
            (should (emacs-window-configuration-p cfg))
            ;; mutate state heavily
            (emacs-window-delete-other-windows w1)
            (should (= 1 (length (emacs-window-window-list))))
            ;; restore
            (emacs-window-set-window-configuration cfg)
            (should (= 2 (length (emacs-window-window-list))))
            ;; selected window is the leaf whose id matches w2's id at
            ;; snapshot — buffer should be b2
            (should (eq b2 (emacs-window-window-buffer
                            (emacs-window-selected-window))))))))))

(ert-deftest emacs-window-config-survives-deep-copy ()
  ;; mutating restored tree must NOT affect the snapshot.
  (emacs-window-test--with-fresh-world
    (emacs-window-selected-window)
    (emacs-window-split-window)
    (let ((cfg (emacs-window-current-window-configuration)))
      (emacs-window-set-window-configuration cfg)
      (let ((restored-root emacs-window--root))
        (emacs-window-delete-other-windows)
        (should-not (eq restored-root emacs-window--root))
        ;; snapshot.root is still a 2-leaf tree
        (should (= 2 (length
                      (emacs-window--leaves-of
                       (emacs-window-configuration-root cfg)))))))))

;;;; E. window selection (4 tests)

(ert-deftest emacs-window-select-window-changes-selection ()
  (emacs-window-test--with-fresh-world
    (let* ((w1 (emacs-window-selected-window))
           (w2 (emacs-window-split-window)))
      (emacs-window-select-window w2)
      (should (eq w2 (emacs-window-selected-window)))
      (emacs-window-select-window w1)
      (should (eq w1 (emacs-window-selected-window))))))

(ert-deftest emacs-window-select-window-rejects-deleted ()
  (emacs-window-test--with-fresh-world
    (let* ((w1 (emacs-window-selected-window))
           (w2 (emacs-window-split-window)))
      (emacs-window-delete-window w2)
      (should-error (emacs-window-select-window w2)
                    :type 'emacs-window-deleted)
      (should (eq w1 (emacs-window-selected-window))))))

(ert-deftest emacs-window-save-selected-window-restores ()
  (emacs-window-test--with-fresh-world
    (let* ((w1 (emacs-window-selected-window))
           (w2 (emacs-window-split-window)))
      (emacs-window-save-selected-window
       (emacs-window-select-window w2)
       (should (eq w2 (emacs-window-selected-window))))
      (should (eq w1 (emacs-window-selected-window))))))

(ert-deftest emacs-window-with-selected-window-restores ()
  (emacs-window-test--with-fresh-world
    (let* ((w1 (emacs-window-selected-window))
           (w2 (emacs-window-split-window))
           (saw nil))
      (emacs-window-with-selected-window w2
        (setq saw (emacs-window-selected-window)))
      (should (eq w2 saw))
      (should (eq w1 (emacs-window-selected-window))))))

;;;; X. error / edge-case (3 tests)

(ert-deftest emacs-window-balance-windows-equalizes-3-vsplit ()
  (emacs-window-test--with-fresh-world
    (let ((w1 (emacs-window-selected-window)))
      (emacs-window-split-window nil 4 'below)
      (emacs-window-split-window nil 7 'below)
      (emacs-window-balance-windows)
      (let ((heights (mapcar #'emacs-window-window-height
                             (emacs-window-window-list))))
        ;; 24 ÷ 3 = 8 each, with at most 0 leftover.  All values should be
        ;; in {8} since 24 is divisible.
        (should (cl-every (lambda (h) (= h 8)) heights))))))

(ert-deftest emacs-window-delete-collapses-singleton-parent ()
  ;; After:  vsplit -> hsplit on lower half -> delete one of the hsplit
  ;; children.  The parent hsplit has only one child and must be spliced
  ;; out so the surviving leaf becomes a direct child of the vsplit.
  (emacs-window-test--with-fresh-world
    (let* ((w1 (emacs-window-selected-window))
           (w2 (emacs-window-split-window nil nil 'below))
           (w3 (emacs-window-split-window w2 nil 'right)))
      (emacs-window-delete-window w3)
      (let ((leaves (emacs-window--all-leaves)))
        (should (= 2 (length leaves)))
        ;; w2's parent should now be the original vsplit, not the
        ;; collapsed hsplit.
        (should (eq 'vertical
                    (emacs-window-direction (emacs-window-parent w2))))))))

(ert-deftest emacs-window-window-end-clamps-to-buffer ()
  (emacs-window-test--with-fresh-world
    (emacs-window-test--with-3-buffers (b1)
      (nelisp-ec-with-current-buffer b1
        (nelisp-ec-insert "hi"))
      (let ((w (emacs-window-selected-window)))
        (emacs-window-set-window-buffer w b1)
        ;; buffer size = 2 ⇒ end clamp to 3
        (should (= 3 (emacs-window-window-end w)))))))

(provide 'emacs-window-test)

;;; emacs-window-test.el ends here
