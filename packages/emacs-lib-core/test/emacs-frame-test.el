;;; emacs-frame-test.el --- ERT tests for emacs-frame.el  -*- lexical-binding: t; -*-

;; Phase 1 module 3/6 tests per nelisp-emacs Doc 01 (LOCKED v2).
;; Covers all 8 categories of `emacs-frame-*' API across 25+ tests:
;;   A. frame query                (4 tests)
;;   B. frame creation/deletion    (5 tests)
;;   C. frame size                 (4 tests)
;;   D. frame position             (1 test)
;;   E. frame parameters           (3 tests)
;;   F. frame visibility / Z-order (4 tests)
;;   G. frame selection / focus    (2 tests)
;;   H. frame->windows + display   (2 tests)
;;   X. backend swap-in / invariant (3 tests)

(require 'ert)
(require 'cl-lib)
(require 'emacs-frame)

;;; Fresh-world fixture

(defmacro emacs-frame-test--with-fresh-world (&rest body)
  "Run BODY with a clean emacs-frame state."
  (declare (indent 0) (debug (body)))
  `(let ((emacs-frame--id-counter       0)
         (emacs-frame--registry         nil)
         (emacs-frame--selected         nil)
         (emacs-frame--focus            nil)
         (emacs-frame--backend-dispatch nil))
     ,@body))

;;;; A. frame query (4 tests)

(ert-deftest emacs-frame-framep-true-and-false ()
  (emacs-frame-test--with-fresh-world
    (let ((f (emacs-frame-selected-frame)))
      (should (emacs-frame-framep f))
      (should-not (emacs-frame-framep 'symbol))
      (should-not (emacs-frame-framep nil))
      (should-not (emacs-frame-framep 42)))))

(ert-deftest emacs-frame-selected-frame-auto-creates ()
  (emacs-frame-test--with-fresh-world
    ;; Registry is empty before the call.
    (should (null emacs-frame--registry))
    (let ((f (emacs-frame-selected-frame)))
      (should (emacs-frame-p f))
      (should (eq f (emacs-frame-selected-frame)))
      (should (= 1 (length (emacs-frame-frame-list)))))))

(ert-deftest emacs-frame-frame-list-tracks-creates-and-deletes ()
  (emacs-frame-test--with-fresh-world
    (let ((f1 (emacs-frame-selected-frame))
          (f2 (emacs-frame-make-frame))
          (f3 (emacs-frame-make-frame)))
      (should (equal (list f1 f2 f3) (emacs-frame-frame-list)))
      (emacs-frame-delete-frame f2)
      (should (equal (list f1 f3) (emacs-frame-frame-list))))))

(ert-deftest emacs-frame-window-frame-resolves-stub-sentinel ()
  (emacs-frame-test--with-fresh-world
    (let ((sel (emacs-frame-selected-frame)))
      (should (eq sel (emacs-frame-window-frame nil)))
      (should (eq sel (emacs-frame-window-frame
                       'emacs-window--default-frame)))
      (should (eq sel (emacs-frame-window-frame sel)))
      ;; Unknown opaque object returns nil rather than signalling.
      (should (null (emacs-frame-window-frame 'unknown-window-obj))))))

;;;; B. frame creation / deletion (5 tests)

(ert-deftest emacs-frame-make-frame-default-is-80x24 ()
  (emacs-frame-test--with-fresh-world
    (let ((f (emacs-frame-make-frame)))
      (should (= 80 (emacs-frame-frame-width  f)))
      (should (= 24 (emacs-frame-frame-height f))))))

(ert-deftest emacs-frame-make-frame-applies-params ()
  (emacs-frame-test--with-fresh-world
    (let ((f (emacs-frame-make-frame
              '((width . 100) (height . 40) (name . "alpha")))))
      (should (= 100 (emacs-frame-frame-width  f)))
      (should (= 40  (emacs-frame-frame-height f)))
      (should (equal "alpha" (emacs-frame-frame-parameter f 'name))))))

(ert-deftest emacs-frame-make-frame-id-is-unique-and-not-recycled ()
  (emacs-frame-test--with-fresh-world
    (let* ((f1 (emacs-frame-selected-frame))
           (id1 (emacs-frame-id f1))
           (f2 (emacs-frame-make-frame))
           (id2 (emacs-frame-id f2)))
      (should (/= id1 id2))
      (emacs-frame-delete-frame f2)
      ;; Doc 34 §2.11 invariant: id is NOT recycled.
      (let ((f3 (emacs-frame-make-frame)))
        (should-not (memq (emacs-frame-id f3) (list id1 id2)))))))

(ert-deftest emacs-frame-delete-frame-rejects-sole-frame ()
  (emacs-frame-test--with-fresh-world
    (let ((f (emacs-frame-selected-frame)))
      (should-error (emacs-frame-delete-frame f)
                    :type 'emacs-frame-only))))

(ert-deftest emacs-frame-delete-other-frames-keeps-target ()
  (emacs-frame-test--with-fresh-world
    (let ((f1 (emacs-frame-selected-frame)))
      (emacs-frame-make-frame)
      (emacs-frame-make-frame)
      (should (= 3 (length (emacs-frame-frame-list))))
      (emacs-frame-delete-other-frames f1)
      (should (equal (list f1) (emacs-frame-frame-list))))))

;;;; C. frame size (4 tests)

(ert-deftest emacs-frame-default-dimensions ()
  (emacs-frame-test--with-fresh-world
    (let ((f (emacs-frame-selected-frame)))
      (should (= 80 (emacs-frame-frame-width  f)))
      (should (= 24 (emacs-frame-frame-height f))))))

(ert-deftest emacs-frame-pixel-derivations ()
  (emacs-frame-test--with-fresh-world
    (let ((f (emacs-frame-selected-frame)))
      (should (= (* 80 emacs-frame--char-width)
                 (emacs-frame-frame-pixel-width f)))
      (should (= (* 24 emacs-frame--char-height)
                 (emacs-frame-frame-pixel-height f)))
      (should (= emacs-frame--char-width
                 (emacs-frame-frame-char-width f)))
      (should (= emacs-frame--char-height
                 (emacs-frame-frame-char-height f))))))

(ert-deftest emacs-frame-set-frame-size-updates-pixel ()
  (emacs-frame-test--with-fresh-world
    (let ((f (emacs-frame-selected-frame)))
      (emacs-frame-set-frame-size f 120 50)
      (should (= 120 (emacs-frame-frame-width  f)))
      (should (= 50  (emacs-frame-frame-height f)))
      (should (= (* 120 emacs-frame--char-width)
                 (emacs-frame-frame-pixel-width f)))
      (should (= (* 50  emacs-frame--char-height)
                 (emacs-frame-frame-pixel-height f))))))

(ert-deftest emacs-frame-set-frame-size-rejects-too-small ()
  (emacs-frame-test--with-fresh-world
    (let ((f (emacs-frame-selected-frame)))
      (should-error (emacs-frame-set-frame-size f 0 50)
                    :type 'emacs-frame-bad-size)
      (should-error (emacs-frame-set-frame-size f 80 0)
                    :type 'emacs-frame-bad-size))))

;;;; D. frame position (1 test)

(ert-deftest emacs-frame-set-frame-position-updates-left-top ()
  (emacs-frame-test--with-fresh-world
    (let ((f (emacs-frame-selected-frame)))
      (emacs-frame-set-frame-position f 50 25)
      (should (= 50 (emacs-frame-left f)))
      (should (= 25 (emacs-frame-top  f)))
      (should-error (emacs-frame-set-frame-position f "x" 0)
                    :type 'wrong-type-argument))))

;;;; E. frame parameters (3 tests)

(ert-deftest emacs-frame-frame-parameters-includes-derived ()
  (emacs-frame-test--with-fresh-world
    (let* ((f (emacs-frame-make-frame '((foo . 1))))
           (p (emacs-frame-frame-parameters f)))
      (should (equal 1   (cdr (assq 'foo p))))
      (should (equal 80  (cdr (assq 'width  p))))
      (should (equal 24  (cdr (assq 'height p))))
      (should (assq 'pixel-width  p))
      (should (assq 'pixel-height p))
      (should (assq 'visibility   p)))))

(ert-deftest emacs-frame-set-frame-parameter-roundtrip ()
  (emacs-frame-test--with-fresh-world
    (let ((f (emacs-frame-selected-frame)))
      (emacs-frame-set-frame-parameter f 'background-color "white")
      (should (equal "white"
                     (emacs-frame-frame-parameter f 'background-color)))
      ;; Setting again replaces, doesn't duplicate.
      (emacs-frame-set-frame-parameter f 'background-color "black")
      (should (equal "black"
                     (emacs-frame-frame-parameter f 'background-color)))
      (let ((occurs (cl-count-if
                     (lambda (kv) (eq 'background-color (car kv)))
                     (emacs-frame-parameters f))))
        (should (= 1 occurs))))))

(ert-deftest emacs-frame-modify-frame-parameters-bulk ()
  (emacs-frame-test--with-fresh-world
    (let ((f (emacs-frame-selected-frame)))
      (emacs-frame-modify-frame-parameters
       f '((width . 90) (height . 30) (custom . "x")))
      (should (= 90 (emacs-frame-frame-width  f)))
      (should (= 30 (emacs-frame-frame-height f)))
      (should (equal "x" (emacs-frame-frame-parameter f 'custom))))))

;;;; F. frame visibility / Z-order (4 tests)

(ert-deftest emacs-frame-visibility-default-and-toggle ()
  (emacs-frame-test--with-fresh-world
    (let ((f (emacs-frame-selected-frame)))
      (should (emacs-frame-frame-visible-p f))
      (emacs-frame-make-frame-invisible f)
      (should-not (emacs-frame-frame-visible-p f))
      (emacs-frame-make-frame-visible f)
      (should (emacs-frame-frame-visible-p f)))))

(ert-deftest emacs-frame-make-frame-with-iconified-visibility ()
  (emacs-frame-test--with-fresh-world
    (let ((f (emacs-frame-make-frame '((visibility . iconified)))))
      (should (eq 'iconified (emacs-frame-visible f)))
      ;; iconified still counts as visible (matches Emacs).
      (should (emacs-frame-frame-visible-p f)))))

(ert-deftest emacs-frame-raise-frame-moves-to-back ()
  (emacs-frame-test--with-fresh-world
    (let* ((f1 (emacs-frame-selected-frame))
           (f2 (emacs-frame-make-frame))
           (f3 (emacs-frame-make-frame)))
      (emacs-frame-raise-frame f1)
      (should (equal (list f2 f3 f1) emacs-frame--registry))
      ;; Selection unchanged by raise.
      (should (eq f1 (emacs-frame-selected-frame))))))

(ert-deftest emacs-frame-lower-frame-moves-to-front ()
  (emacs-frame-test--with-fresh-world
    (let* ((f1 (emacs-frame-selected-frame))
           (f2 (emacs-frame-make-frame))
           (f3 (emacs-frame-make-frame)))
      (emacs-frame-lower-frame f3)
      (should (equal (list f3 f1 f2) emacs-frame--registry)))))

;;;; G. frame selection / focus (2 tests)

(ert-deftest emacs-frame-select-frame-changes-selection ()
  (emacs-frame-test--with-fresh-world
    (let* ((f1 (emacs-frame-selected-frame))
           (f2 (emacs-frame-make-frame)))
      (should (eq f1 (emacs-frame-selected-frame)))
      (emacs-frame-select-frame f2)
      (should (eq f2 (emacs-frame-selected-frame)))
      (should (eq f2 (emacs-frame-frame-focus))))))

(ert-deftest emacs-frame-select-frame-rejects-dead ()
  (emacs-frame-test--with-fresh-world
    (let* ((f1 (emacs-frame-selected-frame))
           (f2 (emacs-frame-make-frame)))
      (emacs-frame-delete-frame f2)
      (should-error (emacs-frame-select-frame f2)
                    :type 'emacs-frame-dead)
      ;; selected_frame fell back to f1 after the delete.
      (should (eq f1 (emacs-frame-selected-frame))))))

;;;; H. frame->windows + display (2 tests)

(ert-deftest emacs-frame-frame-windows-empty-when-no-window-module ()
  (emacs-frame-test--with-fresh-world
    (let ((f (emacs-frame-selected-frame)))
      ;; emacs-window is not (require)d here, so the result depends
      ;; on whether some other test has loaded it -- accept either
      ;; nil or a list of objects.  Either way, calling must not
      ;; signal.
      (let ((windows (emacs-frame-frame-windows f)))
        (should (or (null windows) (listp windows)))))))

(ert-deftest emacs-frame-display-pixel-dimensions-are-stable ()
  (emacs-frame-test--with-fresh-world
    (should (= emacs-frame--display-cols  (emacs-frame-display-pixel-width)))
    (should (= emacs-frame--display-lines (emacs-frame-display-pixel-height)))))

;;;; X. backend swap-in / Doc 34 §2.11 + Doc 43 §2.1 invariants (3 tests)

(ert-deftest emacs-frame-stub-backend-default ()
  (emacs-frame-test--with-fresh-world
    (let ((f (emacs-frame-selected-frame)))
      (should (eq 'stub (emacs-frame-current-backend)))
      (should (eq 'stub (emacs-frame-backend f)))
      ;; framep returns the backend symbol, mirroring Emacs.
      (should (eq 'stub (emacs-frame-framep f)))
      (should (eq 'stub (emacs-frame-frame-live-p f))))))

(ert-deftest emacs-frame-set-backend-dispatch-records-name ()
  (emacs-frame-test--with-fresh-world
    (let* ((f1 (emacs-frame-selected-frame))
           (calls nil)
           (dispatch
            (list :name             'tui
                  :frame-create     (lambda (f params)
                                      (push (list 'create f params) calls)
                                      'tui-handle)
                  :frame-resize     (lambda (f c l)
                                      (push (list 'resize f c l) calls))
                  :frame-destroy    (lambda (f)
                                      (push (list 'destroy f) calls))
                  :capability-query (lambda (cap)
                                      (memq cap '(frame-create
                                                  frame-destroy
                                                  frame-resize
                                                  truecolor))))))
      (emacs-frame-set-backend-dispatch dispatch)
      (should (eq 'tui (emacs-frame-current-backend)))
      (should (eq 'tui (emacs-frame-backend f1)))
      (should (emacs-frame-capability-p 'truecolor))
      (should-not (emacs-frame-capability-p 'mouse))
      ;; New frame goes through the dispatch.
      (let ((f2 (emacs-frame-make-frame '((width . 90)))))
        (emacs-frame-set-frame-size f2 100 30)
        (emacs-frame-delete-frame f2)
        (should (assoc 'create calls))
        (should (assoc 'resize calls))
        (should (assoc 'destroy calls))
        (should (eq 'tui-handle (emacs-frame-backend-obj f2)))))))

(ert-deftest emacs-frame-stub-invariant-locked ()
  "Doc 34 §2.11 stub-mode invariant: 80x24 + unique frame-id."
  (emacs-frame-test--with-fresh-world
    (let* ((f (emacs-frame-selected-frame))
           (g (emacs-frame-make-frame)))
      ;; (1) stub default size = 80x24.
      (should (= 80 (emacs-frame-frame-width  f)))
      (should (= 24 (emacs-frame-frame-height f)))
      ;; (2) frame-id is unique across frames.
      (should (/= (emacs-frame-id f) (emacs-frame-id g)))
      ;; (3) delete-frame in stub mode is registry-only (no signal).
      (emacs-frame-delete-frame g)
      (should (emacs-frame-dead-p g))
      ;; (4) capabilities default to the stub-mode minimum core.
      (should (emacs-frame-capability-p 'frame-create))
      (should (emacs-frame-capability-p 'frame-resize))
      (should-not (emacs-frame-capability-p 'truecolor)))))

(provide 'emacs-frame-test)
;;; emacs-frame-test.el ends here
