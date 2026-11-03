;;; emacs-redisplay-core-smoke.el --- isolated ERT for lightweight redisplay core  -*- lexical-binding: t; -*-

;;; Commentary:

;; The lightweight core defines the same public API names as the full
;; redisplay engine, so it must be exercised in an isolated process
;; rather than loaded into the full host ERT suite.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emacs-redisplay-core)

(defvar emacs-redisplay-core-smoke--point 1
  "Dynamic point value used by the smoke window fixture.")

(defmacro emacs-redisplay-core-smoke--with-window (text-var draws-var &rest body)
  "Run BODY with one fake 10x3 window backed by TEXT-VAR.
Every backend draw call is appended to DRAWS-VAR as (ROW COL TEXT)."
  (declare (indent 2) (debug (symbolp symbolp body)))
  `(let ((emacs-redisplay-core-smoke--cursor nil))
     (cl-letf (((symbol-function 'emacs-window-selected-window)
              (lambda () 'core-smoke-window))
             ((symbol-function 'emacs-window-window-buffer)
              (lambda (_) 'core-smoke-buffer))
             ((symbol-function 'emacs-window-window-width)
              (lambda (_) 10))
             ((symbol-function 'emacs-window-window-height)
              (lambda (_) 3))
             ((symbol-function 'emacs-window-window-start)
              (lambda (_) 1))
             ((symbol-function 'emacs-window-window-point)
              (lambda (_) (if (boundp 'emacs-redisplay-core-smoke--point)
                              emacs-redisplay-core-smoke--point
                            1)))
             ((symbol-function 'emacs-window-window-edges)
              (lambda (_) '(0 0 10 3)))
             ((symbol-function 'emacs-window-p)
              (lambda (w) (eq w 'core-smoke-window)))
             ((symbol-function 'emacs-redisplay-core--buffer-string)
              (lambda (_) ,text-var))
             ((symbol-function 'emacs-redisplay-core--buffer-name)
              (lambda (_) "buf"))
             ((symbol-function 'emacs-redisplay-core--buffer-size)
              (lambda (_) (length ,text-var)))
             ((symbol-function 'emacs-redisplay-core--buffer-text-tick)
              (lambda (_) nil))
             ((symbol-function 'emacs-tui-backend-canvas-draw-text)
              (lambda (_backend _frame row col text &optional _face)
                (setq ,draws-var
                      (append ,draws-var
                              (list (list row col text))))
                (length text)))
             ((symbol-function 'emacs-tui-backend-canvas-flush)
              (lambda (&rest _) t))
             ((symbol-function 'emacs-tui-backend-cursor-show)
              (lambda (_backend _frame row col)
                (setq ,draws-var
                      (append ,draws-var
                              (list (list 'cursor row col))))
                t))
             ((symbol-function 'emacs-tui-backend-cursor-show-if-changed)
              (lambda (_backend _frame row col)
                (unless (equal emacs-redisplay-core-smoke--cursor
                               (cons row col))
                  (setq emacs-redisplay-core-smoke--cursor (cons row col))
                  (setq ,draws-var
                        (append ,draws-var
                                (list (list 'cursor row col)))))
                emacs-redisplay-core-smoke--cursor))
             ((symbol-function 'emacs-tui-backend--cup)
              (lambda (row col) (format "<%d,%d>" row col)))
             ((symbol-function 'emacs-tui-backend--emit)
              (lambda (string)
                (setq ,draws-var
                      (append ,draws-var
                              (list (list 'emit string))))
                t)))
     ,@body))
     )

(ert-deftest emacs-redisplay-core-smoke/unchanged-rows-do-not-flush ()
  "Repeated redisplay of the same rows should emit no backend writes."
  (let ((text "abc")
        (draws nil))
    (emacs-redisplay-core-smoke--with-window text draws
      (let ((h (emacs-redisplay-init (list :backend 'backend))))
        (emacs-redisplay-redisplay-window h 'core-smoke-window)
        (should (= 2 (emacs-redisplay-flush-frame h 'frame)))
        (should (equal draws
                       '((0 0 "abc       ")
                         (2 0 " buf      "))))
        (setq draws nil)
        (emacs-redisplay-redisplay-window h 'core-smoke-window)
        (should (= 0 (emacs-redisplay-flush-frame h 'frame)))
        (should-not draws)))))

(ert-deftest emacs-redisplay-core-smoke/shorter-row-clears-tail ()
  "When a row shrinks, flush emits padded spaces to clear stale cells."
  (let ((text "abc")
        (draws nil))
    (emacs-redisplay-core-smoke--with-window text draws
      (let ((h (emacs-redisplay-init (list :backend 'backend))))
        (emacs-redisplay-redisplay-window h 'core-smoke-window)
        (emacs-redisplay-flush-frame h 'frame)
        (setq text "a"
              draws nil)
        (emacs-redisplay-redisplay-window h 'core-smoke-window)
        (should (= 1 (emacs-redisplay-flush-frame h 'frame)))
        (should (equal draws '((0 0 "a         "))))))))

(ert-deftest emacs-redisplay-core-smoke/fast-repaint-paints-body-and-mode-line ()
  "The event-loop fast path paints dirty visible text and mode line."
  (let ((text "hello")
        (draws nil))
    (emacs-redisplay-core-smoke--with-window text draws
      (let ((h (emacs-redisplay-init (list :backend 'backend))))
        (should (emacs-redisplay-core-repaint h 'frame))
        (should (equal draws
                       '((0 0 "hello     ")
                         (2 0 " buf      ")
                         (cursor 0 0))))))))

(ert-deftest emacs-redisplay-core-smoke/fast-repaint-paints-visible-lines-and-cursor ()
  "Fast repaint should include multiple dirty body rows and cursor."
  (let ((text "one\ntwo")
        (draws nil)
        (emacs-redisplay-core-smoke--point 6))
    (emacs-redisplay-core-smoke--with-window text draws
      (let ((h (emacs-redisplay-init (list :backend 'backend))))
        (should (emacs-redisplay-core-repaint h 'frame))
        (should (equal draws
                       '((0 0 "one       ")
                         (1 0 "two       ")
                         (2 0 " buf      ")
                         (cursor 1 1))))))))

(ert-deftest emacs-redisplay-core-smoke/fast-repaint-skips-unchanged-rows ()
  "Repeated fast repaint should not refresh rows or stable cursor."
  (let ((text "stable")
        (draws nil))
    (emacs-redisplay-core-smoke--with-window text draws
      (let ((h (emacs-redisplay-init (list :backend 'backend))))
        (should (emacs-redisplay-core-repaint h 'frame))
        (setq draws nil)
        (should (emacs-redisplay-core-repaint h 'frame))
        (should-not draws)))))

(ert-deftest emacs-redisplay-core-smoke/current-line-repaint-direct-emits-one-row ()
  "Current-line repaint should avoid a whole-window matrix flush."
  (let ((text "a")
        (draws nil)
        (emacs-redisplay-core-smoke--point 2))
    (emacs-redisplay-core-smoke--with-window text draws
      (let ((h (emacs-redisplay-init (list :backend 'backend))))
        (should (emacs-redisplay-core-repaint-current-line h 'frame))
        (should (equal draws
                       '((emit "<0,0>a         <0,1>"))))
        (setq draws nil)
        (should (emacs-redisplay-core-repaint-current-line h 'frame))
        (should-not draws)))))

(ert-deftest emacs-redisplay-core-smoke/current-line-insert-hint-avoids-buffer-string ()
  "Insert hints should update the cached row without reading buffer text."
  (let ((text "")
        (draws nil))
    (emacs-redisplay-core-smoke--with-window text draws
      (let ((h (emacs-redisplay-init (list :backend 'backend))))
        (should (emacs-redisplay-core-initial-paint h 'frame))
        (setq draws nil)
        (cl-letf (((symbol-function 'emacs-redisplay-core--buffer-string)
                   (lambda (&rest _)
                     (error "buffer-string should not run"))))
          (should (emacs-redisplay-core-repaint-current-line
                   h 'frame (list :kind 'insert-char :char ?a))))
        (should (equal draws
                       '((emit "<0,0>a         <0,1>"))))))))

(ert-deftest emacs-redisplay-core-smoke/current-line-vector-insert-hint ()
  "Reusable vector insert hints should work like plist hints."
  (let ((text "")
        (draws nil))
    (emacs-redisplay-core-smoke--with-window text draws
      (let ((h (emacs-redisplay-init (list :backend 'backend))))
        (should (emacs-redisplay-core-initial-paint h 'frame))
        (setq draws nil)
        (should (emacs-redisplay-core-repaint-current-line
                 h 'frame (vector 'insert-char ?b 1 2)))
        (should (equal draws
                       '((emit "<0,0>b         <0,1>"))))))))

(ert-deftest emacs-redisplay-core-smoke/current-line-vector-insert-text-hint ()
  "Burst insert text hints should avoid reading buffer text."
  (let ((text "")
        (draws nil))
    (emacs-redisplay-core-smoke--with-window text draws
      (let ((h (emacs-redisplay-init (list :backend 'backend))))
        (should (emacs-redisplay-core-initial-paint h 'frame))
        (setq draws nil)
        (cl-letf (((symbol-function 'emacs-redisplay-core--buffer-string)
                   (lambda (&rest _)
                     (error "buffer-string should not run"))))
          (should (emacs-redisplay-core-repaint-current-line
                   h 'frame (vector 'insert-text "abc" 1 4))))
        (should (equal draws
                       '((emit "<0,0>abc       <0,3>"))))))))

(ert-deftest emacs-redisplay-core-smoke/initial-paint-seeds-empty-cache ()
  "Empty startup paint should let the next repaint avoid buffer text copy."
  (let ((text "")
        (draws nil))
    (emacs-redisplay-core-smoke--with-window text draws
      (let ((h (emacs-redisplay-init (list :backend 'backend))))
        (should (emacs-redisplay-core-initial-paint h 'frame))
        (should (equal draws '((emit "<2,0> buf "))))
        (setq draws nil)
        (should (emacs-redisplay-core-repaint h 'frame))
        (should (equal draws '((cursor 0 0))))))))

(provide 'emacs-redisplay-core-smoke)

;;; emacs-redisplay-core-smoke.el ends here
