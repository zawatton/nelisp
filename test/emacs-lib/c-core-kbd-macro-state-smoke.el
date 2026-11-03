;;; c-core-kbd-macro-state-smoke.el --- Macro replay/cache regression -*- lexical-binding: t; -*-

;; Run unchanged with GNU Emacs -Q --batch -l, or with the standalone
;; --cold-load-from current C-core bundle image --load this file.
;; The canonical other-area probe rows deliberately remain unchanged.

(defun c-core-kbd-macro-state--check (name macro count expected &optional policy)
  "Check replayed text, point and cache lifetime for NAME under POLICY."
  (let ((buffer (generate-new-buffer " *kbd-macro-state*"))
        (old-window-buffer (window-buffer (selected-window)))
        (old-current-buffer (current-buffer)))
    (unwind-protect
        (progn
          (set-window-buffer (selected-window) buffer)
          (set-buffer buffer)
          (use-local-map
           (let ((map (make-sparse-keymap)))
             (define-key map [f1] 'ignore)
             (define-key map "a" 'self-insert-command)
             (define-key map "b" 'self-insert-command)
             map))
          (when (eq policy 'unibyte) (set-buffer-multibyte nil))
          (let ((cache-long-scans (not (eq policy 'cache-off)))
                (auto-composition-mode (not (eq policy 'composition-off)))
                (global-disable-point-adjustment (eq policy 'adjustment-off))
                ;; Isolate command finalization from optional editor hooks.
                (pre-command-hook nil) (post-command-hook nil)
                (post-self-insert-hook nil))
            (unless (null (newline-cache-check))
              (error "%s: fresh buffer already has a cache" name))
            (execute-kbd-macro macro count)
            (let ((observed (list (buffer-string) (point) (newline-cache-check))))
              (unless (equal observed expected)
                (error "%s: expected %S, observed %S" name expected observed))
              (princ (format "KBD-STATE|%s|%S\n" name observed))))
          ;; Match the area probes' cleanup: cache creation survives erase
          ;; and restoring text, but does not leak into another buffer.
          (when (equal expected '("aa" 3 [[] []]))
            (erase-buffer)
            (unless (equal (newline-cache-check) [[] []])
              (error "%s: cleanup lost the cache" name))
            (with-temp-buffer
              (unless (null (newline-cache-check))
                (error "%s: cache leaked into a temporary buffer" name)))))
      (set-window-buffer (selected-window) old-window-buffer)
      (set-buffer old-current-buffer)
      (kill-buffer buffer))))

(c-core-kbd-macro-state--check 'empty [] 2 '("" 1 nil))
(c-core-kbd-macro-state--check 'single "a" 1 '("a" 2 nil))
(c-core-kbd-macro-state--check 'repeated "a" 2 '("aa" 3 [[] []]))
(c-core-kbd-macro-state--check 'sequence "ab" 1 '("ab" 3 [[] []]))
(c-core-kbd-macro-state--check 'noop [f1 f1] 1 '("" 1 nil))
(c-core-kbd-macro-state--check 'cache-off "a" 2 '("aa" 3 nil) 'cache-off)
(c-core-kbd-macro-state--check 'composition-off "a" 2 '("aa" 3 nil) 'composition-off)
(c-core-kbd-macro-state--check 'adjustment-off "a" 2 '("aa" 3 nil) 'adjustment-off)
(c-core-kbd-macro-state--check 'unibyte "a" 2 '("aa" 3 nil) 'unibyte)
(princ "KBD-STATE-DONE|checked=9\n")
t
