;;; s52c-gnu-comparison.el --- Package blocker differential probes -*- lexical-binding: t; -*-
;; Run unchanged on GNU Emacs and the GUI heap image; compare S52C rows.
(defun s52c-row (name thunk)
  (princ (format "S52C|%S|%S\n" name
                 (condition-case err (funcall thunk) (error (list 'error err))))))
(dolist (full '(nil t))
  (dolist (nodigits '(nil t))
    (dolist (setter '(define-key keymap-set))
      (s52c-row
       (list 'suppression full nodigits setter)
       (lambda ()
         (let ((map (if full (make-keymap) (make-sparse-keymap))))
           (define-key map [C-f5] 'modified-command)
           (let ((result (suppress-keymap map nodigits)))
             (if (eq setter 'define-key)
                 (define-key map "gd" 'prefix-command)
               (keymap-set map "g d" 'prefix-command))
             (list result (lookup-key map [remap self-insert-command])
                   (lookup-key map "7") (lookup-key map "-")
                   (lookup-key map "a") (lookup-key map "gd")
                   (lookup-key map [C-f5])))))))))
(s52c-row 'explicit-nonprefix
          (lambda () (let ((map (make-sparse-keymap)))
                       (define-key map "g" 'undefined)
                       (define-key map "gd" 'test-command))))
(s52c-row 'keymap-set-explicit-nonprefix
          (lambda () (let ((map (make-sparse-keymap)))
                       (keymap-set map "g" 'undefined)
                       (keymap-set map "g d" 'test-command))))
(s52c-row 'bootstrap-event-scanner
          (lambda () (let ((map (current-global-map)))
                       (map-keymap (lambda (event _binding)
                                     (lookup-key map (vector event))) map))))
(s52c-row 'vector-sequences
          (lambda () (let ((map (make-sparse-keymap)) events)
                       (define-key map [24 102] 'vector-command)
                       (define-key map [f5] 'function-command)
                       (map-keymap (lambda (event _binding) (push event events)) map)
                       (list (sort events (lambda (a b) (string< (format "%S" a) (format "%S" b))))
                             (where-is-internal 'vector-command map)
                             (lookup-key map [24 102])
                             (lookup-key map [f5])))))
(s52c-row 'invalid-nested-vector
          (lambda () (lookup-key (make-sparse-keymap) [[24]])))
;; Genuine GNU traversal builds event vectors from map-keymap callbacks.
(s52c-row 'substitute-scanner
          (lambda () (let ((map (make-sparse-keymap)))
                       (define-key map [24 102] 'old-command)
                       (substitute-key-definition 'old-command 'new-command map)
                       (lookup-key map [24 102]))))
(defun s52c-word-callback (_position limit)
  (error "Strict word movement invoked callback at %S" limit))
(dolist (function '(forward-word-strictly backward-word-strictly))
  (dolist (count '(nil 0 1 2 9 -1 -2 -9))
    (dolist (start '(1 6 15))
      (s52c-row
       (list function count start)
       (lambda ()
         (with-temp-buffer
           (insert "alpha beta end")
           (goto-char start)
           (let* ((table (make-char-table nil))
                  (find-word-boundary-function-table table))
             (set-char-table-range table t 's52c-word-callback)
             (let ((result (funcall function count)))
               (list (point) result (eq find-word-boundary-function-table table)
                     (commandp function))))))))))
(s52c-row 'word-callback-restored-on-error
          (lambda ()
            (with-temp-buffer
              (let* ((table (make-char-table nil))
                     (find-word-boundary-function-table table)
                     (error (condition-case err (forward-word-strictly 'bad)
                              (error (car err)))))
                (list error (eq table find-word-boundary-function-table))))))
(princ "S52C-DONE\n")
