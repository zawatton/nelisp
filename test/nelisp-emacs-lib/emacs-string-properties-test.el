;;; emacs-string-properties-test.el --- string property substrate checks -*- lexical-binding: t; -*-

(require 'ert)

(defconst emacs-string-properties-test--root
  (expand-file-name "../.."
                    (file-name-directory (or load-file-name buffer-file-name))))

(dolist (dir '("packages/nelisp-emacs-foundation/src"
               "packages/nelisp-emacs-buffer-core/src"
               "packages/nelisp-emacs-text-core/src"))
  (add-to-list 'load-path (expand-file-name dir emacs-string-properties-test--root)))

(load "nelisp-emacs-compat" nil t)
(load "emacs-buffer" nil t)
(load "emacs-buffer-builtins" nil t)
(load "emacs-string" nil t)

(ert-deftest emacs-string-properties-test/string-ranges-copy-and-mutate ()
  "String sidecars use zero-based ranges and copy independently."
  (let* ((source (copy-sequence "abcd"))
         (copy (copy-sequence source)))
    (emacs-buffer-string-text-property 'put source 1 3 'help-echo "source")
    (emacs-buffer-string-text-property 'copy source copy)
    (emacs-buffer-string-text-property 'add copy 0 4 '(face bold))
    (should-not (emacs-buffer-string-text-property 'get source 0 'help-echo))
    (should (equal "source"
                   (emacs-buffer-string-text-property 'get source 1 'help-echo)))
    (should (eq 'bold (emacs-buffer-string-text-property 'get copy 0 'face)))
    (should (eq 'bold (emacs-buffer-string-text-property 'get copy 3 'face)))
    (should (equal "source"
                   (emacs-buffer-string-text-property 'get copy 1 'help-echo)))
    (emacs-buffer-string-text-property 'set copy 2 3 '(face italic))
    (should (eq 'italic (emacs-buffer-string-text-property 'get copy 2 'face)))
    (should (eq 'bold (emacs-buffer-string-text-property 'get copy 1 'face)))
    (emacs-buffer-string-text-property 'remove copy 1 2 '(help-echo))
    (should-not (emacs-buffer-string-text-property 'get copy 1 'help-echo))
    (should (equal "source"
                   (emacs-buffer-string-text-property 'get source 1 'help-echo)))))

(ert-deftest emacs-string-properties-test/end-boundary-is-readable ()
  "String property reads at length and at empty-string position zero are nil."
  (should-not (get-text-property 4 'face "abcd"))
  (should-not (text-properties-at 4 "abcd"))
  (should-not (get-text-property 0 'face ""))
  (should-not (text-properties-at 0 "")))

(ert-deftest emacs-string-properties-test/insert-copies-runs-at-offsets ()
  "Insertion transfers string ranges and preserves shifted buffer ranges."
  (let* ((source (copy-sequence "abcd"))
         (copy (copy-sequence source))
         (buffer (nelisp-ec-generate-new-buffer "*string-properties-test*"))
         (old-buffer (nelisp-ec-current-buffer)))
    (emacs-buffer-string-text-property 'put source 1 3 'face 'bold)
    (emacs-buffer-string-text-property 'copy source copy)
    (unwind-protect
        (progn
          (nelisp-ec-set-buffer buffer)
          (nelisp-ec-insert "tail")
          (emacs-buffer-put-text-property 2 4 'rear-nonsticky t buffer)
          (nelisp-ec--set-buffer-point buffer 1)
          (nelisp-ec-insert "A" copy "B")
          (should (equal "AabcdBtail" (nelisp-ec-buffer-string)))
          (should-not (emacs-buffer-get-text-property 2 'face buffer))
          (should (eq 'bold (emacs-buffer-get-text-property 3 'face buffer)))
          (should (eq 'bold (emacs-buffer-get-text-property 4 'face buffer)))
          (should-not (emacs-buffer-get-text-property 5 'face buffer))
          (should-not (emacs-buffer-get-text-property 7 'rear-nonsticky buffer))
          (should (eq t (emacs-buffer-get-text-property 8 'rear-nonsticky buffer)))
          (should (eq t (emacs-buffer-get-text-property 9 'rear-nonsticky buffer)))
          (should-not (emacs-buffer-get-text-property 10 'rear-nonsticky buffer)))
      (when old-buffer (nelisp-ec-set-buffer old-buffer)))))

(ert-deftest emacs-string-properties-test/public-insert-preserves-sidecar-runs ()
  "The public native INSERT path exposes ranges through the property API."
  (let* ((source (copy-sequence "xy"))
         (styled (propertize source 'face 'bold)))
    (with-temp-buffer
      (insert "L" styled "R")
      (should (equal "LxyR" (buffer-string)))
      (should-not (get-text-property 1 'face))
      (should (eq 'bold (get-text-property 2 'face)))
      (should (eq 'bold (get-text-property 3 'face)))
      (should-not (get-text-property 4 'face))
      (should-not (emacs-buffer-string-text-property 'get source 0 'face)))))

(ert-deftest emacs-string-properties-test/buffer-ranges-copy-to-strings ()
  "Buffer sidecar ranges become zero-based string runs without changing text."
  (with-temp-buffer
    (insert "abcd")
    (let* ((buffer (current-buffer))
           (ext (emacs-buffer--ensure-text-property-ext buffer))
           (text (buffer-substring-no-properties 1 5)))
      (setf (emacs-buffer--ext-text-props ext)
            (emacs-buffer--tp-add nil 2 4 '(face bold)))
      (emacs-buffer--copy-buffer-properties-to-string buffer 1 5 text)
      (should (equal "abcd" text))
      (should-not (emacs-buffer-string-text-property 'get text 0 'face))
      (should (eq 'bold (emacs-buffer-string-text-property 'get text 1 'face)))
      (should (eq 'bold (emacs-buffer-string-text-property 'get text 2 'face)))
      (should-not (emacs-buffer-string-text-property 'get text 3 'face)))))

(ert-deftest emacs-string-properties-test/gnu-propertize-binding-is-preserved ()
  "Loading the fallback source does not replace an existing GNU propertize."
  (let ((before (symbol-function 'propertize)))
    (load (expand-file-name "packages/nelisp-emacs-foundation/src/emacs-string.el"
                            emacs-string-properties-test--root)
          nil t)
    (should (eq before (symbol-function 'propertize)))))

(provide 'emacs-string-properties-test)
;;; emacs-string-properties-test.el ends here

(ert-deftest emacs-string-properties-test/copy-runs-one-step-matches-per-run-adds ()
  "The one-step install on a fresh string equals adding run by run."
  (let ((runs (list (list 1 3 (list 'face 'bold))
                    (list 3 4 (list 'mouse-face 'highlight 'help-echo "x"))
                    (list 6 9 (list 'face 'italic))
                    (list 12 20 (list 'invisible t))))
        (reference (lambda (string runs start end)
                     (dolist (run runs)
                       (let ((lo (max start (nth 0 run))) (hi (min end (nth 1 run))))
                         (when (< lo hi)
                           (emacs-buffer-string-text-property
                            'add string (- lo start) (- hi start) (nth 2 run)))))
                     (emacs-buffer-string-text-property 'runs string))))
    ;; Sorted runs on a fresh string: one-step path, same cells and plists.
    (let ((fast (make-string 12 ?a)) (slow (make-string 12 ?a)))
      (emacs-buffer--copy-runs-to-string fast runs 2 14)
      (should (equal (emacs-buffer-string-text-property 'runs fast)
                     (funcall reference slow runs 2 14)))
      (should (eq (emacs-buffer--tp-plist
                   (car (emacs-buffer--ext-text-props
                         (gethash fast emacs-buffer--string-state))))
                  (nth 2 (nth 0 runs)))))
    ;; Unsorted runs fall back to per-run adds.
    (let ((fast (make-string 12 ?a)) (slow (make-string 12 ?a))
          (unsorted (reverse runs)))
      (emacs-buffer--copy-runs-to-string fast unsorted 2 14)
      (should (equal (emacs-buffer-string-text-property 'runs fast)
                     (funcall reference slow unsorted 2 14))))
    ;; A string that already has properties keeps the merging path.
    (let ((fast (make-string 12 ?a)) (slow (make-string 12 ?a)))
      (dolist (s (list fast slow))
        (emacs-buffer-string-text-property 'add s 0 5 (list 'face 'shadow)))
      (emacs-buffer--copy-runs-to-string fast runs 2 14)
      (should (equal (emacs-buffer-string-text-property 'runs fast)
                     (funcall reference slow runs 2 14))))))

(ert-deftest emacs-string-properties-test/tp-add-ordered-matches-general ()
  "The linear interval add equals the general sorting add on valid lists."
  (random "emacs-string-properties-test")
  (dotimes (_ 500)
    (let ((intervals nil) (pos 0))
      ;; Sorted, disjoint, non-empty cells with random gaps and plists.
      (dotimes (_ (random 12))
        (setq pos (+ pos (random 4)))
        (let ((end (+ pos 1 (random 5))))
          (push (emacs-buffer--tp-cell pos end (list 'face (random 3) 'help-echo "h"))
                intervals)
          (setq pos end)))
      (setq intervals (nreverse intervals))
      (let* ((start (random (+ pos 3)))
             (end (+ start 1 (random 8)))
             (plist (if (zerop (random 2)) (list 'mouse-face (random 2))
                       (list 'mouse-face (random 2) 'face 'bold))))
        (should (equal (emacs-buffer--tp-add-ordered
                        (copy-tree intervals) start end plist)
                       (emacs-buffer--tp-add-general
                        (copy-tree intervals) start end plist))))))
  ;; Unordered input is refused so the general version handles it.
  (should-not (emacs-buffer--tp-add-ordered
               (list (emacs-buffer--tp-cell 5 8 '(a 1))
                     (emacs-buffer--tp-cell 0 3 '(b 2)))
               1 6 '(c 3))))
