;;; probe-208.el --- Wave 7 full 208-manifest compile probe  -*- lexical-binding: t -*-
;;
;; Usage:
;;   NELISP_PROBE_REPO=/abs/path \
;;     timeout 600 ./target/release/nelisp --batch -l test/wave-7-probes/probe-208.el
;;
;; 進捗 print = 20 entry 毎 + 最後に `PASS RATE: X/Y failed=N` summary。
;; 失敗 first 5 distinct error も出す。
;;
;; **timeout 600s 推奨**、hang したら kill して partial 確認。
;; Wave 7 着地条件 = X ≥ 50/208 + require chain pass。
;; 反復実験用には probe-first-n.el 推奨 (= 10 entries で短時間)。

(let* ((root (or (getenv "NELISP_PROBE_REPO")
                 "/home/madblack-21/Cowork/Notes/dev/nelisp"))
       (lisp (concat root "/lisp"))
       (script (concat root "/scripts/compile-elisp-objects.el"))
       (out-dir (or (getenv "NELISP_ELISP_OBJECTS_DIR")
                    "/tmp/elisp-probe-208")))
  (setq load-path (cons lisp load-path))
  (setenv "NELISP_ELISP_OBJECTS_DIR" out-dir)
  (setenv "NELISP_PHASE47_TARGET_ARCH"
          (or (getenv "NELISP_PHASE47_TARGET_ARCH") "x86_64"))
  (setenv "NELISP_PHASE47_TARGET_OS"
          (or (getenv "NELISP_PHASE47_TARGET_OS") "linux"))
  (load script nil nil t)
  (let* ((entries compile-elisp-objects-manifest)
         (arch 'x86_64) (format 'elf)
         (i 0) (ok 0) (failed nil))
    (while entries
      (setq i (1+ i))
      (when (= 0 (mod i 20))
        (princ (format "  [%d] ok=%d failed=%d\n" i ok (length failed))))
      (let* ((entry (car entries)) (feature (car entry))
             (props (cdr entry)) (src-var (plist-get props :source-var))
             (output (plist-get props :output))
             (requires-arch (plist-get props :requires-arch))
             (out-path (expand-file-name output out-dir)))
        (condition-case e
            (cond
             ((and requires-arch (symbolp requires-arch)
                   (not (eq requires-arch arch)))
              nil)
             (t
              (require feature)
              (nelisp-phase47-compile-to-object
               (symbol-value src-var) out-path :format format :arch arch)
              (setq ok (1+ ok))))
          (t (push (cons (format "%S" feature)
                         (substring (format "%S" e) 0
                                    (min 100 (length (format "%S" e)))))
                   failed))))
      (setq entries (cdr entries)))
    (princ (format "PASS RATE: %d/%d failed=%d\n" ok i (length failed)))
    (when failed
      (let ((seen nil) (cur (reverse failed)) (n 0))
        (while (and cur (< n 5))
          (let ((sig (cdr (car cur))))
            (unless (member sig seen) (push sig seen)
                    (princ (format "  ex %S => %s\n" (car (car cur)) sig))
                    (setq n (1+ n))))
          (setq cur (cdr cur)))))))
