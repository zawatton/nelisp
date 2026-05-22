;;; probe-first-n.el --- Wave 7 first-N manifest compile probe  -*- lexical-binding: t -*-
;;
;; Usage:
;;   NELISP_PROBE_REPO=/abs/path NELISP_PROBE_LIMIT=10 \
;;     ./target/release/nelisp --batch -l test/wave-7-probes/probe-first-n.el
;;
;; NELISP_PROBE_LIMIT (= entry 数) default 10、full 208 は probe-208.el。
;;
;; 出力 = 各 entry の OK/FAIL、最後に `subtotal X/Y` summary。
;; 進捗 metric = X 値で前後比較、回帰検出に。

(let* ((root (or (getenv "NELISP_PROBE_REPO")
                 "/home/madblack-21/Cowork/Notes/dev/nelisp"))
       (lisp (concat root "/lisp"))
       (script (concat root "/scripts/compile-elisp-objects.el"))
       (limit (string-to-number (or (getenv "NELISP_PROBE_LIMIT") "10")))
       (out-dir (or (getenv "NELISP_ELISP_OBJECTS_DIR")
                    "/tmp/elisp-probe-first-n")))
  (setq load-path (cons lisp load-path))
  (setenv "NELISP_ELISP_OBJECTS_DIR" out-dir)
  (setenv "NELISP_PHASE47_TARGET_ARCH"
          (or (getenv "NELISP_PHASE47_TARGET_ARCH") "x86_64"))
  (setenv "NELISP_PHASE47_TARGET_OS"
          (or (getenv "NELISP_PHASE47_TARGET_OS") "linux"))
  (load script nil nil t)
  (let* ((entries compile-elisp-objects-manifest)
         (i 0) (ok 0))
    (catch 'done
      (while entries
        (setq i (1+ i))
        (when (> i limit) (throw 'done nil))
        (let* ((entry (car entries)) (feature (car entry))
               (props (cdr entry)) (src-var (plist-get props :source-var))
               (output (plist-get props :output))
               (out-path (expand-file-name output out-dir)))
          (princ (format "[%d] %S ... " i feature))
          (condition-case e
              (progn (require feature)
                     (nelisp-phase47-compile-to-object
                      (symbol-value src-var) out-path :format 'elf :arch 'x86_64)
                     (setq ok (1+ ok))
                     (princ "OK\n"))
            (t (princ (format "FAIL %S\n" e)))))
        (setq entries (cdr entries))))
    (princ (format "subtotal %d/%d\n" ok i))))
