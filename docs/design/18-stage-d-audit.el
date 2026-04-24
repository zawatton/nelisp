;;; 18-stage-d-audit.el --- Stage D dependency audit for anvil.el  -*- lexical-binding: t; -*-

;; Doc 18 Stage D Phase 6.0 audit script.
;;
;; 走査対象: Notes/dev/anvil.el 配下の *.el。
;; 検出対象: Emacs UI / display 依存の API 呼び出し。Phase 6.x で headless
;; profile 設計時にここで列挙された使用箇所を対応順序づけに使う。
;;
;; 使い方:
;;   emacs --batch -l docs/design/18-stage-d-audit.el \
;;         --eval '(stage-d-audit-run "/home/madblack-21/Cowork/Notes/dev/anvil.el")'
;;
;; 出力: `(:total N :buckets ((:api SYM :count K :files (...)) ...))' plist。
;; Phase 6.0 LOCK 後に =docs/design/18-stage-d-audit-report-YYYY-MM-DD.org= として
;; snapshot commit 推奨。

(require 'cl-lib)
(require 'subr-x)

(defconst stage-d-audit--ui-apis
  ;; Phase 6.0 初版 bucket 群。Phase 6.1 実装時に追加検出が出たら足す。
  '(;; Buffer / point / marker ops — NeLisp port or string-buffer 代替
    (buffer-ops
     with-current-buffer with-temp-buffer find-file find-file-noselect
     save-excursion save-restriction goto-char point point-min point-max
     buffer-string buffer-substring buffer-substring-no-properties
     insert insert-buffer-substring delete-region erase-buffer
     set-buffer get-buffer generate-new-buffer kill-buffer
     buffer-modified-p set-buffer-modified-p)
    ;; File / save-buffer — persistent state は Phase 5-F.1 state layer へ
    (file-save
     save-buffer write-file write-region save-some-buffers
     revert-buffer auto-save-mode)
    ;; TUI / transient / menu — headless では CLI flag / stdin JSON
    (ui-tui
     transient-define-prefix transient-define-suffix transient-setup
     tabulated-list-mode dashboard-mode
     read-string read-from-minibuffer y-or-n-p yes-or-no-p
     completing-read message)
    ;; Timer / async idle — NeLisp actor scheduler or OS timer 代替
    (timer-idle
     run-with-idle-timer run-with-timer run-at-time cancel-timer
     timer-list)
    ;; File-notify — inotify/kqueue direct or Phase 5-F.3+ で NeLisp port
    (file-notify
     file-notify-add-watch file-notify-rm-watch)
    ;; Process / sentinel — Phase 5-C primitive は存在、sentinel filter 抽象化要
    (process-io
     make-process set-process-sentinel set-process-filter
     process-send-string process-send-eof delete-process)
    ;; Org-mode — anvil-org-index 等で濃厚使用、Phase 5-F で org-parser 必要
    (org-mode
     org-mode org-element-parse-buffer org-map-entries
     org-entry-get org-entry-put org-read-date))
  "Bucket 名 → 検出対象 symbol list。各 bucket は headless port の作業単位。")

(defun stage-d-audit--scan-file (path)
  "Return alist of (SYM . count) occurrences in PATH."
  (let ((counts (make-hash-table :test 'eq)))
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (dolist (bucket stage-d-audit--ui-apis)
        (dolist (sym (cdr bucket))
          (let ((re (concat "(" (regexp-quote (symbol-name sym)) "\\_>")))
            (save-excursion
              (goto-char (point-min))
              (let ((n 0))
                (while (re-search-forward re nil t) (cl-incf n))
                (when (> n 0)
                  (puthash sym (+ n (gethash sym counts 0)) counts))))))))
    (let (result)
      (maphash (lambda (k v) (push (cons k v) result)) counts)
      (sort result (lambda (a b) (> (cdr a) (cdr b)))))))

(defun stage-d-audit-run (root)
  "Walk ROOT directory for *.el、aggregate API usage, return summary plist."
  (let ((files (directory-files-recursively root "\\.el\\'"))
        (by-api (make-hash-table :test 'eq))
        (files-by-api (make-hash-table :test 'eq))
        (total 0))
    (dolist (path files)
      ;; skip .elc byproducts and test fixtures that intentionally exercise UI
      (unless (or (string-suffix-p ".elc" path)
                  (string-match-p "/tests/fixtures/" path))
        (dolist (pair (stage-d-audit--scan-file path))
          (let ((sym (car pair))
                (count (cdr pair)))
            (puthash sym (+ count (gethash sym by-api 0)) by-api)
            (let ((existing (gethash sym files-by-api)))
              (unless (member path existing)
                (puthash sym (cons path existing) files-by-api)))
            (cl-incf total count)))))
    (let (buckets)
      (dolist (bucket stage-d-audit--ui-apis)
        (let ((bucket-name (car bucket))
              (bucket-count 0)
              (bucket-apis nil))
          (dolist (sym (cdr bucket))
            (let ((n (gethash sym by-api 0)))
              (when (> n 0)
                (cl-incf bucket-count n)
                (push (list :api sym
                            :count n
                            :files (sort (copy-sequence (gethash sym files-by-api))
                                         #'string<))
                      bucket-apis))))
          (push (list :bucket bucket-name
                      :count bucket-count
                      :apis (sort bucket-apis
                                  (lambda (a b) (> (plist-get a :count)
                                                   (plist-get b :count)))))
                buckets)))
      (let ((summary (list :root root
                           :files-scanned (length files)
                           :total total
                           :buckets (sort buckets
                                          (lambda (a b) (> (plist-get a :count)
                                                           (plist-get b :count)))))))
        (prin1 summary)
        (terpri)
        summary))))

(provide '18-stage-d-audit)
;;; 18-stage-d-audit.el ends here
