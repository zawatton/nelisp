;;; nelisp-marker-parity-test.el --- marker slice 1/2 parity  -*- lexical-binding: t; -*-

;; Runs the SAME forms on host Emacs and on the standalone `nelisp'
;; binary (see the companion .sh) and diffs the printed results.  Every
;; buffer used below has an explicit, file-unique name (never the
;; anonymous `with-temp-buffer'/leading-space kind) so no block's
;; output depends on how many temp buffers an earlier block created --
;; a real risk here since `#<marker at N in BUF>' embeds the buffer
;; name literally.

(let (results)

  ;; markerp: non-markers, a real marker, a non-marker record (buffer).
  (push (list 'markerp-basic
              (let ((b (generate-new-buffer "mk-not-a-marker")))
                (prog1 (list (markerp 5) (markerp "x") (markerp nil)
                             (markerp (make-marker)) (markerp b))
                  (kill-buffer b))))
        results)

  ;; make-marker: unattached from the start.
  (push (list 'make-marker
              (let ((m (make-marker)))
                (list (markerp m) (marker-position m) (marker-buffer m))))
        results)

  ;; insertion-type: default nil; set-marker-insertion-type returns its
  ;; raw TYPE argument (not a normalized boolean); marker-insertion-type
  ;; itself always normalizes.
  (push (list 'insertion-type-roundtrip
              (let ((m (make-marker)))
                (list (marker-insertion-type m)
                      (set-marker-insertion-type m t)
                      (marker-insertion-type m)
                      (set-marker-insertion-type m nil)
                      (marker-insertion-type m))))
        results)

  ;; insert: a marker AT the insertion point advances only if its own
  ;; insertion-type is t; one strictly past it always advances.
  (push (list 'insert-and-type
              (let* ((buf (generate-new-buffer "mk-insert"))
                     (stay nil) (advance nil) (after nil))
                (with-current-buffer buf
                  (insert "abcdef")
                  (setq stay (copy-marker 3 nil))
                  (setq advance (copy-marker 3 t))
                  (setq after (copy-marker 5 nil))
                  (goto-char 3)
                  (insert "XY"))
                (prog1 (list (marker-position stay) (marker-position advance)
                             (marker-position after))
                  (kill-buffer buf))))
        results)

  ;; insert-before-markers: every marker at the insertion point advances,
  ;; regardless of its own insertion-type.
  (push (list 'insert-before-markers
              (let* ((buf (generate-new-buffer "mk-ibm"))
                     (m nil))
                (with-current-buffer buf
                  (insert "abcdef")
                  (setq m (copy-marker 3 nil))
                  (goto-char 3)
                  (insert-before-markers "Z"))
                (prog1 (marker-position m)
                  (kill-buffer buf))))
        results)

  ;; delete-region: before start unaffected; inside the deleted range
  ;; collapses to start; past the end shifts back by the deleted length.
  (push (list 'delete-region
              (let* ((buf (generate-new-buffer "mk-delete"))
                     (a nil) (b nil) (c nil))
                (with-current-buffer buf
                  (insert "abcdefghij")
                  (setq a (copy-marker 2 nil))
                  (setq b (copy-marker 5 nil))
                  (setq c (copy-marker 8 nil))
                  (delete-region 4 7))
                (prog1 (list (marker-position a) (marker-position b)
                             (marker-position c))
                  (kill-buffer buf))))
        results)

  ;; erase-buffer: every marker collapses to point-min, regardless of type.
  (push (list 'erase-buffer
              (let* ((buf (generate-new-buffer "mk-erase"))
                     (a nil) (b nil))
                (with-current-buffer buf
                  (insert "abcdefghij")
                  (setq a (copy-marker 3 nil))
                  (setq b (copy-marker 7 t))
                  (erase-buffer))
                (prog1 (list (marker-position a) (marker-position b))
                  (kill-buffer buf))))
        results)

  ;; kill-buffer: markers into a killed buffer detach (buffer and
  ;; position both nil), never a stale pointer into the dead buffer.
  (push (list 'kill-buffer-detach
              (let* ((buf (generate-new-buffer "mk-kill"))
                     (m nil))
                (with-current-buffer buf
                  (insert "abcdef")
                  (setq m (copy-marker 3 nil)))
                (kill-buffer buf)
                (list (markerp m) (marker-buffer m) (marker-position m))))
        results)

  ;; save-excursion: the saved point is a marker, so an edit BODY makes
  ;; before it carries the restored point along (Emacs 31.1 answers 9,
  ;; not the original numeric offset 6).
  (push (list 'save-excursion
              (with-temp-buffer
                (insert "xxxxxx")
                (goto-char 6)
                (save-excursion (goto-char 1) (insert "XXX"))
                (point)))
        results)

  ;; replace-match: markers relocate through the underlying
  ;; delete-region + insert exactly as a manual pair would.
  (push (list 'replace-match
              (let* ((buf (generate-new-buffer "mk-replace"))
                     (before nil) (at-start nil) (at-end nil) (after nil))
                (with-current-buffer buf
                  (insert "foo bar foo")
                  (goto-char 1)
                  (search-forward "bar")
                  (setq before (copy-marker 3 nil))
                  (setq at-start (copy-marker (match-beginning 0) nil))
                  (setq at-end (copy-marker (match-end 0) t))
                  (setq after (copy-marker (1+ (match-end 0)) nil))
                  (replace-match "BAZZZ" t t))
                (prog1 (list (marker-position before) (marker-position at-start)
                             (marker-position at-end) (marker-position after)
                             (with-current-buffer buf (buffer-string)))
                  (kill-buffer buf))))
        results)

  ;; insert-file-contents with REPLACE: implemented as erase-buffer then
  ;; a plain insert, so a type-nil marker sitting at point-min stays
  ;; there and a type-t one advances past the replacement text.
  (push (list 'insert-file-contents-replace
              (let* ((file (make-temp-file "nelisp-marker-parity-"))
                     (buf (generate-new-buffer "mk-ifc-replace"))
                     (stay nil) (advance nil))
                (write-region "REPLACED" nil file)
                (with-current-buffer buf
                  (insert "old text")
                  (setq stay (copy-marker 1 nil))
                  (setq advance (copy-marker 1 t))
                  (insert-file-contents file nil nil nil t))
                (prog1 (list (marker-position stay) (marker-position advance)
                             (with-current-buffer buf (buffer-string)))
                  (kill-buffer buf)
                  (delete-file file))))
        results)

  ;; set-marker: clamps beyond either bound; accepts a marker as
  ;; POSITION; accepts an explicit BUFFER different from current.
  (push (list 'set-marker-clamp-and-forms
              (let* ((buf-a (generate-new-buffer "mk-set-a"))
                     (buf-b (generate-new-buffer "mk-set-b"))
                     (m (make-marker))
                     (src nil) (high nil) (low nil) (via-buffer nil))
                (with-current-buffer buf-a (insert "abcde"))  ; 1..6
                (with-current-buffer buf-b (insert "xyz"))    ; 1..4
                (setq high (progn (set-marker m 999 buf-a) (marker-position m)))
                (setq low (progn (set-marker m -5 buf-a) (marker-position m)))
                (setq src (copy-marker 3 nil))
                (with-current-buffer buf-a (set-marker src 2))
                ;; POSITION is itself a marker, with an explicit BUFFER.
                (set-marker m src buf-a)
                (setq via-buffer (list (marker-position m) (eq (marker-buffer m) buf-a)))
                ;; BUFFER argument selects a different buffer than current.
                (set-marker m 2 buf-b)
                (prog1 (list high low via-buffer
                             (marker-position m) (eq (marker-buffer m) buf-b))
                  (kill-buffer buf-a)
                  (kill-buffer buf-b))))
        results)

  ;; copy-marker: unspecified -> nowhere; an existing nowhere marker
  ;; copies as nowhere too, regardless of which buffer is current; TYPE.
  (push (list 'copy-marker-forms
              (let* ((buf (generate-new-buffer "mk-copy"))
                     (nowhere (make-marker))
                     (unspecified nil) (from-nowhere nil) (typed nil))
                (with-current-buffer buf
                  (insert "abcdef")
                  (setq unspecified (copy-marker))
                  (setq from-nowhere (copy-marker nowhere))
                  (setq typed (copy-marker 4 t)))
                (prog1
                    (list (marker-buffer unspecified) (marker-position unspecified)
                          (marker-buffer from-nowhere) (marker-position from-nowhere)
                          (marker-insertion-type typed) (marker-position typed)
                          (eq (marker-buffer typed) buf))
                  (kill-buffer buf)))
        )
        results)

  ;; equality: `eq' is identity; `equal' compares buffer+position only
  ;; (insertion-type is NOT part of it), and two nowhere-pointing
  ;; markers are `equal' unconditionally.
  (push (list 'equality
              (let* ((buf (generate-new-buffer "mk-eq"))
                     (m1 nil) (m2 nil) (m3 nil) (u1 (make-marker)) (u2 (make-marker)))
                (with-current-buffer buf
                  (insert "abcdef")
                  (setq m1 (copy-marker 3 nil))
                  (setq m2 (copy-marker 3 t))
                  (setq m3 (copy-marker 4 nil)))
                (prog1
                    (list (eq m1 m1) (eq m1 m2) (equal m1 m2) (equal m1 m3)
                          (equal u1 u2) (eq u1 u2))
                  (kill-buffer buf))))
        results)

  ;; print format: `#<marker at N in BUF>' / `#<marker in no buffer>',
  ;; per GNU's print.c.  Buffer name is explicit and file-unique, so
  ;; this does not depend on any other block's temp-buffer count.
  (push (list 'print-format
              (let* ((buf (generate-new-buffer "mk-print"))
                     (m nil) (u (make-marker)))
                (with-current-buffer buf
                  (insert "hello")
                  (setq m (copy-marker 2 nil)))
                (prog1 (list (prin1-to-string m) (prin1-to-string u))
                  (kill-buffer buf))))
        results)

  (princ (format "%S\n" (nreverse results))))

;;; nelisp-marker-parity-test.el ends here
