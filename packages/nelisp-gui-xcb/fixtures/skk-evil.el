;;; skk-evil.el --- Real ddskk/evil fixture shared by GNU and NeLisp -*- lexical-binding: t; -*-
;; No personal dictionary is opened. All paths point into an isolated fixture.
(defvar nelisp-gui-fixture-captured-events nil)
(defvar-local nelisp-gui-fixture-capture-mode nil)
(defvar nelisp-gui-fixture-capture-alist nil)
(defun nelisp-gui-fixture-capture-event ()
  (interactive)
  (push last-command-event nelisp-gui-fixture-captured-events)
  (princ (format "GUI-CAPTURE|event=%S|\n" last-command-event)))

(defun nelisp-gui-skk-evil-fixture ()
  (let* ((fixture (getenv "NELISP_GUI_VENDOR_FIXTURE"))
         (out (getenv "NELISP_GUI_FIXTURE_OUT"))
         (buffer (generate-new-buffer "*SKK GUI*")))
    (unless (and fixture out) (error "Missing pinned SKK fixture paths"))
    (setq load-path (append (list (concat fixture "/gnu") (concat fixture "/ddskk-test")
                                 (concat fixture "/evil")) load-path))
    (setq skk-user-directory (concat out "/skk/")
          skk-init-file (concat out "/skk/empty-init.el")
          skk-jisyo (cons (concat out "/skk/private-fixture-jisyo") 'utf-8-unix)
          skk-large-jisyo (cons (getenv "NELISP_GUI_SKK_DICTIONARY") 'utf-8-unix)
          skk-jisyo-code 'utf-8-unix skk-share-private-jisyo nil
          ;; Pin the standard SPC conversion key for both readers. The fixed
          ;; reader misdecodes the vendor default ?\040 (tracked in NEEDS-SHARED).
          skk-start-henkan-char 32
          skk-server-host nil skk-servers-list nil skk-use-color-cursor nil
          skk-keep-record nil skk-inhibit-ja-dic-search t
          skk-show-mode-show nil skk-auto-okuri-process nil skk-dcomp-activate nil
          skk-use-look nil skk-use-search-web nil skk-use-gtk nil
          skk-use-viper nil
          skk-kakutei-jisyo nil skk-aux-large-jisyo nil
          evil-want-integration nil evil-want-keybinding nil)
    (require 'term/tty-colors "tty-colors")
    ;; The standalone image omits GNU's lazy minibuffer library. Use its
    ;; pinned source: the reader cannot yet skip this release's .elc doc blocks.
    (unless (fboundp 'completion-table-dynamic)
      (load (concat fixture "/gnu/minibuffer.el") nil t t))
    (unless (boundp 'isearch-mode-map)
      (load (concat fixture "/gnu/isearch.el") nil t t))
    (unless (fboundp 'register-input-method)
      (load (concat fixture "/gnu/register-input-method.el") nil t t))
    (require 'skk)
    (require 'evil)
    (set-buffer buffer)
    (set-window-buffer (selected-window) buffer)
    (select-window (selected-window))
    (setq buffer-file-name (concat out "/saved.txt")
          buffer-file-coding-system 'utf-8-unix
          mode-line-format '(" Real ddskk / Evil insert ")
          header-line-format nil)
    (evil-local-mode 1)
    (evil-insert-state)
    (skk-mode 1)
    ;; The fixture starts an ordinary SKK conversion, so xdotool can type the
    ;; requested lowercase nihon + SPC + RET without any editing eval later.
    (skk-set-henkan-point-subr)
    (let ((map (make-sparse-keymap)))
      (define-key map [f5] 'save-buffer)
      (define-key map [f6] 'nelisp-gui-fixture-capture-event)
      (define-key map (vector (event-convert-list '(control ?a))) 'nelisp-gui-fixture-capture-event)
      (define-key map (vector (event-convert-list '(meta ?a))) 'nelisp-gui-fixture-capture-event)
      (define-key map [S-left] 'nelisp-gui-fixture-capture-event)
      (define-key map (vector (event-convert-list '(control shift ?a))) 'nelisp-gui-fixture-capture-event)
      (define-key map [64] 'nelisp-gui-fixture-capture-event)
      (define-key map [91] 'nelisp-gui-fixture-capture-event)
      (define-key map (vector (event-convert-list '(super ?a))) 'nelisp-gui-fixture-capture-event)
      ;; An ordinary emulation map gives these diagnostic keys precedence
      ;; over Evil/SKK while all other keys retain the actual minor-mode maps.
      (setq nelisp-gui-fixture-capture-mode t
            nelisp-gui-fixture-capture-alist
            (list (cons 'nelisp-gui-fixture-capture-mode map)))
      (add-to-list 'emulation-mode-map-alists 'nelisp-gui-fixture-capture-alist))
    (princ (format "GUI-SKK|skk=%S|evil=%S|state=%S|dictionary=%S|buffer=%S|\n"
                   skk-mode evil-local-mode evil-state skk-large-jisyo (buffer-name buffer)))
    (let ((status (format "skk=%S evil=%S state=%S" skk-mode evil-local-mode evil-state)))
      (with-temp-file (concat out "/ready") (insert status)))))
(provide 'nelisp-gui-skk-evil-fixture)
