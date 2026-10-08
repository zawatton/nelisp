;;; skk-evil.el --- Real ddskk/evil fixture shared by GNU and NeLisp -*- lexical-binding: t; -*-
;; No personal dictionary is opened. All paths point into an isolated fixture.
(defvar nelisp-gui-fixture-captured-events nil)
(defvar-local nelisp-gui-fixture-capture-mode nil)
(defvar nelisp-gui-fixture-capture-alist nil)
(defun nelisp-gui-fixture-capture-event ()
  (interactive)
  (push last-command-event nelisp-gui-fixture-captured-events)
  (princ (format "GUI-CAPTURE|event=%S|\n" last-command-event)))

(defun nelisp-gui-skk-evil-configure ()
  "Apply isolated paths and Custom values before loading or using packages."
  (let ((fixture (getenv "NELISP_GUI_VENDOR_FIXTURE"))
        (out (getenv "NELISP_GUI_FIXTURE_OUT")))
    (unless (and fixture out) (error "Missing pinned SKK fixture paths"))
    (dolist (directory (list (concat fixture "/evil") (concat fixture "/ddskk-test")
                             (concat fixture "/gnu")))
      (setq load-path (cons directory (remove directory load-path))))
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
          evil-want-integration nil evil-want-keybinding nil)))

(defun nelisp-gui-skk-evil-load ()
  "Load the genuine fixture packages without opening X or activating modes."
  (nelisp-gui-skk-evil-configure)
  (let ((fixture (getenv "NELISP_GUI_VENDOR_FIXTURE")))
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
    (require 'evil)))

(defvar nelisp-gui-skk-evil-image-dictionary-buffer nil)

(defun nelisp-gui-skk-evil-prepare-image ()
  "Prepare genuine SKK one-time state and the public dictionary headlessly.
Only the isolated empty init and pinned public dictionary are read.  Local
mode state is disposable; each live fixture still activates real modes."
  (nelisp-gui-skk-evil-load)
  (with-temp-buffer (skk-mode 1))
  (setq nelisp-gui-skk-evil-image-dictionary-buffer
        (skk-get-jisyo-buffer skk-large-jisyo t))
  (nelisp-gui-skk-evil-assert-preloaded)
  (nelisp-gui-skk-evil-assert-headless))

(defun nelisp-gui-skk-evil-assert-preloaded ()
  "Reject a dump that lost reusable SKK setup or dictionary buffer state."
  (unless (and (featurep 'skk) (featurep 'evil) skk-mode-invoked
               skk-rule-tree (keymapp skk-j-mode-map)
               (buffer-live-p nelisp-gui-skk-evil-image-dictionary-buffer))
    (error "SKK image missing initialized rules/maps/dictionary"))
  (with-current-buffer nelisp-gui-skk-evil-image-dictionary-buffer
    (unless (and (> (buffer-size) 0) skk-okuri-ari-min
                 skk-okuri-ari-max skk-okuri-nasi-min)
      (error "SKK image missing parsed dictionary boundaries")))
  t)

(defun nelisp-gui-skk-evil-restore-dictionary ()
  "Reuse the dumped dictionary only after the launcher verifies its bytes.
Invalidate the saved buffer for a missing or changed dictionary, including
a replacement with the same basename (ddskk's buffer cache key)."
  (when (buffer-live-p nelisp-gui-skk-evil-image-dictionary-buffer)
    (unless (equal (getenv "NELISP_GUI_SKK_DICTIONARY_PRELOADED") "1")
      (kill-buffer nelisp-gui-skk-evil-image-dictionary-buffer)
      (setq nelisp-gui-skk-evil-image-dictionary-buffer nil))))

(defun nelisp-gui-skk-evil-assert-headless ()
  "Reject live transport/foreign objects before dumping and after restoring."
  (dolist (symbol '(nelisp-gui-frontend--xcb nelisp-gui-frontend--renderer
                    nelisp-gui-selection--state nelisp-gui-selection--owners
                    nl-ffi-libffi--cache nl-ffi-libffi--types
                    nl-ffi-loader--file-mappings nl-ffi-loader--reservations
                    nl-ffi-loader--tls-tp nl-ffi--library-order
                    nl-ffi--pending-cstring-releases))
    (when (and (boundp symbol) (symbol-value symbol))
      (error "Live image state: %s" symbol)))
  (dolist (symbol '(nelisp-gui-xcb--calls nl-ffi--dlsym-cache))
    (when (and (boundp symbol) (> (hash-table-count (symbol-value symbol)) 0))
      (error "Live foreign cache: %s" symbol)))
  (when (boundp 'nl-ffi--libraries)
    (maphash (lambda (name entry)
               (when (plist-get entry :handle) (error "Live library: %s" name)))
             nl-ffi--libraries))
  t)

(defvar nelisp-gui-skk-evil-fingerprint-symbols nil)

(defun nelisp-gui-skk-evil-fingerprint (file)
  "Write deterministic features, keymaps, hooks and package Custom values.
Render complete values with circular references enabled.  Include
all named keymaps/hooks, including shared maps modified by package loading."
  (let ((symbols nil) (state nil) (print-circle t) (print-length nil) (print-level nil)
        (trace (getenv "NELISP_GUI_FINGERPRINT_TRACE")))
    ;; The fixed reader cannot enumerate its global obarray.  The probe
    ;; inventory comes from GNU package loading plus the exact bundle sources.
    (unless nelisp-gui-skk-evil-fingerprint-symbols (error "Missing fingerprint inventory"))
    (dolist (symbol nelisp-gui-skk-evil-fingerprint-symbols)
       (when (boundp symbol)
         (let ((name (symbol-name symbol)))
           (when (or (keymapp (symbol-value symbol))
                     (string-suffix-p "-hook" name)
                     (string-suffix-p "-functions" name)
                     (and (or (string-prefix-p "skk-" name)
                              (string-prefix-p "evil-" name))
                          (get symbol 'custom-type)))
             (push symbol symbols)))))
    (dolist (symbol '(minor-mode-map-alist minor-mode-overriding-map-alist
                      emulation-mode-map-alists overriding-local-map
                      overriding-terminal-local-map input-method-alist load-path))
      (when (and (boundp symbol) (not (memq symbol symbols))) (push symbol symbols)))
    (when trace (princ (format "GUI-FINGERPRINT|scanned=%d|\n" (length symbols))))
    (setq symbols (sort symbols (lambda (a b) (string< (symbol-name a) (symbol-name b)))))
    (when trace (princ "GUI-FINGERPRINT|sorted|\n"))
    (dolist (symbol symbols)
      (push (list symbol (default-value symbol)) state))
    (setq state (cons (cons 'features (sort (copy-sequence features)
                                          (lambda (a b) (string< (symbol-name a) (symbol-name b)))))
                      (nreverse state)))
    ;; The fixed reader's strings already hold UTF-8 bytes.  Write the
    ;; complete graph directly rather than editing a temporary buffer.
    (let ((text (concat (prin1-to-string state) "\n")))
      (if (fboundp 'nl-write-file)
          (nl-write-file file (string-as-unibyte text))
        (write-region text nil file nil 'silent)))
    (princ (format "GUI-PACKAGE-FINGERPRINT|variables=%d|\n" (length symbols)))))

(defun nelisp-gui-skk-evil-fixture ()
  ;; A restored heap already owns package definitions and initialized rules.
  ;; Always rebind live paths; ordinary GNU/base-image fixtures still load.
  (if (and (featurep 'skk) (featurep 'evil))
      (nelisp-gui-skk-evil-configure)
    (nelisp-gui-skk-evil-load))
  (nelisp-gui-skk-evil-restore-dictionary)
  (let* ((out (getenv "NELISP_GUI_FIXTURE_OUT"))
         (buffer (generate-new-buffer "*SKK GUI*")))
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
