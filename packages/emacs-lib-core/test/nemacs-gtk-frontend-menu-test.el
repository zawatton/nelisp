;;; nemacs-gtk-frontend-menu-test.el --- ERT for GTK menu wiring -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(setq load-prefer-newer t)
(require 'nemacs-gtk-frontend)

(ert-deftest nemacs-gtk-menu-spec-includes-find-file-leaf ()
  (let* ((file-menu (assoc "File" nemacs-gtk--menu-spec))
         (leaves (cdr file-menu)))
    (should (equal '("Open File..." . "find-file")
                   (assoc "Open File..." leaves)))))

(ert-deftest nemacs-gtk-menu-spec-includes-shared-help-leaves ()
  (let* ((help-menu (assoc "Help" nemacs-gtk--menu-spec))
         (leaves (cdr help-menu)))
    (should (equal '("Describe Function..." . "describe-function")
                   (assoc "Describe Function..." leaves)))
    (should (equal '("Describe Bindings" . "describe-bindings")
                   (assoc "Describe Bindings" leaves)))
    (should (equal '("Apropos..." . "apropos")
                   (assoc "Apropos..." leaves)))))

(ert-deftest nemacs-gtk-menu-accels-defconst-shape ()
  (should (listp nemacs-gtk--menu-accels))
  (dolist (entry nemacs-gtk--menu-accels)
    (should (consp entry))
    (should (stringp (car entry)))
    (should (stringp (cdr entry)))))

(ert-deftest nemacs-gtk-menu-command-actions-map-direct-commands ()
  (should (eq 'find-file
              (emacs-command-loop-menu-action-command
               "find-file" nemacs-gtk--menu-command-actions)))
  (should (eq 'undo
              (emacs-command-loop-menu-action-command
               "undo" nemacs-gtk--menu-command-actions)))
  (should (eq 'delete-other-windows
              (emacs-command-loop-menu-action-command
               "delete-other-windows" nemacs-gtk--menu-command-actions)))
  (should-not
   (emacs-command-loop-menu-action-command
    "describe-function" nemacs-gtk--menu-command-actions)))

(ert-deftest nemacs-gtk-key-event-normalization-uses-shared-api ()
  (should (eq 'left
              (emacs-command-loop-normalize-key-event
               nemacs-gtk--keysym-left 0 0
               :named-events nemacs-gtk--keysym-command-loop-events
               :control-mask nemacs-gtk--gdk-control-mask)))
  (should (= ?\C-x
             (emacs-command-loop-normalize-key-event
              ?x nemacs-gtk--gdk-control-mask 0
              :named-events nemacs-gtk--keysym-command-loop-events
              :control-mask nemacs-gtk--gdk-control-mask))))

(ert-deftest nemacs-gtk-keyboard-quit-uses-shared-reset-plan ()
  (let ((nemacs-gtk--active-buffer-name "*welcome*")
        (nemacs-gtk--minibuffer-active t)
        (nemacs-gtk--minibuffer-prompt "M-x ")
        (nemacs-gtk--minibuffer-input "abc")
        (nemacs-gtk--minibuffer-on-confirm #'ignore)
        (nemacs-gtk--minibuffer-completion-fn #'list)
        (nemacs-gtk--minibuffer-candidates '("abc"))
        (nemacs-gtk--isearch-active t)
        (nemacs-gtk--query-replace-pending-key t)
        (nemacs-gtk--query-replace-state 'state)
        (nemacs-gtk--describe-key-pending t)
        (nemacs-gtk--register-pending-op 'copy)
        (nemacs-gtk--quoted-insert-pending t)
        (nemacs-gtk--mark-pos 2)
        (nemacs-gtk--mark-buffer "*welcome*")
        (nemacs-gtk--shift-region t)
        (nemacs-gtk--pending-prefix [24])
        (nemacs-gtk--last-key-text ""))
    (get-buffer-create "*welcome*")
    (nemacs-gtk-keyboard-quit)
    (should-not nemacs-gtk--minibuffer-active)
    (should-not nemacs-gtk--isearch-active)
    (should-not nemacs-gtk--query-replace-pending-key)
    (should-not nemacs-gtk--query-replace-state)
    (should-not nemacs-gtk--describe-key-pending)
    (should-not nemacs-gtk--register-pending-op)
    (should-not nemacs-gtk--quoted-insert-pending)
    (should-not nemacs-gtk--mark-pos)
    (should-not nemacs-gtk--mark-buffer)
    (should-not nemacs-gtk--shift-region)
      (should-not nemacs-gtk--pending-prefix)
      (should (equal "Quit" nemacs-gtk--last-key-text))))

(ert-deftest nemacs-gtk-isearch-search-from-start-uses-shared-helper ()
  (let ((nemacs-gtk--active-buffer-name "*welcome*")
        (nemacs-gtk--isearch-query "foo")
        (nemacs-gtk--isearch-direction 'forward)
        (nemacs-gtk--isearch-start-pos 7)
        (nemacs-gtk--isearch-failing nil)
        called)
    (get-buffer-create "*welcome*")
    (cl-letf (((symbol-function 'emacs-isearch-search-from-start-direct)
               (lambda (query direction start-point)
                 (setq called (list query direction start-point
                                    (buffer-name)))
                 (list :status 'failing
                       :query query
                       :direction direction
                       :start-point start-point
                       :failing t
                       :point start-point))))
      (let ((result (nemacs-gtk--isearch-search-from-start)))
        (should (eq 'failing (plist-get result :status)))
        (should nemacs-gtk--isearch-failing)
        (should (equal '("foo" forward 7 "*welcome*")
                       called))))))

(ert-deftest nemacs-gtk-isearch-cancel-uses-shared-restore-helper ()
  (let ((nemacs-gtk--active-buffer-name "*welcome*")
        (nemacs-gtk--isearch-active t)
        (nemacs-gtk--isearch-start-pos 7)
        (nemacs-gtk--last-key-text "")
        called)
    (get-buffer-create "*welcome*")
    (cl-letf (((symbol-function 'emacs-isearch-restore-start-direct)
               (lambda (start-point)
                 (setq called (list start-point (buffer-name)))
                 (list :status 'restored
                       :point start-point
                       :failing nil))))
      (should (nemacs-gtk--isearch-handle-key 7))
      (should-not nemacs-gtk--isearch-active)
      (should (equal '(7 "*welcome*") called))
      (should (equal "isearch cancelled"
                     nemacs-gtk--last-key-text)))))

(ert-deftest nemacs-gtk-isearch-repeat-uses-shared-helper ()
  (let ((nemacs-gtk--active-buffer-name "*welcome*")
        (nemacs-gtk--isearch-active t)
        (nemacs-gtk--isearch-query "foo")
        (nemacs-gtk--isearch-direction 'forward)
        (nemacs-gtk--isearch-failing t)
        called)
    (get-buffer-create "*welcome*")
    (cl-letf (((symbol-function 'emacs-isearch-repeat-direct)
               (lambda (query direction)
                 (setq called (list query direction (buffer-name)))
                 (list :status 'found
                       :query query
                       :direction direction
                       :failing nil
                       :point 12))))
      (should (nemacs-gtk--isearch-handle-key 19))
      (should-not nemacs-gtk--isearch-failing)
      (should (equal '("foo" forward "*welcome*")
                     called)))))

(ert-deftest nemacs-gtk-isearch-source-shape-uses-shared-helpers ()
  (let ((source
         (with-temp-buffer
           (insert-file-contents (locate-library "nemacs-gtk-frontend.el"))
           (goto-char (point-min))
           (re-search-forward "(defun nemacs-gtk--isearch-search-from-start" nil t)
           (let ((start (match-beginning 0)))
             (goto-char start)
             (forward-sexp)
             (let ((first (buffer-substring start (point))))
               (re-search-forward "(defun nemacs-gtk--isearch-handle-key" nil t)
               (setq start (match-beginning 0))
               (goto-char start)
               (forward-sexp)
               (concat first "\n" (buffer-substring start (point))))))))
    (dolist (needle '("emacs-isearch-search-from-start-direct"
                      "emacs-isearch-restore-start-direct"
                      "emacs-isearch-repeat-direct"))
      (should (string-match-p (regexp-quote needle) source)))
    (dolist (needle '("nelisp-ec-goto-char"
                      "search-forward nemacs-gtk--isearch-query"
                      "search-backward nemacs-gtk--isearch-query"))
      (should-not (string-match-p (regexp-quote needle) source)))))

(ert-deftest nemacs-gtk-query-replace-uses-shared-callback-helper ()
  (let ((nemacs-gtk--active-buffer-name "gtk-query-replace-test")
        (nemacs-gtk--query-replace-state nil)
        (nemacs-gtk--query-replace-pending-key nil)
        (nemacs-gtk--last-key-text ""))
    (with-current-buffer (get-buffer-create "gtk-query-replace-test")
      (erase-buffer)
      (insert "alpha beta alpha")
      (goto-char (point-min)))
    (nemacs-gtk-query-replace)
    (should nemacs-gtk--minibuffer-active)
    (should (equal "Query replace: " nemacs-gtk--minibuffer-prompt))
    (funcall nemacs-gtk--minibuffer-on-confirm "alpha")
    (should (equal "Query replace alpha with: "
                   nemacs-gtk--minibuffer-prompt))
    (funcall nemacs-gtk--minibuffer-on-confirm "OMEGA")
    (should (emacs-query-replace-session-active-p
             nemacs-gtk--query-replace-state))
    (should nemacs-gtk--query-replace-pending-key)
    (should (equal "Replace alpha with OMEGA? (y/n/!/q)"
                   nemacs-gtk--last-key-text))))

(ert-deftest nemacs-gtk-query-replace-empty-from-reports-shared-status ()
  (let ((nemacs-gtk--active-buffer-name "*welcome*")
        (nemacs-gtk--query-replace-state nil)
        (nemacs-gtk--query-replace-pending-key nil)
        (nemacs-gtk--last-key-text ""))
    (get-buffer-create "*welcome*")
    (nemacs-gtk-query-replace)
    (funcall nemacs-gtk--minibuffer-on-confirm "")
    (should-not nemacs-gtk--query-replace-state)
    (should-not nemacs-gtk--query-replace-pending-key)
    (should (equal "query-replace: empty FROM"
                   nemacs-gtk--last-key-text))))

(ert-deftest nemacs-gtk-minibuffer-tab-uses-shared-plan ()
  (let ((nemacs-gtk--active-buffer-name "*welcome*")
        (nemacs-gtk--minibuffer-active t)
        (nemacs-gtk--minibuffer-completion-fn #'list)
        (nemacs-gtk--minibuffer-input "a")
        (nemacs-gtk--minibuffer-candidates '("alpha" "alpine"))
        (nemacs-gtk--last-key-text "")
        called)
    (get-buffer-create "*welcome*")
    (cl-letf (((symbol-function 'emacs-minibuffer-gui-key-plan)
               (lambda (event input candidates completion-fn)
                 (setq called (list event input candidates completion-fn))
                 (list :action 'update
                       :input "alp"
                       :candidates '("alpha" "alpine")
                       :message "2 candidates"))))
      (nemacs-gtk--dispatch-key nemacs-gtk--keysym-tab 0 0)
      (should (equal (list 'tab "a" '("alpha" "alpine") #'list)
                     called))
      (should (equal "alp" nemacs-gtk--minibuffer-input))
      (should (equal '("alpha" "alpine")
                     nemacs-gtk--minibuffer-candidates))
      (should (equal "2 candidates" nemacs-gtk--last-key-text)))))

(ert-deftest nemacs-gtk-prompt-state-uses-shared-plans ()
  (let ((callback #'ignore)
        (completion-fn #'list)
        called-enter
        exit-called)
    (cl-letf (((symbol-function 'emacs-minibuffer-gui-enter-state)
               (lambda (prompt on-confirm fn)
                 (setq called-enter (list prompt on-confirm fn))
                 (list :active t
                       :prompt "Prompt: "
                       :input ""
                       :on-confirm on-confirm
                       :completion-fn fn
                       :candidates '("alpha"))))
              ((symbol-function 'emacs-minibuffer-gui-exit-state)
               (lambda ()
                 (setq exit-called t)
                 (list :active nil
                       :prompt ""
                       :input ""
                       :on-confirm nil
                       :completion-fn nil
                       :candidates nil))))
      (nemacs-gtk--begin-prompt "Prompt: " callback completion-fn)
      (should (equal (list "Prompt: " callback completion-fn)
                     called-enter))
      (should nemacs-gtk--minibuffer-active)
      (should (equal "Prompt: " nemacs-gtk--minibuffer-prompt))
      (should (eq callback nemacs-gtk--minibuffer-on-confirm))
      (should (eq completion-fn nemacs-gtk--minibuffer-completion-fn))
      (should (equal '("alpha") nemacs-gtk--minibuffer-candidates))
      (nemacs-gtk--end-prompt)
      (should exit-called)
      (should-not nemacs-gtk--minibuffer-active)
      (should (equal "" nemacs-gtk--minibuffer-prompt))
      (should-not nemacs-gtk--minibuffer-on-confirm)
      (should-not nemacs-gtk--minibuffer-completion-fn)
      (should-not nemacs-gtk--minibuffer-candidates))))

(ert-deftest nemacs-gtk-set-mark-command-uses-shared-helper ()
  (let ((nemacs-gtk--active-buffer-name "*welcome*")
        (nemacs-gtk--mark-pos nil)
        (nemacs-gtk--mark-buffer nil)
        (nemacs-gtk--shift-region t)
        (nemacs-gtk--last-key-text ""))
    (get-buffer-create "*welcome*")
    (cl-letf (((symbol-function 'emacs-edit-set-mark-direct)
               (lambda (buffer-name)
                 (should (equal "*welcome*" buffer-name))
                 (list :mark 9
                       :buffer buffer-name
                       :shift-region nil
                       :message "Mark set @ 9"))))
      (nemacs-gtk-set-mark-command)
      (should (equal 9 nemacs-gtk--mark-pos))
      (should (equal "*welcome*" nemacs-gtk--mark-buffer))
      (should-not nemacs-gtk--shift-region)
      (should (equal "Mark set @ 9" nemacs-gtk--last-key-text)))))

(ert-deftest nemacs-gtk-region-bounds-uses-shared-helper ()
  (let ((nemacs-gtk--active-buffer-name "*welcome*")
        (nemacs-gtk--mark-pos 3)
        (nemacs-gtk--mark-buffer "*welcome*")
        called)
    (get-buffer-create "*welcome*")
    (cl-letf (((symbol-function 'emacs-edit-region-bounds-direct)
               (lambda (mark-pos mark-buffer active-buffer-name)
                 (setq called (list mark-pos mark-buffer active-buffer-name))
                 '(3 . 7))))
      (should (equal '(3 . 7) (nemacs-gtk--region-bounds)))
      (should (equal '(3 "*welcome*" "*welcome*") called)))))

(ert-deftest nemacs-gtk-shift-selection-before-motion-uses-shared-plan ()
  (let ((nemacs-gtk--active-buffer-name "*welcome*")
        (nemacs-gtk--mark-pos nil)
        (nemacs-gtk--mark-buffer nil)
        (nemacs-gtk--shift-region nil)
        (nemacs-gtk--last-key-text "")
        plan-args)
    (get-buffer-create "*welcome*")
    (cl-letf (((symbol-function 'nelisp-ec-point)
               (lambda () 11))
              ((symbol-function 'emacs-edit-shift-selection-plan)
               (lambda (event mods &rest plist)
                 (setq plan-args (list event mods plist))
                 (list :action 'activate
                       :mark 11
                       :buffer "*welcome*"
                       :shift-region t
                       :message "Mark activated"))))
      (nemacs-gtk--shift-selection-before-motion
       'right nemacs-gtk--gdk-shift-mask)
      (should (eq 'right (car plan-args)))
      (should (equal 11 nemacs-gtk--mark-pos))
      (should (equal "*welcome*" nemacs-gtk--mark-buffer))
      (should nemacs-gtk--shift-region)
      (should (equal "Mark activated" nemacs-gtk--last-key-text)))))

(ert-deftest nemacs-gtk-page-scroll-uses-shared-helper ()
  (let ((nemacs-gtk--active-buffer-name "*welcome*")
        (nemacs-gtk--buffer-area-end 12)
        helper-calls
        scroll-calls
        ensure-count)
    (get-buffer-create "*welcome*")
    (cl-letf (((symbol-function 'emacs-edit-page-scroll-direct)
               (lambda (direction viewport-height)
                 (push (list direction viewport-height) helper-calls)
                 (list :status 'moved
                       :direction direction
                       :delta (if (eq direction 'up) -10 10))))
              ((symbol-function 'nemacs-gtk--scroll-by)
               (lambda (delta)
                 (push delta scroll-calls)))
              ((symbol-function 'nemacs-gtk--ensure-cursor-visible)
               (lambda ()
                 (setq ensure-count (1+ (or ensure-count 0))))))
      (nemacs-gtk-page-up)
      (nemacs-gtk-page-down)
      (should (equal '((down 12) (up 12)) helper-calls))
      (should (equal '(10 -10) scroll-calls))
      (should (= 2 ensure-count)))))

(ert-deftest nemacs-gtk-switch-to-buffer-uses-shared-plan ()
  (let ((nemacs-gtk--active-buffer-name "*welcome*")
        (nemacs-gtk--scroll-offset 9)
        (nemacs-gtk--last-key-text "")
        sync-called)
    (get-buffer-create "*welcome*")
    (get-buffer-create "target")
    (cl-letf (((symbol-function 'nemacs-gtk--sync-window-title)
               (lambda () (setq sync-called t)))
              ((symbol-function 'nemacs-gtk--buffer-mode)
               (lambda (name)
                 (should (equal "target" name))
                 'text-mode)))
      (nemacs-gtk-switch-to-buffer)
      (should nemacs-gtk--minibuffer-active)
      (funcall nemacs-gtk--minibuffer-on-confirm "target")
      (should (equal "target" nemacs-gtk--active-buffer-name))
      (should (= 0 nemacs-gtk--scroll-offset))
      (should sync-called)
      (should (equal "Switched: target" nemacs-gtk--last-key-text)))))

(ert-deftest nemacs-gtk-switch-to-buffer-missing-uses-shared-message ()
  (let ((nemacs-gtk--active-buffer-name "*welcome*")
        (nemacs-gtk--scroll-offset 9)
        (nemacs-gtk--last-key-text "")
        sync-called)
    (get-buffer-create "*welcome*")
    (cl-letf (((symbol-function 'nemacs-gtk--sync-window-title)
               (lambda () (setq sync-called t))))
      (nemacs-gtk-switch-to-buffer)
      (funcall nemacs-gtk--minibuffer-on-confirm "missing")
      (should (equal "*welcome*" nemacs-gtk--active-buffer-name))
      (should (= 9 nemacs-gtk--scroll-offset))
      (should-not sync-called)
      (should (equal "No buffer: missing" nemacs-gtk--last-key-text)))))

(ert-deftest nemacs-gtk-kill-buffer-uses-shared-plan ()
  (let ((nemacs-gtk--active-buffer-name "target")
        (nemacs-gtk--scroll-offset 9)
        (nemacs-gtk--last-key-text "")
        sync-called)
    (get-buffer-create "*welcome*")
    (get-buffer-create "target")
    (cl-letf (((symbol-function 'nemacs-gtk--sync-window-title)
               (lambda () (setq sync-called t))))
      (nemacs-gtk-kill-buffer)
      (should-not (get-buffer "target"))
      (should (equal "*welcome*" nemacs-gtk--active-buffer-name))
      (should (= 0 nemacs-gtk--scroll-offset))
      (should sync-called)
      (should (equal "Killed: target" nemacs-gtk--last-key-text)))))

(ert-deftest nemacs-gtk-kill-buffer-refuses-welcome-via-shared-plan ()
  (let ((nemacs-gtk--active-buffer-name "*welcome*")
        (nemacs-gtk--scroll-offset 9)
        (nemacs-gtk--last-key-text "")
        sync-called)
    (get-buffer-create "*welcome*")
    (cl-letf (((symbol-function 'nemacs-gtk--sync-window-title)
               (lambda () (setq sync-called t))))
      (nemacs-gtk-kill-buffer)
      (should (get-buffer "*welcome*"))
      (should (equal "*welcome*" nemacs-gtk--active-buffer-name))
      (should (= 9 nemacs-gtk--scroll-offset))
      (should-not sync-called)
      (should (equal "kill-buffer: refusing *welcome*"
                     nemacs-gtk--last-key-text)))))

(ert-deftest nemacs-gtk-save-buffers-kill-emacs-no-dirty-uses-shared-helper ()
  (let ((nemacs-gtk--quit-requested nil)
        (nemacs-gtk--last-key-text ""))
    (cl-letf (((symbol-function 'nemacs-gtk--unsaved-file-buffers)
               (lambda () nil)))
      (nemacs-gtk-save-buffers-kill-emacs)
      (should nemacs-gtk--quit-requested)
      (should (equal "C-x C-c → quit" nemacs-gtk--last-key-text)))))

(ert-deftest nemacs-gtk-save-buffers-kill-emacs-saves-dirty-via-shared-helper ()
  (let ((nemacs-gtk--quit-requested nil)
        (nemacs-gtk--last-key-text "")
        (saved nil)
        (buffer (get-buffer-create "dirty")))
    (cl-letf (((symbol-function 'nemacs-gtk--unsaved-file-buffers)
               (lambda () (list buffer)))
              ((symbol-function 'save-buffer)
               (lambda ()
                 (push (buffer-name) saved))))
      (nemacs-gtk-save-buffers-kill-emacs)
      (should nemacs-gtk--minibuffer-active)
      (should (equal "1 modified buffer(s).  Save? (y/n/c): "
                     nemacs-gtk--minibuffer-prompt))
      (funcall nemacs-gtk--minibuffer-on-confirm "y")
      (should nemacs-gtk--quit-requested)
      (should (equal '("dirty") saved))
      (should (equal "Saved 1 buffer(s) — quit"
                     nemacs-gtk--last-key-text)))))

(ert-deftest nemacs-gtk-buffer-menu-spec-uses-shared-helper ()
  (let (called)
    (cl-letf (((symbol-function 'buffer-list)
               (lambda () '(:a :b)))
              ((symbol-function 'emacs-buffer-ui-buffer-menu-spec)
               (lambda (buffers &rest plist)
                 (setq called (list buffers plist))
                 '(("  alpha" . "switch-to-buffer:alpha")))))
      (should (equal '(("  alpha" . "switch-to-buffer:alpha"))
                     (nemacs-gtk--buffer-menu-spec)))
      (should (equal '(:a :b) (car called)))
      (should (plist-get (cadr called) :name-function))
      (should (plist-get (cadr called) :file-function))
      (should (plist-get (cadr called) :modified-function)))))

(ert-deftest nemacs-gtk-bookmark-completion-uses-shared-helper ()
  (let ((nemacs-gtk--bookmarks '(("alpha" . (:buffer "a" :pos 1))))
        called)
    (cl-letf (((symbol-function 'emacs-bookmark-ui-completion-candidates)
               (lambda (bookmarks input)
                 (setq called (list bookmarks input))
                 '("alpha"))))
      (should (equal '("alpha")
                     (nemacs-gtk--bookmark-completion "al")))
      (should (equal (list nemacs-gtk--bookmarks "al") called)))))

(ert-deftest nemacs-gtk-bookmark-list-uses-shared-listing ()
  (let ((nemacs-gtk--bookmarks '(("alpha" . (:buffer "a" :pos 1))))
        (nemacs-gtk--active-buffer-name "*welcome*")
        (nemacs-gtk--scroll-offset 9)
        (nemacs-gtk--last-key-text "")
        called
        replaced
        sync-called)
    (cl-letf (((symbol-function 'emacs-bookmark-ui-listing)
               (lambda (bookmarks)
                 (setq called bookmarks)
                 (list :entries bookmarks
                       :count 1
                       :text "Bookmarks:\n\n  alpha -> a:1\n")))
              ((symbol-function 'emacs-buffer-ui-replace-text-buffer)
               (lambda (name text &optional ensure-final-newline)
                 (setq replaced
                       (list name text ensure-final-newline))
                 (list :status 'replaced
                       :buffer-name name
                       :text text)))
              ((symbol-function 'nemacs-gtk--sync-window-title)
               (lambda () (setq sync-called t))))
      (nemacs-gtk-bookmark-list)
      (should (equal nemacs-gtk--bookmarks called))
      (should (equal '("*Bookmarks*" "Bookmarks:\n\n  alpha -> a:1\n" nil)
                     replaced))
      (should (equal "*Bookmarks*" nemacs-gtk--active-buffer-name))
      (should (= 0 nemacs-gtk--scroll-offset))
      (should sync-called)
      (should (equal "bookmark-list: 1 entries"
                     nemacs-gtk--last-key-text)))))

(ert-deftest nemacs-gtk-bookmark-jump-uses-shared-plan-and-goto-helper ()
  (let ((nemacs-gtk--bookmarks '(("alpha" . (:buffer "target" :pos 99))))
        (nemacs-gtk--active-buffer-name "*welcome*")
        (nemacs-gtk--last-key-text "")
        prompt-args
        plan-args
        goto-args
        ensure-called)
    (get-buffer-create "*welcome*")
    (get-buffer-create "target")
    (cl-letf (((symbol-function 'nemacs-gtk--begin-prompt)
               (lambda (prompt callback completion-fn)
                 (setq prompt-args (list prompt completion-fn))
                 (funcall callback "alpha")))
              ((symbol-function 'emacs-bookmark-ui-jump-plan)
               (lambda (bookmarks input &optional buffer-exists-p)
                 (setq plan-args (list bookmarks input buffer-exists-p))
                 (list :status 'ok
                       :bookmark input
                       :buffer-name "target"
                       :point 99
                       :message "bookmark-jump: alpha -> target:99")))
              ((symbol-function 'emacs-edit-goto-position-direct)
               (lambda (pos)
                 (setq goto-args (list pos (buffer-name)))
                 (list :status 'moved
                       :requested-point pos
                       :point 4)))
              ((symbol-function 'nemacs-gtk--ensure-cursor-visible)
               (lambda () (setq ensure-called t))))
      (nemacs-gtk-bookmark-jump)
      (should (equal (list "Jump to bookmark: "
                           #'nemacs-gtk--bookmark-completion)
                     prompt-args))
      (should (equal (list nemacs-gtk--bookmarks "alpha" #'get-buffer)
                     plan-args))
      (should (equal "target" nemacs-gtk--active-buffer-name))
      (should (equal '(99 "target") goto-args))
      (should ensure-called)
      (should (equal "bookmark-jump: alpha -> target:99"
                     nemacs-gtk--last-key-text)))))

(ert-deftest nemacs-gtk-bookmark-jump-source-shape-uses-shared-helpers ()
  (let ((source
         (with-temp-buffer
           (insert-file-contents (locate-library "nemacs-gtk-frontend.el"))
           (goto-char (point-min))
           (re-search-forward "(defun nemacs-gtk-bookmark-jump" nil t)
           (let ((start (match-beginning 0)))
             (goto-char start)
             (forward-sexp)
             (buffer-substring start (point))))))
    (should (string-match-p
             (regexp-quote "emacs-bookmark-ui-jump-plan")
             source))
    (should (string-match-p
             (regexp-quote "emacs-edit-goto-position-direct")
             source))
    (should-not
     (string-match-p
      (regexp-quote "nelisp-ec-goto-char")
      source))))

(ert-deftest nemacs-gtk-show-text-buffer-uses-shared-buffer-helper ()
  (let ((nemacs-gtk--active-buffer-name "*welcome*")
        (nemacs-gtk--scroll-offset 9)
        replaced
        sync-called)
    (cl-letf (((symbol-function 'emacs-buffer-ui-replace-text-buffer)
               (lambda (name text &optional ensure-final-newline)
                 (setq replaced
                       (list name text ensure-final-newline))
                 (list :status 'replaced
                       :buffer-name name
                       :text text)))
              ((symbol-function 'nemacs-gtk--sync-window-title)
               (lambda () (setq sync-called t))))
      (nemacs-gtk--show-text-buffer "*Help*" "body")
      (should (equal '("*Help*" "body" t) replaced))
      (should (equal "*Help*" nemacs-gtk--active-buffer-name))
      (should (= 0 nemacs-gtk--scroll-offset))
      (should sync-called))))

(ert-deftest nemacs-gtk-text-buffer-source-shape-uses-shared-buffer-helper ()
  (let ((source
         (with-temp-buffer
           (insert-file-contents (locate-library "nemacs-gtk-frontend.el"))
           (buffer-string))))
    (should (string-match-p
             (regexp-quote "emacs-buffer-ui-replace-text-buffer")
             source))
    (should-not
     (string-match-p
      (regexp-quote "(defun nemacs-gtk--get-or-create-text-buffer")
      source))))

(ert-deftest nemacs-gtk-special-buffer-commands-use-shared-plans ()
  (let ((nemacs-gtk--active-buffer-name "*welcome*")
        (nemacs-gtk--scroll-offset 9)
        (nemacs-gtk--last-key-text "")
        calls
        sync-count)
    (cl-letf (((symbol-function 'emacs-special-buffers-display-plan)
               (lambda (name message)
                 (push (list name message) calls)
                 (list :status 'ok
                       :buffer-name name
                       :scroll-offset 0
                       :message message)))
              ((symbol-function 'nemacs-gtk--sync-window-title)
               (lambda () (setq sync-count (1+ (or sync-count 0))))))
      (nemacs-gtk-scratch-buffer)
      (should (equal "*scratch*" nemacs-gtk--active-buffer-name))
      (should (= 0 nemacs-gtk--scroll-offset))
      (should (equal "scratch-buffer" nemacs-gtk--last-key-text))
      (nemacs-gtk-messages-buffer)
      (should (equal "*Messages*" nemacs-gtk--active-buffer-name))
      (should (= 0 nemacs-gtk--scroll-offset))
      (should (equal "messages-buffer" nemacs-gtk--last-key-text))
      (should (equal '(("*Messages*" "messages-buffer")
                       ("*scratch*" "scratch-buffer"))
                     calls))
      (should (= 2 sync-count)))))

(ert-deftest nemacs-gtk-cheat-sheet-uses-shared-display-position-helper ()
  (let ((nemacs-gtk--active-buffer-name "other")
        (nemacs-gtk--scroll-offset 9)
        (nemacs-gtk--last-key-text "")
        prepare-called
        moved-in-buffer
        sync-called)
    (get-buffer-create "*welcome*")
    (cl-letf (((symbol-function 'nemacs-gtk--prepare-cheat-sheet-buffer)
               (lambda () (setq prepare-called t)))
              ((symbol-function 'emacs-buffer-ui-move-to-buffer-start)
               (lambda (&optional buffer)
                 (setq moved-in-buffer (list buffer (buffer-name)))
                 (list :status 'moved
                       :point 1
                       :scroll-offset 0)))
              ((symbol-function 'nemacs-gtk--sync-window-title)
               (lambda () (setq sync-called t))))
      (nemacs-gtk-cheat-sheet)
      (should prepare-called)
      (should (equal "*welcome*" nemacs-gtk--active-buffer-name))
      (should (equal '(nil "*welcome*") moved-in-buffer))
      (should (= 0 nemacs-gtk--scroll-offset))
      (should sync-called)
      (should (equal "cheat-sheet" nemacs-gtk--last-key-text)))))

(ert-deftest nemacs-gtk-special-buffer-source-shape-uses-shared-plans ()
  (let ((source
         (with-temp-buffer
           (insert-file-contents (locate-library "nemacs-gtk-frontend.el"))
           (buffer-string))))
    (should (string-match-p
             (regexp-quote "emacs-special-buffers-display-plan")
             source))
    (should-not
     (string-match-p
      (regexp-quote ";; This buffer is for text that is not saved")
      source))
    (should-not
     (string-match-p
      (regexp-quote "(nelisp-ec-insert")
      source))))

(ert-deftest nemacs-gtk-cheat-sheet-source-shape-uses-shared-display-helper ()
  (let ((source
         (with-temp-buffer
           (insert-file-contents (locate-library "nemacs-gtk-frontend.el"))
           (goto-char (point-min))
           (re-search-forward "(defun nemacs-gtk-cheat-sheet" nil t)
           (let ((start (match-beginning 0)))
             (goto-char start)
             (forward-sexp)
             (buffer-substring start (point))))))
    (should (string-match-p
             (regexp-quote "emacs-buffer-ui-move-to-buffer-start")
             source))
    (should-not
     (string-match-p
      (regexp-quote "nelisp-ec-goto-char")
      source))))

(ert-deftest nemacs-gtk-buffer-boundary-commands-use-shared-edit ()
  (let ((nemacs-gtk--active-buffer-name "*welcome*")
        calls)
    (get-buffer-create "*welcome*")
    (cl-letf (((symbol-function 'emacs-edit-goto-buffer-boundary-direct)
               (lambda (boundary)
                 (push boundary calls)
                 (list :status 'moved
                       :boundary boundary
                       :point (if (eq boundary 'beginning) 1 9)))))
      (nemacs-gtk-meta-beginning-of-buffer)
      (nemacs-gtk-meta-end-of-buffer)
      (should (equal '(end beginning) calls)))))

(ert-deftest nemacs-gtk-mark-whole-buffer-uses-shared-edit ()
  (let ((nemacs-gtk--active-buffer-name "*welcome*")
        (nemacs-gtk--mark-pos nil)
        (nemacs-gtk--mark-buffer nil)
        (nemacs-gtk--shift-region t)
        (nemacs-gtk--last-key-text "")
        called-buffer)
    (get-buffer-create "*welcome*")
    (cl-letf (((symbol-function 'emacs-edit-mark-whole-buffer-direct)
               (lambda (&optional buffer-name)
                 (setq called-buffer buffer-name)
                 (list :status 'marked
                       :mark 1
                       :point 9
                       :buffer buffer-name
                       :shift-region nil
                       :message "Selected whole buffer (8 chars)"))))
      (nemacs-gtk-mark-whole-buffer)
      (should (equal "*welcome*" called-buffer))
      (should (equal 1 nemacs-gtk--mark-pos))
      (should (equal "*welcome*" nemacs-gtk--mark-buffer))
      (should-not nemacs-gtk--shift-region)
      (should (equal "Selected whole buffer (8 chars)"
                     nemacs-gtk--last-key-text)))))

(ert-deftest nemacs-gtk-exchange-point-and-mark-uses-shared-edit ()
  (let ((nemacs-gtk--active-buffer-name "*welcome*")
        (nemacs-gtk--mark-pos 2)
        (nemacs-gtk--mark-buffer "*welcome*")
        (nemacs-gtk--shift-region t)
        (nemacs-gtk--last-key-text "")
        called-args)
    (get-buffer-create "*welcome*")
    (cl-letf (((symbol-function 'emacs-edit-exchange-point-and-mark-direct)
               (lambda (mark-pos mark-buffer active-buffer-name)
                 (setq called-args
                       (list mark-pos mark-buffer active-buffer-name))
                 (list :status 'exchanged
                       :point 2
                       :mark 7
                       :buffer active-buffer-name
                       :shift-region nil
                       :message "Exchange point and mark"))))
      (nemacs-gtk-exchange-point-and-mark)
      (should (equal '(2 "*welcome*" "*welcome*") called-args))
      (should (equal 7 nemacs-gtk--mark-pos))
      (should (equal "*welcome*" nemacs-gtk--mark-buffer))
      (should-not nemacs-gtk--shift-region)
      (should (equal "Exchange point and mark"
                     nemacs-gtk--last-key-text)))))

(ert-deftest nemacs-gtk-buffer-motion-source-shape-uses-shared-edit ()
  (let ((source
         (with-temp-buffer
           (insert-file-contents (locate-library "nemacs-gtk-frontend.el"))
           (buffer-string))))
    (dolist (needle '("emacs-edit-goto-buffer-boundary-direct"
                      "emacs-edit-mark-whole-buffer-direct"
                      "emacs-edit-exchange-point-and-mark-direct"))
      (should (string-match-p (regexp-quote needle) source)))))

(ert-deftest nemacs-gtk-shell-command-uses-shared-buffer-helper ()
  (let ((nemacs-gtk--last-key-text "")
        displayed
        seen-command)
    (cl-letf (((symbol-function 'emacs-shell-command-run-to-string)
               (lambda (command &optional input)
                 (setq seen-command (list command input))
                 "shell out\n"))
              ((symbol-function 'nemacs-gtk--show-text-buffer)
               (lambda (name text)
                 (setq displayed (list name text)))))
      (nemacs-gtk-shell-command)
      (should nemacs-gtk--minibuffer-active)
      (funcall nemacs-gtk--minibuffer-on-confirm "printf ok")
      (should (equal '("printf ok" nil) seen-command))
      (should (equal '("*Shell Command Output*" "shell out\n")
                     displayed))
      (should (equal "shell-command: printf ok (10 bytes)"
                     nemacs-gtk--last-key-text)))))

(ert-deftest nemacs-gtk-shell-command-on-region-uses-shared-buffer-helper ()
  (let ((nemacs-gtk--active-buffer-name "*welcome*")
        (nemacs-gtk--last-key-text "")
        displayed
        seen-command)
    (get-buffer-create "*welcome*")
    (cl-letf (((symbol-function 'nemacs-gtk--region-bounds)
               (lambda () '(2 . 8)))
              ((symbol-function 'nelisp-ec-buffer-substring)
               (lambda (beg end)
                 (format "region:%d:%d" beg end)))
              ((symbol-function 'emacs-shell-command-run-to-string)
               (lambda (command &optional input)
                 (setq seen-command (list command input))
                 "REGION\n"))
              ((symbol-function 'nemacs-gtk--show-text-buffer)
               (lambda (name text)
                 (setq displayed (list name text)))))
      (nemacs-gtk-shell-command-on-region)
      (should nemacs-gtk--minibuffer-active)
      (funcall nemacs-gtk--minibuffer-on-confirm "tr a-z A-Z")
      (should (equal '("tr a-z A-Z" "region:2:8") seen-command))
      (should (equal '("*Shell Command Output*" "REGION\n")
                     displayed))
      (should (equal "shell-command-on-region: 10→7 bytes"
                     nemacs-gtk--last-key-text)))))

(ert-deftest nemacs-gtk-write-file-uses-shared-fileio-helper ()
  (let ((nemacs-gtk--active-buffer-name "*welcome*")
        (nemacs-gtk--last-key-text "")
        written
        synced)
    (get-buffer-create "*welcome*")
    (cl-letf (((symbol-function 'write-file)
               (lambda (path)
                 (setq written (list path (buffer-name)))
                 "/tmp/written.el"))
              ((symbol-function 'nemacs-gtk--sync-window-title)
               (lambda ()
                 (setq synced t))))
      (nemacs-gtk-write-file)
      (should nemacs-gtk--minibuffer-active)
      (funcall nemacs-gtk--minibuffer-on-confirm "/tmp/new.el")
      (should (equal '("/tmp/new.el" "*welcome*") written))
      (should synced)
      (should (equal "Wrote: /tmp/written.el" nemacs-gtk--last-key-text)))))

(ert-deftest nemacs-gtk-undo-uses-shared-direct-api ()
  (let ((nemacs-gtk--active-buffer-name "*welcome*")
        (nemacs-gtk--last-key-text "")
        called)
    (get-buffer-create "*welcome*")
    (cl-letf (((symbol-function 'emacs-undo-undo-direct)
               (lambda (&optional arg)
                 (setq called arg)
                 (list :status 'ok :message "undo: shared"))))
      (nemacs-gtk-undo)
      (should-not called)
      (should (equal "undo: shared" nemacs-gtk--last-key-text)))))

(ert-deftest nemacs-gtk-mouse-yank-primary-uses-shared-direct-api ()
  (let ((nemacs-gtk--active-buffer-name "*welcome*")
        (nemacs-gtk--last-mouse-event '(button 2 4 5))
        (nemacs-gtk--cache-synced-buffer nil)
        (nemacs-gtk--last-key-text "")
        point-called)
    (get-buffer-create "*welcome*")
    (cl-letf (((symbol-function 'nemacs-gtk--cell-to-point)
               (lambda (row col)
                 (should (= 4 row))
                 (should (= 5 col))
                 9))
              ((symbol-function 'emacs-edit-mouse-yank-primary-direct)
               (lambda (point &optional arg)
                 (setq point-called point)
                 (should-not arg)
                 (list :beg nil :end nil :text nil :point point))))
      (nemacs-gtk-mouse-yank-primary)
      (should (= 9 point-called))
      (should (equal "mouse-2 yank @ point 9" nemacs-gtk--last-key-text)))))

(ert-deftest nemacs-gtk-mouse-set-point-uses-shared-goto-helper ()
  (let ((nemacs-gtk--active-buffer-name "*welcome*")
        (nemacs-gtk--last-mouse-event '(press 1 4 5))
        (nemacs-gtk--press-point nil)
        (nemacs-gtk--last-key-text "")
        goto-called
        deactivated)
    (get-buffer-create "*welcome*")
    (cl-letf (((symbol-function 'nemacs-gtk--window-at-cell)
               (lambda (_row _col) nil))
              ((symbol-function 'nemacs-gtk--cell-to-point)
               (lambda (row col)
                 (should (= 4 row))
                 (should (= 5 col))
                 9))
              ((symbol-function 'emacs-edit-goto-position-direct)
               (lambda (point)
                 (setq goto-called (list point (buffer-name)))
                 (list :status 'moved
                       :requested-point point
                       :point point)))
              ((symbol-function 'nemacs-gtk--deactivate-mark)
               (lambda () (setq deactivated t))))
      (nemacs-gtk-mouse-set-point)
      (should (equal '(9 "*welcome*") goto-called))
      (should deactivated)
      (should (= 9 nemacs-gtk--press-point))
      (should (equal "mouse-1 → point 9 (cell 4,5)"
                     nemacs-gtk--last-key-text)))))

(ert-deftest nemacs-gtk-mouse-drag-region-uses-shared-goto-helper ()
  (let ((nemacs-gtk--active-buffer-name "*welcome*")
        (nemacs-gtk--last-mouse-event '(motion 1 4 5))
        (nemacs-gtk--press-point 3)
        (nemacs-gtk--mark-pos nil)
        (nemacs-gtk--mark-buffer nil)
        (nemacs-gtk--shift-region t)
        (nemacs-gtk--last-key-text "")
        goto-called
        plan-called)
    (get-buffer-create "*welcome*")
    (cl-letf (((symbol-function 'nemacs-gtk--cell-to-point)
               (lambda (row col)
                 (should (= 4 row))
                 (should (= 5 col))
                 9))
              ((symbol-function 'emacs-edit-mouse-drag-region-plan)
               (lambda (press point mark-pos mark-buffer active-buffer-name)
                 (setq plan-called
                       (list press point mark-pos mark-buffer
                             active-buffer-name))
                 (list :status 'anchored
                       :mark press
                       :buffer active-buffer-name
                       :shift-region nil
                       :point point
                       :message "drag → 3..9")))
              ((symbol-function 'emacs-edit-goto-position-direct)
               (lambda (point)
                 (setq goto-called (list point (buffer-name)))
                 (list :status 'moved
                       :requested-point point
                       :point point))))
      (nemacs-gtk-mouse-drag-region)
      (should (equal '(3 9 nil nil "*welcome*") plan-called))
      (should (equal '(9 "*welcome*") goto-called))
      (should (= 3 nemacs-gtk--mark-pos))
      (should (equal "*welcome*" nemacs-gtk--mark-buffer))
      (should-not nemacs-gtk--shift-region)
      (should (equal "drag → 3..9" nemacs-gtk--last-key-text)))))

(ert-deftest nemacs-gtk-mouse-point-source-shape-uses-shared-goto-helper ()
  (let ((source
         (with-temp-buffer
           (insert-file-contents (locate-library "nemacs-gtk-frontend.el"))
           (goto-char (point-min))
           (re-search-forward "(defun nemacs-gtk-mouse-set-point" nil t)
           (let ((start (match-beginning 0)))
             (goto-char start)
             (forward-sexp)
             (let ((first (buffer-substring start (point))))
               (re-search-forward "(defun nemacs-gtk-mouse-drag-region" nil t)
               (setq start (match-beginning 0))
               (goto-char start)
               (forward-sexp)
               (concat first "\n" (buffer-substring start (point))))))))
    (should (string-match-p
             (regexp-quote "emacs-edit-goto-position-direct")
             source))
      (should-not
       (string-match-p
        (regexp-quote "nelisp-ec-goto-char")
        source))))

(ert-deftest nemacs-gtk-mouse-select-word-uses-shared-command-result ()
  (let ((nemacs-gtk--active-buffer-name "*welcome*")
        (nemacs-gtk--last-mouse-event '(double 1 4 5))
        (nemacs-gtk--mark-pos nil)
        (nemacs-gtk--mark-buffer nil)
        (nemacs-gtk--shift-region t)
        (nemacs-gtk--last-key-text "")
        called)
    (get-buffer-create "*welcome*")
    (cl-letf (((symbol-function 'nemacs-gtk--cell-to-point)
               (lambda (row col)
                 (should (= 4 row))
                 (should (= 5 col))
                 9))
              ((symbol-function 'emacs-edit-run-select-word-at-command)
               (lambda (point buffer-name)
                 (setq called (list point buffer-name (buffer-name)))
                 (list :status 'selected
                       :mark 4
                       :point 6
                       :buffer buffer-name
                       :shift-region nil
                       :message "Selected word (2 chars)"))))
      (nemacs-gtk-mouse-select-word)
      (should (equal '(9 "*welcome*" "*welcome*") called))
      (should (= 4 nemacs-gtk--mark-pos))
      (should (equal "*welcome*" nemacs-gtk--mark-buffer))
      (should-not nemacs-gtk--shift-region)
      (should (equal "Selected word (2 chars)"
                     nemacs-gtk--last-key-text)))))

(ert-deftest nemacs-gtk-mouse-select-line-uses-shared-command-result ()
  (let ((nemacs-gtk--active-buffer-name "*welcome*")
        (nemacs-gtk--last-mouse-event '(triple 1 4 5))
        (nemacs-gtk--mark-pos nil)
        (nemacs-gtk--mark-buffer nil)
        (nemacs-gtk--shift-region t)
        (nemacs-gtk--last-key-text "")
        called)
    (get-buffer-create "*welcome*")
    (cl-letf (((symbol-function 'nemacs-gtk--cell-to-point)
               (lambda (row col)
                 (should (= 4 row))
                 (should (= 5 col))
                 9))
              ((symbol-function 'emacs-edit-run-select-line-at-command)
               (lambda (point buffer-name)
                 (setq called (list point buffer-name (buffer-name)))
                 (list :status 'selected
                       :mark 4
                       :point 7
                       :buffer buffer-name
                       :shift-region nil
                       :message "Selected line (3 chars)"))))
      (nemacs-gtk-mouse-select-line)
      (should (equal '(9 "*welcome*" "*welcome*") called))
      (should (= 4 nemacs-gtk--mark-pos))
      (should (equal "*welcome*" nemacs-gtk--mark-buffer))
      (should-not nemacs-gtk--shift-region)
      (should (equal "Selected line (3 chars)"
                     nemacs-gtk--last-key-text)))))

(ert-deftest nemacs-gtk-mouse-selection-source-shape-uses-shared-edit-api ()
  (let ((source
         (with-temp-buffer
           (insert-file-contents (locate-library "nemacs-gtk-frontend.el"))
           (buffer-string))))
    (dolist (needle '("emacs-edit-mouse-drag-region-plan"
                      "emacs-edit-run-select-word-at-command"
                      "emacs-edit-run-select-line-at-command"))
      (should (string-match-p (regexp-quote needle) source)))
    (dolist (needle '("emacs-edit-select-word-at-direct"
                      "emacs-edit-select-line-at-direct"))
      (should-not (string-match-p (regexp-quote needle) source)))))

(ert-deftest nemacs-gtk-yank-pop-uses-shared-result-api ()
  (let ((nemacs-gtk--active-buffer-name "*welcome*")
        (nemacs-gtk--last-key-text "")
        result-called
        applied)
    (get-buffer-create "*welcome*")
    (cl-letf (((symbol-function 'emacs-edit-yank-pop-result-direct)
               (lambda (&optional arg)
                 (setq result-called arg)
                 (list :status 'ok
                       :message "yank-pop"
                       :beg 1
                       :end 2
                       :text "x"
                       :replacement t
                       :delete-len 1
                       :delete-text "y")))
              ((symbol-function 'nemacs-gtk--apply-edit-result-cache)
               (lambda (result)
                 (setq applied result))))
      (nemacs-gtk-yank-pop)
      (should (= 1 result-called))
      (should (equal "yank-pop" nemacs-gtk--last-key-text))
      (should (equal "x" (plist-get applied :text))))))

(ert-deftest nemacs-gtk-yank-pop-error-skips-cache-apply ()
  (let ((nemacs-gtk--active-buffer-name "*welcome*")
        (nemacs-gtk--last-key-text "")
        applied)
    (get-buffer-create "*welcome*")
    (cl-letf (((symbol-function 'emacs-edit-yank-pop-result-direct)
               (lambda (&optional _arg)
                 (list :status 'error
                       :message "yank-pop: Previous command was not a yank")))
              ((symbol-function 'nemacs-gtk--apply-edit-result-cache)
               (lambda (_result)
                 (setq applied t))))
      (nemacs-gtk-yank-pop)
      (should-not applied)
      (should (equal "yank-pop: Previous command was not a yank"
                     nemacs-gtk--last-key-text)))))

(ert-deftest nemacs-gtk-forward-sentence-uses-shared-direct-api ()
  (let ((nemacs-gtk--active-buffer-name "*welcome*")
        (nemacs-gtk--last-key-text "")
        called
        cursor-visible)
    (get-buffer-create "*welcome*")
    (cl-letf (((symbol-function 'emacs-edit-forward-sentence-direct)
               (lambda (&optional arg)
                 (setq called arg)
                 (list :old-point 1 :point 5 :status 'moved)))
              ((symbol-function 'nemacs-gtk--ensure-cursor-visible)
               (lambda ()
                 (setq cursor-visible t))))
      (nemacs-gtk-forward-sentence)
      (should-not called)
      (should cursor-visible)
      (should (equal "forward-sentence -> 5"
                     nemacs-gtk--last-key-text)))))

(ert-deftest nemacs-gtk-forward-sentence-eob-uses-shared-status ()
  (let ((nemacs-gtk--active-buffer-name "*welcome*")
        (nemacs-gtk--last-key-text ""))
    (get-buffer-create "*welcome*")
    (cl-letf (((symbol-function 'emacs-edit-forward-sentence-direct)
               (lambda (&optional _arg)
                 (list :old-point 1 :point 3 :status 'eob)))
              ((symbol-function 'nemacs-gtk--ensure-cursor-visible)
               #'ignore))
      (nemacs-gtk-forward-sentence)
      (should (equal "forward-sentence -> EOB"
                     nemacs-gtk--last-key-text)))))

(ert-deftest nemacs-gtk-backward-sentence-uses-shared-direct-api ()
  (let ((nemacs-gtk--active-buffer-name "*welcome*")
        (nemacs-gtk--last-key-text "")
        called
        cursor-visible)
    (get-buffer-create "*welcome*")
    (cl-letf (((symbol-function 'emacs-edit-backward-sentence-direct)
               (lambda (&optional arg)
                 (setq called arg)
                 (list :old-point 10 :point 5 :status 'moved)))
              ((symbol-function 'nemacs-gtk--ensure-cursor-visible)
               (lambda ()
                 (setq cursor-visible t))))
      (nemacs-gtk-backward-sentence)
      (should-not called)
      (should cursor-visible)
      (should (equal "backward-sentence -> 5"
                     nemacs-gtk--last-key-text)))))

(ert-deftest nemacs-gtk-sentence-and-sexp-source-shape-uses-shared-edit ()
  (let ((source
         (with-temp-buffer
           (insert-file-contents (locate-library "nemacs-gtk-frontend.el"))
           (buffer-string))))
    (dolist (needle '("emacs-edit-forward-sentence-direct"
                      "emacs-edit-backward-sentence-direct"
                      "emacs-edit-matching-paren-position-direct"))
      (should (string-match-p (regexp-quote needle) source)))
    (dolist (needle '("(defun nemacs-gtk--scan-sexp-forward"
                      "(defun nemacs-gtk--scan-sexp-backward"
                      "(defun nemacs-gtk--sexp-symbol-char-p"
                      "(defun nemacs-gtk--sentence-end-char-p"))
      (should-not (string-match-p (regexp-quote needle) source)))))

(ert-deftest nemacs-gtk-paren-match-pos-uses-shared-edit-helper ()
  (let ((nemacs-gtk--active-buffer-name "*welcome*")
        called-buffer)
    (get-buffer-create "*welcome*")
    (cl-letf (((symbol-function 'emacs-edit-matching-paren-position-direct)
               (lambda ()
                 (setq called-buffer (buffer-name))
                 7)))
      (should (equal 7 (nemacs-gtk--paren-match-pos)))
      (should (equal "*welcome*" called-buffer)))))

(ert-deftest nemacs-gtk-paren-match-source-shape-uses-shared-edit-helper ()
  (let ((source
         (with-temp-buffer
           (insert-file-contents (locate-library "nemacs-gtk-frontend.el"))
           (goto-char (point-min))
           (re-search-forward "(defun nemacs-gtk--paren-match-pos" nil t)
           (let ((start (match-beginning 0)))
             (goto-char start)
             (forward-sexp)
             (buffer-substring start (point))))))
    (should (string-match-p
             (regexp-quote "emacs-edit-matching-paren-position-direct")
             source))
    (dolist (needle '("nelisp-ec-goto-char"
                      "emacs-edit-scan-sexp-forward"
                      "emacs-edit-scan-sexp-backward"))
      (should-not (string-match-p (regexp-quote needle) source)))))

(ert-deftest nemacs-gtk-mark-defun-uses-shared-direct-api ()
  (let ((nemacs-gtk--active-buffer-name "*welcome*")
        (nemacs-gtk--mark-pos nil)
        (nemacs-gtk--mark-buffer nil)
        (nemacs-gtk--last-key-text "")
        called
        cursor-visible)
    (get-buffer-create "*welcome*")
    (cl-letf (((symbol-function 'emacs-edit-mark-defun-direct)
               (lambda ()
                 (setq called t)
                 (list :beg 2 :end 9 :point 2 :status 'marked)))
              ((symbol-function 'nemacs-gtk--ensure-cursor-visible)
               (lambda ()
                 (setq cursor-visible t))))
      (nemacs-gtk-mark-defun)
      (should called)
      (should cursor-visible)
      (should (equal 9 nemacs-gtk--mark-pos))
      (should (eq (get-buffer "*welcome*") nemacs-gtk--mark-buffer))
      (should (equal "mark-defun: 2..9"
                     nemacs-gtk--last-key-text)))))

(ert-deftest nemacs-gtk-mark-defun-reports-shared-error ()
  (let ((nemacs-gtk--active-buffer-name "*welcome*")
        (nemacs-gtk--last-key-text ""))
    (get-buffer-create "*welcome*")
    (cl-letf (((symbol-function 'emacs-edit-mark-defun-direct)
               (lambda ()
                 (list :beg nil :end nil :status 'no-top-level))))
      (nemacs-gtk-mark-defun)
      (should (equal "mark-defun: no top-level form"
                     nemacs-gtk--last-key-text)))))

(ert-deftest nemacs-gtk-narrow-to-defun-uses-shared-direct-api ()
  (let ((nemacs-gtk--active-buffer-name "*welcome*")
        (nemacs-gtk--last-key-text "")
        called)
    (get-buffer-create "*welcome*")
    (cl-letf (((symbol-function 'emacs-edit-narrow-to-defun-direct)
               (lambda ()
                 (setq called t)
                 (list :beg 2 :end 9 :point 9 :status 'narrowed))))
      (nemacs-gtk-narrow-to-defun)
      (should called)
      (should (equal "narrow-to-defun: 2..9"
                     nemacs-gtk--last-key-text)))))

(ert-deftest nemacs-gtk-defun-source-shape-uses-shared-edit ()
  (let ((source
         (with-temp-buffer
           (insert-file-contents (locate-library "nemacs-gtk-frontend.el"))
           (buffer-string))))
    (dolist (needle '("emacs-edit-mark-defun-direct"
                      "emacs-edit-narrow-to-defun-direct"))
      (should (string-match-p (regexp-quote needle) source)))
    (should-not
     (string-match-p
      (regexp-quote "(defun nemacs-gtk--beginning-of-defun")
      source))))

(ert-deftest nemacs-gtk-comment-dwim-uses-shared-direct-api ()
  (let ((nemacs-gtk--active-buffer-name "*welcome*")
        (nemacs-gtk--last-key-text "")
        called-bounds
        applied)
    (get-buffer-create "*welcome*")
    (cl-letf (((symbol-function 'nemacs-gtk--region-bounds)
               (lambda ()
                 '(2 . 5)))
              ((symbol-function 'emacs-edit-comment-dwim-direct)
               (lambda (&optional bounds)
                 (setq called-bounds bounds)
                 (list :status 'region
                       :line-count 2
                       :edits (list (list :beg 3 :end 6 :text ";; ")
                                    (list :beg 1 :end 4 :text ";; ")))))
              ((symbol-function 'nemacs-gtk--apply-edit-result-cache)
               (lambda (edit)
                 (push edit applied))))
      (nemacs-gtk-comment-dwim)
      (should (equal '(2 . 5) called-bounds))
      (should (equal 2 (length applied)))
      (should (equal "comment-dwim region" nemacs-gtk--last-key-text)))))

(ert-deftest nemacs-gtk-comment-source-shape-uses-shared-edit ()
  (let ((source
         (with-temp-buffer
           (insert-file-contents (locate-library "nemacs-gtk-frontend.el"))
           (buffer-string))))
    (should (string-match-p
             (regexp-quote "emacs-edit-comment-dwim-direct")
             source))
    (dolist (needle '("(defun nemacs-gtk--line-bounds-around-point"
                      "(defun nemacs-gtk--line-already-commented-p"
                      "(defun nemacs-gtk--toggle-line-comment"))
      (should-not (string-match-p (regexp-quote needle) source)))))

(ert-deftest nemacs-gtk-transform-region-uses-shared-command-helper ()
  (let ((nemacs-gtk--active-buffer-name "*welcome*")
        (nemacs-gtk--mark-pos 2)
        (nemacs-gtk--mark-buffer "*welcome*")
        (nemacs-gtk--last-key-text "")
        helper-args
        applied)
    (get-buffer-create "*welcome*")
    (cl-letf (((symbol-function 'emacs-edit-run-transform-region-command)
               (lambda (mark-pos mark-buffer active-buffer-name transform label)
                 (setq helper-args
                       (list mark-pos mark-buffer active-buffer-name
                             (funcall transform "ab") label))
                 (list :status 'transformed
                       :message "upcase-region: 2 chars"
                       :edit (list :beg 2 :end 4 :text "AB"))))
              ((symbol-function 'nemacs-gtk--apply-edit-result-cache)
               (lambda (edit)
                 (setq applied edit))))
      (nemacs-gtk-upcase-region)
      (should (equal '(2 "*welcome*" "*welcome*" "AB" "upcase-region")
                     helper-args))
      (should (equal "AB" (plist-get applied :text)))
      (should (equal "upcase-region: 2 chars"
                     nemacs-gtk--last-key-text)))))

(ert-deftest nemacs-gtk-transform-region-source-shape-uses-shared-helper ()
  (let ((source
         (with-temp-buffer
           (insert-file-contents (locate-library "nemacs-gtk-frontend.el"))
           (buffer-string))))
    (should (string-match-p
             (regexp-quote "emacs-edit-run-transform-region-command")
             source))
    (dolist (needle '("(defun nemacs-gtk--region-bounds-or-error"
                      "(defun nemacs-gtk--transform-region"))
      (should-not (string-match-p (regexp-quote needle) source)))))

(ert-deftest nemacs-gtk-fill-paragraph-uses-shared-command-helper ()
  (let ((nemacs-gtk--active-buffer-name "*welcome*")
        (nemacs-gtk--fill-column 42)
        (nemacs-gtk--last-key-text "")
        called-column
        applied)
    (get-buffer-create "*welcome*")
    (cl-letf (((symbol-function 'emacs-edit-run-fill-paragraph-command)
               (lambda (column)
                 (setq called-column column)
                 (list :status 'filled
                       :message "fill-paragraph: 3→2 chars"
                       :edit (list :beg 1 :end 3 :text "ab"))))
              ((symbol-function 'nemacs-gtk--apply-edit-result-cache)
               (lambda (edit)
                 (setq applied edit))))
      (nemacs-gtk-fill-paragraph)
      (should (equal 42 called-column))
      (should (equal "ab" (plist-get applied :text)))
      (should (equal "fill-paragraph: 3→2 chars"
                     nemacs-gtk--last-key-text)))))

(ert-deftest nemacs-gtk-mark-paragraph-uses-shared-command-helper ()
  (let ((nemacs-gtk--active-buffer-name "*welcome*")
        (nemacs-gtk--mark-pos nil)
        (nemacs-gtk--mark-buffer nil)
        (nemacs-gtk--shift-region t)
        (nemacs-gtk--last-key-text "")
        called-buffer)
    (get-buffer-create "*welcome*")
    (cl-letf (((symbol-function 'emacs-edit-run-mark-paragraph-command)
               (lambda (buffer-name)
                 (setq called-buffer buffer-name)
                 (list :status 'marked
                       :mark 2
                       :point 9
                       :buffer buffer-name
                       :shift-region nil
                       :message "Mark paragraph"))))
      (nemacs-gtk-mark-paragraph)
      (should (equal "*welcome*" called-buffer))
      (should (equal 2 nemacs-gtk--mark-pos))
      (should (equal "*welcome*" nemacs-gtk--mark-buffer))
      (should-not nemacs-gtk--shift-region)
      (should (equal "Mark paragraph" nemacs-gtk--last-key-text)))))

(ert-deftest nemacs-gtk-paragraph-source-shape-uses-shared-command-helper ()
  (let ((source
         (with-temp-buffer
           (insert-file-contents (locate-library "nemacs-gtk-frontend.el"))
           (buffer-string))))
    (dolist (needle '("emacs-edit-run-fill-paragraph-command"
                      "emacs-edit-run-mark-paragraph-command"))
      (should (string-match-p (regexp-quote needle) source)))
    (dolist (needle '("emacs-edit-fill-paragraph-direct"
                      "emacs-edit-mark-paragraph-direct"))
      (should-not (string-match-p (regexp-quote needle) source)))))

(ert-deftest nemacs-gtk-electric-pair-uses-shared-command-helper ()
  (let ((nemacs-gtk--active-buffer-name "*welcome*")
        (nemacs-gtk--last-key-text "")
        called-char
        applied)
    (get-buffer-create "*welcome*")
    (cl-letf (((symbol-function 'emacs-edit-run-electric-pair-command)
               (lambda (char &optional open-pairs close-set)
                 (setq called-char char)
                 (should-not open-pairs)
                 (should-not close-set)
                 (list :status 'paired
                       :message "electric-pair: ()"
                       :edit (list :beg 2 :end 4 :text "()"))))
              ((symbol-function 'nemacs-gtk--apply-edit-result-cache)
               (lambda (edit)
                 (setq applied edit))))
      (nemacs-gtk--electric-pair-handle ?\()
      (should (equal ?\( called-char))
      (should (equal "()" (plist-get applied :text)))
      (should (equal "electric-pair: ()" nemacs-gtk--last-key-text)))))

(ert-deftest nemacs-gtk-electric-pair-source-shape-uses-shared-helper ()
  (let ((source
         (with-temp-buffer
           (insert-file-contents (locate-library "nemacs-gtk-frontend.el"))
           (buffer-string))))
    (should (string-match-p
             (regexp-quote "emacs-edit-run-electric-pair-command")
             source))
    (should (string-match-p
             (regexp-quote "emacs-edit-electric-pair-default-open-pairs")
             source))
    (should (string-match-p
             (regexp-quote "emacs-edit-electric-pair-default-close-set")
             source))
    (dolist (needle '("emacs-edit-electric-pair-direct"
                      "nemacs-gtk--electric-open-pairs"
                      "nemacs-gtk--electric-close-set"))
      (should-not (string-match-p (regexp-quote needle) source)))))

(ert-deftest nemacs-gtk-quoted-insert-uses-shared-command-helper ()
  (let ((nemacs-gtk--active-buffer-name "*welcome*")
        (nemacs-gtk--quoted-insert-pending t)
        (nemacs-gtk--minibuffer-active nil)
        (nemacs-gtk--isearch-active nil)
        (nemacs-gtk--query-replace-pending-key nil)
        (nemacs-gtk--describe-key-pending nil)
        (nemacs-gtk--register-pending-op nil)
        (nemacs-gtk--electric-pair-mode nil)
        (nemacs-gtk--pending-prefix nil)
        (nemacs-gtk--last-key-text "")
        called-char
        applied
        cursor-visible)
    (get-buffer-create "*welcome*")
    (cl-letf (((symbol-function 'emacs-edit-run-quoted-insert-command)
               (lambda (char)
                 (setq called-char char)
                 (list :status 'inserted
                       :message "quoted-insert: x (#120)"
                       :edit (list :beg 1 :end 2 :text "x"))))
              ((symbol-function 'nemacs-gtk--apply-edit-result-cache)
               (lambda (edit)
                 (setq applied edit)))
              ((symbol-function 'nemacs-gtk--ensure-cursor-visible)
               (lambda ()
                 (setq cursor-visible t))))
      (nemacs-gtk--dispatch-key ?x 0 ?x)
      (should-not nemacs-gtk--quoted-insert-pending)
      (should (equal ?x called-char))
      (should (equal "x" (plist-get applied :text)))
      (should cursor-visible)
      (should (equal "quoted-insert: x (#120)"
                     nemacs-gtk--last-key-text)))))

(ert-deftest nemacs-gtk-quoted-insert-source-shape-uses-shared-helper ()
  (let ((source
         (with-temp-buffer
           (insert-file-contents (locate-library "nemacs-gtk-frontend.el"))
           (buffer-string))))
    (should (string-match-p
             (regexp-quote "emacs-edit-run-quoted-insert-command")
             source))
    (should-not
     (string-match-p
      (regexp-quote "(nelisp-ec-insert (string ch))")
      source))))

(ert-deftest nemacs-gtk-handle-menu-action-find-file-dispatches ()
  (let (called)
    (cl-letf (((symbol-function 'call-interactively)
               (lambda (command &optional _record-flag _keys)
                 (setq called command)
                 :ok)))
      (let ((result (nemacs-gtk--handle-menu-action "find-file")))
        (should (equal "find-file" (plist-get result :action)))
        (should (eq #'find-file (plist-get result :command)))
        (should (eq :ok (plist-get result :value))))
      (should (eq #'find-file called)))))

(ert-deftest nemacs-gtk-keyboard-find-file-uses-shared-open-helper ()
  (let ((nemacs-gtk--active-buffer-name "*welcome*")
        (nemacs-gtk--scroll-offset 9)
        (nemacs-gtk--last-key-text "")
        synced
        applied)
    (get-buffer-create "*welcome*")
    (let ((buffer (get-buffer-create "note.el")))
      (cl-letf (((symbol-function 'nelisp-gtk-show-open-dialog)
                 (lambda (title)
                   (should (equal "Open File" title))
                   "/tmp/note.el"))
                ((symbol-function 'find-file-noselect)
                 (lambda (path)
                   (should (equal "/tmp/note.el" path))
                   buffer))
                ((symbol-function 'nemacs-gtk--apply-mode-for-buffer)
                 (lambda (name)
                   (setq applied name)))
                ((symbol-function 'nemacs-gtk--sync-window-title)
                 (lambda ()
                   (setq synced t)))
                ((symbol-function 'nemacs-gtk--buffer-mode)
                 (lambda (name)
                   (should (equal "note.el" name))
                   'emacs-lisp-mode)))
        (nemacs-gtk-keyboard-find-file)
        (should (equal "note.el" nemacs-gtk--active-buffer-name))
        (should (= 0 nemacs-gtk--scroll-offset))
        (should (equal "note.el" applied))
        (should synced)
        (should (equal "Opened: /tmp/note.el [emacs-lisp-mode]"
                       nemacs-gtk--last-key-text))))))

(ert-deftest nemacs-gtk-keyboard-find-file-cancel-uses-shared-open-helper ()
  (let ((nemacs-gtk--active-buffer-name "*welcome*")
        (nemacs-gtk--scroll-offset 9)
        (nemacs-gtk--last-key-text ""))
    (cl-letf (((symbol-function 'nelisp-gtk-show-open-dialog)
               (lambda (_title) nil)))
      (nemacs-gtk-keyboard-find-file)
      (should (equal "*welcome*" nemacs-gtk--active-buffer-name))
      (should (= 9 nemacs-gtk--scroll-offset))
      (should (equal "Open: cancelled" nemacs-gtk--last-key-text)))))

(ert-deftest nemacs-gtk-handle-menu-action-undo-dispatches ()
  (let (called)
    (cl-letf (((symbol-function 'call-interactively)
               (lambda (command &optional _record-flag _keys)
                 (setq called command)
                 :ok)))
      (let ((result (nemacs-gtk--handle-menu-action "undo")))
        (should (equal "undo" (plist-get result :action)))
        (should (eq #'undo (plist-get result :command)))
        (should (eq :ok (plist-get result :value))))
      (should (eq #'undo called)))))

(ert-deftest nemacs-gtk-handle-menu-action-help-uses-adapter ()
  (let ((nemacs-gtk--active-buffer-name "*welcome*")
        (nemacs-gtk--scroll-offset 9)
        (nemacs-gtk--last-key-text ""))
    (cl-letf (((symbol-function 'nemacs-gtk--sync-window-title)
               (lambda () nil)))
      (nemacs-gtk--handle-menu-action "describe-function")
      (should nemacs-gtk--minibuffer-active)
      (should (equal "Describe function: " nemacs-gtk--minibuffer-prompt))
      (funcall nemacs-gtk--minibuffer-on-confirm "forward-char")
      (should (equal "*Help*" nemacs-gtk--active-buffer-name))
      (let ((text (nelisp-ec-with-current-buffer
                      (cdr (assoc "*Help*" nelisp-ec--buffers))
                    (nelisp-ec-buffer-string))))
        (should (string-match-p "forward-char is a function" text))))))

(ert-deftest nemacs-gtk-handle-menu-action-help-bindings-uses-adapter ()
  (let ((nemacs-gtk--active-buffer-name "*welcome*")
        (nemacs-gtk--scroll-offset 9)
        (nemacs-gtk--last-key-text ""))
    (cl-letf (((symbol-function 'nemacs-gtk--sync-window-title)
               (lambda () nil)))
      (nemacs-gtk--handle-menu-action "describe-bindings")
      (should (equal "*Bindings*" nemacs-gtk--active-buffer-name))
      (let ((text (nelisp-ec-with-current-buffer
                      (cdr (assoc "*Bindings*" nelisp-ec--buffers))
                    (nelisp-ec-buffer-string))))
        (should (string-match-p "C-h b[ \t]+nemacs-gtk-describe-bindings"
                                text))))))

(ert-deftest nemacs-gtk-handle-menu-action-describe-key-uses-gtk-state ()
  (let ((nemacs-gtk--describe-key-pending nil)
        (nemacs-gtk--last-key-text ""))
    (nemacs-gtk--handle-menu-action "describe-key")
    (should nemacs-gtk--describe-key-pending)
    (should (equal "Describe key (press a key)..."
                   nemacs-gtk--last-key-text))))

(ert-deftest nemacs-gtk-dispatch-describe-key-uses-shared-summary ()
  (let ((nemacs-gtk--active-buffer-name "*welcome*")
        (nemacs-gtk--describe-key-pending t)
        (nemacs-gtk--minibuffer-active nil)
        (nemacs-gtk--isearch-active nil)
        (nemacs-gtk--query-replace-pending-key nil)
        (nemacs-gtk--register-pending-op nil)
        (nemacs-gtk--quoted-insert-pending nil)
        (nemacs-gtk--electric-pair-mode nil)
        (nemacs-gtk--pending-prefix nil)
        (nemacs-gtk--last-key-text ""))
    (get-buffer-create "*welcome*")
    (cl-letf (((symbol-function 'nemacs-gtk--lookup-key-vec)
               (lambda (vector)
                 (should (equal [6] vector))
                 'forward-char)))
      (nemacs-gtk--dispatch-key ?f nemacs-gtk--gdk-control-mask 0)
      (should-not nemacs-gtk--describe-key-pending)
      (should (equal "C-f runs forward-char" nemacs-gtk--last-key-text)))))

(ert-deftest nemacs-gtk-handle-menu-action-help-apropos-uses-adapter ()
  (let ((nemacs-gtk--active-buffer-name "*welcome*")
        (nemacs-gtk--scroll-offset 9)
        (nemacs-gtk--last-key-text ""))
    (cl-letf (((symbol-function 'nemacs-gtk--sync-window-title)
               (lambda () nil)))
      (nemacs-gtk--handle-menu-action "apropos")
      (should nemacs-gtk--minibuffer-active)
      (should (equal "Apropos command: " nemacs-gtk--minibuffer-prompt))
      (funcall nemacs-gtk--minibuffer-on-confirm "describe")
      (should (equal "*Apropos*" nemacs-gtk--active-buffer-name))
      (let ((text (nelisp-ec-with-current-buffer
                      (cdr (assoc "*Apropos*" nelisp-ec--buffers))
                    (nelisp-ec-buffer-string))))
        (should (string-match-p "Apropos: describe" text))))))

(ert-deftest nemacs-gtk-execute-extended-command-prefers-prefixed-command ()
  (let ((nemacs-gtk--active-buffer-name "*welcome*")
        (nemacs-gtk--last-key-text "")
        called)
    (get-buffer-create "*welcome*")
    (cl-letf (((symbol-function 'nemacs-gtk-save-buffer)
               (lambda ()
                 (interactive)
                 (setq called 'nemacs-gtk-save-buffer)))
              ((symbol-function 'save-buffer)
               (lambda ()
                 (interactive)
                 (setq called 'save-buffer))))
      (execute-extended-command)
      (should nemacs-gtk--minibuffer-active)
      (funcall nemacs-gtk--minibuffer-on-confirm "save-buffer")
      (should (eq 'nemacs-gtk-save-buffer called))
      (should (equal "M-x save-buffer ✓" nemacs-gtk--last-key-text)))))

(ert-deftest nemacs-gtk-repeat-uses-shared-command-loop-helper ()
  (let ((emacs-command-loop--last-command 'forward-char)
        (nemacs-gtk--last-key-text "")
        called)
    (cl-letf (((symbol-function 'call-interactively)
               (lambda (command &optional _record-flag _keys)
                 (setq called command)
                 :called)))
      (nemacs-gtk-repeat)
      (should (eq 'forward-char called))
      (should (equal "repeat: forward-char" nemacs-gtk--last-key-text)))))

(ert-deftest nemacs-gtk-repeat-reports-empty-and-unbound ()
  (let ((emacs-command-loop--last-command nil)
        (nemacs-gtk--last-key-text ""))
    (nemacs-gtk-repeat)
    (should (equal "repeat: nothing to repeat" nemacs-gtk--last-key-text)))
  (let ((emacs-command-loop--last-command 'nemacs-gtk-repeat)
        (nemacs-gtk--last-key-text ""))
    (nemacs-gtk-repeat)
    (should (equal "repeat: nothing to repeat" nemacs-gtk--last-key-text)))
  (let ((emacs-command-loop--last-command 'missing-command)
        (nemacs-gtk--last-key-text ""))
    (nemacs-gtk-repeat)
    (should (equal "repeat: missing-command not fboundp"
                   nemacs-gtk--last-key-text))))

(ert-deftest nemacs-gtk-toggle-read-only-uses-buffer-helper ()
  (let ((nemacs-gtk--active-buffer-name "*welcome*")
        (nemacs-gtk--last-key-text ""))
    (get-buffer-create "*welcome*")
    (with-current-buffer "*welcome*"
      (setq buffer-read-only nil))
    (nemacs-gtk-toggle-read-only)
    (should (equal "buffer-read-only: on" nemacs-gtk--last-key-text))
    (with-current-buffer "*welcome*"
      (should buffer-read-only))
    (nemacs-gtk-toggle-read-only)
    (should (equal "buffer-read-only: off" nemacs-gtk--last-key-text))
    (with-current-buffer "*welcome*"
      (should-not buffer-read-only))))

(ert-deftest nemacs-gtk-help-adapter-describe-function ()
  (let ((nemacs-gtk--active-buffer-name "*welcome*")
        (nemacs-gtk--scroll-offset 9)
        (nemacs-gtk--last-key-text ""))
    (cl-letf (((symbol-function 'nemacs-gtk--sync-window-title)
               (lambda () nil)))
      (nemacs-gtk-describe-function)
      (should nemacs-gtk--minibuffer-active)
      (should (equal "Describe function: " nemacs-gtk--minibuffer-prompt))
      (funcall nemacs-gtk--minibuffer-on-confirm "forward-char")
      (should (equal "*Help*" nemacs-gtk--active-buffer-name))
      (should (= 0 nemacs-gtk--scroll-offset))
      (let ((text (nelisp-ec-with-current-buffer
                      (cdr (assoc "*Help*" nelisp-ec--buffers))
                    (nelisp-ec-buffer-string))))
        (should (string-match-p "forward-char is a function" text))))))

(ert-deftest nemacs-gtk-help-adapter-apropos ()
  (let ((nemacs-gtk--active-buffer-name "*welcome*")
        (nemacs-gtk--scroll-offset 9)
        (nemacs-gtk--last-key-text ""))
    (cl-letf (((symbol-function 'nemacs-gtk--sync-window-title)
               (lambda () nil)))
      (nemacs-gtk-apropos)
      (should nemacs-gtk--minibuffer-active)
      (should (equal "Apropos command: " nemacs-gtk--minibuffer-prompt))
      (funcall nemacs-gtk--minibuffer-on-confirm "describe")
      (should (equal "*Apropos*" nemacs-gtk--active-buffer-name))
      (should (= 0 nemacs-gtk--scroll-offset))
      (let ((text (nelisp-ec-with-current-buffer
                      (cdr (assoc "*Apropos*" nelisp-ec--buffers))
                    (nelisp-ec-buffer-string))))
        (should (string-match-p "Apropos: describe" text))
        (should (string-match-p "describe-function" text))))))

(ert-deftest nemacs-gtk-help-adapter-describe-bindings ()
  (let ((nemacs-gtk--active-buffer-name "*welcome*")
        (nemacs-gtk--scroll-offset 9)
        (nemacs-gtk--last-key-text ""))
    (cl-letf (((symbol-function 'nemacs-gtk--sync-window-title)
               (lambda () nil)))
      (nemacs-gtk-describe-bindings)
      (should (equal "*Bindings*" nemacs-gtk--active-buffer-name))
      (should (= 0 nemacs-gtk--scroll-offset))
      (let ((text (nelisp-ec-with-current-buffer
                      (cdr (assoc "*Bindings*" nelisp-ec--buffers))
                    (nelisp-ec-buffer-string))))
        (should (string-match-p "Key bindings in the current GUI runtime"
                                text))
        (should (string-match-p "C-h b[ \t]+nemacs-gtk-describe-bindings"
                                text))
        (should (string-match-p "M-x describe-function[ \t]+describe-function"
                                text))))))

(ert-deftest nemacs-gtk-apply-edit-result-cache-insert-bumps-length ()
  (let ((nemacs-gtk--active-buffer-name "buf")
        (nemacs-gtk--line-count-cache '("buf" 10 2))
        (nemacs-gtk--cache-synced-buffer t)
        calls)
    (cl-letf (((symbol-function 'nelisp-gtk-buffer-edit)
               (lambda (&rest args)
                 (push args calls))))
      (nemacs-gtk--apply-edit-result-cache
       '(:beg 3 :end 4 :text "x" :overwrote nil))
      (should (equal '((3 0 "x")) (nreverse calls)))
      (should (equal '("buf" 11 2) nemacs-gtk--line-count-cache)))))

(ert-deftest nemacs-gtk-apply-edit-result-cache-overwrite-keeps-length ()
  (let ((nemacs-gtk--active-buffer-name "buf")
        (nemacs-gtk--line-count-cache '("buf" 10 2))
        (nemacs-gtk--cache-synced-buffer t)
        calls)
    (cl-letf (((symbol-function 'nelisp-gtk-buffer-edit)
               (lambda (&rest args)
                 (push args calls))))
      (nemacs-gtk--apply-edit-result-cache
       '(:beg 3 :end 4 :text "x" :overwrote t))
      (should (equal '((3 1 "x")) (nreverse calls)))
      (should (equal '("buf" 10 2) nemacs-gtk--line-count-cache)))))

(ert-deftest nemacs-gtk-apply-edit-result-cache-delete-bumps-length ()
  (let ((nemacs-gtk--active-buffer-name "buf")
        (nemacs-gtk--line-count-cache '("buf" 10 2))
        (nemacs-gtk--cache-synced-buffer t)
        calls)
    (cl-letf (((symbol-function 'nelisp-gtk-buffer-edit)
               (lambda (&rest args)
                 (push args calls))))
      (nemacs-gtk--apply-edit-result-cache
       '(:beg 3 :end 4 :text "x" :delete-len 1 :delete-text "x"
         :deleted-newline nil))
      (should (equal '((3 1 "")) (nreverse calls)))
      (should (equal '("buf" 9 2) nemacs-gtk--line-count-cache)))))

(ert-deftest nemacs-gtk-apply-edit-result-cache-newline-delete-invalidates ()
  (let ((nemacs-gtk--active-buffer-name "buf")
        (nemacs-gtk--line-count-cache '("buf" 10 2))
        (nemacs-gtk--cache-synced-buffer t)
        calls)
    (cl-letf (((symbol-function 'nelisp-gtk-buffer-edit)
               (lambda (&rest args)
                 (push args calls))))
      (nemacs-gtk--apply-edit-result-cache
       '(:beg 3 :end 4 :text "\n" :delete-len 1 :delete-text "\n"
         :deleted-newline t))
      (should (equal '((3 1 "")) (nreverse calls)))
      (should (null nemacs-gtk--line-count-cache)))))

(ert-deftest nemacs-gtk-apply-edit-result-cache-replacement-uses-delete-len ()
  (let ((nemacs-gtk--active-buffer-name "buf")
        (nemacs-gtk--line-count-cache '("buf" 10 2))
        (nemacs-gtk--cache-synced-buffer t)
        calls)
    (cl-letf (((symbol-function 'nelisp-gtk-buffer-edit)
               (lambda (&rest args)
                 (push args calls))))
      (nemacs-gtk--apply-edit-result-cache
       '(:beg 3 :end 5 :text "zz" :replacement t
         :delete-len 5 :delete-text "alpha"))
      (should (equal '((3 5 "zz")) (nreverse calls)))
      (should (equal '("buf" 7 2) nemacs-gtk--line-count-cache)))))

(provide 'nemacs-gtk-frontend-menu-test)

;;; nemacs-gtk-frontend-menu-test.el ends here
