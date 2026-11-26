;;; nemacs-user-snapshot.el --- headless package preload for a user dump -*- lexical-binding: t; -*-

;; These dynamically scoped observations exist only in the disposable baker.
(defvar nemacs-user-snapshot--reads)
(defvar nemacs-user-snapshot--lookups)
(defvar nemacs-user-snapshot--directories)
(defvar nemacs-user-snapshot--environment)
(defvar nemacs-user-snapshot--refused)
(defvar nemacs-user-snapshot--declaring-library)
(defvar nemacs-user-snapshot--rearm-show-paren nil)

(defun nemacs-user-snapshot--local-name (file)
  (when (or (file-remote-p file)
            (string-match-p "\\`/[^/]+:" file))
    (error "User snapshot requires a local path: %s" file))
  (let ((file-name-handler-alist nil))
    (expand-file-name file)))

(defun nemacs-user-snapshot--read (original path &optional literal)
  (let* ((name (nemacs-user-snapshot--local-name path))
         (file-name-handler-alist nil)
         ;; Hash the on-disk representation, including a compressed suffix.
         (bytes (nelisp--syscall-read-file name t))
         (digest (and bytes (secure-hash 'sha256 bytes)))
         (result (funcall original path literal)))
    (when result
      (unless digest (error "Could not fingerprint loaded file: %s" name))
      (when (and literal (not (string-suffix-p ".gz" name))
                 (not (equal digest (secure-hash 'sha256 result))))
        (error "File changed during preload: %s" name))
      (push (vector name digest) nemacs-user-snapshot--reads))
    result))

(defun nemacs-user-snapshot--observe-directory (file)
  (let ((directory (file-name-directory file)))
    (when (eq (gethash directory nemacs-user-snapshot--directories :unseen) :unseen)
      (let* ((file-name-handler-alist nil)
             (attributes (file-attributes directory)))
        (puthash directory
                 (if attributes
                     (vector (vconcat (file-attribute-modification-time attributes))
                             (vconcat (file-attribute-status-change-time attributes))
                             (vconcat (file-attribute-file-identifier attributes)))
                   :null)
                 nemacs-user-snapshot--directories)))))

(defun nemacs-user-snapshot--find (original file suffixes path newer &optional predicate)
  (let* ((absolute (file-name-absolute-p file))
         (directories
          (if absolute
              (list (nemacs-user-snapshot--local-name file))
            (mapcar (lambda (directory)
                      (nemacs-user-snapshot--local-name
                       (if (and (stringp directory) (not (equal directory "")))
                           directory default-directory)))
                    (or path '(nil))))))
    ;; Capture effective search paths, including failed lookups. Stamping the
    ;; parent directories here also detects changes during the bake itself.
    (dolist (directory directories)
      (nemacs-user-snapshot--observe-directory
       (if absolute directory (expand-file-name file directory))))
    (push (vector file (vconcat (or suffixes '("")))
                  (vconcat directories) (if absolute t :false))
          nemacs-user-snapshot--lookups)
    (funcall original file suffixes path newer predicate)))

(defun nemacs-user-snapshot--getenv (original name &optional frame)
  (let ((value (funcall original name frame)))
    (push (vector name (or value :null)) nemacs-user-snapshot--environment)
    value))

(defun nemacs-user-snapshot--refuse (operation &rest _)
  (setq nemacs-user-snapshot--refused operation)
  (error "User snapshot preload attempted %s" operation))

(defun nemacs-user-snapshot--ffi-declaration (original library)
  (let ((nemacs-user-snapshot--declaring-library t))
    (funcall original library)))

(defun nemacs-user-snapshot--guard-foreign-open (operation original &rest arguments)
  ;; Static readers may try dlopen before falling back. A failed attempt
  ;; has no handle and keeps the package's normal error/fallback behavior.
  (let ((handle (apply original arguments))
        (path (car arguments)))
    (when handle
      ;; The cold driver already strips and lazily reopens handles registered
      ;; by ffi:library. A directly returned foreign handle has no such owner.
      (unless (and nemacs-user-snapshot--declaring-library
                   (file-name-absolute-p path))
        (nemacs-user-snapshot--refuse operation))
      (let* ((name (nemacs-user-snapshot--local-name path))
             (bytes (nelisp--syscall-read-file name t)))
        (unless bytes (error "Could not fingerprint foreign library: %s" name))
        (push (vector name (secure-hash 'sha256 bytes)) nemacs-user-snapshot--reads)))
    handle))

(defun nemacs-user-snapshot--open-descriptors ()
  (let ((directory "/proc/self/fd/") (file-name-handler-alist nil))
    (unless (file-directory-p directory)
      (error "User snapshot requires Linux procfs"))
    ;; The directory enumeration itself has closed its descriptor by now.
    (delq nil (mapcar (lambda (name)
                       (let ((target (file-symlink-p (concat directory name))))
                         (and target (cons name target))))
                     (directory-files directory nil "\\`[0-9]+\\'")))))

(defun nemacs-user-snapshot--display-state ()
  (list (current-buffer) (selected-window) (selected-frame)
        (mapcar (lambda (frame)
                  (list frame (copy-tree (frame-parameters frame))
                        (mapcar (lambda (window)
                                  (list window (window-buffer window)
                                        (window-point window) (window-start window)
                                        (window-total-width window) (window-total-height window)
                                        (copy-tree (window-parameters window))))
                                (window-list frame))))
                (frame-list))))

(defun nemacs-user-snapshot--check-session ()
  (when (or (not noninteractive)
            (bound-and-true-p nemacs-initialized)
            (bound-and-true-p command-line-processed)
            (and (fboundp 'daemonp) (daemonp))
            (process-list) (bound-and-true-p timer-list)
            (and (bound-and-true-p timer-idle-list)
                 (not (and nemacs-user-snapshot--rearm-show-paren
                           (eq (car timer-idle-list) show-paren--idle-timer)
                           (null (cdr timer-idle-list)))))
            (and (fboundp 'all-threads) (cdr (all-threads))))
    (error "User snapshot requires headless state without processes, threads or timers"))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (or buffer-file-name (buffer-base-buffer)
                (and (buffer-modified-p)
                     (not (equal (buffer-name) "*Messages*"))))
        (error "User snapshot cannot retain visiting, indirect or modified buffer: %s"
               (buffer-name))))))

(defun nemacs-user-snapshot--prepare (init-directory paths features prefer-newer)
  "Preload FEATURES with PATHS and print observations for the manifest."
  ;; The base editor starts this one Lisp idle timer while baking. Its
  ;; deadline belongs to the baker. Recreate it only on the restore path.
  (setq nemacs-user-snapshot--rearm-show-paren
        (and (bound-and-true-p show-paren-mode)
             (bound-and-true-p show-paren--idle-timer) t))
  (nemacs-user-snapshot--check-session)
  (let* ((nemacs-user-snapshot--reads nil)
         (nemacs-user-snapshot--lookups nil)
         (nemacs-user-snapshot--directories (make-hash-table :test 'equal))
         (nemacs-user-snapshot--environment nil)
         (nemacs-user-snapshot--refused nil)
         (nemacs-user-snapshot--declaring-library nil)
         (base-history (mapcar #'car load-history))
         (base-path (copy-sequence load-path))
         (preload-path (append paths base-path))
         (profile (file-name-as-directory init-directory))
         (base-display (nemacs-user-snapshot--display-state))
         (base-descriptors (nemacs-user-snapshot--open-descriptors))
         (base-environment (copy-sequence process-environment))
         (messages (and (get-buffer "*Messages*")
                        (with-current-buffer "*Messages*" (buffer-string))))
         (post-gc (copy-tree (bound-and-true-p post-gc-hook)))
         (observers '((emacs-load--read-file-string . nemacs-user-snapshot--read)
                      (emacs-load--find-file . nemacs-user-snapshot--find)
                      (getenv . nemacs-user-snapshot--getenv)
                      (ffi:library . nemacs-user-snapshot--ffi-declaration)))
         (forbidden '(make-process start-process start-file-process
                      start-process-shell-command make-pipe-process
                      make-network-process open-network-stream
                      make-serial-process make-thread module-load sqlite-open
                      file-notify-add-watch lock-buffer
                      make-frame current-window-configuration
                      server-start run-at-time run-with-timer run-with-idle-timer
                      timer-activate timer-activate-when-idle))
         originals added-paths)
    (unwind-protect
        (let ((load-path (copy-sequence preload-path))
              (load-prefer-newer prefer-newer)
              (user-emacs-directory profile)
              (emacs-load-auto-native-compile nil)
              (native-comp-jit-compilation nil))
          (dolist (entry observers)
            (let* ((name (car entry)) (original (symbol-function name)))
              (unless original (error "Snapshot observer unavailable: %S" name))
              (push (cons name original) originals)
              (fset name (apply-partially (cdr entry) original))))
          (dolist (name forbidden)
            (when (fboundp name)
              (push (cons name (symbol-function name)) originals)
              (fset name (apply-partially #'nemacs-user-snapshot--refuse name))))
          (dolist (name '(nl-ffi--dlopen nl-ffi-loader-open))
            (when (fboundp name)
              (let ((original (symbol-function name)))
                (push (cons name original) originals)
                (fset name (apply-partially #'nemacs-user-snapshot--guard-foreign-open
                                            name original)))))
          (dolist (feature features)
            (require feature))
          (when nemacs-user-snapshot--refused
            (error "User snapshot preload attempted %s" nemacs-user-snapshot--refused))
          ;; Keep package-owned prepend operations such as adding calendar
          ;; during Org's load. The supplied search prefix is bake-only.
          (let ((count (- (length load-path) (length preload-path))))
            (unless (and (>= count 0) (equal (nthcdr count load-path) preload-path))
              (error "User snapshot preload reordered or removed load-path entries"))
            (setq added-paths (seq-take load-path count))
            (dolist (path added-paths)
              (unless (and (stringp path) (file-name-absolute-p path)
                           (equal path (nemacs-user-snapshot--local-name path)))
                (error "Invalid package-added load-path entry: %S" path))))
          (unless (eq load-prefer-newer prefer-newer)
            (error "User snapshot preload changed load-prefer-newer"))
          (unless (equal user-emacs-directory profile)
            (error "User snapshot preload changed user-emacs-directory"))
          (nemacs-user-snapshot--check-session))
      (dolist (entry originals) (fset (car entry) (cdr entry))))
    (unless (equal base-display (nemacs-user-snapshot--display-state))
      (error "User snapshot preload changed frame or window state"))
    (let ((descriptors (nemacs-user-snapshot--open-descriptors)))
      (unless (equal base-descriptors descriptors)
        (error "User snapshot preload changed descriptors or left open descriptors: %S"
               (seq-remove (lambda (entry) (member entry base-descriptors)) descriptors))))
    (unless (equal base-environment process-environment)
      (error "User snapshot preload changed the process environment"))
    (unless (equal post-gc (bound-and-true-p post-gc-hook))
      (error "User snapshot preload changed post-gc-hook"))
    (when messages
      (with-current-buffer "*Messages*"
        (unless (equal messages (buffer-string))
          (with-silent-modifications
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert messages))))))
    (dolist (entry load-history)
      (let ((path (car entry)))
        (when (and (stringp path) (not (member path base-history))
                   (not (seq-some (lambda (read)
                                    (equal (aref read 0) path))
                                  nemacs-user-snapshot--reads)))
          (error "User snapshot could not account for loaded file: %s" path))))
    (setq load-path (append added-paths base-path))
    (when nemacs-user-snapshot--rearm-show-paren
      (setq timer-idle-list nil show-paren--idle-timer nil))
    (princ "NEMACS-USER-SNAPSHOT-INPUTS ")
    (princ (json-serialize
            (vector (vconcat (mapcar (lambda (path) (or path :null)) base-path))
                    (vconcat (delq nil base-history))
                    (vconcat (delete-dups nemacs-user-snapshot--reads))
                    (vconcat (delete-dups nemacs-user-snapshot--lookups))
                    (vconcat (delete-dups nemacs-user-snapshot--environment))
                    (let (directories)
                      (maphash (lambda (path stamp)
                                 (push (vector path stamp) directories))
                               nemacs-user-snapshot--directories)
                      (vconcat directories))
                    (vconcat added-paths))))
    (terpri)))

(defun nemacs-user-snapshot-reinit ()
  "Restore session-local runtime state and run the portable-dump hook."
  (nemacs-runtime-dump-reinit)
  (when (and nemacs-user-snapshot--rearm-show-paren (not show-paren--idle-timer))
    (setq show-paren--idle-timer
          (run-with-idle-timer show-paren-delay t #'show-paren-function)))
  (run-hooks 'after-pdump-load-hook))

;;; nemacs-user-snapshot.el ends here
