;;; emacs-os-detect-test.el --- tests for runtime OS detection  -*- lexical-binding: t; -*-

;;; Commentary:

;; Two lanes, mirroring `emacs-server-client-test.el':
;;
;;   * Host ERT pins the pure mapping/triple logic and the
;;     substrate-absent contract (on host Emacs `syscall-direct' is not
;;     bound, so detection must return nil and mutate nothing).
;;   * The standalone gate drives the REAL NeLisp reader, where the
;;     `uname(2)' syscall is live, and asserts the detected fields are
;;     coherent.

;;; Code:

(require 'ert)

(defconst emacs-os-detect-test--root
  (expand-file-name
   ".."
   (file-name-directory (or load-file-name buffer-file-name))))

(add-to-list 'load-path (expand-file-name "src" emacs-os-detect-test--root))
(require 'emacs-os-detect)

(ert-deftest emacs-os-detect/uname-syscall-number-contract ()
  (should (integerp emacs-os-uname-syscall-number))
  (should (= 63 emacs-os-uname-syscall-number)))

(defun emacs-os-detect-test--reader ()
  "Return an executable standalone reader path, or nil."
  (let ((candidates
         (list (getenv "NELISP")
               (expand-file-name "vendor/nelisp/target/nelisp"
                                 emacs-os-detect-test--root))))
    (catch 'found
      (dolist (c candidates)
        (when (and c (file-executable-p c))
          (throw 'found c)))
      nil)))

;;; --- pure sysname -> system-type mapping -------------------------------

(ert-deftest emacs-os-detect/sysname-mapping-known ()
  (should (eq 'gnu/linux     (emacs-os-sysname->system-type "Linux")))
  (should (eq 'darwin        (emacs-os-sysname->system-type "Darwin")))
  (should (eq 'berkeley-unix (emacs-os-sysname->system-type "FreeBSD")))
  (should (eq 'berkeley-unix (emacs-os-sysname->system-type "NetBSD")))
  (should (eq 'berkeley-unix (emacs-os-sysname->system-type "OpenBSD")))
  (should (eq 'berkeley-unix (emacs-os-sysname->system-type "DragonFly")))
  (should (eq 'usg-unix-v    (emacs-os-sysname->system-type "SunOS")))
  (should (eq 'gnu           (emacs-os-sysname->system-type "GNU")))
  (should (eq 'aix           (emacs-os-sysname->system-type "AIX")))
  (should (eq 'haiku         (emacs-os-sysname->system-type "Haiku")))
  (should (eq 'cygwin        (emacs-os-sysname->system-type "CYGWIN_NT-10.0")))
  (should (eq 'windows-nt    (emacs-os-sysname->system-type "MINGW64_NT-10.0")))
  (should (eq 'windows-nt    (emacs-os-sysname->system-type "MSYS_NT-10.0"))))

(ert-deftest emacs-os-detect/sysname-mapping-case-insensitive ()
  (should (eq 'gnu/linux (emacs-os-sysname->system-type "LINUX")))
  (should (eq 'darwin    (emacs-os-sysname->system-type "darwin"))))

(ert-deftest emacs-os-detect/sysname-mapping-unknown-is-nil ()
  ;; Unknown kernels must return nil so callers keep their default
  ;; rather than guess a wrong `system-type'.
  (should (null (emacs-os-sysname->system-type "Plan9")))
  (should (null (emacs-os-sysname->system-type "")))
  (should (null (emacs-os-sysname->system-type nil))))

;;; --- config-triple synthesis -------------------------------------------

(ert-deftest emacs-os-detect/config-triple-linux ()
  ;; Reproduces the historical hard-coded default on this target.
  (should (equal "x86_64-pc-linux-gnu"
                 (emacs-os--config-triple "x86_64" 'gnu/linux "6.12.0")))
  (should (equal "i686-pc-linux-gnu"
                 (emacs-os--config-triple "i686" 'gnu/linux "6.12.0"))))

(ert-deftest emacs-os-detect/config-triple-darwin ()
  (should (equal "arm64-apple-darwin23"
                 (emacs-os--config-triple "arm64" 'darwin "23.4.0"))))

(ert-deftest emacs-os-detect/config-triple-unknown-vendor ()
  ;; A non-x86 machine on Linux falls back to the `unknown' vendor.
  (should (equal "aarch64-unknown-linux-gnu"
                 (emacs-os--config-triple "aarch64" 'gnu/linux "6.12.0"))))

;;; --- A2: OS-shaped polyfills (path-separator / exec-suffixes) ----------

(ert-deftest emacs-os-detect/polyfills-linux ()
  (let ((system-type 'gnu/linux)
        (path-separator "?")
        (exec-suffixes 'sentinel))
    (emacs-os-apply-os-polyfills!)
    (should (equal ":" path-separator))
    (should (null exec-suffixes))))

(ert-deftest emacs-os-detect/polyfills-windows ()
  (let ((system-type 'windows-nt)
        (path-separator "?")
        (exec-suffixes nil))
    (emacs-os-apply-os-polyfills!)
    (should (equal ";" path-separator))
    (should (member ".exe" exec-suffixes))))

;;; --- A1: environment-derived dirs (getenv-backed) ----------------------

(ert-deftest emacs-os-detect/env-helper ()
  ;; getenv is live on host Emacs; PATH is set on any dev/CI host.
  (should (stringp (emacs-os--env "PATH")))
  (should (null (emacs-os--env "NELISP_DEFINITELY_UNSET_VAR_XYZ"))))

(ert-deftest emacs-os-detect/dirs-from-env ()
  ;; Mutates only let-bound copies, so the host daemon's real path/dirs
  ;; are untouched.
  (let ((temporary-file-directory "SENTINEL/")
        (user-emacs-directory "SENTINEL/")
        (exec-path '("SENTINEL"))
        (path-separator (if (boundp 'path-separator) path-separator ":")))
    (emacs-os-detect-and-set-dirs!)
    (when (emacs-os--env "HOME")
      (should (string-suffix-p ".emacs.d/" user-emacs-directory))
      (let ((home (replace-regexp-in-string "\\\\" "/" (emacs-os--env "HOME")))
            (udir (replace-regexp-in-string "\\\\" "/" user-emacs-directory)))
        (when (eq system-type 'windows-nt)
          (setq home (downcase home)
                udir (downcase udir)))
        (should (string-prefix-p home udir))))
    (when (emacs-os--env "PATH")
      (should (listp exec-path))
      (should (> (length exec-path) 0))
      (should-not (equal '("SENTINEL") exec-path)))))

;;; --- substrate-absent contract (host) ----------------------------------

(ert-deftest emacs-os-detect/host-has-no-substrate ()
  ;; Host Emacs does not expose the raw reader syscall surface.
  (skip-unless (not (fboundp 'syscall-direct)))
  (should-not (emacs-os--substrate-available-p)))

(ert-deftest emacs-os-detect/host-uname-is-nil ()
  (skip-unless (not (fboundp 'syscall-direct)))
  (should (null (emacs-uname t)))
  (should (null (emacs-detect-system-type))))

(ert-deftest emacs-os-detect/host-set-is-noop ()
  ;; Without the substrate, `emacs-os-detect-and-set!' returns nil and
  ;; must NOT clobber the host's existing `system-type'.
  (skip-unless (not (fboundp 'syscall-direct)))
  (let ((before system-type))
    (should (null (emacs-os-detect-and-set!)))
    (should (eq before system-type))))

;;; --- standalone reader gate (real uname(2)) ----------------------------

(ert-deftest emacs-os-detect/standalone-uname-coherent ()
  (let ((reader (emacs-os-detect-test--reader)))
    (skip-unless reader)
    (let* ((module (expand-file-name "src/emacs-os-detect.el"
                                     emacs-os-detect-test--root))
           (form (format
                  "(progn (load %S) (prin1 (list :type system-type :name (system-name) :conf system-configuration :sys (plist-get (emacs-uname) :sysname))))"
                  module))
           (out (with-temp-buffer
                  (call-process reader nil t nil "--eval" form)
                  (buffer-string))))
      ;; The reader prints the plist somewhere in its output; pull it back.
      (should (string-match ":type \\([^ ]+\\)" out))
      (let ((stype (intern (match-string 1 out))))
        ;; Whatever the CI host is, it must be a known `system-type'.
        (should (memq stype '(gnu/linux darwin berkeley-unix gnu
                              gnu/kfreebsd usg-unix-v aix haiku
                              cygwin windows-nt))))
      ;; nodename must be non-empty and not the "standalone" placeholder.
      (should (string-match ":name \"\\([^\"]+\\)\"" out))
      (should-not (equal "standalone" (match-string 1 out)))
      ;; sysname must map to the same system-type we set.
      (should (string-match ":sys \"\\([^\"]+\\)\"" out)))))

(provide 'emacs-os-detect-test)

;;; emacs-os-detect-test.el ends here
