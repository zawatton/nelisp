;;; m1-bootsmoke-test.el --- T97 M1 boot smoke ERT  -*- lexical-binding: t; -*-

;; T97 — M1 milestone integration smoke test for booting
;; anvil-server / anvil-defs / anvil-state on top of the NeLisp
;; standalone substrate (Wave 1+2: load / emacs-compat / fileio /
;; regex / json / sqlite / coding / base64 / secure-hash / ec-bridge).
;;
;; The shell harness `scripts/m1-bootsmoke.sh' actually exercises the
;; end-to-end MCP `tools/list' round-trip in a clean batch subprocess
;; (so host-side state cannot mask substrate gaps).  These ERTs are
;; in-process consistency checks: the shell script exists, is
;; executable, and the substrate / anvil-core load preconditions hold
;; for the *current* Emacs.
;;
;; Layout:
;;   1. m1-bootsmoke-script-exists       — file present + +x bit
;;   2. m1-bootsmoke-substrate-load-clean — every nelisp-* feature loads
;;   3. m1-bootsmoke-bridge-install      — `nelisp-ec-bridge-install'
;;                                          returns 0 on host (no-op)
;;   4. m1-bootsmoke-anvil-core-load     — anvil-server / -defs / -state
;;                                          require with no error
;;   5. m1-bootsmoke-anvil-defs-server-id — anvil-defs registers under
;;                                          "emacs-eval" server-id
;;   6. m1-bootsmoke-substrate-api-surface — key public APIs are bound
;;
;;; Code:

(require 'ert)

(defconst m1-bootsmoke-test--repo
  (file-name-directory
   (directory-file-name
    (file-name-directory
     (or load-file-name buffer-file-name))))
  "Repo root containing src/ and scripts/ for this test module.")

(defconst m1-bootsmoke-test--script
  (expand-file-name "scripts/m1-bootsmoke.sh" m1-bootsmoke-test--repo))

(defconst m1-bootsmoke-test--substrate-features
  '(nelisp-load
    nelisp-emacs-compat
    nelisp-emacs-compat-fileio
    nelisp-regex
    nelisp-json
    nelisp-sqlite
    nelisp-coding
    nelisp-base64
    nelisp-secure-hash
    nelisp-ec-bridge)
  "Substrate features that the M1 boot script loads (in order).")

(defconst m1-bootsmoke-test--anvil-features
  '(anvil
    anvil-server
    anvil-server-commands
    anvil-defs
    anvil-state)
  "anvil-core features that must load on top of the substrate.")

(defun m1-bootsmoke-test--anvil-path ()
  "Return the path to anvil.el package, or nil if unavailable."
  (or (getenv "ANVIL_PATH")
      (cl-some
       (lambda (cand)
         (and (file-directory-p cand)
              (file-readable-p (expand-file-name "anvil-server.el" cand))
              cand))
       (list (expand-file-name "~/Cowork/Notes/dev/anvil.el")
             (expand-file-name "~/Notes/dev/anvil.el")
             (expand-file-name "~/.emacs.d/external-packages/anvil.el")))))

(ert-deftest m1-bootsmoke-script-exists ()
  "scripts/m1-bootsmoke.sh exists, is executable, and is non-empty."
  (should (file-exists-p m1-bootsmoke-test--script))
  (should (file-executable-p m1-bootsmoke-test--script))
  (should (> (nth 7 (file-attributes m1-bootsmoke-test--script)) 200)))

(ert-deftest m1-bootsmoke-substrate-load-clean ()
  "Every Wave 1+2 nelisp-* feature loads without raising."
  (let ((src-dir (expand-file-name "src" m1-bootsmoke-test--repo)))
    (should (file-directory-p src-dir))
    (let ((load-path (cons src-dir load-path)))
      (dolist (feat m1-bootsmoke-test--substrate-features)
        (should (or (featurep feat)
                    (require feat nil 'noerror))
                ;; ert-explainer would be nicer; for now: feat name is
                ;; in the failure stack via the should form itself.
                )))))

(ert-deftest m1-bootsmoke-bridge-install ()
  "On host Emacs, `nelisp-ec-bridge-install' is a strict no-op (= 0
new aliases) because every Emacs builtin is already `fboundp'."
  (let ((src-dir (expand-file-name "src" m1-bootsmoke-test--repo)))
    (let ((load-path (cons src-dir load-path)))
      (require 'nelisp-emacs-compat)
      (require 'nelisp-emacs-compat-fileio)
      (require 'nelisp-ec-bridge)
      (should (fboundp 'nelisp-ec-bridge-install))
      (should (fboundp 'nelisp-ec-bridge-status))
      ;; Install reports 0 newly-installed aliases on host Emacs.
      (should (= 0 (nelisp-ec-bridge-install)))
      (let ((status (nelisp-ec-bridge-status)))
        (should (>= (plist-get status :total) 48))
        (should (= 0 (plist-get status :installed)))
        (should (>= (plist-get status :host-bound) 48))
        (should (= 0 (plist-get status :missing-ec)))))))

(ert-deftest m1-bootsmoke-anvil-core-load ()
  "anvil / anvil-server / anvil-server-commands / anvil-defs /
anvil-state all `require' cleanly on top of the substrate."
  (let ((anvil-path (m1-bootsmoke-test--anvil-path)))
    (skip-unless anvil-path)
    (let ((src-dir (expand-file-name "src" m1-bootsmoke-test--repo))
          (load-path load-path))
      (push src-dir load-path)
      (push anvil-path load-path)
      ;; Substrate first — order matters, ec-bridge depends on others.
      (dolist (feat m1-bootsmoke-test--substrate-features)
        (require feat))
      (nelisp-ec-bridge-install)
      (dolist (feat m1-bootsmoke-test--anvil-features)
        (should (or (featurep feat)
                    (require feat nil 'noerror)))))))

(ert-deftest m1-bootsmoke-anvil-defs-server-id ()
  "anvil-defs advertises its tools under the \"emacs-eval\"
server-id, NOT the \"default\" one used by anvil-server."
  (let ((anvil-path (m1-bootsmoke-test--anvil-path)))
    (skip-unless anvil-path)
    (let ((load-path load-path))
      (push (expand-file-name "src" m1-bootsmoke-test--repo) load-path)
      (push anvil-path load-path)
      (require 'anvil-defs)
      (should (boundp 'anvil-defs--server-id))
      (should (equal (symbol-value 'anvil-defs--server-id)
                     "emacs-eval")))))

(ert-deftest m1-bootsmoke-substrate-api-surface ()
  "Spot-check the substrate public API surface.

The full M1 path needs at minimum a JSON parser, a SQLite handle
opener, a base64 codec, a hash codec, the regex `string-match'
substitute, and the file-I/O bridge.  The scripts/m1-bootsmoke.sh
script trusts these names; the ERT pins the contract."
  (let ((load-path
         (cons (expand-file-name "src" m1-bootsmoke-test--repo)
               load-path)))
    (dolist (feat m1-bootsmoke-test--substrate-features)
      (require feat))
    ;; JSON
    (should (fboundp 'nelisp-json-parse-string))
    (should (fboundp 'nelisp-json-serialize))
    ;; SQLite (FFI; compat alias)
    (should (fboundp 'nelisp-sqlite-open))
    ;; Base64
    (should (fboundp 'nelisp-b64-encode-string))
    ;; Secure hash
    (should (fboundp 'nelisp-hash-sha256))
    (should (fboundp 'nelisp-hash-secure-hash))
    ;; Regex (NeLisp side; pure-Lisp NFA)
    (should (fboundp 'nelisp-rx-string-match))
    ;; File I/O bridge (nelisp-emacs-compat-fileio)
    (should (fboundp 'nelisp-ec-write-region))
    (should (fboundp 'nelisp-ec-insert-file-contents))))

(provide 'm1-bootsmoke-test)
;;; m1-bootsmoke-test.el ends here
